unit SLTaskMgr;

{$DEFINE DELETE_FILE}

interface

uses
  Windows, Classes, ExtCtrls, SLTask, HttpDownload, HttpMessage, HttpHeaderExtracter;

const
  SSLTaskStorageFile = 'tasks.ini';
  SSLTaskTempDir = 'temp';
  CSLTaskDefaultThreadCount = 5;

type
  TSLTaskOnProgress = procedure(Sender: TObject; var Task: TSLTask) of object;
  TSLTaskOnCompleted = procedure(Sender: TObject; var Task: TSLTask) of object;
  TSLTaskonFailed = procedure(Sender: TObject; var Task: TSLTask) of object;

  TSLTaskID = SLTask.TSLTaskID;

  TSLTaskManager = class(TObject)
  private
    FTempDir: string;
    FTaskList: TList;
    FDownloadThreadCount: Integer;
    FTotalRedownloadCount: Integer;
    FTaskStorageFile: string;
    FOnCompleted: TSLTaskOnCompleted;
    FOnProgress: TSLTaskOnProgress;
    FOnFailed: TSLTaskonFailed;
    FTmrClean: TTimer;
    function IndexOf(ID: TSLTaskID): Integer; // -1: not found
    procedure SetTempDir(dir: string);
    procedure SetTaskStorageFile(const Value: string);
    procedure DoHttpFileDone(Sender: TObject; FileName: string; FileSize: Integer);
    function DownloadSegments(Index: Integer): Boolean;
    procedure MergeNewSegments(Index: Integer; TaskID: TSLTaskID);

    procedure SetDownloadThreadCount(const Value: Integer);
    procedure UpdateProgress(Index: Integer);
    procedure DoTmrCleanTimer(Sender: TObject);
    procedure StopByIndex(Index: Integer);
    procedure CleanTasks;
  public
    { compulsory }
    constructor Create;
    destructor Destroy; override;
    function Add(NewTask: TSLNewTask): PSLTask;
    function Remove(ID: TSLTaskID): Integer;
    procedure Delete(Index: Integer);
    procedure Start(ID: TSLTaskID);
    procedure Stop(ID: TSLTaskID);
    procedure LoadTaskList;
    procedure SaveTaskList;
    procedure UpdateProgresses;
    { optional }
    function Get(ID: TSLTaskID): PSLTask; overload;
    function Get(Index: Integer): PSLTask; overload;
    function TaskCount: Integer;
    property TempDir: string read FTempDir write SetTempDir;
    property TaskListFile: string read FTaskStorageFile write SetTaskStorageFile;
    property DownloadThreadCount: Integer read FDownloadThreadCount write SetDownloadThreadCount;
    property OnProgress: TSLTaskOnProgress read FOnProgress write FOnProgress;
    property OnCompleted: TSLTaskOnCompleted read FOnCompleted write FOnCompleted;
    property OnFailed: TSLTaskonFailed read FOnFailed write FOnFailed;
  end;

implementation

uses
  Forms, SysUtils, SyncObjs, PerlRegEx, SLTaskList, IsmvSettings, mp4_io;

{ TSLTaskManager }

function GetCurrentDir: string;
begin
  Result := ExtractFileDir(Application.ExeName);
end;

procedure ProcessMessages;
begin
  Sleep(10);
  Application.ProcessMessages;
end;

function GetSegmentUrls(IsmUrl, UrlFormat, Bitrate: string;
  StartTimes: TStringList): TStringList;
const
  reBitrate: string = '{bitrate}';
  reStartTime: string = '{start time}';
  reManifest: string = 'manifest';
var
  re: TPerlRegEx;
  UrlPtn: string;
  i, count: Integer;
begin
  re := TPerlRegEx.Create;
  try
    re.Replacement := '%s';
    re.Options := re.Options + [preCaseLess]; // add by XX 20111212

    re.Subject := IsmUrl;
    re.RegEx := reManifest;
    re.ReplaceAll;
    UrlPtn := re.Subject;

    re.Subject := UrlFormat;
    re.RegEx := reBitrate;
    re.ReplaceAll;

    re.RegEx := reStartTime;
    re.ReplaceAll;

    UrlPtn := Format(UrlPtn, [re.Subject]);

    Result := TStringList.Create;
    count := StartTimes.Count;
    for i := 0 to count - 1 do
    begin
      Result.Add(Format(UrlPtn, [Bitrate, StartTimes[i]]));
    end;
  finally
    re.Free;
  end;
end;

function TSLTaskManager.Add(NewTask: TSLNewTask): PSLTask;
var
  PTask: PSLTask;
begin
  PTask := nil;
  NewSLTask(PTask, NewTask);
  Result := PTask;
  FTaskList.Add(PTask);
end;

function TSLTaskManager.Remove(ID: TSLTaskID): Integer;
begin
  Result := IndexOf(ID);
  if Result >= 0 then
    Delete(Result);
end;

procedure TSLTaskManager.Delete(Index: Integer);
var
  p: PSLTask;
begin
  if (Index < 0) or (Index >= FTaskList.Count) then Exit;

  p := PSLTask(FTaskList[Index]);
  if p.TaskState = tsRunning then
  begin
    StopByIndex(Index);
  end;
  p.TaskState := tsFinished;

  CleanTasks;
end;

procedure TSLTaskManager.Start(ID: TSLTaskID);
var
  Index: Integer;
begin
  FTotalRedownloadCount := 0;
  
  Index := IndexOf(ID);
  if Index < 0 then Exit;

  with PSLTask(FTaskList[Index])^ do
  begin
    Assert(PageUrl <> EmptyStr);
    Assert(XmlUrl <> EmptyStr);
    Assert(XmlFile <> EmptyStr);
    Assert(MovieFile <> EmptyStr);
    Assert(FileExists(XmlFile));

    QueryPerformanceCounter(startCount);

    if DownloadList = nil then
    begin
      DownloadList := THttpDownloadList.Create(nil);
      DownloadList.MaxThreadsNum := FDownloadThreadCount;
      DownloadList.OnFileDone := DoHttpFileDone;
    end;

    if (TaskState = tsStopped) then
      if FileExists(MovieFile) then
      begin
        if not Assigned(MovieFileStream) then
          MovieFileStream := TFileStream.Create(MovieFile, fmOpenWrite or fmShareDenyNone);
        MovieFileStream.Position := CurrentSize;//MovieFileStream.Size;
      end
      else
      begin
        CurrentSize := 0;
        NextVideoIndex := 0;
        NextAudioIndex := 0;
      end;
    TaskState := tsRunning;
    DownloadSegments(Index);
  end;
end;

procedure TSLTaskManager.Stop(ID: TSLTaskID);
begin
  StopByIndex(IndexOf(ID));
end;

constructor TSLTaskManager.Create;
begin
  FTempDir := SSLTaskTempDir;
  if not DirectoryExists(FTempDir) then MkDir(Format('%s\%s', [GetCurrentDir, FTempDir]));

  FTaskStorageFile := SSLTaskStorageFile;

  FTaskList := TList.Create;

  FTmrClean := TTimer.Create(nil);
  FTmrClean.Enabled := False;
  FTmrClean.OnTimer := DoTmrCleanTimer;
  FTmrClean.Interval := 500;

  FDownloadThreadCount := CSLTaskDefaultThreadCount;
end;

destructor TSLTaskManager.Destroy;
var
  i: Integer;
begin
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      StopByIndex(i);
    end;
  except
  end;

  SaveTaskList;
  FreeSLTaskList(FTaskList);
  FTmrClean.Free;
  inherited;
end;

function TSLTaskManager.IndexOf(ID: TSLTaskID): Integer;
var
  i, count: Integer;
begin
  Result := -1;
  count := FTaskList.Count;
  for i := 0 to count - 1 do
  begin
    if PSLTask(FTaskList[i]).ID = ID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TSLTaskManager.LoadTaskList;
begin
  FTaskList := LoadSLTaskList(Format('%s\%s', [GetCurrentDir, FTaskStorageFile]));
end;

procedure TSLTaskManager.SaveTaskList;
begin
  SaveSLTaskList(FTaskList, Format('%s\%s', [GetCurrentDir, FTaskStorageFile]));
end;

procedure TSLTaskManager.SetTaskStorageFile(const Value: string);
begin
  FTaskStorageFile := Value;
end;

procedure TSLTaskManager.SetTempDir(dir: string);
begin
  FTempDir := dir;
end;

function TSLTaskManager.DownloadSegments(Index: Integer): Boolean;
var
  J: Integer;
  videoUrls, audioUrls: TStringList;
  folder: string;
  minCount: Integer;

  procedure AddDownload(DownloadList: THttpDownloadList; folder: string;
    i: Integer;
    type_: Char;
    NextIndex: Integer;
    var Exists: array of Boolean; urls: TStringList;
    msg: THttpMessage);
  var
    fn: string;
  begin
    fn := Format('%s\%s%d.ismv', [folder, type_, i]);
    if i >= NextIndex then
    begin
      DownloadList.Add(msg, urls[i], fn); //added by lijm
    end;
  end;
begin
  Result := False;

  with PSLTask(FTaskList[Index])^ do
  begin
    try
      videoUrls := GetSegmentUrls(XmlUrl, MovieSettings.VideoUrl,
        IntToStr(MovieSettings.VideoBitrate), MovieSettings.VideoStartTimes);
      audioUrls := GetSegmentUrls(XmlUrl, MovieSettings.AudioUrl,
        IntToStr(MovieSettings.AudioBitrate), MovieSettings.AudioStartTimes);

      try
        folder := Format('%s\%s\%s', [GetCurrentDir, FTempDir, ID]);
        if not DirectoryExists(folder) then
          MkDir(folder);

        if videoUrls.Count <= audioUrls.Count then
          minCount := videoUrls.Count
        else
          minCount := audioUrls.Count;
        for J := 0 to minCount - 1 do
        begin
          AddDownload(DownloadList, folder, J, 'v', NextVideoIndex, ExistVideos, videoUrls, HttpRequestMsg);
          AddDownload(DownloadList, folder, J, 'a', NextAudioIndex, ExistAudios, audioUrls, HttpRequestMsg);
        end;
        for J := minCount to videoUrls.Count - 1 do
        begin
          AddDownload(DownloadList, folder, J, 'v', NextVideoIndex, ExistVideos, videoUrls, HttpRequestMsg);
        end;
        for J := minCount to audioUrls.Count - 1 do
        begin
          AddDownload(DownloadList, folder, J, 'a', NextAudioIndex, ExistAudios, audioUrls, HttpRequestMsg);
        end;

      finally
        videoUrls.Free;
        audioUrls.Free;
      end;
    except
      Exit;
    end;

  end;

  Result := True;
end;

procedure TSLTaskManager.MergeNewSegments(Index: Integer; TaskID: TSLTaskID);

  procedure WriteSegment(type_name: Char; next_index: Integer;
    offsets: TStringList);
  var
    fn: string;
    input: TFileStream;
    offset: Int64;
  begin
    with PSLTask(FTaskList[Index])^ do
    begin
      fn := Format('%s\%s\%s\%s%d.ismv', [GetCurrentDir, FTempDir, ID, type_name, next_index]);
      if not FileExists(fn) then
        fn := Format('%s\%s\%s\%s%d.mp4', [GetCurrentDir, FTempDir, ID, type_name, next_index]);
      input := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
      try
        write_ismv_part(MovieFileStream, input, offset);
      finally
        input.Free;
      end;
{$IFDEF DELETE_FILE}
      DeleteFile(fn);
{$ENDIF}
      case type_name of
        'v': ExistVideos[next_index] := False;
        'a': ExistAudios[next_index] := False;
      end;
      offsets.Append(IntToStr(offset));
    end;
  end;
begin
  if (FTaskList.Count <= 0) or (IndexOf(TaskID) = -1) then
  begin
    Exit;
  end;

  with PSLTask(FTaskList[Index])^ do
  begin
    MergeLock.Enter;
    try
      if MovieFileStream = nil then
      begin
        // create output stream & write ismv head at first time
        try
          with TStringList.Create do
          begin
            SaveToFile(MovieFile);
            Free;
          end;
        except
          StopByIndex(Index);
        end;
        MovieFileStream := TFileStream.Create(MovieFile, fmOpenWrite or fmShareDenyNone);
        write_ismv_head(MovieFileStream, @MovieSettings);
      end;

      while (NextVideoIndex < VideoPartCount)
        and (NextAudioIndex < AudioPartCount) do
      begin
        if (NextVideoIndex <= NextAudioIndex) then // 视频片段优先
        begin
          { merge new Video segment }
          if ExistVideos[NextVideoIndex] then
          begin
            WriteSegment('v', NextVideoIndex, VideoOffsets);
            Inc(NextVideoIndex);
          end
          else Break;
        end
        else
        begin
          { merge new Audio segment }
          if ExistAudios[NextAudioIndex] then
          begin
            WriteSegment('a', NextAudioIndex, AudioOffsets);
            Inc(NextAudioIndex);
          end
          else Break;
        end;
      end;
      if (NextAudioIndex >= AudioPartCount) then
      begin
        while NextVideoIndex < VideoPartCount do
        begin
          { merge new Video segment }
          if ExistVideos[NextVideoIndex] then
          begin
            WriteSegment('v', NextVideoIndex, VideoOffsets);
            Inc(NextVideoIndex);
          end
          else break;
        end;
      end;
      if (NextVideoIndex >= VideoPartCount) then
      begin
        while NextAudioIndex < AudioPartCount do
        begin
          { merge new Audio segment }
          if ExistAudios[NextAudioIndex] then
          begin
            WriteSegment('a', NextAudioIndex, AudioOffsets);
            Inc(NextAudioIndex);
          end
          else break;
        end;
      end;
    finally
      MergeLock.Leave;
    end;
    if (NextVideoIndex = VideoPartCount)
      and (NextAudioIndex = AudioPartCount) then // 下载完成
    begin
      // write tail of ismv file
      write_ismv_tail(MovieFileStream, @MovieSettings, VideoOffsets, AudioOffsets);

      if Assigned(FOnCompleted) then
        FOnCompleted(Self, PSLTask(FTaskList[Index])^);

      TaskState := tsFinished;

      CleanTasks;
    end;
  end;
end;

procedure TSLTaskManager.DoHttpFileDone(Sender: TObject; FileName: string;
  FileSize: Integer);
var
  taskID: string;
  fileIndex: Integer;
  pureName: string;
  taskIndex: Integer;
  input: TFileStream;
  function EraseZero(s: string): string;
  var
    i: Integer;
  begin
    SetLength(Result, Length(s));
    for i := 1 to Length(s) do
    begin
      if s[i] <> #0 then Result[i] := s[i];
    end;
  end;
  procedure ReDownload;
  var
    index: Integer;
  begin
    Inc(FTotalRedownloadCount);

    index := IndexOf(taskID);
    if index < 0 then Exit;
    
    with PSLTask(FTaskList[index])^ do
    begin
      if FTotalRedownloadCount > 3 * (VideoPartCount + AudioPartCount) then
      begin
        StopByIndex(index);
        if Assigned(FOnFailed) then
          FOnFailed(Self, PSLTask(FTaskList[index])^);
        Exit;
      end;
      if pureName[1] = 'v' then
        with GetSegmentUrls(XmlUrl, MovieSettings.VideoUrl,
          IntToStr(MovieSettings.VideoBitrate), MovieSettings.VideoStartTimes) do
          try
            DownloadList.Add(HttpRequestMsg, Strings[NextVideoIndex], FileName);
          finally
            Free;
          end
      else // type_name = 'a'
        with GetSegmentUrls(XmlUrl, MovieSettings.AudioUrl,
          IntToStr(MovieSettings.AudioBitrate), MovieSettings.AudioStartTimes) do
          try
            DownloadList.Add(HttpRequestMsg, Strings[NextAudioIndex], FileName);
          finally
            Free;
          end;
    end;
  end;
begin
  FileName := EraseZero(FileName);

  with TStringList.create do
    try
      Delimiter := '\';
      DelimitedText := FileName;
      taskID := Strings[Count - 2];
      pureName := Strings[Count - 1];
    finally
      Free;
    end;

  // 检查下载的文件是否有效
  if not FileExists(FileName) then
  begin
    ReDownload;
    Exit;
  end
  else
  begin
    input := TFileStream.Create(FileName, fmShareDenyNone);
    try
      if not check_ismv_part(input) then
      begin
        ReDownload;
        Exit;
      end;
    finally
      input.Free;
    end;
  end;

  fileIndex := StrToInt(Copy(pureName, 2, Length(pureName) - 1 - Length(ExtractFileExt(pureName))));
  taskIndex := IndexOf(taskID);
  if taskIndex < 0 then Exit;

  with PSLTask(FTaskList[taskIndex])^ do
    case pureName[1] of
      'v': ExistVideos[fileIndex] := True;
      'a': ExistAudios[fileIndex] := True;
    end;

  MergeNewSegments(taskIndex, taskID);
  SaveTaskList;
  ProcessMessages;
  UpdateProgresses;
end;

procedure TSLTaskManager.SetDownloadThreadCount(const Value: Integer);
var
  i: Integer;
begin
  FDownloadThreadCount := Value;
  for i := 0 to FTaskList.Count do
  begin
    PSLTask(FTaskList[i]).DownloadList.MaxThreadsNum := Value;
  end;
end;

function TSLTaskManager.Get(ID: TSLTaskID): PSLTask;
begin
  Result := FTaskList[IndexOf(ID)];
end;

function TSLTaskManager.TaskCount: Integer;
begin
  Result := FTaskList.Count;
end;

function TSLTaskManager.Get(Index: Integer): PSLTask;
begin
  Result := FTaskList[index];
end;

procedure TSLTaskManager.UpdateProgress(Index: Integer);
var
  p: PSLTask;
begin
  p := PSLTask(FTaskList[Index]);
  with p^ do
  begin
    if not Assigned(MovieFileStream) then Exit;

    if TaskState = tsRunning then
    begin
      CurrentSize := MovieFileStream.Position;
      if NextVideoIndex + NextAudioIndex <> 0 then
        TotalSize := CurrentSize * (VideoPartCount + AudioPartCount) div (NextVideoIndex + NextAudioIndex);
      QueryPerformanceCounter(NewCount);
      Speed := (CurrentSize) / ((NewCount - StartCount) / GetFrequency());
      ElapsedTime := (NewCount - StartCount) / GetFrequency();
      RemainingTime := (TotalSize - CurrentSize) / speed;
    end;

    if Assigned(FOnProgress) then
      FOnProgress(Self, p^);
  end;
end;

procedure TSLTaskManager.UpdateProgresses;
var
  i: Integer;
begin
  for i := 0 to FTaskList.Count - 1 do
  begin
    UpdateProgress(i);
  end;
end;

procedure TSLTaskManager.DoTmrCleanTimer(Sender: TObject);
var
  i: Integer;
  p: PSLTask;
begin
  for i := FTaskList.Count - 1 downto 0 do
  begin
    p := PSLTask(FTaskList[i]);
    if p.TaskState = tsFinished then
    begin
      if Assigned(p.MovieFileStream) then FreeAndNil(p.MovieFileStream);
      if Assigned(p.DownloadList) then FreeAndNil(p.DownloadList);
      // remove tmp files
      RemoveDir(Format('%s\%s\%s', [GetCurrentDir, FTempDir, p.ID]));
      // remove task
      DisposeSLTask(p);
      FTaskList.Delete(i);
    end;
  end;
  FTmrClean.Enabled := False;
end;

procedure TSLTaskManager.StopByIndex(Index: Integer);
var
  i: Integer;
begin
  with PSLTask(FTaskList[Index])^ do
  begin
    TaskState := tsStopped;
    FreeAndNil(DownloadList);
  end;
end;

procedure TSLTaskManager.CleanTasks;
begin
  FTmrClean.Enabled := True;
end;

end.

