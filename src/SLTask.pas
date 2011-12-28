unit SLTask;

interface

uses
  Classes, SyncObjs, IsmvSettings, HttpDownload, HttpMessage;

{ Silverlight Download & Process Task }
type
  Booleans = array of Boolean;
  TLargeInteger = Int64;
  TSLTaskID = string;

  TSLTaskState = (tsInit, tsRunning, tsStopped, tsFinished);

  PSLNewTask = ^TSLNewTask;
  TSLNewTask = record
    PageUrl: string; // 视频所在网页的URL
    XmlUrl: string; // 任务对应的manifest.xml文件的URL
    XmlFile: string; // 保存manifest.xml文件的本地文件名（含完整路径）
    VideoIndex: Integer; // 选择的视频质量的序号（第一个是0）
    AudioIndex: Integer; // 选择的音频质量的序号（第一个是0）
    MovieFile: string; // 保存视频的本地文件名（含完整路径）
    HttpRequestMsg: THttpMessage;
  end;

  PSLTask = ^TSLTask;
  TSLTask = record
    ID: TSLTaskID; // 任务的唯一标识，创建任务时由程序生成
    PageUrl: string; // 视频所在网页的URL
    XmlUrl: string; // 任务对应的manifest.xml文件的URL
    XmlFile: string; // 保存manifest.xml文件的本地文件名（含完整路径）
    VideoIndex: Integer; // 选择的视频质量的序号（第一个是0）
    AudioIndex: Integer; // 选择的音频质量的序号（第一个是0）
    MovieFile: string; // 保存视频的本地文件名（含完整路径）
    MovieSettings: TIsmvSettings;
    TaskState: TSLTaskState; // 任务状态
    TotalSize: Int64; // 实时估算的总大小。单位：B
    CurrentSize: Int64; // 已下载大小。单位：B

    StartCount: Int64; // 用于计时
    NewCount: Int64;
    SpeedCount: Int64;
    Speed: Extended; // 单位：B/s
    ElapsedTime: Extended;
    RemainingTime: Extended;

    VideoPartCount: Integer;
    AudioPartCount: Integer;
    NextVideoIndex: Integer; // 下一个准备merge的video分片文件编号
    NextAudioIndex: Integer; // 下一个准备merge的audio分片文件编号
    ExistVideos: Booleans; // 默认False default; 对应分片文件下载完后为True
    ExistAudios: Booleans; // 默认False default; 对应分片文件下载完后为True
    MovieFileStream: TFileStream;
    VideoOffsets: TStringList;
    AudioOffsets: TStringList;

    DownloadList: THttpDownloadList;
    MergeLock: TCriticalSection;
    HttpRequestMsg: THttpMessage;
  end;

procedure NewSLTask(var p: PSLTask; NewTask: TSLNewTask);
procedure DisposeSLTask(p: PSLTask);

function GetFrequency: TLargeInteger;

implementation

uses
  SysUtils, Windows;

function GetGUID: string;
var
  guid: TGUID;
begin
  Result := EmptyStr;
  if CreateGUID(guid) = S_OK then
  begin
    Result := GUIDToString(guid);
  end;
end;

var
  FrequencyValue: TLargeInteger = 0;

function GetFrequency: TLargeInteger;
begin
  if FrequencyValue = 0 then
  begin
    QueryPerformanceFrequency(FrequencyValue);
  end;
  Result := FrequencyValue;
end;

procedure NewSLTask(var p: PSLTask; NewTask: TSLNewTask);
begin
  New(p);
  with p^ do
  begin
    ID := GetGUID;
    PageUrl := NewTask.PageUrl;
    XmlUrl := NewTask.XmlUrl;
    XmlFile := NewTask.XmlFile;
    VideoIndex := NewTask.VideoIndex;
    AudioIndex := NewTask.AudioIndex;
    MovieFile := NewTask.MovieFile;
    LoadIsmvSettings(@MovieSettings, NewTask.XmlFile, VideoIndex, AudioIndex);
    TaskState := tsInit;

    TotalSize := 0;
    CurrentSize := 0;

    QueryPerformanceCounter(StartCount);
    QueryPerformanceCounter(NewCount);
    Speed := 0;
    ElapsedTime := 0;
    RemainingTime := 0;

    VideoPartCount := MovieSettings.VideoStartTimes.Count;
    AudioPartCount := MovieSettings.AudioStartTimes.Count;
    NextVideoIndex := 0;
    NextAudioIndex := 0;

    SetLength(ExistVideos, VideoPartCount);
    SetLength(ExistAudios, AudioPartCount);
    VideoOffsets := TStringList.Create;
    AudioOffsets := TStringList.Create;

    DownloadList := nil;
    MovieFileStream := nil;
    MergeLock := TCriticalSection.Create;
    HttpRequestMsg := NewTask.HttpRequestMsg;
  end;
end;

procedure DisposeSLTask(p: PSLTask);
begin
  with p^ do
  begin
    SetLength(ExistVideos, 0);
    SetLength(ExistAudios, 0);
    VideoOffsets.Free;
    AudioOffsets.Free;
    MergeLock.Free;
    if Assigned(HttpRequestMsg) then FreeAndNil(HttpRequestMsg);
  end;
  Dispose(p);
end;

end.

