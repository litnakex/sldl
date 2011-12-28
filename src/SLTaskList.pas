unit SLTaskList;

interface

uses
  Classes;

function LoadSLTaskList(IniFile: string): TList;
procedure SaveSLTaskList(list: TList; inifile: string);
procedure FreeSLTaskList(list: TList);

implementation

uses
  SysUtils, SyncObjs, EncdDecd, IniFiles, SLTask, IsmvSettings,
  HttpDownload, HttpMessage;

const
  SID = 'ID';
  SPageUrl = 'PageUrl';
  SXmlUrl = 'XmlUrl';
  SXmlFile = 'XmlFile';
  SVideoIndex = 'VideoIndex';
  SAudioIndex = 'AudioIndex';
  SMovieFile = 'MovieFile';
  STotalSize = 'TotalSize';
  SCurrentSize = 'CurrentSize';
  SVideoPartCount = 'VideoPartCount';
  SAudioPartCount = 'AudioPartCount';
  SNextVideoIndex = 'NextVideoIndex';
  SNextAudioIndex = 'NextAudioIndex';
  SExistVideos = 'ExistVideos';
  SExistAudios = 'ExistAudios';
  SVideoOffsets = 'VideoOffsets';
  SAudioOffsets = 'AudioOffsets';
  SHttpRequestMessage = 'HttpRequestMessage';

  COffsetsSeq = ',';

function BoolArrToStr(arr: Booleans): string;
var
  p: array of Char;
  i: Integer;
  len: Integer;
begin
  len := Length(arr);
  SetLength(p, len + 1);
  for i := 0 to len - 1 do
  begin
    if arr[i] then
      p[i] := '1'
    else
      p[i] := '0';
  end;
  Result := string(p);
  SetLength(p, 0);
end;

procedure StrToBoolArr(str: string; out result: Booleans);
var
  Len: Integer;
  i: Integer;
begin
  len := Length(str);
  SetLength(Result, Len);
  for i := 1 to len do
  begin
    if str[i] = '1' then
      Result[i - 1] := True
    else
      Result[i - 1] := False;
  end;
end;

procedure Split(str: string; sep: Char; out result: TStringList);
var
  i: Integer;
begin
  with TStringList.Create do
  begin
    Delimiter := sep;
    DelimitedText := str;
    for i := 0 to Count - 1 do
    begin
      result.Add(Strings[i]);
    end;
  end;
end;

function Joint(strs: TStringList; joint: Char): string;
begin
  strs.Delimiter := joint;
  result := strs.DelimitedText;
end;

function LoadSLTaskList(IniFile: string): TList;
var
  ini: TIniFile;
  sections, sectionValues: TStringList;
  section: string;
  i: Integer;
  PTask: PSLTask;
  MsgStr: string;
begin
  Result := TList.Create;
  if not FileExists(IniFile) then Exit;

  ini := TIniFile.Create(IniFile);
  sections := TStringList.Create;
  try
    ini.ReadSections(sections);
    for i := 0 to sections.Count - 1 do
    begin
      section := sections[i]; // ID
      sectionValues := TStringList.Create;
      try
        ini.ReadSectionValues(section, sectionValues);
        New(PTask);
        try
          with PTask^ do
          begin
            TaskState := tsStopped;
            ID := section;
            PageUrl := sectionValues.Values[SPageUrl];
            XmlUrl := sectionValues.Values[SXmlUrl];
            XmlFile := sectionValues.Values[SXmlFile];
            MovieFile := sectionValues.Values[SMovieFile];
            VideoIndex := StrToInt(sectionValues.Values[SVideoIndex]);
            AudioIndex := StrToInt(sectionValues.Values[SAudioIndex]);
            LoadIsmvSettings(@MovieSettings, XmlFile, VideoIndex, AudioIndex);
            TotalSize := StrToInt64(sectionValues.Values[STotalSize]);
            CurrentSize := StrToInt64(sectionValues.Values[SCurrentSize]);
            Speed := 0;

            VideoPartCount := StrToInt(sectionValues.Values[SVideoPartCount]);
            AudioPartCount := StrToInt(sectionValues.Values[SAudioPartCount]);
            NextVideoIndex := StrToInt(sectionValues.Values[SNextVideoIndex]);
            NextAudioIndex := StrToInt(sectionValues.Values[SNextAudioIndex]);
            SetLength(ExistVideos, VideoPartCount);
            SetLength(ExistAudios, AudioPartCount);
            VideoOffsets := TStringList.Create;
            AudioOffsets := TStringList.Create;
            Split(sectionValues.Values[SVideoOffsets], COffsetsSeq, VideoOffsets);
            Split(sectionValues.Values[SAudioOffsets], COffsetsSeq, AudioOffsets);
            if sectionValues.IndexOf(SHttpRequestMessage) >= 0 then
            begin
              MsgStr := DecodeString(sectionValues.Values[SHttpRequestMessage]);
              HttpRequestMsg := THttpMessage.Create(MsgStr);
            end;

            DownloadList := nil;
            MovieFileStream := nil;
            MergeLock := TCriticalSection.Create;
            HttpRequestMsg := nil;
          end;
        except
          Dispose(PTask);
          Continue;
        end;
        Result.Add(PTask);
      finally
        sectionValues.Free;
      end;
    end;
  finally
    sections.Free;
    ini.Free;
  end;
end;

procedure SaveSLTaskList(list: TList; inifile: string);
var
  ini: TIniFile;
  i: Integer;
begin
  with TStringList.Create do
    SaveToFile(inifile);

  ini := TIniFile.Create(inifile);
  try
    for i := 0 to list.Count - 1 do
    begin
      with PSLTask(list[i])^ do
      begin
        ini.WriteString(ID, SPageUrl, PageUrl);
        ini.WriteString(ID, SXmlUrl, XmlUrl);
        ini.WriteString(ID, SXmlFile, XmlFile);
        ini.WriteString(ID, SMovieFile, MovieFile);
        ini.WriteInteger(ID, SVideoIndex, VideoIndex);
        ini.WriteInteger(ID, SAudioIndex, AudioIndex);
        ini.WriteInteger(ID, STotalSize, TotalSize);
        ini.WriteInteger(ID, SCurrentSize, CurrentSize);
        ini.WriteInteger(ID, SVideoPartCount, VideoPartCount);
        ini.WriteInteger(ID, SAudioPartCount, AudioPartCount);
        ini.WriteInteger(ID, SNextVideoIndex, NextVideoIndex);
        ini.WriteInteger(ID, SNextAudioIndex, NextAudioIndex);
        ini.WriteString(ID, SVideoOffsets, Joint(VideoOffsets, COffsetsSeq));
        ini.WriteString(ID, SAudioOffsets, Joint(AudioOffsets, COffsetsSeq));
        if Assigned(HttpRequestMsg) then
        begin
          ini.WriteString(ID, SHttpRequestMessage, EncodeString(HttpRequestMsg.ToString));
        end;
      end;
    end;
  finally
    ini.Free;
  end;
end;

procedure FreeSLTaskList(list: TList);
var
  i: Integer;
begin
  for i := 0 to list.Count - 1 do
  begin
    DisposeSLTask(list[i]);
  end;
  list.Free;
end;

end.

