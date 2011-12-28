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
    PageUrl: string; // ��Ƶ������ҳ��URL
    XmlUrl: string; // �����Ӧ��manifest.xml�ļ���URL
    XmlFile: string; // ����manifest.xml�ļ��ı����ļ�����������·����
    VideoIndex: Integer; // ѡ�����Ƶ��������ţ���һ����0��
    AudioIndex: Integer; // ѡ�����Ƶ��������ţ���һ����0��
    MovieFile: string; // ������Ƶ�ı����ļ�����������·����
    HttpRequestMsg: THttpMessage;
  end;

  PSLTask = ^TSLTask;
  TSLTask = record
    ID: TSLTaskID; // �����Ψһ��ʶ����������ʱ�ɳ�������
    PageUrl: string; // ��Ƶ������ҳ��URL
    XmlUrl: string; // �����Ӧ��manifest.xml�ļ���URL
    XmlFile: string; // ����manifest.xml�ļ��ı����ļ�����������·����
    VideoIndex: Integer; // ѡ�����Ƶ��������ţ���һ����0��
    AudioIndex: Integer; // ѡ�����Ƶ��������ţ���һ����0��
    MovieFile: string; // ������Ƶ�ı����ļ�����������·����
    MovieSettings: TIsmvSettings;
    TaskState: TSLTaskState; // ����״̬
    TotalSize: Int64; // ʵʱ������ܴ�С����λ��B
    CurrentSize: Int64; // �����ش�С����λ��B

    StartCount: Int64; // ���ڼ�ʱ
    NewCount: Int64;
    SpeedCount: Int64;
    Speed: Extended; // ��λ��B/s
    ElapsedTime: Extended;
    RemainingTime: Extended;

    VideoPartCount: Integer;
    AudioPartCount: Integer;
    NextVideoIndex: Integer; // ��һ��׼��merge��video��Ƭ�ļ����
    NextAudioIndex: Integer; // ��һ��׼��merge��audio��Ƭ�ļ����
    ExistVideos: Booleans; // Ĭ��False default; ��Ӧ��Ƭ�ļ��������ΪTrue
    ExistAudios: Booleans; // Ĭ��False default; ��Ӧ��Ƭ�ļ��������ΪTrue
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

