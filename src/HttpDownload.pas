unit HttpDownload;

interface

uses
  Classes, SysUtils, HttpMessage, HttpHeaderExtracter, GFHTTPGet2, MyUtils2,
  Windows, SyncObjs{$IFDEF DEBUG}, ULog{$ENDIF};

const
  CDefaultThreasNum: Integer = 10;

type
  THttpDownloadList = class
  private
    FOwner: TComponent;
    FCountDone: Integer;
    FCountRunning: Integer; // 已用线程数
    FLimitRunning: Integer; // 最大线程数
    FCountTotal: Integer;

    FDownloadList: TList;
    FMessageList: TList;

    FProgressChange: TOnProgressChange;
    FAllFilesDone: TOnAllFilesDone;
    FOnFileDone: TOnFileDone;
  public
    constructor Create(aOwner: TComponent);
    destructor Destroy; override;

      { 添加新的下载项。aFileName中不包含文件扩展名，程序会自动检测文件类型并添加相应扩展名 }
    procedure Add(aRequestMessage: THttpMessage; aUrl: string; aFileName: string); overload;
    procedure Add(aUrl, aFileName: string); overload;

    function Count: Integer;
    function At(aIndex: Integer): TGFHTTPGet;
    procedure Stop(aIndex: Integer);
    procedure Pause(aIndex: Integer);
    procedure Remove(aIndex: Integer);

  private
    function GetDownloader(aIndex: Integer): TGFHTTPGet;
    function GetMessage(aIndex: Integer): THttpMessage;
    procedure SetMaxThreadNum(const aNum: Integer);

    procedure DoneFile(Sender: TObject; FileName: string; FileSize: Integer);
    procedure StopAndFree(aIndex: Integer);

  published
    property MaxThreadsNum: Integer read FLimitRunning write SetMaxThreadNum;
    property OnFileDone: TOnFileDone read FOnFileDone write FOnFileDone;
    property OnProgressChange: TOnProgressChange read FProgressChange write FProgressChange;
    property OnAllFilesDone: TOnAllFilesDone read FAllFilesDone write FAllFilesDone;

  end;

implementation

var
  FCriticalSection: TCriticalSection;

{ THttpDownload }

procedure THttpDownloadList.Add(aUrl, aFileName: string);
begin
  Add(nil, aUrl, aFileName);
end;

procedure THttpDownloadList.Add(aRequestMessage: THttpMessage; aUrl: string;
  aFileName: string);
var
  NewDownload: TGFHTTPGet;
  I, count: Integer;
begin
  Inc(FCountTotal);

  count := Self.Count;
  for I := 0 to count - 1 do
  begin
    with GetDownloader(I) do
      if (URL = aUrl) and (FileName = aFileName) then
      begin
        Remove(I);
        Break;
      end;
  end;

  NewDownload := TGFHTTPGet.Create(FOwner);
  FDownloadList.Add(NewDownload);
  FMessageList.Add(aRequestMessage);
  if aRequestMessage <> nil then
  begin
    NewDownload.PostQuery := aRequestMessage.Entity;
    NewDownload.OtherHeaders := THttpHeaderExtracter.Create(
      aRequestMessage.ToString).ToString;
  end;
  
  NewDownload.AcceptTypes := '*/*';
  NewDownload.FileName := Copy(aFileName, 0, Length(aFileName)-Length(ExtractFileExt(aFileName)));
  NewDownload.FileExtName := ExtractFileExt(aFileName);
  NewDownload.URL := aUrl;
  NewDownload.OnDoneFile := DoneFile;

  if FCountRunning < FLimitRunning then
  begin
    FCriticalSection.Enter;
    try
      Inc(FCountRunning);
      NewDownload.RewriteIfExisted := True;
      NewDownload.GetFile;
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

function THttpDownloadList.At(aIndex: Integer): TGFHTTPGet;
begin
  Result := GetDownloader(aIndex);
end;

function THttpDownloadList.Count: Integer;
begin
  Result := FDownloadList.Count;
end;

constructor THttpDownloadList.Create(aOwner: TComponent);
begin
  FOwner := aOwner;
  FLimitRunning := CDefaultThreasNum;
  FCountRunning := 0;
  FCountDone := 0;
  FCountTotal := 0;
  FDownloadList := TList.Create;
  FMessageList := TList.Create;
  FOnFileDone := nil;
  FProgressChange := nil;
end;

destructor THttpDownloadList.Destroy;
var
  I: Integer;
begin
  for I := 0 to FDownloadList.Count - 1 do
  begin
    StopAndFree(I);
  end;
  FDownloadList.Free;
  FMessageList.Free;
  inherited;
end;

procedure THttpDownloadList.DoneFile(Sender: TObject; FileName: string;
  FileSize: Integer);
var
  I: Integer;
//  count: Integer;
begin
{$IFDEF DEBUG}
  Log.AddInfo('HttpDownload: [' + FileName + '] Downloaded');
{$ENDIF}

  FCriticalSection.Enter;
  try
    try
//    count := Self.Count;
      Inc(FCountDone);

      if Assigned(FOnFileDone) then
      begin
        FOnFileDone(Sender, FileName, FileSize);
      end;

      if Assigned(FProgressChange) then
      begin
        FProgressChange(Self, FCountDone, FCountTotal, CStateDownloadingSplits);
      end;

      for I := 0 to self.count - 1 do
      begin
        if GetDownloader(I) = TGFHTTPGet(Sender) then
        begin
{$IFDEF DEBUG}
          Log.AddInfo('HttpDownload: [' + GetDownloader(I).FileName + '] Removed');
{$ENDIF}
          Remove(I);
          Dec(FCountRunning);
          Break;
        end;
      end;

      if self.Count <= 0 then
      begin
{$IFDEF DEBUG}
        Log.AddInfo('HttpDownload: All splits Downloaded');
{$ENDIF}
        if Assigned(FAllFilesDone) then FAllFilesDone(Self);
        FCountTotal := 0;
        Exit;
      end;

      for I := 0 to self.Count - 1 do
      begin
        if not GetDownloader(I).IsBusy then
        begin
          Inc(FCountRunning);
{$IFDEF DEBUG}
          Log.AddInfo('HttpDownload: start download split [' + GetDownloader(I).FileName + ']');
{$ENDIF}
          GetDownloader(I).RewriteIfExisted := true;
          GetDownloader(I).GetFile;
          Break;
        end;
      end;
    except
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function THttpDownloadList.GetDownloader(aIndex: Integer): TGFHTTPGet;
begin
  Result := TGFHTTPGet(FDownloadList[aIndex]);
end;

function THttpDownloadList.GetMessage(aIndex: Integer): THttpMessage;
begin
  Result := THttpMessage(FMessageList[aIndex]);
end;

procedure THttpDownloadList.Pause(aIndex: Integer);
begin
  GetDownloader(aIndex).Pause;
end;

procedure THttpDownloadList.Remove(aIndex: Integer);
begin
  if (aIndex < 0) or (aIndex >= FDownloadList.Count) then Exit;
  StopAndFree(aIndex);
  FDownloadList.Delete(aIndex);
  FMessageList.Delete(aIndex);
end;

procedure THttpDownloadList.SetMaxThreadNum(const aNum: Integer);
var
  I, n: Integer;
  count: Integer;
begin
  FCriticalSection.Enter;
  try
    if aNum < FCountRunning then
    begin
      n := 0;
      count := FDownloadList.Count;
      for I := 0 to count - 1 do
      begin
        if n >= FCountRunning then
        begin
          if GetDownloader(I).IsBusy then
          begin
            GetDownloader(I).Pause;
            GetDownloader(I).IsBusy := False;
            Continue;
          end;
        end;

        if GetDownloader(I).IsBusy then
          Inc(n);
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure THttpDownloadList.Stop(aIndex: Integer);
begin
  StopAndFree(aIndex);
end;

procedure THttpDownloadList.StopAndFree(aIndex: Integer);
var
  p: TGFHTTPGet;
begin
  p := GetDownloader(aIndex);
  p.ClearSizeRecord;
  p.OnProgress := nil;
  p.OnDoneFile := nil;
  p.OnDoneFile := nil;
  p.OnError := nil;
  while p.IsBusy do
  begin
    p.Stop;
    Sleep(1);
  end;
  p.Free;
  GetMessage(aIndex).Free;
end;

initialization
  FCriticalSection := TCriticalSection.Create;

finalization
  FCriticalSection.Free;

end.

