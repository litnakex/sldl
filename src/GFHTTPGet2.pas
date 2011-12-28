{*************************************************************}
{            HTTPGet component for Delphi 32                  }
{ Version:   1.94                                             }
{ E-Mail:    info@utilmind.com                                }
{ WWW:       http://www.utilmind.com                          }
{ Created:   October  19, 1999                                }
{ Modified:  February 21, 2001                                }
{ Legal:     Copyright (c) 1999-2000, UtilMind Solutions      }
{*************************************************************}
{ PROPERTIES:                                                 }
{   Agent: String - User Agent                                }
{                                                             }
{*  BinaryData: Boolean - This setting specifies which type   }
{*                        of data will taken from the web.    }
{*                        If you set this property TRUE then  }
{*                        component will determinee the size  }
{*                        of files *before* getting them from }
{*                        the web.                            }
{*                        If this property is FALSE then as we}
{*                        do not knows the file size the      }
{*                        OnProgress event will doesn't work. }
{*                        Also please remember that is you set}
{*                        this property as TRUE you will not  }
{*                        capable to get from the web ASCII   }
{*                        data and ofter got OnError event.   }
{                                                             }
{   FileName: String - Path to local file to store the data   }
{                      taken from the web                     }
{   Password, UserName - set this properties if you trying to }
{                        get data from password protected     }
{                        directories.                         }
{   Referer: String - Additional data about referer document  }
{   URL: String - The url to file or document                 }
{   UseCache: Boolean - Get file from the Internet Explorer's }
{                       cache if requested file is cached.    }
{*************************************************************}
{ METHODS:                                                    }
{   GetFile - Get the file from the web specified in the URL  }
{             property and store it to the file specified in  }
{             the FileName property                           }
{   GetString - Get the data from web and return it as usual  }
{               String. You can receive this string hooking   }
{               the OnDoneString event.                       }
{   Abort - Stop the current session                          }
{*************************************************************}
{ EVENTS:                                                     }
{   OnDoneFile - Occurs when the file is downloaded           }
{   OnDoneString - Occurs when the string is received         }
{   OnError - Occurs when error happend                       }
{   OnProgress - Occurs at the receiving of the BINARY DATA   }
{*************************************************************}
{ Please see demo program for more information.               }
{*************************************************************}
{                     IMPORTANT NOTE:                         }
{ This software is provided 'as-is', without any express or   }
{ implied warranty. In no event will the author be held       }
{ liable for any damages arising from the use of this         }
{ software.                                                   }
{ Permission is granted to anyone to use this software for    }
{ any purpose, including commercial applications, and to      }
{ alter it and redistribute it freely, subject to the         }
{ following restrictions:                                     }
{ 1. The origin of this software must not be misrepresented,  }
{    you must not claim that you wrote the original software. }
{    If you use this software in a product, an acknowledgment }
{    in the product documentation would be appreciated but is }
{    not required.                                            }
{ 2. Altered source versions must be plainly marked as such,  }
{    and must not be misrepresented as being the original     }
{    software.                                                }
{ 3. This notice may not be removed or altered from any       }
{    source distribution.                                     }
{*************************************************************}

unit GFHTTPGet2;

interface

uses
  Windows, Messages, SysUtils, Classes, WinInet, Forms, HttpMessage,
  HttpHeaderExtracter, IntelligentExt;

const
  DEF_INI_FILE_EXT = '.ini'; //�ļ����ص�˵���ļ�
  MSGBX_TITLE_ALERT = 'ע��';
  MSGBX_TEXT_FILEEXISTS = '�ļ��Ѵ��ڣ����Ǿ��ļ��������أ�';

type
  TOnProgressEvent = procedure(Sender: TObject; TotalSize, Readed: Integer) of object;
  TOnDoneFileEvent = procedure(Sender: TObject; FileName: string; FileSize: Integer) of object;
  TOnDoneStringEvent = procedure(Sender: TObject; Result: string) of object;

  TGFHTTPGet = class;

  THTTPGetThread = class(TThread)
  private
    FTAcceptTypes,
      FTAgent,
      FTURL,
      FTFileName,
      FTFileExtName,
      FTStringResult,
      FTUserName,
      FTPassword,
      FTPostQuery,
      FTReferer: string;
    FTBinaryData,
      FTUseCache: Boolean;
    FTResult: Boolean;
    FTFileSize: Integer;
    FTToFile: Boolean;
    FOtherHeaders: RHttpMessageHeaders;

    FTRewriteIfExisted: Boolean;

    BytesToRead, BytesReaded: DWord;
    LocalFileSize: int64;
    FTProgress: TOnProgressEvent;
    FSpeed: Double;
    FElapsedTime: Double;
    FSpeedCount: TLargeInteger;
    FStartCount: TLargeInteger;
    FRemainingTime: Double;
    FHttpget: TGFHTTPGet;
   // FFileSize: integer;
    procedure UpdateProgress;
    procedure SetSpeed(const Value: Double);
    procedure SetElapsedTime(const Value: Double);
    class function GetFrequency: TLargeInteger;
    function GetRemainingTime: Double;
    procedure SetRemainingTime(const Value: Double);
    procedure SetFileSize(const Value: integer);
  protected
    procedure Execute; override;
  public
    constructor Create(aOwer: TGFHTTPGet;
      aAcceptTypes, aAgent, aURL, aFileName, aFileExtName, aUserName, aPassword, aPostQuery, aReferer: string;
      aBinaryData, aUseCache: Boolean; aOtherHeaders: RHttpMessageHeaders;
      aProgress: TOnProgressEvent; aToFile: Boolean); overload;
    constructor Create(aOwer: TGFHTTPGet;
      aAcceptTypes, aAgent, aURL, aFileName, aFileExtName, aUserName, aPassword, aPostQuery, aReferer: string;
      aBinaryData, aUseCache: Boolean;
      aProgress: TOnProgressEvent; aToFile: Boolean); overload;
  published
    property Speed: Double read FSpeed write SetSpeed;
    property ElapsedTime: Double read FElapsedTime write SetElapsedTime;
    property RemainingTime: Double read GetRemainingTime;
    property FileSize: integer read FTFileSize write SetFileSize;
    property RewriteIfExisted: Boolean read FTRewriteIfExisted write FTRewriteIfExisted;
  end;

  TGFHTTPGet = class(TComponent)
  private
    FAcceptTypes: string;
    FAgent: string;
    FBinaryData: Boolean;
    FURL: string;
    FUseCache: Boolean;
    FFileName: string;
    FFileExtName: string;
    FUserName: string;
    FPassword: string;
    FPostQuery: string;
    FReferer: string;
    FOtherHeaders: string;
    FWaitThread: Boolean;
    FRewriteIfExisted: Boolean;

    FThread: THTTPGetThread;
    FError: TNotifyEvent;
    FResult: Boolean;

    FProgress: TOnProgressEvent;
    FDoneFile: TOnDoneFileEvent;
    FDoneString: TOnDoneStringEvent;
    FSpeed: Double;
    FIsBusy: boolean;

    procedure ThreadDone(Sender: TObject);

    procedure SetSpeed(const Value: Double);
    procedure SetIsBusy(const Value: boolean);

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetFile;
    procedure GetString;
    procedure Pause;
    procedure resume;
    procedure Stop;
    procedure ClearSizeRecord();
  published
    property AcceptTypes: string read FAcceptTypes write FAcceptTypes;
    property Agent: string read FAgent write FAgent;
    property BinaryData: Boolean read FBinaryData write FBinaryData;
    property URL: string read FURL write FURL;
    property UseCache: Boolean read FUseCache write FUseCache;
    property FileName: string read FFileName write FFileName;
    property FileExtName: string read FFileExtName write FFileExtName;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property PostQuery: string read FPostQuery write FPostQuery;
    property Referer: string read FReferer write FReferer;
    property OtherHeaders: string read FOtherHeaders write FOtherHeaders;
    property WaitThread: Boolean read FWaitThread write FWaitThread;
    property Speed: Double read FSpeed write SetSpeed;
    property OnProgress: TOnProgressEvent read FProgress write FProgress;
    property OnDoneFile: TOnDoneFileEvent read FDoneFile write FDoneFile;
    property OnDoneString: TOnDoneStringEvent read FDoneString write FDoneString;
    property OnError: TNotifyEvent read FError write FError;
    property IsBusy: boolean read FIsBusy write SetIsBusy;
    property RewriteIfExisted: Boolean read FRewriteIfExisted write FRewriteIfExisted;
  end;

procedure Register;

implementation

//  THTTPGetThread

constructor THTTPGetThread.Create(aOwer: TGFHTTPGet;
  aAcceptTypes, aAgent, aURL, aFileName, aFileExtName, aUserName, aPassword, aPostQuery, aReferer: string;
  aBinaryData, aUseCache: Boolean; aOtherHeaders: RHttpMessageHeaders;
  aProgress: TOnProgressEvent; aToFile: Boolean);
begin
  FreeOnTerminate := True;
  inherited Create(True);
  FHttpget := aOwer;
  FTAcceptTypes := aAcceptTypes;
  FTAgent := aAgent;
  FTURL := aURL;
  FTFileName := aFileName;
  FTFileExtName := aFileExtName;
  FTUserName := aUserName;
  FTPassword := aPassword;
  FTPostQuery := aPostQuery;
  FTReferer := aReferer;
  FTProgress := aProgress;
  FTBinaryData := aBinaryData;
  FTUseCache := aUseCache;
  FOtherHeaders := aOtherHeaders;
  FTRewriteIfExisted := False;

  FTToFile := aToFile;
  FStartCount := 0;
  QueryPerformanceCounter(FStartCount);
  Resume;
end;

constructor THTTPGetThread.Create(aOwer: TGFHTTPGet; aAcceptTypes, aAgent,
  aURL, aFileName, aFileExtName, aUserName, aPassword, aPostQuery, aReferer: string;
  aBinaryData, aUseCache: Boolean; aProgress: TOnProgressEvent;
  aToFile: Boolean);
var
  Headers: RHttpMessageHeaders;
begin
  Headers.Count := 0;
  SetLength(Headers.Names, 0);
  SetLength(Headers.Values, 0);
  Create(aOwer, aAcceptTypes, aAgent, aURL, aFileName, aFileExtName,
    aUserName, aPassword, aPostQuery, aReferer, aBinaryData,
    aUseCache, Headers, aProgress, aToFile);
end;

procedure THTTPGetThread.UpdateProgress;
var
  NewCount: TLargeInteger;
begin
  FSpeed := 0;
  FElapsedTime := 0;
  if QueryPerformanceCounter(NewCount) and (GetFrequency() > 0) then
  begin
    FSpeed := (BytesReaded) / ((NewCount - FStartCount) / GetFrequency());
    FSpeedCount := NewCount;
    FElapsedTime := (FSpeedCount - FStartCount) / GetFrequency();
  end;
  FTProgress(FHttpget, FTFileSize, BytesReaded + LocalFileSize);
end;

procedure THTTPGetThread.Execute;
var
  hSession, hConnect, hRequest: hInternet;
  HostName, FileName, HostPort: string;
  f: file;
  Buf: Pointer;
  dwBufLen, dwIndex: DWord;
  Data: array[0..$400] of Char;
  TempStr: string;
  RequestMethod: PChar;
  InternetFlag: DWord;
  AcceptType: LPStr;
  nPort: integer;
  //=====================================
  //==�ϵ���������
  IniFile: string;
  ServerModiDate, ModiDate: string;
  UrlHeader: string;
  pDate: Pointer;
  dwDateLen, dwIndex2: DWord;
  FTResult2: Boolean;
  //=====================================
  ErrCode: Integer;
  I: Integer;
  RequestStr: string;
  FirstLoop: Boolean;
  MBID: Integer;

  procedure ParseURL(URL: string; var HostName, FileName, HostPort: string);

    procedure ReplaceChar(c1, c2: Char; var St: string);
    var
      p: Integer;
    begin
      while True do
      begin
        p := Pos(c1, St);
        if p = 0 then
          Break
        else
          St[p] := c2;
      end;
    end;

  var
    i: Integer;
    sPortPos, sPortPos2, ePortPos: integer;
  begin
    if Pos('http://', LowerCase(URL)) <> 0 then
      System.Delete(URL, 1, 7);

    i := Pos('/', URL);
    if i = 0 then
    begin
      HostName := URL;
      Exit;
    end;
    HostName := Copy(URL, 1, i - 1);
    FileName := Copy(URL, i, Length(URL) - i + 1);

    sPortPos := Pos(':', Url);
    sPortPos2 := Pos('://', Url);
    if sPortPos2 <> sPortPos then
    begin
      if sPortPos > 0 then
      begin
        ePortPos := Pos('/', Url);
        HostPort := Copy(Url, sPortPos + 1, ePortPos - sPortPos - 1);
        HostName := Copy(HostName, 0, Pos(':', HostName) - 1);
      end;
    end;

    if (Length(HostName) > 0) and (HostName[Length(HostName)] = '/') then
      SetLength(HostName, Length(HostName) - 1);
  end;

  procedure CloseHandles;
  begin
    InternetCloseHandle(hRequest);
    InternetCloseHandle(hConnect);
    InternetCloseHandle(hSession);
  end;

  function GetFileModiDate(FileName: string): string;
  var
    Fs: TStringList;
  begin
    Result := '';
    Fs := TStringList.Create;
    try
      Fs.LoadFromFile(FileName);
      if Fs.Count > 0 then
        Result := Fs.Strings[0];
    finally
      Fs.Free;
    end;
  end;

  function GetFileSize(FileName: string): int64;
  var
    FStream: TFileStream;
  begin
    Result := 0;
    FStream := TFileStream.Create(FileName, fmShareDenyNone);
    try
      Result := FStream.Size;
    finally
      FStream.Free;
    end;
  end;

  procedure SaveToFile1(Scr: string; FileName: string);
  var
    fs: TStringList;
  begin
    try
      if FileExists(FileName) then
        DeleteFile(FileName);
    except
      Exit;
    end;
    fs := TStringList.Create;
    fs.Add(scr);
    fs.SaveToFile(FileName);
    fs.Free;
  end;

begin
  //==========================================================================
  //��鱾���Ƿ������ص�һ����ļ���˵���ļ�
  LocalFileSize := 0;
  IniFile := FTFileName + DEF_INI_FILE_EXT;
  if FileExists(FTFileName) then
  begin
    if FileExists(IniFile) then //ȡ����ʱ���ص��ļ����޸����ڣ����ںͷ������Ƚ��Ƿ���ͬһ�ļ�
    begin
      ModiDate := GetFileModiDate(IniFile);
      if ModiDate <> '' then
      begin
        LocalFileSize := GetFileSize(FTFileName); //�õ����������صĴ�С
      end;
    end;
  end;
  //==========================================================================
  try
    HostPort := '80';
    nPort := 80;
    ParseURL(FTURL, HostName, FileName, HostPort);
    if Pos('?', FileName) > 0 then
      FileName := Copy(FileName, 1, Pos('?', FileName)-1);
    try
      if hostport <> '' then
        nPort := StrToInt(HostPort);
    except
      nPort := 80;
    end;

    if Terminated then
    begin
      FTResult := False;
      Exit;
    end;

    if FTAgent <> '' then
      hSession := InternetOpen(PChar(FTAgent),
        INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0)
    else
      hSession := InternetOpen(nil,
        INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

    hConnect := InternetConnect(hSession, PChar(HostName),
      nPort, PChar(FTUserName), PChar(FTPassword),
      INTERNET_SERVICE_HTTP, 0, 0);

    if FTPostQuery = '' then
      RequestMethod := 'GET'
    else
      RequestMethod := 'POST';

    if FTUseCache then
      InternetFlag := 0
    else
      InternetFlag := INTERNET_FLAG_RELOAD;

    AcceptType := PChar(FTAcceptTypes); //@changed 2010-09-23
    hRequest := HttpOpenRequest(hConnect, RequestMethod, PChar(FileName),
      'HTTP/1.1',
      PChar(FTReferer), @AcceptType, InternetFlag, 0);

    { ��������� }
    RequestStr := '';
    for I := 0 to FOtherHeaders.Count - 1 do
    begin
      RequestStr := RequestStr + FOtherHeaders.Names[I] + ': ' + FOtherHeaders.Values[I] + #13#10;
    end;
    if FTPostQuery = '' then
    begin
      if I = 0 then
        HttpSendRequest(hRequest, nil, 0, nil, 0)
      else
        HttpSendRequest(hRequest, PChar(RequestStr), Length(RequestStr), nil, 0);
    end
    else
    begin
      if I = 0 then
        HttpSendRequest(hRequest, nil, 0, PChar(FTPostQuery), Length(FTPostQuery))
      else
        HttpSendRequest(hRequest, PChar(RequestStr), Length(RequestStr),
          PChar(FTPostQuery), Length(FTPostQuery));
    end;

    if Terminated then
    begin
      CloseHandles;
      FTResult := False;
      Exit;
    end;

    dwIndex := 0;
    dwBufLen := 2048;
    GetMem(Buf, dwBufLen);

    FTResult := HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH,
      Buf, dwBufLen, dwIndex);

    if FTResult or not FTBinaryData then
    begin
      if FTResult then
        FTFileSize := StrToInt(StrPas(Buf));
    end;
    //=====================================================
    dwIndex2 := 0;
    dwDateLen := 256;
    GetMem(pDate, dwDateLen);

    FTResult2 := HttpQueryInfo(hRequest, HTTP_QUERY_LAST_MODIFIED,
      pDate, dwDateLen, dwIndex2);

    if FTresult2 then
    begin
      ServerModiDate := StrPas(pDate);
      if ServerModiDate <> '' then
      begin
        SaveToFile1(ServerModiDate, IniFile);
      end;
    end;

    if Terminated then
    begin
      FreeMem(Buf);
      FreeMem(pDate);
      CloseHandles;
      FTResult := False;
      Exit;
    end;

    //============================================================================
    //�����жϷ�����֧�ֲ�֧�ֶϵ���������֧�֣���ɾ�������ļ�����������.
    //֧�֣��������µ�����ͷ�ַ�����send����������
    //============================================================================
    BytesReaded := 0;

    if (ServerModiDate = ModiDate) and (ServerModiDate <> '') then
    begin // ֧�ֶϵ�����
      InternetCloseHandle(hRequest);
      hRequest := HttpOpenRequest(hConnect, RequestMethod, PChar(FileName),
        'HTTP/1.1', PChar(FTReferer), @AcceptType, InternetFlag, 0);
      if FTPostQuery = '' then
      begin
        UrlHeader := 'RANGE: bytes=' + IntToStr(LocalFileSize) + '-' +
          IntToStr(FTFileSize)
          + #13#10;
        for I := 0 to FOtherHeaders.Count - 1 do
        begin
          RequestStr := RequestStr + FOtherHeaders.Names[I] + ': '
            + FOtherHeaders.Values[I] + #13#10;
        end;
        RequestStr := RequestStr + UrlHeader;
        HttpSendRequest(hRequest, PChar(RequestStr), Length(RequestStr), nil, 0);
      end
      else
      begin
        UrlHeader := 'Content-Type: application/x-www-form-urlencoded' + #13#10
          +
          'RANGE: bytes=' + IntToStr(LocalFileSize) + '-' + IntToStr(FTFileSize)
          + #13#10;
        HttpSendRequest(hRequest, pchar(UrlHeader), Length(UrlHeader),
          PChar(FTPostQuery), Length(FTPostQuery));
      end;
    end;

    FirstLoop := True;
    while True do
    begin
      Application.ProcessMessages;
      if Terminated then
      begin
        if (not FirstLoop) and FTToFile then
          CloseFile(f);
        FreeMem(Buf);
        FreeMem(pDate);
        CloseHandles;

        FTResult := False;
        Exit;
      end;

      if not InternetReadFile(hRequest, @Data, SizeOf(Data), BytesToRead) then
        Break
      else if BytesToRead = 0 then
        Break;

      if FirstLoop and FTToFile then
      begin
        if FTFileExtName = '' then
        begin
          FTFileExtName := GetIntelligentExt(Data);
        end;
        FTFileName := FTFileName + FTFileExtName;
        AssignFile(f, FTFileName);
        if FileExists(FTFileName) then
        begin
          if (ServerModiDate = ModiDate) and (ModiDate <> '') then
          begin // ����
            reset(f, 1);
            seek(f, localFileSize);
          end
          else
          begin
            if (FTRewriteIfExisted)
              or (Application.MessageBox(MSGBX_TEXT_FILEEXISTS,
              MSGBX_TITLE_ALERT, MB_OKCANCEL) = ID_OK) then
            begin
              Rewrite(f, 1);
            end
            else
              Break;
          end;
        end
        else
        begin
          Rewrite(f, 1);
        end;
      end
      else
        FTStringResult := '';

      if FTToFile then
        BlockWrite(f, Data, BytesToRead)
      else
      begin
        TempStr := Data;
        SetLength(TempStr, BytesToRead);
        FTStringResult := FTStringResult + TempStr;
      end;

      inc(BytesReaded, BytesToRead);
      if Assigned(FTProgress) then
        Synchronize(UpdateProgress);

      FirstLoop := False;
    end;

    if FTToFile then
      FTResult := (FTFileSize - LocalFileSize) = Integer(BytesReaded)
    else
    begin
      SetLength(FTStringResult, BytesReaded);
      FTResult := BytesReaded <> 0;
    end;

    if FTToFile and not FirstLoop then
      CloseFile(f);

    FreeMem(Buf);
    FreeMem(pDate);
    CloseHandles;
  except
  end;
//  Sleep(1000);
//  Application.ProcessMessages;
end;
// HTTPGet

constructor TGFHTTPGet.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FAcceptTypes := '*/*';
  FAgent := 'GetFLV HTTPGet';
end;

destructor TGFHTTPGet.Destroy;
begin
//  Abort;
  inherited Destroy;
end;

procedure TGFHTTPGet.GetFile;
var
  Msg: TMsg;
begin
  IsBusy := true;
  if not Assigned(FThread) then
  begin
    FThread := THTTPGetThread.Create(self, FAcceptTypes, FAgent, FURL, FFileName,
      FFileExtName, FUserName, FPassword, FPostQuery, FReferer, FBinaryData,
      FUseCache, THttpHeaderExtracter.Create(FOtherHeaders).GetHttpHeaders,
      FProgress, True);
    FThread.RewriteIfExisted := FRewriteIfExisted;
    FThread.OnTerminate := ThreadDone;
    if FWaitThread then
      while Assigned(FThread) do
        while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
  end;

end;

procedure TGFHTTPGet.GetString;
var
  Msg: TMsg;
begin
  if not Assigned(FThread) then
  begin
    FThread := THTTPGetThread.Create(self, FAcceptTypes, FAgent, FURL, FFileName,
      FFileExtName, FUserName, FPassword, FPostQuery, FReferer, FBinaryData,
      FUseCache, THttpHeaderExtracter.Create(FOtherHeaders).GetHttpHeaders,
      FProgress, False);
    FThread.OnTerminate := ThreadDone;
    if FWaitThread then
      while Assigned(FThread) do
        while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
  end
end;

procedure TGFHTTPGet.Stop;
begin
  IsBusy := false;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.FTResult := False;
  end;
end;

procedure TGFHTTPGet.ThreadDone(Sender: TObject);
begin
  try
    FResult := FThread.FTResult;
    if FResult then
      if FThread.FTToFile then
        if Assigned(FDoneFile) then FDoneFile(Self, FThread.FTFileName, FThread.FTFileSize) else
      else
        if Assigned(FDoneString) then FDoneString(Self, FThread.FTStringResult) else
      else
        if Assigned(FError) then FError(Self);
        FThread := nil;
  except
  end;
end;

procedure Register;
begin
  RegisterComponents('GetFLV', [TGFHTTPGet]);
end;

procedure TGFHTTPGet.SetSpeed(const Value: Double);
begin
  FSpeed := Value;
end;

procedure THTTPGetThread.SetSpeed(const Value: Double);
begin
  FSpeed := Value;
end;

procedure THTTPGetThread.SetElapsedTime(const Value: Double);
begin
  FElapsedTime := Value;
end;

var
  FrequencyValue: TLargeInteger = 0;
class function THTTPGetThread.GetFrequency: TLargeInteger;
begin
  if (FrequencyValue = 0) then
  begin
    QueryPerformanceFrequency(FrequencyValue);
  end;
  Result := FrequencyValue;
end;

procedure THTTPGetThread.SetRemainingTime(const Value: Double);
begin

end;

function THTTPGetThread.GetRemainingTime: Double;
begin
  if (FTFileSize > 0) and (Speed > 0) then
  begin
    Result := (FTFileSize - BytesReaded) / Speed;
  end else
  begin
    Result := 0;
  end;
end;

procedure TGFHTTPGet.Pause;
begin
  if Assigned(FThread) then
  begin
    FThread.Suspend;
//    FThread.FTResult := False;
  end;
end;

procedure TGFHTTPGet.resume;
begin

end;

procedure THTTPGetThread.SetFileSize(const Value: integer);
begin
  FTFileSize := Value;
end;

procedure TGFHTTPGet.SetIsBusy(const Value: boolean);
begin
  FIsBusy := Value;
end;


procedure TGFHTTPGet.ClearSizeRecord;
begin
  try
    if FileExists(FileName + DEF_INI_FILE_EXT) then
      DeleteFile(FileName + DEF_INI_FILE_EXT);
  except
  end;

//
end;

end.

