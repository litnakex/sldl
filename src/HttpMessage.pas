unit HttpMessage;

interface

uses
  Classes, SysUtils, MyUtils2;

type
  (*****************************************************************************
  ** 记 录 名：RHttpRequestHeaders
  ** 功能描述：HTTP报文首部，包含了一个报文中所有的首部名称和值
  ** 编写日期：2010-09-26
  *****************************************************************************)
  RHttpMessageHeaders = record
    Names: array of string;
    Values: array of string;
    Count: Integer;
  end;

  (*****************************************************************************
  ** 类    名：THttpMessage
  ** 功能描述：HTTP报文类，封装了HTTP报文的全部内容
  ** 编写日期：2010-09-14
  *****************************************************************************)
  THttpMessage = class
  private
    FLen: Integer;
    FBufStr: string;
    FSrcPort: Word;
    FDstPort: Word;

    function GetURI: string;
    function GetPostString: string;
    function GetHost: String;
    function GetURL: string;
    function GetSrcPort: Word;
    function GetDstPort: Word;
    function GetAccept: string;
    function GetAcceptLanguage: string;
    function GetReferer: string;
    function GetXFlashVersion: string;
    function GetAcceptEncoding: string;
    function GetUserAgent: string;
    function GetConnection: string;
    function GetContentType: string;
    function GetContentLength: string;
    function GetCookie: string;
    function GetEntityBody: string;
    function GetValueOf(aField: string): string;

  public
    constructor Create(const Buf; Len: Cardinal; aSrcPort: Word = 0; aDstPort: Word = 0); overload;
    constructor Create(aBufStr: string; aSrcPort: Word = 0; aDstPort: Word = 0); overload;
    destructor Destroy; override;
    function ToString: string;
    procedure Append(aMsg: THttpMessage);

    function IsVideoRequest: Boolean;

    function IsVideoResponse: Boolean; //是视频相应包

    function IsHttp: Boolean;
    function HasRequestMethod: Boolean;

    class function NullInstance: THttpMessage;
  published
    property Size: Integer read FLen;
    property URI: string read GetURI;
    property PostString: string read GetPostString;
    property Host: string read GetHost;
    property URL: string read GetURL;
    property SrcPort: Word read GetSrcPort;
    property DstPort: Word read GetDstPort;
    property Accept: string read GetAccept;
    property AcceptLanguage: string read GetAcceptLanguage;
    property Referer: string read GetReferer;
    property XFlashVersion: string read GetXFlashVersion;
    property AcceptEncoding: string read GetAcceptEncoding;
    property UserAgent: string read GetUserAgent;
    property Connection: string read GetConnection;
    property ContentType: string read GetContentType;
    property ContentLength: string read GetContentLength;
    property Cookie: string read GetCookie;
    property Entity: string read GetEntityBody;
  end;

  THttpMessageList = class
    private
      FList: TList;
    public
      constructor Create;
      destructor Destroy;override;
      

  end;

implementation

{ THttpDataPack }

function GetStrFromBuf(Buf: PChar; Len: Integer): string;
//从数据包得到字符串
var
  I, j: Integer;
begin
  result := '';
  for I := 0 to Len - 1 do
  begin
    j := ord(buf[i]);

    if (j > 9) and (j < 128) then
      Result := Result + (Buf[I]);
  end;
end;

constructor THttpMessage.Create(const Buf; Len: Cardinal; aSrcPort,
  aDstPort: Word);
begin
  FBufStr := GetStrFromBuf(@Buf, Len);
  FLen := Len;
  FSrcPort := aSrcPort;
  FDstPort := aDstPort;
end;

procedure THttpMessage.Append(aMsg: THttpMessage);
begin
  Self.FBufStr := Self.FBufStr + aMsg.ToString;
  Self.FLen := Self.FLen + aMsg.Size;
end;

constructor THttpMessage.Create(aBufStr: string; aSrcPort, aDstPort: Word);
begin
  FBufStr := aBufStr;
  FLen := Length(aBufStr);
  FSrcPort := aSrcPort;
  FDstPort := aDstPort;
end;

destructor THttpMessage.Destroy;
begin
  inherited;
end;

function THttpMessage.GetAccept: string;
begin
  Result := GetValueOf(FEILD_ACCEPT)
end;

function THttpMessage.GetAcceptEncoding: string;
begin
  Result := GetValueOf(FEILD_ACCEPT_ENCODING)
end;

function THttpMessage.GetAcceptLanguage: string;
begin
  Result := GetValueOf(FEILD_ACCEPT_LANGUAGE)
end;

function THttpMessage.GetConnection: string;
begin
  Result := GetValueOf(FEILD_CONNECTION)
end;

function THttpMessage.GetContentLength: string;
var
  I, Len: Integer;
begin
  Result := GetValueOf(FEILD_CONTENT_LENGTH);
  Len := Length(Result);
  for I := 1 to Len do
  begin
    if (Result[I] < '0') or (Result[I] > '9') then
    begin
      Result := '';
      Break;
    end;
  end;
  if Result = '' then Result := '0';
end;

function THttpMessage.GetContentType: string;
begin
  Result := GetValueOf(FEILD_CONTENT_TYPE);
end;

function THttpMessage.GetCookie: string;
begin
  Result := GetValueOf(FEILD_COOKIE);
end;

function THttpMessage.GetDstPort: Word;
begin
  Result := FDstPort;
end;

function THttpMessage.GetEntityBody: string;
var
  StartIdx: Integer;
begin
  StartIdx := Pos(CRLF + CRLF, FBufStr);
  if StartIdx = 0 then
  begin
    Result := '';
    Exit;
  end;
  Inc(StartIdx, 4);
  Result := Copy(FBufStr, StartIdx, FLen - StartIdx + 1);
end;

function THttpMessage.GetPostString: string;
begin
  Result := GetValueOf(REQ_FUNC_POST);
end;

function THttpMessage.GetReferer: string;
begin
  Result := GetValueOf(FEILD_REFERER)
end;

function THttpMessage.GetSrcPort: Word;
begin
  Result := FSrcPort;
end;

function THttpMessage.GetURI: string;
const
  CHttpUrlBegin = 'http://';
var
  slashPos: Integer;
begin
  Result := GetValueOf(REQ_FUNC_GET);

  // if begin with 'http://', remove 'http://hostname'
  if Pos(CHttpUrlBegin, Result) = 1 then
  begin
    Result := Copy(Result, Length(CHttpUrlBegin)+1, Length(Result)-Length(CHttpUrlBegin));
    slashPos := Pos('/', Result);
    Result := Copy(Result, slashPos, Length(Result)-slashPos+1);
  end;
end;

function THttpMessage.GetHost: String;
begin
  Result := GetValueOf(FEILD_HOST);
end;

function THttpMessage.GetURL: string;
const
  CHttpUrlBegin = 'http://';
begin
  Result := CHttpUrlBegin + GetHost;
  if GetURI <> '' then
    Result := Result + GetURI
  else if GetPostString <> '' then
    Result := Result + GetPostString;
end;

function THttpMessage.GetUserAgent: string;
begin
  Result := GetValueOf(FEILD_USER_AGENT)
end;

function THttpMessage.GetValueOf(aField: string): string;
(*******************************************************************************
** 函 数 名：GetValueOf(aField: String): String
** 功能描述：获得HTTP报文中以'aField'开头的首部行的值
** 输入参数：aField - String 首部字段名
** 返 回 值：首部值
** 日    期：2010-09-13
** 修改日期：2010-09-20
*******************************************************************************)
var
  PackString: string;
  StartIndex, EndIndex: Integer;
begin
  PackString := FBufStr;
  StartIndex := Pos(LowerCase(aField), LowerCase(PackString));
  if StartIndex = 0 then
  begin
    Result := '';
    Exit;
  end;
  StartIndex := StartIndex + Length(aField);
  while PackString[StartIndex] = ' ' do
  begin
    Inc(StartIndex);
  end;
  EndIndex := StartIndex;
  while (PackString[EndIndex] <> ' ')
    and (PackString[EndIndex] <> #13)
    and (PackString[EndIndex] <> #10)
    and (EndIndex < FLen) do
  begin
    Inc(EndIndex);
  end;
  Result := copy(PackString, StartIndex, EndIndex - StartIndex);
end;

function THttpMessage.GetXFlashVersion: string;
begin
  Result := GetValueOf(FEILD_X_FLASH_VERSION)
end;

function THttpMessage.HasRequestMethod: Boolean;
begin
  Result := False;
  if (Self.URI <> '') or (Self.PostString <> '') then Result := True;
end;

function THttpMessage.IsHttp: Boolean;
begin
  Result := False;
  if (FSrcPort = HTTP_SERVER_PORT_80) or (FDstPort = HTTP_SERVER_PORT_80)
    or (FSrcPort = HTTP_SERVER_PORT_81) or (FDstPort = HTTP_SERVER_PORT_81)
    or (FSrcPort = HTTP_SERVER_PORT_8080) or (FDstPort = HTTP_SERVER_PORT_8080)
    then
  begin
    Result := True;
  end;
end;

function THttpMessage.IsVideoRequest: Boolean;
(*******************************************************************************
** 函 数 名：IsVideoRequest
** 功能描述：判断本HTTP数据包是否是视频请求包，根据三个判断条件：
             1、URI扩展名为已知视频文件扩展名
** 日    期：2010-09-14
*******************************************************************************)
begin
  Result := False;
  if (SubStrExists(VIDEO_FILE_EXTS, LowerCase(ExtractFileExt(Self.URI)))) then
  begin
    Result := True;
  end;
end;

function THttpMessage.IsVideoResponse: Boolean;
(*******************************************************************************
** 函 数 名：IsVideoResponse
** 功能描述：判断本HTTP数据包是不是服务器响应的视频数据包，根据三个判断条件：
             1、HTTP包开头不是'HTTP'，则一定不是视频数据包
             2、HTTP包中Content-Type字段值是视频类型（video）
             3、HTTP包中Content-Type字段值不是图片、文字类型（image、text）
             4、HTTP包中Content-Length大于一定值（不满足此条件则一律按False处理）
** 创建日期：2010-09-17
** 修改日期：2010-11-02
*******************************************************************************)
begin
  Result := True;
  if Pos(RES_HTTP_HEAD, Self.FBufStr) <> 1 then
  begin
    Result := False;
    Exit;
  end;
  if SubStrExists(Self.ContentType, CT_VIDEO) then
  begin
    Result := True;
    Exit;
  end
  else
  if SubStrExists(Self.ContentType, CT_AUDIO) then
  begin
    Result := True;
    Exit;
  end
  else
  if SubStrExists(Self.ContentType, CT_IMAGE)
    or SubStrExists(Self.ContentType, CT_TEXT) then
  begin
    Result := False;
    Exit;
  end;
  if StrToInt(Self.ContentLength) > VIDEO_FILE_SIZE_LIMIT then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
end;

class function THttpMessage.NullInstance: THttpMessage;
begin
  Result := THttpMessage.Create('', 0, 0, 0);
end;

function THttpMessage.ToString: string;
begin
  Result := FBufStr;
end;

{ THttpMessageList }

constructor THttpMessageList.Create;
begin
  FList := TList.Create;
end;

destructor THttpMessageList.Destroy;
begin
  FList.Free;
  inherited;
end;

end.

