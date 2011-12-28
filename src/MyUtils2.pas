unit MyUtils2;

{$DEFINE CHINESE}

interface

uses
  WinSock, Windows, Classes, SyncObjs, SysUtils, Messages, DateUtils
  {$IFDEF DEBUG}, ULog{$ENDIF};

{$IFDEF CHINESE}
const
  CStateDownloadingManifest: string = '1/4: 下载索引文件';
  CStateDownloadingSplits  : string = '2/4: 下载视频分片文件';
  CStateTransformingSplits : string = '3/4: 转换视频分片文件';
  CStateMergingSplits      : string = '4/4: 合成视频文件';
  CStateCanceled           : string = '已取消';
  CStateCompleted          : string = '完成';
{$ELSE}
const
  CStateDownloadingManifest: string = '1/4: Downloading manifest';
  CStateDownloadingSplits  : string = '2/4: Downloading split files';
  CStateTransformingSplits : string = '3/4: Transforming split files';
  CStateMergingSplits      : string = '4/4: Merging split files';
  CStateCanceled           : string = 'Canceled';
  CStateCompleted          : string = 'All completed';
{$ENDIF}

const
  CManifestExt  = '.xml';
  CSvltVideoExt = '.ismv';

type
  TOnProduceCompleted = procedure(Sender: TObject) of object;
  TOnProduceFailed    = procedure(Sender: TObject) of object;
  TOnManifestDownloaed = procedure(Sender: TObject;
    const VideoBitrates: TStringList; var VideoSelected: Integer;
    const AudioBitrates: TStringList; var AudioSelected: Integer) of object;
  TOnManifestWrong = procedure(Sender: TObject; Info: string) of object;
  TOnProgressChange = procedure(Sender: TObject;
    Progress, ProgressMax: Integer; State: string = '') of object;
  TOnFileDone = procedure(Sender: TObject; FileName: string; FileSize: Integer) of object;
  TOnAllFilesDone = procedure(Sender: TObject) of object;
  
type
  Bytes = array of Byte;

function StrToBytes(aHexStr: string): Bytes;

function GetServerPort(s: TSocket): Word;
function GetClientPort(s: TSocket): Word;

function SecondsFrom(const TimeStr: string): Int64;
function SecondsFromDef(const TimeStr: string; const Default: Int64 = -1): Int64;
function CreationTime(): Int64;

function SubStrExists(Str: string; SubStr: string): Boolean;

function CopyAll(Src, Dst: TStream): Boolean;

function BSwap16(x: Word): Word;
function BSwap32(x: Integer): Integer;
function BSwap64(x: Int64): Int64;

{ for writing data into binary file stream }
procedure WriteLenBack(aEndPos, aInitPos: Int64; var aOutput: TFileStream);
procedure WriteLen(aInitPos: Int64; var aOutput: TFileStream);
procedure WriteFromFile(aPath: string; var aOutput: TFileStream);
procedure WriteInt8 (aInt8:  Byte; var aOutput: TFileStream);
procedure WriteInt16(aInt16: Word; var aOutput: TFileStream);
procedure WriteInt32(aInt32: Integer; var aOutput: TFileStream);
procedure WriteInt64(aInt64: Int64; var aOutput: TFileStream);
procedure WriteStr(aStr: string; var aOutput: TFileStream);
procedure WriteBytes(aByteArr: array of Byte; var aOutput: TFileStream);

var
  G_MsgCriticalSectionResv : TRTLCriticalSection;
  G_MsgCriticalSectionSent : TRTLCriticalSection;

type
  (*****************************************************************************
  ** 记 录 名：THttpHooked
  ** 作    用：保存hook到的原始数据
  ** 创建日期：2010-10-30
  *****************************************************************************)
  THttpHooked = class
    public
      Socket: TSocket;
      BufStream: TMemoryStream;
      Len   : Integer;
      Flags : Integer;
      constructor Create(aSocket: TSocket; const aBuf; aLen, aFlags: Integer);
      destructor Destroy;override;
  end;

const
  //
  // 消息
  //
  MSG_HTTP_RESV      = WM_USER + 1;
  MSG_NEWVIDEO_FOUND = WM_USER + 2;
  MSG_NEW_MANIFEST   = WM_USER + 3;
  MSG_HAS_MANIFEST   = WM_USER + 4;
  
  //
  // HTTP首部
  //
  HTTP_HEADERS: array[0..47] of string
  = (
  { HTTP通用首部 }
    'Cache-Control',
    'Warning',
    'Connection',
    'Date',
    'Pragma',
    'Trailer',
    'Transfer-Encoding',
    'Upgrade',
    'Via',
  { HTTP请求首部 }
    'Accept',
    'Accept-Charset',
    'Accept-Encoding',
    'Accept-Language',
    'Authorization',
    'Expect',
    'From',
    'Host',
    'If-Match',
    'If-Modified-Since',
    'If-None-Match',
    'If-Range',
    'If-Unmodified-Since',
    'Max-Forwards',
    'Proxy-Authorization',
    'Range',
    'Referer',
    'TE',
    'User-Agent',
  { HTTP相应首部}
    'Accept-Ranges',
    'Age',
    'ETag',
    'Location',
    'Proxy-Authenticate',
    'Retry-After',
    'Server',
    'Vary',
    'WWW-Authenticate',
  { HTTP实体首部 }
    'Allow',
    'Content-Encoding',
    'Content-Language',
    'Content-Length',
    'Content-Location',
    'Content-MD5',
    'Content-Type',
    'Expires',
    'Last-Modified',
  // 其他
    'x-flash-version',
    'Cookie'
    );

  //
  // 视频文件扩展名
  //
  VIDEO_FILE_EXTS = '.flv.f4v.hlv.mp4.avi.mpeg.rm.rmvb';

  //
  // HTTP请求报文方法
  //
  REQ_FUNC_GET = 'get';
  REQ_FUNC_POST = 'post';

  //
  // HTTP相应报文开头
  //
  RES_HTTP_HEAD = 'HTTP';

  //
  // HTTP包中可能存在的首部字段名
  //
  FEILD_HOST = 'Host:';
  FEILD_ACCEPT = 'Accept:';
  FEILD_ACCEPT_LANGUAGE = 'Accept-Language:';
  FEILD_REFERER = 'Referer:';
  FEILD_X_FLASH_VERSION = 'x-flash-version:';
  FEILD_ACCEPT_ENCODING = 'Accept-Encoding:';
  FEILD_USER_AGENT = 'User-Agent:';
  FEILD_CONNECTION = 'Connection:';
  FEILD_CONTENT_TYPE = 'Content-Type:';
  FEILD_CONTENT_LENGTH = 'Content-Length:';
  FEILD_COOKIE = 'Cookie:';

  //
  // HTTP包中Content-Type首部的取值
  //
  CT_APPLICATION = 'application';
  CT_IMAGE = 'image';
  CT_TEXT = 'text';
  CT_VIDEO = 'video';
  CT_AUDIO = 'audio';

  //
  // 视频文件最小体积
  // 当文件类型未知时，将大于此体积的文件识别为视频文件
  //
  VIDEO_FILE_SIZE_LIMIT = 1024 * 1024;

  //
  // HTTP服务器端口
  //
  HTTP_SERVER_PORT_80 = 80;
  HTTP_SERVER_PORT_81 = 81;
  HTTP_SERVER_PORT_8080 = 8080;

  //
  // 回车换行
  //
  CRLF = #13#10;











implementation

function GetServerPort(s: TSocket): Word;
(*******************************************************************************
** 函 数 名：GetServerPort(s: TSocket): Word
** 功能描述：获得目的端口
** 输入参数：s - TSocket 当前网络连接的Socket套接字
** 日    期：2010-09-15
*******************************************************************************)
var
  SockAddr: TSockAddr;
  Size: Integer;
begin
  size:=sizeof(TSockAddr);
  getpeername(s, SockAddr,size);
  Result := ntohs(SockAddr.sin_port);
end;

function GetClientPort(s: TSocket): Word;
(*******************************************************************************
** 函 数 名：GetClientPort(s: TSocket): Word
** 功能描述：获得源端口
** 输入参数：s - TSocket 当前网络连接的Socket套接字
** 日    期：2010-09-15
*******************************************************************************)
var
  SockAddr: TSockAddr;
  Size: Integer;
begin
  size:=sizeof(TSockAddr);
  getsockname(s, SockAddr, Size);
  Result := ntohs(SockAddr.sin_port);
end;

{ THttpHooked }

constructor THttpHooked.Create(aSocket: TSocket; const aBuf; aLen,
  aFlags: Integer);
begin
  Self.Socket := aSocket;
  Self.BufStream := TMemoryStream.Create;
  Self.BufStream.WriteBuffer(aBuf, aLen);
  Self.Len := aLen;
  Self.Flags := aFlags;
end;

destructor THttpHooked.Destroy;
begin
  BufStream.Free;
  inherited;
end;

function SecondsFrom(const TimeStr: string): Int64;
var
  currTime: TDateTime;
  initTime: TDateTime;
  settings: TFormatSettings;
begin
  GetLocaleFormatSettings(GetUserDefaultLCID, settings);
  settings.DateSeparator := '-';
  settings.TimeSeparator := ':';
  settings.ShortDateFormat := 'yyyy-mm-dd';
  settings.ShortTimeFormat := 'hh:nn:ss';
  currTime := Now;
  initTime := StrToDateTime(TimeStr, settings);
  Result := SecondsBetween(currTime, initTime);
end;

function SecondsFromDef(const TimeStr: string; const Default: Int64 = -1): Int64;
begin
  try
    Result := SecondsFrom(TimeStr);
  except
    Result := Default;
  end;
end;

function CreationTime(): Int64;
begin
  Result := SecondsFrom('1904-01-01 00:00:00');
end;

function SubStrExists(Str: string; SubStr: string): Boolean;
begin
  Result := False;
  if Pos(SubStr, Str) <> 0 then
  begin
    Result := True;
  end;
end;

function CopyAll(Src, Dst: TStream): Boolean;
const
  CBufSize = 512;
var
  buf: array[0..CBufSize] of Byte;
  count: Integer;
begin
  Result := False;
  try
    while Src.Size - Src.Position > CBufSize do
    begin
      Src.ReadBuffer(buf, CBufSize);
      Dst.WriteBuffer(buf, CBufSize);
    end;
    count := Src.Size - Src.Position;
    Src.ReadBuffer(buf, count);
    Dst.WriteBuffer(buf, count);
  except
    Exit;
  end;
    
  Result := True;
end;

function BSwap16(x: Word): Word;
begin
  Result := Word(BSwap32(Integer(x)) shr 16);
end;

function BSwap32(x: Integer): Integer;
asm
  BSWAP eax
end;

function BSwap64(x: Int64): Int64;
var
  H, L: Integer;
  xs: string;
begin
  xs := IntToHex(x, 16);
  H := BSwap32(StrToInt('$'+Copy(xs, 1, 8)));
  L := BSwap32(StrToInt('$'+Copy(xs, 9, 8)));
  xs := IntToHex(L, 8) + IntToHex(H, 8);
  result := StrToInt64('$'+xs);
end;

function StrToBytes(aHexStr: string): Bytes;
var
  bcount: Integer;
  i: Integer;
begin
  if Length(aHexStr) mod 2 <> 0 then
    aHexStr := '0' + aHexStr;

  bcount := Length(aHexStr) div 2;
  SetLength(Result, bcount);

  for i:=1 to bcount do
  begin
    Result[i-1] := StrToInt('$'+aHexStr[2*i-1]) * $10 + StrToInt('$'+aHexStr[2*i]);
  end;
end;

{ for writing data into binary file stream }
procedure WriteLenBack(aEndPos, aInitPos: Int64; var aOutput: TFileStream);
var
  tmpInt32: Integer;
begin
  aOutput.Position := aInitPos;
  tmpInt32 := BSwap32(aEndPos - aInitPos);
  aOutput.WriteBuffer(tmpInt32, 4);
end;
procedure WriteLen(aInitPos: Int64; var aOutput: TFileStream);
var
  endPos: Int64;
begin
  endPos := aOutput.Position;
  aOutput.Position := aInitPos;
  WriteInt32(endPos - aInitPos, aOutput);
  aOutput.Position := endPos;
end;
procedure WriteFromFile(aPath: string; var aOutput: TFileStream);
var
  input: TFileStream;
begin
  input := TFileStream.Create(aPath, fmOpenRead);
  try
    CopyAll(input, aOutput);
  finally
    input.Free;
  end;
end;
procedure WriteInt8 (aInt8:  Byte; var aOutput: TFileStream);
begin
  aOutput.WriteBuffer(aInt8, 1);
end;
procedure WriteInt16(aInt16: Word; var aOutput: TFileStream);
begin
  aInt16 := BSwap16(Integer(aInt16)) shr 16;
  aOutput.WriteBuffer(aInt16, 2);
end;
procedure WriteInt32(aInt32: Integer; var aOutput: TFileStream);
begin
  aInt32 := BSwap32(aInt32);
  aOutput.WriteBuffer(aInt32, 4);
end;
procedure WriteInt64(aInt64: Int64; var aOutput: TFileStream);
begin
  aInt64 := BSwap64(aInt64);
  aOutput.WriteBuffer(aInt64, 8);
end;
procedure WriteStr(aStr: string; var aOutput: TFileStream);
begin
  aOutput.WriteBuffer(aStr[1], Length(aStr));
end;
procedure WriteBytes(aByteArr: array of Byte; var aOutput: TFileStream);
begin
  if Length(aByteArr) > 0 then
  begin
    aOutput.WriteBuffer(aByteArr[Low(aByteArr)], Length(aByteArr));
  end;
end;

initialization
  InitializeCriticalSection(G_MsgCriticalSectionResv);
  InitializeCriticalSection(G_MsgCriticalSectionSent);

finalization


end.
