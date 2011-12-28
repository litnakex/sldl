unit HttpHeaderExtracter;

interface

uses
  HttpMessage, Classes, SysUtils;

type
  (*****************************************************************************
  ** 类    名：THttpHeaderExtracter
  ** 功能描述：HTTP报文首部提取器
  ** 编写日期：2010-09-26
  *****************************************************************************)
  THttpHeaderExtracter = class
    private
      FNextIndex: Integer;
      FBufStr   : String;
      FLen      : Integer;
    public
      constructor Create(aHttpMsgStr: String);
      procedure Reset;
      function NextHeaderName: String;
      function NextHeaderValue: String;
      function GetHttpHeaders: RHttpMessageHeaders;
      function ToString: String;  
  end;

implementation

{ THttpHeaderExtracter }

constructor THttpHeaderExtracter.Create(aHttpMsgStr: String);
begin
  FBufStr := aHttpMsgStr;
  FLen := Length(FBufStr);
  Self.Reset;
end;

function THttpHeaderExtracter.GetHttpHeaders: RHttpMessageHeaders;
var
  HeaderNameList: TList;
  NextName: String;
  I: Integer;
begin
  HeaderNameList := TList.Create;
  NextName := NextHeaderName;
  while NextName <> '' do
  begin
    HeaderNameList.Add(PChar(NextName));
    NextName := NextHeaderName;
    NextHeaderValue;
  end;
  Result.Count := HeaderNameList.Count;
  SetLength(Result.Names, Result.Count);
  SetLength(Result.Values, Result.Count);
  Self.Reset;
  for I := 0 to Result.Count - 1 do
  begin
    Result.Names[I] := Self.NextHeaderName;
    Result.Values[I]:= Self.NextHeaderValue;
  end;
end;

function THttpHeaderExtracter.NextHeaderName: String;
var
  TempName: String;
begin
  TempName := '';
//  if FNextIndex = 1 then NextLine;
  while (FNextIndex <= FLen)
    and (FBufStr[FNextIndex] <> #13) and (FBufStr[FNextIndex] <> #10) do
  begin
    if FBufStr[FNextIndex] = ':' then
    begin
      Inc(FNextIndex);
      while FBufStr[FNextIndex] = ' ' do
      begin
        Inc(FNextIndex);
      end;
      Result := TempName;
      Exit;
    end;
    TempName := TempName + FBufStr[FNextIndex];
    Inc(FNextIndex);
  end;
  if FNextIndex >= FLen then
  begin
    Result := '';
    Exit;
  end;
  while (FNextIndex <= FLen) and ((FBufStr[FNextIndex] = #13) or (FBufStr[FNextIndex] = #10)) do
  begin
    Inc(FNextIndex);
  end;
  Result := NextHeaderName;
end;

function THttpHeaderExtracter.NextHeaderValue: String;
var
  TempValue: String;
begin
  TempValue := '';
  while (FNextIndex <= FLen)
    and (FBufStr[FNextIndex] <> #13) and (FBufStr[FNextIndex] <> #10) do
  begin
    TempValue := TempValue + FBufStr[FNextIndex];
    Inc(FNextIndex);
  end;
  if FNextIndex >= FLen then
  begin
    Result := '';
    Exit;
  end;
  Result := TempValue;
end;

procedure THttpHeaderExtracter.Reset;
begin
  FNextIndex := 1;
end;

function THttpHeaderExtracter.ToString: String;
var
  NextIndex: Integer;
begin
  { 去掉第一行 }
  NextIndex := 1;
  while (NextIndex <= FLen) and (FBufStr[NextIndex] <> #13)
    and (FBufStr[NextIndex] <> #10) do
  begin
    Inc(NextIndex);
  end;
  if NextIndex > FLen then
  begin
    Result := '';
    Exit;
  end;
  NextIndex := NextIndex + 2;
  { 从第二行开始的部分作为首部内容返回 }
  while (NextIndex <= FLen) do
  begin
    if (FBufStr[NextIndex] = #13) then
    begin
      Result := Result + #13#10;
      Inc(NextIndex, 2);
      if (NextIndex > FLen) or (FBufStr[NextIndex] = #13)then
      begin
        Exit;
      end;
    end;
    Result := Result + FBufStr[NextIndex];
    Inc(NextIndex);
  end;
end;

end.
