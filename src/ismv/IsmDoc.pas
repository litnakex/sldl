unit IsmDoc;

interface

uses
  Forms, Classes, SysUtils, XMLDoc, xmldom, XMLIntf, PerlRegEx
{$IFDEF DEBUG}, ULog{$ENDIF};

type
  { forward declarations }
  TISMDocument = class;

  PSvltDuration = ^TSvltDuration;
  TSvltDuration = Int64;
  TSvltDurations = array of TSvltDuration;

  TSvltWidths = array of Integer;
  TSvltHeights = array of Integer;

  PSvltBitrate = ^TSvltBitrate;
  TSvltBitrate = Int64;

  { video quality levels}
  PSvltVideoQualityLevel = ^TSvltVideoQualityLevel;
  TSvltVideoQualityLevel = record
    Index: Integer;
    Bitrate: TSvltBitrate;
    FourCC: string;
    NALUnitLengthField: Integer;
    MaxWidth: Integer;
    MaxHeight: Integer;
    CodecPrivateData: string;
  end;
  TSvltVideoQualityLevels = array of TSvltVideoQualityLevel;

  { audio quality levels}
  PSvltAudioQualityLevel = ^TSvltAudioQualityLevel;
  TSvltAudioQualityLevel = record
    Bitrate: TSvltBitrate;
    FourCC: string;
    SamplingRate: Integer;
    Channels: Integer;
    BitsPerSample: Integer;
    PacketSize: Integer;
    AudioTag: Integer;
    CodecPrivateData: string;
  end;
  TSvltAudioQualityLevels = array of TSvltAudioQualityLevel;

  { class definition }
  TISMDocument = class
  private
    FXmlStream: TStream;
    FXmlDoc: TXMLDocument;
  public
    constructor Create(aDocStream: TStream); overload;
    constructor Create(aDocFile: string); overload;
    destructor Destroy; override;
  public
    class function Check(aDocStr: string): Boolean;
    function GetMajorVersion: string;
    function GetMinorVersion: string;
    function GetDuration: string;
    function GetVideoChunks: string;
    function GetAudioChunks: string;
    function GetVideoDisplayWidth: string;
    function GetVideoDisplayHeight: string;
    function GetVideoQualityLevels_: string;
    function GetAudioQualityLevels_: string;
    function GetVideoUrlFormat: string;
    function GetAudioUrlFormat: string;
    function GetVideoQualityLevel(aIndex: Integer): TSvltVideoQualityLevel;
    function GetAudioQualityLevel(aIndex: Integer): TSvltAudioQualityLevel;
    function GetVideoStartTimes: TSvltDurations;
    function GetAudioStartTimes: TSvltDurations;
  private
    function GetL2RootNode(aType: string): IXMLNode; //aType is 'video' or 'audio'
    function GetUrlFormat(aType: string): string;
    function GetStartTimes(aType: string): TSvltDurations;
  end;

implementation

{ TISMDocument }

const
  mkType: string = 'Type';
  mkMajorVersion: string = 'MajorVersion';
  mkMinorVersion: string = 'MinorVersion';
  mkDuration: string = 'Duration';
  mkChunks: string = 'Chunks';
  mkQualityLevels: string = 'QualityLevels';
  mkDisplayWidth: string = 'DisplayWidth';
  mkDisplayHeight: string = 'DisplayHeight';

  mkQualityLevel: string = 'QualityLevel';
  mkIndex: string = 'Index';
  mkBitrate: string = 'Bitrate';
  mkFourCC: string = 'FourCC';
  mkNALUnitLengthField: string = 'NALUnitLengthField';
  mkCodecPrivateData: string = 'CodecPrivateData';
  mkSamplingRate: string = 'SamplingRate';
  mkChannels: string = 'Channels';
  mkBitsPerSample: string = 'BitsPerSample';
  mkPacketSize: string = 'PacketSize';
  mkAudioTag: string = 'AudioTag';

  mkWidth: string = 'Width';
  mkHeight: string = 'Height';
  mkMaxWidth: string = 'MaxWidth';
  mkMaxHeight: string = 'MaxHeight';
  mkC: string = 'c';
  mkCIndex: string = 'n';
  mkCDuration: string = 'd';
  mkCDuration2: string = 't';

  mkWaveFormatEx: string = 'WaveFormatEx';
  mkUrl: string = 'Url';
  mkVideo: string = 'video';
  mkAudio: string = 'audio';

constructor TISMDocument.Create(aDocStream: TStream);
var
  tmpXml: TStringList;
  tmpstr: string;
  reg: TPerlRegEx;
  I, count: Integer;
begin
  FXmlDoc := TXMLDocument.Create(Application);

  tmpXml := TStringList.Create;
  try
    tmpXml.LoadFromStream(aDocStream);

    reg := TPerlRegEx.Create;
    try
      reg.RegEx := '<\?xml.*\?>';
      reg.Replacement := '';
      reg.Options := [preCaseLess];
      reg.Compile;
      reg.Study;
      count := tmpXml.Count;
      for I := 0 to count - 1 do
      begin
        reg.Subject := tmpXml[I];
        if reg.Match then
        begin
          tmpXml.Delete(I);
          Break;
        end;
      end;
    finally
      reg.Free;
    end;

    FXmlStream := TMemoryStream.Create;
    tmpstr := tmpXml.Text;
    FXmlStream.Write(PChar(tmpstr)^, Length(tmpstr));
    FXmlStream.Position := 0;
    FXmlDoc.LoadFromStream(FXmlStream);
  finally
    tmpXml.Free;
  end;
end;

constructor TISMDocument.Create(aDocFile: string);
begin
  FXmlDoc := TXMLDocument.Create(Application);
  FXmlDoc.LoadFromFile(aDocFile);
end;

destructor TISMDocument.Destroy;
begin
  FXmlStream.Free;
  FXmlDoc.Free;
  inherited;
end;

function TISMDocument.GetL2RootNode(aType: string): IXMLNode;
var
  L2Nodes: IXMLNodeList;
  count: Integer;
  I: Integer;
begin
  try
    L2Nodes := FXmlDoc.DocumentElement.ChildNodes;
    count := L2Nodes.Count;
    for I := 0 to count - 1 do
    begin
      with L2Nodes[I] do
      begin
        if AttributeNodes[mkType].NodeValue = aType then
        begin
          Result := L2Nodes[I];
          exit;
        end;
      end;
    end;
  except
    Result := nil;
  end;
end;

function TISMDocument.GetAudioUrlFormat: string;
begin
  Result := GetUrlFormat(mkAudio);
end;

function TISMDocument.GetMajorVersion: string;
begin
  try
    Result := FXmlDoc.DocumentElement.AttributeNodes[mkMajorVersion].NodeValue;
  except
    Result := '2';
  end;
end;

function TISMDocument.GetMinorVersion: string;
begin
  try
    Result := FXmlDoc.DocumentElement.AttributeNodes[mkMinorVersion].NodeValue;
  except
    Result := '0';
  end;
end;

function TISMDocument.GetDuration: string;
begin
  try
    Result := FXmlDoc.DocumentElement.AttributeNodes[mkDuration].NodeValue;
  except
    Result := '0';
  end;
end;

function TISMDocument.GetUrlFormat(aType: string): string;
begin
  try
    Result := GetL2RootNode(aType).AttributeNodes[mkUrl].NodeValue;
  except
    Result := '';
  end;
end;

function TISMDocument.GetVideoUrlFormat: string;
begin
  Result := GetUrlFormat(mkVideo);
end;

function TISMDocument.GetVideoQualityLevel(aIndex: Integer): TSvltVideoQualityLevel;
var
  L2RootNode: IXMLNode;
begin
  L2RootNode := GetL2RootNode(mkVideo);

  with L2RootNode.ChildNodes[aIndex] do
    if NodeName = mkQualityLevel then
    begin
      try
        Result.Index := StrToInt(AttributeNodes[mkIndex].NodeValue);
      except
        Result.Index := 0;
      end;
      try
        Result.Bitrate := StrToInt64(AttributeNodes[mkBitrate].NodeValue);
      except
        Result.Bitrate := 0;
      end;
      try
        Result.FourCC := AttributeNodes[mkFourCC].NodeValue;
      except
        Result.FourCC := '    ';
      end;
      try
        Result.NALUnitLengthField := AttributeNodes[mkNALUnitLengthField].NodeValue;
      except
        Result.NALUnitLengthField := 4;
      end;
      try
        Result.MaxWidth := StrToInt(AttributeNodes[mkMaxWidth].NodeValue);
      except
        Result.MaxWidth := 0;
      end;
      try
        Result.MaxHeight := StrToInt(AttributeNodes[mkMaxHeight].NodeValue);
      except
        Result.MaxHeight := 0;
      end;
      try
        Result.CodecPrivateData := AttributeNodes[mkCodecPrivateData].NodeValue;
      except
        Result.CodecPrivateData := '';
      end;
    end;
end;

function TISMDocument.GetAudioQualityLevel(aIndex: Integer): TSvltAudioQualityLevel;
var
  L2RootNode: IXMLNode;
begin
  L2RootNode := GetL2RootNode(mkAudio);

  with L2RootNode.ChildNodes[aIndex] do
    if NodeName = mkQualityLevel then
    begin
      try
        Result.Bitrate := StrToInt64(AttributeNodes[mkBitrate].NodeValue);
      except
        Result.Bitrate := 0;
      end;
      try
        Result.FourCC := AttributeNodes[mkFourCC].NodeValue
      except
        try
          Result.FourCC := L2RootNode.AttributeNodes[mkFourCC].NodeValue;
        except
          Result.FourCC := '    ';
        end;
      end;
      try
        Result.SamplingRate := StrToInt(AttributeNodes[mkSamplingRate].NodeValue);
      except
        Result.SamplingRate := 0;
      end;
      try
        Result.Channels := StrToInt(AttributeNodes[mkChannels].NodeValue);
      except
        Result.Channels := 2;
      end;
      try
        Result.BitsPerSample := StrToInt(AttributeNodes[mkBitsPerSample].NodeValue);
      except
        Result.BitsPerSample := 0;
      end;
      try
        Result.PacketSize := StrToInt(AttributeNodes[mkPacketSize].NodeValue);
      except
        Result.PacketSize := 0;
      end;
      try
        Result.AudioTag := StrToInt(AttributeNodes[mkAudioTag].NodeValue);
      except
        Result.AudioTag := 0;
      end;
      try
        Result.CodecPrivateData := AttributeNodes[mkCodecPrivateData].NodeValue;
      except
        Result.CodecPrivateData := '';
      end;
    end;
end;

function TISMDocument.GetAudioStartTimes: TSvltDurations;
begin
  Result := GetStartTimes(mkAudio);
end;

function TISMDocument.GetVideoStartTimes: TSvltDurations;
begin
  Result := GetStartTimes(mkVideo);
end;

function TISMDocument.GetStartTimes(aType: string): TSvltDurations;
var
  L2RootNode: IXMLNode;
  I, count: Integer;
  resList: TList;
  pDura: PSvltDuration;
  bTime: boolean;
begin
  bTime := false;
  try
    L2RootNode := GetL2RootNode(aType);
    resList := TList.Create;
    try
      count := L2RootNode.ChildNodes.Count;
      for I := 0 to count - 1 do
      begin
        with L2RootNode.ChildNodes[I] do
          if NodeName = mkC then
          begin
            New(pDura);
            if HasAttribute(mkCDuration) then
              pDura^ := StrToInt64(AttributeNodes[mkCDuration].NodeValue)
            else if HasAttribute(mkCDuration2) then
            begin
              pDura^ := StrToInt64(AttributeNodes[mkCDuration2].NodeValue);
              bTime := true;
            end;
            resList.Add(pDura);
          end;
      end;

      count := resList.Count;
      SetLength(Result, count);
      if bTime then
      begin
        for I := 0 to count - 1 do
        begin
          Result[I] := PSvltDuration(resList[I])^;
          Dispose(PSvltDuration(resList[I]));
        end;
      end
      else
      begin
        Result[0] := 0;
        for I := 1 to count - 1 do
        begin
          Result[I] := Result[I - 1] + PSvltDuration(resList[I - 1])^;
          Dispose(PSvltDuration(resList[I - 1]));
        end;
        Dispose(PSvltDuration(resList[count - 1]));
      end;
    finally
      resList.Free;
    end;
  except
    SetLength(Result, 0);
  end;
end;

class function TISMDocument.Check(aDocStr: string): Boolean;
const
  CL1RootName = '<SmoothStreamingMedia';
  CL2RootName = '<StreamIndex';
  CL3RootName = '<QualityLevel';
begin
  Result := False;
  if Pos(CL1RootName, aDocStr) = 0 then Exit;
  if Pos(CL2RootName, aDocStr) = 0 then Exit;
  if Pos(CL3RootName, aDocStr) = 0 then Exit;
  Result := True;
end;

function TISMDocument.GetVideoChunks: string;
begin
  try
    Result := GetL2RootNode('video').AttributeNodes[mkChunks].NodeValue;
  except
    Result := '0';
  end;
end;

function TISMDocument.GetAudioChunks: string;
begin
  try
    Result := GetL2RootNode('audio').AttributeNodes[mkChunks].NodeValue;
  except
    Result := '0';
  end;
end;

function TISMDocument.GetAudioQualityLevels_: string;
var
  i, num: integer;
  L2RootNode: IXMLNode;
begin
// fixed by lijm
  L2RootNode := GetL2RootNode(mkAudio);
  num := 0;
  try
    for i := 0 to L2RootNode.ChildNodes.Count - 1 do
    begin
      with L2RootNode.ChildNodes[i] do
        if NodeName = mkQualityLevel then
        begin
          num := num + 1;
        end;
    end;
  except
  end;
  result := inttostr(num);
end;

function TISMDocument.GetVideoQualityLevels_: string;
var
  i, num: integer;
  L2RootNode: IXMLNode;
begin
 // fixed by lijm
  L2RootNode := GetL2RootNode(mkVideo);
  num := 0;
  try
    for i := 0 to L2RootNode.ChildNodes.Count - 1 do
    begin
      with L2RootNode.ChildNodes[i] do
        if NodeName = mkQualityLevel then
        begin
          num := num + 1;
        end;
    end;
  except
  end;
  result := inttostr(num);
end;

function TISMDocument.GetVideoDisplayHeight: string;
begin
  try
    Result := GetL2RootNode('video').AttributeNodes[mkDisplayHeight].NodeValue;
  except
    Result := '0';
  end;
end;

function TISMDocument.GetVideoDisplayWidth: string;
begin
  try
    Result := GetL2RootNode('video').AttributeNodes[mkDisplayWidth].NodeValue;
  except
    Result := '0';
  end;
end;

end.

