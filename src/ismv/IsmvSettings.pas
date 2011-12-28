unit IsmvSettings;

interface

uses
  Classes;

type
  PIsmvSettings = ^TIsmvSettings;
  TIsmvSettings = record
    { version & duration}
    MajorVersion: Integer;
    MinorVersion: Integer;
    Duration: Int64;
    timescale: Int64;
    { video info }
    VideoChunks: Integer;
    VideoQualityLevels: Integer;
//    VideoDisplayWidth: Integer;
//    VideoDisplayHeight: Integer;
    VideoUrl: string;
    { video info (with index) }
    VideoIndex: Integer;
    VideoBitrate: Integer;
    VideoFourCC: string;
    VideoNALUnitLengthField: Integer;
    VideoMaxWidth: Integer;
    VideoMaxHeight: Integer;
    VideoCodecPrivateData: string;
    { audio info }
    AudioChunks: Integer;
    AudioQualityLevels: Integer;
    AudioUrl: string;
    { audio info (with index) }
    AudioIndex: Integer;
    AudioBitrate: Integer;
    AudioFourCC: string;
    AudioSamplingRate: Integer;
    AudioChannels: Integer;
    AudioBitsPerSample: Integer;
    AudioPacketSize: Integer;
    AudioTag: Integer;
    AudioCodecPrivateData: string;
    { other list }
    VideoUrls: TStringList;
    AudioUrls: TStringList;
    VideoStartTimes: TStringList;
    AudioStartTimes: TStringList;
//    VideoMoofOffsets: TStringList;
//    AudioMoofOffsets: TStringList;
  end;

procedure InitIsmvSettings(p: PIsmvSettings);
procedure LoadIsmvSettings(p: PIsmvSettings; src_file: string;
  video_selected, audio_selected: Integer);
procedure FreeIsmvSettings(p: PIsmvSettings);

implementation

uses
  IsmDoc, SysUtils;

procedure InitIsmvSettings(p: PIsmvSettings);
begin
  with p^ do
  begin
    VideoUrls := TStringList.Create;
    AudioUrls := TStringList.Create;
    VideoStartTimes := TStringList.Create;
    AudioStartTimes := TStringList.Create;
//    VideoMoofOffsets := TStringList.Create;
//    AudioMoofOffsets := TStringList.Create;
  end;
end;

procedure LoadIsmvSettings(p: PIsmvSettings; src_file: string;
  video_selected, audio_selected: Integer);
var
  ismDoc: TISMDocument;

  function GetAudioStartTimes: TStringList;
  var
    I, Count: Integer;
    StartTimes: TSvltDurations;
  begin
    Result := TStringList.Create;
    StartTimes := ismDoc.GetAudioStartTimes;
    try
      Count := Length(StartTimes);
      for I := 0 to count - 1 do
      begin
        result.Add(IntToStr(StartTimes[I]));
      end;
    finally
      SetLength(StartTimes, 0);
    end;
  end;
  function GetVideoStartTimes: TStringList;
  var
    I, Count: Integer;
    StartTimes: TSvltDurations;
  begin
    Result := TStringList.Create;
    StartTimes := ismDoc.GetVideoStartTimes;
    try
      Count := Length(StartTimes);
      for I := 0 to count - 1 do
      begin
        result.Add(IntToStr(StartTimes[I]));
      end;
    finally
      SetLength(StartTimes, 0);
    end;
  end;
var
  VideoQL: TSvltVideoQualityLevel;
  AudioQL: TSvltAudioQualityLevel;
begin
  ismDoc := TISMDocument.Create(src_file);
  try
    with p^ do
    begin
      { version & duration}
      MajorVersion := StrToInt(ismDoc.GetMajorVersion);
      MinorVersion := StrToInt(ismDoc.GetMinorVersion);
      Duration := StrToInt64(ismDoc.GetDuration);
      timescale := 10000000;
      { video info }
      VideoChunks := StrToInt(ismDoc.GetVideoChunks);
      VideoQualityLevels := StrToInt(ismDoc.GetVideoQualityLevels_);
      VideoUrl := ismDoc.GetVideoUrlFormat;
      { video info (with index) }
      VideoQL := ismDoc.GetVideoQualityLevel(video_selected);
      VideoIndex := video_selected;
      VideoBitrate := VideoQL.Bitrate;
      VideoFourCC := VideoQL.FourCC;
      VideoNALUnitLengthField := VideoQL.NALUnitLengthField;
      VideoMaxWidth := VideoQL.MaxWidth;
      VideoMaxHeight := VideoQL.MaxHeight;
      VideoCodecPrivateData := VideoQL.CodecPrivateData;
      { audio info }
      AudioChunks := StrToInt(ismDoc.GetAudioChunks);
      AudioQualityLevels := StrToInt(ismDoc.GetAudioQualityLevels_);
      AudioUrl := ismDoc.GetAudioUrlFormat;
      { audio info (with index) }
      AudioQL := ismDoc.GetAudioQualityLevel(audio_selected);
      AudioIndex := audio_selected;
      AudioBitrate := AudioQL.Bitrate;
      AudioFourCC := AudioQL.FourCC;
      AudioSamplingRate := AudioQL.SamplingRate;
      AudioChannels := AudioQL.Channels;
      AudioBitsPerSample := AudioQL.BitsPerSample;
      AudioPacketSize := AudioQL.PacketSize;
      AudioTag := AudioQL.AudioTag;
      AudioCodecPrivateData := AudioQL.CodecPrivateData;
      { other list }
      VideoStartTimes := GetVideoStartTimes;
      AudioStartTimes := GetAudioStartTimes;
//      VideoMoofOffsets:= TStringList.Create; // modify while merging
//      AudioMoofOffsets:= TStringList.Create; // modify while merging
    end;
  finally
    ismDoc.Free;
  end;
end;

procedure FreeIsmvSettings(p: PIsmvSettings);
begin
  with p^ do
  begin
    VideoUrls.Free;
    AudioUrls.Free;
    VideoStartTimes.Free;
    AudioStartTimes.Free;
//    VideoMoofOffsets.Free;
//    AudioMoofOffsets.Free;
  end;
end;

end.

