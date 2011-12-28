unit mp4_io;

interface

uses
  Classes, SysUtils, Windows, DateUtils, IsmDoc, IsmvSettings;

type
  uint8_t  = Byte;
  uint16_t = Word;
  uint32_t = LongWord;
  uint64_t = Int64;

type
  Bytes = array of Byte;
  THandlerType = (htVide, htSoun, htHint);
//  TSegmentType = (stVideo, stAudio); // 分片类型

const
  H264_PROFILE_BASELINE = 66;
  H264_PROFILE_MAIN = 77;
  H264_PROFILE_EXTENDED = 88;
  H264_PROFILE_HIGH = 100;
  H264_PROFILE_HIGH10 = 110;
  H264_PROFILE_HIGH422 = 122;
  H264_PROFILE_HIGH444 = 144;
  H264_PROFILE_HIGH444_PREDICTIVE = 244;

{ time }
function SecondsFrom(const TimeStr: string): Int64;
function SecondsFromDef(const TimeStr: string; const Default: Int64 = -1): Int64;
function CreationTime(): Int64;

{ 4-char to int32 }
function FOURCC(fourCh: string): uint32_t;
function HexStrToBytes(aHexStr: string): Bytes;

{ byte swap }
function BSwap16(x: uint16_t): uint16_t;
function BSwap32(x: uint32_t): uint32_t;
function BSwap64(x: uint64_t): uint64_t;

{ write bits }
procedure write_8 (aOutput: TStream; aInt8:  uint8_t);
procedure write_16(aOutput: TStream; aInt16: uint16_t);
procedure write_24(aOutput: TStream; aInt32: uint32_t);
procedure write_32(aOutput: TStream; aInt32: uint32_t);
procedure write_64(aOutput: TStream; aInt64: uint64_t);
procedure write_str(aOutput: TStream; aStr: string);
procedure write_bs(aOutput: TStream; aByteArr: array of uint8_t);
function write_cp(Dst: TStream; Src: TStream): Int64; overload;
function write_cp(Dst: TStream; Src: TStream; Len: Int64): Int64; overload;
function write_len(aOutput: TStream; aInitPos: uint64_t): uint32_t;


{ read bits }
function read_8(input: TStream): uint8_t;
function read_16(input: TStream): uint16_t;
function read_24(input: TStream): uint32_t;
function read_32(input: TStream): uint32_t;
function read_64(input: TStream): uint64_t;

{ mp4 writer }
procedure write_ismv_head(output: TStream; p: PIsmvSettings);
procedure write_ismv_part(output, input: TStream; out offset: Int64); // 写入ismv分片（需要时进行转换）
procedure write_ismv_tail(output: TStream; p: PIsmvSettings;
  VideoMoofOffsets, AudioMoofOffsets: TStringList);
procedure write_ismv_body(output: TStream; InputFileList: TStringList;
  out VideoMoofOffsets, AudioMoofOffsets: TStringList);

function check_ismv_part(input: TStream): Boolean;

function len_mfra(tfraVersion, VideoPartCount, AudioPartCount: Integer): Int64;

implementation

function FOURCC(fourCh: string): uint32_t;
begin
  Assert(Length(fourCh) = 4);
  Result := (uint32_t(fourCh[1]) shl 24)
          + (uint32_t(fourCh[2]) shl 16)
          + (uint32_t(fourCh[3]) shl 8)
          + (uint32_t(fourCh[4]));
end;

function HexStrToBytes(aHexStr: string): Bytes;
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

function BSwap16(x: uint16_t): uint16_t;
begin
  Result := Word(BSwap32(Integer(x)) shr 16);
end;

function BSwap32(x: uint32_t): uint32_t;
asm
  BSWAP eax
end;

function BSwap64(x: uint64_t): uint64_t;
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

procedure write_8 (aOutput: TStream; aInt8:  uint8_t);
begin
  aOutput.WriteBuffer(aInt8, 1);
end;
procedure write_16(aOutput: TStream; aInt16: uint16_t);
var
  b: uint8_t;
begin
  b := uint8_t(aInt16 shr 8);   aOutput.WriteBuffer(b, 1);
  b := uint8_t(aInt16 shr 0);   aOutput.WriteBuffer(b, 1);
end;
procedure write_24(aOutput: TStream; aInt32: uint32_t);
var
  b: uint8_t;
begin
  b := uint8_t(aInt32 shr 16);  aOutput.WriteBuffer(b, 1);
  b := uint8_t(aInt32 shr 8);   aOutput.WriteBuffer(b, 1);
  b := uint8_t(aInt32 shr 0);   aOutput.WriteBuffer(b, 1);
end;
procedure write_32(aOutput: TStream; aInt32: uint32_t);
var
  b: uint8_t;
begin
  b := uint8_t(aInt32 shr 24);  aOutput.WriteBuffer(b, 1);
  b := uint8_t(aInt32 shr 16);  aOutput.WriteBuffer(b, 1);
  b := uint8_t(aInt32 shr 8);   aOutput.WriteBuffer(b, 1);
  b := uint8_t(aInt32 shr 0);   aOutput.WriteBuffer(b, 1);
end;
procedure write_64(aOutput: TStream; aInt64: uint64_t);
begin
  write_32(aOutput, uint32_t(aInt64 shr 32));
  write_32(aOutput, uint32_t(aInt64 shr 0));
end;
procedure write_str(aOutput: TStream; aStr: string);
begin
  aOutput.WriteBuffer(aStr[1], Length(aStr));
end;
procedure write_bs(aOutput: TStream; aByteArr: array of uint8_t);
begin
  if Length(aByteArr) > 0 then
  begin
    aOutput.WriteBuffer(aByteArr[Low(aByteArr)], Length(aByteArr));
  end;
end;

function write_cp(Dst: TStream; Src: TStream): Int64;
begin
  Result := write_cp(Dst, Src, Src.Size - Src.Position);
end;
function write_cp(Dst: TStream; Src: TStream; Len: Int64): Int64;
var
  count: Int64;
begin
  count := len;
  if count > Src.Size - Src.Position then
    count := Src.Size - Src.Position;
  Result := Dst.CopyFrom(Src, count);
end;

function write_len(aOutput: TStream; aInitPos: uint64_t): uint32_t;
var
  endPos: uint64_t;
begin
  endPos := aOutput.Position;
  aOutput.Position := aInitPos;
  Result := uint32_t(endPos - aInitPos);
  write_32(aOutput, Result);
  aOutput.Position := endPos;
end;


procedure write_ismv_part(output, input: TStream; out offset: Int64);
const
  cBufSize = 500;
var
  ifstream : TStream;
  ofstream : TStream;
  countNum: Integer;
  tmpInt: Integer;
begin
  Assert(output <> nil);
  Assert(input <> nil);

  offset := output.Position;

  ifstream := input;
  ofstream := output;

  // save head size & moof size add 4
  tmpInt := read_32(ifstream) + 4;
  countNum := tmpInt + 8;
  write_32(ofstream, tmpInt);

  write_cp(ofstream, ifstream, 20);

  // traf size add 4
  write_32(ofstream, read_32(ifstream) + 4);

  // 'traf'
  write_cp(ofstream, ifstream, 4);

  // get tfhd size
  tmpInt := read_32(ifstream);
  write_32(ofstream, tmpInt);

  // copy tfhd data
  write_cp(ofstream, ifstream, tmpInt-4);

  // trun size add 4
  write_32(ofstream, read_32(ifstream) + 4);

  // 'trun'
  write_cp(ofstream, ifstream, 4);

  // add 1
  tmpInt := read_32(ifstream);
  if (tmpInt and 1) = 1 then // tmpInt最低位是1，直接复制文件
  begin
    ifstream.Position := 0;
    ofstream.Position := offset;
    write_cp(ofstream, ifstream);
    Exit;
  end;
  write_32(ofstream, tmpInt + 1);
    
  write_cp(ofstream, ifstream, 4);
  write_32(ofstream, countNum);
  write_cp(ofstream, ifstream);
end;

{ read bits }
function read_8(input: TStream): uint8_t;
begin
  input.Read(Result, 1);
end;
function read_16(input: TStream): uint16_t;
begin
  input.Read(Result, 2);
  Result := BSwap16(Result);
end;
function read_24(input: TStream): uint32_t;
begin
  input.Read(Result, 3);
  Result := BSwap32(Result);
  Result := Result shr 8;
end;
function read_32(input: TStream): uint32_t;
begin
  input.Read(Result, 4);
  Result := BSwap32(Result);
end;
function read_64(input: TStream): uint64_t;
begin
  input.Read(Result, 8);
  Result := BSwap64(Result);
end;

{ write_ismv_head }
procedure write_ftyp(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);    // size
  write_32(output, FOURCC('ftyp')); // type
  write_32(output, FOURCC('isml')); // major_brand
  write_32(output, 1);    // minor_version
  write_32(output, FOURCC('piff')); // compatible_brands
  write_32(output, FOURCC('iso2'));
  write_len(output, s);
end;
procedure write_mvhd(output: TStream;
  version: uint8_t;
  next_track_ID: uint32_t;
  p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);    // size
  write_32(output, FOURCC('mvhd')); // type
  write_8(output, version);    // version=1
  write_24(output, 0);   // flags
  if version = 1 then
  begin
    write_64(output, CreationTime); // creation_time
    write_64(output, CreationTime); // modification_time
    write_32(output, p.timescale); // timescale
    write_64(output, p.Duration); // duration
  end
  else
  begin
    write_32(output, CreationTime); // creation_time
    write_32(output, CreationTime); // modification_time
    write_32(output, p.timescale); // timescale
    write_32(output, p.Duration); // duration
  end;
  write_32(output, $00010000); // rate, typically 1.0
  write_16(output, $0100);     // volume, typically, full volume
  write_16(output, 0);  // const bit(16) reserved = 0;
  write_32(output, 0);  // const unsigned int(32)[2] reserved = 0;
  write_32(output, 0);
  write_32(output, $00010000); // matrix[0]
  write_32(output, 0);
  write_32(output, 0);
  write_32(output, 0);
  write_32(output, $00010000); // matrix[4]
  write_32(output, 0);
  write_32(output, 0);
  write_32(output, 0);
  write_32(output, $40000000); // matrix[8]
  write_32(output, 0);  // pre_defined[0]
  write_32(output, 0);  // pre_defined[1]
  write_32(output, 0);  // pre_defined[2]
  write_32(output, 0);  // pre_defined[3]
  write_32(output, 0);  // pre_defined[4]
  write_32(output, 0);  // pre_defined[5]
  write_32(output, next_track_ID);  // next_track_ID
  write_len(output, s);
end;
procedure write_tkhd(output: TStream;
  version: uint8_t;
  track_ID: uint32_t;
  handler_type: THandlerType;
  p: PIsmvSettings);
var
  s: Int64;
  Width, Height: Integer;
  duration: Int64;
begin
  Width := p.VideoMaxWidth;
  height:= p.VideoMaxHeight;
  duration := p.Duration;
  
  s := output.Position;
  write_32(output, 0);    // size
  write_32(output, FOURCC('tkhd')); // type
  write_8(output, 1); // version=1
  write_24(output, 7); // 24-bit flags(track_enabled, track_in_movie, track_in_preview)
  if version = 1 then
  begin
    write_64(output, CreationTime); // creation_time
    write_64(output, CreationTime); // modification_time
    write_32(output, track_ID); // trackID
    write_32(output, 0); // reserved = 0
    write_64(output, duration); // duration
  end
  else
  begin
    write_32(output, CreationTime); // creation_time
    write_32(output, CreationTime); // modification_time
    write_32(output, track_ID); // trackID
    write_32(output, 0); // reserved = 0
    write_32(output, duration); // duration
  end;
  write_32(output, 0); // reserved[0] = 0
  write_32(output, 0); // reserved[1] = 0
  write_16(output, 0); // layer = 0
  write_16(output, 0); // alternate_group = 0;
  if handler_type = htSoun then
    write_16(output, $0100) // volume = {if track_is_audio 0x0100 else 0};
  else
    write_16(output, 0);
  write_16(output, 0);// reserved = 0;
  write_32(output, $00010000); // matrix[0] = 0x00010000
  write_32(output, 0);
  write_32(output, 0);
  write_32(output, 0);
  write_32(output, $00010000); // matrix[4] = 0x00010000
  write_32(output, 0);
  write_32(output, 0);
  write_32(output, 0);
  write_32(output, $40000000); // matrix[8] = 0x40000000
  if handler_type = htSoun then
  begin
    write_32(output, 0); // width = 0(audio)
    write_32(output, 0); // height= 0(audio)
  end
  else
  begin
    write_32(output, width shl 16);
    write_32(output, Height shl 16);
  end;
  write_len(output, s);
end;
procedure write_mdhd(output: TStream;
  version: Integer;
  p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);    // size
  write_32(output, FOURCC('mdhd')); // type
  write_8(output, 1); // version = 1
  write_24(output, 0); // flags
  if version = 1 then
  begin
    write_64(output, CreationTime); // creation_time
    write_64(output, CreationTime); // modification_time
    write_32(output, p.timescale); // timescale
    write_64(output, p.Duration);
  end
  else
  begin
    write_32(output, CreationTime); // creation_time
    write_32(output, CreationTime); // modification_time
    write_32(output, p.timescale); // timescale
    write_32(output, p.Duration);
  end;
  write_16(output, $55C4); // bit(1) pad = 0; unsigned int(5)[3] language;
  write_16(output, 0); // pre_defined = 0;
  write_len(output, s);
end;
procedure write_hdlr(output: TStream;
  handler_type: THandlerType);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);    // size
  write_32(output, FOURCC('hdlr')); // type
  write_8(output, 0); // version = 0
  write_24(output, 0);
  write_32(output, 0); // pre_defined = 0
  case handler_type of // handler_type = 'soun' ('vide', 'soun' or 'hint')
    htVide:
      begin
        write_str(output, 'vide');
        write_32(output, 0);
        write_64(output, 0);
        write_str(output, 'Video'+#0); // name = 'Video\0'
      end;
    htSoun:
      begin
        write_str(output, 'soun');
        write_32(output, 0);
        write_64(output, 0);
        write_str(output, 'Audio'+#0); // name = 'Audio\0'
      end;
    htHint:
      begin
        write_str(output, 'hint');
        write_32(output, 0);
        write_64(output, 0);        
        write_str(output, 'Hint'+#0); // name = 'Hint\0'
      end;
  end;
  write_len(output, s);
end;
procedure write_smhd(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);    // size
  write_32(output, FOURCC('smhd')); // type
  write_8(output, 0); // version = 0
  write_24(output, 0);
  write_16(output, 0); // balance = 0
  write_16(output, 0); // reserved = 0
  write_len(output, s);
end;
procedure write_vmhd(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('vmhd')); // type
  write_8(output, 0);   // version = 0
  write_24(output, 1);  // bit(24) flags = 1
  write_16(output, 0);  // template unsigned int(16) graphicsmode = 0; // copy, see below
  write_16(output, 0);  // template unsigned int(16)[3] opcolor = {0, 0, 0};
  write_16(output, 0);
  write_16(output, 0);
  write_len(output, s);
end;
procedure write_hmhd(output: TStream);
begin
    
end;
procedure write_nmhd(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('nmhd')); // type
  write_8(output, 0);   // version = 0
  write_24(output, 0);  // bit(24) flags = 0
  write_len(output, s);
end;
procedure write_url(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('url ')); // type
  write_8(output, 0);   // version = 0
  write_24(output, 1);  // bit(24) flags = 0x000001
  write_len(output, s);
end;
procedure write_dref(output: TStream; entry_count: uint32_t);
var
  s: Int64;
  i: Integer;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('dref')); // type
  write_8(output, 0);   // version = 0
  write_24(output, 0);  // bit(24) flags = 0
  write_32(output, entry_count);
  for i := 1 to entry_count do
  begin
    write_url(output);
  end;
  write_len(output, s);
end;
procedure write_dinf(output: TStream; entry_count: uint32_t);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('dinf')); // type
  write_dref(output, entry_count);
  write_len(output, s);
end;
procedure write_stts(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('stts')); // type
  write_64(output, 0); // bit(64) 0
  write_len(output, s);
end;
procedure write_ctts(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('ctts')); // type
  write_64(output, 0); // bit(64) 0
  write_len(output, s);
end;
procedure write_stsc(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('stsc')); // type
  write_64(output, 0); // bit(64) 0
  write_len(output, s);
end;
procedure write_stco(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('stco')); // type
  write_64(output, 0); // bit(64) 0
  write_len(output, s);
end;
procedure write_stsz(output: TStream);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('stsz')); // type
  write_64(output, 0); // bit(64) 0
  write_32(output, 0); // bit(32) 0
  write_len(output, s);
end;
procedure write_codec_private_data(output: TStream; codec_private_data: string; write_length: Boolean);
var
  pdbytes: Bytes;
begin
  pdbytes := HexStrToBytes(codec_private_data);
  if write_length then
    write_16(output, BSwap16(Length(pdbytes)));   // sample_entry->codec_private_data_length_
  write_bs(output, pdbytes);                      // sample_entry->codec_private_data_
end;
procedure write_wfex(output: TStream; p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('wfex')); // type
  write_16(output, BSwap16(p.AudioTag));          // sample_entry->wFormatTag
  write_16(output, BSwap16(p.AudioChannels));     // audio->channel_count
  write_32(output, BSwap32(p.AudioSamplingRate)); // audio->samplerate_ >> 16
  write_32(output, BSwap32(p.AudioBitrate shr 3));// sample_entry->nAvgBytesPerSec = Audio_Bitrate / 8
  write_16(output, BSwap16(p.AudioPacketSize));   // audio->packet_size_
  write_16(output, BSwap16(p.AudioBitsPerSample));// audio->sample_size_

  write_codec_private_data(output, p.AudioCodecPrivateData, True);

  write_len(output, s);
end;
procedure write_esds(output: TStream; p: PIsmvSettings);
var
  s: Int64;
  privdata_arr: Bytes;
  privdata_arr_len: Integer;
const
  streamDependenceFlag  = 0;
  URL_Flag              = 0;
  OCRstreamFlag         = 0;
  streamPriority        = 0;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('esds')); // type
  write_8(output, 0); // version
  write_24(output, 0);// flags

  // ES_Descriptor ES_Des
  write_8(output, $03); // ES_DescrTag 见14496-1 Table 1
  write_8(output, $19); // Length Field：$03 or $19
  write_16(output, 0);  // bit(16) ES_ID;
  write_8(output, (streamDependenceFlag shl 7) //bit(1) streamDependenceFlag;
               or (URL_Flag shl 6)             //bit(1) URL_Flag;
               or (OCRstreamFlag shl 5)        //bit(1) OCRstreamFlag;
               or (streamPriority));           //bit(5) streamPriority;

  // DecoderConfigDescriptor decConfigDescr; {
  write_8(output, $04); // TAG
  write_8(output, $11); // Length 17
  write_8(output, $40); // objectTypeIndication 14496-1 Table8,
                        // 0x40是Audio ISO/IEC 14496-3
  write_8(output, $15); //15 :15(hex) =   // stream_type (0x11=vid, 0x15=aud)
                        //   :0001 0101
                        //   :0001 01   :streamType, 5是Audio Stream, 14496-1 Table9
                        //   :       0  :upStream
                        //   :        1 :reserved
  write_24(output, 0);       // bufferSizeDB
  write_32(output, 0);       // maxBitrate
  write_32(output, 0);       // avgBitrate

  // DecoderSpecificDescriptor decSpecificInfo {
  if Length(p.AudioCodecPrivateData) > 0 then
  begin
    privdata_arr := HexStrToBytes(p.AudioCodecPrivateData);
    privdata_arr_len := Length(privdata_arr);
    Assert(privdata_arr_len = 2);
    
    write_8(output, $05);               // DecSpecificInfoTag
    write_8(output, privdata_arr_len);  // Length Field：2
    Assert(Length(p.AudioCodecPrivateData) = 4);
    write_bs(output, privdata_arr);
//    write_16(output, StrToInt('$'+p.AudioCodecPrivateData));
                            // 1388 :14496-3 1.6
                            //      :1388(hex)=
                            //      :0001 0011 1000 1000(bit)
                            //      :0001 0                     :audioObjectType 2 GASpecificConfig
                            //      :      011 1                :samplingFrequencyIndex
                            //      :           000 1           :channelConfiguration 1
                            //      :                00         :cpConfig
                            //      :                  0        :directMapping
  end
  else // if objectTypeIndication = $40
  begin
    Assert(1<0);
  end;
  // } end: decSpecificInfo
  // } end: decConfigDescr

  { TODO: esds中，3个字节不知道什么意思，暂时没要 }
//  write_24(output, $56E500);

  // SLConfigDescriptor slConfigDescr;
  write_8(output, $06); //06       :SLConfigDescrTag
  write_8(output, $01); //   01    :Length Field：1
  write_8(output, $02); //      02 :predefined 0x02 Reserved for use in MP4 files

  write_len(output, s);
end;
procedure write_AudioSampleEntry(output: TStream;
  codingname: string; data_reference_index: uint16_t;
  p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);                  // size
  write_32(output, FOURCC(codingname)); // type
  write_24(output, 0);                    // const unsigned int(8)[6] reserved = 0;
  write_24(output, 0);
  write_16(output, data_reference_index); // unsigned int(16) data_reference_index

  write_32(output, 0);                    // const unsigned int(32)[2] reserved = 0;
  write_32(output, 0); 
  write_16(output, p.AudioChannels);      // template unsigned int(16) channelcount = 2;
  write_16(output, p.AudioBitsPerSample); // template unsigned int(16) samplesize = 16;
  write_16(output, 0);                    // unsigned int(16) pre_defined = 0;
  write_16(output, 0);                    // const unsigned int(16) reserved = 0;
  write_32(output, p.AudioSamplingRate shl 16); // template unsigned int(32) samplerate = {timescale of media}<<16;

  // 'owma' is immedialy followed by the codec private data
  if codingname = 'owma' then
  begin
    write_codec_private_data(output, p.AudioCodecPrivateData, False);
  end
  else
  // 'wma ' is following the standard more closely and needs a wfex box.
  if codingname = 'wma ' then
  begin
    write_wfex(output, p);
  end
  else
  if codingname = 'mp4a' then
  begin
    write_esds(output, p);
  end
  else
  begin
    { TODO: 暂时没用 }
  end;
  write_len(output, s);
end;
procedure write_dvc1(output: TStream;
  VideoCodecPrivateData: string);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('dvc1')); // type

  if Length(VideoCodecPrivateData) = 31 then
    write_16(output, $C890)
  else
    write_16(output, $C810);

  write_bs(output, [$24, $B8, $1D, $FF, $3F]);

  write_codec_private_data(output, VideoCodecPrivateData, False);

  write_len(output, s);
end;
procedure write_avcC(output: TStream;
  VideoCodecPrivateData: string;
  NALUnitLength: Integer);

  procedure get_sps_pps_from_priv_data(priv_data: string;
    out sps: string; out pps: string);
  const
    flag1: string = '00000001';
    flag2: string = '000001';
  var
    tmp: string;
    flag: string;
  begin
    if Pos(flag1, priv_data) = 1 then
      flag := flag1
    else
      flag := flag2;

    tmp := Copy(priv_data, Length(flag)+1, Length(priv_data)-Length(flag));
    sps := Copy(tmp, 1, Pos(flag,tmp)-1);
    tmp := Copy(tmp, Length(sps)+1, Length(tmp)-Length(sps));
    pps := Copy(tmp, Length(flag)+1, Length(tmp)-Length(flag));
  end;
const
  sequence_parameter_sets: Integer = 1;
  picture_parameter_sets: Integer  = 1;
var
  s: Int64;
  sps, pps: string;
  sps_bs, pps_bs: Bytes;  
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('avcC')); // type

  get_sps_pps_from_priv_data(VideoCodecPrivateData, sps, pps);
  sps_bs := HexStrToBytes(sps);
  pps_bs := HexStrToBytes(pps);

  // AVCDecoderConfigurationRecord {
  write_8(output, 1);                 // configuration_version
  write_8(output, sps_bs[1]);         // AVC profile_indication);
  write_8(output, sps_bs[2]);         // AVC profile_compatibility);
  write_8(output, sps_bs[3]);         // AVC level_indication);

  write_8(output, $FC or (NALUnitLength - 1));//write_8(p,0xfc|(nal_unit_length-1));

  write_8(output, $E0 or sequence_parameter_sets);//write_8(p,0xe0|sequence_parameter_sets);
  write_16(output, Length(sps_bs));       //write_16(p, sps.size());
  write_bs(output, sps_bs);               //memcpy(p, sps.data(), sps.size());

  write_8(output, picture_parameter_sets);//write_8(p,picture_parameter_sets);
  write_16(output, Length(pps_bs));       //write_16(p, pps.size());
  write_bs(output, pps_bs);               //memcpy(p, pps.data(), pps.size());
  
  // } end: AVCDecoderConfigurationRecord

  SetLength(sps_bs, 0);
  SetLength(pps_bs, 0);

  write_len(output, s);
end;
procedure write_btrt(output: TStream; p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('btrt')); // type
  { TODO: btrt中3个字段的值待定 }
  write_32(output, 35419);                //unsigned int(32) bufferSizeDB;
  write_32(output, p.VideoBitrate);       //unsigned int(32) maxBitrate;
  write_32(output, p.VideoBitrate shr 1); //unsigned int(32) avgBitrate;
  write_len(output, s);
end;
procedure write_VideoSampleEntry(output: TStream;
  codingname: string; data_reference_index: uint16_t;
  p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC(codingname)); // type
  write_24(output, 0);  // const unsigned int(8)[6] reserved = 0;
  write_24(output, 0);
  write_16(output, data_reference_index);  // unsigned int(16) data_reference_index

  write_16(output, 0); //unsigned int(16) pre_defined = 0;
  write_16(output, 0); //const unsigned int(16) reserved = 0;
  write_32(output, 0); //unsigned int(32)[3] pre_defined = 0;
  write_32(output, 0);
  write_32(output, 0);
  write_16(output, p.VideoMaxWidth); //unsigned int(16) width;
  write_16(output, p.VideoMaxHeight);//unsigned int(16) height;
  write_32(output, $00480000);//template unsigned int(32) horizresolution = 0x00480000; // 72 dpi
  write_32(output, $00480000);//template unsigned int(32) vertresolution = 0x00480000; // 72 dpi
  write_32(output, 0); //const unsigned int(32) reserved = 0;
  write_16(output, 1); //template unsigned int(16) frame_count = 1;
  write_64(output, 0); //string[32] compressorname;
  write_64(output, 0);
  write_64(output, 0);
  write_64(output, 0);
  write_16(output, $18);//template unsigned int(16) depth = 0x0018;
  write_16(output, $FFFF);//int(16) pre_defined = -1;

  if (codingname = 'vc-1')
      or (codingname = 'ovc1') then
  begin
    write_dvc1(output, p.VideoCodecPrivateData);
  end
  else //if (codingname = 'avc1') then
  begin
    write_avcC(output, p.VideoCodecPrivateData, p.VideoNALUnitLengthField);
    write_btrt(output, p);
  end;

  write_len(output, s);
end;
procedure write_HintSampleEntry(output: TStream);
begin

end;
procedure write_sample_entrys(output: TStream; entry_count: Integer;
    p: PIsmvSettings);
var
  i: Integer;
begin

end;
procedure write_stsd(output: TStream;
  handler_type: THandlerType;
  entry_count: Integer;
  p: PIsmvSettings
  );
var
  s: Int64;
  i: Integer;
begin
  s := output.Position;
  write_32(output, 0);    // size
  write_32(output, FOURCC('stsd')); // type
  write_8(output, 0);      // version = 0
  write_24(output, 0);     // bit(24) flags = 0
  write_32(output, entry_count); // entry_count

  for i := 1 to entry_count do
  begin
    case handler_type of
    htVide:
      begin
        if p.VideoFourCC = 'H264' then
          write_VideoSampleEntry(output, 'avc1', 1, p)
        else// if p.VideoFourCC = 'WVC1' then
          write_VideoSampleEntry(output, 'vc-1', 1, p);
        //write_VideoSampleEntry(output, 'ovc1', 1, p);
      end;
    htSoun:
      begin
        if (p.AudioFourCC = 'AACL')
            or (p.AudioFourCC = 'AACH')
            or (p.AudioFourCC = 'mp4a') then
//          write_AudioSampleEntry(output, 'owma', 1, p)
//          write_AudioSampleEntry(output, 'wma ', 1, p)
          write_AudioSampleEntry(output, 'mp4a', 1, p)
        else
        //if (p.AudioFourCC = 'WMA2')
        //    or (p.AudioFourCC = 'WMAP') then
          write_AudioSampleEntry(output, 'wma ', 1, p);
      end;
    htHint:
      begin
        write_HintSampleEntry(output);
      end;
    end;
  end;

  write_len(output, s);
end;
procedure write_stbl(output: TStream;
  handler_type: THandlerType;
  p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('stbl')); // type

  write_stts(output);
  write_ctts(output);
  write_stsc(output);
  write_stco(output);
  write_stsz(output);

  write_stsd(output, handler_type, 1, p);

  write_len(output, s);
end;
procedure write_minf(output: TStream;
  handler_type: THandlerType;
  p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('minf')); // type

  case handler_type of
    htSoun:
      write_smhd(output);
    htVide:
      write_vmhd(output);
//      htHint:
//        write_hmhd;
  else
    write_nmhd(output);
  end;

  write_dinf(output, 1);
  write_stbl(output, handler_type, p);
            
  write_len(output, s);
end;
procedure write_mdia(output: TStream;
  handler_type: THandlerType;
  p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('mdia')); // type

  // start moov.trak.mdia.mdhd
  write_mdhd(output, 1, p);

  // start moov.trak.mdia.hdlr
  write_hdlr(output, handler_type);

  // start moov.trak.mdia.minf
  write_minf(output, handler_type, p);

  write_len(output, s);
end;
procedure write_trak(output: TStream;
  track_ID: uint32_t;
  handler_type: THandlerType;
  p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0);              // size
  write_32(output, FOURCC('trak')); // type
    
  // start moov.trak.tkhd
  write_tkhd(output, 1, track_ID, handler_type, p);

  // start moov.trak.mdia
  write_mdia(output, handler_type, p);

  write_len(output, s);
end;
procedure write_mehd(output: TStream;
  version: uint8_t; fragment_duration: Int64);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('mehd')); // type
  write_8(output, version); // version
  write_24(output, 0); // flags
  if version = 1 then
    write_64(output, fragment_duration)  // unsigned int(64) fragment_duration;
  else
    write_32(output, fragment_duration); // unsigned int(32) fragment_duration;
  write_len(output, s);
end;
procedure write_trex(output: TStream;
  track_ID: Integer);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('trex')); // type
  write_32(output, 0); // version & flags
  write_32(output, track_ID); //unsigned int(32) track_ID;
  write_32(output, 1);//unsigned int(32) default_sample_description_index;
  write_32(output, 0);//unsigned int(32) default_sample_duration;
  write_32(output, 0);//unsigned int(32) default_sample_size;
  write_32(output, 0);//unsigned int(32) default_sample_flags
  {default_sample_flags:
    bit(6) reserved=0;
    unsigned int(2) sample_depends_on;
    unsigned int(2) sample_is_depended_on;
    unsigned int(2) sample_has_redundancy;
    bit(3) sample_padding_value;
    bit(1) sample_is_difference_sample;
      // i.e. when 1 signals a non-key or non-sync sample
    unsigned int(16) sample_degradation_priority;}
  write_len(output, s);
end;
procedure write_mvex(output: TStream;
  fragment_duration: uint64_t);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('mvex')); // type
  write_mehd(output, 1, fragment_duration);
  write_trex(output, 1);
  write_trex(output, 2);
  write_len(output, s);
end;
procedure write_moov(output: TStream; p: PIsmvSettings);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('moov')); // type

  // moov.mvhd
  write_mvhd(output, 1, 3, p);

  // moov.mvex
  write_mvex(output, p.Duration);

  // moov.trak 1 (audio)
  write_trak(output, 1, htSoun, p);

  // moov.trak 2 (video)
  write_trak(output, 2, htVide, p);

  write_len(output, s);
end;


procedure write_ismv_head(output: TStream; p: PIsmvSettings);
begin
  write_ftyp(output);

  write_moov(output, p);

//  write_mvex(output, p.Duration);
end;

{ write_ismv_tail }
procedure write_tfra(output: TStream;
  version: uint8_t;
  track_ID: uint32_t;
  start_times, moof_offsets: TStringList);
var
  s: Int64;
  I, count: Integer;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('tfra')); // type
  write_8(output, version); // version = 1
  write_24(output, 0);
  write_32(output, track_ID); //track_ID
  write_32(output, 0);

  count := start_times.Count;
  write_32(output, count);
  for I:=0 to count-1 do
  begin
    if (version = 1) then // int64
    begin
      write_64(output, StrToInt64(start_times[I]));
      write_64(output, StrToInt64(moof_offsets[I]));
    end
    else // int32
    begin
      write_32(output, StrToInt64(start_times[I]));
      write_32(output, StrToInt64(moof_offsets[I]));
    end;
    write_24(output, $101010);
  end;

  write_len(output, s);
end;

procedure write_mfro(output: TStream;
  version: uint8_t);
var
  s: Int64;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('mfro')); // type
  write_8(output, version); // version = 0
  write_24(output, 0);
  write_32(output, 0); // size of mfra
  write_len(output, s);
end;

procedure write_mfra(output: TStream; p: PIsmvSettings;
  VideoMoofOffsets, AudioMoofOffsets: TStringList);
var
  s: Int64;
  mfraSize: uint32_t;
begin
  s := output.Position;
  write_32(output, 0); // size
  write_32(output, FOURCC('mfra')); // type

  write_tfra(output, 1, 1, p.AudioStartTimes, AudioMoofOffsets);
  write_tfra(output, 1, 2, p.VideoStartTimes, VideoMoofOffsets);
  write_mfro(output, 0);

  { wirte mfra size at the beginning and ending of mfra block }
  mfraSize := write_len(output, s);
  output.Position := output.Position - 4;
  write_32(output, mfraSize);
end;

procedure write_ismv_tail(output: TStream; p: PIsmvSettings;
  VideoMoofOffsets, AudioMoofOffsets: TStringList);
begin
  write_mfra(output, p, VideoMoofOffsets, AudioMoofOffsets);
end;

procedure write_ismv_body(output: TStream; InputFileList: TStringList;
  out VideoMoofOffsets, AudioMoofOffsets: TStringList);
var
  I, count: Integer;
  input: TFileStream;
  SortedFileNames: array of string;
  SVideoFileNames, SAudioFileNames: array of string;
  videoNum, audioNum: Integer;
  existedSeqNum: Boolean;
  buf: array[0..3] of Byte;
  seqNum: Integer;
  trackId: Integer;
  curName: string;
  curIdx: Integer;
begin
  count := InputFileList.Count;
  SetLength(SortedFileNames, count + 1);

  // sort splits by sequence number
  existedSeqNum := False;
  for I:=0 to count-1 do
  begin
    if not FileExists(InputFileList[I]) then Exit;

    input := TFileStream.Create(InputFileList[I], fmOpenRead);
    try
      input.Position := 20;
      input.ReadBuffer(buf, 4);
      seqNum := BSwap32(Integer(buf));

      if SortedFileNames[seqNum] <> '' then
      begin
        existedSeqNum := True;
        Break;
      end;

      SortedFileNames[seqNum] := InputFileList[I];
    finally
      input.Free;
    end;
  end;

  // sort splits by file name
  videoNum := 0;
  audioNum := 0;
  if existedSeqNum then
  begin
    for I:=0 to count do
    begin
      SortedFileNames[I] := EmptyStr;
    end;

    for I:=0 to count-1 do
    begin
      curName := Copy(InputFileList[I],
            Length(ExtractFilePath(InputFileList[I]))+1,
            Length(InputFileList[I])-Length(ExtractFilePath(InputFileList[I])));
      curIdx := StrToInt(Copy(curName, 2, Length(curName)-Length(ExtractFileExt(curName))-1));
      if curName[1] = 'v' then
      begin
        Inc(videoNum);
      end
      else // curName[1] = 'a'
      begin
        Inc(audioNum);
      end;
    end;

    SetLength(SVideoFileNames, videoNum+1);
    SetLength(SAudioFileNames, audioNum+1);
    for I:=0 to count-1 do
    begin
      curName := Copy(InputFileList[I],
            Length(ExtractFilePath(InputFileList[I]))+1,
            Length(InputFileList[I])-Length(ExtractFilePath(InputFileList[I])));
      curIdx := StrToInt(Copy(curName, 2, Length(curName)-Length(ExtractFileExt(curName))-1));
      if curName[1] = 'v' then
      begin
        SVideoFileNames[curIdx] := InputFileList[I];
      end
      else // curName[1] = 'a'
      begin
        SAudioFileNames[curIdx] := InputFileList[I];
      end;
    end;
  end;

  // merge splits into output file
  if not existedSeqNum then
  begin
    for I:=0 to count do
    begin
      if not FileExists(SortedFileNames[I]) then Continue;

      input := TFileStream.Create(SortedFileNames[I], fmOpenRead);
      try
        input.Position := $2C;
        input.ReadBuffer(buf, 4);
        trackId := BSwap32(Integer(buf));
        if trackId = 1 then // audio track
        begin
          AudioMoofOffsets.Add(IntToStr(output.Position));
        end
        else
        begin
          VideoMoofOffsets.Add(IntToStr(output.Position));
        end;

        input.Position := 0;
        write_cp(output, input);
      finally
        input.Free;
      end;
    end;
  end
  else
  begin
    for I:=0 to videoNum do
    begin
      if not FileExists(SVideoFileNames[I]) then Continue;

      input := TFileStream.Create(SVideoFileNames[I], fmOpenRead);
      try
        VideoMoofOffsets.Add(IntToStr(output.Position));
        write_cp(output, input);
      finally
        input.Free;
      end;
    end;

    for I:=0 to audioNum do
    begin
      if not FileExists(SAudioFileNames[I]) then Continue;

      input := TFileStream.Create(SAudioFileNames[I], fmOpenRead);
      try
        AudioMoofOffsets.Add(IntToStr(output.Position));
        write_cp(output, input);
      finally
        input.Free;
      end;
    end;
  end;
end;

function len_mfra(tfraVersion, VideoPartCount, AudioPartCount: Integer): Int64;
begin
  if tfraVersion = 1 then
  begin
    Result := 72 + 19*VideoPartCount + 19*AudioPartCount;
  end
  else
  begin
    Result := 72 + 11*VideoPartCount + 11*AudioPartCount;
  end;
end;

{ check ismv part: including boxes of moof & mdat }
function check_ismv_part(input: TStream): Boolean;
var
  len: uint64_t;
  pos: Int64;
begin
  Result := False;
  pos := input.Position;
  try
    len := read_32(input);
    if input.Size <= input.Position + len - 4 then Exit;
    input.Position := input.Position + len - 4;

    len := read_32(input);
    if input.Size < input.Position + len - 4 then Exit;
  finally
    input.Position := pos;
  end;

  Result := True;
end;

end.
