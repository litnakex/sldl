unit IntelligentExt;

interface

function GetIntelligentExt(Buf: array of Char): String;

type
  RExtInfo = packed record
    BeginIndex  : Integer;
    EndIndex    : Integer;
    Magic       : PChar;
    Ext         : PChar;
  end;

const
  asf_object_id : array[0..15] of Integer
  =($30, $26, $b2, $75,
    $8e, $66, $cf, $11,
    $a6, $d9, $00, $aa,
    $00, $62, $ce, $6c);

var
  ExtInfoList : array[0..31] of RExtInfo;


implementation

uses SysUtils;

procedure InitialExtInfoList;
begin
  ExtInfoList[0].BeginIndex := 4;
  ExtInfoList[0].EndIndex := 12;
  ExtInfoList[0].Magic := 'ftyp3gp4';
  ExtInfoList[0].Ext := '.3gp';

  ExtInfoList[1].BeginIndex := 2;
  ExtInfoList[1].EndIndex := 5;
  ExtInfoList[1].Magic := 'AMR';
  ExtInfoList[1].Ext := '.amr';

  ExtInfoList[2].BeginIndex := 11;
  ExtInfoList[2].EndIndex := 18;
  ExtInfoList[2].Magic := 'libfaac';
  ExtInfoList[2].Ext := '.aac';

  ExtInfoList[3].BeginIndex := 0;
  ExtInfoList[3].EndIndex := 16;
  ExtInfoList[3].Magic := PChar(@asf_object_id);
  ExtInfoList[3].Ext := '.asf';

  ExtInfoList[4].BeginIndex := 1;
  ExtInfoList[4].EndIndex := 4;
  ExtInfoList[4].Magic := 'snd';
  ExtInfoList[4].Ext := '.au';

  ExtInfoList[5].BeginIndex := 8;
  ExtInfoList[5].EndIndex := 11;
  ExtInfoList[5].Magic := 'AVI';
  ExtInfoList[5].Ext := '.avi';

  ExtInfoList[6].BeginIndex := 0;
  ExtInfoList[6].EndIndex := 4;
  ExtInfoList[6].Magic := 'fLaC';
  ExtInfoList[6].Ext := '.flac';

  ExtInfoList[7].BeginIndex := 0;
  ExtInfoList[7].EndIndex := 3;
  ExtInfoList[7].Magic := 'FLV';
  ExtInfoList[7].Ext := '.flv';

  ExtInfoList[8].BeginIndex := 0;
  ExtInfoList[8].EndIndex := 3;
  ExtInfoList[8].Magic := 'ID3';
  ExtInfoList[8].Ext := '.mp3';

  ExtInfoList[9].BeginIndex := 4;
  ExtInfoList[9].EndIndex := 8;
  ExtInfoList[9].Magic := 'ftyp';
  ExtInfoList[9].Ext := '.mp4';

  ExtInfoList[10].BeginIndex := 4;
  ExtInfoList[10].EndIndex := 8;
  ExtInfoList[10].Magic := 'moov';
  ExtInfoList[10].Ext := '.mp4';

  ExtInfoList[11].BeginIndex := 4;
  ExtInfoList[11].EndIndex := 8;
  ExtInfoList[11].Magic := 'moof';
  ExtInfoList[11].Ext := '.mp4';

  ExtInfoList[12].BeginIndex := 4;
  ExtInfoList[12].EndIndex := 12;
  ExtInfoList[12].Magic := 'ftypisom';
  ExtInfoList[12].Ext := '.m4a';

  ExtInfoList[13].BeginIndex := 4;
  ExtInfoList[13].EndIndex := 10;
  ExtInfoList[13].Magic := 'ftypqt';
  ExtInfoList[13].Ext := '.mov';

  ExtInfoList[14].BeginIndex := 0;
  ExtInfoList[14].EndIndex := 4;
  ExtInfoList[14].Magic := 'MMMD';
  ExtInfoList[14].Ext := '.mmf';

  ExtInfoList[15].BeginIndex := 0;
  ExtInfoList[15].EndIndex := 4;
  ExtInfoList[15].Magic := 'OggS';
  ExtInfoList[15].Ext := '.ogg';

  ExtInfoList[16].BeginIndex := 0;
  ExtInfoList[16].EndIndex := 3;
  ExtInfoList[16].Magic := '.ra';
  ExtInfoList[16].Ext := '.ra';

  ExtInfoList[17].BeginIndex := 1;
  ExtInfoList[17].EndIndex := 4;
  ExtInfoList[17].Magic := 'RMF';
  ExtInfoList[17].Ext := '.rm';

  ExtInfoList[18].BeginIndex := 0;
  ExtInfoList[18].EndIndex := 2;
  ExtInfoList[18].Magic := 'G@';
  ExtInfoList[18].Ext := '.ts';

  ExtInfoList[19].BeginIndex := 8;
  ExtInfoList[19].EndIndex := 11;
  ExtInfoList[19].Magic := 'WAV';
  ExtInfoList[19].Ext := '.wav';

  ExtInfoList[20].BeginIndex := 0;
  ExtInfoList[20].EndIndex := 3;
  ExtInfoList[20].Magic := 'Rar';
  ExtInfoList[20].Ext := '.rar';

  ExtInfoList[21].BeginIndex := 0;
  ExtInfoList[21].EndIndex := 2;
  ExtInfoList[21].Magic := 'MZ';
  ExtInfoList[21].Ext := '.exe';

  ExtInfoList[22].BeginIndex := 0;
  ExtInfoList[22].EndIndex := 2;
  ExtInfoList[22].Magic := 'BM';
  ExtInfoList[22].Ext := '.bmp';

  ExtInfoList[23].BeginIndex := 0;
  ExtInfoList[23].EndIndex := 3;
  ExtInfoList[23].Magic := 'II*';
  ExtInfoList[23].Ext := '.tif';

  ExtInfoList[24].BeginIndex := 12;
  ExtInfoList[24].EndIndex := 16;
  ExtInfoList[24].Magic := 'IHDR';
  ExtInfoList[24].Ext := '.png';

  ExtInfoList[25].BeginIndex := 0;
  ExtInfoList[25].EndIndex := 3;
  ExtInfoList[25].Magic := 'PAK';
  ExtInfoList[25].Ext := '.pak';

  ExtInfoList[26].BeginIndex := 0;
  ExtInfoList[26].EndIndex := 4;
  ExtInfoList[26].Magic := 'FWS';
  ExtInfoList[26].Ext := '.swf';

  ExtInfoList[27].BeginIndex := 0;
  ExtInfoList[27].EndIndex := 4;
  ExtInfoList[27].Magic := '%PDF';
  ExtInfoList[27].Ext := '.pdf';

  ExtInfoList[28].BeginIndex := 0;
  ExtInfoList[28].EndIndex := 3;
  ExtInfoList[28].Magic := 'GIF';
  ExtInfoList[28].Ext := '.gif';

  ExtInfoList[29].BeginIndex := 4;
  ExtInfoList[29].EndIndex := 8;
  ExtInfoList[29].Magic := 'JFIF';
  ExtInfoList[29].Ext := '.jpg';

  ExtInfoList[30].BeginIndex := 0;
  ExtInfoList[30].EndIndex := 6;
  ExtInfoList[30].Magic := '<html>';
  ExtInfoList[30].Ext := '.html';

  ExtInfoList[31].BeginIndex := 0;
  ExtInfoList[31].EndIndex := 6;
  ExtInfoList[31].Magic := '<HTML>';
  ExtInfoList[31].Ext := '.html';
end;

function MemCmp(const Buf1, Buf2: PChar; Size: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Size - 1 do
  begin
    if Buf1[I] <> Buf2[I] then Exit;
  end;
  Result := True;
end;

function GetIntelligentExt(Buf: array of Char): String;
var
  I, Len: Integer;
begin
  InitialExtInfoList;
  Len := Length(ExtInfoList);
  for I := 0 to Len - 1 do
  begin
    if MemCmp(@(Buf[ExtInfoList[I].BeginIndex]),
      ExtInfoList[I].Magic,
      ExtInfoList[I].EndIndex - ExtInfoList[I].BeginIndex) then
    begin
      Result := ExtInfoList[I].Ext;
      Exit;
    end;
  end;
  Result := '.flv';
end;

end.
