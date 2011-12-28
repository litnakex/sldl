program SvltDownloader;

uses
  Forms,
  GFHTTPGet2 in 'src\GFHTTPGet2.pas',
  HttpDownload in 'src\HttpDownload.pas',
  HttpHeaderExtracter in 'src\HttpHeaderExtracter.pas',
  HttpMessage in 'src\HttpMessage.pas',
  IntelligentExt in 'src\IntelligentExt.pas',
  mp4_io in 'src\ismv\mp4_io.pas',
  IsmDoc in 'src\ismv\IsmDoc.pas',
  IsmvSettings in 'src\ismv\IsmvSettings.pas',
  SLTask in 'src\SLTask.pas',
  SLTaskMgr in 'src\SLTaskMgr.pas',
  DemoForm2 in 'src\DemoForm2.pas' {Form1},
  SLTaskList in 'src\SLTaskList.pas',
  PerlRegEx in 'src\PerlRegEx\PerlRegEx.pas',
  pcre in 'src\PerlRegEx\pcre.pas',
  MyUtils2 in 'src\MyUtils2.pas',
  MSXML2_TLB in 'C:\Program Files\Borland\Delphi7\Imports\MSXML2_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
