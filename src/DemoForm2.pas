unit DemoForm2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SLTask, ComCtrls, Menus, ImgList, ExtCtrls;

type
  TForm1 = class(TForm)
    btnXmlFile: TButton;
    dlgOpenXmlFile: TOpenDialog;
    lvTasks: TListView;
    pmControl: TPopupMenu;
    Stop1: TMenuItem;
    Remove1: TMenuItem;
    Start1: TMenuItem;
    dlgSaveMovie: TSaveDialog;
    imDownload: TImageList;
    cbbXmlUrl: TComboBox;
    procedure btnXmlFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure pmControlPopup(Sender: TObject);
  private
    { Private declarations }
    procedure DoSLTaskOnProgress(Sender: TObject; var Task: TSLTask);
    procedure DoSLTaskOnCompleted(Sender: TObject; var Task: TSLTask);
    procedure DoSLTaskOnFailed(Sender: TObject; var Task: TSLTask);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  SelectedVideoIndex = 2;
  SelectedAudioIndex = 0;

implementation

uses
  SLTaskMgr;

{$R *.dfm}

var
  FSLTaskManager: TSLTaskManager;

function GetFormattedBytes(FABytes: Double): string;
var
  ABytes: double;
begin
  try
    ABytes := FABytes;
    if (ABytes < 1024) then
    begin
      Result := Format('%.2n b', [ABytes]);
    end else
    begin
      ABytes := (ABytes / 1024);
      if (ABytes < 1024) then
      begin
        Result := Format('%.2n Kb', [ABytes]);
      end else
      begin
        ABytes := (ABytes / 1024);
        Result := Format('%.2n Mb', [ABytes]);
      end;
    end;
  except
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSLTaskManager := TSLTaskManager.Create;
  with FSLTaskManager do
  begin
    OnProgress := DoSLTaskOnProgress;
    OnCompleted := DoSLTaskOnCompleted;
    OnFailed := DoSLTaskOnFailed;
    LoadTaskList;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: Integer;
  PTask: PSLTask;
begin
  for i := 0 to FSLTaskManager.TaskCount - 1 do
  begin
    PTask := FSLTaskManager.Get(i);
    with lvTasks.Items.Add do
    begin
      ImageIndex := 1;
      with PTask^ do
      begin
        Caption := extractFileName(PTask.MovieFile);
        SubItems.Add(GetFormattedBytes(TotalSize));
        SubItems.Add(GetFormattedBytes(CurrentSize));
        if TotalSize <> 0 then
          SubItems.Add(Format('%3.1f', [100 * CurrentSize / TotalSize]) + '%')
        else
          SubItems.Add('0.0%'); // Format('%3.1f', [0]) + '%';
        SubItems.Add(GetFormattedBytes(Speed) + '/s');
        SubItems.Add(ID);
        Data := PTask;
      end;
    end;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FSLTaskManager) then FreeAndNil(FSLTaskManager);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FSLTaskManager) then FreeAndNil(FSLTaskManager);
end;

procedure TForm1.DoSLTaskOnCompleted(Sender: TObject; var Task: TSLTask);
var
  i: Integer;
begin
  for i := 0 to lvTasks.Items.Count - 1 do
  begin
    if lvTasks.Items[i].Data = @Task then
    begin
      lvTasks.Items[i].Delete;
      Break;
    end;
  end;
end;

procedure TForm1.DoSLTaskOnFailed(Sender: TObject; var Task: TSLTask);
var
  i: Integer;
begin
  for i := 0 to lvTasks.Items.Count - 1 do
  begin
    if lvTasks.Items[i].Data = @Task then
    begin
      lvTasks.Items[i].ImageIndex := 2; // failed
      Break;
    end;
  end;
end;

procedure TForm1.btnXmlFileClick(Sender: TObject);
var
  newTask: TSLNewTask;
  PTask: PSLTask;
begin
  if not dlgOpenXmlFile.Execute then Exit;
  with newTask do
  begin
    PageUrl := 'test page';
    XmlUrl := cbbXmlUrl.Text;
    XmlFile := dlgOpenXmlFile.FileName;
    VideoIndex := SelectedVideoIndex;
    AudioIndex := SelectedAudioIndex;
    if not dlgSaveMovie.Execute then Exit;
    MovieFile := dlgSaveMovie.FileName;
    HttpRequestMsg := nil; { TODO: HTTP msg }
//    MovieFile := 'C:\Users\xx\Desktop\test-201111204.ismv';
  end;
  PTask := FSLTaskManager.Add(newTask);
  with lvTasks.Items.Add do
  begin
    Caption := extractFileName(PTask.MovieFile);
    SubItems.Add('0'); // TotalSize
    SubItems.Add('0'); // CurrentSize
    SubItems.Add('0.0%');// Percent
    SubItems.Add('0'); // Speed
    SubItems.Add(PTask.ID);
    ImageIndex := 0;
    data := PTask;
  end;
  FSLTaskManager.Start(PTask.ID);
end;

procedure TForm1.DoSLTaskOnProgress(Sender: TObject; var Task: TSLTask);
var
  i: Integer;
  ptask: PSLTask;
begin
  for i := 0 to lvTasks.Items.Count - 1 do
  begin
    with lvTasks.Items[i] do
    begin
      ptask := Data;
      if ptask <> nil then
      begin
        if ptask^.ID = Task.ID then
        begin
          SubItems[0] := GetFormattedBytes(Task.TotalSize);
          SubItems[1] := GetFormattedBytes(Task.CurrentSize);
          if Task.TotalSize <> 0 then
            SubItems[2] := Format('%3.1f', [100 * Task.CurrentSize / Task.TotalSize]) + '%'
          else
            SubItems[2] := '0.0%'; // Format('%3.1f', [0]) + '%';
          SubItems[3] := GetFormattedBytes(Task.Speed) + '/s';
          SubItems[4] := Task.ID;
        end;
      end;
    end;
  end;
end;

procedure TForm1.Stop1Click(Sender: TObject);
var
  ptask: PSLTask;
begin
  ptask := lvTasks.Selected.Data;
  FSLTaskManager.Stop(ptask^.ID);
  lvTasks.Selected.ImageIndex := 1;
end;

procedure TForm1.Remove1Click(Sender: TObject);
var
  ptask: PSLTask;
begin
  ptask := lvTasks.Selected.Data;
  lvTasks.DeleteSelected;
  FSLTaskManager.Remove(ptask^.ID);
end;

procedure TForm1.Start1Click(Sender: TObject);
var
  ptask: PSLTask;
begin
  lvTasks.Selected.ImageIndex := 0;
  Sleep(10);
  ptask := lvTasks.Selected.Data;
  FSLTaskManager.Start(ptask^.ID);
  lvTasks.Selected.ImageIndex := 0;
end;

procedure TForm1.pmControlPopup(Sender: TObject);
begin
  if lvTasks.Selected = nil then
  begin
    Start1.Enabled := False;
    Stop1.Enabled := False;
    Remove1.Enabled := False;
    Exit;
  end;
  if FSLTaskManager.Get(lvTasks.Selected.Index).TaskState = tsRunning then
  begin
    Start1.Enabled := False;
    Stop1.Enabled := True;
    Remove1.Enabled := True;
  end
  else
  begin
    Start1.Enabled := True;
    Stop1.Enabled := False;
    Remove1.Enabled := True;
  end;
end;

end.

