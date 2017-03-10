unit uFrmDialogNotebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmDialogNotebook }

  TfrmDialogNotebook = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure okClick(Sender: TObject);
    procedure cancelClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDialogNotebook: TfrmDialogNotebook;
  nameLbl, tagLbl: TLabel;
  nameEdit: TEdit;
  tagSelect: TComboBox;
  okImg, cancelImg: TImage;

implementation

uses
  uGUI, uFrmDash;

{$R *.lfm}

{ TfrmDialogNotebook }

procedure TfrmDialogNotebook.okClick(Sender: TObject);
var
  TagClr, SelectedTagClr: TTagColor;
  NameStr: string;
  i: integer;
begin
  // Get Parameters
  SelectedTagClr:= tcRed;
  for TagClr:= tcRed to tcPink do
    if TagColorStr[TagClr] = tagSelect.SelText then
      SelectedTagClr:= TagClr;
  NameStr:= nameEdit.Text;
  // Create
  sidebar.Buttons.AddNotebook(notebookPanel, NameStr, SelectedTagClr);
  frmDialogNotebook.Hide;
end;

procedure TfrmDialogNotebook.cancelClick(Sender: TObject);
begin
  frmDialogNotebook.Hide;
end;

procedure TfrmDialogNotebook.FormCreate(Sender: TObject);
var
  TagClr: TTagColor;
begin
  with frmDialogNotebook do
    begin
      Caption:= 'Create Notebook';
      Width:= 604;
      Height:= 113;
      Left:= frmDash.Left + ((frmDash.Width-Width) div 2);
      Top:= frmDash.Top + ((frmDash.Height-Height) div 2);
      Color:= clBorderGrey; //TRAIL
      Visible:= False;
    end;
  nameLbl:= TLabel.Create(nil);
  with nameLbl do
    begin
      Parent:= frmDialogNotebook;
      Left:= 16;
      Top:= 18;
      Caption:= 'Name:'
    end;
  nameEdit:= TEdit.Create(nil);
  with nameEdit do
    begin
      Parent:= frmDialogNotebook;
      Top:= 16;
      Left:= 104;
      Width:= frmDialogNotebook.Width-118;
    end;
  tagLbl:= TLabel.Create(nil);
  with tagLbl do
    begin
      Parent:= frmDialogNotebook;
      Left:= 16;
      Top:= 46;
      Caption:= 'Tag-Color:'
    end;
  tagSelect:= TComboBox.Create(nil);
  with tagSelect do
    begin
      Parent:= frmDialogNotebook;
      Left:= 104;
      Top:= 43;
      Width:= frmDialogNotebook.Width-118;
      for TagClr:= tcRed to tcPink do
        AddItem(TagColorStr[TagClr],nil);
    end;
  okImg:= TImage.Create(nil);
  with okImg do
    begin
      Parent:= frmDialogNotebook;
      Width:= 59;
      Height:= 25;
      Left:= frmDialogNotebook.Width-(2*Width+27);
      Top:= frmDialogNotebook.Height-(Height+11);
      Picture.LoadFromFile('data/okImg.png');
      Cursor:= crHandpoint;
      OnClick:= @okClick;
    end;
  cancelImg:= TImage.Create(nil);
  with cancelImg do
    begin
      Parent:= frmDialogNotebook;
      Width:= 59;
      Height:= 25;
      Left:= frmDialogNotebook.Width-(Width+15);
      Top:= frmDialogNotebook.Height-(Height+11);
      Picture.LoadFromFile('data/cancelImg.png');
      Cursor:= crHandpoint;
      OnClick:= @cancelClick;
    end;
end;

procedure TfrmDialogNotebook.FormResize(Sender: TObject);
begin
  nameEdit.Width:= frmDialogNotebook.Width-118;
  tagSelect.Width:= frmDialogNotebook.Width-118;
  with okImg do
    begin
      Left:= frmDialogNotebook.Width-(2*Width+27);
      Top:= frmDialogNotebook.Height-(Height+11);
    end;
  with cancelImg do
    begin
      Left:= frmDialogNotebook.Width-(Width+15);
      Top:= frmDialogNotebook.Height-(Height+11);
    end;
end;

end.

