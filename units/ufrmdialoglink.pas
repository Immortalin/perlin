unit uFrmDialogLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmDialogLink }

  TfrmDialogLink = class(TForm)
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
  frmDialogLink: TfrmDialogLink;
  nameLbl, linkLbl: TLabel;
  nameEdit, linkEdit: TEdit;
  okImg, cancelImg: TImage;

implementation

uses
  uFrmDash, uGUI;

{$R *.lfm}

{ TfrmDialogLink }

procedure TfrmDialogLink.okClick(Sender: TObject);
var
  txt: textfile;
begin
  sidebar.Buttons.AddLink(nameEdit.Text,linkEdit.Text);
  frmDialogLink.Hide;
  AssignFile(txt,'notes/links.perlin');
  Append(txt);
  writeln(txt,nameEdit.Text);
  writeln(txt,linkEdit.Text);
  CloseFile(txt);
  //
  nameEdit.Text:= '';
  linkEdit.Text:= '';
end;

procedure TfrmDialogLink.cancelClick(Sender: TObject);
begin
  frmDialogLink.Hide;
end;

procedure TfrmDialogLink.FormCreate(Sender: TObject);
begin
  with frmDialogLink do
    begin
      Caption:= 'Create Link';
      Width:= 604;
      Height:= 113;
      Left:= frmDash.Left + ((frmDash.Width-Width) div 2);
      Top:= frmDash.Top + ((frmDash.Height-Height) div 2);
      Color:= clBorderGrey;
      Visible:= False;
    end;
  nameLbl:= TLabel.Create(nil);
  with nameLbl do
    begin
      Parent:= frmDialogLink;
      Left:= 16;
      Top:= 18;
      Caption:= 'Text:'
    end;
  nameEdit:= TEdit.Create(nil);
  with nameEdit do
    begin
      Parent:= frmDialogLink;
      Top:= 16;
      Left:= 104;
      Width:= frmDialogLink.Width-118;
    end;
  linkLbl:= TLabel.Create(nil);
  with linkLbl do
    begin
      Parent:= frmDialogLink;
      Left:= 16;
      Top:= 46;
      Caption:= 'URL:'
    end;
  linkEdit:= TEdit.Create(nil);
  with linkEdit do
    begin
      Parent:= frmDialogLink;
      Left:= 104;
      Top:= 43;
      Width:= frmDialogLink.Width-118;
    end;
  okImg:= TImage.Create(nil);
  with okImg do
    begin
      Parent:= frmDialogLink;
      Width:= 59;
      Height:= 25;
      Left:= frmDialogLink.Width-(2*Width+27);
      Top:= frmDialogLink.Height-(Height+11);
      Picture.LoadFromFile('data/okImg.png');
      Cursor:= crHandpoint;
      OnClick:= @okClick;
    end;
  cancelImg:= TImage.Create(nil);
  with cancelImg do
    begin
      Parent:= frmDialogLink;
      Width:= 59;
      Height:= 25;
      Left:= frmDialogLink.Width-(Width+15);
      Top:= frmDialogLink.Height-(Height+11);
      Picture.LoadFromFile('data/cancelImg.png');
      Cursor:= crHandpoint;
      OnClick:= @cancelClick;
    end;
end;

procedure TfrmDialogLink.FormResize(Sender: TObject);
begin
  nameEdit.Width:= frmDialogLink.Width-118;
  linkEdit.Width:= frmDialogLink.Width-118;
  with okImg do
    begin
      Left:= frmDialogLink.Width-(2*Width+27);
      Top:= frmDialogLink.Height-(Height+11);
    end;
  with cancelImg do
    begin
      Left:= frmDialogLink.Width-(Width+15);
      Top:= frmDialogLink.Height-(Height+11);
    end;
end;

end.

