unit uFrmDialogPage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmDialogPage }

  TfrmDialogPage = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDialogPage: TfrmDialogPage;
  typeLbl: TLabel;
  typeSelect: TListBox;
  addObjectImg, okImg: TImage;
  dividerShp: TShape;

implementation

uses
  uGUI, uFrmDash, uNoteObjects;

{$R *.lfm}

{ TfrmDialogPage }

procedure TfrmDialogPage.FormCreate(Sender: TObject);
var
  StrObj: TNoteObjects;
begin
  with frmDialogPage do
    begin
      Caption:= 'Create Page';
      Width:= 640;
      Height:= 480;
      Left:= frmDash.Left + ((frmDash.Width-Width) div 2);
      Top:= frmDash.Top + ((frmDash.Height-Height) div 2);
      Color:= clWhite;
    end;
  typeLbl:= TLabel.Create(nil);
  with typeLbl do
    begin
      Parent:= frmDialogPage;
      Left:= 8;
      Top:= 8;
      Caption:= 'Objects:';
      Width:= Canvas.TextWidth(Caption);
    end;
  typeSelect:= TListBox.Create(nil);
  with typeSelect do
    begin
      Parent:= frmDialogPage;
      Left:= 8;
      Top:= 28;
      Height:= frmDialogPage.Height-70;
      Width:= (frmDialogPage.Width div 2)-16;
      {
        IF ELEMENTS ARE ADDED TO TNoteObjects
        CHANGE THE BOUND:     Basic_TextBox
        TO THE FINAL ELEMENT, AND UPDATE IN THIS COMMENT
      }
      for StrObj:= noCGP_Header to Basic_TextBox do
        AddItem(StrNoteObjects[StrObj],nil);
    end;
  addObjectImg:= TImage.Create(nil);
  with addObjectImg do
    begin
      Parent:= frmDialogPage;
      Width:= 59;
      Height:= 25;
      Picture.LoadFromFile('data/addObjectImg.png');
      Cursor:= crHandPoint;
      Top:= frmDialogPage.Height-(Height+8);
      Left:= typeSelect.Left+typeSelect.Width-Width;
    end;
  dividerShp:= TShape.Create(nil);
  with dividerShp do
    begin
      Parent:= frmDialogPage;
      Left:= frmDialogPage.Width div 2;
      Height:= frmDialogPage.Height;
      Width:= 1;
      Pen.Color:= clBorderGrey;
    end;
  okImg:= TImage.Create(nil);
  with okImg do
    begin
      Parent:= frmDialogPage;
      Width:= 59;
      Height:= 25;
      Picture.LoadFromFile('data/okImg.png');
      Cursor:= crHandPoint;
      Top:= frmDialogPage.Height-(Height+8);
      Left:= frmDialogPage.Width-(Width+8);
    end;

end;

procedure TfrmDialogPage.FormResize(Sender: TObject);
begin
  with typeSelect do
    begin
      Height:= frmDialogPage.Height-70;
      Width:= (frmDialogPage.Width div 2)-16;
    end;
  with addObjectImg do
    begin
      Top:= frmDialogPage.Height-(Height+8);
      Left:= typeSelect.Left+typeSelect.Width-Width;
    end;
  with dividerShp do
    begin
      Left:= frmDialogPage.Width div 2;
      Height:= frmDialogPage.Height;
    end;
  with okImg do
    begin
      Top:= frmDialogPage.Height-(Height+8);
      Left:= frmDialogPage.Width-(Width+8);
    end;
end;

end.

