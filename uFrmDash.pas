unit uFrmDash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, lcltype;

type

  { TfrmDash }

  TfrmDash = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDash: TfrmDash;

implementation

uses
  uSidebar;

var
  sidebar: TSidebar;
  topbar: TTopbar;
  debugLbl: TLabel;

{$R *.lfm}

{ TfrmDash }

procedure TfrmDash.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure TfrmDash.FormCreate(Sender: TObject);
begin
  with frmDash do
    begin
      Width:= 1024;
      Height:= 720;
      Color:= clWhite;
    end;
  topbar:= TTopbar.Create(frmDash);
  sidebar:= TSidebar.Create(frmDash);
  with sidebar.Buttons do
    begin
      AddHeader('Notebooks');
      AddNotebook('All Notes');
      AddNotebook('Computer-Science');
      AddNotebook('Further Pure 2');
      AddNotebook('Statistics 2');
      AddNotebook('Statistics 3');
      AddHeader('Tags');
      AddTag('Computer-Science',tcLightBlue);
      AddTag('Mathematics',tcPurple);
      AddTag('Further Maths.',tcPink);
    end;
  debugLbl:= TLabel.Create(nil);
  with debugLbl do
    begin
      Parent:= frmDash;
      Caption:= 'debug';
      Width:= Canvas.TextWidth('debug');
      Left:= frmDash.Width-Width;
      Top:= frmDash.Height-Height;
      Visible:= False;
    end;
end;

procedure TfrmDash.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    with debugLbl do
      Visible:= not Visible;
end;


procedure TfrmDash.FormResize(Sender: TObject);
begin
  sidebar.FitToContainer;
  topbar.FitToContainer;
  with debugLbl do
    begin
      Caption:= concat(inttostr(frmDash.Width),' × ',inttostr(frmDash.Height));
      Width:= Canvas.TextWidth(Caption);
      Left:= frmDash.Width-Width;
      Top:= frmDash.Height-Height;
    end;
end;

end.

