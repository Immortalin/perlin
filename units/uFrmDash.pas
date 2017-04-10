unit uFrmDash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, lcltype, uGUI, uNotebook;

type

  { TfrmDash }

  TfrmDash = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure ScrollChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  MAX_NOTEBOOKS = 16;

type
  TNotebookRecord = record
    ID: integer;
    Notebook: TNotebook;
    Name: string;
    Tag: TTagColor;
  end;

var
  frmDash: TfrmDash;
  sidebar: TSidebar;
  topbar: TTopbar;
  pagebar: TPagebar;
  notebookPanel: TPanel;
  notebookScrollbar: TScrollBar;
  debugLbl: TLabel;
  notebookCount: integer;
  notebooks: array[1..MAX_NOTEBOOKS] of TNotebookRecord;

implementation

uses
  uLoadNotes;


{$R *.lfm}

{ TfrmDash }

procedure TfrmDash.ScrollChange(Sender: TObject);
var
  scroll: TScrollbar;
  book: integer;
begin
  scroll:= Sender as TScrollBar;
  for book:= 1 to notebookCount do
    begin
      with notebooks[book].Notebook.Panel do
        Top:= round((notebookPanel.Height-Height)*(scroll.Position/100));
    end;
end;

procedure TfrmDash.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure TfrmDash.FormCreate(Sender: TObject);
begin
  Randomize;
  notebookCount:= 0;
  with frmDash do
    begin
      Left:= 100;
      Top:= 100;
      Caption:= 'Perlin';
      Width:= 1024;
      Height:= 640;
      Color:= clDefault;
    end;
  notebookPanel:= TPanel.Create(frmDash);
  with notebookPanel do
    begin
      Parent:= frmDash;
      Width:= frmDash.Width-(SIDEBAR_WIDTH+PAGEBAR_WIDTH);
      Height:= frmDash.Height-TOPBAR_HEIGHT;
      Left:= SIDEBAR_WIDTH+PAGEBAR_WIDTH;
      Top:= TOPBAR_HEIGHT;
      BevelWidth:= 0;
      Visible:= False;
      Color:= clDefault;
    end;
  notebookScrollbar:= TScrollBar.Create(notebookPanel);
  with notebookScrollbar do
    begin
      Parent:= notebookPanel;
      Kind:= sbVertical;
      Min:= 1;
      Max:= 100;
      Position:= 1;
      Top:= 0;
      Left:= notebookPanel.Width-Width;
      OnChange:= @ScrollChange;
    end;
  topbar:= TTopbar.Create(frmDash);
  sidebar:= TSidebar.Create(frmDash);
  pagebar:= TPagebar.Create(frmDash);
  pagebar.Panel.Hide;
  with sidebar.Buttons do
    begin
      AddHeader('Notebooks');
      AddHeader('Links');
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
  notebookCurrentWidth:= 506-SCROLLBAR_WIDTH;
  LoadNotebooks(true);
  LoadLinks;
end;

procedure TfrmDash.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Showmessage('Key pressed');
  case Key of
  VK_F1: debugLbl.Visible:= Visible;
  VK_F5:
    begin
      pagebar.Clear;
      LoadNotebooks(false);
      ShowMessage('Loading...');
    end;
  end;

end;

procedure TfrmDash.FormResize(Sender: TObject);
begin
  with frmDash do
    begin
      Width:= 1024;
      Height:= 640;
    end;
  sidebar.FitToContainer;
  topbar.FitToContainer;
  pagebar.FitToContainer;
  with debugLbl do
    begin
      Caption:= concat(inttostr(frmDash.Width),' × ',inttostr(frmDash.Height));
      Width:= Canvas.TextWidth(Caption);
      Left:= frmDash.Width-Width;
      Top:= frmDash.Height-Height;
    end;
  with notebookScrollbar do
    Height:= notebookPanel.Height;
end;

end.

