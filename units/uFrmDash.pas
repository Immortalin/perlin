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
  private
    { private declarations }
  public
    { public declarations }
  end;

  function AnyPageVisible(book: TNotebook): boolean;

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
  //G
  sidebar: TSidebar;
  topbar: TTopbar;
  pagebar: TPagebar;
  notebookPanel: TScrollBox;
  debugLbl: TLabel;
  notebookCount: integer;
  notebooks: array[1..MAX_NOTEBOOKS] of TNotebookRecord;

implementation

uses
  uLoadNotes, LCLIntf;


{$R *.lfm}

{ TfrmDash }

function AnyPageVisible(book: TNotebook): boolean;
var
  AnyVisible: boolean;
  pages: integer;
begin
  AnyVisible:= False;
    for pages:= 1 to book.GetPageCount do
      if book.Page[pages].Panel.Visible then
        AnyVisible:= True;
  AnyPageVisible:= AnyVisible;
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
  notebookPanel:= TScrollbox.Create(frmDash);
  with notebookPanel do
    begin
      Parent:= frmDash;
      Width:= frmDash.Width-(SIDEBAR_WIDTH+PAGEBAR_WIDTH);
      Height:= frmDash.Height-TOPBAR_HEIGHT;
      Left:= SIDEBAR_WIDTH+PAGEBAR_WIDTH;
      Top:= TOPBAR_HEIGHT;
      Visible:= True;
      Color:= clDefault;
      HorzScrollBar.Visible:= False;
      VertScrollBar.Visible:= False;
      VertScrollBar.Tracking:= True;
      BorderStyle:= bsNone;
     // OnMouseMove:= @ScrollChange;
      //OnMouseWheel:= @ScrollChange;
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
  {with frmDash do
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
    Height:= notebookPanel.Height;  }
end;

end.

