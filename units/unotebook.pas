unit uNotebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uGUI, StdCtrls, ExtCtrls, Graphics, Controls,
  uNoteObjects;

const
  MAX_PAGES_PER_NOTEBOOK = 24;
  MAX_OBJECTS_PER_PAGE = 50;

type
  TPage = class
    private
      Name, Desc: string;
      ParentContainer: TWinControl;
      ObjectCount: integer;
    public
      Panel: TPanel;
      NoteObject: array [1..MAX_OBJECTS_PER_PAGE] of TNoteObject;
      Bottom: integer;
      function GetName: string;
      function GetBottom: integer;
      function GetNoteCount: integer;
      function GetContainer: TWinControl;
      procedure Show;
      procedure Hide;
      procedure IncNotes;
      procedure IncBottom(num: integer);
      constructor Create(PageName, PageDesc: string; Container: TWinControl);
  end;

  TNotebook = class
    private
      Name: string;
      Tag: TTagColor;
      ParentContainer: TWinControl;
      PageCount: integer;
    public
      Page: array [1..MAX_PAGES_PER_NOTEBOOK] of TPage;
      Panel: TPanel;
      function GetPageName(ID: integer): string;
      function GetPageCount: integer;
      function LastPage: TPage;
      function GetName: string;
      procedure AddPage(PageName, PageDesc: string);
      procedure ShowPage(PageNum: integer);
      procedure Show;
      procedure Hide;
      constructor Create(Container: TWinControl; Text: string; Clr: TTagColor);
  end;

var
  notebookCurrentWidth: integer;

implementation

uses
  uFrmDash, Dialogs, uPanelCapture, CRT;

function TNotebook.GetPageName(ID: integer): string;
begin
  GetPageName:= Page[ID].GetName;
end;

function TNotebook.GetPageCount: integer;
begin
  GetPageCount:= PageCount;
end;

function TNotebook.LastPage: TPage;
begin
  LastPage:= Page[PageCount];
end;

procedure TNotebook.AddPage(PageName, PageDesc: string);
begin
  inc(PageCount);
  Page[PageCount]:= TPage.Create(PageName,PageDesc,Panel);
end;

procedure TPage.IncNotes;
begin
  ObjectCount:= ObjectCount + 1;
end;

procedure TPage.IncBottom(num: integer);
begin
  Bottom:= Bottom + num;
end;

function TPage.GetNoteCount: integer;
begin
  GetNoteCount:= ObjectCount;
end;

function TPage.GetBottom: integer;
begin
  GetBottom:= Bottom;
end;

function TPage.GetContainer: TWinControl;
begin
  GetContainer:= Panel;
end;

procedure TNotebook.ShowPage(PageNum: integer);
var
  k: integer;
begin
  for k:= 1 to PageCount do
    if not (k = PageNum) then
      Page[k].Hide
    else
      Page[k].Show;
end;

function TPage.GetName: string;
begin
  GetName:= Name;
end;

function TNotebook.GetName: string;
begin
  GetName:= Name;
end;

procedure TPage.Show;
{var
  temp1, temp2: integer; }
begin
  Panel.Visible:= True;
  // SCREENSHOT DEBUGGING
  {Delay(1000);
  temp1:= frmDash.Height;
  temp2:= notebookPanel.Height;
  uFrmDash.frmDash.ShowOnTop;
  uFrmDash.frmDash.FormStyle:= fsSystemStayOnTop;
  uFrmDash.frmDash.Height:= TOPBAR_HEIGHT+Panel.Height;
  uFrmDash.notebookPanel.Height:= Panel.Height;
  SaveContainerAsImage(Panel,'screenshots/');
  //uFrmDash.frmDash.Height:= temp1;
  //uFrmDash.notebookPanel.Height:= temp2;    }
end;

procedure TPage.Hide;
begin
  Panel.Visible:= False;
end;

constructor TPage.Create(PageName, PageDesc: string; Container: TWinControl);
begin
  Name:= PageName;
  Desc:= PageDesc;
  Bottom:= 8;
  ParentContainer:= Container;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Width:= Container.Width;
      Height:= Container.Height;
      BevelWidth:= 0;
      Color:= clWhite;
      Visible:= False;
    end;
end;

procedure TNotebook.Hide;
begin
  Panel.Visible:= False;
end;

procedure TNotebook.Show;
begin
  Panel.Visible:= True;
end;

constructor TNotebook.Create(Container: TWinControl; Text: string; Clr: TTagColor);
begin
  PageCount:= 0;
  Name:= Text;
  Tag:= Clr;
  ParentContainer:= Container;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Width:= ParentContainer.Width-SCROLLBAR_WIDTH;
      Height:= ParentContainer.Height;
      BevelWidth:= 0;
      Color:= clDefault;
      Visible:= False;
    end;
end;

end.

