unit uNotebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uGUI, StdCtrls, ExtCtrls, Graphics, Controls,
  uNoteObjects;

const
  MAX_PAGES_PER_NOTEBOOK = 20;
  MAX_OBJECTS_PER_PAGE = 20;

type
  TPage = class
    private
      Panel: TPanel;
      ParentContainer: TWinControl;
      ObjectCount, Bottom: integer;
      NoteObject: array [1..MAX_OBJECTS_PER_PAGE] of TNoteObject;
    public
      constructor Create(Container: TWinControl);
  end;

  TNotebook = class
    private
      Name: string;
      Tag: TTagColor;
      Panel: TPanel;
      ParentContainer: TWinControl;
      PageCount: integer;
      Page: array [1..MAX_PAGES_PER_NOTEBOOK] of TPage;
    public
      procedure Show;
      procedure Hide;
      constructor Create(Container: TWinControl; Text: string; Clr: TTagColor);
  end;

implementation

uses
  uFrmDash, Dialogs;

constructor TPage.Create(Container: TWinControl);
begin
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
      //Color:= clFuchsia;
    end;
  // DEBUG
  inc(ObjectCount);
  NoteObject[ObjectCount]:= TnoCGP_Header.Create(Panel,490,Bottom,'Header Example');
  inc(ObjectCount);
  NoteObject[ObjectCount]:= TnoCGP_Subheader.Create(Panel,200,Bottom,'Subheader');
  inc(ObjectCount);
  NoteObject[ObjectCount]:= TnoBasic_Textbox.Create(Panel,450,Bottom,'Lorem ipsum dolor sit amet, mei definiebas referrentur ut. Eos persius persecuti inciderint ea, in labore docendi qui. Qui in laboramus persecuti cotidieque, adhuc quaestio quo ei, delicata tincidunt sed eu. Id his prima facete invenire, qui habemus dignissim ut, id pertinacia ullamcorper cum. Dolorum electram abhorreant eu per. Est sint disputationi te. Recusabo facilisis persequeris cu pro, ei usu primis maluisset.');
  inc(ObjectCount);
  NoteObject[ObjectCount]:= TnoCGP_Subheader.Create(Panel,200,Bottom,'Subheader No 2');
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
  ParentContainer:= frmDash;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Width:= ParentContainer.Width;
      Height:= ParentContainer.Height;
      Visible:= True;
      BevelWidth:= 0;
      Color:= clDefault;
    end;
  // DEBUG
  inc(PageCount);
  Page[PageCount]:= TPage.Create(Panel);
end;

end.

