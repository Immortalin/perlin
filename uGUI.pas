unit uGUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Controls, Graphics, StdCtrls,
  lclintf, Dialogs;

const
  BUTTON_MAX = 16;
  PAGEPREVIEW_MAX = 16;

  SIDEBAR_WIDTH = 278;
  PAGEBAR_WIDTH = 240;
  TOPBAR_HEIGHT = 85;
  PAGEPREVIEW_HEIGHT = 96;

  clBorderGrey = $00F4F4F4;
  clTextGrey = $009A938A;
  clTextLightGrey = $00D3CABE;
  clDarkGrey = $00908782;


type
  TTagColor = (tcRed, tcOrange, tcGreen, tcLightBlue, tcPurple, tcPink);

  TNotebooksData = record
    Name: string;
    Tag: TTagColor;
  end;

  TLinksData = record
    Name, URL: string;
  end;

const
  TagColorStr : array [TTagColor] of string[10]
    = ('Red', 'Orange', 'Green', 'Light-Blue', 'Purple', 'Pink');

type

  TPagePreview = class
    private
      ParentContainer: TWinControl;
      Panel: TPanel;
      TagColor: TTagColor;
      TagImage: TImage;
      HeaderText, PreviewText: TLabel;
      VertLine: TShape;
      procedure MouseEnter(Sender: TObject);
      procedure MouseLeave(Sender: TObject);
    public
      constructor Create(Num: integer; TagClr: TTagColor; HeaderTxt, SubTxt: string; Container: TWinControl);
  end;

  TPagebar = class
    private
      ParentContainer: TWinControl;
      Panel: TPanel;
      Preview: array[0..PAGEPREVIEW_MAX-1] of TPagePreview;
      PageCount: integer;
      VertLine: TShape;
    public
      procedure FitToContainer;
      procedure AddPage(TagClr: TTagColor; HeaderTxt, SubTxt: string);
      constructor Create(Container: TWinControl);
  end;

  TSidebarButton = class
    private
      LblText, linkAddress: string;
      Circle: TImage;
      Panel: TPanel;
      lbl: TLabel;
      procedure MouseEnter(Sender: TObject);
      procedure MouseLeave(Sender: TObject);
      procedure MouseClick(Sender: TObject);
    public
      constructor CreateHeader(Text: string; Pos: integer; Container: TWinControl);
      constructor CreateNotebook(Text: string; TagClr: TTagColor; Pos: integer; Container: TWinControl);
      constructor CreateLink(Text, Link: string; Pos: integer; Container: TWinControl);
      destructor Destroy;
  end;

  TSidebarButtons = class
    private
      ParentContainer: TWinControl;
      Panel: TPanel;
      buttonsHeight: integer;
      buttonCount, notebookCount, linkCount: integer;
      Buttons: array [1..BUTTON_MAX] of TSidebarButton;
      StoredNotebooks: array [1..BUTTON_MAX] of TNotebooksData;
      StoredLinks: array[1..BUTTON_MAX] of TLinksData;
      procedure UpdateButtons;
    public
      function GetNotebookCount: integer;
      procedure Clear;
      procedure AddHeader(Text: string);
      procedure AddNotebook(Container: TWinControl; Text: string; TagClr: TTagColor);
      procedure AddLink(Text, Link: string);
      constructor Create(Container: TWinControl);
  end;

  TSidebar = class
    private
      ParentContainer: TWinControl;
      Panel: TPanel;
      VertLine, HorzLine: TShape;
      CornerImg: TImage;
    public
      Buttons: TSidebarButtons;
      procedure FitToContainer;
      constructor Create(Container: TWinControl);
  end;

  TTopbar = class
    private
      ParentContainer: TWinControl;
      Panel: TPanel;
      addImg: TImage;
      HorzLine: TShape;
      CreateOptions: TPanel;
      OptionBorder: TShape;
      OptionLabels: array [0..2] of TLabel;
      procedure MouseEnter(Sender: TObject);
      procedure MouseLeave(Sender: TObject);
      procedure MouseClick(Sender: TObject);
    public
      procedure FitToContainer;
      constructor Create(Container: TWinControl);
  end;

implementation

uses
  uFrmDialogNotebook, uFrmDialogPage, uFrmDialogLink, uFrmDash, uNotebook;

function TSidebarButtons.GetNotebookCount: integer;
begin
  GetNotebookCount:= notebookCount;
end;

procedure TSidebarButtons.UpdateButtons;
var
  i: integer;
begin
  Clear;
  if notebookCount>0 then
    begin
      AddHeader('Notebooks');
      for i:= 1 to notebookCount do
        begin
          with StoredNotebooks[i] do
            Buttons[i]:= TSidebarButton.CreateNotebook(Name, Tag, buttonsHeight, Panel);
          buttonsHeight:= buttonsHeight + 32;
        end;
    end;
  if linkCount>0 then
    begin
      AddHeader('Links');
      for i:= 1 to linkCount do
        begin
          with StoredLinks[i] do
            Buttons[i]:= TSidebarButton.CreateLink(Name, URL, buttonsHeight, Panel);
          buttonsHeight:= buttonsHeight + 32;
        end;
    end;
end;

destructor TSidebarButton.Destroy;
begin
  Panel.Destroy;
end;

procedure TSidebarButtons.Clear;
var
  i: integer;
begin
  buttonCount:= 0;
  buttonsHeight:= 8;
  for i:= 1 to buttonCount do
    Buttons[i].Destroy;
end;

procedure TPagePreview.MouseEnter(Sender: TObject);
begin
  Panel.Color:= clBorderGrey;
end;

procedure TPagePreview.MouseLeave(Sender: TObject);
begin
  Panel.Color:= clNone;
end;

constructor TPagePreview.Create(Num: integer; TagClr: TTagColor; HeaderTxt, SubTxt: string; Container: TWinControl);
var
  str: string;
begin
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Width:= PAGEBAR_WIDTH-1;
      Height:= PAGEPREVIEW_HEIGHT;
      Top:= Num*PAGEPREVIEW_HEIGHT;
      BevelWidth:= 0;
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
    end;
  VertLine:= TShape.Create(nil);
  with VertLine do
    begin
      Parent:= Panel;
      Width:= Panel.Width;
      Height:= 1;
      Top:= Panel.Height-1;
      Pen.Color:= clBorderGrey;
      Brush.Color:= clWhite;
      Cursor:= crHandPoint;
    end;
  TagImage:= TImage.Create(nil);
  with TagImage do
    begin
      Parent:= Panel;
      Left:= 12;
      Top:= 12;
      Width:= 8;
      Height:= 8;
      case TagClr of
        tcRed:
          str:= 'red';
        tcOrange:
          str:= 'orange';
        tcGreen:
          str:= 'green';
        tcLightBlue:
          str:= 'lightblue';
        tcPurple:
          str:= 'purple';
        tcPink:
          str:= 'pink';
      else
        str:= 'lightblue';
      end;
      Picture.LoadFromFile(concat('data/',str,'Dot.png'));
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
    end;
  HeaderText:= TLabel.Create(nil);
  with HeaderText do
    begin
      Parent:= Panel;
      Top:= 6;
      Left:= 32;
      Caption:= HeaderTxt;
      Font.Color:= clTextGrey;
      Font.Size:= 11;
      Width:= Canvas.TextWidth(HeaderTxt);
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
    end;
  PreviewText:= TLabel.Create(nil);
  with PreviewText do
    begin
      Parent:= Panel;
      AutoSize:= False;
      Top:= 34;
      Left:= 24;
      Caption:= SubTxt;
      Font.Color:= clTextGrey;
      Width:= PAGEBAR_WIDTH - 48;
      Height:= PAGEPREVIEW_HEIGHT - 48;
      WordWrap:= True;
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
    end;
end;

procedure TPagebar.AddPage(TagClr: TTagColor; HeaderTxt, SubTxt: string);
begin
  inc(PageCount);
  Preview[PageCount]:= TPagePreview.Create(PageCount, TagClr, HeaderTxt, SubTxt, Panel);
end;

procedure TPagebar.FitToContainer;
begin
  Panel.Height:= ParentContainer.Height-TOPBAR_HEIGHT;
  VertLine.Height:= Panel.Height;
end;

constructor TPagebar.Create(Container: TWinControl);
begin
  ParentContainer:= Container;
  PageCount:= -1;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= ParentContainer;
      Width:= PAGEBAR_WIDTH;
      Height:= ParentContainer.Height-TOPBAR_HEIGHT;
      Top:= TOPBAR_HEIGHT;
      Left:= SIDEBAR_WIDTH;
      BevelWidth:= 0;
      Color:= clWhite;
    end;
  VertLine:= TShape.Create(nil);
  with VertLine do
    begin
      Parent:= Panel;
      Pen.Color:= clBorderGrey;
      Width:= 1;
      Height:= Panel.Height;
      Left:= Panel.Width-1;
    end;
end;

procedure TTopbar.MouseEnter(Sender: TObject);
begin
  CreateOptions.Show;
  if Sender is TLabel then
    (Sender as TLabel).Color:= clBorderGrey;
end;

procedure TTopbar.MouseLeave(Sender: TObject);
begin
  CreateOptions.Hide;
  if Sender is TLabel then
    (Sender as TLabel).Color:= clNone;
end;

procedure TTopbar.MouseClick(Sender: TObject);
begin
  if Sender is TLabel then
    case (Sender as TLabel).Caption of
      '    Create Page': frmDialogPage.Show;
      '    Create Notebook': frmDialogNotebook.Show;
      '    Create Link': ;
    end;
end;

procedure TTopbar.FitToContainer;
begin
  Panel.Width:= ParentContainer.Width-SIDEBAR_WIDTH;
  addImg.Left:= Panel.Width-addImg.Width;
  HorzLine.Width:= Panel.Width;
  CreateOptions.Left:= ParentContainer.Width-CreateOptions.Width;
end;

constructor TTopbar.Create(Container: TWinControl);
var
  i: integer;
begin
  ParentContainer:= Container;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= ParentContainer;
      Height:= TOPBAR_HEIGHT;
      Width:= ParentContainer.Width-SIDEBAR_WIDTH;
      Left:= SIDEBAR_WIDTH;
      BevelWidth:= 0;
    end;
  addImg:= TImage.Create(nil);
  with addImg do
    begin
      Parent:= Panel;
      Width:= 91;
      Height:= TOPBAR_HEIGHT-1;
      Left:= Panel.Width-Width;
      Picture.LoadFromFile('data/addImg.png');
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
    end;
  HorzLine:= TShape.Create(nil);
  with HorzLine do
    begin
      Parent:= Panel;
      Width:= Panel.Width;
      Height:= 1;
      Pen.Color:= clBorderGrey;
      Top:= Panel.Height-1;
    end;
  CreateOptions:= TPanel.Create(nil);
  with CreateOptions do
    begin
      Parent:= ParentContainer;
      Width:= 160;
      Height:= 96;
      Left:= ParentContainer.Width-Width;
      Top:= Panel.Height-1;
      BevelWidth:= 0;
      Visible:= False;
      Color:= clWhite;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
    end;
  OptionBorder:= TShape.Create(nil);
  with OptionBorder do
    begin
      Parent:= CreateOptions;
      Height:= CreateOptions.Height;
      Width:= CreateOptions.Width;
      Pen.Color:= clBorderGrey;
      Brush.Color:= clWhite;
    end;
  for i:= 0 to 2 do
    begin
      OptionLabels[i]:= TLabel.Create(nil);
      with OptionLabels[i] do
        begin
          Parent:= CreateOptions;
          case i of
            0: Caption:= '    Create Page';
            1: Caption:= '    Create Notebook';
            2: Caption:= '    Create Link';
          end;
          {case i of // DEBUG
            0: Color:= clRed;
            1: Color:= clYellow;
            2: Color:= clLime;
          end;}
          Font.Color:= clTextGrey;
          AutoSize:= False;
          Width:= OptionBorder.Width;
          Height:= CreateOptions.Height div 3;
          Layout:= tlCenter;
          Alignment:= taLeftJustify;
          Top:= i*(CreateOptions.Height div 3);
          Cursor:= crHandPoint;
          OnMouseEnter:= @MouseEnter;
          OnMouseLeave:= @MouseLeave;
          OnClick:= @MouseClick;
        end;
    end;
end;

procedure TSidebarButton.MouseClick(Sender: TObject);
var
  i: integer;
begin
  if not (linkAddress = '') then
    OpenURL(linkAddress)
  else
    for i:= 1 to sidebar.Buttons.GetNotebookCount do
      if notebooks[i].Name = lbl.Caption then
        notebooks[i].Notebook.Show
      else
        notebooks[i].Notebook.Hide;
end;

procedure TSidebarButton.MouseEnter(Sender: TObject);
begin
  lbl.Font.Color:= clTextLightGrey;
end;

procedure TSidebarButton.MouseLeave(Sender: TObject);
begin
  lbl.Font.Color:= clTextGrey;
end;

constructor TSidebarButton.CreateHeader(Text: string; Pos: integer; Container: TWinControl);
var
  i: integer;
  SpacedOutText: string;
begin
  linkAddress:= '';
  // text -> t e x t
  SpacedOutText:= '';
  for i:= 1 to length(Text) do
    SpacedOutText:= SpacedOutText + Text[i] + ' ';
  LblText:= SpacedOutText;
  //
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Height:= 40;
      Width:= SIDEBAR_WIDTH-16;
      Left:= 8;
      Top:= Pos;
      BevelWidth:= 0;
      BorderStyle:= bsNone;
      BorderWidth:= 0;
      Color:= clWhite;
    end;
  lbl:= TLabel.Create(nil);
  with lbl do
    begin
      Parent:= Panel;
      Font.Color:= clTextGrey;
      Font.Style:= [fsBold];
      Font.Size:= 9;
      Caption:= SpacedOutText;
      Top:= (Panel.Height-Height) div 2;
      Left:= 24;
      Width:= Canvas.TextWidth(SpacedOutText);
    end;
end;

constructor TSidebarButton.CreateNotebook(Text: string; TagClr: TTagColor; Pos: integer; Container: TWinControl);
var
  str: string;
begin
  linkAddress:= '';
  LblText:= Text;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Height:= 40;
      Width:= SIDEBAR_WIDTH-16;
      Left:= 8;
      Top:= Pos;
      BevelWidth:= 0;
      BorderStyle:= bsNone;
      BorderWidth:= 0;
      Color:= clWhite;
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
      OnClick:= @MouseClick;
    end;
  lbl:= TLabel.Create(nil);
  with lbl do
    begin
      Parent:= Panel;
      Font.Color:= clTextGrey;
      Font.Size:= 9;
      Caption:= Text;
      Top:= (Panel.Height-Height) div 2;
      Left:= 44;
      Width:= Canvas.TextWidth(Text);
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
      OnClick:= @MouseClick;
    end;
  Circle:= TImage.Create(Panel);
  with Circle do
    begin
      Parent:= Panel;
      case TagClr of
        tcRed:
          str:= 'red';
        tcOrange:
          str:= 'orange';
        tcGreen:
          str:= 'green';
        tcLightBlue:
          str:= 'lightblue';
        tcPurple:
          str:= 'purple';
        tcPink:
          str:= 'pink';
      else
        str:= 'lightblue';
      end;
      Picture.LoadFromFile(concat('data/',str,'Dot.png'));
      Width:= 8;
      Height:= 8;
      Top:= (Panel.Height-Height) div 2;
      Left:= Top;
    end;
end;

constructor TSidebarButton.CreateLink(Text, Link: string; Pos: integer; Container: TWinControl);
begin
  linkAddress:= Link;
  LblText:= Text;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Height:= 40;
      Width:= SIDEBAR_WIDTH-16;
      Left:= 8;
      Top:= Pos;
      BevelWidth:= 0;
      BorderStyle:= bsNone;
      BorderWidth:= 0;
      Color:= clWhite;
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
    end;
  lbl:= TLabel.Create(nil);
  with lbl do
    begin
      Parent:= Panel;
      Font.Color:= clTextGrey;
      Font.Size:= 9;
      Caption:= Text;
      Top:= (Panel.Height-Height) div 2;
      Left:= 44;
      Width:= Canvas.TextWidth(Text);
      Cursor:= crHandPoint;
      OnMouseEnter:= @MouseEnter;
      OnMouseLeave:= @MouseLeave;
      OnClick:= @MouseClick;
    end;
end;

procedure TSidebarButtons.AddHeader(Text: string);
begin
  inc(buttonCount);
  Buttons[buttonCount]:= TSidebarButton.CreateHeader(Text, buttonsHeight, Panel);
  buttonsHeight:= buttonsHeight+32;
end;

procedure TSidebarButtons.AddNotebook(Container: TWinControl; Text: string; TagClr: TTagColor);
begin

  inc(notebookCount);
  ParentContainer:= Container;
  with StoredNotebooks[notebookCount] do
    begin
      Name:= Text;
      Tag:= TagClr;
    end;
  with Notebooks[notebookCount] do
    begin
      Name:= Text;
      Tag:= TagClr;
      Notebook:= uNotebook.TNotebook.Create(Container, Text, TagClr);
    end;
  UpdateButtons;
end;

procedure TSidebarButtons.AddLink(Text, Link: string);
begin
  inc(buttonCount);
  inc(linkCount);
  with StoredLinks[linkCount] do
    begin
      Name:= Text;
      URL:= Link;
    end;
  UpdateButtons;
end;

constructor TSidebarButtons.Create(Container: TWinControl);
begin
  buttonsHeight:= 8;
  buttonCount:= 0;
  notebookCount:= 0;
  linkCount:= 0;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Left:= 0;
      Top:= 85;
      Height:= Container.Height-Top;
      Width:= Container.Width-1;
      BevelWidth:= 0;
      BorderWidth:= 0;
      BorderStyle:= bsNone;
    end;
end;

procedure TSidebar.FitToContainer;
begin
  Panel.Height:= ParentContainer.Height;
  VertLine.Height:= Panel.Height;
end;

constructor TSidebar.Create(Container: TWinControl);
begin
  ParentContainer:= Container;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Height:= Container.Height+1;
      Width:= SIDEBAR_WIDTH;
      BorderWidth:= 0;
      Color:= clWhite;
    end;
  VertLine:= TShape.Create(nil);
  with VertLine do
    begin
      Parent:= Panel;
      Height:= Panel.Height;
      Width:= 1;
      Left:= SIDEBAR_WIDTH-1;
      Pen.Color:= clBorderGrey;
    end;
  HorzLine:= TShape.Create(nil);
  with HorzLine do
    begin
      Parent:= Panel;
      Height:= 1;
      Width:= SIDEBAR_WIDTH;
      Top:= 84;
      Pen.Color:= clBorderGrey;
    end;
  CornerImg:= TImage.Create(nil);
  with CornerImg do
    begin
      Parent:= Panel;
      Width:= SIDEBAR_WIDTH;
      Height:= 84;
      Picture.LoadFromFile('data/cornerImg.png');
    end;
  Buttons:= TSidebarButtons.Create(Panel);
end;

end.

