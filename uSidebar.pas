unit uSidebar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Controls, Graphics, StdCtrls;

const
  BUTTON_MAX = 16;
  SIDEBAR_WIDTH = 278;
  clBorderGrey = $00F4F4F4;
  clTextGrey = $009A938A; //8A939A
  clTextLightGrey = $00D3CABE;

type
  TTagColor = (tcRed, tcOrange, tcGreen, tcLightBlue, tcPurple, tcPink);

  TSidebarButton = class
    private
      LblText: string;
      Circle: TImage;
      Panel: TPanel;
      lbl: TLabel;
      procedure MouseEnter(Sender: TObject);
      procedure MouseLeave(Sender: TObject);
    public
      constructor CreateHeader(Text: string; Pos: integer; Container: TWinControl);
      constructor CreateTag(Text: string; TagClr: TTagColor; Pos: integer; Container: TWinControl);
      constructor CreateNotebook(Text: string; Pos: integer; Container: TWinControl);
  end;

  TSidebarButtons = class
    private
      Panel: TPanel;
      buttonsHeight, buttonCount: integer;
      Buttons: array [1..BUTTON_MAX] of TSidebarButton;
    public
      procedure AddHeader(Text: string);
      procedure AddTag(Text: string; TagClr: TTagColor);
      procedure AddNotebook(Text: string);
      constructor Create(Container: TWinControl);
  end;

  TSidebar = class
    private
      ParentContainer: TWinControl;
      Panel: TPanel;
      VertLine, HorzLine: TShape;
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
    public
      procedure FitToContainer;
      constructor Create(Container: TWinControl);
  end;

implementation

procedure TTopbar.FitToContainer;
begin
  Panel.Width:= ParentContainer.Width-SIDEBAR_WIDTH;
  addImg.Left:= Panel.Width-addImg.Width;
  HorzLine.Width:= Panel.Width;
end;

constructor TTopbar.Create(Container: TWinControl);
begin
  ParentContainer:= Container;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Container;
      Height:= 85;
      Width:= Container.Width-SIDEBAR_WIDTH;
      Left:= SIDEBAR_WIDTH;
      BevelWidth:= 0;
    end;
  addImg:= TImage.Create(nil);
  with addImg do
    begin
      Parent:= Panel;
      Width:= 91;
      Height:= 84;
      Left:= Panel.Width-Width;
      Picture.LoadFromFile('data/addImg.png');
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

constructor TSidebarButton.CreateNotebook(Text: string; Pos: integer; Container: TWinControl);
begin
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
    end;
end;

constructor TSidebarButton.CreateTag(Text: string; TagClr: TTagColor; Pos: integer; Container: TWinControl);
var
  str: string;
begin
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

procedure TSidebarButtons.AddHeader(Text: string);
begin
  inc(buttonCount);
  Buttons[buttonCount]:= TSidebarButton.CreateHeader(Text, buttonsHeight, Panel);
  buttonsHeight:= buttonsHeight+32;
end;

procedure TSidebarButtons.AddNotebook(Text: string);
begin
  inc(buttonCount);
  Buttons[buttonCount]:= TSidebarButton.CreateNotebook(Text, buttonsHeight, Panel);
  buttonsHeight:= buttonsHeight+32;
end;

procedure TSidebarButtons.AddTag(Text: string; TagClr: TTagColor);
begin
  inc(buttonCount);
  Buttons[buttonCount]:= TSidebarButton.CreateTag(Text, TagClr, buttonsHeight, Panel);
  buttonsHeight:= buttonsHeight+32;
end;

constructor TSidebarButtons.Create(Container: TWinControl);
begin
  buttonsHeight:= 8;
  buttonCount:= 0;
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
  Buttons:= TSidebarButtons.Create(Panel);
end;

end.

