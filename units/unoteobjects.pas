unit uNoteObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, Graphics, Math;

type
  TNoteObjects = (noHeader,
                  noSubheader,
                  noChapterheader,
                  noText,
                  noKeyphrase,
                  noImage);
  TNoteStyle = (nsCGP,
                nsBasic);

const
  StrNoteObjects : array [TNoteObjects] of string[16]
    = ('Header',
       'Subheader',
       'Chapter-header',
       'Text',
       'Key-phrase',
       'Image');
  StrNoteStyles : array [TNoteStyle] of string[8]
    = ('CGP',
       'Basic');

  OBJECT_COUNT = 5;
  OBJECT_BUFFER = 12;

  CHAPTERHEADER_PREFIX = '%';
  HEADER_PREFIX = '#';
  SUBHEADER_PREFIX = '##';
  TEXT_PREFIX = '';
  KEYPHRASE_PREFIX = '**';
  IMAGE_PREFIX = '~';

  clCGPHeader = $00884A4F;
  clCGPSubheader = $00B4CBA8;
  clWashedPink = $00E5DEED;
  clWashedBlue = $00EEE1DE;
  clWashedRed = $00E1E1F5;

type
  TNoteObject = class
    private
      Container: TWinControl;
      Panel: TPanel;
    public
      destructor Destroy;
  end;

  TnoCGP_Header = class(TNoteObject)
    private
      Text: TLabel;
    public
      constructor Create(Cont: TWinControl; var Pos: integer; Str: string);
  end;

  TnoCGP_Subheader = class(TNoteObject)
    private
      Text: TLabel;
    public
      constructor Create(Cont: TWinControl; var Pos: integer; Str: string);
  end;

  TnoCGP_Keyphrase = class(TNoteObject)
    private
      Text: TLabel;
    public
      constructor Create(Cont: TWinControl; var Pos: integer; Str: string; Clr: TColor);
  end;

  TnoCGP_Chapterheader = class(TNoteObject)
    private
      Text: TLabel;
    public
      constructor Create(Cont: TWinControl; var Pos: integer; Str: ansistring);
  end;

  TnoBasic_Textbox = class(TNoteObject)
    private
      Text: TLabel;
    public
      constructor Create(Cont: TWinControl; var Pos: integer; Str: ansistring);
  end;

  TnoImage = class(TNoteObject)
    private
      Image: TImage;
    public
      constructor Create(Cont: TWinControl; var Pos: integer; Path: string; ImgWidth, ImgHeight: integer);
  end;

function StrToNote(StrNote: string): TNoteObjects;
function StrToStyle(StrStyle: string): TNoteStyle;

implementation

uses
  Dialogs, uNotebook;

function StrToNote(StrNote: string): TNoteObjects;
var
  i: TNoteObjects;
begin
  for i:= noHeader to noImage do
    if StrNote = StrNoteObjects[i] then
      StrToNote:= i;
end;

function StrToStyle(StrStyle: string): TNoteStyle;
var
  i: TNoteStyle;
begin
  for i:= nsCGP to nsBasic do
    if StrStyle = StrNoteStyles[i] then
      StrToStyle:= i;
end;

destructor TNoteObject.Destroy;
begin
  Panel.Destroy;
end;

constructor TnoImage.Create(Cont: TWinControl; var Pos: integer; Path: string; ImgWidth, ImgHeight: integer);
begin
  Container:= Cont;
  Pos:= Pos + 12;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Cont;
      Width:= notebookCurrentWidth-48;
      Height:= ImgHeight;
      Left:= 24;
      Top:= Pos-OBJECT_BUFFER;
      BevelWidth:= 0;
      Color:= clWhite;
    end;
  Image:= TImage.Create(nil);
  with Image do
    begin
      Parent:= Panel;
      Width:= ImgWidth;
      Height:= ImgHeight;
      Left:= (Panel.Width-Width) div 2;
      Stretch:= True;
      Picture.LoadFromFile(Path);
    end;
  Pos:= Pos + Panel.Height;
end;

constructor TnoBasic_Textbox.Create(Cont: TWinControl; var Pos: integer; Str: ansistring);
var
  strPart: ansistring;
  lineCount, i: integer;
begin
  Container:= Cont;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Cont;
      Width:= notebookCurrentWidth-32;
      Left:= 16;
      Top:= Pos;
      BevelWidth:= 0;
      Color:= clWhite;
    end;
  Text:= TLabel.Create(nil);
  with Text do
    begin
      Parent:= Panel;
      AutoSize:= False;
      WordWrap:= True;
      Width:= Panel.Width;
      Caption:= Str;
      Font.Size:= 9;
      // calculate string count
      strPart:= '';
      lineCount:= 1;
      for i:= 1 to length(str) do
        begin
          if strPart = '' then
            strPart:= str[i]
          else
            strPart:= concat(strPart,str[i]);
          if Canvas.TextWidth(strPart)>=Width then
            begin
              inc(lineCount);
              strPart:= '';
            end;
        end;
      Height:= (lineCount) * 15;
      //Height:= Canvas.TextFitInfo(Str,Width)-16;
    end;
  Panel.Height:= Text.Height;
  Pos:= Pos+Panel.Height+OBJECT_BUFFER;
end;

constructor TnoCGP_Header.Create(Cont: TWinControl; var Pos: integer; Str: string);
begin
  Container:= Cont;
  Pos:= Pos + 8; // increase spacing
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Cont;
      Width:= notebookCurrentWidth-16;
      Height:= 40; // VARIES BETWEEN OBJECTS
      Left:= 8;
      Top:= Pos;
      BevelWidth:= 0;
      Color:= clCGPHeader;
    end;
  Text:= TLabel.Create(nil);
  with Text do
    begin
      Parent:= Panel;
      Caption:= Str;
      Font.Color:= clWhite;
      Font.Size:= 14;
      Font.Style:= [fsBold];
      AutoSize:= False;
      Width:= Panel.Width;
      Height:= Panel.Height;
      Layout:= tlCenter;
      Alignment:= taCenter;
    end;
  Pos:= Pos+Panel.Height+OBJECT_BUFFER;
end;

constructor TnoCGP_Subheader.Create(Cont: TWinControl; var Pos: integer; Str: string);
begin
  Container:= Cont;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Cont;
      Width:= notebookCurrentWidth div 3;
      Height:= 28; // VARIES BETWEEN OBJECTS
      Left:= 24;
      Top:= Pos;
      BevelWidth:= 0;
      Color:= clCGPSubheader;
    end;
  Text:= TLabel.Create(nil);
  with Text do
    begin
      Parent:= Panel;
      Caption:= Str;
      Font.Color:= clBlack;
      Font.Size:= 12;
      Font.Style:= [fsItalic];
      AutoSize:= False;
      Width:= Canvas.TextWidth(Str);
      Left:= (Panel.Height-Height);
      Height:= Panel.Height;
      Layout:= tlCenter;
      Alignment:= taCenter;
    end;
  Panel.Width:= Text.Width+(2*Text.Left);
  Pos:= Pos+Panel.Height+OBJECT_BUFFER;

end;

constructor TnoCGP_Keyphrase.Create(Cont: TWinControl; var Pos: integer; Str: string; Clr: TColor);
begin
  Container:= Cont;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Cont;
      Width:= notebookCurrentWidth div 3;
      Height:= 24; // VARIES BETWEEN OBJECTS
      Top:= Pos;
      BevelWidth:= 0;
      Color:= clr;
    end;
  Text:= TLabel.Create(nil);
  with Text do
    begin
      Parent:= Panel;
      Caption:= Str;
      Font.Color:= clBlack;
      //Font.Style:= [fsItalic];
      AutoSize:= False;
      Width:= Canvas.TextWidth(Str);
      Left:= (Panel.Height-Height);
      Height:= Panel.Height;
      Layout:= tlCenter;
      Alignment:= taCenter;
    end;
  Panel.Width:= Text.Width+(2*Text.Left);
  Panel.Left:= (notebookCurrentWidth-Panel.Width) div 2;
  Pos:= Pos+Panel.Height+OBJECT_BUFFER;
end;

constructor TnoCGP_Chapterheader.Create(Cont: TWinControl; var Pos: integer; Str: string);
begin
  Container:= Cont;
  Panel:= TPanel.Create(nil);
  with Panel do
    begin
      Parent:= Cont;
      Width:= notebookCurrentWidth div 3;
      Height:= 24; // VARIES BETWEEN OBJECTS
      Top:= Pos;
      BevelWidth:= 0;
      Color:= clWhite;
    end;
  Text:= TLabel.Create(nil);
  with Text do
    begin
      Parent:= Panel;
      Caption:= Str;
      Font.Color:= clBlack;
      Font.Size:= 13;
      Font.Style:= [fsItalic];
      AutoSize:= False;
      Width:= Canvas.TextWidth(Str);
      Left:= (Panel.Height-Height);
      Height:= Panel.Height;
      Layout:= tlCenter;
      Alignment:= taCenter;
    end;
  Panel.Width:= Text.Width+(2*Text.Left);
  Panel.Left:= (notebookCurrentWidth-Panel.Width) div 2;
  Pos:= Pos+Panel.Height;
end;

end.

