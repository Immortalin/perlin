unit uFrmDialogPage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, uCreatePageGUI;

type

  { TfrmDialogPage }

  TfrmDialogPage = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);

    procedure TypeClick(Sender: TObject);
    procedure CreatePageClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ReloadNotebookCombo(Sender: TObject);
  end;

var
  frmDialogPage: TfrmDialogPage;
  notebookLbl, titleLbl, descLbl, typeLbl, paramLbl, styleLbl: TLabel;
  titleEdit: TEdit;
  notebookCombo, styleCombo: TCombobox;
  descMemo, scriptMemo: TMemo;
  typeSelect: TListBox;
  okImg: TImage;
  dividerShp: TShape;
  parameterPanel: TGroupBox;
  // Parameter Objects
  ParameterOptionsPanel: TParameterOptionsPanel;

implementation

uses
  uGUI, uFrmDash, uNoteObjects;

{$R *.lfm}

{ TfrmDialogPage }

procedure TfrmDialogPage.CreatePageClick(Sender: TObject);
var
  parentNotebook, pageName, description, filename, style: string;
  txt: TextFile;
  i: integer;
begin
  // assigning variables
  pageName:= titleEdit.Text;
  parentNotebook:= notebookCombo.Text;
  description:= descMemo.Text;
  filename:= lowercase(concat('notes/',parentNotebook,'_',pageName,'.perlin'));
  style:= styleCombo.Text;
  // file creation
  AssignFile(txt,filename);
  Rewrite(txt);
  writeln(txt,pageName);
  writeln(txt,description);
  writeln(txt,style);
  with scriptMemo do
    for i:= 0 to Lines.Count do
      writeln(txt,Lines[i]);
  closefile(txt);
  // add reference to notebook textfile
  filename:= lowercase(concat('notes/',parentNotebook,'.perlin'));
  AssignFile(txt,filename);
  Append(txt);
  writeln(txt,pageName);
  writeln(txt,description);
  closefile(txt);
  // reset all fields
  titleEdit.Text:= '';
  notebookCombo.Text:= '';
  descMemo.Text:= '';
  scriptMemo.Text:= '';
  // TO-DO: reset the parameter input fields
end;

procedure TfrmDialogPage.ReloadNotebookCombo(Sender: TObject);
var
  txt: TextFile;
  tempStr: string;
begin
  notebookCombo.Clear;
  AssignFile(txt,'notes/notebooks.perlin');
  Reset(txt);
  while not EOF(txt) do
    with notebookCombo do
      begin
        readln(txt,tempStr); // notebook name
        AddItem(tempStr,nil);
        readln(txt); // tag color, unneeded
      end;
  CloseFile(txt);
end;

procedure TfrmDialogPage.TypeClick(Sender: TObject);
var
  TypeList: TListBox;
  StrObj: TNoteObjects;
  headerInput: TEdit;
begin
  TypeList:= Sender as TListBox;
  with TypeList do
    begin
      for StrObj:= noHeader to noImage do
        if StrNoteObjects[StrObj] = Items[ItemIndex] then
          ParameterOptionsPanel.Setup(StrObj);
      parameterPanel.Caption:= Items[ItemIndex];
    end;
end;

procedure TfrmDialogPage.FormCreate(Sender: TObject);
var
  StrObj: TNoteObjects;
  StrStyle: TNoteStyle;
begin
  with frmDialogPage do
    begin
      Caption:= 'Create Page';
      Width:= 640;
      Height:= 480;
      Left:= frmDash.Left + ((frmDash.Width-Width) div 2);
      Top:= frmDash.Top + ((frmDash.Height-Height) div 2);
      //Color:= clWhite;
      Color:= clBorderGrey;
    end;
  typeLbl:= TLabel.Create(nil);
  with typeLbl do
    begin
      Parent:= frmDialogPage;
      Left:= 8;
      Top:= 206;
      Caption:= 'Objects:';
      Width:= Canvas.TextWidth(Caption);
    end;
  typeSelect:= TListBox.Create(nil);
  with typeSelect do
    begin
      Parent:= frmDialogPage;
      Left:= 8;
      Top:= 230;
      Height:= 208;
      Width:= (frmDialogPage.Width div 2)-16;
      {
        IF ELEMENTS ARE ADDED TO TNoteObjects
        CHANGE THE BOUND:     noBasic_TextBox
        TO THE FINAL ELEMENT, AND UPDATE IN THIS COMMENT
      }
      for StrObj:= noHeader to noImage do
        AddItem(StrNoteObjects[StrObj],nil);
      OnClick:= @TypeClick;
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
      OnClick:= @CreatePageClick;
    end;
  titleLbl:= TLabel.Create(nil);
  with titleLbl do
    begin
      Parent:= frmDialogPage;
      Caption:= 'Title:';
      Top:= 10;
      Left:= 8;
    end;
  titleEdit:= TEdit.Create(nil);
  with titleEdit do
    begin
      Parent:= frmDialogPage;
      Width:= 224;
      Left:= (frmDialogPage.Width div 2) - (Width + 8);
      Top:= 8;
    end;
  notebookLbl:= TLabel.Create(nil);
  with notebookLbl do
    begin
      Parent:= frmDialogPage;
      Caption:= 'Notebook:';
      Top:= 42;
      Left:= 8;
    end;
  notebookCombo:= TComboBox.Create(nil);
  with notebookCombo do
    begin
      Parent:= frmDialogPage;
      Width:= titleEdit.Width;
      Left:= titleEdit.Left;
      Top:= 40;
    end;
  descLbl:= TLabel.Create(nil);
  with descLbl do
    begin
      Parent:= frmDialogPage;
      Caption:= 'Description:';
      Top:= 74;
      Left:= 8;
    end;
  descMemo:= TMemo.Create(nil);
  with descMemo do
    begin
      Parent:= frmDialogPage;
      Width:= titleEdit.Width;
      Left:= titleEdit.Left;
      Top:= 72;
      Height:= 91;
    end;
  styleLbl:= TLabel.Create(nil);
  with styleLbl do
    begin
      Parent:= frmDialogPage;
      Caption:= 'Style:';
      Top:= 174;
      Left:= 8;
    end;
  styleCombo:= TCombobox.Create(nil);
  with styleCombo do
    begin
      Parent:= frmDialogPage;
      Width:= titleEdit.Width;
      Left:= titleEdit.Left;
      Top:= 172;
      for StrStyle:= nsCGP to nsBasic do
        AddItem(StrNoteStyles[StrStyle],nil);
    end;
  scriptMemo:= TMemo.Create(nil);
  with scriptMemo do
    begin
      Parent:= frmDialogPage;
      Width:= (frmDialogPage.Width div 2)-16;
      Height:= 160;
      Top:= frmDialogPage.Height-(Height+41);
      Left:= (frmDialogPage.Width div 2) + 8;
      WordWrap:= False;
      ScrollBars:= ssAutoBoth;
    end;
  parameterPanel:= TGroupBox.Create(nil);
  with parameterPanel do
    begin
      Parent:= frmDialogPage;
      Width:= (frmDialogPage.Width div 2)-16;
      Left:= (frmDialogPage.Width div 2) + 8;
      Top:= 8;
      Height:= frmDialogPage.Height - (64+scriptMemo.Height);
      Caption:= '';
      //BorderStyle:= bsNone;
      //BevelWidth:= 0;
    end;
  ParameterOptionsPanel:= TParameterOptionsPanel.Create(parameterPanel, scriptMemo);
end;

procedure TfrmDialogPage.FormResize(Sender: TObject);
begin
  // lock the form dimension in a dodgy way (not perminant)
  with frmDialogPage do
    begin
      Width:= 640;
      Height:= 480;
    end;
  {with typeSelect do
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
    end;   }
end;

end.

