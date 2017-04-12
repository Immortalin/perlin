unit uCreatePageGUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, uNoteObjects;

type
  TParameterOptionsPanel = class
    private
      ScriptOut: TMemo;
      ObjectToBeCreated: TNoteObjects;
      ParentContainer: TWinControl;
      addScriptImg: TImage;
      // parameter input objects
      lineInput_1,lineInput_2,lineInput_3: TEdit;
      parameterLbl_1, parameterLbl_2, parameterLbl_3: TLabel;
      editBox_1: TMemo;
      //
      procedure AddScript(Sender: TObject);
    public
      procedure Setup(NoteObject: TNoteObjects);
      constructor Create(Container: TWinControl; TextOut: TMemo);
  end;

implementation

procedure TParameterOptionsPanel.AddScript(Sender: TObject);
begin
  with ScriptOut.Lines do
    begin
      case ObjectToBeCreated of
        noHeader:
          Add(concat(HEADER_PREFIX,lineInput_1.Text));
        noSubheader:
          Add(concat(SUBHEADER_PREFIX,lineInput_1.Text));
        noText:
          Add(concat(TEXT_PREFIX,editBox_1.Text));
        noKeyphrase:
          Add(concat(KEYPHRASE_PREFIX,lineInput_1.Text));
        noImage:
          Add(concat(IMAGE_PREFIX,lineInput_1.Text,
              '[',lineInput_2.Text,',',lineInput_3.Text,']'));
      end;
    end;
  lineInput_1.Text:= '';
  lineInput_2.Text:= '';
  lineInput_3.Text:= '';
  editBox_1.Text:= '';
end;

procedure TParameterOptionsPanel.Setup(NoteObject: TNoteObjects);
begin
  //
  ObjectToBeCreated:= NoteObject;
  // hide all input fields
  lineInput_1.Hide;
  lineInput_2.Hide;
  lineInput_3.Hide;
  parameterLbl_1.Hide;
  parameterLbl_2.Hide;
  parameterLbl_3.Hide;
  editBox_1.Hide;
  addScriptImg.Hide;
  // show required input fields
  case NoteObject of
    noHeader:
      begin
        lineInput_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Header:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noSubheader:
      begin
        lineInput_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Subheader:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noText:
      begin
        editBox_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Text:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noKeyphrase:
      begin
        lineInput_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Keyphrase:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noImage:
      begin
        lineInput_1.Show;
        lineInput_2.Show;
        lineInput_3.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Image Path:';
            Top:= lineInput_1.Top+2;
          end;
        with parameterLbl_2 do
          begin
            Show;
            Caption:= 'Width:';
            Top:= lineInput_2.Top+2;
          end;
        with parameterLbl_3 do
          begin
            Show;
            Caption:= 'Height:';
            Top:= lineInput_3.Top+2;
          end;
      end;
  end;
  addScriptImg.Show;
end;

constructor TParameterOptionsPanel.Create(Container: TWinControl; TextOut: TMemo);
var
  FieldWidth: integer;
begin
  ScriptOut:= TextOut;
  fieldWidth:= Container.Width-(96+24);
  lineInput_1:= TEdit.Create(nil);
  with lineInput_1 do
    begin
      Parent:= Container;
      Width:= fieldWidth;
      Left:= 96;
      Top:= 16;
      Visible:= False;
    end;
  lineInput_2:= TEdit.Create(nil);
  with lineInput_2 do
    begin
      Parent:= Container;
      Width:= fieldWidth;
      Left:= 96;
      Top:= 24+Height;
      Visible:= False;
    end;
  lineInput_3:= TEdit.Create(nil);
  with lineInput_3 do
    begin
      Parent:= Container;
      Width:= fieldWidth;
      Left:= 96;
      Top:= 32+(Height*2);
      Visible:= False;
    end;
  editBox_1:= TMemo.Create(nil);
  with editBox_1 do
    begin
      Parent:= Container;
      Width:= fieldWidth;
      Left:= 96;
      Top:= 16;
      Visible:= False;
    end;
  parameterLbl_1:= TLabel.Create(nil);
  with parameterLbl_1 do
    begin
      Parent:= Container;
      Left:= 12;
    end;
  parameterLbl_2:= TLabel.Create(nil);
  with parameterLbl_2 do
    begin
      Parent:= Container;
      Left:= 12;
    end;
  parameterLbl_3:= TLabel.Create(nil);
  with parameterLbl_3 do
    begin
      Parent:= Container;
      Left:= 12;
    end;

  //
  addScriptImg:= TImage.Create(nil);
  with addScriptImg do
    begin
      Parent:= Container;
      Width:= 59;
      Height:= 25;
      Picture.LoadFromFile('data/rightArrowBtn.png');
      Cursor:= crHandPoint;
      Top:= Container.Height-(Height+25);
      Left:= Container.Width-(Width+8);
      OnClick:= @AddScript;
    end;
end;

end.

