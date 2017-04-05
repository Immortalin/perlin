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
      Add(StrNoteObjects[ObjectToBeCreated]);
      case ObjectToBeCreated of
        noCGP_Header:
          begin
            Add(lineInput_1.Text);
          end;
        noCGP_Subheader:
          begin
            Add(lineInput_1.Text);
          end;
        noCGP_Keyphrase:
          begin
            Add(lineInput_1.Text);
          end;
        noCGP_TextBox:
          begin
            Add(editBox_1.Text);
          end;
        noBasic_Header:
          begin
            Add(lineInput_1.Text);
          end;
        noBasic_Image:
          begin
            Add(lineInput_1.Text);
            Add(lineInput_2.Text);
            Add(lineInput_3.Text);
          end;
        noBasic_TextBox:
          begin
            Add(editBox_1.Text);
          end;
      end;
      Add('↵');
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
  // hide all objects
  lineInput_1.Hide;
  lineInput_2.Hide;
  lineInput_3.Hide;
  parameterLbl_1.Hide;
  parameterLbl_2.Hide;
  parameterLbl_3.Hide;
  editBox_1.Hide;
  addScriptImg.Hide;
  // show according to noteobject
  case NoteObject of
    noCGP_Header:
      begin
        lineInput_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Header:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noCGP_Subheader:
      begin
        lineInput_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Subheader:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noCGP_Keyphrase:
      begin
        lineInput_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Keyphrase:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noCGP_TextBox:
      begin
        editBox_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Text:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noBasic_Header:
      begin
        lineInput_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Header:';
            Top:= lineInput_1.Top+2;
          end;
      end;
    noBasic_Image:
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
    noBasic_TextBox:
      begin
        editBox_1.Show;
        with parameterLbl_1 do
          begin
            Show;
            Caption:= 'Text:';
            Top:= lineInput_1.Top+2;
          end;
      end;
  end;
  addScriptImg.Show;
end;

constructor TParameterOptionsPanel.Create(Container: TWinControl; TextOut: TMemo);
begin
  ScriptOut:= TextOut;
  lineInput_1:= TEdit.Create(nil);
  with lineInput_1 do
    begin
      Parent:= Container;
      Width:= 160;
      Left:= 96;
      Top:= 16;
      Visible:= False;
    end;
  lineInput_2:= TEdit.Create(nil);
  with lineInput_2 do
    begin
      Parent:= Container;
      Width:= 160;
      Left:= 96;
      Top:= 24+Height;
      Visible:= False;
    end;
  lineInput_3:= TEdit.Create(nil);
  with lineInput_3 do
    begin
      Parent:= Container;
      Width:= 160;
      Left:= 96;
      Top:= 32+(Height*2);
      Visible:= False;
    end;
  editBox_1:= TMemo.Create(nil);
  with editBox_1 do
    begin
      Parent:= Container;
      Width:= 160;
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

