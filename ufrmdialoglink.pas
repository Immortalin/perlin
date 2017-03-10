unit uFrmDialogLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmDialogLink }

  TfrmDialogLink = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDialogLink: TfrmDialogLink;

implementation

{$R *.lfm}

{ TfrmDialogLink }

procedure TfrmDialogLink.FormCreate(Sender: TObject);
begin
end;

end.

