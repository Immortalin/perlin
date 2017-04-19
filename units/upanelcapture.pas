unit uPanelCapture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

procedure SaveContainerAsImage(Container: TWinControl; Path: string);

implementation

procedure SaveContainerAsImage(Container: TWinControl; Path: string);
begin
  MyDC:= GetDC(notebookPanel.Handle);
  MyBitmap:= TBitmap.Create;
  try
    MyBitmap.LoadFromDevice(MyDC);
    MyBitmap.SaveToFile(concat('panel',inttostr(random(10000)),'.bmp'));
  finally
    ReleaseDC(notebookPanel.Handle, MyDC);
    FreeAndNil(MyBitmap);
  end;
end

end.

