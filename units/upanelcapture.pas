unit uPanelCapture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLIntf, LCLType;

procedure SaveContainerAsImage(Container: TWinControl; Path: string);

implementation

procedure SaveContainerAsImage(Container: TWinControl; Path: string);
var
  MyDC: HDC;
  MyBitmap: TBitmap;
begin
  MyDC:= GetDC(Container.Handle);
  MyBitmap:= TBitmap.Create;
  try
    MyBitmap.LoadFromDevice(MyDC);
    MyBitmap.SaveToFile(concat(path,'panel',inttostr(random(10000)),'.bmp'));
  finally
    ReleaseDC(Container.Handle, MyDC);
    FreeAndNil(MyBitmap);
  end;
end;

end.

