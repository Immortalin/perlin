unit uPageFromTXT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uNoteObjects;

function TextToPerlinPredictive(path: string; style: TNoteStyle): TStrings;

implementation

function TextToPerlinPredictive(path: string; style: TNoteStyle): TStrings;
var
  TextStrings, PerlinStrings: TStrings;
  tempStr: ansistring;
  txt: Textfile;
  Header, Subheader, Textbox, Image, Keyphrase: TNoteObjects;
begin
  TextStrings:= TStrings.Create;
  PerlinStrings:= TStrings.Create;
  AssignFile(txt,path);
  Reset(txt);
  while not EOF(txt) do
    begin
      readln(txt,tempStr);
      TextStrings.Add(tempStr);
    end;
  closefile(txt);
  { Given format:
  HEADER

  SUBHEADER
  TEXT
  TEXT
  TEXT

  SUBHEADER
  TEXT

  IMAGE

  SUBHEADER
  TEXT
  }
  case style of
    nsCGP:;
    nsBasic:;
  end;
end;

end.

