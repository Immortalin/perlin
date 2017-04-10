unit uLoadNotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uNoteObjects, uGUI, uNotebook, uFrmDash;

procedure LoadNotebooks(loadContent: boolean);
procedure LoadPages(BookID: integer);
procedure LoadNotes(BookID, PageID: integer);
procedure LoadAllPreviews(BookID: integer);
procedure LoadLinks;

implementation

uses
  Dialogs;

procedure LoadLinks;
var
  txt: textfile;
  tempName, tempURL: string;
begin
  AssignFile(txt,'notes/links.perlin');
  Reset(txt);
  while not EOF(txt) do
    begin
      readln(txt,tempName);
      readln(txt,tempURL);
      sidebar.Buttons.AddLink(tempName,tempURL);
    end;
  closefile(txt);
end;

procedure LoadNotes(BookID, PageID: integer);
var
  txt: textfile;
  tempStr: string;
  tempAnsiStr: ansistring;
  tempInt1,tempInt2: integer;
  pageName, pageDesc: string;
  tempNoteObj: TNoteObjects;
begin
  AssignFile(txt,concat('notes/',notebooks[BookID].Notebook.GetName,'_',notebooks[BookID].Notebook.Page[PageID].GetName,'.perlin'));
  Reset(txt);
  readln(txt,pageName);
  readln(txt,pageDesc);
  while not EOF(txt) do
    begin
      readln(txt,tempStr); // note object as string (name)
      tempNoteObj:= StrToNote(tempStr);
      notebooks[BookID].Notebook.Page[PageID].IncNotes;
      case tempNoteObj of
        noCGP_Header:
          begin
            readln(txt,tempStr); // header text
            if not (tempStr = '') then
              with notebooks[BookID].Notebook.Page[PageID] do
                NoteObject[GetNoteCount]:= TnoCGP_Header.Create(GetContainer,Bottom,tempStr);
          end;
        noCGP_Subheader:
          begin
            readln(txt,tempStr); // subheader text
            if not (tempStr = '') then
              with notebooks[BookID].Notebook.Page[PageID] do
                NoteObject[GetNoteCount]:= TnoCGP_Subheader.Create(GetContainer,Bottom,tempStr);
          end;
        noCGP_Keyphrase:
          begin
            readln(txt,tempStr); // subheader text
            if not (tempStr = '') then
              with notebooks[BookID].Notebook.Page[PageID] do
                NoteObject[GetNoteCount]:= TnoCGP_Keyphrase.Create(GetContainer,Bottom,tempStr,clWashedPink);
          end;
        noCGP_TextBox:
          begin
            readln(txt,tempStr); // text
      //   with Book.Page[PageID] do
       //     NoteObject[GetNoteCount]:= TnoCGP_Text
          end;
        noBasic_Header:
          begin
            readln(txt,tempStr); // header text
          end;
        noBasic_Image:
          begin
            readln(txt,tempStr); // image path
            readln(txt,tempInt1); // image width
            readln(txt,tempInt2); // image height
            with notebooks[BookID].Notebook.Page[PageID] do
              NoteObject[GetNoteCount]:= TnoBasic_Image.Create(GetContainer,Bottom,tempStr, tempInt1, tempInt2);
          end;
        noBasic_TextBox:
          begin
            readln(txt,tempAnsiStr); // text
            with notebooks[BookID].Notebook.Page[PageID] do
              NoteObject[GetNoteCount]:= TnoBasic_Textbox.Create(GetContainer,Bottom,tempAnsiStr);
          end;
      end;
      readln(txt,tempStr); // linebreak
      with notebooks[BookID].Notebook.Page[PageID] do
        if Bottom > Panel.Height then
          Panel.Height:= Bottom + 8;
      with notebooks[BookID].Notebook.Panel do
        if notebooks[BookID].Notebook.Page[PageID].Bottom > Height then
          Height:= notebooks[BookID].Notebook.Page[PageID].Bottom + 8;
    end;
  CloseFile(txt);
end;

procedure LoadAllPreviews(BookID: integer);
var
  filename, tempName, tempDesc, tempStr: string;
  txt: TextFile;
begin
  filename:= concat('notes/',lowercase(notebooks[BookID].Name),'.perlin');
  AssignFile(txt,filename);
  Reset(txt);
  while not EOF(txt) do
    begin
      Readln(txt,tempName);
      Readln(txt,tempDesc);
      with notebooks[BookID] do
        PageBar.AddPage(Name,Tag,tempName,tempDesc);
    end;
  CloseFile(txt);
end;

procedure LoadPages(BookID: integer);
var
  filename, tempName, tempDesc, tempStr: string;
  txt: TextFile;
begin
  filename:= concat('notes/',lowercase(notebooks[BookID].Name),'.perlin');
  AssignFile(txt,filename);
  Reset(txt);
  while not EOF(txt) do
    begin
      Readln(txt,tempName);
      Readln(txt,tempDesc);
      with notebooks[BookID] do
        begin
          Notebook.AddPage(tempName,tempDesc);
          PageBar.AddPage(Name,Tag,tempName,tempDesc);
          LoadNotes(BookID,Notebook.GetPageCount);
        end;
    end;
  CloseFile(txt);
end;

procedure LoadNotebooks(loadContent: boolean);
var
  tempStr: string;
  tempTagClr: TTagColor;
  txt: textfile;
begin
  AssignFile(txt,'notes/notebooks.perlin');
  Reset(txt);
  while not EOF(txt) do
    begin
      readln(txt,tempStr); // notebook name
      readln(txt,tempTagClr); // notebook tag color
      inc(notebookCount);
      with sidebar do
        begin
          Buttons.AddNotebook(notebookPanel, tempStr, tempTagClr);
          if loadContent then
            LoadPages(Buttons.GetNotebookCount)
          else
            LoadAllPreviews(Buttons.GetNotebookCount);
        end;
    end;
  CloseFile(txt);
end;

end.

