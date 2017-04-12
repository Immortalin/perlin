unit uLoadNotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, uNoteObjects, uGUI, uNotebook, uFrmDash;

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
  line: ansistring;
  tempPath: string;
  tempWidth,tempHeight: integer;
  pageName, pageDesc, pageStyle: string;
  lineObject: TNoteObjects;
begin
  AssignFile(txt,concat('notes/',notebooks[BookID].Notebook.GetName,'_',notebooks[BookID].Notebook.Page[PageID].GetName,'.perlin'));
  Reset(txt);
  readln(txt,pageName);
  readln(txt,pageDesc);
  readln(txt,pageStyle);
  while not EOF(txt) do
    begin
      notebooks[BookID].Notebook.Page[PageID].IncNotes;
      readln(txt,line); // load the line
      // check subheader then header
      if LeftStr(line,length(SUBHEADER_PREFIX)) = SUBHEADER_PREFIX then
        lineObject:= noSubheader
      else if LeftStr(line,length(HEADER_PREFIX)) = HEADER_PREFIX then
        lineObject:= noHeader
      else if LeftStr(line,length(KEYPHRASE_PREFIX)) = KEYPHRASE_PREFIX then
        lineObject:= noKeyphrase
      else if LeftStr(line,length(IMAGE_PREFIX)) = IMAGE_PREFIX then
        lineObject:= noImage
      else
        lineObject:= noText;
      case lineObject of
        noHeader:
          begin
            line:= RightStr(line,length(line)-length(HEADER_PREFIX));
            with notebooks[BookID].Notebook.Page[PageID] do
              case StrToStyle(pageStyle) of
                nsCGP: NoteObject[GetNoteCount]:= TnoCGP_Header.Create(GetContainer,Bottom,line);
                nsBasic: NoteObject[GetNoteCount]:= TnoCGP_Header.Create(GetContainer,Bottom,line);
              end;
          end;
        noSubheader:
          begin
            line:= RightStr(line,length(line)-length(SUBHEADER_PREFIX));
            with notebooks[BookID].Notebook.Page[PageID] do
              case StrToStyle(pageStyle) of
                nsCGP: NoteObject[GetNoteCount]:= TnoCGP_Subheader.Create(GetContainer,Bottom,line);
                nsBasic: NoteObject[GetNoteCount]:= TnoCGP_Subheader.Create(GetContainer,Bottom,line);
              end;

          end;
        noKeyphrase:
          begin
            line:= RightStr(line,length(line)-length(KEYPHRASE_PREFIX));
            with notebooks[BookID].Notebook.Page[PageID] do
              case StrToStyle(pageStyle) of
                nsCGP: NoteObject[GetNoteCount]:= TnoCGP_Keyphrase.Create(GetContainer,Bottom,line,clWashedPink);
                nsBasic: NoteObject[GetNoteCount]:= TnoCGP_Keyphrase.Create(GetContainer,Bottom,line,clWashedPink);
              end;

          end;
        noImage:
          begin
            line:= RightStr(line,length(line)-length(IMAGE_PREFIX));
            tempPath:= LeftStr(line,Pos('[',line)-1);
            tempWidth:= strtoint(MidStr(line,length(tempPath)+2,Pos(',',line)-(length(tempPath)+2)));
            tempHeight:= strtoint(MidStr(line,Pos(',',line)+1,length(line)-Pos(',',line)-1));
            with notebooks[BookID].Notebook.Page[PageID] do
              NoteObject[GetNoteCount]:= TnoImage.Create(GetContainer,Bottom,tempPath,tempWidth,tempHeight);
          end;
        noText:
          begin
            with notebooks[BookID].Notebook.Page[PageID] do
              NoteObject[GetNoteCount]:= TnoBasic_Textbox.Create(GetContainer,Bottom,line);
          end;
      end;



      {case tempNoteObj of
        noHeader://CGP
          begin
            readln(txt,tempStr); // header text
            if not (tempStr = '') then
              with notebooks[BookID].Notebook.Page[PageID] do
                NoteObject[GetNoteCount]:= TnoCGP_Header.Create(GetContainer,Bottom,tempStr);
          end;
        noSubheader: //CGP
          begin
            readln(txt,tempStr); // subheader text
            if not (tempStr = '') then
              with notebooks[BookID].Notebook.Page[PageID] do
                NoteObject[GetNoteCount]:= TnoCGP_Subheader.Create(GetContainer,Bottom,tempStr);
          end;
        noKeyphrase:
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
      end;     }
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

