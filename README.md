### Perlin - Latest Release
#### Released 30/03/2017
-----
Lorem ipsum dolor sit amet, vivamus placerat lorem taciti dolor mi, nulla urna nec, nibh eros ut arcu aliquam a, id consequat in integer mauris sit placerat. Orci diam consectetuer ut tellus pulvinar phasellus, et quis accumsan quam dolor sed. Non mattis nisl tenetur sed quam, vel porta sit maecenas augue tempus. Porta potenti, cursus velit, fermentum sed fermentum non mauris mi sit. In dui. Non duis consectetur ad duis, risus donec tellus, mauris vitae vehicula taciti euismod praesent fermentum, mauris maecenas, sed id. In luctus, dictum lorem turpis in sit mi, vitae duis per, ipsum nulla eu justo eu dictum, erat ad.

Added the functionality to load note information, and save them to their corresponding page in the correct format.

```pascal
readln(txt,tempStr); // header text
if not (tempStr = '') the
  with notebooks[BookID].Notebook.Page[PageID] do
    NoteObject[GetNoteCount]:= TnoCGP_Header.Create(GetContainer,Bottom,tempStr);
```

Some code is intended to be updated in the future; There are multiple uses of recursion that uses defined bounds of the _TNoteObjects_ identifiers, as can be seen is this example from the _uFrmDialogPage_ unit.

```pascal
TypeList:= Sender as TListBox;
  with TypeList do
    begin
      for StrObj:= noCGP_Header to noBasic_TextBox do
        if StrNoteObjects[StrObj] = Items[ItemIndex] then
          ParameterOptionsPanel.Setup(StrObj);
      parameterPanel.Caption:= Items[ItemIndex];
    end; 
```
