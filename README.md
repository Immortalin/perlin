### What is Perlin?

Perlin is a desktop-application that can view and compile notes in a modular form.

![Screenshot of Perlin](https://github.com/ldhmachin/Perlin/blob/master/screenshots/1.PNG)
----
### Amendments to be added

* highlight function
* keyphrase note object in _CGP style_
* search functionality

----
### Perlin - Latest Release
#### Released 30/03/2017

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

This is quite a nuisance as when note objects are added, all instances of these loops must be amended. 
