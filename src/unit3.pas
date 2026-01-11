(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FileChecker2                                          *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls,
  Buttons, udirsync, ufilechecker;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
  private
    MergedBuffers: TFileList;
    DataBaseFiles: TFileList;
    CopyList, Dellist: TFileList;
    RenameList: TRenameList;
    info: TReportInfos;
    Procedure GenerateResults();
  public
    Procedure Init;
    Procedure GenerateResultsWith(Const aDataBaseFiles, aMergedBuffers: TFileList);
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses unit8;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Scan root folders';
  label1.caption := '';
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
End;

Procedure TForm3.FormResize(Sender: TObject);
Begin
  label1.top := Button1.Top + Button1.Height + 8;
End;

Procedure TForm3.SpeedButton1Click(Sender: TObject);
  Procedure Add(a, b: String);
  Var
    index: integer;
  Begin
    index := form8.StringGrid1.RowCount;
    form8.StringGrid1.RowCount := index + 1;
    form8.StringGrid1.Cells[0, index] := a;
    form8.StringGrid1.Cells[1, index] := b;
  End;
Var
  i: Integer;
Begin
  // Explore List (Achtung sollte gleich sein wie: CreateReportFile2
  form8.StringGrid1.BeginUpdate;
  form8.StringGrid1.RowCount := 0;
  If length(RenameList) <> 0 Then Begin
    add('Renamelist', info.RenameInfo);
    add('From', 'To');
    For i := 0 To high(RenameList) Do Begin
      add(
        RenameList[i].SourceRoot + RenameList[i].SourceFile,
        RenameList[i].DestRoot + RenameList[i].DestFile
        );
    End;
  End;
  If length(CopyList) <> 0 Then Begin
    add('Added', info.CopyInfo);
    For i := 0 To high(CopyList) Do Begin
      add(CopyList[i].Root + CopyList[i].FileName, '');
    End;
  End;
  If length(DelList) <> 0 Then Begin
    add('Deleted', Info.DelInfo);
    For i := 0 To high(DelList) Do Begin
      add(DelList[i].Root + DelList[i].FileName, '');
    End;
  End;
  form8.StringGrid1.AutoSizeColumns;
  form8.StringGrid1.EndUpdate();
  form8.ShowModal;
End;

Procedure TForm3.Button1Click(Sender: TObject);
Var
  Buffer: TFileList;

  // Übernimmt Buffer nach MergedBuffer und initialisiert die Root mit ;)
  Procedure MergeBuffers(Const aRoot: String);
  Var
    aLen, i: Integer;
  Begin
    aLen := length(MergedBuffers);
    setlength(MergedBuffers, aLen + length(buffer));
    For i := 0 To high(buffer) Do Begin
      MergedBuffers[i + aLen] := buffer[i];
      MergedBuffers[i + aLen].FileName := aRoot + buffer[i].FileName;
      MergedBuffers[i + aLen].Root := aRoot;
    End;
  End;


Var
  found, i: Integer;
  Roots: TStringArray;
Begin
  MergedBuffers := Nil;
  Buffer := Nil;
  // TODO: Prüfen ob es PendingJobs gibt, welche als Target ein gewähltes Root folder haben
  //       \-> Wenn Ja, abfragen ob vorher diese Jobs abgearbeitet werden sollen ..

  // Scan
  For i := 0 To CheckListBox1.Items.count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      Label1.Caption := 'Scan: ' + CheckListBox1.Items[i] + '...';
      Application.ProcessMessages;
      ScanDirToBuffer(RootFolders[i].RootFolder, RootFolders[i].Excludes, Buffer);
      MergeBuffers(RootFolders[i].RootFolder);
    End;
  End;
  found := length(MergedBuffers);
  Label1.Caption := format('Found %d files..', [found]);
  Application.ProcessMessages;
  If found = 0 Then Begin
    ShowMessage('Nothing found, skip now.');
    exit;
  End;
  Roots := Nil;
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      setlength(Roots, high(Roots) + 2);
      Roots[high(Roots)] := CheckListBox1.Items[i];
    End;
  End;
  DataBaseFiles := DataBaseFilesAsTFileList(Roots);
  GenerateResults();
End;

Procedure TForm3.Button2Click(Sender: TObject);

  Function ToDataSet(Const aFile: TFileEntry): TDataSet;
  Begin
    // Nur die Statischen Teile
    result.Filename := aFile.FileName;
    result.Size := aFile.FileSize;
    result.Root := afile.Root;
    result.Rating := Nil;
    result.Categories := Nil;
    result.Comment := '';
    result.Scedule := 0;
    result.Added := now;
    // Die dynamischen Teile werden immer via RefreshDynamicContent gesetzt !
    result.Rootlabel := '';
    result.lRootlabel := '';
    result.lFilename := '';
  End;

Var
  alen, i, index, j: Integer;
Begin
  // Die Dateien kommen so wie sie sind in die Datenbank
  // 1. Alle Löschungen
  For i := 0 To high(Dellist) Do Begin
    index := GetIndexOf(Dellist[i].Root, Dellist[i].FileName);
    If index = -1 Then Begin
      Raise exception.create('Error, should delete: "' + Dellist[i].FileName + '" but is not in database.');
    End;
    For j := index To high(DataBase) - 1 Do Begin
      DataBase[j] := DataBase[j + 1];
    End;
    setlength(DataBase, high(DataBase));
  End;
  // 2. Alles was Umbenannt wurde
  For i := 0 To high(RenameList) Do Begin
    index := GetIndexOf(RenameList[i].SourceRoot, RenameList[i].SourceFile);
    If index = -1 Then Begin
      index := GetIndexOf(RenameList[i].DestRoot, RenameList[i].SourceFile);
      If index = -1 Then Begin
        showmessage('Error, could not find: "' + RenameList[i].SourceFile + '" in database.');
      End;
    End;
    If index <> -1 Then Begin
      DataBase[Index].Filename := RenameList[i].DestFile;
      DataBase[Index].Root := RenameList[i].DestRoot;
      RefreshDynamicContent(index);
    End;
  End;
  // Alles was dazu gekommen ist..
  alen := length(DataBase);
  setlength(DataBase, alen + length(CopyList));
  For i := 0 To high(CopyList) Do Begin
    DataBase[i + alen] := ToDataSet(CopyList[i]);
    RefreshDynamicContent(i + alen);
  End;
  SortFileList(DataBase);
  DBChanged := true;
  showmessage('Done.');
End;

Procedure TForm3.Button5Click(Sender: TObject);
Begin
  // Export Lists
  If SaveDialog1.Execute Then Begin
    CreateReportFile2(SaveDialog1.FileName, RenameList, CopyList, DelList, Info);
  End;
End;

Procedure TForm3.Init;
Var
  i: Integer;
Begin
  MergedBuffers := Nil;
  //  fDataSet := Nil;
  RenameList := Nil;
  CopyList := Nil;
  Dellist := Nil;
  info.RenameInfo := '';
  info.CopyInfo := '';
  info.DelInfo := '';
  CheckListBox1.Clear;
  For i := 0 To high(RootFolders) Do Begin
    CheckListBox1.Items.Add(RootFolders[i].RootLabel);
    CheckListBox1.Checked[i] := DirectoryExists(RootFolders[i].RootFolder);
  End;
  Label1.Caption := '';
End;

Procedure TForm3.GenerateResultsWith(Const aDataBaseFiles,
  aMergedBuffers: TFileList);
Begin
  DataBaseFiles := aDataBaseFiles;
  MergedBuffers := aMergedBuffers;
  GenerateResults();
End;

Procedure TForm3.GenerateResults;

  Procedure CleanFromRoots(Var alist: TFileList);
  Var
    i: Integer;
  Begin
    For i := 0 To high(alist) Do Begin
      delete(alist[i].FileName, 1, length(alist[i].Root))
    End;
  End;

  Procedure CleanFromRoots(Var alist: TRenameList);
  Var
    i: Integer;
  Begin
    For i := 0 To high(alist) Do Begin
      delete(alist[i].SourceFile, 1, length(alist[i].SourceRoot));
      delete(alist[i].DestFile, 1, length(alist[i].DestRoot));
    End;
  End;
Begin
  RenameList := Nil;
  CopyList := Nil;
  Dellist := Nil;
  udirsync.SortFileList(MergedBuffers);
  udirsync.SortFileList(DataBaseFiles);
  GenerateJobLists('', '', MergedBuffers, DataBaseFiles, RenameList, CopyList, Dellist, false);
  // Die Roots müssen nun wieder "raus"
  CleanFromRoots(CopyList);
  CleanFromRoots(Dellist);
  CleanFromRoots(RenameList);
  info.CopyInfo := FileSizeToString(FileListToSize(CopyList));
  info.DelInfo := FileSizeToString(FileListToSize(DelList));
  info.RenameInfo := FileSizeToString(RenameFileListToSize(RenameList));
  Label1.Caption := Label1.Caption + format(
    LineEnding +
    '%d files of size %s need to be renamed' + LineEnding +
    '%d files of size %s need to be added' + LineEnding +
    '%d files of size %s need to be deleted' + LineEnding +
    '%d files are already in sync' + LineEnding
    , [
    length(RenameList), FileSizeToString(RenameFileListToSize(RenameList))
      , length(CopyList), FileSizeToString(FileListToSize(CopyList))
      , length(DelList), FileSizeToString(FileListToSize(DelList))
      , (length(MergedBuffers) - (length(CopyList) + length(RenameList)))
      ]);

End;

End.

