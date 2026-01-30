(******************************************************************************)
(* FileChecker2                                                    26.12.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : Filechecker2 is a tool to keep track of / move / work with   *)
(*               files stored at multiple locations (even if they are offline)*)
(*                                                                            *)
(* Description : <Module_description>                                         *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: - Dateien Pfade umbenennen, welche noch in Pending Jobs sind *)
(*                 werden nicht aktualisiert -> Die Jobs werden ungültig      *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
(*  Silk icon set 1.3 used                                                    *)
(*  ----------------------                                                    *)
(*  Mark James                                                                *)
(*   https://peacocksoftware.com/silk                                         *)
(******************************************************************************)
(*  This work is licensed under a                                             *)
(*  Creative Commons Attribution 2.5 License.                                 *)
(*  [ http://creativecommons.org/licenses/by/2.5/ ]                           *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ComCtrls, Menus, ExtCtrls, lNetComponents, IniFiles
  , udirsync
  , ufilechecker
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    ListBox1: TListBox;
    LTCPComponent1: TLTCPComponent;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    Procedure ComboBox1Change(Sender: TObject);
    Procedure ComboBox1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
      );
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure SpeedButton3Click(Sender: TObject);
    Procedure SpeedButton4Click(Sender: TObject);
    Procedure SpeedButton5Click(Sender: TObject);
    Procedure StatusBar1Click(Sender: TObject);
    Procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      Const Rect: TRect);
    Procedure StatusBar1Hint(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    ResultsAsFolders: Boolean;
    FormShowOnce: Boolean;
    ReportPendingJobsDoable: Boolean;
    Procedure LoadQueryHistory;
    Procedure StoreQueryHistory;

    Procedure QueryPendingJobs;
    Function CursorToPanelIndex: integer;
  public

    Function SelectionToIndexes(): TIntegers; // Gibt eine Liste der Indexe der Dateien in der Datenbank aus, welche Aktuell angewählt sind

    Procedure UpdateSelectedState;
    Procedure UpdatePendingJobs;
    Procedure UpdateConnectedRoots;
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses LCLType, math, lclintf
  , unit2 // Settings
  , unit3 // Scan RootFolder Dialog
  , unit4 // Move Files Dialog
  , unit5 // Detail Info Dialog
  , unit6 // Job Execute Dialog
  // , unit7 // Show pending Jobs Details
  // , unit8 // Detail Scan Rootfolder Dialog
  , unit9 // Sync with server
  // , unit10 // Select remote database dialog
  , unit11 // Root Info dialog
  , unit12 // category editor
  , ucopycomandercontroller
  ;

Const
  // see TForm1.StatusBar1Hint for more details ;)
  PanelIndexDataSetInfo = 0;
  PanelIndexSumFileSize = 1;
  PanelIndexPendingJobInfo = 2;
  PanelIndexRootsInfo = 3;
  PanelIndexSearchResultInfo = 4;
  PanelIndexOpenTmpFolder = 5;

  ImageIndexOptions = 0;
  ImageIndexSearch = 1;
  ImageIndexExploreTo = 2;
  ImageIndexSkript = 3;
  ImageIndexConnections = 4;
  ImageIndexServerSync = 5;
  ImageIndexDoSkripts = 6;
  ImageIndexFolderSearch = 7;
  ImageIndexMove = 8;
  ImageIndexRefres = 9;
  ImageIndexFolderAdd = 10;
  ImageIndexFolderSub = 11;
  ImageIndexStop = 12;
  ImageIndexRainbow = 13;
  ImageIndexFiles = 14;
  ImageIndexFolder = 15;
  ImageIndexServerUpload = 16;
  ImageIndexServerDownload = 17;
  ImageIndexFileSum = 18;

  { TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  Caption := 'Filechecker 2 ver. 0.01, by Corpsman, www.Corpsman.de';
  ResultsAsFolders := false;
  Categories := TStringList.Create;
  IniFile := TIniFile.Create(GetAppConfigFile(false, true));
  Inifile.CacheUpdates := true;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  ComboBox1.text := '';
  ReportPendingJobsDoable := true;

  // Do this via code, this is more save against changings of the consts ;)
  StatusBar1.Panels[PanelIndexDataSetInfo].Width := 150;
  StatusBar1.Panels[PanelIndexSumFileSize].Width := 100;
  StatusBar1.Panels[PanelIndexPendingJobInfo].Width := 150;
  StatusBar1.Panels[PanelIndexRootsInfo].Width := 150;
  StatusBar1.Panels[PanelIndexSearchResultInfo].Width := 32;
  StatusBar1.Panels[PanelIndexOpenTmpFolder].Width := 32;
  For i := 0 To StatusBar1.Panels.Count - 1 Do
    StatusBar1.Panels[i].Style := psOwnerDraw;

  LoadSettings;
  LoadQueryHistory;
  LoadPendingJobs;
  LoadCategories;

  LoadDataBase;
  UpdateSelectedState;
  UpdatePendingJobs;
  UpdateConnectedRoots;
  CopyCommanderController_Init(LTCPComponent1);
  FormShowOnce := true;
  //  - Testen: umbenennen einer Datei die Offline ist und dann den Job Durchführen
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  If FormShowOnce Then Begin
    FormShowOnce := false;
    ComboBox1.SetFocus;
    QueryPendingJobs;
  End;
End;

Procedure TForm1.ListBox1DblClick(Sender: TObject);
Begin
  // Detail Infos zum Datensatz aufrufen
  If ListBox1.ItemIndex = -1 Then exit;
  If ListBox1.items[ListBox1.ItemIndex] = SearchInfo Then exit;
  If Selected[ListBox1.ItemIndex] < 0 Then exit; // Verzeichnisse haben keine Datensätze
  form5.Init(Selected[ListBox1.ItemIndex]);
  If form5.ShowModal = mrOK Then Begin
    form5.LCLToDataSet(Selected[ListBox1.ItemIndex]);
  End;
End;

Procedure TForm1.ListBox1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  OldSelectedStart, OldSelectedEnd, i: integer;
Begin
  //  If key = VK_F6 Then Begin // Debug Löschen
  //    key := 0;
  //    MenuItem1Click(Nil);
  //    exit;
  //  End;
  If ListBox1.Count = 0 Then exit;
  If ListBox1.items[ListBox1.ItemIndex] = SearchInfo Then exit;
  // Backup eines ggf selected state
  OldSelectedStart := -1;
  OldSelectedEnd := -1;
  If ssShift In Shift Then Begin
    OldSelectedStart := ListBox1.Items.Count;
    OldSelectedEnd := -1;
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      If ListBox1.Selected[i] Then Begin
        OldSelectedStart := min(i, OldSelectedStart);
        OldSelectedEnd := max(i, OldSelectedEnd);
      End;
    End;
    // Der Index ist außerhalb der Selection
    If (ListBox1.ItemIndex < OldSelectedStart) Or
      (ListBox1.ItemIndex > OldSelectedEnd) Then Begin
      OldSelectedStart := -1;
      OldSelectedEnd := -1;
    End;
  End;
  // F2 = Rename
  If key = VK_F2 Then MenuItem3Click(Nil);

  // F6 = Move
  If key = VK_F6 Then MenuItem1Click(Nil);

  // F8 = Delete
  If key = VK_F8 Then MenuItem4Click(Nil);

  If key = VK_DOWN Then Begin
    ListBox1.ItemIndex := min(ListBox1.ItemIndex + 1, ListBox1.Count - 1);
  End;
  If key = VK_UP Then Begin
    ListBox1.ItemIndex := max(0, ListBox1.ItemIndex - 1);
  End;
  If key = VK_HOME Then Begin
    ListBox1.ItemIndex := 0;
  End;
  If key = VK_END Then Begin
    ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  End;
  If key = VK_PRIOR Then Begin
    ListBox1.ItemIndex := max(ListBox1.ItemIndex - (ListBox1.Height Div ListBox1.ItemHeight - 1), 0);
  End;
  If key = VK_NEXT Then Begin
    ListBox1.ItemIndex := min(ListBox1.ItemIndex + (ListBox1.Height Div ListBox1.ItemHeight - 1), ListBox1.Count - 1);
  End;
  // Restore eines ggf Selected State
  If (ssShift In Shift) And (OldSelectedStart <> -1) Then Begin
    OldSelectedStart := min(ListBox1.ItemIndex, OldSelectedStart);
    OldSelectedEnd := max(ListBox1.ItemIndex, OldSelectedEnd);
    For i := OldSelectedStart To OldSelectedEnd Do Begin
      ListBox1.Selected[i] := true;
    End;
  End;
  If (ssCtrl In Shift) And (key = VK_A) Then Begin
    ListBox1.SelectAll;
  End;
  key := 0;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  // Move Files to
  If ListBox1.ItemIndex = -1 Then exit;
  If ListBox1.items[ListBox1.ItemIndex] = SearchInfo Then exit;
  If Selected[ListBox1.ItemIndex] < 0 Then Begin
    showmessage('Move Folder not implemented yet.');
    exit;
  End;
  Form4.Init(SelectionToIndexes());
  form4.ShowModal;
  ComboBox1Change(Nil); // Die Suchergebnisse müssen ggf angepasst werden
  UpdatePendingJobs; // Falls Jobs entstanden sind muss das auch angezeigt werden
  // Wenn aus all den Aktionen Jobs entstanden sind, dann gleich den Job Dialog starten
  If PendingJobsDoable <> 0 Then SpeedButton5.Click;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  fn, s, rootLabel, folder: String;
Begin
  // Explore to
  If ListBox1.ItemIndex = -1 Then exit;
  If ListBox1.items[ListBox1.ItemIndex] = SearchInfo Then exit;
  If Selected[ListBox1.ItemIndex] < 0 Then Begin
    // Explore to a selected folder
    s := ListBox1.items[ListBox1.ItemIndex];
    rootLabel := trim(copy(s, 1, pos(':', s) - 1));
    folder := trim(copy(s, pos(':', s) + 1, length(s)));
    folder := RootLabelToRootFolder(rootLabel) + folder;
  End
  Else Begin
    // Explore to a selected file folder
    fn := DataBase[Selected[ListBox1.ItemIndex]].Root + DataBase[Selected[ListBox1.ItemIndex]].Filename;
    folder := ExtractFileDir(fn);
  End;
  openurl(folder);
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Var
  Searcher, default, s, NewName: String;
  i: Integer;
Begin
  // File Rename
  If ListBox1.ItemIndex = -1 Then exit;
  If ListBox1.items[ListBox1.ItemIndex] = SearchInfo Then exit;
  If Selected[ListBox1.ItemIndex] < 0 Then Begin
    showmessage('Folder Rename not implemented yet.');
    exit;
  End;
  default := ExtractFileName(DataBase[Selected[ListBox1.ItemIndex]].Filename);
  s := InputBox('Rename', 'File', default);
  // Bei Abbruch wird s zum Default, leerstrings sind nicht erlaubt !
  If (s <> '') And (s <> default) Then Begin
    newname := ExtractFilePath(DataBase[Selected[ListBox1.ItemIndex]].Filename);
    newname := newname + s;
    Searcher := DataBase[Selected[ListBox1.ItemIndex]].RootLabel + ': ' + NewName;
    TryDoMove(Selected[ListBox1.ItemIndex], DataBase[Selected[ListBox1.ItemIndex]].Root, NewName);
    SortFileList(DataBase);
    // Ansicht neu Laden
    ComboBox1Change(Nil);
    // Neuen Datensatz auswählen
    For i := 0 To ListBox1.Items.count - 1 Do Begin
      If ListBox1.Items[i] = Searcher Then Begin
        ListBox1.ItemIndex := i;
        break;
      End;
    End;
    UpdatePendingJobs;
  End;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Var
  i, j: Integer;
  de, e, fn: String;
  DelList: TIntegers;
  fs: Int64;
Begin
  // Delete File
  If ListBox1.ItemIndex = -1 Then exit;
  If ListBox1.items[ListBox1.ItemIndex] = SearchInfo Then exit;
  If Selected[ListBox1.ItemIndex] < 0 Then Begin
    showmessage('Delete Folder not implemented yet.');
    exit;
  End;
  e := '';
  de := '';
  DelList := SelectionToIndexes();
  fs := 0;
  // 1. Die Selected Indexe müssen absteigend sein, sonst kommt es beim Löschen zu komischen dingen
  DelList.sort(true);
  For i := 0 To high(DelList) Do Begin
    fn := DataBase[DelList[i]].Root + DataBase[DelList[i]].Filename;
    If FileExists(fn) Then Begin
      If DeleteFile(fn) Then Begin
        // Löschen aus der Datenbank
        fs := fs + DataBase[DelList[i]].Size;
        For j := DelList[i] To high(DataBase) - 1 Do Begin
          DataBase[j] := DataBase[j + 1];
        End;
        setlength(DataBase, high(DataBase));
      End
      Else Begin
        de := de + fn + LineEnding;
      End;
    End
    Else Begin
      e := e + fn + LineEnding;
    End;
  End;
  ComboBox1Change(Nil); // ggf neu Triggern der Suche
  UpdateSelectedState(); // Auf jeden Fall die DB info aktualisieren ;)
  If e <> '' Then Begin
    showmessage('Error, actually only "Online" files can be deleted, could not delete the following files:' + LineEnding + e);
  End;
  If de <> '' Then Begin
    showmessage('Error, was unable to delete the following files:' + LineEnding + de);
  End;
  If (e = '') And (de = '') Then showmessage('Deleted ' + FileSizeToString(fs));
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Categories
  If ListBox1.ItemIndex = -1 Then exit;
  If ListBox1.items[ListBox1.ItemIndex] = SearchInfo Then exit;
  If Selected[ListBox1.ItemIndex] < 0 Then Begin
    showmessage('Categories not available in folder mode');
    exit;
  End;
  form12.Init(Selected[ListBox1.ItemIndex]);
  form12.ShowModal;

End;

Procedure TForm1.ComboBox1Change(Sender: TObject);

Var
  Root: String;
  filename, // Der lowercase Filename
  filename2, // der Lowercase filename mit " " -> "_"
  LastFolder: String;

  Procedure AddEntryToList(index: integer);
  Var
    folder, FileFolder: String;
  Begin
    If ResultsAsFolders Then Begin
      SelectedFileSize := SelectedFileSize + DataBase[index].Size;
      // Abschneiden des Dateinamens
      FileFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(DataBase[index].Filename));
      // Abschneiden des 1. Verzeichnis Namens (Staffel_**)
      FileFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(FileFolder));
      If trim(FileFolder) = '' Then Begin
        FileFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(DataBase[index].Filename));
      End;
      If trim(FileFolder) = '' Then Begin
        // Dateien die direkt im Root folder liegen ..
        exit;
      End;
      folder := DataBase[index].RootLabel + ': ' + FileFolder;
      (*
       * Zum Glück sind die Datensätze sortiert, eine pos Prüfung über die Listbox
       * wäre hier rechenzeit technisch tödlich..
       *)
      If folder = LastFolder Then exit;
      LastFolder := Folder;
      // If pos(folder, listbox1.Items.Text) <> 0 Then exit;
      Selected[SelectedCnt] := -1;
      inc(SelectedCnt);
      ListBox1.Items.Add(folder);
    End
    Else Begin
      Selected[SelectedCnt] := index;
      SelectedFileSize := SelectedFileSize + DataBase[index].Size;
      inc(SelectedCnt);
      ListBox1.Items.Add(DataBase[index].RootLabel + ': ' + DataBase[index].Filename);
    End;
  End;

  Procedure SearchForFiles;
  Var
    i: Integer;
  Begin
    For i := 0 To high(DataBase) Do Begin
      // Falsche Root
      If Root <> '' Then Begin
        If pos(Root, DataBase[i].lRootlabel) = 0 Then Continue;
      End;
      // Filename Matcht nicht
      If (filename <> '*') And (pos(filename2, DataBase[i].lFilename) = 0) And (pos(filename, DataBase[i].lFilename) = 0) Then Continue;

      // TODO: weitere Checks ?
      AddEntryToList(i);
    End;
  End;

  Procedure SearchInComments;
  Var
    i: Integer;
  Begin
    For i := 0 To high(DataBase) Do Begin
      // Kommentar Matcht nicht
      If (filename <> '*') And (pos(filename, lowercase(DataBase[i].Comment)) = 0) Then Continue;

      // TODO: weitere Checks ?
      AddEntryToList(i);
    End;
  End;

  Procedure SearchInCategories;
  Var
    i, j: Integer;
    res: Boolean;
  Begin
    For i := 0 To high(DataBase) Do Begin
      // Kommentar Matcht nicht
      If Not assigned(DataBase[i].Categories) Then Continue;
      res := false;
      For j := 0 To high(DataBase[i].Categories) Do Begin
        If pos(filename, lowercase(DataBase[i].Categories[j])) <> 0 Then res := true;
      End;
      If Not res Then Continue;
      // TODO: weitere Checks ?
      AddEntryToList(i);
    End;
  End;

Var
  s: String;
Begin
  s := lowercase(trim(ComboBox1.Text));
  ListBox1.Items.BeginUpdate;
  ListBox1.Clear;
  SelectedCnt := 0;
  SelectedFileSize := 0;
  If trim(s) = '' Then Begin
    UpdateSelectedState;
    ListBox1.Items.EndUpdate;
    exit;
  End;
  // Wir durchsuchen die Datenbank ;)
  If length(Selected) <> length(DataBase) Then Begin
    setlength(Selected, length(DataBase));
  End;
  (*
   * Der User kann ganz "normal" suchen, oder er stellt eine Anfrage der form
   * EBNF: {<root_label>":"}<file>
   * <file> = * ist die Wildcard für "alles"
   *)
  Root := '';
  filename := s;
  If pos(':', filename) <> 0 Then Begin
    Root := copy(filename, 1, pos(':', filename) - 1);
    delete(filename, 1, length(Root) + 1);
  End;
  filename2 := StringReplace(filename, ' ', '_', [rfReplaceAll]);

  If (filename <> '*') And (length(filename) < SearchCharBorder) Then Begin
    UpdateSelectedState;
    ListBox1.Items.add(SearchInfo);
    ListBox1.Items.EndUpdate;
    exit;
  End;
  LastFolder := '';

  Case Root Of
    // Sonder Such Sachen
    'comment': SearchInComments;
    'cat': SearchInCategories;
  Else Begin
      // das Reguläre suchen über Dateinamen
      // Root = '', oder eben eine spezielle Root
      SearchForFiles;
    End;
  End;

  ListBox1.Items.EndUpdate;
  If IniFile.ReadBool('Search', 'AlwaysJumpToLast', false) Then Begin
    If ListBox1.Count <> 0 Then ListBox1.ItemIndex := ListBox1.Count - 1;
  End;
  UpdateSelectedState;
End;

Procedure TForm1.ComboBox1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_RETURN Then Begin
    key := 0;
    If Application.MessageBox(pchar('Add "' + ComboBox1.Text + '" to query history ?'), 'Question', MB_ICONQUESTION Or MB_YESNO) = ID_YES Then Begin
      ComboBox1.Items.Add(ComboBox1.Text);
      StoreQueryHistory;
    End;
  End;
  If key = VK_ESCAPE Then Begin
    Close;
  End;
  If (key = VK_DOWN) And (ComboBox1.Text <> '') Then Begin
    key := VK_TAB;
  End;
  If key = VK_TAB Then Begin
    If ListBox1.Items.Count <> 0 Then Begin
      ListBox1.SetFocus;
      ListBox1.ItemIndex := 0;
    End;
    key := 0;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Timer1.Enabled := false;
  StoreSettings; // Wegen den Disk informationen müssen die Settings nun immer beim Beenden gespeichert werden ..
  StoreDataBase;
  StoreQueryHistory;
  StorePendingJobs;
  StoreCategories;
  CopyCommanderController_Clear();
  Categories.free;
  Categories := Nil;
  IniFile.Free;
  IniFile := Nil;
End;

Procedure TForm1.SpeedButton2Click(Sender: TObject);
Begin
  form2.SettingsToLCL();
  If form2.ShowModal = mrOK Then Begin
    form2.LCLToSettings;
    StoreSettings();
    LoadQueryHistory();
  End;
  UpdateConnectedRoots;
End;

Procedure TForm1.SpeedButton3Click(Sender: TObject);
Begin
  // Scan Root Folders
  form3.Init;
  form3.ShowModal;
  // ggf. die Suchergebnisse neu aufbauen
  If ComboBox1.text <> '' Then Begin
    ComboBox1.OnChange(ComboBox1);
  End;
  UpdateSelectedState;
End;

Procedure TForm1.SpeedButton4Click(Sender: TObject);
Begin
  // Sync Database mit Server
  // TODO: DB integritätscheck machen -> sind tatsächlich alle Dateien in der DB da wo die DB behauptet ? das geht durch neu einlesen am einfachsten ;)
  form9.ShowModal;
  UpdateSelectedState;
End;

Procedure TForm1.SpeedButton5Click(Sender: TObject);
Begin
  // Execute Pending Jobs
  If form6.visible Then exit;
  form6.Init;
  form6.ShowModal;
  UpdatePendingJobs; // Danach dann ..
End;

Procedure TForm1.StatusBar1Click(Sender: TObject);
Begin
  Case CursorToPanelIndex() Of
    PanelIndexSearchResultInfo: Begin
        ResultsAsFolders := Not ResultsAsFolders;
        ComboBox1Change(Nil);
        StatusBar1.Invalidate;
      End;
    PanelIndexOpenTmpFolder: Begin
        OpenURL(JobTempFolder);
      End;
    PanelIndexRootsInfo: Begin
        form11.Init;
        form11.ShowModal;
      End;
  End;
End;

Procedure TForm1.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; Const Rect: TRect);
Var
  index, i: integer;
  s: String;
Begin
  index := -1;
  For i := 0 To StatusBar1.Panels.Count - 1 Do Begin
    If panel = StatusBar1.Panels[i] Then Begin
      Case i Of
        PanelIndexDataSetInfo: index := ImageIndexExploreTo;
        PanelIndexSumFileSize: index := ImageIndexFileSum;
        PanelIndexPendingJobInfo: index := ImageIndexSkript;
        PanelIndexRootsInfo: index := ImageIndexConnections;
        PanelIndexSearchResultInfo: Begin
            If ResultsAsFolders Then Begin
              index := ImageIndexFolder;
            End
            Else Begin
              index := ImageIndexFiles;
            End;
          End;
        PanelIndexOpenTmpFolder: Begin
            index := ImageIndexRainbow;
          End;
      End;
      break;
    End;
  End;
  ImageList1.Draw(StatusBar1.Canvas, rect.Left + 8, rect.Top, index);
  s := StatusBar1.Panels[i].Text;
  StatusBar1.Canvas.Brush.Style := bsClear;
  StatusBar1.Canvas.TextOut(rect.Left + 8 + 16 + 8, (rect.Top + rect.Bottom - StatusBar1.Canvas.TextHeight(s)) Div 2, s);
End;

Procedure TForm1.StatusBar1Hint(Sender: TObject);
Begin
  // Setzen des Passenden Hints als pseudo EBNF ;)
  StatusBar1.Hint := '';
  Case CursorToPanelIndex() Of
    PanelIndexDataSetInfo: StatusBar1.Hint := '<actual selected datasets>/<num of total datasets>';
    PanelIndexSumFileSize: StatusBar1.Hint := 'Filesize of all selected datasets';
    PanelIndexPendingJobInfo: StatusBar1.Hint := '<num of pending jobs>';
    PanelIndexRootsInfo: StatusBar1.Hint := '<num of visible root folders>/<num of total root folders>';
    PanelIndexSearchResultInfo: StatusBar1.Hint := 'toggle search results folders / files';
    PanelIndexOpenTmpFolder: StatusBar1.Hint := 'click to open tmp folder';
  End;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  UpdateConnectedRoots;
  If ReportPendingJobsDoable And (PendingJobsDoable <> 0) Then Begin
    QueryPendingJobs();
  End;
End;

Procedure TForm1.LoadQueryHistory;
Var
  cnt, i: integer;
  s: String;
Begin
  ComboBox1.Items.Clear;
  cnt := IniFile.ReadInteger('Queries', 'Count', 0);
  For i := 0 To cnt - 1 Do Begin
    s := IniFile.ReadString('Queries', 'Query' + inttostr(i), '');
    If trim(s) <> '' Then
      ComboBox1.Items.Add(s);
  End;
End;

Procedure TForm1.StoreQueryHistory;
Var
  i: integer;
Begin
  IniFile.WriteInteger('Queries', 'Count', ComboBox1.Items.Count);
  For i := 0 To ComboBox1.Items.Count - 1 Do Begin
    IniFile.WriteString('Queries', 'Query' + inttostr(i), ComboBox1.Items[i]);
  End;
End;

Procedure TForm1.QueryPendingJobs;
Begin
  If Form4.Visible Then exit;
  If PendingJobsDoable <> 0 Then Begin
    ReportPendingJobsDoable := false;
    If Application.MessageBox('At least one pending job could be executed, start jobs now ?', 'Question', MB_ICONQUESTION Or MB_YESNO) = ID_YES Then Begin
      SpeedButton5.Click;
    End;
  End;
End;

Function TForm1.CursorToPanelIndex: integer;
Var
  p: TPoint;
  xmin, xmax, i: integer;
Begin
  p := ScreenToClient(Mouse.CursorPos);
  result := -1;
  xmin := 0;
  xmax := 0;
  For i := 0 To StatusBar1.Panels.Count - 1 Do Begin
    xmax := xmax + StatusBar1.Panels[i].Width;
    If (p.x >= xmin) And (p.x <= xmax) Then Begin
      result := i;
      break;
    End;
    xmin := xmax;
  End;
End;

Function TForm1.SelectionToIndexes: TIntegers;
Var
  i, cnt: Integer;
Begin
  result := Nil;
  If ListBox1.ItemIndex = -1 Then exit;
  If ListBox1.items[ListBox1.ItemIndex] = SearchInfo Then exit;
  cnt := 0;
  setlength(result, ListBox1.items.Count);
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Selected[i] Then Begin
      result[cnt] := Selected[i];
      inc(cnt);
    End;
  End;
  setlength(result, cnt);
End;

Procedure TForm1.UpdateSelectedState;
Begin
  StatusBar1.Panels[PanelIndexDataSetInfo].Text := format('%d/%d', [SelectedCnt, length(DataBase)]);
  StatusBar1.Panels[PanelIndexSumFileSize].Text := FileSizeToString(SelectedFileSize);
End;

Procedure TForm1.UpdatePendingJobs;
Begin
  StatusBar1.Panels[PanelIndexPendingJobInfo].Text := format('%d', [length(PendingJobs)]);
End;

Procedure TForm1.UpdateConnectedRoots;
Var
  c, i: Integer;
Begin
  C := 0;
  For i := 0 To high(RootFolders) Do Begin
    If DirectoryExists(RootFolders[i].RootFolder) Then Begin
      inc(c);
      If RootFolders[i].DiskSize = 0 Then Begin
        RootFolders[i].DiskSize := GetDiskSize(RootFolders[i].RootFolder);
      End;
      RootFolders[i].DiskFree := GetDiskFree(RootFolders[i].RootFolder);
    End;
  End;
  StatusBar1.Panels[PanelIndexRootsInfo].Text := format('%d/%d', [c, length(RootFolders)]);
End;

End.

