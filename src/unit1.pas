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
(* Known Issues: none                                                         *)
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
  ComCtrls, Menus, ExtCtrls, IniFiles, ufilechecker;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    ListBox1: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
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
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure SpeedButton3Click(Sender: TObject);
    Procedure SpeedButton4Click(Sender: TObject);
    Procedure SpeedButton5Click(Sender: TObject);
    Procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      Const Rect: TRect);
    Procedure StatusBar1Hint(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    FormShowOnce: Boolean;
    Procedure LoadQueryHistory;
    Procedure StoreQueryHistory;

    Procedure QueryPendingJobs;
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
  //  , unit7 // Show pending Jobs Details
  ;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Caption := 'Filechecker 2 ver. 0.01, by Corpsman, www.Corpsman.de';
  IniFile := TIniFile.Create(GetAppConfigFile(false, true));
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  ComboBox1.text := '';
  LoadSettings;
  LoadQueryHistory;
  LoadPendingJobs;

  LoadDataBase;
  UpdateSelectedState;
  UpdatePendingJobs;
  UpdateConnectedRoots;
  FormShowOnce := true;
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
  form5.Init(DataBase[Selected[ListBox1.ItemIndex]]);
  form5.ShowModal;
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

  // F6 = Move
  If key = VK_F6 Then MenuItem1Click(Nil);
  // F8 = Delete

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
  key := 0;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  Form4.Init(SelectionToIndexes);
  form4.ShowModal;
  ComboBox1Change(Nil); // Die Suchergebnisse müssen ggf angepasst werden
  UpdatePendingJobs; // Falls Jobs entstanden sind muss das auch angezeigt werden
  // Wenn aus all den Aktionen Jobs entstanden sind, dann gleich den Job Dialog starten
  If PendingJobsDoable <> 0 Then SpeedButton5.Click;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  fn: String;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    fn := DataBase[Selected[ListBox1.ItemIndex]].Root + DataBase[Selected[ListBox1.ItemIndex]].Filename;
    openurl(ExtractFileDir(fn));
  End;
End;

Procedure TForm1.ComboBox1Change(Sender: TObject);
Var
  f2, r, f, s: String;
  i: Integer;
Begin
  s := lowercase(trim(ComboBox1.Text));
  ListBox1.Items.BeginUpdate;
  ListBox1.Clear;
  SelectedCnt := 0;
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
  r := '';
  f := s;
  If pos(':', f) <> 0 Then Begin
    r := copy(f, 1, pos(':', f) - 1);
    delete(f, 1, length(r) + 1);
  End;
  f2 := StringReplace(f, ' ', '_', [rfReplaceAll]);
  For i := 0 To high(DataBase) Do Begin
    // Falsche Root
    If r <> '' Then Begin
      If pos(r, DataBase[i].lRootlabel) = 0 Then Continue;
    End;
    // Filename Matcht nicht
    If (f <> '*') And (pos(f2, DataBase[i].lFilename) = 0) And (pos(f, DataBase[i].lFilename) = 0) Then Continue;

    // TODO: weitere Checks ?

    Selected[SelectedCnt] := i;
    inc(SelectedCnt);
    ListBox1.Items.Add(DataBase[i].RootLabel + ': ' + DataBase[i].Filename);
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
    End;
  End;
  If key = VK_ESCAPE Then Begin
    Close;
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
  StoreDataBase;
  StoreQueryHistory;
  StorePendingJobs;
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
  // TODO: DB integritätscheck machen -> sind tatsächlich alle Dateien in der DB da wo die DB behauptet ?
End;

Procedure TForm1.SpeedButton5Click(Sender: TObject);
Begin
  // Execute Pending Jobs
  form6.Init;
  form6.ShowModal;
  UpdatePendingJobs; // Danach dann ..
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
      index := i + 2; // Oh man ist das ein HACK
      break;
    End;
  End;
  ImageList1.Draw(StatusBar1.Canvas, rect.Left + 8, rect.Top, index);
  s := StatusBar1.Panels[i].Text;
  StatusBar1.Canvas.Brush.Style := bsClear;
  StatusBar1.Canvas.TextOut(rect.Left + 8 + 16 + 8, (rect.Top + rect.Bottom - StatusBar1.Canvas.TextHeight(s)) Div 2, s);
End;

Procedure TForm1.StatusBar1Hint(Sender: TObject);
Var
  Index: Integer;
  p: TPoint;
  xmin, xmax, i: integer;
Begin
  // Rauskriegen auf welchem Panel die Maus gerade ist.
  // Warum auch immer die panels kein ClientRect haben, dann machen wir das halt von Hand ;)
  p := ScreenToClient(Mouse.CursorPos);
  index := 0;
  xmin := 0;
  xmax := 0;
  For i := 0 To StatusBar1.Panels.Count - 1 Do Begin
    xmax := xmax + StatusBar1.Panels[i].Width;
    If (p.x >= xmin) And (p.x <= xmax) Then Begin
      index := i;
      break;
    End;
    xmin := xmax;
  End;
  // Setzen des Passenden Hints als pseudo EBNF ;)
  StatusBar1.Hint := '';
  Case index Of
    0: Begin
        StatusBar1.Hint := '<actual selected datasets>/<num of total datasets>';
      End;
    1: Begin
        StatusBar1.Hint := '<num of pending jobs>';
      End;
    2: Begin
        StatusBar1.Hint := '<num of visible root folders>/<num of total root folders>';
      End;
  End;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  UpdateConnectedRoots;
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
  If PendingJobsDoable <> 0 Then Begin
    If Application.MessageBox('At least one pending job could be executed, start jobs now ?', 'Question', MB_ICONQUESTION Or MB_YESNO) = ID_YES Then Begin
      SpeedButton5.Click;
    End;
  End;
End;

Function TForm1.SelectionToIndexes: TIntegers;
Var
  i: Integer;
Begin
  result := Nil;
  // Move Files to ..
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Selected[i] Then Begin
      setlength(result, high(result) + 2);
      result[high(result)] := Selected[i];
    End;
  End;
End;

Procedure TForm1.UpdateSelectedState;
Begin
  StatusBar1.Panels[0].Text := format('%d/%d', [SelectedCnt, length(DataBase)]);
End;

Procedure TForm1.UpdatePendingJobs;
Begin
  StatusBar1.Panels[1].Text := format('%d', [length(PendingJobs)]);
End;

Procedure TForm1.UpdateConnectedRoots;
Var
  c, i: Integer;
Begin
  C := 0;
  For i := 0 To high(RootFolders) Do Begin
    If DirectoryExists(RootFolders[i].RootFolder) Then inc(c);
  End;
  StatusBar1.Panels[2].Text := format('%d/%d', [c, length(RootFolders)]);
End;

End.

