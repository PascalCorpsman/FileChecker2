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
Unit unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls,
  Grids, ufilechecker;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure StringGrid1Click(Sender: TObject);
    Procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      Var CanSelect: Boolean);
  private
    fFiles: TIntegers;
    faRow: Integer;
  public
    Procedure Init(Const Selection: TIntegers);

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses math, LCLType;

// Wir müssen nun den Dateinamen Stamm heraus bekommen, dieser steht vor
// <Zahl>x<Zahl> oder <Zahl>e<Zahl>, wenn es eine Serie ist, sonst ists eh egal.
// Können wir das nicht finden -> kann kein Dateinamen Stamm bestimmt werden.

Function GuessBaseFileName(aFilename: String): String;
Var
  i, j: Integer;
Begin
  aFilename := ExtractFileName(aFilename);
  result := '';
  For i := 2 To length(aFilename) - 1 Do Begin
    If (
      (aFilename[i] = 'x') Or
      (aFilename[i] = 'X') Or
      (aFilename[i] = 'e') Or
      (aFilename[i] = 'E')
      )
      And (aFilename[i - 1] In ['0'..'9'])
      And (aFilename[i + 1] In ['0'..'9'])
      Then Begin
      For j := i - 1 Downto 1 Do Begin
        If (Not (aFilename[j] In ['0'..'9'])) Then Begin
          result := copy(aFilename, 1, j);
          exit;
        End;
      End;
    End;
  End;
End;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Move';
End;

Procedure TForm4.FormShow(Sender: TObject);
Begin
  edit1.SetFocus;
End;

Procedure TForm4.StringGrid1Click(Sender: TObject);
Begin
  If faRow <> -1 Then Begin
    edit1.text := StringGrid1.Cells[0, faRow];
  End;
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  // Select All
  CheckListBox1.CheckAll(cbChecked);
End;

Procedure TForm4.Button2Click(Sender: TObject);
Begin
  // Select None
  CheckListBox1.CheckAll(cbUnchecked);
End;

Procedure TForm4.Button3Click(Sender: TObject);
Var
  i, removerscnt: Integer;
  s: String;
  removers: TIntegers;
Begin
  // Do it
  removers := Nil;
  setlength(removers, CheckListBox1.Items.Count);
  removerscnt := 0;
  CheckListBox1.Items.BeginUpdate;
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      If CheckBox1.Checked Then Begin
        s := ExtractFilePath(DataBase[fFiles[i]].Filename);
      End
      Else Begin
        s := Edit1.text;
      End;
      s := FixPathDelims(s);
      If s <> '' Then s := IncludeTrailingPathDelimiter(s);
      s := s + ExtractFileName(DataBase[fFiles[i]].Filename);
      If TryDoMove(fFiles[i], RootFolders[ComboBox2.ItemIndex].RootFolder, s) Then Begin
        removers[removerscnt] := i;
        inc(removerscnt);
        CheckListBox1.Checked[i] := false;
      End;
    End;
  End;
  // Wir löschen die bereits "erledigten" aus der Checklist heraus, die gibt es nun ja nicht mehr
  For i := removerscnt - 1 Downto 0 Do Begin
    CheckListBox1.Items.Delete(removers[i]);
  End;
  CheckListBox1.Items.EndUpdate;
End;

Procedure TForm4.CheckBox1Click(Sender: TObject);
Begin
  If CheckBox1.Checked Then Begin
    edit1.text := '';
    StringGrid1.RowCount := 0;
  End;
End;

Procedure TForm4.Edit1Change(Sender: TObject);
Var
  s, s2: String;
  i: Integer;
  b: Boolean;
Begin
  s := lowercase(Edit1.text);
  If s = '' Then exit;
  StringGrid1.BeginUpdate;
  s2 := StringReplace(s, ' ', '_', [rfReplaceAll]);
  If s = '' Then Begin
    For i := 0 To StringGrid1.RowCount - 1 Do Begin
      StringGrid1.RowHeights[i] := StringGrid1.DefaultRowHeight;
    End;
  End
  Else Begin
    For i := 0 To StringGrid1.RowCount - 1 Do Begin
      b := pos(s, lowercase(StringGrid1.Cells[0, i])) <> 0;
      If Not b Then Begin
        b := pos(s2, lowercase(StringGrid1.Cells[0, i])) <> 0;
      End;
      StringGrid1.RowHeights[i] := ifthen(b, StringGrid1.DefaultRowHeight, 0)
    End;
  End;
  StringGrid1.EndUpdate();
End;

Procedure TForm4.Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If key = VK_RETURN Then Begin
    Button3.Click;
    key := 0;
  End;
End;

Procedure TForm4.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  Var CanSelect: Boolean);
Begin
  faRow := aRow;
End;

Procedure TForm4.Init(Const Selection: TIntegers);
Var
  i: Integer;
  Filedir, lf, af: String;
Begin
  faRow := -1;
  // Die Root Labels
  ComboBox2.Items.Clear;
  For i := 0 To high(RootFolders) Do Begin
    ComboBox2.Items.Add(RootFolders[i].RootLabel);
  End;
  ComboBox2.ItemIndex := 0; // TODO: ggf einen besseren Index wählen..

  // Die Dateien, welche es zu verschieben gillt
  CheckListBox1.Clear;
  fFiles := Selection;
  For i := 0 To high(fFiles) Do Begin
    CheckListBox1.Items.Add(DataBase[fFiles[i]].RootLabel + ': ' + DataBase[fFiles[i]].Filename);
    CheckListBox1.Checked[i] := true;
  End;
  StringGrid1.BeginUpdate;
  StringGrid1.RowCount := 0;
  //  StringGrid1.Cells[0, 0] := '';
  lf := '';
  For i := 0 To high(DataBase) Do Begin
    af := ExtractFilePath(DataBase[i].Filename);
    If lf <> af Then Begin
      StringGrid1.RowCount := StringGrid1.RowCount + 1;
      StringGrid1.Cells[0, StringGrid1.RowCount - 1] := af;
      lf := af;
    End;
  End;
  StringGrid1.EndUpdate();
  StringGrid1.AutoSizeColumns;
  // das muss nach dem Befüllen der Stringgrid sein !
  Filedir := ExtractFileDir(DataBase[fFiles[0]].Filename);
  If Filedir <> '' Then Begin
    Edit1.text := Filedir;
  End
  Else Begin
    Edit1.text := '';
    lf := GuessBaseFileName(DataBase[fFiles[0]].Filename);
    If lf <> '' Then Begin
      // Die Aktuelle Quelle hat keine Verzeichnis Struktur, dann schauen wir mal
      // ob dir die Datei nicht in der Datenbank finden wo sie ggf doch einen Ordner hat ..
      // Suche von Hinten findet dann auch immer den Aktuellsten ;)
      For i := high(DataBase) Downto 0 Do Begin
        If pos(lf, DataBase[i].Filename) <> 0 Then Begin
          af := ExtractFileDir(DataBase[i].Filename);
          If af <> '' Then Begin
            edit1.text := af;
            exit;
          End;
        End;
      End;
    End;
  End;
End;

End.

