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
    CheckListBox1: TCheckListBox;
    ComboBox2: TComboBox;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure StringGrid1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      Var CanSelect: Boolean);
  private
    fFiles: TIntegers;
  public
    Procedure Init(Const Selection: TIntegers);

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses math;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Move';
End;

Procedure TForm4.FormShow(Sender: TObject);
Begin
  StringGrid1.Selection := rect(0, 0, 0, 0);
  StringGrid1.EditorMode := true;
  StringGrid1.Editor.SetFocus;
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
  i: Integer;
  s: String;
Begin
  // Do it
  CheckListBox1.Items.BeginUpdate;
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      s := StringGrid1.Cells[0, 0];
      If s <> '' Then s := IncludeTrailingPathDelimiter(s);
      s := s + ExtractFileName(DataBase[fFiles[i]].Filename);
      TryDoMove(fFiles[i], RootFolders[ComboBox2.ItemIndex].RootFolder, s);
      CheckListBox1.Checked[i] := false;
    End;
  End;
  CheckListBox1.Items.EndUpdate;
End;

Procedure TForm4.StringGrid1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  s, s2: String;
  i: Integer;
  b: Boolean;
Begin
  If StringGrid1.EditorMode Then Begin
    StringGrid1.BeginUpdate;
    s := lowercase(StringGrid1.Cells[0, 0]);
    s2 := StringReplace(s, ' ', '_', [rfReplaceAll]);
    If s = '' Then Begin
      For i := 1 To StringGrid1.RowCount - 1 Do Begin
        StringGrid1.RowHeights[i] := StringGrid1.DefaultRowHeight;
      End;
    End
    Else Begin
      For i := 1 To StringGrid1.RowCount - 1 Do Begin
        b := pos(s, lowercase(StringGrid1.Cells[0, i])) <> 0;
        If Not b Then Begin
          b := pos(s2, lowercase(StringGrid1.Cells[0, i])) <> 0;
        End;
        StringGrid1.RowHeights[i] := ifthen(b, StringGrid1.DefaultRowHeight, 0)
      End;
    End;
    StringGrid1.EndUpdate();
  End;
End;

Procedure TForm4.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  Var CanSelect: Boolean);
Begin
  If aRow <> 0 Then Begin
    StringGrid1.Cells[0, 0] := StringGrid1.Cells[0, aRow];
  End;
End;

Procedure TForm4.Init(Const Selection: TIntegers);
Var
  i: Integer;
  lf, af: String;
Begin
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
  StringGrid1.RowCount := 1;
  StringGrid1.Cells[0, 0] := ExtractFileDir(DataBase[fFiles[0]].Filename);
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
End;

End.

