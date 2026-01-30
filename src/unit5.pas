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
Unit unit5;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ufilechecker;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    DateEdit1: TDateEdit;
    Edit1: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure Init(aDataSetIndex: Integer);
    Procedure LCLToDataSet(aDataSetIndex: Integer);
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Uses udirsync;

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  caption := 'File detail';
End;

Procedure TForm5.Init(aDataSetIndex: Integer);
Begin
  label2.caption := DataBase[aDataSetIndex].Filename;
  label4.caption := RootFolderToRootLabel(DataBase[aDataSetIndex].Root);
  label6.caption := FileSizeToString(DataBase[aDataSetIndex].Size);
  // Tatsächliche Einträge in der Datenbank
//    Rating: TStringArray; // ";" Liste von Freitexten
  edit1.text := DataBase[aDataSetIndex].Categories.Join(', ');
  memo1.Text := DataBase[aDataSetIndex].Comment; // Freitext, den der User beliebig setzen kann
  //    Scedule: TDateTime; // 0 = Deaktiviert, <> Zeitpunkt in der Zukunft, ab dem "Comment" gezeigt wird wenn nach Wiedervorlagen gefragt wird
  label12.caption := FormatDateTime('HH:MM:SS, DD.MM.YYYY', DataBase[aDataSetIndex].Added);
End;

Procedure TForm5.LCLToDataSet(aDataSetIndex: Integer);
Begin
  // TODO: Hier fehlt noch einiges..
  If DataBase[aDataSetIndex].Comment <> Memo1.Text Then Begin
    DataBase[aDataSetIndex].Comment := Memo1.Text;
    DBChanged := true;
  End;
End;

End.

