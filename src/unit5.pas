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
    Procedure Init(Const aDataSet: TDataSet);
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

Procedure TForm5.Init(Const aDataSet: TDataSet);
Begin
  label2.caption := aDataSet.Filename;
  label4.caption := RootFolderToRootLabel(aDataSet.Root);
  label6.caption := FileSizeToString(aDataSet.Size);
  // Tatsächliche Einträge in der Datenbank
//    Rating: TStringArray; // ";" Liste von Freitexten
//    Categories: TStringArray; // ";" Liste von Freitexten
//    Comment: String; // Freitext, den der User beliebig setzen kann
//    Scedule: TDateTime; // 0 = Deaktiviert, <> Zeitpunkt in der Zukunft, ab dem "Comment" gezeigt wird wenn nach Wiedervorlagen gefragt wird
  label12.caption := FormatDateTime('HH:MM:SS, DD.MM.YYYY', aDataSet.Added);
End;

End.

