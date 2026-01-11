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
Unit Unit10;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

Type

  { TForm10 }

  TForm10 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure InitWith(Const list: TStringList);

  End;

Var
  Form10: TForm10;

Implementation

{$R *.lfm}

{ TForm10 }

Procedure TForm10.FormCreate(Sender: TObject);
Begin
  caption := 'Select database';
  RadioGroup1.Caption := '';
End;

Procedure TForm10.InitWith(Const list: TStringList);
Var
  i: Integer;
Begin
  RadioGroup1.Items.Clear;
  For i := 0 To list.Count - 1 Do Begin
    RadioGroup1.Items.Add(list[i]);
  End;
  If RadioGroup1.Items.Count <> 0 Then RadioGroup1.ItemIndex := 0;
End;

End.

