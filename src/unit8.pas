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
Unit Unit8;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

Type

  { TForm8 }

  TForm8 = Class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form8: TForm8;

Implementation

{$R *.lfm}

{ TForm8 }

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  caption := 'Diff view';
  button1.Align := alBottom;
  StringGrid1.Align := alClient;
End;

End.

