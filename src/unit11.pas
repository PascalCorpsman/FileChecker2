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
Unit Unit11;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

Type

  { TForm11 }

  TForm11 = Class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ProgressBar1: TProgressBar;
    ScrollBox1: TScrollBox;
    Procedure FormCreate(Sender: TObject);
  private
    roots: Array Of TGroupBox;
    Function CreateLCLRoot(aLabel: String; aFree, aSize: Int64; aTop: integer): TGroupBox;
  public
    Procedure Init;

  End;

Var
  Form11: TForm11;

Implementation

{$R *.lfm}

Uses udirsync, ufilechecker;

{ TForm11 }

Procedure TForm11.FormCreate(Sender: TObject);
Begin
  caption := 'Root info';
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  GroupBox1.free; // Nur für die entwicklung ;)
  roots := Nil;
End;

Function TForm11.CreateLCLRoot(aLabel: String; aFree, aSize: Int64;
  aTop: integer): TGroupBox;
Var
  L: TLabel;
  p: TProgressBar;
Begin
  result := TGroupBox.Create(ScrollBox1);
  result.name := 'gb' + aLabel;
  result.Parent := ScrollBox1;
  result.Left := 8;
  result.Top := aTop;
  result.Width := ScrollBox1.Width - 16;
  result.Height := 80;
  result.Caption := '';
  result.Anchors := [akTop, akLeft, akRight];
  L := TLabel.Create(result);
  l.name := result.name + 'L1';
  l.Parent := result;
  l.top := 8;
  l.left := 8;
  l.Caption := aLabel;
  p := TProgressBar.Create(result);
  p.name := result.name + 'P1';
  p.Parent := result;
  p.left := 8;
  p.Top := 32;
  p.Width := Result.Width - 16;
  p.Anchors := [akTop, akLeft, akRight];
  p.Min := 0;
  p.Max := 100;
  If (aSize <> 0) And (aFree <> -1) Then Begin
    p.Position := (100 * (aSize - aFree)) Div (aSize);
  End
  Else
    p.position := 0;
  l := TLabel.Create(Result);
  l.Name := result.Name + 'L2';
  l.Parent := Result;
  l.top := 56;
  l.left := 8;
  If (aSize <> 0) And (aFree <> -1) Then Begin
    l.caption := 'Used: ' + FileSizeToString(aSize - aFree);
  End
  Else Begin
    l.caption := 'Unknown';
  End;
  l := TLabel.Create(Result);
  l.Name := result.Name + 'L3';
  l.Parent := Result;
  l.top := 8;
  l.left := result.Width - 100 - 8;
  l.AutoSize := false;
  l.Width := 100;
  l.Alignment := taRightJustify;
  l.Anchors := [akRight, akTop];
  If (aSize <> 0) Then Begin
    l.caption := 'Total: ' + FileSizeToString(aSize);
  End
  Else Begin
    l.caption := 'Unknown';
  End;
  l := TLabel.Create(Result);
  l.Name := result.Name + 'L4';
  l.Parent := Result;
  l.top := 56;
  l.left := result.Width - 100 - 8;
  l.AutoSize := false;
  l.Width := 100;
  l.Alignment := taRightJustify;
  l.Anchors := [akRight, akTop];
  If (aFree <> -1) Then Begin
    l.caption := 'Free: ' + FileSizeToString(aFree);
  End
  Else Begin
    l.caption := 'Unknown';
  End;
End;

Procedure TForm11.Init;
Var
  i: Integer;
Begin
  For i := 0 To high(roots) Do
    roots[i].free;
  setlength(roots, length(RootFolders));
  For i := 0 To high(roots) Do Begin
    Roots[i] := CreateLCLRoot(
      RootFolders[i].RootLabel,
      RootFolders[i].DiskFree,
      RootFolders[i].DiskSize,
      8 + i * (8 + 80));
  End;
End;

End.

