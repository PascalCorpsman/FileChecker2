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
Unit Unit7;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ufilechecker;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBox1: TScrollBox;
    Procedure FormCreate(Sender: TObject);
  private
    fJobs: Array Of TGroupBox;

    Procedure OnCancelButtonClick(Sender: TObject);
    Procedure AddJob(Const aJob: TJob);
  public
    Procedure Init;

  End;

Var
  Form7: TForm7;

Implementation

{$R *.lfm}

{ TForm7 }

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  fJobs := Nil;
  caption := 'Job details';
  // GroupBox1 is only for designtime !
  GroupBox1.Free;
  GroupBox1 := Nil;
End;

Procedure TForm7.OnCancelButtonClick(Sender: TObject);
Var
  index: Integer;
Begin
  showmessage('Not implemented yet.');
  // 1. Check ob der Job Rückgängig gemacht werden kann
  index := TButton(sender).Tag;

End;

Procedure TForm7.AddJob(Const aJob: TJob);
Var
  t, index, i: Integer;
  g: TGroupBox;
  l: TLabel;
  e: TEdit;
  b: TButton;
Begin
  t := 8;
  For i := 0 To high(fJobs) Do Begin
    t := t + fJobs[i].Height + 8;
  End;

  setlength(fJobs, high(fJobs) + 2);
  index := high(fJobs);

  g := TGroupBox.Create(ScrollBox1);
  g.name := 'Job' + inttostr(index);
  g.Parent := ScrollBox1;
  g.left := 8;
  g.top := t;
  g.Width := ScrollBox1.Width - 16;
  g.Anchors := [akTop, akLeft, akRight];
  g.Caption := ' ' + JobDetailToString(aJob.Job) + ' ';
  Case aJob.Job Of
    jMove: Begin
        g.Height := 137;
        l := TLabel.Create(g);
        l.name := 'SourceLabel';
        l.Parent := g;
        l.caption := 'Source:';
        l.Left := 8;
        l.top := 8;
        e := TEdit.Create(g);
        e.Name := 'SourceEdit';
        e.Parent := g;
        e.text := aJob.RealSourceFile;
        e.Top := 24;
        e.Left := 8;
        e.Width := g.Width - 89;
        e.Anchors := [akTop, akLeft, akRight];
        l := TLabel.create(g);
        l.name := 'DestLabel';
        l.parent := g;
        l.Left := 8;
        l.top := 64;
        l.Caption := 'Destination:';
        e := TEdit.Create(g);
        e.Name := 'DestEdit';
        e.Parent := g;
        e.text := aJob.TargetRoot + ajob.TargetFilename;
        e.Top := 80;
        e.Left := 8;
        e.Width := g.Width - 89;
        e.Anchors := [akTop, akLeft, akRight];
        b := TButton.Create(g);
        b.name := 'DelButton';
        b.Parent := g;
        b.left := g.Width - 75;
        b.Top := 24;
        b.Width := 59;
        b.Height := 79;
        b.Tag := index;
        b.Caption := 'Cancel';
        b.OnClick := @OnCancelButtonClick;
        b.Anchors := [akTop, akRight];
      End;
    //    jDelete: Begin
    //
    //      End;
  Else Begin
      showmessage('Error, preview for ' + JobDetailToString(aJob.Job) + ' not implemented yet.');
    End;
  End;
  fJobs[index] := g;
End;

Procedure TForm7.Init;
Var
  i: Integer;
Begin
  // Clear
  For i := 0 To high(fJobs) Do Begin
    fJobs[i].free;
  End;
  setlength(fJobs, 0);
  For i := 0 To high(PendingJobs) Do Begin
    AddJob(PendingJobs[i]);
  End;

End;

End.

