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
Unit Unit6;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ufilechecker;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ProgressBar1: TProgressBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

    fAbort: Boolean;
  public
    Procedure Init;

  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Jobs';
End;

Procedure TForm6.Button3Click(Sender: TObject);
Begin
  // Fragt ob es Jobs abbrechen soll und geht ggf raus
  close;
End;

Procedure TForm6.Button2Click(Sender: TObject);
Begin
  // Cancel, bricht Jobs ab und geht raus
  fAbort := true;
  Close;
End;

Procedure TForm6.Button1Click(Sender: TObject);
Var
  i, j: Integer;
  JobsDone: Array Of Boolean;
Begin
  // Execute Jobs
  fAbort := false;
  ProgressBar1.Max := strtoint(label4.caption);
  ProgressBar1.Position := 0;
  JobsDone := Nil;
  setlength(JobsDone, length(PendingJobs));
  FillChar(JobsDone[0], length(PendingJobs), false);
  For i := 0 To high(PendingJobs) Do Begin
    If ExecuteJob(PendingJobs[i]) Then Begin
      JobsDone[i] := true;
      ProgressBar1.Position := ProgressBar1.Position + 1;
    End;
    Application.ProcessMessages;
    If fAbort Then Begin
      break;
    End;
  End;
  // Löschen der Abgearbeiteten Jobs
  For i := high(JobsDone) Downto 0 Do Begin
    If JobsDone[i] Then Begin
      For j := i To high(PendingJobs) Do Begin
        PendingJobs[j] := PendingJobs[j + 1];
      End;
      SetLength(PendingJobs, high(PendingJobs));
    End;
  End;
  If Not fAbort Then Begin
    showmessage('Done.');
  End;
End;

Procedure TForm6.Init;
Begin
  label2.caption := inttostr(length(PendingJobs));
  label4.caption := inttostr(PendingJobsDoable());
End;

End.

