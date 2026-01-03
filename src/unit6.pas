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
  Buttons, Grids, lNetComponents, ufilechecker;

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
    SpeedButton3: TSpeedButton;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure SpeedButton3Click(Sender: TObject);
  private

    fAbort: Boolean;
  public
    Procedure Init;

  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

Uses
  ucopycomandercontroller
  , udirsync
  , unit7 // Job Detail Dialog
  ;

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Jobs';
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
End;

Procedure TForm6.SpeedButton3Click(Sender: TObject);
Begin
  // Show Jobs
  form7.init;
  form7.ShowModal;
End;

Procedure TForm6.Button3Click(Sender: TObject);
Begin
  // TODO: Fragt ob es Jobs abbrechen soll und geht ggf raus
  close;
End;

Procedure TForm6.Button2Click(Sender: TObject);
Begin
  // TODO: Cancel, bricht Jobs ab und geht raus
  fAbort := true;
  Close;
End;

Procedure TForm6.Button1Click(Sender: TObject);
Var
  cnt, i, j: Integer;
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
  cnt := 0;
  For i := high(JobsDone) Downto 0 Do Begin
    If JobsDone[i] Then Begin
      inc(cnt);
      For j := i To high(PendingJobs) - 1 Do Begin
        PendingJobs[j] := PendingJobs[j + 1];
      End;
      SetLength(PendingJobs, high(PendingJobs));
    End;
  End;
  StopCopyCommander(CopyCommanderIP, CopyCommanderPort);
  If fAbort Then Begin
    showmessage(format('Abort, was able to finish: %d Jobs', [cnt]));
  End
  Else Begin
    showmessage(format('Done, was able to finish: %d Jobs', [cnt]));
  End;
End;

Procedure TForm6.Init;

  Function RootToIndex(Const aRoot: String): integer;
  Var
    i: Integer;
  Begin
    result := -1;
    For i := 0 To high(RootFolders) Do Begin
      If RootFolders[i].RootFolder = aRoot Then Begin
        result := i;
        break;
      End;
    End;
  End;

Const
  _Out = 0;
  _In = 1;

Var
  i, index: Integer;
  InOut: Array Of Array[0..1] Of int64;
Begin
  label2.caption := inttostr(length(PendingJobs));
  label4.caption := inttostr(PendingJobsDoable());
  InOut := Nil;
  setlength(InOut, Length(RootFolders));
  For i := 0 To high(InOut) Do Begin
    InOut[i][_Out] := 0;
    InOut[i][_In] := 0;
  End;
  For i := 0 To high(PendingJobs) Do Begin
    Case PendingJobs[i].Job Of
      jMove: Begin
          index := RootToIndex(PendingJobs[i].SourceRoot);
          InOut[index][_Out] := InOut[index][_Out] + PendingJobs[i].FileSize;
          index := RootToIndex(PendingJobs[i].TargetRoot);
          InOut[index][_In] := InOut[index][_In] + PendingJobs[i].FileSize;
        End;
    Else Begin
        Raise exception.create('Error: TForm6.Init, missing handler for ' + JobDetailToString(PendingJobs[i].Job));
      End;
    End;
  End;
  StringGrid1.RowCount := length(RootFolders) + 1;
  For i := 0 To high(RootFolders) Do Begin
    StringGrid1.Cells[0, i + 1] := RootFolders[i].RootLabel;
    StringGrid1.Cells[1, i + 1] := FileSizeToString(InOut[i, _Out]);
    StringGrid1.Cells[2, i + 1] := FileSizeToString(InOut[i, _In]);
  End;
  StringGrid1.AutoSizeColumns;
End;

End.

