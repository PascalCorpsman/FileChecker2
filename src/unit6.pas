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
    Label5: TLabel;
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
    Procedure RefreshGrid();
  public
    Procedure Init;

  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

Uses
  LCLType
  , ucopycomandercontroller
  , udirsync
  , unit7 // Job Detail Dialog
  ;

Const
  _Out = 0;
  _In = 1;

Var
  InOut: Array Of Array[0..1] Of int64;

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
  init;
End;

Procedure TForm6.RefreshGrid();
Var
  i: Integer;
Begin
  For i := 0 To high(RootFolders) Do Begin
    StringGrid1.Cells[0, i + 1] := RootFolders[i].RootLabel;
    StringGrid1.Cells[1, i + 1] := FileSizeToString(InOut[i, _Out]);
    StringGrid1.Cells[2, i + 1] := FileSizeToString(InOut[i, _In]);
  End;
End;

Procedure TForm6.Button3Click(Sender: TObject);
Begin
  // Fragt ob es Jobs abbrechen soll und geht ggf raus
  If Not Button1.Enabled Then Begin
    Case Application.MessageBox('Jobs are running, abort ?', 'Question', MB_ICONQUESTION Or MB_YESNO) Of
      id_yes: Begin
          Button2Click(Nil);
          exit;
        End;
      id_no: Begin
          exit;
        End;
    End;
  End;
  close;
End;

Procedure TForm6.Button2Click(Sender: TObject);
Begin
  // Cancel, bricht Jobs ab und geht raus
  If Not Button1.Enabled Then Begin
    fAbort := true;
    showmessage('The current job will be finished, than will abort.');
  End;
  Close;
End;

Procedure TForm6.Button1Click(Sender: TObject);
Var
  cnt, i, j, index: Integer;
  JobsDone: Array Of Boolean;
Begin
  button1.Enabled := false;
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
      label5.caption := format('%d/%d', [ProgressBar1.Position, ProgressBar1.Max]);
      // !! ACHTUNG !!, die Umkehrfunktion zu dem hier ist implementiert in TForm6.Init
      Case PendingJobs[i].Job Of
        jMove: Begin
            index := GetRootIndexOf(PendingJobs[i].SourceRoot);
            InOut[index][_Out] := InOut[index][_Out] - PendingJobs[i].FileSize;
            index := GetRootIndexOf(PendingJobs[i].TargetRoot);
            InOut[index][_In] := InOut[index][_In] - PendingJobs[i].FileSize;
          End;
      Else Begin
          Raise exception.create('Error: TForm6.Button1Click, missing handler for ' + JobDetailToString(PendingJobs[i].Job));
        End;
      End;
      RefreshGrid();
      Application.ProcessMessages;
    End
    Else Begin
      nop(); // For debugging
    End;
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
  Init;
  button1.Enabled := true;
End;

Procedure TForm6.Init;

Var
  i, index: Integer;

Begin
  label2.caption := inttostr(length(PendingJobs));
  label4.caption := inttostr(PendingJobsDoable());
  label5.caption := '';
  InOut := Nil;
  setlength(InOut, Length(RootFolders));
  For i := 0 To high(InOut) Do Begin
    InOut[i][_Out] := 0;
    InOut[i][_In] := 0;
  End;
  For i := 0 To high(PendingJobs) Do Begin
    // !! ACHTUNG !!, die Umkehrfunktion zu dem hier ist implementiert in TForm6.Button1Click
    Case PendingJobs[i].Job Of
      jMove: Begin
          index := GetRootIndexOf(PendingJobs[i].SourceRoot);
          InOut[index][_Out] := InOut[index][_Out] + PendingJobs[i].FileSize;
          index := GetRootIndexOf(PendingJobs[i].TargetRoot);
          InOut[index][_In] := InOut[index][_In] + PendingJobs[i].FileSize;
        End;
    Else Begin
        Raise exception.create('Error: TForm6.Init, missing handler for ' + JobDetailToString(PendingJobs[i].Job));
      End;
    End;
  End;
  StringGrid1.RowCount := length(RootFolders) + 1;
  RefreshGrid();
  StringGrid1.AutoSizeColumns;
End;

End.

