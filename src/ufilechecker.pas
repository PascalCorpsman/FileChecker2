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
Unit ufilechecker;

{$MODE ObjFPC}{$H+}

{$MODESWITCH TypeHelpers}

Interface

Uses
  Classes, SysUtils, IniFiles, udirsync;

Const
  Separator = #9;
  Divider = ';';

Type

  TJobDetail =
    (
    jMove
    , jDelete
    );

  TIntegers = Array Of Integer;

  TStringArrayHelper = Type Helper For TStringArray
    Function Join(Const delimiter: String): String;
  End;

  TDataSet = Record
    // Tatsächliche Einträge in der Datenbank
    Filename: String; // Relativer Dateiname ab "Root"
    Root: String; // "Root" Verzeichnis abschließend mit Pathdelim -> Auflösbar nach Rootlabel via TRoot
    Size: Int64; // in Byte
    Rating: TStringArray; // ";" Liste von Freitexten
    Categories: TStringArray; // ";" Liste von Freitexten
    Comment: String; // Freitext, den der User beliebig setzen kann
    Scedule: TDateTime; // 0 = Deaktiviert, <> Zeitpunkt in der Zukunft, ab dem "Comment" gezeigt wird wenn nach Wiedervorlagen gefragt wird
    Added: TDateTime; // Zeitpunkt an dem die Datei in die Datenbank aufgenommen wurde
    // Zur Laufzeit "generierte" Einträge
    RootLabel: String;
    lRootlabel: String; // := lowercase(RootLabel);
    lFilename: String; // := lowercase(Filename);
  End;

  TDataSets = Array Of TDataSet;

  TRootFolder = Record
    RootFolder: String; // Verzeichnis abschließend mit Pathdelim
    RootLabel: String; // Beschreigung die vom User eingegeben werden darf
    Excludes: TStringArray; // Liste der Unterverzeichnisse, welche ignoriert werden sollem beim Scannen
  End;

  TRootFolders = Array Of TRootFolder;

  // Sammel Container für Jobs
  TJob = Record
    Job: TJobDetail;
    SourceRoot: String;
    SourceFile: String;
    RealSourceFile: String; // Absoluter Dateiname (inclusive Root), im Einfachsten Fall SourceRoot + SourceFile
    TargetRoot: String; // Ziel Root
    TargetFilename: String; // Ziel Dateiname Relativ zu Ziel Root
  End;

Var
  IniFile: TIniFile = Nil;

  JobTempFolder: String; // Verzeichnis, in dass die Anwendung Jobs zwischenlagert, dass es den User nicht stört ;)
  CopyCommanderCmd: String; // Befehl zum Starten des Copy Commanders
  RootFolders: TRootFolders;
  DataBase: TDataSets = Nil; // !! ACHTUNG !! Database muss immer sortiert sein, also nach jeder Änderung SortFileList !
  Selected: TIntegers = Nil; //
  SelectedCnt: Integer = 0;
  PendingJobs: Array Of TJob = Nil;
  DBChanged: Boolean = false;

Procedure LoadSettings;
Procedure LoadDataBase;
Procedure LoadPendingJobs;

Procedure StoreSettings;
Procedure StoreDataBase;
Procedure StorePendingJobs;

Procedure SortFileList(Var aList: TDataSets);

(*
 * -1 = Fehler, nicht gefunden
 * sonst Index in DataBase
 *)
Function GetIndexOf(Const Root, Filename: String): integer;

(*
 * Gibt zu einer Root das Passende Label zurück
 * '' = fehler
 *)
Function RootFolderToRootLabel(Const aRoot: String): String;

Procedure Nop(); // Just for debugging ;)

Procedure CreateReportFile2(FileName: String;
  Const RenameList: TRenameList; Const CopyList, DelList: TFileList;
  Const Info: TReportInfos);

Procedure TryDoMove(index: Integer; TargetRoot, TargetFilename: String);

(*
 * Aktualisiert die Dynamischen Datenfelder des TDataSet Datensatzes von Index
 *)
Procedure RefreshDynamicContent(index: integer);

(*
 * Gibt die Anzahl der Ausführbaren Jobs zurück
 *)
Function PendingJobsDoable: integer;

Function ExecuteJob(Const aJob: TJob): Boolean;

Implementation

Procedure Nop;
Begin

End;

(*
 * Ersetzt:
 * Separator -> +9
 * LineEnding -> +3
 * + durch ++
 *)

Function SerializeString(Const Input: String): String;
Begin
  result := StringReplace(Input, '+', '++', [rfReplaceAll]);
  result := StringReplace(result, Separator, '+9', [rfReplaceAll]);
  result := StringReplace(result, LineEnding, '+3', [rfReplaceAll]);
  // TODO: weitere ?
End;

(*
 * Umkehrfunktion zu SerializeString
 *)

Function DeSerializeString(Const Input: String): String;
Begin
  result := StringReplace(Input, '+3', LineEnding, [rfReplaceAll]);
  result := StringReplace(result, '+9', Separator, [rfReplaceAll]);
  // TODO: weitere ?
  result := StringReplace(result, '++', '+', [rfReplaceAll]);
End;

Function TStringArrayHelper.Join(Const delimiter: String): String;
Var
  i: Integer;
Begin
  Result := '';
  For i := Low(Self) To High(Self) Do Begin
    If i > Low(Self) Then
      Result := Result + delimiter; // Füge das Trennzeichen nur zwischen den Elementen hinzu
    Result := Result + Self[i];
  End;
End;

Function JobDetailToString(aDetail: TJobDetail): String;
Begin
  Case aDetail Of
    jMove: result := 'Move';
    jDelete: result := 'Delete';
  Else Begin
      Raise Exception.Create('JobDetailToString: missing detail');
    End;
  End;
End;

Function StringToJobDetail(aStr: String): TJobDetail;
Begin
  Case lowercase(aStr) Of
    'move': result := jMove;
    'delete': result := jDelete;
  Else Begin
      Raise Exception.Create('StringToJobDetail: missing detail for: ' + astr);
    End;
  End;
End;

Procedure LoadSettings;
Var
  ecnt, cnt: LongInt;
  i, j: Integer;
Begin
  JobTempFolder := IniFile.ReadString('TempFolder', 'Folder', '');

  cnt := IniFile.ReadInteger('RootFolders', 'Count', 0);
  setlength(RootFolders, cnt);
  For i := 0 To cnt - 1 Do Begin
    RootFolders[i].RootFolder := IniFile.ReadString('RootFolders', 'Folder' + inttostr(i), '');
    RootFolders[i].RootLabel := IniFile.ReadString('RootFolders', 'Label' + inttostr(i), '');
    ecnt := IniFile.ReadInteger('RootFolders', 'ExcludeCount' + inttostr(i), 0);
    setlength(RootFolders[i].Excludes, ecnt);
    For j := 0 To ecnt - 1 Do Begin
      RootFolders[i].Excludes[j] := IniFile.ReadString('RootFolders', 'Exclude' + inttostr(i) + '_' + inttostr(j), '');
    End;
  End;

  CopyCommanderCmd := IniFile.ReadString('CopyCommander', 'CMD',
    'CopyCommander2'{$IFDEF Windows} + '.exe'{$ENDIF} + ' -enablerest -restport=8080');
End;

Procedure StoreSettings;
Var
  i, j: Integer;
Begin
  IniFile.WriteString('TempFolder', 'Folder', JobTempFolder);

  IniFile.WriteInteger('RootFolders', 'Count', length(RootFolders));
  For i := 0 To high(RootFolders) Do Begin
    IniFile.WriteString('RootFolders', 'Folder' + inttostr(i), RootFolders[i].RootFolder);
    IniFile.WriteString('RootFolders', 'Label' + inttostr(i), RootFolders[i].RootLabel);

    IniFile.WriteInteger('RootFolders', 'ExcludeCount' + inttostr(i), length(RootFolders[i].Excludes));
    For j := 0 To high(RootFolders[i].Excludes) Do Begin
      IniFile.WriteString('RootFolders', 'Exclude' + inttostr(i) + '_' + inttostr(j), RootFolders[i].Excludes[j]);
    End;
  End;

  IniFile.WriteString('CopyCommander', 'CMD', CopyCommanderCmd);
End;

Function DataSetToString(Const aDataSet: TDataSet): String;
Var
  fs: TFormatSettings;
Begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  result :=
    aDataSet.Filename + Separator +
    aDataSet.Root + Separator +
    IntToStr(aDataSet.Size) + Separator +
    aDataSet.Rating.Join(Divider) + Separator +
    aDataSet.Categories.Join(Divider) + Separator +
    SerializeString(aDataSet.Comment) + Separator +
    FloatToStr(aDataSet.Scedule, fs) + Separator +
    FloatToStr(aDataSet.Added, fs);
End;

Function StringToDataSet(Const aDataSet: String): TDataSet;
Var
  sa: TStringArray;
  fs: TFormatSettings;
Begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  sa := aDataSet.Split(Separator);
  // TODO: Checks ?

  result.Filename := sa[0];
  result.Root := sa[1];
  result.Size := StrToInt64(sa[2]);
  result.Rating := sa[3].Split(Divider);
  result.Categories := sa[4].Split(Divider);
  result.Comment := DeSerializeString(sa[5]);
  result.Scedule := StrToFloat(sa[6], fs);
  result.Added := StrToFloat(sa[7], fs);
End;

Procedure LoadDataBase;
Var
  sl: TStringList;
  fn: String;
  i: Integer;
Begin
  // DataBase
  fn := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'database.db';
  If Not FileExists(fn) Then exit;
  sl := TStringList.Create;
  sl.LoadFromFile(fn);
  setlength(DataBase, sl.Count);
  For i := 0 To sl.Count - 1 Do Begin
    DataBase[i] := StringToDataSet(sl[i]);
    RefreshDynamicContent(i);
  End;
  sl.free;
  DBChanged := false;
End;

Procedure StoreDataBase;
Var
  sl: TStringList;
  fn: String;
  i: Integer;
Begin
  If Not DBChanged Then exit;
  fn := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'database.db';
  sl := TStringList.Create;
  For i := 0 To high(DataBase) Do Begin
    sl.add(DataSetToString(DataBase[i]));
  End;
  sl.SaveToFile(fn);
  sl.free;
  DBChanged := false;
End;

Procedure LoadPendingJobs;
Var
  cnt, i: integer;
Begin
  cnt := IniFile.ReadInteger('Jobs', 'Count', 0);
  setlength(PendingJobs, cnt);
  For i := 0 To high(PendingJobs) Do Begin
    PendingJobs[i].Job := StringToJobDetail(IniFile.ReadString('Jobs', 'Job' + IntToStr(i), ''));
    PendingJobs[i].SourceFile := IniFile.ReadString('Jobs', 'SourceFile' + IntToStr(i), '');
    PendingJobs[i].SourceRoot := IniFile.ReadString('Jobs', 'SourceRoot' + IntToStr(i), '');
    PendingJobs[i].RealSourceFile := IniFile.ReadString('Jobs', 'RealSourceFile' + IntToStr(i), '');
    PendingJobs[i].TargetRoot := IniFile.ReadString('Jobs', 'TargetRoot' + IntToStr(i), '');
    PendingJobs[i].TargetFilename := IniFile.ReadString('Jobs', 'TargetFilename' + IntToStr(i), '');
  End;
End;

Procedure StorePendingJobs;
Var
  i: integer;
Begin
  IniFile.WriteInteger('Jobs', 'Count', length(PendingJobs));
  For i := 0 To high(PendingJobs) Do Begin
    IniFile.WriteString('Jobs', 'Job' + IntToStr(i), JobDetailToString(PendingJobs[i].Job));
    IniFile.WriteString('Jobs', 'SourceFile' + IntToStr(i), PendingJobs[i].SourceFile);
    IniFile.WriteString('Jobs', 'SourceRoot' + IntToStr(i), PendingJobs[i].SourceRoot);
    IniFile.WriteString('Jobs', 'RealSourceFile' + IntToStr(i), PendingJobs[i].RealSourceFile);
    IniFile.WriteString('Jobs', 'TargetRoot' + IntToStr(i), PendingJobs[i].TargetRoot);
    IniFile.WriteString('Jobs', 'TargetFilename' + IntToStr(i), PendingJobs[i].TargetFilename);
  End;
End;

Procedure SortFileList(Var aList: TDataSets);
  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    p: String;
    h: TDataSet;
  Begin
    If Li < Re Then Begin
      p := aList[Trunc((li + re) / 2)].FileName; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(aList[l].FileName, p) < 0 Do
          inc(l);
        While CompareStr(aList[r].FileName, p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := aList[l];
          aList[l] := aList[r];
          aList[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Begin
  Quick(0, high(aList));
End;

Function GetIndexOf(Const Root, Filename: String): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(DataBase) Do Begin
    If (DataBase[i].Root = Root) And
      (DataBase[i].Filename = Filename) Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function RootFolderToRootLabel(Const aRoot: String): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 0 To high(RootFolders) Do Begin
    If aRoot = RootFolders[i].RootFolder Then Begin
      result := RootFolders[i].RootLabel;
      exit;
    End;
  End;
End;

Procedure AddJob(Const aJob: TJob);
Var
  Index: Integer;
Begin
  // TODO: ggf. noch irgendwelche Checks ?
  setlength(PendingJobs, high(PendingJobs) + 2);
  PendingJobs[high(PendingJobs)] := aJob;
  (*
   * Jeder Job der Angenommen wird, wird auf der Datenbank schon "durchgeführt"
   *)
  Index := GetIndexOf(ajob.SourceRoot, aJob.SourceFile);
  If index = -1 Then Begin
    Raise exception.Create('Error: AddJob, can not find source: "' + aJob.SourceFile + '" in database.');
  End;
  Case aJob.Job Of
    jMove: Begin
        DataBase[index].Root := aJob.TargetRoot;
        DataBase[index].Filename := aJob.TargetFilename;
        RefreshDynamicContent(index);
        DBChanged := true;
      End;
  Else Begin
      Raise exception.Create('Error: AddJob, missing implementation for: ' + JobDetailToString(ajob.Job));
    End;
  End;
End;

Procedure CreateReportFile2(FileName: String; Const RenameList: TRenameList;
  Const CopyList, DelList: TFileList; Const Info: TReportInfos);
Var
  sl: TStringList;
  i: Integer;
Begin
  sl := TStringList.create;
  If length(RenameList) <> 0 Then Begin
    sl.add('Renamelist;"' + info.RenameInfo + '"');
    sl.add('From;To;');
    For i := 0 To high(RenameList) Do Begin
      sl.add(
        '"' + RenameList[i].SourceRoot + RenameList[i].SourceFile + '";' +
        '"' + RenameList[i].DestRoot + RenameList[i].DestFile + '"'
        );
    End;
  End;
  If length(CopyList) <> 0 Then Begin
    sl.add('Added;"' + info.CopyInfo + '"');
    For i := 0 To high(CopyList) Do Begin
      sl.add(
        '"' + CopyList[i].Root + CopyList[i].FileName + '"'
        );
    End;
  End;
  If length(DelList) <> 0 Then Begin
    sl.add('Deleted;"' + Info.DelInfo + '"');
    sl.add('File');
    For i := 0 To high(DelList) Do Begin
      sl.add(
        '"' + DelList[i].Root + DelList[i].FileName + '"'
        );
    End;
  End;
  sl.SaveToFile(FileName);
  sl.free;
End;

Procedure AddMoveJob(index: Integer; TargetRoot, TargetFilename: String);
Var
  TmpTarget, SourceName: String;
  job: TJob;
Begin
  SourceName := DataBase[index].Root + DataBase[index].Filename;
  // Wenn es die Datei gibt und wir sie mittels Rename in unser Temp Verzeichnis
  // Schieben können, machen wir das, damit ists dann "aufgeräumter" ;)
  If FileExists(SourceName) And (JobTempFolder <> '') Then Begin
    TmpTarget := JobTempFolder + ExtractFileName(SourceName);
    If RenameFile(SourceName, TmpTarget) Then Begin
      SourceName := TmpTarget;
    End;
  End;
  job.Job := jMove;
  job.SourceFile := DataBase[index].Filename;
  job.SourceRoot := DataBase[index].Root;
  job.RealSourceFile := SourceName;
  job.TargetRoot := TargetRoot;
  job.TargetFilename := TargetFilename;
  AddJob(job);
End;

Procedure TryDoMove(index: Integer; TargetRoot, TargetFilename: String);
Var
  TargetFolder, SourceName, TargetName: String;
Begin
  // Sind wir mit der Aktuellen Root verbunden ?
  SourceName := DataBase[index].Root + DataBase[index].Filename;
  If Not FileExists(SourceName) Then Begin
    AddMoveJob(index, TargetRoot, TargetFilename);
    exit;
  End;
  TargetFolder := ExtractFilePath(TargetFilename);
  // Sind wir mit dem Target Root verbunden ?
  If Not ForceDirectories(TargetRoot + TargetFolder) Then Begin
    AddMoveJob(index, TargetRoot, TargetFilename);
    exit;
  End;
  TargetName := TargetRoot + TargetFilename;
  // Scheint Alles Verbunden zu sein, geht das Move via Rename ?
  If Not RenameFile(SourceName, TargetName) Then Begin
    AddMoveJob(index, TargetRoot, TargetFilename);
    exit;
  End;
  // Es hat tatsächlich mit einem "Rename" Geklappt -> DB Updaten und wir sind fertig
  DataBase[index].Filename := TargetFilename;
  DataBase[index].Root := TargetRoot;
  RefreshDynamicContent(index);
  DBChanged := true;
End;

Procedure RefreshDynamicContent(index: integer);
Begin
  If (index < 0) Or (index > high(DataBase)) Then exit;
  DataBase[index].RootLabel := RootFolderToRootLabel(DataBase[index].Root);
  DataBase[index].lRootlabel := lowercase(DataBase[index].RootLabel);
  DataBase[index].lFilename := lowercase(DataBase[index].Filename);
End;

Function PendingJobsDoable: integer;
Var
  i: Integer;
  fp: String;
Begin
  result := 0;
  For i := 0 To high(PendingJobs) Do Begin
    If FileExists(PendingJobs[i].RealSourceFile) Then Begin
      fp := ExtractFileDir(PendingJobs[i].TargetFilename);
      If ForceDirectories(PendingJobs[i].TargetRoot + fp) Then Begin
        inc(result);
      End;
    End;
  End;
End;

Function ExecuteJob(Const aJob: TJob): Boolean;
Var
  fp: String;
Begin
  result := false;
  If Not FileExists(aJob.RealSourceFile) Then exit;
  fp := ExtractFileDir(aJob.TargetFilename);
  If Not ForceDirectories(aJob.TargetRoot + fp) Then exit;
  // Der Einfache Fall, wir können verschieben
  If RenameFile(ajob.RealSourceFile, aJob.TargetRoot + aJob.TargetFilename) Then Begin
    result := true;
    exit;
  End;
  // Der Aufwändige Fall, wir müssen über den CopyCommander2 die Datei verschieben..

  //  ExecuteJob über CopyCommander2 !
End;

End.

