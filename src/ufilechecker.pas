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
  CopyCommanderIP = '127.0.0.1';
  CopyCommanderPort = 8080;

Type

  TJobDetail =
    (
    jMove
    , jDelete
    );

  TIntegers = Array Of Integer;

  { TIntegersHelper }

  TIntegersHelper = Type Helper For Tintegers
    Procedure Sort(descending: Boolean = false);
  End;

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
    DiskSize: Int64; // Größe in Bytes der jeweiligen Festplatte 0 = Unbekannt
    DiskFree: Int64; // -1 = Unbekannt
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
    FileSize: Int64;
  End;

Var
  IniFile: TIniFile = Nil;

  JobTempFolder: String; // Verzeichnis, in dass die Anwendung Jobs zwischenlagert, dass es den User nicht stört ;)
  CopyCommanderCmd: String; // Befehl zum Starten des Copy Commanders
  RootFolders: TRootFolders;
  DataBase: TDataSets = Nil; // !! ACHTUNG !! Database muss immer sortiert sein, also nach jeder Änderung SortFileList !
  Selected: TIntegers = Nil; //
  SelectedCnt: Integer = 0;
  SelectedFileSize: Int64 = 0;
  PendingJobs: Array Of TJob = Nil;
  DBChanged: Boolean = false;

  SearchCharBorder: integer = 3;
  SearchInfo: String = 'Enter at least 3 chars to start search';

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
Function GetRootIndexOf(Const Root: String): integer;

(*
 * Gibt zu einer Root das Passende Label zurück
 * '' = fehler
 *)
Function RootFolderToRootLabel(Const aRoot: String): String;
Function RootLabelToRootFolder(Const aLabel: String): String;

Procedure Nop(); // Just for debugging ;)

Procedure CreateReportFile2(FileName: String;
  Const RenameList: TRenameList; Const CopyList, DelList: TFileList;
  Const Info: TReportInfos);

(*
 * Wenn False, dann passt die Datei nicht auf TargetRoot
 *)
Function TryDoMove(index: Integer; TargetRoot, TargetFilename: String): Boolean;

(*
 * Aktualisiert die Dynamischen Datenfelder des TDataSet Datensatzes von Index
 *)
Procedure RefreshDynamicContent(index: integer);

(*
 * Gibt die Anzahl der Ausführbaren Jobs zurück
 *)
Function PendingJobsDoable: integer;

Function ExecuteJob(Const aJob: TJob): Boolean;

Function JobDetailToString(aDetail: TJobDetail): String;

(*
 * Gibt die Datenbang als TFileList zurück
 *)
Function DataBaseFilesAsTFileList(Const AvailableRoots: TStringArray = Nil): TFileList;

Function StringToDataSet(Const aDataSet: String): TDataSet;

Function FixPathDelims(aFileFolder: String): String;

Function RevertJob(Const aJob: TJob): boolean;

Function GetDiskSize(afolder: String): int64;
Function GetDiskFree(afolder: String): int64;

Implementation

Uses ucopycomandercontroller, dialogs;

Procedure Nop;
Begin

End;

Function GetFileSize(Filename: String): Int64;
Var
  sr: TSearchRec;
Begin
  result := 0;
  // Alle Verzeichnisse
  If FindFirst(Filename, faAnyFile, SR) = 0 Then Begin
    result := sr.Size;
    FindClose(SR);
  End;
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

{ TIntegersHelper }

Procedure TIntegersHelper.Sort(descending: Boolean);
  Procedure Quick(li, re: integer);
  Var
    l, r, h, p: Integer;
  Begin
    If Li < Re Then Begin
      // Achtung, das Pivotelement darf nur einam vor den While schleifen ausgelesen werden, danach nicht mehr !!
      p := self[Trunc((li + re) / 2)]; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        If descending Then Begin
          While self[l] > p Do
            inc(l);
          While self[r] < p Do
            dec(r);
        End
        Else Begin
          While self[l] < p Do
            inc(l);
          While self[r] > p Do
            dec(r);
        End;
        If L <= R Then Begin
          h := self[l];
          self[l] := self[r];
          self[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Begin
  Quick(0, high(self));
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
    RootFolders[i].RootFolder := IncludeTrailingPathDelimiter(IniFile.ReadString('RootFolders', 'Folder' + inttostr(i), ''));
    RootFolders[i].RootLabel := IniFile.ReadString('RootFolders', 'Label' + inttostr(i), '');
    RootFolders[i].DiskSize := IniFile.ReadInt64('RootFolders', 'Size' + inttostr(i), 0);
    RootFolders[i].DiskFree := IniFile.ReadInt64('RootFolders', 'Space' + inttostr(i), -1);
    ecnt := IniFile.ReadInteger('RootFolders', 'ExcludeCount' + inttostr(i), 0);
    setlength(RootFolders[i].Excludes, ecnt);
    For j := 0 To ecnt - 1 Do Begin
      RootFolders[i].Excludes[j] := IniFile.ReadString('RootFolders', 'Exclude' + inttostr(i) + '_' + inttostr(j), '');
    End;
  End;

  CopyCommanderCmd := IniFile.ReadString('CopyCommander', 'CMD',
    'CopyCommander2'{$IFDEF Windows} + '.exe'{$ENDIF} + ' -enablerest -restport=8080');

  SearchCharBorder := inifile.ReadInteger('Search', 'MinCharCount', 3);
  SearchInfo := format('Enter at least %d chars to start search', [SearchCharBorder]);

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
    IniFile.WriteInt64('RootFolders', 'Size' + inttostr(i), RootFolders[i].DiskSize);
    IniFile.WriteInt64('RootFolders', 'Space' + inttostr(i), RootFolders[i].DiskFree);

    IniFile.WriteInteger('RootFolders', 'ExcludeCount' + inttostr(i), length(RootFolders[i].Excludes));
    For j := 0 To high(RootFolders[i].Excludes) Do Begin
      IniFile.WriteString('RootFolders', 'Exclude' + inttostr(i) + '_' + inttostr(j), RootFolders[i].Excludes[j]);
    End;
  End;

  IniFile.WriteString('CopyCommander', 'CMD', CopyCommanderCmd);

  inifile.WriteInteger('Search', 'MinCharCount', SearchCharBorder);
  inifile.UpdateFile;
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

Function FixPathDelims(aFileFolder: String): String;
Begin
  result := aFileFolder;
{$IFDEF Windows}
  result := StringReplace(result, '/', PathDelim, [rfReplaceAll]);
{$ENDIF}
{$IFDEF Linux}
  result := StringReplace(result, '\', PathDelim, [rfReplaceAll]);
{$ENDIF}
End;

Function RevertJob(Const aJob: TJob): boolean;
Var
  sourceFile, sourcefolder: String;
  index: Integer;
Begin
  result := false;
  Case aJob.Job Of
    jMove: Begin
        // Physikalisches Verschieben der Datei
        If Not FileExists(ajob.RealSourceFile) Then exit;
        sourceFile := aJob.SourceRoot + aJob.SourceFile;
        sourcefolder := ExtractFileDir(sourceFile);
        If Not ForceDirectories(sourcefolder) Then exit;
        If Not RenameFile(ajob.RealSourceFile, sourceFile) Then exit;
        result := true;
        // Umtragen in der Datenbank
        index := GetIndexOf(aJob.TargetRoot, aJob.TargetFilename);
        If index = -1 Then exit;
        DataBase[index].Filename := aJob.SourceFile;
        DataBase[index].Root := aJob.SourceRoot;
        RefreshDynamicContent(index);
      End;
  Else Begin
      Raise exception.Create('RevertJob, not implemented for: ' + JobDetailToString(ajob.Job));
    End;
  End;
End;

Procedure LoadDataBase;
Var
  sl: TStringList;
  fn: String;
  i, cnt: Integer;
Begin
  // DataBase
  fn := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'database.db';
  If Not FileExists(fn) Then exit;
  sl := TStringList.Create;
  sl.LoadFromFile(fn);
  setlength(DataBase, sl.Count);
  cnt := 0;
  For i := 0 To sl.Count - 1 Do Begin
    DataBase[cnt] := StringToDataSet(sl[i]);
    If trim(DataBase[cnt].Root) <> '' Then Begin
      RefreshDynamicContent(cnt);
      inc(cnt);
    End;
  End;
  sl.free;
  If length(DataBase) <> cnt Then Begin
    // Eine Beschädigte DB wurde wieder her gestellt ..
    setlength(DataBase, cnt);
    DBChanged := true;
  End
  Else Begin
    DBChanged := false;
  End;
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
    PendingJobs[i].FileSize := IniFile.ReadInt64('Jobs', 'FileSize' + IntToStr(i), 0);
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
    IniFile.WriteInt64('Jobs', 'FileSize' + IntToStr(i), PendingJobs[i].FileSize);
  End;
  inifile.UpdateFile;
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

Function GetRootIndexOf(Const Root: String): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(RootFolders) Do Begin
    If RootFolders[i].RootFolder = Root Then Begin
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

Function RootLabelToRootFolder(Const aLabel: String): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 0 To high(RootFolders) Do Begin
    If aLabel = RootFolders[i].RootLabel Then Begin
      result := RootFolders[i].RootFolder;
      exit;
    End;
  End;
End;

Function AddJob(Const aJob: TJob): Boolean;
Var
  Index: Integer;
Begin
  result := false;
  // TODO: ggf. noch irgendwelche Checks ?
  setlength(PendingJobs, high(PendingJobs) + 2);
  PendingJobs[high(PendingJobs)] := aJob;
  (*
   * Jeder Job der Angenommen wird, wird auf der Datenbank schon "durchgeführt"
   *)
  Index := GetIndexOf(ajob.SourceRoot, aJob.SourceFile);
  If index = -1 Then Begin
    Showmessage('Error: AddJob, can not find source: "' + aJob.SourceFile + '" in database.');
    exit;
  End;
  Case aJob.Job Of
    jMove: Begin
        DataBase[index].Root := aJob.TargetRoot;
        DataBase[index].Filename := aJob.TargetFilename;
        RefreshDynamicContent(index);
        DBChanged := true;
        result := true;
      End;
  Else Begin
      Raise exception.create('Error: AddJob, missing implementation for: ' + JobDetailToString(ajob.Job));
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
    For i := 0 To high(DelList) Do Begin
      sl.add(
        '"' + DelList[i].Root + DelList[i].FileName + '"'
        );
    End;
  End;
  sl.SaveToFile(FileName);
  sl.free;
End;

Function AddMoveJob(index: Integer; TargetRoot, TargetFilename: String): Boolean;
Var
  TmpTarget, SourceName: String;
  job: TJob;
  Rootindex: Integer;
Begin
  result := false;
  Rootindex := GetRootIndexOf(TargetRoot);
  If Rootindex = -1 Then Begin
    showmessage('Error, invalid target root: ' + TargetRoot + ' for ' + ExtractFileName(TargetFilename));
    exit;
  End;
  If RootFolders[Rootindex].DiskFree <> -1 Then Begin
    If RootFolders[Rootindex].DiskFree < DataBase[index].Size Then Begin
      showmessage('Error, ' + ExtractFileName(TargetFilename) + ' will not have enough space on ' + RootFolders[Rootindex].RootLabel);
      exit;
    End;
  End;
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
  job.FileSize := DataBase[index].Size;
  result := AddJob(job);
  If result Then Begin
    RootFolders[Rootindex].DiskFree := RootFolders[Rootindex].DiskFree - job.FileSize;
  End;
End;

Function TryDoMove(index: Integer; TargetRoot, TargetFilename: String): Boolean;
Var
  TargetFolder, SourceName, TargetName: String;
Begin
  result := false;
  // Sind wir mit der Aktuellen Root verbunden ?
  SourceName := DataBase[index].Root + DataBase[index].Filename;
  If Not FileExists(SourceName) Then Begin
    result := AddMoveJob(index, TargetRoot, TargetFilename);
    exit;
  End;
  TargetFolder := ExtractFilePath(TargetFilename);
  // Sind wir mit dem Target Root verbunden ?
  If Not ForceDirectories(TargetRoot + TargetFolder) Then Begin
    result := AddMoveJob(index, TargetRoot, TargetFilename);
    exit;
  End;
  TargetName := TargetRoot + TargetFilename;
  // Scheint Alles Verbunden zu sein, geht das Move via Rename ?
  If Not RenameFile(SourceName, TargetName) Then Begin
    result := AddMoveJob(index, TargetRoot, TargetFilename);
    exit;
  End;
  // Es hat tatsächlich mit einem "Rename" Geklappt -> DB Updaten und wir sind fertig
  DataBase[index].Filename := TargetFilename;
  DataBase[index].Root := TargetRoot;
  RefreshDynamicContent(index);
  DBChanged := true;
  result := true;
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
  index, RootIndex: Integer;
Begin
  result := false;
  If Not FileExists(aJob.RealSourceFile) Then exit;
  fp := ExtractFileDir(aJob.TargetFilename);
  If Not ForceDirectories(aJob.TargetRoot + fp) Then exit;
  // Der Einfacher Fall, wir können verschieben
  If RenameFile(ajob.RealSourceFile, aJob.TargetRoot + aJob.TargetFilename) Then Begin
    result := true;
    exit;
  End;
  // Past die Datei überhaupt auf das Zielverzeichnis ?
  RootIndex := GetRootIndexOf(aJob.TargetRoot);
  If RootIndex = -1 Then Begin
    showmessage('Error, invalid target root for: ' + ExtractFileName(aJob.TargetFilename));
    exit;
  End;
  // Sollte eigentlich nicht vorkommen, schadet aber auch nicht..
  If RootFolders[RootIndex].DiskFree = 0 Then RootFolders[RootIndex].DiskFree := GetDiskFree(RootFolders[RootIndex].RootFolder);
  // Prüfen ob die Datei überhaupt "Platz" hat ;)
  If aJob.FileSize > RootFolders[RootIndex].DiskFree Then Begin
    showmessage('Error, not enough free diskspace on ' + RootFolders[RootIndex].RootLabel + ' to store: ' + ExtractFileName(aJob.TargetFilename));
    exit;
  End;
  // 1. Check if Copy Commander is Running
  If Not isCopyCommanderRestAPIRunning(CopyCommanderIP, CopyCommanderPort) Then Begin
    // 1.5 Start Copy Commander
    If Not StartCopyCommander(CopyCommanderCmd) Then Begin
      showmessage('Error, unable to start copycommander2');
      exit;
    End;
    delay(2000); // TODO: gibt es hier einen besseren Weg als zu warten ?
    // 1.75 Check if Copy Commander is Running -> Not Error
    If Not isCopyCommanderRestAPIRunning(CopyCommanderIP, CopyCommanderPort) Then Begin
      // Todo: Fehlermeldung ?
      showmessage('Error, copycommander2 does not respond on API');
      exit;
    End;
  End;
  // 2. Copy Commander läuft, den Job Übergeben ;)
  Case aJob.Job Of
    jMove: Begin
        // Der Copy Commander hat behauptet, dass es geklappt hat
        If Not CopyCommanderMoveFile(aJob.RealSourceFile, aJob.TargetRoot + aJob.TargetFilename) Then exit;
        // Das Prüfen wir hier noch mal Explizit
        If Not FileExists(aJob.TargetRoot + aJob.TargetFilename) Then exit;
        // Explizit auch die Dateigröße Prüfen !
        index := GetIndexOf(aJob.TargetRoot, aJob.TargetFilename);
        If index = -1 Then exit;
        If GetFileSize(aJob.TargetRoot + aJob.TargetFilename) <> DataBase[index].Size Then Begin
          Showmessage('Error, during file moving, please check validity of file: ' + aJob.TargetRoot + aJob.TargetFilename);
          exit;
        End;
        result := true;
      End;
  Else Begin
      showmessage('Error, ' + JobDetailToString(aJob.Job) + ' not implemented in ufilechecker.ExecuteJob.');
      exit;
    End;
  End;
End;

Function DataBaseFilesAsTFileList(Const AvailableRoots: TStringArray
  ): TFileList;
Var
  cnt, i, j: Integer;
  Availables: TStringArray;
  found: Boolean;
Begin
  (*
   * Nur die mit Aktiven "Roots" übernehmen
   *)
  result := Nil;
  Availables := Nil;
  setlength(Availables, length(AvailableRoots));
  For i := 0 To high(AvailableRoots) Do Begin
    Availables[i] := LowerCase(AvailableRoots[i]);
  End;
  setlength(result, length(DataBase));
  cnt := 0;
  For i := 0 To high(DataBase) Do Begin
    found := Not assigned(Availables);
    For j := 0 To high(Availables) Do Begin
      If Availables[j] = DataBase[i].lRootlabel Then Begin
        found := true;
        break;
      End;
    End;
    If found Then Begin
      result[cnt].FileName := DataBase[i].Root + DataBase[i].Filename;
      result[cnt].FileSize := DataBase[i].Size;
      result[cnt].Root := DataBase[i].Root;
      inc(cnt);
    End;
  End;
  setlength(result, cnt);
End;

Function GetDiskSize(afolder: String): int64;
Var
{$IFDEF Windows}
  currP: String;
{$ENDIF}
  DiskNum: integer;
Begin
{$IFDEF Windows}
  DiskNum := 0;
  currP := GetCurrentDirUTF8;
  SetCurrentDirUTF8(afolder);
{$ELSE}
  DiskNum := AddDisk(afolder);
{$ENDIF}
  result := DiskSize(DiskNum);
{$IFDEF Windows}
  SetCurrentDirUTF8(currP);
{$ENDIF}
End;

Function GetDiskFree(afolder: String): int64;
Var
{$IFDEF Windows}
  currP: String;
{$ENDIF}
  DiskNum: integer;
Begin
{$IFDEF Windows}
  DiskNum := 0;
  currP := GetCurrentDirUTF8;
  SetCurrentDirUTF8(afolder);
{$ELSE}
  DiskNum := AddDisk(afolder);
{$ENDIF}
  result := DiskFree(DiskNum);
{$IFDEF Windows}
  SetCurrentDirUTF8(currP);
{$ENDIF}
End;


End.

