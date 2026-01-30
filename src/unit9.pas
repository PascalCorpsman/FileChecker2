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
Unit Unit9;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

Type

  { TForm9 }

  TForm9 = Class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Procedure FormCreate(Sender: TObject);
    Procedure SpeedButton4Click(Sender: TObject);
    Procedure SpeedButton5Click(Sender: TObject);
    Procedure SpeedButton6Click(Sender: TObject);
    Procedure SpeedButton7Click(Sender: TObject);
  private

  public

  End;

Var
  Form9: TForm9;

Implementation

{$R *.lfm}

Uses LCLType, usslconnector, udirsync, ufilechecker, unit3;

{ TForm9 }

Procedure TForm9.FormCreate(Sender: TObject);
Begin
  caption := 'Server sync';
End;

Procedure TForm9.SpeedButton4Click(Sender: TObject);
Var
  m: TMemoryStream;
  fn: String;
Begin
  // Upload
  StoreDataBase; // Sicherstellen, dass die Datenbank auch gespeichert ist.
  If Not Login(
    IniFile.ReadString('Server', 'URL', 'https://127.0.0.1'),
    IniFile.ReadString('Server', 'Port', '8443'),
    ClientID,
    IniFile.ReadString('Server', 'Username', ''),
    IniFile.ReadString('Server', 'Password', '')
    ) Then Begin
    showmessage('Error, unable to log in, abort.');
    Logout;
    exit;
  End;
  m := TMemoryStream.Create;
  fn := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'database.db';
  m.LoadFromFile(fn);
  m.Position := 0;
  If SendDB(m) Then Begin
    showmessage('Successfully uploaded database.');
  End
  Else Begin
    showmessage('Failed to upload database.');
  End;
  m.free;
  Logout;
End;

Procedure TForm9.SpeedButton5Click(Sender: TObject);
Var
  m: TMemoryStream;
  sl: TStringList;
  LoadedDataBaseFiles: TFileList;
  i: Integer;
  d: TDataSet;
  SelectedDataBase: String;
Begin
  // Download
  If Not assigned(RootFolders) Then Begin
    showmessage('Error, you have to have at least one root folder defined.');
    exit;
  End;
  If PendingJobs <> Nil Then Begin
    If ID_NO = application.MessageBox('Downloading and syncing will create false positives if there are unfinished jobs, press no if you want to abort.', 'Warning', MB_YESNO Or MB_ICONWARNING) Then Begin
      exit;
    End;
  End;
  StoreDataBase;
  If Not Login(
    IniFile.ReadString('Server', 'URL', 'https://127.0.0.1'),
    IniFile.ReadString('Server', 'Port', '8443'),
    ClientID,
    IniFile.ReadString('Server', 'Username', ''),
    IniFile.ReadString('Server', 'Password', '')
    ) Then Begin
    showmessage('Error, unable to log in, abort.');
    Logout;
    exit;
  End;
  m := RequestaDBAndDownloadIt(SelectedDataBase);
  If Not assigned(m) Then Begin
    showmessage('Error, unable to download database.');
    exit;
  End;
  sl := TStringList.Create;
  sl.LoadFromStream(m);
  m.free;
  LoadedDataBaseFiles := Nil;
  setlength(LoadedDataBaseFiles, sl.Count);
  For i := 0 To sl.Count - 1 Do Begin
    d := StringToDataSet(sl[i]);
    LoadedDataBaseFiles[i].FileName := RootFolders[0].RootFolder + FixPathDelims(d.Filename);
    LoadedDataBaseFiles[i].FileSize := d.Size;
    LoadedDataBaseFiles[i].Root := RootFolders[0].RootFolder;
  End;
  sl.free;
  form3.GenerateResultsWith(SelectedDataBase, DataBaseFilesAsTFileList(Nil), LoadedDataBaseFiles);
  form3.ShowModal;
  ModalResult := mrOK
End;

Procedure TForm9.SpeedButton6Click(Sender: TObject);
Begin
  // Export Database as .compressed file
  If SaveDialog1.Execute Then Begin
    If SaveCompressedDatabaseAs(SaveDialog1.FileName) Then Begin
      showmessage('Done.');
    End
    Else Begin
      showmessage('Error, unable to export.');
    End;
  End;
End;

Procedure TForm9.SpeedButton7Click(Sender: TObject);
Var
  sl: TStringList;
Begin
  // Import Compressed Database
  If OpenDialog1.Execute Then Begin
    sl := CompressedDatabaseToStringlist(OpenDialog1.FileName);
    If Not assigned(sl) Then Begin
      showmessage('Error, during uncompressing.');
      exit;
    End;
    //    sl.SaveToFile('Database_Uncompressed.txt');
    showmessage('Todo:, db is loaded, but not imported yet..');
    sl.free;
  End;
End;

End.

