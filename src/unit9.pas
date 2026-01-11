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
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Procedure FormCreate(Sender: TObject);
    Procedure SpeedButton4Click(Sender: TObject);
    Procedure SpeedButton5Click(Sender: TObject);
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
  If Application.MessageBox('There are no further checks did you merge and download the database before ?', 'Warning', MB_ICONQUESTION Or MB_YESNO) = ID_YES Then Begin
    // Sicherstellen, dass die Datenbank auch gespeichert ist.
    StoreDataBase;
    If Not Login(
      IniFile.ReadString('Server', 'URL', 'https://127.0.0.1'),
      IniFile.ReadString('Server', 'Port', '8443'),
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
End;

Procedure TForm9.SpeedButton5Click(Sender: TObject);
Var
  m: TMemoryStream;
  sl: TStringList;
  LoadedDataBaseFiles: TFileList;
  i: Integer;
  d: TDataSet;
Begin
  // Download
  If Not assigned(RootFolders) Then Begin
    showmessage('Error, you have to have at least one root folder defined.');
    exit;
  End;
  StoreDataBase;
  If Not Login(
    IniFile.ReadString('Server', 'URL', 'https://127.0.0.1'),
    IniFile.ReadString('Server', 'Port', '8443'),
    IniFile.ReadString('Server', 'Username', ''),
    IniFile.ReadString('Server', 'Password', '')
    ) Then Begin
    showmessage('Error, unable to log in, abort.');
    Logout;
    exit;
  End;
  m := DownloadDB();
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
    LoadedDataBaseFiles[i].FileName := d.Filename;
    LoadedDataBaseFiles[i].FileSize := d.Size;
    LoadedDataBaseFiles[i].Root := RootFolders[0].RootFolder;
  End;
  sl.free;
  form3.GenerateResultsWith(DataBaseFilesAsTFileList(Nil), LoadedDataBaseFiles);
  form3.ShowModal;
  ModalResult := mrOK
End;

End.

