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
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ufilechecker, usslconnector;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button10Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
  private

    fRootFolders: TRootFolders;
  public
    Procedure SettingsToLCL;
    Procedure LCLToSettings;

  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Settings';
  Edit1.text := '';
  Edit2.text := '';
  Edit3.text := '';
  Edit4.text := '';
  Edit5.text := '';
  Edit6.text := '127.0.0.1';
  Edit7.text := '8081';
End;

Procedure TForm2.ListBox1Click(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    Edit1.Text := fRootFolders[ListBox1.ItemIndex].RootFolder;
    Edit2.Text := fRootFolders[ListBox1.ItemIndex].RootLabel;
    s := '';
    For i := 0 To high(fRootFolders[ListBox1.ItemIndex].Excludes) Do Begin
      If i <> 0 Then s := s + ';';
      s := s + fRootFolders[ListBox1.ItemIndex].Excludes[i];
    End;
    Edit5.Text := s;
  End;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  If SelectDirectoryDialog1.Execute Then Begin
    edit1.text := IncludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);
  End;
End;

Procedure TForm2.Button10Click(Sender: TObject);
Begin
  If Login(Edit6.text, edit7.text, edit10.text, edit9.text) Then Begin
    showmessage('Login succeed.');
  End
  Else Begin
    showmessage('Testlogin failed.');
  End;
  Logout;
End;

Procedure TForm2.Button11Click(Sender: TObject);
Var
  OldPW, NewPW: String;
Begin
  OldPW := PasswordBox('Old password', 'Please enter old password');
  If OldPW = '' Then exit;
  NewPW := PasswordBox('New password', 'Please enter New password');
  If NewPW = '' Then exit;
  If OldPW = NewPW Then Begin
    showmessage('Error, no change.');
    exit;
  End;
  If trim(NewPW) = '' Then Begin
    showmessage('Error, new password is only space empty.');
    exit;
  End;
  If Login(Edit6.text, edit7.text, edit10.text, OldPW) Then Begin
    If SetPassword(NewPW) Then Begin
      edit9.text := NewPW;
      showmessage('Login succeed.');
    End
    Else Begin
      showmessage('Failed to set new password.');
    End;
  End
  Else Begin
    showmessage('Login failed.');
  End;
  Logout;
End;

Procedure TForm2.Button4Click(Sender: TObject);
Var
  sa: TStringArray;
  i, cnt: Integer;
  b: Boolean;
Begin
  // TODO: Doppelte Checken, Gültigkeit Checken
  If trim(edit2.text) = '' Then Begin
    ShowMessage('Error, empty labels are not allowed.');
    exit;
  End;
  b := (pos(Separator, edit1.text) <> 0) Or
    (pos(Separator, edit2.text) <> 0) Or
    (pos(Separator, edit5.text) <> 0) Or
    (pos(Divider, edit1.text) <> 0) Or
    (pos(Divider, edit2.text) <> 0) Or
    (pos(':', edit2.text) <> 0);
  If b Then Begin
    Showmessage('Tab or ";" or ":" are not allowed to be used.');
    exit;
  End;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If edit2.Text = ListBox1.Items[i] Then Begin
      Showmessage('Error, all labels need to be disjoint.');
      exit;
    End;
  End;
  setlength(fRootFolders, high(fRootFolders) + 2);
  fRootFolders[high(fRootFolders)].RootFolder := Edit1.Text;
  fRootFolders[high(fRootFolders)].RootLabel := Edit2.Text;
  sa := trim(Edit5.text).Split(';');
  cnt := 0;
  For i := 0 To high(sa) Do Begin
    If trim(sa[i]) <> '' Then inc(cnt);
  End;
  setlength(fRootFolders[high(fRootFolders)].Excludes, cnt);
  cnt := 0;
  For i := 0 To high(sa) Do Begin
    If trim(sa[i]) <> '' Then Begin
      fRootFolders[high(fRootFolders)].Excludes[cnt] := sa[i];
      inc(cnt);
    End;
  End;
  ListBox1.Items.Add(edit2.text);
End;

Procedure TForm2.Button5Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Del
  If ListBox1.ItemIndex = -1 Then exit;
  For i := ListBox1.ItemIndex To high(fRootFolders) - 1 Do Begin
    setlength(fRootFolders[i].Excludes, 0);
    fRootFolders[i] := fRootFolders[i + 1];
  End;
  setlength(fRootFolders, high(fRootFolders));
  ListBox1.Items.Delete(ListBox1.ItemIndex);
End;

Procedure TForm2.Button6Click(Sender: TObject);
Begin
  If SelectDirectoryDialog1.Execute Then Begin
    edit3.text := IncludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);
  End;
End;

Procedure TForm2.Button7Click(Sender: TObject);
Begin
  If ListBox2.ItemIndex <> -1 Then Begin
    ListBox2.Items.Delete(ListBox2.ItemIndex);
  End;
End;

Procedure TForm2.Button8Click(Sender: TObject);
Var
  tmp: TRootFolder;
Begin
  // Move Root 1 up
  If ListBox1.ItemIndex < 1 Then exit;
  tmp := fRootFolders[ListBox1.ItemIndex];
  fRootFolders[ListBox1.ItemIndex] := fRootFolders[ListBox1.ItemIndex - 1];
  fRootFolders[ListBox1.ItemIndex - 1] := tmp;
  ListBox1.Items.Exchange(ListBox1.ItemIndex, ListBox1.ItemIndex - 1);
  ListBox1.ItemIndex := ListBox1.ItemIndex - 1;
End;

Procedure TForm2.Button9Click(Sender: TObject);
Var
  tmp: TRootFolder;
Begin
  // Move Root 1 down
  If ListBox1.ItemIndex > ListBox1.Count - 2 Then exit;
  tmp := fRootFolders[ListBox1.ItemIndex];
  fRootFolders[ListBox1.ItemIndex] := fRootFolders[ListBox1.ItemIndex + 1];
  fRootFolders[ListBox1.ItemIndex + 1] := tmp;
  ListBox1.Items.Exchange(ListBox1.ItemIndex, ListBox1.ItemIndex + 1);
  ListBox1.ItemIndex := ListBox1.ItemIndex + 1;
End;

Procedure TForm2.SettingsToLCL;
Var
  i, j: Integer;
  s: String;
Begin
  edit3.text := JobTempFolder;

  ListBox1.Clear;
  setlength(fRootFolders, length(RootFolders));
  For i := 0 To high(RootFolders) Do Begin
    fRootFolders[i].RootFolder := RootFolders[i].RootFolder;
    fRootFolders[i].RootLabel := RootFolders[i].RootLabel;
    setlength(fRootFolders[i].Excludes, length(RootFolders[i].Excludes));
    For j := 0 To high(RootFolders[i].Excludes) Do Begin
      fRootFolders[i].Excludes[j] := RootFolders[i].Excludes[j];
    End;
    ListBox1.Items.Add(RootFolders[i].RootLabel);
  End;

  edit4.text := CopyCommanderCmd;

  listbox2.clear;
  j := IniFile.ReadInteger('Queries', 'Count', 0);
  For i := 0 To j - 1 Do Begin
    s := IniFile.ReadString('Queries', 'Query' + inttostr(i), '');
    If trim(s) <> '' Then
      listbox2.Items.Add(s);
  End;
  CheckBox1.Checked := IniFile.ReadBool('Search', 'AlwaysJumpToLast', false);
  edit8.text := inttostr(SearchCharBorder);

  edit6.text := IniFile.ReadString('Server', 'URL', 'https://127.0.0.1');
  edit7.text := IniFile.ReadString('Server', 'Port', '8443');
  edit10.text := IniFile.ReadString('Server', 'Username', '');
  edit9.text := IniFile.ReadString('Server', 'Password', '');
End;

Procedure TForm2.LCLToSettings;
Var
  i, j: Integer;
Begin
  JobTempFolder := IncludeTrailingPathDelimiter(edit3.text);

  setlength(RootFolders, length(fRootFolders));
  For i := 0 To high(fRootFolders) Do Begin
    RootFolders[i].RootFolder := fRootFolders[i].RootFolder;
    RootFolders[i].RootLabel := fRootFolders[i].RootLabel;
    setlength(RootFolders[i].Excludes, length(fRootFolders[i].Excludes));
    For j := 0 To high(fRootFolders[i].Excludes) Do Begin
      RootFolders[i].Excludes[j] := fRootFolders[i].Excludes[j];
    End;
  End;
  CopyCommanderCmd := edit4.text;

  IniFile.ReadInteger('Queries', 'Count', listbox2.Items.Count);
  For i := 0 To listbox2.Items.Count - 1 Do Begin
    IniFile.WriteString('Queries', 'Query' + inttostr(i), listbox2.Items[i]);
  End;

  IniFile.WriteBool('Search', 'AlwaysJumpToLast', CheckBox1.Checked);
  SearchCharBorder := strtointdef(edit8.text, 3);
  IniFile.WriteInteger('Search', 'MinCharCount', SearchCharBorder);
  SearchInfo := format('Enter at least %d chars to start search', [SearchCharBorder]);

  IniFile.WriteInteger('Queries', 'Count', listbox2.Items.Count);
  For i := 0 To listbox2.Items.Count - 1 Do Begin
    IniFile.WriteString('Queries', 'Query' + inttostr(i), listbox2.Items[i]);
  End;

  IniFile.WriteString('Server', 'URL', edit6.text);
  IniFile.WriteString('Server', 'Port', edit7.text);
  IniFile.WriteString('Server', 'Username', edit10.text);
  IniFile.WriteString('Server', 'Password', edit9.text);
End;

End.

