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
Program server;

{$MODE objfpc}{$H+}

{.$DEFINE Debug}

Uses
  SysUtils, Classes, Base64, FPHttpApp, HTTPRoute, HTTPDefs, OpenSslSockets,
  StrUtils, Types, IniFiles, CustApp
  , DCPrijndael, DCPsha256, DCPcrypt2
  ;

Const
{$IFDEF Debug}
  SeedSize = 16;
  TokenSize = 16;
{$ELSE}
  SeedSize = 2048;
  TokenSize = 2048;
{$ENDIF}

  (* Rechte sind Bitkodiert, nicht jeder User darf alles *)
  RightToSetDB = 1;
  RightToReloadSettings = 2;
  RightToEditUsers = 4;

  delta10min = 1 / (24 * 6); // TDateTime hat 1 = 1 Tag 0.5 = 12H

Type
  TUser = Record
    Username: String;
    Password: String;
    LastChallenge: Array[0..SeedSize - 1] Of byte;
    Token: String;
    TokenCreationTime: TDateTime;
    Rights: Integer;
  End;

  TUsers = Array Of TUser;

Var
  Users: TUsers;
  ServerPort: integer;
  CertificateFileName, PrivateKeyFileName: String;
  KeepRunning: Boolean;

Procedure Log(value: String);
Begin
  writeln(FormatDateTime('YYYY.MM.DD HH:MM:SS ', now) + value);
End;

Procedure LoadSettings;
Var
  ini: TIniFile;
  i: Integer;
Begin
  users := Nil;
  ini := TIniFile.Create('settings.ini');
  ServerPort := ini.ReadInteger('Server', 'Port', 8443);
  CertificateFileName := ini.ReadString('Server', 'Certificate', 'server.crt');
  PrivateKeyFileName := ini.ReadString('Server', 'PrivateKey', 'server.key');
  setlength(users, ini.ReadInteger('User', 'Count', 0));
  For i := 0 To high(Users) Do Begin
    users[i].Username := ini.ReadString('User', 'Name' + inttostr(i), '');
    users[i].Password := ini.ReadString('User', 'PW' + inttostr(i), '');
    users[i].Rights := ini.ReadInteger('User', 'Rights' + inttostr(i), 0);
    fillchar(users[i].LastChallenge[0], sizeof(users[i].LastChallenge[0]), 0);
    Log('  adduser: ' + users[i].Username);
  End;
  ini.free;
End;

(*
 * -1 = user nicht bekannt
 *)

Function GetUserIndex(Username: String): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(Users) Do Begin
    If users[i].Username = Username Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function GetIDFromStringArray(ID: String; Const StringArray: TStringDynArray): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 0 To length(StringArray) Div 2 - 1 Do Begin
    If StringArray[2 * i] = id Then Begin
      result := StringArray[2 * i + 1];
      break;
    End;
  End;
End;

Procedure SendEmptyResponce(Const aResponse: TResponse; Code: integer);
Begin
  aResponse.Code := Code;
  aResponse.ContentLength := 0;
  aResponse.Content := '';
  aResponse.SendContent;
End;

(*
 * Gibt den Userindex zurück, oder -1, im Fehlerfall
 *)

Function UserExistsAndIsAllowed(aRequest: TRequest; aResponse: TResponse): Integer;
Var
  StringArray: TStringDynArray;
  Username, Token: String;
Begin
  result := -1;
  StringArray := SplitString(aRequest.Authorization, ' ');
  // Kennen wir den User ?
  Username := GetIDFromStringArray('Username', StringArray);
  If Username = '' Then Begin
    SendEmptyResponce(aResponse, 401);
    exit;
  End;
  Username := DecodeStringBase64(Username);
  result := GetUserIndex(Username);
  If result = -1 Then Begin
    SendEmptyResponce(aResponse, 401);
    Exit;
  End;
  // Timeout
  If now > Users[result].TokenCreationTime + delta10min Then Begin
    SendEmptyResponce(aResponse, 401);
    result := -1;
    Exit;
  End;
  // Ist der User berechtigt ?
  Token := GetIDFromStringArray('Bearer', StringArray);
  If token <> Users[result].Token Then Begin
    SendEmptyResponce(aResponse, 401);
    result := -1;
    Exit;
  End;
End;

Procedure getDB(aRequest: TRequest; aResponse: TResponse);
Var
  Filestream: TFileStream;
  dbName, RawBody, EncodedBody, DatabaseFileName: String;
  UserIndex, i: Integer;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  dbName := DecodeStringBase64(aRequest.Content);
  // Prüfen ob der dbName tatsächlich auf eine Datenbank zeigt oder irgendwo hin
  // Nur Datenbanken zulassen !
  DatabaseFileName := '';
  For i := 0 To high(Users) Do Begin
    If (Users[i].Username) = dbName Then Begin
      DatabaseFileName := 'db' + dbName + '.db';
      break;
    End;
  End;
  If (DatabaseFileName <> '') And (FileExists(DatabaseFileName)) Then Begin
    Log('Send ' + DatabaseFileName + ' to ' + Users[UserIndex].Username);
    Filestream := TFileStream.Create(DatabaseFileName, fmOpenRead Or fmShareDenyWrite);
    RawBody := '';
    Try
      SetLength(RawBody, Filestream.Size);
      Filestream.Read(RawBody[1], Filestream.Size);
    Finally
      Filestream.Free;
    End;
    EncodedBody := EncodeStringBase64(RawBody);
    aResponse.Code := 200;
    aResponse.ContentLength := EncodedBody.Length;
    aResponse.Content := EncodedBody;
    aResponse.SendContent;
  End
  Else Begin
    Log('No db present, send 404');
    SendEmptyResponce(aResponse, 404);
  End;
End;

Procedure getDBList(aRequest: TRequest; aResponse: TResponse);
Var
  EncodedBody: String;
  UserIndex, i: Integer;
  sl: TStringList;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  sl := TStringList.Create;
  For i := 0 To high(Users) Do Begin
    If FileExists('db' + Users[i].Username + '.db') Then Begin
      sl.Add(Users[i].Username);
    End;
  End;
  EncodedBody := EncodeStringBase64(sl.text);
  Log(Users[i].Username + ' requested DB list, found ' + IntToStr(sl.count));
  sl.free;
  aResponse.Code := 200;
  aResponse.ContentLength := EncodedBody.Length;
  aResponse.Content := EncodedBody;
  aResponse.SendContent;
End;

Procedure ReloadSettings(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex: Integer;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Users[UserIndex].Rights And RightToReloadSettings = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  Log(Users[UserIndex].Username + ' triggert reload settings.');
  LoadSettings;
  SendEmptyResponce(aResponse, 200);
End;

Procedure setDB(aRequest: TRequest; aResponse: TResponse);
Var
  Filestream: TFileStream;
  dbString: String;
  UserIndex: Integer;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Users[UserIndex].Rights And RightToSetDB = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  dbString := DecodeStringBase64(aRequest.Content);
  // Leere Datenbanken wollen wir nicht
  If dbString = '' Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  Try
    Filestream := TFileStream.Create('db' + Users[UserIndex].Username + '.db', fmCreate Or fmOpenWrite);
    Filestream.Write(dbString[1], length(dbString));
    Filestream.free;
    Log('Received db from ' + Users[UserIndex].Username);
  Except
    SendEmptyResponce(aResponse, 500);
    Exit;
  End;
  SendEmptyResponce(aResponse, 200);
End;

Procedure setPassword(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex: integer;
  NewPW: String;
  ini: TIniFile;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  NewPW := DecodeStringBase64(aRequest.Content);
  Log('Set new password for: ' + Users[UserIndex].Username);
  // Sofort übernehmen
  Users[UserIndex].Password := NewPW;
  // Und für den Reboot ;)
  ini := TIniFile.Create('settings.ini');
  ini.WriteString('User', 'PW' + inttostr(UserIndex), NewPW);
  ini.free;
  SendEmptyResponce(aResponse, 200);
End;

Procedure getuserlist(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex, i: integer;
  sl: TStringList;
  m: TMemoryStream;
  RawBody, EncodedBody: String;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Users[UserIndex].Rights And RightToEditUsers = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  sl := TStringList.Create;
  For i := 0 To high(Users) Do Begin
    sl.add(Format('%d;%s', [Users[i].Rights, Users[i].Username]));
  End;
  m := TMemoryStream.Create;
  sl.SaveToStream(m);
  sl.free;
  m.position := 0;
  RawBody := '';
  setlength(RawBody, m.Size);
  m.Read(RawBody[1], m.Size);
  m.free;
  EncodedBody := EncodeStringBase64(RawBody);
  Log(Users[UserIndex].Username + ' requested userlist');
  aResponse.Code := 200;
  aResponse.ContentLength := EncodedBody.Length;
  aResponse.Content := EncodedBody;
  aResponse.SendContent;
End;

Procedure setuserright(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex: integer;
  Content, UserName: String;
  sa: TStringArray;
  UserRight, i: Integer;
  ini: TIniFile;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Users[UserIndex].Rights And RightToEditUsers = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  Content := DecodeStringBase64(aRequest.Content);
  sa := Content.Split(';');
  If length(sa) < 2 Then Begin
    SendEmptyResponce(aResponse, 404);
    Exit;
  End;
  UserName := sa[0];
  UserRight := strtointdef(sa[1], 0);
  For i := 0 To high(Users) Do Begin
    If Users[i].Username = Username Then Begin
      Log('Set new rights for: ' + Username);
      // Sofort übernehmen
      Users[i].Rights := UserRight;
      // Und für den Reboot ;)
      ini := TIniFile.Create('settings.ini');
      ini.WriteInteger('User', 'Rights' + inttostr(i), UserRight);
      ini.free;
      SendEmptyResponce(aResponse, 200);
      exit;
    End;
  End;
  // User nicht gefunden
  SendEmptyResponce(aResponse, 404);
End;

Procedure deluser(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex, i, j: integer;
  Username: String;
  ini: TIniFile;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Users[UserIndex].Rights And RightToEditUsers = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  Username := DecodeStringBase64(aRequest.Content);
  For i := 0 To high(Users) Do Begin
    If Users[i].Username = Username Then Begin
      // Sofort Übernehmen
      For j := i To high(Users) - 1 Do Begin
        Users[j] := Users[j + 1];
      End;
      setlength(Users, high(Users));
      // Und für den Reboot ;)
      ini := TIniFile.Create('settings.ini');
      For j := i To high(Users) Do Begin
        ini.WriteString('User', 'Name' + inttostr(j), users[j].Username);
        ini.WriteString('User', 'PW' + inttostr(j), users[j].Password);
        ini.WriteInteger('User', 'Rights' + inttostr(j), users[j].Rights);
      End;
      ini.WriteInteger('User', 'Count', length(Users));
      ini.free;
      Log('Del user: ' + Username);
      SendEmptyResponce(aResponse, 200);
      exit;
    End;
  End;
  // User nicht gefunden
  SendEmptyResponce(aResponse, 404);
End;

Procedure adduser(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex, i: integer;
  Username, Content, Password: String;
  ini: TIniFile;
  Rights: Integer;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Users[UserIndex].Rights And RightToEditUsers = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  Content := DecodeStringBase64(aRequest.Content);
  Rights := strtointdef(copy(Content, 1, pos(';', Content) - 1), 0);
  delete(Content, 1, pos(';', Content));
  Username := copy(Content, 1, pos(';', Content) - 1);
  delete(Content, 1, pos(';', Content));
  Password := Content;
  For i := 0 To high(Users) Do Begin
    If Users[i].Username = Username Then Begin // User existiert schon -> Raus
      SendEmptyResponce(aResponse, 404);
      exit;
    End;
  End;
  // Sofort Übernehmen
  setlength(Users, high(Users) + 2);
  users[High(Users)].Username := Username;
  users[High(Users)].Rights := Rights;
  users[High(Users)].Password := Password;
  fillchar(users[High(Users)].LastChallenge[0], sizeof(users[High(Users)].LastChallenge[0]), 0);
  // Und für den Reboot ;)
  ini := TIniFile.Create('settings.ini');
  ini.WriteString('User', 'Name' + inttostr(High(Users)), users[high(Users)].Username);
  ini.WriteString('User', 'PW' + inttostr(High(Users)), users[high(Users)].Password);
  ini.WriteInteger('User', 'Rights' + inttostr(High(Users)), users[high(Users)].Rights);
  ini.WriteInteger('User', 'Count', length(Users));
  ini.free;
  Log('  adduser: ' + Username);
  SendEmptyResponce(aResponse, 200);
End;

Procedure getchallenge(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex: integer;
  Username: String;
  Seed, EncodedBody: String;
  i: Integer;
Begin
  // User Requests seed
  Username := GetIDFromStringArray('Username', SplitString(aRequest.Authorization, ' '));
  If Username = '' Then Begin
    SendEmptyResponce(aResponse, 401);
    exit;
  End;
  Username := DecodeStringBase64(Username);
  Log('Got a challenge request from user: ' + Username);
  // Prüfen ob es den User bei uns überhaupt gibt, ..
  UserIndex := GetUserIndex(Username);
  If UserIndex = -1 Then Begin
    SendEmptyResponce(aResponse, 401);
    Exit;
  End;
  // Bekannte User bekommen eine Challenge die wir zur Authentifizierungsprüfung speichern müssen
  seed := '';
  setlength(Seed, SeedSize);
  For i := 0 To SeedSize - 1 Do Begin
    seed[i + 1] := chr(random(256));
    Users[UserIndex].LastChallenge[i] := ord(seed[i + 1]);
  End;
{$IFDEF Debug}
  Log('Send challenge:');
  For i := 1 To length(seed) Do Begin
    write(format(' %0.2X', [ord(seed[i])]));
  End;
  writeln('');
{$ENDIF}
  EncodedBody := EncodeStringBase64(seed);
  aResponse.Code := 200;
  aResponse.ContentLength := length(EncodedBody);
  aResponse.Content := EncodedBody;
  aResponse.SendContent;
End;

Procedure requesttoken(aRequest: TRequest; aResponse: TResponse);
Var
  token, ChallengeString, Username, rndString, EncodedBody: String;
  i, UserIndex: Integer;
  unEncryptedSeed, EncryptedSeed: TMemoryStream;
  EncrytpStream: TDCP_rijndael;
Begin
  Username := GetIDFromStringArray('Username', SplitString(aRequest.Authorization, ' '));
  If Username = '' Then Begin
    SendEmptyResponce(aResponse, 401);
    exit;
  End;
  Username := DecodeStringBase64(Username);
  Log('Got a challenge responce from user: ' + Username);
  UserIndex := GetUserIndex(Username);
  If UserIndex = -1 Then Begin
    SendEmptyResponce(aResponse, 401);
    Exit;
  End;
  ChallengeString := DecodeStringBase64(aRequest.Content);
{$IFDEF Debug}
  For i := 1 To length(ChallengeString) Do Begin
    write(format(' %0.2X', [ord(ChallengeString[i])]));
  End;
  writeln('');
{$ENDIF}
  rndString := '';
  setlength(rndString, SeedSize);
  For i := 0 To SeedSize - 1 Do Begin
    rndString[i + 1] := chr(Users[UserIndex].LastChallenge[i]);
  End;
  unEncryptedSeed := TMemoryStream.Create;
  unEncryptedSeed.Write(rndString[1], length(rndString));
  unEncryptedSeed.Position := 0;
  // 2. Verschlüsseln wie es der User gemacht hat
  EncryptedSeed := TMemoryStream.Create;
  EncrytpStream := TDCP_rijndael.Create(Nil);
  EncrytpStream.InitStr(Users[UserIndex].Password, TDCP_sha256);
  EncrytpStream.CipherMode := cmCBC; // Cipher-Block Chaining (CBC)
  EncrytpStream.EncryptStream(unEncryptedSeed, EncryptedSeed, unEncryptedSeed.Size);
  EncrytpStream.free;
  unEncryptedSeed.free;
  setlength(rndString, EncryptedSeed.Size);
  EncryptedSeed.Position := 0;
  EncryptedSeed.Read(rndString[1], EncryptedSeed.Size);
  EncryptedSeed.free;
  // Prüfen ob der User auch weis was er tut ;)
  For i := 0 To SeedSize - 1 Do Begin
    If rndString[i + 1] <> ChallengeString[i + 1] Then Begin
      SendEmptyResponce(aResponse, 401);
      Exit;
    End;
  End;
  // Alles hat geklappt, der User darf die nächsten 10 mins mit uns reden, so lange er immer brav das folgende mit sendet:
  Token := '';
  setlength(Token, TokenSize);
  For i := 0 To TokenSize - 1 Do Begin
    Token[i + 1] := chr(random(256));
  End;
  Users[UserIndex].TokenCreationTime := now;
  Log(Username + ' logged in');
{$IFDEF Debug}
  Log('Send token:');
  For i := 1 To length(Token) Do Begin
    write(format(' %0.2X', [ord(Token[i])]));
  End;
  writeln('');
{$ENDIF}
  EncodedBody := EncodeStringBase64(Token);
  Users[UserIndex].Token := EncodedBody;
  aResponse.Code := 200;
  aResponse.ContentLength := length(EncodedBody);
  aResponse.Content := EncodedBody;
  aResponse.SendContent;
End;

Begin
  (*
   * History: 0.01 - Initial version
   *          0.02 - getdblist, getdb benötigt nun den Namen der Datenbank
   *          0.03 - setDB hatte falsche Dateinamen abgelegt.
   *          0.04 - remote user management
   *)
  Log('Filechecker server ver. 0.04');
  Randomize;
  LoadSettings;
  KeepRunning := true;
  While KeepRunning Do Begin
    Application.Initialize;
    HTTPRouter.RegisterRoute('/getchallenge', @getchallenge);
    HTTPRouter.RegisterRoute('/getdb', @getDB);
    HTTPRouter.RegisterRoute('/getdblist', @getDBList);
    HTTPRouter.RegisterRoute('/reloadsettings', @ReloadSettings);
    HTTPRouter.RegisterRoute('/requesttoken', @requesttoken);
    HTTPRouter.RegisterRoute('/setdb', @setDB); // Benötigt Erweiterte Rechte
    HTTPRouter.RegisterRoute('/setpassword', @setPassword);
    HTTPRouter.RegisterRoute('/getuserlist', @getuserlist); // Benötigt Erweiterte Rechte
    HTTPRouter.RegisterRoute('/setuserright', @setuserright); // Benötigt Erweiterte Rechte
    HTTPRouter.RegisterRoute('/deluser', @deluser); // Benötigt Erweiterte Rechte
    HTTPRouter.RegisterRoute('/adduser', @adduser); // Benötigt Erweiterte Rechte
    Application.Port := ServerPort;
    Application.UseSSL := true;
    Application.CertificateData.Certificate.FileName := CertificateFileName;
    Application.CertificateData.PrivateKey.FileName := PrivateKeyFileName;
    Log('  is running on port: ' + inttostr(ServerPort));
    Try
      Application.Run;
    Except
      On av: Exception Do Begin
        Log('Fatal: ' + av.Message);
      End;
    End;
    FreeAndNil(Application); // Clean Old Instance

    // Create new Instance
    Application := THTTPApplication.Create(Nil);
    CustomApplication := Application;
  End;
  Log('  stopped.');
End.

End.

