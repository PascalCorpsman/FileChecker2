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

  TClient = Record
    ClientID: String;
    Users: TUsers;
  End;

  TClients = Array Of TClient;

Var
  Clients: TClients;
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
  i, j: Integer;
Begin
  Clients := Nil;
  ini := TIniFile.Create('settings.ini');
  ServerPort := ini.ReadInteger('Server', 'Port', 8443);
  CertificateFileName := ini.ReadString('Server', 'Certificate', 'server.crt');
  PrivateKeyFileName := ini.ReadString('Server', 'PrivateKey', 'server.key');
  setlength(Clients, ini.ReadInteger('Client', 'Count', 0));
  For i := 0 To high(Clients) Do Begin
    Clients[i].ClientID := ini.ReadString('Client', 'ID' + inttostr(i), '');
    Log('  addclient: ' + Clients[i].ClientID);
    setlength(Clients[i].Users, ini.ReadInteger('ClientUser' + inttostr(i), 'Count', 0));
    For j := 0 To high(Clients[i].Users) Do Begin
      Clients[i].Users[j].Username := ini.ReadString('ClientUser' + inttostr(i), 'Name' + inttostr(j), '');
      Clients[i].Users[j].Password := ini.ReadString('ClientUser' + inttostr(i), 'PW' + inttostr(j), '');
      Clients[i].Users[j].Rights := ini.ReadInteger('ClientUser' + inttostr(i), 'Rights' + inttostr(j), 0);
      fillchar(Clients[i].Users[j].LastChallenge[0], sizeof(Clients[i].Users[j].LastChallenge[0]), 0);
      Log('   adduser: ' + Clients[i].Users[j].Username);
    End;
  End;
  ini.free;
End;

(*
 * -1,-1 = user nicht bekannt
 * x = Client
 * y = User
 *)

Function GetClientUserIndex(Client, Username: String): Tpoint;
Var
  i, j: Integer;
Begin
  result := point(-1, -1);
  For j := 0 To high(Clients) Do Begin
    If Clients[j].ClientID = Client Then Begin
      For i := 0 To high(Clients[j].Users) Do Begin
        If Clients[j].Users[i].Username = Username Then Begin
          result := point(j, i);
          exit;
        End;
      End;
    End;
  End;
End;

(*
 * Gibt den Dateinamen für Client und User wie er auf der Festplatte liegt zurück.
 *)

Function GetClientUserDBFileName(UserIndex: TPoint): String;
Begin
  result := '';
  If (UserIndex.x < 0) Or (UserIndex.x > high(Clients)) Then exit;
  If (UserIndex.Y < 0) Or (UserIndex.Y > high(Clients[UserIndex.x].Users)) Then exit;
  result := Clients[UserIndex.x].ClientID + PathDelim + 'db' + Clients[UserIndex.x].Users[UserIndex.y].Username + '.db';
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
 * -1,-1 = user nicht bekannt
 * x = Client
 * y = User
 *)

Function UserExistsAndIsAllowed(aRequest: TRequest; aResponse: TResponse): TPoint;
Var
  StringArray: TStringDynArray;
  Username, Token, Client: String;
Begin
  result := point(-1, -1);
  StringArray := SplitString(aRequest.Authorization, ' ');
  // Kennen wir den User ?
  Username := GetIDFromStringArray('Username', StringArray);
  Client := GetIDFromStringArray('Client', StringArray);
  If (Username = '') Or (client = '') Then Begin
    SendEmptyResponce(aResponse, 401);
    exit;
  End;
  Username := DecodeStringBase64(Username);
  Client := DecodeStringBase64(Client);
  result := GetClientUserIndex(Client, Username);
  If result.x = -1 Then Begin
    SendEmptyResponce(aResponse, 401);
    Exit;
  End;
  // Timeout
  If now > Clients[Result.x].Users[result.Y].TokenCreationTime + delta10min Then Begin
    SendEmptyResponce(aResponse, 401);
    result := point(-1, -1);
    Exit;
  End;
  // Ist der User berechtigt ?
  Token := GetIDFromStringArray('Bearer', StringArray);
  If token <> Clients[Result.x].Users[result.Y].Token Then Begin
    SendEmptyResponce(aResponse, 401);
    result := point(-1, -1);
    Exit;
  End;
End;

Procedure getDB(aRequest: TRequest; aResponse: TResponse);
Var
  Filestream: TFileStream;
  dbName, RawBody, EncodedBody, DatabaseFileName: String;
  UserIndex: Tpoint;
  i: Integer;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  dbName := DecodeStringBase64(aRequest.Content);
  // Prüfen ob der dbName tatsächlich auf eine Datenbank zeigt oder irgendwo hin
  // Nur Datenbanken zulassen !
  DatabaseFileName := '';
  For i := 0 To high(Clients[UserIndex.x].Users) Do Begin
    If (Clients[UserIndex.x].Users[i].Username) = dbName Then Begin
      DatabaseFileName := GetClientUserDBFileName(UserIndex);
      break;
    End;
  End;
  If (DatabaseFileName <> '') And (FileExists(DatabaseFileName)) Then Begin
    Log('Send ' + DatabaseFileName + ' to ' + Clients[UserIndex.x].Users[i].Username);
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
  UserIndex: Tpoint;
  i: Integer;
  sl: TStringList;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  sl := TStringList.Create;
  For i := 0 To high(Clients[UserIndex.x].Users) Do Begin
    If FileExists(GetClientUserDBFileName(point(userindex.x, i))) Then Begin
      sl.Add(Clients[UserIndex.x].Users[i].Username);
    End;
  End;
  EncodedBody := EncodeStringBase64(sl.text);
  Log(Clients[UserIndex.x].Users[UserIndex.y].Username + ' requested DB list for client ' + Clients[UserIndex.x].ClientID + ', found ' + IntToStr(sl.count));
  sl.free;
  aResponse.Code := 200;
  aResponse.ContentLength := EncodedBody.Length;
  aResponse.Content := EncodedBody;
  aResponse.SendContent;
End;

Procedure ReloadSettings(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex: Tpoint;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Clients[UserIndex.x].Users[UserIndex.y].Rights And RightToReloadSettings = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  Log(Clients[UserIndex.x].Users[UserIndex.y].Username + ' triggert reload settings.');
  LoadSettings;
  SendEmptyResponce(aResponse, 200);
End;

Procedure setDB(aRequest: TRequest; aResponse: TResponse);
Var
  Filestream: TFileStream;
  dbfilename, dbString: String;
  UserIndex: TPoint;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Clients[UserIndex.x].Users[UserIndex.y].Rights And RightToSetDB = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  dbString := DecodeStringBase64(aRequest.Content);
  // Leere Datenbanken wollen wir nicht
  If dbString = '' Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  dbfilename := GetClientUserDBFileName(UserIndex);
  Try
    If Not ForceDirectories(ExtractFilePath(dbfilename)) Then Begin
      SendEmptyResponce(aResponse, 500);
      Exit;
    End;
    Filestream := TFileStream.Create(dbfilename, fmCreate Or fmOpenWrite);
    Filestream.Write(dbString[1], length(dbString));
    Filestream.free;
    Log('Received db from ' + Clients[UserIndex.x].Users[UserIndex.y].Username + ' for client ' + Clients[UserIndex.x].ClientID);
  Except
    SendEmptyResponce(aResponse, 500);
    Exit;
  End;
  SendEmptyResponce(aResponse, 200);
End;

Procedure setPassword(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex: TPoint;
  NewPW: String;
  ini: TIniFile;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  NewPW := DecodeStringBase64(aRequest.Content);
  Log(Clients[UserIndex.x].ClientID + ' set new password for: ' + Clients[UserIndex.x].Users[UserIndex.y].Username);
  // Sofort übernehmen
  Clients[UserIndex.x].Users[UserIndex.y].Password := NewPW;
  // Und für den Reboot ;)
  ini := TIniFile.Create('settings.ini');
  ini.WriteString('ClientUser' + inttostr(UserIndex.x), 'PW' + inttostr(UserIndex.y), NewPW);
  ini.free;
  SendEmptyResponce(aResponse, 200);
End;

Procedure getuserlist(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex: Tpoint;
  i: integer;
  sl: TStringList;
  m: TMemoryStream;
  RawBody, EncodedBody: String;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Clients[UserIndex.x].Users[UserIndex.y].Rights And RightToEditUsers = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  sl := TStringList.Create;
  For i := 0 To high(Clients[UserIndex.x].Users) Do Begin
    sl.add(Format('%d;%s', [Clients[UserIndex.x].Users[i].Rights, Clients[UserIndex.x].Users[i].Username]));
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
  Log(Clients[UserIndex.x].ClientID + ': ' + Clients[UserIndex.x].Users[UserIndex.y].Username + ' requested userlist');
  aResponse.Code := 200;
  aResponse.ContentLength := EncodedBody.Length;
  aResponse.Content := EncodedBody;
  aResponse.SendContent;
End;

Procedure setuserright(aRequest: TRequest; aResponse: TResponse);
Var
  UserIndex: Tpoint;
  Content, UserName: String;
  sa: TStringArray;
  UserRight, i: Integer;
  ini: TIniFile;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Clients[UserIndex.x].Users[UserIndex.y].Rights And RightToEditUsers = 0 Then Begin
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
  For i := 0 To high(Clients[UserIndex.x].Users) Do Begin
    If Clients[UserIndex.x].Users[i].Username = Username Then Begin
      Log('Set new rights for: ' + Username);
      // Sofort übernehmen
      Clients[UserIndex.x].Users[i].Rights := UserRight;
      // Und für den Reboot ;)
      ini := TIniFile.Create('settings.ini');
      ini.WriteInteger('ClientUser' + inttostr(UserIndex.x), 'Rights' + inttostr(i), UserRight);
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
  UserIndex: Tpoint;
  i, j: integer;
  Username: String;
  ini: TIniFile;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Clients[UserIndex.X].Users[UserIndex.Y].Rights And RightToEditUsers = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  Username := DecodeStringBase64(aRequest.Content);
  For i := 0 To high(Clients[UserIndex.X].Users) Do Begin
    If Clients[UserIndex.X].Users[i].Username = Username Then Begin
      // Sofort Übernehmen
      For j := i To high(Clients[UserIndex.X].Users) - 1 Do Begin
        Clients[UserIndex.X].Users[j] := Clients[UserIndex.X].Users[j + 1];
      End;
      setlength(Clients[UserIndex.X].Users, high(Clients[UserIndex.X].Users));
      // Und für den Reboot ;)
      ini := TIniFile.Create('settings.ini');
      For j := i To high(Clients[UserIndex.X].Users) Do Begin
        ini.WriteString('ClientUser' + inttostr(UserIndex.X), 'Name' + inttostr(j), Clients[UserIndex.X].Users[j].Username);
        ini.WriteString('ClientUser' + inttostr(UserIndex.X), 'PW' + inttostr(j), Clients[UserIndex.X].Users[j].Password);
        ini.WriteInteger('ClientUser' + inttostr(UserIndex.X), 'Rights' + inttostr(j), Clients[UserIndex.X].Users[j].Rights);
      End;
      ini.WriteInteger('ClientUser' + inttostr(UserIndex.X), 'Count', length(Clients[UserIndex.X].Users));
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
  UserIndex: Tpoint;
  i: integer;
  Username, Content, Password: String;
  ini: TIniFile;
  Rights: Integer;
Begin
  // Prechecks
  UserIndex := UserExistsAndIsAllowed(aRequest, aResponse);
  If UserIndex.x = -1 Then exit;
  // Spezielle Berechtigungen Prüfen
  If Clients[UserIndex.X].Users[UserIndex.Y].Rights And RightToEditUsers = 0 Then Begin
    SendEmptyResponce(aResponse, 403);
    Exit;
  End;
  Content := DecodeStringBase64(aRequest.Content);
  Rights := strtointdef(copy(Content, 1, pos(';', Content) - 1), 0);
  delete(Content, 1, pos(';', Content));
  Username := copy(Content, 1, pos(';', Content) - 1);
  delete(Content, 1, pos(';', Content));
  Password := Content;
  For i := 0 To high(Clients[UserIndex.X].Users) Do Begin
    If Clients[UserIndex.X].Users[i].Username = Username Then Begin // User existiert schon -> Raus
      SendEmptyResponce(aResponse, 404);
      exit;
    End;
  End;
  // Sofort Übernehmen
  setlength(Clients[UserIndex.X].Users, high(Clients[UserIndex.X].Users) + 2);
  Clients[UserIndex.X].Users[High(Clients[UserIndex.X].Users)].Username := Username;
  Clients[UserIndex.X].Users[High(Clients[UserIndex.X].Users)].Rights := Rights;
  Clients[UserIndex.X].Users[High(Clients[UserIndex.X].Users)].Password := Password;
  fillchar(Clients[UserIndex.X].Users[High(Clients[UserIndex.X].Users)].LastChallenge[0], sizeof(Clients[UserIndex.X].Users[High(Clients[UserIndex.X].Users)].LastChallenge[0]), 0);
  // Und für den Reboot ;)
  ini := TIniFile.Create('settings.ini');
  ini.WriteString('ClientUser' + inttostr(UserIndex.X), 'Name' + inttostr(High(Clients[UserIndex.X].Users)), Clients[UserIndex.X].Users[high(Clients[UserIndex.X].Users)].Username);
  ini.WriteString('ClientUser' + inttostr(UserIndex.X), 'PW' + inttostr(High(Clients[UserIndex.X].Users)), Clients[UserIndex.X].Users[high(Clients[UserIndex.X].Users)].Password);
  ini.WriteInteger('ClientUser' + inttostr(UserIndex.X), 'Rights' + inttostr(High(Clients[UserIndex.X].Users)), Clients[UserIndex.X].Users[high(Clients[UserIndex.X].Users)].Rights);
  ini.WriteInteger('ClientUser' + inttostr(UserIndex.X), 'Count', length(Clients[UserIndex.X].Users));
  ini.free;
  Log(Clients[UserIndex.X].ClientID + ',  adduser: ' + Username);
  SendEmptyResponce(aResponse, 200);
End;

Procedure getchallenge(aRequest: TRequest; aResponse: TResponse);
Var
  StringArray: TStringDynArray;
  UserIndex: Tpoint;
  Username, Seed, EncodedBody, Client: String;
  i: Integer;
Begin
  // User Requests seed
  StringArray := SplitString(aRequest.Authorization, ' ');
  Username := GetIDFromStringArray('Username', StringArray);
  Client := GetIDFromStringArray('Client', StringArray);
  If (Username = '') Or (client = '') Then Begin
    SendEmptyResponce(aResponse, 401);
    exit;
  End;
  Username := DecodeStringBase64(Username);
  Client := DecodeStringBase64(Client);
  // Prüfen ob es den User bei uns überhaupt gibt, ..
  UserIndex := GetClientUserIndex(Client, Username);
  If UserIndex.x = -1 Then Begin
    SendEmptyResponce(aResponse, 401);
    Exit;
  End;
  Log(Clients[UserIndex.x].ClientID + ', got a challenge request from user: ' + Username);
  // Bekannte User bekommen eine Challenge die wir zur Authentifizierungsprüfung speichern müssen
  seed := '';
  setlength(Seed, SeedSize);
  For i := 0 To SeedSize - 1 Do Begin
    seed[i + 1] := chr(random(256));
    Clients[UserIndex.x].Users[UserIndex.y].LastChallenge[i] := ord(seed[i + 1]);
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
  token, ChallengeString, Username, rndString, EncodedBody, Client: String;
  i: Integer;
  UserIndex: TPoint;
  unEncryptedSeed, EncryptedSeed: TMemoryStream;
  EncrytpStream: TDCP_rijndael;
  StringArray: TStringDynArray;
Begin
  StringArray := SplitString(aRequest.Authorization, ' ');
  Username := GetIDFromStringArray('Username', StringArray);
  Client := GetIDFromStringArray('Client', StringArray);
  If (Username = '') Or (client = '') Then Begin
    SendEmptyResponce(aResponse, 401);
    exit;
  End;
  Username := DecodeStringBase64(Username);
  client := DecodeStringBase64(client);
  UserIndex := GetClientUserIndex(client, Username);
  If UserIndex.x = -1 Then Begin
    SendEmptyResponce(aResponse, 401);
    Exit;
  End;
  Log(Clients[UserIndex.x].ClientID + ', got a challenge responce from user: ' + Username);
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
    rndString[i + 1] := chr(Clients[UserIndex.x].Users[UserIndex.Y].LastChallenge[i]);
  End;
  unEncryptedSeed := TMemoryStream.Create;
  unEncryptedSeed.Write(rndString[1], length(rndString));
  unEncryptedSeed.Position := 0;
  // 2. Verschlüsseln wie es der User gemacht hat
  EncryptedSeed := TMemoryStream.Create;
  EncrytpStream := TDCP_rijndael.Create(Nil);
  EncrytpStream.InitStr(Clients[UserIndex.x].Users[UserIndex.y].Password, TDCP_sha256);
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
  Clients[UserIndex.x].Users[UserIndex.y].TokenCreationTime := now;
  Log(Clients[UserIndex.x].ClientID + ', ' + Username + ' logged in');
{$IFDEF Debug}
  Log('Send token:');
  For i := 1 To length(Token) Do Begin
    write(format(' %0.2X', [ord(Token[i])]));
  End;
  writeln('');
{$ENDIF}
  EncodedBody := EncodeStringBase64(Token);
  Clients[UserIndex.x].Users[UserIndex.y].Token := EncodedBody;
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
   *          0.05 - add multi client support, changed complete interface !
   *)
  Log('Filechecker server ver. 0.05');
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

