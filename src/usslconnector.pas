Unit usslconnector;

{$MODE ObjFPC}{$H+}

{.$DEFINE Debug}

Interface

Uses
  Classes, SysUtils;

Function Login(URL, Port, UserName, Password: String): Boolean;

Procedure Logout;

Function DownloadDB(): TMemorystream;
Function SendDB(Const db: TStream): Boolean;

Function SetPassword(NewPassword: String): Boolean;

Function ReloadSettings(): Boolean;

Implementation

Uses
  base64
  , DCPrijndael, DCPsha256, DCPcrypt2
  , fphttpclient, opensslsockets
  , Dialogs // Debugging
  ;

Var
  LoggedIn: Boolean = false;
  Client: TFPHttpClient = Nil;
  BaseURL: String = '';

Procedure nop();
Begin

End;

Function Login(URL, Port, UserName, Password: String): Boolean;
Var
{$IFDEF Debug}
  i: integer;
  b: Byte;
  ServerToken: TBytes;
{$ENDIF}
  rndString, AuthHeader, Challengeres: String;
  m, unEncryptedSeed, EncryptedSeed: TMemoryStream;
  EncrytpStream: TDCP_rijndael;
Begin
  result := false;
  Logout;
  If port <> '' Then Begin
    url := url + ':' + Port;
  End;
  client := TFPHttpClient.Create(Nil);
  // 1. Anfrage der Challenge
  AuthHeader := 'Username ' + EncodeStringBase64(Username);
  client.AddHeader('Authorization', AuthHeader);
  Try
    rndString := client.Get(URL + '/getchallenge');
  Except
    // Wir steigen hier aus, wenn die URL gar nicht erst erreichbar ist..
    On av: Exception Do Begin
      showmessage(av.Message + LineEnding +
        'HTTP.ResultCode: ' + inttostr(client.ResponseStatusCode) + ' ; ' + client.ResponseStatusText
        );
      exit;
    End;
  End;
  If client.ResponseStatusCode <> 200 Then exit;
  rndString := DecodeStringBase64(rndString);
  unEncryptedSeed := TMemoryStream.Create;
  unEncryptedSeed.Write(rndString[1], length(rndString));
{$IFDEF Debug}
  writeln('Got challenge:');
  unEncryptedSeed.Position := 0;
  b := 0;
  For i := 0 To unEncryptedSeed.Size - 1 Do Begin
    unEncryptedSeed.read(b, sizeof(b));
    write(format(' %0.2X', [b]));
  End;
  writeln('');
{$ENDIF}
  unEncryptedSeed.Position := 0;
  // 2. Verschlüsseln des Random mit eigenem PW
  EncryptedSeed := TMemoryStream.Create;
  EncrytpStream := TDCP_rijndael.Create(Nil);
  EncrytpStream.InitStr(Password, TDCP_sha256);
  EncrytpStream.CipherMode := cmCBC; // Cipher-Block Chaining (CBC)
  EncrytpStream.EncryptStream(unEncryptedSeed, EncryptedSeed, unEncryptedSeed.Size);
  EncrytpStream.free;
  unEncryptedSeed.free;
  // 3. Senden Verschlüsseltes Random an Server
  setlength(rndString, EncryptedSeed.Size);
  EncryptedSeed.Position := 0;
  EncryptedSeed.Read(rndString[1], EncryptedSeed.Size);
{$IFDEF Debug}
  writeln('Send challenge responce:');
  EncryptedSeed.Position := 0;
  b := 0;
  For i := 0 To EncryptedSeed.Size - 1 Do Begin
    EncryptedSeed.read(b, sizeof(b));
    write(format(' %0.2X', [b]));
  End;
  writeln('');
{$ENDIF}
  EncryptedSeed.free;
  rndString := EncodeStringBase64(rndString);
  m := TMemoryStream.Create;
  m.Write(rndString[1], length(rndString));
  m.Position := 0;
  Client.RequestBody := m;
  Challengeres := Client.Post(url + '/requesttoken');
  m.free;
  If client.ResponseStatusCode <> 200 Then exit;
  // 4 Antwort Akzeptiert
  Challengeres := DecodeStringBase64(Challengeres);
{$IFDEF Debug}
  ServerToken := Nil;
  setlength(ServerToken, length(Challengeres));
  For i := 0 To high(ServerToken) Do Begin
    ServerToken[i] := ord(Challengeres[i + 1]);
  End;
  writeln('Got token:');
  For i := 0 To high(ServerToken) Do Begin
    write(format(' %0.2X', [ServerToken[i]]));
  End;
  writeln('');
{$ENDIF}
  // TODO: Eigentlich müsste das free und create nicht sein, aber ohne kann man Header nicht neu setzen :/
  client.free;
  client := TFPHttpClient.Create(Nil);
  AuthHeader := 'Bearer ' + EncodeStringBase64(Challengeres) + ' Username ' + EncodeStringBase64(Username);
  client.AddHeader('Authorization', AuthHeader);
  BaseURL := url;
  LoggedIn := true;
  result := true;
End;

Procedure Logout;
Begin
  If assigned(Client) Then Begin
    Client.free;
  End;
  Client := Nil;
  BaseURL := '';
  LoggedIn := false;
End;

Function DownloadDB: TMemorystream;
Var
  db, dbString: String;
Begin
  result := Nil;
  If Not LoggedIn Then exit;
  Try
    db := client.Get(BaseURL + '/getdb');
  Except
    exit;
  End;
  If client.ResponseStatusCode <> 200 Then exit;
  dbString := DecodeStringBase64(db);
  result := TMemoryStream.Create;
  result.Write(dbString[1], length(dbString));
  result.Position := 0;
End;

Function SendDB(Const db: TStream): Boolean;
Var
  DBString: String;
  m: TMemoryStream;
Begin
  result := false;
  If Not LoggedIn Then exit;
  DBString := '';
  setlength(DBString, db.Size);
  db.Read(DBString[1], db.Size);
  DBString := EncodeStringBase64(DBString);
  m := TMemoryStream.Create;
  m.Write(DBString[1], length(DBString));
  m.Position := 0;
  Try
    Client.RequestBody := m;
    client.Post(BaseURL + '/setdb');
  Except
    m.free;
    exit;
  End;
  m.free;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

Function SetPassword(NewPassword: String): Boolean;
Var
  m: TMemoryStream;
Begin
  result := false;
  If Not LoggedIn Then exit;
  NewPassword := EncodeStringBase64(NewPassword);
  m := TMemoryStream.Create;
  m.Write(NewPassword[1], length(NewPassword));
  m.Position := 0;
  Try
    Client.RequestBody := m;
    client.Post(BaseURL + '/setpassword');
  Except
    m.free;
    exit;
  End;
  m.free;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

Function ReloadSettings(): Boolean;
Begin
  result := false;
  If Not LoggedIn Then exit;
  Try
    client.Post(BaseURL + '/reloadsettings');
  Except
    exit;
  End;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

End.

