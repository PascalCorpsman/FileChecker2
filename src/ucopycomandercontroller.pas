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
Unit ucopycomandercontroller;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, lNet;

Procedure CopyCommanderController_Init(Const Connection: TLTcp = Nil);
Procedure CopyCommanderController_Clear();

Function StartCopyCommander(aCommand: String): Boolean;
Function StopCopyCommander(aIP: String; aPort: integer): Boolean;

Function isCopyCommanderRestAPIRunning(aIP: String; aPort: integer): Boolean;

Procedure Delay(delta_in_ms: integer);

Function CopyCommanderMoveFile(aFrom, aTo: String): Boolean;

Implementation

Uses
  process, UTF8Process, forms
  , Dialogs // Debug -- Remove
  , urest
  , uJSON
  ;

Const
  Idle = 0;
  Busy = 1;

Type

  { TDummyObject }

  TDummyObject = Class
    PostResult: String;
    GetResult: String;
    GetState: integer;
    Procedure OnGetResult(Sender: TObject; Const aPath: String; Const aContent: TJSONObj);
    Procedure OnPostResult(Sender: TObject; Const aPath: String; aResult: TJSONObj);
  End;

Var
  Restclient: TRestClient = Nil;
  DummyObject: TDummyObject = Nil;
  at: QWord;

Procedure StartTimeOut;
Begin
  at := GetTickCount64;
End;

Function TimeOutTriggered(delta: integer): boolean;
Begin
  result := at + delta < GetTickCount64;
End;

Procedure CopyCommanderController_Init(Const Connection: TLTcp);
Begin
  If assigned(Restclient) Then Restclient.Free;
  Restclient := TRestClient.Create(Connection);
  If Not assigned(DummyObject) Then DummyObject := TDummyObject.create;

End;

Procedure CopyCommanderController_Clear();
Begin
  If assigned(Restclient) Then Restclient.Free;
  Restclient := Nil;
  If assigned(DummyObject) Then
    DummyObject.free
End;

Function StartCopyCommander(aCommand: String): Boolean;
Var
  p: TProcessUTF8;
Begin
  result := false;
  If Not assigned(Restclient) Then exit;
  p := TProcessUTF8.Create(Nil);
  p.Options := [poWaitOnExit];
  p.CurrentDirectory := ExtractFileDir(aCommand);
  p.Executable := ExtractFileName(aCommand);
  p.Execute;
  p.free;
  result := true;
End;

Function StopCopyCommander(aIP: String; aPort: integer): Boolean;
Var
  job: TJSONNode;
Begin
  result := false;
  If Not assigned(Restclient) Then exit;
  If Not isCopyCommanderRestAPIRunning(aIP, aPort) Then exit;
  job := TJSONNode.Create;
  job.AddObj(TJSONValue.Create('skipJobs', 'true', true));
  DummyObject.PostResult := '';
  If Not Restclient.Post('/API/shutdown', job, @DummyObject.OnPostResult) Then Begin
    job.free;
    exit;
  End;
  job.Free;
  StartTimeOut;
  While DummyObject.PostResult = '' Do Begin
    Delay(10);
    If TimeOutTriggered(5000) Then Begin
      // TODO: eine geeignete Fehlermeldung ?
      exit;
    End;
  End;
  // TODO: das Result auswerten ?
  result := true;
End;

Function isCopyCommanderRestAPIRunning(aIP: String; aPort: integer): Boolean;
Var
  at: QWord;
Begin
  result := false;
  If Not assigned(Restclient) Then exit;
  result := Restclient.Connected;
  If Not result Then Begin // Können wir ggf auf den Port verbinden ?
    If Not Restclient.Connect(aIP, aPort) Then exit;
    at := GetTickCount64;
    While at + 1500 >= GetTickCount64 Do Begin
      Restclient.CallAction;
      Delay(10);
      Restclient.CallAction;
      result := Restclient.Connected;
      If result Then exit;
    End;
  End;
End;

Procedure Delay(delta_in_ms: integer);
Var
  at: QWord;
Begin
  at := GetTickCount64;
  While at + delta_in_ms >= GetTickCount64 Do Begin
    Application.ProcessMessages;
    sleep(1); // TODO: was hier wohl der Cleverste wert ist ?
  End;
End;

Function CopyCommanderMoveFile(aFrom, aTo: String): Boolean;
Var
  job: TJSONNode;
Begin
  result := false;
  // 1. Den Job erstellen und an den Copy Commander übergeben
  DummyObject.PostResult := '';
  job := TJSONNode.Create;
  job.AddObj(TJSONValue.Create('operation', 'move', true));
  job.AddObj(TJSONValue.Create('source', aFrom, true));
  job.AddObj(TJSONValue.Create('target', aTo, true));
  If Not Restclient.Post('/API/job', job, @DummyObject.OnPostResult) Then Begin
    job.free;
    exit;
  End;
  job.free;
  StartTimeOut;
  While DummyObject.PostResult = '' Do Begin
    Delay(10);
    If TimeOutTriggered(5000) Then Begin
      exit;
    End;
  End;
  If pos('queued', lowercase(DummyObject.PostResult)) = 0 Then exit;
  // 2. Warten bis der Job erledigt ist.
  DummyObject.GetState := busy;
  Repeat
    If Not Restclient.Get('/API/status', @DummyObject.OnGetResult) Then Begin
      exit;
    End;
    // Der Copy Commander ist wieder Idle -> Wir sind fertig ;)
    If DummyObject.GetState = Idle Then Begin
      break;
    End;
    Delay(500); // So ein kopierjob dauert schon mal länger, nicht übertreiben mit dem Polling ;)
  Until false;
  result := true;
End;

{ TDummyObject }

Procedure TDummyObject.OnGetResult(Sender: TObject; Const aPath: String;
  Const aContent: TJSONObj);
Var
  no: TJSONObj;
Begin
  GetResult := aContent.ToString();
  no := aContent.FindPath('State');
  If assigned(no) Then Begin
    GetState := strtointdef(TJSONValue(no).Value, Busy);
  End;
End;

Procedure TDummyObject.OnPostResult(Sender: TObject; Const aPath: String;
  aResult: TJSONObj);
Begin
  PostResult := aPath + ':' + aResult.ToString();
End;

End.

