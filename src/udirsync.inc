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

{$DEFINE RootField}

(*
 * By this method you can "freely" adjust TRenameEntry and TFileEntry
 * as Advanced Records do not allow adding fields to them
 *)

TRenameEntry = Record
  SourceFile: String;
  FileSize: Int64;
  DestFile: String;
  Root: String;
End;

TFileEntry = Record
  FileName: String;
  FileSize: Int64;
  Root: String;
End;

