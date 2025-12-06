(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FPC_Atomic                                            *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uearlylog;

{$MODE objfpc}{$H+}

{$I globaldefines.inc}

Interface

{$IFDEF DARWIN}
Procedure EarlyLog(Const Msg: String);
{$ELSE}
Procedure EarlyLog(Const Msg: String);
Begin
  // Early logging only on macOS for now
End;
{$ENDIF}

Implementation

Uses SysUtils
{$IFDEF DARWIN}
  , LazFileUtils
{$ENDIF}
  ;

Procedure EarlyLog(Const Msg: String);
{$IFDEF DARWIN}
Var
  LogFile: TextFile;
  LogPath: String;
Begin
  Try
    LogPath := IncludeTrailingPathDelimiter(GetUserDir) + 'Library/Application Support/fpc_atomic/early_debug.log';
    If Not DirectoryExistsUTF8(ExtractFilePath(LogPath)) Then
      ForceDirectoriesUTF8(ExtractFilePath(LogPath));
    AssignFile(LogFile, LogPath);
    If FileExistsUTF8(LogPath) Then
      Append(LogFile)
    Else
      Rewrite(LogFile);
    WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' [EARLY] ' + Msg);
    Flush(LogFile);
    CloseFile(LogFile);
  Except
    // Ignore errors in early logging
  End;
End;
{$ELSE}
Begin
  // Early logging only on macOS for now
End;
{$ENDIF}

End.

