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

Procedure EarlyLog(Const Msg: String);

Implementation

Uses SysUtils
{$IFDEF DARWIN}
  , LazFileUtils
{$ENDIF}
  ;

Procedure EarlyLog(Const Msg: String);
Var
  LogFile: TextFile;
  LogPath: String;
Begin
  Try
{$IFDEF DARWIN}
    LogPath := IncludeTrailingPathDelimiter(GetUserDir) + 'Library/Application Support/fpc_atomic/early_debug.log';
    If Not DirectoryExistsUTF8(ExtractFilePath(LogPath)) Then
      ForceDirectoriesUTF8(ExtractFilePath(LogPath));
{$ELSE}
    // On Windows and Linux, create log file next to executable
    LogPath := ChangeFileExt(ParamStr(0), '_debug.log');
{$ENDIF}
    AssignFile(LogFile, LogPath);
    If FileExists(LogPath) Then
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

End.

