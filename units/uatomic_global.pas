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
Unit uatomic_global;

{$MODE ObjFPC}{$H+}

Interface

{$I ../client/globaldefines.inc}

Uses
  Classes, SysUtils, ulogger;

Type
  TLogLevel = (llTrace, lldebug, llInfo, llWarning, llError, llCritical, llFatal);
  TLogShowHandler = Procedure(Msg: String; WarnLevel: TLogLevel);

  (*
   * Returns the config file, which always points to fpc_atomic.ini
   *)
Function GetAtomicConfigFile(): String;

{$IFDEF Server}
Function GetAtomicStatsFile(): String;
{$ENDIF}

(*
 * Returns a Log file separated by application, or raises an exception, if the corresponding define is not set
 *)
Function GetAtomicAppLogFile(): String;

(*
 * Returns a temp folder, wich is for all applications the same
 *)
Function GetAtomicTempDir(): String;


Var
  LogShowHandler: TLogShowHandler = Nil; // Debendency Injection auf eine LogShowMsg, wenn es die nicht gibt wird die von ulogger.pas genommen

Procedure InitLogger();
Procedure LoggerSetOnDoLog(LogEvent: TOnDoLog);

Procedure LogShow(LogText: String; LogLevel: TLogLevel = llInfo);
Procedure Log(LogText: String; LogLevel: TLogLevel = llInfo);

Function LogEnter(LogText: String): integer;
Procedure LogLeave(EnterID: integer {=-1});
Function MaxStackDepth(): Integer;

Function GetLoggerLoglevel(): integer;
{$IFDEF Windows}
Procedure EnableLogToConsole();
{$ENDIF}
Procedure SetLoggerLogFile(Filename: String);
Procedure SetLogLevel(Level: integer);
Procedure AssertLog(Criteria: Boolean; LogText: String; LogLevel: TLogLevel = llInfo); // Logt nur wenn "Criteria" = true
Function LogLevelToString(LogLevel: TLogLevel): String;

Implementation

Function GetApplicationName: String;
Begin
  result := 'fpc_atomic';
End;

Var
  VendorNameEvent: TGetVendorNameEvent = Nil;

Procedure Setup;
Begin
  // Rescue OnGetApplicationName
  VendorNameEvent := OnGetApplicationName;
  OnGetApplicationName := @GetApplicationName;
End;

Procedure TearDown;
Begin
  // Restore OnGetApplicationName
  OnGetApplicationName := VendorNameEvent;
  VendorNameEvent := Nil;
End;

Function GetAtomicConfigFile: String;
Begin
  Setup;
  result := GetAppConfigFile(false, true);
  TearDown;
End;

{$IFDEF Server}

Function GetAtomicStatsFile: String;
Begin
  Setup;
  result := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'stats.txt';
  TearDown;
End;
{$ENDIF}

Function GetAtomicAppLogFile: String;
Var
  AppLog: String;
Begin
  Setup;
  (*
   * /var/log/fpc_atomic needs root, so this is not the right ..
   * for starting, we use subfolder "log"
   *)
  result := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'log' + PathDelim;
  AppLog := '';
{$IFDEF Client}
  AppLog := 'client.log';
{$ENDIF}
{$IFDEF Server}
  AppLog := 'server.log';
{$ENDIF}
{$IFDEF Launcher}
  AppLog := 'launcher.log';
{$ENDIF}
{$IFDEF CD_Data_Extractor}
  AppLog := 'cd_data_extractor.log';
{$ENDIF}
  If AppLog = '' Then Begin
    Raise Exception.Create('Error, no apptype define set!');
  End;
  result := result + AppLog;
  If Not ForceDirectories(ExtractFileDir(result)) Then Begin
    Raise Exception.Create('Error, unable to setup logfile: ' + Result);
  End;
  TearDown;
End;

Function GetAtomicTempDir: String;
Begin
  Setup;
  result := GetTempDir(false);
  TearDown;
End;

Function ConvertLogLevel(ll: TLogLevel): ulogger.TLogLevel;
Begin
  Case ll Of
    llTrace: result := ulogger.llTrace;
    lldebug: result := ulogger.lldebug;
    llInfo: result := ulogger.llInfo;
    llWarning: result := ulogger.llWarning;
    llError: result := ulogger.llError;
    llCritical: result := ulogger.llCritical;
    llFatal: result := ulogger.llFatal;
  Else
    Raise Exception.Create('ConvertLogLevel: Hier ist was kaputt.');
  End;
End;

Procedure Log(LogText: String; LogLevel: TLogLevel);
Begin
  logger.Log(LogText, ConvertLogLevel(LogLevel));
End;

Function LogEnter(LogText: String): integer;
Begin
  result := logger.LogEnter(LogText);
End;

Procedure LogLeave(EnterID: integer);
Begin
  logger.LogLeave(EnterID);
End;

Function MaxStackDepth(): Integer;
Begin
  result := Logger.MaxStackDepth;
End;

Function GetLoggerLoglevel: integer;
Begin
  result := Logger.loglevel;
End;

Procedure InitLogger;
Var
  s: String;
Begin
{$IFDEF Server}
  logger.LogToConsole := true;
{$ENDIF}
{$IFDEF Linux}
  logger.LogToConsole := true;
{$ENDIF}
  // TODO: On Darwin also Log to console by default ?
  logger.LogToFile := true;
  s := GetAtomicAppLogFile();
  // Delete the Logfile from last time, is not not needed anymore ;)
  If FileExists(s) Then Begin
    If Not DeleteFile(s) Then Begin
      Raise Exception.Create('Error, not write access to log file: ' + s);
    End;
  End;
  logger.SetLogFilename(s);
  logger.AutoLogStackOnFatal := true;
  logger.StackTraceValidation := true;
  logger.SetLogLevel(2);
  // logger.HaltOnFatal := true; -> Eigentlich sollte das schon wieder rein, oder ?
End;

Procedure LoggerSetOnDoLog(LogEvent: TOnDoLog);
Begin
  logger.OnDoLog := LogEvent;
End;

{$IFDEF Windows}

Procedure EnableLogToConsole();
Begin
  logger.LogToConsole := true;
End;
{$ENDIF}

Procedure SetLoggerLogFile(Filename: String);
Begin
  // Todo : Theoretisch müsste man hier prüfen ob auch Schreibrechte existieren..
  logger.SetLogFilename(Filename);
  logger.LogToFile := true;
End;

Procedure SetLogLevel(Level: integer);
Begin
  logger.SetLogLevel(level);
End;

Procedure AssertLog(Criteria: Boolean; LogText: String; LogLevel: TLogLevel);
Begin
  // Alles muss durch Logshow geschleift werden
  If Criteria Then Begin
    LogShow(LogText, LogLevel);
  End;
End;

Function LogLevelToString(LogLevel: TLogLevel): String;
Begin
  result := ulogger.LogLevelToString(ConvertLogLevel(LogLevel));
End;

Procedure LogShow(LogText: String; LogLevel: TLogLevel);
Begin
  If assigned(LogShowHandler) Then Begin
    LogShowHandler(LogText, LogLevel);
  End
  Else Begin
{$IFDEF Server}
    log(LogText, LogLevel);
{$ELSE}
    ulogger.LogShow(LogText, ConvertLogLevel(LogLevel));
{$ENDIF}
  End;
End;

End.

