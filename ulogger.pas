(******************************************************************************)
(* ulogger.pas                                                     06.11.2015 *)
(*                                                                            *)
(* Version     : 0.06                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This unit gives you the ability to create application logs   *)
(*               to support a better error finding within your application    *)
(*               there a several log levels and log outputs supported.        *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - prototypen für Stacktracing                           *)
(*               0.03 - Halt on Fatal Option                                  *)
(*                      MaxStackDepth                                         *)
(*                      Optionales StackBoundaryChecking                      *)
(*                      Optionales Anfügen Methodenname beim Logging          *)
(*                      LogShow                                               *)
(*               0.04 - Added loglevel llError                                *)
(*               0.05 - Logshow für Konsole                                   *)
(*               0.06 - laderoutine für Logfiles                              *)
(*                                                                            *)
(******************************************************************************)

Unit ulogger;

{$MODE objfpc}{$H+}

Interface

(*
 * if you get an compiler error here create the ulogger.inc and enable the following
 * defines as needed:
   {$.define USELCL} // Use LCL instead of console output
 *)
{$I ulogger.inc}

Uses classes
{$IFDEF USELCL}
  , dialogs
{$ENDIF}
  ;

Type

  (*
   * Jeder Logeintrag kann einer bestimmten Log Gruppe zugeordnet werden
   * Es empfiehlt sich dabei folgende Gliederungshierarchie zu verwenden.
   *
   * Für Produktivcode sollte mindetens LogLevel (3) Aktiv sein.
   *
   * Level :
   *(0)   Trace = [llTrace, lldebug, llInfo, llWarning, llError, llCritical, llFatal]
   *              Ein Trace Log, dient im Allgemeinen der Nachvollziehbarkeit des
   *              Programmflusses, er erzeugt den Größten Datenoutput und ist
   *              deswegen, die schwächste Form des Loggings
   *              z.B.: bei Betreten und Beenden einer Methode
   *
   *(1)  Debugg = [lldebug, llInfo, llWarning, llError, llCritical, llFatal]
   *              Ein Debugg Log, ist eine Willkürlich und generell nur Temporär
   *              ausgegebene Information
   *              z.B.: Bei der Suche nach Fehlern, Ausgabe von Werten
   *
   *(2)    Info = [llInfo, llWarning, llError, llCritical, llFatal]
   *              Alles was nur der "Interesse" halber geloggt wird, aber eigentlich
   *              nicht kritisch ist.
   *              z.B.: Aktuelle Uhrzeit, Anzahl eingeloggter Benutzer ..
   *
   *(3) Warning = [llWarning, llError, llCritical, llFatal]
   *              Ein Warning Log, ist ein Fehler, welcher die Ausführung der
   *              Anwendung nicht weiter beeinflusst. Aber dennoch nicht der
   *              Übliche/ Erwartete Zustand eingenommen wird.
   *              z.B.: Vergessen eine Setting zu spezifizieren, die Anwendung nimmt einen Default Wert
   *
   *(4)   Error = [llError, llCritical, llFatal]
   *              Ein Error Log, ist ein Fehler, welcher die Ausführung negativ
   *              beeinflusst aber nicht zum Absturz führt. Z.B. Fehlen einer
   *              kompletten Konfigurationsdatei, das Programm kann eingeschränkt weiter genutzt werden.
   *
   *(5)Critical = [llCritical, llFatal]
   *              Ein Kritischer Log, ist ein Fataler Fehler, welcher aber eine
   *              geeignete Fehlerbehandlung hat, das Programm bleibt Konsistent
   *              und kann weiter Fehlerfrei (mit Defaultwerten) betrieben werden.
   *              z.B.: Zugriff auf ein nicht existierendes Array Element => Rückgabe Default Wert
   *                    Umwandlung von String in Int, mit Defaultwert
   *
   *(6)   Fatal = [llFatal]
   *              Ein Fataler Log, führt im Prinzip zu einem Programmabsturz,
   *              alles oder Programmteile sind hiernach nicht mehr ausführbar.
   *              Die Anwendung ist in einem Undefinierten Zustand, bei dem sie
   *              nicht mehr definiert weiter arbeiten kann und beendet werden
   *              sollte.
   *              z.B.: Zugriffsverletzung auf einen Speicherbereich,
   *                    except block im Try
   *)

  TLogLevel = (llTrace, lldebug, llInfo, llWarning, llError, llCritical, llFatal);
  TLogLevelSet = Set Of TLogLevel;

  (*
   * Die Loggerklasse ist nur Verfügbar, damit "eigene" instanzen erzeugt
   * werden können.
   * Zur Nutzung der unit ist kein eigenständiges Erzeugen notwendig.
   *)

  (*
   * Soll der Stack mit geloggt werden muss jede Prozedur wie folgt gestaltet werden
   * Zur Nutzung muss das llTrace nicht Aktiviert werden, es genügt, dass LogTraceStack
   * gesetzt wird, und das an LogStack übergebene Loglevel aktuell ebenfalls geloggt wird.
   *
   * Procedure Dummy;
   * begin
   *   Log('Dummy', llTrace);
   *   ..
   *   { Vor jedem "Halt", muss auch LogLeave stehen }
   *
   *   LogLeave;
   * end;
   *
   * Dann kann zu jeder Zeit Mittels : LogStack( .. ) der Aktuelle Stack geschrieben werden.
   *)

  { TLogger }

  TLogger = Class
  private
    fAddRoutineNameToLogs: Boolean;
    fAutoLogStackOnFatal: Boolean;
    fCheckMaxStackDepth: integer;
    fFilename: String;
    fHaltOnFatal: Boolean;
    fLogLevel: TLogLevelSet;
    FLogFile: TextFile;
    fFlushLogFileOnLog: Boolean;
    flogfileisopened: Boolean;
    FEnable: Boolean;
    FDateFormat: String; // Todo : Soll mal dem User verfügbar gemacht werden
    fLogToConsole: Boolean;
    flogtoFile: Boolean;
    fLogStackTrace: Boolean;
    fStack: TStringList;
    fMaxStackDepth: integer;
    fCheckStackBoundaries: Boolean;
    Procedure OpenLogFile; // Öffnet evtl. das Filehandle
    Procedure DoLog(Const Text: String); // Gibt den Text auf der Console aus, oder Speichert in ihn die LogDatei
    Function CreateLog(Logtext: String; Const Loglevel: TLogLevel): String; // Erzeugt den Passend Eingerückten Logeintrag, welcher Gespeichert oder auf die Konsole Ausgegeben wird
    Procedure CloseLogFile(Force: Boolean); // Schließt evtl. das Filehandle
    Function StackLog: String; // Der Stacklog als Text
    Procedure SetEnable(Const aValue: Boolean);
    Function getLoglevel(): integer;
  public
    (*
     * Stack Bezogene Optionen
     *)
    Property AddRoutineNameToLogs: Boolean read fAddRoutineNameToLogs write fAddRoutineNameToLogs; // Wenn Stacktracing Aktiviert ist, dann kann hier der Aktuelle Methodenname Automatisch mit angefügt werden.
    Property AutoLogStackOnFatal: Boolean read fAutoLogStackOnFatal write fAutoLogStackOnFatal; // Automatisches Stack Trace Schreiben bei Fatalen Logs
    Property CheckMaxStackDepth: integer read fCheckMaxStackDepth write fCheckMaxStackDepth; // Bei Aktivierter Stack überwachung ist dies die Grenze wogegen geprüft wird
    Property CheckStackBoundaries: Boolean read fCheckStackBoundaries write fCheckStackBoundaries; // Wenn Gesetzt, dann Loggt der Logger einen Fatalen Log, wenn Stacktiefe > fCheckMaxStackDepth oder < 0 wird.
    Property LogStackTrace: Boolean read fLogStackTrace write fLogStackTrace; // Wenn Aktiviert, dann muss zu jedem llTrace ein LogLeave aufgerufen werden.
    Property MaxStackDepth: integer read fMaxStackDepth; // Gibt die bisher tiefste Stacktiefe zurück

    (*
     * Generelle Optionen
     *)
    Property Enable: Boolean read FEnable write SetEnable; //Nur Wenn Enabled wird überhaupt geloggt
    Property HaltOnFatal: Boolean read fHaltOnFatal write fHaltOnFatal; // Wenn gesetzt, dann wird die Anwendung via Halt bei einem Fatalen Eintrag automatisch beendet.

    (*
     * Logging Spezifische Optionen
     *)
    Property FlushLogFileOnLog: Boolean read fFlushLogFileOnLog write fFlushLogFileOnLog; // Wenn True, wird jedes Log Ereignis auf die HDD durchgeschrieben und Sichergestellt, dass es auch drin ist, andernfalls wird evtl durch das OS gecached
    Property LogToConsole: Boolean read fLogToConsole write fLogToConsole; // Soll Auch ein Logging auf die Konsole stattfinden
    Property LogToFile: Boolean read flogtoFile write flogtofile; // Soll in eine Datei geschrieben werden ?
    Property LogLevel: Integer read getLoglevel;

    Constructor Create();
    Destructor Destroy; override;

    Procedure SetCustomLogging(Const LogLevel_: TLogLevelSet); // Frei Konfigurierbares Logging
    Procedure SetLogFilename(Const Filename: String); // Setzt das den Dateinamen für die Logdatei
    Procedure SetLogLevel(Const Level: integer); // Setzt gemäß der Obigen Spezifikation das Logging

    // Der Eigentliche Log Mechanismus
    Procedure AssertLog(Criteria: Boolean; LogText: String; LogLevel_: TLogLevel = llInfo); // Logt nur wenn "Criteria" = true
    Procedure Log(LogText: String; LogLevel_: TLogLevel = llInfo);
    Procedure LogShow(LogText: String; LogLevel_: TLogLevel = llInfo);
    Procedure LogBool(Value: Boolean; Description: String = ''; LogLevel_: TLogLevel = llInfo);
    Procedure LogHex(Value: uInt32; Description: String = ''; LogLevel_: TLogLevel = llInfo);
    Procedure LogInt(Value: Int32; Description: String = ''; LogLevel_: TLogLevel = llInfo);
    Procedure LogUInt(Value: uInt32; Description: String = ''; LogLevel_: TLogLevel = llInfo);

    Function LoggerLogs(Const LogLevel_: TLogLevel): Boolean; // True, wenn Loglevel vom Logger geloggt wird

    Procedure LogLeave; // Wenn LogTraceStack Aktiviert, "Ende" einer Routine
    Procedure LogStack(LogLevel_: TLogLevel = llinfo); // Wenn LogTraceStack Aktiviert, dann kann so der Stacktrace geloggt werden
  End;

  (*
   * Zum Laden einer LogDatei
   *)

  TLogEntry = Record
    TimeStamp: TDateTime;
    LogLevel: TLogLevel;
    LogText: String;
  End;

  TLogEntryArray = Array Of TLogEntry;

Var
  (*
   * Konfiguration des Loggers, Optional
   *
   * Ohne Konfiguration gelten folgende Einstellungen :
   *
   * Loglevel(2)
   * Enable = True
   * LogToFile = True
   * LogToConsole = False
   * FlushLogFileOnLog = True
   * Logfilename = Exename mit Dateiendung ".log"
   * DateFormat = 'YYYY.MM.DD HH:NN:SS'
   *
   *)
  Logger: TLogger = Nil; // Wird Automatisch initialisiert und Freigegeben, dient nur dem Zugriff auf die Konfiguration

  (*
   * Diverse Log Funktionen für Basistypen, Wrapper für logger.XY
   *)
Procedure AssertLog(Criteria: Boolean; LogText: String; LogLevel: TLogLevel = llInfo); // Logt nur wenn "Criteria" = true
Procedure Log(LogText: String; LogLevel: TLogLevel = llInfo); // -- Kann auch mit Mehrzeiligen Strings umgehen
Procedure LogShow(LogText: String; LogLevel: TLogLevel = llInfo);
Procedure LogBool(Value: Boolean; Description: String = ''; LogLevel: TLogLevel = llInfo);
Procedure LogHex(Value: uInt32; Description: String = ''; LogLevel: TLogLevel = llInfo);
Procedure LogInt(Value: Int32; Description: String = ''; LogLevel: TLogLevel = llInfo);
Procedure LogUInt(Value: uInt32; Description: String = ''; LogLevel: TLogLevel = llInfo);

Procedure LogLeave; // Wenn LogTraceStack Aktiviert, dann kann so der Stacktrace geloggt werden
Procedure LogStack(LogLevel: TLogLevel = llinfo); // Wenn LogTraceStack Aktiviert, "Ende" einer Routine

(*
 * Sonstige
 *)
Function LogLevelToString(LogLevel: TLogLevel): String;
Function LoadLogFile(Const Filename: String): TLogEntryArray;

Implementation

Uses sysutils, FileUtil, LazFileUtils,
{$IFDEF USELCL}
  lazutf8,
{$ENDIF}
  math;

{$IFNDEF USELCL}

Function utf8tosys(value: String): String;
Begin
  result := value;
End;
{$ENDIF}

(*
 * Umkehrfunktion zu FormatDateTime
 * Die Implementierung ist nicht vollständig, Erkennt aber ob die Konvertierung
 * Fehlgeschlagen ist, wenn dem so ist, dann result := -1
 *)

Function StrToDateTimeFormat(Input, Format: String): TDateTime;
Var
  y, m, d, h, n, s, z: String;
  ip, fp: integer;
  Block: Boolean;
Begin
  (*
   * Die Idee ist den Input String gemäß dem Format String zu Parsen und alle
   * Formatstring Tokens in den einzelnen Container zu sammeln.
   * Dann wird Konvertiert und mittels anschließender Rückkonvertierung
   * geprüft ob alles geklappt hat *g*.
   *)
  // Der Formatstring muss mindestens so lang sein wie der Eingabestring.
  If length(Format) < length(Input) Then Begin
    Result := -1;
    exit;
  End;
  y := '';
  m := '';
  d := '';
  h := '';
  n := '';
  s := '';
  z := '';
  Block := FALSE;
  Format := lowercase(Format);
  ip := 1;
  fp := 1;
  While fp <= length(Format) Do Begin
    If Block Then Begin
      If Format[fp] = '''' Then Begin
        Block := FALSE;
        inc(fp);
      End
      Else Begin
        inc(fp);
        inc(ip);
      End;
    End
    Else Begin
      Case Format[fp] Of
        '''': Begin
            Block := true;
            dec(ip);
          End;
        'y': y := y + Input[ip];
        'm': m := m + Input[ip];
        'd': d := d + Input[ip];
        'h': h := h + Input[ip];
        'n': n := n + Input[ip];
        's': s := s + Input[ip];
        'z': z := z + Input[ip];
      End;
      inc(fp);
      inc(ip);
    End;
  End;
  // Sind die yy Daten nur als zweistellige Zahl vorhanden, dann auf 2000+ verschieben
  If strtointdef(y, 0) < 2000 Then Begin
    y := IntToStr(strtointdef(y, 0) + 2000);
  End;
  Try
    Result := EncodeDate(strtointdef(y, 0), strtointdef(m, 0), strtointdef(d, 0)) + EncodeTime(strtointdef(h, 0), strtointdef(n, 0), strtointdef(s, 0), strtointdef(z, 0));
  Except
    Result := -1;
    exit;
  End;
  // Wenn alles geklappt hat, muss sich die Inverse wieder bilden lassen ;)
  z := FormatDateTime(Format, Result);
  If lowercase(FormatDateTime(Format, Result)) <> lowercase(Input) Then Begin
    Result := -1;
  End;
End;

Procedure LogLeave;
Begin
  If assigned(logger) Then
    logger.LogLeave;
End;

Procedure LogStack(LogLevel: TLogLevel);
Begin
  If assigned(logger) Then
    logger.LogStack(loglevel);
End;

Function LogLevelToString(LogLevel: TLogLevel): String;
Begin
  result := 'unkn.';
  Case LogLevel Of
    llTrace: result := 'Trace';
    lldebug: result := 'Debug';
    llInfo: result := 'Info ';
    llWarning: result := 'Warn ';
    llError: result := 'Error';
    llCritical: result := 'Crit ';
    llFatal: result := 'Fatal';
  End;
End;

Function StrToLogLevel(LogLevel: String): TLogLevel;
Begin
  result := llFatal;
  Case LogLevel Of
    'Trace': result := llTrace;
    'Debug': result := lldebug;
    'Info ': result := llInfo;
    'Warn ': result := llWarning;
    'Error': result := llError;
    'Crit ': result := llCritical;
    'Fatal': result := llFatal;
  End;
End;

Function LoadLogFile(Const Filename: String): TLogEntryArray;
Var
  s: String;
  sl: TStringList;
  cnt, i: Integer;
Begin
  result := Nil;
  If FileExistsutf8(Filename) Then Begin
    // Gepuffertes Laden des Logfiles, so kann das Logfile während des neu ladens weiter beschrieben werden.
    s := GetTempFileNameUTF8(GetTempDir(), '');
    sl := TStringList.Create;
    If CopyFile(utf8tosys(filename), utf8tosys(s), false, false) Then Begin
      sl.LoadFromFile(s);
      If Not DeleteFileUTF8(s) Then Begin
{$IFDEF USELCL}
        showmessage('Error on deleting temporary file : ' + s);
{$ELSE}
        writeln('Error on deleting temporary file : ' + s);
{$ENDIF}
      End;
    End;
  End
  Else Begin
    sl.LoadFromFile(Filename);
  End;
  // Laden der Einträge
  setlength(result, sl.Count);
  cnt := 0;
  i := 0;
  While i < sl.count Do Begin
    s := sl[i];
    If trim(s) <> '' Then Begin
      If s[26] = '|' Then Begin // Der Begin eines Einziligen Logs
        result[cnt].TimeStamp := StrToDateTimeFormat(copy(s, 1, 19), Logger.fDateFormat);
        result[cnt].Loglevel := StrToLogLevel(copy(s, 21, 5));
        result[cnt].LogText := copy(s, 27, length(s));
        inc(cnt);
      End
      Else Begin // Ein Mehrzeiliger Log
        If cnt = 0 Then Begin
{$IFDEF USELCL}
          showmessage('Error invalid .log file.');
{$ELSE}
          writeln('Error invalid .log file.');
{$ENDIF}
          setlength(result, 0);
          sl.free;
          exit;
        End;
{$IFDEF Linux}
        result[cnt - 1].Logtext := result[cnt - 1].Logtext + LineEnding + copy(s, 27, length(s));
{$ELSE}
        // Auf Windows kann die Listview kein CRT in den Zellen Darstellen, leider :(
        result[cnt - 1].Logtext := result[cnt - 1].Logtext + ' ' + copy(s, 27, length(s));
{$ENDIF}
      End;
    End;
    inc(i);
  End;
  setlength(result, cnt);
  sl.free;
End;

Procedure AssertLog(Criteria: Boolean; LogText: String; LogLevel: TLogLevel);
Begin
  logger.AssertLog(Criteria, LogText, LogLevel);
End;

Procedure Log(LogText: String; LogLevel: TLogLevel = llInfo);
Begin
  logger.Log(LogText, LogLevel);
End;

Procedure LogShow(LogText: String; LogLevel: TLogLevel);
Begin
  logger.LogShow(LogText, LogLevel);
End;

Procedure LogBool(Value: Boolean; Description: String = ''; LogLevel: TLogLevel = llInfo);
Begin
  logger.LogBool(value, Description, LogLevel);
End;

Procedure LogInt(Value: Int32; Description: String = ''; LogLevel: TLogLevel = llInfo);
Begin
  logger.LogInt(value, Description, LogLevel);
End;

Procedure LoguInt(Value: uInt32; Description: String = ''; LogLevel: TLogLevel = llInfo);
Begin
  logger.LoguInt(value, Description, LogLevel);
End;

Procedure LogHex(Value: uInt32; Description: String = ''; LogLevel: TLogLevel = llInfo);
Begin
  logger.LogHex(value, Description, LogLevel);
End;

{ TLogger }

Constructor TLogger.Create;
Begin
  Inherited create;
  fAddRoutineNameToLogs := true;
  fCheckMaxStackDepth := 100;
  fCheckStackBoundaries := false;
  fMaxStackDepth := 0;
  fHaltOnFatal := false;
  fAutoLogStackOnFatal := false;
  fStack := TStringList.Create;
  fLogStackTrace := false;
  fFilename := '';
  Enable := True;
  SetLogFilename(ChangeFileExt(ParamStr(0), '.log'));
  SetLogLevel(2);
  flogfileisopened := false;
  fFlushLogFileOnLog := true;
  FDateFormat := 'YYYY.MM.DD HH:NN:SS';
  fLogToConsole := false;
  flogtoFile := true;
End;

Destructor TLogger.Destroy;
Begin
  fStack.free;
  CloseLogFile(true);
End;

Procedure TLogger.CloseLogFile(Force: Boolean);
Begin
  If (Force Or fFlushLogFileOnLog) And flogfileisopened Then Begin
    CloseFile(FLogFile);
    flogfileisopened := false;
  End;
End;

Function TLogger.CreateLog(Logtext: String; Const Loglevel: TLogLevel): String;
Var
  s, t: String;
  i: integer;
Begin
  If fAddRoutineNameToLogs And fLogStackTrace And (loglevel <> lltrace) Then Begin
    If fStack.Count > 0 Then Begin
      Logtext := fstack[fStack.Count - 1] + ': ' + Logtext;
    End;
  End;
  // Evtl. Vorkommende Steuerzeichen in CRT's umwandeln
  Logtext := StringReplace(Logtext, '\n', LineEnding, [rfReplaceAll, rfIgnoreCase]);
  If pos(LineEnding, Logtext) <> 0 Then Begin
    Logtext := TrimRight(Logtext);
    // 1. Zeile mit Header
    s := FormatDateTime(FDateFormat, now) + '|' + LogLevelToString(Loglevel) + '|';
    result := s;
    For i := 1 To length(s) Do Begin
      s[i] := ' ';
    End;
    Logtext := Logtext + LineEnding;
    While Logtext <> '' Do Begin
      t := copy(Logtext, 1, pos(LineEnding, Logtext) - 1);
      delete(Logtext, 1, length(t) + length(LineEnding));
      result := result + t;
      If Logtext <> '' Then
        result := result + LineEnding + s;
    End;
  End
  Else Begin
    result := FormatDateTime(FDateFormat, now) + '|' + LogLevelToString(Loglevel) + '|' + Logtext;
  End;
End;

Procedure TLogger.Log(LogText: String; LogLevel_: TLogLevel);
Begin
  If Not assigned(self) Then exit; // Wenn sich der Logger selbst killt via halt, dann wird er freigegeben bevor er fertig ist.
  If Not FEnable Then exit;
  If loglevel_ In fLogLevel Then Begin
    OpenLogFile;
    DoLog(CreateLog(Logtext, Loglevel_));
    If fAutoLogStackOnFatal And (LogLevel_ = llFatal) Then Begin
      DoLog(CreateLog(StackLog(), Loglevel_));
    End;
    CloseLogFile(false);
  End;
  // Wenn ein Stacktrace geschrieben werden soll, dann
  If fLogStackTrace And (LogLevel_ = llTrace) Then Begin
    fStack.Add(LogText);
    fMaxStackDepth := max(fMaxStackDepth, fstack.Count);
    If fstack.count > fCheckMaxStackDepth Then Begin
      log('Stack overflow.', llfatal);
    End;
  End;
  // Halt on Fatal
  If fHaltOnFatal And (loglevel_ = llFatal) Then Begin
    fstack.Clear; // Wir beenden via Gewalt, also Stack auch Löschen, wenn dann wurde er eh schon geloggt.
    CloseLogFile(true);
    halt(1);
  End;
End;

Procedure TLogger.LogShow(LogText: String; LogLevel_: TLogLevel);
Begin
{$IFDEF USELCL}
  ShowMessage(LogLevelToString(LogLevel_) + ' : ' + LogText);
{$ELSE}
  writeln(LogLevelToString(LogLevel_) + ' : ' + LogText);
{$ENDIF}
  If LogLevel_ In fLogLevel Then Begin
    log(LogText, LogLevel_);
  End;
End;

Procedure TLogger.LogBool(Value: Boolean; Description: String;
  LogLevel_: TLogLevel);
Begin
  If value Then Begin
    log(Description + ' = True ', LogLevel_);
  End
  Else Begin
    log(Description + ' = False', LogLevel_);
  End;
End;

Procedure TLogger.LogHex(Value: uInt32; Description: String; LogLevel_: TLogLevel
  );
Begin
  log(Description + format(' = %0.8X', [value]), LogLevel_);
End;

Function TLogger.LoggerLogs(Const LogLevel_: TLogLevel): Boolean;
Begin
  result := LogLevel_ In fLogLevel;
End;

Procedure TLogger.LogInt(Value: Int32; Description: String; LogLevel_: TLogLevel
  );
Begin
  log(Description + format(' = %d', [value]), LogLevel_);
End;

Procedure TLogger.LogLeave;
Begin
  If fLogStackTrace Then Begin
    If (fstack.Count <> 0) Then Begin
      fstack.Delete(fstack.Count - 1);
    End
    Else Begin
      If fCheckStackBoundaries Then Begin
        log('Stack underflow.', llfatal);
      End;
    End;
  End;
End;

Function TLogger.StackLog: String;
Var
  i: integer;
Begin
  result := 'Stacktrace:' + LineEnding;
  For I := 0 To fStack.Count - 1 Do Begin
    result := result + '  ' + fstack[i] + LineEnding;
  End;
End;

Procedure TLogger.LogStack(LogLevel_: TLogLevel);
Begin
  If fLogStackTrace And (LogLevel_ In fLogLevel) Then Begin
    Log(StackLog(), LogLevel_);
  End;
End;

Procedure TLogger.DoLog(Const Text: String);
Begin
  If flogfileisopened Then Begin // Wenn Keine datei geöffnet ist, kann auch nicht geschrieben werden.
    writeln(FLogFile, Text);
  End;
  If fLogToConsole Then Begin // Wenn auch auf die Konsole ausgegeben werden soll
    writeln(Text);
  End;
End;

Procedure TLogger.LogUInt(Value: uInt32; Description: String;
  LogLevel_: TLogLevel);
Begin
  log(Description + format(' = %u', [value]), LogLevel_);
End;

Procedure TLogger.OpenLogFile;
Begin
  If Not flogtoFile Then exit;
  If Not flogfileisopened Then Begin
    assignfile(FLogFile, utf8tosys(fFilename));
    If FileExists(fFilename) Then Begin
      append(FLogFile);
    End
    Else Begin
      Rewrite(FLogFile);
    End;
    flogfileisopened := true;
  End;
End;

Procedure TLogger.SetCustomLogging(Const LogLevel_: TLogLevelSet);
Begin
  fLogLevel := LogLevel_;
End;

Procedure TLogger.SetEnable(Const aValue: Boolean);
Begin
  If FEnable <> avalue Then Begin
    FEnable := avalue;
    If Not FEnable Then CloseLogFile(true);
  End;
End;

Procedure TLogger.SetLogFilename(Const Filename: String);
Begin
  If (fFilename <> Filename) Then Begin
    fFilename := Filename;
    // Wird der Dateiname Geändert, obwohl die Logdatei gerade geöffnet ist, muss die alte vorher geschlossen werden.
    If flogfileisopened Then Begin
      CloseLogFile(true);
    End;
  End;
End;

Procedure TLogger.AssertLog(Criteria: Boolean; LogText: String;
  LogLevel_: TLogLevel);
Begin
  If Not assigned(self) Then exit;
  If Not FEnable Then exit;
  If Criteria Then Begin
    Log(LogText, LogLevel_);
  End
  Else Begin
    If fLogStackTrace And (loglevel_ = lltrace) Then Begin
      fstack.Add(Logtext);
      fMaxStackDepth := max(fMaxStackDepth, fstack.Count);
    End;
  End;
End;

Function TLogger.getLoglevel: integer;
Begin
  result := -1;
  If fLogLevel = [llFatal, llCritical, llError, llWarning, llInfo, lldebug, llTrace] Then result := 0;
  If fLogLevel = [llFatal, llCritical, llError, llWarning, llInfo, lldebug] Then result := 1;
  If fLogLevel = [llFatal, llCritical, llError, llWarning, llInfo] Then result := 2;
  If fLogLevel = [llFatal, llCritical, llError, llWarning] Then result := 3;
  If fLogLevel = [llFatal, llCritical, llError] Then result := 4;
  If fLogLevel = [llFatal, llCritical] Then result := 5;
  If fLogLevel = [llFatal] Then result := 6;
End;

Procedure TLogger.SetLogLevel(Const Level: integer);
Begin
  Case level Of
    0: fLogLevel := [llFatal, llCritical, llError, llWarning, llInfo, lldebug, llTrace];
    1: fLogLevel := [llFatal, llCritical, llError, llWarning, llInfo, lldebug];
    2: fLogLevel := [llFatal, llCritical, llError, llWarning, llInfo];
    3: fLogLevel := [llFatal, llCritical, llError, llWarning];
    4: fLogLevel := [llFatal, llCritical, llError];
    5: fLogLevel := [llFatal, llCritical];
    6: fLogLevel := [llFatal]
  Else
    fLogLevel := []; // Undefiniert = Aus
  End;
End;

Initialization
  logger := TLogger.Create;

Finalization
  logger.Free;
  logger := Nil;

End.

