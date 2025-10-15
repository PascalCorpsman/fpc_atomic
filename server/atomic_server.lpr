(******************************************************************************)
(* FPC Atomic Server                                               01.03.2023 *)
(*                                                                            *)
(* Version     : See updater_settings.inc                                     *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This is the Server module for FPC Atomic                     *)
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
(* History     : See uatomic_common.pas                                       *)
(*                                                                            *)
(******************************************************************************)

Program atomic_server;

Uses
  sysutils, lazutf8, LazFileUtils
  , uatomic_server
  , uatomic_common
  ;

Procedure PrintHelp;
Begin
  writeln('');
  writeln('Online help for FPC Atomic Server ver. ' + format('%0.2f', [ProtocollVersion / 100]));
  writeln('');
  writeln('-p <Port> = Spezifies the port number to listen to.');
  writeln('-l <LogLevel> = Sets Loglevel (default = 2)');
  writeln('-t <Timeout> = Sets automatic close on no user connected, default ' + inttostr(ServerAutoTimeout) + 'ms');
  writeln('               0 = Disabled, typing "ESC" will terminate');
  writeln('-f <Filename> = Logs additional to a file');
  writeln('-ats <factor> = overwrite default atomic speed (default = 5)');
  writeln('-h = This screen');
  writeln('');
  writeln('Commands during execution:');
  writeln('ESC = close server (no matter if a game is running or not');
  writeln('u = unload ai (only for debugging)');
  writeln('a = load ai (only for debugging)');
End;

Var
  Server: TServer;
  Autotimeout: integer = ServerAutoTimeout;
  Port: integer = -1;
  i: integer;
  s: String;
  si: Single;
  Params: Array Of Boolean = Nil; // Zum Prüfen ob auch alle übergebenen Parameter verwendet wurden.
Begin
  (*
     OPL: - Alle Krankheiten (Speed Up, Switch Bombermen,  Fast Bomb,  Small Flame, Eject Bomb Fast + Kick)
          - Alle TODO's
          - Random Powerup ??
          - Einer der Handschuhe kann Bombem tragen und dann auf andere "werfen", so dass diese dann Powerups verlieren und "stun" sind ..

    Known Bugs:
          - Wenn eine Bombe in ein Brennendes Feld "Rollt" muss sie instant explodieren
            \-> Implementiert, aber noch nicht getestet.

    Maybe Implementierungen:
          - Kollisionserkennung zwischen Atomic und Bomben via Distanzmessung und nicht via "Koordinaten" ?
  // *)
  DefaultFormatSettings.DecimalSeparator := '.';
  CalculateAtomicSpeeds(AtomicDefaultSpeed);
  // Logger Konfigurieren
  InitLogger();
  setlength(params, Paramcount + 1);
  For i := 1 To Paramcount Do Begin
    params[i] := false;
  End;
  For i := 1 To Paramcount - 1 Do Begin
    If lowercase(paramstr(i)) = '-ats' Then Begin
      si := strtointdef(paramstr(i + 1), round(AtomicDefaultSpeed * 10)) / 10;
      CalculateAtomicSpeeds(si);
      Log(format('Overwrite default atomic speed with: %0.1f', [si]), llInfo);
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If lowercase(paramstr(i)) = '-p' Then Begin
      port := strtointdef(paramstr(i + 1), -1);
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If lowercase(paramstr(i)) = '-l' Then Begin
      SetLogLevel(strtointdef(paramstr(i + 1), 2));
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If lowercase(paramstr(i)) = '-t' Then Begin
      Autotimeout := strtointdef(paramstr(i + 1), ServerAutoTimeout);
      Params[i] := true;
      Params[i + 1] := true;
    End;
    If lowercase(paramstr(i)) = '-f' Then Begin
      s := ExtractFilePath(ParamStrutf8(i + 1));
      Params[i] := true;
      Params[i + 1] := true;
      If Not DirectoryExistsutf8(s) Then Begin
        If Not CreateDirUTF8(s) Then Begin
          Log('Could not create : ' + s, llWarning);
          Continue;
        End;
      End;
      SetLoggerLogFile(paramstr(i + 1));
    End;
    If (lowercase(paramstr(i)) = '-h') Or (lowercase(paramstr(i)) = '-?') Then Begin
      PrintHelp;
      exit;
    End;
  End;
  Log('FPC Atomic - Server ver. ' + format('%0.2f', [ProtocollVersion / 100]) + ' by Corpsman www.Corpsman.de', llInfo); // Eigentlich müsste dieser Log weiter oben sein, aber nur so ist er auch in der evtl. erstellten .log Datei
  s := '';
  For i := 1 To high(Params) Do Begin
    If Not params[i] Then Begin
      s := s + ' ' + ParamStrUTF8(i);
    End;
  End;
  If s <> '' Then Begin
    log('Unknown parameter :' + s, llWarning);
  End;
  setlength(params, 0);
  // Es mus mindestens der Port gesetzt werden.
  If port = -1 Then Begin
    Log('No argument -p <portnumber> passed. Stopping now.', llCritical);
    Printhelp;
    exit;
  End
  Else Begin
    Log('Launching on Port : ' + inttostr(port), llinfo);
    If Autotimeout = 0 Then Begin
      Log('Autotimeout = 0, press "ESC" to terminate.', llinfo);
    End;
  End;
  // 2. Server Erstellen
  Randomize;
  Try
    Server := TServer.create(Port, Autotimeout);
  Except
    On e: exception Do Begin
      log(e.Message, llFatal);
      Server := Nil;
    End;
  End;

  // 3. Endlosschleife
  If assigned(server) Then Begin
    server.LoadStatistiks();
    Try
      server.execute;
    Except
      On e: Exception Do Begin
        log(e.Message, llFatal);
      End;
    End;
    server.SaveStatistiks();
    log('Shutting down, thanks for playing.', llInfo);
    // 4. Fertig, aufräumen
    server.free;
  End;
End.

