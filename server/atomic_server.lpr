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
  sysutils
  , uatomic_server
  , uatomic_common
  , uatomic_global
  , uip
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
  writeln('-ats <factor> = overwrite default atomic speed (default = 5)');
  writeln('-d = if set do not ignore docker ip when reporting own ip');
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
  i, j, EnterID: integer;
  s: String;
  si: Single;
  Params: Array Of Boolean = Nil; // Zum Prüfen ob auch alle übergebenen Parameter verwendet wurden.
  adapters: TNetworkAdapterList;
  serverIP: String;
  showdockerip: Boolean;
Begin
  InitLogger();
  EnterID := LogEnter('atomic_server.lpr');
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
  showdockerip := false;
  CalculateAtomicSpeeds(AtomicDefaultSpeed);
  // Logger Konfigurieren
  setlength(params, Paramcount + 1);
  For i := 1 To Paramcount Do Begin
    params[i] := false;
  End;
  For i := 1 To Paramcount Do Begin
    If (i < ParamCount) Then Begin // Al Params that also evaluate the "next" param
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
        log('Set Log level to: ' + paramstr(i + 1), llInfo);
        Params[i] := true;
        Params[i + 1] := true;
      End;
      If lowercase(paramstr(i)) = '-t' Then Begin
        Autotimeout := strtointdef(paramstr(i + 1), ServerAutoTimeout);
        Params[i] := true;
        Params[i + 1] := true;
      End;
    End;
    // All Params that are only "Flags"
    If lowercase(paramstr(i)) = '-d' Then Begin
      Params[i] := true;
      showdockerip := true;
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
      s := s + ' ' + ParamStr(i);
    End;
  End;
  If s <> '' Then Begin
    log('Unknown parameter:' + s, llWarning);
  End;
  setlength(params, 0);
  // Es mus mindestens der Port gesetzt werden.
  If port = -1 Then Begin
    Log('No argument -p <portnumber> passed. Stopping now.', llCritical);
    Printhelp;
    exit;
  End
  Else Begin
    // Get server IP address for display
    serverIP := '127.0.0.1'; // Default to localhost
    Try
      adapters := GetLocalIPs();
      // Find first non-localhost IP address (prefer network IP over localhost)
      For j := 0 To High(adapters) Do Begin
        If (adapters[j].IpAddress <> '127.0.0.1') And (adapters[j].IpAddress <> '') And
          ((pos('docker', lowercase(adapters[j].AdapterName)) = 0) Or showdockerip) Then Begin
          serverIP := adapters[j].IpAddress;
          break;
        End;
      End;
    Except
      // If we can't get IP address, use localhost
      serverIP := '127.0.0.1';
    End;

    // Display server connection info in a nice formatted box
    Log('===========================================', llinfo);
    Log('', llinfo);
    Log('        Server is Running on address', llinfo);
    Log('', llinfo);
    Log('                ' + serverIP, llinfo);
    Log('', llinfo);
    Log('                 Port: ' + inttostr(port), llinfo);
    Log('', llinfo);
    Log('===========================================', llinfo);
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
    server.LoadPlayerStatistiks();
    Try
      server.execute;
    Except
      On e: Exception Do Begin
        log(e.Message, llFatal);
      End;
    End;
    server.SavePlayerStatistiks();
    server.SaveStatistiks();
    log('Shutting down, thanks for playing.', llInfo);
    // 4. Fertig, aufräumen
    server.free;
  End;
  LogLeave(EnterID);
  log('Max Stack depth: ' + IntToStr(MaxStackDepth), lldebug);

End.

