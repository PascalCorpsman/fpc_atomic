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
Unit uatomic_server;

{$MODE ObjFPC}{$H+}

{$I ../client/globaldefines.inc}

Interface

Uses
  Classes, SysUtils, Lnet, crt,
  uChunkmanager, uatomic_common, uatomic_messages, uatomic_field, uai_types;

Type

  TGameStatistik = Record
    Total: Array[TStatSelector] Of UInt64;
    LastRun: Array[TStatSelector] Of UInt64;
  End;

  TGameState = (
    gsWaitForPlayersToConnect // Wir warten auf Spieler die Mit Spielen wollen
    , gsPlayerSetup // Die Spieler wollen ihre Farben Editieren
    , gsMapSetup // Die Spieler wollen die Eigenschaften der Karte Editieren
    , gsPlaying // Die Spieler Spielen auf der Karte
    , gsShowHighscore // Anzeige der Highscores
    );

  TUidInfos = Record
    Uid: Integer;
    Fields: TFieldHashNameList;
  End;

  tFrameLog = Record
    TimeStamp: QWord;
    AccumulatedSize: uint64;
    Count: integer;
  End;

  { TServer }

  TServer = Class
  private
    fKickAllPlayer: boolean; // If True, the server kicks every player during the next "Idle"
    fFrameLog: tFrameLog;
    fStatistik: TGameStatistik;
    fRandomMap: Boolean;
    fLastHeartbeatTimestamp: int64; // Der Zeitpunkt an welchen der Letzte Heartbeat versendet wird
    fPlayingTimeasc: int64; // Zeit in ms die die runde schon läuft
    fPlayingTimedesc: int64; // Zeit in ms die die Runde noch läuft

    fpausing: Boolean;
    fSyncPause: Boolean;
    foldpausevalue: Boolean;
    fPauseTimestamp: int64; // Der Zeitpunkt, an dem eine Pause gestartet wurde
    FLastFrameTimestamp: int64; // Zeitpunkt an dem das Letzte "Frame" Berechnet wurde
    fLastClientUpdateTimestamp: int64; // Zeitpunkt an dem zuletzt alle Clients Aktualisiert wurden

    fSettings: TAtomicSettings;
    fFields: Array Of TAtomicField;
    fConnectedClientCount: integer; // Anzahl der Tatsächlich verbundenen Clients, wird bestimmt bei HandleSwitchToPlayerSetup und ist immer <= GetActivePlayerCount()
    fActualField: TAtomicField; // Das Feld, welches für dieses Match Aktiv ist, Wird mittels miUpdateFieldSetup gesetzt

    fUDP: TLUdp; // Zum Empfangen und Senden der Aktiven Server Verbindungen
    fTCP: TLTcp; // Die L-Net TCP Komponente
    fChunkManager: TChunkManager; // Der Chunkmanager zum senden der Daten
    fTCPPort: integer; // Der Port auf welchem wir den Chunkmanager gestaret haben (für die UDP-Broadcaster)
    factive: Boolean; // True, so lange clients verbunden sind
    fLastActiveTickTimestamp: Int64; // Zum Bestimmen ob der Server sich automatisch beenden soll, wenn Keine Spieler mehr aktiv sind.

    fGameState: TGameState;
    fAutotimeout: integer;
    fPLayer: TPlayers;
    fUidInfos: Array Of TUidInfos; // Informationen die am Spieler hängen aber nicht an den Positionen ..

    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);
    Procedure OnError(Const msg: String; aSocket: TLSocket);

    Procedure OnUDPReceiveEvent(aSocket: TLSocket);
    Procedure OnUDPError(Const msg: String; aSocket: TLSocket);
    Procedure OnUDPDisconnect(aSocket: TLSocket);

    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
    Procedure ResetFieldAvailabe();
    Function SendChunk(UserDefinedID: Integer; Data: TStream; UID: integer): Boolean;
    Procedure SendSplashMessage(msg: String; TargetUID: integer);
    Procedure SendSettings();

    Procedure HandleRequestUserLogin(Const Stream: TStream; UID: integer);
    Procedure HandleSwitchToPlayerSetup;
    Procedure HandleSwitchToMapProperties(UID: Integer);
    Procedure HandleStartGame();
    Procedure HandleStartRound();
    Procedure HandleTogglePause();
    Procedure HandleShowVictory();
    Procedure HandlePlayerGetsPowerUp(Var Player: TPlayer; PlayerIndex: integer;
      PowerUp: TPowerUps);
    Procedure HandlePlaySoundEffect(PlayerIndex: integer; Effect: TSoundEffect);
    Procedure HandleStatisticCallback(StatSelector: TStatSelector; Value: uint64 = 1);

    Procedure HandleLoadSettings(Const Stream: TStream);
    Procedure HandleChangePlayerKey(PlayerIndex, Direction: Integer; PlayerName: String; UID: Integer);
    Procedure HandleSwitchToWaitForPlayersToConnect;
    Procedure HandleUpdateFieldSetup(Const Stream: TStream; Uid: integer);
    Procedure HandleReceiveHeartBeat(p: integer; t: int64);
    Procedure HandleClientKeyEvent(Const Stream: TStream);

    Procedure RefreshAllPlayerStats(Uid: integer);
    Procedure PlayerLeaves(PlayerUid: integer);
    Function GetActivePlayerCount(): Integer; // Ermittelt alle Spieler, derren UID <> NoPlayer ist !
    Procedure EvalFieldHashList(Const List: TFieldHashNameList; SendToMaster: Boolean);

    Procedure CheckSynchrons;
    Procedure ApplyPause(Value: Boolean);
    Procedure CreateNewFrame; // Quasi das Virtuelle render
    Procedure UpdateAllClients; // Aktualisierung aller Clients
    Procedure SendPlayerStatistiks; // Am Ende einer Runde müssen alle Spieler über die Aktuelle Statistik informiert werden
    Procedure EndGameCheck;
    Function MatchFinished(): Boolean;

    Procedure LoadAi();
    Procedure HurryHandling;
  public
    Constructor Create(Port, AutoTimeOut: Integer);
    Destructor Destroy(); override;
    Procedure Execute;
    Procedure LoadStatistiks();
    Procedure SaveStatistiks();
  End;

Implementation

Uses FileUtil, LazUTF8, LazFileUtils, uvectormath, math, IniFiles, uai, uip;

{ Helper function to resolve data directory path }

Function ResolveResourceBase(BasePath: String): String;
Var
  testPath: String;
  appBundlePath: String;
Begin
  // Normalize base path to absolute path
  BasePath := ExpandFileName(IncludeTrailingPathDelimiter(BasePath));
  
  // Try multiple locations for data directory
  // 1. Direct relative to executable (works with symlinks)
  testPath := BasePath + 'data';
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    exit;
  End;
  // 2. Relative path from MacOS directory in .app bundle
  testPath := ExpandFileName(BasePath + '../Resources/data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    exit;
  End;
  // 3. Try symlink path (../../data from MacOS) - for shared data directory
  testPath := ExpandFileName(BasePath + '../../data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    exit;
  End;
  // 4. Try symlink path (../../../data from MacOS) - alternative symlink location
  testPath := ExpandFileName(BasePath + '../../../data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    exit;
  End;
  // 5. Try path next to .app bundle (for cases where data is outside the bundle)
  // If BasePath contains ".app/Contents/MacOS/", try going up to the .app bundle's parent directory
  If Pos('.app/Contents/MacOS/', BasePath) > 0 Then Begin
    appBundlePath := Copy(BasePath, 1, Pos('.app/Contents/MacOS/', BasePath) + 4); // Get path up to ".app"
    appBundlePath := ExtractFilePath(ExcludeTrailingPathDelimiter(appBundlePath)); // Get parent directory of .app
    testPath := ExpandFileName(appBundlePath + 'data');
    If DirectoryExistsUTF8(testPath) Then Begin
      Result := IncludeTrailingPathDelimiter(testPath);
      exit;
    End;
  End;
  // Fallback: use base path (may not exist, but at least we tried)
  Result := BasePath + 'data' + PathDelim;
End;

{ TServer }

Constructor TServer.Create(Port, AutoTimeOut: Integer);
Var
  sl: TStringList;
  i: Integer;
  adapters: TNetworkAdapterList;
  serverIP: String;
  j: Integer;
Begin
  log('TServer.create', lltrace);
  Inherited create;
  fChunkManager := Nil;
  factive := false;
  fFields := Nil;
  HandleSwitchToWaitForPlayersToConnect;
  fAutotimeout := AutoTimeOut;
  fTCP := TLTcp.Create(Nil);
  fTCP.ReuseAddress := true; // Bei Absturz kann so sofort wieder neu verbunden werden
  fTCP.OnAccept := @OnAccept;
  fTCP.OnError := @OnError;
  fTCP.OnDisconnect := @OnDisconnect;
  fUDP := TLUdp.Create(Nil);
  fUDP.OnReceive := @OnUDPReceiveEvent;
  fUDP.OnError := @OnUDPError;
  fUDP.OnDisconnect := @OnUDPDisconnect;

  fChunkManager := TChunkManager.create;
  fChunkManager.RegisterConnection(fTCP);
  fChunkManager.OnReceivedChunk := @OnReceivedChunk;
  // Start network thread for non-blocking network processing
  fChunkManager.StartNetworkThread();
  log('Network thread started for server', llInfo);

  fLastActiveTickTimestamp := GetTickCount64;
  fTCPPort := Port;

  // Laden aller Felder
  // Resolve data directory path - check multiple locations for .app bundle compatibility
  sl := FindAllDirectories(ResolveResourceBase(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))) + 'maps', false);
  sl.Sorted := true;
  sl.Sort; // Ist beim Client wichtig, beim Server wäre es theoretisch egal, so ists aber leichter zum debuggen
  setlength(fFields, sl.Count);
  For i := 0 To sl.Count - 1 Do Begin
    fFields[i] := TAtomicField.Create(@HandlePlaySoundEffect, @HandleStatisticCallback);
    If Not fFields[i].loadFromDirectory(sl[i]) Then Begin
      Raise exception.create('Error, unable to load field:' + sl[i]);
    End;
  End;
  sl.free;
  setlength(fFields, high(fFields) + 2);
  fFields[high(fFields)] := TAtomicRandomField.Create(Nil, Nil); // Die initialisiert sich bereits richtig ;)
  LoadAi();
  
  If Not fUDP.Listen(UDPPingPort) Then Begin
    log('Error, unable to listen on port: ' + inttostr(UDPPingPort), llFatal);
    LogLeave;
    exit;
  End;
  If Not fChunkManager.Listen(Port) Then Begin
    log('Error could not listen on port: ' + inttostr(port), llFatal);
    LogLeave;
    exit;
  End;
  factive := true; // Haben wir es bis hier her geschafft, darf die Execute auch "laufen"
  LogLeave;
End;

Destructor TServer.Destroy;
Var
  i: Integer;
Begin
  log('TServer.destroy', lltrace);
  If assigned(fChunkManager) Then Begin
    fChunkManager.Disconnect(true);
    fChunkManager.free;
  End;
  // Das Disconnect macht ja der ChunkManager !
  While fTCP.Connected Do Begin
    fTCP.CallAction;
  End;
  ftcp.free;
  // Den UDP auch sauber platt machen
  If fUDP.Connected Then Begin
    fUDP.Disconnect(true);
    While fUDP.Connected Do Begin
      fUDP.CallAction;
    End;
  End;
  fUDP.free;
  For i := 0 To high(fFields) Do Begin
    fFields[i].Free;
  End;
  setlength(fFields, 0);
  UnLoadAiLib(); // da ist das AiDeInit mit drin ;)
  LogLeave;
End;

Procedure TServer.OnAccept(aSocket: TLSocket);
Begin
  // Wir Aktzeptieren eine Engehende Verbindung
  log('TServer.OnAccept : ' + aSocket.PeerAddress, llTrace);
  fChunkManager.SetNoDelay(true);
  LogLeave;
End;

Procedure TServer.OnDisconnect(aSocket: TLSocket);
Var
  uid: integer;
Begin
  // Wir verlieren einen Spieler
  If assigned(asocket) Then Begin
    log('TServer.OnDisconnect : ' + aSocket.PeerAddress, llTrace);
  End
  Else Begin
    log('TServer.OnDisconnect', llTrace);
  End;
  uid := fChunkManager.SocketToUID(aSocket);
  PlayerLeaves(uid);
  LogLeave;
End;

Procedure TServer.OnError(Const msg: String; aSocket: TLSocket);
Begin
  log('TServer.OnError', llTrace);
  If assigned(asocket) Then Begin
    log(asocket.PeerAddress + msg, llError)
  End
  Else Begin
    log(msg, llError)
  End;
  LogLeave;
End;

Procedure TServer.OnUDPError(Const msg: String; aSocket: TLSocket);
Begin
  log('TServer.OnUDPError', llTrace);
  If assigned(asocket) Then Begin
    log(asocket.PeerAddress + msg, llError)
  End
  Else Begin
    log(msg, llError)
  End;
  LogLeave;
End;

Procedure TServer.OnUDPDisconnect(aSocket: TLSocket);
Begin
  log('TServer.OnUDPDisconnect', llTrace);
  If assigned(asocket) Then Begin
    log('TServer.OnUDPDisconnect: ' + asocket.PeerAddress, llError)
  End
  Else Begin
    log('TServer.OnUDPDisconnect', llError)
  End;
  LogLeave;
End;

Procedure TServer.OnUDPReceiveEvent(aSocket: TLSocket);
Var
  UserName: String;
  Buffer: Array[0..1024 - 1] Of byte;
  cnt, i, ReadCnt: Integer;
  b: Byte;
Begin
  If assigned(aSocket) Then Begin
    log('TServer.OnUDPReceiveEvent : ' + aSocket.PeerAddress, llTrace);
  End
  Else Begin
    log('TServer.OnUDPReceiveEvent', llTrace);
  End;
  Repeat
    (*
     * Es muss immer alles gelesen werden, sonst passieren merkwürdige Dinge
     *)
    ReadCnt := aSocket.Get(buffer, 1024);
    If fGameState = gsWaitForPlayersToConnect Then Begin // Nur im Editor Modus, aktzeptieren wir Spieler, also Antworten wir auch nur wenn wir im Editor Modus sind
      If GetActivePlayerCount() <> 0 Then Begin // Es ist midnestens 1 Spieler Angemeldet, also geben wir den Namen des 1. Spielers an
        (*
         * Das ist hier zwar Richtig, aber eigentlich Falsch, korrekt wäre das Suchen des 1. Spielers mit UID <> NoPlayer !
         * -> Aktuell wertet der Client den String aber eh nicht aus, von daher, who cares...
         *)
        UserName := fPLayer[0].UserName + 's game';
      End
      Else Begin // Gar kleine Spieler, dann nur den Servernamen
        UserName := 'no user connected';
      End;
      username := username + ':' + inttostr(fTCPPort);
      cnt := length(username);
      b := 21; // CTD hat hier 42 -> Wir wollen das die beiden nicht "Kompatibel" sind !
      For i := 0 To cnt - 1 Do Begin
        buffer[i] := ord(username[i + 1]);
        b := b Xor buffer[i];
      End;
      Buffer[cnt] := b;
      aSocket.Send(buffer, cnt + 1);
    End;
  Until ReadCnt = 0;
  LogLeave;
End;

Procedure TServer.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Var
  s: String;
  i, j: integer;
  ts: QWord;
Begin
  //responceID := Chunk.UserDefinedID And $FFFF0000;
  HandleStatisticCallback(sTotalNetworkPacketsIn);
  HandleStatisticCallback(sTotalNetworkBytesIn, Chunk.Data.Size + ChunkManagerHeaderLen);
{$IFDEF DoNotLog_CyclicMessages}
  If ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) And
    ((Chunk.UserDefinedID And $FFFF) <> miClientKeyEvent) Then
{$ENDIF}
    log(format('TServer.OnReceivedChunk : %d, %s', [Chunk.UID, MessageIdentifierToString(Chunk.UserDefinedID)]), llTrace);

  Case (Chunk.UserDefinedID And $FFFF) Of
    miTogglePause: Begin
        HandleTogglePause();
      End;
    miClientKeyEvent: Begin
        HandleClientKeyEvent(Chunk.Data);
      End;
    miHeartBeat: Begin
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        ts := 0;
        chunk.Data.Read(ts, sizeof(ts));
        HandleReceiveHeartBeat(i, ts);
      End;
    miRequestLogin: Begin
        HandleRequestUserLogin(Chunk.data, chunk.UID);
      End;
    miSwitchToPlayerSetup: Begin
        HandleSwitchToPlayerSetup();
      End;
    miUpdateSettings: Begin
        HandleLoadSettings(Chunk.Data);
      End;
    miChangePlayerKey: Begin
        i := -1;
        chunk.Data.Read(i, sizeof(i));
        j := 0;
        chunk.Data.Read(j, sizeof(j));
        s := Chunk.Data.ReadAnsiString;
        HandleChangePlayerKey(i, j, s, Chunk.UID);
      End;
    miSwitchToFieldSetup: Begin
        HandleSwitchToMapProperties(Chunk.UID);
      End;
    miUpdateFieldSetup: Begin
        HandleUpdateFieldSetup(Chunk.Data, Chunk.UID);
      End;
    miStartGame: Begin
        If fGameState = gsShowHighscore Then Begin
          If MatchFinished() Then Begin
            HandleShowVictory();
          End
          Else Begin
            HandleStartRound();
          End;
        End
        Else Begin
          HandleStartGame();
        End;
      End;
  Else Begin
      log('Unknown user defined id : ' + inttostr(Chunk.UserDefinedID And $FFFF), llError);
    End;
  End;
{$IFDEF DoNotLog_CyclicMessages}
  If ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) And
    ((Chunk.UserDefinedID And $FFFF) <> miClientKeyEvent) Then
    LogLeave;
{$ENDIF}
End;

Procedure TServer.ResetFieldAvailabe;
Var
  i: integer;
Begin
  log('TServer.ResetFieldAvailabe', lltrace);
  For i := 0 To high(fFields) Do Begin
    fFields[i].Available := true;
  End;
  LogLeave;
End;

Function TServer.SendChunk(UserDefinedID: Integer; Data: TStream; UID: integer
  ): Boolean;
Var
  i: integer;
  dataLen: int64;
Begin
{$IFDEF DoNotLog_CyclicMessages}
  If ((UserDefinedID And $FFFF) <> miUpdateGameData) And
    ((UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    log(format('TServer.SendChunk : %d, %s', [uid, MessageIdentifierToString(UserDefinedID)]), llTrace);
  datalen := ChunkManagerHeaderLen;
  If assigned(data) Then dataLen := dataLen + Data.Size;
  result := fChunkManager.SendChunk(UserDefinedID, data, uid);
  If uid = 0 Then Begin
    datalen := dataLen * max(1, fConnectedClientCount);
  End
  Else Begin
    If uid < 0 Then Begin
      // An Alle außer einem
      datalen := dataLen * max(0, fConnectedClientCount - 1);
    End
    Else Begin
      // Die Länge stimmt es geht ja nur an einen ;)
    End;
  End;
  HandleStatisticCallback(sTotalNetworkPacketsOut);
  HandleStatisticCallback(sTotalNetworkBytesOut, dataLen);
  fFrameLog.AccumulatedSize := fFrameLog.AccumulatedSize + dataLen;
  fFrameLog.Count := fFrameLog.Count + 1;
  If Not result Then Begin
    If uid < 0 Then Begin
      (*
       * Ein Broadcast (negativer UID) zu allen außer einem Spieler
       * Ein Broadcast bringt nur was wenn wir mehr als 1 Spieler haben ;)
       *)
      If GetActivePlayerCount() > 1 Then Begin
        log('Could not send broadcast (uid=' + inttostr(uid) + '), maybe no more clients connected.', llWarning);
      End
      Else Begin
        log('Could not send broadcast (uid=' + inttostr(uid) + '), only one or no active players.', llWarning);
      End;
    End
    Else Begin
      // Try to find the player with this UID
      For i := low(fPLayer) To high(fPLayer) Do Begin
        If fPLayer[i].UID = UID Then Begin
          log('Could not send to player : ' + fPLayer[i].UserName + ' (uid=' + inttostr(uid) + ')', llCritical);
          LogLeave;
          exit;
        End;
      End;
      // Player not found
      log('Could not send to player with uid=' + inttostr(uid) + ' (player not found)', llCritical);
    End;
  End;
{$IFDEF DoNotLog_CyclicMessages}
  If (UserDefinedID <> miUpdateGameData) And
    (UserDefinedID <> miHeartBeat) Then
{$ENDIF}
    LogLeave;
End;

Procedure TServer.SendSplashMessage(msg: String; TargetUID: integer);
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.WriteAnsiString(msg);
  SendChunk(miSplashHint, m, TargetUID);
End;

Procedure TServer.HandleRequestUserLogin(Const Stream: TStream; UID: integer);
Var
  m: TMemoryStream;
  index, i, j, cnt: integer;
  Username: String;
  ClientVersion: uint32;
  fieldlist: TFieldHashNameList;
  ClientMode: Byte;
Begin
  log('TServer.HandleUserLoginRequest', llTrace);
  ClientVersion := $FFFFFFFF;
  Stream.Read(ClientVersion, sizeof(ClientVersion));
  m := TMemoryStream.Create;
  // 0. Check ob die beiden Versionen Compatibel sind
  If ProtocollVersion <> ClientVersion Then Begin
    i := EC_Invalid_Versions;
    m.Write(i, sizeof(i));
    SendChunk(miRequestLoginResult, m, UID);
    LogLeave;
    exit;
  End;

  ClientMode := 0;
  Stream.Read(ClientMode, sizeof(ClientMode));
  Username := Stream.ReadAnsiString;

{$IFDEF Release}
  If ClientMode <> GameModeRelease Then
{$ELSE}
  If ClientMode <> GameModeDebug Then
{$ENDIF}Begin
    i := EC_Invalid_Mode_Versions;
    m.Write(i, sizeof(i));
    SendChunk(miRequestLoginResult, m, UID);
    LogLeave;
    exit;
  End;
  //// 1. Check : Passwort
  //If Password <> fPassword Then Begin
  //  i := EC_Invalid_Password;
  //  m.Write(i, sizeof(i));
  //  SendChunk(miRequestLoginResult, m, UID);
  //  LogLeave;
  //  exit;
  //End;
  // 2. Check : Darf der Spieler überhaupt verbinden = Läuft gerade ein Spiel ?
  If (fGameState <> gsWaitForPlayersToConnect) Then Begin
    i := EC_game_full;
    m.Write(i, sizeof(i));
    SendChunk(miRequestLoginResult, m, UID);
    LogLeave;
    exit;
  End;
  // 3. Check : Gibt es den Spielernamen bereits ?
  cnt := 0;
  For j := low(fPLayer) To high(fPLayer) Do Begin
    If (fPLayer[j].UID <> NoPlayer) Then Begin
      inc(cnt);
      If (fPLayer[j].UserName = Username) Then Begin
        i := EC_User_already_exists;
        m.Write(i, sizeof(i));
        SendChunk(miRequestLoginResult, m, UID);
        LogLeave;
        exit;
      End;
    End;
  End;
  // 4. Check: Max 10 Spieler
  If cnt >= length(fPLayer) Then Begin
    i := EC_Too_Much_Player;
    m.Write(i, sizeof(i));
    SendChunk(miRequestLoginResult, m, UID);
    LogLeave;
    exit;
  End;
  // Alles I.O. Der User kann beitreten
  // Suchen eines Freien Platzes für den neuen Spieler ;)
  index := -1;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = NoPlayer Then Begin
      index := i;
      break;
    End;
  End;
  (*
   * Oben der Check ging durch und hier finden wir nix, das darf nicht sein, alles canceln !
   *)
  If index = -1 Then Begin
    log('Error player overflow', llCritical);
    HandleSwitchToWaitForPlayersToConnect;
    LogLeave;
    exit;
  End;
  // Der erste Spieler Loggt sich ein -> Wird zum Bestimmenden ;)
  If cnt = 0 Then Begin
    assert(fSettings.MasterUid = -1, 'First player connects, but old data was not cleared.');
    fSettings.MasterUid := UID;
    ResetFieldAvailabe();
  End;
  fPLayer[index].UID := UID;
  fPLayer[index].UserName := Username;
  fPLayer[index].Keyboard := ks0; // Beim Anlegen gibt es nur den Spieler 1 mit Keyboard 1
  fPLayer[index].Kills := 0;
  fPLayer[index].Score := 0;
  i := -1;
  Stream.Read(i, SizeOf(i));
  fieldlist := Nil;
  setlength(fieldlist, i);
  For i := 0 To high(fieldlist) Do Begin
    fieldlist[i].Name := Stream.ReadAnsiString;
    Stream.Read(fieldlist[i].Hash, sizeof(fieldlist[i].Hash));
  End;
  setlength(fUidInfos, high(fUidInfos) + 2);
  fUidInfos[high(fUidInfos)].Uid := UID;
  fUidInfos[high(fUidInfos)].Fields := fieldlist;
  log(format('Accepted player : %s as %d, now %d player in logged in', [Username, uid, cnt + 1]), llInfo);
  i := EC_No_Error;
  m.Write(i, sizeof(i));
  i := uid;
  m.Write(i, sizeof(i));
  i := fSettings.MasterUid;
  m.Write(i, sizeof(i));
  SendChunk(miRequestLoginResult, m, UID);
  // Allen Mitteilen das wir nen neuen Spieler haben
  RefreshAllPlayerStats(0); // Allen Anwesenden Mitteilen Welche Spieler es so gibt
  // Der Spieler ist Drin, nun müssen wir noch die Verfügbaren Karten von Beiden "Checken"
  // Und dem Master Die Schnittmenge Mitteilen
  EvalFieldHashList(fieldlist, true);
  // CRITICAL: Always send available field list to all clients after evaluation
  // This ensures all clients (including newly connected ones) receive the updated field list
  // The master already got it from EvalFieldHashList above, but we need to send to all
  // to ensure remote clients get the field list even if they're not the master
  EvalFieldHashList(fieldlist, false); // Send to all clients (including the new one)
  LogLeave;
End;

Procedure TServer.HandleSwitchToPlayerSetup;
Begin
  log('TServer.HandleSwitchToPlayerSetup', llTrace);
  SendSettings(); // Alle Clients werden nun Settingstechnisch Gleich Geschaltet
  fConnectedClientCount := GetActivePlayerCount();
  fGameState := gsPlayerSetup;
  SendChunk(miSwitchToPlayerSetup, Nil, 0);
  LogLeave;
End;

Procedure TServer.SendSettings;
Var
  m: TMemoryStream;
Begin
  log('TServer.SendSettings', llTrace);
  m := TMemoryStream.Create;
  m.write(fSettings.TeamPlay, sizeof(fSettings.TeamPlay));
  m.write(fSettings.RandomStart, sizeof(fSettings.RandomStart));
  // Nodename hat der Server nicht
  m.write(fSettings.ConveyorSpeed, sizeof(fSettings.ConveyorSpeed));
  SchemeToStream(m, fSettings.Scheme);
  m.write(fSettings.PlayTime, sizeof(fSettings.PlayTime));
  m.write(fSettings.LostPlayersRevertToAI, sizeof(fSettings.LostPlayersRevertToAI));
  // Playsounds hat der Server nicht
  // Keyboard hat der Server nicht
  m.WriteAnsiString(fSettings.LastPlayedField);
  m.write(fSettings.LastPlayedFieldHash, sizeof(fSettings.LastPlayedFieldHash));
  m.write(fSettings.LastWinsToWinMatch, sizeof(fSettings.LastWinsToWinMatch));
  // Port hat der Server nicht
  // CheckForUpdates hat der Server nicht
  (*
   * Die Settings kamen ja von der MasterUid, also kriegen sie Alle bis auf der Master ;)
   *)
  SendChunk(miUpdateSettings, m, -fSettings.MasterUid);
  LogLeave;
End;

Procedure TServer.HandleLoadSettings(Const Stream: TStream);
Var
  i: Integer;
Begin
  log('TServer.HandleLoadSettings', llTrace);
  Stream.read(fSettings.TeamPlay, sizeof(fSettings.TeamPlay));
  Stream.read(fSettings.RandomStart, sizeof(fSettings.RandomStart));
  // Nodename hat der Server nicht
  Stream.read(fSettings.ConveyorSpeed, sizeof(fSettings.ConveyorSpeed));
  If Not SchemeFromStream(Stream, fSettings.Scheme) Then Begin
    log('Error, could not load scheme from stream', llCritical);
    HandleSwitchToWaitForPlayersToConnect();
    LogLeave;
    exit;
  End;
  Stream.read(fSettings.PlayTime, sizeof(fSettings.PlayTime));
  Stream.read(fSettings.LostPlayersRevertToAI, sizeof(fSettings.LostPlayersRevertToAI));
  // Playsounds hat der Server nicht
  // Keyboard hat der Server nicht
  fSettings.LastPlayedField := Stream.ReadAnsiString();
  Stream.read(fSettings.LastPlayedFieldHash, sizeof(fSettings.LastPlayedFieldHash));
  Stream.read(fSettings.LastWinsToWinMatch, sizeof(fSettings.LastWinsToWinMatch));
  // Port hat der Server nicht
  // CheckForUpdates hat der Server nicht

  // Übernehmen der Team Informationen in die FPLayer
  For i := 0 To high(fPLayer) Do Begin
    fPLayer[i].Team := fSettings.Scheme.PlayerStartPositions[i].Team;
  End;
  LogLeave;
End;


(*
 * Direction = 1
 *   Master:
 *     Off        -> Uid (key0)
 *     Uid (key0) -> Uid (key1)
 *     Uid (key1) -> AI
 *     AI         -> Off
 *   Jeder Andere:
 *     Off        -> Uid (key0)
 *     Uid (key0) -> Uid (key1)
 *     Uid (key1) -> Off
 * Directoin = -1
 *   Master:
 *     Off        -> AI
 *     AI         -> Uid (key1)
 *     Uid (key1) -> Uid (key0)
 *     Uid (key0) -> Off
 *   Jeder Andere:
 *     Off        -> Uid (key1)
 *     Uid (key1) -> Uid (key0)
 *     Uid (key0) -> Off
 * Alles Andere ist nicht erlaubt -> Keine Änderung
 *)

Procedure TServer.HandleChangePlayerKey(PlayerIndex, Direction: Integer;
  PlayerName: String; UID: Integer);
  
  // Helper function to get next input method in cycle
  Function GetNextKeySet(Current: TKeySet; IsMaster: Boolean): TKeySet;
  Begin
    Case Current Of
      ks0: result := ks1;
      ks1: result := ksJoy1;
      ksJoy1: result := ksJoy2;
      ksJoy2: Begin
          If IsMaster Then Begin
            result := ks0; // Will be set to AI, this is just a placeholder
          End
          Else Begin
            result := ks0; // Will be set to NoPlayer, this is just a placeholder
          End;
        End;
    Else
      result := ks0;
    End;
  End;
  
  // Helper function to get previous input method in cycle
  Function GetPrevKeySet(Current: TKeySet; IsMaster: Boolean): TKeySet;
  Begin
    Case Current Of
      ks0: Begin
          If IsMaster Then Begin
            result := ksJoy2; // Will be set to AI, this is just a placeholder
          End
          Else Begin
            result := ksJoy2; // Will be set to NoPlayer, this is just a placeholder
          End;
        End;
      ks1: result := ks0;
      ksJoy1: result := ks1;
      ksJoy2: result := ksJoy1;
    Else
      result := ks0;
    End;
  End;
  
Begin
  log(format('TServer.HandleChangePlayerKey (%d, %d, %s)', [PlayerIndex, Direction, PlayerName]), llTrace);
  If (PlayerIndex = -1) Or (Direction = 0) Or (UID = 0) Then Begin
    LogLeave;
    exit;
  End;
  
  // Master cycle: Off -> Keyboard 0 -> Keyboard 1 -> Joy 1 -> Joy 2 -> AI -> Off
  // Non-Master cycle: Off -> Keyboard 0 -> Keyboard 1 -> Joy 1 -> Joy 2 -> Off
  
  If Direction > 0 Then Begin
    // Forward cycle
    If UID = fSettings.MasterUid Then Begin
      // Master player
      If fPLayer[PlayerIndex].UID = NoPlayer Then Begin
        // Off -> Keyboard 0
        fPLayer[PlayerIndex].UID := UID;
        fPLayer[PlayerIndex].Keyboard := ks0;
        fPLayer[PlayerIndex].UserName := PlayerName;
        RefreshAllPlayerStats(0);
      End
      Else If fPLayer[PlayerIndex].UID = UID Then Begin
        // Same player, cycle through input methods
          If fPLayer[PlayerIndex].Keyboard = ks0 Then Begin
            fPLayer[PlayerIndex].Keyboard := ks1;
          End
        Else If fPLayer[PlayerIndex].Keyboard = ks1 Then Begin
          fPLayer[PlayerIndex].Keyboard := ksJoy1;
        End
        Else If fPLayer[PlayerIndex].Keyboard = ksJoy1 Then Begin
          fPLayer[PlayerIndex].Keyboard := ksJoy2;
        End
        Else If fPLayer[PlayerIndex].Keyboard = ksJoy2 Then Begin
          // Joy 2 -> AI
            fPLayer[PlayerIndex].UID := AIPlayer;
            fPLayer[PlayerIndex].UserName := '';
          End;
          RefreshAllPlayerStats(0);
        End
      Else If fPLayer[PlayerIndex].UID = AIPlayer Then Begin
        // AI -> Off
            fPLayer[PlayerIndex].UID := NoPlayer;
            fPLayer[PlayerIndex].UserName := '';
            RefreshAllPlayerStats(0);
      End;
    End
    Else Begin
      // Non-Master player
      If fPLayer[PlayerIndex].UID = NoPlayer Then Begin
        // Off -> Keyboard 0
        fPLayer[PlayerIndex].UID := UID;
        fPLayer[PlayerIndex].Keyboard := ks0;
        fPLayer[PlayerIndex].UserName := PlayerName;
        RefreshAllPlayerStats(0);
      End
      Else If fPLayer[PlayerIndex].UID = UID Then Begin
        // Same player, cycle through input methods
          If fPLayer[PlayerIndex].Keyboard = ks0 Then Begin
            fPLayer[PlayerIndex].Keyboard := ks1;
          End
        Else If fPLayer[PlayerIndex].Keyboard = ks1 Then Begin
          fPLayer[PlayerIndex].Keyboard := ksJoy1;
        End
        Else If fPLayer[PlayerIndex].Keyboard = ksJoy1 Then Begin
          fPLayer[PlayerIndex].Keyboard := ksJoy2;
        End
        Else If fPLayer[PlayerIndex].Keyboard = ksJoy2 Then Begin
          // Joy 2 -> Off
            fPLayer[PlayerIndex].UID := NoPlayer;
            fPLayer[PlayerIndex].UserName := '';
          End;
          RefreshAllPlayerStats(0);
      End;
    End;
  End
  Else Begin
    // Backward cycle
    If UID = fSettings.MasterUid Then Begin
      // Master player
      If fPLayer[PlayerIndex].UID = NoPlayer Then Begin
        // Off -> AI
        fPLayer[PlayerIndex].UID := AIPlayer;
        fPLayer[PlayerIndex].UserName := '';
        RefreshAllPlayerStats(0);
      End
      Else If fPLayer[PlayerIndex].UID = AIPlayer Then Begin
        // AI -> Joy 2
          fPLayer[PlayerIndex].UID := UID;
        fPLayer[PlayerIndex].Keyboard := ksJoy2;
          fPLayer[PlayerIndex].UserName := PlayerName;
          RefreshAllPlayerStats(0);
        End
      Else If fPLayer[PlayerIndex].UID = UID Then Begin
        // Same player, cycle backwards through input methods
        If fPLayer[PlayerIndex].Keyboard = ksJoy2 Then Begin
          fPLayer[PlayerIndex].Keyboard := ksJoy1;
        End
        Else If fPLayer[PlayerIndex].Keyboard = ksJoy1 Then Begin
          fPLayer[PlayerIndex].Keyboard := ks1;
        End
        Else If fPLayer[PlayerIndex].Keyboard = ks1 Then Begin
              fPLayer[PlayerIndex].Keyboard := ks0;
            End
        Else If fPLayer[PlayerIndex].Keyboard = ks0 Then Begin
          // Keyboard 0 -> Off
              fPLayer[PlayerIndex].UID := NoPlayer;
              fPLayer[PlayerIndex].UserName := '';
            End;
            RefreshAllPlayerStats(0);
      End;
    End
    Else Begin
      // Non-Master player
      If fPLayer[PlayerIndex].UID = NoPlayer Then Begin
        // Off -> Joy 2
        fPLayer[PlayerIndex].UID := UID;
        fPLayer[PlayerIndex].Keyboard := ksJoy2;
        fPLayer[PlayerIndex].UserName := PlayerName;
        RefreshAllPlayerStats(0);
      End
      Else If fPLayer[PlayerIndex].UID = UID Then Begin
        // Same player, cycle backwards through input methods
        If fPLayer[PlayerIndex].Keyboard = ksJoy2 Then Begin
          fPLayer[PlayerIndex].Keyboard := ksJoy1;
        End
        Else If fPLayer[PlayerIndex].Keyboard = ksJoy1 Then Begin
          fPLayer[PlayerIndex].Keyboard := ks1;
        End
        Else If fPLayer[PlayerIndex].Keyboard = ks1 Then Begin
            fPLayer[PlayerIndex].Keyboard := ks0;
          End
        Else If fPLayer[PlayerIndex].Keyboard = ks0 Then Begin
          // Keyboard 0 -> Off
            fPLayer[PlayerIndex].UID := NoPlayer;
            fPLayer[PlayerIndex].UserName := '';
          End;
          RefreshAllPlayerStats(0);
      End;
    End;
  End;
  LogLeave;
End;

Procedure TServer.HandleSwitchToMapProperties(UID: Integer);
Var
  t1, t2, i, j, aicnt, pcnt: integer;
  a: Array Of Integer;
  b: Boolean;
Begin
  log(format('TServer.HandleSwitchToMapProperties (UID=%d)', [UID]), llInfo);
  aicnt := 0;
  pcnt := 0;
  t1 := 0;
  t2 := 0;
  // Prüfen ob Genug Spieler verfügbar sind und diese auch in den Richtigen Teams !
  For i := 0 To high(fPlayer) Do Begin
    If fPlayer[i].UID > 0 Then inc(pcnt);
    If fPlayer[i].UID = AIPlayer Then inc(aicnt);
    If fPlayer[i].UID <> NoPlayer Then Begin
      If fPlayer[i].Team = 0 Then Begin
        inc(t1);
      End
      Else Begin
        inc(t2);
      End;
    End;
  End;
  If (pcnt = 1) And (aicnt = 0) Then Begin
    SendSplashMessage('Only one player on the map makes no sense, please wait for other players or activate at least one ai player.', UID);
    LogLeave;
    exit;
  End;

  If fSettings.TeamPlay Then Begin
    If (t1 = 0) Or (t2 = 0) Then Begin
      SendSplashMessage('In teamplay at least one player per team ist needed.', UID);
      LogLeave;
      exit;
    End;
  End;

  // Suchen auf Doppelt vergebene Spieler
  For i := 0 To high(fPLayer) Do Begin
    For j := i + 1 To high(fPLayer) Do Begin
      If (fPLayer[i].UID > 0) And (fPLayer[i].UID = fPLayer[j].UID) And (fPLayer[i].Keyboard = fPLayer[j].Keyboard) Then Begin
        SendSplashMessage('Error, client "' + fPLayer[i].UserName + '" uses two playerslots with the same key settings.', UID);
        LogLeave;
        exit;
      End;
    End;
  End;

  // Debug logging: Show all active player slots
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID > NoPlayer Then Begin
      log(format('  Slot %d: UID=%d, Username=%s, Keyboard=%d', 
        [i, fPLayer[i].UID, fPLayer[i].UserName, Ord(fPLayer[i].Keyboard)]), llInfo);
    End;
  End;
  
  // Note: The check "all connected clients have at least one slot" was removed
  // because it incorrectly flagged local co-op as an error (one UID with multiple keyboards).
  // The existing checks are sufficient:
  // 1. Duplicate slot check (line 925-933) prevents same UID + same Keyboard
  // 2. Minimum player check (line 910-914) ensures enough players to start
  // Alle Checks gut -> Umschalten auf die Karteneigenschaften
  fGameState := gsMapSetup;
  SendChunk(miSwitchToFieldSetup, Nil, 0);
  LogLeave;
End;

Procedure TServer.HandleStartGame;
Var
  i: Integer;
Begin
  log('TServer.HandleStartGame', llTrace);
  HandleStatisticCallback(sMatchesStarted);
  // 1. Alle Player "Initialisieren
  For i := 0 To high(fPLayer) Do Begin
    fPLayer[i].Score := 0;
    fPLayer[i].Kills := 0;
  End;
  fRandomMap := (fActualField.Hash = 0) And (fActualField.Name = '');
  HandleStartRound;
  LogLeave;
End;

Procedure TServer.HandleStartRound;
  Function PowersFromScheme(Const Scheme: TScheme): TAtomicPowers;
  Begin
    result.Speed := AtomicDefaultSpeed;
    result.AvailableBombs := 1;
    // result.OverAllBombs := 1; -- Egal weil es unten "Hart" noch mal gesetzt wird
    result.FlameLen := 1;
    result.CanKickBombs := false;
    result.CanPunchBombs := false;
    result.CanGrabBombs := false;
    result.CanSpooger := false;
    result.TriggerBomb := 0;
    result.JellyBombs := false;
    If Scheme.PowerUps[puExtraBomb].Bornwith <> 0 Then Begin
      result.AvailableBombs := max(1, Scheme.PowerUps[puExtraBomb].BornWith);
    End;
    result.OverAllBombs := result.AvailableBombs;
    If Scheme.PowerUps[puLongerFlameLength].Bornwith > 1 Then Begin
      result.FlameLen := Scheme.PowerUps[puLongerFlameLength].Bornwith;
    End;
    If Scheme.PowerUps[puExtraSpeed].Bornwith > 0 Then Begin
      result.Speed := min(AtomicDefaultSpeed * power(AtomicSpeedChange, max(0, Scheme.PowerUps[puExtraSpeed].Bornwith)), AtomicMaxSpeed);
    End;
    // Manche Powerups schließen sich gegenseitig aus, dass muss hier und in HandlePlayerGetsPowerUp berücksichtigt werden
    // Die Reihenfolge hier gibt dabei die Prio mit an, im Spiel nachher löscht das neueste immer die alten

    If Scheme.PowerUps[puCanCick].Bornwith > 0 Then Begin
      result.CanKickBombs := true;
    End;
    If Scheme.PowerUps[puCanPunch].Bornwith > 0 Then Begin
      result.CanPunchBombs := true;
    End;
    If Scheme.PowerUps[puCanSpooger].Bornwith > 0 Then Begin
      result.CanSpooger := true;
      result.CanGrabBombs := false;
    End;
    If Scheme.PowerUps[puCanGrab].Bornwith > 0 Then Begin
      result.CanGrabBombs := true;
      result.CanSpooger := false;
    End;
    If Scheme.PowerUps[puGoldFlame].Bornwith > 0 Then Begin
      result.FlameLen := 99; // Egal hauptsache mehr als 15 !
    End;
    If Scheme.PowerUps[puTrigger].Bornwith > 0 Then Begin
      result.TriggerBomb := result.OverAllBombs;
    End;
    If Scheme.PowerUps[puCanJelly].Bornwith > 0 Then Begin
      result.JellyBombs := true;
    End;
  End;

Var
  a, b, c, i: Integer;
  n: QWord;
  m: TMemoryStream;
  Startindex: Array[0..length(PlayerColors) - 1] Of Integer;
  pu: TPowerUps;
Begin
  log('TServer.HandleStartRound', llTrace);
  HandleStatisticCallback(sGamesStarted);

  If fSettings.PlayTime = 0 Then Begin
    fPlayingTimedesc := -1000; // Unendlich  ;)
  End
  Else Begin
    fPlayingTimedesc := fSettings.PlayTime * 1000;
  End;
  fPlayingTimeasc := 0;
  For i := 0 To high(Startindex) Do Begin
    Startindex[i] := i;
  End;
  // Wenn Random Start -> dann permutieren wir hier die Startpositionen ;)
  If fSettings.RandomStart Then Begin
    For i := 0 To 100 Do Begin
      a := random(length(Startindex));
      b := random(length(Startindex));
      If a <> b Then Begin
        c := Startindex[a];
        Startindex[a] := Startindex[b];
        Startindex[b] := c;
      End;
    End;
  End;
  // 1. Alle Player "Initialisieren
  For i := 0 To high(fPLayer) Do Begin
    fPLayer[i].Info.Alive := fPLayer[i].UID <> NoPlayer;
    fPLayer[i].Info.Animation := raStandStill;
    fPLayer[i].Info.Value := 0;
    fPLayer[i].Info.Position := v2(fSettings.Scheme.PlayerStartPositions[Startindex[i]].x + 0.5, fSettings.Scheme.PlayerStartPositions[Startindex[i]].y + 0.5);
    fPLayer[i].Info.Direction := 0;
    fPLayer[i].Info.ColorIndex := i;
    fPLayer[i].MoveState := msStill;
    fPLayer[i].Action := aaNone;
    fPLayer[i].Flying := false;
    fPLayer[i].IsInHohle := false;
    fPLayer[i].Disease := [];
    fPLayer[i].DiseaseCounter := 0;
    fPLayer[i].IdleTimer := 0;
    fPLayer[i].Info.Dying := false; // Wir Sind alle wieder am Leben ;)
    // fPLayer[i].Team := fSettings.Scheme.PlayerStartPositions[i].Team; -- Wurde schon gemacht in HandleLoadSettings
    fPLayer[i].Powers := PowersFromScheme(fSettings.Scheme);
    For pu In TPowerUps Do Begin
      fPLayer[i].PowerUpCounter[pu] := 0;
    End;
  End;
  // 2. Init der Karte
  // Steht das Match gerade auf "Zufällige" Karten dann wählen wir hier eine neue Zufällige Karte aus
  If fRandomMap Then Begin
    fActualField := fFields[Random(Length(fFields))];
    While ((fActualField.Hash = 0) And (fActualField.Name = '')) Or (Not fActualField.Available) Do Begin
      fActualField := fFields[Random(Length(fFields))];
    End;
    // Die Clients auf die Richtigen Karten Umschalten,
    // !! Achtung das ist doppelt implementiert (siehe TServer.HandleUpdateFieldSetup)
    m := TMemoryStream.Create;
    m.WriteAnsiString(fActualField.Name);
    m.Write(fActualField.Hash, sizeof(fActualField.Hash));
    m.write(fSettings.LastWinsToWinMatch, sizeof(fSettings.LastWinsToWinMatch));
    SendChunk(miUpdateFieldSetup, m, 0);
  End;

  fActualField.Initialize(fPLayer, fSettings.Scheme);
  SendChunk(miStartGame, Nil, 0);
  n := GetTickCount64;
  For i := 0 To high(fPLayer) Do Begin // Egal das das alle sind ..
    fPLayer[i].LastSynchronTimeStamp := n;
  End;
  FLastFrameTimestamp := n;
  fLastHeartbeatTimestamp := n;
  fLastClientUpdateTimestamp := n;
  fpausing := false;
  fSyncPause := false;
  foldpausevalue := false;
  fGameState := gsPlaying;
  fFrameLog.Count := 0;
  fFrameLog.AccumulatedSize := 0;
  If assigned(AiNewRound) Then Begin
    // TODO: diese 100% müssen noch einstellbar gemacht werden
    AiNewRound(100);
  End;
  UpdateAllClients(); // Sofort alle Clients informieren, auf dass die auch gleich was sinnvolles sehen ..
  LogLeave;
End;

Procedure TServer.HandleTogglePause;
Var
  m: TMemoryStream;
  i: Integer;
Begin
  log('TServer.HandleTogglePause', llTrace);
  fpausing := Not fpausing;
  ApplyPause(fpausing);
  If fpausing Then Begin
    For i := 0 To high(fPLayer) Do Begin
      fPLayer[i].MoveState := msStill;
      fPLayer[i].Action := aaNone;
    End;
  End;
  m := TMemoryStream.Create;
  m.Write(fpausing, sizeof(fpausing));
  SendChunk(miTogglePause, m, 0);
  LogLeave;
End;

Procedure TServer.HandleShowVictory;
Begin
  // Den Victory Screen auswählen
  // Der Gewinner wurde bereits in EndGameCheck bestimmt.
  SendChunk(miShowVictory, Nil, 0);
  fKickAllPlayer := true;
End;

Procedure TServer.HandlePlayerGetsPowerUp(Var Player: TPlayer;
  PlayerIndex: integer; PowerUp: TPowerUps);
Var
  index, cnt, i: integer;
  tmppos: TVector2;
Begin
  log('TServer.HandlePlayerGetsPowerUp', llTrace);
  // TODO: Die Force, Override, forbidden dinge müssen hier noch berücksichtigt werden..
  //       \-> Das Forbidden wird in TAtomicField.Initialize bereits gemacht.
  If PowerUp <> puNone Then Begin
    HandleStatisticCallback(sPowerupsCollected);
    (*
     * Mit Zählen, was der Spieler so aufsammelt
     *)
    Player.PowerUpCounter[PowerUp] := Player.PowerUpCounter[PowerUp] + 1;
  End;
  Case PowerUp Of
    puNone: Begin
      End;
    puExtraBomb: Begin
        Player.Powers.AvailableBombs := Player.Powers.AvailableBombs + 1;
        Player.Powers.OverAllBombs := Player.Powers.OverAllBombs + 1;
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
      End;
    puLongerFlameLength: Begin
        Player.Powers.FlameLen := min(99, Player.Powers.FlameLen + 1);
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
      End;
    puDisease: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetBadPowerUp);
        Case random(3) Of
          0: Player.Disease := Player.Disease + [dInvertedKeyboard];
          1: Begin
              Player.Disease := Player.Disease + [dNoBombs];
              Player.Action := aaNone;
            End;
          2: Player.Disease := Player.Disease + [dDudBombs];
          3: Begin
              If Not (dSuperSlow In Player.Disease) Then Begin
                Player.BeforeSlowDiseaseSpeed := Player.powers.Speed;
                Player.Disease := Player.Disease + [dSuperSlow];
                Player.powers.Speed := AtomicSlowSpeed / (AtomicSpeedChange * AtomicSpeedChange);
              End;
            End;
        End;
        Player.DiseaseCounter := AtomicDiseaseTime;
      End;
    puCanCick: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
        Player.Powers.CanKickBombs := true;
      End;
    puCanPunch: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
        Player.Powers.CanPunchBombs := true;
      End;
    puExtraSpeed: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
        Player.Powers.Speed := min(Player.Powers.Speed * AtomicSpeedChange, AtomicMaxSpeed);
      End;
    puCanGrab: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
        Player.Powers.CanGrabBombs := true;
        Player.Powers.CanSpooger := false;
      End;
    puCanSpooger: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
        Player.Powers.CanSpooger := true;
        Player.Powers.CanGrabBombs := false;
      End;
    puGoldFlame: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
        Player.Powers.FlameLen := 99;
      End;
    puTrigger: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
        Player.Powers.TriggerBomb := Player.Powers.TriggerBomb + Player.Powers.OverAllBombs;
      End;
    puCanJelly: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetGoodPowerUp);
        Player.Powers.JellyBombs := true;
      End;
    puSuperBadDisease: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetBadPowerUp);
        If random(100) < 10 Then Begin
          cnt := 0;
          For i := 0 To high(fPLayer) Do Begin
            If fPLayer[i].Info.Alive Then inc(cnt);
          End;
          (*
           * Mit Einer Wahrscheinlichkeit von 10 % tauschen wir die Plätze mit einem anderen Spieler ;)
           *)
          If cnt > 1 Then Begin
            Repeat
              index := random(Length(fPLayer));
            Until fPLayer[index].Info.Alive And (PlayerIndex <> index);
            tmppos := fPLayer[index].Info.Position;
            fPLayer[index].Info.Position := fPLayer[PlayerIndex].Info.Position;
            fPLayer[PlayerIndex].Info.Position := tmppos;
          End;
        End
        Else Begin
          Player.Disease := Player.Disease + [dEbola];
          Player.Powers.TriggerBomb := 0;
          Player.DiseaseCounter := AtomicDiseaseTime;
        End;
      End;
    puSlow: Begin
        HandlePlaySoundEffect(PlayerIndex, seGetBadPowerUp);
        Player.Powers.Speed := AtomicSlowSpeed;
      End;
    purandom: Begin
        log('TServer.HandlePlayerGetsPowerUp: Missing implementation for purandom', llError);
      End;
  End;
  LogLeave;
End;

Procedure TServer.HandlePlaySoundEffect(PlayerIndex: integer;
  Effect: TSoundEffect);
Var
  m, m2: TMemoryStream;
  targetUID: Integer;
  i: Integer;
  playerUID: Integer;
Begin
  log('TServer.HandlePlaySoundEffect', llTrace);
  // Check if player is valid
  If (PlayerIndex < 0) Or (PlayerIndex > high(fPLayer)) Then Begin
    log('Invalid PlayerIndex: ' + inttostr(PlayerIndex), llError);
    LogLeave;
    exit;
  End;
  
  playerUID := fPLayer[PlayerIndex].UID;
  
  // Skip inactive slots (UID = NoPlayer = 0)
  If playerUID = NoPlayer Then Begin
    log('Skipping sound for inactive player slot: ' + inttostr(PlayerIndex), llTrace);
    LogLeave;
    exit;
  End;
  
  m := TMemoryStream.Create;
  m.Write(Effect, sizeof(Effect));
  
  // Handle AI players (UID = AIPlayer = -1) - send sounds to all other players
  If playerUID = AIPlayer Then Begin
    // For AI players, send sounds to all connected players (broadcast)
    // Use negative UID to indicate broadcast to all except sender (but sender is AI, so send to all)
    If GetActivePlayerCount() > 0 Then Begin
      // Send to all connected players (UID > 0)
      targetUID := 0; // 0 means broadcast to all connected players
      // Actually, we need to send to each connected player individually
      // because SendChunk with negative UID excludes the sender, but we want to include all
      For i := 0 To high(fPLayer) Do Begin
        If fPLayer[i].UID > 0 Then Begin
          // Create a new stream for each player
          m2 := TMemoryStream.Create;
          m2.Write(Effect, sizeof(Effect));
          SendChunk(miPlaySoundEffekt, m2, fPLayer[i].UID);
        End;
      End;
      m.Free; // Free the original stream
    End
    Else Begin
      log('Skipping AI sound - no connected players', llInfo);
      m.Free;
    End;
  End
  Else If playerUID > 0 Then Begin
    // Normal player (UID > 0)
    If Effect = seOtherPlayerDied Then Begin
      // Send to all players except this one (broadcast with negative UID)
      // Only send if there are at least 2 active players
      If GetActivePlayerCount() > 1 Then Begin
        targetUID := -playerUID;
        SendChunk(miPlaySoundEffekt, m, targetUID);
      End
      Else Begin
        log('Skipping seOtherPlayerDied sound - only one player connected', llInfo);
        m.Free;
      End;
    End
    Else Begin
      // Send to this specific player
      targetUID := playerUID;
      SendChunk(miPlaySoundEffekt, m, targetUID);
    End;
  End
  Else Begin
    // Unknown UID value
    log('Unknown player UID: ' + inttostr(playerUID) + ' for player index: ' + inttostr(PlayerIndex), llWarning);
    m.Free;
  End;
  LogLeave;
End;

Procedure TServer.HandleStatisticCallback(StatSelector: TStatSelector;
  Value: uint64);
Begin
  fStatistik.LastRun[StatSelector] := fStatistik.LastRun[StatSelector] + Value;
End;

Procedure TServer.HandleSwitchToWaitForPlayersToConnect;
Var
  cnt, i: Integer;
Begin
  log('TServer.HandleSwitchToWaitForPlayersToConnect', llTrace);
  fGameState := gsWaitForPlayersToConnect;
  fConnectedClientCount := 0;
  cnt := GetActivePlayerCount();
  For i := 0 To high(fPLayer) Do Begin
    fPLayer[i].UID := NoPlayer;
    fPLayer[i].UserName := '';
    fPLayer[i].Kills := 0;
    fPLayer[i].Score := 0;
  End;
  If assigned(fChunkManager) And (cnt > 0) Then Begin
    SendChunk(miCommandoBackToMainMenu, Nil, 0);
  End;
  // TODO: Klären was noch alles zurück gesetzt werden muss ..
  fSettings.MasterUid := -1;
  fActualField := Nil;
  fSettings.LastWinsToWinMatch := 3; // Default der Clients
  LogLeave;
End;

Procedure TServer.HandleUpdateFieldSetup(Const Stream: TStream; Uid: integer);
Var
  FieldName: String;
  FieldHash: UInt64;
  Wins, i: INteger;
  m: TMemoryStream;
Begin
  log('TServer.HandleUpdateFieldSetup', llTrace);
  m := TMemoryStream.Create;
  m.CopyFrom(Stream, Stream.Size - Stream.Position);
  m.Position := 0;
  FieldName := m.ReadAnsiString;
  FieldHash := 0;
  Wins := 0;
  m.Read(FieldHash, SizeOf(FieldHash));
  m.Read(Wins, SizeOf(Wins));

  For i := 0 To high(fFields) Do Begin
    If (fFields[i].Name = FieldName)
      And (fFields[i].Hash = FieldHash) Then Begin
      fActualField := fFields[i];
      fSettings.LastWinsToWinMatch := Wins;
      // Weiterleiten an alle Clients,
      // !! Achtung, das ist doppelt implementiert (siehe TServer.HandleStartRound;);
      m.Position := 0;
      SendChunk(miUpdateFieldSetup, m, -uid);
      LogLeave;
      exit;
    End;
  End;
  (*
   * Fehler, alle fliegen raus !
   *)
  Log('Error, could not find field: ' + FieldName, llCritical);
  HandleSwitchToWaitForPlayersToConnect();
  LogLeave;
End;

Procedure TServer.HandleReceiveHeartBeat(p: integer; t: int64);
Var
  i: Integer;
Begin
  If fGameState <> gsPlaying Then exit;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = p Then Begin
      fPLayer[i].LastSynchronTimeStamp := t;
    End;
  End;
End;

Procedure TServer.HandleClientKeyEvent(Const Stream: TStream);
Var
  Player: integer;
  Double, State: Boolean;
  Key: TAtomicKey;
Begin
  If fpausing Then exit;
  player := -1;
  State := false;
  Double := false;
  key := akFirstAction;
  stream.Read(Player, sizeof(Player));
  stream.Read(State, sizeof(State));
  stream.Read(key, sizeof(key));
  stream.Read(Double, sizeof(Double));
  fPLayer[Player].IdleTimer := 0; // Der Spieler hat eine Eingabe gemacht -> reset des Idle Timers ;)
  If State Then Begin
    If dInvertedKeyboard In fPLayer[Player].Disease Then Begin
      Case key Of
        akUp: key := akDown;
        akDown: key := akUp;
        akLeft: key := akRight;
        akRight: key := akLeft;
      End;
    End;
    Case Key Of
      akUp: fPLayer[Player].MoveState := msUp;
      akDown: fPLayer[Player].MoveState := msDown;
      akLeft: fPLayer[Player].MoveState := msLeft;
      akRight: fPLayer[Player].MoveState := msRight;
      akFirstAction: Begin
          If (Not (dNoBombs In fPLayer[Player].Disease)) Then Begin
            If Double Then Begin
              fPLayer[Player].Action := aaFirstDouble;
            End
            Else Begin
              fPLayer[Player].Action := aaFirst;
            End;
          End;
        End;
      akSecondAction: Begin
          If (Not (dNoBombs In fPLayer[Player].Disease)) Then Begin
            If Double Then Begin
              fPLayer[Player].Action := aaSecondDouble;
            End
            Else Begin
              fPLayer[Player].Action := aaSecond;
            End;
          End;
        End;
    End;
  End
  Else Begin
    // Nur die "Cursor" werden false gemacht -> immer Stillstand
    fPLayer[Player].MoveState := msStill;
  End;
End;

Procedure TServer.RefreshAllPlayerStats(Uid: integer);
Var
  m: TMemoryStream;
  j, i: Integer;
  k: TKeySet;
Begin
  // Removed log/logleave - was causing stack overflow on frequent calls
  m := TMemoryStream.Create;
  // Einfügen aller Spielerinformationen, dass diese übernommen werden können (z.B. nach Load Game)
  j := length(fPLayer);
  m.write(j, sizeof(j));
  For i := 0 To high(fPLayer) Do Begin
    j := fPLayer[i].UID;
    m.write(j, sizeof(j));
    k := fPLayer[i].Keyboard;
    m.write(k, sizeof(k));
    j := fPLayer[i].Kills;
    m.write(j, sizeof(j));
    j := fPLayer[i].Score;
    m.write(j, sizeof(j));
    m.WriteAnsiString(fPLayer[i].UserName);
  End;
  SendChunk(miRefreshPlayerStats, m, UID);
End;

Procedure TServer.PlayerLeaves(PlayerUid: integer);
Var
  t0, t1, i, cnt, c: integer;
  m: TMemoryStream;
Begin
  log('TServer.PlayerLeaves', llTrace);
  If PlayerUid = 0 Then Begin
    Log('Disconnect user with unknown uid', llCritical);
    LogLeave;
    exit;
  End;
  cnt := 0;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].UID = PlayerUid Then Begin
      log('Lost player : ' + fPLayer[i].UserName, llInfo);
      If fSettings.LostPlayersRevertToAI Then Begin
        fPLayer[i].UID := AIPlayer;
      End
      Else Begin
        fPLayer[i].UID := NoPlayer;
      End;
      fPLayer[i].UserName := '';
      If fPLayer[i].Info.Alive Then Begin // Sollte der Spieler noch am Leben sein -> stirbt er evtl jetzt
        fPLayer[i].Info.Alive := fPLayer[i].UID <> NoPlayer;
      End;
      If Not fPLayer[i].Info.Alive Then Begin
        fPLayer[i].Kills := 0;
        fPLayer[i].Score := 0;
      End;
      // TODO: Alles andere des Spielers auch noch löschen ?
    End;
    // Zählen der Noch aktiven Spieler
    If fPLayer[i].UID > 0 Then inc(cnt);
  End;
  For i := 0 To high(fUidInfos) Do Begin
    If fUidInfos[i].Uid = PlayerUid Then Begin
      For c := i To high(fUidInfos) - 1 Do Begin
        fUidInfos[c] := fUidInfos[c + 1];
      End;
      setlength(fUidInfos, high(fUidInfos));
      break;
    End;
  End;
  // Der "Master" Spieler geht
  If PlayerUid = fSettings.MasterUid Then Begin // TODO: Klären ob es Sinnvoll ist das bei allen Spielern zu machen und nicht nur beim Master ..
    // So lange wir im Spiel definierenden Teil sind
    // Bedeutet der Verlust des Master Spielers den Ausstieg ins
    // Hauptmenü
    If fGameState In [gsWaitForPlayersToConnect, gsPlayerSetup, gsMapSetup] Then Begin
      cnt := 0;
    End;
    // Der Master ist gegangen und wir sind mitten im Spielen, wir müssen einen neuen Bestimmen, sonst können die anderen das Match nicht zu ende Spielen..
    If cnt <> 0 Then Begin
      fSettings.MasterUid := -1;
      For i := 0 To high(fPLayer) Do Begin
        If fPLayer[i].UID > NoPlayer Then Begin
          fSettings.MasterUid := fPLayer[i].UID;
          log('Lost primary player, selected ' + fPLayer[i].UserName + ' as new primary player.', llWarning);
          break;
        End;
      End;
      m := TMemoryStream.Create;
      i := fSettings.MasterUid;
      m.Write(i, SizeOf(i));
      SendChunk(miUpdateMasterID, m, -PlayerUid);
    End;
  End;
  // Wenn im TeamSpiel kann es sein, dass nur noch 1 Team übrig ist und alle Spieler des anderen Rausgeflogen sind ..
  If fSettings.TeamPlay Then Begin
    t0 := 0;
    t1 := 0;
    For i := 0 To high(fPLayer) Do Begin
      If fPLayer[i].UID <> NoPlayer Then Begin
        If fPLayer[i].Team = 0 Then Begin
          inc(t0);
        End
        Else Begin
          inc(t1);
        End;
      End;
    End;
    // Das Spiel so Manipulieren, dass nach dem MatchStatistik Screen der ShowVictory Screen kommt
    For i := 0 To high(fPLayer) Do Begin
      If fPLayer[i].UID <> NoPlayer Then Begin
        If fPLayer[i].Team = 0 Then Begin
          If (t1 = 0) Then Begin
            fSettings.LastWinsToWinMatch := fPLayer[i].Score;
          End;
        End
        Else Begin
          If (t0 = 0) Then Begin
            fSettings.LastWinsToWinMatch := fPLayer[i].Score;
          End;
        End;
      End;
    End;
  End;
  If cnt > 0 Then Begin
    // Wenn es noch andere Spieler gibt Aktualisieren der Spielerliste (z.B. im Start Game Mode wichtig..)
    RefreshAllPlayerStats(0);
    ResetFieldAvailabe();
    For i := 0 To high(fUidInfos) Do Begin
      EvalFieldHashList(fUidInfos[i].Fields, i = high(fUidInfos)); // Nur beim Letzten auch Tatsächlich senden, das Spart Traffic
    End;
  End
  Else Begin
    // Der letzte Geht, zurück in den Edit Modus, macht nur im Timeout=0 Modus sinn, sonst beeden wir eh nach 1000ms
    HandleSwitchToWaitForPlayersToConnect;
  End;
  fConnectedClientCount := fConnectedClientCount - 1;
  If (cnt = 0) And (fAutotimeout <> 0) Then Begin
    log(format('Lost last client, will shut down in %0.3fs', [fAutotimeout / 1000]), llInfo);
  End;
  LogLeave;
End;

Function TServer.GetActivePlayerCount: Integer;
Var
  i: Integer;
Begin
  // !! Hier kein Logger, dass ist sonst zu viel
  result := 0;
  For i := 0 To high(fPLayer) Do Begin
    If (fPLayer[i].UID <> NoPlayer) And (fPLayer[i].UID > 0) Then inc(result);
  End;
End;

Procedure TServer.EvalFieldHashList(Const List: TFieldHashNameList;
  SendToMaster: Boolean);
Var
  m, m2: TMemorystream;
  i, j: Integer;
  found: Boolean;
Begin
  (*
   * Gleicht List mit der Eigenen Fieldliste ab, und streicht aus der Fieldliste
   * alle die raus die nicht in List sind
   *
   * Danach wird diese "kleinste" Schnittmenge an den Master gesendet.
   *)
  log('TServer.EvalMapHashList', lltrace);
  m := TMemoryStream.Create;
  For i := 0 To high(fFields) Do Begin
    found := false;
    If fFields[i].Available Then Begin // karten die Früher schon ausgeschlossen wurden brauchen nicht mehr geprüft werden..
      For j := 0 To high(List) Do Begin
        // Use case-insensitive comparison for field names to handle Windows/Mac differences
        // Hash comparison is still case-sensitive and should match if files are identical
        If (CompareText(fFields[i].Name, list[j].Name) = 0) And
          (fFields[i].Hash = list[j].Hash) Then Begin
          found := true;
          break;
        End
        Else Begin
          // Log mismatch for debugging cross-platform issues
          If (CompareText(fFields[i].Name, list[j].Name) = 0) And
             (fFields[i].Hash <> list[j].Hash) Then Begin
            log(format('Field name match but hash mismatch: Server "%s" (hash: %d) vs Client "%s" (hash: %d)', 
              [fFields[i].Name, fFields[i].Hash, list[j].Name, list[j].Hash]), llWarning);
          End;
        End;
      End;
    End;
    If found Then Begin
      m.WriteAnsiString(fFields[i].Name);
      m.write(fFields[i].hash, sizeof(fFields[i].hash));
    End
    Else Begin
      log(fFields[i].Name + ' will be disabled as it is not present on all game instances.', llWarning);
      fFields[i].Available := false;
    End;
  End;
  If SendToMaster Then Begin
    SendChunk(miAvailableFieldList, m, fSettings.MasterUid);
  End
  Else Begin
    // Send to all connected clients (not just master)
    // This is needed when a new client connects and needs the field list
    // Create a copy of the stream for each client since SendChunk takes ownership
    For i := 0 To high(fUidInfos) Do Begin
      m2 := TMemoryStream.Create;
      m.Position := 0;
      m2.CopyFrom(m, m.Size);
      SendChunk(miAvailableFieldList, m2, fUidInfos[i].Uid);
    End;
    m.free;
  End;
  LogLeave;
End;

Procedure TServer.CheckSynchrons;
Var
  n: int64;
  i: integer;
  b: Boolean;
  s: String;
  m: TMemoryStream;
Begin
  If fpausing Then exit;
  n := GetTickCount64;
  // versenden der HeartBeat Aufforderung an die Clients
  If n - fLastHeartbeatTimestamp >= HeartBeatTime Then Begin
    fLastHeartbeatTimestamp := n;
    m := TMemoryStream.Create;
    m.Write(n, SizeOf(n));
    SendChunk(miHeartBeat, m, 0);
  End;
  // Prüfen der Empfangenen HeartBeat Aufforderungen
  s := '';
  b := false;
  For i := 0 To high(fplayer) Do Begin
    (*
     * Nur die "Lebendigen" und nicht AI Spieler gehen in die Rechnung mit ein.
     *)
    If fplayer[i].Info.Alive And (fplayer[i].UID > 0) And (n - fplayer[i].LastSynchronTimeStamp > SynchonizeTimeOut) Then Begin
      s := fplayer[i].UserName;
      b := true;
      break;
    End;
  End;
  // Mindestens 1 Client ist asynchron, wir leiten eine Zwangspause ein / aus
  If b Then Begin
    If Not fSyncPause Then Begin // Positive Flanke der SyncPausierung
      // Removed logging - was spamming logs on slow clients
      fSyncPause := true;
      ApplyPause(fpausing);
    End;
  End
  Else Begin
    If fSyncPause Then Begin // Negative Flanke der SyncPausierung
      // Removed logging - was spamming logs
      fSyncPause := false;
      ApplyPause(fpausing);
    End;
  End;
End;

Procedure TServer.ApplyPause(Value: Boolean);
Var
  t: int64;
  i: integer;
Begin
  value := value Or fSyncPause;
  //  fmap.Pause(Value);
  //  fSpawnModul.Pause(Value);
  If Value Then Begin // Wir Starten die Pause
    If Not foldpausevalue Then Begin
      fPauseTimestamp := GetTickCount64;
    End;
  End
  Else Begin //Wir Beenden die Pause, nun müssen alle Zeitbasen passend Verschoben werden
    If foldpausevalue Then Begin
      t := GetTickCount64() - fPauseTimestamp; // T = Zeit in ms wie lange die Pause gedauert hat
      FLastFrameTimestamp := FLastFrameTimestamp + t;
      fLastClientUpdateTimestamp := fLastClientUpdateTimestamp + t;
      // fLastActiveTickTimestamp := fLastActiveTickTimestamp + t; -- Egal ob das gemacht wird oder nicht ..
      (*
       * Verschieben aller Zeitbasen für die Bandbreitenlimitierung
       *)
      fLastHeartbeatTimestamp := fLastHeartbeatTimestamp + t;
      For i := 0 To high(fPLayer) Do Begin
        fPLayer[i].LastSynchronTimeStamp := fPLayer[i].LastSynchronTimeStamp + t;
      End;
      // fFrameLog wird nicht gesetzt, läuft paralell weiter auch während einer Pause.
    End;
  End;
  foldpausevalue := value;
End;

Procedure TServer.CreateNewFrame;
Var
  Alive: Array[0..1] Of Integer;
  i, j: Integer;
  aiInfo: TaiInfo;
  AiCommand: TAiCommand;
  OldCounter: UInt16;
Begin
  HandleStatisticCallback(sFramesRendered);
  If fPlayingTimedesc <> -1000 Then Begin
    fPlayingTimedesc := fPlayingTimedesc - FrameRate;
  End;
  fPlayingTimeasc := fPlayingTimeasc + FrameRate;
  (*
   * im Teamplay die Teamfarben anwählen, und der Blocker, dass das nicht andauernd gemacht wird
   *)
  If fSettings.TeamPlay And
    (fPlayingTimeasc > AtomicTeamSwitchTime) And
    (fPlayingTimeasc < AtomicTeamSwitchTime + 2 * FrameRate) Then Begin
    For i := 0 To high(fPLayer) Do Begin
      If fPLayer[i].Team = TeamIndexWhite Then Begin
        fPLayer[i].Info.ColorIndex := WhiteColorIndex;
      End
      Else Begin
        fPLayer[i].Info.ColorIndex := RedColorIndex;
      End;
    End;
  End;
  If assigned(AiHandlePlayer) Then Begin
    aiInfo := fActualField.GetAiInfo(fPLayer, fSettings.TeamPlay);
  End;
  // Die Spieler Bewegen
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].Info.Alive And (Not fPLayer[i].Info.Dying) Then Begin
      fPLayer[i].IdleTimer := fPLayer[i].IdleTimer + FrameRate; // Wir zählen den immer hoch, der läuft erst nach 41 Tagen über...
      If fPLayer[i].UID = AIPlayer Then Begin
        If assigned(AiHandlePlayer) Then Begin
          // Removed debug logging - was causing stack overflow due to frequency
          Try
            AiCommand := AiHandlePlayer(i, aiInfo);
          Except
            On e: exception Do Begin
              log('Ai raised an exception: ' + e.Message, llCritical);
            End;
          End;
          Case AiCommand.MoveState Of
            amNone: fPLayer[i].MoveState := msStill;
            amLeft: fPLayer[i].MoveState := msLeft;
            amRight: fPLayer[i].MoveState := msRight;
            amUp: fPLayer[i].MoveState := msUp;
            amDown: fPLayer[i].MoveState := msDown;
          End;
          Case AiCommand.Action Of
            apNone: fPLayer[i].Action := aaNone;
            apFirst: fPLayer[i].Action := aaFirst;
            apFirstDouble: fPLayer[i].Action := aaFirstDouble;
            apSecond: fPLayer[i].Action := aaSecond;
            apSecondDouble: fPLayer[i].Action := aaSecondDouble;
          End;
        End;
      End;
      If fActualField.HandleMovePlayer(fPLayer, i, fSettings.ConveyorSpeed) Then Begin
        // im Hurry mode kann der Spieler sterben, wenn ein Solid Brick an seiner Koordinate "entsteht"
        fActualField.KillPlayer(fPLayer, i);
      End;

      (*
       * Das "Krank" sein ;)
       *)
      If fPLayer[i].Disease <> [] Then Begin
        (*
         * Das Anstecken
         *)
        For j := 0 To high(fPLayer) Do Begin
          If (i <> j) And
            (LenV2SQR(fPLayer[i].Info.Position - fPLayer[j].Info.Position) <= 0.5 * 0.5) And
            (fPLayer[i].Disease <> fPLayer[j].Disease) Then Begin
            fPLayer[j].Disease := fPLayer[i].Disease;
            fPLayer[j].DiseaseCounter := AtomicDiseaseTime;
          End;
        End;
        (*
         * Das Heilen
         *)
        fPLayer[i].DiseaseCounter := max(0, fPLayer[i].DiseaseCounter - FrameRate);
        If fPLayer[i].DiseaseCounter = 0 Then Begin
          If dSuperSlow In fPLayer[i].Disease Then Begin
            fPLayer[i].Powers.Speed := fPLayer[i].BeforeSlowDiseaseSpeed;
          End;
          fPLayer[i].Disease := [];
        End;
      End;
    End;
  End;
  // Aktions durchführen
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].Info.Alive And (Not fPLayer[i].Info.Dying) Then Begin
      If dEbola In fPLayer[i].Disease Then fPLayer[i].Action := aaFirst; // Wenn der Spieler "Ebola" hat, legt er wann immer möglich eine Bombe
      // TODO: Feature oder Bug, wenn der Spieler Ebola hat und dNoBombs wird er hier verschont ...
      If (fPLayer[i].Action <> aaNone) And (Not (dNoBombs In fPLayer[i].Disease)) Then Begin
        fActualField.HandleActionPlayer(fPLayer[i], i);
        fPLayer[i].Action := aaNone;
      End;
    End;
  End;
  // Bomben Handeln
  // TODO: Magic Numbers -> entfernen klären ...
  fActualField.HandleBombs(fPLayer, (fPlayingTimedesc <= 80000) And (fPlayingTimedesc <> -1000), fSettings.ConveyorSpeed);
  // Kollision mit Flammen
  fActualField.HandlePlayerVsMap(fPLayer, @HandlePlayerGetsPowerUp);

  fActualField.HandleFieldAnims;

  (* Das Sterben der Spieler Einleiten *)
  Alive[0] := 0;
  Alive[1] := 0;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].Info.Alive Then Begin // Hier darf Dieing nicht stehen, da es ja genau darum geht unten die Todeszeit ab zu prüfen !
      (*
       * Den Zähler lassen wir mal immer Zählen und damit nichts schief geht alle 1 min überlaufen, sollte er grad nicht gebraucht werden ...
       * Jede Animation die den Counter Braucht setzt ihn beim Start auf 0 zurück, damit ist dann alles Save ;)
       *)
      OldCounter := fPLayer[i].Info.Counter;
      fPLayer[i].Info.Counter := (fPLayer[i].Info.Counter + FrameRate) Mod 60000;
      // Der Teleporter versetzt den Spieler genau 1 mal und zwar bei der Hälfte der Animationszeit
      If (fPLayer[i].Info.Animation = raTeleport) And
        (OldCounter < AtomicAnimationTimeTeleport Div 2) And
        (fPLayer[i].Info.Counter >= AtomicAnimationTimeTeleport Div 2) Then Begin
        fActualField.TelePortPlayer(fPLayer[i]);
      End;
      If fPLayer[i].Info.Dying Then Begin
        If fPLayer[i].Info.Counter > AtomicDietimeout Then Begin
          fPLayer[i].Info.Alive := false; // Wir sind gestorben und die Animation ist nun auch durch..
          fPLayer[i].Info.Dying := false;
          fActualField.RepopulatePlayersCollectedPowerUps(fPLayer, i);
        End;
      End
      Else Begin
        inc(alive[fPLayer[i].Team]);
      End;
    End
    Else Begin
      fPLayer[i].Info.Dying := false;
    End;
  End;
  If fSettings.TeamPlay Then Begin
    If (Alive[0] < 1) Or (Alive[1] < 1) Then Begin // Abschalten sobald das andere Team Platt gemacht wurde !
      fActualField.DisableAllBombs;
    End;
  End
  Else Begin
    If Alive[0] + Alive[1] <= 1 Then Begin // Abschalten Aller Bomben sobald nur noch einer am Leben ist bzw noch nicht am Sterben ;)
      fActualField.DisableAllBombs;
    End;
  End;
  HurryHandling;
  EndGameCheck();
End;

Procedure TServer.UpdateAllClients;
Var
  m: TMemoryStream;
  i: Integer;
  DiseasedInfo: TAtomicInfo;
  StartTime: QWord;
Begin
  StartTime := GetTickCount64;
  // Gesendet werden immer 3 Datensätze
  m := TMemoryStream.Create;
  // Die Rundenzeit mit übertragen
  i := fPlayingTimedesc Div 1000;
  m.write(i, sizeof(i));
  // 1. Alles was die Spieler angeht
  For i := 0 To high(fplayer) Do Begin
    (*
     * Der Spieler langweilt sich und wird nicht zufällig gerade gegrillt
     *)
    If (fPLayer[i].IdleTimer > AtomicIdleTimeout) And (fPLayer[i].Info.Animation <> raDie) And (fPLayer[i].Info.Animation <> raLockedIn) Then Begin
      fPLayer[i].IdleTimer := 0;
      fPLayer[i].Info.Animation := raZen;
      fPLayer[i].Info.Counter := 0;
      fPLayer[i].Info.Value := random(65536); // Eine Zufällige Zen Animation auswählen ;)
      If fPLayer[i].UID > 0 Then HandlePlaySoundEffect(i, seZen); // Nur "echte" spieler kriegen den Ton
    End;
    (*
     * Wenn der Spieler eine Krankheit hat, dann wechselt seine Farbe alle AtomicDiseaseColorChangeTime
     *)
    If (fPLayer[i].Disease <> []) And ((fPLayer[i].DiseaseCounter Mod AtomicDiseaseColorChangePeriodTime) < AtomicDiseaseColorChangePeriodTime - AtomicDiseaseColorChangePeriodRelaxTime) Then Begin
      DiseasedInfo := fPLayer[i].Info;
      DiseasedInfo.ColorIndex := (fPLayer[i].DiseaseCounter Div AtomicDiseaseColorChangeTime) Mod length(PlayerColors);
      m.Write(DiseasedInfo, sizeof(DiseasedInfo));
    End
    Else Begin
      m.Write(fPLayer[i].Info, sizeof(fPLayer[i].Info));
    End;
    // Der Client Wertet die Flanke der animation aus und hällt diese Selbst so lange wie er sie braucht
    // d.h. Die Animation wird hier direkt wieder Platt gemacht.
    If fPLayer[i].Info.Animation In OneTimeAnimations Then Begin
      fPLayer[i].Info.Animation := raStandStill;
    End;
  End;
  // 2. Alles was das zu Rendernde Feld angeht
  fActualField.AppendGamingData(m);
  
  // Removed performance logging - was causing log spam and potential stack overflow
  SendChunk(miUpdateGameData, m, 0);
End;

Procedure TServer.SendPlayerStatistiks;
Var
  m: TMemoryStream;
  i: Integer;
Begin
  log('TServer.SendPlayerStatistiks', lltrace);
  m := TMemoryStream.Create;
  For i := 0 To high(fPLayer) Do Begin
    m.Write(fPLayer[i].Score, sizeof(fPLayer[i].Score));
    m.Write(fPLayer[i].Kills, sizeof(fPLayer[i].Kills));
  End;
  SendChunk(miUpdatePlayerStatistik, m, 0);
  LogLeave;
End;

Procedure TServer.EndGameCheck;
Var
  (*
  Merken wem wir alles schon einen "Winner" gesendet haben
  -> Das Verhindert, dass wenn 2 Spieler am Selben Rechner sitzen
     diese den "GewinnerSound" doppelt hören.
  *)
  SoundSends: Array Of Integer;
  Procedure SendWinner(uid, playerindex: integer);
  Var
    i: Integer;
  Begin
    For i := 0 To high(SoundSends) Do Begin
      If SoundSends[i] = uid Then exit;
    End;
    setlength(SoundSends, high(SoundSends) + 2);
    SoundSends[high(SoundSends)] := uid;
    HandlePlaySoundEffect(playerindex, seWinner);
  End;

Var
  WinnerTeamIndex, teamwhite, teamred, cnt, cntindex, i: Integer;
  m: TMemoryStream;
  v: TVictor;
Begin
  (*
   * Bedingungen zum "Ende"
   *)
  // 1. Nur noch <=1 Spieler am Leben, oder im Team Modus nur noch 1 Team am Leben
  teamwhite := 0;
  teamred := 0;
  cnt := 0;
  cntindex := -1;
  SoundSends := Nil;
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].Info.Alive Then Begin
      inc(cnt);
      cntindex := i;
      If fPLayer[i].Team = TeamIndexWhite Then Begin
        inc(teamwhite);
      End
      Else Begin
        inc(teamred);
      End;
    End;
  End;
  If fSettings.TeamPlay Then Begin
    // Nur noch 1 Team am Leben
    If (teamwhite = 0) Or (teamred = 0) Then Begin
      // Draw Game Check
      // => Es kann passieren, dass das andere Team gerade am "Sterben" ist aber eben auch tot
      //    Aus diesem Grund muss hier noch mal extra geprüft werden ob die Gewinner Mannschaft
      //    auch wirklich am Leben ist..
      WinnerTeamIndex := IfThen((teamred = 0), TeamIndexWhite, TeamIndexRed);
      cnt := 0;
      For i := 0 To high(fPLayer) Do Begin
        If (fPLayer[i].Team = WinnerTeamIndex) And fPLayer[i].Info.Alive And (Not fPLayer[i].Info.Dying) Then Begin
          inc(cnt);
        End;
      End;
      If cnt <> 0 Then Begin // Wir haben wirklich einen Gewinner ;)
        // Die Scores erhöhen
        If teamred = 0 Then Begin // Team weis hat gewonnen
          v := vWhiteTeam;
          For i := 0 To high(fPLayer) Do Begin
            If fPLayer[i].Team = 0 Then Begin
              SendWinner(fPLayer[i].UID, i);
              inc(fPLayer[i].Score);
            End;
          End;
        End
        Else Begin // Team 2 hat gewonnen
          v := vRedTeam;
          For i := 0 To high(fPLayer) Do Begin
            If fPLayer[i].Team = 1 Then Begin
              SendWinner(fPLayer[i].UID, i);
              inc(fPLayer[i].Score);
            End;
          End;
        End;
        SendPlayerStatistiks(); // Damit alle Clients auch die Richtigen Scores anzeigen diese nun Aktualisieren
        m := TMemoryStream.Create;
        m.Write(v, SizeOf(v));
        SendChunk(miShowMatchStatistik, m, 0);
        fGameState := gsShowHighscore;
      End
      Else Begin
        // Draw Game, aber es laufen noch Todesanimationen,
        // -> Damit der Code unten nicht direkt das DrawGame triggert mus cnt
        //    manipuliert werden ;)
        For i := 0 To high(fPLayer) Do Begin
          If (fPLayer[i].Team = WinnerTeamIndex) And (fPLayer[i].Info.Dying) Then Begin
            cnt := 1; // Egal hauptsache <> 0
          End;
        End;
      End;
    End;
  End
  Else Begin
    // Nur noch 1 Überlebender Spieler
    If cnt = 1 Then Begin
      // Dem Sieger den Punkt geben, aber nur, wenn er nicht auch gerade am sterben ist ..
      // Wenn der Spieler Gestorben ist geht dann cnt von alleine Später auf 0 ;)
      If Not fPLayer[cntindex].Info.Dying Then Begin
        SendWinner(fPLayer[cntindex].UID, cntindex);
        fPLayer[cntindex].Score := fPLayer[cntindex].Score + 1;
        SendPlayerStatistiks(); // Damit alle Clients auch die Richtigen Scores anzeigen diese nun Aktualisieren
        v := TVictor(integer(vCol0) + cntindex);
        m := TMemoryStream.Create;
        m.Write(v, SizeOf(v));
        SendChunk(miShowMatchStatistik, m, 0);
        fGameState := gsShowHighscore;
      End;
    End;
  End;
  // 2. Zeit abgelaufen (wenn Zeitlimit Aktiv)
  If ((fPlayingTimedesc <> -1000) And (fPlayingTimedesc <= 0)) Or (cnt = 0) Then Begin
    (*
     * Nachdem auf Jedenfall oben niemand gewonnen hat ist nun schluss mittels draw
     *)
    SendChunk(miDrawGame, Nil, 0);
    fGameState := gsShowHighscore;
  End;
End;

Function TServer.MatchFinished: Boolean;
Var
  i: Integer;
Begin
  result := false;
  (*
   * Hat wenigstens 1 Spieler die Notwendige Anzahl an Runden gewonnen ?
   * Hier ist es tatsächlich egal ob Teamplay oder nicht, da beim Teamplay
   * ja jeder im Team einen Score bekommt ;)
   *)
  For i := 0 To high(fPLayer) Do Begin
    If fPLayer[i].Score >= fSettings.LastWinsToWinMatch Then Begin
      result := true;
      break;
    End;
  End;
End;

Procedure TServer.LoadAi;
Begin
  log('TServer.LoadAi', lltrace);
  If Not LoadAiLib() Then Begin
    logshow('Could not load ai, ai functions are not available.', llError);
    LogLeave;
    exit;
  End;
  If AiInit() Then Begin
    logshow(format('Ai "%s" loaded successfully!', [AiVersion()]), llInfo);
  End
  Else Begin
    logshow('Failure on Ai load (AiInit returned false).', llError);
    UnLoadAiLib;
  End;
  If fGameState = gsPlaying Then Begin
    // TODO: Diese 100% müssen noch einstellbar gemacht werden !
    AiNewRound(100);
  End;
  LogLeave;
End;

Procedure TServer.HurryHandling;
Begin
  If fPlayingTimedesc = -1000 Then exit; // Nicht im Infinity Mode
  If Not fActualField.BombsEnabled Then exit; // Es gibt wohl keine Spieler mehr auf der Karte -> kein Hurry mehr ;)
  If fPlayingTimedesc = 85000 Then Begin
    SendChunk(miShowHurry, Nil, 0);
  End;
  If fPlayingTimedesc <= 80000 Then Begin
    If fPlayingTimedesc Mod 500 = 0 Then Begin
      fActualField.IncHurry();
      HandlePlaySoundEffect(0, seHurryBrick);
    End;
  End;
End;

Procedure TServer.Execute;
Var
  n: QWord;
Begin
  log('TServer.Execute', lltrace);
  // Loop in einer Endlosschleife, so lange bis 1000ms lang kein Client mehr connected ist, dann raus
  While factive Do Begin
    // Process incoming network chunks from network thread
    fChunkManager.ProcessIncomingChunks();
    fUDP.CallAction();
    If fKickAllPlayer Then Begin
      log('KickAllPlayer', lltrace);
      fKickAllPlayer := false;
      HandleSwitchToWaitForPlayersToConnect();
      LogLeave;
    End;
    If fGameState = gsPlaying Then Begin // Im Spielmodus Frames und Updates der Clients Berechnen
      //  alle 10 s Loggen wie Groß die Spieldaten waren, welche gesandt
      n := GetTickCount64();
      If fFrameLog.TimeStamp + 10000 <= n Then Begin
        fFrameLog.TimeStamp := n;
        If fFrameLog.Count <> 0 Then Begin
          log(Format('Send %d frames with avg size of %.1f Bytes.', [fFrameLog.Count, fFrameLog.AccumulatedSize / fFrameLog.Count]), lldebug);
        End
        Else Begin
          log('Send 0 frames.', lldebug);
        End;
        fFrameLog.Count := 0;
        fFrameLog.AccumulatedSize := 0;
      End;
      CheckSynchrons; //Sind alle Player noch Synchron, wenn nein Warten
      //      If fpausing Then Begin // Zählen der Pausenzeit
      //        n := GetTickCount64;
      //        If n <> pt Then Begin
      //          fOverAllPausingTime := fOverAllPausingTime + n - pt;
      //          pt := n;
      //        End;
      //      End
      //      Else Begin
      //        pt := GetTickCount64;
      //      End;
      If (Not (fpausing Or fSyncPause)) Then Begin
        n := GetTickCount64();
        If FLastFrameTimestamp + FrameRate <= n Then Begin
          // Durch das Aufaddieren der Zeit und nicht direkt setzen auf n könnte hier eine
          // Überpropertionale rechenlast erzeugt werden, besonders, wenn CreateNewFrame, länger dauert als (FrameRate Div Speedup)
{$IFDEF Release}
          FLastFrameTimestamp := FLastFrameTimestamp + FrameRate;
{$ELSE}
          // Sonst scrollt die Zeit ewig weiter, wenn man mim Debugger die Zeit anhällt
          FLastFrameTimestamp := n;
{$ENDIF}
          CreateNewFrame;
        End;
        // Egal, welcher Speedup, das Spiel wird mit konstanter Rate Aktualisiert
        If fLastClientUpdateTimestamp + UpdateRate <= n Then Begin
          fLastClientUpdateTimestamp := n; // fLastClientUpdateTimestamp + UpdateRate; Verhindern von oben beschriebener Situation
          // Only send updates if there are connected clients
          If GetActivePlayerCount() > 0 Then Begin
            UpdateAllClients;
          End;
        End;
      End;
    End;
    (*
     * Ab hier geht es nur noch darum zu erkennen ob die Anwendung beendet werden
     * soll.
     *)
    If KeyPressed Then Begin
      Case ReadKey() Of
        #27: Begin
            Log('Close by user input.', llInfo);
            factive := false;
          End;
        'a', 'A': Begin
            LoadAi;
          End;
        'u', 'U': Begin
            UnLoadAiLib;
            LogShow('Disabled ai', llInfo);
          End;
      End;
    End;
    // Beenden Bedingung erkennen
    If GetActivePlayerCount() <> 0 Then Begin
      fLastActiveTickTimestamp := GetTickCount64();
    End
    Else Begin
      // fAutotimeout ms lang keine Activen Clients, dieser Server kann geschlossen werden
      If (fAutotimeout <> 0) Then Begin
        If (fLastActiveTickTimestamp + fAutotimeout < GetTickCount64()) Then Begin
          factive := false;
        End;
      End;
      {
      // Zwangsabbruch, weil der Server sich selbst geschlossen hat
      If Not fChunkManager.Connected Then Begin
        log('Chunkmanager lost connection.', llfatal);
        factive := false;
      End;
      }
    End;
    If fGameState = gsPlaying Then Begin
{$IFDEF Windows}
      sleep(0);
{$ELSE}
      sleep(1);
{$ENDIF}
    End
    Else Begin
      sleep(1);
    End;
  End;
  LogLeave;
End;

Procedure TServer.LoadStatistiks;
Var
  ini: TIniFile;
  p: String;
Begin
  p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  ini := TIniFile.Create(p + 'stats.txt');
  fStatistik.Total[sMatchesStarted] := ini.ReadInt64('Total', 'MatchesStarted', 0);
  fStatistik.Total[sGamesStarted] := ini.ReadInt64('Total', 'GamesStarted', 0);
  fStatistik.Total[sFramesRendered] := ini.ReadInt64('Total', 'FramesRendered', 0);
  fStatistik.Total[sBombsDropped] := ini.ReadInt64('Total', 'BombsDropped', 0);
  fStatistik.Total[sPowerupsCollected] := ini.ReadInt64('Total', 'PowerupsCollected', 0);
  fStatistik.Total[sPlayerDeaths] := ini.ReadInt64('Total', 'PlayerDeaths', 0);
  fStatistik.Total[sBricksDestroyed] := ini.ReadInt64('Total', 'BricksDestroyed', 0);
  fStatistik.Total[sPowerUpDestroyed] := ini.ReadInt64('Total', 'PowerUpDestroyed', 0);
  fStatistik.Total[sTotalNetworkBytesIn] := ini.ReadInt64('Total', 'TotalNetworkBytesIn', 0);
  fStatistik.Total[sTotalNetworkBytesOut] := ini.ReadInt64('Total', 'TotalNetworkBytesOut', 0);
  fStatistik.Total[sTotalNetworkPacketsIn] := ini.ReadInt64('Total', 'TotalNetworkPacketsIn', 0);
  fStatistik.Total[sTotalNetworkPacketsOut] := ini.ReadInt64('Total', 'TotalNetworkPacketsOut', 0);

  (*
   * Alles Aktuelle = 0 ;)
   *)
  FillChar(fStatistik.LastRun, sizeof(fStatistik.LastRun), 0);
  ini.free;
End;

Procedure TServer.SaveStatistiks;
Var
  ini: TIniFile;
  p: String;
Begin
  p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  ini := TIniFile.Create(p + 'stats.txt');
  Try
    ini.writeInt64('Total', 'MatchesStarted', fStatistik.Total[sMatchesStarted] + fStatistik.LastRun[sMatchesStarted]);
    ini.writeInt64('Total', 'GamesStarted', fStatistik.Total[sGamesStarted] + fStatistik.LastRun[sGamesStarted]);
    ini.writeInt64('Total', 'FramesRendered', fStatistik.Total[sFramesRendered] + fStatistik.LastRun[sFramesRendered]);
    ini.writeInt64('Total', 'BombsDropped', fStatistik.Total[sBombsDropped] + fStatistik.LastRun[sBombsDropped]);
    ini.writeInt64('Total', 'PowerupsCollected', fStatistik.Total[sPowerupsCollected] + fStatistik.LastRun[sPowerupsCollected]);
    ini.writeInt64('Total', 'PlayerDeaths', fStatistik.Total[sPlayerDeaths] + fStatistik.LastRun[sPlayerDeaths]);
    ini.writeInt64('Total', 'BricksDestroyed', fStatistik.Total[sBricksDestroyed] + fStatistik.LastRun[sBricksDestroyed]);
    ini.writeInt64('Total', 'PowerUpDestroyed', fStatistik.Total[sPowerUpDestroyed] + fStatistik.LastRun[sPowerUpDestroyed]);
    ini.writeInt64('Total', 'TotalNetworkBytesIn', fStatistik.Total[sTotalNetworkBytesIn] + fStatistik.LastRun[sTotalNetworkBytesIn]);
    ini.writeInt64('Total', 'TotalNetworkBytesOut', fStatistik.Total[sTotalNetworkBytesOut] + fStatistik.LastRun[sTotalNetworkBytesOut]);
    ini.writeInt64('Total', 'TotalNetworkPacketsIn', fStatistik.Total[sTotalNetworkPacketsIn] + fStatistik.LastRun[sTotalNetworkPacketsIn]);
    ini.writeInt64('Total', 'TotalNetworkPacketsOut', fStatistik.Total[sTotalNetworkPacketsOut] + fStatistik.LastRun[sTotalNetworkPacketsOut]);

    ini.writeInt64('LastRun', 'MatchesStarted', fStatistik.LastRun[sMatchesStarted]);
    ini.writeInt64('LastRun', 'GamesStarted', fStatistik.LastRun[sGamesStarted]);
    ini.writeInt64('LastRun', 'FramesRendered', fStatistik.LastRun[sFramesRendered]);
    ini.writeInt64('LastRun', 'BombsDropped', fStatistik.LastRun[sBombsDropped]);
    ini.writeInt64('LastRun', 'PowerupsCollected', fStatistik.LastRun[sPowerupsCollected]);
    ini.writeInt64('LastRun', 'PlayerDeaths', fStatistik.LastRun[sPlayerDeaths]);
    ini.writeInt64('LastRun', 'BricksDestroyed', fStatistik.LastRun[sBricksDestroyed]);
    ini.writeInt64('LastRun', 'PowerUpDestroyed', fStatistik.LastRun[sPowerUpDestroyed]);
    ini.writeInt64('LastRun', 'TotalNetworkBytesIn', fStatistik.LastRun[sTotalNetworkBytesIn]);
    ini.writeInt64('LastRun', 'TotalNetworkBytesOut', fStatistik.LastRun[sTotalNetworkBytesOut]);
    ini.writeInt64('LastRun', 'TotalNetworkPacketsIn', fStatistik.LastRun[sTotalNetworkPacketsIn]);
    ini.writeInt64('LastRun', 'TotalNetworkPacketsOut', fStatistik.LastRun[sTotalNetworkPacketsOut]);
  Finally
    ini.free;
  End;
End;

End.

