Unit uatomic_messages;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes;

Const
  // MessageIdentifiers müssen Numerisch disjunkt sein !!!

  (*
   * Die Nachrichten ID's sind eine 32-Bit Integer zahlen
   *
   * ID's dürfen aber nur im Bereich [1 .. 65535] vergeben werden (also die unteren 2-Byte)
   *
   * die oberen 2 Byte werden für "interne" Zwecke des Clients verwendet und dürfen daher keine ID sein.
   *
   * Der Server Spiegelt die oberen 2-ID-Bytes in jede Anwort die er sendet, damit kann ein Client mehrere Gleiche Anfragen senden und diese
   * dennoch aus einander halten.
   *
   * Broadcast Nachrichten haben immer als Highword = 0
   *
   * => Achtung, das ist noch nicht optimal implementiert, eher so dass es funktioniert, im Zweifel also noch mal Prüfen !!
   *
   *)

  (*
   * Nachrichten, welche Server und Client Gleichermasen empfangen und senden
   *)
  miTogglePause = 1; // Ein Client will Pausieren, der Server Verteilt..
  miHeartBeat = 7;
  miRefreshPlayerStats = 10; // Client -> Server Gib mal info, Server -> Client Antwort
  miSwitchToPlayerSetup = 15; // Der Spieler1 Client will ins Spieler Setup umschalten, der Server schaltet alle Clients in Player Setup um
  miUpdateSettings = 16; // Der Client Sendet seine Settings zum Server, der Server Verteilt das Scheme File an ale Clients
  miSwitchToFieldSetup = 17; // Der Client will in den Karten Editor Screen wechseln
  miStartGame = 18;

  (*
   * Nachrichten vom Client an den Server
   *)
  miRequestLogin = 101; // Der CLient sendet sein Benutzername und Version und will mit Einsteigen.
  miRequestSettingsResult = 134; // Der Client sendet dem Server seine Settings.
  miChangePlayerKey = 135;
  miUpdateFieldSetup = 137;
  miClientKeyEvent = 138;

  (*
   * Nachrichten vom Server an den Client
   *)
  miRequestLoginResult = 200; // Die Antwort des Servers auf miRequestLogin
  miSplashHint = 205; // Der Server will beim Client eine Splashnachricht anzeigen
  miCommandoBackToMainMenu = 226; // Der Server schmeist alle Raus, warum auch immer
  miAvailableFieldList = 227; // Der Server teilt einem Client mit, welche Karten er kennt, alle anderen werden Deaktiviert.
  miUpdateGameData = 228; // Der Server Teilt die Aktuellen Daten des letzten Frames mit
  miDrawGame = 229;
  miShowMatchStatistik = 230;
  miShowVictory = 231;
  miUpdatePlayerStatistik = 232;
  miPlaySoundEffekt = 233;
  miUpdateMasterID = 234; // Der Master Spieler hat sich geändert (nur Relevant, wenn während eines Matches der Master Raus fliegt)
  miShowHurry = 235;

Function MessageIdentifierToString(value: integer): String;
Inline;

Implementation

Uses sysutils;

Function MessageIdentifierToString(value: integer): String;
Begin
  value := value And $FFFF; // Abschneiden der oberen beiden Byte
  result := inttostr(value);
  Case value Of
    miTogglePause: result := 'TogglePause';
    miHeartBeat: result := 'HeartBeat';
    miRefreshPlayerStats: result := 'RefreshPlayerStats';
    miSwitchToPlayerSetup: result := 'SwitchToPlayerSetup';
    miUpdateSettings: result := 'UpdateSettings';
    miSwitchToFieldSetup: result := 'SwitchToFieldSetup';
    miStartGame: result := 'StartGame';

    miRequestLogin: result := 'RequestLogin';
    miRequestSettingsResult: result := 'RequestSettingsResult';
    miChangePlayerKey: result := 'ChangePlayerKey';
    miUpdateFieldSetup: result := 'UpdateFieldSetup';
    miClientKeyEvent: result := 'ClientKeyEvent';

    miRequestLoginResult: result := 'RequestLoginResult';
    miSplashHint: result := 'SplashHint';
    miCommandoBackToMainMenu: result := 'CommandoBackToMainMenu';
    miAvailableFieldList: result := 'AvailableFieldList';
    miUpdateGameData: result := 'UpdateGameData';
    miShowMatchStatistik: result := 'ShowMatchStatistik';
    miDrawGame: result := 'DrawGame';
    miShowVictory: result := 'ShowVictory';
    miUpdatePlayerStatistik: result := 'UpdatePlayerStatistik';
    miPlaySoundEffekt: result := 'PlaySoundEffekt';
    miUpdateMasterID: result := 'UpdateMasterID';
    miShowHurry: result := 'ShowHurry';
  End;
End;

End.

