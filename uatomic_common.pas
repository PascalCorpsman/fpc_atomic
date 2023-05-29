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
Unit uatomic_common;

{$MODE ObjFPC}{$H+}

Interface

{$I globaldefines.inc}

Uses
  Classes, SysUtils, ulogger, Graphics, ugraphics, uvectormath;

{$I updater_settings.inc}

Const
  (*
   * Historie :  0.01    = Initialversion (Activate Updater, MainMenu, OptionsDialog)
   * -release P- 0.02    = Neue Schriftart zum Besseren Lesen
   *                       Alles bis zum "StarGame" soweit fertig
   * -release P- 0.03    = Durchverbinden bis in die Karte möglich
   * -release P- 0.04    = Erste Spielbare Version
   * -release P- 0.05    = Bugfix bei mehrfachen ein / Ausloggen der Spieler konnte kein "Master" mehr bestimmt werden
   * -release P- 0.06    = 2 Player on 1 PC
   *                       Bugfix, Bomb "roll" stopped unexpected
   * -release P- 0.07000 = Locked_In Animation
   *                       Anzeige SoundInfo bei "M", "+", "-", Speichern des Sound Volumes über den neustart
   *                       Fix: Memleak when closing during SFX-Play
   *                       Grab Bombs
   *                       Punch Bombs
   *                       More Diseases !
   *                       Arrows for field 3
   *             0.07001 = Fix AV on Return when not first player
   *                       Fix Crash, when First player skip game after showing Field Setup dialog
   *                       Add Settings.Randomstart implemented
   *                       Fix prevent optical Z-fighting of bombs laying on a arrow
   * -release P- 0.07002 = FIX Missing Free on Play Sound
   * -release P- 0.07003 = Fix Brick explosion animation, when server needs sync pausing.
   *                       Enable Conveyor
   *                       FIX Animation in Mainmenu broken if game was left during pause
   *                       FIX not all Zenanimations have been used in release build
   * -release P- 0.07004 = Adjust conveyor speeds to more realistic values
   *                       Key 1-7 in Main Menu (as shortcut)
   *                       Disconnect during Vircoty Screen (this gives the ability to reconnect to a new game, while "other" players still in the "old" game)
   *                       Respawn collected powerups of dead player
   * -releaseGP- 0.07005 = Fix, invalid errormessage during loading on windows plattforms
   *                       Fix, improve bomb triggering, with ability to punch bombs (but not punching)
   * -releaseGP- 0.07006 = Disable musik if user want to exit the game
   *                       First version that is actual capable of doing the automated version update without errors (testen only under Linux)
   * -releaseG - 0.07007 = Bombs thrown over a already burning koordinate will not ignite
   *                       Do not cancel loading the game, when some animations are missing.
   *                       cd_data_extractor switch to to relative paths
   *                       improve error Message if game loading fails
   *             0.07008 = Allow "Back" to exit options
   *                       "j" command in Main Menu (connect to internet game)
   * -releaseGP- 0.08000 = Prüfung Debug / Release version beim Verbinden
   *)
  Version: uint32 = updater_int_Version; // ACHTUNG die Versionsnummer mus hier und in der Zeile darunter angepasst werden
  defCaption = 'FPC Atomic ver. ' + updater_Version // ACHTUNG die Versionsnummer mus hier und in der Zeile darüber angepasst werden
{$IFDEF DebuggMode}
  + ' build: ' + {$I %DATE%} + '  ' + {$I %TIME%}
{$ENDIF}
  ;

  RF_VersionInfo = 'Your version: %s' + LineEnding +
    'Online version: %s' + LineEnding +
    'Release Message: ' + LineEnding + LineEnding + '%s' + LineEnding + LineEnding +
    'If you start the update now, all unsaved data will get lost.' + LineEnding +
    'Would you like to update FPC Atomic now?';

  FieldWidth = 15; // Anzahl der Kacheln Waagrecht
  FieldHeight = 11; // Anzahl der Kacheln Senkrecht

  GameWidth = 640; // Anwendungsbreite in Pixel
  GameHeight = 480; // Anwendungshöhe in Pixel
  FieldxOff = 20; // Das Offset in Pixel zum Anfahren der Linken Oberen Karten Ecke
  FieldyOff = 66; // Das Offset in Pixel zum Anfahren der Linken Oberen Karten Ecke
  FieldBlockWidth = 40; // Breite einer Kachel in Pixel
  FieldBlockHeight = 36; // Höhe einer Kachel in Pixel

  ServerAutoTimeout = 3000; // Zeit in ms bis der Server sich autoamtisch beendet, wenn keine Spieler verbunden sind.
  UDPPingPort = 8005; // Der Port auf welchem Client und Server Lauschen um heraus zu finden ob offene Spiele da sind.
  TCPDefaultPort = 5521; // Taken from: https://github.com/HerbFargus/SDL-Bomber/blob/master/bomber.c
  ChunkManagerHeaderLen = 12; // uChunkmanager.pas    HeaderLen = 12

  FrameRate = 10; // Zeit in ms bis ein neues Frame berechnet wird
  UpdateRate = 40; // Zeit in ms bis die Clients wieder Aktualisiert werden

  SynchonizeTimeOut = 150; // Zeit in ms Kommt mehr als 150ms lang keine Heartbeat Message von allen Clients, dann wird eine Zwangspause eingeleitet.
  HeartBeatTime = 100; // Zeit in ms Muss Sinnigerweise << SynchonizeTimeOut sein.

  AtomicActionDoubleTime = 200; // Zeit in ms die zwischen 2 Tastendrücken liegen muss damit sie als "Doppelte" erkannt werden.
  AtomicDieTimeout = 5000; // Die Zeit in ms die gewartet wird bis das Spiel nach dem Letzten "Sterbenden" Beendet wird.
  AtomicIdleTimeout = 10000; // Zeit ab derer der Atomic anfängt sich zu langweilen muss >> 6050 ms sein, weil allein die Zen Animation schon so lange braucht

  AtomicDefaultSpeed = 0.5; // Die "Grundgeschwindigkeit" in Kacheln Pro Sekunde
  AtomicSpeedChange = 1.1; // Geschwindigkeitsänderung beim aufsammeln eines Rollschuh Items 1.1 = 10% Schneller
  AtomicMaxSpeed = AtomicDefaultSpeed * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange; // Maximale Geschwindigkeit Eines Atomic in Kacheln Pro Sekunde
  AtomicSlowSpeed = AtomicDefaultSpeed / (AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange); // Niedrigst mögliche Geschwindigkeit ("Schnecke") in Kacheln Pro Sekunde

  (*
   * Es gibt Startpunkte die direkt auf den Bändern liegen, da muss der Spieler auf jeden Fall
   * Schneller laufen können, alls die Schnellsten bänder !
   *)
  ConveyorSlowSpeed = AtomicDefaultSpeed / (AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange);
  ConveyorMiddleSpeed = AtomicDefaultSpeed / (AtomicSpeedChange * AtomicSpeedChange);
  ConveyorFastSpeed = AtomicDefaultSpeed / (AtomicSpeedChange);

  AtomicShowSoundInfoTime = 1000; // Zeit in ms wie lange die Soundinfo angezeigt wird.

  (*
   * Diese Zeiten ergeben sich aus TAtomic.LoadAsColor, doch da der Server das nicht macht müssen sie hier als Konstanten Stehen
   *)
  AtomicAnimationTimeKick = 500;
  AtomicAnimationTimePup = 550;
  AtomicAnimationTimePunch = 500;

  AtomicTeamSwitchTime = 2000; // Zeit in ms die es dauert bis der Server die Farben der Spieler auf die Teamfarbe umstellt
  AtomicBombDetonateTime = 2000; // Zeit in ms die es dauert bis eine Normale Bombe explodiert
  AtomicTimeTriggeredBombTimeOut = 15000; // Zeit in ms, Timeout nachdem eine Zeitgesteuerte Bombe wieder zu einer "normalen" wird vor allem Wichtig, wenn der Spieler Stirbt und seine Bombe noch "Rum" liegt
  AtomicBombDudTimeMin = 2500;
  AtomicBombDudTimeMax = 5500;
  AtomicBombBigFlyTime = 500;
  AtomicBombSmallFlyTime = 250;

  AtomicDiseaseTime = 10000; // Zeit in ms wie Lange man Krank ist
  BrickExplodeTime = 900; //  Zeit in ms, Beim Explodieren eines Steines Setzt der Server für diese Zeit den "Puls" zum senden des Exploded Flags an die Clients, danach wird es wieder gelöscht
  FlameTime = 500; // Zeit in ms, wie Lange die Flammen Stehen bleiben..

  (*
   * Error Codes, verwendet beim Einloggen eines Spielers
   *)
  EC_No_Error = 0;
  EC_User_already_exists = 1; // Spielername bereits vergeben
  EC_Game_Full = 2; // Das Spiel ist bereits im Gange, keine weiteren Spieler mehr erlaubt
  EC_Invalid_Versions = 3; // Bezogen auf Vergleich Client Version vs. Server Version
  EC_Too_Much_Player = 4; // Es dürfen Maximal 10 Spieler beitreten !
  EC_Invalid_Mode_Versions = 5; // Bezogen auf Vergleich Client Debug vs. Release

  (*
   * Damit der Tiefentest funktioniert müssen diverse Layer definiert werden
   *
   * Verfügbarer Z-Bereich ]-1..1[
   *
   *)
  atomic_EPSILON = 0.05;

  (*
   * Die Karte Rendert sich wie Folgt :
   * -0.9  = atomic_Map_Layer, Screen Hintergrund Bild
   * -0.85 = Cursor layer für Screens, Solid, Brick
   *
   *  0.0  = Das Layer in dem die Atomics gerendert werden.
   *
   *  0.9  = atomic_dialog_Layer   = Loader Dialog (der nutzt auch 0.95 )
   *  0.95 = FPS-Counter Layer
   *)
  atomic_Map_Layer = -0.9;

  atomic_Bomb_Layer = -0.45;

  atomic_Layer = 0.0;

  atomic_dialog_Layer = 0.9;

  (*
   * siehe: TPlayer.uid
   *)
  NoPlayer = 0;
  AIPlayer = -1;

  TeamIndexWhite = 0;
  TeamIndexRed = 1;

  WhiteColorIndex = 0; // Siehe PlayerColors
  RedColorIndex = 2; // Siehe PlayerColors

  (*
   * Diese Farben sind nur im Range 0..100
   * Wenn sie als TColor genutzt werden sollen, müssen sie noch durch
   *
   * Function AtomicPlayerColorToColor(aColor: TRGB): TColor;
   *
   *)
{$IFDEF Only3Player}
  PlayerColors: Array[0..2] Of TRGB = (
    (R: 100; G: 100; B: 100), // white
    (R: 20; G: 20; B: 20), // black
    (R: 100; G: 0; B: 0) // red
    );
{$ELSE}
  PlayerColors: Array[0..9] Of TRGB = (
    (R: 100; G: 100; B: 100), // white
    (R: 20; G: 20; B: 20), // black
    (R: 100; G: 0; B: 0), // red
    (R: 0; G: 0; B: 100), // blue
    (R: 0; G: 100; B: 0), // green
    (R: 100; G: 100; B: 0), // yellow
    (R: 0; G: 100; B: 100), // cyan
    (R: 100; G: 0; B: 100), // magenta
    (R: 100; G: 50; B: 0), // orange
    (R: 50; G: 0; B: 100) // purple
    );
{$ENDIF}

  (*
   * Zur Unterscheidung, damit auf dem Server immer nur alle Debug oder Release haben..
   * die beiden folgenden Konstanten müssen also unterschiedlich sein !!
   *)
  GameModeRelease = 1;
  GameModeDebug = 2;

Type

  (*
   * Alles würuber der Server so Statistiken führt ;)
   *)
  TStatSelector = (
    sMatchesStarted
    , sGamesStarted
    , sFramesRendered
    , sBombsDropped
    , sPowerupsCollected
    , sPlayerDeaths
    , sBricksDestroyed
    , sPowerUpDestroyed
    , sTotalNetworkBytesIn // UDP-Daten werden Ignoriert
    , sTotalNetworkBytesOut // UDP-Daten werden Ignoriert
    , sTotalNetworkPacketsIn // UDP-Daten werden Ignoriert
    , sTotalNetworkPacketsOut // UDP-Daten werden Ignoriert
    );

  TStatisticCallback = Procedure(StatSelector: TStatSelector; Value: uint64 = 1) Of Object;

  TBombAnimation = (
    baNormal, // Fertig
    baTimeTriggered, // Fertig
    baDud, // Fertig, nicht getestet
    baWobble // Prallt eine Bombe ab, ist sie ab dann im Wobble Mode -> nur bei Jelly Bombem
    );

  TBombMoveDir = (bmNone, bmUp, bmDown, bmLeft, bmRight, bmFly);

  TBombInfo = Record
    ColorIndex: integer; // 0..9 Farbe in der der Client die Bombe Rendern soll
    Position: TVector2; // in Field Coordinaten, können aber auch "Verrückt" sein, wenn die Bombe wild rum fliegt..
    Animation: TBombAnimation; // Die Jeweilige Animation
    AnimationOffset: uint16; // Damit nicht alle "Gleich" aussehen
{$IFDEF Server}
    FlyStart, FlyTarget: TVector2;
    FlyTime: integer; // Zeit In ms Seit derer die Bombe Fliegt
    FlyFinTime: integer; // Zeit in ms wenn der Flug fertig sein soll
    Speed: Single;
    PlayerIndex: Integer; // Wer hat die Bombe den nun gelegt !
    Lifetime: integer; // Zeit in Ms seit wann die Bombe Lebt
    FireLen: integer; // Strahlläng ein Kacheln (>= 1)
    Detonated: Boolean; // Wird für handle Bombs benötigt, damit man weis welche bombem in diesem Run bereits "Explodiert" sind
    Jelly: Boolean;
    MoveDir: TBombMoveDir;
    DudTime: integer; // Zähler für die Jeweilige Bombe wie lange sie "Dud" ist
{$ENDIF}
  End;

  TSoundEffect = (
    seNone // wtf warum sollte der Server wollen dass wir keinen Soundeffekt spielen ?
    , seBombDrop
    , seBombKick
    , seBombStop
    , seBombJelly
    , seBombBounce
    , seBombGrab
    , seBombPunch
    , seBombExplode
    , seAtomicDie
    , seWinner
    , seGetGoodPowerUp
    , seGetBadPowerUp
    , seZen
    , seOtherPlayerDied
    , seHurryBrick
    , seHurry
    );

  TConveyorSpeed = (csSlow, csMiddle, csFast);

  TFieldHashName = Record
    Name: String;
    Hash: UInt64;
  End;

  TFieldHashNameList = Array Of TFieldHashName;

  TBrickData = (bdSolid, bdBrick, bdBlank);

  TPlayerStartPosition = Record
    x, y: integer; // [0..14, 0..10]
    Team: integer; // [0,1]
  End;

  TDisease = (
    dSuperSlow // -- Wird noch nicht wieder zurück genommen ..
    , dInvertedKeyboard // -- Fertig
    , dNoBombs // -- Fertig
    , dDudBombs // -- Fertig
    (*
     * Ab hier Super Bad Disease !
     *)
    // dSwitchBomberman -- Wird nicht gespeichert weil wird nicht mehr zurück genommen !
    , dEbola // die Pupserkrankheit -- Fertig
    );

  TPowerUps = (
    puNone // Kein Powerup
    , puExtraBomb // -- Fertig
    , puLongerFlameLength // -- Fertig
    , puCanCick // -- Fertig
    , puExtraSpeed // -- Fertig
    , puCanSpooger // -- Fertig
    , puCanPunch // -- Fertig
    , puCanGrab // -- Fertig
    , puGoldFlame // -- Fertig
    , puTrigger // -- Fertig
    , puCanJelly // -- Fertig
    // Ab hier Krankheiten
    , puDisease // -- Fertig (siehe auch TDisease)
    , puSuperBadDisease // -- Fertig
    , puSlow // -- Fertig
    // Zufall, ...
    , purandom
    );

  TPowerTexArray = Array[TPowerUps] Of Integer;

  TPowerUp = Record
    BornWith: Integer; // <0 Ignoriert, >=1 "erlaubt" oder der Wert bei der Geburt
    HasOverride: Boolean; // Wenn Override dann wird der Wert von Override_value genommen und kann auch nicht mehr geändert werden
    OverrideValue: integer; // wie Bornwidth nur eben als Override Value
    Forbidden: Boolean; // Wenn True, dann kann es nicht zufällig auftauchen
  End;

  TScheme = Record
    Name: String; // Name des Schemas, wird der irgendwo angezeigt
    BrickDensity: integer; // [0.. 100%]
    BrickData: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of TBrickData;
    PlayerStartPositions: Array[0..length(PlayerColors) - 1] Of TPlayerStartPosition;
    PowerUps: Array[TPowerUps] Of TPowerUp;
  End;

  TFlame = (
    fCross
    , fup
    , fdown
    , fleft
    , fright
    , fend
    );

  TFlames = Set Of TFlame;

  TFieldBrick = Record
    BrickData: TBrickData; // 1. Rendern Solid / Brick
    Exploding: Boolean;
    // Wenn Brickdata = bdBlank
    PowerUp: TPowerUps;
    // Wenn PowerUp = puNone
    Flame: TFlames;
    FlameColor: integer;
    FlamePlayer: integer; // Der PlayerIndex der Flame (zwecks der Kill Anrechnung)

    Counter: uint16; // Der Zähler, der Zählt wie Lange die Aktuelle animation schon läuft (Entweder Flamme oder Explosion)
  End;

  TFieldBricks = Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of TFieldBrick;

  TRenderAnimation = (
    raStandStill // -- Fertig
    , raWalk // -- Fertig
    , raKick // -- Fertig
    , raPunch
    , raPup // -- Fertig
    , raDie // -- Fertig
    , raZen
    , raLockedIn // -- Fertig
    );
Const
  (*
   * Animationen die nur 1 mal an die Clients gesendet werden und danach wieder gelöscht werden
   * Die Clients lassen die Animation Laufen und "überschreiben" so lange diese laufen die Übertragene Animation
   *)
  OneTimeAnimations = [
    raKick
    , raPunch
    , raPup
    , raDie
    , raZen
    , raLockedIn
    ];
Type
  (*
   * Alle Informationen, welche Zyklisch vom Server gesendet werden
   *)
  TAtomicInfo = Record
    Alive: Boolean; // Rendern, Ja / Nein
    Dieing: Boolean; // Wenn True, dann "Stirbt" der Spieler gerade
    Animation: TRenderAnimation; // Welche Animation soll gerade Gerendert werden ?
    Counter: uint16; // Zähler Wie Lange der PLayer etwas "Tut" Dieing = True ist. -> Der kann ggf noch nach TPlayer wandern
    Value: uint16; // Bei raDie, raZen, raLockedIn muss eine bestimmte angewählt werden, hier steht ihr Index
    Position: TVector2; // Position in Weltkoordinaten (muss noch mit FieldBlockWidth, FieldBlockHeight skalliert werden zum Rendern
    Direction: Single; // Blickrichtung im Mathematischen Drehsinn
    ColorIndex: uint8; // 0 .. length(PlayerColors) - 1 -> Zur Anzeige / Auswahl beim Rendering
  End;

  TAtomicKey = (
    akUp, akDown, akLeft, akRight,
    akFirstAction, // Die Primäraktion = vk_Return -> Bombe Legen
    akSecondAction // Die Sekundäraktoin = vk_Back -> Bombe Punchen ...
    );

  TAtomicAction = (aaNone, aaFirst, aaSecond, aaFirstDouble, aaSecondDouble);

  TMoveState = (msStill, msLeft, msRight, msUp, msDown);

  TAtomicPowers = Record
    (*
     * Alles ab hier wird über das Schema Initialisiert
     *)
    Speed: Single; // Aktuelle Laufgeschwindigkeit
    AvailableBombs: integer; // Wie viele Bomben darf der Spieler noch Legen
    OverAllBombs: integer; // Die Summe aller zur Verfügung stehenden Bomben = Summe Gerade Gelegte + AvailableBombs
    FlameLen: integer; // Goldflame setzt das einfach hoch
    CanKickBombs: Boolean;
    CanPunchBombs: Boolean;
    CanGrabBombs: Boolean;
    CanSpooger: Boolean;
    TriggerBomb: Integer; //
    JellyBombs: Boolean;
  End;

  TKeySet = (
    ks0 // Keyboard 0
    , ks1 // Keyboard 1
    );

  TPlayer = Record
    UserName: String;
    Keyboard: TKeySet; // Der Server schleift das zwar durch, braucht es aber eigentlich nicht ...
    UID: integer; // 0 = off, -1 = AI, > 0 = Menschlicher Spieler
    Kills: integer; // Summe aller Kills des Spielers - Selfkills
    Score: Integer; // Summe aller Spiele bei denen der Spieler auf der Sieger Seite war
    Team: Integer; // Wenn Teamplay, dann ist die Teamnummer relevant [0,1] wird aus dem Scheme file geladen
    Info: TAtomicInfo; // Alles notwendige zum Rendern auf dem Client
{$IFDEF Client}
    Edge: Boolean; // True, wenn bezogen auf die Letzte Aktualisierung durch den Server sich die Animation geändert hat.
    KeysPressed: Array[TAtomicKey] Of Boolean;
{$ENDIF}
{$IFDEF Server}
    Flying: Boolean; // is the player flying ? Not physically on the map
    IdleTimer: UInt32; // Der Zähler der sich Merkt wie viele ms lang der Spieler keine "Eingaben" gemacht hat !! ist nicht exakt mehr so grob
    Disease: Set Of TDisease;
    BeforeSlowDiseaseSpeed: Single;
    DiseaseCounter: Integer; // Zähler der Mit Zählt wann die Krankheit wieder vorbei ist.
    LastSynchronTimeStamp: QWord;
    MoveState: TMoveState;
    Action: TAtomicAction;
    Powers: TAtomicPowers;
    PowerUpCounter: Array[TPowerUps] Of Integer; // Zähler, welche Powerups der Spieler wie oft aufgenommen hat, wenn er stirbt werden diese wieder "Verteilt"
{$ENDIF}
  End;

  TPlayerGetsPowerUpEvent = Procedure(Var Player: TPlayer; PlayerIndex: integer; PowerUp: TPowerUps) Of Object;

  TVictor = (vRedTeam, vWhiteTeam, vCol0, vCol1, vCol2, vCol3, vCol4, vCol5, vCol6, vCol7, vCol8, vCol9);

  TPlayers = Array[0..length(PlayerColors) - 1] Of TPlayer;

  TLogLevel = (llTrace, lldebug, llInfo, llWarning, llError, llCritical, llFatal);
  TLogShowHandler = Procedure(Msg: String; WarnLevel: TLogLevel);

  TKeys = Record
    KeyUp: Word;
    KeyDown: Word;
    KeyLeft: Word;
    KeyRight: Word;
    KeyPrimary: Word;
    KeySecondary: Word;
  End;

  TAtomicSettings = Record
    // Werte aus den Optionen
    TeamPlay: Boolean;
    RandomStart: Boolean;
{$IFDEF Client}
    NodeName: String;
{$ENDIF}
    ConveyorSpeed: TConveyorSpeed;
    SchemeFile: String;
    PlayTime: integer; // in [s]
    LostPlayersRevertToAI: Boolean;
{$IFDEF Client}
    PlaySounds: Boolean;
    Keys: Array[TKeySet] Of TKeys;
    ShowFPS: Boolean;
{$ENDIF}
    LastPlayedField: String;
    LastPlayedFieldHash: uint64;
    LastWinsToWinMatch: Integer;

{$IFDEF Client}
    // Werte die zum Spielen benötigt werden
    Port: integer;
    Fullscreen: Boolean;
    VolumeValue: integer; // 0 .. 10000
    // --- Werte die "Intern" sind
    CheckForUpdates: Boolean; // Eigentlich nicht Steuerbar, nur für die Entwicklung gedacht ..
{$ENDIF}
{$IFDEF Server}
    MasterUid: Integer; // -1 = undefiniert, sonst die Uid desjenigen, der in den Menüs das sagen hat. Wird mit miRequestLogin gesetzt
    Scheme: TScheme; // Wird mit miUpdateSettings gesetzt
{$ENDIF}
  End;

Var

  LogShowHandler: TLogShowHandler = Nil; // Debendency Injection auf eine LogShowMsg, wenn es die nicht gibt wird die von ulogger.pas genommen

Procedure LogShow(LogText: String; LogLevel: TLogLevel = llInfo);
Procedure Log(LogText: String; LogLevel: TLogLevel = llInfo);
Procedure LogLeave;
Function GetLoggerLoglevel(): integer;
Procedure InitLogger();
{$IFDEF Windows}
Procedure EnableLogToConsole();
{$ENDIF}
Procedure SetLoggerLogFile(Filename: String);
Procedure SetLogLevel(Level: integer);
Procedure AssertLog(Criteria: Boolean; LogText: String; LogLevel: TLogLevel = llInfo); // Logt nur wenn "Criteria" = true
Function LogLevelToString(LogLevel: TLogLevel): String;

Procedure SchemeToStream(Const Stream: TStream; Const Scheme: TScheme); // Speichert ein Scheme in einen Stream
Function SchemeFromStream(Const Stream: TStream; Out Scheme: TScheme): Boolean;
Function GetDefaultScheme(): TScheme;

Function AtomicPlayerColorToColor(aColor: TRGB): TColor;

{$IFDEF Client}
Function AtomicDefaultKeys(Index: TKeySet): TKeys;
{$ENDIF}

Procedure Nop(); // Nur zum Debuggen ;)
Function BrickShemeToString(Const Scheme: TScheme): String; // Only Debug

Implementation

Uses math
{$IFDEF Client}
  , LCLType
{$ENDIF}
  ;

Procedure Nop(); // Nur zum Debuggen ;)
Begin

End;

Function BrickShemeToString(Const Scheme: TScheme): String; // Only Debug
Var
  j, i: Integer;
  s, t: String;
Begin
  s := '';
  For j := 0 To FieldHeight - 1 Do Begin
    t := '';
    For i := 0 To FieldWidth - 1 Do Begin
      Case Scheme.BrickData[i, j] Of
        bdBlank: t := t + '.';
        bdBrick: t := t + ':';
        bdSolid: t := t + '#';
      End;
    End;
    s := s + LineEnding + t;
  End;
  result := trim(s);
End;

{$IFDEF Client}

Function AtomicDefaultKeys(Index: TKeySet): TKeys;
Begin
  If Index = ks0 Then Begin
    result.KeyUp := VK_UP;
    result.KeyDown := VK_DOWN;
    result.KeyLeft := VK_LEFT;
    result.KeyRight := VK_RIGHT;
    result.KeyPrimary := vk_Return;
    result.KeySecondary := vk_Back;
  End
  Else Begin
    result.KeyUp := VK_W;
    result.KeyDown := VK_S;
    result.KeyLeft := VK_A;
    result.KeyRight := VK_D;
    result.KeyPrimary := VK_CAPITAL;
    result.KeySecondary := VK_TAB;
  End;
End;
{$ENDIF}

Function AtomicPlayerColorToColor(aColor: TRGB): TColor;
Var
  r, g, b: integer;
Begin
  r := min(255, round(aColor.r * 2.55));
  g := min(255, round(aColor.g * 2.55));
  b := min(255, round(aColor.b * 2.55));
  result := Graphics.RGBToColor(r, g, b);
End;

Procedure SchemeToStream(Const Stream: TStream; Const Scheme: TScheme);
Begin
  Stream.WriteAnsiString(Scheme.Name);
  stream.Write(Scheme.BrickDensity, SizeOf(Scheme.BrickDensity));
  stream.Write(Scheme.BrickData, SizeOf(Scheme.BrickData));
  stream.Write(Scheme.PlayerStartPositions, SizeOf(Scheme.PlayerStartPositions));
  stream.Write(Scheme.PowerUps, SizeOf(Scheme.PowerUps));
End;

Function SchemeFromStream(Const Stream: TStream; Out Scheme: TScheme): Boolean;
Begin
  result := false;
  Scheme.Name := stream.ReadAnsiString;
  stream.read(Scheme.BrickDensity, SizeOf(Scheme.BrickDensity));
  stream.read(Scheme.BrickData, SizeOf(Scheme.BrickData));
  stream.read(Scheme.PlayerStartPositions, SizeOf(Scheme.PlayerStartPositions));
  stream.read(Scheme.PowerUps, SizeOf(Scheme.PowerUps));
  result := true;
End;

Function GetDefaultScheme: TScheme;
  Function sp(x, y, t: integer): TPlayerStartPosition;
  Begin
    result.x := x;
    result.y := y;
    result.Team := t;
  End;

  Function empty(): TPowerUp;
  Begin
    result.BornWith := 0;
    result.HasOverride := false;
    result.OverrideValue := 0;
    result.Forbidden := false;
  End;

Var
  j, i: Integer;
Begin
  (*
   * Das ist Im Prinzip das Basic.sch
   *)
  result.Name := 'Just the BASIC SET! (10)';
  result.BrickDensity := 90;
  // alle ::::::::::::::: Zeilen
  For j := 0 To (FieldHeight - 1) Div 2 Do Begin
    For i := 0 To FieldWidth - 1 Do Begin
      result.BrickData[i, j * 2] := bdBrick;
    End;
  End;
  // Alle .#.#.#.#.#.#.#. Zeilen
  For j := 0 To (FieldHeight - 1) Div 2 - 1 Do Begin
    For i := 0 To FieldWidth - 1 Do Begin
      If i Mod 2 = 0 Then Begin
        result.BrickData[i, j * 2 + 1] := bdBrick;
      End
      Else Begin
        result.BrickData[i, j * 2 + 1] := bdSolid;
      End;
    End;
  End;
  result.PlayerStartPositions[0] := sp(0, 0, 0);
  result.PlayerStartPositions[1] := sp(14, 10, 1);
  result.PlayerStartPositions[2] := sp(0, 10, 0);
{$IFNDEF Only3Player}
  result.PlayerStartPositions[3] := sp(14, 0, 1);
  result.PlayerStartPositions[4] := sp(6, 4, 0);
  result.PlayerStartPositions[5] := sp(8, 0, 1);
  result.PlayerStartPositions[6] := sp(12, 4, 0);
  result.PlayerStartPositions[7] := sp(2, 6, 1);
  result.PlayerStartPositions[8] := sp(10, 8, 0);
  result.PlayerStartPositions[9] := sp(6, 10, 1);
{$ENDIF}
  result.PowerUps[puExtraBomb] := empty;
  result.PowerUps[puLongerFlameLength] := empty;
  result.PowerUps[puDisease] := empty;
  result.PowerUps[puCanCick] := empty;
  result.PowerUps[puExtraSpeed] := empty;
  result.PowerUps[puCanPunch] := empty;
  result.PowerUps[puCanGrab] := empty;
  result.PowerUps[puCanSpooger] := empty;
  result.PowerUps[puGoldFlame] := empty;
  result.PowerUps[puTrigger] := empty;
  result.PowerUps[puCanJelly] := empty;
  result.PowerUps[puSuperBadDisease] := empty;
  result.PowerUps[puSlow] := empty;
  result.PowerUps[purandom] := empty;
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
  ulogger.Log(LogText, ConvertLogLevel(LogLevel));
End;

Procedure LogLeave;
Begin
  ulogger.LogLeave;
End;

Function GetLoggerLoglevel: integer;
Begin
  result := Logger.loglevel;
End;

{$IFDEF Server}

Procedure InitLogger();
Begin
  logger.LogToConsole := true;
  logger.LogToFile := false;
  logger.AutoLogStackOnFatal := true;
  logger.LogStackTrace := true;
  logger.SetLogLevel(2);
End;
{$ELSE}

Procedure InitLogger;
Begin
  logger.LogToFile := false;
  logger.SetLogLevel(2);
  logger.AutoLogStackOnFatal := true;
  logger.LogStackTrace := true;
{$IFDEF Linux}
  logger.LogToConsole := true;
{$ENDIF}
End;
{$ENDIF}

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

