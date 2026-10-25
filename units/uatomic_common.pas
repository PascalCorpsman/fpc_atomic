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

{$I ../client/globaldefines.inc}

Uses
  Classes, SysUtils, Graphics, ugraphics, uvectormath
{$IFDEF Client}
  , uopengl_graphikengine
{$ENDIF}
  ;

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
   *             0.08001 = Fix, crash when changing username
   *                       das "herunterlaufen" von Laufbändern geht nun "besser"
   * -releaseGP- 0.09000 = Fix, render glitch on brick destroy (only on slow computers and high transmit delays) -> TFieldBrick geändert, deswegen 0.08 auf 0.09
   *                       Fix, crash of server if player is running into a exploding brick
   * -releaseGP- 0.09001 = Anpassen Geschwindigkeiten Conveyor und Schildkröte
   *                       Fix, im Teamplay wurden die Bomben nicht abgeschaltet, wenn das erste Team keine Spieler mehr hat
   *                       Fix, extract Soundhandling into own class to hopefully fix crashes on Linux systems
   * -releaseGP- 0.10000 = ADD: Löcher für Field 04
   *                       ADD: Rollierendes MainMenu (Feature request by community)
   *                       Fix, Laufbänder schieben Atomic auf Powerups
   *                       Fix, Locked in animation got crazy on some machines
   *             0.10001 = ADD: Visualize if a player has a disease
   *                       Fix, memleak on Windows systems
   *                       ADD: Sound for Hole Transfer
   * -releaseGP- 0.11000 = ADD: Trampolines
   * -releaseG - 0.11001 = FIX: Server crash when loading Field09
   * -releaseG - 0.11002 = FIX: disable Kick, Throw onto trampolins
   *                       FIX: do not spooge over hohles
   *                       FIX: Settings where not instantly stored when leaving options dialog
   *                       ADD: start with ai
   *                       FIX: Disease animation
   * -releaseG - 0.11003 = ADD: First Ai version that beats humans
   * -releaseG - 0.11004 = ADD: Update AI-Interface to 0.02 -> for better C support
   * -releaseG - 0.11005 = FIX: Do not respawn PowerUps on Holes
   * -releaseGP- 0.11006 = FIX: If a bomb triggers a bomb from a other player then the second bombs owner is not allowed to get the kill, instead the firsts bombs owner get the kill!
   *                       FIX: Player is imortable during teleporting
   *                       FIX: Schemes that do not define a "team" for player pos, crashed during loading..
   *                       FIX: try to improove Update process
   * -releaseG - 0.11007 = no changes only Ai-Update
   * -releaseG - 0.11008 = ADD: Release Launcher
   * -releaseG - 0.11009 = ADD: Speedup startup of client by more than 30s
   *             0.11010 = ADD: show Nodenames in "TPlayerSetupMenu" Menu (Improve Orig game!)
   *             0.12000 = ADD: Show scheme informations during connections -> need change in ProtocollVersion
   *                       FIX: sheme -> scheme
   * -releaseG - 0.12001 = FIX: #4 Solid Brick not set in timeout mode
   * -releaseG - 0.12002 = ADD: Major change in graphic engine
   * -releaseG - 0.12003 = ADD: Support für Gamepads / Joysticks via SDL2
   * -releaseG - 0.12004 = FIX: Hurry, Playerdead and Map Hole tex have been loaded without transparancy
   *                       ADD: Show / Hide Cursor, when in / not in Main Menu
   * -releaseG - 0.12005 = FIX: wrote Patch for DrawGame glitch
   *                       FIX: Crash when SDL-Keyboard slot is used as AI
   *                       ADD: Support for multiple Joysticks with same name
   * -releaseG - 0.12006 = FIX: add missing SDL_PumpEvents
   *                       ADD: Feature Request #8 load remote IP / Port Settings in "J" Dialog
   *                       FIX: "Slow" bug under Windows #7
   * -releaseG - 0.12007 = FIX: made Windows version more robust (start game did not really work ...)
   * -releaseG - 0.12008 = FIX: Drawgame in Teamsplay -> no finish detected
   * -releaseG - 0.12009 = FIX: if player has no bomb disease and places a bomb (which does not pop up), then it pops up exactly at that moment when the disease is over
   * -releaseG - 0.12010 = ADD: Feature Request Proportional Rendering
   * -releaseG - 0.12011 = FIX: Crash during Key Up
   *                       FIX: Gui glitch during select field screen
   *                       ADD: alternative + and - keys for sound control
   *                       ADD: Player Statistiks (y)
   *                       ADD: GameController Support
   *             0.12012 = ADD: Check for Nodenames on server and client
   *                       FIX: Windows remote join dialog did not close correctly
   *                       FIX: powerup destroyed was wrong calculated
   *                       ADD: implement missing Brick spawning for haunted house field.
   *             0.13001 = ADD: Disable hohles in Hurry mode, see https://bomberman.fandom.com/wiki/The_Coal_Mine
   *                       ADD: Disable trampolins in Hurry mode, see https://bomberman.fandom.com/wiki/Deep_Forest_Green
   * -releaseG - 0.13002 = ADD: Switch to OpenGL Core Profile rendering (use shader instead of legacy calls)
   * -releaseG - 0.13003 = ADD: More relialible kill app on OpenGL error
   *                       ADD: made time edit menu rolling from 90s to 600s + infinity
   *                       ADD: Save statistics after each round
   *                       ADD: give extra trigger bomb if availibility already exists and a new bomb is taken
   *                       ADD: Spawn bricks on haunted hall only if there is no player close
   *                       ADD: GTK3 now default on Linux
   *                       ADD: Improved SDL2 errormessage
   *             0.13004 = ADD: speedup startup
   *                       FIX: connect on some linux devices stalled
   *                       FIX: trampolin placing was wrong
   *                       ADD: optional mods via atomic_server.ini (default off), first mod: thrown bombs landing on a player stun him
   *                       ADD: invulnerability of players (base for further mods), ends when hurry starts
   *                       ADD: optional positive disease, makes the player invulnerable for some time (not during hurry)
   *                       ADD: optional hit points per player name (handicap) -> need change in ProtocollVersion
   *                       ADD: mod test mode with keys F3..F11 (without F10) for testing the mods (only master, only if enabled in atomic_server.ini)
   *                       ADD: optional ghosts, dead players walk around the border and can throw bombs to get back into the game
   *                       ADD: ghosts: max count per round and optional max time until they vanish (flicker faster before)
   *                       ADD: mod settings code shared by server and launcher (defaults, limits, load / save)
   *                       ADD: launcher: new layout (live name check, game data / version status) and mods dialog for atomic_server.ini
   *                       ADD: optional glove at start, until the first step of a round every player can throw a bomb like with the glove
   *                       ADD: optional disease "walk through bricks", the player is drawn see-through
   *                       ADD: optional open borders, players walk over the border and come out on the opposite side (after a set time, shown by arrows on the frame)
   *                       ADD: optional teleporters in the middle of the borders, they bring the player to the opposite border
   *                       ADD: optional boxing glove, with the punch item the second action stuns an opponent in front of the player
   *                       ADD: optional disease "white screen", the screen of the player turns white for some time
   *                       ADD: optional white bomb item, the next bomb explodes without flames and turns the screens of all opponents white
   *                       ADD: optional flamethrower item, hold the second action to fire, opponents in the beam die after some time
   *                       FIX: disease "super slow" was never chosen
   *                       FIX: respawned powerups could be placed below a bomb
   *)

  ProtocollVersion: uint32 = 14; // ACHTUNG die Versionsnummer mus hier und in der Zeile darunter angepasst werden
  Version = '0.13004';
  defCaption = 'FPC Atomic ver. ' + Version // ACHTUNG die Versionsnummer mus hier und in der Zeile darüber angepasst werden
{$IFDEF DebuggMode}
  + ' build: ' + {$I %DATE%} + '  ' + {$I %TIME%}
{$ENDIF}
  ;

  FieldWidth = 15; // Anzahl der Kacheln Waagrecht
  FieldHeight = 11; // Anzahl der Kacheln Senkrecht

  GameWidth = 640; // Anwendungsbreite in Pixel
  GameHeight = 480; // Anwendungshöhe in Pixel
  FieldxOff = 20; // Das Offset in Pixel zum Anfahren der Linken Oberen Karten Ecke
  FieldyOff = 66; // Das Offset in Pixel zum Anfahren der Linken Oberen Karten Ecke
  FieldBlockWidth = 40; // Breite einer Kachel in Pixel
  FieldBlockHeight = 36; // Höhe einer Kachel in Pixel

  FieldTrampCount = 8; // Anzahl an Trampolinen, welche auf einer Karte generiert werden, wenn diese das fHastrampolins flag gesetzt hat ( Laut: https://www.youtube.com/watch?v=fO9HhzhEloE bei 5:47 sind das 8 )

  FieldBrickMinSpawnTime = 1000; // Wenn fHasSpawningBricks gesetzt, mindest Zeit in ms bis ein neuer Brick auf einem Leeren Feld erstellt wird
  FieldBrickMaxSpawnTime = 4000; // Wenn fHasSpawningBricks gesetzt, maximale Zeit in ms bis ein neuer Brick auf einem Leeren Feld erstellt wird  (Laut: https://github.com/HerbFargus/Atomic-Bomberman/blob/master/data/config/valuelist.res )

  ServerAutoTimeout = 3000; // Zeit in ms bis der Server sich autoamtisch beendet, wenn keine Spieler verbunden sind.
  UDPPingPort = 8005; // Der Port auf welchem Client und Server Lauschen um heraus zu finden ob offene Spiele da sind.
  TCPDefaultPort = 5521; // Taken from: https://github.com/HerbFargus/SDL-Bomber/blob/master/bomber.c
  ChunkManagerHeaderLen = 12; // uChunkmanager.pas    HeaderLen = 12

  FrameRate = 10; // Zeit in ms bis ein neues Frame berechnet wird
  UpdateRate = 20; // Zeit in ms bis die Clients wieder Aktualisiert werden (50 FPS for smoother gameplay)

  SynchonizeTimeOut = 800; // Zeit in ms Kommt mehr als 800ms lang keine Heartbeat Message von allen Clients, dann wird eine Zwangspause eingeleitet. (Increased for older machines) (increased for online play over internet)
  HeartBeatTime = 100; // Zeit in ms Muss Sinnigerweise << SynchonizeTimeOut sein.

  AtomicActionDoubleTime = 200; // Zeit in ms die zwischen 2 Tastendrücken liegen muss damit sie als "Doppelte" erkannt werden.
  AtomicDieTimeout = 5000; // Die Zeit in ms die gewartet wird bis das Spiel nach dem Letzten "Sterbenden" Beendet wird.
  AtomicIdleTimeout = 10000; // Zeit ab derer der Atomic anfängt sich zu langweilen muss >> 6050 ms sein, weil allein die Zen Animation schon so lange braucht
  AtomicTrampFlyTime = 1500; // Zeit in ms die es dauert bis ein Spieler der durch ein Trampolin rumfliegt wieder gelandet ist und weiter Spielen kann

  AtomicDefaultSpeed: single = 0.5; // Die "Grundgeschwindigkeit" in Kacheln Pro Sekunde
  AtomicSpeedChange = 1.1; // Geschwindigkeitsänderung beim aufsammeln eines Rollschuh Items 1.1 = 10% Schneller

  ConnectionRetryInterval = 2000; // Retry every 2 seconds, when connecting to a localhost game

Var
  AtomicMaxSpeed: single; // Maximale Geschwindigkeit eines Atomic in Kacheln Pro Sekunde
  AtomicSlowSpeed: single; // Niedrigst mögliche Geschwindigkeit ("Schnecke") in Kacheln Pro Sekunde
  (*
   * Es gibt Startpunkte die direkt auf den Bändern liegen, da muss der Spieler auf jeden Fall
   * Schneller laufen können, alls die Schnellsten bänder !
   *)
  ConveyorSlowSpeed: Single;
  ConveyorMiddleSpeed: Single;
  ConveyorFastSpeed: Single;

Const
  AtomicShowSoundInfoTime = 1000; // Zeit in ms wie lange die Soundinfo angezeigt wird.

  (*
   * Diese Zeiten ergeben sich aus TAtomic.LoadAsColor, doch da der Server das nicht macht müssen sie hier als Konstanten Stehen
   *)
  AtomicAnimationTimeKick = 500;
  AtomicAnimationTimePup = 550;
  AtomicAnimationTimePunch = 500;
  AtomicAnimationTimeTeleport = 1000; // 500ms auf dem alten Loch, dann Teleport dann 500ms auf dem neuen Loch

  AtomicTeamSwitchTime = 2000; // Zeit in ms die es dauert bis der Server die Farben der Spieler auf die Teamfarbe umstellt
  AtomicBombDetonateTime = 2000; // Zeit in ms die es dauert bis eine Normale Bombe explodiert
  AtomicTimeTriggeredBombTimeOut = 15000; // Zeit in ms, Timeout nachdem eine Zeitgesteuerte Bombe wieder zu einer "normalen" wird vor allem Wichtig, wenn der Spieler Stirbt und seine Bombe noch "Rum" liegt
  AtomicBombDudTimeMin = 2500;
  AtomicBombDudTimeMax = 5500;
  AtomicBombBigFlyTime = 500;
  AtomicBombSmallFlyTime = 250;

  AtomicDiseaseTime = 10000; // Zeit in ms wie Lange man Krank ist
  AtomicDiseaseColorChangeTime = 100; // Zeit in ms, nach derer ein Kranker Atomic seine Farbe wechselt
  (*
   * Die folgenden Beiden bilden ein Paar
   * Es wird alle AtomicDiseaseColorChangePeriodTime für AtomicDiseaseColorChangePeriodRelaxTime das Blinken "unterbrochen" und die Korrekte Farbe angezeigt.
   *)
  AtomicDiseaseColorChangePeriodTime = 1500; //
  AtomicDiseaseColorChangePeriodRelaxTime = 500; // Zeit in derer der Atomic trotz Krankheit nicht Blinkt bezogen auf AtomicDiseaseColorChangePeriodTime

  (*
   * Damit man Stun und Unverwundbarkeit unterscheiden kann, blinken beide in unterschiedlicher Farbe und Geschwindigkeit
   *)
  AtomicStunColorChangeTime = 50; // Zeit in ms, nach derer ein gestunnter Atomic zwischen seiner Farbe und schwarz (bzw. rot) wechselt
  AtomicInvulnColorChangeTime = 150; // Zeit in ms, nach derer ein unverwundbarer Atomic zwischen seiner Farbe und weiß (bzw. rot) wechselt

  (*
   * Obere Grenzen der Mod Einstellungen aus atomic_server.ini, die untere Grenze ist 0 (Ausnahmen stehen dabei).
   * Der Server zieht ungültige Werte auf diese Grenzen, der Mods Dialog im Launcher nutzt sie für seine Eingabefelder.
   *)
  ModMaxStunMs = 10000; // StunMinMs, StunMaxMs
  ModMaxPositiveDiseaseChance = 100; // in %
  ModMaxPositiveMs = 120000; // PositiveMinMs, PositiveMaxMs
  ModMaxBrickWalkChance = 100; // in %
  ModMaxBrickWalkMs = 120000; // BrickWalkMinMs, BrickWalkMaxMs
  ModMaxBrickWalkTransparency = 100; // in %, 100 = unsichtbar
  ModMaxGhostReviveInvulnMs = 60000;
  ModMaxGhostFlameLen = 99; // untere Grenze 1
  ModMaxGhostFlickerMs = 5000;
  ModMaxGhostCount = 99; // GhostMaxCount
  ModMaxGhostTimeMs = 600000; // GhostMaxTimeMs
  ModMaxInvulnTimeMs = 60000; // untere Grenze FlameTime, sonst trifft die selbe Flamme den Spieler im nächsten Frame gleich noch mal
  ModMaxHitPoints = 255; // DefaultHitPoints und [Handicap], untere Grenze 1 (TAtomicInfo.HitPoints ist ein uint8)
  ModMaxRoundTimeSec = 3600;
  ModMaxStartInvulnMs = 600000;
  ModMaxTeleportersActiveSec = 600;
  ModMaxOpenBordersActiveSec = 600;
  ModMaxPunchStunCooldownMs = 10000;
  ModMaxFlashChance = 100; // in %
  ModMaxFlashIntensity = 100; // in %, 100 = komplett weiß
  ModMaxFlashMs = 10000; // Dauer und Übergang zurück
  ModMaxFlameThrowerChance = 100; // in %
  ModMaxFlameThrowerFuelMs = 60000;
  ModMaxFlameThrowerRange = FieldWidth - 1; // untere Grenze 1
  ModMaxFlameThrowerKillMs = 10000;

  (*
   * Mod Geister: Die Geister laufen auf einem gedachten Rahmen rund um das Spielfeld (y = -1, x = FieldWidth, y = FieldHeight, x = -1),
   * im Uhrzeigersinn beginnend links oben. Jede Rahmenposition wirft damit eindeutig in genau eine Zeile oder Spalte.
   *)
  GhostRingLength = 2 * FieldWidth + 2 * FieldHeight; // Anzahl der Positionen auf dem Rahmen (52)
  GhostSpeed = 5; // Geschwindigkeit eines Geistes in Randfeldern pro Sekunde
  GhostWarnTime = 5000; // Zeit in ms, so lange vor dem Verschwinden (GhostMaxTimeMs) flackert ein Geist doppelt so schnell
  GhostThrowDistance = 3; // So viele Felder senkrecht vom Rand ins Feld wirft ein Geist seine Bombe
  GhostScale = 0.5; // Geister werden im Client verkleinert auf dem Rahmen außerhalb des Spielfeldes gerendert
  OpenBordersWarnTime = 3000; // Zeit in ms, so lange vor dem Öffnen blinken die Pfeile der offenen Grenzen

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
  EC_Invalid_Username = 6; // Der User loggt sich mit einem nicht erlaubten usernamen ein

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
  BlackColorIndex = 1; // Siehe PlayerColors
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
    (B: 100; G: 100; R: 100), // white
    (B: 20; G: 20; R: 20), // black
    (B: 0; G: 0; R: 100) // red
    );
{$ELSE}
  PlayerColors: Array[0..9] Of TRGB = (
    (B: 100; G: 100; R: 100), // white
    (B: 20; G: 20; R: 20), // black
    (B: 0; G: 0; R: 100), // red
    (B: 100; G: 0; R: 0), // blue
    (B: 0; G: 100; R: 0), // green
    (B: 0; G: 100; R: 100), // yellow
    (B: 100; G: 100; R: 0), // cyan
    (B: 100; G: 0; R: 100), // magenta
    (B: 0; G: 50; R: 100), // orange
    (B: 100; G: 0; R: 50) // purple
    );
{$ENDIF}

  (*
   * Zur Unterscheidung, damit auf dem Server immer nur alle Debug oder Release haben..
   * die beiden folgenden Konstanten müssen also unterschiedlich sein !!
   *)
  GameModeRelease = 1;
  GameModeDebug = 2;

  Achsistrigger = 32767 Div 4; // Wert um den sich eine "Move" Achse auf dem Joystick von der Mittelstellung weg bewegen muss, damit ein "Hit" erkannt wird

Type

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
    WhiteBomb: Boolean; // Mod weiße Bombe: explodiert ohne Flammen und flasht alle Gegner (zählt nicht zu den Bomben des Spielers), der Client zeichnet sie grau
{$IFDEF Server}
    FlyStart, FlyTarget: TVector2;
    FlyTime: integer; // Zeit In ms Seit derer die Bombe Fliegt
    FlyFinTime: integer; // Zeit in ms wenn der Flug fertig sein soll
    Speed: Single;
    PlayerIndex: Integer; // Wer hat die Bombe den nun gelegt !
    TriggerPlayerindex: integer; // Wessen Bombe hat die initiale Zündung ausgelöst -> Wer bekommt den Kill ?
    Lifetime: integer; // Zeit in Ms seit wann die Bombe Lebt
    FireLen: integer; // Strahlläng ein Kacheln (>= 1)
    Detonated: Boolean; // Wird für handle Bombs benötigt, damit man weis welche bombem in diesem Run bereits "Explodiert" sind
    Jelly: Boolean;
    MoveDir: TBombMoveDir;
    DudTime: integer; // Zähler für die Jeweilige Bombe wie lange sie "Dud" ist
    StunOnLand: Boolean; // True, wenn die Bombe geworfen (nicht gepuncht) wurde, landet sie auf einem Spieler, wird dieser gestunnt (nur wenn Mod aktiv)
    GhostBomb: Boolean; // True, die Bombe wurde von einem Geist geworfen (der Geist bekommt keine Bombe zurück, darf aber nach der Explosion wieder werfen)
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
    , seWrapHohle
    , seTrampoline
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
{$IFDEF Client}
  TPowerTexArray = Array[TPowerUps] Of TGraphikItem;
{$ENDIF}

  TPowerUp = Record
    BornWith: Integer; // <0 Ignoriert, >=1 "erlaubt" oder der Wert bei der Geburt
    HasOverride: Boolean; // Wenn Override dann wird der Wert von Override_value genommen und kann auch nicht mehr geändert werden
    OverrideValue: integer; // wie Bornwidth nur eben als Override Value
    Forbidden: Boolean; // Wenn True, dann kann es nicht zufällig auftauchen
  End;

  TScheme = Record
    Filename: String; // Der Dateiname aus dem das Schema geladen wurde
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
    Exploding: Boolean; // Intern für alles mögliche
    ExplodingRenderFlag: Boolean; // Zum Triggern der "Exploding" animation
    // Wenn Brickdata = bdBlank
    PowerUp: TPowerUps;
    // Wenn PowerUp = puNone
    Flame: TFlames;
    FlameColor: integer;
    FlamePlayer: integer; // Der PlayerIndex der Flame (zwecks der Kill Anrechnung)
    Tramp: Boolean; // Wenn True, dann ist auf diesem Feld ein Trampolin
    TrampRunning: Boolean; // Wenn True, dann "wackelt" die Tramp Animation
    TrampOffset: Byte; // Das Tramp Running Offset, sonst sehen die alle Gleich aus..
    Counter: uint16; // Der Zähler, der Zählt wie Lange die Aktuelle animation schon läuft (Entweder Flamme oder Explosion oder Trampolin)
    WhiteBomb: Boolean; // Mod weiße Bombe: True, das PowerUp (immer puExtraBomb) ist eine weiße Bombe, der Client zeichnet es weiß
    FlameThrowerItem: Boolean; // Mod Flammenwerfer: True, das PowerUp (immer puLongerFlameLength) ist ein Flammenwerfer, der Client zeichnet einen roten Rahmen
  End;

  TFieldBricks = Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of TFieldBrick;

  TRenderAnimation = (
    raStandStill // -- Fertig
    , raWalk // -- Fertig
    , raKick // -- Fertig
    , raPunch
    , raPup // -- Fertig
    , raDie // -- Fertig
    , raTeleport // Teleport von Loch zu Loch
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
    Dying: Boolean; // Wenn True, dann "Stirbt" der Spieler gerade
    Animation: TRenderAnimation; // Welche Animation soll gerade Gerendert werden ?
    Counter: uint16; // Zähler Wie Lange der PLayer etwas "Tut" Dieing = True ist. -> Der kann ggf noch nach TPlayer wandern
    Value: uint16; // Bei raDie, raZen, raLockedIn muss eine bestimmte angewählt werden, hier steht ihr Index
    Position: TVector2; // Position in Weltkoordinaten (muss noch mit FieldBlockWidth, FieldBlockHeight skalliert werden zum Rendern
    Direction: Single; // Blickrichtung im Mathematischen Drehsinn
    ColorIndex: uint8; // 0 .. length(PlayerColors) - 1 -> Zur Anzeige / Auswahl beim Rendering
    HitPoints: uint8; // Mod Trefferpunkte: Anzeige im Client wie viele Treffer der Spieler noch aushält, 0 = nicht anzeigen (Mod aus oder ShowHitPoints = 0)
    TestInvulnTime: uint16; // Nur im Mod Test Modus: Restzeit der Unverwundbarkeit in ms (für die Zustandsanzeige), sonst 0
    TestStunTime: uint16; // Nur im Mod Test Modus: Restzeit des Stun in ms (für die Zustandsanzeige), sonst 0
    Ghost: Boolean; // Mod Geister: True, der Spieler ist tot (Alive = false) und läuft als Geist am Rand entlang
    GhostTime: uint16; // Mod Geister: Restzeit in ms bis der Geist verschwindet (für die Warnung im Client), 0 = kein Limit
    BrickWalk: Boolean; // Mod durch Blöcke laufen: True, der Spieler kann durch Blöcke laufen bzw. steht noch in einem (der Client zeichnet ihn durchsichtig)
    TestBrickWalkTime: uint16; // Nur im Mod Test Modus: Restzeit von "durch Blöcke laufen" in ms (für die Zustandsanzeige), sonst 0
    FlameThrowerLen: uint8; // Mod Flammenwerfer: Länge des Strahls in Kacheln in Blickrichtung, 0 = feuert gerade nicht
  End;

  (*
   * Befehle des Mod Test Modus zum Testen der Mods, werden per miDebugCommand vom Client zum Server gesendet
   *)
  TModTestCommand = (
    mtPositiveDisease // F5, Positive Krankheit -> Unverwundbar
    , mtHitMe // F6, Treffer wie durch eine Flamme (Trefferpunkt weg bzw. sterben)
    , mtGhost // F7, zum Geist machen bzw. als Geist zurück ins Spiel
    , mtHurry // F8, Hurry sofort starten
    , mtStun // F9, Stun
    , mtAllItems // F4, Kick, Punch, Grab, Bomben, Flammen
    , mtMaxHitPoints // F11, Trefferpunkte wieder auf Maximum
    , mtBrickWalk // F12, durch Blöcke laufen
    , mtFlash // F2, weißer Bildschirm
    , mtWhiteBomb // F1, die nächste gelegte Bombe ist weiß
    , mtSpawnItems // Einfg, die Items der neuen Mods auf freie Felder verteilen
    );

  TAtomicKey = (
    akNone, // Kein Gültiger Key
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
    TriggerBomb: Integer; // Anzahl wie viele Bomben der Spieler noch im Trigger Mode legen darf.
    JellyBombs: Boolean;
  End;

  TKeySet = (
    ks0 // Keyboard 0
    , ks1 // Keyboard 1
    );

  TAtomicKeyEvent = Procedure(Const aKey: TAtomicKey; Const aKeyset: TKeySet) Of Object;

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
    IsInHohle: Boolean; // True, when player is on a hohle
    Flying: Boolean; // is the player flying ? Not physically on the map
    FlyStart, FlyTarget: TVector2; // Start und Zielposition, wenn ein Spieler gerade "Fliegt"
    IdleTimer: UInt32; // Der Zähler der sich Merkt wie viele ms lang der Spieler keine "Eingaben" gemacht hat !! ist nicht exakt mehr so grob
    Disease: Set Of TDisease;
    BeforeSlowDiseaseSpeed: Single;
    DiseaseCounter: Integer; // Zähler der Mit Zählt wann die Krankheit wieder vorbei ist.
    LastSynchronTimeStamp: QWord;
    MoveState: TMoveState;
    Action: TAtomicAction;
    Powers: TAtomicPowers;
    PowerUpCounter: Array[TPowerUps] Of Integer; // Zähler, welche Powerups der Spieler wie oft aufgenommen hat, wenn er stirbt werden diese wieder "Verteilt"
    StunTimer: Integer; // Zeit in ms, wie lange der Spieler noch gestunnt ist (0 = nicht gestunnt), so lange kann er sich weder bewegen noch Aktionen ausführen
    InvulnTimer: Integer; // Zeit in ms, wie lange der Spieler noch unverwundbar ist (0 = verwundbar), endet spätestens mit Beginn des Hurry
    MaxHitPoints: Integer; // Mod Trefferpunkte: Trefferpunkte zu Rundenbeginn (aus [Handicap] bzw. DefaultHitPoints)
    HitPoints: Integer; // Mod Trefferpunkte: Anzahl Treffer die der Spieler noch aushält (Info.HitPoints ist nur die Anzeige davon)
    GhostRingPos: Single; // Mod Geister: Position auf dem Rahmen [0..GhostRingLength[, der ganzzahlige Anteil ist die Rahmenposition, siehe GhostRingToField
    GhostDir: Integer; // Mod Geister: Letzte Laufrichtung auf dem Ring (-1, 0, 1), damit läuft der Geist bis zum nächsten Randfeld weiter
    GhostCount: Integer; // Mod Geister: Wie oft der Spieler in dieser Runde schon Geist geworden ist
    GhostTimer: Integer; // Mod Geister: Zeit in ms bis der Geist verschwindet, wenn er keinen Gegner trifft (nur wenn GhostMaxTimeMs > 0)
    GhostBombActive: Boolean; // Mod Geister: True, so lange die geworfene Geisterbombe noch nicht explodiert ist
    StartGlove: Boolean; // Mod Handschuh beim Start: True, bis sich der Spieler in dieser Runde das erste Mal bewegt hat, so lange kann er Bomben werfen wie mit Handschuh
    BrickWalkTimer: Integer; // Mod durch Blöcke laufen: Zeit in ms, so lange kann der Spieler noch Blöcke betreten (danach nur noch heraus laufen)
    PunchStunCooldown: Integer; // Mod Boxhandschuh: Zeit in ms, so lange kann der Spieler keinen Gegner mehr stunnen
    WhiteBombs: Integer; // Mod weiße Bombe: So viele der nächsten gelegten Bomben sind weiß
    FlameThrowerFuel: Integer; // Mod Flammenwerfer: Zeit in ms, so lange kann der Spieler noch feuern (0 = kein Flammenwerfer)
    FlameThrowerFiring: Boolean; // Mod Flammenwerfer: True, so lange die zweite Aktion gedrückt ist
    FlameThrowerExposure: Integer; // Mod Flammenwerfer: Zeit in ms, so lange steht der Spieler schon am Stück in einem Strahl
    StartPosition: TVector2; // Mod Geister: Startposition der Runde, dort kommt ein Geist wieder ins Spiel
{$ENDIF}
  End;

  (*
   * Optionale Spiel Mods, werden aus atomic_server.ini geladen, alle Mods sind per Default aus.
   * Der Server liest sie beim Start, der Launcher bearbeitet sie im Mods Dialog.
   *)
  TModSettings = Record
    StunOnBombLanding: Boolean; // True, Landet eine geworfene Bombe auf einem Spieler, wird dieser gestunnt
    StunMinMs: Integer; // Minimale Stun Zeit in ms
    StunMaxMs: Integer; // Maximale Stun Zeit in ms
    PunchStun: Boolean; // True, mit dem Punch Item stunnt die zweite Aktion einen Gegner direkt vor dem Spieler
    PunchStunMinMs: Integer; // Minimale Stun Zeit in ms
    PunchStunMaxMs: Integer; // Maximale Stun Zeit in ms
    PunchStunCooldownMs: Integer; // Zeit in ms, so lange kann der Schläger danach nicht erneut schlagen (sonst wäre ein Dauer Stun möglich)
    PositiveDisease: Boolean; // True, ein Teil der Krankheits Items macht statt krank unverwundbar (nicht im Hurry)
    PositiveDiseaseChance: Integer; // in %, Anteil der Krankheits Items die positiv wirken
    PositiveMinMs: Integer; // Minimale Zeit in ms der Unverwundbarkeit durch die positive Krankheit
    PositiveMaxMs: Integer; // Maximale Zeit in ms der Unverwundbarkeit durch die positive Krankheit
    BrickWalk: Boolean; // True, ein Teil der Krankheits Items lässt durch zerstörbare Blöcke laufen (nicht im Hurry)
    BrickWalkChance: Integer; // in %, Anteil der Krankheits Items (die nicht schon positiv waren), die durch Blöcke laufen lassen
    BrickWalkMinMs: Integer; // Minimale Zeit in ms
    BrickWalkMaxMs: Integer; // Maximale Zeit in ms
    BrickWalkTransparency: Integer; // in %, so durchsichtig wird der Spieler gezeichnet (0 = normal, 100 = unsichtbar)
    FlashDisease: Boolean; // True, ein Teil der Krankheits Items macht den Bildschirm des Spielers kurz weiß
    FlashDiseaseChance: Integer; // in %, Anteil der Krankheits Items (die nicht schon positiv waren)
    FlashDiseaseIntensity: Integer; // in %, wie weiß der Bildschirm wird (100 = man sieht nichts mehr)
    FlashDiseaseMs: Integer; // Zeit in ms, so lange bleibt der Bildschirm weiß
    FlashDiseaseFadeMs: Integer; // Zeit in ms, so lange dauert der Übergang zurück
    WhiteBomb: Boolean; // True, ein Teil der Items unter den Steinen sind weiße Bomben, die alle Gegner flashen
    WhiteBombChance: Integer; // in %, Anteil der Items unter den Steinen, die zur weißen Bombe werden
    WhiteBombIntensity: Integer; // in %, wie weiß der Bildschirm der Gegner wird
    WhiteBombMs: Integer; // Zeit in ms, so lange bleibt der Bildschirm weiß
    WhiteBombFadeMs: Integer; // Zeit in ms, so lange dauert der Übergang zurück
    FlameThrower: Boolean; // True, ein Teil der Items unter den Steinen sind Flammenwerfer (zweite Aktion gedrückt halten)
    FlameThrowerChance: Integer; // in %, Anteil der Items unter den Steinen, die zum Flammenwerfer werden
    FlameThrowerFuelMs: Integer; // Zeit in ms, so lange kann man mit einem Item feuern (wird nur beim Feuern verbraucht, weitere Items füllen auf)
    FlameThrowerRange: Integer; // Reichweite des Strahls in Kacheln
    FlameThrowerKillMs: Integer; // Zeit in ms, so lange muss ein Gegner am Stück im Strahl stehen, bis er getroffen ist
    FlameThrowerBurnItems: Boolean; // True, Items im Strahl verbrennen sofort (wie bei einer Flamme), sonst geht der Strahl darüber hinweg
    Ghosts: Boolean; // True, Tote laufen bis zum Hurry als Geist am Rand und können mit einem Treffer eines Gegners zurück ins Spiel
    GhostReviveInvulnMs: Integer; // Zeit in ms, so lange ist ein zurückgekehrter Geist unverwundbar
    GhostFlameLen: Integer; // Flammenlänge der Geisterbombe
    GhostFlickerMs: Integer; // Zeit in ms, Geister sind im Client 3 Takte sichtbar und 1 Takt unsichtbar, 0 = kein Flackern
    GhostMaxCount: Integer; // Wie oft ein Spieler pro Runde Geist werden kann, 0 = unbegrenzt
    GhostMaxTimeMs: Integer; // Zeit in ms, nach derer ein Geist ohne Treffer auf einen Gegner verschwindet, 0 = unbegrenzt
    HitPoints: Boolean; // True, Spieler halten mehrere Treffer aus (pro Spielername in [Handicap], nicht im Hurry)
    InvulnTimeMs: Integer; // Zeit in ms, so lange ist der Spieler nach einem nicht tödlichen Treffer unverwundbar (>= FlameTime)
    DefaultHitPoints: Integer; // Trefferpunkte, wenn der Spieler nicht in [Handicap] steht
    ShowHitPoints: Boolean; // True, die Clients zeigen die Trefferpunkte über den Spielern an
    Handicap: TStringList; // Inhalt von [Handicap] als Spielername=Trefferpunkte
    GloveAtStart: Boolean; // True, bis zum ersten Schritt einer Runde kann jeder seine Bombe werfen wie mit Handschuh (gegen das Einsperren beim Start)
    OpenBorders: Boolean; // True, Spieler können über den Rand laufen und kommen auf der Gegenseite heraus
    OpenBordersTopBottom: Boolean; // Offene Grenzen oben <-> unten
    OpenBordersLeftRight: Boolean; // Offene Grenzen links <-> rechts
    OpenBordersActiveSec: Integer; // Zeit in s nach Rundenbeginn, ab der die Grenzen offen sind (damit keiner gleich aus seiner Startecke flieht)
    Teleporters: Boolean; // True, in der Mitte der Ränder gibt es Teleporter, die zum gegenüberliegenden Rand bringen (nicht im Hurry)
    TeleportersTopBottom: Boolean; // Teleporter oben <-> unten
    TeleportersLeftRight: Boolean; // Teleporter links <-> rechts
    TeleportersActiveSec: Integer; // Zeit in s nach Rundenbeginn, ab der die Teleporter erscheinen und wirken
    (*
     * [Debug] Abschnitt, nur zum Testen der Mods
     *)
    ModTestMode: Boolean; // Master Schalter für den Mod Test Modus, nur wenn True werden die folgenden Debug Werte und die Test Tasten ausgewertet
    StartItems: String; // z.B. "kick,punch,grab,bomb*3" diese Items bekommt jeder Spieler zu Rundenbeginn
    RoundTimeSec: Integer; // > 0 überschreibt die Rundenzeit in s (Hurry beginnt bei 80s Restzeit, 90 = Hurry nach 10s)
    StartInvulnMs: Integer; // Zeit in ms, so lange sind alle Spieler zu Rundenbeginn unverwundbar
    GhostsKeepRound: Boolean; // True, Geister zählen für das Rundenende als lebendig (damit kann man Geister auch zu zweit testen)
  End;

  TPlayerGetsPowerUpEvent = Procedure(Var Player: TPlayer; PlayerIndex: integer; PowerUp: TPowerUps) Of Object;

  TVictor = (vRedTeam, vWhiteTeam, vCol0, vCol1, vCol2, vCol3, vCol4, vCol5, vCol6, vCol7, vCol8, vCol9);

  TPlayers = Array[0..length(PlayerColors) - 1] Of TPlayer;

  TKeys = Record
    UseSDL2: Boolean;
    // if UseSDL2 then this is used
    Name: String;
    NameIndex: Integer; // Der Index, Der Wievielte Controller das ist (nur Relevant wenn mehrere Controller mit dem Selben Namen genutzt werden..)
    AchsisIdle: Array[0..1] Of Integer; // 0 = Hoch Runter, 1 = Links Rechts
    AchsisIndex: Array[0..1] Of integer; // 0 = Hoch Runter, 1 = Links Rechts
    AchsisDirection: Array[0..1] Of integer; // 0 = Hoch Runter, 1 = Links Rechts
    ButtonsIdle: Array[0..1] Of Boolean; // 0 = First, 1 = Second
    ButtonIndex: Array[0..1] Of integer; // 0 = First, 1 = Second
    IsGameController: Boolean; // Wenn True, dann werden anstadt der Achsen die D_Pads verwendet
    // if not UseSDL2 then this key's are valid
    KeyUp: Word;
    KeyDown: Word;
    KeyLeft: Word;
    KeyRight: Word;
    KeyPrimary: Word;
    KeySecondary: Word;
  End;

  TKeyArray = Array[TKeySet] Of TKeys;

  TAtomicSettings = Record
    // Werte aus den Optionen
    TeamPlay: Boolean;
    RandomStart: Boolean;
{$IFDEF Client}
    NodeName: String;
    Proportional: Boolean; // Screen mit Schwarzen Balken, oder ohne
{$ENDIF}
    ConveyorSpeed: TConveyorSpeed;
    SchemeFile: String;
    PlayTime: integer; // in [s]
    LostPlayersRevertToAI: Boolean;
{$IFDEF Client}
    PlaySounds: Boolean;
    Keys: TKeyArray;
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
    Router_IP: String; // Für das "j" command
    Router_Port: String; // Für das "j" command
    // --- Werte die "Intern" sind
    CheckForUpdates: Boolean; // Eigentlich nicht Steuerbar, nur für die Entwicklung gedacht ..
{$ENDIF}
{$IFDEF Server}
    MasterUid: Integer; // -1 = undefiniert, sonst die Uid desjenigen, der in den Menüs das sagen hat. Wird mit miRequestLogin gesetzt
    Scheme: TScheme; // Wird mit miUpdateSettings gesetzt
{$ENDIF}
  End;


Procedure SchemeToStream(Const Stream: TStream; Const Scheme: TScheme); // Speichert ein Scheme in einen Stream
Function SchemeFromStream(Const Stream: TStream; Out Scheme: TScheme): Boolean;
Function GetDefaultScheme(): TScheme;
Function ModTestCommandToString(Value: TModTestCommand): String;

(*
 * Gemeinsamer Code von Server und Launcher für atomic_server.ini, damit beide dieselben Defaults und Grenzen nutzen.
 * Settings.Handicap wird bei Bedarf erzeugt und muss vom Aufrufer wieder frei gegeben werden.
 *)
Procedure ModSettingsDefaults(Var Settings: TModSettings);
// Liest alle Werte (fehlende = Default), zieht ungültige Werte mit einer Warnung auf ihre Grenzen und legt fehlende Schlüssel in der Datei an
Procedure LoadModSettingsFromFile(Const Filename: String; Var Settings: TModSettings);
// Schreibt alle Werte und [Handicap] komplett neu (Kommentare in [Handicap] gehen dabei verloren)
Function SaveModSettingsToFile(Const Filename: String; Const Settings: TModSettings): Boolean;

{$IFDEF Server}
Function GhostRingToField(Index: integer): TPoint; // Rechnet einen Index auf dem Rahmen in Feld Koordinaten um (liegt immer außerhalb des Feldes)
Function FieldToGhostRing(Const Position: TVector2): integer; // Liefert den Index der Rahmenposition, welche am nächsten an Position liegt
Function GhostRingToPosition(RingPos: Single): TVector2; // Position in Feld Koordinaten, zwischen zwei Randfeldern wird interpoliert
{$ENDIF}

{$IFNDEF Server} // Das nutzt auch der Launcher, also darf hier nicht auf Client geprüft werden sondern es muss nicht server sein ;)
Function AtomicPlayerColorToColor(aColor: TRGB): TColor;
Function AtomicDefaultKeys(Index: TKeySet): TKeys;
{$ENDIF}

Procedure Nop(); // Nur zum Debuggen ;)
Function BrickSchemeToString(Const Scheme: TScheme): String; // Only Debug

// Berechnet alle Spieler abhängigen Geschwindigkeiten und setzt die Spielergeschwindigkeit auf NewDefaultSpeed
Procedure CalculateAtomicSpeeds(NewDefaultSpeed: Single);

Function KeySetToString(aKeySet: TKeySet): String;
Function StringToKeySet(aValue: String): TKeySet;

Function ValidNodeName(aNodeName: String): Boolean;

{$IFNDEF Server} // Das nutzt der Launcher und der Client
(*
 * If Dynamic loading of sdl2.dll failed, this routing try's to get at least the fileversion
 *)
Function GetSDL2VersionByManualDynamicLoading(LibFilename: String): String;
{$ENDIF}

Implementation

Uses math, IniFiles, uatomic_global
{$IFNDEF Server} // Das nutzt auch der Launcher, also darf hier nicht auf Client geprüft werden sondern es muss nicht server sein ;)
  , LCLType
  , SDL2, dynlibs
{$ENDIF}
  ;

Procedure Nop(); // Nur zum Debuggen ;)
Begin

End;

Function BrickSchemeToString(Const Scheme: TScheme): String; // Only Debug
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

Procedure CalculateAtomicSpeeds(NewDefaultSpeed: Single);
Begin
  AtomicDefaultSpeed := NewDefaultSpeed;
  AtomicMaxSpeed := AtomicDefaultSpeed * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange; // Maximale Geschwindigkeit Eines Atomic in Kacheln Pro Sekunde
  AtomicSlowSpeed := AtomicDefaultSpeed / (AtomicSpeedChange * AtomicSpeedChange); // Niedrigst mögliche Geschwindigkeit ("Schnecke") in Kacheln Pro Sekunde
  ConveyorSlowSpeed := AtomicDefaultSpeed / (AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange);
  ConveyorMiddleSpeed := AtomicDefaultSpeed / (AtomicSpeedChange * AtomicSpeedChange * AtomicSpeedChange);
  ConveyorFastSpeed := AtomicDefaultSpeed / (AtomicSpeedChange * AtomicSpeedChange);
End;

Function KeySetToString(aKeySet: TKeySet): String;
Begin
  result := '';
  Case aKeySet Of
    ks0: result := 'ks0';
    ks1: result := 'ks1';
  Else Begin
      Raise exception.create('Error: KeySetToString, missing implementation.');
    End;
  End;
End;

Function StringToKeySet(aValue: String): TKeySet;
Begin
  Case lowercase(trim(aValue)) Of
    'ks0': result := ks0;
    'ks1': result := ks1;
  Else Begin
      Raise exception.create('Error: StringToKeySet, missing implementation for: ' + aValue);
    End;
  End;
End;

Function ValidNodeName(aNodeName: String): Boolean;
Var
  i: Integer;
Begin
  result := true;
  If trim(aNodeName) = '' Then result := false;
  If length(aNodeName) > 19 Then result := false;
  For i := 1 To length(aNodeName) Do Begin
    If (Not (aNodeName[i] In ['a'..'z', 'A'..'Z', '0'..'9', ' ', '_'])) Then Result := false;
  End;
  If lowercase(trim(aNodeName)) = 'off' Then result := false;
  If lowercase(trim(aNodeName)) = 'key 0' Then result := false;
  If lowercase(trim(aNodeName)) = 'key 1' Then result := false;
End;

{$IFNDEF Server}

Function GetSDL2VersionByManualDynamicLoading(LibFilename: String): String;
Var
  LibHandle: TLibHandle;
  GetVersion: TSDL_GetVersion_proc;
  ver: TSDL_Version;
Begin
  result := 'Not installed / found.';
  If LibFilename = '' Then
    LibFilename := SDL_LibName;
  LibHandle := LoadLibrary(LibFilename);
  If LibHandle <> 0 Then Begin
    GetVersion := TSDL_GetVersion_proc(GetProcAddress(LibHandle, 'SDL_GetVersion'));
    If assigned(GetVersion) Then Begin
      GetVersion(@ver);
      result := format('%d.%d.%d', [ver.major, ver.minor, ver.patch]);
    End;
    UnloadLibrary(LibHandle);
  End;
End;

Function AtomicDefaultKeys(Index: TKeySet): TKeys;
Begin
  result.UseSDL2 := false;
  result.IsGameController := false;
  result.Name := '';
  result.NameIndex := 0;
  result.AchsisIndex[0] := -1;
  result.AchsisIndex[1] := -1;
  result.AchsisIdle[0] := 0;
  result.AchsisIdle[1] := 0;
  result.AchsisDirection[0] := 0;
  result.AchsisDirection[1] := 0;
  result.ButtonIndex[0] := -1;
  result.ButtonIndex[1] := -1;
  result.ButtonsIdle[0] := false;
  result.ButtonsIdle[1] := false;
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

Function AtomicPlayerColorToColor(aColor: TRGB): TColor;
Var
  r, g, b: integer;
Begin
  r := min(255, round(aColor.r * 2.55));
  g := min(255, round(aColor.g * 2.55));
  b := min(255, round(aColor.b * 2.55));
  result := Graphics.RGBToColor(r, g, b);
End;
{$ENDIF}

Procedure SchemeToStream(Const Stream: TStream; Const Scheme: TScheme);
Begin
  Stream.WriteAnsiString(Scheme.Filename);
  Stream.WriteAnsiString(Scheme.Name);
  stream.Write(Scheme.BrickDensity, SizeOf(Scheme.BrickDensity));
  stream.Write(Scheme.BrickData, SizeOf(Scheme.BrickData));
  stream.Write(Scheme.PlayerStartPositions, SizeOf(Scheme.PlayerStartPositions));
  stream.Write(Scheme.PowerUps, SizeOf(Scheme.PowerUps));
End;

Function SchemeFromStream(Const Stream: TStream; Out Scheme: TScheme): Boolean;
Begin
  result := false;
  Scheme.Filename := stream.ReadAnsiString;
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
    result.Team := min(1, max(0, t)); // Sicherstellen das Team auch wirklich nur 0 oder 1 sein kann !
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
  result.Filename := 'BASIC.SCH';
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

{$IFDEF Server}

Function GhostRingToField(Index: integer): TPoint;
Begin
  index := index Mod GhostRingLength;
  If index < 0 Then index := index + GhostRingLength;
  If index < FieldWidth Then Begin // Oben von links nach rechts
    result := point(index, -1);
  End
  Else If index < FieldWidth + FieldHeight Then Begin // Rechts von oben nach unten
    result := point(FieldWidth, index - FieldWidth);
  End
  Else If index < 2 * FieldWidth + FieldHeight Then Begin // Unten von rechts nach links
    result := point(FieldWidth - 1 - (index - FieldWidth - FieldHeight), FieldHeight);
  End
  Else Begin // Links von unten nach oben
    result := point(-1, FieldHeight - 1 - (index - 2 * FieldWidth - FieldHeight));
  End;
End;

Function GhostRingToPosition(RingPos: Single): TVector2;
Var
  k: integer;
  f: Single;
  a, b: TPoint;
Begin
  k := trunc(RingPos);
  f := RingPos - k;
  a := GhostRingToField(k);
  b := GhostRingToField(k + 1);
  result := v2(a.x + 0.5 + (b.x - a.x) * f, a.y + 0.5 + (b.y - a.y) * f);
End;

Function FieldToGhostRing(Const Position: TVector2): integer;
Var
  i: integer;
  p: TPoint;
  d, best: Single;
Begin
  result := 0;
  best := 1000;
  For i := 0 To GhostRingLength - 1 Do Begin
    p := GhostRingToField(i);
    d := sqr(p.x + 0.5 - Position.x) + sqr(p.y + 0.5 - Position.y);
    If d < best Then Begin
      best := d;
      result := i;
    End;
  End;
End;
{$ENDIF}

Procedure ModSettingsDefaults(Var Settings: TModSettings);
Begin
  // Alle Mods sind aus
  Settings.StunOnBombLanding := false;
  Settings.StunMinMs := 100;
  Settings.StunMaxMs := 500;
  Settings.PunchStun := false;
  Settings.PunchStunMinMs := 300;
  Settings.PunchStunMaxMs := 800;
  Settings.PunchStunCooldownMs := 1500;
  Settings.PositiveDisease := false;
  Settings.PositiveDiseaseChance := 15;
  Settings.PositiveMinMs := 5000;
  Settings.PositiveMaxMs := 20000;
  Settings.BrickWalk := false;
  Settings.BrickWalkChance := 15;
  Settings.BrickWalkMinMs := 5000;
  Settings.BrickWalkMaxMs := 10000;
  Settings.BrickWalkTransparency := 50;
  Settings.FlashDisease := false;
  Settings.FlashDiseaseChance := 15;
  Settings.FlashDiseaseIntensity := 90;
  Settings.FlashDiseaseMs := 2500;
  Settings.FlashDiseaseFadeMs := 2000;
  Settings.WhiteBomb := false;
  Settings.WhiteBombChance := 10;
  Settings.WhiteBombIntensity := 90;
  Settings.WhiteBombMs := 2500;
  Settings.WhiteBombFadeMs := 2000;
  Settings.FlameThrower := false;
  Settings.FlameThrowerChance := 5;
  Settings.FlameThrowerFuelMs := 5000;
  Settings.FlameThrowerRange := 3;
  Settings.FlameThrowerKillMs := 1000;
  Settings.FlameThrowerBurnItems := true;
  Settings.Ghosts := false;
  Settings.GhostReviveInvulnMs := 3000;
  Settings.GhostFlameLen := 2;
  Settings.GhostFlickerMs := 200;
  Settings.GhostMaxCount := 1;
  Settings.GhostMaxTimeMs := 0;
  Settings.HitPoints := false;
  Settings.InvulnTimeMs := 5000;
  Settings.DefaultHitPoints := 1;
  Settings.ShowHitPoints := true;
  If Not assigned(Settings.Handicap) Then Settings.Handicap := TStringList.Create;
  Settings.Handicap.Clear;
  Settings.GloveAtStart := false;
  Settings.OpenBorders := false;
  Settings.OpenBordersTopBottom := true;
  Settings.OpenBordersLeftRight := true;
  Settings.OpenBordersActiveSec := 30;
  Settings.Teleporters := false;
  Settings.TeleportersTopBottom := true;
  Settings.TeleportersLeftRight := true;
  Settings.TeleportersActiveSec := 60;
  Settings.ModTestMode := false;
  Settings.StartItems := '';
  Settings.RoundTimeSec := 0;
  Settings.StartInvulnMs := 0;
  Settings.GhostsKeepRound := false;
End;

(*
 * Schreibt alle Werte außer [Handicap], bei OnlyMissing nur die, die in der Datei noch fehlen.
 * Damit stehen die Schlüssel für Laden (fehlende anlegen) und Speichern nur an einer Stelle.
 *)

Procedure WriteModSettings(Const ini: TIniFile; Const Settings: TModSettings; OnlyMissing: Boolean);

  Procedure WriteB(Const Section, Key: String; Value: Boolean);
  Begin
    If OnlyMissing And ini.ValueExists(Section, Key) Then exit;
    ini.WriteBool(Section, Key, Value);
  End;

  Procedure WriteI(Const Section, Key: String; Value: Integer);
  Begin
    If OnlyMissing And ini.ValueExists(Section, Key) Then exit;
    ini.WriteInteger(Section, Key, Value);
  End;

  Procedure WriteS(Const Section, Key: String; Value: String);
  Begin
    If OnlyMissing And ini.ValueExists(Section, Key) Then exit;
    ini.WriteString(Section, Key, Value);
  End;

Begin
  WriteB('Mods', 'StunOnBombLanding', Settings.StunOnBombLanding);
  WriteI('Mods', 'StunMinMs', Settings.StunMinMs);
  WriteI('Mods', 'StunMaxMs', Settings.StunMaxMs);
  WriteB('Mods', 'PunchStun', Settings.PunchStun);
  WriteI('Mods', 'PunchStunMinMs', Settings.PunchStunMinMs);
  WriteI('Mods', 'PunchStunMaxMs', Settings.PunchStunMaxMs);
  WriteI('Mods', 'PunchStunCooldownMs', Settings.PunchStunCooldownMs);
  WriteB('Mods', 'PositiveDisease', Settings.PositiveDisease);
  WriteI('Mods', 'PositiveDiseaseChance', Settings.PositiveDiseaseChance);
  WriteI('Mods', 'PositiveMinMs', Settings.PositiveMinMs);
  WriteI('Mods', 'PositiveMaxMs', Settings.PositiveMaxMs);
  WriteB('Mods', 'BrickWalk', Settings.BrickWalk);
  WriteI('Mods', 'BrickWalkChance', Settings.BrickWalkChance);
  WriteI('Mods', 'BrickWalkMinMs', Settings.BrickWalkMinMs);
  WriteI('Mods', 'BrickWalkMaxMs', Settings.BrickWalkMaxMs);
  WriteI('Mods', 'BrickWalkTransparency', Settings.BrickWalkTransparency);
  WriteB('Mods', 'FlashDisease', Settings.FlashDisease);
  WriteI('Mods', 'FlashDiseaseChance', Settings.FlashDiseaseChance);
  WriteI('Mods', 'FlashDiseaseIntensity', Settings.FlashDiseaseIntensity);
  WriteI('Mods', 'FlashDiseaseMs', Settings.FlashDiseaseMs);
  WriteI('Mods', 'FlashDiseaseFadeMs', Settings.FlashDiseaseFadeMs);
  WriteB('Mods', 'WhiteBomb', Settings.WhiteBomb);
  WriteI('Mods', 'WhiteBombChance', Settings.WhiteBombChance);
  WriteI('Mods', 'WhiteBombIntensity', Settings.WhiteBombIntensity);
  WriteI('Mods', 'WhiteBombMs', Settings.WhiteBombMs);
  WriteI('Mods', 'WhiteBombFadeMs', Settings.WhiteBombFadeMs);
  WriteB('Mods', 'FlameThrower', Settings.FlameThrower);
  WriteI('Mods', 'FlameThrowerChance', Settings.FlameThrowerChance);
  WriteI('Mods', 'FlameThrowerFuelMs', Settings.FlameThrowerFuelMs);
  WriteI('Mods', 'FlameThrowerRange', Settings.FlameThrowerRange);
  WriteI('Mods', 'FlameThrowerKillMs', Settings.FlameThrowerKillMs);
  WriteB('Mods', 'FlameThrowerBurnItems', Settings.FlameThrowerBurnItems);
  WriteB('Mods', 'Ghosts', Settings.Ghosts);
  WriteI('Mods', 'GhostReviveInvulnMs', Settings.GhostReviveInvulnMs);
  WriteI('Mods', 'GhostFlameLen', Settings.GhostFlameLen);
  WriteI('Mods', 'GhostFlickerMs', Settings.GhostFlickerMs);
  WriteI('Mods', 'GhostMaxCount', Settings.GhostMaxCount);
  WriteI('Mods', 'GhostMaxTimeMs', Settings.GhostMaxTimeMs);
  WriteB('Mods', 'HitPoints', Settings.HitPoints);
  WriteI('Mods', 'InvulnTimeMs', Settings.InvulnTimeMs);
  WriteI('Mods', 'DefaultHitPoints', Settings.DefaultHitPoints);
  WriteB('Mods', 'ShowHitPoints', Settings.ShowHitPoints);
  WriteB('Mods', 'GloveAtStart', Settings.GloveAtStart);
  WriteB('Mods', 'OpenBorders', Settings.OpenBorders);
  WriteB('Mods', 'OpenBordersTopBottom', Settings.OpenBordersTopBottom);
  WriteB('Mods', 'OpenBordersLeftRight', Settings.OpenBordersLeftRight);
  WriteI('Mods', 'OpenBordersActiveSec', Settings.OpenBordersActiveSec);
  WriteB('Mods', 'Teleporters', Settings.Teleporters);
  WriteB('Mods', 'TeleportersTopBottom', Settings.TeleportersTopBottom);
  WriteB('Mods', 'TeleportersLeftRight', Settings.TeleportersLeftRight);
  WriteI('Mods', 'TeleportersActiveSec', Settings.TeleportersActiveSec);
  WriteB('Debug', 'ModTestMode', Settings.ModTestMode);
  WriteS('Debug', 'StartItems', Settings.StartItems);
  WriteI('Debug', 'RoundTimeSec', Settings.RoundTimeSec);
  WriteI('Debug', 'StartInvulnMs', Settings.StartInvulnMs);
  WriteB('Debug', 'GhostsKeepRound', Settings.GhostsKeepRound);
End;

Procedure LoadModSettingsFromFile(Const Filename: String;
  Var Settings: TModSettings);

  // Zieht einen ungültigen Wert mit einer Warnung auf MinValue..MaxValue
  Procedure Clamp(Const Key: String; Var Value: Integer; MinValue, MaxValue: Integer);
  Begin
    If (Value < MinValue) Or (Value > MaxValue) Then Begin
      log(ExtractFileName(Filename) + ': ' + Key + ' not in ' + inttostr(MinValue) + '..' + inttostr(MaxValue) + ', clamping', llWarning);
      Value := min(MaxValue, max(MinValue, Value));
    End;
  End;

Var
  ini: TIniFile;
Begin
  ModSettingsDefaults(Settings);
  If Not ForceDirectories(ExtractFileDir(Filename)) Then Begin
    log('Unable to create: ' + Filename, llCritical);
    exit;
  End;
  ini := TIniFile.Create(Filename);
  Try
    Settings.StunOnBombLanding := ini.ReadBool('Mods', 'StunOnBombLanding', Settings.StunOnBombLanding);
    Settings.StunMinMs := ini.ReadInteger('Mods', 'StunMinMs', Settings.StunMinMs);
    Settings.StunMaxMs := ini.ReadInteger('Mods', 'StunMaxMs', Settings.StunMaxMs);
    Settings.PunchStun := ini.ReadBool('Mods', 'PunchStun', Settings.PunchStun);
    Settings.PunchStunMinMs := ini.ReadInteger('Mods', 'PunchStunMinMs', Settings.PunchStunMinMs);
    Settings.PunchStunMaxMs := ini.ReadInteger('Mods', 'PunchStunMaxMs', Settings.PunchStunMaxMs);
    Settings.PunchStunCooldownMs := ini.ReadInteger('Mods', 'PunchStunCooldownMs', Settings.PunchStunCooldownMs);
    Settings.PositiveDisease := ini.ReadBool('Mods', 'PositiveDisease', Settings.PositiveDisease);
    Settings.PositiveDiseaseChance := ini.ReadInteger('Mods', 'PositiveDiseaseChance', Settings.PositiveDiseaseChance);
    Settings.PositiveMinMs := ini.ReadInteger('Mods', 'PositiveMinMs', Settings.PositiveMinMs);
    Settings.PositiveMaxMs := ini.ReadInteger('Mods', 'PositiveMaxMs', Settings.PositiveMaxMs);
    Settings.BrickWalk := ini.ReadBool('Mods', 'BrickWalk', Settings.BrickWalk);
    Settings.BrickWalkChance := ini.ReadInteger('Mods', 'BrickWalkChance', Settings.BrickWalkChance);
    Settings.BrickWalkMinMs := ini.ReadInteger('Mods', 'BrickWalkMinMs', Settings.BrickWalkMinMs);
    Settings.BrickWalkMaxMs := ini.ReadInteger('Mods', 'BrickWalkMaxMs', Settings.BrickWalkMaxMs);
    Settings.BrickWalkTransparency := ini.ReadInteger('Mods', 'BrickWalkTransparency', Settings.BrickWalkTransparency);
    Settings.FlashDisease := ini.ReadBool('Mods', 'FlashDisease', Settings.FlashDisease);
    Settings.FlashDiseaseChance := ini.ReadInteger('Mods', 'FlashDiseaseChance', Settings.FlashDiseaseChance);
    Settings.FlashDiseaseIntensity := ini.ReadInteger('Mods', 'FlashDiseaseIntensity', Settings.FlashDiseaseIntensity);
    Settings.FlashDiseaseMs := ini.ReadInteger('Mods', 'FlashDiseaseMs', Settings.FlashDiseaseMs);
    Settings.FlashDiseaseFadeMs := ini.ReadInteger('Mods', 'FlashDiseaseFadeMs', Settings.FlashDiseaseFadeMs);
    Settings.WhiteBomb := ini.ReadBool('Mods', 'WhiteBomb', Settings.WhiteBomb);
    Settings.WhiteBombChance := ini.ReadInteger('Mods', 'WhiteBombChance', Settings.WhiteBombChance);
    Settings.WhiteBombIntensity := ini.ReadInteger('Mods', 'WhiteBombIntensity', Settings.WhiteBombIntensity);
    Settings.WhiteBombMs := ini.ReadInteger('Mods', 'WhiteBombMs', Settings.WhiteBombMs);
    Settings.WhiteBombFadeMs := ini.ReadInteger('Mods', 'WhiteBombFadeMs', Settings.WhiteBombFadeMs);
    Settings.FlameThrower := ini.ReadBool('Mods', 'FlameThrower', Settings.FlameThrower);
    Settings.FlameThrowerChance := ini.ReadInteger('Mods', 'FlameThrowerChance', Settings.FlameThrowerChance);
    Settings.FlameThrowerFuelMs := ini.ReadInteger('Mods', 'FlameThrowerFuelMs', Settings.FlameThrowerFuelMs);
    Settings.FlameThrowerRange := ini.ReadInteger('Mods', 'FlameThrowerRange', Settings.FlameThrowerRange);
    Settings.FlameThrowerKillMs := ini.ReadInteger('Mods', 'FlameThrowerKillMs', Settings.FlameThrowerKillMs);
    Settings.FlameThrowerBurnItems := ini.ReadBool('Mods', 'FlameThrowerBurnItems', Settings.FlameThrowerBurnItems);
    Settings.Ghosts := ini.ReadBool('Mods', 'Ghosts', Settings.Ghosts);
    Settings.GhostReviveInvulnMs := ini.ReadInteger('Mods', 'GhostReviveInvulnMs', Settings.GhostReviveInvulnMs);
    Settings.GhostFlameLen := ini.ReadInteger('Mods', 'GhostFlameLen', Settings.GhostFlameLen);
    Settings.GhostFlickerMs := ini.ReadInteger('Mods', 'GhostFlickerMs', Settings.GhostFlickerMs);
    Settings.GhostMaxCount := ini.ReadInteger('Mods', 'GhostMaxCount', Settings.GhostMaxCount);
    Settings.GhostMaxTimeMs := ini.ReadInteger('Mods', 'GhostMaxTimeMs', Settings.GhostMaxTimeMs);
    Settings.HitPoints := ini.ReadBool('Mods', 'HitPoints', Settings.HitPoints);
    Settings.InvulnTimeMs := ini.ReadInteger('Mods', 'InvulnTimeMs', Settings.InvulnTimeMs);
    Settings.DefaultHitPoints := ini.ReadInteger('Mods', 'DefaultHitPoints', Settings.DefaultHitPoints);
    Settings.ShowHitPoints := ini.ReadBool('Mods', 'ShowHitPoints', Settings.ShowHitPoints);
    ini.ReadSectionValues('Handicap', Settings.Handicap); // Spielername=Trefferpunkte
    Settings.GloveAtStart := ini.ReadBool('Mods', 'GloveAtStart', Settings.GloveAtStart);
    Settings.OpenBorders := ini.ReadBool('Mods', 'OpenBorders', Settings.OpenBorders);
    Settings.OpenBordersTopBottom := ini.ReadBool('Mods', 'OpenBordersTopBottom', Settings.OpenBordersTopBottom);
    Settings.OpenBordersLeftRight := ini.ReadBool('Mods', 'OpenBordersLeftRight', Settings.OpenBordersLeftRight);
    Settings.OpenBordersActiveSec := ini.ReadInteger('Mods', 'OpenBordersActiveSec', Settings.OpenBordersActiveSec);
    Settings.Teleporters := ini.ReadBool('Mods', 'Teleporters', Settings.Teleporters);
    Settings.TeleportersTopBottom := ini.ReadBool('Mods', 'TeleportersTopBottom', Settings.TeleportersTopBottom);
    Settings.TeleportersLeftRight := ini.ReadBool('Mods', 'TeleportersLeftRight', Settings.TeleportersLeftRight);
    Settings.TeleportersActiveSec := ini.ReadInteger('Mods', 'TeleportersActiveSec', Settings.TeleportersActiveSec);
    Settings.ModTestMode := ini.ReadBool('Debug', 'ModTestMode', Settings.ModTestMode);
    Settings.StartItems := ini.ReadString('Debug', 'StartItems', Settings.StartItems);
    Settings.RoundTimeSec := ini.ReadInteger('Debug', 'RoundTimeSec', Settings.RoundTimeSec);
    Settings.StartInvulnMs := ini.ReadInteger('Debug', 'StartInvulnMs', Settings.StartInvulnMs);
    Settings.GhostsKeepRound := ini.ReadBool('Debug', 'GhostsKeepRound', Settings.GhostsKeepRound);
    (*
     * Ungültige Werte auf gültige Grenzen ziehen
     *)
    Clamp('StunMinMs', Settings.StunMinMs, 0, ModMaxStunMs);
    Clamp('StunMaxMs', Settings.StunMaxMs, 0, ModMaxStunMs);
    If Settings.StunMaxMs < Settings.StunMinMs Then Begin
      log(ExtractFileName(Filename) + ': StunMaxMs < StunMinMs, using StunMinMs', llWarning);
      Settings.StunMaxMs := Settings.StunMinMs;
    End;
    Clamp('PunchStunMinMs', Settings.PunchStunMinMs, 0, ModMaxStunMs);
    Clamp('PunchStunMaxMs', Settings.PunchStunMaxMs, 0, ModMaxStunMs);
    If Settings.PunchStunMaxMs < Settings.PunchStunMinMs Then Begin
      log(ExtractFileName(Filename) + ': PunchStunMaxMs < PunchStunMinMs, using PunchStunMinMs', llWarning);
      Settings.PunchStunMaxMs := Settings.PunchStunMinMs;
    End;
    Clamp('PunchStunCooldownMs', Settings.PunchStunCooldownMs, 0, ModMaxPunchStunCooldownMs);
    Clamp('PositiveDiseaseChance', Settings.PositiveDiseaseChance, 0, ModMaxPositiveDiseaseChance);
    Clamp('PositiveMinMs', Settings.PositiveMinMs, 0, ModMaxPositiveMs);
    Clamp('PositiveMaxMs', Settings.PositiveMaxMs, 0, ModMaxPositiveMs);
    If Settings.PositiveMaxMs < Settings.PositiveMinMs Then Begin
      log(ExtractFileName(Filename) + ': PositiveMaxMs < PositiveMinMs, using PositiveMinMs', llWarning);
      Settings.PositiveMaxMs := Settings.PositiveMinMs;
    End;
    Clamp('BrickWalkChance', Settings.BrickWalkChance, 0, ModMaxBrickWalkChance);
    Clamp('BrickWalkMinMs', Settings.BrickWalkMinMs, 0, ModMaxBrickWalkMs);
    Clamp('BrickWalkMaxMs', Settings.BrickWalkMaxMs, 0, ModMaxBrickWalkMs);
    If Settings.BrickWalkMaxMs < Settings.BrickWalkMinMs Then Begin
      log(ExtractFileName(Filename) + ': BrickWalkMaxMs < BrickWalkMinMs, using BrickWalkMinMs', llWarning);
      Settings.BrickWalkMaxMs := Settings.BrickWalkMinMs;
    End;
    Clamp('BrickWalkTransparency', Settings.BrickWalkTransparency, 0, ModMaxBrickWalkTransparency);
    Clamp('FlashDiseaseChance', Settings.FlashDiseaseChance, 0, ModMaxFlashChance);
    Clamp('FlashDiseaseIntensity', Settings.FlashDiseaseIntensity, 0, ModMaxFlashIntensity);
    Clamp('FlashDiseaseMs', Settings.FlashDiseaseMs, 0, ModMaxFlashMs);
    Clamp('FlashDiseaseFadeMs', Settings.FlashDiseaseFadeMs, 0, ModMaxFlashMs);
    Clamp('WhiteBombChance', Settings.WhiteBombChance, 0, ModMaxFlashChance);
    Clamp('WhiteBombIntensity', Settings.WhiteBombIntensity, 0, ModMaxFlashIntensity);
    Clamp('WhiteBombMs', Settings.WhiteBombMs, 0, ModMaxFlashMs);
    Clamp('WhiteBombFadeMs', Settings.WhiteBombFadeMs, 0, ModMaxFlashMs);
    Clamp('FlameThrowerChance', Settings.FlameThrowerChance, 0, ModMaxFlameThrowerChance);
    Clamp('FlameThrowerFuelMs', Settings.FlameThrowerFuelMs, 0, ModMaxFlameThrowerFuelMs);
    Clamp('FlameThrowerRange', Settings.FlameThrowerRange, 1, ModMaxFlameThrowerRange);
    Clamp('FlameThrowerKillMs', Settings.FlameThrowerKillMs, 0, ModMaxFlameThrowerKillMs);
    Clamp('GhostReviveInvulnMs', Settings.GhostReviveInvulnMs, 0, ModMaxGhostReviveInvulnMs);
    Clamp('GhostFlameLen', Settings.GhostFlameLen, 1, ModMaxGhostFlameLen);
    Clamp('GhostFlickerMs', Settings.GhostFlickerMs, 0, ModMaxGhostFlickerMs);
    Clamp('GhostMaxCount', Settings.GhostMaxCount, 0, ModMaxGhostCount);
    Clamp('GhostMaxTimeMs', Settings.GhostMaxTimeMs, 0, ModMaxGhostTimeMs);
    Clamp('InvulnTimeMs', Settings.InvulnTimeMs, FlameTime, ModMaxInvulnTimeMs);
    Clamp('DefaultHitPoints', Settings.DefaultHitPoints, 1, ModMaxHitPoints);
    Clamp('RoundTimeSec', Settings.RoundTimeSec, 0, ModMaxRoundTimeSec);
    Clamp('StartInvulnMs', Settings.StartInvulnMs, 0, ModMaxStartInvulnMs);
    Clamp('TeleportersActiveSec', Settings.TeleportersActiveSec, 0, ModMaxTeleportersActiveSec);
    Clamp('OpenBordersActiveSec', Settings.OpenBordersActiveSec, 0, ModMaxOpenBordersActiveSec);
    (*
     * Fehlende Schlüssel mit ihren Defaults anlegen, damit man sie in der Datei auch findet
     *)
    WriteModSettings(ini, Settings, true);
  Finally
    ini.free;
  End;
End;

Function SaveModSettingsToFile(Const Filename: String;
  Const Settings: TModSettings): Boolean;
Var
  ini: TIniFile;
  i: Integer;
Begin
  result := false;
  If Not ForceDirectories(ExtractFileDir(Filename)) Then Begin
    log('Unable to create: ' + Filename, llError);
    exit;
  End;
  Try
    ini := TIniFile.Create(Filename);
    Try
      ini.CacheUpdates := true;
      WriteModSettings(ini, Settings, false);
      ini.EraseSection('Handicap');
      For i := 0 To Settings.Handicap.Count - 1 Do Begin
        ini.WriteString('Handicap', Settings.Handicap.Names[i], Settings.Handicap.ValueFromIndex[i]);
      End;
      ini.UpdateFile;
      result := true;
    Finally
      ini.free;
    End;
  Except
    On av: Exception Do Begin
      log('Unable to write: ' + Filename + ' ' + av.Message, llError);
    End;
  End;
End;

Function ModTestCommandToString(Value: TModTestCommand): String;
Begin
  result := '';
  Case Value Of
    mtPositiveDisease: result := 'positive disease';
    mtHitMe: result := 'hit me';
    mtGhost: result := 'ghost on / off';
    mtHurry: result := 'start hurry';
    mtStun: result := 'stun me';
    mtAllItems: result := 'all items';
    mtMaxHitPoints: result := 'max hit points';
    mtBrickWalk: result := 'walk through bricks';
    mtFlash: result := 'white screen';
    mtWhiteBomb: result := 'white bomb';
    mtSpawnItems: result := 'spawn mod items';
  End;
End;

End.

