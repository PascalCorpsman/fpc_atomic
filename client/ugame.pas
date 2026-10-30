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
Unit ugame;

{$MODE ObjFPC}{$H+}

{$I globaldefines.inc}

Interface

Uses
  Classes, SysUtils, controls, OpenGLContext, lNetComponents, lnet,
  uatomic_common, uopengl_animation, uscreens, uChunkmanager, uatomic_field,
  uatomic, uopengl_graphikengine, usounds, uloaderdialog;

Type
  TUDPPingData = Record
    Active: Boolean;
    LastTickValue: QWord;
    StartTickValue: QWord; // To measure time since "join" dialog -> This will automatically popup the "Join" dialog if no local game can be found..
    Connection: TLUDPComponent; // Die UDP Componente, mit welcher wir via Boradcast nach offenen Servern fragen
  End;

  { TSoundInfo }

  TSoundInfo = Class
  private
    fMusik: Boolean;
    fVolume: integer;
    fLastChangeTick: uint64;
    Procedure SetMusik(AValue: Boolean);
    Procedure SetVolume(AValue: Integer);
  public
    Property Volume: Integer write SetVolume;
    Property Musik: Boolean write SetMusik;
    Constructor Create(); virtual;
    Destructor Destroy(); override;
    Procedure Render;
  End;

  THurry = Record
    Enabled: Boolean;
    Texture: TGraphikItem;
    TimeStamp: qword;
  End;

  TViewPortInfo = Record
    ScreenWidth: Integer;
    ScreenHeight: Integer;
    TopLeft: Tpoint;
    Scale: Single;
  End;

  TSerialInit = Record
    //   -1 = never executed -> Create Loader Screen
    //    0 = reconnect all callbacks an start creating the screens
    //    1 = finish creating the screens, load screen Data
    //    2 = Load Powerups and Bricks animations
    //    3 = Load Atomic Fields
    //    5 = Load Atomics
    //    6 = Free Loader and switch to MainScreen
    // 1000 = Finished
    aStep: integer;
    // Es folgt die Liste aller Local Notwendigen Variablen, welche eigentlich in TGame.Initialize
    // stehen sollten, aber über mehrere Aufrufe gerettet werden müssen.
    Loader: TLoaderDialog;
    sl: TStringList;
    j: integer;
    p: String;
{$IFDEF ShowInitTime}
    t, total: UInt64;
{$ENDIF}
    TrampStatic: Integer; // Wenn das Trampolin gerade nicht "an wackelt"
    hohletex: TGraphikItem;
  End;

  { TGame }

  TGame = Class
  private
    fSerialInit: TSerialInit;
    fTramp, fConveyors, fArrows: TOpenGL_Animation; // Wird den Karten zur Verfügung gestellt
    fHurry: THurry;
    fSoundManager: TSoundManager;
    fSoundInfo: TSoundInfo;
    fLastIdleTick: QWord;
    fLastKeyDown: Array[TKeySet] Of Array[akFirstAction..akSecondAction] Of QWORD;
    fPlayerIndex: Array[TKeySet] Of integer; // Der Index in Fplayer
    fPlayerdeadTex: TGraphikItem;
    fPowerUpsTex: TPowerTexArray;
    fBombCount: integer;
    fBombs: Array[0..15 * 11 - 1] Of TBombInfo;
    fParamJoinIP: String;
    fParamJoinPort: integer;
    fWaitingForLocalServer: Boolean; // True when waiting for local server to start
    fLastConnectionAttempt: QWord; // Timestamp of last connection attempt
    fPlayingTime_s: integer;
    fPause: Boolean;
    fNeedDisconnect: Boolean; // Wenn True, dann wird ein Disconnect via Idle Handler durchgeführt (das darf nach LNet nicht im Socket Event gemacht werden da es sonst eine AV-Gibt)
    fActualField: TAtomicField;
    fAtomics: TAtomics;
    fHuntedAtomic: TAtomic; // Spielmodus Hunter: Der Sprite Satz in Gold für den Gejagten (HuntedGold)
    fHuntedAuraTex: TGraphikItem; // Spielmodus Hunter: Radialer Verlauf (weiß, Alpha nicht invertiert) für die Aura, wird im Code erzeugt
    fBackupSettings: TAtomicSettings; // Sind wir nicht der Master Spieler, dann müssen wir unsere Settings Sichern
    fInitialized: Boolean; // True, wenn Initialize erfolgreich durchgelaufen wurde
    fFields: Array Of TAtomicField;
    fScheme: TScheme;
    fModTestMode: Boolean; // True, wenn der Server den Mod Test Modus erlaubt (miModInfo), nur dann reagieren die F-Tasten
    fGhostFlickerMs: Integer; // Zeit in ms eines Flacker Taktes der Geister (miModInfo), 0 = kein Flackern
    fBrickWalkTransparency: Integer; // in %, so durchsichtig werden Spieler gezeichnet, die durch Blöcke laufen (miModInfo)
    fGameMode: TGameMode; // Der eingestellte Spielmodus (miModInfo), im Team play wird gmHunter ignoriert (siehe EffectiveGameMode)
    fHuntedGold: Boolean; // Spielmodus Hunter: Gejagten in Gold zeichnen (miModInfo)
    fHuntedAura: Boolean; // Spielmodus Hunter: Aura um den Gejagten zeichnen (miModInfo)
    fHuntedAuraSelf: Boolean; // Spielmodus Hunter: Der Gejagte sieht seine eigene Aura auch (miModInfo)
    fAiGameModes: UInt32; // Bitmaske AiMode_* der Spielmodi, die die KI des Servers kann, 0 = keine KI geladen (miModInfo)
    fRenderFrame: Int64; // Zähler der gerenderten Spiel Frames, für die Durchsichtigkeit (BrickWalkVisible)
    fFlashTimeStamp: QWord; // Mod weißer Bildschirm: Beginn des Flashs, 0 = kein Flash
    fFlashIntensity: Integer; // in %
    fFlashMs: Integer; // Dauer in ms
    fFlashFadeMs: Integer; // Übergang zurück in ms
    fShowModTestHelp: Boolean; // True, die Hilfe des Mod Test Modus (F3) wird angezeigt
    fModTestMessage: String; // Rückmeldung zum zuletzt gesendeten Test Befehl
    fModTestMessageTimeStamp: QWord; // Zeitpunkt, an dem fModTestMessage gesetzt wurde
    fOpenBordersSince: Array[Boolean] Of QWord; // Mod offene Grenzen: GetTickCount64, seit dem oben / unten (True) bzw. links / rechts (False) offen ist, 0 = zu
{$IFNDEF DebuggMode}
    fPlayer: TPlayers;
{$ENDIF}
    fUserID: Integer; // Eine Kopie unserer Userid (Kopie, weil sie in TPlayer auch ist), wird beim Erfolgreichen Verbinden zum Server gesetzt.
    fMasterUserID: Integer; // Die Master User ID (Für das SpielerSetup)
    fOwner: TOpenGLControl;
    fgameState: TGameState;
    fScreens: Array[TScreenEnum] Of TScreen;
    fActualScreen: TScreen;

    fViewPortInfo: TViewPortInfo; // Alles was man Braucht um das Spiel mit "schwarzen" Balken zu Rendern
    fOnResizeCapture: TNotifyEvent;
    FOnMouseMoveCapture: TMouseMoveEvent;
    FOnMouseDownCapture, FOnMouseupCapture: TMouseEvent;
    FOnDblClickCapture: TNotifyEvent;
    FOnKeyDownCapture, FOnKeyUpCapture: TKeyEvent;

    fChunkManager: TChunkManager;
    fUDPPingData: TUDPPingData;

    Procedure FOnResize(Sender: TObject);
    Procedure FOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure FOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure FOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure FOnDblClick(Sender: TObject);

    Procedure FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FOnSDLKeyDown(Const aKey: TAtomicKey; Const aKeyset: TKeySet);
    Procedure FOnSDLKeyUp(Const aKey: TAtomicKey; Const aKeyset: TKeySet);

    Function KeyboardkeyToAtomicKey(key: word; Const aKeyset: TKeySet): TAtomicKey;
    Procedure CheckKeyDown(Const aKey: TAtomicKey; Const aKeyset: TKeySet);
    Procedure CheckKeyUp(Const aKey: TAtomicKey; Const aKeyset: TKeySet);
    Procedure CheckModTestKey(Key: Word; Shift: TShiftState);
    Procedure ConvertKeyToControl(aKey: TAtomicKey; Const Event: TKeyEvent);

    Function LoadSchemeFromFile(Const SchemeFileName: String): Boolean;
    Procedure OnQuitGameSoundFinished(Sender: TObject);

    Procedure RenderPlayerbyInfo(Const Info: TAtomicInfo; Edge: Boolean; PlayerIndex: Integer);
    Function HunterActive(): Boolean;
    Procedure CreateHuntedAuraTexture();
    Procedure RenderHuntedAuras();
    Procedure RenderGhost(Const Info: TAtomicInfo);
    Function BrickWalkVisible(): Boolean;
    Procedure RenderOpenBorders();
    Procedure RenderFlash();
    Procedure RenderFlameThrowers();
    Procedure RenderFieldHeader();
    Procedure RenderBombs();

    Procedure Connection_Connect(aSocket: TLSocket);
    Procedure Connection_Disconnect(aSocket: TLSocket);
    Procedure Connection_Error(Const msg: String; aSocket: TLSocket);

    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
    Procedure SendChunk(UserDefinedID: Integer; Const Data: TStream);
    Procedure OnUDPConnection_Receive(aSocket: TLSocket); // Empfängt die UDP Broadcast Nachrichten der Server
    Procedure OnUDPConnection_Error(Const msg: String; aSocket: TLSocket);

    (*
     * Routinen die Der Client in OnReceivedChunk auslöst
     *)
    Procedure HandleRefreshPlayerStats(Const Stream: TMemoryStream);
    Procedure HandleUpdateAvailableFieldList(Const Stream: TMemoryStream);
    Procedure HandleUpdateFieldSetup(Const Stream: TMemoryStream);
    Procedure HandleLoadSettings(Const Stream: TMemoryStream);
    Procedure HandleStartGame();
    Procedure HandleDrawGame();
    Procedure HandleUpdateGameData(Const Stream: TMemoryStream);
    Procedure HandleSetPause(Value: Boolean);
    Procedure HandleSwitchToMatchStatistik(Const Stream: TMemoryStream);
    Procedure HandleUpdatePlayerStatistik(Const Stream: TMemoryStream);
    Procedure HandlePlaySoundEffekt(Const Stream: TMemoryStream);
    Procedure HandleUpdateMasterId(Const Stream: TMemoryStream);
    Procedure HandleHurryUp();

    (*
     * Routinen die der Master Spieler Aufruft um den Rest zu Aktualisieren
     *)
    Function StartHost: Boolean;
    Function GetServerIPAddress(): String; // Get local network IP address for server display
    Procedure SendSettings();
    Procedure UpdateFieldSetup(FieldName: String; FieldHash: Uint64; NeededWins: integer);
    Procedure SwitchToPlayerSetup;
    Procedure SwitchToFieldSetup;
    Procedure UpdatePlayerIsFirst();

    (*
     * UDP-/ TCP-Zeug Beim Verbinden
     *)
    Procedure StartPingingForGames;
    Procedure StopPingingForGames;
    Procedure Join(IP: String; Port: Integer);
    Procedure PingForOpenGames;
    Procedure DoDisconnect();
  public
{$IFDEF DebuggMode}
    fPlayer: TPlayers;
{$ENDIF}
    Settings: TAtomicSettings; // für uscreens

    OnNeedHideCursor: TNotifyEvent;
    OnNeedShowCursor: TNotifyEvent;

    Constructor Create(Const Owner: TOpenGLControl);
    Destructor Destroy; override;

    Procedure RegisterTCPConnection(Const Connection: TLTCPComponent);
    Procedure RegisterUDPConnection(Const Connection: TLUDPComponent);

    Procedure SwitchToScreen(TargetScreen: TScreenEnum);

    Function Initialize: Boolean; // Lädt alles was es so zu laden gibt (OpenGL-Technisch), wird einmalig in OnMakeCurrent Aufgerufen
    Procedure Disconnect();
    Procedure Render();

    Procedure OnIdle; // Wird von Application.OnIdle aufgerufen
    Procedure JoinViaParams(ip: String; Port: integer); // Nur 1 mal direkt nach dem Start erlaubt (siehe TForm1.OnIdle)

    (*
     * Werden und dürfen nur aus uScreens heraus aufgerufen werden
     *)
    Procedure ChangePlayerKey(PlayerIndex, Direction: Integer);
    Procedure UpdateWinsToWin(Delta: Integer);
    Procedure UpdateSelectedField(Delta: Integer);
    Procedure StartGame();
    Procedure StartPlayingSong(Filename: String);
    Procedure PlaySoundEffect(Filename: String; EndCallback: TNotifyEvent = Nil);
    Procedure Resize();
  End;

Var
  Game: TGame; // Die SpielEngine
  NeedFormClose: Boolean = false; // Set to true when user confirms exit, triggers form close after quit sound

Implementation

Uses dglopengl
  , FileUtil
  , LazUTF8
  , LazFileUtils
  , Graphics
  , Forms
  , Unit1 // Nur wegen Close, ggf via Dependency Injection besser lösen...
  , math
  , uip
  , uOpenGL_ASCII_Font
  , uvectormath
  , LCLType
  , process
  , UTF8Process
  , uatomic_messages
  , uatomicfont
  , uopengl_spriteengine
  , ugraphics
  , uatomic_global
  , usdl_input
  , uopengl_shaderprimitives
  ;

(*
 * Gibt den Index von Value in aArray zurück, -1 wenn nicht enthalten.
 *)

Function IndexOf(Value: TAtomicKey; Const aArray: Array Of TAtomicKey): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(aArray) Do Begin
    If Value = aArray[i] Then Begin
      result := i;
      break;
    End;
  End;
End;

{ TSoundInfo }

Constructor TSoundInfo.Create;
Begin
  Inherited create;
  fLastChangeTick := GetTickCount64;
End;

Destructor TSoundInfo.Destroy;
Begin

End;

Procedure TSoundInfo.SetMusik(AValue: Boolean);
Begin
  fLastChangeTick := GetTickCount64;
  fMusik := AValue;
End;

Procedure TSoundInfo.SetVolume(AValue: Integer);
Begin
  fLastChangeTick := GetTickCount64;
  fVolume := AValue;
End;

Procedure TSoundInfo.Render;
Const
  x = 600;
  w = 12;
  h = 50;
  y = 240 - h Div 2;
Var
  s: Single;
Begin
  If GetTickCount64 - fLastChangeTick > AtomicShowSoundInfoTime Then exit;
  UseColorShader;
  //glPushMatrix();
  //glTranslatef(0, 0, atomic_dialog_Layer + atomic_EPSILON);
  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_DEPTH_TEST);
  // Anzeige "Volume"
  glshaderBegin(GL_TRIANGLE_FAN);
  SetShaderColor(0, 0, 0);
  glShaderVertex(x, y, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(x, y + h, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(x + w, y + h, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(x + w, y, atomic_dialog_Layer + atomic_EPSILON);
  glShaderEnd;
  glshaderBegin(GL_TRIANGLE_FAN);
  s := 1 - fVolume / 10000;
  SetShaderColor(0, 1, 1);
  glShaderVertex(x, y + s * h, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(x, y + h, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(x + w, y + h, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(x + w, y + s * h, atomic_dialog_Layer + atomic_EPSILON);
  glShaderEnd;
  UseTextureShader;
  // Anzeige Musik
  OpenGL_ASCII_Font.Color := clWhite;
  OpenGL_ASCII_Font.Textout(320 - 32, 400, atomic_dialog_Layer + atomic_EPSILON, 'Musik ' + BoolToStr(fMusik, 'on', 'off'));
  glEnable(GL_DEPTH_TEST);
  //glPopMatrix();
End;

{ TGame }

Procedure TGame.SwitchToScreen(TargetScreen: TScreenEnum);
Var
  EnterID: Integer;
Begin
  If Not fInitialized Then exit; // So Lange wir nicht Initialisiert sind, machen wir gar nix !
  EnterID := LogEnter('TGame.SwitchToScreen');
  If (TargetScreen = sHost) Or (TargetScreen = sJoinNetwork) Then Begin
    TJoinMenu(fScreens[sJoinNetwork]).SetServerIP(''); // Reset IP in case of "join"
    fParamJoinIP := ''; // Reset last Join IP, this is needed, if player was server before, ..
    If assigned(OnNeedHideCursor) Then OnNeedHideCursor(Nil);
  End;
  (*
   * Wenn es "individuell" noch was zu tun gibt ...
   *)
  Case TargetScreen Of
    sDrawGame: Begin
        fgameState := gs_MainMenu;
      End;
    sMainScreen: Begin
        fgameState := gs_MainMenu;
        StopPingingForGames;
        fWaitingForLocalServer := false; // Stop waiting for local server when returning to main menu
        Disconnect();
        HandleSetPause(false); // Sonst bleibt die Hauptmenü Animation ggf stehen, das will natürlich keiner ;)
        If assigned(OnNeedShowCursor) Then OnNeedShowCursor(Nil);
        // Close a maybe openened join dialog ..
        TMainMenu(fScreens[sMainScreen]).StopJoinQuestion;
      End;
    sExitBomberman: Begin
        NeedFormClose := true;
        StartPlayingSong(''); // Disable the Background musik if playing, that speeds up the shutdown process or at least stop making noises
        PlaySoundEffect('data' + pathdelim + 'res' + PathDelim + 'quitgame.wav', @OnQuitGameSoundFinished);
      End;
    sHost: Begin
        fParamJoinIP := '';
        If Not StartHost Then Begin // Wenn der Server nicht gestartet werden kann -> Raus
          LogLeave(EnterID);
          exit;
        End;
        // After starting server, automatically connect to localhost
        // This is faster and more reliable than UDP broadcast
        fParamJoinIP := '127.0.0.1';
        fParamJoinPort := Settings.Port;
        log('Server started, will connect to localhost:' + inttostr(fParamJoinPort), llInfo);

        // Get and display server IP address for other players to connect
        If Assigned(fScreens[sJoinNetwork]) Then Begin
          TJoinMenu(fScreens[sJoinNetwork]).SetServerIP(GetServerIPAddress());
        End;

        TargetScreen := sJoinNetwork;
        // The client will automatically connect via JoinViaParams in sJoinNetwork handler
      End;
    sPlayerSetupRequest: Begin
        SwitchToPlayerSetup;
        // Egal wie da es diesen Screen nicht gibt wird er nicht übernommen
        LogLeave(EnterID);
        exit;
      End;
    sPlayerSetup: Begin
        // Den Screen mit den Aktuellsten Daten versorgen
        TPlayerSetupMenu(fScreens[sPlayerSetup]).TeamPlay := Settings.TeamPlay;
        TPlayerSetupMenu(fScreens[sPlayerSetup]).GameMode := fGameMode;
        TPlayerSetupMenu(fScreens[sPlayerSetup]).AiGameModes := fAiGameModes;
        TPlayerSetupMenu(fScreens[sPlayerSetup]).LoadPlayerdata(fPlayer, fUserID);
      End;
    sEditFieldSetupRequest: Begin
        SwitchToFieldSetup;
        // Egal wie da es diesen Screen nicht gibt wird er nicht übernommen
        LogLeave(EnterID);
        exit;
      End;
    sEditFieldSetup: Begin
        // Nichts
      End;
    sMatchStatistik: Begin
        fgameState := gs_MainMenu;
        TMatchStatistikMenu(fScreens[sMatchStatistik]).LoadPlayerdata(fPlayer);
      End;
    sVictory: Begin
        fgameState := gs_MainMenu;
        TVictoryMenu(fScreens[sVictory]).Victor := TMatchStatistikMenu(fScreens[sMatchStatistik]).Victor;
      End;
  End;
  // Ist hier unten, weil Host und Join das Gleichermasen machen
  If TargetScreen = sJoinNetwork Then Begin
    (*
     * Wenn der Spieler via IP-Settings gejoint hat soll er das nun auch wieder können
     *)
    If fParamJoinIP <> '' Then Begin
      JoinViaParams(fParamJoinIP, fParamJoinPort);
      LogLeave(EnterID);
      exit;
    End
    Else Begin
      If FileExists('data' + PathDelim + 'schemes' + PathDelim + Settings.SchemeFile) Then Begin
        StartPingingForGames;
      End
      Else Begin
        logshow('Error, unable to load scheme file: ' + Settings.SchemeFile + LineEnding + 'please adjust in options.', llError);
        SwitchToScreen(sMainScreen);
        LogLeave(EnterID);
        exit;
      End;
    End;
  End;
  fActualScreen := fScreens[TargetScreen];
  (*
   * Der Exit Screen ist nicht Existent, sollte es irgendwann mal einen "Byby sound geben, dann sieht das anders aus ;)
   *)
  If assigned(fActualScreen) Then Begin
    fActualScreen.reset;
  End;
  UpdatePlayerIsFirst();
  LogLeave(EnterID);
End;

Procedure TGame.ChangePlayerKey(PlayerIndex, Direction: Integer);
Var
  m: TMemoryStream;
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.ChangePlayerKey');
  m := TMemoryStream.Create;
  m.Write(PlayerIndex, SizeOf(PlayerIndex));
  m.Write(Direction, SizeOf(Direction));
  m.WriteAnsiString(Settings.NodeName);
  SendChunk(miChangePlayerKey, m);
  LogLeave(EnterID);
End;

Procedure TGame.StartPlayingSong(Filename: String);
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.StartPlayingSong');
  fSoundManager.PlaySound(Filename);
  LogLeave(EnterID);
End;

Procedure TGame.PlaySoundEffect(Filename: String; EndCallback: TNotifyEvent);
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.PlaySoundEffect');
  // Die Soundeffekte lassen sich unabhängig von der Musik (PlaySounds) abschalten, der EndCallback kommt trotzdem
  If (Not Settings.PlaySoundEffects) Or (Not fSoundManager.PlaySoundEffekt(Filename, EndCallback)) Then Begin
    If assigned(EndCallback) Then Begin
      EndCallback(Nil);
    End;
  End;
  LogLeave(EnterID);
End;

Procedure TGame.Resize;
Begin
  FOnResize(fOwner);
End;

Procedure TGame.FOnResize(Sender: TObject);
Var
  ScreenRatio, GameRatio: Single;
Begin
  fViewPortInfo.TopLeft := point(0, 0);
  If settings.Proportional Then Begin
    fViewPortInfo.ScreenWidth := TOpenGLControl(sender).Width;
    fViewPortInfo.ScreenHeight := TOpenGLControl(sender).Height;
    (*
     * Do a Proportional Scale like TImage, but from hand ;)
     *)
    Gameratio := GameWidth / GameHeight;
    ScreenRatio := fViewPortInfo.ScreenWidth / fViewPortInfo.ScreenHeight;
    If ScreenRatio > GameRatio Then Begin
      fViewPortInfo.Scale := fViewPortInfo.ScreenHeight / GameHeight;
      fViewPortInfo.TopLeft.X := trunc((fViewPortInfo.ScreenWidth - (fViewPortInfo.Scale * GameWidth)) / 2);
    End
    Else Begin
      fViewPortInfo.Scale := fViewPortInfo.ScreenWidth / GameWidth;
      fViewPortInfo.TopLeft.Y := trunc((fViewPortInfo.ScreenHeight - (fViewPortInfo.Scale * GameHeight)) / 2);
    End;
  End
  Else Begin
    fViewPortInfo.ScreenWidth := GameWidth;
    fViewPortInfo.ScreenHeight := GameHeight;
    fViewPortInfo.Scale := 1;
  End;
  If assigned(fOnResizeCapture) Then fOnResizeCapture(sender);
End;

Procedure TGame.FOnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  xx, yy: Integer;
Begin
  // Umrechnen der Echten Koords in die Screencoords
  If settings.Proportional Then Begin
    xx := x - fViewPortInfo.TopLeft.X;
    yy := y - fViewPortInfo.TopLeft.Y;
    xx := trunc(xx / fViewPortInfo.Scale);
    yy := trunc(yy / fViewPortInfo.Scale);
  End
  Else Begin
    xx := round(ConvertDimension(0, fOwner.ClientWidth, x, 0, GameWidth));
    yy := round(ConvertDimension(0, fOwner.ClientHeight, y, 0, GameHeight));
  End;
  Case fgameState Of
    gs_MainMenu: Begin
        If assigned(fActualScreen) Then Begin
          fActualScreen.OnMouseDown(Sender, button, Shift, xx, yy);
        End;
      End;
  End;
  If Assigned(FOnMouseDownCapture) Then Begin
    FOnMouseDownCapture(Sender, Button, Shift, x, y);
  End;
End;

Procedure TGame.FOnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  // TODO: Prüfen ob das gelöscht werden kann
  If Assigned(FOnMouseUpCapture) Then Begin
    FOnMouseUpCapture(sender, Button, shift, x, y);
  End;
End;

Procedure TGame.FOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
  );
Begin
  // TODO: Prüfen ob das gelöscht werden kann
  If Assigned(FOnMousemoveCapture) Then Begin
    FOnMousemoveCapture(Sender, Shift, x, y);
  End;
End;

Procedure TGame.FOnDblClick(Sender: TObject);
Begin
  // TODO: Prüfen ob das gelöscht werden kann
  If assigned(FOnDblClickCapture) Then Begin
    FOnDblClickCapture(sender);
  End;
End;

Procedure TGame.FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  aVolume: Dword;
  k: TKeySet;
Begin
  (*
   * Der Hack zum Beenden von Atomic Bomberman im Fehlerfall ;)
   *)
  If (Not fInitialized) And (Key = VK_ESCAPE) Then Begin
    fInitialized := true;
    SwitchToScreen(sExitBomberman);
    exit;
  End;
  If Not ((ssalt In Shift) And (key = VK_RETURN)) Then Begin // Sonst wird das VK_Return ggf unsinnig ausgewertet
    If (key = VK_ADD) Or (key = VK_OEM_PLUS) Then Begin
      aVolume := fSoundManager.IncVolume;
      settings.VolumeValue := aVolume;
      fBackupSettings.VolumeValue := aVolume;
      fSoundInfo.Volume := aVolume;
    End;
    If (key = VK_SUBTRACT) Or (key = VK_OEM_MINUS) Then Begin
      aVolume := fSoundManager.DecVolume();
      settings.VolumeValue := aVolume;
      fBackupSettings.VolumeValue := aVolume;
      fSoundInfo.Volume := aVolume;
    End;
    If key = VK_M Then Begin // Toggle Musik an aus
      Settings.PlaySounds := Not Settings.PlaySounds;
      fBackupSettings.PlaySounds := Settings.PlaySounds;
      fSoundInfo.Musik := Settings.PlaySounds;
      Case fgameState Of
        gs_MainMenu: Begin
            If assigned(fActualScreen) Then Begin
              fActualScreen.StartPLaySong();
            End;
          End;
        gs_Gaming: Begin
            StartPlayingSong(fActualField.Sound);
          End;
      End;
    End;
    Case fgameState Of
      gs_MainMenu: Begin
          If assigned(fActualScreen) Then Begin
            fActualScreen.OnKeyDown(Sender, Key, Shift);
          End;
        End;
      gs_Gaming: Begin
          If key = VK_ESCAPE Then Begin
{$IFNDEF Only3Player}
            If ID_YES = Application.MessageBox('Do you really want to quit?', 'Question', MB_ICONQUESTION Or MB_YESNO) Then
{$ENDIF}
              SwitchToScreen(sMainScreen);
          End;
          If key = VK_P Then Begin
            SendChunk(miTogglePause, Nil);
          End;
          If fModTestMode Then CheckModTestKey(Key, Shift);
          For k In TKeySet Do Begin
            If fPlayerIndex[k] <> -1 Then CheckKeyDown(KeyboardkeyToAtomicKey(key, k), k);
          End;
        End;
    End;
  End;
  If assigned(FOnKeyDownCapture) Then Begin
    FOnKeyDownCapture(sender, key, shift);
  End;
End;

Procedure TGame.CheckModTestKey(Key: Word; Shift: TShiftState);
Var
  m: TMemoryStream;
  Command: TModTestCommand;
  k: TKeySet;
Begin
  Case Key Of
    VK_F3: Begin
        fShowModTestHelp := Not fShowModTestHelp;
        exit;
      End;
    VK_F5: Command := mtPositiveDisease;
    VK_F6: Command := mtHitMe;
    VK_F7: Command := mtGhost;
    VK_F8: Command := mtHurry;
    VK_F9: Command := mtStun;
    VK_F4: Command := mtAllItems; // F10 ist unter Windows die Menü Taste
    VK_F11: Command := mtMaxHitPoints;
    VK_F12: Command := mtBrickWalk;
    VK_F2: Command := mtFlash;
    VK_F1: Command := mtWhiteBomb;
    VK_INSERT: Command := mtSpawnItems;
  Else
    exit;
  End;
  // Mit Shift wirkt der Test Befehl auf den 2. Spieler an diesem PC, spielt hier nur einer auf Tastatur 2, dann immer auf ihn
  k := ks0;
  If ((ssShift In Shift) Or (fPlayerIndex[ks0] = -1)) And (fPlayerIndex[ks1] <> -1) Then k := ks1;
  If fPlayerIndex[k] = -1 Then exit;
  If fUserID <> fMasterUserID Then Begin
    fModTestMessage := 'Mod test mode: only the master player';
    fModTestMessageTimeStamp := GetTickCount64;
    exit;
  End;
  m := TMemoryStream.Create;
  m.Write(Command, sizeof(Command));
  m.Write(fPlayerIndex[k], sizeof(fPlayerIndex[k]));
  SendChunk(miDebugCommand, m);
  fModTestMessage := 'Test: ' + ModTestCommandToString(Command);
  fModTestMessageTimeStamp := GetTickCount64;
End;

Procedure TGame.CheckKeyDown(Const aKey: TAtomicKey; Const aKeyset: TKeySet);
Var
  m: TMemoryStream;
  db, b: Boolean;
  ak: TAtomicKey;
  n: QWORD;
Begin
  If akey = akNone Then exit;
  If fPlayerIndex[aKeyset] = -1 Then exit;
  m := TMemoryStream.Create;
  b := true;
  db := false;
  m.Write(fPlayerIndex[aKeyset], SizeOf(fPlayerIndex[aKeyset]));
  m.Write(b, sizeof(b));
  Case IndexOf(aKey, [akLeft, akRight, akUp, akDown, akFirstAction, akSecondAction]) Of
    0: ak := akLeft;
    1: ak := akRight;
    2: ak := akUp;
    3: ak := akDown;
    4: Begin
        ak := akFirstAction;
        n := GetTickCount64;
        If fLastKeyDown[aKeyset][ak] + AtomicActionDoubleTime > n Then db := true;
        fLastKeyDown[aKeyset][ak] := n;
      End;
    5: Begin
        ak := akSecondAction;
        n := GetTickCount64;
        If fLastKeyDown[aKeyset][ak] + AtomicActionDoubleTime > n Then db := true;
        fLastKeyDown[aKeyset][ak] := n;
      End;
  End;
  fPlayer[fPlayerIndex[aKeyset]].KeysPressed[ak] := True;
  m.Write(ak, SizeOf(ak));
  m.Write(db, sizeof(db));
  SendChunk(miClientKeyEvent, m);
End;

Procedure TGame.CheckKeyUp(Const aKey: TAtomicKey; Const aKeyset: TKeySet);
Var
  m: TMemoryStream;
  ak: TAtomicKey;
  db, b: Boolean;
  j: TAtomicKey;
Begin
  If akey = akNone Then exit;
  If fPlayerIndex[aKeyset] = -1 Then exit;
  Case IndexOf(aKey, [akLeft, akRight, akUp, akDown, akFirstAction, akSecondAction]) Of
    0: ak := akLeft;
    1: ak := akRight;
    2: ak := akUp;
    3: ak := akDown;
    5: Begin
        // Mod Flammenwerfer: Der Server muss wissen, wann die zweite Aktion los gelassen wird (nur dafür, die Bewegung bleibt)
        m := TMemoryStream.Create;
        b := false;
        db := false;
        ak := akSecondAction;
        m.Write(fPlayerIndex[aKeyset], SizeOf(fPlayerIndex[aKeyset]));
        m.Write(b, sizeof(b));
        m.Write(ak, SizeOf(ak));
        m.Write(db, sizeof(db));
        SendChunk(miClientKeyEvent, m);
        exit;
      End;
  Else Begin
      // Unknown, or unused key ..
      exit;
    End;
  End;
  fPlayer[fPlayerIndex[aKeyset]].KeysPressed[ak] := false;
  // Das Problem ist, das der Server bei Key Up die Animation komplett stoppt, das führt dazu, dass der Spieler "träge" wirkt
  // Aus diesem Grund muss hier geschaut werden ob ggf. noch eine andere Taste "Aktiv" ist, wenn ja wird stattdessen derren Down gesendet !
  b := false;
  db := false;
  For j In [akUp, akDown, akLeft, akRight] Do Begin
    If fPlayer[fPlayerIndex[aKeyset]].KeysPressed[j] Then Begin
      b := true;
      ak := j;
      break;
    End;
  End;
  m := TMemoryStream.Create;
  m.Write(fPlayerIndex[aKeyset], SizeOf(fPlayerIndex[aKeyset]));
  m.Write(b, sizeof(b));
  m.Write(ak, SizeOf(ak));
  m.Write(db, sizeof(db)); // Double wird bei key Up ignoriert
  SendChunk(miClientKeyEvent, m);
End;

Procedure TGame.ConvertKeyToControl(aKey: TAtomicKey; Const Event: TKeyEvent);
Var
  K: Word;
Begin
  k := VK_UP;
  If akUp = aKey Then event(Nil, k, []);
  k := VK_DOWN;
  If akDown = aKey Then event(Nil, k, []);
  k := VK_LEFT;
  If akLeft = aKey Then event(Nil, k, []);
  k := VK_RIGHT;
  If akRight = aKey Then event(Nil, k, []);
  k := VK_RETURN;
  If akFirstAction = aKey Then event(Nil, k, []);
  k := VK_ESCAPE;
  If akSecondAction = aKey Then event(Nil, k, []);
End;

Procedure TGame.FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  k: TKeySet;
Begin
  If fgameState = gs_Gaming Then Begin
    For k In TKeySet Do Begin
      If fPlayerIndex[k] <> -1 Then CheckKeyUp(KeyboardkeyToAtomicKey(Key, k), k);
    End;
  End;
  If assigned(FOnKeyUpCapture) Then Begin
    FOnKeyUpCapture(sender, key, shift);
  End;
End;

Procedure TGame.FOnSDLKeyDown(Const aKey: TAtomicKey; Const aKeyset: TKeySet);
Begin
  writeln('SDLDown');
  If fgameState = gs_Gaming Then Begin
    CheckKeyDown(aKey, aKeyset);
  End
  Else Begin
    ConvertKeyToControl(akey, @FOnKeyDown);
  End;
End;

Procedure TGame.FOnSDLKeyUp(Const aKey: TAtomicKey; Const aKeyset: TKeySet);
Begin
  writeln('SDLUP');
  If fgameState = gs_Gaming Then Begin
    CheckKeyUp(aKey, aKeyset);
  End
  Else Begin
    ConvertKeyToControl(akey, @FOnKeyUp);
  End;
End;

Function TGame.KeyboardkeyToAtomicKey(key: word; Const aKeyset: TKeySet
  ): TAtomicKey;
Begin
  result := akNone;
  If Settings.Keys[aKeyset].UseSDL2 Then exit;
  If key = Settings.Keys[aKeyset].KeyUp Then exit(akUp);
  If key = Settings.Keys[aKeyset].KeyDown Then exit(akDown);
  If key = Settings.Keys[aKeyset].KeyLeft Then exit(akLeft);
  If key = Settings.Keys[aKeyset].KeyRight Then exit(akRight);
  If key = Settings.Keys[aKeyset].KeyPrimary Then exit(akFirstAction);
  If key = Settings.Keys[aKeyset].KeySecondary Then exit(akSecondAction);
End;

Function TGame.GetServerIPAddress: String;
Var
  adapters: TNetworkAdapterList;
  i: Integer;
Begin
  Result := '127.0.0.1'; // Default to localhost
  Try
    adapters := GetLocalIPs();
    // Find first non-localhost IP address (prefer network IP over localhost)
    For i := 0 To High(adapters) Do Begin
      If (adapters[i].IpAddress <> '127.0.0.1') And (adapters[i].IpAddress <> '') Then Begin
        Result := adapters[i].IpAddress;
        log('Server IP address: ' + Result, llInfo);
        exit;
      End;
    End;
    // If no network IP found, use localhost
    log('No network IP found, using localhost: ' + Result, llInfo);
  Except
    On E: Exception Do Begin
      log('Error getting server IP address: ' + E.Message + ', using localhost', llWarning);
      Result := '127.0.0.1';
    End;
  End;
End;

Function TGame.StartHost: Boolean;
Var
  serv: String;
  p: TProcessUTF8;
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.StartHost');
  result := false;
  // Starten des Atomic_servers, dann als Client verbinden
  serv := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'atomic_server';
{$IFDEF Windows}
  serv := serv + '.exe';
{$ENDIF}
  If FileExistsUTF8(serv) Then Begin
    p := TProcessUTF8.Create(Nil);
{$IFDEF Windows}
    // Windows needs the new console, otherwise the server could lag..
    p.Options := [poNewConsole];
{$ELSE}
    p.Options := [poDetached];
{$ENDIF}
    p.Executable := serv;
    p.Parameters.Add('-p');
    p.Parameters.Add(inttostr(settings.Port));
    p.Parameters.Add('-l');
    p.Parameters.Add(IntToStr(GetLoggerLoglevel()));
    p.Execute;
    p.free;
  End
  Else Begin
    LogShow('Error: could not find server application, abort now', llError);
    LogLeave(EnterID);
    exit;
  End;
  // Bis der Server Steht dauerts ein bischen, also warten wir
{$IFDEF Windows}
  sleep(1500); // Unter Windows scheint es deutlich "länger" zu dauern, wie lange genau wäre gut zu wissen ..
{$ELSE}
  sleep(500);
{$ENDIF}
  // StartClientGame; --> Das Verbinden macht der SwitchToScreen schon ;)
  Application.Restore;
  Application.BringToFront;
  result := true;
  LogLeave(EnterID);
End;

Procedure TGame.SwitchToPlayerSetup;
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.SwitchToPlayerSetup');
  SendChunk(miSwitchToPlayerSetup, Nil);
  LogLeave(EnterID);
End;

Procedure TGame.SwitchToFieldSetup;
Var
  i, EnterID: integer;
Begin
  EnterID := LogEnter('TGame.SwitchToMapSetup');
  (*
   * Alle Prüfungen ob die Spieler Einstellungen Richtig sind macht der Server
   * Er antwortet entweder mit ner SpashWarning oder schaltet um.
   *)

  // zuerst senden wir allen die Aktuellen Wunschdaten für das Field zu
  TFieldSetupMenu(fScreens[sEditFieldSetup]).LastWinsToWinMatch := Settings.LastWinsToWinMatch;
  TFieldSetupMenu(fScreens[sEditFieldSetup]).SchemeFile := settings.SchemeFile;

  fActualField := Nil;
  For i := 0 To high(fFields) Do Begin
    If (Not assigned(fActualField)) Then Begin
      If fFields[i].Available Then fActualField := fFields[i];
    End;
    If (fFields[i].Name = Settings.LastPlayedField) And
      (fFields[i].Hash = Settings.LastPlayedFieldHash) And
      (fFields[i].Available)
      Then Begin
      fActualField := fFields[i];
      break;
    End;
  End;
  TFieldSetupMenu(fScreens[sEditFieldSetup]).ActualField := fActualField;

  If fActualField = Nil Then Begin
    LogShow('Error, could not find a field to play on.', llError);
    LogLeave(EnterID);
    exit;
  End;

  UpdateFieldSetup(fActualField.Name, fActualField.Hash,
    TFieldSetupMenu(fScreens[sEditFieldSetup]).LastWinsToWinMatch);

  SendChunk(miSwitchToFieldSetup, Nil);
  LogLeave(EnterID);
End;

Procedure TGame.UpdatePlayerIsFirst;
Var
  ascreen: TScreenEnum;
  i: Integer;
Begin
  For ascreen In TScreenEnum Do Begin
    If assigned(fScreens[aScreen]) Then
      fScreens[aScreen].PlayerIsFirst := fUserID = fMasterUserID;
  End;
  (*
   * Der FieldSetup Dialog muss nun wieder sauber initialisiert werden
   *)
  If assigned(fScreens[sEditFieldSetup]) Then Begin
    For i := 0 To high(fPlayer) Do Begin
      If fPlayer[i].UID = fMasterUserID Then Begin
        TFieldSetupMenu(fScreens[sEditFieldSetup]).MasterPlayerName := fPlayer[i].UserName;
        break;
      End;
    End;
  End;
End;

Procedure TGame.Connection_Connect(aSocket: TLSocket);
Var
  m: TMemoryStream;
  i, EnterID: Integer;
  B: Byte;
Begin
  // Der Client ist beim Server Registriert, nun gilt es um die Mitspielerlaubniss zu fragen.
  EnterID := LogEnter('TGame.Connection_Connect');

  // Stop waiting for local server - we're connected now
  fWaitingForLocalServer := false;

  fChunkManager.SetNoDelay(true);
  fModTestMode := false; // Wird vom Server ggf. mittels miModInfo aktiviert
  fGhostFlickerMs := 0;
  fBrickWalkTransparency := 0;
  fShowModTestHelp := false;
  m := TMemoryStream.Create;
  m.Write(ProtocollVersion, sizeof(ProtocollVersion));
{$IFDEF Release}
  b := GameModeRelease;
{$ELSE}
  b := GameModeDebug;
{$ENDIF}
  m.write(b, sizeof(b));
  m.WriteAnsiString(Settings.NodeName);
  i := length(fFields);
  m.Write(i, sizeof(i));
  For i := 0 To high(fFields) Do Begin
    m.WriteAnsiString(fFields[i].Name);
    m.Write(fFields[i].Hash, sizeof(fFields[i].Hash));
  End;
  fBackupSettings := Settings; // Wir Sichern unsere Einstellungen !
  SendChunk(miRequestLogin, m);
  fUserID := -1;
  LogLeave(EnterID);
End;

Procedure TGame.Connection_Disconnect(aSocket: TLSocket);
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.Connection_Disconnect');
  // Verbindung zum Server verloren / getrennt
  Settings := fBackupSettings;
  // Im Victory Screen können wir beliebig lange bleiben, von da aus geht es eh nur noch ins Hauptmenü ;)
  If fActualScreen <> fScreens[sVictory] Then Begin
    SwitchToScreen(sMainScreen); // Wir Fliegen raus auf die Top ebene
  End;
  LogLeave(EnterID);
End;

Procedure TGame.Connection_Error(Const msg: String; aSocket: TLSocket);
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.Connection_Error');

  // If we're waiting for local server, don't show error and don't switch to main menu
  // OnIdle will retry the connection periodically
  If fWaitingForLocalServer Then Begin
    log('Connection error while waiting for local server: ' + msg + ' - will retry', llInfo);
    LogLeave(EnterID);
    exit;
  End;

  // For remote servers or if we're not waiting, show error and go back to main menu
  LogShow(msg, llError);
  SwitchToScreen(sMainScreen); // Wir Fliegen raus auf die Top ebene
  LogLeave(EnterID);
End;

Procedure TGame.OnUDPConnection_Error(Const msg: String; aSocket: TLSocket);
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.UDPConnection_Error');
  StopPingingForGames();
  Disconnect();
  LogShow(msg, llError);
  SwitchToScreen(sMainScreen); // Wir Fliegen raus auf die Top ebene
  LogLeave(EnterID);
End;

Procedure TGame.OnUDPConnection_Receive(aSocket: TLSocket);
Var
  Buffer: Array[0..1023] Of byte;
  cnt: integer;
  B: Byte;
  i: Integer;
  serverIP, ServerText: String;
  serverPort, EnterID: integer;
Begin
  EnterID := LogEnter('TGame.OnUDPConnection_Receive');
  Repeat
    cnt := aSocket.Get(buffer, 1024);
    (*
     * So wie es Aussieht scheinen noch weitere UDP Packete an zu kommen
     * selbst, wenn StopPingingForGames bereits aufgerufen wurde
     * der Get oben liest die "Puffer" leer, aber Reagieren tun wir nicht mehr drauf !
     *)
    If (cnt <> 0) And (fUDPPingData.Active) Then Begin
      b := 21; // CTD nutzt 42 wir wollen nicht kompatibel sein !
      ServerText := '';
      setlength(ServerText, cnt - 1);
      For i := 0 To cnt - 1 Do Begin
        b := b Xor buffer[i];
        If i <> cnt - 1 Then Begin
          ServerText[i + 1] := chr(buffer[i]);
        End;
      End;
      If b = 0 Then Begin //
        log('TGame.OnUDPConnection_Receive, CRC OK = ' + ServerText, llInfo);
        serverIP := aSocket.PeerAddress;
        serverPort := -1;
        For i := length(ServerText) Downto 1 Do Begin
          If ServerText[i] = ':' Then Begin
            serverPort := strtointdef(copy(ServerText, i + 1, length(ServerText)), -1);
            break;
          End;
        End;
        If serverPort = -1 Then Begin
          log('Could not resolve port informations', llWarning);
          LogLeave(EnterID);
          exit;
        End;
        (*
         * Wir wählen uns ins Spiel ein ;-)
         *)
        StopPingingForGames; // Wir wählen uns ja ein, also dürfen wir auch nicht mehr weiter pingen
        join(serverIP, serverPort);
      End;
    End;
  Until cnt = 0;
  LogLeave(EnterID);
End;

Procedure TGame.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Var
  i, EnterID: integer;
  ts: QWORD;
  s: String;
  m: TMemoryStream;
  b: Boolean;
  DoLog: Boolean;
Begin
{$IFDEF DoNotLog_CyclicMessages}
  dolog := ((Chunk.UserDefinedID And $FFFF) <> miUpdateGameData) And
    ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) And
    ((Chunk.UserDefinedID And $FFFF) <> miClientKeyEvent);
{$ELSE}
  DoLog := true;
{$ENDIF}
  EnterID := 0;
  If DoLog Then
    EnterID := LogEnter('TGame.OnReceivedChunk : ' + MessageIdentifierToString(Chunk.UserDefinedID));
  Case (Chunk.UserDefinedID And $FFFF) Of
    miUpdateMasterID: Begin
        HandleUpdateMasterId(Chunk.Data);
      End;
    miPlaySoundEffekt: Begin
        HandlePlaySoundEffekt(Chunk.Data);
      End;
    miTogglePause: Begin
        b := false;
        Chunk.Data.Read(b, SizeOf(b));
        HandleSetPause(b);
      End;
    miRequestLoginResult: Begin
        i := -1;
        Chunk.Data.Read(i, sizeof(i));
        If i = EC_No_Error Then Begin
          TJoinMenu(fScreens[sJoinNetwork]).Connected := true;
          i := -1;
          Chunk.Data.Read(i, sizeof(i));
          fUserID := i;
          i := -1;
          Chunk.Data.Read(i, sizeof(i));
          fMasterUserID := i;
          // Wir dürfen mit machen, sind wir auch der "Boss", dann müssen wir dem Server nun unsere Scheme informationen zusenden
          If fUserID = fMasterUserID Then Begin
            SendSettings();
          End;
          UpdatePlayerIsFirst(); // Das Stellt gleich alle Screens richtig ein ;)
        End
        Else Begin
          // Wir dürfen nicht mit spielen, also alles wieder zurück auf Anfang
          s := 'Unknown error.';
          Case i Of
            EC_User_already_exists: s := 'A player with this username already joined the server.';
            EC_game_full: s := 'Game already in progress. No additional connections are allowed.';
            EC_Invalid_Versions: s := 'Server and client have different versions. Please use the same version as server!';
            EC_Too_Much_Player: s := 'Only 10 player per server allowed, sorry this server is full!';
            EC_Invalid_Username: s := 'Username invalid, login not allowed.';
{$IFDEF Release}
            EC_Invalid_Mode_Versions: s := 'Server is in debug mode, while client in release mode -> not compatible.';
{$ELSE}
            EC_Invalid_Mode_Versions: s := 'Server is in release mode, while client in debug mode ->  not compatible.';
{$ENDIF}
          End;
          Disconnect();
          SwitchToScreen(sMainScreen);
          LogShow('Could not join the server : ' + s, llInfo);
        End;
      End;
    miShowHurry: Begin
        HandleHurryUp();
      End;
    miFlash: Begin
        Chunk.Data.Read(fFlashIntensity, sizeof(fFlashIntensity));
        Chunk.Data.Read(fFlashMs, sizeof(fFlashMs));
        Chunk.Data.Read(fFlashFadeMs, sizeof(fFlashFadeMs));
        // Spielen 2 an diesem PC, teilen sie sich den Bildschirm, dann gibt es keinen Flash
        If (fPlayerIndex[ks0] = -1) Or (fPlayerIndex[ks1] = -1) Then Begin
          fFlashTimeStamp := GetTickCount64;
        End;
      End;
    miModInfo: Begin
        fModTestMode := false;
        Chunk.Data.Read(fModTestMode, sizeof(fModTestMode));
        fGhostFlickerMs := 0;
        Chunk.Data.Read(fGhostFlickerMs, sizeof(fGhostFlickerMs));
        fBrickWalkTransparency := 0;
        Chunk.Data.Read(fBrickWalkTransparency, sizeof(fBrickWalkTransparency));
        // Spielmodus Hunter (Reihenfolge siehe TServer.SendModInfo)
        fGameMode := gmClassic;
        Chunk.Data.Read(fGameMode, sizeof(fGameMode));
        fHuntedGold := false;
        Chunk.Data.Read(fHuntedGold, sizeof(fHuntedGold));
        fHuntedAura := false;
        Chunk.Data.Read(fHuntedAura, sizeof(fHuntedAura));
        fHuntedAuraSelf := false;
        Chunk.Data.Read(fHuntedAuraSelf, sizeof(fHuntedAuraSelf));
        fAiGameModes := 0;
        Chunk.Data.Read(fAiGameModes, sizeof(fAiGameModes));
        // Die Mods können auch noch kommen, wenn das Player Setup schon angezeigt wird
        If assigned(fScreens[sPlayerSetup]) Then Begin
          TPlayerSetupMenu(fScreens[sPlayerSetup]).GameMode := fGameMode;
          TPlayerSetupMenu(fScreens[sPlayerSetup]).AiGameModes := fAiGameModes;
        End;
      End;
    miStartGame: Begin
        HandleStartGame();
      End;
    miDrawGame: Begin
        HandleDrawGame();
      End;
    miUpdatePlayerStatistik: Begin
        HandleUpdatePlayerStatistik(Chunk.Data);
      End;
    miShowMatchStatistik: Begin
        HandleSwitchToMatchStatistik(Chunk.data);
      End;
    miShowVictory: Begin
        SwitchToScreen(sVictory);
      End;
    miUpdateGameData: Begin
        HandleUpdateGameData(Chunk.Data);
      End;
    miHeartBeat: Begin
        ts := 0;
        Chunk.Data.Read(ts, sizeof(ts));
        m := TMemoryStream.Create;
        m.Write(fUserID, sizeof(fUserID));
        m.write(ts, sizeof(ts));
        SendChunk(miHeartBeat, m);
      End;
    miRefreshPlayerStats: Begin
        HandleRefreshPlayerStats(Chunk.Data);
      End;
    miSwitchToPlayerSetup: Begin
        SwitchToScreen(sPlayerSetup);
      End;
    miCommandoBackToMainMenu: Begin
        // Der Server Schmeißt alle raus, das macht er irgendwann, oder wenn er den Victory Screen anzeigt ;)
        // So können sich vereinzelte spieler den Victory screen noch weiter ansehen und die anderen können schon wieder
        // dem Join beitreten.
        If fActualScreen <> fScreens[sVictory] Then Begin
          SwitchToScreen(sMainScreen);
        End;
        Disconnect(); // Damit das klappt, muss der Spieler aber auf jeden Fall disconnecten.
      End;
    miSplashHint: Begin
        s := Chunk.Data.ReadAnsiString;
        LogShow(s, llInfo);
      End;
    miSwitchToFieldSetup: Begin
        SwitchToScreen(sEditFieldSetup);
      End;
    miAvailableFieldList: Begin
        HandleUpdateAvailableFieldList(Chunk.Data);
      End;
    miUpdateFieldSetup: Begin
        HandleUpdateFieldSetup(CHunk.Data);
      End;
    miUpdateSettings: Begin
        HandleLoadSettings(Chunk.Data)
      End;
  Else Begin
      log('Unknown user defined id : ' + inttostr((Chunk.UserDefinedID And $FFFF)), llError);
    End;
  End;
  If DoLog Then
    LogLeave(EnterID);
End;

Procedure TGame.SendChunk(UserDefinedID: Integer; Const Data: TStream);
Var
  DoLog: Boolean;
  EnterID: Integer;
Begin
{$IFDEF DoNotLog_CyclicMessages}
  DoLog := ((UserDefinedID And $FFFF) <> miHeartBeat);
{$ELSE}
  DoLog := true;
{$ENDIF}
  EnterID := 0;
  If DoLog Then
    EnterID := LogEnter('Tctd.SendChunk : ' + MessageIdentifierToString(UserDefinedID));
  If Not fChunkManager.SendChunk(UserDefinedID, data) Then Begin
    log('Could not send.', llCritical);
  End;
  If DoLog Then
    LogLeave(EnterID);
End;

Procedure TGame.HandleRefreshPlayerStats(Const Stream: TMemoryStream);
Var
  cnt, i, uid, j, EnterID: integer;
  found: Boolean;
  k: TKeySet;
Begin
  (*
   * Das muss immer 1:1 zu TServer.RefreshAllPlayerStats sein !
   *)
  EnterID := LogEnter('TGame.HandleRefreshPlayerStats');
  cnt := -1;
  stream.Read(cnt, sizeof(cnt));
  If cnt = -1 Then Begin
    log('Error, invalid playerstats', llError);
    LogLeave(EnterID);
    exit;
  End;
  found := false;
  For i := 0 To high(fPlayer) Do Begin
    uid := -1;
    stream.Read(uid, SizeOf(uid));
    If uid = fUserID Then found := true;
    fPlayer[i].UID := uid;
    k := ks0;
    stream.Read(k, sizeof(k));
    fPlayer[i].Keyboard := k;
    j := -1;
    stream.Read(j, SizeOf(j));
    fPlayer[i].Kills := j;
    j := -1;
    stream.Read(j, SizeOf(j));
    fPlayer[i].Score := j;
    fPlayer[i].UserName := Stream.ReadAnsiString;
  End;
  If Not found Then Begin
    // Während des Spieler Setups ist es möglich dass der Spieler kurzzeitig nicht in den Players steht !
    If fActualScreen <> fScreens[sPlayerSetup] Then Begin
      LogShow('Error user not known anymore to server, exiting actual session', llError);
      SwitchToScreen(sMainScreen);
      LogLeave(EnterID);
      exit;
    End;
  End;
  // Für den Join Network Screen
  If fActualScreen = fScreens[sJoinNetwork] Then Begin
    TJoinMenu(fScreens[sJoinNetwork]).LoadPlayerdata(fPlayer);
  End;
  // Wird während des Editierens des Screens eine Spielerposition verändert muss das so angezeigt werden ..
  If fActualScreen = fScreens[sPlayerSetup] Then Begin
    TPlayerSetupMenu(fScreens[sPlayerSetup]).LoadPlayerdata(fPlayer, fUserID);
  End;
  LogLeave(EnterID);
End;

Procedure TGame.SendSettings;
Var
  m: TMemoryStream;
  Mods: TModSettings;
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.SendSettings');
  If Not LoadSchemeFromFile(Settings.SchemeFile) Then Begin
    LogShow('Could not load:' + Settings.SchemeFile, llFatal);
    SwitchToScreen(sMainScreen); // Wir schmeißen uns wieder Raus, da hier offensichtlich was nicht geklappt hat
    LogLeave(EnterID);
    exit;
  End;
  m := TMemoryStream.Create;
  m.write(Settings.TeamPlay, sizeof(Settings.TeamPlay));
  m.write(Settings.RandomStart, sizeof(Settings.RandomStart));
  // Nodename hat der Server nicht
  m.write(Settings.ConveyorSpeed, sizeof(Settings.ConveyorSpeed));
  SchemeToStream(m, fScheme);
  m.write(Settings.PlayTime, sizeof(Settings.PlayTime));
  m.write(Settings.LostPlayersRevertToAI, sizeof(Settings.LostPlayersRevertToAI));
  // Playsounds hat der Server nicht
  // Keyboard hat der Server nicht
  m.WriteAnsiString(Settings.LastPlayedField);
  m.write(Settings.LastPlayedFieldHash, sizeof(Settings.LastPlayedFieldHash));
  m.write(Settings.LastWinsToWinMatch, sizeof(Settings.LastWinsToWinMatch));
  // Port hat der Server nicht
  // CheckForUpdates hat der Server nicht
  SendChunk(miUpdateSettings, m);
  // Die Mods bestimmt ebenfalls der Master, auch auf einem fremden Server
  Mods.Handicap := Nil;
  LoadModSettingsFromFile(GetAtomicModsIniFile(), Mods);
  m := TMemoryStream.Create;
  m.WriteAnsiString(ModSettingsToString(Mods));
  Mods.Handicap.free;
  SendChunk(miUpdateModSettings, m);
  (*
   * Da der Server das nicht mehr zu uns zurück schickt muss der passende Screen dazu hier "händisch" gesetzt werden.
   *)
  TPlayerSetupMenu(fscreens[sPlayerSetup]).LoadScheme(fScheme);
  LogLeave(EnterID);
End;

Procedure TGame.HandleUpdateAvailableFieldList(Const Stream: TMemoryStream);
Var
  i, EnterID: Integer;
  ahash: uint64;
  s: String;
Begin
  EnterID := LogEnter('TGame.HandleUpdateAvailableFieldList');
  For i := 0 To high(fFields) Do Begin
    fFields[i].Available := false;
  End;
  // Wir wissen nicht wie viele das sind ..
  While stream.Position < stream.Size Do Begin
    s := stream.ReadAnsiString;
    ahash := 0;
    stream.Read(ahash, sizeof(ahash));
    For i := 0 To high(fFields) Do Begin
      If (fFields[i].Name = s) And
        (fFields[i].Hash = ahash) Then Begin
        fFields[i].Available := true;
        break;
      End;
    End;
  End;
  // Prüfen ob wir wenigstens noch eine Verfügbare Karte haben
  For i := 0 To high(fFields) - 1 Do Begin // Das Letzte Field ist das Random Field, das zählt nicht
    If fFields[i].Available Then Begin
      LogLeave(EnterID);
      exit;
    End;
  End;
  // Wenn wir bis hier her kamen, dann gibt es keine Verfügbaren Karten mehr
  LogShow('The cutset of available fields over all player is zero, at least one player has to leafe the game to be able to play.', llError);
  LogLeave(EnterID);
End;

Procedure TGame.HandleUpdateFieldSetup(Const Stream: TMemoryStream);
Var
  FieldName: String;
  FieldHash: UInt64;
  Wins, i, EnterID: INteger;
Begin
  EnterID := LogEnter('TGame.HandleUpdateFieldSetup');
  FieldName := Stream.ReadAnsiString;
  FieldHash := 0;
  Wins := 0;
  fActualField := Nil;
  stream.Read(FieldHash, SizeOf(FieldHash));
  stream.Read(Wins, SizeOf(Wins));
  For i := 0 To high(fFields) Do Begin
    If (fFields[i].Name = FieldName)
      And (fFields[i].Hash = FieldHash) Then Begin
      fActualField := fFields[i];
      TFieldSetupMenu(fScreens[sEditFieldSetup]).ActualField := fActualField;
      TFieldSetupMenu(fScreens[sEditFieldSetup]).LastWinsToWinMatch := wins;
      Settings.LastWinsToWinMatch := wins;
      LogLeave(EnterID);
      exit;
    End;
  End;
  // Der Spieler fliegt raus, er kann offensichtlich nicht mit spielen
  // Eigentlich darf dieser Code hier aber nie kommen, weil der Master
  // Nicht weiterschalten kann wenn nicht alle Spieler die Karten haben ..
  LogShow('Error, could not find: ' + FieldName, llFatal);
  SwitchToScreen(sMainScreen);
  LogLeave(EnterID);
End;

Procedure TGame.HandleLoadSettings(Const Stream: TMemoryStream);
Var
  EnterID: integer;
Begin
  EnterID := LogEnter('TGame.HandleSettingsFromStream');
  Stream.Read(Settings.TeamPlay, SizeOf(Settings.TeamPlay));
  Stream.Read(Settings.RandomStart, SizeOf(Settings.RandomStart));
  // Nodename hat der Server nicht
  Stream.Read(Settings.ConveyorSpeed, SizeOf(Settings.ConveyorSpeed));
  If SchemeFromStream(Stream, fScheme) Then Begin
    TPlayerSetupMenu(fScreens[sPlayerSetup]).LoadScheme(fScheme);
    TFieldSetupMenu(fScreens[sEditFieldSetup]).SchemeFile := fScheme.Filename;
  End
  Else Begin
    logShow('Error, could not extract scheme send from server', llCritical);
    SwitchToScreen(sMainScreen);
    logleave(EnterID);
    exit;
  End;
  Stream.Read(Settings.PlayTime, sizeof(Settings.PlayTime));
  Stream.Read(Settings.LostPlayersRevertToAI, sizeof(Settings.LostPlayersRevertToAI));
  // Playsounds hat der Server nicht
  // Keyboard hat der Server nicht
  Settings.LastPlayedField := Stream.ReadAnsiString();
  Stream.Read(Settings.LastPlayedFieldHash, sizeof(Settings.LastPlayedFieldHash));
  Stream.Read(Settings.LastWinsToWinMatch, sizeof(Settings.LastWinsToWinMatch));
  // Port hat der Server nicht
  // CheckForUpdates hat der Server nicht
  logleave(EnterID);
End;

Procedure TGame.HandleStartGame;
Var
  j: TAtomicKey;
  i: Integer;
  k: TKeySet;
Begin
  fHurry.Enabled := false;
  fFlashTimeStamp := 0;
  fOpenBordersSince[true] := 0; // Die Pfeile der offenen Grenzen stehen in jeder Runde wieder OpenBordersArrowTime
  fOpenBordersSince[false] := 0;
  For k In TKeySet Do Begin
    fPlayerIndex[k] := -1;
    fLastKeyDown[k][akFirstAction] := 0;
    fLastKeyDown[k][akSecondAction] := 0;
  End;
  For i := 0 To high(fPlayer) Do Begin
    (*
     * Löschen der ggf. vorherigen Die Animation, sonst beginnt der Spieler das Spiel erst mal sterbend...
     *
     * siehe hierzu: TGame.HandleUpdateGameData
     *)
    fPlayer[i].Info.Animation := raStandStill;
    If fPlayer[i].UID = fUserID Then Begin
      fPlayerIndex[fPlayer[i].Keyboard] := i;
    End;
    For j In TAtomicKey Do Begin
      fPlayer[i].KeysPressed[j] := false;
    End;
  End;
  fActualField.Reset;
  // TODO: What ever da noch alles so fehlt
  StartPlayingSong(fActualField.Sound);
  HandleSetPause(false); // Pause auf jeden Fall, aus
  fPlayingTime_s := 0;
  fgameState := gs_Gaming;
End;

Procedure TGame.HandleDrawGame;
Begin
  SwitchToScreen(sDrawGame);
End;

Procedure TGame.HandleUpdateGameData(Const Stream: TMemoryStream);
Var
  i: Integer;
  OldValue: UInt16; // Bei Die, Zen, Locked in braucht es ebenfalls den Index
  OldAnim: TRenderAnimation;
Begin
  stream.Read(fPlayingTime_s, sizeof(fPlayingTime_s));
  For i := 0 To high(fPlayer) Do Begin
    OldValue := fPlayer[i].Info.Value;
    OldAnim := fPlayer[i].Info.Animation;
    Stream.Read(fPlayer[i].Info, sizeof(fPlayer[i].Info));
    (*
     * Der Server sendet die OneTimeAnimations genau 1 mal
     * Wenn der Server aber schneller neu Sendet als der CLient Rendert
     * dann würde der Client diese Flanke beim Rendern gar nicht berücksichtigen
     * -> Hier ein Or und im Rendern dann das False !
     *)
    fPlayer[i].Edge := (fPlayer[i].Info.Animation In OneTimeAnimations) And (OldAnim <> fPlayer[i].Info.Animation);
    (*
     * Wenn eine neue Animation kommt, stellen wir hier Sicher, dass die neue die alte
     * überschreiben kann z.B. "Zen" -> Die
     * Die Zen Animation, kann zusätzlich aber auch vom Laufen unterbrochen werden, da
     * diese ja nur kommt wenn der Spieler AtomicZenTime lang keine Eingaben gemacht hat.
     *)
    If (fPlayer[i].Edge) Or (((OldAnim = raZen) Or (oldAnim = raLockedIn)) And (fPlayer[i].Info.Animation = raWalk)) Then Begin
      OldAnim := raStandStill; // -- Egal es soll ja nur die OldAnim In OneTimeAnimations verhindert werden
    End;
    (*
     * Wenn die Alte Animation eine einmal ablaufen Animation ist.
     * Ein Geist (Mod) beginnt ohne die Todesanimation, sonst würde diese hier nochmal abgespielt.
     *)
    If (OldAnim In OneTimeAnimations) And (Not fPlayer[i].Info.Ghost) Then Begin
      If fPlayer[i].Info.Counter < fAtomics[0].GetAnimTimeInMs(OldAnim, OldValue) Then Begin
        fPlayer[i].Info.Animation := OldAnim;
        fPlayer[i].Info.Value := OldValue;
      End;
    End;
  End;
  fActualField.ReadGameingData(Stream);
  (* Wir Ziehen uns nun die Bomben Informationen raus ;-) *)
  fBombCount := 0;
  stream.Read(fBombCount, sizeof(fBombCount));
  For i := 0 To fBombCount - 1 Do Begin
    stream.Read(fBombs[i].ColorIndex, SizeOf(fBombs[i].ColorIndex));
    stream.Read(fBombs[i].Position, SizeOf(fBombs[i].Position));
    stream.Read(fBombs[i].Animation, SizeOf(fBombs[i].Animation));
    stream.Read(fBombs[i].AnimationOffset, SizeOf(fBombs[i].AnimationOffset));
    stream.Read(fBombs[i].WhiteBomb, SizeOf(fBombs[i].WhiteBomb));
  End;
End;

Procedure TGame.HandleSetPause(Value: Boolean);
Var
  ak: TAtomicKey;
  k: TKeySet;
Begin
  fPause := Value;
  OpenGL_SpriteEngine.enabled := Not value; // Global alle Animationen anhalten
  (*
   * Ende der Pause
   *)
  If Not fPause Then Begin // TODO: Irgendwas stimmt mit der Pause und den Tasten noch nicht ...
    (*
     * Egal was während der Pause war, nun ist es wieder "entdrückt"
     *)
    For ak In TAtomicKey Do Begin
      For k In TKeySet Do Begin
        If fPlayerIndex[k] <> -1 Then fPlayer[fPlayerIndex[k]].KeysPressed[ak] := false;
      End;
    End;
  End;
End;

Procedure TGame.HandleSwitchToMatchStatistik(Const Stream: TMemoryStream);
Var
  v: TVictor;
Begin
  v := vRedTeam;
  Stream.Read(v, SizeOf(v));
  TMatchStatistikMenu(fScreens[sMatchStatistik]).Victor := v;
  SwitchToScreen(sMatchStatistik);
End;

Procedure TGame.HandleUpdatePlayerStatistik(Const Stream: TMemoryStream);
Var
  i, EnterID: integer;
Begin
  EnterID := LogEnter('TGame.HandleUpdatePlayerStatistik');
  For i := 0 To high(fPLayer) Do Begin
    Stream.Read(fPLayer[i].Score, sizeof(fPLayer[i].Score));
    Stream.Read(fPLayer[i].Kills, sizeof(fPLayer[i].Kills));
  End;
  LogLeave(EnterID);
End;

Procedure TGame.HandlePlaySoundEffekt(Const Stream: TMemoryStream);
Var
  se: TSoundEffect;
  s: String;
Begin
  se := seNone;
  stream.Read(se, SizeOf(TSoundEffect));
  s := '';
  Case se Of
    seNone: Begin // Nix
      End;
    seBombDrop: s := SelectRandomSound(BombDrops);
    seBombKick: s := SelectRandomSound(BombKick);
    seBombStop: s := SelectRandomSound(BombStop);
    seBombJelly: s := SelectRandomSound(BombJelly);
    seBombBounce: s := SelectRandomSound(BombBounce);
    seBombGrab: s := SelectRandomSound(BombGrab);
    seBombPunch: s := SelectRandomSound(BombPunch);
    seBombExplode: s := SelectRandomSound(BombExplode);
    seAtomicDie: s := SelectRandomSound(AtomicDie);
    seWinner: s := SelectRandomSound(Winner);
    seGetGoodPowerUp: s := SelectRandomSound(GetGoodPowerUp);
    seGetBadPowerUp: s := SelectRandomSound(GetBadPowerUp);
    seZen: s := SelectRandomSound(AtomicZen);
    seOtherPlayerDied: s := SelectRandomSound(OtherPlayerDie);
    seHurryBrick: s := SelectRandomSound(HurryBrick);
    seHurry: s := SelectRandomSound(Hurry);
    seWrapHohle: s := SelectRandomSound(AtomicWrapHole);
    seTrampoline: s := SelectRandomSound(AtomicJump);
  End;
  If s <> '' Then s := 'data' + PathDelim + 'sounds' + PathDelim + s;
  PlaySoundEffect(s);
End;

Procedure TGame.HandleUpdateMasterId(Const Stream: TMemoryStream);
Var
  i: integer;
Begin
  i := -1;
  Stream.Read(i, SizeOf(i));
  fMasterUserID := i;
  UpdatePlayerIsFirst();
End;

Procedure TGame.HandleHurryUp;
Var
  m: TMemoryStream;
  se: TSoundEffect;
Begin
  m := TMemoryStream.Create;
  se := seHurry;
  m.write(se, sizeof(se));
  m.Position := 0;
  HandlePlaySoundEffekt(m);
  m.free;
  fHurry.Enabled := true;
  fHurry.TimeStamp := GetTickCount64;
End;

Procedure TGame.UpdateFieldSetup(FieldName: String; FieldHash: Uint64;
  NeededWins: integer);
Var
  m: TMemoryStream;
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.HandleUpdateFieldSetup');
  Settings.LastPlayedField := FieldName;
  Settings.LastPlayedFieldHash := FieldHash;
  Settings.LastWinsToWinMatch := NeededWins;
  m := TMemoryStream.Create;
  m.WriteAnsiString(FieldName);
  m.write(FieldHash, sizeof(FieldHash));
  m.Write(NeededWins, SizeOf(NeededWins));
  SendChunk(miUpdateFieldSetup, m);
  LogLeave(EnterID);
End;

Procedure TGame.UpdateWinsToWin(Delta: Integer);
Var
  PrevVal, EnterID: integer;
Begin
  EnterID := LogEnter('TGame.UpdateWinsToWin');
  PrevVal := Settings.LastWinsToWinMatch;
  Settings.LastWinsToWinMatch := max(1, Settings.LastWinsToWinMatch + Delta);
  fBackupSettings.LastWinsToWinMatch := Settings.LastWinsToWinMatch;
  If PrevVal <> Settings.LastWinsToWinMatch Then Begin
    TFieldSetupMenu(fScreens[sEditFieldSetup]).LastWinsToWinMatch := Settings.LastWinsToWinMatch;
    UpdateFieldSetup(Settings.LastPlayedField, Settings.LastPlayedFieldHash, Settings.LastWinsToWinMatch);
  End;
  LogLeave(EnterID);
End;

Procedure TGame.UpdateSelectedField(Delta: Integer);
Var
  i, index, OldIndex, EnterID: integer;
Begin
  EnterID := LogEnter('TGame.UpdateSelectedField');
  // 1. Den Aktuellen Index Raus Kriegen
  index := -1;
  For i := 0 To high(fFields) Do Begin
    If (fFields[i].Name = TFieldSetupMenu(fScreens[sEditFieldSetup]).ActualField.Name) And
      (fFields[i].Hash = TFieldSetupMenu(fScreens[sEditFieldSetup]).ActualField.Hash) Then Begin
      index := i;
      break;
    End;
  End;
  If index = -1 Then Begin
    Raise Exception.Create('TGame.UpdateSelectedField: BAM');
  End;
  OldIndex := index;
  // 2. So Lange in Delta Richtung weiter Zählen, bis wieder ein Available kommt
  Repeat
    index := (index + Delta + length(fFields)) Mod length(fFields);
  Until (fFields[index].Available);
  // 3. Alle Updaten
  If OldIndex <> index Then Begin
    fActualField := fFields[index];
    TFieldSetupMenu(fScreens[sEditFieldSetup]).ActualField := fActualField;
    settings.LastPlayedField := fFields[index].Name;
    settings.LastPlayedFieldHash := fFields[index].Hash;
    fBackupSettings.LastPlayedField := fFields[index].Name;
    fBackupSettings.LastPlayedFieldHash := fFields[index].Hash;
    UpdateFieldSetup(Settings.LastPlayedField, Settings.LastPlayedFieldHash, Settings.LastWinsToWinMatch);
  End;
  LogLeave(EnterID);
End;

Function TGame.LoadSchemeFromFile(Const SchemeFileName: String): Boolean;
(*
 * -R, 0,:#.::::::::::::::: -> Reihe 0
 *)
  Procedure EvalBrickRow(Data: String);
  Var
    sa: TStringArray;
    row: LongInt;
    i: Integer;
  Begin
    sa := data.Split(',');
    If length(sa) <> 3 Then Raise exception.create('Error, invalid brickrow: ' + data);
    If length(sa[2]) <> 15 Then Raise exception.create('Error, invalid brickrow: ' + data);
    row := strtointdef(sa[1], -1);
    If (row < 0) Or (row > 10) Then Raise exception.create('Error, invalid brickrow: ' + data);
    For i := 0 To 14 Do Begin
      Case sa[2][i + 1] Of
        '#': fScheme.BrickData[i, row] := bdSolid;
        ':': fScheme.BrickData[i, row] := bdBrick;
        '.': fScheme.BrickData[i, row] := bdBlank;
      Else Begin
          Raise exception.create('Error, invalid brickrow: ' + data);
        End;
      End;
    End;
  End;

  (*
   * -S,index,x,y[,team]
   * -S,1,14,10,1
   *)
  Procedure EvalPlayerPos(Data: String);
  Var
    sa: TStringArray;
    pPos: LongInt;
  Begin
    sa := data.Split(',');
    If length(sa) < 4 Then Raise exception.create('Error, invalid player starting position: ' + data);
    pPos := strtoint(sa[1]);
    If (pPos >= 0) And (pPos <= high(fScheme.PlayerStartPositions)) Then Begin
      fScheme.PlayerStartPositions[pPos].x := strtoint(sa[2]);
      fScheme.PlayerStartPositions[pPos].y := strtoint(sa[3]);
      If length(sa) >= 5 Then Begin
        fScheme.PlayerStartPositions[pPos].Team := strtoint(sa[4]);
      End;
    End;
  End;

  Procedure EvalPowerUp(Data: String);
    Function inttoPowerUps(Value: integer): TPowerUps;
    Begin
      Case value Of
        (*
         * Ja hier gibt es nicht alle, weil nicht alle einstellbar sind.
         *)
        0: result := puExtraBomb;
        1: result := puLongerFlameLength;
        2: result := puDisease;
        3: result := puCanCick;
        4: result := puExtraSpeed;
        5: result := puCanPunch;
        6: result := puCanGrab;
        7: result := puCanSpooger;
        8: result := puGoldFlame;
        9: result := puTrigger;
        10: result := puCanJelly;
        11: result := puSuperBadDisease;
        12: result := purandom;
      Else Begin
          Raise exception.create('Error, invalid value for powerup.');
        End;
      End;
    End;

  Var
    sa: TStringArray;
    pPos: LongInt;
    pp: TPowerUps;
  Begin
    sa := data.Split(',');
    If length(sa) < 6 Then Raise exception.create('Error, invalid powerup seting: ' + data);
    pPos := strtoint(sa[1]);
    pp := inttoPowerUps(pPos);
    fScheme.PowerUps[pp].Bornwith := strtoint(sa[2]);
    fScheme.PowerUps[pp].HasOverride := odd(strtoint(sa[3]));
    fScheme.PowerUps[pp].OverrideValue := strtoint(sa[4]);
    fScheme.PowerUps[pp].forbidden := odd(strtoint(sa[5]));
    // sa[6] ist nur ein Kommentar und wird ignoriert
  End;

Var
  fn, s: String;
  sl: TStringList;
  i, EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.LoadScheme: ' + SchemeFileName);
  result := false;
  fn := 'data' + PathDelim + 'schemes' + PathDelim + SchemeFileName;
  If Not FileExistsUTF8(fn) Then Begin
    logshow('Error, could not find scheme file:' + SchemeFileName, llCritical);
    LogLeave(EnterID);
    exit;
  End;
  fScheme := GetDefaultScheme();
  sl := TStringList.Create;
  sl.LoadFromFile(fn);
  fScheme.Filename := ExtractFileName(fn);
  Try
    For i := 0 To sl.Count - 1 Do Begin
      s := trim(sl[i]);
      If (s = '') Or (s[1] = ';') Then Continue;
      Case s[2] Of
        'V': nop(); // Nichts
        'N': fScheme.Name := copy(s, 4, length(s));
        'B': fScheme.BrickDensity := strtoint(copy(s, 4, length(s)));
        'R': EvalBrickRow(s);
        'S': EvalPlayerPos(s);
        'P': EvalPowerUp(s);
      Else Begin
          Raise exception.create('Error, unknown descriptor: "' + s[2] + '"');
        End;
      End;
    End;
  Except
    On av: Exception Do Begin
      logshow('Error, invalid scheme file: ' + SchemeFileName + LineEnding + av.Message, llCritical);
      LogLeave(EnterID);
      exit;
    End;
  End;
  sl.free;
  result := true;
  LogLeave(EnterID);
End;

Procedure TGame.OnQuitGameSoundFinished(Sender: TObject);
Begin
  If NeedFormClose Then Begin
    form1.Close;
  End;
End;

Procedure TGame.RenderPlayerbyInfo(Const Info: TAtomicInfo; Edge: Boolean; PlayerIndex: Integer);
Begin
  (*
   * Spielmodus Hunter: Der Gejagte wird in Gold gezeichnet, aber nur wenn ColorIndex seine echte Farbe ist
   * (= der Slot Index, siehe Rundenstart im Server). Sonst blinkt er gerade (Stun / Unverwundbarkeit) und das
   * Blinken soll sichtbar bleiben. Im Team play ändern sich die Farben, da gibt es aber keinen Hunter.
   *)
  If Info.Hunted And fHuntedGold And (Info.ColorIndex = PlayerIndex) And assigned(fHuntedAtomic) Then Begin
    fHuntedAtomic.Render(Info, Edge);
  End
  Else Begin
    fAtomics[info.ColorIndex].Render(Info, Edge);
  End;
End;

Function TGame.HunterActive: Boolean;
Begin
  // Wie EffectiveGameMode: Im Team play wird Hunter ignoriert
  result := (fGameMode = gmHunter) And (Not Settings.TeamPlay);
End;

Procedure TGame.CreateHuntedAuraTexture;
Const
  AuraSize = 64; // Kantenlänge der Textur in Pixel (Zweierpotenz)
Var
  Data: Array Of Array[0..3] Of Byte;
  i, j, c: Integer;
  d, a: Single;
  tex: GLuint;
Begin
  (*
   * Spielmodus Hunter: Ein radialer Verlauf weiß -> durchsichtig für die Aura.
   * Die Textur wird direkt erzeugt (und nicht über die Graphik Engine geladen), weil deren Texturen ein
   * invertiertes Alpha haben, damit ließe sich die Aura über uColor nicht stufenlos ausblenden.
   * Hier ist das Alpha normal, gezeichnet wird mit glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA) (siehe RenderHuntedAuras).
   * Die Textur lebt so lange wie der OpenGL Kontext (wie die Texturen der Graphik Engine).
   *)
  Data := Nil;
  setlength(Data, AuraSize * AuraSize);
  c := 0;
  For j := 0 To AuraSize - 1 Do Begin
    For i := 0 To AuraSize - 1 Do Begin
      d := sqrt(sqr(i + 0.5 - AuraSize / 2) + sqr(j + 0.5 - AuraSize / 2)) / (AuraSize / 2); // 0 = Mitte, 1 = Rand
      a := max(0, 1 - d);
      a := a * a * (3 - 2 * a); // Weicher Rand (Smoothstep)
      Data[c, 0] := 255;
      Data[c, 1] := 255;
      Data[c, 2] := 255;
      Data[c, 3] := round(255 * a);
      inc(c);
    End;
  End;
  tex := 0;
  glGenTextures(1, @tex);
  glBindTexture(GL_TEXTURE_2D, tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, AuraSize, AuraSize, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Data[0]);
  glBindTexture(GL_TEXTURE_2D, 0);
  fHuntedAuraTex := Default(TGraphikItem);
  fHuntedAuraTex.Image := tex;
  fHuntedAuraTex.Name := 'HuntedAura';
  fHuntedAuraTex.IsAlphaImage := true;
  fHuntedAuraTex.Stretched := smNone;
  fHuntedAuraTex.OrigWidth := AuraSize;
  fHuntedAuraTex.OrigHeight := AuraSize;
  fHuntedAuraTex.StretchedWidth := AuraSize;
  fHuntedAuraTex.StretchedHeight := AuraSize;
End;

Procedure TGame.RenderHuntedAuras;
Const
  AuraTiles = 1.6; // Durchmesser der Aura in Kacheln
  AuraPeriodMs = 800; // Dauer eines Puls Taktes
  AuraAlphaMin = 0.35;
  AuraAlphaMax = 0.7;
Var
  i: Integer;
  TwoLocal: Boolean;
  w, h, x, y, a: Single;
  c: TVector4;
Begin
  (*
   * Spielmodus Hunter: Eine pulsierende, halbtransparente Aura in Gold unter dem Gejagten.
   * Wird nach dem Feld und vor den Bomben / Spielern gezeichnet und schreibt nicht in den Tiefenpuffer,
   * dadurch liegt alles was danach kommt darüber.
   *)
  If (Not fHuntedAura) Or (fHuntedAuraTex.Image = 0) Then exit;
  // Spielen 2 an diesem PC, dann sieht der Jäger am selben Bildschirm die Aura auch -> immer zeichnen
  TwoLocal := (fPlayerIndex[ks0] <> -1) And (fPlayerIndex[ks1] <> -1);
  w := AuraTiles * FieldBlockWidth;
  h := AuraTiles * FieldBlockHeight;
  // Sinus Puls zwischen AuraAlphaMin und AuraAlphaMax
  a := AuraAlphaMin + (AuraAlphaMax - AuraAlphaMin) * (0.5 + 0.5 * sin(2 * pi * (GetTickCount64 Mod AuraPeriodMs) / AuraPeriodMs));
  c.x := HuntedGoldColor.r / 100;
  c.y := HuntedGoldColor.g / 100;
  c.z := HuntedGoldColor.b / 100;
  c.w := a;
  // Normales Alpha Blending wie bei RenderFlash (die Textur hat ein nicht invertiertes Alpha)
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDepthMask(GL_FALSE);
  UseTextureShader(c);
  For i := 0 To high(fPlayer) Do Begin
    If (Not fPlayer[i].Info.Alive) Or fPlayer[i].Info.Dying Or (Not fPlayer[i].Info.Hunted) Then Continue;
    // Der Gejagte selbst sieht seine Aura nur bei HuntedAuraSelf
    If (Not fHuntedAuraSelf) And (Not TwoLocal) And
      ((i = fPlayerIndex[ks0]) Or (i = fPlayerIndex[ks1])) Then Continue;
    // Mittelpunkt = da wo TAtomic.Render den Schatten (die Füße) zeichnet
    x := fPlayer[i].Info.Position.x * FieldBlockWidth + FieldxOff;
    y := (fPlayer[i].Info.Position.y + 0.25) * FieldBlockHeight + FieldyOff;
    RenderQuad(x - w / 2, y - h / 2, atomic_Map_Layer + 3 * atomic_EPSILON, w, h, fHuntedAuraTex);
  End;
  UseTextureShader();
  glDepthMask(GL_TRUE);
  glDisable(GL_BLEND);
End;

Procedure TGame.RenderGhost(Const Info: TAtomicInfo);
Var
  px, py, ox, oy: Single;
  ViewPort, m: TMatrix4x4;
Begin
  // Die selbe Transformation wie in TGame.Render
  ViewPort := IdentityMatrix4x4;
  ViewPort := TranslateMatrix4x4(ViewPort, fViewPortInfo.TopLeft.X, fViewPortInfo.TopLeft.y, 0);
  ViewPort := ScaleMatrix4x4(ViewPort, fViewPortInfo.Scale, fViewPortInfo.Scale, 1);
  (*
   * Der Geist steht auf dem Rahmen eine Kachel außerhalb des Feldes, so viel Platz ist aber nicht
   * (links / rechts 20 Pixel, unten 18 Pixel), deshalb wird er um GhostScale verkleinert und
   * stufenlos näher an das Feld heran geschoben (ox, oy), damit er nicht aus dem Bild ragt.
   *)
  px := Info.Position.x * FieldBlockWidth + FieldxOff; // Der Punkt um den TAtomic.Render die Figur zeichnet
  py := (Info.Position.y + 0.25) * FieldBlockHeight + FieldyOff;
  ox := 0.25 * FieldBlockWidth * min(1, max(0, 0.5 - Info.Position.x))
    - 0.25 * FieldBlockWidth * min(1, max(0, Info.Position.x - (FieldWidth - 0.5)));
  oy := 0.1 * FieldBlockHeight * min(1, max(0, 0.5 - Info.Position.y))
    - 0.5 * FieldBlockHeight * min(1, max(0, Info.Position.y - (FieldHeight - 0.5)));
  m := TranslateMatrix4x4(ViewPort, px + ox, py + oy, 0);
  m := ScaleMatrix4x4(m, GhostScale, GhostScale, 1);
  m := TranslateMatrix4x4(m, -px, -py, 0);
  SetShaderTransform(m);
  fAtomics[info.ColorIndex].Render(Info, false);
  SetShaderTransform(ViewPort);
End;

Procedure TGame.RenderFlameThrowers;
Var
  i, k, dx, dy, x, y: integer;
  ani: TOpenGL_Animation;
Begin
  // Mod Flammenwerfer: Der Strahl wird mit den Flammen der Explosion gezeichnet (Mitte und Ende in Blickrichtung)
  For i := 0 To high(fPlayer) Do Begin
    If (Not fPlayer[i].Info.Alive) Or fPlayer[i].Info.Dying Or (fPlayer[i].Info.FlameThrowerLen = 0) Then Continue;
    dx := 0;
    dy := 0;
    Case (trunc(fPlayer[i].Info.Direction) Div 90) Of
      0: dx := 1;
      1: dy := -1;
      2: dx := -1;
      3: dy := 1;
    End;
    x := trunc(fPlayer[i].Info.Position.x);
    y := trunc(fPlayer[i].Info.Position.y);
    For k := 1 To fPlayer[i].Info.FlameThrowerLen Do Begin
      If k = fPlayer[i].Info.FlameThrowerLen Then Begin
        ani := fAtomics[fPlayer[i].Info.FlameThrowerColor].FlameEnd;
      End
      Else Begin
        ani := fAtomics[fPlayer[i].Info.FlameThrowerColor].FlameMiddle;
      End;
      ani.Render(FieldxOff + (x + k * dx) * FieldBlockWidth, FieldyOff + (y + k * dy) * FieldBlockHeight, atomic_Map_Layer + 2 * atomic_EPSILON, (trunc(fPlayer[i].Info.Direction) Div 90) * 90);
    End;
  End;
End;

Procedure TGame.RenderFlash;
Var
  t: QWord;
  a: Single;
Begin
  (*
   * Mod weißer Bildschirm: Erst fFlashMs lang mit voller Stärke, dann über fFlashFadeMs zurück.
   * Normales Alpha Blending, damit die Stärke stufenlos geht (die Texturen der Engine nutzen ein invertiertes Alpha).
   *)
  If fFlashTimeStamp = 0 Then exit;
  t := GetTickCount64 - fFlashTimeStamp;
  If t < fFlashMs Then Begin
    a := fFlashIntensity / 100;
  End
  Else If t < fFlashMs + fFlashFadeMs Then Begin
    a := fFlashIntensity / 100 * (1 - (t - fFlashMs) / fFlashFadeMs);
  End
  Else Begin
    fFlashTimeStamp := 0;
    exit;
  End;
  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  UseColorShader;
  SetShaderColor(1, 1, 1, a);
  glShaderBegin(GL_TRIANGLE_FAN);
  glShaderVertex(0, 0, atomic_dialog_Layer);
  glShaderVertex(GameWidth, 0, atomic_dialog_Layer);
  glShaderVertex(GameWidth, GameHeight, atomic_dialog_Layer);
  glShaderVertex(0, GameHeight, atomic_dialog_Layer);
  glShaderEnd();
  UseTextureShader;
  glDisable(GL_BLEND);
  glEnable(GL_DEPTH_TEST);
End;

Procedure TGame.RenderOpenBorders;

  Function ShowOpen(Open: Boolean; Index: Boolean): Boolean;
  Var
    n: QWord;
  Begin
    // Nach dem Öffnen stehen die Pfeile nur noch OpenBordersArrowTime, danach stören sie nur in der Grafik
    n := GetTickCount64;
    If Not Open Then fOpenBordersSince[Index] := 0;
    If Open And (fOpenBordersSince[Index] = 0) Then fOpenBordersSince[Index] := n;
    result := Open And (n - fOpenBordersSince[Index] < OpenBordersArrowTime);
  End;

Var
  i: Integer;
  tb, lr: Boolean;
Begin
  (*
   * Mod offene Grenzen: Pfeile außen auf dem Rahmen zeigen, welche Ränder offen sind.
   * Kurz vor dem Öffnen blinken sie (OpenBordersWarnTime).
   *)
  tb := ShowOpen(fActualField.OpenTopBottom, true) Or (fActualField.OpenWarnTopBottom And ((GetTickCount64 Div 250) Mod 2 = 0));
  lr := ShowOpen(fActualField.OpenLeftRight, false) Or (fActualField.OpenWarnLeftRight And ((GetTickCount64 Div 250) Mod 2 = 0));
  If Not (tb Or lr) Then exit;
  glBindTexture(GL_TEXTURE_2D, 0);
  AtomicFont.Color := clYellow;
  AtomicFont.BackColor := clBlack;
  If lr Then Begin
    For i := 0 To FieldHeight - 1 Do Begin
      AtomicFont.Textout(5, FieldyOff + i * FieldBlockHeight + 11, atomic_Layer - atomic_EPSILON, '<');
      AtomicFont.Textout(FieldxOff + FieldWidth * FieldBlockWidth + 5, FieldyOff + i * FieldBlockHeight + 11, atomic_Layer - atomic_EPSILON, '>');
    End;
  End;
  If tb Then Begin
    For i := 0 To FieldWidth - 1 Do Begin
      AtomicFont.Textout(FieldxOff + i * FieldBlockWidth + 15, FieldyOff - 16, atomic_Layer - atomic_EPSILON, '^');
      AtomicFont.Textout(FieldxOff + i * FieldBlockWidth + 15, FieldyOff + FieldHeight * FieldBlockHeight + 2, atomic_Layer - atomic_EPSILON, 'v');
    End;
  End;
End;

Function TGame.BrickWalkVisible: Boolean;
Var
  k: Int64;
Begin
  (*
   * Mod durch Blöcke laufen: Der Spieler wird nur in (100 - fBrickWalkTransparency) % der Frames gezeichnet,
   * gleichmäßig verteilt, dadurch wirkt er durchsichtig (die Grafik Engine kennt keine Teil-Transparenz).
   *)
  k := 100 - fBrickWalkTransparency;
  result := (fRenderFrame * k) Div 100 <> ((fRenderFrame - 1) * k) Div 100;
End;

Procedure TGame.RenderFieldHeader;
Var
  x, y, i: Integer;
  s: String;
Begin
  (*
   * Die Spieler Info's
   *)
  For i := 0 To high(fPlayer) Do Begin
    If fPlayer[i].UID <> NoPlayer Then Begin
      If i = 1 Then Begin
        AtomicFont.BackColor := clWhite;
      End
      Else Begin
        AtomicFont.BackColor := clBlack;
      End;
      AtomicFont.Color := AtomicPlayerColorToColor(PlayerColors[i]);
      If HunterActive() Then Begin
        // Spielmodus Hunter: Die Zeit als Gejagter statt der Kills, sonst passt es nicht in die 100 Pixel (10 Zeichen) breite Spalte
        s := format('S:%d H:%d', [fPlayer[i].Score, fPlayer[i].Info.HuntedTime]);
      End
      Else Begin
        s := format('S:%d K:%d', [fPlayer[i].Score, fPlayer[i].Kills]);
      End;
      x := (i Div 2) * 100 + 10;
      y := (i Mod 2) * 16 + 10;
      glBindTexture(GL_TEXTURE_2D, 0);
      AtomicFont.Textout(x, y, atomic_dialog_Layer + atomic_EPSILON, s);
      If (Not fPlayer[i].Info.Alive) And (Not fPlayer[i].Info.Ghost) Then Begin // Ein Geist ist noch nicht ganz raus
        SetShaderAlphaThreshold(0.5);
        RenderAlphaQuad(x - 4, y - 4, atomic_dialog_Layer + atomic_EPSILON + atomic_EPSILON, fPlayerdeadTex);
        SetShaderAlphaThreshold(0);
      End;
    End;
  End;
  AtomicFont.BackColor := clBlack; // Auf Jeden Fall die BackColor wieder Resetten
  (*
   * Trefferpunkte Mod: Über dem Kopf des Spielers steht wie viele Treffer er noch aushält,
   * aber nur wenn es mehr als einer ist (sonst ist es ja wie im Original).
   *)
  AtomicFont.Color := clWhite;
  For i := 0 To high(fPlayer) Do Begin
    If fPlayer[i].Info.Alive And (Not fPlayer[i].Info.Dying) And (fPlayer[i].Info.HitPoints > 1) Then Begin
      x := round(fPlayer[i].Info.Position.x * FieldBlockWidth) + FieldxOff - 5;
      y := round((fPlayer[i].Info.Position.y - 1.5) * FieldBlockHeight) + FieldyOff;
      AtomicFont.Textout(x, y, atomic_dialog_Layer + atomic_EPSILON, inttostr(fPlayer[i].Info.HitPoints));
    End;
  End;
  If fModTestMode Then Begin
    (*
     * Damit nie unbemerkt im Mod Test Modus gespielt wird, steht das immer unten im Bild
     *)
    AtomicFont.Color := clYellow;
    AtomicFont.Textout(10, GameHeight - 16, atomic_dialog_Layer + atomic_EPSILON, 'MOD TEST MODE - F3: HELP');
    // Rückmeldung zum letzten Test Befehl
    If (fModTestMessage <> '') And (GetTickCount64 - fModTestMessageTimeStamp < 2000) Then Begin
      AtomicFont.Textout(330, GameHeight - 16, atomic_dialog_Layer + atomic_EPSILON, fModTestMessage);
    End;
    (*
     * Zustandsanzeige je Spieler: Restzeiten von Unverwundbarkeit und Stun über dem Kopf
     *)
    AtomicFont.Color := clYellow;
    For i := 0 To high(fPlayer) Do Begin
      If fPlayer[i].Info.Alive And (Not fPlayer[i].Info.Dying) Then Begin
        s := '';
        If fPlayer[i].Info.TestInvulnTime > 0 Then s := s + format('I%.1f ', [fPlayer[i].Info.TestInvulnTime / 10]);
        If fPlayer[i].Info.TestStunTime > 0 Then s := s + format('S%.1f ', [fPlayer[i].Info.TestStunTime / 10]);
        If fPlayer[i].Info.TestBrickWalkTime > 0 Then s := s + format('B%.1f', [fPlayer[i].Info.TestBrickWalkTime / 10]);
        If s <> '' Then Begin
          x := round(fPlayer[i].Info.Position.x * FieldBlockWidth) + FieldxOff - 20;
          y := round((fPlayer[i].Info.Position.y - 2) * FieldBlockHeight) + FieldyOff;
          AtomicFont.Textout(x, y, atomic_dialog_Layer + atomic_EPSILON, trim(s));
        End;
      End;
    End;
    If fShowModTestHelp Then Begin
      AtomicFont.Color := clWhite;
      AtomicFont.Textout(120, 60, atomic_dialog_Layer + atomic_EPSILON, 'F1  : ' + ModTestCommandToString(mtWhiteBomb));
      AtomicFont.Textout(120, 80, atomic_dialog_Layer + atomic_EPSILON, 'F2  : ' + ModTestCommandToString(mtFlash));
      AtomicFont.Textout(120, 100, atomic_dialog_Layer + atomic_EPSILON, 'F3  : hide this help');
      AtomicFont.Textout(120, 120, atomic_dialog_Layer + atomic_EPSILON, 'F4  : ' + ModTestCommandToString(mtAllItems));
      AtomicFont.Textout(120, 140, atomic_dialog_Layer + atomic_EPSILON, 'F5  : ' + ModTestCommandToString(mtPositiveDisease));
      AtomicFont.Textout(120, 160, atomic_dialog_Layer + atomic_EPSILON, 'F6  : ' + ModTestCommandToString(mtHitMe));
      AtomicFont.Textout(120, 180, atomic_dialog_Layer + atomic_EPSILON, 'F7  : ' + ModTestCommandToString(mtGhost));
      AtomicFont.Textout(120, 200, atomic_dialog_Layer + atomic_EPSILON, 'F8  : ' + ModTestCommandToString(mtHurry));
      AtomicFont.Textout(120, 220, atomic_dialog_Layer + atomic_EPSILON, 'F9  : ' + ModTestCommandToString(mtStun));
      AtomicFont.Textout(120, 240, atomic_dialog_Layer + atomic_EPSILON, 'F11 : ' + ModTestCommandToString(mtMaxHitPoints));
      AtomicFont.Textout(120, 260, atomic_dialog_Layer + atomic_EPSILON, 'F12 : ' + ModTestCommandToString(mtBrickWalk));
      AtomicFont.Textout(120, 280, atomic_dialog_Layer + atomic_EPSILON, 'Ins : ' + ModTestCommandToString(mtSpawnItems));
      AtomicFont.Textout(120, 310, atomic_dialog_Layer + atomic_EPSILON, 'Shift + key: 2nd player on this PC');
      AtomicFont.Textout(120, 330, atomic_dialog_Layer + atomic_EPSILON, 'I = invulnerable, S = stunned,');
      AtomicFont.Textout(120, 350, atomic_dialog_Layer + atomic_EPSILON, 'B = walk through bricks (seconds)');
    End;
  End;
  (*
   * Die Verbleibende Spielzeit
   *)
  AtomicBigFont.Textout(500, 10, atomic_dialog_Layer + atomic_EPSILON, fPlayingTime_s);
End;

Procedure TGame.RenderBombs;
Const
  Gray: TVector4 = (x: 0.6; y: 0.6; z: 0.6; w: 1.0); // Mod weiße Bombe: grau, damit man sie nicht mit den Bomben des weißen Spielers verwechselt
Var
  i: Integer;
  ani: TAnimation;
Begin
  SetShaderAlphaThreshold(0.5);
  For i := 0 To fBombCount - 1 Do Begin
    ani.ani := Nil;
    Case fBombs[i].Animation Of
      baNormal: ani := fAtomics[fBombs[i].ColorIndex].Bomb;
      baTimeTriggered: ani := fAtomics[fBombs[i].ColorIndex].Bomb_trigger;
      baDud: ani := fAtomics[fBombs[i].ColorIndex].Bomb_dud;
      baWobble: ani := fAtomics[fBombs[i].ColorIndex].Bomb_Wobble;
    End;
    If Not assigned(ani.ani) Then Begin
      LogShow('Error: TGame.RenderBombs: no Animation found.', llFatal);
    End;
    ani.ani.AnimationOffset := fBombs[i].AnimationOffset;
    If fBombs[i].WhiteBomb Then Begin
      UseTextureShader(Gray); // Setzt die AlphaThreshold zurück
      SetShaderAlphaThreshold(0.5);
    End;
    ani.ani.Render(Fieldxoff + fBombs[i].Position.x * FieldBlockWidth + ani.OffsetX, FieldyOff + fBombs[i].Position.y * FieldBlockHeight + ani.OffsetY, atomic_Bomb_Layer, 0);
    If fBombs[i].WhiteBomb Then Begin
      UseTextureShader();
      SetShaderAlphaThreshold(0.5);
    End;
  End;
  SetShaderAlphaThreshold(0);
End;

Procedure TGame.PingForOpenGames;
Var
  N: TNetworkAdapterList;
  i, EnterID: Integer;
  s: String;
  Key: Word;
Begin
  If fNeedDisconnect Then exit;
  If Not fUDPPingData.Active Then exit;
  If GetTickCount64 - fUDPPingData.LastTickValue >= 500 Then Begin
    fUDPPingData.LastTickValue := GetTickCount64;
    EnterID := LogEnter('TGame.PingForOpenGames');
    If assigned(fUDPPingData.Connection) Then Begin
      (*
       * Sollte warum auch immer der UDP Down sein, versuchen wir ihn neu zu starten ..
       *)
      If Not fUDPPingData.Connection.Connected Then Begin
        If Not fUDPPingData.Connection.Connect(LADDR_ANY, UDPPingPort) Then Begin
          LogShow('Error, could not start UDP Server.', llError);
          SwitchToScreen(sMainScreen);
          LogLeave(EnterID);
        End;
      End;
      Try
        n := GetLocalIPs();
        For i := 0 To high(n) Do Begin
          s := CalculateBroadCastAddressFromAddress(n[i].IpAddress, n[i].SubnetMask);
          fUDPPingData.Connection.SendMessage('Ping', s);
        End;
      Except
        log('Could not ping, maybe there is no valid network card present.', llCritical);
      End;
    End;
    LogLeave(EnterID);
  End;
  // If we do not find a Local game after 2 tries, bring up the "Ask for IP Dialog"
  // First Try is in ms 0
  // Second Try is in ms 500
  If fUDPPingData.StartTickValue + 550 <= GetTickCount64 Then Begin
    fUDPPingData.StartTickValue := GetTickCount64 + 1000 * 60 * 60; // Only poll once, by forwarding the next poll 1h this should be the case ;)
    key := VK_J;
    fScreens[sMainScreen].OnKeyDown(Nil, key, []);
  End;
End;

Procedure TGame.JoinViaParams(ip: String; Port: integer);
Var
  EnterID: Integer;
Begin
  (*
   * Beim Verbinden via Parameter müssen wir die
   * SwitchToScreen umgehen und passend initialisieren das die Spielengine genau gleich
   * initialisiert ist wie sie das via "Join" gewesen wäre ;)
   *)
  EnterID := LogEnter(format('TGame.JoinViaParams: %s:%d', [ip, port]));
  Settings.Router_IP := ip;
  Settings.Router_Port := inttostr(Port);
  fBackupSettings.Router_IP := ip;
  fBackupSettings.Router_Port := inttostr(Port);
  fActualScreen := fScreens[sJoinNetwork];
  fActualScreen.reset;
  fParamJoinIP := ip;
  fParamJoinPort := port;
  Join(ip, port);
  logleave(EnterID);
End;

Procedure TGame.StartGame;
Begin
  SendChunk(miStartGame, Nil);
End;

Procedure TGame.StartPingingForGames;
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.StartPingingForGames');
  fUDPPingData.Active := true;
  fUDPPingData.LastTickValue := GetTickCount64 - 500;
  fUDPPingData.StartTickValue := GetTickCount64;
  logleave(EnterID);
End;

Procedure TGame.StopPingingForGames;
Begin
  fUDPPingData.Active := false;
End;

Procedure TGame.Join(IP: String; Port: Integer);
Var
  isLocalhost: Boolean;
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.Join');
  log(format('Joining to %s on port %d as %s', [ip, port, Settings.NodeName]));

  // Store connection parameters for retry logic
  fParamJoinIP := ip;
  fParamJoinPort := port;

  // Check if connecting to localhost
  isLocalhost := (ip = '127.0.0.1') Or (ip = 'localhost');

  If isLocalhost Then Begin
    // For localhost, enable waiting mode - will retry periodically in OnIdle
    // This is needed because localhost "connection refused" comes back immediately
    // while remote servers may take time to respond (allowing time for server to start)
    fWaitingForLocalServer := true;
    fLastConnectionAttempt := 0; // Will trigger immediate attempt in OnIdle
    log('Localhost connection - will retry periodically until server is available', llInfo);
  End
  Else Begin
    // For remote servers, connection attempt is async - if it fails, Connection_Error will handle it
    fWaitingForLocalServer := false;
  End;

  // Initiate async connection attempt
  // Note: Connect() returning True only means the async connection was initiated,
  // not that we're actually connected. Real connection status comes via callbacks.
  If Not fChunkManager.Connect(ip, port) Then Begin
    // Immediate failure - connection couldn't even be initiated
    log('Could not initiate connection', llWarning);
    If Not isLocalhost Then Begin
      LogShow('Error: Could not initiate connection to server', llError);
      SwitchToScreen(sMainScreen);
    End;
    // For localhost, OnIdle will retry
  End
  Else Begin
    // Close a maybe openened join dialog ..
    TMainMenu(fScreens[sMainScreen]).StopJoinQuestion;
  End;
  logleave(EnterID);
End;

Constructor TGame.Create(Const Owner: TOpenGLControl);
Var
  k: TKeySet;
Begin
  Inherited Create;
  fOwner := Owner;
  fSerialInit.aStep := -1;
  fSoundManager := TSoundManager.Create();
  fSoundInfo := TSoundInfo.Create();
  fParamJoinIP := '';
  fWaitingForLocalServer := false;
  fPause := false;
  fNeedDisconnect := false;
  For k In TKeySet Do Begin
    fPlayerIndex[k] := -1;
  End;
  fInitialized := false;
  fActualScreen := Nil;
  fChunkManager := TChunkManager.create;
  fUDPPingData.Connection := Nil;
  fUDPPingData.Active := false;
  fLastIdleTick := GetTickCount64;
  OnNeedHideCursor := Nil;
  OnNeedShowCursor := Nil;
End;

Destructor TGame.Destroy;
Var
  i: TScreenEnum;
  j: Integer;
Begin
  SDL_FreeSticks;
  fSoundManager.free;
  fSoundManager := Nil;
  fArrows.free;
  fConveyors.free;
  fTramp.Free;
  fSoundInfo.free;
  fSoundInfo := Nil;
  fActualScreen := Nil;
  For i In TScreenEnum Do Begin
    If assigned(fScreens[i]) Then fScreens[i].Free;
    fScreens[i] := Nil;
  End;
  For j := 0 To high(fFields) Do Begin
    fFields[j].Free;
  End;
  For j := 0 To high(fAtomics) Do Begin
    fAtomics[j].Free;
  End;
  fHuntedAtomic.Free;
  fHuntedAtomic := Nil;
  setlength(fFields, 0);
  DoDisconnect();
  fChunkManager.free;
  fChunkManager := Nil;
End;

Procedure TGame.RegisterTCPConnection(Const Connection: TLTCPComponent);
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.RegisterTCPConnection');
  Connection.OnConnect := @Connection_Connect;
  Connection.OnDisconnect := @Connection_Disconnect;
  Connection.OnError := @Connection_Error;
  fChunkManager.OnReceivedChunk := @OnReceivedChunk;
  fChunkManager.RegisterConnection(Connection);
  LogLeave(EnterID);
End;

Procedure TGame.RegisterUDPConnection(Const Connection: TLUDPComponent);
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TGame.RegisterUDPConnection');
  fUDPPingData.Connection := Connection;
  StopPingingForGames();
  If assigned(Connection) Then Begin
    If fUDPPingData.Connection.Connected Then fUDPPingData.Connection.Disconnect();
    fUDPPingData.Connection.OnReceive := @OnUDPConnection_Receive;
    fUDPPingData.Connection.OnError := @OnUDPConnection_Error;
    If Not fUDPPingData.Connection.Connect(LADDR_ANY, UDPPingPort) Then Begin
      LogShow('Error, could not start UDP Server.', llError);
    End;
  End;
  LogLeave(EnterID);
End;

Function TGame.Initialize: Boolean;
Var
  i: TScreenEnum;
  field: TAtomicRandomField;
  EnterID: Integer;
  ScreenCount, ScreenDone: Integer;
{$IFDEF ShowInitTime}

  Procedure TimePoint(tp: Integer);
  Var
    d, n: UInt64;
  Begin
    n := GetTickCount64;
    d := n - fSerialInit.t;
    fSerialInit.t := n;
    writeln(format('%d: %d', [tp, d]));
  End;
{$ENDIF}
Begin
  result := false;
  EnterID := LogEnter('TGame.Initialize');
  Case fSerialInit.aStep Of
    -1: Begin
{$IFDEF ShowInitTime}
        fSerialInit.t := GetTickCount64;
        fSerialInit.total := fSerialInit.t;
{$ENDIF}
        fSerialInit.Loader := TLoaderDialog.create(fOwner);
        (*
         * Lade Prozente
         * 0..10 : Screens
         * 11..50: Fields
         * 51..100: Atomic's
         *)
        fSerialInit.Loader.Percent := 0;
        fSerialInit.Loader.Render(); // So schnell wie Möglich dem User mal Anzeigen, dass wir dran sind
        fSerialInit.aStep := 0;
      End;
    0: Begin
{$IFDEF ShowInitTime}
        TimePoint(1);
{$ENDIF}
        fSerialInit.Loader.Percent := 1;
        fSerialInit.Loader.Render();
        fSoundInfo.Musik := Settings.PlaySounds;
        fSoundInfo.Volume := Settings.VolumeValue;
        If Not fSoundManager.SetVolumeValue(Settings.VolumeValue) Then Begin
          log('Could not adjust sound volume.', llError);
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        FOnMouseMoveCapture := fOwner.OnMousemove;
        fOwner.OnMousemove := @FOnMouseMove;
        FOnMouseDownCapture := fOwner.OnMouseDown;
        fOwner.OnMouseDown := @FOnMouseDown;
        FOnMouseupCapture := fOwner.OnMouseup;
        fOwner.OnMouseup := @FOnMouseup;
        FOnDblClickCapture := fOwner.OnDblClick;
        fOwner.OnDblClick := @FOnDblClick;
        FOnKeyDownCapture := fOwner.OnKeyDown;
        fOwner.OnKeyDown := @FOnKeyDown;
        FOnKeyUpCapture := fOwner.OnKeyUp;
        fOwner.OnKeyUp := @FOnKeyUp;
        fOnResizeCapture := fOwner.OnResize;
        fOwner.OnResize := @FOnResize;

        fSerialInit.p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
        AtomicBigFont.CreateFont(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim);
        fSerialInit.Loader.Percent := 3;
        fSerialInit.Loader.Render();

        // Laden aller Screens
        fScreens[sMainScreen] := TMainMenu.Create(self);
        fScreens[sHost] := Nil;
        fScreens[sJoinNetwork] := TJoinMenu.Create(self);
        fScreens[sPlayerSetupRequest] := Nil;
        fScreens[sPlayerSetup] := TPlayerSetupMenu.Create(self);
        fScreens[sEditFieldSetupRequest] := Nil;
        fSerialInit.Loader.Percent := 5;
        fSerialInit.Loader.Render();
        fSerialInit.aStep := 1;
      End;
    1: Begin
        ScreenDone := 0;
        fSerialInit.Loader.Percent := 5;
        fSerialInit.Loader.Render();
        fScreens[sEditFieldSetup] := TFieldSetupMenu.Create(self);
        fScreens[sDrawGame] := TDrawGameMenu.create(self);
        fScreens[sVictory] := TVictoryMenu.create(self);
        fScreens[sMatchStatistik] := TMatchStatistikMenu.create(self);
        fScreens[sOptions] := TOptionsMenu.Create(self);
        fScreens[sExitBomberman] := Nil;
        ScreenCount := 0;
        For i In TScreenEnum Do Begin
          If assigned(fScreens[i]) Then inc(ScreenCount);
        End;
        If ScreenCount = 0 Then ScreenCount := 1;
        For i In TScreenEnum Do Begin
          If Not assigned(fScreens[i]) Then Continue;
          fScreens[i].LoadFromDisk(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim);
          inc(ScreenDone);
          fSerialInit.Loader.Percent := 5 + round(5 * ScreenDone / ScreenCount);
          fSerialInit.Loader.Render();
        End;
        fSerialInit.Loader.Percent := 10;
        fSerialInit.Loader.Render();
        fSerialInit.aStep := 2;
      End;
    2: Begin
{$IFDEF ShowInitTime}
        TimePoint(2);
{$ENDIF}
        fSerialInit.Loader.Percent := 10;
        fSerialInit.Loader.Render();
        fPowerUpsTex[puNone].Image := 0; // Das Gibts ja net -> Weg
        fPowerUpsTex[puExtraBomb] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powbomb.png', smClamp);
        If fPowerUpsTex[puExtraBomb].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puLongerFlameLength] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powflame.png', smClamp);
        If fPowerUpsTex[puLongerFlameLength].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puDisease] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powdisea.png', smClamp);
        If fPowerUpsTex[puDisease].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puCanCick] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powkick.png', smClamp);
        If fPowerUpsTex[puCanCick].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puExtraSpeed] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powskate.png', smClamp);
        If fPowerUpsTex[puExtraSpeed].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puCanPunch] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powpunch.png', smClamp);
        If fPowerUpsTex[puCanPunch].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puCanGrab] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powgrab.png', smClamp);
        If fPowerUpsTex[puCanGrab].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puCanSpooger] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powspoog.png', smClamp);
        If fPowerUpsTex[puCanSpooger].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puGoldFlame] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powgold.png', smClamp);
        If fPowerUpsTex[puGoldFlame].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puTrigger] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powtrig.png', smClamp);
        If fPowerUpsTex[puTrigger].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puCanJelly] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powjelly.png', smClamp);
        If fPowerUpsTex[puCanJelly].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puSuperBadDisease] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powebola.png', smClamp);
        If fPowerUpsTex[puSuperBadDisease].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[puSlow] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powslow.png', smClamp);
        If fPowerUpsTex[puSlow].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fPowerUpsTex[purandom] := OpenGL_GraphikEngine.LoadGraphikItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'powrand.png', smClamp);
        If fPowerUpsTex[purandom].Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fSerialInit.Loader.Percent := 10;
        fSerialInit.Loader.Render();
        // Load PlayerDead Tex correct.
        fPlayerdeadTex.Image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'playerdead.png', ugraphics.ColorToRGB(clfuchsia), smClamp);
        fPlayerdeadTex := OpenGL_GraphikEngine.FindItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'playerdead.png');
        If fPlayerdeadTex.Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fhurry.Texture.Image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'hurry.png', ugraphics.ColorToRGB(clfuchsia), smClamp);
        If fhurry.Texture.Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fhurry.Texture := OpenGL_GraphikEngine.FindItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'hurry.png');
        fSerialInit.hohletex.image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'hole.png', ugraphics.ColorToRGB(clfuchsia), smClamp);
        If fSerialInit.hohletex.Image = 0 Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fSerialInit.hohletex := OpenGL_GraphikEngine.FindItem(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'hole.png');
        fSerialInit.Loader.Percent := 10;
        fSerialInit.Loader.Render();
{$IFDEF ShowInitTime}
        TimePoint(3);
{$ENDIF}
        // Laden der Felder
        fArrows := TOpenGL_Animation.Create;
        If Not fArrows.LoadFromFile(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'arrows.ani', true) Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fConveyors := TOpenGL_Animation.Create;
        If Not fConveyors.LoadFromFile(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'conveyor.ani', true) Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fTramp := TOpenGL_Animation.Create;
        If Not fTramp.LoadFromFile(fSerialInit.p + 'data' + PathDelim + 'res' + PathDelim + 'tramp.ani', true) Then Begin
          fSerialInit.aStep := 1000;
          result := true;
          logleave(EnterID);
          exit;
        End;
        fSerialInit.Loader.Percent := 10;
        fSerialInit.Loader.Render();
{$IFDEF ShowInitTime}
        TimePoint(4);
{$ENDIF}

        (*
         * Da es Sich bewegende Trampoline gibt, und "Statische" müssen wir ein extra Sprite mit einem "Statischen" anlegen,
         * das nachher gerendert werden kann, via fTramp.GetFirstBitmap() geht es nicht, da da sonst Rundungsfehler entstehen die man sieht :/
         *)
        fSerialInit.TrampStatic := OpenGL_SpriteEngine.AddSprite(
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].Image,
          'TrampStaticSprite',
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].AlphaImage,
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].Rect,
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].Width,
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].Height,
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].FramesPerRow,
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].FramesPerCol,
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].FrameStart,
          1,
          OpenGL_SpriteEngine.Sprite[fTramp.Sprite[0].SpriteIndex].dtTime,
          Nil,
          Nil
          );
{$IFDEF ShowInitTime}
        TimePoint(5);
{$ENDIF}
        fSerialInit.Loader.Percent := 10;
        fSerialInit.Loader.Render();
        fSerialInit.sl := FindAllDirectories(fSerialInit.p + 'data' + PathDelim + 'maps', false);
        fSerialInit.sl.Sorted := true;
        fSerialInit.sl.Sort;
{$IFDEF Only1Map}
        While fSerialInit.sl.Count > 1 Do Begin
          fSerialInit.sl.Delete(fSerialInit.sl.Count - 1);
        End;
{$ENDIF}
        setlength(fFields, fSerialInit.sl.Count);
        If fSerialInit.sl.count = 0 Then Begin
          LogShow('Error, no fields to load found', llFatal);
          fSerialInit.aStep := 1000;
          result := true;
          LogLeave(EnterID);
          exit;
        End;
{$IFDEF ShowInitTime}
        TimePoint(6);
{$ENDIF}
        fSerialInit.Loader.Percent := 10;
        fSerialInit.Loader.Render();
        fSerialInit.j := 0;
        fSerialInit.aStep := 3;
      End;
    3: Begin
        If fSerialInit.j < fSerialInit.sl.Count Then Begin
          // Unter GTK3 vor teuren Schritten einmal explizit presenten,
          // sonst kann ein schwarzes Zwischenbild sichtbar werden.
          fSerialInit.Loader.Percent := 10 + round(40 * fSerialInit.j / fSerialInit.sl.count);
          fSerialInit.Loader.Render();
          fFields[fSerialInit.j] := TAtomicField.Create();
          If Not fFields[fSerialInit.j].loadFromDirectory(fSerialInit.sl[fSerialInit.j], fArrows, fConveyors, fTramp, fSerialInit.hohletex, fSerialInit.TrampStatic) Then Begin
            LogShow('Error, unable to load field:' + fSerialInit.sl[fSerialInit.j], llFatal);
            fSerialInit.aStep := 1000;
            result := true;
            LogLeave(EnterID);
            exit;
          End;
          fSerialInit.Loader.Percent := 10 + round(40 * (fSerialInit.j + 1) / fSerialInit.sl.count);
          fSerialInit.Loader.Render();
{$IFDEF ShowInitTime}
          TimePoint(7 + fSerialInit.j);
{$ENDIF}
          inc(fSerialInit.j);
        End
        Else Begin
          fSerialInit.Loader.Percent := 50;
          fSerialInit.Loader.Render();
          fSerialInit.aStep := 4;
          fSerialInit.sl.free;
          fSerialInit.sl := Nil;
        End;
      End;
    4: Begin
        // Anfügen der Random Karte
        fSerialInit.Loader.Percent := 50;
        fSerialInit.Loader.Render();
        field := TAtomicRandomField.Create(); // Die initialisiert sich bereits richtig ;)
        field.CreatePreview(FFields);

        setlength(fFields, length(fFields) + 1); // Die Random Karte MUSS immer die Letzte sein (siehe auch TGame.HandleUpdateAvailableFieldList)
        fFields[high(fFields)] := field;
{$IFDEF ShowInitTime}
        TimePoint(20);
{$ENDIF}
        fSerialInit.j := 0;
        fSerialInit.aStep := 5;
      End;
    5: Begin
        // Laden aller Atomic's in ihren Farben --> Das ist was Ladetechnisch richtig weh tut, alles andere ist eigentlich "erträglich"
        // + 1, als letztes kommt der Gold Satz für den Gejagten (Spielmodus Hunter)
        If fSerialInit.j <= high(fAtomics) Then Begin
          fSerialInit.Loader.Percent := 50 + round(50 * fSerialInit.j / (length(fAtomics) + 1));
          fSerialInit.Loader.Render();
          fAtomics[fSerialInit.j] := TAtomic.Create;
          If Not fAtomics[fSerialInit.j].InitAsColor(fSerialInit.p + 'data' + PathDelim + 'atomic' + PathDelim, PlayerColors[fSerialInit.j]) Then Begin
            LogShow('Error, unable to load atomic.', llFatal);
            fSerialInit.aStep := 1000;
            result := true;
            LogLeave(EnterID);
            exit;
          End;
{$IFDEF ShowInitTime}
          TimePoint(21 + fSerialInit.j);
{$ENDIF}
          inc(fSerialInit.j);
        End
        Else If fSerialInit.j = length(fAtomics) Then Begin
          fSerialInit.Loader.Percent := 50 + round(50 * fSerialInit.j / (length(fAtomics) + 1));
          fSerialInit.Loader.Render();
          fHuntedAtomic := TAtomic.Create;
          If Not fHuntedAtomic.InitAsColor(fSerialInit.p + 'data' + PathDelim + 'atomic' + PathDelim, HuntedGoldColor) Then Begin
            LogShow('Error, unable to load atomic.', llFatal);
            fSerialInit.aStep := 1000;
            result := true;
            LogLeave(EnterID);
            exit;
          End;
          CreateHuntedAuraTexture();
          inc(fSerialInit.j);
        End
        Else Begin
          fSerialInit.Loader.Percent := 100;
          fSerialInit.Loader.Render(); // Als letztes kriegt der User zu sehen, dass wir fertig sind :-)
          fSerialInit.aStep := 6;
        End;
      End;
    6: Begin
        fSerialInit.Loader.free;
        fSerialInit.Loader := Nil;
        fInitialized := true;
        SwitchToScreen(sMainScreen);
{$IFDEF Linux}
        If Settings.Fullscreen Then Begin
          form1.SetFullScreen(True);
        End;
{$ENDIF}
{$IFDEF ShowInitTime}
        TimePoint(100);
        fSerialInit.total := GetTickCount64 - fSerialInit.total;
        writeln(format('Total: %d', [fSerialInit.total]));
{$ENDIF}
        fSerialInit.aStep := 1000;
        result := true;
      End;
  End;
  LogLeave(EnterID);
End;

Procedure TGame.Disconnect;
Begin
  (*
   * Lnet mag es nicht (erzeugt eine AV oder ein Memleak), wenn aus einem LEvent
   * heraus ein Disconnect aufgerufen wird der DoDisconnect code würde an dieser
   * Stelle zusätzlich auch eine Endlosrekursion erzeugen.
   * -> Einfachster Weg ist das Auslagern in den Idle Event
   *)
  fNeedDisconnect := true;
End;

Procedure TGame.DoDisconnect;
Var
  EnterID: Integer;
Begin
  (*
   * Wird über den Idle Handler aufgerufen und ist damit definitiv außerhalb der LEventer Eventloop
   *)
  fNeedDisconnect := false;
  EnterID := LogEnter('TGame.DoDisconnect');
  If fChunkManager.Connected Then Begin
    fChunkManager.Disconnect(true);
    While fChunkManager.Connected Do Begin
      fChunkManager.CallAction();
    End;
  End;
  If assigned(fUDPPingData.Connection) Then Begin
    fUDPPingData.Active := false;
    If fUDPPingData.Connection.Connected Then Begin
      fUDPPingData.Connection.Disconnect(true);
    End;
  End;
  logleave(EnterID);
End;

Procedure TGame.Render;
Var
  i, t: Integer;
  n: QWORD;
  m: TMatrix4x4;
Begin
  Go2d(fViewPortInfo.ScreenWidth, fViewPortInfo.ScreenHeight);
  m := IdentityMatrix4x4;
  m := TranslateMatrix4x4(m, fViewPortInfo.TopLeft.X, fViewPortInfo.TopLeft.y, 0);
  m := ScaleMatrix4x4(m, fViewPortInfo.Scale, fViewPortInfo.Scale, 1);
  SetShaderTransform(m);

  If Not fInitialized Then Begin
    glBindTexture(GL_TEXTURE_2D, 0);
    OpenGL_ASCII_Font.Color := clred;
    OpenGL_ASCII_Font.Textout(fViewPortInfo.TopLeft.X + 20, fViewPortInfo.TopLeft.y + 50,
      'Not initialized!' + LineEnding +
      'Error during loading, please restart application.' + LineEnding + LineEnding +
      'If the problem persists try a re run of the cd_data_extractor'
      );
    Exit2d();
    exit;
  End;
  Case fgameState Of
    gs_MainMenu: Begin
        If assigned(fActualScreen) Then fActualScreen.Render;
      End;
    gs_Gaming: Begin
        fActualField.render(fAtomics, fPowerUpsTex);
        RenderHuntedAuras(); // Nach dem Feld und vor den Bomben, damit die Aura unter Bomben und Spielern liegt
        RenderBombs;
        RenderOpenBorders(); // Vor den Spielern, damit die Pfeile unter ihnen (und unter den Geistern auf dem Rahmen) liegen
        RenderFlameThrowers();
        inc(fRenderFrame);
        For i := 0 To high(fPlayer) Do Begin
          If fPlayer[i].Info.Alive Then Begin
            // Wer durch Blöcke laufen kann, wird durchsichtig gezeichnet (nicht in der Pause, nicht beim Sterben und nie auf dem Frame mit der Flanke einer einmaligen Animation)
            If fPause Or fPlayer[i].Info.Dying Or fPlayer[i].Edge Or (Not fPlayer[i].Info.BrickWalk) Or BrickWalkVisible() Then Begin
              RenderPlayerbyInfo(fPlayer[i].Info, fPlayer[i].edge, i);
              fPlayer[i].edge := false;
            End;
          End;
          // Geister (Mod) werden klein auf dem Rahmen und flackernd gerendert (nicht in der Pause)
          If fPlayer[i].Info.Ghost Then Begin
            // Kurz bevor der Geist verschwindet (GhostMaxTimeMs), flackert er doppelt so schnell
            t := fGhostFlickerMs;
            If (fPlayer[i].Info.GhostTime > 0) And (fPlayer[i].Info.GhostTime <= GhostWarnTime) Then Begin
              If t = 0 Then t := 200;
              t := max(1, t Div 2);
            End;
            If fPause Or (t = 0) Or ((GetTickCount64 Div t) Mod 4 <> 3) Then Begin
              RenderGhost(fPlayer[i].Info);
            End;
            fPlayer[i].edge := false;
          End;
        End;
        If fPause Then Begin
          glBindTexture(GL_TEXTURE_2D, 0);
          AtomicFont.Color := clYellow;
          AtomicFont.BackColor := clBlack;
          AtomicFont.Textout(320 - 2 * 14, GameHeight Div 2, atomic_dialog_Layer + atomic_EPSILON, 'Pause');
        End;
        RenderFieldHeader();
        // TODO: Auslagern in eine eigene Procedur
        If fHurry.Enabled Then Begin // Wenn An, dann 5 mal Flashen und wieder aus ;)
          n := (GetTickCount64 - fHurry.TimeStamp) Div 500;
          If n Mod 2 = 0 Then Begin
            SetShaderAlphaThreshold(0.5);
            RenderAlphaQuad((GameHeight - fHurry.Texture.OrigHeight) / 2, (GameWidth - fHurry.Texture.OrigWidth) / 2, 0, fHurry.Texture);
            SetShaderAlphaThreshold(0);
          End;
          If n > 5 Then fHurry.Enabled := false;
        End;
        RenderFlash();
      End;
  End;
  fSoundInfo.Render;
  ResetShaderTransform;
  Exit2d();
End;

Procedure TGame.OnIdle;
Begin
  PingForOpenGames;
  (*
   * Doing the "real" disconnect in the idle handler, is the fastest way to do without risking a endles
   * recursion of disconnects in diconnect.
   *)
  If fNeedDisconnect Then Begin
    DoDisconnect();
  End;

  // Convert SDL-Inputs to "keyboard" inputs ;)
    // During Menu's map keys to Up, Down, Left, Right, Return and Escape
  SDL_SticksToKeyEvent(@FOnSDLKeyDown, @FOnSDLKeyUp);

  // Periodically retry connection if we're waiting for local server to start
  // On localhost, "connection refused" comes back immediately, so we need to retry periodically
  If fWaitingForLocalServer And (fParamJoinIP <> '') Then Begin
    If (fLastConnectionAttempt = 0) Or (GetTickCount64 - fLastConnectionAttempt >= ConnectionRetryInterval) Then Begin
      fLastConnectionAttempt := GetTickCount64;
      log(format('Retrying connection to %s:%d...', [fParamJoinIP, fParamJoinPort]), llInfo);
      // Initiate new async connection attempt
      If Not fChunkManager.Connect(fParamJoinIP, fParamJoinPort) Then Begin
        log('Could not initiate connection, will retry later', llInfo);
      End;
      // If Connect returned True, we wait for Connection_Connect or Connection_Error callback
    End;
  End;
End;

End.

