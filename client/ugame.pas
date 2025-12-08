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
  uatomic_common, uopengl_animation, uscreens, uChunkmanager, uatomic_field, uatomic, uopengl_graphikengine, usounds, usdl_joystick, usdl_gamecontroller,
  uloaderdialog;

Type
  TUDPPingData = Record
    Active: Boolean;
    LastTickValue: UInt64;
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

  { TGame }

  TGame = Class
  private
    fsdl_Loaded: Boolean;
    fsdlJoysticks: Array[TKeySet] Of TSDL_Joystick;
    fsdlControllers: Array[TKeySet] Of TSDL_GameControllerEx;
    fControllerLogged: Array[TKeySet] Of Boolean; // Track if we've logged controller usage
    fMenuDpadState: record
      up, down, left, right, buttonA, buttonB: Boolean;
    end; // Track previous D-pad state for menu navigation
    fBlockGameInputUntilRelease: Boolean; // Block game input until all buttons released
    fBlockMenuInputUntilRelease: Boolean; // Block menu input until all buttons released
    fTramp, fConveyors, fArrows: TOpenGL_Animation; // Wird den Karten zur Verfügung gestellt
    fHurry: THurry;
    fSoundManager: TSoundManager;
    fSoundInfo: TSoundInfo;
    fLastIdleTick: QWord;
    fLastUpdateTimestamp: QWord; // For network performance monitoring
    fLastKeyDown: Array[akFirstAction..akSecondAction] Of QWORD;
    fPlayerdeadTex: TGraphikItem;
    fPowerUpsTex: TPowerTexArray;
    fBombCount: integer;
    fBombs: Array[0..15 * 11 - 1] Of TBombInfo;
    fParamJoinIP: String;
    fParamJoinPort: integer;
    fPlayingTime_s: integer;
    fPause: Boolean;
    fNeedDisconnect: Boolean; // Wenn True, dann wird ein Disconnect via Idle Handler durchgeführt (das darf nach LNet nicht im Socket Event gemacht werden da es sonst eine AV-Gibt)
    fActualField: TAtomicField;
    fPlayerIndex: Array[TKeySet] Of integer; // Der Index in Fplayer,
    fAtomics: TAtomics;
    fBackupSettings: TAtomicSettings; // Sind wir nicht der Master Spieler, dann müssen wir unsere Settings Sichern
    fInitialized: Boolean; // True, wenn Initialize erfolgreich durchgelaufen wurde
    fLoaderDialog: TLoaderDialog; // Reference to loading dialog during initialization
    fFields: Array Of TAtomicField;
    fScheme: TScheme;
{$IFNDEF DebuggMode}
    fPlayer: TPlayers;
{$ENDIF}
    fUserID: Integer; // Eine Kopie unserer Userid (Kopie, weil sie in TPlayer auch ist), wird beim Erfolgreichen Verbinden zum Server gesetzt.
    fMasterUserID: Integer; // Die Master User ID (Für das SpielerSetup)
    fOwner: TOpenGLControl;
    fgameState: TGameState;
    fScreens: Array[TScreenEnum] Of TScreen;
    fActualScreen: TScreen;

    FOnMouseMoveCapture: TMouseMoveEvent;
    FOnMouseDownCapture, FOnMouseupCapture: TMouseEvent;
    FOnDblClickCapture: TNotifyEvent;
    FOnKeyDownCapture, FOnKeyUpCapture: TKeyEvent;

    fChunkManager: TChunkManager;
    fUDPPingData: TUDPPingData;
    FViewportOffsetX: Integer;
    FViewportOffsetY: Integer;
    FViewportWidth: Integer;
    FViewportHeight: Integer;
    FControlWidth: Integer;
    FControlHeight: Integer;
    FViewportScale: Double;
    fDataPath: String; // Base path to data directory

    // === CLIENT-SIDE INTERPOLATION ===
    // Snapshots of server state
    fServerSnapshots: Array[0..1] Of Record
      Timestamp: QWord;          // GetTickCount64 when received
      ServerTime_ms: Integer;    // fPlayingTime_s * 1000
      PlayerInfos: Array[0..9] Of TAtomicInfo;
      Valid: Boolean;            // Is this snapshot valid?
    End;
    fCurrentSnapshot: Integer;   // Index of current snapshot (0 or 1)
    
    // Interpolated state (calculated each frame)
    fInterpolatedState: Record
      PlayingTime_ms: Integer;
      PlayerInfos: Array[0..9] Of TAtomicInfo;
    End;
    
    // Interpolation parameters
    fExtrapolationLimit: Integer;   // Max extrapolation time (default 100ms)
    
    // Note: fInterpolationDelay and fDebugStats are now in public section for debug overlay access

    Procedure FOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure FOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure FOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure FOnDblClick(Sender: TObject);

    Procedure FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);

    Function CheckKeyDown(Key: Word; Keys: TKeySet): Boolean;
    Procedure CheckKeyUp(Key: Word; Keys: TKeySet);
    Procedure CheckSDLKeys();
    Procedure ReadCurrentButtonState(var up, down, left, right, buttonA, buttonB: Boolean);
    Function AreAllGamepadButtonsReleased(): Boolean;
    Procedure CheckSDLKeysForMenu();
    Procedure ReinitControllersWithLogs();

    Function LoadSchemeFromFile(Const SchemeFileName: String): Boolean;
    Procedure OnQuitGameSoundFinished(Sender: TObject);
    Function ResolveResourceBase(BasePath: String): String;

    Procedure RenderPlayerbyInfo(Const Info: TAtomicInfo; Edge: Boolean);
    Procedure RenderFieldHeader();
    Procedure RenderBombs();
    Function PointInsideViewport(ScreenX, ScreenY: Integer): Boolean;
    Function ScreenToGameX(ScreenX: Integer): Integer;
    Function ScreenToGameY(ScreenY: Integer): Integer;
    
    // Client-side interpolation
    Procedure InterpolateGameState();

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
    
    // Expose interpolation fields for debug overlay (must be before properties!)
    fDebugStats: Record
      LastRTT: Integer;
      AvgRTT: Single;
      MaxRTT: Integer;             // Worst RTT since game start
      InterpolationFactor: Single;
      MaxInterpFactor: Single;     // Worst interpolation factor
      IsExtrapolating: Boolean;
      SnapshotAge: Integer;
      MaxSnapAge: Integer;         // Worst snapshot age
    End;
    fInterpolationDelay: Integer;

    OnNeedHideCursor: TNotifyEvent;
    OnNeedShowCursor: TNotifyEvent;
    
    Property PlayingTime_s: Integer Read fPlayingTime_s;
    Property ChunkManager: TChunkManager Read fChunkManager; // For network thread access
    Property LoaderDialog: TLoaderDialog Read fLoaderDialog; // For OnPaint to render loading dialog during initialization
    Property IsInitialized: Boolean Read fInitialized; // Check if game is initialized

    Constructor Create();
    Destructor Destroy; override;

    Procedure RegisterTCPConnection(Const Connection: TLTCPComponent);
    Procedure RegisterUDPConnection(Const Connection: TLUDPComponent);

    Procedure SwitchToScreen(TargetScreen: TScreenEnum);

    Procedure Initialize(Const Owner: TOpenGLControl); // Lädt alles was es so zu laden gibt (OpenGL-Technisch), wird einmalig in OnMakeCurrent Aufgerufen
    Procedure Disconnect();
    Procedure Render();

    Procedure OnIdle; // Wird von Application.OnIdle aufgerufen
    Procedure JoinViaParams(ip: String; Port: integer); // Nur 1 mal direkt nach dem Start erlaubt (siehe TForm1.OnIdle)

    Function GetControllerCount: Integer;

    (*
     * Werden und dürfen nur aus uScreens heraus aufgerufen werden
     *)
    Procedure ChangePlayerKey(PlayerIndex, Direction: Integer);
    Procedure UpdateWinsToWin(Delta: Integer);
    Procedure UpdateSelectedField(Delta: Integer);
    Procedure StartGame();
    Procedure StartPlayingSong(Filename: String);
    Procedure PlaySoundEffect(Filename: String; EndCallback: TNotifyEvent = Nil);
    Procedure SetViewportMetrics(ControlWidth, ControlHeight, OffsetX, OffsetY,
      ViewportWidth, ViewportHeight: Integer);
    Function GetKeySetDisplayName(Keys: TKeySet): String;
  End;

Var
  Game: TGame; // Die SpielEngine

Implementation

Uses dglopengl
  , FileUtil
  , LazUTF8
  , LazFileUtils
  , Graphics
  , Forms
  , Unit1 // Nur wegen Close, ggf via Dependency Injection besser lösen...
  , math
  , sdl2
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
  , uearlylog
  ;

Var
  NeedFormClose: Boolean = false;

  (*
   * Gibt den Index von Value in aArray zurück, -1 wenn nicht enthalten.
   *)

Function IndexOf(Value: Integer; Const aArray: Array Of Integer): integer;
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
  glPushMatrix();
  glTranslatef(0, 0, atomic_dialog_Layer + atomic_EPSILON);
  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_DEPTH_TEST);
  // Anzeige "Volume"
  glBegin(GL_QUADS);
  glColor3f(0, 0, 0);
  glVertex2f(x, y);
  glVertex2f(x, y + h);
  glVertex2f(x + w, y + h);
  glVertex2f(x + w, y);
  s := 1 - fVolume / 10000;
  glColor3f(0, 1, 1);
  glVertex2f(x, y + s * h);
  glVertex2f(x, y + h);
  glVertex2f(x + w, y + h);
  glVertex2f(x + w, y + s * h);
  glend;
  // Anzeige Musik
  OpenGL_ASCII_Font.Color := clWhite;
  OpenGL_ASCII_Font.Textout(320 - 32, 400, 'Musik ' + BoolToStr(fMusik, 'on', 'off'));
  glEnable(GL_DEPTH_TEST);
  glPopMatrix();
End;

{ TGame }

Procedure TGame.SwitchToScreen(TargetScreen: TScreenEnum);
Var
  index, index2: integer;
Begin
  If Not fInitialized Then exit; // So Lange wir nicht Initialisiert sind, machen wir gar nix !
  log('TGame.SwitchToScreen', llTrace);
  
  // Blocking is now done immediately when button is pressed in CheckSDLKeysForMenu
  // Don't reset fMenuDpadState here - let the blocking mechanism handle it
  // Sobald wir versuchen uns in ein Spiel ein zu loggen muss ggf. SDL initialisiert werden
  // Also initialize for main menu and options to enable gamepad navigation
  If (TargetScreen = sHost) Or (TargetScreen = sJoinNetwork) Or 
     (TargetScreen = sMainScreen) Or (TargetScreen = sOptions) Then Begin
    If assigned(OnNeedHideCursor) Then OnNeedHideCursor(Nil);
    ReinitControllersWithLogs();
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
        Disconnect();
        HandleSetPause(false); // Sonst bleibt die Hauptmenü Animation ggf stehen, das will natürlich keiner ;)
        If assigned(OnNeedShowCursor) Then OnNeedShowCursor(Nil);
      End;
    sExitBomberman: Begin
        NeedFormClose := true;
        StartPlayingSong(''); // Disable the Background musik if playing, that speeds up the shutdown process or at least stop making noises
        If fDataPath = '' Then Begin
          fDataPath := ResolveResourceBase(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));
        End;
        PlaySoundEffect(fDataPath + 'res' + PathDelim + 'quitgame.wav', @OnQuitGameSoundFinished);
      End;
    sHost: Begin
        fParamJoinIP := '';
        If Not StartHost Then Begin // Wenn der Server nicht gestartet werden kann -> Raus
          LogLeave;
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
        LogLeave;
        exit;
      End;
    sPlayerSetup: Begin
        // Den Screen mit den Aktuellsten Daten versorgen
        ReinitControllersWithLogs(); // ensure detection fresh in setup
        TPlayerSetupMenu(fScreens[sPlayerSetup]).TeamPlay := Settings.TeamPlay;
        TPlayerSetupMenu(fScreens[sPlayerSetup]).LoadPlayerdata(fPlayer, fUserID);
      End;
    sEditFieldSetupRequest: Begin
        SwitchToFieldSetup;
        // Egal wie da es diesen Screen nicht gibt wird er nicht übernommen
        LogLeave;
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
      exit;
    End
    Else Begin
      If (fDataPath <> '') And FileExistsUTF8(fDataPath + 'schemes' + PathDelim + Settings.SchemeFile) Then Begin
        StartPingingForGames;
      End
      Else Begin
        logshow('Error, unable to load scheme file: ' + Settings.SchemeFile + LineEnding + 'please adjust in options.', llError);
        SwitchToScreen(sMainScreen);
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
  LogLeave;
End;

Procedure TGame.ChangePlayerKey(PlayerIndex, Direction: Integer);
Var
  m: TMemoryStream;
  numJoy: Integer;
  currentKeySet: TKeySet;
  skipJoy1, skipJoy2: Boolean;
  nextKeySet: TKeySet;
Begin
  log('TGame.ChangePlayerKey', llTrace);
  
  // Check which joysticks are available
  numJoy := 0;
  skipJoy1 := false;
  skipJoy2 := false;
  try
    If fsdl_Loaded And Assigned(SDL_NumJoysticks) Then Begin
      numJoy := SDL_NumJoysticks();
      // Joy 1 is available only if at least 1 joystick is detected
      skipJoy1 := (numJoy < 1);
      // Joy 2 is available only if at least 2 joysticks are detected
      skipJoy2 := (numJoy < 2);
    End
    Else Begin
      // SDL not loaded, skip both joysticks
      skipJoy1 := true;
      skipJoy2 := true;
    End;
  except
    // On error, skip both joysticks
    skipJoy1 := true;
    skipJoy2 := true;
  end;
  
  // If joysticks need to be skipped, we need to send multiple change requests
  // to skip unavailable joysticks. But this is complex and could cause issues.
  // Instead, we'll let the server cycle through all options, and the client
  // will handle skipping unavailable joysticks in HandleRefreshPlayerStats.
  // This is simpler and more reliable.
  
  m := TMemoryStream.Create;
  m.Write(PlayerIndex, SizeOf(PlayerIndex));
  m.Write(Direction, SizeOf(Direction));
  m.WriteAnsiString(Settings.NodeName);
  SendChunk(miChangePlayerKey, m);
  LogLeave;
End;

Procedure TGame.StartPlayingSong(Filename: String);
Begin
  log('TGame.StartPlayingSong', llTrace);
  fSoundManager.PlaySound(Filename);
  LogLeave;
End;

Procedure TGame.PlaySoundEffect(Filename: String; EndCallback: TNotifyEvent);
Begin
  log('TGame.PlaySoundEffect', llTrace);
  If Not fSoundManager.PlaySoundEffekt(Filename, EndCallback) Then Begin
    If assigned(EndCallback) Then Begin
      EndCallback(Nil);
    End;
  End;
  LogLeave;
End;

Procedure TGame.FOnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  xx, yy: Integer;
Begin
  If Not PointInsideViewport(X, Y) Then Begin
    If Assigned(FOnMouseDownCapture) Then
      FOnMouseDownCapture(Sender, Button, Shift, X, Y);
    exit;
  End;
  xx := ScreenToGameX(X);
  yy := ScreenToGameY(Y);
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
  KeyProcessed: Boolean;
Begin
  (*
   * Der Hack zum Beenden von Atomic Bomberman im Fehlerfall ;)
   *)
  If (Not fInitialized) And (Key = VK_ESCAPE) Then Begin
    fInitialized := true;
    SwitchToScreen(sExitBomberman);
    exit;
  End;
  KeyProcessed := false;
  If Not ((ssalt In Shift) And (key = VK_RETURN)) Then Begin // Sonst wird das VK_Return ggf unsinnig ausgewertet
    If key = VK_ADD Then Begin
      aVolume := fSoundManager.IncVolume;
      settings.VolumeValue := aVolume;
      fBackupSettings.VolumeValue := aVolume;
      fSoundInfo.Volume := aVolume;
      KeyProcessed := true;
    End;
    If key = VK_SUBTRACT Then Begin
      aVolume := fSoundManager.DecVolume();
      settings.VolumeValue := aVolume;
      fBackupSettings.VolumeValue := aVolume;
      fSoundInfo.Volume := aVolume;
      KeyProcessed := true;
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
      KeyProcessed := true;
    End;
    Case fgameState Of
      gs_MainMenu: Begin
          If assigned(fActualScreen) Then Begin
            // Save original key value before menu processes it
            // Check if this is a key that menu typically processes
            KeyProcessed := (key In [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_RETURN, VK_BACK, VK_ESCAPE]) Or
                           ((fPlayerIndex[ks0] <> -1) And (key In [Settings.Keys[ks0].KeyPrimary, Settings.Keys[ks0].KeySecondary])) Or
                           ((fPlayerIndex[ks1] <> -1) And (key In [Settings.Keys[ks1].KeyPrimary, Settings.Keys[ks1].KeySecondary]));
            // Let menu process the key first - menu will play sounds and handle the key
            // Menu needs Key to remain unchanged during OnKeyDown to properly process it
            fActualScreen.OnKeyDown(Sender, Key, Shift);
            // If menu set key to 0, it was definitely processed and consumed
            If (key = 0) Then KeyProcessed := true;
            // After menu processed the key, we need to mark it as handled to prevent system beep
            // but we do this at the end, after menu had chance to play sounds
          End;
        End;
      gs_Gaming: Begin
          If key = VK_ESCAPE Then Begin
{$IFNDEF Only3Player}
            If ID_YES = Application.MessageBox('Do you really want to quit?', 'Question', MB_ICONQUESTION Or MB_YESNO) Then
{$ENDIF}
              SwitchToScreen(sMainScreen);
            KeyProcessed := true;
          End;
          If key = VK_P Then Begin
            SendChunk(miTogglePause, Nil);
            KeyProcessed := true;
          End;
          If fPlayerIndex[ks0] <> -1 Then Begin
            If CheckKeyDown(key, ks0) Then KeyProcessed := true;
          End;
          If fPlayerIndex[ks1] <> -1 Then Begin
            If CheckKeyDown(key, ks1) Then KeyProcessed := true;
          End;
        End;
    End;
  End;
  If assigned(FOnKeyDownCapture) Then Begin
    FOnKeyDownCapture(sender, key, shift);
  End;
  // Mark key as handled to prevent system beep on macOS
  // For gaming state, always set Key := 0 if processed
  If KeyProcessed And (fgameState = gs_Gaming) Then Begin
    Key := 0;
  End;
  // For menu: don't set Key := 0 to allow menu sounds to play properly
  // Menu needs Key unchanged to process it and play sounds
  // The menu OnKeyDown handlers should process keys and prevent system beep naturally
  // by consuming the key events they handle
End;

Function TGame.CheckKeyDown(Key: Word; Keys: TKeySet): Boolean;
Var
  m: TMemoryStream;
  db, b: Boolean;
  ak: TAtomicKey;
  n: QWORD;
Begin
  result := false;
  If key In [Settings.Keys[Keys].KeyLeft, Settings.Keys[Keys].KeyRight, Settings.Keys[Keys].KeyUp, Settings.Keys[Keys].KeyDown, Settings.Keys[Keys].KeyPrimary, Settings.Keys[Keys].KeySecondary] Then Begin
    result := true;
    m := TMemoryStream.Create;
    b := true;
    db := false;
    m.Write(fPlayerIndex[keys], SizeOf(fPlayerIndex[keys]));
    m.Write(b, sizeof(b));
    Case IndexOf(Key, [Settings.Keys[Keys].KeyLeft, Settings.Keys[Keys].KeyRight, Settings.Keys[Keys].KeyUp, Settings.Keys[Keys].KeyDown, Settings.Keys[Keys].KeyPrimary, Settings.Keys[Keys].KeySecondary]) Of
      0: ak := akLeft;
      1: ak := akRight;
      2: ak := akUp;
      3: ak := akDown;
      4: Begin
          ak := akFirstAction;
          n := GetTickCount64;
          If fLastKeyDown[ak] + AtomicActionDoubleTime > n Then db := true;
          fLastKeyDown[ak] := n;
        End;
      5: Begin
          ak := akSecondAction;
          n := GetTickCount64;
          If fLastKeyDown[ak] + AtomicActionDoubleTime > n Then db := true;
          fLastKeyDown[ak] := n;
        End;
    End;
    fPlayer[fPlayerIndex[keys]].KeysPressed[ak] := True;
    m.Write(ak, SizeOf(ak));
    m.Write(db, sizeof(db));
    SendChunk(miClientKeyEvent, m);
  End;
End;

Procedure TGame.CheckKeyUp(Key: Word; Keys: TKeySet);
Var
  m: TMemoryStream;
  ak: TAtomicKey;
  db, b: Boolean;
  j: TAtomicKey;
Begin
  If key In [Settings.Keys[Keys].KeyLeft, Settings.Keys[Keys].KeyRight, Settings.Keys[Keys].KeyUp, Settings.Keys[Keys].KeyDown] Then Begin
    Case IndexOf(Key, [Settings.Keys[Keys].KeyLeft, Settings.Keys[Keys].KeyRight, Settings.Keys[Keys].KeyUp, Settings.Keys[Keys].KeyDown]) Of
      0: ak := akLeft;
      1: ak := akRight;
      2: ak := akUp;
      3: ak := akDown;
    End;
    fPlayer[fPlayerIndex[Keys]].KeysPressed[ak] := false;
    // Das Problem ist, das der Server bei Key Up die Animation komplett stoppt, das führt dazu, dass der Spieler "träge" wirkt
    // Aus diesem Grund muss hier geschaut werden ob ggf noch eine Andere Taste "Aktiv" ist, wenn ja wird stattdessen derren Down gesendet !
    b := false;
    db := false;
    For j In [akUp, akDown, akLeft, akRight] Do Begin
      If fPlayer[fPlayerIndex[Keys]].KeysPressed[j] Then Begin
        b := true;
        ak := j;
        break;
      End;
    End;
    m := TMemoryStream.Create;
    m.Write(fPlayerIndex[Keys], SizeOf(fPlayerIndex[Keys]));
    m.Write(b, sizeof(b));
    m.Write(ak, SizeOf(ak));
    m.Write(db, sizeof(db)); // Double wird beu Richtungs Key's ignoriert
    SendChunk(miClientKeyEvent, m);
  End;
End;

Procedure TGame.FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If fgameState = gs_Gaming Then Begin
    If fPlayerIndex[ks0] <> -1 Then CheckKeyUp(Key, ks0);
    If fPlayerIndex[ks1] <> -1 Then CheckKeyUp(key, ks1);
    If fPlayerIndex[ksJoy1] <> -1 Then CheckKeyUp(key, ksJoy1);
    If fPlayerIndex[ksJoy2] <> -1 Then CheckKeyUp(key, ksJoy2);
  End;
  If assigned(FOnKeyUpCapture) Then Begin
    FOnKeyUpCapture(sender, key, shift);
  End;
End;

Procedure TGame.CheckSDLKeys();

  Procedure CheckKeys(Keys: TKeySet);
  Var
    d, d0, d1: Integer;
    up, down, left, right, first, second: Boolean;
    wasUp, wasDown, wasLeft, wasRight: Boolean; // Previous state for hysteresis
    diagonalThreshold: Integer; // Lower threshold when in diagonal mode
    wasDiagonal: Boolean; // Was in diagonal mode in previous frame
    isDiagonal: Boolean;
    verticalStrength, horizontalStrength: Integer;
    dominantDirection: TAtomicKey;
    sendDominant: Boolean;
    dpadUp, dpadDown, dpadLeft, dpadRight: Boolean; // D-Pad state
    wasDpadUp, wasDpadDown, wasDpadLeft, wasDpadRight: Boolean; // Previous D-Pad state
    isDpadDiagonal: Boolean; // D-Pad diagonal state
    dpadActive: Boolean; // Whether D-Pad is currently active
  Begin
    If fPlayerIndex[keys] = AIPlayer Then exit;
    If fPlayerIndex[keys] = -1 Then exit;
    // Use legacy joystick mapping if configured
    If Not Settings.Keys[keys].UseSDL2 Then exit;
    If Not assigned(fsdlJoysticks[keys]) Then exit;
    
    // Remember previous state for hysteresis
    wasUp := fPlayer[fPlayerIndex[keys]].KeysPressed[akUp];
    wasDown := fPlayer[fPlayerIndex[keys]].KeysPressed[akDown];
    wasLeft := fPlayer[fPlayerIndex[keys]].KeysPressed[akLeft];
    wasRight := fPlayer[fPlayerIndex[keys]].KeysPressed[akRight];
    
    // 1. Check D-Pad first (digital input, should be sent immediately like keyboard)
    // D-Pad is processed separately from analog stick to ensure smooth diagonal movement
    dpadUp := false;
    dpadDown := false;
    dpadLeft := false;
    dpadRight := false;
    dpadActive := false;
    
    // Universal approach: Check both HAT and button-based D-Pad
    // Some controllers use HAT (classic), others use buttons 11-14 (modern controllers like DualSense)
    try
      // First check HAT-based D-pad (classic joysticks)
      if fsdlJoysticks[keys].HatCount > 0 then begin
        d := fsdlJoysticks[keys].Hat[0];
        if (d and SDL_HAT_UP) <> 0 then begin
          dpadUp := true;
          dpadActive := true;
        end;
        if (d and SDL_HAT_DOWN) <> 0 then begin
          dpadDown := true;
          dpadActive := true;
        end;
        if (d and SDL_HAT_LEFT) <> 0 then begin
          dpadLeft := true;
          dpadActive := true;
        end;
        if (d and SDL_HAT_RIGHT) <> 0 then begin
          dpadRight := true;
          dpadActive := true;
        end;
      end;
      
      // Also check button-based D-pad (modern controllers like DualSense, some Xbox controllers)
      // Check if controller has enough buttons and try buttons 11-14
      // Note: We check both HAT and buttons, so if controller has both, HAT takes priority
      if (not dpadActive) and (fsdlJoysticks[keys].ButtonCount > 14) then begin
        // Modern controllers: D-pad as buttons 11-14
        // Button 11 = Up, 12 = Down, 13 = Left, 14 = Right
        if fsdlJoysticks[keys].Button[11] then begin
          dpadUp := true;
          dpadActive := true;
        end;
        if fsdlJoysticks[keys].Button[12] then begin
          dpadDown := true;
          dpadActive := true;
        end;
        if fsdlJoysticks[keys].Button[13] then begin
          dpadLeft := true;
          dpadActive := true;
        end;
        if fsdlJoysticks[keys].Button[14] then begin
          dpadRight := true;
          dpadActive := true;
        end;
      end;
    except
      // Ignore D-pad errors, analog stick still works
    end;
    
    // If D-Pad is active, send events immediately (like keyboard) and skip analog stick processing
    // This ensures D-Pad behaves like keyboard input with smooth diagonal movement
    if dpadActive then begin
      // Remember previous D-Pad state
      wasDpadUp := fPlayer[fPlayerIndex[keys]].KeysPressed[akUp];
      wasDpadDown := fPlayer[fPlayerIndex[keys]].KeysPressed[akDown];
      wasDpadLeft := fPlayer[fPlayerIndex[keys]].KeysPressed[akLeft];
      wasDpadRight := fPlayer[fPlayerIndex[keys]].KeysPressed[akRight];
      
      // Send all direction changes (like keyboard - each key is independent)
      // This allows server to process diagonal movement when multiple directions are active
      If wasDpadUp <> dpadUp Then Begin
        If dpadUp Then Begin
          CheckKeyDown(Settings.Keys[keys].KeyUp, keys);
        End
        Else Begin
          CheckKeyUp(Settings.Keys[keys].KeyUp, keys);
        End;
      End;
      If wasDpadDown <> dpadDown Then Begin
        If dpadDown Then Begin
          CheckKeyDown(Settings.Keys[keys].KeyDown, keys);
        End
        Else Begin
          CheckKeyUp(Settings.Keys[keys].KeyDown, keys);
        End;
      End;
      If wasDpadLeft <> dpadLeft Then Begin
        If dpadLeft Then Begin
          CheckKeyDown(Settings.Keys[keys].KeyLeft, keys);
        End
        Else Begin
          CheckKeyUp(Settings.Keys[keys].KeyLeft, keys);
        End;
      End;
      If wasDpadRight <> dpadRight Then Begin
        If dpadRight Then Begin
          CheckKeyDown(Settings.Keys[keys].KeyRight, keys);
        End
        Else Begin
          CheckKeyUp(Settings.Keys[keys].KeyRight, keys);
        End;
      End;
      
      // Enhanced: When in diagonal mode, send all active directions every frame
      // This matches keyboard behavior where all pressed keys are sent continuously
      // Server processes diagonal movement when multiple directions are active
      isDpadDiagonal := ((dpadUp Or dpadDown) And (dpadLeft Or dpadRight));
      If isDpadDiagonal Then Begin
        // Send all active directions every frame (like keyboard - keys are held down)
        // This ensures smooth diagonal movement like keyboard
        If dpadUp Then Begin
          CheckKeyDown(Settings.Keys[keys].KeyUp, keys);
        End;
        If dpadDown Then Begin
          CheckKeyDown(Settings.Keys[keys].KeyDown, keys);
        End;
        If dpadLeft Then Begin
          CheckKeyDown(Settings.Keys[keys].KeyLeft, keys);
        End;
        If dpadRight Then Begin
          // Send right last so server processes it (matching keyboard behavior)
          CheckKeyDown(Settings.Keys[keys].KeyRight, keys);
        End;
      End;
      
      // D-Pad is active, skip analog stick processing
      // Process buttons and exit early
      first := Settings.Keys[keys].ButtonsIdle[0] = fsdlJoysticks[keys].Button[Settings.Keys[keys].ButtonIndex[0]];
      second := Settings.Keys[keys].ButtonsIdle[1] = fsdlJoysticks[keys].Button[Settings.Keys[keys].ButtonIndex[1]];
      
      // Das Key Up wird bei Action nicht geprüft..
      If (fPlayer[fPlayerIndex[keys]].KeysPressed[akFirstAction] <> first) And first Then Begin
        CheckKeyDown(Settings.Keys[keys].KeyPrimary, keys);
      End;
      // Das Key Up wird bei Action nicht geprüft..
      If (fPlayer[fPlayerIndex[keys]].KeysPressed[akSecondAction] <> second) And Second Then Begin
        CheckKeyDown(Settings.Keys[keys].KeySecondary, keys);
      End;
      // 3. Speichern für die nächste Runde ;)
      fPlayer[fPlayerIndex[keys]].KeysPressed[akFirstAction] := first;
      fPlayer[fPlayerIndex[keys]].KeysPressed[akSecondAction] := second;
      
      // Exit early - D-Pad processing is complete, analog stick is skipped
      exit;
    end;
    
    // D-Pad is not active, process analog stick normally (original logic unchanged)
    // 1. Ermitteln des Aktuellen "Gedrückt" stati
    up := false;
    down := false;
    left := false;
    right := false;
    
    // Calculate axis differences
    d0 := Settings.Keys[keys].AchsisIdle[0] - fsdlJoysticks[keys].Axis[Settings.Keys[keys].AchsisIndex[0]];
    d1 := Settings.Keys[keys].AchsisIdle[1] - fsdlJoysticks[keys].Axis[Settings.Keys[keys].AchsisIndex[1]];
    
    // Use hysteresis for diagonal directions: if we were in diagonal mode (both axes active),
    // use lower threshold to turn off. This prevents flickering when one axis slightly drops below threshold.
    // Check if we were in diagonal mode in previous frame
    diagonalThreshold := achsistrigger Div 2; // 50% of normal threshold when in diagonal
    wasDiagonal := ((wasUp Or wasDown) And (wasLeft Or wasRight));
    
    // Check up/down axis with hysteresis
    If abs(d0) > achsistrigger Then Begin
      // Normal threshold for activation
      If sign(d0) = Settings.Keys[keys].AchsisDirection[0] Then Begin
        up := true;
      End
      Else Begin
        down := true;
      End;
    End
    Else If wasDiagonal And (abs(d1) > achsistrigger) And (abs(d0) > diagonalThreshold) Then Begin
      // If we were in diagonal mode and horizontal axis is still active,
      // use lower threshold to keep vertical direction active
      If sign(d0) = Settings.Keys[keys].AchsisDirection[0] Then Begin
        up := true;
      End
      Else Begin
        down := true;
      End;
    End;
    
    // Check left/right axis with hysteresis
    If abs(d1) > achsistrigger Then Begin
      // Normal threshold for activation
      If sign(d1) = Settings.Keys[keys].AchsisDirection[1] Then Begin
        left := true;
      End
      Else Begin
        right := true;
      End;
    End
    Else If wasDiagonal And (abs(d0) > achsistrigger) And (abs(d1) > diagonalThreshold) Then Begin
      // If we were in diagonal mode and vertical axis is still active,
      // use lower threshold to keep horizontal direction active
      If sign(d1) = Settings.Keys[keys].AchsisDirection[1] Then Begin
        left := true;
      End
      Else Begin
        right := true;
      End;
    End;
    first := Settings.Keys[keys].ButtonsIdle[0] = fsdlJoysticks[keys].Button[Settings.Keys[keys].ButtonIndex[0]];
    second := Settings.Keys[keys].ButtonsIdle[1] = fsdlJoysticks[keys].Button[Settings.Keys[keys].ButtonIndex[1]];
    
    // 2. Rauskriegen ob ein Event abgeleitet werden muss
    // For diagonal directions, determine which direction should win based on axis strength
    // This matches keyboard behavior where the last key pressed determines direction
    // When in diagonal mode, send the stronger direction last so server processes it correctly
    
    // Determine if we're in diagonal mode and which direction should win
    Begin
      isDiagonal := ((up Or down) And (left Or right));
      verticalStrength := 0;
      horizontalStrength := 0;
      dominantDirection := akUp; // Default
      sendDominant := false;
      
      // Calculate axis strengths for diagonal mode
      If isDiagonal Then Begin
        If up Or down Then verticalStrength := abs(d0);
        If left Or right Then horizontalStrength := abs(d1);
        
        // Determine dominant direction based on axis strength
        // If vertical is stronger, prioritize up/down; if horizontal is stronger, prioritize left/right
        If (verticalStrength > horizontalStrength) Then Begin
          If up Then dominantDirection := akUp
          Else If down Then dominantDirection := akDown;
          sendDominant := true;
        End
        Else If (horizontalStrength > verticalStrength) Then Begin
          If left Then dominantDirection := akLeft
          Else If right Then dominantDirection := akRight;
          sendDominant := true;
        End
        Else Begin
          // Equal strength - use priority order: Up > Down > Left > Right
          If up Then dominantDirection := akUp
          Else If down Then dominantDirection := akDown
          Else If left Then dominantDirection := akLeft
          Else If right Then dominantDirection := akRight;
          sendDominant := true;
        End;
      End;
      
      // Send direction changes, handling diagonal mode specially
      If isDiagonal And sendDominant Then Begin
        // In diagonal mode, send all active directions, but send the dominant one last
        // This ensures server processes the correct direction
        
        // Send non-dominant directions first (if they changed)
        If (dominantDirection <> akUp) Then Begin
          If fPlayer[fPlayerIndex[keys]].KeysPressed[akUp] <> up Then Begin
            If up Then CheckKeyDown(Settings.Keys[keys].KeyUp, keys)
            Else CheckKeyUp(Settings.Keys[keys].KeyUp, keys);
          End;
        End;
        If (dominantDirection <> akDown) Then Begin
          If fPlayer[fPlayerIndex[keys]].KeysPressed[akDown] <> down Then Begin
            If down Then CheckKeyDown(Settings.Keys[keys].KeyDown, keys)
            Else CheckKeyUp(Settings.Keys[keys].KeyDown, keys);
          End;
        End;
        If (dominantDirection <> akLeft) Then Begin
          If fPlayer[fPlayerIndex[keys]].KeysPressed[akLeft] <> left Then Begin
            If left Then CheckKeyDown(Settings.Keys[keys].KeyLeft, keys)
            Else CheckKeyUp(Settings.Keys[keys].KeyLeft, keys);
          End;
        End;
        If (dominantDirection <> akRight) Then Begin
          If fPlayer[fPlayerIndex[keys]].KeysPressed[akRight] <> right Then Begin
            If right Then CheckKeyDown(Settings.Keys[keys].KeyRight, keys)
            Else CheckKeyUp(Settings.Keys[keys].KeyRight, keys);
          End;
        End;
        
        // Send dominant direction last so server processes it
        Case dominantDirection Of
          akUp: Begin
            If fPlayer[fPlayerIndex[keys]].KeysPressed[akUp] <> up Then Begin
              If up Then CheckKeyDown(Settings.Keys[keys].KeyUp, keys)
              Else CheckKeyUp(Settings.Keys[keys].KeyUp, keys);
            End
            Else If up Then Begin
              // Re-send to ensure server processes it (in case other direction was sent first)
              CheckKeyDown(Settings.Keys[keys].KeyUp, keys);
            End;
          End;
          akDown: Begin
            If fPlayer[fPlayerIndex[keys]].KeysPressed[akDown] <> down Then Begin
              If down Then CheckKeyDown(Settings.Keys[keys].KeyDown, keys)
              Else CheckKeyUp(Settings.Keys[keys].KeyDown, keys);
            End
            Else If down Then Begin
              // Re-send to ensure server processes it (in case other direction was sent first)
              CheckKeyDown(Settings.Keys[keys].KeyDown, keys);
            End;
          End;
          akLeft: Begin
            If fPlayer[fPlayerIndex[keys]].KeysPressed[akLeft] <> left Then Begin
              If left Then CheckKeyDown(Settings.Keys[keys].KeyLeft, keys)
              Else CheckKeyUp(Settings.Keys[keys].KeyLeft, keys);
            End
            Else If left Then Begin
              // Re-send to ensure server processes it (in case other direction was sent first)
              CheckKeyDown(Settings.Keys[keys].KeyLeft, keys);
            End;
          End;
          akRight: Begin
            If fPlayer[fPlayerIndex[keys]].KeysPressed[akRight] <> right Then Begin
              If right Then CheckKeyDown(Settings.Keys[keys].KeyRight, keys)
              Else CheckKeyUp(Settings.Keys[keys].KeyRight, keys);
            End
            Else If right Then Begin
              // Re-send to ensure server processes it (in case other direction was sent first)
              CheckKeyDown(Settings.Keys[keys].KeyRight, keys);
            End;
          End;
        End;
      End
      Else Begin
        // Normal single-direction mode - send changes as before
    If fPlayer[fPlayerIndex[keys]].KeysPressed[akUp] <> up Then Begin
      If up Then Begin
        CheckKeyDown(Settings.Keys[keys].KeyUp, keys);
      End
      Else Begin
        CheckKeyUp(Settings.Keys[keys].KeyUp, keys);
      End;
    End;
    If fPlayer[fPlayerIndex[keys]].KeysPressed[akDown] <> Down Then Begin
      If Down Then Begin
        CheckKeyDown(Settings.Keys[keys].KeyDown, keys);
      End
      Else Begin
        CheckKeyUp(Settings.Keys[keys].KeyDown, keys);
      End;
    End;
    If fPlayer[fPlayerIndex[keys]].KeysPressed[akLeft] <> left Then Begin
      If left Then Begin
        CheckKeyDown(Settings.Keys[keys].KeyLeft, keys);
      End
      Else Begin
        CheckKeyUp(Settings.Keys[keys].KeyLeft, keys);
      End;
    End;
    If fPlayer[fPlayerIndex[keys]].KeysPressed[akRight] <> right Then Begin
      If right Then Begin
        CheckKeyDown(Settings.Keys[keys].KeyRight, keys);
      End
      Else Begin
        CheckKeyUp(Settings.Keys[keys].KeyRight, keys);
          End;
        End;
      End;
    End;
    // Das Key Up wird bei Action nicht geprüft..
    If (fPlayer[fPlayerIndex[keys]].KeysPressed[akFirstAction] <> first) And first Then Begin
      CheckKeyDown(Settings.Keys[keys].KeyPrimary, keys);
    End;
    // Das Key Up wird bei Action nicht geprüft..
    If (fPlayer[fPlayerIndex[keys]].KeysPressed[akSecondAction] <> second) And Second Then Begin
      CheckKeyDown(Settings.Keys[keys].KeySecondary, keys);
    End;
    // 3. Speichern für die nächste Runde ;)
    fPlayer[fPlayerIndex[keys]].KeysPressed[akFirstAction] := first;
    fPlayer[fPlayerIndex[keys]].KeysPressed[akSecondAction] := second;
  End;

Begin
  If Not fsdl_Loaded Then exit;
  // If neither joystick is present and UseSDL2 not configured, nothing to do
  // Check all possible keysets that might use SDL
  If Not (Settings.Keys[ks0].UseSDL2 Or Settings.Keys[ks1].UseSDL2 Or 
          Settings.Keys[ksJoy1].UseSDL2 Or Settings.Keys[ksJoy2].UseSDL2) Then exit;
  
  // If we're blocking input until buttons are released, skip input processing
  If fBlockGameInputUntilRelease Then Begin
    exit;
  End;
  
  // SDL_PumpEvents is now called in OnIdle before this function

  CheckKeys(ks0);
  CheckKeys(ks1);
  CheckKeys(ksJoy1);
  CheckKeys(ksJoy2);
End;

Procedure TGame.ReadCurrentButtonState(var up, down, left, right, buttonA, buttonB: Boolean);
  
  Procedure CheckJoystick(Keys: TKeySet);
Var
  d: Integer;
Begin
    If Not Assigned(fsdlJoysticks[Keys]) Then exit;
    try
      // Check analog stick
      d := fsdlJoysticks[Keys].Axis[1]; // Y axis
      if d < -12000 then up := true;
      if d > 12000 then down := true;
      d := fsdlJoysticks[Keys].Axis[0]; // X axis
      if d < -12000 then left := true;
      if d > 12000 then right := true;
      
      // Check D-pad buttons (DualSense style)
      if fsdlJoysticks[Keys].ButtonCount > 14 then begin
        if fsdlJoysticks[Keys].Button[11] then up := true;
        if fsdlJoysticks[Keys].Button[12] then down := true;
        if fsdlJoysticks[Keys].Button[13] then left := true;
        if fsdlJoysticks[Keys].Button[14] then right := true;
      end;
      // Check HAT (classic joysticks)
      if fsdlJoysticks[Keys].HatCount > 0 then begin
        d := fsdlJoysticks[Keys].Hat[0];
        if (d and SDL_HAT_UP) <> 0 then up := true;
        if (d and SDL_HAT_DOWN) <> 0 then down := true;
        if (d and SDL_HAT_LEFT) <> 0 then left := true;
        if (d and SDL_HAT_RIGHT) <> 0 then right := true;
      end;
      
      // Buttons: X = button 0 (Enter), Circle = button 1 (Esc)
      if fsdlJoysticks[Keys].ButtonCount > 0 then buttonA := fsdlJoysticks[Keys].Button[0];
      if fsdlJoysticks[Keys].ButtonCount > 1 then buttonB := fsdlJoysticks[Keys].Button[1];
  except
    // Ignore errors
  end;
  End;
  
Begin
  up := false;
  down := false;
  left := false;
  right := false;
  buttonA := false;
  buttonB := false;
  
  // Check all possible joysticks - first available one wins
  CheckJoystick(ks0);
  CheckJoystick(ks1);
  CheckJoystick(ksJoy1);
  CheckJoystick(ksJoy2);
End;

Function TGame.AreAllGamepadButtonsReleased(): Boolean;
Var
  up, down, left, right, buttonA, buttonB: Boolean;
Begin
  // Read current button state
  ReadCurrentButtonState(up, down, left, right, buttonA, buttonB);
  
  // Return true only if ALL buttons are released
  result := not (up or down or left or right or buttonA or buttonB);
End;

Procedure TGame.CheckSDLKeysForMenu();
Var
  up, down, left, right, buttonA, buttonB: Boolean;
  d, i: Integer;
  key: Word;
  shift: TShiftState;
Begin
  // Only process in menu, not during gameplay
  If fgameState = gs_Gaming Then exit;
  If Not fsdl_Loaded Then exit;
  If Not assigned(fActualScreen) Then exit;
  
  // If we're blocking menu input until buttons are released, skip input processing
  If fBlockMenuInputUntilRelease Then Begin
    exit;
  End;
  
  // Check if any joystick is available (check all possible keysets)
  If Not (Assigned(fsdlJoysticks[ks0]) Or Assigned(fsdlJoysticks[ks1]) Or 
          Assigned(fsdlJoysticks[ksJoy1]) Or Assigned(fsdlJoysticks[ksJoy2])) Then exit;
  
  // Read D-pad and button state from first available controller
  ReadCurrentButtonState(up, down, left, right, buttonA, buttonB);
  
  shift := [];
  
  // Generate key events on state change (press only)
  if up and not fMenuDpadState.up then begin
    key := VK_UP;
    fActualScreen.OnKeyDown(nil, key, shift);
  end;
  if down and not fMenuDpadState.down then begin
    key := VK_DOWN;
    fActualScreen.OnKeyDown(nil, key, shift);
  end;
  if left and not fMenuDpadState.left then begin
    key := VK_LEFT;
    fActualScreen.OnKeyDown(nil, key, shift);
  end;
  if right and not fMenuDpadState.right then begin
    key := VK_RIGHT;
    fActualScreen.OnKeyDown(nil, key, shift);
  end;
  if buttonA and not fMenuDpadState.buttonA then begin
    key := VK_RETURN;
    // Mark button as pressed AND block menu input BEFORE calling OnKeyDown to prevent double-trigger
    fMenuDpadState.buttonA := true;
    fBlockMenuInputUntilRelease := true;
    fActualScreen.OnKeyDown(nil, key, shift);
  end;
  if buttonB and not fMenuDpadState.buttonB then begin
    key := VK_ESCAPE;
    // Mark button as pressed AND block menu input BEFORE calling OnKeyDown to prevent double-trigger
    fMenuDpadState.buttonB := true;
    fBlockMenuInputUntilRelease := true;
    fActualScreen.OnKeyDown(nil, key, shift);
  end;
  
  // Save state for next frame
  fMenuDpadState.up := up;
  fMenuDpadState.down := down;
  fMenuDpadState.left := left;
  fMenuDpadState.right := right;
  // buttonA and buttonB are already set above if they were just pressed,
  // otherwise update them normally
  if not buttonA then fMenuDpadState.buttonA := false;
  if not buttonB then fMenuDpadState.buttonB := false;
End;

Function TGame.GetControllerCount: Integer;
begin
  result := 0;
  if Assigned(fsdlJoysticks[ks0]) then inc(result);
  if Assigned(fsdlJoysticks[ks1]) then inc(result);
  if Assigned(fsdlJoysticks[ksJoy1]) then inc(result);
  if Assigned(fsdlJoysticks[ksJoy2]) then inc(result);
end;

Procedure TGame.ReinitControllersWithLogs();
Var
  index, index2: integer;
  numJoy: Integer;
  i: Integer;
Begin
  // If menu or game input is blocked, skip reinit to avoid resetting button states
  If (fBlockMenuInputUntilRelease Or fBlockGameInputUntilRelease) And Assigned(fsdlJoysticks[ks0]) Then Begin
    exit;
  End;
  
  // Always try to init SDL and detect controllers
  If (Not fsdl_Loaded) Then Begin
    fsdl_Loaded := SDL_LoadLib('');
    If fsdl_Loaded Then Begin
      // Initialize with GAMECONTROLLER (implies JOYSTICK)
      fsdl_Loaded := SDL_Init(SDL_INIT_GAMECONTROLLER) = 0;
      End;
  End;
  
  // Clean up old instances
  If assigned(fsdlJoysticks[ks0]) Then Begin
    fsdlJoysticks[ks0].Free;
    fsdlJoysticks[ks0] := Nil;
  End;
  If assigned(fsdlJoysticks[ks1]) Then Begin
    fsdlJoysticks[ks1].Free;
    fsdlJoysticks[ks1] := Nil;
  End;
  If assigned(fsdlJoysticks[ksJoy1]) Then Begin
    fsdlJoysticks[ksJoy1].Free;
    fsdlJoysticks[ksJoy1] := Nil;
  End;
  If assigned(fsdlJoysticks[ksJoy2]) Then Begin
    fsdlJoysticks[ksJoy2].Free;
    fsdlJoysticks[ksJoy2] := Nil;
  End;
  If assigned(fsdlControllers[ks0]) Then Begin
    fsdlControllers[ks0].Free;
    fsdlControllers[ks0] := Nil;
  End;
  If assigned(fsdlControllers[ks1]) Then Begin
    fsdlControllers[ks1].Free;
    fsdlControllers[ks1] := Nil;
  End;
  If assigned(fsdlControllers[ksJoy1]) Then Begin
    fsdlControllers[ksJoy1].Free;
    fsdlControllers[ksJoy1] := Nil;
  End;
  If assigned(fsdlControllers[ksJoy2]) Then Begin
    fsdlControllers[ksJoy2].Free;
    fsdlControllers[ksJoy2] := Nil;
  End;
  // Reset logging flags
  fControllerLogged[ks0] := false;
  fControllerLogged[ks1] := false;
  fControllerLogged[ksJoy1] := false;
  fControllerLogged[ksJoy2] := false;
  
  If fsdl_Loaded Then Begin
    // Try to detect and configure joystick-based controls (SDL_Joystick + existing mapping)
    try
      numJoy := SDL_NumJoysticks();
    except
      numJoy := 0;
    end;
    
    // Auto-configure ksJoy1/ksJoy2 to use first/second joystick
    // Note: We do NOT auto-configure ks0/ks1 anymore - they remain as keyboard inputs
    // Users can manually select Joy 1 or Joy 2 in the player setup menu
    // Always configure ksJoy1 and ksJoy2 so they're available for selection
    if numJoy > 0 then begin
      // Configure Joy 1 (ksJoy1) to use joystick 0
      // Only set UseSDL2 if not already configured (preserve user settings)
      if not Settings.Keys[ksJoy1].UseSDL2 then begin
        Settings.Keys[ksJoy1].UseSDL2 := true;
      try
        if SDL_JoystickNameForIndex(0) <> nil then
            Settings.Keys[ksJoy1].Name := SDL_JoystickNameForIndex(0)
        else
            Settings.Keys[ksJoy1].Name := '';
      except
          Settings.Keys[ksJoy1].Name := '';
      end;
        Settings.Keys[ksJoy1].NameIndex := 0;
        Settings.Keys[ksJoy1].AchsisIndex[0] := 1;
        Settings.Keys[ksJoy1].AchsisIdle[0] := 0;
        Settings.Keys[ksJoy1].AchsisDirection[0] := 1;
        Settings.Keys[ksJoy1].AchsisIndex[1] := 0;
        Settings.Keys[ksJoy1].AchsisIdle[1] := 0;
        Settings.Keys[ksJoy1].AchsisDirection[1] := 1;
        Settings.Keys[ksJoy1].ButtonIndex[0] := 0;
        Settings.Keys[ksJoy1].ButtonsIdle[0] := false;
        Settings.Keys[ksJoy1].ButtonIndex[1] := 2;
        Settings.Keys[ksJoy1].ButtonsIdle[1] := false;
      end;
    end;
    if numJoy > 1 then begin
      // Configure Joy 2 (ksJoy2) to use joystick 1
      // Only set UseSDL2 if not already configured (preserve user settings)
      if not Settings.Keys[ksJoy2].UseSDL2 then begin
        Settings.Keys[ksJoy2].UseSDL2 := true;
      try
        if SDL_JoystickNameForIndex(1) <> nil then
            Settings.Keys[ksJoy2].Name := SDL_JoystickNameForIndex(1)
        else
            Settings.Keys[ksJoy2].Name := '';
      except
          Settings.Keys[ksJoy2].Name := '';
      end;
        Settings.Keys[ksJoy2].NameIndex := 0;
        Settings.Keys[ksJoy2].AchsisIndex[0] := 1;
        Settings.Keys[ksJoy2].AchsisIdle[0] := 0;
        Settings.Keys[ksJoy2].AchsisDirection[0] := 1;
        Settings.Keys[ksJoy2].AchsisIndex[1] := 0;
        Settings.Keys[ksJoy2].AchsisIdle[1] := 0;
        Settings.Keys[ksJoy2].AchsisDirection[1] := 1;
        Settings.Keys[ksJoy2].ButtonIndex[0] := 0;
        Settings.Keys[ksJoy2].ButtonsIdle[0] := false;
        Settings.Keys[ksJoy2].ButtonIndex[1] := 2;
        Settings.Keys[ksJoy2].ButtonsIdle[1] := false;
      end;
    end;

    // Open legacy joystick instances according to configured names
    If Settings.Keys[ks0].UseSDL2 Then Begin
      index := ResolveJoystickNameToIndex(Settings.Keys[ks0].Name, Settings.Keys[ks0].NameIndex);
      If index <> -1 Then Begin
        fsdlJoysticks[ks0] := TSDL_Joystick.Create(index);
      End;
    End;
    If Settings.Keys[ks1].UseSDL2 Then Begin
      index := ResolveJoystickNameToIndex(Settings.Keys[ks1].Name, Settings.Keys[ks1].NameIndex);
      If index <> -1 Then Begin
        fsdlJoysticks[ks1] := TSDL_Joystick.Create(index);
      End;
    End;
    // Universal approach: Always map ksJoy1 and ksJoy2 by physical index, not by name
    // This ensures any controller works regardless of its name
    // ksJoy1 always maps to physical joystick index 0, ksJoy2 to index 1
    If Settings.Keys[ksJoy1].UseSDL2 And (numJoy > 0) Then Begin
      // Always use physical index 0 for ksJoy1, regardless of saved name
      // This ensures universal compatibility - any controller at index 0 will work
      try
        fsdlJoysticks[ksJoy1] := TSDL_Joystick.Create(0);
      except
        On E: Exception Do Begin
          fsdlJoysticks[ksJoy1] := Nil;
        End;
      end;
    End;
    If Settings.Keys[ksJoy2].UseSDL2 And (numJoy > 1) Then Begin
      // Always use physical index 1 for ksJoy2, regardless of saved name
      // This ensures universal compatibility - any controller at index 1 will work
      try
        fsdlJoysticks[ksJoy2] := TSDL_Joystick.Create(1);
      except
        On E: Exception Do Begin
          fsdlJoysticks[ksJoy2] := Nil;
        End;
      end;
    End;
  End;
End;

Function TGame.GetKeySetDisplayName(Keys: TKeySet): String;
begin
	// Handle joystick-specific keysets - show just "Joy 1" or "Joy 2" without controller name
	if keys = ksJoy1 then begin
		exit('Joy 1');
	end;
	if keys = ksJoy2 then begin
		exit('Joy 2');
	end;
	
	// Keyboard labels - shortened to "Key 0" and "Key 1" for consistency with "Joy 1" and "Joy 2"
	if keys = ks0 then exit('Key 0') else exit('Key 1');
end;

Function TGame.GetServerIPAddress(): String;
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
  basePath, clientPath: String;
{$IFDEF DARWIN}
  serverAppPath: String;
  portCheck: TProcessUTF8;
  portCheckOutput: String;
  portCheckCount: Integer;
  portListening: Boolean;
{$ENDIF}
Begin
  //Begin
  log('TGame.StartHost', llTrace);
  result := false;
  // Starten des Atomic_servers, dann als Client verbinden
  clientPath := ExtractFilePath(ParamStrUTF8(0));
  basePath := IncludeTrailingPathDelimiter(clientPath);
{$IFDEF DARWIN}
  // On macOS, check if we're in an .app bundle
  // If so, look for FPCAtomicServer.app in the same directory as FPCAtomic.app
  If Pos('.app/Contents/MacOS/', clientPath) > 0 Then Begin
    // We're in an .app bundle (e.g., FPCAtomic.app/Contents/MacOS/)
    // Go up to the app bundle directory (../../ from MacOS)
    serverAppPath := ExpandFileName(IncludeTrailingPathDelimiter(clientPath) + '../../');
    // Replace FPCAtomic.app with FPCAtomicServer.app
    If Pos('FPCAtomic.app', serverAppPath) > 0 Then Begin
      serverAppPath := StringReplace(serverAppPath, 'FPCAtomic.app', 'FPCAtomicServer.app', []);
    End
    Else Begin
      // Fallback: try relative to MacOS directory
      serverAppPath := ExpandFileName(IncludeTrailingPathDelimiter(clientPath) + '../../../FPCAtomicServer.app');
    End;
    log('Looking for server app at: ' + serverAppPath, llInfo);
    If DirectoryExistsUTF8(serverAppPath) Then Begin
      // Launch server directly with atomic_server binary (like run_server script does)
      // This ensures DYLD_LIBRARY_PATH is set correctly and parameters are passed
      serv := IncludeTrailingPathDelimiter(serverAppPath) + 'Contents/MacOS/atomic_server';
      If FileExistsUTF8(serv) Then Begin
        log('Launching server directly: ' + serv, llInfo);
        p := TProcessUTF8.Create(Nil);
        p.Options := [poDetached];
        p.Executable := '/bin/zsh';
        p.Parameters.Add('-c');
        // Set DYLD_LIBRARY_PATH and run atomic_server with parameters
        p.Parameters.Add('cd ' + IncludeTrailingPathDelimiter(serverAppPath) + 'Contents/MacOS' +
          ' && export DYLD_LIBRARY_PATH=' + IncludeTrailingPathDelimiter(serverAppPath) + 'Contents/lib:$DYLD_LIBRARY_PATH' +
          ' && ' + serv + ' -p ' + inttostr(settings.Port) +
          ' -t 0 -l ' + IntToStr(GetLoggerLoglevel()));
        p.Execute;
        p.free;
        result := true;
        LogLeave;
        exit;
      End
      Else Begin
        log('Server binary not found at: ' + serv, llWarning);
      End;
    End
    Else Begin
      log('Server app not found at: ' + serverAppPath, llWarning);
    End;
  End;
{$ENDIF}
  // Fallback: look for atomic_server binary in the same directory
  serv := basePath + 'atomic_server';
{$IFDEF Windows}
  serv := serv + '.exe';
{$ENDIF}
  If FileExistsUTF8(serv) Then Begin
    p := TProcessUTF8.Create(Nil);
    p.Options := [poNewConsole];
    p.Executable := serv;
    p.Parameters.Add('-p');
    p.Parameters.Add(inttostr(settings.Port));
    p.Parameters.Add('-t');
    p.Parameters.Add('0');
    p.Parameters.Add('-l');
    p.Parameters.Add(IntToStr(GetLoggerLoglevel()));
    p.Parameters.Add('-f');
    p.Parameters.Add(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'logs' + PathDelim + 'server.log');
    p.Execute;
    p.free;
  End
  Else Begin
    LogShow('Error: could not find server application, abort now', llError);
    LogLeave;
    exit;
  End;
  // Bis der Server Steht dauerts ein bischen, also warten wir
  // On macOS, .app bundles take longer to start, especially with network thread initialization
  // Also need time for the server to initialize and start listening on the port
{$IFDEF Windows}
  sleep(3000); // Unter Windows scheint es deutlich "länger" zu dauern
{$ELSE}
{$IFDEF DARWIN}
  // Wait for server to start and check if port is listening
  sleep(2000); // Initial wait for .app bundle to start
  // Check if server is actually listening on the port (up to 5 seconds)
  portListening := false;
  portCheckCount := 0;
  While (portCheckCount < 10) And (Not portListening) Do Begin
    sleep(500); // Check every 500ms
    Inc(portCheckCount);
    
    // Use lsof to check if port is listening
    portCheck := TProcessUTF8.Create(Nil);
    portCheck.Options := [poUsePipes, poWaitOnExit];
    portCheck.Executable := '/usr/sbin/lsof';
    portCheck.Parameters.Add('-i');
    portCheck.Parameters.Add(':' + inttostr(settings.Port));
    portCheck.Execute;
    
    SetLength(portCheckOutput, 1024);
    SetLength(portCheckOutput, portCheck.Output.Read(portCheckOutput[1], Length(portCheckOutput)));
    portCheck.Free;
    
    // If lsof found something, port is listening
    If Length(portCheckOutput) > 0 Then Begin
      portListening := true;
      log('Server is listening on port ' + inttostr(settings.Port), llInfo);
    End;
  End;
  
  If Not portListening Then Begin
    log('Warning: Server port not detected after ' + inttostr(portCheckCount * 500) + 'ms, will try to connect anyway', llWarning);
  End;
{$ELSE}
  sleep(3000); // For other Unix-like systems
{$ENDIF}
{$ENDIF}
  // StartClientGame; --> Das Verbinden macht der SwitchToScreen schon ;)
  Application.Restore;
  Application.BringToFront;
  result := true;
  LogLeave;
End;

Procedure TGame.SwitchToPlayerSetup;
Begin
  log('TGame.SwitchToPlayerSetup', llTrace);
  SendChunk(miSwitchToPlayerSetup, Nil);
  LogLeave;
End;

Procedure TGame.SwitchToFieldSetup;
Var
  i: integer;
Begin
  log('TGame.SwitchToMapSetup', llTrace);
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
    LogLeave;
    exit;
  End;

  UpdateFieldSetup(fActualField.Name, fActualField.Hash,
    TFieldSetupMenu(fScreens[sEditFieldSetup]).LastWinsToWinMatch);

  SendChunk(miSwitchToFieldSetup, Nil);
  LogLeave;
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
  i: Integer;
  B: Byte;
Begin
  // Der Client ist beim Server Registriert, nun gilt es um die Mitspielerlaubniss zu fragen.
  log('TGame.Connection_Connect', llTrace);
  fChunkManager.SetNoDelay(true);
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
  LogLeave;
End;

Procedure TGame.Connection_Disconnect(aSocket: TLSocket);
Begin
  log('TGame.Connection_Disconnect', llTrace);
  // Verbindung zum Server verloren / getrennt
  Settings := fBackupSettings;
  // Im Victory Screen können wir beliebig lange bleiben, von da aus geht es eh nur noch ins Hauptmenü ;)
  If fActualScreen <> fScreens[sVictory] Then Begin
    SwitchToScreen(sMainScreen); // Wir Fliegen raus auf die Top ebene
  End;
  LogLeave;
End;

Procedure TGame.Connection_Error(Const msg: String; aSocket: TLSocket);
Begin
  log('TGame.Connection_Error', llTrace);
  LogShow(msg, llError);
  SwitchToScreen(sMainScreen); // Wir Fliegen raus auf die Top ebene
  LogLeave;
End;

Procedure TGame.OnUDPConnection_Error(Const msg: String; aSocket: TLSocket);
Begin
  log('TGame.UDPConnection_Error', llTrace);
  StopPingingForGames();
  Disconnect();
  LogShow(msg, llError);
  SwitchToScreen(sMainScreen); // Wir Fliegen raus auf die Top ebene
  LogLeave;
End;

Procedure TGame.OnUDPConnection_Receive(aSocket: TLSocket);
Var
  Buffer: Array[0..1023] Of byte;
  cnt: integer;
  B: Byte;
  i: Integer;
  serverIP, ServerText: String;
  serverPort: integer;
Begin
  log('TGame.OnUDPConnection_Receive', llTrace);
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
          LogLeave;
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
  LogLeave;
End;

Procedure TGame.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Var
  i: integer;
  ts: QWORD;
  s: String;
  m: TMemoryStream;
  b: Boolean;
Begin
{$IFDEF DoNotLog_CyclicMessages}
  If ((Chunk.UserDefinedID And $FFFF) <> miUpdateGameData) And
    ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) And
    ((Chunk.UserDefinedID And $FFFF) <> miClientKeyEvent) Then
{$ENDIF}
    log('TGame.OnReceivedChunk : ' + MessageIdentifierToString(Chunk.UserDefinedID), llTrace);
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
{$IFDEF DoNotLog_CyclicMessages}
  If ((Chunk.UserDefinedID And $FFFF) <> miUpdateGameData) And
    ((Chunk.UserDefinedID And $FFFF) <> miHeartBeat) And
    ((Chunk.UserDefinedID And $FFFF) <> miClientKeyEvent) Then
{$ENDIF}
    LogLeave;
End;

Procedure TGame.SendChunk(UserDefinedID: Integer; Const Data: TStream);
Begin
{$IFDEF DoNotLog_CyclicMessages}
  If ((UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    log('Tctd.SendChunk : ' + MessageIdentifierToString(UserDefinedID), llTrace);
  If Not fChunkManager.SendChunk(UserDefinedID, data) Then Begin
    log('Could not send.', llCritical);
  End;
{$IFDEF DoNotLog_CyclicMessages}
  If ((UserDefinedID And $FFFF) <> miHeartBeat) Then
{$ENDIF}
    LogLeave;
End;

Procedure TGame.HandleRefreshPlayerStats(Const Stream: TMemoryStream);
Var
  cnt, i, uid, j, index, numJoy: integer;
  found: Boolean;
  k: TKeySet;
Begin
  (*
   * Das muss immer 1:1 zu TServer.RefreshAllPlayerStats sein !
   *)
  log('TGame.HandleRefreshPlayerStats', llTrace);
  cnt := -1;
  stream.Read(cnt, sizeof(cnt));
  If cnt = -1 Then Begin
    log('Error, invalid playerstats', llError);
    LogLeave;
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
    
    // Check if selected joystick is available, if not, skip to next available option
    numJoy := 0;
    try
      If fsdl_Loaded And Assigned(SDL_NumJoysticks) Then Begin
        numJoy := SDL_NumJoysticks();
      End;
    except
      numJoy := 0;
    end;
    
    // If server set Joy 1 but no joysticks are available, skip to next option
    If (k = ksJoy1) And (numJoy < 1) Then Begin
      // Skip to next option by sending another change request
      If (uid = fUserID) And (fPlayer[i].UID = uid) Then Begin
        // This is our player, send change request to skip unavailable joystick
        ChangePlayerKey(i, 1);
        // Don't process this update, wait for next refresh
        LogLeave;
        exit;
      End
      Else Begin
        // Not our player, just set to Keyboard 1 as fallback
        k := ks1;
      End;
    End
    // If server set Joy 2 but only 0-1 joysticks are available, skip to next option
    Else If (k = ksJoy2) And (numJoy < 2) Then Begin
      // Skip to next option by sending another change request
      If (uid = fUserID) And (fPlayer[i].UID = uid) Then Begin
        // This is our player, send change request to skip unavailable joystick
        ChangePlayerKey(i, 1);
        // Don't process this update, wait for next refresh
        LogLeave;
        exit;
      End
      Else Begin
        // Not our player, just set to Keyboard 1 as fallback
        k := ks1;
      End;
    End;
    
    // Reset UseSDL2 based on selected input type
    // If player selected keyboard (ks0 or ks1), disable SDL2 and close joystick
    // If player selected joystick (ksJoy1 or ksJoy2), enable SDL2
    If (k = ks0) Or (k = ks1) Then Begin
      // Keyboard input selected - disable SDL2 and close joystick
      If Settings.Keys[k].UseSDL2 Then Begin
        Settings.Keys[k].UseSDL2 := false;
        // Close joystick if open
        If assigned(fsdlJoysticks[k]) Then Begin
          fsdlJoysticks[k].Free;
          fsdlJoysticks[k] := Nil;
        End;
        If assigned(fsdlControllers[k]) Then Begin
          fsdlControllers[k].Free;
          fsdlControllers[k] := Nil;
        End;
      End;
    End Else If (k = ksJoy1) Or (k = ksJoy2) Then Begin
      // Joystick input selected - ensure SDL2 is enabled
      If Not Settings.Keys[k].UseSDL2 Then Begin
        Settings.Keys[k].UseSDL2 := true;
        // Open joystick if not already open
        // Universal approach: Always map by physical index, not by name
        // ksJoy1 always maps to physical joystick index 0, ksJoy2 to index 1
        If Not assigned(fsdlJoysticks[k]) And fsdl_Loaded Then Begin
          If (k = ksJoy1) And (numJoy > 0) Then Begin
            // Always use physical index 0 for ksJoy1
            try
              fsdlJoysticks[k] := TSDL_Joystick.Create(0);
            except
              On E: Exception Do Begin
                fsdlJoysticks[k] := Nil;
              End;
            end;
          End Else If (k = ksJoy2) And (numJoy > 1) Then Begin
            // Always use physical index 1 for ksJoy2
            try
              fsdlJoysticks[k] := TSDL_Joystick.Create(1);
            except
              On E: Exception Do Begin
                fsdlJoysticks[k] := Nil;
              End;
            end;
          End;
        End;
      End Else Begin
        // SDL2 already enabled, but make sure joystick is open
        // Universal approach: Always map by physical index, not by name
        If Not assigned(fsdlJoysticks[k]) And fsdl_Loaded Then Begin
          If (k = ksJoy1) And (numJoy > 0) Then Begin
            // Always use physical index 0 for ksJoy1
            try
              fsdlJoysticks[k] := TSDL_Joystick.Create(0);
            except
              On E: Exception Do Begin
                fsdlJoysticks[k] := Nil;
              End;
            end;
          End Else If (k = ksJoy2) And (numJoy > 1) Then Begin
            // Always use physical index 1 for ksJoy2
            try
              fsdlJoysticks[k] := TSDL_Joystick.Create(1);
            except
              On E: Exception Do Begin
                fsdlJoysticks[k] := Nil;
              End;
            end;
          End;
        End;
      End;
    End;
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
      LogLeave;
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
  LogLeave;
End;

Procedure TGame.SendSettings;
Var
  m: TMemoryStream;
Begin
  log('TGame.SendSettings', llTrace);
  If Not LoadSchemeFromFile(Settings.SchemeFile) Then Begin
    LogShow('Could not load:' + Settings.SchemeFile, llFatal);
    SwitchToScreen(sMainScreen); // Wir schmeißen uns wieder Raus, da hier offensichtlich was nicht geklappt hat
    LogLeave;
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
  (*
   * Da der Server das nicht mehr zu uns zurück schickt muss der passende Screen dazu hier "händisch" gesetzt werden.
   *)
  TPlayerSetupMenu(fscreens[sPlayerSetup]).LoadScheme(fScheme);
  LogLeave;
End;

Procedure TGame.HandleUpdateAvailableFieldList(Const Stream: TMemoryStream);
Var
  i: Integer;
  ahash: uint64;
  s: String;
Begin
  log('TGame.HandleUpdateAvailableFieldList', llTrace);
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
      LogLeave;
      exit;
    End;
  End;
  // Wenn wir bis hier her kamen, dann gibt es keine Verfügbaren Karten mehr
  LogShow('The cutset of available fields over all player is zero, at least one player has to leafe the game to be able to play.', llError);
  LogLeave;
End;

Procedure TGame.HandleUpdateFieldSetup(Const Stream: TMemoryStream);
Var
  FieldName: String;
  FieldHash: UInt64;
  Wins, i: INteger;
Begin
  log('TGame.HandleUpdateFieldSetup', llTrace);
  FieldName := Stream.ReadAnsiString;
  FieldHash := 0;
  Wins := 0;
  fActualField := Nil;
  stream.Read(FieldHash, SizeOf(FieldHash));
  stream.Read(Wins, SizeOf(Wins));
  // First try exact match (name and hash)
  For i := 0 To high(fFields) Do Begin
    If (CompareText(fFields[i].Name, FieldName) = 0)
      And (fFields[i].Hash = FieldHash) Then Begin
      fActualField := fFields[i];
      TFieldSetupMenu(fScreens[sEditFieldSetup]).ActualField := fActualField;
      TFieldSetupMenu(fScreens[sEditFieldSetup]).LastWinsToWinMatch := wins;
      Settings.LastWinsToWinMatch := wins;
      LogLeave;
      exit;
    End;
  End;
  // If exact match not found, try name-only match (for cross-platform compatibility)
  // This handles cases where Windows and Mac have different file versions but same field name
  For i := 0 To high(fFields) Do Begin
    If CompareText(fFields[i].Name, FieldName) = 0 Then Begin
      // Name matches but hash differs - accept for cross-platform compatibility
      log(format('Field name match but hash mismatch (using local field): Server "%s" (hash: %d) vs Local "%s" (hash: %d)', 
        [FieldName, FieldHash, fFields[i].Name, fFields[i].Hash]), llWarning);
      fActualField := fFields[i];
      TFieldSetupMenu(fScreens[sEditFieldSetup]).ActualField := fActualField;
      TFieldSetupMenu(fScreens[sEditFieldSetup]).LastWinsToWinMatch := wins;
      Settings.LastWinsToWinMatch := wins;
      LogLeave;
      exit;
    End;
  End;
  // Der Spieler fliegt raus, er kann offensichtlich nicht mit spielen
  // Eigentlich darf dieser Code hier aber nie kommen, weil der Master
  // Nicht weiterschalten kann wenn nicht alle Spieler die Karten haben ..
  LogShow('Error, could not find: ' + FieldName, llFatal);
  SwitchToScreen(sMainScreen);
  LogLeave;
End;

Procedure TGame.HandleLoadSettings(Const Stream: TMemoryStream);
Begin
  log('TGame.HandleSettingsFromStream', llTrace);
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
    logleave;
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
End;

Procedure TGame.HandleStartGame;
Var
  j: TAtomicKey;
  i: Integer;
Begin
  // Für das HartBeating und zum schnelleren Zugriff auf die Player.Infos ..
  fHurry.Enabled := false;
  fPlayerIndex[ks0] := -1;
  fPlayerIndex[ks1] := -1;
  fPlayerIndex[ksJoy1] := -1;
  fPlayerIndex[ksJoy2] := -1;
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
  fLastKeyDown[akFirstAction] := 0;
  fLastKeyDown[akSecondAction] := 0;
  // Transfer menu blocking to game blocking if menu input was blocked (button still pressed)
  // Otherwise set game blocking anyway to be safe
  If fBlockMenuInputUntilRelease Then Begin
    // Button was pressed in menu and is still blocked - transfer to game blocking
    fBlockGameInputUntilRelease := true;
    fBlockMenuInputUntilRelease := false;
  End Else Begin
    // Set game blocking anyway to prevent any stray presses
    fBlockGameInputUntilRelease := true;
  End;
  fActualField.Reset;
  // TODO: What ever da noch alles so fehlt
  StartPlayingSong(fActualField.Sound);
  HandleSetPause(false); // Pause auf jeden Fall, aus
  fPlayingTime_s := 0;
  
  // Reset interpolation snapshots when game starts
  fServerSnapshots[0].Valid := False;
  fServerSnapshots[0].Timestamp := 0;
  fServerSnapshots[1].Valid := False;
  fServerSnapshots[1].Timestamp := 0;
  fCurrentSnapshot := 0;
  fInterpolatedState.PlayingTime_ms := 0;
  
  // Reset MAX stats for new game
  fDebugStats.MaxRTT := 0;
  fDebugStats.MaxSnapAge := 0;
  fDebugStats.MaxInterpFactor := 0;
  
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
  CurrentTime, TimeSinceLastUpdate: QWord;
  PacketSize: Int64;
Begin
  CurrentTime := GetTickCount64;
  PacketSize := Stream.Size;
  
  // Measure time since last update (for debug stats only, no logging)
  If fLastUpdateTimestamp > 0 Then Begin
    TimeSinceLastUpdate := CurrentTime - fLastUpdateTimestamp;
    // Removed logging - was slowing down client on older machines
  End;
  // Calculate RTT for debug stats
  If fLastUpdateTimestamp > 0 Then Begin
    fDebugStats.LastRTT := CurrentTime - fLastUpdateTimestamp;
    If fDebugStats.AvgRTT = 0 Then
      fDebugStats.AvgRTT := fDebugStats.LastRTT
    Else
      fDebugStats.AvgRTT := fDebugStats.AvgRTT * 0.9 + fDebugStats.LastRTT * 0.1; // Running average
    // Track MAX RTT
    If fDebugStats.LastRTT > fDebugStats.MaxRTT Then
      fDebugStats.MaxRTT := fDebugStats.LastRTT;
  End;
  fLastUpdateTimestamp := CurrentTime;
  
  // === NEW: Store data in snapshot instead of directly in fPlayingTime_s ===
  // Switch to next snapshot buffer
  fCurrentSnapshot := 1 - fCurrentSnapshot;
  
  // Read server data into NEW snapshot
  stream.Read(fPlayingTime_s, sizeof(fPlayingTime_s)); // Read temporarily
  fServerSnapshots[fCurrentSnapshot].ServerTime_ms := fPlayingTime_s * 1000; // Convert to ms
  fServerSnapshots[fCurrentSnapshot].Timestamp := CurrentTime;
  fServerSnapshots[fCurrentSnapshot].Valid := True;
  
  // Read player positions into snapshot
  For i := 0 To high(fPlayer) Do Begin
    OldValue := fPlayer[i].Info.Value;
    OldAnim := fPlayer[i].Info.Animation;
    Stream.Read(fServerSnapshots[fCurrentSnapshot].PlayerInfos[i], sizeof(TAtomicInfo));
    
    // === IMPORTANT: Only copy NON-POSITION data to fPlayer for Edge detection ===
    // DO NOT copy Position - that's handled by interpolation!
    fPlayer[i].Info.Animation := fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Animation;
    fPlayer[i].Info.Alive := fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Alive;
    fPlayer[i].Info.Dying := fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Dying;
    fPlayer[i].Info.Direction := fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Direction;
    fPlayer[i].Info.Counter := fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Counter;
    fPlayer[i].Info.Value := fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Value;
    fPlayer[i].Info.ColorIndex := fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].ColorIndex;
    // Position is NOT copied here - it's interpolated in InterpolateGameState()!
    
    (*
     * Der Server sendet die OneTimeAnimations genau 1 mal
     * Wenn der Server aber schneller neu Sendet als der CLient Rendert
     * dann würde der Client diese Flanke beim Rendern gar nicht berücksichtigen
     * -> Hier ein Or und im Rendern dann das False !
     *)
    fPlayer[i].Edge := (fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Animation In OneTimeAnimations) And 
                       (OldAnim <> fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Animation);
    (*
     * Wenn eine neue Animation kommt, stellen wir hier Sicher, dass die neue die alte
     * überschreiben kann z.B. "Zen" -> Die
     * Die Zen Animation, kann zusätzlich aber auch vom Laufen unterbrochen werden, da
     * diese ja nur kommt wenn der Spieler AtomicZenTime lang keine Eingaben gemacht hat.
     *)
    If (fPlayer[i].Edge) Or (((OldAnim = raZen) Or (oldAnim = raLockedIn)) And 
       (fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Animation = raWalk)) Then Begin
      OldAnim := raStandStill; // -- Egal es soll ja nur die OldAnim In OneTimeAnimations verhindert werden
    End;
    (*
     * Wenn die Alte Animation eine einmal ablaufen Animation ist.
     *)
    If OldAnim In OneTimeAnimations Then Begin
      If fServerSnapshots[fCurrentSnapshot].PlayerInfos[i].Counter < fAtomics[0].GetAnimTimeInMs(OldAnim, OldValue) Then Begin
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
  End;
End;

Procedure TGame.HandleSetPause(Value: Boolean);
Var
  ak: TAtomicKey;
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
      If fPlayerIndex[ks0] <> -1 Then fPlayer[fPlayerIndex[ks0]].KeysPressed[ak] := false;
      If fPlayerIndex[ks1] <> -1 Then fPlayer[fPlayerIndex[ks1]].KeysPressed[ak] := false;
      If fPlayerIndex[ksJoy1] <> -1 Then fPlayer[fPlayerIndex[ksJoy1]].KeysPressed[ak] := false;
      If fPlayerIndex[ksJoy2] <> -1 Then fPlayer[fPlayerIndex[ksJoy2]].KeysPressed[ak] := false;
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
  i: integer;
Begin
  log('TGame.HandleUpdatePlayerStatistik', lltrace);
  For i := 0 To high(fPLayer) Do Begin
    Stream.Read(fPLayer[i].Score, sizeof(fPLayer[i].Score));
    Stream.Read(fPLayer[i].Kills, sizeof(fPLayer[i].Kills));
  End;
  LogLeave;
End;

Procedure TGame.HandlePlaySoundEffekt(Const Stream: TMemoryStream);
Var
  se: TSoundEffect;
  s, soundFile: String;
Begin
  se := seNone;
  stream.Read(se, SizeOf(TSoundEffect));
  soundFile := '';
  Case se Of
    seNone: Begin // Nix
      End;
    seBombDrop: soundFile := SelectRandomSound(BombDrops);
    seBombKick: soundFile := SelectRandomSound(BombKick);
    seBombStop: soundFile := SelectRandomSound(BombStop);
    seBombJelly: soundFile := SelectRandomSound(BombJelly);
    seBombBounce: soundFile := SelectRandomSound(BombBounce);
    seBombGrab: soundFile := SelectRandomSound(BombGrab);
    seBombPunch: soundFile := SelectRandomSound(BombPunch);
    seBombExplode: soundFile := SelectRandomSound(BombExplode);
    seAtomicDie: soundFile := SelectRandomSound(AtomicDie);
    seWinner: soundFile := SelectRandomSound(Winner);
    seGetGoodPowerUp: soundFile := SelectRandomSound(GetGoodPowerUp);
    seGetBadPowerUp: soundFile := SelectRandomSound(GetBadPowerUp);
    seZen: soundFile := SelectRandomSound(AtomicZen);
    seOtherPlayerDied: soundFile := SelectRandomSound(OtherPlayerDie);
    seHurryBrick: soundFile := SelectRandomSound(HurryBrick);
    seHurry: soundFile := SelectRandomSound(Hurry);
    seWrapHohle: soundFile := SelectRandomSound(AtomicWrapHole);
    seTrampoline: soundFile := SelectRandomSound(AtomicJump);
  End;
  If soundFile <> '' Then Begin
    If fDataPath = '' Then Begin
      fDataPath := ResolveResourceBase(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));
      log('fDataPath was empty, resolved to: ' + fDataPath, llInfo);
    End;
    s := fDataPath + 'sounds' + PathDelim + soundFile;
    log('Sound effect path: ' + s, llTrace);
    If Not FileExistsUTF8(s) Then Begin
      log('Sound effect file not found: ' + s + ', trying to re-resolve data path', llWarning);
      // Try to re-resolve data path
      fDataPath := ResolveResourceBase(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));
      s := fDataPath + 'sounds' + PathDelim + soundFile;
      log('Retrying with resolved path: ' + s, llInfo);
      If Not FileExistsUTF8(s) Then Begin
        log('Sound effect file still not found: ' + s, llError);
      End;
    End;
    PlaySoundEffect(s);
  End;
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

Procedure TGame.HandleHurryUp();
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
Begin
  log('TGame.HandleUpdateFieldSetup', llTrace);
  Settings.LastPlayedField := FieldName;
  Settings.LastPlayedFieldHash := FieldHash;
  Settings.LastWinsToWinMatch := NeededWins;
  m := TMemoryStream.Create;
  m.WriteAnsiString(FieldName);
  m.write(FieldHash, sizeof(FieldHash));
  m.Write(NeededWins, SizeOf(NeededWins));
  SendChunk(miUpdateFieldSetup, m);
  LogLeave;
End;

Procedure TGame.UpdateWinsToWin(Delta: Integer);
Var
  PrevVal: integer;
Begin
  log('TGame.UpdateWinsToWin', llTrace);
  PrevVal := Settings.LastWinsToWinMatch;
  Settings.LastWinsToWinMatch := max(1, Settings.LastWinsToWinMatch + Delta);
  fBackupSettings.LastWinsToWinMatch := Settings.LastWinsToWinMatch;
  If PrevVal <> Settings.LastWinsToWinMatch Then Begin
    TFieldSetupMenu(fScreens[sEditFieldSetup]).LastWinsToWinMatch := Settings.LastWinsToWinMatch;
    UpdateFieldSetup(Settings.LastPlayedField, Settings.LastPlayedFieldHash, Settings.LastWinsToWinMatch);
  End;
  LogLeave;
End;

Procedure TGame.UpdateSelectedField(Delta: Integer);
Var
  i, index, OldIndex: integer;
Begin
  log('TGame.UpdateSelectedField', llTrace);
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
  LogLeave;
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
  i: Integer;
Begin
  log('TGame.LoadScheme: ' + SchemeFileName, llTrace);
  result := false;
  If fDataPath = '' Then Begin
    fDataPath := ResolveResourceBase(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));
  End;
  fn := fDataPath + 'schemes' + PathDelim + SchemeFileName;
  If Not FileExistsUTF8(fn) Then Begin
    logshow('Error, could not find scheme file:' + SchemeFileName, llCritical);
    LogLeave;
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
      LogLeave;
      exit;
    End;
  End;
  sl.free;
  result := true;
  LogLeave;
End;

Procedure TGame.OnQuitGameSoundFinished(Sender: TObject);
Begin
  If NeedFormClose Then Begin
    form1.Close;
  End;
End;

Procedure TGame.RenderPlayerbyInfo(Const Info: TAtomicInfo; Edge: Boolean);
Begin
  fAtomics[info.ColorIndex].Render(Info, Edge);
End;

Procedure TGame.RenderFieldHeader;
Var
  x, y, i: Integer;
  s: String;
Begin
  glPushMatrix;
  glTranslatef(0, 0, atomic_dialog_Layer + atomic_EPSILON);
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
      s := format('S:%d K:%d', [fPlayer[i].Score, fPlayer[i].Kills]);
      x := (i Div 2) * 100 + 10;
      y := (i Mod 2) * 16 + 10;
      glBindTexture(GL_TEXTURE_2D, 0);
      AtomicFont.Textout(x, y, s);
      If Not fPlayer[i].Info.Alive Then Begin
        glColor4f(1, 1, 1, 1);
        glPushMatrix;
        glTranslatef(x - 4, y - 4, atomic_EPSILON);
        glAlphaFunc(GL_LESS, 0.5);
        glEnable(GL_ALPHA_TEST);
        RenderAlphaQuad(0, 0, fPlayerdeadTex);
        gldisable(GL_ALPHA_TEST);
        glPopMatrix;
      End;
    End;
  End;
  AtomicFont.BackColor := clBlack; // Auf Jeden Fall die BackColor wieder Resetten
  (*
   * Die Verbleibende Spielzeit
   * === CHANGED: Use interpolated time instead of raw server time ===
   *)
  glColor3f(1, 1, 1);
  AtomicBigFont.Textout(500, 10, fInterpolatedState.PlayingTime_ms div 1000);
  glPopMatrix;
End;

Procedure TGame.RenderBombs;
Var
  i: Integer;
  ani: TAnimation;
Begin
  glPushMatrix;
  glColor4f(1, 1, 1, 1);
  glAlphaFunc(GL_LESS, 0.5);
  glEnable(GL_ALPHA_TEST);
  glTranslatef(0, 0, atomic_Bomb_Layer);
  For i := 0 To fBombCount - 1 Do Begin
    glPushMatrix;
    glTranslatef(Fieldxoff + fBombs[i].Position.x * FieldBlockWidth, FieldyOff + fBombs[i].Position.y * FieldBlockHeight, 0);
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
    glTranslatef(ani.OffsetX, ani.OffsetY, 0);
    ani.ani.Render(0);
    glPopMatrix;
  End;
  gldisable(GL_ALPHA_TEST);
  glPopMatrix;
End;

Procedure TGame.PingForOpenGames;
Var
  N: TNetworkAdapterList;
  i: Integer;
  s: String;
Begin
  If fNeedDisconnect Then exit;
  If Not fUDPPingData.Active Then exit;
  If GetTickCount64 - fUDPPingData.LastTickValue >= 500 Then Begin
    fUDPPingData.LastTickValue := GetTickCount64;
    log('TGame.PingForOpenGames', llTrace);
    If assigned(fUDPPingData.Connection) Then Begin
      (*
       * Sollte warum auch immer der UDP Down sein, versuchen wir ihn neu zu starten ..
       *)
      If Not fUDPPingData.Connection.Connected Then Begin
        If Not fUDPPingData.Connection.Connect('', UDPPingPort) Then Begin
          LogShow('Error, could not start UDP Server.', llError);
          SwitchToScreen(sMainScreen);
          LogLeave;
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
    LogLeave;
  End;
End;

Procedure TGame.JoinViaParams(ip: String; Port: integer);
Begin
  (*
   * Beim Verbinden via Parameter müssen wir die
   * SwitchToScreen umgehen und passend initialisieren das die Spielengine genau gleich
   * initialisiert ist wie sie das via "Join" gewesen wäre ;)
   *)
  log(format('TGame.JoinViaParams: %s:%d', [ip, port]), lltrace);
  Settings.Router_IP := ip;
  Settings.Router_Port := inttostr(Port);
  fBackupSettings.Router_IP := ip;
  fBackupSettings.Router_Port := inttostr(Port);
  fActualScreen := fScreens[sJoinNetwork];
  fActualScreen.reset;
  fParamJoinIP := ip;
  fParamJoinPort := port;
  Join(ip, port);
  logleave;
End;

Procedure TGame.StartGame;
Begin
  SendChunk(miStartGame, Nil);
End;

Procedure TGame.StartPingingForGames;
Begin
  log('TGame.StartPingingForGames', lltrace);
  fUDPPingData.Active := true;
  fUDPPingData.LastTickValue := GetTickCount64 - 500;
  logleave;
End;

Procedure TGame.StopPingingForGames;
Begin
  fUDPPingData.Active := false;
End;

Procedure TGame.Join(IP: String; Port: Integer);
Var
  retryCount: Integer;
  maxRetries: Integer;
  retryDelay: Integer;
  isLocalhost: Boolean;
Begin
  log('TGame.Join', lltrace);
  log(format('Joining to %s on port %d as %s', [ip, port, Settings.NodeName]));
  
  // Check if connecting to localhost (after starting server)
  isLocalhost := (ip = '127.0.0.1') Or (ip = 'localhost');
  
  If isLocalhost Then Begin
    // For localhost, retry more times (server might still be starting)
    // Server needs time to: 1) Start .app bundle, 2) Initialize, 3) Start listening on port
    maxRetries := 10;
    retryDelay := 1000; // 1000ms (1 second) between retries - server needs time to start
  End
  Else Begin
    // For remote servers, try only once
    maxRetries := 1;
    retryDelay := 0;
  End;
  
  retryCount := 0;
  While retryCount < maxRetries Do Begin
    If fChunkManager.Connect(ip, port) Then Begin
      log('Successfully connected to server', llInfo);
      logleave;
      exit;
    End;
    
    Inc(retryCount);
    If retryCount < maxRetries Then Begin
      log(format('Connection attempt %d/%d failed, retrying in %dms...', [retryCount, maxRetries, retryDelay]), llInfo);
      Sleep(retryDelay);
    End;
  End;
  
  // All retries failed
  LogShow('Error could not connect to server after ' + inttostr(maxRetries) + ' attempts', llError);
  SwitchToScreen(sMainScreen);
  logleave;
End;

Constructor TGame.Create;
Begin
  Inherited Create;
  fsdl_Loaded := false;
  fsdlJoysticks[ks0] := Nil;
  fsdlJoysticks[ks1] := Nil;
  fControllerLogged[ks0] := false;
  fControllerLogged[ks1] := false;
  fLastUpdateTimestamp := 0; // Initialize network performance monitoring
  
  // Initialize interpolation
  fServerSnapshots[0].Valid := False;
  fServerSnapshots[0].Timestamp := 0;
  fServerSnapshots[1].Valid := False;
  fServerSnapshots[1].Timestamp := 0;
  fCurrentSnapshot := 0;
  fInterpolationDelay := 0;     // 0ms delay - interpolate between last 2 snapshots immediately
  fExtrapolationLimit := 50;    // Max 50ms extrapolation (reduced from 100ms)
  fDebugStats.LastRTT := 0;
  fDebugStats.AvgRTT := 0;
  fDebugStats.MaxRTT := 0;
  fDebugStats.InterpolationFactor := 0;
  fDebugStats.MaxInterpFactor := 0;
  fDebugStats.IsExtrapolating := False;
  fDebugStats.SnapshotAge := 0;
  fDebugStats.MaxSnapAge := 0;
  
  fMenuDpadState.up := false;
  fMenuDpadState.down := false;
  fMenuDpadState.left := false;
  fMenuDpadState.right := false;
  fMenuDpadState.buttonA := false;
  fMenuDpadState.buttonB := false;
  fBlockGameInputUntilRelease := false;
  fBlockMenuInputUntilRelease := false;
  fSoundManager := TSoundManager.Create();
  fSoundInfo := TSoundInfo.Create();
  fParamJoinIP := '';
  fPause := false;
  fNeedDisconnect := false;
  fPlayerIndex[ks0] := -1;
  fPlayerIndex[ks1] := -1;
  fPlayerIndex[ksJoy1] := -1;
  fPlayerIndex[ksJoy2] := -1;
  fInitialized := false;
  fActualScreen := Nil;
  fChunkManager := TChunkManager.create;
  fUDPPingData.Connection := Nil;
  fUDPPingData.Active := false;
  fLastIdleTick := GetTickCount64;
  OnNeedHideCursor := Nil;
  OnNeedShowCursor := Nil;
  FViewportOffsetX := 0;
  FViewportOffsetY := 0;
  FViewportWidth := GameWidth;
  FViewportHeight := GameHeight;
  FControlWidth := GameWidth;
  FControlHeight := GameHeight;
  FViewportScale := 1;
  fDataPath := ''; // Will be set in Initialize
End;

Destructor TGame.Destroy;
Var
  i: TScreenEnum;
  j: Integer;
Begin
  If assigned(fsdlJoysticks[ks0]) Then fsdlJoysticks[ks0].free;
  If assigned(fsdlJoysticks[ks1]) Then fsdlJoysticks[ks1].free;
  If assigned(fsdlJoysticks[ksJoy1]) Then fsdlJoysticks[ksJoy1].free;
  If assigned(fsdlJoysticks[ksJoy2]) Then fsdlJoysticks[ksJoy2].free;
  fsdlJoysticks[ks0] := Nil;
  fsdlJoysticks[ks1] := Nil;
  fsdlJoysticks[ksJoy1] := Nil;
  fsdlJoysticks[ksJoy2] := Nil;
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
  setlength(fFields, 0);
  DoDisconnect();
  fChunkManager.free;
  fChunkManager := Nil;
End;

Procedure TGame.RegisterTCPConnection(Const Connection: TLTCPComponent);
Begin
  log('TGame.RegisterTCPConnection', llTrace);
  Connection.OnConnect := @Connection_Connect;
  Connection.OnDisconnect := @Connection_Disconnect;
  Connection.OnError := @Connection_Error;
  fChunkManager.OnReceivedChunk := @OnReceivedChunk;
  fChunkManager.RegisterConnection(Connection);
  // Start network thread for non-blocking network processing
  fChunkManager.StartNetworkThread();
  log('Network thread started for client', llInfo);
  LogLeave;
End;

Procedure TGame.RegisterUDPConnection(Const Connection: TLUDPComponent);
Begin
  log('TGame.RegisterUDPConnection', llTrace);
  fUDPPingData.Connection := Connection;
  StopPingingForGames();
  If assigned(Connection) Then Begin
    If fUDPPingData.Connection.Connected Then fUDPPingData.Connection.Disconnect();
    fUDPPingData.Connection.OnReceive := @OnUDPConnection_Receive;
    fUDPPingData.Connection.OnError := @OnUDPConnection_Error;
    If Not fUDPPingData.Connection.Connect('', UDPPingPort) Then Begin
      LogShow('Error, could not start UDP Server.', llError);
    End;
  End;
  LogLeave;
End;

Procedure TGame.Initialize(Const Owner: TOpenGLControl);
Var
  Loader: TLoaderDialog;
  p: String;
  i: TScreenEnum;
  sl: TStringList;
  j: integer;
  field: TAtomicRandomField;
  hohletex: TGraphikItem;
  fTrampStatic: Integer; // Wenn das Trampolin gerade nicht "an wackelt"
{$IFDEF ShowInitTime}
  t: UInt64;

  Procedure TimePoint(tp: Integer);
  Var
    d, n: UInt64;
  Begin
    n := GetTickCount64;
    d := n - t;
    t := n;
    writeln(format('%d: %d', [tp, d]));
  End;
{$ENDIF}
Begin
{$IFDEF ShowInitTime}
  t := GetTickCount64;
{$ENDIF}
  uearlylog.EarlyLog('TGame.Initialize: Starting');
  uearlylog.EarlyLog('TGame.Initialize: Owner assigned: ' + BoolToStr(Assigned(Owner), true));
  log('TGame.Initialize', lltrace);
  log('TGame.Initialize: Owner assigned: ' + BoolToStr(Assigned(Owner), true), llInfo);
  fOwner := Owner;
  
  // Resolve data path BEFORE creating Loader dialog (Loader needs it)
  p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  uearlylog.EarlyLog('TGame.Initialize: Assets root: ' + p);
  // Resolve data directory path - check multiple locations for .app bundle compatibility
  fDataPath := ResolveResourceBase(p);
  uearlylog.EarlyLog('TGame.Initialize: Data path: ' + fDataPath);
  uearlylog.EarlyLog('TGame.Initialize: Data path exists: ' + BoolToStr(DirectoryExists(fDataPath), true));
  log('Assets root: ' + p, llInfo);
  log('Data path: ' + fDataPath, llInfo);
  log('Data path exists: ' + BoolToStr(DirectoryExists(fDataPath), true), llInfo);
  Try
    uearlylog.EarlyLog('TGame.Initialize: Creating TLoaderDialog...');
    Loader := TLoaderDialog.create(Owner, fDataPath);
    fLoaderDialog := Loader; // Store reference for OnPaint
    uearlylog.EarlyLog('TGame.Initialize: TLoaderDialog created successfully');
    log('TLoaderDialog created successfully', llInfo);
  Except
    On E: Exception Do Begin
      uearlylog.EarlyLog('ERROR creating TLoaderDialog: ' + E.Message);
      uearlylog.EarlyLog('Exception class: ' + E.ClassName);
      log('ERROR creating TLoaderDialog: ' + E.Message, llError);
      log('Exception class: ' + E.ClassName, llError);
      Raise;
    End;
  End;
  (*
   * Lade Prozente
   * 0..10 : Screens
   * 11..50: Fields
   * 51..100: Atomic's
   *)
  Loader.Percent := 0;
  Loader.Render(); // So schnell wie Möglich dem User mal Anzeigen, dass wir dran sind
{$IFDEF ShowInitTime}
  TimePoint(1);
{$ENDIF}
  fSoundInfo.Musik := Settings.PlaySounds;
  fSoundInfo.Volume := Settings.VolumeValue;
  If Not fSoundManager.SetVolumeValue(Settings.VolumeValue) Then Begin
    log('Could not adjust sound volume.', llError);
    logleave;
    exit;
  End;
  FOnMouseMoveCapture := Owner.OnMousemove;
  Owner.OnMousemove := @FOnMouseMove;
  FOnMouseDownCapture := Owner.OnMouseDown;
  Owner.OnMouseDown := @FOnMouseDown;
  FOnMouseupCapture := Owner.OnMouseup;
  Owner.OnMouseup := @FOnMouseup;
  FOnDblClickCapture := Owner.OnDblClick;
  Owner.OnDblClick := @FOnDblClick;
  FOnKeyDownCapture := Owner.OnKeyDown;
  Owner.OnKeyDown := @FOnKeyDown;
  FOnKeyUpCapture := Owner.OnKeyUp;
  Owner.OnKeyUp := @FOnKeyUp;

  AtomicBigFont.CreateFont(fDataPath + 'res' + PathDelim);

  // Laden aller Screens
  fScreens[sMainScreen] := TMainMenu.Create(self);
  fScreens[sHost] := Nil;
  fScreens[sJoinNetwork] := TJoinMenu.Create(self);
  fScreens[sPlayerSetupRequest] := Nil;
  fScreens[sPlayerSetup] := TPlayerSetupMenu.Create(self);
  fScreens[sEditFieldSetupRequest] := Nil;
  Loader.Percent := 5;
  Loader.Render();
  fScreens[sEditFieldSetup] := TFieldSetupMenu.Create(self);
  fScreens[sDrawGame] := TDrawGameMenu.create(self);
  fScreens[sVictory] := TVictoryMenu.create(self);
  fScreens[sMatchStatistik] := TMatchStatistikMenu.create(self);
  fScreens[sOptions] := TOptionsMenu.Create(self);
  fScreens[sExitBomberman] := Nil;

  log('Loading UI screens from ' + fDataPath + 'res', llInfo);
  For i In TScreenEnum Do Begin
    If Not assigned(fScreens[i]) Then Continue;
    fScreens[i].LoadFromDisk(fDataPath + 'res' + PathDelim);
  End;
  Loader.Percent := 10;
  Loader.Render();
{$IFDEF ShowInitTime}
  TimePoint(2);
{$ENDIF}

  fPowerUpsTex[puNone] := 0; // Das Gibts ja net -> Weg
  fPowerUpsTex[puExtraBomb] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powbomb.png', smStretchHard);
  If fPowerUpsTex[puExtraBomb] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powbomb.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puLongerFlameLength] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powflame.png', smStretchHard);
  If fPowerUpsTex[puLongerFlameLength] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powflame.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puDisease] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powdisea.png', smStretchHard);
  If fPowerUpsTex[puDisease] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powdisea.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puCanCick] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powkick.png', smStretchHard);
  If fPowerUpsTex[puCanCick] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powkick.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puExtraSpeed] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powskate.png', smStretchHard);
  If fPowerUpsTex[puExtraSpeed] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powskate.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puCanPunch] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powpunch.png', smStretchHard);
  If fPowerUpsTex[puCanPunch] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powpunch.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puCanGrab] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powgrab.png', smStretchHard);
  If fPowerUpsTex[puCanGrab] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powgrab.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puCanSpooger] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powspoog.png', smStretchHard);
  If fPowerUpsTex[puCanSpooger] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powspoog.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puGoldFlame] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powgold.png', smStretchHard);
  If fPowerUpsTex[puGoldFlame] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powgold.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puTrigger] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powtrig.png', smStretchHard);
  If fPowerUpsTex[puTrigger] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powtrig.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puCanJelly] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powjelly.png', smStretchHard);
  If fPowerUpsTex[puCanJelly] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powjelly.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puSuperBadDisease] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powebola.png', smStretchHard);
  If fPowerUpsTex[puSuperBadDisease] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powebola.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[puSlow] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powslow.png', smStretchHard);
  If fPowerUpsTex[puSlow] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powslow.png', llError);
    logleave;
    exit;
  End;
  fPowerUpsTex[purandom] := OpenGL_GraphikEngine.LoadGraphik(fDataPath + 'res' + PathDelim + 'powrand.png', smStretchHard);
  If fPowerUpsTex[purandom] = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'powrand.png', llError);
    logleave;
    exit;
  End;
  // Load PlayerDead Tex correct.
  fPlayerdeadTex.Image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(fDataPath + 'res' + PathDelim + 'playerdead.png', ugraphics.ColorToRGB(clfuchsia), smClamp);
  fPlayerdeadTex := OpenGL_GraphikEngine.FindItem(fDataPath + 'res' + PathDelim + 'playerdead.png');
  If fPlayerdeadTex.Image = 0 Then Begin
    log('Failed to locate texture handle for playerdead.png', llError);
    logleave;
    exit;
  End;
  fhurry.Texture.Image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(fDataPath + 'res' + PathDelim + 'hurry.png', ugraphics.ColorToRGB(clfuchsia), smClamp);
  If fhurry.Texture.Image = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'hurry.png', llError);
    logleave;
    exit;
  End;
  fhurry.Texture := OpenGL_GraphikEngine.FindItem(fDataPath + 'res' + PathDelim + 'hurry.png');
  hohletex.image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(fDataPath + 'res' + PathDelim + 'hole.png', ugraphics.ColorToRGB(clfuchsia), smStretchHard);
  If hohletex.Image = 0 Then Begin
    log('Failed to load texture ' + fDataPath + 'res' + PathDelim + 'hole.png', llError);
    logleave;
    exit;
  End;
  hohletex := OpenGL_GraphikEngine.FindItem(fDataPath + 'res' + PathDelim + 'hole.png');
{$IFDEF ShowInitTime}
  TimePoint(3);
{$ENDIF}

  // Laden der Felder
  fArrows := TOpenGL_Animation.Create;
  If Not fArrows.LoadFromFile(fDataPath + 'res' + PathDelim + 'arrows.ani', true) Then Begin
    log('Failed to load animation ' + fDataPath + 'res' + PathDelim + 'arrows.ani', llError);
    logleave;
    Exit;
  End;
  fConveyors := TOpenGL_Animation.Create;
  If Not fConveyors.LoadFromFile(fDataPath + 'res' + PathDelim + 'conveyor.ani', true) Then Begin
    log('Failed to load animation ' + fDataPath + 'res' + PathDelim + 'conveyor.ani', llError);
    logleave;
    Exit;
  End;
  fTramp := TOpenGL_Animation.Create;
  If Not fTramp.LoadFromFile(fDataPath + 'res' + PathDelim + 'tramp.ani', true) Then Begin
    log('Failed to load animation ' + fDataPath + 'res' + PathDelim + 'tramp.ani', llError);
    logleave;
    Exit;
  End;
{$IFDEF ShowInitTime}
  TimePoint(4);
{$ENDIF}

  (*
   * Da es Sich bewegende Trampoline gibt, und "Statische" müssen wir ein extra Sprite mit einem "Statischen" anlegen,
   * das nachher gerendert werden kann, via fTramp.GetFirstBitmap() geht es nicht, da da sonst Rundungsfehler entstehen die man sieht :/
   *)
  fTrampStatic := OpenGL_SpriteEngine.AddSprite(
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
  log('Scanning map directories in ' + fDataPath + 'maps', llInfo);
  sl := FindAllDirectories(fDataPath + 'maps', false);
  sl.Sorted := true;
  sl.Sort;
{$IFDEF Only1Map}
  While sl.Count > 1 Do Begin
    sl.Delete(sl.Count - 1);
  End;
{$ENDIF}
  setlength(fFields, sl.Count);
  If sl.count = 0 Then Begin
    LogShow('Error, no fields to load found in ' + fDataPath + 'maps', llFatal);
    LogLeave;
    exit;
  End;
  log('Found ' + inttostr(sl.Count) + ' map directories', llInfo);
{$IFDEF ShowInitTime}
  TimePoint(6);
{$ENDIF}
  For j := 0 To sl.count - 1 Do Begin
    fFields[j] := TAtomicField.Create();
    If Not fFields[j].loadFromDirectory(sl[j], fArrows, fConveyors, fTramp, hohletex.Image, fTrampStatic) Then Begin
      LogShow('Error, unable to load field: ' + sl[j], llFatal);
      LogLeave;
      exit;
    End;
    Loader.Percent := 10 + round(40 * j / sl.count);
    Loader.Render();
{$IFDEF ShowInitTime}
    TimePoint(7 + j);
{$ENDIF}
  End;
  sl.free;
  // Anfügen der Random Karte
  field := TAtomicRandomField.Create(); // Die initialisiert sich bereits richtig ;)
  field.CreatePreview(FFields);

  setlength(fFields, length(fFields) + 1); // Die Random Karte MUSS immer die Letzte sein (siehe auch TGame.HandleUpdateAvailableFieldList)
  fFields[high(fFields)] := field;
{$IFDEF ShowInitTime}
  TimePoint(20);
{$ENDIF}
  // Laden aller Atomic's in ihren Farben --> Das ist was Ladetechnisch richtig weh tut, alles andere ist eigentlich "erträglich"
  For j := 0 To high(fAtomics) Do Begin
    Loader.Percent := 50 + round(50 * j / length(fAtomics));
    Loader.Render();
    fAtomics[j] := TAtomic.Create;
    If Not fAtomics[j].InitAsColor(fDataPath + 'atomic' + PathDelim, PlayerColors[j]) Then Begin
      LogShow('Error, unable to load atomic from ' + fDataPath + 'atomic' + PathDelim, llFatal);
      LogLeave;
      exit;
    End;
{$IFDEF ShowInitTime}
    TimePoint(21 + j);
{$ENDIF}
  End;
  Loader.Percent := 100;
  Loader.Render(); // Als letztes kriegt der User zu sehen, dass wir fertig sind :-)
  Loader.free;
  fLoaderDialog := Nil; // Clear reference after initialization
  fInitialized := true;
  SwitchToScreen(sMainScreen);
{$IFDEF ShowInitTime}
  logshow('Initialisation took ' + inttostr((GetTickCount64 - t) Div 1000) + ' seconds.', llInfo);
{$ENDIF}

{$IFDEF Linux}
  If Settings.Fullscreen Then Begin
    form1.SetFullScreen(True);
  End;
{$ENDIF}
{$IFDEF ShowInitTime}
  TimePoint(100);
{$ENDIF}
  LogLeave;
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
Begin
  (*
   * Wird über den Idle Handler aufgerufen und ist damit definitiv außerhalb der LEventer Eventloop
   *)
  fNeedDisconnect := false;
  log('TGame.DoDisconnect', lltrace);
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
  logleave;
End;

// Client-side interpolation implementation
Procedure TGame.InterpolateGameState();
Var
  CurrentTime, RenderTime: QWord;
  OldSnapIndex, NewSnapIndex: Integer;
  T, Extrapolation: Single;
  i: Integer;
  TimeDiff, SnapAge: Int64;
  VelX, VelY: Single;
  PlayerSpeed: Single;
  
  // Linear interpolation
  Function Lerp(A, B, T: Single): Single;
  Begin
    Result := A + (B - A) * T;
  End;
  
  // Interpolate 2D vector
  Function LerpVector2(Const A, B: TVector2; T: Single): TVector2;
  Begin
    Result.x := Lerp(A.x, B.x, T);
    Result.y := Lerp(A.y, B.y, T);
  End;
  
Begin
  CurrentTime := GetTickCount64;
  
  // Get the snapshot indices
  OldSnapIndex := 1 - fCurrentSnapshot;  // Older
  NewSnapIndex := fCurrentSnapshot;      // Newer
  
  // If we don't have 2 valid snapshots yet, use the latest one
  If (Not fServerSnapshots[OldSnapIndex].Valid) Or (Not fServerSnapshots[NewSnapIndex].Valid) Then Begin
    If fServerSnapshots[NewSnapIndex].Valid Then Begin
      fInterpolatedState.PlayingTime_ms := fServerSnapshots[NewSnapIndex].ServerTime_ms;
      fInterpolatedState.PlayerInfos := fServerSnapshots[NewSnapIndex].PlayerInfos;
      fDebugStats.InterpolationFactor := 1.0;
      fDebugStats.IsExtrapolating := False;
      fDebugStats.SnapshotAge := CurrentTime - fServerSnapshots[NewSnapIndex].Timestamp;
    End;
    Exit;
  End;
  
  // Calculate snapshot age
  SnapAge := CurrentTime - fServerSnapshots[NewSnapIndex].Timestamp;
  fDebugStats.SnapshotAge := SnapAge;
  // Track MAX snapshot age
  If SnapAge > fDebugStats.MaxSnapAge Then
    fDebugStats.MaxSnapAge := SnapAge;
  
  // Calculate time since last snapshot (for interpolation/extrapolation)
  TimeDiff := fServerSnapshots[NewSnapIndex].Timestamp - fServerSnapshots[OldSnapIndex].Timestamp;
  
  // If snapshots are too far apart (> 150ms), don't interpolate - just use newest
  If TimeDiff > 150 Then Begin
    fInterpolatedState.PlayingTime_ms := fServerSnapshots[NewSnapIndex].ServerTime_ms;
    fInterpolatedState.PlayerInfos := fServerSnapshots[NewSnapIndex].PlayerInfos;
    fDebugStats.InterpolationFactor := 1.0;
    fDebugStats.IsExtrapolating := False;
    Exit;
  End;
  
  // Calculate interpolation factor based on time between snapshots
  // We want to render at the "latest safe time" which is the newest snapshot + elapsed time
  If TimeDiff > 0 Then Begin
    T := (CurrentTime - fServerSnapshots[OldSnapIndex].Timestamp) / TimeDiff;
  End Else Begin
    T := 1.0;  // Use newest if timestamps are same
  End;
  
  // === INTERPOLATION (T between 0 and 1) ===
  If (T >= 0.0) And (T <= 1.0) Then Begin
    fDebugStats.InterpolationFactor := T;
    fDebugStats.IsExtrapolating := False;
    // Track MAX interpolation factor
    If T > fDebugStats.MaxInterpFactor Then
      fDebugStats.MaxInterpFactor := T;
    
    // Interpolate playing time
    fInterpolatedState.PlayingTime_ms := Round(
      Lerp(fServerSnapshots[OldSnapIndex].ServerTime_ms, fServerSnapshots[NewSnapIndex].ServerTime_ms, T)
    );
    
    // Interpolate player positions
    For i := 0 To 9 Do Begin
      fInterpolatedState.PlayerInfos[i].Position := LerpVector2(
        fServerSnapshots[OldSnapIndex].PlayerInfos[i].Position,
        fServerSnapshots[NewSnapIndex].PlayerInfos[i].Position,
        T
      );
      
      // Don't interpolate these - use newest values
      fInterpolatedState.PlayerInfos[i].Direction := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Direction;
      fInterpolatedState.PlayerInfos[i].Animation := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Animation;
      fInterpolatedState.PlayerInfos[i].Alive := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Alive;
      fInterpolatedState.PlayerInfos[i].Dying := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Dying;
      fInterpolatedState.PlayerInfos[i].Counter := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Counter;
      fInterpolatedState.PlayerInfos[i].Value := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Value;
      fInterpolatedState.PlayerInfos[i].ColorIndex := fServerSnapshots[NewSnapIndex].PlayerInfos[i].ColorIndex;
    End;
  End
  // === EXTRAPOLATION (T > 1.0 - we're ahead of last snapshot) ===
  Else If T > 1.0 Then Begin
    // Calculate how much time has elapsed since last snapshot
    Extrapolation := (CurrentTime - fServerSnapshots[NewSnapIndex].Timestamp) / 1000.0; // in seconds
    
    // Limit extrapolation aggressively to prevent jitter
    If Extrapolation > (fExtrapolationLimit / 1000.0) Then Begin
      Extrapolation := fExtrapolationLimit / 1000.0;
    End;
    
    fDebugStats.InterpolationFactor := T;
    fDebugStats.IsExtrapolating := True;
    // Track MAX interpolation factor (extrapolation)
    If T > fDebugStats.MaxInterpFactor Then
      fDebugStats.MaxInterpFactor := T;
    
    // Extrapolate playing time smoothly
    fInterpolatedState.PlayingTime_ms := fServerSnapshots[NewSnapIndex].ServerTime_ms + Round(Extrapolation * 1000);
    
    // For positions: DON'T extrapolate - just use latest snapshot to avoid jitter!
    // Extrapolation causes "jittery" movement when predictions are wrong
    For i := 0 To 9 Do Begin
      // Just use the latest snapshot position (no prediction)
      fInterpolatedState.PlayerInfos[i].Position := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Position;
      
      // Copy other fields
      fInterpolatedState.PlayerInfos[i].Direction := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Direction;
      fInterpolatedState.PlayerInfos[i].Animation := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Animation;
      fInterpolatedState.PlayerInfos[i].Alive := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Alive;
      fInterpolatedState.PlayerInfos[i].Dying := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Dying;
      fInterpolatedState.PlayerInfos[i].Counter := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Counter;
      fInterpolatedState.PlayerInfos[i].Value := fServerSnapshots[NewSnapIndex].PlayerInfos[i].Value;
      fInterpolatedState.PlayerInfos[i].ColorIndex := fServerSnapshots[NewSnapIndex].PlayerInfos[i].ColorIndex;
    End;
  End
  // === FALLBACK (T < 0 - should not happen, but handle it) ===
  Else Begin
    fDebugStats.InterpolationFactor := 0.0;
    fDebugStats.IsExtrapolating := False;
    fInterpolatedState.PlayingTime_ms := fServerSnapshots[OldSnapIndex].ServerTime_ms;
    fInterpolatedState.PlayerInfos := fServerSnapshots[OldSnapIndex].PlayerInfos;
  End;
End;

Procedure TGame.Render;
Var
  i: Integer;
  n: QWORD;
Begin
  // Viewport is set in OpenGLControl1Resize, don't override it here
  Go2d(GameWidth, GameHeight);
  If Not fInitialized Then Begin
    glBindTexture(GL_TEXTURE_2D, 0);
    OpenGL_ASCII_Font.Color := clred;
    OpenGL_ASCII_Font.Textout(20, 50,
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
        // === NEW: Interpolate game state before rendering ===
        InterpolateGameState();
        
        CheckSDLKeys;
        fActualField.render(fAtomics, fPowerUpsTex);
        RenderBombs;
        For i := 0 To high(fPlayer) Do Begin
          // Use interpolated positions instead of raw server data
          If fInterpolatedState.PlayerInfos[i].Alive Then Begin
            RenderPlayerbyInfo(fInterpolatedState.PlayerInfos[i], fPlayer[i].edge);
            fPlayer[i].edge := false;
          End;
        End;
        If fPause Then Begin
          glPushMatrix();
          glTranslatef(0, 0, atomic_dialog_Layer + atomic_EPSILON);
          glBindTexture(GL_TEXTURE_2D, 0);
          AtomicFont.Color := clYellow;
          AtomicFont.BackColor := clBlack;
          AtomicFont.Textout(320 - 2 * 14, GameHeight Div 2, 'Pause');
          glPopMatrix();
        End;
        RenderFieldHeader();
        // TODO: Auslagern in eine eigene Procedur
        If fHurry.Enabled Then Begin // Wenn An, dann 5 mal Flashen und wieder aus ;)
          n := (GetTickCount64 - fHurry.TimeStamp) Div 500;
          If n Mod 2 = 0 Then Begin
            glPushMatrix();
            glDisable(GL_DEPTH_TEST);
            glColor4f(1, 1, 1, 1);
            glAlphaFunc(GL_LESS, 0.5);
            glEnable(GL_ALPHA_TEST);
            RenderAlphaQuad((GameHeight - fHurry.Texture.OrigHeight) Div 2, (GameWidth - fHurry.Texture.OrigWidth) Div 2, fHurry.Texture);
            gldisable(GL_ALPHA_TEST);
            glEnable(GL_DEPTH_TEST);
            glPopMatrix();
          End;
          If n > 5 Then fHurry.Enabled := false;
        End;
      End;
  End;
  fSoundInfo.Render;
  Exit2d();
End;

Function TGame.PointInsideViewport(ScreenX, ScreenY: Integer): Boolean;
Begin
  If (FViewportWidth <= 0) Or (FViewportHeight <= 0) Then
    exit(true);
  Result :=
    (ScreenX >= FViewportOffsetX) And
    (ScreenX <= FViewportOffsetX + FViewportWidth) And
    (ScreenY >= FViewportOffsetY) And
    (ScreenY <= FViewportOffsetY + FViewportHeight);
End;

Function TGame.ScreenToGameX(ScreenX: Integer): Integer;
Var
  Local: Double;
Begin
  If (FViewportWidth <= 0) Or (FViewportScale <= 0) Then
    exit(ScreenX);
  Local := (ScreenX - FViewportOffsetX) / FViewportScale;
  If Local < 0 Then
    Local := 0
  Else If Local > GameWidth Then
    Local := GameWidth;
  Result := round(Local);
End;

Function TGame.ScreenToGameY(ScreenY: Integer): Integer;
Var
  Local: Double;
Begin
  If (FViewportHeight <= 0) Or (FViewportScale <= 0) Then
    exit(ScreenY);
  Local := (ScreenY - FViewportOffsetY) / FViewportScale;
  If Local < 0 Then
    Local := 0
  Else If Local > GameHeight Then
    Local := GameHeight;
  Result := round(Local);
End;

Procedure TGame.SetViewportMetrics(ControlWidth, ControlHeight, OffsetX,
  OffsetY, ViewportWidth, ViewportHeight: Integer);
Begin
  FControlWidth := ControlWidth;
  FControlHeight := ControlHeight;
  FViewportOffsetX := OffsetX;
  FViewportOffsetY := OffsetY;
  FViewportWidth := ViewportWidth;
  FViewportHeight := ViewportHeight;
  If (ViewportWidth > 0) Then
    FViewportScale := ViewportWidth / GameWidth
  Else
    FViewportScale := 1;
  If FViewportScale <= 0 Then
    FViewportScale := 1;
  log(Format('Viewport metrics: control=%dx%d viewport=%dx%d scale=%.4f offset=(%d,%d)',
    [FControlWidth, FControlHeight, FViewportWidth, FViewportHeight, FViewportScale,
    FViewportOffsetX, FViewportOffsetY]), llInfo);
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
  
  // Always pump SDL events first to update button states
  // Check all possible keysets that might use SDL
  If fsdl_Loaded And (Settings.Keys[ks0].UseSDL2 Or Settings.Keys[ks1].UseSDL2 Or 
                      Settings.Keys[ksJoy1].UseSDL2 Or Settings.Keys[ksJoy2].UseSDL2) Then Begin
    SDL_PumpEvents();
  End;
  
  // Check if we need to unblock game input (waiting for all buttons to be released)
  If fBlockGameInputUntilRelease Then Begin
    If AreAllGamepadButtonsReleased() Then Begin
      fBlockGameInputUntilRelease := false;
    End;
  End;
  
  // Check if we need to unblock menu input (waiting for all buttons to be released)
  If fBlockMenuInputUntilRelease Then Begin
    If AreAllGamepadButtonsReleased() Then Begin
      fBlockMenuInputUntilRelease := false;
    End;
  End;
  
  // Poll controllers/joysticks each idle tick for input (both menu and game)
  CheckSDLKeys();
  // Also poll for menu navigation via gamepad
  CheckSDLKeysForMenu();
End;

Function TGame.ResolveResourceBase(BasePath: String): String;
Var
  testPath: String;
  appBundlePath: String;
Begin
  // Normalize base path to absolute path
  BasePath := ExpandFileName(IncludeTrailingPathDelimiter(BasePath));
  log('ResolveResourceBase: BasePath=' + BasePath, llInfo);
  
  // Try multiple locations for data directory
  // 1. Direct relative to executable (works with symlinks)
  testPath := BasePath + 'data';
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    log('ResolveResourceBase: Found data at ' + Result, llInfo);
    exit;
  End;
  // 2. Relative path from MacOS directory in .app bundle
  testPath := ExpandFileName(BasePath + '../Resources/data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    log('ResolveResourceBase: Found data at ' + Result, llInfo);
    exit;
  End;
  // 3. Try symlink path (../../data from MacOS) - for shared data directory
  testPath := ExpandFileName(BasePath + '../../data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    log('ResolveResourceBase: Found data at ' + Result, llInfo);
    exit;
  End;
  // 4. Try symlink path (../../../data from MacOS) - alternative symlink location
  testPath := ExpandFileName(BasePath + '../../../data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    log('ResolveResourceBase: Found data at ' + Result, llInfo);
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
      log('ResolveResourceBase: Found data at ' + Result, llInfo);
      exit;
    End;
  End;
  // Fallback: use base path (may not exist, but at least we tried)
  Result := BasePath + 'data' + PathDelim;
  log('Warning: Could not resolve data directory, using fallback: ' + Result, llWarning);
End;

End.

