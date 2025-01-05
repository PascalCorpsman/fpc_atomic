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
  uatomic_common, uopengl_animation, uscreens, uChunkmanager, uatomic_field, uatomic, uopengl_graphikengine, usounds, usdl_joystick;

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
    fTramp, fConveyors, fArrows: TOpenGL_Animation; // Wird den Karten zur Verfügung gestellt
    fHurry: THurry;
    fSoundManager: TSoundManager;
    fSoundInfo: TSoundInfo;
    fLastIdleTick: QWord;
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

    Procedure FOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure FOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure FOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure FOnDblClick(Sender: TObject);

    Procedure FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);

    Procedure CheckKeyDown(Key: Word; Keys: TKeySet);
    Procedure CheckKeyUp(Key: Word; Keys: TKeySet);
    Procedure CheckSDLKeys();

    Function LoadSchemeFromFile(Const SchemeFileName: String): Boolean;
    Procedure OnQuitGameSoundFinished(Sender: TObject);

    Procedure RenderPlayerbyInfo(Const Info: TAtomicInfo; Edge: Boolean);
    Procedure RenderFieldHeader();
    Procedure RenderBombs();

    Procedure Connection_Connect(aSocket: TLSocket);
    Procedure Connection_Disconnect(aSocket: TLSocket);
    Procedure Connection_Error(Const msg: String; aSocket: TLSocket);

    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
    Procedure SendChunk(UserDefinedID: Integer; Const Data: TStream);
    Procedure OnUDPConnection_Receive(aSocket: TLSocket); // Empfängt die UDP Broadcast Nachrichten der Server

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

    (*
     * Werden und dürfen nur aus uScreens heraus aufgerufen werden
     *)
    Procedure ChangePlayerKey(PlayerIndex, Direction: Integer);
    Procedure UpdateWinsToWin(Delta: Integer);
    Procedure UpdateSelectedField(Delta: Integer);
    Procedure StartGame();
    Procedure StartPlayingSong(Filename: String);
    Procedure PlaySoundEffect(Filename: String; EndCallback: TNotifyEvent = Nil);
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
  , uloaderdialog
  , uvectormath
  , LCLType
  , process
  , UTF8Process
  , uatomic_messages
  , uatomicfont
  , uopengl_spriteengine
  , ugraphics
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
  index: integer;
Begin
  If Not fInitialized Then exit; // So Lange wir nicht Initialisiert sind, machen wir gar nix !
  log('TGame.SwitchToScreen', llTrace);
  // Sobald wir versuchen uns in ein Spiel ein zu loggen muss ggf. SDL initialisiert werden
  If (TargetScreen = sHost) Or (TargetScreen = sJoinNetwork) Then Begin
    If assigned(OnNeedHideCursor) Then OnNeedHideCursor(Nil);
    If (Settings.Keys[ks0].UseSDL2 Or Settings.Keys[ks1].UseSDL2) Then Begin
      If (Not fsdl_Loaded) Then Begin
        fsdl_Loaded := SDL_LoadLib('');
        If fsdl_Loaded Then Begin
          fsdl_Loaded := SDL_Init(SDL_INIT_JOYSTICK) = 0;
        End;
      End;
      If assigned(fsdlJoysticks[ks0]) Then fsdlJoysticks[ks0].Free;
      If assigned(fsdlJoysticks[ks1]) Then fsdlJoysticks[ks1].Free;
      fsdlJoysticks[ks0] := Nil;
      fsdlJoysticks[ks1] := Nil;
      If fsdl_Loaded Then Begin
        If Settings.Keys[ks0].UseSDL2 Then Begin
          index := ResolveJoystickNameToIndex(Settings.Keys[ks0].Name);
          If index <> -1 Then Begin
            fsdlJoysticks[ks0] := TSDL_Joystick.Create(index);
          End;
        End;
        If Settings.Keys[ks1].UseSDL2 Then Begin
          index := ResolveJoystickNameToIndex(Settings.Keys[ks1].Name);
          If index <> -1 Then Begin
            fsdlJoysticks[ks1] := TSDL_Joystick.Create(index);
          End;
        End;
      End;
    End;
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
        PlaySoundEffect('data' + pathdelim + 'res' + PathDelim + 'quitgame.wav', @OnQuitGameSoundFinished);
      End;
    sHost: Begin
        fParamJoinIP := '';
        If Not StartHost Then Begin // Wenn der Server nicht gestartet werden kann -> Raus
          LogLeave;
          exit;
        End;
        TargetScreen := sJoinNetwork;
        //Als nächstes muss es einen Server zum Drauf verbinden geben
        // -> Der Server Startet und macht den UDP Auf
        //    Sobald der Client den finden verbindet er direkt, PW gibt es keins !
      End;
    sPlayerSetupRequest: Begin
        SwitchToPlayerSetup;
        // Egal wie da es diesen Screen nicht gibt wird er nicht übernommen
        LogLeave;
        exit;
      End;
    sPlayerSetup: Begin
        // Den Screen mit den Aktuellsten Daten versorgen
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
      If FileExists('data' + PathDelim + 'schemes' + PathDelim + Settings.SchemeFile) Then Begin
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
Begin
  log('TGame.ChangePlayerKey', llTrace);
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
  // Umrechnen der Echten Koords in die Screencoords
  xx := round(ConvertDimension(0, fOwner.ClientWidth, x, 0, GameWidth));
  yy := round(ConvertDimension(0, fOwner.ClientHeight, y, 0, GameHeight));
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
    If key = VK_ADD Then Begin
      aVolume := fSoundManager.IncVolume;
      settings.VolumeValue := aVolume;
      fBackupSettings.VolumeValue := aVolume;
      fSoundInfo.Volume := aVolume;
    End;
    If key = VK_SUBTRACT Then Begin
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
            SwitchToScreen(sMainScreen);
          End;
          If key = VK_P Then Begin
            SendChunk(miTogglePause, Nil);
          End;
          If fPlayerIndex[ks0] <> -1 Then CheckKeyDown(key, ks0);
          If fPlayerIndex[ks1] <> -1 Then CheckKeyDown(key, ks1);
        End;
    End;
  End;
  If assigned(FOnKeyDownCapture) Then Begin
    FOnKeyDownCapture(sender, key, shift);
  End;
End;

Procedure TGame.CheckKeyDown(Key: Word; Keys: TKeySet);
Var
  m: TMemoryStream;
  db, b: Boolean;
  ak: TAtomicKey;
  n: QWORD;
Begin
  If key In [Settings.Keys[Keys].KeyLeft, Settings.Keys[Keys].KeyRight, Settings.Keys[Keys].KeyUp, Settings.Keys[Keys].KeyDown, Settings.Keys[Keys].KeyPrimary, Settings.Keys[Keys].KeySecondary] Then Begin
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
  End;
  If assigned(FOnKeyUpCapture) Then Begin
    FOnKeyUpCapture(sender, key, shift);
  End;
End;

Procedure TGame.CheckSDLKeys();

  Procedure CheckKeys(Keys: TKeySet);
  Var
    d: Integer;
    up, down, left, right, first, second: Boolean;
  Begin
    If Not Settings.Keys[keys].UseSDL2 Then exit;
    If Not assigned(fsdlJoysticks[keys]) Then exit;
    If fPlayerIndex[keys] = AIPlayer Then exit;
    // 1. Ermitteln des Aktuellen "Gedrückt" stati
    up := false;
    down := false;
    left := false;
    right := false;
    d := Settings.Keys[keys].AchsisIdle[0] - fsdlJoysticks[keys].Axis[Settings.Keys[keys].AchsisIndex[0]];
    If abs(d) > achsistrigger Then Begin
      If sign(d) = Settings.Keys[keys].AchsisDirection[0] Then Begin
        up := true;
      End
      Else Begin
        down := true;
      End;
    End;
    d := Settings.Keys[keys].AchsisIdle[1] - fsdlJoysticks[keys].Axis[Settings.Keys[keys].AchsisIndex[1]];
    If abs(d) > achsistrigger Then Begin
      If sign(d) = Settings.Keys[keys].AchsisDirection[1] Then Begin
        left := true;
      End
      Else Begin
        right := true;
      End;
    End;
    first := Settings.Keys[keys].ButtonsIdle[0] = fsdlJoysticks[keys].Button[Settings.Keys[keys].ButtonIndex[0]];
    second := Settings.Keys[keys].ButtonsIdle[1] = fsdlJoysticks[keys].Button[Settings.Keys[keys].ButtonIndex[1]];
    // 2. Rauskriegen ob ein Event abgeleitet werden muss
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

Var
  event: TSDL_Event;
Begin
  If Not fsdl_Loaded Then exit;
  If Not (Settings.Keys[ks0].UseSDL2 Or Settings.Keys[ks1].UseSDL2) Then exit;

  While SDL_PollEvent(@event) <> 0 Do Begin // TODO: Braucht man das wirklich ?
  End;

  CheckKeys(ks0);
  CheckKeys(ks1);
End;

Function TGame.StartHost: Boolean;
Var
  serv: String;
  p: TProcessUTF8;
Begin
  //Begin
  log('TGame.StartHost', llTrace);
  result := false;
  // Starten des Atomic_servers, dann als Client verbinden
  serv := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'atomic_server';
{$IFDEF Windows}
  serv := serv + '.exe';
{$ENDIF}
  If FileExistsUTF8(serv) Then Begin
    p := TProcessUTF8.Create(Nil);
    p.Options := [poNewConsole];
    p.Executable := serv;
    p.Parameters.Add('-p');
    p.Parameters.Add(inttostr(settings.Port));
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
  sleep(500); // Bis der Server Steht dauerts ein bischen, also warten wir
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
  cnt, i, uid, j: integer;
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
  For i := 0 To high(fFields) Do Begin
    If (fFields[i].Name = FieldName)
      And (fFields[i].Hash = FieldHash) Then Begin
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
     *)
    If OldAnim In OneTimeAnimations Then Begin
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
  fn := 'data' + PathDelim + 'schemes' + PathDelim + SchemeFileName;
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
   *)
  glColor3f(1, 1, 1);
  AtomicBigFont.Textout(500, 10, fPlayingTime_s);
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
Begin
  log('TGame.Join', lltrace);
  log(format('Joining to %s on port %d as %s', [ip, port, Settings.NodeName]));
  (*
   * Sollte aus welchem Grund auch immer der Chunkmanager nicht verbinden können (ggf weil er schon verbunden ist),
   * fliegt er hier über den Wechsel in den MainScreen raus und wird dann automatisch disconnected
   * -> Spätestens wenn der User es nun erneut versucht kommt er wieder rein.
   *)
  If Not fChunkManager.Connect(ip, port) Then Begin
    LogShow('Error could not connect to server', llError);
    SwitchToScreen(sMainScreen);
  End;
  logleave;
End;

Constructor TGame.Create;
Begin
  Inherited Create;
  fsdl_Loaded := false;
  fsdlJoysticks[ks0] := Nil;
  fsdlJoysticks[ks1] := Nil;
  fSoundManager := TSoundManager.Create();
  fSoundInfo := TSoundInfo.Create();
  fParamJoinIP := '';
  fPause := false;
  fNeedDisconnect := false;
  fPlayerIndex[ks0] := -1;
  fPlayerIndex[ks1] := -1;
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
  If assigned(fsdlJoysticks[ks0]) Then fsdlJoysticks[ks0].free;
  If assigned(fsdlJoysticks[ks1]) Then fsdlJoysticks[ks1].free;
  fsdlJoysticks[ks0] := Nil;
  fsdlJoysticks[ks1] := Nil;
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
  LogLeave;
End;

Procedure TGame.RegisterUDPConnection(Const Connection: TLUDPComponent);
Begin
  log('TGame.RegisterUDPConnection', llTrace);
  fUDPPingData.Connection := Connection;
  If assigned(Connection) Then Begin
    If fUDPPingData.Connection.Connected Then fUDPPingData.Connection.Disconnect();
    fUDPPingData.Connection.OnReceive := @OnUDPConnection_Receive;
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
  log('TGame.Initialize', lltrace);
  fOwner := Owner;
  Loader := TLoaderDialog.create(Owner);
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

  p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  AtomicBigFont.CreateFont(p + 'data' + PathDelim + 'res' + PathDelim);

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

  For i In TScreenEnum Do Begin
    If Not assigned(fScreens[i]) Then Continue;
    fScreens[i].LoadFromDisk(p + 'data' + PathDelim + 'res' + PathDelim);
  End;
  Loader.Percent := 10;
  Loader.Render();
{$IFDEF ShowInitTime}
  TimePoint(2);
{$ENDIF}

  fPowerUpsTex[puNone] := 0; // Das Gibts ja net -> Weg
  fPowerUpsTex[puExtraBomb] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powbomb.png', smStretchHard);
  If fPowerUpsTex[puExtraBomb] = 0 Then exit;
  fPowerUpsTex[puLongerFlameLength] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powflame.png', smStretchHard);
  If fPowerUpsTex[puLongerFlameLength] = 0 Then exit;
  fPowerUpsTex[puDisease] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powdisea.png', smStretchHard);
  If fPowerUpsTex[puDisease] = 0 Then exit;
  fPowerUpsTex[puCanCick] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powkick.png', smStretchHard);
  If fPowerUpsTex[puCanCick] = 0 Then exit;
  fPowerUpsTex[puExtraSpeed] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powskate.png', smStretchHard);
  If fPowerUpsTex[puExtraSpeed] = 0 Then exit;
  fPowerUpsTex[puCanPunch] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powpunch.png', smStretchHard);
  If fPowerUpsTex[puCanPunch] = 0 Then exit;
  fPowerUpsTex[puCanGrab] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powgrab.png', smStretchHard);
  If fPowerUpsTex[puCanGrab] = 0 Then exit;
  fPowerUpsTex[puCanSpooger] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powspoog.png', smStretchHard);
  If fPowerUpsTex[puCanSpooger] = 0 Then exit;
  fPowerUpsTex[puGoldFlame] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powgold.png', smStretchHard);
  If fPowerUpsTex[puGoldFlame] = 0 Then exit;
  fPowerUpsTex[puTrigger] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powtrig.png', smStretchHard);
  If fPowerUpsTex[puTrigger] = 0 Then exit;
  fPowerUpsTex[puCanJelly] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powjelly.png', smStretchHard);
  If fPowerUpsTex[puCanJelly] = 0 Then exit;
  fPowerUpsTex[puSuperBadDisease] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powebola.png', smStretchHard);
  If fPowerUpsTex[puSuperBadDisease] = 0 Then exit;
  fPowerUpsTex[puSlow] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powslow.png', smStretchHard);
  If fPowerUpsTex[puSlow] = 0 Then exit;
  fPowerUpsTex[purandom] := OpenGL_GraphikEngine.LoadGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'powrand.png', smStretchHard);
  If fPowerUpsTex[purandom] = 0 Then exit;
  // Load PlayerDead Tex correct.
  fPlayerdeadTex.Image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'playerdead.png', ugraphics.ColorToRGB(clfuchsia), smClamp);
  fPlayerdeadTex := OpenGL_GraphikEngine.FindItem(p + 'data' + PathDelim + 'res' + PathDelim + 'playerdead.png');
  If fPlayerdeadTex.Image = 0 Then exit;
  fhurry.Texture.Image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'hurry.png', ugraphics.ColorToRGB(clfuchsia), smClamp);
  If fhurry.Texture.Image = 0 Then exit;
  fhurry.Texture := OpenGL_GraphikEngine.FindItem(p + 'data' + PathDelim + 'res' + PathDelim + 'hurry.png');
  hohletex.image := OpenGL_GraphikEngine.LoadAlphaColorGraphik(p + 'data' + PathDelim + 'res' + PathDelim + 'hole.png', ugraphics.ColorToRGB(clfuchsia), smStretchHard);
  If hohletex.Image = 0 Then exit;
  hohletex := OpenGL_GraphikEngine.FindItem(p + 'data' + PathDelim + 'res' + PathDelim + 'hole.png');
{$IFDEF ShowInitTime}
  TimePoint(3);
{$ENDIF}

  // Laden der Felder
  fArrows := TOpenGL_Animation.Create;
  If Not fArrows.LoadFromFile(p + 'data' + PathDelim + 'res' + PathDelim + 'arrows.ani', true) Then Begin
    Exit;
  End;
  fConveyors := TOpenGL_Animation.Create;
  If Not fConveyors.LoadFromFile(p + 'data' + PathDelim + 'res' + PathDelim + 'conveyor.ani', true) Then Begin
    Exit;
  End;
  fTramp := TOpenGL_Animation.Create;
  If Not fTramp.LoadFromFile(p + 'data' + PathDelim + 'res' + PathDelim + 'tramp.ani', true) Then Begin
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
  sl := FindAllDirectories(p + 'data' + PathDelim + 'maps', false);
  sl.Sorted := true;
  sl.Sort;
{$IFDEF Only1Map}
  While sl.Count > 1 Do Begin
    sl.Delete(sl.Count - 1);
  End;
{$ENDIF}
  setlength(fFields, sl.Count);
  If sl.count = 0 Then Begin
    LogShow('Error, no fields to load found', llFatal);
    LogLeave;
    exit;
  End;
{$IFDEF ShowInitTime}
  TimePoint(6);
{$ENDIF}
  For j := 0 To sl.count - 1 Do Begin
    fFields[j] := TAtomicField.Create();
    If Not fFields[j].loadFromDirectory(sl[j], fArrows, fConveyors, fTramp, hohletex.Image, fTrampStatic) Then Begin
      LogShow('Error, unable to load field:' + sl[j], llFatal);
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
    If Not fAtomics[j].InitAsColor(p + 'data' + PathDelim + 'atomic' + PathDelim, PlayerColors[j]) Then Begin
      LogShow('Error, unable to load atomic.', llFatal);
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
    If fUDPPingData.Connection.Connected Then Begin
      fUDPPingData.Connection.Disconnect(true);
    End;
  End;
  logleave;
End;

Procedure TGame.Render;
Var
  i: Integer;
  n: QWORD;
Begin
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
        CheckSDLKeys;
        fActualField.render(fAtomics, fPowerUpsTex);
        RenderBombs;
        For i := 0 To high(fPlayer) Do Begin
          If fPlayer[i].Info.Alive Then Begin
            RenderPlayerbyInfo(fPlayer[i].Info, fPlayer[i].edge);
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
End;

End.

