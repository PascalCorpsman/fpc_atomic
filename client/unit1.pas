(******************************************************************************)
(* FPC_Atomic                                                      01.03.2023 *)
(*                                                                            *)
(* Version     : see uatomic_common.pas                                       *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Reimplementation of the Atomic Bomberman game using FPC and  *)
(*               modern network tecnologies.                                  *)
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
(* History     : see uatomic_common.pas                                       *)
(*                                                                            *)
(******************************************************************************)


(*
 * Die einzelnen Karten https://www.youtube.com/watch?v=I00PsXh_xTg
 * Einzelspieler, mit Geistern: https://www.youtube.com/watch?v=1S6KNaMUP0w
 * Sicht auf Optionen Dialog: https://www.youtube.com/watch?v=fO9HhzhEloE
 * Network Spiel aus Server Sicht: https://www.youtube.com/watch?v=cuZ2GOkse5k
 *)
Unit Unit1;

{$MODE objfpc}{$H+}

{$I globaldefines.inc}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Math, ExtCtrls, IniPropStorage,
  OpenGlcontext, lNetComponents, lNet,
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  , uopengl_graphikengine // Die OpenGLGraphikengine ist eine Eigenproduktion von www.Corpsman.de, und kann getrennt geladen werden.
  , uOpenGL_ASCII_Font
  , ugame
  , uatomic_common;

{$IFDEF AUTOMODE}
Const
  AM_Idle = 0; // Nichts
  AM_StartClient = 1; // Wenn Parameter -d übergeben wird, dann wird automatisch als client auf 127.0.0.1 mit User "Client" verbunden.
{$ENDIF}

Type

{$IFDEF AUTOMODE}
  TAutomodeData = Record
    State: integer;
  End;
{$ENDIF}

  (*
   * Alle Nachrichten, die aus dem OnPaint des OpenGLControl heraus erzeugt werden
   * Können durch die LCL nicht angezeigt werden.
   * Also müssen Sie auf den "OnIdle" umgeleitet werden.
   *)
  TUserMessages = Record
    Msg: String;
    WarnLevel: TLogLevel;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    IniPropStorage1: TIniPropStorage;
    LTCPComponent1: TLTCPComponent;
    LUDPComponent1: TLUDPComponent;
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure IniPropStorage1SavingProperties(Sender: TObject);
    Procedure OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    fUserMessages: Array Of TUserMessages;
    // Ehemals Globale Variablen
    Initialized: Boolean; // Wenn True dann ist OpenGL initialisiert
    Form1ShowOnce: Boolean;
    FPS_Counter, LastFPS_Counter: integer;
    LastFPSTime: int64;
    fLastControlWidth, fLastControlHeight: Integer; // Track last control size to detect resize
    fLevelStartTime: int64; // Time when current level started (for elapsed time tracking)
    fLastPlayingTime_s: integer; // Last known playing time to detect level start
    fGameInitialized: Boolean; // Flag to track if Game.Initialize has been called
    FWishFullscreen: Boolean;
    FAdjustingSize: Boolean;
{$IFDEF AUTOMODE}
    AutomodeData: TAutomodeData;
{$ENDIF}
    ConnectParamsHandled: Boolean; // Die Übergabe Parameter -ip, -port werden nur 1 mal ausgewertet !
    Procedure OnIdle(Sender: TObject; Var Done: Boolean);
    Procedure Load_Atomic_Settings;
    Function GetWorkDir(Out Directory: String): Boolean;
    Procedure HideCursor(Sender: TObject);
    Procedure ShowCursor(Sender: TObject);
  public
    { public declarations }
    Procedure OnConnectToServer(Sender: TObject);
    Procedure OnDisconnectFromServer(Sender: TObject);
    Procedure AddUserMessage(Msg: String; WarnLevel: TLogLevel);
    Procedure SetFullScreen(Value: Boolean);
  End;

Var
  Form1: TForm1;

Procedure ShowUserMessage(Msg: String; WarnLevel: TLogLevel);

Implementation

{$R *.lfm}

Uses lazfileutils, LazUTF8, LCLType
{$IFDEF Windows}
  , windows // Für Konsolenmodus zur Laufzeit
{$ENDIF}
  , bass
  //  , unit18 // Der Fortschrittsbalken während des Updates
  , uatomicfont
{$IFDEF AUTOMODE}
  , uscreens
{$ENDIF}
  , uearlylog
  ;

Var
  DefFormat: TFormatSettings;

Procedure ShowUserMessage(Msg: String; WarnLevel: TLogLevel);
Begin
  form1.AddUserMessage(msg, WarnLevel);
End;

{ TForm1 }

Var
  allowcnt: Integer = 0;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Begin
  // Changed to llTrace to reduce log spam
  // log('OpenGLControl1MakeCurrent allowcnt=' + inttostr(allowcnt), llTrace);
  If allowcnt > 2 Then Begin
    exit;
  End;
  inc(allowcnt);
  // Sollen Dialoge beim Starten ausgeführt werden ist hier der Richtige Zeitpunkt
  If allowcnt = 1 Then Begin
    // Init dglOpenGL.pas , Teil 2
    ReadExtensions; // Anstatt der Extentions kann auch nur der Core geladen werden. ReadOpenGLCore;
    ReadImplementationProperties;
  End;
  If (allowcnt >= 1) And (Not Initialized) Then Begin // Ensure initialization runs once when the context becomes available.
    log('Initializing OpenGL resources (allowcnt=' + inttostr(allowcnt) + ')', llInfo);
    OpenGL_GraphikEngine.clear;
    Create_ASCII_Font;
    AtomicFont.CreateFont;
    glenable(GL_TEXTURE_2D); // Texturen
    glEnable(GL_DEPTH_TEST); // Tiefentest
    glDepthFunc(gl_less);
    glBlendFunc(gl_one, GL_ONE_MINUS_SRC_ALPHA); // Sorgt dafür, dass Voll Transparente Pixel nicht in den Tiefenpuffer Schreiben.

    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
    Timer1.Enabled := true;
{$IFDEF Windows}
    // On Windows, call Game.Initialize directly here to ensure textures are loaded
    // before any rendering happens. On Windows, the OpenGL context is ready at this point.
    If Assigned(Game) And Not fGameInitialized Then Begin
      fGameInitialized := true; // Set flag first to prevent re-entry
      log('Calling Game.Initialize from OpenGLControl1MakeCurrent (Windows)', llInfo);
      log('Game assigned: ' + BoolToStr(Assigned(Game), true), llInfo);
      log('OpenGLControl1 assigned: ' + BoolToStr(Assigned(OpenGLControl1), true), llInfo);
      Try
        Game.initialize(OpenGLControl1);
        log('Game.Initialize completed successfully', llInfo);
        log('Game.IsInitialized: ' + BoolToStr(Game.IsInitialized, true), llInfo);
      Except
        On E: Exception Do Begin
          log('ERROR in Game.Initialize: ' + E.Message, llError);
          log('Exception class: ' + E.ClassName, llError);
        End;
      End;
      // Process messages to allow OnPaint to render loading dialog
      Application.ProcessMessages;
    End Else Begin
      If Not Assigned(Game) Then
        log('WARNING: Game is not assigned in OpenGLControl1MakeCurrent (Windows)', llWarning);
      If fGameInitialized Then
        log('Game already initialized, skipping', llInfo);
    End;
{$ELSE}
    // NOTE: On macOS, Game.Initialize is called from Application.OnIdle
    // This ensures Application.Run is active, so OnPaint can be called during initialization
    // On macOS, the context might not be fully ready at this point
    fGameInitialized := false; // Will be set to true in OnIdle after Game.Initialize completes
{$ENDIF}
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  s: String;
  CurrentPlayingTime: Integer;
  ElapsedMs, ElapsedSeconds, Minutes, Seconds: Integer;
Begin
  (*
   * Unter Windows kann es vorkommen, dass dieses OnPaint ausgelöst wird obwohl wir noch am Laden in OpenGLControl1MakeCurrent sind
   * Wenn das Passiert, bekommt der User eine Fehlermeldung die nicht stimmt.
   *
   * Zum Glück kann man das Abfangen in dem man hier den Timer1 abprüft und das so verhindert ;)
   * 
   * NOTE: Allow rendering during initialization to show loading dialog (critical on macOS and Windows)
   *)
  If Not Timer1.Enabled Then exit;
  // Allow rendering loading dialog even if Initialized is false (during Game.Initialize on macOS)
  // On macOS, Initialized may be false but Game.Initialize is in progress
  If Not Initialized And (Not Assigned(Game) Or Not Assigned(Game.LoaderDialog) Or Game.IsInitialized) Then Exit;
  // On Windows, Game.Initialize is called from OpenGLControl1MakeCurrent, so if Initialized is true
  // but Game is not initialized yet, we should wait (though this should not happen)
{$IFDEF Windows}
  If Initialized And (Not Assigned(Game) Or (Assigned(Game) And Not Game.IsInitialized And Not Assigned(Game.LoaderDialog))) Then Exit;
{$ENDIF}
  
  // Check if window size changed and update viewport if needed
  If (OpenGLControl1.ClientWidth <> fLastControlWidth) Or 
     (OpenGLControl1.ClientHeight <> fLastControlHeight) Then Begin
    OpenGLControl1Resize(Nil);
  End;
  
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glcolor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  // Render loading dialog during initialization, otherwise render game
  If Assigned(Game) And Assigned(Game.LoaderDialog) And Not Game.IsInitialized Then Begin
    // Render loading dialog directly during initialization (critical for macOS visibility)
    Game.LoaderDialog.RenderDirect();
  End Else If Assigned(Game) And Game.IsInitialized Then Begin
    Game.Render();
  End;
  If Assigned(Game) And Game.IsInitialized And Game.Settings.ShowFPS Then Begin
    // Track level start time - detect when level begins (playing time resets to 0 or jumps up)
    If Assigned(Game) Then Begin
      CurrentPlayingTime := Game.PlayingTime_s;
      // Detect level start: playing time reset to 0 (or -1 for infinity) when level begins
      // or when playing time suddenly increases (new round starts)
      If (CurrentPlayingTime >= 0) And ((fLastPlayingTime_s < 0) Or 
         (fLastPlayingTime_s > CurrentPlayingTime + 5)) Then Begin
        // Level started (reset detected or time jumped backwards significantly)
        fLevelStartTime := GetTickCount64();
      End;
      fLastPlayingTime_s := CurrentPlayingTime;
    End;
    
    Go2d(OpenGLControl1.Width, OpenGLControl1.Height);
    glBindTexture(GL_TEXTURE_2D, 0);
    glTranslatef(0, 0, atomic_dialog_Layer + atomic_EPSILON);
    
    // === ENHANCED DEBUG OVERLAY - BOTTOM OF SCREEN ===
    // Use AtomicFont for larger, more readable text
    AtomicFont.Color := clWhite;
    
    // First line: FPS and Network stats with MAX values
    s := 'FPS: ' + inttostr(LastFPS_Counter);
    If Assigned(Game) Then Begin
      s := s + format(' | RTT: %dms (max %dms)', [Game.fDebugStats.LastRTT, Game.fDebugStats.MaxRTT]);
      s := s + format(' | Snap: %dms (max %dms)', [Game.fDebugStats.SnapshotAge, Game.fDebugStats.MaxSnapAge]);
    End;
    AtomicFont.Textout(10, OpenGLControl1.Height - 60, s);
    
    // Second line: Interpolation stats with MAX
    If Assigned(Game) Then Begin
      s := format('Interp: %.2f (max %.2f)', [Game.fDebugStats.InterpolationFactor, Game.fDebugStats.MaxInterpFactor]);
      If Game.fDebugStats.IsExtrapolating Then
        s := s + ' [EXTRAPOLATING]'
      Else
        s := s + ' [interpolating]';
      AtomicFont.Textout(10, OpenGLControl1.Height - 40, s);
    End;
    
    // Third line: Viewport info (if in debug mode)
    {$IFDEF DebuggMode}
    If Assigned(Game) Then Begin
      s := 'Viewport: ' + inttostr(OpenGLControl1.ClientWidth) + 'x' + inttostr(OpenGLControl1.ClientHeight);
      s := s + ' | Game: ' + inttostr(Game.DebugViewportWidth) + 'x' + inttostr(Game.DebugViewportHeight);
      s := s + format(' Scale: %.2f', [Game.DebugViewportScale]);
      AtomicFont.Textout(10, OpenGLControl1.Height - 20, s);
      
      // AI position debug
      If game.fPlayer[1].UID = -1 Then Begin
        s := format('AI: %.2f, %.2f', [game.fPlayer[1].Info.Position.x, game.fPlayer[1].Info.Position.y]);
        OpenGL_ASCII_Font.Textout(10, OpenGLControl1.Height - 5, s);
      End;
    End;
    {$ENDIF}
    Exit2d();
  End;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Var
  ControlW, ControlH: Integer;
  ScaleX, ScaleY, UniformScale: Double;
  ViewportWidth, ViewportHeight: Integer;
  OffsetX, OffsetY: Integer;
Begin
  ControlW := OpenGLControl1.ClientWidth;
  ControlH := OpenGLControl1.ClientHeight;
  If ControlW <= 0 Then ControlW := GameWidth;
  If ControlH <= 0 Then ControlH := GameHeight;

  ScaleX := ControlW / GameWidth;
  ScaleY := ControlH / GameHeight;
  If ScaleX < ScaleY Then
    UniformScale := ScaleX
  Else
    UniformScale := ScaleY;
  If UniformScale <= 0 Then
    UniformScale := 1;

  ViewportWidth := Round(GameWidth * UniformScale);
  ViewportHeight := Round(GameHeight * UniformScale);
  If ViewportWidth < 1 Then ViewportWidth := 1;
  If ViewportHeight < 1 Then ViewportHeight := 1;
  If ViewportWidth > ControlW Then ViewportWidth := ControlW;
  If ViewportHeight > ControlH Then ViewportHeight := ControlH;

  OffsetX := (ControlW - ViewportWidth) div 2;
  OffsetY := (ControlH - ViewportHeight) div 2;

  // Always set viewport if OpenGL context is available
  // CRITICAL: Set projection matrix to fixed logical dimensions (GameWidth x GameHeight)
  // This ensures game coordinates are always the same regardless of viewport size
  // Without this, changing viewport size changes the logical coordinate system,
  // which affects animation timing and game speed
  If Initialized And OpenGLControl1.MakeCurrent Then Begin
    glViewport(OffsetX, OffsetY, ViewportWidth, ViewportHeight);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    // Set orthographic projection with fixed logical dimensions (GameWidth x GameHeight)
    // This ensures that game coordinates are always the same, regardless of viewport size
    glOrtho(0, GameWidth, GameHeight, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);
  End;

  // Update game viewport metrics even before initialization
  If Assigned(Game) Then
    Game.SetViewportMetrics(ControlW, ControlH, OffsetX, OffsetY,
      ViewportWidth, ViewportHeight);
  
  // Track current control size to detect resize changes
  fLastControlWidth := ControlW;
  fLastControlHeight := ControlH;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  FileloggingDir: String;
  i: integer;
  AutoLogFile: String;
  ConfigDirUsed: String;
{$IFDEF DARWIN}
  ConfigDir: String;
{$ENDIF}
Begin
  Randomize;
  ConnectParamsHandled := false;
  Initialized := false; // Wenn True dann ist OpenGL initialisiert
  Form1ShowOnce := true;
  FAdjustingSize := false;
  fGameInitialized := false; // Game.Initialize will be called from OnIdle
  FileloggingDir := '';
  AutoLogFile := '';
  ConfigDirUsed := '';
{$IFDEF DARWIN}
  ConfigDir := IncludeTrailingPathDelimiter(GetUserDir) + 'Library/Application Support/fpc_atomic/';
  If Not ForceDirectoriesUTF8(ConfigDir) Then
    ConfigDir := IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(False));
  If (ConfigDir <> '') And ForceDirectoriesUTF8(ConfigDir) Then Begin
    IniPropStorage1.IniFileName := ConfigDir + 'fpc_atomic.ini';
    ConfigDirUsed := ConfigDir;
    AutoLogFile := ConfigDir + 'debug.log';
  End
  Else
{$ENDIF}
    IniPropStorage1.IniFileName := 'fpc_atomic.ini';
  DefFormat := DefaultFormatSettings;
  DefFormat.DecimalSeparator := '.';
  LogShowHandler := @ShowUserMessage;
  fUserMessages := Nil;
  DefaultFormatSettings.DecimalSeparator := '.';
  InitLogger();
  For i := 1 To Paramcount Do Begin
{$IFDEF Windows}
    If lowercase(ParamStrUTF8(i)) = '-c' Then Begin
      AllocConsole; // in Windows unit
      IsConsole := True; // in System unit
      SysInitStdIO; // in System unit
      EnableLogToConsole();
      SetConsoleOutputCP(CP_UTF8);
    End;
{$ENDIF}
    If i < Paramcount Then Begin // Alle Parameter, welche einen weiteren Parameter auslesen
      // FileLogging
      If lowercase(ParamStrUTF8(i)) = '-f' Then Begin
        FileloggingDir := ExtractFilePath(ParamStrUTF8(i + 1));
        If Not DirectoryExistsUTF8(FileloggingDir) Then Begin
          If Not CreateDirUTF8(FileloggingDir) Then Begin
            FileloggingDir := '';
          End;
        End;
        If FileloggingDir <> '' Then Begin
          FileloggingDir := ParamStrUTF8(i + 1);
          SetLoggerLogFile(FileloggingDir);
        End;
      End;
      // Loglevel
      If lowercase(ParamStrUTF8(i)) = '-l' Then Begin
        SetLogLevel(strtointdef(ParamStrUTF8(i + 1), 2));
      End;
    End;
  End;
  If (FileloggingDir = '') And (AutoLogFile <> '') Then Begin
    FileloggingDir := AutoLogFile;
    SetLoggerLogFile(FileloggingDir);
  End;
  If ConfigDirUsed <> '' Then
    log('Using config dir: ' + ConfigDirUsed, llInfo);
  log('TForm1.FormCreate', llInfo);
  log('TForm1.FormCreate', lltrace);
  If FileloggingDir = '' Then Begin
    log('Disabled, file logging.', llWarning);
  End
  Else Begin
    log('Writing log to: ' + FileloggingDir, llInfo);
  End;
  caption := defCaption;

  If (BASS_GetVersion() Shr 16) <> Bassversion Then Begin
    showmessage('Unable to init the Bass Library ver. :' + BASSVERSIONTEXT);
    halt;
  End;

  (*
  BASS_DEVICE_DMIX = Erlaubt das Mehrfache Beschreiben auf die Soundkarte.
  Ohne dieses Flag, können keine anderen Sound anwendungen mehr laufen.
  *)
{$IFDEF Windows}
  If Not Bass_init(-1, 44100, BASS_DEVICE_DMIX, 0, Nil) Then Begin
{$ELSE}
  // Steht der Compiler auf Object Pascal mus diese Zeile genommen werden.
  If Not Bass_init(-1, 44100, BASS_DEVICE_DMIX, Nil, Nil) Then Begin
{$ENDIF}
    showmessage('Unable to init the device, Error code :' + inttostr(BASS_ErrorGetCode));
    halt;
  End;

  ClientWidth := 640;
  ClientHeight := 480;
  Tform(self).Constraints.MinHeight := 480;
  Tform(self).Constraints.Minwidth := 640;
{$IFDEF Darwin}
  // Prevent fullscreen mode by limiting window size (prevents "Game Mode" activation)
  Tform(self).Constraints.MaxWidth := Screen.Width - 1;
  Tform(self).Constraints.MaxHeight := Screen.Height - 1;
{$ENDIF}
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    LogShow('Error, could not init dglOpenGL.pas', llfatal);
    LogLeave;
    Halt(1);
  End;
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  Timer1.Interval := 17;
  OpenGLControl1.Align := alClient;
  fLastControlWidth := 0;
  fLastControlHeight := 0;
  fLevelStartTime := 0;
  fLastPlayingTime_s := -1;
  Game := TGame.Create();
  game.OnNeedHideCursor := @HideCursor;
  game.OnNeedShowCursor := @ShowCursor;
  Load_Atomic_Settings;
{$IFDEF AUTOMODE}
  AutomodeData.State := AM_Idle;
{$ENDIF}
  Application.AddOnIdleHandler(@OnIdle);
  LogLeave;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  log('TForm1.FormDestroy', llTrace);
  If assigned(Game) Then
    Game.free;
  Game := Nil;
  // Free Bass
  If Not Bass_Free Then
    showmessage('Unable to free Bass, Error code :' + inttostr(BASS_ErrorGetCode));
  LogLeave;
End;

Procedure TForm1.FormShow(Sender: TObject);
{$IFDEF AUTOMODE}
Var
  i: integer;
{$ENDIF}
Begin
  If Form1ShowOnce Then Begin
    Form1ShowOnce := false;
    Game.RegisterTCPConnection(LTCPComponent1);
    Game.RegisterUDPConnection(LUDPComponent1);
{$IFDEF AUTOMODE}
    If Paramcount <> 0 Then Begin
      For i := 1 To Paramcount Do Begin
        If (lowercase(ParamStrUTF8(i)) = '-d') Then Begin
          // Verbinden auf einen Lokalen Server
          AutomodeData.State := AM_StartClient;
          break;
        End;
      End;
    End;
{$ENDIF}
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  log('TForm1.FormCloseQuery', llTrace);
  // Todo: Speichern der Map, oder wenigstens Nachfragen ob gespeichert werden soll
  log('Shuting down.', llInfo);
  IniPropStorage1.WriteInteger('ProtocollVersion', ProtocollVersion);
  IniPropStorage1.WriteString('Version', Version);
  //setValue('MainForm', 'Left', inttostr(Form1.left));
  //setValue('MainForm', 'Top', inttostr(Form1.top));
  //setValue('MainForm', 'Width', inttostr(Form1.Width));
  //setValue('MainForm', 'Height', inttostr(Form1.Height));

  timer1.Enabled := false;
  Initialized := false;
  // Eine Evtl bestehende Verbindung Kappen, so lange die LCL und alles andere noch Lebt.
  Game.Disconnect;
  Game.OnIdle;
  LogLeave;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Var
{$IFDEF DebuggMode}
  i: Cardinal;
  p: Pchar;
{$ENDIF}
  t: int64;
{$IFDEF Darwin}
  MaxWidth, MaxHeight: Integer;
{$ENDIF}
Begin
{$IFDEF Darwin}
  // Convert fullscreen attempt (green button click) to large windowed mode
  If WindowState = wsFullScreen Then Begin
    WindowState := wsNormal;
    MaxWidth := Screen.Width - 1;
    MaxHeight := Screen.Height - 1;
    Tform(self).Constraints.MaxWidth := MaxWidth;
    Tform(self).Constraints.MaxHeight := MaxHeight;
    Left := (Screen.Width - MaxWidth) div 2;
    Top := (Screen.Height - MaxHeight) div 2;
    Width := MaxWidth;
    Height := MaxHeight;
    fWishFullscreen := True;
    Game.Settings.Fullscreen := True;
  End;
{$ENDIF}
  If Initialized Then Begin
    inc(FPS_Counter);
    t := GetTickCount64();
    If LastFPSTime + 1000 <= t Then Begin
      LastFPSTime := t;
      LastFPS_Counter := FPS_Counter;
      FPS_Counter := 0;
    End;
    OpenGLControl1.Invalidate;
{$IFDEF DebuggMode}
    i := glGetError();
    If i <> 0 Then Begin
      Timer1.Enabled := false;
      p := gluErrorString(i);
      showmessage('OpenGL Error (' + inttostr(i) + ') occured.' + LineEnding + LineEnding +
        'OpenGL Message : "' + p + '"' + LineEnding + LineEnding +
        'Applikation will be terminated.');
      close;
    End;
{$ENDIF}
  End;
End;

Procedure TForm1.OnIdle(Sender: TObject; Var Done: Boolean);
Var
  i, port: Integer;
  msg, ip: String;
Begin
  // CRITICAL: On macOS, Game.Initialize must be called from OnIdle (after Application.Run starts)
  // This ensures OnPaint can be called during initialization to show the loading dialog
  // On Windows, Game.Initialize is called directly from OpenGLControl1MakeCurrent
{$IFNDEF Windows}
  If Not fGameInitialized And Initialized And Assigned(Game) Then Begin
    // Ensure OpenGL context is active before initializing (required for texture loading)
    // On macOS, context might not be ready immediately after Application.Run starts
    If Not OpenGLControl1.MakeCurrent Then Begin
      // Context not ready yet, try again next time
      exit;
    End;
    fGameInitialized := true; // Set flag first to prevent re-entry
    // Initialize game - this will load textures and create loading dialog
    // All texture loading happens here, so context must be active
    Game.initialize(OpenGLControl1);
    // Process messages to allow OnPaint to render loading dialog
    // This is critical on macOS to show loading progress
    Application.ProcessMessages;
  End;
{$ENDIF}
  
  // Process incoming network chunks from network thread
  If Assigned(Game) Then Begin
    Game.ChunkManager.ProcessIncomingChunks();
  End;
  
{$IFDEF AUTOMODE}
  Case AutomodeData.State Of
    AM_Idle: Begin
        // Nichts zu tun ..
      End;
    AM_StartClient: Begin
        If Initialized Then Begin
          AutomodeData.State := AM_Idle;
          Game.SwitchToScreen(sJoinNetwork);
          //          Atomic.SwitchToScreen(sHost);
          ConnectParamsHandled := true; // Wenn wir im Automode sind, werten wir -ip und -port nicht mehr aus!
        End;
      End;
  End;
{$ENDIF}
  If Not ConnectParamsHandled Then Begin
    ConnectParamsHandled := true;
    (*
     * Im Falle, dass ein Spieler übers internet beitreten will, geht das typische UDP Suchen nicht
     * Dann übergibt er die Zieldaten via Paramstr und hat so auch seinen Spass ;)
     *)
    ip := '';
    port := 0;
    For i := 1 To ParamCount Do Begin
      If ParamStr(i) = '-ip' Then Begin
        ip := ParamStr(i + 1);
      End;
      If ParamStr(i) = '-port' Then Begin
        port := strtointdef(ParamStr(i + 1), 0);
      End;
    End;
    If (ip <> '') And (port > 0) Then Begin
      Game.JoinViaParams(ip, port);
    End;
  End;
  // Anzeigen der Usernachrichten, welche aus OpenGLControl.OnPaint kommen
  While Length(fUserMessages) <> 0 Do Begin
    // LogShow(fUserMessages[0].Msg, fUserMessages[0].WarnLevel); -- Das würde eine EndlosRekursion geben ...
    // Weiterleiten an den Logger
    log(fUserMessages[0].Msg, fUserMessages[0].WarnLevel);
    // Merken der Nachricht für die Anzeige nachher
    msg := LogLevelToString(fUserMessages[0].WarnLevel) + ' : ' + fUserMessages[0].Msg;
    // Entfernen aus der Merkliste
    For i := 0 To high(fUserMessages) - 1 Do Begin
      fUserMessages[i] := fUserMessages[i + 1];
    End;
    setlength(fUserMessages, high(fUserMessages));
    // Anzeigen der Nachricht
    ShowMessage(msg); // Unter Windows ist das nicht Blockend, deswegen muss die Nachricht gelöscht werden bevor sie angezeigt wird
  End;
  (*
   * Wir haben nix zu tun, evtl wollen wir ja gerade einem Spiel beitreten, dann pingen wir mal ein bisschen rum ;)
   *)
  If assigned(game) Then
    Game.OnIdle;
End;

Procedure TForm1.Load_Atomic_Settings;
Var
  SectionBakup: String;
Begin
  // Read the Remote settings from "Launcher" Section
  SectionBakup := IniPropStorage1.IniSection;
  IniPropStorage1.IniSection := 'Launcher';
  Game.Settings.Router_IP := IniPropStorage1.ReadString('Router_IP', '127.0.0.1');
  Game.Settings.Router_Port := IniPropStorage1.ReadString('Router_Port', '5521');
  IniPropStorage1.IniSection := SectionBakup;
  // Default Settings..
  Game.Settings.VolumeValue := IniPropStorage1.ReadInteger('Volume', 10000);
  Game.Settings.TeamPlay := IniPropStorage1.ReadBoolean('TeamPlay', false);
  Game.Settings.RandomStart := IniPropStorage1.ReadBoolean('RandomStart', true);
  Game.Settings.NodeName := IniPropStorage1.ReadString('NodeName', 'Wasteland Blaster');
  Case IniPropStorage1.ReadInteger('Conveyorspeed', 1) Of
    0: Game.Settings.ConveyorSpeed := csSlow;
    1: Game.Settings.ConveyorSpeed := csMiddle;
    2: Game.Settings.ConveyorSpeed := csFast;
  Else
    Game.Settings.ConveyorSpeed := csMiddle;
  End;
  Game.Settings.SchemeFile := IniPropStorage1.ReadString('SchemeFile', 'BASIC.SCH');
  Game.Settings.PlayTime := IniPropStorage1.ReadInteger('PlayTime', 2 * 60 + 30);
  Game.Settings.LostPlayersRevertToAI := IniPropStorage1.ReadBoolean('LostPlayersRevertToAI', false);
  Game.Settings.PlaySounds := IniPropStorage1.ReadBoolean('PlaySounds', true);
  Game.Settings.Keys[ks0] := AtomicDefaultKeys(ks0);
  Game.Settings.Keys[ks0].UseSDL2 := IniPropStorage1.ReadBoolean('UseSDL', Game.Settings.Keys[ks0].UseSDL2);
  Game.Settings.Keys[ks0].KeyUp := IniPropStorage1.ReadInteger('KeyUp', Game.Settings.Keys[ks0].KeyUp);
  Game.Settings.Keys[ks0].KeyDown := IniPropStorage1.ReadInteger('KeyDown', Game.Settings.Keys[ks0].KeyDown);
  Game.Settings.Keys[ks0].KeyLeft := IniPropStorage1.ReadInteger('KeyLeft', Game.Settings.Keys[ks0].KeyLeft);
  Game.Settings.Keys[ks0].KeyRight := IniPropStorage1.ReadInteger('KeyRight', Game.Settings.Keys[ks0].KeyRight);
  Game.Settings.Keys[ks0].KeyPrimary := IniPropStorage1.ReadInteger('KeyPrimary', Game.Settings.Keys[ks0].KeyPrimary);
  Game.Settings.Keys[ks0].KeySecondary := IniPropStorage1.ReadInteger('KeySecondary', Game.Settings.Keys[ks0].KeySecondary);
  If Game.Settings.Keys[ks0].UseSDL2 Then Begin
    Game.Settings.Keys[ks0].Name := IniPropStorage1.ReadString('SDL_Name', Game.Settings.Keys[ks0].Name);
    Game.Settings.Keys[ks0].NameIndex := IniPropStorage1.readInteger('SDL_NameIndex', Game.Settings.Keys[ks0].NameIndex);
    Game.Settings.Keys[ks0].ButtonIndex[0] := IniPropStorage1.readInteger('SDL_First', Game.Settings.Keys[ks0].ButtonIndex[0]);
    Game.Settings.Keys[ks0].ButtonsIdle[0] := IniPropStorage1.ReadBoolean('SDL_FirstIdle', Game.Settings.Keys[ks0].ButtonsIdle[0]);
    Game.Settings.Keys[ks0].ButtonIndex[1] := IniPropStorage1.readInteger('SDL_Second', Game.Settings.Keys[ks0].ButtonIndex[1]);
    Game.Settings.Keys[ks0].ButtonsIdle[1] := IniPropStorage1.ReadBoolean('SDL_SecondIdle', Game.Settings.Keys[ks0].ButtonsIdle[1]);
    Game.Settings.Keys[ks0].AchsisIndex[0] := IniPropStorage1.readInteger('SDL_UpDown', Game.Settings.Keys[ks0].AchsisIndex[0]);
    Game.Settings.Keys[ks0].AchsisIdle[0] := IniPropStorage1.readInteger('SDL_UpDownIdle', Game.Settings.Keys[ks0].AchsisIdle[0]);
    Game.Settings.Keys[ks0].AchsisDirection[0] := IniPropStorage1.readInteger('SDL_UpDownDirection', Game.Settings.Keys[ks0].AchsisDirection[0]);
    Game.Settings.Keys[ks0].AchsisIndex[1] := IniPropStorage1.readInteger('SDL_LeftRight', Game.Settings.Keys[ks0].AchsisIndex[1]);
    Game.Settings.Keys[ks0].AchsisIdle[1] := IniPropStorage1.readInteger('SDL_LeftRightIdle', Game.Settings.Keys[ks0].AchsisIdle[1]);
    Game.Settings.Keys[ks0].AchsisDirection[1] := IniPropStorage1.readInteger('SDL_LeftRightDirection', Game.Settings.Keys[ks0].AchsisDirection[1]);
  End;
  Game.Settings.Keys[ks1] := AtomicDefaultKeys(ks1);
  Game.Settings.Keys[ks1].UseSDL2 := IniPropStorage1.ReadBoolean('UseSDL2', Game.Settings.Keys[ks1].UseSDL2);
  Game.Settings.Keys[ks1].KeyUp := IniPropStorage1.ReadInteger('KeyUp2', Game.Settings.Keys[ks1].KeyUp);
  Game.Settings.Keys[ks1].KeyDown := IniPropStorage1.ReadInteger('KeyDown2', Game.Settings.Keys[ks1].KeyDown);
  Game.Settings.Keys[ks1].KeyLeft := IniPropStorage1.ReadInteger('KeyLeft2', Game.Settings.Keys[ks1].KeyLeft);
  Game.Settings.Keys[ks1].KeyRight := IniPropStorage1.ReadInteger('KeyRight2', Game.Settings.Keys[ks1].KeyRight);
  Game.Settings.Keys[ks1].KeyPrimary := IniPropStorage1.ReadInteger('KeyPrimary2', Game.Settings.Keys[ks1].KeyPrimary);
  Game.Settings.Keys[ks1].KeySecondary := IniPropStorage1.ReadInteger('KeySecondary2', Game.Settings.Keys[ks1].KeySecondary);
  If Game.Settings.Keys[ks1].UseSDL2 Then Begin
    Game.Settings.Keys[ks1].Name := IniPropStorage1.ReadString('SDL_Name2', Game.Settings.Keys[ks1].Name);
    Game.Settings.Keys[ks1].NameIndex := IniPropStorage1.readInteger('SDL_NameIndex2', Game.Settings.Keys[ks1].NameIndex);
    Game.Settings.Keys[ks1].ButtonIndex[0] := IniPropStorage1.readInteger('SDL_First2', Game.Settings.Keys[ks1].ButtonIndex[0]);
    Game.Settings.Keys[ks1].ButtonsIdle[0] := IniPropStorage1.ReadBoolean('SDL_FirstIdle2', Game.Settings.Keys[ks1].ButtonsIdle[0]);
    Game.Settings.Keys[ks1].ButtonIndex[1] := IniPropStorage1.readInteger('SDL_Second2', Game.Settings.Keys[ks1].ButtonIndex[1]);
    Game.Settings.Keys[ks1].ButtonsIdle[1] := IniPropStorage1.ReadBoolean('SDL_SecondIdle2', Game.Settings.Keys[ks1].ButtonsIdle[1]);
    Game.Settings.Keys[ks1].AchsisIndex[0] := IniPropStorage1.readInteger('SDL_UpDown2', Game.Settings.Keys[ks1].AchsisIndex[0]);
    Game.Settings.Keys[ks1].AchsisIdle[0] := IniPropStorage1.readInteger('SDL_UpDownIdle2', Game.Settings.Keys[ks1].AchsisIdle[0]);
    Game.Settings.Keys[ks1].AchsisDirection[0] := IniPropStorage1.readInteger('SDL_UpDownDirection2', Game.Settings.Keys[ks1].AchsisDirection[0]);
    Game.Settings.Keys[ks1].AchsisIndex[1] := IniPropStorage1.readInteger('SDL_LeftRight2', Game.Settings.Keys[ks1].AchsisIndex[1]);
    Game.Settings.Keys[ks1].AchsisIdle[1] := IniPropStorage1.readInteger('SDL_LeftRightIdle2', Game.Settings.Keys[ks1].AchsisIdle[1]);
    Game.Settings.Keys[ks1].AchsisDirection[1] := IniPropStorage1.readInteger('SDL_LeftRightDirection2', Game.Settings.Keys[ks1].AchsisDirection[1]);
  End;
  // Load joystick mappings for ksJoy1
  Game.Settings.Keys[ksJoy1] := AtomicDefaultKeys(ksJoy1);
  Game.Settings.Keys[ksJoy1].UseSDL2 := IniPropStorage1.ReadBoolean('UseSDLJoy1', Game.Settings.Keys[ksJoy1].UseSDL2);
  If Game.Settings.Keys[ksJoy1].UseSDL2 Then Begin
    Game.Settings.Keys[ksJoy1].Name := IniPropStorage1.ReadString('SDL_NameJoy1', Game.Settings.Keys[ksJoy1].Name);
    Game.Settings.Keys[ksJoy1].NameIndex := IniPropStorage1.readInteger('SDL_NameIndexJoy1', Game.Settings.Keys[ksJoy1].NameIndex);
    Game.Settings.Keys[ksJoy1].ButtonIndex[0] := IniPropStorage1.readInteger('SDL_FirstJoy1', Game.Settings.Keys[ksJoy1].ButtonIndex[0]);
    Game.Settings.Keys[ksJoy1].ButtonsIdle[0] := IniPropStorage1.ReadBoolean('SDL_FirstIdleJoy1', Game.Settings.Keys[ksJoy1].ButtonsIdle[0]);
    Game.Settings.Keys[ksJoy1].ButtonIndex[1] := IniPropStorage1.readInteger('SDL_SecondJoy1', Game.Settings.Keys[ksJoy1].ButtonIndex[1]);
    Game.Settings.Keys[ksJoy1].ButtonsIdle[1] := IniPropStorage1.ReadBoolean('SDL_SecondIdleJoy1', Game.Settings.Keys[ksJoy1].ButtonsIdle[1]);
    Game.Settings.Keys[ksJoy1].AchsisIndex[0] := IniPropStorage1.readInteger('SDL_UpDownJoy1', Game.Settings.Keys[ksJoy1].AchsisIndex[0]);
    Game.Settings.Keys[ksJoy1].AchsisIdle[0] := IniPropStorage1.readInteger('SDL_UpDownIdleJoy1', Game.Settings.Keys[ksJoy1].AchsisIdle[0]);
    Game.Settings.Keys[ksJoy1].AchsisDirection[0] := IniPropStorage1.readInteger('SDL_UpDownDirectionJoy1', Game.Settings.Keys[ksJoy1].AchsisDirection[0]);
    Game.Settings.Keys[ksJoy1].AchsisIndex[1] := IniPropStorage1.readInteger('SDL_LeftRightJoy1', Game.Settings.Keys[ksJoy1].AchsisIndex[1]);
    Game.Settings.Keys[ksJoy1].AchsisIdle[1] := IniPropStorage1.readInteger('SDL_LeftRightIdleJoy1', Game.Settings.Keys[ksJoy1].AchsisIdle[1]);
    Game.Settings.Keys[ksJoy1].AchsisDirection[1] := IniPropStorage1.readInteger('SDL_LeftRightDirectionJoy1', Game.Settings.Keys[ksJoy1].AchsisDirection[1]);
  End;
  // Load joystick mappings for ksJoy2
  Game.Settings.Keys[ksJoy2] := AtomicDefaultKeys(ksJoy2);
  Game.Settings.Keys[ksJoy2].UseSDL2 := IniPropStorage1.ReadBoolean('UseSDLJoy2', Game.Settings.Keys[ksJoy2].UseSDL2);
  If Game.Settings.Keys[ksJoy2].UseSDL2 Then Begin
    Game.Settings.Keys[ksJoy2].Name := IniPropStorage1.ReadString('SDL_NameJoy2', Game.Settings.Keys[ksJoy2].Name);
    Game.Settings.Keys[ksJoy2].NameIndex := IniPropStorage1.readInteger('SDL_NameIndexJoy2', Game.Settings.Keys[ksJoy2].NameIndex);
    Game.Settings.Keys[ksJoy2].ButtonIndex[0] := IniPropStorage1.readInteger('SDL_FirstJoy2', Game.Settings.Keys[ksJoy2].ButtonIndex[0]);
    Game.Settings.Keys[ksJoy2].ButtonsIdle[0] := IniPropStorage1.ReadBoolean('SDL_FirstIdleJoy2', Game.Settings.Keys[ksJoy2].ButtonsIdle[0]);
    Game.Settings.Keys[ksJoy2].ButtonIndex[1] := IniPropStorage1.readInteger('SDL_SecondJoy2', Game.Settings.Keys[ksJoy2].ButtonIndex[1]);
    Game.Settings.Keys[ksJoy2].ButtonsIdle[1] := IniPropStorage1.ReadBoolean('SDL_SecondIdleJoy2', Game.Settings.Keys[ksJoy2].ButtonsIdle[1]);
    Game.Settings.Keys[ksJoy2].AchsisIndex[0] := IniPropStorage1.readInteger('SDL_UpDownJoy2', Game.Settings.Keys[ksJoy2].AchsisIndex[0]);
    Game.Settings.Keys[ksJoy2].AchsisIdle[0] := IniPropStorage1.readInteger('SDL_UpDownIdleJoy2', Game.Settings.Keys[ksJoy2].AchsisIdle[0]);
    Game.Settings.Keys[ksJoy2].AchsisDirection[0] := IniPropStorage1.readInteger('SDL_UpDownDirectionJoy2', Game.Settings.Keys[ksJoy2].AchsisDirection[0]);
    Game.Settings.Keys[ksJoy2].AchsisIndex[1] := IniPropStorage1.readInteger('SDL_LeftRightJoy2', Game.Settings.Keys[ksJoy2].AchsisIndex[1]);
    Game.Settings.Keys[ksJoy2].AchsisIdle[1] := IniPropStorage1.readInteger('SDL_LeftRightIdleJoy2', Game.Settings.Keys[ksJoy2].AchsisIdle[1]);
    Game.Settings.Keys[ksJoy2].AchsisDirection[1] := IniPropStorage1.readInteger('SDL_LeftRightDirectionJoy2', Game.Settings.Keys[ksJoy2].AchsisDirection[1]);
  End;
  Game.Settings.ShowFPS := IniPropStorage1.ReadBoolean('ShowFPS', false);
  Game.Settings.CheckForUpdates := IniPropStorage1.ReadBoolean('CheckForUpdates', true);
  Game.Settings.LastPlayedField := IniPropStorage1.ReadString('LastPlayedField', '');
  Game.Settings.LastPlayedFieldHash := StrToInt64Def(IniPropStorage1.ReadString('LastPlayedFieldHash', ''), 0);
  Game.Settings.LastWinsToWinMatch := IniPropStorage1.ReadInteger('LastWinsToWinMatch', 3);

  Game.Settings.Port := IniPropStorage1.ReadInteger('Port', TCPDefaultPort);

  Game.Settings.Fullscreen := IniPropStorage1.ReadBoolean('Fullscreen', false);
  If Game.Settings.Fullscreen Then Begin
{$IFDEF Windows}
    BorderStyle := bsNone;
    SetFullScreen(True);
{$ENDIF}
  End;
End;

Procedure TForm1.IniPropStorage1SavingProperties(Sender: TObject);
Var
  SectionBakup: String;
Begin
  // Read the Remote settings from "Launcher" Section
  SectionBakup := IniPropStorage1.IniSection;
  IniPropStorage1.IniSection := 'Launcher';
  IniPropStorage1.WriteString('Router_IP', Game.Settings.Router_IP);
  IniPropStorage1.WriteString('Router_Port', Game.Settings.Router_Port);
  IniPropStorage1.IniSection := SectionBakup;
  // Default Settings..
  IniPropStorage1.WriteInteger('Volume', Game.Settings.VolumeValue);
  IniPropStorage1.WriteBoolean('TeamPlay', Game.Settings.TeamPlay);
  IniPropStorage1.WriteBoolean('RandomStart', Game.Settings.RandomStart);
  IniPropStorage1.WriteString('NodeName', Game.Settings.NodeName);
  Case Game.Settings.ConveyorSpeed Of
    csSlow: IniPropStorage1.WriteInteger('Conveyorspeed', 0);
    csMiddle: IniPropStorage1.WriteInteger('Conveyorspeed', 1);
    csFast: IniPropStorage1.WriteInteger('Conveyorspeed', 2);
  End;
  // Game.Settings.ConveyorSpeed // TODO: Einstellbar machen
  IniPropStorage1.WriteString('SchemeFile', Game.Settings.SchemeFile);
  IniPropStorage1.WriteInteger('PlayTime', Game.Settings.PlayTime);
  IniPropStorage1.WriteBoolean('LostPlayersRevertToAI', Game.Settings.LostPlayersRevertToAI);
  IniPropStorage1.WriteBoolean('PlaySounds', Game.Settings.PlaySounds);
  IniPropStorage1.WriteInteger('KeyUp', Game.Settings.Keys[ks0].KeyUp);
  IniPropStorage1.WriteInteger('KeyDown', Game.Settings.Keys[ks0].KeyDown);
  IniPropStorage1.WriteInteger('KeyLeft', Game.Settings.Keys[ks0].KeyLeft);
  IniPropStorage1.WriteInteger('KeyRight', Game.Settings.Keys[ks0].KeyRight);
  IniPropStorage1.WriteInteger('KeyPrimary', Game.Settings.Keys[ks0].KeyPrimary);
  IniPropStorage1.WriteInteger('KeySecondary', Game.Settings.Keys[ks0].KeySecondary);
  IniPropStorage1.WriteBoolean('UseSDL', Game.Settings.Keys[ks0].UseSDL2);
  IniPropStorage1.WriteString('SDL_Name', Game.Settings.Keys[ks0].Name);
  IniPropStorage1.WriteInteger('SDL_NameIndex', Game.Settings.Keys[ks0].NameIndex);
  IniPropStorage1.WriteInteger('SDL_First', Game.Settings.Keys[ks0].ButtonIndex[0]);
  IniPropStorage1.WriteBoolean('SDL_FirstIdle', Game.Settings.Keys[ks0].ButtonsIdle[0]);
  IniPropStorage1.WriteInteger('SDL_Second', Game.Settings.Keys[ks0].ButtonIndex[1]);
  IniPropStorage1.WriteBoolean('SDL_SecondIdle', Game.Settings.Keys[ks0].ButtonsIdle[1]);
  IniPropStorage1.WriteInteger('SDL_UpDown', Game.Settings.Keys[ks0].AchsisIndex[0]);
  IniPropStorage1.WriteInteger('SDL_UpDownIdle', Game.Settings.Keys[ks0].AchsisIdle[0]);
  IniPropStorage1.WriteInteger('SDL_UpDownDirection', Game.Settings.Keys[ks0].AchsisDirection[0]);
  IniPropStorage1.WriteInteger('SDL_LeftRight', Game.Settings.Keys[ks0].AchsisIndex[1]);
  IniPropStorage1.WriteInteger('SDL_LeftRightIdle', Game.Settings.Keys[ks0].AchsisIdle[1]);
  IniPropStorage1.WriteInteger('SDL_LeftRightDirection', Game.Settings.Keys[ks0].AchsisDirection[1]);

  IniPropStorage1.WriteInteger('KeyUp2', Game.Settings.Keys[ks1].KeyUp);
  IniPropStorage1.WriteInteger('KeyDown2', Game.Settings.Keys[ks1].KeyDown);
  IniPropStorage1.WriteInteger('KeyLeft2', Game.Settings.Keys[ks1].KeyLeft);
  IniPropStorage1.WriteInteger('KeyRight2', Game.Settings.Keys[ks1].KeyRight);
  IniPropStorage1.WriteInteger('KeyPrimary2', Game.Settings.Keys[ks1].KeyPrimary);
  IniPropStorage1.WriteInteger('KeySecondary2', Game.Settings.Keys[ks1].KeySecondary);
  IniPropStorage1.WriteBoolean('UseSDL2', Game.Settings.Keys[ks1].UseSDL2);
  IniPropStorage1.WriteString('SDL_Name2', Game.Settings.Keys[ks1].Name);
  IniPropStorage1.WriteInteger('SDL_NameIndex2', Game.Settings.Keys[ks1].NameIndex);
  IniPropStorage1.WriteInteger('SDL_First2', Game.Settings.Keys[ks1].ButtonIndex[0]);
  IniPropStorage1.WriteBoolean('SDL_FirstIdle2', Game.Settings.Keys[ks1].ButtonsIdle[0]);
  IniPropStorage1.WriteInteger('SDL_Second2', Game.Settings.Keys[ks1].ButtonIndex[1]);
  IniPropStorage1.WriteBoolean('SDL_SecondIdle2', Game.Settings.Keys[ks1].ButtonsIdle[1]);
  IniPropStorage1.WriteInteger('SDL_UpDown2', Game.Settings.Keys[ks1].AchsisIndex[0]);
  IniPropStorage1.WriteInteger('SDL_UpDownIdle2', Game.Settings.Keys[ks1].AchsisIdle[0]);
  IniPropStorage1.WriteInteger('SDL_UpDownDirection2', Game.Settings.Keys[ks1].AchsisDirection[0]);
  IniPropStorage1.WriteInteger('SDL_LeftRight2', Game.Settings.Keys[ks1].AchsisIndex[1]);
  IniPropStorage1.WriteInteger('SDL_LeftRightIdle2', Game.Settings.Keys[ks1].AchsisIdle[1]);
  IniPropStorage1.WriteInteger('SDL_LeftRightDirection2', Game.Settings.Keys[ks1].AchsisDirection[1]);
  // Save joystick mappings for ksJoy1
  IniPropStorage1.WriteBoolean('UseSDLJoy1', Game.Settings.Keys[ksJoy1].UseSDL2);
  If Game.Settings.Keys[ksJoy1].UseSDL2 Then Begin
    IniPropStorage1.WriteString('SDL_NameJoy1', Game.Settings.Keys[ksJoy1].Name);
    IniPropStorage1.WriteInteger('SDL_NameIndexJoy1', Game.Settings.Keys[ksJoy1].NameIndex);
    IniPropStorage1.WriteInteger('SDL_FirstJoy1', Game.Settings.Keys[ksJoy1].ButtonIndex[0]);
    IniPropStorage1.WriteBoolean('SDL_FirstIdleJoy1', Game.Settings.Keys[ksJoy1].ButtonsIdle[0]);
    IniPropStorage1.WriteInteger('SDL_SecondJoy1', Game.Settings.Keys[ksJoy1].ButtonIndex[1]);
    IniPropStorage1.WriteBoolean('SDL_SecondIdleJoy1', Game.Settings.Keys[ksJoy1].ButtonsIdle[1]);
    IniPropStorage1.WriteInteger('SDL_UpDownJoy1', Game.Settings.Keys[ksJoy1].AchsisIndex[0]);
    IniPropStorage1.WriteInteger('SDL_UpDownIdleJoy1', Game.Settings.Keys[ksJoy1].AchsisIdle[0]);
    IniPropStorage1.WriteInteger('SDL_UpDownDirectionJoy1', Game.Settings.Keys[ksJoy1].AchsisDirection[0]);
    IniPropStorage1.WriteInteger('SDL_LeftRightJoy1', Game.Settings.Keys[ksJoy1].AchsisIndex[1]);
    IniPropStorage1.WriteInteger('SDL_LeftRightIdleJoy1', Game.Settings.Keys[ksJoy1].AchsisIdle[1]);
    IniPropStorage1.WriteInteger('SDL_LeftRightDirectionJoy1', Game.Settings.Keys[ksJoy1].AchsisDirection[1]);
  End;
  // Save joystick mappings for ksJoy2
  IniPropStorage1.WriteBoolean('UseSDLJoy2', Game.Settings.Keys[ksJoy2].UseSDL2);
  If Game.Settings.Keys[ksJoy2].UseSDL2 Then Begin
    IniPropStorage1.WriteString('SDL_NameJoy2', Game.Settings.Keys[ksJoy2].Name);
    IniPropStorage1.WriteInteger('SDL_NameIndexJoy2', Game.Settings.Keys[ksJoy2].NameIndex);
    IniPropStorage1.WriteInteger('SDL_FirstJoy2', Game.Settings.Keys[ksJoy2].ButtonIndex[0]);
    IniPropStorage1.WriteBoolean('SDL_FirstIdleJoy2', Game.Settings.Keys[ksJoy2].ButtonsIdle[0]);
    IniPropStorage1.WriteInteger('SDL_SecondJoy2', Game.Settings.Keys[ksJoy2].ButtonIndex[1]);
    IniPropStorage1.WriteBoolean('SDL_SecondIdleJoy2', Game.Settings.Keys[ksJoy2].ButtonsIdle[1]);
    IniPropStorage1.WriteInteger('SDL_UpDownJoy2', Game.Settings.Keys[ksJoy2].AchsisIndex[0]);
    IniPropStorage1.WriteInteger('SDL_UpDownIdleJoy2', Game.Settings.Keys[ksJoy2].AchsisIdle[0]);
    IniPropStorage1.WriteInteger('SDL_UpDownDirectionJoy2', Game.Settings.Keys[ksJoy2].AchsisDirection[0]);
    IniPropStorage1.WriteInteger('SDL_LeftRightJoy2', Game.Settings.Keys[ksJoy2].AchsisIndex[1]);
    IniPropStorage1.WriteInteger('SDL_LeftRightIdleJoy2', Game.Settings.Keys[ksJoy2].AchsisIdle[1]);
    IniPropStorage1.WriteInteger('SDL_LeftRightDirectionJoy2', Game.Settings.Keys[ksJoy2].AchsisDirection[1]);
  End;

  IniPropStorage1.WriteBoolean('ShowFPS', Game.Settings.ShowFPS);
  IniPropStorage1.WriteBoolean('CheckForUpdates', Game.Settings.CheckForUpdates);
  IniPropStorage1.WriteString('LastPlayedField', Game.Settings.LastPlayedField);
  IniPropStorage1.WriteString('LastPlayedFieldHash', inttostr(Game.Settings.LastPlayedFieldHash));
  IniPropStorage1.WriteInteger('LastWinsToWinMatch', Game.Settings.LastWinsToWinMatch);

  IniPropStorage1.WriteInteger('Port', Game.Settings.Port);
  IniPropStorage1.WriteBoolean('Fullscreen', Game.Settings.Fullscreen);
End;

Procedure TForm1.SetFullScreen(Value: Boolean);
Var
  MaxWidth, MaxHeight: Integer;
Begin
  If value Then Begin
    // On macOS: use large windowed mode instead of true fullscreen to avoid "Game Mode"
    MaxWidth := Screen.Width - 1;
    MaxHeight := Screen.Height - 1;
{$IFDEF Darwin}
    Tform(self).Constraints.MaxWidth := MaxWidth;
    Tform(self).Constraints.MaxHeight := MaxHeight;
{$ENDIF}
    Left := (Screen.Width - MaxWidth) div 2;
    Top := (Screen.Height - MaxHeight) div 2;
    Width := MaxWidth;
    Height := MaxHeight;
    WindowState := wsNormal;
  End
  Else Begin
{$IFDEF Darwin}
    Tform(self).Constraints.MaxWidth := Screen.Width - 1;
    Tform(self).Constraints.MaxHeight := Screen.Height - 1;
{$ENDIF}
    WindowState := wsNormal;
    Left := (Screen.Width - 640) div 2;
    Top := (Screen.Height - 480) div 2;
    Width := 640;
    Height := 480;
  End;
  Game.Settings.Fullscreen := value;
  fWishFullscreen := value;
End;

Procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  // Für Windows noch nicht optimal, ..
  // Siehe auch: https://www.lazarusforum.de/viewtopic.php?f=9&t=14831
  If (key = 13) And (ssAlt In Shift) Then Begin
    If WindowState = wsNormal Then Begin
      SetFullScreen(true);
    End
    Else Begin
      SetFullScreen(false);
    End;
  End;
End;

Procedure TForm1.FormResize(Sender: TObject);
Const
  DesiredWidth = GameWidth;
  DesiredHeight = GameHeight;
Var
  TargetHeight, TargetWidth: Integer;
{$IFDEF Darwin}
  MaxWidth, MaxHeight: Integer;
{$ENDIF}
Begin
{$IFDEF Darwin}
  // Convert fullscreen attempt (green button click) to large windowed mode
  If WindowState = wsFullScreen Then Begin
    WindowState := wsNormal;
    MaxWidth := Screen.Width - 1;
    MaxHeight := Screen.Height - 1;
    Tform(self).Constraints.MaxWidth := MaxWidth;
    Tform(self).Constraints.MaxHeight := MaxHeight;
    Left := (Screen.Width - MaxWidth) div 2;
    Top := (Screen.Height - MaxHeight) div 2;
    Width := MaxWidth;
    Height := MaxHeight;
    fWishFullscreen := True;
    Game.Settings.Fullscreen := True;
    Exit;
  End;
{$ENDIF}
{$IFDEF Windows}
  If fWishFullscreen Then Begin
    WindowState := wsFullScreen;
  End;
{$ENDIF}
  If FAdjustingSize Then
    Exit;
  If WindowState <> wsNormal Then
    Exit;
  FAdjustingSize := true;
  Try
    TargetHeight := Round(ClientWidth * DesiredHeight / DesiredWidth);
    If Abs(TargetHeight - ClientHeight) > 1 Then Begin
      ClientHeight := TargetHeight;
    End
    Else Begin
      TargetWidth := Round(ClientHeight * DesiredWidth / DesiredHeight);
      If Abs(TargetWidth - ClientWidth) > 1 Then
        ClientWidth := TargetWidth;
    End;
  Finally
    FAdjustingSize := false;
  End;
End;

Function TForm1.GetWorkDir(Out Directory: String): Boolean;
Begin
  (*
   * Das Spiel braucht bisher kein "eigenes" Verzeichnis
   *)
  Directory := IncludeTrailingPathDelimiter(GetTempDir(false)) + 'FPC_Atomic';
  result := ForceDirectoriesUTF8(Directory);
End;

Procedure TForm1.HideCursor(Sender: TObject);
Begin
  Cursor := crNone;
  OpenGLControl1.Cursor := crNone;
End;

Procedure TForm1.ShowCursor(Sender: TObject);
Begin
  Cursor := crDefault;
  OpenGLControl1.Cursor := crDefault;
End;

Procedure TForm1.OnConnectToServer(Sender: TObject);
Begin
  log('TForm1.OnConnectToServer', lltrace);
  //MenuItem7.Enabled := True; // New Map
  //MenuItem8.Enabled := True; // Load Map
  //MenuItem9.Enabled := True; // Load game
  //MenuItem19.Enabled := True; // Open Chat
  LogLeave;
End;

Procedure TForm1.OnDisconnectFromServer(Sender: TObject);
Begin
  log('TForm1.OnDisconnectFromServer', lltrace);
  // TODO: Rauswurf in Toplevel Ebene
  //caption := defCaption;
  //MenuItem7.Enabled := false; // New Map
  //MenuItem8.Enabled := false; // Load Map
  //MenuItem18.Enabled := false; // Start / Restart game
  //MenuItem19.Enabled := false; // Open Chat
  //MenuItem20.enabled := false; // Transfer
  ////  MenuItem23.enabled := false; // Restart last Wave
  //MenuItem24.enabled := false; // Continue Game
  //MenuItem25.Enabled := false; // Abort Round
  //MenuItem27.enabled := false; // Save Game
  //MenuItem29.enabled := false; // Map Tex Editor
  //
  //// Form2 = Connect Dialog
  //If form3.Visible Then form3.Close; // Select Map Size Dialog (New Map)
  //If form4.Visible Then form4.Close; // Map Editor Dialog
  //// Form5 = Optionen
  //If form6.Visible Then form6.Close; // Building Editor
  //If form7.Visible Then form7.Close; // Opponent Editor
  //If form8.Visible Then form8.Close; // Game Statistiks
  //If form9.Visible Then form9.Close; // Map Highscores
  //If form10.Visible Then form10.ModalResult := mrCancel; // Load Map Dialog
  //If form11.Visible Then form11.ModalResult := mrCancel; // New Game Dialog mit Spieler Plazierungs auswahl
  //If form12.Visible Then form12.close; // Player infos
  //If form13.Visible Then form13.close; // Chat Dialog
  //If form14.Visible Then form14.close; // Building / Oppenent Übersicht Global / Local
  //If form15.Visible Then form15.close; // Abfrage beim copieren von Opponents / Gebäuden in Unit14
  //If form16.Visible Then form16.close; // Savegame Dialog
  //If form17.Visible Then form17.close; // Map Textrure Generator Dialog

  LogLeave;
End;

Procedure TForm1.AddUserMessage(Msg: String; WarnLevel: TLogLevel);
Begin
  setlength(fUserMessages, length(fUserMessages) + 1);
  fUserMessages[high(fUserMessages)].Msg := Msg;
  fUserMessages[high(fUserMessages)].WarnLevel := WarnLevel;
End;

End.

