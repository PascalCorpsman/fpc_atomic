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
{$IFDEF DARWIN}
  {$modeswitch objectivec1}
{$ENDIF}

{$I globaldefines.inc}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, IniPropStorage,
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
    Procedure FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
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
    FWishFullscreen: Boolean;
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
{$IFDEF DARWIN}
  , CocoaAll
{$ENDIF}
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
  If (allowcnt >= 1) And (Not Initialized) Then Begin
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
    Game.initialize(OpenGLControl1);
    Timer1.Enabled := true;
    allowcnt := 2;
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  s: String;
Begin
  (*
   * Unter Windows kann es vorkommen, dass dieses OnPaint ausgelöst wird obwohl wir noch am Laden in OpenGLControl1MakeCurrent sind
   * Wenn das Passiert, bekommt der User eine Fehlermeldung die nicht stimmt.
   *
   * Zum Glück kann man das Abfangen in dem man hier den Timer1 abprüft und das so verhindert ;)
   *)
  If Not Timer1.Enabled Then exit;
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glcolor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  Game.Render();
  If Game.Settings.ShowFPS Then Begin
    Go2d(OpenGLControl1.Width, OpenGLControl1.Height);
    glBindTexture(GL_TEXTURE_2D, 0);
    OpenGL_ASCII_Font.Color := clwhite;
    glTranslatef(0, 0, atomic_dialog_Layer + atomic_EPSILON);
    s := 'FPS : ' + inttostr(LastFPS_Counter);
{$IFDEF DebuggMode}
    If game.fPlayer[1].UID = -1 Then Begin // Wenn der Spieler 1 eine AI ist ..
      s := s + format(' AI: %0.4f %0.4f', [game.fPlayer[1].Info.Position.x, game.fPlayer[1].Info.Position.y]);
    End;
{$ENDIF}
    OpenGL_ASCII_Font.Textout(5, 5, s);
    Exit2d();
  End;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  FileloggingDir: String;
  i: integer;
Begin
  Randomize;
  ConnectParamsHandled := false;
  Initialized := false; // Wenn True dann ist OpenGL initialisiert
  Form1ShowOnce := true;
  IniPropStorage1.IniFileName := 'fpc_atomic.ini';
{$IFDEF DARWIN}
  IniPropStorage1.IniFileName :=
    IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(False)) + 'fpc_atomic.ini';
  ForceDirectoriesUTF8(ExtractFilePath(IniPropStorage1.IniFileName));
{$ENDIF}
  DefFormat := DefaultFormatSettings;
  DefFormat.DecimalSeparator := '.';
  LogShowHandler := @ShowUserMessage;
  fUserMessages := Nil;
  DefaultFormatSettings.DecimalSeparator := '.';
  InitLogger();
  FileloggingDir := '';
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
  log('TForm1.FormCreate', llInfo);
  log('TForm1.FormCreate', lltrace);
  If FileloggingDir = '' Then Begin
    log('Disabled, file logging.', llWarning);
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
{$IFDEF DARWIN}
var
  View: NSView;
{$ENDIF}
Begin
  If Form1ShowOnce Then Begin
    Form1ShowOnce := false;
    {$IFDEF DARWIN}
    // ensure OpenGL control receives keyboard focus on macOS
    ActiveControl := OpenGLControl1;
    OpenGLControl1.SetFocus;
    if (OpenGLControl1.HandleAllocated) then
    begin
      View := NSView(OpenGLControl1.Handle);
      if Assigned(View) and Assigned(View.window) then
      begin
        View.window.makeFirstResponder(View);
        View.window.orderFrontRegardless;
      end;
    end;
    {$ENDIF}
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

Procedure TForm1.FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  // fallback only when OpenGL control is not the active widget
  If Assigned(Game) And (ActiveControl <> OpenGLControl1) Then
    Game.HandleKeyDown(Sender, Key, Shift);
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
Begin
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

  IniPropStorage1.WriteBoolean('ShowFPS', Game.Settings.ShowFPS);
  IniPropStorage1.WriteBoolean('CheckForUpdates', Game.Settings.CheckForUpdates);
  IniPropStorage1.WriteString('LastPlayedField', Game.Settings.LastPlayedField);
  IniPropStorage1.WriteString('LastPlayedFieldHash', inttostr(Game.Settings.LastPlayedFieldHash));
  IniPropStorage1.WriteInteger('LastWinsToWinMatch', Game.Settings.LastWinsToWinMatch);

  IniPropStorage1.WriteInteger('Port', Game.Settings.Port);
  IniPropStorage1.WriteBoolean('Fullscreen', Game.Settings.Fullscreen);
End;

Procedure TForm1.SetFullScreen(Value: Boolean);
Begin
  If value Then Begin
    // TODO: Klären ob man unter Windows wirklich die Differenzierung braucht
{$IFDEF Windows}
    WindowState := wsMaximized;
{$ELSE}
    WindowState := wsFullScreen;
{$ENDIF}
  End
  Else Begin
    WindowState := wsNormal;
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
Begin
  If fWishFullscreen Then Begin
    WindowState := wsFullScreen;
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

