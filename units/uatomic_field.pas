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
Unit uatomic_field;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils
{$IFDEF Client}
  , uopengl_animation
  , uatomic
{$ENDIF}
  , uatomic_common
{$IFDEF Server}
  , ufifo, uvectormath
  , uai_types
{$ENDIF}

  ;

Type

{$IFDEF Server}
  TIntFifo = specialize TBufferedFifo < Integer > ;
  TPointFifo = specialize TBufferedFifo < Tpoint > ;
  TPlaySoundEffectCallback = Procedure(PlayerIndex: integer; Effect: TSoundEffect) Of Object;
{$ENDIF}

{$IFDEF Client}
  TBrickAnimation = Record
    ani: TOpenGL_Animation;
    Active: Boolean;
  End;
{$ENDIF}

  { TAtomicField }

  TAtomicField = Class
  private
{$IFDEF Server}
    fBombsEnabled: Boolean;
    fBombs: Array[0..FieldWidth * FieldHeight - 1] Of TBombInfo; // Das Array ist eigentlich hoch Dynamisch, aber so allokieren wir nicht andauernd neuen Speicher
    fBombCount: Integer;
    fBombDetonateFifo: TIntFifo;
    fPowerUpClearFifo: TPointFifo;
    fPlaySoundEffect: TPlaySoundEffectCallback;
    fHurryIndex: integer; // Der Index in der HurryCoords Konstante, wenn Hurry Aktiv ist.
{$ENDIF}
    fHasArrows: Boolean; // Wenn True, dann hat die Karte die Lustigen Pfeilchen auf denen die Bomben hin und her geschubst werden..
    fArrowDirs: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of integer; // Die Richtungen der "Pfeile"  -1 = Aus, 0, 90, 180, 270 = Winkel
    fHasConveyors: Boolean; // Wenn True, dann hat die Karte diese Blauen Fließbänder auf denen Spieler und Bomben automatisch bewegt werden (geschwindigkeit via Settings einstellbar)
    fConveyorDirs: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of integer; // Die Richtungen der "Pfeile"  -1 = Aus, 0, 90, 180, 270 = Winkel
    fHasHoles: Boolean; // Wenn True, dann hat die Karte die 4 Löcher, welche den Atomic im Gegenuhrzeigersinn hin und her beamen
    fHastrampolins: Boolean; // Wenn True, dann hat die Karte "trampoline"
    fHoles: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of boolean; // True, an dieser Stelle wird ein "Loch" gezeichnet
{$IFDEF Client}
    fTrampStaticSprite, fHoleTex, fFieldTex, fBrickTex, fSolidTex: integer;
    fxBricks: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of TBrickAnimation;
    fxBrickAniTime: integer;
    fPreviewLines: Array[0..4] Of String;
    fTramp, fArrows, fConveyors: TOpenGL_Animation;
{$ENDIF}
    fSoundFile: String;
    fHash: Uint64; // Wird beim laden der Karte berechnet, dient zur Identifizierung auf "Gleichheit"
    fField: TFieldBricks;
{$IFDEF Client}
    Procedure RenderBlock(x, y: integer; Brick: TBrickData);
    Function OnxBrickOverflow(Sender: TObject): Boolean;
{$ENDIF}
{$IFDEF Server}
    (*
     * False, wenn der Strahl gestoppt wird und nicht mehr "weiter" Laufen darf
     *)
    Function Detonate(x, y: integer; PlayerIndex, ColorIndex: integer; Flame: TFlame): Boolean;
    Function FielWalkable(x, y: integer; AlsoCheckPowerUps: Boolean): Boolean;
{$ENDIF}
  public
    Name: String;
    Available: Boolean; // Wenn True, dann kann die Karte verwendet werden und Alle Spieler haben sie.
{$IFDEF Server}
    StatisticCallback: TStatisticCallback;
    Property BombsEnabled: Boolean read fBombsEnabled;
{$ENDIF}
    Property Hash: UInt64 read fHash; // Pseudo MD5Hash über alle Dateien der Karte
    Property Sound: String read fSoundFile;
    Constructor Create(
{$IFDEF Server}
      PlaySoundEffectCallback: TPlaySoundEffectCallback;
      sStatisticCallback: TStatisticCallback
{$ENDIF}
      ); virtual;

    Destructor Destroy(); override;
    Function loadFromDirectory(Dir: String
{$IFDEF Client}
      ; Const aArrows: TOpenGL_Animation; Const aConveyors: TOpenGL_Animation; Const aTramp: TOpenGL_Animation; Const aHohle, aTrampStatic: Integer
{$ENDIF}
      ): Boolean;
{$IFDEF Client}
    Procedure RenderPreview; virtual;
    Procedure Render(Const Atomics: TAtomics; PowerTexs: TPowerTexArray);
    Procedure ReadGameingData(Const Stream: TStream);
    Procedure Reset(); // Wie Initialize nur eben die Client version
{$ENDIF}
{$IFDEF Server}
    Procedure Initialize(Const Players: TPlayers; Const Scheme: TScheme); // Setzt die Spielerpositionen gleich mit
    Procedure AppendGamingData(Const Stream: TStream);
    Function HandleMovePlayer(Var Players: TPlayers; PlayerIndex: integer; ConveyorSpeed: TConveyorSpeed): Boolean; // True, wenn der Spieler gestorben ist.
    Procedure HandleActionPlayer(Var Player: TPlayer; PlayerIndex: integer);
    Procedure HandleBombs(Var Players: TPlayers; PreHurry: Boolean; ConveyorSpeed: TConveyorSpeed);
    Procedure HandlePlayerVsMap(Var Players: TPlayers; PlayerGetsPowerUp: TPlayerGetsPowerUpEvent);
    Procedure HandleFieldAnims;
    Procedure DisableAllBombs();
    Function GetAiInfo(Const Players: TPlayers; TeamPlay: Boolean): TaiInfo;
    Procedure IncHurry();
    Procedure KillPlayer(Var Players: TPlayers; Index: integer); // Ohne Punktewertung
    Procedure RepopulatePlayersCollectedPowerUps(Const Players: TPlayers; PlayerIndex: Integer);
    Procedure TelePortPlayer(Var Player: TPlayer);
{$ENDIF}
  End;

  { TAtomicRandomField }

  TAtomicRandomField = Class(TAtomicField)
  private
{$IFDEF Client}
    fPreviewGrid: Array[0..4, 0..4] Of Integer;
{$ENDIF}
  public
{$IFDEF Client}
    Procedure CreatePreview(Const Fields: Array Of TAtomicField);
    Procedure RenderPreview; override;
{$ENDIF}
  End;

Implementation

Uses
  Graphics, IniFiles, md5
{$IFDEF server}
  , math
{$ENDIF}
{$IFDEF Client}
  , uvectormath
  , dglOpenGL
  , uopengl_graphikengine
  , uopengl_spriteengine
  , ugraphics
{$ENDIF}
  ;

{$IFDEF server}
Const
  (*
   * Beschreibt einen Weg von Links oben schneckenmäßig im Uhrzeigersinn bis nach innen rein, bis nur noch 5 Felder in der Mitte ügrig sind.
   *)
  HurryCoords: Array[0..159] Of Tpoint = (
    (x: 0; y: 0), (x: 1; y: 0), (x: 2; y: 0), (x: 3; y: 0), (x: 4; y: 0), (x: 5; y: 0), (x: 6; y: 0), (x: 7; y: 0), (x: 8; y: 0), (x: 9; y: 0), (x: 10; y: 0), (x: 11; y: 0), (x: 12; y: 0), (x: 13; y: 0), (x: 14; y: 0),
    (x: 14; y: 1), (x: 14; y: 2), (x: 14; y: 3), (x: 14; y: 4), (x: 14; y: 5), (x: 14; y: 6), (x: 14; y: 7), (x: 14; y: 8), (x: 14; y: 9), (x: 14; y: 10),
    (x: 13; y: 10), (x: 12; y: 10), (x: 11; y: 10), (x: 10; y: 10), (x: 9; y: 10), (x: 8; y: 10), (x: 7; y: 10), (x: 6; y: 10), (x: 5; y: 10), (x: 4; y: 10), (x: 3; y: 10), (x: 2; y: 10), (x: 1; y: 10), (x: 0; y: 10),
    (x: 0; y: 9), (x: 0; y: 8), (x: 0; y: 7), (x: 0; y: 6), (x: 0; y: 5), (x: 0; y: 4), (x: 0; y: 3), (x: 0; y: 2), (x: 0; y: 1),
    (x: 1; y: 1), (x: 2; y: 1), (x: 3; y: 1), (x: 4; y: 1), (x: 5; y: 1), (x: 6; y: 1), (x: 7; y: 1), (x: 8; y: 1), (x: 9; y: 1), (x: 10; y: 1), (x: 11; y: 1), (x: 12; y: 1), (x: 13; y: 1),
    (x: 13; y: 2), (x: 13; y: 3), (x: 13; y: 4), (x: 13; y: 5), (x: 13; y: 6), (x: 13; y: 7), (x: 13; y: 8), (x: 13; y: 9),
    (x: 12; y: 9), (x: 11; y: 9), (x: 10; y: 9), (x: 9; y: 9), (x: 8; y: 9), (x: 7; y: 9), (x: 6; y: 9), (x: 5; y: 9), (x: 4; y: 9), (x: 3; y: 9), (x: 2; y: 9), (x: 1; y: 9),
    (x: 1; y: 8), (x: 1; y: 7), (x: 1; y: 6), (x: 1; y: 5), (x: 1; y: 4), (x: 1; y: 3), (x: 1; y: 2),
    (x: 2; y: 2), (x: 3; y: 2), (x: 4; y: 2), (x: 5; y: 2), (x: 6; y: 2), (x: 7; y: 2), (x: 8; y: 2), (x: 9; y: 2), (x: 10; y: 2), (x: 11; y: 2), (x: 12; y: 2),
    (x: 12; y: 3), (x: 12; y: 4), (x: 12; y: 5), (x: 12; y: 6), (x: 12; y: 7), (x: 12; y: 8),
    (x: 11; y: 8), (x: 10; y: 8), (x: 9; y: 8), (x: 8; y: 8), (x: 7; y: 8), (x: 6; y: 8), (x: 5; y: 8), (x: 4; y: 8), (x: 3; y: 8), (x: 2; y: 8),
    (x: 2; y: 7), (x: 2; y: 6), (x: 2; y: 5), (x: 2; y: 4), (x: 2; y: 3),
    (x: 3; y: 3), (x: 4; y: 3), (x: 5; y: 3), (x: 6; y: 3), (x: 7; y: 3), (x: 8; y: 3), (x: 9; y: 3), (x: 10; y: 3), (x: 11; y: 3),
    (x: 11; y: 4), (x: 11; y: 5), (x: 11; y: 6), (x: 11; y: 7),
    (x: 10; y: 7), (x: 9; y: 7), (x: 8; y: 7), (x: 7; y: 7), (x: 6; y: 7), (x: 5; y: 7), (x: 4; y: 7), (x: 3; y: 7),
    (x: 3; y: 6), (x: 3; y: 5), (x: 3; y: 4),
    (x: 4; y: 4), (x: 5; y: 4), (x: 6; y: 4), (x: 7; y: 4), (x: 8; y: 4), (x: 9; y: 4), (x: 10; y: 4),
    (x: 10; y: 5), (x: 10; y: 6),
    (x: 9; y: 6), (x: 8; y: 6), (x: 7; y: 6), (x: 6; y: 6), (x: 5; y: 6), (x: 4; y: 6),
    (x: 4; y: 5)
    );
{$ENDIF}

{$IFDEF Client}

  { TAtomicRandomField }

Procedure TAtomicRandomField.CreatePreview(Const Fields: Array Of TAtomicField);
Var
  index, i, j: Integer;
Begin
  // Irgend eine Hintergrund Graphik Wählen
  fFieldTex := Fields[random(Length(Fields))].fFieldTex;
  // Irgend welche Bricks / Solids nehmen
  For i := 0 To 4 Do Begin
    For j := 0 To 4 Do Begin
      index := random(Length(Fields));
      If ((i = 1) And (j = 1)) Or
        ((i = 3) And (j = 1)) Or
        ((i = 1) And (j = 3)) Or
        ((i = 3) And (j = 3)) Then Begin
        fPreviewGrid[i, j] := Fields[index].fSolidTex;
      End
      Else Begin
        fPreviewGrid[i, j] := Fields[index].fBrickTex;
      End;
    End;
  End;
  // Ein Paar Felder sollen auch "Leer" bleiben
  fPreviewGrid[0, 0] := 0;
  fPreviewGrid[1, 0] := 0;
  fPreviewGrid[0, 1] := 0;
  fPreviewGrid[0, 3] := 0;
  fPreviewGrid[1, 4] := 0;
End;

Procedure TAtomicRandomField.RenderPreview;

Var
  j, i: Integer;
Begin
  (*
   * Die Hintergrund Graphik kann auf jeden Fall gerendert werden..
   *)
  glColor3f(1, 1, 1);
  glpushmatrix();
  glTranslatef(0, 0, atomic_Map_Layer);
  RenderQuad(v2(0, 0), v2(GameWidth, GameHeight), 0, false, fFieldTex);
  For j := 0 To 4 Do Begin
    For i := 0 To 4 Do Begin
      If fPreviewGrid[i, j] = 0 Then Continue;
      glPushMatrix;
      glTranslatef(Fieldxoff + i * FieldBlockWidth, FieldyOff + j * FieldBlockHeight, atomic_EPSILON);
      RenderAlphaQuad(v2(FieldBlockWidth / 2, FieldBlockHeight / 2), FieldBlockWidth, -FieldBlockHeight, 0, fPreviewGrid[i, j]);
      glPopMatrix;
    End;
  End;
  glpopmatrix();
End;
{$ENDIF}

{ TAtomicField }

// Die IDE Code vervollständigung killt manchmal den Korrekten Header, deswegen hier die "Kopiervorlage"
//Constructor TAtomicField.Create(
//{$IFDEF Server}
//  PlaySoundEffectCallback: TPlaySoundEffectCallback;
//  sStatisticCallback: TStatisticCallback
//{$ENDIF}
//  );

Constructor TAtomicField.Create(
{$IFDEF Server}
  PlaySoundEffectCallback: TPlaySoundEffectCallback;
  sStatisticCallback: TStatisticCallback
{$ENDIF}
  );
Var
  i, j: integer;
Begin
{$IFDEF Client}
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      fxBricks[i, j].ani := Nil;
    End;
  End;
{$ENDIF}
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      fArrowDirs[i, j] := -1;
      fConveyorDirs[i, j] := -1;
      fHoles[i, j] := false;
    End;
  End;
  // Die 4 Löcher
  fHoles[2, 2] := true;
  fHoles[12, 2] := true;
  fHoles[12, 8] := true;
  fHoles[2, 8] := true;
  // Alle nach Rechts
  fArrowDirs[2, 0] := 0;
  fArrowDirs[6, 0] := 0;
  fArrowDirs[10, 0] := 0;
  fArrowDirs[0, 2] := 0;
  fArrowDirs[4, 2] := 0;
  fArrowDirs[8, 2] := 0;
  fArrowDirs[12, 2] := 0;
  fArrowDirs[4, 4] := 0;
  fArrowDirs[0, 6] := 0;
  fArrowDirs[8, 6] := 0;
  fArrowDirs[12, 6] := 0;
  // Alle nach Oben
  fArrowDirs[2, 2] := 90;
  fArrowDirs[6, 2] := 90;
  fArrowDirs[10, 2] := 90;
  fArrowDirs[0, 4] := 90;
  fArrowDirs[2, 6] := 90;
  fArrowDirs[4, 6] := 90;
  fArrowDirs[10, 6] := 90;
  fArrowDirs[0, 8] := 90;
  fArrowDirs[2, 10] := 90;
  fArrowDirs[6, 10] := 90;
  fArrowDirs[10, 10] := 90;
  // Alle nach Links
  fArrowDirs[2, 4] := 180;
  fArrowDirs[10, 4] := 180;
  fArrowDirs[14, 4] := 180;
  fArrowDirs[6, 6] := 180;
  fArrowDirs[2, 8] := 180;
  fArrowDirs[6, 8] := 180;
  fArrowDirs[10, 8] := 180;
  fArrowDirs[14, 8] := 180;
  fArrowDirs[4, 10] := 180;
  fArrowDirs[8, 10] := 180;
  fArrowDirs[12, 10] := 180;
  // Alle nach unten
  fArrowDirs[4, 0] := 270;
  fArrowDirs[8, 0] := 270;
  fArrowDirs[12, 0] := 270;
  fArrowDirs[14, 2] := 270;
  fArrowDirs[6, 4] := 270;
  fArrowDirs[8, 4] := 270;
  fArrowDirs[12, 4] := 270;
  fArrowDirs[14, 6] := 270;
  fArrowDirs[4, 8] := 270;
  fArrowDirs[8, 8] := 270;
  fArrowDirs[12, 8] := 270;
  // Nach Rechts
  For i := 2 To 11 Do Begin
    fConveyorDirs[i, 2] := 0;
  End;
  // Nach Unten
  For j := 2 To 7 Do Begin
    fConveyorDirs[12, j] := 270;
  End;
  // Nach Links
  For i := 3 To 12 Do Begin
    fConveyorDirs[i, 8] := 180;
  End;
  // Nach oben
  For j := 3 To 8 Do Begin
    fConveyorDirs[2, j] := 90;
  End;
  Name := '';
  fHash := 0;
{$IFDEF Server}
  StatisticCallback := sStatisticCallback;
  fPlaySoundEffect := PlaySoundEffectCallback;
  fBombCount := 0;
  fBombDetonateFifo := TIntFifo.create(128);
  fPowerUpClearFifo := TPointFifo.create(128);
{$ENDIF}
End;

Destructor TAtomicField.Destroy;
{$IFDEF Client}
Var
  i, j: Integer;
{$ENDIF}
Begin
{$IFDEF Client}
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do
      If assigned(fxBricks[i, j].ani) Then fxBricks[i, j].ani.free;
{$ENDIF}
{$IFDEF Server}
  fBombDetonateFifo.free;
  fPowerUpClearFifo.free;
{$ENDIF}
  Inherited Destroy;
End;

// Die IDE Code vervollständigung killt manchmal den Korrekten Header, deswegen hier die "Kopiervorlage"
//Function TAtomicField.loadFromDirectory(Dir: String
//{$IFDEF Client}
//  ; Const aArrows: TOpenGL_Animation; Const aConveyors: TOpenGL_Animation; Const aTramp: TOpenGL_Animation; Const aHohle, aTrampStatic: Integer
//{$ENDIF}
//  ): Boolean;

Function TAtomicField.loadFromDirectory(Dir: String
{$IFDEF Client}
  ; Const aArrows: TOpenGL_Animation; Const aConveyors: TOpenGL_Animation; Const aTramp: TOpenGL_Animation; Const aHohle, aTrampStatic: Integer
{$ENDIF}
  ): Boolean;
Var
  tmphash: TMD5Digest;

  Procedure AppendHash;
  Var
    val: UInt64;
    i: Integer;
  Begin
    val := 0;
    For i := 0 To 7 Do Begin
      val := val Shl 8;
      val := val Or tmphash[i];
    End;
    fHash := fHash Xor val;

    val := 0;
    For i := 8 To 15 Do Begin
      val := val Shl 8;
      val := val Or tmphash[i];
    End;
    fHash := fHash Xor val;
  End;

Var
  ini: TIniFile;
{$IFDEF Client}
  i, j: Integer;
  xBrick: TOpenGL_Animation;
{$ENDIF}
Begin
  result := false;
{$IFDEF Client}
  fArrows := aArrows;
  fConveyors := aConveyors;
  fTramp := aTramp;
  fTrampStaticSprite := aTrampStatic;
{$ENDIF}
  fHash := 0;
  dir := IncludeTrailingPathDelimiter(dir);

  If Not FileExists(dir + 'brick.png') Then exit;
{$IFDEF Client}
  fBrickTex := OpenGL_GraphikEngine.LoadAlphaColorGraphik(dir + 'brick.png', ColorToRGB(clfuchsia), smStretchHard);
{$ENDIF}
  tmphash := MD5File(dir + 'brick.png');
  AppendHash;

  If Not FileExists(dir + 'field.png') Then exit;
{$IFDEF Client}
  fFieldTex := OpenGL_GraphikEngine.LoadGraphik(dir + 'field.png', smStretchHard);
{$ENDIF}
  tmphash := MD5File(dir + 'field.png');
  AppendHash;

  If Not FileExists(dir + 'info.txt') Then exit;
  ini := TIniFile.Create(dir + 'info.txt');
  name := ini.ReadString('General', 'Name', '');
  fHasArrows := ini.ReadBool('General', 'HasArrows', false);
  fHasConveyors := ini.ReadBool('General', 'HasConveyors', false);
  fHasHoles := ini.ReadBool('General', 'HasHoles', false);
  fHastrampolins := ini.ReadBool('General', 'Hastrampolins', false);

  // T
{$IFDEF Client}
  For i := 0 To 4 Do Begin
    fPreviewLines[i] := ini.ReadString('Preview', 'Row' + inttostr(i + 1), '.....');
  End;
{$ENDIF}
  ini.free;
  tmphash := MD5File(dir + 'info.txt');
  AppendHash;
  If name = '' Then exit; // Name ='' und Hash = 0 ist reserviert für das Random Field

  If Not FileExists(dir + 'solid.png') Then exit;
{$IFDEF Client}
  fSolidTex := OpenGL_GraphikEngine.LoadAlphaColorGraphik(dir + 'solid.png', ColorToRGB(clfuchsia), smStretchHard);
  fHoleTex := aHohle;
{$ENDIF}
  tmphash := MD5File(dir + 'solid.png');
  AppendHash;

  fSoundFile := Dir + 'sound.wav';
  If Not FileExists(fSoundFile) Then exit;
  tmphash := MD5File(fSoundFile);
  AppendHash;

  If Not FileExists(dir + 'xbrick.ani') Then exit;
{$IFDEF Client}
  xBrick := TOpenGL_Animation.Create;
  xBrick.LoadFromFile(dir + 'xbrick.ani');
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do Begin
      fxBricks[i, j].ani := TOpenGL_Animation.Create;
      fxBricks[i, j].ani.CloneFrom(xBrick);
      fxBricks[i, j].ani.Tag := i + j * FieldWidth;
      // TODO: Das Problem ist wenn der Server zu Langsam ist scheint das nicht zu gehen ...
      fxBricks[i, j].ani.OnAnimationOverflowEvent := @OnxBrickOverflow;
      fxBricks[i, j].Active := false;
    End;
  fxBrickAniTime := xBrick.Sprite[0].FrameCount * xBrick.Sprite[0].TimePerFrame;
  xBrick.Free;
{$ENDIF}
  tmphash := MD5File(dir + 'xbrick.ani');
  AppendHash;

  result := true;
End;

{$IFDEF Server}

Function TAtomicField.Detonate(x, y: integer; PlayerIndex, ColorIndex: integer;
  Flame: TFlame): Boolean;
Var
  i: Integer;
Begin
  result := false;
  If (x < 0) Or (y < 0) Or (x > FieldWidth - 1) Or (y > FieldHeight - 1) Then exit;
  (*
   * Durch Analyse von: https://www.youtube.com/watch?v=qOtCVIMFJu0 bei sec 0.37
   * Flamen "überschreiben" sich gegenseitig
   * Powerups verbrennen sofort ohne Flamme
   *)
  result := true;
  Case fField[x, y].BrickData Of
    bdBlank: Begin
        If fField[x, y].PowerUp = puNone Then Begin
          fField[x, y].FlameColor := ColorIndex;
          fField[x, y].FlamePlayer := PlayerIndex;
          If Not (fCross In fField[x, y].Flame) Then Begin // Sonst sieht es komisch aus ..
            fField[x, y].Flame := [Flame];
          End;
          fField[x, y].Counter := 0;
        End
        Else Begin
          fPowerUpClearFifo.Push(point(x, y));
          //          fField[x, y].PowerUp := puNone; -- Das geht net, da sonst 2 Bombem hintereinander durch das Popup durchschießen können !
          result := false; // Egal wie der Strahl wird hier gestoppt !
        End;
      End;
    bdBrick: Begin
        If Not fField[x, y].Exploding Then Begin
          StatisticCallback(sBricksDestroyed);
        End;
        fField[x, y].Exploding := true;
        fField[x, y].ExplodingRenderFlag := true;
        // fField[x, y].BrickData := bdBlank; // der darf nicht Gleich weg genommen werden, da sonst  2 Bomben auch 2 Steine auf einmal wegsprengen !
        fField[x, y].Counter := 0;
        result := False;
      End;
    bdSolid: result := false; // Ein Nicht zerstörbarer Brick, der ist einfach ;)
  End;
  // Auslösen von Kettenreaktionen
  For i := 0 To fBombCount - 1 Do Begin
    If Not fBombs[i].Detonated Then Begin
      If (trunc(fBombs[i].Position.x) = x) And (trunc(fBombs[i].Position.y) = y) Then Begin
        fBombDetonateFifo.push(i);
      End;
    End;
  End;
End;

(*
 * True, wenn man auf diese Koordinaten laufen kann
 * es Kann belegt sein von einer Bombe und oder einem Stein
 *)

Function TAtomicField.FielWalkable(x, y: integer; AlsoCheckPowerUps: Boolean
  ): Boolean;
Var
  i: Integer;
Begin
  result := false;
  If (x < 0) Or (x > FieldWidth - 1) Or (y < 0) Or (y > FieldHeight - 1) Then exit;
  result := fField[x, y].BrickData = bdBlank;
  If Not result Then exit;
  If AlsoCheckPowerUps Then Begin
    result := fField[x, y].PowerUp = puNone;
  End;
  If Not result Then exit;
  // Prüfung auf Bomben, reicht das so schon ?
  For i := 0 To fBombCount - 1 Do Begin
    If (x = trunc(fBombs[i].Position.x)) And
      (y = trunc(fBombs[i].Position.y)) And
      (fBombs[i].MoveDir <> bmFly) Then Begin
      result := false;
      exit;
    End;
  End;
End;

Procedure TAtomicField.Initialize(Const Players: TPlayers; Const Scheme: TScheme
  );
  Function SetBlank(x, y: integer): Boolean;
  Begin
    result := false;
    If (x >= 0) And (x < FieldWidth) And (y >= 0) And (y < FieldHeight) Then Begin
      fField[x, y].BrickData := bdBlank;
      result := true;
    End;
  End;

  Function IsBlank(x, y: integer): boolean;
  Begin
    result := false;
    If (x >= 0) And (x < FieldWidth) And (y >= 0) And (y < FieldHeight) Then Begin
      result := fField[x, y].BrickData = bdBlank;
    End;
  End;

Var
  px, py, i, j, maxpow: Integer;
  b: Boolean;
Begin
{$IFDEF Server}
  fBombCount := 0; // Sollte es je noch Bomben gegeben haben nu sind sie Platt ;)
  fBombsEnabled := true;
  fHurryIndex := -1;
{$ENDIF}
  // Die Felder mit Elementen "Bevölkern"
  // 1. Brick / Solid / Blank
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      fField[i, j].Exploding := false;
      fField[i, j].ExplodingRenderFlag := false;
      fField[i, j].Counter := 0;
      fField[i, j].PowerUp := puNone;
      fField[i, j].Flame := [];
      fField[i, j].FlameColor := 0;
      fField[i, j].FlamePlayer := -1;
      fField[i, j].Tramp := false;
      fField[i, j].TrampRunning := false;
      fField[i, j].TrampOffset := random(256);
      Case Scheme.BrickData[i, j] Of
        bdSolid: fField[i, j].BrickData := bdSolid;
        bdBlank: fField[i, j].BrickData := bdBlank;
        bdBrick: Begin
            If Scheme.BrickDensity > random(100) Then Begin
              fField[i, j].BrickData := bdBrick;
            End
            Else Begin
              fField[i, j].BrickData := bdBlank;
            End;
          End;
      End;
    End;
  End;
  // 2. Die Spielerpositionen "Frei" Räumen
  For i := 0 To high(Players) Do Begin
    If Players[i].Info.Alive Then Begin
      SetBlank(trunc(Players[i].Info.Position.x), trunc(Players[i].Info.Position.y));
      SetBlank(trunc(Players[i].Info.Position.x) - 1, trunc(Players[i].Info.Position.y));
      SetBlank(trunc(Players[i].Info.Position.x) + 1, trunc(Players[i].Info.Position.y));
      SetBlank(trunc(Players[i].Info.Position.x), trunc(Players[i].Info.Position.y - 1));
      SetBlank(trunc(Players[i].Info.Position.x), trunc(Players[i].Info.Position.y + 1));
    End;
  End;
  // 2.5 Wenn die Karte Trampoline hat, dann verteilen wir diese nun
  If fHastrampolins Then Begin
    For i := 0 To FieldTrampCount - 1 Do Begin
      px := -1;
      While px = -1 Do Begin
        px := random(FieldWidth);
        py := random(FieldHeight);
        // Verhindern, dass Trampoline an Aktiven Spieler Positionen erstellt werden
        For j := 0 To high(Players) Do Begin
          If Players[j].Info.Alive Then Begin
            If (trunc(Players[j].Info.Position.x) = px) And
              (trunc(Players[j].Info.Position.x) = px) Then Begin
              px := -1;
              break;
            End;
          End;
        End;
        If (px <> -1) And fField[px, py].Tramp Then px := -1; // Sicherstellen, dass es auch wirklich FieldTrampCount werden !
      End;
      fField[px, py].Tramp := true;
    End;
  End;
  // 2.5 Wenn die Karte "löcher" hat, dann müssen die immer Frei bleiben, und mindestens 1 adjazentes Feld auch !
  If fHasHoles Then Begin
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        If fHoles[i, j] Then Begin
          SetBlank(i, j);
          If (Not IsBlank(i - 1, j)) And (Not IsBlank(i + 1, j))
            And (Not IsBlank(i, j - 1)) And (Not IsBlank(i, j + 1)) Then Begin
            b := false;
            While Not b Do Begin
              Case Random(4) Of
                0: b := SetBlank(i - 1, j);
                1: b := SetBlank(i + 1, j);
                2: b := SetBlank(i, j - 1);
                3: b := SetBlank(i, j + 1);
              End;
            End;
          End;
        End;
      End;
    End;
  End;
  // 3. Die PowerUps die Hinter den "Bricks" liegen
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      If fField[i, j].BrickData = bdBrick Then Begin // Nur Wo Wegsprengbare Steine sind, können Powerups drunder sein..
        (* Die Random Powerups sind "Abschaltbar" *)
        If Scheme.PowerUps[purandom].BornWith > 0 Then Begin
          maxpow := 15;
        End
        Else Begin
          maxpow := 14;
        End;
        Case random(maxpow) Of
          0: fField[i, j].PowerUp := puNone;
          1: fField[i, j].PowerUp := puExtraBomb;
          2: fField[i, j].PowerUp := puLongerFlameLength;
          3: fField[i, j].PowerUp := puDisease;
          4: fField[i, j].PowerUp := puCanCick;
          5: fField[i, j].PowerUp := puExtraSpeed;
          6: fField[i, j].PowerUp := puCanPunch;
          7: fField[i, j].PowerUp := puCanGrab;
          8: fField[i, j].PowerUp := puCanSpooger;
          9: fField[i, j].PowerUp := puGoldFlame;
          10: fField[i, j].PowerUp := puTrigger;
          11: fField[i, j].PowerUp := puCanJelly;
          12: fField[i, j].PowerUp := puSuperBadDisease;
          13: fField[i, j].PowerUp := puSlow;
          14: fField[i, j].PowerUp := purandom;
        End;
        // TODO: das sollte ggf noch feiner Justiert werden ..
        If fField[i, j].PowerUp <> puNone Then Begin
          If Scheme.PowerUps[fField[i, j].PowerUp].Forbidden Then fField[i, j].PowerUp := puNone;
        End;
      End;
    End;
  End;
End;

Procedure TAtomicField.AppendGamingData(Const Stream: TStream);
Var
  i, j: integer;
Begin
  (*
   * Da auf jedem Feld immer nur 1 Ding Gerendert werden kann ist das hier recht Easy ;)
   *)
  stream.Write(fField, SizeOf(fField));
  // Sicherstellen, dass das ExplodingRenderFlag nur als Flanke raus geht (Also genau 1 mal gesetzt ist !)
  For i := 0 To high(fField) Do Begin
    For j := 0 To high(fField[i]) Do Begin
      fField[i, j].ExplodingRenderFlag := false;
    End;
  End;
{$IFDEF Server}
  (*
   * Wir Kopieren nur die Gültigen Bomben raus
   *)
  stream.Write(fBombCount, SizeOf(fBombCount));
  For i := 0 To fBombCount - 1 Do Begin
    stream.Write(fBombs[i].ColorIndex, SizeOf(fBombs[i].ColorIndex));
    stream.Write(fBombs[i].Position, SizeOf(fBombs[i].Position));
    stream.Write(fBombs[i].Animation, SizeOf(fBombs[i].Animation));
    stream.Write(fBombs[i].AnimationOffset, SizeOf(fBombs[i].AnimationOffset));
  End;
{$ENDIF}
End;

Function TAtomicField.HandleMovePlayer(Var Players: TPlayers; PlayerIndex: integer; ConveyorSpeed: TConveyorSpeed): Boolean;

  Function GetBombIndex(x, y: integer): integer;
  Var
    i: Integer;
  Begin
    result := -1;
    For i := 0 To fBombCount - 1 Do Begin
      If (trunc(fBombs[i].Position.x) = x) And
        (trunc(fBombs[i].Position.y) = y) And
        (fBombs[i].MoveDir <> bmFly) Then Begin // Fliegende Bomben haben keine Kollisionen !
        result := i;
        exit;
      End;
    End;
  End;

Const
  Epsilon = 0.25; // Je Größer dieser Wert, desto mehr wird das Umlaufen der Ecken unterstützt (maximal wäre 0.5 möglich)
Var
  s, dx, dy, cSpeed, commaparty, commapartx, rSpeed: Single;
  dxi, dyi, nx, ny, x, y, index: Integer;

Begin
  result := false;

  If Players[PlayerIndex].Flying Then Begin
    s := Players[PlayerIndex].Info.Counter / AtomicTrampFlyTime;
    dx := Players[PlayerIndex].FlyTarget.x - Players[PlayerIndex].FlyStart.x;
    dy := Players[PlayerIndex].FlyTarget.y - Players[PlayerIndex].FlyStart.y;
    Players[PlayerIndex].Info.Position.x := Players[PlayerIndex].FlyStart.x + dx * s;
    Players[PlayerIndex].Info.Position.y := Players[PlayerIndex].FlyStart.y + dy * s - (FieldHeight * 3) * sin(pi * s); // Die Bogenhöhe beim Trampolin
    // Der Flug ist Beendet, sicherstellen, dass wir auf jeden Fall am Ziel angekommen sind.
    If Players[PlayerIndex].Info.Counter > AtomicTrampFlyTime Then Begin
      Players[PlayerIndex].Info.Position := Players[PlayerIndex].FlyTarget;
      Players[PlayerIndex].Flying := false;
    End;
    exit; // Während des Fluges hat der Spieler natürlich keine Kontrolle über sich ;)
  End;

  // Die Kick Animation -> da bewegen wir uns erst mal nicht ;)
  If (Players[PlayerIndex].Info.Animation = raKick) And (Players[PlayerIndex].Info.Counter < AtomicAnimationTimeKick - 2 * UpdateRate) Then exit; // A Bissle schneller wieder "Frei" geben als geplant
  If (Players[PlayerIndex].Info.Animation = raPup) And (Players[PlayerIndex].Info.Counter < AtomicAnimationTimePup - 2 * UpdateRate) Then exit; // A Bissle schneller wieder "Frei" geben als geplant
  If (Players[PlayerIndex].Info.Animation = raTeleport) And (Players[PlayerIndex].Info.Counter < AtomicAnimationTimeTeleport - 2 * UpdateRate) Then exit; // A Bissle schneller wieder "Frei" geben als geplant

  (*
   * Kickt der Spieler eine Bombe auf einem Laufband addieren sich die Geschwindigkeiten !
   *)
  cSpeed := 0;
  If fHasConveyors And (Not Players[PlayerIndex].Flying) Then Begin
    Case ConveyorSpeed Of
      csSlow: cSpeed := ConveyorSlowSpeed / FrameRate; // So Umrechnen dass Speed = Kachel Pro Sekunde ist.
      csMiddle: cSpeed := ConveyorMiddleSpeed / FrameRate; // So Umrechnen dass Speed = Kachel Pro Sekunde ist.
      csFast: cSpeed := ConveyorFastSpeed / FrameRate; // So Umrechnen dass Speed = Kachel Pro Sekunde ist.
    End;
    x := trunc(Players[PlayerIndex].info.Position.x);
    y := trunc(Players[PlayerIndex].info.Position.y);
    If fConveyorDirs[x, y] <> -1 Then Begin // -- Aber nur wenn wir auch tatsächlich auf einem Laufband stehen..
      commapartx := (Players[PlayerIndex].info.Position.x - trunc(Players[PlayerIndex].info.Position.x));
      commaparty := (Players[PlayerIndex].info.Position.y - trunc(Players[PlayerIndex].info.Position.y));
      // Die Bombe befindet sich "ungefähr" in der Mitte
      dxi := 0;
      dyi := 0;
      Case fConveyorDirs[x, y] Of
        0: dxi := 1;
        90: dyi := -1;
        180: dxi := -1;
        270: dyi := 1;
      End;
      nx := x + dxi;
      ny := y + dyi;
      If Not FielWalkable(nx, ny, false) Then Begin // Das Laufband kann uns theoretisch auf ein Powerup drauf schieben, ob wir das nun wollen oder nicht ;)
        dxi := 0;
        dyi := 0;
      End;
      Players[PlayerIndex].info.Position.x := Players[PlayerIndex].info.Position.x + dxi * cSpeed;
      Players[PlayerIndex].info.Position.y := Players[PlayerIndex].info.Position.y + dyi * cSpeed;
      // Reinziehen der Bomben auf Ordentliche Koordinaten !
      If (commaparty <> 0.5) Then Begin
        If (dxi <> 0) Then Begin
          Players[PlayerIndex].info.Position.y := Players[PlayerIndex].info.Position.y + (0.5 - commaparty) * abs(commapartx - 0.5);
        End
        Else Begin
          If Not (Players[PlayerIndex].MoveState In [msDown, msUp]) Then
            Players[PlayerIndex].info.Position.y := Players[PlayerIndex].info.Position.y + (0.5 - commaparty) * cSpeed;
        End;
      End;
      If (commapartx <> 0.5) Then Begin
        If (dyi <> 0) Then Begin
          Players[PlayerIndex].info.Position.x := Players[PlayerIndex].info.Position.x + (0.5 - commapartx) * abs(commaparty - 0.5);
        End
        Else Begin
          If Not (Players[PlayerIndex].MoveState In [msRight, msLeft]) Then
            Players[PlayerIndex].info.Position.x := Players[PlayerIndex].info.Position.x + (0.5 - commapartx) * cSpeed;
        End;
      End;
    End;
  End;
  cSpeed := 0;
  rSpeed := Players[PlayerIndex].Powers.Speed / FrameRate; // So Umrechnen dass Speed = Kachel Pro Sekunde ist.

  // Das Bewegen des Atomics anhand Seiner Daten
  Case Players[PlayerIndex].MoveState Of
    msStill: Players[PlayerIndex].Info.Animation := raStandStill;
    msRight: Begin
        Players[PlayerIndex].Info.Animation := raWalk;
        Players[PlayerIndex].Info.Direction := 0;
        Players[PlayerIndex].Info.Position.x := min(FieldWidth - 0.5, Players[PlayerIndex].Info.Position.x + rSpeed);
        commapartx := (Players[PlayerIndex].Info.Position.x - trunc(Players[PlayerIndex].Info.Position.x));
        commaparty := (Players[PlayerIndex].Info.Position.y - trunc(Players[PlayerIndex].Info.Position.y));
        If commaparty <> 0.5 Then Begin
          Players[PlayerIndex].Info.Position.y := Players[PlayerIndex].Info.Position.y + (0.5 - commaparty) * abs(commapartx - 0.5);
        End;
        // Schauen ob die kachel auf die wir Laufen wollen überhaupt "begehbar" ist
        If commapartx > 0.5 Then Begin // Wir wollen Tatsächlich auf die Nächste Kachel Laufen
          x := trunc(Players[PlayerIndex].Info.Position.x) + 1;
          y := trunc(Players[PlayerIndex].info.Position.y);
          If commaparty > 0.5 + Epsilon Then Begin
            y := y + 1;
          End;
          If Not FielWalkable(x, y, false) Then Begin
            index := GetBombIndex(x, y);
            // Kann der Spieler Bomben Kicken ? und liegt da ne Bombe
            If (Players[PlayerIndex].Powers.CanKickBombs) And (index <> -1) Then Begin
              If FielWalkable(x + 1, y, true) Then Begin // Kann die Bombe sich bewegen ?
                If fBombs[index].MoveDir <> bmRight Then Begin
                  fPlaySoundEffect(PlayerIndex, seBombKick);
                  Players[PlayerIndex].Info.Animation := raKick;
                  Players[PlayerIndex].Info.Counter := 0;
                  fBombs[index].MoveDir := bmRight;
                  fBombs[index].Jelly := Players[PlayerIndex].Powers.JellyBombs;
                  fBombs[index].Speed := Players[PlayerIndex].Powers.Speed + cSpeed;
                End;
              End
              Else Begin
                If fBombs[index].MoveDir <> bmNone Then Begin
                  fPlaySoundEffect(PlayerIndex, seBombStop);
                  fBombs[index].MoveDir := bmNone;
                  fBombs[index].Position.x := trunc(fBombs[index].Position.x) + 0.5;
                  fBombs[index].Position.y := trunc(fBombs[index].Position.y) + 0.5;
                End;
                Players[PlayerIndex].Info.Position.x := trunc(Players[PlayerIndex].Info.Position.x) + 0.5;
              End;
            End
            Else Begin
              If (index <> -1) And (fBombs[index].MoveDir <> bmNone) Then Begin
                fPlaySoundEffect(PlayerIndex, seBombStop);
                fBombs[index].MoveDir := bmNone;
                fBombs[index].Position.x := trunc(fBombs[index].Position.x) + 0.5;
                fBombs[index].Position.y := trunc(fBombs[index].Position.y) + 0.5;
              End;
              Players[PlayerIndex].Info.Position.x := trunc(Players[PlayerIndex].Info.Position.x) + 0.5;
            End;
          End;
        End;
      End;
    msLeft: Begin
        Players[PlayerIndex].Info.Animation := raWalk;
        Players[PlayerIndex].Info.Direction := 180;
        Players[PlayerIndex].Info.Position.x := max(0.5, Players[PlayerIndex].Info.Position.x - rSpeed);
        commapartx := (Players[PlayerIndex].Info.Position.x - trunc(Players[PlayerIndex].Info.Position.x));
        commaparty := (Players[PlayerIndex].Info.Position.y - trunc(Players[PlayerIndex].Info.Position.y));
        If commaparty <> 0.5 Then Begin
          Players[PlayerIndex].Info.Position.y := Players[PlayerIndex].Info.Position.y + (0.5 - commaparty) * abs(commapartx - 0.5);
        End;
        // Schauen ob die kachel auf die wir Laufen wollen überhaupt "begehbar" ist
        If commapartx < 0.5 Then Begin // Wir wollen Tatsächlich auf die Nächste Kachel Laufen
          x := trunc(Players[PlayerIndex].Info.Position.x) - 1;
          y := trunc(Players[PlayerIndex].info.Position.y);
          If commaparty > 0.5 + Epsilon Then Begin
            y := y + 1;
          End;
          If Not FielWalkable(x, y, false) Then Begin
            index := GetBombIndex(x, y);
            // Kann der Spieler Bomben Kicken ? und liegt da ne Bombe
            If (Players[PlayerIndex].Powers.CanKickBombs) And (index <> -1) Then Begin
              If FielWalkable(x - 1, y, true) Then Begin // Kann die Bombe sich bewegen ?
                If fBombs[index].MoveDir <> bmLeft Then Begin
                  fPlaySoundEffect(PlayerIndex, seBombKick);
                  Players[PlayerIndex].Info.Animation := raKick;
                  Players[PlayerIndex].Info.Counter := 0;
                  fBombs[index].MoveDir := bmLeft;
                  fBombs[index].Jelly := Players[PlayerIndex].Powers.JellyBombs;
                  fBombs[index].Speed := Players[PlayerIndex].Powers.Speed + cSpeed;
                End;
              End
              Else Begin
                If fBombs[index].MoveDir <> bmNone Then Begin
                  fPlaySoundEffect(PlayerIndex, seBombStop);
                  fBombs[index].MoveDir := bmNone;
                  fBombs[index].Position.x := trunc(fBombs[index].Position.x) + 0.5;
                  fBombs[index].Position.y := trunc(fBombs[index].Position.y) + 0.5;
                End;
                Players[PlayerIndex].Info.Position.x := trunc(Players[PlayerIndex].Info.Position.x) + 0.5;
              End;
            End
            Else Begin
              If (index <> -1) And (fBombs[index].MoveDir <> bmNone) Then Begin
                fPlaySoundEffect(PlayerIndex, seBombStop);
                fBombs[index].MoveDir := bmNone;
                fBombs[index].Position.x := trunc(fBombs[index].Position.x) + 0.5;
                fBombs[index].Position.y := trunc(fBombs[index].Position.y) + 0.5;
              End;
              Players[PlayerIndex].Info.Position.x := trunc(Players[PlayerIndex].Info.Position.x) + 0.5;
            End;
          End;
        End;
      End;
    msUp: Begin
        Players[PlayerIndex].Info.Animation := raWalk;
        Players[PlayerIndex].Info.Direction := 90;
        Players[PlayerIndex].Info.Position.y := max(0.5, Players[PlayerIndex].Info.Position.y - rSpeed);
        commapartx := (Players[PlayerIndex].Info.Position.x - trunc(Players[PlayerIndex].Info.Position.x));
        commaparty := (Players[PlayerIndex].Info.Position.y - trunc(Players[PlayerIndex].Info.Position.y));
        If commapartx <> 0.5 Then Begin
          Players[PlayerIndex].Info.Position.x := Players[PlayerIndex].Info.Position.x + (0.5 - commapartx) * abs(commaparty - 0.5);
        End;
        // Schauen ob die kachel auf die wir Laufen wollen überhaupt "begehbar" ist
        If commaparty < 0.5 Then Begin // Wir wollen Tatsächlich auf die Nächste Kachel Laufen
          x := trunc(Players[PlayerIndex].Info.Position.x);
          y := trunc(Players[PlayerIndex].info.Position.y) - 1;
          If commapartx > 0.5 + Epsilon Then Begin
            x := x + 1;
          End;
          If Not FielWalkable(x, y, false) Then Begin
            index := GetBombIndex(x, y);
            // Kann der Spieler Bomben Kicken ? und liegt da ne Bombe
            If (Players[PlayerIndex].Powers.CanKickBombs) And (index <> -1) Then Begin
              If FielWalkable(x, y - 1, true) Then Begin // Kann die Bombe sich bewegen ?
                If fBombs[index].MoveDir <> bmUp Then Begin
                  fPlaySoundEffect(PlayerIndex, seBombKick);
                  Players[PlayerIndex].Info.Animation := raKick;
                  Players[PlayerIndex].Info.Counter := 0;
                  fBombs[index].MoveDir := bmUp;
                  fBombs[index].Jelly := Players[PlayerIndex].Powers.JellyBombs;
                  fBombs[index].Speed := Players[PlayerIndex].Powers.Speed + cSpeed;
                End;
              End
              Else Begin
                If fBombs[index].MoveDir <> bmNone Then Begin
                  fPlaySoundEffect(PlayerIndex, seBombStop);
                  fBombs[index].MoveDir := bmNone;
                  fBombs[index].Position.x := trunc(fBombs[index].Position.x) + 0.5;
                  fBombs[index].Position.y := trunc(fBombs[index].Position.y) + 0.5;
                End;
                Players[PlayerIndex].Info.Position.y := trunc(Players[PlayerIndex].Info.Position.y) + 0.5;
              End;
            End
            Else Begin
              If (index <> -1) And (fBombs[index].MoveDir <> bmNone) Then Begin
                fPlaySoundEffect(PlayerIndex, seBombStop);
                fBombs[index].MoveDir := bmNone;
                fBombs[index].Position.x := trunc(fBombs[index].Position.x) + 0.5;
                fBombs[index].Position.y := trunc(fBombs[index].Position.y) + 0.5;
              End;
              Players[PlayerIndex].Info.Position.y := trunc(Players[PlayerIndex].Info.Position.y) + 0.5;
            End;
          End;
        End;
      End;
    msDown: Begin
        Players[PlayerIndex].Info.Animation := raWalk;
        Players[PlayerIndex].Info.Direction := 270;
        Players[PlayerIndex].Info.Position.y := min(FieldHeight - 0.5, Players[PlayerIndex].Info.Position.y + rSpeed);
        commapartx := (Players[PlayerIndex].Info.Position.x - trunc(Players[PlayerIndex].Info.Position.x));
        commaparty := (Players[PlayerIndex].Info.Position.y - trunc(Players[PlayerIndex].Info.Position.y));
        If commapartx <> 0.5 Then Begin
          Players[PlayerIndex].Info.Position.x := Players[PlayerIndex].Info.Position.x + (0.5 - commapartx) * abs(commaparty - 0.5);
        End;
        // Schauen ob die kachel auf die wir Laufen wollen überhaupt "begehbar" ist
        If commaparty > 0.5 Then Begin // Wir wollen Tatsächlich auf die Nächste Kachel Laufen
          x := trunc(Players[PlayerIndex].Info.Position.x);
          y := trunc(Players[PlayerIndex].info.Position.y) + 1;
          If commapartx > 0.5 + Epsilon Then Begin
            x := x + 1;
          End;
          If Not FielWalkable(x, y, false) Then Begin
            index := GetBombIndex(x, y);
            // Kann der Spieler Bomben Kicken ? und liegt da ne Bombe
            If (Players[PlayerIndex].Powers.CanKickBombs) And (index <> -1) Then Begin
              If FielWalkable(x, y + 1, true) Then Begin // Kann die Bombe sich bewegen ?
                If fBombs[index].MoveDir <> bmDown Then Begin
                  fPlaySoundEffect(PlayerIndex, seBombKick);
                  Players[PlayerIndex].Info.Animation := raKick;
                  Players[PlayerIndex].Info.Counter := 0;
                  fBombs[index].MoveDir := bmDown;
                  fBombs[index].Jelly := Players[PlayerIndex].Powers.JellyBombs;
                  fBombs[index].Speed := Players[PlayerIndex].Powers.Speed + cSpeed;
                End;
              End
              Else Begin
                If fBombs[index].MoveDir <> bmNone Then Begin
                  fPlaySoundEffect(PlayerIndex, seBombStop);
                  fBombs[index].MoveDir := bmNone;
                  fBombs[index].Position.x := trunc(fBombs[index].Position.x) + 0.5;
                  fBombs[index].Position.y := trunc(fBombs[index].Position.y) + 0.5;
                End;
                Players[PlayerIndex].Info.Position.y := trunc(Players[PlayerIndex].Info.Position.y) + 0.5;
              End;
            End
            Else Begin
              If (index <> -1) And (fBombs[index].MoveDir <> bmNone) Then Begin
                fPlaySoundEffect(PlayerIndex, seBombStop);
                fBombs[index].MoveDir := bmNone;
                fBombs[index].Position.x := trunc(fBombs[index].Position.x) + 0.5;
                fBombs[index].Position.y := trunc(fBombs[index].Position.y) + 0.5;
              End;
              Players[PlayerIndex].Info.Position.y := trunc(Players[PlayerIndex].Info.Position.y) + 0.5;
            End;
          End;
        End;
      End;
  End;
  // evtl. ist der Spieler ja "eingesperrt"
  // TODO: Wenn die Spieler "Fliegen" muss das hier berücksichtigt werden
  x := trunc(Players[PlayerIndex].Info.Position.x);
  y := trunc(Players[PlayerIndex].info.Position.y);
  If (Not FielWalkable(x - 1, y, false)) And
    (Not FielWalkable(x + 1, y, false)) And
    (Not FielWalkable(x, y - 1, false)) And
    (Not FielWalkable(x, y + 1, false)) And
    (Players[PlayerIndex].info.Animation <> raLockedIn) Then Begin
    Players[PlayerIndex].info.Animation := raLockedIn;
    Players[PlayerIndex].info.Value := random(65536);
    Players[PlayerIndex].info.Counter := 0;
  End;
  result := fField[x, y].BrickData = bdSolid;
End;

Procedure TAtomicField.HandleActionPlayer(Var Player: TPlayer;
  PlayerIndex: integer);

  Function PlaceBombOn(aX, aY: Single): Boolean;
  Var
    i: Integer;
  Begin
    result := false;
    // Darf der Spieler überhaupt noch Bomben legen =
    If player.Powers.AvailableBombs <= 0 Then exit;
    // Liegt auf dem Feld schon eine Bombe ?
    For i := 0 To fBombCount - 1 Do Begin
      If (trunc(fBombs[i].Position.x) = trunc(ax)) And
        (trunc(fBombs[i].Position.y) = trunc(ay)) Then exit;
    End;
    // Es darf keine Bombe auf ein Loch gelegt werden !
    If fHasHoles And fHoles[trunc(ax), trunc(ay)] Then exit;
    fBombs[fBombCount].ColorIndex := player.info.ColorIndex;
    fBombs[fBombCount].Position.x := trunc(ax) + 0.5;
    fBombs[fBombCount].Position.y := trunc(ay) + 0.5;
    If player.Powers.TriggerBomb > 0 Then Begin
      fBombs[fBombCount].Animation := baTimeTriggered;
      player.Powers.TriggerBomb := player.Powers.TriggerBomb - 1;
    End
    Else Begin
      If dDudBombs In player.Disease Then Begin
        fBombs[fBombCount].Animation := baDud;
        fBombs[fBombCount].DudTime := AtomicBombDudTimeMin + random(AtomicBombDudTimeMax - AtomicBombDudTimeMin);
      End
      Else Begin
        fBombs[fBombCount].Animation := baNormal;
      End;
    End;
    fBombs[fBombCount].AnimationOffset := random(65536);
    fBombs[fBombCount].PlayerIndex := PlayerIndex;
    fBombs[fBombCount].Lifetime := 0;
    fBombs[fBombCount].FireLen := player.Powers.FlameLen;
    fBombs[fBombCount].Jelly := false; // Das wird vom "Kickenden" Spieler übernommen
    fBombs[fBombCount].MoveDir := bmNone;
    fBombs[fBombCount].Speed := player.Powers.Speed;
    inc(fBombCount);
    player.Powers.AvailableBombs := player.Powers.AvailableBombs - 1;
    result := true;
    If fBombCount > high(fBombs) Then Begin
      log('TAtomicField.HandleActionPlayer: bomb overflow', llfatal);
      exit;
    End;
  End;

Var
  bx, by, x, y, dx, dy, i: Integer;
  handled: Boolean;
Begin
  If Player.Flying Then exit; // Im Flug darf der Spieler natürlich nichts machen ;)
  Case player.Action Of
    aaFirstDouble: Begin
        (* Jeder Doppelt action geht eine Einfach Aktion vorraus ! *)
        player.Action := aaFirst;
        HandleActionPlayer(Player, PlayerIndex);
        // Ab jetzt können die Doppelt Aktionen richtig ausgewertet werden
        If player.Powers.CanSpooger Then Begin // Die Fähigkeit Viele auf einen Schlag zu legen
          dx := 0;
          dy := 0;
          Case (trunc(player.info.Direction) Div 90) Of
            0: dx := 1;
            1: dy := -1;
            2: dx := -1;
            3: dy := 1;
          End;
          x := trunc(player.Info.Position.x);
          y := trunc(player.Info.Position.y);
          For i := 1 To player.Powers.AvailableBombs Do Begin
            If Not FielWalkable(x + i * dx, y + i * dy, true) {And (i <> 1)} Then exit;
            If fHasHoles Then Begin // An Löchern ist auch schluss mit dem "Spoogen"
              If fHoles[x + i * dx, y + i * dy] Then exit;
            End;
            If fHastrampolins Then Begin // Auf Trampoline kann ebenfalls keine Bombe gelegt werden
              If fField[x + i * dx, y + i * dy].Tramp Then exit;
            End;
            If PlaceBombOn(x + i * dx + 0.5, y + i * dy + 0.5) Then Begin
              fPlaySoundEffect(PlayerIndex, seBombDrop);
              StatisticCallback(sBombsDropped);
            End;
          End;
        End;
        If Player.Powers.CanGrabBombs Then Begin
          // Gibt es eine Bombe zum Graben ?
          x := trunc(player.Info.Position.x);
          y := trunc(player.Info.Position.y);
          For i := 0 To high(fBombs) Do Begin
            If fBombs[i].MoveDir = bmFly Then Continue; // Fliegende Bomben dürfen nicht gegriffen werden.
            bx := trunc(fBombs[i].Position.x);
            by := trunc(fBombs[i].Position.y);
            If (x = bx) And (y = by) Then Begin
              dx := 0;
              dy := 0;
              Case (trunc(player.info.Direction) Div 90) Of
                0: dx := 1;
                1: dy := -1;
                2: dx := -1;
                3: dy := 1;
              End;
              fBombs[i].FlyStart := v2(bx + 0.5, by + 0.5);
              fBombs[i].FlyTarget := v2(bx + 0.5 + 3 * dx, by + 0.5 + 3 * dy);
              fBombs[i].MoveDir := bmFly;
              fBombs[i].FlyTime := 0;
              fBombs[i].FlyFinTime := AtomicBombBigFlyTime;
              fPlaySoundEffect(PlayerIndex, seBombGrab);
              Player.Info.Animation := raPup;
              Player.Info.Counter := 0;
              break;
            End;
          End;
        End;
      End;
    aaFirst: Begin
        If PlaceBombOn(trunc(player.Info.Position.x) + 0.5, trunc(player.Info.Position.y) + 0.5) Then Begin
          fPlaySoundEffect(PlayerIndex, seBombDrop);
          StatisticCallback(sBombsDropped);
        End;
      End;
    aaSecond: Begin
        handled := false; // This flag allows to trigger bombs while walking = more responsive ;)
        // Punch = Laufen gegen eine Bombe und dann Second Key ;)
        If Player.Powers.CanPunchBombs And (Player.MoveState <> msStill) Then Begin
          x := trunc(player.Info.Position.x);
          y := trunc(player.Info.Position.y);
          dx := 0;
          dy := 0;
          Case (trunc(player.info.Direction) Div 90) Of
            0: dx := 1;
            1: dy := -1;
            2: dx := -1;
            3: dy := 1;
          End;
          For i := 0 To high(fBombs) Do Begin
            If fBombs[i].MoveDir = bmFly Then Continue; // Fliegende Bomben dürfen nicht gepuncht werden.
            bx := trunc(fBombs[i].Position.x);
            by := trunc(fBombs[i].Position.y);
            If (x + dx = bx) And (y + dy = by) Then Begin
              fBombs[i].FlyStart := v2(bx + 0.5, by + 0.5);
              fBombs[i].FlyTarget := v2(bx + 0.5 + 3 * dx, by + 0.5 + 3 * dy);
              fBombs[i].MoveDir := bmFly;
              fBombs[i].FlyTime := 0;
              fBombs[i].FlyFinTime := AtomicBombBigFlyTime;
              fPlaySoundEffect(PlayerIndex, seBombPunch);
              Player.Info.Animation := raPunch;
              Player.Info.Counter := 0;
              handled := true;
              break;
            End;
          End;
        End;
        If Not handled Then Begin
          (*
           * Zünden der eigenen Time Triggered Bomben, das geht irgendwie immer ...
           *)
          For i := 0 To fBombCount - 1 Do Begin
            If (fBombs[i].PlayerIndex = PlayerIndex) And (fBombs[i].Animation = baTimeTriggered) And (fBombs[i].MoveDir <> bmFly) Then Begin
              fBombs[i].Animation := baNormal;
              fBombs[i].Lifetime := AtomicBombDetonateTime;
            End;
          End;
        End;
      End;
  End;
End;

Procedure TAtomicField.HandleBombs(Var Players: TPlayers; PreHurry: Boolean; ConveyorSpeed: TConveyorSpeed);
Type
  TDir = (DirUp, DirDown, DirLeft, DirRight);

  Function BlockedByHole(x, y: integer): Boolean; // Gillt nur für Bomben, wenn diese sich bewegen, deswegen extra
  Begin
    If fHasHoles Then Begin
      result := fHoles[x, y];
    End
    Else Begin
      result := false;
    End;
  End;

  Function BlockedByTramp(x, y: integer): Boolean; // Gillt nur für Bomben, wenn diese sich bewegen, deswegen extra
  Begin
    If fHastrampolins Then Begin
      result := fField[x, y].Tramp;
    End
    Else Begin
      result := false;
    End;
  End;

  Function BlockedByPlayer(x, y: integer): Boolean;
  Var
    i: Integer;
  Begin
    result := false;
    For i := 0 To high(Players) Do Begin
      If (Players[i].Info.Alive) And (Not Players[i].Info.Dieing) Then Begin
        If (trunc(Players[i].Info.Position.x) = x) And
          (trunc(Players[i].Info.Position.y) = y) Then Begin
          result := true;
          break;
        End;
      End;
    End;
  End;

  Function DirToMoveDir(aDir: integer): TBombMoveDir;
  Begin
    result := bmNone;
    Case aDir Of
      0: result := bmRight;
      90: result := bmUp;
      180: result := bmLeft;
      270: result := bmDown;
    End;
  End;

  Procedure AddEndFlame(x, y, PlayerIndex: integer);
  Begin
    (*
     * So überschreibt jeder Spieler ggf. einen Anderen, es sieht so aus als
     * ob TAtomicField.Detonate das auch so macht, wenn nicht, muss das If mit rein !
     *)
    //If fField[x, y].Flame = [] Then Begin
    fField[x, y].FlamePlayer := PlayerIndex;
    //End;
    fField[x, y].Flame := fField[x, y].Flame + [fend];
  End;

Var
  dxi, dyi, nx, ny, x, y, i: Integer;
  index, j, dir: Integer;
  dirs: Set Of TDir;
  p: TPoint;
  cSpeed, rSpeed, commapartx, commaparty: Single;
  BombExplodeSound: Array[0..Length(PlayerColors) - 1] Of Boolean;
  dx, dy, s: Single;
  mdir: TBombMoveDir;
Begin
  If Not fBombsEnabled Then exit; // Bomben dürfen nicht mehr gezündet werden !
  fBombDetonateFifo.Clear;
  fPowerUpClearFifo.Clear;
  For i := 0 To fBombCount - 1 Do Begin
    rSpeed := fBombs[i].Speed / FrameRate; // So Umrechnen dass Speed = Kachel Pro Sekunde ist.
    fBombs[i].Lifetime := fBombs[i].Lifetime + FrameRate; // Countdown bis zur Detonation
    fBombs[i].detonated := false;
    // Die Bombe bewegt
    If fHasArrows And (fBombs[i].MoveDir In [bmDown, bmLeft, bmUp, bmRight]) Then Begin
      x := trunc(fBombs[i].Position.x);
      y := trunc(fBombs[i].Position.y);
      commapartx := (fBombs[i].Position.x - trunc(fBombs[i].Position.x));
      commaparty := (fBombs[i].Position.y - trunc(fBombs[i].Position.y));
      // Die Bombe befindet sich "ungefähr" in der Mitte
      If (abs(commapartx - 0.5) <= rSpeed) And (abs(commaparty - 0.5) <= rSpeed) Then Begin
        dir := fArrowDirs[x, y];
        mdir := DirToMoveDir(dir);
        If (dir <> -1) And (mdir <> fBombs[i].MoveDir) Then Begin
          fBombs[i].MoveDir := mdir;
          fBombs[i].Position.x := x + 0.5;
          fBombs[i].Position.y := y + 0.5;
          // TODO: hier prüfen ob die Bombe weiter in dir gewünschte Richtung kann, wenn nicht Stop
          fPlaySoundEffect(fBombs[i].PlayerIndex, seBombBounce);
        End;
      End;
    End;
    If fHasConveyors And (fBombs[i].MoveDir In [bmNone, bmDown, bmLeft, bmUp, bmRight]) Then Begin
      cSpeed := 0; // -- Fehler
      Case ConveyorSpeed Of
        csSlow: cSpeed := ConveyorSlowSpeed / FrameRate; // So Umrechnen dass Speed = Kachel Pro Sekunde ist.
        csMiddle: cSpeed := ConveyorMiddleSpeed / FrameRate; // So Umrechnen dass Speed = Kachel Pro Sekunde ist.
        csFast: cSpeed := ConveyorFastSpeed / FrameRate; // So Umrechnen dass Speed = Kachel Pro Sekunde ist.
      End;
      x := trunc(fBombs[i].Position.x);
      y := trunc(fBombs[i].Position.y);
      commapartx := (fBombs[i].Position.x - trunc(fBombs[i].Position.x));
      commaparty := (fBombs[i].Position.y - trunc(fBombs[i].Position.y));
      // Die Bombe befindet sich "ungefähr" in der Mitte
      dxi := 0;
      dyi := 0;
      Case fConveyorDirs[x, y] Of
        0: dxi := 1;
        90: dyi := -1;
        180: dxi := -1;
        270: dyi := 1;
      End;
      nx := x + dxi;
      ny := y + dyi;
      If Not FielWalkable(nx, ny, true) Then Begin
        dxi := 0;
        dyi := 0;
      End;
      fBombs[i].Position.x := fBombs[i].Position.x + dxi * cSpeed;
      fBombs[i].Position.y := fBombs[i].Position.y + dyi * cSpeed;
      // Reinziehen der Bomben auf Ordentliche Koordinaten !
      If (commaparty <> 0.5) Then Begin
        If (dxi <> 0) Then Begin
          fBombs[i].Position.y := fBombs[i].Position.y + (0.5 - commaparty) * abs(commapartx - 0.5);
        End
        Else Begin
          fBombs[i].Position.y := fBombs[i].Position.y + (0.5 - commaparty) * cSpeed;
        End;
      End;
      If (commapartx <> 0.5) Then Begin
        If (dyi <> 0) Then Begin
          fBombs[i].Position.x := fBombs[i].Position.x + (0.5 - commapartx) * abs(commaparty - 0.5);
        End
        Else Begin
          fBombs[i].Position.x := fBombs[i].Position.x + (0.5 - commapartx) * cSpeed;
        End;
      End;
    End;
    Case fBombs[i].MoveDir Of
      bmFly: Begin
          fBombs[i].Lifetime := fBombs[i].Lifetime - FrameRate; // Countdown bis zur Detonation wieder Rückgängig machen
          fBombs[i].FlyTime := fBombs[i].FlyTime + FrameRate;
          s := fBombs[i].FlyTime / fBombs[i].FlyFinTime;
          dx := fBombs[i].FlyTarget.x - fBombs[i].FlyStart.x;
          dy := fBombs[i].FlyTarget.y - fBombs[i].FlyStart.y;
          If dy = 0 Then Begin
            // Die Bombe "Fliegt" Waagrecht
            fBombs[i].Position.x := fBombs[i].FlyStart.x + dx * s;
            fBombs[i].Position.y := fBombs[i].FlyStart.y - 1.5 * sin(pi * s); // Die Bogenhöhe beim Werfen
          End
          Else Begin
            // Die Bombe fliegt Senkrecht
            fBombs[i].Position.y := fBombs[i].FlyStart.y + dy * s;
          End;
          // Hier muss ein Mod der Position rein !
          If fBombs[i].Position.x < 0 Then fBombs[i].Position.x := fBombs[i].Position.x + FieldWidth - 1;
          If fBombs[i].Position.x >= FieldWidth Then fBombs[i].Position.x := fBombs[i].Position.x - FieldWidth;
          If dy <> 0 Then Begin
            If fBombs[i].Position.y < 0 Then fBombs[i].Position.y := fBombs[i].Position.y + FieldHeight - 1;
            If fBombs[i].Position.y >= FieldHeight Then fBombs[i].Position.y := fBombs[i].Position.y - FieldHeight;
          End;
          If fBombs[i].FlyTime >= fBombs[i].FlyFinTime Then Begin
            fBombs[i].Position := fBombs[i].FlyTarget;
            // Hier muss ein Mod der Position rein !
            If fBombs[i].Position.x < 0 Then fBombs[i].Position.x := fBombs[i].Position.x + FieldWidth - 1;
            If fBombs[i].Position.x >= FieldWidth Then fBombs[i].Position.x := fBombs[i].Position.x - FieldWidth;
            If fBombs[i].Position.y < 0 Then fBombs[i].Position.y := fBombs[i].Position.y + FieldHeight - 1;
            If fBombs[i].Position.y >= FieldHeight Then fBombs[i].Position.y := fBombs[i].Position.y - FieldHeight;
            x := trunc(fBombs[i].Position.x);
            y := trunc(fBombs[i].Position.y);
            If FielWalkable(x, y, true) And (Not BlockedByHole(x, y)) And (Not BlockedBytramp(x, y)) Then Begin
              // 1. Das Feld darf belegt werden -> Fertig
              fBombs[i].MoveDir := bmNone;
            End
            Else Begin
              // 2. Das Feld ist belegt, wir starten ein Bouncing
              dx := 0;
              dy := 0;
              Case random(4) Of
                0: dx := 1;
                1: dy := -1;
                2: dx := -1;
                3: dy := 1;
              End;
              fBombs[i].FlyStart := v2(x + 0.5, y + 0.5);
              fBombs[i].FlyTarget := v2(x + 0.5 + dx, y + 0.5 + dy);
              fBombs[i].FlyTime := 0;
              fBombs[i].FlyFinTime := AtomicBombSmallFlyTime;
              fPlaySoundEffect(fBombs[i].PlayerIndex, seBombBounce);
            End;
          End;
        End;
      bmNone: Begin // Nix zu tun
        End;
      bmRight: Begin
          fBombs[i].Position.x := fBombs[i].Position.x + rSpeed;
          commapartx := (fBombs[i].Position.x - trunc(fBombs[i].Position.x));
          commaparty := (fBombs[i].Position.y - trunc(fBombs[i].Position.y));
          If commaparty <> 0.5 Then Begin // Reinziehen der Bomben auf Ordentliche Koordinaten !
            fBombs[i].Position.y := fBombs[i].Position.y + (0.5 - commaparty) * abs(commapartx - 0.5);
          End;
          If commapartx > 0.5 Then Begin // Wir wollen Tatsächlich auf die Nächste Kachel Laufen
            x := trunc(fBombs[i].Position.x) + 1;
            y := trunc(fBombs[i].Position.y);
            If (Not FielWalkable(x, y, true)) Or (BlockedByPlayer(x, y) Or BlockedByHole(x, y) Or BlockedBytramp(x, y)) Then Begin
              fBombs[i].Position.x := trunc(fBombs[i].Position.x) + 0.5;
              If fBombs[i].Jelly Then Begin
                fPlaySoundEffect(fBombs[i].PlayerIndex, seBombJelly);
                fBombs[i].MoveDir := bmLeft;
                If fBombs[i].Animation = baNormal Then fBombs[i].Animation := baWobble;
              End
              Else Begin
                fPlaySoundEffect(fBombs[i].PlayerIndex, seBombStop);
                fBombs[i].MoveDir := bmNone;
                If fBombs[i].Animation = baWobble Then fBombs[i].Animation := baNormal;
              End;
            End;
          End;
        End;
      bmLeft: Begin
          fBombs[i].Position.x := fBombs[i].Position.x - rSpeed;
          commapartx := (fBombs[i].Position.x - trunc(fBombs[i].Position.x));
          commaparty := (fBombs[i].Position.y - trunc(fBombs[i].Position.y));
          If commaparty <> 0.5 Then Begin // Reinziehen der Bomben auf Ordentliche Koordinaten !
            fBombs[i].Position.y := fBombs[i].Position.y + (0.5 - commaparty) * abs(commapartx - 0.5);
          End;
          If commapartx < 0.5 Then Begin // Wir wollen Tatsächlich auf die Nächste Kachel Laufen
            x := trunc(fBombs[i].Position.x) - 1;
            y := trunc(fBombs[i].Position.y);
            If (Not FielWalkable(x, y, true)) Or (BlockedByPlayer(x, y) Or BlockedByHole(x, y) Or BlockedBytramp(x, y)) Then Begin
              fBombs[i].Position.x := trunc(fBombs[i].Position.x) + 0.5;
              If fBombs[i].Jelly Then Begin
                fPlaySoundEffect(fBombs[i].PlayerIndex, seBombJelly);
                fBombs[i].MoveDir := bmRight;
                If fBombs[i].Animation = baNormal Then fBombs[i].Animation := baWobble;
              End
              Else Begin
                fPlaySoundEffect(fBombs[i].PlayerIndex, seBombStop);
                fBombs[i].MoveDir := bmNone;
                If fBombs[i].Animation = baWobble Then fBombs[i].Animation := baNormal;
              End;
            End;
          End;
        End;
      bmUp: Begin
          fBombs[i].Position.y := fBombs[i].Position.y - rSpeed;
          commapartx := (fBombs[i].Position.x - trunc(fBombs[i].Position.x));
          commaparty := (fBombs[i].Position.y - trunc(fBombs[i].Position.y));
          If commapartx <> 0.5 Then Begin
            fBombs[i].Position.x := fBombs[i].Position.x + (0.5 - commapartx) * abs(commaparty - 0.5);
          End;
          If commaparty < 0.5 Then Begin // Wir wollen Tatsächlich auf die Nächste Kachel Laufen
            x := trunc(fBombs[i].Position.x);
            y := trunc(fBombs[i].Position.y) - 1;
            If (Not FielWalkable(x, y, true)) Or (BlockedByPlayer(x, y) Or BlockedByHole(x, y) Or BlockedBytramp(x, y)) Then Begin
              fBombs[i].Position.y := trunc(fBombs[i].Position.y) + 0.5;
              If fBombs[i].Jelly Then Begin
                fPlaySoundEffect(fBombs[i].PlayerIndex, seBombJelly);
                fBombs[i].MoveDir := bmDown;
                If fBombs[i].Animation = baNormal Then fBombs[i].Animation := baWobble;
              End
              Else Begin
                fPlaySoundEffect(fBombs[i].PlayerIndex, seBombStop);
                fBombs[i].MoveDir := bmNone;
                If fBombs[i].Animation = baWobble Then fBombs[i].Animation := baNormal;
              End;
            End;
          End;
        End;
      bmDown: Begin
          fBombs[i].Position.y := fBombs[i].Position.y + rSpeed;
          commapartx := (fBombs[i].Position.x - trunc(fBombs[i].Position.x));
          commaparty := (fBombs[i].Position.y - trunc(fBombs[i].Position.y));
          If commapartx <> 0.5 Then Begin
            fBombs[i].Position.x := fBombs[i].Position.x + (0.5 - commapartx) * abs(commaparty - 0.5);
          End;
          If commaparty > 0.5 Then Begin // Wir wollen Tatsächlich auf die Nächste Kachel Laufen
            x := trunc(fBombs[i].Position.x);
            y := trunc(fBombs[i].Position.y) + 1;
            If (Not FielWalkable(x, y, true)) Or (BlockedByPlayer(x, y) Or BlockedByHole(x, y) Or BlockedBytramp(x, y)) Then Begin
              fBombs[i].Position.y := trunc(fBombs[i].Position.y) + 0.5;
              If fBombs[i].Jelly Then Begin
                fPlaySoundEffect(fBombs[i].PlayerIndex, seBombJelly);
                fBombs[i].MoveDir := bmUp;
                If fBombs[i].Animation = baNormal Then fBombs[i].Animation := baWobble;
              End
              Else Begin
                fPlaySoundEffect(fBombs[i].PlayerIndex, seBombStop);
                fBombs[i].MoveDir := bmNone;
                If fBombs[i].Animation = baWobble Then fBombs[i].Animation := baNormal;
              End;
            End;
          End;
        End;
    End;
    Case fBombs[i].Animation Of
      baNormal, baWobble: Begin
          If fBombs[i].Lifetime >= AtomicBombDetonateTime Then Begin
            fBombDetonateFifo.Push(i);
          End;
        End;
      baTimeTriggered: Begin
          If fBombs[i].Lifetime >= AtomicTimeTriggeredBombTimeOut Then Begin
            fBombs[i].Animation := baNormal;
            fBombs[i].Lifetime := 0;
          End;
        End;
      baDud: Begin
          If fBombs[i].Lifetime >= fBombs[i].DudTime Then Begin
            fBombs[i].Animation := baNormal;
            fBombs[i].Lifetime := 0;
          End;
        End;
    End;
    // Trifft eine Bombe auf ein bereits Explodierendes Feld geht sie auf jeden Fall auch hoch
    // (dass kann nur bei sich bewegenden Bomben passieren, da die Liegenden in Detonate schon berücksichtigt werden.)
    x := trunc(fBombs[i].Position.x);
    y := trunc(fBombs[i].Position.y);
    If (y > 0) And (fBombs[i].MoveDir <> bmFly) Then Begin // Eine Fliegende Bombe kann Negative Koordinaten kriegen
      If (fField[x, y].Flame <> []) Then Begin
        fBombs[i].Position.x := x + 0.5;
        fBombs[i].Position.y := y + 0.5;
        fBombDetonateFifo.Push(i);
      End;
    End;
    (*
     * Die Bombe muss explodieren bevor sie unter dem Stein begraben wird !
     *)
    If PreHurry Then Begin
      If (fHurryIndex + 1 >= 0) And (fHurryIndex + 1 <= high(HurryCoords)) Then Begin
        p := HurryCoords[fHurryIndex + 1];
        If (x = p.x) And (y = p.y) Then Begin
          fBombs[i].Position.x := x + 0.5;
          fBombs[i].Position.y := y + 0.5;
          fBombDetonateFifo.Push(i);
        End;
      End;
    End;
  End;

  // Alle Bombem die Hochgehen sollen ab arbeiten
  While Not fBombDetonateFifo.isempty Do Begin
    index := fBombDetonateFifo.Pop;
    If fBombs[index].Detonated Then Continue;
    fBombs[index].Detonated := true;
    (*
     * Die Bombe wurde gezündet, der Spieler darf wieder eine Neue Legen
     *)
    Players[fBombs[index].PlayerIndex].Powers.AvailableBombs := Players[fBombs[index].PlayerIndex].Powers.AvailableBombs + 1;
    x := trunc(fBombs[index].Position.x);
    y := trunc(fBombs[index].Position.y);
    If fField[x, y].BrickData = bdSolid Then continue; // Die Bombe Explodiert auf einem Festen Brick -> dann richtet sie keinen Schaden an
    If fField[x, y].BrickData = bdBrick Then Begin // Die Bombe explodiert auf einem Brick, den macht sie kaputt, aber nichts weiter ..
      Detonate(x, y, fBombs[index].PlayerIndex, fBombs[index].ColorIndex, fCross);
      Continue;
    End
    Else Begin
      Detonate(x, y, fBombs[index].PlayerIndex, fBombs[index].ColorIndex, fCross);
    End;
    dirs := [DirUp, DirDown, DirLeft, DirRight];
    For i := 1 To fBombs[index].FireLen Do Begin
      If DirUp In dirs Then Begin
        If (Not Detonate(x, y - i, fBombs[index].PlayerIndex, fBombs[index].ColorIndex, fup)) Then Begin
          dirs := dirs - [DirUp];
          AddEndFlame(x, y - i + 1, fBombs[index].PlayerIndex);
        End;
        If (i = fBombs[index].FireLen) And (y - i >= 0) Then Begin
          AddEndFlame(x, y - i, fBombs[index].PlayerIndex);
        End;
      End;
      If Dirdown In dirs Then Begin
        If (Not Detonate(x, y + i, fBombs[index].PlayerIndex, fBombs[index].ColorIndex, fdown)) Then Begin
          dirs := dirs - [Dirdown];
          AddEndFlame(x, y + i - 1, fBombs[index].PlayerIndex);
        End;
        If (i = fBombs[index].FireLen) And (y + i < FieldHeight) Then Begin
          AddEndFlame(x, y + i, fBombs[index].PlayerIndex);
        End;
      End;
      If DirLeft In dirs Then Begin
        If (Not Detonate(x - i, y, fBombs[index].PlayerIndex, fBombs[index].ColorIndex, fleft)) Then Begin
          dirs := dirs - [DirLeft];
          AddEndFlame(x - i + 1, y, fBombs[index].PlayerIndex);
        End;
        If (i = fBombs[index].FireLen) And (x - i > 0) Then Begin
          AddEndFlame(x - i, y, fBombs[index].PlayerIndex);
        End;
      End;
      If DirRight In dirs Then Begin
        If (Not Detonate(x + i, y, fBombs[index].PlayerIndex, fBombs[index].ColorIndex, fright)) Then Begin
          dirs := dirs - [DirRight];
          AddEndFlame(x + i - 1, y, fBombs[index].PlayerIndex);
        End;
        If (i = fBombs[index].FireLen) And (x + i < FieldWidth) Then Begin
          AddEndFlame(x + i, y, fBombs[index].PlayerIndex);
        End;
      End;
    End;
  End;

  (*
   * Entfernen aller Gezündeter Bomben
   *)
  For i := 0 To high(BombExplodeSound) Do Begin // Merken welchen Spielern wir den Explode Sound senden sollen
    BombExplodeSound[i] := false;
  End;
  For i := fBombCount - 1 Downto 0 Do Begin
    If fBombs[i].Detonated Then Begin
      BombExplodeSound[fBombs[i].PlayerIndex] := true;
      For j := i To fBombCount - 2 Do Begin
        fBombs[j] := fBombs[j + 1];
      End;
      Dec(fBombCount);
    End;
  End;
  // So hört ein Spieler Pro Massen Explusion immer nur einen Sound !
  For i := 0 To high(BombExplodeSound) Do Begin
    If BombExplodeSound[i] Then Begin
      fPlaySoundEffect(i, seBombExplode);
    End;
  End;

  (*
   * Entfernen aller explodierten Powerups
   *)
  While Not fPowerUpClearFifo.isempty Do Begin
    p := fPowerUpClearFifo.Pop;
    If fField[p.x, p.y].PowerUp <> puNone Then Begin
      StatisticCallback(sPowerUpDestroyed);
    End;
    fField[p.x, p.y].PowerUp := puNone;
  End;
End;

Procedure TAtomicField.HandlePlayerVsMap(Var Players: TPlayers;
  PlayerGetsPowerUp: TPlayerGetsPowerUpEvent);

// Gemäß: https://www.youtube.com/watch?v=fO9HhzhEloE (bei 6:00) sieht es aber so aus,
//        das die Atomics in einem "Range" von +-2 wieder Runter kommen
Const
  TrampRange = 2;
Var
  nx, ny, x, y, i: integer;
Begin
  For i := 0 To high(Players) Do Begin
    If (Not Players[i].Info.Alive) Or (Players[i].Info.Dieing) Or (Players[i].Flying) Then Continue;
    x := trunc(Players[i].Info.Position.x);
    y := trunc(Players[i].Info.Position.y);
    // Die Kollision Spieler gegen PowerUp
    If fField[x, y].PowerUp <> puNone Then Begin
      PlayerGetsPowerUp(Players[i], i, fField[x, y].PowerUp);
      fField[x, y].PowerUp := puNone;
    End;
    // Die Kollision gegen ein Loch
    If fHasHoles And fHoles[x, y] Then Begin
      If (Not Players[i].IsInHohle) Then Begin // Flanke generieren ;)
        Players[i].IsInHohle := true;
        Players[i].Info.Animation := raTeleport;
        Players[i].Info.Counter := 0;
        Players[i].Info.Position.x := x + 0.5;
        Players[i].Info.Position.y := y + 0.5;
        fPlaySoundEffect(i, seWrapHohle);
      End;
    End
    Else Begin
      Players[i].IsInHohle := false;
    End;
    If fHastrampolins And fField[x, y].Tramp Then Begin
      fPlaySoundEffect(i, seTrampoline);
      // Triggern der Animation der Karte
      fField[x, y].TrampRunning := true;
      fField[x, y].Counter := 0;
      // Triggern des "Fliegens" des Spielers
      // 1. Neue ZielKoordinate bestimmen
      nx := -1;
      While nx = -1 Do Begin
        // So können die Atomics einfach "irgendwo" wieder landen
        // nx := random(FieldWidth);
        // ny := Random(FieldHeight);
        nx := min(FieldWidth - 1, max(0, x + random(TrampRange * 2 + 1) - TrampRange));
        ny := min(FieldHeight - 1, max(0, y + random(TrampRange * 2 + 1) - TrampRange));
        // Das Ziel Feld muss Leer sein = Kein Stein oder Tramp
        If (fField[nx, ny].BrickData <> bdBlank)
          Or (fField[nx, ny].Tramp) Then Begin
          nx := -1;
        End;
        // Das gibt es eigentlich nicht, aber auf Löchern landen ist auch verboten!
        If fHasHoles Then Begin
          If fHoles[nx, ny] Then
            nx := -1;
        End;
      End;
      Players[i].Flying := true;
      Players[i].Info.Counter := 0;
      Players[i].FlyStart := Players[i].Info.Position;
      Players[i].FlyTarget := v2(nx + 0.5, ny + 0.5);
    End;
    // Die Kollision Spieler gegen eine Flamme
    If fField[x, y].Flame <> [] Then Begin
      If Not Players[i].Info.Dieing Then Begin
        If i = fField[x, y].FlamePlayer Then Begin
          // Selfkill
          Players[fField[x, y].FlamePlayer].Kills := Players[fField[x, y].FlamePlayer].Kills - 1;
        End
        Else Begin
          // Echter Kill ;)
          Players[fField[x, y].FlamePlayer].Kills := Players[fField[x, y].FlamePlayer].Kills + 1;
        End;
        fPlaySoundEffect(i, seAtomicDie);
        fPlaySoundEffect(i, seOtherPlayerDied);
        StatisticCallback(sPlayerDeaths);
        KillPlayer(Players, i);
      End;
    End;
  End;
End;

Procedure TAtomicField.HandleFieldAnims;
Var
  i, j: Integer;
Begin
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      (*
       * Der Counter läuft pauschal immer mit, und läuft alle 60s über, alles was den Counter braucht setzt ihn auf 0 zurück
       *)
      fField[i, j].Counter := (fField[i, j].Counter + FrameRate) Mod 60000;
      If fField[i, j].Exploding Then Begin
        (*
         * Dauert es zu lange, bis Brickdate auf bdBlank gesetzt wird
         * dann sieht das Optisch doof aus (nur bei Synchronisationsthemen
         * Geht es zu schnell (oder eben so schnell wie jetzt, dann kann der Spieler)
         * viel zu Früh auf die Felder laufen, das macht er aber auf eigenes Risiko, da
         * hier natürlich ein PowerUp liegt, dass er noch nicht sieht ;)
         *)
        If fField[i, j].Counter >= 2 * UpdateRate Then Begin
          fField[i, j].BrickData := bdBlank;
        End;
        If fField[i, j].Counter >= BrickExplodeTime Then Begin
          // Die Explosionsanimation gibt es in nur 2 Fällen
          fField[i, j].Exploding := false;
        End;
      End;
      If fField[i, j].Flame <> [] Then Begin
        If fField[i, j].Counter >= FlameTime Then Begin
          // Die Explusionsanimation gibt es in nur 2 Fällen
          fField[i, j].Flame := [];
        End;
      End;
      If fField[i, j].TrampRunning Then Begin
        If fField[i, j].Counter >= AtomicTrampFlyTime Then Begin
          fField[i, j].TrampRunning := false;
        End;
      End;
    End;
  End;
End;

Procedure TAtomicField.DisableAllBombs;
Begin
  fBombsEnabled := false;
End;

Function TAtomicField.GetAiInfo(Const Players: TPlayers; TeamPlay: Boolean): TaiInfo;
Var
  i, j: Integer;
Begin
  result.Teamplay := TeamPlay;
  For i := 0 To high(Players) Do Begin
    result.PlayerInfos[i].Team := Players[i].Team;
    result.PlayerInfos[i].Position.x := Players[i].Info.Position.x;
    result.PlayerInfos[i].Position.y := Players[i].Info.Position.y;
    result.PlayerInfos[i].Alive := Players[i].Info.Alive And (Not Players[i].Info.Dieing);
    result.PlayerInfos[i].Flying := Players[i].Flying;
    result.PlayerInfos[i].FlameLength := Players[i].Powers.FlameLen;
    result.PlayerInfos[i].Speed := Players[i].Powers.Speed;
    // TODO: In die Spielerinfos mit geben welche Krankheiten der Spieler hat.
    result.PlayerInfos[i].Abilities := 0;
    If Players[i].Powers.CanKickBombs Then Begin
      result.PlayerInfos[i].Abilities := result.PlayerInfos[i].Abilities Or Ability_CanKick;
    End;
    If Players[i].Powers.CanSpooger Then Begin
      result.PlayerInfos[i].Abilities := result.PlayerInfos[i].Abilities Or Ability_CanSpoog;
    End;
    If Players[i].Powers.CanPunchBombs Then Begin
      result.PlayerInfos[i].Abilities := result.PlayerInfos[i].Abilities Or Ability_CanPunch;
    End;
    If Players[i].Powers.CanGrabBombs Then Begin
      result.PlayerInfos[i].Abilities := result.PlayerInfos[i].Abilities Or Ability_CanGrab;
    End;
    If Players[i].Powers.TriggerBomb > 0 Then Begin
      result.PlayerInfos[i].Abilities := result.PlayerInfos[i].Abilities Or Ability_CanTrigger;
    End;
    If Players[i].Powers.JellyBombs Then Begin
      result.PlayerInfos[i].Abilities := result.PlayerInfos[i].Abilities Or Ability_CanJelly;
    End;
    If dNoBombs In Players[i].Disease Then Begin
      result.PlayerInfos[i].AvailableBombs := 0;
    End
    Else Begin
      result.PlayerInfos[i].AvailableBombs := Players[i].Powers.AvailableBombs;
    End;
  End;
  // Im Debug Modus die Oberen "Tot" schalten
  For i := high(Players) + 1 To 9 Do Begin
    result.PlayerInfos[i].Alive := false;
  End;
  // Field
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      Case fField[i, j].BrickData Of
        bdSolid: result.Field[i, j] := fSolid;
        bdBrick: result.Field[i, j] := fBrick;
        bdBlank: Begin
            If fField[i, j].Flame <> [] Then Begin
              result.Field[i, j] := fFlame;
            End
            Else Begin
              Case fField[i, j].PowerUp Of
                puNone: Begin
                    result.Field[i, j] := fBlank;
                    If fHasHoles Then Begin
                      If fHoles[i, j] Then Begin
                        result.Field[i, j] := fHole;
                      End;
                    End;
                    If fHastrampolins Then Begin
                      If fField[i, j].Tramp Then Begin
                        result.Field[i, j] := fTramp;
                      End;
                    End;
                    If fHasConveyors Then Begin
                      Case fConveyorDirs[i, j] Of
                        0: result.Field[i, j] := fConveyorRight;
                        90: result.Field[i, j] := fConveyorUp;
                        180: result.Field[i, j] := fConveyorLeft;
                        270: result.Field[i, j] := fConveyorDown;
                      End;
                    End;
                    If fHasArrows Then Begin
                      Case fArrowDirs[i, j] Of
                        0: result.Field[i, j] := fArrowRight;
                        90: result.Field[i, j] := fArrowUp;
                        180: result.Field[i, j] := fArrowLeft;
                        270: result.Field[i, j] := fArrowDown;
                      End;
                    End;
                  End;
                puExtraBomb: result.Field[i, j] := fExtraBomb;
                puLongerFlameLength: result.Field[i, j] := fLongerFlame;
                puCanCick: result.Field[i, j] := fKick;
                puExtraSpeed: result.Field[i, j] := fExtraSpeed;
                puCanSpooger: result.Field[i, j] := fSpooger;
                puCanPunch: result.Field[i, j] := fPunch;
                puCanGrab: result.Field[i, j] := fGrab;
                puGoldFlame: result.Field[i, j] := fGoldflame;
                puTrigger: result.Field[i, j] := fTrigger;
                puCanJelly: result.Field[i, j] := fJelly;
                // Ab hier Krankheiten oder "schlechtes"
                puDisease: result.Field[i, j] := fBadDisease;
                puSuperBadDisease: result.Field[i, j] := fBadDisease;
                puSlow: result.Field[i, j] := fSlow;
                // Zufall, ...
                purandom: result.Field[i, j] := fRandom;
              End;
            End;
          End;
      End;
    End;
  End;
  // Bombs
  result.BombsCount := fBombCount;
  setlength(result.Bombs, fBombCount);
  For i := 0 To result.BombsCount - 1 Do Begin
    result.Bombs[i].Position.x := fBombs[i].Position.x;
    result.Bombs[i].Position.y := fBombs[i].Position.y;
    // Das kann nur bei "Fliegenden" Bombem vor kommen, fangen wir aber dennoch mal ab ;)
    If result.Bombs[i].Position.x < 0 Then result.Bombs[i].Position.x := result.Bombs[i].Position.x + FieldWidth - 1;
    If result.Bombs[i].Position.x >= FieldWidth Then result.Bombs[i].Position.x := result.Bombs[i].Position.x - FieldWidth;
    If result.Bombs[i].Position.y < 0 Then result.Bombs[i].Position.y := result.Bombs[i].Position.y + FieldHeight - 1;
    If result.Bombs[i].Position.y >= FieldWidth Then result.Bombs[i].Position.y := result.Bombs[i].Position.y - FieldHeight;
    result.Bombs[i].FlameLength := fBombs[i].FireLen;
    result.Bombs[i].Flying := fBombs[i].MoveDir = bmFly;
    result.Bombs[i].Owner := fBombs[i].PlayerIndex;
    result.Bombs[i].ManualTrigger := fBombs[i].Animation = baTimeTriggered;
    result.Bombs[i].Jelly := fBombs[i].Jelly;
    result.Bombs[i].DudBomb := fBombs[i].Animation = baDud;
    result.Bombs[i].Lifetime := fBombs[i].Lifetime;
  End;
End;

Procedure TAtomicField.IncHurry;
Var
  p: TPoint;
Begin
  inc(fHurryIndex);
  If (fHurryIndex < 0) Or (fHurryIndex > high(HurryCoords)) Then exit; // Komisch ..
  p := HurryCoords[fHurryIndex];
  fField[p.x, p.y].BrickData := bdSolid;
End;

Procedure TAtomicField.KillPlayer(Var Players: TPlayers; Index: integer);
Begin
  Players[Index].MoveState := msStill; // Zum Sterben halten wir an ;)
  Players[Index].Info.Animation := raDie;
  Players[Index].Info.Value := Random(65536); // Eine Zufällige Todesanimation wählen
  Players[Index].Info.Dieing := true;
  Players[Index].Info.Direction := 0;
  Players[Index].Info.Counter := 0;
End;

Procedure TAtomicField.RepopulatePlayersCollectedPowerUps(
  Const Players: TPlayers; PlayerIndex: Integer);
Var
  pu: TPowerUps;
  cnt, i, OverflowProtect, j: Integer;
  x, y: Int64;
  b, noHole: Boolean;
Begin
  log('TServer.RepopulatePlayersCollectedPowerUps', lltrace);
  For pu In TPowerUps Do Begin
    If pu = puNone Then Continue;
    (*
     * Bei "Fähigkeiten" begrenzen wir das ganze auf maximal 1
     *)
    cnt := Players[PlayerIndex].PowerUpCounter[pu];
    If pu In [puCanCick, puCanSpooger, puCanPunch, puCanGrab, puGoldFlame, puCanJelly] Then Begin
      cnt := min(cnt, 1);
    End;
    For i := 0 To cnt - 1 Do Begin
      OverflowProtect := 0;
      While (OverflowProtect < FieldWidth * FieldHeight) Do Begin
        x := Random(FieldWidth);
        y := Random(FieldHeight);
        // Das Feld ist Prinzipiel mal "Frei"
        noHole := true;
        If fHasHoles Then Begin // Auf Löchern dürfen keine PowerUps erzeugt werden
          If fHoles[x, y] Then noHole := false;
        End;
        If (fField[x, y].BrickData = bdBlank) And (fField[x, y].Flame = []) And (fField[x, y].PowerUp = puNone) And noHole Then Begin
          b := true;
          // Verhindern dass ein Powerup da generiert wird wo ein Spieler Steht.
          For j := 0 To high(Players) Do Begin
            If Not Players[j].Info.Alive Then Continue;
            If Players[j].Flying Then Continue;
            If (x = trunc(Players[j].Info.Position.x)) And
              (y = trunc(Players[j].Info.Position.y)) Then Begin
              b := false;
              break;
            End;
          End;
          // Verhindern, das ein Powerup da generiert wird wo eine Bombe liegt
          If b Then Begin
            For j := 0 To fBombCount - 1 Do Begin
              If fBombs[j].MoveDir = bmFly Then Continue;
              If (x = trunc(fBombs[j].Position.x)) And
                (x = trunc(fBombs[j].Position.x)) Then Begin
                b := false;
                break;
              End;
            End;
          End;
          // Wie haben eine Feld gefunden, das belegt werden kann ;)
          If b Then Begin
            fField[x, y].PowerUp := pu;
            OverflowProtect := FieldWidth * FieldHeight; // Aus der While Sauber Raus gehen..
          End;
        End;
        inc(OverflowProtect);
      End;
    End;
  End;
  LogLeave;
End;

Procedure TAtomicField.TelePortPlayer(Var Player: TPlayer);
Var
  x, y: Integer;
Begin
  (*
   * Den Spieler im Gegenuhrzegeigersinn auf der Karte drehen
   *)
  x := trunc(Player.Info.Position.x);
  y := trunc(Player.Info.Position.y);
  If x = 2 Then Begin
    If y = 2 Then Begin
      Player.Info.Position.y := 8.5;
    End
    Else Begin
      Player.Info.Position.x := 12.5;
    End;
  End
  Else Begin
    If y = 2 Then Begin
      Player.Info.Position.x := 2.5;
    End
    Else Begin
      Player.Info.Position.y := 2.5;
    End;
  End;
End;

{$ENDIF}

{$IFDEF Client}

Procedure TAtomicField.RenderBlock(x, y: integer; Brick: TBrickData);
Begin
  If Brick = bdBlank Then exit;
  glPushMatrix;
  glTranslatef(Fieldxoff + x * FieldBlockWidth, FieldyOff + y * FieldBlockHeight, 0);
  If Brick = bdSolid Then Begin
    RenderAlphaQuad(v2(FieldBlockWidth / 2, FieldBlockHeight / 2), FieldBlockWidth, -FieldBlockHeight, 0, fSolidTex);
  End
  Else Begin
    RenderAlphaQuad(v2(FieldBlockWidth / 2, FieldBlockHeight / 2), FieldBlockWidth, -FieldBlockHeight, 0, fBrickTex);
  End;
  glPopMatrix;
End;

Function TAtomicField.OnxBrickOverflow(Sender: TObject): Boolean;
Var
  index, x, y: integer;
Begin
  index := TOpenGL_Animation(sender).Tag;
  x := index Mod FieldWidth;
  y := index Div FieldWidth;
  fxBricks[x, y].Active := false;
  result := false; // Do not loop the animation !
End;

Procedure TAtomicField.RenderPreview;
Var
  j, i: Integer;
Begin
  (*
   * Die Hintergrund Graphik kann auf jeden Fall gerendert werden..
   *)
  glColor3f(1, 1, 1);
  glpushmatrix();
  glTranslatef(0, 0, atomic_Map_Layer);
  RenderQuad(v2(0, 0), v2(GameWidth, GameHeight), 0, false, fFieldTex);
  glTranslatef(0, 0, atomic_EPSILON);
  For j := 0 To 4 Do Begin
    For i := 0 To 4 Do Begin
      Case fPreviewLines[j][i + 1] Of
        '.': RenderBlock(i, j, bdBlank);
        ':': RenderBlock(i, j, bdBrick);
        '#': RenderBlock(i, j, bdSolid);
      End;
    End;
  End;
  glpopmatrix();
End;

Procedure TAtomicField.Render(Const Atomics: TAtomics; PowerTexs: TPowerTexArray
  );

  Procedure RenderArrow(x, y: integer);
  Begin
    If fArrowDirs[x, y] <> -1 Then Begin
      glPushMatrix;
      glTranslatef(Fieldxoff + x * FieldBlockWidth + 10, FieldyOff + y * FieldBlockHeight + 15, atomic_EPSILON);
      fArrows.Render(fArrowDirs[x, y]);
      glPopMatrix;
    End;
  End;

  Procedure RenderConveyor(x, y: integer);
  Begin
    If fConveyorDirs[x, y] <> -1 Then Begin
      glPushMatrix;
      glTranslatef(Fieldxoff + x * FieldBlockWidth + 00, FieldyOff + y * FieldBlockHeight + 00, atomic_EPSILON);
      fConveyors.Render(fConveyorDirs[x, y]);
      glPopMatrix;
    End;
  End;

  Procedure RenderHohle(x, y: integer);
  Begin
    If fholes[x, y] Then Begin
      glPushMatrix;
      glTranslatef(Fieldxoff + x * FieldBlockWidth + 00, FieldyOff + y * FieldBlockHeight + 00, atomic_EPSILON);
      RenderAlphaQuad(v2(FieldBlockWidth / 2, FieldBlockHeight / 2), FieldBlockWidth, -FieldBlockHeight, 0, fHoleTex);
      glPopMatrix;
    End;
  End;

  Procedure RenderTramp(x, y: integer);
  Begin
    If fField[x, y].Tramp Then Begin
      glPushMatrix;
      glTranslatef(Fieldxoff + x * FieldBlockWidth + 00, FieldyOff + y * FieldBlockHeight + 00, atomic_EPSILON);
      If fField[x, y].TrampRunning Then Begin
        fTramp.AnimationOffset := fField[x, y].TrampOffset;
        fTramp.Render(fConveyorDirs[x, y]);
      End
      Else Begin
        OpenGL_SpriteEngine.RenderSprite(fTrampStaticSprite);
      End;
      glPopMatrix;
    End;
  End;

Var
  i, j: Integer;
  FlameAni: TOpenGL_Animation;
  FlameAngle: integer;
Begin
  glColor3f(1, 1, 1);
  glpushmatrix();
  glTranslatef(0, 0, atomic_Map_Layer);
  RenderQuad(v2(0, 0), v2(GameWidth, GameHeight), 0, false, fFieldTex);
  // Alle Blöcke, Flammen Powerups etc ...
  glpushmatrix();
  glTranslatef(0, 0, atomic_EPSILON);
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      // Rendern der Brickdaten
      If fxBricks[i, j].Active Then Begin // Soll gerade die "Explode" Animation laufen ??
        glPushMatrix;
        glTranslatef(Fieldxoff + i * FieldBlockWidth, FieldyOff + j * FieldBlockHeight, 0);
        fxBricks[i, j].Ani.Render(0);
        glPopMatrix;
      End
      Else Begin
        If fField[i, j].BrickData <> bdBlank Then Begin
          RenderBlock(i, j, fField[i, j].BrickData);
        End
        Else Begin
          // Rendern der Flamen
          If fField[i, j].Flame <> [] Then Begin
            // Wählen der Richtigen Animation
            If fCross In fField[i, j].Flame Then Begin
              FlameAni := Atomics[fField[i, j].FlameColor].FlameCross;
            End
            Else Begin
              If fend In fField[i, j].Flame Then Begin
                FlameAni := Atomics[fField[i, j].FlameColor].FlameEnd;
              End
              Else Begin
                FlameAni := Atomics[fField[i, j].FlameColor].FlameMiddle;
              End;
            End;
            FlameAngle := 0;
            If fup In fField[i, j].Flame Then FlameAngle := 90;
            If fleft In fField[i, j].Flame Then FlameAngle := 180;
            If fdown In fField[i, j].Flame Then FlameAngle := 270;
            glPushMatrix;
            glTranslatef(Fieldxoff + i * FieldBlockWidth, FieldyOff + j * FieldBlockHeight, 0);
            FlameAni.Render(FlameAngle);
            glPopMatrix;
          End
          Else Begin
            // Render der Powerups
            If fField[i, j].PowerUp <> puNone Then Begin
              glPushMatrix;
              glTranslatef(Fieldxoff + i * FieldBlockWidth, FieldyOff + j * FieldBlockHeight, 0);
              RenderQuad(v2(20, 18), 40, 36, 0, PowerTexs[fField[i, j].PowerUp]);
              glPopMatrix;
            End
            Else Begin
              // Es ist einfach nichts auf der Kachel, ggf ist dann ja ein Pfeil oder ein Laufband zu sehen ..
              If fHasArrows Then Begin
                RenderArrow(i, j);
              End;
              If fHasConveyors Then Begin
                RenderConveyor(i, j);
              End;
              If fHasHoles Then Begin
                RenderHohle(i, j);
              End;
              If fHastrampolins Then Begin
                RenderTramp(i, j);
              End;
            End;
          End;
        End;
      End;
    End;
  End;
  glpopmatrix();
  glpopmatrix();
End;

Procedure TAtomicField.ReadGameingData(Const Stream: TStream);

Var
  i, j: Integer;
Begin
  (*
   * Da auf jedem Feld immer nur 1 Ding Gerendert werden kann ist das hier recht Easy ;)
   *)
  Stream.Read(fField, sizeof(fField));
  // GGF Starten einer Brick Explosion Animation
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      (*
       * Das ist ein Bischen Tricky da wir keinen direkten Event bekommen
       * nur die Information dass die Explosionsanimation laufen soll
       * und einen Counter wie Lange sie schon läuft..
       *)
      If fField[i, j].ExplodingRenderFlag Then Begin
        fxBricks[i, j].ani.ResetAnimation();
        fxBricks[i, j].Active := true;
      End;
    End;
  End;
End;

Procedure TAtomicField.Reset;
Var
  i, j: Integer;
Begin
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      fxBricks[i, j].Active := false;
    End;
  End;
End;

{$ENDIF}

End.

