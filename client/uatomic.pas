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
Unit uatomic;

{$MODE ObjFPC}{$H+}

{$I globaldefines.inc}

Interface

Uses
  Classes, SysUtils, uvectormath, uatomic_common, uatomic_global
  , Graphics
  , uGraphics
  , uopengl_animation
  , uopengl_graphikengine
  ;

Type

  TGameState = (
    gs_MainMenu, // Der Spieler ist im Hauptmenü
    gs_Gaming // Das Spiel läuft
    );

  tAtomicAnimation = (
    aaStandStill,
    aaWalk,
    aaKick, // Kicken
    aaPunch, // Weg schlagen (Roter Handschu)
    aaPup // Die Pupserkrankheit
    );

  TAnimation = Record
    Ani: TOpenGL_Animation;
    OffsetX, OffsetY: integer;
  End;

  TAnimations = Array Of TAnimation;

  { TAtomic }

  TAtomic = Class
  private
    fDieAnimations: TAnimations;
    fLockedInAnimations: TAnimations;
    fZenAnimations: TAnimations;
    fAnimations: Array[tAtomicAnimation] Of TAnimation;
    fShadowTex: TGraphikItem;

  public
    FlameEnd, FlameCross, FlameMiddle: TOpenGL_Animation;
    Bomb: TAnimation;
    Bomb_dud: TAnimation;
    Bomb_trigger: TAnimation;
    Bomb_Wobble: TAnimation;

    Constructor Create(); virtual;
    Destructor Destroy(); override;
    Function GetAnimTimeInMs(Anim: TRenderAnimation; Value: uint16): Integer;
    Function InitAsColor(path: String; aTargetColor: TRGB): Boolean;

    Procedure Render(Const Info: TAtomicInfo; Edge: Boolean);
  End;

  TAtomics = Array[0..length(PlayerColors) - 1] Of TAtomic;

Implementation

Uses
  math
  , dglOpenGL
  , IntfGraphics
  , GraphType
  ;

Type

  TBufferedTextures = Record
    OrigFilename: String;
    Image: TLazIntfImage; // Das PNG, einmalig dekodiert als R8G8B8A8
  End;

Var
  BufferedTextures: Array Of TBufferedTextures = Nil;

(*
 * Liefert das PNG als R8G8B8A8 Rohdaten, jede Datei wird nur 1 mal dekodiert.
 *)
Function GetRawImage(PNGImage: String): TLazIntfImage;
Var
  Desc: TRawImageDescription;
  Reader: TLazReaderPNG;
  i: Integer;
Begin
  For i := 0 To High(BufferedTextures) Do Begin
    If BufferedTextures[i].OrigFilename = PNGImage Then Begin
      result := BufferedTextures[i].Image;
      exit;
    End;
  End;
  result := TLazIntfImage.Create(0, 0);
  Desc.Init_BPP32_R8G8B8A8_BIO_TTB(0, 0);
  result.DataDescription := Desc;
  Reader := TLazReaderPNG.Create;
  Try
    result.LoadFromFile(PNGImage, Reader);
  Finally
    Reader.Free;
  End;
  SetLength(BufferedTextures, High(BufferedTextures) + 2);
  BufferedTextures[High(BufferedTextures)].OrigFilename := PNGImage;
  BufferedTextures[High(BufferedTextures)].Image := result;
End;

(*
 * Färbt PNGImage in PlayerColor ein und lädt es unter Name als Textur in die Graphikengine.
 * Die Textur hat die Originalgröße (kein Auffüllen auf 2^n, spart ca. 40% Speicher), Fuchsia wird
 * nach dem Einfärben transparent (Alpha = 255).
 *)
Function LoadColorTabledTexture(PNGImage: String; PlayerColor: TRGB; Name: String): TGraphikItem;
Var
  Img: TLazIntfImage;
  pSrc, pDst, pStart: PByte;
  r, g, b, n, k, m, x, y, w, h: Integer;
  cr, cg, cb: Integer;
Begin
  result := Default(TGraphikItem);
  If Not FileExists(PNGImage) Then Exit;
  // Jede Kombination aus Datei und Farbe wird nur 1 mal eingefärbt (z.B. flame.png wird 3 mal angefragt)
  If OpenGL_GraphikEngine.TryGetItem(Name, result) Then Exit;

  Img := GetRawImage(PNGImage);
  w := Img.Width;
  h := Img.Height;
  cr := PlayerColor.r;
  cg := PlayerColor.g;
  cb := PlayerColor.b;
  GetMem(pStart, w * h * 4);
  Try
    For y := 0 To h - 1 Do Begin
      pSrc := Img.GetDataLineStart(y);
      pDst := pStart + y * w * 4;
      For x := 0 To w - 1 Do Begin
        r := pSrc[0];
        g := pSrc[1];
        b := pSrc[2];

        If (g > r) And (g > b) Then Begin
          n := (r + b) Div 2;
          k := (g - n);

          r := n + (k * cr) Div 100;
          g := n + (k * cg) Div 100;
          b := n + (k * cb) Div 100;

          m := Max(r, Max(g, b));
          If m > 255 Then Begin
            r := (r * 255) Div m;
            g := (g * 255) Div m;
            b := (b * 255) Div m;
          End;
        End;

        pDst[0] := r;
        pDst[1] := g;
        pDst[2] := b;
        // Achtung, Alpha ist invertiert: 255 = transparent
        If (r = 255) And (g = 0) And (b = 255) Then
          pDst[3] := 255
        Else
          pDst[3] := 0;
        inc(pSrc, 4);
        inc(pDst, 4);
      End;
    End;
    result := OpenGL_GraphikEngine.LoadRGBAGraphikItem(pStart, w, h, w, h, Name, true);
  Finally
    FreeMem(pStart);
  End;
End;

{ TAtomic }

Constructor TAtomic.Create;
Begin
  Inherited Create();
End;

Destructor TAtomic.Destroy;
Var
  ta: tAtomicAnimation;
  i: integer;
Begin
  For i := 0 To high(fZenAnimations) Do Begin
    fZenAnimations[i].ani.Free;
  End;
  For i := 0 To high(fLockedInAnimations) Do Begin
    fLockedInAnimations[i].ani.Free;
  End;
  For i := 0 To high(fDieAnimations) Do Begin
    fDieAnimations[i].ani.Free;
  End;

  For ta In tAtomicAnimation Do Begin
    fAnimations[ta].ani.free;
  End;
  Bomb.ani.free;
  Bomb_dud.ani.free;
  Bomb_trigger.ani.free;
  Bomb_Wobble.ani.free;
  FlameCross.free;
  FlameMiddle.free;
  FlameEnd.free;
End;

Function TAtomic.GetAnimTimeInMs(Anim: TRenderAnimation; Value: uint16
  ): Integer;

Var
  Ani: TOpenGL_Animation;
Begin
  result := 0;
  ani := Nil;
  Case Anim Of
    raStandStill: ani := Nil;
    raWalk: ani := Nil;
    raKick: ani := fAnimations[aaKick].ani;
    raPunch: ani := fAnimations[aaPunch].ani;
    raPup: ani := fAnimations[aaPup].ani;
    raDie: Begin
        //ani := fDieAnimations[Value Mod length(fDieAnimations)].Ani;
        // Die Todesanimation ist ein Sonderfall, die darf nicht ablaufen, da nach der Animation der Atomic nicht mehr gerendert werden darf !
        result := AtomicDieTimeout * 2;
      End;
    raZen: ani := fZenAnimations[Value Mod length(fZenAnimations)].Ani;
    raLockedIn: ani := fLockedInAnimations[Value Mod length(fLockedInAnimations)].Ani;
  Else Begin
      Raise exception.Create('Error, missing implementation: TAtomic.GetAnimTimeInMs');
    End;
  End;
  If assigned(ani) Then Begin
    result := ani.Sprite[0].FrameCount * ani.Sprite[0].TimePerFrame;
  End;
End;

Function TAtomic.InitAsColor(path: String; aTargetColor: TRGB): Boolean;

  Function CreateAnimation(Filename: String; AngleOffset: Single; TimePerFrame, w, h, fpr, fpc, c: integer; Const RangeData: Array Of Integer): TOpenGL_Animation;
  Var
    gi: TGraphikItem;
    s: TAniSprite;
    i: Integer;
    fn: String;
  Begin
    result := Nil;
    If Not FileExists(Filename) Then exit;
    fn := ExtractFileName(Filename) + ColorToString(RGBToColor(aTargetColor));
    (*
     * Die Textur wird direkt aus den Rohdaten erzeugt, die Sprites haben kein Bitmap
     * (spart den RAM und die Umwege über TBitmap). Der Name entspricht dem, den
     * TOpenGL_Animation.CreateOpenGLData selbst vergeben würde.
     *)
    gi := LoadColorTabledTexture(Filename, aTargetColor, '-Sprite' + fn + 'Image');
    If gi.Image = 0 Then exit;
    result := TOpenGL_Animation.Create;
    result.AngleOffset := AngleOffset;
    For i := 0 To high(RangeData) Do Begin
      If i <> 0 Then result.AddRange(true); // Die Animation wird bereits mit einem Range erzeugt, also darf der 1. nicht generiert werden ;)
      s := Result.Sprite[i];
      s.Bitmap := Nil;
      s.Derived := i <> 0;
      s.AlphaImage := true;
      s.AlphaMask := Nil;
      s.TimePerFrame := TimePerFrame;
      s.rect := rect(0, 0, gi.OrigWidth, gi.OrigHeight);
      s.Name := fn;
      s.Width := w;
      s.Height := h;
      s.FramesPerRow := fpr;
      s.FramesPerCol := fpc;
      s.FrameOffset := RangeData[i];
      s.FrameCount := c;
      Result.Sprite[i] := s;
    End;
    // Die OpenGL Daten dürfen erst erstellt werden, wenn alle Daten gültig drin sind
    Result.CreateOpenGLData(0, gi);
    For i := 1 To high(RangeData) Do Begin
      Result.CreateOpenGLData(i);
    End;
  End;

  Function LoadDir(Dir: String; Var Animations: TAnimations): Boolean;
  Var
    sl: TStringList;
    w, h, tpf, fpc, fpr, i, c, rox, roy: integer;
    sa: TStringArray;
  Begin
    result := true;
    sl := TStringList.Create;
    sl.LoadFromFile(dir + 'files.txt');
    For i := 0 To sl.Count - 1 Do Begin
      If trim(sl[i]) = '' Then Continue;
      If pos('#', trim(sl[i])) = 1 Then Continue;
      sa := sl[i].Split(';');
      If length(sa) <> 9 Then exit(false);
      If Not FileExists(dir + sa[0]) Then Begin
        Log(format('Warning, can not load animation, file: %s does not exist', [dir + sa[0]]), llWarning);
        Continue; // Gibt es die eine Animation nicht, dann wird sie ignoriert
      End;
      tpf := strtointdef(sa[1], -1);
      If tpf = -1 Then exit(false);
      fpr := strtointdef(sa[2], -1);
      If fpr = -1 Then exit(false);
      fpc := strtointdef(sa[3], -1);
      If fpc = -1 Then exit(false);
      w := StrToIntDef(sa[4], -1);
      If w = -1 Then exit(false);
      h := StrToIntDef(sa[5], -1);
      If h = -1 Then exit(false);
      c := strtointdef(sa[6], -1);
      If c = -1 Then exit(false);
      rox := strtointdef(sa[7], 0);
      roy := strtointdef(sa[8], 0);

      setlength(Animations, high(Animations) + 2);
      Animations[high(Animations)].Ani := CreateAnimation(dir + sa[0], 0, tpf, w, h, fpr, fpc, c, [0]);
      Animations[high(Animations)].OffsetX := rox;
      Animations[high(Animations)].Offsety := roy;
{$IFDEF Only1Directory}
      // Immer nur den ersten Eintrag laden, der Rest wird eingespart ;
      break;
{$ENDIF}
    End;
    sl.free;
    If Length(Animations) = 0 Then result := false;
  End;

Begin
  result := false;
  (*
   * Laden aller Texturen die ein Einzelner Spieler so haben kann
   *)
  fShadowTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(path + 'shadow.png', smClamp);
  fAnimations[aaWalk].Ani := CreateAnimation(path + 'walk.png', 45, 50, 110, 110, 15, 4, 15, [0, 15, 45, 30]);
  fAnimations[aaWalk].OffsetX := 55;
  fAnimations[aaWalk].OffsetY := 90;
  If Not assigned(fAnimations[aaWalk].Ani) Then exit;

  fAnimations[aaStandStill].ani := CreateAnimation(path + 'stand.png', 45, 100, 110, 110, 2, 2, 1, [0, 1, 3, 2]);
  fAnimations[aaStandStill].OffsetX := 55;
  fAnimations[aaStandStill].OffsetY := 90;
  If Not assigned(fAnimations[aaStandStill].ani) Then exit;

  fAnimations[aaKick].ani := CreateAnimation(path + 'kick.png', 45, 50, 50, 69, 10, 4, 10, [10, 0, 30, 20]);
  fAnimations[aaKick].OffsetX := 25;
  fAnimations[aaKick].OffsetY := 60;
  If Not assigned(fAnimations[aaKick].ani) Then exit;

  fAnimations[aaPunch].ani := CreateAnimation(path + 'punbomb.png', 45, 50, 110, 110, 10, 4, 10, [30, 10, 20, 0]);
  fAnimations[aaPunch].OffsetX := 55;
  fAnimations[aaPunch].OffsetY := 90;
  If Not assigned(fAnimations[aaPunch].ani) Then exit;

  fAnimations[aaPup].ani := CreateAnimation(path + 'pupbomb.png', 45, 50, 110, 110, 11, 4, 11, [11, 33, 0, 22]);
  fAnimations[aaPup].OffsetX := 55;
  fAnimations[aaPup].OffsetY := 90;
  If Not assigned(fAnimations[aaPup].ani) Then exit;


  Bomb.ani := CreateAnimation(path + 'bomb.png', 0, 50, 40, 37, 5, 4, 18, [0]);
  Bomb.OffsetX := -20;
  Bomb.OffsetY := -37 Div 2;
  If Not assigned(Bomb.ani) Then exit;
  Bomb_dud.ani := CreateAnimation(path + 'bomb_dud.png', 0, 750, 40, 40, 1, 2, 2, [0]);
  Bomb_dud.OffsetX := -20;
  Bomb_dud.OffsetY := -20;
  If Not assigned(Bomb_dud.ani) Then exit;
  Bomb_trigger.ani := CreateAnimation(path + 'bomb_trigger.png', 0, 50, 40, 40, 5, 4, 18, [0]);
  Bomb_trigger.OffsetX := -20;
  Bomb_trigger.OffsetY := -20;
  If Not assigned(Bomb_trigger.ani) Then exit;
  Bomb_Wobble.ani := CreateAnimation(path + 'bomb_wobble.png', 0, 75, 40, 37, 4, 3, 12, [0]);
  Bomb_Wobble.OffsetX := -20;
  Bomb_Wobble.OffsetY := -37 Div 2;
  If Not assigned(Bomb_Wobble.ani) Then exit;
  // Grid is 5 columns x 9 rows, each frame is 41x37 pixels
  // Each row is a 5-frame animation of one flame element:
  // Row 0 (frames 0-4): FlameCross - 5-frame animation of cross pattern
  // Row 1 (frames 5-9): FlameEnd up (vertical top end) - 5-frame animation
  // Row 2 (frames 10-14): FlameEnd left (horizontal left end) - 5-frame animation
  // Row 3 (frames 15-19): FlameEnd down (vertical bottom end) - 5-frame animation
  // Row 4 (frames 20-24): FlameEnd right (horizontal right end) - 5-frame animation
  // Row 5 (frames 25-29): FlameMiddle up (vertical middle going up) - 5-frame animation
  // Row 6 (frames 30-34): FlameMiddle down (vertical middle going down) - 5-frame animation
  // Row 7 (frames 35-39): FlameMiddle right (horizontal middle going right) - 5-frame animation
  // Row 8 (frames 40-44): FlameMiddle left (horizontal middle going left) - 5-frame animation
  // Each range corresponds to a direction: 0°=right, 90°=up, 180°=left, 270°=down
  FlameCross := CreateAnimation(path + 'flame.png', 0, 50, 41, 37, 5, 9, 5, [0]); // Row 0: cross animation (5 frames)
  If Not assigned(FlameCross) Then exit;
  // FlameMiddle: 4 ranges for 4 directions, each with 5-frame animation
  // Frame offsets: 40=right(0°), 30=up(90°), 35=left(180°), 25=down(270°) - swapped right/left and up/down
  FlameMiddle := CreateAnimation(path + 'flame.png', 0, 50, 41, 37, 5, 9, 5, [40, 30, 35, 25]);
  If Not assigned(FlameMiddle) Then exit;
  // FlameEnd: 4 ranges for 4 directions, each with 5-frame animation
  // EqualizeRanges divides 360° into 4 equal parts: Range 0=0-89°, Range 1=90-179°, Range 2=180-269°, Range 3=270-359°
  // When FlameAngle is set: 0°=right, 90°=up, 180°=left, 270°=down
  // Frame offsets: 20=right(0°), 5=up(90°), 10=left(180°), 15=down(270°)
  FlameEnd := CreateAnimation(path + 'flame.png', 0, 50, 41, 37, 5, 9, 5, [20, 5, 10, 15]); // right, up, left, down
  If Not assigned(FlameEnd) Then exit;
  If Not LoadDir(Path + 'idle' + PathDelim, fZenAnimations) Then exit;
  If Not LoadDir(Path + 'locked_in' + PathDelim, fLockedInAnimations) Then exit;
  If Not LoadDir(Path + 'die' + PathDelim, fDieAnimations) Then exit;

  result := true;
End;

Procedure TAtomic.Render(Const Info: TAtomicInfo; Edge: Boolean);
Const // TODO: Redundant zu uatomic_field !
  xOff = 20;
  yOff = 66;
Var
  fAnimation: TAnimation;
  aDirection: Single;
Begin
  aDirection := Info.Direction;
  SetShaderAlphaThreshold(0.5);
  // Die Notwendige Animation
  If Info.Dying Then Begin
    // Das hier Triggert hoffentlich nur bei der ersten Steigenden Flanke
    fAnimation := fDieAnimations[Info.Value Mod length(fDieAnimations)];
    (*
     * Wenn die Animation abgelaufen ist -> Abschalten und warten bis das Spiel beendet ist.
     *)
    If info.Counter >= fAnimation.Ani.Sprite[0].TimePerFrame * fAnimation.Ani.Sprite[0].FrameCount Then Begin
      fAnimation.Ani := Nil;
    End;
  End
  Else Begin
    // Der Schatten
    RenderAlphaQuad(info.Position.x * FieldBlockWidth + xOff - fShadowTex.OrigWidth / 2, (info.Position.y + 0.25) * FieldBlockHeight + yOff - fShadowTex.OrigHeight / 2, atomic_Layer, fShadowTex);
    // anhand der Info die Passende Animation wählen !
    Case Info.Animation Of
      raStandStill: fAnimation := fAnimations[aaStandStill];
      raWalk: fAnimation := fAnimations[aaWalk];
      raKick: fAnimation := fAnimations[aaKick];
      raPunch: fAnimation := fAnimations[aaPunch];
      raPup: fAnimation := fAnimations[aaPup];
      raDie: fAnimation := fDieAnimations[Info.Value Mod length(fDieAnimations)]; // Eigentlich müsste es diesen Fall gar nicht geben da info.Dieing ja gesetzt ist dabei ..
      raZen: fAnimation := fZenAnimations[Info.Value Mod length(fZenAnimations)];
      raLockedIn: fAnimation := fLockedInAnimations[Info.Value Mod length(fLockedInAnimations)];
      raTeleport: Begin
          // Die Idee, dass die Animation alle 50 ms um 90 Grad weiter dreht, denn dem Atomic soll schwindelig werden !
          fAnimation := fAnimations[aaWalk];
          aDirection := ((Info.Counter Mod 200) Div 50) * 90;
          Edge := True; // Der User soll immer das 1. Frame sehen, denn der Atomic steht ja wenn er sich dreht
        End;
    End;
  End;
  If assigned(fAnimation.Ani) Then Begin
    If Edge And (info.Animation <> raWalk) Then Begin
      fAnimation.Ani.ResetAnimation();
    End;
    fAnimation.ani.Render(info.Position.x * FieldBlockWidth + xOff - fAnimation.OffsetX, (info.Position.y + 0.25) * FieldBlockHeight + yOff - fAnimation.OffsetY, atomic_Layer + atomic_EPSILON, aDirection);
  End;
  SetShaderAlphaThreshold(0);

  (*
   * Zum Debuggen ein Punkt exakt da wo finfo.Position ist !
   * )
  glPushMatrix;
  glDisable(GL_DEPTH_TEST);
  glTranslatef(finfo.Position.x * FieldBlockWidth + xOff, finfo.Position.y * FieldBlockHeight + yOff, atomic_Layer);
  glPointSize(3);
  glBegin(GL_POINTS);
  glColor3f(1, 0, 0);
  glVertex2f(0, 0);
  glend();
  glPointSize(1);
  glEnable(GL_DEPTH_TEST);
  glPopMatrix;
  // Ende Debuggen *)
End;


Var
  i: integer;

Finalization
  For i := 0 To high(BufferedTextures) Do Begin
    BufferedTextures[i].Image.Free;
  End;
  setlength(BufferedTextures, 0);

End.

