(******************************************************************************)
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
(*
 * The atomic: all sprite sheets of the player figure, built from the rig in
 * uag_rig. Every animation is a function frame -> pose (plus a few effects
 * like smoke or confetti for the death animations).
 *
 * Files (layout as expected by client/uatomic.pas InitAsColor):
 *   walk.png, bwalk.png   15 x 4 cells 110x110, rows right, up, down, left
 *   stand.png             2 x 2 cells, frames right, up, down, left
 *   kick.png              10 x 4 cells 50x69, rows up, right, down, left
 *   punbomb.png           10 x 4 cells, rows down, up, left, right
 *   pupbomb.png           11 x 4 cells, rows left, right, down, up
 *   shadow.png            29 x 17, real alpha
 *   die/, idle/, locked_in/  own animations + files.txt (front view), the names
 *                         differ from the original ones (other grids)
 * Feet (ground point) at (55, 90) of a 110x110 cell, (25, 60) for kick.
 *
 * The base set uses the area 'atomic', with Ctx.PerPlayer every player j
 * gets an own figure in atomic/p<j> from the area 'atomic/p<j>'. atomic/hunted
 * is the base figure with a crown.
 *)
Unit uag_gen_atomic;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Uses uag_image, uag_context, uag_output;

(*
 * All sheets of one figure into Dir (e.g. 'atomic/p3/'), Area is the random
 * source ('atomic' or 'atomic/p<j>'). Used by the generator and the editor.
 *)
Procedure GenerateFigure(Ctx: TAGContext; Output: TAGOutput; Const Area: TAGArea; Const Dir: String; Hunted: Boolean);

(*
 * Pixel style: outline of Scale pixel around the silhouette in every w x h
 * cell of Img. Selective = 16 bit console look: on the lit side (top, left)
 * the outline is a dark tone of the neighbouring part instead of Col.
 *)
Procedure PixelOutline(Img: TAGImage; w, h, Scale: Integer; Col: TAGColor; Selective: Boolean = false);

Implementation

Uses SysUtils, uag_math, uag_canvas, uag_render, uag_registry, uag_rig;

Const
  CellW = 110;
  CellH = 110;
  FootX = 55;
  FootY = 90;
  // the side views are drawn as a three quarter view, the face stays visible
  FacingR = 0.85;
  FacingL = -0.85;
  // the kick cell has only 60 pixel above the ground point and is 50 pixel wide, the wider styles a bit smaller
  KickScale: Array[TAGStyle] Of Double = (0.84, 0.76, 0.8, 0.73);
  FilesHeader = '#Filename; AnimationTimePerFrame; FramesPerRow; FramesPerCol; FrameWidth, FrameHeight, TotalFrameCount; RenderOffsetX, RenderOffsetY';

  FilesPerSet = 7 + 9 + 5 + 5; // sheets + die (8 + files.txt) + idle (4 + 1) + locked_in (4 + 1)

Type
  TAnimKind = (akStand, akWalk, akCarry, akKick, akPunch, akPup);

  TClipKind = (
    ckDieSmoke, ckDieMelt, ckDieGhost, ckDieSpin, ckDieBurnt, ckDiePop, ckDieFlat, ckDieBeam,
    ckIdleWave, ckIdleStretch, ckIdleLook, ckIdleHover,
    ckLockPanic, ckLockHop, ckLockCower, ckLockPush);

Const
  ClipFrames: Array[TClipKind] Of Integer = (
    26, 28, 32, 28, 30, 24, 26, 24,
    40, 36, 48, 48,
    24, 20, 24, 32);

(* 0..1 between a and b with soft start and end *)
Function Ease(t, a, b: Double): Double;
Begin
  If b <= a Then exit(ord(t >= b));
  result := AGSmoothStep((t - a) / (b - a));
End;

Function Lin(t, a, b: Double): Double;
Begin
  If b <= a Then exit(ord(t >= b));
  result := AGClamp((t - a) / (b - a), 0, 1);
End;

(* ---------------------------------------------------------------------------- *)
(* Poses                                                                         *)

(*
 * Arms up, the hands hold the bomb the engine draws above the figure (see
 * TGame.RenderCarriedBomb in client/ugame.pas: CarryHeight = 1.4 fields =
 * 50.4 pixel above the player position, the bomb sprite 40 x 37 with the
 * offset (-20, -18), its ball (radius about 12) has its bottom at y = 35 of
 * the sprite). In cell coordinates the ball is at (CarryBallX, CarryBallY).
 * The arm angles are solved per frame and facing (small grid search), so the
 * gloves touch the ball on both sides whatever the proportions and the bob
 * of the walk are.
 *)
Const
  CarryBallX = 54.5;
  CarryBallY = FootY - 0.25 * 36 - 1.4 * 36 - 37 Div 2 + 35 - 12.2; // 35.4
  CarryBallR = 12.2;

Procedure SetCarryArms(Var p: TPose; Const Fig: TFigure; Phase, Facing: Double);
Var
  i, a, b, side, it: Integer;
  tx, ty, best, d, sw, spr, bsw, bspr, step0, step1: Double;
  q: TPose;
  hp: Array[0..1] Of TAGPoint;

  Function Cost(Arm: Integer; Swing, Spread: Double): Double;
  Var
    pt: TAGPoint;
  Begin
    q.Arm[Arm].Swing := Swing;
    q.Arm[Arm].Spread := Spread;
    pt := FigurePoint(Fig, q, Facing, FootX, FootY, HandPoint(Fig, q, Arm));
    result := sqr(pt.x - tx) + sqr(pt.y - ty) + 0.5 * sqr(Spread - 0.75); // prefer a natural spread
  End;

Begin
  For i := 0 To 1 Do Begin
    p.Arm[i].Swing := AG_PI - 0.2 + 0.06 * AGSin(Phase + i * AG_PI);
    p.Arm[i].Spread := 0.75;
    p.Arm[i].Bend := 0.15;
    p.Arm[i].BendSpread := -0.65;
  End;
  p.Reach := 1.3;
  p.HandsFront := 1;
  q := p;
  // which glove is on the left of the screen
  For i := 0 To 1 Do
    hp[i] := FigurePoint(Fig, q, Facing, FootX, FootY, HandPoint(Fig, q, i));
  For i := 0 To 1 Do Begin
    If (hp[i].x > hp[1 - i].x) Or ((hp[i].x = hp[1 - i].x) And (i = 0)) Then side := 1 Else side := -1;
    tx := CarryBallX + side * (CarryBallR + Fig.GloveR * 0.1);
    ty := CarryBallY + CarryBallR * 0.1;
    bsw := p.Arm[i].Swing;
    bspr := p.Arm[i].Spread;
    best := Cost(i, bsw, bspr);
    // coarse grid, then refine around the best point
    For a := 0 To 20 Do
      For b := 0 To 12 Do Begin
        sw := 1.4 + 2.2 * a / 20;
        spr := -0.2 + 1.8 * b / 12;
        d := Cost(i, sw, spr);
        If d < best Then Begin
          best := d;
          bsw := sw;
          bspr := spr;
        End;
      End;
    step0 := 0.055;
    step1 := 0.075;
    For it := 0 To 5 Do Begin
      For a := -2 To 2 Do
        For b := -2 To 2 Do Begin
          sw := bsw + a * step0;
          spr := bspr + b * step1;
          d := Cost(i, sw, spr);
          If d < best Then Begin
            best := d;
            bsw := sw;
            bspr := spr;
          End;
        End;
      step0 := step0 * 0.45;
      step1 := step1 * 0.45;
    End;
    p.Arm[i].Swing := bsw;
    p.Arm[i].Spread := bspr;
    q.Arm[i] := p.Arm[i];
  End;
End;

Function WalkPose(Const Fig: TFigure; Phase: Double; Carry: Boolean; Facing: Double): TPose;
Var
  sp, cp: Double;
Begin
  result := NeutralPose;
  sp := AGSin(Phase);
  cp := AGCos(Phase);
  result.Leg[0].Swing := Fig.Stride * sp;
  result.Leg[1].Swing := -Fig.Stride * sp;
  result.Leg[0].Bend := Fig.StepHeight * AGMax(0, cp) + 0.1;
  result.Leg[1].Bend := Fig.StepHeight * AGMax(0, -cp) + 0.1;
  result.Leg[0].Swing := result.Leg[0].Swing + 0.5 * result.Leg[0].Bend; // lift the knee, keep the foot under the body
  result.Leg[1].Swing := result.Leg[1].Swing + 0.5 * result.Leg[1].Bend;
  result.RootU := Fig.Bob * (AGAbs(cp) - 0.6);
  result.Lean := 0.1;
  result.Roll := 0.04 * sp;
  result.HeadNod := -0.04 * AGAbs(cp);
  If Carry Then Begin
    result.Lean := 0.0;
    SetCarryArms(result, Fig, Phase, Facing);
  End
  Else Begin
    result.Arm[0].Swing := -Fig.ArmSwing * sp;
    result.Arm[1].Swing := Fig.ArmSwing * sp;
    result.Arm[0].Bend := 0.35 + 0.3 * AGMax(0, -sp);
    result.Arm[1].Bend := 0.35 + 0.3 * AGMax(0, sp);
    result.Arm[0].Spread := 0.3;
    result.Arm[1].Spread := 0.3;
  End;
End;

Function KickPose(Const Fig: TFigure; i, n: Integer): TPose;
Var
  t, w, k, b: Double;
  p: TPose;
Begin
  t := i / n;
  p := NeutralPose;
  w := Ease(t, 0, 0.3) * (1 - Ease(t, 0.3, 0.45)); // wind up
  k := Ease(t, 0.3, 0.45) * (1 - Ease(t, 0.65, 1.0)); // strike
  p.Leg[0].Swing := -0.7 * w + 1.25 * k;
  p.Leg[0].Bend := 1.1 * w + 0.1 * k;
  p.Leg[1].Bend := 0.25 * (w + k);
  p.Leg[1].Swing := 0.12 * (w + k);
  p.RootU := -1.2 * Fig.Scale * (w + k);
  p.Lean := 0.12 * w - 0.22 * k;
  b := w + k;
  p.Arm[0].Swing := 0.6 * w - 0.5 * k;
  p.Arm[1].Swing := -0.3 * w + 0.7 * k;
  p.Arm[0].Spread := 0.18 + 0.3 * b; // the kick cell is only 50 pixel wide
  p.Arm[1].Spread := 0.18 + 0.3 * b;
  p.Arm[0].Bend := 0.3 + 0.4 * b;
  p.Arm[1].Bend := 0.3 + 0.4 * b;
  p.HeadNod := 0.1 * k;
  If (k > 0.6) Then p.EyeOpen := 0.35;
  result := p;
End;

Function PunchPose(Const Fig: TFigure; i, n: Integer): TPose;
Var
  t, w, k: Double;
  p: TPose;
Begin
  t := i / n;
  p := NeutralPose;
  w := Ease(t, 0, 0.3) * (1 - Ease(t, 0.3, 0.45));
  k := Ease(t, 0.3, 0.45) * (1 - Ease(t, 0.6, 1.0));
  p.Arm[0].Swing := -0.5 * w + 1.5 * k;
  p.Arm[0].Bend := 1.5 * w + 0.05 * k;
  p.Arm[0].Spread := 0.25 - 0.15 * k;
  p.Arm[1].Swing := 0.4 * w - 0.5 * k;
  p.Arm[1].Bend := 0.5 + 0.6 * k;
  p.Lean := -0.05 * w + 0.25 * k;
  p.Roll := 0.05 * w - 0.06 * k;
  p.Leg[0].Swing := 0.35 * k;
  p.Leg[0].Bend := 0.2 * k;
  p.Leg[1].Swing := -0.3 * k;
  p.RootF := 2.0 * Fig.Scale * k;
  p.RootU := -0.8 * Fig.Scale * k;
  p.HeadNod := 0.08 * k;
  result := p;
End;

Function CrouchPose(Depth: Double): TPose; // Depth 0..1
Var
  i: Integer;
Begin
  result := NeutralPose;
  For i := 0 To 1 Do Begin
    result.Leg[i].Swing := 0.75 * Depth;
    result.Leg[i].Bend := 1.5 * Depth;
    result.Leg[i].Spread := 0.04 + 0.12 * Depth;
  End;
  result.Lean := 0.35 * Depth;
End;

Function PupPose(Const Fig: TFigure; i, n: Integer; Facing: Double): TPose;
Var
  t, d, u: Double;
  p, c: TPose;
  j: Integer;
Begin
  t := i / (n - 1);
  d := Ease(t, 0, 0.35) * (1 - Ease(t, 0.5, 0.85)); // down
  u := Ease(t, 0.5, 1.0); // arms up
  p := CrouchPose(d);
  p.RootU := -Fig.LegLen * 0.32 * d;
  For j := 0 To 1 Do Begin
    p.Arm[j].Swing := 0.05 + 0.85 * d;
    p.Arm[j].Spread := 0.22 + 0.15 * d;
    p.Arm[j].Bend := 0.3 - 0.2 * d;
  End;
  p.HeadNod := 0.25 * d;
  c := NeutralPose;
  SetCarryArms(c, Fig, 0, Facing);
  For j := 0 To 1 Do
    p.Arm[j] := BlendPose(p, c, u).Arm[j];
  p.Reach := BlendPose(p, c, u).Reach;
  p.HandsFront := BlendPose(p, c, u).HandsFront;
  result := p;
End;

Function AnimPose(Kind: TAnimKind; Const Fig: TFigure; i, n: Integer; Facing: Double): TPose;
Begin
  Case Kind Of
    akWalk: result := WalkPose(Fig, AG_TWO_PI * i / n, false, Facing);
    akCarry: result := WalkPose(Fig, AG_TWO_PI * i / n, true, Facing);
    akKick: result := KickPose(Fig, i, n);
    akPunch: result := PunchPose(Fig, i, n);
    akPup: result := PupPose(Fig, i, n, Facing);
  Else
    result := NeutralPose;
  End;
End;

(* ---------------------------------------------------------------------------- *)
(* Effects                                                                       *)

Procedure Puff(C: TAGCanvas; Style: TAGStyle; x, y, r: Double; Col, Outline: TAGColor);
Begin
  If r < 0.6 Then exit;
  C.Circle(x, y, r);
  DrawShapeOutlined(C, Style, Col, Outline, 0.8);
End;

Procedure Star(C: TAGCanvas; Style: TAGStyle; x, y, r, Rot: Double; Points: Integer; Inner: Double; Col, Outline: TAGColor);
Var
  pts: Array Of Double;
  k: Integer;
  a, rr: Double;
Begin
  If r < 0.6 Then exit;
  setlength(pts, 4 * Points);
  For k := 0 To 2 * Points - 1 Do Begin
    a := Rot + AG_PI * k / Points - AG_HALF_PI;
    If odd(k) Then rr := r * Inner Else rr := r;
    pts[2 * k] := x + rr * AGCos(a);
    pts[2 * k + 1] := y + rr * AGSin(a);
  End;
  C.Polygon(pts);
  DrawShapeOutlined(C, Style, Col, Outline, 0.8);
End;

(* Draws the figure scaled around the point (px, py) *)
Procedure DrawScaled(C: TAGCanvas; Style: TAGStyle; Const Fig: TFigure; Const Pose: TPose; Facing, sx, sy, px, py, Rot: Double);
Begin
  If (sx < 0.02) Or (sy < 0.02) Then exit;
  C.Push;
  C.Translate(px, py);
  C.Rotate(Rot);
  C.Scale(sx, sy);
  C.Translate(-px, -py);
  DrawFigure(C, Style, Fig, Pose, Facing, FootX, FootY);
  C.Pop;
End;

(* ---------------------------------------------------------------------------- *)

(*
 * Pixel style: the inner outlines are thin, the silhouette gets a closed
 * outline of one big pixel (2x2) around every cell.
 *)
Procedure PixelOutline(Img: TAGImage; w, h, Scale: Integer; Col: TAGColor; Selective: Boolean);
Var
  src: TAGImage;
  cx, cy, bx, by, x0, y0, i, j: Integer;
  c: TAGColor;

  Function Solid(x, y: Integer): Boolean; // block at (x, y) relative to the cell
  Begin
    If (x < 0) Or (y < 0) Or (x >= w) Or (y >= h) Then exit(false);
    result := src.GetPixel(x0 + x, y0 + y).a >= 128;
  End;

  (* Outline color of the block (bx, by): on the lit side the darker part color *)
  Function EdgeCol: TAGColor;
  Var
    n: TAGColor;
  Begin
    result := Col;
    If Not Selective Then exit;
    If Solid(bx + Scale, by) Then n := src.GetPixel(x0 + bx + Scale, y0 + by)
    Else If Solid(bx, by + Scale) Then n := src.GetPixel(x0 + bx, y0 + by + Scale)
    Else exit; // shadow side (bottom, right): the dark outline
    If AGSameColor(n, Col) Then exit;
    result := AGWithAlpha(AGShade(n, 0.5), 255);
    If IsGreenDominant(n) Then Begin
      If Not IsGreenDominant(result) Then result := Col;
    End
    Else
      result := NotGreen(result);
  End;

Begin
  src := Img.Clone;
  Try
    For cy := 0 To Img.Height Div h - 1 Do
      For cx := 0 To Img.Width Div w - 1 Do Begin
        x0 := cx * w;
        y0 := cy * h;
        by := 0;
        While by < h Do Begin
          bx := 0;
          While bx < w Do Begin
            If (Not Solid(bx, by)) And (Solid(bx - Scale, by) Or Solid(bx + Scale, by) Or Solid(bx, by - Scale) Or Solid(bx, by + Scale)) Then Begin
              c := EdgeCol;
              For j := 0 To Scale - 1 Do
                For i := 0 To Scale - 1 Do
                  If (bx + i < w) And (by + j < h) Then Img.SetPixel(x0 + bx + i, y0 + by + j, c);
            End;
            inc(bx, Scale);
          End;
          inc(by, Scale);
        End;
      End;
  Finally
    src.Free;
  End;
End;

Procedure GenerateFigure(Ctx: TAGContext; Output: TAGOutput; Const Area: TAGArea; Const Dir: String; Hunted: Boolean);
Var
  Fig: TFigure;
  Style: TAGStyle;
  Outline: TAGColor;

  Procedure SaveSheet(Const RelPath: String; Img: TAGImage; w, h: Integer);
  Var
    info: TAGStyleInfo;
  Begin
    info := GetStyleInfo(Style);
    If info.PixelScale > 1 Then PixelOutline(Img, w, h, info.PixelScale, Outline, Style = asPixel);
    Output.SaveImage(RelPath, Img, smKeyed);
  End;

  Procedure DirSheet(Const Name: String; Kind: TAnimKind; FramesPerDir, w, h, ax, ay: Integer; Const RowFacing: Array Of Double;
    Scale: Double = 1.0);
  Var
    img: TAGImage;
    Facings: Array[0..3] Of Double;
    k: Integer;

    Procedure DrawFrame(C: TAGCanvas; Frame: Integer);
    Var
      row, i: Integer;
    Begin
      row := Frame Div FramesPerDir;
      i := Frame Mod FramesPerDir;
      If Scale <> 1 Then Begin // smaller around the ground point
        C.Push;
        C.Translate(ax, ay);
        C.Scale(Scale, Scale);
        C.Translate(-ax, -ay);
      End;
      DrawFigure(C, Style, Fig, AnimPose(Kind, Fig, i, FramesPerDir, Facings[row]), Facings[row], ax, ay);
      If Scale <> 1 Then C.Pop;
    End;

  Begin
    For k := 0 To 3 Do
      Facings[k] := RowFacing[k];
    img := RenderSheet(Ctx, FramesPerDir, 4, w, h, 4 * FramesPerDir, @DrawFrame);
    Try
      SaveSheet(Dir + Name, img, w, h);
    Finally
      img.Free;
    End;
  End;

  Procedure StandSheet;
  Var
    img: TAGImage;

    Procedure DrawFrame(C: TAGCanvas; Frame: Integer);
    Const
      F: Array[0..3] Of Double = (FacingR, FacingUp, FacingDown, FacingL);
    Begin
      DrawFigure(C, Style, Fig, NeutralPose, F[Frame], FootX, FootY);
    End;

  Begin
    img := RenderSheet(Ctx, 2, 2, CellW, CellH, 4, @DrawFrame);
    Try
      SaveSheet(Dir + 'stand.png', img, CellW, CellH);
    Finally
      img.Free;
    End;
  End;

  Procedure ShadowImage;
  Var
    img: TAGImage;
    c: TAGCanvas;
  Begin
    img := TAGImage.Create(29, 17);
    c := TAGCanvas.Create(img);
    Try
      c.Ellipse(14.5, 8.5, 14.3, 8.3);
      If Style = asSoft Then // soft contact shadow, fades out to the edge
        c.FillPaint(c.Radial(14.5, 8.5, 14.3, [AGRGBA(0, 0, 0, 255), AGRGBA(0, 0, 0, 230), AGRGBA(0, 0, 0, 0)]))
      Else
        c.Fill(AGRGBA(0, 0, 0, 255));
      Output.SaveImage(Dir + 'shadow.png', img, smAlpha);
    Finally
      c.Free;
      img.Free;
    End;
  End;

  (* One clip of the die / idle / locked_in folders, front view *)
  Procedure DrawClip(C: TAGCanvas; Kind: TClipKind; i, n: Integer);
  Var
    t, e, e2, x, y, r, a, vx, vy, tt, sx: Double;
    p: TPose;
    f: TFigure;
    k, j, s0: Integer;
    S: Double;
    m: TAGMatrix;
    gray: TAGColor;
    Facing: Double;

    Function R_(Const Key: String; Index: Integer): Double;
    Begin
      result := Area.Rnd(Key, Index);
    End;

  Begin
    t := i / AGMax(1, n - 1);
    S := Fig.Scale;
    p := NeutralPose;
    Facing := FacingDown;
    Case Kind Of
      (* ------------------------------------------------ die *)
      ckDieSmoke: Begin
          If i < 6 Then Begin
            p.Arm[0].Spread := 1.3;
            p.Arm[1].Spread := 1.3;
            p.Arm[0].Swing := 0.3;
            p.Arm[1].Swing := 0.3;
            p.RootU := 2 * S * ord(odd(i));
            f := Fig;
            If odd(i) Then f := TintFigure(Fig, AGWhite, 0.7, true);
            DrawFigure(C, Style, f, p, Facing, FootX, FootY);
          End
          Else If i < 15 Then Begin
            f := TintFigure(Fig, AGRGB(40, 36, 40), 0.85, false);
            e := Ease(i, 6, 15);
            p.EyeOpen := 1 - 0.8 * e;
            DrawScaled(C, Style, f, p, Facing, 1 - 0.75 * e, 1 - 0.85 * e, FootX, FootY, 0);
          End;
          For k := 0 To 8 Do Begin
            s0 := 4 + k;
            If i < s0 Then Continue;
            tt := i - s0;
            x := FootX + (R_('smokex', k) - 0.5) * 34 * S + (R_('smokedx', k) - 0.5) * tt * 1.2 * S;
            y := FootY - 8 * S - R_('smokey', k) * 30 * S - tt * 2.4 * S;
            r := (3 + tt * 1.3) * S * AGClamp((n - 1 - i) / 6, 0, 1);
            gray := NotGreen(AGRGB(120 + AGRound(R_('smokec', k) * 70), 118 + AGRound(R_('smokec', k) * 70), 128 + AGRound(R_('smokec', k) * 70)));
            Puff(C, Style, x, y, r, gray, AGShade(gray, 0.55));
          End;
        End;
      ckDieMelt: Begin
          e := Ease(t, 0, 0.55);
          // puddle under the figure
          r := (6 + 13 * e) * S;
          If i > 18 Then r := r * (1 - Ease(i, 18, n - 1));
          If r > 0.8 Then Begin
            C.Ellipse(FootX, FootY - 1, r, r * 0.33);
            DrawShapeOutlined(C, Style, Fig.Col.Suit, Outline, 1);
          End;
          If i <= 15 Then Begin
            For k := 0 To 1 Do Begin
              p.Arm[k].Spread := 0.35 + 0.5 * e;
              p.Arm[k].Swing := 0;
              p.Arm[k].Bend := 0.1;
            End;
            p.HeadNod := 0.2 * e;
            p.EyeOpen := 1 - 0.6 * e;
            DrawScaled(C, Style, Fig, p, Facing, 1 + 0.45 * e, 1 - 0.62 * e, FootX, FootY, 0);
          End
          Else If i < n - 3 Then Begin
            // two eyes swimming in the puddle
            For k := 0 To 1 Do Begin
              x := FootX + (2 * k - 1) * 4.5 * S * (1 - Ease(i, 18, n - 1)) * 1.4;
              Puff(C, Style, x, FootY - 3 * S, (2.6 * (1 - 0.7 * Ease(i, 18, n - 1)) + 0.6) * S, Fig.Col.Eye, Outline);
            End;
          End;
        End;
      ckDieGhost: Begin
          e := Ease(t, 0, 0.3);
          p.EyeOpen := 1 - Ease(t, 0.15, 0.3);
          p.Arm[0].Spread := 0.22 + 0.9 * e;
          p.Arm[1].Spread := 0.22 + 0.9 * e;
          If (i < 12) Or ((i < 22) And Not odd(i)) Then
          Begin
            C.Push;
            C.Translate(-14 * S * e, -17 * S * e); // the lying figure stays inside the cell
            DrawScaled(C, Style, Fig, p, Facing, 1 - 0.1 * e, 1 - 0.1 * e, FootX - 6 * S, FootY, 1.45 * e);
            C.Pop;
          End;
          If i >= 8 Then Begin
            tt := i - 8;
            x := FootX + 7 * S * AGSin(tt * 0.6);
            y := FootY - 22 * S - tt * 3.4;
            sx := AGMin(1, tt / 4);
            gray := NotGreen(AGRGB(236, 238, 252));
            C.Push;
            C.Translate(x, y);
            C.Scale(1.5 * S * sx, 1.5 * S * sx);
            C.MoveTo(-9, 2);
            C.ArcTo(0, 0, 9, 10, AG_PI, AG_TWO_PI);
            C.LineTo(9, 12);
            For k := 0 To 5 Do
              C.QuadTo(7.5 - 3 * k, 12 + 4 * (1 - 2 * ord(odd(k))) + 1.5 * AGSin(tt + k), 6 - 3 * k, 12);
            C.ClosePath;
            DrawShapeOutlined(C, Style, gray, NotGreen(AGRGB(120, 130, 175)), 1);
            C.Ellipse(-3.3, -1, 1.6, 2.4);
            C.Ellipse(3.3, -1, 1.6, 2.4);
            C.Fill(NotGreen(AGRGB(40, 40, 70)));
            C.Pop;
          End;
        End;
      ckDieSpin: Begin
          a := AG_TWO_PI * 3.2 * t * t;
          p.Turn := a;
          e := Ease(t, 0.35, 1);
          For k := 0 To 1 Do Begin
            p.Arm[k].Spread := 0.3 + 1.0 * Ease(t, 0, 0.3);
            p.Arm[k].Bend := 0.1;
          End;
          p.EyeOpen := 0.3 + 0.7 * ord(i < 8);
          DrawScaled(C, Style, Fig, p, Facing, 1 - 0.3 * e, 1 - e, FootX, FootY + 2, 0);
          If i > 9 Then
            For k := 0 To 5 Do Begin
              tt := (i - 9) + k * 0.7;
              a := AG_TWO_PI * k / 6 + 0.3;
              r := (2 + tt * 0.35) * S * AGClamp((n - 1 - i) / 5, 0, 1);
              gray := NotGreen(AGRGB(170, 150, 120));
              Puff(C, Style, FootX + AGCos(a) * (10 + tt * 1.2) * S, FootY - 3 * S + AGSin(a) * (3 + tt * 0.4) * S, r, gray, AGShade(gray, 0.5));
            End;
        End;
      ckDieBurnt: Begin
          If i < 5 Then Begin
            p.Arm[0].Spread := 1.1;
            p.Arm[1].Spread := 1.1;
            f := Fig;
            If odd(i) Then f := TintFigure(Fig, AGRGB(255, 150, 40), 0.6, false);
            DrawFigure(C, Style, f, p, Facing, FootX, FootY);
          End
          Else Begin
            f := TintFigure(Fig, AGRGB(34, 30, 34), 0.9, false);
            p.Arm[0].Spread := 1.1;
            p.Arm[1].Spread := 1.1;
            p.EyeOpen := 1 - 0.9 * ord((i = 9) Or (i = 10));
            e := Lin(i, 12, 25); // crumbling from the top
            y := FootY - 64 * S + (64 * S + 2) * e;
            // heap
            r := (3 + 13 * e) * S;
            If i > 26 Then r := r * (1 - Lin(i, 26, n - 1) * 0.6);
            If i >= 12 Then Begin
              C.Ellipse(FootX, FootY - 1, r, r * 0.38);
              DrawShapeOutlined(C, Style, f.Col.Suit, Outline, 0.8);
            End;
            If e < 1 Then Begin
              m := C.GetMatrix;
              C.SetClip(0, AGRound(y * m.d + m.f), C.Image.Width, C.Image.Height);
              DrawFigure(C, Style, f, p, Facing, FootX, FootY);
              C.ResetClip;
            End;
            // falling ashes
            If (i >= 12) Then
              For k := 0 To 13 Do Begin
                tt := i - (12 + R_('ash', k) * 13);
                If (tt < 0) Or (tt > 6) Then Continue;
                x := FootX + (R_('ashx', k) - 0.5) * 26 * S;
                y := FootY - 60 * S + R_('ash', k) * 60 * S + tt * tt * 1.4;
                If y > FootY - 2 Then Continue;
                Puff(C, Style, x, y, 1.6, f.Col.Suit, Outline);
              End;
          End;
        End;
      ckDiePop: Begin
          If i < 15 Then Begin
            e := Ease(i, 0, 14);
            For k := 0 To 1 Do Begin
              p.Arm[k].Spread := 0.3 + 1.0 * e;
              p.Leg[k].Spread := 0.04 + 0.4 * e;
              p.Arm[k].Bend := 0.1;
            End;
            p.RootU := 8 * S * e;
            p.Roll := 0.08 * AGSin(i * 1.3) * e;
            p.EyeOpen := 1;
            DrawScaled(C, Style, Fig, p, Facing, 1 + 0.4 * e, 1 + 0.3 * e, FootX, FootY - 30 * S, 0);
          End
          Else Begin
            tt := i - 15;
            If tt < 3 Then Begin
              Star(C, Style, FootX, FootY - 40 * S, (24 + tt * 5) * S, tt * 0.3, 8, 0.45, NotGreen(AGRGB(255, 250, 220)), NotGreen(AGRGB(230, 150, 40)));
              Star(C, Style, FootX, FootY - 40 * S, (12 + tt * 3) * S, tt * 0.3 + 0.2, 8, 0.5, NotGreen(AGRGB(255, 220, 80)), NotGreen(AGRGB(230, 150, 40)));
            End;
            For k := 0 To 17 Do Begin
              a := AG_TWO_PI * (k + R_('conf', k)) / 18;
              vx := AGCos(a) * (3 + 3 * R_('confv', k));
              vy := AGSin(a) * (3 + 3 * R_('confv', k)) - 1.5;
              x := FootX + vx * tt * 1.6 * S;
              y := FootY - 40 * S + (vy * tt * 1.6 + 0.45 * tt * tt) * S;
              If (k Mod 3) = 0 Then gray := Fig.Col.Accent Else gray := PlayerGreen(0.4 + 0.5 * R_('confc', k));
              C.Push;
              C.Translate(x, y);
              C.Rotate(tt * 0.7 + k);
              C.Rect(-1.8 * S, -1.2 * S, 3.6 * S, 2.4 * S);
              DrawShapeOutlined(C, Style, gray, Outline, 0.5);
              C.Pop;
            End;
          End;
        End;
      ckDieFlat: Begin
          e := Ease(i, 0, 3);
          e2 := Ease(i, 19, n - 1);
          p.EyeOpen := 1 - 0.9 * ord(i >= 3);
          For k := 0 To 1 Do Begin
            p.Arm[k].Spread := 0.22 + 1.2 * e;
            p.Leg[k].Spread := 0.04 + 0.5 * e;
          End;
          DrawScaled(C, Style, Fig, p, Facing, (1 + 0.25 * e) * (1 - e2), 1 - 0.6 * e, FootX, FootY, 0);
          If (i >= 3) And (i < n - 2) Then
            For k := 0 To 2 Do Begin
              a := i * 0.55 + AG_TWO_PI * k / 3;
              Star(C, Style, FootX + 15 * S * AGCos(a), FootY - 30 * S + 4 * S * AGSin(a), 3.6 * S, i * 0.4, 5, 0.45,
                NotGreen(AGRGB(255, 220, 60)), NotGreen(AGRGB(160, 100, 20)));
            End;
        End;
      ckDieBeam: Begin
          r := 15 * S * Ease(i, 0, 4) * (1 - Ease(i, 16, n - 1));
          If r > 0.6 Then Begin
            C.Rect(FootX - r, 0, 2 * r, FootY + 2);
            C.Fill(NotGreen(AGRGB(255, 248, 200)));
            C.Rect(FootX - r * 0.45, 0, r * 0.9, FootY + 2);
            C.Fill(AGWhite);
          End;
          e := Ease(i, 4, 16);
          p.Arm[0].Spread := 0.1;
          p.Arm[1].Spread := 0.1;
          p.Arm[0].Swing := AG_PI * e;
          p.Arm[1].Swing := AG_PI * e;
          p.EyeOpen := 1 - 0.8 * e;
          If i < 16 Then Begin
            C.Push;
            C.Translate(0, -34 * S * e * e);
            DrawScaled(C, Style, Fig, p, Facing, 1 - 0.7 * e, 1 + 0.4 * e, FootX, FootY, 0);
            C.Pop;
          End;
          If i >= 6 Then
            For k := 0 To 4 Do Begin
              tt := i - 6 - k * 1.5;
              If tt < 0 Then Continue;
              Star(C, Style, FootX + (R_('beamx', k) - 0.5) * 34 * S, FootY - (10 + R_('beamy', k) * 60 + tt * 2) * S,
                3.5 * S * AGClamp((n - 1 - i) / 5, 0, 1), 0, 4, 0.35, AGWhite, NotGreen(AGRGB(200, 170, 60)));
            End;
        End;
      (* ------------------------------------------------ idle *)
      ckIdleWave: Begin
          e := Ease(i, 0, 8) * (1 - Ease(i, n - 9, n - 1));
          p.Arm[0].Spread := 0.22 + 2.0 * e;
          p.Arm[0].Swing := 0.2 * e;
          p.Arm[0].Bend := 0.3 * (1 - e);
          p.Arm[0].BendSpread := e * (0.25 + 0.55 * AGSin((i - 8) * AG_TWO_PI / 8));
          p.HeadRoll := 0.12 * e;
          p.Roll := -0.05 * e;
          p.EyeOpen := 1 - 0.85 * ord((i = 20) Or (i = 21));
          DrawFigure(C, Style, Fig, p, Facing, FootX, FootY);
        End;
      ckIdleStretch: Begin
          e := Ease(i, 0, 12) * (1 - Ease(i, 24, n - 1));
          For k := 0 To 1 Do Begin
            p.Arm[k].Spread := 0.22 + 2.0 * e;
            p.Arm[k].Bend := 0.3 * (1 - e);
            p.Arm[k].BendSpread := -0.3 * e;
          End;
          p.RootU := 2 * S * e;
          p.Lean := -0.12 * e;
          p.HeadNod := -0.15 * e;
          p.EyeOpen := 1 - 0.9 * e;
          DrawFigure(C, Style, Fig, p, Facing, FootX, FootY);
        End;
      ckIdleLook: Begin
          a := AGSin(AG_TWO_PI * t);
          p.HeadTurn := 0.7 * a;
          p.LookS := AGClamp(1.6 * a, -1, 1);
          p.Lean := 0.03;
          // tap with the right foot
          e := AGMax(0, AGSin(AG_TWO_PI * 6 * t));
          p.Leg[0].Swing := 0.25 + 0.2 * e;
          p.Leg[0].Bend := 0.25 + 0.25 * e;
          p.RootU := -0.4 * S;
          p.Arm[0].Spread := 0.45;
          p.Arm[1].Spread := 0.45;
          p.Arm[0].Swing := -0.25;
          p.Arm[1].Swing := -0.25;
          p.Arm[0].Bend := 1.4;
          p.Arm[1].Bend := 1.4;
          p.Arm[0].BendSpread := -1.2;
          p.Arm[1].BendSpread := -1.2;
          DrawFigure(C, Style, Fig, p, Facing, FootX, FootY);
        End;
      ckIdleHover: Begin
          e := Ease(i, 0, 10) * (1 - Ease(i, n - 11, n - 1));
          For k := 0 To 1 Do Begin
            p.Leg[k].Swing := 1.3 * e;
            p.Leg[k].Spread := 0.04 + 0.75 * e;
            p.Leg[k].Bend := 2.3 * e;
            p.Leg[k].BendSpread := -1.1 * e;
            p.Arm[k].Spread := 0.22 + 0.6 * e;
            p.Arm[k].Swing := 0.4 * e;
            p.Arm[k].Bend := 0.3 + 0.9 * e;
          End;
          p.RootU := e * S * (10 + 2.5 * AGSin(AG_TWO_PI * 2 * t));
          p.EyeOpen := 1 - 0.92 * e;
          // little ground shadow
          r := (9 - 3 * e) * S;
          C.Ellipse(FootX, FootY, r, r * 0.3);
          C.Fill(NotGreen(AGRGB(40, 40, 50)));
          DrawFigure(C, Style, Fig, p, Facing, FootX, FootY);
        End;
      (* ------------------------------------------------ locked in *)
      ckLockPanic: Begin
          a := AGSin(AG_TWO_PI * 2 * t);
          p.HeadTurn := 0.6 * a;
          p.LookS := AGClamp(1.5 * a, -1, 1);
          For k := 0 To 1 Do Begin
            p.Arm[k].Swing := 0.4;
            p.Arm[k].Spread := 1.35;
            p.Arm[k].Bend := 1.9;
            p.Arm[k].BendSpread := -0.5;
          End;
          p.RootS := 0.8 * S * (2 * ord(odd(i)) - 1);
          DrawFigure(C, Style, Fig, p, Facing, FootX, FootY);
        End;
      ckLockHop: Begin
          a := AGAbs(AGSin(AG_TWO_PI * t)); // two hops
          e := AGMax(0, 1 - a * 3) * ord(i > 0); // landing
          p := CrouchPose(0.25 * e);
          p.RootU := (9 * a - 2 * e) * S;
          For k := 0 To 1 Do Begin
            p.Arm[k].Spread := 0.3 + 2.2 * a;
            p.Arm[k].Bend := 0.2;
          End;
          p.EyeOpen := 1;
          DrawFigure(C, Style, Fig, p, Facing, FootX, FootY);
        End;
      ckLockCower: Begin
          e := Ease(i, 0, 5);
          p := CrouchPose(0.7 * e);
          p.RootU := -Fig.LegLen * 0.25 * e;
          For k := 0 To 1 Do Begin
            p.Arm[k].Swing := AG_PI - 0.5;
            p.Arm[k].Spread := 0.22 + 0.55 * e;
            p.Arm[k].Bend := 0.6 * e;
            p.Arm[k].BendSpread := -1.1 * e;
            p.Arm[k] := BlendPose(NeutralPose, p, e).Arm[k];
          End;
          p.HeadNod := 0.3 * e;
          p.Roll := 0.04 * e * (2 * ord(odd(i)) - 1);
          p.EyeOpen := 1 - 0.9 * e;
          DrawFigure(C, Style, Fig, p, Facing, FootX, FootY);
        End;
      ckLockPush: Begin
          j := i Mod 16;
          If i < 16 Then Facing := FacingR Else Facing := FacingL;
          e := Ease(j, 0, 3) * (1 - Ease(j, 12, 15));
          a := AGSin(AG_TWO_PI * 2 * j / 16);
          p.Lean := (0.25 + 0.08 * a) * e;
          For k := 0 To 1 Do Begin
            p.Arm[k].Swing := (1.45 + 0.1 * k) * e;
            p.Arm[k].Bend := 0.3 * (1 - e) + (0.25 - 0.2 * a) * e;
            p.Arm[k].Spread := 0.2;
          End;
          p.Leg[0].Swing := 0.35 * e;
          p.Leg[0].Bend := 0.3 * e;
          p.Leg[1].Swing := -0.45 * e;
          p.RootF := -2 * S * e;
          DrawFigure(C, Style, Fig, p, Facing, FootX, FootY);
        End;
    End;
  End;

  Procedure ClipSheet(Const SubDir: String; Const Kinds: Array Of TClipKind; Const Names: Array Of String);
  Var
    lines: Array Of String;
    k, cols, rows, n: Integer;
    img: TAGImage;
    Kind: TClipKind;

    Procedure DrawFrame(C: TAGCanvas; Frame: Integer);
    Begin
      DrawClip(C, Kind, Frame, n);
    End;

  Begin
    setlength(lines, length(Kinds) + 1);
    lines[0] := FilesHeader;
    For k := 0 To high(Kinds) Do Begin
      Kind := Kinds[k];
      n := ClipFrames[Kind];
      cols := 8;
      If n < cols Then cols := n;
      rows := (n + cols - 1) Div cols;
      img := RenderSheet(Ctx, cols, rows, CellW, CellH, n, @DrawFrame);
      Try
        SaveSheet(Dir + SubDir + '/' + Names[k] + '.png', img, CellW, CellH);
      Finally
        img.Free;
      End;
      lines[k + 1] := format('%s.png;50;%d;%d;%d;%d;%d;%d;%d', [Names[k], cols, rows, CellW, CellH, n, FootX, FootY]);
    End;
    Output.SaveText(Dir + SubDir + '/files.txt', lines);
  End;

Begin
  Style := Ctx.Style;
  Fig := MakeFigure(Area, Hunted);
  If IsCoarse(Style) Then Fig.OutlineScale := Fig.OutlineScale * 0.6; // inner lines, the silhouette gets its outline in SaveSheet
  If Style = asRetro Then Fig.OutlineScale := 0; // no room for inner lines, the parts are told apart by color and shading
  Outline := Fig.Col.Outline;
  If GetStyleInfo(Style).Palette Then Outline := AGRetroColor(Outline);
  // rows of the sheets, see the ranges in client/uatomic.pas (right, up, left, down)
  DirSheet('walk.png', akWalk, 15, CellW, CellH, FootX, FootY, [FacingR, FacingUp, FacingDown, FacingL]);
  DirSheet('bwalk.png', akCarry, 15, CellW, CellH, FootX, FootY, [FacingR, FacingUp, FacingDown, FacingL]);
  StandSheet;
  // the original kick figure is about 56 pixel high
  DirSheet('kick.png', akKick, 10, 50, 69, 25, 60, [FacingUp, FacingR, FacingDown, FacingL], KickScale[Style]);
  DirSheet('punbomb.png', akPunch, 10, CellW, CellH, FootX, FootY, [FacingDown, FacingUp, FacingL, FacingR]);
  DirSheet('pupbomb.png', akPup, 11, CellW, CellH, FootX, FootY, [FacingL, FacingR, FacingDown, FacingUp]);
  ShadowImage;
  ClipSheet('die', [ckDieSmoke, ckDieMelt, ckDieGhost, ckDieSpin, ckDieBurnt, ckDiePop, ckDieFlat, ckDieBeam],
    ['smoke', 'melt', 'ghost', 'spin', 'burnt', 'pop', 'flat', 'beam']);
  ClipSheet('idle', [ckIdleWave, ckIdleStretch, ckIdleLook, ckIdleHover], ['wave', 'stretch', 'look', 'hover']);
  ClipSheet('locked_in', [ckLockPanic, ckLockHop, ckLockCower, ckLockPush], ['panic', 'hop', 'cower', 'push']);
End;

Procedure GenerateAtomic(Ctx: TAGContext; Output: TAGOutput);
Var
  base: TAGArea;
  j: Integer;
Begin
  base := Ctx.Area('atomic');
  GenerateFigure(Ctx, Output, base, 'atomic/', false);
  GenerateFigure(Ctx, Output, base, 'atomic/hunted/', true);
  If Ctx.PerPlayer Then
    For j := 0 To 9 Do
      GenerateFigure(Ctx, Output, base.Sub('p' + IntToStr(j)), 'atomic/p' + IntToStr(j) + '/', false);
End;

Function ExpectedAtomic(Ctx: TAGContext): Integer;
Begin
  result := 2 * FilesPerSet; // atomic/ and atomic/hunted/
  If Ctx.PerPlayer Then result := result + 10 * FilesPerSet;
End;

Initialization
  RegisterGenerator('atomic', 'Player figure (atomic/*: walk, stand, kick, die, idle, ...)', @ExpectedAtomic, @GenerateAtomic);

End.
