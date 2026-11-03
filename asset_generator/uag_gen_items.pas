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
 * Power ups, bombs, flames and the small in-field objects (agent B).
 *
 * Generators:
 *   powerups: res/pow*.png, 14 opaque icons 40x36 on a beveled tile. The tile
 *             color tells the group (blue = stats, teal = abilities, red /
 *             purple = bad, violet = random), the seed only shifts it a bit.
 *   bomb:     atomic/bomb.png, bomb_dud.png, bomb_trigger.png, bomb_wobble.png.
 *             The body is drawn in PlayerGreen on purpose: these sheets are
 *             loaded with LoadColorTabledTexture, so every player gets bombs in
 *             his own color (like the original). Fuse, cap, spark, panel are
 *             NotGreen and keep their color.
 *   flame:    atomic/flame.png, 5 x 9 cells of 41 x 37 (cross, ends up / left /
 *             down / right, middles vertical / horizontal). Also recolored with
 *             the player color: layers in PlayerGreen with a white core. The
 *             edge waves are periodic in 40 px (horizontal) and 36 px
 *             (vertical) and in the 5 frames, so neighbouring cells and the loop
 *             connect without seams.
 *   objects:  res/hole.png, arrows.ani, conveyor.ani, tramp.ani, infinity.png,
 *             playerdead.png, hurry.png.
 *)
Unit uag_gen_items;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Uses uag_canvas, uag_context;

(*
 * Draws frame 0 of bomb.png (cell 40 x 37) into C with the current
 * transformation, e.g. for preview.png.
 *)
Procedure DrawBombPreview(Ctx: TAGContext; C: TAGCanvas);

Implementation

Uses SysUtils, uag_math, uag_random, uag_image, uag_render, uag_output, uag_registry;

Const
  OutlineDark: TAGColor = (r: 26; g: 22; b: 32; a: 255);

  (* Stroke width that does not break up in pixel style (half resolution, no anti aliasing) *)

Function SW(St: TAGStyle; w: Double): Double;
Begin
  If IsCoarse(St) Then
    result := AGMax(w, GetStyleInfo(St).MinPen)
  Else
    result := w;
End;

Procedure Star(C: TAGCanvas; cx, cy, ro, ri: Double; n: Integer; Rot: Double);
Var
  i: Integer;
  a, r: Double;
Begin
  For i := 0 To 2 * n - 1 Do Begin
    a := Rot + AG_PI * i / n;
    If odd(i) Then r := ri Else r := ro;
    If i = 0 Then
      C.MoveTo(cx + r * AGCos(a), cy + r * AGSin(a))
    Else
      C.LineTo(cx + r * AGCos(a), cy + r * AGSin(a));
  End;
  C.ClosePath;
End;

(*
 * Open polylines as point lists (x0, y0, x1, y1, ...). The canvas closes an
 * open sub path when it is stroked a second time with Keep = true, so lines
 * with a border are built twice from the list.
 *)

Type
  TPts = Array Of Double;

Procedure PAdd(Var P: TPts; x, y: Double);
Begin
  setlength(P, length(P) + 2);
  P[high(P) - 1] := x;
  P[high(P)] := y;
End;

Procedure PQuad(Var P: TPts; x0, y0, cx, cy, x1, y1: Double);
Var
  i: Integer;
  t, mt: Double;
Begin
  For i := 0 To 16 Do Begin
    t := i / 16;
    mt := 1 - t;
    PAdd(P, mt * mt * x0 + 2 * mt * t * cx + t * t * x1, mt * mt * y0 + 2 * mt * t * cy + t * t * y1);
  End;
End;

Procedure PCubic(Var P: TPts; x0, y0, c1x, c1y, c2x, c2y, x1, y1: Double);
Var
  i: Integer;
  t, mt: Double;
Begin
  For i := 0 To 20 Do Begin
    t := i / 20;
    mt := 1 - t;
    PAdd(P, mt * mt * mt * x0 + 3 * mt * mt * t * c1x + 3 * mt * t * t * c2x + t * t * t * x1,
      mt * mt * mt * y0 + 3 * mt * mt * t * c1y + 3 * mt * t * t * c2y + t * t * t * y1);
  End;
End;

Procedure PArc(Var P: TPts; cx, cy, rx, ry, a0, a1: Double);
Var
  i: Integer;
  a: Double;
Begin
  For i := 0 To 24 Do Begin
    a := a0 + (a1 - a0) * i / 24;
    PAdd(P, cx + rx * AGCos(a), cy + ry * AGSin(a));
  End;
End;

Procedure StrokePts(C: TAGCanvas; Const P: TPts; w: Double; Col: TAGColor);
Var
  i: Integer;
Begin
  If length(P) < 4 Then exit;
  C.MoveTo(P[0], P[1]);
  i := 2;
  While i + 1 <= high(P) Do Begin
    C.LineTo(P[i], P[i + 1]);
    inc(i, 2);
  End;
  C.Stroke(w, Col);
End;

(* A line with a dark border *)

Procedure OutlinedPts(C: TAGCanvas; St: TAGStyle; Const P: TPts; w: Double; Col, Border: TAGColor);
Var
  bw: Double;
Begin
  If IsCoarse(St) Then bw := GetStyleInfo(St).MinPen Else bw := 1.6;
  StrokePts(C, P, SW(St, w) + bw, Border);
  StrokePts(C, P, SW(St, w), Col);
End;

(* Shiny spot on round things, the soft style already has a gradient *)

Procedure Shine(C: TAGCanvas; St: TAGStyle; cx, cy, rx, ry, Angle: Double; Col: TAGColor);
Begin
  C.Push;
  C.Translate(cx, cy);
  C.Rotate(Angle);
  If St = asSoft Then Begin
    C.Ellipse(0, 0, rx * 0.6, ry * 0.6);
    C.Fill(AGWhite);
  End
  Else Begin
    C.Ellipse(0, 0, rx, ry);
    C.Fill(Col);
  End;
  C.Pop;
End;

(******************************************************************************)
(* Icons for the power ups                                                    *)
(******************************************************************************)

(* Small dark bomb, r = radius of the ball, fuse to the top right *)

Procedure MiniBomb(C: TAGCanvas; St: TAGStyle; cx, cy, r: Double; Body: TAGColor; Spark: Boolean);
Var
  a, capx, capy: Double;
  pts: TPts;
Begin
  a := AGDeg(-55);
  capx := cx + r * AGCos(a);
  capy := cy + r * AGSin(a);
  // fuse
  pts := Nil;
  PQuad(pts, capx + 0.15 * r, capy - 0.2 * r, capx + 0.25 * r, capy - 0.75 * r, capx + 0.7 * r, capy - 0.6 * r);
  OutlinedPts(C, St, pts, AGMax(1.0, 0.16 * r), AGRGB(196, 160, 104), OutlineDark);
  // cap
  C.Push;
  C.Translate(capx, capy);
  C.Rotate(a + AG_HALF_PI);
  C.Rect(-0.3 * r, -0.25 * r, 0.6 * r, 0.45 * r);
  DrawShapeOutlined(C, St, AGRGB(160, 162, 172), OutlineDark, 0.7);
  C.Pop;
  // ball
  C.Circle(cx, cy, r);
  DrawShapeOutlined(C, St, Body, OutlineDark, 0.8);
  Shine(C, St, cx - 0.38 * r, cy - 0.38 * r, 0.3 * r, 0.18 * r, AGDeg(-45), AGShade(Body, 1.7));
  If Spark Then Begin
    Star(C, capx + 0.7 * r, capy - 0.6 * r, 0.55 * r, 0.22 * r, 5, AGDeg(-10));
    DrawShapeOutlined(C, St, AGRGB(255, 214, 64), AGRGB(170, 60, 20), 0.6);
    C.Circle(capx + 0.7 * r, capy - 0.6 * r, 0.16 * r);
    C.Fill(AGWhite);
  End;
End;

(* Flame in a box of -1..1 (x) and -1 (tip) .. 1 (bottom) *)

Procedure FlamePath(C: TAGCanvas);
Begin
  C.MoveTo(0.08, -1);
  C.CubicTo(0.4, -0.55, 1.0, -0.4, 0.92, 0.32);
  C.CubicTo(0.86, 0.8, 0.45, 1.0, 0, 1.0);
  C.CubicTo(-0.45, 1.0, -0.92, 0.75, -0.9, 0.28);
  C.CubicTo(-0.88, -0.12, -0.6, -0.3, -0.5, -0.66);
  C.CubicTo(-0.32, -0.42, -0.2, -0.3, -0.12, -0.36);
  C.CubicTo(-0.05, -0.62, -0.1, -0.82, 0.08, -1);
  C.ClosePath;
End;

Procedure FlameIcon(C: TAGCanvas; St: TAGStyle; cx, cy, w, h: Double; Const Cols: Array Of TAGColor);
Var
  i: Integer;
  s: Double;
Begin
  For i := 0 To high(Cols) Do Begin
    s := 1 - 0.3 * i;
    C.Push;
    C.Translate(cx, cy + h * (1 - s) * 0.85);
    C.Scale(w * s, h * s);
    FlamePath(C);
    C.Pop;
    If i = 0 Then
      DrawShapeOutlined(C, St, Cols[i], OutlineDark, 0.9)
    Else
      DrawShape(C, St, Cols[i], 0);
  End;
End;

Procedure Skull(C: TAGCanvas; St: TAGStyle; cx, cy: Double; Bone: TAGColor);
Var
  dark: TAGColor;
  i: Integer;
Begin
  dark := AGRGB(30, 20, 26);
  C.RoundRect(cx - 5.5, cy + 2, 11, 7, 2.2);
  DrawShapeOutlined(C, St, AGShade(Bone, 0.88), OutlineDark, 0.9);
  C.Ellipse(cx, cy - 2.2, 9.2, 8.2);
  DrawShapeOutlined(C, St, Bone, OutlineDark, 0.9);
  // eyes and nose
  C.Ellipse(cx - 3.7, cy - 1.6, 2.6, 2.9);
  C.Ellipse(cx + 3.7, cy - 1.6, 2.6, 2.9);
  C.Fill(dark);
  C.Polygon([cx, cy + 1.6, cx - 1.4, cy + 4, cx + 1.4, cy + 4]);
  C.Fill(dark);
  // teeth
  For i := -1 To 1 Do Begin
    C.MoveTo(cx + 2.4 * i, cy + 6);
    C.LineTo(cx + 2.4 * i, cy + 8.6);
    C.Stroke(SW(St, 0.9), dark);
  End;
End;

Procedure Bone(C: TAGCanvas; St: TAGStyle; x0, y0, x1, y1: Double; Col: TAGColor);
Var
  dx, dy, l, nx, ny: Double;
Begin
  dx := x1 - x0;
  dy := y1 - y0;
  l := AGSqrt(dx * dx + dy * dy);
  nx := -dy / l * 1.9;
  ny := dx / l * 1.9;
  C.Circle(x0 + nx, y0 + ny, 2.3);
  C.Circle(x0 - nx, y0 - ny, 2.3);
  C.Circle(x1 + nx, y1 + ny, 2.3);
  C.Circle(x1 - nx, y1 - ny, 2.3);
  DrawShapeOutlined(C, St, Col, OutlineDark, 0.8);
  C.Capsule(x0, y0, x1, y1, 1.8);
  DrawShape(C, St, Col, 0);
End;

Procedure BootPath(C: TAGCanvas);
Begin
  C.MoveTo(-7, -10);
  C.LineTo(2, -10);
  C.LineTo(2, -1.5);
  C.CubicTo(6, -1.5, 10.5, -0.5, 10.5, 4);
  C.LineTo(10.5, 7);
  C.LineTo(-7, 7);
  C.ClosePath;
End;

Procedure Boot(C: TAGCanvas; St: TAGStyle; Col: TAGColor);
Var
  i: Integer;
Begin
  C.Rect(-7.5, 6, 18.5, 3.2);
  DrawShapeOutlined(C, St, AGRGB(52, 46, 50), OutlineDark, 0.8);
  BootPath(C);
  DrawShapeOutlined(C, St, Col, OutlineDark, 0.9);
  C.Rect(-7.5, -11.5, 10, 3.2);
  DrawShapeOutlined(C, St, AGRGB(236, 232, 220), OutlineDark, 0.8);
  For i := 0 To 2 Do Begin
    C.MoveTo(-1, -5 + 3 * i);
    C.LineTo(3 + 1.5 * i, -4.5 + 3 * i);
    C.Stroke(SW(St, 0.9), AGShade(Col, 0.4));
  End;
End;

Procedure SpeedLines(C: TAGCanvas; St: TAGStyle; x, y, len: Double; Col: TAGColor);
Var
  i: Integer;
Begin
  For i := -1 To 1 Do Begin
    C.MoveTo(x - len + AGAbs(i) * 2, y + i * 4.5);
    C.LineTo(x, y + i * 4.5);
    C.Stroke(SW(St, 1.6), Col);
  End;
End;

(******************************************************************************)
(* powerups                                                                   *)
(******************************************************************************)

Procedure GeneratePowerups(Ctx: TAGContext; Output: TAGOutput);
Const
  Names: Array[0..13] Of String = ('powbomb', 'powflame', 'powdisea', 'powkick', 'powskate', 'powpunch',
    'powgrab', 'powspoog', 'powgold', 'powtrig', 'powjelly', 'powebola', 'powslow', 'powrand');
  // 0 = stats, 1 = abilities, 2 = bad, 3 = very bad, 4 = random
  Group: Array[0..13] Of Integer = (0, 0, 2, 1, 0, 1, 1, 1, 0, 1, 1, 3, 2, 4);
  GroupHue: Array[0..4] Of Double = (214, 176, 356, 292, 262);
  GroupVal: Array[0..4] Of Double = (0.66, 0.58, 0.55, 0.36, 0.6);
Var
  area: TAGArea;
  St: TAGStyle;
  dHue, sat, corner: Double;
  icon: Integer;
  img: TAGImage;
  i: Integer;

  Procedure Tile(C: TAGCanvas);
  Var
    base: TAGColor;
    g: Integer;
  Begin
    g := Group[icon];
    base := AGHSV(GroupHue[g] + dHue, sat, GroupVal[g]);
    C.Rect(-1, -1, 42, 38);
    C.Fill(AGShade(base, 0.32));
    C.RoundRect(2, 2, 37.5, 33.5, corner);
    C.Fill(AGShade(base, 0.55));
    C.RoundRect(1, 1, 37, 33, corner);
    C.Fill(AGShade(base, 1.45));
    C.RoundRect(3, 3, 34, 30, corner - 1);
    DrawShape(C, St, base, 0);
  End;

  Procedure DrawIcon(C: TAGCanvas; Frame: Integer);
  Var
    k: Integer;
    a: Double;
    pts: TPts;
  Begin
    Tile(C);
    Case icon Of
      0: Begin // extra bomb
          MiniBomb(C, St, 18, 21, 9.5, AGRGB(52, 54, 76), true);
          C.Rect(27, 21, 8, 2.6);
          C.Rect(29.7, 18.3, 2.6, 8);
          DrawShapeOutlined(C, St, AGWhite, OutlineDark, 0.6);
        End;
      1: Begin // longer flame
          FlameIcon(C, St, 20, 19.5, 10, 13, [AGRGB(226, 60, 24), AGRGB(255, 152, 32), AGRGB(255, 236, 120)]);
        End;
      2: Begin // disease
          Skull(C, St, 20, 17, AGRGB(214, 222, 170));
          For k := 0 To 2 Do Begin
            C.Circle(8 + 12 * k, 29 - 2 * (k Mod 2), 1.6 + 0.4 * (k Mod 2));
            C.Fill(AGRGB(170, 200, 90));
          End;
        End;
      3: Begin // kick
          C.Push;
          C.Translate(15, 20);
          C.Rotate(AGDeg(-22));
          Boot(C, St, AGRGB(150, 92, 48));
          C.Pop;
          MiniBomb(C, St, 30, 22, 5, AGRGB(52, 54, 76), false);
          For k := 0 To 2 Do Begin
            a := AGDeg(150 + 30 * k);
            C.MoveTo(30 + 6.5 * AGCos(a), 22 + 6.5 * AGSin(a));
            C.LineTo(30 + 9.5 * AGCos(a), 22 + 9.5 * AGSin(a));
            C.Stroke(SW(St, 1.3), AGRGB(255, 236, 140));
          End;
        End;
      4: Begin // speed (roller skate)
          SpeedLines(C, St, 11, 15, 6, AGRGB(230, 240, 255));
          C.Push;
          C.Translate(21, 14);
          C.Rect(-8, 9, 19.5, 2.4);
          DrawShapeOutlined(C, St, AGRGB(170, 172, 184), OutlineDark, 0.7);
          For k := 0 To 1 Do Begin
            C.Circle(-4 + 11 * k, 13.2, 2.8);
            DrawShapeOutlined(C, St, AGRGB(60, 60, 70), OutlineDark, 0.7);
            C.Circle(-4 + 11 * k, 13.2, 1.0);
            C.Fill(AGRGB(230, 220, 120));
          End;
          Boot(C, St, AGRGB(214, 44, 40));
          C.Pop;
        End;
      5: Begin // punch (boxing glove)
          SpeedLines(C, St, 9, 18, 5, AGRGB(230, 240, 255));
          C.RoundRect(9, 13, 8, 11, 2);
          DrawShapeOutlined(C, St, AGRGB(240, 238, 232), OutlineDark, 0.9);
          C.Ellipse(24, 18, 10, 8.5);
          DrawShapeOutlined(C, St, AGRGB(214, 36, 36), OutlineDark, 0.9);
          C.Ellipse(22, 12.5, 5.5, 3.4);
          DrawShapeOutlined(C, St, AGRGB(214, 36, 36), OutlineDark, 0.8);
          Shine(C, St, 27, 15, 3.5, 2, AGDeg(-20), AGRGB(255, 120, 110));
        End;
      6: Begin // grab (open hand)
          C.Push;
          C.Translate(20, 20);
          If IsCoarse(St) Then Begin
            // the half resolution only has room for three fingers
            C.Capsule(-4, 0, -7.5, -9, 2.8);
            C.Capsule(0, -1, 0, -12.5, 2.8);
            C.Capsule(4, 0, 7.5, -9, 2.8);
          End
          Else Begin
            C.Capsule(-5, 0, -8.2, -9, 2.2);
            C.Capsule(-1.8, -1, -2.8, -12.5, 2.2);
            C.Capsule(1.8, -1, 2.6, -12.2, 2.2);
            C.Capsule(5, 0, 7.6, -8.6, 2.2);
          End;
          C.Capsule(5, 6, 12, 1, 2.3);
          DrawShapeOutlined(C, St, AGRGB(250, 246, 236), OutlineDark, 0.9);
          C.RoundRect(-7, -3, 14.5, 13, 4);
          DrawShapeOutlined(C, St, AGRGB(250, 246, 236), OutlineDark, 0.9);
          C.MoveTo(-3, 4);
          C.QuadTo(0, 6, 3, 4);
          C.Stroke(SW(St, 0.9), AGRGB(150, 140, 130));
          C.Pop;
        End;
      7: Begin // spooger (line of bombs)
          For k := 0 To 2 Do
            MiniBomb(C, St, 8.5 + 11 * k, 24, 4.3, AGRGB(52, 54, 76), k = 2);
          OutlinedPts(C, St, [7, 9, 28, 9], 2.2, AGRGB(255, 230, 90), OutlineDark);
          C.Polygon([27, 4.5, 34, 9, 27, 13.5]);
          DrawShapeOutlined(C, St, AGRGB(255, 230, 90), OutlineDark, 0.7);
        End;
      8: Begin // gold flame
          FlameIcon(C, St, 20, 19.5, 10.5, 13.5, [AGRGB(196, 132, 18), AGRGB(255, 206, 54), AGRGB(255, 250, 200)]);
          Star(C, 31, 9, 4, 1.1, 4, 0);
          C.Fill(AGWhite);
          Star(C, 9, 26, 3, 0.9, 4, 0);
          C.Fill(AGWhite);
        End;
      9: Begin // trigger (remote control)
          OutlinedPts(C, St, [24, 12, 29, 5], 1.4, AGRGB(190, 190, 200), OutlineDark);
          C.Circle(29, 5, 2.2);
          DrawShapeOutlined(C, St, AGRGB(255, 70, 50), OutlineDark, 0.7);
          For k := 1 To 2 Do Begin
            C.ArcTo(29, 5, 3.5 + 2.6 * k, 3.5 + 2.6 * k, AGDeg(-70), AGDeg(10));
            C.Stroke(SW(St, 1.1), AGRGB(255, 240, 150));
          End;
          C.RoundRect(12, 11, 15, 19, 3);
          DrawShapeOutlined(C, St, AGRGB(70, 72, 84), OutlineDark, 0.9);
          C.Circle(19.5, 18, 4.3);
          DrawShapeOutlined(C, St, AGRGB(230, 40, 40), AGRGB(40, 10, 10), 0.8);
          Shine(C, St, 18.5, 16.5, 1.6, 1, 0, AGRGB(255, 150, 140));
          C.Rect(15, 25, 3, 2);
          C.Rect(21, 25, 3, 2);
          C.Fill(AGRGB(150, 152, 170));
        End;
      10: Begin // jelly (bouncing bomb)
          For k := 0 To 3 Do Begin
            a := AGDeg(200 + 28 * k);
            C.Circle(19 + 13 * AGCos(a), 25 + 15 * AGSin(a), AGMax(1.3, SW(St, 0) * 0.8));
            C.Fill(AGRGB(220, 235, 255));
          End;
          C.Push;
          C.Translate(25, 25);
          C.Scale(1.2, 0.82);
          MiniBomb(C, St, 0, 0, 7.5, AGRGB(70, 110, 220), false);
          C.Pop;
          C.MoveTo(17, 31);
          C.LineTo(33, 31);
          C.Stroke(SW(St, 1.2), AGRGB(230, 240, 255));
        End;
      11: Begin // ebola (skull and bones)
          Bone(C, St, 9, 8, 31, 28, AGRGB(236, 230, 212));
          Bone(C, St, 31, 8, 9, 28, AGRGB(236, 230, 212));
          Skull(C, St, 20, 17, AGRGB(200, 226, 96));
        End;
      12: Begin // slow (snail)
          C.MoveTo(28, 21);
          C.LineTo(29, 13);
          C.MoveTo(31, 22);
          C.LineTo(33.5, 14.5);
          C.Stroke(SW(St, 1.1), OutlineDark);
          C.Circle(29, 13, 1.4);
          C.Circle(33.5, 14.5, 1.4);
          C.Fill(OutlineDark);
          C.MoveTo(6, 27);
          C.LineTo(30, 27);
          C.QuadTo(34, 27, 33, 21);
          C.QuadTo(31, 18, 28, 21);
          C.LineTo(25, 23);
          C.LineTo(6, 24);
          C.ClosePath;
          DrawShapeOutlined(C, St, AGRGB(224, 200, 150), OutlineDark, 0.9);
          C.Circle(17, 17, 9);
          DrawShapeOutlined(C, St, AGRGB(196, 110, 46), OutlineDark, 0.9);
          C.MoveTo(17, 17);
          For k := 1 To 24 Do Begin
            a := AGDeg(-90 + 25 * k);
            C.LineTo(17 + (0.3 * k) * AGCos(a), 17 + (0.3 * k) * AGSin(a));
          End;
          C.Stroke(SW(St, 1.2), AGRGB(100, 50, 20));
        End;
      13: Begin // random
          pts := Nil;
          PArc(pts, 20, 13, 6.5, 6, AG_PI + 0.2, AG_TWO_PI + 0.9);
          PQuad(pts, 20 + 6.5 * AGCos(0.9), 13 + 6 * AGSin(0.9), 20, 18, 20, 22.5);
          OutlinedPts(C, St, pts, 3.8, AGRGB(255, 220, 60), OutlineDark);
          C.Circle(20, 28.5, 3.4);
          C.Fill(OutlineDark);
          C.Circle(20, 28.5, 2.1);
          C.Fill(AGRGB(255, 220, 60));
        End;
    End;
  End;

Begin
  area := Ctx.Area('powerups');
  St := Ctx.Style;
  dHue := area.Vary('hue', 0, 10);
  sat := area.Vary('sat', 0.55, 0.1);
  corner := area.Vary('corner', 5, 2.5);
  For i := 0 To high(Names) Do Begin
    icon := i;
    img := RenderImage(Ctx, 40, 36, @DrawIcon);
    Try
      Output.SaveImage('res/' + Names[i] + '.png', img, smOpaque);
    Finally
      img.Free;
    End;
  End;
End;

(******************************************************************************)
(* bomb                                                                       *)
(******************************************************************************)

(* Preview = Nil: all bomb sheets into Output, else only frame 0 of bomb.png into Preview *)
Procedure BombWork(Ctx: TAGContext; Output: TAGOutput; Preview: TAGCanvas);
Const
  KNormal = 0;
  KTrigger = 1;
  KDud = 2;
Var
  area: TAGArea;
  St: TAGStyle;
  size, pulse, shade, curl: Double;
  band: Integer;
  img: TAGImage;
  kind: Integer;
  wobble: Boolean;

  Procedure Spark(C: TAGCanvas; x, y: Double; Const Key: String);
  Var
    rnd: TAGRandom;
    i, n: Integer;
    a, l: Double;
  Begin
    rnd := area.Stream(Key);
    n := 5 + rnd.RangeI(0, 2);
    For i := 0 To n - 1 Do Begin
      a := rnd.Range(0, AG_TWO_PI);
      l := rnd.Range(2.5, 4.6);
      C.MoveTo(x + 1.5 * AGCos(a), y + 1.5 * AGSin(a));
      C.LineTo(x + l * AGCos(a), y + l * AGSin(a));
      If rnd.Chance(0.5) Then
        C.Stroke(SW(St, 1.1), AGRGB(255, 240, 120))
      Else
        C.Stroke(SW(St, 1.1), AGRGB(255, 150, 40));
    End;
    Star(C, x, y, rnd.Range(2.6, 3.3), 1.2, 5, rnd.Range(0, AG_TWO_PI));
    DrawShapeOutlined(C, St, AGRGB(255, 216, 60), AGRGB(200, 70, 20), 0.6);
    C.Circle(x, y, 1.2);
    C.Fill(AGWhite);
  End;

  (* Ball with cap, center (0, 0), radius r, in the current transformation *)
  Procedure Ball(C: TAGCanvas; r: Double; Frame: Integer);
  Var
    a, capx, capy, tx, ty, ex, ey: Double;
    body: TAGColor;
    i: Integer;
    lit: Boolean;
    pts: TPts;
  Begin
    pts := Nil;
    a := AGDeg(-52);
    capx := (r + 0.5) * AGCos(a);
    capy := (r + 0.5) * AGSin(a);
    body := PlayerGreen(shade);
    tx := capx + 2.6 * AGCos(a);
    ty := capy + 2.6 * AGSin(a);
    // fuse / antenna behind the cap
    Case kind Of
      KNormal: Begin
          ex := tx + 3.5;
          ey := ty - 5.5;
          PCubic(pts, tx, ty, tx + 0.5, ty - 3.5 * curl, ex - 3 * curl, ey + 1.5, ex, ey);
          OutlinedPts(C, St, pts, 1.6, AGRGB(206, 170, 112), OutlineDark);
        End;
      KDud: Begin
          ex := tx + 3.5;
          ey := ty - 2;
          PQuad(pts, tx, ty, tx + 0.5, ty - 2.5, ex, ey);
          OutlinedPts(C, St, pts, 1.6, AGRGB(70, 64, 60), OutlineDark);
        End;
      KTrigger: Begin
          ex := tx + 4;
          ey := ty - 8;
          OutlinedPts(C, St, [tx, ty, ex, ey], 1.2, AGRGB(196, 198, 210), OutlineDark);
        End;
    End;
    C.Push;
    C.Translate(capx, capy);
    C.Rotate(a + AG_HALF_PI);
    C.RoundRect(-3.4, -2.6, 6.8, 5, 1);
    DrawShapeOutlined(C, St, AGRGB(168, 170, 182), OutlineDark, 0.8);
    C.Pop;
    // body
    C.Circle(0, 0, r);
    DrawShapeOutlined(C, St, body, OutlineDark);
    If St <> asSoft Then Begin
      // two tone shading: lighter disc to the top left
      C.Circle(-0.13 * r, -0.13 * r, 0.8 * r);
      C.Fill(PlayerGreen(AGMin(1, shade + 0.22)));
    End;
    If band = 1 Then Begin
      C.Push;
      C.Rotate(AGDeg(-18));
      C.ArcTo(0, 0.12 * r, 0.97 * r, 0.34 * r, AGDeg(8), AGDeg(172));
      C.Stroke(SW(St, 0.2 * r), AGRGB(84, 86, 98));
      C.Pop;
    End;
    Shine(C, St, -0.4 * r, -0.42 * r, 0.3 * r, 0.17 * r, AGDeg(-42), AGShade(PlayerGreen(0.95), 1.55));
    If St <> asSoft Then Begin
      C.Circle(-0.5 * r, -0.5 * r, 0.09 * r + 0.5);
      C.Fill(AGWhite);
    End;
    Case kind Of
      KNormal: Spark(C, ex, ey, 'spark' + IntToStr(ord(wobble)) + '_' + IntToStr(Frame));
      KTrigger: Begin
          // panel with a running light
          C.Push;
          C.Translate(0, 0.18 * r);
          If Not IsCoarse(St) Then C.Rotate(AGDeg(-16));
          C.RoundRect(-0.62 * r, -0.24 * r, 1.24 * r, 0.48 * r, 1.4);
          DrawShapeOutlined(C, St, AGRGB(40, 32, 40), OutlineDark, 0.7);
          For i := 0 To 2 Do Begin
            lit := (Frame Div 3) Mod 3 = i;
            C.Rect(-0.44 * r + i * 0.34 * r, -0.1 * r, 0.2 * r, 0.2 * r);
            If lit Then
              C.Fill(AGRGB(255, 70, 50))
            Else
              C.Fill(AGRGB(110, 26, 26));
          End;
          C.Pop;
          // lamp
          lit := (Frame Mod 6) < 3;
          If lit Then Begin
            For i := 0 To 3 Do Begin
              C.MoveTo(ex + 2.6 * AGCos(AG_HALF_PI * i + 0.8), ey + 2.6 * AGSin(AG_HALF_PI * i + 0.8));
              C.LineTo(ex + 4.3 * AGCos(AG_HALF_PI * i + 0.8), ey + 4.3 * AGSin(AG_HALF_PI * i + 0.8));
              C.Stroke(SW(St, 1.0), AGRGB(255, 200, 90));
            End;
            C.Circle(ex, ey, 2.4);
            DrawShapeOutlined(C, St, AGRGB(255, 64, 44), OutlineDark, 0.7);
            C.Circle(ex - 0.6, ey - 0.6, 0.8);
            C.Fill(AGWhite);
          End
          Else Begin
            C.Circle(ex, ey, 2.4);
            DrawShapeOutlined(C, St, AGRGB(120, 30, 30), OutlineDark, 0.7);
          End;
        End;
      KDud: Begin
          // smoke cloud that grows, drawn as one shape so the outline only goes around it
          If Frame = 0 Then Begin
            C.Circle(ex - 1, ey - 3, 2.2);
            C.Circle(ex - 4, ey - 5.5, 2.8);
            C.Circle(ex + 1.8, ey - 5, 1.9);
          End
          Else Begin
            C.Circle(ex - 0.5, ey - 3.5, 2.6);
            C.Circle(ex - 4.5, ey - 6.5, 3.4);
            C.Circle(ex - 9, ey - 6, 2.6);
            C.Circle(ex + 2.5, ey - 6.5, 2.3);
          End;
          DrawShapeOutlined(C, St, AGRGB(190, 190, 198), AGRGB(96, 96, 106), 0.7);
          // soot on the body
          C.Ellipse(0.35 * r, -0.55 * r, 0.22 * r, 0.12 * r);
          C.Fill(AGRGB(50, 46, 46));
        End;
    End;
  End;

  Procedure DrawBomb(C: TAGCanvas; Frame: Integer);
  Var
    s, r, t, cy: Double;
  Begin
    r := 12.2 * size;
    Case kind Of
      KNormal: cy := 23;
      KTrigger: cy := 24;
    Else
      cy := 25;
    End;
    If wobble Then Begin
      t := AG_TWO_PI * Frame / 12;
      C.Translate(19.5, cy + r);
      C.Rotate(AGDeg(9) * AGSin(t));
      C.Scale(1 + 0.07 * AGSin(2 * t), 1 - 0.07 * AGSin(2 * t));
      C.Translate(0, -r);
    End
    Else Begin
      If kind = KDud Then
        s := 1
      Else
        s := 1 - pulse * (1 - AGCos(AG_TWO_PI * Frame / 18)) / 2;
      C.Translate(19.5, cy);
      C.Scale(s, s);
    End;
    Ball(C, r, Frame);
  End;

  Procedure Sheet(Const Name: String; aKind: Integer; aWobble: Boolean; fpr, fpc, w, h, Count: Integer);
  Begin
    kind := aKind;
    wobble := aWobble;
    img := RenderSheet(Ctx, fpr, fpc, w, h, Count, @DrawBomb);
    Try
      Output.SaveImage('atomic/' + Name, img, smKeyed);
    Finally
      img.Free;
    End;
  End;

Begin
  area := Ctx.Area('bomb');
  St := Ctx.Style;
  size := area.Vary('size', 1.0, 0.06);
  pulse := area.Vary('pulse', 0.13, 0.04);
  shade := area.Vary('shade', 0.5, 0.1);
  curl := area.Vary('curl', 1.0, 0.4);
  band := area.Pick('band', 2);
  If assigned(Preview) Then Begin
    kind := KNormal;
    wobble := false;
    DrawBomb(Preview, 0);
    exit;
  End;
  Sheet('bomb.png', KNormal, false, 5, 4, 40, 37, 18);
  Sheet('bomb_dud.png', KDud, false, 1, 2, 40, 40, 2);
  Sheet('bomb_trigger.png', KTrigger, false, 5, 4, 40, 40, 18);
  Sheet('bomb_wobble.png', KNormal, true, 4, 3, 40, 37, 12);
End;

Procedure GenerateBomb(Ctx: TAGContext; Output: TAGOutput);
Begin
  BombWork(Ctx, Output, Nil);
End;

Procedure DrawBombPreview(Ctx: TAGContext; C: TAGCanvas);
Begin
  BombWork(Ctx, Nil, C);
End;

(******************************************************************************)
(* flame                                                                      *)
(******************************************************************************)

Procedure GenerateFlame(Ctx: TAGContext; Output: TAGOutput);
Const
  CellW = 41;
  CellH = 37;
  CX = 20; // axis of vertical flames
  CY = 18; // axis of horizontal flames
  PerH = 40; // period of the waves (grid spacing)
  PerV = 36;
  WaveA: Array[1..3] Of Double = (0.12, 0.08, 0.06);
  LickA = 0.3;
  WaveM: Array[1..3] Of Integer = (1, -2, 3); // time factor, integer -> 5 frame loop
Var
  area: TAGArea;
  St: TAGStyle;
  wScale, amp, coreW, tipPos, taperLen, tipPh: Double;
  ph: Array[0..1, 1..3] Of Double;
  LW, LTip: Array Of Double;
  LCol: Array Of TAGColor;
  img: TAGImage;
  i, k: Integer;

  Function HalfW(u, P: Double; Side, Layer, t: Integer): Double;
  Var
    j: Integer;
    s: Double;
  Begin
    s := 0;
    For j := 1 To 3 Do
      s := s + WaveA[j] * AGSin(AG_TWO_PI * (j * u / P + ph[Side, j] + 0.07 * Layer + WaveM[j] * t / 5));
    // flame licks: sharp peaks, standing, flickering with time
    s := s + LickA * (1 - 0.7 * Layer / high(LW)) * ((1 - AGAbs(AGSin(AG_PI * (4 * u / P + ph[Side, 1] * 2))))
      - 0.36) * (0.7 + 0.3 * AGCos(AG_TWO_PI * (t / 5 + u / P + ph[Side, 2])));
    result := LW[Layer] * wScale * (1 + 0.05 * AGCos(AG_TWO_PI * t / 5)) * (1 + amp * s);
  End;

  (* Band along the axis from u0 to u1, TipLo / TipHi = round tip at that end *)
  Procedure Band(C: TAGCanvas; Horiz: Boolean; u0, u1: Double; TipLo, TipHi: Boolean; Layer, t: Integer);
  Var
    n, j: Integer;
    u, v, P, cc, tp, d: Double;

    Procedure Pt(uu, vv: Double; First: Boolean);
    Begin
      If Horiz Then Begin
        If First Then C.MoveTo(uu, vv) Else C.LineTo(uu, vv);
      End
      Else Begin
        If First Then C.MoveTo(vv, uu) Else C.LineTo(vv, uu);
      End;
    End;

    Function Taper(uu: Double): Double;
    Begin
      result := 1;
      If TipHi Then Begin
        d := (u1 - uu) / taperLen;
        If d < 1 Then result := AGSqrt(AGMax(0, 1 - sqr(1 - d)));
      End;
      If TipLo Then Begin
        d := (uu - u0) / taperLen;
        If d < 1 Then result := result * AGSqrt(AGMax(0, 1 - sqr(1 - d)));
      End;
    End;

  Begin
    If Horiz Then Begin
      P := PerH;
      cc := CY;
    End
    Else Begin
      P := PerV;
      cc := CX;
    End;
    If TipLo Then u0 := u0 + LTip[Layer];
    If TipHi Then u1 := u1 - LTip[Layer];
    If u1 <= u0 + 1 Then exit;
    n := AGCeil(u1 - u0);
    For j := 0 To n Do Begin
      u := u0 + (u1 - u0) * j / n;
      tp := Taper(u);
      v := cc - HalfW(u, P, 0, Layer, t) * tp;
      Pt(u, v, j = 0);
    End;
    For j := n Downto 0 Do Begin
      u := u0 + (u1 - u0) * j / n;
      tp := Taper(u);
      v := cc + HalfW(u, P, 1, Layer, t) * tp;
      Pt(u, v, false);
    End;
    C.ClosePath;
    C.Fill(LCol[Layer]);
  End;

  Procedure DrawFlame(C: TAGCanvas; Frame: Integer);
  Var
    row, t, L: Integer;
    flick, tipH, tipV: Double;
  Begin
    row := Frame Div 5;
    t := Frame Mod 5;
    flick := 1.8 * AGSin(AG_TWO_PI * t / 5 + tipPh);
    tipH := tipPos + flick; // distance of the tip from the connecting side
    tipV := (tipPos - 4) + flick;
    For L := 0 To high(LW) Do
      Case row Of
        0: Begin // cross
            Band(C, true, -1, CellW + 1, false, false, L, t);
            Band(C, false, -1, CellH + 1, false, false, L, t);
            C.Circle(CX, CY, LW[L] * wScale * 1.3 * (1 + 0.05 * AGCos(AG_TWO_PI * t / 5)));
            C.Fill(LCol[L]);
          End;
        1: Band(C, false, PerV - tipV, CellH + 1, true, false, L, t); // end up
        2: Band(C, true, PerH - tipH, CellW + 1, true, false, L, t); // end left
        3: Band(C, false, -1, tipV, false, true, L, t); // end down
        4: Band(C, true, -1, tipH, false, true, L, t); // end right
        5, 6: Band(C, false, -1, CellH + 1, false, false, L, t); // middle vertical
        7, 8: Band(C, true, -1, CellW + 1, false, false, L, t); // middle horizontal
      End;
  End;

Const
  BaseW: Array[0..3] Of Double = (1.0, 0.72, 0.42, 0.16);
  BaseTip: Array[0..3] Of Double = (0, 2.5, 5, 8);
Var
  baseCol: Array[0..3] Of TAGColor;
  n: Integer;
  s, f: Double;
Begin
  area := Ctx.Area('flame');
  St := Ctx.Style;
  wScale := 10.5 * area.Vary('width', 1.0, 0.1);
  amp := area.Vary('wobble', 1.0, 0.35);
  coreW := area.Vary('core', 1.0, 0.25);
  tipPos := area.Vary('tip', 31, 2);
  taperLen := 13;
  tipPh := AG_TWO_PI * area.Rnd('tipphase');
  For i := 0 To 1 Do
    For k := 1 To 3 Do
      ph[i, k] := area.Rnd('phase', i * 3 + k);
  baseCol[0] := PlayerGreen(0.12);
  baseCol[1] := PlayerGreen(0.55);
  baseCol[2] := AGShade(PlayerGreen(0.92), 1.45);
  baseCol[3] := AGRGB(255, 252, 236);
  // soft: more layers in between for a smooth glow
  If St = asSoft Then n := 7 Else n := 4;
  setlength(LW, n);
  setlength(LTip, n);
  setlength(LCol, n);
  For i := 0 To n - 1 Do Begin
    s := 3 * i / (n - 1);
    k := AGFloor(s);
    If k >= 3 Then k := 2;
    f := s - k;
    LW[i] := AGLerp(BaseW[k], BaseW[k + 1], f);
    LTip[i] := AGLerp(BaseTip[k], BaseTip[k + 1], f);
    LCol[i] := AGMix(baseCol[k], baseCol[k + 1], f);
  End;
  LW[n - 1] := LW[n - 1] * coreW;
  img := RenderSheet(Ctx, 5, 9, CellW, CellH, 45, @DrawFlame);
  Try
    Output.SaveImage('atomic/flame.png', img, smKeyed);
  Finally
    img.Free;
  End;
End;

(******************************************************************************)
(* objects                                                                    *)
(******************************************************************************)

Procedure GenerateObjects(Ctx: TAGContext; Output: TAGOutput);
Var
  area: TAGArea;
  St: TAGStyle;
  arrowHue, beltHue, holeHue, hurryHue, slant: Double;
  img: TAGImage;

  Procedure DrawHole(C: TAGCanvas; Frame: Integer);
  Var
    i: Integer;
    a, rr: Double;
    earth: TAGColor;
  Begin
    earth := AGHSV(holeHue, 0.45, 0.5);
    C.Ellipse(20, 19, 17.5, 14.5);
    DrawShape(C, St, earth);
    C.Ellipse(20, 19.5, 13.5, 10.5);
    C.Fill(AGShade(earth, 0.5));
    C.Ellipse(20, 21.5, 12.2, 8.6);
    C.FillPaint(C.Radial(20, 23, 12, [AGRGB(4, 3, 3), AGRGB(16, 12, 10), AGShade(earth, 0.3)]));
    For i := 0 To 6 Do Begin
      a := AG_TWO_PI * (i + area.Rnd('rock', i) * 0.6) / 7;
      rr := 1.6 + 1.2 * area.Rnd('rocksize', i);
      C.Circle(20 + 15.5 * AGCos(a), 19 + 12.5 * AGSin(a), rr);
      DrawShape(C, St, AGHSV(holeHue + 10, 0.15, 0.55 + 0.15 * area.Rnd('rockv', i)), 0.7);
    End;
  End;

  Procedure DrawArrow(C: TAGCanvas; Frame: Integer);
  Begin
    C.Translate(10.5, 10.5);
    C.Rotate(-AG_HALF_PI * Frame); // 0 right, 1 up, 2 left, 3 down
    C.Polygon([-8.5, -3.2, 0.5, -3.2, 0.5, -8.5, 9, 0, 0.5, 8.5, 0.5, 3.2, -8.5, 3.2]);
    DrawShapeOutlined(C, St, AGHSV(arrowHue, 0.85, 1.0), OutlineDark, 1.1);
  End;

  Procedure DrawConveyor(C: TAGCanvas; Frame: Integer);
  Var
    dir, step, k: Integer;
    horiz: Boolean;
    off, p, x0: Double;
    accent, rail, belt: TAGColor;

    Procedure Chevron(pos: Double);
    Begin
      // drawn pointing right / down, the sign of the direction mirrors it
      Case dir Of
        0: C.Polygon([pos, 8, pos + 4, 8, pos + 9, 18, pos + 4, 28, pos, 28, pos + 5, 18]);
        2: C.Polygon([pos + 9, 8, pos + 5, 8, pos, 18, pos + 5, 28, pos + 9, 28, pos + 4, 18]);
        1: C.Polygon([8, pos + 9, 8, pos + 5, 20, pos, 32, pos + 5, 32, pos + 9, 20, pos + 4]);
        3: C.Polygon([8, pos, 8, pos + 4, 20, pos + 9, 32, pos + 4, 32, pos, 20, pos + 5]);
      End;
      DrawShapeOutlined(C, St, accent, OutlineDark, 0.6);
    End;

  Begin
    // frames 0-4 left, 5-9 right, 10-13 up, 14-17 down (as in the original .ani)
    If Frame < 5 Then Begin dir := 2; step := Frame; End
    Else If Frame < 10 Then Begin dir := 0; step := Frame - 5; End
    Else If Frame < 14 Then Begin dir := 1; step := Frame - 10; End
    Else Begin dir := 3; step := Frame - 14; End;
    horiz := (dir = 0) Or (dir = 2);
    accent := AGHSV(beltHue, 0.8, 0.95);
    rail := AGRGB(150, 152, 162);
    belt := AGRGB(58, 60, 68);
    C.Rect(-1, -1, 42, 38);
    C.Fill(belt);
    If horiz Then Begin
      p := 10;
      off := 2 * step;
      If dir = 2 Then off := -off;
      For k := -2 To 5 Do Begin
        x0 := k * p + off;
        C.Rect(x0 + 7, 4, 1, 28);
        C.Fill(AGRGB(44, 46, 52));
      End;
      For k := -2 To 5 Do
        Chevron(k * p + off);
      C.Rect(-1, 0, 42, 4);
      C.Rect(-1, 32, 42, 4);
      DrawShapeOutlined(C, St, rail, OutlineDark, 0.5);
      For k := 0 To 3 Do Begin
        C.Circle(5 + 10 * k, 2, 0.9);
        C.Circle(5 + 10 * k, 34, 0.9);
      End;
      C.Fill(AGShade(rail, 0.5));
    End
    Else Begin
      p := 12;
      off := 3 * step;
      If dir = 1 Then off := -off;
      For k := -2 To 4 Do Begin
        x0 := k * p + off;
        C.Rect(4, x0 + 10, 32, 1);
        C.Fill(AGRGB(44, 46, 52));
      End;
      For k := -2 To 4 Do
        Chevron(k * p + off);
      C.Rect(0, -1, 4, 38);
      C.Rect(36, -1, 4, 38);
      DrawShapeOutlined(C, St, rail, OutlineDark, 0.5);
      For k := 0 To 2 Do Begin
        C.Circle(2, 6 + 12 * k, 0.9);
        C.Circle(38, 6 + 12 * k, 0.9);
      End;
      C.Fill(AGShade(rail, 0.5));
    End;
  End;

  Procedure DrawTramp(C: TAGCanvas; Frame: Integer);
  Const
    Press: Array[0..4] Of Double = (0, 0.6, 1.0, -0.5, 0.15);
  Var
    p, a: Double;
    i: Integer;
    mat: TAGColor;
  Begin
    p := Press[Frame];
    C.Ellipse(20, 19, 18, 15);
    DrawShape(C, St, AGRGB(70, 80, 104));
    C.Ellipse(20, 19, 15.2, 12.4);
    C.Fill(AGRGB(36, 40, 54));
    For i := 0 To 11 Do Begin
      a := AG_TWO_PI * i / 12;
      C.MoveTo(20 + 15.5 * AGCos(a), 19 + 12.8 * AGSin(a));
      C.LineTo(20 + (12.5 - p) * AGCos(a), 19 + (10 - 0.8 * p) * AGSin(a) + p);
      C.Stroke(SW(St, 1.0), AGRGB(200, 204, 214));
    End;
    If p < 0 Then mat := AGHSV(arrowHue + 180, 0.6, 0.62) Else mat := AGHSV(arrowHue + 180, 0.6, 0.5 - 0.12 * p);
    C.Ellipse(20, 19 + p, 12.5 - p, 10 - 0.8 * p);
    DrawShape(C, St, mat, 0.8);
    C.Ellipse(20, 19 + 1.4 * p, 6 * (1 - 0.25 * p), 4.6 * (1 - 0.25 * p));
    C.Stroke(SW(St, 1.2), AGRGB(230, 236, 255));
    C.Circle(20, 19 + 1.4 * p, 1.5);
    C.Fill(AGRGB(230, 236, 255));
  End;

  Procedure DrawInfinity(C: TAGCanvas; Frame: Integer);
  Var
    i: Integer;
    t, s, d: Double;
  Begin
    For i := 0 To 64 Do Begin
      t := AG_TWO_PI * i / 64;
      s := AGSin(t);
      d := 1 + s * s;
      If i = 0 Then
        C.MoveTo(24 + 19.5 * AGCos(t) / d, 8.5 + 15 * s * AGCos(t) / d)
      Else
        C.LineTo(24 + 19.5 * AGCos(t) / d, 8.5 + 15 * s * AGCos(t) / d);
    End;
    C.ClosePath;
    C.Stroke(SW(St, 5.4), OutlineDark, true);
    C.Stroke(SW(St, 2.8), AGHSV(hurryHue, 0.8, 1.0), St = asSoft);
    If St = asSoft Then C.Stroke(1.0, AGRGB(255, 250, 220));
  End;

  Procedure DrawDead(C: TAGCanvas; Frame: Integer);
  Var
    i: Integer;
    pts: TPts;
  Begin
    pts := Nil;
    For i := 0 To 8 Do
      PAdd(pts, 3 + i * 71 / 8, 4 + 11 * ((i + 1) Mod 2));
    OutlinedPts(C, St, pts, 2.0, AGRGB(236, 40, 30), AGRGB(70, 0, 0));
  End;

  Procedure DrawHurry(C: TAGCanvas; Frame: Integer);
  Const
    T = 12;
  Var
    m: TAGMatrix;
    fillc: TAGColor;
    sc, ox, oy: Double;

    Procedure Letters(dx, dy: Double);
    Var
      x: Double;
      pass: Integer;
    Begin
      C.Push;
      C.Translate(dx, dy);
      C.Transform(m);
      C.FillRule := frEvenOdd;
      x := 0;
      // H
      C.Polygon([x, 0, x + T, 0, x + T, 26, x + 40 - T, 26, x + 40 - T, 0, x + 40, 0, x + 40, 64,
        x + 40 - T, 64, x + 40 - T, 38, x + T, 38, x + T, 64, x, 64]);
      x := x + 46;
      // U
      C.Polygon([x, 0, x + T, 0, x + T, 52, x + 40 - T, 52, x + 40 - T, 0, x + 40, 0, x + 40, 56,
        x + 32, 64, x + 8, 64, x, 56]);
      x := x + 46;
      // R, R
      For pass := 0 To 1 Do Begin
        C.Polygon([x, 0, x + 32, 0, x + 40, 8, x + 40, 28, x + 33, 35, x + 41, 64, x + 28, 64, x + 21, 38,
          x + T, 38, x + T, 64, x, 64]);
        C.Polygon([x + T, 11, x + 26, 11, x + 28, 13, x + 28, 25, x + 26, 27, x + T, 27]);
        x := x + 47;
      End;
      // Y
      C.Polygon([x, 0, x + T, 0, x + 20, 20, x + 40 - T, 0, x + 40, 0, x + 26, 32, x + 26, 64, x + 14, 64, x + 14, 32]);
      x := x + 46;
      // !
      C.Polygon([x + 1, 0, x + 14, 0, x + 11, 46, x + 4, 46]);
      C.Polygon([x + 2, 52, x + 13, 52, x + 13, 64, x + 2, 64]);
      C.Pop;
    End;

  Begin
    m := AGIdentity;
    m.c := -slant;
    // letters: 6 glyphs, 0..246 wide before the slant, 64 high, scaled to fit with the shadow
    fillc := AGHSV(hurryHue, 0.85, 1.0);
    sc := AGMin((278 - 12) / (246 + 64 * slant + 4), (91 - 14) / 68);
    ox := (278 - (246 + 64 * slant + 4) * sc) / 2;
    oy := (91 - 68 * sc) / 2;
    C.Translate(ox, oy);
    C.Scale(sc, sc);
    // shadow
    Letters(64 * slant + 4, 4);
    C.Fill(AGRGB(70, 10, 6));
    Letters(64 * slant, 0);
    If St = asSoft Then Begin
      C.FillPaint(C.Linear(0, 0, 0, 64, [AGRGB(255, 250, 200), fillc, AGHSV(hurryHue - 25, 0.95, 0.9)]), true);
      C.Stroke(2.2, AGRGB(110, 14, 8));
    End
    Else If St = asRetro Then Begin
      C.FillPaint(AGBandPaint(C, 0, 64, fillc), true);
      C.Stroke(3.6 / sc, AGRGB(110, 14, 8));
    End
    Else
      DrawShapeOutlined(C, St, fillc, AGRGB(110, 14, 8), 1.6);
    C.FillRule := frNonZero;
  End;

Begin
  area := Ctx.Area('objects');
  St := Ctx.Style;
  arrowHue := area.Vary('arrowhue', 48, 14);
  beltHue := area.Vary('belthue', 40, 15);
  holeHue := area.Vary('holehue', 28, 10);
  hurryHue := area.Vary('hurryhue', 46, 8);
  slant := area.Vary('slant', 0.2, 0.06);

  img := RenderImage(Ctx, 40, 36, @DrawHole);
  Try
    Output.SaveImage('res/hole.png', img, smKeyed);
  Finally
    img.Free;
  End;
  img := RenderSheet(Ctx, 4, 1, 21, 21, 4, @DrawArrow);
  Try
    Output.SaveAni('res/arrows.ani', img, AniSpec(45, 100, 4, 1, 21, 21, [0, 1, 2, 3], [1, 1, 1, 1]));
  Finally
    img.Free;
  End;
  img := RenderSheet(Ctx, 5, 4, 40, 36, 18, @DrawConveyor);
  Try
    Output.SaveAni('res/conveyor.ani', img, AniSpec(45, 100, 5, 4, 40, 36, [5, 10, 0, 14], [5, 4, 5, 4]));
  Finally
    img.Free;
  End;
  img := RenderSheet(Ctx, 3, 2, 40, 36, 5, @DrawTramp);
  Try
    Output.SaveAni('res/tramp.ani', img, AniSpec(0, 100, 3, 2, 40, 36, [0], [5]));
  Finally
    img.Free;
  End;
  img := RenderImage(Ctx, 48, 17, @DrawInfinity);
  Try
    Output.SaveImage('res/infinity.png', img, smKeyed);
  Finally
    img.Free;
  End;
  img := RenderImage(Ctx, 77, 20, @DrawDead);
  Try
    Output.SaveImage('res/playerdead.png', img, smKeyed);
  Finally
    img.Free;
  End;
  img := RenderImage(Ctx, 278, 91, @DrawHurry);
  Try
    Output.SaveImage('res/hurry.png', img, smKeyed);
  Finally
    img.Free;
  End;
End;

Initialization
  RegisterGenerator('powerups', 'Power up icons (res/pow*.png)', 14, @GeneratePowerups);
  RegisterGenerator('bomb', 'Bombs (atomic/bomb*.png)', 4, @GenerateBomb);
  RegisterGenerator('flame', 'Flames (atomic/flame.png)', 1, @GenerateFlame);
  RegisterGenerator('objects', 'Field objects, hurry, infinity (res/)', 7, @GenerateObjects);

End.
