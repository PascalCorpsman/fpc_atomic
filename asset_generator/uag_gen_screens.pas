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
 * Menu screens and their small graphics (res/), own design.
 *
 * Contract with client/uscreens.pas (where the engine draws on top):
 *  - mainmenu.png: 7 labels baked in, cursor ani at (310, 100 + i * 37), mouse
 *    area x 350..610. The left half (x 0..320) is the launcher portrait.
 *  - options.png: 13 text lines at x = 60, y = 37 + 28 * i, cursor at
 *    (20, 25 + 28 * i) -> calm area, decoration only on the right.
 *  - join.png: player setup text x 60..500, y 77..460, join text at (100, 50).
 *  - results.png: text x 130..560, y 100..400, title baked above.
 *  - fieldsetup.png: keyed, the field preview shows through the fuchsia
 *    window x 380..598, y 82..279; text at (55, 176) / (60, 400) ...
 *  - victory0..9.png: winner = index of PlayerColors, team0 = white team,
 *    team1 = red team, draw.png.
 *  - loaddialog.png: 9-slice of 24 x 24 tiles (edges and center are tiled /
 *    stretched, so they only depend on the distance to the border).
 *  - bigfont.png: 4 x 3 cells of 18 x 27, digits 0..9 and ':' (game timer).
 *)
Unit uag_gen_screens;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Implementation

Uses SysUtils, uag_math, uag_random, uag_image, uag_canvas, uag_context, uag_output, uag_render, uag_registry, uag_font;

Const
  // PlayerColors of the engine (units/uatomic_common.pas), R, G, B in 0..100
  PlayerRGB: Array[0..9, 0..2] Of Integer = (
    (100, 100, 100), (20, 20, 20), (100, 0, 0), (0, 0, 100), (0, 100, 0),
    (100, 100, 0), (0, 100, 100), (100, 0, 100), (100, 50, 0), (50, 0, 100));
  ConfettiCols: Array[0..5] Of Cardinal = ($F04848, $48A8F0, $F0D040, $60D068, $F09040, $F4F4F4);

Var
  Ol: TAGColor; // outline of figures and props

Function PlayerColor(i: Integer): TAGColor;
Begin
  result := AGRGB(AGRound(PlayerRGB[i, 0] * 2.55), AGRound(PlayerRGB[i, 1] * 2.55), AGRound(PlayerRGB[i, 2] * 2.55));
End;

(* Player color usable as a body color (black gets a bit of light, so the shading shows) *)
Function FigureColor(i: Integer): TAGColor;
Begin
  Case i Of
    0: result := AGHex($ECECF0);
    1: result := AGHex($3C3C48);
    3: result := AGHex($2850E8);
    7: result := AGHex($E838D0); // never exactly fuchsia (transparent key)
    9: result := AGHex($8030D8);
  Else
    result := PlayerColor(i);
  End;
End;

Function GradColor(Const Cols: Array Of TAGColor; t: Double): TAGColor;
Var
  seg: Double;
  i: Integer;
Begin
  t := AGClamp(t, 0, 1);
  seg := t * high(Cols);
  i := AGFloor(seg);
  If i >= high(Cols) Then exit(Cols[high(Cols)]);
  result := AGMix(Cols[i], Cols[i + 1], seg - i);
End;

(* Vertical gradient, pixel style uses steps, retro fewer and harder ones *)
Procedure VGradient(C: TAGCanvas; Style: TAGStyle; x, y, w, h: Double; Const Cols: Array Of TAGColor);
Var
  k, Bands: Integer;
Begin
  If Style = asRetro Then Bands := 7 Else Bands := 18;
  If IsCoarse(Style) Then Begin
    For k := 0 To Bands - 1 Do Begin
      C.Rect(x, y + h * k / Bands, w, h / Bands + 1);
      C.Fill(GradColor(Cols, (k + 0.5) / Bands));
    End;
  End
  Else Begin
    C.Rect(x, y, w, h);
    C.FillPaint(C.Linear(x, y, x, y + h, Cols));
  End;
End;

Procedure Glow(C: TAGCanvas; cx, cy, r: Double; Col: TAGColor; Alpha: Integer);
Begin
  C.Circle(cx, cy, r);
  C.FillPaint(C.Radial(cx, cy, r, [AGWithAlpha(Col, Alpha), AGWithAlpha(Col, Alpha Div 3), AGWithAlpha(Col, 0)]));
End;

Procedure StarPoly(C: TAGCanvas; cx, cy, ro, ri: Double; n: Integer; rot: Double);
Var
  pts: Array Of Double;
  k: Integer;
  a, r: Double;
Begin
  pts := Nil;
  setlength(pts, 4 * n);
  For k := 0 To 2 * n - 1 Do Begin
    a := rot + AG_PI * k / n;
    If odd(k) Then r := ri Else r := ro;
    pts[2 * k] := cx + r * AGCos(a);
    pts[2 * k + 1] := cy + r * AGSin(a);
  End;
  C.Polygon(pts);
End;

Procedure Stars(C: TAGCanvas; Var R: TAGRandom; Count: Integer; x0, y0, w, h: Double);
Var
  k: Integer;
  x, y, s: Double;
  a: Integer;
Begin
  For k := 0 To Count - 1 Do Begin
    x := x0 + R.NextUnit * w;
    y := y0 + R.NextUnit * h;
    s := 0.6 + R.NextUnit * R.NextUnit * 1.6;
    a := 90 + R.RangeI(0, 165);
    If k Mod 11 = 0 Then Begin
      s := s + 1.2;
      StarPoly(C, x, y, s * 2.6, s * 0.45, 4, 0);
      C.Fill(AGWithAlpha(AGHex($FFF8E0), a));
    End
    Else Begin
      C.Circle(x, y, s);
      C.Fill(AGWithAlpha(AGWhite, a));
    End;
  End;
End;

Procedure Rays(C: TAGCanvas; cx, cy: Double; n: Integer; len: Double; Col: TAGColor; Alpha: Integer; rot: Double);
Var
  k: Integer;
  a0, a1: Double;
Begin
  For k := 0 To n - 1 Do Begin
    a0 := rot + AG_TWO_PI * k / n;
    a1 := a0 + AG_PI / n;
    C.Polygon([cx, cy, cx + len * AGCos(a0), cy + len * AGSin(a0), cx + len * AGCos(a1), cy + len * AGSin(a1)]);
    C.Fill(AGWithAlpha(Col, Alpha));
  End;
End;

Procedure Confetti(C: TAGCanvas; Var R: TAGRandom; Count: Integer; x0, y0, w, h: Double);
Var
  k: Integer;
Begin
  For k := 0 To Count - 1 Do Begin
    C.Push;
    C.Translate(x0 + R.NextUnit * w, y0 + R.NextUnit * h);
    C.Rotate(R.NextUnit * AG_TWO_PI);
    C.Rect(-3.5, -1.8, 7, 3.6);
    C.Fill(AGHex(ConfettiCols[R.RangeI(0, high(ConfettiCols))]));
    C.Pop;
  End;
End;

(* Round bomb with cap, fuse and spark; Phase 0..1 changes the spark *)
Procedure DrawBomb(C: TAGCanvas; Style: TAGStyle; cx, cy, r, Phase, rot: Double; Lit: Boolean = true; Body: Cardinal = $2C3448);
Var
  ex, ey, s: Double;
Begin
  // fuse behind the cap
  ex := cx + r * 1.12;
  ey := cy - r * 1.3;
  C.BeginPath;
  C.MoveTo(cx + r * 0.72, cy - r * 0.72);
  C.QuadTo(cx + r * 1.2, cy - r * 0.86, ex, ey);
  C.Stroke(AGMax(1.6, r * 0.16), Ol);
  C.BeginPath;
  C.MoveTo(cx + r * 0.72, cy - r * 0.72);
  C.QuadTo(cx + r * 1.2, cy - r * 0.86, ex, ey);
  C.Stroke(AGMax(0.8, r * 0.09), AGHex($D8B070));
  C.Circle(cx, cy, r);
  DrawShapeOutlined(C, Style, AGHex(Body), Ol);
  // cap
  C.Push;
  C.Translate(cx + r * 0.66, cy - r * 0.66);
  C.Rotate(AGDeg(45));
  C.RoundRect(-r * 0.3, -r * 0.2, r * 0.6, r * 0.34, r * 0.06);
  DrawShapeOutlined(C, Style, AGHex($8C94A8), Ol, 0.8);
  C.Pop;
  // highlight
  C.Ellipse(cx - r * 0.38, cy - r * 0.42, r * 0.24, r * 0.15);
  C.Fill(AGWithAlpha(AGWhite, 170));
  If Lit Then Begin
    s := r * 0.42 * (0.75 + 0.5 * Phase);
    Glow(C, ex, ey, s * 2.2, AGHex($FFC040), 110);
    StarPoly(C, ex, ey, s, s * 0.42, 7, rot);
    C.Fill(AGHex($FF9020));
    StarPoly(C, ex, ey, s * 0.6, s * 0.28, 7, rot + 0.3);
    C.Fill(AGHex($FFF0A0));
  End;
End;

Procedure DrawTrophy(C: TAGCanvas; Style: TAGStyle; x, yb: Double);
Const
  Gold: Cardinal = $F0C030;
Begin
  // handles
  C.BeginPath;
  C.MoveTo(x - 14, yb - 40);
  C.QuadTo(x - 28, yb - 40, x - 13, yb - 25);
  C.Stroke(5, Ol);
  C.BeginPath;
  C.MoveTo(x + 14, yb - 40);
  C.QuadTo(x + 28, yb - 40, x + 13, yb - 25);
  C.Stroke(5, Ol);
  C.BeginPath;
  C.MoveTo(x - 14, yb - 40);
  C.QuadTo(x - 28, yb - 40, x - 13, yb - 25);
  C.Stroke(2.4, AGHex(Gold));
  C.BeginPath;
  C.MoveTo(x + 14, yb - 40);
  C.QuadTo(x + 28, yb - 40, x + 13, yb - 25);
  C.Stroke(2.4, AGHex(Gold));
  C.Rect(x - 3, yb - 18, 6, 12);
  DrawShapeOutlined(C, Style, AGHex($D8A020), Ol);
  C.RoundRect(x - 12, yb - 8, 24, 8, 2);
  DrawShapeOutlined(C, Style, AGHex($A07020), Ol);
  C.BeginPath;
  C.MoveTo(x - 17, yb - 46);
  C.LineTo(x + 17, yb - 46);
  C.QuadTo(x + 16, yb - 18, x, yb - 17);
  C.QuadTo(x - 16, yb - 18, x - 17, yb - 46);
  C.ClosePath;
  DrawShapeOutlined(C, Style, AGHex(Gold), Ol);
  StarPoly(C, x, yb - 33, 6, 2.6, 5, -AG_HALF_PI);
  C.Fill(AGHex($FFF4C0));
End;

Type
  TFigureProp = (fpNone, fpTrophy, fpBomb);

(*
 * Own bomber figure: helmet with face plate and antenna, suit in Col.
 * (x, y) = between the feet, s = scale (1 = about 175 pixel high).
 * ArmL / ArmR in degree, 0 = hanging down, 180 = straight up.
 *)
Procedure DrawFigure(C: TAGCanvas; Style: TAGStyle; x, y, s: Double; Col: TAGColor;
  ArmL, ArmR: Double; Prop: TFigureProp; Mirror: Boolean; Smile: Boolean = true; Phase: Double = 0.5);
Var
  dark: TAGColor;

  Procedure Arm(side: Integer; a: Double);
  Var
    sx, sy, hx, hy, rad: Double;
  Begin
    sx := side * 21;
    sy := -89;
    rad := AGDeg(a);
    hx := sx + side * AGSin(rad) * 33;
    hy := sy + AGCos(rad) * 33;
    C.Capsule(sx, sy, hx, hy, 7.5);
    DrawShapeOutlined(C, Style, AGShade(Col, 0.88), Ol);
    If side = 1 Then Begin
      If Prop = fpTrophy Then DrawTrophy(C, Style, hx, hy + 2);
      If Prop = fpBomb Then DrawBomb(C, Style, hx + 3, hy - 21, 19, Phase, 0.4);
    End;
    C.Circle(hx, hy, 9.5);
    DrawShapeOutlined(C, Style, AGHex($F2F2F4), Ol);
  End;

Begin
  dark := AGShade(Col, 0.68);
  C.Push;
  C.Translate(x, y);
  If Mirror Then C.Scale(-s, s) Else C.Scale(s, s);
  // legs and boots
  C.Capsule(-11, -52, -14, -13, 8.5);
  DrawShapeOutlined(C, Style, dark, Ol);
  C.Capsule(11, -52, 14, -13, 8.5);
  DrawShapeOutlined(C, Style, dark, Ol);
  C.Ellipse(-17, -8, 14, 8.5);
  DrawShapeOutlined(C, Style, AGHex($30303C), Ol);
  C.Ellipse(17, -8, 14, 8.5);
  DrawShapeOutlined(C, Style, AGHex($30303C), Ol);
  // body
  C.RoundRect(-25, -99, 50, 52, 15);
  DrawShapeOutlined(C, Style, Col, Ol);
  C.Rect(-25, -60, 50, 8);
  DrawShapeOutlined(C, Style, AGHex($343444), Ol, 0.6);
  C.Rect(-5, -61.5, 10, 11);
  DrawShapeOutlined(C, Style, AGHex($E8B830), Ol, 0.6);
  C.Circle(0, -80, 9);
  DrawShapeOutlined(C, Style, AGHex($F4F0E8), Ol, 0.7);
  C.Circle(0, -80, 4.2);
  C.Fill(AGHex($E04830));
  // arms
  Arm(-1, ArmL);
  Arm(1, ArmR);
  // antenna
  C.BeginPath;
  C.MoveTo(0, -158);
  C.LineTo(3, -172);
  C.Stroke(3.2, Ol);
  C.Circle(3.5, -175, 6);
  DrawShapeOutlined(C, Style, AGHex($FF7038), Ol);
  // head
  C.Circle(0, -130, 31);
  DrawShapeOutlined(C, Style, Col, Ol);
  C.Ellipse(0, -125, 22.5, 16.5);
  DrawShapeOutlined(C, Style, AGHex($FFF0DA), Ol, 0.8);
  C.Ellipse(-8, -128, 3.8, 6.4);
  C.Ellipse(8, -128, 3.8, 6.4);
  C.Fill(AGHex($181820));
  C.Circle(-6.8, -130.5, 1.4);
  C.Circle(9.2, -130.5, 1.4);
  C.Fill(AGWhite);
  C.BeginPath;
  If Smile Then Begin
    C.MoveTo(-6.5, -117);
    C.QuadTo(0, -110, 6.5, -117);
  End
  Else Begin
    C.MoveTo(-5, -114);
    C.QuadTo(0, -116, 5, -114);
  End;
  C.Stroke(2.2, AGHex($402020));
  C.Pop;
End;

Procedure Podium(C: TAGCanvas; Style: TAGStyle; x, y, w, h: Double; Const Txt: String);
Begin
  C.Rect(x, y + 8, w, h);
  DrawShapeOutlined(C, Style, AGHex($6A5888), Ol);
  C.RoundRect(x - 6, y, w + 12, 12, 3);
  DrawShapeOutlined(C, Style, AGHex($E8C050), Ol);
  If Txt <> '' Then
    DrawTextStyled(C, Style, x + w / 2, y + 22, 28, Txt, AGHex($FFE070), Ol, faCenter, 0.2);
End;

(* Night sky with stars, base of most screens *)
Procedure NightSky(C: TAGCanvas; Style: TAGStyle; Var R: TAGRandom; Hue: Double; StarCount: Integer);
Begin
  VGradient(C, Style, 0, 0, 640, 480, [AGHSV(Hue, 0.7, 0.07), AGHSV(Hue + 15, 0.6, 0.18), AGHSV(Hue + 35, 0.55, 0.3)]);
  Stars(C, R, StarCount, 0, 0, 640, 480);
End;

Procedure GenerateScreens(Ctx: TAGContext; Output: TAGOutput);
Var
  area: TAGArea;
  Style: TAGStyle;
  hue: Double;
  img: TAGImage;
  winner, k: Integer;
  hero: Integer;
  drawA, drawB: Integer;

  Procedure Title(C: TAGCanvas; cx, y, Size: Double; Const Txt: String);
  Begin
    DrawTextStyled(C, Style, cx, y, Size, Txt, AGHex($FFC838), AGHex($501808), faCenter, 0.19, -1, 0.12, 4);
  End;

  Procedure DrawMainMenu(C: TAGCanvas; Frame: Integer);
  Const
    Labels: Array[0..6] Of String = ('Start Game', 'Start Network Game', 'Join Network Game', 'Options',
      'About', 'Online Manual', 'Exit');
  Var
    R: TAGRandom;
    i: Integer;
    sz, a, px, py: Double;
  Begin
    R := area.Sub('mainmenu').Stream('stars');
    NightSky(C, Style, R, hue, 170);
    // left: key art (launcher portrait)
    Glow(C, 165, 300, 210, AGHSV(hue + 50, 0.6, 0.9), 90);
    C.Ellipse(160, 540, 300, 95);
    C.Fill(AGHSV(hue + 10, 0.5, 0.12));
    // spark trail from the fuse to the title
    For i := 0 To 26 Do Begin
      a := i / 26;
      px := 248 + 50 * a + 14 * AGSin(a * 9);
      py := 232 - 128 * a;
      C.Circle(px, py, 2.6 * (1 - a) + 0.8);
      C.Fill(AGWithAlpha(AGHex($FFD060), AGRound(230 - 150 * a)));
    End;
    DrawFigure(C, Style, 140, 462, 1.62, FigureColor(hero), 125, 132, fpBomb, false, true, 0.8);
    DrawTextStyled(C, Style, 160, 24, 30, 'FPC', AGHex($E8F0FF), AGHex($182040), faCenter, 0.2, -1, 0.12, 3);
    Title(C, 160, 64, 58, 'ATOMIC');
    // right: menu
    C.RoundRect(338, 86, 284, 7 * 37 + 28, 14);
    C.Fill(AGWithAlpha(AGHSV(hue, 0.5, 0.05), 170), true);
    C.Stroke(2, AGWithAlpha(AGHSV(hue + 30, 0.4, 0.8), 160));
    For i := 0 To 6 Do Begin
      sz := 18;
      While TextWidth(Labels[i], sz, 0.17) > 245 Do sz := sz - 0.5;
      DrawTextStyled(C, Style, 356, 120 + i * 37 - sz / 2, sz, Labels[i], AGHex($FFF0C8), AGHex($281808), faLeft, 0.17, -1, 0, 2);
    End;
  End;

  Procedure DrawOptions(C: TAGCanvas; Frame: Integer);
  Var
    R: TAGRandom;
    i: Integer;
  Begin
    R := area.Sub('options').Stream('stars');
    VGradient(C, Style, 0, 0, 640, 480, [AGHSV(hue - 30, 0.6, 0.09), AGHSV(hue - 20, 0.55, 0.2)]);
    For i := -6 To 16 Do Begin
      C.Polygon([i * 60, 480, i * 60 + 30, 480, i * 60 + 330, 0, i * 60 + 300, 0]);
      C.Fill(AGWithAlpha(AGWhite, 8));
    End;
    Stars(C, R, 40, 480, 0, 160, 480);
    C.RoundRect(10, 16, 470, 380, 14);
    C.Fill(AGWithAlpha(AGBlack, 90), true);
    C.Stroke(2, AGWithAlpha(AGHSV(hue, 0.4, 0.8), 110));
    Glow(C, 560, 390, 110, AGHex($FFB040), 70);
    DrawBomb(C, Style, 548, 400, 40, 0.8, 0.2);
    C.Push;
    C.Translate(572, 330);
    C.Rotate(-AG_HALF_PI);
    DrawTextStyled(C, Style, 0, 0, 44, 'OPTIONS', AGHex($FFC838), AGHex($501808), faLeft, 0.19, -1, 0.12, 3);
    C.Pop;
  End;

  Procedure Burst(C: TAGCanvas; cx, cy, r, rot: Double; Col: Integer);
  Begin
    StarPoly(C, cx, cy, r, r * 0.62, 12, rot);
    DrawShapeOutlined(C, Style, AGHex($F0B020), AGHex($A02010), 1.6);
    StarPoly(C, cx, cy, r * 0.8, r * 0.5, 12, rot + 0.13);
    C.Fill(AGHex($FFD848));
    C.Push;
    C.Translate(cx, cy + r * 0.48);
    C.Scale(r / 150, r / 150);
    DrawFigure(C, Style, 0, 0, 1, FigureColor(Col), 30, 160, fpNone, false);
    C.Pop;
  End;

  Procedure DrawJoin(C: TAGCanvas; Frame: Integer);
  Var
    R: TAGRandom;
  Begin
    R := area.Sub('join').Stream('stars');
    NightSky(C, Style, R, hue + 20, 110);
    C.RoundRect(8, 8, 624, 464, 16);
    C.Stroke(3, AGWithAlpha(AGHSV(hue + 40, 0.5, 0.8), 120));
    Burst(C, 585, 75, 52, 0.1, area.Pick('join1', 10));
    Burst(C, 590, 240, 44, 0.3, area.Pick('join2', 10));
    Burst(C, 582, 400, 54, 0.2, area.Pick('join3', 10));
  End;

  Procedure DrawResults(C: TAGCanvas; Frame: Integer);
  Var
    R: TAGRandom;
  Begin
    R := area.Sub('results').Stream('stars');
    VGradient(C, Style, 0, 0, 640, 480, [AGHSV(hue + 30, 0.7, 0.16), AGHSV(hue + 45, 0.65, 0.3), AGHSV(hue + 30, 0.7, 0.12)]);
    Stars(C, R, 220, 0, 0, 640, 480);
    Glow(C, 320, 45, 240, AGHex($FFB060), 70);
    Title(C, 320, 20, 50, 'RESULTS');
    DrawFigure(C, Style, 55, 470, 0.5, FigureColor(area.Pick('res1', 10)), 30, 150, fpNone, false);
    DrawFigure(C, Style, 590, 470, 0.5, FigureColor(area.Pick('res2', 10)), 150, 30, fpNone, true);
    DrawFigure(C, Style, 55, 95, 0.42, FigureColor(area.Pick('res3', 10)), 20, 20, fpNone, false);
    DrawFigure(C, Style, 590, 95, 0.42, FigureColor(area.Pick('res4', 10)), 20, 20, fpNone, true);
  End;

  Procedure DrawFieldSetup(C: TAGCanvas; Frame: Integer);
  Var
    R: TAGRandom;
  Begin
    R := area.Sub('fieldsetup').Stream('stars');
    VGradient(C, Style, 0, 0, 640, 480, [AGHSV(hue - 20, 0.5, 0.1), AGHSV(hue - 10, 0.45, 0.24)]);
    Stars(C, R, 60, 0, 0, 640, 480);
    DrawTextStyled(C, Style, 24, 26, 36, 'FIELD SETUP', AGHex($FFC838), AGHex($501808), faLeft, 0.19, -1, 0.12, 3);
    C.RoundRect(12, 150, 350, 90, 12);
    C.Fill(AGWithAlpha(AGBlack, 100), true);
    C.Stroke(2, AGWithAlpha(AGHSV(hue, 0.3, 0.8), 110));
    // frame around the preview window
    C.RoundRect(366, 68, 247, 226, 10);
    DrawShapeOutlined(C, Style, AGHex($8890A8), Ol);
    C.Rect(376, 78, 227, 206);
    C.Fill(AGHex($202430));
    C.Circle(373, 75, 3);
    C.Circle(606, 75, 3);
    C.Circle(373, 287, 3);
    C.Circle(606, 287, 3);
    C.Fill(AGHex($404858));
    DrawTextStyled(C, Style, 489, 300, 16, 'Preview', AGHex($D8E0F0), Ol, faCenter, 0.17);
    Glow(C, 545, 400, 90, AGHex($FFB040), 60);
    DrawBomb(C, Style, 535, 405, 38, 0.6, 0.5);
  End;

  Procedure VictoryBase(C: TAGCanvas; Const Key: String; RayCol: TAGColor);
  Var
    R: TAGRandom;
  Begin
    R := area.Sub(Key).Stream('stars');
    NightSky(C, Style, R, hue, 150);
    Rays(C, 320, 320, 18, 560, RayCol, 34, area.Sub(Key).Rnd('rays'));
    Glow(C, 320, 300, 220, RayCol, 80);
    C.Ellipse(320, 530, 430, 115);
    C.Fill(AGHSV(hue + 20, 0.45, 0.13));
    C.Ellipse(320, 445, 170, 26);
    C.Fill(AGWithAlpha(AGWhite, 22));
  End;

  Procedure DrawVictory(C: TAGCanvas; Frame: Integer);
  Var
    R: TAGRandom;
    col: TAGColor;
  Begin
    col := FigureColor(winner);
    VictoryBase(C, 'victory' + IntToStr(winner), AGMix(col, AGWhite, 0.35));
    Podium(C, Style, 255, 378, 130, 110, '1');
    DrawFigure(C, Style, 320, 380, 1.36, col, 160, 150, fpTrophy, false);
    R := area.Sub('victory' + IntToStr(winner)).Stream('confetti');
    Confetti(C, R, 70, 10, 110, 620, 330);
    Title(C, 320, 22, 70, 'VICTORY');
  End;

  Procedure DrawTeam(C: TAGCanvas; Frame: Integer);
  Var
    R: TAGRandom;
    col: TAGColor;
  Begin
    If winner = 0 Then col := FigureColor(0) Else col := FigureColor(2);
    VictoryBase(C, 'team' + IntToStr(winner), AGMix(col, AGWhite, 0.4));
    Podium(C, Style, 120, 410, 110, 80, '');
    Podium(C, Style, 410, 410, 110, 80, '');
    Podium(C, Style, 260, 372, 120, 120, '1');
    DrawFigure(C, Style, 175, 412, 0.92, col, 40, 165, fpNone, false);
    DrawFigure(C, Style, 465, 412, 0.92, col, 40, 165, fpNone, true);
    DrawFigure(C, Style, 320, 374, 1.05, col, 160, 150, fpTrophy, false);
    R := area.Sub('team' + IntToStr(winner)).Stream('confetti');
    Confetti(C, R, 60, 10, 140, 620, 300);
    Title(C, 320, 16, 62, 'VICTORY');
    If winner = 0 Then
      DrawTextStyled(C, Style, 320, 96, 26, 'White Team', AGHex($F4F4F8), Ol, faCenter, 0.19, -1, 0, 3)
    Else
      DrawTextStyled(C, Style, 320, 96, 26, 'Red Team', AGHex($FF5040), Ol, faCenter, 0.19, -1, 0, 3);
  End;

  Procedure DrawDraw(C: TAGCanvas; Frame: Integer);
  Var
    R: TAGRandom;
    i: Integer;
  Begin
    R := area.Sub('draw').Stream('stars');
    NightSky(C, Style, R, hue + 15, 160);
    C.Ellipse(320, 540, 440, 120);
    C.Fill(AGHSV(hue + 20, 0.45, 0.13));
    // smoke of a bomb that did not go off
    For i := 0 To 5 Do Begin
      C.Circle(340 + 12 * AGSin(i * 1.7), 380 - i * 18, 9 + i * 3);
      C.Fill(AGWithAlpha(AGHex($B0B0C0), 120 - i * 16));
    End;
    DrawBomb(C, Style, 320, 410, 32, 0, 0, false);
    DrawFigure(C, Style, 175, 455, 1.25, FigureColor(drawA), 70, 100, fpNone, false, false);
    DrawFigure(C, Style, 465, 455, 1.25, FigureColor(drawB), 70, 100, fpNone, true, false);
    Title(C, 320, 26, 64, 'DRAW GAME');
  End;

  Procedure DrawLoadDialog(C: TAGCanvas; Frame: Integer);
  Begin
    // plain fills only: edges and center are tiled, a gradient would show seams
    C.Rect(0, 0, 72, 72);
    C.Fill(AGBlack);
    C.RoundRect(0, 0, 72, 72, 10);
    C.Fill(AGHSV(hue, 0.6, 0.12));
    C.RoundRect(2, 2, 68, 68, 8);
    C.Fill(AGHSV(hue, 0.45, 0.85));
    C.RoundRect(4, 4, 64, 64, 6);
    C.Fill(AGHSV(hue, 0.55, 0.6));
    C.RoundRect(8, 8, 56, 56, 4);
    C.Fill(AGHSV(hue, 0.6, 0.08));
    C.RoundRect(10, 10, 52, 52, 3);
    C.Fill(AGHSV(hue, 0.55, 0.17));
  End;

  Procedure DrawOptionsCursor(C: TAGCanvas; Frame: Integer);
  Begin
    C.Polygon([3, 10.5, 15, 10.5, 15, 3.5, 29, 16, 15, 28.5, 15, 21.5, 3, 21.5]);
    DrawShapeOutlined(C, Style, AGHex($FFA828), AGHex($301808), 1.2);
    C.Polygon([5, 12.5, 17, 12.5, 17, 8, 22, 12.5]);
    C.Fill(AGWithAlpha(AGHex($FFF0B0), 200));
  End;

  Procedure DrawCursorFrame(C: TAGCanvas; Frame: Integer);
  Var
    t, ph, r: Double;
    k: Integer;
    Rs: TAGRandom;
  Begin
    t := AG_TWO_PI * Frame / 18;
    ph := 0.5 + 0.5 * AGSin(t * 2);
    r := 12.5 + 0.7 * AGSin(t);
    DrawBomb(C, Style, 17, 24, r, ph, t * 1.5, true, $5070B0);
    // a few flying sparks
    Rs := area.Sub('cursor').Stream('sparks' + IntToStr(Frame));
    For k := 0 To 2 Do Begin
      C.Circle(31 + Rs.Range(-6, 6), 7 + Rs.Range(-6, 4), 1.1);
      C.Fill(AGHex($FFE080));
    End;
  End;

  Procedure DrawBigFont(C: TAGCanvas; Frame: Integer);
  Const
    Size = 19;
    Weight = 0.18;
  Var
    s: String;
    x, y, pen, ow: Double;
  Begin
    If Frame = 10 Then s := ':' Else s := chr(ord('0') + Frame);
    pen := Size * Weight;
    If Style = asRetro Then ow := 3 Else If Style = asPixel Then ow := 2 Else ow := 1.4;
    x := 9 - TextWidth(s, Size, Weight) / 2;
    y := (27 - Size - pen - 2 * ow) / 2 + pen / 2 + ow;
    StrokeText(C, x, y, Size, s, pen + 2 * ow, AGHex($200C04), Weight, 0);
    If Style = asSoft Then Begin
      StrokeText(C, x, y, Size, s, pen, AGHex($E07818), Weight, 0);
      StrokeText(C, x, y, Size, s, pen * 0.55, AGHex($FFC040), Weight, 0, -0.3, -0.4);
    End
    Else
      StrokeText(C, x, y, Size, s, pen, AGHex($FFB028), Weight, 0);
  End;

  Procedure Save(Const Name: String; Mode: TAGSaveMode);
  Begin
    Output.SaveImage('res/' + Name, img, Mode);
    FreeAndNil(img);
  End;

Var
  x, y: Integer;
Begin
  Style := Ctx.Style;
  area := Ctx.Area('screens');
  hue := area.Vary('hue', 252, 22);
  hero := area.Pick('hero', 6);
  Case hero Of
    0: hero := 0;
    1: hero := 2;
    2: hero := 3;
    3: hero := 5;
    4: hero := 8;
  Else
    hero := 6;
  End;
  drawA := area.Pick('drawa', 10);
  drawB := (drawA + 1 + area.Pick('drawb', 9)) Mod 10;

  img := RenderImage(Ctx, 640, 480, @DrawMainMenu);
  Save('mainmenu.png', smOpaque);
  img := RenderImage(Ctx, 640, 480, @DrawOptions);
  Save('options.png', smOpaque);
  img := RenderImage(Ctx, 640, 480, @DrawJoin);
  Save('join.png', smOpaque);
  img := RenderImage(Ctx, 640, 480, @DrawResults);
  Save('results.png', smOpaque);
  img := RenderImage(Ctx, 640, 480, @DrawFieldSetup);
  For y := 82 To 279 Do
    For x := 380 To 598 Do
      img.Pixels[y * 640 + x] := AGTransparent;
  Save('fieldsetup.png', smKeyed);
  For k := 0 To 9 Do Begin
    winner := k;
    img := RenderImage(Ctx, 640, 480, @DrawVictory);
    Save('victory' + IntToStr(k) + '.png', smOpaque);
  End;
  For k := 0 To 1 Do Begin
    winner := k;
    img := RenderImage(Ctx, 640, 480, @DrawTeam);
    Save('team' + IntToStr(k) + '.png', smOpaque);
  End;
  img := RenderImage(Ctx, 640, 480, @DrawDraw);
  Save('draw.png', smOpaque);
  img := RenderImage(Ctx, 72, 72, @DrawLoadDialog);
  Save('loaddialog.png', smOpaque);
  img := RenderImage(Ctx, 32, 32, @DrawOptionsCursor);
  Save('options_cursor.png', smKeyed);
  img := RenderSheet(Ctx, 4, 3, 18, 27, 11, @DrawBigFont);
  Save('bigfont.png', smKeyed);
  img := RenderSheet(Ctx, 5, 4, 40, 40, 18, @DrawCursorFrame);
  Try
    Output.SaveAni('res/mainmenu_cursor.ani', img, AniSpec(0, 40, 5, 4, 40, 40, [0], [18]));
  Finally
    FreeAndNil(img);
  End;
End;

Initialization
  Ol := AGHex($14121C);
  RegisterGenerator('screens', 'Menu screens, cursors, big font, dialog (res/)', 22, @GenerateScreens);

End.
