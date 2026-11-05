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
 * Maps (agent D): maps/FieldXX/{field.png, brick.png, solid.png, xbrick.ani}
 *
 * Every map has a fixed theme (by its folder, see Themes), the theme decides
 * palette, ground, frame and block shapes. The seed (area 'map:FieldXX') only
 * varies details: tints, positions of stones, tufts, cracks, ...
 *
 * Layout of field.png (from units/uatomic_common.pas and client/ugame.pas):
 * - play field: 15 x 11 cells of 40 x 36 pixel, top left at (20, 66), so the
 *   ground covers x 20..619, y 66..461 and is drawn cell aligned
 * - HUD on top (y < 46): the engine writes the player scores at
 *   x = 10 + 100 * (i div 2), y = 10 / 26 and the big timer at (500, 10),
 *   so this is a dark panel
 * - the frame around the play field is decoration, the engine draws the
 *   "open border" arrows on it
 * - overlays (arrows, conveyors, holes, trampolines) are drawn on top of empty
 *   cells, the ground therefore has no big features inside the play field
 *)
Unit uag_gen_world;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Uses uag_image, uag_context, uag_output;

Const
  MapCount = 11;

(* All files of the map maps/Field<Index>/ *)
Procedure GenerateMap(Ctx: TAGContext; Output: TAGOutput; Index: Integer);

(* Single images of a map (as rendered, not keyed yet), the caller frees them *)
Function RenderMapField(Ctx: TAGContext; Index: Integer): TAGImage;
Function RenderMapBrick(Ctx: TAGContext; Index: Integer): TAGImage;
Function RenderMapSolid(Ctx: TAGContext; Index: Integer): TAGImage;

Function MapThemeName(Index: Integer): String; // e.g. 'ice'

Implementation

Uses SysUtils, uag_canvas, uag_render, uag_registry, uag_math, uag_random;

Const
  FX = 20; // left edge of the play field
  FY = 66; // top edge of the play field
  CW = 40; // cell size
  CH = 36;
  GW = 15; // cells
  GH = 11;
  FR = FX + GW * CW; // 620
  FB = FY + GH * CH; // 462

Type
  TTheme = (thMeadow, thClassic, thIce, thEgypt, thMine, thBeach, thAlien, thHaunted, thWater, thForest, thCity);

  (* Shape of the xbrick.ani of the original map (kept identical) *)
  TXBrickLayout = Record
    Cols, Rows, Count: Integer;
  End;

  TMap = Record
    Index: Integer;
    Theme: TTheme;
    A: TAGArea;
    Style: TAGStyle;
    PS: Integer; // pixel scale of the style
    Hue: Double; // small hue shift in degree
    Light: Double; // brightness factor
  End;

Const
  Themes: Array[0..MapCount - 1] Of TTheme = (
    thMeadow, // Green Acres
    thClassic, // Classic Green Acres
    thIce, // The Hockey Rink
    thEgypt, // Ancient Egypt
    thMine, // The Coal Mine
    thBeach, // The Beach
    thAlien, // Aliens
    thHaunted, // Haunted House
    thWater, // Under the Ocean
    thForest, // Deep Forest Green
    thCity // Inner City Trash
    );

  XLayouts: Array[0..MapCount - 1] Of TXBrickLayout = (
    (Cols: 3; Rows: 3; Count: 9),
    (Cols: 3; Rows: 3; Count: 9),
    (Cols: 2; Rows: 4; Count: 8),
    (Cols: 3; Rows: 3; Count: 9),
    (Cols: 2; Rows: 5; Count: 10),
    (Cols: 3; Rows: 3; Count: 9),
    (Cols: 2; Rows: 5; Count: 10),
    (Cols: 3; Rows: 3; Count: 9),
    (Cols: 2; Rows: 5; Count: 10),
    (Cols: 2; Rows: 5; Count: 10),
    (Cols: 2; Rows: 5; Count: 10)
    );

  (* ---------------------------------------------------------------------- *)
  (* Color helpers                                                          *)
  (* ---------------------------------------------------------------------- *)

Procedure ToHSV(c: TAGColor; Out h, s, v: Double);
Var
  r, g, b, mx, mn, d: Double;
Begin
  r := c.r / 255;
  g := c.g / 255;
  b := c.b / 255;
  mx := AGMax(r, AGMax(g, b));
  mn := AGMin(r, AGMin(g, b));
  d := mx - mn;
  v := mx;
  If mx > 0 Then s := d / mx Else s := 0;
  If d = 0 Then
    h := 0
  Else If mx = r Then Begin
    h := 60 * ((g - b) / d);
    If h < 0 Then h := h + 360;
  End
  Else If mx = g Then
    h := 60 * ((b - r) / d + 2)
  Else
    h := 60 * ((r - g) / d + 4);
End;

(* Hue shift in degree, saturation and value factors *)
Function Jit(c: TAGColor; dh, fs, fv: Double): TAGColor;
Var
  h, s, v: Double;
  a: Byte;
Begin
  a := c.a;
  ToHSV(c, h, s, v);
  h := h + dh;
  While h < 0 Do h := h + 360;
  While h >= 360 Do h := h - 360;
  result := AGHSV(h, AGClamp(s * fs, 0, 1), AGClamp(v * fv, 0, 1));
  result.a := a;
End;

(* Theme color with the variation of the map *)
Function TC(Const M: TMap; Hex: Cardinal): TAGColor;
Begin
  result := Jit(AGHex(Hex), M.Hue, 1, M.Light);
End;

Function TCA(Const M: TMap; Hex: Cardinal; Alpha: Integer): TAGColor;
Begin
  result := AGWithAlpha(TC(M, Hex), Alpha);
End;

(* ---------------------------------------------------------------------- *)
(* Drawing helpers                                                        *)
(* ---------------------------------------------------------------------- *)

(* Minimum pen width, thin lines get gaps in the pixel style *)
Function Pen(Const M: TMap; w: Double): Double;
Begin
  If (M.PS > 1) And (w < GetStyleInfo(M.Style).MinPen) Then result := GetStyleInfo(M.Style).MinPen Else result := w;
End;

Procedure Line(C: TAGCanvas; x0, y0, x1, y1, w: Double; Col: TAGColor);
Begin
  C.BeginPath;
  C.MoveTo(x0, y0);
  C.LineTo(x1, y1);
  C.Stroke(w, Col);
End;

Procedure Box(C: TAGCanvas; x, y, w, h: Double; Col: TAGColor);
Begin
  C.Rect(x, y, w, h);
  C.Fill(Col);
End;

Procedure Disc(C: TAGCanvas; x, y, r: Double; Col: TAGColor);
Begin
  C.Circle(x, y, r);
  C.Fill(Col);
End;

Procedure Oval(C: TAGCanvas; x, y, rx, ry: Double; Col: TAGColor);
Begin
  C.Ellipse(x, y, rx, ry);
  C.Fill(Col);
End;

(* Clip to a rectangle given in pixels of the final image *)
Procedure ClipTo(C: TAGCanvas; Const M: TMap; x0, y0, x1, y1: Integer);
Begin
  C.SetClip(x0 Div M.PS, y0 Div M.PS, (x1 + M.PS - 1) Div M.PS, (y1 + M.PS - 1) Div M.PS);
End;

(* Fills the current path with a vertical gradient in the soft style, else solid *)
Procedure FillV(C: TAGCanvas; Const M: TMap; y0, y1: Double; Top, Bottom: TAGColor);
Var
  p: TAGPaint;
Begin
  If M.Style = asSoft Then Begin
    p := C.Linear(0, y0, 0, y1, [Top, Bottom]);
    C.FillPaint(p);
  End
  Else
    C.Fill(AGMix(Top, Bottom, 0.5));
End;

(* Irregular round polygon (stone, bush, coal lump) *)
Procedure Lump(C: TAGCanvas; Var R: TAGRandom; cx, cy, rx, ry, Rough: Double; n: Integer);
Var
  i: Integer;
  a, f: Double;
  pts: Array Of Double;
Begin
  pts := Nil;
  setlength(pts, 2 * n);
  For i := 0 To n - 1 Do Begin
    a := AG_TWO_PI * i / n;
    f := 1 - Rough + 2 * Rough * R.NextUnit;
    pts[2 * i] := cx + rx * f * AGCos(a);
    pts[2 * i + 1] := cy + ry * f * AGSin(a);
  End;
  C.BeginPath;
  C.MoveTo(pts[0], pts[1]);
  For i := 1 To n - 1 Do
    C.QuadTo(pts[2 * i], pts[2 * i + 1], (pts[2 * i] + pts[(2 * i + 2) Mod (2 * n)]) / 2, (pts[2 * i + 1] + pts[(2 * i + 3) Mod (2 * n)]) / 2);
  C.QuadTo(pts[0], pts[1], (pts[0] + pts[2]) / 2, (pts[1] + pts[3]) / 2);
  C.ClosePath;
End;

(* Shape with the style: flat / pixel = fill + outline, soft = gradient *)
Procedure Shape(C: TAGCanvas; Const M: TMap; Col: TAGColor; Outline: Double = 1.0);
Begin
  DrawShape(C, M.Style, Col, Outline);
End;

Procedure ShapeO(C: TAGCanvas; Const M: TMap; Col, OCol: TAGColor; Outline: Double = 1.0);
Begin
  DrawShapeOutlined(C, M.Style, Col, OCol, Outline);
End;

(* Inside of a rectangle in the band around the play field? *)
Function InFrame(x, y: Double): Boolean;
Begin
  result := (y >= 46) And ((x < FX) Or (x >= FR) Or (y < FY) Or (y >= FB));
End;

(* Calls Proc for positions along the frame band (step s), used for stones etc. *)
Type
  TBandProc = Procedure(x, y: Double; Index: Integer) Is Nested;

Procedure ForBand(s: Double; Proc: TBandProc);
Var
  x, y: Double;
  i: Integer;
Begin
  i := 0;
  // top and bottom band
  x := s / 2;
  While x < 640 Do Begin
    Proc(x, (46 + FY) / 2, i);
    inc(i);
    Proc(x, (FB + 480) / 2, i);
    inc(i);
    x := x + s;
  End;
  // left and right band
  y := FY + s / 2;
  While y < FB Do Begin
    Proc(FX / 2, y, i);
    inc(i);
    Proc((FR + 640) / 2, y, i);
    inc(i);
    y := y + s;
  End;
End;

(* HUD panel at the top, the engine writes the scores and the time on it *)
Procedure DrawHud(C: TAGCanvas; Const M: TMap; Rim, Inner: TAGColor);
Begin
  C.RoundRect(2, 2, 636, 42, 7);
  Shape(C, M, Rim, 1.0);
  C.RoundRect(6, 5, 628, 36, 4);
  C.Fill(AGShade(Rim, 0.35));
  C.RoundRect(7, 6, 626, 34, 3);
  C.Fill(Inner);
  // separators between the 5 player columns and the timer
  Line(C, 105, 9, 105, 37, Pen(M, 1), AGMix(Inner, Rim, 0.35));
  Line(C, 205, 9, 205, 37, Pen(M, 1), AGMix(Inner, Rim, 0.35));
  Line(C, 305, 9, 305, 37, Pen(M, 1), AGMix(Inner, Rim, 0.35));
  Line(C, 405, 9, 405, 37, Pen(M, 1), AGMix(Inner, Rim, 0.35));
  Line(C, 500, 9, 500, 37, Pen(M, 1), AGMix(Inner, Rim, 0.35));
End;

(* Dark edge inside the play field, gives the ground some depth *)
Procedure InnerShadow(C: TAGCanvas; Const M: TMap; Strength: Integer);
Var
  i: Integer;
Begin
  For i := 0 To 3 Do Begin
    C.Rect(FX, FY + i, FR - FX, 1);
    C.Rect(FX + i, FY + 1 + i, 1, FB - FY - 1 - i);
    C.Fill(AGRGBA(0, 0, 0, Strength * (4 - i) Div 4));
  End;
End;

(* Thin frame line around the play field *)
Procedure FieldRim(C: TAGCanvas; Const M: TMap; Col: TAGColor; w: Double);
Begin
  C.BeginPath;
  C.MoveTo(FX - w / 2, FY - w / 2);
  C.LineTo(FR + w / 2, FY - w / 2);
  C.LineTo(FR + w / 2, FB + w / 2);
  C.LineTo(FX - w / 2, FB + w / 2);
  C.ClosePath;
  C.Stroke(Pen(M, w), Col);
End;

(* Soft light / vignette over the whole play field (only soft style) *)
Procedure SoftLight(C: TAGCanvas; Const M: TMap; cx, cy, r: Double; Light, Dark: TAGColor);
Var
  p: TAGPaint;
Begin
  If M.Style <> asSoft Then exit;
  C.Rect(FX, FY, FR - FX, FB - FY);
  p := C.Radial(cx, cy, r, [Light, AGWithAlpha(Light, 0), Dark]);
  C.FillPaint(p);
End;

(* ---------------------------------------------------------------------- *)
(* Frames (band around the play field + HUD)                             *)
(* ---------------------------------------------------------------------- *)

Procedure FrameStones(C: TAGCanvas; Const M: TMap; Mortar, Stone: TAGColor; Size: Double);
Var
  R: TAGRandom;

  Procedure P(x, y: Double; Index: Integer);
  Var
    k: Integer;
    cx, cy: Double;
  Begin
    For k := 0 To 1 Do Begin
      cx := x + (R.NextUnit - 0.5) * Size * 0.5 + k * Size * 0.5 - Size * 0.25;
      cy := y + (R.NextUnit - 0.5) * Size * 0.6;
      Lump(C, R, cx, cy, Size * (0.42 + 0.15 * R.NextUnit), Size * (0.36 + 0.12 * R.NextUnit), 0.18, 7);
      Shape(C, M, Jit(Stone, 0, 1, 0.82 + 0.3 * R.NextUnit), 0.8);
    End;
  End;

Begin
  R := M.A.Stream('framestones');
  Box(C, 0, 44, 640, 436, Mortar);
  ForBand(Size, @P);
End;

Procedure FrameTiles(C: TAGCanvas; Const M: TMap; Joint, Tile: TAGColor; Size: Double);
Var
  x, y: Double;
  R: TAGRandom;

  Procedure T(tx, ty: Double);
  Var
    col: TAGColor;
  Begin
    col := Jit(Tile, 0, 1, 0.92 + 0.14 * R.NextUnit);
    C.RoundRect(tx + 1, ty + 1, Size - 2, Size - 2, 2);
    Shape(C, M, col, 0.6);
    If M.Style <> asSoft Then Begin
      Line(C, tx + 3, ty + 2.5, tx + Size - 3, ty + 2.5, Pen(M, 1.2), AGShade(col, 1.35));
      Line(C, tx + 2.5, ty + 3, tx + 2.5, ty + Size - 3, Pen(M, 1.2), AGShade(col, 1.25));
    End;
  End;

Begin
  R := M.A.Stream('frametiles');
  Box(C, 0, 44, 640, 436, Joint);
  x := 0;
  While x < 640 Do Begin
    T(x, 46);
    T(x, FB);
    x := x + Size;
  End;
  y := FY;
  While y < FB Do Begin
    T(0, y);
    T(FR, y);
    y := y + Size;
  End;
End;

(* ---------------------------------------------------------------------- *)
(* Grounds                                                                *)
(* ---------------------------------------------------------------------- *)

Procedure GroundMeadow(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  j, k: Integer;
  x, y: Double;
  g1, g2, col: TAGColor;
Begin
  g1 := TC(M, $3E8A2E);
  g2 := TC(M, $46982F);
  For j := 0 To GH - 1 Do Begin
    C.Rect(FX, FY + j * CH, FR - FX, CH);
    If j Mod 2 = 0 Then C.Fill(g1) Else C.Fill(g2);
  End;
  R := M.A.Stream('tufts');
  For k := 0 To 520 Do Begin
    x := FX + 2 + R.NextUnit * (FR - FX - 4);
    y := FY + 5 + R.NextUnit * (FB - FY - 7);
    If R.NextUnit < 0.5 Then col := AGShade(g1, 0.72) Else col := AGShade(g2, 1.22);
    Line(C, x, y, x - 1.6, y - 3.5, Pen(M, 0.9), col);
    Line(C, x + 0.5, y, x + 0.8, y - 4.5, Pen(M, 0.9), col);
    Line(C, x + 1, y, x + 2.8, y - 3.2, Pen(M, 0.9), col);
  End;
  // flowers
  For k := 0 To 34 Do Begin
    x := FX + 4 + R.NextUnit * (FR - FX - 8);
    y := FY + 4 + R.NextUnit * (FB - FY - 8);
    If R.NextUnit < 0.6 Then col := AGHex($F4F0E0) Else col := AGHex($F0D040);
    Disc(C, x - 1.4, y, 1.2, col);
    Disc(C, x + 1.4, y, 1.2, col);
    Disc(C, x, y - 1.4, 1.2, col);
    Disc(C, x, y + 1.4, 1.2, col);
    Disc(C, x, y, 0.9, AGHex($E08020));
  End;
  SoftLight(C, M, 250, 180, 520, AGRGBA(255, 255, 220, 40), AGRGBA(0, 20, 0, 70));
End;

Procedure GroundClassic(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  i, j: Integer;
  x, y: Double;
  g1, g2, col: TAGColor;
Begin
  g1 := TC(M, $2F7F3A);
  g2 := TC(M, $378A41);
  For j := 0 To GH - 1 Do
    For i := 0 To GW - 1 Do Begin
      x := FX + i * CW;
      y := FY + j * CH;
      If (i + j) Mod 2 = 0 Then col := g1 Else col := g2;
      Box(C, x, y, CW, CH, col);
      If M.Style = asSoft Then Begin
        C.Rect(x, y, CW, CH);
        C.FillPaint(C.Linear(x, y, x + CW, y + CH, [AGRGBA(255, 255, 255, 26), AGRGBA(0, 0, 0, 0), AGRGBA(0, 0, 0, 30)]));
      End;
      // small corner marks, very calm
      Box(C, x + CW / 2 - 1, y + CH / 2 - 1, 2, 2, AGShade(col, 0.85));
    End;
  If M.Style = asSoft Then Begin
    // painted material: short grass blades in both tones
    R := M.A.Stream('classicblades');
    For i := 0 To 420 Do Begin
      x := FX + 2 + R.NextUnit * (FR - FX - 4);
      y := FY + 5 + R.NextUnit * (FB - FY - 7);
      If R.NextUnit < 0.5 Then col := AGShade(g1, 0.78) Else col := AGShade(g2, 1.2);
      Line(C, x, y, x - 1.2, y - 3.2, 0.9, col);
      Line(C, x + 0.6, y, x + 1.6, y - 3.6, 0.9, col);
    End;
  End;
End;

Procedure GroundIce(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  k: Integer;
  x, y, a, l: Double;
  ice, red, blue, scr: TAGColor;
  p: TAGPaint;

  Procedure Ring(cx, cy, rr: Double; Col: TAGColor);
  Begin
    C.BeginPath;
    C.Circle(cx, cy, rr);
    If M.PS > 2 Then C.Stroke(4.5, Col) Else If M.PS > 1 Then C.Stroke(3, Col) Else C.Stroke(2, Col);
  End;

Begin
  ice := TC(M, $DCEBF4);
  Box(C, FX, FY, FR - FX, FB - FY, ice);
  If M.Style = asSoft Then Begin
    C.Rect(FX, FY, FR - FX, FB - FY);
    p := C.Radial(320, 264, 360, [AGRGBA(255, 255, 255, 200), AGRGBA(255, 255, 255, 60), AGRGBA(150, 180, 210, 90)]);
    C.FillPaint(p);
  End;
  // blades scratches
  R := M.A.Stream('scratches');
  scr := AGShade(ice, 0.9);
  For k := 0 To 90 Do Begin
    x := FX + R.NextUnit * (FR - FX);
    y := FY + R.NextUnit * (FB - FY);
    a := R.Range(-0.6, 0.6);
    l := R.Range(10, 40);
    Line(C, x, y, x + l * AGCos(a), y + l * AGSin(a), Pen(M, 0.7), AGWithAlpha(scr, 160));
  End;
  red := AGRGBA(205, 60, 70, 150);
  blue := AGRGBA(70, 110, 210, 140);
  // center line and blue lines on cell borders
  Box(C, 318, FY, 4, FB - FY, red);
  Box(C, FX + 4 * CW - 2, FY, 5, FB - FY, blue);
  Box(C, FX + 11 * CW - 3, FY, 5, FB - FY, blue);
  Box(C, FX + CW - 1, FY, 2, FB - FY, red);
  Box(C, FR - CW - 1, FY, 2, FB - FY, red);
  Ring(320, 264, 52, AGWithAlpha(blue, 170));
  Disc(C, 320, 264, 3, blue);
  Ring(FX + 2.5 * CW, FY + 2.5 * CH, 34, red);
  Ring(FX + 12.5 * CW, FY + 2.5 * CH, 34, red);
  Ring(FX + 2.5 * CW, FY + 8.5 * CH, 34, red);
  Ring(FX + 12.5 * CW, FY + 8.5 * CH, 34, red);
  Disc(C, FX + 2.5 * CW, FY + 2.5 * CH, 3, red);
  Disc(C, FX + 12.5 * CW, FY + 2.5 * CH, 3, red);
  Disc(C, FX + 2.5 * CW, FY + 8.5 * CH, 3, red);
  Disc(C, FX + 12.5 * CW, FY + 8.5 * CH, 3, red);
  // goal creases
  C.BeginPath;
  C.MoveTo(FX + CW, FY + 4.6 * CH);
  C.QuadTo(FX + CW + 30, FY + 5.5 * CH, FX + CW, FY + 6.4 * CH);
  C.Fill(AGRGBA(90, 140, 230, 90));
  C.BeginPath;
  C.MoveTo(FR - CW, FY + 4.6 * CH);
  C.QuadTo(FR - CW - 30, FY + 5.5 * CH, FR - CW, FY + 6.4 * CH);
  C.Fill(AGRGBA(90, 140, 230, 90));
End;

Procedure GroundEgypt(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  i, j, k: Integer;
  x, y, cx, cy: Double;
  sand, col, joint: TAGColor;
Begin
  sand := TC(M, $D8B47A);
  joint := AGShade(sand, 0.62);
  Box(C, FX, FY, FR - FX, FB - FY, joint);
  R := M.A.Stream('slabs');
  For j := 0 To GH - 1 Do
    For i := 0 To GW - 1 Do Begin
      x := FX + i * CW;
      y := FY + j * CH;
      col := Jit(sand, R.Range(-4, 4), 1, 0.9 + 0.14 * R.NextUnit);
      C.Rect(x + 1, y + 1, CW - 2, CH - 2);
      If M.Style = asSoft Then
        C.FillPaint(C.Linear(x, y, x + CW, y + CH, [AGShade(col, 1.12), col, AGShade(col, 0.88)]))
      Else
        C.Fill(col);
      // light top / dark bottom edge of the slab
      Box(C, x + 1, y + 1, CW - 2, 1, AGShade(col, 1.18));
      Box(C, x + 1, y + CH - 2, CW - 2, 1, AGShade(col, 0.85));
      // pores
      For k := 0 To 4 Do
        Disc(C, x + 4 + R.NextUnit * (CW - 8), y + 4 + R.NextUnit * (CH - 8), 0.7 + 0.5 * R.NextUnit, AGShade(col, 0.82));
      // a crack now and then
      If R.NextUnit < 0.12 Then Begin
        cx := x + 6 + R.NextUnit * 20;
        cy := y + 4 + R.NextUnit * 10;
        C.BeginPath;
        C.MoveTo(cx, cy);
        C.LineTo(cx + 5, cy + 6);
        C.LineTo(cx + 4, cy + 12);
        C.LineTo(cx + 10, cy + 18);
        C.Stroke(Pen(M, 0.9), AGShade(col, 0.7));
      End;
    End;
  // drifted sand
  For k := 0 To 22 Do Begin
    Lump(C, R, FX + R.NextUnit * (FR - FX), FY + R.NextUnit * (FB - FY), R.Range(14, 34), R.Range(6, 12), 0.25, 8);
    C.Fill(AGWithAlpha(AGShade(sand, 1.08), 90));
  End;
  SoftLight(C, M, 320, 230, 480, AGRGBA(255, 240, 200, 50), AGRGBA(80, 40, 0, 60));
End;

Procedure GroundMine(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  k: Integer;
  x, y: Double;
  dirt, col: TAGColor;
  p: TAGPaint;
Begin
  dirt := TC(M, $4A4238);
  Box(C, FX, FY, FR - FX, FB - FY, dirt);
  R := M.A.Stream('dirt');
  // big soft patches
  For k := 0 To 60 Do Begin
    Lump(C, R, FX + R.NextUnit * (FR - FX), FY + R.NextUnit * (FB - FY), R.Range(12, 40), R.Range(8, 26), 0.3, 9);
    If R.NextUnit < 0.5 Then C.Fill(AGWithAlpha(AGShade(dirt, 0.75), 120)) Else C.Fill(AGWithAlpha(AGShade(dirt, 1.18), 100));
  End;
  // pebbles and coal crumbs
  For k := 0 To 340 Do Begin
    x := FX + R.NextUnit * (FR - FX);
    y := FY + R.NextUnit * (FB - FY);
    If R.NextUnit < 0.45 Then col := AGHex($1C1A1A) Else col := Jit(dirt, 0, 0.6, 1.3 + 0.3 * R.NextUnit);
    Lump(C, R, x, y, R.Range(1.0, 2.6), R.Range(0.8, 2.0), 0.3, 6);
    C.Fill(col);
  End;
  // lantern light pools on the walls of the frame side
  For k := 0 To 3 Do Begin
    x := FX + (2.5 + 10 * (k Mod 2)) * CW;
    y := FY + (1 + 9 * (k Div 2)) * CH;
    C.Rect(FX, FY, FR - FX, FB - FY);
    p := C.Radial(x, y, 120, [AGRGBA(255, 220, 120, 70), AGRGBA(255, 200, 100, 0)]);
    C.FillPaint(p);
  End;
  SoftLight(C, M, 320, 264, 380, AGRGBA(255, 220, 160, 20), AGRGBA(0, 0, 0, 110));
End;

Procedure GroundBeach(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  k, i: Integer;
  x, y, w: Double;
  sand, col: TAGColor;
Begin
  sand := TC(M, $E6CC98);
  Box(C, FX, FY, FR - FX, FB - FY, sand);
  R := M.A.Stream('ripples');
  // ripples
  For k := 0 To 150 Do Begin
    x := FX + R.NextUnit * (FR - FX);
    y := FY + R.NextUnit * (FB - FY);
    w := R.Range(14, 34);
    C.BeginPath;
    C.MoveTo(x, y);
    C.QuadTo(x + w / 2, y - 3, x + w, y);
    If R.NextUnit < 0.6 Then col := AGShade(sand, 0.84) Else col := AGShade(sand, 1.15);
    C.Stroke(Pen(M, 1.3), col);
  End;
  // grains
  For k := 0 To 500 Do
    Box(C, FX + R.NextUnit * (FR - FX - 1), FY + R.NextUnit * (FB - FY - 1), 1, 1, Jit(sand, 0, 1, 0.8 + 0.4 * R.NextUnit));
  // shells and starfish
  For k := 0 To 9 Do Begin
    x := FX + 10 + R.NextUnit * (FR - FX - 20);
    y := FY + 10 + R.NextUnit * (FB - FY - 20);
    If k Mod 3 = 0 Then Begin
      C.Push;
      C.Translate(x, y);
      C.Rotate(R.NextUnit * AG_TWO_PI);
      C.BeginPath;
      For i := 0 To 9 Do Begin
        If i Mod 2 = 0 Then w := 4.5 Else w := 1.8;
        If i = 0 Then C.MoveTo(w * AGCos(AG_TWO_PI * i / 10), w * AGSin(AG_TWO_PI * i / 10))
        Else C.LineTo(w * AGCos(AG_TWO_PI * i / 10), w * AGSin(AG_TWO_PI * i / 10));
      End;
      C.ClosePath;
      Shape(C, M, AGHex($E07850), 0.6);
      C.Pop;
    End
    Else Begin
      C.Push;
      C.Translate(x, y);
      C.Rotate(R.NextUnit * AG_TWO_PI);
      C.BeginPath;
      C.MoveTo(0, 3);
      C.QuadTo(-4, 1, -3, -2);
      C.QuadTo(0, -4, 3, -2);
      C.QuadTo(4, 1, 0, 3);
      Shape(C, M, AGHex($F2E6DA), 0.6);
      Line(C, 0, 2.5, -1.5, -2.5, Pen(M, 0.5), AGHex($C8B0A0));
      Line(C, 0, 2.5, 1.5, -2.5, Pen(M, 0.5), AGHex($C8B0A0));
      C.Pop;
    End;
  End;
  SoftLight(C, M, 320, 200, 480, AGRGBA(255, 255, 230, 60), AGRGBA(120, 80, 20, 50));
End;

Procedure GroundAlien(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  i, j: Integer;
  x, y: Double;
  plate, groove, glow, col: TAGColor;
Begin
  plate := TC(M, $35505A);
  groove := AGShade(plate, 0.45);
  glow := TC(M, $50E0D0);
  Box(C, FX, FY, FR - FX, FB - FY, groove);
  R := M.A.Stream('plates');
  For j := 0 To GH - 1 Do
    For i := 0 To GW - 1 Do Begin
      x := FX + i * CW;
      y := FY + j * CH;
      col := Jit(plate, 0, 1, 0.94 + 0.1 * R.NextUnit);
      C.RoundRect(x + 1.5, y + 1.5, CW - 3, CH - 3, 3);
      If M.Style = asSoft Then
        C.FillPaint(C.Linear(x, y, x, y + CH, [AGShade(col, 1.25), col, AGShade(col, 0.8)]))
      Else
        C.Fill(col);
      Box(C, x + 3, y + 2, CW - 6, 1, AGShade(col, 1.3));
      // rivets
      Disc(C, x + 5, y + 5, 1.2, AGShade(col, 1.5));
      Disc(C, x + CW - 5, y + 5, 1.2, AGShade(col, 1.5));
      Disc(C, x + 5, y + CH - 5, 1.2, AGShade(col, 0.6));
      Disc(C, x + CW - 5, y + CH - 5, 1.2, AGShade(col, 0.6));
      // engraved lines, some glowing
      If (i + 2 * j) Mod 3 = 0 Then Begin
        Line(C, x + 10, y + CH / 2, x + CW - 10, y + CH / 2, Pen(M, 1.4), AGShade(col, 0.6));
        If R.NextUnit < 0.35 Then
          Line(C, x + 12, y + CH / 2, x + CW - 12, y + CH / 2, Pen(M, 0.8), AGWithAlpha(glow, 200));
      End
      Else If (i + 2 * j) Mod 3 = 1 Then Begin
        C.BeginPath;
        C.MoveTo(x + 10, y + 9);
        C.LineTo(x + 16, y + 9);
        C.LineTo(x + 20, y + 13);
        C.Stroke(Pen(M, 1), AGShade(col, 0.65));
      End;
    End;
  SoftLight(C, M, 320, 264, 420, AGRGBA(160, 255, 240, 30), AGRGBA(0, 0, 20, 90));
End;

Procedure GroundHaunted(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  j, k: Integer;
  x, y, w: Double;
  wood, col: TAGColor;
Begin
  wood := TC(M, $5A4048);
  Box(C, FX, FY, FR - FX, FB - FY, AGShade(wood, 0.45));
  R := M.A.Stream('planks');
  // 3 planks per cell row (12 px), joints staggered
  For j := 0 To GH * 3 - 1 Do Begin
    y := FY + j * 12;
    x := FX - (j Mod 4) * 30 - 20;
    While x < FR Do Begin
      w := 80 + 40 * R.RangeI(0, 2);
      col := Jit(wood, R.Range(-6, 6), 1, 0.85 + 0.25 * R.NextUnit);
      C.Rect(x + 1, y + 1, w - 2, 10);
      If M.Style = asSoft Then
        C.FillPaint(C.Linear(0, y, 0, y + 12, [AGShade(col, 1.2), col, AGShade(col, 0.8)]))
      Else
        C.Fill(col);
      // grain
      Line(C, x + 4, y + 4, x + w - 6, y + 4.5, Pen(M, 0.6), AGShade(col, 0.82));
      Line(C, x + 10, y + 7.5, x + w - 14, y + 7, Pen(M, 0.6), AGShade(col, 0.88));
      // nails
      Disc(C, x + 3, y + 6, 0.9, AGHex($2A2026));
      // knot
      If R.NextUnit < 0.2 Then Oval(C, x + w * R.Range(0.3, 0.7), y + 6, 2.6, 1.6, AGShade(col, 0.65));
      x := x + w;
    End;
  End;
  // ghostly fog
  For k := 0 To 14 Do Begin
    Lump(C, R, FX + R.NextUnit * (FR - FX), FY + R.NextUnit * (FB - FY), R.Range(30, 60), R.Range(12, 22), 0.25, 9);
    C.Fill(AGRGBA(200, 190, 230, 24));
  End;
  SoftLight(C, M, 320, 264, 400, AGRGBA(220, 200, 255, 30), AGRGBA(10, 0, 20, 110));
End;

Procedure GroundWater(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  k: Integer;
  x, y, w: Double;
  sand, col: TAGColor;
  p: TAGPaint;
Begin
  sand := TC(M, $8EB4A8);
  Box(C, FX, FY, FR - FX, FB - FY, sand);
  R := M.A.Stream('seafloor');
  For k := 0 To 40 Do Begin
    Lump(C, R, FX + R.NextUnit * (FR - FX), FY + R.NextUnit * (FB - FY), R.Range(15, 40), R.Range(8, 20), 0.3, 9);
    C.Fill(AGWithAlpha(AGShade(sand, 0.85 + 0.3 * R.NextUnit), 90));
  End;
  // caustics: light wavy net
  For k := 0 To 120 Do Begin
    x := FX + R.NextUnit * (FR - FX);
    y := FY + R.NextUnit * (FB - FY);
    w := R.Range(10, 24);
    C.BeginPath;
    C.MoveTo(x, y);
    C.QuadTo(x + w * 0.5, y + R.Range(-6, 6), x + w, y + R.Range(-4, 4));
    C.QuadTo(x + w * 1.2, y + R.Range(4, 10), x + w * 0.8, y + R.Range(10, 16));
    C.Stroke(Pen(M, 1.2), AGRGBA(225, 250, 255, 70));
  End;
  // pebbles
  For k := 0 To 60 Do Begin
    Lump(C, R, FX + R.NextUnit * (FR - FX), FY + R.NextUnit * (FB - FY), R.Range(1.5, 3), R.Range(1, 2.2), 0.3, 6);
    col := Jit(sand, 0, 0.5, 0.6 + 0.5 * R.NextUnit);
    C.Fill(col);
  End;
  C.Rect(FX, FY, FR - FX, FB - FY);
  p := C.Linear(0, FY, 0, FB, [AGRGBA(200, 255, 255, 40), AGRGBA(0, 40, 80, 0), AGRGBA(0, 30, 70, 70)]);
  C.FillPaint(p);
End;

Procedure GroundForest(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  k: Integer;
  x, y: Double;
  soil, moss, col: TAGColor;
Begin
  soil := TC(M, $6A5236);
  moss := TC(M, $4E6A2A);
  Box(C, FX, FY, FR - FX, FB - FY, soil);
  R := M.A.Stream('forestfloor');
  For k := 0 To 46 Do Begin
    Lump(C, R, FX + R.NextUnit * (FR - FX), FY + R.NextUnit * (FB - FY), R.Range(14, 36), R.Range(10, 24), 0.35, 10);
    C.Fill(AGWithAlpha(Jit(moss, 0, 1, 0.8 + 0.3 * R.NextUnit), 150));
  End;
  // leaves
  For k := 0 To 260 Do Begin
    x := FX + R.NextUnit * (FR - FX);
    y := FY + R.NextUnit * (FB - FY);
    Case R.RangeI(0, 3) Of
      0: col := AGHex($C07828);
      1: col := AGHex($A0502A);
      2: col := AGHex($D0A040);
    Else
      col := AGHex($7A5A2A);
    End;
    C.Push;
    C.Translate(x, y);
    C.Rotate(R.NextUnit * AG_TWO_PI);
    C.Ellipse(0, 0, 3.2, 1.6);
    C.Fill(Jit(col, 0, 1, M.Light));
    Line(C, -3, 0, 3, 0, Pen(M, 0.5), AGShade(col, 0.7));
    C.Pop;
  End;
  // twigs
  For k := 0 To 18 Do Begin
    x := FX + R.NextUnit * (FR - FX);
    y := FY + R.NextUnit * (FB - FY);
    Line(C, x, y, x + R.Range(-10, 10), y + R.Range(-5, 5), Pen(M, 1.1), AGHex($3A2A1A));
  End;
  SoftLight(C, M, 300, 220, 460, AGRGBA(255, 240, 180, 40), AGRGBA(0, 20, 0, 90));
End;

Procedure GroundCity(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  i, j, k: Integer;
  x, y, cx, cy: Double;
  conc, col: TAGColor;
Begin
  conc := TC(M, $8C8C88);
  Box(C, FX, FY, FR - FX, FB - FY, AGShade(conc, 0.6));
  R := M.A.Stream('pavement');
  For j := 0 To GH - 1 Do
    For i := 0 To GW - 1 Do Begin
      x := FX + i * CW;
      y := FY + j * CH;
      col := Jit(conc, 0, 1, 0.9 + 0.14 * R.NextUnit);
      C.Rect(x + 1, y + 1, CW - 2, CH - 2);
      If M.Style = asSoft Then
        C.FillPaint(C.Linear(x, y, x + CW, y + CH, [AGShade(col, 1.1), col, AGShade(col, 0.9)]))
      Else
        C.Fill(col);
      For k := 0 To 5 Do
        Box(C, x + 2 + R.NextUnit * (CW - 5), y + 2 + R.NextUnit * (CH - 5), 1, 1, AGShade(col, 0.8 + 0.4 * R.NextUnit));
      If R.NextUnit < 0.15 Then Begin
        cx := x + 4 + R.NextUnit * 10;
        cy := y + 3;
        C.BeginPath;
        C.MoveTo(cx, cy);
        C.LineTo(cx + 6, cy + 8);
        C.LineTo(cx + 5, cy + 16);
        C.LineTo(cx + 14, cy + 26);
        C.Stroke(Pen(M, 0.9), AGShade(col, 0.6));
      End;
    End;
  // oil stains and dirt
  For k := 0 To 12 Do Begin
    Lump(C, R, FX + R.NextUnit * (FR - FX), FY + R.NextUnit * (FB - FY), R.Range(6, 16), R.Range(4, 10), 0.3, 8);
    C.Fill(AGRGBA(30, 28, 34, 70));
  End;
  // a few papers
  For k := 0 To 8 Do Begin
    C.Push;
    C.Translate(FX + 8 + R.NextUnit * (FR - FX - 16), FY + 8 + R.NextUnit * (FB - FY - 16));
    C.Rotate(R.NextUnit * AG_TWO_PI);
    C.Rect(-3, -2, 6, 4);
    C.Fill(AGHex($E8E4D8));
    C.Pop;
  End;
  SoftLight(C, M, 320, 264, 420, AGRGBA(255, 255, 240, 30), AGRGBA(0, 0, 0, 80));
End;

(* ---------------------------------------------------------------------- *)
(* Frames per theme                                                       *)
(* ---------------------------------------------------------------------- *)

Procedure FrameIce(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  k: Integer;
  x, y: Double;
Begin
  // stands with spectators
  Box(C, 0, 44, 640, 436, TC(M, $22283A));
  R := M.A.Stream('crowd');
  For k := 0 To 520 Do Begin
    x := R.NextUnit * 640;
    y := 46 + R.NextUnit * 434;
    If Not InFrame(x, y) Then Continue;
    Disc(C, x, y, 1.8, AGHSV(R.NextUnit * 360, 0.5, 0.5 + 0.4 * R.NextUnit));
  End;
  // boards: white with yellow kick plate, rounded corners
  C.RoundRect(FX - 9, FY - 9, FR - FX + 18, FB - FY + 18, 22);
  C.RoundRect(FX - 2, FY - 2, FR - FX + 4, FB - FY + 4, 16);
  C.FillRule := frEvenOdd;
  Shape(C, M, AGHex($F4F4F0), 0.8);
  C.FillRule := frNonZero;
  C.BeginPath;
  C.RoundRect(FX - 4, FY - 4, FR - FX + 8, FB - FY + 8, 18);
  C.Stroke(Pen(M, 2.2), AGHex($E8C030));
  // posts of the glass
  For k := 0 To 16 Do Begin
    x := FX - 6 + k * (FR - FX + 12) / 16;
    Box(C, x - 1.5, FY - 12, 3, 4, AGHex($505860));
    Box(C, x - 1.5, FB + 8, 3, 4, AGHex($505860));
  End;
End;

Procedure FrameEgypt(C: TAGCanvas; Const M: TMap);
Var
  stone, dark: TAGColor;
  R: TAGRandom;

  Procedure Glyph(x, y: Double; Index: Integer);
  Begin
    // carved symbols: ankh, eye, bird, wave, sun
    Case (Index + R.RangeI(0, 4)) Mod 5 Of
      0: Begin
          C.BeginPath;
          C.Ellipse(x, y - 4, 2.5, 3);
          C.Stroke(Pen(M, 1.3), dark);
          Line(C, x, y - 1, x, y + 6, Pen(M, 1.3), dark);
          Line(C, x - 4, y + 1, x + 4, y + 1, Pen(M, 1.3), dark);
        End;
      1: Begin
          C.BeginPath;
          C.MoveTo(x - 6, y);
          C.QuadTo(x, y - 6, x + 6, y);
          C.QuadTo(x, y + 4, x - 6, y);
          C.Stroke(Pen(M, 1.2), dark);
          Disc(C, x, y - 0.5, 1.6, dark);
          Line(C, x - 1, y + 2, x - 3, y + 6, Pen(M, 1.1), dark);
        End;
      2: Begin
          C.BeginPath;
          C.MoveTo(x - 5, y + 5);
          C.LineTo(x - 2, y - 1);
          C.LineTo(x + 1, y - 4);
          C.LineTo(x + 4, y - 3);
          C.LineTo(x + 1, y - 1);
          C.LineTo(x + 3, y + 5);
          C.Stroke(Pen(M, 1.2), dark);
        End;
      3: Begin
          C.BeginPath;
          C.MoveTo(x - 6, y);
          C.LineTo(x - 3, y - 3);
          C.LineTo(x, y);
          C.LineTo(x + 3, y - 3);
          C.LineTo(x + 6, y);
          C.Stroke(Pen(M, 1.2), dark);
        End;
    Else Begin
        C.BeginPath;
        C.Circle(x, y, 3.5);
        C.Stroke(Pen(M, 1.2), dark);
        Disc(C, x, y, 1.2, dark);
      End;
    End;
  End;

  Procedure Blocks(x0, y0, x1, y1, bw: Double);
  Var
    x: Double;
  Begin
    x := x0;
    While x < x1 - 0.5 Do Begin
      C.Rect(x + 0.5, y0 + 0.5, AGMin(bw, x1 - x) - 1, y1 - y0 - 1);
      Shape(C, M, Jit(stone, 0, 1, 0.9 + 0.15 * R.NextUnit), 0.6);
      x := x + bw;
    End;
  End;

Var
  y: Double;
Begin
  stone := TC(M, $C49A5C);
  dark := AGShade(stone, 0.5);
  R := M.A.Stream('hieroglyphs');
  Box(C, 0, 44, 640, 436, AGShade(stone, 0.55));
  Blocks(0, 46, 640, FY, 40);
  Blocks(0, FB, 640, 480, 40);
  y := FY;
  While y < FB Do Begin
    C.Rect(0.5, y + 0.5, FX - 1, CH - 1);
    Shape(C, M, Jit(stone, 0, 1, 0.9 + 0.15 * R.NextUnit), 0.6);
    C.Rect(FR + 0.5, y + 0.5, 640 - FR - 1, CH - 1);
    Shape(C, M, Jit(stone, 0, 1, 0.9 + 0.15 * R.NextUnit), 0.6);
    y := y + CH;
  End;
  ForBand(40, @Glyph);
  // golden corner scarabs
  Disc(C, 10, 56, 6, AGHex($E0B030));
  Disc(C, 630, 56, 6, AGHex($E0B030));
  Disc(C, 10, 471, 6, AGHex($E0B030));
  Disc(C, 630, 471, 6, AGHex($E0B030));
  FieldRim(C, M, dark, 2);
End;

Procedure FrameMine(C: TAGCanvas; Const M: TMap);
Var
  wood: TAGColor;
  R: TAGRandom;
  x, y: Double;

  Procedure Beam(bx, by, bw, bh: Double);
  Var
    col: TAGColor;
  Begin
    col := Jit(wood, 0, 1, 0.85 + 0.25 * R.NextUnit);
    C.Rect(bx, by, bw, bh);
    Shape(C, M, col, 0.8);
    If bw > bh Then Begin
      Line(C, bx + 4, by + bh * 0.35, bx + bw - 4, by + bh * 0.4, Pen(M, 0.7), AGShade(col, 0.75));
      Line(C, bx + 8, by + bh * 0.7, bx + bw - 10, by + bh * 0.65, Pen(M, 0.7), AGShade(col, 0.75));
    End
    Else Begin
      Line(C, bx + bw * 0.35, by + 4, bx + bw * 0.4, by + bh - 4, Pen(M, 0.7), AGShade(col, 0.75));
      Line(C, bx + bw * 0.7, by + 8, bx + bw * 0.65, by + bh - 10, Pen(M, 0.7), AGShade(col, 0.75));
    End;
  End;

Begin
  wood := TC(M, $7A5430);
  R := M.A.Stream('beams');
  Box(C, 0, 44, 640, 436, AGHex($181614));
  x := 0;
  While x < 640 Do Begin
    Beam(x, 47, 80, 17);
    Beam(x, FB + 1, 80, 16);
    x := x + 80;
  End;
  y := FY;
  While y < FB Do Begin
    Beam(1, y, 18, 72);
    Beam(FR + 1, y, 18, 72);
    y := y + 72;
  End;
  // iron brackets with bolts
  x := 0;
  While x <= 640 Do Begin
    Box(C, x - 3, 47, 6, 17, AGHex($4C4C50));
    Box(C, x - 3, FB + 1, 6, 16, AGHex($4C4C50));
    Disc(C, x, 55, 1.2, AGHex($A0A0A8));
    Disc(C, x, FB + 9, 1.2, AGHex($A0A0A8));
    x := x + 80;
  End;
  // lanterns in the corners
  Disc(C, 10, 56, 6, AGHex($FFD070));
  Disc(C, 630, 56, 6, AGHex($FFD070));
  Disc(C, 10, 56, 3, AGHex($FFF4C0));
  Disc(C, 630, 56, 3, AGHex($FFF4C0));
  FieldRim(C, M, AGHex($100E0C), 2);
End;

Procedure FrameBeach(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  x: Double;
  k: Integer;
  water: TAGColor;

  Procedure P(px, py: Double; Index: Integer);
  Begin
    If py < FY Then exit; // top is water
    Lump(C, R, px + R.Range(-3, 3), py + R.Range(-2, 2), R.Range(5, 8), R.Range(4, 6), 0.2, 7);
    Shape(C, M, Jit(TC(M, $A08A70), 0, 1, 0.8 + 0.35 * R.NextUnit), 0.7);
    If R.NextUnit < 0.3 Then Begin
      // seaweed
      C.BeginPath;
      C.MoveTo(px - 3, py + 4);
      C.QuadTo(px - 6, py - 2, px - 1, py - 6);
      C.Stroke(Pen(M, 1.4), AGHex($3A7040));
    End;
  End;

Begin
  R := M.A.Stream('shore');
  Box(C, 0, 44, 640, 436, TC(M, $C8A870));
  // water with foam on top
  water := TC(M, $3090C0);
  C.Rect(0, 44, 640, FY - 44);
  FillV(C, M, 44, FY, AGShade(water, 0.8), AGShade(water, 1.2));
  C.BeginPath;
  C.MoveTo(0, FY - 6);
  x := 0;
  For k := 0 To 31 Do Begin
    C.QuadTo(x + 10, FY - 10 + 3 * R.NextUnit, x + 20, FY - 6);
    x := x + 20;
  End;
  C.LineTo(640, FY);
  C.LineTo(0, FY);
  C.ClosePath;
  C.Fill(AGHex($F0F8FA));
  ForBand(16, @P);
  FieldRim(C, M, AGShade(TC(M, $C8A870), 0.7), 1.5);
End;

Procedure FrameAlien(C: TAGCanvas; Const M: TMap);
Var
  metal, glow: TAGColor;
  x, y: Double;
  i: Integer;
Begin
  metal := TC(M, $3A3450);
  glow := TC(M, $50F0D0);
  Box(C, 0, 44, 640, 436, AGShade(metal, 0.6));
  // panels
  C.Rect(1, 47, 638, FY - 48);
  Shape(C, M, metal, 0.8);
  C.Rect(1, FB + 1, 638, 480 - FB - 2);
  Shape(C, M, metal, 0.8);
  C.Rect(1, FY, FX - 2, FB - FY);
  Shape(C, M, metal, 0.8);
  C.Rect(FR + 1, FY, 640 - FR - 2, FB - FY);
  Shape(C, M, metal, 0.8);
  // pipes
  Line(C, 4, 51, 636, 51, Pen(M, 2.5), AGShade(metal, 1.5));
  Line(C, 4, 476, 636, 476, Pen(M, 2.5), AGShade(metal, 1.5));
  // lights
  x := 30;
  i := 0;
  While x < 640 Do Begin
    C.RoundRect(x - 6, 55, 12, 6, 3);
    If i Mod 2 = 0 Then C.Fill(glow) Else C.Fill(AGHex($F0A040));
    C.RoundRect(x - 6, FB + 5, 12, 6, 3);
    If i Mod 2 = 1 Then C.Fill(glow) Else C.Fill(AGHex($F0A040));
    x := x + 60;
    inc(i);
  End;
  y := FY + CH / 2;
  While y < FB Do Begin
    Disc(C, FX / 2, y, 3, glow);
    Disc(C, (FR + 640) / 2, y, 3, glow);
    y := y + 2 * CH;
  End;
  FieldRim(C, M, glow, 1.5);
End;

Procedure FrameHaunted(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  stone: TAGColor;
  web: TAGColor;
  x, y: Double;
  k: Integer;

  Procedure Cobweb(cx, cy, sx, sy: Double);
  Var
    i: Integer;
    a: Double;
  Begin
    For i := 0 To 4 Do Begin
      a := AG_HALF_PI * i / 4;
      Line(C, cx, cy, cx + sx * 26 * AGCos(a), cy + sy * 26 * AGSin(a), Pen(M, 0.7), web);
    End;
    For i := 1 To 3 Do Begin
      C.BeginPath;
      C.MoveTo(cx + sx * 8 * i, cy);
      C.QuadTo(cx + sx * 5 * i, cy + sy * 5 * i, cx, cy + sy * 8 * i);
      C.Stroke(Pen(M, 0.7), web);
    End;
  End;

Begin
  R := M.A.Stream('wall');
  stone := TC(M, $55505E);
  web := AGRGBA(230, 230, 240, 170);
  Box(C, 0, 44, 640, 436, AGShade(stone, 0.45));
  // stone wall in rows
  y := 46;
  k := 0;
  While y < 480 Do Begin
    x := -(k Mod 2) * 14;
    While x < 640 Do Begin
      If InFrame(x + 14, y + 5) Or InFrame(x + 2, y + 2) Or InFrame(x + 26, y + 8) Then Begin
        C.RoundRect(x + 1, y + 1, 26, 8, 2);
        Shape(C, M, Jit(stone, 0, 1, 0.8 + 0.35 * R.NextUnit), 0.6);
      End;
      x := x + 28;
    End;
    y := y + 10;
    inc(k);
  End;
  Cobweb(FX + 1, FY + 1, 1, 1);
  Cobweb(FR - 1, FY + 1, -1, 1);
  // candles
  For k := 0 To 1 Do Begin
    x := 6 + k * 620 + 2;
    Box(C, x - 3, FB - 30, 6, 14, AGHex($E8E0C8));
    Oval(C, x, FB - 34, 2.4, 4, AGHex($FFC040));
    Oval(C, x, FB - 33, 1, 2, AGHex($FFF0B0));
  End;
  FieldRim(C, M, AGShade(stone, 0.3), 2);
End;

Procedure FrameWater(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  deep: TAGColor;

  Procedure P(px, py: Double; Index: Integer);
  Var
    col: TAGColor;
    k: Integer;
  Begin
    Case R.RangeI(0, 3) Of
      0: col := AGHex($F08050);
      1: col := AGHex($E8C050);
      2: col := AGHex($A070D0);
    Else
      col := AGHex($E06070);
    End;
    col := Jit(col, M.Hue, 1, M.Light);
    If R.NextUnit < 0.5 Then Begin
      // branching coral
      For k := 0 To 2 Do Begin
        C.BeginPath;
        C.MoveTo(px, py + 6);
        C.QuadTo(px + (k - 1) * 3, py, px + (k - 1) * 6 + R.Range(-1, 1), py - 6);
        C.Stroke(Pen(M, 3), AGShade(col, 0.6));
        C.BeginPath;
        C.MoveTo(px, py + 6);
        C.QuadTo(px + (k - 1) * 3, py, px + (k - 1) * 6 + R.Range(-1, 1), py - 6);
        C.Stroke(Pen(M, 1.8), col);
      End;
    End
    Else Begin
      Lump(C, R, px, py, R.Range(6, 8), R.Range(5, 7), 0.22, 8);
      Shape(C, M, col, 0.8);
      Disc(C, px - 2, py - 2, 1.2, AGShade(col, 1.4));
      Disc(C, px + 2, py + 1, 1.0, AGShade(col, 0.7));
    End;
  End;

Var
  k: Integer;
Begin
  R := M.A.Stream('reef');
  deep := TC(M, $1A4A6A);
  C.Rect(0, 44, 640, 436);
  FillV(C, M, 44, 480, AGShade(deep, 1.2), AGShade(deep, 0.8));
  ForBand(14, @P);
  // bubbles
  For k := 0 To 30 Do Begin
    C.BeginPath;
    C.Circle(R.NextUnit * 640, 48 + R.NextUnit * 430, 1.5 + 2 * R.NextUnit);
    C.Stroke(Pen(M, 0.8), AGRGBA(220, 250, 255, 200));
  End;
  FieldRim(C, M, AGShade(deep, 0.6), 2);
End;

Procedure FrameForest(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  leaf: TAGColor;

  Procedure P(px, py: Double; Index: Integer);
  Var
    k: Integer;
  Begin
    For k := 0 To 2 Do Begin
      Lump(C, R, px + R.Range(-7, 7), py + R.Range(-6, 6), R.Range(7, 11), R.Range(6, 9), 0.25, 8);
      Shape(C, M, Jit(leaf, R.Range(-10, 10), 1, 0.7 + 0.45 * R.NextUnit), 0.7);
    End;
  End;

Begin
  R := M.A.Stream('foliage');
  leaf := TC(M, $3A6A2A);
  Box(C, 0, 44, 640, 436, AGShade(leaf, 0.35));
  ForBand(14, @P);
  // tree trunks in the corners
  C.Circle(10, 56, 12);
  Shape(C, M, TC(M, $6A4A2A), 1);
  C.Circle(630, 56, 12);
  Shape(C, M, TC(M, $6A4A2A), 1);
  FieldRim(C, M, AGShade(leaf, 0.3), 2);
End;

Procedure FrameCity(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  brick, col: TAGColor;
  x, y: Double;
  k: Integer;
Begin
  R := M.A.Stream('wall');
  brick := TC(M, $9A4A38);
  Box(C, 0, 44, 640, 436, AGHex($6A6460));
  y := 46;
  k := 0;
  While y < 480 Do Begin
    x := -(k Mod 2) * 10;
    While x < 640 Do Begin
      If InFrame(x + 10, y + 3) Or InFrame(x + 1, y + 1) Or InFrame(x + 19, y + 6) Then Begin
        col := Jit(brick, R.Range(-5, 5), 1, 0.8 + 0.3 * R.NextUnit);
        Box(C, x + 0.5, y + 0.5, 19, 6, col);
        Box(C, x + 0.5, y + 0.5, 19, 1, AGShade(col, 1.2));
      End;
      x := x + 20;
    End;
    y := y + 7;
    inc(k);
  End;
  // pipes along the top and bottom
  Line(C, 0, 50, 640, 50, Pen(M, 4), AGHex($505A60));
  Line(C, 0, 49, 640, 49, Pen(M, 1.2), AGHex($8A9AA0));
  x := 40;
  While x < 640 Do Begin
    Box(C, x - 2, 47, 4, 7, AGHex($3A4044));
    x := x + 120;
  End;
  FieldRim(C, M, AGHex($2A2826), 2);
End;

(* ---------------------------------------------------------------------- *)
(* Style kits: the style decides palette, ground pattern and block shapes *)
(* ---------------------------------------------------------------------- *)
(*
 * The theme stays the same in every style (ice stays ice, Egypt stays
 * Egypt), but every style has its own kit:
 *   flat  = pop / vector: a bold palette of a few flat colors per theme
 *           (FlatPals), large clean stripes / tiles / dots without texture,
 *           geometric blocks (rounded rectangles) with one symbol, an ink
 *           outline, a hard flat shadow of the frame on the ground
 *   soft  = painted material: the naturalistic grounds above (grass blades,
 *           sand ripples, wood grain, ...), light from the top left over the
 *           whole field, the blocks cast a soft shadow onto the ground
 *   pixel = 16-bit tiles: the ground is built from repeating tiles of half a
 *           cell (20 x 18) with classic motifs (checker, bricks, cobbles,
 *           planks, plates), bevelled blocks with a one pixel light / dark
 *           edge, every color from a 5 step ramp per theme (PixPals)
 *   retro = the painted shapes of soft at a third of the resolution, snapped
 *           to the VGA palette (unchanged)
 *)

Type
  TKitKind = (kkPaint, kkFlat, kkTile);

  TFlatPal = Record
    G1, G2: Cardinal; // ground
    F1, F2: Cardinal; // frame, frame symbols and rim
    Br, Br2: Cardinal; // brick, its symbol
    So, So2: Cardinal; // solid, its details
    Ac: Cardinal; // accent (flowers, markings)
    GPat: Integer; // ground pattern, see GroundFlat
    FSym, BSym: Integer; // symbols of frame and brick, see FlatSymbol
  End;

  TPixPal = Record
    G1, G2: Cardinal; // ground ramps
    Fr: Cardinal; // frame ramp
    Br, So: Cardinal; // brick and solid ramps
    GTile: Integer; // ground tile, see GroundPixel
    BMotif: Integer; // motif of the brick, see PixelBrick
  End;

  TRamp = Array[0..4] Of TAGColor; // dark .. light

Const
  InkHex = $1A1626;

  FlatPals: Array[TTheme] Of TFlatPal = (
    (G1: $8CD65A; G2: $7AC64A; F1: $2F6B4F; F2: $F7D354; Br: $F29E4C; Br2: $FFF1C9; So: $5B6B8C; So2: $9AA8C8; Ac: $FF6F59; GPat: 0; FSym: 0; BSym: 1), // meadow
    (G1: $56B870; G2: $46A660; F1: $35495E; F2: $F2F2F2; Br: $E4572E; Br2: $FFE0D0; So: $8D99AE; So2: $EDF2F4; Ac: $FFC914; GPat: 1; FSym: 7; BSym: 7), // classic
    (G1: $EAF6FF; G2: $D6ECFB; F1: $2B4C7E; F2: $FFFFFF; Br: $7FD1F5; Br2: $FFFFFF; So: $3D5A80; So2: $98C1D9; Ac: $EE4266; GPat: 2; FSym: 4; BSym: 4), // ice
    (G1: $F4D58D; G2: $EAC274; F1: $8D5524; F2: $2EC4B6; Br: $F7B05B; Br2: $8D5524; So: $6D4C3D; So2: $FFD166; Ac: $2EC4B6; GPat: 3; FSym: 3; BSym: 3), // egypt
    (G1: $55597A; G2: $4A4E69; F1: $22223B; F2: $F2A541; Br: $E8A33D; Br2: $5A3A12; So: $9A8C98; So2: $D9C5C0; Ac: $F2A541; GPat: 4; FSym: 2; BSym: 2), // mine
    (G1: $FFE5A8; G2: $FFD68A; F1: $00A6D6; F2: $FFFFFF; Br: $FF9F1C; Br2: $FFF3D6; So: $6C757D; So2: $ADB5BD; Ac: $FF5D73; GPat: 5; FSym: 5; BSym: 4), // beach
    (G1: $2E1F5E; G2: $3A2A73; F1: $160F30; F2: $3DF5D8; Br: $8A5CF6; Br2: $3DF5D8; So: $8A80A8; So2: $F5D547; Ac: $3DF5D8; GPat: 6; FSym: 2; BSym: 2), // alien
    (G1: $5E4B6E; G2: $54426A; F1: $241B2F; F2: $F7A440; Br: $A86F4C; Br2: $3A2A2E; So: $9E9AB5; So2: $D8D5E8; Ac: $F7A440; GPat: 7; FSym: 6; BSym: 1), // haunted
    (G1: $3FB8C9; G2: $33A5BA; F1: $114B7A; F2: $FF8A5B; Br: $FF7A6B; Br2: $FFD4C9; So: $1E6E8C; So2: $9BE3EE; Ac: $FFE066; GPat: 8; FSym: 0; BSym: 5), // water
    (G1: $6A9A44; G2: $5C8A3A; F1: $2D4A22; F2: $E9C46A; Br: $3FA34D; Br2: $B5E48C; So: $8B5E3C; So2: $D9A066; Ac: $E76F51; GPat: 9; FSym: 3; BSym: 2), // forest
    (G1: $A7A9AC; G2: $96989C; F1: $3D405B; F2: $F4D35E; Br: $E09F3E; Br2: $FFF1D0; So: $3A86FF; So2: $BFD7FF; Ac: $F4D35E; GPat: 10; FSym: 7; BSym: 6) // city
    );

  PixPals: Array[TTheme] Of TPixPal = (
    (G1: $4CA03A; G2: $44922F; Fr: $8A8C84; Br: $B87A3C; So: $9A9A92; GTile: 0; BMotif: 0), // meadow
    (G1: $2F8F3A; G2: $3FA44A; Fr: $A4A8AE; Br: $B84A30; So: $A0A4AA; GTile: 1; BMotif: 1), // classic
    (G1: $D4E6F2; G2: $C4DAEA; Fr: $5878A0; Br: $A8E0F4; So: $405878; GTile: 2; BMotif: 2), // ice
    (G1: $D8B47A; G2: $CCA466; Fr: $C49A5C; Br: $E0B878; So: $7A6650; GTile: 3; BMotif: 3), // egypt
    (G1: $5A5044; G2: $4A4238; Fr: $7A5430; Br: $3A3842; So: $6A5E54; GTile: 4; BMotif: 4), // mine
    (G1: $E2D290; G2: $D6C27E; Fr: $3090C0; Br: $D8A858; So: $6A645C; GTile: 5; BMotif: 5), // beach
    (G1: $35505A; G2: $2C444C; Fr: $3A3450; Br: $60D8E8; So: $505070; GTile: 6; BMotif: 6), // alien
    (G1: $6A4C56; G2: $5A4048; Fr: $55505E; Br: $7A5030; So: $8A8698; GTile: 7; BMotif: 7), // haunted
    (G1: $8EB4A8; G2: $80A89C; Fr: $1A4A6A; Br: $F07848; So: $44607A; GTile: 8; BMotif: 8), // water
    (G1: $6A5236; G2: $4E6A2A; Fr: $3A6A2A; Br: $4C8A30; So: $7A5432; GTile: 9; BMotif: 9), // forest
    (G1: $8C8C88; G2: $80807C; Fr: $9A4A38; Br: $B89058; So: $3A6A9A; GTile: 10; BMotif: 10) // city
    );

  (* Ground color of the painted grounds, the soft blocks cast their shadow in a darker tone of it *)
  SoftGround: Array[TTheme] Of Cardinal = ($42902E, $338440, $CFE2EE, $D0AC72, $4A4238, $E0C690, $35505A, $4A3640, $8EB4A8, $5E5A32, $868682);

Function Kit(Const M: TMap): TKitKind;
Begin
  Case M.Style Of
    asFlat: result := kkFlat;
    asPixel: result := kkTile;
  Else
    result := kkPaint;
  End;
End;

(* 5 step ramp of a base color, shadows shift a bit to blue, lights to yellow (16-bit look) *)
Procedure MakeRamp(Base: TAGColor; Out R: TRamp);
Begin
  R[0] := Jit(Base, 8, 1.15, 0.48);
  R[1] := Jit(Base, 4, 1.08, 0.74);
  R[2] := Base;
  R[3] := Jit(Base, -5, 0.85, 1.22);
  R[4] := Jit(Base, -9, 0.6, 1.48);
End;

(* Block with a light top / left and a dark bottom / right edge of e pixel *)
Procedure PixBevel(C: TAGCanvas; x, y, w, h: Double; Hi, Mid, Lo: TAGColor; e: Double);
Begin
  Box(C, x, y, w, h, Lo);
  Box(C, x, y, w - e, h - e, Hi);
  Box(C, x + e, y + e, w - 2 * e, h - 2 * e, Mid);
End;

(* Flat symbols: 0 circle, 1 plus, 2 diamond, 3 triangle, 4 star, 5 ring, 6 chevron, 7 square *)
Procedure FlatSymbol(C: TAGCanvas; Kind: Integer; cx, cy, r: Double; Col: TAGColor);
Var
  i: Integer;
  a, rr: Double;
Begin
  C.BeginPath;
  Case Kind Of
    0: C.Circle(cx, cy, r);
    1: Begin
        C.Rect(cx - r, cy - 0.34 * r, 2 * r, 0.68 * r);
        C.Rect(cx - 0.34 * r, cy - r, 0.68 * r, 2 * r);
      End;
    2: C.Polygon([cx, cy - r, cx + r, cy, cx, cy + r, cx - r, cy]);
    3: C.Polygon([cx, cy - r, cx + 0.98 * r, cy + 0.75 * r, cx - 0.98 * r, cy + 0.75 * r]);
    4: Begin
        For i := 0 To 9 Do Begin
          a := -AG_HALF_PI + AG_PI * i / 5;
          If odd(i) Then rr := 0.45 * r Else rr := r;
          If i = 0 Then C.MoveTo(cx + rr * AGCos(a), cy + rr * AGSin(a)) Else C.LineTo(cx + rr * AGCos(a), cy + rr * AGSin(a));
        End;
        C.ClosePath;
      End;
    5: Begin
        C.Circle(cx, cy, r);
        C.Circle(cx, cy, 0.5 * r);
        C.FillRule := frEvenOdd;
      End;
    6: C.Polygon([cx - r, cy - 0.55 * r, cx, cy + 0.2 * r, cx + r, cy - 0.55 * r, cx + r, cy + 0.1 * r, cx, cy + 0.85 * r, cx - r, cy + 0.1 * r]);
  Else
    C.RoundRect(cx - 0.8 * r, cy - 0.8 * r, 1.6 * r, 1.6 * r, 0.3 * r);
  End;
  C.Fill(Col);
  C.FillRule := frNonZero;
End;

(* ---------------------------------------------------------------------- *)
(* flat kit                                                               *)
(* ---------------------------------------------------------------------- *)

Procedure GroundFlat(C: TAGCanvas; Const M: TMap);
Var
  P: TFlatPal;
  g1, g2, ac, wh: TAGColor;
  R: TAGRandom;
  i, j, k: Integer;
  x, y: Double;

  Procedure Flower(fx, fy: Double);
  Begin
    Disc(C, fx - 2.4, fy, 2.2, wh);
    Disc(C, fx + 2.4, fy, 2.2, wh);
    Disc(C, fx, fy - 2.4, 2.2, wh);
    Disc(C, fx, fy + 2.4, 2.2, wh);
    Disc(C, fx, fy, 1.7, ac);
  End;

  Procedure Ring(cx, cy, rr, w: Double; Col: TAGColor);
  Begin
    C.BeginPath;
    C.Circle(cx, cy, rr);
    C.Stroke(w, Col);
  End;

Begin
  P := FlatPals[M.Theme];
  g1 := TC(M, P.G1);
  g2 := TC(M, P.G2);
  ac := TC(M, P.Ac);
  wh := AGHex($FFFBF0);
  R := M.A.Stream('flatground');
  Box(C, FX, FY, FR - FX, FB - FY, g1);
  Case P.GPat Of
    0: Begin // mowed lawn: stripes of one cell column, a few flowers
        For i := 0 To GW - 1 Do
          If odd(i) Then Box(C, FX + i * CW, FY, CW, FB - FY, g2);
        For k := 0 To 13 Do
          Flower(FX + R.RangeI(0, GW) * CW, FY + R.RangeI(0, GH) * CH);
      End;
    1: Begin // checker of whole cells with a small square in the middle
        For j := 0 To GH - 1 Do
          For i := 0 To GW - 1 Do Begin
            If (i + j) Mod 2 = 1 Then Box(C, FX + i * CW, FY + j * CH, CW, CH, g2);
            C.RoundRect(FX + i * CW + 16, FY + j * CH + 14, 8, 8, 2);
            C.Fill(AGShade(g2, 0.9));
          End;
      End;
    2: Begin // rink: bold flat markings
        Box(C, FX, FY + 4 * CH, FR - FX, 3 * CH, g2);
        Box(C, 316, FY, 8, FB - FY, ac);
        Box(C, FX + 4 * CW - 3, FY, 7, FB - FY, AGHex($3A86FF));
        Box(C, FX + 11 * CW - 4, FY, 7, FB - FY, AGHex($3A86FF));
        Ring(320, 264, 54, 6, AGHex($3A86FF));
        Disc(C, 320, 264, 6, AGHex($3A86FF));
        For k := 0 To 3 Do Begin
          x := FX + (2.5 + 10 * (k Mod 2)) * CW;
          y := FY + (2.5 + 6 * (k Div 2)) * CH;
          Ring(x, y, 34, 5, ac);
          Disc(C, x, y, 5, ac);
        End;
      End;
    3: Begin // big sandstone tiles of 2 x 2 cells, a triangle on every tile
        For j := 0 To GH - 1 Do
          For i := 0 To GW - 1 Do
            If ((i Div 2) + (j Div 2)) Mod 2 = 1 Then Box(C, FX + i * CW, FY + j * CH, CW, CH, g2);
        For j := 0 To GH Div 2 Do
          For i := 0 To GW Div 2 Do
            FlatSymbol(C, 3, FX + (2 * i + 1) * CW, FY + (2 * j + 1) * CH, 6, AGShade(g2, 0.86));
      End;
    4: Begin // mine: wide diagonal stripes, a few round pebbles
        x := FX - (FB - FY);
        While x < FR Do Begin
          C.Polygon([x, FB, x + 30, FB, x + 30 + (FB - FY), FY, x + (FB - FY), FY]);
          C.Fill(g2);
          x := x + 60;
        End;
        For k := 0 To 16 Do
          Disc(C, FX + 6 + R.NextUnit * (FR - FX - 12), FY + 6 + R.NextUnit * (FB - FY - 12), 2.5 + 2 * R.NextUnit, AGShade(g1, 0.72));
      End;
    5: Begin // beach: wavy bands, a few starfish
        For j := 0 To GH - 1 Do
          If odd(j) Then Begin
            C.BeginPath;
            C.MoveTo(FX, FY + j * CH);
            For i := 0 To 14 Do
              C.QuadTo(FX + i * CW + 20, FY + j * CH - 8, FX + (i + 1) * CW, FY + j * CH);
            C.LineTo(FR, FY + (j + 1) * CH);
            For i := 14 Downto 0 Do
              C.QuadTo(FX + i * CW + 20, FY + (j + 1) * CH - 8, FX + i * CW, FY + (j + 1) * CH);
            C.ClosePath;
            C.Fill(g2);
          End;
        For k := 0 To 6 Do
          FlatSymbol(C, 4, FX + (R.RangeI(0, GW - 1) + 0.5) * CW, FY + (R.RangeI(0, GH - 1) + 0.5) * CH, 6, ac);
      End;
    6: Begin // alien: grid lines and glowing dots on the crossings
        For i := 0 To GW Do
          Box(C, FX + i * CW - 1, FY, 2, FB - FY, g2);
        For j := 0 To GH Do
          Box(C, FX, FY + j * CH - 1, FR - FX, 2, g2);
        For j := 0 To GH - 1 Do
          For i := 0 To GW - 1 Do
            Disc(C, FX + i * CW + CW / 2, FY + j * CH + CH / 2, 5, g2);
        For j := 0 To GH Do
          For i := 0 To GW Do
            If (i + j) Mod 3 = 0 Then Disc(C, FX + i * CW, FY + j * CH, 2.6, ac);
      End;
    7: Begin // haunted: flat vertical planks with a dark gap
        For i := 0 To 2 * GW - 1 Do Begin
          If odd(i + (i Div 5)) Then Box(C, FX + i * 20, FY, 20, FB - FY, g2);
          Box(C, FX + i * 20, FY, 2, FB - FY, AGShade(g1, 0.72));
        End;
      End;
    8: Begin // water: fish scale waves
        For j := 0 To 2 * GH Do
          For i := 0 To GW Do Begin
            x := FX + i * CW + 20 * (j Mod 2);
            y := FY + j * 18;
            C.BeginPath;
            C.ArcTo(x, y, 20, 18, 0, AG_PI);
            C.ClosePath;
            If odd(j) Then C.Fill(g2) Else C.Fill(AGShade(g1, 1.06));
          End;
      End;
    9: Begin // forest: polka dots on the cell corners
        For j := 0 To GH Do
          For i := 0 To GW Do
            Disc(C, FX + i * CW, FY + j * CH, 10, g2);
        For k := 0 To 9 Do
          FlatSymbol(C, 2, FX + (R.RangeI(0, GW - 1) + 0.5) * CW, FY + (R.RangeI(0, GH - 1) + 0.5) * CH, 4, ac);
      End;
  Else Begin // city: slabs with wide joints and a crosswalk
      Box(C, FX, FY, FR - FX, FB - FY, g2);
      For j := 0 To GH - 1 Do
        For i := 0 To GW - 1 Do
          Box(C, FX + i * CW + 2, FY + j * CH + 2, CW - 4, CH - 4, g1);
      For i := 0 To GW - 1 Do Begin
        Box(C, FX + i * CW + 4, FY + 5 * CH + 4, 14, CH - 8, AGHex($F4F4F0));
        Box(C, FX + i * CW + 24, FY + 5 * CH + 4, 14, CH - 8, AGHex($F4F4F0));
      End;
    End;
  End;
  // the frame casts a hard flat shadow onto the ground
  Box(C, FX, FY, FR - FX, 6, AGRGBA(0, 0, 0, 60));
  Box(C, FX, FY + 6, 6, FB - FY - 6, AGRGBA(0, 0, 0, 60));
End;

Procedure FrameFlat(C: TAGCanvas; Const M: TMap);
Var
  P: TFlatPal;
  f1, f2: TAGColor;

  Procedure S(x, y: Double; Index: Integer);
  Begin
    FlatSymbol(C, P.FSym, x, y, 5.5, f2);
  End;

Begin
  P := FlatPals[M.Theme];
  f1 := TC(M, P.F1);
  f2 := TC(M, P.F2);
  Box(C, 0, 44, 640, 436, f1);
  // a lighter band along the outer edge
  Box(C, 0, 44, 640, 4, AGShade(f1, 1.35));
  Box(C, 0, 476, 640, 4, AGShade(f1, 0.7));
  ForBand(40, @S);
  FieldRim(C, M, f2, 4);
  FieldRim(C, M, AGHex(InkHex), 1.5);
End;

Procedure FlatBlock(C: TAGCanvas; Const M: TMap; Solid: Boolean);
Var
  P: TFlatPal;
  ink, so, so2: TAGColor;
Begin
  P := FlatPals[M.Theme];
  ink := AGHex(InkHex);
  If Not Solid Then Begin
    C.RoundRect(3, 3, 34, 30, 8);
    ShapeO(C, M, TC(M, P.Br), ink, 1.0);
    FlatSymbol(C, P.BSym, 20, 17, 7.5, TC(M, P.Br2));
  End
  Else Begin
    so := TC(M, P.So);
    so2 := TC(M, P.So2);
    // a heavy block: front face and top face, four bolts
    C.RoundRect(1.5, 1.5, 37, 33, 4);
    ShapeO(C, M, AGShade(so, 0.68), ink, 1.0);
    C.RoundRect(1.5, 1.5, 37, 25, 4);
    ShapeO(C, M, so, ink, 0.8);
    Disc(C, 7, 7, 2.2, so2);
    Disc(C, 33, 7, 2.2, so2);
    Disc(C, 7, 21, 2.2, so2);
    Disc(C, 33, 21, 2.2, so2);
    C.RoundRect(13, 8, 14, 12, 3);
    C.Fill(AGShade(so, 0.8));
  End;
End;

(* ---------------------------------------------------------------------- *)
(* pixel kit                                                              *)
(* ---------------------------------------------------------------------- *)

Procedure GroundPixel(C: TAGCanvas; Const M: TMap);
Const
  TW = 20; // tile = half a cell
  TH = 18;
  Leaf: Array[0..2] Of Cardinal = ($C07828, $A0502A, $D0A040);
Var
  P: TPixPal;
  a, b: TRamp;
  R: TAGRandom;
  i, j, k: Integer;
  x, y, w: Double;
  col: TAGColor;
Begin
  P := PixPals[M.Theme];
  MakeRamp(TC(M, P.G1), a);
  MakeRamp(TC(M, P.G2), b);
  R := M.A.Stream('pixtiles');
  Box(C, FX, FY, FR - FX, FB - FY, a[2]);
  Case P.GTile Of
    0: Begin // grass: checker of cells, v shaped tufts on every tile
        For j := 0 To GH - 1 Do
          For i := 0 To GW - 1 Do
            If (i + j) Mod 2 = 1 Then Box(C, FX + i * CW, FY + j * CH, CW, CH, b[2]);
        For j := 0 To 2 * GH - 1 Do
          For i := 0 To 2 * GW - 1 Do Begin
            x := FX + i * TW + 4 + 2 * R.RangeI(0, 4);
            y := FY + j * TH + 4 + 2 * R.RangeI(0, 3);
            Box(C, x, y, 2, 2, a[1]);
            Box(C, x + 4, y, 2, 2, a[1]);
            Box(C, x + 2, y + 2, 2, 2, a[1]);
            Box(C, x + 2, y - 2, 2, 2, a[3]);
          End;
      End;
    1: Begin // classic: checker of bevelled cells
        For j := 0 To GH - 1 Do
          For i := 0 To GW - 1 Do Begin
            If (i + j) Mod 2 = 0 Then col := a[2] Else col := b[2];
            PixBevel(C, FX + i * CW, FY + j * CH, CW, CH, AGMix(col, a[3], 0.6), col, AGMix(col, a[1], 0.6), 2);
            Box(C, FX + i * CW + 18, FY + j * CH + 16, 4, 4, AGMix(col, a[1], 0.5));
          End;
      End;
    2: Begin // ice: shiny tiles with a glint, rink lines
        For j := 0 To GH - 1 Do
          For i := 0 To GW - 1 Do Begin
            If (i + j) Mod 2 = 0 Then col := a[2] Else col := b[2];
            PixBevel(C, FX + i * CW, FY + j * CH, CW, CH, a[4], col, AGMix(col, a[1], 0.5), 2);
            If (i * 3 + j) Mod 4 = 0 Then Begin
              Box(C, FX + i * CW + 6, FY + j * CH + 12, 2, 2, a[4]);
              Box(C, FX + i * CW + 8, FY + j * CH + 10, 2, 2, a[4]);
              Box(C, FX + i * CW + 10, FY + j * CH + 8, 2, 2, a[4]);
            End;
          End;
        Box(C, 316, FY, 6, FB - FY, AGHex($D03848));
        Box(C, FX + 4 * CW - 2, FY, 4, FB - FY, AGHex($4060D0));
        Box(C, FX + 11 * CW - 2, FY, 4, FB - FY, AGHex($4060D0));
        C.BeginPath;
        C.Circle(320, 264, 52);
        C.Stroke(4, AGHex($4060D0));
      End;
    3: Begin // egypt: bevelled sandstone slabs of one cell with pores
        For j := 0 To GH - 1 Do
          For i := 0 To GW - 1 Do Begin
            If R.NextUnit < 0.5 Then col := a[2] Else col := b[2];
            PixBevel(C, FX + i * CW, FY + j * CH, CW, CH, a[3], col, a[1], 2);
            Box(C, FX + i * CW + 6 + 2 * R.RangeI(0, 12), FY + j * CH + 6 + 2 * R.RangeI(0, 10), 2, 2, a[1]);
            Box(C, FX + i * CW + 6 + 2 * R.RangeI(0, 12), FY + j * CH + 6 + 2 * R.RangeI(0, 10), 2, 2, a[1]);
          End;
      End;
    4, 10: Begin // mine / city: cobbles / paving bricks of half a cell, staggered
        Box(C, FX, FY, FR - FX, FB - FY, a[0]);
        For j := 0 To 2 * GH - 1 Do Begin
          x := FX - 10 * (j Mod 2);
          While x < FR Do Begin
            If R.NextUnit < 0.5 Then col := a[2] Else col := b[2];
            PixBevel(C, x, FY + j * TH, TW - 2, TH - 2, a[3], col, a[1], 2);
            If (P.GTile = 4) And (R.NextUnit < 0.2) Then Box(C, x + 6, FY + j * TH + 6, 4, 2, AGHex($181618));
            x := x + TW;
          End;
        End;
        If P.GTile = 10 Then
          For k := 0 To 8 Do
            Box(C, FX + 2 * R.RangeI(2, 296), FY + 2 * R.RangeI(2, 194), 6, 4, AGHex($E8E4D8));
      End;
    5: Begin // beach: sand with a fixed dot pattern and ripple marks
        For j := 0 To 2 * GH - 1 Do
          For i := 0 To 2 * GW - 1 Do Begin
            x := FX + i * TW;
            y := FY + j * TH;
            Box(C, x + 2 + 10 * (j Mod 2), y + 4, 2, 2, a[3]);
            Box(C, x + 12 - 10 * (j Mod 2), y + 12, 2, 2, a[1]);
            If R.NextUnit < 0.3 Then Begin
              Box(C, x + 4, y + 8, 8, 2, a[3]);
              Box(C, x + 6, y + 10, 8, 2, a[1]);
            End;
          End;
      End;
    6: Begin // alien: metal plates with rivets, some glowing grooves
        For j := 0 To GH - 1 Do
          For i := 0 To GW - 1 Do Begin
            x := FX + i * CW;
            y := FY + j * CH;
            PixBevel(C, x, y, CW, CH, a[3], a[2], a[0], 2);
            Box(C, x + 4, y + 4, 2, 2, a[4]);
            Box(C, x + 34, y + 4, 2, 2, a[4]);
            Box(C, x + 4, y + 30, 2, 2, a[1]);
            Box(C, x + 34, y + 30, 2, 2, a[1]);
            If (i + 2 * j) Mod 3 = 0 Then Begin
              Box(C, x + 10, y + 16, 20, 4, a[0]);
              If R.NextUnit < 0.5 Then Box(C, x + 12, y + 16, 16, 2, TC(M, $50E0D0));
            End;
          End;
      End;
    7: Begin // haunted: planks of 12 pixel, staggered joints
        For j := 0 To GH * 3 - 1 Do Begin
          y := FY + j * 12;
          x := FX - (j Mod 4) * 20;
          While x < FR Do Begin
            w := 40 + 40 * R.RangeI(0, 2);
            If R.NextUnit < 0.5 Then col := a[2] Else col := b[2];
            Box(C, x, y, w, 12, col);
            Box(C, x, y, w, 2, a[3]);
            Box(C, x, y + 10, w, 2, a[0]);
            Box(C, x + w - 2, y, 2, 12, a[0]);
            Box(C, x + 8, y + 6, w / 2, 2, a[1]);
            Box(C, x + 4, y + 4, 2, 2, a[0]);
            x := x + w;
          End;
        End;
      End;
    8: Begin // sea floor: wave marks on every second tile
        For j := 0 To 2 * GH - 1 Do
          For i := 0 To 2 * GW - 1 Do Begin
            x := FX + i * TW;
            y := FY + j * TH;
            If (i + j) Mod 2 = 0 Then Begin
              Box(C, x + 2, y + 8, 4, 2, a[4]);
              Box(C, x + 6, y + 6, 4, 2, a[4]);
              Box(C, x + 10, y + 8, 4, 2, a[4]);
              Box(C, x + 14, y + 6, 4, 2, a[4]);
            End
            Else
              Box(C, x + 8, y + 10, 2, 2, a[1]);
          End;
      End;
  Else Begin // forest: dirt and moss tiles with leaves
      For j := 0 To 2 * GH - 1 Do
        For i := 0 To 2 * GW - 1 Do Begin
          x := FX + i * TW;
          y := FY + j * TH;
          If R.NextUnit < 0.45 Then Begin
            Box(C, x, y, TW, TH, b[2]);
            Box(C, x + 4, y + 4, 2, 2, b[3]);
            Box(C, x + 12, y + 10, 2, 2, b[1]);
          End;
          For k := 0 To 1 Do Begin
            col := Jit(AGHex(Leaf[R.RangeI(0, 2)]), 0, 1, M.Light);
            Box(C, x + 2 * R.RangeI(1, 7), y + 2 * R.RangeI(1, 7), 4, 2, col);
          End;
        End;
    End;
  End;
  // hard shadow of the frame
  Box(C, FX, FY, FR - FX, 4, AGRGBA(0, 0, 0, 90));
  Box(C, FX, FY + 4, 4, FB - FY - 4, AGRGBA(0, 0, 0, 90));
End;

Procedure FramePixel(C: TAGCanvas; Const M: TMap);
Var
  f: TRamp;
  i, j: Integer;

  Procedure Blk(x, y, w, h: Double; Alt: Boolean);
  Var
    mid: TAGColor;
  Begin
    If Alt Then mid := AGMix(f[2], f[1], 0.35) Else mid := f[2];
    PixBevel(C, x, y, w - 2, h - 2, f[3], mid, f[1], 2);
  End;

Begin
  MakeRamp(TC(M, PixPals[M.Theme].Fr), f);
  Box(C, 0, 44, 640, 436, f[0]);
  For i := 0 To 31 Do Begin
    Blk(i * 20, 46, 20, 20, odd(i));
    Blk(i * 20, FB, 20, 18, Not odd(i));
  End;
  For j := 0 To 2 * GH - 1 Do Begin
    Blk(0, FY + j * 18, 20, 18, odd(j));
    Blk(FR, FY + j * 18, 20, 18, Not odd(j));
  End;
  FieldRim(C, M, f[0], 2);
End;

Procedure PixelBlock(C: TAGCanvas; Const M: TMap; Solid: Boolean);
Var
  P: TPixPal;
  b: TRamp;
  R: TAGRandom;
  k, i, j, hw: Integer;
  x, y: Double;
  grey: TAGColor;
Begin
  P := PixPals[M.Theme];
  R := M.A.Stream('pixblock');
  If Solid Then Begin
    // hard block: wide bevel and a raised stud in the middle
    MakeRamp(TC(M, P.So), b);
    Box(C, 0, 0, 40, 36, b[0]);
    PixBevel(C, 2, 2, 36, 32, b[4], b[2], b[1], 4);
    PixBevel(C, 14, 12, 12, 12, b[4], b[3], b[1], 2);
    exit;
  End;
  MakeRamp(TC(M, P.Br), b);
  Box(C, 0, 0, 40, 36, b[0]);
  PixBevel(C, 2, 2, 36, 32, b[3], b[2], b[1], 2);
  Case P.BMotif Of
    0: Begin // crate: planks, a stepped brace and nails
        Box(C, 4, 12, 32, 2, b[1]);
        Box(C, 4, 22, 32, 2, b[1]);
        For k := 0 To 12 Do
          Box(C, 6 + 2 * k, 28 - 2 * AGRound(k * 11 / 12), 4, 2, b[1]);
        Box(C, 4, 4, 2, 2, b[0]);
        Box(C, 34, 4, 2, 2, b[0]);
        Box(C, 4, 30, 2, 2, b[0]);
        Box(C, 34, 30, 2, 2, b[0]);
      End;
    1: Begin // clay bricks
        For j := 0 To 2 Do Begin
          y := 2 + j * 11;
          Box(C, 2, y + 9, 36, 2, b[0]);
          Box(C, 4, y + 2, 32, 2, b[3]);
          If odd(j) Then Begin
            Box(C, 12, y, 2, 10, b[0]);
            Box(C, 28, y, 2, 10, b[0]);
          End
          Else
            Box(C, 20, y, 2, 10, b[0]);
        End;
      End;
    2: Begin // ice block: glints
        Box(C, 6, 6, 10, 2, b[4]);
        Box(C, 6, 8, 2, 6, b[4]);
        Box(C, 22, 24, 2, 2, b[4]);
        Box(C, 24, 22, 2, 2, b[4]);
        Box(C, 26, 20, 2, 2, b[4]);
        Box(C, 28, 18, 2, 2, b[4]);
        Box(C, 12, 26, 6, 2, b[1]);
      End;
    3: Begin // sandstone with a carved ankh
        Box(C, 18, 6, 4, 2, b[0]);
        Box(C, 16, 8, 2, 6, b[0]);
        Box(C, 22, 8, 2, 6, b[0]);
        Box(C, 18, 14, 4, 2, b[0]);
        Box(C, 18, 16, 4, 14, b[0]);
        Box(C, 12, 18, 16, 2, b[0]);
        Box(C, 20, 8, 2, 6, b[3]);
      End;
    4: Begin // coal: bevelled lumps
        For j := 0 To 2 Do
          For i := 0 To 2 Do
            PixBevel(C, 4 + 11 * i + 2 * (j Mod 2), 4 + 10 * j, 10, 8, b[3], b[2], b[0], 2);
        For k := 0 To 3 Do
          Box(C, 6 + 2 * R.RangeI(0, 13), 6 + 2 * R.RangeI(0, 11), 2, 2, AGHex($A0A8C0));
      End;
    5: Begin // sand castle: notches, windows and a door
        Box(C, 12, 2, 4, 6, b[1]);
        Box(C, 24, 2, 4, 6, b[1]);
        Box(C, 2, 8, 36, 2, b[1]);
        Box(C, 6, 14, 4, 4, b[0]);
        Box(C, 30, 14, 4, 4, b[0]);
        Box(C, 16, 22, 8, 12, b[0]);
        Box(C, 18, 20, 4, 2, b[0]);
      End;
    6: Begin // crystal: stepped diamond
        For k := 0 To 11 Do Begin
          y := 6 + 2 * k;
          hw := 2 + 2 * AGRound(AGMin(k, 11 - k) * 0.9);
          Box(C, 20 - hw, y, 2 * hw, 2, b[3]);
          Box(C, 20 - hw, y, 2, 2, b[4]);
          Box(C, 18 + hw, y, 2, 2, b[1]);
        End;
        Box(C, 18, 8, 2, 20, b[4]);
      End;
    7: Begin // barrel: staves and two metal hoops
        grey := AGHex($6A6A74);
        Box(C, 12, 2, 2, 32, b[1]);
        Box(C, 26, 2, 2, 32, b[1]);
        Box(C, 2, 8, 36, 4, grey);
        Box(C, 2, 8, 36, 2, AGShade(grey, 1.4));
        Box(C, 2, 24, 36, 4, grey);
        Box(C, 2, 24, 36, 2, AGShade(grey, 1.4));
      End;
    8: Begin // coral: polka dots
        For j := 0 To 2 Do
          For i := 0 To 3 Do Begin
            x := 5 + 8 * i + 2 * (j Mod 2);
            y := 6 + 9 * j;
            Box(C, x, y, 4, 4, b[4]);
            Box(C, x + 2, y + 4, 2, 2, b[0]);
          End;
      End;
    9: Begin // bush: leaf clusters and berries
        For k := 0 To 15 Do Begin
          x := 4 + 2 * R.RangeI(0, 14);
          y := 4 + 2 * R.RangeI(0, 12);
          Box(C, x, y, 4, 2, b[4]);
          Box(C, x + 2, y + 2, 2, 2, b[1]);
        End;
        For k := 0 To 2 Do
          Box(C, 6 + 2 * R.RangeI(0, 13), 6 + 2 * R.RangeI(0, 11), 2, 2, AGHex($E03838));
      End;
  Else Begin // cardboard box: tape and a label
      Box(C, 16, 2, 8, 32, b[3]);
      Box(C, 16, 2, 2, 32, b[4]);
      Box(C, 6, 8, 8, 8, AGHex($E8E4D8));
      Box(C, 8, 10, 4, 2, b[0]);
      Box(C, 8, 13, 4, 2, b[0]);
    End;
  End;
End;

(* ---------------------------------------------------------------------- *)
(* soft kit: light over the whole field, blocks cast a shadow             *)
(* ---------------------------------------------------------------------- *)

(* Light from the top left over the play field *)
Procedure SoftFieldLight(C: TAGCanvas; Const M: TMap);
Begin
  C.Rect(FX, FY, FR - FX, FB - FY);
  C.FillPaint(C.Linear(FX, FY, FR, FB, [AGRGBA(255, 250, 225, 46), AGRGBA(255, 250, 225, 0), AGRGBA(10, 0, 30, 0), AGRGBA(10, 0, 30, 56)]));
End;

(*
 * Opaque soft shadow of a block towards the bottom right, in a darker tone of
 * the ground (the cell is keyed, a translucent shadow would get lost)
 *)
Procedure SoftBlockShadow(Img: TAGImage; Const M: TMap);
Var
  src: TAGImage;
  x, y, d: Integer;
  g: TAGColor;
Begin
  src := Img.Clone;
  Try
    g := TC(M, SoftGround[M.Theme]);
    For y := 0 To Img.Height - 1 Do
      For x := 0 To Img.Width - 1 Do Begin
        If src.GetPixel(x, y).a >= 128 Then Continue;
        For d := 1 To 4 Do
          If src.GetPixel(x - d, y - d).a >= 128 Then Begin
            Img.SetPixel(x, y, AGShade(g, 0.5 + 0.08 * d));
            break;
          End;
      End;
  Finally
    src.Free;
  End;
End;

(* ---------------------------------------------------------------------- *)
(* field.png                                                              *)
(* ---------------------------------------------------------------------- *)

Procedure DrawFieldImage(C: TAGCanvas; Const M: TMap);
Var
  rim, inner: TAGColor;
Begin
  // frame
  If Kit(M) = kkFlat Then FrameFlat(C, M)
  Else If Kit(M) = kkTile Then FramePixel(C, M)
  Else
  Case M.Theme Of
    thMeadow: FrameStones(C, M, TC(M, $34362F), TC(M, $8A8C84), 12);
    thClassic: FrameTiles(C, M, TC(M, $50545A), TC(M, $A4A8AE), 20);
    thIce: FrameIce(C, M);
    thEgypt: FrameEgypt(C, M);
    thMine: FrameMine(C, M);
    thBeach: FrameBeach(C, M);
    thAlien: FrameAlien(C, M);
    thHaunted: FrameHaunted(C, M);
    thWater: FrameWater(C, M);
    thForest: FrameForest(C, M);
    thCity: FrameCity(C, M);
  End;
  // ground, exactly inside the play field
  ClipTo(C, M, FX, FY, FR, FB);
  If Kit(M) = kkFlat Then GroundFlat(C, M)
  Else If Kit(M) = kkTile Then GroundPixel(C, M)
  Else Begin
  Case M.Theme Of
    thMeadow: GroundMeadow(C, M);
    thClassic: GroundClassic(C, M);
    thIce: GroundIce(C, M);
    thEgypt: GroundEgypt(C, M);
    thMine: GroundMine(C, M);
    thBeach: GroundBeach(C, M);
    thAlien: GroundAlien(C, M);
    thHaunted: GroundHaunted(C, M);
    thWater: GroundWater(C, M);
    thForest: GroundForest(C, M);
    thCity: GroundCity(C, M);
  End;
  If M.Style = asSoft Then SoftFieldLight(C, M);
  If M.Theme <> thIce Then InnerShadow(C, M, 90) Else InnerShadow(C, M, 40);
  End;
  C.ResetClip;
  // HUD on top
  Box(C, 0, 0, 640, 46, AGHex($0C0C10));
  Case M.Theme Of
    thMeadow: rim := TC(M, $6A6C64);
    thClassic: rim := TC(M, $8A8E94);
    thIce: rim := TC(M, $B8C8D8);
    thEgypt: rim := TC(M, $C49A5C);
    thMine: rim := TC(M, $7A5430);
    thBeach: rim := TC(M, $C8A870);
    thAlien: rim := TC(M, $4A6A80);
    thHaunted: rim := TC(M, $6A607A);
    thWater: rim := TC(M, $3A7A9A);
    thForest: rim := TC(M, $5A7A3A);
  Else
    rim := TC(M, $8A4A3A);
  End;
  If Kit(M) = kkFlat Then rim := TC(M, FlatPals[M.Theme].F2)
  Else If Kit(M) = kkTile Then rim := TC(M, PixPals[M.Theme].Fr);
  inner := AGHex($08080A);
  DrawHud(C, M, rim, inner);
End;

(* ---------------------------------------------------------------------- *)
(* Blocks                                                                 *)
(* ---------------------------------------------------------------------- *)

(* Box seen slightly from above: top face and a darker front face *)
Procedure Block3D(C: TAGCanvas; Const M: TMap; x, y, w, h, Depth, Radius: Double; Top: TAGColor);
Begin
  C.RoundRect(x, y, w, h, Radius);
  Shape(C, M, AGShade(Top, 0.68), 1.0);
  C.RoundRect(x, y, w, h - Depth, Radius);
  Shape(C, M, Top, 0.8);
End;

Procedure DrawBrick(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  col, c2: TAGColor;
  i, j: Integer;
  x, y: Double;
Begin
  If Kit(M) = kkFlat Then Begin
    FlatBlock(C, M, false);
    exit;
  End;
  If Kit(M) = kkTile Then Begin
    PixelBlock(C, M, false);
    exit;
  End;
  R := M.A.Stream('brick');
  Case M.Theme Of
    thMeadow: Begin // wooden crate
        col := TC(M, $B07A40);
        Block3D(C, M, 2, 2, 36, 33, 6, 2, col);
        c2 := AGShade(col, 0.7);
        For i := 1 To 3 Do
          Line(C, 3, 2 + i * 6.75, 37, 2 + i * 6.75, Pen(M, 0.8), c2);
        Line(C, 5, 26, 35, 5, Pen(M, 3), AGShade(col, 0.85));
        C.BeginPath;
        C.Rect(4, 4, 32, 23);
        C.Stroke(Pen(M, 2), AGShade(col, 0.6));
        Disc(C, 6, 6, 1, AGHex($404040));
        Disc(C, 34, 6, 1, AGHex($404040));
        Disc(C, 6, 25, 1, AGHex($404040));
        Disc(C, 34, 25, 1, AGHex($404040));
      End;
    thClassic: Begin // clay bricks, fills the whole cell
        Box(C, 0, 0, 40, 36, TC(M, $C8C0B0));
        col := TC(M, $B04A30);
        For j := 0 To 3 Do Begin
          x := -10 * (j Mod 2);
          While x < 40 Do Begin
            C.Rect(AGMax(x + 1, 1), j * 9 + 1, AGMin(x + 19, 39) - AGMax(x + 1, 1), 7.5);
            Shape(C, M, Jit(col, R.Range(-4, 4), 1, 0.85 + 0.25 * R.NextUnit), 0.5);
            x := x + 20;
          End;
        End;
      End;
    thIce: Begin // ice cube
        col := TC(M, $A8E0F4);
        Block3D(C, M, 2, 1, 36, 34, 7, 5, col);
        Line(C, 8, 6, 16, 6, Pen(M, 2), AGRGBA(255, 255, 255, 220));
        Line(C, 7, 9, 7, 16, Pen(M, 2), AGRGBA(255, 255, 255, 200));
        Line(C, 22, 18, 30, 12, Pen(M, 1), AGShade(col, 0.8));
        Line(C, 26, 22, 32, 20, Pen(M, 1), AGShade(col, 0.8));
      End;
    thEgypt: Begin // sandstone block with a carved sign
        col := TC(M, $E0B878);
        Block3D(C, M, 1, 1, 38, 34, 6, 2, col);
        c2 := AGShade(col, 0.62);
        C.BeginPath;
        C.Rect(6, 5, 28, 19);
        C.Stroke(Pen(M, 1), c2);
        C.BeginPath;
        C.Ellipse(20, 12, 3, 3.5);
        C.Stroke(Pen(M, 1.4), c2);
        Line(C, 20, 15.5, 20, 22, Pen(M, 1.4), c2);
        Line(C, 16, 17.5, 24, 17.5, Pen(M, 1.4), c2);
      End;
    thMine: Begin // coal heap
        Lump(C, R, 20, 23, 19, 12, 0.08, 9);
        ShapeO(C, M, AGHex($242226), AGHex($0A0A0C), 1);
        For i := 0 To 7 Do Begin
          x := 8 + (i Mod 4) * 8 + R.Range(-1.5, 1.5);
          y := 25 - (i Div 4) * 10 + R.Range(-1.5, 1.5);
          If i >= 6 Then Begin
            x := 15 + (i - 6) * 10;
            y := 7;
          End;
          Lump(C, R, x, y, R.Range(6, 8), R.Range(5, 7), 0.25, 7);
          ShapeO(C, M, Jit(AGHex($34323A), 0, 1, 0.8 + 0.5 * R.NextUnit), AGHex($0A0A0C), 1);
          Line(C, x - 3, y - 3, x, y - 4, Pen(M, 1.1), AGHex($9098B0));
        End;
      End;
    thBeach: Begin // sand castle
        col := TC(M, $D8A858);
        Oval(C, 20, 33, 19, 3, AGShade(col, 0.7));
        C.BeginPath;
        C.MoveTo(2, 34);
        C.LineTo(2, 4);
        C.LineTo(9, 4);
        C.LineTo(9, 9);
        C.LineTo(12.5, 9);
        C.LineTo(12.5, 13);
        C.LineTo(16, 13);
        C.LineTo(16, 9);
        C.LineTo(24, 9);
        C.LineTo(24, 13);
        C.LineTo(27.5, 13);
        C.LineTo(27.5, 9);
        C.LineTo(31, 9);
        C.LineTo(31, 4);
        C.LineTo(38, 4);
        C.LineTo(38, 34);
        C.ClosePath;
        Shape(C, M, col, 1);
        // towers
        Box(C, 3, 4, 2, 3, AGShade(col, 1.25));
        Box(C, 32, 4, 2, 3, AGShade(col, 1.25));
        Line(C, 3, 18, 37, 18, Pen(M, 1), AGShade(col, 0.75));
        Line(C, 3, 26, 37, 26, Pen(M, 1), AGShade(col, 0.75));
        C.BeginPath;
        C.MoveTo(15, 34);
        C.LineTo(15, 25);
        C.QuadTo(20, 18, 25, 25);
        C.LineTo(25, 34);
        Shape(C, M, AGShade(col, 0.45), 0.6);
        Disc(C, 5.5, 13, 1.4, AGShade(col, 0.5));
        Disc(C, 34.5, 13, 1.4, AGShade(col, 0.5));
        Line(C, 34.5, 4, 34.5, 0.5, Pen(M, 0.8), AGHex($604020));
        C.Polygon([34.5, 0.5, 39.5, 1.8, 34.5, 3.2]);
        C.Fill(AGHex($E04030));
      End;
    thAlien: Begin // crystal cluster
        For i := 0 To 4 Do Begin
          x := 7 + i * 6.5;
          C.Push;
          C.Translate(x, 33);
          C.Rotate(AGDeg(-30 + i * 15 + R.Range(-5, 5)));
          y := 18 + 10 * R.NextUnit;
          If i = 2 Then y := 30;
          C.Polygon([-4, 0, -4, -y + 5, 0, -y, 4, -y + 5, 4, 0]);
          Shape(C, M, Jit(TC(M, $60D8E8), R.Range(-15, 15), 1, 0.85 + 0.2 * R.NextUnit), 0.8);
          Line(C, -1.5, -3, -1.5, -y + 6, Pen(M, 1), AGRGBA(255, 255, 255, 180));
          C.Pop;
        End;
        Oval(C, 20, 33, 17, 3, AGHex($303040));
      End;
    thHaunted: Begin // old barrel
        col := TC(M, $7A5030);
        C.BeginPath;
        C.MoveTo(8, 2);
        C.LineTo(32, 2);
        C.QuadTo(39, 18, 32, 34);
        C.LineTo(8, 34);
        C.QuadTo(1, 18, 8, 2);
        Shape(C, M, col, 1);
        For i := 1 To 3 Do
          Line(C, 6 + i * 7, 3, 6 + i * 7, 33, Pen(M, 0.8), AGShade(col, 0.65));
        Line(C, 5, 9, 35, 9, Pen(M, 2.4), AGHex($505058));
        Line(C, 5, 27, 35, 27, Pen(M, 2.4), AGHex($505058));
        Oval(C, 20, 4, 11, 2.5, AGShade(col, 0.5));
      End;
    thWater: Begin // coral clump
        For i := 0 To 5 Do Begin
          x := 8 + (i Mod 3) * 12 + R.Range(-2, 2);
          y := 25 - (i Div 3) * 12 + R.Range(-2, 2);
          Lump(C, R, x, y, R.Range(7, 9), R.Range(7, 9), 0.2, 8);
          Shape(C, M, Jit(TC(M, $F07848), R.Range(-12, 12), 1, 0.85 + 0.25 * R.NextUnit), 0.9);
          Disc(C, x - 2, y - 2, 1.1, AGShade(TC(M, $F07848), 1.5));
          Disc(C, x + 2, y + 1, 1, AGShade(TC(M, $F07848), 0.6));
          Disc(C, x + 1, y - 3, 0.9, AGShade(TC(M, $F07848), 0.6));
        End;
      End;
    thForest: Begin // bush
        Lump(C, R, 20, 21, 18, 14, 0.1, 10);
        Shape(C, M, AGShade(TC(M, $3C7228), 0.8), 1);
        For i := 0 To 6 Do Begin
          x := 9 + (i Mod 4) * 7.5 + R.Range(-1.5, 1.5);
          y := 26 - (i Div 4) * 12 + R.Range(-1, 1);
          If i = 6 Then Begin
            x := 20;
            y := 9;
          End;
          Lump(C, R, x, y, R.Range(7, 9), R.Range(6, 8), 0.22, 9);
          Shape(C, M, Jit(TC(M, $4C8A30), R.Range(-8, 8), 1, 0.85 + 0.3 * R.NextUnit), 0.8);
        End;
        For i := 0 To 4 Do
          Disc(C, 7 + R.NextUnit * 26, 6 + R.NextUnit * 24, 1.5, AGHex($D83838));
      End;
    thCity: Begin // cardboard box
        col := TC(M, $B89058);
        Block3D(C, M, 3, 3, 34, 31, 8, 1, col);
        Box(C, 17, 3, 6, 23, AGHex($D8C8A0));
        Line(C, 6, 9, 14, 9, Pen(M, 1), AGShade(col, 0.6));
        Line(C, 6, 12, 12, 12, Pen(M, 1), AGShade(col, 0.6));
        C.Polygon([28, 15, 31, 20, 25, 20]);
        C.Fill(AGShade(col, 0.55));
      End;
  End;
End;

Procedure DrawSolid(C: TAGCanvas; Const M: TMap);
Var
  R: TAGRandom;
  col, c2: TAGColor;
  i: Integer;
  x: Double;
Begin
  If Kit(M) = kkFlat Then Begin
    FlatBlock(C, M, true);
    exit;
  End;
  If Kit(M) = kkTile Then Begin
    PixelBlock(C, M, true);
    exit;
  End;
  R := M.A.Stream('solid');
  Case M.Theme Of
    thMeadow: Begin // stone pillar
        col := TC(M, $9A9A92);
        Block3D(C, M, 1, 1, 38, 34, 8, 6, col);
        Line(C, 10, 8, 16, 14, Pen(M, 1), AGShade(col, 0.6));
        Line(C, 16, 14, 14, 20, Pen(M, 1), AGShade(col, 0.6));
        Line(C, 6, 5, 18, 5, Pen(M, 1.6), AGShade(col, 1.3));
        Disc(C, 30, 18, 2, AGHex($6A8A40));
        Disc(C, 33, 20, 1.6, AGHex($5A7A38));
      End;
    thClassic: Begin // beveled concrete block
        col := TC(M, $A0A4AA);
        Box(C, 0, 0, 40, 36, AGShade(col, 0.55));
        C.Polygon([0, 0, 40, 0, 35, 5, 5, 5, 5, 31, 0, 36]);
        C.Fill(AGShade(col, 1.3));
        C.Polygon([40, 0, 40, 36, 0, 36, 5, 31, 35, 31, 35, 5]);
        C.Fill(AGShade(col, 0.7));
        C.Rect(5, 5, 30, 26);
        Shape(C, M, col, 0);
        Box(C, 12, 11, 16, 14, AGShade(col, 0.88));
      End;
    thIce: Begin // steel barrier with snow
        col := TC(M, $405878);
        Block3D(C, M, 1, 2, 38, 33, 8, 3, col);
        For i := 0 To 2 Do
          Disc(C, 8 + i * 12, 22, 1.5, AGShade(col, 1.5));
        C.BeginPath;
        C.MoveTo(2, 8);
        C.QuadTo(6, 1, 14, 3);
        C.QuadTo(22, 0, 30, 3);
        C.QuadTo(38, 2, 38, 9);
        C.QuadTo(30, 11, 20, 9);
        C.QuadTo(10, 12, 2, 8);
        Shape(C, M, AGHex($F4F8FC), 0.6);
      End;
    thEgypt: Begin // granite pyramid seen from above
        col := TC(M, $7A6650);
        C.Polygon([1, 1, 39, 1, 20, 18]);
        Shape(C, M, AGShade(col, 1.25), 0.7);
        C.Polygon([39, 1, 39, 35, 20, 18]);
        Shape(C, M, AGShade(col, 0.8), 0.7);
        C.Polygon([39, 35, 1, 35, 20, 18]);
        Shape(C, M, AGShade(col, 0.6), 0.7);
        C.Polygon([1, 35, 1, 1, 20, 18]);
        Shape(C, M, AGShade(col, 1.05), 0.7);
        Disc(C, 20, 18, 2.2, AGHex($E8C040));
      End;
    thMine: Begin // rock with a wooden prop
        col := TC(M, $6A5E54);
        Lump(C, R, 20, 18, 18, 16, 0.12, 9);
        Shape(C, M, col, 1);
        Lump(C, R, 15, 13, 8, 6, 0.2, 7);
        C.Fill(AGShade(col, 1.2));
        Line(C, 22, 20, 30, 26, Pen(M, 1), AGShade(col, 0.6));
        Disc(C, 28, 10, 1.5, AGHex($D0B040));
        Disc(C, 10, 24, 1.2, AGHex($D0B040));
      End;
    thBeach: Begin // dark rock
        col := TC(M, $6A645C);
        Lump(C, R, 20, 19, 19, 16, 0.1, 9);
        Shape(C, M, col, 1);
        Lump(C, R, 14, 12, 9, 6, 0.2, 7);
        C.Fill(AGShade(col, 1.25));
        Disc(C, 28, 25, 2, AGHex($E8E0D8));
        Disc(C, 31, 23, 1.4, AGHex($E8E0D8));
      End;
    thAlien: Begin // metal pylon with an energy core
        col := TC(M, $505070);
        Block3D(C, M, 2, 1, 36, 34, 7, 8, col);
        Disc(C, 20, 14, 7, AGShade(col, 0.5));
        Disc(C, 20, 14, 5, TC(M, $F0A040));
        Disc(C, 18.5, 12.5, 2, AGHex($FFF0C0));
        Line(C, 6, 4, 6, 24, Pen(M, 1.2), AGShade(col, 1.4));
      End;
    thHaunted: Begin // tombstone
        col := TC(M, $8A8698);
        C.BeginPath;
        C.MoveTo(5, 35);
        C.LineTo(5, 14);
        C.QuadTo(5, 1, 20, 1);
        C.QuadTo(35, 1, 35, 14);
        C.LineTo(35, 35);
        C.ClosePath;
        Shape(C, M, col, 1);
        Line(C, 20, 7, 20, 22, Pen(M, 2.2), AGShade(col, 0.55));
        Line(C, 14, 12, 26, 12, Pen(M, 2.2), AGShade(col, 0.55));
        Box(C, 5, 30, 30, 5, AGShade(col, 0.7));
        Disc(C, 9, 26, 1.5, AGHex($5A7A50));
        Disc(C, 31, 24, 1.2, AGHex($5A7A50));
      End;
    thWater: Begin // rock with barnacles
        col := TC(M, $44607A);
        Lump(C, R, 20, 19, 19, 16, 0.1, 9);
        Shape(C, M, col, 1);
        For i := 0 To 6 Do Begin
          x := 8 + R.NextUnit * 24;
          C.Circle(x, 8 + R.NextUnit * 20, 2.2);
          Shape(C, M, AGHex($D8D4C8), 0.5);
        End;
      End;
    thForest: Begin // tree stump with rings
        col := TC(M, $7A5432);
        C.RoundRect(3, 8, 34, 27, 8);
        Shape(C, M, AGShade(col, 0.75), 1);
        Oval(C, 20, 14, 17, 11, AGShade(col, 1.35));
        c2 := AGShade(col, 0.9);
        For i := 1 To 3 Do Begin
          C.BeginPath;
          C.Ellipse(20, 14, 4 * i, 2.6 * i);
          C.Stroke(Pen(M, 0.9), c2);
        End;
        For i := 0 To 3 Do
          Line(C, 6 + i * 9, 22, 6 + i * 9, 33, Pen(M, 1), AGShade(col, 0.5));
      End;
    thCity: Begin // oil drum
        col := TC(M, $3A6A9A);
        C.RoundRect(5, 2, 30, 33, 3);
        Shape(C, M, col, 1);
        Oval(C, 20, 5, 14, 3.5, AGShade(col, 1.3));
        Oval(C, 20, 5, 10, 2.2, AGShade(col, 0.6));
        Line(C, 5, 15, 35, 15, Pen(M, 1.6), AGShade(col, 0.6));
        Line(C, 5, 25, 35, 25, Pen(M, 1.6), AGShade(col, 0.6));
        Line(C, 9, 9, 9, 31, Pen(M, 1.5), AGShade(col, 1.4));
        C.Polygon([20, 17, 24, 23, 16, 23]);
        C.Fill(AGHex($F0D040));
      End;
  End;
End;

(* ---------------------------------------------------------------------- *)
(* xbrick.ani: the brick breaks into pieces, fire and smoke               *)
(* ---------------------------------------------------------------------- *)

Function MakeXBrick(Ctx: TAGContext; Const M: TMap; Brick: TAGImage): TAGImage;
Var
  L: TXBrickLayout;
  f, x, y, q, sx, sy, dx, dy, ps: Integer;
  t: Double;
  thr: Array Of Double;
  R: TAGRandom;
  fimg, fire: TAGImage;
  c: TAGColor;

  Procedure DrawFire(Cv: TAGCanvas; Frame: Integer);
  Var
    k: Integer;
    a, d, rr, ft: Double;
    col: TAGColor;
    FRn: TAGRandom;
  Begin
    ft := t;
    FRn := M.A.Stream('fire');
    // smoke puffs, growing and fading, kept inside the cell
    For k := 0 To 5 Do Begin
      a := AG_TWO_PI * k / 6 + FRn.NextUnit;
      d := 3 + 7 * ft;
      rr := (2.5 + 3 * FRn.NextUnit) * (0.6 + 0.7 * ft);
      If ft > 0.7 Then rr := rr * (1 - 1.6 * (ft - 0.7));
      If (ft > 0.25) And (rr > 1) Then Begin
        Cv.Circle(AGClamp(20 + d * AGCos(a), rr + 1, 39 - rr), AGClamp(18 + d * AGSin(a) - 5 * ft, rr + 1, 35 - rr), rr);
        DrawShape(Cv, M.Style, AGShade(AGHex($9A968E), 1.05 + 0.25 * ft), 0.4);
      End;
    End;
    // fire ball, largest in the middle of the animation
    If (ft > 0.05) And (ft < 0.8) Then Begin
      rr := 15 * AGSin(AG_PI * (ft - 0.05) / 0.75);
      For k := 0 To 6 Do Begin
        a := AG_TWO_PI * k / 7 + FRn.NextUnit * 0.5;
        Cv.Circle(20 + rr * 0.45 * AGCos(a), 18 + rr * 0.4 * AGSin(a), rr * (0.45 + 0.2 * FRn.NextUnit));
        col := AGMix(AGHex($E04818), AGHex($F8A028), FRn.NextUnit);
        DrawShape(Cv, M.Style, col, 0.4);
      End;
      Cv.Circle(20, 18, rr * 0.5);
      DrawShape(Cv, M.Style, AGHex($FFE070), 0);
    End;
    // embers
    If ft > 0.5 Then
      For k := 0 To 7 Do Begin
        a := AG_TWO_PI * FRn.NextUnit;
        d := 6 + 14 * ft * FRn.NextUnit;
        Disc(Cv, 20 + d * AGCos(a), 18 + d * AGSin(a), 1.2 * (1.2 - ft) + 0.4, AGHex($F8B040));
      End;
  End;

Begin
  L := XLayouts[M.Index];
  ps := M.PS;
  result := TAGImage.Create(L.Cols * CW, L.Rows * CH);
  // dissolve thresholds per (pixel block)
  R := M.A.Stream('dissolve');
  thr := Nil;
  setlength(thr, 10 * 9); // per 4 x 4 block, the brick crumbles in chunks
  For y := 0 To high(thr) Do
    thr[y] := R.NextUnit;
  For f := 0 To L.Count - 1 Do Begin
    t := f / (L.Count - 1);
    fimg := TAGImage.Create(CW, CH);
    // 4 pieces of the brick move outwards and crumble
    For q := 0 To 3 Do Begin
      sx := 2 * (q Mod 2) - 1;
      sy := 2 * (q Div 2) - 1;
      dx := sx * AGRound(10 * t * t / ps) * ps;
      dy := sy * AGRound(8 * t * t / ps) * ps + AGRound(4 * t / ps) * ps;
      For y := 0 To CH Div 2 - 1 Do
        For x := 0 To CW Div 2 - 1 Do Begin
          c := Brick.GetPixel(x + (q Mod 2) * (CW Div 2), y + (q Div 2) * (CH Div 2));
          If c.a < 128 Then Continue;
          If AGSameColor(c, AGFuchsia) Then Continue;
          If thr[((y + (q Div 2) * (CH Div 2)) Div 4) * 10 + (x + (q Mod 2) * (CW Div 2)) Div 4] < 1.3 * t - 0.15 Then Continue;
          If f > 0 Then c := AGMix(c, AGHex($201810), 0.6 * t);
          If (f > 0) And GetStyleInfo(M.Style).Palette Then c := AGRetroColor(c);
          fimg.SetPixel(x + (q Mod 2) * (CW Div 2) + dx, y + (q Div 2) * (CH Div 2) + dy, c);
        End;
    End;
    If f > 0 Then Begin
      fire := RenderImage(Ctx, CW, CH, @DrawFire);
      fimg.Draw(fire, 0, 0);
      fire.Free;
    End;
    result.Copy(fimg, (f Mod L.Cols) * CW, (f Div L.Cols) * CH);
    fimg.Free;
  End;
End;

(* ---------------------------------------------------------------------- *)
(* Generator                                                              *)
(* ---------------------------------------------------------------------- *)

Function InitMap(Ctx: TAGContext; Index: Integer): TMap;
Begin
  result.Index := Index;
  result.Theme := Themes[Index];
  result.A := Ctx.Area(format('map:Field%.2d', [Index]));
  result.Style := Ctx.Style;
  result.PS := GetStyleInfo(Ctx.Style).PixelScale;
  result.Hue := result.A.Vary('hue', 0, 8);
  result.Light := result.A.Vary('light', 1, 0.06);
End;

Function MapThemeName(Index: Integer): String;
Begin
  WriteStr(result, Themes[Index]);
  result := LowerCase(copy(result, 3, length(result)));
End;

Function RenderMapField(Ctx: TAGContext; Index: Integer): TAGImage;
Var
  M: TMap;

  Procedure DF(C: TAGCanvas; Frame: Integer);
  Begin
    DrawFieldImage(C, M);
  End;

Begin
  M := InitMap(Ctx, Index);
  result := RenderImage(Ctx, 640, 480, @DF);
End;

Function RenderMapBrick(Ctx: TAGContext; Index: Integer): TAGImage;
Var
  M: TMap;

  Procedure DB(C: TAGCanvas; Frame: Integer);
  Begin
    DrawBrick(C, M);
  End;

Begin
  M := InitMap(Ctx, Index);
  result := RenderImage(Ctx, CW, CH, @DB);
  If M.Style = asSoft Then SoftBlockShadow(result, M);
End;

Function RenderMapSolid(Ctx: TAGContext; Index: Integer): TAGImage;
Var
  M: TMap;

  Procedure DS(C: TAGCanvas; Frame: Integer);
  Begin
    DrawSolid(C, M);
  End;

Begin
  M := InitMap(Ctx, Index);
  result := RenderImage(Ctx, CW, CH, @DS);
  If M.Style = asSoft Then SoftBlockShadow(result, M);
End;

Procedure GenerateMap(Ctx: TAGContext; Output: TAGOutput; Index: Integer);
Var
  dir: String;
  img, brick: TAGImage;
  L: TXBrickLayout;
Begin
  dir := format('maps/Field%.2d/', [Index]);
  img := RenderMapField(Ctx, Index);
  Try
    Output.SaveImage(dir + 'field.png', img, smOpaque);
  Finally
    img.Free;
  End;
  brick := RenderMapBrick(Ctx, Index);
  Try
    Output.SaveImage(dir + 'brick.png', brick, smKeyed);
    brick.KeyTransparent;
    img := MakeXBrick(Ctx, InitMap(Ctx, Index), brick);
    Try
      L := XLayouts[Index];
      Output.SaveAni(dir + 'xbrick.ani', img, AniSpec(0, 100, L.Cols, L.Rows, CW, CH, [0], [L.Count]));
    Finally
      img.Free;
    End;
  Finally
    brick.Free;
  End;
  img := RenderMapSolid(Ctx, Index);
  Try
    Output.SaveImage(dir + 'solid.png', img, smKeyed);
  Finally
    img.Free;
  End;
End;

Procedure GenerateMaps(Ctx: TAGContext; Output: TAGOutput);
Var
  i: Integer;
Begin
  For i := 0 To MapCount - 1 Do
    GenerateMap(Ctx, Output, i);
End;

Initialization
  RegisterGenerator('maps', 'Maps (maps/FieldXX: field, brick, solid, xbrick)', 4 * MapCount, @GenerateMaps);

End.
