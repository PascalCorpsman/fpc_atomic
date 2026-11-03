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
 * Style dependent drawing and sprite sheets.
 *
 * Generators describe shapes once, the style decides how they look:
 *   flat  = "vector cartoon": thick dark outline, flat fill with one hard
 *           cel shadow band at the bottom right, no gradient, no highlight;
 *           afterwards the colors are made more saturated (StylePost)
 *   soft  = "painted clay": no dark outline (only a thin, slightly darker
 *           tone of the own color), smooth radial gradient with a specular
 *           spot, a soft contact shadow under every shape; afterwards a
 *           warmer, softer palette, a subtle paint texture and a vignette on
 *           big opaque images (StylePost)
 *   pixel = "16-bit console": rendered at half resolution without anti
 *           aliasing, scaled up (nearest neighbour), 4 shading steps with
 *           ordered (Bayer) dithering between them, selective outline (dark
 *           at the bottom right, a darker tone of the own color at the lit
 *           top left); afterwards every channel is reduced to 10 levels with
 *           ordered dithering (StylePost)
 *   retro = rendered at a third of the resolution without anti aliasing,
 *           hard cel shading (highlight spot top left, base, shadow crescent
 *           bottom right), 1 coarse pixel outline, afterwards every pixel is
 *           snapped to a fixed palette of saturated colors (see AGRetroColor)
 *
 * Example (bomb frame):
 *   sheet := RenderSheet(Ctx, 5, 4, 40, 37, 18, @DrawFrame);
 *   with  Procedure DrawFrame(C: TAGCanvas; Frame: Integer);
 *         Begin C.Circle(20, 20, 12); DrawShape(C, Ctx.Style, AGHex($303040)); End;
 *)
Unit uag_render;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Uses uag_image, uag_canvas, uag_context;

Type

  TAGFrameProc = Procedure(C: TAGCanvas; Frame: Integer) Is Nested;

  TAGStyleInfo = Record
    AntiAlias: Boolean;
    PixelScale: Integer; // 1 = full resolution
    OutlineWidth: Double; // default outline width in pixel of the final image
    Gradient: Boolean;
    CelShade: Boolean; // hard steps instead of a gradient (retro)
    Palette: Boolean; // snap all pixels to the retro palette in RenderSheet
    MinPen: Double; // thinner strokes get gaps (no anti aliasing), in pixel of the final image
    Dither: Boolean; // shading steps with ordered dithering (pixel)
    Ambient: Boolean; // soft contact shadow under every shape (soft)
  End;

Function GetStyleInfo(Style: TAGStyle): TAGStyleInfo;

(* true for the styles that render at a lower resolution (pixel, retro) *)
Function IsCoarse(Style: TAGStyle): Boolean;

(*
 * The retro palette: a few hue ramps of saturated colors. Keeps the player
 * color semantics: a green dominant color (g > r and g > b) becomes a green
 * dominant palette color, every other color a color that is not green
 * dominant. Alpha is kept, fuchsia stays fuchsia.
 *)
Function AGRetroColor(c: TAGColor): TAGColor;
Procedure AGRetroQuantize(Img: TAGImage);

(*
 * Keeps the engine rules after a color change of a pixel from Orig to c:
 * c is green dominant exactly if Orig is, fuchsia stays fuchsia and no new
 * color close to fuchsia appears. Alpha is taken from c.
 *)
Function AGKeepRules(Orig, c: TAGColor): TAGColor;

(*
 * Derived shading color c of Base (lighter / darker tone) on the same side
 * of the player color rule as Base.
 *)
Function AGSameSide(Base, c: TAGColor): TAGColor;

(*
 * The whole image post processing of a style (palette, texture, dithering),
 * RenderSheet calls it for every frame at the resolution of the style
 * (before scaling up). Opaque images of at least 256 x 192 pixel count as
 * backgrounds (soft: vignette).
 *)
Procedure AGStylePost(Img: TAGImage; Style: TAGStyle);

(*
 * Retro title look: vertical gradient from y0 (top) to y1 (bottom) in user
 * space as 4 hard bands, light top, Base, dark bottom.
 *)
Function AGBandPaint(C: TAGCanvas; y0, y1: Double; Base: TAGColor): TAGPaint;

(*
 * Vertical fill of titles / big digits from y0 (top) to y1 (bottom) in user
 * space in the look of the style: flat = base with one hard darker band at
 * the bottom, soft = smooth light to dark gradient, pixel = 4 steps with
 * ordered dithering, retro = AGBandPaint.
 *)
Function AGTitlePaint(C: TAGCanvas; Style: TAGStyle; y0, y1: Double; Base: TAGColor): TAGPaint;

(*
 * Fills the current path of C in the look of Style and draws its outline.
 * OutlineScale scales the default outline width (0 = no outline).
 * The path is cleared afterwards.
 *)
Procedure DrawShape(C: TAGCanvas; Style: TAGStyle; Base: TAGColor; OutlineScale: Double = 1.0);

(*
 * Same as DrawShape, but with an explicit outline color (e.g. black for all
 * parts of the atomic).
 *)
Procedure DrawShapeOutlined(C: TAGCanvas; Style: TAGStyle; Base, Outline: TAGColor; OutlineScale: Double = 1.0);

(*
 * Renders Count frames into a sheet of FramesPerRow x FramesPerCol cells of
 * FrameW x FrameH pixel. Frame i is at column i mod FramesPerRow, row
 * i div FramesPerRow. Draw gets a canvas whose coordinates are the pixels of
 * the cell (0..FrameW, 0..FrameH), independent of the style. Cells without a
 * frame stay transparent.
 *)
Function RenderSheet(Ctx: TAGContext; FramesPerRow, FramesPerCol, FrameW, FrameH, Count: Integer; Draw: TAGFrameProc): TAGImage;

(* A single image, Frame = 0 *)
Function RenderImage(Ctx: TAGContext; W, H: Integer; Draw: TAGFrameProc): TAGImage;

Implementation

Uses uag_math;

Function GetStyleInfo(Style: TAGStyle): TAGStyleInfo;
Begin
  result.CelShade := false;
  result.Palette := false;
  result.MinPen := 0;
  result.Dither := false;
  result.Ambient := false;
  Case Style Of
    asSoft: Begin
        result.AntiAlias := true;
        result.PixelScale := 1;
        result.OutlineWidth := 0.9;
        result.Gradient := true;
        result.Ambient := true;
      End;
    asPixel: Begin
        result.AntiAlias := false;
        result.PixelScale := 2;
        result.OutlineWidth := 3.0; // 1.5 pixel at half resolution, thinner strokes get gaps without anti aliasing
        result.Gradient := false;
        result.MinPen := 2.2;
        result.Dither := true;
      End;
    asRetro: Begin
        result.AntiAlias := false;
        result.PixelScale := 3;
        result.OutlineWidth := 4.2; // 1.4 pixel at a third of the resolution: closed, but only one pixel wide
        result.Gradient := false;
        result.CelShade := true;
        result.Palette := true;
        result.MinPen := 3.3;
      End;
  Else Begin
      result.AntiAlias := true;
      result.PixelScale := 1;
      result.OutlineWidth := 2.5;
      result.Gradient := false;
      result.CelShade := true;
    End;
  End;
End;

Function IsCoarse(Style: TAGStyle): Boolean;
Begin
  result := GetStyleInfo(Style).PixelScale > 1;
End;

Type
  TRetroEntry = Record
    c: TAGColor;
    Green: Boolean;
  End;

Var
  RetroPal: Array Of TRetroEntry;

Procedure InitRetroPalette;
Const
  // ramps from dark to light, $RRGGBB
  Cols: Array[0..69] Of Cardinal = (
    // grey (outline, steel)
    $0C0A14, $2A2A3A, $505064, $808094, $B4B4C4, $E4E4EC, $FFFFFF,
    // blue grey (metal, stone)
    $1C2438, $34446A, $5C70A0, $94A8D0,
    // indigo (night skies, dark backgrounds)
    $181634, $2A2656, $40397C,
    // teal (frames)
    $14404A, $246C74, $4A9CA0,
    // red
    $480810, $8C1018, $D02024, $FF5038, $FF9C84,
    // orange
    $5C2000, $A44000, $E87000, $FFA41C, $FFD474,
    // yellow
    $5C4C00, $A08C00, $E4C800, $FFF040, $FFFCB0,
    // brown / skin
    $3C2010, $6C3C1C, $A06834, $D8A064, $FFD4A8,
    // cyan
    $004458, $007C98, $00B8D4, $48E8FF, $B4F8FF,
    // blue
    $0C1464, $1C34B4, $3060F0, $6C98FF, $B4CCFF,
    // purple
    $340860, $6418A8, $9C30E0, $C474FF,
    // magenta / pink
    $5C0838, $A01870, $E034A0, $FF78C8, $FFC0E4,
    // green (green dominant, r = b like the player green: recolored neutrally)
    $003C00, $046C04, $0EA00E, $1CCE1C, $2EFF2E, $A0FFA0,
    // grey green (green dominant, for stone and moss)
    $4C584C, $7C887C, $B0BCB0,
    // grass (green dominant, yellowish)
    $284C08, $4C8410, $80BC20, $B8E850,
    $000000);
Var
  i: Integer;
Begin
  setlength(RetroPal, length(Cols));
  For i := 0 To high(Cols) Do Begin
    RetroPal[i].c := AGHex(Cols[i]);
    RetroPal[i].Green := IsGreenDominant(RetroPal[i].c);
  End;
End;

Function AGRetroColor(c: TAGColor): TAGColor;
Var
  i, best: Integer;
  d, bd, rm, dr, dg, db: Int64;
  green: Boolean;
Begin
  If (c.a = 0) Or ((c.r = 255) And (c.g = 0) And (c.b = 255)) Then exit(c);
  green := IsGreenDominant(c);
  best := -1;
  bd := high(Int64);
  For i := 0 To high(RetroPal) Do Begin
    If RetroPal[i].Green <> green Then Continue;
    // "red mean" weighted distance, integer only
    rm := (Int64(c.r) + RetroPal[i].c.r) Div 2;
    dr := Int64(c.r) - RetroPal[i].c.r;
    dg := Int64(c.g) - RetroPal[i].c.g;
    db := Int64(c.b) - RetroPal[i].c.b;
    d := ((512 + rm) * dr * dr) Div 256 + 4 * dg * dg + ((767 - rm) * db * db) Div 256;
    If d < bd Then Begin
      bd := d;
      best := i;
    End;
  End;
  If best < 0 Then exit(c);
  result := RetroPal[best].c;
  result.a := c.a;
End;

Procedure AGRetroQuantize(Img: TAGImage);
Var
  i: Integer;
  last, lastRes: TAGColor;
Begin
  last := AGTransparent;
  lastRes := AGTransparent;
  For i := 0 To high(Img.Pixels) Do Begin
    If Img.Pixels[i].a = 0 Then Continue;
    If Not AGSameColor(Img.Pixels[i], last) Then Begin
      last := Img.Pixels[i];
      lastRes := AGRetroColor(last);
    End;
    Img.Pixels[i] := lastRes;
  End;
End;

Function AGBandPaint(C: TAGCanvas; y0, y1: Double; Base: TAGColor): TAGPaint;
Var
  i: Integer;
  cols: Array[0..3] Of TAGColor;
Begin
  cols[0] := AGShade(Base, 1.6);
  cols[1] := AGShade(Base, 1.2);
  cols[2] := Base;
  cols[3] := AGShade(Base, 0.62);
  result := C.Linear(0, y0, 0, y1, [Base, Base]);
  setlength(result.Stops, 8);
  For i := 0 To 3 Do Begin
    result.Stops[2 * i].Pos := i / 4;
    result.Stops[2 * i].Color := cols[i];
    result.Stops[2 * i + 1].Pos := (i + 1) / 4;
    result.Stops[2 * i + 1].Color := cols[i];
  End;
End;

(*
 * Retro: base color, a shadow crescent at the bottom right and a small
 * highlight spot at the top left, as hard steps of one radial paint.
 *)
Function CelPaint(C: TAGCanvas; Base: TAGColor; x0, y0, x1, y1: Double): TAGPaint;
Var
  w, h, r: Double;
  hi, sh: TAGColor;
  m: TAGMatrix;
  n: Integer;

  Procedure Stop(Pos: Double; Col: TAGColor);
  Begin
    result.Stops[n].Pos := Pos;
    result.Stops[n].Color := Col;
    inc(n);
  End;

Begin
  w := x1 - x0;
  h := y1 - y0;
  r := AGMax(w, h);
  hi := AGShade(Base, 1.55);
  sh := AGShade(Base, 0.58);
  If IsGreenDominant(Base) Then Begin
    If Not IsGreenDominant(hi) Then hi := Base;
    If Not IsGreenDominant(sh) Then sh := Base;
  End
  Else Begin
    hi := NotGreen(hi);
    sh := NotGreen(sh);
  End;
  m := C.GetMatrix;
  C.SetMatrix(AGIdentity);
  result := C.Radial(x0 + 0.32 * w, y0 + 0.28 * h, r, [Base, Base]);
  C.SetMatrix(m);
  n := 0;
  setlength(result.Stops, 6);
  If r < 4 Then hi := Base; // no room for a highlight
  Stop(0, hi);
  Stop(0.15, hi);
  Stop(0.15, Base);
  Stop(0.6, Base);
  Stop(0.6, sh);
  Stop(1, sh);
End;

Function AGSameSide(Base, c: TAGColor): TAGColor;
Var
  m: Integer;
Begin
  result := c;
  If IsGreenDominant(Base) Then Begin
    If Not IsGreenDominant(result) Then Begin
      m := result.r;
      If result.b > m Then m := result.b;
      If m < 255 Then result.g := m + 1 Else result := Base;
    End;
  End
  Else
    result := NotGreen(result);
End;

Function NearFuchsia(c: TAGColor): Boolean;
Begin
  result := (c.r > 180) And (c.b > 180) And (c.g < 90);
End;

Function AGKeepRules(Orig, c: TAGColor): TAGColor;
Begin
  If (Orig.r = 255) And (Orig.g = 0) And (Orig.b = 255) Then exit(Orig);
  result := AGSameSide(Orig, c);
  If NearFuchsia(result) And Not NearFuchsia(Orig) Then Begin
    result := Orig;
    result.a := c.a;
  End;
End;

(* Hard shading stop color, on the same player color side as Base *)
Function Tone(Base: TAGColor; Factor: Double): TAGColor;
Begin
  result := AGSameSide(Base, AGShade(Base, Factor));
End;

(* the light comes from the top left: center of the shading and the distance to the far corner *)
Procedure LightCenter(x0, y0, x1, y1, fx, fy: Double; Out cx, cy, r: Double);
Begin
  cx := x0 + fx * (x1 - x0);
  cy := y0 + fy * (y1 - y0);
  r := AGSqrt(sqr(x1 - cx) + sqr(y1 - cy));
  If r < 0.5 Then r := 0.5;
End;

(* paint in device space (identity matrix while it is built) *)
Function DeviceRadial(C: TAGCanvas; cx, cy, r: Double): TAGPaint;
Var
  m: TAGMatrix;
Begin
  m := C.GetMatrix;
  C.SetMatrix(AGIdentity);
  result := C.Radial(cx, cy, r, [AGBlack, AGBlack]);
  C.SetMatrix(m);
End;

Procedure SetStops(Var p: TAGPaint; Const Pos: Array Of Double; Const Cols: Array Of TAGColor);
Var
  i: Integer;
Begin
  setlength(p.Stops, length(Pos));
  For i := 0 To high(Pos) Do Begin
    p.Stops[i].Pos := Pos[i];
    p.Stops[i].Color := Cols[i];
  End;
  p.Color := Cols[0];
End;

Function AGTitlePaint(C: TAGCanvas; Style: TAGStyle; y0, y1: Double; Base: TAGColor): TAGPaint;
Begin
  result := C.Linear(0, y0, 0, y1, [Base, Base]);
  Case Style Of
    asFlat: SetStops(result, [0, 0.58, 0.58, 1], [Base, Base, Tone(Base, 0.76), Tone(Base, 0.76)]);
    asSoft: SetStops(result, [0, 0.3, 0.65, 1], [Tone(Base, 1.6), Tone(Base, 1.2), Base, Tone(Base, 0.78)]);
    asPixel: Begin
        SetStops(result, [0.05, 0.35, 0.65, 0.95], [Tone(Base, 1.5), Tone(Base, 1.18), Base, Tone(Base, 0.68)]);
        result.DitherWidth := 0.5;
      End;
  Else
    result := AGBandPaint(C, y0, y1, Base);
  End;
End;

(* Flat: base color and one hard shadow crescent at the bottom right *)
Function FlatPaint(C: TAGCanvas; Base: TAGColor; x0, y0, x1, y1: Double): TAGPaint;
Var
  cx, cy, r: Double;
  sh: TAGColor;
Begin
  LightCenter(x0, y0, x1, y1, 0.36, 0.3, cx, cy, r);
  result := DeviceRadial(C, cx, cy, r);
  sh := Tone(Base, 0.7);
  SetStops(result, [0, 0.66, 0.66, 1], [Base, Base, sh, sh]);
End;

(* Soft: smooth radial gradient with a specular spot *)
Function SoftPaint(C: TAGCanvas; Base: TAGColor; x0, y0, x1, y1: Double): TAGPaint;
Var
  cx, cy, r: Double;
  spec: TAGColor;
Begin
  LightCenter(x0, y0, x1, y1, 0.34, 0.27, cx, cy, r);
  result := DeviceRadial(C, cx, cy, r);
  If AGMax(x1 - x0, y1 - y0) >= 7 Then
    spec := Tone(Base, 1.75)
  Else
    spec := Tone(Base, 1.35);
  SetStops(result, [0, 0.07, 0.2, 0.5, 0.82, 1],
    [spec, Tone(Base, 1.42), Tone(Base, 1.16), Base, Tone(Base, 0.7), Tone(Base, 0.52)]);
End;

(* Pixel: 4 shading steps with ordered dithering between them *)
Function PixelPaint(C: TAGCanvas; Base: TAGColor; x0, y0, x1, y1: Double): TAGPaint;
Var
  cx, cy, r: Double;
Begin
  LightCenter(x0, y0, x1, y1, 0.34, 0.28, cx, cy, r);
  result := DeviceRadial(C, cx, cy, r);
  SetStops(result, [0, 0.36, 0.66, 0.9], [Tone(Base, 1.38), Base, Tone(Base, 0.74), Tone(Base, 0.52)]);
  result.DitherWidth := 0.35;
End;

(*
 * Soft: contact shadow below and right of the shape, a few translucent
 * layers (alpha stays below the transparency key threshold, so it only shows
 * on top of other shapes or opaque backgrounds)
 *)
Procedure AmbientShadow(C: TAGCanvas; sz: Double);
Var
  dx, dy, r: Double;
  col: TAGColor;
Begin
  dx := AGClamp(0.05 * sz, 0.6, 2.5);
  dy := AGClamp(0.09 * sz, 1, 4);
  col := AGHex($2A1608);
  // a cheap blur: a wide light penumbra and the core shadow
  r := AGClamp(0.08 * sz, 0.8, 2.5);
  C.OffsetPath(dx + r, dy + r);
  C.Fill(AGWithAlpha(col, 16), true);
  C.OffsetPath(-r, -r);
  C.Fill(AGWithAlpha(col, 30), true);
  C.OffsetPath(-dx, -dy);
End;

Procedure DrawShapeOutlined(C: TAGCanvas; Style: TAGStyle; Base, Outline: TAGColor; OutlineScale: Double);
Var
  info: TAGStyleInfo;
  x0, y0, x1, y1, sz, s, pw: Double;
  keep: Boolean;
  paint: TAGPaint;
  m: TAGMatrix;
  o: TAGColor;
Begin
  info := GetStyleInfo(Style);
  If Not C.PathBounds(x0, y0, x1, y1) Then exit;
  sz := AGMax(x1 - x0, y1 - y0); // device pixel (= coarse pixel of pixel / retro)
  keep := OutlineScale > 0;
  // scale of the current transformation (outline widths are in pixel of the final image)
  m := C.GetMatrix;
  s := (AGSqrt(sqr(m.a) + sqr(m.b)) + AGSqrt(sqr(m.c) + sqr(m.d))) / 2;
  If s <= 0 Then s := 1;
  Case Style Of
    asFlat: Begin
        If sz >= 6 Then
          C.FillPaint(FlatPaint(C, Base, x0, y0, x1, y1), keep)
        Else
          C.Fill(Base, keep);
      End;
    asSoft: Begin
        If info.Ambient And keep And (Base.a = 255) And (sz >= 6) Then AmbientShadow(C, sz);
        C.FillPaint(SoftPaint(C, Base, x0, y0, x1, y1), keep);
      End;
    asPixel: Begin
        If sz >= 3 Then
          C.FillPaint(PixelPaint(C, Base, x0, y0, x1, y1), keep)
        Else
          C.Fill(Base, keep);
      End;
  Else Begin
      If info.CelShade And (sz >= 2.5) Then
        C.FillPaint(CelPaint(C, Base, x0, y0, x1, y1), keep)
      Else
        C.Fill(Base, keep);
    End;
  End;
  If Not keep Then exit;
  pw := info.OutlineWidth * OutlineScale / info.PixelScale / s;
  Case Style Of
    asSoft: Begin
        // no ink line, only a slightly darker tone of the own color
        o := AGSameSide(Base, AGMix(AGShade(Base, 0.62), Outline, 0.12));
        o.a := Base.a;
        C.Stroke(pw, o);
      End;
    asPixel: Begin
        If sz >= 4 Then Begin
          // selective outline: a darker tone of the own color on the lit top left, dark at the bottom right
          o := Tone(Base, 0.55);
          m := C.GetMatrix;
          C.SetMatrix(AGIdentity);
          paint := C.Linear(x0, y0, x1, y1, [o, Outline]);
          C.SetMatrix(m);
          paint.Stops[0].Pos := 0.3;
          paint.Stops[1].Pos := 0.6;
          paint.DitherWidth := 0.6;
          C.StrokePaint(pw, paint);
        End
        Else
          C.Stroke(pw, Outline);
      End;
  Else
    C.Stroke(pw, Outline);
  End;
End;

Procedure DrawShape(C: TAGCanvas; Style: TAGStyle; Base: TAGColor; OutlineScale: Double);
Var
  o: TAGColor;
Begin
  Case Style Of
    asSoft: o := AGShade(Base, 0.55);
    asFlat: o := AGMix(AGShade(Base, 0.22), AGHex($100C18), 0.5);
  Else
    o := AGShade(Base, 0.3);
  End;
  DrawShapeOutlined(C, Style, Base, AGSameSide(Base, o), OutlineScale);
End;

(* ---------------------------------------------------------------------- *)
(* Post processing of whole images                                        *)
(* ---------------------------------------------------------------------- *)

(* Flat: more saturated colors (channels pushed away from their mean) *)
Procedure PostFlat(Img: TAGImage);
Var
  i: Integer;
  c, o, last, lastRes: TAGColor;
  mean: Double;
Begin
  last := AGTransparent;
  lastRes := AGTransparent;
  For i := 0 To high(Img.Pixels) Do Begin
    o := Img.Pixels[i];
    If o.a = 0 Then Continue;
    If AGSameColor(o, last) Then Begin
      Img.Pixels[i] := lastRes;
      Continue;
    End;
    mean := (o.r + o.g + o.b) / 3;
    c := o;
    c.r := AGClampI(AGRound(mean + 1.3 * (o.r - mean)), 0, 255);
    c.g := AGClampI(AGRound(mean + 1.3 * (o.g - mean)), 0, 255);
    c.b := AGClampI(AGRound(mean + 1.3 * (o.b - mean)), 0, 255);
    c := AGKeepRules(o, c);
    last := o;
    lastRes := c;
    Img.Pixels[i] := c;
  End;
End;

(* Soft: warmer, a little less saturated, paint texture, vignette on backgrounds *)
Procedure PostSoft(Img: TAGImage);
Var
  x, y, i, d: Integer;
  c, o: TAGColor;
  mean, n, v, dx, dy, cx, cy, f: Double;
  opaque: Boolean;
Begin
  opaque := (Img.Width >= 256) And (Img.Height >= 192);
  If opaque Then
    For i := 0 To high(Img.Pixels) Do
      If Img.Pixels[i].a < 255 Then Begin
        opaque := false;
        break;
      End;
  cx := Img.Width / 2;
  cy := Img.Height / 2;
  For y := 0 To Img.Height - 1 Do
    For x := 0 To Img.Width - 1 Do Begin
      i := y * Img.Width + x;
      o := Img.Pixels[i];
      If o.a = 0 Then Continue;
      c := o;
      If Not IsGreenDominant(o) Then Begin
        mean := (o.r + o.g + o.b) / 3;
        c.r := AGClampI(AGRound(mean + 0.9 * (o.r - mean) + 6), 0, 255);
        c.g := AGClampI(AGRound(mean + 0.9 * (o.g - mean) + 2), 0, 255);
        c.b := AGClampI(AGRound(mean + 0.9 * (o.b - mean) - 5), 0, 255);
      End;
      // paint texture: two octaves of value noise, the same offset on all channels
      n := 4.5 * AGValueNoise(x, y, 3, 17) + 3.5 * AGValueNoise(x, y, 11, 5);
      f := 1;
      If opaque Then Begin
        dx := (x - cx) / cx;
        dy := (y - cy) / cy;
        v := AGClamp((AGSqrt(dx * dx + dy * dy) - 0.6) / 0.85, 0, 1);
        f := 1 - 0.24 * AGSmoothStep(v);
      End;
      d := AGRound(n);
      c.r := AGClampI(AGRound((c.r + d) * f), 0, 255);
      c.g := AGClampI(AGRound((c.g + d) * f), 0, 255);
      c.b := AGClampI(AGRound((c.b + d) * f), 0, 255);
      Img.Pixels[i] := AGKeepRules(o, c);
    End;
End;

(* Pixel: 10 levels per channel, ordered dithering in the middle between two levels *)
Procedure PostPixel(Img: TAGImage);
Const
  Step = 255 / 9;
  Width = 0.2;
Var
  x, y, i: Integer;
  c, o: TAGColor;
  th: Double;

  Function Q(v: Integer): Integer;
  Var
    qq, f: Double;
    k: Integer;
  Begin
    qq := v / Step;
    k := AGFloor(qq);
    f := AGClamp((qq - k - 0.5) / Width + 0.5, 0, 1);
    If f > th Then inc(k);
    result := AGClampI(AGRound(k * Step), 0, 255);
  End;

Begin
  For y := 0 To Img.Height - 1 Do
    For x := 0 To Img.Width - 1 Do Begin
      i := y * Img.Width + x;
      o := Img.Pixels[i];
      If o.a = 0 Then Continue;
      th := AGBayer4(x, y);
      c := o;
      c.r := Q(o.r);
      c.g := Q(o.g);
      c.b := Q(o.b);
      Img.Pixels[i] := AGKeepRules(o, c);
    End;
End;

Procedure AGStylePost(Img: TAGImage; Style: TAGStyle);
Begin
  Case Style Of
    asFlat: PostFlat(Img);
    asSoft: PostSoft(Img);
    asPixel: PostPixel(Img);
    asRetro: AGRetroQuantize(Img);
  End;
End;

Function RenderSheet(Ctx: TAGContext; FramesPerRow, FramesPerCol, FrameW, FrameH, Count: Integer; Draw: TAGFrameProc): TAGImage;
Var
  info: TAGStyleInfo;
  f, sw, sh: Integer;
  small, big, cell: TAGImage;
  c: TAGCanvas;
Begin
  info := GetStyleInfo(Ctx.Style);
  result := TAGImage.Create(FramesPerRow * FrameW, FramesPerCol * FrameH);
  If Count > FramesPerRow * FramesPerCol Then Count := FramesPerRow * FramesPerCol;
  sw := (FrameW + info.PixelScale - 1) Div info.PixelScale;
  sh := (FrameH + info.PixelScale - 1) Div info.PixelScale;
  For f := 0 To Count - 1 Do Begin
    small := TAGImage.Create(sw, sh);
    c := TAGCanvas.Create(small);
    Try
      c.AntiAlias := info.AntiAlias;
      If info.PixelScale <> 1 Then c.Scale(1 / info.PixelScale, 1 / info.PixelScale);
      Draw(c, f);
    Finally
      c.Free;
    End;
    AGStylePost(small, Ctx.Style);
    If info.PixelScale <> 1 Then Begin
      big := small.ScaleNearest(info.PixelScale);
      small.Free;
      cell := big.Crop(0, 0, FrameW, FrameH);
      big.Free;
    End
    Else
      cell := small;
    result.Copy(cell, (f Mod FramesPerRow) * FrameW, (f Div FramesPerRow) * FrameH);
    cell.Free;
  End;
End;

Function RenderImage(Ctx: TAGContext; W, H: Integer; Draw: TAGFrameProc): TAGImage;
Begin
  result := RenderSheet(Ctx, 1, 1, W, H, 1, Draw);
End;

Initialization
  InitRetroPalette;

End.
