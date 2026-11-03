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
 *   flat  = solid fill with a dark outline
 *   soft  = radial gradient (highlight top left, shadow bottom right), thin outline
 *   pixel = rendered at half resolution without anti aliasing, scaled up
 *           (nearest neighbour), solid fill, 1 px outline
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
 * Retro title look: vertical gradient from y0 (top) to y1 (bottom) in user
 * space as 4 hard bands, light top, Base, dark bottom.
 *)
Function AGBandPaint(C: TAGCanvas; y0, y1: Double; Base: TAGColor): TAGPaint;

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
  Case Style Of
    asSoft: Begin
        result.AntiAlias := true;
        result.PixelScale := 1;
        result.OutlineWidth := 1.0;
        result.Gradient := true;
      End;
    asPixel: Begin
        result.AntiAlias := false;
        result.PixelScale := 2;
        result.OutlineWidth := 3.0; // 1.5 pixel at half resolution, thinner strokes get gaps without anti aliasing
        result.Gradient := false;
        result.MinPen := 2.2;
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
      result.OutlineWidth := 1.6;
      result.Gradient := false;
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

Procedure DrawShapeOutlined(C: TAGCanvas; Style: TAGStyle; Base, Outline: TAGColor; OutlineScale: Double);
Var
  info: TAGStyleInfo;
  x0, y0, x1, y1, w, h, s: Double;
  paint: TAGPaint;
  m: TAGMatrix;
Begin
  info := GetStyleInfo(Style);
  If Not C.PathBounds(x0, y0, x1, y1) Then exit;
  If info.CelShade And (AGMax(x1 - x0, y1 - y0) >= 2.5) Then
    C.FillPaint(CelPaint(C, Base, x0, y0, x1, y1), OutlineScale > 0)
  Else If info.Gradient Then Begin
    w := x1 - x0;
    h := y1 - y0;
    // the paint is defined in device space, so build it with the identity matrix
    m := C.GetMatrix;
    C.SetMatrix(AGIdentity);
    paint := C.Radial(x0 + 0.35 * w, y0 + 0.3 * h, 0.9 * AGMax(w, h),
      [AGShade(Base, 1.5), Base, AGShade(Base, 0.55)]);
    C.SetMatrix(m);
    C.FillPaint(paint, OutlineScale > 0);
  End
  Else
    C.Fill(Base, OutlineScale > 0);
  If OutlineScale > 0 Then Begin
    // width in pixel of the final image, independent of the current transformation
    s := (AGSqrt(sqr(C.GetMatrix.a) + sqr(C.GetMatrix.b)) + AGSqrt(sqr(C.GetMatrix.c) + sqr(C.GetMatrix.d))) / 2;
    If s <= 0 Then s := 1;
    C.Stroke(info.OutlineWidth * OutlineScale / info.PixelScale / s, Outline);
  End;
End;

Procedure DrawShape(C: TAGCanvas; Style: TAGStyle; Base: TAGColor; OutlineScale: Double);
Var
  o: TAGColor;
Begin
  If GetStyleInfo(Style).Gradient Then
    o := AGShade(Base, 0.45)
  Else
    o := AGShade(Base, 0.3);
  DrawShapeOutlined(C, Style, Base, o, OutlineScale);
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
    If info.Palette Then AGRetroQuantize(small);
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
