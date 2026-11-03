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
  End;

Function GetStyleInfo(Style: TAGStyle): TAGStyleInfo;

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
      End;
  Else Begin
      result.AntiAlias := true;
      result.PixelScale := 1;
      result.OutlineWidth := 1.6;
      result.Gradient := false;
    End;
  End;
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
  If info.Gradient Then Begin
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

End.
