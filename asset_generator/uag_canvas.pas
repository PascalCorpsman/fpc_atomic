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
 * Vector canvas of the asset generator: paths, transformations, fills with
 * solid colors and gradients, strokes.
 *
 * The scanline rasterizer works in fixed point (24.8) with 4 sub scanlines per
 * pixel, so the result does not depend on the platform. Coordinates are in
 * pixels, (0, 0) is the top left corner of the top left pixel, y points down.
 *
 * Usage:
 *   c := TAGCanvas.Create(img);
 *   c.Translate(55, 60); c.Rotate(AGDeg(10));
 *   c.Ellipse(0, 0, 20, 14);           // adds a sub path
 *   c.Fill(AGRGB(200, 40, 40));        // fills and clears the path
 *   c.Circle(0, 0, 5); c.Stroke(2, AGBlack);
 *)
Unit uag_canvas;

{$MODE ObjFPC}{$H+}

Interface

Uses uag_image;

Type

  TAGPoint = Record
    x, y: Double;
  End;

  TAGPoints = Array Of TAGPoint;

  TAGMatrix = Record
    a, b, c, d, e, f: Double; // x' = a*x + c*y + e, y' = b*x + d*y + f
  End;

  TAGFillRule = (frNonZero, frEvenOdd);

  TAGPaintKind = (pkSolid, pkLinear, pkRadial);

  TAGGradientStop = Record
    Pos: Double; // 0..1
    Color: TAGColor;
  End;

  TAGGradientStops = Array Of TAGGradientStop;

  { TAGPaint: color source for Fill, gradient coordinates are in user space at the time of creation }

  TAGPaint = Record
    Kind: TAGPaintKind;
    Color: TAGColor;
    Stops: TAGGradientStops;
    // linear: from p0 to p1, radial: center p0, radius r (in device space, set by the canvas)
    p0, p1: TAGPoint;
    r: Double;
  End;

  { TAGCanvas }

  TAGCanvas = Class
  private
    fImage: TAGImage;
    fMatrix: TAGMatrix;
    fStack: Array Of TAGMatrix;
    fSubPaths: Array Of TAGPoints; // in device space
    fSubClosed: Array Of Boolean; // per sub path, false = open polyline (matters for Stroke only)
    fCurrent: TAGPoints;
    fClipX0, fClipY0, fClipX1, fClipY1: Integer;
    Procedure FlushCurrent(Close: Boolean);
    Function TransformPoint(x, y: Double): TAGPoint;
    Function PaintColorAt(Const Paint: TAGPaint; x, y: Double): TAGColor;
    Procedure RasterizePaths(Const Paths: Array Of TAGPoints; Const Paint: TAGPaint; Rule: TAGFillRule);
    Function DeviceScale: Double; // average scale of the current matrix (for curve flattening and stroke width)
  public
    AntiAlias: Boolean; // default true, false = hard pixels (pixel art)
    FillRule: TAGFillRule; // default frNonZero
    Property Image: TAGImage read fImage;

    Constructor Create(aImage: TAGImage);

    // Transformation (applied to all following path commands)
    Procedure Push;
    Procedure Pop;
    Procedure ResetTransform;
    Procedure Translate(dx, dy: Double);
    Procedure Scale(sx, sy: Double);
    Procedure Rotate(Radian: Double);
    Procedure Transform(Const m: TAGMatrix); // m is applied before the current matrix
    Function GetMatrix: TAGMatrix;
    Procedure SetMatrix(Const m: TAGMatrix);

    // Clipping in device pixels (inclusive x0,y0 .. exclusive x1,y1)
    Procedure SetClip(x0, y0, x1, y1: Integer);
    Procedure ResetClip;

    // Path building
    Procedure BeginPath; // discards the current path
    Procedure MoveTo(x, y: Double);
    Procedure LineTo(x, y: Double);
    Procedure QuadTo(cx, cy, x, y: Double);
    Procedure CubicTo(c1x, c1y, c2x, c2y, x, y: Double);
    Procedure ArcTo(cx, cy, rx, ry, StartRad, EndRad: Double); // adds a line to the start point if a sub path is open
    Procedure ClosePath;

    // Shapes (each one is a closed sub path)
    Procedure Rect(x, y, w, h: Double);
    Procedure RoundRect(x, y, w, h, Radius: Double);
    Procedure Ellipse(cx, cy, rx, ry: Double);
    Procedure Circle(cx, cy, r: Double);
    Procedure Polygon(Const Pts: Array Of Double); // x0, y0, x1, y1, ...
    Procedure Capsule(x0, y0, x1, y1, Radius: Double); // line with round ends, e.g. arms and legs

    // Paints (coordinates in user space)
    Function Solid(c: TAGColor): TAGPaint;
    Function Linear(x0, y0, x1, y1: Double; Const Colors: Array Of TAGColor): TAGPaint; // evenly spaced stops
    Function Radial(cx, cy, r: Double; Const Colors: Array Of TAGColor): TAGPaint; // evenly spaced stops

    // Drawing, all of them clear the path afterwards unless Keep = true
    Procedure Fill(c: TAGColor; Keep: Boolean = false);
    Procedure FillPaint(Const Paint: TAGPaint; Keep: Boolean = false);
    Procedure Stroke(Width: Double; c: TAGColor; Keep: Boolean = false); // round joins and caps, width in user space
    Function PathBounds(Out x0, y0, x1, y1: Double): Boolean; // device space, false if empty
  End;

Function AGPoint(x, y: Double): TAGPoint;
Function AGIdentity: TAGMatrix;
Function AGMultiply(Const m1, m2: TAGMatrix): TAGMatrix; // first m1, then m2

Implementation

Uses uag_math;

Const
  SubSamples = 4; // sub scanlines per pixel when anti aliasing
  FixShift = 8;
  FixOne = 1 Shl FixShift;

Function AGPoint(x, y: Double): TAGPoint;
Begin
  result.x := x;
  result.y := y;
End;

Function AGIdentity: TAGMatrix;
Begin
  result.a := 1;
  result.b := 0;
  result.c := 0;
  result.d := 1;
  result.e := 0;
  result.f := 0;
End;

Function AGMultiply(Const m1, m2: TAGMatrix): TAGMatrix;
Begin
  result.a := m1.a * m2.a + m1.b * m2.c;
  result.b := m1.a * m2.b + m1.b * m2.d;
  result.c := m1.c * m2.a + m1.d * m2.c;
  result.d := m1.c * m2.b + m1.d * m2.d;
  result.e := m1.e * m2.a + m1.f * m2.c + m2.e;
  result.f := m1.e * m2.b + m1.f * m2.d + m2.f;
End;

Function SignedArea(Const p: TAGPoints): Double;
Var
  i, j: Integer;
Begin
  result := 0;
  For i := 0 To high(p) Do Begin
    j := (i + 1) Mod length(p);
    result := result + p[i].x * p[j].y - p[j].x * p[i].y;
  End;
End;

Procedure Reverse(Var p: TAGPoints);
Var
  i: Integer;
  t: TAGPoint;
Begin
  For i := 0 To length(p) Div 2 - 1 Do Begin
    t := p[i];
    p[i] := p[high(p) - i];
    p[high(p) - i] := t;
  End;
End;

{ TAGCanvas }

Constructor TAGCanvas.Create(aImage: TAGImage);
Begin
  Inherited Create;
  fImage := aImage;
  fMatrix := AGIdentity;
  AntiAlias := true;
  FillRule := frNonZero;
  ResetClip;
End;

Procedure TAGCanvas.Push;
Begin
  setlength(fStack, length(fStack) + 1);
  fStack[high(fStack)] := fMatrix;
End;

Procedure TAGCanvas.Pop;
Begin
  If length(fStack) = 0 Then exit;
  fMatrix := fStack[high(fStack)];
  setlength(fStack, high(fStack));
End;

Procedure TAGCanvas.ResetTransform;
Begin
  fMatrix := AGIdentity;
End;

Procedure TAGCanvas.Translate(dx, dy: Double);
Var
  m: TAGMatrix;
Begin
  m := AGIdentity;
  m.e := dx;
  m.f := dy;
  Transform(m);
End;

Procedure TAGCanvas.Scale(sx, sy: Double);
Var
  m: TAGMatrix;
Begin
  m := AGIdentity;
  m.a := sx;
  m.d := sy;
  Transform(m);
End;

Procedure TAGCanvas.Rotate(Radian: Double);
Var
  m: TAGMatrix;
  s, c: Double;
Begin
  s := AGSin(Radian);
  c := AGCos(Radian);
  m.a := c;
  m.b := s;
  m.c := -s;
  m.d := c;
  m.e := 0;
  m.f := 0;
  Transform(m);
End;

Procedure TAGCanvas.Transform(Const m: TAGMatrix);
Begin
  fMatrix := AGMultiply(m, fMatrix);
End;

Function TAGCanvas.GetMatrix: TAGMatrix;
Begin
  result := fMatrix;
End;

Procedure TAGCanvas.SetMatrix(Const m: TAGMatrix);
Begin
  fMatrix := m;
End;

Procedure TAGCanvas.SetClip(x0, y0, x1, y1: Integer);
Begin
  fClipX0 := AGClampI(x0, 0, fImage.Width);
  fClipY0 := AGClampI(y0, 0, fImage.Height);
  fClipX1 := AGClampI(x1, 0, fImage.Width);
  fClipY1 := AGClampI(y1, 0, fImage.Height);
End;

Procedure TAGCanvas.ResetClip;
Begin
  fClipX0 := 0;
  fClipY0 := 0;
  fClipX1 := fImage.Width;
  fClipY1 := fImage.Height;
End;

Function TAGCanvas.TransformPoint(x, y: Double): TAGPoint;
Begin
  result.x := fMatrix.a * x + fMatrix.c * y + fMatrix.e;
  result.y := fMatrix.b * x + fMatrix.d * y + fMatrix.f;
End;

Function TAGCanvas.DeviceScale: Double;
Begin
  result := (AGSqrt(fMatrix.a * fMatrix.a + fMatrix.b * fMatrix.b) + AGSqrt(fMatrix.c * fMatrix.c + fMatrix.d * fMatrix.d)) / 2;
End;

Procedure TAGCanvas.FlushCurrent(Close: Boolean);
Begin
  If length(fCurrent) >= 2 Then Begin
    setlength(fSubPaths, length(fSubPaths) + 1);
    fSubPaths[high(fSubPaths)] := fCurrent;
    setlength(fSubClosed, length(fSubPaths));
    fSubClosed[high(fSubClosed)] := Close;
  End;
  fCurrent := Nil;
End;

Procedure TAGCanvas.BeginPath;
Begin
  fSubPaths := Nil;
  fSubClosed := Nil;
  fCurrent := Nil;
End;

Procedure TAGCanvas.MoveTo(x, y: Double);
Begin
  FlushCurrent(false);
  setlength(fCurrent, 1);
  fCurrent[0] := TransformPoint(x, y);
End;

Procedure TAGCanvas.LineTo(x, y: Double);
Begin
  If length(fCurrent) = 0 Then Begin
    MoveTo(x, y);
    exit;
  End;
  setlength(fCurrent, length(fCurrent) + 1);
  fCurrent[high(fCurrent)] := TransformPoint(x, y);
End;

Procedure TAGCanvas.QuadTo(cx, cy, x, y: Double);
Var
  p0, p1, p2, q: TAGPoint;
  n, i: Integer;
  t, mt: Double;
Begin
  If length(fCurrent) = 0 Then MoveTo(cx, cy);
  p0 := fCurrent[high(fCurrent)];
  p1 := TransformPoint(cx, cy);
  p2 := TransformPoint(x, y);
  // Number of segments from the length of the control polygon, about 2 px per segment
  n := AGClampI(AGCeil((AGSqrt(sqr(p1.x - p0.x) + sqr(p1.y - p0.y)) + AGSqrt(sqr(p2.x - p1.x) + sqr(p2.y - p1.y))) / 2), 2, 64);
  For i := 1 To n Do Begin
    t := i / n;
    mt := 1 - t;
    q.x := mt * mt * p0.x + 2 * mt * t * p1.x + t * t * p2.x;
    q.y := mt * mt * p0.y + 2 * mt * t * p1.y + t * t * p2.y;
    setlength(fCurrent, length(fCurrent) + 1);
    fCurrent[high(fCurrent)] := q;
  End;
End;

Procedure TAGCanvas.CubicTo(c1x, c1y, c2x, c2y, x, y: Double);
Var
  p0, p1, p2, p3, q: TAGPoint;
  n, i: Integer;
  t, mt: Double;
Begin
  If length(fCurrent) = 0 Then MoveTo(c1x, c1y);
  p0 := fCurrent[high(fCurrent)];
  p1 := TransformPoint(c1x, c1y);
  p2 := TransformPoint(c2x, c2y);
  p3 := TransformPoint(x, y);
  n := AGClampI(AGCeil((AGSqrt(sqr(p1.x - p0.x) + sqr(p1.y - p0.y)) + AGSqrt(sqr(p2.x - p1.x) + sqr(p2.y - p1.y))
    + AGSqrt(sqr(p3.x - p2.x) + sqr(p3.y - p2.y))) / 2), 2, 96);
  For i := 1 To n Do Begin
    t := i / n;
    mt := 1 - t;
    q.x := mt * mt * mt * p0.x + 3 * mt * mt * t * p1.x + 3 * mt * t * t * p2.x + t * t * t * p3.x;
    q.y := mt * mt * mt * p0.y + 3 * mt * mt * t * p1.y + 3 * mt * t * t * p2.y + t * t * t * p3.y;
    setlength(fCurrent, length(fCurrent) + 1);
    fCurrent[high(fCurrent)] := q;
  End;
End;

Procedure TAGCanvas.ArcTo(cx, cy, rx, ry, StartRad, EndRad: Double);
Var
  n, i: Integer;
  a, len: Double;
Begin
  len := AGAbs(EndRad - StartRad) * AGMax(rx, ry) * DeviceScale;
  n := AGClampI(AGCeil(len / 2), 2, 128);
  For i := 0 To n Do Begin
    a := StartRad + (EndRad - StartRad) * i / n;
    LineTo(cx + rx * AGCos(a), cy + ry * AGSin(a));
  End;
End;

Procedure TAGCanvas.ClosePath;
Begin
  FlushCurrent(true);
End;

Procedure TAGCanvas.Rect(x, y, w, h: Double);
Begin
  MoveTo(x, y);
  LineTo(x + w, y);
  LineTo(x + w, y + h);
  LineTo(x, y + h);
  ClosePath;
End;

Procedure TAGCanvas.RoundRect(x, y, w, h, Radius: Double);
Begin
  Radius := AGMin(Radius, AGMin(w, h) / 2);
  If Radius <= 0 Then Begin
    Rect(x, y, w, h);
    exit;
  End;
  FlushCurrent(false);
  ArcTo(x + w - Radius, y + Radius, Radius, Radius, -AG_HALF_PI, 0);
  ArcTo(x + w - Radius, y + h - Radius, Radius, Radius, 0, AG_HALF_PI);
  ArcTo(x + Radius, y + h - Radius, Radius, Radius, AG_HALF_PI, AG_PI);
  ArcTo(x + Radius, y + Radius, Radius, Radius, AG_PI, AG_PI + AG_HALF_PI);
  ClosePath;
End;

Procedure TAGCanvas.Ellipse(cx, cy, rx, ry: Double);
Begin
  FlushCurrent(false);
  ArcTo(cx, cy, rx, ry, 0, AG_TWO_PI);
  ClosePath;
End;

Procedure TAGCanvas.Circle(cx, cy, r: Double);
Begin
  Ellipse(cx, cy, r, r);
End;

Procedure TAGCanvas.Polygon(Const Pts: Array Of Double);
Var
  i: Integer;
Begin
  If length(Pts) < 4 Then exit;
  MoveTo(Pts[0], Pts[1]);
  i := 2;
  While i + 1 <= high(Pts) Do Begin
    LineTo(Pts[i], Pts[i + 1]);
    inc(i, 2);
  End;
  ClosePath;
End;

Procedure TAGCanvas.Capsule(x0, y0, x1, y1, Radius: Double);
Var
  a: Double;
Begin
  a := AGAtan2(y1 - y0, x1 - x0);
  FlushCurrent(false);
  ArcTo(x1, y1, Radius, Radius, a - AG_HALF_PI, a + AG_HALF_PI);
  ArcTo(x0, y0, Radius, Radius, a + AG_HALF_PI, a + AG_HALF_PI + AG_PI);
  ClosePath;
End;

Function TAGCanvas.Solid(c: TAGColor): TAGPaint;
Begin
  result := Default(TAGPaint);
  result.Kind := pkSolid;
  result.Color := c;
End;

Function MakeStops(Const Colors: Array Of TAGColor): TAGGradientStops;
Var
  i: Integer;
Begin
  result := Nil;
  setlength(result, length(Colors));
  For i := 0 To high(Colors) Do Begin
    If length(Colors) = 1 Then
      result[i].Pos := 0
    Else
      result[i].Pos := i / (length(Colors) - 1);
    result[i].Color := Colors[i];
  End;
End;

Function TAGCanvas.Linear(x0, y0, x1, y1: Double; Const Colors: Array Of TAGColor): TAGPaint;
Begin
  result := Default(TAGPaint);
  result.Kind := pkLinear;
  result.p0 := TransformPoint(x0, y0);
  result.p1 := TransformPoint(x1, y1);
  result.Stops := MakeStops(Colors);
  If length(Colors) > 0 Then result.Color := Colors[0];
End;

Function TAGCanvas.Radial(cx, cy, r: Double; Const Colors: Array Of TAGColor): TAGPaint;
Begin
  result := Default(TAGPaint);
  result.Kind := pkRadial;
  result.p0 := TransformPoint(cx, cy);
  result.r := r * DeviceScale;
  result.Stops := MakeStops(Colors);
  If length(Colors) > 0 Then result.Color := Colors[0];
End;

Function TAGCanvas.PaintColorAt(Const Paint: TAGPaint; x, y: Double): TAGColor;
Var
  t, dx, dy, l2: Double;
  i: Integer;
Begin
  If (Paint.Kind = pkSolid) Or (length(Paint.Stops) < 2) Then exit(Paint.Color);
  If Paint.Kind = pkLinear Then Begin
    dx := Paint.p1.x - Paint.p0.x;
    dy := Paint.p1.y - Paint.p0.y;
    l2 := dx * dx + dy * dy;
    If l2 = 0 Then
      t := 0
    Else
      t := ((x - Paint.p0.x) * dx + (y - Paint.p0.y) * dy) / l2;
  End
  Else Begin
    If Paint.r <= 0 Then
      t := 0
    Else
      t := AGSqrt(sqr(x - Paint.p0.x) + sqr(y - Paint.p0.y)) / Paint.r;
  End;
  t := AGClamp(t, 0, 1);
  For i := 0 To high(Paint.Stops) - 1 Do Begin
    If t <= Paint.Stops[i + 1].Pos Then Begin
      If Paint.Stops[i + 1].Pos <= Paint.Stops[i].Pos Then exit(Paint.Stops[i + 1].Color);
      exit(AGMix(Paint.Stops[i].Color, Paint.Stops[i + 1].Color,
        (t - Paint.Stops[i].Pos) / (Paint.Stops[i + 1].Pos - Paint.Stops[i].Pos)));
    End;
  End;
  result := Paint.Stops[high(Paint.Stops)].Color;
End;

Type
  TEdge = Record
    x0, y0, x1, y1: Int64; // fixed point, y0 < y1
    Dir: Integer; // +1 downwards in the original path, -1 upwards
  End;

Procedure TAGCanvas.RasterizePaths(Const Paths: Array Of TAGPoints; Const Paint: TAGPaint; Rule: TAGFillRule);
Var
  Edges: Array Of TEdge;
  Cov: Array Of Integer; // coverage per pixel of the current row, max = SubSamples * FixOne
  Xs: Array Of Int64;
  Ws: Array Of Integer;
  i, j, k, n, ec, row, sub, cnt, wind, px, pxa, pxb, x0i, x1i: Integer;
  ys, xa, xb, ymin, ymax: Int64;
  e: TEdge;
  p, q: TAGPoint;
  tx: Int64;
  tw: Integer;
  inside, wasInside: Boolean;
  rowMin, rowMax, rowY0, rowY1, subCount, maxCov, alpha: Integer;
  c: TAGColor;
Begin
  // Edges in fixed point
  Edges := Nil;
  ec := 0;
  ymin := High(Int64);
  ymax := Low(Int64);
  For i := 0 To high(Paths) Do Begin
    n := length(Paths[i]);
    If n < 2 Then Continue;
    For j := 0 To n - 1 Do Begin
      p := Paths[i][j];
      q := Paths[i][(j + 1) Mod n];
      e.x0 := AGRound(p.x * FixOne);
      e.y0 := AGRound(p.y * FixOne);
      e.x1 := AGRound(q.x * FixOne);
      e.y1 := AGRound(q.y * FixOne);
      If e.y0 = e.y1 Then Continue;
      e.Dir := 1;
      If e.y0 > e.y1 Then Begin
        tx := e.x0; e.x0 := e.x1; e.x1 := tx;
        tx := e.y0; e.y0 := e.y1; e.y1 := tx;
        e.Dir := -1;
      End;
      If ec >= length(Edges) Then setlength(Edges, ec * 2 + 16);
      Edges[ec] := e;
      inc(ec);
      If e.y0 < ymin Then ymin := e.y0;
      If e.y1 > ymax Then ymax := e.y1;
    End;
  End;
  If ec = 0 Then exit;

  rowY0 := AGClampI(ymin Div FixOne - 1, fClipY0, fClipY1);
  rowY1 := AGClampI(ymax Div FixOne + 1, fClipY0, fClipY1);
  If rowY0 >= rowY1 Then exit;
  If fClipX1 <= fClipX0 Then exit;
  setlength(Cov, fImage.Width + 1);
  setlength(Xs, ec);
  setlength(Ws, ec);
  If AntiAlias Then subCount := SubSamples Else subCount := 1;
  maxCov := subCount * FixOne;

  For row := rowY0 To rowY1 - 1 Do Begin
    rowMin := High(Integer);
    rowMax := -1;
    For sub := 0 To subCount - 1 Do Begin
      // sample y of this sub scanline (in fixed point)
      ys := Int64(row) * FixOne + ((2 * sub + 1) * FixOne) Div (2 * subCount);
      // crossings
      cnt := 0;
      For i := 0 To ec - 1 Do Begin
        If (ys < Edges[i].y0) Or (ys >= Edges[i].y1) Then Continue;
        Xs[cnt] := Edges[i].x0 + ((Edges[i].x1 - Edges[i].x0) * (ys - Edges[i].y0)) Div (Edges[i].y1 - Edges[i].y0);
        Ws[cnt] := Edges[i].Dir;
        inc(cnt);
      End;
      If cnt < 2 Then Continue;
      // insertion sort by x (stable, deterministic)
      For i := 1 To cnt - 1 Do Begin
        tx := Xs[i];
        tw := Ws[i];
        j := i - 1;
        While (j >= 0) And (Xs[j] > tx) Do Begin
          Xs[j + 1] := Xs[j];
          Ws[j + 1] := Ws[j];
          dec(j);
        End;
        Xs[j + 1] := tx;
        Ws[j + 1] := tw;
      End;
      // spans
      wind := 0;
      wasInside := false;
      xa := 0;
      For i := 0 To cnt - 1 Do Begin
        wind := wind + Ws[i];
        If Rule = frNonZero Then
          inside := wind <> 0
        Else
          inside := odd(wind);
        If inside And Not wasInside Then xa := Xs[i];
        If wasInside And Not inside Then Begin
          xb := Xs[i];
          // clip
          If xa < Int64(fClipX0) * FixOne Then xa := Int64(fClipX0) * FixOne;
          If xb > Int64(fClipX1) * FixOne Then xb := Int64(fClipX1) * FixOne;
          If xb > xa Then Begin
            If AntiAlias Then Begin
              pxa := xa Div FixOne;
              pxb := xb Div FixOne;
              If pxa = pxb Then
                Cov[pxa] := Cov[pxa] + (xb - xa)
              Else Begin
                Cov[pxa] := Cov[pxa] + (FixOne - (xa - Int64(pxa) * FixOne));
                For k := pxa + 1 To pxb - 1 Do
                  Cov[k] := Cov[k] + FixOne;
                If pxb < length(Cov) Then
                  Cov[pxb] := Cov[pxb] + (xb - Int64(pxb) * FixOne);
              End;
              If pxa < rowMin Then rowMin := pxa;
              If pxb > rowMax Then rowMax := pxb;
            End
            Else Begin
              // a pixel is set if its center lies in [xa, xb)
              x0i := (xa - FixOne Div 2 + FixOne - 1) Div FixOne; // xa >= 0 after clipping
              x1i := (xb - FixOne Div 2 + FixOne - 1) Div FixOne - 1;
              For k := x0i To x1i Do
                If (k >= fClipX0) And (k < fClipX1) Then Cov[k] := FixOne;
              If x0i < rowMin Then rowMin := x0i;
              If x1i > rowMax Then rowMax := x1i;
            End;
          End;
        End;
        wasInside := inside;
      End;
    End;
    // write the row
    If rowMax < 0 Then Continue;
    rowMin := AGClampI(rowMin, fClipX0, fClipX1 - 1);
    rowMax := AGClampI(rowMax, fClipX0, fClipX1 - 1);
    For px := rowMin To rowMax Do Begin
      If Cov[px] > 0 Then Begin
        alpha := (Int64(AGClampI(Cov[px], 0, maxCov)) * 255 + maxCov Div 2) Div maxCov;
        c := PaintColorAt(Paint, px + 0.5, row + 0.5);
        fImage.BlendPixel(px, row, c, alpha);
      End;
    End;
    For px := AGClampI(rowMin - 1, 0, high(Cov)) To AGClampI(rowMax + 1, 0, high(Cov)) Do
      Cov[px] := 0;
  End;
End;

Procedure TAGCanvas.FillPaint(Const Paint: TAGPaint; Keep: Boolean);
Var
  paths: Array Of TAGPoints;
Begin
  FlushCurrent(true);
  paths := fSubPaths;
  RasterizePaths(paths, Paint, FillRule);
  If Not Keep Then BeginPath;
End;

Procedure TAGCanvas.Fill(c: TAGColor; Keep: Boolean);
Begin
  FillPaint(Solid(c), Keep);
End;

Procedure TAGCanvas.Stroke(Width: Double; c: TAGColor; Keep: Boolean);
Var
  polys: Array Of TAGPoints;
  hw: Double;

  Procedure AddPoly(p: TAGPoints);
  Begin
    // all parts with the same orientation, so nonzero gives their union
    If SignedArea(p) < 0 Then Reverse(p);
    setlength(polys, length(polys) + 1);
    polys[high(polys)] := p;
  End;

  Procedure AddDisc(Const m: TAGPoint);
  Var
    p: TAGPoints;
    n, i: Integer;
  Begin
    n := AGClampI(AGCeil(AG_TWO_PI * hw / 2), 8, 64);
    p := Nil;
    setlength(p, n);
    For i := 0 To n - 1 Do Begin
      p[i].x := m.x + hw * AGCos(AG_TWO_PI * i / n);
      p[i].y := m.y + hw * AGSin(AG_TWO_PI * i / n);
    End;
    AddPoly(p);
  End;

  Procedure AddSegment(Const a, b: TAGPoint);
  Var
    p: TAGPoints;
    dx, dy, l, nx, ny: Double;
  Begin
    dx := b.x - a.x;
    dy := b.y - a.y;
    l := AGSqrt(dx * dx + dy * dy);
    If l = 0 Then exit;
    nx := -dy / l * hw;
    ny := dx / l * hw;
    p := Nil;
    setlength(p, 4);
    p[0] := AGPoint(a.x + nx, a.y + ny);
    p[1] := AGPoint(b.x + nx, b.y + ny);
    p[2] := AGPoint(b.x - nx, b.y - ny);
    p[3] := AGPoint(a.x - nx, a.y - ny);
    AddPoly(p);
  End;

Var
  i, j, n: Integer;
  sp: TAGPoints;
  oldRule: TAGFillRule;
Begin
  // A sub path that is still open is stroked as a polyline
  FlushCurrent(false);
  hw := Width * DeviceScale / 2;
  polys := Nil;
  If hw > 0 Then Begin
    For i := 0 To high(fSubPaths) Do Begin
      sp := fSubPaths[i];
      n := length(sp);
      For j := 0 To n - 1 Do Begin
        AddDisc(sp[j]);
        If j < n - 1 Then
          AddSegment(sp[j], sp[j + 1])
        Else If fSubClosed[i] Then
          AddSegment(sp[j], sp[0]);
      End;
    End;
    oldRule := FillRule;
    RasterizePaths(polys, Solid(c), frNonZero);
    FillRule := oldRule;
  End;
  If Not Keep Then BeginPath;
End;

Function TAGCanvas.PathBounds(Out x0, y0, x1, y1: Double): Boolean;
Var
  i, j: Integer;

  Procedure Add(Const p: TAGPoint);
  Begin
    If Not result Then Begin
      x0 := p.x; x1 := p.x; y0 := p.y; y1 := p.y;
      result := true;
    End
    Else Begin
      x0 := AGMin(x0, p.x); x1 := AGMax(x1, p.x);
      y0 := AGMin(y0, p.y); y1 := AGMax(y1, p.y);
    End;
  End;

Begin
  result := false;
  x0 := 0; y0 := 0; x1 := 0; y1 := 0;
  For i := 0 To high(fSubPaths) Do
    For j := 0 To high(fSubPaths[i]) Do
      Add(fSubPaths[i][j]);
  For j := 0 To high(fCurrent) Do
    Add(fCurrent[j]);
End;

End.
