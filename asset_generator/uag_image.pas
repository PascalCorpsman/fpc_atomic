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
 * Colors and RGBA images of the asset generator.
 *
 * Rules of the game engine every generated image has to follow:
 * - Fuchsia (255, 0, 255) is transparent, it is a hard mask (no alpha). Use
 *   KeyTransparent before saving, it turns every pixel with alpha < 128 into
 *   fuchsia and every other pixel into a fully opaque one.
 * - Player color: on all atomic sheets every pixel with g > r and g > b is
 *   recolored to the player color at runtime. Draw areas that should get the
 *   player color with PlayerGreen, and pass every other color through
 *   NotGreen (see IsGreenDominant).
 *)
Unit uag_image;

{$MODE ObjFPC}{$H+}

Interface

Type

  TAGColor = Packed Record
    r, g, b, a: Byte; // straight (not premultiplied) alpha, 255 = opaque
  End;

  { TAGImage }

  TAGImage = Class
  private
    fWidth, fHeight: Integer;
  public
    Pixels: Array Of TAGColor; // row by row, top left first
    Property Width: Integer read fWidth;
    Property Height: Integer read fHeight;
    Constructor Create(aWidth, aHeight: Integer); // all pixels transparent
    Function Clone: TAGImage;
    Procedure Clear(c: TAGColor);
    Function GetPixel(x, y: Integer): TAGColor; // outside = transparent
    Procedure SetPixel(x, y: Integer; c: TAGColor); // outside = ignored
    Procedure BlendPixel(x, y: Integer; c: TAGColor; Coverage: Integer); // Coverage 0..255, source over
    Procedure Draw(Src: TAGImage; x, y: Integer); // source over
    Procedure Copy(Src: TAGImage; x, y: Integer); // replace, including alpha
    Function Crop(x, y, w, h: Integer): TAGImage;
    Function ScaleNearest(Factor: Integer): TAGImage;
    Procedure KeyTransparent; // alpha < 128 -> fuchsia, everything else -> opaque
    Procedure FlipHorizontal;
  End;

Const
  AGFuchsia: TAGColor = (r: 255; g: 0; b: 255; a: 255);
  AGTransparent: TAGColor = (r: 0; g: 0; b: 0; a: 0);
  AGBlack: TAGColor = (r: 0; g: 0; b: 0; a: 255);
  AGWhite: TAGColor = (r: 255; g: 255; b: 255; a: 255);

Function AGRGB(r, g, b: Integer): TAGColor;
Function AGRGBA(r, g, b, a: Integer): TAGColor;
Function AGHex(Value: Cardinal): TAGColor; // $RRGGBB
Function AGMix(c1, c2: TAGColor; t: Double): TAGColor; // t = 0 -> c1, t = 1 -> c2 (alpha too)
Function AGShade(c: TAGColor; Factor: Double): TAGColor; // < 1 darker, > 1 lighter (towards white)
Function AGWithAlpha(c: TAGColor; a: Integer): TAGColor;
Function AGHSV(h, s, v: Double): TAGColor; // h in degree, s, v 0..1
Function IsGreenDominant(c: TAGColor): Boolean; // the engine would recolor this pixel
Function NotGreen(c: TAGColor): TAGColor; // lowers g just below max(r, b) if needed
Function PlayerGreen(Brightness: Double): TAGColor; // 0 = dark .. 1 = light, recolored by the engine
Function AGSameColor(c1, c2: TAGColor): Boolean;

(*
 * Threshold of the 4 x 4 ordered (Bayer) dither matrix at pixel (x, y),
 * in 0..1 (exclusive), (k + 0.5) / 16 for k = 0..15.
 *)
Function AGBayer4(x, y: Integer): Double;

(*
 * Deterministic value noise in -1..1, smooth between grid points Cell pixel
 * apart (integer hash, bilinear interpolation).
 *)
Function AGValueNoise(x, y, Cell: Integer; Seed: Cardinal): Double;

Implementation

Uses uag_math;

Function AGRGB(r, g, b: Integer): TAGColor;
Begin
  result.r := AGClampI(r, 0, 255);
  result.g := AGClampI(g, 0, 255);
  result.b := AGClampI(b, 0, 255);
  result.a := 255;
End;

Function AGRGBA(r, g, b, a: Integer): TAGColor;
Begin
  result := AGRGB(r, g, b);
  result.a := AGClampI(a, 0, 255);
End;

Function AGHex(Value: Cardinal): TAGColor;
Begin
  result := AGRGB((Value Shr 16) And $FF, (Value Shr 8) And $FF, Value And $FF);
End;

Function AGMix(c1, c2: TAGColor; t: Double): TAGColor;
Begin
  t := AGClamp(t, 0, 1);
  result.r := AGRound(c1.r + (c2.r - c1.r) * t);
  result.g := AGRound(c1.g + (c2.g - c1.g) * t);
  result.b := AGRound(c1.b + (c2.b - c1.b) * t);
  result.a := AGRound(c1.a + (c2.a - c1.a) * t);
End;

Function AGShade(c: TAGColor; Factor: Double): TAGColor;
Begin
  If Factor <= 1 Then Begin
    result := AGRGBA(AGRound(c.r * Factor), AGRound(c.g * Factor), AGRound(c.b * Factor), c.a);
  End
  Else Begin
    result := AGMix(c, AGWithAlpha(AGWhite, c.a), Factor - 1);
  End;
End;

Function AGWithAlpha(c: TAGColor; a: Integer): TAGColor;
Begin
  result := c;
  result.a := AGClampI(a, 0, 255);
End;

Function AGHSV(h, s, v: Double): TAGColor;
Var
  c, x, m, hh: Double;
  r, g, b: Double;
  sector: Integer;
Begin
  h := h - 360 * AGFloor(h / 360);
  s := AGClamp(s, 0, 1);
  v := AGClamp(v, 0, 1);
  c := v * s;
  hh := h / 60;
  sector := AGFloor(hh);
  x := c * (1 - AGAbs(hh - 2 * AGFloor(hh / 2) - 1));
  m := v - c;
  r := 0;
  g := 0;
  b := 0;
  Case sector Of
    0: Begin r := c; g := x; End;
    1: Begin r := x; g := c; End;
    2: Begin g := c; b := x; End;
    3: Begin g := x; b := c; End;
    4: Begin r := x; b := c; End;
  Else Begin r := c; b := x; End;
  End;
  result := AGRGB(AGRound((r + m) * 255), AGRound((g + m) * 255), AGRound((b + m) * 255));
End;

Function IsGreenDominant(c: TAGColor): Boolean;
Begin
  result := (c.g > c.r) And (c.g > c.b);
End;

Function NotGreen(c: TAGColor): TAGColor;
Var
  m: Integer;
Begin
  result := c;
  If IsGreenDominant(c) Then Begin
    m := c.r;
    If c.b > m Then m := c.b;
    result.g := m;
  End;
End;

Function PlayerGreen(Brightness: Double): TAGColor;
Var
  g: Integer;
Begin
  // The engine keeps n = (r + b) / 2 and scales k = g - n with the player color,
  // so r = b keeps the hue neutral, a little r / b gives light tones.
  Brightness := AGClamp(Brightness, 0, 1);
  g := AGRound(60 + 195 * Brightness);
  result := AGRGB(AGRound(g * 0.18 * Brightness), g, AGRound(g * 0.18 * Brightness));
End;

Function AGSameColor(c1, c2: TAGColor): Boolean;
Begin
  result := (c1.r = c2.r) And (c1.g = c2.g) And (c1.b = c2.b) And (c1.a = c2.a);
End;

Function AGBayer4(x, y: Integer): Double;
Const
  M: Array[0..15] Of Integer = (0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5);
Begin
  result := (M[(y And 3) * 4 + (x And 3)] + 0.5) / 16;
End;

Function HashUnit(x, y: Integer; Seed: Cardinal): Double;
Var
  h: Cardinal;
Begin
  {$PUSH}{$R-}{$Q-}
  h := Cardinal(x) * Cardinal(374761393) + Cardinal(y) * Cardinal(668265263) + Seed * Cardinal(2246822519);
  h := (h Xor (h Shr 13)) * Cardinal(1274126177);
  h := h Xor (h Shr 16);
  {$POP}
  result := (h And $FFFF) / 65535 * 2 - 1;
End;

Function AGValueNoise(x, y, Cell: Integer; Seed: Cardinal): Double;
Var
  gx, gy: Integer;
  fx, fy, a, b: Double;
Begin
  If Cell < 1 Then Cell := 1;
  If x < 0 Then x := 0;
  If y < 0 Then y := 0;
  gx := x Div Cell;
  gy := y Div Cell;
  fx := (x Mod Cell) / Cell;
  fy := (y Mod Cell) / Cell;
  a := HashUnit(gx, gy, Seed) + (HashUnit(gx + 1, gy, Seed) - HashUnit(gx, gy, Seed)) * fx;
  b := HashUnit(gx, gy + 1, Seed) + (HashUnit(gx + 1, gy + 1, Seed) - HashUnit(gx, gy + 1, Seed)) * fx;
  result := a + (b - a) * fy;
End;

{ TAGImage }

Constructor TAGImage.Create(aWidth, aHeight: Integer);
Var
  i: Integer;
Begin
  Inherited Create;
  fWidth := aWidth;
  fHeight := aHeight;
  setlength(Pixels, aWidth * aHeight);
  For i := 0 To high(Pixels) Do
    Pixels[i] := AGTransparent;
End;

Function TAGImage.Clone: TAGImage;
Begin
  result := TAGImage.Create(fWidth, fHeight);
  If length(Pixels) > 0 Then
    move(Pixels[0], result.Pixels[0], length(Pixels) * SizeOf(TAGColor));
End;

Procedure TAGImage.Clear(c: TAGColor);
Var
  i: Integer;
Begin
  For i := 0 To high(Pixels) Do
    Pixels[i] := c;
End;

Function TAGImage.GetPixel(x, y: Integer): TAGColor;
Begin
  If (x < 0) Or (y < 0) Or (x >= fWidth) Or (y >= fHeight) Then exit(AGTransparent);
  result := Pixels[y * fWidth + x];
End;

Procedure TAGImage.SetPixel(x, y: Integer; c: TAGColor);
Begin
  If (x < 0) Or (y < 0) Or (x >= fWidth) Or (y >= fHeight) Then exit;
  Pixels[y * fWidth + x] := c;
End;

Procedure TAGImage.BlendPixel(x, y: Integer; c: TAGColor; Coverage: Integer);
Var
  sa, da, oa: Integer;
  p: ^TAGColor;
Begin
  If (x < 0) Or (y < 0) Or (x >= fWidth) Or (y >= fHeight) Then exit;
  If Coverage <= 0 Then exit;
  If Coverage > 255 Then Coverage := 255;
  sa := (c.a * Coverage + 127) Div 255; // 0..255
  If sa = 0 Then exit;
  p := @Pixels[y * fWidth + x];
  If sa = 255 Then Begin
    p^ := c;
    p^.a := 255;
    exit;
  End;
  // Straight alpha "source over" in integers: oa = sa + da * (1 - sa)
  da := (p^.a * (255 - sa) + 127) Div 255;
  oa := sa + da;
  If oa = 0 Then exit;
  p^.r := (c.r * sa + p^.r * da + oa Div 2) Div oa;
  p^.g := (c.g * sa + p^.g * da + oa Div 2) Div oa;
  p^.b := (c.b * sa + p^.b * da + oa Div 2) Div oa;
  p^.a := oa;
End;

Procedure TAGImage.Draw(Src: TAGImage; x, y: Integer);
Var
  i, j: Integer;
Begin
  For j := 0 To Src.Height - 1 Do
    For i := 0 To Src.Width - 1 Do
      BlendPixel(x + i, y + j, Src.Pixels[j * Src.Width + i], 255);
End;

Procedure TAGImage.Copy(Src: TAGImage; x, y: Integer);
Var
  i, j: Integer;
Begin
  For j := 0 To Src.Height - 1 Do
    For i := 0 To Src.Width - 1 Do
      SetPixel(x + i, y + j, Src.Pixels[j * Src.Width + i]);
End;

Function TAGImage.Crop(x, y, w, h: Integer): TAGImage;
Var
  i, j: Integer;
Begin
  result := TAGImage.Create(w, h);
  For j := 0 To h - 1 Do
    For i := 0 To w - 1 Do
      result.Pixels[j * w + i] := GetPixel(x + i, y + j);
End;

Function TAGImage.ScaleNearest(Factor: Integer): TAGImage;
Var
  i, j: Integer;
Begin
  result := TAGImage.Create(fWidth * Factor, fHeight * Factor);
  For j := 0 To result.Height - 1 Do
    For i := 0 To result.Width - 1 Do
      result.Pixels[j * result.Width + i] := Pixels[(j Div Factor) * fWidth + (i Div Factor)];
End;

Procedure TAGImage.KeyTransparent;
Var
  i: Integer;
Begin
  For i := 0 To high(Pixels) Do Begin
    If Pixels[i].a < 128 Then
      Pixels[i] := AGFuchsia
    Else Begin
      Pixels[i].a := 255;
      // A drawn pixel must never become the key color by accident
      If (Pixels[i].r = 255) And (Pixels[i].g = 0) And (Pixels[i].b = 255) Then Pixels[i].b := 254;
    End;
  End;
End;

Procedure TAGImage.FlipHorizontal;
Var
  i, j: Integer;
  t: TAGColor;
Begin
  For j := 0 To fHeight - 1 Do
    For i := 0 To fWidth Div 2 - 1 Do Begin
      t := Pixels[j * fWidth + i];
      Pixels[j * fWidth + i] := Pixels[j * fWidth + fWidth - 1 - i];
      Pixels[j * fWidth + fWidth - 1 - i] := t;
    End;
End;

End.
