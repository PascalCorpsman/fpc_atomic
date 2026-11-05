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
 * Deterministic math for the asset generator.
 *
 * All generated images must be bit identical on every platform and compiler
 * version. The RTL functions sin, cos, sqrt, ... may be computed with the x87
 * unit (80 bit) on some targets and with SSE (64 bit) on others, so they can
 * differ in the last bit. Everything here only uses +, -, *, / on Double,
 * which IEEE 754 defines exactly. Never use Extended in the generator.
 *
 * Constant expressions like 1 / 3 are folded by the compiler, on targets with
 * Extended = 80 bit in Extended precision. The polynomial coefficients are
 * therefore computed at runtime (in Double) in the initialization section.
 *)
Unit uag_math;

{$MODE ObjFPC}{$H+}

Interface

Const
  AG_PI = 3.14159265358979323846;
  AG_TWO_PI = 6.28318530717958647692;
  AG_HALF_PI = 1.57079632679489661923;

Function AGFloor(x: Double): Int64;
Function AGCeil(x: Double): Int64;
Function AGRound(x: Double): Int64; // Rounds half up (floor(x + 0.5)), unlike Round (banker's rounding)
Function AGSin(x: Double): Double;
Function AGCos(x: Double): Double;
Function AGSqrt(x: Double): Double;
Function AGAtan2(y, x: Double): Double;
Function AGDeg(Degree: Double): Double; // Degree -> Radian
Function AGClamp(x, lo, hi: Double): Double;
Function AGClampI(x, lo, hi: Int64): Int64;
Function AGLerp(a, b, t: Double): Double;
Function AGMin(a, b: Double): Double;
Function AGMax(a, b: Double): Double;
Function AGAbs(x: Double): Double;
Function AGSmoothStep(t: Double): Double; // 0..1 -> 0..1, soft start and end
Function AGExp(x: Double): Double; // e^x, x is clamped to [-700, 700]
Function AGLn(x: Double): Double; // natural logarithm, x <= 0 gives -1e300
Function AGPow(b, x: Double): Double; // b^x for b > 0, 0 for b <= 0

Implementation

Var
  SinC: Array[1..8] Of Double; // (-1)^k / (2k+1)!
  AtanC: Array[1..9] Of Double; // (-1)^k / (2k+1)
  ExpC: Array[0..16] Of Double; // 1 / k!
  LnC: Array[0..12] Of Double; // 1 / (2k+1)
  Ln2: Double; // computed in the initialization from the series

Function AGFloor(x: Double): Int64;
Begin
  result := Trunc(x);
  If x < result Then dec(result);
End;

Function AGCeil(x: Double): Int64;
Begin
  result := Trunc(x);
  If x > result Then inc(result);
End;

Function AGRound(x: Double): Int64;
Begin
  result := AGFloor(x + 0.5);
End;

(*
 * sin on [-pi/2, pi/2] via Taylor polynomial (Horner), error < 1e-15
 *)
Function SinPoly(x: Double): Double;
Var
  x2, p: Double;
  k: Integer;
Begin
  x2 := x * x;
  p := SinC[8];
  For k := 7 Downto 1 Do
    p := SinC[k] + x2 * p;
  result := x * (1.0 + x2 * p);
End;

Function AGSin(x: Double): Double;
Begin
  // Range reduction to [-pi, pi]
  x := x - AG_TWO_PI * AGRound(x / AG_TWO_PI);
  // Then to [-pi/2, pi/2] using sin(pi - x) = sin(x)
  If x > AG_HALF_PI Then x := AG_PI - x;
  If x < -AG_HALF_PI Then x := -AG_PI - x;
  result := SinPoly(x);
End;

Function AGCos(x: Double): Double;
Begin
  result := AGSin(x + AG_HALF_PI);
End;

Function AGSqrt(x: Double): Double;
Var
  i: Integer;
  e: Integer;
  m: Double;
Begin
  If x <= 0 Then exit(0);
  // Start value: scale x into [1, 4) by powers of 4 so Newton converges in a fixed number of steps
  m := x;
  e := 0;
  While m >= 4 Do Begin
    m := m / 4;
    inc(e);
  End;
  While m < 1 Do Begin
    m := m * 4;
    dec(e);
  End;
  result := 0.5 * (m + 1); // sqrt(m) in [1, 2), start above the root
  For i := 1 To 8 Do
    result := 0.5 * (result + m / result);
  While e > 0 Do Begin
    result := result * 2;
    dec(e);
  End;
  While e < 0 Do Begin
    result := result / 2;
    inc(e);
  End;
End;

(*
 * atan on [0, 1] via the identity atan(x) = 2 atan(x / (1 + sqrt(1 + x^2))) twice,
 * then a Taylor series on the reduced argument (|x| < 0.2).
 *)
Function AtanSmall(x: Double): Double;
Var
  x2, p: Double;
  k: Integer;
Begin
  x2 := x * x;
  p := AtanC[9];
  For k := 8 Downto 1 Do
    p := AtanC[k] + x2 * p;
  result := x * (1.0 + x2 * p);
End;

Function AtanUnit(x: Double): Double; // x in [0, 1]
Var
  y: Double;
Begin
  y := x / (1 + AGSqrt(1 + x * x));
  y := y / (1 + AGSqrt(1 + y * y));
  result := 4 * AtanSmall(y);
End;

Function AGAtan2(y, x: Double): Double;
Var
  ax, ay, a: Double;
Begin
  ax := AGAbs(x);
  ay := AGAbs(y);
  If (ax = 0) And (ay = 0) Then exit(0);
  If ay <= ax Then
    a := AtanUnit(ay / ax)
  Else
    a := AG_HALF_PI - AtanUnit(ax / ay);
  If x < 0 Then a := AG_PI - a;
  If y < 0 Then a := -a;
  result := a;
End;

Function AGDeg(Degree: Double): Double;
Begin
  result := Degree * (AG_PI / 180.0);
End;

Function AGClamp(x, lo, hi: Double): Double;
Begin
  If x < lo Then exit(lo);
  If x > hi Then exit(hi);
  result := x;
End;

Function AGClampI(x, lo, hi: Int64): Int64;
Begin
  If x < lo Then exit(lo);
  If x > hi Then exit(hi);
  result := x;
End;

Function AGLerp(a, b, t: Double): Double;
Begin
  result := a + (b - a) * t;
End;

Function AGMin(a, b: Double): Double;
Begin
  If a < b Then result := a Else result := b;
End;

Function AGMax(a, b: Double): Double;
Begin
  If a > b Then result := a Else result := b;
End;

Function AGAbs(x: Double): Double;
Begin
  If x < 0 Then result := -x Else result := x;
End;

Function AGSmoothStep(t: Double): Double;
Begin
  t := AGClamp(t, 0, 1);
  result := t * t * (3 - 2 * t);
End;

Function AGExp(x: Double): Double;
Var
  k, i: Integer;
  r, p: Double;
Begin
  x := AGClamp(x, -700, 700);
  // x = k * ln 2 + r with |r| <= ln 2 / 2, e^x = 2^k * e^r
  k := AGRound(x / Ln2);
  r := x - k * Ln2;
  p := ExpC[16];
  For i := 15 Downto 0 Do
    p := ExpC[i] + r * p;
  While k > 0 Do Begin
    p := p * 2;
    dec(k);
  End;
  While k < 0 Do Begin
    p := p / 2;
    inc(k);
  End;
  result := p;
End;

(*
 * ln(m) = 2 atanh(s) with s = (m - 1) / (m + 1), m scaled by powers of 2 into
 * [0.7071, 1.4142] so |s| < 0.172 and 13 terms are exact to Double precision.
 *)
Function LnReduced(m: Double): Double;
Var
  s, s2, p: Double;
  k: Integer;
Begin
  s := (m - 1) / (m + 1);
  s2 := s * s;
  p := LnC[12];
  For k := 11 Downto 0 Do
    p := LnC[k] + s2 * p;
  result := 2 * s * p;
End;

Function AGLn(x: Double): Double;
Var
  e: Integer;
Begin
  If x <= 0 Then exit(-1E300);
  e := 0;
  While x > 1.4142135623730951 Do Begin
    x := x / 2;
    inc(e);
  End;
  While x < 0.7071067811865476 Do Begin
    x := x * 2;
    dec(e);
  End;
  result := e * Ln2 + LnReduced(x);
End;

Function AGPow(b, x: Double): Double;
Begin
  If b <= 0 Then exit(0);
  If x = 0 Then exit(1);
  result := AGExp(x * AGLn(b));
End;

Procedure InitCoefficients;
Var
  k: Integer;
  f, sign: Double;
Begin
  f := 1;
  sign := -1;
  For k := 1 To 8 Do Begin
    f := f * (2 * k) * (2 * k + 1); // (2k+1)!, exact up to k = 8
    SinC[k] := sign / f;
    sign := -sign;
  End;
  sign := -1;
  For k := 1 To 9 Do Begin
    AtanC[k] := sign / (2 * k + 1);
    sign := -sign;
  End;
  f := 1;
  For k := 0 To 16 Do Begin
    If k > 0 Then f := f * k;
    ExpC[k] := 1 / f;
  End;
  For k := 0 To 12 Do
    LnC[k] := 1 / (2 * k + 1);
  // ln 2 = 2 * ln(sqrt 2), the argument is inside the fast range of the series
  Ln2 := 2 * LnReduced(AGSqrt(2));
End;

Initialization
  InitCoefficients;

End.
