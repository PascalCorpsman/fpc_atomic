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
 * Own sound designs for wav2ini. Every game sound gets a recipe (explosion,
 * impact, boing, whoosh, warp, ...) that is only parameterized by the coarse
 * features of the original (length, attack, where the level drops, brightness,
 * pitch level, number of late hits). Spoken sounds get OWN phrases, spoken by
 * the formant voice of uag_sound with ring modulation; of the recording only
 * the length and the pitch level are used.
 *)
Unit uw2i_design;

{$MODE ObjFPC}{$H+}

Interface

Uses SysUtils, Classes, Math, uw2i_analysis;

Type

  TDPart = Record
    Wave: String;
    T, Len, F0, F1, Vol, Attack, Curve, Duty, VibHz, VibDepth, Gap, Ring, Bw, Lp0, Lp1: Double;
    Rep: Integer;
    Fm: Array[1..3, 0..1] Of Double;
  End;

  TDParts = Array Of TDPart;

  TDesign = Record
    Kind: String; // recipe name, for the comment
    Text: String; // own phrase (voices) or short description
    Parts: TDParts;
  End;

  TNoiseCalib = Function(Lp: Double): Double; // low pass of white noise -> measured brightness

Var
  NoiseBrightness: TNoiseCalib = Nil; // set by the main program (renders and measures)
  NoiseGain: TNoiseCalib = Nil; // low pass -> factor that gives the loudness of white noise

Function DesignSound(Const Name: String; Const F: TFeatures): TDesign;
Function PartLine(Const P: TDPart): String;
Procedure ScaleVolumes(Var D: TDesign; Factor: Double);

Implementation

Var
  FS: TFormatSettings;

Function Num(v: Double): String;
Begin
  If (Abs(v) >= 20) Or (Frac(v) = 0) Then
    result := IntToStr(Round(v))
  Else If Abs(v) >= 2 Then
    result := FormatFloat('0.#', v, FS)
  Else
    result := FormatFloat('0.###', v, FS);
End;

Function Rng(a, b: Double): String;
Begin
  If Num(a) = Num(b) Then
    result := Num(a)
  Else
    result := Num(a) + '..' + Num(b);
End;

Function PartLine(Const P: TDPart): String;
Begin
  result := Format('%-8s', [P.Wave]);
  If P.T > 0 Then result := result + ' t=' + Num(P.T);
  result := result + ' len=' + Num(P.Len) + ' f=' + Rng(P.F0, P.F1);
  If P.Wave = 'voice' Then
    result := result + ' f1=' + Rng(P.Fm[1, 0], P.Fm[1, 1]) + ' f2=' + Rng(P.Fm[2, 0], P.Fm[2, 1]) +
      ' f3=' + Rng(P.Fm[3, 0], P.Fm[3, 1]);
  result := result + ' vol=' + Num(P.Vol);
  If P.Attack <> 1 Then result := result + ' attack=' + Num(P.Attack);
  If P.Curve <> 1 Then result := result + ' curve=' + Num(P.Curve);
  If (P.Wave = 'square') And (P.Duty <> 0.5) Then result := result + ' duty=' + Num(P.Duty);
  If P.VibHz > 0 Then result := result + ' vib=' + Num(P.VibHz) + ',' + Num(P.VibDepth);
  If P.Rep > 1 Then result := result + ' rep=' + IntToStr(P.Rep) + ',' + Num(P.Gap);
  If P.Ring > 0 Then result := result + ' ring=' + Num(P.Ring);
  If (P.Wave = 'voice') And (P.Bw <> 80) Then result := result + ' bw=' + Num(P.Bw);
  If P.Lp0 > 0 Then result := result + ' lp=' + Rng(P.Lp0, P.Lp1);
End;

Procedure ScaleVolumes(Var D: TDesign; Factor: Double);
Var
  i: Integer;
Begin
  For i := 0 To high(D.Parts) Do
    D.Parts[i].Vol := Min(4, D.Parts[i].Vol * Factor);
End;

{ ---------- helpers ---------- }

Function NewPart(Const Wave: String; T, Len, F0, F1, Vol: Double): TDPart;
Begin
  result := Default(TDPart);
  result.Wave := Wave;
  result.T := Round(T);
  result.Len := Max(4, Round(Len));
  result.F0 := F0;
  result.F1 := F1;
  result.Vol := Vol;
  result.Attack := 1;
  result.Curve := 1;
  result.Duty := 0.5;
  result.Rep := 1;
  result.Bw := 80;
End;

Procedure Add(Var D: TDesign; Const P: TDPart);
Begin
  setlength(D.Parts, length(D.Parts) + 1);
  D.Parts[high(D.Parts)] := P;
End;

(* deterministic small number in [0, 1) from the name, for differences between copies *)
Function NameRnd(Const Name: String; Salt: Integer): Double;
Var
  h: LongWord;
  i: Integer;
Begin
  h := 2166136261 Xor LongWord(Salt * 7919);
  For i := 1 To length(Name) Do Begin
    h := h Xor ord(Name[i]);
    h := h * 16777619;
  End;
  h := h Xor (h Shr 15);
  h := h * 2246822519;
  h := h Xor (h Shr 13);
  result := (h And $FFFFFF) / 16777216;
End;

(* low pass for white noise that gives the wanted brightness (spectral centroid), 0 = none *)
Function LpFor(Bright: Double): Double;
Var
  lo, hi, mid: Double;
  i: Integer;
Begin
  If Bright <= 0 Then Bright := 2000;
  If Not assigned(NoiseBrightness) Then exit(Max(60, Bright / 2));
  If Bright >= 0.95 * NoiseBrightness(0) Then exit(0);
  lo := 30;
  hi := 11000;
  For i := 1 To 30 Do Begin
    mid := Sqrt(lo * hi);
    If NoiseBrightness(mid) < Bright Then lo := mid Else hi := mid;
  End;
  result := Round(Sqrt(lo * hi));
End;

(* white noise, low passed so the brightness glides from BrightA to BrightB *)
Function NoiseP(T, Len, BrightA, BrightB, Vol: Double): TDPart;
Begin
  result := NewPart('noise', T, Len, 22050, 22050, Vol);
  result.Lp0 := LpFor(BrightA);
  result.Lp1 := LpFor(BrightB);
  If (result.Lp0 = 0) And (result.Lp1 > 0) Then result.Lp0 := 11000;
  If (result.Lp1 = 0) And (result.Lp0 > 0) Then result.Lp1 := 11000;
  If assigned(NoiseGain) And (result.Lp0 > 0) Then
    result.Vol := Vol * NoiseGain(Sqrt(result.Lp0 * result.Lp1));
End;

(* noise of a speech sound: white, low passed below Color (22050 = white) *)
Function SpNoise(T, Len, Color, Vol: Double): TDPart;
Begin
  result := NewPart('noise', T, Len, 22050, 22050, Vol);
  If Color < 22050 Then Begin
    result.Lp0 := Round(Color * 0.6);
    result.Lp1 := result.Lp0;
    If assigned(NoiseGain) Then result.Vol := Vol * Sqrt(NoiseGain(result.Lp0)); // speech: only half way
  End;
End;

(* time (ms) after the loudest point when the envelope first drops below Db, Def if never *)
Function DropTime(Const F: TFeatures; Db: Double; Def: Double): Double;
Var
  i, loud: Integer;
  a, b: TEnvPoint;
Begin
  loud := 0;
  For i := 0 To high(F.Env) Do
    If F.Env[i].Db > F.Env[loud].Db Then loud := i;
  For i := loud + 1 To high(F.Env) Do
    If F.Env[i].Db < Db Then Begin
      a := F.Env[i - 1];
      b := F.Env[i];
      If b.Db = a.Db Then exit(b.Ms);
      exit(a.Ms + (b.Ms - a.Ms) * (a.Db - Db) / (a.Db - b.Db));
    End;
  result := Def;
End;

(* curve so that (1 - t/Len)^curve reaches -20 dB at time T20 *)
Function FitCurve(T20, Len: Double): Double;
Var
  x: Double;
Begin
  x := EnsureRange(T20 / Max(1, Len), 0.05, 0.97);
  result := EnsureRange(Ln(0.1) / Ln(1 - x), 0.3, 8);
  result := Round(result * 10) / 10;
End;

(* curve so that (1 - t/Len)^curve reaches -30 dB at time T30 *)
Function FitCurve30(T30, Len: Double): Double;
Var
  x: Double;
Begin
  x := EnsureRange(T30 / Max(1, Len), 0.05, 0.97);
  result := EnsureRange(Ln(0.0316) / Ln(1 - x), 0.3, 8);
  result := Round(result * 10) / 10;
End;

Function Active(Const F: TFeatures; Def: Double): Double;
Begin
  If F.ActiveMs > 0 Then result := F.ActiveMs Else result := Def;
End;

(* late hits (debris): onsets after Start ms, count and mean distance *)
Procedure LateHits(Const F: TFeatures; Start: Integer; Out First, Count: Integer; Out Gap: Double);
Var
  i, last: Integer;
Begin
  First := -1;
  Count := 0;
  last := 0;
  For i := 0 To high(F.Onsets) Do
    If F.Onsets[i] >= Start Then Begin
      If First < 0 Then First := F.Onsets[i];
      last := F.Onsets[i];
      inc(Count);
    End;
  If Count > 1 Then Gap := (last - First) / (Count - 1) Else Gap := 100;
End;

(* noise that holds until the level of the original drops, then fades (2 lines) *)
Procedure HoldNoise(Var D: TDesign; Const F: TFeatures; Len, BrightA, BrightB, Vol: Double);
Var
  p: TDPart;
  drop: Double;
Begin
  drop := Min(DropTime(F, -8, Len * 0.3), Len * 0.8);
  p := NoiseP(0, Min(drop * 1.5 + 30, Len), BrightA, Sqrt(BrightA * BrightB), Vol);
  p.Attack := Min(30, F.AttackMs) + 1;
  p.Curve := 0.9;
  Add(D, p);
  p := NoiseP(0, Len, BrightA, BrightB, Vol * 0.6);
  p.Attack := drop * 0.5 + 1;
  p.Curve := FitCurve(DropTime(F, -20, Len * 0.7), Len);
  Add(D, p);
End;

{ ---------- recipes ---------- }

Procedure Explosion(Const Name: String; Const F: TFeatures; Var D: TDesign);
Var
  len, drop, bs, be, boom, late: Double;
  p: TDPart;
  first, cnt: Integer;
  gap: Double;
Begin
  D.Kind := 'explosion';
  D.Text := 'crack, roaring blast, low boom, debris, rumble';
  len := Active(F, 1600);
  drop := DropTime(F, -8, len * 0.5);
  bs := F.BrightStart * 1.1;
  be := F.BrightEnd * 0.7;
  boom := 85 + 50 * NameRnd(Name, 1); // own choice per file, not measured
  // crack
  p := NewPart('noise', 0, 60 + 30 * NameRnd(Name, 2), 22050, 22050, 0.32);
  p.Curve := 3;
  Add(D, p);
  // roaring blast, holds until the level drops
  p := NoiseP(0, Min(drop * 1.5 + 100, len), bs, Sqrt(bs * be), 0.32);
  p.Attack := Min(25, Max(2, F.AttackMs));
  p.Curve := 0.9;
  Add(D, p);
  // tail: darker, decays to -20 dB where the original does
  p := NoiseP(0, len, bs * 0.7, be * 0.6, 0.22);
  p.Attack := Round(drop * 0.4);
  p.Curve := FitCurve30(DropTime(F, -30, len * 0.95), len);
  Add(D, p);
  // low boom
  p := NewPart('sine', 0, Min(len, 1000) * 0.75, boom, 30, 0.28);
  p.Attack := 2;
  p.Curve := 1.8;
  Add(D, p);
  p := NewPart('triangle', 0, len * 0.45, boom * 0.62, 26, 0.12);
  p.Curve := 2;
  Add(D, p);
  // falling bomb whistle, if the original starts with a high tone (own shape)
  If (F.PitchStart >= 500) And (F.Tonal >= 0.05) Then Begin
    p := NewPart('sine', 0, 300, F.PitchStart * 1.3, F.PitchStart * 0.45, 0.10);
    p.Attack := 20;
    p.Curve := 0.8;
    Add(D, p);
  End;
  // debris (late hits of the original, only count and mean distance)
  LateHits(F, 250, first, cnt, gap);
  If cnt >= 2 Then Begin
    p := NoiseP(first, 45, 2600, 900, 0.15);
    p.Curve := 3;
    p.Rep := Min(cnt, 8);
    p.Gap := Round(gap);
    Add(D, p);
  End;
  // rumble
  late := len * 0.3;
  p := NewPart('noise', late, len - late, 22050, 22050, 0.30);
  p.Lp0 := 260;
  p.Lp1 := 90;
  If assigned(NoiseGain) Then p.Vol := 0.12 * NoiseGain(150);
  p.Attack := 200;
  p.Curve := 1.5;
  Add(D, p);
End;

Procedure Impact(Const Name: String; Const F: TFeatures; Var D: TDesign; Heavy: Double);
Var
  len, t20, f0, f1, ratio: Double;
  p: TDPart;
  wave: String;
Begin
  D.Kind := 'impact';
  len := Active(F, 250);
  t20 := DropTime(F, -20, len * 0.6);
  If (F.Tonal >= 0.5) And (F.PitchStart > 0) Then Begin
    D.Text := 'tonal thump with click';
    f0 := F.PitchStart * 1.15;
    f1 := Max(F.PitchEnd * 0.8, 30);
    ratio := F.BrightStart / Max(1, F.PitchStart);
    If ratio < 2 Then wave := 'sine'
    Else If ratio < 5 Then wave := 'triangle'
    Else wave := 'square';
    p := NewPart(wave, 0, len, f0, f1, 0.45);
    p.Attack := Min(10, F.AttackMs) + 1;
    p.Curve := FitCurve(t20, len);
    Add(D, p);
    p := NewPart('sine', 0, len * 0.6, f0 * 0.5, f1 * 0.5, 0.25 * Heavy);
    p.Curve := 2;
    Add(D, p);
  End
  Else Begin
    D.Text := 'noise hit with low thump';
    HoldNoise(D, F, len, F.BrightStart, F.BrightEnd, 0.35);
    p := NewPart('sine', 0, Min(160, len), 150 + 40 * NameRnd(Name, 3), 50, 0.35 * Heavy);
    p.Curve := 2;
    Add(D, p);
  End;
  // click
  p := NewPart('noise', 0, 18, 22050, 22050, 0.22);
  p.Curve := 3;
  Add(D, p);
End;

Procedure Kick(Const Name: String; Const F: TFeatures; Var D: TDesign);
Var
  p: TDPart;
Begin
  Impact(Name, F, D, 1.2);
  D.Kind := 'kick';
  // cartoon "whack": short square drop
  p := NewPart('square', 0, 60, 330 + 80 * NameRnd(Name, 4), 140, 0.10);
  p.Duty := 0.3;
  p.Curve := 2;
  Add(D, p);
End;

Procedure Boing(Const Name: String; Const F: TFeatures; Var D: TDesign);
Var
  len, t20, f0, f1: Double;
  p: TDPart;
Begin
  D.Kind := 'boing';
  D.Text := 'springy cartoon boing';
  len := Active(F, 350);
  t20 := DropTime(F, -20, len * 0.8);
  If (F.Tonal >= 0.3) And (F.PitchStart > 0) Then Begin
    f0 := F.PitchStart;
    f1 := F.PitchEnd;
    While f0 > 400 Do Begin // cartoon spring range
      f0 := f0 / 2;
      f1 := f1 / 2;
    End;
    If Abs(f1 / f0 - 1) < 0.08 Then f1 := f0 * 1.35; // a boing must move
  End
  Else Begin
    f0 := 180 + 60 * NameRnd(Name, 5);
    f1 := f0 * 2.1;
  End;
  p := NewPart('triangle', 0, len, f0, f1, 0.5);
  p.Attack := 3;
  p.Curve := FitCurve(t20, len);
  p.VibHz := 14 + 6 * NameRnd(Name, 6);
  p.VibDepth := 0.06;
  Add(D, p);
  p := NewPart('sine', 0, len * 0.7, f0 * 2, f1 * 2, 0.12);
  p.Curve := 2;
  p.VibHz := 14;
  p.VibDepth := 0.06;
  Add(D, p);
  p := NoiseP(0, 20, F.BrightStart, 2000, 0.12);
  p.Curve := 3;
  Add(D, p);
End;

Procedure Whoosh(Const Name: String; Const F: TFeatures; Var D: TDesign; Grab: Boolean);
Var
  len, t20, att: Double;
  p: TDPart;
Begin
  D.Kind := 'whoosh';
  len := Active(F, 200);
  att := Max(5, F.AttackMs);
  t20 := DropTime(F, -20, len * 0.8);
  p := NoiseP(0, len, F.BrightStart * 0.8, F.BrightEnd * 1.1, 0.4);
  p.Attack := att;
  p.Curve := FitCurve(t20, len);
  Add(D, p);
  p := NewPart('sine', 0, len, 300 + 100 * NameRnd(Name, 7), 900, 0.08);
  p.Attack := att;
  p.Curve := 2;
  Add(D, p);
  If Grab Then Begin
    D.Text := 'quick snatch with a click';
    p := NewPart('square', 0, 22, 900, 600, 0.12);
    p.Curve := 2;
    Add(D, p);
  End
  Else
    D.Text := 'air whoosh of a throw';
End;

Procedure Warp(Const Name: String; Const F: TFeatures; Var D: TDesign; Dir: Integer);
Var
  len, t20, a, b: Double;
  p: TDPart;
Begin
  D.Kind := 'warp';
  len := Active(F, 400);
  t20 := DropTime(F, -20, len * 0.8);
  If Dir > 0 Then Begin
    a := 220;
    b := 1700;
    D.Text := 'rising teleport sweep with sizzle';
  End
  Else If Dir < 0 Then Begin
    a := 1700;
    b := 220;
    D.Text := 'falling teleport sweep with sizzle';
  End
  Else Begin
    a := 2400;
    b := 300;
    D.Text := 'short zap';
  End;
  p := NewPart('saw', 0, len, a, b, 0.18);
  p.Attack := Max(5, F.AttackMs * 0.6);
  p.Curve := FitCurve(t20, len);
  p.VibHz := 9;
  p.VibDepth := 0.05;
  If Dir <> 0 Then p.Ring := 60;
  Add(D, p);
  p := NewPart('triangle', 0, len, a * 2, b * 2, 0.14);
  p.Attack := p.Attack;
  p.Curve := 1.5;
  p.VibHz := 23;
  p.VibDepth := 0.03;
  Add(D, p);
  p := NoiseP(0, len, F.BrightStart, F.BrightEnd, 0.14);
  p.Attack := Max(5, F.AttackMs);
  p.Curve := FitCurve(t20, len);
  Add(D, p);
End;

Procedure Brick(Const Name: String; Const F: TFeatures; Var D: TDesign);
Var
  len, clank: Double;
  p: TDPart;
  first, cnt: Integer;
  gap: Double;
Begin
  D.Kind := 'brick';
  D.Text := 'stone block lands: thud, clank, gravel';
  len := Active(F, 650);
  clank := 380 + 160 * NameRnd(Name, 8);
  LateHits(F, 200, first, cnt, gap);
  HoldNoise(D, F, len, F.BrightStart, F.BrightEnd, 0.30);
  p := NewPart('sine', 0, 220, 140, 45, 0.40);
  p.Curve := 2;
  If cnt >= 1 Then Begin
    p.Rep := 2;
    p.Gap := first;
  End;
  Add(D, p);
  p := NewPart('square', 0, 130, clank, clank * 0.97, 0.08);
  p.Curve := 3;
  p.Duty := 0.35;
  Add(D, p);
  p := NewPart('square', 0, 100, clank * 1.53, clank * 1.5, 0.05);
  p.Curve := 3;
  Add(D, p);
  // gravel
  p := NoiseP(60, len * 0.6, 3000, 1500, 0.07);
  p.Curve := 2;
  p.Rep := 1;
  Add(D, p);
End;

Procedure ClickPlatform(Const Name: String; Const F: TFeatures; Var D: TDesign);
Var
  len: Double;
  p: TDPart;
Begin
  D.Kind := 'brick';
  D.Text := 'mechanical click and grinding slide';
  len := Active(F, 600);
  p := NewPart('square', 0, 12, 1800, 1500, 0.25);
  p.Curve := 2;
  Add(D, p);
  p := NoiseP(0, len, F.BrightStart, F.BrightEnd, 0.22);
  p.Attack := Max(10, F.AttackMs);
  p.Curve := FitCurve(DropTime(F, -20, len * 0.8), len);
  Add(D, p);
  p := NewPart('square', 30, 10, 2400, 2200, 0.10);
  p.Rep := Max(2, Round(len / 70));
  p.Gap := 60;
  p.Curve := 2;
  Add(D, p);
  p := NewPart('sine', 0, len * 0.8, 95, 70, 0.12);
  p.Attack := 60;
  p.Curve := 1.5;
  Add(D, p);
End;

{ ---------- speech: own phrases ---------- }

Type
  TPhon = Record
    Tok: String;
    Kind: Char; // V vowel, D diphthong, N nasal, L liquid, F fricative, P plosive, H h, Z voiced fricative
    F1, F2, F3: Double; // target (diphthong: start)
    E1, E2, E3: Double; // diphthong end
    Dur: Double; // ms, unstressed
    NoiseF, NoiseVol: Double;
    Voiced: Boolean;
  End;

Const
  PhonCount = 37;

Var
  Phons: Array[0..PhonCount - 1] Of TPhon;
  PhonN: Integer = 0;

Procedure DefPhon(Const Tok: String; Kind: Char; F1, F2, F3, E1, E2, E3, Dur, NoiseF, NoiseVol: Double; Voiced: Boolean);
Begin
  Phons[PhonN].Tok := Tok;
  Phons[PhonN].Kind := Kind;
  Phons[PhonN].F1 := F1;
  Phons[PhonN].F2 := F2;
  Phons[PhonN].F3 := F3;
  Phons[PhonN].E1 := E1;
  Phons[PhonN].E2 := E2;
  Phons[PhonN].E3 := E3;
  Phons[PhonN].Dur := Dur;
  Phons[PhonN].NoiseF := NoiseF;
  Phons[PhonN].NoiseVol := NoiseVol;
  Phons[PhonN].Voiced := Voiced;
  inc(PhonN);
End;

Procedure InitPhons;
Begin
  // vowels (F1 / F2 / F3)
  DefPhon('a', 'V', 750, 1250, 2500, 0, 0, 0, 95, 0, 0, true);
  DefPhon('e', 'V', 420, 2000, 2650, 0, 0, 0, 95, 0, 0, true);
  DefPhon('i', 'V', 290, 2250, 3000, 0, 0, 0, 90, 0, 0, true);
  DefPhon('o', 'V', 480, 850, 2400, 0, 0, 0, 95, 0, 0, true);
  DefPhon('u', 'V', 320, 780, 2300, 0, 0, 0, 90, 0, 0, true);
  DefPhon('E', 'V', 580, 1800, 2500, 0, 0, 0, 75, 0, 0, true);
  DefPhon('I', 'V', 400, 2000, 2600, 0, 0, 0, 70, 0, 0, true);
  DefPhon('O', 'V', 570, 950, 2400, 0, 0, 0, 75, 0, 0, true);
  DefPhon('U', 'V', 380, 950, 2300, 0, 0, 0, 70, 0, 0, true);
  DefPhon('ae', 'V', 620, 1750, 2600, 0, 0, 0, 90, 0, 0, true);
  DefPhon('@', 'V', 500, 1450, 2500, 0, 0, 0, 50, 0, 0, true);
  DefPhon('6', 'V', 560, 1300, 1800, 0, 0, 0, 70, 0, 0, true);
  // diphthongs
  DefPhon('ai', 'D', 750, 1250, 2500, 320, 2150, 2900, 140, 0, 0, true);
  DefPhon('au', 'D', 750, 1250, 2500, 350, 820, 2300, 140, 0, 0, true);
  DefPhon('oi', 'D', 520, 900, 2400, 320, 2100, 2800, 140, 0, 0, true);
  // nasals, liquids
  DefPhon('m', 'N', 250, 1100, 2300, 0, 0, 0, 65, 0, 0, true);
  DefPhon('n', 'N', 250, 1650, 2600, 0, 0, 0, 60, 0, 0, true);
  DefPhon('N', 'N', 250, 2000, 2700, 0, 0, 0, 65, 0, 0, true);
  DefPhon('l', 'L', 360, 1100, 2700, 0, 0, 0, 55, 0, 0, true);
  DefPhon('r', 'L', 480, 1300, 1700, 0, 0, 0, 50, 2500, 0.05, true);
  DefPhon('j', 'L', 280, 2200, 3000, 0, 0, 0, 45, 0, 0, true);
  DefPhon('v', 'Z', 300, 1000, 2200, 0, 0, 0, 60, 3000, 0.06, true);
  DefPhon('w', 'L', 300, 750, 2200, 0, 0, 0, 50, 0, 0, true);
  DefPhon('z', 'Z', 300, 1600, 2600, 0, 0, 0, 80, 22050, 0.12, true);
  // fricatives
  DefPhon('s', 'F', 0, 0, 0, 0, 0, 0, 95, 22050, 0.16, false);
  DefPhon('S', 'F', 0, 0, 0, 0, 0, 0, 100, 6500, 0.18, false);
  DefPhon('f', 'F', 0, 0, 0, 0, 0, 0, 80, 14000, 0.08, false);
  DefPhon('C', 'F', 0, 0, 0, 0, 0, 0, 85, 11000, 0.12, false);
  DefPhon('x', 'F', 0, 0, 0, 0, 0, 0, 85, 3200, 0.13, false);
  DefPhon('h', 'H', 0, 0, 0, 0, 0, 0, 50, 5000, 0.07, false);
  // plosives: closure, then burst
  DefPhon('p', 'P', 0, 0, 0, 0, 0, 0, 55, 1500, 0.22, false);
  DefPhon('t', 'P', 0, 0, 0, 0, 0, 0, 55, 9000, 0.24, false);
  DefPhon('k', 'P', 0, 0, 0, 0, 0, 0, 60, 3200, 0.24, false);
  DefPhon('b', 'P', 0, 0, 0, 0, 0, 0, 40, 1100, 0.14, true);
  DefPhon('d', 'P', 0, 0, 0, 0, 0, 0, 40, 4000, 0.14, true);
  DefPhon('g', 'P', 0, 0, 0, 0, 0, 0, 45, 2500, 0.14, true);
  DefPhon('_', ' ', 0, 0, 0, 0, 0, 0, 140, 0, 0, false);
End;

Function FindPhon(Const Tok: String): Integer;
Var
  i: Integer;
Begin
  For i := 0 To PhonN - 1 Do
    If Phons[i].Tok = Tok Then exit(i);
  Raise Exception.Create('unknown phoneme: ' + Tok);
End;

Type
  TMood = (mNeutral, mExcited, mSad, mUrgent, mCalm, mFarewell, mMock, mDisgust, mProud);

(* pitch factor at position x (0..1) of the phrase *)
Function Contour(Mood: TMood; x: Double): Double;
Begin
  Case Mood Of
    mExcited: result := 1.25 + 0.35 * Sin(Pi * x) - 0.2 * x;
    mSad: result := 1.15 - 0.5 * x;
    mUrgent: result := 1.2 + 0.1 * Sin(2 * Pi * x) - 0.15 * x;
    mCalm: result := 0.95 - 0.1 * x;
    mFarewell: result := 1.0 + 0.25 * Sin(Pi * x) - 0.25 * x * x;
    mMock: If x < 0.5 Then result := 1.2 Else result := 1.0 - 0.15 * (x - 0.5);
    mDisgust: result := 1.5 - 0.4 * x;
    mProud: result := 1.1 + 0.2 * Sin(Pi * x * 0.8) - 0.3 * x * x;
  Else
    result := 1.1 - 0.3 * x;
  End;
End;

(*
 * Phrase: phonemes separated by blanks, "'" before a vowel = stressed,
 * "|" = word border (short gap), "_" = pause.
 *)
Procedure Speak(Const Phrase: String; Mood: TMood; Pitch, Target, Ring: Double; Var D: TDesign);
Type
  TItem = Record
    Ph: Integer;
    Stress: Boolean;
    Gap: Boolean;
    Dur: Double;
  End;
Var
  items: Array Of TItem;
  words: TStringList;
  i: Integer;
  tok: String;
  fixedSum, vowelSum, scale, t, x0, x1, total, accent: Double;
  ph: TPhon;
  prev: Array[1..3] Of Double;
  havePrev, first, long: Boolean;
  p: TDPart;

  Procedure VoiceLine(T0, Len, A1, A2, A3, B1, B2, B3, Vol, Pa, Pb: Double; Last: Boolean);
  Begin
    p := NewPart('voice', T0, Len + 4, Pa, Pb, Vol);
    p.Fm[1, 0] := A1;
    p.Fm[2, 0] := A2;
    p.Fm[3, 0] := A3;
    p.Fm[1, 1] := B1;
    p.Fm[2, 1] := B2;
    p.Fm[3, 1] := B3;
    If first Then p.Attack := 10 Else p.Attack := 3;
    If Last Then p.Curve := 1.2 Else p.Curve := 0;
    p.Ring := Ring;
    p.Bw := 90;
    Add(D, p);
    first := false;
  End;

  Function IsLastVoiced(Idx: Integer): Boolean;
  Var
    j: Integer;
  Begin
    For j := Idx + 1 To high(items) Do
      If (Not items[j].Gap) And (Phons[items[j].Ph].Kind In ['V', 'D', 'N', 'L', 'Z']) Then exit(false);
    result := true;
  End;

Var
  dur, pa, pb, trans: Double;
Begin
  items := Nil;
  words := TStringList.Create;
  Try
    words.Delimiter := ' ';
    words.StrictDelimiter := true;
    words.QuoteChar := #0;
    words.DelimitedText := Phrase;
    For i := 0 To words.Count - 1 Do Begin
      tok := words[i];
      If tok = '' Then Continue;
      setlength(items, length(items) + 1);
      With items[high(items)] Do Begin
        Stress := false;
        Gap := false;
        If tok = '|' Then Begin
          Gap := true;
          Dur := 35;
          Ph := -1;
          Continue;
        End;
        If tok[1] = '''' Then Begin
          Stress := true;
          Delete(tok, 1, 1);
        End;
        long := (length(tok) > 1) And (tok[length(tok)] = ':');
        If long Then Delete(tok, length(tok), 1);
        Ph := FindPhon(tok);
        Dur := Phons[Ph].Dur;
        If Stress Then Dur := Dur * 1.7;
        If long Then Dur := Dur * 3;
      End;
    End;
  Finally
    words.Free;
  End;
  // fit to the target length: vowels take most of the stretch
  fixedSum := 0;
  vowelSum := 0;
  For i := 0 To high(items) Do
    If (Not items[i].Gap) And (Phons[items[i].Ph].Kind In ['V', 'D']) Then
      vowelSum := vowelSum + items[i].Dur
    Else
      fixedSum := fixedSum + items[i].Dur;
  scale := EnsureRange(Target / Max(1, fixedSum + vowelSum), 0.7, 1.4);
  For i := 0 To high(items) Do
    If Not ((Not items[i].Gap) And (Phons[items[i].Ph].Kind In ['V', 'D'])) Then
      items[i].Dur := items[i].Dur * scale;
  scale := EnsureRange((Target - fixedSum * scale) / Max(1, vowelSum), 0.6, 2.2);
  For i := 0 To high(items) Do
    If (Not items[i].Gap) And (Phons[items[i].Ph].Kind In ['V', 'D']) Then
      items[i].Dur := items[i].Dur * scale;
  total := 0;
  For i := 0 To high(items) Do
    total := total + items[i].Dur;
  // lines
  t := 20;
  havePrev := false;
  first := true;
  For i := 0 To high(items) Do Begin
    dur := items[i].Dur;
    If items[i].Gap Then Begin
      t := t + dur;
      Continue;
    End;
    ph := Phons[items[i].Ph];
    x0 := (t - 20) / total;
    x1 := (t - 20 + dur) / total;
    accent := 1;
    If items[i].Stress Then accent := 1.12;
    pa := Pitch * Contour(Mood, x0) * accent;
    pb := Pitch * Contour(Mood, x1);
    Case ph.Kind Of
      'V', 'D': Begin
          If Not havePrev Then Begin
            prev[1] := ph.F1;
            prev[2] := ph.F2;
            prev[3] := ph.F3;
          End;
          trans := Min(45, dur * 0.4);
          VoiceLine(t, trans, prev[1], prev[2], prev[3], ph.F1, ph.F2, ph.F3, 0.6, pa, pa, false);
          If ph.Kind = 'D' Then Begin
            VoiceLine(t + trans, dur - trans, ph.F1, ph.F2, ph.F3, ph.E1, ph.E2, ph.E3, 0.6, pa, pb, IsLastVoiced(i));
            prev[1] := ph.E1;
            prev[2] := ph.E2;
            prev[3] := ph.E3;
          End
          Else Begin
            VoiceLine(t + trans, dur - trans, ph.F1, ph.F2, ph.F3, ph.F1, ph.F2, ph.F3, 0.6, pa, pb, IsLastVoiced(i));
            prev[1] := ph.F1;
            prev[2] := ph.F2;
            prev[3] := ph.F3;
          End;
          havePrev := true;
        End;
      'N', 'L', 'Z': Begin
          If Not havePrev Then Begin
            prev[1] := ph.F1;
            prev[2] := ph.F2;
            prev[3] := ph.F3;
          End;
          If ph.Kind = 'N' Then
            VoiceLine(t, dur, prev[1], prev[2], prev[3], ph.F1, ph.F2, ph.F3, 0.28, pa, pb, IsLastVoiced(i))
          Else
            VoiceLine(t, dur, prev[1], prev[2], prev[3], ph.F1, ph.F2, ph.F3, 0.4, pa, pb, IsLastVoiced(i));
          prev[1] := ph.F1;
          prev[2] := ph.F2;
          prev[3] := ph.F3;
          havePrev := true;
          If ph.NoiseVol > 0 Then Begin
            p := SpNoise(t, dur, ph.NoiseF, ph.NoiseVol);
            p.Attack := 5;
            p.Curve := 0;
            Add(D, p);
          End;
        End;
      'F', 'H': Begin
          p := SpNoise(t, dur, ph.NoiseF, ph.NoiseVol);
          p.Attack := dur * 0.3;
          p.Curve := 0.8;
          Add(D, p);
          havePrev := false;
        End;
      'P': Begin
          // closure is silence, burst at the end
          p := SpNoise(t + dur * 0.65, Max(12, dur * 0.35), ph.NoiseF, ph.NoiseVol);
          p.Curve := 3;
          Add(D, p);
          havePrev := false;
        End;
    End;
    t := t + dur;
  End;
  // breath: a little bright noise under the whole phrase
  p := NewPart('noise', 20, t - 20, 22050, 22050, 0.035);
  p.Attack := 30;
  p.Curve := 0.7;
  Add(D, p);
End;

(* own screams: no words, a vowel with an own pitch shape *)
Procedure Scream(Const F: TFeatures; Var D: TDesign; Long: Boolean);
Var
  len, base: Double;
  p: TDPart;

  Procedure V(T, Len, Pa, Pb, A1, B1, A2, B2, A3, B3, Vol, Curve: Double; VibHz, VibD: Double);
  Begin
    p := NewPart('voice', T, Len, Pa, Pb, Vol);
    p.Fm[1, 0] := A1;
    p.Fm[1, 1] := B1;
    p.Fm[2, 0] := A2;
    p.Fm[2, 1] := B2;
    p.Fm[3, 0] := A3;
    p.Fm[3, 1] := B3;
    p.Attack := 8;
    p.Curve := Curve;
    p.VibHz := VibHz;
    p.VibDepth := VibD;
    p.Bw := 110;
    p.Ring := 35;
    Add(D, p);
  End;

Begin
  D.Kind := 'scream';
  len := Active(F, 1200);
  base := EnsureRange(Sqrt(Max(1, F.PitchMin) * Max(1, F.PitchMax)), 300, 600);
  If Long Then Begin
    D.Text := 'own falling "Neeeiiin..." wail, then a soft thud';
    V(0, len * 0.25, base * 0.8, base * 1.15, 300, 450, 1700, 2000, 2600, 2600, 0.6, 0, 6, 0.02);
    V(len * 0.25 - 4, len * 0.55, base * 1.15, base * 0.55, 450, 700, 2000, 1900, 2600, 2700, 0.6, 0, 5.5, 0.05);
    V(len * 0.8 - 4, len * 0.17, base * 0.55, base * 0.35, 700, 300, 1900, 2100, 2700, 2900, 0.45, 1.5, 5, 0.06);
    p := NoiseP(len * 0.9, 220, 900, 350, 0.3);
    p.Curve := 2.5;
    Add(D, p);
    p := NewPart('sine', len * 0.9, 200, 120, 45, 0.35);
    p.Curve := 2;
    Add(D, p);
  End
  Else Begin
    D.Text := 'own short "Aaah!" scream, rising then dropping';
    V(0, len * 0.2, base * 0.9, base * 1.3, 600, 850, 1200, 1350, 2500, 2700, 0.6, 0, 0, 0);
    V(len * 0.2 - 4, len * 0.45, base * 1.3, base * 1.2, 850, 850, 1350, 1300, 2700, 2700, 0.62, 0, 7.5, 0.04);
    V(len * 0.65 - 4, len * 0.35, base * 1.2, base * 0.6, 850, 450, 1300, 900, 2700, 2400, 0.55, 1.4, 7, 0.05);
  End;
  p := SpNoise(0, len * 0.8, 9000, 0.04);
  p.Attack := 40;
  p.Curve := 1.5;
  Add(D, p);
End;

Function VoicePitch(Const F: TFeatures; Lo, Hi: Double): Double;
Begin
  // only the pitch level of the recording, clamped to the voice range of the character
  If (F.Tonal > 0.2) And (F.PitchMin > 0) Then
    result := EnsureRange(Sqrt(F.PitchMin * F.PitchMax), Lo, Hi)
  Else
    result := (Lo + Hi) / 2;
  result := Round(result);
End;

Procedure Say(Const F: TFeatures; Var D: TDesign; Const Text, Phrase: String; Mood: TMood; Ring, Lo, Hi, Stretch: Double);
Begin
  D.Kind := 'voice';
  D.Text := 'own phrase "' + Text + '"';
  Speak(Phrase, Mood, VoicePitch(F, Lo, Hi), Active(F, 800) * Stretch - 40, Ring, D);
End;

{ ---------- small own jingles ---------- }

Procedure Note(Var D: TDesign; Const Wave: String; T, Len, F, Vol: Double; Curve: Double = 1.5);
Var
  p: TDPart;
Begin
  p := NewPart(Wave, T, Len, F, F, Vol);
  p.Curve := Curve;
  p.Attack := 3;
  If Wave = 'square' Then p.Duty := 0.25;
  Add(D, p);
End;

Procedure Fanfare(Var D: TDesign);
Begin
  D.Kind := 'jingle';
  D.Text := 'own short victory fanfare (the original file is empty)';
  Note(D, 'square', 0, 140, 523.25, 0.16);
  Note(D, 'square', 130, 140, 659.25, 0.16);
  Note(D, 'square', 260, 140, 783.99, 0.16);
  Note(D, 'square', 390, 700, 1046.5, 0.18, 1.2);
  Note(D, 'triangle', 390, 700, 261.63, 0.3, 1.2);
  Note(D, 'triangle', 0, 390, 196, 0.25, 0.6);
  Note(D, 'sine', 390, 700, 1318.5, 0.07, 1.4);
End;

Procedure PickupBlip(Var D: TDesign);
Begin
  D.Kind := 'pickup';
  D.Text := 'two note rising blip';
  Note(D, 'square', 0, 60, 659.25, 0.20, 1);
  Note(D, 'square', 55, 95, 987.77, 0.20, 1.5);
End;

Procedure Sparkle(Var D: TDesign; Const F: TFeatures);
Var
  p: TDPart;
Begin
  D.Kind := 'pickup';
  D.Text := 'rising sparkle arpeggio';
  Note(D, 'sine', 0, 110, 1568, 0.22, 2);
  Note(D, 'sine', 55, 110, 2093, 0.22, 2);
  Note(D, 'sine', 110, 110, 2637, 0.2, 2);
  Note(D, 'sine', 165, 140, 3136, 0.2, 2);
  p := NoiseP(0, Active(F, 300), F.BrightStart, F.BrightEnd, 0.06);
  p.Attack := 40;
  p.Curve := 2;
  Add(D, p);
End;

Procedure Pop(Var D: TDesign; Const F: TFeatures);
Var
  p: TDPart;
Begin
  D.Kind := 'pickup';
  D.Text := 'bubble pop with a ringing body';
  p := NewPart('sine', 0, 90, 950, 180, 0.5);
  p.Curve := 1.5;
  Add(D, p);
  p := NewPart('noise', 0, 12, 22050, 22050, 0.25);
  p.Curve := 3;
  Add(D, p);
  p := NewPart('sine', 50, Active(F, 340) - 50, 520, 390, 0.12);
  p.Curve := 2;
  p.VibHz := 11;
  p.VibDepth := 0.03;
  Add(D, p);
End;

Procedure Shing(Var D: TDesign; Const F: TFeatures);
Var
  p: TDPart;
  len: Double;
Begin
  D.Kind := 'pickup';
  D.Text := 'metallic blade shing';
  len := Active(F, 430);
  p := NoiseP(0, len * 0.5, 6000, 1500, 0.25);
  p.Attack := 25;
  p.Curve := 2;
  Add(D, p);
  p := NewPart('sine', 20, len, 2350, 2380, 0.16);
  p.Attack := 30;
  p.Curve := 1.6;
  p.VibHz := 7;
  p.VibDepth := 0.008;
  Add(D, p);
  p := NewPart('sine', 20, len * 0.8, 3530, 3560, 0.09);
  p.Attack := 30;
  p.Curve := 2;
  Add(D, p);
  p := NewPart('triangle', 0, len * 0.6, 590, 600, 0.06);
  p.Attack := 30;
  p.Curve := 2;
  Add(D, p);
End;

Procedure Sneeze(Var D: TDesign; Const F: TFeatures);
Begin
  Say(F, D, 'Hatschi!', 'h a | t S ''i', mExcited, 40, 150, 260, 1);
End;

Procedure Cough(Var D: TDesign; Const F: TFeatures);
Var
  p: TDPart;
  cnt: Integer;
Begin
  D.Kind := 'sick';
  D.Text := 'own sickly coughing';
  cnt := EnsureRange(F.OnsetCount, 3, 8);
  p := NoiseP(0, 50, 1800, 1200, 0.35);
  p.Curve := 2;
  p.Rep := cnt;
  p.Gap := Round((Active(F, 600) - 60) / cnt);
  Add(D, p);
  p := NewPart('voice', 0, 45, 160, 130, 0.25);
  p.Fm[1, 0] := 450;
  p.Fm[1, 1] := 450;
  p.Fm[2, 0] := 1500;
  p.Fm[2, 1] := 1500;
  p.Fm[3, 0] := 2400;
  p.Fm[3, 1] := 2400;
  p.Curve := 2;
  p.Rep := cnt;
  p.Gap := Round((Active(F, 600) - 60) / cnt);
  p.Ring := 30;
  Add(D, p);
End;

Procedure Bloop(Var D: TDesign; Const F: TFeatures);
Var
  p: TDPart;
Begin
  D.Kind := 'menu';
  D.Text := 'menu back: whoosh and falling bloop';
  p := NoiseP(0, Active(F, 280), F.BrightStart, F.BrightEnd, 0.2);
  p.Attack := 60;
  p.Curve := 2;
  Add(D, p);
  p := NewPart('sine', 40, 160, 880, 330, 0.35);
  p.Curve := 1.5;
  Add(D, p);
  p := NewPart('square', 40, 90, 440, 220, 0.07);
  p.Curve := 2;
  Add(D, p);
End;

{ ---------- table: file -> recipe ---------- }

Function DesignSound(Const Name: String; Const F: TFeatures): TDesign;
Var
  n: String;
Begin
  If PhonN = 0 Then InitPhons;
  result := Default(TDesign);
  n := LowerCase(Name);
  // explosions
  If (Copy(n, 1, 5) = 'bomb_') Or (Copy(n, 1, 4) = 'expl') Then Explosion(n, F, result)
  // bombs
  Else If (n = 'kbomb1') Or (n = 'kbomb2') Or (n = 'kicker3') Or (n = 'kicker10') Then Kick(n, F, result)
  Else If (n = 'bombstop') Or (n = 'bmbstop1') Or (n = 'bmbstop2') Then Impact(n, F, result, 1.3)
  Else If (n = 'bmdrop1') Or (n = 'bmdrop2') Then Impact(n, F, result, 1)
  Else If (n = 'bmdrop3') Or (n = 'bombboun') Or (n = '1017') Or (n = '1036') Or (n = '1045') Or (n = 'trampo') Then Boing(n, F, result)
  Else If (n = 'grab1') Or (n = 'grab2') Then Whoosh(n, F, result, true)
  Else If Copy(n, 1, 7) = 'bmbthrw' Then Whoosh(n, F, result, false)
  // atomic
  Else If n = 'transin' Then Warp(n, F, result, 1)
  Else If n = 'transout' Then Warp(n, F, result, -1)
  Else If n = 'warp1' Then Warp(n, F, result, 0)
  Else If n = 'scream1' Then Scream(F, result, false)
  Else If n = 'die1' Then Scream(F, result, true)
  Else If n = 'zen1' Then Say(F, result, 'Ommm...', '''o: m:', mCalm, 25, 90, 130, 1)
  Else If n = 'zen2' Then Say(F, result, 'Uaaah... hmm', '''a: u a _ h m:', mCalm, 25, 90, 130, 1)
  // hurry
  Else If (n = 'clikplat') Then ClickPlatform(n, F, result)
  Else If Copy(n, 1, 7) = 'sqrdrop' Then Brick(n, F, result)
  Else If n = 'hurry' Then Say(F, result, 'Beeil dich!', 'b @ ''ai l | d I C', mUrgent, 55, 120, 200, 1)
  Else If n = 'hurytuf' Then Say(F, result, 'Schneller, schneller!', 'S n ''E l 6 _ S n ''E l 6', mUrgent, 55, 120, 200, 1)
  // power ups
  Else If n = 'woohoo1' Then Say(F, result, 'Juhu!', 'j u h ''u', mExcited, 30, 160, 280, 1)
  Else If n = 'allrite' Then Say(F, result, 'Super!', 'z ''u p 6', mExcited, 30, 140, 240, 1)
  Else If n = 'schwing' Then Shing(result, F)
  Else If n = 'get1' Then PickupBlip(result)
  Else If n = 'get2' Then Sparkle(result, F)
  Else If n = 'coolpop' Then Pop(result, F)
  Else If n = 'ohno1' Then Say(F, result, 'Oh nein!', '''o | n ''ai n', mSad, 40, 130, 220, 1)
  Else If n = 'disease1' Then Sneeze(result, F)
  Else If n = 'disease2' Then Say(F, result, 'Iiih, baeh!', '''i _ b ''ae', mDisgust, 45, 180, 300, 1)
  Else If n = 'disease3' Then Cough(result, F)
  // other player died
  Else If n = 'gotcha' Then Say(F, result, 'Erwischt!', 'E 6 | v ''I S t', mMock, 45, 120, 220, 1)
  Else If n = 'later' Then Say(F, result, 'Bye bye!', 'b ai | b ''ai', mMock, 45, 140, 240, 1)
  Else If n = 'cul8r' Then Say(F, result, 'Bis dann!', 'b I s | d ''a n', mFarewell, 45, 120, 220, 1)
  Else If n = 'roasted' Then Say(F, result, 'Knusprig!', 'k n ''U s p r I C', mMock, 45, 120, 220, 1)
  Else If n = 'toeasy' Then Say(F, result, 'Viel zu leicht!', 'f i l | t s u | l ''ai C t', mMock, 45, 110, 200, 1)
  Else If n = 'youblow' Then Say(F, result, 'Das war nix!', 'd a s | v a 6 | n ''I k s', mMock, 45, 110, 200, 1)
  Else If n = 'eatdust' Then Say(F, result, 'Staub geschluckt!', 'S t ''au p | g @ S l ''U k t', mNeutral, 45, 110, 200, 1)
  Else If n = 'smelsmok' Then Say(F, result, 'Riecht nach Rauch!', 'r ''i C t | n a x | r ''au x', mMock, 45, 110, 200, 1)
  Else If n = 'stupidio' Then Say(F, result, 'So ein Pech!', 'z o | ai n | p ''E C', mSad, 45, 110, 200, 1)
  Else If n = 'suckitdn' Then Say(F, result, 'Pech gehabt!', 'p ''E C | g @ h ''a p t', mMock, 45, 110, 200, 1)
  Else If n = 'tastpain' Then Say(F, result, 'Au! Autsch!', '''au _ ''au t S', mExcited, 45, 140, 240, 1)
  Else If n = 'tastpai2' Then Say(F, result, 'Das tat weh!', 'd a s | t a t | v ''e', mMock, 45, 110, 200, 1)
  Else If n = 'gotahurt' Then Say(F, result, 'Das muss weh tun!', 'd a s | m U s | v ''e | t u n', mMock, 45, 110, 200, 1)
  Else If n = 'cribrown' Then Say(F, result, 'Ausgeknipst!', '''au s g @ k n I p s t', mNeutral, 45, 110, 200, 1)
  // winner
  Else If n = 'proud' Then Say(F, result, 'Bravo!', 'b r ''a v o', mProud, 30, 130, 220, 1)
  Else If n = 'theman' Then Say(F, result, 'Champion!', 'S ''E m p j o n', mProud, 30, 120, 200, 1)
  Else If n = 'youwin1' Then Say(F, result, 'Gewonnen!', 'g @ v ''O n @ n', mProud, 30, 140, 240, 1)
  Else If n = '1000' Then Fanfare(result)
  // menu
  Else If n = 'menuexit' Then Bloop(result, F)
  Else If n = 'quitgame' Then Say(F, result, 'Auf Wiedersehen!', 'au f | v ''i d 6 z e @ n', mFarewell, 35, 110, 200, 1)
  Else
    Impact(n, F, result, 1);
End;

Initialization
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

End.
