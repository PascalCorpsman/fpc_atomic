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
 * Coarse features of a recording for wav2ini. Only a handful of numbers are
 * measured (length, a simplified loudness envelope with at most 8 points,
 * pitch at start / end, tone vs. noise, brightness range, onsets), never a
 * frame by frame track, so a description built from them is an own
 * interpretation and not a (lossy) copy of the recording.
 *)
Unit uw2i_analysis;

{$MODE ObjFPC}{$H+}

Interface

Uses SysUtils, Classes, Math;

Const
  MaxEnvPoints = 8;
  MaxOnsets = 8;

Type
  TFloatBuf = Array Of Double;

  TEnvPoint = Record
    Ms: Integer;
    Db: Double; // relative to the loudest frame (0 = loudest)
  End;

  TFeatures = Record
    Name: String;
    Rate: Integer;
    LenMs: Integer; // whole file
    ActiveMs: Integer; // until the level stays below -40 dB
    Peak: Double; // 0..1
    Rms: Double; // of the active part
    AttackMs: Integer; // first sound to loudest frame
    Env: Array Of TEnvPoint; // <= MaxEnvPoints
    Tonal: Double; // 0 = noise, 1 = clear pitch (energy weighted)
    PitchStart, PitchEnd, PitchMin, PitchMax: Double; // Hz, 0 = no pitch
    BrightStart, BrightEnd, BrightMin, BrightMax: Double; // spectral centroid Hz
    Onsets: Array Of Integer; // ms, <= MaxOnsets
    OnsetCount: Integer; // all detected onsets
  End;

Function LoadWav(Const FileName: String; Out Sig: TFloatBuf; Out Rate: Integer): Boolean;
Function Analyze(Const Name: String; Const Sig: TFloatBuf; Rate: Integer): TFeatures;
Function FeatureLine(Const F: TFeatures): String; // one comment line
Function Spectrum(Const Sig: TFloatBuf; Center, Len: Integer): TFloatBuf; // magnitudes 0..Len/2, Hann window

Implementation

Type
  TComplex = Record
    Re, Im: Double;
  End;
  TComplexBuf = Array Of TComplex;

Function LoadWav(Const FileName: String; Out Sig: TFloatBuf; Out Rate: Integer): Boolean;
Var
  f: TFileStream;
  id: Array[0..3] Of Char;
  size, chunk, srate, brate: LongWord;
  fmt, ch, bits, align: Word;
  data: Array Of Byte;
  i, c, n: Integer;
  v: Double;
Begin
  result := false;
  Sig := Nil;
  Rate := 22050;
  ch := 1;
  bits := 16;
  f := TFileStream.Create(FileName, fmOpenRead Or fmShareDenyWrite);
  Try
    f.ReadBuffer(id, 4);
    If id <> 'RIFF' Then exit;
    f.ReadBuffer(size, 4);
    f.ReadBuffer(id, 4);
    If id <> 'WAVE' Then exit;
    While f.Position <= f.Size - 8 Do Begin
      f.ReadBuffer(id, 4);
      f.ReadBuffer(chunk, 4);
      If id = 'fmt ' Then Begin
        f.ReadBuffer(fmt, 2);
        f.ReadBuffer(ch, 2);
        f.ReadBuffer(srate, 4);
        f.ReadBuffer(brate, 4);
        f.ReadBuffer(align, 2);
        f.ReadBuffer(bits, 2);
        f.Seek(chunk - 16, soCurrent);
        If (fmt <> 1) Or Not (bits In [8, 16]) Then exit;
        Rate := srate;
      End
      Else If id = 'data' Then Begin
        data := Nil;
        chunk := Min(chunk, f.Size - f.Position);
        setlength(data, chunk);
        If chunk > 0 Then f.ReadBuffer(data[0], chunk);
        n := chunk Div (ch * (bits Div 8));
        setlength(Sig, n);
        For i := 0 To n - 1 Do Begin
          v := 0;
          For c := 0 To ch - 1 Do
            If bits = 16 Then
              v := v + SmallInt(data[(i * ch + c) * 2] Or (data[(i * ch + c) * 2 + 1] Shl 8)) / 32768
            Else
              v := v + (data[i * ch + c] - 128) / 128;
          Sig[i] := v / ch;
        End;
        exit(true);
      End
      Else
        f.Seek(chunk + (chunk And 1), soCurrent);
    End;
  Finally
    f.Free;
  End;
End;

Procedure FFT(Var X: TComplexBuf);
Var
  n, i, j, k, m, step: Integer;
  t: TComplex;
  wre, wim, ure, uim, ang: Double;
Begin
  n := length(X);
  j := 0;
  For i := 0 To n - 2 Do Begin
    If i < j Then Begin
      t := X[i];
      X[i] := X[j];
      X[j] := t;
    End;
    k := n Shr 1;
    While k <= j Do Begin
      j := j - k;
      k := k Shr 1;
    End;
    j := j + k;
  End;
  step := 1;
  While step < n Do Begin
    ang := -Pi / step;
    For m := 0 To step - 1 Do Begin
      wre := Cos(ang * m);
      wim := Sin(ang * m);
      i := m;
      While i < n Do Begin
        j := i + step;
        ure := X[j].Re * wre - X[j].Im * wim;
        uim := X[j].Re * wim + X[j].Im * wre;
        X[j].Re := X[i].Re - ure;
        X[j].Im := X[i].Im - uim;
        X[i].Re := X[i].Re + ure;
        X[i].Im := X[i].Im + uim;
        i := i + 2 * step;
      End;
    End;
    step := step * 2;
  End;
End;

Function Spectrum(Const Sig: TFloatBuf; Center, Len: Integer): TFloatBuf;
Var
  x: TComplexBuf;
  i, p: Integer;
Begin
  x := Nil;
  setlength(x, Len);
  For i := 0 To Len - 1 Do Begin
    p := Center - Len Div 2 + i;
    If (p >= 0) And (p <= high(Sig)) Then
      x[i].Re := Sig[p] * (0.5 - 0.5 * Cos(2 * Pi * i / (Len - 1)))
    Else
      x[i].Re := 0;
    x[i].Im := 0;
  End;
  FFT(x);
  result := Nil;
  setlength(result, Len Div 2 + 1);
  For i := 0 To Len Div 2 Do
    result[i] := Sqrt(Sqr(x[i].Re) + Sqr(x[i].Im));
End;

Function ToDb(v: Double): Double;
Begin
  result := 20 * Log10(Max(v, 1E-9));
End;

(* normalized autocorrelation pitch, 0 if unclear *)
Function Pitch(Const Sig: TFloatBuf; Start, Len, Rate: Integer; Out Clarity: Double): Double;
Var
  lag, minLag, maxLag, best, i: Integer;
  r, r0, r1, bestR: Double;
  rs: TFloatBuf;
Begin
  result := 0;
  Clarity := 0;
  minLag := Rate Div 1000;
  maxLag := Min(Rate Div 60, Len Div 2);
  If (Start < 0) Or (Start + Len > length(Sig)) Or (maxLag <= minLag) Then exit;
  rs := Nil;
  setlength(rs, maxLag + 2);
  r0 := 0;
  For i := Start To Start + Len - 1 Do
    r0 := r0 + Sqr(Sig[i]);
  If r0 < 1E-6 Then exit;
  For lag := minLag - 1 To maxLag + 1 Do Begin
    r := 0;
    r1 := 0;
    For i := Start To Start + Len - 1 - lag Do Begin
      r := r + Sig[i] * Sig[i + lag];
      r1 := r1 + Sqr(Sig[i + lag]);
    End;
    rs[Min(lag, maxLag + 1)] := r / Sqrt(Max(1E-12, r0 * r1));
  End;
  // first strong peak (avoids octave errors towards low pitches)
  bestR := 0;
  best := 0;
  For lag := minLag To maxLag Do
    If (rs[lag] > rs[lag - 1]) And (rs[lag] >= rs[lag + 1]) And (rs[lag] > bestR) Then Begin
      bestR := rs[lag];
      best := lag;
    End;
  For lag := minLag To maxLag Do
    If (rs[lag] > rs[lag - 1]) And (rs[lag] >= rs[lag + 1]) And (rs[lag] > 0.9 * bestR) Then Begin
      best := lag;
      bestR := rs[lag];
      break;
    End;
  If best > 0 Then Begin
    Clarity := bestR;
    result := Rate / best;
  End;
End;

Function Median(a: TFloatBuf): Double;
Var
  i, j: Integer;
  t: Double;
Begin
  If length(a) = 0 Then exit(0);
  For i := 1 To high(a) Do Begin
    t := a[i];
    j := i - 1;
    While (j >= 0) And (a[j] > t) Do Begin
      a[j + 1] := a[j];
      dec(j);
    End;
    a[j + 1] := t;
  End;
  result := a[length(a) Div 2];
End;

Procedure Push(Var a: TFloatBuf; v: Double);
Begin
  setlength(a, length(a) + 1);
  a[high(a)] := v;
End;

(* Douglas-Peucker on the dB envelope, increase the tolerance until <= MaxEnvPoints *)
Procedure Simplify(Const Db: TFloatBuf; FrameMs: Integer; Var F: TFeatures);
Var
  keep: Array Of Boolean;
  tol: Double;
  cnt, i: Integer;

  Procedure Rec(a, b: Integer);
  Var
    i, best: Integer;
    d, bestD, lin: Double;
  Begin
    If b - a < 2 Then exit;
    best := -1;
    bestD := tol;
    For i := a + 1 To b - 1 Do Begin
      lin := Db[a] + (Db[b] - Db[a]) * (i - a) / (b - a);
      d := Abs(Db[i] - lin);
      If d > bestD Then Begin
        bestD := d;
        best := i;
      End;
    End;
    If best < 0 Then exit;
    keep[best] := true;
    Rec(a, best);
    Rec(best, b);
  End;

Begin
  keep := Nil;
  setlength(keep, length(Db));
  tol := 3;
  Repeat
    For i := 0 To high(keep) Do
      keep[i] := (i = 0) Or (i = high(keep));
    Rec(0, high(Db));
    cnt := 0;
    For i := 0 To high(keep) Do
      If keep[i] Then inc(cnt);
    tol := tol * 1.3;
  Until cnt <= MaxEnvPoints;
  F.Env := Nil;
  For i := 0 To high(keep) Do
    If keep[i] Then Begin
      setlength(F.Env, length(F.Env) + 1);
      F.Env[high(F.Env)].Ms := i * FrameMs;
      F.Env[high(F.Env)].Db := Db[i];
    End;
End;

Function Analyze(Const Name: String; Const Sig: TFloatBuf; Rate: Integer): TFeatures;
Const
  FrameMs = 10;
Var
  frame, nFrames, i, j, k, first, last, loud, fftLen, pitchLen: Integer;
  db, rmsF, pitches, starts, ends, cents, cStart, cEnd: TFloatBuf;
  e, maxRms, sum, sumW, tonW, clarity, p, num, den, c, prevMin: Double;
  spec: TFloatBuf;
Begin
  result := Default(TFeatures);
  result.Name := Name;
  result.Rate := Rate;
  result.LenMs := Round(length(Sig) * 1000 / Rate);
  If length(Sig) < Rate Div 50 Then exit;
  frame := Rate * FrameMs Div 1000;
  nFrames := length(Sig) Div frame;
  rmsF := Nil;
  setlength(rmsF, nFrames);
  maxRms := 0;
  loud := 0;
  For i := 0 To nFrames - 1 Do Begin
    e := 0;
    For j := i * frame To (i + 1) * frame - 1 Do
      e := e + Sqr(Sig[j]);
    rmsF[i] := Sqrt(e / frame);
    If rmsF[i] > maxRms Then Begin
      maxRms := rmsF[i];
      loud := i;
    End;
  End;
  For i := 0 To high(Sig) Do
    result.Peak := Max(result.Peak, Abs(Sig[i]));
  If maxRms <= 0 Then exit;
  db := Nil;
  setlength(db, nFrames);
  For i := 0 To nFrames - 1 Do
    db[i] := Max(-60, ToDb(rmsF[i] / maxRms));
  first := 0;
  While (first < nFrames - 1) And (db[first] < -30) Do inc(first);
  last := nFrames - 1;
  While (last > first) And (db[last] < -40) Do dec(last);
  result.ActiveMs := (last + 1) * FrameMs;
  result.AttackMs := (loud - first) * FrameMs;
  sum := 0;
  For i := 0 To last Do
    sum := sum + Sqr(rmsF[i]);
  result.Rms := Sqrt(sum / (last + 1));
  // envelope: smooth over 3 frames, then simplify
  setlength(db, last + 1);
  For i := 1 To high(db) - 1 Do
    db[i] := (db[i - 1] + 2 * db[i] + db[i + 1]) / 4;
  Simplify(db, FrameMs, result);
  // onsets: rise of more than 6 dB over the minimum of the last 40 ms, loud enough
  result.Onsets := Nil;
  result.OnsetCount := 0;
  i := 0;
  While i <= last Do Begin
    prevMin := 0;
    For j := Max(0, i - 4) To i - 1 Do
      If (j = Max(0, i - 4)) Or (ToDb(rmsF[j] / maxRms) < prevMin) Then prevMin := ToDb(rmsF[j] / maxRms);
    If i = 0 Then prevMin := -60;
    If (ToDb(rmsF[i] / maxRms) > -24) And (ToDb(rmsF[i] / maxRms) - prevMin > 6) Then Begin
      inc(result.OnsetCount);
      If length(result.Onsets) < MaxOnsets Then Begin
        setlength(result.Onsets, length(result.Onsets) + 1);
        result.Onsets[high(result.Onsets)] := i * FrameMs;
      End;
      i := i + 6; // 60 ms dead time
    End
    Else
      inc(i);
  End;
  // pitch and brightness on 40 ms / 1024 sample windows of the loud part
  pitchLen := Rate * 40 Div 1000;
  fftLen := 1024;
  pitches := Nil;
  starts := Nil;
  ends := Nil;
  cents := Nil;
  cStart := Nil;
  cEnd := Nil;
  sumW := 0;
  tonW := 0;
  i := first * frame;
  While i + pitchLen < (last + 1) * frame Do Begin
    k := i Div frame;
    If (k < nFrames) And (ToDb(rmsF[k] / maxRms) > -25) Then Begin
      p := Pitch(Sig, i, pitchLen, Rate, clarity);
      sumW := sumW + rmsF[k];
      If (p > 0) And (clarity > 0.75) Then Begin
        tonW := tonW + rmsF[k];
        Push(pitches, p);
        If i < first * frame + (last - first + 1) * frame Div 4 Then Push(starts, p);
        If i > (last + 1) * frame - (last - first + 1) * frame Div 4 Then Push(ends, p);
      End;
      spec := Spectrum(Sig, i + pitchLen Div 2, fftLen);
      num := 0;
      den := 0;
      For j := 1 To high(spec) Do Begin
        num := num + j * Rate / fftLen * spec[j];
        den := den + spec[j];
      End;
      If den > 0 Then Begin
        c := num / den;
        Push(cents, c);
        If i < first * frame + (last - first + 1) * frame Div 4 Then Push(cStart, c);
        If i > (last + 1) * frame - (last - first + 1) * frame Div 3 Then Push(cEnd, c);
      End;
    End;
    i := i + pitchLen Div 2;
  End;
  If sumW > 0 Then result.Tonal := tonW / sumW;
  If length(pitches) > 0 Then Begin
    result.PitchMin := MinValue(pitches);
    result.PitchMax := MaxValue(pitches);
    If length(starts) = 0 Then Push(starts, pitches[0]);
    If length(ends) = 0 Then Push(ends, pitches[high(pitches)]);
    result.PitchStart := Median(starts);
    result.PitchEnd := Median(ends);
  End;
  If length(cents) > 0 Then Begin
    result.BrightMin := MinValue(cents);
    result.BrightMax := MaxValue(cents);
    If length(cStart) = 0 Then Push(cStart, cents[0]);
    If length(cEnd) = 0 Then Push(cEnd, cents[high(cents)]);
    result.BrightStart := Median(cStart);
    result.BrightEnd := Median(cEnd);
  End;
End;

Function FeatureLine(Const F: TFeatures): String;
Var
  i: Integer;
  fs: TFormatSettings;
Begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  result := Format('len=%d active=%d peak=%.2f rms=%.3f attack=%d tonal=%.2f',
    [F.LenMs, F.ActiveMs, F.Peak, F.Rms, F.AttackMs, F.Tonal], fs);
  If F.PitchStart > 0 Then
    result := result + Format(' pitch=%.0f..%.0f (%.0f-%.0f)', [F.PitchStart, F.PitchEnd, F.PitchMin, F.PitchMax], fs);
  result := result + Format(' bright=%.0f..%.0f (%.0f-%.0f)', [F.BrightStart, F.BrightEnd, F.BrightMin, F.BrightMax], fs);
  result := result + ' env=';
  For i := 0 To high(F.Env) Do Begin
    If i > 0 Then result := result + ',';
    result := result + Format('%d:%.0f', [F.Env[i].Ms, F.Env[i].Db], fs);
  End;
  result := result + Format(' onsets=%d', [F.OnsetCount]);
  If length(F.Onsets) > 1 Then Begin
    result := result + ' (';
    For i := 0 To high(F.Onsets) Do Begin
      If i > 0 Then result := result + ',';
      result := result + IntToStr(F.Onsets[i]);
    End;
    result := result + ')';
  End;
End;

End.
