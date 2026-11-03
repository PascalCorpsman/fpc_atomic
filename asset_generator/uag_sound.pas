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
 * Small synthesizer for the sound effects of a generated set.
 *
 * A sound is a list of parts (one text line each) that are mixed together.
 * The text format is the one of fpcTesttone (testtoene.txt), so definitions
 * can be played with that tool too:
 *
 *   [name]
 *   ; comment
 *   wave t=.. len=.. f=.. vol=.. attack=.. curve=.. duty=.. vib=.. rep=.. ring=.. f1=.. f2=.. f3=.. bw=..
 *
 *   wave     square | triangle | saw | sine | noise | voice
 *   t, len   start and duration in ms
 *   f        frequency in Hz, "a..b" glides exponentially; noise: new random
 *            values per second (sample and hold, low values sound dull);
 *            voice: pitch of the voice
 *   vol      volume 0..1 (voice is normalized to this peak)
 *   attack   fade in, ms
 *   curve    decay (1-t)^curve over len, 0 = constant with 5 ms fade out
 *   duty     pulse width of square
 *   vib      vibrato "hz,depth" (depth relative, 0.03 = 3 %)
 *   rep      repeat "count,distance_ms"
 *   ring     ring modulation in Hz (robot sound), 0 = off
 *   f1 f2 f3 formants of voice in Hz, "a..b" glides linearly
 *   bw       bandwidth of the formants in Hz (default 80)
 *   lp       low pass in Hz (2 poles), "a..b" glides exponentially, 0 = off.
 *            Extension of uag_sound: fpcTesttone ignores it (plays brighter).
 *            Typical: noise f=22050 lp=300 is a dark rumble.
 *
 * Differences to fpcTesttone: 22050 Hz (the rate of the game sounds), Double,
 * deterministic math (uag_math) and seeded noise (TAGRandom), so the same
 * seed gives bit identical WAVs on every platform. Levels above 0.6 are
 * compressed by a soft limiter instead of being clipped (loud explosions).
 *)
Unit uag_sound;

{$MODE ObjFPC}{$H+}
{$Q-}{$R-}

Interface

Uses Classes, SysUtils;

Const
  AG_SOUND_RATE = 22050;

Type

  TAGWave = (awSquare, awTriangle, awSaw, awSine, awNoise, awVoice);

  TAGSoundPart = Record
    Wave: TAGWave;
    T, Len: Double; // ms
    F0, F1: Double;
    Vol, Attack, Curve, Duty: Double;
    VibHz, VibDepth: Double;
    Rep: Integer;
    Gap: Double;
    Ring: Double;
    Fm: Array[1..3, 0..1] Of Double; // formant n: start, end
    Bw: Double;
    Lp0, Lp1: Double; // low pass cutoff start, end (0 = off)
  End;

  TAGSoundParts = Array Of TAGSoundPart;
  TAGSamples = Array Of Double;
  TAGPcm = Array Of SmallInt;

  (*
   * Small per set deviation of a sound, neutral = Pitch 1, Decay 1.
   * Pitch multiplies all frequencies (not the formants, the voice keeps its
   * vowels), Decay > 1 makes the decays longer (curve / Decay).
   *)
  TAGSoundVariation = Record
    Pitch: Double;
    Decay: Double;
    Seed: QWord; // noise
  End;

  { TAGSoundBook: all sections of a definition text }

  TAGSoundBook = Class
  private
    fNames: TStringList;
    fParts: Array Of TAGSoundParts;
    fErrors: TStringList;
    Function GetCount: Integer;
    Function GetName(Index: Integer): String;
    Function GetParts(Index: Integer): TAGSoundParts;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure LoadFromLines(Const Lines: Array Of String);
    Procedure LoadFromStrings(Lines: TStrings);
    Function IndexOf(Const Name: String): Integer; // -1 if unknown
    Property Count: Integer read GetCount;
    Property Names[Index: Integer]: String read GetName;
    Property Parts[Index: Integer]: TAGSoundParts read GetParts;
    Property Errors: TStringList read fErrors; // lines that were not understood
  End;

Function AGNeutralVariation(Seed: QWord = 12345): TAGSoundVariation;
Function AGParseSoundPart(Const Line: String; Out P: TAGSoundPart): Boolean;
Function AGSoundLength(Const Parts: TAGSoundParts): Integer; // samples
Procedure AGSynthSound(Const Parts: TAGSoundParts; Const Variation: TAGSoundVariation; Out Buf: TAGSamples);
Function AGSamplesToPcm(Const Buf: TAGSamples): TAGPcm; // soft limiter above 0.6, never clips
Function AGPcmToWav(Const Pcm: TAGPcm): TBytes; // 16 bit mono AG_SOUND_RATE
Function AGRenderWav(Const Parts: TAGSoundParts; Const Variation: TAGSoundVariation): TBytes;

Implementation

Uses uag_math, uag_random;

Const
  ReleaseMs = 5;
  FormantGain: Array[1..3] Of Double = (1.0, 0.6, 0.35);

Function ParseNum(Const s: String; Out v: Double): Boolean;
Var
  fs: TFormatSettings;
Begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;
  result := TryStrToFloat(Trim(s), v, fs);
End;

Function ParseRange(Const s: String; Out a, b: Double): Boolean;
Var
  p: Integer;
Begin
  p := Pos('..', s);
  If p > 0 Then
    result := ParseNum(Copy(s, 1, p - 1), a) And ParseNum(Copy(s, p + 2, MaxInt), b)
  Else Begin
    result := ParseNum(s, a);
    b := a;
  End;
End;

Function AGNeutralVariation(Seed: QWord): TAGSoundVariation;
Begin
  result.Pitch := 1;
  result.Decay := 1;
  result.Seed := Seed;
End;

Function AGParseSoundPart(Const Line: String; Out P: TAGSoundPart): Boolean;
Var
  words: TStringList;
  i, k: Integer;
  key, val: String;
  a, b: Double;
Begin
  result := false;
  P := Default(TAGSoundPart);
  P.Len := 100;
  P.F0 := 440;
  P.F1 := 440;
  P.Vol := 0.2;
  P.Attack := 1;
  P.Curve := 1;
  P.Duty := 0.5;
  P.Rep := 1;
  P.Bw := 80;
  // neutral vowel (schwa) if no formants are given
  P.Fm[1, 0] := 500;
  P.Fm[1, 1] := 500;
  P.Fm[2, 0] := 1500;
  P.Fm[2, 1] := 1500;
  P.Fm[3, 0] := 2500;
  P.Fm[3, 1] := 2500;
  words := TStringList.Create;
  Try
    ExtractStrings([' ', #9], [], PChar(Line), words);
    If words.Count = 0 Then exit;
    Case LowerCase(words[0]) Of
      'square': P.Wave := awSquare;
      'triangle': P.Wave := awTriangle;
      'saw': P.Wave := awSaw;
      'sine': P.Wave := awSine;
      'noise': P.Wave := awNoise;
      'voice': P.Wave := awVoice;
    Else
      exit;
    End;
    For i := 1 To words.Count - 1 Do Begin
      k := Pos('=', words[i]);
      If k = 0 Then Continue;
      key := LowerCase(Copy(words[i], 1, k - 1));
      val := Copy(words[i], k + 1, MaxInt);
      If (key = 'vib') Or (key = 'rep') Then
        val := StringReplace(val, ',', '..', []);
      If Not ParseRange(val, a, b) Then Continue;
      Case key Of
        't': P.T := a;
        'len': P.Len := a;
        'f': Begin
            P.F0 := a;
            P.F1 := b;
          End;
        'vol': P.Vol := a;
        'attack': P.Attack := a;
        'curve': P.Curve := a;
        'duty': P.Duty := a;
        'vib': Begin
            P.VibHz := a;
            P.VibDepth := b;
          End;
        'rep': Begin
            P.Rep := AGRound(AGMax(1, a));
            P.Gap := b;
          End;
        'ring': P.Ring := a;
        'f1': Begin
            P.Fm[1, 0] := a;
            P.Fm[1, 1] := b;
          End;
        'f2': Begin
            P.Fm[2, 0] := a;
            P.Fm[2, 1] := b;
          End;
        'f3': Begin
            P.Fm[3, 0] := a;
            P.Fm[3, 1] := b;
          End;
        'bw': P.Bw := a;
        'lp': Begin
            P.Lp0 := a;
            P.Lp1 := b;
          End;
      End;
    End;
    result := P.Len > 0;
  Finally
    words.Free;
  End;
End;

{ TAGSoundBook }

Constructor TAGSoundBook.Create;
Begin
  Inherited Create;
  fNames := TStringList.Create;
  fErrors := TStringList.Create;
End;

Destructor TAGSoundBook.Destroy;
Begin
  fNames.Free;
  fErrors.Free;
  Inherited Destroy;
End;

Function TAGSoundBook.GetCount: Integer;
Begin
  result := fNames.Count;
End;

Function TAGSoundBook.GetName(Index: Integer): String;
Begin
  result := fNames[Index];
End;

Function TAGSoundBook.GetParts(Index: Integer): TAGSoundParts;
Begin
  result := fParts[Index];
End;

Function TAGSoundBook.IndexOf(Const Name: String): Integer;
Var
  i: Integer;
Begin
  For i := 0 To fNames.Count - 1 Do
    If SameText(fNames[i], Name) Then exit(i);
  result := -1;
End;

Procedure TAGSoundBook.LoadFromLines(Const Lines: Array Of String);
Var
  i, cur: Integer;
  line: String;
  p: TAGSoundPart;
Begin
  fNames.Clear;
  fErrors.Clear;
  fParts := Nil;
  cur := -1;
  For i := 0 To high(Lines) Do Begin
    line := Lines[i];
    If Pos(';', line) > 0 Then line := Copy(line, 1, Pos(';', line) - 1);
    line := Trim(line);
    If line = '' Then Continue;
    If (line[1] = '[') And (line[length(line)] = ']') Then Begin
      cur := fNames.Add(Copy(line, 2, length(line) - 2));
      setlength(fParts, cur + 1);
      fParts[cur] := Nil;
    End
    Else If (cur >= 0) And AGParseSoundPart(line, p) Then Begin
      setlength(fParts[cur], length(fParts[cur]) + 1);
      fParts[cur][high(fParts[cur])] := p;
    End
    Else
      fErrors.Add(IntToStr(i + 1) + ': ' + line);
  End;
End;

Procedure TAGSoundBook.LoadFromStrings(Lines: TStrings);
Var
  a: Array Of String;
  i: Integer;
Begin
  a := Nil;
  setlength(a, Lines.Count);
  For i := 0 To Lines.Count - 1 Do
    a[i] := Lines[i];
  LoadFromLines(a);
End;

{ Synthesis }

Type
  TResonator = Record
    Y1, Y2: Double;
  End;

  (*
   * Two pole resonator (Klatt): emphasizes frequencies around F with
   * bandwidth Bw, unity gain at 0 Hz.
   *)
Function Resonate(Var R: TResonator; X, F, Bw: Double): Double;
Var
  rr, a, b, c: Double;
Begin
  rr := AGExp(-AG_PI * Bw / AG_SOUND_RATE);
  c := -rr * rr;
  b := 2 * rr * AGCos(AG_TWO_PI * F / AG_SOUND_RATE);
  a := 1 - b - c;
  result := a * X + b * R.Y1 + c * R.Y2;
  R.Y2 := R.Y1;
  R.Y1 := result;
End;

Function Fract(x: Double): Double;
Begin
  result := x - AGFloor(x);
End;

(*
 * Raw signal of one part (without envelope). Frequencies glide exponentially
 * (incrementally, the factor per sample is computed once).
 *)
Procedure Oscillate(Const P: TAGSoundPart; N: Integer; Pitch: Double; Var Rnd: TAGRandom; Var Raw: TAGSamples);
Var
  s, k: Integer;
  tt, f, fStart, fEnd, step, phase, v, noise, src, peak, ringStep, lp, lpStep, lpA, y1, y2: Double;
  lastStep: Int64;
  expo: Boolean;
  res: Array[1..3] Of TResonator;
Begin
  setlength(Raw, N);
  phase := 0;
  lastStep := -1;
  noise := 0;
  For k := 1 To 3 Do Begin
    res[k].Y1 := 0;
    res[k].Y2 := 0;
  End;
  fStart := P.F0 * Pitch;
  fEnd := P.F1 * Pitch;
  expo := (fStart > 0) And (fEnd > 0) And (fStart <> fEnd);
  If expo Then
    step := AGPow(fEnd / fStart, 1 / N)
  Else
    step := 1;
  f := fStart;
  ringStep := P.Ring / AG_SOUND_RATE;
  lp := 0;
  lpStep := 1;
  If (P.Lp0 > 0) And (P.Lp1 > 0) Then Begin
    lp := P.Lp0 * Pitch;
    lpStep := AGPow(P.Lp1 / P.Lp0, 1 / N);
  End;
  y1 := 0;
  y2 := 0;
  For s := 0 To N - 1 Do Begin
    tt := s / N;
    If expo Then Begin
      If s > 0 Then f := f * step;
    End
    Else
      f := fStart + (fEnd - fStart) * tt;
    v := f;
    If P.VibHz > 0 Then
      v := f * (1 + P.VibDepth * AGSin(AG_TWO_PI * Fract(P.VibHz * s / AG_SOUND_RATE)));
    phase := phase + v / AG_SOUND_RATE;
    Case P.Wave Of
      awSquare: If Fract(phase) < P.Duty Then v := 1 Else v := -1;
      awTriangle: v := 4 * AGAbs(Fract(phase) - 0.5) - 1;
      awSaw: v := 2 * Fract(phase) - 1;
      awSine: v := AGSin(AG_TWO_PI * Fract(phase));
      awVoice: Begin
          // saw as buzzing vocal cords, then through the three formants
          src := 2 * Fract(phase) - 1;
          v := 0;
          For k := 1 To 3 Do
            v := v + FormantGain[k] * Resonate(res[k], src,
              P.Fm[k, 0] + (P.Fm[k, 1] - P.Fm[k, 0]) * tt, P.Bw * k);
        End;
    Else Begin // noise
        If AGFloor(phase) <> lastStep Then Begin
          lastStep := AGFloor(phase);
          noise := 2 * Rnd.NextUnit - 1;
        End;
        v := noise;
      End;
    End;
    If P.Ring > 0 Then
      v := v * AGSin(AG_TWO_PI * Fract(ringStep * s));
    If lp > 0 Then Begin
      // two one pole low passes in a row
      lpA := 1 - AGExp(-AG_TWO_PI * AGMin(lp, 0.45 * AG_SOUND_RATE) / AG_SOUND_RATE);
      y1 := y1 + lpA * (v - y1);
      y2 := y2 + lpA * (y1 - y2);
      v := y2;
      lp := lp * lpStep;
    End;
    Raw[s] := v;
  End;
  // the filters of the voice have no fixed gain: normalize to peak 1
  If P.Wave = awVoice Then Begin
    peak := 0;
    For s := 0 To N - 1 Do
      peak := AGMax(peak, AGAbs(Raw[s]));
    If peak > 0 Then
      For s := 0 To N - 1 Do
        Raw[s] := Raw[s] / peak;
  End;
End;

Function MsToSamples(ms: Double): Integer;
Begin
  result := AGRound(ms * AG_SOUND_RATE / 1000);
End;

Function AGSoundLength(Const Parts: TAGSoundParts): Integer;
Var
  i: Integer;
Begin
  result := 0;
  For i := 0 To high(Parts) Do
    With Parts[i] Do
      result := AGClampI(MsToSamples(T + (Rep - 1) * Gap + Len), result, MaxInt);
End;

Procedure AGSynthSound(Const Parts: TAGSoundParts; Const Variation: TAGSoundVariation; Out Buf: TAGSamples);
Var
  i, k, n, start, total, s, release, attackN: Integer;
  tt, env, cv, decay: Double;
  raw: TAGSamples;
  rnd: TAGRandom;
Begin
  total := AGSoundLength(Parts);
  Buf := Nil;
  setlength(Buf, total + 1);
  For s := 0 To high(Buf) Do
    Buf[s] := 0;
  rnd.Init(Variation.Seed, 77);
  raw := Nil;
  decay := Variation.Decay;
  If decay <= 0 Then decay := 1;
  release := MsToSamples(ReleaseMs);
  For i := 0 To high(Parts) Do
    With Parts[i] Do Begin
      cv := Curve / decay;
      For k := 0 To Rep - 1 Do Begin
        start := MsToSamples(T + k * Gap);
        n := MsToSamples(Len);
        If n <= 0 Then Continue;
        attackN := MsToSamples(Attack);
        Oscillate(Parts[i], n, Variation.Pitch, rnd, raw);
        For s := 0 To n - 1 Do Begin
          tt := s / n;
          env := 1;
          If (attackN > 0) And (s < attackN) Then
            env := s / attackN;
          If cv > 0 Then
            env := env * AGPow(1 - tt, cv)
          Else If s > n - release Then
            env := env * (n - s) / release;
          If start + s <= high(Buf) Then
            Buf[start + s] := Buf[start + s] + Vol * env * raw[s];
        End;
      End;
    End;
End;

(* identity below Knee, above it approaches 0.98 smoothly (tanh) *)
Function SoftLimit(x: Double): Double;
Const
  Knee = 0.6;
  Room = 0.98 - Knee;
Var
  a, e: Double;
Begin
  a := AGAbs(x);
  If a <= Knee Then exit(x);
  e := AGExp(-2 * (a - Knee) / Room);
  a := Knee + Room * (1 - e) / (1 + e);
  If x < 0 Then result := -a Else result := a;
End;

Function AGSamplesToPcm(Const Buf: TAGSamples): TAGPcm;
Var
  i: Integer;
Begin
  result := Nil;
  setlength(result, length(Buf));
  For i := 0 To high(Buf) Do
    result[i] := AGClampI(AGRound(SoftLimit(Buf[i]) * 32767), -32767, 32767);
End;

Function AGPcmToWav(Const Pcm: TAGPcm): TBytes;
Var
  pos: Integer;

  Procedure W8(v: Byte);
  Begin
    result[pos] := v;
    inc(pos);
  End;

  Procedure W16(v: Word);
  Begin
    W8(v And $FF);
    W8(v Shr 8);
  End;

  Procedure W32(v: Cardinal);
  Begin
    W16(v And $FFFF);
    W16(v Shr 16);
  End;

  Procedure Tag(Const s: String);
  Var
    i: Integer;
  Begin
    For i := 1 To 4 Do
      W8(ord(s[i]));
  End;

Var
  i: Integer;
Begin
  result := Nil;
  setlength(result, 44 + 2 * length(Pcm));
  pos := 0;
  Tag('RIFF');
  W32(36 + 2 * length(Pcm));
  Tag('WAVE');
  Tag('fmt ');
  W32(16);
  W16(1); // PCM
  W16(1); // mono
  W32(AG_SOUND_RATE);
  W32(AG_SOUND_RATE * 2);
  W16(2);
  W16(16);
  Tag('data');
  W32(2 * length(Pcm));
  For i := 0 To high(Pcm) Do
    W16(Word(Pcm[i]));
End;

Function AGRenderWav(Const Parts: TAGSoundParts; Const Variation: TAGSoundVariation): TBytes;
Var
  buf: TAGSamples;
Begin
  AGSynthSound(Parts, Variation, buf);
  result := AGPcmToWav(AGSamplesToPcm(buf));
End;

End.
