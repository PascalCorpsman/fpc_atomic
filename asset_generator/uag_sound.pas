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
 * seed gives bit identical WAVs on every platform. Levels above 0.75 are
 * compressed by a soft limiter instead of being clipped; the gain staging
 * (HeadroomGain) keeps the level so low that it is rarely active.
 *
 * Sound kits (TAGSoundKit, one per graphics style) change the parts while
 * rendering, sounds.ini stays the same: flat = clean and punchy, soft = warm
 * (darker, softer attacks, lower, short room reverb), pixel = 16 bit console
 * (pulse waves with varying duty, vibrato, short echo), retro = harsh 8 bit
 * (square / triangle only, coarse noise, 8 kHz sample hold, 7 bit). Voice
 * parts keep their wave and get lighter effects, so they stay intelligible.
 *
 * Music (TAGMusicBook, music.ini): a small step sequencer on top of the same
 * synth. A song has instruments (one or more part lines each), patterns
 * (tracker like text, one token per step), parts (patterns played together)
 * and an order. Example:
 *
 *   [song:menu]
 *   bpm=128              ; tempo
 *   beat=4               ; steps per beat (4 = 16th notes)
 *   key=2                ; transpose of all notes in semitones
 *   rms=0.15             ; loudness of the result (RMS; lower if the limiter would
 *                        ; be active in more than 1 % of the time)
 *   loop=1               ; seamless loop: the tail of the end continues at the start
 *   inst lead = square vol=0.12 duty=0.25 attack=4 curve=0.5 gate=0.9 echo=3,0.3
 *   inst kick = sine len=140 f=160..42 vol=0.6 curve=1.4
 *   pat beat  = x . . . x . . . x . . . x . . .
 *   pat tune  = C5 - E5 . G5 - - . | @tune+5
 *   part A    = kick:beat lead:tune lead:tune-12
 *   order     = A A*2
 *
 *   inst     a part line of the instrument (several lines with the same name
 *            are layered). For notes f is a factor of the note frequency
 *            (default 1, "f=2" one octave up) and len is the note length
 *            (steps * step time * gate) unless len= is given. For hits (x)
 *            f is in Hz. gate=0..1 (default 0.9), echo=steps,feedback.
 *   pat      tokens: C4 C#4 Db4 (note, "!" appended = accent), x X o (hit,
 *            accent, soft), . (rest), - (tie, holds the previous note),
 *            | (ignored, bar line), @name+n (an earlier pattern, transposed).
 *            A second "pat" line with the same name continues the pattern.
 *   part     inst:pattern[+-semitones] ...; the part is as long as its longest
 *            pattern, shorter patterns repeat.
 *   order    part names, name*n repeats.
 *
 *   [music:mainmenu_sound]    ; a file res/mainmenu_sound.wav
 *   song=menu
 *   tempo=1  key=0  order=... ; optional changes of the song for this file
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
    Tag: Integer; // identity of the line (sound kits choose per line, not per note)
  End;

  TAGSoundParts = Array Of TAGSoundPart;
  TAGSamples = Array Of Double;
  TAGPcm = Array Of SmallInt;

  TAGSoundKit = (skNone, skFlat, skSoft, skPixel, skRetro);

  (*
   * Per set deviation of a sound, neutral = AGNeutralVariation.
   * Pitch multiplies all frequencies (not the formants, the voice keeps its
   * vowels), Decay > 1 makes the decays longer (curve / Decay), Bright
   * multiplies the low pass cutoffs, NoiseRate the rate of noise parts
   * (colour of the noise). Kit = sound kit of the style.
   *)
  TAGSoundVariation = Record
    Pitch: Double;
    Decay: Double;
    Seed: QWord; // noise
    Kit: TAGSoundKit;
    Bright: Double;
    NoiseRate: Double;
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

  { Music }

  TAGMusicTokenKind = (mtRest, mtTie, mtNote, mtHit);

  TAGMusicToken = Record
    Kind: TAGMusicTokenKind;
    Note: Integer; // MIDI number, C4 = 60
    Vel: Double;
  End;

  TAGMusicInst = Record
    Name: String;
    Parts: TAGSoundParts;
    FixedLen: Array Of Boolean; // len= given: note length is ignored
    Gate: Double;
    EchoSteps, EchoFb: Double;
  End;

  TAGMusicPattern = Record
    Name: String;
    Tokens: Array Of TAGMusicToken;
  End;

  TAGMusicEntry = Record
    Inst, Pat, Transpose: Integer;
  End;

  TAGMusicPart = Record
    Name: String;
    Entries: Array Of TAGMusicEntry;
    Steps: Integer;
  End;

  TAGSong = Record
    Name: String;
    Bpm, Rms: Double;
    Beat, Key: Integer;
    Loop: Boolean;
    Order: String;
    Insts: Array Of TAGMusicInst;
    Pats: Array Of TAGMusicPattern;
    Parts: Array Of TAGMusicPart;
  End;

  (* one music file: res/<Name>.wav = Song with optional changes *)
  TAGMusicFile = Record
    Name, Song, Order: String; // Order '' = the one of the song
    Tempo: Double;
    Key: Integer;
  End;

  (* per set deviation of the music, neutral = AGNeutralMusicVariation *)
  TAGMusicVariation = Record
    Sound: TAGSoundVariation; // Kit, Bright, NoiseRate, Seed (Pitch and Decay are not used)
    Transpose: Integer; // semitones
    Tempo: Double; // factor of bpm
    Duty: Double; // factor of the pulse widths
  End;

  { TAGMusicBook: all sections of music.ini }

  TAGMusicBook = Class
  private
    fSongs: Array Of TAGSong;
    fFiles: Array Of TAGMusicFile;
    fErrors: TStringList;
    Function GetFile(Index: Integer): TAGMusicFile;
    Function GetFileCount: Integer;
    Function GetSong(Index: Integer): TAGSong;
    Function GetSongCount: Integer;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure LoadFromLines(Const Lines: Array Of String);
    Procedure LoadFromStrings(Lines: TStrings);
    Function IndexOfSong(Const Name: String): Integer;
    (* renders file Index (the loop, before the limiter); raises on errors *)
    Procedure Render(Index: Integer; Const Variation: TAGMusicVariation; Out Buf: TAGSamples);
    Function RenderWav(Index: Integer; Const Variation: TAGMusicVariation): TBytes;
    Property SongCount: Integer read GetSongCount;
    Property Songs[Index: Integer]: TAGSong read GetSong;
    Property FileCount: Integer read GetFileCount;
    Property Files[Index: Integer]: TAGMusicFile read GetFile;
    Property Errors: TStringList read fErrors;
  End;

Function AGNeutralVariation(Seed: QWord = 12345): TAGSoundVariation;
Function AGNeutralMusicVariation(Seed: QWord = 12345): TAGMusicVariation;
Function AGParseSoundPart(Const Line: String; Out P: TAGSoundPart): Boolean;
Function AGSoundLength(Const Parts: TAGSoundParts): Integer; // samples
(* parts through the kit, synthesized and kit effects (reverb, echo, crush) *)
Procedure AGSynthSound(Const Parts: TAGSoundParts; Const Variation: TAGSoundVariation; Out Buf: TAGSamples);
Function AGRenderMusic(Const Song: TAGSong; Const OrderText: String; Const Variation: TAGMusicVariation): TAGSamples;
Function AGSamplesToPcm(Const Buf: TAGSamples): TAGPcm; // soft limiter above 0.75, never clips
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
  result.Kit := skNone;
  result.Bright := 1;
  result.NoiseRate := 1;
End;

Function AGNeutralMusicVariation(Seed: QWord): TAGMusicVariation;
Begin
  result.Sound := AGNeutralVariation(Seed);
  result.Transpose := 0;
  result.Tempo := 1;
  result.Duty := 1;
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
      p.Tag := length(fParts[cur]) + 1;
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

Function IsTonal(w: TAGWave): Boolean;
Begin
  result := w In [awSquare, awTriangle, awSaw, awSine];
End;

(*
 * Loudness of a pulse wave (+-1) with this duty relative to a square: 3/4
 * of its AC level in dB (narrow pulses sound thinner, but not that much
 * quieter as their energy says)
 *)
Function PulseLevel(Duty: Double): Double;
Begin
  result := AGPow(AGMax(0.2, 2 * AGSqrt(AGClamp(Duty, 0, 1) * (1 - AGClamp(Duty, 0, 1)))), 0.75);
End;

(*
 * Sound kit and the set variation applied to one part. Voice parts keep wave
 * and filters (intelligible), they only follow the pitch of the kit.
 *)
Procedure KitPart(Var P: TAGSoundPart; Const V: TAGSoundVariation);
Const
  Duties: Array[0..2] Of Double = (0.125, 0.25, 0.5);
Var
  d: Double;
Begin
  If P.Wave = awNoise Then Begin
    P.F0 := P.F0 * V.NoiseRate;
    P.F1 := P.F1 * V.NoiseRate;
  End;
  If (P.Lp0 > 0) And (P.Wave <> awVoice) Then Begin
    P.Lp0 := P.Lp0 * V.Bright;
    P.Lp1 := P.Lp1 * V.Bright;
  End;
  If P.Wave = awVoice Then Begin
    If V.Kit = skSoft Then Begin
      P.F0 := P.F0 * 0.97;
      P.F1 := P.F1 * 0.97;
    End;
    exit;
  End;
  Case V.Kit Of
    skFlat: Begin // clean and punchy: no saw buzz, fast attacks, tight decays
        If P.Wave = awSaw Then Begin
          P.Wave := awSquare;
          P.Duty := 0.5;
        End;
        If P.Wave <> awNoise Then P.Attack := AGMin(P.Attack, 2); // noise keeps its attack (no click)
        If P.Curve > 0 Then P.Curve := P.Curve * 1.25;
        P.VibDepth := P.VibDepth * 0.5;
      End;
    skSoft: Begin // warm: round waves, darker, softer attacks, a bit lower and longer
        If P.Wave = awSquare Then P.Wave := awTriangle;
        If P.Lp0 > 0 Then Begin
          P.Lp0 := P.Lp0 * 0.6;
          P.Lp1 := P.Lp1 * 0.6;
          If P.Wave = awNoise Then P.Vol := P.Vol / AGSqrt(0.6); // noise: power ~ cutoff
        End
        Else Begin
          If P.Wave = awNoise Then d := 3500 Else d := 2600;
          P.Lp0 := d * V.Bright;
          P.Lp1 := P.Lp0;
          // high tones (pickup sparkles) keep their loudness behind the low pass
          If P.Wave <> awNoise Then
            P.Vol := P.Vol / AGMax(0.6, 1 / (1 + AGPow(0.5 * (P.F0 + P.F1) / P.Lp0, 2)));
        End;
        If P.Wave = awNoise Then
          P.Attack := AGMax(P.Attack, 3)
        Else
          P.Attack := AGMax(P.Attack, 6);
        If P.Curve > 0 Then P.Curve := P.Curve * 0.8;
        If P.Wave <> awNoise Then Begin
          P.F0 := P.F0 * 0.95;
          P.F1 := P.F1 * 0.95;
        End;
      End;
    skPixel: Begin // 16 bit console: pulse waves with a duty per line, vibrato, some filtering
        If P.Wave In [awSine, awSaw, awSquare] Then Begin
          // same loudness as the wave it replaces (see retro), narrow pulses have less energy
          Case P.Wave Of
            awSine: d := 0.6;
            awSaw: d := 0.75;
          Else
            d := PulseLevel(P.Duty);
          End;
          P.Wave := awSquare;
          P.Duty := Duties[AGClampI(AGFloor(HashUnitI(V.Seed, 'duty', P.Tag) * 3), 0, 2)];
          P.Vol := P.Vol * d / PulseLevel(P.Duty);
        End;
        If IsTonal(P.Wave) And (P.VibHz = 0) And (P.Len >= 150) Then Begin
          P.VibHz := 6;
          P.VibDepth := 0.015;
        End;
        If P.Lp0 = 0 Then Begin
          If P.Wave = awNoise Then d := 7000 Else d := 6000;
          P.Lp0 := d * V.Bright;
          P.Lp1 := P.Lp0;
        End
        Else Begin
          P.Lp0 := P.Lp0 * 0.85;
          P.Lp1 := P.Lp1 * 0.85;
          If P.Wave = awNoise Then P.Vol := P.Vol / AGSqrt(0.85); // noise: power ~ cutoff
        End;
      End;
    skRetro: Begin // harsh 8 bit: square and triangle only, coarse noise, no filters on tones
        // a square has more energy than the sine (+3 dB) or saw it replaces: same loudness
        If P.Wave = awSine Then Begin
          P.Wave := awSquare;
          P.Duty := 0.5;
          P.Vol := P.Vol * 0.6;
        End
        Else If P.Wave = awSaw Then Begin
          P.Wave := awSquare;
          P.Duty := 0.5;
          P.Vol := P.Vol * 0.75;
        End;
        If P.Wave = awSquare Then Begin
          If P.Duty < 0.19 Then P.Duty := 0.125
          Else If P.Duty < 0.37 Then P.Duty := 0.25
          Else P.Duty := 0.5;
        End;
        If P.Wave = awNoise Then Begin
          P.F0 := AGMin(P.F0, 22050) * 0.35;
          P.F1 := AGMin(P.F1, 22050) * 0.35;
          P.Vol := P.Vol * 0.5; // the coarse noise has more energy below the low pass
          If P.Lp0 > 0 Then Begin
            P.Lp0 := P.Lp0 * 1.5;
            P.Lp1 := P.Lp1 * 1.5;
          End;
        End
        Else Begin
          P.Lp0 := 0;
          P.Lp1 := 0;
        End;
        P.Attack := AGMin(P.Attack, 2);
      End;
  End;
End;

Procedure SynthParts(Const Parts: TAGSoundParts; Const Variation: TAGSoundVariation; Out Buf: TAGSamples);
Var
  i, k, n, start, total, s, release, attackN: Integer;
  tt, env, cv, decay: Double;
  raw: TAGSamples;
  rnd: TAGRandom;
  ps: TAGSoundParts;
Begin
  ps := Nil;
  setlength(ps, length(Parts));
  For i := 0 To high(Parts) Do Begin
    ps[i] := Parts[i];
    KitPart(ps[i], Variation);
  End;
  total := AGSoundLength(ps);
  Buf := Nil;
  setlength(Buf, total + 1);
  For s := 0 To high(Buf) Do
    Buf[s] := 0;
  rnd.Init(Variation.Seed, 77);
  raw := Nil;
  decay := Variation.Decay;
  If decay <= 0 Then decay := 1;
  release := MsToSamples(ReleaseMs);
  For i := 0 To high(ps) Do
    With ps[i] Do Begin
      cv := Curve / decay;
      For k := 0 To Rep - 1 Do Begin
        start := MsToSamples(T + k * Gap);
        n := MsToSamples(Len);
        If n <= 0 Then Continue;
        attackN := MsToSamples(Attack);
        Oscillate(ps[i], n, Variation.Pitch, rnd, raw);
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

{ Effects of the kits. Circular = the buffer is a loop: the state after a
  first pass (end of the loop) is the start of the second pass, which is
  written. Otherwise a tail is appended and trimmed after the silence. }

Type
  TDelayLine = Record
    Buf: Array Of Double;
    Pos: Integer;
  End;

  TFxKind = (fxReverb, fxEcho);

  TFx = Record
    Kind: TFxKind;
    Wet, Fb, Damp: Double;
    Comb: Array[0..3] Of TDelayLine;
    Store: Array[0..3] Of Double;
    Ap: Array[0..1] Of TDelayLine;
    Echo: TDelayLine;
  End;

Procedure InitLine(Var L: TDelayLine; Samples: Integer);
Var
  i: Integer;
Begin
  L.Buf := Nil;
  setlength(L.Buf, AGClampI(Samples, 1, MaxInt));
  For i := 0 To high(L.Buf) Do
    L.Buf[i] := 0;
  L.Pos := 0;
End;

(* small room (structure of Freeverb: 4 combs, 2 allpasses) *)
Function NewReverb(Wet: Double): TFx;
Const
  CombMs: Array[0..3] Of Double = (25.3, 26.9, 29.0, 30.7);
  ApMs: Array[0..1] Of Double = (12.6, 10.0);
Var
  i: Integer;
Begin
  result := Default(TFx);
  result.Kind := fxReverb;
  result.Wet := Wet;
  result.Fb := 0.72;
  result.Damp := 0.4;
  For i := 0 To 3 Do Begin
    InitLine(result.Comb[i], MsToSamples(CombMs[i]));
    result.Store[i] := 0;
  End;
  For i := 0 To 1 Do
    InitLine(result.Ap[i], MsToSamples(ApMs[i]));
End;

(* feedback delay, the first repeat has the level Wet *)
Function NewEcho(DelaySamples: Integer; Wet, Fb: Double): TFx;
Begin
  result := Default(TFx);
  result.Kind := fxEcho;
  result.Wet := Wet;
  result.Fb := Fb;
  InitLine(result.Echo, DelaySamples);
End;

Function FxStep(Var F: TFx; x: Double): Double; // wet signal only
Var
  i: Integer;
  o, b, sum: Double;
Begin
  If F.Kind = fxEcho Then Begin
    o := F.Echo.Buf[F.Echo.Pos];
    F.Echo.Buf[F.Echo.Pos] := x + F.Fb * o;
    F.Echo.Pos := (F.Echo.Pos + 1) Mod length(F.Echo.Buf);
    exit(F.Wet * o);
  End;
  sum := 0;
  For i := 0 To 3 Do
    With F.Comb[i] Do Begin
      o := Buf[Pos];
      F.Store[i] := o * (1 - F.Damp) + F.Store[i] * F.Damp;
      Buf[Pos] := x + F.Store[i] * F.Fb;
      Pos := (Pos + 1) Mod length(Buf);
      sum := sum + o;
    End;
  sum := sum * 0.25;
  For i := 0 To 1 Do
    With F.Ap[i] Do Begin
      b := Buf[Pos];
      o := b - sum;
      Buf[Pos] := sum + b * 0.5;
      Pos := (Pos + 1) Mod length(Buf);
      sum := o;
    End;
  result := F.Wet * sum;
End;

Procedure ApplyFx(Var Buf: TAGSamples; Var F: TFx; Circular: Boolean; TailMs: Double);
Var
  s, n, last: Integer;
Begin
  n := length(Buf);
  If n = 0 Then exit;
  If Circular Then Begin
    For s := 0 To n - 1 Do
      FxStep(F, Buf[s]);
    For s := 0 To n - 1 Do
      Buf[s] := Buf[s] + FxStep(F, Buf[s]);
  End
  Else Begin
    setlength(Buf, n + MsToSamples(TailMs));
    For s := n To high(Buf) Do
      Buf[s] := 0;
    For s := 0 To high(Buf) Do
      Buf[s] := Buf[s] + FxStep(F, Buf[s]);
    last := high(Buf);
    While (last >= n) And (AGAbs(Buf[last]) < 0.0005) Do
      dec(last);
    setlength(Buf, last + 1);
  End;
End;

(*
 * Space of the kit: soft = room reverb, pixel = short echo. Amount 1 = full
 * (sound effects), lower = lighter (voices, music).
 *)
Procedure KitSpace(Var Buf: TAGSamples; Kit: TAGSoundKit; Amount: Double; Circular: Boolean);
Var
  fx: TFx;
Begin
  Case Kit Of
    skSoft: Begin
        fx := NewReverb(0.55 * Amount);
        ApplyFx(Buf, fx, Circular, 600);
      End;
    skPixel: Begin
        fx := NewEcho(MsToSamples(95), 0.3 * Amount, 0.3);
        ApplyFx(Buf, fx, Circular, 450);
      End;
  End;
End;

(* retro: sample and hold at 8 kHz and 7 bit (Amount 1), lighter below *)
Procedure KitCrush(Var Buf: TAGSamples; Kit: TAGSoundKit; Amount: Double);
Var
  s: Integer;
  rate, acc, hold, q: Double;
Begin
  If Kit <> skRetro Then exit;
  rate := (8000 + (1 - Amount) * 4000) / AG_SOUND_RATE;
  q := AGPow(2, AGRound(7 + 2 * (1 - Amount)) - 1);
  acc := 1;
  hold := 0;
  For s := 0 To high(Buf) Do Begin
    If acc >= 1 Then Begin
      acc := acc - AGFloor(acc);
      hold := AGRound(Buf[s] * q) / q;
    End;
    acc := acc + rate;
    Buf[s] := hold;
  End;
End;

(*
 * Gain staging: the soft limiter (SoftLimit) only catches single peaks.
 * HeadroomGain returns the largest gain <= Gain at which at most
 * MaxLimitShare of the 10 ms blocks of Buf reach the knee of the limiter, so
 * it is rarely active (no audible squashing, the retro crush gets a clean
 * signal). The kits keep the loudness of the parts they change (KitPart:
 * squares instead of sines, darker low passes), so all kits sound about
 * equally loud.
 *)
Const
  LimitKnee = 0.75;
  BlockSamples = 220; // 10 ms
  MaxLimitShare = 0.01;

Function HeadroomGain(Const Buf: TAGSamples; Gain: Double): Double;
Var
  peaks: Array Of Double;
  n, i, b, k, allowed: Integer;
  p, t: Double;
Begin
  result := Gain;
  n := (length(Buf) + BlockSamples - 1) Div BlockSamples;
  If n = 0 Then exit;
  peaks := Nil;
  setlength(peaks, n);
  For b := 0 To n - 1 Do
    peaks[b] := 0;
  For i := 0 To high(Buf) Do Begin
    b := i Div BlockSamples;
    If AGAbs(Buf[i]) > peaks[b] Then peaks[b] := AGAbs(Buf[i]);
  End;
  // the (allowed + 1)-th largest block peak must stay below the knee
  allowed := AGFloor(MaxLimitShare * n);
  For k := 0 To allowed Do
    For b := k + 1 To n - 1 Do
      If peaks[b] > peaks[k] Then Begin
        t := peaks[k];
        peaks[k] := peaks[b];
        peaks[b] := t;
      End;
  p := peaks[AGClampI(allowed, 0, n - 1)];
  If p * Gain > 0.99 * LimitKnee Then result := 0.99 * LimitKnee / p;
End;

Procedure ApplyGain(Var Buf: TAGSamples; Gain: Double);
Var
  s: Integer;
Begin
  If Gain = 1 Then exit;
  For s := 0 To high(Buf) Do
    Buf[s] := Buf[s] * Gain;
End;

Function HasVoice(Const Parts: TAGSoundParts): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To high(Parts) Do
    If Parts[i].Wave = awVoice Then exit(true);
  result := false;
End;

Procedure AGSynthSound(Const Parts: TAGSoundParts; Const Variation: TAGSoundVariation; Out Buf: TAGSamples);
Var
  amount: Double;
Begin
  SynthParts(Parts, Variation, Buf);
  If Variation.Kit = skNone Then exit;
  If HasVoice(Parts) Then amount := 0.4 Else amount := 1;
  KitSpace(Buf, Variation.Kit, amount, false);
  ApplyGain(Buf, HeadroomGain(Buf, 1));
  KitCrush(Buf, Variation.Kit, amount);
End;

(* identity below the knee, above it approaches 0.98 smoothly (tanh) *)
Function SoftLimit(x: Double): Double;
Const
  Knee = LimitKnee;
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

{ Music }

Type
  TAGIntArray = Array Of Integer;

Function MidiHz(Note: Integer): Double;
Begin
  result := 440 * AGPow(2, (Note - 69) / 12);
End;

(* value of "key=..." in a part line, '' if missing *)
Function WordValue(Const Line, Key: String): String;
Var
  words: TStringList;
  i: Integer;
Begin
  result := '';
  words := TStringList.Create;
  Try
    ExtractStrings([' ', #9], [], PChar(Line), words);
    For i := 1 To words.Count - 1 Do
      If SameText(Copy(words[i], 1, length(Key) + 1), Key + '=') Then
        result := Copy(words[i], length(Key) + 2, MaxInt);
  Finally
    words.Free;
  End;
End;

Function ParseNote(Const s: String; Out Note: Integer; Out Accent: Boolean): Boolean;
Const
  Semis: Array['A'..'G'] Of Integer = (9, 11, 0, 2, 4, 5, 7);
Var
  i, oct: Integer;
  t: String;
Begin
  result := false;
  t := s;
  Accent := (t <> '') And (t[length(t)] = '!');
  If Accent Then delete(t, length(t), 1);
  If (length(t) < 2) Or Not (t[1] In ['A'..'G']) Then exit;
  Note := Semis[t[1]];
  i := 2;
  If t[i] = '#' Then Begin
    inc(Note);
    inc(i);
  End
  Else If t[i] = 'b' Then Begin
    dec(Note);
    inc(i);
  End;
  If Not TryStrToInt(Copy(t, i, MaxInt), oct) Then exit;
  Note := Note + 12 * (oct + 1);
  result := true;
End;

(* "name", "name+3", "name-12" *)
Procedure SplitTranspose(Const s: String; Out Name: String; Out Transpose: Integer);
Var
  i: Integer;
Begin
  Name := s;
  Transpose := 0;
  For i := 2 To length(s) Do
    If s[i] In ['+', '-'] Then Begin
      Name := Copy(s, 1, i - 1);
      If Not TryStrToInt(Copy(s, i, MaxInt), Transpose) Then Transpose := 0;
      exit;
    End;
End;

Function PatIndex(Const Song: TAGSong; Const Name: String): Integer;
Var
  i: Integer;
Begin
  For i := 0 To high(Song.Pats) Do
    If SameText(Song.Pats[i].Name, Name) Then exit(i);
  result := -1;
End;

Function InstIndex(Const Song: TAGSong; Const Name: String): Integer;
Var
  i: Integer;
Begin
  For i := 0 To high(Song.Insts) Do
    If SameText(Song.Insts[i].Name, Name) Then exit(i);
  result := -1;
End;

Function PartIndex(Const Song: TAGSong; Const Name: String): Integer;
Var
  i: Integer;
Begin
  For i := 0 To high(Song.Parts) Do
    If SameText(Song.Parts[i].Name, Name) Then exit(i);
  result := -1;
End;

(* order text -> part indices, '' if ok, else the error *)
Function ResolveOrder(Const Song: TAGSong; Const OrderText: String; Out Order: TAGIntArray): String;
Var
  words: TStringList;
  i, k, n, p: Integer;
  name: String;
Begin
  result := '';
  Order := Nil;
  words := TStringList.Create;
  Try
    ExtractStrings([' ', #9], [], PChar(OrderText), words);
    For i := 0 To words.Count - 1 Do Begin
      name := words[i];
      n := 1;
      k := Pos('*', name);
      If k > 0 Then Begin
        n := StrToIntDef(Copy(name, k + 1, MaxInt), 0);
        name := Copy(name, 1, k - 1);
      End;
      p := PartIndex(Song, name);
      If (p < 0) Or (n < 1) Then exit('unknown part in order: ' + words[i]);
      For k := 1 To n Do Begin
        setlength(Order, length(Order) + 1);
        Order[high(Order)] := p;
      End;
    End;
    If length(Order) = 0 Then result := 'empty order';
  Finally
    words.Free;
  End;
End;

{ TAGMusicBook }

Constructor TAGMusicBook.Create;
Begin
  Inherited Create;
  fErrors := TStringList.Create;
End;

Destructor TAGMusicBook.Destroy;
Begin
  fErrors.Free;
  Inherited Destroy;
End;

Function TAGMusicBook.GetFile(Index: Integer): TAGMusicFile;
Begin
  result := fFiles[Index];
End;

Function TAGMusicBook.GetFileCount: Integer;
Begin
  result := length(fFiles);
End;

Function TAGMusicBook.GetSong(Index: Integer): TAGSong;
Begin
  result := fSongs[Index];
End;

Function TAGMusicBook.GetSongCount: Integer;
Begin
  result := length(fSongs);
End;

Function TAGMusicBook.IndexOfSong(Const Name: String): Integer;
Var
  i: Integer;
Begin
  For i := 0 To high(fSongs) Do
    If SameText(fSongs[i].Name, Name) Then exit(i);
  result := -1;
End;

Procedure TAGMusicBook.LoadFromLines(Const Lines: Array Of String);
Var
  i, k, cs, cf, e: Integer;
  line, left, right, kind, name, err: String;
  words: TStringList;
  v: Double;
  order: TAGIntArray;

  Procedure Error(Const Msg: String);
  Begin
    fErrors.Add(IntToStr(i + 1) + ': ' + Msg + ': ' + Lines[i]);
  End;

  Procedure AddInst;
  Var
    n, p: Integer;
    part: TAGSoundPart;
    s: String;
  Begin
    If Not AGParseSoundPart(right, part) Then Begin
      Error('instrument line not understood');
      exit;
    End;
    n := InstIndex(fSongs[cs], name);
    If n < 0 Then Begin
      n := length(fSongs[cs].Insts);
      setlength(fSongs[cs].Insts, n + 1);
      fSongs[cs].Insts[n] := Default(TAGMusicInst);
      fSongs[cs].Insts[n].Name := name;
      fSongs[cs].Insts[n].Gate := 0.9;
    End;
    If WordValue(right, 'f') = '' Then Begin // notes: f is a factor
      part.F0 := 1;
      part.F1 := 1;
    End;
    With fSongs[cs].Insts[n] Do Begin
      p := length(Parts);
      part.Tag := 16 * (n + 1) + p;
      setlength(Parts, p + 1);
      Parts[p] := part;
      setlength(FixedLen, p + 1);
      FixedLen[p] := WordValue(right, 'len') <> '';
      s := WordValue(right, 'gate');
      If (s <> '') And ParseNum(s, v) Then Gate := AGClamp(v, 0.05, 1);
      s := WordValue(right, 'echo');
      If s <> '' Then Begin
        If Not ParseRange(StringReplace(s, ',', '..', []), EchoSteps, EchoFb) Then
          Error('echo=steps,feedback expected');
        EchoFb := AGClamp(EchoFb, 0, 0.9);
      End;
    End;
  End;

  Procedure AddPattern;
  Var
    n, j, k2, tr, src, note: Integer;
    t: TAGMusicToken;
    accent: Boolean;
    w, sub: String;
  Begin
    n := PatIndex(fSongs[cs], name);
    If n < 0 Then Begin
      n := length(fSongs[cs].Pats);
      setlength(fSongs[cs].Pats, n + 1);
      fSongs[cs].Pats[n].Name := name;
      fSongs[cs].Pats[n].Tokens := Nil;
    End;
    words.Clear;
    ExtractStrings([' ', #9], [], PChar(right), words);
    For j := 0 To words.Count - 1 Do Begin
      w := words[j];
      If w = '|' Then Continue;
      If w[1] = '@' Then Begin
        SplitTranspose(Copy(w, 2, MaxInt), sub, tr);
        src := PatIndex(fSongs[cs], sub);
        If (src < 0) Or (src = n) Then Begin
          Error('unknown pattern ' + w);
          Continue;
        End;
        For k2 := 0 To high(fSongs[cs].Pats[src].Tokens) Do Begin
          t := fSongs[cs].Pats[src].Tokens[k2];
          If t.Kind = mtNote Then t.Note := t.Note + tr;
          With fSongs[cs].Pats[n] Do Begin
            setlength(Tokens, length(Tokens) + 1);
            Tokens[high(Tokens)] := t;
          End;
        End;
        Continue;
      End;
      t := Default(TAGMusicToken);
      t.Vel := 1;
      If w = '.' Then t.Kind := mtRest
      Else If w = '-' Then t.Kind := mtTie
      Else If w = 'x' Then t.Kind := mtHit
      Else If w = 'X' Then Begin
        t.Kind := mtHit;
        t.Vel := 1.4;
      End
      Else If w = 'o' Then Begin
        t.Kind := mtHit;
        t.Vel := 0.5;
      End
      Else If ParseNote(w, note, accent) Then Begin
        t.Kind := mtNote;
        t.Note := note;
        If accent Then t.Vel := 1.35;
      End
      Else Begin
        Error('unknown token ' + w);
        Continue;
      End;
      With fSongs[cs].Pats[n] Do Begin
        setlength(Tokens, length(Tokens) + 1);
        Tokens[high(Tokens)] := t;
      End;
    End;
  End;

  Procedure AddPart;
  Var
    n, j, c, ii, pp, tr: Integer;
    w, pat: String;
  Begin
    If PartIndex(fSongs[cs], name) >= 0 Then Begin
      Error('part defined twice');
      exit;
    End;
    n := length(fSongs[cs].Parts);
    setlength(fSongs[cs].Parts, n + 1);
    fSongs[cs].Parts[n] := Default(TAGMusicPart);
    fSongs[cs].Parts[n].Name := name;
    words.Clear;
    ExtractStrings([' ', #9], [], PChar(right), words);
    For j := 0 To words.Count - 1 Do Begin
      w := words[j];
      c := Pos(':', w);
      If c = 0 Then Begin
        Error('inst:pattern expected, not ' + w);
        Continue;
      End;
      ii := InstIndex(fSongs[cs], Copy(w, 1, c - 1));
      SplitTranspose(Copy(w, c + 1, MaxInt), pat, tr);
      pp := PatIndex(fSongs[cs], pat);
      If (ii < 0) Or (pp < 0) Then Begin
        Error('unknown instrument or pattern ' + w);
        Continue;
      End;
      With fSongs[cs].Parts[n] Do Begin
        setlength(Entries, length(Entries) + 1);
        Entries[high(Entries)].Inst := ii;
        Entries[high(Entries)].Pat := pp;
        Entries[high(Entries)].Transpose := tr;
        Steps := AGClampI(length(fSongs[cs].Pats[pp].Tokens), Steps, MaxInt);
      End;
    End;
    If fSongs[cs].Parts[n].Steps = 0 Then Error('empty part');
  End;

Begin
  fSongs := Nil;
  fFiles := Nil;
  fErrors.Clear;
  cs := -1;
  cf := -1;
  words := TStringList.Create;
  Try
    For i := 0 To high(Lines) Do Begin
      line := Lines[i];
      If Pos(';', line) > 0 Then line := Copy(line, 1, Pos(';', line) - 1);
      line := Trim(line);
      If line = '' Then Continue;
      If (line[1] = '[') And (line[length(line)] = ']') Then Begin
        line := Copy(line, 2, length(line) - 2);
        cs := -1;
        cf := -1;
        If SameText(Copy(line, 1, 5), 'song:') Then Begin
          cs := length(fSongs);
          setlength(fSongs, cs + 1);
          fSongs[cs] := Default(TAGSong);
          fSongs[cs].Name := Trim(Copy(line, 6, MaxInt));
          fSongs[cs].Bpm := 120;
          fSongs[cs].Beat := 4;
          fSongs[cs].Loop := true;
        End
        Else If SameText(Copy(line, 1, 6), 'music:') Then Begin
          cf := length(fFiles);
          setlength(fFiles, cf + 1);
          fFiles[cf] := Default(TAGMusicFile);
          fFiles[cf].Name := Trim(Copy(line, 7, MaxInt));
          fFiles[cf].Tempo := 1;
        End
        Else
          Error('[song:name] or [music:file] expected');
        Continue;
      End;
      k := Pos('=', line);
      If (k = 0) Or ((cs < 0) And (cf < 0)) Then Begin
        Error('not understood');
        Continue;
      End;
      left := Trim(Copy(line, 1, k - 1));
      right := Trim(Copy(line, k + 1, MaxInt));
      words.Clear;
      ExtractStrings([' ', #9], [], PChar(left), words);
      If words.Count = 2 Then Begin
        kind := LowerCase(words[0]);
        name := words[1];
        If cs < 0 Then Error('only in [song:..]')
        Else If kind = 'inst' Then AddInst
        Else If kind = 'pat' Then AddPattern
        Else If kind = 'part' Then AddPart
        Else Error('inst, pat or part expected');
        Continue;
      End;
      kind := LowerCase(left);
      If cs >= 0 Then Begin
        If kind = 'order' Then fSongs[cs].Order := right
        Else If (kind = 'bpm') And ParseNum(right, v) And (v > 0) Then fSongs[cs].Bpm := v
        Else If (kind = 'beat') And TryStrToInt(right, e) And (e > 0) Then fSongs[cs].Beat := e
        Else If (kind = 'key') And TryStrToInt(right, e) Then fSongs[cs].Key := e
        Else If (kind = 'rms') And ParseNum(right, v) Then fSongs[cs].Rms := v
        Else If kind = 'loop' Then fSongs[cs].Loop := right <> '0'
        Else Error('not understood');
      End
      Else Begin
        If kind = 'song' Then fFiles[cf].Song := right
        Else If kind = 'order' Then fFiles[cf].Order := right
        Else If (kind = 'tempo') And ParseNum(right, v) And (v > 0) Then fFiles[cf].Tempo := v
        Else If (kind = 'key') And TryStrToInt(right, e) Then fFiles[cf].Key := e
        Else Error('not understood');
      End;
    End;
  Finally
    words.Free;
  End;
  // references: every order must resolve
  For k := 0 To high(fSongs) Do Begin
    err := ResolveOrder(fSongs[k], fSongs[k].Order, order);
    If err <> '' Then fErrors.Add('[song:' + fSongs[k].Name + '] ' + err);
  End;
  For k := 0 To high(fFiles) Do Begin
    e := IndexOfSong(fFiles[k].Song);
    If e < 0 Then
      fErrors.Add('[music:' + fFiles[k].Name + '] unknown song ' + fFiles[k].Song)
    Else If fFiles[k].Order <> '' Then Begin
      err := ResolveOrder(fSongs[e], fFiles[k].Order, order);
      If err <> '' Then fErrors.Add('[music:' + fFiles[k].Name + '] ' + err);
    End;
  End;
End;

Procedure TAGMusicBook.LoadFromStrings(Lines: TStrings);
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

Procedure TAGMusicBook.Render(Index: Integer; Const Variation: TAGMusicVariation; Out Buf: TAGSamples);
Var
  f: TAGMusicFile;
  v: TAGMusicVariation;
  s: Integer;
  order: String;
Begin
  f := fFiles[Index];
  s := IndexOfSong(f.Song);
  If s < 0 Then Raise Exception.Create('music: unknown song ' + f.Song);
  v := Variation;
  v.Tempo := v.Tempo * f.Tempo;
  v.Transpose := v.Transpose + f.Key;
  order := f.Order;
  If order = '' Then order := fSongs[s].Order;
  Buf := AGRenderMusic(fSongs[s], order, v);
End;

Function TAGMusicBook.RenderWav(Index: Integer; Const Variation: TAGMusicVariation): TBytes;
Var
  buf: TAGSamples;
Begin
  Render(Index, Variation, buf);
  result := AGPcmToWav(AGSamplesToPcm(buf));
End;

(* DC blocker (about 15 Hz), pulse waves with duty <> 0.5 have an offset *)
Procedure DcBlock(Var Buf: TAGSamples; Circular: Boolean);
Const
  R = 0.996;
Var
  s: Integer;
  x1, y1, x: Double;
Begin
  If length(Buf) = 0 Then exit;
  x1 := 0;
  y1 := 0;
  If Circular Then Begin
    x1 := Buf[high(Buf)];
    For s := 0 To high(Buf) Do Begin // warm up with one loop
      y1 := Buf[s] - x1 + R * y1;
      x1 := Buf[s];
    End;
  End;
  For s := 0 To high(Buf) Do Begin
    x := Buf[s];
    y1 := x - x1 + R * y1;
    x1 := x;
    Buf[s] := y1;
  End;
End;

Function AGRenderMusic(Const Song: TAGSong; Const OrderText: String; Const Variation: TAGMusicVariation): TAGSamples;
Type
  TEvent = Record
    Step, Steps, Note: Integer; // Note -1 = hit
    Vel: Double;
  End;
Var
  order: TAGIntArray;
  err: String;
  stepMs, rms: Double;
  i, o, e, k, s, total, offset, len, transpose, n, cur: Integer;
  parts: TAGSoundParts;
  events: Array Of TEvent;
  tracks: Array Of TAGSamples;
  buf, mix: TAGSamples;
  tok: TAGMusicToken;
  P: TAGSoundPart;
  sv: TAGSoundVariation;
  fx: TFx;
  hz: Double;
Begin
  err := ResolveOrder(Song, OrderText, order);
  If err <> '' Then Raise Exception.Create('music ' + Song.Name + ': ' + err);
  stepMs := 60000 / (Song.Bpm * AGMax(0.1, Variation.Tempo)) / Song.Beat;
  total := 0;
  For o := 0 To high(order) Do
    total := total + Song.Parts[order[o]].Steps;
  len := MsToSamples(total * stepMs);
  transpose := Song.Key + Variation.Transpose;
  tracks := Nil;
  setlength(tracks, length(Song.Insts));
  For i := 0 To high(Song.Insts) Do Begin
    // all notes of this instrument as parts of one long sound
    parts := Nil;
    offset := 0;
    For o := 0 To high(order) Do Begin
      With Song.Parts[order[o]] Do
        For e := 0 To high(Entries) Do Begin
          If Entries[e].Inst <> i Then Continue;
          events := Nil;
          cur := -1;
          n := length(Song.Pats[Entries[e].Pat].Tokens);
          For s := 0 To Steps - 1 Do Begin
            tok := Song.Pats[Entries[e].Pat].Tokens[s Mod n];
            Case tok.Kind Of
              mtRest: cur := -1;
              mtTie: If cur >= 0 Then inc(events[cur].Steps);
            Else Begin
                cur := length(events);
                setlength(events, cur + 1);
                events[cur].Step := offset + s;
                events[cur].Steps := 1;
                events[cur].Vel := tok.Vel;
                If tok.Kind = mtNote Then
                  events[cur].Note := tok.Note + Entries[e].Transpose + transpose
                Else
                  events[cur].Note := -1;
              End;
            End;
          End;
          For s := 0 To high(events) Do
            For k := 0 To high(Song.Insts[i].Parts) Do Begin
              P := Song.Insts[i].Parts[k];
              P.T := P.T + events[s].Step * stepMs;
              If Not Song.Insts[i].FixedLen[k] Then
                P.Len := events[s].Steps * stepMs * Song.Insts[i].Gate;
              If events[s].Note >= 0 Then Begin
                hz := MidiHz(events[s].Note);
                P.F0 := P.F0 * hz;
                P.F1 := P.F1 * hz;
              End;
              P.Vol := P.Vol * events[s].Vel;
              If P.Wave = awSquare Then P.Duty := AGClamp(P.Duty * Variation.Duty, 0.05, 0.95);
              setlength(parts, length(parts) + 1);
              parts[high(parts)] := P;
            End;
        End;
      offset := offset + Song.Parts[order[o]].Steps;
    End;
    If length(parts) = 0 Then Continue;
    sv := Variation.Sound;
    sv.Pitch := 1;
    sv.Decay := 1;
    sv.Seed := DeriveSeed(Variation.Sound.Seed, 'music/' + Song.Insts[i].Name);
    SynthParts(parts, sv, buf);
    tracks[i] := buf;
    If Not Song.Loop Then len := AGClampI(length(buf), len, MaxInt);
  End;
  mix := Nil;
  setlength(mix, len);
  For s := 0 To len - 1 Do
    mix[s] := 0;
  For i := 0 To high(tracks) Do Begin
    If length(tracks[i]) = 0 Then Continue;
    // loop: what sounds past the end continues at the start
    buf := Nil;
    setlength(buf, len);
    For s := 0 To len - 1 Do
      buf[s] := 0;
    For s := 0 To high(tracks[i]) Do
      If Song.Loop Then
        buf[s Mod len] := buf[s Mod len] + tracks[i][s]
      Else If s < len Then
        buf[s] := tracks[i][s];
    With Song.Insts[i] Do
      If (EchoFb > 0) And (EchoSteps > 0) Then Begin
        fx := NewEcho(MsToSamples(EchoSteps * stepMs), EchoFb, EchoFb);
        ApplyFx(buf, fx, Song.Loop, 0);
        setlength(buf, len);
      End;
    For s := 0 To len - 1 Do
      mix[s] := mix[s] + buf[s];
  End;
  DcBlock(mix, Song.Loop);
  KitSpace(mix, Variation.Sound.Kit, 0.6, Song.Loop);
  If Song.Rms > 0 Then Begin
    rms := 0;
    For s := 0 To high(mix) Do
      rms := rms + mix[s] * mix[s];
    rms := AGSqrt(rms / AGMax(1, length(mix)));
    If rms > 0 Then
      ApplyGain(mix, HeadroomGain(mix, Song.Rms / rms)); // quieter if the limiter would be busy
  End;
  KitCrush(mix, Variation.Sound.Kit, 0.7);
  result := mix;
End;

End.
