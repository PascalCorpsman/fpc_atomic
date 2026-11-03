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
 * wav2ini: helper for the sound definitions of the asset generator
 * (asset_generator/sounds.ini). Run by the developers, not part of the game.
 *
 *   wav2ini --design <data dir> <out.ini>
 *       Measures coarse features of the original sounds (data/sounds/*.wav,
 *       data/res/menuexit.wav, quitgame.wav) and writes an own design per
 *       file (see uw2i_design). Loudness is matched to the original (one
 *       number per sound). The result is a draft to be refined by hand.
 *   wav2ini --inc <sounds.ini | music.ini> <out.inc>
 *       Converts sounds.ini (music.ini) into the include file that is
 *       compiled into the asset generator (uag_sounds_ini.inc,
 *       uag_music_ini.inc, constant SoundsIni / MusicIni). Run after every
 *       change of the ini.
 *   wav2ini --render <sounds.ini> <out dir> [name ...]
 *       Renders sections with neutral variation (quick listening / checks).
 *   wav2ini --music <music.ini> <out dir> [kit [seed]]
 *       Renders every [music:..] file with neutral variation, optionally with
 *       a sound kit (flat, soft, pixel, retro) and noise seed.
 *   wav2ini --features <file.wav> ...
 *       Prints the measured features.
 *   wav2ini --compare <dir a> <dir b>
 *       Features of all WAVs in a next to the ones with the same name in b.
 *   wav2ini --png <out.png> <file.wav> [<file.wav> ...]
 *       Waveform and spectrogram of each file, stacked (same time scale).
 *
 * Build: cd asset_generator/tools && fpc -Mobjfpc -Sh -O2 -Fu.. -FU<lib> wav2ini.lpr
 *)
Program wav2ini;

{$MODE ObjFPC}{$H+}

Uses
  SysUtils, Classes, Math, uw2i_analysis, uw2i_design, uw2i_png, uag_sound, uag_random;

Var
  RawPeak: Double; // peak of the last RenderLines before the limiter
  CalibRates: Array Of Double;
  CalibBright: Array Of Double;
  CalibRms: Array Of Double;

Function RenderLines(Const Lines: Array Of String; Seed: QWord): TFloatBuf;
Var
  book: TAGSoundBook;
  buf: TAGSamples;
  pcm: TAGPcm;
  i: Integer;
  l: Array Of String;
Begin
  l := Nil;
  setlength(l, length(Lines) + 1);
  l[0] := '[x]';
  For i := 0 To high(Lines) Do
    l[i + 1] := Lines[i];
  book := TAGSoundBook.Create;
  Try
    book.LoadFromLines(l);
    If book.Errors.Count > 0 Then Raise Exception.Create('not understood: ' + book.Errors[0]);
    AGSynthSound(book.Parts[0], AGNeutralVariation(Seed), buf);
  Finally
    book.Free;
  End;
  RawPeak := 0;
  For i := 0 To high(buf) Do
    RawPeak := Max(RawPeak, Abs(buf[i]));
  pcm := AGSamplesToPcm(buf);
  result := Nil;
  setlength(result, length(pcm));
  For i := 0 To high(pcm) Do
    result[i] := pcm[i] / 32768;
End;

Function DesignLines(Const D: TDesign): TStringList;
Var
  i: Integer;
Begin
  result := TStringList.Create;
  For i := 0 To high(D.Parts) Do
    result.Add(PartLine(D.Parts[i]));
End;

Function RenderDesign(Const D: TDesign; Seed: QWord): TFloatBuf;
Var
  sl: TStringList;
Begin
  sl := DesignLines(D);
  Try
    result := RenderLines(sl.ToStringArray, Seed);
  Finally
    sl.Free;
  End;
End;

(* brightness of low passed white noise for a few cutoffs, measured with the same analysis *)
Procedure Calibrate;
Var
  r: Double;
  sig: TFloatBuf;
  f: TFeatures;

  Procedure Add(Cut: Double; Const Line: String);
  Begin
    sig := RenderLines([Line], 1);
    f := Analyze('calib', sig, AG_SOUND_RATE);
    setlength(CalibRates, length(CalibRates) + 1);
    setlength(CalibBright, length(CalibBright) + 1);
    setlength(CalibRms, length(CalibRms) + 1);
    CalibRates[high(CalibRates)] := Cut;
    CalibBright[high(CalibBright)] := (f.BrightStart + f.BrightEnd) / 2;
    CalibRms[high(CalibRms)] := f.Rms;
  End;

Begin
  r := 30;
  While r < 11000 Do Begin
    Add(r, 'noise len=500 f=22050 vol=0.5 curve=0 lp=' + IntToStr(Round(r)));
    r := r * 1.2;
  End;
  Add(11000, 'noise len=500 f=22050 vol=0.5 curve=0 lp=11000');
End;

(* loudness of low passed noise relative to white noise *)
Function CalibGain(Lp: Double): Double;
Var
  i: Integer;
  t, w: Double;
Begin
  w := CalibRms[high(CalibRms)];
  If (Lp <= 0) Or (Lp >= CalibRates[high(CalibRates)]) Then exit(1);
  If Lp <= CalibRates[0] Then exit(w / CalibRms[0]);
  For i := 1 To high(CalibRates) Do
    If Lp <= CalibRates[i] Then Begin
      t := (Ln(Lp) - Ln(CalibRates[i - 1])) / (Ln(CalibRates[i]) - Ln(CalibRates[i - 1]));
      exit(w / (CalibRms[i - 1] + t * (CalibRms[i] - CalibRms[i - 1])));
    End;
  result := 1;
End;

(* Lp = 0: white noise *)
Function CalibLookup(Rate: Double): Double;
Var
  i: Integer;
  t: Double;
Begin
  If Rate <= 0 Then Rate := 11000;
  If Rate <= CalibRates[0] Then exit(CalibBright[0]);
  For i := 1 To high(CalibRates) Do
    If Rate <= CalibRates[i] Then Begin
      t := (Ln(Rate) - Ln(CalibRates[i - 1])) / (Ln(CalibRates[i]) - Ln(CalibRates[i - 1]));
      exit(CalibBright[i - 1] + t * (CalibBright[i] - CalibBright[i - 1]));
    End;
  result := CalibBright[high(CalibBright)];
End;

Function SeedOf(Const Name: String): QWord;
Begin
  result := FNV1a64(Name);
End;

Procedure DoDesign(Const DataDir, OutFile: String);
Var
  files, outL, lines: TStringList;
  sr: TSearchRec;
  i, pass: Integer;
  sig, ours: TFloatBuf;
  rate: Integer;
  f, g: TFeatures;
  d: TDesign;
  name, dir: String;
  scale: Double;
Begin
  Calibrate;
  NoiseBrightness := @CalibLookup;
  NoiseGain := @CalibGain;
  files := TStringList.Create;
  outL := TStringList.Create;
  outL.LineBreak := #10;
  Try
    dir := IncludeTrailingPathDelimiter(DataDir);
    If FindFirst(dir + 'sounds' + PathDelim + '*.wav', faAnyFile, sr) = 0 Then Begin
      Repeat
        files.Add(dir + 'sounds' + PathDelim + sr.Name);
      Until FindNext(sr) <> 0;
      FindClose(sr);
    End;
    files.Sort;
    files.Add(dir + 'res' + PathDelim + 'menuexit.wav');
    files.Add(dir + 'res' + PathDelim + 'quitgame.wav');
    outL.Add('; Sound definitions of the asset generator (generator "sounds").');
    outL.Add('; Own interpretations of the game events: each section is a short synth recipe');
    outL.Add('; (format of fpcTesttone / uag_sound). Drafted by tools/wav2ini from coarse');
    outL.Add('; features (length, loudness, brightness, pitch level) and refined by hand.');
    outL.Add('; Spoken sounds are own phrases. After a change: wav2ini --inc sounds.ini uag_sounds_ini.inc');
    outL.Add(';');
    outL.Add('; welle t=.. len=.. f=.. vol=.. attack=.. curve=.. duty=.. vib=.. rep=.. ring=.. f1=.. f2=.. f3=.. bw=.. lp=..');
    outL.Add('; (description of the fields: asset_generator/uag_sound.pas; lp = low pass, an extension that');
    outL.Add('; fpcTesttone ignores). Section name = file name of the game (sounds/, menuexit + quitgame: res/).');
    outL.Add('; "measured" lines: the coarse features of the original the draft was derived from (env = level');
    outL.Add('; in dB at ms, at most 8 points; onsets = count and times of hits).');
    For i := 0 To files.Count - 1 Do Begin
      name := ChangeFileExt(ExtractFileName(files[i]), '');
      If Not LoadWav(files[i], sig, rate) Then Begin
        WriteLn('cannot read ', files[i]);
        Continue;
      End;
      f := Analyze(name, sig, rate);
      d := DesignSound(name, f);
      // loudness: match the RMS of the original (one number), keep the peak below full scale
      If f.Rms > 0 Then
        For pass := 1 To 4 Do Begin
          ours := RenderDesign(d, SeedOf(name));
          g := Analyze(name, ours, AG_SOUND_RATE);
          If g.Rms <= 0 Then break;
          // the soft limiter compresses the peaks, so iterate on the rendered RMS;
          // do not push the limiter too hard (peak of the mix before it at most 1.3)
          scale := f.Rms / g.Rms;
          If RawPeak * scale > 1.3 Then scale := 1.3 / RawPeak;
          ScaleVolumes(d, scale);
        End;
      outL.Add('');
      outL.Add('[' + name + ']');
      outL.Add('; ' + d.Kind + ': ' + d.Text);
      If f.LenMs > 0 Then
        outL.Add('; measured: ' + FeatureLine(f));
      lines := DesignLines(d);
      Try
        outL.AddStrings(lines);
      Finally
        lines.Free;
      End;
      WriteLn(name: 10, ' ', d.Kind: 10, ' ', length(d.Parts), ' lines');
    End;
    outL.SaveToFile(OutFile);
    For i := 0 To high(CalibRates) Do
      Write(Round(CalibRates[i]), ':', Round(CalibBright[i]), ' ');
    WriteLn;
  Finally
    files.Free;
    outL.Free;
  End;
End;

Procedure DoInc(Const IniFile, OutFile: String);
Var
  src, dst: TStringList;
  i: Integer;
  line, constName: String;
  first: Boolean;
Begin
  // sounds.ini -> SoundsIni, music.ini -> MusicIni
  constName := ChangeFileExt(ExtractFileName(IniFile), '');
  constName := UpperCase(Copy(constName, 1, 1)) + Copy(constName, 2, MaxInt) + 'Ini';
  src := TStringList.Create;
  dst := TStringList.Create;
  dst.LineBreak := #10;
  Try
    src.LoadFromFile(IniFile);
    dst.Add('(*');
    dst.Add(' * Generated from ' + ExtractFileName(IniFile) + ' by tools/wav2ini --inc, do not edit.');
    dst.Add(' * Comments and empty lines are dropped.');
    dst.Add(' *)');
    first := true;
    For i := 0 To src.Count - 1 Do Begin
      line := src[i];
      If Pos(';', line) > 0 Then line := Copy(line, 1, Pos(';', line) - 1);
      line := Trim(line);
      If line = '' Then Continue;
      If length(line) > 250 Then Raise Exception.Create('line too long: ' + line);
      If first Then
        dst.Add(constName + ': Array[0..%COUNT%] Of String = (')
      Else
        dst[dst.Count - 1] := dst[dst.Count - 1] + ',';
      dst.Add('  ''' + StringReplace(line, '''', '''''', [rfReplaceAll]) + '''');
      first := false;
    End;
    dst.Add('  );');
    dst.Text := StringReplace(dst.Text, '%COUNT%', IntToStr(dst.Count - 7), []);
    dst.SaveToFile(OutFile);
  Finally
    src.Free;
    dst.Free;
  End;
End;

Procedure DoRender(Const IniFile, OutDir: String; Only: TStrings);
Var
  sl: TStringList;
  book: TAGSoundBook;
  i: Integer;
  data: TBytes;
  fs: TFileStream;
Begin
  sl := TStringList.Create;
  book := TAGSoundBook.Create;
  Try
    sl.LoadFromFile(IniFile);
    book.LoadFromStrings(sl);
    For i := 0 To book.Errors.Count - 1 Do
      WriteLn('not understood: ', book.Errors[i]);
    ForceDirectories(OutDir);
    For i := 0 To book.Count - 1 Do Begin
      If (Only.Count > 0) And (Only.IndexOf(book.Names[i]) < 0) Then Continue;
      data := AGRenderWav(book.Parts[i], AGNeutralVariation(SeedOf(book.Names[i])));
      fs := TFileStream.Create(IncludeTrailingPathDelimiter(OutDir) + book.Names[i] + '.wav', fmCreate);
      Try
        fs.WriteBuffer(data[0], length(data));
      Finally
        fs.Free;
      End;
    End;
  Finally
    sl.Free;
    book.Free;
  End;
End;

Procedure DoMusic(Const IniFile, OutDir, KitName: String; Seed: QWord);
Var
  sl: TStringList;
  book: TAGMusicBook;
  i: Integer;
  data: TBytes;
  fs: TFileStream;
  v: TAGMusicVariation;
  t: QWord;
Begin
  sl := TStringList.Create;
  book := TAGMusicBook.Create;
  Try
    sl.LoadFromFile(IniFile);
    book.LoadFromStrings(sl);
    For i := 0 To book.Errors.Count - 1 Do
      WriteLn('error: ', book.Errors[i]);
    If book.Errors.Count > 0 Then Halt(2);
    v := AGNeutralMusicVariation(Seed);
    Case LowerCase(KitName) Of
      'flat': v.Sound.Kit := skFlat;
      'soft': v.Sound.Kit := skSoft;
      'pixel': v.Sound.Kit := skPixel;
      'retro': v.Sound.Kit := skRetro;
    End;
    ForceDirectories(OutDir);
    For i := 0 To book.FileCount - 1 Do Begin
      t := GetTickCount64;
      data := book.RenderWav(i, v);
      WriteLn(book.Files[i].Name, ': ', (length(data) - 44) Div 2, ' samples, ', GetTickCount64 - t, ' ms');
      fs := TFileStream.Create(IncludeTrailingPathDelimiter(OutDir) + book.Files[i].Name + '.wav', fmCreate);
      Try
        fs.WriteBuffer(data[0], length(data));
      Finally
        fs.Free;
      End;
    End;
  Finally
    sl.Free;
    book.Free;
  End;
End;

Procedure DoFeatures(First: Integer);
Var
  i, rate: Integer;
  sig: TFloatBuf;
Begin
  For i := First To ParamCount Do
    If LoadWav(ParamStr(i), sig, rate) Then
      WriteLn(ChangeFileExt(ExtractFileName(ParamStr(i)), ''): 10, ' ',
        FeatureLine(Analyze(ParamStr(i), sig, rate)))
    Else
      WriteLn(ParamStr(i), ': cannot read');
End;

Procedure DoCompare(Const DirA, DirB: String);
Var
  sr: TSearchRec;
  names: TStringList;
  i, rate: Integer;
  sig: TFloatBuf;
  a, b: TFeatures;
  fb: String;
Begin
  names := TStringList.Create;
  Try
    If FindFirst(IncludeTrailingPathDelimiter(DirA) + '*.wav', faAnyFile, sr) = 0 Then Begin
      Repeat
        names.Add(sr.Name);
      Until FindNext(sr) <> 0;
      FindClose(sr);
    End;
    names.Sort;
    WriteLn(Format('%-10s %6s %6s | %5s %5s | %5s %5s | %5s %5s | %11s %11s | %4s %4s',
      ['name', 'lenA', 'lenB', 'pkA', 'pkB', 'rmsA', 'rmsB', 'tonA', 'tonB', 'brightA', 'brightB', 'onA', 'onB']));
    For i := 0 To names.Count - 1 Do Begin
      fb := IncludeTrailingPathDelimiter(DirB) + names[i];
      a := Default(TFeatures);
      b := Default(TFeatures);
      If LoadWav(IncludeTrailingPathDelimiter(DirA) + names[i], sig, rate) Then a := Analyze(names[i], sig, rate);
      If FileExists(fb) And LoadWav(fb, sig, rate) Then b := Analyze(names[i], sig, rate);
      WriteLn(Format('%-10s %6d %6d | %5.2f %5.2f | %5.3f %5.3f | %5.2f %5.2f | %5.0f-%5.0f %5.0f-%5.0f | %4d %4d',
        [ChangeFileExt(names[i], ''), a.LenMs, b.LenMs, a.Peak, b.Peak, a.Rms, b.Rms, a.Tonal, b.Tonal,
        a.BrightStart, a.BrightEnd, b.BrightStart, b.BrightEnd, a.OnsetCount, b.OnsetCount]));
    End;
  Finally
    names.Free;
  End;
End;

Var
  only: TStringList;
  i: Integer;
  pngFiles: Array Of String;
Begin
  If ParamCount < 1 Then Begin
    WriteLn('wav2ini --design <data dir> <out.ini> | --inc <ini> <out.inc> | --render <ini> <dir> [names] |');
    WriteLn('        --music <ini> <dir> [kit [seed]] |');
    WriteLn('        --features <wav>... | --compare <dir a> <dir b> | --png <out.png> <wav>...');
    Halt(1);
  End;
  Try
    If (ParamStr(1) = '--design') And (ParamCount >= 3) Then DoDesign(ParamStr(2), ParamStr(3))
    Else If (ParamStr(1) = '--inc') And (ParamCount >= 3) Then DoInc(ParamStr(2), ParamStr(3))
    Else If (ParamStr(1) = '--render') And (ParamCount >= 3) Then Begin
      only := TStringList.Create;
      Try
        For i := 4 To ParamCount Do
          only.Add(ParamStr(i));
        DoRender(ParamStr(2), ParamStr(3), only);
      Finally
        only.Free;
      End;
    End
    Else If (ParamStr(1) = '--music') And (ParamCount >= 3) Then
      DoMusic(ParamStr(2), ParamStr(3), ParamStr(4), StrToQWordDef(ParamStr(5), 12345))
    Else If ParamStr(1) = '--features' Then DoFeatures(2)
    Else If (ParamStr(1) = '--compare') And (ParamCount >= 3) Then DoCompare(ParamStr(2), ParamStr(3))
    Else If (ParamStr(1) = '--png') And (ParamCount >= 3) Then Begin
      pngFiles := Nil;
      For i := 3 To ParamCount Do Begin
        setlength(pngFiles, length(pngFiles) + 1);
        pngFiles[high(pngFiles)] := ParamStr(i);
      End;
      WritePng(ParamStr(2), pngFiles);
    End
    Else Begin
      WriteLn('unknown command');
      Halt(1);
    End;
  Except
    On e: Exception Do Begin
      WriteLn('error: ', e.Message);
      Halt(2);
    End;
  End;
End.
