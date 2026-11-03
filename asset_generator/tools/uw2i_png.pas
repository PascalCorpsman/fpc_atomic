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
 * Waveform + spectrogram pictures for wav2ini --png (visual check of the
 * designs against the originals). Every file is one strip, all strips use the
 * same time scale (vertical grid line every 250 ms).
 *)
Unit uw2i_png;

{$MODE ObjFPC}{$H+}

Interface

Procedure WritePng(Const OutFile: String; Const Files: Array Of String);

Implementation

Uses SysUtils, Math, FPImage, FPWritePNG, uw2i_analysis;

Const
  W = 900;
  WaveH = 60;
  SpecH = 128;
  Sep = 6;

Function Heat(v: Double): TFPColor; // 0..1
Var
  r, g, b: Double;
Begin
  v := EnsureRange(v, 0, 1);
  r := EnsureRange(3 * v, 0, 1);
  g := EnsureRange(3 * v - 1, 0, 1);
  b := EnsureRange(3 * v - 2, 0, 1) + 0.25 * (1 - v) * Ord(v > 0.02);
  result.Red := Round(r * 65535);
  result.Green := Round(g * 65535);
  result.Blue := Round(Min(1, b) * 65535);
  result.Alpha := 65535;
End;

Function Gray(v: Integer): TFPColor;
Begin
  result.Red := v * 257;
  result.Green := v * 257;
  result.Blue := v * 257;
  result.Alpha := 65535;
End;

Procedure WritePng(Const OutFile: String; Const Files: Array Of String);
Var
  img: TFPMemoryImage;
  sigs: Array Of TFloatBuf;
  rates: Array Of Integer;
  i, x, y, y0, s0, s1, s, maxLen, rate, bin: Integer;
  mn, mx, msPerPx, v: Double;
  spec: TFloatBuf;
  wr: TFPWriterPNG;
Begin
  setlength(sigs, length(Files));
  setlength(rates, length(Files));
  maxLen := 1;
  For i := 0 To high(Files) Do Begin
    If Not LoadWav(Files[i], sigs[i], rate) Then sigs[i] := Nil;
    rates[i] := rate;
    maxLen := Max(maxLen, Round(length(sigs[i]) * 1000 / rate));
  End;
  msPerPx := maxLen / W;
  img := TFPMemoryImage.Create(W, length(Files) * (WaveH + SpecH + Sep));
  Try
    For y := 0 To img.Height - 1 Do
      For x := 0 To W - 1 Do
        img.Colors[x, y] := Gray(20);
    For i := 0 To high(Files) Do Begin
      y0 := i * (WaveH + SpecH + Sep);
      For x := 0 To W - 1 Do Begin
        s0 := Round(x * msPerPx * rates[i] / 1000);
        s1 := Round((x + 1) * msPerPx * rates[i] / 1000);
        If (round(x * msPerPx) Mod 250) < msPerPx Then
          For y := y0 To y0 + WaveH + SpecH - 1 Do
            img.Colors[x, y] := Gray(60);
        If s0 >= length(sigs[i]) Then Continue;
        mn := 0;
        mx := 0;
        For s := s0 To Min(s1, high(sigs[i])) Do Begin
          mn := Min(mn, sigs[i][s]);
          mx := Max(mx, sigs[i][s]);
        End;
        For y := Round(WaveH / 2 - mx * WaveH / 2) To Round(WaveH / 2 - mn * WaveH / 2) Do
          img.Colors[x, y0 + EnsureRange(y, 0, WaveH - 1)] := Gray(220);
        spec := Spectrum(sigs[i], (s0 + s1) Div 2, 256);
        For y := 0 To SpecH - 1 Do Begin
          bin := y + 1;
          v := 20 * Log10(Max(1E-9, spec[bin] / 32)); // full scale sine ~ 0 dB
          img.Colors[x, y0 + WaveH + SpecH - 1 - y] := Heat((v + 80) / 80);
        End;
      End;
    End;
    wr := TFPWriterPNG.Create;
    Try
      img.SaveToFile(OutFile, wr);
    Finally
      wr.Free;
    End;
  Finally
    img.Free;
  End;
End;

End.
