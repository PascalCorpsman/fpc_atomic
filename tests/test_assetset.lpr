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
 * Checks a generated asset set against the engine contract.
 *
 *   fpc test_assetset.lpr && ./test_assetset <set dir> [<data dir>]
 *
 * For every PNG in the set that also exists in the data dir (original files,
 * only used for the expected size, default ../data next to this folder), it reports:
 *   - size differs from the original
 *   - pink fringe: pixels close to fuchsia that are not fuchsia (anti aliasing
 *     against the transparent background, the engine shows them pink)
 *   - green dominant pixels (g > r and g > b) outside atomic/: the engine only
 *     recolors atomic sheets, elsewhere this is fine, so only reported as info
 * For .ani files it parses the header and the embedded BMP.
 *
 * Exit code 0 = no errors, 1 = errors found, 2 = wrong parameters.
 *)
Program test_assetset;

{$MODE ObjFPC}{$H+}

Uses
  Classes, SysUtils, FPImage, FPReadPNG;

Var
  SetDir, DataDir: String;
  Errors, Infos: TStringList;
  Count: integer;

Function LoadPng(Const Filename: String): TFPMemoryImage;
Var
  Reader: TFPReaderPNG;
Begin
  result := TFPMemoryImage.Create(0, 0);
  Reader := TFPReaderPNG.Create;
  Try
    result.LoadFromFile(Filename, Reader);
  Finally
    Reader.Free;
  End;
End;

Function NearFuchsia(r, g, b: integer): Boolean;
Begin
  result := (r > 180) And (b > 180) And (g < 90) And Not ((r = 255) And (g = 0) And (b = 255));
End;

Procedure CheckPng(Const Path, Rel: String);
Var
  Img, Orig: TFPMemoryImage;
  x, y, r, g, b, Fringe, Fuchsia, Green: integer;
  c: TFPColor;
Begin
  Try
    Img := LoadPng(Path);
  Except
    On e: Exception Do Begin
      Errors.Add(Rel + ': can not be read (' + e.Message + ')');
      exit;
    End;
  End;
  Try
    If (DataDir <> '') And FileExists(DataDir + Rel) Then Begin
      Orig := LoadPng(DataDir + Rel);
      If (Orig.Width <> Img.Width) Or (Orig.Height <> Img.Height) Then
        Errors.Add(format('%s: size %dx%d, original %dx%d', [Rel, Img.Width, Img.Height, Orig.Width, Orig.Height]));
      Orig.Free;
    End;
    Fringe := 0;
    Fuchsia := 0;
    Green := 0;
    For y := 0 To Img.Height - 1 Do
      For x := 0 To Img.Width - 1 Do Begin
        c := Img.Colors[x, y];
        r := c.Red Shr 8;
        g := c.Green Shr 8;
        b := c.Blue Shr 8;
        If (r = 255) And (g = 0) And (b = 255) Then
          inc(Fuchsia)
        Else If NearFuchsia(r, g, b) Then
          inc(Fringe);
        If (g > r) And (g > b) Then inc(Green);
      End;
  Finally
    Img.Free;
  End;
  // Opaque images (no fuchsia at all) may use pink on purpose
  If (Fringe > 0) And (Fuchsia > 0) Then Errors.Add(format('%s: %d pink fringe pixels', [Rel, Fringe]));
  If (pos('atomic/', Rel) <> 1) And (Green > 0) Then
    Infos.Add(format('%s: %d green dominant pixels (fine outside atomic/)', [Rel, Green]));
  If (pos('atomic/', Rel) = 1) And (Green = 0) And (pos('shadow', Rel) = 0) Then
    Infos.Add(Rel + ': no green pixels, nothing gets the player color');
End;

Procedure CheckAni(Const Path, Rel: String);
Var
  ms: TMemoryStream;
  d: PByte;
  p, n, i, ln: Int64;
  BmpSize: DWord;
  Head: String;

  Function I32(At: Int64): Int64;
  Begin
    result := Int32(d[At] Or (d[At + 1] Shl 8) Or (d[At + 2] Shl 16) Or (DWord(d[At + 3]) Shl 24));
  End;

Begin
  ms := TMemoryStream.Create;
  Try
    ms.LoadFromFile(Path);
    d := ms.Memory;
    If ms.Size < 20 Then Begin
      Errors.Add(Rel + ': too short');
      exit;
    End;
    SetString(Head, PChar(@d[4]), 8);
    If Head <> 'CANIFILE' Then Begin
      Errors.Add(Rel + ': no CANIFILE header');
      exit;
    End;
    p := 16;
    n := I32(p);
    p := p + 4;
    For i := 0 To n - 1 Do Begin
      p := p + 9;
      If p >= ms.Size Then break;
      If d[p] <> 0 Then Begin
        p := p + 1;
        If p + 6 > ms.Size Then break;
        BmpSize := DWord(I32(p + 2));
        p := p + BmpSize;
      End
      Else
        p := p + 1;
      p := p + 1 + 4 + 16;
      If p + 4 > ms.Size Then break;
      ln := I32(p);
      p := p + 4 + ln + 1 + 24;
    End;
    If p <> ms.Size Then Errors.Add(format('%s: parsed %d of %d bytes', [Rel, p, ms.Size]));
  Finally
    ms.Free;
  End;
End;

Procedure Walk(Const Dir, RelDir: String);
Var
  sr: TSearchRec;
  Names: TStringList;
  i: integer;
  Rel, Ext: String;
Begin
  Names := TStringList.Create;
  Try
    If FindFirst(Dir + '*', faAnyFile, sr) = 0 Then Begin
      Repeat
        If (sr.Name <> '.') And (sr.Name <> '..') Then Names.AddObject(sr.Name, TObject(PtrInt(sr.Attr)));
      Until FindNext(sr) <> 0;
      FindClose(sr);
    End;
    Names.Sort;
    For i := 0 To Names.Count - 1 Do Begin
      Rel := RelDir + Names[i];
      If (PtrInt(Names.Objects[i]) And faDirectory) <> 0 Then Begin
        Walk(Dir + Names[i] + PathDelim, Rel + '/');
        Continue;
      End;
      Ext := LowerCase(ExtractFileExt(Names[i]));
      If Ext = '.png' Then Begin
        inc(Count);
        CheckPng(Dir + Names[i], Rel);
      End
      Else If Ext = '.ani' Then Begin
        inc(Count);
        CheckAni(Dir + Names[i], Rel);
      End;
    End;
  Finally
    Names.Free;
  End;
End;

Var
  i: integer;
Begin
  If ParamCount < 1 Then Begin
    writeln('Usage: test_assetset <set dir> [<data dir>]');
    halt(2);
  End;
  SetDir := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
  If ParamCount > 1 Then
    DataDir := ParamStr(2)
  Else
    DataDir := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'data';
  DataDir := IncludeTrailingPathDelimiter(ExpandFileName(DataDir));
  If Not DirectoryExists(SetDir) Then Begin
    writeln('Not found: ', SetDir);
    halt(2);
  End;
  Errors := TStringList.Create;
  Infos := TStringList.Create;
  Count := 0;
  Walk(SetDir, '');
  For i := 0 To Infos.Count - 1 Do
    writeln('info : ', Infos[i]);
  For i := 0 To Errors.Count - 1 Do
    writeln('ERROR: ', Errors[i]);
  writeln(format('%d files checked, %d errors', [Count, Errors.Count]));
  If Errors.Count > 0 Then ExitCode := 1;
  Errors.Free;
  Infos.Free;
End.
