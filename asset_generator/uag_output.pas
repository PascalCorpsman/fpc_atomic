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
 * Writes the generated files of an asset set: PNG, .ani (format of
 * TOpenGL_Animation, version 5) and text files. All writers produce the same
 * bytes for the same input (fixed compression level, no time stamps).
 *
 * Paths are relative to the set folder and use '/' (e.g. 'atomic/walk.png'),
 * the same layout as the data folder of the game.
 *
 * For previews: DryRun = true and KeepImages = true writes nothing and keeps
 * a copy of every image in Images (keyed images with fuchsia, as saved) and
 * of every text file (FindText).
 * Cancelled can be set from another thread, the next Save* raises EAbort.
 *)
Unit uag_output;

{$MODE ObjFPC}{$H+}

Interface

Uses Classes, uag_image;

Type

  TAGSaveMode = (
    smKeyed, // fuchsia transparency (hard mask), most sprites
    smOpaque, // no transparency, e.g. field.png and menu screens
    smAlpha // real alpha channel, only shadow.png
    );

  TAGAniRange = Record
    FrameOffset, FrameCount: Integer;
  End;

  TAGAniSpec = Record
    AngleOffset: Single; // 0 for 1 range, 45 for 4 ranges (right, up, left, down are chosen by angle)
    TimePerFrame: Integer; // ms
    FramesPerRow, FramesPerCol: Integer;
    FrameW, FrameH: Integer;
    Ranges: Array Of TAGAniRange; // 1 or 4 entries, angle ranges are split evenly
  End;

  TAGProgressEvent = Procedure(Sender: TObject; Const RelPath: String; Done, Total: Integer) Of Object;

  { TAGOutput }

  TAGOutput = Class
  private
    fDir: String;
    fFiles: TStringList;
    fLock: TRTLCriticalSection;
    fExpected: Integer;
    fImages: TStringList;
    fTexts: TStringList;
    Function FullPath(Const RelPath: String): String;
    Procedure Written(Const RelPath: String);
    Procedure CheckCancel;
    Procedure KeepImage(Const RelPath: String; Img: TAGImage; Keyed: Boolean);
  public
    DryRun: Boolean; // true = nothing is written, only counted (for the progress total)
    KeepImages: Boolean; // true = a copy of every saved image is kept in Images
    Cancelled: Boolean; // set by the UI thread, the next Save* raises EAbort
    OnProgress: TAGProgressEvent;
    Constructor Create(Const aDir: String);
    Destructor Destroy; override;
    Property Dir: String read fDir;
    Property Files: TStringList read fFiles; // relative paths of all written files
    Property Expected: Integer read fExpected write fExpected; // total for the progress, 0 = unknown
    Property Images: TStringList read fImages; // RelPath, Objects = TAGImage (owned), only with KeepImages
    Function FindImage(Const RelPath: String): TAGImage; // Nil if not kept
    Function FindText(Const RelPath: String): String; // lines separated by #10, '' if not kept (only with KeepImages)

    Procedure SaveImage(Const RelPath: String; Img: TAGImage; Mode: TAGSaveMode);
    Procedure SaveAni(Const RelPath: String; Img: TAGImage; Const Spec: TAGAniSpec); // Img is keyed with fuchsia
    Procedure SaveText(Const RelPath: String; Const Lines: Array Of String);
  End;

Function AniSpec(AngleOffset: Single; TimePerFrame, FramesPerRow, FramesPerCol, FrameW, FrameH: Integer;
  Const RangeOffsets, RangeCounts: Array Of Integer): TAGAniSpec;

Procedure WritePNG(Img: TAGImage; Mode: TAGSaveMode; Stream: TStream);

Implementation

Uses SysUtils, FPImage, FPWritePNG, ZStream;

Function AniSpec(AngleOffset: Single; TimePerFrame, FramesPerRow, FramesPerCol, FrameW, FrameH: Integer;
  Const RangeOffsets, RangeCounts: Array Of Integer): TAGAniSpec;
Var
  i: Integer;
Begin
  result := Default(TAGAniSpec);
  result.AngleOffset := AngleOffset;
  result.TimePerFrame := TimePerFrame;
  result.FramesPerRow := FramesPerRow;
  result.FramesPerCol := FramesPerCol;
  result.FrameW := FrameW;
  result.FrameH := FrameH;
  setlength(result.Ranges, length(RangeOffsets));
  For i := 0 To high(RangeOffsets) Do Begin
    result.Ranges[i].FrameOffset := RangeOffsets[i];
    result.Ranges[i].FrameCount := RangeCounts[i];
  End;
End;

Procedure WritePNG(Img: TAGImage; Mode: TAGSaveMode; Stream: TStream);
Var
  fp: TFPMemoryImage;
  w: TFPWriterPNG;
  x, y: Integer;
  src: TAGImage;
  c: TAGColor;
  fc: TFPColor;
Begin
  If Mode = smKeyed Then Begin
    src := Img.Clone;
    src.KeyTransparent;
  End
  Else
    src := Img;
  fp := TFPMemoryImage.Create(src.Width, src.Height);
  w := TFPWriterPNG.Create;
  Try
    For y := 0 To src.Height - 1 Do
      For x := 0 To src.Width - 1 Do Begin
        c := src.Pixels[y * src.Width + x];
        fc.red := c.r * 257;
        fc.green := c.g * 257;
        fc.blue := c.b * 257;
        If Mode = smAlpha Then
          fc.alpha := c.a * 257
        Else
          fc.alpha := alphaOpaque;
        fp.Colors[x, y] := fc;
      End;
    w.UseAlpha := Mode = smAlpha;
    w.Indexed := false;
    w.GrayScale := false;
    w.WordSized := false;
    w.CompressionLevel := clMax;
    fp.SaveToStream(Stream, w);
  Finally
    w.Free;
    fp.Free;
    If src <> Img Then src.Free;
  End;
End;

(*
 * 24 bit BMP exactly like LCL TBitmap.SaveToStream writes it (the .ani loader
 * reads it with TBitmap.LoadFromStream).
 *)
Procedure WriteBMP24(Img: TAGImage; Stream: TStream);
Var
  rowSize, imgSize, x, y: Integer;
  row: Array Of Byte;
  c: TAGColor;

  Procedure W16(v: Word);
  Begin
    v := NtoLE(v);
    Stream.Write(v, 2);
  End;

  Procedure W32(v: Cardinal);
  Begin
    v := NtoLE(v);
    Stream.Write(v, 4);
  End;

Begin
  rowSize := ((Img.Width * 3 + 3) Div 4) * 4;
  imgSize := rowSize * Img.Height;
  // BITMAPFILEHEADER
  W16($4D42); // 'BM'
  W32(54 + imgSize);
  W32(0);
  W32(54);
  // BITMAPINFOHEADER
  W32(40);
  W32(Img.Width);
  W32(Img.Height); // positive = bottom up
  W16(1);
  W16(24);
  W32(0); // BI_RGB
  W32(imgSize);
  W32(100); // pixels per meter, as written by the LCL
  W32(100);
  W32(0);
  W32(0);
  row := Nil;
  setlength(row, rowSize);
  For y := Img.Height - 1 Downto 0 Do Begin
    FillChar(row[0], rowSize, 0);
    For x := 0 To Img.Width - 1 Do Begin
      c := Img.Pixels[y * Img.Width + x];
      row[x * 3 + 0] := c.b;
      row[x * 3 + 1] := c.g;
      row[x * 3 + 2] := c.r;
    End;
    Stream.Write(row[0], rowSize);
  End;
End;

Procedure WriteAni(Img: TAGImage; Const Spec: TAGAniSpec; Stream: TStream);
Const
  Magic: String = 'CANIFILE';
Var
  i, n, v: Integer;
  b: Boolean;
  name: String;
  src: TAGImage;

  Procedure WI(x: Integer);
  Begin
    x := NtoLE(x);
    Stream.Write(x, 4);
  End;

  Procedure WB(x: Boolean);
  Begin
    Stream.Write(x, 1);
  End;

Begin
  src := Img.Clone;
  Try
    src.KeyTransparent;
    WI(5); // TAnimationVersion
    Stream.Write(Magic[1], length(Magic));
    Stream.Write(Spec.AngleOffset, 4);
    n := length(Spec.Ranges);
    WI(n);
    For i := 0 To n - 1 Do Begin
      // TOpenGL_Animation.EqualizeRanges
      WI((360 * i) Div n);
      If i = n - 1 Then
        v := 360
      Else
        v := (360 * (i + 1)) Div n - 1;
      WI(v);
      WB(i <> 0); // Derived
      b := i = 0;
      WB(b); // has bitmap
      If b Then WriteBMP24(src, Stream);
      WB(false); // no alpha mask
      WI(Spec.TimePerFrame);
      // Rect: Left, Top, Right, Bottom
      WI(0);
      WI(0);
      WI(src.Width);
      WI(src.Height);
      name := 'Sprite ' + IntToStr(i + 1);
      Stream.WriteAnsiString(name);
      WB(true); // AlphaImage (fuchsia)
      WI(Spec.FrameW);
      WI(Spec.FrameH);
      WI(Spec.Ranges[i].FrameOffset);
      WI(Spec.Ranges[i].FrameCount);
      WI(Spec.FramesPerRow);
      WI(Spec.FramesPerCol);
    End;
  Finally
    src.Free;
  End;
End;

{ TAGOutput }

Constructor TAGOutput.Create(Const aDir: String);
Begin
  Inherited Create;
  fDir := IncludeTrailingPathDelimiter(aDir);
  fFiles := TStringList.Create;
  fImages := TStringList.Create;
  fImages.OwnsObjects := true;
  fTexts := TStringList.Create;
  InitCriticalSection(fLock);
End;

Destructor TAGOutput.Destroy;
Begin
  fImages.Free;
  fTexts.Free;
  fFiles.Free;
  DoneCriticalSection(fLock);
  Inherited Destroy;
End;

Function TAGOutput.FullPath(Const RelPath: String): String;
Begin
  result := fDir + StringReplace(RelPath, '/', PathDelim, [rfReplaceAll]);
  If Not DryRun Then ForceDirectories(ExtractFilePath(result));
End;

Procedure TAGOutput.Written(Const RelPath: String);
Var
  done: Integer;
Begin
  EnterCriticalSection(fLock);
  Try
    fFiles.Add(RelPath);
    done := fFiles.Count;
  Finally
    LeaveCriticalSection(fLock);
  End;
  If assigned(OnProgress) Then OnProgress(self, RelPath, done, fExpected);
End;

Procedure TAGOutput.CheckCancel;
Begin
  If Cancelled Then Raise EAbort.Create('Cancelled');
End;

Procedure TAGOutput.KeepImage(Const RelPath: String; Img: TAGImage; Keyed: Boolean);
Var
  c: TAGImage;
Begin
  If Not KeepImages Then exit;
  c := Img.Clone;
  If Keyed Then c.KeyTransparent;
  EnterCriticalSection(fLock);
  Try
    fImages.AddObject(RelPath, c);
  Finally
    LeaveCriticalSection(fLock);
  End;
End;

Function TAGOutput.FindImage(Const RelPath: String): TAGImage;
Var
  i: Integer;
Begin
  i := fImages.IndexOf(RelPath);
  If i >= 0 Then
    result := TAGImage(fImages.Objects[i])
  Else
    result := Nil;
End;

Function TAGOutput.FindText(Const RelPath: String): String;
Var
  i: Integer;
Begin
  result := '';
  EnterCriticalSection(fLock);
  Try
    i := fTexts.IndexOfName(RelPath);
    If i >= 0 Then result := StringReplace(fTexts.ValueFromIndex[i], #1, #10, [rfReplaceAll]);
  Finally
    LeaveCriticalSection(fLock);
  End;
End;

Procedure TAGOutput.SaveImage(Const RelPath: String; Img: TAGImage; Mode: TAGSaveMode);
Var
  fs: TFileStream;
Begin
  CheckCancel;
  KeepImage(RelPath, Img, Mode = smKeyed);
  If Not DryRun Then Begin
    fs := TFileStream.Create(FullPath(RelPath), fmCreate);
    Try
      WritePNG(Img, Mode, fs);
    Finally
      fs.Free;
    End;
  End;
  Written(RelPath);
End;

Procedure TAGOutput.SaveAni(Const RelPath: String; Img: TAGImage; Const Spec: TAGAniSpec);
Var
  fs: TFileStream;
Begin
  CheckCancel;
  KeepImage(RelPath, Img, true);
  If Not DryRun Then Begin
    fs := TFileStream.Create(FullPath(RelPath), fmCreate);
    Try
      WriteAni(Img, Spec, fs);
    Finally
      fs.Free;
    End;
  End;
  Written(RelPath);
End;

Procedure TAGOutput.SaveText(Const RelPath: String; Const Lines: Array Of String);
Var
  fs: TFileStream;
  i: Integer;
  s: String;
Begin
  CheckCancel;
  s := '';
  For i := 0 To high(Lines) Do
    s := s + Lines[i] + #10; // same bytes on every platform
  If KeepImages Then Begin
    EnterCriticalSection(fLock);
    Try
      fTexts.Add(RelPath + '=' + StringReplace(s, #10, #1, [rfReplaceAll]));
    Finally
      LeaveCriticalSection(fLock);
    End;
  End;
  If Not DryRun Then Begin
    fs := TFileStream.Create(FullPath(RelPath), fmCreate);
    Try
      If s <> '' Then fs.Write(s[1], length(s));
    Finally
      fs.Free;
    End;
  End;
  Written(RelPath);
End;

End.
