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
 * of every text file (FindText). KeepData additionally keeps the binary
 * files (sounds) in memory (FindData), generators that skip expensive work
 * in a DryRun do it then.
 * Cancelled can be set from another thread, the next Save* raises EAbort.
 *
 * Atomic save of a set (BeginSetSave, CommitSetSave, DiscardSetSave): a set
 * folder is always generated completely, files added to it by hand are lost
 * on the next save. Everything is written into a temporary sibling folder
 * .<folder>.tmp-<n> (the launcher ignores folders starting with '.' and
 * folders without profile.ini, profile.ini is written last). On success the
 * old folder is renamed to .<folder>.old-<n>, the temporary folder to
 * <folder> and the old one is deleted. If a rename fails (a file of the set
 * is in use, e.g. by the game) the old set stays and EAGSetInUse is raised.
 * On cancel or error the temporary folder is deleted, the old set is not
 * touched. CleanupSetLeftovers removes what a crash left behind (an old
 * folder whose set is missing is renamed back).
 *)
Unit uag_output;

{$MODE ObjFPC}{$H+}

Interface

Uses Classes, SysUtils, uag_image;

Type

  EAGSetInUse = Class(Exception); // the old set could not be replaced (file in use)

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

  { TAGData: a kept binary file }

  TAGData = Class
    Bytes: TBytes;
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
    fData: TStringList;
    Function FullPath(Const RelPath: String): String;
    Procedure Written(Const RelPath: String);
    Procedure CheckCancel;
    Procedure KeepImage(Const RelPath: String; Img: TAGImage; Keyed: Boolean);
  public
    DryRun: Boolean; // true = nothing is written, only counted (for the progress total)
    KeepImages: Boolean; // true = a copy of every saved image is kept in Images
    KeepData: Boolean; // true = every binary file (SaveData) is kept in KeptData, rendered also with DryRun
    Cancelled: Boolean; // set by the UI thread, the next Save* raises EAbort
    OnProgress: TAGProgressEvent;
    CancelAfter: Integer; // debug: Cancelled is set after this many files, 0 = off
    Constructor Create(Const aDir: String);
    Procedure Retarget(Const aDir: String); // writes into aDir from now on (temporary folder of the atomic save)
    Destructor Destroy; override;
    Property Dir: String read fDir;
    Property Files: TStringList read fFiles; // relative paths of all written files
    Property Expected: Integer read fExpected write fExpected; // total for the progress, 0 = unknown
    Property Images: TStringList read fImages; // RelPath, Objects = TAGImage (owned), only with KeepImages
    Function FindImage(Const RelPath: String): TAGImage; // Nil if not kept
    Function FindText(Const RelPath: String): String; // lines separated by #10, '' if not kept (only with KeepImages)
    Property KeptData: TStringList read fData; // RelPath, Objects = TAGData (owned), only with KeepData
    Function FindData(Const RelPath: String): TBytes; // Nil if not kept

    Procedure SaveImage(Const RelPath: String; Img: TAGImage; Mode: TAGSaveMode);
    Procedure SaveAni(Const RelPath: String; Img: TAGImage; Const Spec: TAGAniSpec); // Img is keyed with fuchsia
    Procedure SaveText(Const RelPath: String; Const Lines: Array Of String);
    Procedure SaveData(Const RelPath: String; Const Data: TBytes); // binary file as is (e.g. WAV), Data may be Nil with DryRun
  End;

Function AniSpec(AngleOffset: Single; TimePerFrame, FramesPerRow, FramesPerCol, FrameW, FrameH: Integer;
  Const RangeOffsets, RangeCounts: Array Of Integer): TAGAniSpec;

Procedure WritePNG(Img: TAGImage; Mode: TAGSaveMode; Stream: TStream);

(* Atomic save of a set, see the unit header. Directories with or without trailing path delimiter. *)
Function BeginSetSave(Const TargetDir: String): String; // creates the temporary folder, result with trailing delimiter
Procedure CommitSetSave(Const TempDir, TargetDir: String); // replaces TargetDir by TempDir, EAGSetInUse if not possible
Procedure DiscardSetSave(Const TempDir: String); // deletes the temporary folder (if it exists)
Procedure CleanupSetLeftovers(Const ParentDir: String; Const OnlyName: String = ''); // removes .<x>.tmp-<n>, restores or removes .<x>.old-<n>, only if older than 10 minutes (another generator may still be running); OnlyName <> '': only those of this set

Function DeleteTree(Const Dir: String): Boolean; // recursive, true if the folder is gone
Procedure CopyTree(Const Src, Dst: String); // recursive copy of all files

Implementation

Uses FPImage, FPWritePNG, ZStream;

Function SplitDir(Const Dir: String; Out Parent, Name: String): Boolean;
Var
  d: String;
Begin
  d := ExcludeTrailingPathDelimiter(ExpandFileName(Dir));
  Parent := ExtractFilePath(d);
  Name := ExtractFileName(d);
  result := (Name <> '') And (Parent <> '');
End;

Function FreeSibling(Const Parent, Name, Kind: String): String;
Var
  n: Integer;
Begin
  n := 1;
  Repeat
    result := Parent + '.' + Name + '.' + Kind + '-' + IntToStr(n);
    inc(n);
  Until Not DirectoryExists(result) And Not FileExists(result);
End;

(* virus scanners and the search indexer hold new files for a short time *)
Function RenameRetry(Const OldName, NewName: String): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To 9 Do Begin
    If RenameFile(OldName, NewName) Then exit(true);
    If Not DirectoryExists(OldName) Then exit(false);
    Sleep(50 * (i + 1));
  End;
  result := false;
End;

Const
  LeftoverMarker = '.replacing';

Procedure TouchMarker(Const Dir: String);
Var
  fs: TFileStream;
Begin
  Try
    fs := TFileStream.Create(IncludeTrailingPathDelimiter(Dir) + LeftoverMarker, fmCreate);
    fs.Free;
  Except
    // only a protection against a parallel cleanup
  End;
End;

{$PUSH}{$WARN SYMBOL_PLATFORM OFF} // faSymLink, faReadOnly
Function DeleteTree(Const Dir: String): Boolean;
Var
  sr: TSearchRec;
  d: String;
Begin
  d := IncludeTrailingPathDelimiter(Dir);
  // faSymLink: on Unix FindFirst would otherwise follow a link and report its target as a plain folder
  If FindFirst(d + '*', faAnyFile Or faSymLink, sr) = 0 Then Begin
    Repeat
      If (sr.Name = '.') Or (sr.Name = '..') Then Continue;
      If ((sr.Attr And faDirectory) <> 0) And ((sr.Attr And faSymLink) = 0) Then
        DeleteTree(d + sr.Name)
      Else If (sr.Attr And faDirectory) <> 0 Then
        RemoveDir(d + sr.Name) // a link to a folder: only the link
      Else Begin
        If (sr.Attr And faReadOnly) <> 0 Then FileSetAttr(d + sr.Name, sr.Attr And Not faReadOnly);
        DeleteFile(d + sr.Name);
      End;
    Until FindNext(sr) <> 0;
    FindClose(sr);
  End;
  RemoveDir(ExcludeTrailingPathDelimiter(Dir));
  result := Not DirectoryExists(ExcludeTrailingPathDelimiter(Dir));
End;
{$POP}

Procedure CopyTree(Const Src, Dst: String);
Var
  sr: TSearchRec;
  s, d: String;
  fi, fo: TFileStream;
Begin
  s := IncludeTrailingPathDelimiter(Src);
  d := IncludeTrailingPathDelimiter(Dst);
  ForceDirectories(d);
  If FindFirst(s + '*', faAnyFile, sr) = 0 Then Begin
    Try
      Repeat
        If (sr.Name = '.') Or (sr.Name = '..') Then Continue;
        If (sr.Attr And faDirectory) <> 0 Then
          CopyTree(s + sr.Name, d + sr.Name)
        Else Begin
          fi := TFileStream.Create(s + sr.Name, fmOpenRead Or fmShareDenyWrite);
          Try
            fo := TFileStream.Create(d + sr.Name, fmCreate);
            Try
              fo.CopyFrom(fi, 0);
            Finally
              fo.Free;
            End;
          Finally
            fi.Free;
          End;
        End;
      Until FindNext(sr) <> 0;
    Finally
      FindClose(sr);
    End;
  End;
End;

Function BeginSetSave(Const TargetDir: String): String;
Var
  parent, name: String;
  n: Integer;
Begin
  If Not SplitDir(TargetDir, parent, name) Then
    Raise Exception.CreateFmt('Invalid set folder: %s', [TargetDir]);
  // 'auto' is reserved for the sound selection of the launcher ([Launcher] SoundSet)
  If LowerCase(name) = 'auto' Then
    Raise Exception.Create('The folder name "auto" is reserved, please choose another name.');
  ForceDirectories(parent);
  // CreateDir fails if the folder exists, so two generators never write into the same temp folder
  n := 1;
  Repeat
    result := parent + '.' + name + '.tmp-' + IntToStr(n);
    If CreateDir(result) Then break;
    If ((Not DirectoryExists(result)) And (Not FileExists(result))) Or (n > 1000) Then
      Raise Exception.CreateFmt('Could not create the folder %s', [result]);
    inc(n);
  Until false;
  result := IncludeTrailingPathDelimiter(result);
End;

Procedure CommitSetSave(Const TempDir, TargetDir: String);
Var
  parent, name, target, tmp, old: String;
Begin
  If Not SplitDir(TargetDir, parent, name) Then
    Raise Exception.CreateFmt('Invalid set folder: %s', [TargetDir]);
  target := parent + name;
  tmp := ExcludeTrailingPathDelimiter(TempDir);
  If Not DirectoryExists(target) Then Begin
    If Not RenameRetry(tmp, target) Then
      Raise EAGSetInUse.CreateFmt('Could not rename %s to %s', [tmp, target]);
    exit;
  End;
  old := FreeSibling(parent, name, 'old');
  If Not RenameRetry(target, old) Then
    Raise EAGSetInUse.CreateFmt('The set %s is in use (is the game running with it?), close the game and try again. The old set was kept.', [name]);
  // A rename keeps the file times: a fresh file, so the cleanup of another instance does not take it for an old leftover
  TouchMarker(old);
  If Not RenameRetry(tmp, target) Then Begin
    // put the old set back, if that fails too CleanupSetLeftovers does it on the next start (after 10 minutes)
    DeleteFile(IncludeTrailingPathDelimiter(old) + LeftoverMarker);
    If RenameRetry(old, target) Then
      Raise EAGSetInUse.CreateFmt('Could not rename %s to %s, the old set was kept.', [tmp, target])
    Else
      Raise EAGSetInUse.CreateFmt('Could not rename %s to %s, the old set is in %s and comes back on the next start.', [tmp, target, old]);
  End;
  DeleteTree(old); // if something is left, CleanupSetLeftovers removes it on the next start
End;

Procedure DiscardSetSave(Const TempDir: String);
Begin
  If (TempDir <> '') And DirectoryExists(ExcludeTrailingPathDelimiter(TempDir)) Then
    DeleteTree(TempDir);
End;

(* '.<name>.<kind>-<digits>' -> name *)
Function LeftoverName(Const Entry, Kind: String; Out Name: String): Boolean;
Var
  i, p: Integer;
Begin
  result := false;
  Name := '';
  If (length(Entry) < 2) Or (Entry[1] <> '.') Then exit;
  i := length(Entry);
  While (i > 1) And (Entry[i] In ['0'..'9']) Do
    dec(i);
  If i = length(Entry) Then exit; // no digits
  p := i - length(Kind) - 1; // position of the '.' before Kind
  If p < 3 Then exit;
  If copy(Entry, p, length(Kind) + 2) <> '.' + Kind + '-' Then exit;
  Name := copy(Entry, 2, p - 2);
  result := Name <> '';
End;

(*
 * Newest modification time of all files below Dir (0 if there are none). A
 * folder another instance is still writing has a fresh file in it.
 *)
Function NewestFileTime(Const Dir: String): TDateTime;
Var
  sr: TSearchRec;
  d: String;
  t: TDateTime;
Begin
  result := 0;
  // The folder itself too: a temp folder that was just created is still empty
  If FindFirst(ExcludeTrailingPathDelimiter(Dir), faDirectory, sr) = 0 Then Begin
    result := FileDateToDateTime(sr.Time);
    FindClose(sr);
  End;
  d := IncludeTrailingPathDelimiter(Dir);
  If FindFirst(d + '*', faAnyFile, sr) = 0 Then Begin
    Repeat
      If (sr.Name = '.') Or (sr.Name = '..') Then Continue;
      If (sr.Attr And faDirectory) <> 0 Then
        t := NewestFileTime(d + sr.Name)
      Else
        t := FileDateToDateTime(sr.Time);
      If t > result Then result := t;
    Until FindNext(sr) <> 0;
    FindClose(sr);
  End;
End;

Const
  LeftoverAge = 10 / (24 * 60); // 10 minutes: younger leftovers may belong to a generator that is still running

Function IsOldLeftover(Const Dir: String): Boolean;
Begin
  result := Now - NewestFileTime(Dir) > LeftoverAge;
End;

Procedure CleanupSetLeftovers(Const ParentDir: String; Const OnlyName: String);
Var
  sr: TSearchRec;
  d, name: String;
  entries: TStringList;
  i: Integer;
Begin
  d := IncludeTrailingPathDelimiter(ParentDir);
  entries := TStringList.Create;
  Try
    If FindFirst(d + '.*', faDirectory, sr) = 0 Then Begin
      Repeat
        If (sr.Attr And faDirectory) <> 0 Then entries.Add(sr.Name);
      Until FindNext(sr) <> 0;
      FindClose(sr);
    End;
    entries.Sort;
    For i := 0 To entries.Count - 1 Do
      If LeftoverName(entries[i], 'tmp', name) And ((OnlyName = '') Or (name = OnlyName)) And IsOldLeftover(d + entries[i]) Then
        DeleteTree(d + entries[i]);
    For i := 0 To entries.Count - 1 Do
      If LeftoverName(entries[i], 'old', name) And ((OnlyName = '') Or (name = OnlyName)) And IsOldLeftover(d + entries[i]) Then Begin
        If Not DirectoryExists(d + name) Then Begin
          DeleteFile(d + entries[i] + PathDelim + LeftoverMarker);
          RenameFile(d + entries[i], d + name); // crash between the two renames: the old set comes back
        End
        Else
          DeleteTree(d + entries[i]);
      End;
  Finally
    entries.Free;
  End;
End;

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
  fData := TStringList.Create;
  fData.OwnsObjects := true;
  InitCriticalSection(fLock);
End;

Procedure TAGOutput.Retarget(Const aDir: String);
Begin
  fDir := IncludeTrailingPathDelimiter(aDir);
End;

Destructor TAGOutput.Destroy;
Begin
  fImages.Free;
  fTexts.Free;
  fData.Free;
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
    If (CancelAfter > 0) And (done >= CancelAfter) Then Cancelled := true;
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

Function TAGOutput.FindData(Const RelPath: String): TBytes;
Var
  i: Integer;
Begin
  result := Nil;
  EnterCriticalSection(fLock);
  Try
    i := fData.IndexOf(RelPath);
    If i >= 0 Then result := TAGData(fData.Objects[i]).Bytes;
  Finally
    LeaveCriticalSection(fLock);
  End;
End;

Procedure TAGOutput.SaveData(Const RelPath: String; Const Data: TBytes);
Var
  fs: TFileStream;
  d: TAGData;
Begin
  CheckCancel;
  If KeepData Then Begin
    d := TAGData.Create;
    d.Bytes := Copy(Data);
    EnterCriticalSection(fLock);
    Try
      fData.AddObject(RelPath, d);
    Finally
      LeaveCriticalSection(fLock);
    End;
  End;
  If Not DryRun Then Begin
    fs := TFileStream.Create(FullPath(RelPath), fmCreate);
    Try
      If length(Data) > 0 Then fs.WriteBuffer(Data[0], length(Data));
    Finally
      fs.Free;
    End;
  End;
  Written(RelPath);
End;

End.
