(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
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
 * Inspired by:
 *   https://www.fileformat.info/format/pcx/egff.htm
 *)
Unit upcx;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Type

  (*
   * This PCX-Loader is far away from beeing a valid full complete PCX-Loader
   * all it has to do is load a .pcx file from Atomic Bomberman CD and convert
   * it into a .bmp file.
   *)
  { TPCX }

  TPCX = Class
  private
    fHasColorTable: Boolean; // Switch between fEGAColors and fColorTable
    fColorTable: Array[0..255] Of TColor;
    fEGAColors: Array[0..15] Of TColor;
    fData: Array Of Byte; // The uncompressed data of color indexes
    fWidth, fheight: integer;
    Procedure InitEGAColors(Const Palette: Array Of uint8);
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;
    Procedure LoadFromFile(Const FileName: String);
    Function AsTBitmap(): TBitmap;
  End;

Implementation

Uses
  LCLType, IntfGraphics, fpImage;

Type

  TPCXHeader = Packed Record
    Identifier: uint8; // PCX Id Number (Always 0x0A)
    Version: uint8; // Version Number
    Encoding: uint8; // Encoding Format
    BitsPerPixel: uint8; // Bits per Pixel
    XStart: uint16; // Left of image
    YStart: uint16; // Top of Image
    XEnd: uint16; // Right of Image
    YEnd: uint16; // Bottom of image
    HorzRes: uint16; // Horizontal Resolution
    VertRes: uint16; // Vertical Resolution
    Palette: Array[0..48 - 1] Of uint8; // 16-Color EGA Palette
    Reserved1: uint8; // Reserved (Always 0)
    NumBitPlanes: uint8; // Number of Bit Planes
    BytesPerLine: uint16; // Bytes per Scan-line
    PaletteType: uint16; // Palette Type
    HorzScreenSize: uint16; // Horizontal Screen Size
    VertScreenSize: uint16; // Vertical Screen Size
    Reserved2: Array[0..54 - 1] Of uint8; // Reserved (Always 0)
  End;

  { TPCX }

Procedure TPCX.InitEGAColors(Const Palette: Array Of uint8);
Var
  i: Integer;
Begin
  // The Palette is coded in "RGB"
  // Convert to 16 TColor values "BGR"
  For i := 0 To 15 Do Begin
    fEGAColors[i] := Palette[i * 3] Or (Palette[i * 3 + 1] Shl 8) Or (Palette[i * 3 + 2] Shl 16);
  End;
End;

Constructor TPCX.Create;
Begin
  Inherited Create;
  fData := Nil;
End;

Destructor TPCX.Destroy;
Begin
  setlength(fData, 0);
End;

Procedure TPCX.LoadFromFile(Const FileName: String);
Var
  m: TMemoryStream;
  //MaxNumberOfColors: integer;
  //ScanLineLength: integer;
  //LinePaddingSize,
  index, i: integer;
  r, g, b, byte, runcount, runValue: uint8;
  Header: TPCXHeader;
Begin
  m := TMemoryStream.Create;
  m.LoadFromFile(FileName);
  If m.size < sizeof(TPCXHeader) Then Begin
    m.free;
    Raise exception.create('Error invalid filesize');
  End;
  Header.Identifier := 0;
  m.Read(Header, SizeOf(Header));
  If Header.Identifier <> $0A Then Begin
    m.free;
    Raise exception.create('Error invalid file, header mismatch');
  End;
  If Header.Encoding <> 1 Then Begin
    m.free;
    Raise exception.create('Error invalid file, unknown encoding');
  End;
  fWidth := Header.XEnd - Header.XStart + 1;
  fheight := Header.YEnd - Header.YStart + 1;
  //MaxNumberOfColors := (1 Shl (Header.BitsPerPixel * Header.NumBitPlanes));
  //ScanLineLength := Header.BytesPerLine * Header.NumBitPlanes;
  //LinePaddingSize := ((Header.BytesPerLine * Header.NumBitPlanes) * (8 Div Header.BitsPerPixel)) - ((Header.XEnd - Header.XStart) + 1);
  setlength(fData, fWidth * fheight);
  index := 0;
  Repeat
    byte := 0;
    m.Read(byte, sizeof(byte));
    If ((byte And $C0) = $C0) Then Begin // 2-Byte Code
      runcount := byte And $3F;
      runValue := 0;
      m.Read(runValue, sizeof(runValue));
    End
    Else Begin // 1-Byte Code
      runcount := 1;
      runValue := byte;
    End;
    For i := 0 To runcount - 1 Do Begin
      fData[index] := runValue;
      inc(index);
    End;
  Until (index >= length(fData));
  (*
   * if there are now at least 768 bytes "unread" -> This is the color table
   * read and use it !
   *)
  If (m.Position + 3 * 256 + 1) <= m.Size Then Begin
    m.Position := m.Size - 3 * 256;
    fHasColorTable := true;
    r := 0;
    g := 0;
    b := 0;
    For i := 0 To 255 Do Begin
      m.Read(r, sizeof(r));
      m.Read(g, sizeof(g));
      m.Read(b, sizeof(b));
      fColorTable[i] := (r) Or (g Shl 8) Or (b Shl 16);
    End;
  End
  Else Begin
    // TODO: Untested Code here
    InitEGAColors(Header.Palette);
    fHasColorTable := false;
    For i := 0 To 255 Do Begin
      fColorTable[i] := 0;
    End;
  End;
  m.free;
End;

Function TPCX.AsTBitmap: TBitmap;

  Function IndexToFPColor(index: integer): TFPColor;
  Var
    col: integer;
    c: TColor;
    r, g, b: integer;
  Begin
    col := fData[index];
    If fHasColorTable Then Begin
      // Color Table
      c := fColorTable[col];
    End
    Else Begin
      // Ega Colors
      // TODO: geht das so einfach ?
      c := fEGAColors[col];
    End;
    r := (c And $FF) Shl 8;
    g := (c And $FF00);
    b := (c And $FF0000) Shr 8;
    result.Red := r;
    result.Green := g;
    result.Blue := b;
    result.Alpha := 255 Shl 8;
  End;

Var
  Bitmap: TBitmap;
  j, i: Integer;
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  // 2. Alles in ein TBitmap umwandeln
  Bitmap := TBitmap.Create;
  Bitmap.Width := fwidth;
  Bitmap.Height := fheight;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  For j := 0 To bitmap.height - 1 Do Begin
    For i := 0 To bitmap.width - 1 Do Begin
      TempIntfImg.Colors[i, j] := IndexToFPColor(j * fwidth + i);
    End;
  End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  Bitmap.Transparent := false;
  result := Bitmap;
  TempIntfImg.free;
End;

End.

