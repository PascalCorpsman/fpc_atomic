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
 *   https://github.com/mmatyas/ab_aniex         -> decoding .ani files
 *   http://www.paulbourke.net/dataformats/tga/  -> decoding .tga files
 *)
Unit uanifile;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Type

  size_t = uint32; // wer weis das schon so genau, aber bezogen auf die Damalige Zeit macht nur 32-Bit Sinn.

  CImg = Record
    typ: uint16;
    width: uint16;
    height: uint16;
    hotspot_x: uint16;
    hotspot_y: uint16;
    keycolor_bytes: uint16;
    bpp: uint16;
    compressed_size: size_t;
    uncompressed_size: size_t;
  End;

  TPixel = Record
    r, g, b, a: uint8;
  End;

  { TTGA }

  TTGA = Class
  private
    TGA_HEADER_SIZE: size_t;
    bpp: UInt16;
    width: UInt16;
    height: UInt16;
    palette_data: Array Of uint8;
    palette_size: size_t;
    data: Array Of uint8;
    data_size: size_t;
    uncompressedData: Array Of TPixel;
    Procedure UnCompressData();
  public
    Constructor Create(Const img: CImg); virtual;
    Destructor Destroy(); override;
    Procedure SaveToStream(Const Stream: TStream);
    Function AsTBitmap(): TBitmap;
  End;

  { TFileItem }

  TFileItem = Object
    // from file
    signature_bytes: Array[0..3] Of uint8;
    len: UInt32;
    id: UInt16;

    // calculated
    signature: String;
    start_pos: Int64;
    end_pos: Int64;

    Procedure Read(Const Stream: TStream);
  End;


  tAniImage = Record
    Name: String;
    Bitmap: TBitmap;
  End;

  { TAniFile }

  TAniFile = Class
  private
    Fimages: Array Of tAniImage;
    Function getImage(Index: integer): tAniImage;
    Function getImageCount: Integer;
    Function ParseCImg(Const Stream: TStream; Const frame_item: TFileItem): TTGA;
    Procedure ParseFrame(Const Stream: TStream; Const Ani_Item: TFileItem);
    Procedure Clear;
  public
    Property ImageCount: Integer read getImageCount;
    Property Image[Index: integer]: tAniImage read getImage;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    (*
     * Load a Atomic Bomberman .ani file and make its images accessable via "Image", "ImageCount" and "ImageIndexByName"
     *)
    Function LoadFromFile(Const Filename: String): Boolean;

    Function ImageIndexByName(ImageName: String): integer; // -1 if not found
  End;

Implementation

Uses
  LCLType, IntfGraphics, fpImage;

{ TTGA }

Constructor TTGA.Create(Const img: CImg);
Begin
  Inherited Create;
  TGA_HEADER_SIZE := 54;
  bpp := img.bpp;
  width := img.width;
  height := img.height;
  palette_data := Nil;
  palette_size := 0;
  data := Nil;
  data_size := img.compressed_size;
  If img.bpp < 16 Then Begin
    palette_size := 4 * (1 Shl img.bpp);
    setlength(palette_data, palette_size);
  End;
  setlength(data, data_size);
  uncompressedData := Nil;
End;

Destructor TTGA.Destroy;
Begin
  setlength(data, 0);
  setlength(palette_data, 0);
  setlength(uncompressedData, 0);
End;

Procedure TTGA.UnCompressData;
  Function PacketToPixel(Value: Array Of uint8): TPixel;
  Begin
    result.r := (Value[1] And $7C) Shl 1;
    result.g := ((Value[1] And $03) Shl 6) Or ((Value[0] And $E0) Shr 2);
    result.b := (Value[0] And $1F) Shl 3;
    result.a := (Value[1] And $80);
    If result.a <> 0 Then Begin
      result.a := 255;
    End;
  End;

Var
  m: TMemoryStream;
  n, i, j: Integer;
  PacketHeader: uint8;
  Packet: Array[0..1] Of uint8; // Normally 2 .. 4 bytes, but as you can see in SaveToStream we use a hard coded 16-Bits ;)
  p: TPixel;
Begin
  (*
   * according to http://www.paulbourke.net/dataformats/tga/
   *
   * the header from SaveToStream says that the content is :
   *  10  -  Runlength encoded RGB images.
   * with 16-Bit Pixels
   * this means that each pixel ist stored as
   * |            |              |
   *  <ggg><bbbbb> <a><rrrrr><gg>
   * little endian 16-Bit value.
   *)
  setlength(uncompressedData, width * height);
  fillchar(uncompressedData[0], sizeof(uncompressedData), 0);
  m := TMemoryStream.Create;
  m.Write(data[0], length(data) * sizeof(UInt8));
  m.Position := 0;
  n := 0;
  PacketHeader := 0;
  Packet[0] := 0;
  Packet[1] := 0;
  While n < width * height Do Begin
    m.read(PacketHeader, SizeOf(PacketHeader));
    m.Read(Packet[0], sizeof(Packet));
    p := PacketToPixel(packet);
    j := (PacketHeader And $7F);
    uncompressedData[n] := p; // 1. Element
    inc(n);
    If (PacketHeader And $80) = $80 Then Begin // RLE-Packet
      For i := 0 To j - 1 Do Begin // die weiteren
        uncompressedData[n] := p;
        inc(n);
      End;
    End
    Else Begin // Normal-Packets
      For i := 0 To j - 1 Do Begin // die weiteren
        m.Read(Packet[0], sizeof(Packet));
        p := PacketToPixel(packet);
        uncompressedData[n] := p;
        inc(n);
      End;
    End;
  End;
  If n <> length(uncompressedData) Then Begin
    Raise Exception.Create('Could not decompress.');
  End;
  m.free;
End;

Procedure TTGA.SaveToStream(Const Stream: TStream);
Var
  header: Array[0..11] Of UInt8 =
  (
    0, // idlength
    0, // colourmaptype
    10, // datatypecode
    0, // colourmaporigin
    0, // colourmaporigin
    0, // colourmaplength
    0, // colourmaplength
    0, // colourmapdepth
    0, // x_origin
    0, // x_origin
    0, // y_origin
    0 // y_origin
    );
  u8: uint8;
Begin
  stream.Write(header, SizeOf(header));
  Stream.Write(width, SizeOf(width)); // width
  Stream.Write(height, SizeOf(height)); // height
  u8 := 16;
  Stream.Write(u8, sizeof(u8)); // bitsperpixel = 16
  // Targa Image Descriptor Byte
  //   bit 5: screen origin bit
  u8 := $20;
  Stream.Write(u8, sizeof(u8)); // imagedescriptor
  Stream.Write(data[0], length(data));
End;

Function TTGA.AsTBitmap: TBitmap;

  Function IndexToFPColor(index: integer): TFPColor;
  Begin
    result.Red := uncompressedData[index].r Shl 8;
    result.Green := uncompressedData[index].g Shl 8;
    result.Blue := uncompressedData[index].b Shl 8;
    result.Alpha := uncompressedData[index].a Shl 8;
  End;

Var
  Bitmap: TBitmap;
  j, i: Integer;
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  // 1. alles Auspacken
  UnCompressData();
  // 2. Alles in ein TBitmap umwandeln
  Bitmap := TBitmap.Create;
  Bitmap.Width := width;
  Bitmap.Height := height;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  For j := 0 To bitmap.height - 1 Do Begin
    For i := 0 To bitmap.width - 1 Do Begin
      TempIntfImg.Colors[i, j] := IndexToFPColor(j * width + i);
    End;
  End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  Bitmap.Transparent := false;
  result := Bitmap;
  TempIntfImg.free;
End;

{ TFileItem }

Procedure TFileItem.Read(Const Stream: TStream);
Begin
  Stream.read(signature_bytes, 4);
  Stream.Read(len, sizeof(len));
  Stream.Read(id, sizeof(id));
  signature := chr(signature_bytes[0]) + chr(signature_bytes[1]) + chr(signature_bytes[2]) + chr(signature_bytes[3]);
  start_pos := Stream.Position;
  end_pos := start_pos + len;
End;

{ TAniFile }

Function TAniFile.ParseCImg(Const Stream: TStream; Const frame_item: TFileItem
  ): TTGA;
Var
  img: CImg;
  dummy32, additional_size: uint32;
  has_palette_header: Boolean;
  palette_size: size_t;
  dummy16: uint16;
  dummy8: uint8;
Begin
  If frame_item.len < 32 Then Begin
    Raise exception.Create('CIMG is too small!');
  End;
  //
  // basic info (8B)
  //
  img.typ := 0;
  stream.Read(img.typ, sizeof(img.typ));
  dummy16 := 0;
  stream.Read(dummy16, sizeof(dummy16)); // unknown
  additional_size := 0;
  stream.Read(additional_size, sizeof(additional_size));
  If additional_size < 24 Then Begin
    Raise Exception.Create('CIMG special header is too small!');
  End;
  If additional_size > frame_item.end_pos - Stream.Position Then Begin
    Raise Exception.Create('CIMG size mismatch!');
  End;
  has_palette_header := false;
  palette_size := 0;
  If additional_size <> 0 Then Begin
    additional_size := additional_size - 24;
    has_palette_header := additional_size > 0;
    palette_size := additional_size;
  End
  Else Begin
    Raise exception.create('CIMG invalid additional_size');
  End;
  //
  // image meta (16B)
  //
  dummy32 := 0;
  stream.Read(dummy32, sizeof(dummy32)); // unknown
  stream.Read(img.width, sizeof(img.width));
  stream.Read(img.height, sizeof(img.height));
  stream.Read(img.hotspot_x, sizeof(img.hotspot_x));
  stream.Read(img.hotspot_y, sizeof(img.hotspot_y));
  stream.Read(img.keycolor_bytes, sizeof(img.keycolor_bytes));
  stream.Read(dummy16, sizeof(dummy16)); // unknown
  Case img.typ Of
    $04: img.bpp := 10;
    $05: Begin
        img.bpp := 24;
        If (palette_size > 0) Then Begin
          Raise Exception.Create('CIMG type 5 cannot have palette!');
        End;
      End;
    $0A: Begin
        img.bpp := 4;
        If (palette_size <> 64) Then Begin
          Raise Exception.Create('CIMG type 10 must have 64 byte long header!');
        End;
      End;
    $0B: Begin
        If (palette_size <> 1024) Then Begin
          Raise Exception.Create('CIMG type 11 must have 1024 byte long header!');
        End;
      End;
  Else Begin
      Raise Exception.Create('CIMG has unknown image type!');
    End;
  End;

  //  printf("info: found an image: %d x %d @ %dbpp\n", img.width, img.height, img.bpp);
  //  if (img.type != 0x04)
  //    printf("warning: image is not in the default format, export may fail\n");

  //
  // palette data (vary)
  //
  If (has_palette_header) Then Begin
    (*
     * We ignore the pallete anyway, so just skip it.
     *)
    stream.Position := stream.Position + additional_size;
  End;

  //
  // special header (vary)
  //
  stream.Read(dummy16, sizeof(dummy16)); // unknown
  stream.Read(dummy16, sizeof(dummy16)); // unknown
  dummy32 := 0;
  stream.Read(dummy32, sizeof(dummy32));
  img.compressed_size := dummy32 - 12;
  dummy32 := 0;
  stream.Read(dummy32, sizeof(dummy32));
  img.uncompressed_size := dummy32;

  //
  // Read Image Data
  //
  result := TTGA.Create(img);
  stream.Read(result.data[0], result.data_size);

  //
  // item end
  //
  If (stream.Position + 1 > frame_item.end_pos) Then Begin
    // no terminator
  End
  Else If (stream.Position + 1 < frame_item.end_pos) Then Begin
    dummy8 := 0;
    Stream.Read(dummy8, SizeOf(dummy8));
    stream.Position := stream.Position + (frame_item.end_pos - Stream.Position - 1);
  End
  Else Begin
    dummy8 := 0;
    Stream.Read(dummy8, SizeOf(dummy8));
    If (dummy8 <> 255) Then Begin
      Stream.Read(dummy8, SizeOf(dummy8));
    End;
  End;
End;

Function TAniFile.getImageCount: Integer;
Begin
  result := Length(Fimages);
End;

Function TAniFile.getImage(Index: integer): tAniImage;
Begin
  result := Fimages[index];
End;

Procedure TAniFile.ParseFrame(Const Stream: TStream; Const Ani_Item: TFileItem);
Var
  has_name: Boolean;
  item: TFileItem;
  name: String;
  Cimg: TTGA;
Begin
  has_name := false;
  cimg := Nil;
  While (Stream.Position < Ani_Item.end_pos) Do Begin
    item.Read(Stream);
    Case item.signature Of
      'FNAM': Begin // Brauchen wir nicht, aber schadet auch nicht ;)
          name := '';
          setlength(name, item.len);
          Stream.Read(name[1], item.len);
          name := trim(name);
          has_name := true;
        End;
      'CIMG': Begin
          If assigned(Cimg) Then Begin
            Raise Exception.Create('Error multiple CIMG in frame.');
          End;
          cimg := ParseCImg(Stream, item);

        End;
    Else Begin
        Stream.Position := Stream.Position + item.len;
      End;
    End;
  End;
  If assigned(cimg) And has_name Then Begin
    setlength(Fimages, high(Fimages) + 2);
    Fimages[high(Fimages)].Name := name;
    Fimages[high(Fimages)].Bitmap := Cimg.AsTBitmap();
  End;
  If assigned(cimg) Then
    cimg.Free;
End;

Procedure TAniFile.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(Fimages) Do Begin
    Fimages[i].Bitmap.Free;
  End;
  setlength(Fimages, 0);
End;

Constructor TAniFile.Create;
Begin
  Inherited create;
  Fimages := Nil;
End;

Destructor TAniFile.Destroy;
Begin
  Clear;
End;

Function TAniFile.LoadFromFile(Const Filename: String): Boolean;
Var
  m: TMemoryStream;
  signature: String;
  file_end, file_length: uint32;
  file_id: uint16;
  item: TFileItem;
Begin
  result := false;
  Clear;
  m := TMemoryStream.Create;
  m.LoadFromFile(Filename);
  signature := '';
  setlength(signature, 10);
  m.Read(signature[1], 10);
  If signature <> 'CHFILEANI ' Then Begin
    m.free;
    exit;
  End;
  file_length := 0;
  m.read(file_length, sizeof(file_length));
  file_id := 0;
  m.read(file_id, sizeof(file_id));
  file_end := file_length;
  While (m.Position < file_end) Do Begin
    item.Read(m);
    If item.signature = 'FRAM' Then Begin
      ParseFrame(m, item);
    End
    Else Begin
      m.Position := m.Position + item.len;
    End;
  End;
  m.free;
  result := true;
End;

Function TAniFile.ImageIndexByName(ImageName: String): integer;
Var
  i: Integer;
Begin
  result := -1;
  ImageName := LowerCase(ImageName);
  For i := 0 To high(Fimages) Do Begin
    If lowercase(Fimages[i].Name) = ImageName Then Begin
      result := i;
      break;
    End;
  End;
End;

End.

