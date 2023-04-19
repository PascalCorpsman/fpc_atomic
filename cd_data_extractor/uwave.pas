(******************************************************************************)
(* uwave.pas                                                       ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : FPC implementation to read / write / create .wav files       *)
(*               This unit is nothing than complete, but still usefull and    *)
(*               usable.                                                      *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)


Unit uwave;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  (*
   * Quelle :
   *          http://soundfile.sapp.org/doc/WaveFormat/
   *          https://de.wikipedia.org/wiki/RIFF_WAVE
   *)
  TRIFF = Record
    ChunkSize: uInt32; // Filesize - 8
    Format: String; // Muss immer "WAVE" sein.
  End;

  TFMT = Record
    ID: String; // Muss immer 'fmt ' sein, wenn das nicht der Fall ist, dann ist der Record noch nicht initialisiert
    AudioFormat: uInt16;
    NumChannels: uInt16;
    SampleRate: uInt32; // Frequenz mit der das Wave abgetastet wurde, sollte [44100, 22050]
    ByteRate: uInt32; // = SampleRate * NumChannels * BitsPerSample/8
    BlockAlign: uInt16; // = NumChannels * BitsPerSample/8
    BitsPerSample: uInt16; // Auflösung der Daten in Bit (16 oder 8)
  End;

  (*
   * Usage :
   *         1. Load and Visualize
   *            .create
   *            .loadfromfile / .loadfromstream
   *            for i := 0 to Samplecount - 1 do
   *               .Sample[Channel, i]
   *
   *         2. Create and Store
   *            .create
   *            .InitNewBuffer
   *            .AddSample / .AddMultipleSamples oder .Sample[Channel, i]
   *            .savefromfile / .savetostream
   *)

  { TWave }

  TWave = Class
  private
    fRIFF: TRIFF;
    ffmt: TFMT;
    (*
     * 1- Dimension = Kanal
     * 2- Dimension = X
     *)
    fRawData: Array Of Array Of Single; // Wir speichern Intern alles als 4-Byte Float im Range [-1 .. 1] und Rechnen dann immer entsprechend um ;)

    Function GetChannelCount: uint32;
    Function GetSample(Channel, Index: integer): Single;
    Procedure SetSample(Channel, Index: integer; AValue: Single);
    Function GetSampleCount: uInt32;
    Function GetSampleRate: uInt32;
    Function ReadCheckString(Const Stream: TStream; Data: String): Boolean; // Liest und Prüft ob Data als nächstes als Text im Stream steht.
    Procedure WriteString(Const Stream: TStream; Data: String); // Schreibt den String ohne Längeninformation in den Stream

    Function ReadChunk(Const Stream: TStream; Var Suceed: Boolean): uint32;
    Function LoadRIFFChunk(Const Stream: TStream): Boolean;
    Function ReadFmtChunk(Const Stream: TStream; Var Suceed: Boolean): uint32; // Liest einen FormatChunk, unter der Annahme das die Prüfung aug 'fmt ' bereits erfolgreich war und gelesen wurde.
    Function ReadDataChunk(Const Stream: TStream; Var Suceed: Boolean): uint32; // Liest einen DataChunk, unter der Annahme das die Prüfung aug 'data' bereits erfolgreich war und gelesen wurde, die gelesenen Daten werden an fRawData hinten an gehängt.

    Procedure SaveRiFFChunk(Const Stream: TStream);
    Procedure SaveFmtChunk(Const Stream: TStream);
    Procedure SaveDataChunk(Const Stream: TStream);

    Function ReadSample(Const Stream: TStream): Single;
    Procedure WriteSample(Const Stream: TStream; Value: Single);

    Procedure ClearRawData;
  public
    Property ChannelCount: uint32 read GetChannelCount; // Anzahl der Kanäle (1 = Mono, 2 = Stereo, .. )
    Property SampleRate: uInt32 read GetSampleRate; // Frequenz in derer das Wave Vorliegt (= Samples Pro Sekunde)
    Property SampleCount: uInt32 read GetSampleCount; // Anzahl der Samples
    Property Sample[Channel, Index: integer]: Single read GetSample write SetSample; // Zugriff auf das einzelne Sample

    Constructor Create;
    Destructor Destroy; override;

    Function LoadFromFile(Const Filename: String): Boolean;
    Function LoadFromStream(Const Stream: TStream): Boolean;

    Function SaveToFile(Const Filename: String): Boolean;
    Function SaveToStream(Const Stream: TStream): Boolean;

    (*
     * Initialisiert die Internen Puffer mit einem neuen Datensatz, welcher später Gespeichert werden kann
     * Ist die Anzahl der zu erstellenden Samples Bekannt kann diese zur steigerung der Performance bereits mit angegeben werden.
     * Dann ist der Zugriff via Sample[] zu nutzen.
     *
     * Wenn die Anzahl der Samples nicht bekannt ist, dann muss SampleCount = 0 übergeben werden und die später
     * an zu fügenden Samples via AddSample eingefügt werden.
     *)
    Function InitNewBuffer(NewChannelCount, NewSampleRate, BitsPerSample: integer; NewSampleCount: integer = 0): Boolean;
    (*
     * Fügt ein einzelnes Sample Hinzu das Array Data muss genau so Breit sein wieviele ChannelCount vorhanden sind sonst Fehler
     *)
    Function AddSample(Const Data: Array Of Single): boolean;
    (*
     * Fügt eine Menge An Samples hinzu dabei muss Data ein Vielvaches von ChannelCount sein sonst Fehler
     * Der Aufbau des Arrays muss wie Folgt sein
     *   <Kanal_1_Sample_1_> .. <Kanal_N_Sample_1> .. <Kanal_1_Sample_2_> .. <Kanal_N_Sample_2> .. .. <Kanal_1_Sample_K_> .. <Kanal_N_Sample_K>
     *)
    Function AddMultipleSamples(Const Data: Array Of Single): boolean;
  End;

Implementation

Uses math;

{ TWave }

Constructor TWave.Create;
Begin
  Inherited;
  fRawData := Nil;
  ffmt.ID := '';
  ffmt.SampleRate := 0;
End;

Destructor TWave.Destroy;
Begin
  ClearRawData;
End;

Function TWave.ReadCheckString(Const Stream: TStream; Data: String): Boolean;
Var
  buf: String;
Begin
  result := false;
  buf := '';
  setlength(buf, length(data));
  stream.Read(buf[1], length(data));
  result := buf = data;
End;

Procedure TWave.WriteString(Const Stream: TStream; Data: String);
Var
  b: char;
  i: Integer;
Begin
  For i := 1 To length(data) Do Begin
    b := data[i];
    Stream.Write(b, sizeof(b));
  End;
End;

Function TWave.ReadChunk(Const Stream: TStream; Var Suceed: Boolean): uint32;
Var
  SubChunkID: String;
Begin
  result := 0;
  Suceed := false;
  SubChunkID := '';
  Setlength(SubChunkID, 4);
  stream.Read(SubChunkID[1], 4);
  Case SubChunkID Of
    'fmt ': Begin
        result := ReadFmtChunk(Stream, Suceed) + 4; // Plus die Länge von 'fmt '
      End;
    'data': Begin
        If ffmt.ID <> 'fmt ' Then exit; // Fehler Format wurde noch nicht gelesen
        result := ReadDataChunk(Stream, Suceed) + 4; // Plus die Länge von 'data'
      End
  Else Begin
      // Fehlerfall unbekannte Chunk id
      result := 4; // 4 Bytes haben wir gelesen.
      exit;
    End;
  End;
End;

Function TWave.ReadFmtChunk(Const Stream: TStream; Var Suceed: Boolean): uint32;
Var
  s: uInt32;
Begin
  result := 0;
  Suceed := false;
  ffmt.ID := 'fmt ';
  // Anzahl der Nachfolgenden Bytes, welche den Format Chunk Spezifizieren das sind immer 16 !
  s := 0;
  stream.Read(s, sizeof(s));
  If s <> 16 Then exit;
  // das Format, wir unterstützen nur 1 = Uncompressed
  ffmt.AudioFormat := 2; //-- init mit Ungültig
  stream.Read(ffmt.AudioFormat, sizeof(ffmt.AudioFormat));
  If ffmt.AudioFormat <> 1 Then exit;
  stream.Read(ffmt.NumChannels, sizeof(ffmt.NumChannels));
  stream.Read(ffmt.SampleRate, sizeof(ffmt.SampleRate));
  stream.Read(ffmt.ByteRate, sizeof(ffmt.ByteRate));
  stream.Read(ffmt.BlockAlign, sizeof(ffmt.BlockAlign));
  stream.Read(ffmt.BitsPerSample, sizeof(ffmt.BitsPerSample));
  (*
   * Wenn die Bedingungen schon bekannt sind können wir sie auch prüfen *g*
   *)
  If ffmt.BitsPerSample Mod 8 <> 0 Then exit;
  If ffmt.ByteRate <> (ffmt.SampleRate * ffmt.NumChannels * (ffmt.BitsPerSample Div 8)) Then exit;
  If ffmt.BlockAlign <> (ffmt.NumChannels * (ffmt.BitsPerSample Div 8)) Then exit;
  (*
   * Alles I.O. Dann geben wir das auch Raus
   *)
  Suceed := true;
  result := 24; // Es wurden 16 + 4(=Chunksize) Byte gelesen
  setlength(fRawData, ffmt.NumChannels); // Wir reservieren für die n Kanäle die Einstiegspointer
End;

Procedure TWave.SaveFmtChunk(Const Stream: TStream);
Var
  s: uInt32;
Begin
  WriteString(Stream, 'fmt ');
  s := 16; // Der header ist 16 Byte Groß
  stream.Write(s, sizeof(s));
  stream.Write(ffmt.AudioFormat, sizeof(ffmt.AudioFormat));
  stream.Write(ffmt.NumChannels, sizeof(ffmt.NumChannels));
  stream.Write(ffmt.SampleRate, sizeof(ffmt.SampleRate));
  stream.Write(ffmt.ByteRate, sizeof(ffmt.ByteRate));
  stream.Write(ffmt.BlockAlign, sizeof(ffmt.BlockAlign));
  stream.Write(ffmt.BitsPerSample, sizeof(ffmt.BitsPerSample));
End;

Procedure TWave.SaveDataChunk(Const Stream: TStream);
Var
  s: uInt32;
  i, j: Integer;
Begin
  WriteString(Stream, 'data');
  s := ffmt.NumChannels * Length(fRawData[0]) * (ffmt.BitsPerSample Div 8);
  stream.Write(s, sizeof(s));
  For i := 0 To high(fRawData[0]) Do Begin
    For j := 0 To high(fRawData) Do Begin
      WriteSample(Stream, fRawData[j, i]);
    End;
  End;
End;

Function TWave.ReadDataChunk(Const Stream: TStream; Var Suceed: Boolean
  ): uint32;
Var
  ocnt, cnt, ChunkSize: uInt32;
  i, j: integer;
Begin
  result := 0;
  Suceed := false;
  // Auslesen wieviele Datensätze es gibt
  ChunkSize := 0;
  Stream.Read(ChunkSize, sizeof(ChunkSize));
  If ChunkSize = 0 Then exit;
  // berechnen der Anzahl der Datenpunkte die in diesem Chunk gelesen werden
  cnt := ChunkSize Div (ffmt.NumChannels * (ffmt.BitsPerSample Div 8));
  // Erweitern des Speicherbereiches und init von ocnt
  For i := 0 To ffmt.NumChannels - 1 Do Begin
    ocnt := length(fRawData[i]);
    setlength(fRawData[i], ocnt + cnt);
  End;
  // Auslesen der SampleDaten und entsprechend der Kanäle aufteilen
  For i := 0 To cnt - 1 Do Begin
    For j := 0 To ffmt.NumChannels - 1 Do Begin
      fRawData[j, ocnt + i] := ReadSample(Stream);
    End;
  End;
  Suceed := true;
  result := ChunkSize + 4; // es wurden ChunkSize + 4(=Chunksize) Byte gelesen
End;

Function TWave.ReadSample(Const Stream: TStream): Single;
Var
  i8: uInt8;
  i16: Int16;
Begin
  (*
   * Laden entsprechend der Bitrate und Konvertieren nach [-1 .. 1]
   *)
  result := 0;
  Case ffmt.BitsPerSample Of
    8: Begin
        i8 := 0;
        stream.Read(i8, sizeof(i8));
        result := i8;
        result := result / 127;
      End;
    16: Begin
        i16 := 0;
        Stream.Read(i16, sizeof(i16));
        result := i16;
        result := result / 32767;
      End;
  Else Begin
      Raise Exception.create('BitPerSample value ' + inttostr(ffmt.BitsPerSample) + ' not implemented.');
    End;
  End;
  // Clamp falls doch irgendwo Rundungsfehler sind.
  result := max(-1.0, min(1.0, result));
End;

Procedure TWave.WriteSample(Const Stream: TStream; Value: Single);
Var
  i8: Int8;
  i16: Int16;
Begin
  Case ffmt.BitsPerSample Of
    8: Begin
        i8 := trunc(Value * 127);
        stream.Write(i8, sizeof(i8));
      End;
    16: Begin
        i16 := trunc(Value * 32767);
        stream.Write(i16, sizeof(i16));
      End;
  Else Begin
      Raise Exception.create('BitPerSample value ' + inttostr(ffmt.BitsPerSample) + ' not implemented.');
    End;
  End;
End;

Procedure TWave.ClearRawData;
Var
  i: Integer;
Begin
  For i := 0 To high(fRawData) Do Begin
    setlength(fRawData[i], 0);
  End;
  setlength(fRawData, 0);
End;

Procedure TWave.SetSample(Channel, Index: integer; AValue: Single);
Begin
  If (Channel >= 0) And (Channel <= high(fRawData)) And
    (Index >= 0) And (index <= high(fRawData[Channel])) Then Begin
    aValue := min(1.0, max(-1.0, aValue));
    fRawData[Channel, Index] := AValue;
  End
  Else Begin
    Raise Exception.Create(format('Error invalid index / Channel [%d/%d]', [Channel, Index]));
  End;
End;

Function TWave.LoadRIFFChunk(Const Stream: TStream): Boolean;
Begin
  result := false;
  If Not ReadCheckString(Stream, 'RIFF') Then exit;
  stream.Read(fRIFF.ChunkSize, sizeof(fRIFF.ChunkSize));
  If Not ReadCheckString(Stream, 'WAVE') Then exit;
  fRIFF.Format := 'WAVE';
  result := true;
End;

Procedure TWave.SaveRiFFChunk(Const Stream: TStream);
Begin
  WriteString(Stream, 'RIFF');
  stream.Write(fRIFF.ChunkSize, sizeof(fRIFF.ChunkSize));
  WriteString(Stream, 'WAVE');
End;

Function TWave.GetSampleCount: uInt32;
Begin
  If assigned(fRawData) Then Begin
    result := length(fRawData[0]);
  End
  Else
    result := 0;
End;

Function TWave.GetSampleRate: uInt32;
Begin
  result := ffmt.SampleRate;
End;

Function TWave.GetSample(Channel, Index: integer): Single;
Begin
  If (Channel >= 0) And (Channel <= high(fRawData)) And
    (Index >= 0) And (index <= high(fRawData[Channel])) Then Begin
    result := fRawData[Channel, Index];
  End
  Else Begin
    Raise Exception.Create(format('Error invalid index / Channel [%d/%d]', [Channel, Index]));
  End;
End;

Function TWave.GetChannelCount: uint32;
Begin
  result := length(fRawData);
End;

Function TWave.LoadFromStream(Const Stream: TStream): Boolean;
Var
  BytesToRead: Int64;
Begin
  result := LoadRIFFChunk(Stream);
  If Not result Then exit; // RIFF header nicht erkannt.
  // Lesen der Subchunks so lange bis alle Daten eingelesen wurden.
  ffmt.ID := ''; // Markieren als noch nicht Initialisiert
  ClearRawData;
  BytesToRead := fRIFF.ChunkSize;
  While (BytesToRead > 0) And result Do Begin
    BytesToRead := BytesToRead - ReadChunk(Stream, Result);
  End;
End;

Function TWave.SaveToFile(Const Filename: String): Boolean;
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  result := SaveToStream(m);
  m.SaveToFile(Filename);
  m.free;
End;

Function TWave.SaveToStream(Const Stream: TStream): Boolean;
Begin
  result := false;
  If ffmt.ID = '' Then exit;
  // Die Chunksize der RIFF Datei ( = Filesize - 8 Byte) muss noch berechnet werden, der Rest wurde mit InitNewBuffer bereits gemacht.
  fRIFF.ChunkSize :=
    // RIFF - Header
// Die Fehlenden 8-byte zur Filesize sind der Text 'RIFF' und die RiffChunksize
  4 + // 'WAVE'
  // Format - Chunk
  8 + // 'fmt ' + fmtChunksize (4-Byte)
  16 + // Anzahl Nutzdatenbytes FMT-Chunk
  // Data - Chunk
  8 + // 'data' + dataChungsize (4-byte)
  ffmt.NumChannels * Length(fRawData[0]) * (ffmt.BitsPerSample Div 8) // Speicherverbrauch durch die Daten
  ;
  SaveRiFFChunk(Stream);
  SaveFmtChunk(Stream);
  SaveDataChunk(stream);
  result := true;
End;

Function TWave.InitNewBuffer(NewChannelCount, NewSampleRate,
  BitsPerSample: integer; NewSampleCount: integer): Boolean;
Var
  i: Integer;
Begin
  result := false;
  If (bitsPerSample Mod 8) <> 0 Then exit;
  If (NewChannelCount <= 0) Then exit;
  // Todo : Weitere Checks wären hier wohl angebracht !!
  ClearRawData;
  // Init des neuen ffmt Headers
  ffmt.ID := 'fmt ';
  ffmt.AudioFormat := 1;
  ffmt.NumChannels := NewChannelCount;
  setlength(fRawData, NewChannelCount);
  For i := 0 To NewChannelCount - 1 Do Begin
    setlength(fRawData[i], NewSampleCount);
  End;
  ffmt.SampleRate := NewSampleRate;
  ffmt.ByteRate := NewSampleRate * NewChannelCount * (BitsPerSample Div 8);
  ffmt.BlockAlign := NewChannelCount * (BitsPerSample Div 8);
  ffmt.BitsPerSample := BitsPerSample;
  result := true;
End;

Function TWave.AddSample(Const Data: Array Of Single): boolean;
Var
  j: Integer;
Begin
  result := false;
  If Length(data) <> Length(fRawData) Then exit;
  For j := 0 To high(fRawData) Do Begin
    setlength(fRawData[j], high(fRawData[j]) + 2);
    fRawData[j, high(fRawData[j])] := data[j];
  End;
  result := true;
End;

Function TWave.AddMultipleSamples(Const Data: Array Of Single): boolean;
Var
  c, cnt, olen, j, i: integer;
Begin
  result := false;
  If length(data) Mod length(fRawData) <> 0 Then exit;
  c := length(fRawData); // Anzahl der Kanäle
  cnt := length(data) Div c; // Anzahl der Ein zu fügenden Samples
  For j := 0 To high(fRawData) Do Begin
    oLen := length(fRawData[j]); // Alte Länge für Offset Berechnung
    setlength(fRawData[j], olen + cnt);
    For i := 0 To cnt - 1 Do Begin
      fRawData[j, oLen + i] := data[j + c * i];
    End;
  End;
  result := true;
End;

Function TWave.LoadFromFile(Const Filename: String): Boolean;
Var
  m: TMemoryStream;
Begin
  If Not FileExists(Filename) Then Begin
    result := false;
    exit;
  End;
  m := TMemoryStream.Create;
  m.LoadFromFile(Filename);
  result := LoadFromStream(m);
  m.free;
End;

End.

