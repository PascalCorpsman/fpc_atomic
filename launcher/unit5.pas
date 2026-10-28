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
Unit Unit5;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, IniFiles;

Type

  { TForm5 }

  (*
   * Die Spiel Optionen, die man auch im Optionen Menü des Spiels einstellen kann (gleiche Schlüssel in fpc_atomic.cfg).
   * Was schon im Hauptfenster des Launchers steht (Vollbild, Sound, FPS, Name, Tasten), fehlt hier absichtlich.
   *)
  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Procedure Button1Click(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    fMapDirs: TStringList; // Zu jedem Eintrag in ComboBox4 das Verzeichnis der Karte, '' = Zufallskarte, '?' = gibt es hier nicht
    Procedure SetPlayTime(Value: integer);
    Function GetPlayTime(): integer;
    Procedure SetScheme(Const Value: String);
    Procedure ReadSchemes();
    Procedure ReadMaps();
    Procedure SetMap(Const Value: String);
    Procedure UpdatePreview();
  public
    Procedure LoadFromIni(Const ini: TIniFile);
    Procedure SaveToIni(Const ini: TIniFile);
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Uses ulauncher, math, md5, FileUtil, IntfGraphics, FPImage, uatomic_common, ulocales;

Const
  RandomMapName = 'Random each game'; // Die Zufallskarte des Spiels, gespeichert als Name '' und Hash 0
  MapFiles: Array[0..5] Of String = ('brick.png', 'field.png', 'info.txt', 'solid.png', 'sound.wav', 'xbrick.ani'); // In der Reihenfolge, in der TAtomicField.Create sie hasht
  PreviewScale = 2; // Die Vorschau ist halb so groß wie das Spiel (640 x 480)

Type
  TSchemeRows = Array[0..FieldHeight - 1] Of String;

Function MapComplete(Const Dir: String): Boolean;
Var
  i: integer;
Begin
  // Nur vollständige Karten lädt auch das Spiel
  result := false;
  For i := 0 To high(MapFiles) Do Begin
    If Not FileExists(Dir + MapFiles[i]) Then exit;
  End;
  result := true;
End;

Function MapHash(Const Dir: String): UInt64;
Var
  d: TMD5Digest;
  val: UInt64;
  i, j: integer;
Begin
  // Wie TAtomicField.Create: Von jeder Datei werden beide Hälften des MD5 per Xor eingerechnet
  result := 0;
  For i := 0 To high(MapFiles) Do Begin
    d := MD5File(Dir + MapFiles[i]);
    val := 0;
    For j := 0 To 7 Do Begin
      val := (val Shl 8) Or d[j];
    End;
    result := result Xor val;
    val := 0;
    For j := 8 To 15 Do Begin
      val := (val Shl 8) Or d[j];
    End;
    result := result Xor val;
  End;
End;

Function LoadSchemeRows(Const FileName: String; Out Rows: TSchemeRows): Boolean;
Var
  sl: TStringList;
  sa: TStringArray;
  i, row: integer;
Begin
  // Nur die Steine (-R,Zeile,15 Zeichen aus # : .) wie TGame.LoadSchemeFromFile, mehr braucht die Vorschau nicht
  result := false;
  For i := 0 To high(Rows) Do Begin
    Rows[i] := '';
  End;
  If Not FileExists(FileName) Then exit;
  sl := TStringList.Create;
  sl.LoadFromFile(FileName);
  For i := 0 To sl.Count - 1 Do Begin
    If pos('-R,', sl[i]) <> 1 Then Continue;
    sa := sl[i].Split(',');
    If length(sa) <> 3 Then Continue;
    row := StrToIntDef(trim(sa[1]), -1);
    If (row < 0) Or (row > high(Rows)) Or (length(trim(sa[2])) <> FieldWidth) Then Continue;
    Rows[row] := trim(sa[2]);
    result := true;
  End;
  sl.free;
End;

Function LoadPng(Const FileName: String): TLazIntfImage;
Var
  png: TPortableNetworkGraphic;
Begin
  png := TPortableNetworkGraphic.Create;
  png.LoadFromFile(FileName);
  result := png.CreateIntfImage;
  png.free;
End;

Procedure DrawTile(Const Field, Tile: TLazIntfImage; x, y: integer);
Var
  i, j: integer;
  c: TFPColor;
Begin
  // Fuchsia ist durchsichtig, wie beim Laden der Kacheln im Spiel (LoadAlphaColorGraphik)
  For j := 0 To min(Tile.Height, FieldBlockHeight) - 1 Do Begin
    For i := 0 To min(Tile.Width, FieldBlockWidth) - 1 Do Begin
      c := Tile.Colors[i, j];
      If (c.red Shr 8 = 255) And (c.green Shr 8 = 0) And (c.blue Shr 8 = 255) Then Continue;
      Field.Colors[x + i, y + j] := c;
    End;
  End;
End;

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Var
  i: integer;
Begin
  caption := 'Game options';
  fMapDirs := TStringList.Create;
  // Die Spielzeit wie im Spiel: unendlich oder 90 .. 600 s in 30 s Schritten
  ComboBox3.Items.Clear;
  ComboBox3.Items.Add('Infinity');
  For i := 3 To 20 Do Begin
    ComboBox3.Items.Add(format('%d:%0.2d', [(i * 30) Div 60, (i * 30) Mod 60]));
  End;
End;

Procedure TForm5.ReadSchemes;
Var
  sr: TSearchRec;
  sl: TStringList;
Begin
  // Alle Schemes, die im Spiel zur Auswahl stehen (jedes Mal neu, der CD Extraktor kann sie inzwischen angelegt haben)
  sl := TStringList.Create;
  sl.Sorted := true;
  If FindFirst('data' + PathDelim + 'schemes' + PathDelim + '*', faAnyFile, sr) = 0 Then Begin
    Repeat
      If uppercase(ExtractFileExt(sr.Name)) = '.SCH' Then sl.Add(sr.Name);
    Until FindNext(sr) <> 0;
  End;
  FindClose(sr);
  ComboBox2.Items.Assign(sl);
  sl.free;
End;

Procedure TForm5.FormDestroy(Sender: TObject);
Begin
  fMapDirs.free;
End;

Procedure TForm5.ReadMaps;
Var
  sl: TStringList;
  ini: TIniFile;
  dir, aName: String;
  i: integer;
Begin
  // Alle vollständigen Karten mit ihrem Namen aus info.txt, die Zufallskarte ist wie im Spiel die letzte
  ComboBox4.Items.Clear;
  fMapDirs.Clear;
  sl := FindAllDirectories('data' + PathDelim + 'maps', false);
  sl.Sort;
  For i := 0 To sl.Count - 1 Do Begin
    dir := IncludeTrailingPathDelimiter(sl[i]);
    If Not MapComplete(dir) Then Continue;
    ini := TIniFile.Create(dir + 'info.txt');
    aName := ini.ReadString('General', 'Name', '');
    ini.free;
    If aName = '' Then Continue; // Name '' ist für die Zufallskarte reserviert
    ComboBox4.Items.Add(aName);
    fMapDirs.Add(dir);
  End;
  sl.free;
  ComboBox4.Items.Add(Tr(RandomMapName));
  fMapDirs.Add('');
End;

Procedure TForm5.SetMap(Const Value: String);
Var
  i: integer;
Begin
  If Value = '' Then Begin
    ComboBox4.ItemIndex := fMapDirs.IndexOf('');
    exit;
  End;
  i := ComboBox4.Items.IndexOf(Value);
  // Eine Karte, die es hier nicht (mehr) gibt, bleibt auswählbar und wird beim Speichern nicht angefasst
  If i = -1 Then Begin
    ComboBox4.Items.Add(Value);
    fMapDirs.Add('?');
    i := ComboBox4.Items.Count - 1;
  End;
  ComboBox4.ItemIndex := i;
End;

Procedure TForm5.UpdatePreview;
Var
  Rows: TSchemeRows;
  Field, Brick, Solid, Preview: TLazIntfImage;
  bmp: TBitmap;
  dir, Info: String;
  i, j, x, y, r, g, b: integer;
  c: TFPColor;
Begin
  (*
   * Die Karte mit allen Steinen des Schemes, halb so groß wie im Spiel. Im Spiel fehlt je nach Steindichte
   * des Schemes zufällig ein Teil der Steine, und die Startfelder der Spieler werden frei geräumt.
   *)
  LoadSchemeRows('data' + PathDelim + 'schemes' + PathDelim + ComboBox2.Text, Rows);
  dir := '?';
  If ComboBox4.ItemIndex >= 0 Then dir := fMapDirs[ComboBox4.ItemIndex];
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.SetSize(640 Div PreviewScale, 480 Div PreviewScale);
  If (dir <> '') And (dir <> '?') And MapComplete(dir) Then Begin
    Field := LoadPng(dir + 'field.png');
    Brick := LoadPng(dir + 'brick.png');
    Solid := LoadPng(dir + 'solid.png');
    For j := 0 To FieldHeight - 1 Do Begin
      For i := 0 To FieldWidth - 1 Do Begin
        If length(Rows[j]) <> FieldWidth Then Continue;
        x := FieldxOff + i * FieldBlockWidth;
        y := FieldyOff + j * FieldBlockHeight;
        Case Rows[j][i + 1] Of
          ':': DrawTile(Field, Brick, x, y);
          '#': DrawTile(Field, Solid, x, y);
        End;
      End;
    End;
    // Verkleinern, dabei je PreviewScale x PreviewScale Pixel mitteln (StretchDraw ließe einfach Pixel weg)
    Preview := bmp.CreateIntfImage;
    For y := 0 To Preview.Height - 1 Do Begin
      For x := 0 To Preview.Width - 1 Do Begin
        r := 0;
        g := 0;
        b := 0;
        For j := 0 To PreviewScale - 1 Do Begin
          For i := 0 To PreviewScale - 1 Do Begin
            c := Field.Colors[min(Field.Width - 1, x * PreviewScale + i), min(Field.Height - 1, y * PreviewScale + j)];
            r := r + c.red;
            g := g + c.green;
            b := b + c.blue;
          End;
        End;
        c.red := r Div (PreviewScale * PreviewScale);
        c.green := g Div (PreviewScale * PreviewScale);
        c.blue := b Div (PreviewScale * PreviewScale);
        c.alpha := alphaOpaque;
        Preview.Colors[x, y] := c;
      End;
    End;
    bmp.LoadFromIntfImage(Preview);
    Preview.free;
    Field.free;
    Brick.free;
    Solid.free;
  End
  Else Begin
    // Zufallskarte oder keine Kartengrafik: nur das Raster des Schemes
    bmp.Canvas.Brush.Color := $00203020;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
    For j := 0 To FieldHeight - 1 Do Begin
      For i := 0 To FieldWidth - 1 Do Begin
        If length(Rows[j]) <> FieldWidth Then Continue;
        Case Rows[j][i + 1] Of
          ':': bmp.Canvas.Brush.Color := $003060A0;
          '#': bmp.Canvas.Brush.Color := clGray;
        Else
          Continue;
        End;
        x := (FieldxOff + i * FieldBlockWidth) Div PreviewScale;
        y := (FieldyOff + j * FieldBlockHeight) Div PreviewScale;
        bmp.Canvas.FillRect(x, y, x + FieldBlockWidth Div PreviewScale - 1, y + FieldBlockHeight Div PreviewScale - 1);
      End;
    End;
    If dir = '' Then Begin
      Info := Tr(RandomMapName);
    End
    Else Begin
      Info := Tr('No map graphics, extract the CD data first.');
    End;
    bmp.Canvas.Brush.Style := bsClear;
    bmp.Canvas.Font.Color := clWhite;
    bmp.Canvas.TextOut(FieldxOff Div PreviewScale, 8, Info);
  End;
  Image1.Picture.Assign(bmp);
  bmp.free;
End;

Procedure TForm5.ComboBox2Change(Sender: TObject);
Begin
  // Gemeinsam für Scheme und Karte
  UpdatePreview();
End;

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  // Defaults, die selben wie im Spiel (client/unit1.pas Load_Atomic_Settings)
  CheckBox1.Checked := false;
  CheckBox2.Checked := true;
  CheckBox3.Checked := false;
  SetScheme('BASIC.SCH');
  SetMap(''); // Ohne gespeicherte Karte nimmt das Spiel die Zufallskarte (Name '' und Hash 0)
  SetPlayTime(2 * 60 + 30);
  SpinEdit2.Value := 3;
  ComboBox1.ItemIndex := 1;
  SpinEdit1.Value := 5521;
  SpinEdit3.Value := 100;
  CheckBox4.Checked := true;
  UpdatePreview();
End;

Procedure TForm5.SetPlayTime(Value: integer);
Begin
  // 0 = unendlich, sonst der nächste Eintrag in 30 s Schritten
  If Value <= 0 Then Begin
    ComboBox3.ItemIndex := 0;
  End
  Else Begin
    ComboBox3.ItemIndex := min(18, max(1, round(Value / 30) - 2));
  End;
End;

Function TForm5.GetPlayTime: integer;
Begin
  If ComboBox3.ItemIndex <= 0 Then Begin
    result := 0;
  End
  Else Begin
    result := (ComboBox3.ItemIndex + 2) * 30;
  End;
End;

Procedure TForm5.SetScheme(Const Value: String);
Begin
  // Ein Scheme, das es nicht (mehr) gibt, bleibt trotzdem auswählbar, damit nichts verloren geht
  If ComboBox2.Items.IndexOf(Value) = -1 Then ComboBox2.Items.Add(Value);
  ComboBox2.ItemIndex := ComboBox2.Items.IndexOf(Value);
End;

Procedure TForm5.LoadFromIni(Const ini: TIniFile);
Begin
  ReadSchemes();
  ReadMaps();
  CheckBox1.Checked := ini.ReadBool(FPC_AtomicIniSection, 'TeamPlay', false);
  CheckBox2.Checked := ini.ReadBool(FPC_AtomicIniSection, 'RandomStart', true);
  CheckBox3.Checked := ini.ReadBool(FPC_AtomicIniSection, 'LostPlayersRevertToAI', false);
  SetScheme(ini.ReadString(FPC_AtomicIniSection, 'SchemeFile', 'BASIC.SCH'));
  SetMap(ini.ReadString(FPC_AtomicIniSection, 'LastPlayedField', ''));
  SetPlayTime(ini.ReadInteger(FPC_AtomicIniSection, 'PlayTime', 2 * 60 + 30));
  SpinEdit2.Value := ini.ReadInteger(FPC_AtomicIniSection, 'LastWinsToWinMatch', 3);
  ComboBox1.ItemIndex := min(2, max(0, ini.ReadInteger(FPC_AtomicIniSection, 'Conveyorspeed', 1)));
  SpinEdit1.Value := ini.ReadInteger(FPC_AtomicIniSection, 'Port', 5521);
  SpinEdit3.Value := ini.ReadInteger(FPC_AtomicIniSection, 'Volume', 10000) Div 100;
  CheckBox4.Checked := ini.ReadBool(FPC_AtomicIniSection, 'PlaySoundEffects', true);
  UpdatePreview();
End;

Procedure TForm5.SaveToIni(Const ini: TIniFile);
Var
  dir: String;
Begin
  ini.WriteBool(FPC_AtomicIniSection, 'TeamPlay', CheckBox1.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'RandomStart', CheckBox2.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'LostPlayersRevertToAI', CheckBox3.Checked);
  ini.WriteString(FPC_AtomicIniSection, 'SchemeFile', ComboBox2.Text);
  // Die Karte wie im Spiel mit Name und Hash, eine hier unbekannte Karte bleibt wie sie ist
  If ComboBox4.ItemIndex >= 0 Then Begin
    dir := fMapDirs[ComboBox4.ItemIndex];
    If dir = '' Then Begin
      ini.WriteString(FPC_AtomicIniSection, 'LastPlayedField', '');
      ini.WriteString(FPC_AtomicIniSection, 'LastPlayedFieldHash', '0');
    End
    Else If dir <> '?' Then Begin
      ini.WriteString(FPC_AtomicIniSection, 'LastPlayedField', ComboBox4.Text);
      ini.WriteString(FPC_AtomicIniSection, 'LastPlayedFieldHash', IntToStr(MapHash(dir)));
    End;
  End;
  ini.WriteInteger(FPC_AtomicIniSection, 'PlayTime', GetPlayTime());
  ini.WriteInteger(FPC_AtomicIniSection, 'LastWinsToWinMatch', SpinEdit2.Value);
  ini.WriteInteger(FPC_AtomicIniSection, 'Conveyorspeed', ComboBox1.ItemIndex);
  ini.WriteInteger(FPC_AtomicIniSection, 'Port', SpinEdit1.Value);
  // Das Spiel ändert die Lautstärke in 5 % Schritten (0 .. 10000)
  ini.WriteInteger(FPC_AtomicIniSection, 'Volume', SpinEdit3.Value * 100);
  ini.WriteBool(FPC_AtomicIniSection, 'PlaySoundEffects', CheckBox4.Checked);
End;

End.
