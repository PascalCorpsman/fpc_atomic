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
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    Procedure Button1Click(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure ComboBox7Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Label9Click(Sender: TObject);
  private
    fMapDirs: TStringList; // Zu jedem Eintrag in ComboBox4 das Verzeichnis der Karte, '' = Zufallskarte, '?' = gibt es hier nicht
    fAssetFolders: TStringList; // Zu jedem Eintrag in ComboBox5 der Ordner des Grafiksatzes unter assets/, '' = Original
    fSoundFolders: TStringList; // Zu jedem Eintrag in ComboBox6 der Wert von [Launcher] SoundSet: SoundSetAuto, '' = Original, sonst der Ordner
    fAiLibs: TStringList; // Zu jedem Eintrag in ComboBox7 der Name der KI für den Server (Parameter -ai), 'ai' = Original
    Procedure ReadAiLibs(Const Selected: String);
    Procedure SetPlayTime(Value: integer);
    Function GetPlayTime(): integer;
    Procedure SetScheme(Const Value: String);
    Procedure ReadSchemes();
    Procedure ReadMaps();
    Procedure ReadAssetSets(Const Selected: String);
    Procedure ReadSoundSets(Const Selected: String);
    Function MapGfx(Const Dir, Filename: String): String;
    Function MapGfxComplete(Const Dir: String): Boolean;
    Procedure SetMap(Const Value: String);
    Procedure UpdatePreview();
  public
    Procedure LoadFromIni(Const ini: TIniFile);
    Procedure SaveToIni(Const ini: TIniFile);
  End;

Var
  Form5: TForm5;

Function AiLibDisplayName(Const LibName: String): String; // Der Name einer KI für die Anzeige, LibName ohne Endung ('ai_smart')

Implementation

{$R *.lfm}

Uses ulauncher, math, md5, FileUtil, IntfGraphics, FPImage, dynlibs, uatomic_common, ulanguage, ulayout, uassetpath, usmartsettings;

Const
  OriginalAiLib = 'ai'; // ai.dll / libai.so
  SmartAiLib = 'ai_smart'; // ai_smart.dll / libai_smart.so, die einzige KI mit Schwierigkeit (ai_smart.ini)

Const
  MapGfxFiles: Array[0..3] Of String = ('brick.png', 'field.png', 'solid.png', 'xbrick.ani');
  PreviewScale = 2; // Die Vorschau ist halb so groß wie das Spiel (640 x 480)

Type
  TSchemeRows = Array[0..FieldHeight - 1] Of String;

Function AiLibDisplayName(Const LibName: String): String;
Type
  TAiDisplayName = Function(): pchar; cdecl;
Var
  Lib: TLibHandle;
  f: TAiDisplayName;
Begin
  // Die bekannten KIs übersetzt, sonst der Name aus der KI (optionaler Export AiDisplayName), sonst der Dateiname
  If LibName = OriginalAiLib Then exit(R_AiOriginal);
  If LibName = SmartAiLib Then exit(R_AiSmart);
  result := LibName;
{$IFDEF Windows}
  Lib := dynlibs.LoadLibrary(AppDir() + LibName + '.dll');
{$ELSE}
  Lib := dynlibs.LoadLibrary(AppDir() + 'lib' + LibName + '.so');
{$ENDIF}
  If Lib = NilHandle Then exit;
  Try
    f := TAiDisplayName(dynlibs.GetProcAddress(Lib, 'AiDisplayName'));
    If assigned(f) And (trim(String(f())) <> '') Then result := trim(String(f()));
  Except
    result := LibName;
  End;
  dynlibs.UnloadLibrary(Lib);
End;

Function MapHash(Const Dir: String): UInt64;
Var
  d: TMD5Digest;
  val: UInt64;
  j: integer;
Begin
  // Wie TAtomicField.loadFromDirectory: Nur info.txt, beide Hälften des MD5 werden per Xor eingerechnet
  result := 0;
  d := MD5File(Dir + 'info.txt');
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
  fAssetFolders := TStringList.Create;
  fSoundFolders := TStringList.Create;
  fAiLibs := TStringList.Create;
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
  fAssetFolders.free;
  fSoundFolders.free;
  fAiLibs.free;
End;

Procedure TForm5.ReadAiLibs(Const Selected: String);
Var
  sr: TSearchRec;
  n, Prefix, Ext: String;
  i: integer;
Begin
  // Alle KIs im Programmordner: ai.dll (Original) und ai_*.dll, unter Linux libai.so und libai_*.so
{$IFDEF Windows}
  Prefix := '';
  Ext := '.dll';
{$ELSE}
  Prefix := 'lib';
  Ext := '.so';
{$ENDIF}
  fAiLibs.Clear;
  ComboBox7.Items.Clear;
  fAiLibs.Add(OriginalAiLib);
  ComboBox7.Items.Add(R_AiOriginal);
  If FindFirst(AppDir() + Prefix + OriginalAiLib + '_*' + Ext, faAnyFile, sr) = 0 Then Begin
    Repeat
      n := copy(sr.Name, length(Prefix) + 1, length(sr.Name) - length(Prefix) - length(Ext));
      If fAiLibs.IndexOf(n) <> -1 Then Continue;
      fAiLibs.Add(n);
      ComboBox7.Items.Add(AiLibDisplayName(n));
    Until FindNext(sr) <> 0;
  End;
  FindClose(sr);
  ComboBox7.ItemIndex := 0; // Unbekannt oder nicht installiert: der Server nimmt auch die Original KI
  For i := 1 To fAiLibs.Count - 1 Do
    If fAiLibs[i] = trim(Selected) Then ComboBox7.ItemIndex := i;
  ComboBox7Change(Nil);
End;

Procedure TForm5.ComboBox7Change(Sender: TObject);
Begin
  // Die Schwierigkeit gibt es nur für die Smart KI
  SpinEdit4.Enabled := (ComboBox7.ItemIndex >= 0) And (ComboBox7.ItemIndex < fAiLibs.Count) And
    (fAiLibs[ComboBox7.ItemIndex] = SmartAiLib);
  Label13.Enabled := SpinEdit4.Enabled;
End;

Procedure TForm5.ReadAssetSets(Const Selected: String);
Var
  Names: TStringList;
  i: integer;
Begin
  // Original und alle Grafiksätze in assets/ (jedes Mal neu, der Generator kann inzwischen welche angelegt haben)
  Names := TStringList.Create;
  ListAssetSets(fAssetFolders, Names);
  fAssetFolders.Insert(0, '');
  Names.Insert(0, R_AssetSetOriginal);
  ComboBox5.Items.Assign(Names);
  Names.free;
  ComboBox5.ItemIndex := 0;
  For i := 1 To fAssetFolders.Count - 1 Do Begin
    If SameFolderName(fAssetFolders[i], Selected) Then ComboBox5.ItemIndex := i;
  End;
End;

Procedure TForm5.ReadSoundSets(Const Selected: String);
Var
  Names: TStringList;
  i: integer;
Begin
  // Automatisch, Original und alle Sätze in assets/, die Sounds haben
  Names := TStringList.Create;
  ListSoundSets(fSoundFolders, Names);
  fSoundFolders.Insert(0, '');
  Names.Insert(0, R_SoundSetOriginal);
  fSoundFolders.Insert(0, SoundSetAuto);
  Names.Insert(0, R_SoundSetAuto);
  ComboBox6.Items.Assign(Names);
  Names.free;
  ComboBox6.ItemIndex := 1; // Ein unbekannter Satz (gelöscht oder ohne Sounds) ist im Spiel auch Original
  If SameText(trim(Selected), SoundSetAuto) Then ComboBox6.ItemIndex := 0;
  For i := 2 To fSoundFolders.Count - 1 Do Begin
    If SameFolderName(fSoundFolders[i], trim(Selected)) Then ComboBox6.ItemIndex := i;
  End;
End;

Function TForm5.MapGfx(Const Dir, Filename: String): String;
Var
  Folder: String;
Begin
  // Die Kartengrafik aus dem gewählten Grafiksatz, wenn es sie dort gibt, sonst die aus data/maps (wie im Spiel)
  result := Dir + Filename;
  If (ComboBox5.ItemIndex <= 0) Or (ComboBox5.ItemIndex >= fAssetFolders.Count) Then exit;
  Folder := AssetSetsDir() + fAssetFolders[ComboBox5.ItemIndex] + PathDelim + 'maps' + PathDelim +
    ExtractFileName(ExcludeTrailingPathDelimiter(Dir)) + PathDelim + Filename;
  If FileExists(Folder) Then result := Folder;
End;

Function TForm5.MapGfxComplete(Const Dir: String): Boolean;
Var
  i: integer;
Begin
  // Wie im Spiel: alle Grafiken aus dem Grafiksatz oder aus data/maps, sound.wav ist optional
  result := false;
  For i := 0 To high(MapGfxFiles) Do Begin
    If Not FileExists(MapGfx(Dir, MapGfxFiles[i])) Then exit;
  End;
  result := true;
End;

Procedure TForm5.FormShow(Sender: TObject);
Begin
  FitLayout(self); // Platz für die Texte im aktuellen Widgetset und in der aktuellen Sprache
End;

Procedure TForm5.Label9Click(Sender: TObject);
Begin
  If SameText(ExtractFileExt(Language.CurrentLanguage), '.de') Then Begin
    OpenManual('GAMEOPTIONS_MANUAL_DE.md');
  End
  Else Begin
    OpenManual('GAMEOPTIONS_MANUAL.md');
  End;
End;

Procedure TForm5.ReadMaps;
Var
  sl: TStringList;
  ini: TIniFile;
  dir, aName: String;
  i: integer;
Begin
  // Alle Karten mit ihrem Namen aus info.txt (wie im Spiel, die Grafiken können aus dem Grafiksatz kommen), die Zufallskarte ist wie im Spiel die letzte
  ComboBox4.Items.Clear;
  fMapDirs.Clear;
  sl := FindAllDirectories('data' + PathDelim + 'maps', false);
  sl.Sort;
  For i := 0 To sl.Count - 1 Do Begin
    dir := IncludeTrailingPathDelimiter(sl[i]);
    If Not FileExists(dir + 'info.txt') Then Continue;
    ini := TIniFile.Create(dir + 'info.txt');
    aName := ini.ReadString('General', 'Name', '');
    ini.free;
    If aName = '' Then Continue; // Name '' ist für die Zufallskarte reserviert
    ComboBox4.Items.Add(aName);
    fMapDirs.Add(dir);
  End;
  sl.free;
  ComboBox4.Items.Add(R_RandomMap);
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
  If (dir <> '') And (dir <> '?') And MapGfxComplete(dir) Then Begin
    Field := LoadPng(MapGfx(dir, 'field.png'));
    Brick := LoadPng(MapGfx(dir, 'brick.png'));
    Solid := LoadPng(MapGfx(dir, 'solid.png'));
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
      Info := R_RandomMap;
    End
    Else Begin
      Info := R_NoMapGraphics;
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
  ComboBox5.ItemIndex := 0;
  ComboBox6.ItemIndex := 0; // Automatisch
  ComboBox7.ItemIndex := 0; // Original KI
  ComboBox7Change(Nil);
  SpinEdit4.Value := DefaultDifficulty;
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
Var
  smart: TIniFile;
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
  ReadAssetSets(ini.ReadString('Launcher', 'AssetSet', ''));
  ReadSoundSets(ini.ReadString('Launcher', 'SoundSet', SoundSetAuto));
  ReadAiLibs(ini.ReadString('Launcher', 'AiLib', ''));
  // Die Schwierigkeit steht in ai_smart.ini (die liest die Smart KI selbst)
  SpinEdit4.Value := DefaultDifficulty;
  If FileExists(SmartIniFile()) Then Begin
    smart := TIniFile.Create(SmartIniFile());
    Try
      SpinEdit4.Value := max(0, min(100, smart.ReadInteger('General', 'Difficulty', DefaultDifficulty)));
    Finally
      smart.free;
    End;
  End;
  UpdatePreview();
End;

Procedure TForm5.SaveToIni(Const ini: TIniFile);
Var
  dir: String;
  smart: TIniFile;
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
  // Der Grafiksatz, die selbe Einstellung wie die Pfeile unter dem Bild im Hauptfenster
  If (ComboBox5.ItemIndex >= 0) And (ComboBox5.ItemIndex < fAssetFolders.Count) Then Begin
    ini.WriteString('Launcher', 'AssetSet', fAssetFolders[ComboBox5.ItemIndex]);
  End;
  // Die Sounds, unabhängig vom Grafiksatz (SoundSetAuto, '' = Original oder ein Ordner unter assets/)
  If (ComboBox6.ItemIndex >= 0) And (ComboBox6.ItemIndex < fSoundFolders.Count) Then Begin
    ini.WriteString('Launcher', 'SoundSet', fSoundFolders[ComboBox6.ItemIndex]);
  End;
  // Die KI eines gehosteten Servers ('' = Original), das Spiel gibt sie dem Server als -ai mit
  If (ComboBox7.ItemIndex > 0) And (ComboBox7.ItemIndex < fAiLibs.Count) Then
    ini.WriteString('Launcher', 'AiLib', fAiLibs[ComboBox7.ItemIndex])
  Else
    ini.WriteString('Launcher', 'AiLib', '');
  // Die Schwierigkeit in ai_smart.ini, gibt es die noch nicht: mit allen Werten und Kommentaren anlegen (wie die KI selbst)
  If SpinEdit4.Enabled Then Begin
    Try
      If FileExists(SmartIniFile()) Then Begin
        smart := TIniFile.Create(SmartIniFile());
        Try
          smart.WriteInteger('General', 'Difficulty', SpinEdit4.Value);
        Finally
          smart.free;
        End;
      End
      Else Begin
        WriteDefaults(SmartIniFile(), SpinEdit4.Value);
      End;
    Except
      On e: Exception Do
        showmessage(e.Message);
    End;
  End;
End;

End.
