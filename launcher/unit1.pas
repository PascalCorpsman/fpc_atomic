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
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniFiles, UTF8Process, ulauncher;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
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
    ComboBox1: TComboBox;
    Panel1: TPanel;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure Button10Click(Sender: TObject);
    Procedure Button12Click(Sender: TObject);
    Procedure Button13Click(Sender: TObject);
    Procedure Label8Click(Sender: TObject);
    Procedure Label9Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure CheckBox3Change(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Panel1MouseEnter(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    ini: TIniFile;
    OnlineVersion: Single; // Von der Prüfung beim Start, -1 = unbekannt
    OnlineLauncherVersion: integer; // Von der Prüfung beim Start, -1 = unbekannt
    QuietLog: Boolean; // True -> loggen ohne das Log Fenster zu zeigen (Prüfung beim Start)
    ProtocollVersion: integer;
    Version: Single;
    UpToDate: Boolean; // True, wenn "Updates..." in diesem Lauf nichts mehr zum Herunterladen gefunden hat
    GameDataFound: Boolean; // True, wenn CheckForFiles nichts vermisst, dann ist der Launch Knopf blau
    CanGenerate: Boolean; // True, wenn Spieldaten fehlen und der Grafik-Generator einen Satz erzeugen kann (Button13), dann ist der Launch Knopf auch blau
    GameProcess: TProcessUTF8; // Das laufende Spiel, wenn der Launcher danach wieder erscheinen soll (CheckBox6), sonst Nil
    fLanguageCodes: Array Of String;
    fAssetFolders: TStringList; // Die Grafiksätze, [0] = '' = Original, sonst der Ordner unter assets/
    fAssetNames: TStringList; // Dazu die Namen aus profile.ini
    fAssetIndex: integer; // Der gewählte Grafiksatz in fAssetFolders

    Procedure LoadLanguages();
    Procedure LoadSettings();
    Procedure UpdateStatus();
    Procedure UpdateModsCaption();
    Procedure UpdateGameModeCaption();
    Procedure StoreSettings();
    Procedure ApplyLanguage();

    Procedure LoadSideImage();
    Procedure ReadAssetSets();
    Procedure SelectAssetSet(Index: integer);
    Function AssetGeneratorExe(): String;
    Function AssetGeneratorCliExe(): String;
    Procedure StartAssetGenerator();
    Function SelectedAssetFolder(): String;
    Function SelectedSoundSet(): String;
    Function MissingGameData(): TStringList;
    Function SelectUsableAssetSet(): Boolean;
    Procedure PrepareSchemes();
    Function GenerateAssetSet(): Boolean;
    Procedure OnDoLog(Const LogText: String);
    Procedure OnStartCheckDone(Ok: Boolean; Const Filename, ErrorText: String);
    Function GetCheckUpdatesOnStart: Boolean;
    Procedure SetCheckUpdatesOnStart(Value: Boolean);
  public
    Procedure StoreVersion(NewVersion: single);
    Property CheckUpdatesOnStart: Boolean read GetCheckUpdatesOnStart write SetCheckUpdatesOnStart;

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses Math, process, Unit2, Unit3, Unit4, Unit5, Unit6, LCLType, lclintf
  , ukeyboarddialog, uatomic_common, uatomic_global, ulanguage, ulayout, uupdatecheck, uassetpath
{$IFDEF Windows}
  , LResources
  , ssl_openssl_lib, ssl_openssl, blcksock
{$ENDIF}
  ;

Const
  LauncherLanguageCodes: Array[0..3] Of String = ('en', 'de', 'fr', 'es');

{$IFDEF Windows}

Function CheckAndMaybeExtract(Const RessourceDLL: String): Boolean;
Var
  st: TLazarusResourceStream;
Begin
  result := FileExists(RessourceDLL + '.dll');
  If Not result Then Begin
    // https://wiki.freepascal.org/Lazarus_Resources
    st := TLazarusResourceStream.Create(RessourceDLL, Nil);
    Try
      st.SaveToFile(ExtractFilePath(ParamStr(0)) + RessourceDLL + '.dll');
      result := true;
    Except
      On av: exception Do Begin
        log(av.Message);
        st.free;
        exit;
      End;
    End;
    st.free;
  End;
End;
{$ENDIF}

Procedure ClearLog();
Begin
  form2.Memo1.Clear;
  If FileExists(GetAtomicAppLogFile()) Then
    DeleteFile(GetAtomicAppLogFile());
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
  fm: TFormatSettings;
Begin
  InitLogger();
  LoggerSetOnDoLog(@OnDoLog);
  // Hints erst nach 1 s zeigen (Standard 0,5 s), damit sie nicht stören, dafür länger stehen lassen
  Application.HintPause := 1000;
  Application.HintHidePause := 10000;
  ini := TIniFile.Create(GetAtomicConfigFile());
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  UpToDate := false;
  fAssetFolders := TStringList.Create;
  fAssetNames := TStringList.Create;
  fAssetIndex := 0;
  OnlineVersion := -1;
  OnlineLauncherVersion := -1;
  QuietLog := false;
  // Vom Updater übrig gelassen, der alte Launcher
  If FileExists(ParamStr(0) + '.old') Then DeleteFile(ParamStr(0) + '.old');
  LoadSideImage();
  LoadLanguages();
  LoadSettings(); // Liest auch den Grafiksatz und zeigt sein Seitenbild (LoadSideImage)
  // Ohne CD-Daten: einen brauchbaren (generierten) Satz wählen, falls es einen gibt, und dessen Schemes nach data/schemes kopieren
  SelectUsableAssetSet();
  PrepareSchemes();
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  caption := format('FPC Atomic Launcher, ver. %0.2f', [LauncherVersion / 100], fm);
  // Handle Launcher Parameters
  For i := 1 To ParamCount Do Begin
    If ParamStr(i) = '-cti' Then CheckBox3.Checked := true;
    If ParamStr(i) = '-ip' Then edit2.text := ParamStr(i + 1);
    If ParamStr(i) = '-port' Then edit3.text := ParamStr(i + 1);
  End;
  CheckBox3Change(Nil);
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  StoreSettings();
  close;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  P: TProcessUTF8;
Begin
  // run cd data extractor
  StoreSettings();
  ini.UpdateFile;
  // Short Prechecks
  If (Not FileExists('cd_data_extractor'{$IFDEF Windows} + '.exe'{$ENDIF})) Or
    (Not FileExists('cd_data_extractor'{$IFDEF Windows} + '.exe'{$ENDIF})) Then Begin
    showmessage(R_InstallationIncomplete);
    exit;
  End;
  // Run the App ;)
  p := TProcessUTF8.Create(Nil);
  p.Executable := 'cd_data_extractor'{$IFDEF Windows} + '.exe'{$ENDIF};
  p.Options := p.Options + [poWaitOnExit];
  p.Execute;
  p.free;
  UpdateStatus();
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  Dialog: TKeyboardDialog;
  Keys: Array[TKeySet] Of TKeys;
Begin
  // Key Assignment
  dialog := TKeyboardDialog.CreateNew(self, 0);
  Keys[ks0] := AtomicDefaultKeys(ks0);
  Keys[ks0].KeyUp := ini.ReadInteger(FPC_AtomicIniSection, 'KeyUp', Keys[ks0].KeyUp);
  Keys[ks0].KeyDown := ini.ReadInteger(FPC_AtomicIniSection, 'KeyDown', Keys[ks0].KeyDown);
  Keys[ks0].KeyLeft := ini.ReadInteger(FPC_AtomicIniSection, 'KeyLeft', Keys[ks0].KeyLeft);
  Keys[ks0].KeyRight := ini.ReadInteger(FPC_AtomicIniSection, 'KeyRight', Keys[ks0].KeyRight);
  Keys[ks0].KeyPrimary := ini.ReadInteger(FPC_AtomicIniSection, 'KeyPrimary', Keys[ks0].KeyPrimary);
  Keys[ks0].KeySecondary := ini.ReadInteger(FPC_AtomicIniSection, 'KeySecondary', Keys[ks0].KeySecondary);
  Keys[ks0].UseSDL2 := ini.ReadBool(FPC_AtomicIniSection, 'UseSDL', Keys[ks0].UseSDL2);
  If Keys[ks0].UseSDL2 Then Begin
    Keys[ks0].Name := ini.ReadString(FPC_AtomicIniSection, 'SDL_Name', Keys[ks0].Name);
    Keys[ks0].NameIndex := ini.readInteger(FPC_AtomicIniSection, 'SDL_NameIndex', Keys[ks0].NameIndex);
    Keys[ks0].IsGameController := ini.ReadBool(FPC_AtomicIniSection, 'SDL_IsGameController', Keys[ks0].IsGameController);
    Keys[ks0].ButtonIndex[0] := ini.readInteger(FPC_AtomicIniSection, 'SDL_First', Keys[ks0].ButtonIndex[0]);
    Keys[ks0].ButtonsIdle[0] := ini.readBool(FPC_AtomicIniSection, 'SDL_FirstIdle', Keys[ks0].ButtonsIdle[0]);
    Keys[ks0].ButtonIndex[1] := ini.readInteger(FPC_AtomicIniSection, 'SDL_Second', Keys[ks0].ButtonIndex[1]);
    Keys[ks0].ButtonsIdle[1] := ini.readBool(FPC_AtomicIniSection, 'SDL_SecondIdle', Keys[ks0].ButtonsIdle[1]);
    Keys[ks0].AchsisIndex[0] := ini.readInteger(FPC_AtomicIniSection, 'SDL_UpDown', Keys[ks0].AchsisIndex[0]);
    Keys[ks0].AchsisIdle[0] := ini.readInteger(FPC_AtomicIniSection, 'SDL_UpDownIdle', Keys[ks0].AchsisIdle[0]);
    Keys[ks0].AchsisDirection[0] := ini.readInteger(FPC_AtomicIniSection, 'SDL_UpDownDirection', Keys[ks0].AchsisDirection[0]);
    Keys[ks0].AchsisIndex[1] := ini.readInteger(FPC_AtomicIniSection, 'SDL_LeftRight', Keys[ks0].AchsisIndex[1]);
    Keys[ks0].AchsisIdle[1] := ini.readInteger(FPC_AtomicIniSection, 'SDL_LeftRightIdle', Keys[ks0].AchsisIdle[1]);
    Keys[ks0].AchsisDirection[1] := ini.readInteger(FPC_AtomicIniSection, 'SDL_LeftRightDirection', Keys[ks0].AchsisDirection[1]);
    Keys[ks0].UseAnalogStick := ini.ReadBool(FPC_AtomicIniSection, 'SDL_AnalogStick', Keys[ks0].UseAnalogStick);
    Keys[ks0].UseHat := ini.ReadBool(FPC_AtomicIniSection, 'SDL_Hat', Keys[ks0].UseHat);
    Keys[ks0].UseHysteresis := ini.ReadBool(FPC_AtomicIniSection, 'SDL_Hysteresis', Keys[ks0].UseHysteresis);
    Keys[ks0].UseDominantAxis := ini.ReadBool(FPC_AtomicIniSection, 'SDL_DominantAxis', Keys[ks0].UseDominantAxis);
    Keys[ks0].MappingVersion := ini.ReadInteger(FPC_AtomicIniSection, 'SDL_MappingVersion', Keys[ks0].MappingVersion);
  End;
  Keys[ks1] := AtomicDefaultKeys(ks1);
  Keys[ks1].KeyUp := ini.ReadInteger(FPC_AtomicIniSection, 'KeyUp2', Keys[ks1].KeyUp);
  Keys[ks1].KeyDown := ini.ReadInteger(FPC_AtomicIniSection, 'KeyDown2', Keys[ks1].KeyDown);
  Keys[ks1].KeyLeft := ini.ReadInteger(FPC_AtomicIniSection, 'KeyLeft2', Keys[ks1].KeyLeft);
  Keys[ks1].KeyRight := ini.ReadInteger(FPC_AtomicIniSection, 'KeyRight2', Keys[ks1].KeyRight);
  Keys[ks1].KeyPrimary := ini.ReadInteger(FPC_AtomicIniSection, 'KeyPrimary2', Keys[ks1].KeyPrimary);
  Keys[ks1].KeySecondary := ini.ReadInteger(FPC_AtomicIniSection, 'KeySecondary2', Keys[ks1].KeySecondary);
  Keys[ks1].UseSDL2 := ini.ReadBool(FPC_AtomicIniSection, 'UseSDL2', Keys[ks1].UseSDL2);
  If Keys[ks1].UseSDL2 Then Begin
    Keys[ks1].Name := ini.ReadString(FPC_AtomicIniSection, 'SDL_Name2', Keys[ks1].Name);
    Keys[ks1].NameIndex := ini.readInteger(FPC_AtomicIniSection, 'SDL_NameIndex2', Keys[ks1].NameIndex);
    Keys[ks1].IsGameController := ini.ReadBool(FPC_AtomicIniSection, 'SDL_IsGameController2', Keys[ks1].IsGameController);
    Keys[ks1].ButtonIndex[0] := ini.readInteger(FPC_AtomicIniSection, 'SDL_First2', Keys[ks1].ButtonIndex[0]);
    Keys[ks1].ButtonsIdle[0] := ini.readBool(FPC_AtomicIniSection, 'SDL_FirstIdle2', Keys[ks1].ButtonsIdle[0]);
    Keys[ks1].ButtonIndex[1] := ini.readInteger(FPC_AtomicIniSection, 'SDL_Second2', Keys[ks1].ButtonIndex[1]);
    Keys[ks1].ButtonsIdle[1] := ini.readBool(FPC_AtomicIniSection, 'SDL_SecondIdle2', Keys[ks1].ButtonsIdle[1]);
    Keys[ks1].AchsisIndex[0] := ini.readInteger(FPC_AtomicIniSection, 'SDL_UpDown2', Keys[ks1].AchsisIndex[0]);
    Keys[ks1].AchsisIdle[0] := ini.readInteger(FPC_AtomicIniSection, 'SDL_UpDownIdle2', Keys[ks1].AchsisIdle[0]);
    Keys[ks1].AchsisDirection[0] := ini.readInteger(FPC_AtomicIniSection, 'SDL_UpDownDirection2', Keys[ks1].AchsisDirection[0]);
    Keys[ks1].AchsisIndex[1] := ini.readInteger(FPC_AtomicIniSection, 'SDL_LeftRight2', Keys[ks1].AchsisIndex[1]);
    Keys[ks1].AchsisIdle[1] := ini.readInteger(FPC_AtomicIniSection, 'SDL_LeftRightIdle2', Keys[ks1].AchsisIdle[1]);
    Keys[ks1].AchsisDirection[1] := ini.readInteger(FPC_AtomicIniSection, 'SDL_LeftRightDirection2', Keys[ks1].AchsisDirection[1]);
    Keys[ks1].UseAnalogStick := ini.ReadBool(FPC_AtomicIniSection, 'SDL_AnalogStick2', Keys[ks1].UseAnalogStick);
    Keys[ks1].UseHat := ini.ReadBool(FPC_AtomicIniSection, 'SDL_Hat2', Keys[ks1].UseHat);
    Keys[ks1].UseHysteresis := ini.ReadBool(FPC_AtomicIniSection, 'SDL_Hysteresis2', Keys[ks1].UseHysteresis);
    Keys[ks1].UseDominantAxis := ini.ReadBool(FPC_AtomicIniSection, 'SDL_DominantAxis2', Keys[ks1].UseDominantAxis);
    Keys[ks1].MappingVersion := ini.ReadInteger(FPC_AtomicIniSection, 'SDL_MappingVersion2', Keys[ks1].MappingVersion);
  End;
  Dialog.LoadKeys(Keys[ks0], Keys[ks1]);
  If Dialog.Execute() Then Begin
    Keys[ks0] := Dialog.GetKeys(ks0);
    Keys[ks1] := Dialog.GetKeys(ks1);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyUp', Keys[ks0].KeyUp);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyDown', Keys[ks0].KeyDown);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyLeft', Keys[ks0].KeyLeft);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyRight', Keys[ks0].KeyRight);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyPrimary', Keys[ks0].KeyPrimary);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeySecondary', Keys[ks0].KeySecondary);
    ini.WriteBool(FPC_AtomicIniSection, 'UseSDL', Keys[ks0].UseSDL2);
    ini.WriteString(FPC_AtomicIniSection, 'SDL_Name', Keys[ks0].Name);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_NameIndex', Keys[ks0].NameIndex);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_IsGameController', Keys[ks0].IsGameController);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_First', Keys[ks0].ButtonIndex[0]);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_FirstIdle', Keys[ks0].ButtonsIdle[0]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_Second', Keys[ks0].ButtonIndex[1]);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_SecondIdle', Keys[ks0].ButtonsIdle[1]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_UpDown', Keys[ks0].AchsisIndex[0]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_UpDownIdle', Keys[ks0].AchsisIdle[0]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_UpDownDirection', Keys[ks0].AchsisDirection[0]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_LeftRight', Keys[ks0].AchsisIndex[1]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_LeftRightIdle', Keys[ks0].AchsisIdle[1]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_LeftRightDirection', Keys[ks0].AchsisDirection[1]);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_AnalogStick', Keys[ks0].UseAnalogStick);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_Hat', Keys[ks0].UseHat);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_Hysteresis', Keys[ks0].UseHysteresis);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_DominantAxis', Keys[ks0].UseDominantAxis);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_MappingVersion', Keys[ks0].MappingVersion);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyUp2', Keys[ks1].KeyUp);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyDown2', Keys[ks1].KeyDown);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyLeft2', Keys[ks1].KeyLeft);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyRight2', Keys[ks1].KeyRight);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeyPrimary2', Keys[ks1].KeyPrimary);
    ini.WriteInteger(FPC_AtomicIniSection, 'KeySecondary2', Keys[ks1].KeySecondary);
    ini.WriteBool(FPC_AtomicIniSection, 'UseSDL2', Keys[ks1].UseSDL2);
    ini.WriteString(FPC_AtomicIniSection, 'SDL_Name2', Keys[ks1].Name);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_NameIndex2', Keys[ks1].NameIndex);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_IsGameController2', Keys[ks1].IsGameController);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_First2', Keys[ks1].ButtonIndex[0]);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_FirstIdle2', Keys[ks1].ButtonsIdle[0]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_Second2', Keys[ks1].ButtonIndex[1]);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_SecondIdle2', Keys[ks1].ButtonsIdle[1]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_UpDown2', Keys[ks1].AchsisIndex[0]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_UpDownIdle2', Keys[ks1].AchsisIdle[0]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_UpDownDirection2', Keys[ks1].AchsisDirection[0]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_LeftRight2', Keys[ks1].AchsisIndex[1]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_LeftRightIdle2', Keys[ks1].AchsisIdle[1]);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_LeftRightDirection2', Keys[ks1].AchsisDirection[1]);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_AnalogStick2', Keys[ks1].UseAnalogStick);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_Hat2', Keys[ks1].UseHat);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_Hysteresis2', Keys[ks1].UseHysteresis);
    ini.WriteBool(FPC_AtomicIniSection, 'SDL_DominantAxis2', Keys[ks1].UseDominantAxis);
    ini.WriteInteger(FPC_AtomicIniSection, 'SDL_MappingVersion2', Keys[ks1].MappingVersion);
  End;
  dialog.free;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  // Mods, der Dialog liest und schreibt atomic_mods.ini selbst
  form4.ShowModal;
  UpdateModsCaption();
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  // Spiel Optionen, die selben Schlüssel wie im Optionen Menü des Spiels
  form5.LoadFromIni(ini);
  If form5.ShowModal = mrOK Then Begin
    form5.SaveToIni(ini);
    ini.UpdateFile;
    ReadAssetSets(); // Der Grafiksatz kann sich im Dialog geändert haben
  End;
End;

Procedure TForm1.Button10Click(Sender: TObject);
Begin
  // Gemeinsam für die Pfeile unter dem Bild: Button10 = voriger, Button11 = nächster Grafiksatz
  If fAssetFolders.Count = 0 Then exit;
  If Sender = Button11 Then Begin
    SelectAssetSet((fAssetIndex + 1) Mod fAssetFolders.Count);
  End
  Else Begin
    SelectAssetSet((fAssetIndex + fAssetFolders.Count - 1) Mod fAssetFolders.Count);
  End;
End;

Procedure TForm1.Button12Click(Sender: TObject);
Begin
  (*
   * Gemeinsam für Grafiken... und den Doppelklick auf das Bild: öffnet den Grafik-Generator mit dem gewählten Satz.
   * Ohne Generator passiert nichts, ein einfacher Klick auf das Bild schaltet absichtlich nicht (er käme vor jedem Doppelklick).
   *)
  StartAssetGenerator();
End;

Procedure TForm1.Button13Click(Sender: TObject);
Begin
  // Nur sichtbar, wenn Spieldaten fehlen: einen Grafiksatz erzeugen, ohne das Spiel zu starten (das macht der Launch Knopf)
  GenerateAssetSet();
End;

Procedure TForm1.Button9Click(Sender: TObject);
Begin
  // Spielmodus, der Dialog liest und schreibt atomic_mods.ini selbst, Team play nur für seinen Hinweis
  form6.TeamPlay := ini.ReadBool(FPC_AtomicIniSection, 'TeamPlay', false);
  form6.ShowModal;
  UpdateGameModeCaption();
End;

Procedure TForm1.Label8Click(Sender: TObject);
Begin
  If SameText(ExtractFileExt(Language.CurrentLanguage), '.de') Then Begin
    OpenManual('MANUAL_DE.md');
  End
  Else Begin
    OpenManual('MANUAL.md');
  End;
End;

Procedure TForm1.Label9Click(Sender: TObject);
Var
  s: String;
Begin
  // Open Config folder
  s := GetAtomicConfigFile();
  s := ExtractFileDir(s);
  OpenURL(s);
End;

Procedure TForm1.CheckBox3Change(Sender: TObject);
Begin
  // Router IP und Port braucht es nur, wenn über einen Router verbunden wird
  Label2.Enabled := CheckBox3.Checked;
  Edit2.Enabled := CheckBox3.Checked;
  Label3.Enabled := CheckBox3.Checked;
  Edit3.Enabled := CheckBox3.Checked;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Begin
  // Den Spielernamen sofort prüfen, die Meldung beim Launch bleibt als Sicherheitsnetz
  If ValidNodeName(trim(Edit1.Text)) Then Begin
    Label4.Caption := R_ValidName;
    Label4.Font.Color := clGreen;
  End
  Else Begin
    Label4.Caption := R_InvalidNameHint;
    Label4.Font.Color := clRed;
  End;
End;

Procedure TForm1.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  (*
   * Der Launch "Knopf" ist ein Panel (nur so lässt er sich blau färben), daher Alt+L und Leertaste (wenn er den Fokus hat) hier von Hand.
   * Return startet im Hauptdialog immer (auch mit Strg / Shift, die wertet Button2Click aus), außer ein anderer Knopf hat den Fokus oder die Sprachauswahl ist aufgeklappt.
   *)
  If ((ssAlt In Shift) And (key = VK_L)) Or
    ((ActiveControl = Panel1) And (key = VK_SPACE)) Or
    ((key = VK_RETURN) And ((Shift - [ssCtrl, ssShift]) = []) And Not (ActiveControl Is TCustomButton) And Not ComboBox1.DroppedDown) Then Begin
    key := 0;
    Button2Click(Nil);
  End;
End;

Procedure TForm1.Panel1MouseEnter(Sender: TObject);
Var
  p: TPoint;
Begin
  // Gemeinsam für Enter und Leave von Panel1 und Label7, ob die Maus über dem Knopf ist entscheidet die Position
  p := Panel1.ScreenToClient(Mouse.CursorPos);
  If GameDataFound Or CanGenerate Then Begin
    If (p.x >= 0) And (p.y >= 0) And (p.x < Panel1.Width) And (p.y < Panel1.Height) Then Begin
      Panel1.Color := $00A85A00; // Dunkleres Blau
    End
    Else Begin
      Panel1.Color := $00C06700;
    End;
    Label7.Font.Color := clWhite;
    Panel1.BevelOuter := bvNone;
  End
  Else Begin
    // Ohne Spieldaten (und ohne Generator) sieht er aus wie ein normaler Knopf, ein Klick zeigt die fehlenden Dateien
    Panel1.Color := clBtnFace;
    Label7.Font.Color := clBtnText;
    Panel1.BevelOuter := bvRaised;
  End;
End;

Procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Begin
  Panel1MouseEnter(Sender);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  fAssetFolders.free;
  fAssetFolders := Nil;
  fAssetNames.free;
  fAssetNames := Nil;
  ini.free;
  ini := Nil;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  P: TProcessUTF8;
  sl: TStringList;
Begin
  If Not ValidNodeName(trim(Edit1.text)) Then Begin
    showmessage(Edit1.text + LineEnding + R_InvalidNodeName);
    exit;
  End;
  // Launch
  StoreSettings();
  ini.UpdateFile;
  sl := MissingGameData();
  If (sl.count <> 0) And SelectUsableAssetSet() Then Begin
    // Original gewählt, aber keine CD-Daten: es gibt einen vollständigen (generierten) Satz, dann eben den
    sl.free;
    sl := MissingGameData();
  End;
  If (sl.count <> 0) And CanGenerate Then Begin
    (*
     * Keine Spieldaten und kein brauchbarer Grafiksatz: den Generator starten, warten und bei Erfolg mit dem neuen Satz
     * weiter starten. Die GUI fragt selbst nach dem Stil (mit Abbrechen), ohne sie läuft der Kommandozeilen-Generator,
     * dann fragen wir vorher.
     *)
    sl.free;
    If Not FileExists(AssetGeneratorExe()) Then Begin
      If id_yes <> Application.MessageBox(pchar(R_AskGenerateGraphics), pchar(R_GenerateGraphicsTitle), MB_YESNO Or MB_ICONQUESTION) Then exit;
    End;
    If Not GenerateAssetSet() Then exit;
    sl := MissingGameData();
  End;
  If sl.count <> 0 Then Begin
    showmessage(R_MissingFiles + LineEnding +
      sl.Text + LineEnding +
      R_RunExtractorAgain);
    sl.free;
    exit;
  End;
  sl.free;
  PrepareSchemes(); // Falls der gewählte Satz Schemes mitbringt, data/schemes aber noch leer ist
  // Short Prechecks
  If (Not FileExists('fpc_atomic'{$IFDEF Windows} + '.exe'{$ENDIF})) Or
    (Not FileExists('atomic_server'{$IFDEF Windows} + '.exe'{$ENDIF})) Then Begin
    showmessage(R_InstallationIncomplete);
    exit;
  End;
  // Run the App ;)
  p := TProcessUTF8.Create(Nil);
  p.Options := [poDetached];
  p.Executable := 'fpc_atomic'{$IFDEF Windows} + '.exe'{$ENDIF};
  If GetKeyState(VK_CONTROL) < 0 Then Begin
    // Strg + Launch: Das Spiel hostet sofort ein Netzwerkspiel, ohne Umweg über das Hauptmenü
    p.Parameters.Add('-host');
  End
  Else If GetKeyState(VK_SHIFT) < 0 Then Begin
    // Shift + Launch: Das Spiel tritt sofort einem Netzwerkspiel bei (sucht im lokalen Netz, sonst fragt es nach IP und Port)
    p.Parameters.Add('-join');
  End
  Else If CheckBox3.Checked Then Begin
    p.Parameters.Add('-ip');
    p.Parameters.Add(Edit2.Text);
    p.Parameters.Add('-port');
    p.Parameters.Add(Edit3.Text);
  End;
  // Der Grafiksatz, ohne den Parameter nimmt das Spiel die Originalgrafiken aus data
  If (fAssetIndex > 0) And (fAssetIndex < fAssetFolders.Count) Then Begin
    p.Parameters.Add('-assets');
    p.Parameters.Add(fAssetFolders[fAssetIndex]);
  End;
  // Die Sounds, ohne den Parameter nimmt das Spiel die Originalsounds aus data
  If SelectedSoundSet() <> '' Then Begin
    p.Parameters.Add('-sounds');
    p.Parameters.Add(SelectedSoundSet());
  End;
  If CheckBox6.Checked Then Begin
    // Der Launcher wartet versteckt, bis das Spiel beendet ist, und erscheint dann wieder (siehe Timer1Timer)
    p.Options := [];
    p.Execute;
    GameProcess := p;
    Hide;
    Timer1.Enabled := true;
  End
  Else Begin
    p.Execute;
    p.free;
    close;
  End;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  // Das Spiel wurde beendet -> der Launcher erscheint wieder
  If assigned(GameProcess) And GameProcess.Running Then exit;
  Timer1.Enabled := false;
  GameProcess.free;
  GameProcess := Nil;
  // Das Spiel schreibt beim Beenden fpc_atomic.cfg, also neu laden (Name, Optionen, Version)
  ini.free;
  ini := TIniFile.Create(GetAtomicConfigFile());
  LoadSettings();
  UpdateStatus();
  Show;
  BringToFront;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
{$IFDEF Linux}
  If DirectoryExists('cd_data_extractor') Then Begin
    If id_yes = Application.MessageBox(pchar(R_ExtractorFolderConflict + LineEnding +
      R_RenameFolder), pchar(R_Warning), MB_YESNO) Then Begin
      If Not RenameFile('cd_data_extractor' + PathDelim, 'cd_data_extractor_' + PathDelim) Then Begin
        showmessage(R_RenameFailed);
        exit;
      End;
    End
    Else Begin
      exit;
    End;
  End;
{$ENDIF}
  ClearLog();
  // Der Dialog prüft online und zeigt, was aktualisiert werden kann
  Form3.Execute(Version);
  UpToDate := Form3.UpToDate;
  UpdateStatus();
End;

Procedure TForm1.LoadSideImage;
Var
  p: TPortableNetworkGraphic;
  SetDir: String;
Begin
  (*
   * Das Bild zeigt den gewählten Grafiksatz: dessen preview.png, sonst die linke Hälfte seines Hauptmenüs,
   * sonst (und für Original) die linke Hälfte des Hauptmenüs aus data
   *)
  Image1.Picture.Clear;
  If (fAssetIndex > 0) And (fAssetIndex < fAssetFolders.Count) Then Begin
    SetDir := AssetSetsDir() + fAssetFolders[fAssetIndex] + PathDelim;
    Try
      If FileExists(SetDir + AssetPreviewFile) Then Begin
        Image1.Picture.LoadFromFile(SetDir + AssetPreviewFile);
        exit;
      End;
      If FileExists(SetDir + 'res' + PathDelim + 'mainmenu.png') Then Begin
        p := TPortableNetworkGraphic.Create;
        Try
          p.LoadFromFile(SetDir + 'res' + PathDelim + 'mainmenu.png');
          p.Width := p.Width Div 2;
          Image1.Picture.Assign(p);
        Finally
          p.free;
        End;
        exit;
      End;
    Except
      On av: Exception Do Begin
        log(av.Message); // Ein kaputtes Bild im Satz, dann eben das Original
        Image1.Picture.Clear;
      End;
    End;
  End;
  If FileExists('data' + PathDelim + 'res' + PathDelim + 'mainmenu.png') Then Begin
    p := TPortableNetworkGraphic.Create;
    p.LoadFromFile('data' + PathDelim + 'res' + PathDelim + 'mainmenu.png');
    p.Width := p.Width Div 2;
    Image1.Picture.Assign(p);
    p.free;
  End;
End;

Procedure TForm1.ReadAssetSets;
Var
  Folder: String;
  i: integer;
Begin
  // Original und alle Sätze in assets/, gewählt ist der aus [Launcher] AssetSet (gibt es ihn nicht mehr, dann Original)
  ListAssetSets(fAssetFolders, fAssetNames);
  fAssetFolders.Insert(0, '');
  fAssetNames.Insert(0, R_AssetSetOriginal);
  Folder := ini.ReadString('Launcher', 'AssetSet', '');
  fAssetIndex := 0;
  For i := 1 To fAssetFolders.Count - 1 Do Begin
    If SameText(fAssetFolders[i], Folder) Then Begin
      fAssetIndex := i;
      break;
    End;
  End;
  // Den Generator gibt es evtl. noch nicht, dann gibt es auch keinen Knopf und keinen Doppelklick
  Button12.Visible := FileExists(AssetGeneratorExe());
  Image1.ShowHint := Button12.Visible;
  Button10.Enabled := fAssetFolders.Count > 1;
  Button11.Enabled := fAssetFolders.Count > 1;
  SelectAssetSet(fAssetIndex);
End;

Procedure TForm1.SelectAssetSet(Index: integer);
Begin
  If (Index < 0) Or (Index >= fAssetFolders.Count) Then Index := 0;
  fAssetIndex := Index;
  If fAssetFolders.Count = 0 Then Begin
    Label11.Caption := R_AssetSetOriginal;
  End
  Else Begin
    Label11.Caption := fAssetNames[fAssetIndex];
    Label11.Hint := fAssetNames[fAssetIndex];
    // Gleich speichern, die selbe Einstellung nutzen der Dialog Spieloptionen und der Start des Spiels
    If ini.ReadString('Launcher', 'AssetSet', '') <> fAssetFolders[fAssetIndex] Then Begin
      ini.WriteString('Launcher', 'AssetSet', fAssetFolders[fAssetIndex]);
    End;
  End;
  LoadSideImage();
  UpdateStatus(); // Ob die Spieldaten reichen, hängt vom Grafiksatz ab
End;

Function TForm1.AssetGeneratorExe: String;
Begin
  result := AppDir() + 'asset_generator_gui'{$IFDEF Windows} + '.exe'{$ENDIF};
End;

Function TForm1.AssetGeneratorCliExe: String;
Begin
  result := AppDir() + 'asset_generator'{$IFDEF Windows} + '.exe'{$ENDIF};
End;

Function TForm1.SelectedAssetFolder: String;
Begin
  result := '';
  If (fAssetIndex > 0) And (fAssetIndex < fAssetFolders.Count) Then result := fAssetFolders[fAssetIndex];
End;

Function TForm1.SelectedSoundSet: String;
Begin
  // [Launcher] SoundSet (Standard Automatisch) zum gewählten Grafiksatz aufgelöst, '' = Originalsounds aus data
  result := ResolveSoundSet(ini.ReadString('Launcher', 'SoundSet', SoundSetAuto), SelectedAssetFolder());
End;

Function TForm1.MissingGameData: TStringList;
Begin
  // Mit Grafiksatz reichen dessen Grafiken und Schemes, Sounds sind dann optional (siehe CheckForFiles)
  result := CheckForFiles(SelectedAssetFolder());
End;

Function TForm1.SelectUsableAssetSet: Boolean;
Var
  sl: TStringList;
  i: integer;
Begin
  (*
   * Ist "Original" gewählt, gibt es aber keine CD-Daten, dann den ersten Grafiksatz wählen, mit dem das Spiel starten
   * kann (z.B. einen generierten). Mit CD-Daten oder einem gewählten Satz ändert sich nichts. Result = umgeschaltet.
   *)
  result := false;
  If (fAssetIndex <> 0) Or (fAssetFolders.Count < 2) Then exit;
  sl := CheckForFiles('');
  i := sl.Count;
  sl.free;
  If i = 0 Then exit;
  For i := 1 To fAssetFolders.Count - 1 Do Begin
    sl := CheckForFiles(fAssetFolders[i]);
    result := sl.Count = 0;
    sl.free;
    If result Then Begin
      SelectAssetSet(i);
      exit;
    End;
  End;
End;

Procedure TForm1.PrepareSchemes;
Var
  i, Count: integer;
  s, r: String;
Begin
  (*
   * Ohne CD-Daten ist data/schemes leer, Spiel und Server lesen die Schemes aber nur von dort. Dann die des gewählten
   * Satzes (sonst die irgendeines Satzes, generierte Schemes sind überall gleich) dorthin kopieren.
   * Liegen dort schon Schemes (CD), passiert nichts.
   *)
  If HasSchemeFiles(SchemesDir()) Then exit;
  Count := CopySetSchemes(SelectedAssetFolder());
  For i := 1 To fAssetFolders.Count - 1 Do Begin
    If Count <> 0 Then break;
    Count := CopySetSchemes(fAssetFolders[i]);
  End;
  If Count = 0 Then exit;
  // Generierte Schemes haben kein BASIC.SCH (die Voreinstellung), dann das erste vorhandene
  s := ini.ReadString(FPC_AtomicIniSection, 'SchemeFile', 'BASIC.SCH');
  r := ResolveSchemeFile(s);
  If r <> s Then Begin
    ini.WriteString(FPC_AtomicIniSection, 'SchemeFile', r);
    ini.UpdateFile;
  End;
  UpdateStatus();
End;

Function TForm1.GenerateAssetSet: Boolean;
Var
  p: TProcessUTF8;
  Folder, s, Line: String;
  sl: TStringList;
  n: integer;
  UseGui: Boolean;
  Buffer: Array[0..4095] Of char;

  // Die Ausgabe des Kommandozeilen-Generators lesen (sonst läuft die Pipe voll), Zeilen "[x/y] Datei" zeigen den Fortschritt
  Procedure ReadOutput;
  Var
    c, i: integer;
  Begin
    If Not (poUsePipes In p.Options) Then exit;
    While p.Output.NumBytesAvailable > 0 Do Begin
      c := p.Output.NumBytesAvailable;
      If c > SizeOf(Buffer) Then c := SizeOf(Buffer);
      c := p.Output.Read(Buffer, c);
      If c <= 0 Then break;
      For i := 0 To c - 1 Do Begin
        If Buffer[i] In [#10, #13] Then Begin
          If (Line <> '') And (Line[1] = '[') And (pos(']', Line) > 0) Then Begin
            Label5.Caption := R_GeneratingGraphics + ' ' + copy(Line, 1, pos(']', Line));
          End;
          Line := '';
        End
        Else Begin
          Line := Line + Buffer[i];
        End;
      End;
    End;
  End;

Begin
  (*
   * Erzeugt einen neuen Grafiksatz (mit Schemes) in assets/Generated (Generated2, ... wenn es den schon gibt),
   * wählt ihn aus und kopiert seine Schemes nach data/schemes (wenn dort keine sind). Result = das Spiel kann jetzt starten.
   * - asset_generator_gui --auto --set <Ordner>: zeigt die Stil-Auswahl und den Fortschritt, schreibt den Ordner nach
   *   assets/last_set.txt, Exit-Code 0 = erzeugt, sonst abgebrochen oder Fehler.
   * - Ohne GUI: asset_generator (Kommandozeile) mit zufälligem Seed im Stil "flat", der Fortschritt steht im Status.
   * Wie beim CD Extraktor wartet der Launcher auf das Ende, er ist solange gesperrt, zeichnet sich aber neu.
   *)
  result := false;
  UseGui := FileExists(AssetGeneratorExe());
  If (Not UseGui) And (Not FileExists(AssetGeneratorCliExe())) Then exit;
  StoreSettings();
  ini.UpdateFile;
  Folder := 'Generated';
  n := 1;
  While DirectoryExists(AssetSetsDir() + Folder) Do Begin
    inc(n);
    Folder := 'Generated' + inttostr(n);
  End;
  Line := '';
  Buffer[0] := #0; // Beruhigt den Compiler
  p := TProcessUTF8.Create(Nil);
  Enabled := false;
  Screen.Cursor := crHourGlass;
  Label5.Caption := R_GeneratingGraphics;
  Label5.Font.Color := clWindowText;
  Try
    p.CurrentDirectory := AppDir();
    If UseGui Then Begin
      p.Executable := AssetGeneratorExe();
      p.Parameters.Add('--auto');
      p.Parameters.Add('--set');
      p.Parameters.Add(Folder);
      p.Options := [];
    End
    Else Begin
      p.Executable := AssetGeneratorCliExe();
      p.Parameters.Add('--out');
      p.Parameters.Add(AssetSetsDir() + Folder);
      p.Parameters.Add('--name');
      p.Parameters.Add(Folder);
      p.Parameters.Add('--seed');
      p.Parameters.Add('random');
      p.Parameters.Add('--style');
      p.Parameters.Add('flat');
      p.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    End;
    Try
      p.Execute;
      While p.Running Do Begin
        ReadOutput();
        Application.ProcessMessages;
        Sleep(20);
      End;
      ReadOutput();
      result := p.ExitCode = 0;
      If Not result Then log('Graphics generator ended with exit code ' + inttostr(p.ExitCode));
    Except
      On av: Exception Do Begin
        log('Could not run the graphics generator: ' + av.Message);
        result := false;
      End;
    End;
  Finally
    p.free;
    Screen.Cursor := crDefault;
    Enabled := true;
  End;
  If result And UseGui And FileExists(AssetSetsDir() + 'last_set.txt') Then Begin
    // Die GUI schreibt den tatsächlich erzeugten Ordner hierher
    sl := TStringList.Create;
    Try
      sl.LoadFromFile(AssetSetsDir() + 'last_set.txt');
      If sl.Count > 0 Then Begin
        s := trim(sl[0]);
        If s <> '' Then Folder := s;
      End;
    Finally
      sl.free;
    End;
  End;
  If result Then result := FileExists(AssetSetsDir() + Folder + PathDelim + AssetProfileFile);
  If Not result Then Begin
    UpdateStatus();
    showmessage(R_GenerateFailed);
    exit;
  End;
  // Den neuen Satz auswählen (ReadAssetSets liest [Launcher] AssetSet), dann seine Schemes kopieren
  ini.WriteString('Launcher', 'AssetSet', Folder);
  ini.UpdateFile;
  ReadAssetSets();
  PrepareSchemes();
  UpdateStatus();
  result := GameDataFound;
  If Not result Then showmessage(R_GenerateFailed);
End;

Procedure TForm1.StartAssetGenerator;
Var
  P: TProcessUTF8;
Begin
  If Not FileExists(AssetGeneratorExe()) Then exit;
  StoreSettings();
  ini.UpdateFile;
  // Wie der CD Extraktor: warten, bis der Generator beendet ist, dann assets/ neu einlesen
  p := TProcessUTF8.Create(Nil);
  Try
    p.Executable := AssetGeneratorExe();
    If (fAssetIndex > 0) And (fAssetIndex < fAssetFolders.Count) Then Begin
      p.Parameters.Add('--set');
      p.Parameters.Add(fAssetFolders[fAssetIndex]);
    End
    Else Begin
      p.Parameters.Add('--new');
    End;
    p.Options := p.Options + [poWaitOnExit];
    p.Execute;
  Finally
    p.free;
  End;
  ReadAssetSets();
End;

Procedure TForm1.OnDoLog(Const LogText: String);
Var
  aText: String;
Begin
  aText := copy(LogText, pos('|', LogText) + 1, length(LogText));
  aText := copy(aText, pos('|', aText) + 1, length(aText));
  // Der Update Dialog zeigt das Log selbst (ohne die Stack Info)
  If Form3.Visible Then Begin
    Form3.AddLog(trim(copy(aText, pos(':', aText) + 1, length(aText))));
    exit;
  End;
  form2.Memo1.Append(aText);
  If QuietLog Then exit;
  If Not form2.Visible Then Begin
    form2.top := form1.top;
    form2.Left := form1.left + form1.Width + 10;
    Form2.Show;
  End;
End;

Procedure TForm1.OnStartCheckDone(Ok: Boolean; Const Filename,
  ErrorText: String);
Var
  v: TVersion;
Begin
  // Fehler werden nur geloggt, die Prüfung beim Start soll niemanden stören
  QuietLog := true;
  Try
    If Ok Then Begin
      v := TVersion.Create();
      If v.LoadFromFile(Filename) Then Begin
        OnlineVersion := v.Version;
        OnlineLauncherVersion := v.LauncherVersion;
      End;
      v.free;
    End
    Else Begin
      log('Check for updates at the start: ' + ErrorText);
    End;
    DeleteFile(Filename);
  Finally
    QuietLog := false;
  End;
  If assigned(ini) Then UpdateStatus();
End;

Function TForm1.GetCheckUpdatesOnStart: Boolean;
Begin
  result := assigned(ini) And ini.ReadBool('Launcher', 'CheckUpdatesOnStart', true);
End;

Procedure TForm1.SetCheckUpdatesOnStart(Value: Boolean);
Begin
  If Not assigned(ini) Then exit;
  ini.WriteBool('Launcher', 'CheckUpdatesOnStart', Value);
  ini.UpdateFile;
End;

Procedure TForm1.StoreVersion(NewVersion: single);
Var
  fm: TFormatSettings;
Begin
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  ini.WriteString(FPC_AtomicIniSection, 'Version', format('%0.5f', [NewVersion], fm));
  ini.UpdateFile;
  Version := NewVersion;
End;

Procedure TForm1.LoadLanguages;
Var
  Search: TSearchRec;
  Codes: TStringList;
  LanguageCode: String;
  Index, FileIndex: Integer;
Begin
  setlength(fLanguageCodes, length(LauncherLanguageCodes));
  Codes := TStringList.Create;
  Try
    Codes.Sorted := true;
    Codes.Duplicates := dupIgnore;
    If FindFirst(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'launcher_lang.*', faAnyFile, Search) = 0 Then Begin
      Try
        Repeat
          If (Search.Attr And faDirectory) <> 0 Then Continue;
          If ChangeFileExt(Search.Name, '') <> 'launcher_lang' Then Continue;
          LanguageCode := copy(ExtractFileExt(Search.Name), 2, MaxInt);
          If LanguageCode <> '' Then Codes.Add(LanguageCode);
        Until FindNext(Search) <> 0;
      Finally
        FindClose(Search);
      End;
    End;
    For Index := low(LauncherLanguageCodes) To high(LauncherLanguageCodes) Do Begin
      fLanguageCodes[Index] := LauncherLanguageCodes[Index];
      FileIndex := Codes.IndexOf(fLanguageCodes[Index]);
      If FileIndex >= 0 Then Begin
        fLanguageCodes[Index] := Codes[FileIndex];
        Codes.Delete(FileIndex);
      End;
    End;
    For Index := 0 To Codes.Count - 1 Do Begin
      setlength(fLanguageCodes, length(fLanguageCodes) + 1);
      fLanguageCodes[high(fLanguageCodes)] := Codes[Index];
      ComboBox1.Items.Add(Codes[Index]);
    End;
  Finally
    Codes.Free;
  End;
End;

Procedure TForm1.LoadSettings;
Var
  fm: TFormatSettings;
  Index: Integer;
  LanguageCode: String;
Begin
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  CheckBox1.Checked := ini.ReadBool(FPC_AtomicIniSection, 'ShowFPS', false);
  CheckBox2.Checked := ini.ReadBool(FPC_AtomicIniSection, 'Fullscreen', false);
  CheckBox3.Checked := false;
  CheckBox4.Checked := ini.ReadBool(FPC_AtomicIniSection, 'PlaySounds', true);
  CheckBox5.Checked := Not ini.ReadBool(FPC_AtomicIniSection, 'Proportional', true);
  CheckBox7.Checked := ini.ReadBool(FPC_AtomicIniSection, 'RememberWindow', true);
  edit1.text := ini.ReadString(FPC_AtomicIniSection, 'NodeName', 'Player 1');
  ProtocollVersion := ini.ReadInteger(FPC_AtomicIniSection, 'ProtocollVersion', -1);
  Version := strtofloatdef(ini.ReadString(FPC_AtomicIniSection, 'Version', '-1'), -1, fm);

  edit2.text := ini.ReadString('Launcher', 'Router_IP', '127.0.0.1');
  edit3.text := ini.ReadString('Launcher', 'Router_Port', '5521');
  CheckBox6.Checked := ini.ReadBool('Launcher', 'ShowAfterGame', false);
  LanguageCode := ini.ReadString('Launcher', 'Language', 'en');
  ComboBox1.ItemIndex := 0;
  For Index := low(fLanguageCodes) To high(fLanguageCodes) Do Begin
    If SameText(LanguageCode, fLanguageCodes[Index]) Then Begin
      ComboBox1.ItemIndex := Index;
      break;
    End;
  End;
  ReadAssetSets();
End;

Procedure TForm1.UpdateStatus;
Var
  sl: TStringList;
  fm: TFormatSettings;
  UpdateAvailable: Boolean;
Begin
  // Links: sind die Spieldaten vollständig, rechts: die installierte Version (ohne Netzwerkzugriff)
  If Not assigned(fAssetFolders) Then exit; // Noch nicht initialisiert
  // Mit gewähltem Grafiksatz reichen dessen Grafiken und Schemes (Sounds sind dann optional)
  sl := MissingGameData();
  If sl.Count = 0 Then Begin
    Label5.Caption := R_GameDataFound;
    Label5.Font.Color := clGreen;
    // Spielbar, aber stumm: weder CD-Sounds noch Sounds im gewählten Satz (siehe Spieloptionen, Sounds)
    If (Not HasOriginalSounds()) And (SelectedSoundSet() = '') Then Label5.Caption := Label5.Caption + ' ' + R_NoSounds;
  End
  Else Begin
    Label5.Caption := R_GameDataMissing;
    Label5.Font.Color := clRed;
  End;
  // Der Knopf daneben, der das Problem löst, wird fett
  If sl.Count = 0 Then Begin
    Button4.Font.Style := [];
  End
  Else Begin
    Button4.Font.Style := [fsBold];
  End;
  // Ohne Spieldaten kann der Grafik-Generator einen Satz erzeugen (Button13, der Launch Knopf macht das auch)
  CanGenerate := (sl.Count <> 0) And (FileExists(AssetGeneratorExe()) Or FileExists(AssetGeneratorCliExe()));
  Button13.Visible := CanGenerate;
  Button13.Font.Style := [fsBold];
  // Die fehlenden Dateien stehen im Tooltip
  Label5.Hint := trim(sl.Text);
  Label5.ShowHint := sl.Count <> 0;
  If (sl.Count = 0) And (Not HasOriginalSounds()) And (SelectedSoundSet() = '') Then Begin
    Label5.Hint := R_NoSoundsHint;
    Label5.ShowHint := true;
  End;
  GameDataFound := sl.Count = 0;
  sl.free;
  Panel1MouseEnter(Nil);
  If Version = -1 Then Begin
    Label6.Caption := R_GameVersionUnknown;
  End
  Else Begin
    fm := DefaultFormatSettings;
    fm.DecimalSeparator := '.';
    Label6.Caption := R_GameVersion + format('%0.5f', [Version], fm);
  End;
  If UpToDate Then Label6.Caption := Label6.Caption + R_VersionUpToDate;
  // Von der Prüfung beim Start gefunden
  UpdateAvailable := (Version <> -1) And (
    ((OnlineVersion <> -1) And (OnlineVersion > Version + 0.000001)) Or
    ((OnlineLauncherVersion <> -1) And (OnlineLauncherVersion > LauncherVersion)));
  If UpdateAvailable Then Begin
    fm := DefaultFormatSettings;
    fm.DecimalSeparator := '.';
    Label6.Caption := Label6.Caption + format(RF_UpdateAvailable, [format('%0.5f', [Max(OnlineVersion, Version)], fm)]);
    Label6.Font.Color := $00007FD0; // Orange
  End
  Else Begin
    Label6.Font.Color := clDefault;
  End;
  // Ohne bekannte Version ist das Spiel noch nicht (vollständig) installiert, das macht "Updates..."
  If (Version = -1) Or UpdateAvailable Then Begin
    Button1.Font.Style := [fsBold];
  End
  Else Begin
    Button1.Font.Style := [];
  End;
End;

Procedure TForm1.UpdateModsCaption;
Var
  Settings: TModSettings;
  Count: integer;
Begin
  // Die Anzahl der aktiven Mods steht im Knopf, damit man nicht vergisst, dass welche an sind
  Settings.Handicap := Nil;
  LoadModSettingsFromFile(GetAtomicModsIniFile(), Settings);
  Count := ord(Settings.StunOnBombLanding) + ord(Settings.PunchStun) + ord(Settings.PositiveDisease) +
    ord(Settings.BrickWalk) + ord(Settings.FlashDisease) + ord(Settings.WhiteBomb) +
    ord(Settings.FlameThrower) + ord(Settings.HitPoints) + ord(Settings.Ghosts) +
    ord(Settings.GloveAtStart) + ord(Settings.OpenBorders) + ord(Settings.Teleporters) +
    ord(Settings.ItemConfig) + ord(Settings.CarryBomb) + ord(Settings.ModTestMode);
  Settings.Handicap.free;
  If Count = 0 Then Begin
    Button7.Caption := R_Mods;
  End
  Else Begin
    Button7.Caption := format(RF_ModsCount, [Count]);
  End;
End;

Procedure TForm1.UpdateGameModeCaption;
Var
  Settings: TModSettings;
Begin
  // Der aktive Spielmodus steht neben dem Knopf, der Name ist der übersetzte aus dem Dialog
  Settings.Handicap := Nil;
  LoadModSettingsFromFile(GetAtomicModsIniFile(), Settings);
  Settings.Handicap.free;
  Label10.Caption := form6.Caption + ': ' + form6.GameModeName(Settings.GameMode);
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  i: Integer;
Begin
  If Language.CurrentLanguage <> '' Then exit;
  For i := 1 To ParamCount Do Begin
    If SameText(ParamStr(i), '-lazarus-ide') Then Begin
      Language.AskNameChanges := true;
      Language.DeleteUnusedItems := true;
      ComboBox1.ItemIndex := 0; // Force to en!
      break;
    End;
  End;
  Language.AddIgnoredIdentifier('Form1.Caption');
  Language.AddIgnoredIdentifier('Form1.Button7.Caption'); // Setzt UpdateModsCaption
  Language.AddIgnoredIdentifier('Form1.Label4.Caption');
  Language.AddIgnoredIdentifier('Form1.Label5.Caption');
  Language.AddIgnoredIdentifier('Form1.Label5.Hint');
  Language.AddIgnoredIdentifier('Form1.Label6.Caption');
  Language.AddIgnoredIdentifier('Form1.Label10.Caption'); // Setzt UpdateGameModeCaption
  Language.AddIgnoredIdentifier('Form1.Label11.Caption'); // Der Name des Grafiksatzes, setzt SelectAssetSet
  Language.AddIgnoredIdentifier('Form1.Label11.Hint');
  Language.AddIgnoredIdentifier('Form1.Button10.Caption');
  Language.AddIgnoredIdentifier('Form1.Button11.Caption');
  Language.AddIgnoredIdentifier('Form1.ComboBox1.Items');
  Language.AddIgnoredComponent(Form3.ListView1);
  For i := 0 To Form3.ComponentCount - 1 Do Begin // Form3: zur Laufzeit gesetzt (Labels ohne Caption im Designer, Knöpfe)
    If (Form3.Components[i] Is TLabel) And (TLabel(Form3.Components[i]).Caption = '') Then
      Language.AddIgnoredIdentifier('Form3.' + Form3.Components[i].Name + '.Caption');
  End;
  Language.AddIgnoredIdentifier('Form3.ButtonMain.Caption');
  Language.AddIgnoredIdentifier('Form3.ButtonClose.Caption');
  Language.AddIgnoredComponent(Form4.StringGrid1);
  For i := 80 To 92 Do Begin // Form4: Anteile der Items und Krankheiten, setzt UpdateItemShares
    Language.AddIgnoredIdentifier('Form4.Label' + inttostr(i) + '.Caption');
  End;
  For i := 100 To 103 Do Begin
    Language.AddIgnoredIdentifier('Form4.Label' + inttostr(i) + '.Caption');
  End;
  Language.AddIgnoredIdentifier('Form5.ComboBox2.Items');
  Language.AddIgnoredIdentifier('Form5.ComboBox4.Items');
  Language.AddIgnoredIdentifier('Form5.ComboBox5.Items');
  Language.AddIgnoredIdentifier('Form5.ComboBox6.Items');
  ApplyLanguage();
  // Nur ein Hinweis neben "Updates...", ohne Klick wird nichts heruntergeladen
  If CheckUpdatesOnStart Then Begin
    TUpdateCheckThread.Create(GetUpdateTempFolder() + 'fpc_atomic_version_start.json', @OnStartCheckDone);
  End;
End;

Procedure TForm1.ComboBox1Change(Sender: TObject);
Begin
  If Language.CurrentLanguage = '' Then exit;
  Language.AskNameChanges := false;
  ApplyLanguage();
  ini.UpdateFile;
End;

Procedure TForm1.ApplyLanguage;
Var
  Checked: Array Of Boolean;
  ItemIndex, ConveyorIndex, PlayTimeIndex, Index: Integer;
Begin
  Checked := Nil;
  setlength(Checked, Form4.CheckListBox1.Count);
  For Index := 0 To high(Checked) Do Begin
    Checked[Index] := Form4.CheckListBox1.Checked[Index];
  End;
  ItemIndex := Form4.CheckListBox1.ItemIndex;
  ConveyorIndex := Form5.ComboBox1.ItemIndex;
  PlayTimeIndex := Form5.ComboBox3.ItemIndex;
  Try
    Language.CurrentLanguage := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'launcher_lang.' + fLanguageCodes[ComboBox1.ItemIndex];
  Finally
    For Index := 0 To high(Checked) Do Begin
      Form4.CheckListBox1.Checked[Index] := Checked[Index];
    End;
    Form4.CheckListBox1.ItemIndex := ItemIndex;
    Form5.ComboBox1.ItemIndex := ConveyorIndex;
    Form5.ComboBox3.ItemIndex := PlayTimeIndex;
  End;
  ini.WriteString('Launcher', 'Language', fLanguageCodes[ComboBox1.ItemIndex]);
  Edit1Change(Nil);
  UpdateStatus();
  UpdateModsCaption();
  UpdateGameModeCaption();
  If fAssetNames.Count > 0 Then Begin
    fAssetNames[0] := R_AssetSetOriginal; // Der Name von Original kommt aus der Sprache
    SelectAssetSet(fAssetIndex);
  End;
  FitLayout(self); // Platz für die Texte im aktuellen Widgetset und in der aktuellen Sprache
End;

Procedure TForm1.StoreSettings;
Begin
  ini.WriteBool(FPC_AtomicIniSection, 'ShowFPS', CheckBox1.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'Fullscreen', CheckBox2.Checked);

  ini.WriteBool(FPC_AtomicIniSection, 'PlaySounds', CheckBox4.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'Proportional', Not CheckBox5.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'RememberWindow', CheckBox7.Checked);
  ini.WriteString(FPC_AtomicIniSection, 'NodeName', trim(edit1.text));

  ini.WriteString('Launcher', 'Router_IP', edit2.text);
  ini.WriteString('Launcher', 'Router_Port', edit3.text);
  ini.WriteBool('Launcher', 'ShowAfterGame', CheckBox6.Checked);
End;

{$IFDEF Windows}

Initialization
  // TODO: Hier fehlt noch ein Hinweis wie man die .lrs Datei erzeugt ;)
{$I atomic_launcher.lrs}

  // 1. ggf. die Crypto libs entpacken und dann einrichten
  If Not CheckAndMaybeExtract('ssleay32') Then exit;
  If Not CheckAndMaybeExtract('libeay32') Then exit;

  If SSLImplementation = TSSLNone Then Begin
    If InitSSLInterface Then
      SSLImplementation := TSSLOpenSSL;
  End;
{$ENDIF}

End.

