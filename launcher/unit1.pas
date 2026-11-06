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
  Buttons, IniFiles, UTF8Process, ulauncher, ulaunchersound;

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
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
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
    Label12: TLabel;
    ComboBox1: TComboBox;
    Panel1: TPanel;
    PanelNews: TPanel;
    LabelNews: TLabel;
    ButtonNewsClose: TSpeedButton;
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
    Procedure Button14Click(Sender: TObject);
    Procedure Button16Click(Sender: TObject);
    Procedure Button17Click(Sender: TObject);
    Procedure Image1Click(Sender: TObject);
    Procedure LabelNewsClick(Sender: TObject);
    Procedure ShiftLeftColumn(Delta: Integer); // verschiebt das Bild (die Zeilen darunter hängen daran), merkt sich die Summe in fLeftColumnShift
    Procedure ButtonNewsCloseClick(Sender: TObject);
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
    fLeftColumnShift: Integer; // um so viel ist die linke Spalte ohne Hinweis nach oben gerückt (negativ)
    ini: TIniFile;
    OnlineVersion: Single; // Von der Prüfung beim Start, -1 = unbekannt
    OnlineLauncherVersion: integer; // Von der Prüfung beim Start, -1 = unbekannt
    QuietLog: Boolean; // True -> loggen ohne das Log Fenster zu zeigen (Prüfung beim Start)
    ProtocollVersion: integer;
    Version: Single;
    UpToDate: Boolean; // True, wenn "Updates..." in diesem Lauf nichts mehr zum Herunterladen gefunden hat
    GameDataFound: Boolean; // True, wenn CheckForFiles nichts vermisst, dann ist der Launch Knopf blau
    CanGenerate: Boolean; // True, wenn Spieldaten fehlen und der Grafik-Generator einen Satz erzeugen kann (Einrichtung fett), dann ist der Launch Knopf auch blau
    GameProcess: TProcessUTF8; // Das laufende Spiel, wenn der Launcher danach wieder erscheinen soll (CheckBox6), sonst Nil
    fLanguageCodes: Array Of String;
    fAssetFolders: TStringList; // Die Grafiksätze, [0] = '' = Original, sonst der Ordner unter assets/
    fAssetNames: TStringList; // Dazu die angezeigten Namen (AssetSetDisplayName)
    fAssetIndex: integer; // Der gewählte Grafiksatz in fAssetFolders
    fSoundFolders: TStringList; // Die Werte für [Launcher] SoundSet: [0] = SoundSetAuto, [1] = '' = Original, sonst der Ordner unter assets/
    fSoundNames: TStringList; // Dazu die Namen
    fSoundIndex: integer; // Die gewählte Soundquelle in fSoundFolders
    fSoundTest: TSoundTest; // Probehören (Button16), erst beim ersten Klick angelegt
    fSoundTestFolder: String; // Der Soundsatz, der gerade probe gehört wird ('' = Original)
    fNoGraphics: Boolean; // Das Seitenbild zeigt den Platzhalter "Noch keine Grafiken" (ein Klick öffnet die Einrichtung)
    fJobs: TList; // TStyleJob: die übrigen Stile der Einrichtung, die im Hintergrund erzeugt werden
    fJobTimer: TTimer; // Liest deren Ausgabe und merkt, wenn sie fertig sind
    fCloseAfterJobs: Boolean; // Der Launcher wurde geschlossen, während noch Stile erzeugt werden: danach beenden
    fLaunchCaption, fLaunchShown: String; // Der übersetzte Text des Start Knopfs (Label7) und der zuletzt gesetzte (mit Spielmodus)

    Procedure LoadLanguages();
    Procedure LoadSettings();
    Procedure UpdateStatus();
    Procedure UpdateModsCaption();
    Procedure UpdateGameModeCaption();
    Procedure StoreSettings();
    Procedure ApplyLanguage();

    Procedure LoadSideImage();
    Procedure ShowNoGraphicsImage();
    Procedure ReadAssetSets();
    Procedure SelectAssetSet(Index: integer);
    Function AssetGeneratorExe(): String;
    Function AssetGeneratorCliExe(): String;
    Procedure StartAssetGenerator();
    Function SelectedAssetFolder(): String;
    Function SelectedSoundSet(): String;
    Procedure ReadSoundSets();
    Procedure SelectSoundSet(Index: integer);
    Procedure UpdateSoundCaption();
    Procedure UpdateSoundTestButton();
    Procedure StopSoundTest();
    Procedure ReleaseSoundTest();
    Procedure OnSoundTestFinished(Sender: TObject);
    Function MissingGameData(): TStringList;
    Function SelectUsableAssetSet(): Boolean;
    Procedure PrepareSchemes();
    Function GenerateAssetSet(): Boolean;
    Function RunGenerator(p: TProcessUTF8): Boolean;
    Procedure RunAndWait(p: TProcessUTF8);
    Procedure RunSetup();
    Procedure StartSetupAsync(Data: PtrInt);
    Function SetupGenerate(KeepSelection: Boolean; Out Count: integer): Boolean;
    Procedure UpdateNewsBanner();
    Function StartStyleJob(Const Folder, aName, Style, Profile: String): Boolean;
    Procedure OnJobTimer(Sender: TObject);
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

Uses Math, process, Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, LCLType, lclintf
  , ukeyboarddialog, uatomic_common, uatomic_global, ulanguage, ulayout, uupdatecheck, uassetpath, usmartsettings
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

// Der Name eines Stils in der Sprache des Launchers (uassetpath.AssetStyleNameFunc: die Namen der Sätze der Einrichtung)
Function LauncherStyleName(Const Style: String): String;
Begin
  Case Style Of
    'flat': result := R_StyleFlat;
    'soft': result := R_StyleSoft;
    'pixel': result := R_StylePixel;
    'retro': result := R_StyleRetro;
  Else
    result := '';
  End;
End;

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
  fSoundFolders := TStringList.Create;
  fSoundNames := TStringList.Create;
  fSoundIndex := 0;
  fSoundTest := Nil;
  fSoundTestFolder := '';
  fNoGraphics := false;
  fJobs := TList.Create;
  fCloseAfterJobs := false;
  fJobTimer := TTimer.Create(self);
  fJobTimer.Enabled := false;
  fJobTimer.Interval := 200;
  fJobTimer.OnTimer := @OnJobTimer;
  OnlineVersion := -1;
  OnlineLauncherVersion := -1;
  QuietLog := false;
  // Vom Updater übrig gelassen, der alte Launcher
  If FileExists(ParamStr(0) + '.old') Then DeleteFile(ParamStr(0) + '.old');
  AssetStyleNameFunc := @LauncherStyleName; // Sätze der Einrichtung heißen wie ihr Stil in der aktuellen Sprache
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
  ReleaseSoundTest(); // Der Launcher wartet auf den Extraktor
  p := TProcessUTF8.Create(Nil);
  p.Executable := 'cd_data_extractor'{$IFDEF Windows} + '.exe'{$ENDIF};
  Try
    RunAndWait(p);
  Finally
    p.free;
  End;
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
  // "Sounds abspielen" ist die selbe Einstellung wie "Soundeffekte abspielen" im Dialog, also vorher speichern und danach lesen
  ini.WriteBool(FPC_AtomicIniSection, 'PlaySoundEffects', CheckBox8.Checked);
  form5.LoadFromIni(ini);
  If form5.ShowModal = mrOK Then Begin
    form5.SaveToIni(ini);
    ini.UpdateFile;
    CheckBox8.Checked := ini.ReadBool(FPC_AtomicIniSection, 'PlaySoundEffects', true);
    ReadAssetSets(); // Der Grafiksatz kann sich im Dialog geändert haben
    UpdateGameModeCaption(); // Team play hebt einen anderen Spielmodus auf
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

Procedure TForm1.Button14Click(Sender: TObject);
Begin
  // Gemeinsam für die Pfeile unter dem Grafiksatz: Button14 = vorige, Button15 = nächste Soundquelle
  If fSoundFolders.Count = 0 Then exit;
  If Sender = Button15 Then Begin
    SelectSoundSet((fSoundIndex + 1) Mod fSoundFolders.Count);
  End
  Else Begin
    SelectSoundSet((fSoundIndex + fSoundFolders.Count - 1) Mod fSoundFolders.Count);
  End;
End;

Procedure TForm1.Button16Click(Sender: TObject);
Const
  // Was es davon im Satz gibt, sonst in data (wie im Spiel pro Datei)
  Explosions: Array[0..4] Of String = ('explode2.wav', 'explo1.wav', 'expl6.wav', 'bomb_01.wav', 'explode3.wav');
  Voices: Array[0..4] Of String = ('gotcha.wav', 'roasted.wav', 'later.wav', 'youwin1.wav', 'allrite.wav');
  MusicMs = 8000; // Nur der Anfang der Menümusik
Var
  Steps: TSoundTestSteps;
  Folder: String;

  Procedure Add(Const aFile: String; MaxMs: integer);
  Begin
    If aFile = '' Then exit;
    setlength(Steps, length(Steps) + 1);
    Steps[high(Steps)].Filename := aFile;
    Steps[high(Steps)].MaxMs := MaxMs;
  End;

  Function InSounds(Const Names: Array Of String): String;
  Var
    Rel: Array Of String;
    j: integer;
  Begin
    Rel := Nil;
    setlength(Rel, length(Names));
    For j := 0 To high(Names) Do Rel[j] := 'sounds' + PathDelim + Names[j];
    result := FirstSoundFile(Folder, Rel);
  End;

Begin
  // Probehören der gewählten Soundquelle: eine Explosion, eine Ansage und der Anfang der Menümusik, ein zweiter Klick stoppt
  If assigned(fSoundTest) And fSoundTest.Playing() Then Begin
    StopSoundTest();
    exit;
  End;
  // Ein ausdrücklicher Test: spielt immer alles, auch wenn "Musik abspielen" oder "Sounds abspielen" aus sind
  Folder := SelectedSoundSet();
  Steps := Nil;
  Add(InSounds(Explosions), 0);
  Add(InSounds(Voices), 0);
  Add(FirstSoundFile(Folder, ['res' + PathDelim + 'mainmenu_sound.wav']), MusicMs);
  If length(Steps) = 0 Then Begin
    showmessage(R_SoundTestNothing);
    exit;
  End;
  If Not assigned(fSoundTest) Then Begin
    fSoundTest := TSoundTest.Create;
    fSoundTest.OnFinished := @OnSoundTestFinished;
  End;
  If fSoundTest.Play(Steps, ini.ReadInteger(FPC_AtomicIniSection, 'Volume', 10000)) Then Begin
    fSoundTestFolder := Folder;
  End
  Else Begin
    log('Sound test: ' + fSoundTest.Error, llWarning);
    If fSoundTest.Failed Then Begin
      If fSoundTest.InitErrorCode <> 0 Then
        showmessage(format(RF_SoundTestNoDevice, [fSoundTest.InitErrorCode]))
      Else
        showmessage(R_SoundTestUnavailable + LineEnding + LineEnding + fSoundTest.Error);
    End
    Else Begin
      showmessage(R_SoundTestNothing);
    End;
  End;
  UpdateSoundTestButton();
End;

Procedure TForm1.Button12Click(Sender: TObject);
Begin
  (*
   * Gemeinsam für Grafiken... und den Doppelklick auf das Bild: öffnet den Grafik-Generator mit dem gewählten Satz.
   * Ohne Generator passiert nichts, ein einfacher Klick auf das Bild schaltet absichtlich nicht (er käme vor jedem Doppelklick).
   * Zeigt das Bild den Platzhalter, dann öffnet schon der einfache Klick die Einrichtung (Image1Click).
   *)
  If (Sender = Image1) And fNoGraphics Then exit;
  StartAssetGenerator();
End;

Procedure TForm1.Button17Click(Sender: TObject);
Begin
  RunSetup();
End;

Procedure TForm1.Image1Click(Sender: TObject);
Begin
  // Nur der Platzhalter "Noch keine Grafiken" reagiert auf den einfachen Klick
  If fNoGraphics Then RunSetup();
End;

Procedure TForm1.LabelNewsClick(Sender: TObject);
Var
  n: integer;
Begin
  (*
   * Der Hinweis "Neu: eigene Grafiken" (nur mit CD-Daten und ohne Grafiksätze): eigene erzeugen wie in der Einrichtung,
   * aber Original bleibt gewählt. Abgebrochen: der Hinweis bleibt.
   *)
  If Not SetupGenerate(true, n) Then exit;
  ini.WriteBool('Launcher', 'NewsGraphicsSeen', true);
  ini.UpdateFile;
  UpdateNewsBanner();
  If n > 0 Then showmessage(format(RF_NewsGraphicsDone, [n]));
End;

Procedure TForm1.ButtonNewsCloseClick(Sender: TObject);
Begin
  // Für immer weg
  ini.WriteBool('Launcher', 'NewsGraphicsSeen', true);
  ini.UpdateFile;
  UpdateNewsBanner();
End;

Procedure TForm1.UpdateNewsBanner;
Var
  sl: TStringList;
  ShowNews: Boolean;
Begin
  (*
   * Einmal für Spieler, die schon die CD-Daten haben (die Einrichtung erscheint dann nicht von selbst): ein schmaler
   * Hinweis über dem Seitenbild. Nicht, wenn es schon Grafiksätze gibt, die Einrichtung schon lief, er weg geklickt
   * wurde oder es keinen Generator gibt. Er liegt in der freien Fläche über dem Bild und verschiebt nichts.
   *)
  ShowNews := assigned(fAssetFolders) And (fAssetFolders.Count <= 1)
    And (Not ini.ReadBool('Launcher', 'SetupDone', false))
    And (Not ini.ReadBool('Launcher', 'NewsGraphicsSeen', false))
    And (FileExists(AssetGeneratorExe()) Or FileExists(AssetGeneratorCliExe()));
  If ShowNews Then Begin
    sl := CheckForFiles('');
    ShowNews := sl.Count = 0; // Nur mit vollständigen CD-Daten
    sl.free;
  End;
  PanelNews.Visible := ShowNews;
  // Ohne Hinweis rückt die linke Spalte (Bild, Satz- und Sound-Zeile, Probehören) nach oben bündig zu den Gruppen rechts
  If ShowNews Then
    ShiftLeftColumn(-fLeftColumnShift)
  Else
    ShiftLeftColumn(GroupBox1.Top - Image1.Top);
End;

Procedure TForm1.ShiftLeftColumn(Delta: Integer);
Begin
  // Die Zeilen darunter (Satz, Sounds, Probehören) hängen per Anker am Bild und rücken mit
  If Delta = 0 Then exit;
  Image1.Top := Image1.Top + Delta;
  fLeftColumnShift := fLeftColumnShift + Delta;
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
  If assigned(fJobs) And (fJobs.Count <> 0) Then Begin
    (*
     * Es werden noch Stile der Einrichtung erzeugt: ihre Ausgabe muss weiter gelesen werden, sonst bleiben sie stehen.
     * Also nur verstecken und beenden, wenn sie fertig sind (OnJobTimer).
     *)
    CanClose := false;
    fCloseAfterJobs := true;
    StoreSettings();
    ini.UpdateFile;
    Form2.Hide;
    Hide;
    exit;
  End;
  fJobTimer.Enabled := false;
  fJobs.free;
  fJobs := Nil;
  fSoundTest.Free; // Gibt auch BASS frei
  fSoundTest := Nil;
  fSoundFolders.free;
  fSoundFolders := Nil;
  fSoundNames.free;
  fSoundNames := Nil;
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
  ReleaseSoundTest(); // Das Spiel braucht das Audiogerät
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
  fNoGraphics := false;
  Image1.Cursor := crDefault;
  Image1.ShowHint := Button12.Visible;
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
  End
  Else Begin
    // Weder CD-Daten noch ein Bild des Satzes: der Platzhalter, ein Klick darauf öffnet die Einrichtung
    ShowNoGraphicsImage();
  End;
End;

Procedure TForm1.ShowNoGraphicsImage;
Const
  w = 192;
  h = 288;
Var
  b: TBitmap;

  Procedure CenterText(y: integer; Const s: String);
  Begin
    b.Canvas.TextOut((w - b.Canvas.TextWidth(s)) Div 2, y, s);
  End;

Begin
  fNoGraphics := true;
  b := TBitmap.Create;
  Try
    b.SetSize(w, h);
    b.Canvas.Brush.Color := RGBToColor(40, 40, 48);
    b.Canvas.FillRect(0, 0, w, h);
    b.Canvas.Pen.Color := RGBToColor(120, 120, 140);
    b.Canvas.Pen.Style := psDash;
    b.Canvas.Brush.Style := bsClear;
    b.Canvas.Rectangle(8, 8, w - 8, h - 8);
    b.Canvas.Pen.Style := psSolid;
    b.Canvas.Font.Color := clWhite;
    b.Canvas.Font.Style := [fsBold];
    b.Canvas.Font.Height := -14;
    CenterText(h Div 2 - 24, R_NoGraphicsYet);
    b.Canvas.Font.Style := [];
    b.Canvas.Font.Height := -12;
    b.Canvas.Font.Color := RGBToColor(200, 200, 220);
    CenterText(h Div 2 + 4, R_NoGraphicsYetClick);
    Image1.Picture.Assign(b);
  Finally
    b.free;
  End;
  Image1.Cursor := crHandPoint;
  Image1.ShowHint := false;
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
    If SameFolderName(fAssetFolders[i], Folder) Then Begin
      fAssetIndex := i;
      break;
    End;
  End;
  // Den Generator gibt es evtl. noch nicht, dann gibt es auch keinen Knopf und keinen Doppelklick
  Button12.Visible := FileExists(AssetGeneratorExe());
  Button10.Enabled := fAssetFolders.Count > 1;
  Button11.Enabled := fAssetFolders.Count > 1;
  SelectAssetSet(fAssetIndex);
  ReadSoundSets(); // Sätze mit Sounds können dazu gekommen oder weg sein
  UpdateNewsBanner(); // Mit Grafiksätzen braucht es den Hinweis nicht mehr
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
  UpdateSoundCaption(); // Automatisch hängt vom Grafiksatz ab
End;

Function TForm1.AssetGeneratorExe: String;
Begin
  result := AppDir() + 'asset_generator_gui'{$IFDEF Windows} + '.exe'{$ENDIF};
End;

Function TForm1.AssetGeneratorCliExe: String;
Begin
  result := AppDir() + 'asset_generator_cli'{$IFDEF Windows} + '.exe'{$ENDIF};
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

Procedure TForm1.ReadSoundSets;
Begin
  // Automatisch, Original und alle Sätze mit Sounds, gewählt ist [Launcher] SoundSet (wie in den Spieloptionen, ComboBox6)
  If Not assigned(fSoundFolders) Then exit;
  ListSoundChoices(fSoundFolders, fSoundNames, R_SoundSetAuto, R_SoundSetOriginal);
  Button14.Enabled := fSoundFolders.Count > 1;
  Button15.Enabled := fSoundFolders.Count > 1;
  fSoundIndex := SoundChoiceIndex(fSoundFolders, ini.ReadString('Launcher', 'SoundSet', SoundSetAuto));
  UpdateSoundCaption();
End;

Procedure TForm1.SelectSoundSet(Index: integer);
Begin
  If (Index < 0) Or (Index >= fSoundFolders.Count) Then Index := 1;
  If Index >= fSoundFolders.Count Then exit;
  fSoundIndex := Index;
  // Gleich speichern, die selbe Einstellung nutzen der Dialog Spieloptionen und der Start des Spiels
  If ini.ReadString('Launcher', 'SoundSet', SoundSetAuto) <> fSoundFolders[fSoundIndex] Then Begin
    ini.WriteString('Launcher', 'SoundSet', fSoundFolders[fSoundIndex]);
  End;
  UpdateSoundCaption();
  UpdateStatus(); // "(keine Sounds)" hängt von der Soundquelle ab
End;

Procedure TForm1.UpdateSoundCaption;
Var
  Folder, Plays, s: String;
Begin
  If (Not assigned(fSoundFolders)) Or (fSoundIndex < 0) Or (fSoundIndex >= fSoundFolders.Count) Then exit;
  Label12.Caption := format(RF_SoundSet, [fSoundNames[fSoundIndex]]);
  // Im Tooltip, was tatsächlich gespielt wird (Automatisch hängt vom Grafiksatz ab)
  Folder := SelectedSoundSet();
  If Folder <> '' Then Begin
    Plays := AssetSetDisplayName(Folder);
  End
  Else If HasOriginalSounds() Then Begin
    Plays := R_SoundSetOriginal;
  End
  Else Begin
    Plays := R_NoSounds;
  End;
  // Bei Automatisch steht die tatsächliche Quelle kurz dabei, wenn das zwischen die Pfeile passt (sonst nur im Tooltip)
  If (fSoundIndex = 0) And HandleAllocated And ((Folder <> '') Or HasOriginalSounds()) Then Begin
    If Folder <> '' Then Begin
      s := format(RF_SoundSetAutoShort, [AssetSetDisplayName(Folder)]);
    End
    Else Begin
      s := format(RF_SoundSetAutoShort, [R_AssetSetOriginal]);
    End;
    // Platz zwischen den Pfeilen (Label12.Width taugt nicht, das Label passt seine Breite dem Text an)
    Label12.Canvas.Font.Assign(Label12.Font);
    If Label12.Canvas.TextWidth(s) <= Min(Label12.ClientWidth, Button15.Left - (Button14.Left + Button14.Width)) - 8 Then Label12.Caption := s;
  End;
  // Passt der Text nicht zwischen die Pfeile (breitere Schrift unter GTK), nur der Name ohne "Sounds:"
  If HandleAllocated Then Begin
    Label12.Canvas.Font.Assign(Label12.Font);
    If Label12.Canvas.TextWidth(Label12.Caption) > Min(Label12.ClientWidth, Button15.Left - (Button14.Left + Button14.Width)) - 8 Then
      Label12.Caption := fSoundNames[fSoundIndex];
  End;
  Label12.Hint := R_SoundSetHint + LineEnding + LineEnding + format(RF_SoundSetPlays, [Plays]);
  // Hört man gerade eine andere Quelle probe, dann aufhören
  If assigned(fSoundTest) And fSoundTest.Playing() And (Folder <> fSoundTestFolder) Then StopSoundTest();
  UpdateSoundTestButton();
End;

Procedure TForm1.UpdateSoundTestButton;
Begin
  // Ohne BASS (bass.dll / libbass.so) oder wenn es sich nicht starten lässt, gibt es kein Probehören
  If (assigned(fSoundTest) And fSoundTest.Failed) Or ((Not assigned(fSoundTest)) And (Not SoundLibraryAvailable())) Then Begin
    Button16.Enabled := false;
    Button16.Caption := R_SoundTest;
    Button16.Hint := R_SoundTestUnavailable;
    If assigned(fSoundTest) And (fSoundTest.Error <> '') Then Button16.Hint := Button16.Hint + LineEnding + fSoundTest.Error;
    exit;
  End;
  Button16.Enabled := true;
  Button16.Hint := R_SoundTestHint;
  If assigned(fSoundTest) And fSoundTest.Playing() Then Begin
    Button16.Caption := R_SoundTestStop;
  End
  Else Begin
    Button16.Caption := R_SoundTest;
  End;
End;

Procedure TForm1.StopSoundTest;
Begin
  If assigned(fSoundTest) Then fSoundTest.Stop(); // Ruft OnSoundTestFinished
End;

Procedure TForm1.ReleaseSoundTest;
Begin
  // Stoppen und BASS frei geben (das Audiogerät), beim nächsten Probehören wird es wieder geladen
  If Not assigned(fSoundTest) Then exit;
  fSoundTest.Free;
  fSoundTest := Nil;
  UpdateSoundTestButton();
End;

Procedure TForm1.OnSoundTestFinished(Sender: TObject);
Begin
  If Not assigned(fSoundFolders) Then exit; // Schon beim Schließen
  UpdateSoundTestButton();
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

Function TForm1.RunGenerator(p: TProcessUTF8): Boolean;
Var
  Line: String;
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
   * Startet den Generator p (GUI oder Kommandozeile) und wartet auf sein Ende, Result = Exit-Code 0.
   * Wie beim CD Extraktor ist der Launcher solange gesperrt, zeichnet sich aber neu (und liest die Ausgabe von Stilen,
   * die im Hintergrund erzeugt werden, weiter).
   *)
  result := false;
  Line := '';
  Buffer[0] := #0; // Beruhigt den Compiler
  StoreSettings();
  ini.UpdateFile;
  ReleaseSoundTest();
  Enabled := false;
  Screen.Cursor := crHourGlass;
  Label5.Caption := R_GeneratingGraphics;
  Label5.Font.Color := clWindowText;
  Try
    p.CurrentDirectory := AppDir();
    Try
      p.Execute;
      While p.Running Do Begin
        ReadOutput();
        Application.ProcessMessages;
        Sleep(20);
      End;
      ReadOutput();
      result := p.ExitCode = 0;
      If Not result Then Begin
        // Auch Abbrechen in der Stil-Auswahl, das ist kein Fehler: nur ins Log, ohne das Log-Fenster
        QuietLog := true;
        log('Graphics generator ended with exit code ' + inttostr(p.ExitCode));
        QuietLog := false;
      End;
    Except
      On av: Exception Do Begin
        log('Could not run the graphics generator: ' + av.Message);
        result := false;
      End;
    End;
  Finally
    Screen.Cursor := crDefault;
    Enabled := true;
  End;
End;

// Der Ordner aus last_set.txt (schreibt die GUI nach dem Erzeugen), '' = keiner
Function ReadLastSet(): String;
Var
  sl: TStringList;
Begin
  result := '';
  If Not FileExists(AssetSetsDir() + 'last_set.txt') Then exit;
  sl := TStringList.Create;
  Try
    Try
      sl.LoadFromFile(AssetSetsDir() + 'last_set.txt');
      If sl.Count > 0 Then result := trim(sl[0]);
    Except
      result := '';
    End;
  Finally
    sl.free;
  End;
End;

Function TForm1.GenerateAssetSet: Boolean;
Var
  p: TProcessUTF8;
  Folder, s: String;
  n: integer;
  UseGui: Boolean;
Begin
  (*
   * Erzeugt einen neuen Grafiksatz (mit Schemes) in assets/Generated (Generated2, ... wenn es den schon gibt),
   * wählt ihn aus und kopiert seine Schemes nach data/schemes (wenn dort keine sind). Result = das Spiel kann jetzt starten.
   * - asset_generator_gui --auto --set <Ordner>: zeigt die Stil-Auswahl und den Fortschritt, schreibt den Ordner nach
   *   assets/last_set.txt, Exit-Code 0 = erzeugt, sonst abgebrochen oder Fehler.
   * - Ohne GUI: asset_generator_cli (Kommandozeile) mit zufälligem Seed im Stil "flat", der Fortschritt steht im Status.
   *)
  result := false;
  UseGui := FileExists(AssetGeneratorExe());
  If (Not UseGui) And (Not FileExists(AssetGeneratorCliExe())) Then exit;
  Folder := 'Generated';
  n := 1;
  While DirectoryExists(AssetSetsDir() + Folder) Do Begin
    inc(n);
    Folder := 'Generated' + inttostr(n);
  End;
  If FileExists(AssetSetsDir() + 'last_set.txt') Then DeleteFile(AssetSetsDir() + 'last_set.txt'); // Nur was dieser Lauf schreibt
  p := TProcessUTF8.Create(Nil);
  Try
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
    result := RunGenerator(p);
  Finally
    p.free;
  End;
  If result And UseGui Then Begin
    // Die GUI schreibt den tatsächlich erzeugten Ordner hierher
    s := ReadLastSet();
    If s <> '' Then Folder := s;
  End;
  If (Not result) And UseGui Then Begin
    // Abbrechen in der Stil-Auswahl ist kein Fehler (wie bei der Einrichtung), Fehler zeigt die GUI selbst
    UpdateStatus();
    exit;
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

// Ein Ordnername aus einem Namen: nur a-z 0-9 - _, Umlaute und Akzente vereinfacht
Function FolderFromName(Const Name: String): String;
Const
  Map: Array[0..21] Of Array[0..1] Of String = (
    ('ä', 'ae'), ('ö', 'oe'), ('ü', 'ue'), ('Ä', 'Ae'), ('Ö', 'Oe'), ('Ü', 'Ue'), ('ß', 'ss'),
    ('é', 'e'), ('è', 'e'), ('ê', 'e'), ('É', 'E'), ('à', 'a'), ('â', 'a'), ('á', 'a'),
    ('í', 'i'), ('î', 'i'), ('ó', 'o'), ('ô', 'o'), ('ú', 'u'), ('û', 'u'), ('ñ', 'n'), ('ç', 'c'));
Var
  s: String;
  i: integer;
Begin
  s := Name;
  For i := 0 To high(Map) Do s := StringReplace(s, Map[i][0], Map[i][1], [rfReplaceAll]);
  result := '';
  For i := 1 To length(s) Do Begin
    Case s[i] Of
      'a'..'z', 'A'..'Z', '0'..'9', '-', '_': result := result + s[i];
      ' ', '.': If (result <> '') And (result[length(result)] <> '_') Then result := result + '_';
    End;
  End;
  While (result <> '') And (result[length(result)] = '_') Do delete(result, length(result), 1);
  If result = '' Then result := 'Set';
End;

Procedure TForm1.RunSetup;
Var
  r: TModalResult;
  n: integer;
Begin
  (*
   * Die Einrichtung: woher kommen Grafiken und Sounds (Form7). Eigene erzeugen: die Stil-Auswahl des Generators, wird
   * sie abgebrochen, erscheint die Einrichtung wieder. Von der CD: der CD Extraktor. Später: nichts.
   * Danach erscheint sie nicht mehr von selbst ([Launcher] SetupDone), sondern nur über "Einrichtung..." oder --setup.
   *)
  Repeat
    ini.WriteBool('Launcher', 'SetupDone', true);
    ini.UpdateFile;
    UpdateNewsBanner(); // Die Einrichtung ersetzt den Hinweis
    r := Form7.Execute(FileExists(AssetGeneratorExe()) Or FileExists(AssetGeneratorCliExe()),
      FileExists(AppDir() + 'cd_data_extractor'{$IFDEF Windows} + '.exe'{$ENDIF}));
    Case r Of
      mrYes: If SetupGenerate(false, n) Then break;
      mrNo: Begin
          Button4Click(Nil);
          ReadAssetSets(); // Mit den CD-Daten gibt es das Original (und das Seitenbild)
          break;
        End;
    Else
      break;
    End;
  Until false;
End;

Procedure TForm1.StartSetupAsync(Data: PtrInt);
Begin
  RunSetup();
End;

Type
  TStyleJob = Class
    Process: TProcessUTF8;
    Folder: String;
  End;

// True, wenn assets/<Folder> ein Satz ist, den die Einrichtung erzeugt hat ([General] Origin=setup), den darf sie ersetzen
Function IsSetupSet(Const Folder: String): Boolean;
Var
  pini: TIniFile;
Begin
  result := false;
  If Not FileExists(AssetSetsDir() + Folder + PathDelim + AssetProfileFile) Then exit;
  pini := TIniFile.Create(AssetSetsDir() + Folder + PathDelim + AssetProfileFile);
  Try
    result := SameText(trim(pini.ReadString('General', 'Origin', '')), 'setup');
  Finally
    pini.free;
  End;
End;

Function TForm1.SetupGenerate(KeepSelection: Boolean; Out Count: integer): Boolean;
Const
  Styles: Array[0..3] Of String = ('flat', 'soft', 'pixel', 'retro'); // Reihenfolge wie TAGStyle im Generator
Var
  Names, Folders: Array[0..3] Of String;
  Taken: TStringList;
  i, n: integer;
  p: TProcessUTF8;
  First, FirstStyle, s, f, ns: String;
  UseGui: Boolean;
  pini: TIniFile;
Begin
  (*
   * Eigene Grafiken (Einrichtung): ein Satz pro Stil, Ordner und Name immer englisch (Flat, Soft, Pixel, Retro), angezeigt
   * wird der Name des Stils in der aktuellen Sprache (AssetSetDisplayName, Origin=setup). Gibt es den Ordner schon, dann
   * wird er ersetzt, wenn ihn eine frühere Einrichtung erzeugt hat (profile.ini [General] Origin=setup), sonst (eigener,
   * im Editor gespeicherter Satz) z.B. Flat2. Vertrag mit dem Generator:
   * 1. asset_generator_gui --auto --setup --folders <f,s,p,r> --names <f,s,p,r>: die Stil-Auswahl, der gewählte Stil wird
   *    in seinen Ordner erzeugt, der Ordner steht danach in assets/last_set.txt, Exit-Code 0 = erzeugt.
   *    (Ohne GUI: asset_generator_cli mit Stil flat und zufälligem Seed.)
   * 2. Der Satz wird gewählt (Sounds Automatisch), man kann sofort spielen. KeepSelection (Hinweis "Neu: eigene Grafiken"):
   *    die Auswahl bleibt (z.B. Original).
   * 3. Die anderen drei Stile erzeugt asset_generator_cli im Hintergrund mit --profile <profile.ini des ersten Satzes> (gleicher
   *    Seed, gleiche Sprache der Ansagen), --style und --name; parallel, OnJobTimer liest die Sätze neu ein, wenn sie fertig sind.
   * Result = false: abgebrochen oder Fehler (die Einrichtung erscheint wieder). Count = die neuen Sätze (mit denen, die
   * noch im Hintergrund entstehen).
   *)
  result := false;
  Count := 0;
  UseGui := FileExists(AssetGeneratorExe());
  If (Not UseGui) And (Not FileExists(AssetGeneratorCliExe())) Then exit;
  For i := 0 To 3 Do Begin
    Names[i] := AssetStyleEnglishName(Styles[i]); // Ordner und profile.ini Name englisch, angezeigt wird übersetzt
  End;
  Taken := TStringList.Create;
  Try
    Taken.CaseSensitive := false;
    // Ein Satz, der gerade noch im Hintergrund entsteht (frühere Einrichtung), wird nicht angefasst
    For i := 0 To fJobs.Count - 1 Do Begin
      Taken.Add(TStyleJob(fJobs[i]).Folder);
    End;
    For i := 0 To 3 Do Begin
      f := FolderFromName(Names[i]);
      s := f;
      ns := Names[i];
      n := 1;
      // Vorhandene Sätze werden nur ersetzt, wenn sie von der Einrichtung sind (das Speichern ist atomar)
      While (DirectoryExists(AssetSetsDir() + s) And Not IsSetupSet(s)) Or (Taken.IndexOf(s) >= 0) Do Begin
        inc(n);
        s := f + inttostr(n);
        ns := Names[i] + ' ' + inttostr(n);
      End;
      Folders[i] := s;
      Names[i] := ns;
      Taken.Add(s);
    End;
  Finally
    Taken.free;
  End;
  If FileExists(AssetSetsDir() + 'last_set.txt') Then DeleteFile(AssetSetsDir() + 'last_set.txt');
  First := '';
  p := TProcessUTF8.Create(Nil);
  Try
    If UseGui Then Begin
      p.Executable := AssetGeneratorExe();
      p.Parameters.Add('--auto');
      p.Parameters.Add('--setup');
      p.Parameters.Add('--folders');
      p.Parameters.Add(Folders[0] + ',' + Folders[1] + ',' + Folders[2] + ',' + Folders[3]);
      p.Parameters.Add('--names');
      p.Parameters.Add(Names[0] + ',' + Names[1] + ',' + Names[2] + ',' + Names[3]);
      p.Options := [];
    End
    Else Begin
      p.Executable := AssetGeneratorCliExe();
      p.Parameters.Add('--out');
      p.Parameters.Add(AssetSetsDir() + Folders[0]);
      p.Parameters.Add('--name');
      p.Parameters.Add(Names[0]);
      p.Parameters.Add('--seed');
      p.Parameters.Add('random');
      p.Parameters.Add('--style');
      p.Parameters.Add(Styles[0]);
      p.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    End;
    If Not RunGenerator(p) Then Begin
      // Abgebrochen (oder die GUI hat einen Fehler schon gemeldet)
      UpdateStatus();
      exit;
    End;
  Finally
    p.free;
  End;
  If UseGui Then Begin
    First := ReadLastSet();
  End
  Else Begin
    First := Folders[0];
  End;
  If (First = '') Or Not FileExists(AssetSetsDir() + First + PathDelim + AssetProfileFile) Then Begin
    UpdateStatus();
    showmessage(R_GenerateFailed);
    result := true; // Nicht noch einmal fragen
    exit;
  End;
  // Ohne GUI (Kommandozeile): der Satz ist auch von der Einrichtung, die anderen Stile erben das über --profile
  pini := TIniFile.Create(AssetSetsDir() + First + PathDelim + AssetProfileFile);
  Try
    If pini.ReadString('General', 'Origin', '') = '' Then pini.WriteString('General', 'Origin', 'setup');
  Finally
    pini.free;
  End;
  // Den ersten Satz wählen, Sounds Automatisch (= die Sounds dieses Satzes)
  If Not KeepSelection Then Begin
    ini.WriteString('Launcher', 'AssetSet', First);
    ini.WriteString('Launcher', 'SoundSet', SoundSetAuto);
  End;
  ini.UpdateFile;
  ReadAssetSets();
  PrepareSchemes();
  result := true;
  Count := 1;
  // Die anderen Stile im Hintergrund, mit dem Profil (Seed) des ersten Satzes
  If Not FileExists(AssetGeneratorCliExe()) Then Begin
    UpdateStatus();
    exit;
  End;
  pini := TIniFile.Create(AssetSetsDir() + First + PathDelim + AssetProfileFile);
  Try
    FirstStyle := lowercase(trim(pini.ReadString('General', 'Style', 'flat')));
  Finally
    pini.free;
  End;
  For i := 0 To 3 Do Begin
    If (Styles[i] = FirstStyle) Or SameFolderName(Folders[i], First) Then Continue;
    If StartStyleJob(Folders[i], Names[i], Styles[i], AssetSetsDir() + First + PathDelim + AssetProfileFile) Then inc(Count);
  End;
  UpdateStatus();
End;

Function TForm1.StartStyleJob(Const Folder, aName, Style, Profile: String): Boolean;
Var
  j: TStyleJob;
Begin
  result := false;
  j := TStyleJob.Create;
  j.Folder := Folder;
  j.Process := TProcessUTF8.Create(Nil);
  j.Process.Executable := AssetGeneratorCliExe();
  j.Process.CurrentDirectory := AppDir();
  j.Process.Parameters.Add('--out');
  j.Process.Parameters.Add(AssetSetsDir() + Folder);
  j.Process.Parameters.Add('--profile');
  j.Process.Parameters.Add(Profile);
  j.Process.Parameters.Add('--style');
  j.Process.Parameters.Add(Style);
  j.Process.Parameters.Add('--name');
  j.Process.Parameters.Add(aName);
  j.Process.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  Try
    j.Process.Execute;
  Except
    On av: Exception Do Begin
      QuietLog := true;
      log('Could not start the graphics generator for ' + Folder + ': ' + av.Message);
      QuietLog := false;
      j.Process.free;
      j.free;
      exit;
    End;
  End;
  fJobs.Add(j);
  fJobTimer.Enabled := true;
  result := true;
End;

Procedure TForm1.OnJobTimer(Sender: TObject);
Var
  i, c: integer;
  j: TStyleJob;
  SetsChanged: Boolean;
  Buffer: Array[0..4095] Of char;
Begin
  If Not assigned(fJobs) Then exit;
  SetsChanged := false;
  Buffer[0] := #0; // Beruhigt den Compiler
  For i := fJobs.Count - 1 Downto 0 Do Begin
    j := TStyleJob(fJobs[i]);
    // Die Ausgabe nur lesen, damit die Pipe nicht voll läuft
    While j.Process.Output.NumBytesAvailable > 0 Do Begin
      c := j.Process.Output.NumBytesAvailable;
      If c > SizeOf(Buffer) Then c := SizeOf(Buffer);
      If j.Process.Output.Read(Buffer, c) <= 0 Then break;
    End;
    // Solange ein Generator oder der Extraktor läuft (Launcher gesperrt), werden fertige Jobs erst danach übernommen
    If j.Process.Running Or (Not Enabled) Then Continue;
    If j.Process.ExitCode <> 0 Then Begin
      QuietLog := true;
      log(format('Graphics generator for %s ended with exit code %d', [j.Folder, j.Process.ExitCode]));
      QuietLog := false;
    End;
    j.Process.free;
    j.free;
    fJobs.Delete(i);
    SetsChanged := true;
  End;
  If fJobs.Count = 0 Then fJobTimer.Enabled := false;
  If fCloseAfterJobs Then Begin
    If fJobs.Count = 0 Then Close;
    exit;
  End;
  If SetsChanged Then Begin
    ReadAssetSets(); // Der gewählte Satz bleibt ([Launcher] AssetSet), die neuen kommen in die Liste
    UpdateStatus();
  End;
End;

Procedure TForm1.StartAssetGenerator;
Var
  P: TProcessUTF8;
Begin
  If Not FileExists(AssetGeneratorExe()) Then exit;
  StoreSettings();
  ini.UpdateFile;
  ReleaseSoundTest(); // Der Generator ersetzt evtl. die Sounds des Satzes (die dürfen dabei nicht offen sein) und spielt selbst welche
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
    RunAndWait(p);
  Finally
    p.free;
  End;
  ReadAssetSets();
End;

Procedure TForm1.RunAndWait(p: TProcessUTF8);
Begin
  // Wie poWaitOnExit, aber der Timer liest weiter die Ausgabe der Stile, die im Hintergrund erzeugt werden (sonst läuft deren Pipe voll)
  Enabled := false;
  Try
    p.Execute;
    While p.Running Do Begin
      Application.ProcessMessages;
      Sleep(20);
    End;
  Finally
    Enabled := true;
  End;
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
  CheckBox4.Checked := ini.ReadBool(FPC_AtomicIniSection, 'PlaySounds', true); // Nur die Musik
  CheckBox8.Checked := ini.ReadBool(FPC_AtomicIniSection, 'PlaySoundEffects', true); // Wie "Soundeffekte abspielen" in den Spieloptionen
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
  // Ohne Spieldaten kann der Grafik-Generator Sätze erzeugen (die Einrichtung, der Launch Knopf macht das auch), dann ist sie fett
  CanGenerate := (sl.Count <> 0) And (FileExists(AssetGeneratorExe()) Or FileExists(AssetGeneratorCliExe()));
  If CanGenerate Then Begin
    Button17.Font.Style := [fsBold];
  End
  Else Begin
    Button17.Font.Style := [];
  End;
  // Die fehlenden Dateien stehen im Tooltip
  Label5.Hint := trim(sl.Text);
  Label5.ShowHint := sl.Count <> 0;
  If (sl.Count = 0) And (Not HasOriginalSounds()) And (SelectedSoundSet() = '') Then Begin
    Label5.Hint := R_NoSoundsHint;
    Label5.ShowHint := true;
  End;
  GameDataFound := sl.Count = 0;
  sl.free;
  // Die übrigen Stile der Einrichtung werden noch erzeugt (spielen geht schon)
  If assigned(fJobs) And (fJobs.Count <> 0) Then Begin
    Label5.Caption := format(RF_MoreStyles, [fJobs.Count]);
    Label5.Font.Color := clWindowText;
    Label5.Hint := R_MoreStylesHint;
    Label5.ShowHint := true;
  End;
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
  Mode: TGameMode;
  AiLib, AiName, Lib: String;
  Smart: TIniFile;
Begin
  // Der aktive Spielmodus steht neben dem Knopf, der Name ist der übersetzte aus dem Dialog
  Settings.Handicap := Nil;
  LoadModSettingsFromFile(GetAtomicModsIniFile(), Settings);
  Settings.Handicap.free;
  Label10.Caption := form6.Caption + ': ' + form6.GameModeName(Settings.GameMode);
  (*
   * Auf dem Start Knopf stehen ein anderer Spielmodus als der normale (so wie er gespielt wird, Team play = normal)
   * und immer die KI, die der gehostete Server lädt (die Smart KI mit ihrer Schwierigkeit)
   *)
  If Label7.Caption <> fLaunchShown Then fLaunchCaption := Label7.Caption; // Neu übersetzt
  Mode := EffectiveGameMode(Settings, ini.ReadBool(FPC_AtomicIniSection, 'TeamPlay', false));
  AiLib := trim(ini.ReadString('Launcher', 'AiLib', ''));
{$IFDEF Windows}
  Lib := AppDir() + AiLib + '.dll';
{$ELSE}
  Lib := AppDir() + 'lib' + AiLib + '.so';
{$ENDIF}
  If (AiLib = '') Or (AiLib = 'ai') Or (Not FileExists(Lib)) Then Begin
    AiName := R_AiOriginal; // Fehlt die gewählte KI, nimmt der Server auch die Original KI
  End
  Else If AiLib = 'ai_smart' Then Begin
    AiName := R_AiSmart + ' ' + IntToStr(DefaultDifficulty) + '%';
    If FileExists(SmartIniFile()) Then Begin
      Smart := TIniFile.Create(SmartIniFile());
      Try
        AiName := R_AiSmart + ' ' + IntToStr(Max(0, Min(100, Smart.ReadInteger('General', 'Difficulty', DefaultDifficulty)))) + '%'; // Wie die KI: 0..100
      Finally
        Smart.free;
      End;
    End;
  End
  Else Begin
    AiName := AiLibDisplayName(AiLib);
  End;
  AiName := format(RF_LaunchAi, [AiName]);
  If Mode = gmClassic Then Begin
    Label7.Caption := fLaunchCaption + ' (' + AiName + ')';
  End
  Else Begin
    Label7.Caption := fLaunchCaption + ' (' + form6.GameModeName(Mode) + ', ' + AiName + ')';
  End;
  fLaunchShown := Label7.Caption;
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  i: Integer;
  WantSetup: Boolean;
  sl: TStringList;
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
  Language.AddIgnoredIdentifier('Form1.Label12.Caption'); // Die Soundquelle, setzt UpdateSoundCaption
  Language.AddIgnoredIdentifier('Form1.Label12.Hint');
  Language.AddIgnoredIdentifier('Form1.Button14.Caption');
  Language.AddIgnoredIdentifier('Form1.Button15.Caption');
  Language.AddIgnoredIdentifier('Form1.Button16.Caption'); // Probehören, setzt UpdateSoundTestButton
  Language.AddIgnoredIdentifier('Form1.Button16.Hint');
  Language.AddIgnoredIdentifier('Form1.ButtonNewsClose.Caption'); // Nur das Zeichen ×
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
  (*
   * Die Einrichtung: mit --setup (z.B. vom Spiel, wenn es keine Grafiken findet) immer, sonst einmal beim ersten Start,
   * wenn es weder CD-Daten noch einen Grafiksatz gibt. Erst wenn das Hauptfenster zu sehen ist.
   *)
  WantSetup := false;
  For i := 1 To ParamCount Do Begin
    If SameText(ParamStr(i), '--setup') Or SameText(ParamStr(i), '-setup') Then WantSetup := true;
  End;
  If (Not WantSetup) And (Not ini.ReadBool('Launcher', 'SetupDone', false)) And (fAssetFolders.Count <= 1) Then Begin
    sl := CheckForFiles('');
    WantSetup := sl.Count <> 0;
    sl.free;
  End;
  If WantSetup Then Application.QueueAsyncCall(@StartSetupAsync, 0);
  UpdateNewsBanner();
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
    // Die Sätze der Einrichtung heißen wie ihr Stil in der Sprache
    For Index := 1 To fAssetNames.Count - 1 Do Begin
      fAssetNames[Index] := AssetSetDisplayName(fAssetFolders[Index]);
    End;
    SelectAssetSet(fAssetIndex);
  End;
  If fSoundNames.Count > 1 Then Begin
    fSoundNames[0] := R_SoundSetAuto; // Die Namen von Automatisch und Original kommen aus der Sprache
    fSoundNames[1] := R_SoundSetOriginal;
    For Index := 2 To fSoundNames.Count - 1 Do Begin
      fSoundNames[Index] := AssetSetDisplayName(fSoundFolders[Index]);
    End;
  End;
  FitLayout(self); // Platz für die Texte im aktuellen Widgetset und in der aktuellen Sprache
  UpdateSoundCaption(); // erst nach FitLayout, sonst wird mit den Breiten vor dem Layout gemessen
End;

Procedure TForm1.StoreSettings;
Begin
  ini.WriteBool(FPC_AtomicIniSection, 'ShowFPS', CheckBox1.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'Fullscreen', CheckBox2.Checked);

  ini.WriteBool(FPC_AtomicIniSection, 'PlaySounds', CheckBox4.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'PlaySoundEffects', CheckBox8.Checked);
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

