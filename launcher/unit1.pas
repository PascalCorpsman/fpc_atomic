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
  IniFiles, ulauncher;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
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
    Panel1: TPanel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure CheckBox3Change(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Panel1MouseEnter(Sender: TObject);
    Procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    ini: TIniFile;
    Atomic_Version: TVersion;
    ProtocollVersion: integer;
    Version: Single;
    UpToDate: Boolean; // True, wenn "Updates..." in diesem Lauf nichts mehr zum Herunterladen gefunden hat
    GameDataFound: Boolean; // True, wenn CheckForFiles nichts vermisst, dann ist der Launch Knopf blau

    Procedure LoadSettings();
    Procedure UpdateStatus();
    Procedure StoreSettings();

    Procedure LoadSideImage();
    Procedure OnDoLog(Const LogText: String);
  public
    Procedure StoreVersion(NewVersion: single);

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses UTF8Process, process, Unit2, Unit3, Unit4, LCLType, lclintf
  , ukeyboarddialog, uatomic_common, usynapsedownloader, uatomic_global
{$IFDEF Windows}
  , LResources
  , ssl_openssl_lib, ssl_openssl, blcksock
{$ENDIF}
  ;

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
  Atomic_Version := TVersion.Create();
  UpToDate := false;
  LoadSideImage();
  LoadSettings();
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
  Edit1Change(Nil);
  UpdateStatus();
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
    showmessage('Error, installation not complete, please run "Check for updates"');
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
  End;
  dialog.free;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  s: String;
Begin
  // Open Config folder
  s := GetAtomicConfigFile();
  s := ExtractFileDir(s);
  OpenURL(s);
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  // Mods, der Dialog liest und schreibt atomic_server.ini selbst
  form4.ShowModal;
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
    Label4.Caption := '✓ valid name';
    Label4.Font.Color := clGreen;
  End
  Else Begin
    Label4.Caption := '✗ a-z 0-9 _ space, max. 19';
    Label4.Font.Color := clRed;
  End;
End;

Procedure TForm1.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  // Der Launch "Knopf" ist ein Panel (nur so lässt er sich blau färben), daher Alt+L hier von Hand
  If (ssAlt In Shift) And (key = VK_L) Then Begin
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
  If GameDataFound Then Begin
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
    // Ohne Spieldaten sieht er aus wie ein normaler Knopf, ein Klick zeigt die fehlenden Dateien
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
  Atomic_Version.free;
  Atomic_Version := Nil;
  ini.free;
  ini := Nil;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  P: TProcessUTF8;
  sl: TStringList;
Begin
  If Not ValidNodeName(trim(Edit1.text)) Then Begin
    showmessage(Edit1.text + LineEnding + 'is not a valid nodename, (need to be less than 20 chars and no spezial chars!');
    exit;
  End;
  // Launch
  StoreSettings();
  ini.UpdateFile;
  sl := CheckForFiles();
  If sl.count <> 0 Then Begin
    showmessage('Error, the following files are missing:' + LineEnding +
      sl.Text + LineEnding +
      'Please re run cd_data_extractor');
    sl.free;
    exit;
  End;
  sl.free;
  // Short Prechecks
  If (Not FileExists('fpc_atomic'{$IFDEF Windows} + '.exe'{$ENDIF})) Or
    (Not FileExists('atomic_server'{$IFDEF Windows} + '.exe'{$ENDIF})) Then Begin
    showmessage('Error, installation not complete, please run "Check for updates"');
    exit;
  End;
  // Run the App ;)
  p := TProcessUTF8.Create(Nil);
  p.Options := [poDetached];
  p.Executable := 'fpc_atomic'{$IFDEF Windows} + '.exe'{$ENDIF};
  If CheckBox3.Checked Then Begin
    p.Parameters.Add('-ip');
    p.Parameters.Add(Edit2.Text);
    p.Parameters.Add('-port');
    p.Parameters.Add(Edit3.Text);
  End;
  p.Execute;
  p.free;
  close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  tmpFolder: String;
  dl: TSynapesDownloader;
Begin
{$IFDEF Linux}
  If DirectoryExists('cd_data_extractor') Then Begin
    If id_yes = Application.MessageBox('In the launcher location there is a folder named "cd_data_extractor", this will crash the update process' + LineEnding +
      'Do you want to rename that folder ?', 'Warning', MB_YESNO) Then Begin
      If Not RenameFile('cd_data_extractor' + PathDelim, 'cd_data_extractor_' + PathDelim) Then Begin
        showmessage('Error, could not rename.');
        exit;
      End;
    End
    Else Begin
      exit;
    End;
  End;
{$ENDIF}
  ClearLog();
  // 2. Download der Version Info
  tmpFolder := IncludeTrailingPathDelimiter(GetTempDir()) + 'atomic_update' + PathDelim;
  log('Tempfolder: ' + tmpFolder);
  If Not ForceDirectories(tmpFolder) Then Begin
    log('Error, could not create: ' + tmpFolder);
    exit;
  End;
  dl := TSynapesDownloader.Create;
  If Not dl.DownloadFile(VersionInfoUrl, tmpFolder + 'fpc_atomic_version.json') Then Begin
{$IFDEF Linux}
    log('try installing ssl support: sudo aptitude install libssl-dev');
{$ENDIF}
    dl.free;
    exit;
  End;
  dl.free;
  If Not Atomic_Version.LoadFromFile(tmpFolder + 'fpc_atomic_version.json') Then exit;
  log('Online version: ' + format('%0.5f', [Atomic_Version.Version]));
  If Version = -1 Then Begin
    log('Local version: not available');
  End
  Else Begin
    log('Local version: ' + format('%0.5f', [Version]));
  End;
  log('Online launcher version: ' + format('%0.2f', [Atomic_Version.LauncherVersion / 100]));
  log('Local launcher version: ' + format('%0.2f', [LauncherVersion / 100]));
  form3.InitWith(Atomic_Version, Version = -1);
  If form3.GetFilesToDLCount() = 0 Then Begin
    UpToDate := true;
    showmessage(
      'Your version of fpc_atomic is up to date.'
      //      + LineEnding + LineEnding +
      //      'But the additional *.zip files are not checked.' + LineEnding + LineEnding +
      //      'If you want, you can check these files now manually and update them.'
      );
  End;
  form2.top := form3.top;
  form2.Left := form3.left + form3.Width + 10;
  form3.ShowModal;
  form2.Hide;
  UpdateStatus();
End;

Procedure TForm1.LoadSideImage;
Var
  p: TPortableNetworkGraphic;
Begin
  If FileExists('data' + PathDelim + 'res' + PathDelim + 'mainmenu.png') Then Begin
    p := TPortableNetworkGraphic.Create;
    p.LoadFromFile('data' + PathDelim + 'res' + PathDelim + 'mainmenu.png');
    p.Width := p.Width Div 2;
    Image1.Picture.Assign(p);
    p.free;
  End;
End;

Procedure TForm1.OnDoLog(Const LogText: String);
Var
  aText: String;
Begin
  aText := copy(LogText, pos('|', LogText) + 1, length(LogText));
  aText := copy(aText, pos('|', aText) + 1, length(aText));
  form2.Memo1.Append(aText);
  If Not form2.Visible Then Begin
    form2.top := form1.top;
    form2.Left := form1.left + form1.Width + 10;
    Form2.Show;
  End;
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

Procedure TForm1.LoadSettings;
Var
  fm: TFormatSettings;
Begin
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  CheckBox1.Checked := ini.ReadBool(FPC_AtomicIniSection, 'ShowFPS', false);
  CheckBox2.Checked := ini.ReadBool(FPC_AtomicIniSection, 'Fullscreen', false);
  CheckBox3.Checked := false;
  CheckBox4.Checked := ini.ReadBool(FPC_AtomicIniSection, 'PlaySounds', true);
  CheckBox5.Checked := Not ini.ReadBool(FPC_AtomicIniSection, 'Proportional', true);
  edit1.text := ini.ReadString(FPC_AtomicIniSection, 'NodeName', 'Player 1');
  ProtocollVersion := ini.ReadInteger(FPC_AtomicIniSection, 'ProtocollVersion', -1);
  Version := strtofloatdef(ini.ReadString(FPC_AtomicIniSection, 'Version', '-1'), -1, fm);

  edit2.text := ini.ReadString('Launcher', 'Router_IP', '127.0.0.1');
  edit3.text := ini.ReadString('Launcher', 'Router_Port', '5521');
End;

Procedure TForm1.UpdateStatus;
Var
  sl: TStringList;
  fm: TFormatSettings;
Begin
  // Links: sind die Spieldaten vollständig, rechts: die installierte Version (ohne Netzwerkzugriff)
  sl := CheckForFiles();
  If sl.Count = 0 Then Begin
    Label5.Caption := '✓ Game data found';
    Label5.Font.Color := clGreen;
  End
  Else Begin
    Label5.Caption := '✗ Game data missing, run CD extractor';
    Label5.Font.Color := clRed;
  End;
  // Die fehlenden Dateien stehen im Tooltip
  Label5.Hint := trim(sl.Text);
  Label5.ShowHint := sl.Count <> 0;
  GameDataFound := sl.Count = 0;
  sl.free;
  Panel1MouseEnter(Nil);
  If Version = -1 Then Begin
    Label6.Caption := 'Game ver. unknown';
  End
  Else Begin
    fm := DefaultFormatSettings;
    fm.DecimalSeparator := '.';
    Label6.Caption := 'Game ver. ' + format('%0.5f', [Version], fm);
  End;
  If UpToDate Then Label6.Caption := Label6.Caption + ', up to date';
End;

Procedure TForm1.StoreSettings;
Begin
  ini.WriteBool(FPC_AtomicIniSection, 'ShowFPS', CheckBox1.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'Fullscreen', CheckBox2.Checked);

  ini.WriteBool(FPC_AtomicIniSection, 'PlaySounds', CheckBox4.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'Proportional', Not CheckBox5.Checked);
  ini.WriteString(FPC_AtomicIniSection, 'NodeName', trim(edit1.text));

  ini.WriteString('Launcher', 'Router_IP', edit2.text);
  ini.WriteString('Launcher', 'Router_Port', edit3.text);
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

