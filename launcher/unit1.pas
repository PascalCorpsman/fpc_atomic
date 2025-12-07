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
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    ini: TIniFile;
    Atomic_Version: TVersion;
    ProtocollVersion: integer;
    Version: Single;

    Procedure LoadSettings();
    Procedure StoreSettings();

    Procedure LoadSideImage();
  public
    Procedure StoreVersion(NewVersion: single);

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses UTF8Process, process, Unit2, Unit3, LCLType, LazFileUtils, LazUTF8
  , ukeyboarddialog, uatomic_common, usynapsedownloader
{$IFDEF Windows}
  , LResources
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

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  ini := TIniFile.Create('fpc_atomic.ini');
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  Atomic_Version := TVersion.Create();
  LoadSideImage();
  LoadSettings();
  caption := format('FPC Atomic, launcher ver. %0.2f', [LauncherVersion / 100]);
  // Handle Launcher Parameters
  For i := 1 To ParamCount Do Begin
    If ParamStr(i) = '-cti' Then CheckBox3.Checked := true;
    If ParamStr(i) = '-ip' Then edit2.text := ParamStr(i + 1);
    If ParamStr(i) = '-port' Then edit3.text := ParamStr(i + 1);
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  P: TProcessUTF8;
  extractorPath: String;
Begin
  // run cd data extractor
  StoreSettings();
  ini.UpdateFile;
  // Find cd_data_extractor in the same directory as the launcher (for macOS app bundle)
  // On macOS, this will be in Contents/MacOS/ of the app bundle
  extractorPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'cd_data_extractor'{$IFDEF Windows} + '.exe'{$ENDIF};
  // Short Prechecks
  If Not FileExists(extractorPath) Then Begin
    showmessage('Error, installation not complete, please run "Check for updates"' + LineEnding +
                'Missing: ' + extractorPath);
    exit;
  End;
  // Run the App ;)
  // On macOS, GUI applications need to be run detached to show GUI properly
  p := TProcessUTF8.Create(Nil);
  p.Executable := extractorPath;
{$IFDEF Darwin}
  // On macOS, run GUI application detached so it can show its window
  p.Options := p.Options + [poDetached];
{$ELSE}
  // On other platforms, wait for exit
  p.Options := p.Options + [poWaitOnExit];
{$ENDIF}
  p.Execute;
  p.free;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  Dialog: TKeyboardDialog;
  Keys: Array[TKeySet] Of TKeys;
Begin
  // Key Assignment
  dialog := TKeyboardDialog.CreateNew(self, 0);
  Keys[ks0] := AtomicDefaultKeys(ks0);
  Keys[ks0].KeyUp := ini.ReadInteger('TApplication.Form1', 'KeyUp', Keys[ks0].KeyUp);
  Keys[ks0].KeyDown := ini.ReadInteger('TApplication.Form1', 'KeyDown', Keys[ks0].KeyDown);
  Keys[ks0].KeyLeft := ini.ReadInteger('TApplication.Form1', 'KeyLeft', Keys[ks0].KeyLeft);
  Keys[ks0].KeyRight := ini.ReadInteger('TApplication.Form1', 'KeyRight', Keys[ks0].KeyRight);
  Keys[ks0].KeyPrimary := ini.ReadInteger('TApplication.Form1', 'KeyPrimary', Keys[ks0].KeyPrimary);
  Keys[ks0].KeySecondary := ini.ReadInteger('TApplication.Form1', 'KeySecondary', Keys[ks0].KeySecondary);
  Keys[ks0].UseSDL2 := ini.ReadBool('TApplication.Form1', 'UseSDL', Keys[ks0].UseSDL2);
  If Keys[ks0].UseSDL2 Then Begin
    Keys[ks0].Name := ini.ReadString('TApplication.Form1', 'SDL_Name', Keys[ks0].Name);
    Keys[ks0].NameIndex := ini.readInteger('TApplication.Form1', 'SDL_NameIndex', Keys[ks0].NameIndex);
    Keys[ks0].ButtonIndex[0] := ini.readInteger('TApplication.Form1', 'SDL_First', Keys[ks0].ButtonIndex[0]);
    Keys[ks0].ButtonsIdle[0] := ini.readBool('TApplication.Form1', 'SDL_FirstIdle', Keys[ks0].ButtonsIdle[0]);
    Keys[ks0].ButtonIndex[1] := ini.readInteger('TApplication.Form1', 'SDL_Second', Keys[ks0].ButtonIndex[1]);
    Keys[ks0].ButtonsIdle[1] := ini.readBool('TApplication.Form1', 'SDL_SecondIdle', Keys[ks0].ButtonsIdle[1]);
    Keys[ks0].AchsisIndex[0] := ini.readInteger('TApplication.Form1', 'SDL_UpDown', Keys[ks0].AchsisIndex[0]);
    Keys[ks0].AchsisIdle[0] := ini.readInteger('TApplication.Form1', 'SDL_UpDownIdle', Keys[ks0].AchsisIdle[0]);
    Keys[ks0].AchsisDirection[0] := ini.readInteger('TApplication.Form1', 'SDL_UpDownDirection', Keys[ks0].AchsisDirection[0]);
    Keys[ks0].AchsisIndex[1] := ini.readInteger('TApplication.Form1', 'SDL_LeftRight', Keys[ks0].AchsisIndex[1]);
    Keys[ks0].AchsisIdle[1] := ini.readInteger('TApplication.Form1', 'SDL_LeftRightIdle', Keys[ks0].AchsisIdle[1]);
    Keys[ks0].AchsisDirection[1] := ini.readInteger('TApplication.Form1', 'SDL_LeftRightDirection', Keys[ks0].AchsisDirection[1]);
  End;
  Keys[ks1] := AtomicDefaultKeys(ks1);
  Keys[ks1].KeyUp := ini.ReadInteger('TApplication.Form1', 'KeyUp2', Keys[ks1].KeyUp);
  Keys[ks1].KeyDown := ini.ReadInteger('TApplication.Form1', 'KeyDown2', Keys[ks1].KeyDown);
  Keys[ks1].KeyLeft := ini.ReadInteger('TApplication.Form1', 'KeyLeft2', Keys[ks1].KeyLeft);
  Keys[ks1].KeyRight := ini.ReadInteger('TApplication.Form1', 'KeyRight2', Keys[ks1].KeyRight);
  Keys[ks1].KeyPrimary := ini.ReadInteger('TApplication.Form1', 'KeyPrimary2', Keys[ks1].KeyPrimary);
  Keys[ks1].KeySecondary := ini.ReadInteger('TApplication.Form1', 'KeySecondary2', Keys[ks1].KeySecondary);
  Keys[ks1].UseSDL2 := ini.ReadBool('TApplication.Form1', 'UseSDL2', Keys[ks1].UseSDL2);
  If Keys[ks1].UseSDL2 Then Begin
    Keys[ks1].Name := ini.ReadString('TApplication.Form1', 'SDL_Name2', Keys[ks1].Name);
    Keys[ks1].NameIndex := ini.readInteger('TApplication.Form1', 'SDL_NameIndex2', Keys[ks1].NameIndex);
    Keys[ks1].ButtonIndex[0] := ini.readInteger('TApplication.Form1', 'SDL_First2', Keys[ks1].ButtonIndex[0]);
    Keys[ks1].ButtonsIdle[0] := ini.readBool('TApplication.Form1', 'SDL_FirstIdle2', Keys[ks1].ButtonsIdle[0]);
    Keys[ks1].ButtonIndex[1] := ini.readInteger('TApplication.Form1', 'SDL_Second2', Keys[ks1].ButtonIndex[1]);
    Keys[ks1].ButtonsIdle[1] := ini.readBool('TApplication.Form1', 'SDL_SecondIdle2', Keys[ks1].ButtonsIdle[1]);
    Keys[ks1].AchsisIndex[0] := ini.readInteger('TApplication.Form1', 'SDL_UpDown2', Keys[ks1].AchsisIndex[0]);
    Keys[ks1].AchsisIdle[0] := ini.readInteger('TApplication.Form1', 'SDL_UpDownIdle2', Keys[ks1].AchsisIdle[0]);
    Keys[ks1].AchsisDirection[0] := ini.readInteger('TApplication.Form1', 'SDL_UpDownDirection2', Keys[ks1].AchsisDirection[0]);
    Keys[ks1].AchsisIndex[1] := ini.readInteger('TApplication.Form1', 'SDL_LeftRight2', Keys[ks1].AchsisIndex[1]);
    Keys[ks1].AchsisIdle[1] := ini.readInteger('TApplication.Form1', 'SDL_LeftRightIdle2', Keys[ks1].AchsisIdle[1]);
    Keys[ks1].AchsisDirection[1] := ini.readInteger('TApplication.Form1', 'SDL_LeftRightDirection2', Keys[ks1].AchsisDirection[1]);
  End;
  Dialog.LoadKeys(Keys[ks0], Keys[ks1]);
  If Dialog.Execute() Then Begin
    Keys[ks0] := Dialog.GetKeys(ks0);
    Keys[ks1] := Dialog.GetKeys(ks1);
    ini.WriteInteger('TApplication.Form1', 'KeyUp', Keys[ks0].KeyUp);
    ini.WriteInteger('TApplication.Form1', 'KeyDown', Keys[ks0].KeyDown);
    ini.WriteInteger('TApplication.Form1', 'KeyLeft', Keys[ks0].KeyLeft);
    ini.WriteInteger('TApplication.Form1', 'KeyRight', Keys[ks0].KeyRight);
    ini.WriteInteger('TApplication.Form1', 'KeyPrimary', Keys[ks0].KeyPrimary);
    ini.WriteInteger('TApplication.Form1', 'KeySecondary', Keys[ks0].KeySecondary);
    ini.WriteBool('TApplication.Form1', 'UseSDL', Keys[ks0].UseSDL2);
    ini.WriteString('TApplication.Form1', 'SDL_Name', Keys[ks0].Name);
    ini.WriteInteger('TApplication.Form1', 'SDL_NameIndex', Keys[ks0].NameIndex);
    ini.WriteInteger('TApplication.Form1', 'SDL_First', Keys[ks0].ButtonIndex[0]);
    ini.WriteBool('TApplication.Form1', 'SDL_FirstIdle', Keys[ks0].ButtonsIdle[0]);
    ini.WriteInteger('TApplication.Form1', 'SDL_Second', Keys[ks0].ButtonIndex[1]);
    ini.WriteBool('TApplication.Form1', 'SDL_SecondIdle', Keys[ks0].ButtonsIdle[1]);
    ini.WriteInteger('TApplication.Form1', 'SDL_UpDown', Keys[ks0].AchsisIndex[0]);
    ini.WriteInteger('TApplication.Form1', 'SDL_UpDownIdle', Keys[ks0].AchsisIdle[0]);
    ini.WriteInteger('TApplication.Form1', 'SDL_UpDownDirection', Keys[ks0].AchsisDirection[0]);
    ini.WriteInteger('TApplication.Form1', 'SDL_LeftRight', Keys[ks0].AchsisIndex[1]);
    ini.WriteInteger('TApplication.Form1', 'SDL_LeftRightIdle', Keys[ks0].AchsisIdle[1]);
    ini.WriteInteger('TApplication.Form1', 'SDL_LeftRightDirection', Keys[ks0].AchsisDirection[1]);
    ini.WriteInteger('TApplication.Form1', 'KeyUp2', Keys[ks1].KeyUp);
    ini.WriteInteger('TApplication.Form1', 'KeyDown2', Keys[ks1].KeyDown);
    ini.WriteInteger('TApplication.Form1', 'KeyLeft2', Keys[ks1].KeyLeft);
    ini.WriteInteger('TApplication.Form1', 'KeyRight2', Keys[ks1].KeyRight);
    ini.WriteInteger('TApplication.Form1', 'KeyPrimary2', Keys[ks1].KeyPrimary);
    ini.WriteInteger('TApplication.Form1', 'KeySecondary2', Keys[ks1].KeySecondary);
    ini.WriteBool('TApplication.Form1', 'UseSDL2', Keys[ks1].UseSDL2);
    ini.WriteString('TApplication.Form1', 'SDL_Name2', Keys[ks1].Name);
    ini.WriteInteger('TApplication.Form1', 'SDL_NameIndex2', Keys[ks1].NameIndex);
    ini.WriteInteger('TApplication.Form1', 'SDL_First2', Keys[ks1].ButtonIndex[0]);
    ini.WriteBool('TApplication.Form1', 'SDL_FirstIdle2', Keys[ks1].ButtonsIdle[0]);
    ini.WriteInteger('TApplication.Form1', 'SDL_Second2', Keys[ks1].ButtonIndex[1]);
    ini.WriteBool('TApplication.Form1', 'SDL_SecondIdle2', Keys[ks1].ButtonsIdle[1]);
    ini.WriteInteger('TApplication.Form1', 'SDL_UpDown2', Keys[ks1].AchsisIndex[0]);
    ini.WriteInteger('TApplication.Form1', 'SDL_UpDownIdle2', Keys[ks1].AchsisIdle[0]);
    ini.WriteInteger('TApplication.Form1', 'SDL_UpDownDirection2', Keys[ks1].AchsisDirection[0]);
    ini.WriteInteger('TApplication.Form1', 'SDL_LeftRight2', Keys[ks1].AchsisIndex[1]);
    ini.WriteInteger('TApplication.Form1', 'SDL_LeftRightIdle2', Keys[ks1].AchsisIdle[1]);
    ini.WriteInteger('TApplication.Form1', 'SDL_LeftRightDirection2', Keys[ks1].AchsisDirection[1]);
  End;
  dialog.free;
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
  exePath, serverPath, workDir: String;
Begin
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
  workDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  exePath := workDir + 'fpc_atomic';
  serverPath := workDir + 'atomic_server';
{$IFDEF Windows}
  exePath := exePath + '.exe';
  serverPath := serverPath + '.exe';
{$ENDIF}
  If (Not FileExists(exePath)) Or (Not FileExists(serverPath)) Then Begin
    showmessage('Error, installation not complete, please run "Check for updates"');
    exit;
  End;
  // Run the App ;)
  p := TProcessUTF8.Create(Nil);
  p.Executable := exePath;
  p.CurrentDirectory := ExcludeTrailingPathDelimiter(workDir);
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
End;

Function ResolveResourceBase(BasePath: String): String;
Var
  testPath: String;
  appBundlePath: String;
Begin
  // Normalize base path to absolute path
  BasePath := ExpandFileName(IncludeTrailingPathDelimiter(BasePath));
  
  // Try multiple locations for data directory
  // 1. Direct relative to executable (works with symlinks)
  testPath := BasePath + 'data';
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    exit;
  End;
  // 2. Relative path from MacOS directory in .app bundle
  testPath := ExpandFileName(BasePath + '../Resources/data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    exit;
  End;
  // 3. Try symlink path (../../data from MacOS) - for shared data directory
  testPath := ExpandFileName(BasePath + '../../data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    exit;
  End;
  // 4. Try symlink path (../../../data from MacOS) - alternative symlink location
  testPath := ExpandFileName(BasePath + '../../../data');
  If DirectoryExistsUTF8(testPath) Then Begin
    Result := IncludeTrailingPathDelimiter(testPath);
    exit;
  End;
  // 5. Try path next to .app bundle (for cases where data is outside the bundle)
  // If BasePath contains ".app/Contents/MacOS/", try going up to the .app bundle's parent directory
  If Pos('.app/Contents/MacOS/', BasePath) > 0 Then Begin
    appBundlePath := Copy(BasePath, 1, Pos('.app/Contents/MacOS/', BasePath) + 4); // Get path up to ".app"
    appBundlePath := ExtractFilePath(ExcludeTrailingPathDelimiter(appBundlePath)); // Get parent directory of .app
    testPath := ExpandFileName(appBundlePath + 'data');
    If DirectoryExistsUTF8(testPath) Then Begin
      Result := IncludeTrailingPathDelimiter(testPath);
      exit;
    End;
  End;
  // Fallback: use base path (may not exist, but at least we tried)
  Result := BasePath + 'data' + PathDelim;
End;

Procedure TForm1.LoadSideImage;
Var
  p: TPortableNetworkGraphic;
  dataPath: String;
  imagePath: String;
Begin
  // Resolve data directory path
  dataPath := ResolveResourceBase(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));
  imagePath := dataPath + 'res' + PathDelim + 'mainmenu.png';
  If FileExistsUTF8(imagePath) Then Begin
    p := TPortableNetworkGraphic.Create;
    p.LoadFromFile(imagePath);
    p.Width := p.Width Div 2;
    Image1.Picture.Assign(p);
    p.free;
  End;
End;

Procedure TForm1.StoreVersion(NewVersion: single);
Var
  fm: TFormatSettings;
Begin
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  ini.WriteString('TApplication.Form1', 'Version', format('%0.5f', [NewVersion], fm));
  ini.UpdateFile;
  Version := NewVersion;
End;

Procedure TForm1.LoadSettings;
Var
  fm: TFormatSettings;
Begin
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  CheckBox1.Checked := ini.ReadBool('TApplication.Form1', 'ShowFPS', false);
  CheckBox2.Checked := ini.ReadBool('TApplication.Form1', 'Fullscreen', false);
  CheckBox3.Checked := false;
  CheckBox4.Checked := ini.ReadBool('TApplication.Form1', 'PlaySounds', true);
  edit1.text := ini.ReadString('TApplication.Form1', 'NodeName', 'Player 1');
  ProtocollVersion := ini.ReadInteger('TApplication.Form1', 'ProtocollVersion', -1);
  Version := strtofloatdef(ini.ReadString('TApplication.Form1', 'Version', '-1'), -1, fm);

  edit2.text := ini.ReadString('Launcher', 'Router_IP', '127.0.0.1');
  edit3.text := ini.ReadString('Launcher', 'Router_Port', '5521');
End;

Procedure TForm1.StoreSettings;
Begin
  ini.WriteBool('TApplication.Form1', 'ShowFPS', CheckBox1.Checked);
  ini.WriteBool('TApplication.Form1', 'Fullscreen', CheckBox2.Checked);

  ini.WriteBool('TApplication.Form1', 'PlaySounds', CheckBox4.Checked);
  ini.WriteString('TApplication.Form1', 'NodeName', edit1.text);

  ini.WriteString('Launcher', 'Router_IP', edit2.text);
  ini.WriteString('Launcher', 'Router_Port', edit3.text);
End;

{$IFDEF Windows}

Initialization
  // TODO: Hier fehlt noch ein Hinweis wie man die .lrs Datei erzeugt ;)
{$I atomic_launcher.lrs}

  // SSL initialization removed - using fphttpclient instead of Synapse
  // fphttpclient handles SSL automatically via system libraries
  // Old Synapse SSL code:
  // If Not CheckAndMaybeExtract('ssleay32') Then exit;
  // If Not CheckAndMaybeExtract('libeay32') Then exit;
  // If SSLImplementation = TSSLNone Then Begin
  //   If InitSSLInterface Then
  //     SSLImplementation := TSSLOpenSSL;
  // End;
{$ENDIF}

End.

