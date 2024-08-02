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

Uses UTF8Process, process, Unit2, Unit3
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

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
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
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
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
Begin
  ClearLog();
  // 2. Download der Version Info
  tmpFolder := IncludeTrailingPathDelimiter(GetTempDir()) + 'atomic_update' + PathDelim;
  log('Tempfolder: ' + tmpFolder);
  If Not ForceDirectories(tmpFolder) Then Begin
    log('Error, could not create: ' + tmpFolder);
    exit;
  End;
  If Not DownloadFile(VersionInfoUrl, tmpFolder + 'fpc_atomic_version.json') Then Begin
{$IFDEF Linux}
    log('try installing ssl support: sudo aptitude install libssl-dev');
{$ENDIF}
    exit;
  End;
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

