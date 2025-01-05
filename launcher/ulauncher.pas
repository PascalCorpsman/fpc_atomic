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
Unit ulauncher;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Const
  VersionInfoUrl = 'https://raw.githubusercontent.com/PascalCorpsman/fpc_atomic/main/bin/fpc_atomic.version'; // URL zum DL der Versionsinfo .JSON
  Logfilename = 'fpc_atomic_launcher.log';
  JSON_ROOT = 'fpc_atomic';
  (*
   * Historie : 0.01 = Initial version
   * -Released- 0.02 = ADD: Improve error message, if launcher needs update and no updater is present.
   *                   ADD: Check if all data from cd_data_extractor is extracted
   * -Released- 0.03 = ADD: Wait for cd_data_extractor
   * -Released- 0.04 = FIX: Übernehmen der Localen Version, wenn config_td binary runter geladen wurde.
   * -Released- 0.05 = FIX: prevent update crash on Linux by renaming folder before update
   * -Released- 0.06 = ADD: Keyboard / Joystick Wizzard
   *                   FIX: Do not suggest files with filesize -1
   *                   ADD: Restart Button for SDL-wizzard
   *                   ADD: umstieg auf SynapseDownloader
   * -Released- 0.07 = FIX: crash on start if SDL2.dll is not present
   *                   ADD: Support for multipla Joysticks with the same name
   *            0.08 = 
   *
   * Known Bugs :
   *)
  LauncherVersion: integer = 8;

Type
  TFileKind = (fkFile, fkZip, fkExecutable, fkLib, fkScript);

  TFile = Record
    Kind: TFileKind;
    URL: String;
    Hash: String;
    Size: int64;
    Size2: int64; // Nur für executables
    InFileOffset: String; // Damit kann noch ein Teil des Ziel Dateinamens platt gemacht werden..
    Hash2: String; // Nur für executables
    Filename: String; // Nur für File / executable
    Description: String; // Nur für Zip
  End;

  TFiles = Array Of TFile;

  { TVersion }

  TVersion = Class
  private
    fFormat: TFormatSettings;
    fLauncherVersion: integer;
    fVersion: Single;
    fVersionText: String;
    fdownload_base: TFiles;
    fdownload: TFiles;
    Procedure Clear;
    Function getDownload(index: integer): TFile;
    Function getDownloadBase(index: integer): TFile;
    Function getDownloadBaseCount: integer;
    Function getDownloadCount: integer;
  public
    Property DownloadBaseCount: integer read getDownloadBaseCount;
    Property DownloadBase[index: integer]: TFile read getDownloadBase;
    Property DownloadCount: integer read getDownloadCount;
    Property Download[index: integer]: TFile read getDownload;

    Property Version: Single read fVersion;
    Property LauncherVersion: integer read fLauncherVersion;
    Property VersionText: String read fVersionText;

    Constructor Create();
    Destructor Destroy; override;
    Function LoadFromFile(Const FIlename: String): Boolean;
  End;

Procedure ClearLog();
Procedure Log(Logtext: String);

Function FileSizeToString(Value: Int64): String;

Function Change_DLL_To_lib_so(Const filename: String): String;

Function CheckForFiles(): TStringList;

Implementation

Uses unit1, unit2, ssl_openssl, httpsend, synautil, uJSON, LazFileUtils, ucdextractor;

Procedure ClearLog();
Begin
  form2.Memo1.Clear;
  If FileExists(Logfilename) Then
    DeleteFile(Logfilename);
End;

Procedure Log(Logtext: String);
Var
  f: TextFile;
Begin
  form2.Memo1.Append(Logtext);
  assignfile(f, Logfilename);
  If FileExists(Logfilename) Then
    append(f)
  Else
    Rewrite(f);
  writeln(f, Logtext);
  CloseFile(f);
  If Not form2.Visible Then Begin
    form2.top := form1.top;
    form2.Left := form1.left + form1.Width + 10;
    Form2.Show;
  End;
End;

Function FileSizeToString(Value: Int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value > 1024 Then Begin
    s := 'K';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'M';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'G';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'T';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'P';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If (r Div 100) <> 0 Then
    result := inttostr(value) + ',' + inttostr(r Div 100) + s + 'B'
  Else
    result := inttostr(value) + s + 'B'
End;

Function Change_DLL_To_lib_so(Const filename: String): String;
Var
  fn, fp: String;
Begin
  fp := ExtractFilePath(filename);
  fn := ExtractFileName(filename);
  fn := ExtractFileNameWithoutExt(fn);
  result := fp + 'lib' + fn + '.so';
End;

Function CheckForFiles(): TStringList;
Var
  s: String;
  i, j: Integer;
  optionals, Files: TStringArray;
  found: Boolean;
Begin
  result := TStringList.Create;
  s := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  Files := GetAllFilesToCheck();
  optionals := GetOptionalFilesToCheck;
  For i := 0 To high(Files) Do Begin
    If Not FileExists(s + files[i]) Then Begin
      found := false;
      For j := 0 To high(optionals) Do Begin
        If optionals[j] = Files[i] Then Begin
          found := true;
          break;
        End;
      End;
      If Not found Then Begin
        result.Add(Files[i]);
      End;
    End;
  End;
End;

{ TVersion }

Procedure TVersion.Clear;
Begin
  fVersion := 0;
  fLauncherVersion := 0;
  fVersionText := '';
  setlength(fdownload_base, 0);
  setlength(fdownload, 0);
End;

Function TVersion.getDownload(index: integer): TFile;
Begin
  result := fdownload[index];
End;

Function TVersion.getDownloadBase(index: integer): TFile;
Begin
  result := fdownload_base[index];
End;

Function TVersion.getDownloadBaseCount: integer;
Begin
  result := length(fdownload_base);
End;

Function TVersion.getDownloadCount: integer;
Begin
  result := length(fdownload);
End;

Constructor TVersion.Create();
Begin
  Inherited create;
  fFormat := DefaultFormatSettings;
  fFormat.DecimalSeparator := '.';
  fdownload_base := Nil;
  fdownload := Nil;
  Clear;
End;

Destructor TVersion.Destroy;
Begin

End;

Function TVersion.LoadFromFile(Const FIlename: String): Boolean;

  Function LoadFile(Const jn: TJSONNode): TFile;
  Var
    k: String;
    jno: TJSONNodeObj;
    jv: TJSONValue;
  Begin
    jno := jn.Obj[0] As TJSONNodeObj;
    k := lowercase(jno.Name);
    Case k Of
      'file': result.Kind := fkFile;
      'zip': result.Kind := fkZip;
    End;
    result.Filename := '';
    result.Hash := '';
    result.Hash2 := '';
    result.Description := '';
    result.InFileOffset := '';
    result.Size := 0;
    result.Size2 := 0;
    jv := jno.FindPath('InFileOffset') As TJSONValue;
    If assigned(jv) Then result.InFileOffset := (jv).Value;
    jv := jno.FindPath('Size') As TJSONValue;
    If assigned(jv) Then result.Size := StrToInt64((jv).Value);
    If result.Kind = fkFile Then Begin
      result.Filename := (jno.FindPath('Filename') As TJSONValue).Value;
      If ExtractFileExt(lowercase(result.Filename)) = '.exe' Then Begin
        result.Kind := fkExecutable;
        result.Hash2 := (jno.FindPath('HASH2') As TJSONValue).Value;
        jv := jno.FindPath('Size2') As TJSONValue;
        If assigned(jv) Then result.Size2 := StrToInt64((jv).Value);
      End;
      If ExtractFileExt(lowercase(result.Filename)) = '.dll' Then Begin
        result.Kind := fkLib;
        result.Hash2 := (jno.FindPath('HASH2') As TJSONValue).Value;
        jv := jno.FindPath('Size2') As TJSONValue;
        If assigned(jv) Then result.Size2 := StrToInt64((jv).Value);
      End;
      If ExtractFileExt(lowercase(result.Filename)) = '.sh' Then Begin
        result.Kind := fkScript;
      End;
      result.Hash := (jno.FindPath('HASH') As TJSONValue).Value;
    End
    Else Begin
      result.Description := (jno.FindPath('Description') As TJSONValue).Value;
    End;
    result.URL := (jno.FindPath('URL') As TJSONValue).Value;
  End;

  Procedure LoadFiles(Const ja: TJSONArray; Var container: TFiles);
  Var
    i: Integer;
  Begin
    setlength(container, ja.ObjCount);
    For i := 0 To ja.ObjCount - 1 Do Begin
      container[i] := LoadFile(ja.Obj[i] As TJSONNode);
    End;
  End;

Var
  p: TJSONParser;
  sl: TStringList;
  jn: TJSONNode;
  jo: TJSONObj;
  ja: TJSONArray;
Begin
  log('Load: ' + FIlename);
  result := false;
  Clear;
  p := TJSONParser.Create;
  sl := TStringList.Create;
  sl.LoadFromFile(Filename);
  Try
    jo := p.Parse(sl.Text);
  Except
    On av: exception Do Begin
      log('Error: ' + av.Message);
      p.free;
      exit;
    End;
  End;
  sl.free;
  If Not assigned(jo) Then Begin
    jo.free;
    Log('Error, no data parsed.');
    p.free;
    exit;
  End;
  jn := jo.FindPath(JSON_ROOT) As TJSONNode;
  If Not assigned(jn) Then Begin
    jo.free;
    Log('Error, no valid data parsed.');
    p.free;
    exit;
  End;
  Try
    fVersion := StrToFloat((jn.FindPath('Version') As TJSONValue).Value, fFormat);
    fLauncherVersion := StrToint((jn.FindPath('LauncherVersion') As TJSONValue).Value);
    fVersionText := (jn.FindPath('VersionText') As TJSONValue).Value;
    ja := jn.FindPath('Download_core') As TJSONArray;
    If assigned(ja) Then Begin
      LoadFiles(ja, fdownload_base);
    End;
    ja := jn.FindPath('Download') As TJSONArray;
    If assigned(ja) Then Begin
      LoadFiles(ja, fdownload);
    End;
  Except
    On av: exception Do Begin
      jo.free;
      Log('Error, no valid data parsed: ' + av.Message);
      p.free;
      exit;
    End;
  End;
  result := true;
  p.free;
End;

End.

