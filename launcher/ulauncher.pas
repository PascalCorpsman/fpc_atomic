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
   *            0.02 = ADD: Improve error message, if launcher needs update and no updater is present.
   *                   ADD: Check if all data from cd_data_extractor is extracted
   * Known Bugs :
   *)
  LauncherVersion: integer = 2;

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

Function DownloadFile(URL, Filename: String): boolean;

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

(*
 * Verfolgt 302 und 301 Weiterleitungen
 *)

Procedure Follow_Links(Connection: THTTPSend; BaseURL: String);
  Function ExtractBaseURL(U: String): String;
  Var
    Prot, User, Pass, Host, Port, Path, Para: String;
  Begin
    Prot := '';
    User := '';
    Pass := '';
    Host := '';
    Port := '';
    Path := '';
    Para := '';
    ParseURL(u, Prot, User, Pass, Host, Port, Path, Para);
    result := Prot + '://' + host + '/';
  End;
Var
  t: String;
  timeout, i: Integer;
Begin
  If BaseURL = '' Then exit;
  BaseURL := ExtractBaseURL(BaseURL);
  timeout := 20;
  While ((Connection.ResultCode = 303) Or (Connection.ResultCode = 302) Or (Connection.ResultCode = 301)) And (timeout >= 0) Do Begin
    dec(timeout);
    t := '';
    For i := 0 To Connection.Headers.Count - 1 Do Begin
      If pos('location', lowercase(Connection.Headers[i])) <> 0 Then Begin
        t := Connection.Headers[i];
        t := copy(t, pos(':', t) + 1, length(t));
        t := trim(t);
        If pos('http', lowercase(t)) = 0 Then Begin
          If t[1] = '/' Then delete(t, 1, 1);
          t := BaseURL + t;
        End;
        Connection.Headers.Clear;
        Connection.Document.Clear;
        BaseURL := ExtractBaseURL(t);
        Connection.HTTPMethod('GET', t);
        break;
      End;
    End;
    If t = '' Then Begin
      // das Location feld konnte im Header nicht gefunden werden.
      exit;
    End;
  End;
End;

Function DownloadFile(URL, Filename: String): boolean;
Var
  f: TFileStream;
  http: THTTPSend;
  dir: String;
Begin
  result := false;
  If FileExists(Filename) Then Begin
    If Not DeleteFile(Filename) Then Begin
      log('Error, unable to delete old file: ' + Filename);
      exit;
    End;
  End;
  http := THTTPSend.Create;
  // Log
  log('Download: ' + URL);
  // TODO: Proxy support ?
  //http.ProxyHost := ProxyHost;
  //http.ProxyPass := ProxyPass;
  //http.ProxyPort := ProxyPort;
  //http.ProxyUser := ProxyUser;
  If Not Http.HTTPMethod('GET', url) Then Begin
    Log(
      '\-HTTP.ResultCode: ' + inttostr(Http.ResultCode) + ' ; ' + Http.ResultString + LineEnding +
      '\-HTTP.Sock.LastError: ' + inttostr(Http.Sock.LastError) + ' ; ' + Http.Sock.LastErrorDesc + LineEnding +
      '\-HTTP.Sock.SSL.LastError: ' + inttostr(Http.Sock.SSL.LastError) + ' ; ' + Http.Sock.SSL.LastErrorDesc);
    http.free;
    exit;
  End;
  Follow_Links(http, url);
  If http.Document.Size <> 0 Then Begin
    dir := ExtractFileDir(Filename);
    If dir <> '' Then Begin
      If Not ForceDirectories(dir) Then Begin
        log('Error, could not create: ' + ExtractFileDir(Filename));
        http.free;
        exit;
      End;
    End;
    Try
      f := TFileStream.Create(Filename, fmOpenWrite Or fmCreate);
      f.CopyFrom(http.Document, http.Document.Size);
      f.Free;
      result := true;
    Except
      On av: Exception Do Begin
        Log(av.Message);
        http.free;
        f.Free;
        exit;
      End;
    End;
  End
  Else Begin
    Log('\-No document received.');
    exit;
  End;
  http.free;
  // form1.Log('\-succeed.');
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

