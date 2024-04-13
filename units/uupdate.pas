(******************************************************************************)
(* uupdate.pas                                                     20.12.2018 *)
(*                                                                            *)
(* Version     : 0.06                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : update integration into a FPC/Lazarus Application            *)
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
(*               0.02 - Aufnehmen von Additionals in die Version XML Dateien  *)
(*               0.03 - Besseres Follow_Links                                 *)
(*               0.04 - Fix AV beim beenden nach DoUpdate_Part1               *)
(*               0.05 - Unter Windows warten, bis der Prozess beendet ist     *)
(*               0.06 - Finished dialog und frage nach Restart                *)
(*                                                                            *)
(******************************************************************************)
Unit uupdate;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Dialogs, udomxml;

(*
 *
 * Für die Kommunikation wird Synapse verwendet.
 *
 * Diese Unit stellt eine unabhängige Möglichkeit dar, ein Automatisches Update
 * in eine Anwendung zu integrieren.
 *
 *
 * Die Namen in < > beziehen sich auf Einträge welche in GetVersionFileContent( ..) gesetzt werden.
 *
 * Von der Idee her gibt es ein Onlineverzeichnis in dem liegen 2 Dateien
 *
 * 1.1 die Versionsinformation (zur Prüfung ob ein Update notwendig ist)
 * 1.2 die Anwendung als .zip (beinhaltet im Wurzelverzeichnis die <UpdaterName>)
 *     (<UpdaterName> = im Beispiel updater.exe)
 *
 * Ablauf:
 * 2.1 Die Anwendung prüft mittels (1.1) ob sie aktualisiert werden muss
 * 2.1.1 Wenn ein Update notwendig ist, wird die neue version herunter geladen und die aktuelle
 *     Version mit Hilfe dess UpdaterName zu 100% ersetzt und neu gestartet (Achtung es werden keine Dateien gelöscht).
 *     (UpdaterName wird dabei von DoUpdate_Part1 neu geschrieben)
 *
 * Aufgabe der <UpdaterName> Anwendung:
 *  Überschreiben der eigentlichen Hauptanwendung (mit den Daten aus Workdir) und neustart dieser
 *
 * ---------- Folgende Schritte sind dafür notwendig. ----------
 *
 *Vorbereitend:
 * 3.1 Erstellen einer Versionsinfodatei mittels Aufruf: GetVersionFileContent
 * 3.1.1 Speichern des Inhaltes auf einem Server
 * 3.2 Erstellen der unter (3.1) angegebenen .zip Datei und Speichern auf dem Server
 *
 *Integration in die Anwendung:
 * 4.1. Instanz TUpdater erstellen
 * 4.1.1 Proxy Einstellungen setzten
 * 4.2. Aufruf von TUpdatHelper.GetVersions(..) -- mit link auf Datei von (3.1)
 * 4.3. Wenn die an GetVersions übergebene Callback aufgerufen wurde
 *      Prüfen wie bei Deklaration TUpdatHelper.GetVersions beschrieben
 * 4.4. Aufruf von TUpdatHelper.DoUpdate_Part1
 * 4.4.1 Close
 *
 *Die <UpdaterName> Anwendung:
 * Die <UpdaterName> Anwendung muss folgende Arbeiten erledigen.
 * 1. Kopieren aller Daten aus dem Quell in das Zielverzeichnis
 * 2. Starten der <StarterName> Anwendung nach erfolgreichem kopieren
 * 3. Löschen des Temporären Verzeichnisses
 *
 * <UpdaterName> kann entweder in einer Eigenen Implementierung gemacht werden oder
 * man nimmt die Vorhandene Anwendung aus dem Beispiel.
 *
 * Die Eigenimplementierung muss dann folgendes können:
 * 1. Auswerten der Übergabeparameter "workdir" und "starter"
 * 2. Durchführen der oben beschriebenen Aufgabe.
 * 3. Löschen des Temporären Verzeichnisses
 *
 * !! Achtun !!
 * Für jede Plattform muss eine eigene Anwendung kompiliert und bereit
 * gestellt werde, Getestet wurde bisher unter Linux64 und Windows
 *)

Type

  (*
   * Alle weiteren Additional Einträge
   * <Ident>Value</Ident>
   *)
  TVersionSubRecord = Record
    Ident: String;
    Value: String;
  End;

  TVersion = Record
    Name: String; // Der Name der App (falls im Versionsfile mehrere sind
    Version: String; // Die Version der App welche im Zip file ist.
    DownloadLink: String; // Der Downloadlink unter welchem sich das .zip file befindet (der App die aktualisiert werden soll)
    ReleaseText: String; // Der Text, welcher dem User zur Info über das neue Release angezeigt werden soll
    StarterName: String; // Der Dateiname der Anwendung, welche nach dem Update gestartet werden soll (ohne Dateiendung)
    UpdaterName: String; // Der Dateiname der Updater Anwendung, welche nach dem Entpacken überschrieben wird und dann das eigentliche Update durchführt (ohne Dateiendung)
    Additionals: Array Of TVersionSubRecord; // Eine Liste mit allen anderen Knoten die noch so gefunden wurden

    // Todo: Optional könnte hier eine Liste an Dateien genannt werden, welche for dem Update im Wurzelverzeichnis der Anwendung gelöscht werden sollen..

  End;

  TVersionArray = Array Of TVersion;

  (*
   * Callback die der Updater aufruft, wenn es darum zu klären ob ein Update durchgeführt werden soll
   * Wenn Version = '' -> Fehler und unter Name steht der Fehlertext
   * Sonst gültiger Datensatz.
   *)
  TGetVersionsCallback = Procedure(fAlwaysShowResult: Boolean; OnlineVersions: TVersionArray) Of Object;

  TGotURLCallback = Procedure(URLContent: String) Of Object; // Callback für den Asynchronen Downloader
  TGotFileCallback = Procedure(Filename: String) Of Object; // Callbach für den Asynchronen Downloader

  { TDownloader }

  TDownloader = Class(TThread)
  private
    fUrlContent: String;
    Procedure CallContentCallback;
    Procedure CallCallbackFileContent;
    Function DownloadUrlContent(): String;
    Procedure DownloadFile();
  protected
    Procedure Execute; override;
  public
    ProxyHost: String;
    ProxyPort: String;
    ProxyUser: String;
    ProxyPass: String;
    URL: String; // URL die herunter geladen werden soll
    Filename: String; // Der Dateiname, wo die herunter geladene Datei hin gespeichert werden soll
    CallbackURLContent: TGotURLCallback;
    CallbackFileContent: TGotFileCallback;
  End;

  { TUpdater }

  TUpdater = Class
  private
    fcallback: TGetVersionsCallback;
    fUpdateAllowed: Boolean;
    fAlwaysShowResult: Boolean; // Den Parameter reichen wir nur an die Callback weiter die Klasse selbst macht nichts damit
    fFileDownloaded: Boolean; // Zum Blockieren in DoUpdate_Part_1
    Procedure OnGotURL(URLContent: String);
    Procedure OnFileDownloaded(Filename: String);
    Function ExtractVersionFileContents(Data: String): TVersionArray;
  public
    (*
     * Alles Nachfolgende muss vor den CheckForUpdate initialisiert werden
     *)
    ProxyHost: String;
    ProxyPort: String;
    ProxyUser: String;
    ProxyPass: String;

    LastError: String; // Der Zuletzt aufgetretene Fehler

    fDownloader: TDownloader; // Der Thread der nonblocking den Download durführt

    Constructor Create();
    Destructor Destroy; override;

    (*
     * Lädt die Datei unter URL herunter (asynchron) und ruft die Callback mit dem Ergebnis auf
     * ist das Ergebnis leer, dann steht unter LastError der Fehlercode.
     *
     * Der Wert von AlwaysShowResult wird einfach nur an die Callback durchgereicht
     *)
    Procedure GetVersions(URL: String; AlwaysShowResult: Boolean; Callback: TGetVersionsCallback);

    (*
     * Die Routine startet das Update, sobald sie mit Result=true zurück kommt muss die Hostanwendung beendet werden
     *
     * Die Routine macht folgendes:
     *   1. Runter laden der unter Version angegebenen .zip Datei (synchron)
     *   2. Extrahieren der .zip Datei in das Workdir
     *   3. Überschreiben der UpdaterName im Lokalen Verzeichnis
     *   4. Starten der UpdaterName Anwendung
     *)
    Function DoUpdate_Part1(WorkDir: String; Version: TVersion): Boolean;

    (*
     * Diese Routine wird vom updater aufgerufen, wenn dieser bereits
     * Aktualisiert wurde.
     * Der Updater Kopiert dabei alle Dateien aus Workdir in das Verzeichnis in dem er selbst gerade liegt (= Zielverzeichnis)
     *)
    Function DoUpdate_Part2(SourceDir: String; StarterName: String): Boolean;
  End;

  (*
   * Diese Routine gibt den Dateiinhalt zurück welcher, nachher von CheckForUpdate heruntergeladen und ausgewertet werden soll
   *
   * Theoretisch läst XML mehrere Program Tags zu (und der Parser kann das auch)
   *
   * Will man mehrere Program Tags haben, dann muss die XML Datei gemäß der unten gelisteten
   * implementierung selbst gemacht werden.
   *
   * Hat man nur eine Versionsdatei für eine Anwendung, ist diese Routine der ideal.
   *)
Function GetVersionFileContent(Data: TVersion; Const AdditionalRecords: Array Of TVersionSubRecord): String;

Implementation

Uses ssl_openssl, HTTPSend, zipper, FileUtil, synautil, LCLType,

{$IFDEF Linux}
  Process,
{$ENDIF}
{$IFDEF Windows}
  windows,
  jwatlhelp32,
{$ENDIF}
  forms,
  md5,
  UTF8Process;

Procedure Nop;
Begin

End;

{$IFDEF Windows}

Function processExists(exeFileName: String): Boolean;
Var
  ContinueLoop: Boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
Begin
  Result := false;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  If FSnapshotHandle = INVALID_HANDLE_VALUE Then
    Exit;
  Try
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    While Integer(ContinueLoop) <> 0 Do Begin
      If ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExeFileName)) Or
        (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ExeFileName))) Then Begin
        Result := True;
      End;
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    End;
  Finally
    CloseHandle(FSnapshotHandle);
  End;
End;
{$ENDIF}

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

Function CeckCopyFile(SrcFilename, DestFilename: String): Boolean;
Var
  md5_s, md5_d: String;
Begin
  (*
   * Prüft vor dem Kopieren erst mal ob überhaupt kopiert werden soll
   * Sind Quelle und Ziel Identisch, dann wird nicht kopiert.
   * Das ist unter Windows notwendig, damit die vom Updater verwendete
   * ssl librarie nicht mit sich selbst ersetzt wird, so lange sie noch
   * in Verwendung ist.
   *)
  If FileExists(SrcFilename) And FileExists(DestFilename) Then Begin
    md5_s := MD5Print(MD5File(SrcFilename));
    md5_d := MD5Print(MD5File(DestFilename));
    If md5_s = md5_d Then Begin
      result := true;
    End
    Else Begin
      result := FileUtil.CopyFile(SrcFilename, DestFilename);
    End;
  End
  Else Begin
    result := FileUtil.CopyFile(SrcFilename, DestFilename);
  End;
End;

(*
 * Bastelt alle XML-Sonderzeichen aus dem String.
 *)

Function StringToXMLString(Value: String): String;
Begin
  result := value;
  result := StringReplace(result, '&', '&amp;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := StringReplace(result, '>', '&gt;', [rfReplaceAll]);
  result := StringReplace(result, '"', '&quot;', [rfReplaceAll]);
  result := StringReplace(result, '''', '&apos;', [rfReplaceAll]);
  result := StringReplace(result, LineEnding, '&#13;', [rfReplaceAll]); // -- So sind wir Plattformunabhängig *g*
End;

(*
 * Umkehrfunktion zu StringToXMLString
 *)

Function XMLStringToString(Value: String): String;
Begin
  result := value;
  result := StringReplace(result, '&#13;', LineEnding, [rfReplaceAll]); // -- So sind wir Plattformunabhängig *g*
  result := StringReplace(result, '&apos;', '''', [rfReplaceAll]);
  result := StringReplace(result, '&quot;', '"', [rfReplaceAll]);
  result := StringReplace(result, '&gt;', '>', [rfReplaceAll]);
  result := StringReplace(result, '&lt;', '<', [rfReplaceAll]);
  result := StringReplace(result, '&amp;', '&', [rfReplaceAll]);
End;

Function GetVersionFileContent(Data: TVersion;
  Const AdditionalRecords: Array Of TVersionSubRecord): String;
Var
  i: Integer;
Begin
  result :=
    '<?xml version="1.0"?>' + LineEnding +
    '<programs>' + LineEnding +
    '  <program>' + LineEnding +
    '    <name>' + StringToXMLString(data.Name) + '</name>' + LineEnding +
    '    <version>' + StringToXMLString(data.Version) + '</version>' + LineEnding +
    '    <downloadlink>' + StringToXMLString(data.DownloadLink) + '</downloadlink>' + LineEnding +
    '    <startername>' + StringToXMLString(data.Startername) + '</startername>' + LineEnding +
    '    <updatername>' + StringToXMLString(data.Updatername) + '</updatername>' + LineEnding +
    '    <release_text>' + StringToXMLString(data.ReleaseText) + '</release_text>' + LineEnding;
  For i := 0 To high(AdditionalRecords) Do Begin
    If pos(' ', AdditionalRecords[i].Ident) <> 0 Then Begin // XML Parameter/ Attribute lassen wir nicht zu
      Raise Exception.Create('Space not allowed in ident');
    End;
    If pos('>', AdditionalRecords[i].Ident) <> 0 Then Begin // > Steuerzeichen nicht zulasen
      Raise Exception.Create('> not allowed as ident');
    End;
    If pos('<', AdditionalRecords[i].Ident) <> 0 Then Begin // < Steuerzeichen nicht zulasen
      Raise Exception.Create('< not allowed as ident');
    End;
    result := result + format('    <%s>%s</%s>' + LineEnding, [
      AdditionalRecords[i].Ident,
        StringToXMLString(AdditionalRecords[i].Value),
        AdditionalRecords[i].Ident
        ]);
  End;
  result := result +
    '  </program>' + LineEnding +
    '</programs>'
    ;
End;

{ TDownloader }

Procedure TDownloader.CallContentCallback;
Begin
  If assigned(CallbackURLContent) Then Begin
    CallbackURLContent(fUrlContent);
  End;
End;

Procedure TDownloader.CallCallbackFileContent;
Begin
  If assigned(CallbackFileContent) Then Begin
    CallbackFileContent(Filename);
  End;
End;

Function TDownloader.DownloadUrlContent: String;
Var
  http: THTTPSend;
Begin
  result := '';
  http := THTTPSend.Create;
  http.ProxyHost := ProxyHost;
  http.ProxyPass := ProxyPass;
  http.ProxyPort := ProxyPort;
  http.ProxyUser := ProxyUser;
  If Not Http.HTTPMethod('GET', url) Then Begin
    http.free;
    exit;
  End;
  Follow_Links(http, url);
  If http.Document.Size <> 0 Then Begin
    setlength(result, http.Document.Size);
    http.Document.Seek(0, soFromBeginning);
    http.Document.Read(result[1], http.Document.Size);
  End;
  http.free;
End;

Procedure TDownloader.DownloadFile;
Var
  f: TFileStream;
  http: THTTPSend;
Begin
  http := THTTPSend.Create;
  http.ProxyHost := ProxyHost;
  http.ProxyPass := ProxyPass;
  http.ProxyPort := ProxyPort;
  http.ProxyUser := ProxyUser;
  If Not Http.HTTPMethod('GET', url) Then Begin
    http.free;
    exit;
  End;
  Follow_Links(http, url);
  If http.Document.Size <> 0 Then Begin
    Try
      f := TFileStream.Create(Filename, fmOpenWrite Or fmCreate);
      f.CopyFrom(http.Document, http.Document.Size);
      f.Free;
    Except
      http.free;
      f.Free;
      exit;
    End;
  End;
  http.free;
End;

Procedure TDownloader.Execute;
Begin
  If assigned(CallbackURLContent) Then Begin
    // Download einer URL und Rückgabe als Content
    fUrlContent := DownloadUrlContent();
    If Not Terminated Then Begin
      synchronize(@CallContentCallback);
    End;
  End
  Else Begin
    // Download einer URL und Speichern als Datei
    If assigned(CallbackFileContent) Then Begin
      DownLoadFile;
      If Not Terminated Then Begin
        synchronize(@CallCallbackFileContent);
      End;

    End
    Else Begin
      // ??
    End;
  End;
  Terminate;
End;

{ TUpdater }

Function TUpdater.ExtractVersionFileContents(Data: String): TVersionArray;
// !! Achtung !!
// Der Code hier mus passend zu GetVersionFileContent sein !!

  Function ExtractVersionFile(Const Source: TDomNode; Out Dest: TVersion): Boolean;
    Function StringInList(Ident: String; List: Array Of String): Boolean;
    Var
      i: Integer;
    Begin
      result := false;
      For i := 0 To high(list) Do Begin
        If ident = list[i] Then Begin
          result := true;
          exit;
        End;
      End;
    End;

  Var
    n: TDomNode;
    t: String;
  Begin
    result := false;
    dest.Additionals := Nil;
    n := source.FindNode('name', false);
    If Not assigned(n) Then exit;
    dest.Name := XMLStringToString(n.NodeValue);

    n := source.FindNode('version', false);
    If Not assigned(n) Then exit;
    dest.Version := XMLStringToString(n.NodeValue);

    n := source.FindNode('downloadlink', false);
    If Not assigned(n) Then exit;
    dest.DownloadLink := XMLStringToString(n.NodeValue);

    n := source.FindNode('startername', false);
    If Not assigned(n) Then exit;
    dest.Startername := XMLStringToString(n.NodeValue);

    n := source.FindNode('updatername', false);
    If Not assigned(n) Then exit;
    dest.Updatername := XMLStringToString(n.NodeValue);

    n := source.FindNode('release_text', false);
    If Not assigned(n) Then exit;
    dest.ReleaseText := XMLStringToString(n.NodeValue);

    result := true;

    // Hier Zukunftssicher machen, alles was noch so in dem XML-File drin ist Rauslesen
    n := source.FirstChild;
    While assigned(n) Do Begin
      t := lowercase(n.NodeName);
      If (Not (StringInList(t, ['name', 'version', 'downloadlink', 'startername', 'updatername', 'release_text']))) Then Begin
        setlength(Dest.Additionals, high(Dest.Additionals) + 2);
        Dest.Additionals[high(Dest.Additionals)].Ident := n.NodeName;
        Dest.Additionals[high(Dest.Additionals)].Value := XMLStringToString(n.NodeValue);
      End;
      n := source.NextSibling;
    End;
  End;

Var
  m: TMemoryStream;
  xml: TDOMXML;
  n, root: TDomNode;
  f: TVersion;
Begin
  // !! Achtung !!
  // Der Code hier mus passend zu GetVersionFileContent sein !!
  result := Nil;
  LastError := 'Unknown error';
  Try
    // Wenn gar nichts geladen werden konnte
    If trim(Data) = '' Then Begin
      LastError := 'Error, unable to download version information';
      exit;
    End;
    // Versuch das Empfangene als XML zu laden
    xml := TDOMXML.Create;
    Try
      xml.Clear;
      m := TMemoryStream.Create;
      m.Write(Data[1], length(Data));
      m.Seek(0, soFromBeginning);
      If Not xml.LoadFromStream(m) Then Raise exception.create('');
      m.free;
    Except
      LastError := 'Error, unable to parse version into XML: ' + xml.LastError;
      m.free;
      xml.free;
      exit;
    End;
    If xml.LastError <> '' Then Begin
      LastError := 'Error, unable to parse version into XML: ' + xml.LastError;
      xml.free;
      exit;
    End;
    root := xml.DocumentElement.FindNode('programs', false);
    If Not assigned(root) Then Begin
      LastError := 'Error, unable to locate programs section';
      xml.free;
      exit;
    End;
    n := root.FirstChild;
    LastError := 'Error, unable to locate program in section programs';
    While assigned(n) Do Begin
      If ExtractVersionFile(n, f) Then Begin
        setlength(result, length(result) + 1);
        result[high(result)] := f;
      End;
      n := root.NextSibling;
    End;
    xml.free;
  Except
    On av: Exception Do Begin
      setlength(result, 0);
      LastError := av.Message;
    End;
  End;
  If assigned(result) Then LastError := '';
End;

Procedure TUpdater.OnGotURL(URLContent: String);
Var
  v: TVersionArray;
Begin
  v := ExtractVersionFileContents(URLContent);
  fDownloader := Nil; // Der Thread ist fertig,das merken wir uns nun wieder..
  If assigned(fcallback) Then Begin
    fcallback(fAlwaysShowResult, v);
  End;
End;

Procedure TUpdater.OnFileDownloaded(Filename: String);
Begin
  fFileDownloaded := true;
  fDownloader := Nil; // Der Thread ist fertig,das merken wir uns nun wieder..
End;

Constructor TUpdater.Create;
Begin
  Inherited create;
  LastError := '';
  fUpdateAllowed := false;
  ProxyHost := '';
  ProxyPort := '';
  ProxyUser := '';
  ProxyPass := '';
  fDownloader := Nil;
End;

Destructor TUpdater.Destroy;
Begin
  If assigned(fDownloader) Then Begin
    fDownloader.Terminate;
  End;
End;

Procedure TUpdater.GetVersions(URL: String; AlwaysShowResult: Boolean;
  Callback: TGetVersionsCallback);
Begin
  If (Callback = Nil) Or (assigned(fDownloader)) Then exit; // Ohne Callback brauchen wir gar nicht erst prüfen
  LastError := '';
  fcallback := Callback;
  fAlwaysShowResult := AlwaysShowResult;
  fDownloader := TDownloader.Create(true);
  fDownloader.ProxyHost := ProxyHost;
  fDownloader.ProxyPort := ProxyPort;
  fDownloader.ProxyUser := ProxyUser;
  fDownloader.ProxyPass := ProxyPass;
  fDownloader.URL := URL;
  fDownloader.Filename := '';
  fDownloader.CallbackURLContent := @OnGotURL;
  fDownloader.CallbackFileContent := Nil;
  fDownloader.FreeOnTerminate := true;
  fDownloader.start;
End;

Function TUpdater.DoUpdate_Part1(WorkDir: String; Version: TVersion): Boolean;
Const
{$IFDEF Windows}
  OSExt = '.exe';
{$ELSE}
  OSExt = '';
{$ENDIF}
Var
  ap, d, fn: String;
  uz: TUnZipper;
  pr: TProcessUTF8;
Begin
  // Vorbedingungen abklären
  LastError := '';
  result := false;
  If Not DirectoryExists(WorkDir) Then Begin
    exit;
  End;
  workdir := IncludeTrailingPathDelimiter(WorkDir);
  ap := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  fn := ExtractFileName(version.DownloadLink);
  If (trim(fn) = '') Or (ExtractFileExt(lowercase(fn)) <> '.zip') Then exit;
  If FileExists(WorkDir + fn) Then Begin
    If Not sysutils.DeleteFile(WorkDir + fn) Then Begin
      LastError := 'Could not delete file: ' + WorkDir + fn;
      exit;
    End;
  End;
  d := IncludeTrailingPathDelimiter(copy(fn, 1, pos('.', fn) - 1) + '_download'); // Den Pfad extrahieren *g*
  If DirectoryExists(WorkDir + d) Then Begin
    If Not DeleteDirectory(WorkDir + d, false) Then Begin
      LastError := 'Could not delete directory: ' + WorkDir + d;
      exit;
    End;
  End;
  If FileExists(ap + version.UpdaterName + OSExt) Then Begin
    If Not sysutils.DeleteFile(ap + version.UpdaterName + OSExt) Then Begin
      LastError := 'Could not delete file: ' + ap + version.UpdaterName + OSExt;
      exit;
    End;
  End;

  // 1. Runter laden asynchron, aber Blockierend
  fFileDownloaded := false;
  fDownloader := TDownloader.Create(true);
  fDownloader.ProxyHost := ProxyHost;
  fDownloader.ProxyPort := ProxyPort;
  fDownloader.ProxyUser := ProxyUser;
  fDownloader.ProxyPass := ProxyPass;
  fDownloader.URL := version.DownloadLink;
  fDownloader.Filename := WorkDir + fn;
  fDownloader.CallbackURLContent := Nil;
  fDownloader.CallbackFileContent := @OnFileDownloaded;
  fDownloader.FreeOnTerminate := true;
  fDownloader.start;
  While Not fFileDownloaded Do Begin
    // TODO: Hier könnte man nen Fortschrittsbalken anzeigen ..
    sleep(1);
    Application.ProcessMessages;
  End;
  // Der DL wurde Beendet, aber die Datei nicht geladen -> Fehler
  If Not FileExists(WorkDir + fn) Then Begin
    LastError := 'Could not download: ' + version.DownloadLink;
    exit;
  End;

  // 2. Entpacken
  uz := TUnZipper.Create;
  Try
    uz.FileName := WorkDir + fn;
    uz.OutputPath := WorkDir + d;
    uz.Examine;
    uz.UnZipAllFiles;
  Except
    On av: exception Do Begin
      LastError := av.Message;
      uz.free;
      exit;
    End;
  End;
  uz.free;
  // Nach dem Entpacken das Zip wieder löschen
  If FileExists(WorkDir + fn) Then Begin
    If Not sysutils.DeleteFile(WorkDir + fn) Then Begin
      LastError := 'Could not delete file: ' + WorkDir + fn;
      exit;
    End;
  End;
  // 3. Updater ersetzen
  If Not FileExists(WorkDir + d + version.UpdaterName + OSExt) Then Begin
    LastError := 'Could not find: ' + WorkDir + d + version.UpdaterName + OSExt;
    exit;
  End;
  If Not FileUtil.CopyFile(WorkDir + d + version.UpdaterName + OSExt, ap + version.UpdaterName + OSExt) Then Begin
    LastError := 'Unable to copy: ' + WorkDir + d + version.UpdaterName + OSExt + ' -> ' + ap + version.UpdaterName + OSExt;
    exit;
  End;
  // 4. Updater starten
{$IFDEF Linux}
  // 4.1 Unter Linux muss der Updater noch ausführbar gemacht werden
  pr := TProcessUTF8.Create(Nil);
  pr.Options := [poWaitOnExit];
  pr.CurrentDirectory := ap;
  pr.Executable := 'chmod';
  pr.Parameters.Add('+x');
  pr.Parameters.Add(version.UpdaterName + OSExt);
  pr.Execute;
  pr.free;
{$ENDIF}
  pr := TProcessUTF8.Create(Nil);
  pr.Options := [];
  pr.CurrentDirectory := ap;
  pr.Executable := ap + version.UpdaterName + OSExt;
  pr.Parameters.Add('workdir=' + WorkDir + d);
  pr.Parameters.Add('starter=' + ap + version.StarterName + OSExt);
  pr.Execute; // -- Ab jetzt heist es so schnell wie nur Irgendmöglich Raus aus der Anwendung..
  pr.free;
  result := true;
End;

Function TUpdater.DoUpdate_Part2(SourceDir: String; StarterName: String
  ): Boolean;
{$IFDEF Windows}
Const
  MaxWaitForStarterCloseTime = 10000000; // * 100ms = 1s zum "testen" ob die Zielanwendung Down geht..
{$ENDIF}
Var
  dest, DestDir: String;
  sl: Tstringlist;
  i: Integer;
  p: TProcessUTF8;
  StarterSource: String;
{$IFDEF Windows}
  maxtimeout: Integer;
{$ENDIF}
Begin
  result := false;
  Try
    LastError := '';
    StarterSource := '';
    SourceDir := IncludeTrailingPathDelimiter(SourceDir);
    DestDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
    // 1. Alle Dateien finden
    sl := FindAllFiles(SourceDir, '', true);
    For i := 0 To sl.count - 1 Do Begin
      dest := DestDir + copy(sl[i], length(SourceDir) + 1, length(sl[i]));
      If (dest <> ParamStr(0)) Then Begin
        If (dest = StarterName) Then Begin
          // Unter der Annahme, dass wir von Starter aufgerufen wurden
          // Kopieren wir diesen als aller letztes -> dadurch hatte
          // dieser Maximal viel Zeit sich zu beenden
          StarterSource := sl[i];
        End
        Else Begin
          // 2. Kopieren aller Dateien auser unser Eigenen
          If Not DirectoryExists(ExtractFileDir(dest)) Then Begin
            If Not ForceDirectories(ExtractFileDir(dest)) Then Begin
              LastError := LastError + 'Could not create directory: ' + ExtractFileDir(dest);
            End;
          End;
          If Not CeckCopyFile(sl[i], dest) Then Begin
            LastError := LastError + 'Could not copy : ' + sl[i] + ' -> ' + dest;
            sl.free;
            exit;
          End;
        End;
      End;
      // 3. Löschen der Quelle
      If (dest <> StarterName) Then Begin
        If Not sysutils.DeleteFile(sl[i]) Then Begin
          LastError := LastError + 'Could not delete: ' + sl[i];
        End;
      End;
    End;
    sl.free;
    // Als letztes den Starter Kopieren
    If StarterSource = '' Then Begin // Die Eigentliche Anwendung wurde nicht aktualisiert, da ist was falsch..
      LastError := LastError + 'missing : ' + StarterName;
      exit;
    End;
{$IFDEF Windows}
    // Unter Windows versuchen wir erst zu schauen ob es den Prozess noch gibt und warten ggf. ab
    maxtimeout := 0;
    While processExists(ExtractFileName(StarterName)) And (maxtimeout < MaxWaitForStarterCloseTime) Do Begin
      sleep(100);
      inc(maxtimeout);
      If maxtimeout >= MaxWaitForStarterCloseTime Then Begin
        LastError := LastError + StarterName + ' still running, could not replace it with new version!';
        exit;
      End;
    End;
    maxtimeout := 0;
{$ENDIF}
    If Not CeckCopyFile(StarterSource, StarterName) Then Begin
      LastError := LastError + 'Could not copy : ' + StarterSource + ' -> ' + StarterName;
      exit;
    End;
    If Not DeleteDirectory(SourceDir, false) Then Begin
      LastError := LastError + 'Could not delete: ' + SourceDir;
    End;
    If ID_yes = Application.MessageBox(pchar('Update successfully, do you want to restart ' + ExtractFileName(StarterName) + ' now ?'), 'Info', mb_YesNo Or MB_iconquestion) Then Begin
      // 4. Starten der neuen
      p := TProcessUTF8.Create(Nil);
      p.Executable := StarterName;
      p.Options := [];
      p.Execute;
      p.free;
    End;
  Except
    On av: exception Do Begin
      LastError := LastError + av.Message;
      exit;
    End;
  End;
  result := true;
End;

End.

