(******************************************************************************)
(* usynapsedownloader                                              04.12.2024 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Downloader that utilizes the httpSend class to download a    *)
(*               file and notify a application about the progress.            *)
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
(*                                                                            *)
(******************************************************************************)
Unit usynapsedownloader;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, blcksock, httpsend, synautil;

Type

  TOnFileDownloadUpdateEvent = Procedure(Sender: TObject; aSize, aTotalSize: Int64) Of Object;

  { TSynapesDownloader }

  TSynapesDownloader = Class
  private
    fInstance: THTTPSend;
    aTotalSize, aFileSize: int64;
    Procedure OnHookSocketStatus(Sender: TObject; Reason: THookSocketReason; Const Value: String);
  public
    OnFileDownloadUpdateEvent: TOnFileDownloadUpdateEvent;

    Constructor Create; virtual;
    Destructor Destroy; override;

    Function DownloadFile(URL: String; Filename: String): Boolean;
  End;

Implementation

Uses
{$IFDEF DARWIN}
  ssl_openssl3;
{$ELSE}
  ssl_openssl;
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

{ TSynapesDownloader }

Procedure TSynapesDownloader.OnHookSocketStatus(Sender: TObject;
  Reason: THookSocketReason; Const Value: String);
Var
  i: Integer;
  s: String;
  sa: TStringArray;
Begin
  If Not assigned(fInstance) Then exit;
  If aTotalSize = 0 Then Begin
    // Read Filesize from Header
    For i := 0 To fInstance.Headers.Count - 1 Do Begin
      s := fInstance.Headers[i];
      If pos('Content-Length:', s) <> 0 Then Begin
        sa := s.Split(':');
        If length(sa) = 2 Then Begin
          aTotalSize := StrToInt64Def(trim(sa[1]), 0);
        End;
      End;
    End;
  End
  Else Begin
    If Reason = HR_ReadCount Then Begin
      aFileSize := aFileSize + StrToInt64Def(value, 0);
      If assigned(OnFileDownloadUpdateEvent) Then Begin
        OnFileDownloadUpdateEvent(self, aFileSize, aTotalSize);
      End;
    End;
  End;
End;

Constructor TSynapesDownloader.Create;
Begin
  Inherited Create;
  OnFileDownloadUpdateEvent := Nil;
End;

Destructor TSynapesDownloader.Destroy;
Begin

End;

Function TSynapesDownloader.DownloadFile(URL: String; Filename: String
  ): Boolean;
Var
  f: TFileStream;
  s, dir: String;
Begin
  result := false;
  If assigned(fInstance) Then fInstance.free;
  fInstance := Nil;
  If FileExists(Filename) Then Begin
    If Not DeleteFile(Filename) Then Begin
      Raise exception.create('Error, unable to delete old file: ' + Filename);
      exit;
    End;
  End;
  aTotalSize := 0;
  aFileSize := 0;

  fInstance := THTTPSend.Create;

  // TODO: Proxy support ?
  //fInstance.ProxyHost := ProxyHost;
  //fInstance.ProxyPass := ProxyPass;
  //fInstance.ProxyPort := ProxyPort;
  //fInstance.ProxyUser := ProxyUser;

  fInstance.Sock.OnStatus := @OnHookSocketStatus;
  If Not fInstance.HTTPMethod('GET', url) Then Begin
    s :=
      '\-fInstance.ResultCode: ' + inttostr(fInstance.ResultCode) + ' ; ' + fInstance.ResultString + LineEnding +
      '\-fInstance.Sock.LastError: ' + inttostr(fInstance.Sock.LastError) + ' ; ' + fInstance.Sock.LastErrorDesc + LineEnding +
      '\-fInstance.Sock.SSL.LastError: ' + inttostr(fInstance.Sock.SSL.LastError) + ' ; ' + fInstance.Sock.SSL.LastErrorDesc;
    fInstance.free;
    fInstance := Nil;
    Raise Exception.Create(s);
    exit;
  End;
  Follow_Links(fInstance, url);
  If fInstance.Document.Size <> 0 Then Begin
    // Fertig melden, hat ja wohl geklappt ;)
    If assigned(OnFileDownloadUpdateEvent) Then Begin
      OnFileDownloadUpdateEvent(self, aTotalSize, aTotalSize);
    End;
    dir := ExtractFileDir(Filename);
    If dir <> '' Then Begin
      If Not ForceDirectories(dir) Then Begin
        s := 'Error, could not create: ' + ExtractFileDir(Filename);
        fInstance.free;
        fInstance := Nil;
        Raise Exception.Create(s);
        exit;
      End;
    End;
    Try
      f := TFileStream.Create(Filename, fmOpenWrite Or fmCreate);
      f.CopyFrom(fInstance.Document, fInstance.Document.Size);
      f.Free;
      result := true;
    Except
      On av: Exception Do Begin
        fInstance.free;
        fInstance := Nil;
        f.Free;
        Raise;
        exit;
      End;
    End;
  End;
  fInstance.free;
  fInstance := Nil;
End;

End.

