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
(*              0.02 - Replaced synapse with fphttpclient (2024-12-05)        *)
(*                                                                            *)
(******************************************************************************)
Unit usynapsedownloader;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, fphttpclient, URIParser;

Type

  TOnFileDownloadUpdateEvent = Procedure(Sender: TObject; aSize, aTotalSize: Int64) Of Object;

  { TSynapesDownloader }

  TSynapesDownloader = Class
  private
    fClient: TFPHTTPClient;
    aTotalSize, aFileSize: int64;
    Procedure OnDataReceived(Sender: TObject; Const ContentLength, CurrentPos: Int64);
  public
    OnFileDownloadUpdateEvent: TOnFileDownloadUpdateEvent;

    Constructor Create; virtual;
    Destructor Destroy; override;

    Function DownloadFile(URL: String; Filename: String): Boolean;
  End;

Implementation

Uses
  LazFileUtils;

{ TSynapesDownloader }

Procedure TSynapesDownloader.OnDataReceived(Sender: TObject; Const ContentLength, CurrentPos: Int64);
Begin
  If ContentLength > 0 Then Begin
    aTotalSize := ContentLength;
      End;
  aFileSize := CurrentPos;
      If assigned(OnFileDownloadUpdateEvent) Then Begin
        OnFileDownloadUpdateEvent(self, aFileSize, aTotalSize);
  End;
End;

Constructor TSynapesDownloader.Create;
Begin
  Inherited Create;
  OnFileDownloadUpdateEvent := Nil;
  fClient := Nil;
  aTotalSize := 0;
  aFileSize := 0;
End;

Destructor TSynapesDownloader.Destroy;
Begin
  If assigned(fClient) Then Begin
    fClient.Free;
    fClient := Nil;
  End;
  Inherited Destroy;
End;

Function TSynapesDownloader.DownloadFile(URL: String; Filename: String): Boolean;
Var
  f: TFileStream;
  dir: String;
  RedirectCount: Integer;
  FinalURL: String;
Begin
  result := false;
  aTotalSize := 0;
  aFileSize := 0;
  
  If FileExists(Filename) Then Begin
    If Not DeleteFile(Filename) Then Begin
      Raise exception.create('Error, unable to delete old file: ' + Filename);
      exit;
    End;
  End;

    dir := ExtractFileDir(Filename);
    If dir <> '' Then Begin
      If Not ForceDirectories(dir) Then Begin
      Raise Exception.Create('Error, could not create: ' + dir);
        exit;
      End;
    End;

  fClient := TFPHTTPClient.Create(Nil);
  Try
    fClient.OnDataReceived := @OnDataReceived;
    fClient.AllowRedirect := true;
    fClient.MaxRedirects := 20; // Follow up to 20 redirects
    
    // Handle redirects manually if needed
    FinalURL := URL;
    RedirectCount := 0;
    
    Try
      f := TFileStream.Create(Filename, fmCreate);
      Try
        fClient.Get(FinalURL, f);
      result := true;
        // Final progress update
        If assigned(OnFileDownloadUpdateEvent) Then Begin
          OnFileDownloadUpdateEvent(self, aFileSize, aTotalSize);
        End;
      Finally
        f.Free;
      End;
    Except
      On E: Exception Do Begin
        If assigned(f) Then f.Free;
        Raise Exception.Create('Error downloading file: ' + E.Message);
      End;
    End;
  Finally
    fClient.Free;
    fClient := Nil;
  End;
End;

End.
