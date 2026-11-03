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
Unit uupdatecheck;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  (*
   * Ok = true -> Filename enthält die Versionsinfo (noch nicht geparst)
   *)
  TUpdateCheckDone = Procedure(Ok: Boolean; Const Filename, ErrorText: String) Of Object;

  { TUpdateCheckThread }

  (*
   * Lädt die Versionsinfo (VersionInfoUrl), ohne die GUI zu blockieren,
   * OnDone wird im Hauptthread aufgerufen, der Thread gibt sich selbst frei.
   *)
  TUpdateCheckThread = Class(TThread)
  private
    fFilename: String;
    fError: String;
    fOk: Boolean;
    fOnDone: TUpdateCheckDone;
    Procedure DoDone;
  protected
    Procedure Execute; override;
  public
    Constructor Create(Const Filename: String; OnDone: TUpdateCheckDone);
  End;

(*
 * Der Ordner für die Downloads des Launchers (Versionsinfo, Zips, der neue Launcher)
 *)
Function GetUpdateTempFolder(): String;

Implementation

Uses usynapsedownloader, ulauncher;

Function GetUpdateTempFolder: String;
Begin
  result := IncludeTrailingPathDelimiter(GetTempDir()) + 'atomic_update' + PathDelim;
End;

{ TUpdateCheckThread }

Constructor TUpdateCheckThread.Create(Const Filename: String;
  OnDone: TUpdateCheckDone);
Begin
  fFilename := Filename;
  fOnDone := OnDone;
  fOk := false;
  fError := '';
  FreeOnTerminate := true;
  Inherited Create(false);
End;

Procedure TUpdateCheckThread.Execute;
Var
  dl: TSynapesDownloader;
Begin
  dl := TSynapesDownloader.Create;
  Try
    If Not ForceDirectories(ExtractFilePath(fFilename)) Then Begin
      fError := 'Error, could not create: ' + ExtractFilePath(fFilename);
    End
    Else Begin
      fOk := dl.DownloadFile(VersionInfoUrl, fFilename);
      If Not fOk Then fError := 'Error, no data received from: ' + VersionInfoUrl;
    End;
  Except
    On e: Exception Do Begin
      fOk := false;
      fError := e.Message;
    End;
  End;
  dl.free;
  Synchronize(@DoDone);
End;

Procedure TUpdateCheckThread.DoDone;
Begin
  If assigned(fOnDone) Then fOnDone(fOk, fFilename, fError);
End;

End.
