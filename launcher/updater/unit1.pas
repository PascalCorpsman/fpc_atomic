(******************************************************************************)
(* ctd_updater                                                     16.05.2024 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : updater to update the fpc_atomic_launcher app                *)
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
(*               0.02 - ADD: window with text and progress while updating     *)
(*                      ADD: translation (updater_lang files)                 *)
(*                      FIX: old launcher is renamed (.old) instead of        *)
(*                           deleted, restored if the copy fails, a launcher  *)
(*                           is always started again                          *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    Procedure ApplyLanguage();
    Procedure DoUpdate();
    Procedure StartLauncher(Const Target: String);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses FileUtil, process, UTF8Process, IniFiles, ulanguage;

Function AtomicApplicationName: String;
Begin
  result := 'fpc_atomic';
End;

(*
 * Die Config Datei von fpc_atomic (wie GetAtomicConfigFile in uatomic_global.pas,
 * die Unit wird nicht benutzt, damit der Updater ohne den Logger auskommt)
 *)

Function GetAtomicConfigFile: String;
Var
  Event: TGetAppNameEvent;
Begin
  Event := OnGetApplicationName;
  OnGetApplicationName := @AtomicApplicationName;
  result := GetAppConfigFile(false, true);
  OnGetApplicationName := Event;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Updater ver. 0.02';
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  If Language.CurrentLanguage <> '' Then exit;
  ApplyLanguage();
  Label1.Caption := R_Updating;
  // Das Fenster soll sichtbar sein, bevor die Arbeit beginnt
  Timer1.Enabled := true;
End;

Procedure TForm1.ApplyLanguage;
Var
  ini: TIniFile;
  Code, Filename: String;
Begin
  // Gleiche Sprache wie der Launcher, Englisch wenn es dafür keine Datei gibt
  ini := TIniFile.Create(GetAtomicConfigFile());
  Code := ini.ReadString('Launcher', 'Language', 'en');
  ini.free;
  Filename := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'updater_lang.';
  If Not FileExists(Filename + Code) Then Code := 'en';
  Language.AddIgnoredIdentifier('Form1.Caption');
  Language.CurrentLanguage := Filename + Code;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  Timer1.Enabled := false;
  DoUpdate();
  close;
End;

Procedure TForm1.StartLauncher(Const Target: String);
Var
  p: TProcessUTF8;
Begin
  p := TProcessUTF8.Create(Nil);
  p.Executable := Target;
  p.Execute;
  p.free;
End;

Procedure TForm1.DoUpdate;
Var
  fileToCopyAndStart, target, backup: String;
  aTime: QWord;
  moved: Boolean;
{$IFDEF LINUX}
  p: TProcessUTF8;
{$ENDIF}
Begin
  If ParamCount >= 1 Then Begin
    fileToCopyAndStart := ParamStr(1);
  End
  Else Begin
    showmessage(R_InvalidParameters);
    exit;
  End;
  If Not FileExists(fileToCopyAndStart) Then Begin
    showmessage(format(RF_NotFound, [fileToCopyAndStart]));
    exit;
  End;
  target := ExtractFileName(fileToCopyAndStart);
  backup := target + '.old';
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  (*
   * Der alte Launcher wird umbenannt statt gelöscht, damit er wieder hergestellt
   * werden kann, wenn das Kopieren scheitert.
   *)
  If FileExists(backup) Then DeleteFile(backup);
  aTime := GetTickCount64;
  moved := Not FileExists(target);
  While (Not moved) And (aTime + 10000 > GetTickCount64) Do Begin
    moved := RenameFile(target, backup);
    If Not moved Then Begin
      sleep(100);
      Application.ProcessMessages;
    End;
  End;
  If Not moved Then Begin
    // Der alte Launcher läuft noch, es wurde nichts geändert
    Showmessage(format(RF_StillRunning, [target]));
    exit;
  End;
  If Not CopyFile(fileToCopyAndStart, target) Then Begin
    If FileExists(target) Then DeleteFile(target);
    RenameFile(backup, target);
    Showmessage(format(RF_CopyFailed, [fileToCopyAndStart, target]));
    StartLauncher(target);
    exit;
  End;
{$IFDEF LINUX}
  p := TProcessUTF8.Create(Nil);
  p.Options := [poWaitOnExit];
  p.CurrentDirectory := GetCurrentDir;
  p.Executable := 'chmod';
  p.Parameters.Add('+x');
  p.Parameters.Add(target);
  p.Execute;
  p.free;
{$ENDIF}
  // Der Launcher löscht die Sicherung beim Start
  StartLauncher(target);
End;

End.
