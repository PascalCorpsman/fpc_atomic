(******************************************************************************)
(* ctd_updater                                                     16.05.2024 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
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
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses FileUtil, process, UTF8Process;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Updater ver. 0.01';
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  fileToCopyAndStart, target: String;
  aTime: QWord;
  p: TProcessUTF8;
Begin
  If ParamCount >= 1 Then Begin
    fileToCopyAndStart := ParamStr(1);
  End
  Else Begin
    showmessage('Error, invalid parameters, terminate now.');
    halt;
  End;
  If Not FileExists(fileToCopyAndStart) Then Begin
    showmessage('Error, could not find:' + fileToCopyAndStart);
    halt;
  End;
  target := ExtractFileName(fileToCopyAndStart);
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  // Wir Versuchen die Ziel Datei zu löschen
  aTime := GetTickCount64;
  While FileExists(target) And (aTime + 10000 > GetTickCount64) Do Begin
    sleep(100);
    DeleteFile(target);
  End;
  If FileExists(target) Then Begin
    Showmessage('Error, could not delete: ' + target);
    halt;
  End;
  If Not CopyFile(fileToCopyAndStart, target) Then Begin
    Showmessage('Error, could not copy: ' + fileToCopyAndStart + ' -> ' + target);
    halt;
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
  p := TProcessUTF8.Create(Nil);
  p.Executable := target;
  p.Execute;
  p.free;
  timer1.Enabled := true;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  close;
End;

End.

