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
Program fpc_atomic;

{$MODE objfpc}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, Unit18, uatomic, ukeyboarddialog, ukeygrabber,
  uearlylog, SysUtils
  ;

Begin
  // Early logging - before anything else
{$IFDEF DARWIN}
  EarlyLog('=== Program start ===');
  EarlyLog('Executable: ' + ParamStr(0));
  EarlyLog('Working directory: ' + GetCurrentDir);
{$ENDIF}
  
  // Do not Report if there is no error, that only confuses player..
{$IF declared(UseHeapTrace)}
  GlobalSkipIfNoLeaks := True;
{$ENDIF}
{$IFDEF DARWIN}
  EarlyLog('Before Application.Initialize');
{$ENDIF}
  Application.Title:='';
  Application.Initialize;
{$IFDEF DARWIN}
  EarlyLog('After Application.Initialize');
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
{$IFDEF DARWIN}
  EarlyLog('After CreateForm(TForm1, Form1)');
{$ENDIF}
  Application.CreateForm(TForm18, Form18);
{$IFDEF DARWIN}
  EarlyLog('After CreateForm(TForm18, Form18)');
{$ENDIF}
  Application.Run;
{$IFDEF DARWIN}
  EarlyLog('=== Program end ===');
{$ENDIF}
End.

