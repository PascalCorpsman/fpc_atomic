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
  Forms, Unit1, Unit18,
  SysUtils
  ;

Begin
  // Do not Report if there is no error, that only confuses player..
{$IF declared(UseHeapTrace)}
  GlobalSkipIfNoLeaks := True;
{$ENDIF}
  Application.Title:='';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm18, Form18);
  Application.Run;
End.

