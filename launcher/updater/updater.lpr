(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ctd_updater                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Program updater;

{$MODE objfpc}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
{$IFDEF HASAMIGA}
  athreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1
  { you can add units after this };

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.

