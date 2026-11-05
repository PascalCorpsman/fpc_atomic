(******************************************************************************)
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
(*
 * UI version of the asset generator (see uag_gui for the command line and
 * the contract with the launcher).
 *)
Program asset_generator_gui;

{$MODE ObjFPC}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uag_gui,
  ulaunchersound in '../launcher/ulaunchersound.pas', // sound test of the launcher, plays the editor's sounds
  Unit1, Unit2,
  // every generator unit registers itself, the order here is the order of generation (same as the command line version)
  uag_gen_items,
  uag_gen_atomic,
  uag_gen_world,
  uag_gen_screens,
  uag_gen_schemes,
  uag_gen_sounds,
  uag_preview;

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  ParseCommandLine;
  If GuiMode = gmAuto Then
    Application.CreateForm(TForm2, Form2)
  Else
    Application.CreateForm(TForm1, Form1);
  ApplyLanguage;
  Application.Run;
  If GuiWritten Then
    ExitCode := 0
  Else
    ExitCode := 1;
End.
