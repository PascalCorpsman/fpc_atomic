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
Program atomic_launcher;

{$MODE objfpc}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
{$IFDEF HASAMIGA}
  athreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, unit2, unit3, unit4, unit5
{$IFDEF LCLGTK2}
{$WARNING 'Don''t use GTK2 compiles anymore, this feature will be removed in near future.'}
  , gtk2
{$ENDIF}
  ;

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
{$IFDEF LCLGTK2}
  // GTK2 opens combo boxes as a menu over the whole screen height, show a scrolling list below the box instead
  gtk_rc_parse_string('style "atomic_combo" { GtkComboBox::appears-as-list = 1 } class "GtkComboBox" style "atomic_combo"');
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.Run;
End.

