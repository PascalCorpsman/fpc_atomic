program atomic_launcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, unit2, unit3, ucdextractor, uopengl_animation,
  usdlwizzard;

(*                                                                            *)
(* Modified by  : Pavel Zverina                                               *)
(* Note         : This file has been modified while preserving the original   *)
(*                authorship and license terms.                                *)
(*                                                                            *)
{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

