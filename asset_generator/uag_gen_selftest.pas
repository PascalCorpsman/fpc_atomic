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
 * Test pattern for the core (canvas, styles, sheets, outputs). Not part of a
 * game set, only generated with --only selftest. Also a small example of how
 * a generator is written.
 *)
Unit uag_gen_selftest;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Implementation

Uses uag_image, uag_canvas, uag_context, uag_output, uag_render, uag_registry, uag_math;

Procedure GenerateSelfTest(Ctx: TAGContext; Output: TAGOutput);
Var
  area: TAGArea;
  img: TAGImage;
  hue: Double;

  Procedure DrawFrame(C: TAGCanvas; Frame: Integer);
  Var
    a: Double;
  Begin
    a := AG_TWO_PI * Frame / 8;
    // body in player color
    C.Capsule(30 + 10 * AGCos(a), 30 + 10 * AGSin(a), 80, 80, 12);
    DrawShapeOutlined(C, Ctx.Style, PlayerGreen(0.6), AGBlack);
    // a ball that must keep its color
    C.Circle(70, 35, 16);
    DrawShape(C, Ctx.Style, NotGreen(AGHSV(hue, 0.7, 0.85)));
    // star with a hole (even odd)
    C.Push;
    C.Translate(30, 80);
    C.Rotate(a);
    C.FillRule := frEvenOdd;
    C.Polygon([0, -18, 5, -5, 18, -5, 8, 3, 12, 16, 0, 8, -12, 16, -8, 3, -18, -5, -5, -5]);
    C.Circle(0, 0, 4);
    DrawShape(C, Ctx.Style, AGHex($E0C040));
    C.FillRule := frNonZero;
    C.Pop;
  End;

Begin
  area := Ctx.Area('selftest');
  hue := area.Vary('hue', 200, 160);
  img := RenderSheet(Ctx, 4, 2, 110, 110, 8, @DrawFrame);
  Try
    Output.SaveImage('selftest/sheet.png', img, smKeyed);
    Output.SaveImage('selftest/sheet_alpha.png', img, smAlpha);
    Output.SaveAni('selftest/sheet.ani', img, AniSpec(0, 100, 4, 2, 110, 110, [0], [8]));
  Finally
    img.Free;
  End;
  Output.SaveText('selftest/files.txt', ['#test', 'sheet.png;100;4;2;110;110;8;55;90']);
End;

Initialization
  RegisterGenerator('selftest', 'Test pattern for the core', 4, @GenerateSelfTest);

End.
