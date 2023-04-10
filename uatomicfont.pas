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
Unit uatomicfont;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, uOpenGL_ASCII_Font, LResources;

Type
  (*
   * Wie OpenGL Asciifont nur mit einer "2." Font im Hintergrund
   * -> Damit kann man dann einen "Rahmen" um die Buchstaben machen.
   *
   * Ein Buchstabe hat dadurch die Dimension 10/14 !
   *)

  { TAtomicFont }

  TAtomicFont = Class
  private
    Font1, Font2: TOpenGL_ASCII_Font;
    Procedure Clear;
    Function getBackColor: TColor;
    Function getColor: TColor;
    Procedure SetBackColor(AValue: TColor);
    Procedure setColor(AValue: TColor);
  public
    Property BackColor: TColor read getBackColor write SetBackColor;
    Property Color: TColor read getColor write setColor;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure CreateFont(); // Muss im Make Current Aufgerufen werden !

    Procedure Textout(x, y: integer; Text: String);
  End;

  { TAtomicBigFont }

  TAtomicBigFont = Class
  private
    fDigits: Array[0..9] Of INteger;
    fdouble: Integer;
    fInfinity: Integer;
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure CreateFont(Dir: String); // Muss im Make Current Aufgerufen werden !

    Procedure Textout(x, y: integer; Time: integer);
  End;

Var
  AtomicFont: TAtomicFont = Nil;
  AtomicBigFont: TAtomicBigFont = Nil;

Implementation

Uses dglOpenGL, uopengl_graphikengine, uvectormath;

{ TAtomicBigFont }

Constructor TAtomicBigFont.Create;
Begin
  Inherited Create;
End;

Destructor TAtomicBigFont.Destroy;
Begin

End;

Procedure TAtomicBigFont.CreateFont(Dir: String);
Var
  p: TPortableNetworkGraphic;
  b1, b2: TBitmap;
  i: Integer;
Begin
  fInfinity := OpenGL_GraphikEngine.LoadAlphaColorGraphik(dir + 'infinity.png', Fuchsia, smStretchHard);
  p := TPortableNetworkGraphic.Create;
  p.LoadFromFile(dir + 'bigfont.png');
  b1 := TBitmap.Create;
  b1.Assign(p);
  b1.Transparent := false;
  b2 := TBitmap.Create;
  b2.Width := 18;
  b2.Height := 27;
  For i := 0 To 10 Do Begin
    b2.Canvas.Draw(-(i Mod 4) * 18, -(i Div 4) * 27, b1);
    If i = 10 Then Begin
      fdouble := OpenGL_GraphikEngine.LoadAlphaColorGraphik(b2, 'Bigfont_Letter' + inttostr(i), Fuchsia, smStretchHard);
    End
    Else Begin
      fDigits[i] := OpenGL_GraphikEngine.LoadAlphaColorGraphik(b2, 'Bigfont_Letter' + inttostr(i), Fuchsia, smStretchHard);
    End;
  End;
  b2.free;
  b1.free;
  p.free;
End;

Procedure TAtomicBigFont.Textout(x, y: integer; Time: integer);
Var
  m, s, i: integer;
  st: String;
Begin
  glPushMatrix;
  glAlphaFunc(GL_LESS, 0.5);
  glEnable(GL_ALPHA_TEST);
  glTranslatef(x, y, 0);
  If time < 0 Then Begin
    glTranslatef(0, 10, 0);
    RenderAlphaQuad(v2(24, 17 / 2), 48, -17, 0, fInfinity);
  End
  Else Begin
    m := time Div 60;
    s := time Mod 60;
    // Die Minuten
    st := format('%d', [m]);
    For i := 1 To length(st) Do Begin
      RenderAlphaQuad(v2(9, 27 / 2), 18, -27, 0, fDigits[ord(st[i]) - ord('0')]);
      glTranslatef(18, 0, 0);
    End;
    RenderAlphaQuad(v2(9, 27 / 2), 18, -27, 0, fdouble);
    glTranslatef(18, 0, 0);
    // Die Sekunden
    st := format('%0.2d', [s]);
    For i := 1 To length(st) Do Begin
      RenderAlphaQuad(v2(9, 27 / 2), 18, -27, 0, fDigits[ord(st[i]) - ord('0')]);
      glTranslatef(18, 0, 0);
    End;
  End;
  gldisable(GL_ALPHA_TEST);
  glPopMatrix;
End;

{ TAtomicFont }

Constructor TAtomicFont.Create;
Begin
  Inherited Create;
  Font1 := Nil;
  Font2 := Nil;
End;

Destructor TAtomicFont.Destroy;
Begin
  Clear;
End;

Procedure TAtomicFont.CreateFont;
Var
  bitmap: TBitmap;
Begin
  Clear;
  Bitmap := TBitmap.Create;
  bitmap.LoadFromLazarusResource('OpenGLFont1'); // Die Vordergrund Schriftart
  Font1 := TOpenGL_ASCII_Font.Create(bitmap, 10, 14, 256);
  bitmap.Free;

  Bitmap := TBitmap.Create;
  bitmap.LoadFromLazarusResource('OpenGLFont2'); // Die Hintergrund Schriftart
  Font2 := TOpenGL_ASCII_Font.Create(bitmap, 10, 14, 256);
  bitmap.Free;

  Font1.Color := clWhite;
  Font2.Color := clBlack;
End;

Procedure TAtomicFont.Clear;
Begin
  If assigned(Font1) Then Font1.Free;
  If assigned(Font2) Then Font2.Free;
  font1 := Nil;
  font2 := Nil;
End;

Procedure TAtomicFont.setColor(AValue: TColor);
Begin
  Font1.Color := AValue;
End;

Function TAtomicFont.getColor: TColor;
Begin
  result := Font1.Color;
End;

Procedure TAtomicFont.SetBackColor(AValue: TColor);
Begin
  Font2.Color := AValue;
End;

Function TAtomicFont.getBackColor: TColor;
Begin
  result := Font2.Color;
End;

Procedure TAtomicFont.Textout(x, y: integer; Text: String);
Begin
  glPushMatrix;
  font2.Textout(x, y, text);
  glTranslatef(0, 0, 0.025);
  font1.Textout(x, y, text);
  glPopMatrix;
End;

Initialization

  (*
   * Ressourcendatei wurde erzeugt mittels :
   *
   * /home/corpsman/lazarus/tools/lazres /sda5/sda5/Tools/Projects/FPC_Atomic/uatomicfont.ressource /sda5/sda5/Tools/Projects/FPC_Atomic/GFX/font_1.bmp=OpenGLFont1 /sda5/sda5/Tools/Projects/FPC_Atomic/GFX/font_2.bmp=OpenGLFont2
   *
   *)
{$I uatomicfont.ressource}

  AtomicFont := TAtomicFont.Create;
  AtomicBigFont := TAtomicBigFont.Create();

Finalization

  AtomicFont.free;
  AtomicFont := Nil;
  AtomicBigFont.free;
  AtomicBigFont := Nil;

End.

