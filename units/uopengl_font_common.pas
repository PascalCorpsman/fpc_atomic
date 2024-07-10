(******************************************************************************)
(* uOpenGL_font_common                                             06.07.2024 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : common anchestor class to derive different OpenGL Fonts from *)
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

Unit uopengl_font_common;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uvectormath, Graphics, fpImage;

Type

  { TOpenGL_Font }

  TOpenGL_Font = Class
  protected
    fColor: TVector3; // Farbe der Schrift
    fsize: Single; // Größe der Schrift in Pixeln ( Höhe )

    Function getcolor: TColor; virtual;
    Function getcolorfp: TFPColor; virtual;
    Function getcolorV3: TVector3; virtual;
    Function getsize: Single; virtual;
    Procedure setColor(AValue: TColor); virtual;
    Procedure setColorV3(AValue: TVector3); virtual;
    Procedure setColorfp(AValue: TFPColor); virtual;
    Procedure setsize(AValue: Single); virtual;
  public
    (*
     * Farbe, Größe
     *)
    Property Color: TColor read getcolor write setColor; // Zugriff auf die Schriftfarbe
    Property ColorV3: TVector3 read getcolorV3 write setColorV3; // Zugriff auf die Schriftfarbe
    Property Colorfp: TFPColor read getcolorfp write setColorfp; // Zugriff auf die Schriftfarbe
    Property Size: Single read getsize write setsize; // Höhe der Schrift in Pixeln ( Gillt nur für Textout im 3D-Mode wird die Höhe explizit gesetzt)

    Constructor Create(); virtual;

    (*
     * Text Dimensionen
     *)
    Function TextWidth(Text: String): single; virtual; // abstract
    Function TextHeight(text: String): single; virtual; // abstract

    (*
     * Zeichen Routinen
     *)
    Procedure Textout(x, y: Integer; Text: String); virtual; // abstract
  End;

Implementation

Uses math;

{ TOpenGL_Font }

Constructor TOpenGL_Font.Create;
Begin
  Inherited Create();
  fColor := v3(1, 1, 1);
  fsize := 0; // Needs to be set by child.
End;

Function TOpenGL_Font.TextWidth(Text: String): single;
Begin
  result := 0;
  Raise exception.create(self.ClassName + '.TextWidth: not implemented.');
End;

Function TOpenGL_Font.TextHeight(text: String): single;
Begin
  result := 0;
  Raise exception.create(self.ClassName + '.TextHeight: not implemented.');
End;

Procedure TOpenGL_Font.Textout(x, y: Integer; Text: String);
Begin
  Raise exception.create(self.ClassName + '.Textout: not implemented.');
End;

Function TOpenGL_Font.getcolor: TColor;
Var
  r, g, b: integer;
Begin
  r := max(0, min(255, round(fColor.x * 255)));
  g := max(0, min(255, round(fColor.y * 255)));
  b := max(0, min(255, round(fColor.z * 255)));
  result := r Or (g Shl 8) Or (b Shl 16);
End;

Function TOpenGL_Font.getcolorfp: TFPColor;
Var
  r, g, b: integer;
Begin
  r := max(0, min(255, round(fColor.x * 255))) Shl 8 Or $FF;
  g := max(0, min(255, round(fColor.y * 255))) Shl 8 Or $FF;
  b := max(0, min(255, round(fColor.z * 255))) Shl 8 Or $FF;
  result.red := r;
  result.green := g;
  result.blue := b;
  result.alpha := 0;
End;

Function TOpenGL_Font.getcolorV3: TVector3;
Begin
  result := fColor;
End;

Function TOpenGL_Font.getsize: Single;
Begin
  result := fsize;
End;

Procedure TOpenGL_Font.setColor(AValue: TColor);
Begin
  fcolor.x := byte(Avalue) / 255;
  fcolor.y := byte(Avalue Shr 8) / 255;
  fcolor.z := byte(Avalue Shr 16) / 255;
End;

Procedure TOpenGL_Font.setColorV3(AValue: TVector3);
Begin
  fColor := AValue;
End;

Procedure TOpenGL_Font.setColorfp(AValue: TFPColor);
Begin
  fcolor.x := ((AValue.red And $FF00) Shr 8) / 255;
  fcolor.y := ((AValue.green And $FF00) Shr 8) / 255;
  fcolor.z := ((AValue.blue And $FF00) Shr 8) / 255;
End;

Procedure TOpenGL_Font.setsize(AValue: Single);
Begin
  fsize := AValue;
End;

End.

