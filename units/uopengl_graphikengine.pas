(******************************************************************************)
(* uOpenGLGraphikEngine.pas                                        ??.??.???? *)
(*                                                                            *)
(* Version     : 0.10                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This unit tries to help the user to use graphiks with OpenGL *)
(*               without the need of special knowlege in bitmanipulations.    *)
(*                                                                            *)
(*               The unit is in alpha-state there are a lot functions that    *)
(*               work not correct, please read the implementation part        *)
(*               carefully before use.                                        *)
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
(* History     : 0.01 - Initial version ( exported from balanced )            *)
(*               0.02 - Hinzufügen der RenderBillboard Routine                *)
(*               0.03 - Ändern RenderQuad zur unterstützung für negative Width*)
(*                      Damit Bugfix Spiegelungsbug für 180° rotation         *)
(*               0.04 - added glcolor()                                       *)
(*               0.05 - neuer Stretch mode Nearest Neighbour interpolation    *)
(*               0.06 - Methode RemoveGraphik                                 *)
(*               0.07 - LoadAlphaPNGGraphik                                   *)
(*               0.08 - Fix Memleaks                                          *)
(*               0.09 - Fix LoadAlphaColorGraphik                             *)
(*               0.10 - Fix speedup graphik loading                           *)
(*                                                                            *)
(******************************************************************************)
Unit uopengl_graphikengine;

{$IFDEF FPC}
{$MODE objFPC}{$H+}
{$ENDIF}

Interface

// Je nach dem wie mans Braucht mus hier angepasst werden!!

{.$DEFINE USE_GL}

(*
Ist dieser Schalter an, werden diverse Informationen in die Konsole geschrieben
 - Anzeige des Allokierten OpenGLSpeichers
 - Anzeige der Anzahl der Allokierten Texturen

*)
{.$DEFINE DEBUGGOUTPUT}

Uses
  // Die OpenGL Schnittstelle
{$IFDEF USE_GL}
  gl, glu,
{$ELSE}
  dglopengl,
{$ENDIF}
  // IDE spezifisches
  LCLType, // HBitmap type
  IntfGraphics, // TLazIntfImage type
  fpImage, // TFPColor type
  //  lazjpg, // ist in "Lazarus/Components/Image" zu finden, dieses Packete mus installiert werden !!
  Graphics, // TBitmap
  //, // Für PNG Dateien
  // dialogs, // Debugg
  ugraphics, // TRGB
  Classes, // Function Point
  math, // SinCos
  uvectormath, // Mathe Operationen
  sysutils; // Fileexists, ..

Type

  TStretchMode = (
    smNone, // Kein Stretching, wirft eine Exception bei Größen <> 2^n
    smStretch, // Die Textur wird mittels Stretchdraw bilinear hochgesampelt
    smStretchHard, // Die Textur wird via Nearest Neighbour gestretcht (besonders wichtig bei Binärer Transparenz)
    smClamp // Die ZielTextur wird groß gemacht, und die Quelltextur Oben Links reinkopiert.
    );

  TFRect = Record
    Left, Top, Right, Bottom: Single;
  End;

  TSubImage = Record
    ImageData: Integer; // Pointer auf die Texture
    ImageRect: TFrect; // Ausschnitt der Textur der Gerendert werden soll
    Width: Integer; // Breite auf dem Monitor
    Height: Integer; // Höhe auf dem Monitor
  End;

  TGraphikItem = Record
    Image: Integer;
    Name: String;
    IsAlphaImage: Boolean; // Wenn True, dann wurde Image mit AlphaKanal geladen und kann geblendet werden
    Stretched: TStretchMode;
    OrigWidth: integer;
    OrigHeight: integer;
    StretchedWidth: integer;
    StretchedHeight: integer;
  End;

  { TOpenGL_GraphikEngine }

  TOpenGL_GraphikEngine = Class
  private
    FImages: Array Of TGraphikItem;
{$IFDEF DEBUGGOUTPUT}
    OpenGLBufCount: int64;
{$ENDIF}
  public
    Constructor create;
    Destructor destroy; override;
    Function GetInfo(Value: String): TGraphikItem;
    Function GetInfo(Value: integer): TGraphikItem;
    Function Find(Value: String; ExceptionOnNotExists: Boolean = True): integer; // Gibt die Textur wieder die unter dem Namen gespeichert ist. Pfadangaben nur bei doppeldeutigen namen notwendig.
    Function FindItem(Value: String; ExceptionOnNotExists: Boolean = True): TGraphikItem; // Gibt die Textur wieder die unter dem Namen gespeichert ist. Pfadangaben nur bei doppeldeutigen namen notwendig.
    Procedure Clear; // Alles Freigeben
    (*
    Funktionen die Machen was sie sollen
    *)
    Function LoadGraphik(Filename: String; Stretch: TStretchmode = smNone): Integer; overload; // Laden einer Graphik ohne Alphakanal
    Function LoadGraphikItem(Filename: String; Stretch: TStretchmode = smNone): TGraphikItem; overload; // Laden einer Graphik ohne Alphakanal
    Function LoadGraphik(Const Graphik: TBitmap; Name: String; Stretch: TStretchmode = smNone): Integer; overload; // Laden einer Graphik ohne Alphakanal
    Function LoadAlphaColorGraphik(Filename: String; Color: TRGB; Stretch: TStretchmode = smNone): Integer; overload; // Lädt eine Alphagraphik und setzt den Wert von Color = Transparent.
    Function LoadAlphaColorGraphik(Const Graphik: TBitmap; Name: String; Color: TRGB; Stretch: TStretchmode = smNone): Integer; overload; // Lädt eine Alphagraphik und setzt den Wert von Color = Transparent.
    (*
    Funktionen die NICHT Machen was sie sollen

    Sie werden aber trotzdem in einigen Projekten genutzt und Funktionieren dort.
    *)
    Function LoadAlphaValueGraphik(Filename: String; AlphaValue: byte; Stretch: TStretchmode = smNone): Integer; Deprecated; // Laden einer Graphik, und Vorgeben eines Gesammten Alpha wertes
    Function LoadAlphaGraphik(Filename: String; Stretch: TStretchmode = smNone): Integer; overload; // Laden einer Transparenten Graphik, clfuchsia = Transparent
    Function LoadAlphaGraphikItem(Filename: String; Stretch: TStretchmode = smNone): TGraphikItem; overload; // Laden einer Transparenten Graphik, clfuchsia = Transparent
    Function LoadAlphaGraphik(Const Graphik, AlphaMask: Tbitmap; Name: String; Stretch: TStretchmode = smNone): integer; overload; // Lädt eine Graphik aus TBitmap, und Lädt den Alphakanal aus den Luminanzdaten von Alphamask, Name dient zum späteren Wiederfinden
    Function LoadAlphaGraphik(Graphik, AlphaMask: String; Stretch: TStretchmode = smNone): integer; overload; // Lädt eine Graphik aus TBitmap, und Lädt den Alphakanal aus den Luminanzdaten von Alphamask
    Function LoadAlphaGraphikItem(Graphik, AlphaMask: String; Stretch: TStretchmode = smNone): TGraphikItem; overload; // Lädt eine Graphik aus TBitmap, und Lädt den Alphakanal aus den Luminanzdaten von Alphamask
    Function LoadAlphaPNGGraphik(Graphik: String; Stretch: TStretchmode = smNone): integer; // Lädt eine .png Graphik und nutzt deren Alpha Kanal als Alpha
    (*
     * Wenn eine Graphik Explicit nicht mehr gecached werden soll, true wenn sie gefunden und gelöscht werden konnte
     *)
    Function RemoveGraphik(Value: TGraphikItem): Boolean; overload;
    Function RemoveGraphik(Value: integer): Boolean; overload;
  End;

Const
  Fuchsia: TRGB = (r: 255; g: 0; b: 255);
  Black: TRGB = (r: 0; g: 0; b: 0);
  White: TRGB = (r: 255; g: 255; b: 255);

Var
  OpenGL_GraphikEngine: TOpenGL_GraphikEngine;

  (*
  Rendert in 2D ein Quad

  ACHTUNG Bei Bildern mit der KantenLänge 1 Kommt blödsinn = kein Bild raus !!

  ACHTUNG Diese Routinen funktionieren nicht immer mit eingeschalteten CullFacing !!
  *)
Procedure RenderAlphaQuad(Top, Left: Single; Image: TGraphikItem); overload; // Fertig Getestet
Procedure RenderAlphaQuad(Middle: Tpoint; Width, Height, Angle: Integer; Texture: integer = 0); overload; // Fertig Getestet
Procedure RenderAlphaRQuad(TopLeft, BottomRight: TPoint; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0); overload; // Fertig Getestet
Procedure RenderAlphaRQuad(TopLeft, BottomRight: TVector2; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0); overload; // Fertig Getestet
Procedure RenderAlphaImage(Value: TSubImage);

Procedure RenderQuad(Top, Left: Single; Image: TGraphikItem); overload;
Procedure RenderQuad(Middle: TVector2; Angle: Single; Image: TGraphikItem); overload;
Procedure RenderQuad(Middle: Tpoint; Width, Height, Angle: Integer; Texture: integer = 0); overload; // Fertig Getestet
Procedure RenderQuad(Middle: TVector2; Width, Height, Angle: Single; Texture: integer = 0); overload; // Fertig Getestet

Procedure RenderQuad(TopLeft, BottomRight: TPoint; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0); overload; // Fertig Getestet
Procedure RenderQuad(TopLeft, BottomRight: TVector2; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0); overload; // Fertig Getestet
(*
Rendert einen Kreis, ohne Füllung
*)
Procedure RenderCircle(Middle: TVector2; Steps, Radius: Integer);

(*
Rendert ein Billboard, ACHTUNG bei mehr als einem Bildboard Pro Renderschritt, sollte das "Hohlen" der Modelviewmatrix ausgelagert werden !!
Mittels Dimension kann man die Ausbreitung in x und y Richtung angeben. Position ist stets der Mittelpunkt der textur
*)
Procedure RenderBillboard(Position: TVector3; Dimension: TVector2; Texture: integer = 0);

(*
Erstellt einen Screenshot aus dem Aktuellen Framebuffer,
inclusive Ermittlung der Auflösung.
*)
Function OpenGLScreenshot: TBitmap;

(*
 * Setzt die OpenGLFarbe auf den Wert von TColor
 * Alpha = 0 = Opak
 *)
Procedure glColor(Color: TColor; Alpha: byte = 0);

Function FRect(Top, Left, Bottom, Right: Single): TFRect;

Implementation

{$IFDEF DEBUGGOUTPUT}

Function FileSizetoString(Value: Int64): String;
Var
  s: Char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value > 1024 Then Begin
    s := 'K';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'M';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'G';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'T';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'P';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If (r Div 100) <> 0 Then
    result := inttostr(value) + ',' + inttostr(r Div 100) + s + 'B'
  Else
    result := inttostr(value) + s + 'B'
End;

{$ENDIF}

(*
 * Der Folgende Code bassiert auf dieser Grundlage :
 *  http://wiki.delphigl.com/index.php/Sphärisches_Billboard
 *)

Procedure RenderBillboard(Position: TVector3; Dimension: TVector2;
  Texture: integer);
Var
  up, right: TVector3;
  Matrix: TMatrix4x4;
Begin
  If Texture <> 0 Then Begin
    glBindTexture(gl_texture_2d, Texture);
  End;
  (*
   * Dieser Code muss theoretisch nur ein mal gerechnet werden ( so lange sich die Augposition nicht ändert )
   *)
  glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix[0, 0]);
  glColor4f(1, 1, 1, 1);
  // Auslesen des Right und Up Vektors ( das geht nur wenn man annimmt das die ModelView Matrix Orthogonal ist )
  Right := V3(Matrix[0, 0], Matrix[1, 0], Matrix[2, 0]);
  Up := V3(Matrix[0, 1], Matrix[1, 1], Matrix[2, 1]);
  // Das Billboard auf die gewünschte Größe Skalieren
  glbegin(GL_QUADS);
  (*
   * Mit unterschiedlichen Positionen/ Dimensionen könnten nun mehrere Billboards gerendert werden.
   *)
  right := ScaleV3(Dimension.x / 2, NormV3(right));
  up := ScaleV3(Dimension.y / 2, NormV3(up));
  //Der eigentliche Renderschritt
  glTexCoord2d(1, 1);
  glVertex3f(Position.x + Right.x + Up.x, Position.y + Right.y + Up.y, Position.z + Right.z + Up.z);
  glTexCoord2d(0, 1);
  glVertex3f(Position.x - Right.x + Up.x, Position.y - Right.y + Up.y, Position.z - Right.z + Up.z);
  glTexCoord2d(0, 0);
  glVertex3f(Position.x - Right.x - Up.x, Position.y - Right.y - Up.y, Position.z - Right.z - Up.z);
  glTexCoord2d(1, 0);
  glVertex3f(Position.x + Right.x - Up.x, Position.y + Right.y - Up.y, Position.z + Right.z - Up.z);
  glend();
End;

Function OpenGLScreenshot: TBitmap;
Var
  dim: Array[0..3] Of Integer;
  c: Array Of Array[0..3] Of Byte;
  z, i, j: integer;
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  CurColor: TFPColor;
Begin
  // Auslesen der Framebuffer Auflösung
  glGetIntegerv(GL_VIEWPORT, @dim[0]);
  // Erstellen des Bitmaps
  result := TBitmap.create;
  result.pixelformat := pf24bit;
  result.width := dim[2];
  result.height := dim[3];
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(result.Handle, result.MaskHandle);
  c := Nil;
  setlength(c, dim[2] * dim[3]);
  // Auslesen des Framebuffers in einen temporären Speicher
  glReadPixels(dim[0], dim[1], dim[2], dim[3], GL_RGBA, GL_UNSIGNED_BYTE, @c[0, 0]);
  // Umschreiben des Temporären Speichers in das TBitmap
  z := 0;
  For j := 0 To result.height - 1 Do
    For i := 0 To result.width - 1 Do Begin
      CurColor.red := c[z][0] * 256;
      CurColor.green := c[z][1] * 256;
      CurColor.blue := c[z][2] * 256;
      // c[z][3] wäre der Alphakanal, aber den Braucht man ja hier nicht ...
      //TempIntfImg.Colors[i, j] := CurColor;
      TempIntfImg.Colors[i, result.height - 1 - j] := CurColor;
      inc(z);
    End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  result.Handle := ImgHandle;
  result.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
End;

Procedure glColor(Color: TColor; Alpha: byte);
Var
  r, g, b: Byte;
Begin
  r := color And $FF;
  g := (color Shr 8) And $FF;
  b := (color Shr 16) And $FF;
  glColor4ub(r, g, b, Alpha);
End;

Function FRect(Top, Left, Bottom, Right: Single): TFRect;
Begin
  result.Top := Top;
  result.Left := Left;
  result.Bottom := Bottom;
  result.Right := Right;
End;

Procedure RenderAlphaImage(Value: TSubImage);
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  glbindtexture(GL_TEXTURE_2d, Value.ImageData);
  glbegin(gl_quads);
  glTexCoord2f(Value.ImageRect.left, Value.ImageRect.top);
  glvertex3f(0, 0, 0);
  glTexCoord2f(Value.ImageRect.Right, Value.ImageRect.top);
  glvertex3f(value.Width, 0, 0);
  glTexCoord2f(Value.ImageRect.right, Value.ImageRect.Bottom);
  glvertex3f(value.width, Value.Height, 0);
  glTexCoord2f(Value.ImageRect.left, Value.ImageRect.Bottom);
  glvertex3f(0, Value.height, 0);
  glend;
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

Function IsPowerOfTwo(Value: Integer): Boolean;
Var
  i: Integer;
Begin
  i := 1;
  While i < Value Do
    i := i Shl 1;
  result := i = Value;
End;

Function GetNextPowerOfTwo(Value: integer): Integer;
Var
  i: Integer;
Begin
  i := 1;
  While i < Value Do
    i := i Shl 1;
  result := i;
End;

Procedure RenderCircle(Middle: TVector2; Steps, Radius: Integer);
Var
  delta: Double;
  s, c: extended;
  i: Integer;
Begin
  Delta := 2 * Pi / Steps;
  glbegin(gl_Line_Loop);
  For i := 0 To Steps - 1 Do Begin
    sincos(i * Delta, s, c);
    glvertex3f(middle.x + c * Radius, middle.y + s * Radius, 0);
  End;
  glend;
End;

Procedure RenderAlphaQuad(Top, Left: Single; Image: TGraphikItem);
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderQuad(Top, Left, Image);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

Procedure RenderAlphaQuad(Middle: Tpoint; Width, Height, Angle: Integer; Texture: integer = 0);
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderQuad(Middle, Width, height, angle, texture);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

Procedure RenderAlphaRQuad(TopLeft, BottomRight: TVector2; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0); overload; // Fertig Getestet
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderQuad(Topleft, Bottomright, angle, RotatebyOrigin, texture);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

Procedure RenderAlphaRQuad(TopLeft, BottomRight: TPoint; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0);
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderQuad(Topleft, Bottomright, angle, RotatebyOrigin, texture);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

Procedure RenderQuad(TopLeft, BottomRight: TVector2; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0); overload; // Fertig Getestet
Var
  w2, h2: Single;
Begin
  If RotatebyOrigin Then Begin
    If Texture <> 0 Then
      glBindTexture(gl_texture_2d, Texture);
    glpushmatrix;
    w2 := (BottomRight.x - TopLeft.x) / 2;
    h2 := (BottomRight.y - TopLeft.y) / 2;
    If Angle <> 0 Then
      glRotatef(angle, 0, 0, 1);
    gltranslatef((TopLeft.x + BottomRight.x) / 2, (TopLeft.y + BottomRight.y) / 2, 0);
    glbegin(gl_quads);
    glTexCoord2f(0, 1);
    glvertex3f(-w2, -h2, 0);
    glTexCoord2f(1, 1);
    glvertex3f(w2, -h2, 0);
    glTexCoord2f(1, 0);
    glvertex3f(w2, h2, 0);
    glTexCoord2f(0, 0);
    glvertex3f(-w2, h2, 0);
    glend;
    glpopmatrix;
  End
  Else
    //    RenderQuad(point((TopLeft.x + BottomRight.x) Shr 1, (TopLeft.y + BottomRight.y) Shr 1), BottomRight.x - TopLeft.x, TopLeft.y - BottomRight.y, angle, Texture);
    //    RenderQuad(Fpoint((TopLeft.x + BottomRight.x) / 2, (TopLeft.y + BottomRight.y) / 2), abs(BottomRight.x - TopLeft.x), abs(TopLeft.y - BottomRight.y), angle, Texture);
    RenderQuad(v2((TopLeft.x + BottomRight.x) / 2, (TopLeft.y + BottomRight.y) / 2), abs(BottomRight.x - TopLeft.x), abs(TopLeft.y - BottomRight.y), angle, Texture);
End;

Procedure RenderQuad(TopLeft, BottomRight: TPoint; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0);
Var
  w2, h2: integer;
Begin
  If RotatebyOrigin Then Begin
    If Texture <> 0 Then
      glBindTexture(gl_texture_2d, Texture);
    glpushmatrix;
    w2 := (BottomRight.x - TopLeft.x) Div 2;
    h2 := (BottomRight.y - TopLeft.y) Div 2;
    If Angle <> 0 Then
      glRotatef(angle, 0, 0, 1);
    gltranslatef((TopLeft.x + BottomRight.x) Div 2, (TopLeft.y + BottomRight.y) Div 2, 0);
    glbegin(gl_quads);
    glTexCoord2f(0, 1);
    glvertex3f(-w2, -h2, 0);
    glTexCoord2f(1, 1);
    glvertex3f(w2, -h2, 0);
    glTexCoord2f(1, 0);
    glvertex3f(w2, h2, 0);
    glTexCoord2f(0, 0);
    glvertex3f(-w2, h2, 0);
    glend;
    glpopmatrix;
  End
  Else
    //    RenderQuad(point((TopLeft.x + BottomRight.x) Shr 1, (TopLeft.y + BottomRight.y) Shr 1), BottomRight.x - TopLeft.x, TopLeft.y - BottomRight.y, angle, Texture);
    RenderQuad(point((TopLeft.x + BottomRight.x) Div 2, (TopLeft.y + BottomRight.y) Div 2), abs(BottomRight.x - TopLeft.x), abs(TopLeft.y - BottomRight.y), angle, Texture);
End;

Procedure RenderQuad(Middle: TVector2; Width, Height, Angle: Single; Texture: integer = 0); overload; // Fertig Getestet
Var
  w2, h2: Single;
Begin
  If Texture <> 0 Then
    glBindTexture(gl_texture_2d, Texture);
  glpushmatrix;
  gltranslatef(middle.x, middle.y, 0);
  w2 := Width / 2;
  h2 := height / 2;
  If Angle <> 0 Then
    glRotatef(angle, 0, 0, 1);

  glbegin(gl_quads);
  glTexCoord2f(0, 1);
  glvertex3f(-w2, h2, 0);
  glTexCoord2f(1, 1);
  glvertex3f(w2, h2, 0);
  glTexCoord2f(1, 0);
  glvertex3f(w2, -h2, 0);
  glTexCoord2f(0, 0);
  glvertex3f(-w2, -h2, 0);
  glend;

  // Der Code macht Probleme mit glCullface
  //  glbegin(gl_quads);
  //  glTexCoord2f(0, 1);
  //  glvertex3f(-w2, -h2, 0);
  //  glTexCoord2f(1, 1);
  //  glvertex3f(w2, -h2, 0);
  //  glTexCoord2f(1, 0);
  //  glvertex3f(w2, h2, 0);
  //  glTexCoord2f(0, 0);
  //  glvertex3f(-w2, h2, 0);
  //  glend;
  glpopmatrix;
End;

Procedure RenderQuad(Top, Left: Single; Image: TGraphikItem);
Var
  tw, th: Single;
Begin
  Case Image.Stretched Of
    smClamp: Begin
        tw := Image.OrigWidth / Image.StretchedWidth;
        th := Image.OrigHeight / Image.StretchedHeight;
      End;
    smNone, smStretch, smStretchHard: Begin
        tw := 1;
        th := 1;
      End;
  End;
  glBindTexture(gl_texture_2d, image.Image);
  glpushmatrix;
  gltranslatef(left, top, 0);
  glbegin(gl_quads);
  glTexCoord2f(0, th);
  glvertex3f(0, image.OrigHeight, 0);
  glTexCoord2f(tw, th);
  glvertex3f(image.OrigWidth, image.OrigHeight, 0);
  glTexCoord2f(tw, 0);
  glvertex3f(image.OrigWidth, 0, 0);
  glTexCoord2f(0, 0);
  glvertex3f(0, 0, 0);
  glend;
  glpopmatrix;
End;

Procedure RenderQuad(Middle: TVector2; Angle: Single; Image: TGraphikItem);
Begin
  glPushMatrix;
  gltranslatef(middle.x, middle.y, 0);
  If Angle <> 0 Then
    glRotatef(angle, 0, 0, 1);
  glTranslatef(-Image.OrigWidth / 2, -image.OrigHeight / 2, 0);
  RenderQuad(0, 0, Image);
  glPopMatrix;
End;

Procedure RenderQuad(Middle: Tpoint; Width, Height, Angle: Integer; Texture: integer = 0);
Var
  w2, h2: Integer;
Begin
  If Texture <> 0 Then
    glBindTexture(gl_texture_2d, Texture);
  glpushmatrix;
  gltranslatef(middle.x, middle.y, 0);
  w2 := Width Div 2;
  h2 := height Div 2;
  If Angle <> 0 Then
    glRotatef(angle, 0, 0, 1);
  glbegin(gl_quads);
  glTexCoord2f(0, 1);
  glvertex3f(-w2, -h2, 0);
  glTexCoord2f(1, 1);
  glvertex3f(w2, -h2, 0);
  glTexCoord2f(1, 0);
  glvertex3f(w2, h2, 0);
  glTexCoord2f(0, 0);
  glvertex3f(-w2, h2, 0);
  glend;
  glpopmatrix;
End;

{ TGraphikEngine }

Constructor TOpenGL_GraphikEngine.create;
Begin
  Inherited create;
{$IFDEF DEBUGGOUTPUT}
  OpenGLBufCount := 0;
{$ENDIF}
End;

Destructor TOpenGL_GraphikEngine.destroy;
Begin
  Clear;
End;

Procedure TOpenGL_GraphikEngine.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(Fimages) Do
    If Fimages[i].image <> 0 Then
      glDeleteTextures(1, @Fimages[i].image);
  setlength(Fimages, 0);
{$IFDEF DEBUGGOUTPUT}
  OpenGLBufCount := 0;
{$ENDIF}
End;

Function TOpenGL_GraphikEngine.LoadGraphik(Filename: String; Stretch: TStretchmode): Integer;
Begin
  result := LoadGraphikItem(Filename, Stretch).Image;
End;

Function TOpenGL_GraphikEngine.LoadGraphikItem(Filename: String; Stretch: TStretchmode): TGraphikItem;
Var
  OpenGLData: Array Of Array[0..2] Of Byte;
  Data: String;
  b, b2: Tbitmap;
  IntfImg1: TLazIntfImage;
  CurColor: TFPColor;
  jp: TJPEGImage;
  png: TPortableNetworkGraphic;
  c, j, i: Integer;
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
  ow, oh, nw, nh: INteger;
Begin
  If FileExists(Filename) Then Begin
    Data := LowerCase(Filename);
    // Graphik bereits geladen
    For i := 0 To high(Fimages) Do
      If Fimages[i].Name = Data Then Begin
        result := Fimages[i];
{$IFDEF DEBUGGOUTPUT}
        writeln('TGraphikEngine.LoadGraphik(' + Filename + ')');
        writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
        writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
        exit;
      End;
    // Graphik mus geladen werden
    b := TBitmap.create;
    //{$IFNDEF FPC}
    If lowercase(ExtractFileExt(Filename)) = '.jpg' Then Begin
      jp := TJPEGImage.create;
      jp.LoadFromFile(Filename);
      b.assign(jp);
      jp.free;
    End
    Else If lowercase(ExtractFileExt(Filename)) = '.png' Then Begin
      png := TPortableNetworkGraphic.create;
      png.LoadFromFile(Filename);
      b.assign(png);
      png.free;
    End
    Else Begin
      //{$ELSE}
      //    If lowercase(ExtractFileExt(Filename)) = '.jpg' Then Begin
      //      jp := TJPEGImage.create;
      //      jp.LoadFromFile(Filename);
      //      b.assign(jp);
      //      jp.free;
      //    end;
      //{$ENDIF}
      b.LoadFromFile(Filename);
      //{$IFNDEF FPC}
    End;
    //{$ENDIF}

//    b.PixelFormat := pf24bit;
    // create the raw image
    IntfImg1 := TLazIntfImage.Create(0, 0);
    nw := b.width;
    nh := b.height;
    ow := b.width;
    oh := b.height;
{$IFDEF DEBUGGOUTPUT}
    writeln('TGraphikEngine.LoadGraphik(' + Filename + ')');
    writeln('Orig size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
{$ENDIF}
    Case Stretch Of
      smNone: Begin
        End;
      smStretchHard, smStretch: Begin
          nw := GetNextPowerOfTwo(b.width);
          nh := GetNextPowerOfTwo(b.height);
          If (nw <> b.width) Or (nh <> b.height) Then Begin
            b2 := TBitmap.create;
            b2.PixelFormat := pf24bit;
            b2.width := nw;
            b2.height := nh;
            // b2.canvas.StretchDraw(rect(0, 0, nw, nh), b);
            If Stretch = smStretch Then Begin
              Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imBilinear);
            End
            Else Begin
              Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imNearestNeighbour);
            End;
            b.free;
            b := b2;
          End;
        End;
      smClamp: Begin
          nw := GetNextPowerOfTwo(b.width);
          nh := GetNextPowerOfTwo(b.height);
          If (nw <> b.width) Or (nh <> b.height) Then Begin
            b2 := TBitmap.create;
            b2.PixelFormat := pf24bit;
            b2.width := nw;
            b2.height := nh;
            b2.canvas.Draw(0, 0, b);
            b.free;
            b := b2;
          End;
        End;
    End;
    // load the raw image from the bitmap handles
    IntfImg1.LoadFromBitmap(B.Handle, B.MaskHandle);
{$IFDEF DEBUGGOUTPUT}
    writeln('OpenGL size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
    OpenGLBufCount := OpenGLBufCount + (b.width * b.height * 3);
    writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
{$ENDIF}
    If IsPowerOfTwo(b.width) And IsPowerOfTwo(b.Height) Then Begin
      // Laden der Graphikdaten
      opengldata := Nil;
      setlength(opengldata, b.width * b.height);
      c := 0;
      For j := 0 To b.height - 1 Do Begin
        For i := 0 To b.width - 1 Do Begin
          CurColor := IntfImg1.Colors[i, j];
          OpenGLData[c, 0] := CurColor.Red Div 256;
          OpenGLData[c, 1] := CurColor.green Div 256;
          OpenGLData[c, 2] := CurColor.blue Div 256;
          inc(c);
        End;
      End;
      // Übergeben an OpenGL
      bool := glIsEnabled(GL_TEXTURE_2D);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        glEnable(GL_TEXTURE_2D);
      glGenTextures(1, @Result);
      glBindTexture(GL_TEXTURE_2D, result.Image);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, gl_RGB, b.width, b.height, 0, GL_RGB, GL_UNSIGNED_BYTE, @OpenGLData[0]);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        gldisable(GL_TEXTURE_2D);
      IntfImg1.free;
      b.free;
      // Übernehmen in die Engine
      setlength(Fimages, high(Fimages) + 2);
      Fimages[high(Fimages)].Image := Result.Image;
      Fimages[high(Fimages)].Name := data;
      Fimages[high(Fimages)].Stretched := stretch;
      Fimages[high(Fimages)].OrigWidth := ow;
      Fimages[high(Fimages)].OrigHeight := oh;
      Fimages[high(Fimages)].StretchedWidth := nw;
      Fimages[high(Fimages)].StretchedHeight := nh;
      Fimages[high(Fimages)].IsAlphaImage := false;
      result := Fimages[high(Fimages)];
{$IFDEF DEBUGGOUTPUT}
      writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
    End
    Else
      Raise Exception.create('Error Image ' + extractfilename(Filename) + ' has invalid Width / Height, has to be 2^x.');
  End
  Else Begin
    result.Image := 0;
    Raise Exception.create('Error could not find "' + Filename + '".');
  End;
End;

Function TOpenGL_GraphikEngine.LoadGraphik(Const Graphik: TBitmap; Name: String; Stretch: TStretchmode): Integer; // Laden einer Graphik ohne Alphakanal
Var
  OpenGLData: Array Of Array[0..2] Of Byte;
  Data: String;
  b, b2: Tbitmap;
  IntfImg1: TLazIntfImage;
  CurColor: TFPColor;
  c, j, i: Integer;
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
  ow, oh, nw, nh: Integer;
Begin
  Data := LowerCase(name);
  // Graphik bereits geladen
  For i := 0 To high(Fimages) Do
    If Fimages[i].Name = Data Then Begin
      result := Fimages[i].Image;
{$IFDEF DEBUGGOUTPUT}
      writeln('TGraphikEngine.LoadGraphik(' + name + ')');
      writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
      writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
      exit;
    End;
  // Graphik mus geladen werden
  b := TBitmap.create;
  b.assign(Graphik);
  // create the raw image
  IntfImg1 := TLazIntfImage.Create(0, 0);
  nw := b.width;
  nh := b.height;
  ow := b.width;
  oh := b.height;
{$IFDEF DEBUGGOUTPUT}
  writeln('TGraphikEngine.LoadGraphik(' + name + ')');
  writeln('Orig size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
{$ENDIF}
  Case Stretch Of
    smNone: Begin
      End;
    smStretchHard, smStretch: Begin
        nw := GetNextPowerOfTwo(b.width);
        nh := GetNextPowerOfTwo(b.height);
        If (nw <> b.width) Or (nh <> b.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          // b2.canvas.StretchDraw(rect(0, 0, nw, nh), b);
          If Stretch = smStretch Then Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imBilinear);
          End
          Else Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imNearestNeighbour);
          End;
          b.free;
          b := b2;
        End;
      End;
    smClamp: Begin
        nw := GetNextPowerOfTwo(b.width);
        nh := GetNextPowerOfTwo(b.height);
        If (nw <> b.width) Or (nh <> b.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          b2.canvas.Draw(0, 0, b);
          b.free;
          b := b2;
        End;
      End;
  End;
  // load the raw image from the bitmap handles
  IntfImg1.LoadFromBitmap(B.Handle, B.MaskHandle);
{$IFDEF DEBUGGOUTPUT}
  writeln('OpenGL size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
  OpenGLBufCount := OpenGLBufCount + (b.width * b.height * 3);
  writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
{$ENDIF}
  If IsPowerOfTwo(b.width) And IsPowerOfTwo(b.Height) Then Begin
    // Laden der Graphikdaten
    opengldata := Nil;
    setlength(opengldata, b.width * b.height);
    c := 0;
    For j := 0 To b.height - 1 Do Begin
      For i := 0 To b.width - 1 Do Begin
        CurColor := IntfImg1.Colors[i, j];
        OpenGLData[c, 0] := CurColor.Red Div 256;
        OpenGLData[c, 1] := CurColor.green Div 256;
        OpenGLData[c, 2] := CurColor.blue Div 256;
        inc(c);
      End;
    End;
    // Übergeben an OpenGL
    glGenTextures(1, @Result);
    bool := glIsEnabled(GL_TEXTURE_2D);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, result);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGB, b.width, b.height, 0, GL_RGB, GL_UNSIGNED_BYTE, @OpenGLData[0]);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(GL_TEXTURE_2D);
    IntfImg1.free;
    // Übernehmen in die Engine
    setlength(Fimages, high(Fimages) + 2);
    Fimages[high(Fimages)].Image := Result;
    Fimages[high(Fimages)].Name := data;
    Fimages[high(Fimages)].Stretched := stretch;
    Fimages[high(Fimages)].OrigWidth := ow;
    Fimages[high(Fimages)].OrigHeight := oh;
    Fimages[high(Fimages)].StretchedWidth := nw;
    Fimages[high(Fimages)].StretchedHeight := nh;
    Fimages[high(Fimages)].IsAlphaImage := false;
{$IFDEF DEBUGGOUTPUT}
    writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
  End
  Else
    Raise Exception.create('Error Image ' + extractfilename(name) + ' has invalid Width / Height, has to be 2^x.');
  b.free;
End;

Function TOpenGL_GraphikEngine.GetInfo(Value: String): TGraphikItem;
Var
  i: Integer;
Begin
  Value := LowerCase(Value);
  For i := 0 To high(Fimages) Do
    If pos(value, Fimages[i].Name) <> 0 Then Begin
      result := Fimages[i];
      exit;
    End;
  Raise Exception.create('Error could not find "' + Value + '" in List.');
End;

Function TOpenGL_GraphikEngine.GetInfo(Value: integer): TGraphikItem;
Var
  i: Integer;
Begin
  For i := 0 To high(Fimages) Do
    If Fimages[i].Image = Value Then Begin
      result := Fimages[i];
      exit;
    End;
  Raise Exception.create('Error could not find image "' + inttostr(value) + '" in List.');
End;

Function TOpenGL_GraphikEngine.Find(Value: String; ExceptionOnNotExists: Boolean
  ): integer;
Var
  i: Integer;
Begin
  result := 0;
  Value := LowerCase(Value);
  For i := 0 To high(Fimages) Do Begin
    If pos(value, Fimages[i].Name) <> 0 Then Begin
      result := Fimages[i].Image;
      exit;
    End;
  End;
  If ExceptionOnNotExists Then Begin
    (*
    Will Man Debuggen so ist es manchmal sinnvoller eine Message zu haben, anstatt einer Exception
    *)
    // showmessage('Error Could not Find "' + Value + '" in List.');
    Raise Exception.create('Error Could not Find "' + Value + '" in List.');
  End;
End;

Function TOpenGL_GraphikEngine.FindItem(Value: String;
  ExceptionOnNotExists: Boolean): TGraphikItem;
Var
  i: Integer;
Begin
  result.Image := 0;
  Value := LowerCase(Value);
  For i := 0 To high(Fimages) Do Begin
    If pos(value, Fimages[i].Name) <> 0 Then Begin
      result := Fimages[i];
      exit;
    End;
  End;
  If ExceptionOnNotExists Then Begin
    (*
    Will Man Debuggen so ist es manchmal sinnvoller eine Message zu haben, anstatt einer Exception
    *)
    // showmessage('Error Could not Find "' + Value + '" in List.');
    Raise Exception.create('Error Could not Find "' + Value + '" in List.');
  End;
End;

Function TOpenGL_GraphikEngine.LoadAlphaValueGraphik(Filename: String;
  AlphaValue: byte; Stretch: TStretchmode): Integer;
Var
  OpenGLData: Array Of Array[0..3] Of Byte;
  Data: String;
  b: Tbitmap;
  png: TPortableNetworkGraphic;
  jp: TJPEGImage;
  IntfImg1: TLazIntfImage;
  CurColor: TFPColor;
  c, j, i: Integer;
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  (*
  Die Funktion tut in keinster weise was oben steht das sie tun würde
  Da aber dass was tut in vielen Projekten Reicht ist sie nicht deaktiviert.
  Es sollte einem nur Klar sein dass hier evtl noch deutlich nachgebessert werden muß.
  *)
{$WARNING not Implemented correctly}
  If FileExists(Filename) Then Begin
    Data := LowerCase(Filename);
    // Graphik bereits geladen
    For i := 0 To high(Fimages) Do
      If Fimages[i].Name = Data Then Begin
        result := Fimages[i].Image;
{$IFDEF DEBUGGOUTPUT}
        writeln('TGraphikEngine.LoadAlphaValueGraphik(' + Filename + ')');
        writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
        writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
        exit;
      End;
    // Graphik mus geladen werden
    b := TBitmap.create;
    // create the raw image
    IntfImg1 := TLazIntfImage.Create(0, 0);
    b.PixelFormat := pf24bit;
    If lowercase(ExtractFileExt(Filename)) = '.jpg' Then Begin
      jp := TJPEGImage.create;
      jp.LoadFromFile(Filename);
      b.assign(jp);
      jp.free;
    End
    Else If lowercase(ExtractFileExt(Filename)) = '.png' Then Begin
      png := TPortableNetworkGraphic.create;
      png.LoadFromFile(Filename);
      b.assign(png);
      png.free;
    End
    Else Begin
      b.LoadFromFile(Filename);
    End;
    // load the raw image from the bitmap handles
    IntfImg1.LoadFromBitmap(B.Handle, B.MaskHandle);
{$IFDEF DEBUGGOUTPUT}
    writeln('TGraphikEngine.LoadAlphaValueGraphik(' + Filename + ')');
    writeln('Orig size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
{$ENDIF}
    (* Hier Fehlt der Stretch Teil*)
{$IFDEF DEBUGGOUTPUT}
    writeln('OpenGL size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
    OpenGLBufCount := OpenGLBufCount + (b.width * b.height * 4);
    writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
{$ENDIF}
    If IsPowerOfTwo(b.width) And IsPowerOfTwo(b.Height) Then Begin
      // Laden der Graphikdaten
      opengldata := Nil;
      setlength(opengldata, b.width * b.height);
      c := 0;
      For j := 0 To b.height - 1 Do Begin
        For i := 0 To b.width - 1 Do Begin
          CurColor := IntfImg1.Colors[i, j];
          OpenGLData[c, 0] := CurColor.Red;
          OpenGLData[c, 1] := CurColor.green;
          OpenGLData[c, 2] := CurColor.blue;
          OpenGLData[c, 3] := AlphaValue;
          inc(c);
        End;
      End;
      // Übergeben an OpenGL
      glGenTextures(1, @Result);
      bool := glIsEnabled(GL_TEXTURE_2D);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, result);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, b.width, b.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0]);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        gldisable(GL_TEXTURE_2D);
      IntfImg1.free;
      b.free;
      // Übernehmen in die Engine
      setlength(Fimages, high(Fimages) + 2);
      Fimages[high(Fimages)].Image := Result;
      Fimages[high(Fimages)].Name := data;
      Fimages[high(Fimages)].IsAlphaImage := true;
      // TODO: Hier fehlen einige felder ..
{$IFDEF DEBUGGOUTPUT}
      writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
    End
    Else
      Raise Exception.create('Error Image ' + extractfilename(Filename) + ' has invalid Width / Height, has to be 2^x.');
  End
  Else Begin
    Raise Exception.create('Error Image ' + extractfilename(Filename) + ' not found.');
    result := 0;
  End;
End;

Function TOpenGL_GraphikEngine.LoadAlphaColorGraphik(Const Graphik: TBitmap;
  Name: String; Color: TRGB; Stretch: TStretchmode): Integer; // Lädt eine Alphagraphik und setzt den Wert von Color = Transparent.
Var
  OpenGLData: Array Of Array[0..3] Of Byte;
  Data: String;
  b2, b: Tbitmap;
  IntfImg1: TLazIntfImage;
  CurColor: TFPColor;
  ow, oh, nw, nh, c, j, i: Integer;
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  Data := LowerCase(name);
  // Graphik bereits geladen
  For i := 0 To high(Fimages) Do
    If Fimages[i].Name = Data Then Begin
      result := Fimages[i].Image;
{$IFDEF DEBUGGOUTPUT}
      writeln('TGraphikEngine.LoadAlphaColorgraphik(' + name + ')');
      writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
      writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
      exit;
    End;
  // Graphik mus geladen werden
  b := Tbitmap.create;
  b.assign(Graphik);
  // create the raw image
  IntfImg1 := TLazIntfImage.Create(0, 0);
  // b.PixelFormat := pf24bit;
  ow := b.width;
  oh := b.height;
  nw := b.width;
  nh := b.height;
{$IFDEF DEBUGGOUTPUT}
  writeln('TGraphikEngine.LoadAlphaColorgraphik(' + name + ')');
  writeln('Orig size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
{$ENDIF}
  Case Stretch Of
    smNone: Begin
      End;
    smStretchHard, smStretch: Begin
        nw := GetNextPowerOfTwo(b.width);
        nh := GetNextPowerOfTwo(b.height);
        If (nw <> b.width) Or (nh <> b.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          If Stretch = smStretch Then Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imBilinear);
          End
          Else Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imNearestNeighbour);
          End;
          b.free;
          b := b2;
        End;
      End;
    smClamp: Begin
        nw := GetNextPowerOfTwo(b.width);
        nh := GetNextPowerOfTwo(b.height);
        If (nw <> b.width) Or (nh <> b.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          b2.canvas.Draw(0, 0, b);
          b.free;
          b := b2;
        End;
      End;
  End;
  // load the raw image from the bitmap handles
  IntfImg1.LoadFromBitmap(B.Handle, B.MaskHandle);
{$IFDEF DEBUGGOUTPUT}
  writeln('OpenGL size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
  OpenGLBufCount := OpenGLBufCount + (b.width * b.height * 4);
  writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
{$ENDIF}
  If IsPowerOfTwo(b.width) And IsPowerOfTwo(b.Height) Then Begin
    // Laden der Graphikdaten
    opengldata := Nil;
    setlength(opengldata, b.width * b.height);
    c := 0;
    For j := 0 To b.height - 1 Do Begin
      For i := 0 To b.width - 1 Do Begin
        CurColor := IntfImg1.Colors[i, j];
        OpenGLData[c, 0] := CurColor.Red Div 256;
        OpenGLData[c, 1] := CurColor.green Div 256;
        OpenGLData[c, 2] := CurColor.blue Div 256;
        If (OpenGLData[c, 0] = Color.r) And
          (OpenGLData[c, 1] = Color.g) And
          (OpenGLData[c, 2] = Color.b) Then
          OpenGLData[c, 3] := 255
        Else
          OpenGLData[c, 3] := 0;
        inc(c);
      End;
    End;
    // Übergeben an OpenGL
    glGenTextures(1, @Result);
    bool := glIsEnabled(GL_TEXTURE_2D);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, result);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, b.width, b.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0]);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(GL_TEXTURE_2D);
    IntfImg1.free;
    // Übernehmen in die Engine
    setlength(Fimages, high(Fimages) + 2);
    Fimages[high(Fimages)].Image := Result;
    Fimages[high(Fimages)].Name := data;
    Fimages[high(Fimages)].Stretched := stretch;
    Fimages[high(Fimages)].OrigHeight := oh;
    Fimages[high(Fimages)].OrigWidth := ow;
    Fimages[high(Fimages)].StretchedHeight := nh;
    Fimages[high(Fimages)].StretchedWidth := nw;
    Fimages[high(Fimages)].IsAlphaImage := true;
{$IFDEF DEBUGGOUTPUT}
    writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
  End
  Else Begin
    b.free;
    Raise Exception.create('Error Image ' + extractfilename(name) + ' has invalid Width / Height, has to be 2^x.');
  End;
  b.free;
End;

Function TOpenGL_GraphikEngine.LoadAlphaColorGraphik(Filename: String; Color: TRGB; Stretch: TStretchmode): Integer; // Lädt eine Alphagraphik und setzt den Wert von Color = Transparent.
Var
  Data: String;
  b: TBitmap;
  jp: TJPEGImage;
  png: TPortableNetworkGraphic;
  i: Integer;
Begin
  result := 0;
  If Not FileExists(Filename) Then exit;
  Data := LowerCase(Filename);
  // Graphik bereits geladen
  For i := 0 To high(Fimages) Do
    If Fimages[i].Name = Data Then Begin
      result := Fimages[i].Image;
{$IFDEF DEBUGGOUTPUT}
      writeln('TGraphikEngine.LoadAlphaColorgraphik(' + Filename + ')');
      writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
      writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
      exit;
    End;
  // Graphik mus geladen werden
  b := TBitmap.create;
  // create the raw image
  // Graphik mus geladen werden
  If lowercase(ExtractFileExt(Filename)) = '.jpg' Then Begin
    jp := TJPEGImage.create;
    jp.LoadFromFile(Filename);
    b.assign(jp);
    jp.free;
  End
  Else If lowercase(ExtractFileExt(Filename)) = '.png' Then Begin
    png := TPortableNetworkGraphic.create;
    png.LoadFromFile(Filename);
    b.assign(png);
    png.free;
  End
  Else Begin
    b.LoadFromFile(Filename);
  End;
  result := LoadAlphaColorGraphik(b, Filename, Color, Stretch);
  b.free;
End;

Function TOpenGL_GraphikEngine.LoadAlphaGraphik(Filename: String; Stretch: TStretchmode): Integer;
Begin
  result := LoadAlphaGraphikItem(Filename, Stretch).Image;
End;

Function TOpenGL_GraphikEngine.LoadAlphaGraphikItem(Filename: String; Stretch: TStretchmode): TGraphikItem;
Var
  OpenGLData: Array Of Array[0..3] Of Byte;
  Data: String;
  b2, b: Tbitmap;
  jp: TJPEGImage;
  png: TPortableNetworkGraphic;
  nw, nh, ow, oh: INteger;
  IntfImg1: TLazIntfImage;
  CurColor: TFPColor;
  c, j, i: Integer;
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  (*
  Die Funktion tut in keinster weise was oben steht das sie tun würde
  Da aber dass was tut in vielen Projekten Reicht ist sie nicht deaktiviert.
  Es sollte einem nur Klar sein dass hier evtl noch deutlich nachgebessert werden muß.
  *)
{$WARNING not Implemented correctly}
  If FileExists(Filename) Then Begin
    Data := LowerCase(Filename);
    // Graphik bereits geladen
    For i := 0 To high(Fimages) Do
      If Fimages[i].Name = Data Then Begin
        result := Fimages[i];
{$IFDEF DEBUGGOUTPUT}
        writeln('TGraphikEngine.LoadAlphaGraphik(' + Filename + ')');
        writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
        writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
        exit;
      End;
    // Graphik mus geladen werden
    b := TBitmap.create;
    If lowercase(ExtractFileExt(Filename)) = '.jpg' Then Begin
      jp := TJPEGImage.create;
      jp.LoadFromFile(Filename);
      b.assign(jp);
      jp.free;
    End
    Else If lowercase(ExtractFileExt(Filename)) = '.png' Then Begin
      png := TPortableNetworkGraphic.create;
      png.LoadFromFile(Filename);
      png.TransparentColor := clBlack;
      png.Transparent := false;
      b.assign(png);
      png.free;
    End
    Else Begin
      //{$ELSE}
      //    If lowercase(ExtractFileExt(Filename)) = '.jpg' Then Begin
      //      jp := TJPEGImage.create;
      //      jp.LoadFromFile(Filename);
      //      b.assign(jp);
      //      jp.free;
      //    end;
      //{$ENDIF}
      b.LoadFromFile(Filename);
      //{$IFNDEF FPC}
    End;
    // create the raw image
    IntfImg1 := TLazIntfImage.Create(0, 0);
    // b.PixelFormat := pf24bit;
    nw := b.width;
    nh := b.height;
    ow := b.width;
    oh := b.height;
{$IFDEF DEBUGGOUTPUT}
    writeln('TGraphikEngine.LoadAlphaGraphik(' + Filename + ')');
    writeln('Orig size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
{$ENDIF}
    Case Stretch Of
      smNone: Begin
        End;
      smStretch, smStretchHard: Begin
          nw := GetNextPowerOfTwo(b.width);
          nh := GetNextPowerOfTwo(b.height);
          If (nw <> b.width) Or (nh <> b.height) Then Begin
            b2 := TBitmap.create;
            b2.PixelFormat := pf24bit;
            b2.width := nw;
            b2.height := nh;
            // b2.canvas.StretchDraw(rect(0, 0, nw, nh), b);
            If Stretch = smStretch Then Begin
              Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imBilinear);
            End
            Else Begin
              Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imNearestNeighbour);
            End;
            b.free;
            b := b2;
          End;
        End;
      smClamp: Begin
          nw := GetNextPowerOfTwo(b.width);
          nh := GetNextPowerOfTwo(b.height);
          If (nw <> b.width) Or (nh <> b.height) Then Begin
            b2 := TBitmap.create;
            b2.PixelFormat := pf24bit;
            b2.width := nw;
            b2.height := nh;
            b2.canvas.Draw(0, 0, b);
            b.free;
            b := b2;
          End;
        End;
    End;
    // load the raw image from the bitmap handles
    IntfImg1.LoadFromBitmap(B.Handle, B.MaskHandle);
{$IFDEF DEBUGGOUTPUT}
    writeln('OpenGL size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
    OpenGLBufCount := OpenGLBufCount + (b.width * b.height * 4);
    writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
{$ENDIF}
    If IsPowerOfTwo(b.width) And IsPowerOfTwo(b.Height) Then Begin
      // Laden der Graphikdaten
      opengldata := Nil;
      setlength(opengldata, b.width * b.height);
      c := 0;
      For j := 0 To b.height - 1 Do Begin
        For i := 0 To b.width - 1 Do Begin
          CurColor := IntfImg1.Colors[i, j];
          OpenGLData[c, 0] := CurColor.Red Div 256;
          OpenGLData[c, 1] := CurColor.green Div 256;
          OpenGLData[c, 2] := CurColor.blue Div 256;
          If (OpenGLData[c, 0] = Fuchsia.r) And
            (OpenGLData[c, 1] = Fuchsia.g) And
            (OpenGLData[c, 2] = Fuchsia.b) Then
            OpenGLData[c, 3] := 255
          Else
            OpenGLData[c, 3] := 0;
          inc(c);
        End;
      End;
      // Übergeben an OpenGL
      glGenTextures(1, @Result);
      bool := glIsEnabled(GL_TEXTURE_2D);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, result.Image);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, b.width, b.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0]);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        gldisable(GL_TEXTURE_2D);
      IntfImg1.free;
      b.free;
      // Übernehmen in die Engine
      setlength(Fimages, high(Fimages) + 2);
      Fimages[high(Fimages)].Image := Result.Image;
      Fimages[high(Fimages)].Name := data;
      Fimages[high(Fimages)].OrigWidth := ow;
      Fimages[high(Fimages)].OrigHeight := oh;
      Fimages[high(Fimages)].StretchedWidth := nw;
      Fimages[high(Fimages)].StretchedHeight := nh;
      Fimages[high(Fimages)].Stretched := Stretch;
      Fimages[high(Fimages)].IsAlphaImage := true;
      result := Fimages[high(Fimages)];
{$IFDEF DEBUGGOUTPUT}
      writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
    End
    Else
      Raise Exception.create('Error Image ' + extractfilename(Filename) + ' has invalid Width / Height, has to be 2^x.');
  End
  Else
    result.Image := 0;
End;

Function TOpenGL_GraphikEngine.LoadAlphaGraphik(Const Graphik, AlphaMask: Tbitmap; Name: String; Stretch: TStretchmode): integer; // Lädt eine Graphik aus TBitmap, und Lädt den Alphakanal aus den Luminanzdaten von Alphamask, Name dient zum späteren Wiederfinden
Var
  g, a: TBitmap;
  OpenGLData: Array Of Array[0..3] Of Byte;
  Graphik_intf, Alpha_intf: TLazIntfImage;
  CurColor: TFPColor;
  c, j, i: Integer;
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
  b2: Tbitmap;
  ow, oh, nw, nh: integer;
Begin
  (*
  Die Funktion tut in keinster weise was oben steht das sie tun würde
  Da aber dass was tut in vielen Projekten Reicht ist sie nicht deaktiviert.
  Es sollte einem nur Klar sein dass hier evtl noch deutlich nachgebessert werden muß.
  *)
{$WARNING not Implemented correctly}
  // Suchen falls die Graphik Schon mal geladen wurde
  i := Find(Name, false);
  If i <> 0 Then Begin
    result := i;
{$IFDEF DEBUGGOUTPUT}
    writeln('TGraphikEngine.LoadAlphaGraphik(' + name + ')');
    writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
    writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
    exit;
  End;
  g := TBitmap.Create;
  g.Assign(Graphik);
  a := TBitmap.Create;
  a.Assign(AlphaMask);
  // nebenbedingungen Abprüfen
  If (g.width <> a.width) Or
    (g.height <> a.height) Then Begin
    g.free;
    a.free;
    Raise exception.create('Error graphik and alphaMask have not the same size !');
  End;
  ow := g.width;
  oh := g.height;
  nw := g.width;
  nh := g.height;
{$IFDEF DEBUGGOUTPUT}
  writeln('TGraphikEngine.LoadAlphaGraphik(' + name + ')');
  writeln('Orig size : ' + inttostr(g.width) + 'x' + inttostr(g.height));
{$ENDIF}
  Case Stretch Of
    smNone: Begin
      End;
    smStretchHard, smStretch: Begin
        nw := GetNextPowerOfTwo(g.width);
        nh := GetNextPowerOfTwo(g.height);
        If (nw <> g.width) Or (nh <> g.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          // b2.canvas.StretchDraw(rect(0, 0, nw, nh), g);
          If Stretch = smStretch Then Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), g, imBilinear);
          End
          Else Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), g, imNearestNeighbour);
          End;
          g.free;
          g := b2;
        End;
        If (nw <> a.width) Or (nh <> a.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          If Stretch = smStretch Then Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), a, imBilinear);
          End
          Else Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), a, imNearestNeighbour);
          End;
          a.free;
          a := b2;
        End;
      End;
    smClamp: Begin
        nw := GetNextPowerOfTwo(g.width);
        nh := GetNextPowerOfTwo(g.height);
        If (nw <> g.width) Or (nh <> g.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          b2.canvas.Draw(0, 0, g);
          g.free;
          g := b2;
        End;
        If (nw <> a.width) Or (nh <> a.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          b2.canvas.Draw(0, 0, a);
          a.free;
          a := b2;
        End;
      End;
  End;
{$IFDEF DEBUGGOUTPUT}
  writeln('OpenGL size : ' + inttostr(g.width) + 'x' + inttostr(g.height));
  OpenGLBufCount := OpenGLBufCount + (g.width * g.height * 4);
  writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
{$ENDIF}
  If IsPowerOfTwo(g.width) And IsPowerOfTwo(g.Height) Then Begin
    //    g.pixelformat := pf24bit;
    //    a.pixelformat := pf24bit;
        // create the raw image
    Graphik_intf := TLazIntfImage.Create(0, 0);
    Alpha_intf := TLazIntfImage.Create(0, 0);

    Graphik_intf.LoadFromBitmap(g.Handle, g.MaskHandle);
    Alpha_intf.LoadFromBitmap(a.Handle, a.MaskHandle);
    // Laden der Graphikdaten
    opengldata := Nil;
    setlength(opengldata, g.width * g.height);
    c := 0;
    For j := 0 To g.height - 1 Do Begin
      For i := 0 To g.width - 1 Do Begin
        CurColor := Graphik_intf.Colors[i, j];
        OpenGLData[c, 0] := CurColor.Red Div 256;
        OpenGLData[c, 1] := CurColor.green Div 256;
        OpenGLData[c, 2] := CurColor.blue Div 256;

        CurColor := Alpha_intf.Colors[i, j];
        OpenGLData[c, 3] := FPColortoLuminanz(CurColor);
        inc(c);
      End;
    End;
    // Übergeben an OpenGL
    glGenTextures(1, @Result);
    bool := glIsEnabled(GL_TEXTURE_2D);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, result);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, g.width, g.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0]);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(GL_TEXTURE_2D);
    // entladen des texturspeichers
    If BOOL{$IFDEF USE_GL} = 1{$ENDIF} Then
      GLBindtexture(GL_texture_2d, 0);
    // Übernehmen in die Engine
    Graphik_intf.free;
    Alpha_intf.free;
    setlength(Fimages, high(Fimages) + 2);
    Fimages[high(Fimages)].Image := Result;
    Fimages[high(Fimages)].Name := lowercase(Name);
    Fimages[high(Fimages)].Stretched := Stretch;
    Fimages[high(Fimages)].OrigHeight := oh;
    Fimages[high(Fimages)].OrigWidth := ow;
    Fimages[high(Fimages)].StretchedHeight := nh;
    Fimages[high(Fimages)].StretchedWidth := nw;
    Fimages[high(Fimages)].IsAlphaImage := true;
{$IFDEF DEBUGGOUTPUT}
    writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
  End
  Else
    Raise Exception.create('Error Image ' + extractfilename(name) + ' has invalid Width / Height, has to be 2^x.');
  g.free;
  a.free;
End;

Function TOpenGL_GraphikEngine.LoadAlphaGraphik(Graphik, AlphaMask: String;
  Stretch: TStretchmode): integer;
Begin
  result := LoadAlphaGraphikItem(Graphik, AlphaMask, Stretch).Image;
End;

Function TOpenGL_GraphikEngine.LoadAlphaGraphikItem(Graphik, AlphaMask: String;
  Stretch: TStretchmode): TGraphikItem;
Var
  b2, b, a: TBitmap;
  p: TPortableNetworkGraphic;
  jp: TJPEGImage;
  nw, nh, ow, oh: Integer;
  IntfImg1: TLazIntfImage;
  IntfImg2: TLazIntfImage;
  CurColor: TFPColor;
  OpenGLData: Array Of Array[0..3] Of Byte;
  c, j, i: Integer;
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  (*
  Die Funktion tut in keinster weise, was oben steht das sie tun würde.
  Da aber dass was tut in vielen Projekten Reicht ist sie nicht deaktiviert.
  Es sollte einem nur Klar sein dass hier evtl noch deutlich nachgebessert werden muß.
  *)
{$WARNING not Implemented correctly}
  // Suchen falls die Graphik Schon mal geladen wurde
  i := Find(Graphik, false);
  If i <> 0 Then Begin
    result := FImages[i];
{$IFDEF DEBUGGOUTPUT}
    writeln('TGraphikEngine.LoadAlphaGraphik(' + Graphik + ')');
    writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
    writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
    exit;
  End;
  If Not FileExists(graphik) Then
    Raise Exception.create('Error Image "' + Graphik + '" could not be found.');
  If Not FileExists(AlphaMask) Then
    Raise Exception.create('Error Image "' + AlphaMask + '" could not be found.');
  b := TBitmap.create;
  a := TBitmap.create;
  If lowercase(ExtractFileExt(Graphik)) = '.jpg' Then Begin
    jp := TJPEGImage.create;
    jp.LoadFromFile(Graphik);
    b.assign(jp);
    jp.free;
  End
  Else If lowercase(ExtractFileExt(Graphik)) = '.png' Then Begin
    p := TPortableNetworkGraphic.create;
    p.LoadFromFile(Graphik);
    b.assign(p);
    p.free;
  End
  Else Begin
    b.LoadFromFile(Graphik);
  End;
  // b.PixelFormat := pf24bit;
  If lowercase(ExtractFileExt(AlphaMask)) = '.jpg' Then Begin
    jp := TJPEGImage.create;
    jp.LoadFromFile(AlphaMask);
    a.assign(jp);
    jp.free;
  End
  Else If lowercase(ExtractFileExt(AlphaMask)) = '.png' Then Begin
    p := TPortableNetworkGraphic.create;
    p.LoadFromFile(AlphaMask);
    a.assign(p);
    p.free;
  End
  Else Begin
    a.LoadFromFile(AlphaMask);
  End;
  // a.PixelFormat := pf24bit;
  // create the raw image
  IntfImg1 := TLazIntfImage.Create(0, 0);
  IntfImg2 := TLazIntfImage.Create(0, 0);
  nw := b.width;
  nh := b.height;
  ow := b.width;
  oh := b.height;
{$IFDEF DEBUGGOUTPUT}
  writeln('TGraphikEngine.LoadAlphaGraphik(' + Graphik + ')');
  writeln('Orig size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
{$ENDIF}
  Case Stretch Of
    smNone: Begin
      End;
    smStretchHard, smStretch: Begin
        nw := GetNextPowerOfTwo(b.width);
        nh := GetNextPowerOfTwo(b.height);
        If (nw <> b.width) Or (nh <> b.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          // b2.canvas.StretchDraw(rect(0, 0, nw, nh), b);
          If Stretch = smStretch Then Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imBilinear);
          End
          Else Begin
            Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imNearestNeighbour);
          End;
          b.free;
          b := b2;
        End;
        If (nw <> a.width) Or (nh <> a.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          b2.canvas.StretchDraw(rect(0, 0, nw, nh), a);
          a.free;
          a := b2;
        End;
      End;
    smClamp: Begin
        nw := GetNextPowerOfTwo(b.width);
        nh := GetNextPowerOfTwo(b.height);
        If (nw <> b.width) Or (nh <> b.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          b2.canvas.Draw(0, 0, b);
          b.free;
          b := b2;
        End;
        If (nw <> a.width) Or (nh <> a.height) Then Begin
          b2 := TBitmap.create;
          b2.PixelFormat := pf24bit;
          b2.width := nw;
          b2.height := nh;
          b2.canvas.Draw(0, 0, a);
          a.free;
          a := b2;
        End;
      End;
  End;
  // load the raw image from the bitmap handles
  IntfImg1.LoadFromBitmap(B.Handle, B.MaskHandle);
  IntfImg2.LoadFromBitmap(a.Handle, a.MaskHandle);
{$IFDEF DEBUGGOUTPUT}
  writeln('OpenGL size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
  OpenGLBufCount := OpenGLBufCount + (b.width * b.height * 4);
  writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
{$ENDIF}
  If IsPowerOfTwo(b.width) And IsPowerOfTwo(b.Height) Then Begin
    // Laden der Graphikdaten
    opengldata := Nil;
    setlength(opengldata, b.width * b.height);
    c := 0;
    For j := 0 To b.height - 1 Do Begin
      For i := 0 To b.width - 1 Do Begin
        CurColor := IntfImg1.Colors[i, j];
        OpenGLData[c, 0] := CurColor.Red Div 256;
        OpenGLData[c, 1] := CurColor.green Div 256;
        OpenGLData[c, 2] := CurColor.blue Div 256;
        OpenGLData[c, 3] := FPColortoLuminanz(IntfImg2.Colors[i, j]);
        inc(c);
      End;
    End;
    // Übergeben an OpenGL
    glGenTextures(1, @Result);
    bool := glIsEnabled(GL_TEXTURE_2D);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, result.Image);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, b.width, b.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0, 0]);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(GL_TEXTURE_2D);
    // Übernehmen in die Engine
    setlength(Fimages, high(Fimages) + 2);
    Fimages[high(Fimages)].Image := Result.Image;
    Fimages[high(Fimages)].Name := graphik;
    Fimages[high(Fimages)].Stretched := stretch;
    Fimages[high(Fimages)].OrigWidth := ow;
    Fimages[high(Fimages)].OrigHeight := oh;
    Fimages[high(Fimages)].StretchedWidth := nw;
    Fimages[high(Fimages)].StretchedHeight := nh;
    Fimages[high(Fimages)].IsAlphaImage := true;
    result := Fimages[high(Fimages)];
{$IFDEF DEBUGGOUTPUT}
    writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
  End
  Else Begin
    Raise Exception.create('Error Image ' + extractfilename(graphik) + ' has invalid Width / Height, has to be 2^x.');
  End;
  IntfImg2.free;
  IntfImg1.free;
  b.free;
  a.free;
End;

Function TOpenGL_GraphikEngine.LoadAlphaPNGGraphik(Graphik: String;
  Stretch: TStretchmode): integer;
Var
  CurColor, AlphaColor: TFPColor;
  i, j: Integer;
  a, b: TBitmap;
  p: TPortableNetworkGraphic;
  IntfImg1: TLazIntfImage;
  IntfImg2: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  // Suchen falls die Graphik Schon mal geladen wurde
  i := Find(Graphik, false);
  If i <> 0 Then Begin
    result := i;
{$IFDEF DEBUGGOUTPUT}
    writeln('TGraphikEngine.LoadAlphaGraphik(' + Graphik + ')');
    writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
    writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
    exit;
  End;
  If Not FileExists(graphik) Then
    Raise Exception.create('Error Image "' + Graphik + '" could not be found.');
  a := TBitmap.create;
  b := TBitmap.create;
  If lowercase(ExtractFileExt(Graphik)) = '.png' Then Begin
    p := TPortableNetworkGraphic.create;
    p.LoadFromFile(Graphik);
    b.assign(p);
    p.free;
  End
  Else Begin
    Raise exception.create('Error Image "' + Graphik + '" is not a png.');
  End;
  a.Width := b.Width;
  a.Height := b.Height;
  IntfImg1 := TLazIntfImage.Create(0, 0);
  IntfImg1.LoadFromBitmap(B.Handle, B.MaskHandle);
  IntfImg2 := TLazIntfImage.Create(0, 0);
  IntfImg2.LoadFromBitmap(a.Handle, a.MaskHandle);
  For j := 0 To b.height - 1 Do Begin
    For i := 0 To b.width - 1 Do Begin
      (*
       * So Drehen dass Schwarz = Transparent
       *                Weiß = Opak
       *)
      CurColor := IntfImg1.Colors[i, j];
      AlphaColor.red := (255 - CurColor.alpha Div 256) * 256;
      AlphaColor.green := (255 - CurColor.alpha Div 256) * 256;
      AlphaColor.blue := (255 - CurColor.alpha Div 256) * 256;
      AlphaColor.alpha := 255 * 256;
      IntfImg2.Colors[i, j] := AlphaColor;
    End;
  End;
  IntfImg2.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  a.Handle := ImgHandle;
  a.MaskHandle := ImgMaskHandle;
  //a.SaveToFile(Graphik + 'alpha.bmp');
  IntfImg2.free;
  IntfImg1.free;
  result := LoadAlphaGraphik(b, a, Graphik, Stretch);
  a.free;
  b.free;
End;

Function TOpenGL_GraphikEngine.RemoveGraphik(Value: TGraphikItem): Boolean;
Begin
  result := RemoveGraphik(Value.Image);
End;

Function TOpenGL_GraphikEngine.RemoveGraphik(Value: integer): Boolean;
Var
  i, j: Integer;
Begin
  result := false;
  For i := 0 To high(FImages) Do
    If FImages[i].Image = Value Then Begin
      result := true;
      glDeleteTextures(1, @value);
      For j := i To high(FImages) - 1 Do Begin
        FImages[j] := FImages[j + 1];
      End;
      setlength(FImages, high(FImages));
      exit;
    End;
End;

Initialization

  OpenGL_GraphikEngine := TOpenGL_GraphikEngine.create;

Finalization

  If assigned(OpenGL_GraphikEngine) Then Begin
    OpenGL_GraphikEngine.free;
    OpenGL_GraphikEngine := Nil;
  End;

End.

