(******************************************************************************)
(* uOpenGL_ASCII_Font.pas                                          ??.??.???? *)
(*                                                                            *)
(* Version     : 0.05                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description :                                                              *)
(*        Diese Unit, stellt eine einfach zu nutztende Schrift für OpenGL zur *)
(*        Verfügung. Der Schriftsatz beruht auf dem Original DOS Schriftsatz  *)
(*        in der Größe 8x12.                                                  *)
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
(*               0.02 - Added Billboarded/ 3D-Mode rendering                  *)
(*               0.03 - Disable Lighting on Textout                           *)
(*               0.04 - Suppurt UTF8 Strings                                  *)
(*               0.05 - CurrentColor nicht nach Außen ändern                  *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(* Benutzung : in OpenGLControl1MakeCurrent oder später einmalig              *)
(*             Create_ASCII_Font aufrufen. Danach steht                       *)
(*             OpenGL_ASCII_Font zur Verfügung.                               *)
(*                                                                            *)
(* Soll die Anwendung in 2D immer mit der Selben Auflösung gerendert werden,  *)
(* dann muss im OnResizeEvent folgender Code ( für z.B. 640x480) hinzugefügt  *)
(* werden :                                                                   *)
(*                                                                            *)
(* glPointSize(max(OpenGLControl1.Width / 640, OpenGLControl1.Height / 480)); *)
(*  !! Gilt seit ver 0.02 nicht mehr !!                                       *)
(*                                                                            *)
(******************************************************************************)
Unit uOpenGL_ASCII_Font;

{$MODE ObjFPC}{$H+}

Interface

Uses
  IntfGraphics, fpImage, Graphics, LCLType, LResources, classes,
  dglopengl, // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  uvectormath, // http://corpsman.de/index.php?doc=opengl/opengl_graphikengine
  LConvEncoding,
  math;

Type

  { TOpenGL_ASCII_Font }

  TOpenGL_ASCII_Font = Class
  private
    FBaseList: gluint; // Basispointer, für die OpenGL List
    FCharcount: integer; // Anzahl der Zeichen, sollte immer 256 sein !!
    fColor: TVector3; // Farbe der Schrift
    fsize: Single; // Größe der Schrift in Pixeln ( Höhe )
    fCharheight: integer; // "Echte" Größe eines Buchstabens in Pixeln
    fCharwidth: integer; // "Echte" Größe eines Buchstabens in Pixeln
    Function getcolor: TColor;
    Function getcolorfp: TFPColor;
    Function getcolorV3: TVector3;
    Function getsize: Single;
    Procedure RenderChar(Number: integer); // Zeichnet ein Zeichen
    Procedure setColor(AValue: TColor);
    Procedure setColorV3(AValue: TVector3);
    Procedure setColorfp(AValue: TFPColor);
    Procedure setsize(AValue: Single);
  public
    (*
     * Farbe, Größe
     *)
    Property Color: TColor read getcolor write setColor; // Zugriff auf die Schriftfarbe
    Property ColorV3: TVector3 read getcolorV3 write setColorV3; // Zugriff auf die Schriftfarbe
    Property Colorfp: TFPColor read getcolorfp write setColorfp; // Zugriff auf die Schriftfarbe
    Property Size: Single read getsize write setsize; // Höhe der Schrift in Pixeln ( Gillt nur für Textout im 3D-Mode wird die Höhe explizit gesetzt)
    (*
     * Die Init und Free Routinen brauchen nicht direkt aufgerufen zu werden. Hier
     * Genügt ein Aufruf von : Create_ASCII_Font
     * Freigeben tut sich die Klasse selbständig beim Beenden der Anwendung
     *)
    Constructor Create(Const Bitmap: Tbitmap; CharWidth, CharHeight, CharCount: Integer); // Alles was Weiß ist, ist sichtbar, der Rest Transparent
    Destructor destroy; override;
    (*
     * Text Dimensionen
     *)
    Function TextWidth(Text: String): single; // Breite des Textes in Pixeln  ACHTUNG, Als CRT gillt nur #13 !!
    Function TextHeight(text: String): single; // Höhe des Textes in Pixeln   ACHTUNG, Als CRT gillt nur #13 !!
    Function TextWidth3D(Height: TBaseType; Text: String): TBaseType; // Im 3D-Mode Gibt man die Höhe des gesammten Textes vor, aus dieser Vorgabe lässt sich dann die Breite Ableiten
    (*
     * Zeichen Routinen
     *)
    Procedure Textout(x, y: Integer; Text: String); // Zeichnet einen Text, unter Berücksichtigung von CRT's ( Vorher muss Go2d aufgerufen werden )
    Procedure BillboardTextout(Position: TVector3; Height: TBaseType; Text: String); // Rendert einen Schriftzug ( zentriert auf Position als Billboard ), da hier 3D-Koordinaten gelten, muss dem Text eine Höhe im 3D Skaling gegeben werden.
    Procedure ThreeDTextout(Position, Up, Right: TVector3; Height: TBaseType; Text: String); // Rendert eine  Schriftzug Zentriert auf Position und entsprechend Up und Right, da hier 3D-Koordinaten gelten, muss dem Text eine Höhe im 3D Skaling gegeben werden.
    Procedure RenderTextToRect(rect: TRect; Text: String); // Rendert einen Text So in Rect, dass er Maximal "gestretched" rein pass (ohne Umbrüche, oder so, nur durch Skallierungen)
  End;

Var
  OpenGL_ASCII_Font: TOpenGL_ASCII_Font = Nil;

Procedure Create_ASCII_Font(); // Muss in Make Current aufgerufen werden, da es OpenGLBefehle nutzt.

// Switcher zwischen 2D und 3D-Modus
Procedure Go2d(Width_2D, Height_2d: Integer);
Procedure Exit2d();

Implementation

Procedure Go2d(Width_2D, Height_2d: Integer);
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  glOrtho(0, Width_2D, Height_2d, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

Procedure Create_ASCII_Font();
Var
  bitmap: TBitmap;
Begin
  If Assigned(OpenGL_ASCII_Font) Then OpenGL_ASCII_Font.free;
  Bitmap := TBitmap.Create;
  bitmap.LoadFromLazarusResource('OpenGLFont');
  OpenGL_ASCII_Font := TOpenGL_ASCII_Font.Create(bitmap, 8, 12, 256);
  bitmap.Free;
End;

Function FPColorToColor(Const Color: TFPColor): TColor;
Begin
  result := byte(color.red Shr 8) Or (color.green And $FF00) Or ((color.blue And $FF00) Shl 8);
End;

Function ColorToFPColor(Const Color: TColor): TFPColor;
Begin
  result.alpha := 0;
  result.red := (byte(Color) Shl 8) Or $FF;
  result.green := ((Color) And $FF00) Or $FF;
  result.blue := (((Color) And $FF0000) Shr 8) Or $FF;
End;

{ TOpenGLFont }

Constructor TOpenGL_ASCII_Font.Create(Const Bitmap: Tbitmap; CharWidth,
  CharHeight, CharCount: Integer);
Var
  cxc, ax, ay, i, j: Integer;
  k: GLuint;
  TempIntfImg: TLazIntfImage;
  acol: TColor;
Begin
  Inherited create;
  fsize := CharHeight;
  fCharheight := CharHeight;
  fCharwidth := charwidth;
  fColor := v3(1, 1, 1);
  FCharcount := Charcount;
  FBaseList := glGenLists(Charcount);
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  cxc := Bitmap.width Div Charwidth;
  For k := 0 To FCharcount - 1 Do Begin
    glNewList(FBaseList + k, GL_COMPILE); // Start Building A List
    //    glbegin(GL_POINTS);  <-- Alte Variante mittels glPoints
    glbegin(GL_QUADS); // <-- Neu mittels Quads
{$WARNINGS off}
    ax := (k Mod cxc) * CharWidth;
    ay := (k Div cxc) * Charheight;
{$WARNINGS on}
    // Auslesen der Einzelnen Chars
    For i := 0 To charwidth - 1 Do
      For j := 0 To charheight - 1 Do Begin
        If (ax + i < TempIntfImg.Width) And (ay + j < TempIntfImg.Height) Then Begin
          acol := FPColorToColor(TempIntfImg.Colors[ax + i, ay + j]);
          If acol = clwhite Then Begin
            // glVertex2i(i, j); <-- Alte Variante mittels glPoints
            glVertex2i(i, j + 1); // <-- Neu mittels Quads
            glVertex2i(i + 1, j + 1); // <-- Neu mittels Quads
            glVertex2i(i + 1, j); // <-- Neu mittels Quads
            glVertex2i(i, j); // <-- Neu mittels Quads
          End;
        End;
      End;
    glend;
    gltranslated(CharWidth, 0, 0);
    glEndList(); // Done Building The Display List
  End;
  TempIntfImg.free;
End;

Destructor TOpenGL_ASCII_Font.destroy;
Begin
  glDeleteLists(FBaseList, FCharcount); // Delete All Display Lists
End;

Procedure TOpenGL_ASCII_Font.Textout(x, y: Integer; Text: String);
(*
 * Wenn die Schriftfarbe nicht so aussieht wie gewünscht, dann wurde vergessen ein
 *
 * glBindTexture(GL_TEXTURE_2D, 0);
 *
 * vor dem Aufruf zu machen, das wird hier nicht gemacht, da es unnötig ist, wenn der User gar keine Texturen verwendet..
 *
 * Sieht man gar nichts, dann wurde evtl. vergessen ein go2d auf zu rufen.
 *
 *)
Var
  k: integer;
  c: integer;
  sc: Single;
  light: Boolean;
  //  f: GLfloat;  <-- Alte Variante mittels glPoints
  currentColor: TVector4;
Begin
  //  glGetFloatv(GL_POINT_SIZE, @f); // Bakup der Point Size, diese wird hier Verändert !!  <-- Alte Variante mittels glPoints
  light := glIsEnabled(GL_LIGHTING);
  If light Then Begin
    glDisable(GL_LIGHTING); // Deaktivieren der Beleuchtung, die können wir hier nun wirklich nicht gebrauchen..
  End;
  glGetFloatv(GL_CURRENT_COLOR, @currentColor);
  glPushMatrix();
  glTranslatef(x, y, 0);
  glColor3fv(@fColor);
  sc := (fsize / fCharheight);
  glScalef(sc, sc, sc); // So Skallieren, dass die Schrift die Gewünschte Pixelzahl hat.
  //  sc := sc - 1 + f; // Einrechnen der Aktuellen Auflösung, so dass die Schrift "zusammenhängend" bleibt.  <-- Alte Variante mittels glPoints
  //  glPointSize(sc + 0.45); // Ohne diesen Faktor, sind in diversen Skallierungen keine ganzen Buchstaben zu sehen.  <-- Alte Variante mittels glPoints
  glPushMatrix();
  text := ConvertEncoding(text, EncodingUTF8, 'iso88591');
  For k := 1 To length(text) Do Begin

    //    weiter rauskriegen wo ÄÖÜ ß gerendert werden und ressource erzeugen

    c := ord(text[k]);
{$IFDEF Windows}
    If c = 13 Then Begin
{$ELSE}
    If c = 10 Then Begin
{$ENDIF}
      glPopMatrix();
      glTranslatef(0, fCharheight, 0);
      glPushMatrix();
    End
    Else If (c <> 10) And (c <> 13) Then Begin
      RenderChar(c);
    End;
  End;
  glPopMatrix();
  glPopMatrix();
  //  glPointSize(f);  <-- Alte Variante mittels glPoints
  glColor4fv(@currentColor);
  If light Then Begin
    glenable(GL_LIGHTING);
  End;
End;

Procedure TOpenGL_ASCII_Font.BillboardTextout(Position: TVector3;
  Height: TBaseType; Text: String);
Var
  H, s: Single;
  Matrix: TMatrix4x4;
  light: Boolean;
Begin
  light := glIsEnabled(GL_LIGHTING);
  If light Then Begin
    glDisable(GL_LIGHTING); // Deaktivieren der Beleuchtung, die können wir hier nun wirklich nicht gebrauchen..
  End;
  glPushMatrix();
  // Anfahren der Billboard Position
  glTranslatef(position.x, position.y, position.z);
  // Die Modelviewmatrix so drehen, dass wir Parallel zur Bildschirmebene sind
  glGetFloatv(GL_MODELVIEW_MATRIX, @Matrix[0, 0]);
  // Löschen des Positionsanteiles
  Matrix[3, 0] := 0;
  Matrix[3, 1] := 0;
  Matrix[3, 2] := 0;
  // Matrix Invertieren, das geht so weil die Matrix Orthogonal ist
  Matrix := TransposeMatrix(Matrix);
  // Switch in den 2D-Modus
  glMultMatrixf(@Matrix[0, 0]);
  H := TextHeight(text);
  // Richtig Skallieren
  s := Height / H;
  glScalef(s, -s, s);
  // Den Text auf der Bildboardposition zentrieren
  glTranslatef(-TextWidth(text) / 2, -h / 2, 0);
  // Ganz normal zeichnen
  Textout(0, 0, text);
  glPopMatrix;
  If light Then Begin
    glenable(GL_LIGHTING);
  End;
End;

Procedure TOpenGL_ASCII_Font.ThreeDTextout(Position, Up, Right: TVector3;
  Height: TBaseType; Text: String);
Var
  H, s: Single;
  Matrix: TMatrix4x4;
  light: Boolean;
Begin
  light := glIsEnabled(GL_LIGHTING);
  If light Then Begin
    glDisable(GL_LIGHTING); // Deaktivieren der Beleuchtung, die können wir hier nun wirklich nicht gebrauchen..
  End;
  glPushMatrix();
  // Anfahren der Billboard Position
  glTranslatef(position.x, position.y, position.z);
  // Die Modelviewmatrix so drehen, dass wir Parallel zur Bildschirmebene sind
  Matrix := CalculateOrthoMatrix(v3(0, 0, 0), CrossV3(up, right), up);
  // Switch in den 2D-Modus
  glMultMatrixf(@Matrix[0, 0]);
  H := TextHeight(text);
  // Richtig Skallieren
  s := Height / H;
  glScalef(s, -s, s);
  // Den Text auf der Position zentrieren
  glTranslatef(-TextWidth(text) / 2, -h / 2, 0);
  // Ganz normal zeichnen
  Textout(0, 0, text);
  glPopMatrix;
  If light Then Begin
    glenable(GL_LIGHTING);
  End;
End;

Procedure TOpenGL_ASCII_Font.RenderTextToRect(rect: TRect; Text: String);
Var
  s: Single;
  w, h, th, tw: Single;
Begin
  // 1. Bestimmen der Dimensionen ohne Scallierungen
  w := rect.Right - rect.Left;
  h := rect.Bottom - rect.Top;
  th := TextHeight(Text);
  tw := TextWidth(Text);
  s := Size;
  // 2. Berechnen der "Notwendigen" Scallierung
  If w / tw > h / th Then Begin
    Size := s * h / th;
  End
  Else Begin
    Size := s * w / tw;
  End;
  // 3. Aktualisieren der Dimensionen
  th := TextHeight(Text);
  tw := TextWidth(Text);
  // 4. Zentriert Darstellen
  glPushMatrix;
  glTranslatef(rect.Left, rect.Top, 0);
  Textout(round((w - tw) / 2), round((h - th) / 2), text);
  Size := s;
  glPopMatrix;
End;

Function TOpenGL_ASCII_Font.TextWidth(Text: String): single;
Const
{$IFDEF Windows}
  c = #13;
{$ELSE}
  c = #10;
{$ENDIF}

Var
  j: integer;
Begin
  text := ConvertEncoding(text, EncodingUTF8, 'iso88591');
  j := 0;
  While pos(c, text) <> 0 Do Begin
    j := max(j, pos(c, text) - 1);
    delete(Text, 1, pos(c, text));
  End;
  j := max(j, length(Text));
  result := j * (fsize / fCharheight) * fCharwidth;
End;

Function TOpenGL_ASCII_Font.TextHeight(text: String): single;
Const
{$IFDEF Windows}
  c = #13;
{$ELSE}
  c = #10;
{$ENDIF}
Var
  i, j: integer;
Begin
  j := 1;
  For i := 1 To length(text) Do Begin
    If (text[i]) = c Then inc(j);
  End;
  result := j * fsize;
End;

Function TOpenGL_ASCII_Font.TextWidth3D(Height: TBaseType; Text: String
  ): TBaseType;
Var
  w, h: TBaseType;

Begin
  h := TextHeight(text);
  w := TextWidth(text);
  result := (Height / h) * w;
End;

Procedure TOpenGL_ASCII_Font.RenderChar(Number: integer);
Begin
  glListBase(FBaseList);
  glCallLists(1, GL_UNSIGNED_BYTE, @Number);
End;

Procedure TOpenGL_ASCII_Font.setColor(AValue: TColor);
Begin
  fcolor.x := byte(Avalue) / 255;
  fcolor.y := byte(Avalue Shr 8) / 255;
  fcolor.z := byte(Avalue Shr 16) / 255;
End;

Procedure TOpenGL_ASCII_Font.setColorV3(AValue: TVector3);
Begin
  fColor := AValue;
End;

Procedure TOpenGL_ASCII_Font.setColorfp(AValue: TFPColor);
Begin
  fcolor.x := ((AValue.red And $FF00) Shr 8) / 255;
  fcolor.y := ((AValue.green And $FF00) Shr 8) / 255;
  fcolor.z := ((AValue.blue And $FF00) Shr 8) / 255;
End;

Function TOpenGL_ASCII_Font.getsize: Single;
Begin
  result := fsize;
End;

Function TOpenGL_ASCII_Font.getcolor: TColor;
Var
  r, g, b: integer;
Begin
  r := max(0, min(255, round(fColor.x * 255)));
  g := max(0, min(255, round(fColor.y * 255)));
  b := max(0, min(255, round(fColor.z * 255)));
  result := r Or (g Shl 8) Or (b Shl 16);
End;

Function TOpenGL_ASCII_Font.getcolorV3: TVector3;
Begin
  result := fColor;
End;

Function TOpenGL_ASCII_Font.getcolorfp: TFPColor;
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


Procedure TOpenGL_ASCII_Font.setsize(AValue: Single);
Begin
  fsize := AValue;
End;

Initialization

  (*
   * Ressourcendatei wurde erzeugt mittels :
   *
   * /home/corpsman/lazarus/tools/lazres /sda5/sda5/Tools/Projects/Sample/OpenGL/uopengl_ascii_font.ressource /sda5/sda5/Tools/Projects/Sample/OpenGL/uopengl_ascii_font.bmp=OpenGLFont
   *
   *)
{$I uopenGL_ascii_font.ressource}

Finalization

  If assigned(OpenGL_ASCII_Font) Then OpenGL_ASCII_Font.free;
  OpenGL_ASCII_Font := Nil;

End.

