(******************************************************************************)
(* uOpenGL_ASCII_Font.pas                                          ??.??.???? *)
(*                                                                            *)
(* Version     : 0.08                                                         *)
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
(*               0.06 - Umstellen auf Abgeleitet von TOpenGL_Font             *)
(*               0.07 - TOpenGL_ASCII_BIG_Font = font mit Hintergrund         *)
(*               0.08 - Render via Shader as default                          *)
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

(*
Aktiviert die Nutzung von OpenGL im Legacy Mode, default ist Shader mode, der
auch mit OpenGL 3.3 funktioniert, aber nicht alle Funktionen von OpenGL 3.3 nutzt.
*)
{.$DEFINE LEGACYMODE}

Uses
  IntfGraphics, fpImage, Graphics, LCLType, LResources, classes,
  dglopengl, // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  uvectormath, // http://corpsman.de/index.php?doc=opengl/opengl_graphikengine
  LConvEncoding,
  uopengl_font_common,
  math;

Type

  { TOpenGL_ASCII_Font }

  TOpenGL_ASCII_Font = Class(TOpenGL_Font)
  private
    FCharcount: integer; // Anzahl der Zeichen, sollte immer 256 sein !!
    fCharheight: integer; // "Echte" Größe eines Buchstabens in Pixeln
    fCharwidth: integer; // "Echte" Größe eines Buchstabens in Pixeln
{$IFDEF LEGACYMODE}
    FBaseList: gluint; // Basispointer, für die OpenGL List
    Procedure RenderChar(Number: integer); // Zeichnet ein Zeichen
{$ELSE}
    FVBO: GLuint; // Vertex Buffer Object
    // Pro-Zeichen Dreiecks-Vertices in lokalen Zeichenkoordinaten (x,y Paare, 6 Vertices pro weißem Pixel = 2 Dreiecke)
    FCharData: Array[0..255] Of Array Of Single;
    FCharDataLen: Array[0..255] Of integer; // Anzahl der Floats in FCharData[i]
{$ENDIF}
  public
    (*
     * Die Init und Free Routinen brauchen nicht direkt aufgerufen zu werden. Hier
     * Genügt ein Aufruf von : Create_ASCII_Font
     * Freigeben tut sich die Klasse selbständig beim Beenden der Anwendung
     *)
    Constructor Create(Const Bitmap: Tbitmap; CharWidth, CharHeight, CharCount: Integer); virtual; reintroduce; // Alles was Weiß ist, ist sichtbar, der Rest Transparent
    Destructor Destroy; override;
    (*
     * Text Dimensionen
     *)
    Function TextWidth(Text: String): single; override; // Breite des Textes in Pixeln  ACHTUNG, Als CRT gillt nur #13 !!
    Function TextHeight(text: String): single; override; // Höhe des Textes in Pixeln   ACHTUNG, Als CRT gillt nur #13 !!
    Function TextWidth3D(Height: TBaseType; Text: String): TBaseType; virtual; // Im 3D-Mode Gibt man die Höhe des gesammten Textes vor, aus dieser Vorgabe lässt sich dann die Breite Ableiten
    (*
     * Zeichen Routinen
     *)
    Procedure Textout(x, y: Integer; Text: String); overload; override; // Zeichnet einen Text (Depth = 0.0), unter Berücksichtigung von CRT's (Vorher muss Go2d aufgerufen werden!)
    Procedure Textout(x, y: Integer; Depth: Single; Text: String); overload; virtual; // Wie Textout, jedoch kann hier die Tiefe (NDC z) angegeben werden
{$IFDEF LEGACYMODE}
    Procedure BillboardTextout(Position: TVector3; Height: TBaseType; Text: String); virtual; // Rendert einen Schriftzug (zentriert auf Position als Billboard ), da hier 3D-Koordinaten gelten, muss dem Text eine Höhe im 3D Skaling gegeben werden.
    Procedure ThreeDTextout(Position, Up, Right: TVector3; Height: TBaseType; Text: String); virtual; // Rendert eine  Schriftzug Zentriert auf Position und entsprechend Up und Right, da hier 3D-Koordinaten gelten, muss dem Text eine Höhe im 3D Skaling gegeben werden.
{$ENDIF}
    Procedure RenderTextToRect(rect: TRect; Text: String); overload; virtual; // Rendert einen Text So in Rect, dass er Maximal "gestretched" rein pass (ohne Umbrüche, oder so, nur durch Skallierungen)
    Procedure RenderTextToRect(rect: TRect; Depth: Single; Text: String); overload; virtual; // Wie RenderTextToRect, jedoch kann hier die Tiefe (NDC z) angegeben werden
  End;

  { TOpenGL_ASCII_BIG_Font }

  TOpenGL_ASCII_BIG_Font = Class(TOpenGL_ASCII_Font)
  private
    Font1, Font2: TOpenGL_ASCII_Font;
    Procedure Clear;
  protected
    Function getColor: TColor; override;
    Function getcolorfp: TFPColor; override;
    Function getcolorV3: TVector3; override;
    Function getsize: Single; override;
    Function getBackColor: TColor; virtual;
    Procedure setColor(AValue: TColor); override;
    Procedure setColorV3(AValue: TVector3); override;
    Procedure setColorfp(AValue: TFPColor); override;
    Procedure setsize(AValue: Single); override;
    Procedure setBackColor(AValue: TColor); virtual;
  public
    Property BackColor: TColor read getBackColor write setBackColor;

    (*
     * Die Init und Free Routinen brauchen nicht direkt aufgerufen zu werden. Hier
     * Genügt ein Aufruf von : Create_ASCII_BigFont
     * Freigeben tut sich die Klasse selbständig beim Beenden der Anwendung
     *)
    Constructor Create(Const Bitmap, Mask: Tbitmap; CharWidth, CharHeight, CharCount: Integer); virtual; reintroduce; // Alles was Weiß ist, ist sichtbar, der Rest Transparent
    Destructor Destroy; override;
    (*
     * Text Dimensionen
     *)
    Function TextWidth(Text: String): single; override; // Breite des Textes in Pixeln  ACHTUNG, Als CRT gillt nur #13 !!
    Function TextHeight(text: String): single; override; // Höhe des Textes in Pixeln   ACHTUNG, Als CRT gillt nur #13 !!
    Function TextWidth3D(Height: TBaseType; Text: String): TBaseType; override; // Im 3D-Mode Gibt man die Höhe des gesammten Textes vor, aus dieser Vorgabe lässt sich dann die Breite Ableiten
    (*
     * Zeichen Routinen
     *)
    Procedure Textout(x, y: Integer; Text: String); overload; override;
    Procedure Textout(x, y: Integer; Depth: Single; Text: String); overload; override;
{$IFDEF LEGACYMODE}
    Procedure BillboardTextout(Position: TVector3; Height: TBaseType; Text: String); override; // Rendert einen Schriftzug (zentriert auf Position als Billboard ), da hier 3D-Koordinaten gelten, muss dem Text eine Höhe im 3D Skaling gegeben werden.
    Procedure ThreeDTextout(Position, Up, Right: TVector3; Height: TBaseType; Text: String); override; // Rendert eine  Schriftzug Zentriert auf Position und entsprechend Up und Right, da hier 3D-Koordinaten gelten, muss dem Text eine Höhe im 3D Skaling gegeben werden.
{$ENDIF}
    Procedure RenderTextToRect(rect: TRect; Text: String); overload; override;
    Procedure RenderTextToRect(rect: TRect; Depth: Single; Text: String); overload; override;
  End;

Var
  OpenGL_ASCII_Font: TOpenGL_ASCII_Font = Nil;

  (*
   * Es stehen 2 Schriftarten als "OpenGL_ASCII_Font" zur Verfügung.
   *
   * Create_ASCII_Font(); -> erzeugt eine alte "DOS" like Schrift (8x12)
   * Create_ASCII_BigFont(); -> erzeugt eine alte "Dos" like schrift, aber mit 1-Pixel Rahmen (10x14) (ist besser for buntem Hintergrund erkennbar)
   *)
Procedure Create_ASCII_Font();
Procedure Create_ASCII_BigFont();

Implementation

Uses sysutils
{$IFNDEF LEGACYMODE}
  , uopengl_graphikengine
{$ENDIF}
  ;

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

Procedure Create_ASCII_BigFont(); // Muss in Make Current aufgerufen werden, da es OpenGLBefehle nutzt.
Var
  Mask, bitmap: TBitmap;
Begin
  If Assigned(OpenGL_ASCII_Font) Then OpenGL_ASCII_Font.free;
  Bitmap := TBitmap.Create;
  bitmap.LoadFromLazarusResource('OpenGLBigFont');
  Mask := TBitmap.Create;
  Mask.LoadFromLazarusResource('OpenGLBigFontMask');
  OpenGL_ASCII_Font := TOpenGL_ASCII_BIG_Font.Create(bitmap, Mask, 10, 14, 256);
  Mask.free;
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
{$IFNDEF LEGACYMODE}
  n: integer;
{$ENDIF}
Begin
  Inherited create;
  fsize := CharHeight;
  fCharheight := CharHeight;
  fCharwidth := charwidth;
  FCharcount := Charcount;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  cxc := Bitmap.width Div Charwidth;
{$IFDEF LEGACYMODE}
  FBaseList := glGenLists(Charcount);
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
{$ELSE}
  // Shader-Mode: Dreiecks-Geometrie pro Zeichen aufbauen
  For k := 0 To FCharcount - 1 Do Begin
    FCharDataLen[k] := 0;
    SetLength(FCharData[k], 0);
{$WARNINGS off}
    ax := (k Mod cxc) * CharWidth;
    ay := (k Div cxc) * Charheight;
{$WARNINGS on}
    For i := 0 To charwidth - 1 Do
      For j := 0 To charheight - 1 Do Begin
        If (ax + i < TempIntfImg.Width) And (ay + j < TempIntfImg.Height) Then Begin
          acol := FPColorToColor(TempIntfImg.Colors[ax + i, ay + j]);
          If acol = clwhite Then Begin
            // 2 Dreiecke (6 Vertices) pro weißem Pixel
            n := FCharDataLen[k];
            SetLength(FCharData[k], n + 12); // 6 * 2 Floats
            // Dreieck 1: (i,j), (i+1,j), (i+1,j+1)
            FCharData[k][n + 0] := i;
            FCharData[k][n + 1] := j;
            FCharData[k][n + 2] := i + 1;
            FCharData[k][n + 3] := j;
            FCharData[k][n + 4] := i + 1;
            FCharData[k][n + 5] := j + 1;
            // Dreieck 2: (i,j), (i+1,j+1), (i,j+1)
            FCharData[k][n + 6] := i;
            FCharData[k][n + 7] := j;
            FCharData[k][n + 8] := i + 1;
            FCharData[k][n + 9] := j + 1;
            FCharData[k][n + 10] := i;
            FCharData[k][n + 11] := j + 1;
            Inc(FCharDataLen[k], 12);
          End;
        End;
      End;
  End;
  glGenBuffers(1, @FVBO);
{$ENDIF}
  TempIntfImg.free;
End;

Destructor TOpenGL_ASCII_Font.Destroy;
{$IFNDEF LEGACYMODE}
Var
  k: integer;
{$ENDIF}
Begin
{$IFDEF LEGACYMODE}
  glDeleteLists(FBaseList, FCharcount); // Delete All Display Lists
{$ELSE}
  If FVBO <> 0 Then glDeleteBuffers(1, @FVBO);
  For k := 0 To 255 Do
    SetLength(FCharData[k], 0);
{$ENDIF}
End;

Procedure TOpenGL_ASCII_Font.Textout(x, y: Integer; Text: String);
Begin
  Textout(x, y, 0.0, text);
End;

Procedure TOpenGL_ASCII_Font.Textout(x, y: Integer; Depth: Single; Text: String);
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
{$IFDEF LEGACYMODE}
Var
  k, c: integer;
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
{$ELSE}
Var
  res: Tpoint;
  k, c, vi, fi, nFloats: integer;
  allVerts: Array Of Single;
  sc, penX, penY, startX: Single;
  LocRes, CurrentProgram: GLint;
Begin
  text := ConvertEncoding(text, EncodingUTF8, 'iso88591');
  sc := fsize / fCharheight;
  penX := x;
  penY := y;
  startX := x;
  nFloats := 0;
  allVerts := Nil;
  SetLength(allVerts, 0);
  // Alle Zeichen des Textes in einen Vertex-Buffer aufbauen (Bildschirm-Koordinaten)
  For k := 1 To length(text) Do Begin
    c := ord(text[k]);
{$IFDEF Windows}
    If c = 13 Then Begin
{$ELSE}
    If c = 10 Then Begin
{$ENDIF}
      penX := startX;
      penY := penY + fsize;
    End
    Else If (c <> 10) And (c <> 13) Then Begin
      If (c >= 0) And (c <= 255) And (FCharDataLen[c] > 0) Then Begin
        vi := nFloats;
        // FCharDataLen[c] enthält Anzahl Floats für 2D (x,y), wir brauchen 3D (x,y,z)
        Inc(nFloats, (FCharDataLen[c] Div 2) * 3); // Aus 2D-Vertices (x,y) werden 3D-Vertices (x,y,z)
        SetLength(allVerts, nFloats);
        fi := 0;
        While fi < FCharDataLen[c] Do Begin
          allVerts[vi] := penX + FCharData[c][fi] * sc; // x
          allVerts[vi + 1] := penY + FCharData[c][fi + 1] * sc; // y
          allVerts[vi + 2] := Depth; // z (Tiefe)
          Inc(vi, 3);
          Inc(fi, 2);
        End;
      End;
      penX := penX + fCharwidth * sc;
    End;
  End;
  If nFloats = 0 Then Exit;
  // Use Color Shader from uOpenGL_GraphikEngine.pas, in order to take care of the ShaderMatrix
  UseColorShader;
  SetShaderColor(fColor.x, fColor.y, fColor.z, 1.0);
  glGetIntegerv(GL_CURRENT_PROGRAM, @CurrentProgram);
  LocRes := glGetUniformLocation(CurrentProgram, 'uResolution');
  If LocRes >= 0 Then Begin
    res := Get2DResolution;
    glUniform2f(LocRes, res.x, res.y);
  End;
  // VBO und VAO konfigurieren für vec3 (x,y,z)
  glBindBuffer(GL_ARRAY_BUFFER, FVBO);
  glBufferData(GL_ARRAY_BUFFER, nFloats * SizeOf(GLfloat), @allVerts[0], GL_DYNAMIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, Nil); // 3 Komponenten für vec3
  // Zeichnen
  glDrawArrays(GL_TRIANGLES, 0, nFloats Div 3);
  glBindVertexArray(0);
  UseTextureShader; // Default is textureShader, so switch Back to this
{$ENDIF}
End;

{$IFDEF LEGACYMODE}

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
{$ENDIF} // LEGACYMODE

Procedure TOpenGL_ASCII_Font.RenderTextToRect(rect: TRect; Text: String);
Begin
  RenderTextToRect(rect, 0.0, text);
End;

Procedure TOpenGL_ASCII_Font.RenderTextToRect(rect: TRect; Depth: Single; Text: String);
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
{$IFDEF LEGACYMODE}
  glPushMatrix;
  glTranslatef(rect.Left, rect.Top, 0);
  Textout(round((w - tw) / 2), round((h - th) / 2), depth, text);
  Size := s;
  glPopMatrix;
{$ELSE}
  Textout(round(rect.Left + (w - tw) / 2), round(rect.Top + (h - th) / 2), depth, text);
  Size := s;
{$ENDIF}
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

{$IFDEF LEGACYMODE}

Procedure TOpenGL_ASCII_Font.RenderChar(Number: integer);
Begin
  glListBase(FBaseList);
  glCallLists(1, GL_UNSIGNED_BYTE, @Number);
End;
{$ENDIF} // LEGACYMODE

{ TOpenGL_ASCII_BIG_Font }

Procedure TOpenGL_ASCII_BIG_Font.Clear;
Begin
  If assigned(Font1) Then Font1.Free;
  If assigned(Font2) Then Font2.Free;
  font1 := Nil;
  font2 := Nil;
End;

Function TOpenGL_ASCII_BIG_Font.getColor: TColor;
Begin
  Result := font1.getColor;
End;

Function TOpenGL_ASCII_BIG_Font.getcolorfp: TFPColor;
Begin
  Result := font1.getcolorfp;
End;

Function TOpenGL_ASCII_BIG_Font.getcolorV3: TVector3;
Begin
  Result := font1.getcolorV3;
End;

Function TOpenGL_ASCII_BIG_Font.getsize: Single;
Begin
  Result := font1.getsize;
End;

Function TOpenGL_ASCII_BIG_Font.getBackColor: TColor;
Begin
  Result := font2.getColor;
End;

Procedure TOpenGL_ASCII_BIG_Font.setColor(AValue: TColor);
Begin
  font1.setColor(AValue);
End;

Procedure TOpenGL_ASCII_BIG_Font.setColorV3(AValue: TVector3);
Begin
  font1.setColorV3(AValue);
End;

Procedure TOpenGL_ASCII_BIG_Font.setColorfp(AValue: TFPColor);
Begin
  font1.setColorfp(AValue);
End;

Procedure TOpenGL_ASCII_BIG_Font.setsize(AValue: Single);
Begin
  font1.setsize(AValue);
  font2.setsize(AValue);
End;

Procedure TOpenGL_ASCII_BIG_Font.setBackColor(AValue: TColor);
Begin
  font2.setColor(AValue);
End;

Constructor TOpenGL_ASCII_BIG_Font.Create(Const Bitmap, Mask: Tbitmap;
  CharWidth, CharHeight, CharCount: Integer);
Begin
  If (bitmap.Width <> mask.Width) Or
    (bitmap.Height <> mask.Height) Then Begin
    Raise exception.Create('TOpenGL_ASCII_BIG_Font.Create: bitmap and mask need to have same size!');
  End;
  Font1 := Nil;
  Font2 := Nil;
  Clear;
  Font1 := TOpenGL_ASCII_Font.Create(bitmap, CharWidth, CharHeight, CharCount);
  Font2 := TOpenGL_ASCII_Font.Create(Mask, CharWidth, CharHeight, CharCount);
  Font1.Color := clWhite;
  Font2.Color := clBlack;
End;

Destructor TOpenGL_ASCII_BIG_Font.Destroy;
Begin
  clear;
End;

Function TOpenGL_ASCII_BIG_Font.TextWidth(Text: String): single;
Begin
  result := Font1.TextWidth(Text);
End;

Function TOpenGL_ASCII_BIG_Font.TextHeight(text: String): single;
Begin
  result := Font1.TextHeight(Text);
End;

Function TOpenGL_ASCII_BIG_Font.TextWidth3D(Height: TBaseType; Text: String
  ): TBaseType;
Begin
  result := Font1.TextWidth3D(Height, Text);
End;

Procedure TOpenGL_ASCII_BIG_Font.Textout(x, y: Integer; Text: String);
Begin
  Textout(x, y, 0.0, text);
End;

Procedure TOpenGL_ASCII_BIG_Font.Textout(x, y: Integer; Depth: Single; Text: String);
Begin
{$IFDEF LEGACYMODE}
  glPushMatrix;
  font2.Textout(x, y, depth, text);
  glTranslatef(0, 0, Epsilon);
  font1.Textout(x, y, depth, text);
  glPopMatrix;
{$ELSE}
  glDepthMask(GL_FALSE);
  font2.Textout(x, y, depth, text);
  glDepthMask(GL_TRUE);
  font1.Textout(x, y, depth - 0.001, text);
{$ENDIF}
End;

{$IFDEF LEGACYMODE}

Procedure TOpenGL_ASCII_BIG_Font.BillboardTextout(Position: TVector3;
  Height: TBaseType; Text: String);
Begin
  Raise Exception.Create('TOpenGL_ASCII_BIG_Font.BillboardTextout: not implemented.');
End;

Procedure TOpenGL_ASCII_BIG_Font.ThreeDTextout(Position, Up, Right: TVector3;
  Height: TBaseType; Text: String);
Begin
  Raise Exception.Create('TOpenGL_ASCII_BIG_Font.ThreeDTextout: not implemented.');
End;
{$ENDIF} // LEGACYMODE

Procedure TOpenGL_ASCII_BIG_Font.RenderTextToRect(rect: TRect; Text: String);
Begin
  RenderTextToRect(rect, 0.0, text);
End;

Procedure TOpenGL_ASCII_BIG_Font.RenderTextToRect(rect: TRect; Depth: Single; Text: String);
Begin
{$IFDEF LEGACYMODE}
  glPushMatrix;
  font2.RenderTextToRect(rect, depth, text);
  glTranslatef(0, 0, Epsilon);
  font1.RenderTextToRect(rect, depth, text);
  glPopMatrix;
{$ELSE}
  glDepthMask(GL_FALSE);
  font2.RenderTextToRect(rect, depth, text);
  glDepthMask(GL_TRUE);
  font1.RenderTextToRect(rect, depth - 0.001, text);
{$ENDIF}
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

