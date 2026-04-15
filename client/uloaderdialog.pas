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
Unit uloaderdialog;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, OpenGLContext, uopengl_graphikengine;

Type

  { TLoaderDialog }

  TLoaderDialog = Class
  private
    fWidth: integer;
    fHeight: integer;
    fOwner: TOpenGLControl;
    fTexture: TGraphikItem; // Die 9 Einzeltextruren des Hintergrunds
  public
    Percent: integer;
    Constructor Create(Const Owner: TOpenGLControl);
    Destructor Destroy(); override;
    (*
     * !Achtung!
     * das ist Asynchron und macht ein Vollständiges Render !
     * -> Sollte also nur in Blockierenden Aufgaben aufgerufen werden !
     *)
    Procedure Render();
  End;

Implementation

Uses
  Graphics
  , dglOpenGL
  , Forms
  , LazUTF8
  , uatomic_common
  , uOpenGL_ASCII_Font
{$IFNDEF LEGACYMODE}
  , uopengl_shaderprimitives
{$ENDIF}
  ;

{ TLoaderDialog }

Constructor TLoaderDialog.Create(Const Owner: TOpenGLControl);
Begin
  fWidth := 15 * 24;
  fHeight := 5 * 24;
  fOwner := Owner;
  Percent := 0;
  fTexture := OpenGL_GraphikEngine.LoadGraphikItem(ExtractFilePath(ParamStrUTF8(0)) + 'data' + PathDelim + 'res' + PathDelim + 'loaddialog.png', smClamp);
End;

Destructor TLoaderDialog.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To 8 Do Begin
    OpenGL_GraphikEngine.RemoveGraphik(fTexture);
  End;
  Inherited Create;
End;

Procedure TLoaderDialog.Render;
Var
{$IFNDEF LEGACYMODE}
  l, t,
{$ENDIF}
  x, i, j: Integer;
  s: String;
Begin
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
{$IFDEF LEGACYMODE}
  glLoadIdentity();
  glcolor4f(1, 1, 1, 1);
{$ENDIF}
  glBindTexture(GL_TEXTURE_2D, 0);
  Go2d(GameWidth, GameHeight);
{$IFDEF LEGACYMODE}
  glPushMatrix;
  glTranslatef((GameWidth - fWidth) Div 2, (GameHeight - fHeight) Div 2, atomic_dialog_Layer);
  glPushMatrix;
  // Die "Dialogbox"
  // 1. Zeile
  RenderTiledQuad(0, 0, 0, 3, 3, fTexture); // Ecke Oben Links
  glPushMatrix;
  For i := 1 To (fWidth Div 24) - 2 Do Begin
    glTranslatef(24, 0, 0);
    RenderTiledQuad(0, 0, 1, 3, 3, fTexture); // Obere Mittlere Kante
  End;
  glTranslatef(24, 0, 0);
  RenderTiledQuad(0, 0, 2, 3, 3, fTexture); // Ecke Oben Rechts
  glPopMatrix;
  // Zeile 2 bis N -1
  For j := 1 To (fHeight Div 24) - 2 Do Begin
    glTranslatef(0, 24, 0);
    glPushMatrix;
    RenderTiledQuad(0, 0, 3, 3, 3, fTexture); // Linke Kante
    For i := 1 To (fWidth Div 24) - 2 Do Begin
      glTranslatef(24, 0, 0);
      RenderTiledQuad(0, 0, 4, 3, 3, fTexture); // Die Mittelstücke
    End;
    glTranslatef(24, 0, 0);
    RenderTiledQuad(0, 0, 5, 3, 3, fTexture); // Rechte Kante
    glPopMatrix;
  End;
  // Letzte Zeile
  glTranslatef(0, 24, 0);
  RenderTiledQuad(0, 0, 6, 3, 3, fTexture); // Ecke unten Links
  For i := 1 To (fWidth Div 24) - 2 Do Begin
    glTranslatef(24, 0, 0);
    RenderTiledQuad(0, 0, 7, 3, 3, fTexture); // untere Mittlere Kante
  End;
  glTranslatef(24, 0, 0);
  RenderTiledQuad(0, 0, 8, 3, 3, fTexture); // Ecke Unten Rechts
  glPopMatrix;
  // Rendern der Elemente auf dem Dialog
  glTranslatef(0, 0, atomic_EPSILON);
  glBindTexture(GL_TEXTURE_2D, 0);
  // Der Schriftzug "Loading data..."
  s := 'Loading data...';
  OpenGL_ASCII_Font.Color := clWhite;
  OpenGL_ASCII_Font.Textout((fWidth - round(OpenGL_ASCII_Font.TextWidth(s))) Div 2, 24, s);
  // Die Prozentzahl
  s := inttostr(Percent) + '%';
  OpenGL_ASCII_Font.Color := clYellow;
  OpenGL_ASCII_Font.Textout((fWidth - round(OpenGL_ASCII_Font.TextWidth(s))) Div 2, (fHeight - round(OpenGL_ASCII_Font.TextHeight(s))) Div 2, s);
  // Der Fortschrittsbalken
  glTranslatef(0, 0, -atomic_EPSILON / 2); // Der Weise Rahmen, mittels LineLoop sieht das scheiße aus, also muss der so in den Hintergrund der anderen beiden.
  glBegin(GL_QUADS);
  glColor3f(1, 1, 1);
  glVertex3f(24, 82, 0);
  glVertex3f(24, 82 + 14, 0);
  glVertex3f(fWidth - 24, 82 + 14, 0);
  glVertex3f(fWidth - 24, 82, 0);
  glend;
  glTranslatef(0, 0, atomic_EPSILON);
  x := round((fWidth - 48 - 2) * Percent / 100);
  glBegin(GL_QUADS);
  glColor3f(168 / 255, 168 / 255, 168 / 255);
  glVertex3f(24 + 1, 82 + 1, 0);
  glVertex3f(24 + 1, 82 + 14 - 1, 0);
  glVertex3f(24 + 1 + x, 82 + 14 - 1, 0);
  glVertex3f(24 + 1 + x, 82 + 1, 0);
  glend;
  glBegin(GL_QUADS);
  glColor3f(0, 0, 0);
  glVertex3f(24 + 1 + x, 82 + 1, 0);
  glVertex3f(24 + 1 + x, 82 + 14 - 1, 0);
  glVertex3f(fWidth - 24 - 1, 82 + 14 - 1, 0);
  glVertex3f(fWidth - 24 - 1, 82 + 1, 0);
  glend;
  glPopMatrix;
{$ELSE}
  l := (GameWidth - fWidth) Div 2;
  t := (GameHeight - fHeight) Div 2;
  UseTextureShader;
  // Die "Dialogbox"
  // 1. Zeile
  RenderTiledQuad(l, t, atomic_dialog_Layer, 0, 3, 3, fTexture); // Ecke Oben Links
  For i := 1 To (fWidth Div 24) - 2 Do Begin
    RenderTiledQuad(l + i * 24, t, atomic_dialog_Layer, 1, 3, 3, fTexture); // Obere Mittlere Kante
  End;
  RenderTiledQuad(l + ((fWidth Div 24) - 1) * 24, t, atomic_dialog_Layer, 2, 3, 3, fTexture); // Ecke Oben Rechts
  // Zeile 2 bis N -1
  For j := 1 To (fHeight Div 24) - 2 Do Begin
    RenderTiledQuad(l, t + j * 24, atomic_dialog_Layer, 3, 3, 3, fTexture); // Linke Kante
    For i := 1 To (fWidth Div 24) - 2 Do Begin
      RenderTiledQuad(l + i * 24, t + j * 24, atomic_dialog_Layer, 4, 3, 3, fTexture); // Die Mittelstücke
    End;
    RenderTiledQuad(l + ((fWidth Div 24) - 1) * 24, t + j * 24, atomic_dialog_Layer, 5, 3, 3, fTexture); // Rechte Kante
  End;
  // Letzte Zeile
  RenderTiledQuad(l, t + ((fHeight Div 24) - 1) * 24, atomic_dialog_Layer, 6, 3, 3, fTexture); // Ecke unten Links
  For i := 1 To (fWidth Div 24) - 2 Do Begin
    RenderTiledQuad(l + i * 24, t + ((fHeight Div 24) - 1) * 24, atomic_dialog_Layer, 7, 3, 3, fTexture); // untere Mittlere Kante
  End;
  RenderTiledQuad(l + ((fWidth Div 24) - 1) * 24, t + ((fHeight Div 24) - 1) * 24, atomic_dialog_Layer, 8, 3, 3, fTexture); // Ecke Unten Rechts
  // Rendern der Elemente auf dem Dialog
  glBindTexture(GL_TEXTURE_2D, 0);
  // Der Schriftzug "Loading data..."
  s := 'Loading data...';
  OpenGL_ASCII_Font.Color := clWhite;
  OpenGL_ASCII_Font.Textout(l + (fWidth - round(OpenGL_ASCII_Font.TextWidth(s))) Div 2, t + 24, atomic_dialog_Layer + atomic_EPSILON, s);
  // Die Prozentzahl
  s := inttostr(Percent) + '%';
  OpenGL_ASCII_Font.Color := clYellow;
  OpenGL_ASCII_Font.Textout(l + (fWidth - round(OpenGL_ASCII_Font.TextWidth(s))) Div 2, t + (fHeight - round(OpenGL_ASCII_Font.TextHeight(s))) Div 2, atomic_dialog_Layer + atomic_EPSILON, s);
  // Der Fortschrittsbalken
  UseColorShader;
  glShaderBegin(GL_TRIANGLE_FAN);
  SetShaderColor(1, 1, 1);
  glShaderVertex(l + 24, t + 82, atomic_dialog_Layer + atomic_EPSILON / 2);
  glShaderVertex(l + 24, t + 82 + 14, atomic_dialog_Layer + atomic_EPSILON / 2);
  glShaderVertex(l + fWidth - 24, t + 82 + 14, atomic_dialog_Layer + atomic_EPSILON / 2);
  glShaderVertex(l + fWidth - 24, t + 82, atomic_dialog_Layer + atomic_EPSILON / 2);
  glShaderEnd();
  glShaderBegin(GL_TRIANGLE_FAN);
  x := round((fWidth - 48 - 2) * Percent / 100);
  glShaderBegin(GL_TRIANGLE_FAN);
  SetShaderColor(168 / 255, 168 / 255, 168 / 255);
  glShaderVertex(l + 24 + 1, t + 82 + 1, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(l + 24 + 1, t + 82 + 14 - 1, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(l + 24 + 1 + x, t + 82 + 14 - 1, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(l + 24 + 1 + x, t + 82 + 1, atomic_dialog_Layer + atomic_EPSILON);
  glShaderEnd();
  glShaderBegin(GL_TRIANGLE_FAN);
  SetShaderColor(0, 0, 0);
  glShaderVertex(l + 24 + 1 + x, t + 82 + 1, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(l + 24 + 1 + x, t + 82 + 14 - 1, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(l + fWidth - 24 - 1, t + 82 + 14 - 1, atomic_dialog_Layer + atomic_EPSILON);
  glShaderVertex(l + fWidth - 24 - 1, t + 82 + 1, atomic_dialog_Layer + atomic_EPSILON);
  glShaderEnd();
  UseTextureShader;
{$ENDIF}
  Exit2d();
  fOwner.SwapBuffers;
  Application.ProcessMessages;
End;

End.

