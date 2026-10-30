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
    // Wie TGame.fViewPortInfo, damit der Dialog wie das Spiel skaliert wird (Seitenverhältnis beibehalten)
    ScreenWidth, ScreenHeight: integer;
    Left, Top: integer;
    Scale: Single;
    OnBeforeRender: TNotifyEvent; // Zum Setzen von ScreenWidth .. Scale vor jedem Render
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
  , uopengl_shaderprimitives
  , umessagedialog
  , uvectormath
  ;

{ TLoaderDialog }

Constructor TLoaderDialog.Create(Const Owner: TOpenGLControl);
Begin
  fWidth := 15 * 24;
  fHeight := 5 * 24;
  fOwner := Owner;
  Percent := 0;
  ScreenWidth := GameWidth;
  ScreenHeight := GameHeight;
  Left := 0;
  Top := 0;
  Scale := 1;
  OnBeforeRender := Nil;
  fTexture := OpenGL_GraphikEngine.LoadGraphikItem(ExtractFilePath(ParamStrUTF8(0)) + 'data' + PathDelim + 'res' + PathDelim + 'loaddialog.png', smClamp);
End;

Destructor TLoaderDialog.Destroy;
Begin
  // Die Textur wird nicht freigegeben, TMessageDialog nutzt die gleiche Datei (die Engine gibt sie dann als dieselbe Textur zurück)
  Inherited Destroy;
End;

Procedure TLoaderDialog.Render;
Var
  l, t, x: Integer;
  s: String;
  m: TMatrix4x4;
Begin
  If assigned(OnBeforeRender) Then OnBeforeRender(self);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glBindTexture(GL_TEXTURE_2D, 0);
  Go2d(ScreenWidth, ScreenHeight);
  m := IdentityMatrix4x4;
  m := TranslateMatrix4x4(m, Left, Top, 0);
  m := ScaleMatrix4x4(m, Scale, Scale, 1);
  SetShaderTransform(m);
  l := (GameWidth - fWidth) Div 2;
  t := (GameHeight - fHeight) Div 2;
  UseTextureShader;
  // Die "Dialogbox"
  RenderDialogFrame(l, t, fWidth, fHeight, atomic_dialog_Layer, fTexture);
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
  ResetShaderTransform;
  Exit2d();
  fOwner.SwapBuffers;
  Application.ProcessMessages;
End;

End.

