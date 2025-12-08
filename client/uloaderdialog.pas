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
  Classes, SysUtils, OpenGLContext;

Type

  { TLoaderDialog }

  TLoaderDialog = Class
  private
    fWidth: integer;
    fHeight: integer;
    fOwner: TOpenGLControl;
    fTextures: Array[0..8] Of Integer; // Die 9 Einzeltextruren des Hintergrunds
  public
    Percent: integer;
    Constructor Create(Const Owner: TOpenGLControl; Const DataPath: String = '');
    Destructor Destroy(); override;
    (*
     * !Achtung!
     * das ist Asynchron und macht ein Vollständiges Render !
     * -> Sollte also nur in Blockierenden Aufgaben aufgerufen werden !
     *)
    Procedure Render();
    Procedure RenderDirect(); // Render without SwapBuffers and ProcessMessages - for use in OnPaint
  End;

Implementation

Uses
  Graphics
  , dglOpenGL
  , Forms
  , LazUTF8
  , LazFileUtils
  , uatomic_common
  , uearlylog
  , uopengl_graphikengine
  , uOpenGL_ASCII_Font
  ;

Procedure RenderImg(w, h, ImageIndex: integer); // TODO: das ggf noch mal irgendwohin auslagern ??
Begin
  If ImageIndex = 0 Then Begin
    uearlylog.EarlyLog('RenderImg: WARNING - ImageIndex is 0 (invalid texture)');
    exit;
  End;
  glEnable(GL_TEXTURE_2D);
  glBindTexture(gl_texture_2d, ImageIndex);
  glbegin(gl_quads);
  glTexCoord2f(0, 1);
  glvertex3f(0, h, 0);

  glTexCoord2f(1, 1);
  glvertex3f(w, h, 0);

  glTexCoord2f(1, 0);
  glvertex3f(w, 0, 0);

  glTexCoord2f(0, 0);
  glvertex3f(0, 0, 0);
  glend;
End;

{ TLoaderDialog }

Constructor TLoaderDialog.Create(Const Owner: TOpenGLControl; Const DataPath: String = '');
Var
  p: String;
  png: TPortableNetworkGraphic;
  b, b2: TBitmap;
  i: Integer;
Begin
  fWidth := 15 * 24;
  fHeight := 5 * 24;
  fOwner := Owner;
  Percent := 0;
  // CRITICAL: Ensure OpenGL context is active before loading textures
  // This is required because LoadGraphik uses glGenTextures and glTexImage2D
  If Not Owner.MakeCurrent Then Begin
    uearlylog.EarlyLog('TLoaderDialog.Create: ERROR - OpenGL context is not active');
    Raise Exception.Create('TLoaderDialog.Create: OpenGL context is not active');
  End;
  uearlylog.EarlyLog('TLoaderDialog.Create: OpenGL context is active');
  // Use provided DataPath if available, otherwise fall back to old method
  If DataPath <> '' Then Begin
    p := IncludeTrailingPathDelimiter(DataPath) + 'res' + PathDelim + 'loaddialog.png';
  End Else Begin
    p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'data' + PathDelim + 'res' + PathDelim + 'loaddialog.png';
  End;
  // Log path for debugging
  uearlylog.EarlyLog('TLoaderDialog.Create: Looking for loaddialog.png at: ' + p);
  uearlylog.EarlyLog('TLoaderDialog.Create: File exists: ' + BoolToStr(FileExistsUTF8(p), true));
  uatomic_common.log('TLoaderDialog.Create: Looking for loaddialog.png at: ' + p, llInfo);
  uatomic_common.log('TLoaderDialog.Create: File exists: ' + BoolToStr(FileExistsUTF8(p), true), llInfo);
  png := TPortableNetworkGraphic.Create;
  If Not FileExistsUTF8(p) Then Begin
    png.Free;
    uatomic_common.log('TLoaderDialog.Create: ERROR - Could not find loaddialog.png at ' + p, llError);
    Raise Exception.Create('TLoaderDialog.Create: Could not find loaddialog.png at ' + p);
  End;
  uatomic_common.log('TLoaderDialog.Create: Loading PNG file...', llInfo);
  png.LoadFromFile(p);
  uatomic_common.log('TLoaderDialog.Create: PNG loaded, size: ' + IntToStr(png.Width) + 'x' + IntToStr(png.Height), llInfo);
  b := TBitmap.create;
  b.Assign(png);
  b.Transparent := false;
  png.free;
  b2 := TBitmap.Create;
  b2.Height := 24;
  b2.Width := 24;
  b2.Transparent := false;
  uearlylog.EarlyLog('TLoaderDialog.Create: Loading textures...');
  uatomic_common.log('TLoaderDialog.Create: Loading textures...', llInfo);
  For i := 0 To 8 Do Begin
    b2.Canvas.Draw(-(i Mod 3) * 24, -(i Div 3) * 24, b);
    fTextures[i] := OpenGL_GraphikEngine.LoadGraphik(b2, 'TLoaderDialog_img' + inttostr(i), smStretch);
    If fTextures[i] = 0 Then Begin
      uearlylog.EarlyLog('TLoaderDialog.Create: WARNING - Failed to load texture ' + IntToStr(i));
      uatomic_common.log('TLoaderDialog.Create: WARNING - Failed to load texture ' + IntToStr(i), llWarning);
    End Else Begin
      uearlylog.EarlyLog('TLoaderDialog.Create: Texture ' + IntToStr(i) + ' loaded, ID: ' + IntToStr(fTextures[i]));
      uatomic_common.log('TLoaderDialog.Create: Texture ' + IntToStr(i) + ' loaded, ID: ' + IntToStr(fTextures[i]), llInfo);
    End;
  End;
  uearlylog.EarlyLog('TLoaderDialog.Create: All textures loaded');
  uatomic_common.log('TLoaderDialog.Create: All textures loaded', llInfo);
  b2.free;
  b.free;
End;

Destructor TLoaderDialog.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To 8 Do Begin
    OpenGL_GraphikEngine.RemoveGraphik(fTextures[i]);
  End;
  Inherited Create;
End;

Procedure TLoaderDialog.Render;
Var
  x, i, j: Integer;
  s: String;
Begin
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glcolor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  Go2d(GameWidth, GameHeight);
  glPushMatrix;
  glTranslatef((GameWidth - fWidth) Div 2, (GameHeight - fHeight) Div 2, atomic_dialog_Layer);
  glPushMatrix;
  // Die "Dialogbox"
  // 1. Zeile
  RenderImg(24, 24, fTextures[0]); // Ecke Oben Links
  glPushMatrix;
  For i := 1 To (fWidth Div 24) - 2 Do Begin
    glTranslatef(24, 0, 0);
    RenderImg(24, 24, fTextures[1]); // Obere Mittlere Kante
  End;
  glTranslatef(24, 0, 0);
  RenderImg(24, 24, fTextures[2]); // Ecke Oben Rechts
  glPopMatrix;
  // Zeile 2 bis N -1
  For j := 1 To (fHeight Div 24) - 2 Do Begin
    glTranslatef(0, 24, 0);
    glPushMatrix;
    RenderImg(24, 24, fTextures[3]); // Linke Kante
    For i := 1 To (fWidth Div 24) - 2 Do Begin
      glTranslatef(24, 0, 0);
      RenderImg(24, 24, fTextures[4]); // Die Mittelstücke
    End;
    glTranslatef(24, 0, 0);
    RenderImg(24, 24, fTextures[5]); // Rechte Kante
    glPopMatrix;
  End;
  // Letzte Zeile
  glTranslatef(0, 24, 0);
  RenderImg(24, 24, fTextures[6]); // Ecke unten Links
  For i := 1 To (fWidth Div 24) - 2 Do Begin
    glTranslatef(24, 0, 0);
    RenderImg(24, 24, fTextures[7]); // untere Mittlere Kante
  End;
  glTranslatef(24, 0, 0);
  RenderImg(24, 24, fTextures[8]); // Ecke Unten Rechts
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
  glTranslatef(0, 0, -atomic_EPSILON); // Der Weise Rahmen, mittels LineLoop sieht das scheiße aus, also muss der so in den Hintergrund der anderen beiden.
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
  Exit2d();
  fOwner.SwapBuffers;
  Application.ProcessMessages;
End;

Procedure TLoaderDialog.RenderDirect();
Var
  x, i, j: Integer;
  s: String;
  vp: Array[0..3] Of GLint;
Begin
  // Same as Render(), but without SwapBuffers and ProcessMessages
  // This is for use in OnPaint, which already handles these
  uearlylog.EarlyLog('TLoaderDialog.RenderDirect: Starting render');
  uearlylog.EarlyLog('TLoaderDialog.RenderDirect: fTextures[0] = ' + IntToStr(fTextures[0]));
  
  // CRITICAL: Ensure OpenGL context is active
  If Not fOwner.MakeCurrent Then Begin
    uearlylog.EarlyLog('TLoaderDialog.RenderDirect: ERROR - OpenGL context is not active');
    exit;
  End;
  
  // Check viewport
  glGetIntegerv(GL_VIEWPORT, @vp[0]);
  uearlylog.EarlyLog('TLoaderDialog.RenderDirect: Viewport: ' + IntToStr(vp[0]) + ',' + IntToStr(vp[1]) + ',' + IntToStr(vp[2]) + 'x' + IntToStr(vp[3]));
  
  // Enable texturing
  glEnable(GL_TEXTURE_2D);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glcolor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  Go2d(GameWidth, GameHeight);
  glPushMatrix;
  glTranslatef((GameWidth - fWidth) Div 2, (GameHeight - fHeight) Div 2, atomic_dialog_Layer);
  glPushMatrix;
  // Die "Dialogbox"
  // 1. Zeile
  RenderImg(24, 24, fTextures[0]); // Ecke Oben Links
  glPushMatrix;
  For i := 1 To (fWidth Div 24) - 2 Do Begin
    glTranslatef(24, 0, 0);
    RenderImg(24, 24, fTextures[1]); // Obere Mittlere Kante
  End;
  glTranslatef(24, 0, 0);
  RenderImg(24, 24, fTextures[2]); // Ecke Oben Rechts
  glPopMatrix;
  // Zeile 2 bis N -1
  For j := 1 To (fHeight Div 24) - 2 Do Begin
    glTranslatef(0, 24, 0);
    glPushMatrix;
    RenderImg(24, 24, fTextures[3]); // Linke Kante
    For i := 1 To (fWidth Div 24) - 2 Do Begin
      glTranslatef(24, 0, 0);
      RenderImg(24, 24, fTextures[4]); // Die Mittelstücke
    End;
    glTranslatef(24, 0, 0);
    RenderImg(24, 24, fTextures[5]); // Rechte Kante
    glPopMatrix;
  End;
  // Letzte Zeile
  glTranslatef(0, 24, 0);
  RenderImg(24, 24, fTextures[6]); // Ecke unten Links
  For i := 1 To (fWidth Div 24) - 2 Do Begin
    glTranslatef(24, 0, 0);
    RenderImg(24, 24, fTextures[7]); // untere Mittlere Kante
  End;
  glTranslatef(24, 0, 0);
  RenderImg(24, 24, fTextures[8]); // Ecke Unten Rechts
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
  glTranslatef(0, 0, -atomic_EPSILON); // Der Weise Rahmen, mittels LineLoop sieht das scheiße aus, also muss der so in den Hintergrund der anderen beiden.
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
  Exit2d();
End;

End.

