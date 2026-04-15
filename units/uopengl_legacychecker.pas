(******************************************************************************)
(* uopengl_legacychecker.pas                                       03.04.2026 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Gives a helper function to more easy check if a application  *)
(*               uses old legacy OpenGL functions.                            *)
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
(*               0.02 - ADD CHECK_FORWARD_COMPATIBLE                          *)
(*                                                                            *)
(******************************************************************************)
Unit uopengl_legacychecker;

{$MODE ObjFPC}{$H+}

Interface

(*
 * If enabled, than also checks that are deprecated but usable on some
 * drivers are checked.
 *)
{$DEFINE CHECK_FORWARD_COMPATIBLE}

Uses
  Classes, SysUtils, dglOpenGL;

Type
  TErrorInfoCallback = Procedure(Severity: GLuint; aMessage: String);

  (*
   * The following function shall help porting legacy code to new OpenGL v3.3
   * by calling this function during MakeCurrent you register a callback that is
   * called, whenever a "old" legacy function is called.
   * When placing a breakpoint in the callback you will be able to stacktrace
   * the source of the call.
   *)
Procedure RegisterLegacyCheckerCallback(Const aCallback: TErrorInfoCallback);

(*
 * If you want to use the KHR_debug debug system you also need to set:
 *
 *  OpenGLControl1.DebugContext := True;
 *
 * During Mainform.create and call
 *
 * ReActivateKHRDebug when finally activating Rendering in MakeCurrent
 *)
Procedure ReActivateKHRDebug;

Implementation

Var
  fCallback: TErrorInfoCallback = Nil; // Storage for the registered Callback ;)

  glVertex2dcapture: TglVertex2d;
  glVertex2fcapture: TglVertex2f;
  glVertex2icapture: TglVertex2i;
  glVertex2ivcapture: TglVertex2iv;
  glVertex2dvcapture: TglVertex2dv;
  glVertex2fvcapture: TglVertex2fv;

  glVertex3fcapture: TglVertex3f;
  glVertex3dcapture: TglVertex3d;
  glVertex3icapture: TglVertex3i;
  glVertex3fvcapture: TglVertex3fv;
  glVertex3ivcapture: TglVertex3iv;
  glVertex3dvcapture: TglVertex3dv;

  glVertex4fcapture: TglVertex4f;
  glVertex4icapture: TglVertex4i;
  glVertex4dcapture: TglVertex4d;
  glVertex4fvcapture: TglVertex4fv;
  glVertex4ivcapture: TglVertex4iv;
  glVertex4dvcapture: TglVertex4dv;

  glColor3fcapture: TglColor3f;
  glColor3icapture: TglColor3i;
  glColor3ubcapture: TglColor3ub;
  glColor3fvcapture: TglColor3fv;
  glColor3ivcapture: TglColor3iv;
  glColor3ubvcapture: TglColor3ubv;

  glColor4fcapture: TglColor4f;
  glColor4ubcapture: TglColor4ub;
  glColor4icapture: TglColor4i;
  glColor4fvcapture: TglColor4fv;
  glColor4ivcapture: TglColor4iv;
  glColor4ubvcapture: TglColor4ubv;

  glTexCoord2dcapture: TglTexCoord2d;
  glTexCoord2icapture: TglTexCoord2i;
  glTexCoord2fcapture: TglTexCoord2f;
  glTexCoord2fvcapture: TglTexCoord2fv;
  glTexCoord2ivcapture: TglTexCoord2iv;
  glTexCoord2dvcapture: TglTexCoord2dv;

  glNormal3dcapture: TglNormal3d;
  glNormal3icapture: TglNormal3i;
  glNormal3fcapture: TglNormal3f;
  glNormal3fvcapture: TglNormal3fv;
  glNormal3ivcapture: TglNormal3iv;
  glNormal3dvcapture: TglNormal3dv;

  glBegincapture: TglBegin;
  glEndcapture: TglEnd;
  glPushMatrixcapture: TglPushMatrix;
  glPopMatrixcapture: TglPopMatrix;
  glPushAttribcapture: TglPushAttrib;
  glPopAttribcapture: TglPopAttrib;
  glMatrixModecapture: TglMatrixMode;
  glLoadIdentitycapture: TglLoadIdentity;
  glLoadMatrixfcapture: TglLoadMatrixf;
  glMultMatrixfcapture: TglMultMatrixf;
  glRotatefcapture: TglRotatef;
  glTranslatefcapture: TglTranslatef;
  glScalefcapture: TglScalef;
  glOrthocapture: TglOrtho;
  glFrustumcapture: TglFrustum;

  glEnableClientStatecapture: TglEnableClientState;
  glDisableClientStatecapture: TglDisableClientState;
  glVertexPointercapture: TglVertexPointer;
  glColorPointercapture: TglColorPointer;
  glNormalPointercapture: TglNormalPointer;
  glTexCoordPointercapture: TglTexCoordPointer;

  glNewListcapture: TglNewList;
  glEndListcapture: TglEndList;
  glCallListcapture: TglCallList;
  glGenListscapture: TglGenLists;
  glDeleteListscapture: TglDeleteLists;

  glTexEnvcapture: TglTexEnvf;

  (*
   * Hier werden nur Ab und An Warnungen generiert, denn bestimmte Enums sind
   * nun auch nicht mehr erlaubt!
   *)
  glEnablecapture: TglEnable;
  glDisablecapture: TglDisable;
  glIsEnabledcapture: TglIsEnabled;

{$IFDEF CHECK_FORWARD_COMPATIBLE}
  glLineWidthCapture: TglLineWidth;
  glPointSizeCapture: TglPointSize;
{$ENDIF}

Procedure glVertex2fvWatcher(Const v: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex2fvWatcher');
  glVertex2fvcapture(v);
End;

Procedure glVertex3fvWatcher(Const v: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex3fvWatcher');
  glVertex3fvcapture(v);
End;

Procedure glVertex4fvWatcher(Const v: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex4fvWatcher');
  glVertex4fvcapture(v);
End;

Procedure glVertex2ivWatcher(Const v: PGLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex2ivWatcher');
  glVertex2ivcapture(v);
End;

Procedure glVertex3ivWatcher(Const v: PGLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex3ivWatcher');
  glVertex3ivcapture(v);
End;

Procedure glVertex4ivWatcher(Const v: PGLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex4ivWatcher');
  glVertex4ivcapture(v);
End;

Procedure glVertex2dvWatcher(Const v: PGLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex2dvWatcher');
  glVertex2dvcapture(v);
End;

Procedure glVertex3dvWatcher(Const v: PGLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex3dvWatcher');
  glVertex3dvcapture(v);
End;

Procedure glVertex4dvWatcher(Const v: PGLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex4dvWatcher');
  glVertex4dvcapture(v);
End;

Procedure glColor3fvWatcher(Const v: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor3fvWatcher');
  glColor3fvcapture(v);
End;

Procedure glColor4fvWatcher(Const v: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor4fvWatcher');
  glColor4fvcapture(v);
End;

Procedure glColor3ivWatcher(Const v: PGLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor3ivWatcher');
  glColor3ivcapture(v);
End;

Procedure glColor4ivWatcher(Const v: PGLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor4ivWatcher');
  glColor4ivcapture(v);
End;

Procedure glColor3ubvWatcher(Const v: PGLubyte){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor3ubvWatcher');
  glColor3ubvcapture(v);
End;

Procedure glColor4ubvWatcher(Const v: PGLubyte){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor4ubvWatcher');
  glColor4ubvcapture(v);
End;

Procedure glTexCoord2fvWatcher(Const v: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTexCoord2fvWatcher');
  glTexCoord2fvcapture(v);
End;

Procedure glTexCoord2ivWatcher(Const v: PGLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTexCoord2ivWatcher');
  glTexCoord2ivcapture(v);
End;

Procedure glTexCoord2dvWatcher(Const v: PGLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTexCoord2dvWatcher');
  glTexCoord2dvcapture(v);
End;

Procedure glNormal3fvWatcher(Const v: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glNormal3fvWatcher');
  glNormal3fvcapture(v);
End;

Procedure glNormal3ivWatcher(Const v: PGLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glNormal3ivWatcher');
  glNormal3ivcapture(v);
End;

Procedure glNormal3dvWatcher(Const v: PGLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glNormal3dvWatcher');
  glNormal3dvcapture(v);
End;

Procedure glBeginWatcher(mode: GLenum){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glBeginWatcher');
  glBegincapture(mode);
End;

Procedure glEndWatcher(){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glEndWatcher');
  glEndcapture();
End;

Procedure glVertex2fWatcher(x: GLfloat; y: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex2fWatcher');
  glVertex2fcapture(x, y);
End;

Procedure glVertex3fWatcher(x: GLfloat; y: GLfloat; z: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex3fWatcher');
  glVertex3fcapture(x, y, z);
End;

Procedure glVertex4fWatcher(x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex4fWatcher');
  glVertex4fcapture(x, y, z, w);
End;

Procedure glColor3fWatcher(red: GLfloat; green: GLfloat; blue: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor3fWatcher');
  glColor3fcapture(red, green, blue);
End;

Procedure glColor4fWatcher(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor4fWatcher');
  glColor4fcapture(red, green, blue, alpha);
End;

Procedure glNormal3fWatcher(nx: GLfloat; ny: GLfloat; nz: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glNormal3fWatcher');
  glNormal3fcapture(nx, ny, nz);
End;

Procedure glTexCoord2fWatcher(s: GLfloat; t: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTexCoord2fWatcher');
  glTexCoord2fcapture(s, t);
End;

Procedure glPushMatrixWatcher(){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glPushMatrixWatcher');
  glPushMatrixcapture();
End;

Procedure glPopMatrixWatcher(){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glPopMatrixWatcher');
  glPopMatrixcapture();
End;

Procedure glPushAttribWatcher(mask: GLbitfield){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glPushAttribWatcher');
  glPushAttribcapture(mask);
End;

Procedure glPopAttribWatcher(){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glPopAttribWatcher');
  glPopAttribcapture();
End;

Procedure glMatrixModeWatcher(mode: GLenum){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glMatrixModeWatcher');
  glMatrixModecapture(mode);
End;

Procedure glLoadIdentityWatcher(){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glLoadIdentityWatcher');
  glLoadIdentitycapture();
End;

Procedure glLoadMatrixfWatcher(Const m: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glLoadMatrixfWatcher');
  glLoadMatrixfcapture(m);
End;

Procedure glMultMatrixfWatcher(Const m: PGLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glMultMatrixfWatcher');
  glMultMatrixfcapture(m);
End;

Procedure glRotatefWatcher(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glRotatefWatcher');
  glRotatefcapture(angle, x, y, z);
End;

Procedure glTranslatefWatcher(x: GLfloat; y: GLfloat; z: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTranslatefWatcher');
  glTranslatefcapture(x, y, z);
End;

Procedure glScalefWatcher(x: GLfloat; y: GLfloat; z: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glScalefWatcher');
  glScalefcapture(x, y, z);
End;

Procedure glOrthoWatcher(left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glOrthoWatcher');
  glOrthocapture(left, right, bottom, top, zNear, zFar);
End;

Procedure glFrustumWatcher(left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glFrustumWatcher');
  glFrustumcapture(left, right, bottom, top, zNear, zFar);
End;

Procedure glEnableClientStateWatcher(array_: GLenum){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glEnableClientStateWatcher');
  glEnableClientStatecapture(array_);
End;

Procedure glDisableClientStateWatcher(array_: GLenum){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glDisableClientStateWatcher');
  glDisableClientStatecapture(array_);
End;

Procedure glVertexPointerWatcher(size: GLint; _type: GLenum; stride: GLsizei; Const _pointer: PGLvoid){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertexPointerWatcher');
  glVertexPointercapture(size, _type, stride, _pointer);
End;

Procedure glColorPointerWatcher(size: GLint; _type: GLenum; stride: GLsizei; Const _pointer: PGLvoid){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColorPointerWatcher');
  glColorPointercapture(size, _type, stride, _pointer);
End;

Procedure glNormalPointerWatcher(_type: GLenum; stride: GLsizei; Const _pointer: PGLvoid){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glNormalPointerWatcher');
  glNormalPointercapture(_type, stride, _pointer);
End;

Procedure glTexCoordPointerWatcher(size: GLint; _type: GLenum; stride: GLsizei; Const _pointer: PGLvoid){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTexCoordPointerWatcher');
  glTexCoordPointercapture(size, _type, stride, _pointer);
End;

Procedure glNewListWatcher(list: GLuint; mode: GLenum){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glNewListWatcher');
  glNewListcapture(list, mode);
End;

Procedure glEndListWatcher(){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glEndListWatcher');
  glEndListcapture();
End;

Procedure glCallListWatcher(list: GLuint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glCallListWatcher');
  glCallListcapture(list);
End;

Function glGenListsWatcher(range: GLsizei): GLuint{$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glGenListsWatcher');
  result := glGenListscapture(range);
End;

Procedure glDeleteListsWatcher(list: GLuint; range: GLsizei){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glDeleteListsWatcher');
  glDeleteListscapture(list, range);
End;

Procedure glTexEnvfWatcher(target: GLenum; pname: GLenum; param: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTexEnvfWatcher');
  glTexEnvcapture(target, pname, param);
End;

Procedure glVertex2iWatcher(x: GLint; y: GLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex2iWatcher');
  glVertex2icapture(x, y);
End;

Procedure glVertex3iWatcher(x: GLint; y: GLint; z: GLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex3iWatcher');
  glVertex3icapture(x, y, z);
End;

Procedure glVertex4iWatcher(x: GLint; y: GLint; z: GLint; w: GLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex4iWatcher');
  glVertex4icapture(x, y, z, w);
End;

Procedure glVertex2dWatcher(x: GLdouble; y: GLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex2dWatcher');
  glVertex2dcapture(x, y);
End;

Procedure glVertex3dWatcher(x: GLdouble; y: GLdouble; z: GLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex3dWatcher');
  glVertex3dcapture(x, y, z);
End;

Procedure glVertex4dWatcher(x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glVertex4dWatcher');
  glVertex4dcapture(x, y, z, w);
End;

Procedure glColor3ubWatcher(r: GLubyte; g: GLubyte; b: GLubyte){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor3ubWatcher');
  glColor3ubcapture(r, g, b);
End;

Procedure glColor4ubWatcher(r: GLubyte; g: GLubyte; b: GLubyte; a: GLubyte){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor4ubWatcher');
  glColor4ubcapture(r, g, b, a);
End;

Procedure glColor3iWatcher(r: GLint; g: GLint; b: GLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor3iWatcher');
  glColor3icapture(r, g, b);
End;

Procedure glColor4iWatcher(r: GLint; g: GLint; b: GLint; a: GLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glColor4iWatcher');
  glColor4icapture(r, g, b, a);
End;

Procedure glTexCoord2dWatcher(s: GLdouble; t: GLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTexCoord2dWatcher');
  glTexCoord2dcapture(s, t);
End;

Procedure glTexCoord2iWatcher(s: GLint; t: GLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glTexCoord2iWatcher');
  glTexCoord2icapture(s, t);
End;

Procedure glNormal3dWatcher(nx: GLdouble; ny: GLdouble; nz: GLdouble){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glNormal3dWatcher');
  glNormal3dcapture(nx, ny, nz);
End;

Procedure glNormal3iWatcher(nx: GLint; ny: GLint; nz: GLint){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  fCallback(0, 'glNormal3iWatcher');
  glNormal3icapture(nx, ny, nz);
End;

Function IsLegacyEnum(cap: GLenum): Boolean;
Begin
  // These enums are part of the Fixed-Function pipeline and are invalid
  // in an OpenGL Core Profile context -> they generate GL_INVALID_ENUM (1280).
  Result :=
    (cap = GL_TEXTURE_1D) Or
    (cap = GL_TEXTURE_2D) Or
    (cap = GL_LIGHTING) Or
    (cap = GL_LIGHT0) Or (cap = GL_LIGHT0 + 1) Or (cap = GL_LIGHT0 + 2) Or
    (cap = GL_LIGHT0 + 3) Or (cap = GL_LIGHT0 + 4) Or (cap = GL_LIGHT0 + 5) Or
    (cap = GL_LIGHT0 + 6) Or (cap = GL_LIGHT0 + 7) Or
    (cap = GL_FOG) Or
    (cap = GL_ALPHA_TEST) Or
    (cap = GL_COLOR_MATERIAL) Or
    (cap = GL_NORMALIZE) Or
    (cap = GL_RESCALE_NORMAL) Or
    (cap = GL_LINE_STIPPLE) Or
    (cap = GL_POLYGON_STIPPLE) Or
    (cap = GL_POINT_SMOOTH) Or
    (cap = GL_COLOR_SUM) Or
    (cap = GL_TEXTURE_GEN_S) Or
    (cap = GL_TEXTURE_GEN_T) Or
    (cap = GL_TEXTURE_GEN_T + 1) Or // GL_TEXTURE_GEN_R
  (cap = GL_TEXTURE_GEN_T + 2) Or // GL_TEXTURE_GEN_Q
  (cap = GL_CLIP_PLANE0) Or (cap = GL_CLIP_PLANE0 + 1) Or
    (cap = GL_CLIP_PLANE0 + 2) Or (cap = GL_CLIP_PLANE0 + 3) Or
    (cap = GL_CLIP_PLANE0 + 4) Or (cap = GL_CLIP_PLANE0 + 5);
End;

Procedure glEnableWatcher(cap: GLenum){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  If IsLegacyEnum(cap) Then Begin
    fCallback(0, 'glEnableWatcher');
  End;
  glEnablecapture(cap);
End;

Procedure glDisableWatcher(cap: GLenum){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  If IsLegacyEnum(cap) Then Begin
    fCallback(0, 'glDisableWatcher');
  End;
  glDisablecapture(cap);
End;

Function IsEnabledWatcher(cap: GLenum): GLboolean{$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  If IsLegacyEnum(cap) Then Begin
    fCallback(0, 'IsEnabledWatcher');
  End;
  result := glIsEnabledcapture(cap);
End;

{$IFDEF CHECK_FORWARD_COMPATIBLE}

Procedure glLineWidthWatcher(width: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  If width > 1.0 Then Begin
    (*
     * deprecated since OpenGL 3.0 (2008)
     *)
    fCallback(0, 'glLineWidthWatcher');
  End;
  glLineWidthCapture(width);
End;

Procedure glPointSizeWatcher(size: GLfloat){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  If Not glIsEnabled(GL_PROGRAM_POINT_SIZE) Then Begin
    fCallback(0, 'glPointSizeWatcher');
  End;
  glPointSizeCapture(size);
End;
{$ENDIF}

// GL_KHR_debug driver callback: forwards all non-notification messages to fCallback.
// Must be a plain cdecl procedure (no method, no closure) because it is passed
// directly to the OpenGL driver via glDebugMessageCallback.

Procedure glKHRDebugCallbackProc(source: GLEnum; type_: GLEnum; id: GLUInt;
  severity: GLUInt; length: GLsizei; Const message_: PGLchar;
  userParam: PGLvoid){$IFDEF Windows} stdcall{$ELSE} cdecl{$ENDIF};
Begin
  // GL_DEBUG_SEVERITY_NOTIFICATION is purely informational – skip it to reduce noise.
  If severity <> GL_DEBUG_SEVERITY_NOTIFICATION Then Begin
    fCallback(severity, message_);
  End;
End;

Procedure ReActivateKHRDebug;
Begin
  If Not Assigned(glDebugMessageCallback) Then Exit;
  // Use the captured (original) glEnable so we bypass the watcher
  If Assigned(glEnablecapture) Then Begin
    glEnablecapture(GL_DEBUG_OUTPUT);
    glEnablecapture(GL_DEBUG_OUTPUT_SYNCHRONOUS);
  End
  Else Begin
    glEnable(GL_DEBUG_OUTPUT);
    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
  End;
  glDebugMessageCallback(@glKHRDebugCallbackProc, Nil);
  If Assigned(glDebugMessageControl) Then
    glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, Nil, GL_TRUE);
End;

Procedure RegisterLegacyCheckerCallback(Const aCallback: TErrorInfoCallback);
Begin
  fCallback := aCallback;
  // Here is a list of hopefully "All" gl* calls which are not allowed when not
  // using OpenGL ver 3.3 and up
  glBegincapture := glBegin;
  glBegin := @glBeginWatcher;
  glEndcapture := glEnd;
  glEnd := @glEndWatcher;
  glVertex2fcapture := glVertex2f;
  glVertex2f := @glVertex2fWatcher;
  glVertex3fcapture := glVertex3f;
  glVertex3f := @glVertex3fWatcher;
  glVertex4fcapture := glVertex4f;
  glVertex4f := @glVertex4fWatcher;
  glColor3fcapture := glColor3f;
  glColor3f := @glColor3fWatcher;
  glColor4fcapture := glColor4f;
  glColor4f := @glColor4fWatcher;
  glNormal3fcapture := glNormal3f;
  glNormal3f := @glNormal3fWatcher;
  glTexCoord2fcapture := glTexCoord2f;
  glTexCoord2f := @glTexCoord2fWatcher;
  glPushMatrixcapture := glPushMatrix;
  glPushMatrix := @glPushMatrixWatcher;
  glPopMatrixcapture := glPopMatrix;
  glPopMatrix := @glPopMatrixWatcher;
  glPushAttribcapture := glPushAttrib;
  glPushAttrib := @glPushAttribWatcher;
  glPopAttribcapture := glPopAttrib;
  glPopAttrib := @glPopAttribWatcher;
  glMatrixModecapture := glMatrixMode;
  glMatrixMode := @glMatrixModeWatcher;
  glLoadIdentitycapture := glLoadIdentity;
  glLoadIdentity := @glLoadIdentityWatcher;
  glLoadMatrixfcapture := glLoadMatrixf;
  glLoadMatrixf := @glLoadMatrixfWatcher;
  glMultMatrixfcapture := glMultMatrixf;
  glMultMatrixf := @glMultMatrixfWatcher;
  glRotatefcapture := glRotatef;
  glRotatef := @glRotatefWatcher;
  glTranslatefcapture := glTranslatef;
  glTranslatef := @glTranslatefWatcher;
  glScalefcapture := glScalef;
  glScalef := @glScalefWatcher;
  glOrthocapture := glOrtho;
  glOrtho := @glOrthoWatcher;
  glFrustumcapture := glFrustum;
  glFrustum := @glFrustumWatcher;
  glEnableClientStatecapture := glEnableClientState;
  glEnableClientState := @glEnableClientStateWatcher;
  glDisableClientStatecapture := glDisableClientState;
  glDisableClientState := @glDisableClientStateWatcher;
  glVertexPointercapture := glVertexPointer;
  glVertexPointer := @glVertexPointerWatcher;
  glColorPointercapture := glColorPointer;
  glColorPointer := @glColorPointerWatcher;
  glNormalPointercapture := glNormalPointer;
  glNormalPointer := @glNormalPointerWatcher;
  glTexCoordPointercapture := glTexCoordPointer;
  glTexCoordPointer := @glTexCoordPointerWatcher;
  glNewListcapture := glNewList;
  glNewList := @glNewListWatcher;
  glEndListcapture := glEndList;
  glEndList := @glEndListWatcher;
  glCallListcapture := glCallList;
  glCallList := @glCallListWatcher;
  glGenListscapture := glGenLists;
  glGenLists := @glGenListsWatcher;
  glDeleteListscapture := glDeleteLists;
  glDeleteLists := @glDeleteListsWatcher;
  glTexEnvcapture := glTexEnvf;
  glTexEnvf := @glTexEnvfWatcher;
  glVertex2icapture := glVertex2i;
  glVertex2i := @glVertex2iWatcher;
  glVertex3icapture := glVertex3i;
  glVertex3i := @glVertex3iWatcher;
  glVertex4icapture := glVertex4i;
  glVertex4i := @glVertex4iWatcher;
  glVertex2dcapture := glVertex2d;
  glVertex2d := @glVertex2dWatcher;
  glVertex3dcapture := glVertex3d;
  glVertex3d := @glVertex3dWatcher;
  glVertex4dcapture := glVertex4d;
  glVertex4d := @glVertex4dWatcher;
  glColor3ubcapture := glColor3ub;
  glColor3ub := @glColor3ubWatcher;
  glColor4ubcapture := glColor4ub;
  glColor4ub := @glColor4ubWatcher;
  glColor3icapture := glColor3i;
  glColor3i := @glColor3iWatcher;
  glColor4icapture := glColor4i;
  glColor4i := @glColor4iWatcher;
  glTexCoord2dcapture := glTexCoord2d;
  glTexCoord2d := @glTexCoord2dWatcher;
  glTexCoord2icapture := glTexCoord2i;
  glTexCoord2i := @glTexCoord2iWatcher;
  glNormal3dcapture := glNormal3d;
  glNormal3d := @glNormal3dWatcher;
  glNormal3icapture := glNormal3i;
  glNormal3i := @glNormal3iWatcher;

  glVertex2fvcapture := glVertex2fv;
  glVertex2fv := @glVertex2fvWatcher;
  glVertex3fvcapture := glVertex3fv;
  glVertex3fv := @glVertex3fvWatcher;
  glVertex4fvcapture := glVertex4fv;
  glVertex4fv := @glVertex4fvWatcher;

  glVertex2ivcapture := glVertex2iv;
  glVertex2iv := @glVertex2ivWatcher;
  glVertex3ivcapture := glVertex3iv;
  glVertex3iv := @glVertex3ivWatcher;
  glVertex4ivcapture := glVertex4iv;
  glVertex4iv := @glVertex4ivWatcher;

  glVertex2dvcapture := glVertex2dv;
  glVertex2dv := @glVertex2dvWatcher;
  glVertex3dvcapture := glVertex3dv;
  glVertex3dv := @glVertex3dvWatcher;
  glVertex4dvcapture := glVertex4dv;
  glVertex4dv := @glVertex4dvWatcher;

  glColor3fvcapture := glColor3fv;
  glColor3fv := @glColor3fvWatcher;
  glColor4fvcapture := glColor4fv;
  glColor4fv := @glColor4fvWatcher;

  glColor3ivcapture := glColor3iv;
  glColor3iv := @glColor3ivWatcher;
  glColor4ivcapture := glColor4iv;
  glColor4iv := @glColor4ivWatcher;

  glColor3ubvcapture := glColor3ubv;
  glColor3ubv := @glColor3ubvWatcher;
  glColor4ubvcapture := glColor4ubv;
  glColor4ubv := @glColor4ubvWatcher;

  glTexCoord2fvcapture := glTexCoord2fv;
  glTexCoord2fv := @glTexCoord2fvWatcher;
  glTexCoord2ivcapture := glTexCoord2iv;
  glTexCoord2iv := @glTexCoord2ivWatcher;
  glTexCoord2dvcapture := glTexCoord2dv;
  glTexCoord2dv := @glTexCoord2dvWatcher;

  glNormal3fvcapture := glNormal3fv;
  glNormal3fv := @glNormal3fvWatcher;
  glNormal3ivcapture := glNormal3iv;
  glNormal3iv := @glNormal3ivWatcher;
  glNormal3dvcapture := glNormal3dv;
  glNormal3dv := @glNormal3dvWatcher;

  glEnablecapture := glEnable;
  glEnable := @glEnableWatcher;
  glDisablecapture := glDisable;
  glDisable := @glDisableWatcher;
  glIsEnabledcapture := glIsEnabled;
  glIsEnabled := @IsEnabledWatcher;

{$IFDEF CHECK_FORWARD_COMPATIBLE}
  glLineWidthCapture := glLineWidth;
  glLineWidth := @glLineWidthWatcher;

  glPointSizeCapture := glPointSize;
  glPointSize := @glPointSizeWatcher;
{$ENDIF}

  // If OpenGL 4.3+ is available, additionally activate the driver-level
  // GL_KHR_debug system. It fires synchronously at the exact offending call
  // and captures ALL error sources (not only legacy function calls), making
  // it much more precise than glGetError() polling.
  If Assigned(glDebugMessageCallback) Then Begin
    glEnable(GL_DEBUG_OUTPUT);
    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS); // callback fires in-call, stack trace is valid
    glDebugMessageCallback(@glKHRDebugCallbackProc, Nil);
    // Explicitly enable ALL sources/types/severities.
    // Without this many drivers silently filter everything by default,
    // so the callback is registered but never invoked.
    If Assigned(glDebugMessageControl) Then
      glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, Nil, GL_TRUE);
  End;
End;

End.

