(******************************************************************************)
(* uopengl_shaderprimitives.pas                                    03.04.2026 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Helper functions to port "old" OpenGl legacy code to new     *)
(*               OpenGL Shaded code, use together with                        *)
(*                 uOpenGLGraphikEngine.pas                                   *)
(*               ! Attention !                                                *)
(*               This unit tries to reduce porting effort and is not          *)
(*               "performant" or how you should use shaders in new projects   *)
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
(*               0.02 - Fix GL_INVALID_OPERATION (1282) in Core Profile by    *)
(*                      adding VAO (required since OpenGL 3.2 Core)           *)
(*               0.03 - Enable glLinewidth, glPointSize emulation             *)
(*                                                                            *)
(******************************************************************************)

Unit uopengl_shaderprimitives;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, dglOpenGL, uvectormath;

(*
 * If set this "cuts" the edges of rendered lines to the "true"
 * old behavior of Legacy OpenGl, but as this is looking "ugly" it is
 * off by default.
 *)
{.$DEFINE EMULATE_LEGACY_LINE_RASTERIZATION}

(*
 * Call once during Make Current
 *)
Procedure OpenGL_ShaderPrimitives_InitializeShaderSystem;

(*
 * Call once during destroy
 *)
Procedure OpenGL_ShaderPrimitives_FinalizeShaderSystem;

(*
 * replacement for:
 *
 *  glColor3f(..); // -- Optional, but usually done..
 *  glBegin(aMode);
 *  glvertex(..);
 *  glEnd;
 *
 * How to use:
 *
 * Case 1:
 *  UseColorShader();
 *  SetShaderColor(..); // -- Optional, but usually done..
 *  glShaderBegin(aMode);
 *  glShaderRender([
 *   v3(), ...
 *   ]);
 *  glShaderEnd();
 *  UseTextureShader; // Switch back to Render Textures
 *
 * Case 2:
 *  UseColorShader();
 *  SetShaderColor(..); // -- Optional, but usually done..
 *  glShaderBegin(aMode);
 *  glShaderVertex(..); // As often as needed
 *  glShaderEnd();
 *  UseTextureShader; // Switch back to Render Textures
 *)

Procedure glShaderBegin(aMode: GLenum);
Procedure glShaderRender(Const aData: Array Of TVector3);
Procedure glShaderVertex(Const aData: TVector3); overload;
Procedure glShaderVertex(x, y, z: Single); overload;
Procedure glShaderEnd();

(*
 * glLineWidth is deprecated since OpenGL 3.0
 *)
Procedure glShaderLineWidth(width: GLfloat);

(*
 * glPointSize is not fully supported anymore since ?
 *)
Procedure glShaderPointSize(size: GLfloat);

Implementation

Const
  VertexBufferBlockSize = 1024;

Type
  PVector3 = ^TVector3;

Var
  ShaderVAO: GLuint;
  ShaderVBO: GLint;
  aShaderMode: GLenum;
  RenderVertexBuffer: Array Of TVector3;
  RenderVertexBufferCnt: integer;
  ShaderLineWidth: GLfloat;
  ShaderPointSize: GLfloat;

Procedure OpenGL_ShaderPrimitives_InitializeShaderSystem;
Begin
  If ShaderVAO = 0 Then
    glGenVertexArrays(1, @ShaderVAO);
  If ShaderVBO = 0 Then
    glGenBuffers(1, @ShaderVBO);
  ShaderLineWidth := 1;
  ShaderPointSize := 1;
End;

Procedure OpenGL_ShaderPrimitives_FinalizeShaderSystem;
Begin
  If ShaderVBO <> 0 Then
    glDeleteBuffers(1, @ShaderVBO);
  ShaderVBO := 0;
  If ShaderVAO <> 0 Then
    glDeleteVertexArrays(1, @ShaderVAO);
  ShaderVAO := 0;
End;

Procedure RenderLineSegment(Const v1, v2: TVector3);
Var
  Vertices: Array[0..3] Of TVector3;
  dx, dy, len, ox, oy: Single;
Begin
  // Berechnet 4 Eckpunkte einer ShaderLineWidth "breiten" Linie, die bei v1 startet und v2 endet
  // So wie das Früher der OpenGL Legacy code gemacht hätte, ShaderLineWidth ist Garantiert > 1 !
  dx := v2.x - v1.x;
  dy := v2.y - v1.y;
  len := sqrt(dx * dx + dy * dy);
  ox := (-dy / len) * (ShaderLineWidth / 2);
  oy := (dx / len) * (ShaderLineWidth / 2);

{$IFDEF EMULATE_LEGACY_LINE_RASTERIZATION}
  // Legacy-style: Offset nur in X oder Y, je nach Hauptachse
  If abs(dx) >= abs(dy) Then Begin
    ox := 0;
    oy := ShaderLineWidth / 2; // eher horizontal → Y-Offset
  End
  Else Begin
    ox := ShaderLineWidth / 2;
    oy := 0; // eher vertikal → X-Offset
  End;
{$ENDIF}
  Vertices[0] := v3(v1.x + ox, v1.y + oy, v1.z);
  Vertices[1] := v3(v1.x - ox, v1.y - oy, v1.z);
  Vertices[2] := v3(v2.x + ox, v2.y + oy, v2.z);
  Vertices[3] := v3(v2.x - ox, v2.y - oy, v2.z);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TVector3) * 4, @Vertices[0], GL_DYNAMIC_DRAW);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
End;

Procedure RenderPoint(Const v: TVector3);
Var
  Vertices: Array[0..3] Of TVector3;
Begin
  Vertices[0] := v3(v.x - ShaderPointSize / 2, v.y - ShaderPointSize / 2, v.z);
  Vertices[1] := v3(v.x - ShaderPointSize / 2, v.y + ShaderPointSize / 2, v.z);
  Vertices[3] := v3(v.x + ShaderPointSize / 2, v.y + ShaderPointSize / 2, v.z);
  Vertices[2] := v3(v.x + ShaderPointSize / 2, v.y - ShaderPointSize / 2, v.z);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TVector3) * 4, @Vertices[0], GL_DYNAMIC_DRAW);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
End;

Procedure DoShaderData(Const aData: PVector3; aLen: Integer);
Var
  i: integer;
  pStart: PVector3;
Begin
  If (aShaderMode In [GL_LINES, GL_LINE_LOOP, GL_LINE_STRIP]) Then Begin
    // Wir Rendern immer 1 LinienSegment
    pStart := aData;
    If aShaderMode = GL_LINES Then Begin
      For i := 0 To aLen Div 2 - 1 Do Begin
        RenderLineSegment(pStart^, (pStart + 1)^);
        inc(pStart);
        inc(pStart);
      End;
    End
    Else Begin // GL_LINE_LOOP, GL_LINE_STRIP
      For i := 0 To aLen - 2 Do Begin
        RenderLineSegment(pStart^, (pStart + 1)^);
        inc(pStart);
      End;
      If aShaderMode = GL_LINE_LOOP Then
        RenderLineSegment(pStart^, (aData)^);
    End;
    exit;
  End;
  If (aShaderMode = GL_POINTS) Then Begin
    For i := 0 To aLen - 1 Do Begin
      RenderPoint(aData[i]);
    End;
    exit;
  End;
  //  If (ShaderLineWidth <> 1) Then Begin
  //    Raise exception.Create('glShaderLineWidth <> 1, only supported / emulated for (GL_LINES, GL_LINE_LOOP)');
  //  End;
  //  If (ShaderPointSize <> 1) Then Begin
  //    Raise exception.Create('glShaderPointSize <> 1, only supported / emulated for (GL_POINTS)');
  //  End;
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TVector3) * aLen, aData, GL_DYNAMIC_DRAW);
  glDrawArrays(aShaderMode, 0, aLen);
End;

Procedure glShaderBegin(aMode: GLenum);
Begin
  If ShaderVAO = 0 Then Begin
    Raise exception.create('Error, not initialized, did you call: OpenGL_ShaderPrimitives_InitializeShaderSystem ?');
  End;
  If aMode = GL_QUADS Then Begin
    Raise exception.create('Error, GL_QUADS was removed in OpenGl Core 3.2');
  End;
  If aMode = GL_QUAD_STRIP Then Begin
    Raise exception.create('Error, GL_QUAD_STRIP was removed in OpenGl Core 3.2');
  End;
  If aMode = GL_POLYGON Then Begin
    Raise exception.create('Error, GL_POLYGON was removed in OpenGl Core 3.2');
  End;
  aShaderMode := aMode;
  glBindVertexArray(ShaderVAO);
  glBindBuffer(GL_ARRAY_BUFFER, ShaderVBO);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, Nil);
  RenderVertexBufferCnt := 0;
End;

Procedure glShaderRender(Const aData: Array Of TVector3);
Begin
  DoShaderData(@aData[0], length(aData));
End;

Procedure glShaderVertex(Const aData: TVector3);
Begin
  RenderVertexBuffer[RenderVertexBufferCnt] := aData;
  inc(RenderVertexBufferCnt);
  If RenderVertexBufferCnt > high(RenderVertexBuffer) Then Begin
    setlength(RenderVertexBuffer, length(RenderVertexBuffer) + VertexBufferBlockSize);
  End;
End;

Procedure glShaderVertex(x, y, z: Single);
Begin
  glShaderVertex(v3(x, y, z));
End;

Procedure glShaderEnd();
Begin
  // Die glShaderRender routine wurde nicht benutzt, also hier den VertexBuffer übergeben
  If RenderVertexBufferCnt <> 0 Then Begin
    DoShaderData(@RenderVertexBuffer[0], RenderVertexBufferCnt);
    RenderVertexBufferCnt := 0;
  End;
  glDisableVertexAttribArray(0);
  glBindVertexArray(0);
End;

Procedure glShaderLineWidth(width: GLfloat);
Begin
  ShaderLineWidth := width;
End;

Procedure glShaderPointSize(size: GLfloat);
Begin
  ShaderPointSize := size;
End;

Initialization
  ShaderVAO := 0;
  RenderVertexBuffer := Nil;
  setlength(RenderVertexBuffer, VertexBufferBlockSize);
  RenderVertexBufferCnt := 0;

Finalization
  setlength(RenderVertexBuffer, 0);

End.

