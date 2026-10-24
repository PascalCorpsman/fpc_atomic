(******************************************************************************)
(* uopengl_shaderprimitives.pas                                    03.04.2026 *)
(*                                                                            *)
(* Version     : 0.04                                                         *)
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
(*               0.04 - glShaderEnable for GL_LINE_SMOOTH                     *)
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
Procedure OpenGL_ShaderPrimitives_IncreaseVertexBufferToAtLeast(aNewSize: Integer); // This is a speed optimization function, its call is optional

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
Procedure glShaderVertex(x, y: Single); overload;
Procedure glShaderVertex(x, y, z: Single); overload;
Procedure glShaderEnd();

(*
 * GL_LINE_SMOOTH is emulated and disabled by default. Requires GL_BLEND
 * and the color shader. The smoothing fringe is one object-space unit.
 * Other capabilities are forwarded to glEnable / glDisable.
 *)
Procedure glShaderEnable(aCapability: GLenum);
Procedure glShaderDisable(aCapability: GLenum);

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
  PointBatchSize = 16384;

Type
  PVector3 = ^TVector3;
  TSmoothLineVertex = packed Record
    Position: TVector3;
    Alpha: Single;
  End;
  TLineSection = Array[0..3] Of TSmoothLineVertex;

Var
  ShaderVAO: GLuint;
  ShaderVBO: GLint;
  aShaderMode: GLenum;
  RenderVertexBuffer: Array Of TVector3;
  RenderVertexBufferCnt: integer;
  PointVertexBuffer: Array Of TVector3;
  ShaderLineWidth: GLfloat;
  ShaderPointSize: GLfloat;
  ShaderLineSmooth: Boolean;
  SmoothLinePoints: Array Of TVector3;
  SmoothLineBuffer: Array Of TSmoothLineVertex;

Procedure OpenGL_ShaderPrimitives_InitializeShaderSystem;
Begin
  If ShaderVAO = 0 Then
    glGenVertexArrays(1, @ShaderVAO);
  If ShaderVBO = 0 Then
    glGenBuffers(1, @ShaderVBO);
  ShaderLineWidth := 1;
  ShaderPointSize := 1;
  ShaderLineSmooth := False;
End;

Procedure OpenGL_ShaderPrimitives_IncreaseVertexBufferToAtLeast(
  aNewSize: Integer);
Begin
  If aNewSize > length(RenderVertexBuffer) Then Begin
    setlength(RenderVertexBuffer, aNewSize);
  End;
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
  If len = 0 Then exit;
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

Procedure RenderSmoothLineStrip(Const aData: PVector3; aLen: Integer; Closed: Boolean);
Var
  PointCount, PointIndex, PreviousIndex, NextIndex, VertexCount: Integer;
  InnerRadius, OuterRadius, Coverage, Denominator, OffsetLength: Single;
  DirectionBefore, DirectionAfter, Offset, Center: TVector3;
  FirstSection, PreviousSection, CurrentSection: TLineSection;

  Function Direction(Const StartPoint, EndPoint: TVector3): TVector3;
  Var
    DeltaX, DeltaY, SegmentLength: Single;
  Begin
    DeltaX := EndPoint.x - StartPoint.x;
    DeltaY := EndPoint.y - StartPoint.y;
    SegmentLength := sqrt(DeltaX * DeltaX + DeltaY * DeltaY);
    Result := v3(DeltaX / SegmentLength, DeltaY / SegmentLength, 0);
  End;

  Procedure MakeSection(Const Position, Normal: TVector3; Alpha: Single);
  Var
    Lane: Integer;
    Distance: Single;
  Begin
    For Lane := 0 To 3 Do Begin
      Case Lane Of
        0: Distance := OuterRadius;
        1: Distance := InnerRadius;
        2: Distance := -InnerRadius;
        3: Distance := -OuterRadius;
      End;
      CurrentSection[Lane].Position := v3(Position.x + Normal.x * Distance,
        Position.y + Normal.y * Distance, Position.z);
      If Lane In [1, 2] Then
        CurrentSection[Lane].Alpha := Alpha
      Else
        CurrentSection[Lane].Alpha := 0;
    End;
  End;

  Procedure ConnectSection;
  Var
    Lane: Integer;
  Begin
    For Lane := 0 To 2 Do Begin
      SmoothLineBuffer[VertexCount] := PreviousSection[Lane];
      SmoothLineBuffer[VertexCount + 1] := PreviousSection[Lane + 1];
      SmoothLineBuffer[VertexCount + 2] := CurrentSection[Lane];
      SmoothLineBuffer[VertexCount + 3] := CurrentSection[Lane];
      SmoothLineBuffer[VertexCount + 4] := PreviousSection[Lane + 1];
      SmoothLineBuffer[VertexCount + 5] := CurrentSection[Lane + 1];
      inc(VertexCount, 6);
    End;
    PreviousSection := CurrentSection;
  End;

Begin
  If (aLen < 2) Or (ShaderLineWidth <= 0) Then exit;
  If length(SmoothLinePoints) < aLen Then
    setlength(SmoothLinePoints, aLen);
  PointCount := 0;
  For PointIndex := 0 To aLen - 1 Do Begin
    If PointCount > 0 Then Begin
      If (aData[PointIndex].x = SmoothLinePoints[PointCount - 1].x) And
        (aData[PointIndex].y = SmoothLinePoints[PointCount - 1].y) Then continue;
    End;
    SmoothLinePoints[PointCount] := aData[PointIndex];
    inc(PointCount);
  End;
  If Closed And (PointCount > 1) Then Begin
    If (SmoothLinePoints[0].x = SmoothLinePoints[PointCount - 1].x) And
      (SmoothLinePoints[0].y = SmoothLinePoints[PointCount - 1].y) Then dec(PointCount);
  End;
  If PointCount < 2 Then exit;
  Closed := Closed And (PointCount > 2);
  If length(SmoothLineBuffer) < (PointCount + 1) * 18 Then
    setlength(SmoothLineBuffer, (PointCount + 1) * 18);
  InnerRadius := (ShaderLineWidth - 1) / 2;
  If InnerRadius < 0 Then InnerRadius := 0;
  OuterRadius := (ShaderLineWidth + 1) / 2;
  Coverage := ShaderLineWidth;
  If Coverage > 1 Then Coverage := 1;
  VertexCount := 0;
  For PointIndex := 0 To PointCount - 1 Do Begin
    PreviousIndex := (PointIndex + PointCount - 1) Mod PointCount;
    NextIndex := (PointIndex + 1) Mod PointCount;
    If (Not Closed) And (PointIndex = 0) Then
      DirectionBefore := Direction(SmoothLinePoints[0], SmoothLinePoints[1])
    Else
      DirectionBefore := Direction(SmoothLinePoints[PreviousIndex], SmoothLinePoints[PointIndex]);
    If (Not Closed) And (PointIndex = PointCount - 1) Then
      DirectionAfter := DirectionBefore
    Else
      DirectionAfter := Direction(SmoothLinePoints[PointIndex], SmoothLinePoints[NextIndex]);
    Denominator := 1 + DirectionBefore.x * DirectionAfter.x + DirectionBefore.y * DirectionAfter.y;
    If Denominator > 0.0001 Then Begin
      Offset := v3(-(DirectionBefore.y + DirectionAfter.y) / Denominator,
        (DirectionBefore.x + DirectionAfter.x) / Denominator, 0);
      OffsetLength := sqrt(Offset.x * Offset.x + Offset.y * Offset.y);
      If OffsetLength > 4 Then Begin
        Offset.x := Offset.x * 4 / OffsetLength;
        Offset.y := Offset.y * 4 / OffsetLength;
      End;
    End
    Else
      Offset := v3(-DirectionAfter.y, DirectionAfter.x, 0);
    If (PointIndex = 0) And (Not Closed) Then Begin
      Center := SmoothLinePoints[0];
      Center.x := Center.x - DirectionAfter.x;
      Center.y := Center.y - DirectionAfter.y;
      MakeSection(Center, Offset, 0);
      PreviousSection := CurrentSection;
    End;
    MakeSection(SmoothLinePoints[PointIndex], Offset, Coverage);
    If PointIndex = 0 Then Begin
      FirstSection := CurrentSection;
      If Closed Then PreviousSection := CurrentSection
      Else ConnectSection;
    End
    Else ConnectSection;
  End;
  If Closed Then
    CurrentSection := FirstSection
  Else Begin
    Center := SmoothLinePoints[PointCount - 1];
    Center.x := Center.x + DirectionAfter.x;
    Center.y := Center.y + DirectionAfter.y;
    MakeSection(Center, Offset, 0);
  End;
  ConnectSection;
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, SizeOf(TSmoothLineVertex), Nil);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TSmoothLineVertex) * VertexCount, @SmoothLineBuffer[0], GL_DYNAMIC_DRAW);
  glDrawArrays(GL_TRIANGLES, 0, VertexCount);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, Nil);
End;

Procedure RenderPoints(Const aData: PVector3; aLen: Integer);
Var
  PointOffset, BatchCount, PointIndex, VertexIndex: Integer;
  HalfSize: Single;
  PointValue: TVector3;
Begin
  If aLen <= 0 Then exit;
  BatchCount := aLen;
  If BatchCount > PointBatchSize Then BatchCount := PointBatchSize;
  If length(PointVertexBuffer) < BatchCount * 6 Then
    setlength(PointVertexBuffer, BatchCount * 6);
  HalfSize := ShaderPointSize / 2;
  PointOffset := 0;
  While PointOffset < aLen Do Begin
    BatchCount := aLen - PointOffset;
    If BatchCount > PointBatchSize Then BatchCount := PointBatchSize;
    For PointIndex := 0 To BatchCount - 1 Do Begin
      PointValue := aData[PointOffset + PointIndex];
      VertexIndex := PointIndex * 6;
      PointVertexBuffer[VertexIndex] := v3(PointValue.x - HalfSize, PointValue.y - HalfSize, PointValue.z);
      PointVertexBuffer[VertexIndex + 1] := v3(PointValue.x - HalfSize, PointValue.y + HalfSize, PointValue.z);
      PointVertexBuffer[VertexIndex + 2] := v3(PointValue.x + HalfSize, PointValue.y - HalfSize, PointValue.z);
      PointVertexBuffer[VertexIndex + 3] := PointVertexBuffer[VertexIndex + 2];
      PointVertexBuffer[VertexIndex + 4] := PointVertexBuffer[VertexIndex + 1];
      PointVertexBuffer[VertexIndex + 5] := v3(PointValue.x + HalfSize, PointValue.y + HalfSize, PointValue.z);
    End;
    glBufferData(GL_ARRAY_BUFFER, SizeOf(TVector3) * BatchCount * 6, @PointVertexBuffer[0], GL_DYNAMIC_DRAW);
    glDrawArrays(GL_TRIANGLES, 0, BatchCount * 6);
    inc(PointOffset, BatchCount);
  End;
End;

Procedure DoShaderData(Const aData: PVector3; aLen: Integer);
Var
  i: integer;
  pStart: PVector3;
Begin
  If aLen <= 0 Then exit;
  If (aShaderMode In [GL_LINES, GL_LINE_LOOP, GL_LINE_STRIP]) Then Begin
    If ShaderLineSmooth Then Begin
      If aShaderMode = GL_LINES Then Begin
        For i := 0 To aLen Div 2 - 1 Do
          RenderSmoothLineStrip(aData + i * 2, 2, False);
      End
      Else Begin
        RenderSmoothLineStrip(aData, aLen, aShaderMode = GL_LINE_LOOP);
      End;
      exit;
    End;
    If aLen < 2 Then exit;
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
    RenderPoints(aData, aLen);
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
  If length(aData) = 0 Then exit;
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

Procedure glShaderVertex(x, y: Single);
Begin
  glShaderVertex(v3(x, y, 0));
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

Procedure glShaderEnable(aCapability: GLenum);
Begin
  If aCapability = GL_LINE_SMOOTH Then
    ShaderLineSmooth := True
  Else
    glEnable(aCapability);
End;

Procedure glShaderDisable(aCapability: GLenum);
Begin
  If aCapability = GL_LINE_SMOOTH Then
    ShaderLineSmooth := False
  Else
    glDisable(aCapability);
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
  PointVertexBuffer := Nil;
  ShaderLineSmooth := False;
  setlength(RenderVertexBuffer, VertexBufferBlockSize);
  RenderVertexBufferCnt := 0;

Finalization
  setlength(RenderVertexBuffer, 0);
  setlength(PointVertexBuffer, 0);
  setlength(SmoothLinePoints, 0);
  setlength(SmoothLineBuffer, 0);

End.

