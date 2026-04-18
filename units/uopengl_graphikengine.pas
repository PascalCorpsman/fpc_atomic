(******************************************************************************)
(* uOpenGLGraphikEngine.pas                                        ??.??.???? *)
(*                                                                            *)
(* Version     : 0.14                                                         *)
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
(*               0.11 - Fix LoadAlphaGraphikItem did not respect png          *)
(*                          transparency                                      *)
(*               0.12 - Start removing glpushmatrix / glpopmatrix calls       *)
(*                      Start speedup image loading / decoding                *)
(*               0.13 - Start with support for OpenGL Shader                  *)
(*               0.14 - Fix GL_INVALID_OPERATION in RenderQuad by binding     *)
(*                      ShaderVAO before glVertexAttribPointer calls          *)
(*                                                                            *)
(******************************************************************************)
Unit uopengl_graphikengine;

{$IFDEF FPC}
{$MODE objFPC}{$H+}
{$ENDIF}

Interface

// Je nach dem wie mans Braucht mus hier angepasst werden!!

{.$DEFINE USE_GL}// Deprecated, better do not use anymore

(*
Ist dieser Schalter an, werden diverse Informationen in die Konsole geschrieben
 - Anzeige des Allokierten OpenGLSpeichers
 - Anzeige der Anzahl der Allokierten Texturen

*)
{.$DEFINE DEBUGGOUTPUT}

(*
Aktiviert die Nutzung von OpenGL im Legacy Mode, default ist Shader mode, der
auch mit OpenGL 3.3 funktioniert, aber nicht alle Funktionen von OpenGL 3.3 nutzt.
*)
{.$DEFINE LEGACYMODE}

{$IFDEF LEGACYMODE}
{$IFDEF LCLGTK3}
{$WARNING OpenGL Legacymode will not work when compiled for GTK3}
{$ENDIF}
{$ENDIF}

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
    Image: Integer; // Unique identifier given by OpenGL glGenTextures
    Name: String; // Unique Name of texture (filepath)
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
    Function LoadGraphikItem(Const Graphik: TBitmap; Name: String; Stretch: TStretchmode = smNone): TGraphikItem; overload; // Laden einer Graphik ohne Alphakanal
    Function LoadAlphaColorGraphik(Filename: String; Color: TRGB; Stretch: TStretchmode = smNone): Integer; overload; // Lädt eine Alphagraphik und setzt den Wert von Color = Transparent.
    Function LoadAlphaColorGraphikItem(Filename: String; Color: TRGB; Stretch: TStretchmode = smNone): TGraphikItem; overload;
    Function LoadAlphaColorGraphik(Const Graphik: TBitmap; Name: String; Color: TRGB; Stretch: TStretchmode = smNone): Integer; overload; // Lädt eine Alphagraphik und setzt den Wert von Color = Transparent.
    Function LoadAlphaColorGraphikItem(Const Graphik: TBitmap; Name: String; Color: TRGB; Stretch: TStretchmode = smNone): TGraphikItem; overload;
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
  Fuchsia: TRGB = (b: 255; g: 0; r: 255);
  Black: TRGB = (b: 0; g: 0; r: 0);
  White: TRGB = (b: 255; g: 255; r: 255);

Var
  OpenGL_GraphikEngine: TOpenGL_GraphikEngine;

{$IFNDEF LEGACYMODE}
  (*
   * Shader system initialization and management
   *)
Procedure OpenGL_GraphikEngine_InitializeShaderSystem;
(*
 * Shutdown Shader system, needs to be called in OnDestroy Handler
 *)
Procedure OpenGL_GraphikEngine_FinalizeShaderSystem;
{$ENDIF}

(*
Rendert in 2D ein Quad

ACHTUNG Bei Bildern mit der KantenLänge 1 Kommt blödsinn = kein Bild raus !!

ACHTUNG Diese Routinen funktionieren nicht immer mit eingeschalteten CullFacing !!
*)
{$IFDEF LEGACYMODE}
Procedure RenderAlphaQuad(Top, Left: Single; Image: TGraphikItem); overload; // Fertig Getestet // WTF: warum ist hier top und left vertauscht ?
Procedure RenderAlphaQuad(Middle: TVector2; Width, Height, Angle: Integer; Texture: integer = 0); overload; // Fertig Getestet
Procedure RenderAlphaRQuad(TopLeft, BottomRight: TVector2; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0); overload; // Fertig Getestet
Procedure RenderAlphaImage(Value: TSubImage);
Procedure RenderAlphaTiledQuad(Left, Top: Single; Index, TilesPerRow, TilesPerCol: integer; Const Image: TGraphikItem);

Procedure RenderQuad(Top, Left: Single; Image: TGraphikItem); overload; // WTF: warum ist hier top und left vertauscht ?
Procedure RenderQuad(Middle: TVector2; Angle: Single; Image: TGraphikItem); overload;
Procedure RenderQuad(Middle: TVector2; Width, Height, Angle: Single; Texture: integer = 0); overload; // Fertig Getestet

Procedure RenderQuad(TopLeft, BottomRight: TVector2; Angle: Integer; RotatebyOrigin: Boolean = False; Texture: Integer = 0); overload; // Fertig Getestet
Procedure RenderTiledQuad(Left, Top: Single; Index, TilesPerRow, TilesPerCol: integer; Const Image: TGraphikItem);
{$ELSE}
// Shader mode rendering helper
// Normal functions
Procedure RenderQuad(Left, Top, Depth: Single; Image: TGraphikItem); overload;
Procedure RenderQuad(Middle: TVector3; Angle: Single; Image: TGraphikItem); overload;
Procedure RenderAlphaQuad(Left, Top, Depth: Single; Image: TGraphikItem);

Procedure RenderTiledQuad(Left, Top, Depth: Single; Index, TilesPerRow, TilesPerCol: integer; Const Image: TGraphikItem);
Procedure RenderAlphaTiledQuad(Left, Top, Depth: Single; Index, TilesPerRow, TilesPerCol: integer; Const Image: TGraphikItem);

// Functions that make custom "scalings" possible
Procedure RenderQuad(Left, Top, Depth: Single; TileRenderWidth, TileRenderHeight: Single; Image: TGraphikItem); overload;
Procedure RenderAlphaQuad(Left, Top, Depth: Single; TileRenderWidth, TileRenderHeight: Single; Image: TGraphikItem);

Procedure RenderTiledQuad(Left, Top, Depth, TileRenderWidth, TileRenderHeight: Single; Index, TilesPerRow, TilesPerCol: integer; Const Image: TGraphikItem);
Procedure RenderAlphaTiledQuad(Left, Top, Depth, TileRenderWidth, TileRenderHeight: Single; Index, TilesPerRow, TilesPerCol: integer; Const Image: TGraphikItem);
{$ENDIF}
(*
 * 2D rendering mode setup
 * Go2d sets up orthographic projection for 2D rendering
 * Exit2d restores previous state
 *)
Procedure Go2d(Width, Height: Integer);
Procedure Exit2d();
{$IFNDEF LEGACYMODE}
Function Get2DResolution(): TPoint;
{$ENDIF}

{$IFDEF LEGACYMODE}
(*
Rendert einen Kreis, ohne Füllung
*)
Procedure RenderCircle(Middle: TVector2; Steps, Radius: Integer);

(*
Rendert ein Billboard, ACHTUNG bei mehr als einem Bildboard Pro Renderschritt, sollte das "Hohlen" der Modelviewmatrix ausgelagert werden !!
Mittels Dimension kann man die Ausbreitung in x und y Richtung angeben. Position ist stets der Mittelpunkt der textur
*)
Procedure RenderBillboard(Position: TVector3; Dimension: TVector2; Texture: integer = 0);
{$ENDIF}
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

{$IFNDEF LEGACYMODE}

(*
 * Switches to color shader for rendering colored geometry without textures
 * Call this after Go2d and before rendering colored geometry
 *)
Procedure UseColorShader;

(*
 * Switches back to texture shader for rendering textured geometry
 * Call this to return to normal texture rendering after UseColorShader
 *)
Procedure UseTextureShader(); overload;
Procedure UseTextureShader(Const c: TVector4); overload;

(*
 * Sets the color for the color shader
 * Must be called after UseColorShader and before rendering
 *)
Procedure SetShaderColor(r, g, b: Single; a: Single = 1);
Procedure SetShaderColorub(r, g, b: Byte; a: Byte = 255);

(*
 * Setzt den Alpha-Schwellwert für den Texture-Shader.
 * Pixel mit Alpha < Threshold werden verworfen (discard).
 * Threshold = 0.0 deaktiviert den Alpha-Test (Core-Profile-Ersatz für GL_ALPHA_TEST).
 *)
Procedure SetShaderAlphaThreshold(Threshold: Single);

(*
 * Compiles a single OpenGL shader from source.
 * ShaderType: GL_VERTEX_SHADER or GL_FRAGMENT_SHADER.
 * Raises an exception on compile error.
 *)
Function CompileShader(Src: PChar; ShaderType: GLenum): GLuint;

(*
 * Setzt die uTransform-Uniform in beiden Shadern auf die angegebene Matrix.
 * Entspricht dem kombinierten Effekt von glTranslatef + glScalef im Legacy-Mode.
 * Die Matrix wird als row-major übergeben (TMatrix4x4 aus uvectormath).
 *)
Procedure SetShaderTransform(Const m: TMatrix4x4);

(*
 * Setzt uTransform in beiden Shadern zurück auf die Einheitsmatrix.
 * Muss aufgerufen werden, nachdem der transformierte Bereich fertig gerendert wurde.
 *)
Procedure ResetShaderTransform;
{$ENDIF}

Function FRect(Top, Left, Bottom, Right: Single): TFRect;

Implementation

{$IFNDEF LEGACYMODE}

Const
  // Shader sources for modern OpenGL rendering
  VertexSrc: PChar =
  '#version 330 core'#10 +
    'layout(location = 0) in vec2 aPos;'#10 +
    'layout(location = 1) in vec2 aTexCoord;'#10 +
    'uniform vec2 uResolution;'#10 +
    'uniform float uDepth;'#10 +
    'uniform mat4 uTransform;'#10 +
    'out vec2 vTexCoord;'#10 +
    'void main() {'#10 +
    '  vec4 transformed = uTransform * vec4(aPos, 0.0, 1.0);'#10 +
    '  vec2 ndc = vec2((transformed.x / uResolution.x) * 2.0 - 1.0, 1.0 - (transformed.y / uResolution.y) * 2.0);'#10 +
    '  gl_Position = vec4(ndc, -uDepth, 1.0);'#10 +
    '  vTexCoord = aTexCoord;'#10 +
    '}';

  FragmentSrc: PChar =
  '#version 330 core'#10 +
    'in vec2 vTexCoord;'#10 +
    'out vec4 FragColor;'#10 +
    'uniform sampler2D uTexture;'#10 +
    'uniform vec4 uColor;'#10 +
    'uniform float uAlphaThreshold;'#10 +
    'void main() {'#10 +
    '  vec4 texColor = texture(uTexture, vTexCoord);'#10 +
    '  FragColor = texColor * uColor;'#10 +
    '  if (uAlphaThreshold > 0.0 && FragColor.a >= uAlphaThreshold) discard;'#10 +
    '}';

  // Shader sources for color rendering (no textures)
  ColorVertexSrc: PChar =
  '#version 330 core'#10 +
    'layout(location = 0) in vec3 aPos;'#10 +
    'uniform vec2 uResolution;'#10 +
    'uniform mat4 uTransform;'#10 +
    'void main() {'#10 +
    '  vec4 transformed = uTransform * vec4(aPos.xy, 0.0, 1.0);'#10 +
    '  vec2 ndc = vec2((transformed.x / uResolution.x) * 2.0 - 1.0, 1.0 - (transformed.y / uResolution.y) * 2.0);'#10 +
    '  gl_Position = vec4(ndc, -aPos.z, 1.0);'#10 +
    '}';

  ColorFragmentSrc: PChar =
  '#version 330 core'#10 +
    'out vec4 FragColor;'#10 +
    'uniform vec4 uColor;'#10 +
    'void main() {'#10 +
    '  FragColor = uColor;'#10 +
    '}';
{$ENDIF}

Var
  // LUT for sin / cos in 0.1° steps
  Sin_discrete, Cos_discrete: Array[0..3599] Of Single;

  ShaderVBO: GLuint = 0;

  //{$IFNDEF LEGACYMODE}
    // Shader system variables (only used when not in legacy OpenGL mode)
  ShaderProgram: GLuint = 0;
  ShaderVAO: GLuint = 0;

  // Virtual coordinate space set by Go2d, do not write these values manually !
  VirtualResolutionWidth: Integer = 0;
  VirtualResolutionHeight: Integer = 0;

  // Color shader for rendering without textures
  ColorShaderProgram: GLuint = 0;
  ColorShaderVAO: GLuint = 0;
  //{$ENDIF}

{$IFDEF DEBUGGOUTPUT}

Function FileSizeToString(Value: Int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value >= 1000 Then Begin
    s := 'K';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'M';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'G';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'T';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'P';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If (r Div 100) <> 0 Then Begin
    value := value * 10 + round(r / 100);
    result := format('%0.1f%sB', [value / 10, s]);
  End
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
{$IFDEF LEGACYMODE}
  glColor4ub(r, g, b, Alpha);
{$ELSE}
  // In shader mode, set color via uniform
  SetShaderColor(r / 255.0, g / 255.0, b / 255.0, Alpha / 255.0);
{$ENDIF}
End;

{$IFNDEF LEGACYMODE}

Procedure UseColorShader;
Begin
  glUseProgram(ColorShaderProgram);
  glBindVertexArray(ColorShaderVAO);
End;

Procedure UseTextureShader;
Const
  White: TVector4 = (x: 1.0; y: 1.0; z: 1.0; w: 1.0);
Begin
  UseTextureShader(White);
End;

Procedure UseTextureShader(Const c: TVector4);
Var
  LocColor, LocThreshold: GLint;
Begin
  glUseProgram(ShaderProgram);
  glBindVertexArray(ShaderVAO);
  LocColor := glGetUniformLocation(ShaderProgram, 'uColor');
  If LocColor >= 0 Then
    glUniform4f(LocColor, c.x, c.y, c.z, c.w);
  // Alpha-Test ist standardmäßig deaktiviert; Fragment-Shader-discard nur aktiv wenn > 0.
  LocThreshold := glGetUniformLocation(ShaderProgram, 'uAlphaThreshold');
  If LocThreshold >= 0 Then
    glUniform1f(LocThreshold, 0.0);
End;

Procedure SetShaderColor(r, g, b: Single; a: Single);
Var
  LocColor: GLint;
Begin
  LocColor := glGetUniformLocation(ColorShaderProgram, 'uColor');
  If LocColor >= 0 Then
    glUniform4f(LocColor, r, g, b, a);
End;

Procedure SetShaderColorub(r, g, b: Byte; a: Byte);
Begin
  SetShaderColor(r / 255, g / 255, b / 255, a / 255);
End;

Procedure SetShaderAlphaThreshold(Threshold: Single);
Var
  loc: GLint;
Begin
  loc := glGetUniformLocation(ShaderProgram, 'uAlphaThreshold');
  If loc >= 0 Then
    glUniform1f(loc, Threshold);
End;

Procedure SetShaderTransform(Const m: TMatrix4x4);
Var
  loc: GLint;
  prevProgram: GLint;
Begin
  glGetIntegerv(GL_CURRENT_PROGRAM, @prevProgram);

  glUseProgram(ColorShaderProgram);
  loc := glGetUniformLocation(ColorShaderProgram, 'uTransform');
  If loc >= 0 Then
    glUniformMatrix4fv(loc, 1, GL_TRUE, @m[0, 0]);

  glUseProgram(ShaderProgram);
  loc := glGetUniformLocation(ShaderProgram, 'uTransform');
  If loc >= 0 Then
    glUniformMatrix4fv(loc, 1, GL_TRUE, @m[0, 0]);

  glUseProgram(prevProgram);
End;

Procedure ResetShaderTransform;
Var
  id: TMatrix4x4;
Begin
  id := IdentityMatrix4x4;
  SetShaderTransform(id);
End;

{$ENDIF}

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


Procedure RenderQuad(Middle: TVector2; Width, Height, Angle: Single; Texture: integer = 0); overload; // Fertig Getestet
Var
  hw, hh: Single;
  s, c: Single;
  Right, Up: TVector2;
  TL, TR, BR, BL: TVector2;
  idx: Integer;
Begin
  hw := Width * 0.5;
  hh := Height * 0.5;

  { Angle -> LUT Index (0.1° steps) }
  idx := round(Angle * 10);
  idx := idx Mod 3600;
  If idx < 0 Then
    idx := idx + 3600;
  s := -Sin_discrete[idx];
  c := -Cos_discrete[idx];

  { Basisvektoren als TVector2 }
  Right := V2(c * hw, -s * hw);
  Up := V2(s * hh, c * hh);

  { Ecken berechnen als Vektorarithmetik }
  TL := Middle - Right + Up;
  TR := Middle + Right + Up;
  BR := Middle + Right - Up;
  BL := Middle - Right - Up;

  { OpenGL Zeichnen }
  glBindTexture(GL_TEXTURE_2D, Texture);
  glBegin(GL_QUADS);
  glTexCoord2f(0, 1);
  glVertex2fv(@TL);
  glTexCoord2f(1, 1);
  glVertex2fv(@TR);
  glTexCoord2f(1, 0);
  glVertex2fv(@BR);
  glTexCoord2f(0, 0);
  glVertex2fv(@BL);
  glEnd;
End;
(* -- Dead code ??
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
    RenderQuad(v2((TopLeft.x + BottomRight.x) / 2, (TopLeft.y + BottomRight.y) / 2), abs(BottomRight.x - TopLeft.x), abs(TopLeft.y - BottomRight.y), angle, Texture);
End;
*)

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
    RenderQuad(v2((TopLeft.x + BottomRight.x) / 2, (TopLeft.y + BottomRight.y) / 2), abs(BottomRight.x - TopLeft.x), abs(TopLeft.y - BottomRight.y), angle, Texture);
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
  glbegin(gl_quads);
  glTexCoord2f(0, th);
  glvertex3f(left, top + image.OrigHeight, 0);
  glTexCoord2f(tw, th);
  glvertex3f(left + image.OrigWidth, top + image.OrigHeight, 0);
  glTexCoord2f(tw, 0);
  glvertex3f(left + image.OrigWidth, top, 0);
  glTexCoord2f(0, 0);
  glvertex3f(left, top, 0);
  glend;
End;

Procedure RenderQuad(Middle: TVector2; Angle: Single; Image: TGraphikItem);
Var
  hw, hh: Single;
  s, c: Single;
  Right, Up: TVector2;
  TL, TR, BR, BL: TVector2;
  tw, th: Single;
  idx: Integer;
Begin
  hw := Image.OrigWidth * 0.5;
  hh := Image.OrigHeight * 0.5;

  { Angle -> LUT Index (0.1° steps) }
  idx := Round(Angle * 10);
  idx := idx Mod 3600;
  If idx < 0 Then
    idx := idx + 3600;
  s := -Sin_discrete[idx];
  c := Cos_discrete[idx];

  { Basisvektoren als TVector2 }
  Right := V2(c * hw, -s * hw);
  Up := V2(s * hh, c * hh);

  { Ecken berechnen als Vektorarithmetik }
  TL := Middle - Right + Up;
  TR := Middle + Right + Up;
  BR := Middle + Right - Up;
  BL := Middle - Right - Up;

  { Texturkoordinaten }
  Case Image.Stretched Of
    smClamp: Begin
        tw := Image.OrigWidth / Image.StretchedWidth;
        th := Image.OrigHeight / Image.StretchedHeight;
      End;
  Else Begin
      tw := 1;
      th := 1;
    End;
  End;

  { OpenGL Zeichnen }
  glBindTexture(GL_TEXTURE_2D, Image.Image);
  glBegin(GL_QUADS);
  glTexCoord2f(0, th);
  glVertex2fv(@TL);
  glTexCoord2f(tw, th);
  glVertex2fv(@TR);
  glTexCoord2f(tw, 0);
  glVertex2fv(@BR);
  glTexCoord2f(0, 0);
  glVertex2fv(@BL);
  glEnd;
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

Procedure RenderAlphaQuad(Middle: TVector2; Width, Height, Angle: Integer; Texture: integer = 0);
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

(*
 * Die Idee, ist dass wir die Textur betrachten als "Collection" von vielen Tiles
 * Diese collection wird zu einer Rechteckfläche von TilesPerRow und TilesPerCol
 * in die wir via Index zugreifen, und dann immer nur das passende "teilstück"
 * Rendern ;).
 *)

Procedure RenderTiledQuad(Left, Top: Single; Index, TilesPerRow,
  TilesPerCol: integer; Const Image: TGraphikItem);
Var
  w, h, tw, th: Single;
  ix, iy: integer;
Begin
  ix := index Mod TilesPerRow;
  iy := index Div TilesPerRow;
  w := Image.OrigWidth / TilesPerRow;
  h := Image.OrigHeight / TilesPerCol;
  Case Image.Stretched Of
    smClamp: Begin
        tw := w / Image.StretchedWidth;
        th := h / Image.StretchedHeight;
      End;
    smNone, smStretch, smStretchHard: Begin
        tw := w;
        th := h;
      End;
  End;
  glBindTexture(gl_texture_2d, image.Image);
  glbegin(gl_quads);
  glTexCoord2f(tw * ix, th * (iy + 1));
  glvertex3f(left, top + h, 0);
  glTexCoord2f(tw * (ix + 1), th * (iy + 1));
  glvertex3f(left + w, top + h, 0);
  glTexCoord2f(tw * (ix + 1), th * iy);
  glvertex3f(left + w, top, 0);
  glTexCoord2f(tw * ix, th * iy);
  glvertex3f(left, top, 0);
  glend;
End;

Procedure RenderAlphaTiledQuad(Left, Top{$IFNDEF LEGACYMODE}, Depth{$ENDIF}: Single; Index, TilesPerRow,
  TilesPerCol: integer; Const Image: TGraphikItem);
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderTiledQuad(Left, Top{$IFNDEF LEGACYMODE}, Depth{$ENDIF}, Index, TilesPerRow, TilesPerCol, Image);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

{$IFNDEF LEGACYMODE}

Function CompileShader(Src: PChar; ShaderType: GLenum): GLuint;
Var
  S: GLuint;
  status: GLint;
  Log: Array[0..1023] Of char;
Begin
  result := 0;
  S := glCreateShader(ShaderType);
  glShaderSource(S, 1, @Src, Nil);
  glCompileShader(S);

  glGetShaderiv(S, GL_COMPILE_STATUS, @status);
  If status = 0 Then Begin
    glGetShaderInfoLog(S, 1024, Nil, @Log);
    Raise Exception.Create('Shader Fehler: ' + Log);
  End;

  Result := S;
End;

Function CreateShaderProgram: GLuint;
Var
  vs, fs: GLuint;
  prog: GLuint;
  status: GLint;
  Log: Array[0..1023] Of char;
Begin
  vs := CompileShader(VertexSrc, GL_VERTEX_SHADER);
  fs := CompileShader(FragmentSrc, GL_FRAGMENT_SHADER);

  prog := glCreateProgram();
  glAttachShader(prog, vs);
  glAttachShader(prog, fs);
  glLinkProgram(prog);

  glGetProgramiv(prog, GL_LINK_STATUS, @status);
  If status = 0 Then Begin
    glGetProgramInfoLog(prog, 1024, Nil, @Log);
    Raise Exception.Create('Link Fehler: ' + Log);
  End;

  glDeleteShader(vs);
  glDeleteShader(fs);

  Result := prog;
End;

Function CreateColorShaderProgram: GLuint;
Var
  vs, fs: GLuint;
  prog: GLuint;
  status: GLint;
  Log: Array[0..1023] Of char;
Begin
  vs := CompileShader(ColorVertexSrc, GL_VERTEX_SHADER);
  fs := CompileShader(ColorFragmentSrc, GL_FRAGMENT_SHADER);

  prog := glCreateProgram();
  glAttachShader(prog, vs);
  glAttachShader(prog, fs);
  glLinkProgram(prog);

  glGetProgramiv(prog, GL_LINK_STATUS, @status);
  If status = 0 Then Begin
    glGetProgramInfoLog(prog, 1024, Nil, @Log);
    Raise Exception.Create('Color Shader Link Fehler: ' + Log);
  End;

  glDeleteShader(vs);
  glDeleteShader(fs);

  Result := prog;
End;

Procedure OpenGL_GraphikEngine_InitializeShaderSystem;
Begin
  // Create VBO on first use
  If ShaderVBO = 0 Then
    glGenBuffers(1, @ShaderVBO);

  If ShaderProgram = 0 Then Begin
    ShaderProgram := CreateShaderProgram;
    glGenVertexArrays(1, @ShaderVAO);
  End;

  If ColorShaderProgram = 0 Then Begin
    ColorShaderProgram := CreateColorShaderProgram;
    glGenVertexArrays(1, @ColorShaderVAO);
  End;

  // uTransform muss explizit auf Einheitsmatrix gesetzt werden,
  // da GLSL uniforms standardmäßig 0 sind (keine Einheitsmatrix).
  ResetShaderTransform;
  // uAlphaThreshold explizit auf 0 setzen (kein Alpha-Test), obwohl GLSL-Default bereits 0.0 ist.
  glUseProgram(ShaderProgram);
  SetShaderAlphaThreshold(0.0);
End;

Procedure OpenGL_GraphikEngine_FinalizeShaderSystem;
Begin
  If ShaderVBO <> 0 Then
    glDeleteBuffers(1, @ShaderVBO);
  ShaderVBO := 0;

  If ShaderVAO <> 0 Then
    glDeleteVertexArrays(1, @ShaderVAO);
  ShaderVAO := 0;

  If ShaderProgram <> 0 Then
    glDeleteProgram(ShaderProgram);
  ShaderProgram := 0;
End;

Procedure RenderAlphaTiledQuad(Left, Top, Depth, TileRenderWidth,
  TileRenderHeight: Single; Index, TilesPerRow, TilesPerCol: integer;
  Const Image: TGraphikItem);
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderTiledQuad(Left, Top, Depth, TileRenderWidth, TileRenderHeight, Index, TilesPerRow, TilesPerCol, Image);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);

End;
{$ENDIF}

Procedure Go2d(Width, Height: Integer);
{$IFNDEF LEGACYMODE}
Var
  LocRes: GLint;
{$ENDIF}
Begin
{$IFDEF LEGACYMODE}
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  glOrtho(0, Width, Height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
{$ELSE}
  // Set uResolution on both shader programs to the virtual coordinate space.
  // glViewport handles the actual stretching to the window; shaders must NOT
  // override this with real viewport dimensions.
  VirtualResolutionWidth := Width;
  VirtualResolutionHeight := Height;
  glUseProgram(ShaderProgram);
  LocRes := glGetUniformLocation(ShaderProgram, 'uResolution');
  If LocRes >= 0 Then
    glUniform2f(LocRes, Width, Height);
  glUseProgram(ColorShaderProgram);
  LocRes := glGetUniformLocation(ColorShaderProgram, 'uResolution');
  If LocRes >= 0 Then
    glUniform2f(LocRes, Width, Height);
  UseTextureShader;
{$ENDIF}
End;

Procedure Exit2d;
Begin
{$IFDEF LEGACYMODE}
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
{$ELSE}
  glBindVertexArray(0);
  glUseProgram(0);
{$ENDIF}
End;

{$IFNDEF LEGACYMODE}

Function Get2DResolution: TPoint;
Begin
  result := point(VirtualResolutionWidth, VirtualResolutionHeight);
End;
{$ENDIF}

Procedure RenderQuad(Left, Top, Depth: Single; Image: TGraphikItem);
Var
  tw, th: Single;
  vertices: Array[0..15] Of GLfloat; // 4 vertices * (2 pos + 2 texcoord)
  LocDepth: GLint;
  CurrentProgram: GLint;
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

  // Vertex 0: bottom-left
  vertices[0] := left;
  vertices[1] := top + image.OrigHeight;
  vertices[2] := 0;
  vertices[3] := th;

  // Vertex 1: bottom-right
  vertices[4] := left + image.OrigWidth;
  vertices[5] := top + image.OrigHeight;
  vertices[6] := tw;
  vertices[7] := th;

  // Vertex 2: top-right
  vertices[8] := left + image.OrigWidth;
  vertices[9] := top;
  vertices[10] := tw;
  vertices[11] := 0;

  // Vertex 3: top-left
  vertices[12] := left;
  vertices[13] := top;
  vertices[14] := 0;
  vertices[15] := 0;

  glBindTexture(GL_TEXTURE_2D, image.Image);
  glBindVertexArray(ShaderVAO);
  glBindBuffer(GL_ARRAY_BUFFER, ShaderVBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices[0], GL_DYNAMIC_DRAW);

  // Set depth uniform
  glGetIntegerv(GL_CURRENT_PROGRAM, @CurrentProgram);
  LocDepth := glGetUniformLocation(CurrentProgram, 'uDepth');
  If LocDepth >= 0 Then
    glUniform1f(LocDepth, Depth);

  // Position attribute (location = 0)
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Nil);

  // TexCoord attribute (location = 1)
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Pointer(2 * SizeOf(GLfloat)));

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glBindVertexArray(0);
End;

(*
 * Die Idee, ist dass wir die Textur betrachten als "Collection" von vielen Tiles
 * Diese collection wird zu einer Rechteckfläche von TilesPerRow und TilesPerCol
 * in die wir via Index zugreifen, und dann immer nur das passende "teilstück"
 * Rendern ;).
 *)

Procedure RenderTiledQuad(Left, Top, Depth: Single; Index, TilesPerRow,
  TilesPerCol: integer; Const Image: TGraphikItem);
Var
  w, h, tw, th: Single;
  ix, iy: integer;
  vertices: Array[0..15] Of GLfloat; // 4 vertices * (2 pos + 2 texcoord)
  LocDepth: GLint;
  CurrentProgram: GLint;
Begin
  ix := index Mod TilesPerRow;
  iy := index Div TilesPerRow;
  w := Image.OrigWidth / TilesPerRow;
  h := Image.OrigHeight / TilesPerCol;
  Case Image.Stretched Of
    smClamp: Begin
        tw := w / Image.StretchedWidth;
        th := h / Image.StretchedHeight;
      End;
    smNone, smStretch, smStretchHard: Begin
        tw := w;
        th := h;
      End;
  End;
  glBindTexture(gl_texture_2d, image.Image);

  // Vertex 0: bottom-left
  vertices[0] := left;
  vertices[1] := top + h;
  vertices[2] := tw * ix;
  vertices[3] := th * (iy + 1);

  // Vertex 1: bottom-right
  vertices[4] := left + w;
  vertices[5] := top + h;
  vertices[6] := tw * (ix + 1);
  vertices[7] := th * (iy + 1);

  // Vertex 2: top-right
  vertices[8] := left + w;
  vertices[9] := top;
  vertices[10] := tw * (ix + 1);
  vertices[11] := th * iy;

  // Vertex 3: top-left
  vertices[12] := left;
  vertices[13] := top;
  vertices[14] := tw * ix;
  vertices[15] := th * iy;

  glBindTexture(GL_TEXTURE_2D, image.Image);
  glBindVertexArray(ShaderVAO);
  glBindBuffer(GL_ARRAY_BUFFER, ShaderVBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices[0], GL_DYNAMIC_DRAW);

  // Set depth uniform
  glGetIntegerv(GL_CURRENT_PROGRAM, @CurrentProgram);
  LocDepth := glGetUniformLocation(CurrentProgram, 'uDepth');
  If LocDepth >= 0 Then
    glUniform1f(LocDepth, Depth);

  // Position attribute (location = 0)
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Nil);

  // TexCoord attribute (location = 1)
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Pointer(2 * SizeOf(GLfloat)));

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glBindVertexArray(0);
End;

Procedure RenderTiledQuad(Left, Top, Depth, TileRenderWidth, TileRenderHeight: Single;
  Index, TilesPerRow, TilesPerCol: integer; Const Image: TGraphikItem);
Var
  tw, th: Single;
  ix, iy: integer;
  vertices: Array[0..15] Of GLfloat; // 4 vertices * (2 pos + 2 texcoord)
  LocDepth: GLint;
  CurrentProgram: GLint;
Begin
  ix := index Mod TilesPerRow;
  iy := index Div TilesPerRow;
  Case Image.Stretched Of
    smClamp: Begin
        tw := (Image.OrigWidth / TilesPerRow) / Image.StretchedWidth;
        th := (Image.OrigHeight / TilesPerCol) / Image.StretchedHeight;
      End;
    smNone, smStretch, smStretchHard: Begin
        tw := 1.0 / TilesPerRow;
        th := 1.0 / TilesPerCol;
      End;
  End;
  glBindTexture(gl_texture_2d, image.Image);

  // Vertex 0: bottom-left
  vertices[0] := left;
  vertices[1] := top + TileRenderHeight;
  vertices[2] := tw * ix;
  vertices[3] := th * (iy + 1);

  // Vertex 1: bottom-right
  vertices[4] := left + TileRenderWidth;
  vertices[5] := top + TileRenderHeight;
  vertices[6] := tw * (ix + 1);
  vertices[7] := th * (iy + 1);

  // Vertex 2: top-right
  vertices[8] := left + TileRenderWidth;
  vertices[9] := top;
  vertices[10] := tw * (ix + 1);
  vertices[11] := th * iy;

  // Vertex 3: top-left
  vertices[12] := left;
  vertices[13] := top;
  vertices[14] := tw * ix;
  vertices[15] := th * iy;

  glBindTexture(GL_TEXTURE_2D, image.Image);
  glBindVertexArray(ShaderVAO);
  glBindBuffer(GL_ARRAY_BUFFER, ShaderVBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices[0], GL_DYNAMIC_DRAW);

  // Set depth uniform
  glGetIntegerv(GL_CURRENT_PROGRAM, @CurrentProgram);
  LocDepth := glGetUniformLocation(CurrentProgram, 'uDepth');
  If LocDepth >= 0 Then
    glUniform1f(LocDepth, Depth);

  // Position attribute (location = 0)
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Nil);

  // TexCoord attribute (location = 1)
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Pointer(2 * SizeOf(GLfloat)));

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glBindVertexArray(0);
End;

Procedure RenderQuad(Middle: TVector3; Angle: Single; Image: TGraphikItem);
Var
  hw, hh: Single;
  s, c: Single;
  Center2D: TVector2;
  Right, Up: TVector2;
  TL, TR, BR, BL: TVector2;
  tw, th: Single;
  idx: Integer;
  vertices: Array[0..15] Of GLfloat; // 4 vertices * (2 pos + 2 texcoord)
  LocDepth: GLint;
  CurrentProgram: GLint;
Begin
  hw := Image.OrigWidth * 0.5;
  hh := Image.OrigHeight * 0.5;

  { Angle -> LUT Index (0.1° steps) }
  idx := Round(Angle * 10);
  idx := idx Mod 3600;
  If idx < 0 Then
    idx := idx + 3600;
  s := -Sin_discrete[idx];
  c := Cos_discrete[idx];

  { Basisvektoren als TVector2 }
  Right := V2(c * hw, -s * hw);
  Up := V2(s * hh, c * hh);

  { 2D-Mittelpunkt aus dem 3D-Vektor (z = Tiefe) }
  Center2D := V2(Middle.x, Middle.y);

  { Ecken berechnen als Vektorarithmetik }
  TL := Center2D - Right - Up;
  TR := Center2D + Right - Up;
  BR := Center2D + Right + Up;
  BL := Center2D - Right + Up;

  { Texturkoordinaten }
  Case Image.Stretched Of
    smClamp: Begin
        tw := Image.OrigWidth / Image.StretchedWidth;
        th := Image.OrigHeight / Image.StretchedHeight;
      End;
  Else Begin
      tw := 1;
      th := 1;
    End;
  End;

  // Vertex 0: BL (bottom-left)
  vertices[0] := BL.x;
  vertices[1] := BL.y;
  vertices[2] := 0;
  vertices[3] := th;

  // Vertex 1: BR (bottom-right)
  vertices[4] := BR.x;
  vertices[5] := BR.y;
  vertices[6] := tw;
  vertices[7] := th;

  // Vertex 2: TR (top-right)
  vertices[8] := TR.x;
  vertices[9] := TR.y;
  vertices[10] := tw;
  vertices[11] := 0;

  // Vertex 3: TL (top-left)
  vertices[12] := TL.x;
  vertices[13] := TL.y;
  vertices[14] := 0;
  vertices[15] := 0;

  glBindTexture(GL_TEXTURE_2D, Image.Image);
  glBindVertexArray(ShaderVAO);
  glBindBuffer(GL_ARRAY_BUFFER, ShaderVBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices[0], GL_DYNAMIC_DRAW);

  // Tiefe (Middle.z) als Uniform setzen
  glGetIntegerv(GL_CURRENT_PROGRAM, @CurrentProgram);
  LocDepth := glGetUniformLocation(CurrentProgram, 'uDepth');
  If LocDepth >= 0 Then
    glUniform1f(LocDepth, Middle.z);

  // Position attribute (location = 0)
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Nil);

  // TexCoord attribute (location = 1)
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Pointer(2 * SizeOf(GLfloat)));

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glBindVertexArray(0);
End;

Procedure RenderAlphaQuad(Left, Top, Depth: Single; Image: TGraphikItem);
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderQuad(Left, Top, Depth, Image);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

Procedure RenderQuad(Left, Top, Depth: Single; TileRenderWidth,
  TileRenderHeight: Single; Image: TGraphikItem);
Var
  tw, th: Single;
  vertices: Array[0..15] Of GLfloat; // 4 vertices * (2 pos + 2 texcoord)
  LocDepth: GLint;
  CurrentProgram: GLint;
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

  // Vertex 0: bottom-left
  vertices[0] := left;
  vertices[1] := top + TileRenderHeight;
  vertices[2] := 0;
  vertices[3] := th;

  // Vertex 1: bottom-right
  vertices[4] := left + TileRenderWidth;
  vertices[5] := top + TileRenderHeight;
  vertices[6] := tw;
  vertices[7] := th;

  // Vertex 2: top-right
  vertices[8] := left + TileRenderWidth;
  vertices[9] := top;
  vertices[10] := tw;
  vertices[11] := 0;

  // Vertex 3: top-left
  vertices[12] := left;
  vertices[13] := top;
  vertices[14] := 0;
  vertices[15] := 0;

  glBindTexture(GL_TEXTURE_2D, image.Image);
  glBindVertexArray(ShaderVAO);
  glBindBuffer(GL_ARRAY_BUFFER, ShaderVBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices[0], GL_DYNAMIC_DRAW);

  // Set depth uniform
  glGetIntegerv(GL_CURRENT_PROGRAM, @CurrentProgram);
  LocDepth := glGetUniformLocation(CurrentProgram, 'uDepth');
  If LocDepth >= 0 Then
    glUniform1f(LocDepth, Depth);

  // Position attribute (location = 0)
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Nil);

  // TexCoord attribute (location = 1)
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Pointer(2 * SizeOf(GLfloat)));

  glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glBindVertexArray(0);

End;

Procedure RenderAlphaQuad(Left, Top, Depth: Single; TileRenderWidth,
  TileRenderHeight: Single; Image: TGraphikItem);
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderQuad(Left, Top, Depth, TileRenderWidth, TileRenderHeight, Image);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
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

Function TOpenGL_GraphikEngine.LoadGraphik(Filename: String;
  Stretch: TStretchmode): Integer;
Begin
  result := LoadGraphikItem(Filename, Stretch).Image;
End;

Function TOpenGL_GraphikEngine.LoadGraphikItem(Filename: String;
  Stretch: TStretchmode): TGraphikItem;
Var
  OpenGLData: Array Of Array[0..2] Of Byte;
  Data: String;
  b, b2: Tbitmap;
  IntfImg1: TLazIntfImage;
  CurColor: TFPColor;
  jp: TJPEGImage;
  png: TPortableNetworkGraphic;
  c, j, i: Integer;
{$IFDEF LEGACYMODE}
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
{$ENDIF}
  ow, oh, nw, nh: INteger;
{$IFDEF LCLGTK3}
  IntfImg: TLazIntfImage;
  IntfImg2: TLazIntfImage;
{$ENDIF}
Begin
  If FileExists(Filename) Then Begin
    Data := LowerCase(Filename);
    // Graphik bereits geladen
    For i := 0 To high(Fimages) Do Begin
      If Fimages[i].Name = Data Then Begin
        result := Fimages[i];
{$IFDEF DEBUGGOUTPUT}
        writeln('TGraphikEngine.LoadGraphik(' + Filename + ')');
        writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
        writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
        exit;
      End;
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
      b.assign(png);
      png.free;
    End
    Else Begin
      b.LoadFromFile(Filename);
    End;

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
{$IFDEF LCLGTK3}
            // Das SetSize machts in GTK3 Kaputt
            IntfImg := TLazIntfImage.Create(0, 0);
            IntfImg.LoadFromBitmap(b.Handle, b.MaskHandle);
            IntfImg2 := TLazIntfImage.Create(0, 0);
            IntfImg2.LoadFromBitmap(b.Handle, b.MaskHandle);
            IntfImg2.SetSize(nw, nh);
            For i := 0 To b.Width - 1 Do Begin
              For j := 0 To b.Height - 1 Do Begin
                IntfImg2.Colors[i, j] := IntfImg.Colors[i, j];
              End;
            End;
            b.LoadFromIntfImage(IntfImg2);
            IntfImg.Free;
            IntfImg2.Free;
{$ELSE}
            b2 := TBitmap.create;
            b2.PixelFormat := pf24bit;
            b2.width := nw;
            b2.height := nh;
            b2.canvas.Draw(0, 0, b);
            b.free;
            b := b2;
{$ENDIF}
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
{$IFDEF LEGACYMODE}
      bool := glIsEnabled(GL_TEXTURE_2D);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        glEnable(GL_TEXTURE_2D);
{$ENDIF}
      glGenTextures(1, @Result);
      glBindTexture(GL_TEXTURE_2D, result.Image);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, gl_RGB, b.width, b.height, 0, GL_RGB, GL_UNSIGNED_BYTE, @OpenGLData[0]);
{$IFDEF LEGACYMODE}
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        gldisable(GL_TEXTURE_2D);
{$ENDIF}
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

Function TOpenGL_GraphikEngine.LoadGraphikItem(Const Graphik: TBitmap;
  Name: String; Stretch: TStretchmode): TGraphikItem; // Laden einer Graphik ohne Alphakanal
Var
  OpenGLData: Array Of Array[0..2] Of Byte;
  Data: String;
  b, b2: Tbitmap;
  IntfImg1: TLazIntfImage;
  CurColor: TFPColor;
  c, j, i: Integer;
{$IFDEF LEGACYMODE}
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
{$ENDIF}
  img, ow, oh, nw, nh: Integer;
Begin
  Data := LowerCase(name);
  // Graphik bereits geladen
  For i := 0 To high(Fimages) Do
    If Fimages[i].Name = Data Then Begin
      result := Fimages[i];
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
    glGenTextures(1, @img);
{$IFDEF LEGACYMODE}
    bool := glIsEnabled(GL_TEXTURE_2D);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glEnable(GL_TEXTURE_2D);
{$ENDIF}
    glBindTexture(GL_TEXTURE_2D, img);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGB, b.width, b.height, 0, GL_RGB, GL_UNSIGNED_BYTE, @OpenGLData[0]);
{$IFDEF LEGACYMODE}
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(GL_TEXTURE_2D);
{$ENDIF}
    IntfImg1.free;
    // Übernehmen in die Engine
    setlength(Fimages, high(Fimages) + 2);
    Fimages[high(Fimages)].Image := img;
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
    Raise Exception.create('Error Image ' + extractfilename(name) + ' has invalid Width / Height, has to be 2^x.');
  b.free;
End;

Function TOpenGL_GraphikEngine.LoadGraphik(Const Graphik: TBitmap;
  Name: String; Stretch: TStretchmode): Integer;
Var
  gi: TGraphikItem;
Begin
  gi := LoadGraphikitem(Graphik, Name, Stretch);
  result := gi.image;
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
{$IFDEF LEGACYMODE}
      bool := glIsEnabled(GL_TEXTURE_2D);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        glEnable(GL_TEXTURE_2D);
{$ENDIF}
      glBindTexture(GL_TEXTURE_2D, result);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, b.width, b.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0]);
{$IFDEF LEGACYMODE}
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        gldisable(GL_TEXTURE_2D);
{$ENDIF}
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

Function TOpenGL_GraphikEngine.LoadAlphaColorGraphikItem(
  Const Graphik: TBitmap; Name: String; Color: TRGB; Stretch: TStretchmode
  ): TGraphikItem; // Lädt eine Alphagraphik und setzt den Wert von Color = Transparent.
Var
  Data: String;
  b2, b: Tbitmap;
  img, ow, oh, nw, nh, j, i: Integer;
  pSrc: PRGBA;
  pDst, pStart: PByte;
  Line: Pointer;
{$IFDEF LEGACYMODE}
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
{$ENDIF}
{$IFDEF LCLGTK3}
  IntfImg: TLazIntfImage;
  IntfImg2: TLazIntfImage;
{$ENDIF}
Begin
  Data := LowerCase(name);
  // Graphik bereits geladen
  For i := 0 To high(Fimages) Do
    If Fimages[i].Name = Data Then Begin
      result := Fimages[i];
{$IFDEF DEBUGGOUTPUT}
      writeln('TGraphikEngine.LoadAlphaColorgraphik(' + name + ')');
      writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
      writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
      exit;
    End;
  // Graphik mus geladen werden
  b := Tbitmap.create;
  b.PixelFormat := pf32bit;
  b.assign(Graphik);
  // create the raw image
  ow := b.width;
  oh := b.height;
  nw := ow;
  nh := oh;
{$IFDEF DEBUGGOUTPUT}
  writeln('TGraphikEngine.LoadAlphaColorgraphik(' + name + ')');
  writeln('Orig size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
{$ENDIF}
  Case Stretch Of
    smNone: Begin
      End;
    smStretchHard, smStretch: Begin
        nw := GetNextPowerOfTwo(ow);
        nh := GetNextPowerOfTwo(oh);
        If (nw <> ow) Or (nh <> oh) Then Begin
          b2 := TBitmap.Create;
          b2.PixelFormat := pf32bit;
          b2.SetSize(nw, nh);
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
        nw := GetNextPowerOfTwo(ow);
        nh := GetNextPowerOfTwo(oh);
        If (nw <> ow) Or (nh <> oh) Then Begin
{$IFDEF LCLGTK3}
          // Das SetSize machts in GTK3 Kaputt
          IntfImg := TLazIntfImage.Create(0, 0);
          IntfImg.LoadFromBitmap(b.Handle, b.MaskHandle);
          IntfImg2 := TLazIntfImage.Create(0, 0);
          IntfImg2.LoadFromBitmap(b.Handle, b.MaskHandle);
          IntfImg2.SetSize(nw, nh);
          For i := 0 To b.Width - 1 Do Begin
            For j := 0 To b.Height - 1 Do Begin
              IntfImg2.Colors[i, j] := IntfImg.Colors[i, j];
            End;
          End;
          b.LoadFromIntfImage(IntfImg2);
          IntfImg.Free;
          IntfImg2.Free;
{$ELSE}
          b2 := TBitmap.create;
          b2.PixelFormat := pf32bit;
          b2.SetSize(nw, nh);
          b2.canvas.Draw(0, 0, b);
          b.free;
          b := b2;
{$ENDIF}
        End;
      End;
  End;
  // load the raw image from the bitmap handles
{$IFDEF DEBUGGOUTPUT}
  writeln('OpenGL size : ' + inttostr(b.width) + 'x' + inttostr(b.height));
  OpenGLBufCount := OpenGLBufCount + (b.width * b.height * 4);
  writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
{$ENDIF}
  If IsPowerOfTwo(b.width) And IsPowerOfTwo(b.Height) Then Begin
    // Laden der Graphikdaten
    GetMem(pStart, b.Width * b.Height * 4);
    pDst := pStart;
    For j := 0 To b.Height - 1 Do Begin
      Line := b.ScanLine[j];
      pSrc := PRGBA(Line);
      For i := 0 To b.Width - 1 Do Begin
        pDst^ := pSrc^.R;
        Inc(pDst);
        pDst^ := pSrc^.G;
        Inc(pDst);
        pDst^ := pSrc^.B;
        Inc(pDst);
        If (pSrc^.R = Color.r) And
          (pSrc^.G = Color.g) And
          (pSrc^.B = Color.b) Then
          pDst^ := 255
        Else
          pDst^ := 0;
        Inc(pDst);
        Inc(pSrc);
      End;
    End;
    // Übergeben an OpenGL
    glGenTextures(1, @img);
{$IFDEF LEGACYMODE}
    bool := glIsEnabled(GL_TEXTURE_2D);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glEnable(GL_TEXTURE_2D);
{$ENDIF}
    glBindTexture(GL_TEXTURE_2D, img);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, b.width, b.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, pStart);
{$IFDEF LEGACYMODE}
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(GL_TEXTURE_2D);
{$ENDIF}
    FreeMem(pStart);
    // Übernehmen in die Engine
    setlength(Fimages, high(Fimages) + 2);
    Fimages[high(Fimages)].Image := img;
    Fimages[high(Fimages)].Name := data;
    Fimages[high(Fimages)].Stretched := stretch;
    Fimages[high(Fimages)].OrigHeight := oh;
    Fimages[high(Fimages)].OrigWidth := ow;
    Fimages[high(Fimages)].StretchedHeight := nh;
    Fimages[high(Fimages)].StretchedWidth := nw;
    Fimages[high(Fimages)].IsAlphaImage := true;
    result := Fimages[high(Fimages)];
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

Function TOpenGL_GraphikEngine.LoadAlphaColorGraphik(Filename: String;
  Color: TRGB; Stretch: TStretchmode): Integer;
Begin
  result := LoadAlphaColorGraphikItem(Filename, Color, Stretch).Image;
End;

Function TOpenGL_GraphikEngine.LoadAlphaColorGraphik(Const Graphik: TBitmap;
  Name: String; Color: TRGB; Stretch: TStretchmode): Integer;
Begin
  result := LoadAlphaColorGraphikitem(Graphik, Name, Color, Stretch).Image;
End;

Function TOpenGL_GraphikEngine.LoadAlphaColorGraphikItem(Filename: String;
  Color: TRGB; Stretch: TStretchmode): TGraphikItem; // Lädt eine Alphagraphik und setzt den Wert von Color = Transparent.
Var
  Data: String;
  temp, b: TBitmap;
  jp: TJPEGImage;
  png: TPortableNetworkGraphic;
  i: Integer;
{$IFDEF LCLGTK3}
  IntfImg: TLazIntfImage;
{$ENDIF}
Begin
  result.Image := 0;
  If Not FileExists(Filename) Then exit;
  Data := LowerCase(Filename);
  // Graphik bereits geladen
  For i := 0 To high(Fimages) Do
    If Fimages[i].Name = Data Then Begin
      result := Fimages[i];
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
    // FIX: convert to 32-Bit!
    If b.PixelFormat <> pf32bit Then Begin
      temp := TBitmap.Create;
      temp.PixelFormat := pf32bit;
      temp.SetSize(b.Width, b.Height);
{$IFDEF LCLGTK3}
      IntfImg := TLazIntfImage.Create(0, 0);
      IntfImg.LoadFromBitmap(b.Handle, b.MaskHandle);
      temp.LoadFromIntfImage(IntfImg);
      IntfImg.Free;
{$ELSE}
      temp.Canvas.Draw(0, 0, b);
{$ENDIF}
      b.Assign(temp);
      temp.Free;
    End;
  End;
  result := LoadAlphaColorGraphikitem(b, Filename, Color, Stretch);
  b.free;
End;

Function TOpenGL_GraphikEngine.LoadAlphaGraphik(Filename: String;
  Stretch: TStretchmode): Integer;
Begin
  result := LoadAlphaGraphikItem(Filename, Stretch).Image;
End;

Function TOpenGL_GraphikEngine.LoadAlphaGraphikItem(Filename: String;
  Stretch: TStretchmode): TGraphikItem;
Var
  OpenGLData: Array Of Array[0..3] Of Byte;
  tmp, AlphaMask: Array Of Array Of Byte;
  Data: String;
  b2, b: Tbitmap;
  jp: TJPEGImage;
  png: TPortableNetworkGraphic;
  nw, nh, ow, oh: Integer;
  IntfImg1: TLazIntfImage;
  CurColor: TFPColor;
  c, j, i: Integer;
  fi, fj, uu, vv, u, v: Single;
  xi, yi: integer;
{$IFDEF LEGACYMODE}
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
{$ENDIF}
Begin
  (*
  Die Funktion tut in keinster weise was oben steht das sie tun würde
  Da aber dass was tut in vielen Projekten Reicht ist sie nicht deaktiviert.
  Es sollte einem nur Klar sein dass hier evtl noch deutlich nachgebessert werden muß.
  *)
{$WARNING not Implemented correctly}
  AlphaMask := Nil;
  If FileExists(Filename) Then Begin
    Data := LowerCase(Filename);
    // Graphik bereits geladen
    For i := 0 To high(Fimages) Do Begin
      If Fimages[i].Name = Data Then Begin
        result := Fimages[i];
{$IFDEF DEBUGGOUTPUT}
        writeln('TGraphikEngine.LoadAlphaGraphik(' + Filename + ')');
        writeln('OpenGL Buffer : ' + FileSizetoString(OpenGLBufCount));
        writeln('TextureCount : ' + inttostr(high(Fimages) + 1));
{$ENDIF}
        exit;
      End;
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
      setlength(AlphaMask, b.Width, b.Height);
      IntfImg1 := b.CreateIntfImage;
      For i := 0 To b.Width - 1 Do Begin
        For j := 0 To b.Height - 1 Do Begin
          AlphaMask[i, j] := IntfImg1.Colors[i, j].Alpha Shr 8;
        End;
      End;
      IntfImg1.free;
      png.free;
    End
    Else Begin
      b.LoadFromFile(Filename);
    End;
    // create the raw image
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
            If Stretch = smStretch Then Begin
              Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imBilinear);
            End
            Else Begin
              Stretchdraw(b2.canvas, rect(0, 0, nw, nh), b, imNearestNeighbour);
            End;
            //{
            If assigned(AlphaMask) Then Begin
              If Stretch = smStretch Then Begin
                Raise exception.Create('TOpenGL_GraphikEngine.LoadAlphaGraphikItem: missing implementation for Bilinear');
              End;
              tmp := Nil;
              setlength(tmp, nw, nh);
              // Die Formeln sehen heftig aus, aber sind genau dass was raus
              // Kommt wenn man die ugraphic.pas variante "auspackt"
              For i := 0 To nw - 1 Do Begin
                u := i / (nw - 1);
                uu := u * b.Width;
                xi := trunc(uu);
                fi := uu - xi;
                For j := 0 To nh - 1 Do Begin
                  v := j / (nh - 1);
                  vv := v * b.Height;
                  yi := trunc(vv);
                  fj := vv - yi;
                  If fi <= 0.5 Then Begin
                    If fj <= 0.5 Then Begin
                      tmp[i, j] := AlphaMask[min(xi, high(AlphaMask)), min(yi, high(AlphaMask[0]))];
                    End
                    Else Begin
                      tmp[i, j] := AlphaMask[min(xi, high(AlphaMask)), min(yi + 1, high(AlphaMask[0]))];
                    End;
                  End
                  Else Begin
                    If fj <= 0.5 Then Begin
                      tmp[i, j] := AlphaMask[min(xi + 1, high(AlphaMask)), min(yi, high(AlphaMask[0]))];
                    End
                    Else Begin
                      tmp[i, j] := AlphaMask[min(xi + 1, high(AlphaMask)), min(yi + 1, high(AlphaMask[0]))];
                    End;
                  End;
                End;
              End;
              SetLength(AlphaMask, 0, 0);
              AlphaMask := tmp;
            End; //}
            b.free;
            b := b2;
            b2 := Nil;
            {
            If assigned(AlphaMask) Then Begin // TODO: Das geht zwar, aber mit Effizients hat das nichts mehr zu tun ... Der Obige Code ist schneller, aber kann Bilinear nicht ..
              ba := TBitmap.Create;
              ba.PixelFormat := pf24bit;
              ba.Width := length(AlphaMask);
              ba.Height := length(AlphaMask[0]);
              IntfImg1 := ba.CreateIntfImage;
              For i := 0 To ba.Width - 1 Do Begin
                For j := 0 To ba.Height - 1 Do Begin
                  CurColor.Red := AlphaMask[i, j] Shl 8;
                  CurColor.Green := AlphaMask[i, j] Shl 8;
                  CurColor.Blue := AlphaMask[i, j] Shl 8;
                  CurColor.Alpha := AlphaMask[i, j] Shl 8;
                  IntfImg1.Colors[i, j] := CurColor;
                End;
              End;
              ba.LoadFromIntfImage(IntfImg1);
              IntfImg1.free;
              b2 := TBitmap.create;
              b2.PixelFormat := pf24bit;
              b2.width := nw;
              b2.height := nh;
              If Stretch = smStretch Then Begin
                Stretchdraw(b2.canvas, rect(0, 0, nw, nh), ba, imBilinear);
              End
              Else Begin
                Stretchdraw(b2.canvas, rect(0, 0, nw, nh), ba, imNearestNeighbour);
              End;
              ba.free;
              IntfImg1 := b2.CreateIntfImage;
              setlength(AlphaMask, nw, nh);
              For i := 0 To b2.Width - 1 Do Begin
                For j := 0 To b2.Height - 1 Do Begin
                  CurColor := IntfImg1.Colors[i, j];
                  AlphaMask[i, j] := CurColor.Red Shr 8;
                End;
              End;
              IntfImg1.free;
              b2.free;
            End; //}
          End;
        End;
      smClamp: Begin
          nw := GetNextPowerOfTwo(b.width);
          nh := GetNextPowerOfTwo(b.height);
          If (nw <> b.width) Or (nh <> b.height) Then Begin
            If assigned(AlphaMask) Then Begin
              tmp := Nil;
              setlength(tmp, nw, nh);
              For i := 0 To nw - 1 Do Begin
                For j := 0 To nh - 1 Do Begin
                  If (i < b.width) And (j < b.height) Then Begin
                    tmp[i, j] := AlphaMask[i, j];
                  End
                  Else Begin
                    tmp[i, j] := 0;
                  End;
                End;
              End;
              setlength(AlphaMask, 0, 0);
              AlphaMask := tmp;
            End;
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
    IntfImg1 := b.CreateIntfImage;
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
          If assigned(AlphaMask) Then Begin
            OpenGLData[c, 3] := 255 - AlphaMask[i, j];
          End
          Else Begin
            If (OpenGLData[c, 0] = Fuchsia.r) And
              (OpenGLData[c, 1] = Fuchsia.g) And
              (OpenGLData[c, 2] = Fuchsia.b) Then
              OpenGLData[c, 3] := 255
            Else
              OpenGLData[c, 3] := 0;
          End;
          inc(c);
        End;
      End;
      // Übergeben an OpenGL
      glGenTextures(1, @Result);
{$IFDEF LEGACYMODE}
      bool := glIsEnabled(GL_TEXTURE_2D);
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        glEnable(GL_TEXTURE_2D);
{$ENDIF}
      glBindTexture(GL_TEXTURE_2D, result.Image);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, b.width, b.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0]);
{$IFDEF LEGACYMODE}
      If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
        gldisable(GL_TEXTURE_2D);
{$ENDIF}
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

Function TOpenGL_GraphikEngine.LoadAlphaGraphik(Const Graphik,
  AlphaMask: Tbitmap; Name: String; Stretch: TStretchmode): integer; // Lädt eine Graphik aus TBitmap, und Lädt den Alphakanal aus den Luminanzdaten von Alphamask, Name dient zum späteren Wiederfinden
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
{$IFDEF LEGACYMODE}
    bool := glIsEnabled(GL_TEXTURE_2D);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glEnable(GL_TEXTURE_2D);
{$ENDIF}
    glBindTexture(GL_TEXTURE_2D, result);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, g.width, g.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0]);
{$IFDEF LEGACYMODE}
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(GL_TEXTURE_2D);
    // entladen des texturspeichers
    If BOOL{$IFDEF USE_GL} = 1{$ENDIF} Then
      GLBindtexture(GL_texture_2d, 0);
{$ENDIF}
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
{$IFDEF LEGACYMODE}
  bool: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
{$ENDIF}
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
{$IFDEF LEGACYMODE}
    bool := glIsEnabled(GL_TEXTURE_2D);
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glEnable(GL_TEXTURE_2D);
{$ENDIF}
    glBindTexture(GL_TEXTURE_2D, result.Image);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, b.width, b.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, @OpenGLData[0, 0]);
{$IFDEF LEGACYMODE}
    If Not (Bool{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(GL_TEXTURE_2D);
{$ENDIF}
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
  For i := 0 To high(FImages) Do Begin
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
End;

Var
  i: integer;

Initialization

  OpenGL_GraphikEngine := TOpenGL_GraphikEngine.create;
  For i := 0 To 3599 Do Begin
    SinCos(DegToRad(i / 10), Sin_discrete[i], Cos_discrete[i]);
  End;

Finalization

  If assigned(OpenGL_GraphikEngine) Then Begin
    OpenGL_GraphikEngine.free;
    OpenGL_GraphikEngine := Nil;
  End;

End.

