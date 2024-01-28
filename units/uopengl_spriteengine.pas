(******************************************************************************)
(* uopengl_spriteengine.pas                                        ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : shows the implementation of a opengl sprite engine with all  *)
(*               its needs and callbacks, feel free to change anything you    *)
(*               want.                                                        *)
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
(*               0.02 - Do not render the overflow frame if callback is set   *)
(*               0.03 - let the code decide wheter to show overflow frame or  *)
(*                      not                                                   *)
(*                                                                            *)
(******************************************************************************)
Unit uopengl_spriteengine;

// Je nach dem wie mans Braucht mus hier leider angepasst werden!!

{.$DEFINE USE_GL}

{$MODE objfpc}{$H+}

Interface

Uses
  // Die OpenGL Schnittstelle
{$IFDEF USE_GL}
  gl, glu,
{$ELSE}
  dglOpenGL,
{$ENDIF}
  Classes, SysUtils,
  LCLIntf // Gettickcount
  , uopengl_graphikengine
  ;

Const
  SpriteIndexInvalid = -1; // Ein Index der "ungültig" ist

Type

  (*
   * true = Continue as normal
   * false = hold on last valid frame
   *)
  TSpriteCallback = Function(Sender: TObject; MetaData: Pointer): Boolean Of Object;

  TSprite = Record
    Active: Boolean; // Wenn ein Sprite "Gelöscht" wird -> false
    Image: Integer; // Die Textur die die Animation beinhaltet.
    Rect: TFRect; // das Rect, welches aus der Textur benutzt werden soll um die Frames zu generieren.
    Width, Height: Integer; // Gibt die Größe eines einzigen Frames an.
    FrameStart: Integer; // StartFrame
    FrameCount: Integer; // Anzahl der Frames
    FramesPerRow: Integer; // Anzahl der Frames Pro Zeile
    FramesPerCol: Integer; // Anzahl der Frames Pro Spalte
    AktualFrame: integer; // Aktuelle Frame Nummer
    ddw, ddh: Double; // In Normierten Koordinaten die Größe eines Frames, vorberechnet.
    dtTime: Integer; // Zeit in MS bis zum nächsten Frame
    LastRenderTime: QWord; // Zeitpunkt an dem der Letzte Frame wechsel war.
    Name: String; // Name des Frames
    AlphaImage: Boolean; // Wenn True, dann wird das Frame als AplhaGraphik gerendert.
    MetaData: Pointer; // Pointer auf MetaDaten
    Callback: TSpriteCallback; // Wenn der Framecounter auf 0 zurückspringt wird diese Callback aufgerufen.
  End;

  { TOpenGL_SpriteEngine }

  TOpenGL_SpriteEngine = Class
  private
    Fsprites: Array Of TSprite;

    Function getSprite(Index: Integer): TSprite;
    Function getSpriteCount: integer;
  public
    Enabled: Boolean; // Wenn False, werden die Animationen dennoch Berechnet / Gerendert aber Laufen nicht..
    Property SpriteCount: integer read getSpriteCount;
    Property Sprite[Index: Integer]: TSprite read getSprite;

    Constructor create;
    Destructor destroy; override;

    Procedure Clear; // alle Elemente Löschen.

    (*
     * Fügt ein Sprite hinzu und gibt dessen Index wieder Index in [0..oo[
     *)
    Function AddSprite(
      Image: Integer; // OpenGL_GraphikEngine Image Pointer (am besten mitttels smClamp erstellen, siehe Beispiel in Demo)
      Name: String; // Frei wählbarer Name für das Frame
      AlphaImage: Boolean; // Wenn das Sprite eine Transparente Graphik ist (clFuchsia = Transparent)
      Rect: TFRect; // Das Rechteck auf die Graphik bezogen in der die einzel Frames liegen (0..1)
      Width, Height, // Breite / Höhe mit dem das Frame am Ende gerendert werden soll [in Pixel]
      // Bezogen auf den oben gewählten Rect Ausschnitt
      FramesPerRow, // Anzahl Einzelbilder Pro Reihe
      FramesPerCol, // Anzahl Einzelbilder Pro Spalte
      FrameStartOffset, // Der Index ab welchem die Framecount Frames genommen werden sollen
      FrameCount, // Anzahl Einzelbilder die genutzt werden sollen
      dtTime: Integer; // Zeit in ms bis zum umschalten des nächsten Bildes
      Callback: TSpriteCallback; // Callback, die aufgerufen wird, wenn die Animation "Neu" gestartet wird.
      MetaData: Pointer // Vom User Frei wählbare "metadaten"
      ): Integer; // Fügt ein Sprite Hinzu und gibt dessen Index wieder Index in [0..oo[

    Procedure RemoveSprite(Index: integer);

    (*
     * Rendert das Sprite in das Rechteck (0,0,width,height)
     * AnimationOffset kann beliebig sein, sollte aber pro Object nicht verändert werden, da es sich direkt auf die Angezeigte Animation auswirkt!
     *)
    Procedure RenderSprite(Value: Integer; AnimationOffset_ms: integer = 0);

    Procedure ResetSprite(Value: Integer); // Setzt LastRenderTime = Gettickcount und AktualFrame = 0 => Restart der Animation

    Procedure SyncFrameIndex(Source, Dest: integer); // Synchronisiert die Animationssteps des Dest Sprites auf die des Dest Sprites
  End;

Const
  DefaultRect: TFRect = (Left: 0; Top: 0; Right: 1; Bottom: 1);

Var
  OpenGL_SpriteEngine: TOpenGL_SpriteEngine = Nil;

Implementation

Function TOpenGL_SpriteEngine.getSprite(Index: Integer): TSprite;
Begin
  result := Fsprites[index];
End;

Function TOpenGL_SpriteEngine.getSpriteCount: integer;
Begin
  result := length(Fsprites);
End;

Constructor TOpenGL_SpriteEngine.create;
Begin
  setlength(Fsprites, 0);
  Enabled := true;
End;

Destructor TOpenGL_SpriteEngine.destroy;
Begin
  clear;
End;

Procedure TOpenGL_SpriteEngine.Clear;
Begin
  setlength(Fsprites, 0);
End;

Function TOpenGL_SpriteEngine.AddSprite(Image: Integer; Name: String;
  AlphaImage: Boolean; Rect: TFRect; Width, Height, FramesPerRow, FramesPerCol,
  FrameStartOffset, FrameCount, dtTime: Integer; Callback: TSpriteCallback;
  MetaData: Pointer): Integer; // Fügt ein Sprite Hinzu
Var
  index, i: integer;
Begin
  name := lowercase(name);
  index := -1;
  // Shauen ob ein gelöschtes Sprite genutzt werden kann
  For i := 0 To High(Fsprites) Do
    If (Not Fsprites[i].Active) Then Begin
      index := i;
      break;
    End;
  // Hier und da ein paar Nebenbedingungen abchecken.
  If (FramesPerRow * FramesPerCol < FrameCount) Or
    ((FrameCount - 1) > FramesPerRow * FramesPerCol) Then
    Raise Exception.create('Error, Framecount for "' + name + '" is to much, for given FramePerRow, FramesPerCol settings.');
  If FrameCount < 1 Then
    Raise Exception.create('Error, Framecount for "' + name + '" hast to be 1 or more');
  If dtTime < 0 Then
    Raise Exception.create('Error invalid Value dtTime for "' + name + '"');
  // das Eigentliche Element hinzufügen
  If index = -1 Then Begin
    setlength(Fsprites, high(Fsprites) + 2);
    index := high(Fsprites);
  End;
  Fsprites[index].Active := true;
  Fsprites[index].Image := Image;
  Fsprites[index].Rect := Rect;
  Fsprites[index].Width := Width;
  Fsprites[index].Height := Height;
  Fsprites[index].FrameStart := FrameStartOffset;
  Fsprites[index].Framecount := FrameCount;
  Fsprites[index].FramesPerRow := FramesPerRow;
  Fsprites[index].FramesPerCol := FramesPerCol;
  Fsprites[index].AktualFrame := FrameStartOffset;
  Fsprites[index].dtTime := dtTime;
  Fsprites[index].LastRenderTime := gettickcount;
  Fsprites[index].Name := name;
  Fsprites[index].ddw := (Fsprites[index].Rect.Right - Fsprites[index].Rect.left) / Fsprites[index].FramesPerRow;
  Fsprites[index].ddh := (Fsprites[index].Rect.Bottom - Fsprites[index].Rect.Top) / Fsprites[index].FramesPerCol;
  Fsprites[index].AlphaImage := AlphaImage;
  Fsprites[index].Callback := Callback;
  Fsprites[index].MetaData := MetaData;
  result := index;
End;

Procedure TOpenGL_SpriteEngine.RemoveSprite(Index: integer);
Begin
  Fsprites[Index].Active := false;
End;

Procedure TOpenGL_SpriteEngine.RenderSprite(Value: Integer;
  AnimationOffset_ms: integer);
Var
  aTime: qWord;
  ddw, ddh, dw, dh: double;
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
  Frame, OldFrame, Steps: Integer;
Begin
  If (Value < 0) Or (value > high(Fsprites)) Then exit;
  (*
  Nur wenn es mehr wie ein Frame gibt, brauchen wir eine Weiterschaltung ...
  *)
  If Fsprites[value].Framecount <> 1 Then Begin
    atime := GetTickCount64;
    Steps := (aTime - Fsprites[value].LastRenderTime) Div Fsprites[value].dtTime;
    Fsprites[value].LastRenderTime := Fsprites[value].LastRenderTime + Steps * Fsprites[value].dtTime;
    OldFrame := Fsprites[value].AktualFrame;
    If enabled Then Begin
      Fsprites[value].AktualFrame := Fsprites[value].AktualFrame - Fsprites[Value].FrameStart;
      Fsprites[value].AktualFrame := (Fsprites[value].AktualFrame + Steps) Mod Fsprites[Value].FrameCount;
      Fsprites[value].AktualFrame := Fsprites[value].AktualFrame + Fsprites[Value].FrameStart;
    End;
    If assigned(Fsprites[value].Callback) And ((steps >= Fsprites[value].FrameCount + Fsprites[Value].FrameStart) Or (OldFrame > Fsprites[value].AktualFrame)) Then Begin
      (*
       * User did not want overflow, but not rendering the frame results in a visible glitch (looks like Z-Fighting) -> render the last frame again!
       *)
      If Not Fsprites[value].Callback(self, Fsprites[value].MetaData) Then Begin
        Fsprites[value].AktualFrame := Fsprites[value].FrameStart + Fsprites[Value].FrameCount;
      End;
    End;
  End;
  // Aus AktualFrame Berechnen wir nun den Teil der angesehen werden mus.
  Frame := Fsprites[value].AktualFrame;
  If AnimationOffset_ms <> 0 Then Begin
    frame := frame - Fsprites[Value].FrameStart;
    Frame := (Frame + (AnimationOffset_ms Div Fsprites[value].dtTime)) Mod Fsprites[Value].FrameCount;
    frame := frame + Fsprites[Value].FrameStart;
  End;
  ddw := Fsprites[value].ddw;
  ddh := Fsprites[value].ddh;
  dw := ddw * (Frame Mod Fsprites[value].FramesPerRow);
  dh := ddh * (Frame Div Fsprites[value].FramesPerRow);
  If Fsprites[value].AlphaImage Then Begin
    B := glIsEnabled(gl_Blend);
    If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
      glenable(gl_Blend);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  End;
  glBindTexture(gl_texture_2d, Fsprites[value].Image);
  glBegin(gl_quads);
  glTexCoord2f(Fsprites[value].Rect.left + dw, Fsprites[value].Rect.top + dh);
  glVertex2f(0, 0);
  glTexCoord2f(Fsprites[value].Rect.left + dw, Fsprites[value].Rect.top + dh + ddh);
  glVertex2f(0, Fsprites[value].height);
  glTexCoord2f(Fsprites[value].Rect.left + dw + ddw, Fsprites[value].Rect.top + dh + ddh);
  glVertex2f(Fsprites[value].Width, Fsprites[value].height);
  glTexCoord2f(Fsprites[value].Rect.left + dw + ddw, Fsprites[value].Rect.top + dh);
  glVertex2f(Fsprites[value].Width, 0);
  glend;
  If Fsprites[value].AlphaImage Then Begin
    If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
      gldisable(gl_blend);
  End;
End;

Procedure TOpenGL_SpriteEngine.ResetSprite(Value: Integer);
Begin
  If (Value < 0) Or (value > high(Fsprites)) Then exit;
  Fsprites[value].LastRenderTime := gettickcount;
  Fsprites[value].AktualFrame := Fsprites[value].FrameStart;
End;

Procedure TOpenGL_SpriteEngine.SyncFrameIndex(Source, Dest: integer);
Begin
  Fsprites[Dest].LastRenderTime := Fsprites[Source].LastRenderTime;
  Fsprites[Dest].AktualFrame := (Fsprites[Source].AktualFrame Mod Fsprites[Dest].FrameCount) + Fsprites[Dest].FrameStart;
End;

Initialization

  If assigned(OpenGL_SpriteEngine) Then
    OpenGL_SpriteEngine.free;
  OpenGL_SpriteEngine := TOpenGL_SpriteEngine.create;

Finalization

  If assigned(OpenGL_SpriteEngine) Then
    OpenGL_SpriteEngine.free;
  OpenGL_SpriteEngine := Nil;

End.

