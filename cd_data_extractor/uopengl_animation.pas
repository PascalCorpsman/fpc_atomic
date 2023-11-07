(******************************************************************************)
(* uopengl_animation.pas                                           15.09.2021 *)
(*                                                                            *)
(* Version     : 0.10                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : TOpenGL_Animation is a wrapper class that encapsulates the   *)
(*               TOpenGL_SpriteEngine into one .ani file                      *)
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
(*               0.02 - Entfernen Pseudo Name                                 *)
(*               0.03 - Einführen TAnimationMagicHeader                       *)
(*               0.04 - Unterstützung für FrameOffset                         *)
(*               0.05 - Fix FrameOffset wurde in GetFirstBitmap nicht         *)
(*                      berücksichtigt.                                       *)
(*               0.06 - Der AngleOffset muss in die TAniSprite !              *)
(*                      Fix GetFirstBitmap                                    *)
(*               0.07 - Support für OnAnimationOverflowEvent Callback         *)
(*               0.08 - Tag, ResetSprite                                      *)
(*               0.09 - OnAnimationOverflowEvent for user decisions           *)
(*               0.10 - FIX: changing only sprite name was not stored         *)
(*                                                                            *)
(******************************************************************************)

Unit uopengl_animation;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, uopengl_spriteengine, uopengl_graphikengine, Graphics;

Const
  TAnimationVersion: integer = 5;
  AnimationMagicHeader: String = 'CANIFILE'; // !! Achtung !! dieser String darf nie geändert werden !!!

Type

  (*
   * true = Continue as normal
   * false = hold on last valid frame
   *)
  TOnAnimationOverflowEvent = Function(Sender: TObject): Boolean Of Object;

  TAniSprite = Record
    (*
     * Alles Hier wird gespeichert
     *)
    StartAngle: integer; // Von Winkelbereich \ Bildet den Winkelbereich in dem die Animation verwendet wird
    EndAngle: integer; // bis Winkelbereich   /
    Derived: Boolean; // Wenn True, dann werden die Selben Bilddaten wie im Bild davor verwendet -> Spart Speicherplatz
    Bitmap: TBitmap; // Das Quellbild
    AlphaMask: TBitmap; // Optional, wenn AlphaMasken Verwendet werden die Alpha Maske
    TimePerFrame: integer; // Zeit in Ms bis zum Nächsten Frame weiter geschaltet wird
    Rect: TRect; // Das Rechteck in Pixel, welches von Bitmap und AlphaMask verwendet wird für die Animationen
    Name: String;
    AlphaImage: Boolean; // Wenn True, dann werden entweder Alphamasken oder CLFuchsia als Alpha verwendet.
    Width, Height: integer; // Größe in Pixeln auf die ein Sprite nachher hin Skalliert werden soll.
    FrameCount: integer; // Anzahl der Bilder
    FrameOffset: integer;
    FramesPerRow, FramesPerCol: integer;

    (*
     * Alles Hier wird nicht gespeichert, da Dynamisch Berechnet
     *)
    SpriteIndex: integer;
  End;

  { TOpenGL_Animation }

  TOpenGL_Animation = Class
  private
    fchanged: Boolean;
    fSprites: Array Of TAniSprite;
    fName: String; // Wird als Präfix für die OpenGL Graphik Engine verwendet -> Nutzt den Dateinamen als differnzierungsmerkmal
    Function getAniSprite(Index: integer): TAniSprite;
    Function getSpriteCount: integer;
    Procedure Clear;
    Procedure Init;
    Procedure EqualizeRanges;
    Procedure SetAniSprite(Index: integer; AValue: TAniSprite);

    Function SaveToStream(Const Stream: TStream): Boolean;
    Function LoadFromStream(Const Stream: TStream): Boolean;

    Function OnSpriteOverflow(Sender: TObject; MetaData: Pointer): Boolean;
  public
    Tag: PtrInt; // Kann der Owner nutzen wie er mag, ist default 0
    AngleOffset: Single; // Ein "Konstantes" Winkel Offset, welches immer drauf gerechnet wird !
    AnimationOffset: integer; // Damit kann man den "0-Durchgang" der Animation beeinflussen, damit nicht alle "synchronisiert" sind
    LastError: String;
    OnAnimationOverflowEvent: TOnAnimationOverflowEvent; // Callback die Aufgerufen wird, wenn eine Animation sich "Neu" started, nur Sinnvoll wenn die Animation genau 1 mal ablaufen soll und dann z.B. gestoppt werden soll.

    Property Changed: Boolean read fchanged;
    Property SpriteCount: integer read getSpriteCount;
    Property Sprite[Index: integer]: TAniSprite read getAniSprite write SetAniSprite;

    Constructor Create; virtual;
    Destructor Destroy; override;

    (*
     * Alles zur Nutzung
     * Werden Transparente Graphiken nicht richtig gerendert, bzw blockieren den Tiefenpuffer kann das daran liegen, dass
     * Der Alphatest ggf. nicht aktiviert wurde:

         glAlphaFunc(GL_LESS, 0.5);
         glEnable(GL_ALPHA_TEST);
         Render(Winkel);
         gldisable(GL_ALPHA_TEST);

     *)
    Procedure Render(Angle: Single);

    Procedure ResetAnimation(); // Setzt LastRenderTime = Gettickcount und AktualFrame = 0 => Restart der Animation

    Function Width(Angle: Single): integer;
    Function Height(Angle: Single): integer;

    Function GetFirstBitmap(): TBitmap; // Zum erstellen von Vorschau Bildern, verwendet wird Angle = 0, erstes Frame

    Function SaveToFile(Const Filename: String): Boolean;

    (*
     * Achtung, wenn InitOpenGLData = false, dann muss "CreateOpenGLData" für jeden Index aufgerufen werden !
     *)
    Function LoadFromFile(Const Filename: String; InitOpenGLData: Boolean = True): Boolean;
    Procedure CloneFrom(Const Animation: TOpenGL_Animation);

    (*
     * Alles zum Verwalten, sollte außer in Editoren nicht verwendet werden !
     *)
    Procedure AddRange(Equalize: Boolean = true);
    Procedure DeleteSprite(Index: integer; Equalize: Boolean = true);
    Function CreateOpenGLData(Index: Integer): Boolean; // Erzeugt die OpenGL Sachen für den Jeweiligen Index
    Procedure RemoveOpenGLData(Index: Integer; RemoveOpenGLImage: Boolean = false); // Das RemoveOpenGLImage darf nur im Animation Editor gesetzt werden, sonst kümmert sich da die OpenGL-GraphikEngine selbst darum.

    Function GetBitmapOf(Index: integer): TBitmap;
    Function GetDerivedIndexOf(Index: integer): Integer;
  End;

Operator = (a, b: TAniSprite): Boolean;

Implementation

Uses ugraphics;

Operator = (a, b: TAniSprite): Boolean;
Begin
  result :=
    (a.StartAngle = b.StartAngle)
    And (a.EndAngle = b.EndAngle)
    And (a.Derived = b.Derived)
    And (a.Bitmap = b.Bitmap)
    And (a.AlphaMask = b.AlphaMask)
    And (a.TimePerFrame = b.TimePerFrame)
    And (a.Rect = b.Rect)
    And (a.AlphaImage = b.AlphaImage)
    And (a.Width = b.Width)
    And (a.Height = b.Height)
    And (a.FrameOffset = b.FrameOffset)
    And (a.FrameCount = b.FrameCount)
    And (a.FramesPerRow = b.FramesPerRow)
    And (a.FramesPerCol = b.FramesPerCol)
    // TODO: Warum ist .name hier ausgeschlossen ? (Siehe Bugfix in "SetAniSprite")
    // And (a.Fname = b.Fname) -- Dynamisch erstellte Daten werden nicht verglichen
    // And (a.SpriteIndex = b.SpriteIndex) -- Dynamisch erstellte Daten werden nicht verglichen
  ;
End;

{ TOpenGL_Animation }

Constructor TOpenGL_Animation.Create;
Begin
  Inherited create;
  Tag := 0;
  OnAnimationOverflowEvent := Nil;
  AngleOffset := 0.0;
  AnimationOffset := 0;
  fSprites := Nil;
  Init();
  fchanged := false;
  fName := '-';
End;

Destructor TOpenGL_Animation.Destroy;
Begin
  Clear();
End;

Function TOpenGL_Animation.getSpriteCount: integer;
Begin
  result := Length(fSprites);
End;

Function TOpenGL_Animation.getAniSprite(Index: integer): TAniSprite;
Begin
  result := fSprites[index];
End;

Procedure TOpenGL_Animation.Clear;
Var
  i: Integer;
Begin
  OnAnimationOverflowEvent := Nil;
  fchanged := false;
  fName := '-';
  For i := 0 To high(fSprites) Do Begin
    If assigned(fSprites[i].Bitmap) Then fSprites[i].Bitmap.Free;
    If assigned(fSprites[i].AlphaMask) Then fSprites[i].AlphaMask.Free;
    If (fSprites[i].SpriteIndex <> -1) Then Begin
      OpenGL_SpriteEngine.RemoveSprite(fSprites[I].SpriteIndex);
      // Da Kümmert sich die OpenGL_GraphikEngine selbst drum, gibt es Mehrere Instanzen der Klasse würden sonst deren Texturen gelöscht werden !!
      // If (Not fSprites[i].Derived) Then Begin
      //   OpenGL_GraphikEngine.RemoveGraphik(OpenGL_SpriteEngine.Sprite[fSprites[I].SpriteIndex].Image);
      // End;
    End;
  End;
  setlength(fSprites, 0);
End;

Procedure TOpenGL_Animation.Init;
Begin
  Clear();
  AddRange(true);
  fchanged := false;
End;

Procedure TOpenGL_Animation.EqualizeRanges;
Var
  d: SizeInt;
  i: Integer;
Begin
  // Verteilt die Ranges der Sprites Äquidistant
  d := 360 Div length(fSprites);
  For i := 0 To high(fSprites) Do Begin
    fSprites[i].StartAngle := i * d;
    fSprites[i].EndAngle := (i + 1) * d - 1;
  End;
  fSprites[high(fSprites)].EndAngle := 360;
End;

Procedure TOpenGL_Animation.SetAniSprite(Index: integer; AValue: TAniSprite);
Begin
  If (fSprites[index] = AValue) And (fSprites[index].Name = AValue.Name) Then exit;
  fchanged := true;
  fSprites[index] := AValue;
End;

Procedure TOpenGL_Animation.Render(Angle: Single);
Var
  i: integer;
Begin
  angle := round(Angle + AngleOffset); // Nicht Ideal, aber sonst kann es vor kommen, dass Winkel nicht Angezeigt werden !
  While Angle >= 360 Do
    angle := Angle - 360;
  While Angle < 0 Do
    angle := Angle + 360;
  For i := 0 To high(fSprites) Do Begin
    If (Angle >= fSprites[i].StartAngle) And (Angle <= fSprites[i].EndAngle) Then Begin
      OpenGL_SpriteEngine.RenderSprite(fSprites[i].SpriteIndex, AnimationOffset);
      break;
    End;
  End;
End;

Procedure TOpenGL_Animation.ResetAnimation();
Var
  i: Integer;
Begin
  For i := 0 To high(fSprites) Do Begin
    OpenGL_SpriteEngine.ResetSprite(fSprites[i].SpriteIndex);
  End;
End;

Function TOpenGL_Animation.Width(Angle: Single): integer;
Var
  i: integer;
Begin
  result := 0;
  angle := round(Angle + AngleOffset); // Nicht Ideal, aber sonst kann es vor kommen, dass Winkel nicht Angezeigt werden !
  While Angle >= 360 Do
    angle := Angle - 360;
  While Angle < 0 Do
    angle := Angle + 360;
  For i := 0 To high(fSprites) Do Begin
    If (Angle >= fSprites[i].StartAngle) And (Angle <= fSprites[i].EndAngle) Then Begin
      result := fSprites[i].Width;
      break;
    End;
  End;
End;

Function TOpenGL_Animation.Height(Angle: Single): integer;
Var
  i: integer;
Begin
  result := 0;
  angle := round(Angle + AngleOffset); // Nicht Ideal, aber sonst kann es vor kommen, dass Winkel nicht Angezeigt werden !
  While Angle >= 360 Do
    angle := Angle - 360;
  While Angle < 0 Do
    angle := Angle + 360;
  For i := 0 To high(fSprites) Do Begin
    If (Angle >= fSprites[i].StartAngle) And (Angle <= fSprites[i].EndAngle) Then Begin
      result := fSprites[i].Height;
      break;
    End;
  End;
End;

Function TOpenGL_Animation.GetFirstBitmap: TBitmap;
Var
  img, b: TBitmap;
  Frame, dx, dy, Framex, Framey: Integer;
Begin
  b := TBitmap.create;
  b.Width := fSprites[0].Rect.Width;
  b.Height := fSprites[0].Rect.Height;
  // Ermitteln des Bildes inclusive Transparents
  If assigned(Sprite[0].AlphaMask) Then Begin
    img := MulImage(fSprites[0].Bitmap, Sprite[0].AlphaMask, true);
  End
  Else Begin
    img := TBitmap.Create;
    img.Assign(fSprites[0].Bitmap);
  End;
  // Das hier berücksichtigt,  wenn FrameOffset <> 0 !
  // Zoom auf die 1. Animagionsstufe
  Frame := fSprites[0].FrameOffset;
  dx := (fSprites[0].Rect.Right - fSprites[0].Rect.Left) Div fSprites[0].FramesPerRow;
  dy := (fSprites[0].Rect.Bottom - fSprites[0].Rect.Top) Div fSprites[0].FramesPerCol;
  Framex := (Frame Mod fSprites[0].FramesPerRow);
  Framey := (Frame Div fSprites[0].FramesPerRow);
  b.canvas.draw(-fSprites[0].Rect.Left - dx * FrameX, -fSprites[0].Rect.Top - dy * framey, img);
  // Zoom auf das 1. Teilbild
  b.width := b.width Div fSprites[0].FramesPerRow;
  b.Height := b.Height Div fSprites[0].FramesPerCol;
  result := TBitmap.Create;
  // Stretch auf "Zielgröße"
  result.Width := fSprites[0].Width;
  result.Height := fSprites[0].Height;
  Stretchdraw(result, rect(0, 0, fSprites[0].Width, fSprites[0].Height), b, imNearestNeighbour); // Bicubisch sieht zwar besser aus ruiniert aber die Transparenz
  // Aufräumen
  b.free;
  img.free;
End;

Procedure TOpenGL_Animation.AddRange(Equalize: Boolean);
Begin
  fchanged := true;
  setlength(fSprites, Length(fSprites) + 1);

  fSprites[high(fSprites)].Name := 'Sprite ' + inttostr(Length(fSprites));
  fSprites[high(fSprites)].SpriteIndex := -1;
  fSprites[high(fSprites)].AlphaImage := false;
  fSprites[high(fSprites)].Width := 16;
  fSprites[high(fSprites)].Height := 16;

  fSprites[high(fSprites)].FrameOffset := 0;
  fSprites[high(fSprites)].FrameCount := 1;
  fSprites[high(fSprites)].FramesPerRow := 1;
  fSprites[high(fSprites)].FramesPerCol := 1;

  fSprites[high(fSprites)].SpriteIndex := -1; // Nicht initialisiert
  //fSprites[high(fSprites)].StartAngle := 0; -- Wird unten gemacht
  //fSprites[high(fSprites)].EndAngle := 360; -- Wird unten gemacht
  fSprites[high(fSprites)].AlphaMask := Nil;
  fSprites[high(fSprites)].Bitmap := Nil;
  fSprites[high(fSprites)].Derived := false;
  fSprites[high(fSprites)].TimePerFrame := 100;

  If equalize Then Begin
    EqualizeRanges();
  End
  Else Begin
    // Nimmt dem Letzten Sprite vom Range
    Raise exception.create('TOpenGL_Animation.AddRange, Implementieren.');
  End;
End;

Procedure TOpenGL_Animation.DeleteSprite(Index: integer; Equalize: Boolean);
Var
  i: Integer;
Begin
  If Length(fSprites) = 1 Then exit;
  fchanged := true;
  For i := Index To High(fSprites) - 1 Do Begin
    fSprites[i] := fSprites[i + 1];
  End;
  SetLength(fSprites, High(fSprites));
  If equalize Then Begin
    EqualizeRanges();
  End
  Else Begin
    // Nimmt dem Letzten Sprite vom Range
    Raise exception.create('TOpenGL_Animation.AddRange, Implementieren.');
  End;
End;

Function TOpenGL_Animation.SaveToStream(Const Stream: TStream): Boolean;
Var
  i: Integer;
  b: Boolean;
Begin
  result := false;
  stream.Write(TAnimationVersion, sizeof(TAnimationVersion));
  (* Schreiben des Magic Headers, da die Länge Statisch ist, machen wir das ohne Read/WriteAnsistring ! => Spart die 4 Längenbytes *)
  stream.Write(AnimationMagicHeader[1], length(AnimationMagicHeader));
  stream.write(AngleOffset, sizeof(AngleOffset));
  i := length(fSprites);
  stream.Write(i, SizeOf(i));
  For i := 0 To high(fSprites) Do Begin
    stream.Write(fSprites[i].StartAngle, sizeof(fSprites[i].StartAngle));
    stream.Write(fSprites[i].EndAngle, sizeof(fSprites[i].EndAngle));
    stream.Write(fSprites[i].Derived, sizeof(fSprites[i].Derived));
    b := assigned(fSprites[i].Bitmap);
    stream.Write(b, sizeof(b));
    If b Then Begin
      fSprites[i].Bitmap.SaveToStream(Stream);
    End;
    b := assigned(fSprites[i].AlphaMask);
    stream.Write(b, sizeof(b));
    If b Then Begin
      fSprites[i].AlphaMask.SaveToStream(Stream);
    End;
    stream.Write(fSprites[i].TimePerFrame, sizeof(fSprites[i].TimePerFrame));
    stream.Write(fSprites[i].Rect, sizeof(fSprites[i].Rect));
    stream.WriteAnsiString(fSprites[i].Name);
    stream.Write(fSprites[i].AlphaImage, sizeof(fSprites[i].AlphaImage));
    stream.Write(fSprites[i].Width, sizeof(fSprites[i].Width));
    stream.Write(fSprites[i].Height, sizeof(fSprites[i].Height));
    stream.Write(fSprites[i].FrameOffset, sizeof(fSprites[i].FrameOffset));
    stream.Write(fSprites[i].FrameCount, sizeof(fSprites[i].FrameCount));
    stream.Write(fSprites[i].FramesPerRow, sizeof(fSprites[i].FramesPerRow));
    stream.Write(fSprites[i].FramesPerCol, sizeof(fSprites[i].FramesPerCol));
  End;
  result := true;
End;

Function TOpenGL_Animation.LoadFromStream(Const Stream: TStream): Boolean;
Var
  i: Integer;
  FileVersion: integer;
  b: Boolean;
  s: String;
Begin
  result := false;
  LastError := '';
  FileVersion := -1;
  stream.Read(FileVersion, sizeof(FileVersion));
  If FileVersion > TAnimationVersion Then Begin
    LastError := 'Invalid file version.';
    exit;
  End;
  (* Prüfen des Magic Headers, da die Länge Statisch ist, machen wir das ohne Read/WriteAnsistring ! => Spart die 4 Längenbytes *)
  If FileVersion >= 3 Then Begin
    s := '';
    setlength(s, length(AnimationMagicHeader));
    stream.Read(s[1], length(AnimationMagicHeader));
    If s <> AnimationMagicHeader Then Begin
      LastError := 'Invalid magic header.';
      exit;
    End;
  End;
  Clear;
  If FileVersion = 1 Then Begin // Die Fileversion 1 hatte noch einen String der als Name benutzt wurde -> wird nicht mehr benutzt
    Stream.ReadAnsiString;
  End;
  AngleOffset := 0;
  If FileVersion >= 5 Then Begin
    stream.Read(AngleOffset, SizeOf(AngleOffset));
  End;
  i := -1;
  stream.Read(i, SizeOf(i));
  setlength(fSprites, i);
  For i := 0 To high(fSprites) Do Begin
    stream.Read(fSprites[i].StartAngle, sizeof(fSprites[i].StartAngle));
    stream.Read(fSprites[i].EndAngle, sizeof(fSprites[i].EndAngle));
    stream.Read(fSprites[i].Derived, sizeof(fSprites[i].Derived));
    b := false;
    stream.Read(b, sizeof(b));
    If b Then Begin
      fSprites[i].Bitmap := TBitmap.Create;
      fSprites[i].Bitmap.LoadFromStream(Stream);
    End
    Else Begin
      fSprites[i].Bitmap := Nil;
    End;
    b := false;
    stream.Read(b, sizeof(b));
    If b Then Begin
      fSprites[i].AlphaMask := TBitmap.Create;
      fSprites[i].AlphaMask.LoadFromStream(Stream);
    End
    Else Begin
      fSprites[i].AlphaMask := Nil;
    End;
    stream.Read(fSprites[i].TimePerFrame, sizeof(fSprites[i].TimePerFrame));
    stream.Read(fSprites[i].Rect, sizeof(fSprites[i].Rect));
    fSprites[i].Name := stream.ReadAnsiString();
    stream.Read(fSprites[i].AlphaImage, sizeof(fSprites[i].AlphaImage));
    stream.Read(fSprites[i].Width, sizeof(fSprites[i].Width));
    stream.Read(fSprites[i].Height, sizeof(fSprites[i].Height));
    If FileVersion >= 4 Then Begin
      stream.Read(fSprites[i].FrameOffset, sizeof(fSprites[i].FrameOffset));
    End
    Else Begin
      fSprites[i].FrameOffset := 0;
    End;
    stream.Read(fSprites[i].FrameCount, sizeof(fSprites[i].FrameCount));
    stream.Read(fSprites[i].FramesPerRow, sizeof(fSprites[i].FramesPerRow));
    stream.Read(fSprites[i].FramesPerCol, sizeof(fSprites[i].FramesPerCol));
    (*
     * Hier muss alles was nicht Gespeichert wurde, also Dynamisch ist dennoch initialisiert werden !!
     *)
    fSprites[i].SpriteIndex := -1;
  End;
  Tag := 0;
  result := true;
End;

Function TOpenGL_Animation.OnSpriteOverflow(Sender: TObject; MetaData: Pointer
  ): boolean;
Begin
  (*
   * Default do not interrupt rendering
   *)
  result := true;
  If assigned(OnAnimationOverflowEvent) Then Begin
    (*
     * Callback is set -> Let the user decide what to do ..
     *)
    result := OnAnimationOverflowEvent(Self);
  End;
End;

Function TOpenGL_Animation.CreateOpenGLData(Index: Integer): Boolean;
Var
  d_index: integer;
  img, i: integer;
  imgName: String;
  r: TFRect;
  gi: TGraphikItem;
  sp: TSprite;
Begin
  result := False;
  LastError := '';
  // 0. Evtl alte Textur Platt machen
  RemoveOpenGLData(Index);

  // Bestimmen des Index der QuellTextur
  d_index := GetDerivedIndexOf(Index);

  // 1. die Textur Laden
  imgName := fName + 'Sprite' + fSprites[Index].Name;
  If fSprites[Index].Derived Then Begin
    img := OpenGL_SpriteEngine.Sprite[fSprites[d_Index].SpriteIndex].Image;
  End
  Else Begin
    If Not assigned(fSprites[Index].Bitmap) Then Begin
      LastError := 'No Bitmap defined';
      exit;
    End;
    If fSprites[Index].AlphaImage Then Begin
      If assigned(Sprite[index].AlphaMask) Then Begin
        If (fSprites[index].Bitmap.Width <> fSprites[index].AlphaMask.Width) Or
          (fSprites[index].Bitmap.Height <> fSprites[index].AlphaMask.Height) Then Begin
          LastError := 'Error, image and alphamask differ in dimension.';
          exit;
        End
        Else Begin
          img := OpenGL_GraphikEngine.LoadAlphaGraphik(fSprites[index].Bitmap, fSprites[index].AlphaMask, imgName + 'Image', smClamp);
        End;
      End
      Else Begin
        img := OpenGL_GraphikEngine.LoadAlphaColorGraphik(fSprites[index].Bitmap, imgName + 'Image', ugraphics.ColorToRGB(clFuchsia), smClamp);
      End;
    End
    Else Begin
      img := OpenGL_GraphikEngine.LoadGraphik(fSprites[index].Bitmap, imgName + 'Image', smClamp);
    End;
    // Sollten Nach dieser Textur "Abgeleitete" Kommen müssen diese nun alle auch angepasst werden
    For i := Index + 1 To high(fSprites) Do Begin
      If fSprites[i].Derived Then Begin
        fSprites[i].AlphaImage := fSprites[index].AlphaImage;
        fSprites[i].Width := fSprites[index].Width;
        fSprites[i].Height := fSprites[index].Height;
        fSprites[i].FramesPerRow := fSprites[index].FramesPerRow;
        fSprites[i].FramesPerCol := fSprites[index].FramesPerCol;
        fSprites[i].TimePerFrame := fSprites[index].TimePerFrame;
        (*
         * Hier werden nicht alle Werte übernommen, Ausgenommen sind:
         *   FrameCount, FrameOffset
         * Da sonst alle Abgeleiteten werte stets neu gesetzt werden müssten und das macht natürlich keinen Sinn
         *)
        If fSprites[i].SpriteIndex <> SpriteIndexInvalid Then Begin
          sp := OpenGL_SpriteEngine.Sprite[fSprites[i].SpriteIndex];
          OpenGL_SpriteEngine.RemoveSprite(fSprites[i].SpriteIndex);
          fSprites[i].SpriteIndex :=
            OpenGL_SpriteEngine.AddSprite(img, sp.Name, fSprites[i].AlphaImage,
            sp.Rect,
            fSprites[i].Width,
            fSprites[i].Height,
            fSprites[i].FramesPerRow,
            fSprites[i].FramesPerCol,
            fSprites[i].FrameOffset,
            fSprites[i].FrameCount,
            fSprites[i].TimePerFrame,
            sp.Callback, sp.MetaData);
        End;
      End
      Else Begin
        break;
      End;
    End;
  End;
  // Das Eigentliche Sprite Anlegen
  Try
    // 2. Das Sprite erstellen
    gi := OpenGL_GraphikEngine.GetInfo(img);
    // Bestimmen der Texturkoordinaten, Relativ zu Clamp
    r.Top := 0;
    r.Left := 0;
    r.Right := gi.OrigWidth / gi.StretchedWidth;
    r.Bottom := gi.OrigHeight / gi.StretchedHeight;

    // Umrechnen der Texturkoordinaten gemäß den User Eingaben
    If fSprites[index].Rect.Left = 0 Then Begin
      r.Left := 0;
    End
    Else Begin
      r.Left := r.Right / (fSprites[d_index].Bitmap.Width / fSprites[index].Rect.Left);
    End;
    If fSprites[index].Rect.Top = 0 Then Begin
      r.Top := 0;
    End
    Else Begin
      r.Top := r.Bottom / (fSprites[d_index].Bitmap.Height / fSprites[index].Rect.Top);
    End;
    r.Bottom := r.Bottom / (fSprites[d_index].Bitmap.Height / fSprites[index].Rect.Bottom);
    r.Right := r.Right / (fSprites[d_index].Bitmap.Width / fSprites[index].Rect.Right);

    fSprites[Index].SpriteIndex :=
      OpenGL_SpriteEngine.AddSprite(img, fName + imgName, fSprites[index].AlphaImage,
      r,
      fSprites[index].Width,
      fSprites[index].Height,
      fSprites[index].FramesPerRow,
      fSprites[index].FramesPerCol,
      fSprites[index].FrameOffset,
      fSprites[index].FrameCount,
      fSprites[index].TimePerFrame, @OnSpriteOverflow, Nil
      );
  Except
    On AV: Exception Do Begin
      LastError := av.Message;
      exit;
    End;
  End;
  result := true;
End;

Procedure TOpenGL_Animation.RemoveOpenGLData(Index: Integer;
  RemoveOpenGLImage: Boolean);
Var
  img: integer;
Begin
  If fSprites[Index].SpriteIndex <> -1 Then Begin
    img := OpenGL_SpriteEngine.Sprite[fSprites[Index].SpriteIndex].Image;
    OpenGL_SpriteEngine.RemoveSprite(fSprites[Index].SpriteIndex);
    If RemoveOpenGLImage Then Begin
      If Not fSprites[index].Derived Then Begin
        OpenGL_GraphikEngine.RemoveGraphik(img);
      End;
    End;
  End;
  fSprites[Index].SpriteIndex := -1;
End;

Function TOpenGL_Animation.GetBitmapOf(Index: integer): TBitmap;
Begin
  result := Sprite[GetDerivedIndexOf(Index)].Bitmap;
End;

Function TOpenGL_Animation.GetDerivedIndexOf(Index: integer): Integer;
Begin
  result := Index;
  While (Sprite[Result].Derived) Do Begin
    dec(Result);
  End;
End;

Function TOpenGL_Animation.SaveToFile(Const Filename: String): Boolean;
Var
  fs: TFileStream;
Begin
  result := false;
  fs := TFileStream.Create(Filename, fmCreate Or fmOpenWrite);
  result := SaveToStream(fs);
  fs.free;
End;

Function TOpenGL_Animation.LoadFromFile(Const Filename: String;
  InitOpenGLData: Boolean): Boolean;
Var
  fs: TFileStream;
  i: Integer;
Begin
  result := false;
  fs := TFileStream.Create(Filename, fmOpenRead);
  result := LoadFromStream(fs);
  fName := Filename;
  // Alles Geladen, nun initialisieren wir den OpenGL Teil direkt mal mit
  If InitOpenGLData Then Begin
    For i := 0 To high(fSprites) Do Begin
      CreateOpenGLData(i);
    End;
  End;
  fs.free;
End;

Procedure TOpenGL_Animation.CloneFrom(Const Animation: TOpenGL_Animation);
Var
  s: TMemoryStream;
  i: Integer;
Begin
  s := TMemoryStream.Create;
  Animation.SaveToStream(s);
  s.Position := 0;
  LoadFromStream(s);
  s.free;
  fname := Animation.fName;
  For i := 0 To high(fSprites) Do Begin
    CreateOpenGLData(i);
  End;
End;

End.

