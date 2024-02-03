(******************************************************************************)
(* uGraphiks.pas                                                   ??.??.???? *)
(*                                                                            *)
(* Version     : 0.11                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This unit gives a lot of usefull helper functions for        *)
(*               handling with bitmaps, canvas and color routines.            *)
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
(* History     : 0.01 - initial version                                       *)
(*               0.02 - added "SwapColor", "Stretchdraw"                      *)
(*               0.03 - added "ColorToRGBA"                                   *)
(*               0.04 - added "Color565ToRGB"                                 *)
(*               0.05 - Bugfix in Stretchdraw                                 *)
(*               0.06 - added Rotate*Degrees                                  *)
(*               0.07 - added LeftToRight                                     *)
(*               0.08 - added MulImage                                        *)
(*                      added aberation                                       *)
(*                      added vignetting                                      *)
(*                      added foldImage                                       *)
(*                      add Wrap Modes                                        *)
(*               0.09 - added floodfill                                       *)
(*               0.10 - FIX: revert 90* Rotation images back to old algorithm *)
(*               0.11 - FIX: revert 90* Rotations to matrix multiplications   *)
(*                                                                            *)
(******************************************************************************)

Unit ugraphics;

{$MODE objfpc}{$H+}

Interface

Uses
  LCLType, // HBitmap type
  IntfGraphics, // TLazIntfImage type
  fpImage, // TFPColor type
  classes, // Tpoint
  Graphics, // TBitmap
  uvectormath, // Clamp, TVector2
  math // max, min, arctan2
  ;

Type
  TInterpolationMode = (
    imNone, // Keine Interpolierung die Koordinaten werden immer auf trunc und dann direkt ausgegen => nur Sinnvoll bei 1:1 Abtastungen da sonst bildfehler entstehen können
    imNearestNeighbour, // Erzeugt keinen neuen Farbwerte, nimmt immer den Pixel der an "Nächsten" ist => sieht am schlechtesten aus, geht aber auch am schnellsten
    imBilinear, // Interpoliert, im Prinzip eine gewichtete Glättung der 4 direkt beteiligten Pixel
    imCosine, // Soll besser sein
    imBicubic // Interpoloert, im Prinzip eine gewichtetee Glättung der 16 Benachbarten Pixel (dadurch kann die Krümmung auch berücksichtigt werden)
    );

  TWrapMode = (
    wmBlack, // Ist ein Pixel nicht Teil des Bildes wird er Schwarz eingefärbt
    wmClamp, // Ist ein Pixel nicht Teil des Bildes wird seine Koordinate auf den Nächsten Pixel im Bild zurück Projiziert x >= Image.Width => Image.Width -1
    wmWrap //   Ist ein Pixel nicht Teil des Bildes wird seine Koordinate mittels Modulo in das Bild zurück Projiziert
    );

  TRGB = Record
    r, g, b: Byte;
  End;

  TRGBA = Record
    r, g, b, a: Byte;
  End;


  // https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_HSL
  THSL = Record
    h: integer; // [0..360[
    s: integer; // [0..255]
    L: Integer; // [0..255]
  End;

  THSV = Record
    h: integer; // [0..360[
    s: integer; // [0..255]
    L: Integer; // [0..255]
  End;

  // Farb Konvertierungen
Function Color565ToRGB(value: Word): TRGB; // Wandelt eine 2Byte Farbe in eine 3 Byte RGB Farbe um
Function RGB(r, g, b: Byte): TRGB;
Function ColorToRGB(c: TColor): TRGB;
Function ColorToRGBA(c: TColor; AlphaValue: byte = 0): TRGBA;
Function RGBToColor(rgb: TRGB): TColor;
Function FPColorToColor(Const Color: TFPColor): TColor; // Wandelt eine FPColor in TColor um
Function ColorToFPColor(Const Color: TColor): TFPColor; // Wandelt eine TColor in eine FPColor um
Function FPColorToV4(Const Color: TFPColor): TVector4; // Wandelt eine FPColor in TVector4 um
Function V4ToFPColor(Const V: TVector4): TFPColor; // Wandelt eine TVector4 in eine FPColor um

Function FPColorToHSL(Const Color: TFPColor): THSL;
Function HSLToFPColor(Const hsl: THSL): TFPColor;

Function FPColortoLuminanz(Value: TFPColor): Byte; // Berechnet die Helligkeit einer Farbe
Function ColortoLuminanz(Value: TColor): Byte; // Berechnet die Helligkeit einer Farbe

// Wandelt ein TBitmap in Graustufen um.
Procedure ConvertToGrayscale(Const Bitmap: TBitmap);
// Binarisiert ein Bild, dabei wird alles <= Border auf 0 und alles Größer auf 255 gesetzt.
Procedure Binarisate(Const Bitmap: TBitmap; Border: Byte);

//TODO: Bei den eindeutigen / nicht größen Ändernden iMode und wMode wieder Raus werfen !
// Spiegelt das Bild an der Horizontalen
Procedure UpSideDown(Const Bitmap: TBitmap);
// Spiegelt das Bild an der Vertikalen
Procedure LeftToRight(Const Bitmap: TBitmap);
// Dreht das Bild um 90° im Uhrzeigersinn
Procedure RotateClockWise90Degrees(Const Bitmap: TBitmap);
// Dreht das Bild um 90° gegen Uhrzeigersinn
Procedure RotateCounterClockWise90Degrees(Const Bitmap: TBitmap);
// Dreht das Bild um 180°
Procedure Rotate180Degrees(Const Bitmap: TBitmap);
// Rotiert das Bild um den angegeben Winkel im Gradmaß, die Frei werdenden Flächen sind Schwarz
Procedure RotateDegrees(Const Bitmap: TBitmap; Angle: Single; iMode: TInterpolationMode = imNearestNeighbour; wMode: TWrapMode = wmBlack);

(*
 * Entfernt aus einem Bild die Verzerrung die durch die Projektion entstanden ist.
 * Die beiden Strecken TopLeft, BottomLeft und TopRight, BottomRight definieren 2 Gleichlange Senkrechte Strecken im Zielbild
 * Anschliesend wird das Bild so umgerechnet, dass diese Geraden im Ergebnis Senkrechte sind.
 * Stretch = True => Das Zielbild ist nur der Inhalt der durch die 2 Senkrechten festgelegt wird
 *         = False => Das Zielbild in Summe wird gewandelt
 *)
Procedure CorrectProjection(Const Bitmap: TBitmap; TopLeft, BottomLeft, TopRight, BottomRight: TPoint; iMode: TInterpolationMode = imBilinear; wMode: TWrapMode = wmBlack; Stretch: Boolean = false);

(*
 * Multipliziert eine Matrix, mit einem Bild, Anwendung siehe RotateDegrees, UpSideDown, ..
 *)
Procedure MulImage(Const Bitmap: TBitmap; Matrix: TMatrix3x3; iMode: TInterpolationMode = imBilinear; wMode: TWrapMode = wmBlack); overload;

(*
 * Multipliziert Pixel für Pixel 2 Gleich Große Bilder und gibt das Ergebnis zurück
 * Das Ganze macht dann Sinn, wenn eins der Beiden Bilder ein Graustufen Bild ist, dass die Transparents darstellt
 *
 * Wenn InvertB2 = True => Alle Farbwerte von B2 werden dann 255-Farbwert gerechnet.
 *)
Function MulImage(Const B1: TBitmap; Const B2: TBitmap; InvertB2: Boolean = False): TBitmap; overload;

(*
 * Faltet eine Matrix mit einem Bild
 * ist AbsBeforSet = true, dann werden die zwischenergebnisse vor der Zuweisung ins Bild mit der ABS Funktion "Bereinigt"
 *)
Procedure FoldImage(Const Bitmap: TBitmap; Matrix: TMatrixNxM; AbsBeforeSet: Boolean = false);

(*
 * Addiert Pixelweise die Farbwerte 2er Bilder
 *)
Procedure AddImage(Const Dest, Source: TBitmap);

(*
 * Reduziert die Farben eines Bildes, Steps gibt dabei den Grad der Reduzierung an
 *
 * Sinnvoll ist Steps [2 .. 10]
 *)
Procedure PosterRice(Const Bitmap: TBitmap; Steps: Integer);

(*
 * Wendet eine Kissen/ Tonnenförmige Verzeichnung auf das Bild an
 * LensCenter in Pixelkoordinaten
 * k1, k2 = "Stärke" in x, y-Richtung 0 = Aus, < 0 = Kissenförmig > 0 = Tonnenförmig
 * Oversampling = 0 = Deaktiviert, > 0 = an (in der Regel reicht 1)
 * Zoom = Zoomen während der Transformation: 1 = Deaktiviert, nur werte > 0 sinnvoll
 *)
Procedure Aberration(Const Bitmap: Tbitmap; LensCenter: TVector2; k1, k2: Single; OverSampling: integer = 0; Zoom: Single = 1; iMode: TInterpolationMode = imNearestNeighbour; wMode: TWrapMode = wmBlack);

(*
 * Strength = [0.25 .. 10]
 * MinLevel = [0 .. 2]
 * MaxLevel = [0 .. 2]
 * Default "Verdunkeln" = 1, 1, 0
 * Default "Heller"     = 1, 1, 2
 *)
Procedure Vignetting(Const Bitmap: TBitmap; Strength, MinLevel, MaxLevel: Single);

(*
 * Addiert auf alle RGB den Wert von Value in [-255 .. 255]
 *)
Procedure Brightning(Const Bitmap: TBitmap; Value: integer);

(*
 * Erhöht, verringert den Kontrast des Bildes je nach Value in [-255 .. 255]
 *)
Procedure Contrast(Const Bitmap: TBitmap; contrast: integer);

// Tauscht die Farbe SourceColor mit der Farbe DestColor aus
Procedure SwapColor(Const Bitmap: TBitmap; SourceColor, DestColor: TColor);

(*
 * Füllt beginnend von StartX, StartY via Floodfill alles auf,
 *
 * Wenn AllowDiagonalWalk = True, dann wird zusätzlich auch über die Diagonalen gelaufen
 *)
Procedure FloodFill(Const Bitmap: TBitmap; StartX, StartY: Integer; DestColor: TColor; AllowDiagonalWalk: Boolean = false);

// Zeichnet ein Graphic im Biliniear, oder Nearest Neighbour verfahren. ( Unter Windows gibts das Bilinar nicht, unter Linux das NearesNeighbour *g* )
Procedure Stretchdraw(Const Dest: TBitmap; Destrect: Trect; Const Source: Tbitmap; Mode: TInterpolationMode = imNearestNeighbour (* GTK Default wäre imBilinear *)); overload;
Procedure Stretchdraw(Const Dest: TCanvas; Destrect: Trect; Const Source: Tbitmap; Mode: TInterpolationMode = imNearestNeighbour (* GTK Default wäre imBilinear *)); overload;

// Glätten Image mit einem Filter der Breite 2 * FilterRadius + 1
Procedure SmoothRound(Const Image: TBitmap; FilterRadius: integer); // -- Diese Routine ist eigentlich Unsinnig, da durch die Faltung das ganze eh runde Ecken bekommt... (Runder Kernel)
Procedure Smooth(Const Image: TBitmap; FilterRadius: integer); // (Eckiger Kernel)

// Zeichnet eine Pfeil mit Spitze von StartPoint nach Endpoint auf das TCanvas, und nutzt dabei die pen.color
Procedure RenderArrow(Const Canvas: TCanvas; StartPoint: TPoint; EndPoint: TPoint);

// Zum Konvertieren
Function StringToInterpolationMode(Value: String): TInterpolationMode;
Function InterpolationModeToString(Value: TInterpolationMode): String;

Implementation

Uses sysutils, ufifo; // Exception

Function StringToInterpolationMode(Value: String): TInterpolationMode;
Begin
  Case lowercase(trim(Value)) Of
    'nearestneighbour': result := imNearestNeighbour;
    'bilinear': result := imBilinear;
    'cosine': result := imCosine;
    'bicubic': result := imBicubic;
  Else
    result := imNone;
  End;
End;

Function InterpolationModeToString(Value: TInterpolationMode): String;
Begin
  Case value Of
    imNone: result := 'None';
    imNearestNeighbour: result := 'NearestNeighbour';
    imBilinear: result := 'Bilinear';
    imCosine: result := 'Cosine';
    imBicubic: result := 'Bicubic';
  Else Begin
      result := 'InterpolationModeToString: missing implementation.';
    End;
  End;
End;

Function Color565ToRGB(value: Word): TRGB;
Begin
  result.r := clamp(round(((value Shr 11) And $1F) * 255 / $1F), 0, 255);
  result.g := clamp(round(((value Shr 5) And $3F) * 255 / $3F), 0, 255);
  result.b := clamp(round(((value Shr 0) And $1F) * 255 / $1F), 0, 255);
End;

Function RGB(r, g, b: Byte): TRGB;
Begin
  result.r := r;
  result.g := g;
  result.b := b;
End;

Function ColorToRGB(c: TColor): TRGB;
Begin
  result.r := ($FF And C);
  result.g := ($FF00 And C) Shr 8;
  result.b := ($FF0000 And C) Shr 16;
End;

Function ColorToRGBA(c: TColor; AlphaValue: byte): TRGBA;
Begin
  result.r := ($FF And C);
  result.g := ($FF00 And C) Shr 8;
  result.b := ($FF0000 And C) Shr 16;
  result.a := AlphaValue;
End;

Function RGBToColor(rgb: TRGB): TColor;
Begin
  result := rgb.r Or (rgb.g Shl 8) Or (rgb.b Shl 16);
End;

Function FPColorToColor(Const Color: TFPColor): TColor;
Begin
  result := byte(color.red Shr 8) Or (color.green And $FF00) Or ((color.blue And $FF00) Shl 8);
End;

Function ColorToFPColor(Const Color: TColor): TFPColor;
Begin
  result.alpha := 65535;
  result.red := (byte(Color) Shl 8) Or $FF;
  result.green := ((Color) And $FF00) Or $FF;
  result.blue := (((Color) And $FF0000) Shr 8) Or $FF;
  If result.red = 255 Then Result.red := 0;
  If result.green = 255 Then Result.green := 0;
  If result.blue = 255 Then Result.blue := 0;
End;

Function FPColorToV4(Const Color: TFPColor): TVector4;
Begin
  result.x := (Color.Red Shr 8) / 255;
  result.y := (Color.Green Shr 8) / 255;
  result.z := (Color.Blue Shr 8) / 255;
  result.w := (Color.Alpha Shr 8) / 255;
End;

Function V4ToFPColor(Const V: TVector4): TFPColor;
Begin
  result.Red := clamp(round(v.x * 255), 0, 255) Shl 8;
  result.Green := clamp(round(v.y * 255), 0, 255) Shl 8;
  result.Blue := clamp(round(v.z * 255), 0, 255) Shl 8;
  result.Alpha := clamp(round(v.w * 255), 0, 255) Shl 8;
End;

// Quelle: https://www.pocketmagic.net/enhance-saturation-in-images-programatically/

Function FPColorToHSL(Const Color: TFPColor): THSL;
Var
  maxColor, minColor, r, g, b, d, l, h, s: Single;
Begin
  r := (Color.Red Shr 8) / 255.0;
  g := (Color.Green Shr 8) / 255.0;
  b := (Color.Blue Shr 8) / 255.0;
  // Then, minColor and maxColor are defined. Mincolor is the value of the color component with
  // the smallest value, while maxColor is the value of the color component with the largest value.
  // These two variables are needed because the Lightness is defined as (minColor + maxColor) / 2.
  maxColor := MAX(r, MAX(g, b));
  minColor := MIN(r, MIN(g, b));
  // If minColor equals maxColor, we know that R=G=B and thus the color is a shade of gray.
  // This is a trivial case, hue can be set to anything, saturation has to be set to 0 because
  // only then it's a shade of gray, and lightness is set to R=G=B, the shade of the gray.
  //R == G == B, so it's a shade of gray
  If ((r = g) And (g = b)) Then Begin
    h := 0.0; //it doesn't matter what value it has
    s := 0.0;
    l := r; //doesn't matter if you pick r, g, or b
  End
    // If minColor is not equal to maxColor, we have a real color instead of a shade of gray,
    // so more calculations are needed:
    // Lightness (l) is now set to it's definition of (minColor + maxColor)/2.
    // Saturation (s) is then calculated with a different formula depending if light is in the first
    // half of the second half. This is because the HSL model can be represented as a double cone, the
    // first cone has a black tip and corresponds to the first half of lightness values, the second cone
    // has a white tip and contains the second half of lightness values.
    // Hue (h) is calculated with a different formula depending on which of the 3 color components is
    // the dominating one, and then normalized to a number between 0 and 1.
  Else Begin
    d := maxColor - minColor;
    l := (minColor + maxColor) / 2;
    If (l < 0.5) Then Begin
      s := d / (maxColor + minColor);
    End
    Else Begin
      s := d / (2.0 - maxColor - minColor);
    End;
    If (r = maxColor) Then Begin
      h := (g - b) / (maxColor - minColor);
    End
    Else Begin
      If (g = maxColor) Then Begin
        h := 2.0 + (b - r) / (maxColor - minColor);
      End
      Else Begin
        h := 4.0 + (r - g) / (maxColor - minColor);
      End;
    End;
    h := h / 6; //to bring it to a number between 0 and 1
    If (h < 0) Then h := h + 1;
  End;
  // Finally, H, S and L are calculated out of h,s and l as integers between 0..360 / 0 and 255 and
  // "returned"  as the result.
  result.h := clamp(round(h * 360), 0, 360) Mod 360;
  result.s := clamp(round(s * 255), 0, 255);
  result.L := clamp(round(l * 255), 0, 255);
End;

// Quelle: https://www.pocketmagic.net/enhance-saturation-in-images-programatically/

Function HSLToFPColor(Const hsl: THSL): TFPColor;
Var
  r, g, b, tempr, tempg, tempb, temp1, temp2, h, s, l: Single;
Begin
  h := (hsl.h Mod 360) / 360.0;
  s := hsl.s / 255.0;
  l := hsl.l / 255.0;
  // Then follows a trivial case: if the saturation is 0, the color will be a grayscale color,
  // and the calculation is then very simple: r, g and b are all set to the lightness.
  // If saturation is 0, the color is a shade of gray
  If (s = 0) Then Begin
    r := l;
    g := l;
    b := l;
  End
    // If the saturation is higher than 0, more calculations are needed again. red, green and blue
    // are calculated with the formulas defined in the code.
    // If saturation > 0, more complex calculations are needed
  Else Begin
    //Set the temporary values
    If (l < 0.5) Then Begin
      temp2 := l * (1 + s);
    End
    Else Begin
      temp2 := (l + s) - (l * s);
    End;
    temp1 := 2 * l - temp2;

    tempr := h + 1.0 / 3.0;
    If (tempr > 1) Then tempr := tempr - 1;
    tempg := h;
    tempb := h - 1.0 / 3.0;
    If (tempb < 0) Then tempb := tempb + 1;

    // Red
    If (tempr < 1.0 / 6.0) Then Begin
      r := temp1 + (temp2 - temp1) * 6.0 * tempr;
    End
    Else Begin
      If (tempr < 0.5) Then Begin
        r := temp2;
      End
      Else Begin
        If (tempr < 2.0 / 3.0) Then Begin
          r := temp1 + (temp2 - temp1) * ((2.0 / 3.0) - tempr) * 6.0;
        End
        Else Begin
          r := temp1;
        End;
      End;
    End;

    // Green
    If (tempg < 1.0 / 6.0) Then Begin
      g := temp1 + (temp2 - temp1) * 6.0 * tempg;
    End
    Else Begin
      If (tempg < 0.5) Then Begin
        g := temp2;
      End
      Else Begin
        If (tempg < 2.0 / 3.0) Then Begin
          g := temp1 + (temp2 - temp1) * ((2.0 / 3.0) - tempg) * 6.0;
        End
        Else Begin
          g := temp1;
        End;
      End;
    End;

    // Blue
    If (tempb < 1.0 / 6.0) Then Begin
      b := temp1 + (temp2 - temp1) * 6.0 * tempb;
    End
    Else Begin
      If (tempb < 0.5) Then Begin
        b := temp2;
      End
      Else Begin
        If (tempb < 2.0 / 3.0) Then Begin
          b := temp1 + (temp2 - temp1) * ((2.0 / 3.0) - tempb) * 6.0;
        End
        Else Begin
          b := temp1;
        End;
      End;
    End;
  End;

  //And finally, the results are returned as integers between 0 and 255.
  result.Red := clamp(round(r * 255), 0, 255) Shl 8;
  result.Green := clamp(round(g * 255), 0, 255) Shl 8;
  result.Blue := clamp(round(b * 255), 0, 255) Shl 8;
  result.Alpha := 255 Shr 8;
End;

Function FPColortoLuminanz(Value: TFPColor): Byte;
Begin
  //Y = 0.3R + 0.59G + 0.11B
  result := min(255, max(0,
    round(
    (value.Red Shr 8) * 0.3 +
    (value.green Shr 8) * 0.59 +
    (value.blue Shr 8) * 0.11
    )));
End;

Function ColortoLuminanz(Value: TColor): Byte;
Var
  c: TFPColor;
Begin
  c.red := (value And $FF) Shl 8;
  c.green := (value And $FF00);
  c.blue := (value And $FF0000) Shr 8;
  c.alpha := $FF00;
  result := FPColortoLuminanz(c);
End;

(*
 * Interpoliert 2 Farbwerte gemäß f in [0..1]
 *)

Function InterpolateLinear(c1, c2: TFPColor; f: Single): TFPColor;
//Var
//  res, a, b: TVector4; // -- Diese Variante ist zwar Eleganter, aber auch deutlich Langsammer :(
Var
  r, g, b, a, r1, r2, g1, g2, b1, b2, a1, a2: integer;
Begin
  //  a := FPColorToV4(c1);
  //  b := FPColorToV4(c2);
  //  res := a * (1 - f) + b * f;
  //  result := V4ToFPColor(res);
  r1 := c1.Red Shr 8;
  r2 := c2.Red Shr 8;
  g1 := c1.Green Shr 8;
  g2 := c2.Green Shr 8;
  b1 := c1.Blue Shr 8;
  b2 := c2.Blue Shr 8;
  a1 := c1.Alpha Shr 8;
  a2 := c2.Alpha Shr 8;

  r := round(r2 * f + r1 * (1 - f));
  g := round(g2 * f + g1 * (1 - f));
  b := round(b2 * f + b1 * (1 - f));
  a := round(a2 * f + a1 * (1 - f));

  r := Clamp(r, 0, 255);
  g := Clamp(g, 0, 255);
  b := Clamp(b, 0, 255);
  a := Clamp(a, 0, 255);

  result.Red := r Shl 8;
  result.Green := g Shl 8;
  result.Blue := b Shl 8;
  result.Alpha := a Shl 8;
End;

(*
 * Interpoliert 2 Farbwerte gemäß f in [0..1]
 *)

Function InterpolateCos(c1, c2: TFPColor; f: Single): TFPColor; // Inspired by: https://github.com/samhooke/PerlinNoise/blob/master/Noise.cs  function: InterpolateCosine
Var
  r, g, b, a, r1, r2, g1, g2, b1, b2, a1, a2: integer;
  ft: Single;
Begin
  ft := f * pi;
  f := (1 - cos(ft)) * 0.5;

  r1 := c1.Red Shr 8;
  r2 := c2.Red Shr 8;
  g1 := c1.Green Shr 8;
  g2 := c2.Green Shr 8;
  b1 := c1.Blue Shr 8;
  b2 := c2.Blue Shr 8;
  a1 := c1.Alpha Shr 8;
  a2 := c2.Alpha Shr 8;

  r := round(r2 * f + r1 * (1 - f));
  g := round(g2 * f + g1 * (1 - f));
  b := round(b2 * f + b1 * (1 - f));
  a := round(a2 * f + a1 * (1 - f));

  r := Clamp(r, 0, 255);
  g := Clamp(g, 0, 255);
  b := Clamp(b, 0, 255);
  a := Clamp(a, 0, 255);

  result.Red := r Shl 8;
  result.Green := g Shl 8;
  result.Blue := b Shl 8;
  result.Alpha := a Shl 8;
End;

(*
 * Interpoliert 4 Farbwerte Mittels Polynim 3. Grades f in [0..1]
 * c1, c2, f sind Identisch zu den Parametern von z.B.: InterpolateLinear
 * c0 = Vorgänger von c1
 * c2 = Nachfolger von c2
 *)

Function InterpolateCubic(c0, c1, c2, c3: TFPColor; f: Single): TFPColor; // Inspired by: https://github.com/samhooke/PerlinNoise/blob/master/Noise.cs  function: InterpolateCubic
Var
  v: Array[0..3] Of TVector4;
  res, p, q, r, s, d: TVector4;
  x2, x3: Single;
Begin
  // TODO: Umschreiben auf "Ausgerollt", dass dürfte dann deutlich schneller sein (siehe InterpolateLinear)
  v[0] := FPColorToV4(c0);
  v[1] := FPColorToV4(c1);
  v[2] := FPColorToV4(c2);
  v[3] := FPColorToV4(c3);
  d := (v[0] - v[1]);
  p := (v[3] - v[2]) - d;
  q := d - p;
  r := v[2] - v[0];
  s := v[1];
  x2 := sqr(f);
  x3 := x2 * f;
  res := p * x3 + q * x2 + r * f + s;
  result := V4ToFPColor(res);
End;

(*
 * Gibt den Interpolierten Pixel an x,y zurück, clblack, wenn Außerhalb,
 *)

Function GetPixel(Const Image: TLazIntfImage; x, y: Single; wMode: TWrapMode; iMode: TInterpolationMode): TFPColor;

  Function PointToColor(p: TPoint): TFPColor;
  Begin
    Case wMode Of
      wmBlack: Begin
          If (p.x < 0) Or (p.y < 0) Or (p.x >= Image.Width) Or (p.y >= Image.Height) Then Begin
            result.Red := 0;
            result.Green := 0;
            result.Blue := 0;
            result.Alpha := 0;
          End
          Else Begin
            result := Image.Colors[p.x, p.y];
          End;
        End;
      wmClamp: Begin
          p.x := Clamp(p.x, 0, Image.Width - 1);
          p.y := Clamp(p.y, 0, Image.Height - 1);
          result := Image.Colors[p.x, p.y];
        End;
      wmWrap: Begin
          p.x := Mod2(p.x, Image.Width);
          p.y := Mod2(p.y, Image.Height);
          result := Image.Colors[p.x, p.y];
        End;
    End;
  End;

Var
  p: Array[0..15] Of TPoint;
  c: Array[0..15] Of TFPColor;
  xi, yi: integer;
  fx, fy: Single;
  i: Integer;
  a: Array[0..3] Of TFPColor;
Begin
  result.Red := 0;
  result.Green := 0;
  result.Blue := 0;
  result.Alpha := 0;
  xi := trunc(x);
  yi := trunc(y);
  // Alle Modi, welche die 4 Nachbarpixel benötigen
  (*
   * Das Problem ist das ein Pixel Links oben Angeschlagen ist und nicht Mittig
   * deswegen verschiebt es alles um 0,5 Nach Links oben :(
   *)
  If iMode In [imCosine, imBilinear, imNearestNeighbour] Then Begin
    fx := x - xi;
    fy := y - yi;
    (*  Zuordnung Array Index Relativ zu "x" = zu Interpolierender Punkt
     *   0 1
     *    x
     *   2 3
     *)
    p[0] := point(xi, yi);
    p[1] := point(xi + 1, yi);
    p[2] := point(xi, yi + 1);
    p[3] := point(xi + 1, yi + 1);
    For i := 0 To 3 Do Begin
      c[i] := PointToColor(p[i]);
    End;
  End;

  If (iMode = imBicubic) Then Begin // Hier brauchen wir 16 Punkte
    fx := x - xi;
    fy := y - yi;
    (*  Zuordnung Array Index Relativ zu "x" = zu Interpolierender Punkt
     *   0  1  2  3
     *   4  5  6  7
     *       x
     *   8  9 10 11
     *  12 13 14 15
     *)
    p[0] := point(xi - 1, yi - 1);
    p[1] := point(xi, yi - 1);
    p[2] := point(xi + 1, yi - 1);
    p[3] := point(xi + 2, yi - 1);
    p[4] := point(xi - 1, yi);
    p[5] := point(xi, yi);
    p[6] := point(xi + 1, yi);
    p[7] := point(xi + 2, yi);
    p[8] := point(xi - 1, yi + 1);
    p[9] := point(xi, yi + 1);
    p[10] := point(xi + 1, yi + 1);
    p[11] := point(xi + 2, yi + 1);
    p[12] := point(xi - 1, yi + 2);
    p[13] := point(xi, yi + 2);
    p[14] := point(xi + 1, yi + 2);
    p[15] := point(xi + 2, yi + 2);
    For i := 0 To 15 Do Begin
      c[i] := PointToColor(p[i]);
    End;
  End;

  Case imode Of
    imNone: Begin
        result := PointToColor(Point(xi, yi));
      End;
    imNearestNeighbour: Begin
        If fx <= 0.5 Then Begin
          If fy <= 0.5 Then Begin
            result := c[0];
          End
          Else Begin
            result := c[2];
          End;
        End
        Else Begin
          If fy <= 0.5 Then Begin
            result := c[1];
          End
          Else Begin
            result := c[3];
          End;
        End;
      End;
    imBilinear: Begin
        a[0] := InterpolateLinear(c[0], c[2], fy);
        a[1] := InterpolateLinear(c[1], c[3], fy);
        result := InterpolateLinear(a[0], a[1], fx);
      End;
    imCosine: Begin
        a[0] := InterpolateCos(c[0], c[2], fy);
        a[1] := InterpolateCos(c[1], c[3], fy);
        result := InterpolateCos(a[0], a[1], fx);
      End;
    imBicubic: Begin
        a[0] := InterpolateCubic(c[0], c[4], c[8], c[12], fy);
        a[1] := InterpolateCubic(c[1], c[5], c[9], c[13], fy);
        a[2] := InterpolateCubic(c[2], c[6], c[10], c[14], fy);
        a[3] := InterpolateCubic(c[3], c[7], c[11], c[15], fy);
        result := InterpolateCubic(a[0], a[1], a[2], a[3], fx);
      End;
  End;
End;

Procedure SetPixel(Const Image: TLazIntfImage; x, y: Single; c: TFPColor);
Var
  xi, yi: integer;
Begin
  xi := trunc(x);
  yi := trunc(y);
  If (xi >= 0) And (yi >= 0) And (xi < image.Width) And (yi < Image.Height) Then Begin
    Image.Colors[xi, yi] := c;
  End;
End;

Procedure ConvertToGrayscale(Const Bitmap: TBitmap);
Var
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  CurColor: TFPColor;
  b: Byte;
  i, j: Integer;
Begin
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  For j := 0 To bitmap.height - 1 Do
    For i := 0 To bitmap.width - 1 Do Begin
      b := FPColortoLuminanz(TempIntfImg.Colors[i, j]);
      curcolor.red := word(b) Shl 8;
      curcolor.green := curcolor.red;
      curcolor.blue := curcolor.red;
      curcolor.alpha := 255 * 256;
      TempIntfImg.Colors[i, j] := curcolor;
    End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
End;

Procedure UpSideDown(Const Bitmap: TBitmap);
Var
  m: TMatrix3x3;
Begin
  m := IdentityMatrix3x3;
  m[0, 0] := 1;
  m[1, 1] := -1;
  // Revert Middlepoint shifting
  m[0, 2] := 0.5;
  m[1, 2] := 0.5;
  MulImage(Bitmap, m, imNone, wmBlack);
End;

Procedure LeftToRight(Const Bitmap: TBitmap);
Var
  m: TMatrix3x3;
Begin
  m := IdentityMatrix3x3;
  m[0, 0] := -1;
  m[1, 1] := 1;
  // Revert Middlepoint shifting
  m[0, 2] := 0.5;
  m[1, 2] := 0.5;
  MulImage(Bitmap, m, imNone, wmBlack);
End;

Procedure RotateClockWise90Degrees(Const Bitmap: TBitmap);
Var
  m: TMatrix3x3;
Begin
  m := IdentityMatrix3x3;
  m[0, 0] := 0;
  m[1, 1] := 0;
  m[0, 1] := -1;
  m[1, 0] := 1;
  // Revert Middlepoint shifting
  m[0, 2] := 0.5;
  m[1, 2] := 0.5;
  MulImage(Bitmap, m, imNone, wmBlack);
End;

Procedure RotateCounterClockWise90Degrees(Const Bitmap: TBitmap);
Var
  m: TMatrix3x3;
Begin
  m := IdentityMatrix3x3;
  m[0, 0] := 0;
  m[1, 1] := 0;
  m[0, 1] := 1;
  m[1, 0] := -1;
  // Revert Middlepoint shifting
  m[0, 2] := 0.5;
  m[1, 2] := 0.5;
  MulImage(Bitmap, m, imNone, wmBlack);
End;

Procedure Rotate180Degrees(Const Bitmap: TBitmap);
Var
  m: TMatrix3x3;
Begin
  m := IdentityMatrix3x3;
  m[0, 0] := -1;
  m[1, 1] := -1;
  // Revert Middlepoint shifting
  m[0, 2] := 0.5;
  m[1, 2] := 0.5;
  MulImage(Bitmap, m, imNone, wmBlack);
End;

Procedure RotateDegrees(Const Bitmap: TBitmap; Angle: Single;
  iMode: TInterpolationMode; wMode: TWrapMode);
Var
  mr: TMatrix2x2;
  m: TMatrix3x3;
  i, j: Integer;
Begin
  // Berechnen der Drehmatrix und aufziehen auf 3x3
  mr := CalculateRotationMatrix(angle);
  m := IdentityMatrix3x3;
  For i := 0 To 1 Do Begin
    For j := 0 To 1 Do Begin
      m[i, j] := mr[i, j];
    End;
  End;
  MulImage(Bitmap, m, imode, wmode);
End;

Procedure CorrectProjection(Const Bitmap: TBitmap; TopLeft, BottomLeft,
  TopRight, BottomRight: TPoint; iMode: TInterpolationMode; wMode: TWrapMode;
  Stretch: Boolean);

(*
 * Lösen der Allgemeinen Gleichung der Perspektifischen Verzeichnung durch Einsetzen der 4 Bekannten Punkte
 *
 * Formeln und Lösungen entnommen aus: ISBN 978-3-540-21888-3 Seiten: 230 und 231
 *)
  Function SetMatrixByPoints(Const x, x_: Array Of TVector2): TMatrixNxM;
  Var
    i: Integer;
  Begin
    result := ZeroNxM(9, 8);
    For i := 0 To 3 Do Begin // Gleichung (15.9)
      // * = x_
      result[0, i] := 1;
      result[1, i] := x[i].x;
      result[2, i] := x[i].y;
      result[3, i] := 0;
      result[4, i] := 0;
      result[5, i] := 0;
      result[6, i] := -x_[i].x * x[i].x;
      result[7, i] := -x_[i].x * x[i].y;
      result[8, i] := x_[i].x;
      // * = y_
      result[0, i + 4] := 0;
      result[1, i + 4] := 0;
      result[2, i + 4] := 0;
      result[3, i + 4] := 1;
      result[4, i + 4] := x[i].x;
      result[5, i + 4] := x[i].y;
      result[6, i + 4] := -x_[i].y * x[i].x;
      result[7, i + 4] := -x_[i].y * x[i].y;
      result[8, i + 4] := x_[i].y;
    End;
  End;

  (*
   * Gleichung 15.7:
   *            a0 + a1*x + a2*y
   *  x'(x,y) = ----------------
   *            1 + c1*x + c2*y
   *
   *            b0 + b1*x + b2*y
   *  y'(x,y) = ----------------
   *            1 + c1*x + c2*y
   *)
Var
  a0, a1, a2, b0, b1, b2, c1, c2: Single;

  Procedure CreateFunctionParamsFromSolvedMatrix(Const M: TMatrixNxM);
  Begin
    a0 := M[8, 0];
    a1 := M[8, 1];
    a2 := M[8, 2];
    b0 := M[8, 3];
    b1 := M[8, 4];
    b2 := M[8, 5];
    c1 := M[8, 6];
    c2 := M[8, 7];
  End;

  Function f(x, y: Single): TVector2; // Implementierung der Gleichung 15.7
  Var
    xt, yt, denominator: Single;
  Begin
    denominator := 1 + c1 * x + c2 * y;
    xt := (a0 + a1 * x + a2 * y) / denominator;
    yt := (b0 + b1 * x + b2 * y) / denominator;
    result := v2(xt, yt);
  End;

Var
  Source_intf, Dest_intf: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  tl, bl, br, tr, p: TVector2;
  NI: TBitmap;
  j, i: Integer;
  m, mi: TMatrixNxM;
  dim, vmi, vma: TVector2;
  h, w: Single;
Begin
  If Stretch Then Begin
    dim := v2(Bitmap.Width, Bitmap.Height);
    vmi := v2(0, 0);
    m := SetMatrixByPoints(
      [v2(0, 0), v2(0, Bitmap.Height), v2(Bitmap.Width, Bitmap.Height), v2(bitmap.Width, 0)]
      ,
      [TopLeft, BottomLeft, BottomRight, TopRight]
      );
  End
  Else Begin
    // Dieses Verfahren vergrößert das Zielbild, so wird es nur um das mindeste Vergrößert ;)
    w := max(abs(TopLeft - TopRight), abs(BottomLeft - BottomRight));
    h := max(abs(TopLeft - BottomLeft), abs(TopRight - BottomRight));
    mi := SetMatrixByPoints(
      [TopLeft, BottomLeft, BottomRight, TopRight]
      ,
      [v2(0, 0), v2(0, h), v2(w, h), v2(w, 0)]
      );
    GaussJordan(mi); // GL-Lösen
    CreateFunctionParamsFromSolvedMatrix(mi);
    tl := f(0, 0);
    tr := f(Bitmap.Width, 0);
    bl := f(0, Bitmap.Height);
    br := f(Bitmap.Width, Bitmap.Height);
    vmi := tl;
    vmi := MinV2(vmi, tr);
    vmi := MinV2(vmi, bl);
    vmi := MinV2(vmi, br);
    vma := tl;
    vma := MaxV2(vma, tr);
    vma := MaxV2(vma, bl);
    vma := MaxV2(vma, br);
    dim := vma - vmi;
    m := SetMatrixByPoints(
      [v2(0, 0), v2(0, h), v2(w, h), v2(w, 0)]
      ,
      [TopLeft, BottomLeft, BottomRight, TopRight]
      );
  End;
  // Wir Berechnen die Matrix direkt Invertiert, der Mathe ist das Egal, aber dafür
  // Können wir das Zielbild dann sauber Abtasten ;)
  // Nun muss nur noch das tl, tr, bl, br Rechteck auf das Ni Bild projiziert werden
  GaussJordan(m); // GL-Lösen
  CreateFunctionParamsFromSolvedMatrix(m);
  ni := TBitmap.Create;
  //  ni.Width := Bitmap.Width;
  //  ni.Height := Bitmap.Height;
  ni.Width := ceil(dim.x);
  ni.Height := ceil(Dim.y);
  Source_intf := TLazIntfImage.Create(0, 0);
  Source_intf.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  Dest_intf := TLazIntfImage.Create(0, 0);
  Dest_intf.LoadFromBitmap(ni.Handle, ni.MaskHandle);
  For j := 0 To Dest_intf.Height - 1 Do Begin
    For i := 0 To Dest_intf.Width - 1 Do Begin
      p := f(i + vmi.x, j + vmi.y);
      SetPixel(Dest_intf, i, j, GetPixel(Source_intf, p.x, p.y, wmode, imode));
    End;
  End;
  Dest_intf.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  Source_intf.free;
  Dest_intf.free;
  ni.free;
End;

Procedure Binarisate(Const Bitmap: TBitmap; Border: Byte);
Var
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  CurColor: TFPColor;
  b: Byte;
  i, j: Integer;
Begin
  //  Bitmap.pixelformat := pf24bit; -- Das Darf nicht drin sein, sonst sind evtl alle Werte 0 !!!
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  For j := 0 To bitmap.height - 1 Do
    For i := 0 To bitmap.width - 1 Do Begin
      b := FPColortoLuminanz(TempIntfImg.Colors[i, j]);
      If b <= Border Then Begin
        b := 0;
      End
      Else Begin
        b := 255;
      End;
      curcolor.red := b Shl 8;
      curcolor.green := curcolor.red;
      curcolor.blue := curcolor.red;
      curcolor.alpha := 0;
      TempIntfImg.Colors[i, j] := curcolor;
    End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
End;

Procedure MulImage(Const Bitmap: TBitmap; Matrix: TMatrix3x3;
  iMode: TInterpolationMode; wMode: TWrapMode);
Var
  iMatrix: TMatrix3x3;

  Function Transform(x, y: Single): TVector2;
  Var
    tmp: TVector3;
  Begin
    tmp := v3(x, y, 1);
    tmp := Matrix * tmp;
    result := v2(tmp.x, tmp.y);
  End;

  Function iTransform(x, y: Single): TVector2;
  Var
    tmp: TVector3;
  Begin
    tmp := v3(x, y, 1);
    tmp := iMatrix * tmp;
    result := v2(tmp.x, tmp.y);
  End;

Var
  mi, ma, v: TVector2;
  i, j: integer;
  Source_intf, Dest_intf: TLazIntfImage;
  DestHandle, DestMaskHandle: HBitmap;
  DestBM: TBitmap;
  osx, osy: Single;
Begin
  Source_intf := TLazIntfImage.Create(0, 0);
  Source_intf.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  // 1. bestimmen der Dimension des Zielbildes durch Transformation der Eckpunkte
  mi := Transform(0, 0);
  ma := mi;
  mi := minv2(mi, Transform(Bitmap.Width, 0));
  ma := maxv2(ma, Transform(Bitmap.Width, 0));
  mi := minv2(mi, Transform(Bitmap.Width, Bitmap.Height));
  ma := maxv2(ma, Transform(Bitmap.Width, Bitmap.Height));
  mi := minv2(mi, Transform(0, Bitmap.Height));
  ma := maxv2(ma, Transform(0, Bitmap.Height));
  // 2. Durchführen der eigentlichen Transformation
  DestBM := TBitmap.Create;
  v := (ma - mi);
  DestBM.Width := round(v.x);
  DestBM.Height := round(v.y);
  Dest_intf := TLazIntfImage.Create(0, 0);
  Dest_intf.LoadFromBitmap(DestBM.Handle, DestBM.MaskHandle);
  iMatrix := InvertMatrix2(Matrix);
  If iMatrix.Equal(Zero3x3()) Then Begin
    // Wenn die Matrix nicht Invertierbar ist, muss über die Quellbild Koordinaten Iteriert werden
    // Das macht leider auch ein Oversampling notwendig
    // osx und osy werden "empirisch ermittelt" 2 * häufiger als Notwendig abgetastet
    osx := (DestBM.Width * 2) / max(1, Bitmap.Width);
    osy := (DestBM.Height * 2) / max(1, Bitmap.Height);
    For i := 0 To round(Bitmap.Width * osx) - 1 Do Begin
      For j := 0 To round(Bitmap.Height * osy) - 1 Do Begin
        (*
         * Unter Windows sind die Pixel Links oben Angeschlagen,
         * Damit das aber "Gut" aus sieht, muss ein Pixel mittig Zentriert sein.
         *)
        v := Transform(i / osx, j / osy);
        v := v - mi - v2(0.5, 0.5); // Die Pixelmitte von Links Oben nach Mitte verschieben
        SetPixel(Dest_intf, v.x, v.y, GetPixel(Source_intf, i / osx, j / osy, wMode, iMode));
      End;
    End;
  End
  Else Begin
    // Ist die Matrix Invertierbar kann über die Zielbild Koordinaten Iteriert werden
    // => kein Oversampling notwendig ;)
    For i := trunc(mi.x) To ceil(ma.x) - 1 Do Begin
      For j := trunc(mi.y) To ceil(ma.y) - 1 Do Begin
        (*
         * Unter Windows sind die Pixel Links oben Angeschlagen,
         * Damit das aber "Gut" aus sieht, muss ein Pixel mittig Zentriert sein.
         *)
        v := iTransform(i, j);
        v := v - v2(0.5, 0.5); // Die Pixelmitte von Links Oben nach Mitte verschieben
        SetPixel(Dest_intf, i - mi.x, j - mi.y, GetPixel(Source_intf, v.x, v.y, wmode, iMode));
      End;
    End;
  End;
  Dest_intf.CreateBitmaps(DestHandle, DestMaskHandle, false);
  Bitmap.Handle := DestHandle;
  Bitmap.MaskHandle := DestMaskHandle;
  Source_intf.free;
  Dest_intf.free;
  DestBM.free;
End;

Function MulImage(Const B1: TBitmap; Const B2: TBitmap; InvertB2: Boolean
  ): TBitmap;
Var
  lb1, lb2, lb3: TLazIntfImage;
  i, j: Integer;
  c1, c2, c3: TFPColor;
Begin
  result := Nil;
  If (b1.Width <> b2.Width) Or
    (b1.Height <> b2.Height) Then Begin
    Raise Exception.Create('Error, images differ in size.');
  End;
  result := TBitmap.Create;
  result.Width := b1.Width;
  result.Height := b1.Height;
  lb1 := TLazIntfImage.Create(0, 0);
  lb1.LoadFromBitmap(B1.Handle, B1.MaskHandle);
  lb2 := TLazIntfImage.Create(0, 0);
  lb2.LoadFromBitmap(B2.Handle, B2.MaskHandle);
  lb3 := TLazIntfImage.Create(0, 0);
  lb3.LoadFromBitmap(result.Handle, result.MaskHandle);
  For i := 0 To B1.Width - 1 Do Begin
    For j := 0 To b1.Height - 1 Do Begin
      c1 := lb1.Colors[i, j];
      c2 := lb2.Colors[i, j];
      If InvertB2 Then Begin
        c3.Red := (c1.Red Shr 8) * (255 - (c2.Red Shr 8));
        c3.Green := (c1.Green Shr 8) * (255 - (c2.Green Shr 8));
        c3.Blue := (c1.Blue Shr 8) * (255 - (c2.Blue Shr 8));
        c3.Alpha := (c1.Alpha Shr 8) * (255 - (c2.Alpha Shr 8));
      End
      Else Begin
        c3.Red := (c1.Red Shr 8) * (c2.Red Shr 8);
        c3.Green := (c1.Green Shr 8) * (c2.Green Shr 8);
        c3.Blue := (c1.Blue Shr 8) * (c2.Blue Shr 8);
        c3.Alpha := (c1.Alpha Shr 8) * (c2.Alpha Shr 8);
      End;
      lb3.Colors[i, j] := c3;
    End;
  End;
  result.LoadFromIntfImage(lb3);
  lb1.free;
  lb2.free;
  lb3.free;
End;

Procedure FoldImage(Const Bitmap: TBitmap; Matrix: TMatrixNxM;
  AbsBeforeSet: Boolean);
Var
  r, g, b, a, mg: Single;
  mw, mh, i, j, x, y: integer;
  Source_intf, Dest_intf: TLazIntfImage;
  DestHandle, DestMaskHandle: HBitmap;
  DestBM: TBitmap;
  c: TFPColor;
Begin
  mw := length(Matrix) Div 2;
  mh := Length(Matrix[0]) Div 2;
  Source_intf := TLazIntfImage.Create(0, 0);
  Source_intf.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  DestBM := TBitmap.Create;
  DestBM.Width := Bitmap.Width;
  DestBM.Height := Bitmap.Height;
  Dest_intf := TLazIntfImage.Create(0, 0);
  Dest_intf.LoadFromBitmap(DestBM.Handle, DestBM.MaskHandle);
  // Das Gewicht der Matrix bestimmen
  mg := 0;
  For x := 0 To Length(Matrix) - 1 Do Begin
    For y := 0 To Length(Matrix[0]) - 1 Do Begin
      mg := mg + Matrix[x, y];
    End;
  End;
  If mg = 0 Then mg := 1; // Kantenextraktionsmatrizen haben mg = 0
  For i := 0 To Bitmap.Width - 1 Do Begin
    For j := 0 To Bitmap.Height - 1 Do Begin
      r := 0;
      g := 0;
      b := 0;
      a := 0;
      For x := 0 To Length(Matrix) - 1 Do Begin
        For y := 0 To Length(Matrix[0]) - 1 Do Begin
          c := GetPixel(Source_intf, i + x - mw, j + y - mh, wmBlack, imNearestNeighbour);
          r := r + (c.Red Shr 8) * Matrix[x, y];
          g := g + (c.Green Shr 8) * Matrix[x, y];
          b := b + (c.Blue Shr 8) * Matrix[x, y];
          a := a + (c.Alpha Shr 8) * Matrix[x, y];
        End;
      End;
      r := r / mg;
      g := g / mg;
      b := b / mg;
      a := a / mg;
      If AbsBeforeSet Then Begin
        r := abs(r);
        g := abs(g);
        b := abs(b);
        a := abs(a);
      End;
      c.Red := Clamp(round(r), 0, 255) Shl 8;
      c.Green := Clamp(round(g), 0, 255) Shl 8;
      c.Blue := Clamp(round(b), 0, 255) Shl 8;
      c.Alpha := Clamp(round(a), 0, 255) Shl 8;
      SetPixel(Dest_intf, i, j, c);
    End;
  End;
  Dest_intf.CreateBitmaps(DestHandle, DestMaskHandle, false);
  Bitmap.Handle := DestHandle;
  Bitmap.MaskHandle := DestMaskHandle;
  Source_intf.free;
  Dest_intf.free;
  DestBM.free;
End;

Procedure AddImage(Const Dest, Source: TBitmap);
Var
  Source_intf, Dest_intf: TLazIntfImage;
  i, j: integer;
  c1, c2, c3: TFPColor;
Begin
  If (dest.Width <> Source.Width) Or (Dest.Height <> Source.Height) Then exit;
  Dest_intf := dest.CreateIntfImage;
  Source_intf := Source.CreateIntfImage;
  For i := 0 To dest.Width - 1 Do Begin
    For j := 0 To dest.Height - 1 Do Begin
      c1 := GetPixel(Dest_intf, i, j, wmBlack, imNone);
      c2 := GetPixel(Source_intf, i, j, wmBlack, imNone);
      c3.Red := clamp((c1.Red Shr 8) + (c2.Red Shr 8), 0, 255) Shl 8;
      c3.Green := clamp((c1.Green Shr 8) + (c2.Green Shr 8), 0, 255) Shl 8;
      c3.Blue := clamp((c1.Blue Shr 8) + (c2.Blue Shr 8), 0, 255) Shl 8;
      c3.Alpha := clamp((c1.Alpha Shr 8) + (c2.Alpha Shr 8), 0, 255) Shl 8;
      SetPixel(Dest_intf, i, j, c3);
    End;
  End;
  Dest.LoadFromIntfImage(Dest_intf);
  Dest_intf.free;
  Source_intf.free;
End;

Procedure PosterRice(Const Bitmap: TBitmap; Steps: Integer);
Var
  intf: TLazIntfImage;
  delta, i, j: integer;
  c: TFPColor;
  r, g, b: Integer;
Begin
  delta := 256 Div Steps;
  intf := Bitmap.CreateIntfImage;
  For i := 0 To Bitmap.Width - 1 Do Begin
    For j := 0 To Bitmap.Height - 1 Do Begin
      c := GetPixel(intf, i, j, wmBlack, imNone);
      r := c.Red Shr 8;
      g := c.Green Shr 8;
      b := c.Blue Shr 8;
      r := r - r Mod delta;
      g := g - g Mod delta;
      b := b - b Mod delta;
      c.Red := r Shl 8;
      c.Green := g Shl 8;
      c.Blue := b Shl 8;
      SetPixel(intf, i, j, c);
    End;
  End;
  Bitmap.LoadFromIntfImage(intf);
  intf.free;
End;

Procedure Aberration(Const Bitmap: Tbitmap; LensCenter: TVector2; k1,
  k2: Single; OverSampling: integer; Zoom: Single; iMode: TInterpolationMode;
  wMode: TWrapMode);
Var
  fact_ctd, fact_dtc: Single;
  LambdaX, LambdaY, squeese: Single;

  Function Coord_To_DimensionLess(v: TVector2): TVector2;
  Begin
    result.x := (v.x - LensCenter.x) / fact_ctd;
    result.y := (v.y - LensCenter.y) / fact_ctd;
  End;

  Function DimensionLess_To_Coord(v: TVector2): TVector2;
  Begin
    result.x := (v.x * fact_dtc) + LensCenter.x;
    result.y := (v.y * fact_dtc) + LensCenter.y;
  End;

  Function Transform(v: TVector2): TVector2;
  Var
    pxS, pyS, spxsS, k2_spxsS, k1_pxs: Single;
  Begin
    pxs := sqr(v.x);
    pys := sqr(v.y);
    spxsS := sqr(pxS + pyS);
    k2_spxsS := k2 * spxsS;
    k1_pxs := k1 * pxs;
    result.x := v.x * (1 + k1_pxs + k1 * (1 + LambdaX) * pyS + k2_spxsS);
    result.y := v.y * (1 + (k1_pxs + k1 * (1 + Lambday) * pyS + k2_spxsS) / squeese);
  End;

Var
  in_W, in_H: Integer;
  i, j: Integer;
  i_, j_: Single;
  in_IntfImg, Out_IntfImg: TLazIntfImage;
  Out_ImgHandle, Out_ImgMaskHandle: HBitmap;
  OldCoord: TVector2;
  NewCoord: TVector2;
  res: TBitmap;
Begin
  (* Das hier könnten auch alles noch Parameter sein / werden *)
  LambdaX := 0;
  LambdaY := 0;
  (* Das hier könnten auch alles noch Parameter sein / werden -- ENDE *)
  // Init
  in_w := Bitmap.width;
  in_H := Bitmap.Height;
  squeese := in_w / in_h;
  inc(oversampling, 1);
  res := TBitmap.create;
  res.width := in_w;
  res.height := in_h;
  Out_IntfImg := TLazIntfImage.Create(0, 0);
  Out_IntfImg.LoadFromBitmap(res.Handle, res.MaskHandle);
  in_IntfImg := TLazIntfImage.Create(0, 0);
  in_IntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  fact_ctd := (sqrt(sqr(in_w / 2) + sqr(in_h / 2)));
  fact_dtc := (sqrt(sqr(in_w * Zoom / 2) + sqr(in_h * Zoom / 2)));
  // Eigentliche Transformation
  For j := 0 To (in_h * OverSampling) - 1 Do
    For i := 0 To (in_w * OverSampling) - 1 Do Begin
      i_ := i / oversampling;
      j_ := j / oversampling;
      // In Dimensionslos Umrechnen
      OldCoord := Coord_To_DimensionLess(V2(i_, j_));
      // Transformieren
      NewCoord := Transform(OldCoord);
      // Zurückrechnen in BildKoordinaten
      NewCoord := DimensionLess_To_Coord(NewCoord);
      (* Wir Interpolieren der Wert an der Koordinate *)
      SetPixel(Out_IntfImg, NewCoord.x, NewCoord.y, GetPixel(in_IntfImg, i_, j_, wmode, imode));
    End;
  // Free + Ausgabe
  Out_IntfImg.CreateBitmaps(Out_ImgHandle, Out_ImgMaskHandle, false);
  Bitmap.Handle := Out_ImgHandle;
  Bitmap.MaskHandle := Out_ImgMaskHandle;
  in_IntfImg.free;
  Out_IntfImg.free;
  res.free;
End;

(*
 *
 *)

Procedure Vignetting(Const Bitmap: TBitmap; Strength, MinLevel, MaxLevel: Single
  );
Var
  delta: Single;
  (*
   * i,j sind normiert auf [0.. 1[
   *)
  Function Transform(C: TFPColor; x, y: Single): TFPColor;
  Var
    xx, f: Single;
  Begin
    xx := ((sqr(x - 0.5) + sqr(y - 0.5)) * 2 * pi);
    f := Power(0.5 - cos(xx) / 2, Strength) * delta + MinLevel;
    // Der Code unten Addiert, das ist zum Ausgleichen der Vignetierung ganz gut
    // Es gibt auch eine Variante in der Multipliziert wird. Dann muss
    // Die Umrechnung  -1 * 255 entfallen
    f := (f - 1) * 255;
    result.Red := clamp(round((c.Red Shr 8) + f), 0, 255) Shl 8;
    result.Green := clamp(round((c.Green Shr 8) + f), 0, 255) Shl 8;
    result.Blue := clamp(round((c.Blue Shr 8) + f), 0, 255) Shl 8;
    result.Alpha := clamp(round((c.Alpha Shr 8) + f), 0, 255) Shl 8;
  End;
Var
  res: TBitmap;
  Out_IntfImg, in_IntfImg: TLazIntfImage;
  Out_ImgHandle, Out_ImgMaskHandle: HBitmap;
  i, j: Integer;
  c: TFPColor;
Begin
  delta := MaxLevel - MinLevel;
  res := TBitmap.create;
  res.width := Bitmap.Width;
  res.height := Bitmap.Height;
  Out_IntfImg := TLazIntfImage.Create(0, 0);
  Out_IntfImg.LoadFromBitmap(res.Handle, res.MaskHandle);
  in_IntfImg := TLazIntfImage.Create(0, 0);
  in_IntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  For i := 0 To Bitmap.Width - 1 Do Begin
    For j := 0 To Bitmap.Height - 1 Do Begin
      c := GetPixel(in_IntfImg, i, j, wmBlack, imNone); // Interpolation ist egal weil wir ne 1:1 Abtastung machen
      c := Transform(c, i / Bitmap.Width, j / Bitmap.Height);
      SetPixel(Out_IntfImg, i, j, c);
    End;
  End;
  Out_IntfImg.CreateBitmaps(Out_ImgHandle, Out_ImgMaskHandle, false);
  Bitmap.Handle := Out_ImgHandle;
  Bitmap.MaskHandle := Out_ImgMaskHandle;
  in_IntfImg.free;
  Out_IntfImg.free;
  res.free;
End;

Procedure Brightning(Const Bitmap: TBitmap; Value: integer);
Var
  intf: TLazIntfImage;
  c: TFPColor;
  i, j: integer;
Begin
  If value = 0 Then exit;
  intf := Bitmap.CreateIntfImage;
  For i := 0 To Bitmap.Width - 1 Do Begin
    For j := 0 To Bitmap.Height - 1 Do Begin
      c := intf.Colors[i, j];
      c.Red := clamp((c.Red Shr 8) + Value, 0, 255) Shl 8;
      c.Green := clamp((c.Green Shr 8) + Value, 0, 255) Shl 8;
      c.Blue := clamp((c.Blue Shr 8) + Value, 0, 255) Shl 8;
      intf.Colors[i, j] := c;
    End;
  End;
  bitmap.LoadFromIntfImage(intf);
  intf.free;
End;

Procedure Contrast(Const Bitmap: TBitmap; contrast: integer);
// Quelle: https://www.dfstudios.co.uk/articles/programming/image-programming-algorithms/image-processing-algorithms-part-5-contrast-adjustment/
Var
  intf: TLazIntfImage;
  c: TFPColor;
  i, j: integer;
  f: Single;
Begin
  If contrast = 0 Then exit;
  f := (259 * (contrast + 255)) / (255 * (259 - contrast));
  intf := Bitmap.CreateIntfImage;
  For i := 0 To Bitmap.Width - 1 Do Begin
    For j := 0 To Bitmap.Height - 1 Do Begin
      c := intf.Colors[i, j];
      c.Red := clamp(trunc(f * ((c.Red Shr 8) - 128) + 128), 0, 255) Shl 8;
      c.Green := clamp(trunc(f * ((c.Green Shr 8) - 128) + 128), 0, 255) Shl 8;
      c.Blue := clamp(trunc(f * ((c.Blue Shr 8) - 128) + 128), 0, 255) Shl 8;
      intf.Colors[i, j] := c;
    End;
  End;
  bitmap.LoadFromIntfImage(intf);
  intf.free;
End;

Procedure SwapColor(Const Bitmap: TBitmap; SourceColor, DestColor: TColor);
Var
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  CurColor: TFPColor;
  i, j: Integer;
  c: TColor;
Begin
  //  Bitmap.pixelformat := pf24bit; -- Das Darf nicht drin sein, sonst sind evtl alle Werte 0 !!!
  SourceColor := SourceColor And $00FFFFFF;
  CurColor := ColorToFPColor(DestColor);
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  For j := 0 To bitmap.height - 1 Do
    For i := 0 To bitmap.width - 1 Do Begin
      c := FPColorToColor(TempIntfImg.Colors[i, j]);
      c := c And $00FFFFFF;
      If SourceColor = c Then Begin
        TempIntfImg.Colors[i, j] := curcolor;
      End;
    End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
End;

Procedure FloodFill(Const Bitmap: TBitmap; StartX, StartY: Integer;
  DestColor: TColor; AllowDiagonalWalk: Boolean);

Type
  TPointFifo = specialize TBufferedFifo < TPoint > ;

Var
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  SourceCol: TFPColor;
  DestCol: TFPColor;
  fifo: TPointFifo;
  p: Tpoint;
  w, h: integer;
Begin
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  SourceCol := TempIntfImg.Colors[StartX, StartY];
  DestCol := ColorToFPColor(DestColor);
  If SourceCol = DestCol Then Begin // Das würde Endlos Rekursionen geben !
    TempIntfImg.free;
    exit;
  End;
  fifo := TPointFifo.create();
  fifo.push(point(StartX, StartY));
  w := Bitmap.Width;
  h := Bitmap.Height;
  While Not fifo.isempty Do Begin
    p := fifo.Pop;
    // Nur So lange wir überhaupt im Bild sind
    If (p.x >= 0) And (p.Y >= 0) And
      (p.x < w) And (p.Y < h) Then Begin
      // Es gibt noch was zu tun ;)
      If (TempIntfImg.Colors[p.x, p.y] = SourceCol) Then Begin
        TempIntfImg.Colors[p.x, p.y] := DestCol;
        fifo.Push(point(p.x + 1, p.y));
        fifo.Push(point(p.x - 1, p.y));
        fifo.Push(point(p.x, p.y + 1));
        fifo.Push(point(p.x, p.y - 1));
        If AllowDiagonalWalk Then Begin
          fifo.Push(point(p.x + 1, p.y + 1));
          fifo.Push(point(p.x + 1, p.y - 1));
          fifo.Push(point(p.x - 1, p.y + 1));
          fifo.Push(point(p.x - 1, p.y - 1));
        End;
      End;
    End;
  End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
  fifo.free;
End;

Procedure Stretchdraw(Const Dest: TBitmap; Destrect: Trect;
  Const Source: Tbitmap; Mode: TInterpolationMode);
Var
  Source_intf, Dest_intf: TLazIntfImage;
  DestBM: Tbitmap;
  DestHandle, DestMaskHandle: HBitmap;
  w, h: Integer;
  i, j: Integer;
  u, v: Single;
Begin
  // Berechnen der Zieldimensionen
  w := abs(Destrect.Right - Destrect.Left);
  h := abs(Destrect.Bottom - Destrect.Top);
  // Wenn nichts sichtbar ist, können wir uns die Arbeit auch sparen.
  If (w = 0) Or (h = 0) Then exit;
  // Erstellen der Arbeitsvariablen
  Destbm := TBitmap.Create;
  destbm.Width := w;
  destbm.Height := h;
  //destbm.Canvas.Brush.color := clwhite;
  //destbm.Canvas.rectangle(0, 0, 100, 100);
  Dest_intf := TLazIntfImage.Create(0, 0);
  Dest_intf.LoadFromBitmap(DestBM.Handle, DestBM.MaskHandle);
  Source_intf := TLazIntfImage.Create(0, 0);
  Source_intf.LoadFromBitmap(source.Handle, Source.MaskHandle);
  // So nun wird von Source_intf gelesen und in Dest_intf geschrieben
  // Das Ganze geschieht in der Granularität von Dest_intf und mit dem gewählten Interpolationsmode
  For i := 0 To w - 1 Do Begin
    u := i / (w - 1); // U - Wert bleibt für alle j Gleich
    For j := 0 To h - 1 Do Begin
      v := j / (h - 1); // V - Wert
      Dest_intf.Colors[i, j] := GetPixel(Source_intf, u * Source.width, v * Source.Height, wmClamp, Mode);
    End;
  End;
  // Zurückschreiben, und ohne Stretch in Dest Zeichnen
  Dest_intf.CreateBitmaps(DestHandle, DestMaskHandle, false);
  DestBM.Handle := DestHandle;
  DestBM.MaskHandle := DestMaskHandle;
  //  Das Transpaente managed ja der Aufrufer, so würde es nur gehen, wenn Transparent Color CLBlack
  If Source.Transparent Then Begin
    DestBM.TransparentColor := Source.TransparentColor;
    DestBM.Transparent := true;
  End;
  dest.Canvas.Draw(min(Destrect.Right, Destrect.Left), min(Destrect.Bottom, Destrect.Top), DestBM);
  // Alles wieder Freigeben
  Source_intf.free;
  Dest_intf.free;
  destbm.free;
End;

Procedure Stretchdraw(Const Dest: TCanvas; Destrect: Trect;
  Const Source: Tbitmap; Mode: TInterpolationMode);
Var
  Source_intf, Dest_intf: TLazIntfImage;
  DestBM: Tbitmap;
  DestHandle, DestMaskHandle: HBitmap;
  w, h: Integer;
  i, j: Integer;
  u, v: Single;
Begin
  // Berechnen der Zieldimensionen
  w := abs(Destrect.Right - Destrect.Left);
  h := abs(Destrect.Bottom - Destrect.Top);
  // Wenn nichts sichtbar ist, können wir uns die Arbeit auch sparen.
  If (w = 0) Or (h = 0) Then exit;
  // Erstellen der Arbeitsvariablen
  Destbm := TBitmap.Create;
  destbm.Width := w;
  destbm.Height := h;
  //destbm.Canvas.Brush.color := clwhite;
  //destbm.Canvas.rectangle(0, 0, 100, 100);
  Dest_intf := TLazIntfImage.Create(0, 0);
  Dest_intf.LoadFromBitmap(DestBM.Handle, DestBM.MaskHandle);
  Source_intf := TLazIntfImage.Create(0, 0);
  Source_intf.LoadFromBitmap(source.Handle, Source.MaskHandle);
  // So nun wird von Source_intf gelesen und in Dest_intf geschrieben
  // Das Ganze geschieht in der Granularität von Dest_intf und mit dem gewählten Interpolationsmode
  For i := 0 To w - 1 Do Begin
    u := i / (w - 1); // U - Wert bleibt für alle j Gleich
    For j := 0 To h - 1 Do Begin
      v := j / (h - 1); // V - Wert
      Dest_intf.Colors[i, j] := GetPixel(Source_intf, u * Source.width, v * Source.Height, wmClamp, Mode);
    End;
  End;
  // Zurückschreiben, und ohne Stretch in Dest Zeichnen
  Dest_intf.CreateBitmaps(DestHandle, DestMaskHandle, false);
  DestBM.Handle := DestHandle;
  DestBM.MaskHandle := DestMaskHandle;
  //  Das Transpaente managed ja der Aufrufer, so würde es nur gehen, wenn Transparent Color CLBlack
  If Source.Transparent Then Begin
    DestBM.TransparentColor := Source.TransparentColor;
    DestBM.Transparent := true;
  End;
  dest.Draw(min(Destrect.Right, Destrect.Left), min(Destrect.Bottom, Destrect.Top), DestBM);
  // Alles wieder Freigeben
  Source_intf.free;
  Dest_intf.free;
  destbm.free;
End;

Procedure SmoothRound(Const Image: TBitmap; FilterRadius: integer);
Var
  r, g, b: int64;
  cnt, i, j, x, y: Integer;
  c: TFPColor;
  Source_intf, Dest_intf: TLazIntfImage;
  DestBM: Tbitmap;
  DestHandle, DestMaskHandle: HBitmap;
Begin
  // Todo : Das geht zwar, könnte aber durchaus noch beschleunigt werden, da die Faltung ja überall mit 1 Skalliert ist.
  // Erstellen der Arbeitsvariablen
  DestBM := TBitmap.Create;
  DestBM.Width := Image.Width;
  DestBM.Height := Image.Height;
  //destbm.Canvas.Brush.color := clwhite;
  //destbm.Canvas.rectangle(0, 0, 100, 100);
  Dest_intf := TLazIntfImage.Create(0, 0);
  Dest_intf.LoadFromBitmap(DestBM.Handle, DestBM.MaskHandle);
  Source_intf := TLazIntfImage.Create(0, 0);
  Source_intf.LoadFromBitmap(Image.Handle, Image.MaskHandle);
  For i := 0 To Image.Width - 1 Do Begin
    For j := 0 To Image.Height - 1 Do Begin
      r := 0;
      g := 0;
      b := 0;
      cnt := 0;
      For x := -FilterRadius To FilterRadius Do Begin
        For y := -FilterRadius To FilterRadius Do Begin
          If sqr(x) + sqr(y) <= sqr(FilterRadius) Then Begin
            c := Source_intf.Colors[
              min(Image.Width - 1, max(0, i + x)),
              min(Image.Height - 1, max(0, j + y))
              ];
            r := r + (c.red Shr 8);
            g := g + (c.green Shr 8);
            b := b + (c.blue Shr 8);
            inc(cnt);
          End;
        End;
      End;
      r := r Div cnt;
      g := g Div cnt;
      b := b Div cnt;
      c.red := r Shl 8;
      c.green := g Shl 8;
      c.blue := b Shl 8;
      Dest_intf.Colors[i, j] := c;
    End;
  End;
  // Zurückschreiben, und ohne Stretch in Dest Zeichnen
  Dest_intf.CreateBitmaps(DestHandle, DestMaskHandle, false);
  DestBM.Handle := DestHandle;
  DestBM.MaskHandle := DestMaskHandle;
  image.canvas.Draw(0, 0, DestBM);
  // Alles wieder Freigeben
  Source_intf.free;
  Dest_intf.free;
  destbm.free;
End;

(*
 * Es ist schneller 2 mal Linear als 1 mal 2Dimensional zu filtern.
 * Zusätzlich kann bei 1-Dimensionaler Filterung mit Integralsummen gearbeitet
 * werden, dies erhöht die Geschwindigkeit ebenfalls deutlich
 *)

Procedure Smooth(Const Image: TBitmap; FilterRadius: integer);
Var
  w, r, g, b, i, j: Integer;
  c: TFPColor;
  Source_intf, Dest_intf: TLazIntfImage;
  DestBM: Tbitmap;
  DestHandle, DestMaskHandle: HBitmap;
  line: Array[0..2] Of Array Of Integer; // Die gespeicherten Einzelwerte je Integral als Ringspeichern
  Lines: Array[0..2] Of Integer; // Die Integralsummen je Farbkanal
  Lineptr: integer; // 1. Element im Ringspeicher der Integrale
Begin
  // Erstellen der Arbeitsvariablen
  DestBM := TBitmap.Create;
  DestBM.Width := Image.Width;
  DestBM.Height := Image.Height;
  Dest_intf := TLazIntfImage.Create(0, 0);
  Dest_intf.LoadFromBitmap(DestBM.Handle, DestBM.MaskHandle);
  Source_intf := TLazIntfImage.Create(0, 0);
  Source_intf.LoadFromBitmap(Image.Handle, Image.MaskHandle);
  w := 2 * FilterRadius + 1;
  setlength(line[0], w);
  setlength(line[1], w);
  setlength(line[2], w);
  // Waagrecht Filtern
  For j := 0 To image.Height - 1 Do Begin
    Lines[0] := 0; // Integral = 0
    Lines[1] := 0; // Integral = 0
    Lines[2] := 0; // Integral = 0
    For i := 0 To w - 1 Do Begin // Bilden Integrale in Pixel 0
      If i <= FilterRadius Then Begin // "Hinter" dem Linken Rand
        c := Source_intf.Colors[0, j];
      End
      Else Begin // Im Bild, aber mit Überlaufschutz (falls Filter > Bildbreite)
        c := Source_intf.Colors[min(i - FilterRadius, image.Width - 1), j];
      End;
      r := c.red Shr 8;
      g := c.green Shr 8;
      b := c.blue Shr 8;
      lines[0] := lines[0] + r;
      lines[1] := lines[1] + g;
      lines[2] := lines[2] + b;
      line[0, i] := r;
      line[1, i] := g;
      line[2, i] := b;
    End;
    Lineptr := 0;
    c.red := (lines[0] Div w) Shl 8;
    c.green := (lines[1] Div w) Shl 8;
    c.blue := (lines[2] Div w) Shl 8;
    Dest_intf.Colors[0, j] := c;
    // Berechnen der Nachfolgenden Pixel
    For i := 1 To Image.Width - 1 Do Begin
      c := Source_intf.Colors[min(i + FilterRadius, image.Width - 1), j];
      r := c.red Shr 8;
      g := c.green Shr 8;
      b := c.blue Shr 8;
      lines[0] := lines[0] + r - line[0, Lineptr];
      lines[1] := lines[1] + g - line[1, Lineptr];
      lines[2] := lines[2] + b - line[2, Lineptr];
      line[0, Lineptr] := r;
      line[1, Lineptr] := g;
      line[2, Lineptr] := b;
      Lineptr := (Lineptr + 1) Mod w;
      c.red := (lines[0] Div w) Shl 8;
      c.green := (lines[1] Div w) Shl 8;
      c.blue := (lines[2] Div w) Shl 8;
      Dest_intf.Colors[i, j] := c;
    End;
  End;
  // Senkrecht Filtern
  For i := 0 To image.Width - 1 Do Begin
    Lines[0] := 0; // Integral = 0
    Lines[1] := 0; // Integral = 0
    Lines[2] := 0; // Integral = 0
    For j := 0 To w - 1 Do Begin // Bilden Integrale in Pixel 0
      If j <= FilterRadius Then Begin // "Hinter" dem Oberen Rand
        c := Dest_intf.Colors[i, 0];
      End
      Else Begin // Im Bild, aber mit Überlaufschutz (falls Filter > Bildhöhe)
        c := Dest_intf.Colors[i, min(j - FilterRadius, image.Height - 1)];
      End;
      r := c.red Shr 8;
      g := c.green Shr 8;
      b := c.blue Shr 8;
      lines[0] := lines[0] + r;
      lines[1] := lines[1] + g;
      lines[2] := lines[2] + b;
      line[0, j] := r;
      line[1, j] := g;
      line[2, j] := b;
    End;
    Lineptr := 0;
    c.red := (lines[0] Div w) Shl 8;
    c.green := (lines[1] Div w) Shl 8;
    c.blue := (lines[2] Div w) Shl 8;
    Source_intf.Colors[i, 0] := c;
    // Berechnen der Nachfolgenden Pixel
    For j := 1 To Image.Height - 1 Do Begin
      c := Dest_intf.Colors[i, min(j + FilterRadius, image.Height - 1)];
      r := c.red Shr 8;
      g := c.green Shr 8;
      b := c.blue Shr 8;
      lines[0] := lines[0] + r - line[0, Lineptr];
      lines[1] := lines[1] + g - line[1, Lineptr];
      lines[2] := lines[2] + b - line[2, Lineptr];
      line[0, Lineptr] := r;
      line[1, Lineptr] := g;
      line[2, Lineptr] := b;
      Lineptr := (Lineptr + 1) Mod w;
      c.red := (lines[0] Div w) Shl 8;
      c.green := (lines[1] Div w) Shl 8;
      c.blue := (lines[2] Div w) Shl 8;
      Source_intf.Colors[i, j] := c;
    End;
  End;
  // Zurückschreiben, und ohne Stretch in Dest Zeichnen
  Source_intf.CreateBitmaps(DestHandle, DestMaskHandle, false);
  image.Handle := DestHandle;
  image.MaskHandle := DestMaskHandle;
  // Alles wieder Freigeben
  Source_intf.free;
  Dest_intf.free;
  destbm.free;
End;

Procedure RenderArrow(Const Canvas: TCanvas; StartPoint: TPoint;
  EndPoint: TPoint);
Const
  Pi_viertel = Pi / 4; // 45° im Bogenmass
  Percent = 0.10; // Die Länge der Seitenflügel wird nacher 10 % der Pfeillänge
  MinFluegelLen = 5; // Die Flügel sollen nie Kleiner als MinFluegelLen Pixel sein !
Var
  dx, dy: integer;
  a, len: Single;
Begin
  canvas.MoveTo(StartPoint);
  canvas.LineTo(EndPoint);
  dx := EndPoint.X - StartPoint.X;
  dy := EndPoint.y - StartPoint.y;
  len := sqrt(sqr(dx) + sqr(dy));
  If len > 1 Then Begin
    len := max(MinFluegelLen, len * Percent);
    a := arctan2(-dy, -dx); // Winkel in welcher der Pfeil Zeigt + Pi
    // canvas.MoveTo(EndPoint); // Wir sind schon an EndPoint
    canvas.lineTo(EndPoint.X + round(cos(a + Pi_viertel) * len), EndPoint.y + round(sin(a + Pi_viertel) * len));
    canvas.MoveTo(EndPoint);
    canvas.lineTo(EndPoint.X + round(cos(a - Pi_viertel) * len), EndPoint.y + round(sin(a - Pi_viertel) * len));
  End;
End;

End.

