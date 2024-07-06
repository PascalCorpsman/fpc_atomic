(******************************************************************************)
(* uvectormat.pas                                                  12.11.2014 *)
(*                                                                            *)
(* Version     : 0.18                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This unit holds a lot of vector algorithms, often needed in  *)
(*               programms.                                                   *)
(*               The vector and matrix implementation is byte compatible      *)
(*               with the OpenGL standard if used single as TBasetype.        *)
(*               In Memory the data is stored Column-Major                    *)
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
(* Known Issues: The operand * is cross-product not component-wise product    *)
(*                 = Hadamerd Produkt                                         *)
(*                                                                            *)
(* History     : 0.01 Basisimplementierung                                    *)
(*               0.02 PointInTriangle                                         *)
(*                    CalculateTriangleArea                                   *)
(*                    PointsToCircumCircle                                    *)
(*                    PointsToDelaunayTriangleList                            *)
(*                    Convert_Dimension                                       *)
(*               0.03 Hinzufügen Operator = / für TVector3                    *)
(*                    minV3, maxV3, Zerov2, ZeroV3, v3 := v4                  *)
(*               0.04 Hinzufügen lenv2sqr                                     *)
(*               0.05 Hinzufügen Intersect_Line_Ellipse                       *)
(*               0.06 Hinzufügen IntersectLine2                               *)
(*               0.07 Grundimplementierung zum Umgang mit MatrixNxM und       *)
(*                    VectorN                                                 *)
(*               0.08 Steuerbar via .inc Datei                                *)
(*               0.09 PointInPolygon                                          *)
(*               0.10 Rotationsmatrix für 2D                                  *)
(*               0.11 Determinanten Berechnungen                              *)
(*                    Methode der Kleinsten Fehlerquadrate                    *)
(*                    Gauss Jourdan Verfahren für Matrix NxM                  *)
(*               0.12 Hadamard Produkt                                        *)
(*               0.13 Operand Overloading für V4 Vektoren                     *)
(*               0.14 Umstellen Matrizenmultiklikation auf kleinere Schleifen *)
(*               0.15 Ein bischen Aufräumen, Mapfunktion, diverse             *)
(*                    Konvertier Funktionen                                   *)
(*                    Entfernen aller Operanten Overloadings für              *)
(*                     dynamische Strukturen, da nicht mehr eindeutig         *)
(*                     Dafür einführen Type helper                            *)
(*               0.16 CalculateOrthoganlProjection                            *)
(*               0.17 Map function                                            *)
(*                    TMatrix4x4.Raw, TMatrix4x4.getInverse,                  *)
(*                     TMatrix3x3.getInverse, TMatrix2x2.getInverse           *)
(*                    InvertMatrix2 for TMatrix4x4, TMatrix2x2                *)
(*               0.18 CalculatePlumbFootPoint                                 *)
(*                                                                            *)
(******************************************************************************)
Unit uvectormath;

{$MODE objfpc}{$H+}
{$MODESWITCH advancedrecords}
{$MODESWITCH TypeHelpers}

Interface

Uses
  Math // Sin Cos degtorad radtodeg
  , sysutils // exception
  , classes // TPoint
  ;

Const
  Epsilon = 0.00390625; // = 1 / 256, Unterscheiden sich 2 Float Werte um weniger als Epsilon, dann werden sie als Gleich angesehen

Type

  (*
   * If you get a compiler error with missing file
   * just create a file namend "uvectormath.inc" in your project folder and
   * insert the following content:
   *
   * ---------- Content of file ----------
     // Mit diesem Schalter kann das Überladen der Standard Operatoren Aktiviert
     // werden ( Achtung das kann nicht jeder FPC Compiler )

     {$DEFINE UseOperandOverloading}

     TBaseType = Single; // Alle Komponenten bestehen aus BaseType, zur Nutzung von OpenGL ist Single zwingend !!

     ---------- End content of file ----------
   *)

{$I uvectormath.inc}

  TVector2 = Record
    Case boolean Of
      false: (x, y: TBaseType);
      true: (data: Array[0..1] Of TBaseType);
  End;

  TVector3 = Record
    Case boolean Of
      false: (x, y, z: TBaseType);
      true: (data: Array[0..2] Of TBaseType);
  End;

  TVector4 = Record
    Case boolean Of
      false: (x, y, z, w: TBaseType);
      true: (data: Array[0..3] Of TBaseType);
  End;

  // (0 .. length)
  TVectorN = Array Of TBaseType; // Allgemein

  TVector2Array = Array Of TVector2;

  TVector3Array = Array Of TVector3;

  TVector4Array = Array Of TVector4;

  TVectorNArray = Array Of TVectorN;

  //                 Raw (in memory)
  //(00 10)          ( 0 2)
  //(01 11)        = ( 1 3)
  TMatrix2x2 = Array[0..1, 0..1] Of TBaseType;

  //                 Raw (in memory)
  //(00 10 20)       ( 0 3 6)
  //(01 11 21)     = ( 1 4 7)
  //(02 12 22)       ( 2 5 8)
  TMatrix3x3 = Array[0..2, 0..2] Of TBaseType;

  //                 Raw (in memory)
  //(00 10 20 30)    ( 0  4  8 12)
  //(01 11 21 31)    ( 1  5  9 13)
  //(02 12 22 32)  = ( 2  6 10 14)
  //(03 13 23 33)    ( 3  7 11 15)
  TMatrix4x4 = Array[0..3, 0..3] Of TBaseType;

  //                             Raw (in memory)
  //(0 0      .. Col-1 0    )   ( 0  Row     .. Col*(Row-1))
  //( .       .             )   ( 1  Row+1   .             )
  //( .        .            ) = ( 2   .       .            )
  //(0 Row -1 .. Col-1 Row-1)   ( 3  2*Row-1 .. Col*Row -1 )
  TMatrixNxM = Array Of Array Of TBaseType; // Allgemein

  (*
   * Es folgen Sonder Datentypen
   *)
  TTriangle = Record // Ein Dreieck, welches in einer PunkteListe Enthalten ist
    a, b, c: integer; // Speichert nur Indexe in Array's, daher reichen Integers
  End;

  TTriangleArray = Array Of TTriangle; // Eine Liste von Dreiecken in einer Punkteliste

  TCircle = Record // Ein Kreis
    Center: TVector2; // Kreismittelpunkt
    radius: TBaseType; // Sein Radius
  End;

  TMapFunction = Function(x: Single): Single; // Funktionsprototyp für MapFunction

  { TVector2helper }

  TVector2helper = Record Helper For TVector2
    Function Equal(Other: TVector2): Boolean;
    Function Cross(Other: TVector2): TVector2;
    Function Hadamard(Other: TVector2): TVector2;
    Function Dot(Other: TVector2): TBaseType;
  End;

  { TVector3helper }

  TVector3helper = Record Helper For TVector3
    Function Equal(Other: TVector3): Boolean;
    Function Cross(Other: TVector3): TVector3;
    Function Hadamard(Other: TVector3): TVector3;
    Function Dot(Other: TVector3): TBaseType;
  End;

  { TVector4helper }

  TVector4helper = Record Helper For TVector4
    Function Equal(Other: TVector4): Boolean;
    Function Hadamard(Other: TVector4): TVector4;
    Function Dot(Other: TVector4): TBaseType;
  End;

  { TVectorNhelper }

  TVectorNhelper = Type Helper For TVectorN
    Function Equal(Other: TVectorN): Boolean;
    Function Hadamard(Other: TVectorN): TVectorN;
    Function Transpose(): TMatrixNxM;
  End;

  { TMatrix2x2helper }

  TMatrix2x2helper = Type Helper For TMatrix2x2
    Function Equal(Other: TMatrix2x2): Boolean;
    Function getInverse(): TMatrix2x2; // Zero if there is no invertable Matrix
  End;

  { TMatrix3x3helper }

  TMatrix3x3helper = Type Helper For TMatrix3x3
    Function Equal(Other: TMatrix3x3): Boolean;
    Function getInverse(): TMatrix3x3; // Zero if there is no invertable Matrix
  End;

  { TMatrix4x4helper }

  TMatrix4x4helper = Type Helper For TMatrix4x4
  private
    Function getRaw(index: integer): TBaseType;
    Procedure setRaw(index: integer; AValue: TBaseType);
  public
    Function Equal(Other: TMatrix4x4): Boolean;
    Function getInverse(): TMatrix4x4; // Zero if there is no invertable Matrix
    Property Raw[index: integer]: TBaseType read getRaw write setRaw; // Give access as if Matrix where a 1-Dim array
  End;

  { TMatrixNxMhelper }

  TMatrixNxMhelper = Type Helper For TMatrixNxM
    Function Equal(Other: TMatrixNxM): Boolean;
    Function Hadamard(Other: TMatrixNxM): TMatrixNxM;
    Function Transpose(): TMatrixNxM;
    Function MapMatrix(MapFunction: TMapFunction): TMatrixNxM;
  End;

{$IFDEF UseOperandOverloading}

  (*
    -- Aus Sicherheitsgründen werden alle = Operatoren deaktiviert, da der
       unbedarfte Nutzer sonst ausversehen die beiden erwischen könnte, welche
       nicht gehen und es evtl nicht merkt !
  Operator = (a, b: TVector2): Boolean;
  Operator = (a, b: TVector3): Boolean;
  Operator = (a, b: TVector4): Boolean;
  Operator = (a, b: TVectorN): Boolean; -- Geht nicht, da der Compiler hier einen = auf die Pointer macht
  Operator = (m1, m2: TMatrix2x2): Boolean;
  Operator = (m1, m2: TMatrix3x3): Boolean;
  Operator = (m1, m2: TMatrix4x4): Boolean;
  Operator = (m1, m2: TMatrixNxM): Boolean; -- Geht nicht, da der Compiler hier einen = auf die Pointer macht
  *)

Operator := (p: TPoint): TVector2;
Operator := (p: TVector2): TPoint;
Operator := (v: TVector4): TVector3;
Operator := (M: TMatrix3x3): TMatrixNxM;

Operator - (v: TVector3): TVector3;
Operator - (v: TVector4): TVector4;

Operator + (a, b: TVector2): TVector2;
Operator + (a, b: TVector3): TVector3;
Operator + (a, b: TVector4): TVector4;
Operator + (M1, M2: TMatrixNxM): TMatrixNxM;

Operator - (a, b: TVector2): TVector2;
Operator - (a, b: TVector3): TVector3;
Operator - (a, b: TVector4): TVector4;
Operator - (a, b: TVectorN): TVectorN;
Operator - (M1, M2: TMatrixNxM): TMatrixNxM;

Operator * (s: TBaseType; v: TVector2): TVector2;
Operator * (v: TVector2; s: TBaseType): TVector2;
Operator * (s: TBaseType; v: TVector3): TVector3;
Operator * (v: TVector3; s: TBaseType): TVector3;
Operator * (s: TBaseType; v: TVector4): TVector4;
Operator * (v: TVector4; s: TBaseType): TVector4;
Operator * (s: TBasetype; m: TMatrixNxM): TMatrixNxM;

Operator * (a, b: TVector2): TBaseType;
Operator * (a, b: TVector3): TBaseType;
Operator * (a, b: TVector4): TBaseType;
Operator * (a, b: TVectorN): TBaseType;

(*
  Geht nicht da der Compiler nur die parameter Auswertet und dann die erste Übereinstimmung die er findet
Operator * (a, b: TVector2): TVector2;
Operator * (a, b: TVector3): TVector3;
Operator * (a, b: TVector4): TVector4;
Operator * (a, b: TVectorN): TVectorN;
*)

Operator * (m: TMatrix2x2; v: TVector2): Tvector2;
Operator * (m: TMatrix3x3; v: TVector3): Tvector3;
Operator * (m: TMatrix4x4; v: TVector4): Tvector4;
Operator * (m: TMatrixNxM; v: TVectorN): TVectorN;

Operator * (M1, M2: TMatrix2x2): TMatrix2x2;
Operator * (M1, M2: TMatrix3x3): TMatrix3x3;
Operator * (M1, M2: TMatrixNxM): TMatrixNxM;

Operator / (s: TBaseType; v: TVector2): TVector2;
Operator / (v: TVector2; s: TBaseType): TVector2;
Operator / (s: TBaseType; v: TVector3): TVector3;
Operator / (v: TVector3; s: TBaseType): TVector3;
Operator / (s: TBaseType; v: TVector4): TVector4;
Operator / (v: TVector4; s: TBaseType): TVector4;

{$ENDIF}

// Konstruktoren
Function ZeroV2(): TVector2;
Function ZeroV3(): TVector3;
Function ZeroV4(): TVector4;

Function Zero2x2(): TMatrix2x2;
Function Zero3x3(): TMatrix3x3;
Function Zero4x4(): TMatrix4x4;
Function ZeroNxM(Cols, Rows: integer): TMatrixNxM;

Function IdentityMatrix2x2: TMatrix2x2;
Function IdentityMatrix3x3: TMatrix3x3;
Function IdentityMatrix4x4: TMatrix4x4;

Procedure RandomizeNxM(Var M: TMatrixNxM);

Function V2(Const X, Y: TBaseType): TVector2;
Function V3(Const X, Y, Z: TBaseType): TVector3;
Function V4(Const X, Y, Z, W: TBaseType): TVector4; Overload;
Function V4(Const V: Tvector3; Const W: TBaseType): TVector4; Overload;
Function VN(Const Values: Array Of TBaseType): TVectorN;
Function M2x2(x11, x12, x21, x22: TBaseType): TMatrix2x2;
Function M3x3(x11, x12, x13, x21, x22, x23, x31, x32, x33: TBaseType): TMatrix3x3;
Function M4x4(x11, x12, x13, x14, x21, x22, x23, x24, x31, x32, x33, x34, x41, x42, x43, x44: TBaseType): TMatrix4x4;
Function MNxM(Cols: integer; Const Elements: Array Of TBaseType): TMatrixNxM; overload; // Elements ist eine Matrix Zeilenweise eingelesen
Function MNxM(Cols: integer; V: TvectorN): TMatrixNxM; overload; // v ist eine Matrix Zeilenweise eingelesen

Procedure SaveMNxMToStream(Const Stream: TStream; Const Data: TMatrixNxM);
Function LoadMNxMFromStream(Const Stream: TStream): TMatrixNxM;

Function VNToNxM(Const V: TvectorN): TMatrixNxM; // Konvertiert einen Vektor in eine identische Matrix der Breite 1 und Höhe N
Function NxMToVN(Const M: TMatrixNxM): TVectorN; // Konvertiert eine Matrix in einen Vektor, Zeilenweise eingelesen

// Operatoren
Function Equal(Const a, b: TVector2): Boolean; overload;
Function Equal(Const a, b: TVectorN): Boolean; overload;
Function Equal(Const a, b: TMatrixNxM): Boolean; overload;

Function AddV2(Const a, b: TVector2): Tvector2;
Function AddV3(Const a, b: TVector3): Tvector3;
Function AddV4(Const a, b: TVector4): Tvector4;
Function AddMatrix3x3(M1, M2: TMatrix3x3): TMatrix3x3;
Function AddMatrixNxM(M1, M2: TMatrixNxM): TMatrixNxM;

Function SubV2(Const a, b: TVector2): Tvector2;
Function SubV3(Const a, b: TVector3): Tvector3;
Function SubV4(Const a, b: TVector4): Tvector4;
Function SubVN(Const a, b: TVectorN): TvectorN;
Function SubMatrixNxM(M1, M2: TMatrixNxM): TMatrixNxM;

Function ScaleV2(Const Scalar: TBaseType; Const V: Tvector2): Tvector2;
Function ScaleV3(Const Scalar: TBaseType; Const V: Tvector3): Tvector3;
Function ScaleV4(Const Scalar: TBaseType; Const V: Tvector4): Tvector4;
Function ScaleNxM(Const s: TBaseType; Const M: TMatrixNxM): TMatrixNxM;

Function DotV2(Const a, b: TVector2): TBaseType;
Function DotV3(Const a, b: TVector3): TBaseType;
Function DotV4(Const a, b: TVector4): TBaseType;
Function DotVN(Const a, b: TVectorN): TBaseType;

Function HadamardV2(Const a, b: TVector2): TVector2; // Komponentenweise Multiplication genannt Hadamard Produkt ( https://de.wikipedia.org/wiki/Hadamard-Produkt )
Function HadamardV3(Const a, b: TVector3): TVector3; // Komponentenweise Multiplication genannt Hadamard Produkt ( https://de.wikipedia.org/wiki/Hadamard-Produkt )
Function HadamardV4(Const a, b: TVector4): TVector4; // Komponentenweise Multiplication genannt Hadamard Produkt ( https://de.wikipedia.org/wiki/Hadamard-Produkt )
Function HadamardVN(Const a, b: TvectorN): TvectorN; // Komponentenweise Multiplication genannt Hadamard Produkt ( https://de.wikipedia.org/wiki/Hadamard-Produkt )
Function HadamardNxM(Const a, b: TMatrixNxM): TMatrixNxM; // Komponentenweise Multiplication genannt Hadamard Produkt ( https://de.wikipedia.org/wiki/Hadamard-Produkt )

Function CrossV2(Const a, b: Tvector2): Tvector2;
Function CrossV3(Const a, b: Tvector3): Tvector3;

Function MulVectorMatrix(Const V: TVector2; Const M: TMatrix2x2): TVector2; overload;
Function MulVectorMatrix(Const V: TVector3; Const M: TMatrix3x3): TVector3; overload;
Function MulVectorMatrix(Const V: TVector4; Const M: TMatrix4x4): TVector4; overload;
Function MulVectorMatrix(Const V: TVectorN; Const M: TMatrixNxM): TVectorN; overload;

Function MulMatrix(Const M1, M2: TMatrix2x2): TMatrix2x2; overload;
Function MulMatrix(Const M1, M2: TMatrix3x3): TMatrix3x3; overload;
Function MulMatrix(Const M1, M2: TMatrix4x4): TMatrix4x4; overload;
Function MulMatrix(Const M1, M2: TMatrixNxM): TMatrixNxM; overload;

Function Abs(Const V: TVector2): TBaseType; overload;
Function Abs(Const V: TVector3): TBaseType; overload;
Function Abs(Const V: TVector4): TBaseType; overload;
Function LenV2(Const V: TVector2): TBaseType;
Function LenV3(Const V: TVector3): TBaseType;
Function LenV4(Const V: TVector4): TBaseType;
Function LenVN(Const V: TVectorN): TBaseType;
Function LenV2SQR(Const V: TVector2): TBaseType; // Gibt das Quadrat der Länge des Vectores zurück
Function LenV3SQR(Const V: TVector3): TBaseType; // Gibt das Quadrat der Länge des Vectores zurück
Function LenV4SQR(Const V: TVector4): TBaseType; // Gibt das Quadrat der Länge des Vectores zurück

Function NormV2(Const Value: Tvector2): Tvector2; // Skaliert Value auf Len = 1, 0 wenn Fehler
Function NormV3(Const Value: Tvector3): Tvector3; // Skaliert Value auf Len = 1, 0 wenn Fehler
Function NormV4(Const Value: Tvector4): Tvector4; // Skaliert Value auf Len = 1, 0 wenn Fehler

Function AngleV2(Const a, b: TVector2): TBaseType; // berechnet den Winkel zwischen 2 Vektoren, ACHTUNG, AngleV2 ist immer in [0..180], der Drehsinn wird Ignoriert
Function AngleV2_2(Const a, b: TVector2): TBaseType; // berechnet den Winkel zwischen 2 Vektoren unter Berücksichtigung des Drehsinns -> Damit ist der Winkel [0..360[

Function MinV2(Const A, B: TVector2): TVector2; // Gibt Kompenentenweise die jeweils kleinsten wieder
Function MinV3(Const a, b: TVector3): TVector3; // Gibt komponentenweise die jeweils kleinsten wieder
Function MaxV2(Const A, B: TVector2): TVector2; // Gibt Kompenentenweise die jeweils größten wieder
Function MaxV3(Const a, b: TVector3): TVector3; // Gibt komponentenweise die jeweils größten wieder

Function CeilV2(Const v: Tvector2): TVector2; // Wendet die Ceil Funktion auf jede Komponente von v an

Function Determinant(M: TMatrix2x2): TBaseType; overload; // Berechnet die Determinante einer 2x2 Matrix
Function Determinant(M: TMatrix3x3): TBaseType; overload; // Berechnet die Determinante einer 3x3 Matrix
Function Determinant(M: TMatrixNxM): TBaseType; overload; // Berechnet die Determinante einer NxN Matrix

Function InvertMatrix(M: TMatrix2x2): TMatrix2x2; overload; // Mittels Gauß-Jordan-Algorithmus
Function InvertMatrix(M: TMatrix3x3): TMatrix3x3; overload; // Mittels Gauß-Jordan-Algorithmus
Function InvertMatrix(M: TMatrix4x4): TMatrix4x4; overload; // Mittels Gauß-Jordan-Algorithmus
Function InvertMatrix2(M: TMatrix2x2): TMatrix2x2; overload; // Berechnung über die Adjunkte, 0 wenn nicht Invertierbar
Function InvertMatrix2(M: TMatrix3x3): TMatrix3x3; overload; // Berechnung über die Adjunkte, 0 wenn nicht Invertierbar
Function InvertMatrix2(M: TMatrix4x4): TMatrix4x4; overload; // Berechnung über die Adjunkte, 0 wenn nicht Invertierbar

Function TransposeVector(Const V: TVectorN): TMatrixNxM; // Transponiert einen Vektor und gibt das Ergebnis als Matrix der Breite N und Höhe 1 zurück
Function TransposeMatrix(Const M: TMatrix3x3): TMatrix3x3; overload;
Function TransposeMatrix(Const M: TMatrix4x4): TMatrix4x4; overload;
Function TransposeMatrix(Const M: TMatrixNxM): TMatrixNxM; overload;

(******************************************************************************)
(* Ab hier kommen Funktionen die über die Einfachen Vektoroperatoren hinaus   *)
(* gehen.                                                                     *)
(******************************************************************************)

Procedure GaussJordan(Var M: TMatrixNxM); // Wendet das Gaus Jourdan Verfahren auf die Matrix
Procedure MapMatrix(Var M: TMatrixNxM; MapFunction: TMapFunction); // Wendet die Funktion Mapfunktion auf alle Elemente einer Matrix an
Function MapMatrix2(Const M: TMatrixNxM; MapFunction: TMapFunction): TMatrixNxM; // Wendet die Funktion Mapfunktion auf alle Elemente einer Matrix an
// Erzeugt eine Matrix so, dass wenn ein Vektor mit dieser Matrix
// Multipliziert wird, er nachher um Angle Grad um den Ursprung gedreht wird.
Function CalculateRotationMatrix(Angle: TBaseType): TMatrix2x2; overload;

// Erzeugt eine Rotationsmatrix aus Angle und RotationVector
// Jeder Vektor, welcher mit dieser Matrix Multipliziert wird, wird um die in
// RotationVector angegebenen Achsen Rotiert ( siehe gLRotate )
Function CalculateRotationMatrix(Angle: TBaseType; RotationVector: TVector3): TMatrix4x4; overload;

// Berechnet die Augmatrix eines Kamerasystems
Function CalculateOrthoMatrix(EyePos, Dir, Up: TVector3): TMatrix4x4;

// Berechnet aus 3 gegebenen Punkten und einem Punkt "unterhalb" der aufgespannten
// Ebene einen zugehörigen Normalenvektor, welcher nach außen zeigt.
Function CalculateNormal(A, B, C, Inner: Tvector3): Tvector3;

// Berechnet die Orthogonale Projektion des Punktes M auf die Gerade durch AB und gibt den Lotfußpunkt zurück, wenn M in Gerade durch AB wird A zurück gegeben
Function CalculateOrthoganlProjection(A, B, M: TVector2): TVector2;

// Berechnet den Senkrechten Abstand zwischen der Geraden S+n*R und dem Punkt P
Function CalculatePointLineDistance(S, R, P: TVector2): TBaseType;

// Debugg Operationen
Function Print(Const V: Tvector2): String; overload;
Function Print(Const V: Tvector3): String; overload;
Function Print(Const V: Tvector4): String; overload;
Function Print(Const V: TVectorN): String; overload;

Function Print(Const M: TMatrix2x2): String; overload;
Function Print(Const M: TMatrix3x3): String; overload;
Function Print(Const M: TMatrix4x4): String; overload;
Function Print(Const M: TMatrixNxM): String; overload;

(******************************************************************************)
(* Ab hier kommen Funktionen die Mathematisch aber nicht direkt Vektoralgebra *)
(* sind.                                                                      *)
(******************************************************************************)

Function Sign(Value: TBaseType): Integer; // Gibt das Vorzeichen oder 0 zurück
Function Clamp(Value, Lower, Upper: integer): integer; overload; // Zwingt Value in den Bereich: Lower <= Value <= Upper
Function Clamp(Value, Lower, Upper: Single): Single; overload;
Function Mod2(a, b: Integer): integer; // Bildet Negative "a" auf die passenden positiven Restklassen von b ab.
Function RandomRange(a, b: integer): integer; // Erzeugt eine Zufallszahl im Bereich [a..b) Randomize muss vorher augerufen worden sein !!

// Konvertierungsfunktionen
Function ConvertDimension(vmin, vmax, v: TBaseType; rmin, rmax: TBaseType): TBaseType; // Rechnet einen Wert v in der Scala Vmin-Vmax in die passende Scala rmin-Rmax um. Details siehe unten
Function Map(Value, fromLow, fromHigh: TBaseType; toLow, toHigh: TBaseType): TBaseType; // Basically the same as ConvertDimension, but parameter compatible with the Arduino version from: https://www.arduino.cc/reference/de/language/functions/math/map/

// True wenn das Objekt das durch die Punkte Angegeben ist ein Konvexes Polygon
// Wichtig, die Reihenfolge der Punkte muss korrekt sein
// Der Drehsinn des Polygons wird automatisch ermittelt
Function PointArrIsConvexPolygon(Const Points: TVector2Array): Boolean;

// Berechnet aus einer Punktwolke die Convexe Hülle und gibt diese zurück
Function PointsToConvexHull(Const Points: TVector2Array): TVector2Array;

// Gibt eine Dreiecksliste zu Points zurück, welcher einer Delaunay Triangulierung entspricht
Function PointsToDelaunayTriangleList(Const Points: TVector2Array; CheckForDoublePoints: Boolean = True): TTriangleArray;

// Berechnet aus den 3 Punkten a,b,c einen Kreis, so dass die 3 Punkte auf dessen Kreisbahn liegen, ist der Radius Negativ, dann ist der Kreis eigentlich nicht Berechenbar gewesen.
Function PointsToCircumCircle(Const A, B, C: Tvector2): TCircle;

// Berechnet die Fläche eines durch die Punkte A, B, C gegebenen Dreiecks
Function CalculateTriangleArea(Const A, B, C: TVector2): TBaseType;

// Berechnet den Flächeninhalt eines Polygones
Function CalculatePolygonArea(Const Points: TVector2Array): TBaseType;

// Setzt die Shadow Matrix, derart, dass sie in OpenGL dazu benutzt werden kann, aus Sicht einer Lichtquelle "Schatten" werfende
// Objekte in den Stencil Buffer zu zeichnen.
// ShadowMatrix = Ergebniss
// Normale = Normale des Schatten Empfangenden Objects ( mus eine gerade Fläche sein
// Point = Ein Punkt der Schatten Empfandenden Fläche
// Koordinaten der Lichtquelle ( Im Zweifel ist die W-Komponente auf 1 zu setzen )!!
Procedure GenerateShadowMatrix(Out ShadowMatrix: TMatrix4x4; Const Normal, Point: TVector3; Light: TVector4);

// Errechnet den Winkel Zwischen (X,0) und (X,Y) im Gradmaß aus = Atan2(x,y) * 180 / pi;
Function ArcTangens(Const X, Y: Extended): Extended;

// True wenn, der Punkt P im Dreieck A,B,C liegt.
Function PointInTriangle(Const P: TVector2; A, B, C: TVector2): Boolean;

// Prüft ob P im Achsenparalellen Rechteck TL, BR ist
Function PointInRect(Const P, TL, BR: TVector2): Boolean;

// Prüft ob sich die beiden Achsenparalellen Rechtecke Schneiden ( mit Beinhalten )
Function RectIntersectRect(Const TL1, BR1, TL2, BR2: TVector2): Boolean;

// Prüft ob P im Achsenparalellen Quader TL, BR ist
Function PointInCube(Const P, TL, BR: Tvector3): Boolean;

// True, wenn P innerhalb des Polygons liegt
Function PointInPolygon(Const p: TVector2; Const Polygon: TVector2Array): Boolean;

// Die Punkte P1 - P4 Spannen ein Object Orientiertes Rechetck auf
// Die Punkte müssen im Gegenuhrzeigersinn übergeben werden.
// Result = True wenn P innerhalb dieses Reckeckes liegt
Function PointInOOBB(Const P, P1, P2, P3, P4: TVector2): Boolean;

// True, wenn sich die Geraden A+s*m_A, b+t*m_B schneiden, wenn True ist P der Schnittpunkt
Function IntersectLines(Const A, m_A, B, m_B: TVector2; Out P: TVector2): Boolean;

// True, wenn sich die Geradenstücke AB und CD Schneigen, Wenn True ist P der Schnittpunkt
Function IntersectLine_segments(Const A, B, C, D: TVector2; Out P: TVector2): Boolean;

//
// Gibt die Projektion des Punkts P auf den Lotfußpunkt der Geraden durch P1 und P2 zurück
//
Function CalculatePlumbFootPoint(Const P, P1, P2: TVector2): TVector2;

// Berechnet die Schnittpunkte einer Ellipse mit einem Streckenabschnitt AB
// Die Ellipse ist definiert durch M und hat die Ausdehnung Rx, Ry
// In P1 bzw. P2 sind die sich ergebenden Schnittpunkte hinterlegt
// Result = Anzahl der Schnittpunkte (0,1,2)
//          P2 ist Gültig bei result = 2
//          P1 ist Gültib bei result = 1,2
Function IntersectLineEllipse(Const A, B, M: TVector2; Rx, Ry: TBaseType; Out P1, P2: TVector2): integer;

// True wenn sich zwei OOBB's überschneiden
Function IntersectOOBB(Const A, B, C, D, AA, BB, CC, DD: TVector2): Boolean;

// Berechnet das Trägheitsmoment eines Körpers bezüglich einer Geraden die durch den Nullpunkt geht und den Richtungsvektor v hat.
// Entnommen aus dem Bronstein ( 5. Auflage erschienen 2001 ) Formel  4.70b
// Für genauere Deteils bitte dort Nachlesen
Function InertialTensor(v: TVector3; m: TBaseType): TMatrix3x3;

// Berechnet nach der Methode der Kleinsten Fehlerquadrate die Vorfaktoren
// für ein Anäherndes Polynom der Form
// f(x) = a_0 + a_1*x + a_2*x^2 + ... a_n*x^n
// mit n = Grade
//
// Inspired by: http://www.abi-mathe.de/buch/matrizen/methode-der-kleinsten-quadrate/
Function LeastSquares(Points: TVector2Array; Grade: integer): TVectorN;

// Berechnet das Ergebnis von
// f(x) = a_0 + a_1*x + a_2*x^2 + ... a_n*x^n
// an der Stelle x
// Die Koeffizienten von a werden als Vector übergben
Function CalculatePolynom(x: TBaseType; Const a: TVectorN): TBaseType;

Implementation

{$IFDEF UseOperandOverloading}

Operator := (p: TPoint): TVector2;
Begin
  result.x := p.x;
  result.y := p.y;
End;

Operator := (p: TVector2): TPoint;
Begin
  result.x := round(p.x);
  result.y := round(p.y);
End;

Operator := (v: TVector4): TVector3;
Begin
  result.x := v.x;
  result.y := v.y;
  result.z := v.z;
End;

Operator := (M: TMatrix3x3): TMatrixNxM;
Var
  i, j: Integer;
Begin
  result := Nil;
  setlength(result, 3, 3);
  For i := 0 To 2 Do Begin
    For j := 0 To 2 Do Begin
      result[i, j] := m[i, j];
    End;
  End;
End;

Operator - (v: TVector3): TVector3;
Begin
  result := v3(-v.x, -v.y, -v.z);
End;

Operator - (v: TVector4): TVector4;
Begin
  result := v4(-v.x, -v.y, -v.z, -v.w);
End;

Operator + (a, b: TVector2): TVector2;
Begin
  result := addv2(a, b);
End;

Operator + (a, b: TVector3): TVector3;
Begin
  result := addv3(a, b);
End;

Operator + (a, b: TVector4): TVector4;
Begin
  result := AddV4(a, b);
End;

Operator + (M1, M2: TMatrixNxM): TMatrixNxM;
Begin
  result := AddMatrixNxM(m1, m2);
End;

Operator - (a, b: TVector2): TVector2;
Begin
  result := subv2(a, b);
End;

Operator - (a, b: TVector3): TVector3;
Begin
  result := subv3(a, b);
End;

Operator - (a, b: TVector4): TVector4;
Begin
  result := SubV4(a, b);
End;

Operator - (a, b: TVectorN): TVectorN;
Begin
  result := SubVN(a, b);
End;

Operator - (M1, M2: TMatrixNxM): TMatrixNxM;
Begin
  result := SubMatrixNxM(m1, m2);
End;

Operator * (s: TBaseType; v: TVector2): TVector2;
Begin
  result := ScaleV2(s, v);
End;

Operator * (v: TVector2; s: TBaseType): TVector2;
Begin
  result := ScaleV2(s, v);
End;

Operator * (s: TBaseType; v: TVector3): TVector3;
Begin
  result := ScaleV3(s, v);
End;

Operator * (v: TVector3; s: TBaseType): TVector3;
Begin
  result := ScaleV3(s, v);
End;

Operator * (s: TBaseType; v: TVector4): TVector4;
Begin
  result := ScaleV4(s, v);
End;

Operator * (v: TVector4; s: TBaseType): TVector4;
Begin
  result := ScaleV4(s, v);
End;

Operator * (s: TBasetype; m: TMatrixNxM): TMatrixNxM;
Begin
  result := ScaleNxM(s, m);
End;

Operator * (a, b: TVector2): TBaseType;
Begin
  result := DotV2(a, b);
End;

Operator * (a, b: TVector3): TBaseType;
Begin
  result := DotV3(a, b);
End;

Operator * (a, b: TVector4): TBaseType;
Begin
  result := DotV4(a, b);
End;

Operator * (a, b: TVectorN): TBaseType;
Begin
  result := DotVN(a, b);
End;

Operator * (m: TMatrix2x2; v: TVector2): Tvector2;
Begin
  result := MulVectorMatrix(v, m);
End;

Operator * (m: TMatrix3x3; v: TVector3): Tvector3;
Begin
  result := MulVectorMatrix(v, m);
End;

Operator * (m: TMatrix4x4; v: TVector4): Tvector4;
Begin
  result := MulVectorMatrix(v, m);
End;

Operator * (m: TMatrixNxM; v: TVectorN): TVectorN;
Begin
  result := MulVectorMatrix(v, m);
End;

Operator * (M1, M2: TMatrix2x2): TMatrix2x2;
Begin
  result := MulMatrix(m1, m2);
End;

Operator * (M1, M2: TMatrix3x3): TMatrix3x3;
Begin
  result := MulMatrix(m1, m2);
End;

Operator * (M1, M2: TMatrixNxM): TMatrixNxM;
Begin
  result := MulMatrix(m1, m2);
End;

Operator / (s: TBaseType; v: TVector2): TVector2;
Begin
  result := ScaleV2(1 / s, v);
End;

Operator / (v: TVector2; s: TBaseType): TVector2;
Begin
  result := ScaleV2(1 / s, v);
End;

Operator / (s: TBaseType; v: TVector3): TVector3;
Begin
  result := ScaleV3(1 / s, v);
End;

Operator / (v: TVector3; s: TBaseType): TVector3;
Begin
  result := ScaleV3(1 / s, v);
End;

Operator / (s: TBaseType; v: TVector4): TVector4;
Begin
  result := ScaleV4(1 / s, v);
End;

Operator / (v: TVector4; s: TBaseType): TVector4;
Begin
  result := ScaleV4(1 / s, v);
End;

{$ENDIF}

Function Sign(Value: TBaseType): Integer; // Gibt das Vorzeichen oder 0 zurück
Begin
  If value = 0 Then Begin
    result := 0;
  End
  Else Begin
    If value > 0 Then Begin
      result := 1;
    End
    Else Begin
      result := -1;
    End;
  End;
End;

Function Clamp(Value, Lower, Upper: integer): integer;
Begin
  If value < lower Then Begin
    result := lower;
  End
  Else Begin
    If Value > Upper Then Begin
      result := upper;
    End
    Else Begin
      result := value;
    End;
  End;
End;

Function Clamp(Value, Lower, Upper: Single): Single;
Begin
  If value < lower Then Begin
    result := lower;
  End
  Else Begin
    If Value > Upper Then Begin
      result := upper;
    End
    Else Begin
      result := value;
    End;
  End;
End;

Function Mod2(a, b: Integer): integer;
Begin
  If a >= 0 Then Begin
    result := a Mod b;
  End
  Else Begin
    result := b - ((-a) Mod b);
  End;
End;

Function RandomRange(a, b: integer): integer;
Var
  i: integer;
Begin
  If a > b Then Begin
    i := a;
    a := b;
    b := i;
  End;
  i := b - a;
  result := random(i) + a;
End;

(*
 *       vmin / rmin                      vmax / rmax
 *       |                                |
 *    ---------------------------------------
 *                  |
 *                  v / result
 *
 * Berechnet den Wert von result so, dass er an der entsprechend
 * gleichen Stelle in rmin / rmax liegt, wie  v in vmin / vmax ist.
 *)

Function ConvertDimension(vmin, vmax, v: TBaseType; rmin, rmax: TBaseType
  ): TBaseType;
Begin
  If (vmax - vmin = 0) Then Begin
    result := rmin;
    exit;
  End
  Else Begin
    result := ((((v - vmin) * (rmax - rmin)) / (vmax - vmin)) + rmin);
  End;
End;

Function Map(Value, fromLow, fromHigh: TBaseType; toLow, toHigh: TBaseType
  ): TBaseType;
Begin
  result := ConvertDimension(fromLow, fromHigh, Value, toLow, toHigh);
End;

Function ZeroV2: TVector2;
Begin
  result := v2(0, 0);
End;

Function ZeroV3: TVector3;
Begin
  result := v3(0, 0, 0);
End;

Function ZeroV4: TVector4;
Begin
  result := v4(0, 0, 0, 0);
End;

Function Zero2x2(): TMatrix2x2;
Var
  i, j: Integer;
Begin
  For i := 0 To 1 Do Begin
    For j := 0 To 1 Do Begin
      result[i, j] := 0;
    End;
  End;
End;

Function Zero3x3: TMatrix3x3;
Var
  i, j: Integer;
Begin
  For i := 0 To 2 Do Begin
    For j := 0 To 2 Do Begin
      result[i, j] := 0;
    End;
  End;
End;

Function Zero4x4(): TMatrix4x4;
Var
  i, j: Integer;
Begin
  For i := 0 To 3 Do Begin
    For j := 0 To 3 Do Begin
      result[i, j] := 0;
    End;
  End;
End;

Function ZeroNxM(Cols, Rows: integer): TMatrixNxM;
Var
  i, j: Integer;
Begin
  result := Nil;
  setlength(result, cols, Rows);
  For i := 0 To cols - 1 Do Begin
    For j := 0 To Rows - 1 Do Begin
      result[i, j] := 0;
    End;
  End;
End;

Procedure RandomizeNxM(Var M: TMatrixNxM);
Var
  j, i: Integer;
Begin
  For j := 0 To high(M) Do Begin
    For i := 0 To high(M[j]) Do Begin
      M[j, i] := (Random()) * 2 - 1;
    End;
  End;
End;

Function V2(Const X, Y: TBaseType): TVector2;
Begin
  result.x := x;
  result.y := y;
End;

Function V3(Const X, Y, Z: TBaseType): TVector3;
Begin
  result.x := x;
  result.y := y;
  result.z := z;
End;

Function V4(Const X, Y, Z, W: TBaseType): TVector4;
Begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.w := w;
End;

Function V4(Const V: Tvector3; Const W: TBaseType): TVector4;
Begin
  result.x := v.x;
  result.y := v.y;
  result.z := v.z;
  result.w := w;
End;

Function VN(Const Values: Array Of TBaseType): TVectorN;
Var
  i: Integer;
Begin
  result := Nil;
  setlength(result, length(values));
  For i := 0 To high(Values) Do Begin
    result[i] := Values[i];
  End;
End;

Function Abs(Const V: TVector2): TBaseType;
Begin
  result := LenV2(v);
End;

Function Abs(Const V: TVector3): TBaseType;
Begin
  result := LenV3(v);
End;

Function Abs(Const V: TVector4): TBaseType;
Begin
  result := LenV4(v);
End;

Function M2x2(x11, x12, x21, x22: TBaseType): TMatrix2x2;
Begin
  Result[0, 0] := x11;
  Result[0, 1] := x12;
  Result[1, 0] := x21;
  Result[1, 1] := x22;
End;

Function M3x3(x11, x12, x13, x21, x22, x23, x31, x32, x33: TBaseType
  ): TMatrix3x3;
Begin
  Result[0, 0] := x11;
  Result[0, 1] := x12;
  Result[0, 2] := x13;
  Result[1, 0] := x21;
  Result[1, 1] := x22;
  Result[1, 2] := x23;
  Result[2, 0] := x31;
  Result[2, 1] := x32;
  Result[2, 2] := x33;
End;

Function M4x4(x11, x12, x13, x14, x21, x22, x23, x24, x31, x32, x33, x34, x41,
  x42, x43, x44: TBaseType): TMatrix4x4;
Begin
  Result[0, 0] := x11;
  Result[0, 1] := x12;
  Result[0, 2] := x13;
  Result[0, 3] := x14;
  Result[1, 0] := x21;
  Result[1, 1] := x22;
  Result[1, 2] := x23;
  Result[1, 3] := x24;
  Result[2, 0] := x31;
  Result[2, 1] := x32;
  Result[2, 2] := x33;
  Result[2, 3] := x34;
  Result[3, 0] := x41;
  Result[3, 1] := x42;
  Result[3, 2] := x43;
  Result[3, 3] := x44;
End;

Function MNxM(Cols: integer; Const Elements: Array Of TBaseType): TMatrixNxM;
Var
  v: TVectorN;
Begin
  v := VN(Elements);
  result := MNxM(cols, v);
End;

Function MNxM(Cols: integer; V: TvectorN): TMatrixNxM;
Var
  rows, j, i, c: Integer;
Begin
  result := Nil;
  If Length(v) Mod Cols <> 0 Then Begin
    Raise Exception.Create('uvectormath.MNxM: invalid size for elements');
  End;
  rows := length(v) Div Cols;
  setlength(result, Cols, rows);
  c := 0;
  For j := 0 To rows - 1 Do Begin
    For i := 0 To cols - 1 Do Begin
      result[i, j] := v[c];
      inc(c);
    End;
  End;
End;

Procedure SaveMNxMToStream(Const Stream: TStream; Const Data: TMatrixNxM);
Var
  i, j: UInt32;
Begin
  // Speichern der Dimension
  i := length(data);
  stream.Write(i, sizeof(i));
  j := length(data[0]);
  stream.Write(j, sizeof(j));
  For i := 0 To high(data) Do Begin
    For j := 0 To high(data[i]) Do Begin
      stream.Write(data[i, j], sizeof(data[i, j]));
    End;
  End;
End;

Function LoadMNxMFromStream(Const Stream: TStream): TMatrixNxM;
Var
  i, j: UInt32;
Begin
  result := Nil;
  // Speichern der Dimension
  i := 0;
  stream.Read(i, sizeof(i));
  j := 0;
  stream.Read(j, sizeof(j));
  setlength(Result, i, j);
  For i := 0 To high(Result) Do Begin
    For j := 0 To high(Result[i]) Do Begin
      stream.Read(Result[i, j], sizeof(Result[i, j]));
    End;
  End;
End;

Function VNToNxM(Const V: TvectorN): TMatrixNxM;
Begin
  result := MNxM(1, v);
End;

Function NxMToVN(Const M: TMatrixNxM): TVectorN;
Var
  i, c, j: Integer;
Begin
  result := Nil;
  setlength(result, length(m) * length(m[0]));
  c := 0;
  For j := 0 To high(m[0]) Do Begin
    For i := 0 To high(m) Do Begin
      result[c] := m[i, j];
      inc(c);
    End;
  End;
End;

Function MinV2(Const A, B: TVector2): TVector2;
Begin
  result.x := min(a.x, b.x);
  result.y := min(a.y, b.y);
End;

Function MinV3(Const a, b: TVector3): TVector3;
Begin
  result.x := min(a.x, b.x);
  result.y := min(a.y, b.y);
  result.z := min(a.z, b.z);
End;

Function MaxV2(Const A, B: TVector2): TVector2;
Begin
  result.x := max(a.x, b.x);
  result.y := max(a.y, b.y);
End;

Function MaxV3(Const a, b: TVector3): TVector3;
Begin
  result.x := max(a.x, b.x);
  result.y := max(a.y, b.y);
  result.z := max(a.z, b.z);
End;

Function Equal(Const a, b: TVector2): Boolean;
Begin
  result := a.Equal(b);
End;

Function Equal(Const a, b: TVectorN): Boolean;
Begin
  result := a.Equal(b);
End;

Function AddV2(Const a, b: TVector2): Tvector2;
Begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
End;

Function AddV3(Const a, b: TVector3): Tvector3;
Begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
  result.z := a.z + b.z;
End;

Function AddV4(Const a, b: TVector4): Tvector4;
Begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
  result.z := a.z + b.z;
  result.w := a.w + b.w;
End;

Function SubV2(Const a, b: TVector2): Tvector2;
Begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
End;

Function SubV3(Const a, b: TVector3): Tvector3;
Begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
  result.z := a.z - b.z;
End;

Function SubV4(Const a, b: TVector4): Tvector4;
Begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
  result.z := a.z - b.z;
  result.w := a.w - b.w;
End;

Function SubVN(Const a, b: TVectorN): TvectorN;
Var
  i: Integer;
Begin
  result := Nil;
  If high(a) <> high(b) Then Begin
    Raise Exception.Create('uvectormath.SubVN: invalid dimension.');
  End;
  setlength(result, length(a));
  For i := 0 To high(a) Do Begin
    result[i] := a[i] - b[i];
  End;
End;

Function LenV2(Const V: TVector2): TBaseType;
Begin
  result := sqrt(sqr(v.x) + sqr(v.y));
End;

Function LenV3(Const V: TVector3): TBaseType;
Begin
  result := sqrt(sqr(v.x) + sqr(v.y) + sqr(v.z));
End;

Function LenV4(Const V: TVector4): TBaseType;
Begin
  result := sqrt(sqr(v.x) + sqr(v.y) + sqr(v.z) + sqr(v.w));
End;

Function LenVN(Const V: TVectorN): TBaseType;
Var
  i: integer;
Begin
  result := 0;
  For i := 0 To high(v) Do Begin
    result := result + sqr(v[i]);
  End;
  result := sqrt(result);
End;

Function LenV2SQR(Const V: TVector2): TBaseType;
Begin
  result := sqr(v.x) + sqr(v.y);
End;

Function LenV3SQR(Const V: TVector3): TBaseType;
Begin
  result := sqr(v.x) + sqr(v.y) + sqr(v.z);
End;

Function LenV4SQR(Const V: TVector4): TBaseType;
Begin
  result := sqr(v.x) + sqr(v.y) + sqr(v.z) + sqr(v.w);
End;

Function NormV2(Const Value: Tvector2): Tvector2;
Var
  betrag: TBaseType;
Begin
  betrag := lenV2(value);
  If Betrag <> 0 Then
    result := ScaleV2(1 / Betrag, Value)
  Else
    result := v2(0.0, 0.0);
End;

Function NormV3(Const Value: Tvector3): Tvector3;
Var
  betrag: TBaseType;
Begin
  betrag := lenV3(value);
  If Betrag <> 0 Then
    result := ScaleV3(1 / Betrag, Value)
  Else
    result := v3(0.0, 0.0, 0.0);
End;

Function NormV4(Const Value: Tvector4): Tvector4;
Var
  betrag: TBaseType;
Begin
  betrag := lenV4(value);
  If Betrag <> 0 Then
    result := ScaleV4(1 / Betrag, Value)
  Else
    result := v4(0.0, 0.0, 0.0, 0.0);
End;

Function ScaleV2(Const Scalar: TBaseType; Const V: Tvector2): Tvector2;
Begin
  result.x := scalar * v.x;
  result.y := scalar * v.y;
End;

Function ScaleV3(Const Scalar: TBaseType; Const V: Tvector3): Tvector3;
Begin
  result.x := scalar * v.x;
  result.y := scalar * v.y;
  result.z := scalar * v.z;
End;

Function ScaleV4(Const Scalar: TBaseType; Const V: Tvector4): Tvector4;
Begin
  result.x := scalar * v.x;
  result.y := scalar * v.y;
  result.z := scalar * v.z;
  result.w := scalar * v.w;
End;

Function DotV2(Const a, b: TVector2): TBaseType;
Begin
  result := a.x * b.x + a.y * b.y;
End;

Function DotV3(Const a, b: TVector3): TBaseType;
Begin
  result := a.x * b.x + a.y * b.y + a.z * b.z;
End;

Function DotV4(Const a, b: TVector4): TBaseType;
Begin
  result := a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w;
End;

Function DotVN(Const a, b: TVectorN): TBaseType;
Var
  i: Integer;
Begin
  result := 0;
  If high(a) <> high(b) Then Begin
    Raise Exception.Create('uvectormath.DotVN: invalid dimension.');
  End;
  For i := 0 To high(a) Do Begin
    result := result + a[i] * b[i];
  End;
End;

Function HadamardV2(Const a, b: TVector2): TVector2;
Begin
  result.x := a.x * b.x;
  result.y := a.y * b.y;
End;

Function HadamardV3(Const a, b: TVector3): TVector3;
Begin
  result.x := a.x * b.x;
  result.y := a.y * b.y;
  result.z := a.z * b.z;
End;

Function HadamardV4(Const a, b: TVector4): TVector4;
Begin
  result.x := a.x * b.x;
  result.y := a.y * b.y;
  result.z := a.z * b.z;
  result.w := a.w * b.w;
End;

Function HadamardVN(Const a, b: TvectorN): TvectorN;
Var
  i: Integer;
Begin
  result := Nil;
  If length(a) <> Length(b) Then Begin
    Raise exception.Create('uvectormath.HadamardVN: invalid dimension');
  End;
  setlength(result, length(a));
  For i := 0 To high(a) Do Begin
    result[i] := a[i] * b[i];
  End;
End;

Function HadamardNxM(Const a, b: TMatrixNxM): TMatrixNxM;
Var
  i, j: integer;
Begin
  result := Nil;
  If (length(a) <> length(b)) Or
    (length(a[0]) <> length(b[0])) Then Raise Exception.Create('uvectormath.HadamardNxM: invalid dimension.');
  setlength(result, length(a), length(a[0]));
  For i := 0 To high(a) Do Begin
    For j := 0 To high(a[i]) Do Begin
      result[i, j] := a[i, j] * b[i, j];
    End;
  End;
End;

Function AngleV2(Const a, b: TVector2): TBaseType;
Var
  w, z, n: TBasetype;
Begin
  n := abs(lenv2(a) * lenv2(b));
  If n <> 0 Then Begin
    z := Dotv2(a, b);
    w := clamp(z / n, -1, 1);
    result := radtodeg(arccos(w));
  End
  Else
    result := 0;
End;

Function AngleV2_2(Const a, b: TVector2): TBaseType;
Var
  av1, av2: Single;
Begin
  av1 := AngleV2(v2(1, 0), a);
  If a.y < 0 Then Begin
    av1 := 360 - av1;
  End;
  av2 := AngleV2(v2(1, 0), b);
  If b.y < 0 Then Begin
    av2 := 360 - av2;
  End;
  result := av2 - av1;
  If result < 0 Then result := result + 360;
  If result > 360 Then result := result - 360;
End;

Function CrossV2(Const a, b: Tvector2): Tvector2;
Begin
  result := v2(
    a.y * b.x - a.x * b.y,
    a.x * b.y - a.y * b.x
    );
End;

Function CrossV3(Const a, b: Tvector3): Tvector3;
Begin
  result := v3(
    a.y * b.z - a.z * b.y,
    a.z * b.x - a.x * b.z,
    a.x * b.y - a.y * b.x
    );
End;

Function IdentityMatrix2x2: TMatrix2x2;
Begin
  result[0, 0] := 1;
  result[1, 0] := 0;
  result[0, 1] := 0;
  result[1, 1] := 1;
End;

Function IdentityMatrix3x3: TMatrix3x3;
Begin
  result[0, 0] := 1;
  result[1, 0] := 0;
  result[2, 0] := 0;
  result[0, 1] := 0;
  result[1, 1] := 1;
  result[2, 1] := 0;
  result[0, 2] := 0;
  result[1, 2] := 0;
  result[2, 2] := 1;
End;

Function IdentityMatrix4x4: TMatrix4x4;
Begin
  result[0, 0] := 1;
  result[1, 0] := 0;
  result[2, 0] := 0;
  result[3, 0] := 0;
  result[0, 1] := 0;
  result[1, 1] := 1;
  result[2, 1] := 0;
  result[3, 1] := 0;
  result[0, 2] := 0;
  result[1, 2] := 0;
  result[2, 2] := 1;
  result[3, 2] := 0;
  result[0, 3] := 0;
  result[1, 3] := 0;
  result[2, 3] := 0;
  result[3, 3] := 1;
End;

Function CeilV2(Const v: Tvector2): TVector2;
Begin
  result.x := ceil(v.x);
  result.y := ceil(v.y);
End;

Function Determinant(M: TMatrix2x2): TBaseType;
Begin
  result := m[0, 0] * m[1, 1] - m[0, 1] * m[1, 0];
End;

Function Determinant(M: TMatrix3x3): TBaseType;
Begin
  result :=
    -m[0, 0] * m[1, 2] * m[2, 1]
    - m[1, 0] * m[0, 1] * m[2, 2]
    - m[2, 0] * m[0, 2] * m[1, 1]
    + m[0, 0] * m[1, 1] * m[2, 2]
    + m[1, 0] * m[0, 2] * m[2, 1]
    + m[2, 0] * m[0, 1] * m[1, 2];
End;

(*
 * Berechnet die Determinante einer Matrix nach dem Gauß Algorithmus
 *
 * Inspiriert durch: https://www.mathebibel.de/determinante-berechnen-nach-gauss
 *)

Function Determinant(M: TMatrixNxM): TBaseType;

  Procedure SwapRow(x, y: integer);
  Var
    i: integer;
    t: TBaseType;
  Begin
    For i := 0 To high(m) Do Begin
      t := m[i, x];
      m[i, x] := m[i, y];
      m[i, y] := t;
    End;
  End;

  Procedure SwapCol(x, y: integer);
  Var
    i: integer;
    t: TBaseType;
  Begin
    For i := 0 To high(m) Do Begin
      t := m[x, i];
      m[x, i] := m[y, i];
      m[y, i] := t;
    End;
  End;

  (*
   * x,y Zeile, s scalar
   * y := y + s*x
   *)
  Procedure AddScaled(x: integer; s: TBaseType; y: integer);
  Var
    i: Integer;
  Begin
    For i := 0 To high(M) Do Begin
      m[i, y] := m[i, y] + s * m[i, x];
    End;
  End;

Var
  p, s: TBaseType;
  x, y: Integer;
Begin
  result := 0;
  If length(m) <> (length(m[0])) Then Raise exception.Create('uvectormath.Determinant: No NxN-Matrix.');
  s := 1; // Das Vorzeichen der Determinante ändert sich immer dann, wenn 2 Zeilen / Spalten mit einander Vertauscht werden
  p := 1; // Das Produkt mit dem die Determinante am Schluss multipliziert werden muss
  // Herstellen der Dreiecksmatrix
  For x := 0 To high(m) Do Begin
    If m[x, x] = 0 Then Begin
      // Wir müssen eine Zeile Tauschen
      For y := x + 1 To high(m) Do Begin
        If m[x, y] <> 0 Then Begin
          // Tausche Zeile X mit Zeile Y
          SwapRow(x, y);
          s := -1 * s;
        End;
        break;
      End;
    End;
    If m[x, x] = 0 Then Begin
      // Wir müssen eine Spalte Tauschen
      For y := x + 1 To high(m) Do Begin
        If m[y, x] <> 0 Then Begin
          // Tausche Spalte X mit Spalte Y
          SwapCol(x, y);
          s := -1 * s;
        End;
        break;
      End;
    End;
    If m[x, x] = 0 Then Begin
      result := 0;
      exit;
    End;
    // Ab Jetzt Wissen wir, das m[x,x] <> 0 ist.
    If m[x, x] <> 1 Then Begin // Die Zeile so Skallieren, dass sie Links von X 0en hat und x = 1 und Rechts irgendwas
      p := p * m[x, x];
      For y := x + 1 To high(M) Do Begin
        m[y, x] := m[y, x] / m[x, x];
      End;
      m[x, x] := 1;
    End;
    // Ab Jetzt wissen wir das m[x,x] = 1 ist, nun können die 0en Erzeugt werden
    For y := x + 1 To high(m) Do Begin
      If m[x, y] <> 0 Then Begin // Wo schon eine 0 ist, braucht auch keine Mehr erzeugt werden.
        AddScaled(x, -m[x, y], y);
      End;
    End;
  End;
  // Nun ist die Berechnung der Determinante einfach
  result := p;
  For x := 0 To high(M) Do Begin
    result := result * m[x, x];
  End;
End;

Function Equal(Const a, b: TMatrixNxM): Boolean;
Begin
  result := a.Equal(b);
End;

Function AddMatrix3x3(M1, M2: TMatrix3x3): TMatrix3x3;
Var
  i, j: Integer;
Begin
  For i := 0 To 2 Do
    For j := 0 To 2 Do
      Result[i, j] := m1[i, j] + m2[i, j];
End;

Function AddMatrixNxM(M1, M2: TMatrixNxM): TMatrixNxM;
Var
  i, j: Integer;
Begin
  result := Nil;
  If (length(m1) <> length(m2)) Or
    (length(m1[0]) <> length(m2[0])) Then Begin
    Raise exception.Create('uvectormath.AddMatrixNxM: invalid dimension');
  End;
  setlength(result, length(m1), length(m1[0]));
  For i := 0 To high(m1) Do
    For j := 0 To high(m1[0]) Do
      Result[i, j] := m1[i, j] + m2[i, j];
End;

Function SubMatrixNxM(M1, M2: TMatrixNxM): TMatrixNxM;
Var
  i, j: Integer;
Begin
  result := Nil;
  If (length(m1) <> length(m2)) Or
    (length(m1[0]) <> length(m2[0])) Then Begin
    Raise exception.Create('uvectormath.SubMatrixNxM: invalid dimension');
  End;
  setlength(result, length(m1), length(m1[0]));
  For i := 0 To high(m1) Do
    For j := 0 To high(m1[0]) Do
      Result[i, j] := m1[i, j] - m2[i, j];
End;

(*

Berechnet zu
    ( x x x )
M = ( x x x )
    ( x x x )

die Matrix M' so das gilt

M * M' = 1

Wenn dies nicht möglich ist, weil z.b. die Determinante = 0, wird eine Exception geworfen.

*)

Function InvertMatrix(M: TMatrix2x2): TMatrix2x2;
Const
  dim = 2 - 1;
  (*
  Addiert auf Zeile x die Zeile y
  *)
  Procedure Line_X_Add_Line_Y(x, y: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To dim Do Begin
      result[i, x] := result[i, x] + result[i, y];
      m[i, x] := m[i, x] + m[i, y];
    End;
  End;

  (*
  Zieht von Zeile x die Zeile y ab.
  *)
  Procedure Line_X_Sub_V_Mul_Line_Y(x: integer; v: TBaseType; y: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To dim Do Begin
      result[i, x] := result[i, x] - v * result[i, y];
      m[i, x] := m[i, x] - v * m[i, y];
    End;
  End;

  (*
  Multipliziert die Zeile Line mit dem Wert Value durch
  *)
  Procedure Mul_Line(Value: TBaseType; Line: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To dim Do Begin
      result[i, Line] := result[i, Line] * Value;
      m[i, Line] := m[i, Line] * Value;
    End;
  End;

Var
  i, j: Integer;
  b: Boolean;
Begin
  // Init Ergebniss mit 1 Matrix
  For i := 0 To dim Do
    For j := 0 To dim Do
      If i = j Then
        result[i, j] := 1
      Else
        Result[i, j] := 0;
  // Wir Lösen Spaltenweise
  For i := 0 To dim Do Begin
    // Auf den Diagonal Elementen Erzeugen wir die 1, also darf hier keine 0 stehen.
    If M[i, i] = 0 Then Begin
      b := false;
      For j := i + 1 To dim Do Begin
        If m[i, j] <> 0 Then Begin
          Line_X_Add_Line_Y(i, j);
          b := True;
          break;
        End;
      End;
      If Not b Then Begin
        Raise Exception.create('uvectormath.InvertMatrix: Error unable to invert the Matrix.');
      End;
    End;
    // Nun Lösen wir die Spalte
    // Zuerst erzeugen wir die 1
    Mul_Line(1 / m[i, i], i);
    // Dann Erzeugen wir die 0 en
    For j := 0 To dim Do
      If i <> j Then
        Line_X_Sub_V_Mul_Line_Y(j, m[i, j], i);
  End;

End;

Function InvertMatrix(M: TMatrix3x3): TMatrix3x3;
Const
  dim = 3 - 1;
  (*
  Addiert auf Zeile x die Zeile y
  *)
  Procedure Line_X_Add_Line_Y(x, y: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To dim Do Begin
      result[i, x] := result[i, x] + result[i, y];
      m[i, x] := m[i, x] + m[i, y];
    End;
  End;

  (*
  Zieht von Zeile x die Zeile y ab.
  *)
  Procedure Line_X_Sub_V_Mul_Line_Y(x: integer; v: TBaseType; y: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To dim Do Begin
      result[i, x] := result[i, x] - v * result[i, y];
      m[i, x] := m[i, x] - v * m[i, y];
    End;
  End;

  (*
  Multipliziert die Zeile Line mit dem Wert Value durch
  *)
  Procedure Mul_Line(Value: TBaseType; Line: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To dim Do Begin
      result[i, Line] := result[i, Line] * Value;
      m[i, Line] := m[i, Line] * Value;
    End;
  End;

Var
  i, j: Integer;
  b: Boolean;
Begin
  // Init Ergebniss mit 1 Matrix
  For i := 0 To dim Do
    For j := 0 To dim Do
      If i = j Then
        result[i, j] := 1
      Else
        Result[i, j] := 0;
  // Wir Lösen Spaltenweise
  For i := 0 To dim Do Begin
    // Auf den Diagonal Elementen Erzeugen wir die 1, also darf hier keine 0 stehen.
    If M[i, i] = 0 Then Begin
      b := false;
      For j := i + 1 To dim Do Begin
        If m[i, j] <> 0 Then Begin
          Line_X_Add_Line_Y(i, j);
          b := True;
          break;
        End;
      End;
      If Not b Then Begin
        Raise Exception.create('uvectormath.InvertMatrix: Error unable to invert the Matrix.');
      End;
    End;
    // Nun Lösen wir die Spalte
    // Zuerst erzeugen wir die 1
    Mul_Line(1 / m[i, i], i);
    // Dann Erzeugen wir die 0 en
    For j := 0 To dim Do
      If i <> j Then
        Line_X_Sub_V_Mul_Line_Y(j, m[i, j], i);
  End;
End;

(*

Berechnet zu
    ( x x x x )
    ( x x x x )
M = ( x x x x )
    ( x x x x )

die Matrix M' so das gilt

M * M' = 1

Wenn dies nicht möglich ist, weil z.b. die Determinante = 0, wird eine Exception geworfen.

*)

Function InvertMatrix(M: TMatrix4x4): TMatrix4x4;
Const
  Dim = 4 - 1;
  (*
  Addiert auf Zeile x die Zeile y
  *)
  Procedure Line_X_Add_Line_Y(x, y: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To Dim Do Begin
      result[i, x] := result[i, x] + result[i, y];
      m[i, x] := m[i, x] + m[i, y];
    End;
  End;

  (*
  Zieht von Zeile x die Zeile y ab.
  *)
  Procedure Line_X_Sub_V_Mul_Line_Y(x: integer; v: TBaseType; y: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To Dim Do Begin
      result[i, x] := result[i, x] - v * result[i, y];
      m[i, x] := m[i, x] - v * m[i, y];
    End;
  End;

  (*
  Multipliziert die Zeile Line mit dem Wert Value durch
  *)
  Procedure Mul_Line(Value: TBaseType; Line: Integer);
  Var
    i: Integer;
  Begin
    For i := 0 To Dim Do Begin
      result[i, Line] := result[i, Line] * Value;
      m[i, Line] := m[i, Line] * Value;
    End;
  End;

Var
  i, j: Integer;
  b: Boolean;
Begin
  // Init Ergebniss mit 1 Matrix
  For i := 0 To Dim Do
    For j := 0 To Dim Do
      If i = j Then
        result[i, j] := 1
      Else
        Result[i, j] := 0;
  // Wir Lösen Spaltenweise
  For i := 0 To Dim Do Begin
    // Auf den Diagonal Elementen Erzeugen wir die 1, also darf hier keine 0 stehen.
    If M[i, i] = 0 Then Begin
      b := false;
      For j := i + 1 To Dim Do Begin
        If m[i, j] <> 0 Then Begin
          Line_X_Add_Line_Y(i, j);
          b := True;
          break;
        End;
      End;
      If Not b Then Begin
        Raise Exception.create('uvectormath.InvertMatrix: Error unable to invert the Matrix.');
      End;
    End;
    // Nun Lösen wir die Spalte
    // Zuerst erzeugen wir die 1
    Mul_Line(1 / m[i, i], i);
    // Dann Erzeugen wir die 0 en
    For j := 0 To Dim Do
      If i <> j Then
        Line_X_Sub_V_Mul_Line_Y(j, m[i, j], i);
  End;
End;

Function InvertMatrix2(M: TMatrix2x2): TMatrix2x2;
Var
  det: TBaseType;
Begin
  det := m[0, 0] * m[1, 1] - m[0, 1] * m[1, 0];
  If det = 0 Then Begin
    result := Zero2x2();
  End
  Else Begin
    Result[0, 0] := m[1, 1] / det;
    Result[0, 1] := -m[0, 1] / det;
    Result[1, 0] := -m[1, 0] / det;
    Result[1, 1] := m[0, 0] / det;
  End;
End;

Function InvertMatrix2(M: TMatrix3x3): TMatrix3x3;
Var
  det: TBaseType;
Begin
  det := -m[0, 0] * m[1, 2] * m[2, 1]
    - m[1, 0] * m[0, 1] * m[2, 2]
    - m[2, 0] * m[0, 2] * m[1, 1]
    + m[0, 0] * m[1, 1] * m[2, 2]
    + m[1, 0] * m[0, 2] * m[2, 1]
    + m[2, 0] * m[0, 1] * m[1, 2];
  If det = 0 Then Begin
    result := Zero3x3();
  End
  Else Begin
    Result[0, 0] := (m[1, 1] * m[2, 2] - m[1, 2] * m[2, 1]) / det;
    Result[0, 1] := (-m[0, 1] * m[2, 2] + m[0, 2] * m[2, 1]) / det;
    Result[0, 2] := (m[0, 1] * m[1, 2] - m[0, 2] * m[1, 1]) / det;
    Result[1, 0] := (-m[1, 0] * m[2, 2] + m[1, 2] * m[2, 0]) / det;
    Result[1, 1] := (m[0, 0] * m[2, 2] - m[0, 2] * m[2, 0]) / det;
    Result[1, 2] := (-m[0, 0] * m[1, 2] + m[0, 2] * m[1, 0]) / det;
    Result[2, 0] := (m[1, 0] * m[2, 1] - m[1, 1] * m[2, 0]) / det;
    Result[2, 1] := (-m[0, 0] * m[2, 1] + m[0, 1] * m[2, 0]) / det;
    Result[2, 2] := (m[0, 0] * m[1, 1] - m[0, 1] * m[1, 0]) / det;
  End;
End;

Function InvertMatrix2(M: TMatrix4x4): TMatrix4x4;
Var
  det: TBaseType;
Begin
  det := m[0, 0] * m[1, 1] * m[2, 2] * m[3, 3] -
    m[0, 0] * m[1, 1] * m[2, 3] * m[3, 2] -
    m[0, 0] * m[1, 2] * m[2, 1] * m[3, 3] +
    m[0, 0] * m[1, 2] * m[2, 3] * m[3, 1] +
    m[0, 0] * m[1, 3] * m[2, 1] * m[3, 2] -
    m[0, 0] * m[1, 3] * m[2, 2] * m[3, 1] -
    m[0, 1] * m[1, 0] * m[2, 2] * m[3, 3] +
    m[0, 1] * m[1, 0] * m[2, 3] * m[3, 2] +
    m[0, 1] * m[1, 2] * m[2, 0] * m[3, 3] -
    m[0, 1] * m[1, 2] * m[2, 3] * m[3, 0] -
    m[0, 1] * m[1, 3] * m[2, 0] * m[3, 2] +
    m[0, 1] * m[1, 3] * m[2, 2] * m[3, 0] +
    m[0, 2] * m[1, 0] * m[2, 1] * m[3, 3] -
    m[0, 2] * m[1, 0] * m[2, 3] * m[3, 1] -
    m[0, 2] * m[1, 1] * m[2, 0] * m[3, 3] +
    m[0, 2] * m[1, 1] * m[2, 3] * m[3, 0] +
    m[0, 2] * m[1, 3] * m[2, 0] * m[3, 1] -
    m[0, 2] * m[1, 3] * m[2, 1] * m[3, 0] -
    m[0, 3] * m[1, 0] * m[2, 1] * m[3, 2] +
    m[0, 3] * m[1, 0] * m[2, 2] * m[3, 1] +
    m[0, 3] * m[1, 1] * m[2, 0] * m[3, 2] -
    m[0, 3] * m[1, 1] * m[2, 2] * m[3, 0] -
    m[0, 3] * m[1, 2] * m[2, 0] * m[3, 1] +
    m[0, 3] * m[1, 2] * m[2, 1] * m[3, 0];
  If det = 0 Then Begin
    result := Zero4x4();
  End
  Else Begin
    Result[0, 0] := (m[1, 1] * (m[2, 2] * m[3, 3] - m[2, 3] * m[3, 2]) - m[1, 2] * (m[2, 1] * m[3, 3] - m[2, 3] * m[3, 1]) + m[1, 3] * (m[2, 1] * m[3, 2] - m[2, 2] * m[3, 1])) / det;
    Result[0, 1] := (-m[0, 1] * (m[2, 2] * m[3, 3] - m[2, 3] * m[3, 2]) + m[0, 2] * (m[2, 1] * m[3, 3] - m[2, 3] * m[3, 1]) - m[0, 3] * (m[2, 1] * m[3, 2] - m[2, 2] * m[3, 1])) / det;
    Result[0, 2] := (m[0, 1] * (m[1, 2] * m[3, 3] - m[1, 3] * m[3, 2]) - m[0, 2] * (m[1, 1] * m[3, 3] - m[1, 3] * m[3, 1]) + m[0, 3] * (m[1, 1] * m[3, 2] - m[1, 2] * m[3, 1])) / det;
    Result[0, 3] := (-m[0, 1] * (m[1, 2] * m[2, 3] - m[1, 3] * m[2, 2]) + m[0, 2] * (m[1, 1] * m[2, 3] - m[1, 3] * m[2, 1]) - m[0, 3] * (m[1, 1] * m[2, 2] - m[1, 2] * m[2, 1])) / det;

    Result[1, 0] := (-m[1, 0] * (m[2, 2] * m[3, 3] - m[2, 3] * m[3, 2]) + m[1, 2] * (m[2, 0] * m[3, 3] - m[2, 3] * m[3, 0]) - m[1, 3] * (m[2, 0] * m[3, 2] - m[2, 2] * m[3, 0])) / det;
    Result[1, 1] := (m[0, 0] * (m[2, 2] * m[3, 3] - m[2, 3] * m[3, 2]) - m[0, 2] * (m[2, 0] * m[3, 3] - m[2, 3] * m[3, 0]) + m[0, 3] * (m[2, 0] * m[3, 2] - m[2, 2] * m[3, 0])) / det;
    Result[1, 2] := (-m[0, 0] * (m[1, 2] * m[3, 3] - m[1, 3] * m[3, 2]) + m[0, 2] * (m[1, 0] * m[3, 3] - m[1, 3] * m[3, 0]) - m[0, 3] * (m[1, 0] * m[3, 2] - m[1, 2] * m[3, 0])) / det;
    Result[1, 3] := (m[0, 0] * (m[1, 2] * m[2, 3] - m[1, 3] * m[2, 2]) - m[0, 2] * (m[1, 0] * m[2, 3] - m[1, 3] * m[2, 0]) + m[0, 3] * (m[1, 0] * m[2, 2] - m[1, 2] * m[2, 0])) / det;

    Result[2, 0] := (m[1, 0] * (m[2, 1] * m[3, 3] - m[2, 3] * m[3, 1]) - m[1, 1] * (m[2, 0] * m[3, 3] - m[2, 3] * m[3, 0]) + m[1, 3] * (m[2, 0] * m[3, 1] - m[2, 1] * m[3, 0])) / det;
    Result[2, 1] := (-m[0, 0] * (m[2, 1] * m[3, 3] - m[2, 3] * m[3, 1]) + m[0, 1] * (m[2, 0] * m[3, 3] - m[2, 3] * m[3, 0]) - m[0, 3] * (m[2, 0] * m[3, 1] - m[2, 1] * m[3, 0])) / det;
    Result[2, 2] := (m[0, 0] * (m[1, 1] * m[3, 3] - m[1, 3] * m[3, 1]) - m[0, 1] * (m[1, 0] * m[3, 3] - m[1, 3] * m[3, 0]) + m[0, 3] * (m[1, 0] * m[3, 1] - m[1, 1] * m[3, 0])) / det;
    Result[2, 3] := (-m[0, 0] * (m[1, 1] * m[2, 3] - m[1, 3] * m[2, 1]) + m[0, 1] * (m[1, 0] * m[2, 3] - m[1, 3] * m[2, 0]) - m[0, 3] * (m[1, 0] * m[2, 1] - m[1, 1] * m[2, 0])) / det;

    Result[3, 0] := (-m[1, 0] * (m[2, 1] * m[3, 2] - m[2, 2] * m[3, 1]) + m[1, 1] * (m[2, 0] * m[3, 2] - m[2, 2] * m[3, 0]) - m[1, 2] * (m[2, 0] * m[3, 1] - m[2, 1] * m[3, 0])) / det;
    Result[3, 1] := (m[0, 0] * (m[2, 1] * m[3, 2] - m[2, 2] * m[3, 1]) - m[0, 1] * (m[2, 0] * m[3, 2] - m[2, 2] * m[3, 0]) + m[0, 2] * (m[2, 0] * m[3, 1] - m[2, 1] * m[3, 0])) / det;
    Result[3, 2] := (-m[0, 0] * (m[1, 1] * m[3, 2] - m[1, 2] * m[3, 1]) + m[0, 1] * (m[1, 0] * m[3, 2] - m[1, 2] * m[3, 0]) - m[0, 2] * (m[1, 0] * m[3, 1] - m[1, 1] * m[3, 0])) / det;
    Result[3, 3] := (m[0, 0] * (m[1, 1] * m[2, 2] - m[1, 2] * m[2, 1]) - m[0, 1] * (m[1, 0] * m[2, 2] - m[1, 2] * m[2, 0]) + m[0, 2] * (m[1, 0] * m[2, 1] - m[1, 1] * m[2, 0])) / det;

  End;
End;

Procedure GaussJordan(Var M: TMatrixNxM);
(*
 * x,y Zeile, s scalar
 * y := y + s*x
 *)
  Procedure AddScaled(x: integer; s: TBaseType; y: integer);
  Var
    i: Integer;
  Begin
    For i := 0 To high(M) Do Begin
      m[i, y] := m[i, y] + s * m[i, x];
    End;
  End;
Var
  steps, x, y, i: integer;
Begin
  (*
   * Die Matrix kann nur im Linken Oberen Quadratischen Teil gelöst werden
   *)
  steps := min(high(m), high(m[0]));
  For x := 0 To steps Do Begin
    If m[x, x] = 0 Then Begin // Wir Würden durch 0 Teilen =>  Wir Addieren eine der noch nicht Benutzten Zeilen
      For y := x + 1 To high(m[x]) Do Begin
        If m[x, y] <> 0 Then Begin
          AddScaled(y, 1, x);
          break;
        End;
      End;
    End;
    If m[x, x] = 0 Then Begin // So finden wir keine Lösung !
      // Wir machen die gesamte Matrix zu einer Nullmatrix, so merkt der Aufrufer dass es geknallt hat.
      For i := 0 To high(m) Do Begin
        For y := 0 To high(m[i]) Do Begin
          m[i, y] := 0;
        End;
      End;
      exit;
    End;
    // Teilen der Zeile durch m[x,x] => 0 .. 1 .. *
    For i := x + 1 To high(m) Do Begin
      m[i, x] := m[i, x] / m[x, x];
    End;
    m[x, x] := 1;
    // Die Spalte x auser in m[x,x] zu 0 machen
    For y := 0 To high(m[0]) Do Begin
      If (y <> x) And (m[x, y] <> 0) Then Begin // Wo schon eine 0 ist, braucht auch keine Mehr erzeugt werden.
        AddScaled(x, -m[x, y], y);
      End;
    End;
  End;
End;

Function TransposeVector(Const V: TVectorN): TMatrixNxM;
Var
  i: Integer;
Begin
  result := Nil;
  setlength(result, length(v), 1);
  For i := 0 To high(v) Do Begin
    result[i, 0] := v[i];
  End;
End;

Function TransposeMatrix(Const M: TMatrix3x3): TMatrix3x3;
Var
  i, j: Integer;
Begin
  For i := 0 To 2 Do
    For j := 0 To 2 Do
      result[i, j] := M[j, i];
End;

Function TransposeMatrix(Const M: TMatrix4x4): TMatrix4x4;
Var
  i, j: Integer;
Begin
  For i := 0 To 3 Do
    For j := 0 To 3 Do
      result[i, j] := M[j, i];
End;

Function TransposeMatrix(Const M: TMatrixNxM): TMatrixNxM;
Var
  i, j: Integer;
Begin
  result := Nil;
  setlength(result, length(m[0]), length(m));
  For i := 0 To high(m[0]) Do
    For j := 0 To high(m) Do
      result[i, j] := M[j, i];
End;

Function ScaleNxM(Const s: TBaseType; Const M: TMatrixNxM): TMatrixNxM;
Var
  i, j: Integer;
Begin
  result := Nil;
  setlength(result, length(m), length(m[0]));
  For i := 0 To high(m) Do Begin
    For j := 0 To high(m[0]) Do Begin
      result[i, j] := m[i, j] * s;
    End;
  End;
End;

Function MulVectorMatrix(Const V: TVector2; Const M: TMatrix2x2): TVector2;
Begin
  Result.x := m[0, 0] * v.x + m[0, 1] * v.y;
  Result.y := m[1, 0] * v.x + m[1, 1] * v.y;
End;

Function MulVectorMatrix(Const V: TVector3; Const M: TMatrix3x3): TVector3;
Begin
  Result.x := m[0, 0] * v.x + m[0, 1] * v.y + m[0, 2] * v.z;
  Result.y := m[1, 0] * v.x + m[1, 1] * v.y + m[1, 2] * v.z;
  Result.z := m[2, 0] * v.x + m[2, 1] * v.y + m[2, 2] * v.z;
End;

Function MulVectorMatrix(Const V: TVector4; Const M: TMatrix4x4): TVector4;
Begin
  result.x := m[0, 0] * v.x + m[1, 0] * v.y + m[2, 0] * v.z + m[3, 0] * v.w;
  result.y := m[0, 1] * v.x + m[1, 1] * v.y + m[2, 1] * v.z + m[3, 1] * v.w;
  result.z := m[0, 2] * v.x + m[1, 2] * v.y + m[2, 2] * v.z + m[3, 2] * v.w;
  result.w := m[0, 3] * v.x + m[1, 3] * v.y + m[2, 3] * v.z + m[3, 3] * v.w;
End;

Function MulVectorMatrix(Const V: TVectorN; Const M: TMatrixNxM): TVectorN;
Var
  i, j: integer;
Begin
  result := Nil;
  If (high(V) <> high(m)) Or (high(m) = -1) Then Begin
    Raise Exception.Create('uvectormath.MulVectorMatrix: invalid dimensions.');
  End;
  setlength(result, length(m[0]));
  For j := 0 To high(result) Do Begin
    result[j] := 0;
    For i := 0 To high(m) Do Begin
      result[j] := result[j] + v[i] * m[i, j];
    End;
  End;
End;

Function MulMatrix(Const M1, M2: TMatrix2x2): TMatrix2x2;
Var
  i, j, k: Integer;
  sum: TBaseType;
Begin
  // Durchführen der "Einfachen" Matrizenmultiplikation in O(n^3)
  For i := 0 To high(m2) Do
    For j := 0 To high(m1[0]) Do Begin
      // Dot procuct of values in Col
      sum := 0;
      For k := 0 To high(M1) Do Begin
        sum := sum + m1[k, j] * m2[i, k];
      End;
      result[i, j] := sum;
    End;
End;

Function MulMatrix(Const M1, M2: TMatrix3x3): TMatrix3x3;
Var
  i, j, k: Integer;
  sum: TBaseType;
Begin
  // Durchführen der "Einfachen" Matrizenmultiplikation in O(n^3)
  For i := 0 To high(m2) Do
    For j := 0 To high(m1[0]) Do Begin
      // Dot procuct of values in Col
      sum := 0;
      For k := 0 To high(M1) Do Begin
        sum := sum + m1[k, j] * m2[i, k];
      End;
      result[i, j] := sum;
    End;
End;

Function MulMatrix(Const M1, M2: TMatrix4x4): TMatrix4x4;
Var
  i, j, k: Integer;
  sum: TBaseType;
Begin
  // Durchführen der "Einfachen" Matrizenmultiplikation in O(n^3)
  For i := 0 To high(m2) Do
    For j := 0 To high(m1[0]) Do Begin
      // Dot procuct of values in Col
      sum := 0;
      For k := 0 To high(M1) Do Begin
        sum := sum + m1[k, j] * m2[i, k];
      End;
      result[i, j] := sum;
    End;
End;

Function MulMatrix(Const M1, M2: TMatrixNxM): TMatrixNxM;
Var
  i, j, k: Integer;
  sum: TBaseType;
Begin
  result := Nil;
  // Durchführen der "Einfachen" Matrizenmultiplikation in O(n^3)
  If length(m1) <> length(m2[0]) Then Begin
    Raise Exception.Create('uvectormath.MulMatrix: invalid dimension');
  End;
  setlength(result, length(m2), length(m1[0]));
  For i := 0 To high(m2) Do
    For j := 0 To high(m1[0]) Do Begin
      // Dot procuct of values in Col
      sum := 0;
      For k := 0 To high(M1) Do Begin
        sum := sum + m1[k, j] * m2[i, k];
      End;
      result[i, j] := sum;
    End;
End;

Procedure MapMatrix(Var M: TMatrixNxM; MapFunction: TMapFunction); // Wendet die Funktion Mapfunktion auf alle Elemente einer Matrix an
Var
  i, j: integer;
Begin
  For j := 0 To high(m) Do
    For i := 0 To high(m[0]) Do Begin
      M[j, i] := MapFunction(m[j, i]);
    End;
End;

Function MapMatrix2(Const M: TMatrixNxM; MapFunction: TMapFunction): TMatrixNxM; // Wendet die Funktion Mapfunktion auf alle Elemente einer Matrix an
Var
  i, j: integer;
Begin
  result := Nil;
  setlength(result, length(m), length(m[0]));
  For j := 0 To high(m) Do
    For i := 0 To high(m[0]) Do Begin
      result[j, i] := MapFunction(m[j, i]);
    End;
End;

Function CalculateRotationMatrix(Angle: TBaseType): TMatrix2x2;
Var
  s, c: float;
Begin
  sincos(degtorad(Angle), s, c);
  result[0, 0] := c;
  result[1, 0] := -s;
  result[0, 1] := s;
  result[1, 1] := c;
End;

Function CalculateRotationMatrix(Angle: TBaseType; RotationVector: TVector3
  ): TMatrix4x4;
Var
  OneSubC, s, c: float;
Begin
  RotationVector := NormV3(RotationVector);
  If lenV3(RotationVector) = 0 Then
    Raise exception.Create('uvectormath.CalculateRotationMatrix: Error invalid "RotationVector".');
  If Angle <> 0 Then Begin
    sincos(degtorad(Angle), s, c);
    OneSubC := 1 - c;
    result[0, 0] := RotationVector.x * RotationVector.x * OneSubC + c;
    result[1, 0] := RotationVector.x * RotationVector.y * OneSubC - RotationVector.z * s;
    result[2, 0] := RotationVector.x * RotationVector.z * OneSubC + RotationVector.y * s;
    result[3, 0] := 0;
    result[0, 1] := RotationVector.y * RotationVector.x * OneSubC + RotationVector.z * s;
    result[1, 1] := RotationVector.y * RotationVector.y * OneSubC + c;
    result[2, 1] := RotationVector.y * RotationVector.z * OneSubC - RotationVector.x * s;
    result[3, 1] := 0;
    result[0, 2] := RotationVector.z * RotationVector.x * OneSubC - RotationVector.y * s;
    result[1, 2] := RotationVector.z * RotationVector.y * OneSubC + RotationVector.x * s;
    result[2, 2] := RotationVector.z * RotationVector.z * OneSubC + c;
    result[3, 2] := 0;
    result[0, 3] := 0;
    result[1, 3] := 0;
    result[2, 3] := 0;
    result[3, 3] := 1;
  End
  Else Begin
    result := IdentityMatrix4x4;
  End;
End;

Function PointArrIsConvexPolygon(Const Points: TVector2Array): Boolean;
Const
  pi2 = pi * 2;

Var
  i, ip1, im1: Integer;
  ai: Double;
  a, b: TVector2;
  mode: Shortint; // 1 = alle Winkel müssen >= 180 sein, -1 alle Winkel müssen <= 180 sein
Begin
  // Wenn es nur 2 oder Weniger Punkte
  If High(points) < 2 Then Begin
    result := false;
  End
    // Ein Dreieck
  Else If High(points) = 2 Then Begin
    result := true;
  End
    // Ein Polygon
  Else Begin
    result := true;
    mode := 0;
    For i := 0 To high(Points) Do Begin
      ip1 := (i + 1) Mod (high(points) + 1);
      im1 := (i + high(points)) Mod (high(points) + 1);
      // Ermitteln des Winkels zwischen den Punkten im1, i, ip1
      a := SubV2(points[im1], points[i]);
      b := SubV2(points[ip1], points[i]);
      ai := arctan2(b.y, b.x) - arctan2(a.y, a.x);
      While ai > pi2 Do
        ai := ai - pi2;
      While ai < 0 Do
        ai := ai + pi2;
      // Ermitteln des Drehsinnes des Polygones
      If mode = 0 Then Begin
        If ai <> pi Then Begin
          If ai > pi Then
            mode := 1
          Else
            Mode := -1;
        End;
      End
      Else Begin
        // überprüfen, ob die Winkel dem Drehsinn entsprechen
        If (Mode = 1) And (ai < pi) Then Begin
          result := false;
          exit;
        End;
        If (Mode = -1) And (ai > pi) Then Begin
          result := false;
          exit;
        End;
      End;
    End;
  End;
End;

(* Implemenentiert ist der Jarvis' Wrap Algorithmus *)

Function PointsToConvexHull(Const Points: TVector2Array): TVector2Array;
// less_xy_2(p, q)
//   <=> p is lexicographically smaller than q
  Function Less_xy_2(p, q: TVector2): Boolean;
  Begin
    result := (p.x < q.x) Or ((p.x = q.x) And (p.y < q.y));
  End;

  // rightturn_2(p, q, r)
  //   <=> p is right of all directed lines from q to r
  Function rightturn_2(p, q, r: TVector2): Boolean;
  Begin
    result := (q.x - p.x) * (r.y - p.y) < (q.y - p.y) * (r.x - p.x);
  End;

Var
  q_next, q_now, p_start, h, i: Integer;
Begin
  result := Nil;
  setlength(result, high(Points) + 1);
  If High(result) > 2 Then Begin
    // find point with smallest x-coordinate
    p_start := 0;
    For i := 1 To high(Points) Do Begin
      If Less_xy_2(Points[i], Points[p_start]) Then Begin
        p_start := i;
      End;
    End;
    // set q_next to some OTHER point
    q_next := 0; // Kill the Compiler warning
    For i := 0 To high(Points) Do
      If i <> p_start Then Begin
        q_next := i;
        break;
      End;
    // wrapping
    h := 0;
    q_now := p_start;
    Repeat
      result[h] := Points[q_now];
      inc(h);
      For i := 0 To high(Points) Do
        If rightturn_2(Points[q_now], Points[q_next], Points[i]) Then
          q_next := i;
      q_now := q_next;
      q_next := p_start;
    Until (p_start = q_now);
    // Beschränken der Ausgabe auf die Hin sortierte Convexe Hülle
    setlength(result, h);
  End
  Else Begin
    // Kopieren wenn wir weniger als 4 Punkte haben
    For i := 0 To high(Points) Do
      result[i] := points[i];
  End;
End;

//Credit to Paul Bourke (pbourke@swin.edu.au) for the original Fortran 77 Program :))
//Conversion to Visual Basic by EluZioN (EluZioN@casesladder.com)
//Conversion from VB to Delphi6 by Dr Steve Evans (steve@lociuk.com)
//Conversion from Delphi6 to FreePascal by Corpsman (corpsman@corpsman.de)
///////////////////////////////////////////////////////////////////////////////
//June 2002 Update by Dr Steve Evans (steve@lociuk.com): Heap memory allocation
//added to prevent stack overflow when MaxVertices and MaxTriangles are very large.
//Additional Updates in June 2002:
//Bug in InCircle function fixed. Radius r := Sqrt(rsqr).
//Check for duplicate points added when inserting new point.
//For speed, all points pre-sorted in x direction using quicksort algorithm and
//triangles flagged when no longer needed. The circumcircle centre and radius of
//the triangles are now stored to improve calculation time.
///////////////////////////////////////////////////////////////////////////////
//October 2012 Update by Corpsman (corpsman@corpsman.de): Added dynamical
//Arrays. Bug Fixed in calculating the outer triangle position where to small
//Added more comments in the code
///////////////////////////////////////////////////////////////////////////////
//You can use this code however you like providing the above credits remain in tact

Function PointsToDelaunayTriangleList(Const Points: TVector2Array;
  CheckForDoublePoints: Boolean): TTriangleArray;

Const
  BlockSize = 1000; // Allocating Memory in Blocks, keep allocating overhead small, and gives dynamic allocation

Type

  TInternal = Record
    x, y: TBaseType; // The coords of the original points
    oldindex: integer; // pointer to the original points
  End;

  TInternalArray = Array Of TInternal; // Container for internal storage

  //Created Triangles, vv# are the vertex pointers
  dTriangle = Record
    vv0: Integer; // Index, of the 1. point in triangle, counterclockwise
    va: Integer; // Index, of the 2. point in triangle, counterclockwise
    vv2: Integer; // Index, of the 3. point in triangle, counterclockwise
    PreCalc: boolean; // True if xy, yc, r are defined
    xc, yc, r: TBaseType; // Center and radius of the circumcircle
    Complete: Boolean; // If True, then all calculations of this triangle are finished (triangle will never be changed again)
  End;

Var
  Vertex: TInternalArray; // copy of the input with pointer to the original index
  Triangle: Array Of dTriangle; // All created triangles (will be the result of the function)

  Procedure Quicksort(li, re: integer); //Sort all points by x
  Var
    h: TInternal;
    l, r: Integer;
    p: TBaseType;
  Begin
    If Li < Re Then Begin
      p := Vertex[Trunc((li + re) / 2)].x; // read pivot
      l := Li;
      r := re;
      While l < r Do Begin
        While Vertex[l].x < p Do
          inc(l);
        While Vertex[r].x > p Do
          dec(r);
        If L <= R Then Begin
          h := Vertex[l];
          Vertex[l] := Vertex[r];
          Vertex[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      Quicksort(li, r);
      Quicksort(l, re);
    End;
  End;

  Function InCircle(xp, yp: Tbasetype; // Point to insert
    Out xc: TBaseType; Out yc: TBaseType; Out r: TBaseType;
    j: Integer // Pointer to triangle
    ): Boolean;
    //Return TRUE if the point (xp,yp) lies inside the circumcircle
    //made up by triangle[j]
    //The circumcircle centre is returned in (xc,yc) and the radius r
    //NOTE: A point on the edge is inside the circumcircle
  Var
    dx: TBaseType;
    dy: TBaseType;
    rsqr: TBaseType;
    drsqr: TBaseType;
    Circle: TCircle;
  Begin
    //Check if xc,yc and r have already been calculated
    If Triangle[j].PreCalc Then Begin
      xc := Triangle[j].xc;
      yc := Triangle[j].yc;
      r := Triangle[j].r;
      rsqr := r * r;
      dx := xp - xc;
      dy := yp - yc;
      drsqr := dx * dx + dy * dy;
    End
    Else Begin
      circle := PointsToCircumCircle(
        v2(Vertex[Triangle[j].vv0].x, Vertex[Triangle[j].vv0].y),
        v2(Vertex[Triangle[j].va].x, Vertex[Triangle[j].va].y),
        v2(Vertex[Triangle[j].vv2].x, Vertex[Triangle[j].vv2].y)
        );
      circle.radius := abs(circle.radius);
      Triangle[j].PreCalc := true;
      Triangle[j].xc := circle.Center.x;
      Triangle[j].yc := circle.Center.y;
      Triangle[j].r := circle.radius;
      xc := circle.Center.x;
      yc := circle.Center.y;
      r := circle.radius;
      rsqr := circle.radius * circle.radius;
      dx := xp - circle.Center.x;
      dy := yp - circle.Center.y;
      drsqr := dx * dx + dy * dy;
    End;
    result := drsqr <= rsqr;
  End;

Var
  i, j, k: Integer; // Loop Counters
  maxx, maxy, minx, miny: TBaseType; // To Calculate die Points boundingbox
  dmax: TBaseType; // The Max Dimension in the boundingbox
  ymid, xmid: TBaseType; // The Center of the boundingbox
  NTri: Integer; // Counter for all triangles
  NEdge: Integer; // Counter for all edges
  NVert: integer; // Counter for all Vertices
  inc: Boolean; // True if new point lies within actual triangle and triangle needs partitioning
  xc, yc, r: TBaseType; // Return Parameters from InCircle calculating the Actual Triangle
  Edges: Array[0..1] Of Array Of integer; // All Edges
Begin
  // to less points
  If high(Points) < 2 Then Begin
    result := Nil;
    exit;
  End;
  // trivial solution
  If high(Points) = 2 Then Begin
    SetLength(result, 1);
    result[0].a := 0;
    result[0].b := 1;
    result[0].c := 2;
    exit;
  End;
  // more then 3 points, need calculations
  Vertex := Nil;
  setlength(Vertex, high(Points) + 2 + 3); // first index is unused(because the double edge detection will use index 0 to detect invalid edges), + 3 for supertriangle
  // Calculate bounding box
  maxx := Points[0].x;
  maxy := Points[0].y;
  minx := Points[0].x;
  miny := Points[0].y;
  nvert := high(points) + 1;
  For i := 0 To high(points) Do Begin
    Vertex[i + 1].x := Points[i].x;
    Vertex[i + 1].y := Points[i].y;
    Vertex[i + 1].oldindex := i;
    maxx := max(maxx, Points[i].x);
    maxy := max(maxy, Points[i].y);
    minx := min(minx, Points[i].x);
    miny := min(miny, Points[0].y);
    (*
     * double points are not allowed in point set.
     * if you are shure, that there where only different points you can set CheckForDoublePoints to false
     *)
    If CheckForDoublePoints Then Begin
      For j := i + 1 To high(points) Do Begin
        If (abs(points[i].x - points[j].x) < Epsilon) And
          (abs(points[i].y - points[j].y) < Epsilon) Then Begin
          Raise exception.create(format('uvectormath.PointsToDelaunayTriangleList: Error, point %d and %d are the same.', [i, j]));
        End;
      End;
    End;
  End;
  // Sorting points by x will decrease insertion time.
  QuickSort(1, nvert);
  // The Outer Triangle has to be far away, otherwise there could be some seldom
  // cases in which the convex hul is not calculated correct.
  // Unfortunatulely, if you choose "20" to large (e.g. 100) there come some other errors (in the circumcircle routine)
  dmax := max(maxx - minx, maxy - miny) * 20;
  xmid := (maxx + minx) / 2;
  ymid := (maxy + miny) / 2;
  Vertex[nvert + 1].oldindex := -1;
  Vertex[nvert + 1].x := (xmid - 2 * dmax);
  Vertex[nvert + 1].y := (ymid - dmax);
  Vertex[nvert + 2].oldindex := -1;
  Vertex[nvert + 2].x := xmid;
  Vertex[nvert + 2].y := (ymid + 2 * dmax);
  Vertex[nvert + 3].oldindex := -1;
  Vertex[nvert + 3].x := (xmid + 2 * dmax);
  Vertex[nvert + 3].y := (ymid - dmax);
  // Allocating first blocksize
  Triangle := Nil;
  setlength(Triangle, BlockSize);
  setlength(Edges[0], BlockSize);
  setlength(Edges[1], BlockSize);
  // inserting the supertriangle
  Triangle[1].vv0 := nvert + 1;
  Triangle[1].va := nvert + 2;
  Triangle[1].vv2 := nvert + 3;
  Triangle[1].PreCalc := false;
  Triangle[1].Complete := false;
  ntri := 1;
  // Insert all Points one by one
  For i := 1 To nvert Do Begin
    Nedge := 0;
    //Set up the edge buffer.
    //If the point (Vertex(i).x,Vertex(i).y) lies inside the circumcircle then the
    //three edges of that triangle are added to the edge buffer.
    j := 0;
    Repeat
      j := j + 1;
      If Triangle[j].Complete <> true Then Begin // only check incomplete triangles
        inc := InCircle(
          Vertex[i].x, Vertex[i].y, // Point to be inserted
          xc, yc, r, // return the circumcircle information
          j // Pointer to the triangle, makes
          );
        //Include this if points are sorted by X
        If (xc + r) < Vertex[i].x Then Begin //
          Triangle[j].Complete := True; //
        End //
        Else Begin //
          If inc Then Begin // if Triangle needs partitioning, insert edges
            // Realocate memory if necessery
            If Nedge + 3 > high(Edges[0]) Then Begin
              setlength(Edges[0], high(Edges[0]) + 1 + BlockSize);
              setlength(Edges[1], high(Edges[1]) + 1 + BlockSize);
            End;
            Edges[0, Nedge + 1] := Triangle[j].vv0;
            Edges[1, Nedge + 1] := Triangle[j].va;
            Edges[0, Nedge + 2] := Triangle[j].va;
            Edges[1, Nedge + 2] := Triangle[j].vv2;
            Edges[0, Nedge + 3] := Triangle[j].vv2;
            Edges[1, Nedge + 3] := Triangle[j].vv0;
            Nedge := Nedge + 3;
            // Duplicate the triangle, but why ??
            Triangle[j].vv0 := Triangle[ntri].vv0;
            Triangle[j].va := Triangle[ntri].va;
            Triangle[j].vv2 := Triangle[ntri].vv2;
            Triangle[j].PreCalc := Triangle[ntri].PreCalc;
            Triangle[j].xc := Triangle[ntri].xc;
            Triangle[j].yc := Triangle[ntri].yc;
            Triangle[j].r := Triangle[ntri].r;
            Triangle[ntri].PreCalc := false;
            Triangle[j].Complete := Triangle[ntri].Complete;
            j := j - 1;
            ntri := ntri - 1;
          End;
        End;
      End;
    Until j >= ntri;

    // Tag multiple edges
    // Note: if all triangles are specified anticlockwise then all
    // interior edges are opposite pointing in direction.
    For j := 1 To Nedge - 1 Do Begin
      If Not (Edges[0, j] = 0) And Not (Edges[1, j] = 0) Then Begin
        For k := j + 1 To Nedge Do Begin
          If Not (Edges[0, k] = 0) And Not (Edges[1, k] = 0) Then Begin
            If Edges[0, j] = Edges[1, k] Then Begin
              If Edges[1, j] = Edges[0, k] Then Begin
                Edges[0, j] := 0;
                Edges[1, j] := 0;
                Edges[0, k] := 0;
                Edges[1, k] := 0;
              End;
            End;
          End;
        End;
      End;
    End;

    //  Form new triangles for the current point
    //  Skipping over any tagged edges.
    //  All edges are arranged in clockwise order.
    For j := 1 To Nedge Do Begin
      If Not (Edges[0, j] = 0) And Not (Edges[1, j] = 0) Then Begin
        ntri := ntri + 1;
        // Realocate memory if necessery
        If high(Triangle) < ntri Then Begin
          setlength(Triangle, high(Triangle) + 1 + BlockSize);
        End;
        Triangle[ntri].vv0 := Edges[0, j];
        Triangle[ntri].va := Edges[1, j];
        Triangle[ntri].vv2 := i;
        Triangle[ntri].PreCalc := false;
        Triangle[ntri].Complete := False;
      End;
    End;
  End;

  //Remove triangles with supertriangle vertices
  //These are triangles which have a vertex number greater than NVERT
  i := 0;
  Repeat
    i := i + 1;
    If (Triangle[i].vv0 > nvert) Or (Triangle[i].va > nvert) Or (Triangle[i].vv2 > nvert) Then Begin
      Triangle[i].vv0 := Triangle[ntri].vv0;
      Triangle[i].va := Triangle[ntri].va;
      Triangle[i].vv2 := Triangle[ntri].vv2;
      i := i - 1;
      ntri := ntri - 1;
    End;
  Until i >= ntri;
  // Convert all results to output format, using the "unsorted" versions of the points
  setlength(result, ntri);
  For i := 1 To ntri Do Begin
    result[i - 1].a := Vertex[Triangle[i].vv0].oldindex;
    result[i - 1].b := Vertex[Triangle[i].va].oldindex;
    result[i - 1].c := Vertex[Triangle[i].vv2].oldindex;
  End;
  // Free all variables
  setlength(vertex, 0);
  setlength(Triangle, 0);
  setlength(Edges[0], 0);
  setlength(Edges[1], 0);
End;

// Berechnet aus den 3 Punkten a,b,c einen Kreis, so dass die 3 Punkte auf dessen Kreisbahn liegen
// Formel entnommen aus
// Numerical Recipes 3. Editon §21.3

Function PointsToCircumCircle(Const A, B, C: Tvector2): TCircle;
Var
  a0, a1, c0, c1, det, asq, csq, ctr0, ctr1, rad2: TBaseType;
Begin
  a0 := a.x - b.x;
  a1 := a.y - b.y;
  c0 := c.x - b.x;
  c1 := c.y - b.y;
  det := a0 * c1 - c0 * a1;
  // Wenn alle 3 Punkte auf einer Geraden liegen gibt es eigentlich keinen CircumCircle
  If det = 0 Then Begin
    // Nun Tun wir einfach so als gäbe es den Circumcirle doch
    result.Center := ScaleV2(1 / 3, AddV2(a, AddV2(b, c)));
    Result.radius := -LenV2(SubV2(b, result.Center)); // Durch das - Zeigen wir an, dass es eigentlich nicht ging
    exit;
  End;
  det := 0.5 / det;
  asq := sqr(a0) + sqr(a1);
  csq := sqr(c0) + sqr(c1);
  ctr0 := det * (asq * c1 - csq * a1);
  ctr1 := det * (csq * a0 - asq * c0);
  rad2 := sqr(ctr0) + sqr(ctr1);
  result.Center := v2(ctr0 + b.x, ctr1 + b.y);
  result.radius := sqrt(rad2);
End;

// Result = 0.5 * det( b-a, c-a);

Function CalculateTriangleArea(Const A, B, C: TVector2): TBaseType;
Var
  d1, d2: TVector2;
Begin
  d1 := SubV2(b, a);
  d2 := SubV2(c, a);
  result := 0.5 * (d1.x * d2.y - d2.x * d1.y);
End;

// Find the area of a polygon using a method based on Green's theorem.
//
// The algebraic sign of the area is positive for counterclockwise
// ordering of vertices in the X-Y plane, and negative for
// clockwise ordering.
//
// Reference:  "Centroid of a Polygon" in Graphics Gems IV,
// by Gerard Bashein and Paul R. Detmer,
// Paul S. Heckbert (editor), Academic Press, 1994, pp. 3-6.

Function CalculatePolygonArea(Const Points: TVector2Array): TBaseType;
Var
  i: Integer;
  sum: TBaseType;
Begin
  If High(Points) < 2 Then Begin
    result := 0;
    exit;
  End;
  sum := 0;
  For i := 0 To High(Points) Do Begin
    sum := sum + Points[i].X * Points[(i + 1) Mod (high(Points) + 1)].Y -
      Points[(i + 1) Mod (high(Points) + 1)].X * Points[i].Y
  End;
  result := 0.5 * abs(sum);
End;

Function CalculateOrthoMatrix(EyePos, Dir, Up: TVector3): TMatrix4x4;
Var
  r, u: TVector3;
  TranslationMatrix: TMatrix4x4;
  i, j: Integer;
Begin
  // Erzeugen der Translationsmatrix
  For i := 0 To 3 Do
    For j := 0 To 3 Do
      If i = j Then
        TranslationMatrix[i, j] := 1
      Else
        TranslationMatrix[i, j] := 0;
  TranslationMatrix[3, 0] := -EyePos.x;
  TranslationMatrix[3, 1] := -EyePos.y;
  TranslationMatrix[3, 2] := -EyePos.z;
  // Berechnen der Basisvektoren
  r := CrossV3(dir, up);
  u := CrossV3(r, dir);
  // Normieren der Basisvektoren
  r := NormV3(r);
  u := NormV3(u);
  dir := Normv3(dir);
  // Erzeugen der Basiswechsel Matrix
  result[0, 0] := r.x;
  result[0, 1] := r.y;
  result[0, 2] := r.z;
  result[0, 3] := 0;
  result[1, 0] := u.x;
  result[1, 1] := u.y;
  result[1, 2] := u.z;
  result[1, 3] := 0;
  result[2, 0] := dir.x;
  result[2, 1] := dir.y;
  result[2, 2] := dir.z;
  result[2, 3] := 0;
  result[3, 0] := 0;
  result[3, 1] := 0;
  result[3, 2] := 0;
  result[3, 3] := 1;
  result := TransposeMatrix(result);
  result := MulMatrix(result, TranslationMatrix);
End;

Function CalculateNormal(A, B, C, Inner: Tvector3): Tvector3;
Var
  erg, d1, d2: Tvector3;
Begin
  d1 := subv3(A, B); // Berechnen der Richtungsvektoren der Ebene
  d2 := subv3(A, C); // Berechnen der Richtungsvektoren der Ebene
  erg := CrossV3(d1, d2); // Berechnen eines Senkrecht auf der Ebene Stehenden Vektors
  erg := normV3(erg); // Normieren des Vektors
  // Schaun ob unser Normalenvektor auf den Halbraum zeigt in dem unser Punktinnen liegt
  d1 := subv3(A, Inner);
  If DotV3(erg, d1) < 0 Then // Wenn dann mus er umgedreht werden
    erg := v3(-erg.x, -erg.y, -erg.z);
  result := erg;
End;

Procedure GenerateShadowMatrix(Out ShadowMatrix: TMatrix4x4; Const Normal,
  Point: TVector3; Light: TVector4);
Var
  d, dot: Real;
Begin
  d := -normal.X * point.X - normal.Y * point.Y - normal.Z * point.Z;
  dot := normal.X * light.X + normal.Y * light.Y + normal.Z * light.Z + d * light.W;
  ShadowMatrix[0, 0] := -light.X * normal.X + dot;
  ShadowMatrix[0, 1] := -light.Y * normal.X;
  ShadowMatrix[0, 2] := -light.Z * normal.X;
  ShadowMatrix[0, 3] := -light.W * normal.X;
  ShadowMatrix[1, 0] := -light.X * normal.Y;
  ShadowMatrix[1, 1] := -light.Y * normal.Y + dot;
  ShadowMatrix[1, 2] := -light.Z * normal.Y;
  ShadowMatrix[1, 3] := -light.W * normal.Y;
  ShadowMatrix[2, 0] := -light.X * normal.Z;
  ShadowMatrix[2, 1] := -light.Y * normal.Z;
  ShadowMatrix[2, 2] := -light.Z * normal.Z + dot;
  ShadowMatrix[2, 3] := -light.W * normal.Z;
  ShadowMatrix[3, 0] := -light.X * d;
  ShadowMatrix[3, 1] := -light.Y * d;
  ShadowMatrix[3, 2] := -light.Z * d;
  ShadowMatrix[3, 3] := -light.W * d + dot;
End;

Function ArcTangens(Const X, Y: Extended): Extended;
Begin
  // TODO: Das müsste eigentlich mittels atan2 gemacht werden
  Result := 0;
  If (x = 0) Then Begin
    If (Y >= 0) Then result := 90;
    If (Y < 0) Then result := 270;
  End
  Else Begin
    result := radtodeg(arctan(Y / X));
    If ((X < 0) And (y > 0)) Or ((x < 0) And (Y <= 0)) Then result := 180 + result;
    If (X > 0) And (Y < 0) Then result := 360 + result;
  End;
End;

Function PointInTriangle(Const P: TVector2; A, B, C: TVector2): Boolean;
Var
  tmp1, tmp2, M: TVector2;
  pinnen, innen: TBaseType;
Begin
  // Der Schwerpunkt liegt immer im Dreick
  m := ScaleV2(1 / 3, AddV2(a, AddV2(b, c)));
  // Strecke A,B mit P
  tmp1 := subv2(m, a);
  tmp1 := v2(tmp1.y, -tmp1.x);
  tmp2 := subv2(b, a);
  innen := DotV2(tmp1, tmp2);
  tmp1 := subv2(p, a);
  tmp1 := v2(tmp1.y, -tmp1.x);
  Pinnen := DotV2(tmp1, tmp2);
  result := (sign(innen) = sign(Pinnen));
  // Strecke B,C mit P
  If result Then Begin
    tmp1 := subv2(m, b);
    tmp1 := v2(tmp1.y, -tmp1.x);
    tmp2 := subv2(c, b);
    innen := DotV2(tmp1, tmp2);
    tmp1 := subv2(p, b);
    tmp1 := v2(tmp1.y, -tmp1.x);
    Pinnen := DotV2(tmp1, tmp2);
    result := (sign(innen) = sign(Pinnen));
  End;
  // Strecke C,A mit P
  If result Then Begin
    tmp1 := subv2(m, c);
    tmp1 := v2(tmp1.y, -tmp1.x);
    tmp2 := subv2(a, c);
    innen := DotV2(tmp1, tmp2);
    tmp1 := subv2(p, c);
    tmp1 := v2(tmp1.y, -tmp1.x);
    Pinnen := DotV2(tmp1, tmp2);
    result := (sign(innen) = sign(Pinnen));
  End;
End;

Function PointInRect(Const P, TL, BR: TVector2): Boolean;
Begin
  result :=
    (p.x <= max(tl.x, br.x)) And
    (p.x >= min(tl.x, br.x)) And
    (p.y <= max(tl.y, br.y)) And
    (p.y >= min(tl.y, br.y));
End;

Function RectIntersectRect(Const TL1, BR1, TL2, BR2: TVector2): Boolean;
Begin
  result :=
    PointInRect(tl1, tl2, br2) Or
    PointInRect(BR1, tl2, br2) Or
    PointInRect(v2(tl1.x, br1.y), tl2, br2) Or
    PointInRect(v2(br1.x, tl1.y), tl2, br2) Or
    (*
* Diese Fälle Hier Treten nur auf, wenn alle Punkte des Rechtecks 1 auserhalb des Ersten liegen
* Dann muss das Rechteck 2 mindestens mit 2 Punkten Innerhalb des 1 Liegen (wäre es nur einer, dann
* wäre wieder ein Fall von oben gekommen.
* Den Test auf "Enthalten" sein kann man also mit nur 2 Prüfungen machen. Wichtig die beiden
* Prüfpunkte müssen sich diagonal gegenüberliegen !
*)
  PointInRect(tl2, tl1, br1) Or
    PointInRect(BR2, tl1, br1)
    //    Or PointInRect(v2(tl2.x, br2.y), tl1, br1) // Diese Beiden Fälle kann man sich sparen
//    Or PointInRect(v2(br2.x, tl2.y), tl1, br1) // Diese Beiden Fälle kann man sich sparen
  ;
End;

Function PointInCube(Const P, TL, BR: Tvector3): Boolean;
Begin
  result :=
    (p.x <= max(tl.x, br.x)) And
    (p.x >= min(tl.x, br.x)) And
    (p.y <= max(tl.y, br.y)) And
    (p.y >= min(tl.y, br.y)) And
    (p.z <= max(tl.z, br.z)) And
    (p.z >= min(tl.z, br.z));
End;

Function PointInPolygon(Const p: TVector2; Const Polygon: TVector2Array
  ): Boolean;
// Quelle : https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html
Var
  i, j: integer;
Begin
  result := false;
  j := high(Polygon);
  For i := 0 To high(Polygon) Do Begin
    If (((polygon[i].y > p.y) <> (polygon[j].y > p.y)) And
      (p.x < (polygon[j].x - Polygon[i].x) * (p.y - Polygon[i].y) / (Polygon[j].y - Polygon[i].y) + Polygon[i].x)) Then Begin
      result := Not result;
    End;
    j := i;
  End;
End;

Function PointInOOBB(Const P, P1, P2, P3, P4: TVector2): Boolean;
Var
  dif2, dif1: TVector2;
Begin
  result := False;
  dif1 := SubV2(p1, p);
  dif2 := SubV2(p1, p2);
  If DotV2(dif1, dif2) < 0 Then
    exit;
  dif1 := SubV2(p2, p);
  dif2 := SubV2(p2, p3);
  If DotV2(dif1, dif2) < 0 Then
    exit;
  dif1 := SubV2(p3, p);
  dif2 := SubV2(p3, p4);
  If DotV2(dif1, dif2) < 0 Then
    exit;
  dif1 := SubV2(p4, p);
  dif2 := SubV2(p4, p1);
  If DotV2(dif1, dif2) < 0 Then
    exit;
  result := True;
End;

Function IntersectLine_segments(Const A, B, C, D: TVector2; Out P: TVector2
  ): Boolean;
Var
  r, s, dett, det, a00, a01, a10, a11, a20, a21: TBaseType;
Begin
  result := false;
  a00 := b.x - a.x;
  a01 := b.y - a.y;
  a10 := -(d.x - c.x);
  a11 := -(d.y - c.y);
  a20 := c.x - a.x;
  a21 := c.y - a.y;
  det := a00 * a11 - a01 * a10;
  If det <> 0 Then Begin
    dett := a20 * a11 - a21 * a10;
    r := dett / det;
    dett := a00 * a21 - a01 * a20;
    s := dett / det;
    result := (r >= 0) And (r <= 1) And (s >= 0) And (s <= 1);
    If Result Then Begin
      p := a + r * (b - a);
    End;
  End
  Else Begin
    result := false;
  End;
End;

Function IntersectLines(Const A, m_A, B, m_B: TVector2; Out P: TVector2
  ): Boolean;
Var
  r, dett, det, a00, a01, a10, a11, a20, a21: TBaseType;
Begin
  a00 := m_a.x;
  a01 := m_a.y;
  a10 := -(m_B.x);
  a11 := -(m_B.y);
  det := a00 * a11 - a01 * a10;
  If det <> 0 Then Begin
    a20 := b.x - a.x;
    a21 := b.y - a.y;
    dett := a20 * a11 - a21 * a10;
    r := dett / det;
    p := a + r * m_A;
    result := true;
  End
  Else Begin
    result := false;
  End;
End;

Function CalculatePlumbFootPoint(Const P, P1, P2: TVector2): TVector2;
(*
 * In 2D kann man tatsächlich die Gleichung einfach aufstellen und direkt
 * auflösen.
 *
 * Die Idee:
 * - P3 = beweglicher Punkt auf der Geraden P1 - P2
 * - Der Vektor P - P3 muss im Rechten Winkel zum Richtungsvektor der Geraden stehen
 * => Das Scalarprodukt dieser beiden ist also 0
 *)
Var
  p3: TVector2;
  s, nominator: Single;
Begin
  // Nebenbedingung
  p3 := p2 - p1;
  nominator := LenV2SQR(p3);
  If nominator = 0 Then Begin
    Raise exception.create('uvectormath.CalculatePlumbFootPoint: p1 = p2');
    exit;
  End;
  s := (-p3.x * (p2.x - p.x) - p3.y * (p2.y - p.y)) / nominator;
  result := p3 * s + p2;
End;

Function IntersectLineEllipse(Const A, B, M: TVector2; Rx, Ry: TBaseType; Out
  P1, P2: TVector2): integer;
Var
  x, det, d, h, g, f, b2, v, m_g, c_g: TBaseType;
Begin
  p1 := ZeroV2; // Kill the Compiler warning
  p2 := ZeroV2; // Kill the Compiler warning
  If abs(a.x - b.x) <= Epsilon Then Begin // Schnitt mit einer Senkrechten
    result := 0;
    If (a.x < m.x - rx) Or (a.x > m.x + rx) Then Begin // Die Gerade liegt außerhalb der Ellipse
      result := 0;
    End
    Else Begin
      If abs(a.x - (m.x - rx)) < Epsilon Then Begin // Tangente Links
        result := 1;
        p1 := v2(m.x - rx, m.y);
      End
      Else Begin
        If abs(a.x - (m.x + rx)) < Epsilon Then Begin // Tankente Rechts
          result := 1;
          p1 := v2(m.x + rx, m.y);
        End
        Else Begin // Schnitt mitten durch
          v := sqrt((1 - sqr(a.x - m.x) / sqr(rx)) * sqr(ry));
          result := 2;
          p1 := v2(a.x, v + m.y);
          p2 := v2(a.x, -v + m.y);
        End;
      End;
    End;
  End
  Else Begin
    // Schnitt mit einer normalen Geraden (Nicht Senkrecht)
    m_g := (b.y - a.y) / (b.x - a.x); // Umrechen 2-Punkteform in y = m_g*x+c_g
    c_g := a.y - m_g * a.x; // Umrechen 2-Punkteform in y = m_g*x+c_g
    // Berechnen diverser Hilfsgrößen
    b2 := sqr(ry);
    v := b2 / sqr(rx);
    d := c_g - m.y;
    h := sqr(m_g) + v;
    g := 2 * (m_g * d - m.x * v);
    f := v * sqr(m.x) + sqr(d) - b2;
    det := sqr(g) - 4 * h * f;
    If det < 0 Then Begin // keine Schnittpunkte
      result := 0;
      p1 := ZeroV2;
      p2 := ZeroV2;
    End
    Else Begin
      If det <= Epsilon Then Begin // Ein Schnittpunkt
        result := 1;
        det := sqrt(det);
        x := (-g + det) / (2 * h);
        p1 := v2(x, m_g * x + c_g);
        p2 := ZeroV2;
      End
      Else Begin // Zwei Schnittpunkte
        result := 2;
        det := sqrt(det);
        x := (-g + det) / (2 * h);
        p1 := v2(x, m_g * x + c_g);
        x := (-g - det) / (2 * h);
        p2 := v2(x, m_g * x + c_g);
      End;
    End;
  End;
  // Es hat Schnittpunkte gegeben, nun muss geprüft werden ob sie innerhalb des Streckenabschnittes liegen.
  Case result Of
    1: Begin
        // Prüfen ob P1 auf dem Streckenstück liegt.
        If Not ((min(a.x, b.x) <= p1.x) And (max(a.x, b.x) >= p1.x) And
          (min(a.y, b.y) <= p1.y) And (max(a.y, b.y) >= p1.y)) Then Begin
          result := 0;
          p1 := ZeroV2;
        End;
      End;
    2: Begin
        // Liegt P2 auf der Strecke ?
        If Not ((min(a.x, b.x) <= p2.x) And (max(a.x, b.x) >= p2.x) And
          (min(a.y, b.y) <= p2.y) And (max(a.y, b.y) >= p2.y)) Then Begin
          result := 1;
          p2 := ZeroV2;
        End;
        // Liegt P1 auf der Strecke ?
        If Not ((min(a.x, b.x) <= p1.x) And (max(a.x, b.x) >= p1.x) And
          (min(a.y, b.y) <= p1.y) And (max(a.y, b.y) >= p1.y)) Then Begin
          result := result - 1;
          p1 := p2;
          p2 := ZeroV2;
        End;
      End;
  End;
End;

Function IntersectOOBB(Const A, B, C, D, AA, BB, CC, DD: TVector2): Boolean;
Begin
  result := PointInOOBB(a, aa, bb, cc, dd) Or
    PointInOOBB(b, aa, bb, cc, dd) Or
    PointInOOBB(c, aa, bb, cc, dd) Or
    PointInOOBB(d, aa, bb, cc, dd) Or
    PointInOOBB(AA, a, b, c, d) Or
    PointInOOBB(BB, a, b, c, d) Or
    PointInOOBB(CC, a, b, c, d) Or
    PointInOOBB(DD, a, b, c, d);
End;

Function InertialTensor(v: TVector3; m: TBaseType): TMatrix3x3;
Begin
  Result := M3x3(
    -m * (-sqr(v.z) - sqr(v.y)), -m * (v.y * v.x), -m * (v.z * v.x),
    -m * (v.y * v.x), -m * (-sqr(v.z) - sqr(v.x)), -m * (v.z * v.y),
    -m * (v.z * v.x), -m * (v.z * v.y), -m * (-sqr(v.y) - sqr(v.x)));
End;

Function LeastSquares(Points: TVector2Array; Grade: integer): TVectorN;
  Function intpow(x: TBaseType; y: integer): TBaseType; // result := x^y (y
  Var
    i: Integer;
  Begin
    result := 1;
    For i := 0 To y - 1 Do Begin
      result := result * x;
    End;
  End;

Var
  tmp, c, a_t, A: TMatrixNxM;
  b, bb: TVectorN;
  y, x: Integer;
Begin
  a := Nil;
  b := Nil;
  tmp := Nil;
  setlength(A, Grade + 1, length(Points));
  setlength(b, length(Points));
  // Das Lineare Gleichungssystem Initialisieren
  For y := 0 To high(Points) Do Begin
    b[y] := Points[y].y;
    For x := 0 To Grade Do Begin
      A[x, y] := intpow(Points[y].x, x);
    End;
  End;
  // Nun gilt A*p=b
  a_t := TransposeMatrix(a);
  // Nun wird die Gleichung mit der Transponierten von A durchmultipliziert
  c := a_t * a;
  bb := a_t * b;
  // Als Ergebnis erhalten wir c * p = bb, dabei ist c zu einer Quadratischen Matrix geworden
  // Das Lineare Gleichungssystem kann nun einfach berechnet werden.
  setlength(tmp, Grade + 2, Grade + 1);
  For x := 0 To high(tmp) Do Begin
    For y := 0 To high(tmp[x]) Do Begin
      If x <> high(tmp) Then Begin
        tmp[x, y] := c[x, y];
      End
      Else Begin
        tmp[x, y] := bb[y];
      End;
    End;
  End;
  GaussJordan(tmp);
  result := Nil;
  setlength(result, Grade + 1);
  For y := 0 To Grade Do Begin
    result[y] := tmp[high(tmp), y];
  End;
End;

Function CalculatePolynom(x: TBaseType; Const a: TVectorN): TBaseType;
Var
  i: integer;
  xx: TBaseType;
Begin
  If Not assigned(a) Then Begin
    result := 0;
    exit;
  End;
  result := a[0];
  xx := x;
  For i := 1 To high(a) Do Begin
    result := result + xx * a[i];
    xx := xx * x;
  End;
End;

Function CalculateOrthoganlProjection(A, B, M: TVector2): TVector2;
Var
  d_AB, d_M: TVector2;
Begin
  d_AB := B - A;
  d_M := v2(d_ab.y, -d_AB.x);
  If Not IntersectLines(a, d_AB, m, d_M, result) Then Begin
    result := a;
  End;
End;

Function CalculatePointLineDistance(S, R, P: TVector2): TBaseType;
Var
  lp: TVector2;
Begin
  lp := CalculateOrthoganlProjection(s, s + r, p);
  If Equal(lp, s) Then Begin
    result := 0;
  End
  Else Begin
    result := LenV2(lp - p);
  End;
End;

Function Print(Const V: Tvector2): String;
Begin
  result := format('(%.2f %.2f)', [v.x, v.y]);
End;

Function Print(Const V: Tvector3): String;
Begin
  result := format('(%.2f %.2f %.2f)', [v.x, v.y, v.z]);
End;

Function Print(Const V: Tvector4): String;
Begin
  result := format('(%.2f %.2f %.2f %.2f)', [v.x, v.y, v.z, v.w]);
End;

Function Print(Const V: TVectorN): String;
Var
  i: integer;
Begin
  result := '(';
  For i := 0 To high(v) Do Begin
    result := result + floattostr(v[i]);
    If i <> high(v) Then Begin
      result := result + ' ';
    End;
  End;
  result := result + ')';
End;

Function Print(Const M: TMatrix2x2): String;
Var
  i, j: integer;
Begin
  result := '';
  For j := 0 To 1 Do Begin
    result := result + '(';
    For i := 0 To 1 Do Begin
      result := result + format('%.2f', [m[i, j]]);
      If i <> 1 Then
        result := result + ' '
      Else
        result := result + ')'
    End;
    If j <> 1 Then result := result + LineEnding;
  End;
End;

Function Print(Const M: TMatrix3x3): String;
Var
  i, j: integer;
Begin
  result := '';
  For j := 0 To 2 Do Begin
    result := result + '(';
    For i := 0 To 2 Do Begin
      result := result + format('%.2f', [m[i, j]]);
      If i <> 2 Then
        result := result + ' '
      Else
        result := result + ')'
    End;
    If j <> 2 Then result := result + LineEnding;
  End;
End;

Function Print(Const M: TMatrix4x4): String;
Var
  i, j: integer;
Begin
  result := '';
  For j := 0 To 3 Do Begin
    result := result + '(';
    For i := 0 To 3 Do Begin
      result := result + format('%.2f', [m[i, j]]);
      If i <> 3 Then
        result := result + ' '
      Else
        result := result + ')'
    End;
    If j <> 3 Then result := result + LineEnding;
  End;
End;

Function Print(Const M: TMatrixNxM): String;
Var
  i, j: integer;
Begin
  result := '';
  For j := 0 To high(m[0]) Do Begin
    result := result + '(';
    For i := 0 To high(m) Do Begin
      result := result + format('%.2f', [m[i, j]]);
      If i <> high(m) Then
        result := result + ' '
      Else
        result := result + ')'
    End;
    If j <> high(m[0]) Then result := result + LineEnding;
  End;
End;

{ TVector2helper }

Function TVector2helper.Equal(Other: TVector2): Boolean;
Begin
  result := (abs(self.x - Other.x) < Epsilon) And (abs(self.y - Other.y) < Epsilon);
End;

Function TVector2helper.Cross(Other: TVector2): TVector2;
Begin
  result := CrossV2(self, other);
End;

Function TVector2helper.Hadamard(Other: TVector2): TVector2;
Begin
  result := HadamardV2(self, other);
End;

Function TVector2helper.Dot(Other: TVector2): TBaseType;
Begin
  result := DotV2(self, Other);
End;

{ TVector3helper }

Function TVector3helper.Equal(Other: TVector3): Boolean;
Begin
  result := (abs(self.x - Other.x) < Epsilon) And (abs(self.y - Other.y) < Epsilon) And (abs(self.z - Other.z) < Epsilon);
End;

Function TVector3helper.Cross(Other: TVector3): TVector3;
Begin
  result := CrossV3(Self, Other);
End;

Function TVector3helper.Hadamard(Other: TVector3): TVector3;
Begin
  result := HadamardV3(self, Other);
End;

Function TVector3helper.Dot(Other: TVector3): TBaseType;
Begin
  result := DotV3(self, Other);
End;

{ TVector4helper }

Function TVector4helper.Equal(Other: TVector4): Boolean;
Begin
  result := (abs(self.x - Other.x) < Epsilon) And (abs(self.y - Other.y) < Epsilon) And (abs(self.z - Other.z) < Epsilon) And (abs(self.w - Other.w) < Epsilon);
End;

Function TVector4helper.Hadamard(Other: TVector4): TVector4;
Begin
  result := HadamardV4(self, other);
End;

Function TVector4helper.Dot(Other: TVector4): TBaseType;
Begin
  result := DotV4(self, Other);
End;

{ TVectorNhelper }

Function TVectorNhelper.Equal(Other: TVectorN): Boolean;
Var
  i: Integer;
Begin
  result := false;
  If length(self) <> length(Other) Then exit;
  result := true;
  For i := 0 To high(self) Do Begin
    If abs(self[i] - other[i]) >= Epsilon Then Begin
      result := false;
      exit;
    End;
  End;
End;

Function TVectorNhelper.Hadamard(Other: TVectorN): TVectorN;
Begin
  result := HadamardVN(self, Other);
End;

Function TVectorNhelper.Transpose(): TMatrixNxM;
Begin
  result := TransposeVector(self);
End;

{ TMatrix2x2helper }

Function TMatrix2x2helper.Equal(Other: TMatrix2x2): Boolean;
Var
  i, j: Integer;
Begin
  result := true;
  For i := 0 To 1 Do Begin
    For j := 0 To 1 Do Begin
      If abs(self[i, j] - other[i, j]) > Epsilon Then Begin
        result := false;
        exit;
      End;
    End;
  End;
End;

Function TMatrix2x2helper.getInverse(): TMatrix2x2;
Begin
  result := InvertMatrix2(self);
End;

{ TMatrix3x3helper }

Function TMatrix3x3helper.Equal(Other: TMatrix3x3): Boolean;
Var
  i, j: Integer;
Begin
  result := true;
  For i := 0 To 2 Do Begin
    For j := 0 To 2 Do Begin
      If abs(self[i, j] - other[i, j]) > Epsilon Then Begin
        result := false;
        exit;
      End;
    End;
  End;
End;

Function TMatrix3x3helper.getInverse(): TMatrix3x3;
Begin
  result := InvertMatrix2(self);
End;

{ TMatrix4x4helper }

Function TMatrix4x4helper.getRaw(index: integer): TBaseType;
Type
  PBaseType = ^TBaseType;
Var
  p: PBaseType;
Begin
  p := @self[0, 0];
  inc(p, index);
  result := p^;
  // result := Self[index Div 4, index Mod 4];
End;

Procedure TMatrix4x4helper.setRaw(index: integer; AValue: TBaseType);
Type
  PBaseType = ^TBaseType;
Var
  p: PBaseType;
Begin
  p := @self[0, 0];
  inc(p, index);
  p^ := AValue;
  // Self[index Div 4, index Mod 4] := AValue;
End;

Function TMatrix4x4helper.Equal(Other: TMatrix4x4): Boolean;
Var
  i, j: Integer;
Begin
  result := true;
  For i := 0 To 3 Do Begin
    For j := 0 To 3 Do Begin
      If abs(self[i, j] - other[i, j]) > Epsilon Then Begin
        result := false;
        exit;
      End;
    End;
  End;
End;

Function TMatrix4x4helper.getInverse(): TMatrix4x4;
Begin
  result := InvertMatrix2(self);
End;

{ TMatrixNxMhelper }

Function TMatrixNxMhelper.Equal(Other: TMatrixNxM): Boolean;
Var
  i, j: Integer;
Begin
  result := false;
  If (length(self) <> length(Other)) Or
    (length(self[0]) <> length(Other[0])) Then Begin
    exit;
  End;
  result := true;
  For i := 0 To high(self) Do Begin
    For j := 0 To high(self[0]) Do Begin
      If abs(self[i, j] - Other[i, j]) > Epsilon Then Begin
        result := false;
        exit;
      End;
    End;
  End;
End;

Function TMatrixNxMhelper.Hadamard(Other: TMatrixNxM): TMatrixNxM;
Begin
  result := HadamardNxM(self, Other);
End;

Function TMatrixNxMhelper.Transpose(): TMatrixNxM;
Begin
  result := TransposeMatrix(self);
End;

Function TMatrixNxMhelper.MapMatrix(MapFunction: TMapFunction): TMatrixNxM;
Begin
  result := MapMatrix2(self, MapFunction);
End;

End.

