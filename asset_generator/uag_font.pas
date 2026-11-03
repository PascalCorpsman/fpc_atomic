(******************************************************************************)
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
(*
 * Vector stroke font of the asset generator (own design, rounded geometric
 * capitals). Every glyph is a set of center lines (straight lines and
 * quadratic curves) on a grid with cap height 10, they are drawn with the
 * round pen of TAGCanvas.Stroke. Lower case letters are drawn as small caps.
 * Supported: A-Z, a-z, 0-9, space and . , : ; ! ? - + ' " / ( ) & # * = _ < >
 * and the German umlauts / sharp s (UTF-8 or Latin-1).
 *
 * Coordinates: (x, y) is the top left corner of the capitals, Size is the
 * cap height in pixel. Weight is the pen width relative to Size.
 *
 *   DrawText(C, 10, 10, 20, 'Hello', AGWhite);
 *   DrawTextStyled(C, Ctx.Style, 320, 40, 48, 'VICTORY', AGHex($F0C030), AGBlack, faCenter);
 *   w := TextWidth('Options', 20);
 *)
Unit uag_font;

{$MODE ObjFPC}{$H+}

Interface

Uses uag_image, uag_canvas, uag_context;

Type
  TAGFontAlign = (faLeft, faCenter, faRight);

Const
  AGFontWeight = 0.16; // default pen width / cap height

(* Width of Text in pixel (from the left edge of the first to the right edge of the last pen stroke) *)
Function TextWidth(Const Text: String; Size: Double; Weight: Double = AGFontWeight): Double;

(* Plain text in one color *)
Procedure DrawText(C: TAGCanvas; x, y, Size: Double; Const Text: String; Color: TAGColor;
  Align: TAGFontAlign = faLeft; Weight: Double = AGFontWeight; Slant: Double = 0);

(*
 * Title text in the look of the style: an outline of OutlineW pixel around
 * the letters, flat = solid fill, soft = darker fill with a light highlight
 * on the upper part, pixel = solid fill (the sheet renderer pixelates it).
 * Shadow > 0 adds a drop shadow offset by Shadow pixel.
 *)
Procedure DrawTextStyled(C: TAGCanvas; Style: TAGStyle; x, y, Size: Double; Const Text: String;
  Base, Outline: TAGColor; Align: TAGFontAlign = faLeft; Weight: Double = AGFontWeight;
  OutlineW: Double = -1; Slant: Double = 0; Shadow: Double = 0);

(* Strokes all glyph center lines with a pen of Pen pixel (low level, used by the functions above) *)
Procedure StrokeText(C: TAGCanvas; x, y, Size: Double; Const Text: String; Pen: Double; Color: TAGColor;
  Weight: Double; Slant: Double; dx: Double = 0; dy: Double = 0);

Implementation

Uses uag_math;

Type
  TGlyph = Record
    Defined: Boolean;
    Width: Double; // in units (cap height = 10)
    Strokes: Array Of TAGPoints; // flattened center lines in units
  End;

Var
  Glyphs: Array[0..255] Of TGlyph;
  Accents: Array[0..1] Of TAGPoints; // two dots for umlauts

Const
  SmallCapsH = 0.74;
  SmallCapsW = 0.84;
  QuadSegments = 8;

  (* Number parser without locale and without StrToFloat *)
Function ParseNum(Const s: String): Double;
Var
  i: Integer;
  neg, frac: Boolean;
  v, d: Double;
Begin
  v := 0;
  d := 1;
  neg := false;
  frac := false;
  For i := 1 To length(s) Do Begin
    Case s[i] Of
      '-': neg := true;
      '.': frac := true;
      '0'..'9': Begin
          If frac Then Begin
            d := d * 10;
            v := v + (ord(s[i]) - ord('0')) / d;
          End
          Else
            v := v * 10 + (ord(s[i]) - ord('0'));
        End;
    End;
  End;
  If neg Then v := -v;
  result := v;
End;

Procedure DefGlyph(ch: Char; Width: Double; Const Def: String);
Var
  tokens: Array Of String;
  nums: Array[0..3] Of Double;
  nn, i, k: Integer;
  cmd: Char;
  tok: String;
  cur: TAGPoints;
  last: TAGPoint;
  g: ^TGlyph;

  Procedure FlushStroke;
  Begin
    If length(cur) >= 1 Then Begin
      setlength(g^.Strokes, length(g^.Strokes) + 1);
      g^.Strokes[high(g^.Strokes)] := cur;
    End;
    cur := Nil;
  End;

  Procedure AddPt(x, y: Double);
  Begin
    setlength(cur, length(cur) + 1);
    cur[high(cur)] := AGPoint(x, y);
    last := AGPoint(x, y);
  End;

  Procedure Emit; // M and L
  Begin
    If cmd = 'M' Then FlushStroke;
    AddPt(nums[0], nums[1]);
  End;

  Procedure EmitQuad(p0: TAGPoint);
  Var
    t, mt: Double;
    j: Integer;
  Begin
    For j := 1 To QuadSegments Do Begin
      t := j / QuadSegments;
      mt := 1 - t;
      AddPt(mt * mt * p0.x + 2 * mt * t * nums[0] + t * t * nums[2],
        mt * mt * p0.y + 2 * mt * t * nums[1] + t * t * nums[3]);
    End;
  End;

Begin
  g := @Glyphs[ord(ch)];
  g^.Defined := true;
  g^.Width := Width;
  g^.Strokes := Nil;
  cur := Nil;
  last := AGPoint(0, 0);
  // split into tokens
  tokens := Nil;
  tok := '';
  For i := 1 To length(Def) Do Begin
    If Def[i] = ' ' Then Begin
      If tok <> '' Then Begin
        setlength(tokens, length(tokens) + 1);
        tokens[high(tokens)] := tok;
      End;
      tok := '';
    End
    Else
      tok := tok + Def[i];
  End;
  If tok <> '' Then Begin
    setlength(tokens, length(tokens) + 1);
    tokens[high(tokens)] := tok;
  End;
  cmd := 'M';
  nn := 0;
  For k := 0 To high(tokens) Do Begin
    tok := tokens[k];
    If tok[1] In ['M', 'L', 'Q'] Then Begin
      cmd := tok[1];
      nn := 0;
      delete(tok, 1, 1);
      If tok = '' Then Continue;
    End;
    nums[nn] := ParseNum(tok);
    inc(nn);
    If (cmd = 'Q') And (nn = 4) Then Begin
      EmitQuad(last);
      nn := 0;
    End
    Else If (cmd <> 'Q') And (nn = 2) Then Begin
      Emit;
      nn := 0;
      If cmd = 'M' Then cmd := 'L';
    End;
  End;
  FlushStroke;
End;

Procedure InitGlyphs;
Const
  // shared outlines
  O6 = 'M3 0 Q6 0 6 3 L6 7 Q6 10 3 10 Q0 10 0 7 L0 3 Q0 0 3 0';
  P6 = 'M0 10 L0 0 L3.8 0 Q6 0 6 2.75 Q6 5.5 3.8 5.5 L0 5.5';
  U6 = 'M0 0 L0 7 Q0 10 3 10 Q6 10 6 7 L6 0';
  A6 = 'M0 10 L0 4 Q0 0 3 0 Q6 0 6 4 L6 10 M0 6.2 L6 6.2';
Begin
  DefGlyph(' ', 3.2, '');
  DefGlyph('A', 6, A6);
  DefGlyph('B', 6, 'M0 5 L0 0 L3.6 0 Q5.8 0 5.8 2.5 Q5.8 5 3.6 5 L0 5 L0 10 L3.8 10 Q6 10 6 7.5 Q6 5 3.8 5');
  DefGlyph('C', 6, 'M6 2 Q5.6 0 3 0 Q0 0 0 3 L0 7 Q0 10 3 10 Q5.6 10 6 8');
  DefGlyph('D', 6, 'M0 0 L0 10 L2.6 10 Q6 10 6 6.6 L6 3.4 Q6 0 2.6 0 L0 0');
  DefGlyph('E', 5.4, 'M5.4 0 L0 0 L0 10 L5.4 10 M0 5 L4.2 5');
  DefGlyph('F', 5.4, 'M5.4 0 L0 0 L0 10 M0 5 L4.2 5');
  DefGlyph('G', 6, 'M6 2 Q5.6 0 3 0 Q0 0 0 3 L0 7 Q0 10 3 10 Q6 10 6 7 L6 5.4 L3.4 5.4');
  DefGlyph('H', 6, 'M0 0 L0 10 M6 0 L6 10 M0 5 L6 5');
  DefGlyph('I', 0, 'M0 0 L0 10');
  DefGlyph('J', 5, 'M5 0 L5 7 Q5 10 2.5 10 Q0 10 0 7.6');
  DefGlyph('K', 5.6, 'M0 0 L0 10 M5.6 0 L0.4 5.6 M2.2 3.9 L5.8 10');
  DefGlyph('L', 5, 'M0 0 L0 10 L5 10');
  DefGlyph('M', 7.4, 'M0 10 L0 0 L3.7 6.4 L7.4 0 L7.4 10');
  DefGlyph('N', 6, 'M0 10 L0 0 L6 10 L6 0');
  DefGlyph('O', 6, O6);
  DefGlyph('P', 6, P6);
  DefGlyph('Q', 6, O6 + ' M3.6 7 L6.4 10.4');
  DefGlyph('R', 6, P6 + ' M3 5.5 L6 10');
  DefGlyph('S', 6, 'M5.8 1.6 Q5.2 0 3 0 Q0 0 0 2.6 Q0 4.8 3 5 Q6 5.2 6 7.4 Q6 10 3 10 Q0.6 10 0 8.4');
  DefGlyph('T', 6, 'M0 0 L6 0 M3 0 L3 10');
  DefGlyph('U', 6, U6);
  DefGlyph('V', 6.4, 'M0 0 L3.2 10 L6.4 0');
  DefGlyph('W', 8.4, 'M0 0 L2.1 10 L4.2 2.6 L6.3 10 L8.4 0');
  DefGlyph('X', 6, 'M0 0 L6 10 M6 0 L0 10');
  DefGlyph('Y', 6, 'M0 0 L3 5.2 L6 0 M3 5.2 L3 10');
  DefGlyph('Z', 6, 'M0 0 L6 0 L0 10 L6 10');
  DefGlyph('0', 5.4, 'M2.7 0 Q5.4 0 5.4 3.2 L5.4 6.8 Q5.4 10 2.7 10 Q0 10 0 6.8 L0 3.2 Q0 0 2.7 0 M1.6 6.6 L3.8 3.4');
  DefGlyph('1', 4, 'M0.2 2.2 L2.6 0 L2.6 10 M0.2 10 L4.6 10');
  DefGlyph('2', 5.4, 'M0 2.6 Q0.2 0 2.7 0 Q5.4 0 5.4 2.6 Q5.4 4.6 2.8 6.6 L0 10 L5.4 10');
  DefGlyph('3', 5.4, 'M0 1.4 Q1 0 2.7 0 Q5.4 0 5.4 2.5 Q5.4 4.8 2.4 4.8 Q5.4 4.8 5.4 7.4 Q5.4 10 2.7 10 Q0.8 10 0 8.6');
  DefGlyph('4', 5.6, 'M4.2 10 L4.2 0 L0 7 L5.6 7');
  DefGlyph('5', 5.4, 'M5.2 0 L0.6 0 L0.2 4.4 Q1.2 3.8 2.7 3.8 Q5.4 3.8 5.4 6.9 Q5.4 10 2.7 10 Q0.8 10 0 8.6');
  DefGlyph('6', 5.4, 'M4.8 0.6 Q4 0 2.8 0 Q0 0 0 4 L0 7 Q0 10 2.7 10 Q5.4 10 5.4 7.4 Q5.4 4.8 2.7 4.8 Q0 4.8 0 7');
  DefGlyph('7', 5.4, 'M0 0 L5.4 0 L1.8 10');
  DefGlyph('8', 5.4, 'M2.7 4.8 Q0.2 4.8 0.2 2.4 Q0.2 0 2.7 0 Q5.2 0 5.2 2.4 Q5.2 4.8 2.7 4.8 Q0 4.8 0 7.4 Q0 10 2.7 10 Q5.4 10 5.4 7.4 Q5.4 4.8 2.7 4.8');
  DefGlyph('9', 5.4, 'M0.6 9.4 Q1.4 10 2.6 10 Q5.4 10 5.4 6 L5.4 3 Q5.4 0 2.7 0 Q0 0 0 2.6 Q0 5.2 2.7 5.2 Q5.4 5.2 5.4 3');
  DefGlyph('.', 0, 'M0 9.7 L0 10');
  DefGlyph(',', 0.8, 'M0.8 9.4 L0 11.6');
  DefGlyph(':', 0, 'M0 3.4 L0 3.7 M0 9.7 L0 10');
  DefGlyph(';', 0.8, 'M0.8 3.4 L0.8 3.7 M0.8 9.4 L0 11.6');
  DefGlyph('!', 0, 'M0 0 L0 6.6 M0 9.7 L0 10');
  DefGlyph('?', 5, 'M0 2.4 Q0.2 0 2.5 0 Q5 0 5 2.4 Q5 4.2 2.5 5.2 L2.5 6.6 M2.5 9.7 L2.5 10');
  DefGlyph('-', 3.6, 'M0 5.6 L3.6 5.6');
  DefGlyph('_', 6, 'M0 10.6 L6 10.6');
  DefGlyph('+', 5, 'M0 5.4 L5 5.4 M2.5 2.9 L2.5 7.9');
  DefGlyph('=', 5, 'M0 4.2 L5 4.2 M0 6.8 L5 6.8');
  DefGlyph('*', 4.4, 'M2.2 1 L2.2 5.4 M0.3 2 L4.1 4.4 M0.3 4.4 L4.1 2');
  DefGlyph('''', 0, 'M0 0 L0 2.6');
  DefGlyph('"', 2, 'M0 0 L0 2.6 M2 0 L2 2.6');
  DefGlyph('/', 4.4, 'M0 10.4 L4.4 -0.4');
  DefGlyph('(', 2.4, 'M2.4 -0.4 Q0 2 0 5 Q0 8 2.4 10.4');
  DefGlyph(')', 2.4, 'M0 -0.4 Q2.4 2 2.4 5 Q2.4 8 0 10.4');
  DefGlyph('<', 4.6, 'M4.6 2 L0 5.4 L4.6 8.8');
  DefGlyph('>', 4.6, 'M0 2 L4.6 5.4 L0 8.8');
  DefGlyph('#', 6, 'M1.8 0.6 L1 9.6 M5 0.6 L4.2 9.6 M0 3.4 L6 3.4 M0 6.8 L6 6.8');
  DefGlyph('&', 6.4, 'M6.4 10 L1.4 4.6 Q0.4 3.4 0.6 2.2 Q1 0 3 0 Q5 0 5 2 Q5 3.6 2.6 5 Q0 6.4 0 7.8 Q0 10 2.6 10 Q4.6 10 6.2 6.8');
  // umlauts: base letter + two dots (code points as in Latin-1)
  DefGlyph(#196, 6, A6); // Ä
  DefGlyph(#214, 6, O6); // Ö
  DefGlyph(#220, 6, U6); // Ü
  DefGlyph(#223, 5.6, 'M0 10 L0 3 Q0 0 2.8 0 Q5.4 0 5.4 2.4 Q5.4 4.6 2.8 4.8 Q5.6 5 5.6 7.4 Q5.6 10 2.6 10'); // ß
  setlength(Accents[0], 2);
  Accents[0][0] := AGPoint(1.5, -2.4);
  Accents[0][1] := AGPoint(1.5, -2.1);
  setlength(Accents[1], 2);
  Accents[1][0] := AGPoint(4.5, -2.4);
  Accents[1][1] := AGPoint(4.5, -2.1);
End;

Type
  TFontChar = Record
    Code: Integer; // index into Glyphs
    Small: Boolean; // drawn as small caps
    Dots: Boolean; // umlaut dots
  End;
  TFontChars = Array Of TFontChar;

  (* UTF-8 / Latin-1 text -> glyph list *)
Function Decode(Const Text: String): TFontChars;
Var
  i, n, code: Integer;
  ch: TFontChar;
Begin
  result := Nil;
  setlength(result, length(Text));
  n := 0;
  i := 1;
  While i <= length(Text) Do Begin
    code := ord(Text[i]);
    If (code = $C3) And (i < length(Text)) Then Begin
      code := ord(Text[i + 1]) + $40; // C3 xx -> Latin-1 xx + 40
      inc(i);
    End;
    inc(i);
    ch.Small := false;
    ch.Dots := false;
    Case code Of
      ord('a')..ord('z'): Begin
          ch.Small := true;
          code := code - 32;
        End;
      228: Begin // ä
          ch.Small := true;
          code := 196;
        End;
      246: Begin // ö
          ch.Small := true;
          code := 214;
        End;
      252: Begin // ü
          ch.Small := true;
          code := 220;
        End;
    End;
    ch.Dots := (code = 196) Or (code = 214) Or (code = 220);
    If Not Glyphs[code].Defined Then code := ord('?');
    ch.Code := code;
    result[n] := ch;
    inc(n);
  End;
  setlength(result, n);
End;

Function GlyphAdvance(Const ch: TFontChar; Weight: Double): Double;
Var
  w: Double;
Begin
  w := Glyphs[ch.Code].Width;
  If ch.Small Then w := w * SmallCapsW;
  If ch.Code = ord(' ') Then
    result := w
  Else
    result := w + 10 * Weight + 1.3;
End;

Function TextWidth(Const Text: String; Size: Double; Weight: Double): Double;
Var
  chars: TFontChars;
  i: Integer;
  w: Double;
Begin
  chars := Decode(Text);
  w := 0;
  For i := 0 To high(chars) Do
    w := w + GlyphAdvance(chars[i], Weight);
  If length(chars) > 0 Then w := w - 1.3; // no gap after the last letter
  result := w * Size / 10;
End;

Procedure StrokeText(C: TAGCanvas; x, y, Size: Double; Const Text: String; Pen: Double; Color: TAGColor;
  Weight: Double; Slant: Double; dx: Double; dy: Double);
Var
  chars: TFontChars;
  i, s, k: Integer;
  pen0, sc, sx, sy, px, py: Double;
  g: TGlyph;
  pts: TAGPoints;

  Procedure DoStroke(Const p: TAGPoints);
  Var
    j: Integer;
    ux, uy: Double;
  Begin
    C.BeginPath;
    For j := 0 To high(p) Do Begin
      ux := p[j].x * sx;
      uy := 10 - (10 - p[j].y) * sy;
      px := x + dx + (pen0 + ux + Slant * (10 - uy)) * sc;
      py := y + dy + uy * sc;
      If j = 0 Then C.MoveTo(px, py) Else C.LineTo(px, py);
    End;
    C.Stroke(Pen, Color);
  End;

Begin
  chars := Decode(Text);
  sc := Size / 10;
  pen0 := 5 * Weight; // half the pen width, so x is the left edge of the ink
  For i := 0 To high(chars) Do Begin
    g := Glyphs[chars[i].Code];
    If chars[i].Small Then Begin
      sx := SmallCapsW;
      sy := SmallCapsH;
    End
    Else Begin
      sx := 1;
      sy := 1;
    End;
    For s := 0 To high(g.Strokes) Do
      DoStroke(g.Strokes[s]);
    If chars[i].Dots Then
      For k := 0 To 1 Do Begin
        pts := Accents[k];
        DoStroke(pts);
      End;
    pen0 := pen0 + GlyphAdvance(chars[i], Weight);
  End;
End;

Function AlignX(x, Size: Double; Const Text: String; Align: TAGFontAlign; Weight: Double): Double;
Begin
  Case Align Of
    faCenter: result := x - TextWidth(Text, Size, Weight) / 2;
    faRight: result := x - TextWidth(Text, Size, Weight);
  Else
    result := x;
  End;
End;

Procedure DrawText(C: TAGCanvas; x, y, Size: Double; Const Text: String; Color: TAGColor;
  Align: TAGFontAlign; Weight: Double; Slant: Double);
Begin
  x := AlignX(x, Size, Text, Align, Weight);
  StrokeText(C, x, y, Size, Text, Weight * Size, Color, Weight, Slant);
End;

Procedure DrawTextStyled(C: TAGCanvas; Style: TAGStyle; x, y, Size: Double; Const Text: String;
  Base, Outline: TAGColor; Align: TAGFontAlign; Weight: Double; OutlineW: Double; Slant: Double; Shadow: Double);
Var
  pen: Double;
Begin
  x := AlignX(x, Size, Text, Align, Weight);
  pen := Weight * Size;
  If OutlineW < 0 Then OutlineW := AGMax(1.5, pen * 0.28);
  If Style = asPixel Then OutlineW := AGMax(OutlineW, 2.5);
  If Shadow > 0 Then
    StrokeText(C, x, y, Size, Text, pen + 2 * OutlineW, AGWithAlpha(AGBlack, 150), Weight, Slant, Shadow, Shadow);
  StrokeText(C, x, y, Size, Text, pen + 2 * OutlineW, Outline, Weight, Slant);
  Case Style Of
    asSoft: Begin
        StrokeText(C, x, y, Size, Text, pen, AGShade(Base, 0.72), Weight, Slant);
        StrokeText(C, x, y, Size, Text, pen * 0.62, Base, Weight, Slant, -pen * 0.08, -pen * 0.1);
        StrokeText(C, x, y, Size, Text, pen * 0.26, AGShade(Base, 1.55), Weight, Slant, -pen * 0.16, -pen * 0.2);
      End;
  Else
    StrokeText(C, x, y, Size, Text, pen, Base, Weight, Slant);
  End;
End;

Initialization
  InitGlyphs;

End.
