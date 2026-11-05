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
 * Key art of a set: preview.png (192 x 288, the format of the side image of
 * the launcher) with the figure in a player color, a bomb and a piece of one
 * map (ground, bricks, solid blocks). The same function draws the previews of
 * the style picker of the UI, so it has to be fast (one map field and a few
 * shapes).
 *
 * Also the helpers the UI needs to show the sheets like the engine does:
 * the player colors and the recoloring of client/uatomic.pas
 * (LoadColorTabledTexture) and a map scene with placed blocks.
 *)
Unit uag_preview;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Uses uag_image, uag_context, uag_output;

Const
  PreviewW = 192;
  PreviewH = 288;

Type
  TAGPlayerColor = Record
    r, g, b: Integer; // 0..100 like PlayerColors in units/uatomic_common.pas
  End;

Const
  // same order as PlayerColors in units/uatomic_common.pas
  AGPlayerColors: Array[0..9] Of TAGPlayerColor = (
    (r: 100; g: 100; b: 100), // white
    (r: 20; g: 20; b: 20), // black
    (r: 100; g: 0; b: 0), // red
    (r: 0; g: 0; b: 100), // blue
    (r: 0; g: 100; b: 0), // green
    (r: 100; g: 100; b: 0), // yellow
    (r: 0; g: 100; b: 100), // cyan
    (r: 100; g: 0; b: 100), // magenta
    (r: 100; g: 50; b: 0), // orange
    (r: 50; g: 0; b: 100) // purple
    );
  AGHuntedGold: TAGPlayerColor = (r: 100; g: 75; b: 10);

(* Every pixel with g > r and g > b gets the player color, exactly like the engine *)
Procedure RecolorPlayer(Img: TAGImage; Const Col: TAGPlayerColor);

(* The key art, the caller frees it *)
Function RenderPreview(Ctx: TAGContext): TAGImage;

(*
 * Field (640 x 480) with solid blocks in the usual grid and some bricks,
 * Brick and Solid are keyed (fuchsia = transparent). For the UI.
 *)
Function ComposeMapScene(Field, Brick, Solid: TAGImage; Seed: QWord): TAGImage;

(* Draws Src onto Dst, fuchsia pixels and pixels with alpha < 128 are skipped *)
Procedure DrawKeyed(Dst, Src: TAGImage; x, y: Integer);

Implementation

Uses uag_math, uag_random, uag_canvas, uag_render, uag_registry, uag_rig, uag_gen_atomic, uag_gen_items, uag_gen_world;

Const
  // play field of field.png, see uag_gen_world
  FX = 20;
  FY = 66;
  CW = 40;
  CH = 36;

Procedure RecolorPlayer(Img: TAGImage; Const Col: TAGPlayerColor);
Var
  i, r, g, b, n, k, m: Integer;
Begin
  For i := 0 To high(Img.Pixels) Do Begin
    r := Img.Pixels[i].r;
    g := Img.Pixels[i].g;
    b := Img.Pixels[i].b;
    If (g > r) And (g > b) Then Begin
      n := (r + b) Div 2;
      k := g - n;
      r := n + (k * Col.r) Div 100;
      g := n + (k * Col.g) Div 100;
      b := n + (k * Col.b) Div 100;
      m := r;
      If g > m Then m := g;
      If b > m Then m := b;
      If m > 255 Then Begin
        r := (r * 255) Div m;
        g := (g * 255) Div m;
        b := (b * 255) Div m;
      End;
      Img.Pixels[i].r := r;
      Img.Pixels[i].g := g;
      Img.Pixels[i].b := b;
    End;
  End;
End;

Procedure DrawKeyed(Dst, Src: TAGImage; x, y: Integer);
Var
  i, j: Integer;
  c: TAGColor;
Begin
  For j := 0 To Src.Height - 1 Do
    For i := 0 To Src.Width - 1 Do Begin
      c := Src.Pixels[j * Src.Width + i];
      If (c.a < 128) Or AGSameColor(c, AGFuchsia) Then Continue;
      Dst.SetPixel(x + i, y + j, AGWithAlpha(c, 255));
    End;
End;

Function ComposeMapScene(Field, Brick, Solid: TAGImage; Seed: QWord): TAGImage;
Var
  i, j: Integer;
  R: TAGRandom;
Begin
  result := Field.Clone;
  R.Init(Seed);
  For j := 0 To 10 Do
    For i := 0 To 14 Do Begin
      If odd(i) And odd(j) Then
        DrawKeyed(result, Solid, FX + i * CW, FY + j * CH)
      Else If ((i > 1) Or (j > 1)) And ((i < 13) Or (j < 9)) And R.Chance(0.55) Then
        DrawKeyed(result, Brick, FX + i * CW, FY + j * CH);
    End;
End;

Function RenderPreview(Ctx: TAGContext): TAGImage;
Const
  // cell column 4 of the play field starts 24 pixel left of the image
  CropX = FX + 4 * CW + 24;
  CropY = FY + CH; // rows 1 .. 8
  FigScale = 1.75; // the figure is about 59 pixel high (FigureHeight), about 105 here
  BombScale = 1.75;
  GroundY = 272;
  Bayer: Array[0..3] Of Integer = (0, 2, 3, 1); // 2 x 2 ordered dither
Var
  area: TAGArea;
  mapIndex, i, j, k, row, col: Integer;
  field, brick, solid, layer: TAGImage;
  fig: TFigure;
  pose: TPose;
  facing, FigX, BombX, d, lx, ly: Double;
  info: TAGStyleInfo;

  Procedure DrawLayer(C: TAGCanvas; Frame: Integer);
  Begin
    // contact shadows on the ground: soft = smooth gradient, else hard
    If Ctx.Style = asSoft Then Begin
      C.Ellipse(FigX, GroundY, 36, 10);
      C.FillPaint(C.Radial(FigX, GroundY, 36, [AGRGBA(0, 0, 0, 150), AGRGBA(0, 0, 0, 80), AGRGBA(0, 0, 0, 0)]));
      C.Ellipse(BombX, GroundY + 1, 26, 8);
      C.FillPaint(C.Radial(BombX, GroundY + 1, 26, [AGRGBA(0, 0, 0, 150), AGRGBA(0, 0, 0, 70), AGRGBA(0, 0, 0, 0)]));
    End
    Else Begin
      C.Ellipse(FigX, GroundY, 28, 7);
      C.Fill(AGRGBA(0, 0, 0, 100));
      C.Ellipse(BombX, GroundY + 1, 19, 5);
      C.Fill(AGRGBA(0, 0, 0, 100));
    End;
    // bomb, the bottom of the ball (19.5, 35) of its cell is on the ground
    C.Push;
    C.Translate(BombX, GroundY);
    C.Scale(BombScale, BombScale);
    C.Translate(-19.5, -35);
    DrawBombPreview(Ctx, C);
    C.Pop;
    // figure
    C.Push;
    C.Translate(FigX, GroundY);
    C.Scale(FigScale, FigScale);
    DrawFigure(C, Ctx.Style, fig, pose, facing, 0, 0);
    C.Pop;
  End;

Begin
  area := Ctx.Area('preview');
  mapIndex := area.Pick('map', MapCount);
  info := GetStyleInfo(Ctx.Style);

  // ground and blocks
  field := RenderMapField(Ctx, mapIndex);
  Try
    result := field.Crop(CropX, CropY, PreviewW, PreviewH);
  Finally
    field.Free;
  End;
  brick := RenderMapBrick(Ctx, mapIndex);
  solid := RenderMapSolid(Ctx, mapIndex);
  Try
    brick.KeyTransparent;
    solid.KeyTransparent;
    For j := 0 To 7 Do
      For i := 0 To 5 Do Begin
        col := 4 + i; // column of the play field
        row := 1 + j;
        If odd(col) And odd(row) Then
          DrawKeyed(result, solid, i * CW - 24, j * CH)
        Else If (j < 4) And (area.Rnd('brick', j * 8 + i) < 0.5 - 0.1 * j) Then
          DrawKeyed(result, brick, i * CW - 24, j * CH);
      End;
  Finally
    brick.Free;
    solid.Free;
  End;

  (*
   * Figure in red with a bomb, the pose tells the style apart at a glance:
   *   flat  = jump with a wave, three quarter view
   *   soft  = relaxed, head tilted, hands folded, soft light and shadow
   *   pixel = walk frame to the right like a console sprite
   *   retro = front view, fists on the hips (hero pose)
   *)
  fig := MakeFigure(Ctx.Area('atomic'), false);
  If IsCoarse(Ctx.Style) Then fig.OutlineScale := fig.OutlineScale * 0.6; // like uag_gen_atomic
  If Ctx.Style = asRetro Then fig.OutlineScale := 0;
  pose := NeutralPose;
  FigX := 120;
  BombX := 44;
  Case Ctx.Style Of
    asSoft: Begin
        facing := area.Vary('facing', 0.45, 0.2);
        For k := 0 To 1 Do Begin
          pose.Arm[k].Swing := 0.55;
          pose.Arm[k].Spread := 0.15;
          pose.Arm[k].Bend := 1.0;
          pose.Arm[k].BendSpread := -0.55;
        End;
        pose.HeadRoll := -0.2;
        pose.HeadNod := 0.1;
        pose.Roll := 0.04;
        pose.Leg[0].Swing := 0.08;
        pose.Leg[1].Swing := -0.05;
        pose.LookS := -0.5;
      End;
    asPixel: Begin
        facing := 0.85; // the three quarter view of the side rows of the sheets
        pose.Leg[0].Swing := 0.55;
        pose.Leg[0].Bend := 0.25;
        pose.Leg[1].Swing := -0.45;
        pose.Leg[1].Bend := 0.35;
        pose.Arm[0].Swing := -0.6;
        pose.Arm[1].Swing := 0.6;
        pose.Arm[0].Bend := 0.5;
        pose.Arm[1].Bend := 0.6;
        pose.Lean := 0.12;
        FigX := 124;
      End;
    asRetro: Begin
        facing := FacingDown;
        For k := 0 To 1 Do Begin
          pose.Arm[k].Swing := -0.2;
          pose.Arm[k].Spread := 0.95;
          pose.Arm[k].Bend := 1.9;
          pose.Arm[k].BendSpread := -1.5;
          pose.Leg[k].Spread := 0.22;
        End;
        pose.HeadNod := -0.08;
        pose.Lean := -0.06;
      End;
  Else Begin // flat
      facing := area.Vary('facing', 0.35, 0.25);
      pose.Arm[0].Spread := 2.2;
      pose.Arm[0].Swing := 0.2;
      pose.Arm[0].Bend := 0.05;
      pose.Arm[0].BendSpread := 0.6;
      pose.Arm[1].Swing := 0.25;
      pose.Arm[1].Bend := 0.5;
      pose.Leg[0].Swing := 0.7;
      pose.Leg[0].Bend := 1.3;
      pose.Leg[1].Swing := 0.1;
      pose.Leg[1].Bend := 0.5;
      pose.RootU := 7 * fig.Scale;
      pose.HeadRoll := 0.12;
      pose.Roll := -0.05;
    End;
  End;
  layer := RenderImage(Ctx, PreviewW, PreviewH, @DrawLayer);
  Try
    If info.Palette Then fig.Col.Outline := AGRetroColor(fig.Col.Outline);
    If info.PixelScale > 1 Then PixelOutline(layer, PreviewW, PreviewH, info.PixelScale, fig.Col.Outline, Ctx.Style = asPixel);
    RecolorPlayer(layer, AGPlayerColors[2]);
    result.Draw(layer, 0, 0);
  Finally
    layer.Free;
  End;

  // lighting: darker towards the top, so the figure stands out
  Case Ctx.Style Of
    asSoft: Begin
        // warm light from the top left, smooth falloff to a dark corner bottom right
        For j := 0 To PreviewH - 1 Do
          For i := 0 To PreviewW - 1 Do Begin
            lx := (i - 40) / PreviewW;
            ly := (j - 30) / PreviewH;
            d := AGSqrt(lx * lx + ly * ly);
            k := AGRound(AGClamp(40 - 60 * d, 0, 40));
            If k > 0 Then result.BlendPixel(i, j, AGRGBA(255, 236, 200, 255), k);
            k := AGRound(AGClamp(110 * (d - 0.55), 0, 90));
            If k > 0 Then result.BlendPixel(i, j, AGRGBA(20, 14, 30, 255), k);
          End;
      End;
    asPixel: Begin
        // 4 hard steps with an ordered dither between them, 2 pixel blocks
        For j := 0 To PreviewH Div 3 - 1 Do Begin
          d := 4 * (1 - j / (PreviewH / 3)); // 4 .. 0
          For i := 0 To PreviewW - 1 Do Begin
            k := trunc(d);
            If (d - k) * 4 > Bayer[((j Div 2) Mod 2) * 2 + (i Div 2) Mod 2] Then inc(k);
            If k > 0 Then result.BlendPixel(i, j, AGRGBA(8, 6, 16, 255), 20 * k);
          End;
        End;
      End;
  Else
    For j := 0 To PreviewH Div 3 - 1 Do Begin
      k := AGRound(70 * (1 - j / (PreviewH / 3)));
      If Ctx.Style = asRetro Then k := 18 * ((k + 9) Div 18); // hard bands
      For i := 0 To PreviewW - 1 Do
        result.BlendPixel(i, j, AGRGBA(8, 6, 16, 255), k);
    End;
  End;
  If info.Palette Then AGRetroQuantize(result);
End;

Procedure GeneratePreview(Ctx: TAGContext; Output: TAGOutput);
Var
  img: TAGImage;
Begin
  img := RenderPreview(Ctx);
  Try
    Output.SaveImage('preview.png', img, smOpaque);
  Finally
    img.Free;
  End;
End;

Initialization
  RegisterGenerator('preview', 'Key art for the launcher (preview.png)', 1, @GeneratePreview);

End.
