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
 * Skeleton (rig) of the atomic figure.
 *
 * The figure is a small 3D skeleton (hip, torso, head, arms, legs, boots)
 * made of ellipsoids, capsules and caps on the head sphere (visor, eyes). A
 * pose is a set of joint angles, the animations in uag_gen_atomic are
 * functions frame -> pose. DrawFigure projects the rig orthographically with
 * a slight top down camera for one facing direction and draws all parts back
 * to front with the style dependent DrawShapeOutlined, so front, back and
 * side views come from the same description.
 *
 * Body space: s = right hand side of the figure, u = up, f = forward, origin
 * on the ground between the feet, unit = pixel.
 * World / screen: X = right, Y = up, Z = towards the viewer.
 *)
Unit uag_rig;

{$MODE ObjFPC}{$H+}

Interface

Uses uag_image, uag_canvas, uag_context;

Const
  FacingDown = 0.0; // the figure looks at the viewer
  FacingRight = 1.57079632679489661923;
  FacingUp = 3.14159265358979323846;
  FacingLeft = -1.57079632679489661923;

Type

  TV3 = Record
    s, u, f: Double;
  End;

  TLimbPose = Record
    Swing: Double; // forward (+) / backward (-) from hanging down, pi = straight up
    Spread: Double; // outward from the body
    Bend: Double; // elbow (forward) / knee (backward)
    BendSpread: Double; // added to Spread for the lower segment (forearm inwards etc.)
  End;

  TPose = Record
    RootS, RootU, RootF: Double; // offset of the whole figure in pixel
    Lean: Double; // torso pitch, + = forward
    Roll: Double; // torso tilt sideways, + = towards the right hand side
    Turn: Double; // yaw of the whole figure (spin)
    HeadNod, HeadTurn, HeadRoll: Double;
    Arm: Array[0..1] Of TLimbPose; // 0 = right, 1 = left
    Leg: Array[0..1] Of TLimbPose;
    EyeOpen: Double; // 1 = open, 0 = closed
    LookS, LookU: Double; // pupils -1..1
    Reach: Double; // arm length factor (cartoon stretch), 1 = normal
  End;

  TFigureColors = Record
    Suit, Limb, Helmet: TAGColor; // player color (green)
    Visor, VisorHi, Eye, Pupil, Mouth: TAGColor;
    Glove, Boot, Belt, Accent, Pack, Outline, Crown: TAGColor;
  End;

  TFigure = Record
    HeadR, BodyW, BodyH, BodyD: Double;
    LegLen, ArmLen, LimbR, HipW, ShoulderW: Double;
    BootLen, BootR, GloveR: Double;
    Helmet: Integer; // 0 = crest, 1 = ear pods, 2 = beacon, 3 = band
    Visor: Integer; // 0 = round window, 1 = wide band, 2 = tall window
    Eyes: Integer; // 0 = round, 1 = tall, 2 = wide, 3 = big
    Face: Integer; // 0 = dark visor with glowing eyes, 1 = face with eyes and smile
    EyeW, EyeH, EyeSpread: Double; // degree
    Backpack, Hunted: Boolean;
    OutlineScale: Double;
    Col: TFigureColors;
    // animation character
    ArmSwing, Stride, StepHeight, Bob: Double;
  End;

Function V3(s, u, f: Double): TV3;
Function NeutralPose: TPose;
Function BlendPose(Const a, b: TPose; t: Double): TPose;
Function MakeFigure(Const Area: TAGArea; Hunted: Boolean): TFigure;
Function TintFigure(Const Fig: TFigure; c: TAGColor; Amount: Double; KeepGreen: Boolean): TFigure;

(*
 * Draws the figure, the ground point (between the feet) is at (AX, AY) of the
 * current canvas coordinates. Facing = FacingDown, FacingRight, ...
 *)
Procedure DrawFigure(C: TAGCanvas; Style: TAGStyle; Const Fig: TFigure; Const Pose: TPose; Facing, AX, AY: Double);

(* Screen position of a body space point (e.g. for effects at the head) *)
Function FigurePoint(Const Fig: TFigure; Const Pose: TPose; Facing, AX, AY: Double; Const p: TV3): TAGPoint;
Function HeadCenter(Const Fig: TFigure; Const Pose: TPose): TV3;

Implementation

Uses uag_math, uag_render;

Const
  CamTilt = 0.36; // radian, the camera looks a bit from above

Type
  TItemKind = (ikEllipse, ikCapsule, ikPoly);

  TItem = Record
    Kind: TItemKind;
    Depth: Double;
    Seq: Integer;
    x0, y0, x1, y1, r: Double; // capsule / ellipse center, r
    rx, ry, ang: Double;
    Pts: Array Of Double;
    Color, Outline: TAGColor;
    OScale: Double;
  End;

  TScene = Record
    Items: Array Of TItem;
    Count: Integer;
    AX, AY, Psi, ct, st: Double;
  End;

Function V3(s, u, f: Double): TV3;
Begin
  result.s := s;
  result.u := u;
  result.f := f;
End;

Function VAdd(Const a, b: TV3): TV3;
Begin
  result := V3(a.s + b.s, a.u + b.u, a.f + b.f);
End;

Function VSub(Const a, b: TV3): TV3;
Begin
  result := V3(a.s - b.s, a.u - b.u, a.f - b.f);
End;

Function VMul(Const a: TV3; k: Double): TV3;
Begin
  result := V3(a.s * k, a.u * k, a.f * k);
End;

Function VLen(Const a: TV3): Double;
Begin
  result := AGSqrt(a.s * a.s + a.u * a.u + a.f * a.f);
End;

Function VNorm(Const a: TV3): TV3;
Var
  l: Double;
Begin
  l := VLen(a);
  If l < 1E-9 Then exit(V3(0, 0, 1));
  result := VMul(a, 1 / l);
End;

Function Pitch(Const v: TV3; a: Double): TV3; // + = up vector tips forward
Var
  c, s: Double;
Begin
  c := AGCos(a);
  s := AGSin(a);
  result := V3(v.s, v.u * c - v.f * s, v.u * s + v.f * c);
End;

Function RollV(Const v: TV3; a: Double): TV3; // + = up vector tips to the right hand side
Var
  c, s: Double;
Begin
  c := AGCos(a);
  s := AGSin(a);
  result := V3(v.s * c + v.u * s, -v.s * s + v.u * c, v.f);
End;

Function YawV(Const v: TV3; a: Double): TV3; // + = forward turns towards the right hand side
Var
  c, s: Double;
Begin
  c := AGCos(a);
  s := AGSin(a);
  result := V3(v.s * c - v.f * s, v.u, v.s * s + v.f * c);
End;

Function LimbDir(Swing, Spread, Side: Double): TV3;
Var
  cs: Double;
Begin
  cs := AGCos(Spread);
  result := V3(Side * AGSin(Spread), -cs * AGCos(Swing), cs * AGSin(Swing));
End;

Function NeutralPose: TPose;
Var
  i: Integer;
Begin
  result := Default(TPose);
  For i := 0 To 1 Do Begin
    result.Arm[i].Swing := 0.05;
    result.Arm[i].Spread := 0.3;
    result.Arm[i].Bend := 0.3;
    result.Leg[i].Spread := 0.04;
  End;
  result.EyeOpen := 1;
  result.Reach := 1;
End;

Function BlendPose(Const a, b: TPose; t: Double): TPose;
Var
  i: Integer;

  Function L(x, y: Double): Double;
  Begin
    result := x + (y - x) * t;
  End;

  Function LL(Const x, y: TLimbPose): TLimbPose;
  Begin
    result.Swing := L(x.Swing, y.Swing);
    result.Spread := L(x.Spread, y.Spread);
    result.Bend := L(x.Bend, y.Bend);
    result.BendSpread := L(x.BendSpread, y.BendSpread);
  End;

Begin
  result.RootS := L(a.RootS, b.RootS);
  result.RootU := L(a.RootU, b.RootU);
  result.RootF := L(a.RootF, b.RootF);
  result.Lean := L(a.Lean, b.Lean);
  result.Roll := L(a.Roll, b.Roll);
  result.Turn := L(a.Turn, b.Turn);
  result.HeadNod := L(a.HeadNod, b.HeadNod);
  result.HeadTurn := L(a.HeadTurn, b.HeadTurn);
  result.HeadRoll := L(a.HeadRoll, b.HeadRoll);
  For i := 0 To 1 Do Begin
    result.Arm[i] := LL(a.Arm[i], b.Arm[i]);
    result.Leg[i] := LL(a.Leg[i], b.Leg[i]);
  End;
  result.EyeOpen := L(a.EyeOpen, b.EyeOpen);
  result.LookS := L(a.LookS, b.LookS);
  result.LookU := L(a.LookU, b.LookU);
  result.Reach := L(a.Reach, b.Reach);
End;

Function MakeFigure(Const Area: TAGArea; Hunted: Boolean): TFigure;
Const
  AccentHue: Array[0..5] Of Cardinal = ($FF9A28, $E04040, $3C8CFF, $FFD028, $F0F0F4, $30C8E8);
  BootCol: Array[0..3] Of Cardinal = ($4A3A34, $3A3C48, $2C3460, $6A2C30);
  EyeCol: Array[0..2] Of Cardinal = ($F8F8FF, $90E8FF, $FFE870);
Var
  r: TFigure;
  sz: Double;
  retro: Boolean;

  // default proportions, the retro style has a bigger head and a chunkier body
  Function D(Normal, Chunky: Double): Double;
  Begin
    If retro Then result := Chunky Else result := Normal;
  End;

Begin
  r := Default(TFigure);
  retro := assigned(Area.Ctx) And (Area.Ctx.Style = asRetro);
  sz := Area.Vary('size', 1.0, 0.06);
  r.HeadR := Area.Vary('headsize', D(12.5, 14.5), 1.3) * sz;
  r.BodyW := Area.Vary('bodywidth', D(9.5, 11.0), 1.3) * sz;
  r.BodyH := Area.Vary('bodyheight', D(14.0, 12.5), 1.4) * sz;
  r.BodyD := r.BodyW * 0.7;
  r.LegLen := Area.Vary('leglength', D(14.0, 11.5), 1.6) * sz;
  r.ArmLen := Area.Vary('armlength', D(13.0, 11.5), 1.5) * sz;
  r.LimbR := Area.Vary('limbwidth', D(3.4, 4.3), 0.5) * sz;
  r.HipW := r.BodyW * 0.5;
  r.ShoulderW := r.BodyW * 0.92;
  r.BootR := Area.Vary('bootsize', D(3.9, 4.7), 0.5) * sz;
  r.BootLen := r.BootR * 1.5;
  r.GloveR := r.LimbR * Area.Vary('glovesize', 1.35, 0.15);
  r.Helmet := Area.Pick('helmet', 4);
  r.Visor := Area.Pick('visor', 3);
  r.Eyes := Area.Pick('eyes', 4);
  r.Face := Area.Pick('face', 2);
  Case r.Eyes Of
    0: Begin r.EyeW := 11; r.EyeH := 11; End;
    1: Begin r.EyeW := 8.5; r.EyeH := 14; End;
    2: Begin r.EyeW := 13; r.EyeH := 8; End;
  Else Begin r.EyeW := 14; r.EyeH := 14; End;
  End;
  r.EyeSpread := Area.Vary('eyespread', 21, 3);
  r.Backpack := Area.Chance('backpack', 0.6);
  r.Hunted := Hunted;
  r.OutlineScale := Area.Vary('outline', 1.0, 0.2);
  r.ArmSwing := Area.Vary('armswing', 0.6, 0.15);
  r.Stride := Area.Vary('stride', 0.6, 0.1);
  r.StepHeight := Area.Vary('stepheight', 0.75, 0.2);
  r.Bob := Area.Vary('bob', 1.4, 0.6);

  r.Col.Suit := PlayerGreen(Area.Vary('suitlight', 0.55, 0.1));
  r.Col.Limb := PlayerGreen(0.45);
  r.Col.Helmet := PlayerGreen(Area.Vary('helmetlight', 0.7, 0.1));
  r.Col.Outline := NotGreen(AGRGB(22, 22, 34));
  r.Col.Accent := NotGreen(AGHex(AccentHue[Area.Pick('accent', length(AccentHue))]));
  r.Col.Boot := NotGreen(AGHex(BootCol[Area.Pick('boots', length(BootCol))]));
  r.Col.Glove := NotGreen(AGRGB(238, 238, 244));
  r.Col.Belt := NotGreen(AGRGB(58, 58, 70));
  r.Col.Pack := NotGreen(AGRGB(150, 152, 168));
  r.Col.Crown := NotGreen(AGRGB(255, 200, 40));
  If r.Face = 0 Then Begin
    r.Col.Visor := NotGreen(AGRGB(30, 38, 72));
    r.Col.VisorHi := NotGreen(AGRGB(150, 175, 235));
    r.Col.Eye := NotGreen(AGHex(EyeCol[Area.Pick('eyecolor', length(EyeCol))]));
    r.Col.Pupil := NotGreen(AGRGB(20, 24, 50));
  End
  Else Begin
    r.Col.Visor := NotGreen(AGHSV(Area.Vary('skinhue', 28, 6), Area.Vary('skinsat', 0.32, 0.08), 0.98));
    r.Col.VisorHi := NotGreen(AGRGB(255, 250, 245));
    r.Col.Eye := NotGreen(AGRGB(255, 255, 255));
    r.Col.Pupil := NotGreen(AGRGB(30, 26, 40));
  End;
  r.Col.Mouth := NotGreen(AGRGB(120, 30, 40));
  result := r;
End;

Function TintFigure(Const Fig: TFigure; c: TAGColor; Amount: Double; KeepGreen: Boolean): TFigure;

  Function T(x: TAGColor): TAGColor;
  Begin
    result := AGMix(x, c, Amount);
    If Not KeepGreen Then result := NotGreen(result);
  End;

Begin
  result := Fig;
  With result.Col Do Begin
    Suit := T(Suit);
    Limb := T(Limb);
    Helmet := T(Helmet);
    Visor := T(Visor);
    VisorHi := T(VisorHi);
    Pupil := T(Pupil);
    Mouth := T(Mouth);
    Glove := T(Glove);
    Boot := T(Boot);
    Belt := T(Belt);
    Accent := T(Accent);
    Pack := T(Pack);
    Crown := T(Crown);
    If KeepGreen Then Eye := T(Eye); // a burnt (not green) figure keeps its bright eyes
  End;
End;

(* ---------------------------------------------------------------------------- *)

Type
  TWorld = Record
    X, Y, Z: Double;
  End;

Function ToWorld(Const Sc: TScene; Const v: TV3): TWorld; // direction / offset, no translation
Var
  c, s: Double;
Begin
  c := AGCos(Sc.Psi);
  s := AGSin(Sc.Psi);
  // forward = (sin psi, 0, cos psi), right = forward x up = (-cos psi, 0, sin psi)
  result.X := -v.s * c + v.f * s;
  result.Y := v.u;
  result.Z := v.s * s + v.f * c;
End;

Procedure Project(Const Sc: TScene; Const p: TV3; Out x, y, d: Double);
Var
  w: TWorld;
Begin
  w := ToWorld(Sc, p);
  x := Sc.AX + w.X;
  y := Sc.AY - w.Y * Sc.ct + w.Z * Sc.st;
  d := w.Z * Sc.ct + w.Y * Sc.st;
End;

Procedure ProjectDir(Const Sc: TScene; Const v: TV3; Out x, y, d: Double);
Var
  w: TWorld;
Begin
  w := ToWorld(Sc, v);
  x := w.X;
  y := -w.Y * Sc.ct + w.Z * Sc.st;
  d := w.Z * Sc.ct + w.Y * Sc.st;
End;

Function NewItem(Var Sc: TScene): Integer;
Begin
  If Sc.Count >= length(Sc.Items) Then setlength(Sc.Items, Sc.Count + 16);
  result := Sc.Count;
  Sc.Items[result] := Default(TItem);
  Sc.Items[result].Seq := result;
  inc(Sc.Count);
End;

Procedure AddCapsule(Var Sc: TScene; Const p0, p1: TV3; r: Double; Col, Outline: TAGColor; OScale, Bias: Double);
Var
  i: Integer;
  d0, d1: Double;
Begin
  i := NewItem(Sc);
  Sc.Items[i].Kind := ikCapsule;
  Project(Sc, p0, Sc.Items[i].x0, Sc.Items[i].y0, d0);
  Project(Sc, p1, Sc.Items[i].x1, Sc.Items[i].y1, d1);
  Sc.Items[i].Depth := (d0 + d1) / 2 + Bias;
  Sc.Items[i].r := r;
  Sc.Items[i].Color := Col;
  Sc.Items[i].Outline := Outline;
  Sc.Items[i].OScale := OScale;
End;

(* Ellipsoid with the (scaled) semi axis vectors a1, a2, a3 in body space *)
Procedure AddEllipsoid(Var Sc: TScene; Const Center, a1, a2, a3: TV3; Col, Outline: TAGColor; OScale, Bias: Double);
Var
  i: Integer;
  px, py, pd, a, b, c, h, q, l1, l2: Double;
  dummy: Double;

  Procedure Acc(Const v: TV3);
  Begin
    ProjectDir(Sc, v, px, py, dummy);
    a := a + px * px;
    b := b + px * py;
    c := c + py * py;
  End;

Begin
  a := 0;
  b := 0;
  c := 0;
  Acc(a1);
  Acc(a2);
  Acc(a3);
  h := (a + c) / 2;
  q := AGSqrt(sqr((a - c) / 2) + b * b);
  l1 := h + q;
  l2 := h - q;
  If l2 < 0.01 Then l2 := 0.01;
  i := NewItem(Sc);
  Sc.Items[i].Kind := ikEllipse;
  Project(Sc, Center, Sc.Items[i].x0, Sc.Items[i].y0, pd);
  Sc.Items[i].Depth := pd + Bias;
  Sc.Items[i].rx := AGSqrt(l1);
  Sc.Items[i].ry := AGSqrt(l2);
  Sc.Items[i].ang := 0.5 * AGAtan2(2 * b, a - c);
  Sc.Items[i].Color := Col;
  Sc.Items[i].Outline := Outline;
  Sc.Items[i].OScale := OScale;
End;

Procedure AddSphere(Var Sc: TScene; Const Center: TV3; r: Double; Col, Outline: TAGColor; OScale, Bias: Double);
Begin
  AddEllipsoid(Sc, Center, V3(r, 0, 0), V3(0, r, 0), V3(0, 0, r), Col, Outline, OScale, Bias);
End;

(*
 * Cap on a sphere (center, radius r), centered at direction n with the
 * tangents e1 (horizontal) and e2 (vertical), angular half sizes ah, av in
 * degree. Boundary points on the hidden side are pushed onto the silhouette.
 *)
Procedure AddCap(Var Sc: TScene; Const Center: TV3; r: Double; Const n, e1, e2: TV3; ah, av: Double;
  Col, Outline: TAGColor; OScale, Depth: Double);
Const
  N_ = 28;
Var
  i, k: Integer;
  th, tv, phi, cx, cy, cd, px, py, pd, l, nd, dummy1, dummy2: Double;
  dir: TV3;
Begin
  th := AGSin(AGDeg(ah)) / AGCos(AGDeg(ah));
  tv := AGSin(AGDeg(av)) / AGCos(AGDeg(av));
  // completely on the back side?
  ProjectDir(Sc, n, dummy1, dummy2, nd);
  If nd < -AGSin(AGDeg(AGMax(ah, av))) + 0.05 Then exit;
  Project(Sc, Center, cx, cy, cd);
  k := NewItem(Sc);
  Sc.Items[k].Kind := ikPoly;
  Sc.Items[k].Depth := Depth;
  Sc.Items[k].Color := Col;
  Sc.Items[k].Outline := Outline;
  Sc.Items[k].OScale := OScale;
  setlength(Sc.Items[k].Pts, 2 * N_);
  For i := 0 To N_ - 1 Do Begin
    phi := AG_TWO_PI * i / N_;
    dir := VNorm(VAdd(n, VAdd(VMul(e1, th * AGCos(phi)), VMul(e2, tv * AGSin(phi)))));
    ProjectDir(Sc, VMul(dir, r), px, py, pd);
    If pd < 0 Then Begin
      l := AGSqrt(px * px + py * py);
      If l > 1E-6 Then Begin
        px := px * r / l;
        py := py * r / l;
      End;
    End;
    Sc.Items[k].Pts[2 * i] := cx + px;
    Sc.Items[k].Pts[2 * i + 1] := cy + py;
  End;
End;

(* Cap with direction given by yaw a (towards the right hand side) and pitch b (up), in degree, in a rotated frame *)
Type
  TFrame = Record
    S, U, F: TV3; // unit axes of a local frame in body space
  End;

Function FrameDir(Const Fr: TFrame; Const v: TV3): TV3;
Begin
  result := VAdd(VAdd(VMul(Fr.S, v.s), VMul(Fr.U, v.u)), VMul(Fr.F, v.f));
End;

Procedure AddHeadCap(Var Sc: TScene; Const Fr: TFrame; Const Center: TV3; r, Yaw, Pitch_, ah, av: Double;
  Col, Outline: TAGColor; OScale, Depth: Double);
Var
  a, b: Double;
  n, e1, e2: TV3;
Begin
  a := AGDeg(Yaw);
  b := AGDeg(Pitch_);
  n := FrameDir(Fr, V3(AGSin(a) * AGCos(b), AGSin(b), AGCos(a) * AGCos(b)));
  e1 := FrameDir(Fr, V3(AGCos(a), 0, -AGSin(a)));
  e2 := FrameDir(Fr, V3(-AGSin(a) * AGSin(b), AGCos(b), -AGCos(a) * AGSin(b)));
  AddCap(Sc, Center, r, n, e1, e2, ah, av, Col, Outline, OScale, Depth);
End;

(*
 * Band around the head sphere between the latitudes Lat1 < Lat2 (degree) of
 * the frame Fr, only the visible arc is drawn.
 *)
Procedure AddHeadBand(Var Sc: TScene; Const Fr: TFrame; Const Center: TV3; r, Lat1, Lat2: Double;
  Col, Outline: TAGColor; OScale, Depth: Double);
Const
  N_ = 64;
Var
  vis: Array[0..N_ - 1] Of Boolean;
  k, k0, j, cnt, it: Integer;
  phi, cx, cy, cd, px, py, pd, lm: Double;
  up, lo: Array Of Double;

  Function Dir(Lat, Phi_: Double): TV3;
  Begin
    result := VAdd(VAdd(VMul(Fr.S, AGCos(AGDeg(Lat)) * AGSin(Phi_)), VMul(Fr.U, AGSin(AGDeg(Lat)))),
      VMul(Fr.F, AGCos(AGDeg(Lat)) * AGCos(Phi_)));
  End;

Begin
  lm := (Lat1 + Lat2) / 2;
  cnt := 0;
  For k := 0 To N_ - 1 Do Begin
    ProjectDir(Sc, Dir(lm, AG_TWO_PI * k / N_), px, py, pd);
    vis[k] := pd > 0;
    If vis[k] Then inc(cnt);
  End;
  If (cnt < 2) Or (cnt = N_) Then exit;
  k0 := -1;
  For k := 0 To N_ - 1 Do
    If (Not vis[k]) And vis[(k + 1) Mod N_] Then Begin
      k0 := (k + 1) Mod N_;
      break;
    End;
  If k0 < 0 Then exit;
  Project(Sc, Center, cx, cy, cd);
  setlength(up, 0);
  setlength(lo, 0);
  // from half a step before the first visible point to half a step after the last one
  For j := -1 To cnt Do Begin
    phi := AG_TWO_PI * (k0 + j + 0.5 * ord(j = -1) - 0.5 * ord(j = cnt)) / N_;
    setlength(up, length(up) + 2);
    setlength(lo, length(lo) + 2);
    ProjectDir(Sc, VMul(Dir(Lat2, phi), r), px, py, pd);
    up[high(up) - 1] := cx + px;
    up[high(up)] := cy + py;
    ProjectDir(Sc, VMul(Dir(Lat1, phi), r), px, py, pd);
    lo[high(lo) - 1] := cx + px;
    lo[high(lo)] := cy + py;
  End;
  it := NewItem(Sc);
  Sc.Items[it].Kind := ikPoly;
  Sc.Items[it].Depth := Depth;
  Sc.Items[it].Color := Col;
  Sc.Items[it].Outline := Outline;
  Sc.Items[it].OScale := OScale;
  setlength(Sc.Items[it].Pts, length(up) + length(lo));
  For j := 0 To length(up) Div 2 - 1 Do Begin
    Sc.Items[it].Pts[2 * j] := up[2 * j];
    Sc.Items[it].Pts[2 * j + 1] := up[2 * j + 1];
  End;
  k := length(up);
  For j := length(lo) Div 2 - 1 Downto 0 Do Begin
    Sc.Items[it].Pts[k] := lo[2 * j];
    Sc.Items[it].Pts[k + 1] := lo[2 * j + 1];
    inc(k, 2);
  End;
End;

Procedure SortItems(Var Sc: TScene);
Var
  i, j: Integer;
  t: TItem;

  Function Less(Const a, b: TItem): Boolean;
  Begin
    If a.Depth <> b.Depth Then exit(a.Depth < b.Depth);
    result := a.Seq < b.Seq;
  End;

Begin
  // insertion sort, stable and deterministic
  For i := 1 To Sc.Count - 1 Do Begin
    t := Sc.Items[i];
    j := i - 1;
    While (j >= 0) And Less(t, Sc.Items[j]) Do Begin
      Sc.Items[j + 1] := Sc.Items[j];
      dec(j);
    End;
    Sc.Items[j + 1] := t;
  End;
End;

Procedure DrawItems(C: TAGCanvas; Style: TAGStyle; Var Sc: TScene; OutlineScale: Double);
Var
  i: Integer;
Begin
  For i := 0 To Sc.Count - 1 Do
    With Sc.Items[i] Do Begin
      Case Kind Of
        ikEllipse: Begin
            C.Push;
            C.Translate(x0, y0);
            C.Rotate(ang);
            C.Ellipse(0, 0, rx, ry);
            DrawShapeOutlined(C, Style, Color, Outline, OScale * OutlineScale);
            C.Pop;
          End;
        ikCapsule: Begin
            C.Capsule(x0, y0, x1, y1, r);
            DrawShapeOutlined(C, Style, Color, Outline, OScale * OutlineScale);
          End;
        ikPoly: Begin
            C.Polygon(Pts);
            DrawShapeOutlined(C, Style, Color, Outline, OScale * OutlineScale);
          End;
      End;
    End;
End;

(* ---------------------------------------------------------------------------- *)

Type
  TRig = Record
    Hip, TorsoC, Shoulder: Array[0..1] Of TV3;
    Elbow, Hand, Knee, Ankle, Toe, Heel: Array[0..1] Of TV3;
    HipC, Torso, Head, Belt, Chest, Pack: TV3;
    TF, HF: TFrame; // torso frame, head frame
  End;

Function MakeFrame(Const Pose: TPose; Head: Boolean): TFrame;

  Function R(Const v: TV3): TV3;
  Begin
    result := v;
    If Head Then
      result := YawV(Pitch(RollV(result, Pose.HeadRoll), Pose.HeadNod), Pose.HeadTurn);
    result := RollV(Pitch(result, Pose.Lean), Pose.Roll);
    result := YawV(result, Pose.Turn);
  End;

Begin
  result.S := R(V3(1, 0, 0));
  result.U := R(V3(0, 1, 0));
  result.F := R(V3(0, 0, 1));
End;

Function BuildRig(Const Fig: TFigure; Const Pose: TPose): TRig;
Var
  i: Integer;
  side, a: Double;
  root, hip: TV3;
  BF, TF: TFrame; // body frame without the torso lean (legs), torso frame

  Function Up(Const p: TV3): TV3; // torso local -> body space
  Begin
    result := VAdd(hip, FrameDir(TF, p));
  End;

Begin
  result := Default(TRig);
  root := V3(Pose.RootS, Pose.RootU, Pose.RootF);
  BF.S := YawV(V3(1, 0, 0), Pose.Turn);
  BF.U := V3(0, 1, 0);
  BF.F := YawV(V3(0, 0, 1), Pose.Turn);
  TF := MakeFrame(Pose, false);
  result.TF := TF;
  result.HF := MakeFrame(Pose, true);
  hip := VAdd(root, V3(0, Fig.LegLen + Fig.BootR * 0.85, 0));
  result.HipC := hip;
  result.Torso := Up(V3(0, Fig.BodyH * 0.5 - 0.5, 0));
  result.Belt := Up(V3(0, Fig.BodyH * 0.1, 0));
  result.Chest := Up(V3(0, Fig.BodyH * 0.55, Fig.BodyD * 0.93));
  result.Pack := Up(V3(0, Fig.BodyH * 0.55, -Fig.BodyD * 1.05));
  result.Head := Up(V3(0, Fig.BodyH + Fig.HeadR * 0.72, 0));
  For i := 0 To 1 Do Begin
    If i = 0 Then side := 1 Else side := -1;
    // arms (torso frame)
    result.Shoulder[i] := Up(V3(side * Fig.ShoulderW, Fig.BodyH * 0.8, 0));
    result.Elbow[i] := VAdd(result.Shoulder[i], FrameDir(result.TF,
      VMul(LimbDir(Pose.Arm[i].Swing, Pose.Arm[i].Spread, side), Fig.ArmLen * 0.5 * Pose.Reach)));
    result.Hand[i] := VAdd(result.Elbow[i], FrameDir(result.TF,
      VMul(LimbDir(Pose.Arm[i].Swing + Pose.Arm[i].Bend, Pose.Arm[i].Spread + Pose.Arm[i].BendSpread, side), Fig.ArmLen * 0.5 * Pose.Reach)));
    // legs (body frame, not leaning)
    result.Hip[i] := VAdd(hip, FrameDir(BF, V3(side * Fig.HipW, 0, 0)));
    result.Knee[i] := VAdd(result.Hip[i], FrameDir(BF,
      VMul(LimbDir(Pose.Leg[i].Swing, Pose.Leg[i].Spread, side), Fig.LegLen * 0.5)));
    result.Ankle[i] := VAdd(result.Knee[i], FrameDir(BF,
      VMul(LimbDir(Pose.Leg[i].Swing - Pose.Leg[i].Bend, Pose.Leg[i].Spread + Pose.Leg[i].BendSpread, side), Fig.LegLen * 0.5)));
    a := Pose.Leg[i].Swing - Pose.Leg[i].Bend;
    result.Toe[i] := VAdd(result.Ankle[i], FrameDir(BF, V3(side * 0.4, AGSin(a) * Fig.BootLen, AGCos(a) * Fig.BootLen)));
    result.Heel[i] := VAdd(result.Ankle[i], FrameDir(BF, V3(0, -AGSin(a) * 1.2, -AGCos(a) * 1.2)));
  End;
End;

Function HeadCenter(Const Fig: TFigure; Const Pose: TPose): TV3;
Begin
  result := BuildRig(Fig, Pose).Head;
End;

Function FigurePoint(Const Fig: TFigure; Const Pose: TPose; Facing, AX, AY: Double; Const p: TV3): TAGPoint;
Var
  Sc: TScene;
  d: Double;
Begin
  Sc := Default(TScene);
  Sc.AX := AX;
  Sc.AY := AY;
  Sc.Psi := Facing;
  Sc.ct := AGCos(CamTilt);
  Sc.st := AGSin(CamTilt);
  Project(Sc, p, result.x, result.y, d);
End;

Procedure DrawFigure(C: TAGCanvas; Style: TAGStyle; Const Fig: TFigure; Const Pose: TPose; Facing, AX, AY: Double);
Var
  Sc: TScene;
  Rg: TRig;
  i: Integer;
  side, hd, hx, hy, ew, eh, lx, ly, k, bw, bu, td, fd: Double;
  ol: TAGColor;
  e: Array[0..1] Of Double;
  cp: TAGPoint;
Begin
  Sc := Default(TScene);
  Sc.AX := AX;
  Sc.AY := AY;
  Sc.Psi := Facing;
  Sc.ct := AGCos(CamTilt);
  Sc.st := AGSin(CamTilt);
  Rg := BuildRig(Fig, Pose);
  ol := Fig.Col.Outline;

  // legs and boots
  For i := 0 To 1 Do Begin
    AddCapsule(Sc, Rg.Hip[i], Rg.Knee[i], Fig.LimbR * 1.08, Fig.Col.Limb, ol, 1, -3.0);
    AddCapsule(Sc, Rg.Knee[i], Rg.Ankle[i], Fig.LimbR, Fig.Col.Limb, ol, 1, -2.9);
    AddCapsule(Sc, Rg.Heel[i], Rg.Toe[i], Fig.BootR, Fig.Col.Boot, ol, 1, -2.0);
  End;
  // torso, belt, chest emblem, backpack
  AddEllipsoid(Sc, Rg.Torso, VMul(Rg.TF.S, Fig.BodyW), VMul(Rg.TF.U, Fig.BodyH * 0.58), VMul(Rg.TF.F, Fig.BodyD),
    Fig.Col.Suit, ol, 1, -1.0);
  td := Sc.Items[Sc.Count - 1].Depth;
  ProjectDir(Sc, Rg.TF.F, hx, hy, fd); // fd > 0: the front of the torso faces the viewer
  // belt: thin disc with the width of the torso at its height
  bu := (Fig.BodyH * 0.1 - (Fig.BodyH * 0.5 - 0.5)) / (Fig.BodyH * 0.58);
  bw := AGSqrt(AGMax(0.05, 1 - bu * bu)) * 1.04;
  AddEllipsoid(Sc, Rg.Belt, VMul(Rg.TF.S, Fig.BodyW * bw), VMul(Rg.TF.U, 1.9), VMul(Rg.TF.F, Fig.BodyD * bw),
    Fig.Col.Belt, ol, 0.7, 0);
  Sc.Items[Sc.Count - 1].Depth := td + 0.01;
  If fd > 0.15 Then Begin
    AddEllipsoid(Sc, VAdd(Rg.Belt, VMul(Rg.TF.F, Fig.BodyD * bw * 0.98)), VMul(Rg.TF.S, 2.2), VMul(Rg.TF.U, 1.9),
      VMul(Rg.TF.F, 0.4), Fig.Col.Accent, ol, 0.5, 0);
    Sc.Items[Sc.Count - 1].Depth := td + 0.02;
    AddEllipsoid(Sc, Rg.Chest, VMul(Rg.TF.S, 3.0), VMul(Rg.TF.U, 3.0), VMul(Rg.TF.F, 0.5),
      Fig.Col.Accent, ol, 0.6, 0);
    Sc.Items[Sc.Count - 1].Depth := td + 0.02;
  End;
  If Fig.Backpack Then Begin
    AddEllipsoid(Sc, Rg.Pack, VMul(Rg.TF.S, Fig.BodyW * 0.62), VMul(Rg.TF.U, Fig.BodyH * 0.4), VMul(Rg.TF.F, Fig.BodyD * 0.45),
      Fig.Col.Pack, ol, 1, 0);
    If fd < -0.1 Then
      Sc.Items[Sc.Count - 1].Depth := td + 0.03
    Else
      Sc.Items[Sc.Count - 1].Depth := td - 0.01;
  End;
  // arms and gloves
  For i := 0 To 1 Do Begin
    AddCapsule(Sc, Rg.Shoulder[i], Rg.Elbow[i], Fig.LimbR, Fig.Col.Limb, ol, 1, 1.2);
    AddCapsule(Sc, Rg.Elbow[i], Rg.Hand[i], Fig.LimbR * 0.95, Fig.Col.Limb, ol, 1, 1.3);
    AddSphere(Sc, Rg.Hand[i], Fig.GloveR, Fig.Col.Glove, ol, 1, 1.5);
  End;

  // head: helmet sphere, decoration, visor and eyes as caps
  Project(Sc, Rg.Head, hx, hy, hd);
  hd := hd + 2.5;
  Case Fig.Helmet Of
    0: Begin // crest from front to back over the top
        AddEllipsoid(Sc, VAdd(Rg.Head, VMul(Rg.HF.U, Fig.HeadR * 0.8)), VMul(Rg.HF.S, 1.8),
          VMul(Rg.HF.U, Fig.HeadR * 0.42), VMul(Rg.HF.F, Fig.HeadR * 0.78), Fig.Col.Accent, ol, 1, -100);
        Sc.Items[Sc.Count - 1].Depth := hd - 0.01;
      End;
    1: Begin // ear pods
        For i := 0 To 1 Do Begin
          If i = 0 Then side := 1 Else side := -1;
          AddEllipsoid(Sc, VAdd(Rg.Head, VMul(Rg.HF.S, side * Fig.HeadR * 0.93)), VMul(Rg.HF.S, 2.2),
            VMul(Rg.HF.U, Fig.HeadR * 0.4), VMul(Rg.HF.F, Fig.HeadR * 0.4), Fig.Col.Accent, ol, 1, 0);
          // the pod belongs to the head: in front or behind it
          If Sc.Items[Sc.Count - 1].Depth < hd - Fig.HeadR * 0.3 Then
            Sc.Items[Sc.Count - 1].Depth := hd - 0.02
          Else
            Sc.Items[Sc.Count - 1].Depth := hd + 0.05;
        End;
      End;
    2: Begin // beacon on top
        AddEllipsoid(Sc, VAdd(Rg.Head, VMul(Rg.HF.U, Fig.HeadR * 0.93)), VMul(Rg.HF.S, Fig.HeadR * 0.36),
          VMul(Rg.HF.U, Fig.HeadR * 0.12), VMul(Rg.HF.F, Fig.HeadR * 0.36), Fig.Col.Belt, ol, 1, 0);
        Sc.Items[Sc.Count - 1].Depth := hd - 0.02;
        AddSphere(Sc, VAdd(Rg.Head, VMul(Rg.HF.U, Fig.HeadR * 1.1)), Fig.HeadR * 0.24, Fig.Col.Accent, ol, 1, 0);
        Sc.Items[Sc.Count - 1].Depth := hd - 0.01;
      End;
  End;
  AddSphere(Sc, Rg.Head, Fig.HeadR, Fig.Col.Helmet, ol, 1.1, 0);
  Sc.Items[Sc.Count - 1].Depth := hd;
  If Fig.Helmet = 3 Then // band around the helmet
    AddHeadBand(Sc, Rg.HF, Rg.Head, Fig.HeadR * 1.01, 14, 30, Fig.Col.Accent, ol, 0.7, hd + 0.005);
  If Fig.Hunted Then Begin // crown
    cp := FigurePoint(Fig, Pose, Facing, AX, AY, VAdd(Rg.Head, VMul(Rg.HF.U, Fig.HeadR * 0.92)));
    k := Fig.HeadR * 0.62;
    i := NewItem(Sc);
    Sc.Items[i].Kind := ikPoly;
    Sc.Items[i].Depth := hd + 0.2;
    Sc.Items[i].Color := Fig.Col.Crown;
    Sc.Items[i].Outline := ol;
    Sc.Items[i].OScale := 0.8;
    Sc.Items[i].Pts := [cp.x - k, cp.y + 2, cp.x - k * 1.1, cp.y - k * 0.9, cp.x - k * 0.5, cp.y - k * 0.35,
      cp.x, cp.y - k * 1.1, cp.x + k * 0.5, cp.y - k * 0.35, cp.x + k * 1.1, cp.y - k * 0.9, cp.x + k, cp.y + 2];
  End;

  // visor
  Case Fig.Visor Of
    0: Begin e[0] := 50; e[1] := 42; End;
    1: Begin e[0] := 68; e[1] := 30; End;
  Else Begin e[0] := 42; e[1] := 50; End;
  End;
  AddHeadCap(Sc, Rg.HF, Rg.Head, Fig.HeadR, 0, -10, e[0], e[1], Fig.Col.Visor, ol, 0.8, hd + 0.1);
  If Fig.Face = 0 Then // glass reflex
    AddHeadCap(Sc, Rg.HF, Rg.Head, Fig.HeadR, -e[0] * 0.45, -10 + e[1] * 0.55, e[0] * 0.18, e[1] * 0.12, Fig.Col.VisorHi, ol, 0, hd + 0.11);
  // eyes
  ew := Fig.EyeW * 2.2;
  eh := Fig.EyeH * 2.2 * AGClamp(Pose.EyeOpen, 0.08, 1);
  If Fig.Face = 1 Then Begin
    ew := ew * 0.62;
    eh := eh * 0.7;
  End;
  If e[1] < 35 Then eh := AGMin(eh, 22); // wide band visor
  lx := Pose.LookS * ew * 0.35;
  ly := Pose.LookU * eh * 0.3;
  For i := 0 To 1 Do Begin
    If i = 0 Then side := 1 Else side := -1;
    If Fig.Face = 0 Then Begin
      AddHeadCap(Sc, Rg.HF, Rg.Head, Fig.HeadR, side * Fig.EyeSpread, -6, ew * 0.5, eh * 0.5, Fig.Col.Eye, ol, 0.6, hd + 0.2 + i * 0.001);
      If Pose.EyeOpen > 0.3 Then
        AddHeadCap(Sc, Rg.HF, Rg.Head, Fig.HeadR, side * Fig.EyeSpread + lx, -6 + ly, ew * 0.22, AGMin(eh * 0.3, ew * 0.3),
          Fig.Col.Pupil, ol, 0, hd + 0.3 + i * 0.001);
    End
    Else Begin
      AddHeadCap(Sc, Rg.HF, Rg.Head, Fig.HeadR, side * Fig.EyeSpread * 0.85 + lx, -2 + ly, ew * 0.5, eh * 0.5, Fig.Col.Pupil, ol, 0, hd + 0.2 + i * 0.001);
      If Pose.EyeOpen > 0.3 Then
        AddHeadCap(Sc, Rg.HF, Rg.Head, Fig.HeadR, side * Fig.EyeSpread * 0.85 + lx - ew * 0.14, -2 + ly + eh * 0.16, ew * 0.17, eh * 0.17,
          Fig.Col.Eye, ol, 0, hd + 0.3 + i * 0.001);
    End;
  End;
  If Fig.Face = 1 Then // smile
    AddHeadCap(Sc, Rg.HF, Rg.Head, Fig.HeadR, 0, -24, 10, 3.5, Fig.Col.Mouth, ol, 0, hd + 0.2);

  SortItems(Sc);
  DrawItems(C, Style, Sc, Fig.OutlineScale);
End;

End.
