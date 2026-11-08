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
 * Schemes (schemes/*.SCH): start layouts of the 15 x 11 field.
 *
 * Schemes are game data, not graphics: the server and every client must see
 * exactly the same files. Therefore this generator ignores Ctx completely
 * (seed, style, parameters) and uses a constant seed and only integer logic.
 * Every run on every platform writes the same bytes.
 *
 * Format (parsed by TGame.LoadSchemeFromFile in client/ugame.pas):
 *   -V,2                       version
 *   -N,<name>                  name
 *   -B,<0..100>                brick density in percent (':' cells)
 *   -R,<row>,<15 chars>        # solid, : brick, . blank (rows 0..10)
 *   -S,<player>,<x>,<y>,<team> start positions of the 10 players
 *   -P,<pu>,<bornwith>,<has_override>,<override>,<forbidden>,<comment>
 *
 * Every layout is symmetric (4 fold, left / right or 180 degree rotation),
 * all free cells are connected, and each start cell and its 4 neighbours are
 * free (the engine clears the same cells, but never removes solids).
 *)
Unit uag_gen_schemes;

{$MODE ObjFPC}{$H+}

Interface

Const
  SchemeCount = 25;

Implementation

Uses SysUtils, uag_random, uag_context, uag_output, uag_registry;

Const
  W = 15;
  H = 11;
  SchemeSeed = QWord($5C4E3E5A70A1C);

Type
  TSym = (sym4, symX, sym2); // mirror x and y, mirror x, rotation by 180 degree
  TSpawnSet = (spRim, spInner, spSides);
  TPuPreset = (ppNormal, ppQuick, ppBigBang, ppHealthy, ppJelly, ppPurist, ppTrigger);

  TSchemeDef = Record
    FileName, Name: String;
    Family: Integer;
    Sym: TSym;
    Density: Integer; // -B
    Fill: Integer; // percent of the non solid cells that are bricks
    Extra: Integer; // family parameter, e.g. percent of random solids
    Spawn: TSpawnSet;
    Pu: TPuPreset;
  End;

  TGrid = Array[0..W - 1, 0..H - 1] Of Char;

Const
  // Families
  fmOpen = 0;
  fmPillars = 1;
  fmSparse = 2;
  fmRandom = 3; // Extra = percent solids
  fmCross = 4;
  fmTwinRooms = 5;
  fmDiamond = 6;
  fmLanes = 7;
  fmFrame = 8;
  fmColumns = 9;
  fmPinwheel = 10;
  fmBunkers = 11;
  fmHighway = 12;
  fmStagger = 13;
  fmArena = 14;
  fmZigzag = 15;
  fmCourtyards = 16;
  fmPillarsPlus = 17; // pillars + Extra percent random solids

  Defs: Array[0..SchemeCount - 1] Of TSchemeDef = (
    (FileName: 'CROSSROADS'; Name: 'Crossroads (10)'; Family: fmCross; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'TWINTOWERS'; Name: 'Twin Towers (10)'; Family: fmTwinRooms; Sym: sym4; Density: 85; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'OPENFIELD'; Name: 'Open Field (10)'; Family: fmOpen; Sym: sym4; Density: 80; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'LABYRINTH'; Name: 'Labyrinth (10)'; Family: fmRandom; Sym: sym4; Density: 100; Fill: 85; Extra: 34; Spawn: spInner; Pu: ppNormal),
    (FileName: 'DIAMONDRING'; Name: 'Diamond Ring (10)'; Family: fmDiamond; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'RAILWAY'; Name: 'Railway (10)'; Family: fmLanes; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spSides; Pu: ppNormal),
    (FileName: 'CHESSBOARD'; Name: 'Chessboard (10)'; Family: fmPillarsPlus; Sym: sym4; Density: 95; Fill: 100; Extra: 20; Spawn: spRim; Pu: ppNormal),
    (FileName: 'FEWPILLARS'; Name: 'Few Pillars (10)'; Family: fmSparse; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spInner; Pu: ppNormal),
    (FileName: 'FORTRESS'; Name: 'Fortress (10)'; Family: fmFrame; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'STONEGARDEN'; Name: 'Stone Garden (10)'; Family: fmRandom; Sym: sym2; Density: 90; Fill: 90; Extra: 20; Spawn: spRim; Pu: ppNormal),
    (FileName: 'EASTMEETSWEST'; Name: 'East meets West (Teams)'; Family: fmColumns; Sym: symX; Density: 100; Fill: 100; Extra: 0; Spawn: spSides; Pu: ppNormal),
    (FileName: 'WINDMILL'; Name: 'Windmill (10)'; Family: fmPinwheel; Sym: sym2; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'CANYON'; Name: 'Canyon (10)'; Family: fmColumns; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spInner; Pu: ppNormal),
    (FileName: 'SNOWFLAKE'; Name: 'Snowflake (10)'; Family: fmRandom; Sym: sym4; Density: 90; Fill: 95; Extra: 24; Spawn: spRim; Pu: ppNormal),
    (FileName: 'BUNKERS'; Name: 'Bunkers (10)'; Family: fmBunkers; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'HIGHWAY'; Name: 'Highway (10)'; Family: fmHighway; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'HONEYCOMB'; Name: 'Honeycomb (10)'; Family: fmStagger; Sym: sym2; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'QUICKDRAW'; Name: 'Quickdraw (10)'; Family: fmPillars; Sym: sym4; Density: 60; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppQuick),
    (FileName: 'BIGBANG'; Name: 'Big Bang (10)'; Family: fmSparse; Sym: sym4; Density: 100; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppBigBang),
    (FileName: 'CLEANAIR'; Name: 'Clean Air (no diseases)'; Family: fmBunkers; Sym: sym2; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppHealthy),
    (FileName: 'ARENA'; Name: 'Arena (10)'; Family: fmArena; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppNormal),
    (FileName: 'ZIGZAG'; Name: 'Zigzag (10)'; Family: fmZigzag; Sym: sym2; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppJelly),
    (FileName: 'COURTYARDS'; Name: 'Courtyards (10)'; Family: fmCourtyards; Sym: sym4; Density: 90; Fill: 100; Extra: 0; Spawn: spRim; Pu: ppPurist),
    (FileName: 'MINEFIELD'; Name: 'Minefield (10)'; Family: fmPillarsPlus; Sym: sym2; Density: 100; Fill: 80; Extra: 12; Spawn: spRim; Pu: ppTrigger),
    // An empty field: no solid blocks and no bricks. The launcher falls back to it when there is no BASIC.SCH (ResolveSchemeFile)
    (FileName: 'CLEAR'; Name: 'Clear (empty field)'; Family: fmOpen; Sym: sym4; Density: 0; Fill: 0; Extra: 0; Spawn: spRim; Pu: ppNormal)
    );

  PuNames: Array[0..12] Of String = (
    'an extra bomb', 'longer flame length', 'a disease', 'the ability to kick bombs',
    'extra speed', 'the ability to punch bombs', 'the ability to grab bombs',
    'the spooger', 'goldflame', 'a trigger mechanism', 'jelly (bouncy) bombs',
    'super bad disease', 'random');

  // Start positions: pairs (2k, 2k+1) are 180 degree symmetric and in different teams
  SpawnPos: Array[TSpawnSet, 0..9, 0..1] Of Integer = (
    ((0, 0), (14, 10), (14, 0), (0, 10), (7, 0), (7, 10), (0, 5), (14, 5), (4, 10), (10, 0)),
    ((0, 0), (14, 10), (14, 0), (0, 10), (6, 2), (8, 8), (2, 6), (12, 4), (10, 2), (4, 8)),
    ((0, 1), (14, 9), (0, 9), (14, 1), (0, 5), (14, 5), (0, 3), (14, 7), (0, 7), (14, 3))
    );

Function Rep(Sym: TSym; x, y: Integer): Integer; // index of the representative of the orbit
Begin
  Case Sym Of
    sym4: Begin
        If x > W - 1 - x Then x := W - 1 - x;
        If y > H - 1 - y Then y := H - 1 - y;
      End;
    symX: If x > W - 1 - x Then x := W - 1 - x;
    sym2: If (y > H - 1 - y) Or ((y = H - 1 - y) And (x > W - 1 - x)) Then Begin
        x := W - 1 - x;
        y := H - 1 - y;
      End;
  End;
  result := y * W + x;
End;

Procedure Symmetrize(Var g: TGrid; Sym: TSym);
Var
  x, y, r: Integer;
Begin
  For x := 0 To W - 1 Do
    For y := 0 To H - 1 Do Begin
      r := Rep(Sym, x, y);
      g[x, y] := g[r Mod W, r Div W];
    End;
End;

Function Pct(Var rng: TAGRandom; p: Integer): Boolean; // integer only
Begin
  result := Integer(rng.Next Mod 100) < p;
End;

Function IAbs(v: Integer): Integer;
Begin
  If v < 0 Then result := -v Else result := v;
End;

(* Solid cells of the family, before symmetrization *)
Procedure BuildSolids(Const d: TSchemeDef; Var g: TGrid; Var rng: TAGRandom);
Var
  x, y, dx, dy: Integer;
  s, b: Boolean;
Begin
  For y := 0 To H - 1 Do
    For x := 0 To W - 1 Do Begin
      dx := IAbs(x - 7);
      dy := IAbs(y - 5);
      s := false;
      b := false;
      Case d.Family Of
        fmPillars: s := odd(x) And odd(y);
        fmSparse: s := (x Mod 4 = 1) And (y Mod 4 = 1) Or (x = 7) And (y = 5);
        fmRandom: s := odd(x) And odd(y) And Pct(rng, 100 - d.Extra) Or (odd(x) Xor odd(y)) And Pct(rng, d.Extra);
        fmPillarsPlus: s := odd(x) And odd(y) Or (odd(x) Or odd(y)) And Pct(rng, d.Extra);
        fmCross: s := odd(x) And odd(y) And Not ((dx <= 1) Or (dy <= 1)) Or (x = 7) And (y >= 2) And (y <= 8) And (dy <> 2) Or (y = 5) And (x >= 3) And (x <= 11) And (dx <> 2);
        fmTwinRooms: s := ((x = 2) Or (x = 6)) And (y >= 2) And (y <= 8) And (dy <> 0) Or ((y = 2) Or (y = 8)) And (x >= 2) And (x <= 6) And (x <> 4) Or odd(x) And odd(y) And (dx >= 6);
        fmDiamond: Begin
          s := (dx + dy = 6) And (dx <> 0) And (dy <> 0) And (dx <> 6) Or (dx = 1) And (dy = 1) Or odd(x) And odd(y) And (dx + dy >= 9);
        End;
        fmLanes: s := ((y = 3) Or (y = 7)) And (x Mod 4 <> 3) Or (y = 1) And odd(x) And (dx >= 3) Or (y = 5) And (dx = 3);
        fmFrame: Begin
          s := (dx <= 2) And (dy <= 2) And ((dx = 2) Or (dy = 2)) And (dx <> 0) And (dy <> 0) Or (dx = 0) And (dy = 0) Or odd(x) And odd(y) And (dx >= 4) Or (dx = 5) And (dy = 0);
          b := (dx <= 1) And (dy <= 1) And Not s;
        End;
        fmColumns: s := ((x = 3) Or (x = 11)) And (y Mod 3 <> 2) Or odd(x) And odd(y) And (x >= 5) And (x <= 9) And (dy <> 0);
        fmPinwheel: Begin
          s := (y = 4) And (x >= 2) And (x <= 6) Or (x = 8) And (y >= 1) And (y <= 3) Or (dx = 0) And (dy = 0) Or odd(x) And odd(y) And (dx >= 5) And (dy >= 2);
        End;
        fmBunkers: s := (x Mod 5 In [1, 2]) And (y Mod 4 In [1, 2]) And Not ((dx = 0) And (dy = 0));
        fmHighway: Begin
          s := odd(x) And ((y = 2) Or (y = 4) Or (y = 6) Or (y = 8));
          b := (y = 3) Or (y = 7);
        End;
        fmStagger: Begin
          s := odd(y) And ((x + (y Div 2) * 2) Mod 4 In [1, 2]) And (x > 0) And (x < W - 1);
        End;
        fmArena: Begin
          s := (dx <= 4) And (dy <= 3) And ((dx = 4) Or (dy = 3)) And (dx <> 0) And (dy <> 0) Or odd(x) And odd(y) And ((dx >= 6) Or (dy >= 5));
          b := (dx <= 3) And (dy <= 2);
        End;
        fmZigzag: Begin
          s := (y Mod 4 = 2) And (((x + y) Mod 6) In [0, 1, 2]) Or (y Mod 4 = 0) And (y > 0) And (y < H - 1) And (((x + y + 3) Mod 6) In [0, 1]);
        End;
        fmCourtyards: Begin
          s := ((x = 2) Or (x = 6)) And (y >= 1) And (y <= 4) And (y <> 3) Or ((y = 1) Or (y = 4)) And (x >= 2) And (x <= 6) And (x <> 4) Or (dx = 0) And (dy = 0);
          b := (x >= 3) And (x <= 5) And (y >= 2) And (y <= 3);
        End;
      End;
      If s Then g[x, y] := '#' Else If b Then g[x, y] := '.' Else g[x, y] := ':';
    End;
End;

Function Connected(Const g: TGrid; sx, sy: Integer): Boolean; // all non solid cells reachable from (sx, sy)
Var
  seen: Array[0..W * H - 1] Of Boolean;
  stack: Array[0..W * H - 1] Of Integer;
  sp, c, x, y, i, n: Integer;
Const
  DX: Array[0..3] Of Integer = (1, -1, 0, 0);
  DY: Array[0..3] Of Integer = (0, 0, 1, -1);
Begin
  FillChar(seen, SizeOf(seen), 0);
  stack[0] := sy * W + sx;
  seen[stack[0]] := true;
  sp := 1;
  n := 1;
  While sp > 0 Do Begin
    dec(sp);
    c := stack[sp];
    For i := 0 To 3 Do Begin
      x := c Mod W + DX[i];
      y := c Div W + DY[i];
      If (x < 0) Or (y < 0) Or (x >= W) Or (y >= H) Then Continue;
      If seen[y * W + x] Or (g[x, y] = '#') Then Continue;
      seen[y * W + x] := true;
      stack[sp] := y * W + x;
      inc(sp);
      inc(n);
    End;
  End;
  For y := 0 To H - 1 Do
    For x := 0 To W - 1 Do
      If (g[x, y] <> '#') And Not seen[y * W + x] Then exit(false);
  result := true;
End;

Function BuildScheme(Index: Integer): TStringArray;
Const
  NX: Array[0..4] Of Integer = (0, 1, -1, 0, 0);
  NY: Array[0..4] Of Integer = (0, 0, 0, 1, -1);
Var
  d: TSchemeDef;
  g: TGrid;
  clear: Array[0..W - 1, 0..H - 1] Of Boolean;
  rng: TAGRandom;
  attempt, x, y, i, k, p, r: Integer;
  born, ovr, ovrval, forb: Array[0..12] Of Integer;
  lines: Array Of String;

  Procedure Add(Const s: String);
  Begin
    setlength(lines, length(lines) + 1);
    lines[high(lines)] := s;
  End;

Begin
  d := Defs[Index];
  rng.Init(SchemeSeed, Index + 1);
  attempt := 0;
  Repeat
    BuildSolids(d, g, rng);
    Symmetrize(g, d.Sym);
    // bricks
    For y := 0 To H - 1 Do
      For x := 0 To W - 1 Do
        If (g[x, y] = ':') And Not Pct(rng, d.Fill) Then g[x, y] := '.';
    Symmetrize(g, d.Sym);
    // start cells and their neighbours are free, the mask is symmetrized so the layout stays symmetric
    FillChar(clear, SizeOf(clear), 0);
    For i := 0 To 9 Do
      For k := 0 To 4 Do Begin
        x := SpawnPos[d.Spawn, i, 0] + NX[k];
        y := SpawnPos[d.Spawn, i, 1] + NY[k];
        If (x < 0) Or (y < 0) Or (x >= W) Or (y >= H) Then Continue;
        clear[x, y] := true;
      End;
    For y := 0 To H - 1 Do
      For x := 0 To W - 1 Do
        If clear[x, y] Then Begin
          r := Rep(d.Sym, x, y);
          clear[r Mod W, r Div W] := true;
        End;
    For y := 0 To H - 1 Do
      For x := 0 To W - 1 Do Begin
        r := Rep(d.Sym, x, y);
        If clear[r Mod W, r Div W] Then g[x, y] := '.';
      End;
    inc(attempt);
    If attempt > 50 Then Raise Exception.Create('Scheme ' + d.FileName + ': no connected layout');
  Until Connected(g, SpawnPos[d.Spawn, 0, 0], SpawnPos[d.Spawn, 0, 1]);

  // power ups
  For p := 0 To 12 Do Begin
    born[p] := 0;
    ovr[p] := 0;
    ovrval[p] := 0;
    forb[p] := 0;
  End;
  Case d.Pu Of
    ppQuick: Begin
        born[3] := 1; // kick
        born[5] := 1; // punch
        born[4] := 1; // speed
      End;
    ppBigBang: Begin
        born[0] := 3; // bombs
        born[1] := 2; // flame
        forb[2] := 1;
      End;
    ppHealthy: Begin
        forb[2] := 1;
        forb[11] := 1;
        born[6] := 1; // grab
      End;
    ppJelly: Begin
        born[10] := 1;
        ovr[10] := 1;
        ovrval[10] := 2;
      End;
    ppPurist: Begin
        For p := 3 To 12 Do
          If p <> 4 Then forb[p] := 1;
      End;
    ppTrigger: Begin
        born[9] := 1;
        born[0] := 1;
        forb[7] := 1;
      End;
  End;

  lines := Nil;
  Add('; FPC Atomic scheme file, computed by the asset generator.');
  Add('; Free to use and to change. Lines starting with ; are comments.');
  Add('');
  Add('; file format version');
  Add('-V,2');
  Add('');
  Add('; name of the scheme');
  Add('-N,' + d.Name);
  Add('');
  Add('; chance (percent) that a brick cell really gets a brick');
  Add('-B,' + IntToStr(d.Density));
  Add('');
  Add('; field: # solid, : brick, . free');
  Add(';               11111');
  Add(';     012345678901234');
  For y := 0 To H - 1 Do Begin
    Add(Format('-R,%2d,', [y]));
    For x := 0 To W - 1 Do
      lines[high(lines)] := lines[high(lines)] + g[x, y];
  End;
  Add('');
  Add('; start positions (player, x, y, team)');
  For i := 0 To 9 Do
    Add(Format('-S,%d,%d,%d,%d', [i, SpawnPos[d.Spawn, i, 0], SpawnPos[d.Spawn, i, 1], i Mod 2]));
  Add('');
  Add('; power ups (number, born with, has override, override value, forbidden, comment)');
  For p := 0 To 12 Do
    Add(Format('-P,%2d,%2d,%d,%2d,%2d,%s', [p, born[p], ovr[p], ovrval[p], forb[p], PuNames[p]]));
  result := lines;
End;

Procedure GenerateSchemes(Ctx: TAGContext; Output: TAGOutput);
Var
  i: Integer;
Begin
  // Ctx is not used on purpose, see the header
  For i := 0 To SchemeCount - 1 Do
    Output.SaveText('schemes/' + Defs[i].FileName + '.SCH', BuildScheme(i));
End;

Initialization
  RegisterGenerator('schemes', 'Schemes (schemes/*.SCH, independent of seed and style)', SchemeCount, @GenerateSchemes);

End.
