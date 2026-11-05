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
 * The smart ai, based on the ai of Corpsman (ai/uatomicai.pas), see README.md.
 *
 * Every frame (10 ms):
 *  1. Danger map: for every field the time in ms until flames reach it
 *     (bombs with their remaining time, chain reactions, flames, ill enemies,
 *     hurry). Safe = no known danger.
 *  2. Paths: breadth first search from the own field, a field may only be
 *     entered if it does not explode while the ai is on it.
 *  3. Escape if the own field is in danger.
 *  4. Otherwise goals with a score (bricks, enemies, powerups, ...), the best
 *     one whose bomb still leaves an escape route is taken.
 *  5. Special actions (remote trigger, kick, punch, grab, spooger).
 *
 * Abilities are switched by ai_smart.ini (usmartsettings).
 *)
Unit uatomicai_smart;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uai_types, usmartsettings;

Const
  (*
   * History:
   *             0.01 - Initial version, based on "Atomic ai ver. 0.08 by Corpsman"
   *             0.02 - ADD: [General] Difficulty 0..100 (default 50) sets the chances, reaction, mistakes and remote trigger time
   *                    FIX: shaking (centring, goal switching, turning for spooger / grab, escape at the border of a field)
   *)
  Ai_Version = 'Atomic smart ai ver. 0.02';

  FieldWidth = 15;
  FieldHeight = 11;

  BombDetonateTime = 2000; // AtomicBombDetonateTime of the game
  HurryStartMs = 80000; // the hurry starts with this round time left
  HurryStepMs = 500; // one solid brick every 500 ms
  ThrowMs = 500; // AtomicBombBigFlyTime, a thrown bomb lands after this time
  GameFrameRate = 10; // FrameRate of the game, the ai is called every frame

  Safe = High(integer); // no danger known on this field
  NotReachable = High(integer);

  GoodPowerUps = [fExtraBomb, fLongerFlame, fGoldflame, fExtraSpeed, fKick, fSpooger, fPunch, fGrab, fTrigger, fJelly];
  BadPowerUps = [fRandom, fSlow, fDisease, fBadDisease];
  WalkableFields = [fBlank, fHole, fTramp, fConveyorUp, fConveyorDown, fConveyorLeft, fConveyorRight, fArrowUp, fArrowDown, fArrowLeft, fArrowRight];
  BombPlaceableFields = [fBlank, fConveyorUp, fConveyorDown, fConveyorLeft, fConveyorRight, fArrowUp, fArrowDown, fArrowLeft, fArrowRight];

Type
  PAiInfo = ^TAiInfo;
  TIntGrid = Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of integer;
  TBoolGrid = Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Boolean;
  TMoveGrid = Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of TAiMoveState;

  TGoalKind = (gkNone, gkEscape, gkBomb, gkCollect, gkGoto);

  TGoal = Record
    x, y: integer;
    Kind: TGoalKind;
    Score: integer;
    Name: String;
  End;

  TBombData = Record
    x, y, Len, Owner: integer;
    Time: integer; // ms until it explodes (after chain reactions)
    Manual: Boolean;
  End;

  TPointList = Array Of TPoint;

  { TSmartAi }

  TSmartAi = Class
  private
    fIndex: integer;
    fInfo: PAiInfo;
    fGameMode: Cardinal;
    fHunted: integer;
    fRoundTimeLeft: integer;
    fTime: Int64; // ms since the start of the round (10 ms per call)
    fRandom: QWord;
    fX, fY: integer;
    fMsPerField: integer;
    fFacing: TAiMoveState; // the direction the player looks (last move)
    fBombs: Array Of TBombData;
    fBombAt: TIntGrid; // index into fBombs or -1
    fDanger: TIntGrid;
    fDist: TIntGrid;
    fFirst: TMoveGrid;
    fQueue: Array[0..FieldWidth * FieldHeight - 1] Of TPoint;
    fRollActive: Array[TSmartFeature] Of Boolean;
    fRollUntil: Array[TSmartFeature] Of Int64;
    fNextDecision: Int64;
    fDecisionCell: TPoint; // the field of the last decision, the reaction delay only holds within it
    fLastCommand: TAiCommand;
    fLastGoal: String;
    fEnemyLine: TBoolGrid; // Anticipate: fields in the flame lines of enemies with bombs
    fBombTries: integer; // apFirst on the same field without a bomb appearing
    fBombTryCell, fNoBombCell: TPoint;
    fNoBombUntil: Int64;
    fGoal: TGoal; // the goal of the last decision, it gets a bonus so that the ai does not switch back and forth
    fEscaping: Boolean; // the escape started and goes on until the ai stands on a safe field
    // Only for the log (LogDecisions): the last state, written as death report at the next round if the ai died
    fSeenTime: Int64;
    fSeenX, fSeenY, fSeenDanger: integer;
    fSeenOwnBomb, fSeenEscaping: Boolean;
    fStuckCell: TPoint;
    fStuckSince: Int64;
    fStuckLogged: Boolean;
    fLastMove: TAiMoveState; // for the log of steps back (LogDecisions)
    fLastMoveTime: Int64;

    Procedure LogState(Const Cmd: TAiCommand);

    Function IsLastGoal(x, y: integer; Kind: TGoalKind): Boolean;

    Function Rnd(Range: integer): integer;
    Function Roll(f: TSmartFeature): Boolean;
    Function Me: TAiPlayerInfo;
    Function Inside(x, y: integer): Boolean;
    Function FieldAt(x, y: integer): TAiField;
    Function IsEnemy(i: integer): Boolean;
    Function IsFriend(i: integer): Boolean;
    Function PlayerField(i: integer): TPoint;
    Function Usable(i: integer): Boolean; // alive, on the field, not flying

    Procedure FlameCells(x, y, Len: integer; Var Grid: TBoolGrid);
    Procedure BuildBombs;
    Procedure BuildDanger;
    Procedure BuildEnemyLines;
    Function Enterable(Const Danger: TIntGrid; x, y, ArriveMs: integer; Timed: Boolean): Boolean;
    Procedure Search(sx, sy, StartMs: integer; Const Danger: TIntGrid; BombsBlock, Timed, AllowBad: Boolean;
      Out Dist: TIntGrid; Out First: TMoveGrid);
    Function FreeNeighbours(x, y: integer): integer;
    Function CanSurviveBombs(Const Cells: TPointList; StartX, StartY, StartMs: integer; Out HitFriend: Boolean): Boolean;
    Function CanBombAt(x, y, Steps: integer): Boolean;
    Function CountBrickHits(x, y: integer; Const Bricks: TBoolGrid; Out GoodBurnt: integer): integer;
    Function EnemiesHit(x, y: integer; Out FreeAround: integer): integer;

    Function EscapeGoal(Out Move: TAiMoveState): Boolean;
    Function ChooseGoal(): TGoal;
    Function TriggerNow(): Boolean;
    Function SpecialAction(Var Cmd: TAiCommand): Boolean;
    Function MoveTo(tx, ty: integer): TAiMoveState;
    Function Decide(): TAiCommand;
    Procedure LogGoal(Const Name: String; x, y: integer);
  public
    Constructor Create(aIndex: integer);
    Procedure Reset(aStrength: integer);
    Function CalcAiCommand(Const AiInfo: TAiInfo; GameMode: Cardinal = 0; HuntedIndex: integer = -1;
      RoundTimeLeftMs: integer = -1): TAiCommand;
  End;

Implementation

Const
  DirX: Array[TAiMoveState] Of integer = (0, -1, 1, 0, 0);
  DirY: Array[TAiMoveState] Of integer = (0, 0, 0, -1, 1);
  Moves: Array[0..3] Of TAiMoveState = (amLeft, amRight, amUp, amDown);
  NoPoint: TPoint = (x: - 1; y: - 1);

  BrickValue = 10;
  EnemyValue = 40;
  BurnPenalty = 8;

Var
  HurryCoords: Array[0..159] Of TPoint; // the spiral of the hurry, like in units/uatomic_field.pas
  LatestAiTime: Int64 = 0; // Log: the latest round time of any ai, an ai that stopped earlier died

Procedure InitHurryCoords;
Var
  l, t, r, b, x, y, n: integer;
Begin
  // Clockwise from the top left corner inwards, until 5 fields in the middle are left
  l := 0;
  t := 0;
  r := FieldWidth - 1;
  b := FieldHeight - 1;
  n := 0;
  While n <= high(HurryCoords) Do Begin
    For x := l To r Do Begin
      If n > high(HurryCoords) Then break;
      HurryCoords[n] := Point(x, t);
      inc(n);
    End;
    For y := t + 1 To b Do Begin
      If n > high(HurryCoords) Then break;
      HurryCoords[n] := Point(r, y);
      inc(n);
    End;
    For x := r - 1 Downto l Do Begin
      If n > high(HurryCoords) Then break;
      HurryCoords[n] := Point(x, b);
      inc(n);
    End;
    For y := b - 1 Downto t + 1 Do Begin
      If n > high(HurryCoords) Then break;
      HurryCoords[n] := Point(l, y);
      inc(n);
    End;
    inc(l);
    inc(t);
    dec(r);
    dec(b);
  End;
End;

{ TSmartAi }

Procedure TSmartAi.LogState(Const Cmd: TAiCommand);
Var
  k: integer;
  g: TBoolGrid;
Begin
  // Remember the state for the death report (see Reset)
  If fTime > LatestAiTime Then LatestAiTime := fTime;
  fSeenTime := fTime;
  fSeenX := fX;
  fSeenY := fY;
  fSeenDanger := fDanger[fX, fY];
  fSeenEscaping := fEscaping;
  fSeenOwnBomb := false;
  For k := 0 To high(fBombs) Do
    If fBombs[k].Owner = fIndex Then Begin
      FillChar(g, SizeOf(g), 0);
      FlameCells(fBombs[k].x, fBombs[k].y, fBombs[k].Len, g);
      If g[fX, fY] Then fSeenOwnBomb := true;
    End;
  // Stuck: the ai wants to walk, but stays on the same field for more than 3 s
  If (Cmd.MoveState <> amNone) And (fStuckCell.x = fX) And (fStuckCell.y = fY) Then Begin
    If (Not fStuckLogged) And (fTime - fStuckSince > 3000) Then Begin
      SmartLog(format('P%d stuck on (%d,%d) for 3 s, pos %.2f/%.2f, goal: %s', [fIndex, fX, fY, Me.Position.x, Me.Position.y, fLastGoal]));
      fStuckLogged := true;
    End;
  End
  Else Begin
    fStuckCell := Point(fX, fY);
    fStuckSince := fTime;
    fStuckLogged := false;
  End;
End;

Constructor TSmartAi.Create(aIndex: integer);
Begin
  Inherited Create;
  fIndex := aIndex;
  Reset(100);
End;

Procedure TSmartAi.Reset(aStrength: integer);
Var
  f: TSmartFeature;
Begin
  // aStrength: the server always sends 100 at the moment, the settings come from ai_smart.ini
  // Log: the server stops calling a dead ai, so a death shows up as an ai that went silent before the round ended
  If Settings.LogDecisions And (fSeenTime > 0) Then Begin
    If fSeenTime < LatestAiTime - 100 Then
      SmartLog(format('P%d died at t=%d on (%d,%d), last goal: %s, escaping: %s, danger there: %s, own bomb hit that field: %s',
        [fIndex, fSeenTime, fSeenX, fSeenY, fLastGoal, BoolToStr(fSeenEscaping, 'yes', 'no'),
        BoolToStr(fSeenDanger = Safe, 'none', IntToStr(fSeenDanger) + ' ms'), BoolToStr(fSeenOwnBomb, 'YES (suicide?)', 'no')]))
    Else
      SmartLog(format('P%d survived the round (t=%d)', [fIndex, fSeenTime]));
  End;
  fSeenTime := 0;
  If fIndex = 9 Then LatestAiTime := 0; // AiNewRound resets the ais 0..9 in this order
  fStuckLogged := false;
  fStuckSince := 0;
  fStuckCell := Point(-1, -1);
  fTime := 0;
{$PUSH}{$Q-}{$R-} // the seed is mixed by multiplication, the overflow is wanted
  If Settings.Seed <> 0 Then Begin
    fRandom := Settings.Seed * 6364136223846793005 + QWord(fIndex + 1) * 1442695040888963407;
  End
  Else Begin
    fRandom := QWord(GetTickCount64) * 6364136223846793005 + QWord(fIndex + 1) * 1442695040888963407;
  End;
{$POP}
  If fRandom = 0 Then fRandom := 88172645463325252;
  For f := low(TSmartFeature) To high(TSmartFeature) Do Begin
    fRollUntil[f] := -1;
    fRollActive[f] := false;
  End;
  fFacing := amDown;
  fNextDecision := 0;
  fLastCommand.Action := apNone;
  fLastCommand.MoveState := amNone;
  fLastGoal := '';
  fBombTries := 0;
  fNoBombCell := Point(-1, -1);
  fNoBombUntil := 0;
  fGoal.Kind := gkNone;
  fGoal.x := -1;
  fGoal.y := -1;
  fLastMove := amNone;
  fLastMoveTime := 0;
  fEscaping := false;
End;

Function TSmartAi.IsLastGoal(x, y: integer; Kind: TGoalKind): Boolean;
Begin
  result := (fGoal.Kind = Kind) And (fGoal.x = x) And (fGoal.y = y);
End;

Function TSmartAi.Rnd(Range: integer): integer;
Begin
  // xorshift64, own generator: reproducible with a seed and independent of the server
  fRandom := fRandom Xor (fRandom Shl 13);
  fRandom := fRandom Xor (fRandom Shr 7);
  fRandom := fRandom Xor (fRandom Shl 17);
  If Range <= 0 Then exit(0);
  result := integer(fRandom Mod QWord(Range));
End;

Function TSmartAi.Roll(f: TSmartFeature): Boolean;
Begin
  // Rolled once and kept for RerollMs, rolling every frame would make every chance > 0 almost 100 %
  If Settings.Chance[f] <= 0 Then exit(false);
  If Settings.Chance[f] >= 100 Then exit(true);
  If fTime >= fRollUntil[f] Then Begin
    fRollActive[f] := Rnd(100) < Settings.Chance[f];
    fRollUntil[f] := fTime + Settings.RerollMs;
  End;
  result := fRollActive[f];
End;

Function TSmartAi.Me: TAiPlayerInfo;
Begin
  result := fInfo^.PlayerInfos[fIndex];
End;

Function TSmartAi.Inside(x, y: integer): Boolean;
Begin
  result := (x >= 0) And (y >= 0) And (x < FieldWidth) And (y < FieldHeight);
End;

Function TSmartAi.FieldAt(x, y: integer): TAiField;
Begin
  If Inside(x, y) Then
    result := fInfo^.Field[x, y]
  Else
    result := fSolid;
End;

Function TSmartAi.IsEnemy(i: integer): Boolean;
Begin
  result := false;
  If (i = fIndex) Or (i < 0) Or (i > high(fInfo^.PlayerInfos)) Then exit;
  If fGameMode = AiMode_Hunter Then Begin
    If fIndex = fHunted Then
      result := true // the hunted fights against everybody
    Else
      result := i = fHunted; // hunters only hunt the hunted
  End
  Else Begin
    result := (Not fInfo^.Teamplay) Or (fInfo^.PlayerInfos[i].Team <> Me.Team);
  End;
End;

Function TSmartAi.IsFriend(i: integer): Boolean;
Begin
  result := (i <> fIndex) And (i >= 0) And (i <= high(fInfo^.PlayerInfos)) And Not IsEnemy(i);
End;

Function TSmartAi.PlayerField(i: integer): TPoint;
Begin
  result := Point(trunc(fInfo^.PlayerInfos[i].Position.x), trunc(fInfo^.PlayerInfos[i].Position.y));
End;

Function TSmartAi.Usable(i: integer): Boolean;
Var
  p: TPoint;
Begin
  result := false;
  If Not fInfo^.PlayerInfos[i].Alive Then exit;
  If fInfo^.PlayerInfos[i].Flying Then exit;
  p := PlayerField(i);
  result := Inside(p.x, p.y);
End;

Procedure TSmartAi.FlameCells(x, y, Len: integer; Var Grid: TBoolGrid);
Var
  d, i, cx, cy: integer;
  f: TAiField;
Begin
  // Like TAtomicField.Detonate: flames run over everything walkable, a brick or powerup is hit and stops them, a solid stops them
  If Not Inside(x, y) Then exit;
  Grid[x, y] := true;
  For d := 0 To 3 Do Begin
    cx := x;
    cy := y;
    For i := 1 To Len Do Begin
      cx := cx + DirX[Moves[d]];
      cy := cy + DirY[Moves[d]];
      If Not Inside(cx, cy) Then break;
      f := fInfo^.Field[cx, cy];
      If f = fSolid Then break;
      Grid[cx, cy] := true;
      If Not (f In WalkableFields + [fFlame]) Then break; // brick or powerup
    End;
  End;
End;

Procedure TSmartAi.BuildBombs;
Var
  i, j, n, t: integer;
  Changed: Boolean;
  Grids: Array Of TBoolGrid;
  b: TAiBombInfo;
Begin
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do
      fBombAt[i, j] := -1;
  n := 0;
  setlength(fBombs, fInfo^.BombsCount);
  For i := 0 To fInfo^.BombsCount - 1 Do Begin
    b := fInfo^.Bombs[i];
    If b.Flying Then Continue; // flying bombs do not detonate
    fBombs[n].x := trunc(b.Position.x);
    fBombs[n].y := trunc(b.Position.y);
    If Not Inside(fBombs[n].x, fBombs[n].y) Then Continue;
    fBombs[n].Len := b.FlameLength;
    fBombs[n].Owner := b.Owner;
    fBombs[n].Manual := b.ManualTrigger;
    If Not Roll(sfTimedDanger) Then Begin
      t := 0; // old behaviour: every bomb is dangerous at once
    End
    Else If b.DudBomb Then Begin
      t := 0; // unknown time
    End
    Else If b.ManualTrigger Then Begin
      If b.Owner = fIndex Then
        t := Settings.MaxWaitMs - b.LifeTime // we decide ourselves (TriggerNow only triggers if we are safe)
      Else
        t := 0; // the owner can trigger it any time
    End
    Else Begin
      t := BombDetonateTime - b.LifeTime;
    End;
    If t < 0 Then t := 0;
    fBombs[n].Time := t;
    fBombAt[fBombs[n].x, fBombs[n].y] := n;
    inc(n);
  End;
  setlength(fBombs, n);
  // Chain reactions: a bomb in the flames of an earlier one explodes with it
  setlength(Grids, n);
  For i := 0 To n - 1 Do Begin
    FillChar(Grids[i], SizeOf(TBoolGrid), 0);
    FlameCells(fBombs[i].x, fBombs[i].y, fBombs[i].Len, Grids[i]);
  End;
  Repeat
    Changed := false;
    For i := 0 To n - 1 Do
      For j := 0 To n - 1 Do
        If (i <> j) And Grids[i][fBombs[j].x, fBombs[j].y] And (fBombs[i].Time < fBombs[j].Time) Then Begin
          fBombs[j].Time := fBombs[i].Time;
          Changed := true;
        End;
  Until Not Changed;
End;

Procedure TSmartAi.BuildDanger;
Var
  i, j, k, t, Elapsed: integer;
  g: TBoolGrid;
  p: TPoint;
Begin
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do
      If fInfo^.Field[i, j] = fFlame Then
        fDanger[i, j] := 0
      Else
        fDanger[i, j] := Safe;
  For k := 0 To high(fBombs) Do Begin
    FillChar(g, SizeOf(g), 0);
    FlameCells(fBombs[k].x, fBombs[k].y, fBombs[k].Len, g);
    For i := 0 To FieldWidth - 1 Do
      For j := 0 To FieldHeight - 1 Do
        If g[i, j] And (fBombs[k].Time < fDanger[i, j]) Then fDanger[i, j] := fBombs[k].Time;
  End;
  // Ill enemies are avoided (a disease is never good), unless the ai is ill itself
  If Not Me.IsIll Then Begin
    For i := 0 To high(fInfo^.PlayerInfos) Do Begin
      If (i = fIndex) Or (Not Usable(i)) Or (Not fInfo^.PlayerInfos[i].IsIll) Then Continue;
      p := PlayerField(i);
      fDanger[p.x, p.y] := 0;
    End;
  End;
  // Hurry: the next fields of the spiral are walled up, whoever stands there dies.
  // Some modes have no hurry (hunter with respawn): once the first brick is due but missing, nothing is predicted.
  Elapsed := HurryStartMs - fRoundTimeLeft;
  If Roll(sfRoundEnd) And (fRoundTimeLeft >= 0) And
    ((Elapsed < HurryStepMs) Or (fInfo^.Field[HurryCoords[0].x, HurryCoords[0].y] = fSolid)) Then Begin
    For k := 0 To high(HurryCoords) Do Begin
      t := k * HurryStepMs - Elapsed;
      If t > Settings.HurryLookAheadMs Then break;
      If t < 0 Then t := 0;
      p := HurryCoords[k];
      If t < fDanger[p.x, p.y] Then fDanger[p.x, p.y] := t;
    End;
  End;
End;

Procedure TSmartAi.BuildEnemyLines;
Var
  i: integer;
  p: TPoint;
Begin
  FillChar(fEnemyLine, SizeOf(fEnemyLine), 0);
  If Not Roll(sfAnticipate) Then exit;
  For i := 0 To high(fInfo^.PlayerInfos) Do Begin
    If (Not IsEnemy(i)) Or (Not Usable(i)) Then Continue;
    If fInfo^.PlayerInfos[i].AvailableBombs <= 0 Then Continue;
    p := PlayerField(i);
    FlameCells(p.x, p.y, fInfo^.PlayerInfos[i].FlameLength, fEnemyLine);
  End;
End;

Function TSmartAi.Enterable(Const Danger: TIntGrid; x, y, ArriveMs: integer; Timed: Boolean): Boolean;
Begin
  // The field must not explode while the ai walks over it
  If Not Timed Then exit(true);
  result := (Danger[x, y] = Safe) Or (Danger[x, y] > ArriveMs + fMsPerField + Settings.SafetyMs);
End;

Procedure TSmartAi.Search(sx, sy, StartMs: integer; Const Danger: TIntGrid; BombsBlock, Timed,
  AllowBad: Boolean; Out Dist: TIntGrid; Out First: TMoveGrid);
Var
  Head, Tail, d, nx, ny, nd: integer;
  p: TPoint;
  Allowed: Set Of TAiField;
  f: TAiField;
  Push: TPoint;
  Conveyors: Boolean;
Begin
  Conveyors := Roll(sfConveyors);
  For nx := 0 To FieldWidth - 1 Do
    For ny := 0 To FieldHeight - 1 Do Begin
      Dist[nx, ny] := NotReachable;
      First[nx, ny] := amNone;
    End;
  If Not Inside(sx, sy) Then exit;
  Allowed := WalkableFields + GoodPowerUps;
  If AllowBad Then Allowed := Allowed + BadPowerUps;
  Dist[sx, sy] := 0;
  Head := 0;
  Tail := 0;
  fQueue[Tail] := Point(sx, sy);
  inc(Tail);
  While Head < Tail Do Begin
    p := fQueue[Head];
    inc(Head);
    For d := 0 To 3 Do Begin
      nx := p.x + DirX[Moves[d]];
      ny := p.y + DirY[Moves[d]];
      If Not Inside(nx, ny) Then Continue;
      If Dist[nx, ny] <> NotReachable Then Continue;
      f := fInfo^.Field[nx, ny];
      If Not (f In Allowed) Then Continue;
      If BombsBlock And (fBombAt[nx, ny] <> -1) Then Continue;
      nd := Dist[p.x, p.y] + 1;
      If Not Enterable(Danger, nx, ny, StartMs + nd * fMsPerField, Timed) Then Continue;
      // A conveyor that pushes into a dangerous field is avoided
      If Conveyors And (f In [fConveyorUp, fConveyorDown, fConveyorLeft, fConveyorRight]) Then Begin
        Case f Of
          fConveyorUp: Push := Point(nx, ny - 1);
          fConveyorDown: Push := Point(nx, ny + 1);
          fConveyorLeft: Push := Point(nx - 1, ny);
        Else
          Push := Point(nx + 1, ny);
        End;
        If Inside(Push.x, Push.y) And (Danger[Push.x, Push.y] <> Safe) And
          (Danger[Push.x, Push.y] <= StartMs + (nd + 1) * fMsPerField + Settings.SafetyMs) Then Continue;
      End;
      Dist[nx, ny] := nd;
      If (p.x = sx) And (p.y = sy) Then
        First[nx, ny] := Moves[d]
      Else
        First[nx, ny] := First[p.x, p.y];
      fQueue[Tail] := Point(nx, ny);
      inc(Tail);
    End;
  End;
End;

Function TSmartAi.FreeNeighbours(x, y: integer): integer;
Var
  d, nx, ny: integer;
Begin
  result := 0;
  For d := 0 To 3 Do Begin
    nx := x + DirX[Moves[d]];
    ny := y + DirY[Moves[d]];
    If Inside(nx, ny) And (fInfo^.Field[nx, ny] In WalkableFields + GoodPowerUps) And (fBombAt[nx, ny] = -1) Then inc(result);
  End;
End;

Function TSmartAi.CanSurviveBombs(Const Cells: TPointList; StartX, StartY, StartMs: integer; Out HitFriend: Boolean): Boolean;
Var
  Danger: TIntGrid;
  g: TBoolGrid;
  Dist: TIntGrid;
  First: TMoveGrid;
  i, j, k, NewTime: integer;
  Timed: Boolean;
  p: TPoint;
Begin
  (*
   * New bombs on Cells (placed after StartMs): is there still a field that stays safe and that the ai
   * reaches in time from StartX, StartY? The new bombs explode with the earliest bomb whose flames reach them.
   *)
  result := false;
  HitFriend := false;
  If length(Cells) = 0 Then exit;
  Timed := Roll(sfTimedDanger);
  NewTime := StartMs + BombDetonateTime;
  If Not Timed Then NewTime := 0;
  For k := 0 To high(Cells) Do
    If (fDanger[Cells[k].x, Cells[k].y] <> Safe) And (fDanger[Cells[k].x, Cells[k].y] < NewTime) Then
      NewTime := fDanger[Cells[k].x, Cells[k].y];
  FillChar(g, SizeOf(g), 0);
  For k := 0 To high(Cells) Do
    FlameCells(Cells[k].x, Cells[k].y, Me.FlameLength, g);
  // Bombs in the new flames explode with them
  For k := 0 To high(fBombs) Do
    If g[fBombs[k].x, fBombs[k].y] And (fBombs[k].Time > NewTime) Then
      FlameCells(fBombs[k].x, fBombs[k].y, fBombs[k].Len, g);
  Danger := fDanger;
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do
      If g[i, j] And (NewTime < Danger[i, j]) Then Danger[i, j] := NewTime;
  For i := 0 To high(fInfo^.PlayerInfos) Do
    If IsFriend(i) And Usable(i) Then Begin
      p := PlayerField(i);
      If g[p.x, p.y] Then HitFriend := true;
    End;
  // The new bombs block the way (the ai may leave the field it stands on), marked for the search only
  For k := 0 To high(Cells) Do
    If fBombAt[Cells[k].x, Cells[k].y] = -1 Then fBombAt[Cells[k].x, Cells[k].y] := -2;
  Search(StartX, StartY, StartMs, Danger, true, Timed, false, Dist, First);
  For k := 0 To high(Cells) Do
    If fBombAt[Cells[k].x, Cells[k].y] = -2 Then fBombAt[Cells[k].x, Cells[k].y] := -1;
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do
      If (Dist[i, j] <> NotReachable) And (Danger[i, j] = Safe) Then exit(true);
End;

Function TSmartAi.CanBombAt(x, y, Steps: integer): Boolean;
Var
  Cells: TPointList;
  HitFriend: Boolean;
Begin
  Cells := Nil;
  setlength(Cells, 1);
  Cells[0] := Point(x, y);
  result := CanSurviveBombs(Cells, x, y, Steps * fMsPerField, HitFriend);
  If result And HitFriend And Roll(sfTeamSafety) Then result := false;
End;

Function TSmartAi.CountBrickHits(x, y: integer; Const Bricks: TBoolGrid; Out GoodBurnt: integer): integer;
Var
  d, i, cx, cy: integer;
  f: TAiField;
Begin
  result := 0;
  GoodBurnt := 0;
  For d := 0 To 3 Do Begin
    cx := x;
    cy := y;
    For i := 1 To Me.FlameLength Do Begin
      cx := cx + DirX[Moves[d]];
      cy := cy + DirY[Moves[d]];
      If Not Inside(cx, cy) Then break;
      f := fInfo^.Field[cx, cy];
      If f = fSolid Then break;
      If f = fBrick Then Begin
        If Bricks[cx, cy] Then inc(result); // only bricks that no other bomb destroys already
        break;
      End;
      If f In BadPowerUps Then Begin
        inc(result);
        break;
      End;
      If f In GoodPowerUps Then Begin
        inc(GoodBurnt);
        break;
      End;
    End;
  End;
End;

Function TSmartAi.EnemiesHit(x, y: integer; Out FreeAround: integer): integer;
Var
  g: TBoolGrid;
  i, k: integer;
  p: TPoint;
Begin
  result := 0;
  FreeAround := 0;
  FillChar(g, SizeOf(g), 0);
  FlameCells(x, y, Me.FlameLength, g);
  If Roll(sfChainAttack) Then
    For k := 0 To high(fBombs) Do
      If g[fBombs[k].x, fBombs[k].y] Then FlameCells(fBombs[k].x, fBombs[k].y, fBombs[k].Len, g);
  For i := 0 To high(fInfo^.PlayerInfos) Do Begin
    If (Not IsEnemy(i)) Or (Not Usable(i)) Then Continue;
    p := PlayerField(i);
    If g[p.x, p.y] Then Begin
      inc(result);
      FreeAround := FreeAround + FreeNeighbours(p.x, p.y);
    End;
  End;
End;

Function TSmartAi.EscapeGoal(Out Move: TAiMoveState): Boolean;
Var
  i, j, best, d, bx, by: integer;
  Target: TPoint;
  Dist: TIntGrid;
  First: TMoveGrid;
  Pass: integer;
Begin
  Move := amNone;
  result := false;
  If fDanger[fX, fY] = Safe Then Begin
    fEscaping := false;
    exit;
  End;
  (*
   * With the timed danger map the paths may lead over fields that explode later. Such a field is left only when
   * it gets tight (the explosion is nearer than the way to the next safe field plus a margin), otherwise the
   * goal and the escape would send the ai back and forth at the border of the field. Once the escape started,
   * it goes on until the ai stands on a safe field.
   *)
  If Roll(sfTimedDanger) And (Not fEscaping) Then Begin
    Search(fX, fY, 0, fDanger, true, true, false, Dist, First);
    best := NotReachable;
    For i := 0 To FieldWidth - 1 Do
      For j := 0 To FieldHeight - 1 Do
        If (Dist[i, j] < best) And (fDanger[i, j] = Safe) Then best := Dist[i, j];
    If (best <> NotReachable) And (fDanger[fX, fY] > (best + 2) * fMsPerField + Settings.SafetyMs) Then exit;
  End;
  fEscaping := true;
  result := true;
  (*
   * Pass 0: timed, bombs block (the best way)
   * Pass 1: untimed, bombs block (like the old ai)
   * Pass 2: panic, bombs do not block, bad powerups allowed, holes are a way out (like the old ai)
   *)
  For Pass := 0 To 2 Do Begin
    If (Pass = 0) And (Not Roll(sfTimedDanger)) Then Continue;
    Search(fX, fY, 0, fDanger, Pass < 2, Pass = 0, Pass = 2, Dist, First);
    Target := NoPoint;
    best := NotReachable;
    For i := 0 To FieldWidth - 1 Do
      For j := 0 To FieldHeight - 1 Do Begin
        If Dist[i, j] = NotReachable Then Continue;
        If (fDanger[i, j] <> Safe) And Not ((Pass = 2) And (fInfo^.Field[i, j] = fHole)) Then Continue;
        // OpenSpace: of equally near fields the one with more ways out
        d := Dist[i, j] * 8;
        If Roll(sfOpenSpace) Then d := d - FreeNeighbours(i, j);
        If IsLastGoal(i, j, gkEscape) Then d := d - 9; // keep the escape target, no switching between equal ones
        If d < best Then Begin
          best := d;
          Target := Point(i, j);
        End;
      End;
    If Target.x <> -1 Then Begin
      LogGoal('escape', Target.x, Target.y);
      fGoal.Kind := gkEscape;
      fGoal.x := Target.x;
      fGoal.y := Target.y;
      If (Target.x = fX) And (Target.y = fY) Then
        Move := amNone
      Else
        Move := First[Target.x, Target.y];
      exit;
    End;
  End;
  // Last chance: kick an adjacent bomb away (only if it does not explode while the ai walks against it)
  If Roll(sfKick) And ((Me.Abilities And Ability_CanKick) <> 0) Then Begin
    For d := 0 To 3 Do Begin
      bx := fX + DirX[Moves[d]];
      by := fY + DirY[Moves[d]];
      If (Not Inside(bx, by)) Or (fBombAt[bx, by] < 0) Then Continue;
      If fBombs[fBombAt[bx, by]].Time <= 2 * fMsPerField + Settings.SafetyMs Then Continue;
      If Inside(bx + DirX[Moves[d]], by + DirY[Moves[d]]) And
        (FieldAt(bx + DirX[Moves[d]], by + DirY[Moves[d]]) In WalkableFields) And
        (fBombAt[bx + DirX[Moves[d]], by + DirY[Moves[d]]] = -1) Then Begin
        Move := Moves[d];
        LogGoal('escape by kick', bx, by);
        exit;
      End;
    End;
  End;
  // Nowhere safe: stay (and hope)
  LogGoal('escape impossible', fX, fY);
End;

Function TSmartAi.ChooseGoal: TGoal;
Var
  Bricks: TBoolGrid;
  g: TBoolGrid;
  i, j, k, n, Hits, Burnt, Enemies, FreeAround, Value, best, d, md, Steps: integer;
  Cand: Array Of TGoal;
  c: TGoal;
  p: TPoint;
  CanBomb, Attack, Collect, Ill: Boolean;
  f: TAiField;
Begin
  result.Kind := gkNone;
  result.x := -1;
  result.y := -1;
  result.Score := 0;
  result.Name := '';
  Search(fX, fY, 0, fDanger, true, Roll(sfTimedDanger), false, fDist, fFirst);

  // Infect: an ill ai walks into the nearest enemy
  Ill := Me.IsIll;
  If Ill And Roll(sfInfect) Then Begin
    best := NotReachable;
    For i := 0 To high(fInfo^.PlayerInfos) Do Begin
      If (Not IsEnemy(i)) Or (Not Usable(i)) Then Continue;
      p := PlayerField(i);
      If fDist[p.x, p.y] < best Then Begin
        best := fDist[p.x, p.y];
        result.x := p.x;
        result.y := p.y;
      End;
    End;
    If result.x <> -1 Then Begin
      result.Kind := gkGoto;
      result.Name := 'infect';
      exit;
    End;
  End;

  // Hunted with at most one bomb: keep away from the hunters
  If (fGameMode = AiMode_Hunter) And (fIndex = fHunted) And (Me.AvailableBombs <= 1) And Roll(sfHuntedFlees) Then Begin
    best := -1;
    For i := 0 To FieldWidth - 1 Do
      For j := 0 To FieldHeight - 1 Do Begin
        If (fDist[i, j] = NotReachable) Or (fDanger[i, j] <> Safe) Then Continue;
        md := High(integer);
        For k := 0 To high(fInfo^.PlayerInfos) Do Begin
          If (Not IsEnemy(k)) Or (Not Usable(k)) Then Continue;
          p := PlayerField(k);
          d := abs(p.x - i) + abs(p.y - j);
          If d < md Then md := d;
        End;
        If md = High(integer) Then Continue;
        md := md * 4 - fDist[i, j]; // far from the hunters, but not a long way there
        If md > best Then Begin
          best := md;
          result.x := i;
          result.y := j;
        End;
      End;
    If result.x <> -1 Then Begin
      result.Kind := gkGoto;
      result.Name := 'flee from hunters';
      exit;
    End;
  End;

  // Bricks that no bomb on the field destroys yet
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do
      Bricks[i, j] := fInfo^.Field[i, j] = fBrick;
  For k := 0 To high(fBombs) Do Begin
    FillChar(g, SizeOf(g), 0);
    FlameCells(fBombs[k].x, fBombs[k].y, fBombs[k].Len, g);
    For i := 0 To FieldWidth - 1 Do
      For j := 0 To FieldHeight - 1 Do
        If g[i, j] Then Bricks[i, j] := false;
  End;

  CanBomb := Me.AvailableBombs > 0;
  Attack := Roll(sfAttack) Or (fGameMode = AiMode_Hunter); // in hunter mode the old ai attacks as well
  Collect := Roll(sfPowerups);
  Cand := Nil;
  n := 0;
  setlength(Cand, FieldWidth * FieldHeight * 2);
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do Begin
      Steps := fDist[i, j];
      If Steps = NotReachable Then Continue;
      f := fInfo^.Field[i, j];
      // Good powerups
      If Collect And (f In GoodPowerUps) And (Settings.PowerupWeight[f] > 0) Then Begin
        c.x := i;
        c.y := j;
        c.Kind := gkCollect;
        c.Name := 'collect powerup';
        c.Score := Settings.PowerupWeight[f] * 12 * 100 Div (Steps + 2);
        If IsLastGoal(i, j, gkCollect) Then c.Score := c.Score * 5 Div 4;
        Cand[n] := c;
        inc(n);
      End;
      // Bomb fields
      If CanBomb And (f In BombPlaceableFields) And (fBombAt[i, j] = -1) And
        Not ((fTime < fNoBombUntil) And (fNoBombCell.x = i) And (fNoBombCell.y = j)) Then Begin
        Hits := CountBrickHits(i, j, Bricks, Burnt);
        Enemies := 0;
        FreeAround := 0;
        If Attack Then Enemies := EnemiesHit(i, j, FreeAround);
        Value := Hits * BrickValue + Enemies * EnemyValue;
        If Collect Then Value := Value - Burnt * BurnPenalty;
        If (Enemies > 0) And Roll(sfTraps) Then Value := Value + (4 * Enemies - FreeAround) * 5;
        If Value <= 0 Then Continue; // no bomb without a reason (the old ai placed them everywhere)
        If Roll(sfOpenSpace) Then Value := Value + FreeNeighbours(i, j);
        If fEnemyLine[i, j] Then Value := Value - Settings.AnticipateCost * 3;
        If Value <= 0 Then Continue;
        c.x := i;
        c.y := j;
        c.Kind := gkBomb;
        If Enemies > 0 Then
          c.Name := 'attack'
        Else
          c.Name := 'bricks';
        c.Score := Value * 100 Div (Steps + 2);
        If IsLastGoal(i, j, gkBomb) Then c.Score := c.Score * 5 Div 4; // keep the goal unless another one is clearly better
        Cand[n] := c;
        inc(n);
      End;
    End;
  // The best candidates first, the first bomb field with an escape route wins (at most 8 checks per frame)
  k := 0;
  While (n > 0) And (k < 8) Do Begin
    best := 0;
    For i := 1 To n - 1 Do
      If Cand[i].Score > Cand[best].Score Then best := i;
    c := Cand[best];
    Cand[best] := Cand[n - 1];
    dec(n);
    If c.Kind = gkBomb Then Begin
      inc(k);
      If Not CanBombAt(c.x, c.y, fDist[c.x, c.y]) Then Continue;
    End;
    result := c;
    exit;
  End;

  // Approach: keep a distance of about 3 to the nearest enemy, on safe fields
  If Roll(sfApproach) Then Begin
    best := High(integer);
    For i := 0 To FieldWidth - 1 Do
      For j := 0 To FieldHeight - 1 Do Begin
        If (fDist[i, j] = NotReachable) Or (fDanger[i, j] <> Safe) Then Continue;
        md := High(integer);
        For k := 0 To high(fInfo^.PlayerInfos) Do Begin
          If (Not IsEnemy(k)) Or (Not Usable(k)) Then Continue;
          p := PlayerField(k);
          d := abs(p.x - i) + abs(p.y - j);
          If d < md Then md := d;
        End;
        If md = High(integer) Then Continue;
        d := abs(md - 3) * 10 + fDist[i, j];
        If IsLastGoal(i, j, gkGoto) Then d := d - 15; // the enemy moves: do not jump between fields of the same distance
        If fEnemyLine[i, j] Then d := d + Settings.AnticipateCost * 5;
        If d < best Then Begin
          best := d;
          result.x := i;
          result.y := j;
        End;
      End;
    If result.x <> -1 Then Begin
      result.Kind := gkGoto;
      result.Name := 'approach';
    End;
  End;
End;

Function TSmartAi.TriggerNow: Boolean;
Var
  g: TBoolGrid;
  k, i, Oldest: integer;
  HasManual, EnemyHit: Boolean;
  p: TPoint;
Begin
  result := false;
  HasManual := false;
  Oldest := 0;
  For k := 0 To fInfo^.BombsCount - 1 Do
    If (fInfo^.Bombs[k].Owner = fIndex) And fInfo^.Bombs[k].ManualTrigger And (Not fInfo^.Bombs[k].Flying) Then Begin
      HasManual := true;
      If fInfo^.Bombs[k].LifeTime > Oldest Then Oldest := fInfo^.Bombs[k].LifeTime;
    End;
  If Not HasManual Then exit;
  // All own remote bombs explode together, with chain reactions
  FillChar(g, SizeOf(g), 0);
  For k := 0 To fInfo^.BombsCount - 1 Do
    If (fInfo^.Bombs[k].Owner = fIndex) And fInfo^.Bombs[k].ManualTrigger And (Not fInfo^.Bombs[k].Flying) Then
      FlameCells(trunc(fInfo^.Bombs[k].Position.x), trunc(fInfo^.Bombs[k].Position.y), fInfo^.Bombs[k].FlameLength, g);
  For k := 0 To high(fBombs) Do
    If g[fBombs[k].x, fBombs[k].y] Then FlameCells(fBombs[k].x, fBombs[k].y, fBombs[k].Len, g);
  If g[fX, fY] Then exit; // never ourselves
  EnemyHit := false;
  For i := 0 To high(fInfo^.PlayerInfos) Do Begin
    If Not Usable(i) Then Continue;
    p := PlayerField(i);
    If Not g[p.x, p.y] Then Continue;
    If IsFriend(i) Then exit; // no friendly fire
    If IsEnemy(i) Then EnemyHit := true;
  End;
  // A human needs time to react: an enemy only after MinReactionMs, without an enemy after MaxWaitMs
  If EnemyHit And Roll(sfTrigger) And (Oldest >= Settings.MinReactionMs) Then exit(true);
  result := Oldest >= Settings.MaxWaitMs;
End;

Function TSmartAi.SpecialAction(Var Cmd: TAiCommand): Boolean;
Var
  d, i, k, ex, ey, bx, by, Range: integer;
  Cells: TPointList;
  HitFriend, Ok: Boolean;
  e: TPoint;
  m: TAiMoveState;

  // An enemy in direction m within Range fields, nothing but walkable fields in between
  Function EnemyInLine(sx, sy: integer; mv: TAiMoveState; aRange: integer; Out Dist: integer): Boolean;
  Var
    j, q, cx, cy: integer;
  Begin
    result := false;
    Dist := 0;
    cx := sx;
    cy := sy;
    For j := 1 To aRange Do Begin
      cx := cx + DirX[mv];
      cy := cy + DirY[mv];
      If (Not Inside(cx, cy)) Or Not (fInfo^.Field[cx, cy] In WalkableFields + [fFlame]) Then exit;
      For q := 0 To high(fInfo^.PlayerInfos) Do
        If IsEnemy(q) And Usable(q) And (PlayerField(q) = Point(cx, cy)) Then Begin
          Dist := j;
          exit(true);
        End;
    End;
  End;

Begin
  result := false;
  // Escaping is more important, unless the danger is still far away (a bomb next to the ai always threatens its field)
  If (fDanger[fX, fY] <> Safe) And (fDanger[fX, fY] <= 3 * fMsPerField + Settings.SafetyMs) Then exit;
  Cells := Nil;
  For d := 0 To 3 Do Begin
    m := Moves[d];
    // Spooger: a row of bombs towards an enemy in the line (needs facing that way)
    setlength(Cells, 0);
    If Roll(sfSpooger) And ((Me.Abilities And Ability_CanSpoog) <> 0) And (Me.AvailableBombs >= 2) And
      (fBombAt[fX, fY] = -1) And (fInfo^.Field[fX, fY] In BombPlaceableFields) Then Begin
      // The row like in the game: it stops at the first field where no bomb can be placed
      setlength(Cells, 1);
      Cells[0] := Point(fX, fY);
      For i := 1 To Me.AvailableBombs - 1 Do Begin
        bx := fX + DirX[m] * i;
        by := fY + DirY[m] * i;
        If (Not Inside(bx, by)) Or Not (fInfo^.Field[bx, by] In BombPlaceableFields) Or (fBombAt[bx, by] <> -1) Then break;
        setlength(Cells, length(Cells) + 1);
        Cells[high(Cells)] := Point(bx, by);
      End;
    End;
    // First check, then turn: turning towards an enemy and walking away again every other frame made the ai shake
    If (length(Cells) >= 2) And EnemyInLine(fX, fY, m, high(Cells) + Me.FlameLength, Range) And
      CanSurviveBombs(Cells, fX, fY, 0, HitFriend) And Not (HitFriend And Roll(sfTeamSafety)) Then Begin
      If fFacing <> m Then Begin
        Cmd.MoveState := m; // turn first (one frame)
        Cmd.Action := apNone;
        exit(true);
      End;
      Begin
        Cmd.Action := apFirstDouble;
        Cmd.MoveState := amNone;
        LogGoal('spooger', fX, fY);
        exit(true);
      End;
    End;
    // Grab: place a bomb and throw it 3 fields onto an enemy
    If Roll(sfGrab) And ((Me.Abilities And Ability_CanGrab) <> 0) And (Me.AvailableBombs >= 1) And
      (fBombAt[fX, fY] = -1) And (fInfo^.Field[fX, fY] In BombPlaceableFields) Then Begin
      bx := fX + DirX[m] * 3;
      by := fY + DirY[m] * 3;
      If Inside(bx, by) And (fInfo^.Field[bx, by] In WalkableFields) Then Begin
        For i := 0 To high(fInfo^.PlayerInfos) Do Begin
          If (Not IsEnemy(i)) Or (Not Usable(i)) Then Continue;
          e := PlayerField(i);
          If (abs(e.x - bx) + abs(e.y - by) <= 1) Then Begin
            // The ai does not stand on the thrown bomb: check from its own field. With the carry bombs mod the
            // range can be 1..6 (the ai does not know it), so every possible landing field must be survivable
            setlength(Cells, 1);
            Ok := true;
            For k := 1 To 6 Do Begin
              Cells[0] := Point(fX + DirX[m] * k, fY + DirY[m] * k);
              If Not Inside(Cells[0].x, Cells[0].y) Then break;
              If Not (CanSurviveBombs(Cells, fX, fY, ThrowMs, HitFriend) And Not (HitFriend And Roll(sfTeamSafety))) Then Ok := false;
            End;
            // Turn only if the throw will really follow (see spooger)
            If Ok And (fFacing <> m) Then Begin
              Cmd.MoveState := m;
              Cmd.Action := apNone;
              exit(true);
            End;
            If Ok Then Begin
              Cmd.Action := apFirstDouble;
              Cmd.MoveState := amNone;
              LogGoal('grab and throw', bx, by);
              exit(true);
            End;
          End;
        End;
      End;
    End;
    // Bombs next to the ai in direction m
    bx := fX + DirX[m];
    by := fY + DirY[m];
    If (Not Inside(bx, by)) Or (fBombAt[bx, by] < 0) Then Continue;
    // Kick: walk against the bomb, it slides towards the enemy
    If Roll(sfKick) And ((Me.Abilities And Ability_CanKick) <> 0) And EnemyInLine(bx, by, m, FieldWidth, Range) Then Begin
      ex := bx + DirX[m];
      ey := by + DirY[m];
      If Inside(ex, ey) And (fInfo^.Field[ex, ey] In WalkableFields) And (fBombAt[ex, ey] = -1) Then Begin
        Cmd.MoveState := m;
        Cmd.Action := apNone;
        LogGoal('kick', bx, by);
        exit(true);
      End;
    End;
    // Punch: the bomb flies 3 fields, onto an enemy there. apSecond also triggers own remote bombs, only without them
    If Roll(sfPunch) And ((Me.Abilities And Ability_CanPunch) <> 0) And EnemyInLine(bx, by, m, 3 + fBombs[fBombAt[bx, by]].Len, Range) Then Begin
      k := 0;
      For i := 0 To fInfo^.BombsCount - 1 Do
        If (fInfo^.Bombs[i].Owner = fIndex) And fInfo^.Bombs[i].ManualTrigger Then inc(k);
      If k = 0 Then Begin
        Cmd.MoveState := m;
        Cmd.Action := apSecond;
        LogGoal('punch', bx, by);
        exit(true);
      End;
    End;
  End;
End;

Function TSmartAi.MoveTo(tx, ty: integer): TAiMoveState;
Begin
  (*
   * On the target field the ai stands still. The old ai centred itself there (only for the looks), that made the
   * figure shake up and down when idle. The game centres a walking player itself (TAtomicField.HandleMovePlayer).
   *)
  result := amNone;
  If (tx = fX) And (ty = fY) Then exit;
  If Inside(tx, ty) And (fDist[tx, ty] <> NotReachable) Then result := fFirst[tx, ty];
End;

Procedure TSmartAi.LogGoal(Const Name: String; x, y: integer);
Var
  s: String;
Begin
  If Not Settings.LogDecisions Then exit;
  s := format('%s (%d,%d)', [Name, x, y]);
  If s = fLastGoal Then exit;
  fLastGoal := s;
  SmartLog(format('P%d t=%d %s', [fIndex, fTime, s]));
End;

Function TSmartAi.Decide: TAiCommand;
Var
  Goal: TGoal;
  m: TAiMoveState;
  d, nx, ny, n: integer;
  Options: Array[0..3] Of TAiMoveState;
Begin
  result.Action := apNone;
  result.MoveState := amNone;
  // 1. Escape
  If EscapeGoal(m) Then Begin
    result.MoveState := m;
    exit;
  End;
  // 2. Remote bombs
  If TriggerNow() Then Begin
    result.Action := apSecond;
    LogGoal('trigger', fX, fY);
    exit;
  End;
  // 3. Special actions with items
  If SpecialAction(result) Then exit;
  // 4. Goals
  Goal := ChooseGoal();
  If Goal.Kind = gkNone Then Begin
    result.MoveState := MoveTo(fX, fY);
    exit;
  End;
  fGoal := Goal;
  LogGoal(Goal.Name, Goal.x, Goal.y);
  If (Goal.x = fX) And (Goal.y = fY) Then Begin
    If Goal.Kind = gkBomb Then Begin
      // The game can refuse a bomb on fields the ai sees as free (e.g. teleporters of a mod): after a few tries the
      // field is blocked for a while, otherwise the ai would stand there forever
      If (fBombTries > 0) And (fBombTryCell = Point(fX, fY)) Then
        inc(fBombTries)
      Else Begin
        fBombTries := 1;
        fBombTryCell := Point(fX, fY);
      End;
      If fBombTries > 5 Then Begin
        fNoBombCell := fBombTryCell;
        fNoBombUntil := fTime + 5000;
        fBombTries := 0;
      End;
      result.Action := apFirst;
      exit;
    End;
    result.MoveState := MoveTo(fX, fY);
  End
  Else Begin
    result.MoveState := MoveTo(Goal.x, Goal.y);
  End;
  // Without the timed danger map the old safety check: never step into a dangerous field
  If (Not Roll(sfTimedDanger)) And (result.MoveState <> amNone) Then Begin
    nx := fX + DirX[result.MoveState];
    ny := fY + DirY[result.MoveState];
    If Inside(nx, ny) And (fDanger[nx, ny] <> Safe) Then result.MoveState := amNone;
  End;
  // Reaction: sometimes a mistake (a random step, never into flames or a dangerous field)
  // Only a step can be wrong: an ai that waits at its goal would otherwise wander off and come back
  If (Settings.MistakeChance > 0) And (result.MoveState <> amNone) And (Rnd(100) < Settings.MistakeChance) Then Begin
    n := 0;
    For d := 0 To 3 Do Begin
      nx := fX + DirX[Moves[d]];
      ny := fY + DirY[Moves[d]];
      If Inside(nx, ny) And (fInfo^.Field[nx, ny] In WalkableFields) And (fBombAt[nx, ny] = -1) And (fDanger[nx, ny] = Safe) Then Begin
        Options[n] := Moves[d];
        inc(n);
      End;
    End;
    If n > 0 Then Begin
      result.MoveState := Options[Rnd(n)];
      result.Action := apNone;
    End;
  End;
End;

Function TSmartAi.CalcAiCommand(Const AiInfo: TAiInfo; GameMode: Cardinal; HuntedIndex: integer;
  RoundTimeLeftMs: integer): TAiCommand;
Begin
  result.Action := apNone;
  result.MoveState := amNone;
  fTime := fTime + GameFrameRate; // called every frame
  If Not AiInfo.PlayerInfos[fIndex].Alive Then exit;
  If Settings.LogDecisions Then Begin
    // Alive in this frame (also while the reaction delay skips the decision), for the death report
    fSeenTime := fTime;
    If fTime > LatestAiTime Then LatestAiTime := fTime;
  End;
  If AiInfo.PlayerInfos[fIndex].Flying Then exit;
  fInfo := @AiInfo;
  fGameMode := GameMode;
  fHunted := HuntedIndex;
  If (fHunted < 0) Or (fHunted > high(AiInfo.PlayerInfos)) Then fHunted := -1;
  fRoundTimeLeft := RoundTimeLeftMs;
  fX := trunc(Me.Position.x);
  fY := trunc(Me.Position.y);
  If Not Inside(fX, fY) Then exit;
  // The game moves a player Speed / FrameRate fields per frame (TAtomicField.HandleMovePlayer), 0.5 = 5 fields per second
  If Me.Speed > 0.01 Then
    fMsPerField := round(GameFrameRate * GameFrameRate / Me.Speed)
  Else
    fMsPerField := 1000;
  BuildBombs;
  BuildDanger;
  // Reaction delay: decide only every DelayMs and keep walking in between (an action only once).
  // Not after an action (e.g. the own bomb, the escape must start at once) and not while in danger.
  // Only within the field of the last decision: walking on into the next field overshot the goal and the ai
  // ran up and down around it (with a long delay one step is more than a field)
  If (Settings.DelayMs > 0) And (fTime < fNextDecision) And (fLastCommand.Action = apNone) And
    (fDanger[fX, fY] = Safe) And (fX = fDecisionCell.x) And (fY = fDecisionCell.y) Then Begin
    result.MoveState := fLastCommand.MoveState;
    If (result.MoveState = amNone) Or (Not Inside(fX + DirX[result.MoveState], fY + DirY[result.MoveState])) Or
      (fDanger[fX + DirX[result.MoveState], fY + DirY[result.MoveState]] = Safe) Then exit;
  End;
  fNextDecision := fTime + Settings.DelayMs;
  fDecisionCell := Point(fX, fY);
  BuildEnemyLines;
  result := Decide();
  If result.Action <> apFirst Then fBombTries := 0; // only presses in a row count
  If Settings.LogDecisions Then LogState(result);
  // Log: a step back within 100 ms (the cause of shaking), with the reason of the last goal
  If Settings.LogDecisions And (result.MoveState <> amNone) Then Begin
    If (fLastMove <> amNone) And (fTime - fLastMoveTime <= 100) And
      (DirX[result.MoveState] = -DirX[fLastMove]) And (DirY[result.MoveState] = -DirY[fLastMove]) Then
      SmartLog(format('P%d t=%d reverse at (%d,%d) pos %.2f/%.2f, last goal: %s', [fIndex, fTime, fX, fY,
        Me.Position.x, Me.Position.y, fLastGoal]));
    fLastMove := result.MoveState;
    fLastMoveTime := fTime;
  End;
  If result.MoveState <> amNone Then fFacing := result.MoveState;
  fLastCommand := result;
End;

Initialization
  InitHurryCoords;

End.
