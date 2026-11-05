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
 * Tests for the smart ai (ai_smart/) with the mods the server reports in
 * TAiModeInfo: open borders, walking through bricks, boxing glove, flamethrower,
 * invulnerability, white bomb, glove at start; remote bombs and kicks. Each test
 * builds a small field by hand and calls TSmartAi.CalcAiCommand directly.
 *
 *   mkdir lib
 *   fpc -Mobjfpc -Sh -Fu../ai_smart -Fu../server -FUlib test_ai_smart.lpr
 *   ./test_ai_smart
 *
 * Uses the ai_smart.ini of the config folder if there is one (or one next to
 * the program); the tests assume Difficulty 100 for Borders / BrickWalk / Stun.
 * Exit code 0 = all tests passed.
 *)
Program test_ai_smart;
{$mode objfpc}{$H+}
uses SysUtils, ctypes, uai_types, uatomicai_smart, usmartsettings;

const
  MoveName: array[TAiMoveState] of string = ('none', 'left', 'right', 'up', 'down');
  ActName: array[TAiPlayerAction] of string = ('none', 'first', 'firstdouble', 'second', 'seconddouble');

var
  a: TAiInfo;
  Extra: TAiModeInfo; // further mode info fields of a test (cleared by Base)
  failed: integer = 0;

procedure Base;
var x, y, i: integer;
begin
  FillChar(a, SizeOf(a), 0);
  for x := 0 to 14 do for y := 0 to 10 do a.Field[x, y] := fSolid;
  for i := 0 to 1 do begin
    a.PlayerInfos[i].FlameLength := 3; a.PlayerInfos[i].AvailableBombs := 1; a.PlayerInfos[i].Speed := 0.5;
  end;
  a.PlayerInfos[0].Alive := true;
  a.BombsCount := 0; setlength(a.Bombs, 2);
  FillChar(Extra, SizeOf(Extra), 0);
end;

function Run(ModFlags: cuint32; BrickWalk: integer; Frames: integer = 1): TAiCommand;
var ai: TSmartAi; t: integer;
begin
  ai := TSmartAi.Create(0);
  for t := 1 to Frames do begin
    ai.ModeInfo := Extra;
    ai.ModeInfo.Size := SizeOf(ai.ModeInfo);
    ai.ModeInfo.ModFlags := ModFlags;
    ai.ModeInfo.BrickWalkMs[0] := BrickWalk;
    result := ai.CalcAiCommand(a, AiMode_Classic, -1, -1);
  end;
  ai.Free;
end;

function CountPunches(Frames: integer): integer;
var ai: TSmartAi; t: integer; c: TAiCommand;
begin
  result := 0;
  ai := TSmartAi.Create(0);
  for t := 1 to Frames do begin
    FillChar(ai.ModeInfo, SizeOf(ai.ModeInfo), 0);
    ai.ModeInfo.Size := SizeOf(ai.ModeInfo);
    ai.ModeInfo.ModFlags := AiModFlag_PunchStun;
    c := ai.CalcAiCommand(a, AiMode_Classic, -1, -1);
    if c.Action = apSecond then inc(result);
  end;
  ai.Free;
end;

procedure Check(const Name: string; ok: boolean; c: TAiCommand);
begin
  if ok then writeln('ok   ', Name) else begin writeln('FAIL ', Name, ' (move ', MoveName[c.MoveState], ', action ', ActName[c.Action], ')'); inc(failed); end;
end;

var c: TAiCommand; x: integer;
begin
  LoadSettings();
  // 1. Open border: row y=5 from x=0..3 and x=14, the ai on (0,5), a bomb on (2,5) with flame 3 -> only way out is left over the border to (14,5)
  Base;
  for x := 0 to 3 do a.Field[x, 5] := fBlank;
  a.Field[14, 5] := fBlank;
  a.PlayerInfos[0].Position.x := 0.5; a.PlayerInfos[0].Position.y := 5.5;
  a.BombsCount := 1; a.Bombs[0].Position.x := 2.5; a.Bombs[0].Position.y := 5.5; a.Bombs[0].FlameLength := 3; a.Bombs[0].Owner := 1; a.Bombs[0].LifeTime := 1000;
  c := Run(AiModFlag_WrapLeftRight, 0);
  Check('open border: escape to the left over the border', c.MoveState = amLeft, c);
  c := Run(0, 0);
  Check('closed border: no way to the left', c.MoveState <> amLeft, c);

  // 2. Brick walk: the ai on (5,5) between bricks (4,5),(6,5) and blank (5,4)? no: only bricks around, a bomb on the ai field
  Base;
  a.Field[5, 5] := fBlank; a.Field[4, 5] := fBrick; a.Field[6, 5] := fBrick; a.Field[3, 5] := fBlank; a.Field[7, 5] := fBlank;
  a.PlayerInfos[0].Position.x := 5.5; a.PlayerInfos[0].Position.y := 5.5;
  a.BombsCount := 1; a.Bombs[0].Position.x := 5.5; a.Bombs[0].Position.y := 5.5; a.Bombs[0].FlameLength := 1; a.Bombs[0].Owner := 1; a.Bombs[0].LifeTime := 1100;
  c := Run(0, 3000);
  Check('brick walk: escape through a brick', c.MoveState in [amLeft, amRight], c);
  c := Run(0, 0);
  Check('no brick walk: no step into a brick', not (c.MoveState in [amLeft, amRight]), c);

  // 3. Boxing glove: enemy right of the ai, punch ability
  Base;
  for x := 2 to 8 do a.Field[x, 5] := fBlank;
  a.PlayerInfos[0].Position.x := 4.5; a.PlayerInfos[0].Position.y := 5.5; a.PlayerInfos[0].Abilities := Ability_CanPunch;
  a.PlayerInfos[1].Alive := true; a.PlayerInfos[1].Position.x := 5.5; a.PlayerInfos[1].Position.y := 5.5;
  c := Run(AiModFlag_PunchStun, 0, 2); // frame 1 turns to the right, frame 2 punches
  Check('boxing glove: stun the enemy in front', c.Action = apSecond, c);
  // Not across an open border: the server only hits the real neighbour field
  Base;
  a.Field[0, 5] := fBlank; a.Field[1, 5] := fBlank; a.Field[13, 5] := fBlank; a.Field[14, 5] := fBlank;
  a.PlayerInfos[0].Position.x := 0.5; a.PlayerInfos[0].Position.y := 5.5; a.PlayerInfos[0].Abilities := Ability_CanPunch;
  a.PlayerInfos[1].Alive := true; a.PlayerInfos[1].Position.x := 14.5; a.PlayerInfos[1].Position.y := 5.5;
  c := Run(AiModFlag_PunchStun or AiModFlag_WrapLeftRight, 0, 2);
  Check('boxing glove: no punch over an open border', c.Action <> apSecond, c);
  // At most one punch in 30 frames (turning and punching, then a pause; two ais used to turn back and forth)
  Base;
  for x := 2 to 8 do a.Field[x, 5] := fBlank;
  a.PlayerInfos[0].Position.x := 4.5; a.PlayerInfos[0].Position.y := 5.5; a.PlayerInfos[0].Abilities := Ability_CanPunch;
  a.PlayerInfos[1].Alive := true; a.PlayerInfos[1].Position.x := 5.5; a.PlayerInfos[1].Position.y := 5.5;
  Check('boxing glove: one punch, then a pause', CountPunches(30) = 1, c);
  c := Run(0, 0, 2);
  Check('no boxing glove mod: no punch at a player', c.Action <> apSecond, c);

  // Remote bomb: an enemy in its flames, the ai too -> step out (left or up, both are outside) to trigger
  Base;
  for x := 2 to 8 do a.Field[x, 5] := fBlank;
  a.Field[4, 4] := fBlank;
  a.PlayerInfos[0].Position.x := 4.5; a.PlayerInfos[0].Position.y := 5.5; a.PlayerInfos[0].Abilities := Ability_CanTrigger;
  a.PlayerInfos[1].Alive := true; a.PlayerInfos[1].Position.x := 7.5; a.PlayerInfos[1].Position.y := 5.5;
  a.BombsCount := 1; a.Bombs[0].Position.x := 6.5; a.Bombs[0].Position.y := 5.5; a.Bombs[0].FlameLength := 2;
  a.Bombs[0].Owner := 0; a.Bombs[0].ManualTrigger := true; a.Bombs[0].LifeTime := 1000;
  c := Run(0, 0);
  Check('remote bomb: step out of the own flames to trigger', c.MoveState in [amLeft, amUp], c);
  // Kick: a bomb next to the ai explodes in 100 ms -> do not walk against it
  Base;
  for x := 2 to 12 do a.Field[x, 5] := fBlank;
  a.Field[3, 4] := fBlank; a.Field[3, 3] := fBlank;
  a.PlayerInfos[0].Position.x := 3.5; a.PlayerInfos[0].Position.y := 5.5; a.PlayerInfos[0].Abilities := Ability_CanKick;
  a.PlayerInfos[1].Alive := true; a.PlayerInfos[1].Position.x := 10.5; a.PlayerInfos[1].Position.y := 5.5;
  a.BombsCount := 1; a.Bombs[0].Position.x := 4.5; a.Bombs[0].Position.y := 5.5; a.Bombs[0].FlameLength := 1;
  a.Bombs[0].Owner := 1; a.Bombs[0].LifeTime := 1900;
  c := Run(0, 0);
  Check('kick: not against a bomb that explodes at once', c.MoveState <> amRight, c);

  // Flamethrower: the enemy on (8,5) fires to the left (3 fields), the ai on (6,5) is in the beam -> out of it (up)
  Base;
  for x := 3 to 9 do a.Field[x, 5] := fBlank;
  a.Field[6, 4] := fBlank;
  a.PlayerInfos[0].Position.x := 6.5; a.PlayerInfos[0].Position.y := 5.5;
  a.PlayerInfos[1].Alive := true; a.PlayerInfos[1].Position.x := 8.5; a.PlayerInfos[1].Position.y := 5.5;
  Extra.FlameBeamLen[1] := 3; Extra.FlameBeamDir[1] := 2; Extra.FlameFuelMs[1] := 2000; Extra.FlameRange := 3;
  c := Run(0, 0, 1);
  Check('flamethrower: no reaction in the first moment (ReactionMs)', c.MoveState <> amUp, c);
  c := Run(0, 0, 90); // 900 ms, more than the 600 ms of difficulty 100
  Check('flamethrower: out of the beam after the reaction time', c.MoveState = amUp, c);
  // Startled: with FreezeMs the ai stands still in the beam first (the enemy is the approach goal, it would walk)
  Settings.BeamFreezeMs := 750;
  c := Run(0, 0, 50);
  Check('flamethrower: startled, stands still in the beam (FreezeMs)', c.MoveState = amNone, c);
  Settings.BeamFreezeMs := 0;

  // Invulnerable enemy: the only goal would be a bomb at him -> no bomb
  Base;
  for x := 3 to 9 do a.Field[x, 5] := fBlank;
  a.Field[5, 4] := fBlank; a.Field[5, 3] := fBlank;
  a.PlayerInfos[0].Position.x := 5.5; a.PlayerInfos[0].Position.y := 5.5;
  a.PlayerInfos[1].Alive := true; a.PlayerInfos[1].Position.x := 7.5; a.PlayerInfos[1].Position.y := 5.5;
  c := Run(0, 0);
  Check('enemy can be hit: bomb at him', c.Action = apFirst, c);
  Extra.InvulnMs[1] := 5000;
  c := Run(0, 0);
  Check('invulnerable enemy: no bomb at him', c.Action <> apFirst, c);

  // Invulnerable ai: a bomb next to it, no escape needed
  Base;
  for x := 3 to 7 do a.Field[x, 5] := fBlank;
  a.Field[5, 4] := fBlank; a.Field[5, 3] := fBlank;
  a.PlayerInfos[0].Position.x := 5.5; a.PlayerInfos[0].Position.y := 5.5; a.PlayerInfos[0].AvailableBombs := 0;
  a.BombsCount := 1; a.Bombs[0].Position.x := 6.5; a.Bombs[0].Position.y := 5.5; a.Bombs[0].FlameLength := 2; a.Bombs[0].Owner := 1; a.Bombs[0].LifeTime := 1300;
  c := Run(0, 0);
  Check('bomb next to the ai: escape', c.MoveState = amUp, c);
  Extra.InvulnMs[0] := 5000;
  c := Run(0, 0);
  Check('invulnerable ai: no escape needed', c.MoveState <> amUp, c);

  // White bomb: placed at once
  Base;
  for x := 3 to 7 do a.Field[x, 5] := fBlank;
  a.PlayerInfos[0].Position.x := 5.5; a.PlayerInfos[0].Position.y := 5.5;
  Extra.WhiteBombs[0] := 1;
  c := Run(0, 0);
  Check('white bomb: placed at once', c.Action = apFirst, c);

  // Glove at start: looking down (the default), 3 fields below a free field next to a brick -> throw
  Base;
  a.Field[5, 5] := fBlank; a.Field[5, 8] := fBlank; a.Field[5, 9] := fBrick; a.Field[4, 5] := fBlank; a.Field[3, 5] := fBlank;
  a.PlayerInfos[0].Position.x := 5.5; a.PlayerInfos[0].Position.y := 5.5;
  Extra.StartGlove[0] := 1;
  c := Run(0, 0);
  Check('glove at start: throw at the bricks', c.Action = apFirstDouble, c);

  // NoCover: idle behind a bomb (it does not reach the ai) that shields it from an enemy with a flamethrower
  Base;
  for x := 2 to 8 do a.Field[x, 5] := fBlank;
  a.Field[3, 4] := fBlank;
  a.PlayerInfos[0].Position.x := 3.5; a.PlayerInfos[0].Position.y := 5.5; a.PlayerInfos[0].AvailableBombs := 0;
  a.PlayerInfos[1].Alive := true; a.PlayerInfos[1].Position.x := 7.5; a.PlayerInfos[1].Position.y := 5.5;
  a.BombsCount := 1; a.Bombs[0].Position.x := 5.5; a.Bombs[0].Position.y := 5.5; a.Bombs[0].FlameLength := 1; a.Bombs[0].Owner := 1; a.Bombs[0].LifeTime := 0;
  Extra.FlameFuelMs[1] := 5000; Extra.FlameBeamDir[1] := 2; Extra.FlameRange := 5;
  Settings.Chance[sfNoCover] := 100;
  c := Run(0, 0);
  Check('no cover: steps out from behind the bomb', c.MoveState in [amUp, amDown], c);
  // Counter check: without NoCover (and without Anticipate / Approach, they move away from the line on their own) it stays
  Settings.Chance[sfNoCover] := 0; Settings.Chance[sfAnticipate] := 0; Settings.Chance[sfApproach] := 0;
  c := Run(0, 0);
  Check('cover allowed: stays', not (c.MoveState in [amUp, amDown]), c);
  Settings.Chance[sfAnticipate] := 100; Settings.Chance[sfApproach] := 100;

  writeln(failed, ' failed');
  if failed > 0 then halt(1);
end.
