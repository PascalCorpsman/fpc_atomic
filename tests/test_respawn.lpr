(*
 * Test of TAtomicField.FindRespawnField (game mode hunter): a respawn field must
 * have a way out (a reachable field around a corner), a field enclosed by
 * bricks or a straight dead end is only taken if there is nothing else.
 *
 * Build from this folder (server code, needs the LCL units for Graphics):
 *   fpc -dServer -Mobjfpc -Sh -Fu../units -Fu../server -Fi../server
 *       -Fu<lazarus>/lcl/units/<cpu-os> -Fu<lazarus>/lcl/units/<cpu-os>/<widgetset>
 *       -Fu<lazarus>/components/lazutils/lib/<cpu-os> -FUlib test_respawn.lpr
 *)
Program test_respawn;

{$MODE ObjFPC}{$H+}

Uses
  SysUtils, Classes, uatomic_common, uatomic_field;

Var
  Passed, Failed: Integer;

Procedure Check(Cond: Boolean; Const What: String);
Begin
  If Cond Then inc(Passed) Else Begin
    inc(Failed);
    writeln('FAILED: ', What);
  End;
End;

Function MakeField(Const Free: Array Of TPoint): TAtomicField;
Var
  Scheme: TScheme;
  Players: TPlayers;
  i, j: Integer;
Begin
  Scheme := Default(TScheme);
  For i := 0 To FieldWidth - 1 Do
    For j := 0 To FieldHeight - 1 Do
      Scheme.BrickData[i, j] := bdSolid;
  For i := 0 To high(Free) Do
    Scheme.BrickData[Free[i].x, Free[i].y] := bdBlank;
  Players := Default(TPlayers);
  For i := 0 To high(Players) Do Players[i].UID := NoPlayer;
  result := TAtomicField.Create(Nil, Nil, Nil);
  result.Initialize(Players, Scheme, false);
End;

Function InSet(Const p: TPoint; Const S: Array Of TPoint): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(S) Do
    If (S[i].x = p.x) And (S[i].y = p.y) Then exit(true);
End;

Var
  f: TAtomicField;
  p: TPoint;
  k: Integer;
  LShape: Array[0..2] Of TPoint;
Begin
  Passed := 0;
  Failed := 0;
  Randomize;
  LShape[0] := Point(10, 5);
  LShape[1] := Point(11, 5);
  LShape[2] := Point(11, 6);

  // 1. A single enclosed field, a straight dead end and an L shape: always the L shape
  f := MakeField([Point(1, 1), Point(3, 1), Point(4, 1), Point(5, 1), LShape[0], LShape[1], LShape[2]]);
  For k := 1 To 200 Do Begin
    p := f.FindRespawnField(Point(0, 10));
    Check(InSet(p, LShape), format('run %d: %d,%d is not in the L shape', [k, p.x, p.y]));
  End;
  f.Free;

  // 2. Even if the L shape is close to the hunted (MinDist), it wins over the enclosed field far away
  f := MakeField([Point(0, 0), LShape[0], LShape[1], LShape[2]]);
  For k := 1 To 50 Do Begin
    p := f.FindRespawnField(Point(11, 6));
    Check(InSet(p, LShape), format('near hunted run %d: %d,%d is not in the L shape', [k, p.x, p.y]));
  End;
  f.Free;

  // 3. Only enclosed fields: the old fallback still returns a free field
  f := MakeField([Point(1, 1), Point(13, 9)]);
  p := f.FindRespawnField(Point(1, 1));
  Check((p.x = 13) And (p.y = 9), format('fallback: %d,%d, expected the farthest free field 13,9', [p.x, p.y]));
  f.Free;

  writeln(format('%d passed, %d failed', [Passed, Failed]));
  If Failed > 0 Then halt(1);
End.
