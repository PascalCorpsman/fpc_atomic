(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
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
Unit uatomicai;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uai_types, ufifo;

Const
  (*
   * History: 0.01 - Initialversion
   *)
  Ai_Version = 'Atomic ai ver. 0.01 by Corpsman';

  GoodPowerUps = [fExtraBomb, fLongerFlame, fGoldflame, fExtraSpeed, fKick, fSpooger, fPunch, fGrab, fTrigger, fJelly];
  BadPowerUps = [fRandom, fSlow, fDisease, fBadDisease];
  (*
   * There are 15 * 11 Fields, so every number > 165 will do the trick ;)
   * ! Attention !
   *  The code needs to be NotAccessable a big number, negative values are also not accessable, but will break the logic
   *)
  NotAccessable = 888;

  NoTarget: TPoint = (X: - 1; y: - 1);

Type

  THeightPoint = Record
    x, y, height: integer;
  End;

  THeigtFifo = specialize TBufferedFifo < THeightPoint > ;

  PAiInfo = ^TAiInfo;

  { TAtomicAi }

  TAtomicAi = Class
  private
    fActualTarget: Tpoint; // Aktual Target to Navigate to (-1,-1) = not set
    fIndex: integer; // Der Eigene Spieler Index [0..9]
    fStrength: integer; // Die "gewünschte" stärke des Agenten -> Aktuell nicht genutzt
    fAiInfo: PAiInfo; // Zeiger auf das Aktuell gültige Spiel

    (*
     *  Wird von IsSurvivable initialisiert (on Demand)
     *)
    fIsSurvivable: Array[0..14, 0..10] Of Boolean;
    fIsSurvivable_initialized: Boolean;

    (*
     * Initialize the heightfield used for the A* algorithm to enable "Navigation"
     *   -1 = Nicht erreichbar von aX,aY,
     *        sonst abstand in Kacheln zu aX, aY => 0 an aX, aY
     *  =>
     *)
    fHeightField: Array[0..14, 0..10] Of Integer;
    fHeigtFifo: THeigtFifo; // Die ist Object Global, dass derren Speicher nicht immer neu allokiert werden muss -> Spart Rechenzeit !

    Procedure InitHeightFieldFromPos(aX, aY: integer; BombsAreBlocking: Boolean = false); // Initialisiert fHeightField
    Procedure ResetHeightField(); // Setzt alle felder in fHeightField = -1

    (*
     * True, if coord ax, ay can not be hit by any bomb
     *)
    Function IsSurvivable(aX, aY: integer): Boolean;

    Function EscapeAi(): TPoint; // Ai-Part that runs away from bombs !

    (*
     * Returns the Move Commands to navigate the by the shortest path possible to
     * the given tX, tY coordinates.
     * amNone if reached.
     *)
    Function NavigateTo(tX, tY: Integer; BombsAreBlocking: Boolean = false): TAiMoveState;
  public
    Constructor Create(index: integer); virtual;
    Destructor Destroy(); override;

    (*
     * Reset all internal states and init the ai to a given Strength [0% .. 100%] -> 100% means "Best / Strongest"
     *)
    Procedure Reset(aStrength: integer);

    (*
     * Calculates based on AiInfo what the Agent whant to do as next
     *)
    Function CalcAiCommand(Const AiInfo: TAiInfo): TAiCommand;
  End;

Implementation

{.$DEFINE _Debug_}

{$IFDEF _Debug_}
Uses
  unit1;
{$ENDIF}

Procedure Debug(Const ai: TAtomicAi);
{$IFDEF _Debug_}
Var
  i, j: Integer;
{$ENDIF}
Begin
{$IFDEF _Debug_}
  For i := 0 To 14 Do Begin
    For j := 0 To 10 Do Begin
      form1.StringGrid2.Cells[i, j] := inttostr(ai.fHeightField[i, j]);
      //form1.StringGrid2.Cells[i, j] := IntToStr(ord(ai.fIsSurvivable[i, j]));
    End;
  End;
  form1.Label2.Caption := format('Target: %d %d', [ai.fActualTarget.X, ai.fActualTarget.y]);
{$ENDIF}
End;

Procedure Nop();
Begin

End;

{ TAtomicAi }

Constructor TAtomicAi.Create(index: integer);
Begin
  Inherited create;
  fIndex := index;
  fHeigtFifo := THeigtFifo.create(15 * 11);
  ResetHeightField();
End;

Destructor TAtomicAi.Destroy;
Begin
  fHeigtFifo.free;
  fHeigtFifo := Nil;
End;

Procedure TAtomicAi.InitHeightFieldFromPos(aX, aY: integer;
  BombsAreBlocking: Boolean);
Var
  p: THeightPoint;
  i: Integer;
  skip: Boolean;
Begin
  ResetHeightField();
  (*
   * Idee: Die Fifo
   *)
  p.x := ax;
  p.y := ay;
  p.height := 0;
  fHeigtFifo.Push(p);
  While fHeigtFifo.Count > 0 Do Begin
    p := fHeigtFifo.Pop;
    If (p.x < 0) Or (p.y < 0) Or (p.x > 14) Or (p.y > 10) Then Continue;
    If (fHeightField[p.x, p.y] > p.height) Then Begin
      If (fAiInfo^.Field[p.x, p.y] In [fBlank {, fFlame}] + GoodPowerUps) // + BadPowerUps -- Die KI Will nicht über Bad PowerUps oder Flammen laufen
      Then Begin
        If BombsAreBlocking And (Not ((p.x = trunc(fAiInfo^.PlayerInfos[fIndex].Position.x)) And (p.y = trunc(fAiInfo^.PlayerInfos[fIndex].Position.y)))) Then Begin
          skip := false;
          For i := 0 To fAiInfo^.BombsCount - 1 Do Begin
            If (Not fAiInfo^.Bombs[i].Flying) And
              (p.x = trunc(fAiInfo^.Bombs[i].Position.x)) And (p.y = trunc(fAiInfo^.Bombs[i].Position.y)) Then Begin
              Skip := true;
              break;
            End;
          End;
          If skip Then Continue;
        End;
        fHeightField[p.x, p.y] := p.height;
        p.height := p.height + 1;
        p.x := p.x + 1;
        fHeigtFifo.Push(p);
        p.x := p.x - 2;
        fHeigtFifo.Push(p);
        p.x := p.x + 1;
        p.y := p.y + 1;
        fHeigtFifo.Push(p);
        p.y := p.y - 2;
        fHeigtFifo.Push(p);
      End;
    End;
  End;
End;

Procedure TAtomicAi.ResetHeightField;
Var
  i, j: integer;
Begin
  fHeigtFifo.Clear;
  For i := low(fHeightField) To high(fHeightField) Do Begin
    For j := low(fHeightField[i]) To high(fHeightField[i]) Do Begin
      fHeightField[i, j] := NotAccessable;
    End;
  End;
End;

Function TAtomicAi.IsSurvivable(aX, aY: integer): Boolean;

  Procedure SimExplode(sX, sY, sLen, sDirX, sDirY: Integer);
  Begin
    // TODO: sLen <= 0 oder sLen < 0 ?
    If (sLen < 0) Or (sx < 0) Or (sy < 0) Or (sx > 14) Or (sy > 10) Then exit;
    If Not (fAiInfo^.Field[sx, sy] In [fBlank, fFlame]) Then exit; // Only
    fIsSurvivable[sx, sy] := false;
    sx := sx + sDirX;
    sy := sy + sDirY;
    SimExplode(sx, sy, slen - 1, sdirx, sdiry);
  End;

Var
  i, j: Integer;
Begin
  If Not fIsSurvivable_initialized Then Begin
    fIsSurvivable_initialized := true;
    (*
     * Wir Zünden
     *)
    For i := 0 To 14 Do Begin
      For j := 0 To 10 Do Begin
        fIsSurvivable[i, j] := true;
      End;
    End;
    (*
     * TODO: Viel Cooler wäre wenn man die "zündzeit" berücksichtigen würde.
     * Dann müsste der unten stehende Allgorithmus aber angepasst werden, weil
     * dann geprüft werden müsste ob eine Bombe eine andere Zündet.
     *)
    For i := 0 To fAiInfo^.BombsCount - 1 Do Begin
      If fAiInfo^.Bombs[i].Flying Then Continue; // Flying bombs do not detonate !
      SimExplode(
        trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y),
        fAiInfo^.Bombs[i].FlameLength,
        -1, 0
        );
      SimExplode(
        trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y),
        fAiInfo^.Bombs[i].FlameLength,
        1, 0
        );
      SimExplode(
        trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y),
        fAiInfo^.Bombs[i].FlameLength,
        0, -1
        );
      SimExplode(
        trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y),
        fAiInfo^.Bombs[i].FlameLength,
        0, 1
        );
    End;
  End;
  // Koordinaten außerhalb des Spielfeldes sind generel nicht überlebbar
  If (ax < 0) Or (ax > 14) Or (ay < 0) Or (ay > 10) Then Begin
    result := false;
    exit;
  End;
  result := fIsSurvivable[ax, ay];
End;

Function TAtomicAi.EscapeAi: TPoint;
Var
  ax, ay, i, j, mi: Integer;
  nt: Tpoint;
Begin
  result := NoTarget;
  (*
   * Check, wether the actual coord is "dangerous" or not
   *)
  ax := trunc(fAiInfo^.PlayerInfos[fIndex].Position.x);
  ay := trunc(fAiInfo^.PlayerInfos[fIndex].Position.y);
  If Not IsSurvivable(ax, ay) Then Begin
    (*
     * We are in Trouble, calculate the coordinate which is save and the nearest to our position
     *)
    InitHeightFieldFromPos(ax, ay, true);
    // Debug(self);
    nt := NoTarget;
    mi := NotAccessable;
    // Search for that coordinate, if it exists store it in nt
    For i := 0 To 14 Do Begin
      For j := 0 To 10 Do Begin
        If IsSurvivable(i, j) And (mi > fHeightField[i, j]) Then Begin
          mi := fHeightField[i, j];
          nt := point(i, j);
        End;
      End;
    End;
    result := nt;
  End;
End;

Function TAtomicAi.NavigateTo(tX, tY: Integer; BombsAreBlocking: Boolean
  ): TAiMoveState;

Const
  (*
   * Empririsch ermittelt, je kleiner, desto genauer läuft er auf die
   * Zielkoordinate, zu Klein und er kann "schwingen" anfangen..
   * => Der Wert dient nur der Optik, und der "fein" navigation auf der
   *    Zielkoordinate
   *)
  epsilon = 0.125;

Var
  shortest, ax, ay: integer;
  commapartx, commaparty: Single;
Begin
  result := amNone;
  fActualTarget := Point(tx, ty);
  If fActualTarget = NoTarget Then exit;
  ax := trunc(fAiInfo^.PlayerInfos[fIndex].Position.x);
  ay := trunc(fAiInfo^.PlayerInfos[fIndex].Position.y);
  If (tx = ax) And (ty = aY) Then Begin
    (*
     * The Player "stands" on the coordinate
     * now try to "center" the Player onto .5 / .5 as close as
     * possible, this feature is only for "optical" reasons. it has
     * no effect on the game logic
     *)
    commapartx := fAiInfo^.PlayerInfos[fIndex].Position.x - ax;
    commaparty := fAiInfo^.PlayerInfos[fIndex].Position.y - ay;
    If abs(commapartx - 0.5) > epsilon Then Begin
      If commapartx < 0.5 Then Begin
        result := amRight;
      End
      Else Begin
        result := amLeft;
      End;
    End
    Else Begin
      If abs(commaparty - 0.5) > epsilon Then Begin
        If commaparty < 0.5 Then Begin
          result := amDown;
        End
        Else Begin
          result := amUp;
        End;
      End
      Else Begin
        fActualTarget := NoTarget;
      End;
    End;
    exit; // We already reached our goal -> Exit
  End;
  (*
   * Calculate Height Field in that way that Target is 0, so we only need to
   * navigate to the lowest fHeightField value in reach
   *)
  InitHeightFieldFromPos(tx, ty, BombsAreBlocking);
  Debug(self);
  If fHeightField[tx, ty] = NotAccessable Then Begin
    exit;
  End;
  // There are 4 posibilities where to move, check which is the "shortest" and walk there
  shortest := 15 * 11 + 1; // Max Distance
  // First Loop define General direction
  If (ax > 0) And (fHeightField[ax - 1, ay] <> NotAccessable) Then Begin
    If shortest > fHeightField[ax - 1, ay] Then Begin
      shortest := fHeightField[ax - 1, ay];
      result := amLeft;
    End;
  End;
  If (ax < 14) And (fHeightField[ax + 1, ay] <> NotAccessable) Then Begin
    If shortest > fHeightField[ax + 1, ay] Then Begin
      shortest := fHeightField[ax + 1, ay];
      result := amRight;
    End;
  End;
  If (ay > 0) And (fHeightField[ax, ay - 1] <> NotAccessable) Then Begin
    If shortest > fHeightField[ax, ay - 1] Then Begin
      shortest := fHeightField[ax, ay - 1];
      result := amUp;
    End;
  End;
  If (ay < 10) And (fHeightField[ax, ay + 1] <> NotAccessable) Then Begin
    If shortest > fHeightField[ax, ay + 1] Then Begin
      shortest := fHeightField[ax, ay + 1];
      result := amDown;
    End;
  End;
  // TODO: Implement Strafing -> Faster Moving around corners ;)
End;

Procedure TAtomicAi.Reset(aStrength: integer);
Begin
  fStrength := aStrength;
  fActualTarget := NoTarget;
End;

Function TAtomicAi.CalcAiCommand(Const AiInfo: TAiInfo): TAiCommand;
Var
  eT: TPoint;
Begin
  // Init
  Result.Action := apNone;
  Result.MoveState := amNone;
  If Not AiInfo.PlayerInfos[fIndex].Alive Then exit; // This Ai is dead -> skip
  fAiInfo := @AiInfo;
  fIsSurvivable_initialized := false;
  (*
   * Everything is now initialized, let the magic happen
   *)
  // First prio is life saving -> check wether we should run for our life
  eT := EscapeAi;
  If et <> NoTarget Then Begin
    result.MoveState := NavigateTo(et.X, et.Y, true);
    // TODO: Maybe some should place a bomb here ?
    exit;
  End;

  // Second we try to destroy bricks and collect "goods"

  // Third we search for opponents and try to attack them

  (*
   * Navigate to the choosen target
   *)
  result.MoveState := NavigateTo(fActualTarget.X, fActualTarget.Y);
End;

End.

