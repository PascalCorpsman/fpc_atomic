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
   * History:
   * -Published- 0.01 - Initialversion
   * -Published- 0.02 - Improve TreatedByOwnBombs (add chain reaction checks)
   *                    Cleanup readability of code
   *                    Switch to InterfaceVersion 2
   * -Published- 0.03 - Switch to InterfaceVersion 3
   *                    Take care of Holes
   *                    Allow collecting bad powerups in panik mode
   *             0.04 - Improve Panik Mode, Friendly fire Check
   *             0.05 - Try to skip Other player that have diseases
   *             0.06 - FIX: AV on ill flying AI'S
   *)
  Ai_Version = 'Atomic ai ver. 0.06 by Corpsman';

  (*
   * Powerups the AI want to collect
   *)
  GoodPowerUps = [fExtraBomb, fLongerFlame, fGoldflame, fExtraSpeed, fKick, fSpooger, fPunch, fGrab, fTrigger, fJelly];

  (*
   * Powerups the Ai does not want to collect
   *)
  BadPowerUps = [fRandom, fSlow, fDisease, fBadDisease];

  (*
   * Fields that can be walked on (missing all powerups)
   *)
  WalkableFields = [fBlank, fHole, fTramp, fConveyorUp, fConveyorDown, fConveyorLeft, fConveyorRight, fArrowUp, fArrowDown, fArrowLeft, fArrowRight];

  (*
   * Fielts that some can place a bomb on (missing all powerups)
   *)
  // TODO: should conveyors be excluded from BombPlaceableFields?
  BombPlaceableFields = [fBlank, fConveyorUp, fConveyorDown, fConveyorLeft, fConveyorRight, fArrowUp, fArrowDown, fArrowLeft, fArrowRight];

  (*
   * There are 15 * 11 Fields, so every number > 165 will do the trick ;)
   * ! Attention !
   *  The code needs to be NotAccessable a big number, negative values are also not accessable, but will break the logic
   *)
  NotAccessable = 888;

  NoTarget: TPoint = (X: - 1; y: - 1);

  FieldWidth = 15;
  FieldHeight = 11;

Type

  TBombinfo = Record
    Value: Boolean; // True, if Bomb existing
    Owner: integer;
    FlameLength: integer;
  End;

  THeightPoint = Record
    x, y, height: integer;
  End;

  THeigtFifo = specialize TBufferedFifo < THeightPoint > ;

  PAiInfo = ^TAiInfo;

  { TAtomicAi }

  TAtomicAi = Class
  private
    (*
     * A Lot of this variables could be used Locally in functions. But they are
     * not used locally to prevent allocation / deallocation on the OS-Stack
     * This is done in the hope to reduce the CPU-Time of the algorithms with
     * the cost of more memory consumption.
     *)
    fAix, fAiy: Integer; // The actual position where the Ai is located at the moment
    fPanik: Boolean; // Will be set, if Escape AI realizes that we are stuck and not able to rescue ourself
    fActualTarget: Tpoint; // Aktual Target to Navigate to (-1,-1) = not set
    fIndex: integer; // The own player index
    fStrength: integer; // The requested Ai-Strength -> Actually not used
    fAiInfo: PAiInfo; // Pointer to AIinfo that is used globally by all routines

    (*
     * True, if a bomb is on that coordinates (this is the unrolled version of fAiInfo^.bombs)
     * will be initialized by InitHasBomb / HasBomb
     *)
    fHasBomb: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of TBombinfo;
    fHasBomb_initialized: Boolean;

    (*
     * True, if the coord is not treated by any bomb
     * will be initialized by IsSurvivable, don't use this variable directly
     *)
    fIsSurvivable: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Boolean;
    fIsSurvivable_initialized: Boolean;

    (*
     * Like fIsSurvivable, but with a additional bomb that the ai want to place
     * \-> Does not need a initialized flag as it occures only once in code and this code directly initialize by itself
     *)
    fSimIsSurvivable: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Boolean;

    (*
     * Container that stores "true" if the field coord is treated by own bombs or by chain reaction of own bombs
     *)
    fTreatedByOwnBombs: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Boolean;
    fTreatedByOwnBombs_initialized: Boolean;

    (*
     * If a Bomb is placed on grid coord, this array tells how many bricks could be destroyed (according to the actual flame settings)
     *)
    fDestroyableBricks: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of integer;
    fDestroyableBrickField: Array[0..14, 0..10] Of TAiField; // Field that simulates bomb exposions for getBrickCountAt

    (*
     * List of Coords, is used at different places inside the code
     * used by: BrickDestroyAi, TreatedByOwnBombs
     *)
    fCoordList: Array[0..FieldWidth * FieldHeight] Of TPoint;

    (*
     * Initialize the heightfield used for the A* algorithm to enable "Navigation"
     *   NotAccessable = not accessable from aX, aY
     *                   otherwise distance to aX, aY => 0 = location (aX, aY)
     *  => initialized by InitHeightFieldFromPos
     *)
    fHeightField: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Integer;
    fHeigtFifo: THeigtFifo; // used by InitHeightFieldFromPos

    Procedure InitHeightFieldFromPos(aX, aY: integer; BombsAreBlocking: Boolean = false); // initializing routine for fHeightField
    Procedure ResetHeightField(); // resets fHeightField values

    (*
     * True, if coord ax, ay can not be hit by any bomb
     *)
    Function IsSurvivable(aX, aY: integer): Boolean;

    (*
     * true, if coord ax, ay can be hit by own bombs
     *)
    Function TreatedByOwnBombs(aX, aY: integer): Boolean;

    (*
     * True, if coord ax, ay holds a Bomb
     *)
    Function HasBomb(ax, ay: integer): Boolean;
    Procedure InitHasBomb();

    Function EscapeAi(): TPoint; // Ai-Part that runs away from bombs !

    (*
     * Checks wether the Ai would loose its live if it walkes as Movestate wants to
     * True, if this would lead to death
     *)
    Function SimMoveWouldLeadToDeath(Const MoveState: TAiMoveState): Boolean;

    Function BrickDestroyAi(): TPoint; // Ai-Part thats aim is to destroy as much bricks as possible

    Function TriggerBombAi(): Boolean; // Ai-Part that detects wether the Ai-should trigger his own triggerable bombs

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
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      form1.StringGrid2.Cells[i, j] := inttostr(ai.fDestroyableBricks[i, j]);
      //form1.StringGrid2.Cells[i, j] := inttostr(ai.fHeightField[i, j]);
      //      form1.StringGrid2.Cells[i, j] := IntToStr(ord(ai.fIsSurvivable[i, j]));
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
  Reset(100);
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
  allowedFields: Set Of TAiField;
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
    If (p.x < 0) Or (p.y < 0) Or (p.x >= FieldWidth) Or (p.y >= FieldHeight) Then Continue;
    If (fHeightField[p.x, p.y] > p.height) Then Begin
      // + BadPowerUps -- Die KI Will nicht über Bad PowerUps oder Flammen laufen
      allowedFields := WalkableFields + GoodPowerUps;
      // Im Panik Modus laufen wir zur not auch über Krankheiten, hauptsache wir kommen auf "Sichere" Koords !
      If fPanik Then Begin
        allowedFields := allowedFields + BadPowerUps;
      End;
      If (fAiInfo^.Field[p.x, p.y] In allowedFields) Then Begin
        If BombsAreBlocking And (Not ((p.x = fAix) And (p.y = fAiy))) Then Begin
          If HasBomb(p.x, p.y) Then Continue;
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
    If (sLen < 0) Or (sx < 0) Or (sy < 0) Or (sx >= FieldWidth) Or (sy >= FieldHeight) Then exit;
    If Not (fAiInfo^.Field[sx, sy] In WalkableFields + [fFlame]) Then exit; // Only
    fIsSurvivable[sx, sy] := false;
    sx := sx + sDirX;
    sy := sy + sDirY;
    SimExplode(sx, sy, slen - 1, sdirx, sdiry);
  End;

Var
  px, py, i, j: Integer;
Begin
  If Not fIsSurvivable_initialized Then Begin
    fIsSurvivable_initialized := true;
    (*
     * Wir Zünden
     *)
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        fIsSurvivable[i, j] := (fAiInfo^.Field[i, j] <> fFlame);
      End;
    End;
    // Treat other players that are ill as "deadly"
    For i := 0 To 9 Do Begin
      // If we are the one who is ill, we need to skip our self, otherwise we would run as long as we are ill
      If fAiInfo^.PlayerInfos[i].Alive And fAiInfo^.PlayerInfos[i].IsIll And (i <> fIndex) and (not (fAiInfo^.PlayerInfos[i].Flying)) Then Begin
        px := trunc(fAiInfo^.PlayerInfos[i].Position.x);
        py := trunc(fAiInfo^.PlayerInfos[i].Position.y);
        fIsSurvivable[px, py] := false;
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
  If (ax < 0) Or (ax >= FieldWidth) Or (ay < 0) Or (ay >= FieldHeight) Then Begin
    result := false;
    exit;
  End;
  result := fIsSurvivable[ax, ay];
End;

Function TAtomicAi.TreatedByOwnBombs(aX, aY: integer): Boolean;
Var
  cnt: integer;

  Procedure SimExplode(sX, sY, sLen, sDirX, sDirY: Integer);
  Begin
    If (sLen < 0) Or (sx < 0) Or (sy < 0) Or (sx >= FieldWidth) Or (sy >= FieldHeight) Then exit;
    If Not (fAiInfo^.Field[sx, sy] In [fBlank, fFlame]) Then exit; // Only
    // The sx, sy Coord is calculated the very first time
    If (Not fTreatedByOwnBombs[sx, sy]) And fHasBomb[sx, sy].Value Then Begin
      // We are hitting a other bomb -> add this one to the "hit" stack
      // Push Coord to Stack
      fCoordList[cnt] := point(sx, sy);
      inc(cnt);
    End;
    fTreatedByOwnBombs[sx, sy] := true;
    sx := sx + sDirX;
    sy := sy + sDirY;
    SimExplode(sx, sy, slen - 1, sdirx, sdiry);
  End;
Var
  i, j: Integer;
  p: Tpoint;
Begin
  If Not fTreatedByOwnBombs_initialized Then Begin
    fTreatedByOwnBombs_initialized := true;
    // As we use the HasBomb Informatios -> Init !
    InitHasBomb();
    (*
     * All fields that actually hold flames are "pretreated"
     *)
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        fTreatedByOwnBombs[i, j] := (fAiInfo^.Field[i, j] = fFlame);
      End;
    End;
    // Prefill the bombs to explode stack with all own Bombs
    cnt := 0;
    For i := 0 To fAiInfo^.BombsCount - 1 Do Begin
      If fAiInfo^.Bombs[i].Flying Then Continue; // Flying bombs do not detonate !
      If fAiInfo^.Bombs[i].Owner <> fIndex Then Continue; // Only own bombs are considered
      // Push Coord to Stack
      fCoordList[cnt] := point(trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y));
      inc(cnt);
    End;
    While cnt <> 0 Do Begin
      // Pop Coord from Stack
      dec(cnt);
      p := fCoordList[cnt];
      fTreatedByOwnBombs[p.x, p.y] := true; // "Pretreat" so that the bomb is not accidentially add to stack again by SimExplode
      SimExplode(
        p.x, p.y,
        fHasBomb[p.x, p.y].FlameLength,
        -1, 0
        );
      SimExplode(
        p.x, p.y,
        fHasBomb[p.x, p.y].FlameLength,
        1, 0
        );
      SimExplode(
        p.x, p.y,
        fHasBomb[p.x, p.y].FlameLength,
        0, -1
        );
      SimExplode(
        p.x, p.y,
        fHasBomb[p.x, p.y].FlameLength,
        0, 1
        );
    End;
  End;
  If (ax < 0) Or (ax >= FieldWidth) Or (ay < 0) Or (ay >= FieldHeight) Then Begin
    result := false;
    exit;
  End;
  result := fTreatedByOwnBombs[ax, ay];
End;

Function TAtomicAi.HasBomb(ax, ay: integer): Boolean;
Begin
  InitHasBomb();
  If (ax < 0) Or (ax >= FieldWidth) Or (ay < 0) Or (ay >= FieldHeight) Then Begin
    result := false;
    exit;
  End;
  result := fHasBomb[ax, ay].Value;
End;

Procedure TAtomicAi.InitHasBomb;
Var
  i, j: Integer;
Begin
  If Not fHasBomb_initialized Then Begin
    fHasBomb_initialized := true;
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        fHasBomb[i, j].Value := false;
      End;
    End;
    For i := 0 To fAiInfo^.BombsCount - 1 Do Begin
      If Not fAiInfo^.Bombs[i].Flying Then Begin
        fHasBomb[trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y)].Value := true;
        fHasBomb[trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y)].Owner := fAiInfo^.Bombs[i].Owner;
        fHasBomb[trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y)].FlameLength := fAiInfo^.Bombs[i].FlameLength;
      End;
    End;
  End;
End;

Function TAtomicAi.EscapeAi: TPoint;
Var
  i, j, mi: Integer;
  nt: Tpoint;
Begin
  result := NoTarget;
  (*
   * Check, wether the actual coord is "dangerous" or not
   *)
  If Not IsSurvivable(fAix, fAiy) Then Begin
    (*
     * We are in Trouble, calculate the coordinate which is save and the nearest to our position
     *)
    InitHeightFieldFromPos(fAix, fAiy, true);
    nt := NoTarget;
    mi := NotAccessable;
    // Search for that coordinate, if it exists store it in nt
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        If IsSurvivable(i, j) And (mi > fHeightField[i, j]) Then Begin
          mi := fHeightField[i, j];
          nt := point(i, j);
        End;
      End;
    End;
    (*
     * There is no more "save" place to walk to
     * Retry ignoring the Bombs, in the hope that the AI can kick the bomb away
     * This Routine also walks into a Hole if there is one, so the check for the CanKick Ability needs to be removed.
     *)
    If (nt = NoTarget) { And (fAiInfo^.PlayerInfos[fIndex].Abilities And Ability_CanKick = Ability_CanKick)} Then Begin
      fPanik := true;
      InitHeightFieldFromPos(fAix, fAiy, Not fPanik);
      mi := NotAccessable;
      // Search for that coordinate, if it exists store it in nt
      For i := 0 To FieldWidth - 1 Do Begin
        For j := 0 To FieldHeight - 1 Do Begin
          If (IsSurvivable(i, j) Or (fAiInfo^.Field[i, j] = fHole)) And (mi > fHeightField[i, j]) Then Begin
            mi := fHeightField[i, j];
            nt := point(i, j);
          End;
        End;
      End;
    End;
    result := nt;
  End;
End;

Function TAtomicAi.BrickDestroyAi: TPoint;

  Function getBrickCountAt(x, y: integer): integer;
  Var
    d: Array[TAiMoveState] Of Boolean;
    i: integer;
  Begin
    result := 0;
    If Not (fDestroyableBrickField[x, y] In BombPlaceableFields + [fFlame] + GoodPowerUps) Then Begin
      exit;
    End;
    // There is already a bomb on this field, ignore it
    If HasBomb(x, y) Then exit;

    d[amUp] := y > 0;
    d[amDown] := y < Fieldheight - 1;
    d[amLeft] := x > 0;
    d[amRight] := x < Fieldwidth - 1;

    For i := 0 To fAiInfo^.PlayerInfos[fIndex].FlameLength Do Begin
      If y - i < 0 Then d[amUp] := false;
      If y + i >= FieldHeight Then d[amDown] := false;
      If x - i < 0 Then d[amLeft] := false;
      If x + i >= FieldWidth Then d[amRight] := false;

      // TODO: The checks below ignore the GoodPowerUps which means that they will not get destroyed, is this good ?
      If d[amUp] Then Begin
        If fDestroyableBrickField[x, y - i] In [fBrick] + BadPowerUps Then Begin
          inc(result);
          d[amUp] := false;
        End;
      End;

      If d[amDown] Then Begin
        If fDestroyableBrickField[x, y + i] In [fBrick] + BadPowerUps Then Begin
          inc(result);
          d[amDown] := false;
        End;
      End;

      If d[amLeft] Then Begin
        If fDestroyableBrickField[x - i, y] In [fBrick] + BadPowerUps Then Begin
          inc(result);
          d[amLeft] := false;
        End;
      End;

      If d[amRight] Then Begin
        If fDestroyableBrickField[x + i, y] In [fBrick] + BadPowerUps Then Begin
          inc(result);
          d[amRight] := false;
        End;
      End;
    End;
  End;

  Procedure Sort(li, re: integer);
  Var
    l, r, p: Integer;
    h: TPoint;
  Begin
    If Li < Re Then Begin
      p := fHeightField[fCoordList[Trunc((li + re) / 2)].x, fCoordList[Trunc((li + re) / 2)].y]; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While fHeightField[fCoordList[l].x, fCoordList[l].y] < p Do
          inc(l);
        While fHeightField[fCoordList[r].x, fCoordList[r].y] > p Do
          dec(r);
        If L <= R Then Begin
          h := fCoordList[l];
          fCoordList[l] := fCoordList[r];
          fCoordList[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      Sort(li, r);
      Sort(l, re);
    End;
  End;

  Procedure SimBombExplodeOnBrickField(sX, sY, sLen, sDirX, sDirY: Integer);
  Begin
    If (sLen < 0) Or (sx < 0) Or (sy < 0) Or (sx >= FieldWidth) Or (sy >= FieldHeight) Then exit;
    If Not (fDestroyableBrickField[sx, sy] In [fBlank, fFlame]) Then Begin
      fDestroyableBrickField[sx, sy] := fBlank;
      exit;
    End;
    sx := sx + sDirX;
    sy := sy + sDirY;
    SimBombExplodeOnBrickField(sx, sy, slen - 1, sdirx, sdiry);
  End;

  Procedure SimPlaceBomb(Const Pos: TPoint);
    Procedure SimExplode(sX, sY, sLen, sDirX, sDirY: Integer);
    Begin
      If (sLen < 0) Or (sx < 0) Or (sy < 0) Or (sx >= FieldWidth) Or (sy >= FieldHeight) Then exit;
      If Not (fAiInfo^.Field[sx, sy] In [fBlank, fFlame]) Then exit; // Only
      fSimIsSurvivable[sx, sy] := false;
      sx := sx + sDirX;
      sy := sy + sDirY;
      SimExplode(sx, sy, slen - 1, sdirx, sdiry);
    End;
  Var
    i, j: integer;
  Begin
    // Das Alte Feld übernehmen
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        fSimIsSurvivable[i, j] := IsSurvivable(i, j);
      End;
    End;
    // Die neu gelegte Bombe Simulieren
    SimExplode(
      Pos.x, pos.y,
      fAiInfo^.PlayerInfos[fIndex].FlameLength,
      -1, 0
      );
    SimExplode(
      Pos.x, pos.y,
      fAiInfo^.PlayerInfos[fIndex].FlameLength,
      1, 0
      );
    SimExplode(
      Pos.x, pos.y,
      fAiInfo^.PlayerInfos[fIndex].FlameLength,
      0, -1
      );
    SimExplode(
      Pos.x, pos.y,
      fAiInfo^.PlayerInfos[fIndex].FlameLength,
      0, 1
      );
  End;

Var
  i, j, k: Integer;
  cnt, amax: Integer;
Begin
  result := NoTarget;
  // TODO: 1. Simuliere Alle Bombem als "explodiert" -> Rausnehmen dieser Bricks
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      fDestroyableBrickField[i, j] := fAiInfo^.Field[i, j];
    End;
  End;
  For i := 0 To fAiInfo^.BombsCount - 1 Do Begin
    SimBombExplodeOnBrickField(trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y), fAiInfo^.Bombs[i].FlameLength, 1, 0);
    SimBombExplodeOnBrickField(trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y), fAiInfo^.Bombs[i].FlameLength, -1, 0);
    SimBombExplodeOnBrickField(trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y), fAiInfo^.Bombs[i].FlameLength, 0, 1);
    SimBombExplodeOnBrickField(trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y), fAiInfo^.Bombs[i].FlameLength, 0, -1);
  End;
  // 2. Step all simulierte Felder Durchgehen und die Maximale bestimmen
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      fDestroyableBricks[i, j] := getBrickCountAt(i, j);
    End;
  End;
  // 2. Suchen des Feldes das von der Aktuellen Position aus erreichbar ist und dass die meisten Bricks kaputt machen kann
  InitHeightFieldFromPos(fAix, fAiy, true);
  cnt := 0;
  amax := 0;
  For i := 0 To Fieldwidth - 1 Do Begin
    For j := 0 To Fieldheight - 1 Do Begin
      // Wenn wir aber ein Feld finden, welches "näher" ist und gleich viele Boxen hat, nehmen wir das
      If (fHeightField[i, j] <> NotAccessable) And (fDestroyableBricks[i, j] = amax) Then Begin
        fCoordList[cnt] := point(i, j);
        inc(cnt);
      End;
      // Greedy suchen wir das feld, welches die meisten Bomben hat
      If (fHeightField[i, j] <> NotAccessable) And (fDestroyableBricks[i, j] > amax) Then Begin
        cnt := 0;
        amax := fDestroyableBricks[i, j];
        fCoordList[cnt] := point(i, j);
        inc(cnt);
      End;
    End;
  End;
  // 3. Sortieren der Liste nach Abstand zur Aktuellen Position
  Sort(0, cnt - 1);
  For k := 0 To cnt - 1 Do Begin
    // 1. Simulierte Bombe Legen
    SimPlaceBomb(fCoordList[k]);
    // 2. Gibt es noch wenigstens 1 Feld auf dass wir Flüchten können ?
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        If fSimIsSurvivable[i, j] And (fHeightField[i, j] <> NotAccessable) Then Begin
          // 3. Wenn Ja -> Dass ist unser Target ! Break
          result := fCoordList[k];
          exit;
        End;
      End;
    End;
  End;
End;

Function TAtomicAi.TriggerBombAi(): Boolean;
Var
  px, py: integer;
  iBomb, iPlayer: Integer;
Begin
  result := false;
  If (IsSurvivable(fAix, fAiy)) Then Begin
    // Check if we have triggerable bombs
    For iBomb := 0 To fAiInfo^.BombsCount - 1 Do Begin
      If (fAiInfo^.Bombs[iBomb].Owner = fIndex) And (fAiInfo^.Bombs[iBomb].ManualTrigger) Then Begin
        result := true;
      End;
    End;
    // We do not have any triggerable bomb -> no need to check anything
    // We are not in Teamplay -> kill them all !
    If (Not result) Or (Not fAiInfo^.Teamplay) Then exit;
    For iBomb := 0 To fAiInfo^.BombsCount - 1 Do Begin
      If (fAiInfo^.Bombs[iBomb].Owner = fIndex) And (fAiInfo^.Bombs[iBomb].ManualTrigger) Then Begin
        // Check for friendly fire
        For iPlayer := 0 To high(fAiInfo^.PlayerInfos) Do Begin
          If fAiInfo^.PlayerInfos[iPlayer].Alive And (fAiInfo^.PlayerInfos[iPlayer].Team = fAiInfo^.PlayerInfos[fIndex].Team) Then Begin
            px := trunc(fAiInfo^.PlayerInfos[iPlayer].Position.x);
            py := trunc(fAiInfo^.PlayerInfos[iPlayer].Position.y);
            If (point(px, py) <> point(fAix, fAiy)) And TreatedByOwnBombs(px, py) Then Begin
              result := false;
              exit;
            End;
          End;
        End;
      End;
    End;
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
  Epsilon = 0.125;

Var
  shortest: integer;
  commapartx, commaparty: Single;
Begin
  result := amNone;
  fActualTarget := Point(tx, ty);
  If fActualTarget = NoTarget Then exit;
  If (tx = fAix) And (ty = fAiy) Then Begin
    (*
     * The Player "stands" on the coordinate
     * now try to "center" the Player onto .5 / .5 as close as
     * possible, this feature is only for "optical" reasons. it has
     * no effect on the game logic
     *)
    commapartx := fAiInfo^.PlayerInfos[fIndex].Position.x - fAix;
    commaparty := fAiInfo^.PlayerInfos[fIndex].Position.y - fAiy;
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
  If fHeightField[tx, ty] = NotAccessable Then Begin
    exit;
  End;
  // There are 4 posibilities where to move, check which is the "shortest" and walk there
  shortest := FieldWidth * FieldHeight + 1; // Max Distance
  // First Loop define General direction
  If (fAix > 0) And (fHeightField[fAix - 1, fAiy] <> NotAccessable) Then Begin
    If shortest > fHeightField[fAix - 1, fAiy] Then Begin
      shortest := fHeightField[fAix - 1, fAiy];
      result := amLeft;
    End;
  End;
  If (fAix + 1 < FieldWidth) And (fHeightField[fAix + 1, fAiy] <> NotAccessable) Then Begin
    If shortest > fHeightField[fAix + 1, fAiy] Then Begin
      shortest := fHeightField[fAix + 1, fAiy];
      result := amRight;
    End;
  End;
  If (fAiy > 0) And (fHeightField[fAix, fAiy - 1] <> NotAccessable) Then Begin
    If shortest > fHeightField[fAix, fAiy - 1] Then Begin
      shortest := fHeightField[fAix, fAiy - 1];
      result := amUp;
    End;
  End;
  If (fAiy + 1 < FieldHeight) And (fHeightField[fAix, fAiy + 1] <> NotAccessable) Then Begin
    If shortest > fHeightField[fAix, fAiy + 1] Then Begin
      shortest := fHeightField[fAix, fAiy + 1];
      result := amDown;
    End;
  End;
End;

Function TAtomicAi.SimMoveWouldLeadToDeath(Const MoveState: TAiMoveState): Boolean;
Begin
  result := false;
  Case MoveState Of
    amDown: If (fAiy + 1 < FieldHeight) And (Not IsSurvivable(fAix, fAiy + 1)) Then result := true;
    amUp: If (fAiy - 1 >= 0) And (Not IsSurvivable(fAix, fAiy - 1)) Then result := true;
    amLeft: If (fAix - 1 >= 0) And (Not IsSurvivable(fAix - 1, fAiy)) Then result := true;
    amRight: If (fAix + 1 < FieldWidth) And (Not IsSurvivable(fAix + 1, fAiy)) Then result := true;
  End;
End;

Procedure TAtomicAi.Reset(aStrength: integer);
Begin
  fStrength := aStrength;
  fActualTarget := NoTarget;
End;

Function TAtomicAi.CalcAiCommand(Const AiInfo: TAiInfo): TAiCommand;
Var
  eT, bDT: TPoint;
Begin
  // Init
  Result.Action := apNone;
  Result.MoveState := amNone;
  If Not AiInfo.PlayerInfos[fIndex].Alive Then exit; // This Ai is dead -> skip
  fAiInfo := @AiInfo;
  fIsSurvivable_initialized := false;
  fHasBomb_initialized := false;
  fTreatedByOwnBombs_initialized := false;
  fAix := trunc(fAiInfo^.PlayerInfos[fIndex].Position.x);
  fAiy := trunc(fAiInfo^.PlayerInfos[fIndex].Position.y);
  (*
   * Everything is now initialized, let the magic happen
   *)
  // First prio is life saving -> check wether we should run for our life
  fPanik := false;
  eT := EscapeAi;
  If eT <> NoTarget Then Begin
    result.MoveState := NavigateTo(eT.X, eT.Y, Not fPanik);
    exit;
  End;
  // Second we try to destroy bricks and collect "goods"
  bDT := BrickDestroyAi;
  // 4. Sind wir schon an der Stelle an der wir sein wollen, wenn Ja dann Bombe legen
  // Wir sind wo wir hin wollen, und haben noch bomben übrig -> Bombe Legen
  If (bDT = point(fAix, fAiy)) And (fAiInfo^.PlayerInfos[fIndex].AvailableBombs > 0) Then Begin

    // Question, should this code be included or not ?
    // If included, then the AI triggers bombs as fast as posible, otherwise they are triggered "later"

    //If TriggerBombAi() Then Begin
    // result.Action := apSecond;
    // result.MoveState := amNone; // Stop Moving to secure not accidential walk into Dead zone
    //End
    //Else Begin
    result.Action := apFirst;
    result.MoveState := amNone; // Stop moving to prevent overshoting
    fActualTarget := NoTarget;
    //End;
    exit;
  End;

  (*
   * Navigate to the choosen target
   *)
  result.MoveState := NavigateTo(bDT.X, bDT.Y);

  // Check wether the requested move would kill the Ai
  If SimMoveWouldLeadToDeath(result.MoveState) Then Begin
    result.MoveState := amNone;
    fActualTarget := NoTarget;
  End;

  (*
   * If the Ai has triggerable bombs and is in save position -> Fire (except friendly fire ;))
   *)
  If TriggerBombAi() Then Begin
    result.Action := apSecond;
    result.MoveState := amNone; // Stop Moving to secure not accidential walk into Dead zone
    fActualTarget := NoTarget;
  End;
  // Debug(self);
End;

End.

