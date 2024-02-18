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

  FieldWidth = 15;
  FieldHeight = 11;

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

    fHasBomb: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Boolean; // True, wenn an dieser Stelle eine Bombe Liegt, redundant zu fAiInfo^.bombs
    fHasBomb_initialized: Boolean;

    (*
     *  Wird von IsSurvivable initialisiert (on Demand)
     *)
    fIsSurvivable: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Boolean;
    fIsSurvivable_initialized: Boolean;

    fSimIsSurvivable: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Boolean;

    fTreatedByOwnBombs: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Boolean;
    fTreatedByOwnBombs_initialized: Boolean;

    fDestroyableBricks: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of integer;
    fDestroyableBrickCoordList: Array[0..FieldWidth * FieldHeight] Of TPoint;

    (*
     * Initialize the heightfield used for the A* algorithm to enable "Navigation"
     *   -1 = Nicht erreichbar von aX,aY,
     *        sonst abstand in Kacheln zu aX, aY => 0 an aX, aY
     *  =>
     *)
    fHeightField: Array[0..FieldWidth - 1, 0..FieldHeight - 1] Of Integer;
    fHeigtFifo: THeigtFifo; // Die ist Object Global, dass derren Speicher nicht immer neu allokiert werden muss -> Spart Rechenzeit !

    Procedure InitHeightFieldFromPos(aX, aY: integer; BombsAreBlocking: Boolean = false); // Initialisiert fHeightField
    Procedure ResetHeightField(); // Setzt alle felder in fHeightField = -1

    (*
     * True, if coord ax, ay can not be hit by any bomb
     *)
    Function IsSurvivable(aX, aY: integer): Boolean;

    (*
     * True, if coord ax, ay can not be hit by own bombs
     *)
    Function TreatedByOwnBombs(aX, aY: integer): Boolean;

    Function HasBomb(ax, ay: integer): Boolean;

    Function EscapeAi(): TPoint; // Ai-Part that runs away from bombs !

    Function BrickDestroyAi(): TPoint;

    (*
     * Returns the Move Commands to navigate the by the shortest path possible to
     * the given tX, tY coordinates.
     * amNone if reached.
     *)
    Function NavigateTo(tX, tY: Integer; BombsAreBlocking: Boolean = false): TAiMoveState;

    Function SimMoveStateEscapeAi(Const MoveState: TAiMoveState): Boolean;
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
      //      form1.StringGrid2.Cells[i, j] := inttostr(ai.fDestroyableBricks[i, j]);
      form1.StringGrid2.Cells[i, j] := inttostr(ai.fHeightField[i, j]);
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
      If (fAiInfo^.Field[p.x, p.y] In [fBlank {, fFlame}] + GoodPowerUps) // + BadPowerUps -- Die KI Will nicht über Bad PowerUps oder Flammen laufen
      Then Begin
        If BombsAreBlocking And (Not ((p.x = trunc(fAiInfo^.PlayerInfos[fIndex].Position.x)) And (p.y = trunc(fAiInfo^.PlayerInfos[fIndex].Position.y)))) Then Begin
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
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        fIsSurvivable[i, j] := (fAiInfo^.Field[i, j] <> fFlame);
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

  Procedure SimExplode(sX, sY, sLen, sDirX, sDirY: Integer);
  Begin
    If (sLen < 0) Or (sx < 0) Or (sy < 0) Or (sx >= FieldWidth) Or (sy >= FieldHeight) Then exit;
    If Not (fAiInfo^.Field[sx, sy] In [fBlank, fFlame]) Then exit; // Only
    fTreatedByOwnBombs[sx, sy] := false;
    sx := sx + sDirX;
    sy := sy + sDirY;
    SimExplode(sx, sy, slen - 1, sdirx, sdiry);
  End;
Var
  i, j: Integer;
Begin
  If Not fTreatedByOwnBombs_initialized Then Begin
    fTreatedByOwnBombs_initialized := true;
    (*
     * Wir Zünden
     *)
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        fTreatedByOwnBombs[i, j] := (fAiInfo^.Field[i, j] <> fFlame);
      End;
    End;
    (*
     * TODO: Viel Cooler wäre wenn man die "zündzeit" berücksichtigen würde.
     * Dann müsste der unten stehende Allgorithmus aber angepasst werden, weil
     * dann geprüft werden müsste ob eine Bombe eine andere Zündet.
     *)
    For i := 0 To fAiInfo^.BombsCount - 1 Do Begin
      If fAiInfo^.Bombs[i].Flying Then Continue; // Flying bombs do not detonate !
      If fAiInfo^.Bombs[i].Owner <> fIndex Then Continue; // Only own bombs are considered
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
  If (ax < 0) Or (ax >= FieldWidth) Or (ay < 0) Or (ay >= FieldHeight) Then Begin
    result := false;
    exit;
  End;
  result := fTreatedByOwnBombs[ax, ay];
End;

Function TAtomicAi.HasBomb(ax, ay: integer): Boolean;
Var
  i, j: Integer;
Begin
  If Not fHasBomb_initialized Then Begin
    fHasBomb_initialized := true;
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        fHasBomb[i, j] := false;
      End;
    End;
    For i := 0 To fAiInfo^.BombsCount - 1 Do Begin
      If Not fAiInfo^.Bombs[i].Flying Then Begin
        fHasBomb[trunc(fAiInfo^.Bombs[i].Position.x), trunc(fAiInfo^.Bombs[i].Position.y)] := true;
      End;
    End;
  End;
  If (ax < 0) Or (ax >= FieldWidth) Or (ay < 0) Or (ay >= FieldHeight) Then Begin
    result := false;
    exit;
  End;
  result := fHasBomb[ax, ay];
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
     * Retry ignoring the Bombs, in the hope that the AI has the kicker ability
     *)
    If nt = NoTarget Then Begin
      InitHeightFieldFromPos(ax, ay, false);
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
    // Field with boxes cannot used to place bombs on
    If Not (fAiInfo^.Field[x, y] In [fBlank, fFlame] + GoodPowerUps + BadPowerUps) Then Begin
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

      If d[amUp] Then Begin
        If fAiInfo^.Field[x, y - i] In [fBrick] + BadPowerUps Then Begin
          inc(result);
          d[amUp] := false;
        End;
      End;

      If d[amDown] Then Begin
        If fAiInfo^.Field[x, y + i] In [fBrick] + BadPowerUps Then Begin
          inc(result);
          d[amDown] := false;
        End;
      End;

      If d[amLeft] Then Begin
        If fAiInfo^.Field[x - i, y] In [fBrick] + BadPowerUps Then Begin
          inc(result);
          d[amLeft] := false;
        End;
      End;

      If d[amRight] Then Begin
        If fAiInfo^.Field[x + i, y] In [fBrick] + BadPowerUps Then Begin
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
      p := fHeightField[fDestroyableBrickCoordList[Trunc((li + re) / 2)].x, fDestroyableBrickCoordList[Trunc((li + re) / 2)].y]; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While fHeightField[fDestroyableBrickCoordList[l].x, fDestroyableBrickCoordList[l].y] < p Do
          inc(l);
        While fHeightField[fDestroyableBrickCoordList[r].x, fDestroyableBrickCoordList[r].y] > p Do
          dec(r);
        If L <= R Then Begin
          h := fDestroyableBrickCoordList[l];
          fDestroyableBrickCoordList[l] := fDestroyableBrickCoordList[r];
          fDestroyableBrickCoordList[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      Sort(li, r);
      Sort(l, re);
    End;
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
  ax, ay, i, j, k: Integer;
  cnt, amax: Integer;
Begin
  result := NoTarget;
  ax := trunc(fAiInfo^.PlayerInfos[fIndex].Position.x);
  ay := trunc(fAiInfo^.PlayerInfos[fIndex].Position.y);
  // 1. Step all Felder Durchgehen und die Maximale
  For i := 0 To FieldWidth - 1 Do Begin
    For j := 0 To FieldHeight - 1 Do Begin
      fDestroyableBricks[i, j] := getBrickCountAt(i, j);
    End;
  End;
  // 2. Suchen des Feldes das von der Aktuellen Position aus erreichbar ist und dass die meisten Bricks kaputt machen kann
  InitHeightFieldFromPos(ax, ay, true);
  cnt := 0;
  amax := 0;
  For i := 0 To Fieldwidth - 1 Do Begin
    For j := 0 To Fieldheight - 1 Do Begin
      // Wenn wir aber ein Feld finden, welches "näher" ist und gleich viele Boxen hat, nehmen wir das
      If (fHeightField[i, j] <> NotAccessable) And (fDestroyableBricks[i, j] = amax) Then Begin
        fDestroyableBrickCoordList[cnt] := point(i, j);
        inc(cnt);
      End;
      // Greedy suchen wir das feld, welches die meisten Bomben hat
      If (fHeightField[i, j] <> NotAccessable) And (fDestroyableBricks[i, j] > amax) Then Begin
        cnt := 0;
        amax := fDestroyableBricks[i, j];
        fDestroyableBrickCoordList[cnt] := point(i, j);
        inc(cnt);
      End;
    End;
  End;
  // 3. Sortieren der Liste nach Abstand zur Aktuellen Position
  Sort(0, cnt - 1);
  For k := 0 To cnt - 1 Do Begin
    // 1. Simulierte Bombe Legen
    SimPlaceBomb(fDestroyableBrickCoordList[k]);
    // 2. Gibt es noch wenigstens 1 Feld auf dass wir Flüchten können ?
    For i := 0 To FieldWidth - 1 Do Begin
      For j := 0 To FieldHeight - 1 Do Begin
        If fSimIsSurvivable[i, j] And (fHeightField[i, j] <> NotAccessable) Then Begin
          // 3. Wenn Ja -> Dass ist unser Target ! Break
          result := fDestroyableBrickCoordList[k];
          exit;
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
  If fHeightField[tx, ty] = NotAccessable Then Begin
    exit;
  End;
  // There are 4 posibilities where to move, check which is the "shortest" and walk there
  shortest := FieldWidth * FieldHeight + 1; // Max Distance
  // First Loop define General direction
  If (ax > 0) And (fHeightField[ax - 1, ay] <> NotAccessable) Then Begin
    If shortest > fHeightField[ax - 1, ay] Then Begin
      shortest := fHeightField[ax - 1, ay];
      result := amLeft;
    End;
  End;
  If (ax + 1 < FieldWidth) And (fHeightField[ax + 1, ay] <> NotAccessable) Then Begin
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
  If (ay + 1 < FieldHeight) And (fHeightField[ax, ay + 1] <> NotAccessable) Then Begin
    If shortest > fHeightField[ax, ay + 1] Then Begin
      shortest := fHeightField[ax, ay + 1];
      result := amDown;
    End;
  End;
  // TODO: Implement Strafing -> Faster Moving around corners ;)
End;

Function TAtomicAi.SimMoveStateEscapeAi(Const MoveState: TAiMoveState): Boolean;
Var
  ax, ay: integer;
Begin
  result := false;
  If MoveState = amNone Then exit;
  ax := trunc(fAiInfo^.PlayerInfos[fIndex].Position.x);
  ay := trunc(fAiInfo^.PlayerInfos[fIndex].Position.y);
  Case MoveState Of
    amDown: If (ay + 1 < FieldHeight) And (Not IsSurvivable(ax, ay + 1)) Then result := true;
    amUp: If (ay - 1 >= 0) And (Not IsSurvivable(ax, ay - 1)) Then result := true;
    amLeft: If (ax - 1 >= 0) And (Not IsSurvivable(ax - 1, ay)) Then result := true;
    amRight: If (ax + 1 < FieldWidth) And (Not IsSurvivable(ax + 1, ay)) Then result := true;
  End;
End;

Procedure TAtomicAi.Reset(aStrength: integer);
Begin
  fStrength := aStrength;
  fActualTarget := NoTarget;
End;

Function TAtomicAi.CalcAiCommand(Const AiInfo: TAiInfo): TAiCommand;
Var
  eT: TPoint;
  px, py, ax, ay: Integer;
  iBomb, iPlayer: Integer;
  skipSecondAction: Boolean;
Begin
  // Init
  Result.Action := apNone;
  Result.MoveState := amNone;
  If Not AiInfo.PlayerInfos[fIndex].Alive Then exit; // This Ai is dead -> skip
  fAiInfo := @AiInfo;
  fIsSurvivable_initialized := false;
  fHasBomb_initialized := false;
  fTreatedByOwnBombs_initialized := false;
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
  fActualTarget := BrickDestroyAi;
  // 4. Sind wir schon an der Stelle an der wir sein wollen, wenn Ja dann Bombe legen
  ax := trunc(fAiInfo^.PlayerInfos[fIndex].Position.x);
  ay := trunc(fAiInfo^.PlayerInfos[fIndex].Position.y);
  // Wir sind wo wir hin wollen, und haben noch bomben übrig -> Bombe Legen
  If (fActualTarget = point(ax, ay)) And (fAiInfo^.PlayerInfos[fIndex].AvailableBombs > 0) Then Begin
    result.Action := apFirst;
    result.MoveState := amNone; // Stop moving to prevent overshoting
    exit;
  End;

  (*
   * Navigate to the choosen target
   *)
  result.MoveState := NavigateTo(fActualTarget.X, fActualTarget.Y);
  If SimMoveStateEscapeAi(result.MoveState) Then Begin
    result.MoveState := amNone;
    fActualTarget := NoTarget;
  End;
  (*
   * If the Ai has triggerable bombs and is in save position -> Fire (except friendly fire ;))
   *)
  If (IsSurvivable(ax, ay)) Then Begin
    skipSecondAction := false;
    For iBomb := 0 To fAiInfo^.BombsCount - 1 Do Begin
      If (fAiInfo^.Bombs[iBomb].Owner = fIndex) And (fAiInfo^.Bombs[iBomb].ManualTrigger) Then Begin
        // Check for friendly fire
        If fAiInfo^.Teamplay Then Begin
          For iPlayer := 0 To high(fAiInfo^.PlayerInfos) Do Begin
            If fAiInfo^.PlayerInfos[iPlayer].Alive And (fAiInfo^.PlayerInfos[iPlayer].Team = fAiInfo^.PlayerInfos[fIndex].Team) Then Begin
              px := trunc(fAiInfo^.PlayerInfos[iPlayer].Position.x);
              py := trunc(fAiInfo^.PlayerInfos[iPlayer].Position.y);
              If Not TreatedByOwnBombs(px, py) Then Begin
                skipSecondAction := true;
                break;
              End;
            End;
          End;
        End;
        If skipSecondAction Then Begin
          break;
        End;
        result.Action := apSecond;
        result.MoveState := amNone; // Stop Moving to secure not accidential walk into Dead zone
        break;
      End;
    End;
  End;
  Debug(self);
End;

End.

