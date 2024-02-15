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

Type

  THeightPoint = Record
    x, y, height: integer;
  End;

  THeigtFifo = specialize TBufferedFifo < THeightPoint > ;

  PAiInfo = ^TAiInfo;

  { TAtomicAi }

  TAtomicAi = Class
  private
    fIndex: integer; // Der Eigene Spieler Index [0..9]
    fStrength: integer; // Die "gewünschte" stärke des Agenten
    fAiInfo: PAiInfo; // Zeiger auf das Aktuell gültige Spiel

    (*
     *  Wird von IsSurvivable initialisiert (on Demand)
     *)
    fIsSurvivable: Array[0..14, 0..10] Of Boolean;
    fIsSurvivable_initialized: Boolean;


    (*
     * Initialize the heightfield used for the A* algorithm to enable "Navigation"
     *   -1 = Nicht erreichbar von aX,aY,
     *        sonst Abstand in Kacheln zu aX, aY => 0 an aX, aY
     *  =>
     *)
    fHeightField: Array[0..14, 0..10] Of Integer;
    fHeigtFifo: THeigtFifo; // Die ist Object Global, dass derren Speicher nicht immer neu allokiert werden muss -> Spart Rechenzeit !

    Procedure InitHeightFieldFromPos(aX, aY: integer); // Initialisiert fHeightField
    Procedure ResetHeightField(); // Setzt alle felder in fHeightField = -1

    (*
     * True, if coord ax, ay can not be hit by any bomb
     *)
    Function IsSurvivable(aX, aY: integer): Boolean;
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

 { // -- Begin Debug
Uses
  unit1;

Procedure Debug(Const ai: TAtomicAi);
Var
  i, j: Integer;
Begin
  For i := 0 To 14 Do Begin
    For j := 0 To 10 Do Begin
      //form1.StringGrid2.Cells[i, j] := inttostr(ai.fHeightField[i, j]);
      form1.StringGrid2.Cells[i, j] := IntToStr(ord(ai.fIsSurvivable[i, j]));
    End;
  End;
End;

// End Debug }

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

Procedure TAtomicAi.InitHeightFieldFromPos(aX, aY: integer);
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
    If (p.x < 0) Or (p.y < 0) Or (p.x > 14) Or (p.y > 10) Then Continue;
    If (fHeightField[p.x, p.y] = -1) Or (fHeightField[p.x, p.y] > p.height) Then Begin
      If fAiInfo^.Field[p.x, p.y] In [fBlank, fFlame] + GoodPowerUps // + BadPowerUps -- Die KI Will nicht über Bad PowerUps laufen
      Then Begin
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
      fHeightField[i, j] := -1;
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
  If (ax < 0) Or (ax > 14) Or (ay < 0) Or (ay > 10) Then Begin
    result := false;
    exit;
  End;
  result := fIsSurvivable[ax, ay];
End;

Procedure TAtomicAi.Reset(aStrength: integer);
Begin
  fStrength := aStrength;
End;

Function TAtomicAi.CalcAiCommand(Const AiInfo: TAiInfo): TAiCommand;
Begin
  Result.Action := apNone;
  Result.MoveState := amNone;
  If Not AiInfo.PlayerInfos[fIndex].Alive Then exit; // This Ai is dead -> skip
  fAiInfo := @AiInfo;
  fIsSurvivable_initialized := false;
  (*
   * Ab hier gehts los
   *)
  //InitHeightFieldFromPos(trunc(fAiInfo^.PlayerInfos[0].Position.x), trunc(fAiInfo^.PlayerInfos[0].Position.y));

  IsSurvivable(trunc(fAiInfo^.PlayerInfos[0].Position.x), trunc(fAiInfo^.PlayerInfos[0].Position.y));
 // Debug(self);

End;

End.

