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
     * Wird von InitHeightFieldFrom initialisert ->
     *   -1 = Nicht erreichbar von aX,aY,
     *        sonst abstand in Kacheln zu aX, aY => 0 an aX, aY
     *)
    fHeightField: Array[0..14, 0..10] Of Integer;
    fHeigtFifo: THeigtFifo; // Die ist Object Global, dass derren Speicher nicht immer neu allokiert werden muss -> Spart Rechenzeit !

    Procedure InitHeightFieldFromPos(aX, aY: integer); // Initialisiert fHeightField
    Procedure ResetHeightField(); // Setzt alle felder in fHeightField = -1
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

Procedure TAtomicAi.ResetHeightField();
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
  (*
   * Ab hier gehts los
   *)

End;

End.

