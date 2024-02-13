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
  Classes, SysUtils, uai_types;

Const
  (*
   * History: 0.01 - Initialversion
   *)
  Ai_Version = 'Atomic ai ver. 0.01 by Corpsman';

Type

  { TAtomicAi }

  TAtomicAi = Class
  private
    fIndex: integer; // Der Eigene Spieler Index [0..9]
    fStrength: integer; // Die "gewünschte" stärke des Agenten
    fAiInfo: TAiInfo; // Zeiger auf das Aktuell gültige Spiel

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
End;

Destructor TAtomicAi.Destroy;
Begin

End;

Procedure TAtomicAi.Reset(aStrength: integer);
Begin
  fStrength := aStrength;
End;

Function TAtomicAi.CalcAiCommand(Const AiInfo: TAiInfo): TAiCommand;
Begin
  Result.Action := apNone;
  Result.MoveState := amNone;
  If Not AiInfo.PlayerInfos[fIndex].Alive Then exit; // This Ai is dead.
End;

End.

