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
Library ai;

{$MODE objfpc}{$H+}

Uses
  Classes
  , uai_types
  ;

(*
 * Initialize the Lib
 *)

Function AiInit(): cbool; cdecl;
Begin
  result := true;
  (*
   * Put your implementation here...
   *)
End;

(*
 * Will be called on lib unloading
 *)

Procedure AiDeInit(); cdecl;
Begin
  (*
   * Put your implementation here...
   *)
End;

(*
 * Callback to get Interface version
 *)

Function AiInterfaceVersion(): cuint32; cdecl;
Begin
  (*
   * Do not change !!
   *)
  result := AiLibInterfaceVersion;
End;

(*
 * Versionstring
 *)

Function AiVersion(): PChar; cdecl;
Begin
  (*
   * Put your implementation here...
   *)
  result := pchar('Demo ai ver. 0.01');
End;

(*
 * Tells the Lib to start a new "round" = Reset all old values if needed
 * Strenght in [0% .. 100%] -> 100% means "Best / Strongest"
 *)

Procedure AiNewRound(Strength: cuint8); cdecl;
Begin
  (*
   * Put your implementation here...
   *)
End;

(*
 * Will be Called every 10ms, this is the main routine where the Ai should be implemented
 *)

Function AiHandlePlayer(PlayerIndex: cuint32; Const AiInfo: TAiInfo): TAiCommand; cdecl;
Begin
  Result.Action := apNone;
  Result.MoveState := amNone;
  (*
   * Put your implementation here...
   *)
End;

Exports

  AiInit
  , AiDeInit
  , AiInterfaceVersion
  , AiVersion
  , AiNewRound
  , AiHandlePlayer
  ;

Begin
End.

