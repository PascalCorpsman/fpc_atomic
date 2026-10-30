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
  , ctypes
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

(*
 * Optional (interface version >= 5): Bitmask of the supported game modes, see AiMode_Classic ...
 * If this function is not exported, the server assumes AiMode_Classic Or AiMode_Teamplay.
 * The server does not allow ai players in a game mode the lib does not support.
 *)

Function AiGameModes(): cuint32; cdecl;
Begin
  (*
   * Add AiMode_Hunter if your ai knows how to play this mode (see TAiModeInfo.HuntedIndex)
   *)
  result := AiMode_Classic Or AiMode_Teamplay;
End;

(*
 * Optional (interface version >= 5): If exported it is called instead of AiHandlePlayer
 * with additional infos about the game mode.
 * Only read fields of ModeInfo that fit into ModeInfo.Size, newer servers may append fields.
 *)

Function AiHandlePlayerEx(PlayerIndex: cuint32; Var AiInfo: TAiInfo; Var ModeInfo: TAiModeInfo): TAiCommand; cdecl;
Begin
  (*
   * Put your implementation here...
   * e.g. If ModeInfo.GameMode = AiMode_Hunter Then ... ModeInfo.HuntedIndex ...
   *)
  result := AiHandlePlayer(PlayerIndex, AiInfo);
End;

Exports

  AiInit
  , AiDeInit
  , AiInterfaceVersion
  , AiVersion
  , AiNewRound
  , AiHandlePlayer
  , AiGameModes // Optional, can be removed
  , AiHandlePlayerEx // Optional, can be removed
  ;

Begin
End.

