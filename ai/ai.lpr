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
  , uai_types, uatomicai, ctypes;

Var
  ais: Array[0..9] Of TAtomicAi;

  (*
   * Initialize the Lib
   *)

Function AiInit(): cbool; cdecl;
Var
  i: Integer;
Begin
  result := true;
  (*
   * Put your implementation here...
   *)
  For i := 0 To high(ais) Do Begin
    ais[i] := TAtomicAi.Create(i);
  End;
End;

(*
 * Will be called on lib unloading
 *)

Procedure AiDeInit(); cdecl;
Var
  i: Integer;
Begin
  (*
   * Put your implementation here...
   *)
  For i := 0 To high(ais) Do Begin
    ais[i].Free;
    ais[i] := Nil;
  End;
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

Function AiVersion(): pchar; cdecl;
Begin
  (*
   * Put your implementation here...
   *)
  result := pchar(Ai_Version);
End;

(*
 * Tells the Lib to start a new "round" = Reset all old values if needed
 * Strenght in [0% .. 100%] -> 100% means "Best / Strongest"
 *)

Procedure AiNewRound(Strength: cuint8); cdecl;
Var
  i: Integer;
Begin
  (*
   * Put your implementation here...
   *)
  For i := 0 To high(ais) Do Begin
    ais[i].Reset(Strength);
  End;
End;

(*
 * Will be Called every 10ms, this is the main routine where the Ai should be implemented
 *)

Function AiHandlePlayer(PlayerIndex: cuint32; Var AiInfo: TAiInfo): TAiCommand; cdecl;
Begin
  result := ais[PlayerIndex].CalcAiCommand(AiInfo);
End;

(*
 * Optional (interface version >= 5): Bitmask of the supported game modes, see AiMode_Classic ...
 *)

Function AiGameModes(): cuint32; cdecl;
Begin
  result := AiMode_Classic Or AiMode_Teamplay Or AiMode_Hunter;
End;

(*
 * Optional (interface version >= 5): Will be called instead of AiHandlePlayer, with additional info about the game mode
 *)

Function AiHandlePlayerEx(PlayerIndex: cuint32; Var AiInfo: TAiInfo; Var ModeInfo: TAiModeInfo): TAiCommand; cdecl;
Var
  mi: TAiModeInfo;
Begin
  // Defaults for all fields the server did not send (only read the fields that fit into ModeInfo.Size)
  mi.Size := sizeof(mi);
  mi.GameMode := 0;
  mi.HuntedIndex := -1;
  mi.RoundTimeLeftMs := -1;
  If ModeInfo.Size < sizeof(mi) Then Begin
    move(ModeInfo, mi, ModeInfo.Size);
  End
  Else Begin
    move(ModeInfo, mi, sizeof(mi));
  End;
  result := ais[PlayerIndex].CalcAiCommand(AiInfo, mi.GameMode, mi.HuntedIndex);
End;

Exports

  AiInit
  , AiDeInit
  , AiInterfaceVersion
  , AiVersion
  , AiNewRound
  , AiHandlePlayer
  , AiGameModes
  , AiHandlePlayerEx
  ;

Begin
End.

