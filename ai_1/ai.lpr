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
{$IFDEF AI_LIBRARY}
Library ai;
{$ENDIF}

{$MODE objfpc}{$H+}

Uses
  Classes
  , uai_types,
  fphttpclient, base64, strUtils, sysutils,types,
  websocketserver, AiCommServer
  ;

var
  WebSocket: TAiCommServer;

(*
 * Initialize the Lib
 *)

Function AiInit(): Boolean; cdecl;
Begin
  result := true;
  (*
   * Put your implementation here...
   *)
  WebSocket := TAiCommServer.Create();
End;

(*
 * Will be called on lib unloading
 *)

Procedure AiDeInit(); cdecl;
Begin
  (*
   * Put your implementation here...
   *)
  WebSocket.Free;
End;


(*
 * Tells the Lib to start a new "round" = Reset all old values if needed
 * Strenght in [0% .. 100%] -> 100% means "Best / Strongest"
 *)

Procedure AiNewRound(Strength: uint8); cdecl;
Begin
  (*
   * Put your implementation here...
   *)
End;

(*
 * Will be Called every 10ms, this is the main routine where the Ai should be implemented
 *)

Function AiHandlePlayer(PlayerIndex: uint32; Const AiInfo: TAiInfo): TAiCommand; cdecl;
Begin
  Result.Action := apNone;
  Result.MoveState := amLeft;
  (*

   * Put your implementation here...
   *)
End;

(*
 * Callback to get Interface version
 *)

Function AiInterfaceVersion(): uint32; cdecl;
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


{$IFNDEF AI_LIBRARY}

const Test : String ='{"Teamplay":false,"PlayerInfos":[{"Team":1,"Position":{"x":1,"y":2},"Alive":true,"Flying":false,"FlameLength":2,"AvailableBombs":1,"Speed":1,"Abilities":["Ability_CanKick","Ability_CanGrab"]},{"Team":2,"Position":{"x":3,"y":4},"Alive":false,"Flying":true,"FlameLength":1,"AvailableBombs":2,"Speed":0.5,"Abilities":["Ability_CanSpoog","Ability_CanJelly"]}],"Field":{"fieldSizeX":15,"fieldSizeY":11,"data":[["fSolid","fSolid","fSolid","fSolid","fSolid"],["fSolid","fBlank","fBlank","fBlank","fSolid"],["fSolid","fBlank","fBrick","fBlank","fSolid"],["fSolid","fBlank","fBlank","fBlank","fSolid"],["fSolid","fSolid","fSolid","fSolid","fSolid"]]},"BombsCount":2,"Bombs":[{"Position":{"x":5,"y":6},"FlameLength":1,"Flying":false,"Owner":1,"ManualTrigger":true,"Jelly":false,"DudBomb":false},{"Position":{"x":7,"y":8},"FlameLength":2,"Flying":true,"Owner":2,"ManualTrigger":false,"Jelly":true,"DudBomb":true}]}';
var
  Server : TAiCommServer;
  S : String;
begin
  Server := TAiCommServer.Create;

  Server.Start;

  while (true) do
  begin
    Readln(s);
    Server.SendString(s);
  //  Server.SendString(Test);
//    Sleep(10);
    WriteLn('Response: ',Server.GetLastResponse());
    {
     Aktionen Resetten:

     Aktionen belassen
        links,rechts,oben, unten

    }
    //Sleep(10 *1000);
  end;

  Server.Free;

end.

{$ELSE}

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
{$ENDIF}


