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
{$DEFINE AI_LIBRARY}

{$IFDEF AI_LIBRARY}
library ai;

{$ENDIF}

{$MODE objfpc}{$H+}

uses
  Classes,
  uai_types,
  fphttpclient,
  base64,
  strUtils,
  SysUtils,
  types,
  websocketserver,
  AiCommServer,
  AiInfoJson,
  TypInfo;

var
  AiServer: TAiCommServer = nil;
  JsonBuilder: TStringBuilder;

(*
 * Initialize the Lib
 *)

  function AiInit(): boolean; cdecl;
  begin
    Result := True;
  (*
   * Put your implementation here...
   *)

    if AiServer <> nil then
      exit;
    try


      AiServer := TAiCommServer.Create();
      AiServer.Start;

      writeln('Running AI Server');

    except
      on E: Exception do
      begin
        Writeln(E.ToString);
      end;
    end;
  end;

(*
 * Will be called on lib unloading
 *)

  procedure AiDeInit(); cdecl;
  begin
    // This code cannot be uninstalled. Typing u in FPC AB Server will caus a crash.
    // The LCL framework (websockets) cannot be completely uninitialized.
    // If the UnloadLibrary function of the FPC AB Server is used, it will remove the memory
    // from under our feet, causing the server to crash.

  {
    // Uninstall is not possible
    if AiServer = nil then
      exit;
    try
      FreeAndNil(AiServer);

        ReceiverThreadPool.Free;
        HandlerThreadPool.Free;
        AcceptingThreadPool.Free;
        // Wait a bit for stop to take effect
        Sleep(1000);
    except
      on E: Exception do
      begin
        Writeln(E.ToString);
      end;
    end;  }
  end;


(*
 * Tells the Lib to start a new "round" = Reset all old values if needed
 * Strenght in [0% .. 100%] -> 100% means "Best / Strongest"
 *)
  procedure AiNewRound(Strength: uint8); cdecl;
  var
    JsonBuilder: TStringBuilder;
  begin
  (*
   * Put your implementation here...
   *)
    try
      JsonBuilder := TStringBuilder.Create;
      try
        NewRoundToJson(Strength, JsonBuilder);
        AiServer.SendString(JsonBuilder.toString());
      finally
        JsonBuilder.Free;
      end;
    except
      on E: Exception do
      begin
        Writeln(E.ToString);
      end;
    end;
  end;

(*
 * Will be Called every 10ms, this is the main routine where the Ai should be implemented
 *)

  function AiHandlePlayer(PlayerIndex: uint32; const AiInfo: TAiInfo): TAiCommand; cdecl;
  begin
    try
      AiServer.AiInfo[PlayerIndex] := AiInfo;
      Result := AiServer.AiCommand[PlayerIndex];
    except
      on E: Exception do
      begin
        Writeln('AiHandlePlayer');
        Writeln(E.ToString);
        Writeln(DumpExceptionCallStack(E));
        FillByte(Result, sizeof(Result) ,0);
      end;
    end;
  end;

(*
 * Callback to get Interface version
 *)

  function AiInterfaceVersion(): uint32; cdecl;
  begin
  (*
   * Do not change !!
   *)
    Result := AiLibInterfaceVersion;
  end;

(*
 * Versionstring
 *)

  function AiVersion(): PChar; cdecl;
  begin
  (*
   * Put your implementation here...
   *)
    Result := PChar('Demo ai ver. 0.01');
  end;


{$IFNDEF AI_LIBRARY}

const
  EXAMPLE_JSON: TAiInfo = (
    Teamplay: true;
    PlayerInfos: (
      (Team: 1; Position: (x: 10.0; y: 20.0); Alive: true; Flying: false;
       FlameLength: 3; AvailableBombs: 2; Speed: 1.0; Abilities: 11),
      (Team: 2; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21),
      (Team: 3; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21),
      (Team: 4; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21),
      (Team: 5; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21),
      (Team: 6; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21),
      (Team: 7; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21),
      (Team: 8; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21),
      (Team: 9; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21),
       (Team: 10; Position: (x: 30.0; y: 40.0); Alive: false; Flying: true;
       FlameLength: 2; AvailableBombs: 1; Speed: 0.5; Abilities: 21)
    );
    Field: (
      (fSolid, fSolid, fSolid, fSolid, fSolid, fSolid, fSolid, fSolid, fSolid,
       fSolid, fSolid),
      (fSolid, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank,
       fBlank, fBlank),
      (fSolid, fBlank, fBrick, fBrick, fBrick, fBrick, fBrick, fBrick, fBrick,
       fBrick, fBrick),
      (fSolid, fBlank, fBrick, fSolid, fSolid, fSolid, fSolid, fSolid, fSolid,
       fSolid, fSolid),
      (fSolid, fBlank, fBrick, fSolid, fBlank, fBlank, fBlank, fBlank, fBlank,
       fBlank, fSolid),
      (fSolid, fBlank, fBrick, fSolid, fBlank, fSolid, fSolid, fSolid, fBlank,
       fBlank, fSolid),
      (fSolid, fBlank, fBrick, fSolid, fBlank, fSolid, fBlank, fSolid, fBlank,
       fBlank, fSolid),
      (fSolid, fBlank, fBrick, fSolid, fBlank, fSolid, fSolid, fSolid, fBlank,
       fBlank, fSolid),
      (fSolid, fBlank, fBrick, fSolid, fBlank, fBlank, fBlank, fBlank, fBlank,
       fBlank, fSolid),
      (fSolid, fBlank, fBrick, fSolid, fSolid, fSolid, fSolid, fSolid, fSolid,
       fSolid, fSolid),
      (fSolid, fBlank, fBrick, fBrick, fBrick, fBrick, fBrick, fBrick, fBrick,
       fBrick, fBrick),
      (fSolid, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fSolid, fSolid),
      (fSolid, fSolid, fSolid, fSolid, fSolid, fSolid, fSolid, fSolid, fSolid,fSolid, fSolid),
      (fSolid, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fSolid, fSolid),
      (fSolid, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fBlank, fSolid, fSolid)
);
BombsCount: 2;
Bombs: (
(Position: (x: 50.0; y: 60.0); FlameLength: 3; Flying: true; Owner: 1;
ManualTrigger: false; Jelly: true; DudBomb: false),
(Position: (x: 70.0; y: 80.0); FlameLength: 2; Flying: false; Owner: 2;
ManualTrigger: true; Jelly: false; DudBomb: true)
)
);

  function FormatTime(msec: Integer): String;
var
  minutes, seconds, milliseconds: Integer;
begin
  minutes := msec div 60000;
  seconds := (msec mod 60000) div 1000;
  milliseconds := (msec mod 1000);

  Result := Format('%d:%.2d.%.3d', [minutes, seconds, milliseconds]);
end;


const Test : String ='{"Teamplay":false,"PlayerInfos":[{"Team":1,"Position":{"x":1,"y":2},"Alive":true,"Flying":false,"FlameLength":2,"AvailableBombs":1,"Speed":1,"Abilities":["Ability_CanKick","Ability_CanGrab"]},{"Team":2,"Position":{"x":3,"y":4},"Alive":false,"Flying":true,"FlameLength":1,"AvailableBombs":2,"Speed":0.5,"Abilities":["Ability_CanSpoog","Ability_CanJelly"]}],"Field":{"fieldSizeX":15,"fieldSizeY":11,"data":[["fSolid","fSolid","fSolid","fSolid","fSolid"],["fSolid","fBlank","fBlank","fBlank","fSolid"],["fSolid","fBlank","fBrick","fBlank","fSolid"],["fSolid","fBlank","fBlank","fBlank","fSolid"],["fSolid","fSolid","fSolid","fSolid","fSolid"]]},"BombsCount":2,"Bombs":[{"Position":{"x":5,"y":6},"FlameLength":1,"Flying":false,"Owner":1,"ManualTrigger":true,"Jelly":false,"DudBomb":false},{"Position":{"x":7,"y":8},"FlameLength":2,"Flying":true,"Owner":2,"ManualTrigger":false,"Jelly":true,"DudBomb":true}]}';
var
  S : String;
  i, c, p : Integer;
  aiCommand : TAiCommand;
begin
  jsonBuilder := TStringBuilder.Create;
  AiServer := TAiCommServer.Create;

  AiServer.Start;
  Randomize;

  p := 0;
  c := 0;

  while (true) do
  begin
    Inc(c);
   { if (c > 100) then
      Readln(s);}
//    p := random(10);
P := 0;
   // Writeln('Sent player: ', p);
    Sleep(10);

    AiServer.AiInfo[p] := EXAMPLE_JSON;
//    AiInfoToJson(p mod 10, EXAMPLE_JSON, jsonBuilder);


//    AiServer.SendString(jsonBuilder.ToString());
  //  Server.SendString(Test);
//    Sleep(10);

   { S := AiServer.GetLastResponse();
    if Length(S) <> 0 then
    begin
      WriteLn('ResponseString: ', S);
    end;
    }
//    for i := 0 to MAX_PLAYERS-1 do
    for i := 0 to 0 do
    begin
      aiCommand := AiServer.AiCommand[i];
      if HasAiCommands(aiCommand) then
      begin
         Writeln(i,': Action: ', GetEnumName(TypeInfo(TAiPlayerAction), Ord(aiCommand.Action)));
         Writeln(i,': MoveState: ', GetEnumName(TypeInfo(TAiMoveState), Ord(aiCommand.MoveState)));
         Writeln(i,': Duration: ', FormatTime(AiServer.GetResponseTime(i)));
      end;

    end;

    {
     Aktionen Resetten:

     Aktionen belassen
        links,rechts,oben, unten

    }
    //Sleep(10 *1000);
  end;

  AiServer.Free;

end.

{$ELSE}

exports

  AiInit
  ,
  AiDeInit
  ,
  AiInterfaceVersion
  ,
  AiVersion
  ,
  AiNewRound
  ,
  AiHandlePlayer;

begin
end.
{$ENDIF}
