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
Unit uai;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils
  , uai_types
  , ctypes
  ;

Type

  (*
   * Initialize the Lib with the Playercount (normally 10, only in Debuggmode different)
   *)
  TAiInit = Function(): cBool; cdecl;

  (*
   * Will be called on lib unloading
   *)
  TAiDeInit = Procedure(); cdecl;

  (*
   * Tells the Lib to start a new "round" = Reset all old values if needed
   * Strenght in [0% .. 100%] -> 100% means "Best / Strongest"
   *)
  TAiNewRound = Procedure(Strength: cuint8); cdecl;

  (*
   * Will be Called every 10ms, this is the main routine where the Ai should be implemented
   *)
  TAiHandlePlayer = Function(PlayerIndex: cuint32; Var AiInfo: TAiInfo): TAiCommand; Cdecl;

  (*
   * Callback to get Interface version
   *)
  TAiInterfaceVersion = Function(): cuint32; Cdecl;

  (*
   * Callback for versionstring of ai lib
   *)
  TAiVersion = Function(): pchar; cdecl;

  (*
   * Optional (interface version >= 5): Bitmask of the supported game modes, see AiMode_Classic ...
   *)
  TAiGameModes = Function(): cuint32; cdecl;

  (*
   * Optional (interface version >= 5): Like AiHandlePlayer with additional info about the game mode,
   * if exported it is called instead of AiHandlePlayer
   *)
  TAiHandlePlayerEx = Function(PlayerIndex: cuint32; Var AiInfo: TAiInfo; Var ModeInfo: TAiModeInfo): TAiCommand; Cdecl;

Var
  AiInit: TAiInit = Nil;
  AiDeInit: TAiDeInit = Nil;
  AiNewRound: TAiNewRound = Nil;
  AiHandlePlayer: TAiHandlePlayer = Nil;
  AiInterfaceVersion: TAiInterfaceVersion = Nil;
  AiVersion: TAiVersion = Nil;
  AiHandlePlayerEx: TAiHandlePlayerEx = Nil; // Nil, wenn die Lib es nicht exportiert
  AiGameModesMask: cuint32 = 0; // Die unterstützten Spielmodi der geladenen Lib (AiMode_*), 0 = keine Lib geladen

Function LoadAiLib(): Boolean;
Procedure UnLoadAiLib;

Implementation

Uses dynlibs, uatomic_global;

Var
  Lib: TLibHandle = 0;

Function LoadAiLib(): Boolean;
Var
  Filename: String;
  GameModes: TAiGameModes;
Begin
{$IFDEF Windows}
  Filename := 'ai.dll';
{$ELSE}
  Filename := 'libai.so';
{$ENDIF}
  Filename := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Filename;
  result := false;
  // Eine evtl. geladene Lib komplett entladen (mit AiDeInit), sonst zeigen die optionalen Funktionen ggf. noch in die alte Lib
  UnLoadAiLib;
  If Not FileExists(Filename) Then exit;
  lib := LoadLibrary(Filename);
  If lib = 0 Then exit;
  AiInterfaceVersion := TAiInterfaceVersion(GetProcAddress(lib, 'AiInterfaceVersion'));
  If Not assigned(AiInterfaceVersion) Then Begin
    UnLoadAiLib;
    exit;
  End;

  // Ältere Libs ab AiLibMinInterfaceVersion laufen weiter, sie können nur keine neuen Spielmodi
  If (AiInterfaceVersion() < cuint32(AiLibMinInterfaceVersion)) Or (AiInterfaceVersion() > cuint32(AiLibInterfaceVersion)) Then Begin
    logshow(format('Error, invalid ai interface version. Got %d need %d..%d', [AiInterfaceVersion(), AiLibMinInterfaceVersion, AiLibInterfaceVersion]), llError);
    UnLoadAiLib;
    exit;
  End;

  AiInit := TAiInit(GetProcAddress(lib, 'AiInit'));
  If Not assigned(AiInit) Then Begin
    UnLoadAiLib;
    exit;
  End;
  AiNewRound := TAiNewRound(GetProcAddress(lib, 'AiNewRound'));
  If Not assigned(AiNewRound) Then Begin
    UnLoadAiLib;
    exit;
  End;

  AiHandlePlayer := TAiHandlePlayer(GetProcAddress(lib, 'AiHandlePlayer'));
  If Not assigned(AiHandlePlayer) Then Begin
    UnLoadAiLib;
    exit;
  End;

  AiDeInit := TAiDeInit(GetProcAddress(lib, 'AiDeInit'));
  If Not assigned(AiDeInit) Then Begin
    UnLoadAiLib;
    exit;
  End;

  AiVersion := TAiVersion(GetProcAddress(lib, 'AiVersion'));
  If Not assigned(AiVersion) Then Begin
    UnLoadAiLib;
    exit;
  End;

  // Optional: Spielmodi, ohne den Export kann die Lib das normale Spiel und Team play
  AiGameModesMask := AiMode_Classic Or AiMode_Teamplay;
  If AiInterfaceVersion() >= 5 Then Begin
    GameModes := TAiGameModes(GetProcAddress(lib, 'AiGameModes'));
    If assigned(GameModes) Then AiGameModesMask := GameModes();
    AiHandlePlayerEx := TAiHandlePlayerEx(GetProcAddress(lib, 'AiHandlePlayerEx'));
  End;

  result := true;
End;

Procedure UnLoadAiLib;
Begin
  (*
   * Beim Entladen vorher immer ein Deinit !
   *)
  If assigned(AiDeInit) Then Begin
    AiDeInit();
  End;
  If lib <> 0 Then UnloadLibrary(Lib);
  Lib := 0;
  AiInit := Nil;
  AiNewRound := Nil;
  AiDeInit := Nil;
  AiHandlePlayer := Nil;
  AiInterfaceVersion := Nil;
  AiVersion := Nil;
  AiHandlePlayerEx := Nil;
  AiGameModesMask := 0;
End;

End.

