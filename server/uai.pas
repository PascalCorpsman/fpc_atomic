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

Var
  AiInit: TAiInit = Nil;
  AiDeInit: TAiDeInit = Nil;
  AiNewRound: TAiNewRound = Nil;
  AiHandlePlayer: TAiHandlePlayer = Nil;
  AiInterfaceVersion: TAiInterfaceVersion = Nil;
  AiVersion: TAiVersion = Nil;

Function LoadAiLib(): Boolean;
Procedure UnLoadAiLib;

Implementation

Uses dynlibs, uatomic_global;

Var
  Lib: TLibHandle = 0;

Function LoadAiLib(): Boolean;
Var
  Filename: String;
Begin
{$IFDEF Windows}
  Filename := 'ai.dll';
{$ELSE}
  Filename := 'libai.so';
{$ENDIF}
  Filename := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Filename;
  result := false;
  If lib <> 0 Then UnloadLibrary(lib);
  If Not FileExists(Filename) Then exit;
  lib := LoadLibrary(Filename);
  If lib = 0 Then exit;
  AiInterfaceVersion := TAiInterfaceVersion(GetProcAddress(lib, 'AiInterfaceVersion'));
  If Not assigned(AiInterfaceVersion) Then Begin
    UnLoadAiLib;
    exit;
  End;

  If AiInterfaceVersion() <> AiLibInterfaceVersion Then Begin
    logshow(format('Error, invalid ai interface version. Got %d need %d', [AiInterfaceVersion(), AiLibInterfaceVersion]), llError);
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
End;

End.

