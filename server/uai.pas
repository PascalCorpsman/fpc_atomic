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

Uses dynlibs, uatomic_common;

Var
  Lib: TLibHandle = 0;

Function LoadAiLib(): Boolean;
Var
  Filename, BasePath: String;
{$IFDEF Darwin}
  ArchDir: String = '';
{$ENDIF}

  Function TryLoad(const BasePath: String): Boolean;
  Var
    Candidate: String;
  Begin
    Candidate := IncludeTrailingPathDelimiter(BasePath) + Filename;
    logshow(format('AI: Trying to load from: %s', [Candidate]), llInfo);
    If Not FileExists(Candidate) Then Begin
      logshow(format('AI: File not found: %s', [Candidate]), llWarning);
      Exit(False);
    End;
    Lib := LoadLibrary(Candidate);
    If Lib = 0 Then
      logshow(format('AI: LoadLibrary failed for: %s', [Candidate]), llError)
    Else
      logshow(format('AI: Successfully loaded: %s', [Candidate]), llInfo);
    TryLoad := Lib <> 0;
  End;
Begin
{$IFDEF Windows}
  Filename := 'ai.dll';
{$ELSE}
  {$IFDEF DARWIN}
  Filename := 'libai.dylib';
{$ELSE}
  Filename := 'libai.so';
{$ENDIF}
{$ENDIF}
  WriteLn(StdErr, '[AI_DEBUG] LoadAiLib called, looking for: ', Filename);
  Flush(StdErr);
  logshow(format('AI: LoadAiLib called, looking for: %s', [Filename]), llInfo);
  Result := False;
  If lib <> 0 Then UnloadLibrary(lib);
  Lib := 0;

  BasePath := ExtractFilePath(ParamStr(0));
  logshow(format('AI: BasePath = %s', [BasePath]), llInfo);
  If Not TryLoad(BasePath) Then
  Begin
    logshow('AI: Failed to load from BasePath, trying alternative paths...', llWarning);
{$IFDEF Darwin}
    {$IFDEF CPUAARCH64}
    ArchDir := 'arm64';
    {$ENDIF}
    {$IFDEF CPUX86_64}
    ArchDir := 'x86_64';
    {$ENDIF}
    If ArchDir <> '' Then
      TryLoad(ExpandFileName(IncludeTrailingPathDelimiter(BasePath) + '../../lib/' + ArchDir));
    If (Lib = 0) Then
      TryLoad(ExpandFileName(IncludeTrailingPathDelimiter(BasePath) + '../lib'));
{$ENDIF}
  End;
  If lib = 0 Then Begin
    logshow('AI: Failed to load AI library from all attempted paths', llError);
    exit;
  End;
  logshow('AI: Library loaded, checking functions...', llInfo);
  AiInterfaceVersion := TAiInterfaceVersion(GetProcAddress(lib, 'AiInterfaceVersion'));
  If Not assigned(AiInterfaceVersion) Then Begin
    logshow('AI: Failed to load AiInterfaceVersion function', llError);
    UnLoadAiLib;
    exit;
  End;

  logshow(format('AI: Checking interface version (got %d, need %d)', [AiInterfaceVersion(), AiLibInterfaceVersion]), llInfo);
  If AiInterfaceVersion() <> AiLibInterfaceVersion Then Begin
    logshow(format('Error, invalid ai interface version. Got %d need %d', [AiInterfaceVersion(), AiLibInterfaceVersion]), llError);
    UnLoadAiLib;
    exit;
  End;

  AiInit := TAiInit(GetProcAddress(lib, 'AiInit'));
  If Not assigned(AiInit) Then Begin
    logshow('AI: Failed to load AiInit function', llError);
    UnLoadAiLib;
    exit;
  End;
  
  AiNewRound := TAiNewRound(GetProcAddress(lib, 'AiNewRound'));
  If Not assigned(AiNewRound) Then Begin
    logshow('AI: Failed to load AiNewRound function', llError);
    UnLoadAiLib;
    exit;
  End;

  AiHandlePlayer := TAiHandlePlayer(GetProcAddress(lib, 'AiHandlePlayer'));
  If Not assigned(AiHandlePlayer) Then Begin
    logshow('AI: Failed to load AiHandlePlayer function', llError);
    UnLoadAiLib;
    exit;
  End;

  AiDeInit := TAiDeInit(GetProcAddress(lib, 'AiDeInit'));
  If Not assigned(AiDeInit) Then Begin
    logshow('AI: Failed to load AiDeInit function', llError);
    UnLoadAiLib;
    exit;
  End;

  AiVersion := TAiVersion(GetProcAddress(lib, 'AiVersion'));
  If Not assigned(AiVersion) Then Begin
    logshow('AI: Failed to load AiVersion function', llError);
    UnLoadAiLib;
    exit;
  End;

  logshow('AI: All functions loaded successfully!', llInfo);
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

