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
Unit usdl_input;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uatomic_common, usdl_joystick, sdl2, controls;


Procedure SDL_Init;
Procedure SDL_Quit;

Procedure SDL_CreateSticks(Const akeys: TKeyArray);
Procedure SDL_FreeSticks;

Procedure SDL_SticksToKeyEvent(Const OnKeyDown, OnKeyUp: TKeyEvent);

Implementation

Uses math;

Type
  TKeyState = Record
    Initialized: Boolean;
    Up, Down, Left, Right, first, second: Boolean;
  End;

Var
  fsdl_Loaded: Boolean = false;
  fsdlJoysticks: Array[TKeySet] Of TSDL_Joystick;
  fkeys: TKeyArray;
  LastKeyState: Array[TKeySet] Of TKeyState;

Procedure SDL_Init;
Var
  i: TKeySet;
Begin
  SDL_Quit;
  If (Not fsdl_Loaded) Then Begin
    fsdl_Loaded := SDL_LoadLib('');
    If fsdl_Loaded Then Begin
      fsdl_Loaded := sdl2.SDL_Init(SDL_INIT_GAMECONTROLLER) = 0;
    End;
  End;
  For i In TKeySet Do Begin
    fsdlJoysticks[i] := Nil;
    LastKeyState[i].Initialized := false;
  End;
End;

Procedure SDL_Quit;
Begin
  If fsdl_Loaded Then Begin
    SDL_FreeSticks();
    sdl2.SDL_Quit();
    SDL_UnLoadLib();
  End;
  fsdl_Loaded := false;
End;

Procedure SDL_FreeSticks;
Var
  i: TKeySet;
Begin
  If assigned(fsdlJoysticks[ks0]) Then fsdlJoysticks[ks0].Free;
  If assigned(fsdlJoysticks[ks1]) Then fsdlJoysticks[ks1].Free;
  For i In TKeySet Do Begin
    fsdlJoysticks[i] := Nil;
    LastKeyState[i].Initialized := false;
  End;
End;

Procedure SDL_CreateSticks(Const akeys: TKeyArray);
Var
  index: Integer;
Begin
  If Not fsdl_Loaded Then exit;
  SDL_FreeSticks; // Falls es die schon gibt ;)
  fkeys := akeys;
  If fKeys[ks0].UseSDL2 Then Begin
    index := ResolveJoystickNameToIndex(fKeys[ks0].Name, fKeys[ks0].NameIndex);
    If index <> -1 Then Begin
      fsdlJoysticks[ks0] := TSDL_Joystick.Create(index);
    End;
  End;
  If fKeys[ks1].UseSDL2 Then Begin
    index := ResolveJoystickNameToIndex(fKeys[ks1].Name, fKeys[ks1].NameIndex);
    If index <> -1 Then Begin
      fsdlJoysticks[ks1] := TSDL_Joystick.Create(index);
    End;
  End;
End;

Procedure SDL_SticksToKeyEvent(Const OnKeyDown, OnKeyUp: TKeyEvent);

  Procedure Call(Var Last: Boolean; Event: Boolean; key: word);
  Begin
    If last <> Event Then Begin
      If Event Then Begin
        OnKeyDown(Nil, key, []);
      End
      Else Begin
        OnKeyUp(Nil, key, []);
      End;
    End;
    last := Event;
  End;

  Procedure CheckKeys(Keys: TKeySet);
  Var
    d: Integer;
    up, down, left, right, first, second: Boolean;
  Begin
    If Not assigned(fsdlJoysticks[keys]) Then exit;
    //    If fPlayerIndex[keys] = AIPlayer Then exit;

    // 1. Ermitteln des Aktuellen "Gedrückt" stati
    up := false;
    down := false;
    left := false;
    right := false;
    d := fKeys[keys].AchsisIdle[0] - fsdlJoysticks[keys].Axis[fKeys[keys].AchsisIndex[0]];
    If abs(d) > achsistrigger Then Begin
      If sign(d) = fKeys[keys].AchsisDirection[0] Then Begin
        up := true;
      End
      Else Begin
        down := true;
      End;
    End;
    d := fKeys[keys].AchsisIdle[1] - fsdlJoysticks[keys].Axis[fKeys[keys].AchsisIndex[1]];
    If abs(d) > achsistrigger Then Begin
      If sign(d) = fKeys[keys].AchsisDirection[1] Then Begin
        left := true;
      End
      Else Begin
        right := true;
      End;
    End;
    first := fKeys[keys].ButtonsIdle[0] = fsdlJoysticks[keys].Button[fKeys[keys].ButtonIndex[0]];
    second := fKeys[keys].ButtonsIdle[1] = fsdlJoysticks[keys].Button[fKeys[keys].ButtonIndex[1]];
    // 2. Rauskriegen ob ein Event abgeleitet werden muss
    If Not LastKeyState[Keys].Initialized Then Begin
      LastKeyState[Keys].Initialized := true;
      LastKeyState[Keys].Up := up;
      LastKeyState[Keys].Down := down;
      LastKeyState[Keys].Left := left;
      LastKeyState[Keys].Right := right;
      LastKeyState[Keys].first := first;
      LastKeyState[Keys].second := second;
    End
    Else Begin
      Call(LastKeyState[Keys].Up, up, fKeys[keys].KeyUp);
      Call(LastKeyState[Keys].Down, Down, fKeys[keys].KeyDown);
      Call(LastKeyState[Keys].Left, left, fKeys[keys].KeyLeft);
      Call(LastKeyState[Keys].Right, right, fKeys[keys].KeyRight);
      Call(LastKeyState[Keys].first, first, fKeys[keys].KeyPrimary);
      Call(LastKeyState[Keys].second, second, fKeys[keys].KeySecondary);
    End;
  End;

Var
  event: TSDL_Event;
Begin
  If Not fsdl_Loaded Then exit;
  SDL_PumpEvents();
  While SDL_PollEvent(@event) <> 0 Do Begin // TODO: Braucht man das wirklich ?
  End;
  CheckKeys(ks0);
  CheckKeys(ks1);
End;

End.

