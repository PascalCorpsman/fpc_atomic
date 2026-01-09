(******************************************************************************)
(* SDL GameController wrapper by Corpsman                          2026.01.08 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : SDL2 Wrapper for gamecontroller                              *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
Unit usdl_gamecontroller;

{$MODE objfpc}{$H+}

Interface

(*
 * Wenn es nicht Compiliert, hift ein
 *
 * sudo aptitude install libsdl2-dev
 *)

Uses
  Classes, SysUtils
  , sdl2, usdl_joystick
  ;

Type

  { TSDL_GameController }

  TSDL_GameController = Class(TSDL_Joystick)
  private
    fAxisAvailable: Array[0..SDL_CONTROLLER_AXIS_MAX - 1] Of Boolean;
    fButtonAvailable: Array[0..SDL_CONTROLLER_BUTTON_MAX - 1] Of Boolean;
  protected
    Function GetAxisCount: integer; override;
    Function GetAxisValue(index: integer): integer; override;
    Function GetAxisAvailable(index: integer): boolean; virtual;
    Function GetButtonCount: integer; override;
    Function GetButtonValue(index: integer): boolean; override;
    Function GetButtonAvailable(index: integer): boolean; virtual;
  public
    Property ButtonAvailable[index: integer]: boolean read GetButtonAvailable;
    Property AxisAvailable[index: integer]: boolean read GetAxisAvailable;

    Constructor Create(Index: integer); override;
    Destructor Destroy; override;
  End;

Function ResolveGameControllerNameToIndex(Const aName: String; aIndex: integer = 0): integer; // -1 wenn nicht gefunden

Implementation

Function ResolveGameControllerNameToIndex(Const aName: String; aIndex: integer): integer;
Var
  index, i: Integer;
  s: String;
Begin
  result := -1;
  index := 0;
  If aname = '' Then exit;
  For i := 0 To SDL_NumJoysticks() - 1 Do Begin
    If Not SDL_IsGameController(i) Then Continue;
    s := SDL_gameControllerNameForIndex(i);
    If (aName = s) Then Begin
      result := i;
      If aIndex = index Then Begin
        exit;
      End
      Else Begin
        inc(index);
      End;
    End;
  End;
End;

{ TSDL_GameController }

Function TSDL_GameController.GetButtonValue(index: integer): boolean;
Begin
  result := false;
  If (index < 0) Or (index >= SDL_CONTROLLER_BUTTON_MAX) Then exit;
  If Not (fButtonAvailable[index]) Then exit;
  result := SDL_GameControllerGetButton(fInstance, index) <> 0;
End;

Function TSDL_GameController.getButtonCount: integer;
Begin
  result := SDL_CONTROLLER_BUTTON_MAX;
End;

Function TSDL_GameController.GetButtonAvailable(index: integer): boolean;
Begin
  result := false;
  If (index < 0) Or (index >= SDL_CONTROLLER_BUTTON_MAX) Then exit;
  result := fButtonAvailable[index];
End;

Function TSDL_GameController.getAxisCount: integer;
Begin
  result := SDL_CONTROLLER_AXIS_MAX;
End;

Function TSDL_GameController.GetAxisValue(index: integer): integer;
Begin
  result := 0;
  If (index < 0) Or (index >= SDL_CONTROLLER_AXIS_MAX) Then exit;
  result := SDL_GameControllerGetAxis(fInstance, index);
End;

Function TSDL_GameController.GetAxisAvailable(index: integer): boolean;
Begin
  result := false;
  If (index < 0) Or (index >= SDL_CONTROLLER_AXIS_MAX) Then exit;
  result := fAxisAvailable[index];
End;

Constructor TSDL_GameController.Create(Index: integer);
Var
  i: Integer;
Begin
  If (SDL_WasInit(SDL_INIT_GAMECONTROLLER) And SDL_INIT_GAMECONTROLLER) = 0 Then Begin
    Raise Exception.create('Error SDL subsystem for gamecontroller is not initialized.');
  End;
  SDL_GameControllerEventState(SDL_ENABLE);
  fInstance := SDL_GameControllerOpen(Index);
  If Not assigned(fInstance) Then Begin
    Raise Exception.create('Error could not create gamecontroller instance.');
  End;
  For i := low(fButtonAvailable) To high(fButtonAvailable) Do Begin
    fButtonAvailable[i] := SDL_GameControllerHasButton(fInstance, i);
  End;
  For i := low(fAxisAvailable) To high(fAxisAvailable) Do Begin
    fAxisAvailable[i] := SDL_GameControllerHasAxis(fInstance, i);
  End;
End;

Destructor TSDL_GameController.Destroy;
Begin
  If assigned(fInstance) Then Begin
    SDL_GameControllerClose(fInstance);
    fInstance := Nil;
  End;
End;

End.

