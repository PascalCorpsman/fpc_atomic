(******************************************************************************)
(* SDL Joystick wrapper by Corpsman                                2020.04.21 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : SDL2 Wrapper for joysticks                                   *)
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
Unit usdl_joystick;

{$MODE objfpc}{$H+}

Interface

(*
 * Wenn es nicht Compiliert, hift ein
 *
 * sudo aptitude install libsdl2-dev
 *)

Uses
  Classes, SysUtils
  , sdl2
  ;

Type

  { TSDL_Joystick }

  TSDL_Joystick = Class
  private
    fAxisCount: integer;
    fButtonCount: integer;
    fInstance: Pointer;
    Function GetAxisValue(index: integer): integer;
    Function GetButtonValue(index: integer): boolean;
  public
    Property ButtonCount: integer read fButtonCount;
    Property Button[index: integer]: boolean read GetButtonValue;

    Property AxisCount: integer read fAxisCount;
    Property Axis[index: integer]: integer read GetAxisValue; // ranging from -32768 to 32767

    Constructor Create(Index: integer);
    Destructor Destroy; override;
  End;

Function ResolveJoystickNameToIndex(Const aName: String; aIndex: integer = 0): integer; // -1 wenn nicht gefunden

Implementation

Function ResolveJoystickNameToIndex(Const aName: String; aIndex: integer): integer;
Var
  index, i: Integer;
  s: String;
Begin
  result := -1;
  index := 0;
  If aname = '' Then exit;
  For i := 0 To SDL_NumJoysticks() - 1 Do Begin
    s := SDL_JoystickNameForIndex(i);
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

{ TSDL_Joystick }

Function TSDL_Joystick.GetAxisValue(index: integer): integer;
Begin
  result := SDL_JoystickGetAxis(fInstance, index);
End;

Function TSDL_Joystick.GetButtonValue(index: integer): boolean;
Begin
  result := SDL_JoystickGetButton(fInstance, index) = SDL_PRESSED;
End;

Constructor TSDL_Joystick.Create(Index: integer);
Begin
  Inherited create;
  If (SDL_WasInit(SDL_INIT_JOYSTICK) And SDL_INIT_JOYSTICK) = 0 Then Begin
    Raise Exception.create('Error SDL subsystem for joystick is not initialized.');
  End;
  SDL_JoystickEventState(SDL_ENABLE);
  fInstance := SDL_JoystickOpen(Index);
  If Not assigned(fInstance) Then Begin
    Raise Exception.create('Error could not create joystick instance.');
  End;
  fAxisCount := SDL_JoystickNumAxes(fInstance);
  fButtonCount := SDL_JoystickNumButtons(fInstance);
End;

Destructor TSDL_Joystick.Destroy;
Begin
  If assigned(fInstance) Then Begin
    SDL_JoystickClose(fInstance);
    fInstance := Nil;
  End;
End;

End.

