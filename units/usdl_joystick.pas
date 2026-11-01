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
    fHatCount: integer;
  protected
    fInstance: Pointer;
    fInstanceID: integer;
    Procedure InitDeviceInfo(Index: integer); // Muss von jedem Constructor nach dem Öffnen aufgerufen werden
    Function GetAxisCount: integer; virtual;
    Function GetAxisValue(index: integer): integer; virtual;
    Function GetButtonCount: integer; virtual;
    Function GetButtonValue(index: integer): boolean; virtual;
    Function GetHatCount: integer; virtual;
    Function GetHatValue(index: integer): integer; virtual;
  public
    Property InstanceID: integer read fInstanceID; // = "which" in den SDL Joystick und Gamecontroller Events, -1 wenn unbekannt

    Property ButtonCount: integer read getButtonCount;
    Property Button[index: integer]: boolean read GetButtonValue;

    Property HatCount: integer read GetHatCount;
    Property Hat[index: integer]: integer read GetHatValue; // Bitmaske aus SDL_HAT_UP, SDL_HAT_RIGHT, SDL_HAT_DOWN, SDL_HAT_LEFT

    Property AxisCount: integer read getAxisCount;
    Property Axis[index: integer]: integer read GetAxisValue; // ranging from -32768 to 32767

    Constructor Create(Index: integer); virtual;
    Destructor Destroy; override;
  End;

Function ResolveJoystickNameToIndex(Const aName: String; aIndex: integer = 0): integer; // -1 wenn nicht gefunden

Implementation

Uses dynlibs, math, ctypes;

(*
 * Die folgenden Funktionen sind in sdl2_for_pascal auch mit SDL_RUNTIME_LOADING
 * statisch gebunden, ein direkter Aufruf würde die SDL2 Lib zur Pflicht machen.
 * Deswegen werden sie hier selbst dynamisch geladen.
 *)
Type
  TJoystickNumHats = Function(joystick: Pointer): cint; cdecl;
  TJoystickGetHat = Function(joystick: Pointer; hat: cint): cuint8; cdecl;
  TJoystickGetDeviceInstanceID = Function(device_index: cint): cint32; cdecl;

Var
  ExtraLib: TLibHandle = NilHandle;
  JoystickNumHats: TJoystickNumHats = Nil;
  JoystickGetHat: TJoystickGetHat = Nil;
  JoystickGetDeviceInstanceID: TJoystickGetDeviceInstanceID = Nil;

Procedure LoadExtraFunctions;
Begin
  If ExtraLib <> NilHandle Then exit;
  ExtraLib := LoadLibrary(SDL_LibName);
  If ExtraLib = NilHandle Then exit;
  JoystickNumHats := TJoystickNumHats(GetProcAddress(ExtraLib, 'SDL_JoystickNumHats'));
  JoystickGetHat := TJoystickGetHat(GetProcAddress(ExtraLib, 'SDL_JoystickGetHat'));
  JoystickGetDeviceInstanceID := TJoystickGetDeviceInstanceID(GetProcAddress(ExtraLib, 'SDL_JoystickGetDeviceInstanceID'));
End;

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

Function TSDL_Joystick.getButtonCount: integer;
Begin
  result := fButtonCount;
End;

Function TSDL_Joystick.getAxisCount: integer;
Begin
  result := fAxisCount;
End;

Function TSDL_Joystick.GetAxisValue(index: integer): integer;
Begin
  result := SDL_JoystickGetAxis(fInstance, index);
End;

Function TSDL_Joystick.GetButtonValue(index: integer): boolean;
Begin
  result := SDL_JoystickGetButton(fInstance, index) = SDL_PRESSED;
End;

Function TSDL_Joystick.GetHatCount: integer;
Begin
  result := fHatCount;
End;

Function TSDL_Joystick.GetHatValue(index: integer): integer;
Begin
  result := SDL_HAT_CENTERED;
  If (index < 0) Or (index >= fHatCount) Or (Not assigned(JoystickGetHat)) Then exit;
  result := JoystickGetHat(fInstance, index);
End;

Procedure TSDL_Joystick.InitDeviceInfo(Index: integer);
Begin
  LoadExtraFunctions;
  fInstanceID := -1;
  If assigned(JoystickGetDeviceInstanceID) Then fInstanceID := JoystickGetDeviceInstanceID(Index);
  fHatCount := 0;
  // Bei abgeleiteten Klassen (Gamecontroller) ist fInstance kein SDL_Joystick, dort gibt es auch keine Hats
  If assigned(JoystickNumHats) And (ClassType = TSDL_Joystick) Then Begin
    fHatCount := max(0, JoystickNumHats(fInstance));
  End;
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
  InitDeviceInfo(Index);
End;

Destructor TSDL_Joystick.Destroy;
Begin
  If assigned(fInstance) Then Begin
    SDL_JoystickClose(fInstance);
    fInstance := Nil;
  End;
End;

Finalization
  If ExtraLib <> NilHandle Then UnloadLibrary(ExtraLib);

End.

