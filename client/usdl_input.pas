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
  Classes, SysUtils, controls, uatomic_common, usdl_joystick, usdl_gamecontroller, sdl2;

Procedure SDL_Init;
Procedure SDL_Quit;

Procedure SDL_CreateSticks(Const akeys: TKeyArray);
Procedure SDL_FreeSticks;

Procedure SDL_SticksToKeyEvent(Const OnKeyDown, OnKeyUp: TAtomicKeyEvent);

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
  i: TKeySet;
Begin
  If Not fsdl_Loaded Then exit;
  SDL_FreeSticks; // Falls es die schon gibt ;)
  fkeys := akeys;
  For i In TKeySet Do Begin
    If fKeys[i].UseSDL2 Then Begin
      // Das Gerät kann zwischen dem Auflösen und dem Öffnen verschwinden, dann bleibt der Spieler ohne Gerät, bis es wieder eingesteckt wird
      Try
        If fKeys[i].IsGameController Then Begin
          index := ResolveGameControllerNameToIndex(fKeys[i].Name, fKeys[i].NameIndex);
          If index <> -1 Then Begin
            fsdlJoysticks[i] := TSDL_GameController.create(index);
          End;
        End
        Else Begin
          index := ResolveJoystickNameToIndex(fKeys[i].Name, fKeys[i].NameIndex);
          If index <> -1 Then Begin
            fsdlJoysticks[i] := TSDL_Joystick.Create(index);
          End;
        End;
      Except
        fsdlJoysticks[i] := Nil;
      End;
    End;
  End;
End;

Procedure SDL_SticksToKeyEvent(Const OnKeyDown, OnKeyUp: TAtomicKeyEvent);

  Procedure Call(Var Last: Boolean; Event: Boolean; key: TAtomicKey; aKeyset: TKeySet);
  Begin
    If last <> Event Then Begin
      last := Event; // In Case the OnKey* is blocking, we need to clear the flag first!
      If Event Then Begin
        OnKeyDown(key, aKeyset);
      End
      Else Begin
        OnKeyUp(key, aKeyset);
      End;
    End;
  End;

  (*
   * True, wenn die Achse mit dem Abstand d zur Ruhestellung in Richtung aSign ausgelenkt ist.
   * Mit Hysterese bleibt eine bereits gedrückte Richtung bis AchsisReleaseTrigger gedrückt.
   *)
  Function Deflected(d, aSign: Integer; WasPressed, Hysteresis: Boolean): Boolean;
  Var
    t: Integer;
  Begin
    t := Achsistrigger;
    If WasPressed And Hysteresis Then t := AchsisReleaseTrigger;
    result := (abs(d) > t) And (sign(d) = aSign);
  End;

  (*
   * Diagonal gehaltener Stick: True, wenn die senkrechte Achse gewinnt. Die bisherige Achse behält die Richtung,
   * bis die andere um DominantAxisMargin stärker ist, sonst würde die Richtung bei 45° hin und her springen.
   *)
  Function VerticalWins(mv, mh: Integer; Const Last: TKeyState): Boolean;
  Begin
    If (Last.Up Or Last.Down) And Not (Last.Left Or Last.Right) Then Begin
      result := mh * 100 <= mv * DominantAxisMargin;
    End
    Else If (Last.Left Or Last.Right) And Not (Last.Up Or Last.Down) Then Begin
      result := mv * 100 > mh * DominantAxisMargin;
    End
    Else Begin
      result := mv >= mh;
    End;
  End;

  Procedure CheckKeys(Keys: TKeySet);
  Var
    d, h, mv, mh: Integer;
    up, down, left, right, aUp, aDown, aLeft, aRight, first, second, hyst: Boolean;
    gc: TSDL_GameController;
  Begin
    If Not assigned(fsdlJoysticks[keys]) Then exit;
    hyst := fKeys[keys].UseHysteresis;
    // 1. Ermitteln des Aktuellen "Gedrückt" stati, getrennt nach digital (Steuerkreuz, Hat) und analog (Stick, Achsen)
    up := false;
    down := false;
    left := false;
    right := false;
    aUp := false;
    aDown := false;
    aLeft := false;
    aRight := false;
    mv := 0;
    mh := 0;
    If fsdlJoysticks[keys] Is TSDL_GameController Then Begin
      gc := TSDL_GameController(fsdlJoysticks[keys]);
      up := gc.Button[SDL_CONTROLLER_BUTTON_DPAD_UP];
      down := gc.Button[SDL_CONTROLLER_BUTTON_DPAD_DOWN];
      left := gc.Button[SDL_CONTROLLER_BUTTON_DPAD_LEFT];
      right := gc.Button[SDL_CONTROLLER_BUTTON_DPAD_RIGHT];
      If fKeys[keys].UseAnalogStick Then Begin
        // Negative Werte sind oben bzw. links
        d := -gc.Axis[SDL_CONTROLLER_AXIS_LEFTY];
        aUp := Deflected(d, 1, LastKeyState[Keys].Up, hyst);
        aDown := Deflected(d, -1, LastKeyState[Keys].Down, hyst);
        mv := abs(d);
        d := -gc.Axis[SDL_CONTROLLER_AXIS_LEFTX];
        aLeft := Deflected(d, 1, LastKeyState[Keys].Left, hyst);
        aRight := Deflected(d, -1, LastKeyState[Keys].Right, hyst);
        mh := abs(d);
      End;
    End
    Else Begin
      If fKeys[keys].AchsisIndex[0] >= 0 Then Begin
        d := fKeys[keys].AchsisIdle[0] - fsdlJoysticks[keys].Axis[fKeys[keys].AchsisIndex[0]];
        aUp := Deflected(d, fKeys[keys].AchsisDirection[0], LastKeyState[Keys].Up, hyst);
        aDown := Deflected(d, -fKeys[keys].AchsisDirection[0], LastKeyState[Keys].Down, hyst);
        mv := abs(d);
      End;
      If fKeys[keys].AchsisIndex[1] >= 0 Then Begin
        d := fKeys[keys].AchsisIdle[1] - fsdlJoysticks[keys].Axis[fKeys[keys].AchsisIndex[1]];
        aLeft := Deflected(d, fKeys[keys].AchsisDirection[1], LastKeyState[Keys].Left, hyst);
        aRight := Deflected(d, -fKeys[keys].AchsisDirection[1], LastKeyState[Keys].Right, hyst);
        mh := abs(d);
      End;
      If fKeys[keys].UseHat And (fsdlJoysticks[keys].HatCount > 0) Then Begin
        h := fsdlJoysticks[keys].Hat[0];
        up := (h And SDL_HAT_UP) <> 0;
        down := (h And SDL_HAT_DOWN) <> 0;
        left := (h And SDL_HAT_LEFT) <> 0;
        right := (h And SDL_HAT_RIGHT) <> 0;
      End;
    End;
    // Diagonal gehaltener Stick: nur die stärkere Achse zählt, sonst gewinnt die zuletzt über die Schwelle gegangene Richtung
    If fKeys[keys].UseDominantAxis And (aUp Or aDown) And (aLeft Or aRight) Then Begin
      If VerticalWins(mv, mh, LastKeyState[Keys]) Then Begin
        aLeft := false;
        aRight := false;
      End
      Else Begin
        aUp := false;
        aDown := false;
      End;
    End;
    up := up Or aUp;
    down := down Or aDown;
    left := left Or aLeft;
    right := right Or aRight;
    // Gedrückt ist ein Button, sobald er nicht mehr im Ruhezustand ist
    first := fKeys[keys].ButtonsIdle[0] <> fsdlJoysticks[keys].Button[fKeys[keys].ButtonIndex[0]];
    second := fKeys[keys].ButtonsIdle[1] <> fsdlJoysticks[keys].Button[fKeys[keys].ButtonIndex[1]];
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
      Call(LastKeyState[Keys].Up, up, akUp, keys);
      Call(LastKeyState[Keys].Down, Down, akDown, keys);
      Call(LastKeyState[Keys].Left, left, akLeft, keys);
      Call(LastKeyState[Keys].Right, right, akRight, keys);
      Call(LastKeyState[Keys].first, first, akFirstAction, keys);
      Call(LastKeyState[Keys].second, second, akSecondAction, keys);
    End;
  End;

  // Lässt alles los, was noch als gedrückt gilt, sonst läuft die Figur nach dem Abziehen des Controllers weiter
  Procedure ReleaseKeys(Keys: TKeySet);
  Begin
    If Not LastKeyState[Keys].Initialized Then exit;
    Call(LastKeyState[Keys].Up, false, akUp, keys);
    Call(LastKeyState[Keys].Down, false, akDown, keys);
    Call(LastKeyState[Keys].Left, false, akLeft, keys);
    Call(LastKeyState[Keys].Right, false, akRight, keys);
    Call(LastKeyState[Keys].first, false, akFirstAction, keys);
    Call(LastKeyState[Keys].second, false, akSecondAction, keys);
  End;

Var
  event: TSDL_Event;
  DevicesChanged: Boolean;
Begin
  If Not fsdl_Loaded Then exit;
  SDL_PumpEvents();
  DevicesChanged := false;
  While SDL_PollEvent(@event) <> 0 Do Begin
    Case event.type_ Of
      SDL_JOYDEVICEADDED,
        SDL_JOYDEVICEREMOVED,
        SDL_CONTROLLERDEVICEADDED,
        SDL_CONTROLLERDEVICEREMOVED: DevicesChanged := true;
    End;
  End;
  If DevicesChanged Then Begin
    // Ein Controller wurde ein- oder ausgesteckt (Funk-Pads z.B. bei leerem Akku), also alle Geräte neu öffnen
    ReleaseKeys(ks0);
    ReleaseKeys(ks1);
    SDL_CreateSticks(fkeys);
  End;
  CheckKeys(ks0);
  CheckKeys(ks1);
End;

End.

