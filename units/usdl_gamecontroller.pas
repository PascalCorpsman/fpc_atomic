Unit usdl_gamecontroller;

{$MODE objfpc}{$H+}

Interface

Uses
	Classes, SysUtils,
	sdl2;

Type

	{ TSDL_GameControllerEx }

	TSDL_GameControllerEx = class
	private
		fJoy: PSDL_Joystick;
	public
		constructor Create(Index: Integer);
		destructor Destroy; override;
		function IsAttached: Boolean;
		// Axes (normalized GameController axes)
		function AxisLeftX: Integer; // -32768 .. 32767
		function AxisLeftY: Integer; // -32768 .. 32767
		// D-Pad
		function DpadUp: Boolean;
		function DpadDown: Boolean;
		function DpadLeft: Boolean;
		function DpadRight: Boolean;
		// Buttons
		function ButtonX: Boolean; // Primary as requested
		function ButtonSquare: Boolean; // Secondary as requested (on PlayStation layout)
	end;

// Helpers
function DetectGameControllerIndices(MaxCount: Integer; out idx0, idx1: Integer): Boolean;

Implementation

constructor TSDL_GameControllerEx.Create(Index: Integer);
begin
	inherited Create;
	if (SDL_WasInit(SDL_INIT_JOYSTICK) and SDL_INIT_JOYSTICK) = 0 then begin
		raise Exception.Create('SDL Joystick subsystem not initialized.');
	end;
	SDL_JoystickEventState(SDL_ENABLE);
	fJoy := SDL_JoystickOpen(Index);
	if not Assigned(fJoy) then begin
		raise Exception.Create('Could not open SDL Joystick for controller wrapper.');
	end;
end;

destructor TSDL_GameControllerEx.Destroy;
begin
	if Assigned(fJoy) then begin
		SDL_JoystickClose(fJoy);
		fJoy := nil;
	end;
	inherited Destroy;
end;

function TSDL_GameControllerEx.IsAttached: Boolean;
begin
	if not Assigned(fJoy) then exit(false);
	try
		Result := SDL_JoystickGetAttached(fJoy) = SDL_TRUE;
	except
		Result := false;
	end;
end;

function TSDL_GameControllerEx.AxisLeftX: Integer;
begin
	if not Assigned(fJoy) then exit(0);
	try
		// Common mapping: axis 0 = left stick X
		Result := SDL_JoystickGetAxis(fJoy, 0);
	except
		Result := 0;
	end;
end;

function TSDL_GameControllerEx.AxisLeftY: Integer;
begin
	if not Assigned(fJoy) then exit(0);
	try
		// Common mapping: axis 1 = left stick Y
		Result := SDL_JoystickGetAxis(fJoy, 1);
	except
		Result := 0;
	end;
end;

function TSDL_GameControllerEx.DpadUp: Boolean;
begin
	if not Assigned(fJoy) then exit(false);
	try
		// Prefer hat if present
		if SDL_JoystickNumHats(fJoy) > 0 then begin
			Result := (SDL_JoystickGetHat(fJoy, 0) and SDL_HAT_UP) <> 0;
		end else begin
			// Fallback: treat stick Y negative as up with threshold
			Result := AxisLeftY < -12000;
		end;
	except
		Result := false;
	end;
end;

function TSDL_GameControllerEx.DpadDown: Boolean;
begin
	if not Assigned(fJoy) then exit(false);
	try
		if SDL_JoystickNumHats(fJoy) > 0 then begin
			Result := (SDL_JoystickGetHat(fJoy, 0) and SDL_HAT_DOWN) <> 0;
		end else begin
			Result := AxisLeftY > 12000;
		end;
	except
		Result := false;
	end;
end;

function TSDL_GameControllerEx.DpadLeft: Boolean;
begin
	if not Assigned(fJoy) then exit(false);
	try
		if SDL_JoystickNumHats(fJoy) > 0 then begin
			Result := (SDL_JoystickGetHat(fJoy, 0) and SDL_HAT_LEFT) <> 0;
		end else begin
			Result := AxisLeftX < -12000;
		end;
	except
		Result := false;
	end;
end;

function TSDL_GameControllerEx.DpadRight: Boolean;
begin
	if not Assigned(fJoy) then exit(false);
	try
		if SDL_JoystickNumHats(fJoy) > 0 then begin
			Result := (SDL_JoystickGetHat(fJoy, 0) and SDL_HAT_RIGHT) <> 0;
		end else begin
			Result := AxisLeftX > 12000;
		end;
	except
		Result := false;
	end;
end;

function TSDL_GameControllerEx.ButtonX: Boolean;
begin
	if not Assigned(fJoy) then exit(false);
	try
		// PlayStation layout: Button 0 = X (bottom), Button 1 = Circle, Button 2 = Square, Button 3 = Triangle
		// Xbox layout: Button 0 = A, Button 1 = B, Button 2 = X, Button 3 = Y
		// Try both common mappings: PlayStation X is button 0, Xbox A (equivalent) is also button 0
		if SDL_JoystickNumButtons(fJoy) > 0 then begin
			Result := SDL_JoystickGetButton(fJoy, 0) = SDL_PRESSED;
		end else begin
			Result := false;
		end;
	except
		Result := false;
	end;
end;

function TSDL_GameControllerEx.ButtonSquare: Boolean;
begin
	if not Assigned(fJoy) then exit(false);
	try
		// PlayStation layout: Square is button 2
		// Xbox layout: X button is button 2 (equivalent position)
		if SDL_JoystickNumButtons(fJoy) > 2 then begin
			Result := SDL_JoystickGetButton(fJoy, 2) = SDL_PRESSED;
		end else begin
			Result := false;
		end;
	except
		Result := false;
	end;
end;

function DetectGameControllerIndices(MaxCount: Integer; out idx0, idx1: Integer): Boolean;
var
	i, found: Integer;
begin
	idx0 := -1;
	idx1 := -1;
	found := 0;
	for i := 0 to SDL_NumJoysticks() - 1 do begin
		// Without SDL_GameController API available, consider any joystick as candidate controller
		if found = 0 then begin
			idx0 := i;
			inc(found);
		end else if (found = 1) then begin
			idx1 := i;
			inc(found);
			break;
		end;
		if (MaxCount > 0) and (found >= MaxCount) then break;
	end;
	Result := idx0 <> -1;
end;

end.


