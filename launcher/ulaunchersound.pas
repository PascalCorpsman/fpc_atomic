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
Unit ulaunchersound;

{$MODE ObjFPC}{$H+}

Interface

(*
 * Sound test of the launcher: plays a few sound files one after the other (e.g. an explosion, a voice and the start of
 * the menu music) without blocking the GUI.
 *
 * Uses the BASS library like the game, but loads it dynamically (units/bass.pas links it statically, then the launcher
 * would not start at all without bass.dll / libbass.so). The library is loaded and initialized on the first Play,
 * a missing library or a failed init only disables the sound test (see Failed, Error).
 *)

Uses
  Classes, SysUtils, ExtCtrls, dynlibs;

Type

  TSoundTestStep = Record
    Filename: String; // '' = skipped
    MaxMs: integer; // 0 = play to the end, else stop after MaxMs (with a short fade out)
  End;

  TSoundTestSteps = Array Of TSoundTestStep;

  { TSoundTest }

  TSoundTest = Class
  private
    fLib: TLibHandle;
    fInitDone: Boolean; // BASS_Init was successful
    fFailed: Boolean; // Library missing or BASS_Init failed, then for the whole run
    fError: String;
    fSteps: TSoundTestSteps;
    fStep: integer;
    fStream: DWord;
    fStepStart: QWord;
    fWaitUntil: QWord; // Short pause between two steps
    fFading: Boolean;
    fQuiet: Boolean; // No OnFinished (while Play starts)
    fTimer: TTimer;
    fOnFinished: TNotifyEvent;
    Function LoadLib(): Boolean;
    Function Init(): Boolean;
    Procedure FreeStream();
    Procedure StartNextStep();
    Procedure Finish();
    Procedure OnTimer(Sender: TObject);
  public
    Constructor Create();
    Destructor Destroy(); override;
    (*
     * Starts the steps one after the other, Volume 0..10000 (like [FPC_Atomic] Volume of the game).
     * False if nothing could be started (library / device not available, see Error, or no file of Steps exists).
     *)
    Function Play(Const Steps: TSoundTestSteps; Volume: integer): Boolean;
    Procedure Stop(); // Stops immediately, OnFinished is called if something was playing
    Function Playing(): Boolean;
    Property Failed: Boolean read fFailed;
    Property Error: String read fError;
    Property OnFinished: TNotifyEvent read fOnFinished write fOnFinished; // Played to the end or stopped
  End;

Function SoundLibraryFile(): String; // The BASS library next to the executable
Function SoundLibraryAvailable(): Boolean; // Can BASS be loaded at all (without loading it, Linux: assumed true)

Implementation

Uses
  uassetpath;

Const
{$IFDEF Windows}
  BassLibName = 'bass.dll';
{$ELSE}
{$IFDEF Darwin}
  BassLibName = 'libbass.dylib';
{$ELSE}
  BassLibName = 'libbass.so';
{$ENDIF}
{$ENDIF}

  // From units/bass.pas
  BASS_CONFIG_GVOL_STREAM = 5;
  BASS_DEVICE_DMIX = $2000;
  BASS_UNICODE = $80000000;
  BASS_ACTIVE_STOPPED = 0;
  BASS_ATTRIB_VOL = 2;

  StepPauseMs = 300;
  FadeMs = 800;

Type
  TBASS_GetVersion = Function(): DWord; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_Init = Function(device: Integer; freq, flags: DWord; win: Pointer; clsid: Pointer): LongBool; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_Free = Function(): LongBool; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_ErrorGetCode = Function(): Integer; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_SetConfig = Function(option, value: DWord): LongBool; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_StreamCreateFile = Function(mem: DWord; f: Pointer; offset, length: QWord; flags: DWord): DWord; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_StreamFree = Function(handle: DWord): LongBool; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_ChannelPlay = Function(handle: DWord; restart: LongBool): LongBool; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_ChannelStop = Function(handle: DWord): LongBool; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_ChannelIsActive = Function(handle: DWord): DWord; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};
  TBASS_ChannelSlideAttribute = Function(handle, attrib: DWord; value: Single; time: DWord): LongBool; {$IFDEF Windows}stdcall{$ELSE}cdecl{$ENDIF};

Var
  BASS_GetVersion: TBASS_GetVersion = Nil;
  BASS_Init: TBASS_Init = Nil;
  BASS_Free: TBASS_Free = Nil;
  BASS_ErrorGetCode: TBASS_ErrorGetCode = Nil;
  BASS_SetConfig: TBASS_SetConfig = Nil;
  BASS_StreamCreateFile: TBASS_StreamCreateFile = Nil;
  BASS_StreamFree: TBASS_StreamFree = Nil;
  BASS_ChannelPlay: TBASS_ChannelPlay = Nil;
  BASS_ChannelStop: TBASS_ChannelStop = Nil;
  BASS_ChannelIsActive: TBASS_ChannelIsActive = Nil;
  BASS_ChannelSlideAttribute: TBASS_ChannelSlideAttribute = Nil;

Function SoundLibraryFile: String;
Begin
  result := AppDir() + BassLibName;
End;

Function SoundLibraryAvailable: Boolean;
Begin
{$IFDEF Windows}
  result := FileExists(SoundLibraryFile());
{$ELSE}
  result := true; // Linux: usually in /usr/lib (see README), only loading it tells
{$ENDIF}
End;

{ TSoundTest }

Constructor TSoundTest.Create;
Begin
  Inherited Create;
  fLib := NilHandle;
  fInitDone := false;
  fFailed := false;
  fError := '';
  fSteps := Nil;
  fStream := 0;
  fQuiet := false;
  fTimer := TTimer.Create(Nil);
  fTimer.Enabled := false;
  fTimer.Interval := 50;
  fTimer.OnTimer := @OnTimer;
End;

Destructor TSoundTest.Destroy;
Begin
  fOnFinished := Nil; // The owner is going away
  Stop();
  fTimer.Free;
  If fInitDone Then BASS_Free();
  fInitDone := false;
  If fLib <> NilHandle Then UnloadLibrary(fLib);
  fLib := NilHandle;
  Inherited Destroy;
End;

Function TSoundTest.LoadLib: Boolean;

  Function Get(Const Name: String): Pointer;
  Begin
    result := GetProcedureAddress(fLib, Name);
    If result = Nil Then Raise Exception.Create(Name + ' not found in ' + BassLibName);
  End;

Begin
  result := fLib <> NilHandle;
  If result Then exit;
  If FileExists(SoundLibraryFile()) Then fLib := LoadLibrary(SoundLibraryFile());
{$IFNDEF Windows}
  If fLib = NilHandle Then fLib := LoadLibrary(BassLibName); // System folders (e.g. /usr/lib)
{$ENDIF}
  If fLib = NilHandle Then Begin
    fError := BassLibName + ' could not be loaded.';
    exit;
  End;
  Try
    Pointer(BASS_GetVersion) := Get('BASS_GetVersion');
    Pointer(BASS_Init) := Get('BASS_Init');
    Pointer(BASS_Free) := Get('BASS_Free');
    Pointer(BASS_ErrorGetCode) := Get('BASS_ErrorGetCode');
    Pointer(BASS_SetConfig) := Get('BASS_SetConfig');
    Pointer(BASS_StreamCreateFile) := Get('BASS_StreamCreateFile');
    Pointer(BASS_StreamFree) := Get('BASS_StreamFree');
    Pointer(BASS_ChannelPlay) := Get('BASS_ChannelPlay');
    Pointer(BASS_ChannelStop) := Get('BASS_ChannelStop');
    Pointer(BASS_ChannelIsActive) := Get('BASS_ChannelIsActive');
    Pointer(BASS_ChannelSlideAttribute) := Get('BASS_ChannelSlideAttribute');
    // The API of 2.4 (the one of units/bass.pas)
    If (BASS_GetVersion() Shr 16) <> $0204 Then Raise Exception.Create(BassLibName + ' has the wrong version (2.4 needed).');
  Except
    On e: Exception Do Begin
      fError := e.Message;
      UnloadLibrary(fLib);
      fLib := NilHandle;
      exit;
    End;
  End;
  result := true;
End;

Function TSoundTest.Init: Boolean;
Begin
  result := fInitDone;
  If result Or fFailed Then exit;
  If Not LoadLib() Then Begin
    fFailed := true;
    exit;
  End;
  // Like the game (client/unit1.pas): default device, DMIX so that other applications can still play sounds (ALSA)
  If Not BASS_Init(-1, 44100, BASS_DEVICE_DMIX, Nil, Nil) Then Begin
    fError := 'BASS_Init failed, error code ' + inttostr(BASS_ErrorGetCode());
    fFailed := true;
    exit;
  End;
  fInitDone := true;
  result := true;
End;

Procedure TSoundTest.FreeStream;
Begin
  If fStream <> 0 Then Begin
    BASS_ChannelStop(fStream);
    BASS_StreamFree(fStream);
  End;
  fStream := 0;
End;

Procedure TSoundTest.StartNextStep;
Var
  f: String;
{$IFDEF Windows}
  w: UnicodeString;
{$ENDIF}
Begin
  // The next step that can be played (missing or broken files are skipped)
  FreeStream();
  fFading := false;
  While fStep < length(fSteps) Do Begin
    f := fSteps[fStep].Filename;
    If (f <> '') And FileExists(f) Then Begin
{$IFDEF Windows}
      w := UTF8Decode(f);
      fStream := BASS_StreamCreateFile(0, PWideChar(w), 0, 0, BASS_UNICODE);
{$ELSE}
      fStream := BASS_StreamCreateFile(0, PChar(f), 0, 0, 0);
{$ENDIF}
      If (fStream <> 0) And Not BASS_ChannelPlay(fStream, true) Then FreeStream();
      If fStream <> 0 Then Begin
        fStepStart := GetTickCount64;
        exit;
      End;
    End;
    inc(fStep);
  End;
  Finish();
End;

Procedure TSoundTest.Finish;
Begin
  FreeStream();
  fTimer.Enabled := false;
  fSteps := Nil;
  fStep := 0;
  If assigned(fOnFinished) And (Not fQuiet) Then fOnFinished(self);
End;

Procedure TSoundTest.OnTimer(Sender: TObject);
Var
  Elapsed: QWord;
Begin
  If fStream = 0 Then Begin
    // Pause between two steps
    If GetTickCount64 >= fWaitUntil Then StartNextStep();
    exit;
  End;
  Elapsed := GetTickCount64 - fStepStart;
  If (BASS_ChannelIsActive(fStream) = BASS_ACTIVE_STOPPED) Or
    ((fSteps[fStep].MaxMs > 0) And (Elapsed >= QWord(fSteps[fStep].MaxMs))) Then Begin
    FreeStream();
    inc(fStep);
    If fStep >= length(fSteps) Then Begin
      Finish();
    End
    Else Begin
      fWaitUntil := GetTickCount64 + StepPauseMs;
    End;
    exit;
  End;
  // Fade out shortly before the end of a shortened step (music)
  If (fSteps[fStep].MaxMs > FadeMs) And (Not fFading) And (Elapsed + FadeMs >= QWord(fSteps[fStep].MaxMs)) Then Begin
    fFading := true;
    BASS_ChannelSlideAttribute(fStream, BASS_ATTRIB_VOL, 0, FadeMs);
  End;
End;

Function TSoundTest.Play(Const Steps: TSoundTestSteps; Volume: integer): Boolean;
Begin
  result := false;
  Stop();
  If Not Init() Then exit;
  If Volume < 0 Then Volume := 0;
  If Volume > 10000 Then Volume := 10000;
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  fSteps := Copy(Steps);
  fStep := 0;
  fQuiet := true; // Nothing to play: no OnFinished, the caller sees result = false
  Try
    StartNextStep();
  Finally
    fQuiet := false;
  End;
  result := fStream <> 0;
  If result Then fTimer.Enabled := true;
End;

Procedure TSoundTest.Stop;
Begin
  If (fStream = 0) And (Not fTimer.Enabled) Then exit;
  Finish();
End;

Function TSoundTest.Playing: Boolean;
Begin
  result := (fStream <> 0) Or fTimer.Enabled;
End;

End.
