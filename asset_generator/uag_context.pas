(******************************************************************************)
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
(*
 * Generation context: main seed, style, seeds per area, parameters.
 *
 * Every area (e.g. 'bomb', 'atomic', 'atomic/p3', 'map:Field02') gets its own
 * seed derived from the main seed and its name. A seed can be locked per area
 * in the profile, then this area stays the same while the others change.
 *
 * Parameters: Area.Vary('headsize', 1.0, 0.08) returns a value in
 * 1.0 +- 0.08, it only depends on the seed of the area and the parameter name
 * (not on the order of the calls). The profile can override every value. All
 * parameters that were asked for are recorded, so the UI can list them.
 *
 * profile.ini:
 *   [General]
 *   Name=My set
 *   Seed=123456
 *   Style=flat            (flat | soft | pixel)
 *   PerPlayer=0           (1 = own atomic per player, atomic/p0 .. p9)
 *   [Seeds]
 *   bomb=777              (locks the seed of an area)
 *   [Params]
 *   atomic.headsize=1.1   (overrides a parameter)
 *)
Unit uag_context;

{$MODE ObjFPC}{$H+}
{$MODESWITCH advancedrecords}

Interface

Uses Classes, uag_random;

Type

  TAGStyle = (asFlat, asSoft, asPixel);

  TAGParamInfo = Record
    Area, Name: String;
    Base, Spread, Value: Double;
    Overridden: Boolean;
    IsPick: Boolean; // integer choice 0 .. Spread (Pick, Chance), else Base +- Spread (Vary)
  End;

  TAGContext = Class;

  { TAGArea: random source of one area, cheap to copy }

  TAGArea = Record
    Ctx: TAGContext;
    Name: String;
    Seed: QWord;
    Function Vary(Const Param: String; Base, Spread: Double): Double; // Base +- Spread, overridable
    Function Pick(Const Param: String; Count: Integer): Integer; // 0..Count-1, overridable
    Function Chance(Const Param: String; p: Double): Boolean; // overridable (0 / 1)
    Function Rnd(Const Key: String; Index: Integer = 0): Double; // [0, 1), for details, not listed in the UI
    Function Stream(Const Key: String): TAGRandom; // sequence of random numbers, e.g. for sparks
    Function Sub(Const SubName: String): TAGArea; // e.g. Area('atomic').Sub('p3')
  End;

  { TAGContext }

  TAGContext = Class
  private
    fParams: Array Of TAGParamInfo;
    fOverrides: TStringList; // area.param=value
    fLockedSeeds: TStringList; // area=seed
    fLock: TRTLCriticalSection; // generators may run in threads
    Procedure LogParam(Const Info: TAGParamInfo);
    Function GetParamCount: Integer;
    Function GetParam(Index: Integer): TAGParamInfo;
  public
    Name: String;
    Seed: QWord;
    Style: TAGStyle;
    PerPlayer: Boolean;
    Constructor Create;
    Destructor Destroy; override;
    Procedure Assign(Src: TAGContext); // everything except the parameter log
    Function Clone: TAGContext; // e.g. for a worker thread, the UI keeps its own context
    Function Area(Const AreaName: String): TAGArea;
    Function AreaSeed(Const AreaName: String): QWord;
    Procedure LockSeed(Const AreaName: String; aSeed: QWord);
    Procedure UnlockSeed(Const AreaName: String);
    Function IsSeedLocked(Const AreaName: String): Boolean;
    Procedure SetOverride(Const AreaName, Param: String; Value: Double);
    Procedure ClearOverride(Const AreaName, Param: String);
    Function FindOverride(Const AreaName, Param: String; Out Value: Double): Boolean;
    Procedure ClearParamLog;
    Property ParamCount: Integer read GetParamCount;
    Property Params[Index: Integer]: TAGParamInfo read GetParam;
    Procedure LoadFromFile(Const Filename: String);
    Procedure SaveToFile(Const Filename: String);
  End;

Function StyleToString(s: TAGStyle): String;
Function StringToStyle(Const s: String; Default: TAGStyle = asFlat): TAGStyle;
Function AGFloatToStr(v: Double): String; // with '.', independent of the locale
Function AGStrToFloat(Const s: String; Default: Double): Double;

Implementation

Uses SysUtils, IniFiles, uag_math;

Var
  InvFormat: TFormatSettings;

Function StyleToString(s: TAGStyle): String;
Begin
  Case s Of
    asSoft: result := 'soft';
    asPixel: result := 'pixel';
  Else
    result := 'flat';
  End;
End;

Function StringToStyle(Const s: String; Default: TAGStyle): TAGStyle;
Begin
  Case LowerCase(Trim(s)) Of
    'flat': result := asFlat;
    'soft': result := asSoft;
    'pixel': result := asPixel;
  Else
    result := Default;
  End;
End;

Function AGFloatToStr(v: Double): String;
Begin
  result := FloatToStrF(v, ffGeneral, 15, 0, InvFormat);
End;

Function AGStrToFloat(Const s: String; Default: Double): Double;
Begin
  result := StrToFloatDef(Trim(s), Default, InvFormat);
End;

{ TAGArea }

Function TAGArea.Vary(Const Param: String; Base, Spread: Double): Double;
Var
  info: TAGParamInfo;
  v: Double;
Begin
  info.Area := Name;
  info.Name := Param;
  info.Base := Base;
  info.Spread := Spread;
  info.IsPick := false;
  info.Overridden := Ctx.FindOverride(Name, Param, v);
  If Not info.Overridden Then
    v := Base + Spread * (2 * HashUnit(Seed, Param) - 1);
  info.Value := v;
  Ctx.LogParam(info);
  result := v;
End;

Function TAGArea.Pick(Const Param: String; Count: Integer): Integer;
Var
  info: TAGParamInfo;
  v: Double;
Begin
  If Count <= 1 Then exit(0);
  info.Area := Name;
  info.Name := Param;
  info.Base := 0;
  info.Spread := Count - 1; // UI: integer choice 0 .. Spread
  info.IsPick := true;
  info.Overridden := Ctx.FindOverride(Name, Param, v);
  If info.Overridden Then
    result := AGClampI(AGRound(v), 0, Count - 1)
  Else
    result := AGClampI(AGFloor(HashUnit(Seed, Param) * Count), 0, Count - 1);
  info.Value := result;
  Ctx.LogParam(info);
End;

Function TAGArea.Chance(Const Param: String; p: Double): Boolean;
Begin
  result := Pick(Param + '?', 1000000) < AGRound(p * 1000000);
End;

Function TAGArea.Rnd(Const Key: String; Index: Integer): Double;
Begin
  result := HashUnitI(Seed, Key, Index);
End;

Function TAGArea.Stream(Const Key: String): TAGRandom;
Begin
  result.Init(DeriveSeed(Seed, Key));
End;

Function TAGArea.Sub(Const SubName: String): TAGArea;
Begin
  result := Ctx.Area(Name + '/' + SubName);
End;

{ TAGContext }

Constructor TAGContext.Create;
Begin
  Inherited Create;
  fOverrides := TStringList.Create;
  fLockedSeeds := TStringList.Create;
  InitCriticalSection(fLock);
  Name := 'Generated';
  Seed := 1;
  Style := asFlat;
  PerPlayer := false;
End;

Destructor TAGContext.Destroy;
Begin
  fOverrides.Free;
  fLockedSeeds.Free;
  DoneCriticalSection(fLock);
  Inherited Destroy;
End;

Procedure TAGContext.Assign(Src: TAGContext);
Begin
  Name := Src.Name;
  Seed := Src.Seed;
  Style := Src.Style;
  PerPlayer := Src.PerPlayer;
  fOverrides.Assign(Src.fOverrides);
  fLockedSeeds.Assign(Src.fLockedSeeds);
End;

Function TAGContext.Clone: TAGContext;
Begin
  result := TAGContext.Create;
  result.Assign(self);
End;

Procedure TAGContext.LogParam(Const Info: TAGParamInfo);
Var
  i: Integer;
Begin
  EnterCriticalSection(fLock);
  Try
    For i := 0 To high(fParams) Do
      If (fParams[i].Area = Info.Area) And (fParams[i].Name = Info.Name) Then Begin
        fParams[i] := Info;
        exit;
      End;
    setlength(fParams, length(fParams) + 1);
    fParams[high(fParams)] := Info;
  Finally
    LeaveCriticalSection(fLock);
  End;
End;

Function TAGContext.GetParamCount: Integer;
Begin
  result := length(fParams);
End;

Function TAGContext.GetParam(Index: Integer): TAGParamInfo;
Begin
  result := fParams[Index];
End;

Procedure TAGContext.ClearParamLog;
Begin
  fParams := Nil;
End;

Function TAGContext.AreaSeed(Const AreaName: String): QWord;
Var
  s: String;
Begin
  s := fLockedSeeds.Values[AreaName];
  If s <> '' Then
    result := StrToQWordDef(s, 0)
  Else
    result := DeriveSeed(Seed, AreaName);
End;

Function TAGContext.Area(Const AreaName: String): TAGArea;
Begin
  result.Ctx := self;
  result.Name := AreaName;
  result.Seed := AreaSeed(AreaName);
End;

Procedure TAGContext.LockSeed(Const AreaName: String; aSeed: QWord);
Begin
  fLockedSeeds.Values[AreaName] := IntToStr(aSeed);
End;

Procedure TAGContext.UnlockSeed(Const AreaName: String);
Var
  i: Integer;
Begin
  i := fLockedSeeds.IndexOfName(AreaName);
  If i >= 0 Then fLockedSeeds.Delete(i);
End;

Function TAGContext.IsSeedLocked(Const AreaName: String): Boolean;
Begin
  result := fLockedSeeds.IndexOfName(AreaName) >= 0;
End;

Procedure TAGContext.SetOverride(Const AreaName, Param: String; Value: Double);
Begin
  fOverrides.Values[AreaName + '.' + Param] := AGFloatToStr(Value);
End;

Procedure TAGContext.ClearOverride(Const AreaName, Param: String);
Var
  i: Integer;
Begin
  i := fOverrides.IndexOfName(AreaName + '.' + Param);
  If i >= 0 Then fOverrides.Delete(i);
End;

Function TAGContext.FindOverride(Const AreaName, Param: String; Out Value: Double): Boolean;
Var
  i: Integer;
Begin
  Value := 0;
  i := fOverrides.IndexOfName(AreaName + '.' + Param);
  result := i >= 0;
  If result Then Value := AGStrToFloat(fOverrides.ValueFromIndex[i], 0);
End;

Procedure TAGContext.LoadFromFile(Const Filename: String);
Var
  ini: TIniFile;
  sl: TStringList;
  i: Integer;
Begin
  ini := TIniFile.Create(Filename);
  sl := TStringList.Create;
  Try
    Name := ini.ReadString('General', 'Name', Name);
    Seed := StrToQWordDef(ini.ReadString('General', 'Seed', IntToStr(Seed)), Seed);
    Style := StringToStyle(ini.ReadString('General', 'Style', StyleToString(Style)), Style);
    PerPlayer := ini.ReadBool('General', 'PerPlayer', PerPlayer);
    fLockedSeeds.Clear;
    ini.ReadSectionValues('Seeds', sl);
    For i := 0 To sl.Count - 1 Do
      fLockedSeeds.Add(sl[i]);
    fOverrides.Clear;
    sl.Clear;
    ini.ReadSectionValues('Params', sl);
    For i := 0 To sl.Count - 1 Do
      fOverrides.Add(sl[i]);
  Finally
    sl.Free;
    ini.Free;
  End;
End;

Procedure TAGContext.SaveToFile(Const Filename: String);
Var
  sl: TStringList;
  i: Integer;
Begin
  // Written by hand, so the file has a fixed order and no locale dependent content
  sl := TStringList.Create;
  Try
    sl.Add('[General]');
    sl.Add('Name=' + Name);
    sl.Add('Seed=' + IntToStr(Seed));
    sl.Add('Style=' + StyleToString(Style));
    sl.Add('PerPlayer=' + IntToStr(ord(PerPlayer)));
    sl.Add('');
    sl.Add('[Seeds]');
    For i := 0 To fLockedSeeds.Count - 1 Do
      sl.Add(fLockedSeeds[i]);
    sl.Add('');
    sl.Add('[Params]');
    For i := 0 To fOverrides.Count - 1 Do
      sl.Add(fOverrides[i]);
    sl.SaveToFile(Filename);
  Finally
    sl.Free;
  End;
End;

Initialization
  InvFormat := DefaultFormatSettings;
  InvFormat.DecimalSeparator := '.';
  InvFormat.ThousandSeparator := #0;

End.
