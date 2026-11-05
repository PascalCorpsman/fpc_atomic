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
 * Tests for ResolveStartSets (uassetpath): which graphics and sound set the
 * game (fpc_atomic) starts with, from its parameters -assets / -sounds or the
 * launcher settings [Launcher] AssetSet / SoundSet, and whether it has any
 * graphics at all. No GUI, works in a temporary folder (AppDirOverride).
 *
 *   fpc -Fu../units test_startsets.lpr && ./test_startsets
 *
 * Exit code 0 = all tests passed.
 *)
Program test_startsets;

{$MODE ObjFPC}{$H+}

Uses
  Classes, SysUtils, uassetpath;

Var
  Root: String;
  Failed, Passed: integer;

Procedure WriteFile(Const Rel, Content: String);
Var
  sl: TStringList;
Begin
  ForceDirectories(ExtractFilePath(Root + Rel));
  sl := TStringList.Create;
  Try
    sl.Text := Content;
    sl.SaveToFile(Root + Rel);
  Finally
    sl.free;
  End;
End;

Procedure DeleteTree(Const Dir: String);
Var
  sr: TSearchRec;
Begin
  If FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, sr) = 0 Then Begin
    Repeat
      If (sr.Name = '.') Or (sr.Name = '..') Then Continue;
      If (sr.Attr And faDirectory) <> 0 Then
        DeleteTree(IncludeTrailingPathDelimiter(Dir) + sr.Name)
      Else
        DeleteFile(IncludeTrailingPathDelimiter(Dir) + sr.Name);
    Until FindNext(sr) <> 0;
    FindClose(sr);
  End;
  RemoveDir(Dir);
End;

Procedure MakeSet(Const Folder: String; WithMenu, WithSounds: Boolean);
Begin
  WriteFile('assets' + PathDelim + Folder + PathDelim + 'profile.ini', '[General]' + LineEnding + 'Name=' + Folder);
  If WithMenu Then WriteFile('assets' + PathDelim + Folder + PathDelim + 'res' + PathDelim + 'mainmenu.png', 'png');
  If WithSounds Then WriteFile('assets' + PathDelim + Folder + PathDelim + 'sounds' + PathDelim + 'bmdrop1.wav', 'wav');
End;

Procedure WriteIni(Const AssetSetting, SoundSetting: String);
Begin
  WriteFile('fpc_atomic.cfg', '[Launcher]' + LineEnding + 'AssetSet=' + AssetSetting + LineEnding + 'SoundSet=' + SoundSetting);
End;

Procedure Check(Const Title: String; HaveAssets: Boolean; Const AssetsParam: String; HaveSounds: Boolean;
  Const SoundsParam: String; ExpOk: Boolean; Const ExpGraphics, ExpSounds: String; Const IniFile: String = '*');
Var
  g, s, f: String;
  ok: Boolean;
Begin
  f := IniFile;
  If f = '*' Then f := Root + 'fpc_atomic.cfg';
  ok := ResolveStartSets(f, HaveAssets, AssetsParam, HaveSounds, SoundsParam, g, s);
  If (ok = ExpOk) And (g = ExpGraphics) And (s = ExpSounds) Then Begin
    inc(Passed);
    writeln('ok    ', Title);
  End
  Else Begin
    inc(Failed);
    writeln('FAIL  ', Title, ': got (', ok, ', "', g, '", "', s, '"), expected (', ExpOk, ', "', ExpGraphics, '", "', ExpSounds, '")');
  End;
End;

Begin
  Failed := 0;
  Passed := 0;
  Root := IncludeTrailingPathDelimiter(GetTempDir(false)) + 'fpc_atomic_test_startsets_' + IntToStr(GetProcessID) + PathDelim;
  DeleteTree(Root);
  ForceDirectories(Root);
  AppDirOverride := Root;
  Try
    // Fresh install: nothing at all
    Check('nothing, no config', false, '', false, '', false, '', '', '');
    Check('nothing, config file missing', false, '', false, '', false, '', '');
    WriteIni('', 'auto');
    Check('nothing, config original', false, '', false, '', false, '', '');

    // A generated set with sounds, no CD
    MakeSet('Flach', true, true);
    MakeSet('Weich', true, false);
    WriteIni('Flach', 'auto');
    Check('config set with sounds, auto', false, '', false, '', true, 'Flach', 'Flach');
    WriteIni('Weich', 'auto');
    Check('config set without sounds, auto', false, '', false, '', true, 'Weich', '');
    WriteIni('Weich', 'Flach');
    Check('config sounds of another set', false, '', false, '', true, 'Weich', 'Flach');
    WriteIni('Flach', '');
    Check('config original sounds', false, '', false, '', true, 'Flach', '');
    WriteIni('', 'auto');
    Check('config original without CD -> first usable set', false, '', false, '', true, 'Flach', 'Flach');
    WriteIni('Gone', 'auto');
    Check('config set that no longer exists, no CD -> first usable set', false, '', false, '', true, 'Flach', 'Flach');
    Check('no config file, no CD -> first usable set, sounds auto', false, '', false, '', true, 'Flach', 'Flach', '');
    WriteIni('..', 'auto');
    Check('config with a path out of assets', false, '', false, '', true, 'Flach', 'Flach');
    MakeSet('AAA_Partial', false, true);
    WriteIni('AAA_Partial', 'auto');
    Check('incomplete set without CD -> first usable set', false, '', false, '', true, 'Flach', 'Flach');

    // Parameters override the config
    WriteIni('Flach', 'auto');
    Check('-assets overrides config', true, 'Weich', false, '', true, 'Weich', '');
    Check('-sounds overrides config', false, '', true, 'Weich', true, 'Flach', ''); // Weich has no sounds
    Check('-assets and -sounds', true, 'Weich', true, 'Flach', true, 'Weich', 'Flach');
    Check('-sounds auto', true, 'Flach', true, 'auto', true, 'Flach', 'Flach');
    Check('-assets unknown, no CD -> first usable set', true, 'Nope', false, '', true, 'Flach', 'Flach');

    // With CD data
    WriteFile('data' + PathDelim + 'res' + PathDelim + 'mainmenu.png', 'png');
    WriteFile('data' + PathDelim + 'sounds' + PathDelim + 'bmdrop1.wav', 'wav');
    WriteIni('', 'auto');
    Check('CD, config original', false, '', false, '', true, '', '');
    WriteIni('Gone', 'auto');
    Check('CD, config set that no longer exists -> original', false, '', false, '', true, '', '');
    WriteIni('AAA_Partial', 'auto');
    Check('CD, incomplete set stays (missing files from data)', false, '', false, '', true, 'AAA_Partial', 'AAA_Partial');
    WriteIni('Weich', 'auto');
    Check('CD, set without sounds, auto -> original sounds', false, '', false, '', true, 'Weich', '');
    Check('CD, -assets empty = original', true, '', false, '', true, '', '');
    Check('CD, no config file', false, '', false, '', true, '', '', '');
  Finally
    DeleteTree(Root);
  End;
  writeln;
  writeln(Passed, ' passed, ', Failed, ' failed');
  If Failed <> 0 Then halt(1);
End.
