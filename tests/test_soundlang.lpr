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
 * Tests for the language of the spoken sounds (uassetpath): SoundFile,
 * SoundSetFile and FirstSoundFile look for <dir>/<lang>/<file> in a set, then
 * <dir>/en/<file>, then the plain file of the set, then data/.
 * No GUI, works in a temporary folder (AppDirOverride).
 *
 *   fpc -Fu../units test_soundlang.lpr && ./test_soundlang
 *
 * Exit code 0 = all tests passed.
 *)
Program test_soundlang;

{$MODE ObjFPC}{$H+}

Uses
  Classes, SysUtils, uassetpath;

Var
  Root: String;
  Failed, Passed: integer;
  FuncLang: String;

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

Function P(Const Rel: String): String; // '/' -> PathDelim
Begin
  result := StringReplace(Rel, '/', PathDelim, [rfReplaceAll]);
End;

Procedure ExpectText(Const Title, Got, e: String);
Begin
  If Got = e Then Begin
    inc(Passed);
    writeln('ok    ', Title);
  End
  Else Begin
    inc(Failed);
    writeln('FAIL  ', Title, ': got "', Got, '", expected "', e, '"');
  End;
End;

Procedure Expect(Const Title, Got, ExpRel: String);
Var
  e: String;
Begin
  If ExpRel = '' Then e := '' Else e := Root + P(ExpRel);
  ExpectText(Title, Got, e);
End;

Function TestLang: String;
Begin
  result := FuncLang;
End;

Procedure CheckLang(Const Lang, ExpVoiceML, ExpVoicePlain, ExpQuit: String);
Begin
  SoundLanguage := Lang;
  // a set with language folders (ML) and one without (Plain), both with sounds
  SetSoundSet('ML');
  Expect('ML, lang "' + Lang + '": voice', SoundFile(P('sounds/gotcha.wav')), ExpVoiceML);
  Expect('ML, lang "' + Lang + '": effect (no language folder)', SoundFile(P('sounds/bmdrop1.wav')), 'assets/ML/sounds/bmdrop1.wav');
  Expect('ML, lang "' + Lang + '": res/quitgame', SoundFile(P('res/quitgame.wav')), ExpQuit);
  Expect('ML, lang "' + Lang + '": missing in set -> data', SoundFile(P('sounds/later.wav')), 'data/sounds/later.wav');
  Expect('ML, lang "' + Lang + '": SoundSetFile', SoundSetFile('ML', P('sounds/gotcha.wav')), ExpVoiceML);
  Expect('ML, lang "' + Lang + '": FirstSoundFile', FirstSoundFile('ML', [P('sounds/nothere.wav'), P('sounds/gotcha.wav')]), ExpVoiceML);
  SetSoundSet('Plain');
  Expect('Plain, lang "' + Lang + '": voice', SoundFile(P('sounds/gotcha.wav')), ExpVoicePlain);
  Expect('Plain, lang "' + Lang + '": FirstSoundFile', FirstSoundFile('Plain', [P('sounds/gotcha.wav')]), ExpVoicePlain);
  // the original: data/ has no language folders
  SetSoundSet('');
  Expect('original, lang "' + Lang + '": voice', SoundFile(P('sounds/gotcha.wav')), 'data/sounds/gotcha.wav');
  Expect('original, lang "' + Lang + '": FirstSoundFile', FirstSoundFile('', [P('sounds/gotcha.wav')]), 'data/sounds/gotcha.wav');
  Expect('original, lang "' + Lang + '": SoundSetFile', SoundSetFile('', P('sounds/gotcha.wav')), 'data/sounds/gotcha.wav');
End;

Begin
  Failed := 0;
  Passed := 0;
  Root := IncludeTrailingPathDelimiter(GetTempDir(false)) + 'fpc_atomic_test_soundlang_' + IntToStr(GetProcessID) + PathDelim;
  DeleteTree(Root);
  ForceDirectories(Root);
  AppDirOverride := Root;
  Try
    WriteFile(P('data/sounds/gotcha.wav'), 'cd');
    WriteFile(P('data/sounds/later.wav'), 'cd');
    WriteFile(P('data/res/quitgame.wav'), 'cd');
    WriteFile(P('assets/ML/profile.ini'), '[General]' + LineEnding + 'Name=ML');
    WriteFile(P('assets/ML/sounds/gotcha.wav'), 'default');
    WriteFile(P('assets/ML/sounds/en/gotcha.wav'), 'en');
    WriteFile(P('assets/ML/sounds/de/gotcha.wav'), 'de');
    WriteFile(P('assets/ML/sounds/bmdrop1.wav'), 'fx');
    WriteFile(P('assets/ML/res/quitgame.wav'), 'default');
    WriteFile(P('assets/ML/res/en/quitgame.wav'), 'en');
    WriteFile(P('assets/ML/res/de/quitgame.wav'), 'de');
    WriteFile(P('assets/Plain/profile.ini'), '[General]' + LineEnding + 'Name=Plain');
    WriteFile(P('assets/Plain/sounds/gotcha.wav'), 'default');

    CheckLang('', 'assets/ML/sounds/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/quitgame.wav');
    CheckLang('en', 'assets/ML/sounds/en/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/en/quitgame.wav');
    CheckLang('de', 'assets/ML/sounds/de/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/de/quitgame.wav');
    CheckLang('DE', 'assets/ML/sounds/de/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/de/quitgame.wav');
    CheckLang('fr', 'assets/ML/sounds/en/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/en/quitgame.wav');
    CheckLang('es', 'assets/ML/sounds/en/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/en/quitgame.wav');
    CheckLang('..', 'assets/ML/sounds/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/quitgame.wav');

    // SoundLanguageFunc wins over SoundLanguage and is asked at every lookup (launcher: language switch)
    SoundLanguage := 'en';
    SoundLanguageFunc := @TestLang;
    FuncLang := 'de';
    Expect('func de', SoundSetFile('ML', P('sounds/gotcha.wav')), 'assets/ML/sounds/de/gotcha.wav');
    FuncLang := 'en';
    Expect('func switched to en', SoundSetFile('ML', P('sounds/gotcha.wav')), 'assets/ML/sounds/en/gotcha.wav');
    FuncLang := '';
    Expect('func empty -> plain', SoundSetFile('ML', P('sounds/gotcha.wav')), 'assets/ML/sounds/gotcha.wav');
    SoundLanguageFunc := Nil;

    // [Launcher] Language
    ExpectText('no ini -> en', SoundLanguageFromIni(Root + 'nope.cfg'), 'en');
    WriteFile('fpc_atomic.cfg', '[Launcher]' + LineEnding + 'Language=DE');
    ExpectText('ini Language=DE -> de', SoundLanguageFromIni(Root + 'fpc_atomic.cfg'), 'de');
    WriteFile('fpc_atomic.cfg', '[Launcher]' + LineEnding + 'AssetSet=x');
    ExpectText('ini without Language -> en', SoundLanguageFromIni(Root + 'fpc_atomic.cfg'), 'en');
  Finally
    DeleteTree(Root);
  End;
  writeln;
  writeln(Passed, ' passed, ', Failed, ' failed');
  If Failed <> 0 Then halt(1);
End.
