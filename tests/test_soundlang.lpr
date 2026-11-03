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
 * <dir>/en/<file> (a set without that language), then the plain file of the
 * set, then data/. Also the shown names of the sets (AssetSetDisplayName).
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

Function TestStyleName(Const Style: String): String;
Begin
  If FuncLang = 'de' Then Begin
    Case Style Of
      'flat': result := 'Flach';
      'soft': result := 'Weich';
    Else
      result := '';
    End;
  End
  Else If FuncLang = 'fr' Then Begin
    If Style = 'retro' Then result := 'Rétro' Else result := '';
  End
  Else
    result := '';
End;

Procedure CheckDisplayNames;
Var
  Folders, Names: TStringList;
Begin
  WriteFile(P('assets/Flat/profile.ini'), '[General]' + LineEnding + 'Name=Flat' + LineEnding + 'Style=flat' + LineEnding + 'Origin=setup');
  WriteFile(P('assets/Flat2/profile.ini'), '[General]' + LineEnding + 'Name=Flat 2' + LineEnding + 'Style=flat' + LineEnding + 'Origin=setup');
  WriteFile(P('assets/Soft/profile.ini'), '[General]' + LineEnding + 'Name=Weich' + LineEnding + 'Style=SOFT' + LineEnding + 'Origin=Setup');
  WriteFile(P('assets/Retro/profile.ini'), '[General]' + LineEnding + 'Name=Retro' + LineEnding + 'Style=retro' + LineEnding + 'Origin=setup');
  WriteFile(P('assets/Mine/profile.ini'), '[General]' + LineEnding + 'Name=Mein Satz' + LineEnding + 'Style=flat' + LineEnding + 'Origin=editor');
  WriteFile(P('assets/Odd/profile.ini'), '[General]' + LineEnding + 'Name=Odd one' + LineEnding + 'Style=chalk' + LineEnding + 'Origin=setup');
  WriteFile(P('assets/NoName/profile.ini'), '[General]' + LineEnding + 'Style=pixel');
  AssetStyleNameFunc := Nil;
  ExpectText('name: setup set, no translation -> English', AssetSetDisplayName('Flat'), 'Flat');
  ExpectText('name: setup set 2, no translation', AssetSetDisplayName('Flat2'), 'Flat 2');
  ExpectText('name: setup set, old profile name ignored', AssetSetDisplayName('Soft'), 'Soft');
  ExpectText('name: editor set keeps its name', AssetSetDisplayName('Mine'), 'Mein Satz');
  ExpectText('name: unknown style keeps its name', AssetSetDisplayName('Odd'), 'Odd one');
  ExpectText('name: no name -> folder', AssetSetDisplayName('NoName'), 'NoName');
  ExpectText('name: no set -> folder', AssetSetDisplayName('Nope'), 'Nope');
  AssetStyleNameFunc := @TestStyleName;
  FuncLang := 'de';
  ExpectText('name de: Flat', AssetSetDisplayName('Flat'), 'Flach');
  ExpectText('name de: Flat2', AssetSetDisplayName('Flat2'), 'Flach 2');
  ExpectText('name de: Soft', AssetSetDisplayName('Soft'), 'Weich');
  ExpectText('name de: Retro (no translation -> English)', AssetSetDisplayName('Retro'), 'Retro');
  ExpectText('name de: editor set', AssetSetDisplayName('Mine'), 'Mein Satz');
  FuncLang := 'fr';
  ExpectText('name fr: Retro (language switch)', AssetSetDisplayName('Retro'), 'Rétro');
  ExpectText('name fr: Flat', AssetSetDisplayName('Flat'), 'Flat');
  // the lists show the same names
  FuncLang := 'de';
  Folders := TStringList.Create;
  Names := TStringList.Create;
  Try
    ListAssetSets(Folders, Names);
    ExpectText('ListAssetSets: name of Flat2', Names[Folders.IndexOf('Flat2')], 'Flach 2');
    ExpectText('ListAssetSets: name of Mine', Names[Folders.IndexOf('Mine')], 'Mein Satz');
  Finally
    Folders.free;
    Names.free;
  End;
  AssetStyleNameFunc := Nil;
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
    WriteFile(P('assets/ML/sounds/fr/gotcha.wav'), 'fr');
    WriteFile(P('assets/ML/sounds/es/gotcha.wav'), 'es');
    WriteFile(P('assets/ML/sounds/bmdrop1.wav'), 'fx');
    WriteFile(P('assets/ML/res/quitgame.wav'), 'default');
    WriteFile(P('assets/ML/res/en/quitgame.wav'), 'en');
    WriteFile(P('assets/ML/res/de/quitgame.wav'), 'de');
    WriteFile(P('assets/ML/res/fr/quitgame.wav'), 'fr');
    WriteFile(P('assets/ML/res/es/quitgame.wav'), 'es');
    // a set of an older version: only en and de
    WriteFile(P('assets/Old/profile.ini'), '[General]' + LineEnding + 'Name=Old');
    WriteFile(P('assets/Old/sounds/gotcha.wav'), 'default');
    WriteFile(P('assets/Old/sounds/en/gotcha.wav'), 'en');
    WriteFile(P('assets/Old/sounds/de/gotcha.wav'), 'de');
    WriteFile(P('assets/Plain/profile.ini'), '[General]' + LineEnding + 'Name=Plain');
    WriteFile(P('assets/Plain/sounds/gotcha.wav'), 'default');

    CheckLang('', 'assets/ML/sounds/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/quitgame.wav');
    CheckLang('en', 'assets/ML/sounds/en/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/en/quitgame.wav');
    CheckLang('de', 'assets/ML/sounds/de/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/de/quitgame.wav');
    CheckLang('DE', 'assets/ML/sounds/de/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/de/quitgame.wav');
    CheckLang('fr', 'assets/ML/sounds/fr/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/fr/quitgame.wav');
    CheckLang('es', 'assets/ML/sounds/es/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/es/quitgame.wav');
    CheckLang('it', 'assets/ML/sounds/en/gotcha.wav', 'assets/Plain/sounds/gotcha.wav', 'assets/ML/res/en/quitgame.wav');
    // a set without the language folder: English
    SoundLanguage := 'fr';
    Expect('Old, lang "fr": no fr folder -> en', SoundSetFile('Old', P('sounds/gotcha.wav')), 'assets/Old/sounds/en/gotcha.wav');
    SoundLanguage := 'es';
    Expect('Old, lang "es": no es folder -> en', SoundSetFile('Old', P('sounds/gotcha.wav')), 'assets/Old/sounds/en/gotcha.wav');
    SoundLanguage := 'de';
    Expect('Old, lang "de"', SoundSetFile('Old', P('sounds/gotcha.wav')), 'assets/Old/sounds/de/gotcha.wav');
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

    CheckDisplayNames;
  Finally
    DeleteTree(Root);
  End;
  writeln;
  writeln(Passed, ' passed, ', Failed, ' failed');
  If Failed <> 0 Then halt(1);
End.
