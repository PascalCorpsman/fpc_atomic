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
Unit uassetpath;

{$MODE ObjFPC}{$H+}

Interface

(*
 * Graphics sets
 *
 * A graphics set lives in assets/<Folder>/ next to the executable and has the same sub layout as data/
 * (atomic/, atomic/die/, atomic/idle/, atomic/locked_in/, maps/FieldXX/, res/), plus profile.ini ([General] Name=...)
 * and preview.png. A set may be incomplete, every graphic that is missing in the set is taken from data/.
 * Schemes and maps/FieldXX/info.txt are never part of a set, they always come from data/. Sounds of a set are
 * selected separately (see below).
 * A generated set brings its own schemes in schemes/*.SCH, they are copied to data/schemes/ when there are none
 * (no CD data), so server and clients always read the schemes from data/schemes/ (see CopySetSchemes).
 *
 * Without a selected set (AssetSet = '') everything is loaded from data/ as before.
 *
 * Sounds
 *
 * The sound source is selected independently of the graphics set (SoundSet, client parameter -sounds <Folder>):
 * '' = the original sounds from data/ (CD), '<Folder>' = the sounds of that set (sounds/*.wav, res/*.wav with the same
 * file names as in data/). A sound that is missing in the set is taken from data/ (see SoundFile).
 * The launcher setting [Launcher] SoundSet has the additional value SoundSetAuto (default, client -sounds auto): the
 * sounds of the selected graphics set, with the original graphics (or a set without sounds) the original sounds, see
 * ResolveSoundSet.
 *
 * Spoken sounds of a set exist per language in a sub folder next to the plain file (sounds/de/gotcha.wav,
 * res/en/quitgame.wav, written by the asset generator). The sound language is the language of the launcher
 * ([Launcher] Language, see SoundLanguageFromIni); a set looks for <dir>/<lang>/<file>, then <dir>/en/<file> (a language
 * without own voices, e.g. fr, es), then <dir>/<file>. data/ (CD) has no language folders.
 *)

Uses
  Classes, SysUtils;

Const
  AssetProfileFile = 'profile.ini';
  AssetPreviewFile = 'preview.png';

  SoundSetAuto = 'auto'; // [Launcher] SoundSet: the sounds of the graphics set (original graphics -> original sounds)

Var
  AssetSet: String = ''; // Folder name below assets/, '' = original graphics from data/
  SoundSet: String = ''; // Folder name below assets/, '' = original sounds from data/
  SoundLanguage: String = ''; // Language of the spoken sounds of a set (e.g. 'de'), '' = only the plain files
  (*
   * If assigned, it is asked at every lookup instead of SoundLanguage (the launcher: the language it shows right now,
   * so a language switch takes effect immediately).
   *)
  SoundLanguageFunc: Function(): String = Nil;

(*
 * Selects the set by its folder name, returns false (and selects the original) if there is no such set.
 *)
Function SetAssetSet(Const Folder: String): Boolean;

Function AppDir(): String; // Folder of the executable, with trailing path delimiter
Function AssetDataDir(): String; // <AppDir>data/
Function AssetSetsDir(): String; // <AppDir>assets/
Function AssetSetDir(): String; // <AppDir>assets/<AssetSet>/ or '' if no set is selected

(*
 * RelPath is relative to data/ (e.g. 'res' + PathDelim + 'powbomb.png').
 * Result is assets/<AssetSet>/<RelPath> if that file exists, else data/<RelPath> (both with AppDir in front).
 *)
Function AssetFile(Const RelPath: String): String;

(*
 * The same for a folder, RelDir is relative to data/, the result has a trailing path delimiter.
 * The set folder is used if it contains the file CheckFile (e.g. 'walk.png'), CheckFile = '' -> if the folder exists.
 *)
Function AssetDir(Const RelDir: String; Const CheckFile: String = ''): String;

(*
 * The folders in which the sheets of an atomic are searched, the first one that has a file wins:
 *   assets/<AssetSet>/atomic/<SubDir>/ (only if it has walk.png, e.g. SubDir = 'p0' .. 'p9' or 'hunted')
 *   assets/<AssetSet>/atomic/
 *   data/atomic/
 * Without a set it is only data/atomic/.
 *)
Function AtomicDirs(Const SubDir: String): TStringArray;

(*
 * All sets in assets/ (folders that contain profile.ini), sorted by folder name.
 * Folders gets the folder names, Names the names from profile.ini (the folder name if there is none).
 *)
Procedure ListAssetSets(Const Folders, Names: TStrings);

(*
 * Selects the sound set by its folder name, returns false (and selects the original) if there is no such set
 * or it has no sounds.
 *)
Function SetSoundSet(Const Folder: String): Boolean;

(*
 * RelPath is relative to data/ (e.g. 'sounds' + PathDelim + 'bmdrop1.wav').
 * Result is the first file that exists of: assets/<SoundSet>/<dir>/<lang>/<file> (see SoundLanguages),
 * assets/<SoundSet>/<RelPath>, else data/<RelPath> (all with AppDir in front).
 *)
Function SoundFile(Const RelPath: String): String;

(*
 * The language sub folders a set is searched in, in this order: the current sound language (SoundLanguageFunc or
 * SoundLanguage), then 'en'. Empty if no language is set. Only plain language codes (letters) are used.
 *)
Function SoundLanguages(): TStringArray;

Function SoundLanguageFromIni(Const IniFile: String): String; // [Launcher] Language of IniFile, default 'en'

Function HasOriginalSounds(): Boolean; // True if data/sounds/ contains at least one *.wav (CD data)
Function SetHasSounds(Const Folder: String): Boolean; // True if assets/<Folder>/sounds/ contains at least one *.wav

(*
 * The sound set to use for the setting Setting ([Launcher] SoundSet) and the selected graphics set GraphicsSet:
 *   ''           -> '' (original)
 *   SoundSetAuto -> GraphicsSet, if it has sounds, else '' (the original graphics or a set without sounds -> original)
 *   '<Folder>'   -> Folder, if that set has sounds, else ''
 *)
Function ResolveSoundSet(Const Setting, GraphicsSet: String): String;

(*
 * Like ListAssetSets, but only the sets that have sounds (SetHasSounds).
 *)
Procedure ListSoundSets(Const Folders, Names: TStrings);

(*
 * The values of [Launcher] SoundSet in the order the launcher offers them: SoundSetAuto, '' (original), then the sets
 * with sounds (ListSoundSets). Names gets AutoName and OriginalName for the first two, then the names of the sets.
 *)
Procedure ListSoundChoices(Const Folders, Names: TStrings; Const AutoName, OriginalName: String);

(*
 * The index of Setting in Folders (from ListSoundChoices): 0 = SoundSetAuto, a set by its folder name, everything else
 * (also a set that no longer exists or has no sounds) is 1 = original, like the game treats it.
 *)
Function SoundChoiceIndex(Const Folders: TStrings; Const Setting: String): integer;

(*
 * Like SoundFile, but for the given sound set Folder instead of the global SoundSet:
 * assets/<Folder>/<dir>/<lang>/<file> or assets/<Folder>/<RelPath> if that file exists, else data/<RelPath>.
 *)
Function SoundSetFile(Const Folder, RelPath: String): String;

(*
 * The first of RelPaths (relative to data/) that exists for the sound set Folder: first the files of the set, then the
 * ones in data/ (per file fallback like in the game). '' if none of them exists.
 *)
Function FirstSoundFile(Const Folder: String; Const RelPaths: Array Of String): String;

Function AssetSetName(Const Folder: String): String; // Name from profile.ini, '' = Folder

Function HasOriginalGraphics(): Boolean; // True if data/res/mainmenu.png exists (CD data)
Function SetHasGraphics(Const Folder: String): Boolean; // True if assets/<Folder>/ has profile.ini and res/mainmenu.png
Function FirstGraphicsSet(): String; // The first set (sorted by folder) with SetHasGraphics, '' = none

(*
 * The graphics and sound set the game starts with (client fpc_atomic, also when it is started without the launcher):
 *   graphics: -assets <Folder> if HaveAssets, else [Launcher] AssetSet from IniFile ('' / unknown = original)
 *   sounds:   -sounds <Folder|auto> if HaveSounds, else [Launcher] SoundSet from IniFile (default SoundSetAuto),
 *             resolved with ResolveSoundSet
 * Without CD graphics a selected set that can not be played (or the original) is replaced by FirstGraphicsSet, like the
 * launcher does. Result = the game has graphics (CD graphics or a usable set); False -> nothing is set up yet.
 * IniFile = '' or a file that does not exist: the defaults of the launcher (original graphics, sounds automatic).
 *)
Function ResolveStartSets(Const IniFile: String; HaveAssets: Boolean; Const AssetsParam: String;
  HaveSounds: Boolean; Const SoundsParam: String; Out Graphics, Sounds: String): Boolean;

Var
  AppDirOverride: String = ''; // Tests only: AppDir() returns this (with trailing path delimiter) instead of the exe folder

Function SchemesDir(): String; // <AppDir>data/schemes/
Function HasSchemeFiles(Const Dir: String): Boolean; // True if Dir contains at least one *.SCH

(*
 * Copies assets/<Folder>/schemes/*.SCH to data/schemes/, but only if data/schemes/ has no *.SCH yet
 * (the CD schemes are never mixed with or replaced by generated ones). Result = number of copied files.
 *)
Function CopySetSchemes(Const Folder: String): integer;

(*
 * Wanted if it exists in data/schemes/, else BASIC.SCH, else the first *.SCH (sorted), else Wanted unchanged.
 * Generated schemes have no BASIC.SCH, so the default of the game would not be found without this.
 *)
Function ResolveSchemeFile(Const Wanted: String): String;

Implementation

Uses
  IniFiles;

Function AppDir(): String;
Begin
  If AppDirOverride <> '' Then Begin
    result := IncludeTrailingPathDelimiter(AppDirOverride);
    exit;
  End;
  result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
End;

// Only a plain folder name below assets/, no way out of it
Function IsPlainFolder(Const f: String): Boolean;
Begin
  result := (f <> '') And (f <> '.') And (f <> '..') And (pos('/', f) = 0) And (pos('\', f) = 0);
End;

Function HasOriginalGraphics(): Boolean;
Begin
  result := FileExists(AssetDataDir() + 'res' + PathDelim + 'mainmenu.png');
End;

Function SetHasGraphics(Const Folder: String): Boolean;
Var
  f: String;
Begin
  f := trim(Folder);
  result := IsPlainFolder(f) And FileExists(AssetSetsDir() + f + PathDelim + AssetProfileFile) And
    FileExists(AssetSetsDir() + f + PathDelim + 'res' + PathDelim + 'mainmenu.png');
End;

Function FirstGraphicsSet(): String;
Var
  Folders, Names: TStringList;
  i: integer;
Begin
  result := '';
  Folders := TStringList.Create;
  Names := TStringList.Create;
  Try
    ListAssetSets(Folders, Names);
    For i := 0 To Folders.Count - 1 Do Begin
      If SetHasGraphics(Folders[i]) Then Begin
        result := Folders[i];
        exit;
      End;
    End;
  Finally
    Folders.free;
    Names.free;
  End;
End;

Function ResolveStartSets(Const IniFile: String; HaveAssets: Boolean; Const AssetsParam: String;
  HaveSounds: Boolean; Const SoundsParam: String; Out Graphics, Sounds: String): Boolean;
Var
  ini: TIniFile;
  IniAssets, IniSounds, SoundSetting: String;
Begin
  IniAssets := '';
  IniSounds := SoundSetAuto; // The default of the launcher
  If (IniFile <> '') And FileExists(IniFile) And ((Not HaveAssets) Or (Not HaveSounds)) Then Begin
    ini := TIniFile.Create(IniFile);
    Try
      IniAssets := ini.ReadString('Launcher', 'AssetSet', '');
      IniSounds := ini.ReadString('Launcher', 'SoundSet', SoundSetAuto);
    Finally
      ini.free;
    End;
  End;
  // Parameters override the config
  If HaveAssets Then Begin
    Graphics := trim(AssetsParam);
  End
  Else Begin
    Graphics := trim(IniAssets);
  End;
  If Not (IsPlainFolder(Graphics) And DirectoryExists(AssetSetsDir() + Graphics)) Then Graphics := '';
  // Without CD graphics: a set that can be played (like SelectUsableAssetSet in the launcher)
  If (Not HasOriginalGraphics()) And (Not SetHasGraphics(Graphics)) Then Graphics := FirstGraphicsSet();
  If HaveSounds Then Begin
    SoundSetting := SoundsParam;
  End
  Else Begin
    SoundSetting := IniSounds;
  End;
  Sounds := ResolveSoundSet(SoundSetting, Graphics);
  result := HasOriginalGraphics() Or SetHasGraphics(Graphics);
End;

Function AssetDataDir(): String;
Begin
  result := AppDir() + 'data' + PathDelim;
End;

Function AssetSetsDir(): String;
Begin
  result := AppDir() + 'assets' + PathDelim;
End;

Function AssetSetDir(): String;
Begin
  If AssetSet = '' Then Begin
    result := '';
  End
  Else Begin
    result := AssetSetsDir() + AssetSet + PathDelim;
  End;
End;

Function SetAssetSet(Const Folder: String): Boolean;
Var
  f: String;
Begin
  f := trim(Folder);
  // Only a plain folder name below assets/, no way out of it
  result := (f <> '') And (f <> '.') And (f <> '..') And (pos('/', f) = 0) And (pos('\', f) = 0) And
    DirectoryExists(AssetSetsDir() + f);
  If result Then Begin
    AssetSet := f;
  End
  Else Begin
    AssetSet := '';
  End;
End;

Function AssetFile(Const RelPath: String): String;
Begin
  If AssetSet <> '' Then Begin
    result := AssetSetDir() + RelPath;
    If FileExists(result) Then exit;
  End;
  result := AssetDataDir() + RelPath;
End;

Function AssetDir(Const RelDir: String; Const CheckFile: String): String;
Begin
  If AssetSet <> '' Then Begin
    result := IncludeTrailingPathDelimiter(AssetSetDir() + RelDir);
    If CheckFile = '' Then Begin
      If DirectoryExists(result) Then exit;
    End
    Else Begin
      If FileExists(result + CheckFile) Then exit;
    End;
  End;
  result := IncludeTrailingPathDelimiter(AssetDataDir() + RelDir);
End;

Function AtomicDirs(Const SubDir: String): TStringArray;
Var
  d: String;
Begin
  result := Nil;
  If AssetSet <> '' Then Begin
    d := AssetSetDir() + 'atomic' + PathDelim;
    If (SubDir <> '') And FileExists(d + SubDir + PathDelim + 'walk.png') Then Begin
      setlength(result, length(result) + 1);
      result[high(result)] := d + SubDir + PathDelim;
    End;
    If DirectoryExists(d) Then Begin
      setlength(result, length(result) + 1);
      result[high(result)] := d;
    End;
  End;
  setlength(result, length(result) + 1);
  result[high(result)] := AssetDataDir() + 'atomic' + PathDelim;
End;

// True if Dir contains at least one *.wav (case insensitive, Linux)
Function HasWavFiles(Const Dir: String): Boolean;
Var
  sr: TSearchRec;
Begin
  result := false;
  If FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, sr) = 0 Then Begin
    Try
      Repeat
        If (sr.Attr And faDirectory) <> 0 Then Continue;
        If SameText(ExtractFileExt(sr.Name), '.wav') Then Begin
          result := true;
          break;
        End;
      Until FindNext(sr) <> 0;
    Finally
      FindClose(sr);
    End;
  End;
End;

Function HasOriginalSounds(): Boolean;
Begin
  result := HasWavFiles(AssetDataDir() + 'sounds');
End;

Function SetHasSounds(Const Folder: String): Boolean;
Var
  f: String;
Begin
  // Only a plain folder name below assets/, no way out of it
  f := trim(Folder);
  result := (f <> '') And (f <> '.') And (f <> '..') And (pos('/', f) = 0) And (pos('\', f) = 0) And
    HasWavFiles(AssetSetsDir() + f + PathDelim + 'sounds');
End;

Function SetSoundSet(Const Folder: String): Boolean;
Begin
  result := SetHasSounds(Folder);
  If result Then Begin
    SoundSet := trim(Folder);
  End
  Else Begin
    SoundSet := '';
  End;
End;

Function IsLanguageCode(Const Code: String): Boolean;
Var
  i: integer;
Begin
  result := (Code <> '') And (length(Code) <= 8);
  For i := 1 To length(Code) Do Begin
    If Not (Code[i] In ['a'..'z']) Then result := false;
  End;
End;

Function SoundLanguages(): TStringArray;
Var
  Lang: String;
Begin
  result := Nil;
  If assigned(SoundLanguageFunc) Then Begin
    Lang := lowercase(trim(SoundLanguageFunc()));
  End
  Else Begin
    Lang := lowercase(trim(SoundLanguage));
  End;
  If Not IsLanguageCode(Lang) Then exit;
  If Lang = 'en' Then Begin
    setlength(result, 1);
    result[0] := 'en';
  End
  Else Begin
    setlength(result, 2);
    result[0] := Lang;
    result[1] := 'en';
  End;
End;

Function SoundLanguageFromIni(Const IniFile: String): String;
Var
  ini: TIniFile;
Begin
  result := 'en';
  If (IniFile = '') Or Not FileExists(IniFile) Then exit;
  ini := TIniFile.Create(IniFile);
  Try
    result := lowercase(trim(ini.ReadString('Launcher', 'Language', 'en')));
  Finally
    ini.free;
  End;
End;

(*
 * The file RelPath of the set Folder: first in the language folders (SoundLanguages) next to it, then RelPath itself.
 * '' if the set has none of them.
 *)
Function SoundInSet(Const Folder, RelPath: String): String;
Var
  Langs: TStringArray;
  Base: String;
  i: integer;
Begin
  result := '';
  If trim(Folder) = '' Then exit;
  Base := AssetSetsDir() + trim(Folder) + PathDelim;
  Langs := SoundLanguages();
  For i := 0 To high(Langs) Do Begin
    result := Base + ExtractFilePath(RelPath) + Langs[i] + PathDelim + ExtractFileName(RelPath);
    If FileExists(result) Then exit;
  End;
  result := Base + RelPath;
  If Not FileExists(result) Then result := '';
End;

Function SoundFile(Const RelPath: String): String;
Begin
  result := SoundInSet(SoundSet, RelPath);
  If result = '' Then result := AssetDataDir() + RelPath;
End;

Function ResolveSoundSet(Const Setting, GraphicsSet: String): String;
Begin
  result := '';
  If SameText(trim(Setting), SoundSetAuto) Then Begin
    // The sounds belong to the graphics: a set with sounds plays its own, also when the CD sounds are there
    If SetHasSounds(GraphicsSet) Then Begin
      result := trim(GraphicsSet);
    End;
  End
  Else If SetHasSounds(Setting) Then Begin
    result := trim(Setting);
  End;
End;

Procedure ListSoundSets(Const Folders, Names: TStrings);
Var
  i: integer;
Begin
  ListAssetSets(Folders, Names);
  For i := Folders.Count - 1 Downto 0 Do Begin
    If Not SetHasSounds(Folders[i]) Then Begin
      Folders.Delete(i);
      Names.Delete(i);
    End;
  End;
End;

Procedure ListSoundChoices(Const Folders, Names: TStrings; Const AutoName, OriginalName: String);
Begin
  ListSoundSets(Folders, Names);
  Folders.Insert(0, '');
  Names.Insert(0, OriginalName);
  Folders.Insert(0, SoundSetAuto);
  Names.Insert(0, AutoName);
End;

Function SoundChoiceIndex(Const Folders: TStrings; Const Setting: String): integer;
Var
  i: integer;
Begin
  result := 1;
  If SameText(trim(Setting), SoundSetAuto) Then Begin
    result := 0;
    exit;
  End;
  For i := 2 To Folders.Count - 1 Do Begin
    If SameText(Folders[i], trim(Setting)) Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function SoundSetFile(Const Folder, RelPath: String): String;
Begin
  result := SoundInSet(Folder, RelPath);
  If result = '' Then result := AssetDataDir() + RelPath;
End;

Function FirstSoundFile(Const Folder: String; Const RelPaths: Array Of String): String;
Var
  i: integer;
Begin
  For i := 0 To high(RelPaths) Do Begin
    result := SoundInSet(Folder, RelPaths[i]);
    If result <> '' Then exit;
  End;
  For i := 0 To high(RelPaths) Do Begin
    result := AssetDataDir() + RelPaths[i];
    If FileExists(result) Then exit;
  End;
  result := '';
End;

Function AssetSetName(Const Folder: String): String;
Var
  ini: TIniFile;
Begin
  result := '';
  If FileExists(AssetSetsDir() + Folder + PathDelim + AssetProfileFile) Then Begin
    ini := TIniFile.Create(AssetSetsDir() + Folder + PathDelim + AssetProfileFile);
    Try
      result := trim(ini.ReadString('General', 'Name', ''));
    Finally
      ini.free;
    End;
  End;
  If result = '' Then result := Folder;
End;

Function SchemesDir(): String;
Begin
  result := AssetDataDir() + 'schemes' + PathDelim;
End;

// All *.SCH in Dir (case insensitive, Linux), sorted
Function FindSchemeFiles(Const Dir: String): TStringList;
Var
  sr: TSearchRec;
Begin
  result := TStringList.Create;
  result.Sorted := true;
  result.CaseSensitive := false;
  result.Duplicates := dupIgnore;
  If FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, sr) = 0 Then Begin
    Try
      Repeat
        If (sr.Attr And faDirectory) <> 0 Then Continue;
        If SameText(ExtractFileExt(sr.Name), '.SCH') Then result.Add(sr.Name);
      Until FindNext(sr) <> 0;
    Finally
      FindClose(sr);
    End;
  End;
End;

Function HasSchemeFiles(Const Dir: String): Boolean;
Var
  sl: TStringList;
Begin
  sl := FindSchemeFiles(Dir);
  result := sl.Count <> 0;
  sl.free;
End;

Function CopySetSchemes(Const Folder: String): integer;
Var
  sl: TStringList;
  src: String;
  i: integer;
  fi, fo: TFileStream;
Begin
  result := 0;
  If (trim(Folder) = '') Or HasSchemeFiles(SchemesDir()) Then exit;
  src := AssetSetsDir() + Folder + PathDelim + 'schemes' + PathDelim;
  sl := FindSchemeFiles(src);
  Try
    If sl.Count = 0 Then exit;
    If Not ForceDirectories(SchemesDir()) Then exit;
    For i := 0 To sl.Count - 1 Do Begin
      Try
        fi := TFileStream.Create(src + sl[i], fmOpenRead Or fmShareDenyWrite);
        Try
          fo := TFileStream.Create(SchemesDir() + sl[i], fmCreate);
          Try
            fo.CopyFrom(fi, 0);
          Finally
            fo.free;
          End;
        Finally
          fi.free;
        End;
        inc(result);
      Except
        // A single file that can not be copied (e.g. no write access) is skipped, the caller sees it in the count
      End;
    End;
  Finally
    sl.free;
  End;
End;

Function ResolveSchemeFile(Const Wanted: String): String;
Var
  sl: TStringList;
  i: integer;
Begin
  result := Wanted;
  sl := FindSchemeFiles(SchemesDir());
  Try
    If sl.Count = 0 Then exit;
    i := sl.IndexOf(Wanted);
    If (Wanted <> '') And (i >= 0) Then Begin
      result := sl[i]; // The real spelling of the file (Linux)
      exit;
    End;
    i := sl.IndexOf('BASIC.SCH');
    If i < 0 Then i := 0;
    result := sl[i];
  Finally
    sl.free;
  End;
End;

Procedure ListAssetSets(Const Folders, Names: TStrings);
Var
  sr: TSearchRec;
  sl: TStringList;
  i: integer;
Begin
  Folders.Clear;
  Names.Clear;
  sl := TStringList.Create;
  Try
    sl.Sorted := true;
    sl.CaseSensitive := false;
    If FindFirst(AssetSetsDir() + '*', faDirectory, sr) = 0 Then Begin
      Try
        Repeat
          If (sr.Attr And faDirectory) = 0 Then Continue;
          If (sr.Name = '') Or (sr.Name[1] = '.') Then Continue; // also temporary folders of the asset generator (.<set>.tmp-<n>)
          If Not FileExists(AssetSetsDir() + sr.Name + PathDelim + AssetProfileFile) Then Continue;
          sl.Add(sr.Name);
        Until FindNext(sr) <> 0;
      Finally
        FindClose(sr);
      End;
    End;
    For i := 0 To sl.Count - 1 Do Begin
      Folders.Add(sl[i]);
      Names.Add(AssetSetName(sl[i]));
    End;
  Finally
    sl.free;
  End;
End;

End.

