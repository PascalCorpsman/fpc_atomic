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
 * Sounds, schemes and maps/FieldXX/info.txt are never part of a set, they always come from data/.
 * A generated set brings its own schemes in schemes/*.SCH, they are copied to data/schemes/ when there are none
 * (no CD data), so server and clients always read the schemes from data/schemes/ (see CopySetSchemes).
 *
 * Without a selected set (AssetSet = '') everything is loaded from data/ as before.
 *)

Uses
  Classes, SysUtils;

Const
  AssetProfileFile = 'profile.ini';
  AssetPreviewFile = 'preview.png';

Var
  AssetSet: String = ''; // Folder name below assets/, '' = original graphics from data/

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
Function AssetSetName(Const Folder: String): String; // Name from profile.ini, '' = Folder

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
  result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
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
          If (sr.Name = '.') Or (sr.Name = '..') Then Continue;
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

