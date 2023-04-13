Unit ucdextractor;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Function CheckCDRootFolder(aFolder: String): boolean;
Function CheckFPCAtomicFolder(aFolder: String): boolean;

Procedure ExtractAtomicImages(CDFolder, AtomicFolder: String);
Procedure ExtractAtomicSounds(CDFolder, AtomicFolder: String);
Procedure ExtractAtomicShemes(CDFolder, AtomicFolder: String);

Implementation

Uses
  FileUtil, Unit1;

Function CheckCDRootFolder(aFolder: String): boolean;
Var
  sl: TStringList;
  checks: Array[0..3] Of Boolean;
  i: integer;
Begin
  result := true;
  sl := FindAllDirectories(aFolder, True);
  For i := 0 To high(checks) Do Begin
    checks[i] := false;
  End;
  For i := 0 To sl.Count - 1 Do Begin
    If pos('data' + PathDelim + 'ani', lowercase(sl[i])) <> 0 Then checks[0] := true;
    If pos('data' + PathDelim + 'res', lowercase(sl[i])) <> 0 Then checks[1] := true;
    If pos('data' + PathDelim + 'schemes', lowercase(sl[i])) <> 0 Then checks[2] := true;
    If pos('data' + PathDelim + 'sound', lowercase(sl[i])) <> 0 Then checks[3] := true;
  End;
  For i := 0 To high(checks) Do Begin
    result := result And checks[i];
  End;
  sl.free;
End;

Function CheckFPCAtomicFolder(aFolder: String): boolean;
Var
  s: String;
Begin
  s := 'fpc_atomic';
{$IFDEF Windows}
  s := s + '.exe';
{$ENDIF}
  result := FileExists(IncludeTrailingPathDelimiter(aFolder) + s);
End;

Function GetFolderByMatch(RootFolder, Match: String): String;
Var
  sl: TStringList;
  i: Integer;
Begin
  result := '';
  sl := FindAllDirectories(RootFolder, True);
  For i := 0 To sl.Count - 1 Do Begin
    If pos(match, lowercase(sl[i])) <> 0 Then Begin
      result := sl[i];
      break;
    End;
  End;
  sl.free;
End;

Procedure ExtractAtomicImages(CDFolder, AtomicFolder: String);
Begin
  AddLog('  not implemented yet, sry.');
End;

Procedure ExtractAtomicSounds(CDFolder, AtomicFolder: String);
Begin
  AddLog('  not implemented yet, sry.');
End;

Procedure ExtractAtomicShemes(CDFolder, AtomicFolder: String);
Var
  SchemeFolder: String;
Begin
  SchemeFolder := GetFolderByMatch(CDFolder, 'data' + PathDelim + 'schemes');
  If SchemeFolder = '' Then Begin
    addlog('  Error, unable to find schemes folder.');
  End;

  hier weiter

End;

End.



