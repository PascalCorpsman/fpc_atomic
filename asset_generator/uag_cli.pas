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
 * Command line handling of the asset generator. Runs all generators that are
 * registered (see uag_registry), so a test program only needs:
 *   Program test; Uses uag_cli, uag_gen_items; Begin RunCli; End.
 *
 * The set is saved atomically (see uag_output): everything is written into
 * a temporary sibling folder that replaces --out only when it is complete,
 * the old set stays untouched on an error. A set folder is always generated
 * completely; with --only the existing set is copied into the temporary
 * folder first and only the chosen generators are run again. Leftovers of
 * a crash next to --out are cleaned up at start.
 * Exit code: 0 ok, 1 error or cancelled, 2 wrong parameters, 3 the old set
 * is in use (e.g. by the game) and was kept.
 *
 * Hidden debug option: --cancel-after <n> cancels after n files (as if the
 * user cancelled), the old set must stay unchanged.
 *)
Unit uag_cli;

{$MODE ObjFPC}{$H+}

Interface

Procedure RunCli;

Implementation

Uses
  Classes, SysUtils, uag_random, uag_context, uag_output, uag_registry;

Type

  { TCli }

  TCli = Class
    Procedure OnProgress(Sender: TObject; Const RelPath: String; Done, Total: Integer);
  End;

Procedure TCli.OnProgress(Sender: TObject; Const RelPath: String; Done, Total: Integer);
Begin
  If Total > 0 Then
    writeln(format('[%d/%d] %s', [Done, Total, RelPath]))
  Else
    writeln(format('[%d] %s', [Done, RelPath]));
End;

Procedure PrintHelp;
Var
  i: Integer;
Begin
  writeln('asset_generator - computes graphic sets for FPC Atomic');
  writeln;
  writeln('Usage: asset_generator --out <dir> [options]');
  writeln('  --out <dir>        target folder of the set, e.g. assets/myset');
  writeln('  --profile <file>   load seed, style, locked seeds and parameters');
  writeln('                     (default: <dir>/profile.ini if it exists)');
  writeln('  --seed <n|random>  main seed');
  writeln('  --style <s>        flat | soft | pixel | retro');
  writeln('  --name <text>      name of the set');
  writeln('  --per-player       own atomic per player (atomic/p0 .. p9)');
  writeln('  --only <a,b,..>    only these generators');
  writeln('  --params           list all parameters after generating');
  writeln('  --list             list the generators and exit');
  writeln;
  writeln('Generators:');
  For i := 0 To GeneratorCount - 1 Do
    writeln(format('  %-12s %s', [GetGenerator(i).Name, GetGenerator(i).Description]));
End;

Procedure RunCli;
Var
  Ctx: TAGContext;
  Output: TAGOutput;
  Cli: TCli;
  OutDir, ProfileFile, OnlyList, SeedStr, StyleStr, NameStr: String;
  TmpDir: String;
  PerPlayer, ListParams: Boolean;
  CancelAfter: Integer;
  Only: TStringList;
  i: Integer;
  p: TAGParamInfo;
  t0: QWord;

Begin
  OutDir := '';
  ProfileFile := '';
  OnlyList := '';
  SeedStr := '';
  StyleStr := '';
  NameStr := '';
  PerPlayer := false;
  ListParams := false;
  CancelAfter := 0;
  TmpDir := '';
  i := 1;
  While i <= ParamCount Do Begin
    Case ParamStr(i) Of
      '--out': Begin inc(i); OutDir := ParamStr(i); End;
      '--profile': Begin inc(i); ProfileFile := ParamStr(i); End;
      '--seed': Begin inc(i); SeedStr := ParamStr(i); End;
      '--style': Begin inc(i); StyleStr := ParamStr(i); End;
      '--name': Begin inc(i); NameStr := ParamStr(i); End;
      '--only': Begin inc(i); OnlyList := ParamStr(i); End;
      '--cancel-after': Begin inc(i); CancelAfter := StrToIntDef(ParamStr(i), 0); End;
      '--per-player': PerPlayer := true;
      '--params': ListParams := true;
      '--list', '-h', '--help': Begin
          PrintHelp;
          exit;
        End;
    Else Begin
        writeln('Unknown parameter: ', ParamStr(i));
        halt(2);
      End;
    End;
    inc(i);
  End;
  If OutDir = '' Then Begin
    PrintHelp;
    halt(2);
  End;
  OutDir := IncludeTrailingPathDelimiter(ExpandFileName(OutDir));

  Ctx := TAGContext.Create;
  Output := TAGOutput.Create(OutDir);
  Cli := TCli.Create;
  Only := TStringList.Create;
  Try
    CleanupSetLeftovers(ExtractFilePath(ExcludeTrailingPathDelimiter(OutDir)));
    If (ProfileFile = '') And FileExists(OutDir + 'profile.ini') Then
      ProfileFile := OutDir + 'profile.ini';
    If ProfileFile <> '' Then Ctx.LoadFromFile(ProfileFile);
    If SeedStr = 'random' Then
      Ctx.Seed := NewRandomSeed
    Else If SeedStr <> '' Then
      Ctx.Seed := StrToQWord(SeedStr);
    If StyleStr <> '' Then Ctx.Style := StringToStyle(StyleStr, Ctx.Style);
    If NameStr <> '' Then Ctx.Name := NameStr;
    If PerPlayer Then Ctx.PerPlayer := true;

    Only.CommaText := OnlyList;
    For i := 0 To Only.Count - 1 Do
      If FindGenerator(Only[i]) < 0 Then Begin
        writeln('Unknown generator: ', Only[i]);
        halt(2);
      End;

    Output.Expected := ExpectedTotal(Ctx, Only);
    Output.OnProgress := @Cli.OnProgress;
    Output.CancelAfter := CancelAfter;

    writeln(format('Set "%s", seed %d, style %s', [Ctx.Name, Ctx.Seed, StyleToString(Ctx.Style)]));
    t0 := GetTickCount64;
    TmpDir := BeginSetSave(OutDir);
    If (Only.Count > 0) And DirectoryExists(OutDir) Then Begin
      // only some generators: the rest of the set is kept
      CopyTree(OutDir, TmpDir);
      DeleteFile(TmpDir + 'profile.ini');
    End;
    Output.Retarget(TmpDir);
    RunGenerators(Ctx, Output, Only);
    If Output.Cancelled Then Raise EAbort.Create('Cancelled');
    Ctx.SaveToFile(TmpDir + 'profile.ini'); // last: only now the folder is a complete set
    CommitSetSave(TmpDir, OutDir);
    TmpDir := '';
    writeln(format('%d files in %d ms', [Output.Files.Count, GetTickCount64 - t0]));

    If ListParams Then Begin
      writeln;
      writeln('Parameters:');
      For i := 0 To Ctx.ParamCount - 1 Do Begin
        p := Ctx.Params[i];
        writeln(format('  %s.%s = %s  (%s +- %s)%s', [p.Area, p.Name, AGFloatToStr(p.Value),
          AGFloatToStr(p.Base), AGFloatToStr(p.Spread), BoolToStr(p.Overridden, ' overridden', '')]));
      End;
    End;
  Except
    On EAbort Do Begin
      writeln('Cancelled, the set was not changed.');
      ExitCode := 1;
    End;
    On e: EAGSetInUse Do Begin
      writeln('Error: ', e.Message);
      ExitCode := 3;
    End;
    On e: Exception Do Begin
      writeln('Error: ', e.Message);
      ExitCode := 1;
    End;
  End;
  DiscardSetSave(TmpDir);
  Only.Free;
  Cli.Free;
  Output.Free;
  Ctx.Free;
End;

End.
