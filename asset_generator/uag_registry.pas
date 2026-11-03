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
 * List of all generators. Every generator unit registers itself in its
 * initialization section:
 *
 *   RegisterGenerator('bomb', 'Bombs (bomb*.png)', 3, @GenerateBomb);
 *
 * If the number of files depends on the context (e.g. Ctx.PerPlayer), pass a
 * function instead of the fixed count. ExpectedTotal sums up the files of a
 * run for the progress bar, RunGenerators runs them.
 *
 * A generator writes all files of its area with Output.Save*. It must only use
 * Ctx (seed, style, parameters) and never global state, so the same profile
 * always gives the same files.
 *)
Unit uag_registry;

{$MODE ObjFPC}{$H+}

Interface

Uses Classes, uag_context, uag_output;

Type

  TAGGeneratorProc = Procedure(Ctx: TAGContext; Output: TAGOutput);
  TAGExpectedFunc = Function(Ctx: TAGContext): Integer;

  TAGGenerator = Record
    Name: String; // used for --only, e.g. 'bomb'
    Description: String;
    ExpectedFiles: Integer; // for the progress bar, used if ExpectedFunc = Nil
    ExpectedFunc: TAGExpectedFunc;
    Proc: TAGGeneratorProc;
  End;

Procedure RegisterGenerator(Const Name, Description: String; ExpectedFiles: Integer; Proc: TAGGeneratorProc);
Procedure RegisterGenerator(Const Name, Description: String; ExpectedFunc: TAGExpectedFunc; Proc: TAGGeneratorProc);
Function GeneratorCount: Integer;
Function GetGenerator(Index: Integer): TAGGenerator;
Function FindGenerator(Const Name: String): Integer; // -1 if unknown
Function GeneratorExpected(Index: Integer; Ctx: TAGContext): Integer;

(*
 * Only = Nil or empty: all generators except 'selftest', else the generators
 * named in Only. ExpectedTotal does not count profile.ini.
 *)
Function GeneratorSelected(Index: Integer; Only: TStrings): Boolean;
Function ExpectedTotal(Ctx: TAGContext; Only: TStrings): Integer;
Procedure RunGenerators(Ctx: TAGContext; Output: TAGOutput; Only: TStrings);

Implementation

Uses SysUtils;

Var
  Generators: Array Of TAGGenerator;

Procedure RegisterGenerator(Const Name, Description: String; ExpectedFiles: Integer; Proc: TAGGeneratorProc);
Begin
  If FindGenerator(Name) >= 0 Then Raise Exception.Create('Generator registered twice: ' + Name);
  setlength(Generators, length(Generators) + 1);
  Generators[high(Generators)].Name := Name;
  Generators[high(Generators)].Description := Description;
  Generators[high(Generators)].ExpectedFiles := ExpectedFiles;
  Generators[high(Generators)].ExpectedFunc := Nil;
  Generators[high(Generators)].Proc := Proc;
End;

Procedure RegisterGenerator(Const Name, Description: String; ExpectedFunc: TAGExpectedFunc; Proc: TAGGeneratorProc);
Begin
  RegisterGenerator(Name, Description, 0, Proc);
  Generators[high(Generators)].ExpectedFunc := ExpectedFunc;
End;

Function GeneratorCount: Integer;
Begin
  result := length(Generators);
End;

Function GetGenerator(Index: Integer): TAGGenerator;
Begin
  result := Generators[Index];
End;

Function FindGenerator(Const Name: String): Integer;
Var
  i: Integer;
Begin
  For i := 0 To high(Generators) Do
    If Generators[i].Name = Name Then exit(i);
  result := -1;
End;

Function GeneratorExpected(Index: Integer; Ctx: TAGContext): Integer;
Begin
  If assigned(Generators[Index].ExpectedFunc) Then
    result := Generators[Index].ExpectedFunc(Ctx)
  Else
    result := Generators[Index].ExpectedFiles;
End;

Function GeneratorSelected(Index: Integer; Only: TStrings): Boolean;
Begin
  If (Only = Nil) Or (Only.Count = 0) Then
    result := Generators[Index].Name <> 'selftest'
  Else
    result := Only.IndexOf(Generators[Index].Name) >= 0;
End;

Function ExpectedTotal(Ctx: TAGContext; Only: TStrings): Integer;
Var
  i: Integer;
Begin
  result := 0;
  For i := 0 To high(Generators) Do
    If GeneratorSelected(i, Only) Then
      result := result + GeneratorExpected(i, Ctx);
End;

Procedure RunGenerators(Ctx: TAGContext; Output: TAGOutput; Only: TStrings);
Var
  i: Integer;
Begin
  For i := 0 To high(Generators) Do
    If GeneratorSelected(i, Only) Then
      Generators[i].Proc(Ctx, Output);
End;

End.
