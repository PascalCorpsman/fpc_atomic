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
 * A generator writes all files of its area with Output.Save*. It must only use
 * Ctx (seed, style, parameters) and never global state, so the same profile
 * always gives the same files.
 *)
Unit uag_registry;

{$MODE ObjFPC}{$H+}

Interface

Uses uag_context, uag_output;

Type

  TAGGeneratorProc = Procedure(Ctx: TAGContext; Output: TAGOutput);

  TAGGenerator = Record
    Name: String; // used for --only, e.g. 'bomb'
    Description: String;
    ExpectedFiles: Integer; // for the progress bar, an estimate is fine
    Proc: TAGGeneratorProc;
  End;

Procedure RegisterGenerator(Const Name, Description: String; ExpectedFiles: Integer; Proc: TAGGeneratorProc);
Function GeneratorCount: Integer;
Function GetGenerator(Index: Integer): TAGGenerator;
Function FindGenerator(Const Name: String): Integer; // -1 if unknown

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
  Generators[high(Generators)].Proc := Proc;
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

End.
