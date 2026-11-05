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
 * Command line version of the asset generator: computes a complete graphic
 * set from a seed (and optional profile) into a folder with the layout of the
 * data folder of the game.
 *)
Program asset_generator;

{$MODE ObjFPC}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  uag_cli,
  // every generator unit registers itself, the order here is the order of generation
  uag_gen_selftest,
  uag_gen_items,
  uag_gen_atomic,
  uag_gen_world,
  uag_gen_screens,
  uag_gen_schemes,
  uag_gen_sounds,
  uag_preview;

Begin
  RunCli;
End.
