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
Const

  (*
   * Der Updater braucht die "int" version nicht, die ist nur für Config_td, klar dass beide immer gleich sein müssen !
   *)
  updater_Version = '0.07003'; // Zum Unterscheiden im Updater kann man auch gern "unterversionen nehmen" die Zahl wird als Floatingpoint angenommen.
  updater_int_Version = 007;

  updater_AppName = 'FPC Atomic';
  updater_StarterName = 'fpc_atomic';
  updater_UpdaterName = 'updater';
  updater_DownloadLink = 'https://corpsman.de/download/atomic.zip';
  updater_Additional = ''; // Liste der Potenziellen weiteren Datenfelder alle durch , getrennt.

