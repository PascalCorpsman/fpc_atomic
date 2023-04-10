(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of config_td                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)

{.$DEFINE UseLogger}// wenn Aktiv, dann nutzt der Chunkmanager die Logger Routinen

{.$DEFINE UseMagicHeader} // Wenn Aktiviert, dann wird jedem Chunk eine Kennung vorangestellt, diese prüft dann zusätzlich noch einmal ob die Daten stimmen

