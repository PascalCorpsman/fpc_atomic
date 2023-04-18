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

(*
 * Zur Unterscheidung ob Server oder Client
 * Da die IDE hier durcheinander kommt ist dieses Define gesetzt unter
 * Projekt -> Projekteinstellungen -> Compilereinstellungen -> Benutzerdefinierte Einstellungen
 *      -dClient
 *      -dServer
 *)
{.$define Client}
{.$define Server}

(*
 * Auf die Schnelle alles in Release Mode umstellen, kann auch via -dRelease
 *)
{.$define Release}

(*
 * Wenn dieser Switch gesetzt ist, dann werden Hochfrequente Nachrichten nicht im Trace geloggt.
 *
 * Ignoriert werden :
 *
 * - miHeartBeat
 * - miUpdateGameData
 * - miClientKeyEvent
 *)
{$DEFINE DoNotLog_CyclicMessages}

{$IFNDEF Release}
(*
 * Aktiviert die Auswertung von OpenGL Fehlern nach jedem Rendering
 *)
{$DEFINE DebuggMode}

(*
 * Wenn Aktiviert, dann loggt sich der Client automatisch in einen Server ein (wenn Parameter -d) gesetzt wurde
 *)
{$DEFINE AUTOMODE}

(*
 * Wenn Aktiviert, dann gibt es nur 3 anstatt der typischerweise 10 Spieler
 * -> Zum Entwickeln schneller
 *)
{$DEFINE Only3Player}

(*
 * Wenn Aktiviert, dann wird nur die 1. Karte geladen und nicht alle
 * -> Zum Entwickeln schneller
 *)
{$DEFINE Only1Map}

(*
 * Wenn Aktiviert, dann wird nur die 1. Animation aus den Direktories geladen
 * -> Zum Entwickeln schneller
 *)
{$DEFINE Only1Directory}

(*
 * Wenn Aktiviert, dann sieht man nach dem Laden aller Graphiken, wie Lange das ganze gedauert hat
 * -> Sollte nie im Produktivcode sein
 *)
{.$DEFINE ShowInitTime}

{$ENDIF}

