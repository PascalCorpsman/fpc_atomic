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
Unit ucdextractor;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Type
  TAniJob = Record
    SourceAni: String;
    Width, Height: integer;
    FramesPerRow: integer;
    Alignment: TAlignment;
    Layout: ttextlayout;
    ImageSequence: String;
    DestPng: String;
  End;

  (*
   * Partly separated to be able to use it in the ani job creator ;)
   *)
Function DoAniJob(CDFolder: String; AniJob: TAniJob): TBitmap;

Function CheckCDRootFolder(aFolder: String): boolean;
Function CheckFPCAtomicFolder(aFolder: String): boolean;

Procedure ExtractAtomicAnis(CDFolder, AtomicFolder: String); // in Work
Procedure ExtractAtomicSounds(CDFolder, AtomicFolder: String); // Fertig, getestet.
Procedure ExtractAtomicShemes(CDFolder, AtomicFolder: String); // Fertig, getestet.

Implementation

Uses
  FileUtil, math
  , uwave
  , uanifile
  , ugraphics
  , Unit1 // Bäh invalid dependency!
  ;

Type
  TSoundJob = Record
    Sourcefile: String; // Filename on Atomic Disc
    Destname: String; // Filename in FPC_Atomic
  End;

Const

  SoundJobs: Array[0..96] Of TSoundJob =
  (
    // data/maps/Field** [11]
    (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'GRNACRES.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field00' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'GENERIC.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field01' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'HOCKEY.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field02' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'PYRAMID.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field03' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'MINESHFT.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field04' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'BATTLE.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field05' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'GIEGER.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field06' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'HAUNTED.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field07' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'OCEAN.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field08' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'SWAMP.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field09' + PathDelim + 'sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'SEWER.RSS'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field10' + PathDelim + 'sound.wav') // Fertig, getestet
    // data/res [6]
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'DRAW.RSS'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'draw.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'NETWORK.RSS'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'join_sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'MENU.RSS'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'mainmenu_sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'MENUEXIT.RSS'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'menuexit.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'WIN.RSS'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'player_setup_sound.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'QUITGAME.RSS'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'quitgame.wav') // Fertig, getestet
    // data/sounds [80]
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + '1000.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + '1000.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + '1017.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + '1017.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + '1036.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + '1036.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'allrite.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'allrite.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmbstop1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmbstop1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmbstop2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmbstop2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmbthrw1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmbthrw1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmbthrw3.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmbthrw3.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmbthrw4.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmbthrw4.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmbthrw5.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmbthrw5.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmdrop1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmdrop1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmdrop2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmdrop2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bmdrop3.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bmdrop3.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_01.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_01.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_02.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_02.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_04.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_04.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_04b.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_04b.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_05.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_05.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_06.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_06.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_07.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_07.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_07b.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_07b.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_09.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_09.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_11.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_11.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_12.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_12.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_12b.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_12b.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_17.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_17.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_19.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_19.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bomb_24.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bomb_24.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bombboun.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bombboun.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'bombstop.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'bombstop.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'clikplat.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'clikplat.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'coolpop.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'coolpop.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'cribrown.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'cribrown.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'cul8r.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'cul8r.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'die1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'die1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'disease1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'disease1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'disease2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'disease2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'disease3.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'disease3.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'eatdust.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'eatdust.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'expl6.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'expl6.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'explo1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'explo1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'explode2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'explode2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'explode3.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'explode3.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'explode4.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'explode4.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'get1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'get1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'get2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'get2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'gotahurt.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'gotahurt.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'gotcha.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'gotcha.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'grab1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'grab1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'grab2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'grab2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'hurry.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'hurry.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'hurytuf.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'hurytuf.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'kbomb1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'kbomb1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'kbomb2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'kbomb2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'kicker3.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'kicker3.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'kicker10.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'kicker10.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'later.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'later.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'ohno1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'ohno1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'proud.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'proud.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'roasted.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'roasted.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'schwing.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'schwing.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'scream1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'scream1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'smelsmok.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'smelsmok.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'sqrdrop2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'sqrdrop2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'sqrdrop4.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'sqrdrop4.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'sqrdrop5.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'sqrdrop5.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'sqrdrop6.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'sqrdrop6.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'sqrdrop7.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'sqrdrop7.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'sqrdrop8.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'sqrdrop8.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'stupidio.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'stupidio.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'suckitdn.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'suckitdn.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'tastpai2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'tastpai2.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'tastpain.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'tastpain.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'theman.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'theman.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'toeasy.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'toeasy.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'woohoo1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'woohoo1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'youblow.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'youblow.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'youwin1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'youwin1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'zen1.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'zen1.wav') // Fertig, getestet
    , (Sourcefile: 'data' + PathDelim + 'sound' + PathDelim + 'zen2.RSS'; Destname: 'data' + PathDelim + 'sounds' + PathDelim + 'zen2.wav') // Fertig, getestet
    );

Var
  AniJobs: Array Of TAniJob; // All the .ani jobs will be set in the initialization part of the unit.

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
    If pos(lowercase(match), lowercase(sl[i])) <> 0 Then Begin
      result := sl[i];
      break;
    End;
  End;
  sl.free;
End;

(*
 * Sucht in Folder nach Match
 *)

Function GetFileByMatch(Folder, Match: String): String;
Var
  sl: TStringList;
  i: Integer;
Begin
  result := '';
  // TODO: Wenn Folder nicht existiert, dann muss geschaut werden ob es nur an der Schreibweise liegt und ggf auch hier ein passendes gewählt werden.!
  sl := FindAllFiles(Folder, '*', false);
  For i := 0 To sl.Count - 1 Do Begin
    If pos(lowercase(match), lowercase(sl[i])) <> 0 Then Begin
      result := sl[i];
      break;
    End;
  End;
  sl.free;
End;

Function DoAniJob(CDFolder: String; AniJob: TAniJob): TBitmap;
Var
  ani: TAniFile;
  AniFilename: String;
  cnt, w, h, fpr, i, x, y: integer;
  elements: TStringArray;
  s: String;
  index: LongInt;
  SubImage: TBitmap;
Begin
  result := Nil;
  w := AniJob.Width;
  h := AniJob.Height;
  fpr := AniJob.FramesPerRow;
  If fpr = 0 Then Begin
    exit;
  End;
  AniFilename := GetFileByMatch(ExtractFilePath(CDFolder + AniJob.SourceAni), AniJob.SourceAni);
  If AniFilename = '' Then Begin
    AddLog('  Error: could not find: ' + AniJob.SourceAni);
    exit;
  End;
  ani := TAniFile.Create();
  If Not ani.LoadFromFile(AniFilename) Then Begin
    ani.free;
    exit;
  End;
  s := AniJob.ImageSequence;
  elements := s.Split(',');
  cnt := Length(elements);
  Result := TBitmap.Create;
  Result.Width := w * fpr;
  Result.Height := h * ceil(cnt / fpr);
  Result.Canvas.Brush.Color := clFuchsia;
  Result.Canvas.Brush.Style := bsSolid;
  Result.Canvas.Rectangle(-1, -1, Result.Width + 1, Result.Height + 1);
  For i := 0 To high(elements) Do Begin
    index := strtointdef(elements[i], -1);
    If (index >= ani.ImageCount) Or (index < 0) Then Begin
      ani.free;
      result.free;
      result := Nil;
      exit;
    End;
    SubImage := TBitmap.Create;
    Subimage.Width := w;
    SubImage.Height := h;
    Case AniJob.Alignment Of
      taLeftJustify: x := 0;
      taCenter: x := (w - Ani.Image[index].Bitmap.Width) Div 2;
      taRightJustify: x := (w - Ani.Image[index].Bitmap.Width);
    End;
    Case AniJob.Layout Of
      tlTop: y := 0;
      tlCenter: y := (h - Ani.Image[index].Bitmap.Height) Div 2;
      tlBottom: y := (h - Ani.Image[index].Bitmap.Height);
    End;
    SubImage.Canvas.Draw(x, y, Ani.Image[index].Bitmap);
    SwapColor(SubImage, clBlack, clFuchsia);
    Result.Canvas.Draw(w * (i Mod fpr), h * (i Div fpr), SubImage);
    subimage.free;
  End;
  ani.free;
End;

Procedure ExtractAtomicAnis(CDFolder, AtomicFolder: String);
Var
  i: Integer;
  b: TBitmap;
  p: TPortableNetworkGraphic;
  cnt: integer;
  dn: String;
Begin
  CDFolder := IncludeTrailingPathDelimiter(CDFolder);
  AtomicFolder := IncludeTrailingPathDelimiter(AtomicFolder);
  cnt := 0;
  For i := 0 To high(AniJobs) Do Begin
    b := DoAniJob(CDFolder, AniJobs[i]);
    If assigned(b) Then Begin
      p := TPortableNetworkGraphic.Create;
      p.assign(b);
      dn := AtomicFolder + AniJobs[i].DestPng;
      If Not ForceDirectories(ExtractFilePath(dn)) Then Begin
        AddLog('  Error: could not create folder: ' + ExtractFilePath(dn));
      End
      Else Begin
        Try
          p.SaveToFile(dn);
          inc(cnt);
        Except
          AddLog('  Error: could not save: ' + dn);
        End;
      End;
      p.free;
      b.free;
    End
    Else Begin
      AddLog(format('  Error: %s could not be done.', [AniJobs[i].SourceAni]));
    End;
  End;
  AddLog(format('  %d files created', [cnt]));
End;

Procedure ExtractAtomicSounds(CDFolder, AtomicFolder: String);
Var
  samplecnt, cnt, i, j: integer;
  sSoundFile, tSoundFile: String;
  SoundWarning: Boolean;
  wav: TWave;
  sFile: TFileStream;
  sint: int32;
Begin
  SoundWarning := false;
  cnt := 0;
  CDFolder := IncludeTrailingPathDelimiter(CDFolder);
  AtomicFolder := IncludeTrailingPathDelimiter(AtomicFolder);
  (*
   * normally you would see here a for i := 0 to high(SoundJobs) do begin
   *
   * but during development i did not want to do all jobs again and again and again
   * so i switchted to a repeat and therefor was able to init i with higher values
   * and skip the already tested ones.
   *)
  i := 0;
  Repeat
    // Find the source file
    sSoundFile := GetFileByMatch(ExtractFilePath(CDFolder + SoundJobs[i].Sourcefile), SoundJobs[i].Sourcefile);
    If sSoundFile = '' Then Begin
      AddLog('  Error could not locate: ' + SoundJobs[i].Sourcefile);
      SoundWarning := true;
      Continue;
    End;
    // Load the source file stream and convert into a .wav file
    wav := TWave.Create;
    sFile := TFileStream.Create(sSoundFile, fmOpenRead);
    samplecnt := sFile.Size Div sizeof(sint);
    wav.InitNewBuffer(1, 22050, 16, samplecnt);
    Sint := 0;
    For j := 0 To samplecnt - 1 Do Begin
      sfile.read(Sint, sizeof(sint));
      wav.Sample[0, j] := sint / 2147483647; // Convert into [-1 .. 1]
    End;
    sFile.Free;
    // Store the thing ;)
    tSoundFile := AtomicFolder + SoundJobs[i].Destname;
    If Not ForceDirectories(ExtractFilePath(tSoundFile)) Then Begin
      addlog('  Error unable to create dir: ' + ExtractFilePath(tSoundFile));
      wav.free;
      Continue;
    End;
    If wav.SaveToFile(tSoundFile) Then Begin
      inc(cnt);
    End
    Else Begin
      addlog('  Error unable to save: ' + tSoundFile);
    End;
    wav.free;
    inc(i);
  Until i > high(SoundJobs);
  AddLog(format('  %d files created', [cnt]));
  If SoundWarning Then Begin
    Addlog('  with missing sounds the game is still playable, but with less fun.');
  End;
End;

Procedure ExtractAtomicShemes(CDFolder, AtomicFolder: String);
Var
  sSchemeFolder, tSchemeFolder: String;
  sl: TStringList;
  cnt, i: Integer;
Begin
  sSchemeFolder := GetFolderByMatch(CDFolder, 'data' + PathDelim + 'schemes');
  If sSchemeFolder = '' Then Begin
    addlog('  Error, unable to find schemes folder.');
    exit;
  End;
  tSchemeFolder := IncludeTrailingPathDelimiter(AtomicFolder) + 'data' + PathDelim + 'schemes';
  If Not ForceDirectories(tSchemeFolder) Then Begin
    addlog('  Error, could not create: ' + tSchemeFolder);
    exit;
  End;
  tSchemeFolder := IncludeTrailingPathDelimiter(tSchemeFolder);
  sl := FindAllFiles(sSchemeFolder, '*', false);
  cnt := 0;
  For i := 0 To sl.Count - 1 Do Begin
    If lowercase(ExtractFileExt(sl[i])) = '.sch' Then Begin
      If CopyFile(sl[i], tSchemeFolder + uppercase(ExtractFileName(sl[i]))) Then inc(cnt);
    End;
  End;
  AddLog(format('  %d files copied', [cnt]));
  sl.free;
End;

Procedure AddAniJob(SourceAni: String; Width, Height, FPR: integer; Alignment: TAlignment; Layout: ttextlayout; ImageSequence, DestPng: String);
Begin
  setlength(AniJobs, high(AniJobs) + 2);
  AniJobs[high(AniJobs)].SourceAni := SourceAni;
  AniJobs[high(AniJobs)].Width := Width;
  AniJobs[high(AniJobs)].Height := Height;
  AniJobs[high(AniJobs)].FramesPerRow := FPR;
  AniJobs[high(AniJobs)].Alignment := Alignment;
  AniJobs[high(AniJobs)].Layout := Layout;
  AniJobs[high(AniJobs)].ImageSequence := ImageSequence;
  AniJobs[high(AniJobs)].DestPng := DestPng;
End;

Initialization

  AniJobs := Nil;

  (*
   * Paste here the content from "Copy job to clipboard" button.
   *)
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'BOMBS.ANI', 40, 37, 5, taCenter, tlBottom, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1', 'data' + Pathdelim + 'atomic' + Pathdelim + 'bomb.png');
End.

