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
Unit ucdextractor;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Type
  TTransparentMode = (tmBlack, tmFirstPixel, tmFirstPixelPerFrame);

  TAniJob = Record
    SourceAni: String;
    Width, Height: integer;
    FramesPerRow: integer;
    Alignment: TAlignment;
    Layout: ttextlayout;
    ImageSequence: String;
    DestPng: String;
    TransparentMode: TTransparentMode;
  End;

  TCascadeJob = Array Of TAniJob;

  TSpriteData = Record
    FrameOffset: Integer;
    FrameCount: integer;
  End;

  TAniToAniJob = Record
    AniJob: TAniJob;
    AngleOffset: Single;
    FramesPerRow: integer;
    FramesPerCol: integer;
    TimePerFrame: integer;
    SpriteData: Array Of TSpriteData;
  End;


  (*
   * Partly separated to be able to use it in the ani job creator ;)
   *)
Function DoAniJob(CDFolder: String; Job: TAniJob): TBitmap;

Function CheckCDRootFolder(aFolder: String): boolean;
Function CheckFPCAtomicFolder(aFolder: String): boolean;

Procedure ExtractAtomicAnis(CDFolder, AtomicFolder: String); // Fertig, getestet.
Procedure ExtractAtomicAniToAnis(CDFolder, AtomicFolder: String); // in Work
Procedure ExtractAtomicPCXs(CDFolder, AtomicFolder: String); // Fertig, getestet.
Procedure ExtractAtomicSounds(CDFolder, AtomicFolder: String); // Fertig, getestet.
Procedure ExtractAtomicShemes(CDFolder, AtomicFolder: String); // Fertig, getestet.

Implementation

Uses
  FileUtil, math
  , upcx
  , uwave
  , uanifile
  , ugraphics
  , uopengl_animation
  , Unit1 // Bäh invalid dependency!
  ;

Type

  TPCXJob = Record
    Sourcefile: String; // Filename on Atomic Disc
    Destname: String; // Filename in FPC_Atomic
  End;

  TSoundJob = Record
    Sourcefile: String; // Filename on Atomic Disc
    Destname: String; // Filename in FPC_Atomic
  End;

Const

  PCXJobs: Array[0..43] Of TPCXJob =
  (
    // data/maps/Field** [11]
    (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD0.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field00' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD1.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field01' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD2.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field02' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD3.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field03' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD4.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field04' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD5.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field05' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD6.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field06' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD7.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field07' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD8.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field08' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD9.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field09' + PathDelim + 'field.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'FIELD10.PCX'; Destname: 'data' + PathDelim + 'maps' + PathDelim + 'Field10' + PathDelim + 'field.png')
    // data/res [33]
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'DRAW.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'draw.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'GLUE1.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'fieldsetup.png') // -- This is the special case (handled hardcoded in the "ExtractAtomicPCXs" routine)
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'GLUE5.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'join.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'WINZ.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'loaddialog.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'MAINMENU.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'mainmenu.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'GLUE2.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'options.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWBOMB.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powbomb.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWDISEA.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powdisea.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWEBOLA.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powebola.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWFLAME.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powflame.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWGOLD.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powgold.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWGRAB.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powgrab.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWJELLY.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powjelly.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWKICK.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powkick.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWPUNCH.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powpunch.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWRAND.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powrand.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWSKATE.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powskate.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWSLOW.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powslow.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWSPOOG.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powspoog.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'POWTRIG.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'powtrig.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'RESULTS.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'results.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'TEAM0.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'team0.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'TEAM1.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'team1.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY0.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory0.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY1.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory1.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY2.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory2.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY3.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory3.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY4.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory4.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY5.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory5.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY6.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory6.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY7.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory7.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY8.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory8.png')
    , (Sourcefile: 'data' + PathDelim + 'res' + PathDelim + 'VICTORY9.PCX'; Destname: 'data' + PathDelim + 'res' + PathDelim + 'victory9.png')
    );

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
  CascadeJobs: Array Of TCascadeJob; // All jobs that where merged into one image.
  AniToAniJobs: Array Of TAniToAniJob; // All the jobs where a Atomic Bomberman Ani directly is converted into a FPC_Atomic.ani file

{$IFDEF Linux}
  (*
   * Searches Recursive trough folder and corrects all Subfolder directories according to der caseing
   *)

Function FindCaseInsensitiveFolder(Folder: String): String;
Var
  sl: TStringList;
  i: Integer;
  subfolder: String;
Begin
  If Folder = '' Then Begin
    result := '';
  End
  Else Begin
    If DirectoryExists(Folder) Then Begin
      result := Folder;
    End
    Else Begin
      subfolder := ExtractFileName(ExcludeTrailingPathDelimiter(Folder));
      Folder := FindCaseInsensitiveFolder(ExtractFilePath(ExcludeTrailingPathDelimiter(Folder)));
      If Folder <> '' Then Begin
        sl := FindAllDirectories(Folder, false);
        For i := 0 To sl.Count - 1 Do Begin
          If lowercase(sl[i]) = lowercase(folder + subfolder) Then Begin
            result := sl[i] + PathDelim;
            sl.free;
            exit;
          End;
        End;
        sl.free;
        result := '';
      End;
    End;
  End;
End;
{$ENDIF}

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
  NewFolder: String;
Begin
  result := '';
{$IFDEF Linux}
  (*
   * Linux is case sensitive, if the folder "Casing" does not match
   * -> fix that!
   *)
  If Not DirectoryExists(Folder) Then Begin
    NewFolder := FindCaseInsensitiveFolder(Folder);
    If lowercase(folder) <> lowercase(NewFolder) Then Begin
      result := '';
      exit;
    End
    Else Begin
      Folder := newFolder;
    End;
  End;
{$ENDIF}
  sl := FindAllFiles(Folder, '*', false);
  For i := 0 To sl.Count - 1 Do Begin
    If pos(lowercase(match), lowercase(sl[i])) <> 0 Then Begin
      result := sl[i];
      break;
    End;
  End;
  sl.free;
End;

Function DoAniJob(CDFolder: String; Job: TAniJob): TBitmap;
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
  w := Job.Width;
  h := Job.Height;
  fpr := Job.FramesPerRow;
  If fpr = 0 Then Begin
    exit;
  End;
  AniFilename := GetFileByMatch(ExtractFilePath(CDFolder + Job.SourceAni), Job.SourceAni);
  If AniFilename = '' Then Begin
    AddLog('  Error: could not find: ' + Job.SourceAni);
    exit;
  End;
  ani := TAniFile.Create();
  If Not ani.LoadFromFile(AniFilename) Then Begin
    ani.free;
    exit;
  End;
  s := Job.ImageSequence;
  elements := s.Split(',');
  cnt := Length(elements);
  Result := TBitmap.Create;
  Result.Transparent := false;
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
    SubImage.Transparent := false;
    Subimage.Width := w;
    SubImage.Height := h;
    SubImage.Canvas.Brush.Color := clFuchsia;
    SubImage.Canvas.Brush.Style := bsSolid;
    SubImage.Canvas.Rectangle(-1, -1, SubImage.Width + 1, SubImage.Height + 1);
    Case Job.Alignment Of
      taLeftJustify: x := 0;
      taCenter: x := (w - Ani.Image[index].Bitmap.Width) Div 2;
      taRightJustify: x := (w - Ani.Image[index].Bitmap.Width);
    End;
    Case Job.Layout Of
      tlTop: y := 0;
      tlCenter: y := (h - Ani.Image[index].Bitmap.Height) Div 2;
      tlBottom: y := (h - Ani.Image[index].Bitmap.Height);
    End;
    If job.TransparentMode = tmFirstPixelPerFrame Then Begin
      SwapColor(Ani.Image[index].Bitmap, Ani.Image[index].Bitmap.Canvas.Pixels[0, 0], clFuchsia);
    End;
    SubImage.Canvas.Draw(x, y, Ani.Image[index].Bitmap);
    Result.Canvas.Draw(w * (i Mod fpr), h * (i Div fpr), SubImage);
    subimage.free;
  End;
  ani.free;
  Case job.TransparentMode Of
    tmFirstPixel: SwapColor(result, result.Canvas.Pixels[0, 0], clFuchsia);
    tmBlack: SwapColor(result, clblack, clFuchsia);
  End;
End;

Procedure ExtractAtomicAnis(CDFolder, AtomicFolder: String);
  Function StoreBmp(b: TBitmap; Filename: String): Boolean;
  Var
    p: TPortableNetworkGraphic;
  Begin
    result := false;
    p := TPortableNetworkGraphic.Create;
    p.assign(b);
    If Not ForceDirectories(ExtractFilePath(Filename)) Then Begin
      AddLog('  Error: could not create folder: ' + ExtractFilePath(Filename));
    End
    Else Begin
      Try
        p.SaveToFile(Filename);
        result := true;
      Except
        AddLog('  Error: could not save: ' + Filename);
      End;
    End;
    p.free;
  End;

Var
  i: Integer;
  b: TBitmap;
  cnt, j, k: integer;
  bmps: Array Of TBitmap;
Begin
  CDFolder := IncludeTrailingPathDelimiter(CDFolder);
  AtomicFolder := IncludeTrailingPathDelimiter(AtomicFolder);
  cnt := 0;
  // The Normal Jobs
  For i := 0 To high(AniJobs) Do Begin
    b := DoAniJob(CDFolder, AniJobs[i]);
    If Not assigned(b) Then Begin
      Continue;
    End;
    (*
     * Unfortunatunelly this special texture needs a little "Fix", which is here
     * hardcoded ;)
     *)
    If pos('flame.png', lowercase(AniJobs[i].DestPng)) <> 0 Then Begin
      SwapColor(b, $00F800F8, clFuchsia);
    End;
    If assigned(b) Then Begin
      If StoreBmp(b, AtomicFolder + AniJobs[i].DestPng) Then inc(cnt);
      b.free;
    End
    Else Begin
      AddLog(format('  Error: %s could not be done.', [AniJobs[i].SourceAni]));
    End;
  End;
  // The Cascade Jobs
  bmps := Nil;
  For i := 0 To high(CascadeJobs) Do Begin
    setlength(bmps, length(CascadeJobs[i]));
    For j := 0 To high(CascadeJobs[i]) Do Begin
      bmps[j] := DoAniJob(CDFolder, CascadeJobs[i, j]);
      If Not assigned(bmps[j]) Then Begin
        For k := 0 To j - 1 Do Begin
          bmps[k].free;
        End;
        setlength(bmps, 0);
        AddLog(format('  Error: %s could not be done.', [CascadeJobs[i, j].SourceAni]));
        break;
      End;
    End;
    If Not Assigned(bmps) Then Continue;
    b := TBitmap.Create;
    b.Width := bmps[0].Width;
    b.Height := bmps[0].Height * length(bmps);
    b.Transparent := false;
    For j := high(bmps) Downto 0 Do Begin // as we free the bmp in the same loop and always refer to image 0, the loop needs to run inverted !
      bmps[j].Transparent := false;
      b.canvas.Draw(0, j * bmps[0].Height, bmps[j]);
      bmps[j].free;
    End;
    setlength(bmps, 0);
    If StoreBmp(b, AtomicFolder + CascadeJobs[i, high(CascadeJobs[i])].DestPng) Then inc(cnt);
    b.free;
  End;
  AddLog(format('  %d files created', [cnt]));
End;

Procedure ExtractAtomicAniToAnis(CDFolder, AtomicFolder: String);
Var
  cnt, i, j: integer;
  b: TBitmap;
  ani: TOpenGL_Animation;
  s: TAniSprite;
  targetAniFile: String;
Begin
  cnt := 0;
  CDFolder := IncludeTrailingPathDelimiter(CDFolder);
  AtomicFolder := IncludeTrailingPathDelimiter(AtomicFolder);
  For i := 0 To high(AniToAniJobs) Do Begin
    b := DoAniJob(CDFolder, AniToAniJobs[i].AniJob);
    If Not assigned(b) Then Begin
      Continue;
    End;
    ani := TOpenGL_Animation.Create;
    ani.AngleOffset := AniToAniJobs[i].AngleOffset;
    For j := 0 To high(AniToAniJobs[i].SpriteData) Do Begin
      If j <> 0 Then ani.AddRange(true); // the first range already exists -> create one less ;)
      s := ani.Sprite[j];
      If j = 0 Then Begin // The first sprite gets the Image, all others derive from it.
        s.Bitmap := b;
        s.Derived := false;
      End
      Else Begin
        s.Bitmap := Nil;
        s.Derived := true;
      End;
      s.AlphaImage := true;
      s.AlphaMask := Nil;
      s.TimePerFrame := AniToAniJobs[i].TimePerFrame;
      s.rect := rect(0, 0, b.Width, b.Height);
      // s.Name := targetAniFile; -- Wird automatisch gesetzt
      s.Width := b.Width Div AniToAniJobs[i].FramesPerRow;
      s.Height := b.height Div AniToAniJobs[i].FramesPerCol;
      s.FramesPerRow := AniToAniJobs[i].FramesPerRow;
      s.FramesPerCol := AniToAniJobs[i].FramesPerCol;
      s.FrameOffset := AniToAniJobs[i].SpriteData[j].FrameOffset;
      s.FrameCount := AniToAniJobs[i].SpriteData[j].FrameCount;
      ani.Sprite[j] := s;
    End;
    targetAniFile := AtomicFolder + AniToAniJobs[i].AniJob.DestPng;
    If Not ForceDirectories(ExtractFilePath(targetAniFile)) Then Begin
      addlog('  Error unable to create dir: ' + ExtractFilePath(targetAniFile));
      ani.free;
      Continue;
    End;
    Try
      ani.SaveToFile(targetAniFile);
      inc(cnt);
    Except
      addlog('  Error unable to save: ' + targetAniFile);
    End;
    // b.free; -- the sprite handles the Bitmap now !
    ani.free;
  End;
  AddLog(format('  %d files created', [cnt]));
End;

Procedure ExtractAtomicPCXs(CDFolder, AtomicFolder: String);
Var
  cnt, i: integer;
  pcx: TPCX;
  sPCXFile, tPCXFile: String;
  b: TBitmap;
  p: TPortableNetworkGraphic;
Begin
  cnt := 0;
  CDFolder := IncludeTrailingPathDelimiter(CDFolder);
  AtomicFolder := IncludeTrailingPathDelimiter(AtomicFolder);
  For i := 0 To high(PCXJobs) Do Begin
    sPCXFile := GetFileByMatch(ExtractFilePath(CDFolder + PCXJobs[i].Sourcefile), PCXJobs[i].Sourcefile);
    If sPCXFile = '' Then Begin
      AddLog('  Error could not locate: ' + PCXJobs[i].Sourcefile);
      Continue;
    End;
    pcx := TPCX.Create();
    pcx.LoadFromFile(sPCXFile);
    b := pcx.AsTBitmap();
    pcx.free;
    p := TPortableNetworkGraphic.Create;
    (*
     * This Image has to be "hacked" a window in, which is not part of the "orig" files
     *)
    If pos('fieldsetup.png', PCXJobs[i].Destname) <> 0 Then Begin
      b.Canvas.Pen.Color := clwhite;
      b.Canvas.Pen.Width := 2;
      b.Canvas.Brush.Color := clFuchsia;
      b.Canvas.Rectangle(379, 81, 601, 282);
      b.Canvas.Pixels[378, 80] := clWhite;
      b.Canvas.Pixels[600, 80] := clWhite;
    End;
    p.assign(b);
    b.free;
    // Store the thing ;)
    tPCXFile := AtomicFolder + PCXJobs[i].Destname;
    If Not ForceDirectories(ExtractFilePath(tPCXFile)) Then Begin
      addlog('  Error unable to create dir: ' + ExtractFilePath(tPCXFile));
      p.free;
      Continue;
    End;
    Try
      p.SaveToFile(tPCXFile);
      inc(cnt);
    Except
      addlog('  Error unable to save: ' + tPCXFile);
    End;
    p.free;
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
      inc(i);
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

Procedure AddAniJob(SourceAni: String; Width, Height, FPR: integer; Alignment: TAlignment; Layout: ttextlayout; ImageSequence, DestPng: String; TransparentMode: TTransparentMode);
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
  AniJobs[high(AniJobs)].TransparentMode := TransparentMode;
End;

Procedure PopCascade(JobCount: Integer);
Var
  cj: TCascadeJob;
  i: Integer;
Begin
  cj := Nil;
  setlength(cj, JobCount);
  For i := 0 To JobCount - 1 Do Begin
    cj[high(cj) - i] := AniJobs[high(AniJobs) - i];
  End;
  setlength(AniJobs, Length(AniJobs) - JobCount);
  setlength(CascadeJobs, high(CascadeJobs) + 2);
  CascadeJobs[High(CascadeJobs)] := cj;
End;

(*
 * 2 Datapoints give 1 Dataset
 *  \- first  = FrameOffset
 *  \- second = FrameCount
 *)

Procedure PopAniJob(FramesPerRow, FramesPerCol, TimePerFrame: integer; AngleOffset: Single; DataPoints: Array Of Integer);
Var
  i: Integer;
Begin
  If length(DataPoints) Mod 2 <> 0 Then Begin
    Raise exception.create('Error, invalid settings.');
  End;
  setlength(AniToAniJobs, high(AniToAniJobs) + 2);
  AniToAniJobs[high(AniToAniJobs)].AniJob := AniJobs[high(AniJobs)];
  setlength(AniJobs, high(AniJobs));
  AniToAniJobs[high(AniToAniJobs)].FramesPerRow := FramesPerRow;
  AniToAniJobs[high(AniToAniJobs)].FramesPerCol := FramesPerCol;
  AniToAniJobs[high(AniToAniJobs)].TimePerFrame := TimePerFrame;
  AniToAniJobs[high(AniToAniJobs)].AngleOffset := AngleOffset;
  setlength(AniToAniJobs[high(AniToAniJobs)].SpriteData, length(DataPoints) Div 2);
  For i := 0 To high(AniToAniJobs[high(AniToAniJobs)].SpriteData) Do Begin
    AniToAniJobs[high(AniToAniJobs)].SpriteData[i].FrameOffset := DataPoints[i * 2];
    AniToAniJobs[high(AniToAniJobs)].SpriteData[i].FrameCount := DataPoints[i * 2 + 1];
  End;
End;

Initialization

  AniJobs := Nil;
  CascadeJobs := Nil;
  AniToAniJobs := Nil;

  (*
   * Paste here the content from "Copy job to clipboard" button.
   *)
  // data/atomic/die/*
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE1.ANI', 73, 73, 10, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die1.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE2.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die2.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE3.ANI', 73, 73, 8, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die3.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE4.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die4.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE5.ANI', 110, 110, 4, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die5.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE6.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die6.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE7.ANI', 110, 110, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die7.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE8.ANI', 110, 110, 4, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die8.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE9.ANI', 110, 110, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die9.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE10.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die10.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE11.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die11.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE12.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die12.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE13.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die13.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE14.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die14.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE15.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die15.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE16.ANI', 110, 110, 4, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die16.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE17.ANI', 110, 110, 9, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die17.png', tmFirstPixel); // Fertig, getestet
  // The animations 18 .. 21 are part of the extension pack which can be downloaded here: https://www.oocities.org/timessquare/tower/4056/ani.html
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'xplode18.ani', 110, 110, 9, taCenter, tlCenter, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die18.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE19.ANI', 141, 200, 4, taCenter, tlCenter, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die19.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE20.ANI', 34, 89, 7, taLeftJustify, tlCenter, '0, 1, 2, 3, 4, 5, 6', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die20.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XPLODE21.ANI', 79, 68, 4, taCenter, tlCenter, '8, 9, 10, 11, 12, 13, 14, 15, 16, 2, 0, 1, 3', 'data' + Pathdelim + 'atomic' + Pathdelim + 'die' + Pathdelim + 'die21.png', tmBlack); // Fertig, getestet
  // data/atomic/idle/*
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'APPLBITE.ANI', 73, 73, 11, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120', 'data' + Pathdelim + 'atomic' + Pathdelim + 'idle' + Pathdelim + 'applbite.png', tmFirstPixelPerFrame); // Fertig, getestet
  // Code formater crashes when lines are longer than 1000 Chars ;), therefore this one is separated into 3 lines.
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'HEADWIPE.ANI', 73, 73, 14, taLeftJustify, tlTop,
    '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210'
    , 'data' + Pathdelim + 'atomic' + Pathdelim + 'idle' + Pathdelim + 'headwipe.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'NUCKBLOW.ANI', 73, 73, 11, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120', 'data' + Pathdelim + 'atomic' + Pathdelim + 'idle' + Pathdelim + 'nuckblow.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'ZEN.ANI', 73, 73, 11, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120', 'data' + Pathdelim + 'atomic' + Pathdelim + 'idle' + Pathdelim + 'zen.png', tmFirstPixelPerFrame); // Fertig, getestet
  // data/atomic/locked_in/*
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CORNER0.ANI', 110, 110, 7, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30', 'data' + Pathdelim + 'atomic' + Pathdelim + 'locked_in' + Pathdelim + 'corner0.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CORNER1.ANI', 110, 110, 7, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44', 'data' + Pathdelim + 'atomic' + Pathdelim + 'locked_in' + Pathdelim + 'corner1.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CORNER2.ANI', 110, 110, 8, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38', 'data' + Pathdelim + 'atomic' + Pathdelim + 'locked_in' + Pathdelim + 'corner2.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CORNER3.ANI', 110, 110, 8, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50', 'data' + Pathdelim + 'atomic' + Pathdelim + 'locked_in' + Pathdelim + 'corner3.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CORNER4.ANI', 110, 110, 5, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19', 'data' + Pathdelim + 'atomic' + Pathdelim + 'locked_in' + Pathdelim + 'corner4.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CORNER5.ANI', 110, 110, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'data' + Pathdelim + 'atomic' + Pathdelim + 'locked_in' + Pathdelim + 'corner5.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CORNER6.ANI', 110, 110, 6, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17', 'data' + Pathdelim + 'atomic' + Pathdelim + 'locked_in' + Pathdelim + 'corner6.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CORNER7.ANI', 110, 110, 5, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19', 'data' + Pathdelim + 'atomic' + Pathdelim + 'locked_in' + Pathdelim + 'corner7.png', tmFirstPixel); // Fertig, getestet
  // data/atomic/*
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'BOMBS.ANI', 40, 37, 5, taCenter, tlBottom, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1', 'data' + Pathdelim + 'atomic' + Pathdelim + 'bomb.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'DUDS.ANI', 40, 40, 1, taLeftJustify, tlTop, '0, 1', 'data' + Pathdelim + 'atomic' + Pathdelim + 'bomb_dud.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TRIGANIM.ANI', 40, 40, 5, taCenter, tlBottom, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1', 'data' + Pathdelim + 'atomic' + Pathdelim + 'bomb_trigger.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'BOMBS.ANI', 40, 37, 4, taCenter, tlBottom, '10, 11, 12, 13, 14, 15, 16, 15, 14, 13, 12, 11', 'data' + Pathdelim + 'atomic' + Pathdelim + 'bomb_wobble.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'MFLAME.ANI', 41, 37, 5, taCenter, tlCenter, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44', 'data' + Pathdelim + 'atomic' + Pathdelim + 'flame.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'KICK.ANI', 50, 69, 10, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39', 'data' + Pathdelim + 'atomic' + Pathdelim + 'kick.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'PUNBOMB1.ANI', 110, 110, 10, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'unused', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'PUNBOMB2.ANI', 110, 110, 10, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'unused', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'PUNBOMB3.ANI', 110, 110, 10, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'unused', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'PUNBOMB4.ANI', 110, 110, 10, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'data' + Pathdelim + 'atomic' + Pathdelim + 'punbomb.png', tmFirstPixel); // Fertig, getestet
  PopCascade(4);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'PUP1.ANI', 110, 110, 11, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10', 'unused', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'PUP2.ANI', 110, 110, 11, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10', 'unused', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'PUP3.ANI', 110, 110, 11, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10', 'unused', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'PUP4.ANI', 110, 110, 11, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10', 'data' + Pathdelim + 'atomic' + Pathdelim + 'pupbomb.png', tmFirstPixel); // Fertig, getestet
  PopCascade(4);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'STAND.ANI', 110, 110, 2, taLeftJustify, tlTop, '0, 1, 2, 3', 'data' + Pathdelim + 'atomic' + Pathdelim + 'stand.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'WALK.ANI', 110, 110, 15, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59', 'data' + Pathdelim + 'atomic' + Pathdelim + 'walk.png', tmFirstPixel); // Fertig, getestet
  // data/maps/Field**
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES0.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field00' + Pathdelim + 'brick.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES0.ANI', 40, 36, 1, taLeftJustify, tlTop, '2', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field00' + Pathdelim + 'solid.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES1.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field01' + Pathdelim + 'brick.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES1.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field01' + Pathdelim + 'solid.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES2.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field02' + Pathdelim + 'brick.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES2.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field02' + Pathdelim + 'solid.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES3.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field03' + Pathdelim + 'brick.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES3.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field03' + Pathdelim + 'solid.png', tmBlack); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES4.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field04' + Pathdelim + 'brick.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES4.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field04' + Pathdelim + 'solid.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES5.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field05' + Pathdelim + 'brick.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES5.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field05' + Pathdelim + 'solid.png', tmFirstPixelPerFrame); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES6.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field06' + Pathdelim + 'brick.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES6.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field06' + Pathdelim + 'solid.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES7.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field07' + Pathdelim + 'brick.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES7.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field07' + Pathdelim + 'solid.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES8.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field08' + Pathdelim + 'brick.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES8.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field08' + Pathdelim + 'solid.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES9.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field09' + Pathdelim + 'brick.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES9.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field09' + Pathdelim + 'solid.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES10.ANI', 40, 36, 1, taLeftJustify, tlTop, '1', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field10' + Pathdelim + 'brick.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TILES10.ANI', 40, 36, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field10' + Pathdelim + 'solid.png', tmFirstPixel); // Fertig, getestet
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK0.ANI', 40, 36, 3, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field00' + Pathdelim + 'xbrick.ani', tmFirstPixelPerFrame);
  PopAniJob(3, 3, 100, 0, [0, 9]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK1.ANI', 40, 36, 3, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field01' + Pathdelim + 'xbrick.ani', tmFirstPixelPerFrame);
  PopAniJob(3, 3, 100, 0, [0, 9]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK2.ANI', 40, 36, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field02' + Pathdelim + 'xbrick.ani', tmFirstPixel);
  PopAniJob(2, 4, 100, 0, [0, 8]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK3.ANI', 40, 36, 3, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field03' + Pathdelim + 'xbrick.ani', tmFirstPixelPerFrame);
  PopAniJob(3, 3, 100, 0, [0, 9]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK4.ANI', 40, 36, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field04' + Pathdelim + 'xbrick.ani', tmFirstPixel);
  PopAniJob(2, 5, 100, 0, [0, 10]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK5.ANI', 40, 36, 3, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field05' + Pathdelim + 'xbrick.ani', tmFirstPixelPerFrame);
  PopAniJob(3, 3, 100, 0, [0, 9]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK6.ANI', 40, 36, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field06' + Pathdelim + 'xbrick.ani', tmFirstPixelPerFrame);
  PopAniJob(2, 5, 100, 0, [0, 10]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK7.ANI', 40, 36, 3, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field07' + Pathdelim + 'xbrick.ani', tmFirstPixel);
  PopAniJob(3, 3, 100, 0, [0, 9]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK8.ANI', 40, 36, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field08' + Pathdelim + 'xbrick.ani', tmFirstPixel);
  PopAniJob(2, 5, 100, 0, [0, 10]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK9.ANI', 40, 36, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field09' + Pathdelim + 'xbrick.ani', tmFirstPixel);
  PopAniJob(2, 5, 100, 0, [0, 10]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'XBRICK10.ANI', 40, 36, 2, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9', 'data' + Pathdelim + 'maps' + Pathdelim + 'Field10' + Pathdelim + 'xbrick.ani', tmFirstPixel);
  PopAniJob(2, 5, 100, 0, [0, 10]);
  // data/res/*
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'KFONT.ANI', 18, 27, 4, taCenter, tlCenter, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10', 'data' + Pathdelim + 'res' + Pathdelim + 'bigfont.png', tmBlack);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'HURRY.ANI', 278, 91, 1, taLeftJustify, tlTop, '0', 'data' + Pathdelim + 'res' + Pathdelim + 'hurry.png', tmBlack);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'KFONT.ANI', 48, 17, 1, taCenter, tlCenter, '11', 'data' + Pathdelim + 'res' + Pathdelim + 'infinity.png', tmBlack);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'MISC.ANI', 32, 32, 1, taLeftJustify, tlTop, '11', 'data' + Pathdelim + 'res' + Pathdelim + 'options_cursor.png', tmFirstPixel);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'MISC.ANI', 77, 20, 1, taLeftJustify, tlTop, '10', 'data' + Pathdelim + 'res' + Pathdelim + 'playerdead.png', tmBlack);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'EXTRAS.ANI', 21, 21, 4, taLeftJustify, tlTop, '1, 0, 3, 2', 'data' + Pathdelim + 'res' + Pathdelim + 'arrows.ani', tmBlack);
  PopAniJob(4, 1, 100, 45.0, [0, 1, 1, 1, 2, 1, 3, 1]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'CONVEYOR.ANI', 40, 36, 5, taLeftJustify, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17', 'data' + Pathdelim + 'res' + Pathdelim + 'conveyor.ani', tmFirstPixelPerFrame);
  PopAniJob(5, 4, 100, 45.0, [5, 5, 10, 4, 0, 5, 14, 4]);
  AddAniJob('DATA' + Pathdelim + 'ANI' + Pathdelim + 'TRIGANIM.ANI', 40, 40, 5, taCenter, tlTop, '0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1', 'data' + Pathdelim + 'res' + Pathdelim + 'mainmenu_cursor.ani', tmFirstPixel);
  PopAniJob(5, 4, 40, 0.0, [0, 18]);

End.

