Unit usounds;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Const
  BombDrops: Array[0..2] Of String = ('bmdrop1.wav', 'bmdrop2.wav', 'bmdrop3.wav');
  BombKick: Array[0..3] Of String = ('kbomb1.wav', 'kbomb2.wav', 'kicker3.wav', 'kicker10.wav');
  BombStop: Array[0..2] Of String = ('bombstop.wav', 'bmbstop1.wav', 'bmbstop2.wav');
  BombJelly: Array[0..2] Of String = ('bombboun.wav', '1017.wav', '1036.wav');
  BombBounce: Array[0..0] Of String = ('bmdrop3.wav');
  BombPunch: Array[0..1] Of String = ('kbomb1.wav', 'kbomb2.wav');
  BombGrab: Array[0..5] Of String = (
    'grab1.wav', 'grab2.wav', 'bmbthrw1.wav', 'bmbthrw3.wav', 'bmbthrw4.wav',
    'bmbthrw5.wav');
  BombExplode: Array[0..19] Of String = (
    'expl6.wav', 'explo1.wav', 'explode2.wav', 'explode3.wav', 'explode4.wav',
    'bomb_01.wav', 'bomb_02.wav', 'bomb_04.wav', 'bomb_04b.wav', 'bomb_05.wav',
    'bomb_06.wav', 'bomb_07.wav', 'bomb_07b.wav', 'bomb_09.wav', 'bomb_11.wav',
    'bomb_12.wav', 'bomb_12b.wav', 'bomb_17.wav', 'bomb_19.wav', 'bomb_24.wav');
  AtomicDie: Array[0..1] Of String = ('scream1.wav', 'die1.wav');
  Winner: Array[0..3] Of String = ('proud.wav', 'theman.wav', 'youwin1.wav', '1000.wav');
  GetGoodPowerUp: Array[0..5] Of String = ('woohoo1.wav', 'get1.wav', 'get2.wav', 'coolpop.wav', 'allrite.wav', 'schwing.wav');
  GetBadPowerUp: Array[0..3] Of String = ('ohno1.wav', 'disease1.wav', 'disease2.wav', 'disease3.wav');
  AtomicZen: Array[0..1] Of String = ('zen1.wav', 'zen2.wav');
  OtherPlayerDie: Array[0..14] Of String = (
    'cribrown.wav', 'cul8r.wav', 'gotahurt.wav', 'gotcha.wav', 'later.wav',
    'roasted.wav', 'toeasy.wav', 'youblow.wav', 'eatdust.wav', 'smelsmok.wav',
    'stupidio.wav', 'suckitdn.wav', 'tastpain.wav', 'tastpai2.wav', 'cribrown.wav');
  HurryBrick: Array[0..6] Of String = (
    'clikplat.wav', 'sqrdrop2.wav', 'sqrdrop4.wav', 'sqrdrop5.wav', 'sqrdrop6.wav',
    'sqrdrop7.wav', 'sqrdrop8.wav');
  Hurry: Array[0..1] Of String = ('hurry.wav', 'hurytuf.wav');


Function SelectRandomSound(Sounds: Array Of String): String;

Implementation

Function SelectRandomSound(Sounds: Array Of String): String;
Begin
  result := Sounds[Random(Length(Sounds))];
End;

End.

