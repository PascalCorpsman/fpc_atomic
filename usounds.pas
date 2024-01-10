Unit usounds;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, bass;

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
  AtomicJump: Array[0..3] Of String = ('1017.wav', '1036.wav', '1045.wav', 'trampo.wav');
  AtomicZen: Array[0..1] Of String = ('zen1.wav', 'zen2.wav');
  AtomicWrapHole: Array[0..2] Of String = ('transin.wav', 'transout.wav', 'warp1.wav');

  OtherPlayerDie: Array[0..14] Of String = (
    'cribrown.wav', 'cul8r.wav', 'gotahurt.wav', 'gotcha.wav', 'later.wav',
    'roasted.wav', 'toeasy.wav', 'youblow.wav', 'eatdust.wav', 'smelsmok.wav',
    'stupidio.wav', 'suckitdn.wav', 'tastpain.wav', 'tastpai2.wav', 'cribrown.wav');

  GetGoodPowerUp: Array[0..5] Of String = ('woohoo1.wav', 'get1.wav', 'get2.wav', 'coolpop.wav', 'allrite.wav', 'schwing.wav');
  GetBadPowerUp: Array[0..3] Of String = ('ohno1.wav', 'disease1.wav', 'disease2.wav', 'disease3.wav');

  HurryBrick: Array[0..6] Of String = (
    'clikplat.wav', 'sqrdrop2.wav', 'sqrdrop4.wav', 'sqrdrop5.wav', 'sqrdrop6.wav',
    'sqrdrop7.wav', 'sqrdrop8.wav');
  Hurry: Array[0..1] Of String = ('hurry.wav', 'hurytuf.wav');
  Winner: Array[0..3] Of String = ('proud.wav', 'theman.wav', 'youwin1.wav', '1000.wav');

Type

  TSoundEffektHandles = Record
    Filename: String;
    BassStream: HStream;
    EndCallback: TNotifyEvent;
  End;

  (*
   * Small improvised Sound Engine, that can play 1 song and multiple Sound effects (with or withoud finished Callback)
   *
   * This engine loads each sound only one time into bass and then stores the corresponding handles
   *  => reduced loadings / free's hopefully preventing Linux to crash from time to time due to havy sound plays
   *)

  { TSoundManager }

  TSoundManager = Class
  private
    fSounds: Array Of TSoundEffektHandles;
    fBassSong: DWORD; // One Song playes over a long period
    Procedure StopPlayingSong;
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;
    Procedure PlaySound(Const Filename: String);
    Function PlaySoundEffekt(Const Filename: String; EndCallback: TNotifyEvent = Nil): Boolean;
    Function IncVolume(): Dword;
    Function DecVolume(): Dword;
    Function SetVolumeValue(Value: DWord): Boolean;
  End;

Function SelectRandomSound(Sounds: Array Of String): String;

Implementation

Uses math, uatomic_common
  , ugame // Nicht ideal :( wird für die Settings benötigt.
  ;

Type
  TEndFileInfo = Record
    SelfPtr: TSoundManager;
    BassStream: HStream;
  End;
  PEndFileInfo = ^TEndFileInfo;

Function SelectRandomSound(Sounds: Array Of String): String;
Begin
  result := Sounds[Random(Length(Sounds))];
End;

(*
 * Tutorial von https://www.gausi.de/memp.html, Seite 16
 *)

Procedure EndFileProc(handle: HSYNC; channel, data: DWORD; user: Pointer){$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF};
Var
  udata: PEndFileInfo;
  sm: TSoundManager;
  i: Integer;
Begin
  udata := user;
  sm := udata^.SelfPtr;
  For i := 0 To high(sm.fSounds) Do Begin
    If sm.fSounds[i].BassStream = udata^.BassStream Then Begin
      If Not assigned(sm.fSounds[i].EndCallback) Then Begin
        LogShow('Error could call callback for end sound', llFatal);
      End;
      sm.fSounds[i].EndCallback(sm);
      break;
    End;
  End;
  Dispose(udata);
End;

{ TSoundManager }

Procedure TSoundManager.StopPlayingSong;
Begin
  If fBassSong <> 0 Then Begin
    If Not BASS_ChannelStop(fBassSong) Then Begin
      LogShow('Error could not Stop player, Error code :' + inttostr(BASS_ErrorGetCode), llCritical);
    End;
    If Not BASS_StreamFree(fBassSong) Then Begin
      LogShow('Error could not Free the Stream, Error code :' + inttostr(BASS_ErrorGetCode), llCritical);
    End;
  End;
  fBassSong := 0;
End;

Constructor TSoundManager.Create;
Begin
  fSounds := Nil;
  fBassSong := 0;
End;

Destructor TSoundManager.Destroy;
Var
  i: Integer;
Begin
  // Stop everything
  StopPlayingSong();
  For i := 0 To high(fSounds) Do Begin
    BASS_ChannelStop(fSounds[i].BassStream);
    BASS_StreamFree(fSounds[i].BassStream);
  End;
  setlength(fSounds, 0);
  Inherited Destroy;
End;

Procedure TSoundManager.PlaySound(Const Filename: String);
Var
  Info: BASS_CHANNELINFO;
  s: String;
Begin
  // Alle Bass Channels Stoppen
  If fBassSong <> 0 Then Begin
    Info.filename := ''; // Beruhigt den Compiler
    // Wir sollen den Gerade Spielenden Song noch mal Spielen, da der aber schon läuft -> Raus
    If game.Settings.PlaySounds And BASS_ChannelGetInfo(fBassSong, Info) Then Begin
      s := Info.filename;
      If s = Filename Then exit;
    End;
    StopPlayingSong;
  End;
  // ggf. neu starten des Songs
  If game.Settings.PlaySounds And (FileExists(Filename)) And (Filename <> '') Then Begin
    // Start des Liedes in Endlosschleife
    fBassSong := BASS_StreamCreateFile(false, Pchar(filename), 0, 0, BASS_MUSIC_LOOP);
    If fBassSong = 0 Then Begin
      LogShow('Error unable to load :' + LineEnding + filename + LineEnding + 'Error code :' + inttostr(BASS_ErrorGetCode), llCritical);
      exit;
    End;
    If Not BASS_ChannelPlay(fBassSong, true) Then Begin
      LogShow('Error could not play in channel, Error code :' + inttostr(BASS_ErrorGetCode), llCritical);
    End;
  End;
End;

Function TSoundManager.PlaySoundEffekt(Const Filename: String;
  EndCallback: TNotifyEvent): Boolean;
Var
  Song: HSTREAM;
  i: Integer;
  p: PEndFileInfo;
Begin
  result := false;
  If Not FileExists(Filename) Then Begin
    log('Error could not find soundeffect:' + Filename, llWarning);
    exit;
  End;
  For i := 0 To high(fSounds) Do Begin
    If fSounds[i].Filename = Filename Then Begin
      // Wir kennen den Sound schon, dann spielen wir ihn einfach ab und gut
      If Not BASS_ChannelPlay(fSounds[i].BassStream, true) Then Begin
        LogShow('Error could not play in channel, Error code :' + inttostr(BASS_ErrorGetCode), llCritical);
        exit;
      End;
      result := true;
      exit;
    End;
  End;
  // Wir kennen den Sound noch nicht -> Aufnehmen und starten
  song := BASS_StreamCreateFile(false, Pchar(filename), 0, 0, 0);
  If Song = 0 Then Begin
    LogShow('Error unable to load :' + LineEnding + filename + LineEnding + 'Error code :' + inttostr(BASS_ErrorGetCode), llCritical);
    exit;
  End;
  setlength(fSounds, high(fSounds) + 2);
  fSounds[high(fSounds)].Filename := Filename;
  fSounds[high(fSounds)].BassStream := Song;
  fSounds[high(fSounds)].EndCallback := EndCallback;
  If Assigned(EndCallback) Then Begin
    new(p);
    p^.SelfPtr := Self;
    p^.BassStream := Song;
    Bass_ChannelSetSync(Song, BASS_SYNC_END, 0, @EndFileProc, p);
  End;
  If Not BASS_ChannelPlay(Song, true) Then Begin
    LogShow('Error could not play in channel, Error code :' + inttostr(BASS_ErrorGetCode), llCritical);
    exit;
  End;
  result := true;
End;

Function TSoundManager.IncVolume: Dword;
Begin
  Result := BASS_GetConfig(BASS_CONFIG_GVOL_STREAM);
  Result := min(10000, Result + 500);
  If Not BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Result) Then Begin
    log('Could not adjust sound volume.', llError);
  End;
End;

Function TSoundManager.DecVolume: Dword;
Begin
  Result := BASS_GetConfig(BASS_CONFIG_GVOL_STREAM);
  Result := max(0, Result - 500);
  If Not BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Result) Then Begin
    log('Could not adjust sound volume.', llError);
  End;
End;

Function TSoundManager.SetVolumeValue(Value: DWord): Boolean;
Begin
  result := true;
  If Not BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Value) Then Begin
    result := false;
  End;
End;

End.

