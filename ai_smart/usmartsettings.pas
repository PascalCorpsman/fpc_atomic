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
 * Settings of the smart ai (ai_smart.ini). Every ability has a Chance 0..100:
 * 0 = never, 100 = always, otherwise rolled per opportunity (see TSmartAi.Roll).
 *
 * [General] Difficulty (0..100) sets the chances, the reaction time, the
 * mistakes and the remote trigger time. A key that is set in the file (not
 * commented out) overrides the value of the difficulty.
 *
 * The file is searched next to the server (= next to the lib, for tests) and
 * then in the config folder of the game (%LOCALAPPDATA%\fpc_atomic,
 * ~/.config/fpc_atomic). If there is none, the defaults are written to the
 * config folder, with a comment for every key.
 *)
Unit usmartsettings;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uai_types;

Type
  TSmartFeature = (
    sfAttack, sfPowerups, sfApproach, sfTimedDanger, sfTrigger,
    sfKick, sfPunch, sfGrab, sfSpooger, sfTraps,
    sfTeamSafety, sfOpenSpace, sfInfect, sfHuntedFlees,
    sfAnticipate, sfChainAttack, sfConveyors, sfRoundEnd, sfKickSnipe,
    sfStun, sfBrickWalk, sfBorders, sfFlameBeam, sfInvuln, sfWhiteBomb, sfStartGlove, sfNoCover);

  TSmartSettings = Record
    Difficulty: integer; // 0 = easy .. 100 = strong
    Seed: QWord; // 0 = random
    LogDecisions: Boolean;
    RerollMs: integer; // a Chance is rolled again after this time
    Chance: Array[TSmartFeature] Of integer;
    SafetyMs: integer; // TimedDanger
    MinReactionMs, MaxWaitMs: integer; // Trigger
    AnticipateCost: integer; // Anticipate
    HurryLookAheadMs: integer; // RoundEnd
    SnipeHitChance, SnipeWaitMs, SnipeEndgameBricks: integer; // KickSnipe
    BeamReactionMs, BeamSlowPercent, BeamFreezeMs: integer; // FlameBeam
    DelayMs, MistakeChance: integer; // Reaction
    PowerupWeight: Array[TAiField] Of integer; // Powerups, only the good ones are used
  End;

Const
  FeatureSection: Array[TSmartFeature] Of String = (
    'Attack', 'Powerups', 'Approach', 'TimedDanger', 'Trigger',
    'Kick', 'Punch', 'Grab', 'Spooger', 'Traps',
    'TeamSafety', 'OpenSpace', 'Infect', 'HuntedFlees',
    'Anticipate', 'ChainAttack', 'Conveyors', 'RoundEnd', 'KickSnipe',
    'Stun', 'BrickWalk', 'Borders', 'FlameBeam', 'Invuln', 'WhiteBomb', 'StartGlove', 'NoCover');

  SettingsFileName = 'ai_smart.ini';
  DefaultDifficulty = 50;

Var
  Settings: TSmartSettings;
  SettingsFile: String = ''; // the file that was read ('' = defaults only)

Procedure DefaultSettings(Out s: TSmartSettings; Difficulty: integer = DefaultDifficulty);
Procedure LoadSettings(); // reads ai_smart.ini, writes the defaults if there is none
Function SmartIniFile(): String; // the ini LoadSettings uses: next to the program if it is there, else in the config folder
Procedure WriteDefaults(Const Filename: String; Difficulty: integer = DefaultDifficulty); // the ini with every key and comment (the launcher uses it too)
Procedure SmartLog(Const Msg: String); // only if LogDecisions, into ai_smart.log next to the ini

Implementation

Uses
  IniFiles;

Const
  PowerupKeys: Array[0..9] Of String = ('ExtraBomb', 'LongerFlame', 'GoldFlame', 'Speed', 'Kick', 'Spooger', 'Punch', 'Grab', 'Trigger', 'Jelly');
  PowerupFields: Array[0..9] Of TAiField = (fExtraBomb, fLongerFlame, fGoldflame, fExtraSpeed, fKick, fSpooger, fPunch, fGrab, fTrigger, fJelly);
  PowerupDefaults: Array[0..9] Of integer = (5, 5, 6, 3, 4, 2, 2, 2, 3, 1);

  (*
   * The chance of each ability at difficulty 0 and at 100, linear in between. Surviving (timed danger map,
   * conveyors, hurry) and not hitting team mates stay on at every difficulty: easier, but not stupid.
   *)
  FeatureEasy: Array[TSmartFeature] Of integer = (
    30, 40, 30, 100, 100,
    0, 0, 0, 0, 0,
    100, 50, 0, 30,
    0, 0, 100, 100, 0,
    0, 30, 50,
    100, 30, 100, 0, 100);
  FeatureStrong: Array[TSmartFeature] Of integer = (
    100, 100, 100, 100, 100,
    90, 70, 70, 70, 100,
    100, 100, 70, 100,
    100, 100, 100, 100, 80,
    100, 100, 100,
    100, 100, 100, 100, 0);

  FeatureComments: Array[TSmartFeature] Of String = (
    'Attack enemies with bombs in every game mode (the old ai only attacks in hunter mode).',
    'Walk to good powerups on purpose and avoid burning them with own bombs (weights in [PowerupWeights]).',
    'Without any other goal walk towards the nearest enemy and keep a distance of about 3 fields.',
    'Danger map with the explosion time of every bomb: walk through fields that explode later, check that the escape is fast enough. 0 = every bomb is dangerous at once (old behaviour).',
    'Remote bombs: trigger only if an enemy is hit and the bomb lies for at least MinReactionMs (a human has a chance to react), at the latest after MaxWaitMs. 0 = only after MaxWaitMs.',
    'Kick bombs (needs the kick item): kick a bomb away to escape, or towards an enemy in the same line.',
    'Punch bombs (needs the punch item): punch an adjacent bomb towards an enemy (it flies 3 fields).',
    'Grab and throw (needs the grab item): place a bomb and throw it 3 fields onto an enemy.',
    'Spooger (needs the spooger item): place a row of bombs towards an enemy in the same line.',
    'Prefer attack fields after which the enemy has fewer free neighbour fields.',
    'Never place, kick or trigger a bomb that hits a team mate (teamplay, hunters among each other), below 100 sometimes not.',
    'If two goals are about equal, prefer the field with more free neighbour fields (fewer dead ends).',
    'When the ai is ill, walk to the nearest enemy to pass the disease on.',
    'Hunter mode: the hunted keeps away from the hunters while it has at most one bomb.',
    'Fields in the flame line of an enemy who still has bombs are less attractive as a goal (Cost).',
    'Also count enemies that are hit through a chain reaction (the new bomb ignites another one).',
    'Do not step onto a conveyor that pushes into a dangerous field.',
    'Hurry: the fields that are walled up within HurryLookAheadMs count as dangerous.',
    'Sniping (needs kick and remote bombs): place a remote bomb, kick it into a free line towards an enemy and trigger it when the enemy is in its flames, also several bombs.',
    'Boxing glove mod (needs the punch item): stun an enemy right in front of the ai.',
    'Walk through bricks mod: walk over bricks while the time lasts (also to escape), never stay in one when it ends.',
    'Open borders and teleporters mod: walk out over an open border (comes in on the other side), throw bombs over it; teleporters are avoided like holes.',
    'Flamethrower mod: avoid the beam of a firing player like flames, and the line in front of a player with fuel (Anticipate).',
    'Invulnerability (mods): do not attack enemies that can not be hit, and walk through flames while the ai itself can not be hit.',
    'White bomb mod: do not flee from a white bomb (no flames), and place an own white bomb at once (it only turns the screens of the enemies white).',
    'Glove at start mod: throw a bomb at the start into bricks or at an enemy, before the first step.',
    'Flamethrower mod: an idle ai behind a bomb that shields it from an enemy with a flamethrower steps out of the cover (the easier, the more often).');

Function Clamp(Value, aMin, aMax: integer): integer;
Begin
  result := Value;
  If result < aMin Then result := aMin;
  If result > aMax Then result := aMax;
End;

Function Lerp(Easy, Strong, Difficulty: integer): integer;
Begin
  result := Easy + (Strong - Easy) * Difficulty Div 100;
End;

Procedure DefaultSettings(Out s: TSmartSettings; Difficulty: integer);
Var
  f: TSmartFeature;
  i: integer;
  a: TAiField;
Begin
  Difficulty := Clamp(Difficulty, 0, 100);
  s.Difficulty := Difficulty;
  s.Seed := 0;
  s.LogDecisions := false;
  s.RerollMs := 3000;
  For f := low(TSmartFeature) To high(TSmartFeature) Do Begin
    s.Chance[f] := Lerp(FeatureEasy[f], FeatureStrong[f], Difficulty);
  End;
  s.SafetyMs := 250;
  s.MinReactionMs := Lerp(1000, 300, Difficulty);
  s.MaxWaitMs := 2000;
  s.AnticipateCost := 3;
  s.HurryLookAheadMs := 3000;
  s.SnipeHitChance := Lerp(30, 90, Difficulty);
  s.SnipeWaitMs := 8000;
  s.SnipeEndgameBricks := 10;
  s.BeamReactionMs := Lerp(2200, 600, Difficulty); // the beam kills after FlameThrowerKillMs (default 1500) in it, counted from the hit
  s.BeamSlowPercent := Lerp(50, 0, Difficulty);
  s.BeamFreezeMs := Lerp(1500, 0, Difficulty);
  s.DelayMs := Lerp(250, 0, Difficulty);
  s.MistakeChance := Lerp(10, 0, Difficulty);
  For a := low(TAiField) To high(TAiField) Do Begin
    s.PowerupWeight[a] := 0;
  End;
  For i := 0 To high(PowerupKeys) Do Begin
    s.PowerupWeight[PowerupFields[i]] := PowerupDefaults[i];
  End;
End;

Function ConfigDir(): String;
Begin
  // Same folder as the game and the server use (uatomic_global, application name fpc_atomic)
  result := IncludeTrailingPathDelimiter(ExtractFileDir(ExcludeTrailingPathDelimiter(GetAppConfigDir(false)))) + 'fpc_atomic' + PathDelim;
End;

Function SmartIniFile(): String;
Begin
  // A file next to the server (and the lib, the launcher) wins, for tests with other settings
  result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + SettingsFileName;
  If Not FileExists(result) Then result := ConfigDir() + SettingsFileName;
End;

Procedure WriteDefaults(Const Filename: String; Difficulty: integer);
Const
  ByDifficulty = ' (set by Difficulty, remove the ; to fix it)';
Var
  sl: TStringList;
  d: TSmartSettings;
  f: TSmartFeature;
  i: integer;
Begin
  DefaultSettings(d, Difficulty);
  sl := TStringList.Create;
  Try
    sl.Add('; Settings of the smart ai (ai_smart.dll / libai_smart.so), see ai_smart/README.md');
    sl.Add('; Every ability has a Chance from 0 to 100: 0 = never, 100 = always, otherwise rolled');
    sl.Add('; per opportunity and kept for RerollMs. Changes take effect with the next round.');
    sl.Add('; Difficulty sets the values that are commented out here, a value without ; overrides it.');
    sl.Add('');
    sl.Add('[General]');
    sl.Add('; 0 = easy, 50 = normal, 100 = strong');
    sl.Add('Difficulty=' + IntToStr(d.Difficulty));
    sl.Add('; 0 = random, any other number = the same decisions in the same situations (for tests)');
    sl.Add('Seed=' + IntToStr(d.Seed));
    sl.Add('; 1 = write every new goal into ai_smart.log next to this file');
    sl.Add('LogDecisions=0');
    sl.Add('; Time in ms after which a Chance is rolled again');
    sl.Add('RerollMs=' + IntToStr(d.RerollMs));
    For f := low(TSmartFeature) To high(TSmartFeature) Do Begin
      sl.Add('');
      sl.Add('[' + FeatureSection[f] + ']');
      If FeatureEasy[f] = FeatureStrong[f] Then
        sl.Add('; ' + FeatureComments[f] + ' The same at every difficulty, remove the ; to change it.')
      Else
        sl.Add('; ' + FeatureComments[f] + ByDifficulty);
      sl.Add(';Chance=' + IntToStr(d.Chance[f]));
      Case f Of
        sfTimedDanger: Begin
            sl.Add('; Extra time in ms the ai keeps between itself and an explosion');
            sl.Add('SafetyMs=' + IntToStr(d.SafetyMs));
          End;
        sfTrigger: Begin
            sl.Add('; A remote bomb lies at least this long (ms) before it is triggered at an enemy. Smaller = faster and less fair' + ByDifficulty);
            sl.Add(';MinReactionMs=' + IntToStr(d.MinReactionMs));
            sl.Add('; A remote bomb is triggered after this time (ms) at the latest, also without an enemy');
            sl.Add('MaxWaitMs=' + IntToStr(d.MaxWaitMs));
          End;
        sfAnticipate: Begin
            sl.Add('; How much less attractive such a field is (0..10)');
            sl.Add('Cost=' + IntToStr(d.AnticipateCost));
          End;
        sfRoundEnd: Begin
            sl.Add('; How far the ai looks ahead (ms) for fields that the hurry walls up');
            sl.Add('HurryLookAheadMs=' + IntToStr(d.HurryLookAheadMs));
          End;
        sfFlameBeam: Begin
            sl.Add('; The ai notices a beam only after it fired this long (ms), like a human who needs to react' + ByDifficulty);
            sl.Add(';ReactionMs=' + IntToStr(d.BeamReactionMs));
            sl.Add('; In a beam the ai walks slower by this many percent, like a player who is burning' + ByDifficulty);
            sl.Add(';SlowPercent=' + IntToStr(d.BeamSlowPercent));
            sl.Add('; Hit by a beam the ai stands still this long (ms) first, it is startled (not when a bomb is about to explode)' + ByDifficulty);
            sl.Add(';FreezeMs=' + IntToStr(d.BeamFreezeMs));
          End;
        sfKickSnipe: Begin
            sl.Add('; Chance in % to trigger at the right moment, otherwise 300..800 ms too late (the enemy may be gone)' + ByDifficulty);
            sl.Add(';HitChance=' + IntToStr(d.SnipeHitChance));
            sl.Add('; How long (ms) the ai waits with its remote bombs for an enemy (the game turns them into normal bombs after 15 s)');
            sl.Add('WaitMs=' + IntToStr(d.SnipeWaitMs));
            sl.Add('; Endgame: with at most this many bricks left the ai always snipes when it can (unless Chance=0)');
            sl.Add('EndgameBricks=' + IntToStr(d.SnipeEndgameBricks));
          End;
      End;
    End;
    sl.Add('');
    sl.Add('[Reaction]');
    sl.Add('; The ai decides only every DelayMs (0 = every frame, 10 ms). Larger = slower, more human' + ByDifficulty);
    sl.Add(';DelayMs=' + IntToStr(d.DelayMs));
    sl.Add('; Chance in % that a decision is replaced by a random step (never into flames or a dangerous field)' + ByDifficulty);
    sl.Add(';MistakeChance=' + IntToStr(d.MistakeChance));
    sl.Add('');
    sl.Add('[PowerupWeights]');
    sl.Add('; How much the ai wants each good powerup (0 = ignore, 10 = very much), used with [Powerups]');
    For i := 0 To high(PowerupKeys) Do Begin
      sl.Add(PowerupKeys[i] + '=' + IntToStr(PowerupDefaults[i]));
    End;
    ForceDirectories(ExtractFilePath(Filename));
    sl.SaveToFile(Filename);
  Finally
    sl.Free;
  End;
End;

Procedure LoadSettings();
Var
  ini: TIniFile;
  Candidate: String;
  f: TSmartFeature;
  i: integer;
  Old: Boolean;

  // A key in the file overrides the value of the difficulty
  Procedure Read(Const Section, Key: String; Var Value: integer; aMin, aMax: integer);
  Begin
    If ini.ValueExists(Section, Key) Then Value := Clamp(ini.ReadInteger(Section, Key, Value), aMin, aMax);
  End;

Begin
  DefaultSettings(Settings);
  SettingsFile := '';
  Candidate := SmartIniFile();
  If Candidate = ConfigDir() + SettingsFileName Then Begin
    // A file of version 0.01 has every value set, Difficulty would have no effect: keep it as .bak, write a new one
    Old := false;
    If FileExists(Candidate) Then Begin
      ini := TIniFile.Create(Candidate);
      Try
        Old := Not ini.ValueExists('General', 'Difficulty');
      Finally
        ini.Free;
      End;
      If Old Then Begin
        DeleteFile(Candidate + '.bak');
        RenameFile(Candidate, Candidate + '.bak');
      End;
    End;
    If Not FileExists(Candidate) Then Begin
      Try
        WriteDefaults(Candidate);
      Except
        exit; // no config folder, the defaults are used
      End;
    End;
  End;
  If Not FileExists(Candidate) Then exit;
  SettingsFile := Candidate;
  ini := TIniFile.Create(Candidate);
  Try
    DefaultSettings(Settings, ini.ReadInteger('General', 'Difficulty', DefaultDifficulty));
    Settings.Seed := StrToQWordDef(ini.ReadString('General', 'Seed', '0'), 0);
    Settings.LogDecisions := ini.ReadInteger('General', 'LogDecisions', 0) <> 0;
    Read('General', 'RerollMs', Settings.RerollMs, 100, 60000);
    For f := low(TSmartFeature) To high(TSmartFeature) Do Begin
      Read(FeatureSection[f], 'Chance', Settings.Chance[f], 0, 100);
    End;
    Read('TimedDanger', 'SafetyMs', Settings.SafetyMs, 0, 2000);
    Read('Trigger', 'MinReactionMs', Settings.MinReactionMs, 0, 10000);
    Read('Trigger', 'MaxWaitMs', Settings.MaxWaitMs, Settings.MinReactionMs, 60000);
    Read('Anticipate', 'Cost', Settings.AnticipateCost, 0, 10);
    Read('RoundEnd', 'HurryLookAheadMs', Settings.HurryLookAheadMs, 0, 20000);
    Read('KickSnipe', 'HitChance', Settings.SnipeHitChance, 0, 100);
    Read('KickSnipe', 'WaitMs', Settings.SnipeWaitMs, 0, 14000);
    Read('KickSnipe', 'EndgameBricks', Settings.SnipeEndgameBricks, 0, 165);
    Read('FlameBeam', 'ReactionMs', Settings.BeamReactionMs, 0, 5000);
    Read('FlameBeam', 'SlowPercent', Settings.BeamSlowPercent, 0, 90);
    Read('FlameBeam', 'FreezeMs', Settings.BeamFreezeMs, 0, 5000);
    Read('Reaction', 'DelayMs', Settings.DelayMs, 0, 2000);
    Read('Reaction', 'MistakeChance', Settings.MistakeChance, 0, 100);
    For i := 0 To high(PowerupKeys) Do Begin
      Read('PowerupWeights', PowerupKeys[i], Settings.PowerupWeight[PowerupFields[i]], 0, 10);
    End;
  Finally
    ini.Free;
  End;
End;

Procedure SmartLog(Const Msg: String);
Var
  f: TextFile;
  fn: String;
Begin
  If (Not Settings.LogDecisions) Or (SettingsFile = '') Then exit;
  fn := ExtractFilePath(SettingsFile) + 'ai_smart.log';
  Try
    AssignFile(f, fn);
    If FileExists(fn) Then Append(f) Else Rewrite(f);
    Try
      WriteLn(f, FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + Msg);
    Finally
      CloseFile(f);
    End;
  Except
    // logging must never disturb the game
  End;
End;

End.
