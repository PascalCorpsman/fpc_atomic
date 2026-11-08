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
Unit uatomic_statistics;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uatomic_common;

Type

  (*
   * Alles worüber der Server so Statistiken führt ;)
   *)
  TStatSelector = (
    ssMatchesStarted
    , ssGamesStarted
    , ssFramesRendered
    , ssBombsDropped
    , ssPowerupsCollected
    , ssPlayerDeaths
    , ssBricksDestroyed
    , ssPowerupDestroyed
    , ssTotalNetworkBytesIn // UDP-Daten werden Ignoriert
    , ssTotalNetworkBytesOut // UDP-Daten werden Ignoriert
    , ssTotalNetworkPacketsIn // UDP-Daten werden Ignoriert
    , ssTotalNetworkPacketsOut // UDP-Daten werden Ignoriert
    );

  TStatisticCallback = Procedure(StatSelector: TStatSelector; Value: uint64 = 1) Of Object;

  TGameStatistik = Record
    Total: Array[TStatSelector] Of UInt64;
    LastRun: Array[TStatSelector] Of UInt64;
  End;

  TPlayerStatSelector = (
    pssKills // -- Fertig
    , pssFriendlyFireKills // -- Fertig -- dazu zählen auch Selbstkills
    , pssDeaths // -- Fertig
    , pssPlayedMatches // -- Fertig
    , pssWonMatches // -- Fertig
    , pssPlayedRounds // -- Fertig
    , pssWonRounds // -- Fertig
    , pssDrawRounds // -- Fertig
    , pssPlacedBombs // -- Fertig
    , pssTriggeredBombs // -- Fertig -- Mit dem BombenTrigger
    , pssThrownBombs // -- Fertig -- Mit dem Blauen Handschuh
    , pssPushedBombs // -- Fertig -- Mit dem Roten Handschuh
    , pssSpoogedBombs // -- Fertig -- Mit dem Spooger auf einmal gelegt
    , pssKickedBombs // -- Fertig -- Mit dem Kicker PowerUp
    , pssBricksDestroyed // -- Fertig
    , pssPowerupsCollected // -- Fertig
    , pssPowerupsDestroyed // -- Fertig
    , pssDiseased // -- Fertig -- Anzahl der Krankheiten die ein Spieler bekommen hat
    , pssDiseaseSpread // -- Fertig -- Anzahl der "Ansteckungen" die ein Spieler an andere übergeben hat
    , pssCarriedBombs // -- Fertig -- Mit dem Blauen Handschuh aufgehoben und getragen (Mod Bomben tragen)
    , pssLastPlayed // Beginn des letzten Matches, Unix Zeit (UTC) in s, wird gesetzt und nicht addiert
    , pssPlayTimeMs // Wie lange der Spieler in den Runden gelebt hat
    );

  TPlayerStatSelectorCounts = Array[TPlayerStatSelector] Of uint64;

  TPlayerStatistics = Record
    UserName: String;
    KeySet: TKeySet;
    IsAi: Boolean; // Im Kopf der Datei als ";ai" bzw. ";human" hinter dem KeySet, alte Dateien kennen das nicht
    stats: TPlayerStatSelectorCounts;
  End;

  TPlayerStatisticsArray = Array Of TPlayerStatistics;

  TPlayerStatisticCallback = Procedure(PlayerIndex: Integer; StatSelector: TPlayerStatSelector; Value: uint64 = 1) Of Object;

  { TPlayerStatisticEngine }

  TPlayerStatisticEngine = Class
  private
    fPlayerStatistics: TPlayerStatisticsArray;
    fIndexMapper: Array Of Integer; // Wandelt die Server Indexe um in die fPlayerStatistics Indexe
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    Procedure LoadPlayerStatistics(Const aFilename: String);
    Procedure SavePlayerStatistics(Const aFilename: String);
    Function AsText(): String; // Der Inhalt von Playerstats.csv (für den Snapshot an die Clients)

    Procedure ResetPlayerIDs; // Setzt ALle Aktuellen "Zuordungen zurück

    // Initialisiert alles Notwendige um via Index auf die Interne Struktur zugreifen zu können
    // Wird zu begin eines Matches aufgrufen
    // -> Match Counter ++
    Procedure InitPlayerID(
      Index: integer; // Index, den der Server für Alle Callbacks übergeben wird
      UserName: String; // Zur Identifizierung des Spielers
      KeySet: TKeySet; // Zur Identifizierung des Spielers
      IsAi: Boolean // Zur Identifizierung des Spielers, KI und Mensch mit gleichem Namen sind verschiedene Einträge
      );

    // Verändert den übergebenen StatSelector um Value
    Procedure UpdatePlayerID(
      Index: Integer; // Index, den der Server für Alle Callbacks übergeben wird
      StatSelector: TPlayerStatSelector;
      Value: uint64 = 1);
  End;

Const
  ServerStatisticsVersion = 1; // Das Format von miServerStatistics
  LastMatchFile = 'last_match.ini'; // In GetAtomicServerStatsDir: auf welchem Server das letzte Match zu Ende ging (für den Launcher)

(*
 * Das Format von stats.txt und Playerstats.csv, gemeinsam für Server, Client (Snapshot) und Launcher (Anzeige)
 *)
Function StatSelectorToString(aValue: TStatSelector): String; // Der Schlüssel in stats.txt, nicht übersetzen
Function PlayerStatSelectorToString(aValue: TPlayerStatSelector): String; // Der Schlüssel in der Datei, nicht übersetzen
Function PlayerStatisticsToText(Const Stats: TPlayerStatisticsArray): String;
Function PlayerStatisticsFromText(Const aText: String): TPlayerStatisticsArray;

Function IsLegacyAiName(Const aName: String): Boolean; // Ein KI Name aus einer Datei ohne ";ai" (Version der KI oder "Smart <Stufe>")
Function GetAtomicComputerName(): String; // Der Name des Rechners, zeigt der Launcher zur Kopie der Statistik an

Implementation

Uses DateUtils, uatomic_global
{$IFDEF Unix}
  , Unix
{$ENDIF}
  ;

Function StatSelectorToString(aValue: TStatSelector): String;
Begin
  result := '';
  Case aValue Of
    ssMatchesStarted: result := 'MatchesStarted';
    ssGamesStarted: result := 'GamesStarted';
    ssFramesRendered: result := 'FramesRendered';
    ssBombsDropped: result := 'BombsDropped';
    ssPowerupsCollected: result := 'PowerupsCollected';
    ssPlayerDeaths: result := 'PlayerDeaths';
    ssBricksDestroyed: result := 'BricksDestroyed';
    ssPowerupDestroyed: result := 'PowerUpDestroyed';
    ssTotalNetworkBytesIn: result := 'TotalNetworkBytesIn';
    ssTotalNetworkBytesOut: result := 'TotalNetworkBytesOut';
    ssTotalNetworkPacketsIn: result := 'TotalNetworkPacketsIn';
    ssTotalNetworkPacketsOut: result := 'TotalNetworkPacketsOut';
  Else Begin
      Raise exception.Create('Error: StatSelectorToString, missing implementation.');
    End;
  End;
End;

Function IsLegacyAiName(Const aName: String): Boolean;
Const
  // Die Stufen der Smart KI (usmartsettings.SmartPlayerName), "Smart Nitro"
  SmartLevels: Array[0..9] Of String = ('pipo', 'bombo', 'kiko', 'zappy', 'fuego', 'nitro', 'blazer', 'blaze', 'inferno', 'armageddon');
Var
  l: String;
  i: Integer;
Begin
  (*
   * Früher zählte eine KI unter ihrer Version ("Atomic ai ver. 0.08 by Corpsman", "Atomic Beam AI Version 0.00"),
   * später die Smart KI unter "Smart <Stufe>"
   *)
  l := lowercase(trim(aName));
  result := (pos(' ai ver', l) > 0) Or (pos(' ai version', l) > 0);
  If result Or (copy(l, 1, 6) <> 'smart ') Then exit;
  delete(l, 1, 6);
  For i := 0 To high(SmartLevels) Do Begin
    If l = SmartLevels[i] Then exit(true);
  End;
End;

Function GetAtomicComputerName: String;
Begin
{$IFDEF Windows}
  result := GetEnvironmentVariable('COMPUTERNAME');
{$ELSE}
  result := GetHostName();
{$ENDIF}
  result := trim(result);
End;

Function PlayerStatSelectorToString(aValue: TPlayerStatSelector): String;
Begin
  result := '';
  Case aValue Of
    pssKills: result := 'Kills';
    pssFriendlyFireKills: result := 'Friendly fire kills';
    pssDeaths: result := 'Deaths';
    pssPlayedMatches: result := 'Played Matches';
    pssWonMatches: result := 'Matches won';
    pssPlayedRounds: result := 'Played rounds';
    pssWonRounds: result := 'Rounds won';
    pssDrawRounds: result := 'Draw rounds';
    pssPlacedBombs: result := 'Placed bombs';
    pssTriggeredBombs: result := 'Triggered bombs';
    pssThrownBombs: result := 'Thrown bombs';
    pssPushedBombs: result := 'Pushed bombs';
    pssSpoogedBombs: result := 'Spooged bombs';
    pssKickedBombs: result := 'Kicked bombs';
    pssBricksDestroyed: result := 'Bricks destroyed';
    pssPowerupsCollected: result := 'Power-up collected';
    pssPowerupsDestroyed: result := 'Power-up destroyed';
    pssDiseased: result := 'Diseased';
    pssDiseaseSpread: result := 'Disease spread';
    pssCarriedBombs: result := 'Carried bombs';
    pssLastPlayed: result := 'Last played';
    pssPlayTimeMs: result := 'Play time ms';
  Else Begin
      Raise exception.Create('Error: PlayerStatSelectorToString, missing implementation.');
    End;
  End;
End;

Function PlayerStatisticsFromText(Const aText: String): TPlayerStatisticsArray;
Var
  sl: TStringList;
  tmp, players: TStringArray;
  i, k: Integer;
  j: TPlayerStatSelector;
  s: String;
Begin
  result := Nil;
  sl := TStringList.Create;
  Try
    sl.Text := aText;
    If (sl.Count = 0) Or (trim(sl[0]) = '') Then exit;
    players := trim(sl[0]).Split(#9);
    setlength(result, length(players));
    For i := 0 To high(players) Do Begin
      tmp := players[i].Split(';');
      If length(tmp) >= 2 Then Begin
        result[i].UserName := tmp[0];
        result[i].KeySet := StringToKeySet(tmp[1]);
        If length(tmp) >= 3 Then Begin
          result[i].IsAi := tmp[2] = 'ai';
        End
        Else Begin
          // Eine Datei von vor der Kennung ";ai": die KI am Namen erkennen
          result[i].IsAi := (result[i].KeySet = ks0) And IsLegacyAiName(result[i].UserName);
        End;
      End
      Else Begin
        result[i].UserName := 'Invalid';
        result[i].KeySet := ks0;
        result[i].IsAi := false;
        log(players[i] + ' invalid username / keyset', llError);
      End;
      // Reset alles zu 0
      FillChar(result[i].stats, sizeof(result[i].stats), 0);
    End;
    For i := 1 To sl.Count - 1 Do Begin
      tmp := trim(sl[i]).Split(#9);
      // Ja das ist Umständlich aber dafür "robust"
      If length(tmp) <> length(result) + 1 Then Begin
        log('Drop invalid line: ' + sl[i], llWarning);
        Continue;
      End;
      For j In TPlayerStatSelector Do Begin
        s := PlayerStatSelectorToString(j);
        If tmp[0] = s Then Begin
          For k := 0 To high(result) Do Begin
            result[k].stats[j] := StrToQWordDef(tmp[k + 1], 0);
          End;
          break;
        End;
      End;
    End;
  Finally
    sl.free;
  End;
End;

Function PlayerStatisticsToText(Const Stats: TPlayerStatisticsArray): String;
Var
  sl: TStringList;
  s: String;
  i: Integer;
  j: TPlayerStatSelector;
Begin
  sl := TStringList.Create;
  // Die Header Zeile
  s := #9;
  For i := 0 To high(Stats) Do Begin
    s := s + Stats[i].UserName + ';' + KeySetToString(Stats[i].KeySet);
    // Immer mit Kennung, nur Einträge ohne werden am Namen erkannt (IsLegacyAiName)
    If Stats[i].IsAi Then s := s + ';ai' Else s := s + ';human';
    s := s + #9;
  End;
  sl.Add(s);
  // Der Eigentliche Inhalt ;)
  For j In TPlayerStatSelector Do Begin
    s := PlayerStatSelectorToString(j) + #9;
    For i := 0 To high(Stats) Do Begin
      s := s + IntToStr(Stats[i].stats[j]) + #9;
    End;
    sl.Add(s);
  End;
  result := sl.Text;
  sl.free;
End;

{ TPlayerStatisticEngine }

Constructor TPlayerStatisticEngine.Create;
Begin
  Inherited Create;
  fPlayerStatistics := Nil;
  fIndexMapper := Nil;
End;

Destructor TPlayerStatisticEngine.Destroy;
Begin
  SetLength(fPlayerStatistics, 0);
  SetLength(fIndexMapper, 0);
End;

Procedure TPlayerStatisticEngine.LoadPlayerStatistics(Const aFilename: String);
Var
  sl: TStringList;
  EnterID: Integer;
Begin
  EnterID := LogEnter('TPlayerStatisticEngine.LoadPlayerStatistics');
  If Not FileExists(aFilename) Then Begin
    LogLeave(EnterID);
    exit;
  End;
  sl := TStringList.Create;
  sl.LoadFromFile(aFilename);
  fPlayerStatistics := PlayerStatisticsFromText(sl.Text);
  sl.free;
  LogLeave(EnterID);
End;

Procedure TPlayerStatisticEngine.SavePlayerStatistics(Const aFilename: String);
Var
  sl: TStringList;
Begin
  sl := TStringList.Create;
  sl.Text := AsText();
  If ForceDirectories(ExtractFileDir(aFilename)) Then Begin
    sl.SaveToFile(aFilename);
  End
  Else Begin
    log('Unable to create: ' + aFilename, llCritical);
  End;
  sl.free;
End;

Function TPlayerStatisticEngine.AsText: String;
Begin
  result := PlayerStatisticsToText(fPlayerStatistics);
End;

Procedure TPlayerStatisticEngine.ResetPlayerIDs;
Begin
  setlength(fIndexMapper, 0);
End;

Procedure TPlayerStatisticEngine.InitPlayerID(Index: integer; UserName: String;
  KeySet: TKeySet; IsAi: Boolean);
Var
  ol, i, Localindex: Integer;
Var
  EnterID: Integer;
Begin
  If index < 0 Then exit;
  EnterID := LogEnter('TPlayerStatisticEngine.AssignPlayerID');
  // Löschen Verbotener Zeichen, man weis ja nie was spieler sich für namen ausdenken ..
  username := StringReplace(UserName, #13, '', [rfReplaceAll]);
  username := StringReplace(UserName, #10, '', [rfReplaceAll]);
  username := StringReplace(UserName, #9, '', [rfReplaceAll]);
  username := StringReplace(UserName, ';', '', [rfReplaceAll]);
  Log(format('Username: %s, KeySet %s', [UserName, KeySetToString(KeySet)]), lldebug);
  Localindex := -1;
  // Den Index bestimmen, auf welchem der Spieler getrackt wird ..
  For i := 0 To high(fPlayerStatistics) Do Begin
    (*
     * Eine KI findet auch ihre alten Einträge ohne Kennung (von vor dem ";ai") und markiert sie,
     * ein Mensch nur Einträge, die keine KI sind.
     *)
    If (fPlayerStatistics[i].UserName = UserName) And
      (fPlayerStatistics[i].KeySet = KeySet) And
      (IsAi Or (Not fPlayerStatistics[i].IsAi)) Then Begin
      Localindex := i;
      If IsAi Then fPlayerStatistics[i].IsAi := true;
      break;
    End;
  End;
  If Localindex = -1 Then Begin
    setlength(fPlayerStatistics, high(fPlayerStatistics) + 2);
    fPlayerStatistics[high(fPlayerStatistics)].UserName := UserName;
    fPlayerStatistics[high(fPlayerStatistics)].KeySet := KeySet;
    fPlayerStatistics[high(fPlayerStatistics)].IsAi := IsAi;
    FillChar(fPlayerStatistics[high(fPlayerStatistics)].stats, sizeof(fPlayerStatistics[high(fPlayerStatistics)].stats), 0);
    Localindex := high(fPlayerStatistics);
  End;
  // Hochzählen der Gestarteten Matches
  inc(fPlayerStatistics[Localindex].stats[pssPlayedMatches]);
  fPlayerStatistics[Localindex].stats[pssLastPlayed] := DateTimeToUnix(LocalTimeToUniversal(Now));
  // Abspeichern des Mappings
  If index > high(fIndexMapper) Then Begin
    ol := length(fIndexMapper);
    setlength(fIndexMapper, index + 1);
    For i := ol To high(fIndexMapper) Do
      fIndexMapper[i] := -1;
  End;
  fIndexMapper[Index] := Localindex;
  LogLeave(EnterID);
End;

Procedure TPlayerStatisticEngine.UpdatePlayerID(Index: Integer;
  StatSelector: TPlayerStatSelector; Value: uint64);
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TPlayerStatisticEngine.UpdatePlayerID');
  If (index > high(fIndexMapper)) Then Begin
    log('Invalid index ' + IntToStr(Index), llFatal);
    LogLeave(EnterID);
    exit;
  End;
  If (fIndexMapper[index] < 0) Or (fIndexMapper[index] > high(fPlayerStatistics)) Then Begin
    log('Invalid mapped index ' + IntToStr(fIndexMapper[index]), llFatal);
    LogLeave(EnterID);
    exit;
  End;
  inc(fPlayerStatistics[fIndexMapper[Index]].stats[StatSelector], Value);
  LogLeave(EnterID);
End;

End.

