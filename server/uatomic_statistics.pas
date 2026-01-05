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
    sMatchesStarted
    , sGamesStarted
    , sFramesRendered
    , sBombsDropped
    , sPowerupsCollected
    , sPlayerDeaths
    , sBricksDestroyed
    , sPowerUpDestroyed
    , sTotalNetworkBytesIn // UDP-Daten werden Ignoriert
    , sTotalNetworkBytesOut // UDP-Daten werden Ignoriert
    , sTotalNetworkPacketsIn // UDP-Daten werden Ignoriert
    , sTotalNetworkPacketsOut // UDP-Daten werden Ignoriert
    );

  TStatisticCallback = Procedure(StatSelector: TStatSelector; Value: uint64 = 1) Of Object;

  TGameStatistik = Record
    Total: Array[TStatSelector] Of UInt64;
    LastRun: Array[TStatSelector] Of UInt64;
  End;

  TPlayerStatSelector = (
    psKills // -- Fertig
    , psFriendlyFireKills // -- Fertig -- dazu zählen auch Selbstkills
    , psDeaths // -- Fertig
    , psPlayedMatches // -- Fertig
    , psWonMatches // -- Fertig
    , psPlayedRounds // -- Fertig
    , psWonRounds // -- Fertig
    , psDrawRounds // -- Fertig
    , psPlacedBombs // -- Fertig
    , psTriggeredBombs // -- Fertig -- Mit dem BombenTrigger
    , psThrownBombs // -- Fertig -- Mit dem Blauen Handschuh
    , psPunshedBombs // -- Fertig -- Mit dem Roten Handschuh
    , psSpoogedBombs // -- Fertig -- Mit dem Spooger auf einmal gelegt
    , psKickedBombs // -- Fertig -- Mit dem Kicker PowerUp
    , psBricksDestroyed // -- Fertig
    , psPowerupsCollected // -- Fertig
    , psPowerupsDestroyed // -- Fertig
    , psDiseased // -- Fertig -- Anzahl der Krankheiten die ein Spieler bekommen hat
    , psDiseaseSpreaded // -- Fertig -- Anzahl der "Ansteckungen" die ein Spieler an andere übergeben hat
    );

  TPlayerStatistic = Array[TPlayerStatSelector] Of uint64;

  TPlayerStatistics = Record
    UserName: String;
    KeySet: TKeySet;
    stats: TPlayerStatistic;
  End;

  TPlayerStatisticCallback = Procedure(PlayerIndex: Integer; StatSelector: TPlayerStatSelector; Value: uint64 = 1) Of Object;

  { TPlayerStatisticEngine }

  TPlayerStatisticEngine = Class
  private
    fPlayerStatistics: Array Of TPlayerStatistics;
    fIndexMapper: Array Of Integer; // Wandelt die Server Indexe um in die fPlayerStatistics Indexe
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    Procedure LoadPlayerStatistics(Const aFilename: String);
    Procedure SavePlayerStatistics(Const aFilename: String);

    Procedure ResetPlayerIDs; // Setzt ALle Aktuellen "Zuordungen zurück

    // Initialisiert alles Notwendige um via Index auf die Interne Struktur zugreifen zu können
    // Wird zu begin eines Matches aufgrufen
    // -> Match Counter ++
    Procedure AssignPlayerID(
      Index: integer; // Index, den der Server für Alle Callbacks übergeben wird
      UserName: String; // Zur Identifizierung des Spielers
      KeySet: TKeySet // Zur Identifizierung des Spielers
      );

    // Verändert den übergebenen StatSelector um Value
    Procedure UpdatePlayerID(
      Index: Integer; // Index, den der Server für Alle Callbacks übergeben wird
      StatSelector: TPlayerStatSelector;
      Value: uint64 = 1);
  End;

Implementation

Uses uatomic_global;

Function PlayerStatSelectorToString(aValue: TPlayerStatSelector): String;
Begin
  result := '';
  Case aValue Of
    psKills: result := 'Kills';
    psFriendlyFireKills: result := 'Friendly fire kills';
    psDeaths: result := 'Deaths';
    psPlayedMatches: result := 'Played Matches';
    psWonMatches: result := 'Matches won';
    psPlayedRounds: result := 'Played rounds';
    psWonRounds: result := 'Rounds won';
    psDrawRounds: result := 'Draw rounds';
    psPlacedBombs: result := 'Placed bombs';
    psTriggeredBombs: result := 'Triggered bombs';
    psThrownBombs: result := 'Thrown bombs';
    psPunshedBombs: result := 'Punshed bombs';
    psSpoogedBombs: result := 'Spooged bombs';
    psKickedBombs: result := 'Kicked bombs';
    psBricksDestroyed: result := 'Bricks destroyed';
    psPowerupsCollected: result := 'Powerups collected';
    psPowerupsDestroyed: result := 'Powerups destroyed';
    psDiseased: result := 'Diseased';
    psDiseaseSpreaded: result := 'Disease spreaded';
  Else Begin
      Raise exception.Create('Error: PlayerStatSelectorToString, missing implementation.');
    End;
  End;
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
  tmp, players: TStringArray;
  i, k: Integer;
  j: TPlayerStatSelector;
  s: String;
Begin
  If Not FileExists(aFilename) Then exit;
  sl := TStringList.Create;
  sl.LoadFromFile(aFilename);
  If trim(sl[0]) = '' Then exit;
  players := trim(sl[0]).Split(#9);
  setlength(fPlayerStatistics, length(players));
  For i := 0 To high(players) Do Begin
    tmp := players[i].Split(';');
    fPlayerStatistics[i].UserName := tmp[0];
    fPlayerStatistics[i].KeySet := StringToKeySet(tmp[1]);
    // Reset alles zu 0
    FillChar(fPlayerStatistics[i].stats, sizeof(fPlayerStatistics[i].stats), 0);
  End;
  For i := 1 To sl.Count - 1 Do Begin
    tmp := trim(sl[i]).Split(#9);
    // Ja das ist Umständlich aber dafür "robust"
    If length(tmp) <> length(fPlayerStatistics) + 1 Then Continue;
    For j In TPlayerStatSelector Do Begin
      s := PlayerStatSelectorToString(j);
      If tmp[0] = s Then Begin
        For k := 0 To high(fPlayerStatistics) Do Begin
          fPlayerStatistics[k].stats[j] := StrToInt64Def(tmp[k + 1], 0);
        End;
        break;
      End;
    End;
  End;
  sl.free;
End;

Procedure TPlayerStatisticEngine.SavePlayerStatistics(Const aFilename: String);
Var
  sl: TStringList;
  s: String;
  i: Integer;
  j: TPlayerStatSelector;
Begin
  sl := TStringList.Create;
  // Die Header Zeile
  s := #9;
  For i := 0 To high(fPlayerStatistics) Do Begin
    s := s + fPlayerStatistics[i].UserName + ';' + KeySetToString(fPlayerStatistics[i].KeySet) + #9;
  End;
  sl.Add(s);
  // Der Eigentliche Inhalt ;)
  For j In TPlayerStatSelector Do Begin
    s := PlayerStatSelectorToString(j) + #9;
    For i := 0 To high(fPlayerStatistics) Do Begin
      s := s + IntToStr(fPlayerStatistics[i].stats[j]) + #9;
    End;
    sl.Add(s);
  End;
  sl.SaveToFile(aFilename);
  sl.free;
End;

Procedure TPlayerStatisticEngine.ResetPlayerIDs;
Begin
  setlength(fIndexMapper, 0);
End;

Procedure TPlayerStatisticEngine.AssignPlayerID(Index: integer;
  UserName: String; KeySet: TKeySet);
Var
  ol, i, Localindex: Integer;
Var
  EnterID: Integer;
Begin
  EnterID := LogEnter('TPlayerStatisticEngine.AssignPlayerID');
  // Löschen Verbotener Zeichen, man weis ja nie was spieler sich für namen ausdenken ..
  username := StringReplace(UserName, #9, '', [rfReplaceAll]);
  username := StringReplace(UserName, ';', '', [rfReplaceAll]);
  Log(format('Username: %s, KeySet %s', [UserName, KeySetToString(KeySet)]), llTrace);
  Localindex := -1;
  // Den Index bestimmen, auf welchem der Spieler getrackt wird ..
  For i := 0 To high(fPlayerStatistics) Do Begin
    If (fPlayerStatistics[i].UserName = UserName) And
      (fPlayerStatistics[i].KeySet = KeySet) Then Begin
      Localindex := i;
      break;
    End;
  End;
  If Localindex = -1 Then Begin
    setlength(fPlayerStatistics, high(fPlayerStatistics) + 2);
    fPlayerStatistics[high(fPlayerStatistics)].UserName := UserName;
    fPlayerStatistics[high(fPlayerStatistics)].KeySet := KeySet;
    FillChar(fPlayerStatistics[high(fPlayerStatistics)].stats, sizeof(fPlayerStatistics[high(fPlayerStatistics)].stats), 0);
    Localindex := high(fPlayerStatistics);
  End;
  // Hochzählen der Gestarteten Matches
  inc(fPlayerStatistics[Localindex].stats[psPlayedMatches]);
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

