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
Unit Unit8;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Grids, IniFiles, uatomic_common, uatomic_statistics;

Type

  (*
   * Die Spalten der Spielertabelle, in der Reihenfolge der Anzeige.
   * Kompakt sind die Spalten bis einschließlich scLastPlayed, "Alle Werte" zeigt auch den Rest.
   *)
  TStatColumn = (
    scRank, scName, scKeys, scMatches, scWonMatches, scWinRate, scRounds, scWonRounds, scDrawRounds,
    scKills, scDeaths, scKD, scFriendlyFire, scPlayTime, scLastPlayed,
    scPlacedBombs, scTriggeredBombs, scThrownBombs, scPushedBombs, scKickedBombs, scSpoogedBombs, scCarriedBombs,
    scBricks, scPowerupsCollected, scPowerupsDestroyed, scDiseased, scDiseaseSpread);

  TStatColumns = Array Of TStatColumn;

  // Eine Quelle der Statistik: der eigene Server oder die Kopie eines anderen (server_stats/<ID>.ini / .csv)
  TStatSource = Record
    ID: String; // '' = dieser Computer
    Name: String; // Rechnername des Servers
    Address: String; // IP:Port, über die gespielt wurde
    LastPlayed: Int64; // Unix Zeit (UTC)
  End;

  { TForm8 }

  (*
   * Der Highscore: die Statistik eines Servers (Playerstats.csv und stats.txt).
   * Die Statistik führt immer der Server, also der Rechner, auf dem gehostet wurde. Die eigene liest der Launcher direkt,
   * von anderen Servern hat der Client beim Spielen eine Kopie gespeichert (TGame.HandleServerStatistics).
   * Reset betrifft nach Bestätigung alle Werte des aktiven Tabs der gewählten Quelle, unabhängig vom Spielerfilter.
   * Der andere Tab und die Serverkennung bleiben erhalten; bei fremden Servern wird nur die lokale Kopie geändert.
   *)
  TForm8 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure PageControl1Change(Sender: TObject);
    Procedure StringGrid1GetCellHint(Sender: TObject; ACol, ARow: Integer; Var HintText: String);
    Procedure StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    Procedure StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    Procedure StringGrid2PrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
  private
    fSources: Array Of TStatSource; // Zu jedem Eintrag in ComboBox1
    fPlayers: TPlayerStatisticsArray; // Die Spieler der gewählten Quelle, wie in der Datei
    fShown: TPlayerStatisticsArray; // Zusammengefasst, gefiltert und sortiert, Zeile i + 1 in StringGrid1
    fColumns: TStatColumns; // Die angezeigten Spalten von StringGrid1
    fSortColumn: TStatColumn;
    fSortDesc: Boolean;
    fOwnName: String; // Der eigene Spielername, wird fett gezeigt
    fLoading: Boolean; // True während Execute die Einstellungen setzt, dann nicht neu füllen
    Procedure ReadSources(Const SelectID: String);
    Procedure LoadSource();
    Procedure UpdateResetButton();
    Procedure LoadServerValues(Const Source: TStatSource);
    Procedure FillPlayers();
    Function DisplayName(Index: integer): String; // Der Name in Zeile Index von fShown, bei gleichen Namen mit Nummer des Spielers am PC
    Function CompareRows(Const a, b: TPlayerStatistics): integer;
  public
    (*
     * Zeigt die Statistik, SelectID wählt die Quelle vor ('' = dieser Computer, z.B. nach einem Match die des Servers).
     * ini ist fpc_atomic.cfg, darin merkt sich das Fenster seine Einstellungen.
     *)
    Procedure Execute(Const ini: TIniFile; Const OwnName, SelectID: String);
    Function LocalServerID(): String; // Die Kennung des Servers auf diesem Computer (stats.txt), '' = hier wurde noch nie gehostet
    Function HasSource(Const ID: String): Boolean; // Gibt es zur Kennung eine Quelle (der eigene Server oder eine Kopie)
  End;

Var
  Form8: TForm8;

Implementation

{$R *.lfm}

Uses Math, DateUtils, LCLIntf, LCLType, Buttons, ulanguage, ulayout, uatomic_global;

Const
  MinMatchesForRate = 3; // Darunter zeigen Sieg-% und K/D "–", sonst stünde ein Spieler mit 1 von 1 Match ganz oben
  NoValue = '–';
  CompactColumns = [scRank..scLastPlayed];
  // Die Spalten, die direkt einen Wert der Datei zeigen
  ColumnSelector: Array[scMatches..scDiseaseSpread] Of TPlayerStatSelector = (
    pssPlayedMatches, pssWonMatches, pssWonMatches {scWinRate, berechnet}, pssPlayedRounds, pssWonRounds, pssDrawRounds,
    pssKills, pssDeaths, pssKills {scKD, berechnet}, pssFriendlyFireKills, pssPlayTimeMs, pssLastPlayed,
    pssPlacedBombs, pssTriggeredBombs, pssThrownBombs, pssPushedBombs, pssKickedBombs, pssSpoogedBombs, pssCarriedBombs,
    pssBricksDestroyed, pssPowerupsCollected, pssPowerupsDestroyed, pssDiseased, pssDiseaseSpread);

Function ColumnCaption(c: TStatColumn): String;
Begin
  Case c Of
    scRank: result := '#';
    scName: result := R_StatColName;
    scKeys: result := R_StatColKeys;
    scMatches: result := R_StatColMatches;
    scWonMatches: result := R_StatColWonMatches;
    scWinRate: result := R_StatColWinRate;
    scRounds: result := R_StatColRounds;
    scWonRounds: result := R_StatColWonRounds;
    scDrawRounds: result := R_StatColDrawRounds;
    scKills: result := R_StatColKills;
    scDeaths: result := R_StatColDeaths;
    scKD: result := R_StatColKD;
    scFriendlyFire: result := R_StatColFriendlyFire;
    scPlayTime: result := R_StatColPlayTime;
    scLastPlayed: result := R_StatColLastPlayed;
    scPlacedBombs: result := R_StatColPlacedBombs;
    scTriggeredBombs: result := R_StatColTriggeredBombs;
    scThrownBombs: result := R_StatColThrownBombs;
    scPushedBombs: result := R_StatColPushedBombs;
    scKickedBombs: result := R_StatColKickedBombs;
    scSpoogedBombs: result := R_StatColSpoogedBombs;
    scCarriedBombs: result := R_StatColCarriedBombs;
    scBricks: result := R_StatColBricks;
    scPowerupsCollected: result := R_StatColPowerupsCollected;
    scPowerupsDestroyed: result := R_StatColPowerupsDestroyed;
    scDiseased: result := R_StatColDiseased;
    scDiseaseSpread: result := R_StatColDiseaseSpread;
  End;
End;

Function ColumnHint(c: TStatColumn): String;
Begin
  Case c Of
    scRank: result := R_StatHintRank;
    scName: result := R_StatHintName;
    scKeys: result := R_StatHintKeys;
    scMatches: result := R_StatHintMatches;
    scWonMatches: result := R_StatHintWonMatches;
    scWinRate: result := format(R_StatHintWinRate, [MinMatchesForRate]);
    scRounds: result := R_StatHintRounds;
    scWonRounds: result := R_StatHintWonRounds;
    scDrawRounds: result := R_StatHintDrawRounds;
    scKills: result := R_StatHintKills;
    scDeaths: result := R_StatHintDeaths;
    scKD: result := format(R_StatHintKD, [MinMatchesForRate]);
    scFriendlyFire: result := R_StatHintFriendlyFire;
    scPlayTime: result := R_StatHintPlayTime;
    scLastPlayed: result := R_StatHintLastPlayed;
    scPlacedBombs: result := R_StatHintPlacedBombs;
    scTriggeredBombs: result := R_StatHintTriggeredBombs;
    scThrownBombs: result := R_StatHintThrownBombs;
    scPushedBombs: result := R_StatHintPushedBombs;
    scKickedBombs: result := R_StatHintKickedBombs;
    scSpoogedBombs: result := R_StatHintSpoogedBombs;
    scCarriedBombs: result := R_StatHintCarriedBombs;
    scBricks: result := R_StatHintBricks;
    scPowerupsCollected: result := R_StatHintPowerupsCollected;
    scPowerupsDestroyed: result := R_StatHintPowerupsDestroyed;
    scDiseased: result := R_StatHintDiseased;
    scDiseaseSpread: result := R_StatHintDiseaseSpread;
  Else
    result := '';
  End;
End;

Function StatSelectorCaption(ss: TStatSelector): String;
Begin
  Case ss Of
    ssMatchesStarted: result := R_StatMatchesStarted;
    ssGamesStarted: result := R_StatGamesStarted;
    ssFramesRendered: result := R_StatFramesRendered;
    ssBombsDropped: result := R_StatBombsDropped;
    ssPowerupsCollected: result := R_StatPowerupsCollected;
    ssPlayerDeaths: result := R_StatPlayerDeaths;
    ssBricksDestroyed: result := R_StatBricksDestroyed;
    ssPowerupDestroyed: result := R_StatPowerupDestroyed;
    ssTotalNetworkBytesIn: result := R_StatBytesIn;
    ssTotalNetworkBytesOut: result := R_StatBytesOut;
    ssTotalNetworkPacketsIn: result := R_StatPacketsIn;
    ssTotalNetworkPacketsOut: result := R_StatPacketsOut;
  Else
    result := StatSelectorToString(ss);
  End;
End;

Function FormatPlayTime(ms: QWord): String;
Begin
  result := format('%d:%.2d', [ms Div 3600000, (ms Div 60000) Mod 60]);
End;

Function FormatBytes(b: QWord): String;
Begin
  If b < 1024 * 1024 Then Begin
    result := FormatFloat('#,##0', b / 1024) + ' KB';
  End
  Else Begin
    result := FormatFloat('#,##0.0', b / (1024 * 1024)) + ' MB';
  End;
End;

Function FormatDate(UnixUtc: Int64): String;
Begin
  If UnixUtc <= 0 Then Begin
    result := '';
  End
  Else Begin
    result := DateToStr(UnixToDateTime(UnixUtc, false));
  End;
End;

(*
 * Der Text, für den eine Spalte mindestens breit ist: Zähler wachsen über die Zeit (Bomben, Blöcke, Items gehen in die
 * Zehntausende), mit fester Mindestbreite springen die Spalten nicht und schneiden nichts ab
 *)
Function ColumnMinSample(c: TStatColumn): String;
Begin
  Case c Of
    scRank, scName, scKeys, scWinRate, scKD: result := '';
    scPlayTime: result := '888:88';
    scLastPlayed: result := DateToStr(EncodeDate(2088, 12, 28));
  Else
    result := FormatFloat('#,##0', 888888);
  End;
End;

(*
 * Setzt die Spaltenbreiten nach dem breitesten Text, fett gemessen (der eigene Name ist fett, fett ist breiter).
 * Samples[i] (oder DefaultSample, wenn Samples kürzer ist) ist die Mindestbreite der Spalte i, Spalte 0 hat keine.
 *)
Procedure SizeGridColumns(Grid: TStringGrid; Const Samples: Array Of String; Const DefaultSample: String);
Const
  Padding = 16;
Var
  b: TBitmap;
  c, r, w: integer;
  s: String;
Begin
  b := TBitmap.Create;
  Try
    b.Canvas.Font.Assign(Grid.Font);
    b.Canvas.Font.Style := [fsBold];
    For c := 0 To Grid.ColCount - 1 Do Begin
      If c <= high(Samples) Then s := Samples[c] Else If c > 0 Then s := DefaultSample Else s := '';
      w := 0;
      If s <> '' Then w := b.Canvas.TextWidth(s);
      For r := 0 To Grid.RowCount - 1 Do Begin
        w := max(w, b.Canvas.TextWidth(Grid.Cells[c, r]));
      End;
      Grid.ColWidths[c] := w + Padding;
    End;
  Finally
    b.free;
  End;
End;

(*
 * Der Wert eines Spielers in einer Spalte, false = es gibt keinen (zu wenige Matches für eine Quote),
 * dann zeigt die Tabelle "–" und sortiert ihn immer ans Ende.
 *)
Function ColumnValue(Const p: TPlayerStatistics; c: TStatColumn; Out Value: Double): Boolean;
Begin
  result := true;
  Value := 0;
  Case c Of
    scRank, scName: result := false;
    scKeys: Value := ord(p.KeySet);
    scWinRate: Begin
        result := p.stats[pssPlayedMatches] >= MinMatchesForRate;
        If result Then Value := 100 * p.stats[pssWonMatches] / p.stats[pssPlayedMatches];
      End;
    scKD: Begin
        result := p.stats[pssPlayedMatches] >= MinMatchesForRate;
        If result Then Value := p.stats[pssKills] / max(1, p.stats[pssDeaths]);
      End;
  Else
    Value := p.stats[ColumnSelector[c]];
  End;
End;

Function ColumnText(Const p: TPlayerStatistics; c: TStatColumn): String;
Var
  v: Double;
Begin
  If c = scName Then Begin
    result := p.UserName;
    If p.IsAi Then result := result + R_StatAiSuffix;
    exit;
  End;
  If Not ColumnValue(p, c, v) Then Begin
    result := NoValue;
    exit;
  End;
  Case c Of
    scKeys: result := inttostr(ord(p.KeySet) + 1);
    scWinRate: result := FormatFloat('0', v) + ' %';
    scKD: result := FormatFloat('0.00', v);
    scPlayTime: result := FormatPlayTime(p.stats[pssPlayTimeMs]);
    scLastPlayed: result := FormatDate(p.stats[pssLastPlayed]);
  Else
    result := FormatFloat('#,##0', v);
  End;
End;

{ TForm8 }

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  fSources := Nil;
  fPlayers := Nil;
  fShown := Nil;
  fColumns := Nil;
  fSortColumn := scWonMatches;
  fSortDesc := true;
  fLoading := false;
  PageControl1.ActivePage := TabSheet1;
End;

Procedure TForm8.FormShow(Sender: TObject);
Begin
  FitLayout(Panel1); // Platz für die Texte im aktuellen Widgetset und in der aktuellen Sprache
  FitLayout(Panel2);
End;

Function TForm8.LocalServerID: String;
Var
  ini: TIniFile;
Begin
  result := '';
  If Not FileExists(GetAtomicStatsFile()) Then exit;
  ini := TIniFile.Create(GetAtomicStatsFile());
  Try
    result := ini.ReadString('Server', 'ID', '');
  Finally
    ini.free;
  End;
End;

Function TForm8.HasSource(Const ID: String): Boolean;
Begin
  result := (ID <> '') And ((ID = LocalServerID()) Or FileExists(GetAtomicServerStatsDir() + ID + '.ini'));
End;

Procedure TForm8.ReadSources(Const SelectID: String);
Var
  sr: TSearchRec;
  ini: TIniFile;
  src: TStatSource;
  i, j, Sel: Integer;
  Dir, LocalID: String;
Begin
  fSources := Nil;
  // 1. Der Server auf diesem Computer, den gibt es immer (auch wenn hier noch nie gehostet wurde)
  setlength(fSources, 1);
  fSources[0].ID := '';
  fSources[0].Name := '';
  fSources[0].Address := '';
  fSources[0].LastPlayed := 0;
  // 2. Die Kopien der anderen Server
  Dir := GetAtomicServerStatsDir();
  If FindFirst(Dir + '*.ini', faAnyFile, sr) = 0 Then Begin
    Repeat
      If SameText(sr.Name, LastMatchFile) Then Continue;
      src.ID := ChangeFileExt(sr.Name, '');
      ini := TIniFile.Create(Dir + sr.Name);
      Try
        src.Name := ini.ReadString('Server', 'Name', '');
        src.Address := ini.ReadString('Server', 'Address', '');
        src.LastPlayed := ini.ReadInt64('Server', 'LastPlayed', 0);
      Finally
        ini.free;
      End;
      setlength(fSources, length(fSources) + 1);
      fSources[high(fSources)] := src;
    Until FindNext(sr) <> 0;
  End;
  FindClose(sr);
  // Zuletzt gespielt zuerst
  For i := 2 To high(fSources) Do Begin
    src := fSources[i];
    j := i - 1;
    While (j >= 1) And (fSources[j].LastPlayed < src.LastPlayed) Do Begin
      fSources[j + 1] := fSources[j];
      dec(j);
    End;
    fSources[j + 1] := src;
  End;
  LocalID := LocalServerID();
  Sel := 0;
  ComboBox1.Items.BeginUpdate;
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add(R_StatThisComputer);
  For i := 1 To high(fSources) Do Begin
    If fSources[i].Name <> '' Then Begin
      ComboBox1.Items.Add(fSources[i].Name + ' – ' + fSources[i].Address + ' – ' + FormatDate(fSources[i].LastPlayed));
    End
    Else Begin
      ComboBox1.Items.Add(fSources[i].Address + ' – ' + FormatDate(fSources[i].LastPlayed));
    End;
    If (SelectID <> '') And (fSources[i].ID = SelectID) And (SelectID <> LocalID) Then Sel := i;
  End;
  ComboBox1.Items.EndUpdate;
  ComboBox1.ItemIndex := Sel;
End;

Procedure TForm8.LoadServerValues(Const Source: TStatSource);
Var
  ini: TIniFile;
  ss: TStatSelector;
  r: integer;
  WithLastRun: Boolean;
  Filename: String;

  Function Fmt(ss: TStatSelector; Value: QWord): String;
  Begin
    Case ss Of
      ssFramesRendered: result := FormatPlayTime(Value * FrameRate);
      ssTotalNetworkBytesIn, ssTotalNetworkBytesOut: result := FormatBytes(Value);
    Else
      result := FormatFloat('#,##0', Value);
    End;
  End;

Begin
  // Die Werte des Servers, beim eigenen auch die des letzten Laufs (die Kopien haben nur die Gesamtwerte)
  WithLastRun := Source.ID = '';
  If WithLastRun Then Begin
    Filename := GetAtomicStatsFile();
  End
  Else Begin
    Filename := GetAtomicServerStatsDir() + Source.ID + '.ini';
  End;
  StringGrid2.BeginUpdate;
  Try
    StringGrid2.ColCount := ifthen(WithLastRun, 3, 2);
    StringGrid2.RowCount := 1 + ord(high(TStatSelector)) + 1;
    StringGrid2.Cells[0, 0] := R_StatValue;
    StringGrid2.Cells[1, 0] := R_StatTotal;
    If WithLastRun Then StringGrid2.Cells[2, 0] := R_StatLastRun;
    ini := Nil;
    If FileExists(Filename) Then ini := TIniFile.Create(Filename);
    Try
      r := 1;
      For ss In TStatSelector Do Begin
        StringGrid2.Cells[0, r] := StatSelectorCaption(ss);
        If assigned(ini) Then Begin
          StringGrid2.Cells[1, r] := Fmt(ss, StrToQWordDef(ini.ReadString('Total', StatSelectorToString(ss), '0'), 0));
          If WithLastRun Then
            StringGrid2.Cells[2, r] := Fmt(ss, StrToQWordDef(ini.ReadString('LastRun', StatSelectorToString(ss), '0'), 0));
        End
        Else Begin
          StringGrid2.Cells[1, r] := NoValue;
          If WithLastRun Then StringGrid2.Cells[2, r] := NoValue;
        End;
        inc(r);
      End;
    Finally
      ini.free;
    End;
    SizeGridColumns(StringGrid2, [], FormatBytes(8888 * 1024 * 1024));
  Finally
    StringGrid2.EndUpdate;
  End;
End;

Procedure TForm8.LoadSource;
Var
  src: TStatSource;
  Filename: String;
  sl: TStringList;
Begin
  fPlayers := Nil;
  If (ComboBox1.ItemIndex < 0) Or (ComboBox1.ItemIndex > high(fSources)) Then exit;
  src := fSources[ComboBox1.ItemIndex];
  If src.ID = '' Then Begin
    Filename := GetAtomicPlayerStatsFile();
  End
  Else Begin
    Filename := GetAtomicServerStatsDir() + src.ID + '.csv';
  End;
  If FileExists(Filename) Then Begin
    sl := TStringList.Create;
    Try
      Try
        sl.LoadFromFile(Filename);
        fPlayers := PlayerStatisticsFromText(sl.Text);
      Except
        On e: Exception Do log('Unable to read ' + Filename + ': ' + e.Message, llError);
      End;
    Finally
      sl.free;
    End;
  End;
  Button1.Enabled := src.ID <> '';
  If src.ID = '' Then Begin
    If FileExists(Filename) Or FileExists(GetAtomicStatsFile()) Then Begin
      Label3.Caption := R_StatLocalInfo;
    End
    Else Begin
      Label3.Caption := R_StatNoLocal;
    End;
  End
  Else Begin
    Label3.Caption := format(R_StatRemoteInfo, [src.Name, src.Address, FormatDate(src.LastPlayed)]);
  End;
  LoadServerValues(src);
  FillPlayers();
  UpdateResetButton();
End;

Procedure TForm8.UpdateResetButton;
Var
  src: TStatSource;
  Filename: String;
  ini: TIniFile;
  Players: Boolean;
Begin
  Button4.Enabled := false;
  If (ComboBox1.ItemIndex < 0) Or (ComboBox1.ItemIndex > high(fSources)) Then exit;
  src := fSources[ComboBox1.ItemIndex];
  Players := PageControl1.ActivePage = TabSheet1;
  If Players Then Begin
    If src.ID = '' Then Filename := GetAtomicPlayerStatsFile()
    Else Filename := GetAtomicServerStatsDir() + src.ID + '.csv';
  End
  Else Begin
    If src.ID = '' Then Filename := GetAtomicStatsFile()
    Else Filename := GetAtomicServerStatsDir() + src.ID + '.ini';
  End;
  If Not FileExists(Filename) Then exit;
  If Players Then Begin
    Button4.Enabled := true;
  End
  Else Begin
    ini := TIniFile.Create(Filename);
    Try
      Button4.Enabled := ini.SectionExists('Total') Or ini.SectionExists('LastRun');
    Finally
      ini.free;
    End;
  End;
End;

Procedure TForm8.PageControl1Change(Sender: TObject);
Begin
  If fLoading Then exit;
  UpdateResetButton();
End;

Function TForm8.CompareRows(Const a, b: TPlayerStatistics): integer;
Var
  va, vb: Double;
  oka, okb: Boolean;
Begin
  If fSortColumn = scName Then Begin
    result := CompareText(a.UserName, b.UserName);
    If fSortDesc Then result := -result;
  End
  Else Begin
    oka := ColumnValue(a, fSortColumn, va);
    okb := ColumnValue(b, fSortColumn, vb);
    // Ohne Wert immer ans Ende, egal in welche Richtung sortiert wird
    If oka <> okb Then Begin
      If oka Then result := -1 Else result := 1;
      exit;
    End;
    result := CompareValue(va, vb);
    If fSortDesc Then result := -result;
  End;
  // Gleichstand: mehr Siege zuerst, dann nach Name
  If result = 0 Then result := -CompareValue(a.stats[pssWonMatches], b.stats[pssWonMatches]);
  If result = 0 Then result := CompareText(a.UserName, b.UserName);
End;

Function TForm8.DisplayName(Index: integer): String;
Var
  i: integer;
Begin
  (*
   * Gibt es den Namen mehrfach (ohne "Tastenbelegungen zusammenfassen" je Tastenbelegung eine Zeile),
   * steht ab dem zweiten Spieler am PC dessen Nummer dahinter: "Tom", "Tom (2)"
   *)
  result := ColumnText(fShown[Index], scName);
  If fShown[Index].KeySet = ks0 Then exit;
  For i := 0 To high(fShown) Do Begin
    If (i <> Index) And (fShown[i].UserName = fShown[Index].UserName) And (fShown[i].IsAi = fShown[Index].IsAi) Then Begin
      result := result + ' (' + inttostr(ord(fShown[Index].KeySet) + 1) + ')';
      exit;
    End;
  End;
End;

Procedure TForm8.FillPlayers;
Var
  Samples: Array Of String;
  i, j, k, r: Integer;
  c: TStatColumn;
  s: TPlayerStatSelector;
  p: TPlayerStatistics;
  Found: Boolean;
Begin
  // 1. Zusammenfassen und filtern
  fShown := Nil;
  For i := 0 To high(fPlayers) Do Begin
    Case ComboBox2.ItemIndex Of
      1: If fPlayers[i].IsAi Then Continue; // Ohne KI
      2: If Not fPlayers[i].IsAi Then Continue; // Nur KI
    End;
    Found := false;
    If CheckBox2.Checked Then Begin
      // Gleicher Name mit anderer Tastenbelegung = der gleiche Spieler
      For j := 0 To high(fShown) Do Begin
        If (fShown[j].UserName = fPlayers[i].UserName) And (fShown[j].IsAi = fPlayers[i].IsAi) Then Begin
          For s In TPlayerStatSelector Do Begin
            If s = pssLastPlayed Then Begin
              fShown[j].stats[s] := max(fShown[j].stats[s], fPlayers[i].stats[s]);
            End
            Else Begin
              fShown[j].stats[s] := fShown[j].stats[s] + fPlayers[i].stats[s];
            End;
          End;
          Found := true;
          break;
        End;
      End;
    End;
    If Not Found Then Begin
      setlength(fShown, length(fShown) + 1);
      fShown[high(fShown)] := fPlayers[i];
    End;
  End;
  // 2. Sortieren
  For i := 1 To high(fShown) Do Begin
    p := fShown[i];
    j := i - 1;
    While (j >= 0) And (CompareRows(fShown[j], p) > 0) Do Begin
      fShown[j + 1] := fShown[j];
      dec(j);
    End;
    fShown[j + 1] := p;
  End;
  // 3. Die Spalten
  fColumns := Nil;
  For c In TStatColumn Do Begin
    If (c = scKeys) And CheckBox2.Checked Then Continue;
    If (Not CheckBox1.Checked) And (Not (c In CompactColumns)) Then Continue;
    setlength(fColumns, length(fColumns) + 1);
    fColumns[high(fColumns)] := c;
  End;
  // 4. Die Tabelle
  StringGrid1.BeginUpdate;
  Try
    StringGrid1.ColCount := length(fColumns);
    StringGrid1.RowCount := 1 + length(fShown);
    StringGrid1.FixedCols := 2; // # und Name bleiben beim seitlichen Scrollen stehen
    For k := 0 To high(fColumns) Do Begin
      StringGrid1.Cells[k, 0] := ColumnCaption(fColumns[k]);
      If fColumns[k] = fSortColumn Then Begin
        If fSortDesc Then Begin
          StringGrid1.Cells[k, 0] := StringGrid1.Cells[k, 0] + ' ▼';
        End
        Else Begin
          StringGrid1.Cells[k, 0] := StringGrid1.Cells[k, 0] + ' ▲';
        End;
      End;
    End;
    For r := 0 To high(fShown) Do Begin
      For k := 0 To high(fColumns) Do Begin
        If fColumns[k] = scRank Then Begin
          StringGrid1.Cells[k, r + 1] := inttostr(r + 1);
        End
        Else If fColumns[k] = scName Then Begin
          StringGrid1.Cells[k, r + 1] := DisplayName(r);
        End
        Else Begin
          StringGrid1.Cells[k, r + 1] := ColumnText(fShown[r], fColumns[k]);
        End;
      End;
    End;
    setlength(Samples, length(fColumns));
    For k := 0 To high(fColumns) Do Begin
      Samples[k] := ColumnMinSample(fColumns[k]);
    End;
    SizeGridColumns(StringGrid1, Samples, '');
  Finally
    StringGrid1.EndUpdate;
  End;
End;

Procedure TForm8.ComboBox1Change(Sender: TObject);
Begin
  If fLoading Then exit;
  LoadSource();
End;

Procedure TForm8.CheckBox1Change(Sender: TObject);
Begin
  // Auch für CheckBox2 und ComboBox2
  If fLoading Then exit;
  FillPlayers();
End;

Procedure TForm8.Button1Click(Sender: TObject);
Var
  src: TStatSource;
  s: String;
Begin
  // Die Kopie eines anderen Servers löschen
  If (ComboBox1.ItemIndex < 1) Or (ComboBox1.ItemIndex > high(fSources)) Then exit;
  src := fSources[ComboBox1.ItemIndex];
  s := format(R_StatRemoveAsk, [ComboBox1.Text]);
  If Application.MessageBox(pchar(s), pchar(R_Warning), MB_YESNO Or MB_ICONQUESTION) <> idYes Then exit;
  DeleteFile(GetAtomicServerStatsDir() + src.ID + '.ini');
  DeleteFile(GetAtomicServerStatsDir() + src.ID + '.csv');
  ReadSources('');
  LoadSource();
End;

Procedure TForm8.Button2Click(Sender: TObject);
Var
  Dir: String;
Begin
  // Der Ordner mit den Dateien der gewählten Quelle
  If ComboBox1.ItemIndex > 0 Then Begin
    Dir := GetAtomicServerStatsDir();
  End
  Else Begin
    Dir := ExtractFilePath(GetAtomicStatsFile());
  End;
  ForceDirectories(Dir);
  OpenDocument(Dir);
End;

Procedure TForm8.Button4Click(Sender: TObject);
Var
  src: TStatSource;
  Filename, s: String;
  ini: TIniFile;
  Players: Boolean;
  Confirmation: TForm;
  i: Integer;
  b: TBitBtn;
Begin
  UpdateResetButton();
  If Not Button4.Enabled Then Begin
    LoadSource();
    exit;
  End;
  If (ComboBox1.ItemIndex < 0) Or (ComboBox1.ItemIndex > high(fSources)) Then exit;
  src := fSources[ComboBox1.ItemIndex];
  Players := PageControl1.ActivePage = TabSheet1;
  If Players Then Begin
    s := format(R_StatResetPlayersAsk, [ComboBox1.Text]);
    If src.ID = '' Then Filename := GetAtomicPlayerStatsFile()
    Else Filename := GetAtomicServerStatsDir() + src.ID + '.csv';
  End
  Else Begin
    s := format(R_StatResetServerAsk, [ComboBox1.Text]);
    If src.ID = '' Then Filename := GetAtomicStatsFile()
    Else Filename := GetAtomicServerStatsDir() + src.ID + '.ini';
  End;
  Confirmation := CreateMessageDialog(R_Warning, s, mtWarning, [mbYes, mbNo]);
  Try
    For i := 0 To Confirmation.ComponentCount - 1 Do Begin
      If Confirmation.Components[i] Is TBitBtn Then Begin
        b := TBitBtn(Confirmation.Components[i]);
        b.Default := b.ModalResult = mrNo;
        If b.Default Then Begin
          b.Cancel := true;
          Confirmation.ActiveControl := b;
        End;
      End;
    End;
    If Confirmation.ShowModal <> mrYes Then exit;
  Finally
    Confirmation.free;
  End;
  Try
    If FileExists(Filename) Then Begin
      If Players Then Begin
        If Not DeleteFile(Filename) Then RaiseLastOSError;
      End
      Else Begin
        // Die Kennung und die Metadaten der Quelle bleiben erhalten.
        ini := TIniFile.Create(Filename);
        Try
          ini.EraseSection('Total');
          ini.EraseSection('LastRun');
          ini.UpdateFile;
        Finally
          ini.free;
        End;
      End;
    End;
  Except
    On e: Exception Do Begin
      s := format(R_StatResetError, [Filename, e.Message]);
      log(s, llError);
      Application.MessageBox(pchar(s), pchar(R_Warning), MB_OK Or MB_ICONERROR);
      exit;
    End;
  End;
  LoadSource();
End;

Procedure TForm8.StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
Var
  c: TStatColumn;
Begin
  If (Not IsColumn) Or (Index < 0) Or (Index > high(fColumns)) Then exit;
  c := fColumns[Index];
  If c = scRank Then Begin
    // # zeigt nur die Reihenfolge, der Klick stellt die Standard Sortierung wieder her
    fSortColumn := scWonMatches;
    fSortDesc := true;
  End
  Else If c = fSortColumn Then Begin
    fSortDesc := Not fSortDesc;
  End
  Else Begin
    fSortColumn := c;
    fSortDesc := c <> scName; // Zahlen zuerst die größten, Namen von A bis Z
  End;
  FillPlayers();
End;

Procedure TForm8.StringGrid1GetCellHint(Sender: TObject; ACol, ARow: Integer;
  Var HintText: String);
Begin
  HintText := '';
  If (ARow = 0) And (ACol >= 0) And (ACol <= high(fColumns)) Then Begin
    HintText := ColumnHint(fColumns[ACol]);
  End;
End;

Procedure TForm8.StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
Var
  ts: TTextStyle;
Begin
  If (aCol < 0) Or (aCol > high(fColumns)) Then exit;
  If (fColumns[aCol] <> scName) Then Begin
    ts := StringGrid1.Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    StringGrid1.Canvas.TextStyle := ts;
  End;
  If (aRow < 1) Or (aRow > length(fShown)) Then exit;
  If fShown[aRow - 1].IsAi Then Begin
    StringGrid1.Canvas.Font.Style := StringGrid1.Canvas.Font.Style + [fsItalic];
    If Not (gdSelected In aState) Then StringGrid1.Canvas.Font.Color := clGrayText;
  End
  Else If (fOwnName <> '') And (fShown[aRow - 1].UserName = fOwnName) Then Begin
    StringGrid1.Canvas.Font.Style := StringGrid1.Canvas.Font.Style + [fsBold];
  End;
End;

Procedure TForm8.StringGrid2PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
Var
  ts: TTextStyle;
Begin
  If aCol > 0 Then Begin
    ts := StringGrid2.Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    StringGrid2.Canvas.TextStyle := ts;
  End;
End;

Procedure TForm8.Execute(Const ini: TIniFile; Const OwnName, SelectID: String);
Var
  i: integer;
Begin
  fOwnName := OwnName;
  fLoading := true;
  Try
    i := ini.ReadInteger('Launcher', 'StatsSortColumn', ord(scWonMatches));
    If (i >= ord(low(TStatColumn))) And (i <= ord(high(TStatColumn))) Then fSortColumn := TStatColumn(i);
    fSortDesc := ini.ReadBool('Launcher', 'StatsSortDesc', true);
    CheckBox1.Checked := ini.ReadBool('Launcher', 'StatsAllValues', false);
    CheckBox2.Checked := ini.ReadBool('Launcher', 'StatsMergeKeys', true);
    ComboBox2.ItemIndex := max(0, min(ComboBox2.Items.Count - 1, ini.ReadInteger('Launcher', 'StatsFilter', 0)));
    Width := max(Constraints.MinWidth, ini.ReadInteger('Launcher', 'StatsWidth', Width));
    Height := max(Constraints.MinHeight, ini.ReadInteger('Launcher', 'StatsHeight', Height));
    ReadSources(SelectID);
  Finally
    fLoading := false;
  End;
  LoadSource();
  ShowModal;
  ini.WriteInteger('Launcher', 'StatsSortColumn', ord(fSortColumn));
  ini.WriteBool('Launcher', 'StatsSortDesc', fSortDesc);
  ini.WriteBool('Launcher', 'StatsAllValues', CheckBox1.Checked);
  ini.WriteBool('Launcher', 'StatsMergeKeys', CheckBox2.Checked);
  ini.WriteInteger('Launcher', 'StatsFilter', ComboBox2.ItemIndex);
  ini.WriteInteger('Launcher', 'StatsWidth', Width);
  ini.WriteInteger('Launcher', 'StatsHeight', Height);
End;

End.
