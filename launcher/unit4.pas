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
Unit Unit4;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, CheckLst, Spin, Grids, uatomic_common, ulauncher;

Type

  { TForm4 }

  (*
   * Der Mods Dialog bearbeitet atomic_server.ini, der Server liest sie beim nächsten Start.
   *
   * Links steht jeder Mod als eine Zeile mit Haken, rechts die Seite mit seinen Einstellungen.
   * Ein neuer Mod braucht: einen Eintrag in CheckListBox1, eine TabSheet mit Checkbox und Panel
   * und die beiden Einträge in fModCheckBoxes / fModPanels (FormCreate).
   *)
  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckListBox1: TCheckListBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit10: TSpinEdit;
    SpinEdit11: TSpinEdit;
    SpinEdit12: TSpinEdit;
    SpinEdit13: TSpinEdit;
    SpinEdit14: TSpinEdit;
    SpinEdit15: TSpinEdit;
    SpinEdit16: TSpinEdit;
    SpinEdit17: TSpinEdit;
    SpinEdit18: TSpinEdit;
    SpinEdit19: TSpinEdit;
    SpinEdit20: TSpinEdit;
    SpinEdit21: TSpinEdit;
    SpinEdit22: TSpinEdit;
    SpinEdit23: TSpinEdit;
    SpinEdit24: TSpinEdit;
    SpinEdit25: TSpinEdit;
    SpinEdit26: TSpinEdit;
    SpinEdit27: TSpinEdit;
    SpinEdit28: TSpinEdit;
    SpinEdit29: TSpinEdit;
    SpinEdit30: TSpinEdit;
    SpinEdit31: TSpinEdit;
    SpinEdit32: TSpinEdit;
    SpinEdit33: TSpinEdit;
    SpinEdit34: TSpinEdit;
    SpinEdit35: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    SpinEdit8: TSpinEdit;
    SpinEdit9: TSpinEdit;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    TabSheet12: TTabSheet;
    TabSheet13: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Label62Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckListBox1ClickCheck(Sender: TObject);
    Procedure CheckListBox1SelectionChange(Sender: TObject; User: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    fSettings: TModSettings;
    fModCheckBoxes: Array Of TCheckBox; // Pro Mod die Checkbox oben auf seiner Seite, Index = Index in CheckListBox1 und PageControl1
    fModPanels: Array Of TPanel; // Pro Mod seine Einstellungen, die sind nur aktiv wenn die Checkbox an ist
    Procedure SettingsToControls();
    Function ControlsToSettings(): Boolean; // False, wenn eine Eingabe ungültig ist (dann wurde schon eine Meldung angezeigt)
    Procedure UpdateModEnabled();
    Function ValidHandicapRow(aRow: integer): Boolean;
    Procedure ShowModPage(Page: TTabSheet);
  public

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses uatomic_global;

Function ValidHandicapName(aName: String): Boolean;
Begin
  // "Name~2" ist der 2. Spieler an einem PC (siehe TServer.HandleStartRound), ~ kann in einem Spielernamen nicht vorkommen
  aName := trim(aName);
  If copy(aName, length(aName) - 1, 2) = '~2' Then Begin
    aName := copy(aName, 1, length(aName) - 2);
  End;
  result := ValidNodeName(aName);
End;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Mods';
  fSettings.Handicap := Nil;
  ModSettingsDefaults(fSettings);
  // Die Reihenfolge muss der in CheckListBox1 und PageControl1 entsprechen, der Mod Test Modus ist immer der letzte
  setlength(fModCheckBoxes, 13);
  fModCheckBoxes[0] := CheckBox1;
  fModCheckBoxes[1] := CheckBox16;
  fModCheckBoxes[2] := CheckBox2;
  fModCheckBoxes[3] := CheckBox9;
  fModCheckBoxes[4] := CheckBox17;
  fModCheckBoxes[5] := CheckBox18;
  fModCheckBoxes[6] := CheckBox19;
  fModCheckBoxes[7] := CheckBox3;
  fModCheckBoxes[8] := CheckBox4;
  fModCheckBoxes[9] := CheckBox8;
  fModCheckBoxes[10] := CheckBox10;
  fModCheckBoxes[11] := CheckBox13;
  fModCheckBoxes[12] := CheckBox5;
  setlength(fModPanels, 13);
  fModPanels[0] := Panel1;
  fModPanels[1] := Panel10;
  fModPanels[2] := Panel2;
  fModPanels[3] := Panel7;
  fModPanels[4] := Panel11;
  fModPanels[5] := Panel12;
  fModPanels[6] := Panel13;
  fModPanels[7] := Panel3;
  fModPanels[8] := Panel4;
  fModPanels[9] := Panel6;
  fModPanels[10] := Panel8;
  fModPanels[11] := Panel9;
  fModPanels[12] := Panel5;
  // Die Grenzen sind die selben, die auch der Server beim Laden nutzt
  SpinEdit1.MaxValue := ModMaxStunMs;
  SpinEdit2.MaxValue := ModMaxStunMs;
  SpinEdit21.MaxValue := ModMaxStunMs;
  SpinEdit22.MaxValue := ModMaxStunMs;
  SpinEdit23.MaxValue := ModMaxPunchStunCooldownMs;
  SpinEdit3.MaxValue := ModMaxPositiveDiseaseChance;
  SpinEdit4.MaxValue := ModMaxPositiveMs;
  SpinEdit5.MaxValue := ModMaxPositiveMs;
  SpinEdit15.MaxValue := ModMaxBrickWalkChance;
  SpinEdit16.MaxValue := ModMaxBrickWalkMs;
  SpinEdit17.MaxValue := ModMaxBrickWalkMs;
  SpinEdit18.MaxValue := ModMaxBrickWalkTransparency;
  SpinEdit24.MaxValue := ModMaxFlashChance;
  SpinEdit25.MaxValue := ModMaxFlashIntensity;
  SpinEdit26.MaxValue := ModMaxFlashMs;
  SpinEdit27.MaxValue := ModMaxFlashMs;
  SpinEdit28.MaxValue := ModMaxFlashChance;
  SpinEdit29.MaxValue := ModMaxFlashIntensity;
  SpinEdit30.MaxValue := ModMaxFlashMs;
  SpinEdit31.MaxValue := ModMaxFlashMs;
  SpinEdit32.MaxValue := ModMaxFlameThrowerChance;
  SpinEdit33.MaxValue := ModMaxFlameThrowerFuelMs;
  SpinEdit34.MinValue := 1;
  SpinEdit34.MaxValue := ModMaxFlameThrowerRange;
  SpinEdit35.MaxValue := ModMaxFlameThrowerKillMs;
  SpinEdit19.MaxValue := ModMaxTeleportersActiveSec;
  SpinEdit20.MaxValue := ModMaxOpenBordersActiveSec;
  SpinEdit6.MinValue := FlameTime;
  SpinEdit6.MaxValue := ModMaxInvulnTimeMs;
  SpinEdit7.MinValue := 1;
  SpinEdit7.MaxValue := ModMaxHitPoints;
  SpinEdit8.MinValue := 1;
  SpinEdit8.MaxValue := ModMaxGhostFlameLen;
  SpinEdit9.MaxValue := ModMaxGhostReviveInvulnMs;
  SpinEdit10.MaxValue := ModMaxGhostCount;
  SpinEdit11.MaxValue := ModMaxGhostTimeMs;
  SpinEdit12.MaxValue := ModMaxGhostFlickerMs;
  SpinEdit13.MaxValue := ModMaxRoundTimeSec;
  SpinEdit14.MaxValue := ModMaxStartInvulnMs;
  StringGrid1.Cells[0, 0] := 'Player name';
  StringGrid1.Cells[1, 0] := 'Hit points';
  PageControl1.ActivePageIndex := 0;
End;

Procedure TForm4.FormDestroy(Sender: TObject);
Begin
  fSettings.Handicap.free;
  fSettings.Handicap := Nil;
End;

Procedure TForm4.FormShow(Sender: TObject);
Begin
  // Immer den aktuellen Stand der Datei zeigen, falls sie von Hand geändert wurde
  LoadModSettingsFromFile(GetAtomicServerIniFile(), fSettings);
  SettingsToControls();
  ShowModPage(PageControl1.ActivePage);
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  // Defaults
  ModSettingsDefaults(fSettings);
  SettingsToControls();
End;

Procedure TForm4.Button2Click(Sender: TObject);
Begin
  // OK
  If Not ControlsToSettings() Then exit;
  If Not SaveModSettingsToFile(GetAtomicServerIniFile(), fSettings) Then Begin
    showmessage('Error, could not write:' + LineEnding + GetAtomicServerIniFile());
    exit;
  End;
  ModalResult := mrOK;
End;

Procedure TForm4.Button4Click(Sender: TObject);
Begin
  // Neue Zeile in der Handicap Tabelle, gleich zum Bearbeiten
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  StringGrid1.Row := StringGrid1.RowCount - 1;
  StringGrid1.Col := 0;
  StringGrid1.SetFocus;
  StringGrid1.EditorMode := true;
End;

Procedure TForm4.Button5Click(Sender: TObject);
Begin
  // Markierte Zeile aus der Handicap Tabelle löschen
  If StringGrid1.Row >= StringGrid1.FixedRows Then Begin
    StringGrid1.DeleteRow(StringGrid1.Row);
  End;
End;

Procedure TForm4.Label62Click(Sender: TObject);
Begin
  OpenManual('MODS_MANUAL.md');
End;

Procedure TForm4.CheckBox1Change(Sender: TObject);
Begin
  // Gemeinsam für die Checkboxen CheckBox1..5 oben auf den Seiten
  UpdateModEnabled();
End;

Procedure TForm4.CheckListBox1ClickCheck(Sender: TObject);
Var
  i: integer;
Begin
  // Der Haken in der Liste schaltet den Mod genau so wie die Checkbox auf seiner Seite
  For i := 0 To high(fModCheckBoxes) Do Begin
    fModCheckBoxes[i].Checked := CheckListBox1.Checked[i];
  End;
End;

Procedure TForm4.CheckListBox1SelectionChange(Sender: TObject; User: boolean);
Begin
  If CheckListBox1.ItemIndex >= 0 Then Begin
    PageControl1.ActivePageIndex := CheckListBox1.ItemIndex;
  End;
End;

Procedure TForm4.StringGrid1PrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
Begin
  // Ungültige Handicap Einträge rot, die werden beim OK abgelehnt
  If (aRow >= StringGrid1.FixedRows) And (Not ValidHandicapRow(aRow)) Then Begin
    StringGrid1.Canvas.Font.Color := clRed;
  End;
End;

Procedure TForm4.SettingsToControls;
Var
  i: integer;
Begin
  CheckBox1.Checked := fSettings.StunOnBombLanding;
  SpinEdit1.Value := fSettings.StunMinMs;
  SpinEdit2.Value := fSettings.StunMaxMs;

  CheckBox16.Checked := fSettings.PunchStun;
  SpinEdit21.Value := fSettings.PunchStunMinMs;
  SpinEdit22.Value := fSettings.PunchStunMaxMs;
  SpinEdit23.Value := fSettings.PunchStunCooldownMs;

  CheckBox2.Checked := fSettings.PositiveDisease;
  SpinEdit3.Value := fSettings.PositiveDiseaseChance;
  SpinEdit4.Value := fSettings.PositiveMinMs;
  SpinEdit5.Value := fSettings.PositiveMaxMs;

  CheckBox9.Checked := fSettings.BrickWalk;
  SpinEdit15.Value := fSettings.BrickWalkChance;
  SpinEdit16.Value := fSettings.BrickWalkMinMs;
  SpinEdit17.Value := fSettings.BrickWalkMaxMs;
  SpinEdit18.Value := fSettings.BrickWalkTransparency;

  CheckBox17.Checked := fSettings.FlashDisease;
  SpinEdit24.Value := fSettings.FlashDiseaseChance;
  SpinEdit25.Value := fSettings.FlashDiseaseIntensity;
  SpinEdit26.Value := fSettings.FlashDiseaseMs;
  SpinEdit27.Value := fSettings.FlashDiseaseFadeMs;

  CheckBox18.Checked := fSettings.WhiteBomb;
  SpinEdit28.Value := fSettings.WhiteBombChance;
  SpinEdit29.Value := fSettings.WhiteBombIntensity;
  SpinEdit30.Value := fSettings.WhiteBombMs;
  SpinEdit31.Value := fSettings.WhiteBombFadeMs;

  CheckBox19.Checked := fSettings.FlameThrower;
  SpinEdit32.Value := fSettings.FlameThrowerChance;
  SpinEdit33.Value := fSettings.FlameThrowerFuelMs;
  SpinEdit34.Value := fSettings.FlameThrowerRange;
  SpinEdit35.Value := fSettings.FlameThrowerKillMs;
  CheckBox20.Checked := fSettings.FlameThrowerBurnItems;

  CheckBox3.Checked := fSettings.HitPoints;
  SpinEdit6.Value := fSettings.InvulnTimeMs;
  SpinEdit7.Value := fSettings.DefaultHitPoints;
  CheckBox6.Checked := fSettings.ShowHitPoints;
  StringGrid1.RowCount := StringGrid1.FixedRows + fSettings.Handicap.Count;
  For i := 0 To fSettings.Handicap.Count - 1 Do Begin
    StringGrid1.Cells[0, StringGrid1.FixedRows + i] := fSettings.Handicap.Names[i];
    StringGrid1.Cells[1, StringGrid1.FixedRows + i] := fSettings.Handicap.ValueFromIndex[i];
  End;

  CheckBox4.Checked := fSettings.Ghosts;
  SpinEdit8.Value := fSettings.GhostFlameLen;
  SpinEdit9.Value := fSettings.GhostReviveInvulnMs;
  SpinEdit10.Value := fSettings.GhostMaxCount;
  SpinEdit11.Value := fSettings.GhostMaxTimeMs;
  SpinEdit12.Value := fSettings.GhostFlickerMs;

  CheckBox8.Checked := fSettings.GloveAtStart;

  CheckBox10.Checked := fSettings.OpenBorders;
  CheckBox11.Checked := fSettings.OpenBordersTopBottom;
  CheckBox12.Checked := fSettings.OpenBordersLeftRight;
  SpinEdit20.Value := fSettings.OpenBordersActiveSec;

  CheckBox13.Checked := fSettings.Teleporters;
  CheckBox14.Checked := fSettings.TeleportersTopBottom;
  CheckBox15.Checked := fSettings.TeleportersLeftRight;
  SpinEdit19.Value := fSettings.TeleportersActiveSec;

  CheckBox5.Checked := fSettings.ModTestMode;
  Edit1.Text := fSettings.StartItems;
  SpinEdit13.Value := fSettings.RoundTimeSec;
  SpinEdit14.Value := fSettings.StartInvulnMs;
  CheckBox7.Checked := fSettings.GhostsKeepRound;
  UpdateModEnabled();
End;

Function TForm4.ControlsToSettings: Boolean;
Var
  i, j: integer;
  n: String;
Begin
  result := false;
  If SpinEdit2.Value < SpinEdit1.Value Then Begin
    ShowModPage(TabSheet1);
    showmessage('Error, max. stun time is smaller than min. stun time.');
    exit;
  End;
  If SpinEdit22.Value < SpinEdit21.Value Then Begin
    ShowModPage(TabSheet10);
    showmessage('Error, max. stun time is smaller than min. stun time.');
    exit;
  End;
  If SpinEdit5.Value < SpinEdit4.Value Then Begin
    ShowModPage(TabSheet2);
    showmessage('Error, max. time is smaller than min. time.');
    exit;
  End;
  If SpinEdit17.Value < SpinEdit16.Value Then Begin
    ShowModPage(TabSheet7);
    showmessage('Error, max. time is smaller than min. time.');
    exit;
  End;
  // Die Handicap Tabelle, leere Zeilen werden ignoriert
  For i := StringGrid1.FixedRows To StringGrid1.RowCount - 1 Do Begin
    If Not ValidHandicapRow(i) Then Begin
      ShowModPage(TabSheet3);
      StringGrid1.Row := i;
      showmessage('Error, invalid handicap in line ' + inttostr(i) + ':' + LineEnding +
        'The player name needs to be a valid name (optional with ~2 at the end) and the hit points need to be in 1..' + inttostr(ModMaxHitPoints) + '.');
      exit;
    End;
    n := trim(StringGrid1.Cells[0, i]);
    For j := StringGrid1.FixedRows To i - 1 Do Begin
      If (n <> '') And (lowercase(n) = lowercase(trim(StringGrid1.Cells[0, j]))) Then Begin
        ShowModPage(TabSheet3);
        StringGrid1.Row := i;
        showmessage('Error, the player name "' + n + '" is used twice.');
        exit;
      End;
    End;
  End;

  fSettings.StunOnBombLanding := CheckBox1.Checked;
  fSettings.StunMinMs := SpinEdit1.Value;
  fSettings.StunMaxMs := SpinEdit2.Value;

  fSettings.PunchStun := CheckBox16.Checked;
  fSettings.PunchStunMinMs := SpinEdit21.Value;
  fSettings.PunchStunMaxMs := SpinEdit22.Value;
  fSettings.PunchStunCooldownMs := SpinEdit23.Value;

  fSettings.PositiveDisease := CheckBox2.Checked;
  fSettings.PositiveDiseaseChance := SpinEdit3.Value;
  fSettings.PositiveMinMs := SpinEdit4.Value;
  fSettings.PositiveMaxMs := SpinEdit5.Value;

  fSettings.BrickWalk := CheckBox9.Checked;
  fSettings.BrickWalkChance := SpinEdit15.Value;
  fSettings.BrickWalkMinMs := SpinEdit16.Value;
  fSettings.BrickWalkMaxMs := SpinEdit17.Value;
  fSettings.BrickWalkTransparency := SpinEdit18.Value;

  fSettings.FlashDisease := CheckBox17.Checked;
  fSettings.FlashDiseaseChance := SpinEdit24.Value;
  fSettings.FlashDiseaseIntensity := SpinEdit25.Value;
  fSettings.FlashDiseaseMs := SpinEdit26.Value;
  fSettings.FlashDiseaseFadeMs := SpinEdit27.Value;

  fSettings.WhiteBomb := CheckBox18.Checked;
  fSettings.WhiteBombChance := SpinEdit28.Value;
  fSettings.WhiteBombIntensity := SpinEdit29.Value;
  fSettings.WhiteBombMs := SpinEdit30.Value;
  fSettings.WhiteBombFadeMs := SpinEdit31.Value;

  fSettings.FlameThrower := CheckBox19.Checked;
  fSettings.FlameThrowerChance := SpinEdit32.Value;
  fSettings.FlameThrowerFuelMs := SpinEdit33.Value;
  fSettings.FlameThrowerRange := SpinEdit34.Value;
  fSettings.FlameThrowerKillMs := SpinEdit35.Value;
  fSettings.FlameThrowerBurnItems := CheckBox20.Checked;

  fSettings.HitPoints := CheckBox3.Checked;
  fSettings.InvulnTimeMs := SpinEdit6.Value;
  fSettings.DefaultHitPoints := SpinEdit7.Value;
  fSettings.ShowHitPoints := CheckBox6.Checked;
  fSettings.Handicap.Clear;
  For i := StringGrid1.FixedRows To StringGrid1.RowCount - 1 Do Begin
    n := trim(StringGrid1.Cells[0, i]);
    If n <> '' Then Begin
      fSettings.Handicap.Add(n + '=' + inttostr(StrToInt(trim(StringGrid1.Cells[1, i]))));
    End;
  End;

  fSettings.Ghosts := CheckBox4.Checked;
  fSettings.GhostFlameLen := SpinEdit8.Value;
  fSettings.GhostReviveInvulnMs := SpinEdit9.Value;
  fSettings.GhostMaxCount := SpinEdit10.Value;
  fSettings.GhostMaxTimeMs := SpinEdit11.Value;
  fSettings.GhostFlickerMs := SpinEdit12.Value;

  fSettings.GloveAtStart := CheckBox8.Checked;

  fSettings.OpenBorders := CheckBox10.Checked;
  fSettings.OpenBordersTopBottom := CheckBox11.Checked;
  fSettings.OpenBordersLeftRight := CheckBox12.Checked;
  fSettings.OpenBordersActiveSec := SpinEdit20.Value;

  fSettings.Teleporters := CheckBox13.Checked;
  fSettings.TeleportersTopBottom := CheckBox14.Checked;
  fSettings.TeleportersLeftRight := CheckBox15.Checked;
  fSettings.TeleportersActiveSec := SpinEdit19.Value;

  fSettings.ModTestMode := CheckBox5.Checked;
  fSettings.StartItems := trim(Edit1.Text);
  fSettings.RoundTimeSec := SpinEdit13.Value;
  fSettings.StartInvulnMs := SpinEdit14.Value;
  fSettings.GhostsKeepRound := CheckBox7.Checked;
  result := true;
End;

Procedure TForm4.UpdateModEnabled;
Var
  i, j: integer;
Begin
  // Die Einstellungen eines Mods sind nur aktiv, wenn er an ist, die Liste links zeigt das gleiche wie die Checkboxen
  For i := 0 To high(fModCheckBoxes) Do Begin
    CheckListBox1.Checked[i] := fModCheckBoxes[i].Checked;
    For j := 0 To fModPanels[i].ControlCount - 1 Do Begin
      fModPanels[i].Controls[j].Enabled := fModCheckBoxes[i].Checked;
    End;
  End;
End;

Function TForm4.ValidHandicapRow(aRow: integer): Boolean;
Var
  n, v: String;
Begin
  n := trim(StringGrid1.Cells[0, aRow]);
  v := trim(StringGrid1.Cells[1, aRow]);
  If (n = '') And (v = '') Then Begin
    result := true; // Leere Zeile, wird beim Speichern ignoriert
    exit;
  End;
  result := ValidHandicapName(n) And (StrToIntDef(v, 0) >= 1) And (StrToIntDef(v, 0) <= ModMaxHitPoints);
End;

Procedure TForm4.ShowModPage(Page: TTabSheet);
Begin
  PageControl1.ActivePage := Page;
  CheckListBox1.ItemIndex := Page.PageIndex;
End;

End.
