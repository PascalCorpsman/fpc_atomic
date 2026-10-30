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
Unit Unit6;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Spin, uatomic_common, ulauncher;

Type

  { TForm6 }

  (*
   * Der Dialog Spielmodus bearbeitet die Abschnitte [GameMode] und [Hunter] in atomic_mods.ini,
   * der Client schickt sie als Master zusammen mit den Mods an den Server.
   *
   * Links stehen die Spielmodi, der markierte ist der aktive (es ist immer genau einer aktiv),
   * rechts die Seite mit seinen Einstellungen. Die Reihenfolge der Seiten in PageControl1 ist die von TGameMode,
   * die Einträge in ListBox1 sind die (übersetzten) Captions der Seiten.
   *)
  TForm6 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    ListBox1: TListBox;
    PageControl1: TPageControl;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Label16Click(Sender: TObject);
    Procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    fSettings: TModSettings;
    Procedure SettingsToControls();
    Procedure ControlsToSettings();
    Procedure UpdateEnabled();
    Procedure ShowModePage(Mode: TGameMode);
  public
    TeamPlay: Boolean; // Aus den Spiel Optionen (fpc_atomic.cfg), setzt Form1 vor ShowModal, nur für den Hinweis unten
    Function GameModeName(Mode: TGameMode): String; // Der übersetzte Name des Spielmodus (Caption seiner Seite)
  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

Uses uatomic_global, ulanguage, uitemsedit;

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Game mode';
{$IFDEF Windows}
  // Ohne Tabs zeichnet Windows den Rahmen von PageControl1 2 px innerhalb seiner Grenzen, die Liste daran ausrichten
  ListBox1.SetBounds(ListBox1.Left, PageControl1.Top + 2, ListBox1.Width, PageControl1.Height - 3);
{$ENDIF}
  TeamPlay := false;
  fSettings.Handicap := Nil;
  ModSettingsDefaults(fSettings);
  // Die Grenzen sind die selben, die auch der Server beim Laden nutzt
  SpinEdit1.MaxValue := ModMaxHuntedInvulnMs;
  SpinEdit2.MaxValue := ModMaxHunterRespawnMs;
  SpinEdit3.MaxValue := ModMaxHunterRespawnInvulnMs;
  PageControl1.ActivePageIndex := 0;
  // Der "+" Button fügt Items in Edit1 ein
  TItemsEditMenu.Create(Button4, Edit1);
End;

Procedure TForm6.FormDestroy(Sender: TObject);
Begin
  fSettings.Handicap.free;
  fSettings.Handicap := Nil;
End;

Procedure TForm6.FormShow(Sender: TObject);
Var
  gm: TGameMode;
Begin
  // Die Namen der Modi erst hier, damit sie in der aktuellen Sprache sind
  ListBox1.Items.Clear;
  For gm In TGameMode Do Begin
    ListBox1.Items.Add(GameModeName(gm));
  End;
  // Immer den aktuellen Stand der Datei zeigen, falls sie von Hand oder im Mods Dialog geändert wurde
  LoadModSettingsFromFile(GetAtomicModsIniFile(), fSettings);
  SettingsToControls();
End;

Function TForm6.GameModeName(Mode: TGameMode): String;
Begin
  result := PageControl1.Pages[ord(Mode)].Caption;
End;

Procedure TForm6.Button1Click(Sender: TObject);
Var
  Defaults: TModSettings;
Begin
  // Defaults, aber nur für den Spielmodus, die Mods in der selben Datei bleiben wie sie sind
  Defaults.Handicap := Nil;
  ModSettingsDefaults(Defaults);
  CopyGameModeSettings(Defaults, fSettings);
  Defaults.Handicap.free;
  SettingsToControls();
End;

Procedure TForm6.Button2Click(Sender: TObject);
Begin
  // OK
  ControlsToSettings();
  If Not SaveModSettingsToFile(GetAtomicModsIniFile(), fSettings) Then Begin
    showmessage(R_WriteFailed + LineEnding + GetAtomicModsIniFile());
    exit;
  End;
  ModalResult := mrOK;
End;

Procedure TForm6.CheckBox1Change(Sender: TObject);
Begin
  // Gemeinsam für Respawn und Aura, die schalten Einstellungen frei
  UpdateEnabled();
End;

Procedure TForm6.Label16Click(Sender: TObject);
Begin
  If SameText(ExtractFileExt(Language.CurrentLanguage), '.de') Then Begin
    OpenManual('GAMEOPTIONS_MANUAL_DE.md');
  End
  Else Begin
    OpenManual('GAMEOPTIONS_MANUAL.md');
  End;
End;

Procedure TForm6.ListBox1SelectionChange(Sender: TObject; User: boolean);
Begin
  // Der markierte Eintrag ist der aktive Spielmodus
  If ListBox1.ItemIndex >= 0 Then Begin
    PageControl1.ActivePageIndex := ListBox1.ItemIndex;
  End;
  UpdateEnabled();
End;

Procedure TForm6.SettingsToControls;
Begin
  Edit1.Text := fSettings.HuntedItems;
  ComboBox1.ItemIndex := fSettings.HuntedStart;
  ComboBox2.ItemIndex := fSettings.HuntedLost;
  ComboBox3.ItemIndex := fSettings.HunterScoring;
  ComboBox4.ItemIndex := fSettings.HuntedLoseItems;
  SpinEdit1.Value := fSettings.HuntedInvulnMs;
  CheckBox1.Checked := fSettings.HunterRespawn;
  SpinEdit2.Value := fSettings.HunterRespawnMs;
  SpinEdit3.Value := fSettings.HunterRespawnInvulnMs;
  CheckBox2.Checked := fSettings.HuntedNoDiseases;
  CheckBox3.Checked := fSettings.HuntedGold;
  CheckBox4.Checked := fSettings.HuntedAura;
  CheckBox5.Checked := fSettings.HuntedAuraSelf;
  ShowModePage(fSettings.GameMode);
End;

Procedure TForm6.ControlsToSettings;
Begin
  // Die Werte der Seite Hunter werden auch gespeichert, wenn Klassisch aktiv ist, damit sie beim nächsten Mal noch da sind
  If ListBox1.ItemIndex >= 0 Then Begin
    fSettings.GameMode := TGameMode(ListBox1.ItemIndex);
  End;
  fSettings.HuntedItems := trim(Edit1.Text);
  fSettings.HuntedStart := ComboBox1.ItemIndex;
  fSettings.HuntedLost := ComboBox2.ItemIndex;
  fSettings.HunterScoring := ComboBox3.ItemIndex;
  fSettings.HuntedLoseItems := ComboBox4.ItemIndex;
  fSettings.HuntedInvulnMs := SpinEdit1.Value;
  fSettings.HunterRespawn := CheckBox1.Checked;
  fSettings.HunterRespawnMs := SpinEdit2.Value;
  fSettings.HunterRespawnInvulnMs := SpinEdit3.Value;
  fSettings.HuntedNoDiseases := CheckBox2.Checked;
  fSettings.HuntedGold := CheckBox3.Checked;
  fSettings.HuntedAura := CheckBox4.Checked;
  fSettings.HuntedAuraSelf := CheckBox5.Checked;
End;

Procedure TForm6.UpdateEnabled;
Var
  Hunter: Boolean;
Begin
  // Wartezeit und Unverwundbarkeit gibt es nur mit Respawn, die eigene Aura nur mit Aura
  Label11.Enabled := CheckBox1.Checked;
  SpinEdit2.Enabled := CheckBox1.Checked;
  Label12.Enabled := CheckBox1.Checked;
  SpinEdit3.Enabled := CheckBox1.Checked;
  CheckBox5.Enabled := CheckBox4.Checked;
  // Die Hinweise unten betreffen nur den Hunter Modus
  Hunter := ListBox1.ItemIndex = ord(gmHunter);
  Label13.Visible := Hunter And TeamPlay;
  Label14.Visible := Hunter And fSettings.Ghosts And CheckBox1.Checked;
End;

Procedure TForm6.ShowModePage(Mode: TGameMode);
Begin
  PageControl1.ActivePageIndex := ord(Mode);
  ListBox1.ItemIndex := ord(Mode);
  UpdateEnabled();
End;

End.
