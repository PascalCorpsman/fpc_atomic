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
Unit Unit5;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  IniFiles;

Type

  { TForm5 }

  (*
   * Die Spiel Optionen, die man auch im Optionen Menü des Spiels einstellen kann (gleiche Schlüssel in fpc_atomic.cfg).
   * Was schon im Hauptfenster des Launchers steht (Vollbild, Sound, FPS, Name, Tasten), fehlt hier absichtlich.
   *)
  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    Procedure SetPlayTime(Value: integer);
    Function GetPlayTime(): integer;
    Procedure SetScheme(Const Value: String);
  public
    Procedure LoadFromIni(Const ini: TIniFile);
    Procedure SaveToIni(Const ini: TIniFile);
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Uses ulauncher, math;

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Var
  sr: TSearchRec;
  sl: TStringList;
  i: integer;
Begin
  caption := 'Game options';
  // Die Spielzeit wie im Spiel: unendlich oder 90 .. 600 s in 30 s Schritten
  ComboBox3.Items.Clear;
  ComboBox3.Items.Add('Infinity');
  For i := 3 To 20 Do Begin
    ComboBox3.Items.Add(format('%d:%0.2d', [(i * 30) Div 60, (i * 30) Mod 60]));
  End;
  // Alle Schemes, die im Spiel zur Auswahl stehen
  sl := TStringList.Create;
  sl.Sorted := true;
  If FindFirst('data' + PathDelim + 'schemes' + PathDelim + '*', faAnyFile, sr) = 0 Then Begin
    Repeat
      If uppercase(ExtractFileExt(sr.Name)) = '.SCH' Then sl.Add(sr.Name);
    Until FindNext(sr) <> 0;
  End;
  FindClose(sr);
  ComboBox2.Items.Assign(sl);
  sl.free;
End;

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  // Defaults, die selben wie im Spiel (client/unit1.pas Load_Atomic_Settings)
  CheckBox1.Checked := false;
  CheckBox2.Checked := true;
  CheckBox3.Checked := false;
  SetScheme('BASIC.SCH');
  SetPlayTime(2 * 60 + 30);
  SpinEdit2.Value := 3;
  ComboBox1.ItemIndex := 1;
  SpinEdit1.Value := 5521;
  SpinEdit3.Value := 100;
  CheckBox4.Checked := true;
End;

Procedure TForm5.SetPlayTime(Value: integer);
Begin
  // 0 = unendlich, sonst der nächste Eintrag in 30 s Schritten
  If Value <= 0 Then Begin
    ComboBox3.ItemIndex := 0;
  End
  Else Begin
    ComboBox3.ItemIndex := min(18, max(1, round(Value / 30) - 2));
  End;
End;

Function TForm5.GetPlayTime: integer;
Begin
  If ComboBox3.ItemIndex <= 0 Then Begin
    result := 0;
  End
  Else Begin
    result := (ComboBox3.ItemIndex + 2) * 30;
  End;
End;

Procedure TForm5.SetScheme(Const Value: String);
Begin
  // Ein Scheme, das es nicht (mehr) gibt, bleibt trotzdem auswählbar, damit nichts verloren geht
  If ComboBox2.Items.IndexOf(Value) = -1 Then ComboBox2.Items.Add(Value);
  ComboBox2.ItemIndex := ComboBox2.Items.IndexOf(Value);
End;

Procedure TForm5.LoadFromIni(Const ini: TIniFile);
Begin
  CheckBox1.Checked := ini.ReadBool(FPC_AtomicIniSection, 'TeamPlay', false);
  CheckBox2.Checked := ini.ReadBool(FPC_AtomicIniSection, 'RandomStart', true);
  CheckBox3.Checked := ini.ReadBool(FPC_AtomicIniSection, 'LostPlayersRevertToAI', false);
  SetScheme(ini.ReadString(FPC_AtomicIniSection, 'SchemeFile', 'BASIC.SCH'));
  SetPlayTime(ini.ReadInteger(FPC_AtomicIniSection, 'PlayTime', 2 * 60 + 30));
  SpinEdit2.Value := ini.ReadInteger(FPC_AtomicIniSection, 'LastWinsToWinMatch', 3);
  ComboBox1.ItemIndex := min(2, max(0, ini.ReadInteger(FPC_AtomicIniSection, 'Conveyorspeed', 1)));
  SpinEdit1.Value := ini.ReadInteger(FPC_AtomicIniSection, 'Port', 5521);
  SpinEdit3.Value := ini.ReadInteger(FPC_AtomicIniSection, 'Volume', 10000) Div 100;
  CheckBox4.Checked := ini.ReadBool(FPC_AtomicIniSection, 'PlaySoundEffects', true);
End;

Procedure TForm5.SaveToIni(Const ini: TIniFile);
Begin
  ini.WriteBool(FPC_AtomicIniSection, 'TeamPlay', CheckBox1.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'RandomStart', CheckBox2.Checked);
  ini.WriteBool(FPC_AtomicIniSection, 'LostPlayersRevertToAI', CheckBox3.Checked);
  ini.WriteString(FPC_AtomicIniSection, 'SchemeFile', ComboBox2.Text);
  ini.WriteInteger(FPC_AtomicIniSection, 'PlayTime', GetPlayTime());
  ini.WriteInteger(FPC_AtomicIniSection, 'LastWinsToWinMatch', SpinEdit2.Value);
  ini.WriteInteger(FPC_AtomicIniSection, 'Conveyorspeed', ComboBox1.ItemIndex);
  ini.WriteInteger(FPC_AtomicIniSection, 'Port', SpinEdit1.Value);
  // Das Spiel ändert die Lautstärke in 5 % Schritten (0 .. 10000)
  ini.WriteInteger(FPC_AtomicIniSection, 'Volume', SpinEdit3.Value * 100);
  ini.WriteBool(FPC_AtomicIniSection, 'PlaySoundEffects', CheckBox4.Checked);
End;

End.
