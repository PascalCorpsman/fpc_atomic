(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of config_td                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Procedure AddLog(aLog: String);

Implementation

{$R *.lfm}

Uses Unit2, ucdextractor;

Procedure AddLog(aLog: String);
Begin
  form1.Memo1.Lines.Add(aLog);
  form1.Memo1.SelStart := length(Form1.Memo1.Text); // Scroll down to see newest entry
  Application.ProcessMessages;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  IniPropStorage1.IniFileName := 'settings.ini';
  (*
   * History: 0.01 = Initial version
   *)
  caption := 'FPC Atomic data extractor ver. 0.01';
  label1.caption := IniPropStorage1.ReadString('CD-Root', '');
  label2.caption := IniPropStorage1.ReadString('FPC-Atomic', '');
  memo1.clear;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  (*
   * Disable the ani job generator, it is only used for developping
   *)
  button6.Visible := false;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  showmessage('Click the buttons from top to button, carefully read error messages and solve them.' + LineEnding + LineEnding +
    'If you master to come to the last button without errors you can enjoy the game *g*.');
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  If SelectDirectoryDialog1.Execute Then Begin
    If CheckFPCAtomicFolder(SelectDirectoryDialog1.FileName) Then Begin
      label2.Caption := SelectDirectoryDialog1.FileName;
    End
    Else Begin
      showmessage('Error, the folder should at least contain the executable for fpc_atomic');
    End;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  n: QWord;
Begin
  // TODO: reactivate disabled code, was disabled during development
  n := GetTickCount64();
  // Start Extraction
  AddLog('Start');
  Addlog('  be aware the process will take some time, ...');
  // Start extraction
  If Not CheckCDRootFolder(label1.Caption) Then Begin
    AddLog('Error: invalid atomic bomberman CD-Image folder');
    exit;
  End;
  If Not CheckFPCAtomicFolder(label2.Caption) Then Begin
    AddLog('Error: invalid fpc-atomic folder');
    exit;
  End;
  If Not ForceDirectories(IncludeTrailingPathDelimiter(label2.Caption) + 'data') Then Begin
    addlog('Error, could not create data folder.');
    exit;
  End;
  //  AddLog('PCXs');
  //  ExtractAtomicPCXs(Label1.caption, label2.caption);
  AddLog('ANIs');
  ExtractAtomicAnis(Label1.caption, label2.caption);
  // TODO: Es gibt noch die .ani -> .ani convertierung
  //AddLog('Sounds'); // Fertig, getestet
  //ExtractAtomicSounds(Label1.caption, label2.caption);
  //AddLog('Shemes'); // Fertig, getestet
  //ExtractAtomicShemes(Label1.caption, label2.caption);
  n := GetTickCount64() - n;
  Addlog('Extraction took: ' + inttostr(n Div 1000) + 's');
  AddLog('Done, please check results.');
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  If label1.Caption = '' Then Begin
    showmessage('First set atomic cd root folder!.');
    exit;
  End;
  form2.AtomicRootFolder := IncludeTrailingPathDelimiter(Label1.Caption);
  form2.ShowModal;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  IniPropStorage1.WriteString('CD-Root', label1.caption);
  IniPropStorage1.WriteString('FPC-Atomic', label2.caption);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If SelectDirectoryDialog1.Execute Then Begin
    If CheckCDRootFolder(SelectDirectoryDialog1.FileName) Then Begin
      label1.Caption := SelectDirectoryDialog1.FileName;
    End
    Else Begin
      showmessage('Error, you need to give the Atomic bomberman CD-Image root folder.' +
        'That is typically the folder containing the subfolders "Data", "Data/Sound" ...');
    End;
  End;
End;

End.

