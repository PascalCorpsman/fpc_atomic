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
Unit ukeyboarddialog;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, forms, StdCtrls, Controls, ukeygrabber, uatomic_common;

Type

  { TKeyboardDialog }

  TKeyboardDialog = Class(TForm)
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
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    KeyGrabber1: TButton;
    KeyGrabber2: TButton;
    KeyGrabber3: TButton;
    KeyGrabber4: TButton;
    KeyGrabber5: TButton;
    KeyGrabber6: TButton;
    KeyGrabber7: TButton;
    KeyGrabber8: TButton;
    KeyGrabber9: TButton;
    KeyGrabber10: TButton;
    KeyGrabber11: TButton;
    KeyGrabber12: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Procedure OnKeyGrabberEvent(Sender: TObject);
    Procedure OnSetDefaultEvent(Sender: TObject);
  private
    fkeys: Array[TKeySet] Of TKeys;
  public
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    Procedure LoadKeys(Keys0, keys1: Tkeys);
    Function GetKeys(Index: TKeySet): TKeys;
    Function Execute(): Boolean; // True, wenn Erfolgreich ausgeführt
  End;

Implementation

{ TKeyboardDialog }

Procedure TKeyboardDialog.OnKeyGrabberEvent(Sender: TObject);
Var
  kg: TKeyGrabber;
  key: word;
  index: Integer;
Begin
  kg := TKeyGrabber.CreateNew(Nil);
  key := kg.GrabVKKey();
  kg.free;
  index := Tbutton(Sender).Tag;
  Case index Of
    1: fkeys[ks0].KeyUp := key;
    2: fkeys[ks0].KeyDown := key;
    3: fkeys[ks0].KeyLeft := key;
    4: fkeys[ks0].KeyRight := key;
    5: fkeys[ks0].KeyPrimary := key;
    6: fkeys[ks0].KeySecondary := key;
    7: fkeys[ks1].KeyUp := key;
    8: fkeys[ks1].KeyDown := key;
    9: fkeys[ks1].KeyLeft := key;
    10: fkeys[ks1].KeyRight := key;
    11: fkeys[ks1].KeyPrimary := key;
    12: fkeys[ks1].KeySecondary := key;
  End;
  LoadKeys(fkeys[ks0], fkeys[ks1]);
End;

Procedure TKeyboardDialog.OnSetDefaultEvent(Sender: TObject);
Begin
  LoadKeys(AtomicDefaultKeys(ks0), AtomicDefaultKeys(ks1));
End;

Constructor TKeyboardDialog.CreateNew(AOwner: TComponent; Num: Integer);
  Procedure CreateLabel(Var lb: TLabel; aName, acaption: String; aTop, aleft: integer);
  Begin
    lb := TLabel.Create(self);
    lb.Name := aName;
    lb.Parent := self;
    lb.Caption := acaption;
    lb.Left := aleft;
    lb.Top := aTop;
  End;

  Procedure CreateKeyGrabber(Var kg: TButton; aName: String; aTop, aLeft, aTag: integer);
  Begin
    kg := TButton.Create(self);
    kg.Caption := 'Grab';
    kg.Name := aName;
    kg.Parent := self;
    kg.Top := aTop;
    kg.Left := aLeft;
    kg.Tag := aTag;
    kg.OnClick := @OnKeyGrabberEvent;
  End;

  Procedure CreateButton(Var bt: TButton; aName, aCaption: String; aTop, aLeft: Integer; aResult: TModalResult);
  Begin
    bt := TButton.Create(self);
    bt.Name := aName;
    bt.Parent := Self;
    bt.Caption := aCaption;
    bt.ModalResult := aResult;
    bt.Left := aLeft;
    bt.Top := aTop;
  End;

Begin
  Inherited CreateNew(AOwner, Num);
  width := 600;
  height := 250;
  BorderStyle := bsDialog;
  FormStyle := fsNormal;
  Position := poScreenCenter;
  Caption := 'Keyboard settings';

  CreateLabel(label25, 'Label25', 'Keyboard 0', 0, 10);
  CreateLabel(label1, 'Label1', 'Key Up:', 20, 10);
  CreateLabel(label2, 'Label2', 'Key Down:', 50, 10);
  CreateLabel(label3, 'Label3', 'Key Left:', 80, 10);
  CreateLabel(label4, 'Label4', 'Key Right:', 110, 10);
  CreateLabel(label5, 'Label5', 'Key Primary:', 140, 10);
  CreateLabel(label6, 'Label6', 'Key Secondary:', 170, 10);
  CreateLabel(label7, 'Label7', '?', 20, 110);
  CreateLabel(label8, 'Label8', '?', 50, 110);
  CreateLabel(label9, 'Label9', '?', 80, 110);
  CreateLabel(label10, 'Label10', '?', 110, 110);
  CreateLabel(label11, 'Label11', '?', 140, 110);
  CreateLabel(label12, 'Label12', '?', 170, 110);
  CreateKeyGrabber(KeyGrabber1, 'KeyGrabber1', 20, 200, 1);
  CreateKeyGrabber(KeyGrabber2, 'KeyGrabber2', 50, 200, 2);
  CreateKeyGrabber(KeyGrabber3, 'KeyGrabber3', 80, 200, 3);
  CreateKeyGrabber(KeyGrabber4, 'KeyGrabber4', 110, 200, 4);
  CreateKeyGrabber(KeyGrabber5, 'KeyGrabber5', 140, 200, 5);
  CreateKeyGrabber(KeyGrabber6, 'KeyGrabber6', 170, 200, 6);


  CreateLabel(label26, 'Label26', 'Keyboard 1', 0, 310);
  CreateLabel(label13, 'Label13', 'Key Up:', 20, 310);
  CreateLabel(label14, 'Label14', 'Key Down:', 50, 310);
  CreateLabel(label15, 'Label15', 'Key Left:', 80, 310);
  CreateLabel(label16, 'Label16', 'Key Right:', 110, 310);
  CreateLabel(label17, 'Label17', 'Key Primary:', 140, 310);
  CreateLabel(label18, 'Label18', 'Key Secondary:', 170, 310);
  CreateLabel(label19, 'Label19', '?', 20, 410);
  CreateLabel(label20, 'Label20', '?', 50, 410);
  CreateLabel(label21, 'Label21', '?', 80, 410);
  CreateLabel(label22, 'Label22', '?', 110, 410);
  CreateLabel(label23, 'Label23', '?', 140, 410);
  CreateLabel(label24, 'Label24', '?', 170, 410);
  CreateKeyGrabber(KeyGrabber7, 'KeyGrabber7', 20, 500, 7);
  CreateKeyGrabber(KeyGrabber8, 'KeyGrabber8', 50, 500, 8);
  CreateKeyGrabber(KeyGrabber9, 'KeyGrabber9', 80, 500, 9);
  CreateKeyGrabber(KeyGrabber10, 'KeyGrabber10', 110, 500, 10);
  CreateKeyGrabber(KeyGrabber11, 'KeyGrabber11', 140, 500, 11);
  CreateKeyGrabber(KeyGrabber12, 'KeyGrabber12', 170, 500, 12);

  CreateButton(Button1, 'Button1', 'OK', 210, 505, mrOK);
  CreateButton(Button2, 'Button2', 'Cancel', 210, 420, mrCancel);
  CreateButton(Button3, 'Button3', 'default', 210, 10, mrNone);

  button3.OnClick := @OnSetDefaultEvent;
End;

Procedure TKeyboardDialog.LoadKeys(Keys0, keys1: Tkeys);
Begin
  fkeys[ks0] := Keys0;
  fkeys[ks1] := Keys1;
  label7.Caption := VkKeyToString(fkeys[ks0].KeyUp);
  label8.Caption := VkKeyToString(fkeys[ks0].KeyDown);
  label9.Caption := VkKeyToString(fkeys[ks0].KeyLeft);
  label10.Caption := VkKeyToString(fkeys[ks0].KeyRight);
  label11.Caption := VkKeyToString(fkeys[ks0].KeyPrimary);
  label12.Caption := VkKeyToString(fkeys[ks0].KeySecondary);
  label19.Caption := VkKeyToString(fkeys[ks1].KeyUp);
  label20.Caption := VkKeyToString(fkeys[ks1].KeyDown);
  label21.Caption := VkKeyToString(fkeys[ks1].KeyLeft);
  label22.Caption := VkKeyToString(fkeys[ks1].KeyRight);
  label23.Caption := VkKeyToString(fkeys[ks1].KeyPrimary);
  label24.Caption := VkKeyToString(fkeys[ks1].KeySecondary);
End;

Function TKeyboardDialog.GetKeys(Index: TKeySet): TKeys;
Begin
  result := fkeys[Index];
End;

Function TKeyboardDialog.Execute: Boolean;
Begin
  result := false;
  ModalResult := mrNone;
  ShowModal;
  result := ModalResult = mrOK;
End;

End.

