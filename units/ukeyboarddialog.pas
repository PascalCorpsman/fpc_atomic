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
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Procedure OnKeyGrabberEvent(Sender: TObject);
    Procedure OnSetDefaultEvent(Sender: TObject);
    Procedure OnSetKeyBoardEvent(Sender: TObject);
    Procedure OnSetSDLKeyEvent(Sender: TObject);
  private
    fkeys: Array[TKeySet] Of TKeys;
  public
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    Procedure LoadKeys(Keys0, keys1: Tkeys);
    Function GetKeys(Index: TKeySet): TKeys;
    Function Execute(): Boolean; // True, wenn Erfolgreich ausgeführt
  End;

Implementation

Uses SDL2, Dialogs, usdlwizzard;

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

Procedure TKeyboardDialog.OnSetKeyBoardEvent(Sender: TObject);
Begin
  If Sender = Button5 Then Begin
    fkeys[ks0].UseSDL2 := false;
  End
  Else Begin
    fkeys[ks1].UseSDL2 := false;
  End;
End;

Procedure TKeyboardDialog.OnSetSDLKeyEvent(Sender: TObject);
Var
  ver: TSDL_Version;
  w: TSDLWizzard;
  SDL_DeviceInfo: TDeviceInfo;
Begin
  If Not SDL_LoadLib('') Then Begin
    showmessage('Error, could not load sdl2 lib.' + LineEnding +
      'Please'
{$IFDEF Windows}
      + ' download sdl2.dll and store it into fpc_atomic root folder'
{$ELSE}
      + ' run' + LineEnding + LineEnding + 'sudo aptitude install libsdl2-dev'
{$ENDIF}
      + LineEnding + LineEnding +
      'and retry.'
      );
    exit;
  End;
  If SDL_Init(SDL_INIT_JOYSTICK) <> 0 Then Begin
    showmessage('Error could not init SDL');
    exit;
  End;
  SDL_GetVersion(@ver);
  If SDL_VERSIONNUM(ver.major, ver.minor, ver.patch) < SDL_COMPILEDVERSION Then Begin
    showmessage(format('The version of sdl2 (%d.%d.%d) is to low you have to have at least %d.%d.%d', [ver.major, ver.minor, ver.patch, SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL]));
    exit;
  End;
  w := TSDLWizzard.CreateNew(self);
  If w.execute Then Begin
    SDL_DeviceInfo := w.Device;
    If sender = button4 Then Begin
      fkeys[ks0].UseSDL2 := true;
      fkeys[ks0].Name := SDL_DeviceInfo.Name;
      fkeys[ks0].NameIndex := SDL_DeviceInfo.Nameindex;
      fkeys[ks0].ButtonIndex[0] := SDL_DeviceInfo.FirstButtonIndex;
      fkeys[ks0].ButtonIndex[1] := SDL_DeviceInfo.SecondButtonIndex;
      fkeys[ks0].ButtonsIdle[0] := SDL_DeviceInfo.ButtonsIdle[SDL_DeviceInfo.FirstButtonIndex];
      fkeys[ks0].ButtonsIdle[1] := SDL_DeviceInfo.ButtonsIdle[SDL_DeviceInfo.SecondButtonIndex];
      fkeys[ks0].AchsisIndex[0] := SDL_DeviceInfo.UpDownIndex;
      fkeys[ks0].AchsisIdle[0] := SDL_DeviceInfo.AchsisIdle[SDL_DeviceInfo.UpDownIndex];
      fkeys[ks0].AchsisDirection[0] := SDL_DeviceInfo.UpDirection;
      fkeys[ks0].AchsisIndex[1] := SDL_DeviceInfo.LeftRightIndex;
      fkeys[ks0].AchsisIdle[1] := SDL_DeviceInfo.AchsisIdle[SDL_DeviceInfo.LeftRightIndex];
      fkeys[ks0].AchsisDirection[1] := SDL_DeviceInfo.LeftDirection;
    End
    Else Begin
      fkeys[ks1].UseSDL2 := true;
      fkeys[ks1].Name := SDL_DeviceInfo.Name;
      fkeys[ks1].NameIndex := SDL_DeviceInfo.Nameindex;
      fkeys[ks1].ButtonIndex[0] := SDL_DeviceInfo.FirstButtonIndex;
      fkeys[ks1].ButtonIndex[1] := SDL_DeviceInfo.SecondButtonIndex;
      fkeys[ks1].ButtonsIdle[0] := SDL_DeviceInfo.ButtonsIdle[SDL_DeviceInfo.FirstButtonIndex];
      fkeys[ks1].ButtonsIdle[1] := SDL_DeviceInfo.ButtonsIdle[SDL_DeviceInfo.SecondButtonIndex];
      fkeys[ks1].AchsisIndex[0] := SDL_DeviceInfo.UpDownIndex;
      fkeys[ks1].AchsisIdle[0] := SDL_DeviceInfo.AchsisIdle[SDL_DeviceInfo.UpDownIndex];
      fkeys[ks1].AchsisDirection[0] := SDL_DeviceInfo.UpDirection;
      fkeys[ks1].AchsisIndex[1] := SDL_DeviceInfo.LeftRightIndex;
      fkeys[ks1].AchsisIdle[1] := SDL_DeviceInfo.AchsisIdle[SDL_DeviceInfo.LeftRightIndex];
      fkeys[ks1].AchsisDirection[1] := SDL_DeviceInfo.LeftDirection;
    End;
  End;
  w.free;
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
  height := 280;
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
  CreateButton(Button4, 'Button4', 'Use joystick', 200, 10, mrNone);
  button4.Width := 110;
  CreateButton(Button5, 'Button5', 'Use keyboard', 200, 165, mrNone);
  button5.Width := 110;

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
  CreateButton(Button6, 'Button6', 'Use joystick', 200, 310, mrNone);
  button6.Width := 110;
  CreateButton(Button7, 'Button7', 'Use keyboard', 200, 465, mrNone);
  button7.Width := 110;

  CreateButton(Button1, 'Button1', 'OK', 240, 505, mrOK);
  CreateButton(Button2, 'Button2', 'Cancel', 240, 420, mrCancel);
  CreateButton(Button3, 'Button3', 'default', 240, 10, mrNone);

  button3.OnClick := @OnSetDefaultEvent;
  button4.OnClick := @OnSetSDLKeyEvent;
  button5.OnClick := @OnSetKeyBoardEvent;
  button6.OnClick := @OnSetSDLKeyEvent;
  button7.OnClick := @OnSetKeyBoardEvent;
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

