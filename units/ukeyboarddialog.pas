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

Uses SDL2, Dialogs, usdlwizzard, ulayout
{$IFDEF Launcher}
  , ulanguage
{$ENDIF}
  ;

{$IFNDEF Launcher}
Const
  R_KeyboardSettings = 'Keyboard settings';
  R_Keyboard0 = 'Keyboard 1';
  R_Keyboard1 = 'Keyboard 2';
  R_KeyUp = 'Key Up:';
  R_KeyDown = 'Key Down:';
  R_KeyLeft = 'Key Left:';
  R_KeyRight = 'Key Right:';
  R_KeyPrimary = 'Key Primary:';
  R_KeySecondary = 'Key Secondary:';
  R_Grab = 'Grab';
  R_UseJoystick = 'Use joystick';
  R_UseKeyboard = 'Use keyboard';
  R_Default = 'default';
  R_Cancel = 'Cancel';
{$ENDIF}

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
    label25.caption := R_Keyboard0;
  End
  Else Begin
    fkeys[ks1].UseSDL2 := false;
    label26.caption := R_Keyboard1;
  End;
  FitLayout(self);
End;

Procedure TKeyboardDialog.OnSetSDLKeyEvent(Sender: TObject);
Var
  ver: TSDL_Version;
  w: TSDLWizzard;
  SDL_DeviceInfo: TDeviceInfo;
Begin
  If SDL_LoadLib('') Then Begin
    showmessage('Error, could not load sdl2 lib.' + LineEnding +
      'FPC_Atomic needs to have at least SDL2 version: 2.14' + LineEnding + LineEnding +
      'Please'
{$IFDEF Windows}
      + ' download sdl2.dll and store it into fpc_atomic root folder'
{$ELSE}
      + ' run:' + LineEnding + '  sudo aptitude install libsdl2-dev'
{$ENDIF}
      + LineEnding + LineEnding +
      'and retry.' + LineEnding + LineEnding +
      'Actual installed version is: ' + GetSDL2VersionByManualDynamicLoading('')
      );
    exit;
  End;
  If SDL_Init(SDL_INIT_GAMECONTROLLER) <> 0 Then Begin
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
      fkeys[ks0].IsGameController := SDL_DeviceInfo.IsGameController;
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
      fkeys[ks1].IsGameController := SDL_DeviceInfo.IsGameController;
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
    kg.Caption := R_Grab;
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
  width := 640;
  height := 280;
  BorderStyle := bsDialog;
  FormStyle := fsNormal;
  Position := poScreenCenter;
  Caption := R_KeyboardSettings;

  CreateLabel(label25, 'Label25', R_Keyboard0, 8, 10);
  CreateLabel(label1, 'Label1', R_KeyUp, 20, 10);
  CreateLabel(label2, 'Label2', R_KeyDown, 50, 10);
  CreateLabel(label3, 'Label3', R_KeyLeft, 80, 10);
  CreateLabel(label4, 'Label4', R_KeyRight, 110, 10);
  CreateLabel(label5, 'Label5', R_KeyPrimary, 140, 10);
  CreateLabel(label6, 'Label6', R_KeySecondary, 170, 10);
  CreateLabel(label7, 'Label7', '?', 20, 140);
  CreateLabel(label8, 'Label8', '?', 50, 140);
  CreateLabel(label9, 'Label9', '?', 80, 140);
  CreateLabel(label10, 'Label10', '?', 110, 140);
  CreateLabel(label11, 'Label11', '?', 140, 140);
  CreateLabel(label12, 'Label12', '?', 170, 140);
  CreateKeyGrabber(KeyGrabber1, 'KeyGrabber1', 20, 225, 1);
  CreateKeyGrabber(KeyGrabber2, 'KeyGrabber2', 50, 225, 2);
  CreateKeyGrabber(KeyGrabber3, 'KeyGrabber3', 80, 225, 3);
  CreateKeyGrabber(KeyGrabber4, 'KeyGrabber4', 110, 225, 4);
  CreateKeyGrabber(KeyGrabber5, 'KeyGrabber5', 140, 225, 5);
  CreateKeyGrabber(KeyGrabber6, 'KeyGrabber6', 170, 225, 6);
  CreateButton(Button4, 'Button4', R_UseJoystick, 200, 10, mrNone);
  button4.Width := 110;
  CreateButton(Button5, 'Button5', R_UseKeyboard, 200, 190, mrNone);
  button5.Width := 110;

  CreateLabel(label26, 'Label26', R_Keyboard1, 8, 330);
  CreateLabel(label13, 'Label13', R_KeyUp, 20, 330);
  CreateLabel(label14, 'Label14', R_KeyDown, 50, 330);
  CreateLabel(label15, 'Label15', R_KeyLeft, 80, 330);
  CreateLabel(label16, 'Label16', R_KeyRight, 110, 330);
  CreateLabel(label17, 'Label17', R_KeyPrimary, 140, 330);
  CreateLabel(label18, 'Label18', R_KeySecondary, 170, 330);
  CreateLabel(label19, 'Label19', '?', 20, 460);
  CreateLabel(label20, 'Label20', '?', 50, 460);
  CreateLabel(label21, 'Label21', '?', 80, 460);
  CreateLabel(label22, 'Label22', '?', 110, 460);
  CreateLabel(label23, 'Label23', '?', 140, 460);
  CreateLabel(label24, 'Label24', '?', 170, 460);
  CreateKeyGrabber(KeyGrabber7, 'KeyGrabber7', 20, 545, 7);
  CreateKeyGrabber(KeyGrabber8, 'KeyGrabber8', 50, 545, 8);
  CreateKeyGrabber(KeyGrabber9, 'KeyGrabber9', 80, 545, 9);
  CreateKeyGrabber(KeyGrabber10, 'KeyGrabber10', 110, 545, 10);
  CreateKeyGrabber(KeyGrabber11, 'KeyGrabber11', 140, 545, 11);
  CreateKeyGrabber(KeyGrabber12, 'KeyGrabber12', 170, 545, 12);
  CreateButton(Button6, 'Button6', R_UseJoystick, 200, 330, mrNone);
  button6.Width := 110;
  CreateButton(Button7, 'Button7', R_UseKeyboard, 200, 510, mrNone);
  button7.Width := 110;

  CreateButton(Button1, 'Button1', 'OK', 240, 545, mrOK);
  CreateButton(Button2, 'Button2', R_Cancel, 240, 455, mrCancel);
  CreateButton(Button3, 'Button3', R_Default, 240, 10, mrNone);
  button2.Width := 85;
  button3.Width := 120;

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
  If fkeys[ks0].UseSDL2 Then Begin
    label25.caption := R_Keyboard0 + '(' + keys0.Name + ')';
  End
  Else Begin
    label25.caption := R_Keyboard0;
  End;
  If fkeys[ks1].UseSDL2 Then Begin
    label26.caption := R_Keyboard1 + '(' + keys1.Name + ')';
  End
  Else Begin
    label26.caption := R_Keyboard1;
  End;
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
  FitLayout(self); // Die Namen der Tasten sind unterschiedlich lang
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

