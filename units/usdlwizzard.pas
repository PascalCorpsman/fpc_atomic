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
Unit usdlwizzard;

{$MODE ObjFPC}{$H+}

Interface

Uses
  forms, Classes, SysUtils, ComCtrls, StdCtrls, ExtCtrls, Controls, Buttons,
  usdl_joystick;

Type

  TDeviceInfo = Record
    Name: String;
    AchsisIdle: Array Of Integer;
    ButtonsIdle: Array Of Boolean;
    FirstButtonIndex, SecondButtonIndex: integer;
    UpDownIndex, LeftRightIndex: integer;
    UpDirection, LeftDirection: Integer;
  End;

  TWizzardState = (
    wsCenter // Einlesen der "Ruhe" Positionen
    , wsPressFirstAction
    , wsPressSecondAction
    , wsSearchUpDown
    , wsSearchLeftRight
    , wsIdle
    );

  { TSDLWizzard }

  TSDLWizzard = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Timer1: TTimer;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    First: TShape;
    Second: TShape;
    Procedure OnRefreshDeviceList(Sender: TObject);
    Procedure OnSelectDeviceNext(Sender: TObject);
    Procedure OnCenterDeviceNext(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    WizzardState: TWizzardState;
    fsdlJoyStick: TSDL_Joystick;
    pb: Array Of TProgressBar;
    sp: Array Of TShape;
  public
    Device: TDeviceInfo; // Nur Gültig, wenn Execute = True !
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    Destructor Destroy; override;

    Function Execute(): Boolean; // True, wenn Erfolgreich ausgeführt
  End;

Implementation

Uses SDL2, Dialogs, Graphics, LResources, math, uatomic_common;

{ TSDLWizzard }

Procedure TSDLWizzard.OnRefreshDeviceList(Sender: TObject);
Var
  i: Integer;
Begin
  ComboBox1.Clear;
  For i := 0 To SDL_NumJoysticks() - 1 Do Begin
    ComboBox1.Items.Add(SDL_JoystickNameForIndex(i));
  End;
  If ComboBox1.Items.Count <> 0 Then Begin
    ComboBox1.ItemIndex := 0;
  End;
End;

Procedure TSDLWizzard.OnSelectDeviceNext(Sender: TObject);
Var
  i: Integer;
Begin
  If (ComboBox1.Items.Count = 0) Or (ComboBox1.ItemIndex = -1) Then Begin
    showmessage('Error, nothing selected.');
    exit;
  End;
  Try
    fsdlJoyStick := TSDL_Joystick.Create(ComboBox1.ItemIndex);
  Except
    fsdlJoyStick.free;
    fsdlJoyStick := Nil;
    showmessage('Error could not init joystick.');
    exit;
  End;
  Device.Name := ComboBox1.Text;
  setlength(pb, fsdlJoyStick.AxisCount);
  For i := 0 To fsdlJoyStick.AxisCount - 1 Do Begin
    pb[i] := TProgressBar.Create(TabSheet2);
    pb[i].Name := 'Progressbar' + inttostr(i + 1);
    pb[i].Parent := TabSheet2;
    pb[i].Min := -32768;
    pb[i].Max := 32767;
    pb[i].Orientation := pbVertical;
    pb[i].Top := 48;
    pb[i].Left := 16 + i * (44 - 16);
    pb[i].Width := 20;
    pb[i].Height := 180;
    pb[i].Position := fsdlJoyStick.Axis[i];
  End;
  setlength(sp, fsdlJoyStick.ButtonCount);
  For i := 0 To fsdlJoyStick.ButtonCount - 1 Do Begin
    sp[i] := TShape.Create(TabSheet2);
    sp[i].name := 'Shape' + IntToStr(i + 1);
    sp[i].Parent := TabSheet2;
    sp[i].Shape := stCircle;
    sp[i].Width := 20;
    sp[i].Height := 20;
    sp[i].Top := 48 + 180 + 10;
    sp[i].Left := 16 + i * (20 + 5);
    If fsdlJoyStick.Button[i] Then Begin
      sp[i].Brush.Color := clRed;
    End
    Else Begin
      sp[i].Brush.Color := clWhite;
    End;
  End;
  Device.FirstButtonIndex := -1;
  Device.SecondButtonIndex := -1;
  Device.UpDownIndex := -1;
  Device.LeftRightIndex := -1;
  timer1.Enabled := true;
  SpeedButton1.ImageIndex := -1;
  SpeedButton2.ImageIndex := -1;
  SpeedButton3.ImageIndex := -1;
  SpeedButton4.ImageIndex := -1;
  WizzardState := wsCenter;
  PageControl1.ActivePageIndex := 1;
End;

Procedure TSDLWizzard.OnCenterDeviceNext(Sender: TObject);
Var
  i: Integer;
Begin
  // Einlesen der "Centren" / Idle States aller Buttons / Achsen
  label2.caption := 'Press first action button';
  SetLength(Device.AchsisIdle, fsdlJoyStick.AxisCount);
  For i := 0 To high(Device.AchsisIdle) Do Begin
    Device.AchsisIdle[i] := fsdlJoyStick.Axis[i];
  End;
  setlength(Device.ButtonsIdle, fsdlJoyStick.ButtonCount);
  For i := 0 To high(Device.ButtonsIdle) Do Begin
    Device.ButtonsIdle[i] := fsdlJoyStick.Button[i];
  End;
  Device.FirstButtonIndex := -1;
  Device.SecondButtonIndex := -1;
  WizzardState := wsPressFirstAction;
  Button5.Visible := false;
End;

Procedure TSDLWizzard.Timer1Timer(Sender: TObject);
Var
  event: TSDL_Event;
  i, d: Integer;
Begin
  While SDL_PollEvent(@event) <> 0 Do Begin
    Case event.type_ Of
      SDL_JOYAXISMOTION: Begin // Eine Joystick Achse wurde geändert, diese überbehmen wir
          pb[event.jaxis.axis].Position := fsdlJoyStick.Axis[event.jaxis.axis];
        End;
      SDL_JOYBUTTONUP,
        SDL_JOYBUTTONDOWN: Begin
          If fsdlJoyStick.Button[event.jbutton.button]
            Then Begin
            sp[event.jbutton.button].Brush.Color := clRed;
          End
          Else Begin
            sp[event.jbutton.button].Brush.Color := clWhite;
          End;
          If event.type_ = SDL_JOYBUTTONDOWN Then Begin
            Case WizzardState Of
              wsPressFirstAction: Begin
                  Device.FirstButtonIndex := event.jbutton.button;
                  label2.caption := 'Press second action button';
                  WizzardState := wsPressSecondAction;
                End;
              wsPressSecondAction: Begin
                  If event.jbutton.button <> Device.FirstButtonIndex Then Begin
                    Device.SecondButtonIndex := event.jbutton.button;
                    label2.caption := 'Press up and confirm' + LineEnding + 'with first action';
                    WizzardState := wsSearchUpDown;
                  End;
                End;
              wsSearchUpDown: Begin
                  For i := 0 To fsdlJoyStick.AxisCount - 1 Do Begin
                    If abs(Device.AchsisIdle[i] - fsdlJoyStick.Axis[i]) > achsistrigger Then Begin
                      Device.UpDownIndex := i;
                      Device.UpDirection := sign(Device.AchsisIdle[i] - fsdlJoyStick.Axis[i]);
                      label2.caption := 'Press left and confirm' + LineEnding + 'with first action';
                      WizzardState := wsSearchLeftRight;
                      break;
                    End;
                  End;
                End;
              wsSearchLeftRight: Begin
                  For i := 0 To fsdlJoyStick.AxisCount - 1 Do Begin
                    If abs(Device.AchsisIdle[i] - fsdlJoyStick.Axis[i]) > achsistrigger Then Begin
                      If Device.UpDownIndex <> i Then Begin
                        Device.LeftRightIndex := i;
                        Device.LeftDirection := sign(Device.AchsisIdle[i] - fsdlJoyStick.Axis[i]);
                        label2.caption := 'Finished, press OK to confirm';
                        WizzardState := wsIdle;
                        Button2.Enabled := true;
                        break;
                      End;
                    End;
                  End;
                End;
            End;
          End;
        End;
    End;
  End;
  // Anzeigen der Aktuell Erkannten Tasten
  If Device.FirstButtonIndex <> -1 Then Begin
    If Device.ButtonsIdle[Device.FirstButtonIndex] <> fsdlJoyStick.Button[Device.FirstButtonIndex] Then Begin
      first.Brush.Color := clRed;
    End
    Else Begin
      first.Brush.Color := clWhite;
    End;
  End;
  If Device.SecondButtonIndex <> -1 Then Begin
    If Device.ButtonsIdle[Device.SecondButtonIndex] <> fsdlJoyStick.Button[Device.SecondButtonIndex] Then Begin
      Second.Brush.Color := clRed;
    End
    Else Begin
      Second.Brush.Color := clWhite;
    End;
  End;
  If Device.UpDownIndex <> -1 Then Begin
    SpeedButton2.ImageIndex := -1;
    SpeedButton3.ImageIndex := -1;
    d := (Device.AchsisIdle[Device.UpDownIndex] - fsdlJoyStick.Axis[Device.UpDownIndex]);
    If abs(d) > achsistrigger Then Begin
      If sign(d) = Device.UpDirection Then Begin
        SpeedButton2.ImageIndex := 0;
      End
      Else Begin
        SpeedButton3.ImageIndex := 1;
      End;
    End;
  End;
  If Device.LeftRightIndex <> -1 Then Begin
    SpeedButton1.ImageIndex := -1;
    SpeedButton4.ImageIndex := -1;
    d := (Device.AchsisIdle[Device.LeftRightIndex] - fsdlJoyStick.Axis[Device.LeftRightIndex]);
    If abs(d) > achsistrigger Then Begin
      If sign(d) = Device.LeftDirection Then Begin
        SpeedButton1.ImageIndex := 2;
      End
      Else Begin
        SpeedButton4.ImageIndex := 3;
      End;
    End;
  End;
End;

Constructor TSDLWizzard.CreateNew(AOwner: TComponent; Num: Integer);
  Procedure CreateLabel(Var lb: TLabel; aName, acaption: String; aTop, aleft: integer; aOwner: TWinControl);
  Begin
    lb := TLabel.Create(aOwner);
    lb.Name := aName;
    lb.Parent := aOwner;
    lb.Caption := acaption;
    lb.Left := aleft;
    lb.Top := aTop;
  End;

  Procedure CreateButton(Var bt: TButton; aName, aCaption: String; aTop, aLeft: Integer; aResult: TModalResult; aOwner: TWinControl);
  Begin
    bt := TButton.Create(aOwner);
    bt.Name := aName;
    bt.Parent := aOwner;
    bt.Caption := aCaption;
    bt.ModalResult := aResult;
    bt.Left := aLeft;
    bt.Top := aTop;
    bt.Width := 115;
  End;

  Procedure CreateSpeedButton(Var bt: TSpeedButton; aName: String; aTop, aLeft: Integer; aOwner: TWinControl);
  Begin
    bt := TSpeedButton.Create(aOwner);
    bt.Name := aName;
    bt.Parent := aOwner;
    bt.Images := ImageList1;
    bt.ImageIndex := -1;
    bt.Left := aLeft;
    bt.Top := aTop;
    bt.Width := 50;
    bt.Height := 50;
  End;

  Procedure CreateShape(Var s: TShape; aName: String; aTop, aLeft: Integer; aOwner: TWinControl);
  Begin
    s := TShape.Create(aOwner);
    s.Name := aName;
    s.Parent := aOwner;
    s.Shape := stCircle;
    s.Left := aLeft;
    s.Top := aTop;
    s.Width := 50;
    s.Height := 50;
    s.Brush.Color := clWhite;
  End;

Begin
  Inherited CreateNew(AOwner, Num);
  fsdlJoyStick := Nil;
  width := 609;
  height := 360;
  BorderStyle := bsDialog;
  FormStyle := fsNormal;
  Position := poScreenCenter;
  Caption := 'Joystick settings';
  PageControl1 := TPageControl.Create(self);
  PageControl1.Name := 'PageControl1';
  PageControl1.Parent := self;
  PageControl1.ShowTabs := false;
  PageControl1.Top := 8;
  PageControl1.Left := 8;
  PageControl1.Width := 592;
  PageControl1.Height := 304;
  // Tab Select Device
  TabSheet1 := PageControl1.AddTabSheet;
  CreateLabel(label1, 'Label1', 'Select', 92, 48, TabSheet1);
  ComboBox1 := TComboBox.Create(TabSheet1);
  ComboBox1.Name := 'ComboBox1';
  ComboBox1.Parent := TabSheet1;
  ComboBox1.Left := 104;
  ComboBox1.Top := 80;
  ComboBox1.Width := 400;
  ComboBox1.Style := csDropDownList;
  CreateButton(Button3, 'Button3', 'R', 83, 512, mrNone, TabSheet1);
  Button3.Width := 25;
  Button3.OnClick := @OnRefreshDeviceList;
  CreateButton(Button4, 'Button4', 'Next', 264, 464, mrNone, TabSheet1);
  button4.OnClick := @OnSelectDeviceNext;

  // Tab Select
  TabSheet2 := PageControl1.AddTabSheet;
  CreateLabel(label2, 'Label2', 'Center All axis and do not press' + LineEnding + 'any buttons then click next.', 8, 8, TabSheet2);
  CreateLabel(label3, 'Label3', 'First' + LineEnding + 'action', 168, 416, TabSheet2);
  CreateLabel(label4, 'Label4', 'Second' + LineEnding + 'action', 168, 528, TabSheet2);
  CreateShape(First, 'First', 208, 416, TabSheet2);
  CreateShape(Second, 'Second', 208, 528, TabSheet2);
  CreateButton(Button5, 'Button5', 'Next', 264, 464, mrNone, TabSheet2);
  button5.OnClick := @OnCenterDeviceNext;

  PageControl1.ActivePageIndex := 0;
  CreateButton(Button1, 'Button1', 'Cancel', 320, 8, mrCancel, self);
  CreateButton(Button2, 'Button2', 'OK', 320, 485, mrOK, self);

  ImageList1 := TImageList.Create(self);
  ImageList1.Name := 'ImageList1';
  ImageList1.Width := 32;
  ImageList1.Height := 32;
  ImageList1.AddLazarusResource('Arror_up', clFuchsia);
  ImageList1.AddLazarusResource('Arror_down', clFuchsia);
  ImageList1.AddLazarusResource('Arror_left', clFuchsia);
  ImageList1.AddLazarusResource('Arror_right', clFuchsia);

  CreateSpeedButton(SpeedButton1, 'SpeedButton1', 64, 416, TabSheet2);
  CreateSpeedButton(SpeedButton2, 'SpeedButton2', 8, 472, TabSheet2);
  CreateSpeedButton(SpeedButton3, 'SpeedButton3', 112, 472, TabSheet2);
  CreateSpeedButton(SpeedButton4, 'SpeedButton4', 64, 528, TabSheet2);

  OnRefreshDeviceList(button3);
  Timer1 := TTimer.Create(self);
  Timer1.Name := 'Timer1';
  Timer1.Enabled := false;
  Timer1.Interval := 1;
  Timer1.OnTimer := @Timer1Timer;

  Button2.enabled := false;
End;

Destructor TSDLWizzard.Destroy;
Begin
  timer1.enabled := false;
  If assigned(fsdlJoyStick) Then fsdlJoyStick.free;
  fsdlJoyStick := Nil;
  Inherited Destroy;
End;

Function TSDLWizzard.Execute: Boolean;
Begin
  result := false;
  ModalResult := mrNone;
  ShowModal;
  result := ModalResult = mrOK;
End;

Initialization
{$I usdlwizzard.ressource}

End.

