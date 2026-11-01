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
  usdl_joystick, usdl_gamecontroller;

Type

  TDeviceInfo = Record
    Name: String;
    Nameindex: Integer; // Gibt es mehrere Devices mit dem selben Namen, dann gibt NameIndex an, der wievielte es ist ..
    AchsisIdle: Array Of Integer;
    ButtonsIdle: Array Of Boolean;
    FirstButtonIndex, SecondButtonIndex: integer;
    UpDownIndex, LeftRightIndex: integer; // -1 = nur mit dem Hat steuern
    UpDirection, LeftDirection: Integer;
    IsGameController: Boolean;
  End;

  TWizzardState = (
    wsSelectDevice // Auswahl des Geräts
    , wsCenter // Einlesen der "Ruhe" Positionen (nur Joystick)
    , wsFirstAction
    , wsSecondAction
    , wsUp // nur Joystick
    , wsLeft // nur Joystick
    , wsDone
    );

  (*
   * Richtet einen Controller Schritt für Schritt ein:
   * Gerät wählen, dann je Schritt genau eine Anweisung, Tasten und Richtungen werden automatisch übernommen.
   * Links steht eine Checkliste mit den Ergebnissen, darunter eine Live-Anzeige zum Testen.
   *)

  { TSDLWizzard }

  TSDLWizzard = Class(TForm)
  private
    fsdlJoyStick: TSDL_Joystick;
    fState: TWizzardState;
    fSteps: Array Of TWizzardState; // Die Schritte für das gewählte Gerät
    fStep: Integer; // Index in fSteps, = length(fSteps) wenn fertig
    fHatOnly: Boolean; // Bei "Hoch" wurde der Hat benutzt, dann wird nur mit dem Hat gesteuert
    fHoldAxis: Integer; // Was gerade gehalten wird (Achse, HoldHat oder HoldNone)
    fHoldSince: QWord;
    fWaitStep: TWizzardState; // Dessen Checklisten Zeile blinkt, wsDone = keine
    fDeviceNames: Array Of String; // Joystick Name je SDL Index
    fControllerNames: Array Of String; // Gamecontroller Name je SDL Index, '' wenn es kein Gamecontroller ist
    // Controls
    lbHeader, lbHint, lbError: TLabel;
    DevicePanel, StepPanel: TPanel;
    lbDevice: TLabel;
    cbDevice: TComboBox;
    btRefresh: TButton;
    rbGameController, rbJoystick: TRadioButton;
    lbStep: Array[TWizzardState] Of TLabel; // Checkliste, genutzt werden nur wsCenter .. wsLeft
    gbLive: TGroupBox;
    pb: Array Of TProgressBar;
    sp: Array Of TShape;
    ImageList1: TImageList;
    sbUp, sbDown, sbLeft, sbRight: TSpeedButton;
    btCancel, btBack, btNext: TButton;
    Timer1: TTimer;
    Procedure OnRefreshDeviceList(Sender: TObject);
    Procedure OnDeviceChange(Sender: TObject);
    Procedure OnBack(Sender: TObject);
    Procedure OnNext(Sender: TObject);
    Procedure OnTimer(Sender: TObject);
    Function OpenDevice: Boolean;
    Procedure CloseDevice;
    Procedure CaptureIdle;
    Procedure CreateLiveView;
    Procedure UpdateLiveView;
    Procedure ShowDeviceSelection;
    Procedure GotoStep(aStep: Integer);
    Function StepIndex(aState: TWizzardState): Integer;
    Procedure UpdateStepUI;
    Procedure ButtonPressed(aButton: Integer);
    Procedure CheckDirection;
    Function ButtonName(aButton: Integer): String;
    Function StepResult(aState: TWizzardState): String;
    Procedure SetError(Const aText: String);
    Procedure FocusNext;
  public
    Device: TDeviceInfo; // Nur Gültig, wenn Execute = True !
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    Destructor Destroy; override;

    Function Execute(): Boolean; // True, wenn Erfolgreich ausgeführt
  End;

Implementation

Uses SDL2, Dialogs, Graphics, LResources, math, uatomic_common, ulayout
{$IFDEF Launcher}
  , ulanguage
{$ENDIF}
  ;

{$IFNDEF Launcher}
Const
  R_Cancel = 'Cancel';
  R_DirHat = 'hat';
  RF_Button = 'Button %d';
  R_WizCaption = 'Set up controller';
  R_WizSelectTitle = 'Choose device';
  R_WizSelectHint = 'Choose the controller and how it should be used.';
  R_WizDevice = '&Device:';
  R_WizRefresh = '&Refresh devices';
  R_WizAsGameController = 'Use as &gamecontroller (recommended)';
  R_WizAsJoystick = 'Use as &joystick (own axes and buttons)';
  R_WizNoDevice = 'No controller found. Connect one and refresh the devices.';
  R_WizOpenFailed = 'Could not open the device.';
  R_WizNoDPad = 'This gamecontroller has no D-Pad, please use it as joystick.';
  RF_WizStep = 'Step %d of %d: %s';
  R_WizCenterTitle = 'Rest position';
  R_WizCenterHint = 'Let go of all sticks and buttons, then click Next.';
  R_WizFirstTitle = 'First action';
  R_WizFirstHint = 'Press the button you want to drop bombs with.';
  R_WizSecondTitle = 'Second action';
  R_WizSecondHint = 'Press the button for the second action.';
  R_WizUpTitle = 'Up';
  R_WizUpHint = 'Push the stick or the D-Pad up and hold it for a moment.';
  R_WizLeftTitle = 'Left';
  R_WizLeftHint = 'Push the stick to the left and hold it for a moment.';
  R_WizDoneTitle = 'Done';
  R_WizDoneHint = 'Everything is set up. Click Finish or press the first action to apply it.';
  R_WizWaitButton = 'waiting for button …';
  R_WizWaitDirection = 'waiting for direction …';
  RF_WizAxis = 'axis %d';
  R_WizSameButton = 'This button is already the first action.';
  R_WizDPadButton = 'The D-Pad can not be used for actions, it is only used for the directions.';
  R_WizLive = 'Live view (to test)';
  R_WizBack = '< &Back';
  R_WizNext = '&Next >';
  R_WizFinish = '&Finish';
{$ENDIF}

Const
  HoldTime = 500; // So lange (in ms) muss eine Richtung gehalten werden, bis sie übernommen wird
  HoldNone = -2;
  HoldHat = -3;
  MarkDone = '✓';
  MarkCurrent = '▶';
  MarkPending = '○';
  FormWidth = 600;
  Margin = 8;
  ArrowSize = 36;
  RowHeight = 30; // Abstand der Zeilen in der Checkliste
  PanelTop = 104; // Unter Überschrift und Anweisung
  PanelHeight = 316;
  ButtonTop = PanelTop + PanelHeight + 28;
  BlinkTime = 600; // ms, so schnell blinkt "warte auf ..."

Function StepTitle(aState: TWizzardState): String;
Begin
  Case aState Of
    wsCenter: result := R_WizCenterTitle;
    wsFirstAction: result := R_WizFirstTitle;
    wsSecondAction: result := R_WizSecondTitle;
    wsUp: result := R_WizUpTitle;
    wsLeft: result := R_WizLeftTitle;
  Else
    result := '';
  End;
End;

Function StepHint(aState: TWizzardState): String;
Begin
  Case aState Of
    wsCenter: result := R_WizCenterHint;
    wsFirstAction: result := R_WizFirstHint;
    wsSecondAction: result := R_WizSecondHint;
    wsUp: result := R_WizUpHint;
    wsLeft: result := R_WizLeftHint;
  Else
    result := '';
  End;
End;

{ TSDLWizzard }

Procedure TSDLWizzard.OnRefreshDeviceList(Sender: TObject);
Var
  i, j, n, sel: Integer;
  s, old: String;
Begin
  old := cbDevice.Text;
  SDL_Quit;
  SDL_Init(SDL_INIT_GAMECONTROLLER);
  cbDevice.Clear;
  setlength(fDeviceNames, max(0, SDL_NumJoysticks()));
  setlength(fControllerNames, length(fDeviceNames));
  sel := 0;
  For i := 0 To high(fDeviceNames) Do Begin
    fDeviceNames[i] := SDL_JoystickNameForIndex(i);
    fControllerNames[i] := '';
    If SDL_IsGameController(i) Then fControllerNames[i] := SDL_GameControllerNameForIndex(i);
    s := fControllerNames[i];
    If s = '' Then s := fDeviceNames[i];
    // Gleiche Namen durchnummerieren, sonst kann man zwei gleiche Controller nicht unterscheiden
    n := 1;
    For j := 0 To i - 1 Do Begin
      If (fControllerNames[j] = s) Or (fDeviceNames[j] = s) Then inc(n);
    End;
    If n > 1 Then s := s + format(' (#%d)', [n]);
    cbDevice.Items.Add(s);
    If s = old Then sel := i;
  End;
  If cbDevice.Items.Count <> 0 Then cbDevice.ItemIndex := sel;
  OnDeviceChange(Nil);
End;

Procedure TSDLWizzard.OnDeviceChange(Sender: TObject);
Var
  i: Integer;
Begin
  i := cbDevice.ItemIndex;
  rbGameController.Enabled := (i >= 0) And (fControllerNames[i] <> '');
  rbJoystick.Enabled := i >= 0;
  If rbGameController.Enabled Then Begin
    rbGameController.Checked := true;
  End
  Else Begin
    rbJoystick.Checked := true;
  End;
  btNext.Enabled := i >= 0;
  If i < 0 Then Begin
    SetError(R_WizNoDevice);
  End
  Else Begin
    SetError('');
  End;
End;

Procedure TSDLWizzard.OnBack(Sender: TObject);
Begin
  Case fState Of
    wsSelectDevice: ;
    wsDone: Begin
        If fHatOnly Then Begin
          GotoStep(StepIndex(wsUp));
        End
        Else Begin
          GotoStep(high(fSteps));
        End;
      End;
  Else Begin
      If fStep = 0 Then Begin
        ShowDeviceSelection;
      End
      Else Begin
        GotoStep(fStep - 1);
      End;
    End;
  End;
End;

Procedure TSDLWizzard.OnNext(Sender: TObject);
Begin
  Case fState Of
    wsSelectDevice: Begin
        If OpenDevice Then Begin
          DevicePanel.Visible := false;
          StepPanel.Visible := true;
          btBack.Enabled := true;
          GotoStep(0);
        End;
      End;
    wsCenter: Begin
        CaptureIdle;
        GotoStep(fStep + 1);
      End;
    wsDone: ModalResult := mrOK;
  End;
End;

Procedure TSDLWizzard.OnTimer(Sender: TObject);
Var
  event: TSDL_Event;

  // Ohne bekannte Instance ID (sehr altes SDL2) wird jedes Gerät akzeptiert
  Function OwnDevice(aWhich: Integer): Boolean;
  Begin
    result := (fsdlJoyStick.InstanceID = -1) Or (aWhich = fsdlJoyStick.InstanceID);
  End;

Begin
  If Not assigned(fsdlJoyStick) Then exit;
  SDL_PumpEvents();
  (*
   * Bei einem Gamecontroller liefert SDL zusätzlich die rohen Joystick Events, deren Indizes passen aber nicht
   * zu den Gamecontroller Indizes, die das Spiel später abfragt. Also nur die zum Gerät passenden Events auswerten,
   * und nur die vom gerade eingerichteten Gerät.
   *)
  While SDL_PollEvent(@event) <> 0 Do Begin
    Case event.type_ Of
      SDL_JOYBUTTONDOWN: Begin
          If (Not Device.IsGameController) And OwnDevice(event.jbutton.which) Then ButtonPressed(event.jbutton.button);
        End;
      SDL_CONTROLLERBUTTONDOWN: Begin
          If Device.IsGameController And OwnDevice(event.cbutton.which) Then ButtonPressed(event.cbutton.button);
        End;
    End;
    // Das Drücken kann den Schritt wechseln, die restlichen Events gehören dann nicht mehr dazu
    If Not assigned(fsdlJoyStick) Then exit;
  End;
  CheckDirection;
  UpdateLiveView;
  // "warte auf ..." blinkt langsam, damit man sieht, dass der Wizzard auf den Controller wartet
  If fWaitStep <> wsDone Then Begin
    If (GetTickCount64 Div BlinkTime) Mod 2 = 0 Then Begin
      lbStep[fWaitStep].Font.Color := clWindowText;
    End
    Else Begin
      lbStep[fWaitStep].Font.Color := clGrayText;
    End;
  End;
End;

Function TSDLWizzard.OpenDevice: Boolean;
Var
  i, j: Integer;
  gc: TSDL_GameController;
Begin
  result := false;
  i := cbDevice.ItemIndex;
  If i < 0 Then exit;
  Device.IsGameController := rbGameController.Enabled And rbGameController.Checked;
  Try
    If Device.IsGameController Then Begin
      fsdlJoyStick := TSDL_GameController.Create(i);
    End
    Else Begin
      fsdlJoyStick := TSDL_Joystick.Create(i);
    End;
  Except
    fsdlJoyStick := Nil;
    SetError(R_WizOpenFailed);
    exit;
  End;
  If Device.IsGameController Then Begin
    gc := TSDL_GameController(fsdlJoyStick);
    If (Not gc.ButtonAvailable[SDL_CONTROLLER_BUTTON_DPAD_UP]) Or (Not gc.ButtonAvailable[SDL_CONTROLLER_BUTTON_DPAD_DOWN]) Or
      (Not gc.ButtonAvailable[SDL_CONTROLLER_BUTTON_DPAD_LEFT]) Or (Not gc.ButtonAvailable[SDL_CONTROLLER_BUTTON_DPAD_RIGHT]) Then Begin
      CloseDevice;
      SetError(R_WizNoDPad);
      exit;
    End;
  End;
  // Der NameIndex muss so gezählt werden wie in ResolveGameControllerNameToIndex / ResolveJoystickNameToIndex
  Device.Nameindex := 0;
  If Device.IsGameController Then Begin
    Device.Name := fControllerNames[i];
    For j := 0 To i - 1 Do Begin
      If fControllerNames[j] = Device.Name Then inc(Device.Nameindex);
    End;
    // Ruhestellung und Richtungen kennt SDL bei einem Gamecontroller selbst
    setlength(fSteps, 2);
    fSteps[0] := wsFirstAction;
    fSteps[1] := wsSecondAction;
  End
  Else Begin
    Device.Name := fDeviceNames[i];
    For j := 0 To i - 1 Do Begin
      If fDeviceNames[j] = Device.Name Then inc(Device.Nameindex);
    End;
    setlength(fSteps, 5);
    fSteps[0] := wsCenter;
    fSteps[1] := wsFirstAction;
    fSteps[2] := wsSecondAction;
    fSteps[3] := wsUp;
    fSteps[4] := wsLeft;
  End;
  Device.FirstButtonIndex := -1;
  Device.SecondButtonIndex := -1;
  Device.UpDownIndex := -1;
  Device.LeftRightIndex := -1;
  Device.UpDirection := 0;
  Device.LeftDirection := 0;
  // Bei einem Joystick wird das im Schritt Ruhestellung noch einmal gemacht
  CaptureIdle;
  CreateLiveView;
  Timer1.Enabled := true;
  result := true;
End;

Procedure TSDLWizzard.CloseDevice;
Var
  i: Integer;
Begin
  Timer1.Enabled := false;
  For i := 0 To high(pb) Do Begin
    pb[i].Free;
  End;
  setlength(pb, 0);
  For i := 0 To high(sp) Do Begin
    sp[i].Free;
  End;
  setlength(sp, 0);
  If assigned(fsdlJoyStick) Then fsdlJoyStick.free;
  fsdlJoyStick := Nil;
End;

Procedure TSDLWizzard.CaptureIdle;
Var
  i: Integer;
Begin
  // Einlesen der "Centren" / Idle States aller Buttons / Achsen
  SDL_PumpEvents();
  SetLength(Device.AchsisIdle, fsdlJoyStick.AxisCount);
  For i := 0 To high(Device.AchsisIdle) Do Begin
    Device.AchsisIdle[i] := fsdlJoyStick.Axis[i];
  End;
  setlength(Device.ButtonsIdle, fsdlJoyStick.ButtonCount);
  For i := 0 To high(Device.ButtonsIdle) Do Begin
    Device.ButtonsIdle[i] := fsdlJoyStick.Button[i];
  End;
End;

Procedure TSDLWizzard.CreateLiveView;
Const
  AreaLeft = 8;
  BarHeight = 56;
  ButtonSize = 16;
  ButtonStep = ButtonSize + 4;
Var
  i, p, AreaRight, PerRow, BarStep: Integer;

  Function ButtonVisible(aButton: Integer): Boolean;
  Begin
    // Bei einem Gamecontroller nur die Buttons, die das Gerät auch hat
    result := Not Device.IsGameController Or TSDL_GameController(fsdlJoyStick).ButtonAvailable[aButton];
  End;

Begin
  // Links Achsen und Buttons, rechts die Pfeile
  AreaRight := sbLeft.Left - 16;
  PerRow := (AreaRight - AreaLeft + ButtonStep - ButtonSize) Div ButtonStep;
  BarStep := 20;
  If fsdlJoyStick.AxisCount > 0 Then BarStep := max(4, min(BarStep, (AreaRight - AreaLeft) Div fsdlJoyStick.AxisCount));
  setlength(pb, fsdlJoyStick.AxisCount);
  For i := 0 To fsdlJoyStick.AxisCount - 1 Do Begin
    pb[i] := TProgressBar.Create(gbLive);
    pb[i].Parent := gbLive;
    pb[i].Min := -32768;
    pb[i].Max := 32767;
    pb[i].Orientation := pbVertical;
    pb[i].Top := 4;
    pb[i].Left := AreaLeft + i * BarStep;
    pb[i].Width := min(14, BarStep - 4);
    pb[i].Height := BarHeight;
  End;
  setlength(sp, fsdlJoyStick.ButtonCount);
  p := 0;
  For i := 0 To fsdlJoyStick.ButtonCount - 1 Do Begin
    sp[i] := TShape.Create(gbLive);
    sp[i].Parent := gbLive;
    sp[i].Shape := stCircle;
    sp[i].Width := ButtonSize;
    sp[i].Height := ButtonSize;
    sp[i].Visible := ButtonVisible(i);
    If Not sp[i].Visible Then Continue;
    sp[i].Top := 4 + BarHeight + 8 + (p Div PerRow) * ButtonStep;
    sp[i].Left := AreaLeft + (p Mod PerRow) * ButtonStep;
    inc(p);
  End;
  UpdateLiveView;
End;

Procedure TSDLWizzard.UpdateLiveView;
Var
  i, d, h: Integer;
  dUp, dDown, dLeft, dRight: Boolean;
  gc: TSDL_GameController;
Begin
  If Not assigned(fsdlJoyStick) Then exit;
  For i := 0 To high(pb) Do Begin
    pb[i].Position := fsdlJoyStick.Axis[i];
  End;
  For i := 0 To high(sp) Do Begin
    If Not sp[i].Visible Then Continue;
    If fsdlJoyStick.Button[i] Then Begin
      sp[i].Brush.Color := clRed;
    End
    Else Begin
      sp[i].Brush.Color := clWhite;
    End;
  End;
  // Die Pfeile zeigen, was das Spiel als Richtung erkennen würde
  dUp := false;
  dDown := false;
  dLeft := false;
  dRight := false;
  If Device.IsGameController Then Begin
    gc := TSDL_GameController(fsdlJoyStick);
    dUp := gc.Button[SDL_CONTROLLER_BUTTON_DPAD_UP] Or (gc.Axis[SDL_CONTROLLER_AXIS_LEFTY] < -Achsistrigger);
    dDown := gc.Button[SDL_CONTROLLER_BUTTON_DPAD_DOWN] Or (gc.Axis[SDL_CONTROLLER_AXIS_LEFTY] > Achsistrigger);
    dLeft := gc.Button[SDL_CONTROLLER_BUTTON_DPAD_LEFT] Or (gc.Axis[SDL_CONTROLLER_AXIS_LEFTX] < -Achsistrigger);
    dRight := gc.Button[SDL_CONTROLLER_BUTTON_DPAD_RIGHT] Or (gc.Axis[SDL_CONTROLLER_AXIS_LEFTX] > Achsistrigger);
  End
  Else Begin
    If (Device.UpDownIndex >= 0) And (Device.UpDownIndex <= high(Device.AchsisIdle)) Then Begin
      d := Device.AchsisIdle[Device.UpDownIndex] - fsdlJoyStick.Axis[Device.UpDownIndex];
      If abs(d) > Achsistrigger Then Begin
        dUp := sign(d) = Device.UpDirection;
        dDown := Not dUp;
      End;
    End;
    If (Device.LeftRightIndex >= 0) And (Device.LeftRightIndex <= high(Device.AchsisIdle)) Then Begin
      d := Device.AchsisIdle[Device.LeftRightIndex] - fsdlJoyStick.Axis[Device.LeftRightIndex];
      If abs(d) > Achsistrigger Then Begin
        dLeft := sign(d) = Device.LeftDirection;
        dRight := Not dLeft;
      End;
    End;
    If fsdlJoyStick.HatCount > 0 Then Begin
      h := fsdlJoyStick.Hat[0];
      dUp := dUp Or ((h And SDL_HAT_UP) <> 0);
      dDown := dDown Or ((h And SDL_HAT_DOWN) <> 0);
      dLeft := dLeft Or ((h And SDL_HAT_LEFT) <> 0);
      dRight := dRight Or ((h And SDL_HAT_RIGHT) <> 0);
    End;
  End;
  If dUp Then sbUp.ImageIndex := 0 Else sbUp.ImageIndex := -1;
  If dDown Then sbDown.ImageIndex := 1 Else sbDown.ImageIndex := -1;
  If dLeft Then sbLeft.ImageIndex := 2 Else sbLeft.ImageIndex := -1;
  If dRight Then sbRight.ImageIndex := 3 Else sbRight.ImageIndex := -1;
End;

Procedure TSDLWizzard.ShowDeviceSelection;
Begin
  CloseDevice;
  fState := wsSelectDevice;
  setlength(fSteps, 0);
  fStep := 0;
  lbHeader.Caption := R_WizSelectTitle;
  lbHint.Caption := R_WizSelectHint;
  StepPanel.Visible := false;
  DevicePanel.Visible := true;
  btBack.Enabled := false;
  btNext.Caption := R_WizNext;
  OnDeviceChange(Nil);
  FocusNext;
End;

Procedure TSDLWizzard.GotoStep(aStep: Integer);
Var
  i: Integer;
Begin
  fStep := aStep;
  // Alles ab dem neuen Schritt wird neu eingelesen
  For i := aStep To high(fSteps) Do Begin
    Case fSteps[i] Of
      wsFirstAction: Device.FirstButtonIndex := -1;
      wsSecondAction: Device.SecondButtonIndex := -1;
      wsUp: Begin
          Device.UpDownIndex := -1;
          Device.UpDirection := 0;
          fHatOnly := false;
        End;
      wsLeft: Begin
          Device.LeftRightIndex := -1;
          Device.LeftDirection := 0;
        End;
    End;
  End;
  If fStep >= length(fSteps) Then Begin
    fState := wsDone;
  End
  Else Begin
    fState := fSteps[fStep];
  End;
  fHoldAxis := HoldNone;
  SetError('');
  UpdateStepUI;
End;

Function TSDLWizzard.StepIndex(aState: TWizzardState): Integer;
Var
  i: Integer;
Begin
  result := high(fSteps);
  For i := 0 To high(fSteps) Do Begin
    If fSteps[i] = aState Then exit(i);
  End;
End;

Procedure TSDLWizzard.UpdateStepUI;
Var
  s: TWizzardState;
  i: Integer;
  mark, txt: String;
Begin
  If fState = wsDone Then Begin
    lbHeader.Caption := R_WizDoneTitle;
    lbHint.Caption := R_WizDoneHint;
  End
  Else Begin
    lbHeader.Caption := format(RF_WizStep, [fStep + 1, length(fSteps), StepTitle(fState)]);
    lbHint.Caption := StepHint(fState);
  End;
  fWaitStep := wsDone;
  For s := wsCenter To wsLeft Do Begin
    lbStep[s].Visible := false;
    lbStep[s].Font.Color := clWindowText;
  End;
  For i := 0 To high(fSteps) Do Begin
    s := fSteps[i];
    If i < fStep Then Begin
      mark := MarkDone;
      txt := StepResult(s);
    End
    Else If i = fStep Then Begin
      mark := MarkCurrent;
      Case s Of
        wsFirstAction, wsSecondAction: txt := R_WizWaitButton;
        wsUp, wsLeft: txt := R_WizWaitDirection;
      Else
        txt := '';
      End;
      If txt <> '' Then fWaitStep := s;
    End
    Else Begin
      mark := MarkPending;
      txt := '';
    End;
    If txt <> '' Then txt := ': ' + txt;
    lbStep[s].Caption := mark + '  ' + StepTitle(s) + txt;
    If i = fStep Then Begin
      lbStep[s].Font.Style := [fsBold];
    End
    Else Begin
      lbStep[s].Font.Style := [];
    End;
    lbStep[s].Top := 4 + i * RowHeight;
    lbStep[s].Visible := true;
  End;
  // Weiter gibt es nur, wo nicht automatisch weiter geschaltet wird
  btNext.Enabled := fState In [wsCenter, wsDone];
  If fState = wsDone Then Begin
    btNext.Caption := R_WizFinish;
  End
  Else Begin
    btNext.Caption := R_WizNext;
  End;
  FocusNext;
End;

Procedure TSDLWizzard.ButtonPressed(aButton: Integer);
Begin
  // Ist alles eingerichtet, dann übernimmt die erste Aktion wie Fertig, so muss man nicht zur Maus greifen
  If (fState = wsDone) And (aButton = Device.FirstButtonIndex) Then Begin
    ModalResult := mrOK;
    exit;
  End;
  If Not (fState In [wsFirstAction, wsSecondAction]) Then exit;
  If Device.IsGameController And (aButton >= SDL_CONTROLLER_BUTTON_DPAD_UP) And (aButton <= SDL_CONTROLLER_BUTTON_DPAD_RIGHT) Then Begin
    SetError(R_WizDPadButton);
    exit;
  End;
  If fState = wsFirstAction Then Begin
    Device.FirstButtonIndex := aButton;
    GotoStep(fStep + 1);
  End
  Else Begin
    If aButton = Device.FirstButtonIndex Then Begin
      SetError(R_WizSameButton);
      exit;
    End;
    Device.SecondButtonIndex := aButton;
    GotoStep(fStep + 1);
  End;
End;

Procedure TSDLWizzard.CheckDirection;
Var
  i, axis: Integer;
  n: QWord;
Begin
  If Not (fState In [wsUp, wsLeft]) Then exit;
  // Welche Achse (außer der schon für Hoch / Runter gewählten) ist ausgelenkt, oder der Hat ?
  axis := HoldNone;
  For i := 0 To min(fsdlJoyStick.AxisCount, length(Device.AchsisIdle)) - 1 Do Begin
    If (i <> Device.UpDownIndex) And (abs(Device.AchsisIdle[i] - fsdlJoyStick.Axis[i]) > Achsistrigger) Then Begin
      axis := i;
      break;
    End;
  End;
  If (axis = HoldNone) And (fState = wsUp) And (fsdlJoyStick.HatCount > 0) And (fsdlJoyStick.Hat[0] <> SDL_HAT_CENTERED) Then Begin
    axis := HoldHat;
  End;
  // Erst wenn das selbe lange genug gehalten wird, wird es übernommen
  n := GetTickCount64;
  If axis <> fHoldAxis Then Begin
    fHoldAxis := axis;
    fHoldSince := n;
    exit;
  End;
  If (axis = HoldNone) Or (n - fHoldSince < HoldTime) Then exit;
  If axis = HoldHat Then Begin
    // Gesteuert wird dann nur mit dem Hat, "Links" braucht es nicht mehr
    fHatOnly := true;
    Device.UpDownIndex := -1;
    Device.LeftRightIndex := -1;
    GotoStep(length(fSteps));
  End
  Else If fState = wsUp Then Begin
    Device.UpDownIndex := axis;
    Device.UpDirection := sign(Device.AchsisIdle[axis] - fsdlJoyStick.Axis[axis]);
    GotoStep(fStep + 1);
  End
  Else Begin
    Device.LeftRightIndex := axis;
    Device.LeftDirection := sign(Device.AchsisIdle[axis] - fsdlJoyStick.Axis[axis]);
    GotoStep(fStep + 1);
  End;
End;

Function TSDLWizzard.ButtonName(aButton: Integer): String;
Begin
  If Device.IsGameController Then Begin
    result := GameControllerButtonToString(aButton);
  End
  Else Begin
    result := format(RF_Button, [aButton + 1]);
  End;
End;

Function TSDLWizzard.StepResult(aState: TWizzardState): String;
Begin
  result := '';
  Case aState Of
    wsFirstAction: If Device.FirstButtonIndex >= 0 Then result := ButtonName(Device.FirstButtonIndex);
    wsSecondAction: If Device.SecondButtonIndex >= 0 Then result := ButtonName(Device.SecondButtonIndex);
    wsUp: Begin
        If fHatOnly Then Begin
          result := R_DirHat;
        End
        Else If Device.UpDownIndex >= 0 Then Begin
          result := format(RF_WizAxis, [Device.UpDownIndex + 1]);
        End;
      End;
    wsLeft: Begin
        If fHatOnly Then Begin
          result := R_DirHat;
        End
        Else If Device.LeftRightIndex >= 0 Then Begin
          result := format(RF_WizAxis, [Device.LeftRightIndex + 1]);
        End;
      End;
  End;
End;

Procedure TSDLWizzard.FocusNext;
Var
  c: TWinControl;
Begin
  // Ist Weiter gesperrt, dann bekommt kein Button den Fokus, sonst würde Enter z.B. Abbrechen auslösen
  If btNext.Enabled Then Begin
    c := btNext;
  End
  Else Begin
    c := StepPanel;
  End;
  // Vor dem Anzeigen geht SetFocus nicht, dann bekommt es den Fokus beim Anzeigen
  If Showing And c.CanFocus Then Begin
    c.SetFocus;
  End
  Else If c.Enabled Then Begin
    ActiveControl := c;
  End;
End;

Procedure TSDLWizzard.SetError(Const aText: String);
Begin
  lbError.Caption := aText;
  lbError.Visible := aText <> '';
End;

Constructor TSDLWizzard.CreateNew(AOwner: TComponent; Num: Integer);

  Function CreateLabel(aParent: TWinControl; aCaption: String; aTop, aLeft: integer): TLabel;
  Begin
    result := TLabel.Create(self);
    result.Parent := aParent;
    result.Caption := aCaption;
    result.Left := aLeft;
    result.Top := aTop;
  End;

  Function CreateButton(aParent: TWinControl; aCaption: String; aTop, aLeft, aWidth: Integer): TButton;
  Begin
    result := TButton.Create(self);
    result.Parent := aParent;
    result.Caption := aCaption;
    result.Left := aLeft;
    result.Top := aTop;
    result.Width := aWidth;
  End;

  Function CreatePanel(aTop, aHeight: Integer): TPanel;
  Begin
    result := TPanel.Create(self);
    result.Parent := self;
    result.BevelOuter := bvNone;
    result.Caption := '';
    result.Left := 0;
    result.Top := aTop;
    result.Width := FormWidth;
    result.Height := aHeight;
  End;

  Function CreateArrow(aTop, aLeft: Integer): TSpeedButton;
  Begin
    result := TSpeedButton.Create(self);
    result.Parent := gbLive;
    result.Images := ImageList1;
    result.ImageIndex := -1;
    result.Left := aLeft;
    result.Top := aTop;
    result.Width := ArrowSize;
    result.Height := ArrowSize;
  End;

Var
  s: TWizzardState;
  x: Integer;
Begin
  Inherited CreateNew(AOwner, Num);
  fsdlJoyStick := Nil;
  fState := wsSelectDevice;
  fHatOnly := false;
  fHoldAxis := HoldNone;
  fWaitStep := wsDone;
  ClientWidth := FormWidth;
  ClientHeight := ButtonTop + 25 + Margin;
  BorderStyle := bsDialog;
  FormStyle := fsNormal;
  Position := poScreenCenter;
  Caption := R_WizCaption;

  ImageList1 := TImageList.Create(self);
  ImageList1.Width := 32;
  ImageList1.Height := 32;
  ImageList1.AddLazarusResource('Arror_up', clFuchsia);
  ImageList1.AddLazarusResource('Arror_down', clFuchsia);
  ImageList1.AddLazarusResource('Arror_left', clFuchsia);
  ImageList1.AddLazarusResource('Arror_right', clFuchsia);

  // Überschrift und Anweisung, gelten für alle Schritte
  lbHeader := CreateLabel(self, '', Margin, Margin);
  lbHeader.Font.Style := [fsBold];
  lbHeader.Font.Size := 16;
  lbHint := CreateLabel(self, '', 48, Margin);
  lbHint.Font.Size := 12;
  lbHint.AutoSize := false;
  lbHint.WordWrap := true;
  lbHint.Width := FormWidth - 2 * Margin;
  lbHint.Height := 48;

  // Gerät wählen
  DevicePanel := CreatePanel(PanelTop, PanelHeight);
  cbDevice := TComboBox.Create(self);
  cbDevice.Parent := DevicePanel;
  cbDevice.Style := csDropDownList;
  cbDevice.Left := 120;
  cbDevice.Top := 4;
  cbDevice.Width := FormWidth - 120 - Margin;
  cbDevice.OnChange := @OnDeviceChange;
  lbDevice := CreateLabel(DevicePanel, R_WizDevice, 4, Margin);
  lbDevice.FocusControl := cbDevice;
  // Das Label steht mittig neben der Combobox, egal wie hoch das Widgetset die Combobox macht
  lbDevice.AnchorSideTop.Control := cbDevice;
  lbDevice.AnchorSideTop.Side := asrCenter;
  btRefresh := CreateButton(DevicePanel, R_WizRefresh, 40, 120, 200);
  btRefresh.OnClick := @OnRefreshDeviceList;
  rbGameController := TRadioButton.Create(self);
  rbGameController.Parent := DevicePanel;
  rbGameController.Caption := R_WizAsGameController;
  rbGameController.Left := 120;
  rbGameController.Top := 84;
  rbJoystick := TRadioButton.Create(self);
  rbJoystick.Parent := DevicePanel;
  rbJoystick.Caption := R_WizAsJoystick;
  rbJoystick.Left := 120;
  rbJoystick.Top := 110;

  // Die Schritte: Checkliste und Live-Anzeige
  StepPanel := CreatePanel(PanelTop, PanelHeight);
  StepPanel.Visible := false;
  For s := Low(TWizzardState) To High(TWizzardState) Do Begin
    lbStep[s] := CreateLabel(StepPanel, '', 4, 16);
    lbStep[s].Font.Size := 11;
    lbStep[s].Visible := false;
  End;
  gbLive := TGroupBox.Create(self);
  gbLive.Parent := StepPanel;
  gbLive.Caption := R_WizLive;
  gbLive.Left := Margin;
  gbLive.Top := PanelHeight - 136 - 4; // Unten, darüber ist Platz für die Checkliste
  gbLive.Width := FormWidth - 2 * Margin;
  gbLive.Height := 136;
  x := gbLive.Width - 16 - 3 * ArrowSize;
  sbUp := CreateArrow(0, x + ArrowSize);
  sbLeft := CreateArrow(ArrowSize, x);
  sbRight := CreateArrow(ArrowSize, x + 2 * ArrowSize);
  sbDown := CreateArrow(2 * ArrowSize, x + ArrowSize);

  lbError := CreateLabel(self, '', PanelTop + PanelHeight + 4, Margin);
  lbError.Font.Color := clRed;

  // Unten: Abbrechen links, Zurück und Weiter rechts
  btCancel := CreateButton(self, R_Cancel, ButtonTop, Margin, 100);
  btCancel.ModalResult := mrCancel;
  btCancel.Cancel := true;
  btBack := CreateButton(self, R_WizBack, ButtonTop, FormWidth - 2 * Margin - 2 * 110, 110);
  btBack.OnClick := @OnBack;
  btNext := CreateButton(self, R_WizNext, ButtonTop, FormWidth - Margin - 110, 110);
  btNext.Default := true;
  btNext.OnClick := @OnNext;

  Timer1 := TTimer.Create(self);
  Timer1.Enabled := false;
  Timer1.Interval := 20;
  Timer1.OnTimer := @OnTimer;

  OnRefreshDeviceList(Nil);
  ShowDeviceSelection;
  FitLayout(self); // Platz für die Texte im aktuellen Widgetset und in der aktuellen Sprache
End;

Destructor TSDLWizzard.Destroy;
Begin
  CloseDevice;
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

