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
  Classes, SysUtils, forms, StdCtrls, ExtCtrls, Controls, ukeygrabber, uatomic_common;

Type

  TSDLState = (ssUnknown, ssReady, ssFailed);

  // Alle Controls einer Spieler Spalte
  TPlayerControls = Record
    Box: TGroupBox;
    Keyboard, Controller: TRadioButton;
    // Tastatur
    KeyPanel: TPanel;
    KeyName, KeyValue: Array[0..5] Of TLabel;
    KeyGrabber: Array[0..5] Of TButton;
    // Controller
    PadPanel: TPanel;
    Status, Shared, Mapping, Directions: TLabel;
    Setup: TButton;
    AnalogStick, Hat, Hysteresis, DominantAxis: TCheckBox;
  End;

  { TKeyboardDialog }

  TKeyboardDialog = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Timer1: TTimer;
    Procedure OnKeyGrabberEvent(Sender: TObject);
    Procedure OnSetDefaultEvent(Sender: TObject);
    Procedure OnModeEvent(Sender: TObject);
    Procedure OnSetupEvent(Sender: TObject);
    Procedure OnOptionEvent(Sender: TObject);
    Procedure OnStatusTimer(Sender: TObject);
  private
    fkeys: Array[TKeySet] Of TKeys;
    fPlayer: Array[TKeySet] Of TPlayerControls;
    fUpdating: Boolean; // Verhindert, dass das Setzen der Controls deren Events auslöst
    fSDLState: TSDLState;
    fSDLError: String;
    Function InitSDL(ShowErrors: Boolean): Boolean;
    Function RunWizzard(aKeySet: TKeySet): Boolean;
    Function DeviceStatus(aKeySet: TKeySet; Out Ok: Boolean): String;
    Function SharedWith(aKeySet: TKeySet): Integer; // 0 oder die Nummer (1, 2) des anderen Spielers, der den selben Controller nutzt
    Procedure AssignHotkeys;
    Procedure UpdateTabOrder;
    Procedure OnFormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure UpdateControls;
    Procedure UpdateStatus;
  public
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    Destructor Destroy; override;
    Procedure LoadKeys(Keys0, keys1: Tkeys);
    Function GetKeys(Index: TKeySet): TKeys;
    Function Execute(): Boolean; // True, wenn Erfolgreich ausgeführt
  End;

Implementation

Uses SDL2, Dialogs, Graphics, LCLType, LazUTF8, usdlwizzard, usdl_joystick, usdl_gamecontroller, ulayout
{$IFDEF Launcher}
  , ulanguage
{$ENDIF}
  ;

{$IFNDEF Launcher}
Const
  R_KeyboardSettings = 'Configure controls';
  R_Player0 = 'Player 1';
  R_Player1 = 'Player 2';
  R_ModeKeyboard = 'Keyboard';
  R_ModeController = 'Controller';
  R_KeyUp = 'Key Up:';
  R_KeyDown = 'Key Down:';
  R_KeyLeft = 'Key Left:';
  R_KeyRight = 'Key Right:';
  R_KeyPrimary = 'Key Primary:';
  R_KeySecondary = 'Key Secondary:';
  R_Grab = 'Grab';
  R_Default = 'default';
  R_Cancel = 'Cancel';
  RF_SameController = 'Same controller as player %d';
  RF_SameControllerInfo = 'Player %d already uses this controller, it will move both players at the same time.';
  R_Status = 'Status:';
  R_Connected = 'connected';
  R_NotConnected = 'not connected';
  R_NoDevice = 'no controller set up';
  R_SetupAgain = 'please set up again';
  R_NoSDL = 'SDL2 not available';
  R_Directions = 'Directions:';
  RF_DirAxes = 'axes %d and %d';
  R_DirHat = 'hat';
  R_DirDPad = 'D-Pad';
  RF_Button = 'Button %d';
  R_SetupController = 'Set up controller...';
  R_AnalogStick = 'Use analog stick';
  R_UseHat = 'Use hat (D-Pad)';
  R_Hysteresis = 'Hysteresis (no flickering)';
  R_DominantAxis = 'Diagonal: stronger axis wins';
{$ENDIF}

Const
  BoxWidth = 320;
  BoxHeight = 296;
  Margin = 8;
  MaxNameLength = 28; // Längere Controller Namen werden gekürzt

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
  UpdateControls;
End;

Procedure TKeyboardDialog.OnSetDefaultEvent(Sender: TObject);
Begin
  LoadKeys(AtomicDefaultKeys(ks0), AtomicDefaultKeys(ks1));
End;

Procedure TKeyboardDialog.OnModeEvent(Sender: TObject);
Var
  ks: TKeySet;
Begin
  // Nur der gewählte Radiobutton zählt, je nach Widgetset meldet sich auch der abgewählte
  If fUpdating Or (Not TRadioButton(Sender).Checked) Then exit;
  ks := TKeySet(TComponent(Sender).Tag);
  If Sender = fPlayer[ks].Controller Then Begin
    // Ist noch kein Controller eingerichtet, dann gleich den Wizzard starten, bricht der Spieler ab, bleibt es bei der Tastatur
    If fkeys[ks].Name = '' Then Begin
      RunWizzard(ks);
    End
    Else Begin
      fkeys[ks].UseSDL2 := true;
    End;
  End
  Else Begin
    fkeys[ks].UseSDL2 := false;
  End;
  UpdateControls;
End;

Procedure TKeyboardDialog.OnSetupEvent(Sender: TObject);
Begin
  RunWizzard(TKeySet(TComponent(Sender).Tag));
  UpdateControls;
End;

Procedure TKeyboardDialog.OnOptionEvent(Sender: TObject);
Var
  ks: TKeySet;
Begin
  If fUpdating Then exit;
  ks := TKeySet(TComponent(Sender).Tag);
  fkeys[ks].UseAnalogStick := fPlayer[ks].AnalogStick.Checked;
  fkeys[ks].UseHat := fPlayer[ks].Hat.Checked;
  fkeys[ks].UseHysteresis := fPlayer[ks].Hysteresis.Checked;
  fkeys[ks].UseDominantAxis := fPlayer[ks].DominantAxis.Checked;
  UpdateControls; // Ob die Diagonale greift, hängt am Analogstick
End;

Procedure TKeyboardDialog.OnStatusTimer(Sender: TObject);
Var
  event: TSDL_Event;
Begin
  // Damit ein- und ausgesteckte Controller erkannt werden, muss SDL seine Events verarbeiten dürfen
  If fSDLState <> ssReady Then exit;
  If Not (fkeys[ks0].UseSDL2 Or fkeys[ks1].UseSDL2) Then exit;
  SDL_PumpEvents();
  While SDL_PollEvent(@event) <> 0 Do Begin
  End;
  UpdateStatus;
End;

Function TKeyboardDialog.InitSDL(ShowErrors: Boolean): Boolean;
Var
  ver: TSDL_Version;
Begin
  If fSDLState = ssUnknown Then Begin
    fSDLState := ssFailed;
    If Not SDL_LoadLib('') Then Begin
      fSDLError := 'Error, could not load sdl2 lib.' + LineEnding +
        'FPC_Atomic needs to have at least SDL2 version: 2.14' + LineEnding + LineEnding +
        'Please'
{$IFDEF Windows}
      + ' download sdl2.dll and store it into fpc_atomic root folder'
{$ELSE}
      + ' run:' + LineEnding + '  sudo aptitude install libsdl2-dev'
{$ENDIF}
      + LineEnding + LineEnding +
        'and retry.' + LineEnding + LineEnding +
        'Actual installed version is: ' + GetSDL2VersionByManualDynamicLoading('');
    End
    Else Begin
      If SDL_Init(SDL_INIT_GAMECONTROLLER) <> 0 Then Begin
        fSDLError := 'Error could not init SDL';
      End
      Else Begin
        SDL_GetVersion(@ver);
        If SDL_VERSIONNUM(ver.major, ver.minor, ver.patch) < SDL_COMPILEDVERSION Then Begin
          fSDLError := format('The version of sdl2 (%d.%d.%d) is to low you have to have at least %d.%d.%d', [ver.major, ver.minor, ver.patch, SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL]);
          SDL_Quit;
        End
        Else Begin
          fSDLState := ssReady;
        End;
      End;
    End;
  End;
  result := fSDLState = ssReady;
  If (Not result) And ShowErrors Then showmessage(fSDLError);
End;

Function TKeyboardDialog.RunWizzard(aKeySet: TKeySet): Boolean;

  Function IdleButton(Const Values: Array Of Boolean; aIndex: Integer): Boolean;
  Begin
    result := false;
    If (aIndex >= 0) And (aIndex <= high(Values)) Then result := Values[aIndex];
  End;

  Function IdleAxis(Const Values: Array Of Integer; aIndex: Integer): Integer;
  Begin
    result := 0;
    If (aIndex >= 0) And (aIndex <= high(Values)) Then result := Values[aIndex];
  End;

Var
  w: TSDLWizzard;
  Info: TDeviceInfo;
Begin
  result := false;
  If Not InitSDL(true) Then exit;
  // Der Timer würde sonst dem Wizzard die SDL Events weg nehmen
  Timer1.Enabled := false;
  w := TSDLWizzard.CreateNew(self);
  Try
    If w.execute Then Begin
      Info := w.Device;
      fkeys[aKeySet].UseSDL2 := true;
      fkeys[aKeySet].Name := Info.Name;
      fkeys[aKeySet].NameIndex := Info.Nameindex;
      fkeys[aKeySet].IsGameController := Info.IsGameController;
      fkeys[aKeySet].ButtonIndex[0] := Info.FirstButtonIndex;
      fkeys[aKeySet].ButtonIndex[1] := Info.SecondButtonIndex;
      fkeys[aKeySet].ButtonsIdle[0] := IdleButton(Info.ButtonsIdle, Info.FirstButtonIndex);
      fkeys[aKeySet].ButtonsIdle[1] := IdleButton(Info.ButtonsIdle, Info.SecondButtonIndex);
      // Ein Index von -1 bedeutet, es wird nur mit dem Hat gesteuert
      fkeys[aKeySet].AchsisIndex[0] := Info.UpDownIndex;
      fkeys[aKeySet].AchsisIdle[0] := IdleAxis(Info.AchsisIdle, Info.UpDownIndex);
      fkeys[aKeySet].AchsisDirection[0] := Info.UpDirection;
      fkeys[aKeySet].AchsisIndex[1] := Info.LeftRightIndex;
      fkeys[aKeySet].AchsisIdle[1] := IdleAxis(Info.AchsisIdle, Info.LeftRightIndex);
      fkeys[aKeySet].AchsisDirection[1] := Info.LeftDirection;
      fkeys[aKeySet].MappingVersion := SDLMappingVersion;
      result := true;
    End;
  Finally
    w.free;
    Timer1.Enabled := true;
  End;
  // Das ist erlaubt, aber vermutlich ein Versehen
  If result And (SharedWith(aKeySet) <> 0) Then Begin
    showmessage(format(RF_SameControllerInfo, [SharedWith(aKeySet)]));
  End;
End;

Function TKeyboardDialog.SharedWith(aKeySet: TKeySet): Integer;
Var
  ks: TKeySet;
Begin
  result := 0;
  For ks In TKeySet Do Begin
    If (ks = aKeySet) Or (Not fkeys[ks].UseSDL2) Or (Not fkeys[aKeySet].UseSDL2) Then Continue;
    If (fkeys[ks].Name = fkeys[aKeySet].Name) And (fkeys[ks].NameIndex = fkeys[aKeySet].NameIndex) And
      (fkeys[ks].IsGameController = fkeys[aKeySet].IsGameController) Then Begin
      result := ord(ks) + 1;
    End;
  End;
End;

(*
 * Vergibt die Tastenkürzel (Alt + Buchstabe) automatisch, so passen sie in jeder Sprache und auch zum Namen
 * des Controllers. Bevorzugt werden Wortanfänge, jeder Buchstabe nur einmal.
 *)
Procedure TKeyboardDialog.AssignHotkeys;
Var
  Used: Set Of Char;

  Function Hotkey(aCaption: String): String;
  Var
    i, best: Integer;
    c: Char;
  Begin
    aCaption := StringReplace(aCaption, '&', '', [rfReplaceAll]);
    best := 0;
    For i := 1 To length(aCaption) Do Begin
      c := upcase(aCaption[i]);
      If Not (c In ['A'..'Z', '0'..'9']) Or (c In Used) Then Continue;
      If (i = 1) Or (aCaption[i - 1] = ' ') Then Begin
        best := i;
        break;
      End;
      If best = 0 Then best := i;
    End;
    If best = 0 Then exit(aCaption);
    include(Used, upcase(aCaption[best]));
    result := copy(aCaption, 1, best - 1) + '&' + copy(aCaption, best, length(aCaption));
  End;

Var
  ks: TKeySet;
Begin
  Used := ['1', '2']; // Alt+1 und Alt+2 springen zu den Spielern, siehe OnFormKeyDown
  For ks In TKeySet Do Begin
    fPlayer[ks].Keyboard.Caption := Hotkey(fPlayer[ks].Keyboard.Caption);
    fPlayer[ks].Controller.Caption := Hotkey(fPlayer[ks].Controller.Caption);
    fPlayer[ks].Setup.Caption := Hotkey(fPlayer[ks].Setup.Caption);
    fPlayer[ks].AnalogStick.Caption := Hotkey(fPlayer[ks].AnalogStick.Caption);
    fPlayer[ks].Hat.Caption := Hotkey(fPlayer[ks].Hat.Caption);
    fPlayer[ks].Hysteresis.Caption := Hotkey(fPlayer[ks].Hysteresis.Caption);
    fPlayer[ks].DominantAxis.Caption := Hotkey(fPlayer[ks].DominantAxis.Caption);
  End;
  Button3.Caption := Hotkey(Button3.Caption);
  Button2.Caption := Hotkey(Button2.Caption);
  Button1.Caption := Hotkey(Button1.Caption);
End;

Function TKeyboardDialog.DeviceStatus(aKeySet: TKeySet; Out Ok: Boolean
  ): String;
Var
  index: Integer;
Begin
  Ok := false;
  If fkeys[aKeySet].Name = '' Then exit(R_NoDevice);
  If Not InitSDL(false) Then exit(R_NoSDL);
  // Alte Konfigurationen haben bei Gamecontrollern die rohen Joystick Button Indizes gespeichert
  If fkeys[aKeySet].IsGameController And (fkeys[aKeySet].MappingVersion < SDLMappingVersion) Then exit(R_SetupAgain);
  If fkeys[aKeySet].IsGameController Then Begin
    index := ResolveGameControllerNameToIndex(fkeys[aKeySet].Name, fkeys[aKeySet].NameIndex);
  End
  Else Begin
    index := ResolveJoystickNameToIndex(fkeys[aKeySet].Name, fkeys[aKeySet].NameIndex);
  End;
  If index = -1 Then exit(R_NotConnected);
  Ok := true;
  result := R_Connected;
End;

Procedure TKeyboardDialog.UpdateStatus;
Var
  ks: TKeySet;
  Ok: Boolean;
Begin
  For ks In TKeySet Do Begin
    If Not fkeys[ks].UseSDL2 Then Continue;
    fPlayer[ks].Status.Caption := R_Status + ' ' + DeviceStatus(ks, Ok);
    If Ok Then Begin
      fPlayer[ks].Status.Font.Color := clGreen;
    End
    Else Begin
      fPlayer[ks].Status.Font.Color := clRed;
    End;
  End;
End;

Procedure TKeyboardDialog.UpdateControls;

  Function KeyOf(ks: TKeySet; i: Integer): Word;
  Begin
    Case i Of
      0: result := fkeys[ks].KeyUp;
      1: result := fkeys[ks].KeyDown;
      2: result := fkeys[ks].KeyLeft;
      3: result := fkeys[ks].KeyRight;
      4: result := fkeys[ks].KeyPrimary;
    Else
      result := fkeys[ks].KeySecondary;
    End;
  End;

  // Wie oft die Taste von Spielern belegt ist, die mit der Tastatur spielen
  Function KeyUsage(key: Word): Integer;
  Var
    ks: TKeySet;
    i: Integer;
  Begin
    result := 0;
    For ks In TKeySet Do Begin
      If fkeys[ks].UseSDL2 Then Continue;
      For i := 0 To 5 Do Begin
        If KeyOf(ks, i) = key Then inc(result);
      End;
    End;
  End;

  Function ButtonName(ks: TKeySet; aButton: Integer): String;
  Begin
    If fkeys[ks].IsGameController Then Begin
      result := GameControllerButtonToString(aButton);
    End
    Else Begin
      result := format(RF_Button, [aButton + 1]);
    End;
  End;

Var
  ks: TKeySet;
  i: Integer;
  s: String;
Begin
  fUpdating := true;
  For ks In TKeySet Do Begin
    fPlayer[ks].Keyboard.Checked := Not fkeys[ks].UseSDL2;
    fPlayer[ks].Controller.Checked := fkeys[ks].UseSDL2;
    fPlayer[ks].KeyPanel.Visible := Not fkeys[ks].UseSDL2;
    fPlayer[ks].PadPanel.Visible := fkeys[ks].UseSDL2;
    // Tastatur, doppelt belegte Tasten werden rot
    For i := 0 To 5 Do Begin
      fPlayer[ks].KeyValue[i].Caption := VkKeyToString(KeyOf(ks, i));
      If KeyUsage(KeyOf(ks, i)) > 1 Then Begin
        fPlayer[ks].KeyValue[i].Font.Color := clRed;
      End
      Else Begin
        fPlayer[ks].KeyValue[i].Font.Color := clDefault;
      End;
    End;
    // Controller, der Radiobutton zeigt den Namen des eingerichteten Geräts (ein & wäre ein Tastenkürzel)
    s := R_ModeController;
    If fkeys[ks].Name <> '' Then Begin
      s := StringReplace(fkeys[ks].Name, '&', '+', [rfReplaceAll]);
      If UTF8Length(s) > MaxNameLength Then s := UTF8Copy(s, 1, MaxNameLength - 1) + '…';
      If fkeys[ks].NameIndex > 0 Then s := s + format(' (#%d)', [fkeys[ks].NameIndex + 1]);
    End;
    fPlayer[ks].Controller.Caption := s;
    i := SharedWith(ks);
    fPlayer[ks].Shared.Visible := i <> 0;
    If i <> 0 Then fPlayer[ks].Shared.Caption := format(RF_SameController, [i]);
    If fkeys[ks].Name <> '' Then Begin
      fPlayer[ks].Mapping.Caption :=
        R_KeyPrimary + ' ' + ButtonName(ks, fkeys[ks].ButtonIndex[0]) + LineEnding +
        R_KeySecondary + ' ' + ButtonName(ks, fkeys[ks].ButtonIndex[1]);
      If fkeys[ks].IsGameController Then Begin
        s := R_DirDPad;
      End
      Else If fkeys[ks].AchsisIndex[0] >= 0 Then Begin
        s := format(RF_DirAxes, [fkeys[ks].AchsisIndex[0] + 1, fkeys[ks].AchsisIndex[1] + 1]);
      End
      Else Begin
        s := R_DirHat;
      End;
      fPlayer[ks].Directions.Caption := R_Directions + ' ' + s;
    End
    Else Begin
      fPlayer[ks].Mapping.Caption := '';
      fPlayer[ks].Directions.Caption := '';
    End;
    fPlayer[ks].AnalogStick.Checked := fkeys[ks].UseAnalogStick;
    fPlayer[ks].Hat.Checked := fkeys[ks].UseHat;
    fPlayer[ks].Hysteresis.Checked := fkeys[ks].UseHysteresis;
    // Was für den Gerätetyp nicht greift, wird nur ausgegraut, damit man sieht, dass es die Option gibt
    fPlayer[ks].AnalogStick.Enabled := fkeys[ks].IsGameController;
    fPlayer[ks].Hat.Enabled := Not fkeys[ks].IsGameController;
    // Die Diagonale gibt es nur, wenn ein Stick oder Achsen benutzt werden
    fPlayer[ks].DominantAxis.Checked := fkeys[ks].UseDominantAxis;
    If fkeys[ks].IsGameController Then Begin
      fPlayer[ks].DominantAxis.Enabled := fkeys[ks].UseAnalogStick;
    End
    Else Begin
      fPlayer[ks].DominantAxis.Enabled := fkeys[ks].AchsisIndex[0] >= 0;
    End;
  End;
  UpdateStatus;
  AssignHotkeys;
  fUpdating := false;
  FitLayout(self); // Die Namen der Tasten und Geräte sind unterschiedlich lang
  UpdateTabOrder;
End;

(*
 * Die Tab Reihenfolge so, wie man es sieht: Spieler 1 von oben nach unten, dann Spieler 2, dann die Buttons unten.
 * Die LCL vergibt sie beim Erzeugen nicht zuverlässig (die Radiobuttons landeten hinter den Panels).
 *)
Procedure TKeyboardDialog.UpdateTabOrder;
Var
  ks: TKeySet;
  i: Integer;
Begin
  For ks In TKeySet Do Begin
    With fPlayer[ks] Do Begin
      Box.TabOrder := ord(ks);
      (*
       * Nur der gewählte Radiobutton ist per Tab erreichbar (zwischen beiden wechseln die Pfeiltasten).
       * Unter Windows wird ein Radiobutton gewählt, sobald er den Fokus bekommt, der nicht gewählte darf ihn also
       * nie automatisch bekommen (z.B. beim Anzeigen), sonst würde der Spieler umgeschaltet.
       *)
      Keyboard.TabStop := Keyboard.Checked;
      Controller.TabStop := Controller.Checked;
      If Controller.Checked Then Begin
        Controller.TabOrder := 0;
        Keyboard.TabOrder := 1;
      End
      Else Begin
        Keyboard.TabOrder := 0;
        Controller.TabOrder := 1;
      End;
      KeyPanel.TabOrder := 2;
      PadPanel.TabOrder := 3;
      For i := 0 To 5 Do Begin
        KeyGrabber[i].TabOrder := i;
      End;
      Setup.TabOrder := 0;
      AnalogStick.TabOrder := 1;
      Hat.TabOrder := 2;
      Hysteresis.TabOrder := 3;
      DominantAxis.TabOrder := 4;
    End;
  End;
  Button3.TabOrder := 2;
  Button2.TabOrder := 3;
  Button1.TabOrder := 4;
End;

Procedure TKeyboardDialog.OnFormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  ks: TKeySet;
  c: TWinControl;
Begin
  // Alt+1 / Alt+2 springt zum gewählten Radiobutton des Spielers
  If (Shift * [ssAlt, ssCtrl, ssShift] <> [ssAlt]) Or Not (Key In [VK_1, VK_2, VK_NUMPAD1, VK_NUMPAD2]) Then exit;
  If Key In [VK_1, VK_NUMPAD1] Then Begin
    ks := ks0;
  End
  Else Begin
    ks := ks1;
  End;
  If fkeys[ks].UseSDL2 Then Begin
    c := fPlayer[ks].Controller;
  End
  Else Begin
    c := fPlayer[ks].Keyboard;
  End;
  If c.CanFocus Then c.SetFocus;
  Key := 0;
End;

Constructor TKeyboardDialog.CreateNew(AOwner: TComponent; Num: Integer);

  Function CreateLabel(aParent: TWinControl; aCaption: String; aTop, aLeft: integer): TLabel;
  Begin
    result := TLabel.Create(self);
    result.Parent := aParent;
    result.Caption := aCaption;
    result.Left := aLeft;
    result.Top := aTop;
  End;

  Function CreateButton(aParent: TWinControl; aCaption: String; aTop, aLeft, aWidth: Integer; aResult: TModalResult): TButton;
  Begin
    result := TButton.Create(self);
    result.Parent := aParent;
    result.Caption := aCaption;
    result.ModalResult := aResult;
    result.Left := aLeft;
    result.Top := aTop;
    result.Width := aWidth;
  End;

  Function CreatePanel(aParent: TWinControl; aTop: Integer): TPanel;
  Begin
    result := TPanel.Create(self);
    result.Parent := aParent;
    result.BevelOuter := bvNone;
    result.Caption := '';
    result.Left := 0;
    result.Top := aTop;
    result.Width := BoxWidth - 2 * Margin;
    result.Height := BoxHeight - aTop - 3 * Margin;
  End;

  Function CreateRadioButton(aParent: TWinControl; aCaption: String; aLeft: Integer; aKeySet: TKeySet): TRadioButton;
  Begin
    result := TRadioButton.Create(self);
    result.Parent := aParent;
    result.Caption := aCaption;
    result.Left := aLeft;
    result.Top := 4;
    result.Tag := ord(aKeySet);
    result.OnClick := @OnModeEvent;
  End;

  Function CreateCheckBox(aParent: TWinControl; aCaption: String; aTop: Integer; aKeySet: TKeySet): TCheckBox;
  Begin
    result := TCheckBox.Create(self);
    result.Parent := aParent;
    result.Caption := aCaption;
    result.Left := Margin;
    result.Top := aTop;
    result.Tag := ord(aKeySet);
    result.OnClick := @OnOptionEvent;
  End;

  Procedure CreatePlayer(aKeySet: TKeySet; aCaption: String; aLeft: Integer);
  Const
    RowHeight = 30;
  Var
    KeyNames: Array[0..5] Of String;
    i: Integer;
  Begin
    KeyNames[0] := R_KeyUp;
    KeyNames[1] := R_KeyDown;
    KeyNames[2] := R_KeyLeft;
    KeyNames[3] := R_KeyRight;
    KeyNames[4] := R_KeyPrimary;
    KeyNames[5] := R_KeySecondary;
    With fPlayer[aKeySet] Do Begin
      Box := TGroupBox.Create(self);
      Box.Parent := self;
      Box.Caption := aCaption + ' (Alt+' + inttostr(ord(aKeySet) + 1) + ')';
      Box.Left := aLeft;
      Box.Top := Margin;
      Box.Width := BoxWidth;
      Box.Height := BoxHeight;
      Keyboard := CreateRadioButton(Box, R_ModeKeyboard, Margin, aKeySet);
      // Weiter links, der Radiobutton zeigt den Namen des Controllers, FitLayout schiebt ihn bei Bedarf nach rechts
      Controller := CreateRadioButton(Box, R_ModeController, 104, aKeySet);
      // Tastatur, die Tags 1 .. 12 siehe OnKeyGrabberEvent
      KeyPanel := CreatePanel(Box, 32);
      For i := 0 To 5 Do Begin
        KeyName[i] := CreateLabel(KeyPanel, KeyNames[i], 8 + i * RowHeight, Margin);
        KeyValue[i] := CreateLabel(KeyPanel, '?', 8 + i * RowHeight, 120);
        KeyGrabber[i] := CreateButton(KeyPanel, R_Grab, 4 + i * RowHeight, 210, 85, mrNone);
        KeyGrabber[i].Tag := ord(aKeySet) * 6 + i + 1;
        KeyGrabber[i].OnClick := @OnKeyGrabberEvent;
      End;
      // Controller, liegt an der selben Stelle wie die Tastatur, sichtbar ist immer nur eins von beiden
      PadPanel := CreatePanel(Box, 32);
      Status := CreateLabel(PadPanel, R_Status, 4, Margin);
      Shared := CreateLabel(PadPanel, '', 24, Margin);
      Shared.Font.Color := clRed;
      Mapping := CreateLabel(PadPanel, '', 48, Margin);
      Directions := CreateLabel(PadPanel, '', 84, Margin);
      Setup := CreateButton(PadPanel, R_SetupController, 106, Margin, 200, mrNone);
      Setup.Tag := ord(aKeySet);
      Setup.OnClick := @OnSetupEvent;
      AnalogStick := CreateCheckBox(PadPanel, R_AnalogStick, 144, aKeySet);
      Hat := CreateCheckBox(PadPanel, R_UseHat, 168, aKeySet);
      Hysteresis := CreateCheckBox(PadPanel, R_Hysteresis, 192, aKeySet);
      DominantAxis := CreateCheckBox(PadPanel, R_DominantAxis, 216, aKeySet);
    End;
  End;

Begin
  Inherited CreateNew(AOwner, Num);
  KeyPreview := true;
  OnKeyDown := @OnFormKeyDown;
  fUpdating := false;
  fSDLState := ssUnknown;
  fSDLError := '';
  width := 2 * BoxWidth + 3 * Margin;
  height := BoxHeight + 3 * Margin + 25;
  BorderStyle := bsDialog;
  FormStyle := fsNormal;
  Position := poScreenCenter;
  Caption := R_KeyboardSettings;

  CreatePlayer(ks0, R_Player0, Margin);
  CreatePlayer(ks1, R_Player1, 2 * Margin + BoxWidth);

  Button3 := CreateButton(self, R_Default, BoxHeight + 2 * Margin, Margin, 120, mrNone);
  Button3.OnClick := @OnSetDefaultEvent;
  Button2 := CreateButton(self, R_Cancel, BoxHeight + 2 * Margin, width - 2 * Margin - 2 * 85, 85, mrCancel);
  Button2.Cancel := true; // Escape
  Button1 := CreateButton(self, 'OK', BoxHeight + 2 * Margin, width - Margin - 85, 85, mrOK);

  Timer1 := TTimer.Create(self);
  Timer1.Interval := 1000;
  Timer1.OnTimer := @OnStatusTimer;
  Timer1.Enabled := true;

  LoadKeys(AtomicDefaultKeys(ks0), AtomicDefaultKeys(ks1));
End;

Destructor TKeyboardDialog.Destroy;
Begin
  Timer1.Enabled := false;
  // Wer den Dialog aufruft, initialisiert SDL danach selbst wieder, wenn er es braucht
  If fSDLState = ssReady Then SDL_Quit;
  Inherited Destroy;
End;

Procedure TKeyboardDialog.LoadKeys(Keys0, keys1: Tkeys);
Begin
  fkeys[ks0] := Keys0;
  fkeys[ks1] := Keys1;
  UpdateControls;
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

