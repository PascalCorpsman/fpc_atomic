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
Unit uscreens;

{$MODE ObjFPC}{$H+}

Interface

{$I globaldefines.inc}

Uses
  forms, Classes, SysUtils, StdCtrls, controls, uatomic_common, uopengl_animation, uatomic_field;

Type

  (*
   * Im MainMenu die diversen Screens
   *)
  TScreenEnum = (
    sMainScreen // Hauptscreen
    //, sSinglePlayer -- ??
    , sHost // Den Gibt es gar nicht, der wird direkt auf den sJoinNetwork umgeleitet
    , sJoinNetwork // Alle Spieler gehen zusammen in das Spiel rein
    , sPlayerSetupRequest // Anfrage zum Wechsel in sPlayerSetup
    , sPlayerSetup // Die Spieler dürfen sich ihre Farben "aussuchen"
    , sEditFieldSetupRequest // Anfrage zum Einstellen der Field einstellungen
    , sEditFieldSetup // Der Master stellt die eigenschaften des fields ein
    , sDrawGame // Der Screen der Angezeigt wird wenn ein Unentschieden gespielt wurde
    , sMatchStatistik // Der Screen der die Match Statistik anzeigt
    , sVictory // Der Screen der das Wer hat gewonnen bild anzeigt
    , sOptions // Der Optionen Dialog
    //, sOnlineManual -- ??
    , sExitBomberman // byby
    );

  { TScreen }

  TScreen = Class
  private
    fOwner: TObject;
    fBackFile: String;
    fBackTex: Integer; // OpenGL Pointer der fBackFile
    fSoundFile: String;
    fSoundExitScreen: String;
    fCursorFile: String;
  public

    PlayerIsFirst: Boolean; // Global Verfügbar, True, wenn der Spieler "Spieler 1" ist !

    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); virtual;
    Procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    Constructor Create(Owner: TObject); virtual;
    Destructor Destroy(); override;

    Procedure Render; virtual;
    Procedure LoadFromDisk(ResPath: String); virtual;

    Procedure Reset; virtual; // Abstract
    Procedure StartPLaySong(); virtual;
  End;

  { TJoinQuestionForm }

  TJoinQuestionForm = Class(TForm)
  private
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Procedure EditKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
  public
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  End;

  { TMainMenu }

  TMainMenu = Class(TScreen)
  private
    fCursor: TOpenGL_Animation;
    fCursorPos: integer; // Position der "Bombe" in Menüpunkten
    fJoinQuestionForm: TJoinQuestionForm;
  public
    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); override;
    Procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    Constructor Create(Owner: TObject); override;
    Destructor Destroy(); override;

    Procedure LoadFromDisk(ResPath: String); override;
    Procedure Render; override;
    Procedure Reset; override;

    Procedure StopJoinQuestion;
  End;

  { TJoinMenu }

  TJoinMenu = Class(TScreen)
  private
    fPlayerInfoString: String; // Die Spieler die Gerade auch eingewählt sind.
    fServerIP: String; // Server IP address (for display when hosting)

  public
    Connected: Boolean;
    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); override;

    Constructor Create(Owner: TObject); override;
    Procedure LoadPlayerdata(Const PlayerData: Array Of TPlayer);
    Procedure SetServerIP(Const IP: String); // Set server IP address for display
    Procedure Render; override;
    Procedure Reset; override;
  End;

  { TPlayerSetupMenu }

  TPlayerSetupMenu = Class(TScreen)
  private
    fSchemeFile: String;
    fcursorTex: integer;
    fCursorPos: integer; // Position des "Kopfes" in Menüpunkten
    fPlayerDetails: Array[0..length(PlayerColors) - 1] Of Record
      Team: Integer;
      PlayerData: String;
    End;
  public
    TeamPlay: Boolean; // True = Anzeigen der Spielerfarben als Team wird via "miSwitchToPlayerSetup" gesetzt

    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); override;
    // TODO: Maus Support

    Constructor Create(Owner: TObject); override;

    Procedure LoadScheme(Const Scheme: TScheme); // Wird via miUpdateScheme gesetzt
    Procedure LoadPlayerdata(Const PlayerData: TPlayers; Uid: Integer); // wird via TAtomic.SwitchToScreen gesetzt

    Procedure LoadFromDisk(ResPath: String); override;

    Procedure Render; override;
    Procedure Reset; override;
  End;

  { TFieldSetupMenu }

  TFieldSetupMenu = Class(TScreen)
  private
    fCursorPos: integer;
    fcursorTex: Integer;
  public
    MasterPlayerName: String;
    ActualField: TAtomicField;
    LastWinsToWinMatch: Integer;
    SchemeFile: String;

    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); override;

    Constructor Create(Owner: TObject); override;

    Procedure LoadFromDisk(ResPath: String); override;
    Procedure Render; override;
    Procedure Reset; override;
  End;

  { TDrawGameMenu }

  TDrawGameMenu = Class(TScreen)
  private
  public
    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); override;

    Constructor Create(Owner: TObject); override;

    Procedure Reset; override;
  End;

  { TMatchStatistikMenu }

  TMatchStatistikMenu = Class(TScreen)
  private
    fPlayers: TPlayers;
  public
    Victor: TVictor;
    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); override;

    Procedure LoadPlayerdata(Const PlayerData: TPlayers);

    Constructor Create(Owner: TObject); override;

    Procedure Render; override;

  End;

  { TVictoryMenu }

  TVictoryMenu = Class(TScreen)
  private
    fbackGrounds: Array[TVictor] Of Integer;
  public
    Victor: TVictor;
    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); override;

    Procedure LoadFromDisk(ResPath: String); override;

    Procedure Render; override;
    Constructor Create(Owner: TObject); override;
  End;

  { TOptionsMenu }

  TOptionsMenu = Class(TScreen)
  private
    fcursorTex: integer;
    fCursorPos: integer; // Position des "Kopfes" in Menüpunkten
  public
    Procedure OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState); override;
    Procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    Constructor Create(Owner: TObject); override;

    Procedure LoadFromDisk(ResPath: String); override;
    Procedure Render; override;
    Procedure Reset; override;
  End;

Implementation

Uses LCLType, Math, Graphics, Dialogs, fileutil, StrUtils
  , dglOpenGL
  , Unit1 // WTF, why is this unit in here ?
  , uopengl_graphikengine
  , uatomicfont
  , uvectormath
  , ugraphics
  , ugame
  , ukeyboarddialog
  , uatomic_global
  ;

Type

  { TSchemQuestionForm }

  TSchemQuestionForm = Class(TForm)
    Procedure FormButton1Click(Sender: TObject);
    Procedure FormShow(Sender: Tobject);
    Procedure ListboxKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
  private
    Listbox1: TListbox;
    Button1: TButton;
  public
    Procedure LoadSchemes(SelectedScheme: String);
    // Weil die Form keine Ressource hat, muss sie mittels CreateNew erzeugt werden, was auch immer das für einen unterschied macht ...
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  End;

  { TJoinQuestionForm }

Constructor TJoinQuestionForm.CreateNew(AOwner: TComponent; Num: Integer);
Begin
  Inherited CreateNew(AOwner, Num);
  caption := 'Enter IP-Settings';
  width := 312;
  height := 163;
  Position := poScreenCenter;
  Constraints.MaxWidth := Width;
  Constraints.MinWidth := Width;
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;

  Edit1 := TEdit.Create(self);
  edit1.name := 'Edit1';
  edit1.Parent := self;
  edit1.Top := 32;
  edit1.Left := 16;
  edit1.Width := 288;
  edit1.Text := '127.0.0.1';

  Edit2 := TEdit.Create(self);
  edit2.name := 'Edit2';
  edit2.Parent := self;
  edit2.Top := 88;
  edit2.Left := 16;
  edit2.Width := 288;
  edit2.Text := '9876';

  label1 := TLabel.Create(self);
  label1.Name := 'Label1';
  label1.Parent := self;
  label1.caption := 'IP';
  label1.top := 8;
  label1.Left := 8;

  label2 := TLabel.Create(self);
  label2.Name := 'Label2';
  label2.Parent := self;
  label2.caption := 'Port';
  label2.top := 64;
  label2.Left := 8;

  Button1 := TButton.Create(self);
  Button1.Name := 'Button1';
  Button1.Parent := self;
  Button1.caption := 'OK';
  Button1.ModalResult := mrOK;
  Button1.Default := True; // Make Enter key trigger this button (when focus is on button)
  Button1.Top := 128;
  Button1.Left := 229;

  Button2 := TButton.Create(self);
  Button2.Name := 'Button2';
  Button2.Parent := self;
  Button2.caption := 'Cancel';
  Button2.ModalResult := mrCancel;
  Button2.Cancel := True; // Make Esc key trigger this button
  Button2.Top := 128;
  Button2.Left := 16;

  // Add keyboard support: Enter = OK everywhere
  // Set handlers on Edit fields to catch Enter when focus is in Edit
  Edit1.OnKeyDown := @EditKeyDown;
  Edit2.OnKeyDown := @EditKeyDown;
  // Set handler on form to catch Enter when focus is on form (not on Edit or Button)
  OnKeyDown := @FormKeyDown;
  KeyPreview := True; // Enable form-level key handling
End;

Procedure TJoinQuestionForm.EditKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If key = VK_RETURN Then Begin
    // Enter in Edit field = OK button
    // Strategy: Set focus to OK button, then trigger it programmatically
    Key := 0; // Prevent default behavior (newline in Edit)
    Button1.SetFocus; // Focus the OK button first
    // Now trigger the button click programmatically
    Button1.Click; // This will set ModalResult := mrOK and close the form
  End;
  // Esc is handled automatically by Button2.Cancel := True
End;

Procedure TJoinQuestionForm.FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If key = VK_RETURN Then Begin
    // Enter on form (when focus is NOT on Button1) = OK button
    // Strategy: Set focus to OK button, then trigger it programmatically
    If ActiveControl <> Button1 Then Begin
      Key := 0; // Prevent default behavior
      Button1.SetFocus; // Focus the OK button first
      // Now trigger the button click programmatically
      Button1.Click; // This will set ModalResult := mrOK and close the form
    End;
    // If focus is on Button1, let default behavior (Default := True) handle it
  End;
  // Esc is handled automatically by Button2.Cancel := True
End;

{ TVictoryMenu }

Procedure TVictoryMenu.OnKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If (key = VK_RETURN) Or (key = VK_ESCAPE) Then Begin
    Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
    TGame(fOwner).SwitchToScreen(sMainScreen);
  End;
End;

Procedure TVictoryMenu.LoadFromDisk(ResPath: String);
Begin
  Inherited LoadFromDisk(ResPath);
  fbackGrounds[vWhiteTeam] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'team0.png', smStretch);
  fbackGrounds[vRedTeam] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'team1.png', smStretch);
  fbackGrounds[vCol0] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory0.png', smStretch);
  fbackGrounds[vCol1] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory1.png', smStretch);
  fbackGrounds[vCol2] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory2.png', smStretch);
  fbackGrounds[vCol3] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory3.png', smStretch);
  fbackGrounds[vCol4] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory4.png', smStretch);
  fbackGrounds[vCol5] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory5.png', smStretch);
  fbackGrounds[vCol6] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory6.png', smStretch);
  fbackGrounds[vCol7] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory7.png', smStretch);
  fbackGrounds[vCol8] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory8.png', smStretch);
  fbackGrounds[vCol9] := OpenGL_GraphikEngine.LoadGraphik(ResPath + 'victory9.png', smStretch);
End;

Procedure TVictoryMenu.Render;
Begin
  fBackTex := fbackGrounds[Victor];
  Inherited Render;
End;

Constructor TVictoryMenu.Create(Owner: TObject);
Begin
  Inherited Create(Owner);
  fSoundFile := 'draw.wav';
End;

{ TMatchStatistikMenu }

Procedure TMatchStatistikMenu.OnKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_ESCAPE Then Begin
    Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
    TGame(fOwner).SwitchToScreen(sMainScreen);
  End;
  If PlayerIsFirst Then Begin
    If key = VK_RETURN Then Begin
      TGame(fOwner).StartGame();
    End;
  End;
End;

Procedure TMatchStatistikMenu.LoadPlayerdata(Const PlayerData: TPlayers);
Begin
  fPlayers := PlayerData;
End;

Constructor TMatchStatistikMenu.Create(Owner: TObject);
Begin
  Inherited Create(Owner);
  fBackFile := 'results.png';
  fSoundFile := 'draw.wav';
End;

Procedure TMatchStatistikMenu.Render;

  Function VictorToString(Value: TVictor): String;
  Var
    index: integer;
  Begin
    Case Value Of
      vRedTeam: result := 'Red Team';
      vWhiteTeam: result := 'White Team';
    Else Begin
        index := integer(value) - integer(vCol0);
        result := fPlayers[index].UserName;
        If result = '' Then
          result := 'Ai';
      End;
    End;
  End;

Var
  s, un: String;
  i: Integer;
Begin
  Inherited Render;
  glpushmatrix();
  glTranslatef(0, 0, atomic_Map_Layer + 0.5);
  glBindTexture(GL_TEXTURE_2D, 0);
  glColor4f(1, 1, 1, 1);
  AtomicFont.BackColor := clBlack;
  AtomicFont.Color := $00A8ADAB;
  AtomicFont.Textout(150, 100, '(Match winner must score ' + inttostr(TGame(fOwner).Settings.LastWinsToWinMatch) + ' victories)');
  s := 'Game Winner was ' + VictorToString(Victor) + ' !';
  AtomicFont.BackColor := clBlack;
  AtomicFont.Color := clwhite;
  AtomicFont.Textout(150, 150, s);
  glPushMatrix;
  glTranslatef(130, 200, 0);
  For i := 0 To high(fPlayers) Do Begin
    If fPlayers[i].UID <> NoPlayer Then Begin
      If i = 1 Then Begin
        AtomicFont.BackColor := clWhite;
      End
      Else Begin
        AtomicFont.BackColor := clBlack;
      End;
      AtomicFont.Color := AtomicPlayerColorToColor(PlayerColors[i]);
      un := fPlayers[i].UserName;
      If un = '' Then un := 'Ai';
      un := PadRight(un, 20);
      s := format('%s: score: %d (kills: %d)', [un, fPlayers[i].Score, fPlayers[i].Kills]);
      AtomicFont.Textout(0, 0, s);
      glTranslatef(0, 20, 0);
    End;
  End;
  glPopMatrix;
  AtomicFont.BackColor := clBlack; // Reset nach außen
  glPopMatrix;
End;

{ TDrawGameMenu }

Procedure TDrawGameMenu.OnKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_ESCAPE Then Begin
    Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
    Tgame(fOwner).SwitchToScreen(sMainScreen);
  End;
  If PlayerIsFirst Then Begin
    If key = VK_RETURN Then Begin
      TGame(fOwner).StartGame;
    End;
  End;
End;

Constructor TDrawGameMenu.Create(Owner: TObject);
Begin
  Inherited Create(Owner);
  fBackFile := 'draw.png'; // WTF, warum geht das net als PNG ?
  fSoundFile := 'draw.wav';
  fCursorFile := '';
End;

Procedure TDrawGameMenu.Reset;
Begin
  Inherited Reset;
End;

{ TSchemQuestionForm }

Constructor TSchemQuestionForm.CreateNew(AOwner: TComponent; Num: Integer);
Begin
  Inherited CreateNew(AOwner, Num);
  caption := 'Please select a scheme';
  Width := 350;
  height := 350;
  FormStyle := fsSystemStayOnTop;
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  BorderIcons := [biSystemMenu];
  position := poScreenCenter;
  Button1 := TButton.Create(self);
  button1.name := 'Button1';
  Button1.Parent := self;
  Button1.Align := alBottom;
  button1.caption := 'OK';
  button1.OnClick := @FormButton1Click;
  Listbox1 := TListBox.Create(self);
  Listbox1.Name := 'Listbox1';
  Listbox1.Parent := self;
  Listbox1.Align := alClient;
  Listbox1.OnKeyDown := @ListboxKeyDown;

  OnShow := @FormShow;
End;

Procedure TSchemQuestionForm.FormButton1Click(Sender: TObject);
Begin
  If Listbox1.ItemIndex <> -1 Then Begin
    ModalResult := mrOK;
  End
  Else Begin
    ModalResult := mrCancel; // Anscheinend haben wir keine Scheme files, dann versuchen wir auch nicht sie zu speichern
  End;
End;

Procedure TSchemQuestionForm.FormShow(Sender: Tobject);
Begin
  Listbox1.SetFocus;
  (*
   * Die TopRow Eigenschaft darf erst gesetzt werden, wenn die Listbox dabei ist sich zu zeichnen
   * im LoadSchemes geht das nicht ..
   *)
  If Listbox1.ItemIndex <> -1 Then Begin
    Listbox1.TopIndex := Listbox1.ItemIndex;
  End;
End;

Procedure TSchemQuestionForm.ListboxKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_RETURN Then Begin
    key := 0; // Den Key Löschen, da der Button1.Click das Formular Platt macht und der Key sonst in TScreen nochmal ausgewertet wird.
    button1.Click;
  End;
  If key = VK_ESCAPE Then ModalResult := mrCancel;
End;

Procedure TSchemQuestionForm.LoadSchemes(SelectedScheme: String);
Var
  p: String;
  sl: TStringList;
  i: Integer;
Begin
  p := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'data' + PathDelim + 'schemes';
  sl := FindAllfiles(p, '*.sch', false);
  Listbox1.Clear;
  Listbox1.Sorted := true;
  For i := 0 To sl.Count - 1 Do Begin
    Listbox1.Items.Add(ExtractFileName(sl[i]));
  End;
  For i := 0 To Listbox1.Items.Count - 1 Do Begin
    If lowercase(SelectedScheme) = lowercase(Listbox1.Items[i]) Then Begin
      Listbox1.ItemIndex := i;
      break;
    End;
  End;
  sl.free;
End;

{ TFieldSetupMenu }

Procedure TFieldSetupMenu.OnKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_ESCAPE Then Begin
    Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
    TGame(fOwner).SwitchToScreen(sMainScreen);
  End;
  If PlayerIsFirst Then Begin
    If key = VK_RETURN Then Begin
      TGame(fOwner).StartGame;
    End;
    If key = VK_DOWN Then Begin
      fCursorPos := min(fCursorPos + 1, 1);
    End;
    If key = VK_UP Then Begin
      fCursorPos := max(fCursorPos - 1, 0);
    End;
    If key = VK_LEFT Then Begin
      Case fCursorPos Of
        0: TGame(fOwner).UpdateSelectedField(-1);
        1: TGame(fOwner).UpdateWinsToWin(-1);
      End;
    End;
    If key = VK_Right Then Begin
      Case fCursorPos Of
        0: TGame(fOwner).UpdateSelectedField(1);
        1: TGame(fOwner).UpdateWinsToWin(1);
      End;
    End;
  End;
End;

Constructor TFieldSetupMenu.Create(Owner: TObject);
Begin
  Inherited Create(Owner);
  ActualField := Nil;
  fBackFile := 'fieldsetup.png'; // WTF, warum geht das net als PNG ?
  fSoundFile := 'player_setup_sound.wav';
  fCursorFile := 'options_cursor.png';
End;

Procedure TFieldSetupMenu.LoadFromDisk(ResPath: String);
Begin
  Inherited LoadFromDisk(ResPath);
  (*
   * Neu Laden der Hintergrundgraphik mit Transparenz
   *)
  OpenGL_GraphikEngine.RemoveGraphik(fBackTex);
  fBackTex := OpenGL_GraphikEngine.LoadAlphaColorGraphik(ResPath + fBackFile, ColorToRGB(clfuchsia), smStretchHard);
  fcursorTex := OpenGL_GraphikEngine.LoadAlphaColorGraphik(ResPath + fCursorFile, ColorToRGB(clfuchsia), smStretch);
End;

Procedure TFieldSetupMenu.Render;
Begin
  // Das Vorschaubild
  glColor4f(1, 1, 1, 1);
  // Der Eigentliche Hintergrund
  glpushmatrix();
  glAlphaFunc(GL_LESS, 0.5);
  // Das ist ja eine Textur mit "Fenster" -> Also Alphatest mit an
  glEnable(GL_ALPHA_TEST);
  glTranslatef(0, 0, atomic_Map_Layer + 0.5);
  RenderAlphaQuad(v2(320, 240), GameWidth, -GameHeight, 0, fBackTex);
  gldisable(GL_ALPHA_TEST);
  glBindTexture(GL_TEXTURE_2D, 0);
  glpopmatrix();
  If assigned(ActualField) Then Begin
    AtomicFont.Color := clwhite;
    AtomicFont.BackColor := clBlack;
    If ActualField.Name = '' Then Begin
      AtomicFont.Textout(55, 176, 'Random Each Game');
    End
    Else Begin
      AtomicFont.Textout(55, 176, ActualField.Name);
    End;
  End
  Else Begin
    AtomicFont.Color := clRed;
    AtomicFont.BackColor := clBlack;
    AtomicFont.Textout(55, 176, 'No Field informations...');
  End;
  AtomicFont.Color := clwhite;
  AtomicFont.BackColor := clBlack;
  AtomicFont.Textout(55, 176 + 28, inttostr(LastWinsToWinMatch) + ' Wins to win match');
  If PlayerIsFirst Then Begin
    glPushMatrix();
    glTranslatef(20, 176 - 14 + 28 * fCursorPos, atomic_Map_Layer + 0.5 + atomic_EPSILON);
    RenderAlphaQuad(point(16, 16), 32, -32, 0, fcursorTex);
    glPopMatrix();
  End
  Else Begin
    AtomicFont.Color := clYellow;
    AtomicFont.BackColor := clBlack;
    AtomicFont.Textout(45, 176 - 28 - 28, 'Wait until ' + MasterPlayerName + LineEnding + 'finished setup.');
  End;
  glBindTexture(GL_TEXTURE_2D, 0);
  AtomicFont.Color := clwhite;
  AtomicFont.BackColor := clBlack;
  AtomicFont.Textout(60, 400, 'Scheme: ' + SchemeFile);
  (*
   * Das Eigentliche Kartenvorschau Fenster
   *)
  glpushmatrix();
  glTranslatef(379, 33, 0); // Das Offset zum Vorschaufenster ;)
  If assigned(ActualField) Then Begin
    ActualField.RenderPreview;
  End;
  glpopmatrix();
End;

Procedure TFieldSetupMenu.Reset;
Begin
  Inherited Reset;
  MasterPlayerName := '';
  fCursorPos := 0;
End;

{ TPlayerSetupMenu }

Procedure TPlayerSetupMenu.OnKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_RETURN Then Begin
    If PlayerIsFirst Then Begin
      Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
      Tgame(fOwner).SwitchToScreen(sEditFieldSetupRequest);
    End;
  End;
  If key = VK_ESCAPE Then Begin
    Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
    Tgame(fOwner).SwitchToScreen(sMainScreen);
  End;
  If key = VK_DOWN Then Begin
    fCursorPos := min(fCursorPos + 1, length(PlayerColors) - 1);
  End;
  If key = VK_UP Then Begin
    fCursorPos := max(fCursorPos - 1, 0);
  End;
  If key = VK_LEFT Then Begin
    Tgame(fOwner).ChangePLayerKey(fCursorPos, -1);
  End;
  If key = VK_RIGHT Then Begin
    Tgame(fOwner).ChangePLayerKey(fCursorPos, 1);
  End;
End;

Constructor TPlayerSetupMenu.Create(Owner: TObject);
Begin
  Inherited Create(Owner);
  fBackFile := 'join.png';
  fSoundFile := 'player_setup_sound.wav';
  fCursorFile := 'options_cursor.png';
End;

Procedure TPlayerSetupMenu.LoadScheme(Const Scheme: TScheme);
Var
  i: Integer;
Begin
  fSchemeFile := Scheme.Filename;
  (*
   * Für das Playersetup ist nur die "TEAM" Verteilung relevant / interessant
   *)
  For i := 0 To high(fPlayerDetails) Do Begin
    fPlayerDetails[i].Team := Scheme.PlayerStartPositions[i].Team;
  End;
End;

Procedure TPlayerSetupMenu.LoadPlayerdata(Const PlayerData: TPlayers;
  Uid: Integer);
Var
  i: Integer;
Begin
  (*
   * Siehe TPlayer.UID
   *)
  For i := 0 To high(fPlayerDetails) Do Begin
    If PlayerData[i].UID = Uid Then Begin
      If PlayerData[i].Keyboard = ks0 Then Begin
        fPlayerDetails[i].PlayerData := 'key 0';
      End
      Else Begin
        fPlayerDetails[i].PlayerData := 'key 1';
      End;
      //fCursorPos := i; // Das geht leider nicht, da sonst der Cursor merkwürdig Springt wenn der Spieler 2 mal eingestellt ist..
    End
    Else Begin
      If PlayerData[i].UID > 0 Then Begin
        fPlayerDetails[i].PlayerData := PlayerData[i].UserName; // 'NET'; -- The Orig Game shows at this place only a "NET" entry
      End
      Else Begin
        If PlayerData[i].UID = NoPlayer Then Begin
          fPlayerDetails[i].PlayerData := 'OFF';
        End
        Else Begin
          If PlayerData[i].UID = AIPlayer Then Begin
            fPlayerDetails[i].PlayerData := 'AI';
          End;
        End;
      End;
    End;
  End;
End;

Procedure TPlayerSetupMenu.LoadFromDisk(ResPath: String);
Begin
  Inherited LoadFromDisk(ResPath);
  fcursorTex := OpenGL_GraphikEngine.LoadAlphaColorGraphik(ResPath + fCursorFile, ColorToRGB(clfuchsia), smStretch);
End;

Procedure TPlayerSetupMenu.Render;
Var
  i: Integer;
  s: String;
Begin
  Inherited Render;
  glBindTexture(GL_TEXTURE_2D, 0);
  AtomicFont.Color := clwhite;
  AtomicFont.BackColor := clBlack;
  AtomicFont.Textout(60, 37 + 40, 'Available players:');
  For i := 0 To high(fPlayerDetails) Do Begin
    If i = 1 Then Begin
      AtomicFont.BackColor := clWhite;
    End
    Else Begin
      AtomicFont.BackColor := clBlack;
    End;
    AtomicFont.Color := AtomicPlayerColorToColor(PlayerColors[i]);
    s := format('Player %0.2d: ', [i + 1]);
    AtomicFont.Textout(60 + 20, 37 + (i + 1) * 28 + 50, s);
    If TeamPlay Then Begin
      (*
       * Im Teamplay Färben wir den Detail Text in der teamfarbe ein
       *)
      If fPlayerDetails[i].Team = TeamIndexWhite Then Begin
        AtomicFont.Color := clwhite;
        AtomicFont.BackColor := clBlack;
      End
      Else Begin
        AtomicFont.Color := clRed;
        AtomicFont.BackColor := clBlack;
      End;
    End;
    s := format('              %s', [fPlayerDetails[i].PlayerData]);
    AtomicFont.Textout(60 + 20, 37 + (i + 1) * 28 + 50, s);
  End;
  // Reset am ende
  AtomicFont.Color := clwhite;
  AtomicFont.BackColor := clBlack;
  AtomicFont.Textout(60, 400, 'Scheme: ' + fSchemeFile);
  glPushMatrix();
  glTranslatef(60 - 32 + 10, 37 + 14 + 28 * fCursorPos + 50, atomic_Map_Layer + atomic_EPSILON);
  RenderAlphaQuad(point(16, 16), 32, -32, 0, fcursorTex);
  glPopMatrix();
End;

Procedure TPlayerSetupMenu.Reset;
Begin
  Inherited Reset;
  fCursorPos := 0;
End;

{ TJoinMenu }

Procedure TJoinMenu.OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If (key = VK_ESCAPE) Or (key = VK_BACK) Then Begin
    Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
    Tgame(fOwner).SwitchToScreen(sMainScreen);
  End;
  If key = VK_RETURN Then Begin // Alle Clients sind da -> Ab in den Spieler Config Dialog..
    If PlayerIsFirst Then Begin
      Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
      Tgame(fOwner).SwitchToScreen(sPlayerSetupRequest);
    End;
  End;
End;

Constructor TJoinMenu.Create(Owner: TObject);
Begin
  Inherited Create(Owner);
  fBackFile := 'join.png';
  fSoundFile := 'join_sound.wav';
  fCursorFile := '';
  fServerIP := '';
End;

Procedure TJoinMenu.LoadPlayerdata(Const PlayerData: Array Of TPlayer);
Var
  i: Integer;
Begin
  fPlayerInfoString := 'Connected network players:' + LineEnding + LineEnding;
  For i := 0 To high(PlayerData) Do Begin
    If PlayerData[i].UID <> NoPlayer Then Begin
      fPlayerInfoString := fPlayerInfoString + ' ' + PlayerData[i].UserName + LineEnding;
    End;
  End;
End;

Procedure TJoinMenu.SetServerIP(Const IP: String);
Begin
  fServerIP := IP;
End;

Procedure TJoinMenu.Render;
Var
  serverInfo: String;
Begin
  Inherited Render;
  glPushMatrix;
  glColor3f(1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  AtomicFont.color := $00EAE556;
  AtomicFont.BackColor := clBlack;
  AtomicFont.Textout(100, 50, 'Our Nodename is: ''' + Tgame(fOwner).Settings.NodeName + '''');

  // Display server IP address if available (when hosting)
  If fServerIP <> '' Then Begin
    serverInfo := 'Server IP: ' + fServerIP + ':' + Tgame(fOwner).Settings.Router_Port;
    AtomicFont.color := clLime; // Green color for server IP
    AtomicFont.Textout(100, 75, serverInfo);
  End;

  If Connected Then Begin
    // Der Server hat unseren Login Versuch Grundsätzlich aktzeptiert
    // Wir zeigen nun die Infos der Spieler an, bis der 1. Spieler in den Nächsten Screen umschaltet
    AtomicFont.color := clWhite;
    AtomicFont.Textout(120, 100, fPlayerInfoString);
  End
  Else Begin
    AtomicFont.color := clYellow;
    AtomicFont.Textout(120, 100, 'Waiting for server to host a game..');
  End;
  glPopMatrix;
End;

Procedure TJoinMenu.Reset;
Begin
  Inherited Reset;
  Connected := false;
  fPlayerInfoString := 'waiting for playerlist from server..';
End;

{ TOptionsMenu }

Procedure TOptionsMenu.OnKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  kd: TKeyboardDialog;
  qf: TSchemQuestionForm;
  nn: String;
Begin
  // Zurück ins Hauptmenü
  If (key = VK_ESCAPE) Or (key = VK_BACK) Then Begin
    Tgame(fOwner).PlaySoundEffect(fSoundExitScreen);
    Tgame(fOwner).SwitchToScreen(sMainScreen);
    Form1.IniPropStorage1SavingProperties(Nil);
    Form1.Load_Atomic_Settings;
  End;
  If key = VK_DOWN Then Begin
    fCursorPos := min(fCursorPos + 1, 12);
  End;
  If key = VK_UP Then Begin
    fCursorPos := max(fCursorPos - 1, 0);
  End;
  If (key = VK_RETURN) Or (Key = VK_ADD) Or (key = VK_SUBTRACT)
    Or (Key = VK_LEFT) Or (Key = VK_RIGHT) Then Begin
    Case fCursorPos Of
      0: Tgame(fOwner).Settings.TeamPlay := Not Tgame(fOwner).Settings.TeamPlay;
      1: Tgame(fOwner).Settings.RandomStart := Not Tgame(fOwner).Settings.RandomStart;
      2: Begin
          key := 0;
          nn := InputBox('Edit', 'Enter Nodename', Tgame(fOwner).Settings.NodeName);
          If trim(nn) <> '' Then Begin
            Tgame(fOwner).Settings.NodeName := nn;
          End
          Else Begin
            LogShow('Empty username not allowed', llWarning);
          End;
        End;
      3: Begin
          If key In [VK_RETURN, VK_ADD, VK_RIGHT] Then Begin
            Case Tgame(fOwner).Settings.ConveyorSpeed Of
              csSlow: Tgame(fOwner).Settings.ConveyorSpeed := csMiddle;
              csMiddle: Tgame(fOwner).Settings.ConveyorSpeed := csFast;
              csFast: Tgame(fOwner).Settings.ConveyorSpeed := csSlow;
            End;
          End
          Else Begin
            Case Tgame(fOwner).Settings.ConveyorSpeed Of
              csSlow: Tgame(fOwner).Settings.ConveyorSpeed := csFast;
              csMiddle: Tgame(fOwner).Settings.ConveyorSpeed := csSlow;
              csFast: Tgame(fOwner).Settings.ConveyorSpeed := csMiddle;
            End;
          End;
        End;
      4: Begin // Scheme File
          key := 0;
          qf := TSchemQuestionForm.CreateNew(Nil);
          qf.LoadSchemes(Tgame(fOwner).Settings.SchemeFile);
          qf.ShowModal;
          If qf.ModalResult = mrOK Then Begin
            Tgame(fOwner).Settings.SchemeFile := qf.Listbox1.Items[qf.Listbox1.ItemIndex];
          End;
          qf.free;
        End;
      5: Begin // Play Time
          If key In [VK_LEFT, VK_SUBTRACT] Then Begin
            Tgame(fOwner).Settings.PlayTime := max(0, Tgame(fOwner).Settings.PlayTime - 15);
            If Tgame(fOwner).Settings.PlayTime < 80 Then Tgame(fOwner).Settings.PlayTime := 0;
          End
          Else Begin
            If Tgame(fOwner).Settings.PlayTime = 0 Then Begin
              Tgame(fOwner).Settings.PlayTime := 90;
            End
            Else Begin
              Tgame(fOwner).Settings.PlayTime := Tgame(fOwner).Settings.PlayTime + 15;
            End;
          End;
        End;
      6: Tgame(fOwner).Settings.LostPlayersRevertToAI := Not Tgame(fOwner).Settings.LostPlayersRevertToAI;
      7: Tgame(fOwner).Settings.PlaySounds := Not Tgame(fOwner).Settings.PlaySounds;
      8: Begin
          key := 0;
          kd := TKeyboardDialog.CreateNew(Nil);
          kd.LoadKeys(Tgame(fOwner).Settings.Keys[ks0], Tgame(fOwner).Settings.Keys[ks1]);
          If kd.Execute() Then Begin
            Tgame(fOwner).Settings.Keys[ks0] := kd.GetKeys(ks0);
            Tgame(fOwner).Settings.Keys[ks1] := kd.GetKeys(ks1);
          End;
          kd.Free;
        End;
      9: Tgame(fOwner).Settings.Port := strtointdef(InputBox('Edit', 'Enter port', inttostr(Tgame(fOwner).Settings.Port)), 1234);
      10: Tgame(fOwner).Settings.ShowFPS := Not Tgame(fOwner).Settings.ShowFPS;
      11: Begin
          Tgame(fOwner).Settings.Fullscreen := Not Tgame(fOwner).Settings.Fullscreen;
          Form1.SetFullScreen(Tgame(fOwner).Settings.Fullscreen);
{$IFDEF Windows}
          If Tgame(fOwner).Settings.Fullscreen Then Begin
            LogShow('To switch to Fullscreen mode, you need to restart the game.');
          End;
{$ENDIF}
        End;
      12: Begin
          Tgame(fOwner).Settings.Proportional := Not Tgame(fOwner).Settings.Proportional;
          Tgame(fOwner).Resize();
        End;
    End;
  End;
End;

Procedure TOptionsMenu.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  key: Word;
Begin
  // Der Klick auf die Buttons ;)
  If (x > 60) And (x < 320) And
    (y > 36) And
    (y < 36 + 12 * 28)
    Then Begin
    fCursorPos := (y - 36) Div 28;
    key := VK_RETURN;
    OnKeyDown(Nil, key, []);
  End;
End;

Constructor TOptionsMenu.Create(Owner: TObject);
Begin
  Inherited Create(Owner);
  fBackFile := 'options.png';
  fSoundFile := ''; // Die Optionen haben keinen Extra "Sound"
  fCursorFile := 'options_cursor.png';
End;

Procedure TOptionsMenu.LoadFromDisk(ResPath: String);
Begin
  Inherited LoadFromDisk(ResPath);
  fcursorTex := OpenGL_GraphikEngine.LoadAlphaColorGraphik(ResPath + fCursorFile, ColorToRGB(clfuchsia), smStretch);
End;

Procedure TOptionsMenu.Render;
  Function PrettyTime(Value_s: integer): String;
  Begin
    If Value_s = 0 Then Begin
      result := 'infinity';
    End
    Else Begin
      result := format('%0.2d:%0.2d', [Value_s Div 60, Value_s Mod 60]);
    End;
  End;

  Function ConveyerSpeedToString(aSpeed: TConveyorSpeed): String;
  Begin
    result := '';
    Case aSpeed Of
      csSlow: result := 'Slow';
      csMiddle: result := 'Middle';
      csFast: result := 'Fast';
    End;
  End;

Begin
  Inherited Render;
  glBindTexture(GL_TEXTURE_2D, 0);
  AtomicFont.BackColor := clblack;
  AtomicFont.Color := clwhite;
  AtomicFont.Textout(60, 37,
    'Team Play: ' + BoolToStr(TGame(fOwner).Settings.TeamPlay, 'Yes', 'No') + LineEnding + LineEnding +
    'Random Start: ' + BoolToStr(TGame(fOwner).Settings.RandomStart, 'Yes', 'No') + LineEnding + LineEnding +
    'Node Name: ''' + TGame(fOwner).Settings.NodeName + '''' + LineEnding + LineEnding +
    'Conveyor Speed: ' + ConveyerSpeedToString(TGame(fOwner).Settings.ConveyorSpeed) + LineEnding + LineEnding +
    'Scheme File: ' + TGame(fOwner).Settings.SchemeFile + LineEnding + LineEnding +
    'Play Time: ' + PrettyTime(TGame(fOwner).Settings.PlayTime) + LineEnding + LineEnding +
    'Lost net players revert to AIs: ' + BoolToStr(TGame(fOwner).Settings.LostPlayersRevertToAI, 'Yes', 'No') + LineEnding + LineEnding +
    'Disable music during gameplay: ' + BoolToStr(TGame(fOwner).Settings.PlaySounds, 'No', 'Yes') + LineEnding + LineEnding +
    'Define keyboard layouts' + LineEnding + LineEnding +
    'Network port: ' + inttostr(TGame(fOwner).Settings.Port) + LineEnding + LineEnding +
    'Show FPS: ' + BoolToStr(TGame(fOwner).Settings.ShowFPS, 'Yes', 'No') + LineEnding + LineEnding +
    'Fullscreen: ' + BoolToStr(TGame(fOwner).Settings.Fullscreen, 'Yes', 'No') + LineEnding + LineEnding +
    'Stretch: ' + BoolToStr(TGame(fOwner).Settings.Proportional, 'No', 'Yes')
    );

  glPushMatrix();
  glTranslatef(20, 25 + 28 * fCursorPos, atomic_Map_Layer + atomic_EPSILON);
  RenderAlphaQuad(point(16, 16), 32, -32, 0, fcursorTex);
  glPopMatrix();
End;

Procedure TOptionsMenu.Reset;
Begin
  fCursorPos := 0;
End;

{ TMainMenu }

Procedure TMainMenu.OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If key = VK_J Then Begin
    If assigned(fJoinQuestionForm) Then exit; // Der Dialog ist schon offen ;)
    // Frage IP und Port zum Game.JoinViaParams(ip, port);  ab !
    fJoinQuestionForm := TJoinQuestionForm.CreateNew(Nil, 0);
    fJoinQuestionForm.Edit1.Text := TGame(fOwner).Settings.Router_IP;
    fJoinQuestionForm.Edit2.Text := TGame(fOwner).Settings.Router_Port;
    // TODO:  Wie macht man dialog tatsächlich modal gegenüber Form1 ?, zumindest unter Linux geht das so nicht ..
    Case fJoinQuestionForm.ShowModal Of
      mrOK: Begin
          TGame(fOwner).JoinViaParams(fJoinQuestionForm.Edit1.Text, strtointdef(fJoinQuestionForm.Edit2.Text, 9876));
          // Remember last settings, for next game ;)
          TGame(fOwner).Settings.Router_IP := fJoinQuestionForm.Edit1.Text;
          TGame(fOwner).Settings.Router_Port := inttostr(strtointdef(fJoinQuestionForm.Edit2.Text, 9876));
        End;
      mrAbort: Begin
          // CLient found a server, does not need the Join Question Dialog anymore ..
        End
    Else Begin
        // User Canceled or did something else -> go back to main screen
        TGame(fOwner).SwitchToScreen(sMainScreen);
      End;
    End;
    fJoinQuestionForm.free;
    fJoinQuestionForm := Nil;
  End;
  If key = VK_DOWN Then Begin
    fCursorPos := (fCursorPos + 1) Mod 7; // Feature Request by Community, rotating navigation
  End;
  If key = VK_UP Then Begin
    fCursorPos := (fCursorPos + 6) Mod 7; // Feature Request by Community, rotating navigation
  End;
  If (key >= VK_1) And (key <= VK_7) Then Begin
    fCursorPos := key - VK_1;
    key := VK_RETURN;
  End;
  If key = VK_RETURN Then Begin
    Case fCursorPos Of
      0: logshow('Not yet implemented.', llinfo); // TAtomic(fOwner).SwitchToScreen(); -- Single Player
      1: TGame(fOwner).SwitchToScreen(sHost);
      2: TGame(fOwner).SwitchToScreen(sJoinNetwork);
      3: TGame(fOwner).SwitchToScreen(sOptions);
      4: logshow('Not yet implemented.', llinfo); //TGame(fOwner).SwitchToScreen(); -- About Bomberman
      5: logshow('Not yet implemented.', llinfo); //TGame(fOwner).SwitchToScreen(); -- Online Manual
      6: key := VK_ESCAPE; // -- Exit Bomberman
    End;
  End;
  If key = VK_ESCAPE Then Begin
{$IFNDEF Only3Player}
    If ID_YES = Application.MessageBox('Do you really want to quit?', 'Question', MB_ICONQUESTION Or MB_YESNO) Then
{$ENDIF}
      TGame(fOwner).SwitchToScreen(sExitBomberman);
  End;
End;

Procedure TMainMenu.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  key: Word;
Begin
  // Der Klick auf die Buttons ;)
  If (x > 350) And (x < 610) And
    (y > 100) And (y < 100 + 7 * 37) Then Begin
    fCursorPos := (y - 100) Div 37;
    key := VK_RETURN;
    OnKeyDown(Nil, key, []);
  End;
End;

Constructor TMainMenu.Create(Owner: TObject);
Begin
  Inherited Create(Owner);
  fBackFile := 'mainmenu.png';
  fSoundFile := 'mainmenu_sound.wav';
  fCursorFile := 'mainmenu_cursor.ani';
  fCursor := TOpenGL_Animation.Create;
  fJoinQuestionForm := Nil;
End;

Destructor TMainMenu.Destroy;
Begin
  fCursor.Free;
  Inherited Destroy;
End;

Procedure TMainMenu.LoadFromDisk(ResPath: String);
Begin
  Inherited LoadFromDisk(ResPath);
  fCursor.LoadFromFile(ResPath + fCursorFile);
End;

Procedure TMainMenu.Render;
Begin
  Inherited Render;
  glPushMatrix();
  glTranslatef(310, 100 + fCursorPos * 37, atomic_Map_Layer + atomic_EPSILON);
  fCursor.Render(0);
  glPopMatrix();
End;

Procedure TMainMenu.Reset;
Begin
  Inherited Reset;
  fCursorPos := 0;
End;

Procedure TMainMenu.StopJoinQuestion;
Begin
  If assigned(fJoinQuestionForm) Then Begin
    fJoinQuestionForm.ModalResult := mrAbort;
  End;
End;

{ TScreen }

Procedure TScreen.OnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  // Nichts nur zum Überscheiben da..
End;

Procedure TScreen.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  // Nichts nur zum Überscheiben da..
End;

Constructor TScreen.Create(Owner: TObject);
Begin
  fOwner := Owner;
  If Not (fOwner Is TGame) Then Begin
    Raise exception.Create('Error, Owner has to be TAtomic!');
  End;
  fBackFile := '';
  fSoundFile := '';
  fBackTex := 0;
End;

Destructor TScreen.Destroy;
Begin
  // Nix
End;

Procedure TScreen.Render;
Begin
  (*
   * Die Hintergrund Graphik kann auf jeden Fall gerendert werden..
   *)
  glpushmatrix();
  glTranslatef(0, 0, atomic_Map_Layer);
  glColor3f(1, 1, 1);
  RenderQuad(v2(0, 0), v2(GameWidth, GameHeight), 0, false, fBackTex);
  glpopmatrix();
End;

Procedure TScreen.LoadFromDisk(ResPath: String);
Begin
  fSoundExitScreen := ResPath + 'menuexit.wav';
  If fSoundFile <> '' Then Begin
    fSoundFile := ResPath + fSoundFile;
  End;
  If fBackFile <> '' Then Begin
    fBackTex := OpenGL_GraphikEngine.LoadGraphik(ResPath + fBackFile, smStretch);
  End
  Else Begin
    fBackTex := 0;
  End;
End;

Procedure TScreen.Reset;
Begin
  PlayerIsFirst := false;
  StartPLaySong();
End;

Procedure TScreen.StartPLaySong();
Begin
  // Auf Jeden Fall soll mal die Passende Screen Musik abgespielt werden.
  If trim(fSoundFile) <> '' Then Begin
    TGame(fOwner).StartPlayingSong(fSoundFile);
  End;
End;

End.

