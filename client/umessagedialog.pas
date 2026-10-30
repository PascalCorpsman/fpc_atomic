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
Unit umessagedialog;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uopengl_graphikengine, uatomic_global;

Type

  TMessageDialogKind = (mdkInfo, mdkYesNo);

  TMessageDialogEntry = Record
    Kind: TMessageDialogKind;
    Level: TLogLevel;
    Text: String; // Der Original Text, um doppelte Meldungen zu erkennen
    Lines: Array Of String; // Der auf die Dialogbreite umgebrochene Text
    OnYes: TNotifyEvent;
  End;

  { TMessageDialog }

  (*
   * Meldungen und Ja / Nein Abfragen, die direkt im Spiel (OpenGL) im Stil des
   * Lade Dialogs gezeichnet werden, statt als LCL-Fenster.
   *
   * Der Dialog blockiert nicht, die Antwort "Ja" kommt über OnYes.
   * Mehrere Meldungen werden nacheinander angezeigt.
   *)
  TMessageDialog = Class
  private
    fTexture: TGraphikItem; // Die 9 Einzeltexturen des Rahmens (gleiche Datei wie TLoaderDialog)
    fQueue: Array Of TMessageDialogEntry;
    fYesSelected: Boolean;
    Function getActive: Boolean;
    Function getReady: Boolean;
    Procedure Add(Const Entry: TMessageDialogEntry);
    Procedure CloseActual(Yes: Boolean);
    Procedure GetLayout(Out Left, Top, Height, ContentHeight: Integer);
    Function ButtonAt(x, y: Integer): Integer; // 0 = OK bzw. Yes, 1 = No, -1 = kein Button
  public
    OnOpen: TNotifyEvent; // Wird ausgelöst, sobald ein Dialog sichtbar wird

    Property Active: Boolean read getActive; // True, wenn gerade ein Dialog angezeigt wird
    Property Ready: Boolean read getReady; // True, wenn die Textur geladen ist und der Dialog gezeichnet werden kann

    Constructor Create; virtual;

    Procedure LoadFromDisk(Const Filename: String); // Muss im Make Current Aufgerufen werden !

    Procedure ShowInfo(Msg: String; Level: TLogLevel);
    Procedure AskYesNo(Question: String; OnYes: TNotifyEvent);

    Procedure HandleKey(Key: Word);
    Procedure HandleMouseDown(x, y: Integer); // x, y in Spielkoordinaten (GameWidth x GameHeight)
    Procedure HandleMouseMove(x, y: Integer); // x, y in Spielkoordinaten, markiert Yes / No unter der Maus
    Procedure Render; // Erwartet, dass Go2d bereits auf GameWidth x GameHeight eingestellt ist
  End;

  (*
   * Zeichnet den Rahmen aus 3x3 Kacheln zu je 24x24 Pixeln, Width und Height müssen ein Vielfaches von 24 sein.
   *)
Procedure RenderDialogFrame(Left, Top, Width, Height: integer; Depth: Single; Const Texture: TGraphikItem);

Implementation

Uses
  Graphics
  , LCLType
  , dglOpenGL
  , uatomic_common
  , uatomicfont
  , uopengl_shaderprimitives
  ;

Const
  DialogTile = 24;
  DialogWidth = 22 * DialogTile;
  LetterWidth = 10; // Siehe TAtomicFont
  LineHeight = 14;
  MaxCharsPerLine = (DialogWidth - 2 * DialogTile) Div LetterWidth;
  MaxLines = 20;
  // Linke Kante der Buttons
  ButtonOKLeft = GameWidth Div 2 - 2 * LetterWidth Div 2;
  ButtonYesLeft = GameWidth Div 2 - 60 - 3 * LetterWidth Div 2;
  ButtonNoLeft = GameWidth Div 2 + 60 - 2 * LetterWidth Div 2;

Procedure RenderDialogFrame(Left, Top, Width, Height: integer; Depth: Single;
  Const Texture: TGraphikItem);
Var
  i, j, w, h: Integer;
Begin
  w := Width Div DialogTile;
  h := Height Div DialogTile;
  // 1. Zeile
  RenderTiledQuad(Left, Top, Depth, 0, 3, 3, Texture); // Ecke Oben Links
  For i := 1 To w - 2 Do Begin
    RenderTiledQuad(Left + i * DialogTile, Top, Depth, 1, 3, 3, Texture); // Obere Mittlere Kante
  End;
  RenderTiledQuad(Left + (w - 1) * DialogTile, Top, Depth, 2, 3, 3, Texture); // Ecke Oben Rechts
  // Zeile 2 bis N -1
  For j := 1 To h - 2 Do Begin
    RenderTiledQuad(Left, Top + j * DialogTile, Depth, 3, 3, 3, Texture); // Linke Kante
    For i := 1 To w - 2 Do Begin
      RenderTiledQuad(Left + i * DialogTile, Top + j * DialogTile, Depth, 4, 3, 3, Texture); // Die Mittelstücke
    End;
    RenderTiledQuad(Left + (w - 1) * DialogTile, Top + j * DialogTile, Depth, 5, 3, 3, Texture); // Rechte Kante
  End;
  // Letzte Zeile
  RenderTiledQuad(Left, Top + (h - 1) * DialogTile, Depth, 6, 3, 3, Texture); // Ecke unten Links
  For i := 1 To w - 2 Do Begin
    RenderTiledQuad(Left + i * DialogTile, Top + (h - 1) * DialogTile, Depth, 7, 3, 3, Texture); // untere Mittlere Kante
  End;
  RenderTiledQuad(Left + (w - 1) * DialogTile, Top + (h - 1) * DialogTile, Depth, 8, 3, 3, Texture); // Ecke Unten Rechts
End;

(*
 * Bricht den Text an Leerzeichen auf MaxCharsPerLine um, zu lange Wörter (z.B. Pfade) werden hart getrennt.
 *)
Function WrapText(Text: String): TStringArray;

  Procedure AddLine(Const Line: String);
  Begin
    setlength(result, length(result) + 1);
    result[high(result)] := Line;
  End;

Var
  sl: TStringList;
  i, p: Integer;
  Paragraph, Word, Line: String;
Begin
  result := Nil;
  sl := TStringList.Create;
  sl.Text := Text;
  For i := 0 To sl.Count - 1 Do Begin
    Paragraph := sl[i];
    Line := '';
    While Paragraph <> '' Do Begin
      p := pos(' ', Paragraph);
      If p = 0 Then p := length(Paragraph) + 1;
      Word := copy(Paragraph, 1, p - 1);
      delete(Paragraph, 1, p);
      If Word = '' Then Continue;
      While length(Word) > MaxCharsPerLine Do Begin
        If Line <> '' Then AddLine(Line);
        Line := '';
        AddLine(copy(Word, 1, MaxCharsPerLine));
        delete(Word, 1, MaxCharsPerLine);
      End;
      If Line = '' Then Begin
        Line := Word;
      End
      Else If length(Line) + 1 + length(Word) <= MaxCharsPerLine Then Begin
        Line := Line + ' ' + Word;
      End
      Else Begin
        AddLine(Line);
        Line := Word;
      End;
    End;
    AddLine(Line);
  End;
  sl.Free;
  If length(result) > MaxLines Then Begin
    setlength(result, MaxLines);
    result[MaxLines - 1] := '...';
  End;
End;

{ TMessageDialog }

Constructor TMessageDialog.Create;
Begin
  Inherited Create;
  fTexture.Image := 0;
  fQueue := Nil;
  fYesSelected := false;
  OnOpen := Nil;
End;

Function TMessageDialog.getActive: Boolean;
Begin
  result := length(fQueue) <> 0;
End;

Function TMessageDialog.getReady: Boolean;
Begin
  result := fTexture.Image <> 0;
End;

Procedure TMessageDialog.LoadFromDisk(Const Filename: String);
Begin
  fTexture := OpenGL_GraphikEngine.LoadGraphikItem(Filename, smClamp);
End;

Procedure TMessageDialog.Add(Const Entry: TMessageDialogEntry);
Var
  WasActive: Boolean;
Begin
  // Die gleiche Meldung mehrfach hintereinander wird nur einmal angezeigt
  If Active And (Entry.Kind = mdkInfo) Then Begin
    If (fQueue[high(fQueue)].Kind = mdkInfo) And
      (fQueue[high(fQueue)].Level = Entry.Level) And
      (fQueue[high(fQueue)].Text = Entry.Text) Then exit;
  End;
  WasActive := Active;
  setlength(fQueue, length(fQueue) + 1);
  fQueue[high(fQueue)] := Entry;
  If Not WasActive Then Begin
    fYesSelected := false; // Vorauswahl ist "No", damit ein versehentliches Enter nichts auslöst
    If assigned(OnOpen) Then OnOpen(self);
  End;
End;

Procedure TMessageDialog.CloseActual(Yes: Boolean);
Var
  Callback: TNotifyEvent;
  i: Integer;
Begin
  If Not Active Then exit;
  Callback := fQueue[0].OnYes;
  // Erst entfernen, der Callback darf neue Dialoge öffnen
  For i := 0 To high(fQueue) - 1 Do Begin
    fQueue[i] := fQueue[i + 1];
  End;
  setlength(fQueue, high(fQueue));
  If Active Then Begin
    fYesSelected := false;
    If assigned(OnOpen) Then OnOpen(self);
  End;
  If Yes And assigned(Callback) Then Callback(self);
End;

Procedure TMessageDialog.ShowInfo(Msg: String; Level: TLogLevel);
Var
  e: TMessageDialogEntry;
Begin
  e.Kind := mdkInfo;
  e.Level := Level;
  e.Text := Msg;
  e.Lines := WrapText(Msg);
  e.OnYes := Nil;
  Add(e);
End;

Procedure TMessageDialog.AskYesNo(Question: String; OnYes: TNotifyEvent);
Var
  e: TMessageDialogEntry;
Begin
  e.Kind := mdkYesNo;
  e.Level := llInfo;
  e.Text := Question;
  e.Lines := WrapText(Question);
  e.OnYes := OnYes;
  Add(e);
End;

Procedure TMessageDialog.HandleKey(Key: Word);
Begin
  If Not Active Then exit;
  Case fQueue[0].Kind Of
    mdkInfo: Begin
        If (Key = VK_RETURN) Or (Key = VK_ESCAPE) Or (Key = VK_SPACE) Or (Key = VK_BACK) Then Begin
          CloseActual(false);
        End;
      End;
    mdkYesNo: Begin
        Case Key Of
          VK_LEFT: fYesSelected := true;
          VK_RIGHT: fYesSelected := false;
          VK_Y: CloseActual(true);
          VK_N, VK_ESCAPE, VK_BACK: CloseActual(false);
          VK_RETURN, VK_SPACE: CloseActual(fYesSelected);
        End;
      End;
  End;
End;

Procedure TMessageDialog.GetLayout(Out Left, Top, Height, ContentHeight: Integer
  );
Begin
  ContentHeight := (length(fQueue[0].Lines) + 4) * LineHeight; // Titel, Leerzeile, Text, Leerzeile, Buttons
  Height := ContentHeight + 2 * DialogTile;
  Height := ((Height + DialogTile - 1) Div DialogTile) * DialogTile;
  Left := (GameWidth - DialogWidth) Div 2;
  Top := (GameHeight - Height) Div 2;
End;

Function TMessageDialog.ButtonAt(x, y: Integer): Integer;

  Function Hit(Left, Chars: integer): Boolean;
  Begin
    result := (x >= Left - 10) And (x <= Left + Chars * LetterWidth + 10);
  End;

Var
  l, t, h, ContentHeight, by: Integer;
Begin
  result := -1;
  If Not Active Then exit;
  GetLayout(l, t, h, ContentHeight);
  by := t + DialogTile + (h - 2 * DialogTile - ContentHeight) Div 2 + ContentHeight - LineHeight;
  If (y < by - 5) Or (y > by + LineHeight + 5) Then exit;
  If fQueue[0].Kind = mdkYesNo Then Begin
    If Hit(ButtonYesLeft, 3) Then result := 0;
    If Hit(ButtonNoLeft, 2) Then result := 1;
  End
  Else Begin
    If Hit(ButtonOKLeft, 2) Then result := 0;
  End;
End;

Procedure TMessageDialog.HandleMouseDown(x, y: Integer);
Begin
  Case ButtonAt(x, y) Of
    0: CloseActual(fQueue[0].Kind = mdkYesNo);
    1: CloseActual(false);
  End;
End;

Procedure TMessageDialog.HandleMouseMove(x, y: Integer);
Begin
  Case ButtonAt(x, y) Of
    0: fYesSelected := true;
    1: fYesSelected := false;
  End;
End;

Procedure TMessageDialog.Render;

  Procedure TextoutCentered(y: integer; Const Text: String);
  Begin
    AtomicFont.Textout((GameWidth - length(Text) * LetterWidth) Div 2, y, atomic_dialog_Layer + atomic_EPSILON, Text);
  End;

Var
  l, t, h, y, i, ContentHeight: Integer;
  Title: String;
Begin
  If (Not Active) Or (Not Ready) Then exit;
  GetLayout(l, t, h, ContentHeight);
  // Der Dialog liegt über allem, deshalb wird ohne Tiefentest in der Reihenfolge gezeichnet
  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_DEPTH_TEST);
  // Das Spiel dahinter abdunkeln, wie in TGame.RenderFlash mit normalem Alpha Blending
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  UseColorShader;
  SetShaderColor(0, 0, 0, 0.5);
  glShaderBegin(GL_TRIANGLE_FAN);
  glShaderVertex(0, 0, atomic_dialog_Layer);
  glShaderVertex(GameWidth, 0, atomic_dialog_Layer);
  glShaderVertex(GameWidth, GameHeight, atomic_dialog_Layer);
  glShaderVertex(0, GameHeight, atomic_dialog_Layer);
  glShaderEnd();
  glDisable(GL_BLEND);
  glBlendFunc(gl_one, GL_ONE_MINUS_SRC_ALPHA); // Wie in TForm1.SetupFrame
  UseTextureShader;
  RenderDialogFrame(l, t, DialogWidth, h, atomic_dialog_Layer, fTexture);
  // Der Inhalt
  glBindTexture(GL_TEXTURE_2D, 0);
  AtomicFont.BackColor := clBlack;
  y := t + DialogTile + (h - 2 * DialogTile - ContentHeight) Div 2;
  If fQueue[0].Kind = mdkYesNo Then Begin
    Title := 'Question';
    AtomicFont.Color := clWhite;
  End
  Else Begin
    Case fQueue[0].Level Of
      llWarning: Begin
          Title := 'Warning';
          AtomicFont.Color := clYellow;
        End;
      llError, llCritical, llFatal: Begin
          Title := 'Error';
          AtomicFont.Color := clRed;
        End;
    Else Begin
        Title := 'Info';
        AtomicFont.Color := clWhite;
      End;
    End;
  End;
  TextoutCentered(y, Title);
  y := y + 2 * LineHeight;
  AtomicFont.Color := clWhite;
  For i := 0 To high(fQueue[0].Lines) Do Begin
    TextoutCentered(y, fQueue[0].Lines[i]);
    y := y + LineHeight;
  End;
  y := y + LineHeight;
  // Die Buttons, der ausgewählte ist gelb
  If fQueue[0].Kind = mdkYesNo Then Begin
    If fYesSelected Then AtomicFont.Color := clYellow Else AtomicFont.Color := clGray;
    AtomicFont.Textout(ButtonYesLeft, y, atomic_dialog_Layer + atomic_EPSILON, 'Yes');
    If fYesSelected Then AtomicFont.Color := clGray Else AtomicFont.Color := clYellow;
    AtomicFont.Textout(ButtonNoLeft, y, atomic_dialog_Layer + atomic_EPSILON, 'No');
  End
  Else Begin
    AtomicFont.Color := clYellow;
    AtomicFont.Textout(ButtonOKLeft, y, atomic_dialog_Layer + atomic_EPSILON, 'OK');
  End;
  AtomicFont.Color := clWhite;
  glEnable(GL_DEPTH_TEST);
End;

End.
