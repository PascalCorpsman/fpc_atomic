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

  TMessageDialogKind = (mdkInfo, mdkYesNo, mdkInput, mdkList);

  (*
   * Ein Eingabefeld von AskInput
   *)
  TDialogField = Record
    Caption: String; // Steht über dem Feld
    Value: String; // Der Startwert, nach OK der eingegebene Text
    MaxLength: Integer;
    Chars: TSysCharSet; // Die erlaubten Zeichen
  End;

  TDialogInputEvent = Procedure(Sender: TObject; Const Values: TStringArray) Of Object; // Values in der Reihenfolge der Felder
  TDialogListEvent = Procedure(Sender: TObject; Index: Integer) Of Object;

  TMessageDialogEntry = Record
    Kind: TMessageDialogKind;
    Level: TLogLevel;
    Text: String; // Der Original Text, um doppelte Meldungen zu erkennen, bei mdkInput und mdkList der Titel
    Lines: Array Of String; // Der auf die Dialogbreite umgebrochene Text
    Tag: Integer; // Kennung, um den Dialog von außen zu finden bzw. zu schließen (IsOpen, Remove), 0 = keine
    OnYes: TNotifyEvent;
    OnCancel: TNotifyEvent; // mdkInput, mdkList: Abbrechen (Esc)
    // mdkInput
    Fields: Array Of TDialogField;
    FieldIndex: Integer; // Das Feld, in das getippt wird
    OnInput: TDialogInputEvent;
    // mdkList
    Items: TStringArray;
    ItemIndex: Integer; // Der markierte Eintrag
    TopIndex: Integer; // Der oberste sichtbare Eintrag
    OnList: TDialogListEvent;
  End;

  { TMessageDialog }

  (*
   * Meldungen, Ja / Nein Abfragen, Eingaben und Auswahllisten, die direkt im Spiel (OpenGL) im Stil des
   * Lade Dialogs gezeichnet werden, statt als LCL-Fenster (die machen im Vollbild Probleme, unter Linux Abstürze).
   *
   * Der Dialog blockiert nicht, die Antwort kommt über OnYes, OnInput, OnList bzw. OnCancel.
   * Mehrere Dialoge werden nacheinander angezeigt.
   *)
  TMessageDialog = Class
  private
    fTexture: TGraphikItem; // Die 9 Einzeltexturen des Rahmens (gleiche Datei wie TLoaderDialog)
    fQueue: Array Of TMessageDialogEntry;
    fYesSelected: Boolean; // Bei mdkInput und mdkList: OK statt Abbrechen markiert (Maus)
    fCharAllowed: Boolean; // Die nächste Zeichen Eingabe gehört zu einem Tastendruck, den der offene Dialog bekommen hat
    Function getActive: Boolean;
    Function getReady: Boolean;
    Procedure Add(Const Entry: TMessageDialogEntry);
    Procedure CloseActual(Yes: Boolean);
    Procedure GetLayout(Out Left, Top, Height, ContentHeight: Integer);
    Function ContentTop(Top, Height, ContentHeight: Integer): Integer;
    Function ButtonAt(x, y: Integer): Integer; // 0 = OK bzw. Yes, 1 = No bzw. Abbrechen, -1 = kein Button
    Function RowAt(y: Integer): Integer; // mdkInput: das Feld, mdkList: der Eintrag unter y, -1 = keiner
    Procedure ScrollToItem();
  public
    OnOpen: TNotifyEvent; // Wird ausgelöst, sobald ein Dialog sichtbar wird

    Property Active: Boolean read getActive; // True, wenn gerade ein Dialog angezeigt wird
    Property Ready: Boolean read getReady; // True, wenn die Textur geladen ist und der Dialog gezeichnet werden kann

    Constructor Create; virtual;

    Procedure LoadFromDisk(Const Filename: String); // Muss im Make Current Aufgerufen werden !

    Procedure ShowInfo(Msg: String; Level: TLogLevel);
    Procedure AskYesNo(Question: String; OnYes: TNotifyEvent);
    Procedure AskInput(Title: String; Const Fields: Array Of TDialogField; OnInput: TDialogInputEvent; OnCancel: TNotifyEvent = Nil; Tag: Integer = 0);
    Procedure AskList(Title: String; Const Items: TStringArray; Selected: Integer; OnList: TDialogListEvent; OnCancel: TNotifyEvent = Nil; Tag: Integer = 0);

    Function IsOpen(Tag: Integer): Boolean; // True, wenn ein Dialog mit Tag angezeigt wird oder wartet
    Procedure Remove(Tag: Integer); // Schließt die Dialoge mit Tag, ohne OnYes / OnInput / OnList / OnCancel

    Procedure HandleKey(Key: Word);
    Procedure HandleChar(c: Char); // Die Zeichen der Eingabe (OnKeyPress, berücksichtigt das Tastaturlayout)
    Procedure HandleMouseDown(x, y: Integer); // x, y in Spielkoordinaten (GameWidth x GameHeight)
    Procedure HandleMouseMove(x, y: Integer); // x, y in Spielkoordinaten, markiert Yes / No unter der Maus
    Procedure Render; // Erwartet, dass Go2d bereits auf GameWidth x GameHeight eingestellt ist
  End;

  (*
   * Zeichnet den Rahmen aus 3x3 Kacheln zu je 24x24 Pixeln, Width und Height müssen ein Vielfaches von 24 sein.
   *)
Procedure RenderDialogFrame(Left, Top, Width, Height: integer; Depth: Single; Const Texture: TGraphikItem);

(*
 * Ein Eingabefeld für AskInput
 *)
Function DialogField(Const Caption, Value: String; MaxLength: Integer; Const Chars: TSysCharSet): TDialogField;

Implementation

Uses
  Graphics
  , LCLType
  , dglOpenGL
  , uatomic_common
  , uatomicfont
  , uopengl_shaderprimitives
  , Math
  ;

Const
  DialogTile = 24;
  DialogWidth = 22 * DialogTile;
  LetterWidth = 10; // Siehe TAtomicFont
  LineHeight = 14;
  MaxCharsPerLine = (DialogWidth - 2 * DialogTile) Div LetterWidth;
  MaxLines = 20;
  ListVisibleItems = 12; // So viele Einträge zeigt mdkList auf einmal
  // Linke Kante der Buttons
  ButtonOKLeft = GameWidth Div 2 - 2 * LetterWidth Div 2;
  ButtonYesLeft = GameWidth Div 2 - 60 - 3 * LetterWidth Div 2;
  ButtonNoLeft = GameWidth Div 2 + 60 - 2 * LetterWidth Div 2;
  ButtonInputOKLeft = GameWidth Div 2 - 60 - 2 * LetterWidth Div 2;
  ButtonCancelLeft = GameWidth Div 2 + 60 - 6 * LetterWidth Div 2;
  FieldLeft = (GameWidth - DialogWidth) Div 2 + 2 * DialogTile; // Linke Kante der Felder und Listeneinträge

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

Function DialogField(Const Caption, Value: String; MaxLength: Integer;
  Const Chars: TSysCharSet): TDialogField;
Begin
  result.Caption := Caption;
  result.Value := Value;
  result.MaxLength := MaxLength;
  result.Chars := Chars;
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

Function NewEntry(Kind: TMessageDialogKind; Const Text: String): TMessageDialogEntry;
Begin
  result.Kind := Kind;
  result.Level := llInfo;
  result.Text := Text;
  result.Lines := Nil;
  result.Tag := 0;
  result.OnYes := Nil;
  result.OnCancel := Nil;
  result.Fields := Nil;
  result.FieldIndex := 0;
  result.OnInput := Nil;
  result.Items := Nil;
  result.ItemIndex := 0;
  result.TopIndex := 0;
  result.OnList := Nil;
End;

{ TMessageDialog }

Constructor TMessageDialog.Create;
Begin
  Inherited Create;
  fTexture.Image := 0;
  fQueue := Nil;
  fYesSelected := false;
  fCharAllowed := false;
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
    // Vorauswahl ist "No", damit ein versehentliches Enter nichts auslöst, bei Eingabe und Liste "OK"
    fYesSelected := Entry.Kind In [mdkInput, mdkList];
    fCharAllowed := false;
    If assigned(OnOpen) Then OnOpen(self);
  End;
End;

Procedure TMessageDialog.CloseActual(Yes: Boolean);
Var
  e: TMessageDialogEntry;
  Values: TStringArray;
  i: Integer;
Begin
  If Not Active Then exit;
  e := fQueue[0];
  // Erst entfernen, der Callback darf neue Dialoge öffnen
  For i := 0 To high(fQueue) - 1 Do Begin
    fQueue[i] := fQueue[i + 1];
  End;
  setlength(fQueue, high(fQueue));
  fCharAllowed := false;
  If Active Then Begin
    fYesSelected := fQueue[0].Kind In [mdkInput, mdkList];
    If assigned(OnOpen) Then OnOpen(self);
  End;
  Case e.Kind Of
    mdkInfo, mdkYesNo: Begin
        If Yes And assigned(e.OnYes) Then e.OnYes(self);
      End;
    mdkInput: Begin
        If Yes Then Begin
          Values := Nil;
          setlength(Values, length(e.Fields));
          For i := 0 To high(e.Fields) Do Begin
            Values[i] := e.Fields[i].Value;
          End;
          If assigned(e.OnInput) Then e.OnInput(self, Values);
        End
        Else Begin
          If assigned(e.OnCancel) Then e.OnCancel(self);
        End;
      End;
    mdkList: Begin
        If Yes And (e.ItemIndex >= 0) And (e.ItemIndex <= high(e.Items)) Then Begin
          If assigned(e.OnList) Then e.OnList(self, e.ItemIndex);
        End
        Else Begin
          If assigned(e.OnCancel) Then e.OnCancel(self);
        End;
      End;
  End;
End;

Procedure TMessageDialog.ShowInfo(Msg: String; Level: TLogLevel);
Var
  e: TMessageDialogEntry;
Begin
  e := NewEntry(mdkInfo, Msg);
  e.Level := Level;
  e.Lines := WrapText(Msg);
  Add(e);
End;

Procedure TMessageDialog.AskYesNo(Question: String; OnYes: TNotifyEvent);
Var
  e: TMessageDialogEntry;
Begin
  e := NewEntry(mdkYesNo, Question);
  e.Lines := WrapText(Question);
  e.OnYes := OnYes;
  Add(e);
End;

Procedure TMessageDialog.AskInput(Title: String;
  Const Fields: Array Of TDialogField; OnInput: TDialogInputEvent;
  OnCancel: TNotifyEvent; Tag: Integer);
Var
  e: TMessageDialogEntry;
  i: Integer;
Begin
  e := NewEntry(mdkInput, Title);
  setlength(e.Fields, length(Fields));
  For i := 0 To high(Fields) Do Begin
    e.Fields[i] := Fields[i];
    e.Fields[i].Value := copy(e.Fields[i].Value, 1, e.Fields[i].MaxLength);
  End;
  e.OnInput := OnInput;
  e.OnCancel := OnCancel;
  e.Tag := Tag;
  Add(e);
End;

Procedure TMessageDialog.AskList(Title: String; Const Items: TStringArray;
  Selected: Integer; OnList: TDialogListEvent; OnCancel: TNotifyEvent;
  Tag: Integer);
Var
  e: TMessageDialogEntry;
Begin
  e := NewEntry(mdkList, Title);
  e.Items := copy(Items, 0, length(Items));
  e.ItemIndex := Selected;
  If (e.ItemIndex < 0) Or (e.ItemIndex > high(e.Items)) Then e.ItemIndex := 0;
  // Den markierten Eintrag möglichst in die Mitte
  e.TopIndex := e.ItemIndex - ListVisibleItems Div 2;
  If e.TopIndex > length(e.Items) - ListVisibleItems Then e.TopIndex := length(e.Items) - ListVisibleItems;
  If e.TopIndex < 0 Then e.TopIndex := 0;
  e.OnList := OnList;
  e.OnCancel := OnCancel;
  e.Tag := Tag;
  Add(e);
End;

Function TMessageDialog.IsOpen(Tag: Integer): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(fQueue) Do Begin
    If fQueue[i].Tag = Tag Then exit(true);
  End;
End;

Procedure TMessageDialog.Remove(Tag: Integer);
Var
  i, j: Integer;
  WasFirst: Boolean;
Begin
  If Tag = 0 Then exit;
  WasFirst := Active And (fQueue[0].Tag = Tag);
  j := 0;
  For i := 0 To high(fQueue) Do Begin
    If fQueue[i].Tag <> Tag Then Begin
      fQueue[j] := fQueue[i];
      inc(j);
    End;
  End;
  setlength(fQueue, j);
  If WasFirst And Active Then Begin
    fCharAllowed := false;
    fYesSelected := fQueue[0].Kind In [mdkInput, mdkList];
    If assigned(OnOpen) Then OnOpen(self);
  End;
End;

Procedure TMessageDialog.ScrollToItem;
Begin
  With fQueue[0] Do Begin
    If ItemIndex < 0 Then ItemIndex := 0;
    If ItemIndex > high(Items) Then ItemIndex := high(Items);
    If ItemIndex < TopIndex Then TopIndex := ItemIndex;
    If ItemIndex >= TopIndex + ListVisibleItems Then TopIndex := ItemIndex - ListVisibleItems + 1;
  End;
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
          VK_TAB: fYesSelected := Not fYesSelected;
          VK_Y: CloseActual(true);
          VK_N, VK_ESCAPE, VK_BACK: CloseActual(false);
          VK_RETURN, VK_SPACE: CloseActual(fYesSelected);
        End;
      End;
    mdkInput: Begin
        Case Key Of
          VK_RETURN: CloseActual(true);
          VK_ESCAPE: CloseActual(false);
          VK_TAB, VK_DOWN: fQueue[0].FieldIndex := (fQueue[0].FieldIndex + 1) Mod length(fQueue[0].Fields);
          VK_UP: fQueue[0].FieldIndex := (fQueue[0].FieldIndex + length(fQueue[0].Fields) - 1) Mod length(fQueue[0].Fields);
          VK_BACK: Begin
              With fQueue[0].Fields[fQueue[0].FieldIndex] Do Begin
                If Value <> '' Then delete(Value, length(Value), 1);
              End;
            End;
        Else Begin
            // Die Zeichen selbst kommen über HandleChar (OnKeyPress), aber nur zu Tasten, die der Dialog auch bekommen hat:
            // Die Taste, die den Dialog geöffnet hat (z.B. J), landet so nicht im Feld
            fCharAllowed := true;
          End;
        End;
      End;
    mdkList: Begin
        Case Key Of
          VK_RETURN, VK_SPACE: CloseActual(true);
          VK_ESCAPE, VK_BACK: CloseActual(false);
          VK_UP: dec(fQueue[0].ItemIndex);
          VK_DOWN: inc(fQueue[0].ItemIndex);
          VK_PRIOR: fQueue[0].ItemIndex := fQueue[0].ItemIndex - ListVisibleItems + 1;
          VK_NEXT: fQueue[0].ItemIndex := fQueue[0].ItemIndex + ListVisibleItems - 1;
          VK_HOME: fQueue[0].ItemIndex := 0;
          VK_END: fQueue[0].ItemIndex := high(fQueue[0].Items);
        End;
        If Active And (fQueue[0].Kind = mdkList) Then ScrollToItem();
      End;
  End;
End;

Procedure TMessageDialog.HandleChar(c: Char);
Begin
  If (Not Active) Or (fQueue[0].Kind <> mdkInput) Or (Not fCharAllowed) Then exit;
  fCharAllowed := false;
  With fQueue[0].Fields[fQueue[0].FieldIndex] Do Begin
    If (c In Chars) And (length(Value) < MaxLength) Then Value := Value + c;
  End;
End;

Procedure TMessageDialog.GetLayout(Out Left, Top, Height, ContentHeight: Integer
  );
Var
  n: Integer;
Begin
  Case fQueue[0].Kind Of
    mdkInput: n := 2 + 3 * length(fQueue[0].Fields) + 1; // Titel, Leerzeile, je Feld: Beschriftung, Feld, Leerzeile, Buttons
    mdkList: n := 2 + ListVisibleItems + 2; // Titel, Leerzeile, Einträge, Leerzeile, Buttons
  Else
    n := length(fQueue[0].Lines) + 4; // Titel, Leerzeile, Text, Leerzeile, Buttons
  End;
  ContentHeight := n * LineHeight;
  Height := ContentHeight + 2 * DialogTile;
  Height := ((Height + DialogTile - 1) Div DialogTile) * DialogTile;
  Left := (GameWidth - DialogWidth) Div 2;
  Top := (GameHeight - Height) Div 2;
End;

Function TMessageDialog.ContentTop(Top, Height, ContentHeight: Integer
  ): Integer;
Begin
  result := Top + DialogTile + (Height - 2 * DialogTile - ContentHeight) Div 2;
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
  by := ContentTop(t, h, ContentHeight) + ContentHeight - LineHeight;
  If (y < by - 5) Or (y > by + LineHeight + 5) Then exit;
  Case fQueue[0].Kind Of
    mdkYesNo: Begin
        If Hit(ButtonYesLeft, 3) Then result := 0;
        If Hit(ButtonNoLeft, 2) Then result := 1;
      End;
    mdkInput, mdkList: Begin
        If Hit(ButtonInputOKLeft, 2) Then result := 0;
        If Hit(ButtonCancelLeft, 6) Then result := 1;
      End;
  Else Begin
      If Hit(ButtonOKLeft, 2) Then result := 0;
    End;
  End;
End;

Function TMessageDialog.RowAt(y: Integer): Integer;
Var
  l, t, h, ContentHeight, y0, r: Integer;
Begin
  result := -1;
  If Not Active Then exit;
  GetLayout(l, t, h, ContentHeight);
  y0 := ContentTop(t, h, ContentHeight) + 2 * LineHeight;
  If y < y0 Then exit;
  Case fQueue[0].Kind Of
    mdkInput: Begin
        // Je Feld 3 Zeilen, getroffen wird die Beschriftung oder das Feld
        r := (y - y0) Div (3 * LineHeight);
        If (r <= high(fQueue[0].Fields)) And ((y - y0) Mod (3 * LineHeight) < 2 * LineHeight) Then result := r;
      End;
    mdkList: Begin
        r := (y - y0) Div LineHeight;
        If (r < ListVisibleItems) And (fQueue[0].TopIndex + r <= high(fQueue[0].Items)) Then result := fQueue[0].TopIndex + r;
      End;
  End;
End;

Procedure TMessageDialog.HandleMouseDown(x, y: Integer);
Var
  r: Integer;
Begin
  If Not Active Then exit;
  If fQueue[0].Kind In [mdkInput, mdkList] Then Begin
    r := RowAt(y);
    If r >= 0 Then Begin
      If fQueue[0].Kind = mdkInput Then Begin
        fQueue[0].FieldIndex := r;
      End
      Else Begin
        // Ein Klick auf einen Eintrag wählt ihn
        fQueue[0].ItemIndex := r;
        CloseActual(true);
      End;
      exit;
    End;
  End;
  Case ButtonAt(x, y) Of
    0: CloseActual(fQueue[0].Kind <> mdkInfo);
    1: CloseActual(false);
  End;
End;

Procedure TMessageDialog.HandleMouseMove(x, y: Integer);
Var
  r: Integer;
Begin
  If Not Active Then exit;
  If fQueue[0].Kind = mdkList Then Begin
    r := RowAt(y);
    If r >= 0 Then fQueue[0].ItemIndex := r;
  End;
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

  Procedure Textout(x, y: integer; Const Text: String);
  Begin
    AtomicFont.Textout(x, y, atomic_dialog_Layer + atomic_EPSILON, Text);
  End;

Var
  l, t, h, y, i, ContentHeight: Integer;
  Title, s: String;
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
  y := ContentTop(t, h, ContentHeight);
  Case fQueue[0].Kind Of
    mdkYesNo: Begin
        Title := 'Question';
        AtomicFont.Color := clWhite;
      End;
    mdkInput, mdkList: Begin
        Title := fQueue[0].Text;
        AtomicFont.Color := clWhite;
      End;
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
  End;
  // Bei langen Listen steht die Position mit im Titel
  If (fQueue[0].Kind = mdkList) And (length(fQueue[0].Items) > ListVisibleItems) Then Begin
    Title := Title + format(' (%d/%d)', [fQueue[0].ItemIndex + 1, length(fQueue[0].Items)]);
  End;
  TextoutCentered(y, Title);
  y := y + 2 * LineHeight;
  Case fQueue[0].Kind Of
    mdkInput: Begin
        For i := 0 To high(fQueue[0].Fields) Do Begin
          AtomicFont.Color := clWhite;
          Textout(FieldLeft, y, fQueue[0].Fields[i].Caption);
          y := y + LineHeight;
          // Das Feld, in das getippt wird, ist gelb und hat den Cursor
          s := '> ' + fQueue[0].Fields[i].Value;
          If i = fQueue[0].FieldIndex Then Begin
            AtomicFont.Color := clYellow;
            If (GetTickCount64 Div 400) Mod 2 = 0 Then s := s + '_';
          End
          Else Begin
            AtomicFont.Color := clGray;
          End;
          Textout(FieldLeft, y, s);
          y := y + 2 * LineHeight;
        End;
      End;
    mdkList: Begin
        For i := fQueue[0].TopIndex To min(high(fQueue[0].Items), fQueue[0].TopIndex + ListVisibleItems - 1) Do Begin
          If i = fQueue[0].ItemIndex Then Begin
            AtomicFont.Color := clYellow;
            s := '> ' + fQueue[0].Items[i];
          End
          Else Begin
            AtomicFont.Color := clWhite;
            s := '  ' + fQueue[0].Items[i];
          End;
          Textout(FieldLeft, y, copy(s, 1, MaxCharsPerLine));
          y := y + LineHeight;
        End;
        y := ContentTop(t, h, ContentHeight) + ContentHeight - 2 * LineHeight;
      End;
  Else Begin
      AtomicFont.Color := clWhite;
      For i := 0 To high(fQueue[0].Lines) Do Begin
        TextoutCentered(y, fQueue[0].Lines[i]);
        y := y + LineHeight;
      End;
    End;
  End;
  y := ContentTop(t, h, ContentHeight) + ContentHeight - LineHeight;
  // Die Buttons, der ausgewählte ist gelb
  Case fQueue[0].Kind Of
    mdkYesNo: Begin
        If fYesSelected Then AtomicFont.Color := clYellow Else AtomicFont.Color := clGray;
        Textout(ButtonYesLeft, y, 'Yes');
        If fYesSelected Then AtomicFont.Color := clGray Else AtomicFont.Color := clYellow;
        Textout(ButtonNoLeft, y, 'No');
      End;
    mdkInput, mdkList: Begin
        If fYesSelected Then AtomicFont.Color := clYellow Else AtomicFont.Color := clGray;
        Textout(ButtonInputOKLeft, y, 'OK');
        If fYesSelected Then AtomicFont.Color := clGray Else AtomicFont.Color := clYellow;
        Textout(ButtonCancelLeft, y, 'Cancel');
      End;
  Else Begin
      AtomicFont.Color := clYellow;
      Textout(ButtonOKLeft, y, 'OK');
    End;
  End;
  AtomicFont.Color := clWhite;
  glEnable(GL_DEPTH_TEST);
End;

End.
