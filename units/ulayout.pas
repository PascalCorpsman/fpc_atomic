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
Unit ulayout;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, Controls;

(*
 * Passt ein mit festen Positionen entworfenes Fenster an das Widgetset, die Schrift und die Sprache an.
 *
 * Windows, GTK2 und GTK3 haben unterschiedlich große Controls (GTK3 z.B. deutlich höhere Edits und Comboboxen),
 * dazu kommen unterschiedlich lange Übersetzungen. Mit festen Positionen überlappen dann Texte oder werden abgeschnitten.
 *
 * Die Positionen aus dem Designer bleiben die Grundlage:
 * - Jedes Control bekommt die Größe, die das Widgetset für seinen Text braucht, aber nie weniger als im Designer
 *   (Edits, Comboboxen und SpinEdits behalten ihre Breite, Labels mit WordWrap ihre Breite und werden höher).
 * - Was nebeneinander in einer Zeile liegt, wird nach rechts geschoben, was untereinander liegt, nach unten.
 *   Controls, die im Designer die gleiche Left bzw. Top hatten, behalten sie gemeinsam (Spalten bleiben Spalten),
 *   die Abstände aus dem Designer sind die Mindestabstände.
 * - Panels, GroupBoxen, PageControls und das Fenster werden so weit vergrößert, dass der Inhalt hinein passt.
 * - Ein Label neben einem höheren Control (Edit, Combobox, ...) wird zu diesem vertikal zentriert.
 *
 * Controls mit Align, Ankern an anderen Controls oder rechts / unten werden nicht verschoben, das macht die LCL.
 *
 * Kann beliebig oft aufgerufen werden (z.B. im OnShow nach einem Sprachwechsel), es wird immer von den Positionen
 * aus dem Designer aus gerechnet, die beim ersten Aufruf gemerkt werden.
 *)
Procedure FitLayout(Root: TWinControl);

Implementation

Uses
  Math, Types, StdCtrls, ComCtrls, ExtCtrls, Forms, CheckLst;

Const
  MinGapX = 6; // Mindestabstand nebeneinander
  MinGapY = 4; // Mindestabstand untereinander
  MinMargin = 4; // Mindestabstand zum Rand des Containers

Type

  TLabelAccess = Class(TCustomLabel); // Für CalculateSize

  { TDesignBounds }

  // Die Positionen und Größen aus dem Designer, gehört dem Root und wird mit ihm frei gegeben
  TDesignBounds = Class(TComponent)
  private
    fControls: TFPList;
    fRects: Array Of TRect;
    fClients: Array Of TPoint; // ClientWidth / ClientHeight
    Procedure Add(c: TControl);
  public
    Constructor Create(aRoot: TWinControl); reintroduce;
    Destructor Destroy; override;
    Function IndexOf(c: TControl): integer;
    Function Rect(c: TControl): TRect;
    Function Client(c: TControl): TPoint;
    Procedure Restore();
  End;

  TItem = Record
    c: TControl;
    d: TRect; // Aus dem Designer
    x, y, w, h: integer; // Neu
    FreeX, FreeY: Boolean; // Wird hier waagrecht / senkrecht positioniert
    CountsX, CountsY: Boolean; // Zählt für die Breite / Höhe des Containers
  End;

  TItems = Array Of TItem;

// Links (oben) fest im Designer und nicht an einem anderen Control verankert
Function IsFreeX(c: TControl): Boolean;
Begin
  result := (c.Align = alNone) And (c.Anchors * [akLeft, akRight] = [akLeft]) And (c.AnchorSide[akLeft].Control = Nil);
End;

Function IsFreeY(c: TControl): Boolean;
Begin
  result := (c.Align = alNone) And (c.Anchors * [akTop, akBottom] = [akTop]) And (c.AnchorSide[akTop].Control = Nil);
End;

// Nur echte Container, ein Grid z.B. hat seinen Editor als Kind
Function IsContainer(c: TControl): Boolean;
Begin
  result := ((c Is TCustomPanel) Or (c Is TCustomGroupBox) Or (c Is TScrollingWinControl) Or (c Is TCustomPage)) And
    (TWinControl(c).ControlCount > 0);
End;

{ TDesignBounds }

Constructor TDesignBounds.Create(aRoot: TWinControl);
Begin
  Inherited Create(aRoot);
  fControls := TFPList.Create;
  fRects := Nil;
  fClients := Nil;
  Add(aRoot);
End;

Destructor TDesignBounds.Destroy;
Begin
  fControls.Free;
  Inherited Destroy;
End;

Procedure TDesignBounds.Add(c: TControl);
Var
  i: integer;
Begin
  i := fControls.Add(c);
  setlength(fRects, i + 1);
  setlength(fClients, i + 1);
  fRects[i] := c.BoundsRect;
  fClients[i] := Point(c.ClientWidth, c.ClientHeight);
  If IsContainer(c) Or (c Is TPageControl) Then Begin
    For i := 0 To TWinControl(c).ControlCount - 1 Do Begin
      Add(TWinControl(c).Controls[i]);
    End;
  End;
End;

Function TDesignBounds.IndexOf(c: TControl): integer;
Begin
  result := fControls.IndexOf(c);
End;

Function TDesignBounds.Rect(c: TControl): TRect;
Var
  i: integer;
Begin
  i := IndexOf(c);
  If i = -1 Then Begin
    result := c.BoundsRect; // Erst nach dem ersten Aufruf erstellt
  End
  Else Begin
    result := fRects[i];
  End;
End;

Function TDesignBounds.Client(c: TControl): TPoint;
Var
  i: integer;
Begin
  i := IndexOf(c);
  If i = -1 Then Begin
    result := Point(c.ClientWidth, c.ClientHeight);
  End
  Else Begin
    result := fClients[i];
  End;
End;

Procedure TDesignBounds.Restore;
Var
  i: integer;
  c: TControl;
  r: TRect;
Begin
  For i := 1 To fControls.Count - 1 Do Begin // 0 ist der Root, der wird am Ende gesetzt
    c := TControl(fControls[i]);
    If (c.Parent Is TPageControl) Or (c.Align <> alNone) Then Continue; // Die Seiten setzt das PageControl
    r := c.BoundsRect;
    If IsFreeX(c) Then r.Left := fRects[i].Left;
    If IsFreeY(c) Then r.Top := fRects[i].Top;
    If Not c.AutoSize Then Begin
      r.Right := r.Left + fRects[i].Right - fRects[i].Left;
      r.Bottom := r.Top + fRects[i].Bottom - fRects[i].Top;
    End;
    c.BoundsRect := r;
  End;
End;

Procedure HandlesNeeded(c: TWinControl);
Var
  i: integer;
Begin
  c.HandleNeeded;
  If (c <> c.GetTopParent) And Not (IsContainer(c) Or (c Is TPageControl)) Then exit; // Nicht z.B. in ein Grid und dessen Editor
  For i := 0 To c.ControlCount - 1 Do Begin
    If c.Controls[i] Is TWinControl Then HandlesNeeded(TWinControl(c.Controls[i]));
  End;
End;

Function LayoutChildren(p: TWinControl; Const db: TDesignBounds): TPoint; forward;

(*
 * Die neue Größe eines Controls, Container werden dabei selbst ausgelegt
 *)
Procedure ComputeSize(Var it: TItem; Const db: TDesignBounds);
Var
  pw, ph, i, dw, dh: integer;
  dc, nc: TPoint;
  pc: TPageControl;
Begin
  it.w := it.d.Right - it.d.Left;
  it.h := it.d.Bottom - it.d.Top;
  // Container
  If it.c Is TPageControl Then Begin
    pc := TPageControl(it.c);
    // Alle Seiten sind gleich groß, deren Größe im Designer ist die der aktiven Seite
    dc := Point(0, 0);
    If pc.ActivePage <> Nil Then dc := db.Client(pc.ActivePage);
    dw := 0;
    dh := 0;
    For i := 0 To pc.PageCount - 1 Do Begin
      nc := LayoutChildren(pc.Pages[i], db);
      dw := max(dw, nc.X - dc.X);
      dh := max(dh, nc.Y - dc.Y);
    End;
    it.w := it.w + dw;
    it.h := it.h + dh;
    exit;
  End;
  If IsContainer(it.c) Then Begin
    nc := LayoutChildren(TWinControl(it.c), db);
    If Not (it.c Is TScrollingWinControl) Or (it.c Is TCustomForm) Then Begin // Eine ScrollBox scrollt
      dc := db.Client(it.c);
      it.w := it.w + max(0, nc.X - dc.X);
      it.h := it.h + max(0, nc.Y - dc.Y);
    End;
    exit;
  End;
  // Labels mit Zeilenumbruch behalten ihre Breite und werden höher
  If (it.c Is TCustomLabel) And TLabelAccess(it.c).WordWrap And (Not it.c.AutoSize) Then Begin
    If TLabelAccess(it.c).Caption = '' Then exit;
    pw := 0;
    ph := 0;
    TLabelAccess(it.c).CalculateSize(it.w, pw, ph);
    it.h := max(it.h, ph);
    exit;
  End;
  // Eine Liste wird so breit wie ihr längster Eintrag (dazu Platz für Checkbox und Scrollbalken)
  If it.c Is TCustomListBox Then Begin
    pw := 0;
    For i := 0 To TCustomListBox(it.c).Items.Count - 1 Do Begin
      pw := max(pw, TCustomListBox(it.c).Canvas.TextWidth(TCustomListBox(it.c).Items[i]));
    End;
    If it.c Is TCustomCheckListBox Then pw := pw + 32;
    it.w := max(it.w, pw + 32);
    exit;
  End;
  // Nur Controls mit Text bekommen die Größe, die das Widgetset braucht
  If Not ((it.c Is TCustomLabel) Or (it.c Is TButtonControl) Or (it.c Is TCustomComboBox) Or
    ((it.c Is TCustomEdit) And Not (it.c Is TCustomMemo))) Then exit;
  pw := 0;
  ph := 0;
  it.c.GetPreferredSize(pw, ph);
  If it.c Is TCustomEdit Then Begin
    pw := it.w; // Die Breite eines Eingabefeldes kommt nicht vom Text
  End;
  If it.c Is TCustomComboBox Then Begin // Die einer Combobox vom längsten Eintrag (dazu Platz für den Pfeil)
    For i := 0 To TCustomComboBox(it.c).Items.Count - 1 Do Begin
      pw := max(pw, TCustomComboBox(it.c).Canvas.TextWidth(TCustomComboBox(it.c).Items[i]) + 48);
    End;
  End;
  If it.c.AutoSize Then Begin
    it.w := pw;
    it.h := ph;
  End
  Else Begin
    it.w := max(it.w, pw);
    it.h := max(it.h, ph);
  End;
End;

(*
 * Die Mitte des einen liegt in der Höhe des anderen -> nebeneinander in einer Zeile
 *)
Function SameRow(Const a, b: TItem): Boolean;
Var
  ca, cb: integer;
Begin
  ca := (a.d.Top + a.d.Bottom) Div 2;
  cb := (b.d.Top + b.d.Bottom) Div 2;
  result := ((cb >= a.d.Top) And (cb < a.d.Bottom)) Or ((ca >= b.d.Top) And (ca < b.d.Bottom));
End;

(*
 * Der Abstand hinter a: Bei Controls, deren Größe vom Text kommt, war schon die Größe im Designer nur die des Textes
 * beim ersten Aufruf, da zählt nur der Mindestabstand (sonst würde ein längerer Text auch schieben, wenn noch Platz ist)
 *)
Function DesignGap(Const a: TItem; Gap, MinGap: integer): integer;
Begin
  If a.c.AutoSize Then Begin
    result := MinGap;
  End
  Else Begin
    result := max(MinGap, Gap);
  End;
End;

Function OverlapX(Const a, b: TItem): Boolean;
Begin
  result := (a.x < b.x + b.w) And (b.x < a.x + a.w);
End;

Function SortedKeys(Const Items: TItems; Vertical: Boolean): TIntegerDynArray;
Var
  i, j, k, v: integer;
Begin
  result := Nil;
  For i := 0 To high(Items) Do Begin
    If Vertical Then Begin
      If Not Items[i].FreeY Then Continue;
      v := Items[i].d.Top;
    End
    Else Begin
      If Not Items[i].FreeX Then Continue;
      v := Items[i].d.Left;
    End;
    k := 0;
    While (k <= high(result)) And (result[k] < v) Do inc(k);
    If (k <= high(result)) And (result[k] = v) Then Continue;
    setlength(result, length(result) + 1);
    For j := high(result) Downto k + 1 Do result[j] := result[j - 1];
    result[k] := v;
  End;
End;

Function KeyIndex(Const Keys: TIntegerDynArray; v: integer): integer;
Begin
  For result := 0 To high(Keys) Do Begin
    If Keys[result] = v Then exit;
  End;
  result := -1;
End;

(*
 * Ein Control, das an einem Geschwister verankert ist: Wo die LCL es ungefähr hin legt (für die Größe des Containers)
 *)
Procedure EstimateX(Var it: TItem; Const Items: TItems);
Var
  i: integer;
  s: TControl;
Begin
  it.x := it.d.Left;
  s := it.c.AnchorSide[akLeft].Control;
  If (s <> Nil) And (s.Parent = it.c.Parent) Then Begin
    For i := 0 To high(Items) Do Begin
      If Items[i].c <> s Then Continue;
      Case it.c.AnchorSide[akLeft].Side Of
        asrRight: it.x := Items[i].x + Items[i].w;
        asrCenter: it.x := Items[i].x + (Items[i].w - it.w) Div 2;
      Else
        it.x := Items[i].x;
      End;
      it.x := it.x + it.c.BorderSpacing.Left + it.c.BorderSpacing.Around;
    End;
  End;
End;

Procedure EstimateY(Var it: TItem; Const Items: TItems);
Var
  i: integer;
  s: TControl;
Begin
  it.y := it.d.Top;
  s := it.c.AnchorSide[akTop].Control;
  If (s <> Nil) And (s.Parent = it.c.Parent) Then Begin
    For i := 0 To high(Items) Do Begin
      If Items[i].c <> s Then Continue;
      Case it.c.AnchorSide[akTop].Side Of
        asrBottom: it.y := Items[i].y + Items[i].h;
        asrCenter: it.y := Items[i].y + (Items[i].h - it.h) Div 2;
      Else
        it.y := Items[i].y;
      End;
      it.y := it.y + it.c.BorderSpacing.Top + it.c.BorderSpacing.Around;
      If (akBottom In it.c.Anchors) And (it.c.AnchorSide[akBottom].Control = s) Then it.h := Items[i].h;
    End;
  End;
End;

(*
 * Legt die Kinder von p aus und gibt die Client Größe zurück, die p dafür braucht
 *)
Function LayoutChildren(p: TWinControl; Const db: TDesignBounds): TPoint;
Var
  Items: TItems;
  Keys: TIntegerDynArray;
  Pos: Array Of integer;
  i, j, k, v, dMaxR, dMaxB, MaxR, MaxB, best: integer;
  dc: TPoint;
  r: TRect;
Begin
  Items := Nil;
  setlength(Items, p.ControlCount);
  For i := 0 To high(Items) Do Begin
    Items[i].c := p.Controls[i];
    Items[i].d := db.Rect(Items[i].c);
    Items[i].FreeX := IsFreeX(Items[i].c);
    Items[i].FreeY := IsFreeY(Items[i].c);
    // Was rechts oder unten verankert ist, wandert mit dem Rand mit, Align füllt
    Items[i].CountsX := (Items[i].c.Align = alNone) And (akLeft In Items[i].c.Anchors);
    Items[i].CountsY := (Items[i].c.Align = alNone) And (akTop In Items[i].c.Anchors);
    ComputeSize(Items[i], db);
  End;
  // 0. Was im Designer mit einem gewachsenen Nachbarn in der Zeile unten bündig war (z.B. eine Liste neben einem PageControl), wächst mit
  For i := 0 To high(Items) Do Begin
    If (Not Items[i].FreeY) Or (Items[i].c Is TCustomLabel) Or (Items[i].c Is TButtonControl) Or (Items[i].c Is TCustomEdit) Or
      (Items[i].c Is TCustomComboBox) Then Continue;
    v := 0;
    For j := 0 To high(Items) Do Begin
      If (i = j) Or (Not Items[j].FreeY) Or (abs(Items[i].d.Bottom - Items[j].d.Bottom) > 3) Or (Not SameRow(Items[i], Items[j])) Then Continue;
      v := max(v, Items[j].h - (Items[j].d.Bottom - Items[j].d.Top));
    End;
    Items[i].h := max(Items[i].h, Items[i].d.Bottom - Items[i].d.Top + v);
  End;
  // 1. Waagrecht: Was in einer Zeile rechts von a liegt, kommt hinter a
  Keys := SortedKeys(Items, false);
  Pos := Nil;
  setlength(Pos, length(Keys));
  For k := 0 To high(Keys) Do Begin
    If k = 0 Then Begin
      v := Keys[0];
    End
    Else Begin
      v := Pos[k - 1] + Keys[k] - Keys[k - 1];
    End;
    For i := 0 To high(Items) Do Begin
      If (Not Items[i].FreeX) Or (Items[i].d.Left <> Keys[k]) Then Continue;
      For j := 0 To high(Items) Do Begin
        If (Not Items[j].FreeX) Or (Items[j].d.Left >= Keys[k]) Or (Not SameRow(Items[i], Items[j])) Then Continue;
        v := max(v, Pos[KeyIndex(Keys, Items[j].d.Left)] + Items[j].w + DesignGap(Items[j], Keys[k] - Items[j].d.Right, MinGapX));
      End;
    End;
    Pos[k] := v;
  End;
  For i := 0 To high(Items) Do Begin
    If Items[i].FreeX Then Items[i].x := Pos[KeyIndex(Keys, Items[i].d.Left)];
  End;
  For i := 0 To high(Items) Do Begin
    If Not Items[i].FreeX Then EstimateX(Items[i], Items);
  End;
  // 2. Senkrecht: Was über b liegt (und sich waagrecht mit b überschneidet), kommt über b
  Keys := SortedKeys(Items, true);
  setlength(Pos, length(Keys));
  For k := 0 To high(Keys) Do Begin
    If k = 0 Then Begin
      v := Keys[0];
    End
    Else Begin
      v := Pos[k - 1] + Keys[k] - Keys[k - 1];
    End;
    For i := 0 To high(Items) Do Begin
      If (Not Items[i].FreeY) Or (Items[i].d.Top <> Keys[k]) Then Continue;
      For j := 0 To high(Items) Do Begin
        If (Not Items[j].FreeY) Or (Items[j].d.Top >= Keys[k]) Or SameRow(Items[i], Items[j]) Or
          (Not OverlapX(Items[i], Items[j])) Then Continue;
        v := max(v, Pos[KeyIndex(Keys, Items[j].d.Top)] + Items[j].h + DesignGap(Items[j], Keys[k] - Items[j].d.Bottom, MinGapY));
      End;
    End;
    Pos[k] := v;
  End;
  For i := 0 To high(Items) Do Begin
    If Items[i].FreeY Then Items[i].y := Pos[KeyIndex(Keys, Items[i].d.Top)];
  End;
  // 3. Ein Label wird zu dem nächsten höheren Control rechts daneben vertikal zentriert
  For i := 0 To high(Items) Do Begin
    If (Not Items[i].FreeY) Or (Not (Items[i].c Is TCustomLabel)) Or TLabelAccess(Items[i].c).WordWrap Then Continue;
    best := -1;
    For j := 0 To high(Items) Do Begin
      If (Not Items[j].FreeY) Or (Items[j].c Is TCustomLabel) Or (Items[j].d.Left <= Items[i].d.Left) Or
        (Items[j].h <= Items[i].h) Or (Not SameRow(Items[i], Items[j])) Then Continue;
      If (best = -1) Or (Items[j].x < Items[best].x) Then best := j;
    End;
    If best <> -1 Then Items[i].y := Items[best].y + (Items[best].h - Items[i].h) Div 2;
  End;
  // 4. Die verankerten Controls ungefähr
  For i := 0 To high(Items) Do Begin
    If Not Items[i].FreeY Then EstimateY(Items[i], Items);
  End;
  // 5. Die benötigte Client Größe, der Abstand zum Rand aus dem Designer bleibt
  dc := db.Client(p);
  dMaxR := 0;
  dMaxB := 0;
  MaxR := 0;
  MaxB := 0;
  For i := 0 To high(Items) Do Begin
    If Items[i].CountsX Then Begin
      dMaxR := max(dMaxR, Items[i].d.Right);
      MaxR := max(MaxR, Items[i].x + Items[i].w);
    End;
    If Items[i].CountsY Then Begin
      dMaxB := max(dMaxB, Items[i].d.Bottom);
      MaxB := max(MaxB, Items[i].y + Items[i].h);
    End;
  End;
  // Was im Designer rechts bündig mit dem Inhalt war (z.B. GroupBoxen untereinander), bleibt bündig
  For i := 0 To high(Items) Do Begin
    If Items[i].FreeX And Items[i].CountsX And (Not Items[i].c.AutoSize) And (Items[i].d.Right >= dMaxR - 1) Then Begin
      Items[i].w := max(Items[i].w, MaxR - Items[i].x);
    End;
  End;
  result.X := max(dc.X, MaxR + max(MinMargin, dc.X - dMaxR));
  result.Y := max(dc.Y, MaxB + max(MinMargin, dc.Y - dMaxB));
  // 6. Übernehmen
  For i := 0 To high(Items) Do Begin
    If (Items[i].c Is TTabSheet) Or (Items[i].c.Align <> alNone) Then Continue;
    r := Items[i].c.BoundsRect;
    If Items[i].FreeX Then r.Left := Items[i].x;
    If Items[i].FreeY Then r.Top := Items[i].y;
    // Was an beiden Seiten verankert ist, bekommt seine Größe von der LCL
    If Items[i].c.Anchors * [akLeft, akRight] <> [akLeft, akRight] Then Begin
      If Not (akLeft In Items[i].c.Anchors) Then r.Left := r.Right - Items[i].w; // Rechts bleibt
      r.Right := r.Left + Items[i].w;
    End;
    If Items[i].c.Anchors * [akTop, akBottom] <> [akTop, akBottom] Then r.Bottom := r.Top + Items[i].h;
    Items[i].c.BoundsRect := r;
  End;
End;

Procedure FitLayout(Root: TWinControl);
Var
  db: TDesignBounds;
  i: integer;
  nc, dc: TPoint;
  f: TCustomForm;
  r: TRect;
  OldW, OldH: integer;
  LockW, LockH: Boolean;
  w, h: integer;
  First: Boolean;
Begin
  db := Nil;
  For i := 0 To Root.ComponentCount - 1 Do Begin
    If Root.Components[i] Is TDesignBounds Then db := TDesignBounds(Root.Components[i]);
  End;
  HandlesNeeded(Root); // Ohne Handle kennen die Controls ihre Größe nicht
  First := db = Nil;
  If First Then Begin
    db := TDesignBounds.Create(Root);
  End
  Else Begin
    db.Restore();
  End;
  OldW := Root.Width;
  OldH := Root.Height;
  Root.DisableAutoSizing;
  Try
    nc := LayoutChildren(Root, db);
  Finally
    Root.EnableAutoSizing;
  End;
  dc := db.Client(Root);
  // Ein Fenster mit fester Größe (Constraints Min = Max) behält eine feste Größe, nur eben die neue
  LockW := (Root.Constraints.MaxWidth <> 0) And (Root.Constraints.MinWidth = Root.Constraints.MaxWidth);
  LockH := (Root.Constraints.MaxHeight <> 0) And (Root.Constraints.MinHeight = Root.Constraints.MaxHeight);
  // Die neuen Constraints vor der neuen Größe, sonst ignoriert z.B. Wayland die Größenänderung eines festen Fensters
  w := Root.Width + max(dc.X, nc.X) - Root.ClientWidth;
  h := Root.Height + max(dc.Y, nc.Y) - Root.ClientHeight;
  If LockW Then Begin
    Root.Constraints.MinWidth := 0;
    Root.Constraints.MaxWidth := w;
    Root.Constraints.MinWidth := w;
  End;
  If LockH Then Begin
    Root.Constraints.MinHeight := 0;
    Root.Constraints.MaxHeight := h;
    Root.Constraints.MinHeight := h;
  End;
  Root.SetBounds(Root.Left, Root.Top, w, h);
  // Ein Dialog, der mittig sein soll, bleibt mittig, aber nur solange er noch nicht zu sehen ist (der Benutzer hat ihn evtl. verschoben)
  If (Root Is TCustomForm) And (First Or Not Root.Showing) And ((Root.Width <> OldW) Or (Root.Height <> OldH)) Then Begin
    f := TCustomForm(Root);
    Case f.Position Of
      poMainFormCenter, poOwnerFormCenter: Begin
          If (Application.MainForm <> Nil) And (Application.MainForm <> f) Then Begin
            r := Application.MainForm.BoundsRect;
            // Aber nicht über den Rand des Bildschirms hinaus, wenn das Hauptfenster am Rand liegt
            f.SetBounds(EnsureRange(r.Left + (r.Right - r.Left - f.Width) Div 2, 0, max(0, Screen.Width - f.Width)),
              EnsureRange(r.Top + (r.Bottom - r.Top - f.Height) Div 2, 0, max(0, Screen.Height - f.Height)), f.Width, f.Height);
          End;
        End;
      poScreenCenter, poDesktopCenter: Begin
          f.SetBounds((Screen.Width - f.Width) Div 2, (Screen.Height - f.Height) Div 2, f.Width, f.Height);
        End;
    End;
  End;
End;

End.
