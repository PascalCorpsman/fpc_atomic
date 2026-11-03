(******************************************************************************)
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
(*
 * Editor of a graphics set (--new / --set <folder>).
 *
 * Left the areas of the set, in the middle the images of the selected area,
 * right its parameters. Every area is rendered into memory by a worker thread
 * (one at a time, the selected area first, slider changes are debounced).
 * Player figures are only rendered when they are selected. "Save" runs all
 * generators like the command line version into assets/<folder>/.
 *)
Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, uag_context, uag_output, uag_worker;

Type

  { TAreaNode: one entry of the tree }

  TAreaNode = Class
    Kind: TAGAreaKind;
    Index: Integer;
    Caption: String;
    Output: TAGOutput; // rendered images, Nil = not rendered yet
    Params: TAGParamList;
    Dirty: Boolean; // needs to be rendered (again)
    TreeNode: TTreeNode;
    Destructor Destroy; override;
    Function Area: String;
    Function IsFigure: Boolean;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    ButtonAreaDice: TButton;
    ButtonClose: TButton;
    ButtonResetAll: TButton;
    ButtonSave: TButton;
    ButtonSaveAs: TButton;
    ButtonSeedDice: TButton;
    CheckLock: TCheckBox;
    CheckPerPlayer: TCheckBox;
    ComboColor: TComboBox;
    ComboDir: TComboBox;
    ComboImage: TComboBox;
    ComboStyle: TComboBox;
    ComboZoom: TComboBox;
    EditName: TEdit;
    EditSeed: TEdit;
    LabelArea: TLabel;
    LabelAreaSeed: TLabel;
    LabelColor: TLabel;
    LabelDir: TLabel;
    LabelImage: TLabel;
    LabelName: TLabel;
    LabelSeed: TLabel;
    LabelStatus: TLabel;
    LabelStyle: TLabel;
    LabelZoom: TLabel;
    PaintBox1: TPaintBox;
    PaintBoxAnim: TPaintBox;
    PanelAnim: TPanel;
    PanelAreaHead: TPanel;
    PanelBottom: TPanel;
    PanelCenter: TPanel;
    PanelRight: TPanel;
    PanelTop: TPanel;
    PanelView: TPanel;
    ProgressBar1: TProgressBar;
    ScrollBox1: TScrollBox;
    ScrollBoxParams: TScrollBox;
    TimerAnim: TTimer;
    TimerDebounce: TTimer;
    TreeView1: TTreeView;
    Procedure ButtonAreaDiceClick(Sender: TObject);
    Procedure ButtonCloseClick(Sender: TObject);
    Procedure ButtonResetAllClick(Sender: TObject);
    Procedure ButtonSaveAsClick(Sender: TObject);
    Procedure ButtonSaveClick(Sender: TObject);
    Procedure ButtonSeedDiceClick(Sender: TObject);
    Procedure CheckLockChange(Sender: TObject);
    Procedure CheckPerPlayerChange(Sender: TObject);
    Procedure ComboDirChange(Sender: TObject);
    Procedure ComboImageChange(Sender: TObject);
    Procedure ComboStyleChange(Sender: TObject);
    Procedure EditNameChange(Sender: TObject);
    Procedure EditSeedEditingDone(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBoxAnimPaint(Sender: TObject);
    Procedure TimerAnimTimer(Sender: TObject);
    Procedure TimerDebounceTimer(Sender: TObject);
    Procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    fCtx: TAGContext;
    fNodes: Array Of TAreaNode;
    fFolder: String; // '' = not saved yet
    fShown, fModified, fUpdating, fCloseAfterSave, fScreenshotDone: Boolean;
    fJob: TAGAreaWorker; // running render, Nil if none
    fJobNode: TAreaNode;
    fSaver: TAGSetWorker; // running save, Nil if none
    fSaveFolder: String;
    fDead: TList; // finished workers, freed later
    fViewBmp, fAnimBmp: TBitmap;
    fAnimFrame: Integer;
    fShownNode: TAreaNode; // node of the parameter rows
    fShownRendered: Boolean; // the rows were built after the node was rendered
    fRowLabels: Array Of TLabel;
    fRowBars: Array Of TTrackBar;
    fTestStep: Integer; // --test-save
    Procedure CreateNodes;
    Procedure BuildTree;
    Function SelectedNode: TAreaNode;
    Function FindNode(Kind: TAGAreaKind; Index: Integer): TAreaNode;
    Procedure ContextToControls;
    Procedure MarkDirty(Node: TAreaNode);
    Procedure MarkAllDirty;
    Procedure Changed;
    Procedure Schedule;
    Procedure JobDone(Sender: TObject);
    Procedure FreeDead;
    Procedure ShowNode(NewSelection: Boolean);
    Procedure FillImageCombo(Node: TAreaNode);
    Procedure UpdateView;
    Procedure UpdateAreaHead;
    Procedure BuildParamRows(Node: TAreaNode);
    Procedure UpdateParamRows(Node: TAreaNode; Initial: Boolean);
    Function ParamText(Const p: TAGParamInfo): String;
    Procedure ParamBarChange(Sender: TObject);
    Procedure ParamResetClick(Sender: TObject);
    Procedure StartSave(Const Folder: String);
    Procedure SaveProgress(Sender: TObject);
    Procedure SaveDone(Sender: TObject);
    Procedure EnableEditing(Value: Boolean);
    Procedure SetStatus(Const s: String);
  public
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses Math, uag_random, uag_image, uag_preview, uag_gen_world, uag_gui, ulanguage;

Const
  RowHeight = 62;
  VarySteps = 1000;

  { TAreaNode }

Destructor TAreaNode.Destroy;
Begin
  Output.Free;
  Inherited Destroy;
End;

Function TAreaNode.Area: String;
Begin
  result := AreaName(Kind, Index);
End;

Function TAreaNode.IsFigure: Boolean;
Begin
  result := Kind In [akFigure, akPlayer];
End;

Function ThemeCaption(Index: Integer): String;
Begin
  Case MapThemeName(Index) Of
    'meadow': result := R_ThemeMeadow;
    'classic': result := R_ThemeClassic;
    'ice': result := R_ThemeIce;
    'egypt': result := R_ThemeEgypt;
    'mine': result := R_ThemeMine;
    'beach': result := R_ThemeBeach;
    'alien': result := R_ThemeAlien;
    'haunted': result := R_ThemeHaunted;
    'water': result := R_ThemeWater;
    'forest': result := R_ThemeForest;
  Else
    result := R_ThemeCity;
  End;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  fDead := TList.Create;
  fCtx := TAGContext.Create;
  fViewBmp := TBitmap.Create;
  fAnimBmp := TBitmap.Create;
  // ScrollBox1 shows the images in their own colors
  ScrollBox1.Color := RGBToColor(48, 48, 56);
  PaintBoxAnim.Color := RGBToColor(48, 48, 56);
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Var
  i: Integer;
Begin
  TimerAnim.Enabled := false;
  TimerDebounce.Enabled := false;
  FreeAndNil(fJob);
  FreeAndNil(fSaver);
  FreeDead;
  fDead.Free;
  For i := 0 To high(fNodes) Do
    fNodes[i].Free;
  fNodes := Nil;
  fViewBmp.Free;
  fAnimBmp.Free;
  fCtx.Free;
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  err: String;
Begin
  If fShown Then exit;
  fShown := true;
  // the language is loaded now, so the resource strings can be used
  err := '';
  If (GuiMode = gmSet) And FileExists(SetDir(GuiFolder) + 'profile.ini') Then Begin
    Try
      fCtx.LoadFromFile(SetDir(GuiFolder) + 'profile.ini');
    Except
      On e: Exception Do err := e.Message;
    End;
    fFolder := GuiFolder;
  End
  Else Begin
    fCtx.Seed := NewRandomSeed;
    fCtx.Name := R_DefaultName;
    If GuiMode = gmSet Then Begin
      // no set in this folder yet, "Save" creates it
      fCtx.Name := GuiFolder;
      fFolder := GuiFolder;
    End;
  End;
  If err <> '' Then
    MessageDlg(format(RF_LoadError, [GuiFolder, err]), mtError, [mbOK], 0);
  ContextToControls;
  CreateNodes;
  BuildTree;
  fModified := false;
  Schedule;
End;

Procedure TForm1.ContextToControls;
Begin
  fUpdating := true;
  Try
    EditName.Text := fCtx.Name;
    ComboStyle.ItemIndex := ord(fCtx.Style);
    EditSeed.Text := IntToStr(fCtx.Seed);
    CheckPerPlayer.Checked := fCtx.PerPlayer;
    ButtonSave.Enabled := true;
  Finally
    fUpdating := false;
  End;
End;

Procedure TForm1.CreateNodes;

  Procedure Add(Kind: TAGAreaKind; Index: Integer; Const Caption: String);
  Var
    n: TAreaNode;
  Begin
    n := TAreaNode.Create;
    n.Kind := Kind;
    n.Index := Index;
    n.Caption := Caption;
    n.Dirty := true;
    setlength(fNodes, length(fNodes) + 1);
    fNodes[high(fNodes)] := n;
  End;

Var
  i: Integer;
Begin
  Add(akFigure, 0, R_AreaFigure);
  For i := 0 To 9 Do
    Add(akPlayer, i, format(RF_AreaPlayer, [i + 1]));
  Add(akBomb, 0, R_AreaBombs);
  Add(akFlame, 0, R_AreaFlames);
  Add(akPowerups, 0, R_AreaPowerups);
  Add(akObjects, 0, R_AreaObjects);
  For i := 0 To MapCount - 1 Do
    Add(akMap, i, format('Field%.2d  %s', [i, ThemeCaption(i)]));
  Add(akScreens, 0, R_AreaScreens);
  Add(akPreview, 0, R_AreaPreview);
End;

Procedure TForm1.BuildTree;
Var
  sel: TAreaNode;
  i: Integer;
  fig, maps, t: TTreeNode;
  n: TAreaNode;
Begin
  sel := SelectedNode;
  fUpdating := true;
  TreeView1.Items.BeginUpdate;
  Try
    TreeView1.Items.Clear;
    fig := Nil;
    maps := Nil;
    For i := 0 To high(fNodes) Do Begin
      n := fNodes[i];
      n.TreeNode := Nil;
      Case n.Kind Of
        akFigure: Begin
            t := TreeView1.Items.Add(Nil, n.Caption);
            fig := t;
          End;
        akPlayer: Begin
            If Not fCtx.PerPlayer Then Continue;
            t := TreeView1.Items.AddChild(fig, n.Caption);
          End;
        akMap: Begin
            If maps = Nil Then maps := TreeView1.Items.Add(Nil, R_AreaMaps);
            t := TreeView1.Items.AddChild(maps, n.Caption);
          End;
      Else
        t := TreeView1.Items.Add(Nil, n.Caption);
      End;
      t.Data := n;
      n.TreeNode := t;
    End;
    If assigned(fig) Then fig.Expanded := true;
  Finally
    TreeView1.Items.EndUpdate;
    fUpdating := false;
  End;
  If (sel = Nil) And (GuiArea >= 0) And (GuiArea <= high(fNodes)) Then sel := fNodes[GuiArea];
  If (sel = Nil) Or (sel.TreeNode = Nil) Then sel := fNodes[0];
  TreeView1.Selected := sel.TreeNode;
  ShowNode(true);
End;

Function TForm1.SelectedNode: TAreaNode;
Begin
  result := Nil;
  If assigned(TreeView1.Selected) Then result := TAreaNode(TreeView1.Selected.Data);
End;

Function TForm1.FindNode(Kind: TAGAreaKind; Index: Integer): TAreaNode;
Var
  i: Integer;
Begin
  For i := 0 To high(fNodes) Do
    If (fNodes[i].Kind = Kind) And (fNodes[i].Index = Index) Then exit(fNodes[i]);
  result := Nil;
End;

Procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
Var
  i: Integer;
  sel: TAreaNode;
Begin
  If fUpdating Or (Node = Nil) Then exit;
  If (Node.Data = Nil) And Node.HasChildren Then Begin
    // "Maps": show the first map
    TreeView1.Selected := Node.GetFirstChild;
    exit;
  End;
  sel := SelectedNode;
  // player figures need a lot of memory, only the selected one is kept
  For i := 0 To high(fNodes) Do
    If (fNodes[i].Kind = akPlayer) And (fNodes[i] <> sel) And assigned(fNodes[i].Output) Then Begin
      FreeAndNil(fNodes[i].Output);
      fNodes[i].Dirty := true;
    End;
  ShowNode(true);
  Schedule;
End;

(* ------------------------------------------------------------------------ *)
(* Rendering                                                                *)

Procedure TForm1.Changed;
Begin
  fModified := true;
  // debounce: the next render starts 150 ms after the last change
  TimerDebounce.Enabled := false;
  TimerDebounce.Enabled := true;
End;

Procedure TForm1.MarkDirty(Node: TAreaNode);
Var
  p: TAreaNode;
Begin
  Node.Dirty := true;
  If fJobNode = Node Then fJob.Cancel;
  p := FindNode(akPreview, 0);
  If (Node <> p) And (Node.Kind In [akFigure, akBomb, akMap]) Then Begin
    p.Dirty := true;
    If fJobNode = p Then fJob.Cancel;
  End;
  Changed;
End;

Procedure TForm1.MarkAllDirty;
Var
  i: Integer;
Begin
  For i := 0 To high(fNodes) Do
    fNodes[i].Dirty := true;
  If assigned(fJob) Then fJob.Cancel;
  Changed;
End;

Procedure TForm1.TimerDebounceTimer(Sender: TObject);
Begin
  TimerDebounce.Enabled := false;
  Schedule;
End;

Procedure TForm1.FreeDead;
Var
  i: Integer;
Begin
  For i := 0 To fDead.Count - 1 Do
    TObject(fDead[i]).Free;
  fDead.Clear;
End;

Procedure TForm1.Schedule;
Var
  n, sel: TAreaNode;
  i: Integer;
Begin
  If assigned(fJob) Or assigned(fSaver) Or (length(fNodes) = 0) Then exit;
  FreeDead;
  n := Nil;
  sel := SelectedNode;
  If assigned(sel) And sel.Dirty Then
    n := sel
  Else Begin
    // the launcher image first, then everything in the order of the tree (players only when selected)
    If FindNode(akPreview, 0).Dirty Then
      n := FindNode(akPreview, 0)
    Else
      For i := 0 To high(fNodes) Do
        If fNodes[i].Dirty And (fNodes[i].Kind <> akPlayer) Then Begin
          n := fNodes[i];
          break;
        End;
  End;
  If n = Nil Then Begin
    SetStatus(R_Ready);
    If GuiTestSave <> '' Then Begin
      // debug: the same steps a user would do (the handlers call Schedule again, fTestStep < 0 = inside)
      If fTestStep < 0 Then exit;
      fTestStep := -fTestStep - 1;
      Case fTestStep Of
        -1: Begin
            CheckPerPlayer.Checked := true;
            If length(fRowBars) > 0 Then fRowBars[0].Position := fRowBars[0].Max;
            ButtonAreaDiceClick(Nil);
          End;
        -2: Begin
            fCloseAfterSave := true;
            StartSave(GuiTestSave);
          End;
      End;
      fTestStep := -fTestStep;
      exit;
    End;
    If (GuiScreenshot <> '') And Not fScreenshotDone Then Begin
      fScreenshotDone := true;
      Application.ProcessMessages;
      SaveFormScreenshot(self, GuiScreenshot);
      fModified := false;
      Close;
    End;
    exit;
  End;
  SetStatus(R_Rendering + '  ' + n.Caption);
  n.Dirty := false;
  fJobNode := n;
  fJob := TAGAreaWorker.Create(fCtx, '', @JobDone);
  fJob.Kind := n.Kind;
  fJob.Index := n.Index;
  fJob.Start;
End;

Procedure TForm1.JobDone(Sender: TObject);
Var
  w: TAGAreaWorker;
  n: TAreaNode;
Begin
  If Sender <> fJob Then exit;
  w := fJob;
  n := fJobNode;
  fJob := Nil;
  fJobNode := Nil;
  fDead.Add(w);
  If w.Error <> '' Then
    SetStatus(format(RF_Error, [w.Error]))
  Else If Not w.Aborted Then Begin
    n.Output.Free;
    n.Output := w.Output;
    w.Output := Nil;
    n.Params := w.Params;
    If n = SelectedNode Then ShowNode(false);
  End;
  If Not TimerDebounce.Enabled Then Schedule;
End;

(* ------------------------------------------------------------------------ *)
(* View                                                                     *)

Procedure TForm1.ShowNode(NewSelection: Boolean);
Var
  n: TAreaNode;
Begin
  n := SelectedNode;
  If n = Nil Then exit;
  PanelAnim.Visible := n.IsFigure;
  LabelColor.Visible := n.IsFigure;
  ComboColor.Visible := n.IsFigure;
  LabelDir.Visible := n.IsFigure;
  ComboDir.Visible := n.IsFigure;
  FillImageCombo(n);
  UpdateView;
  UpdateAreaHead;
  If NewSelection Or (n <> fShownNode) Or (length(fRowBars) <> length(n.Params)) Or (fShownRendered <> assigned(n.Output)) Then
    BuildParamRows(n)
  Else
    UpdateParamRows(n, false);
End;

Procedure TForm1.FillImageCombo(Node: TAreaNode);
Var
  old, def, s: String;
  i, idx: Integer;
Begin
  old := '';
  If (ComboImage.ItemIndex >= 0) And (fShownNode = Node) Then old := ComboImage.Items[ComboImage.ItemIndex];
  ComboImage.Items.BeginUpdate;
  Try
    ComboImage.Items.Clear;
    If assigned(Node.Output) Then
      For i := 0 To Node.Output.Images.Count - 1 Do Begin
        s := Node.Output.Images[i];
        If s = 'scene' Then s := R_Scene;
        ComboImage.Items.Add(s);
      End;
  Finally
    ComboImage.Items.EndUpdate;
  End;
  Case Node.Kind Of
    akFigure: def := 'atomic/walk.png';
    akPlayer: def := 'atomic/p' + IntToStr(Node.Index) + '/walk.png';
    akBomb: def := 'atomic/bomb.png';
    akScreens: def := 'res/mainmenu.png';
    akMap: def := R_Scene;
  Else
    def := '';
  End;
  idx := ComboImage.Items.IndexOf(old);
  If idx < 0 Then idx := ComboImage.Items.IndexOf(def);
  If (idx < 0) And (ComboImage.Items.Count > 0) Then idx := 0;
  ComboImage.ItemIndex := idx;
End;

Procedure TForm1.UpdateView;
Var
  n: TAreaNode;
  img, walk: TAGImage;
  zoom: Integer;
  b: TBitmap;
Begin
  n := SelectedNode;
  img := Nil;
  walk := Nil;
  If assigned(n) And assigned(n.Output) And (ComboImage.ItemIndex >= 0) Then Begin
    img := TAGImage(n.Output.Images.Objects[ComboImage.ItemIndex]);
    If n.Kind = akPlayer Then
      walk := n.Output.FindImage('atomic/p' + IntToStr(n.Index) + '/walk.png')
    Else
      walk := n.Output.FindImage('atomic/walk.png');
  End;
  zoom := ComboZoom.ItemIndex + 1;
  If assigned(img) Then Begin
    If n.IsFigure Then Begin
      // recolored like the engine does
      img := img.Clone;
      RecolorPlayer(img, AGPlayerColors[Max(0, ComboColor.ItemIndex)]);
      b := ImageToBitmap(img, zoom);
      img.Free;
    End
    Else
      b := ImageToBitmap(img, zoom);
    fViewBmp.Free;
    fViewBmp := b;
  End
  Else
    fViewBmp.SetSize(0, 0);
  PaintBox1.SetBounds(0, 0, Max(1, fViewBmp.Width), Max(1, fViewBmp.Height));
  PaintBox1.Invalidate;
  If assigned(walk) And n.IsFigure Then Begin
    walk := walk.Clone;
    RecolorPlayer(walk, AGPlayerColors[Max(0, ComboColor.ItemIndex)]);
    b := ImageToBitmap(walk, 2);
    walk.Free;
    fAnimBmp.Free;
    fAnimBmp := b;
  End
  Else
    fAnimBmp.SetSize(0, 0);
  PaintBoxAnim.Invalidate;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Begin
  If fViewBmp.Width > 0 Then
    PaintBox1.Canvas.Draw(0, 0, fViewBmp);
End;

Procedure TForm1.PaintBoxAnimPaint(Sender: TObject);
Const
  // rows of walk.png: right, up, down, left; the combo box: down, left, up, right
  Rows: Array[0..3] Of Integer = (2, 3, 1, 0);
  Cell = 220; // 110 x 110 at zoom 2
Var
  r, x, y: Integer;
  src, dst: TRect;
Begin
  PaintBoxAnim.Canvas.Brush.Color := PaintBoxAnim.Color;
  PaintBoxAnim.Canvas.FillRect(0, 0, PaintBoxAnim.Width, PaintBoxAnim.Height);
  If fAnimBmp.Width < 15 * Cell Then exit;
  r := Rows[Max(0, ComboDir.ItemIndex)];
  src := Rect(fAnimFrame * Cell, r * Cell, (fAnimFrame + 1) * Cell, (r + 1) * Cell);
  x := (PaintBoxAnim.Width - Cell) Div 2;
  y := 16;
  dst := Rect(x, y, x + Cell, y + Cell);
  PaintBoxAnim.Canvas.CopyRect(dst, fAnimBmp.Canvas, src);
End;

Procedure TForm1.TimerAnimTimer(Sender: TObject);
Begin
  If Not PanelAnim.Visible Then exit;
  fAnimFrame := (fAnimFrame + 1) Mod 15;
  PaintBoxAnim.Invalidate;
End;

Procedure TForm1.ComboImageChange(Sender: TObject);
Begin
  UpdateView;
End;

Procedure TForm1.ComboDirChange(Sender: TObject);
Begin
  PaintBoxAnim.Invalidate;
End;

(* ------------------------------------------------------------------------ *)
(* Parameters                                                               *)

Procedure TForm1.UpdateAreaHead;
Var
  n: TAreaNode;
Begin
  n := SelectedNode;
  If n = Nil Then exit;
  LabelArea.Caption := n.Caption;
  LabelAreaSeed.Caption := format(RF_AreaSeed, [IntToStr(fCtx.AreaSeed(n.Area))]);
  fUpdating := true;
  Try
    CheckLock.Checked := fCtx.IsSeedLocked(n.Area);
  Finally
    fUpdating := false;
  End;
End;

Function TForm1.ParamText(Const p: TAGParamInfo): String;
Begin
  If p.IsPick Then
    result := format('%s = %d', [p.Name, Round(p.Value)])
  Else
    result := format('%s = %s', [p.Name, FormatFloat('0.###', p.Value)]);
  If p.Overridden Then result := result + '  *';
End;

Procedure TForm1.BuildParamRows(Node: TAreaNode);
Var
  i, w: Integer;
  l: TLabel;
  b: TTrackBar;
  r: TButton;
Begin
  fShownNode := Node;
  fShownRendered := assigned(Node.Output);
  ScrollBoxParams.DisableAutoSizing;
  Try
    For i := ScrollBoxParams.ControlCount - 1 Downto 0 Do
      ScrollBoxParams.Controls[i].Free;
    fRowLabels := Nil;
    fRowBars := Nil;
    setlength(fRowLabels, length(Node.Params));
    setlength(fRowBars, length(Node.Params));
    w := ScrollBoxParams.ClientWidth - 16;
    If length(Node.Params) = 0 Then Begin
      l := TLabel.Create(ScrollBoxParams);
      l.Parent := ScrollBoxParams;
      l.WordWrap := true;
      l.AutoSize := false;
      l.SetBounds(8, 8, w, 60);
      If Node.Output = Nil Then
        l.Caption := R_Rendering
      Else
        l.Caption := R_NoParams;
      exit;
    End;
    fUpdating := true;
    For i := 0 To high(Node.Params) Do Begin
      l := TLabel.Create(ScrollBoxParams);
      l.Parent := ScrollBoxParams;
      l.SetBounds(8, 6 + i * RowHeight, w - 70, 18);
      fRowLabels[i] := l;
      r := TButton.Create(ScrollBoxParams);
      r.Parent := ScrollBoxParams;
      r.SetBounds(w - 62, 2 + i * RowHeight, 66, 24);
      r.Caption := R_Reset;
      r.Tag := i;
      r.OnClick := @ParamResetClick;
      b := TTrackBar.Create(ScrollBoxParams);
      b.Parent := ScrollBoxParams;
      b.SetBounds(4, 26 + i * RowHeight, w, 30);
      b.TickStyle := tsNone;
      b.Min := 0;
      If Node.Params[i].IsPick Then
        b.Max := Max(1, Round(Node.Params[i].Spread))
      Else
        b.Max := VarySteps;
      b.Tag := i;
      b.OnChange := @ParamBarChange;
      fRowBars[i] := b;
    End;
    fUpdating := false;
  Finally
    ScrollBoxParams.EnableAutoSizing;
  End;
  UpdateParamRows(Node, true);
End;

Procedure TForm1.UpdateParamRows(Node: TAreaNode; Initial: Boolean);
Var
  i, pos: Integer;
  p: TAGParamInfo;
Begin
  fUpdating := true;
  Try
    For i := 0 To Min(high(Node.Params), high(fRowBars)) Do Begin
      p := Node.Params[i];
      fRowLabels[i].Caption := ParamText(p);
      If p.Overridden Then
        fRowLabels[i].Font.Style := [fsBold]
      Else
        fRowLabels[i].Font.Style := [];
      If p.IsPick Then
        pos := Round(p.Value)
      Else If p.Spread > 0 Then
        pos := Round((p.Value - (p.Base - p.Spread)) / (2 * p.Spread) * VarySteps)
      Else
        pos := VarySteps Div 2;
      pos := EnsureRange(pos, fRowBars[i].Min, fRowBars[i].Max);
      // an overridden value is the one of the slider, an older render must not move it back
      If Initial Or Not p.Overridden Then
        fRowBars[i].Position := pos;
    End;
  Finally
    fUpdating := false;
  End;
End;

Procedure TForm1.ParamBarChange(Sender: TObject);
Var
  n: TAreaNode;
  i: Integer;
  v: Double;
  b: TTrackBar;
Begin
  If fUpdating Then exit;
  n := fShownNode;
  b := TTrackBar(Sender);
  i := b.Tag;
  If (n = Nil) Or (i > high(n.Params)) Then exit;
  If n.Params[i].IsPick Then
    v := b.Position
  Else
    v := Round((n.Params[i].Base - n.Params[i].Spread + 2 * n.Params[i].Spread * b.Position / VarySteps) * 10000) / 10000;
  fCtx.SetOverride(n.Area, n.Params[i].Name, v);
  n.Params[i].Value := v;
  n.Params[i].Overridden := true;
  fRowLabels[i].Caption := ParamText(n.Params[i]);
  fRowLabels[i].Font.Style := [fsBold];
  MarkDirty(n);
End;

Procedure TForm1.ParamResetClick(Sender: TObject);
Var
  n: TAreaNode;
  i: Integer;
Begin
  n := fShownNode;
  i := TComponent(Sender).Tag;
  If (n = Nil) Or (i > high(n.Params)) Then exit;
  fCtx.ClearOverride(n.Area, n.Params[i].Name);
  n.Params[i].Overridden := false;
  fRowLabels[i].Font.Style := [];
  MarkDirty(n);
End;

Procedure TForm1.ButtonResetAllClick(Sender: TObject);
Var
  n: TAreaNode;
  i: Integer;
Begin
  n := SelectedNode;
  If n = Nil Then exit;
  For i := 0 To high(n.Params) Do Begin
    fCtx.ClearOverride(n.Area, n.Params[i].Name);
    n.Params[i].Overridden := false;
    If i <= high(fRowLabels) Then fRowLabels[i].Font.Style := [];
  End;
  MarkDirty(n);
End;

Procedure TForm1.ButtonAreaDiceClick(Sender: TObject);
Var
  n: TAreaNode;
Begin
  n := SelectedNode;
  If n = Nil Then exit;
  fCtx.LockSeed(n.Area, NewRandomSeed);
  UpdateAreaHead;
  MarkDirty(n);
End;

Procedure TForm1.CheckLockChange(Sender: TObject);
Var
  n: TAreaNode;
Begin
  If fUpdating Then exit;
  n := SelectedNode;
  If n = Nil Then exit;
  If CheckLock.Checked Then Begin
    fCtx.LockSeed(n.Area, fCtx.AreaSeed(n.Area)); // looks the same, but stays when the main seed changes
    fModified := true;
  End
  Else Begin
    fCtx.UnlockSeed(n.Area);
    MarkDirty(n);
  End;
  UpdateAreaHead;
End;

(* ------------------------------------------------------------------------ *)
(* Settings of the set                                                      *)

Procedure TForm1.EditNameChange(Sender: TObject);
Begin
  If fUpdating Then exit;
  fCtx.Name := EditName.Text;
  fModified := true;
End;

Procedure TForm1.ComboStyleChange(Sender: TObject);
Begin
  If fUpdating Or (ComboStyle.ItemIndex < 0) Then exit;
  fCtx.Style := TAGStyle(ComboStyle.ItemIndex);
  MarkAllDirty;
End;

Procedure TForm1.EditSeedEditingDone(Sender: TObject);
Var
  v: QWord;
Begin
  If fUpdating Then exit;
  If TryStrToQWord(Trim(EditSeed.Text), v) Then Begin
    If v <> fCtx.Seed Then Begin
      fCtx.Seed := v;
      UpdateAreaHead;
      MarkAllDirty;
    End;
  End
  Else
    EditSeed.Text := IntToStr(fCtx.Seed);
End;

Procedure TForm1.ButtonSeedDiceClick(Sender: TObject);
Begin
  fCtx.Seed := NewRandomSeed;
  fUpdating := true;
  EditSeed.Text := IntToStr(fCtx.Seed);
  fUpdating := false;
  UpdateAreaHead;
  MarkAllDirty;
End;

Procedure TForm1.CheckPerPlayerChange(Sender: TObject);
Begin
  If fUpdating Then exit;
  fCtx.PerPlayer := CheckPerPlayer.Checked;
  fModified := true;
  BuildTree;
  Schedule;
End;

(* ------------------------------------------------------------------------ *)
(* Save / close                                                             *)

Procedure TForm1.SetStatus(Const s: String);
Begin
  LabelStatus.Caption := s;
End;

Procedure TForm1.EnableEditing(Value: Boolean);
Begin
  PanelTop.Enabled := Value;
  PanelRight.Enabled := Value;
  TreeView1.Enabled := Value;
  ButtonSave.Enabled := Value;
  ButtonSaveAs.Enabled := Value;
  ProgressBar1.Visible := Not Value;
  If Value Then
    ButtonClose.Caption := R_Close
  Else
    ButtonClose.Caption := R_Cancel;
End;

Procedure TForm1.ButtonSaveAsClick(Sender: TObject);
Var
  aName, folder: String;
Begin
  aName := fCtx.Name;
  If Not InputQuery(R_SaveAsTitle, R_SaveAsPrompt, aName) Then Begin
    fCloseAfterSave := false;
    exit;
  End;
  aName := Trim(aName);
  If aName = '' Then aName := R_DefaultName;
  folder := FolderFromName(aName);
  If (folder <> fFolder) And FileExists(SetDir(folder) + 'profile.ini') Then
    If MessageDlg(format(RF_FolderExists, [folder]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes Then Begin
      fCloseAfterSave := false;
      exit;
    End;
  fCtx.Name := aName;
  fUpdating := true;
  EditName.Text := aName;
  fUpdating := false;
  StartSave(folder);
End;

Procedure TForm1.ButtonSaveClick(Sender: TObject);
Begin
  If fFolder = '' Then
    ButtonSaveAsClick(Sender)
  Else
    StartSave(fFolder);
End;

Procedure TForm1.StartSave(Const Folder: String);
Begin
  If assigned(fSaver) Then exit;
  If assigned(fJob) Then fJob.Cancel; // the area is rendered again after saving
  If assigned(fJobNode) Then fJobNode.Dirty := true;
  fSaveFolder := Folder;
  EnableEditing(false);
  ProgressBar1.Position := 0;
  SetStatus(format(RF_Generating, [0, 0]));
  fSaver := TAGSetWorker.Create(fCtx, SetDir(Folder), @SaveDone, @SaveProgress);
  fSaver.Start;
End;

Procedure TForm1.SaveProgress(Sender: TObject);
Begin
  If Sender <> fSaver Then exit;
  ProgressBar1.Max := Max(1, fSaver.Total);
  ProgressBar1.Position := fSaver.Done;
  SetStatus(format(RF_Generating, [fSaver.Done, fSaver.Total]));
End;

Procedure TForm1.SaveDone(Sender: TObject);
Var
  w: TAGSetWorker;
  ok: Boolean;
Begin
  If Sender <> fSaver Then exit;
  w := fSaver;
  fSaver := Nil;
  fDead.Add(w);
  ok := (w.Error = '') And Not w.Aborted;
  EnableEditing(true);
  If ok Then Begin
    fFolder := fSaveFolder;
    WriteLastSet(fFolder);
    GuiWritten := true;
    fModified := false;
    SetStatus(format(RF_Saved, [fFolder]));
  End
  Else If w.Error <> '' Then Begin
    SetStatus(format(RF_Error, [w.Error]));
    MessageDlg(format(RF_Error, [w.Error]), mtError, [mbOK], 0);
  End
  Else
    SetStatus(R_Cancelled);
  If fCloseAfterSave Then Begin
    fCloseAfterSave := false;
    If ok Then Begin
      Close;
      exit;
    End;
  End;
  Schedule;
End;

Procedure TForm1.ButtonCloseClick(Sender: TObject);
Begin
  If assigned(fSaver) Then
    fSaver.Cancel
  Else
    Close;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If assigned(fSaver) Then Begin
    fSaver.Cancel;
    CanClose := false;
    exit;
  End;
  If fModified Then Begin
    Case MessageDlg(R_SaveChanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0) Of
      mrYes: Begin
          CanClose := false;
          fCloseAfterSave := true;
          ButtonSaveClick(Nil);
        End;
      mrNo: CanClose := true;
    Else
      CanClose := false;
    End;
  End;
End;

End.
