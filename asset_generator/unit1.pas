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
 *
 * Left of the images an animated preview like the engine plays it: figures
 * (all animations of the sheets and of die/, idle/, locked_in/), bombs and a
 * growing explosion. The scene of a map shows a ticking bomb, an explosion
 * and the figure, all in the selected player color.
 *)
Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, uag_context, uag_output, uag_image, uag_preview, uag_worker;

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

  TAnimKind = (anSheet, anExplosionGrow, anExplosion);

  { TAnimDef: one entry of the animation combo box }

  TAnimDef = Record
    Caption: String;
    Kind: TAnimKind;
    Path: String; // image of the area output
    Tpf, Fpr, W, H, Count: Integer; // like the engine: ms per frame, frames per row, cell size, frames per direction
    AX, AY: Integer; // point of the cell that is drawn at the anchor of the paint box
    Ranges: Array[0..3] Of Integer; // first frame of right, up, left, down
    Directional: Boolean;
    Pause: Integer; // ms without an image at the end of the loop (die)
    Zoom, BaseY: Integer; // zoom of the preview, anchor y in the paint box
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
    ComboAnim: TComboBox;
    ComboColor: TComboBox;
    ComboDir: TComboBox;
    ComboImage: TComboBox;
    ComboStyle: TComboBox;
    ComboZoom: TComboBox;
    EditName: TEdit;
    EditSeed: TEdit;
    LabelAnim: TLabel;
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
    ScrollBoxAnim: TScrollBox;
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
    Procedure ComboAnimChange(Sender: TObject);
    Procedure ComboColorChange(Sender: TObject);
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
    fAnimStart, fAnimT: QWord; // ms
    fAnimKey, fSceneKey: Int64; // last drawn frame
    fAnimDefs: Array Of TAnimDef;
    fAnimDef: TAnimDef; // the selected one
    fAnimSheet: TAGImage; // explosion: recolored flame sheet
    fAnimGround: TAGImage; // explosion: ground of a map, Nil = none
    fSceneStatic: TAGImage; // map scene with the figure, Nil = no animated scene shown
    fSceneBomb, fSceneFlame: TAGImage; // recolored sheets of the scene
    fSceneZoom: Integer;
    fAutoGold: Boolean; // the color was switched to gold by selecting the hunted figure
    fPrevColor: Integer;
    fShownNode: TAreaNode; // node of the parameter rows
    fShownRendered: Boolean; // the rows were built after the node was rendered
    fRowLabels: Array Of TLabel;
    fRowBars: Array Of TTrackBar; // Nil for a check box
    fRowChecks: Array Of TCheckBox; // Nil for a slider
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
    Procedure FillAnimCombo(Node: TAreaNode);
    Function SelectedColor: TAGPlayerColor;
    Function Recolored(Img: TAGImage): TAGImage; // clone in the selected color, Nil stays Nil
    Function AnimKey(t: QWord): Int64;
    Procedure UpdateAnim;
    Procedure DrawSceneFrame(t: QWord);
    Procedure DrawAnim(C: TCanvas; W, H: Integer);
    Procedure ScreenshotExtra(Dst: TCanvas);
    Procedure FreeScene;
    Procedure UpdateView;
    Procedure UpdateAreaHead;
    Procedure BuildParamRows(Node: TAreaNode);
    Procedure UpdateParamRows(Node: TAreaNode; Initial: Boolean);
    Function ParamText(Const p: TAGParamInfo): String;
    Procedure ParamBarChange(Sender: TObject);
    Procedure ParamCheckChange(Sender: TObject);
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

Uses Math, uag_math, uag_random, uag_gen_world, uag_gui, ulanguage;

Const
  RowHeight = 62;
  VarySteps = 1000;
  ChanceOn = 0; // override of a Chance parameter, see TAGArea.Chance
  ChanceOff = 999999;
  // flame.png: 5 x 9 cells 41 x 37, first frames of the elements (see client/uatomic.pas)
  FlameCross = 0;
  FlameEndUp = 5;
  FlameEndLeft = 10;
  FlameEndDown = 15;
  FlameEndRight = 20;
  FlameMidDown = 25;
  FlameMidUp = 30;
  FlameMidLeft = 35;
  FlameMidRight = 40;
  FlameTpf = 50;
  BombTpf = 50;
  BombFrames = 18;

(* Draws cell Index (Fpr cells per row, W x H) of the keyed Sheet with its top left corner at x, y of Dst *)
Procedure DrawCell(Dst, Sheet: TAGImage; Index, Fpr, W, H, x, y: Integer);
Var
  i, j, sx, sy: Integer;
  c: TAGColor;
Begin
  If (Sheet = Nil) Or (Index < 0) Then exit;
  sx := (Index Mod Fpr) * W;
  sy := (Index Div Fpr) * H;
  If (sx + W > Sheet.Width) Or (sy + H > Sheet.Height) Then exit;
  For j := 0 To H - 1 Do
    For i := 0 To W - 1 Do Begin
      c := Sheet.Pixels[(sy + j) * Sheet.Width + sx + i];
      If (c.a < 128) Or AGSameColor(c, AGFuchsia) Then Continue;
      Dst.SetPixel(x + i, y + j, AGWithAlpha(c, 255));
    End;
End;

(*
 * Explosion with its center in cell (cx, cy) of the play field and flames of
 * Len cells, flame frame Frame (0..4). OX, OY = top left of cell (0, 0) in
 * Dst. Like TAtomicField.Render: every cell at its top left corner, the cells
 * column by column.
 *)
Procedure DrawExplosion(Dst, Sheet: TAGImage; OX, OY, cx, cy, Len, Frame: Integer);
Var
  i, j, d, e: Integer;
Begin
  If Len < 0 Then exit;
  For i := cx - Len To cx + Len Do
    For j := cy - Len To cy + Len Do Begin
      If (i <> cx) And (j <> cy) Then Continue;
      If (i = cx) And (j = cy) Then
        e := FlameCross
      Else If j = cy Then Begin
        d := i - cx;
        If d < 0 Then Begin
          If -d = Len Then e := FlameEndLeft Else e := FlameMidLeft;
        End
        Else If d = Len Then e := FlameEndRight Else e := FlameMidRight;
      End
      Else Begin
        d := j - cy;
        If d < 0 Then Begin
          If -d = Len Then e := FlameEndUp Else e := FlameMidUp;
        End
        Else If d = Len Then e := FlameEndDown Else e := FlameMidDown;
      End;
      DrawCell(Dst, Sheet, e + Frame, 5, 41, 37, OX + i * SceneCW, OY + j * SceneCH);
    End;
End;

Function FigureDir(Node: TAreaNode): String;
Begin
  Case Node.Kind Of
    akPlayer: result := 'atomic/p' + IntToStr(Node.Index) + '/';
    akHunted: result := 'atomic/hunted/';
  Else
    result := 'atomic/';
  End;
End;

Function SheetDef(Const Caption, Path: String; Tpf, Fpr, W, H, Count, AX, AY: Integer; Const Ranges: Array Of Integer): TAnimDef;
Var
  i: Integer;
Begin
  result := Default(TAnimDef);
  result.Caption := Caption;
  result.Kind := anSheet;
  result.Path := Path;
  result.Tpf := Tpf;
  result.Fpr := Fpr;
  result.W := W;
  result.H := H;
  result.Count := Count;
  result.AX := AX;
  result.AY := AY;
  For i := 0 To 3 Do
    result.Ranges[i] := Ranges[Min(i, high(Ranges))];
  result.Directional := length(Ranges) > 1;
  result.Zoom := 2;
  result.BaseY := 16 + 90 * 2;
End;

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
  result := Kind In [akFigure, akPlayer, akHunted];
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
  fPrevColor := 2;
  fAnimStart := GetTickCount64;
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
  FreeScene;
  fAnimSheet.Free;
  fAnimGround.Free;
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
  Add(akHunted, 0, R_AreaHunted); // last, so the numbers of --area stay; shown below the figure
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
        akHunted: t := TreeView1.Items.AddChild(fig, n.Caption);
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

  Procedure Mark(n: TAreaNode);
  Begin
    n.Dirty := true;
    If fJobNode = n Then fJob.Cancel;
  End;

Begin
  Mark(Node);
  // the hunted figure is the figure with a crown (same area)
  If Node.Kind = akFigure Then Mark(FindNode(akHunted, 0));
  If Node.Kind = akHunted Then Mark(FindNode(akFigure, 0));
  If Node.Kind In [akFigure, akHunted, akBomb, akMap] Then Mark(FindNode(akPreview, 0));
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
  Else If assigned(sel) And (sel.Kind = akMap) And FindNode(akBomb, 0).Dirty Then
    n := FindNode(akBomb, 0) // the scene shows a bomb, an explosion and the figure
  Else If assigned(sel) And (sel.Kind = akMap) And FindNode(akFlame, 0).Dirty Then
    n := FindNode(akFlame, 0)
  Else If assigned(sel) And (sel.Kind = akMap) And FindNode(akFigure, 0).Dirty Then
    n := FindNode(akFigure, 0)
  Else If assigned(sel) And (sel.Kind = akFlame) And FindNode(akMap, 0).Dirty Then
    n := FindNode(akMap, 0) // ground below the explosion
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
            If (length(fRowBars) > 0) And assigned(fRowBars[0]) Then fRowBars[0].Position := fRowBars[0].Max;
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
      SaveFormScreenshot(self, GuiScreenshot, @ScreenshotExtra);
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
    If n = SelectedNode Then
      ShowNode(false)
    Else If assigned(SelectedNode) And (SelectedNode.Kind In [akMap, akFlame]) And (n.Kind In [akFigure, akBomb, akFlame, akMap]) Then
      UpdateView; // the scene or the explosion use the images of other areas
  End;
  If Not TimerDebounce.Enabled Then Schedule;
End;

(* ------------------------------------------------------------------------ *)
(* View                                                                     *)

Procedure TForm1.ShowNode(NewSelection: Boolean);
Var
  n: TAreaNode;
  anim: Boolean;
Begin
  n := SelectedNode;
  If n = Nil Then exit;
  If NewSelection Then Begin
    // the hunted figure is shown in gold like in the game, the other ones in the color chosen before
    If (n.Kind = akHunted) And (ComboColor.ItemIndex <> 10) Then Begin
      fPrevColor := ComboColor.ItemIndex;
      ComboColor.ItemIndex := 10;
      fAutoGold := true;
    End
    Else If (n.Kind <> akHunted) And fAutoGold Then Begin
      If ComboColor.ItemIndex = 10 Then ComboColor.ItemIndex := Max(0, fPrevColor);
      fAutoGold := false;
    End;
  End;
  anim := n.IsFigure Or (n.Kind In [akBomb, akFlame]);
  PanelAnim.Visible := anim;
  LabelColor.Visible := anim Or (n.Kind = akMap);
  ComboColor.Visible := LabelColor.Visible;
  LabelDir.Visible := n.IsFigure;
  ComboDir.Visible := n.IsFigure;
  If n.IsFigure Then
    ScrollBoxAnim.SetBounds(0, ComboDir.Top + ComboDir.Height + 9, PanelAnim.ClientWidth, PanelAnim.ClientHeight - (ComboDir.Top + ComboDir.Height + 9))
  Else
    ScrollBoxAnim.SetBounds(0, ComboAnim.Top + ComboAnim.Height + 9, PanelAnim.ClientWidth, PanelAnim.ClientHeight - (ComboAnim.Top + ComboAnim.Height + 9));
  FillImageCombo(n);
  FillAnimCombo(n);
  UpdateView;
  UpdateAreaHead;
  If NewSelection Or (n <> fShownNode) Or (length(fRowLabels) <> length(n.Params)) Or (fShownRendered <> assigned(n.Output)) Then
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
    akFigure, akPlayer, akHunted: def := FigureDir(Node) + 'walk.png';
    akBomb: def := 'atomic/bomb.png';
    akFlame: def := 'atomic/flame.png';
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

Procedure TForm1.FillAnimCombo(Node: TAreaNode);
Var
  old, dir: String;

  Procedure Add(Const d: TAnimDef);
  Begin
    If (d.Kind = anSheet) And ((Node.Output = Nil) Or (Node.Output.FindImage(d.Path) = Nil)) Then exit;
    setlength(fAnimDefs, length(fAnimDefs) + 1);
    fAnimDefs[high(fAnimDefs)] := d;
  End;

  (* The clips of die/, idle/, locked_in/ as listed in their files.txt *)
  Procedure AddClips(Const Sub, Fmt: String; Pause: Integer);
  Var
    sl: TStringList;
    sa: TStringArray;
    k: Integer;
    d: TAnimDef;
  Begin
    If Node.Output = Nil Then exit;
    sl := TStringList.Create;
    Try
      sl.Text := Node.Output.FindText(dir + Sub + '/files.txt');
      For k := 0 To sl.Count - 1 Do Begin
        If (Trim(sl[k]) = '') Or (pos('#', Trim(sl[k])) = 1) Then Continue;
        // Filename; TimePerFrame; FramesPerRow; FramesPerCol; Width; Height; Count; OffsetX; OffsetY
        sa := sl[k].Split(';');
        If length(sa) <> 9 Then Continue;
        d := SheetDef(format(Fmt, [ChangeFileExt(Trim(sa[0]), '')]), dir + Sub + '/' + Trim(sa[0]),
          StrToIntDef(Trim(sa[1]), 50), Max(1, StrToIntDef(Trim(sa[2]), 1)), StrToIntDef(Trim(sa[4]), 110), StrToIntDef(Trim(sa[5]), 110),
          Max(1, StrToIntDef(Trim(sa[6]), 1)), StrToIntDef(Trim(sa[7]), 55), StrToIntDef(Trim(sa[8]), 90), [0]);
        d.Pause := Pause;
        Add(d);
      End;
    Finally
      sl.Free;
    End;
  End;

Var
  d: TAnimDef;
  i, idx: Integer;
Begin
  old := '';
  If (ComboAnim.ItemIndex >= 0) And (fShownNode = Node) Then old := ComboAnim.Items[ComboAnim.ItemIndex];
  fAnimDefs := Nil;
  If Node.IsFigure Then Begin
    // like TAtomic.InitAsColor in client/uatomic.pas, ranges right, up, left, down
    dir := FigureDir(Node);
    Add(SheetDef(R_AnimWalk, dir + 'walk.png', 50, 15, 110, 110, 15, 55, 90, [0, 15, 45, 30]));
    Add(SheetDef(R_AnimCarryWalk, dir + 'bwalk.png', 50, 15, 110, 110, 15, 55, 90, [0, 15, 45, 30]));
    Add(SheetDef(R_AnimStand, dir + 'stand.png', 100, 2, 110, 110, 1, 55, 90, [0, 1, 3, 2]));
    Add(SheetDef(R_AnimKick, dir + 'kick.png', 50, 10, 50, 69, 10, 25, 60, [10, 0, 30, 20]));
    Add(SheetDef(R_AnimPunch, dir + 'punbomb.png', 50, 10, 110, 110, 10, 55, 90, [30, 10, 20, 0]));
    Add(SheetDef(R_AnimPickUp, dir + 'pupbomb.png', 50, 11, 110, 110, 11, 55, 90, [11, 33, 0, 22]));
    AddClips('die', RF_AnimDie, 600);
    AddClips('idle', RF_AnimIdle, 0);
    AddClips('locked_in', RF_AnimLockedIn, 0);
  End
  Else If Node.Kind = akBomb Then Begin
    // the anchor is the center of the cell (engine: offset -20, -37 div 2 or -20)
    For i := 0 To 3 Do Begin
      Case i Of
        0: d := SheetDef(R_AnimBomb, 'atomic/bomb.png', BombTpf, 5, 40, 37, BombFrames, 20, 18, [0]);
        1: d := SheetDef(R_AnimBombTrigger, 'atomic/bomb_trigger.png', 50, 5, 40, 40, 18, 20, 20, [0]);
        2: d := SheetDef(R_AnimBombWobble, 'atomic/bomb_wobble.png', 75, 4, 40, 37, 12, 20, 18, [0]);
      Else
        d := SheetDef(R_AnimBombDud, 'atomic/bomb_dud.png', 750, 1, 40, 40, 2, 20, 20, [0]);
      End;
      d.Zoom := 4;
      d.BaseY := 16 + 20 * 4;
      Add(d);
    End;
  End
  Else If Node.Kind = akFlame Then Begin
    d := Default(TAnimDef);
    d.Caption := R_AnimExplosionGrow;
    d.Kind := anExplosionGrow;
    d.Path := 'atomic/flame.png';
    d.Zoom := 1;
    Add(d);
    d.Caption := R_AnimExplosion;
    d.Kind := anExplosion;
    Add(d);
  End;
  ComboAnim.Items.BeginUpdate;
  Try
    ComboAnim.Items.Clear;
    For i := 0 To high(fAnimDefs) Do
      ComboAnim.Items.Add(fAnimDefs[i].Caption);
  Finally
    ComboAnim.Items.EndUpdate;
  End;
  idx := ComboAnim.Items.IndexOf(old);
  If (idx < 0) And (GuiAnim >= 0) And (GuiAnim < ComboAnim.Items.Count) Then idx := GuiAnim; // --anim
  If (idx < 0) And (ComboAnim.Items.Count > 0) Then idx := 0;
  ComboAnim.ItemIndex := idx;
  If idx >= 0 Then
    fAnimDef := fAnimDefs[idx]
  Else
    fAnimDef := Default(TAnimDef);
  If old <> ComboAnim.Text Then fAnimStart := GetTickCount64;
End;

Function TForm1.SelectedColor: TAGPlayerColor;
Begin
  If ComboColor.ItemIndex = 10 Then
    result := AGHuntedGold
  Else
    result := AGPlayerColors[EnsureRange(ComboColor.ItemIndex, 0, 9)];
End;

Function TForm1.Recolored(Img: TAGImage): TAGImage;
Begin
  If Img = Nil Then exit(Nil);
  result := Img.Clone;
  RecolorPlayer(result, SelectedColor);
End;

Procedure TForm1.FreeScene;
Begin
  FreeAndNil(fSceneStatic);
  FreeAndNil(fSceneBomb);
  FreeAndNil(fSceneFlame);
End;

Procedure TForm1.UpdateView;
Var
  n, other: TAreaNode;
  img, stand, shadow: TAGImage;
  zoom, fx, fy: Integer;
  b: TBitmap;
Begin
  n := SelectedNode;
  img := Nil;
  FreeScene;
  If assigned(n) And assigned(n.Output) And (ComboImage.ItemIndex >= 0) Then
    img := TAGImage(n.Output.Images.Objects[ComboImage.ItemIndex]);
  zoom := ComboZoom.ItemIndex + 1;
  If assigned(img) And (n.Kind = akMap) And (n.Output.Images[ComboImage.ItemIndex] = 'scene') Then Begin
    // the scene: the figure stands still, bomb and explosion are animated (DrawSceneFrame)
    fSceneStatic := img.Clone;
    other := FindNode(akFigure, 0);
    If assigned(other.Output) Then Begin
      // like TAtomic.Render: position = center of the cell, shadow and figure 0.25 cells lower
      fx := SceneFX + Round((SceneFigureX + 0.5) * SceneCW);
      fy := SceneFY + Round((SceneFigureY + 0.75) * SceneCH);
      shadow := other.Output.FindImage('atomic/shadow.png');
      If assigned(shadow) Then fSceneStatic.Draw(shadow, fx - shadow.Width Div 2, fy - shadow.Height Div 2);
      stand := Recolored(other.Output.FindImage('atomic/stand.png'));
      DrawCell(fSceneStatic, stand, 2, 2, 110, 110, fx - 55, fy - 90); // frame "down"
      stand.Free;
    End;
    other := FindNode(akBomb, 0);
    If assigned(other.Output) Then fSceneBomb := Recolored(other.Output.FindImage('atomic/bomb.png'));
    other := FindNode(akFlame, 0);
    If assigned(other.Output) Then fSceneFlame := Recolored(other.Output.FindImage('atomic/flame.png'));
    fSceneZoom := zoom;
    b := ImageToBitmap(fSceneStatic, zoom);
    fViewBmp.Free;
    fViewBmp := b;
    fSceneKey := -1;
    DrawSceneFrame(GetTickCount64 - fAnimStart);
  End
  Else If assigned(img) Then Begin
    // figures, bombs and flames are recolored like the engine does
    If n.IsFigure Or (n.Kind In [akBomb, akFlame]) Then Begin
      img := Recolored(img);
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
  UpdateAnim;
End;

(* The sheet of the selected animation in the player color *)
Procedure TForm1.UpdateAnim;
Var
  n, m: TAreaNode;
  sheet, field: TAGImage;
  b: TBitmap;
Begin
  n := SelectedNode;
  FreeAndNil(fAnimSheet);
  FreeAndNil(fAnimGround);
  fAnimBmp.SetSize(0, 0);
  If assigned(n) And assigned(n.Output) And PanelAnim.Visible And (fAnimDef.Path <> '') Then Begin
    sheet := Recolored(n.Output.FindImage(fAnimDef.Path));
    If assigned(sheet) Then Begin
      If fAnimDef.Kind = anSheet Then Begin
        b := ImageToBitmap(sheet, fAnimDef.Zoom);
        fAnimBmp.Free;
        fAnimBmp := b;
        sheet.Free;
      End
      Else Begin
        fAnimSheet := sheet;
        // the ground of the first map, if it is rendered already
        m := FindNode(akMap, 0);
        field := Nil;
        If assigned(m.Output) Then field := m.Output.FindImage(format('maps/Field%.2d/field.png', [m.Index]));
        If assigned(field) Then
          fAnimGround := field.Crop(SceneFX + (SceneFlameX - 2) * SceneCW, SceneFY + (SceneFlameY - 2) * SceneCH, 5 * SceneCW + 1, 5 * SceneCH + 1);
      End;
    End;
  End;
  fAnimKey := -1;
  PaintBoxAnim.Invalidate;
End;

(* Bomb and explosion of the map scene, only their parts of fViewBmp are drawn again *)
Procedure TForm1.DrawSceneFrame(t: QWord);
Var
  k, rx, ry, rw, rh: Integer;
  part: TAGImage;
  b: TBitmap;
Begin
  If fSceneStatic = Nil Then exit;
  For k := 0 To 1 Do Begin
    If k = 0 Then Begin
      // bomb: engine x = Fieldxoff + (x + 0.5) * 40 - 20, y = FieldyOff + (y + 0.5) * 36 - 37 div 2
      rx := SceneFX + SceneBombX * SceneCW;
      ry := SceneFY + SceneBombY * SceneCH;
      rw := 40;
      rh := 37;
    End
    Else Begin
      rx := SceneFX + (SceneFlameX - SceneFlameLen) * SceneCW;
      ry := SceneFY + (SceneFlameY - SceneFlameLen) * SceneCH;
      rw := (2 * SceneFlameLen + 1) * SceneCW + 1;
      rh := (2 * SceneFlameLen + 1) * SceneCH + 1;
    End;
    part := fSceneStatic.Crop(rx, ry, rw, rh);
    Try
      If k = 0 Then
        DrawCell(part, fSceneBomb, (t Div BombTpf) Mod BombFrames, 5, 40, 37, 0, 0)
      Else
        DrawExplosion(part, fSceneFlame, SceneFX - rx, SceneFY - ry, SceneFlameX, SceneFlameY, SceneFlameLen, (t Div FlameTpf) Mod 5);
      b := ImageToBitmap(part, fSceneZoom);
      Try
        fViewBmp.Canvas.Draw(rx * fSceneZoom, ry * fSceneZoom, b);
      Finally
        b.Free;
      End;
    Finally
      part.Free;
    End;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Begin
  If fViewBmp.Width > 0 Then
    PaintBox1.Canvas.Draw(0, 0, fViewBmp);
End;

(* The frame that is shown at time t (-2 = pause), a change = repaint *)
Function TForm1.AnimKey(t: QWord): Int64;
Var
  d: TAnimDef;
  loop: QWord;
Begin
  d := fAnimDef;
  If d.Kind = anSheet Then Begin
    If (d.Tpf <= 0) Or (d.Count <= 0) Then exit(-1);
    loop := d.Count * d.Tpf + d.Pause;
    t := t Mod loop;
    If t >= QWord(d.Count * d.Tpf) Then
      result := -2
    Else
      result := t Div QWord(d.Tpf);
  End
  Else
    result := t Div FlameTpf;
End;

Procedure TForm1.PaintBoxAnimPaint(Sender: TObject);
Begin
  DrawAnim(PaintBoxAnim.Canvas, PaintBoxAnim.Width, PaintBoxAnim.Height);
End;

(* --screenshot without the focus: the paint boxes are missing *)
Procedure TForm1.ScreenshotExtra(Dst: TCanvas);
Var
  b: TBitmap;
  p: TPoint;
Begin
  If Not PanelAnim.Visible Then exit;
  b := TBitmap.Create;
  Try
    b.SetSize(PaintBoxAnim.Width, PaintBoxAnim.Height);
    DrawAnim(b.Canvas, b.Width, b.Height);
    p := ScreenToClient(PaintBoxAnim.ClientToScreen(Point(0, 0)));
    Dst.Draw(p.x, p.y, b);
  Finally
    b.Free;
  End;
End;

Procedure TForm1.DrawAnim(C: TCanvas; W, H: Integer);
Const
  // ComboDir: down, left, up, right -> ranges right, up, left, down
  DirRange: Array[0..3] Of Integer = (3, 2, 1, 0);
  // explosion growing: length per step of 5 flame frames, -1 = nothing
  GrowLen: Array[0..7] Of Integer = (0, 1, 2, 2, 2, 2, -1, -1);
Var
  d: TAnimDef;
  f, idx, z, x, y, len: Integer;
  src, dst: TRect;
  img: TAGImage;
  b: TBitmap;
Begin
  C.Brush.Color := PaintBoxAnim.Color;
  C.FillRect(0, 0, W, H);
  d := fAnimDef;
  If d.Kind = anSheet Then Begin
    If fAnimBmp.Width = 0 Then exit;
    f := AnimKey(fAnimT);
    If f < 0 Then exit;
    If d.Directional Then
      idx := f + d.Ranges[DirRange[EnsureRange(ComboDir.ItemIndex, 0, 3)]]
    Else
      idx := f + d.Ranges[0];
    z := d.Zoom;
    src.Left := (idx Mod d.Fpr) * d.W * z;
    src.Top := (idx Div d.Fpr) * d.H * z;
    src.Right := src.Left + d.W * z;
    src.Bottom := src.Top + d.H * z;
    If (src.Right > fAnimBmp.Width) Or (src.Bottom > fAnimBmp.Height) Then exit;
    x := W Div 2 - d.AX * z;
    y := d.BaseY - d.AY * z;
    dst := Rect(x, y, x + d.W * z, y + d.H * z);
    C.CopyRect(dst, fAnimBmp.Canvas, src);
  End
  Else Begin
    If fAnimSheet = Nil Then exit;
    If d.Kind = anExplosion Then
      len := 2
    Else
      len := GrowLen[(fAnimT Div (5 * FlameTpf)) Mod length(GrowLen)];
    If assigned(fAnimGround) Then
      img := fAnimGround.Clone
    Else Begin
      img := TAGImage.Create(5 * SceneCW + 1, 5 * SceneCH + 1);
      img.Clear(AGRGB(40, 40, 46));
    End;
    Try
      DrawExplosion(img, fAnimSheet, 0, 0, 2, 2, len, (fAnimT Div FlameTpf) Mod 5);
      b := ImageToBitmap(img, 1);
      Try
        C.Draw((W - b.Width) Div 2, 16, b);
      Finally
        b.Free;
      End;
    Finally
      img.Free;
    End;
  End;
End;

Procedure TForm1.TimerAnimTimer(Sender: TObject);
Var
  t: QWord;
  k: Int64;
Begin
  t := GetTickCount64 - fAnimStart;
  fAnimT := t;
  If PanelAnim.Visible Then Begin
    k := AnimKey(t);
    If k <> fAnimKey Then Begin
      fAnimKey := k;
      PaintBoxAnim.Invalidate;
    End;
  End;
  If assigned(fSceneStatic) Then Begin
    k := t Div FlameTpf;
    If k <> fSceneKey Then Begin
      fSceneKey := k;
      DrawSceneFrame(t);
    End;
  End;
End;

Procedure TForm1.ComboImageChange(Sender: TObject);
Begin
  UpdateView;
End;

Procedure TForm1.ComboColorChange(Sender: TObject);
Begin
  fAutoGold := false;
  UpdateView;
End;

Procedure TForm1.ComboAnimChange(Sender: TObject);
Var
  i: Integer;
Begin
  If (ComboAnim.ItemIndex >= 0) And (ComboAnim.ItemIndex <= high(fAnimDefs)) Then
    fAnimDef := fAnimDefs[ComboAnim.ItemIndex];
  fAnimStart := GetTickCount64;
  // the sheet of the animation is shown too
  i := ComboImage.Items.IndexOf(fAnimDef.Path);
  If (i >= 0) And (i <> ComboImage.ItemIndex) Then Begin
    ComboImage.ItemIndex := i;
    UpdateView;
  End
  Else
    UpdateAnim;
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

Function ChanceIsOn(Const p: TAGParamInfo): Boolean;
Begin
  result := AGRound(p.Value) < AGRound(p.Probability * 1000000); // like TAGArea.Chance
End;

Function TForm1.ParamText(Const p: TAGParamInfo): String;
Var
  s: String;
Begin
  If p.IsChance Then Begin
    s := p.Name;
    If (s <> '') And (s[length(s)] = '?') Then delete(s, length(s), 1);
    If ChanceIsOn(p) Then
      result := format('%s = %s', [s, R_On])
    Else
      result := format('%s = %s', [s, R_Off]);
  End
  Else If p.IsPick Then
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
  c: TCheckBox;
Begin
  fShownNode := Node;
  fShownRendered := assigned(Node.Output);
  ScrollBoxParams.DisableAutoSizing;
  Try
    For i := ScrollBoxParams.ControlCount - 1 Downto 0 Do
      ScrollBoxParams.Controls[i].Free;
    fRowLabels := Nil;
    fRowBars := Nil;
    fRowChecks := Nil;
    setlength(fRowLabels, length(Node.Params));
    setlength(fRowBars, length(Node.Params));
    setlength(fRowChecks, length(Node.Params));
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
      If Node.Params[i].IsChance Then Begin
        // on / off
        c := TCheckBox.Create(ScrollBoxParams);
        c.Parent := ScrollBoxParams;
        c.SetBounds(8, 30 + i * RowHeight, w - 8, 22);
        c.Caption := R_On;
        c.Tag := i;
        c.OnChange := @ParamCheckChange;
        fRowChecks[i] := c;
        fRowBars[i] := Nil;
        Continue;
      End;
      fRowChecks[i] := Nil;
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
      If assigned(fRowChecks[i]) Then Begin
        If Initial Or Not p.Overridden Then fRowChecks[i].Checked := ChanceIsOn(p);
        Continue;
      End;
      If fRowBars[i] = Nil Then Continue;
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

Procedure TForm1.ParamCheckChange(Sender: TObject);
Var
  n: TAreaNode;
  i: Integer;
  v: Double;
Begin
  If fUpdating Then exit;
  n := fShownNode;
  i := TComponent(Sender).Tag;
  If (n = Nil) Or (i > high(n.Params)) Or Not n.Params[i].IsChance Then exit;
  If TCheckBox(Sender).Checked Then v := ChanceOn Else v := ChanceOff;
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
  aName, folder, msg: String;
Begin
  aName := fCtx.Name;
  If Not InputQuery(R_SaveAsTitle, R_SaveAsPrompt, aName) Then Begin
    fCloseAfterSave := false;
    exit;
  End;
  aName := Trim(aName);
  If aName = '' Then aName := R_DefaultName;
  folder := FolderFromName(aName);
  // the folder is replaced completely by the new set
  If (folder <> fFolder) And DirectoryExists(SetDir(folder)) Then Begin
    If FileExists(SetDir(folder) + 'profile.ini') Then
      msg := format(RF_FolderExists, [folder])
    Else
      msg := format(RF_FolderReplace, [folder]);
    If MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes Then Begin
      fCloseAfterSave := false;
      exit;
    End;
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
  fSaver.Output.CancelAfter := GuiCancelAfter;
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
  Else If w.InUse Then Begin
    SetStatus(format(RF_SetInUse, [fSaveFolder]));
    If GuiTestSave = '' Then MessageDlg(format(RF_SetInUse, [fSaveFolder]), mtError, [mbOK], 0);
  End
  Else If w.Error <> '' Then Begin
    SetStatus(format(RF_Error, [w.Error]));
    If GuiTestSave = '' Then MessageDlg(format(RF_Error, [w.Error]), mtError, [mbOK], 0);
  End
  Else
    SetStatus(R_Cancelled);
  If fCloseAfterSave Then Begin
    fCloseAfterSave := false;
    If ok Or (GuiTestSave <> '') Then Begin
      fModified := false; // --test-save: also ends after a failed save (exit code 1)
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
