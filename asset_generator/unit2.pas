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
 * Style picker (--auto --set <folder>): first start without CD data. Shows
 * the key art of one random seed in the three styles, a click on a style
 * generates the complete set into assets/<folder>/ and ends the program.
 * The set is saved atomically (uag_worker): on cancel or error nothing is
 * left behind, a new folder is not created and an existing set is unchanged.
 *)
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, uag_context, uag_worker;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    ButtonCancel: TButton;
    ButtonRoll: TButton;
    ButtonStop: TButton;
    ImageFlat: TImage;
    ImagePixel: TImage;
    ImageRetro: TImage;
    ImageSoft: TImage;
    LabelFlat: TLabel;
    LabelHint: TLabel;
    LabelPixel: TLabel;
    LabelProgress: TLabel;
    LabelRetro: TLabel;
    LabelSoft: TLabel;
    LabelTitle: TLabel;
    PanelFlat: TPanel;
    PanelPixel: TPanel;
    PanelProgress: TPanel;
    PanelRetro: TPanel;
    PanelSoft: TPanel;
    ProgressBar1: TProgressBar;
    Procedure ButtonCancelClick(Sender: TObject);
    Procedure ButtonRollClick(Sender: TObject);
    Procedure ButtonStopClick(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure StyleClick(Sender: TObject);
  private
    fCtx: TAGContext;
    fPicker: TAGPickerWorker; // running preview worker, Nil if none
    fSet: TAGSetWorker; // running generation, Nil if none
    fDead: TList; // finished workers, freed later
    fShown, fReady: Boolean;
    Procedure StartPreviews;
    Procedure StartGenerate(s: TAGStyle);
    Procedure PickerDone(Sender: TObject);
    Procedure SetProgress(Sender: TObject);
    Procedure SetDone(Sender: TObject);
    Procedure FreeDead;
    Function Images(s: TAGStyle): TImage;
  public
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses uag_random, uag_image, uag_gui, ulanguage;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  fDead := TList.Create;
  fCtx := TAGContext.Create;
  fCtx.Name := GuiFolder;
  fCtx.Seed := NewRandomSeed;
End;

Procedure TForm2.FormDestroy(Sender: TObject);
Begin
  FreeAndNil(fPicker);
  FreeAndNil(fSet);
  FreeDead;
  fDead.Free;
  fCtx.Free;
End;

Procedure TForm2.FormShow(Sender: TObject);
Begin
  If fShown Then exit;
  fShown := true;
  If GuiStyle <> '' Then
    StartGenerate(StringToStyle(GuiStyle))
  Else
    StartPreviews;
End;

Function TForm2.Images(s: TAGStyle): TImage;
Begin
  Case s Of
    asSoft: result := ImageSoft;
    asPixel: result := ImagePixel;
    asRetro: result := ImageRetro;
  Else
    result := ImageFlat;
  End;
End;

Procedure TForm2.FreeDead;
Var
  i: Integer;
Begin
  For i := 0 To fDead.Count - 1 Do
    TObject(fDead[i]).Free;
  fDead.Clear;
End;

Procedure TForm2.StartPreviews;
Var
  s: TAGStyle;
  bmp: TBitmap;
  w: Integer;
Begin
  FreeDead;
  If assigned(fPicker) Then Begin
    fPicker.Cancel;
    fDead.Add(fPicker); // its result is ignored
    fPicker := Nil;
  End;
  fReady := false;
  // placeholders
  bmp := TBitmap.Create;
  Try
    bmp.SetSize(192, 288);
    bmp.Canvas.Brush.Color := RGBToColor(40, 40, 48);
    bmp.Canvas.FillRect(0, 0, 192, 288);
    bmp.Canvas.Font.Color := clWhite;
    w := bmp.Canvas.TextWidth(R_PreviewWait);
    bmp.Canvas.TextOut((192 - w) Div 2, 136, R_PreviewWait);
    For s := low(TAGStyle) To high(TAGStyle) Do
      Images(s).Picture.Assign(bmp);
  Finally
    bmp.Free;
  End;
  fPicker := TAGPickerWorker.Create(fCtx, '', @PickerDone);
  fPicker.Start;
End;

Procedure TForm2.PickerDone(Sender: TObject);
Var
  s: TAGStyle;
  bmp: TBitmap;
Begin
  If Sender <> fPicker Then exit; // an old one (roll again)
  If fPicker.Error <> '' Then Begin
    MessageDlg(format(RF_Error, [fPicker.Error]), mtError, [mbOK], 0);
    Close;
    exit;
  End;
  If Not fPicker.Aborted Then Begin
    For s := low(TAGStyle) To high(TAGStyle) Do Begin
      bmp := ImageToBitmap(fPicker.Images[s], 1);
      Try
        Images(s).Picture.Assign(bmp);
      Finally
        bmp.Free;
      End;
    End;
    fReady := true;
  End;
  fDead.Add(fPicker);
  fPicker := Nil;
  If fReady And (GuiScreenshot <> '') Then Begin
    Application.ProcessMessages;
    SaveFormScreenshot(self, GuiScreenshot);
    Close;
  End;
End;

Procedure TForm2.ButtonRollClick(Sender: TObject);
Begin
  fCtx.Seed := NewRandomSeed;
  StartPreviews;
End;

Procedure TForm2.StyleClick(Sender: TObject);
Begin
  StartGenerate(TAGStyle(TComponent(Sender).Tag));
End;

Procedure TForm2.StartGenerate(s: TAGStyle);
Begin
  If assigned(fSet) Then exit;
  If assigned(fPicker) Then fPicker.Cancel;
  fCtx.Style := s;
  PanelFlat.Visible := false;
  PanelSoft.Visible := false;
  PanelPixel.Visible := false;
  PanelRetro.Visible := false;
  ButtonRoll.Visible := false;
  ButtonCancel.Visible := false;
  LabelHint.Visible := false;
  PanelProgress.Visible := true;
  ProgressBar1.Position := 0;
  LabelProgress.Caption := format(RF_Generating, [0, 0]);
  fSet := TAGSetWorker.Create(fCtx, SetDir(GuiFolder), @SetDone, @SetProgress);
  fSet.Output.CancelAfter := GuiCancelAfter;
  fSet.Start;
End;

Procedure TForm2.SetProgress(Sender: TObject);
Begin
  If Sender <> fSet Then exit;
  ProgressBar1.Max := fSet.Total;
  ProgressBar1.Position := fSet.Done;
  LabelProgress.Caption := format(RF_Generating, [fSet.Done, fSet.Total]);
End;

Procedure TForm2.SetDone(Sender: TObject);
Var
  w: TAGSetWorker;
Begin
  If Sender <> fSet Then exit;
  w := fSet;
  fSet := Nil;
  fDead.Add(w);
  If (w.Error = '') And Not w.Aborted Then Begin
    WriteLastSet(GuiFolder);
    GuiWritten := true;
  End
  Else If w.InUse Then
    MessageDlg(format(RF_SetInUse, [GuiFolder]), mtError, [mbOK], 0)
  Else If w.Error <> '' Then
    MessageDlg(format(RF_Error, [w.Error]), mtError, [mbOK], 0);
  // cancel or error: the worker removed its temporary folder, nothing half done stays behind
  Close;
End;

Procedure TForm2.ButtonStopClick(Sender: TObject);
Begin
  If assigned(fSet) Then fSet.Cancel;
End;

Procedure TForm2.ButtonCancelClick(Sender: TObject);
Begin
  Close;
End;

Procedure TForm2.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  // a running generation is cancelled first, SetDone closes the window
  If assigned(fSet) Then Begin
    fSet.Cancel;
    CanClose := false;
  End;
End;

End.
