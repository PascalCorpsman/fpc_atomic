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
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uanifile, ucdextractor;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure ListBox1Click(Sender: TObject);
  private
    fAni: TAniFile;
    Procedure ClearLCL;
    Function LCLToJob(): TAniJob;
  public
    AtomicRootFolder: String;
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses FileUtil, LazFileUtils, math, Clipbrd;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Ani job creater, only for developing, keep out !';
  label1.caption := '';
  fAni := TAniFile.Create();
  ClearLCL;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
End;

Procedure TForm2.FormDestroy(Sender: TObject);
Begin
  fAni.Free;
  fAni := Nil;
End;

Procedure TForm2.Image2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  index, w, h, fpr: integer;
Begin
  (*
   * Feature Click to index ;-)
   *)
  If ssleft In shift Then Begin
    w := StrToIntDef(edit1.text, 0);
    h := StrToIntDef(edit2.text, 0);
    fpr := StrToIntDef(edit3.text, 0);
    If (w = 0) Or (h = 0) Then exit;
    index := (x Div w) + (y Div h) * fpr;
    If trim(edit4.text) = '' Then Begin
      edit4.text := inttostr(index);
    End
    Else Begin
      edit4.text := edit4.text + ', ' + inttostr(index);
    End;
  End;
End;

Procedure TForm2.ListBox1Click(Sender: TObject);
Begin
  // Preview eines Einzelbildes
  If ListBox1.ItemIndex = -1 Then exit;
  image1.Picture.Assign(fAni.Image[ListBox1.ItemIndex].Bitmap);
  label3.caption := format('%s (%d/%d)',
    [
    ExtractFileNameOnly(fAni.Image[ListBox1.ItemIndex].Name),
      fAni.Image[ListBox1.ItemIndex].Bitmap.Width,
      fAni.Image[ListBox1.ItemIndex].Bitmap.Height
      ]);
End;

Procedure TForm2.ClearLCL;
Begin
  ListBox1.Clear;
  label3.Caption := '';
  Image1.Picture.Clear;
  Image2.Picture.Clear;
  edit1.text := '';
  edit2.text := '';
  edit3.text := '1';
  edit4.text := '';
  edit5.text := '';
  RadioGroup1.ItemIndex := 0;
  RadioGroup2.ItemIndex := 0;
  RadioGroup3.ItemIndex := 1;
  label8.caption := '';
End;

Function TForm2.LCLToJob(): TAniJob;
Begin
  result.SourceAni := label1.Caption;
  result.Width := strtointdef(edit1.Text, 0);
  result.Height := strtointdef(edit2.Text, 0);
  result.FramesPerRow := strtointdef(edit3.Text, 0);
  Case RadioGroup1.ItemIndex Of
    0: result.Alignment := taLeftJustify;
    1: result.Alignment := taCenter;
    2: result.Alignment := taRightJustify;
  End;
  Case RadioGroup2.ItemIndex Of
    0: result.Layout := tlTop;
    1: result.Layout := tlCenter;
    2: result.Layout := tlBottom;
  End;
  result.ImageSequence := edit4.text;
  result.DestPng := edit5.text;
  Case RadioGroup3.ItemIndex Of
    0: result.TransparentMode := tmFirstPixel;
    1: result.TransparentMode := tmBlack;
    2: result.TransparentMode := tmFirstPixelPerFrame;
  End;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  i: Integer;
  maxw, maxh: integer;
  s: String;
Begin
  OpenDialog1.InitialDir := AtomicRootFolder;
  If OpenDialog1.Execute Then Begin
    label1.Caption := ExtractRelativePath(AtomicRootFolder, OpenDialog1.FileName);
    ClearLCL;
    s := lowercase(ExtractFileNameOnly(OpenDialog1.FileName)) + '.png';
    edit5.text := 'data' + PathDelim + s;
    If Not fAni.LoadFromFile(OpenDialog1.FileName) Then Begin
      ShowMessage('Error, unable to load :' + OpenDialog1.FileName);
      exit;
    End;
    maxw := 0;
    maxh := 0;
    s := '';
    For i := 0 To fani.ImageCount - 1 Do Begin
      ListBox1.Items.Add(inttostr(i) + ' - ' + ExtractFileNameOnly(fAni.Image[i].Name));
      maxw := max(maxw, fAni.Image[i].Bitmap.Width);
      maxh := max(maxh, fAni.Image[i].Bitmap.Height);
      If i > 0 Then Begin
        If fAni.ImageCount Mod i = 0 Then Begin
          Edit3.Text := inttostr(i);
        End;
        s := s + ', ' + inttostr(i);
      End
      Else Begin
        s := '0';
      End;
    End;
    edit1.text := IntToStr(maxw);
    edit2.text := IntToStr(maxh);
    edit4.text := s;
    If ListBox1.Items.Count <> 0 Then Begin
      ListBox1.ItemIndex := 0;
      ListBox1Click(Nil);
    End;
    button2.click;
  End;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Var
  preview: TBitmap;
  job: TAniJob;
Begin
  // Create Preview
  Image2.Picture.Clear;
  job := LCLToJob();
  preview := DoAniJob(AtomicRootFolder, job);
  If assigned(preview) Then Begin
    image2.Picture.Assign(preview);
    label8.caption := format('Preview (%d/%d)', [preview.Width, preview.Height]);
    preview.Free;
  End
  Else Begin
    label8.caption := 'Error.';
  End;
End;

Procedure TForm2.Button3Click(Sender: TObject);
  Function FixPath(aFilename: String): String;
  Begin
    result := aFilename;
    result := StringReplace(result, PathDelim, ''' + Pathdelim + ''', [rfReplaceAll]);
  End;
Var
  job: TAniJob;
  s: String;
Begin
  // Copy Job to Clibpoard
  job := LCLToJob();
  s := 'AddAniJob(''' + FixPath(job.SourceAni) + ''', ' +
    inttostr(job.Width) + ', ' +
    inttostr(job.Height) + ', ' +
    inttostr(job.FramesPerRow) + ', ';
  Case job.Alignment Of
    taLeftJustify: s := s + 'taLeftJustify, ';
    taCenter: s := s + 'taCenter, ';
    taRightJustify: s := s + 'taRightJustify, ';
  End;
  Case job.Layout Of
    tlTop: s := s + 'tlTop, ';
    tlCenter: s := s + 'tlCenter, ';
    tlBottom: s := s + 'tlBottom, ';
  End;
  s := s + '''' + job.ImageSequence + ''', ' +
    '''' + FixPath(job.DestPng) + ''', ';
  Case job.TransparentMode Of
    tmBlack: s := s + 'tmBlack);' + LineEnding;
    tmFirstPixel: s := s + 'tmFirstPixel);' + LineEnding;
    tmFirstPixelPerFrame: s := s + 'tmFirstPixelPerFrame);' + LineEnding;
  End;
  Clipboard.AsText := s;
End;

End.

