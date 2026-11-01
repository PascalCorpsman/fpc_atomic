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
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage, ExtCtrls, ComCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button21: TButton;
    Button6: TButton;
    ButtonBack: TButton;
    ButtonClose: TButton;
    ButtonNext: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    IniPropStorage1: TIniPropStorage;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    LabelStep: TLabel;
    Memo1: TMemo;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel21: TPanel;
    PanelButtons: TPanel;
    ProgressBar1: TProgressBar;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button11Click(Sender: TObject);
    Procedure Button12Click(Sender: TObject);
    Procedure Button13Click(Sender: TObject);
    Procedure Button21Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure ButtonBackClick(Sender: TObject);
    Procedure ButtonCloseClick(Sender: TObject);
    Procedure ButtonNextClick(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure RadioButtonChange(Sender: TObject);
  private
    fRunning: Boolean; // True while DoExtraction runs
    fWarnings: integer; // Warnings and errors logged during the extraction
    fCDNote: String; // Additional info for the CD folder (auto found, DATA selected ..)
    Procedure OnDoLog(Const LogText: String);
    Procedure OnProgress(Done, Total: integer);
    Procedure ApplyLanguage();
    Procedure ShowPage(Index: integer);
    Procedure UpdatePage1();
    Procedure UpdatePage2();
    Procedure UpdatePage2Status();
    Procedure UpdatePage3();
    Procedure UpdateButtons();
    Function CDFolder(): String;
    Function AtomicFolder(): String;
    Function Page1Valid(): Boolean;
    Function Page2Valid(): Boolean;
    Procedure SetStatus(aLabel: TLabel; Const aText: String; aColor: TColor);
    Procedure KeepOrder(Const Order: Array Of TControl);
    Procedure RunExtraction();
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses Unit2, ucdextractor, uatomic_global, ulanguage, IniFiles, LCLIntf;

Const
  PageCount = 4;
  AniZipURL = 'https://www.geocities.ws/mheid.geo/download/ani.zip';
  ColorOk = clGreen;
  ColorWarning = $00007FD0; // Orange
  ColorError = clRed;

Function AtomicExeName(): String;
Begin
  result := 'fpc_atomic'{$IFDEF Windows} + '.exe'{$ENDIF};
End;

Function NormFolder(Const aFolder: String): String;
Begin
  result := ExcludeTrailingPathDelimiter(Trim(aFolder));
  If result <> '' Then result := ExpandFileName(result);
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  ExeFolder, Stored, Found: String;
Begin
  InitLogger();
  LoggerSetOnDoLog(@OnDoLog);
  SetProgressCallback(@OnProgress);
  IniPropStorage1.IniFileName := GetAtomicConfigFile;
  (*
   * History is above DefCaption definition
   *)
  caption := DefCaption;
  fRunning := false;
  fCDNote := '';
  Memo1.Clear;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  ExeFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  (*
   * CD content: the folder of the last run, if it is still valid, otherwise search for it.
   *)
  Stored := IniPropStorage1.ReadString('CD-Root', '');
  If Stored <> '' Then Stored := ConcatRelativePath(ExtractFilePath(ParamStr(0)), Stored);
  If (Stored <> '') And CheckCDRootFolder(Stored) Then Begin
    Edit1.Text := Stored;
  End
  Else Begin
    Found := FindCDRootFolder(ExeFolder);
    If Found <> '' Then Begin
      Edit1.Text := Found;
      fCDNote := '1'; // Auto found, the text is set after the language is loaded
    End
    Else Begin
      Edit1.Text := Stored;
      If Stored <> '' Then fCDNote := '2'; // Stored folder invalid
    End;
  End;
  (*
   * FPC Atomic: the folder of the last run, if valid, otherwise the folder of the extractor.
   *)
  Stored := IniPropStorage1.ReadString('FPC-Atomic', '');
  If Stored <> '' Then Stored := ConcatRelativePath(ExtractFilePath(ParamStr(0)), Stored);
  If (Stored <> '') And CheckFPCAtomicFolder(Stored) Then Begin
    Edit2.Text := Stored;
  End
  Else If CheckFPCAtomicFolder(ExeFolder) Or (Stored = '') Then Begin
    Edit2.Text := ExeFolder;
  End
  Else Begin
    Edit2.Text := Stored;
  End;
  Notebook1.PageIndex := 0;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  If Language.CurrentLanguage <> '' Then exit;
  ApplyLanguage();
  // The note of FormCreate can be translated now
  If fCDNote = '1' Then fCDNote := R_AutoFound;
  If fCDNote = '2' Then fCDNote := R_StoredFolderInvalid;
  ShowPage(0);
End;

Procedure TForm1.ApplyLanguage;
Var
  ini: TIniFile;
  Code, Filename: String;
  i: Integer;
Begin
  (*
   * Same language as the launcher, falls back to english if there is no file for it.
   *)
  ini := TIniFile.Create(GetAtomicConfigFile());
  Code := ini.ReadString('Launcher', 'Language', 'en');
  ini.free;
  For i := 1 To ParamCount Do Begin
    If SameText(ParamStr(i), '-lazarus-ide') Then Begin
      Language.AskNameChanges := true;
      Language.DeleteUnusedItems := true;
      Code := 'en'; // Force to en!
      break;
    End;
  End;
  Filename := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'cd_data_extractor_lang.';
  If Not FileExists(Filename + Code) Then Code := 'en';
  Language.AddIgnoredIdentifier('Form1.Caption');
  Language.AddIgnoredIdentifier('Form1.ButtonNext.Caption'); // Set by UpdateButtons
  Language.AddIgnoredComponent(Form2);
  Language.CurrentLanguage := Filename + Code;
End;

Procedure TForm1.FormActivate(Sender: TObject);
Begin
  (*
   * The user may have changed something in the file manager (e.g. merged ani.zip) -> check again
   *)
  If fRunning Or (Language.CurrentLanguage = '') Then exit;
  Case Notebook1.PageIndex Of
    0: UpdatePage1();
    1: UpdatePage2();
  End;
  UpdateButtons();
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If fRunning Then Begin
    ShowMessage(R_ExtractionRunning);
    CanClose := false;
    exit;
  End;
  IniPropStorage1.WriteString('CD-Root', ExtractRelativePath(ExtractFilePath(ParamStr(0)), CDFolder()));
  IniPropStorage1.WriteString('FPC-Atomic', ExtractRelativePath(ExtractFilePath(ParamStr(0)), AtomicFolder()));
End;

Procedure TForm1.OnDoLog(Const LogText: String);
Var
  atext: String;
Begin
  // Abschneiden Zeitstempel
  atext := copy(LogText, pos('|', LogText) + 1, length(LogText));
  // Abschneiden Loglevel
  atext := copy(atext, pos('|', atext) + 1, length(atext));
  // Abschneiden Stack Info
  atext := copy(atext, pos(':', atext) + 1, length(atext));
  Memo1.Lines.Append(atext);
  // The phases of DoExtraction
  If trim(atext) = 'PCXs' Then Label42.Caption := R_PhaseImages;
  If trim(atext) = 'ANIs' Then Label42.Caption := R_PhaseAnimations;
  If trim(atext) = 'Sounds' Then Label42.Caption := R_PhaseSounds;
  If trim(atext) = 'Shemes' Then Label42.Caption := R_PhaseSchemes;
  If (pos('error', lowercase(atext)) <> 0) Or (pos('warning', lowercase(atext)) <> 0) Then inc(fWarnings);
  Application.ProcessMessages;
  If form2.Visible Then Begin
    ShowMessage(atext);
  End;
End;

Procedure TForm1.OnProgress(Done, Total: integer);
Begin
  ProgressBar1.Max := Total;
  ProgressBar1.Position := Done;
  Application.ProcessMessages;
End;

Function TForm1.CDFolder: String;
Begin
  result := NormFolder(Edit1.Text);
End;

Function TForm1.AtomicFolder: String;
Begin
  result := NormFolder(Edit2.Text);
End;

Function TForm1.Page1Valid: Boolean;
Begin
  result := (CDFolder() <> '') And CheckCDRootFolder(CDFolder());
End;

Function TForm1.Page2Valid: Boolean;
Begin
  result := (AtomicFolder() <> '') And CheckFPCAtomicFolder(AtomicFolder()) And
    (Not SameFileName(AtomicFolder(), CDFolder()));
End;

Procedure TForm1.SetStatus(aLabel: TLabel; Const aText: String; aColor: TColor);
Begin
  aLabel.Caption := aText;
  aLabel.Font.Color := aColor;
  aLabel.Visible := aText <> '';
End;

Procedure TForm1.KeepOrder(Const Order: Array Of TControl);
Var
  i: integer;
Begin
  (*
   * alTop controls that are shown again are sorted in by their Top, which may
   * have changed while they were hidden -> restore the designed order.
   *)
  If length(Order) = 0 Then exit;
  Order[0].Parent.DisableAlign;
  Try
    For i := 0 To high(Order) Do Begin
      Order[i].Top := i;
    End;
  Finally
    Order[0].Parent.EnableAlign;
  End;
End;

Procedure TForm1.ShowPage(Index: integer);
Begin
  Notebook1.PageIndex := Index;
  LabelStep.Caption := format(RF_Step, [Index + 1, PageCount]);
  Case Index Of
    0: UpdatePage1();
    1: UpdatePage2();
    2: UpdatePage3();
  End;
  UpdateButtons();
End;

Procedure TForm1.UpdatePage1;
Var
  Labels: Array[TCDFolder] Of TLabel;
  Checks: TCDFolderCheck;
  f: TCDFolder;
  Missing: TStringArray;
  Exists: Boolean;
Begin
  Labels[cdAni] := Label13;
  Labels[cdRes] := Label14;
  Labels[cdSchemes] := Label15;
  Labels[cdSound] := Label16;
  Exists := (CDFolder() <> '') And DirectoryExists(CDFolder());
  If Exists Then Begin
    Checks := CheckCDRootFolderDetails(CDFolder());
    For f In TCDFolder Do Begin
      If Checks[f] Then Begin
        SetStatus(Labels[f], format(RF_SubFolderFound, [PathDelim, uppercase(CDFolderNames[f])]), ColorOk);
      End
      Else Begin
        SetStatus(Labels[f], format(RF_SubFolderMissing, [PathDelim, uppercase(CDFolderNames[f])]), ColorError);
      End;
    End;
  End
  Else Begin
    For f In TCDFolder Do Begin
      SetStatus(Labels[f], '', ColorError);
    End;
    If CDFolder() <> '' Then SetStatus(Label13, R_FolderNotFound, ColorError);
  End;
  // The expansion pack is optional and only of interest, when the folder is valid
  If Page1Valid() Then Begin
    Missing := GetMissingOptionalAnis(CDFolder());
    If length(Missing) = 0 Then Begin
      SetStatus(Label17, R_AniZipComplete, ColorOk);
      SetStatus(Label18, '', ColorOk);
    End
    Else Begin
      SetStatus(Label17, format(RF_AniZipMissing, [length(Missing), GetOptionalAniCount()]), ColorWarning);
      SetStatus(Label18, format(R_AniZipHint, [PathDelim]), clDefault);
    End;
    Panel12.Visible := length(Missing) <> 0;
  End
  Else Begin
    SetStatus(Label17, '', clDefault);
    SetStatus(Label18, '', clDefault);
    Panel12.Visible := false;
  End;
  // The verdict
  If CDFolder() = '' Then Begin
    SetStatus(Label19, R_NoFolder, clDefault);
  End
  Else If Page1Valid() Then Begin
    SetStatus(Label19, trim(fCDNote + LineEnding + R_CDFolderOk), ColorOk);
  End
  Else If Not SameFileName(GetCDRootFolder(CDFolder()), CDFolder()) Then Begin
    // A folder inside the CD content, e.g. DATA itself
    SetStatus(Label19, trim(fCDNote + LineEnding + format(RF_CDFolderInside, [GetCDRootFolder(CDFolder())])), ColorError);
  End
  Else Begin
    SetStatus(Label19, trim(fCDNote + LineEnding + R_CDFolderWrong), ColorError);
  End;
  KeepOrder([Label11, Label12, Panel11, Label13, Label14, Label15, Label16, Label17, Label18, Panel12, Label19]);
End;

Procedure TForm1.UpdatePage2;
Begin
  Try
    UpdatePage2Status();
  Finally
    KeepOrder([Label21, Label22, Panel21, Label23, Label24, Label25]);
  End;
End;

Procedure TForm1.UpdatePage2Status;
Var
  Missing, MissingOptional: integer;
Begin
  SetStatus(Label24, '', clDefault);
  SetStatus(Label25, '', clDefault);
  If AtomicFolder() = '' Then Begin
    SetStatus(Label23, R_NoFolder, clDefault);
    exit;
  End;
  If Not DirectoryExists(AtomicFolder()) Then Begin
    SetStatus(Label23, R_FolderNotFound, ColorError);
    exit;
  End;
  If SameFileName(AtomicFolder(), CDFolder()) Then Begin
    SetStatus(Label23, R_SameFolder, ColorError);
    exit;
  End;
  If Not CheckFPCAtomicFolder(AtomicFolder()) Then Begin
    SetStatus(Label23, format(RF_ExeMissing, [AtomicExeName()]), ColorError);
    exit;
  End;
  SetStatus(Label23, format(RF_ExeFound, [AtomicExeName()]), ColorOk);
  // What is already there
  Missing := length(GetMissingFiles(AtomicFolder(), false));
  MissingOptional := length(GetMissingFiles(AtomicFolder(), true));
  If Missing = 0 Then Begin
    SetStatus(Label24, R_DataComplete, ColorOk);
  End
  Else If Missing + MissingOptional = length(GetAllFilesToCheck()) Then Begin
    SetStatus(Label24, R_DataNone, clDefault);
  End
  Else Begin
    SetStatus(Label24, format(RF_DataMissing, [Missing, length(GetAllFilesToCheck()) - length(GetOptionalFilesToCheck())]), ColorWarning);
  End;
  If MissingOptional <> 0 Then Begin
    SetStatus(Label25, format(RF_OptionalMissing, [MissingOptional]), clDefault);
  End;
End;

Procedure TForm1.UpdatePage3;
Var
  Missing, Total: integer;
  NoData: Boolean;
Begin
  (*
   * Optional files are only counted, when the expansion pack is on the CD, otherwise they can not be created.
   *)
  Missing := length(GetMissingFiles(AtomicFolder(), false));
  If length(GetMissingOptionalAnis(CDFolder())) = 0 Then Begin
    Missing := Missing + length(GetMissingFiles(AtomicFolder(), true));
  End;
  Total := length(GetAllFilesToCheck());
  NoData := length(GetMissingFiles(AtomicFolder(), false)) + length(GetMissingFiles(AtomicFolder(), true)) = Total;
  // Preselect what makes sense
  RadioButton1.Enabled := Not NoData;
  If NoData Then Begin
    RadioButton2.Checked := true;
  End
  Else If Missing = 0 Then Begin
    RadioButton2.Checked := true;
  End
  Else Begin
    RadioButton1.Checked := true;
  End;
  If NoData Then Begin
    Label32.Caption := R_ModeNoData;
  End
  Else If Missing = 0 Then Begin
    Label32.Caption := R_ModeNothingMissing;
  End
  Else Begin
    Label32.Caption := format(RF_ModeMissing, [Missing]);
  End;
  Label33.Caption := format(RF_ModeAll, [Total]);
  Label34.Caption := format(RF_Summary, [CDFolder(), AtomicFolder()]);
End;

Procedure TForm1.UpdateButtons;
Begin
  ButtonBack.Enabled := (Notebook1.PageIndex > 0) And (Not fRunning);
  ButtonClose.Enabled := Not fRunning;
  ButtonNext.Visible := Notebook1.PageIndex < PageCount - 1;
  If Notebook1.PageIndex = PageCount - 2 Then Begin
    ButtonNext.Caption := R_StartExtraction;
  End
  Else Begin
    ButtonNext.Caption := R_Next;
  End;
  Case Notebook1.PageIndex Of
    0: ButtonNext.Enabled := Page1Valid();
    1: ButtonNext.Enabled := Page2Valid();
  Else
    ButtonNext.Enabled := true;
  End;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Begin
  If Language.CurrentLanguage = '' Then exit; // Not shown yet
  fCDNote := '';
  UpdatePage1();
  UpdateButtons();
End;

Procedure TForm1.Edit2Change(Sender: TObject);
Begin
  If Language.CurrentLanguage = '' Then exit; // Not shown yet
  UpdatePage2();
  UpdateButtons();
End;

Procedure TForm1.RadioButtonChange(Sender: TObject);
Begin
  UpdateButtons();
End;

Procedure TForm1.Button11Click(Sender: TObject);
Var
  Folder: String;
Begin
  // Browse for the CD content
  SelectDirectoryDialog1.Title := R_SelectCDFolder;
  If DirectoryExists(CDFolder()) Then SelectDirectoryDialog1.InitialDir := CDFolder();
  If SelectDirectoryDialog1.Execute Then Begin
    Folder := GetCDRootFolder(NormFolder(SelectDirectoryDialog1.FileName));
    Edit1.Text := Folder;
    If Not SameFileName(Folder, NormFolder(SelectDirectoryDialog1.FileName)) Then Begin
      fCDNote := R_DataFolderSelected;
      UpdatePage1();
    End;
  End;
End;

Procedure TForm1.Button12Click(Sender: TObject);
Var
  Folder: String;
Begin
  // Search for the CD content
  Screen.Cursor := crHourGlass;
  Try
    Folder := FindCDRootFolder(ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))));
  Finally
    Screen.Cursor := crDefault;
  End;
  If Folder <> '' Then Begin
    Edit1.Text := Folder;
    fCDNote := R_AutoFound;
  End
  Else Begin
    fCDNote := R_SearchFailed;
  End;
  UpdatePage1();
  UpdateButtons();
End;

Procedure TForm1.Button13Click(Sender: TObject);
Begin
  OpenURL(AniZipURL);
End;

Procedure TForm1.Button21Click(Sender: TObject);
Begin
  // Browse for FPC Atomic
  SelectDirectoryDialog1.Title := R_SelectAtomicFolder;
  If DirectoryExists(AtomicFolder()) Then SelectDirectoryDialog1.InitialDir := AtomicFolder();
  If SelectDirectoryDialog1.Execute Then Begin
    Edit2.Text := NormFolder(SelectDirectoryDialog1.FileName);
  End;
End;

Procedure TForm1.ButtonBackClick(Sender: TObject);
Begin
  If Notebook1.PageIndex > 0 Then ShowPage(Notebook1.PageIndex - 1);
End;

Procedure TForm1.ButtonNextClick(Sender: TObject);
Begin
  If Not ButtonNext.Enabled Then exit;
  If Notebook1.PageIndex = PageCount - 2 Then Begin
    ShowPage(PageCount - 1);
    RunExtraction();
  End
  Else Begin
    ShowPage(Notebook1.PageIndex + 1);
  End;
End;

Procedure TForm1.ButtonCloseClick(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.RunExtraction;
Var
  n: QWord;
  Missing: integer;
Begin
  Memo1.Clear;
  fWarnings := 0;
  ProgressBar1.Position := 0;
  SetStatus(Label43, '', clDefault);
  Label42.Caption := R_PhaseStart;
  fRunning := true;
  UpdateButtons();
  n := GetTickCount64();
  Try
    DoExtraction(CDFolder(), AtomicFolder(), RadioButton1.Checked);
  Finally
    fRunning := false;
  End;
  n := (GetTickCount64() - n) Div 1000;
  Label42.Caption := '';
  ProgressBar1.Position := ProgressBar1.Max;
  Missing := length(GetMissingFiles(AtomicFolder(), false));
  If Missing <> 0 Then Begin
    SetStatus(Label43, format(RF_DoneMissing, [n, Missing]), ColorError);
  End
  Else If fWarnings <> 0 Then Begin
    SetStatus(Label43, format(RF_DoneWarnings, [n, fWarnings]), ColorWarning);
  End
  Else Begin
    SetStatus(Label43, format(RF_DoneOk, [n]), ColorOk);
  End;
  KeepOrder([Label41, Label42, ProgressBar1, Label43, Label44]);
  UpdateButtons();
  ButtonClose.SetFocus;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  // Ani Job Creator
  If Not Page1Valid() Then Begin
    showmessage('First set atomic cd root folder!.');
    exit;
  End;
  form2.AtomicRootFolder := IncludeTrailingPathDelimiter(CDFolder());
  form2.ShowModal;
End;

End.
