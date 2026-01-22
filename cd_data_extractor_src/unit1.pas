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
  IniPropStorage;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    Procedure OnDoLog(Const LogText: String);

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses Unit2, ucdextractor, uatomic_global;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  InitLogger();
  LoggerSetOnDoLog(@OnDoLog);
  IniPropStorage1.IniFileName := GetAtomicConfigFile;
  (*
   * History is above DefCaption definition
   *)
  caption := DefCaption;
  label1.caption := ConcatRelativePath(ExtractFilePath(ParamStr(0)), IniPropStorage1.ReadString('CD-Root', ''));
  label2.caption := ConcatRelativePath(ExtractFilePath(ParamStr(0)), IniPropStorage1.ReadString('FPC-Atomic', ''));
  memo1.clear;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  (*
   * Disable the ani job generator, it is only used for developping
   *)
  button6.Visible := false;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  IniPropStorage1.WriteString('CD-Root', ExtractRelativePath(ExtractFilePath(ParamStr(0)), label1.caption));
  IniPropStorage1.WriteString('FPC-Atomic', ExtractRelativePath(ExtractFilePath(ParamStr(0)), label2.caption));
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
  Application.ProcessMessages;
  If form2.Visible Then Begin
    ShowMessage(atext);
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  showmessage('Click the buttons from top to button, carefully read error messages and solve them.' + LineEnding + LineEnding +
    'If you master to come to the last button without errors you can enjoy the game *g*.');
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Set FPC Atomic Folder
  SelectDirectoryDialog1.Title := button4.caption;
  If SelectDirectoryDialog1.Execute Then Begin
    If CheckFPCAtomicFolder(SelectDirectoryDialog1.FileName) Then Begin
      label2.Caption := SelectDirectoryDialog1.FileName;
      If label1.caption = label2.caption Then Begin
        showmessage('Error, its not allowed to have CD-Image root folder = fpc atomic folder');
        label2.caption := '';
      End;
    End
    Else Begin
      showmessage('Error, the folder should at least contain the executable for fpc_atomic');
    End;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
  Function CheckDir(aDir: String): String;
  Begin
    result := aDir;
    If Not DirectoryExists(aDir) Then Begin
      result := ConcatRelativePath(ExtractFilePath(ParamStr(0)), aDir);
    End;
  End;

Var
  AtomicCDRootFolder, FPCAtomicRootFolder: String;
Begin
  // Start Extraction
  memo1.Clear;
  If label1.caption = label2.caption Then Begin
    showmessage('Error, its not allowed to have CD-Image root folder = fpc atomic folder');
  End;
  AtomicCDRootFolder := CheckDir(Label1.caption);
  FPCAtomicRootFolder := CheckDir(Label2.caption);
  DoExtraction(AtomicCDRootFolder, FPCAtomicRootFolder, CheckBox1.Checked);
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  // Ani Job Creator
  If label1.Caption = '' Then Begin
    showmessage('First set atomic cd root folder!.');
    exit;
  End;
  form2.AtomicRootFolder := IncludeTrailingPathDelimiter(Label1.Caption);
  form2.ShowModal;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Set atomic CD Root folder
  SelectDirectoryDialog1.Title := button1.caption;
  If SelectDirectoryDialog1.Execute Then Begin
    If CheckCDRootFolder(SelectDirectoryDialog1.FileName) Then Begin
      label1.Caption := SelectDirectoryDialog1.FileName;
      If label1.caption = label2.caption Then Begin
        showmessage('Error, its not allowed to have CD-Image root folder = fpc atomic folder');
        label1.caption := '';
      End;
    End
    Else Begin
      showmessage('Error, you need to give the Atomic bomberman CD-Image root folder.' +
        'That is typically the folder containing the subfolders "Data", "Data/Sound" ...');
    End;
  End;
End;

End.

