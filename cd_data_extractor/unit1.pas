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
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses ucdextractor;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  IniPropStorage1.IniFileName := 'settings.ini';
  caption := 'FPC Atomic data extractor ver. 0.01';
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  label1.caption := IniPropStorage1.ReadString('CD-Root', '');
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  IniPropStorage1.WriteString('CD-Root', label1.caption);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If SelectDirectoryDialog1.Execute Then Begin
    If CheckCDRootFolder(SelectDirectoryDialog1.FileName) Then Begin
      label1.Caption := SelectDirectoryDialog1.FileName;
    End
    Else Begin
      showmessage('Error, you need to give the Atomic bomberman CD-Image root folder.' +
        'That is typically the folder containing the subfolders "Data", "Data/Sound" ...');
    End;
  End;
End;

End.

