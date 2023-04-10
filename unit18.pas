(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of config_td                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit18;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls;

Type

  { TForm18 }

  TForm18 = Class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private

  public

  End;

Var
  Form18: TForm18;

Implementation

{$R *.lfm}

{ TForm18 }

Procedure TForm18.FormCreate(Sender: TObject);
Begin
  caption := 'Information';
End;

Procedure TForm18.Timer1Timer(Sender: TObject);
Begin
  ProgressBar1.Position := (ProgressBar1.Position + 1) Mod 100;
End;

End.

