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
Unit ukeygrabber;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, forms, StdCtrls, Controls;

Type

  { TKeyGrabber }

  TKeyGrabber = Class(TForm)
    Label1: TLabel;
    Procedure FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
  private
    fResKey: Word;
  public
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    Function GrabVkKey(): Word;
  End;

Function VkKeyToString(Key: Word): String;

Implementation

Uses LCLType;

Function VkKeyToString(Key: Word): String;
Begin
  result := uppercase(VK2Char(key));
  If result = '?' Then Begin
    Case Key Of
      VK_BACK: result := 'Back';
      VK_RETURN: result := 'Return';
      VK_LEFT: result := 'Left';
      VK_RIGHT: result := 'Right';
      VK_UP: result := 'Up';
      VK_DOWN: result := 'Down';
      VK_SPACE: result := 'Space';
      VK_SHIFT: result := 'Shift';
      VK_TAB: result := 'Tap';
      VK_CAPITAL: result := 'Capital';
      VK_F1..VK_F12: result := 'F' + inttostr(key - VK_F1 + 1);
      VK_INSERT: result := 'Insert';
      VK_DELETE: result := 'Delete';
      VK_HOME: result := 'Home';
      VK_END: result := 'End';
      VK_PRIOR: result := 'Prior';
      VK_NEXT: result := 'Next';
      VK_ADD: result := 'Add';
      VK_SUBTRACT: result := 'Subtract';
      VK_CONTROL:result := 'Control';
    End;
  End;
End;

{ TKeyGrabber }

Constructor TKeyGrabber.CreateNew(AOwner: TComponent; Num: Integer);
Begin
  Inherited CreateNew(AOwner, Num);
  width := 250;
  height := 100;
  BorderStyle := bsDialog;
  FormStyle := fsStayOnTop;
  Position := poScreenCenter;
  Caption := '';
  label1 := TLabel.Create(self);
  label1.Name := 'Label1';
  label1.Parent := self;
  label1.Top := 10;
  label1.Left := 10;
  label1.caption := 'Press key to be grabbed' + LineEnding + 'ESC = Cancel';
  OnKeyDown := @FormKeyDown;
End;

Function TKeyGrabber.GrabVkKey: Word;
Begin
  modalresult := mrNone;
  fResKey := VK_UNKNOWN;
  result := fResKey;
  ShowModal;
  If ModalResult = mrNone Then exit;
  result := fResKey
End;


Procedure TKeyGrabber.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_ESCAPE Then close;
  fResKey := Key;
  ModalResult := mrOK;
End;

End.

