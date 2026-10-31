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
Unit uitemsedit;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, StdCtrls, Menus;

Type

  (*
   * Ein "+" Button neben einem Edit mit Items im Format von StartItems (z.B. "kick,punch,bomb*3"):
   * Der Button zeigt ein Menü mit den Namen der Items (StartItemNames), das gewählte wird hinten angehängt.
   * Items, die schon im Edit stehen, sind ausgegraut und werden nicht doppelt eingefügt (die Anzahl steht als *n im Edit),
   * Grab und Spooger schließen sich aus, das neue ersetzt das andere.
   *
   * Gehört dem Button und wird mit ihm frei gegeben, also einfach im FormCreate anlegen:
   *   TItemsEditMenu.Create(Button4, Edit1);
   *)
  TItemsEditMenu = Class(TComponent)
  private
    fEdit: TEdit;
    fButton: TButton;
    fMenu: TPopupMenu;
    Procedure ButtonClick(Sender: TObject);
    Procedure MenuClick(Sender: TObject);
  public
    Constructor Create(aButton: TButton; aEdit: TEdit); reintroduce;
  End;

  // Index des Eintrags in Items (z.B. "bomb*4"), dessen Name ohne *n ItemName ist, -1 = nicht enthalten
Function ItemsIndexOf(Const Items: TStrings; Const ItemName: String): Integer;

Implementation

Uses uatomic_common;

Function ItemsIndexOf(Const Items: TStrings; Const ItemName: String): Integer;
Var
  i, j: Integer;
  s: String;
Begin
  result := -1;
  For i := 0 To Items.Count - 1 Do Begin
    s := lowercase(trim(Items[i]));
    j := pos('*', s);
    If j > 0 Then s := trim(copy(s, 1, j - 1));
    If s = ItemName Then Begin
      result := i;
      exit;
    End;
  End;
End;

// Die Einträge des Edits, leere Einträge (z.B. von ",," oder einem leeren Edit) fallen weg

Function SplitItems(Const Text: String): TStringList;
Var
  i: Integer;
Begin
  result := TStringList.Create;
  result.Delimiter := ',';
  result.StrictDelimiter := true;
  result.DelimitedText := Text;
  For i := result.Count - 1 Downto 0 Do Begin
    result[i] := trim(result[i]);
    If result[i] = '' Then result.Delete(i);
  End;
End;

{ TItemsEditMenu }

Constructor TItemsEditMenu.Create(aButton: TButton; aEdit: TEdit);
Var
  i: Integer;
  mi: TMenuItem;
Begin
  Inherited Create(aButton);
  fButton := aButton;
  fEdit := aEdit;
  fMenu := TPopupMenu.Create(self);
  For i := 0 To high(StartItemNames) Do Begin
    mi := TMenuItem.Create(fMenu);
    mi.Caption := StartItemNames[i];
    mi.Tag := i;
    mi.OnClick := @MenuClick;
    fMenu.Items.Add(mi);
  End;
  fButton.OnClick := @ButtonClick;
End;

Procedure TItemsEditMenu.ButtonClick(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
  p: TPoint;
Begin
  sl := SplitItems(fEdit.Text);
  For i := 0 To fMenu.Items.Count - 1 Do Begin
    fMenu.Items[i].Enabled := ItemsIndexOf(sl, StartItemNames[fMenu.Items[i].Tag]) = -1;
  End;
  sl.free;
  p := fButton.ClientToScreen(Point(0, fButton.Height));
  fMenu.PopUp(p.x, p.y);
End;

Procedure TItemsEditMenu.MenuClick(Sender: TObject);
Var
  sl: TStringList;
  ItemName, Other: String;
  i: Integer;
Begin
  ItemName := StartItemNames[(Sender As TMenuItem).Tag];
  sl := SplitItems(fEdit.Text);
  Try
    If ItemsIndexOf(sl, ItemName) <> -1 Then exit;
    Other := '';
    If ItemName = 'grab' Then Other := 'spooger';
    If ItemName = 'spooger' Then Other := 'grab';
    If Other <> '' Then Begin
      i := ItemsIndexOf(sl, Other);
      If i <> -1 Then sl.Delete(i);
    End;
    sl.Add(ItemName);
    fEdit.Text := sl.DelimitedText;
  Finally
    sl.free;
  End;
End;

End.
