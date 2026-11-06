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
Unit Unit7;

{$MODE ObjFPC}{$H+}

(*
 * Einrichtung: woher kommen Grafiken und Sounds? Erscheint einmal beim ersten Start ohne Spieldaten und ohne Grafiksatz,
 * sonst über "Einrichtung..." oder den Parameter --setup. Das Ergebnis wertet Form1 aus (RunSetup):
 *   mrYes    = eigene erzeugen (Grafik-Generator, Stil-Auswahl)
 *   mrNo     = von der CD (CD Extraktor)
 *   mrCancel = später
 *)

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    ButtonGenerate: TButton;
    ButtonCD: TButton;
    ButtonLater: TButton;
    LabelTitle: TLabel;
    LabelQuestion: TLabel;
    LabelGenerate: TLabel;
    LabelCD: TLabel;
    LabelLater: TLabel;
    Procedure ButtonCDClick(Sender: TObject);
    Procedure ButtonGenerateClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private
    fCanGenerate, fCanExtract: Boolean;
  public
    (*
     * Zeigt den Dialog, CanGenerate / CanExtract = Generator bzw. CD Extraktor sind vorhanden. Die Knöpfe bleiben
     * immer aktiv: fehlt das Programm, sagt ein Klick warum es nicht geht (ein grauer Knopf erklärt nichts).
     *)
    Function Execute(CanGenerate, CanExtract: Boolean): TModalResult;
  End;

Var
  Form7: TForm7;

Implementation

{$R *.lfm}

Uses ulayout, ulanguage;

{ TForm7 }

Procedure TForm7.FormShow(Sender: TObject);
Begin
  FitLayout(self); // Platz für die Texte im aktuellen Widgetset und in der aktuellen Sprache
End;

Procedure TForm7.ButtonGenerateClick(Sender: TObject);
Begin
  If fCanGenerate Then exit; // ModalResult des Knopfs schließt den Dialog
  showmessage(R_SetupNoGenerator);
  ModalResult := mrNone;
End;

Procedure TForm7.ButtonCDClick(Sender: TObject);
Begin
  If fCanExtract Then exit;
  showmessage(R_SetupNoExtractor);
  ModalResult := mrNone;
End;

Function TForm7.Execute(CanGenerate, CanExtract: Boolean): TModalResult;
Begin
  fCanGenerate := CanGenerate;
  fCanExtract := CanExtract;
  If CanGenerate Then Begin
    ActiveControl := ButtonGenerate;
  End
  Else If CanExtract Then Begin
    ActiveControl := ButtonCD;
  End
  Else Begin
    ActiveControl := ButtonLater;
  End;
  result := ShowModal;
End;

End.

