Unit Unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    ProgressBar1: TProgressBar;
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Progress';
End;

End.

