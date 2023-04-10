Program fpc_atomic;

{$MODE objfpc}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, Unit18, uatomic;

Begin
  Application.Title := '';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm18, Form18);
  Application.Run;
End.

