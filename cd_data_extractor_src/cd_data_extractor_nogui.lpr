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
Program cd_data_extractor_nogui;

{$MODE objfpc}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Classes, imagesforlazarus, ucdextractor, IniFiles, sysutils
  { you can add units after this };

Procedure PrintHelp();
Begin
  //       12345678901234567890123456789012345678901234567890123456789012345678901234567890
  writeln('Online help for ' + DefCaption);
  writeln('Usage:');
  writeln('  either use the gui variant to setup settings.ini or use the following');
  writeln('  command line parameters:');
  writeln(' -cd <path of Atomic Bomberman CD>');
  writeln(' -atomic <path where fpc_atomic binary is stored>');
  halt();
End;

Type

  { TDummy }

  TDummy = Class
    Procedure Log(Logtext: String);
  End;

Function CheckDir(aDir: String): String;
Begin
  result := aDir;
  If Not DirectoryExists(aDir) Then Begin
    result := ConcatRelativePath(ExtractFilePath(ParamStr(0)), aDir);
  End;
End;

Var
  ini: TIniFile;
  CDFolder: String;
  AtomicFolder: String;
  i: Integer;
  dummy: TDummy;

  { TDummy }

Procedure TDummy.Log(Logtext: String);
Begin
  writeln(Logtext);
End;

Begin
  ini := TIniFile.Create('settings.ini');
  CDFolder := ini.ReadString('TApplication.Form1', 'CD-Root', '');
  AtomicFolder := ini.ReadString('TApplication.Form1', 'FPC-Atomic', '');
  ini.free;
  For i := 1 To ParamCount Do Begin
    If ParamStr(i) = '-cd' Then Begin
      CDFolder := CheckDir(ParamStr(i + 1));
    End;
    If ParamStr(i) = '-atomic' Then Begin
      AtomicFolder := CheckDir(ParamStr(i + 1));
    End;
    If (ParamStr(i) = '?') Or (ParamStr(i) = '-?') Or (ParamStr(i) = '-help') Or (ParamStr(i) = '-man') Then Begin
      PrintHelp();
    End;
  End;
  If (CDFolder = '') Or (AtomicFolder = '') Then Begin
    PrintHelp();
  End;
  dummy := TDummy.Create;
  DoExtraction(CDFolder, AtomicFolder, @dummy.Log, false);
  dummy.free;

End.

