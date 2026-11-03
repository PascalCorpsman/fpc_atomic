(******************************************************************************)
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
(*
 * Shared parts of the UI program asset_generator_gui:
 *
 * Command line (the launcher starts it, see launcher/unit1.pas):
 *   --new                  editor with a new random set
 *   --set <folder>         editor with assets/<folder>/profile.ini
 *   --auto --set <folder>  first start without CD data: only the style
 *                          picker, then the set is generated into
 *                          assets/<folder>/ and the program ends
 *   --style <s>            with --auto: no picker, generate with style s
 *                          (flat | soft | pixel)
 *   --setup --folders <f,s,p,r> --names <f,s,p,r>
 *                          with --auto (setup of the launcher): one folder
 *                          and set name per style (comma separated, order
 *                          flat, soft, pixel, retro), the chosen style is
 *                          generated into its folder (instead of --set).
 *                          The launcher reads the seed from the profile.ini
 *                          of that set and generates the other styles with
 *                          the command line version in the background.
 *   --screenshot <file>    (debug) saves the window as png once everything
 *                          is rendered and ends
 *   --area <n>             (debug) selects the n-th area of the editor
 *                          (0 = figure, 1..10 players, 11 bomb, 12 flames,
 *                          13 power ups, 14 field objects, 15.. maps, then
 *                          menu screens, launcher image, hunted figure)
 *   --anim <n>             (debug) selects the n-th animation of the
 *                          animated preview (e.g. 6 = first die animation)
 *   --test-save <folder>   (debug) editor: switches "own figure per player"
 *                          on, moves the first slider, rolls the area seed,
 *                          saves into assets/<folder>/ and ends (also if
 *                          the save fails, without a dialog: exit code 1)
 *   --cancel-after <n>     (debug) a save is cancelled after n files (as if
 *                          the user pressed cancel)
 *   --test-sound <file>    (debug) editor: when everything is rendered, plays
 *                          the menu music at volume 0 for a moment (BASS like
 *                          the launcher, see ulaunchersound) and writes
 *                          "ok ..." or "error: ..." into <file>, then ends
 *                          (or takes the --screenshot first)
 *
 * The editor areas "Sounds" and "Music" (after the hunted figure, --area 29
 * and 30) list the rendered WAVs and play them with the BASS library next to
 * the program (bass.dll / libbass.so, loaded at runtime); without it the play
 * buttons are disabled. The language of the voices (profile Language) of a
 * new set is the one of the launcher (de -> de, all others -> en).
 *
 * Contract with the launcher: after a set was written, its folder name is
 * written into assets/last_set.txt (one line). Exit code 0 = a set was
 * written, 1 = nothing was written (cancel or error).
 *
 * Saving is atomic (see uag_output and uag_worker): the set is written into
 * assets/.<folder>.tmp-<n>/ and replaces assets/<folder>/ only when it is
 * complete; on cancel, error or when the old set is in use (the game runs
 * with it) the old set stays unchanged. A set folder is always generated
 * completely, files added to it by hand are lost on the next save.
 * ParseCommandLine removes leftovers of a crash (assets/.*.tmp-<n>, .old-<n>).
 *)
Unit uag_gui;

{$MODE ObjFPC}{$H+}

Interface

Uses Classes, SysUtils, Graphics, Forms, Controls, StdCtrls, uag_image;

Type
  TGuiMode = (gmNew, gmSet, gmAuto);

Var
  GuiMode: TGuiMode = gmNew;
  GuiFolder: String = ''; // folder below assets/
  GuiScreenshot: String = ''; // --screenshot
  GuiStyle: String = ''; // --style
  GuiArea: Integer = -1; // --area
  GuiAnim: Integer = -1; // --anim
  GuiTestSave: String = ''; // --test-save
  GuiCancelAfter: Integer = 0; // --cancel-after
  GuiTestSound: String = ''; // --test-sound
  GuiWritten: Boolean = false; // a set was written -> exit code 0
  GuiSetup: Boolean = false; // --setup
  GuiSetupFolders: TStringList = Nil; // --folders, one per style (Ord(TAGStyle))
  GuiSetupNames: TStringList = Nil; // --names

Procedure ParseCommandLine;
Procedure ApplyLanguage; // same language as the launcher
Function LauncherLanguage: String; // code of the launcher language, e.g. 'de'
Function DefaultVoiceLanguage: String; // language of the voices of a new set: 'de' or 'en'
Function SoundVolume: Integer; // volume of the game 0..10000 ([FPC_Atomic] Volume)
Function AppDir: String; // with trailing path delimiter
Function AssetsDir: String; // <AppDir>assets/
Function SetDir(Const Folder: String): String; // <AppDir>assets/<Folder>/
Procedure WriteLastSet(Const Folder: String);
Function FolderFromName(Const Name: String): String; // file system safe folder name

(*
 * Image -> bitmap, scaled up by Zoom (nearest neighbour), transparent and
 * fuchsia pixels are shown on a checker board.
 *)
Function ImageToBitmap(Img: TAGImage; Zoom: Integer): TBitmap;

Type
  TScreenshotPaint = Procedure(Canvas: TCanvas) Of Object; // draws onto the client area of the form

(*
 * Saves the client area of Form as png. Windows only lets the front window
 * be copied from the screen; when another program has the focus the form is
 * painted with PaintTo, which misses graphic controls: the captions of the
 * labels are drawn here, Extra draws the content of paint boxes.
 *)
Procedure SaveFormScreenshot(Form: TCustomForm; Const Filename: String; Extra: TScreenshotPaint = Nil);

Implementation

Uses Math, IniFiles, IntfGraphics, GraphType, LCLType, LCLIntf, ulanguage, uatomic_global, uag_output;

Procedure ParseCommandLine;
Var
  i: Integer;
Begin
  i := 1;
  While i <= ParamCount Do Begin
    Case ParamStr(i) Of
      '--new': GuiMode := gmNew;
      '--set': Begin
          inc(i);
          GuiFolder := ParamStr(i);
          If GuiMode <> gmAuto Then GuiMode := gmSet;
        End;
      '--auto': GuiMode := gmAuto;
      '--setup': GuiSetup := true;
      '--folders', '--names': Begin
          If Not assigned(GuiSetupFolders) Then Begin
            GuiSetupFolders := TStringList.Create;
            GuiSetupFolders.StrictDelimiter := true;
            GuiSetupNames := TStringList.Create;
            GuiSetupNames.StrictDelimiter := true;
          End;
          If ParamStr(i) = '--folders' Then
            GuiSetupFolders.CommaText := ParamStr(i + 1)
          Else
            GuiSetupNames.CommaText := ParamStr(i + 1);
          inc(i);
        End;
      '--screenshot': Begin
          inc(i);
          GuiScreenshot := ParamStr(i);
        End;
      '--style': Begin
          inc(i);
          GuiStyle := ParamStr(i);
        End;
      '--test-save': Begin
          inc(i);
          GuiTestSave := ParamStr(i);
        End;
      '--cancel-after': Begin
          inc(i);
          GuiCancelAfter := StrToIntDef(ParamStr(i), 0);
        End;
      '--test-sound': Begin
          inc(i);
          GuiTestSound := ParamStr(i);
        End;
      '--area': Begin
          inc(i);
          GuiArea := StrToIntDef(ParamStr(i), -1);
        End;
      '--anim': Begin
          inc(i);
          GuiAnim := StrToIntDef(ParamStr(i), -1);
        End;
    End;
    inc(i);
  End;
  If (GuiMode = gmAuto) And (GuiFolder = '') Then GuiFolder := 'generated';
  // --setup needs a folder per style
  If GuiSetup And Not (assigned(GuiSetupFolders) And (GuiSetupFolders.Count = 4)) Then GuiSetup := false;
  If DirectoryExists(AssetsDir()) Then CleanupSetLeftovers(AssetsDir());
End;

Function LauncherLanguage: String;
Var
  ini: TIniFile;
Begin
  ini := TIniFile.Create(GetAtomicConfigFile());
  Try
    result := LowerCase(Trim(ini.ReadString('Launcher', 'Language', 'en')));
  Finally
    ini.free;
  End;
End;

Function DefaultVoiceLanguage: String;
Begin
  If LauncherLanguage() = 'de' Then
    result := 'de'
  Else
    result := 'en'; // fr, es, ...: English voices
End;

Function SoundVolume: Integer;
Var
  ini: TIniFile;
Begin
  ini := TIniFile.Create(GetAtomicConfigFile());
  Try
    result := EnsureRange(ini.ReadInteger('FPC_Atomic', 'Volume', 10000), 0, 10000);
  Finally
    ini.free;
  End;
End;

Procedure ApplyLanguage;
Var
  Code, Filename: String;
  i: Integer;
Begin
  Code := LauncherLanguage();
  For i := 1 To ParamCount Do Begin
    If SameText(ParamStr(i), '-lazarus-ide') Then Begin
      Language.AskNameChanges := true;
      Language.DeleteUnusedItems := true;
      Code := 'en'; // Force to en!
      break;
    End;
  End;
  Filename := AppDir() + 'asset_generator_lang.';
  If Not FileExists(Filename + Code) Then Code := 'en';
  Language.CurrentLanguage := Filename + Code;
End;

Function AppDir: String;
Begin
  result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
End;

Function AssetsDir: String;
Begin
  result := AppDir() + 'assets' + PathDelim;
End;

Function SetDir(Const Folder: String): String;
Begin
  result := AssetsDir() + Folder + PathDelim;
End;

Procedure WriteLastSet(Const Folder: String);
Var
  fs: TFileStream;
  s: String;
Begin
  ForceDirectories(AssetsDir());
  s := Folder + LineEnding;
  fs := TFileStream.Create(AssetsDir() + 'last_set.txt', fmCreate);
  Try
    fs.Write(s[1], length(s));
  Finally
    fs.Free;
  End;
End;

Function FolderFromName(Const Name: String): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 1 To length(Name) Do
    Case Name[i] Of
      'a'..'z', 'A'..'Z', '0'..'9', '-', '_': result := result + Name[i];
      ' ', '.': If (result <> '') And (result[length(result)] <> '_') Then result := result + '_';
    End;
  While (result <> '') And (result[length(result)] = '_') Do
    delete(result, length(result), 1);
  If result = '' Then result := 'set';
End;

Function ImageToBitmap(Img: TAGImage; Zoom: Integer): TBitmap;
Const
  Checker: Array[0..1] Of Byte = (58, 70);
Var
  src: TAGImage;
  intf: TLazIntfImage;
  desc: TRawImageDescription;
  x, y, bg, a: Integer;
  p: PByte;
  c: TAGColor;
Begin
  If Zoom > 1 Then src := Img.ScaleNearest(Zoom) Else src := Img;
  intf := TLazIntfImage.Create(0, 0);
  Try
    desc.Init_BPP32_B8G8R8_BIO_TTB(src.Width, src.Height);
    intf.DataDescription := desc;
    For y := 0 To src.Height - 1 Do Begin
      p := intf.GetDataLineStart(y);
      For x := 0 To src.Width - 1 Do Begin
        c := src.Pixels[y * src.Width + x];
        bg := Checker[((x Div 8) + (y Div 8)) And 1];
        If (c.r = 255) And (c.g = 0) And (c.b = 255) Then
          a := 0
        Else
          a := c.a;
        p[0] := (c.b * a + bg * (255 - a)) Div 255;
        p[1] := (c.g * a + bg * (255 - a)) Div 255;
        p[2] := (c.r * a + bg * (255 - a)) Div 255;
        p[3] := 0;
        inc(p, 4);
      End;
    End;
    result := TBitmap.Create;
    result.LoadFromIntfImage(intf);
  Finally
    intf.Free;
    If src <> Img Then src.Free;
  End;
End;

{$IFDEF Windows}
Function GetForegroundWindow: HWND; stdcall; external 'user32.dll';
{$ENDIF}

(* PaintTo misses the captions of the labels *)
Procedure DrawLabels(Canvas: TCanvas; Form: TCustomForm; Parent: TWinControl);
Var
  i: Integer;
  c: TControl;
  p, q: TPoint;
  r: TRect;
Begin
  For i := 0 To Parent.ControlCount - 1 Do Begin
    c := Parent.Controls[i];
    If Not c.Visible Then Continue;
    If c Is TWinControl Then
      DrawLabels(Canvas, Form, TWinControl(c))
    Else If c Is TCustomLabel Then Begin
      p := Form.ScreenToClient(c.ClientToScreen(Point(0, 0)));
      // only the part inside the parent (scroll boxes)
      q := Form.ScreenToClient(Parent.ClientToScreen(Point(0, 0)));
      r := Rect(Max(p.x, q.x), Max(p.y, q.y), Min(p.x + c.Width, q.x + Parent.ClientWidth), Min(p.y + c.Height, q.y + Parent.ClientHeight));
      If (r.Right <= r.Left) Or (r.Bottom <= r.Top) Then Continue;
      Canvas.Font.Assign(TCustomLabel(c).Font);
      Canvas.Brush.Style := bsClear;
      Canvas.TextRect(r, p.x, p.y, TCustomLabel(c).Caption);
    End;
  End;
End;

Procedure SaveFormScreenshot(Form: TCustomForm; Const Filename: String; Extra: TScreenshotPaint);
Var
  bmp, win: TBitmap;
  png: TPortableNetworkGraphic;
  scr: TCanvas;
  i: Integer;
  p: TPoint;
  front: Boolean;
Begin
  Form.BringToFront;
  For i := 0 To 9 Do Begin
    Form.Repaint;
    Application.ProcessMessages;
    Sleep(50);
  End;
  p := Form.ClientToScreen(Point(0, 0));
  front := true;
{$IFDEF Windows}
  front := GetForegroundWindow() = Form.Handle;
{$ENDIF}
  bmp := TBitmap.Create;
  png := TPortableNetworkGraphic.Create;
  Try
    bmp.SetSize(Form.ClientWidth, Form.ClientHeight);
    If front Then Begin
      // copied from the screen, PaintTo misses the captions of the labels
      scr := TCanvas.Create;
      Try
        scr.Handle := GetDC(0);
        bmp.Canvas.CopyRect(Rect(0, 0, bmp.Width, bmp.Height), scr, Rect(p.x, p.y, p.x + bmp.Width, p.y + bmp.Height));
        ReleaseDC(0, scr.Handle);
      Finally
        scr.Free;
      End;
    End
    Else Begin
      // Windows did not bring the window to the front (another program has the focus): paint it
      win := TBitmap.Create;
      Try
        win.SetSize(bmp.Width, bmp.Height);
        Form.PaintTo(win.Canvas, Form.Left - p.x, Form.Top - p.y); // with the window frame
        bmp.Canvas.Draw(0, 0, win);
        DrawLabels(bmp.Canvas, Form, Form);
        If assigned(Extra) Then Extra(bmp.Canvas);
      Finally
        win.Free;
      End;
    End;
    png.Assign(bmp);
    png.SaveToFile(Filename);
  Finally
    png.Free;
    bmp.Free;
  End;
End;

Finalization
  GuiSetupFolders.Free;
  GuiSetupNames.Free;
End.
