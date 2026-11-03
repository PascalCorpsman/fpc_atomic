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
 * Worker threads of the UI (no LCL in here):
 *   TAGAreaWorker    renders one area (e.g. the figure or a map) into memory
 *   TAGSetWorker     writes a complete set into a folder (with progress)
 *   TAGPickerWorker  renders the key art in all three styles (style picker)
 *
 * Every worker gets its own copy of the context, the UI keeps changing its
 * own one meanwhile. OnDone and OnProgress are called in the main thread
 * (TThread.Queue), Sender is the worker. Cancel stops the worker at the next
 * file it writes (EAbort), then Aborted is true. The owner frees the worker
 * after OnDone (not inside OnDone).
 *)
Unit uag_worker;

{$MODE ObjFPC}{$H+}

Interface

Uses Classes, SysUtils, uag_context, uag_output, uag_image;

Type

  TAGAreaKind = (akFigure, akPlayer, akBomb, akFlame, akPowerups, akObjects, akMap, akScreens, akPreview, akHunted);

  TAGParamList = Array Of TAGParamInfo;

  { TAGWorker }

  TAGWorker = Class(TThread)
  private
    fOnDone, fOnProgress: TNotifyEvent;
    Procedure DoDone;
    Procedure DoProgress;
    Procedure OutputProgress(Sender: TObject; Const RelPath: String; aDone, aTotal: Integer);
  protected
    Procedure Execute; override;
    Procedure Work; virtual; abstract;
  public
    Ctx: TAGContext; // own copy, owned
    Output: TAGOutput; // owned, may be taken by the caller (set it to Nil then)
    Error: String; // '' = ok
    Aborted: Boolean;
    Done, Total: Integer; // progress, written by the worker
    Constructor Create(aCtx: TAGContext; Const OutDir: String; OnDone: TNotifyEvent; OnProgress: TNotifyEvent = Nil);
    Destructor Destroy; override;
    Procedure Cancel;
  End;

  { TAGAreaWorker }

  TAGAreaWorker = Class(TAGWorker)
  protected
    Procedure Work; override;
  public
    Kind: TAGAreaKind;
    Index: Integer; // player or map
    Params: TAGParamList; // the parameters of the area, logged while rendering
  End;

  { TAGSetWorker }

  TAGSetWorker = Class(TAGWorker)
  protected
    Procedure Work; override;
  End;

  { TAGPickerWorker }

  TAGPickerWorker = Class(TAGWorker)
  protected
    Procedure Work; override;
  public
    Images: Array[TAGStyle] Of TAGImage; // owned
    Destructor Destroy; override;
  End;

Const
  // play field of field.png like the engine (Fieldxoff, FieldyOff, FieldBlockWidth, FieldBlockHeight)
  SceneFX = 20;
  SceneFY = 66;
  SceneCW = 40;
  SceneCH = 36;
  // cells of the map scene of the editor that stay free of bricks
  SceneBombX = 4; // a ticking bomb
  SceneBombY = 4;
  SceneFigureX = 2; // the figure stands left of it
  SceneFigureY = 4;
  SceneFlameX = 10; // an explosion with flames of SceneFlameLen cells in all directions
  SceneFlameY = 6;
  SceneFlameLen = 2;

Function AreaName(Kind: TAGAreaKind; Index: Integer): String; // e.g. 'atomic/p3', 'map:Field02', the hunted figure is 'atomic'

(*
 * Field (640 x 480) with solid blocks in the usual grid and some bricks, the
 * cells of the bomb, the explosion and the figure (constants Scene...) stay free.
 * Brick and Solid are keyed (fuchsia = transparent). For the editor.
 *)
Function ComposeScene(Field, Brick, Solid: TAGImage; Seed: QWord): TAGImage;

(* Renders all images of an area into Output (use DryRun and KeepImages), maps also get the image 'scene' *)
Procedure RenderArea(Ctx: TAGContext; Output: TAGOutput; Kind: TAGAreaKind; Index: Integer);

(*
 * Deletes the generated files (png, ani, txt, sch) in Dir that are not in
 * Written (relative paths with '/'), e.g. atomic/p3/ after "own figure per
 * player" was switched off. Empty folders are removed.
 *)
Procedure RemoveStaleFiles(Const Dir: String; Written: TStrings);

Implementation

Uses uag_registry, uag_random, uag_gen_atomic, uag_gen_world, uag_preview;

Function AreaName(Kind: TAGAreaKind; Index: Integer): String;
Begin
  Case Kind Of
    akFigure: result := 'atomic';
    akPlayer: result := 'atomic/p' + IntToStr(Index);
    akBomb: result := 'bomb';
    akFlame: result := 'flame';
    akPowerups: result := 'powerups';
    akObjects: result := 'objects';
    akMap: result := format('map:Field%.2d', [Index]);
    akScreens: result := 'screens';
    akHunted: result := 'atomic'; // the base figure with a crown, same parameters
  Else
    result := 'preview';
  End;
End;

Function SceneCellFree(i, j: Integer): Boolean;
Begin
  // start corners of the players
  If ((i <= 1) And (j <= 1)) Or ((i >= 13) And (j >= 9)) Then exit(true);
  If (j = SceneBombY) And (i >= SceneFigureX) And (i <= SceneBombX) Then exit(true);
  If (j = SceneFlameY) And (abs(i - SceneFlameX) <= SceneFlameLen) Then exit(true);
  If (i = SceneFlameX) And (abs(j - SceneFlameY) <= SceneFlameLen) Then exit(true);
  result := false;
End;

Function ComposeScene(Field, Brick, Solid: TAGImage; Seed: QWord): TAGImage;
Var
  i, j: Integer;
  R: TAGRandom;
Begin
  result := Field.Clone;
  R.Init(Seed);
  For j := 0 To 10 Do
    For i := 0 To 14 Do Begin
      If odd(i) And odd(j) Then
        DrawKeyed(result, Solid, SceneFX + i * SceneCW, SceneFY + j * SceneCH)
      Else If R.Chance(0.55) And Not SceneCellFree(i, j) Then
        DrawKeyed(result, Brick, SceneFX + i * SceneCW, SceneFY + j * SceneCH);
    End;
End;

Procedure RunGenerator(Ctx: TAGContext; Output: TAGOutput; Const Name: String);
Var
  i: Integer;
Begin
  i := FindGenerator(Name);
  If i < 0 Then Raise Exception.Create('Unknown generator: ' + Name);
  GetGenerator(i).Proc(Ctx, Output);
End;

Procedure RenderArea(Ctx: TAGContext; Output: TAGOutput; Kind: TAGAreaKind; Index: Integer);
Var
  dir: String;
  field, brick, solid: TAGImage;
Begin
  Case Kind Of
    akFigure: GenerateFigure(Ctx, Output, Ctx.Area('atomic'), 'atomic/', false);
    akPlayer: GenerateFigure(Ctx, Output, Ctx.Area(AreaName(Kind, Index)), 'atomic/p' + IntToStr(Index) + '/', false);
    akBomb: RunGenerator(Ctx, Output, 'bomb');
    akFlame: RunGenerator(Ctx, Output, 'flame');
    akPowerups: RunGenerator(Ctx, Output, 'powerups');
    akObjects: RunGenerator(Ctx, Output, 'objects');
    akMap: Begin
        GenerateMap(Ctx, Output, Index);
        dir := format('maps/Field%.2d/', [Index]);
        field := Output.FindImage(dir + 'field.png');
        brick := Output.FindImage(dir + 'brick.png');
        solid := Output.FindImage(dir + 'solid.png');
        If assigned(field) And assigned(brick) And assigned(solid) Then
          Output.Images.InsertObject(0, 'scene', ComposeScene(field, brick, solid, Ctx.AreaSeed(AreaName(Kind, Index))));
      End;
    akHunted: GenerateFigure(Ctx, Output, Ctx.Area('atomic'), 'atomic/hunted/', true);
    akScreens: RunGenerator(Ctx, Output, 'screens');
    akPreview: RunGenerator(Ctx, Output, 'preview');
  End;
End;

Procedure RemoveStaleFiles(Const Dir: String; Written: TStrings);
Var
  keep: TStringList;

  Procedure Scan(Const Rel: String);
  Var
    sr: TSearchRec;
    full, ext: String;
  Begin
    full := IncludeTrailingPathDelimiter(Dir) + StringReplace(Rel, '/', PathDelim, [rfReplaceAll]);
    If FindFirst(full + '*', faAnyFile, sr) = 0 Then Begin
      Repeat
        If (sr.Name = '.') Or (sr.Name = '..') Then Continue;
        If (sr.Attr And faDirectory) <> 0 Then Begin
          Scan(Rel + sr.Name + '/');
          RemoveDir(full + sr.Name); // only if empty
        End
        Else Begin
          ext := LowerCase(ExtractFileExt(sr.Name));
          If ((ext = '.png') Or (ext = '.ani') Or (ext = '.txt') Or (ext = '.sch')) And
            (keep.IndexOf(Rel + sr.Name) < 0) Then
            DeleteFile(full + sr.Name);
        End;
      Until FindNext(sr) <> 0;
      FindClose(sr);
    End;
  End;

Begin
  keep := TStringList.Create;
  Try
    keep.CaseSensitive := {$IFDEF Windows}false{$ELSE}true{$ENDIF};
    keep.Assign(Written);
    keep.Sorted := true;
    Scan('');
  Finally
    keep.Free;
  End;
End;

{ TAGWorker }

Constructor TAGWorker.Create(aCtx: TAGContext; Const OutDir: String; OnDone: TNotifyEvent; OnProgress: TNotifyEvent);
Begin
  Inherited Create(true);
  FreeOnTerminate := false;
  Ctx := aCtx.Clone;
  Output := TAGOutput.Create(OutDir);
  Output.OnProgress := @OutputProgress;
  fOnDone := OnDone;
  fOnProgress := OnProgress;
  Error := '';
  Aborted := false;
End;

Destructor TAGWorker.Destroy;
Begin
  Cancel;
  Inherited Destroy; // waits for the thread
  RemoveQueuedEvents(self);
  Output.Free;
  Ctx.Free;
End;

Procedure TAGWorker.Cancel;
Begin
  If assigned(Output) Then Output.Cancelled := true;
End;

Procedure TAGWorker.DoDone;
Begin
  If assigned(fOnDone) Then fOnDone(self);
End;

Procedure TAGWorker.DoProgress;
Begin
  If assigned(fOnProgress) Then fOnProgress(self);
End;

Procedure TAGWorker.OutputProgress(Sender: TObject; Const RelPath: String; aDone, aTotal: Integer);
Begin
  Done := aDone;
  Total := aTotal;
  If assigned(fOnProgress) Then Queue(@DoProgress);
End;

Procedure TAGWorker.Execute;
Begin
  Try
    Work;
  Except
    On EAbort Do
      Aborted := true;
    On e: Exception Do
      Error := e.Message;
  End;
  Queue(@DoDone);
End;

{ TAGAreaWorker }

Procedure TAGAreaWorker.Work;
Var
  i, n: Integer;
  p: TAGParamInfo;
  name: String;
Begin
  Output.DryRun := true;
  Output.KeepImages := true;
  Ctx.ClearParamLog;
  RenderArea(Ctx, Output, Kind, Index);
  name := AreaName(Kind, Index);
  n := 0;
  setlength(Params, Ctx.ParamCount);
  For i := 0 To Ctx.ParamCount - 1 Do Begin
    p := Ctx.Params[i];
    If p.Area = name Then Begin
      Params[n] := p;
      inc(n);
    End;
  End;
  setlength(Params, n);
End;

{ TAGSetWorker }

Procedure TAGSetWorker.Work;
Begin
  Output.Expected := ExpectedTotal(Ctx, Nil);
  ForceDirectories(Output.Dir);
  RunGenerators(Ctx, Output, Nil);
  Ctx.SaveToFile(Output.Dir + 'profile.ini');
  RemoveStaleFiles(Output.Dir, Output.Files);
End;

{ TAGPickerWorker }

Procedure TAGPickerWorker.Work;
Var
  s: TAGStyle;
Begin
  For s := low(TAGStyle) To high(TAGStyle) Do Begin
    If Output.Cancelled Then Raise EAbort.Create('Cancelled');
    Ctx.Style := s;
    Images[s] := RenderPreview(Ctx);
  End;
End;

Destructor TAGPickerWorker.Destroy;
Var
  s: TAGStyle;
Begin
  Inherited Destroy;
  For s := low(TAGStyle) To high(TAGStyle) Do
    Images[s].Free;
End;

End.
