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

  TAGAreaKind = (akFigure, akPlayer, akBomb, akFlame, akPowerups, akObjects, akMap, akScreens, akPreview);

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

Function AreaName(Kind: TAGAreaKind; Index: Integer): String; // e.g. 'atomic/p3', 'map:Field02'

(* Renders all images of an area into Output (use DryRun and KeepImages), maps also get the image 'scene' *)
Procedure RenderArea(Ctx: TAGContext; Output: TAGOutput; Kind: TAGAreaKind; Index: Integer);

(*
 * Deletes the generated files (png, ani, txt, sch) in Dir that are not in
 * Written (relative paths with '/'), e.g. atomic/p3/ after "own figure per
 * player" was switched off. Empty folders are removed.
 *)
Procedure RemoveStaleFiles(Const Dir: String; Written: TStrings);

Implementation

Uses uag_registry, uag_gen_atomic, uag_gen_world, uag_preview;

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
  Else
    result := 'preview';
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
          Output.Images.InsertObject(0, 'scene', ComposeMapScene(field, brick, solid, Ctx.AreaSeed(AreaName(Kind, Index))));
      End;
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
