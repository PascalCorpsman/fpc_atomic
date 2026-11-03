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
Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ulauncher, usynapsedownloader;

Type

  TUpdateStatus = (
    usNew, // Datei fehlt
    usChanged, // Datei weicht vom Release ab
    usCurrent, // Datei ist aktuell
    usPackage, // Zip, wird ohne Prüfung eingespielt
    usLauncherNew, // Online gibt es einen neueren Launcher
    usLauncherCurrent // Launcher ist aktuell
    );

  TUpdateItem = Record
    FileContainer: TFile; // URL schon an die Plattform angepasst
    Name: String; // Name in der Liste
    Target: String; // Wohin der Download gespeichert wird
    Hash: String; // Erwarteter MD5 der Plattform
    Size: Int64; // Größe der Plattform
    Status: TUpdateStatus;
    Checked: Boolean;
  End;

  TUpdatePage = (upCheck, upOverview, upProgress, upResult);

  { TForm3 }

  TForm3 = Class(TForm)
    ButtonAll: TButton;
    ButtonClose: TButton;
    ButtonExtractor: TButton;
    ButtonMain: TButton;
    ButtonNone: TButton;
    CheckBoxAutoCheck: TCheckBox;
    CheckBoxShowAll: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelCheck: TLabel;
    LabelCheckHint: TLabel;
    LabelFile: TLabel;
    LabelLauncher: TLabel;
    LabelResult: TLabel;
    LabelResultHint: TLabel;
    LabelTotal: TLabel;
    LabelVersion: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    MemoCheck: TMemo;
    MemoLog: TMemo;
    MemoResult: TMemo;
    Notebook1: TNotebook;
    PageCheck: TPage;
    PageOverview: TPage;
    PageProgress: TPage;
    PageResult: TPage;
    PanelButtons: TPanel;
    PanelExtractor: TPanel;
    PanelList: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    ProgressCheck: TProgressBar;
    Procedure ButtonAllClick(Sender: TObject);
    Procedure ButtonCloseClick(Sender: TObject);
    Procedure ButtonExtractorClick(Sender: TObject);
    Procedure ButtonMainClick(Sender: TObject);
    Procedure ButtonNoneClick(Sender: TObject);
    Procedure CheckBoxAutoCheckChange(Sender: TObject);
    Procedure CheckBoxShowAllChange(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure ListView1ItemChecked(Sender: TObject; Item: TListItem);
  private
    fVersion: TVersion; // Die Versionsinfo von online
    fLocalVersion: Single; // -1 = nicht installiert
    fItems: Array Of TUpdateItem;
    fFilling: Boolean; // ListView1 wird befüllt -> OnItemChecked ignorieren
    fCheckFile: String; // Versionsinfo der laufenden Prüfung, Antworten älterer Prüfungen werden ignoriert
    fRunning: Boolean; // Downloads laufen
    fCancel: Boolean; // Der Benutzer will die Downloads abbrechen
    fDownloader: TSynapesDownloader; // Der laufende Download
    fFailed: Array Of integer; // Index in fItems der fehlgeschlagenen Downloads
    fFailedReason: Array Of String; // Warum der Download in fFailed fehlgeschlagen ist
    fLauncherFile: String; // Der heruntergeladene neue Launcher, '' = keiner
    fDone, fTotal: Int64; // Bytes der Downloads
    fUpToDate: Boolean;
    Procedure ShowPage(Page: TUpdatePage);
    Procedure StartCheck();
    Procedure OnCheckDone(Ok: Boolean; Const Filename, ErrorText: String);
    Procedure ShowCheckError(Const ErrorText: String);
    Procedure AddItem(aFile: TFile; Force: Boolean);
    Procedure FillItems();
    Procedure FillList();
    Procedure ShowOverview();
    Procedure UpdateMainButton();
    Function ItemShown(Const Item: TUpdateItem): Boolean;
    Function StatusText(Status: TUpdateStatus): String;
    Procedure RunUpdate(Const Indices: Array Of integer);
    Function DownloadItem(Index: integer; Out ErrorText: String): Boolean;
    Function MergeZip(Const Item: TUpdateItem; Out ErrorText: String): Boolean;
    Procedure ShowResult(OkCount, Count: integer);
    Procedure KeepOrder(Const Order: Array Of TControl);
    Procedure TriggerUpdater(Executable: String);
    Procedure OnFileDownloadUpdateEvent(Sender: TObject; aSize, aTotalSize: Int64);
  public
    (*
     * Prüft auf Updates und zeigt den Dialog (modal), LocalVersion = -1 -> nicht installiert
     *)
    Procedure Execute(LocalVersion: Single);
    Procedure AddLog(Const aText: String);
    Property UpToDate: Boolean read fUpToDate; // True, wenn die letzte Prüfung nichts zum Aktualisieren gefunden hat
  End;

Var
  Form3: TForm3;

Implementation

Uses Math, lazfileutils, md5, Zipper, FileUtil, process, UTF8Process, Unit1, uatomic_global, ulanguage, uupdatecheck;

{$R *.lfm}

Const
  ColorOk = clGreen;
  ColorWarning = $00007FD0; // Orange
  ColorError = clRed;

Function VersionToString(Value: Single): String;
Var
  fm: TFormatSettings;
Begin
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  result := format('%0.5f', [Value], fm);
End;

Function LauncherVersionToString(Value: integer): String;
Var
  fm: TFormatSettings;
Begin
  fm := DefaultFormatSettings;
  fm.DecimalSeparator := '.';
  result := format('%0.2f', [Value / 100], fm);
End;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Update overview';
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  fVersion := TVersion.Create();
  fItems := Nil;
  fFailed := Nil;
  fRunning := false;
  fDownloader := Nil;
  fCheckFile := '';
End;

Procedure TForm3.FormDestroy(Sender: TObject);
Begin
  fVersion.free;
End;

Procedure TForm3.Execute(LocalVersion: Single);
Begin
  fLocalVersion := LocalVersion;
  fUpToDate := false;
  MemoLog.Clear;
  ListView1.Column[0].Caption := R_ColumnFile;
  ListView1.Column[1].Caption := R_ColumnStatus;
  ListView1.Column[2].Caption := R_ColumnSize;
  StartCheck();
  ShowModal;
End;

Procedure TForm3.AddLog(Const aText: String);
Begin
  MemoLog.Lines.Append(aText);
End;

Procedure TForm3.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  // Während der Downloads heißt Schließen Abbrechen, der Dialog bleibt für das Ergebnis offen
  If fRunning Then Begin
    ButtonCloseClick(Nil);
    CanClose := false;
  End;
End;

Procedure TForm3.ButtonCloseClick(Sender: TObject);
Begin
  If fRunning Then Begin
    fCancel := true;
    If assigned(fDownloader) Then fDownloader.Abort := true;
    ButtonClose.Enabled := false;
  End
  Else Begin
    Close;
  End;
End;

Procedure TForm3.ShowPage(Page: TUpdatePage);
Begin
  Notebook1.PageIndex := ord(Page);
  ButtonClose.Enabled := true;
  If Page = upProgress Then Begin
    ButtonClose.Caption := R_Cancel;
  End
  Else Begin
    ButtonClose.Caption := R_Close;
  End;
  ButtonMain.Visible := false; // Die Seiten zeigen ihn, wenn sie ihn brauchen
End;

Procedure TForm3.KeepOrder(Const Order: Array Of TControl);
Var
  i: integer;
Begin
  (*
   * Wieder eingeblendete alTop Controls werden nach ihrem Top einsortiert, das sich
   * beim Ausblenden geändert haben kann -> die geplante Reihenfolge wieder herstellen.
   *)
  If length(Order) = 0 Then exit;
  Order[0].Parent.DisableAlign;
  Try
    For i := 0 To high(Order) Do Begin
      Order[i].Top := i;
    End;
  Finally
    Order[0].Parent.EnableAlign;
  End;
End;

(*
 * Phase 1: Prüfen
 *)

Procedure TForm3.StartCheck;
Begin
  ShowPage(upCheck);
  LabelCheck.Caption := R_UpdateChecking;
  LabelCheck.Font.Color := clDefault;
  LabelCheckHint.Caption := '';
  ProgressCheck.Visible := true;
  MemoCheck.Visible := false;
  KeepOrder([LabelCheck, ProgressCheck, LabelCheckHint]);
  // Jede Prüfung bekommt einen neuen Namen, damit eine späte Antwort einer älteren ignoriert wird
  fCheckFile := GetUpdateTempFolder() + format('fpc_atomic_version_%d.json', [GetTickCount64()]);
  TUpdateCheckThread.Create(fCheckFile, @OnCheckDone);
End;

Procedure TForm3.OnCheckDone(Ok: Boolean; Const Filename, ErrorText: String);
Var
  s: String;
Begin
  If (Not Visible) Or (Filename <> fCheckFile) Then Begin
    DeleteFile(Filename);
    exit;
  End;
  s := ErrorText;
  If Ok Then Begin
    Ok := fVersion.LoadFromFile(Filename);
    If Not Ok Then s := R_UpdateInvalidData;
  End;
  DeleteFile(Filename);
  If Ok Then Begin
    FillItems();
    ShowOverview();
  End
  Else Begin
    ShowCheckError(s);
  End;
End;

Procedure TForm3.ShowCheckError(Const ErrorText: String);
Begin
  LabelCheck.Caption := R_UpdateCheckFailed;
  LabelCheck.Font.Color := ColorError;
  ProgressCheck.Visible := false;
{$IFDEF Linux}
  LabelCheckHint.Caption := R_UpdateSslHint;
{$ENDIF}
  MemoCheck.Text := trim(ErrorText + LineEnding + LineEnding + MemoLog.Text);
  MemoCheck.Visible := true;
  KeepOrder([LabelCheck, ProgressCheck, LabelCheckHint]);
  ButtonMain.Caption := R_TryAgain;
  ButtonMain.Visible := true;
End;

(*
 * Phase 2: Übersicht
 *)

Procedure TForm3.AddItem(aFile: TFile; Force: Boolean);
Var
  Item: TUpdateItem;
  fn: String;
Begin
  fn := aFile.Filename;
{$IFDEF Windows}
  fn := StringReplace(fn, '/', PathDelim, [rfReplaceAll]);
{$ENDIF}
  Item.Hash := aFile.Hash;
  Item.Size := aFile.Size;
{$IFDEF Linux}
  If aFile.Kind = fkExecutable Then Begin
    fn := ExtractFileNameWithoutExt(fn);
    aFile.URL := copy(aFile.URL, 1, length(aFile.URL) - length('.exe'));
    Item.Hash := aFile.Hash2;
    Item.Size := aFile.Size2;
  End;
  If aFile.Kind = fkLib Then Begin
    fn := Change_DLL_To_lib_so(fn);
    aFile.URL := Change_DLL_To_lib_so(aFile.URL);
    Item.Hash := aFile.Hash2;
    Item.Size := aFile.Size2;
  End;
{$ENDIF}
  Item.FileContainer := aFile;
  Item.Status := usCurrent;
  Item.Checked := false;
  Item.Name := fn;
  Item.Target := fn;
  Case aFile.Kind Of
    fkZip: Begin
        Item.Name := aFile.Description;
        Item.Target := GetUpdateTempFolder() + ExtractFileName(aFile.URL);
        Item.Hash := '';
        Item.Status := usPackage;
        Item.Checked := Force;
      End;
    fkExecutable: If fn = ExtractFileName(ParamStr(0)) Then Begin
        // Den laufenden Launcher kann nur der Updater ersetzen
        Item.Target := GetUpdateTempFolder() + fn;
        If fVersion.LauncherVersion > LauncherVersion Then Begin
          Item.Status := usLauncherNew;
        End
        Else Begin
          Item.Status := usLauncherCurrent;
        End;
        Item.Checked := Item.Status = usLauncherNew;
      End;
  End;
  If Not (Item.Status In [usPackage, usLauncherNew, usLauncherCurrent]) Then Begin
    // Dateien mit Größe -1 gibt es für diese Plattform nicht
    If Item.Size = -1 Then exit;
    If Not FileExists(fn) Then Begin
      Item.Status := usNew;
    End
    Else If lowercase(Item.Hash) <> lowercase(MD5Print(MD5File(fn))) Then Begin
      Item.Status := usChanged;
    End
    Else Begin
      Item.Status := usCurrent;
    End;
    Item.Checked := Item.Status <> usCurrent;
  End;
  setlength(fItems, high(fItems) + 2);
  fItems[high(fItems)] := Item;
End;

Procedure TForm3.FillItems;
Var
  i: integer;
Begin
  setlength(fItems, 0);
  For i := 0 To fVersion.DownloadBaseCount - 1 Do Begin
    AddItem(fVersion.DownloadBase[i], false);
  End;
  // Ohne Installation werden auch die zusätzlichen Pakete gebraucht
  For i := 0 To fVersion.DownloadCount - 1 Do Begin
    AddItem(fVersion.Download[i], fLocalVersion = -1);
  End;
End;

Function TForm3.ItemShown(Const Item: TUpdateItem): Boolean;
Begin
  result := CheckBoxShowAll.Checked Or (Item.Status In [usNew, usChanged, usPackage, usLauncherNew]);
End;

Function TForm3.StatusText(Status: TUpdateStatus): String;
Begin
  Case Status Of
    usNew: result := R_StatusNew;
    usChanged: result := R_StatusChanged;
    usCurrent, usLauncherCurrent: result := R_StatusCurrent;
    usPackage: result := R_StatusPackage;
    usLauncherNew: result := R_StatusLauncherNew;
  End;
End;

Procedure TForm3.FillList;
Var
  i: integer;
  li: TListItem;
Begin
  fFilling := true;
  ListView1.Items.BeginUpdate;
  Try
    ListView1.Items.Clear;
    For i := 0 To high(fItems) Do Begin
      If Not ItemShown(fItems[i]) Then Continue;
      li := ListView1.Items.Add;
      li.Caption := fItems[i].Name;
      li.SubItems.Add(StatusText(fItems[i].Status));
      li.SubItems.Add(FileSizeToString(fItems[i].Size));
      li.Checked := fItems[i].Checked;
      li.Data := Pointer(PtrUInt(i));
    End;
  Finally
    ListView1.Items.EndUpdate;
    fFilling := false;
  End;
  UpdateMainButton();
End;

Procedure TForm3.ShowOverview;
Var
  i, Needed: integer;
Begin
  ShowPage(upOverview);
  Needed := 0;
  For i := 0 To high(fItems) Do Begin
    If fItems[i].Status In [usNew, usChanged, usLauncherNew] Then inc(Needed);
  End;
  LabelVersion.Font.Color := clDefault;
  If fLocalVersion = -1 Then Begin
    LabelVersion.Caption := format(RF_UpdateNotInstalled, [VersionToString(fVersion.Version)]);
  End
  Else If fVersion.Version > fLocalVersion + 0.000001 Then Begin
    LabelVersion.Caption := format(RF_UpdateNewVersion, [VersionToString(fVersion.Version), VersionToString(fLocalVersion)]);
  End
  Else If Needed = 0 Then Begin
    LabelVersion.Caption := format(RF_UpdateCurrent, [VersionToString(fLocalVersion)]);
    LabelVersion.Font.Color := ColorOk;
  End
  Else If Needed = 1 Then Begin
    LabelVersion.Caption := format(RF_UpdateFileDiffers, [VersionToString(fLocalVersion)]);
  End
  Else Begin
    LabelVersion.Caption := format(RF_UpdateFilesDiffer, [VersionToString(fLocalVersion), Needed]);
  End;
  fUpToDate := Needed = 0;
  LabelLauncher.Caption := format(RF_LauncherVersions, [LauncherVersionToString(LauncherVersion), LauncherVersionToString(fVersion.LauncherVersion)]);
  memo1.Text := format('fpc_atomic ver. %s', [VersionToString(fVersion.Version)]) + LineEnding + LineEnding + fVersion.VersionText;
  CheckBoxShowAll.Checked := false;
  CheckBoxAutoCheck.Checked := Form1.CheckUpdatesOnStart;
  FillList();
End;

Procedure TForm3.UpdateMainButton;
Var
  i, Count: integer;
  Size: Int64;
Begin
  Count := 0;
  Size := 0;
  For i := 0 To high(fItems) Do Begin
    If fItems[i].Checked Then Begin
      inc(Count);
      Size := Size + fItems[i].Size;
    End;
  End;
  If Count = 1 Then Begin
    ButtonMain.Caption := format(RF_UpdateButtonOne, [FileSizeToString(Size)]);
  End
  Else Begin
    ButtonMain.Caption := format(RF_UpdateButton, [Count, FileSizeToString(Size)]);
  End;
  ButtonMain.Visible := Count <> 0;
End;

Procedure TForm3.ListView1ItemChecked(Sender: TObject; Item: TListItem);
Begin
  If fFilling Or (Not assigned(Item)) Then exit;
  fItems[PtrUInt(Item.Data)].Checked := Item.Checked;
  UpdateMainButton();
End;

Procedure TForm3.CheckBoxShowAllChange(Sender: TObject);
Var
  i: integer;
Begin
  // Ausgeblendete Dateien werden nicht heruntergeladen
  If Not CheckBoxShowAll.Checked Then Begin
    For i := 0 To high(fItems) Do Begin
      If Not ItemShown(fItems[i]) Then fItems[i].Checked := false;
    End;
  End;
  FillList();
End;

Procedure TForm3.CheckBoxAutoCheckChange(Sender: TObject);
Begin
  Form1.CheckUpdatesOnStart := CheckBoxAutoCheck.Checked;
End;

Procedure TForm3.ButtonAllClick(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To high(fItems) Do Begin
    If ItemShown(fItems[i]) Then fItems[i].Checked := true;
  End;
  FillList();
End;

Procedure TForm3.ButtonNoneClick(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To high(fItems) Do Begin
    fItems[i].Checked := false;
  End;
  FillList();
End;

Procedure TForm3.ButtonMainClick(Sender: TObject);
Var
  Indices: Array Of integer;
  i: integer;
  Updater: String;
  LauncherSelected: Boolean;
Begin
  Updater := 'updater'{$IFDEF Windows} + '.exe'{$ENDIF};
  Case TUpdatePage(Notebook1.PageIndex) Of
    upCheck: StartCheck();
    upOverview: Begin
        LauncherSelected := false;
        For i := 0 To high(fItems) Do Begin
          If fItems[i].Checked And (fItems[i].Status In [usLauncherNew, usLauncherCurrent]) Then Begin
            LauncherSelected := true;
            break;
          End;
        End;
        If LauncherSelected Then Begin
          For i := 0 To high(fItems) Do Begin
            If (fItems[i].FileContainer.Kind = fkExecutable) And
              (fItems[i].Status In [usNew, usChanged]) And
              SameText(ExtractFileName(fItems[i].Target), Updater) Then Begin
              fItems[i].Checked := true;
              break;
            End;
          End;
        End;
        Indices := Nil;
        For i := 0 To high(fItems) Do Begin
          If fItems[i].Checked Then Begin
            setlength(Indices, high(Indices) + 2);
            Indices[high(Indices)] := i;
          End;
        End;
        RunUpdate(Indices);
      End;
    upResult: Begin
        If length(fFailed) <> 0 Then Begin
          RunUpdate(fFailed);
        End
        Else If fLauncherFile <> '' Then Begin
          If Not FileExists(Updater) Then Begin
            showmessage(format(RF_UpdaterMissing, [Updater]));
            exit;
          End;
          // So schnell wie möglich raus aus der Anwendung
          TriggerUpdater(fLauncherFile);
          Application.Terminate;
        End;
      End;
  End;
End;

(*
 * Phase 3: Downloads
 *)

Procedure TForm3.RunUpdate(Const Indices: Array Of integer);
Var
  Order: Array Of integer;
  i, n, OkCount: integer;
  Err: String;
Begin
  // Den Launcher ersetzt der Updater, daher kommt er zuletzt
  Order := Nil;
  setlength(Order, length(Indices));
  n := 0;
  For i := 0 To high(Indices) Do Begin
    If fItems[Indices[i]].Status In [usLauncherNew, usLauncherCurrent] Then Continue;
    Order[n] := Indices[i];
    inc(n);
  End;
  For i := 0 To high(Indices) Do Begin
    If Not (fItems[Indices[i]].Status In [usLauncherNew, usLauncherCurrent]) Then Continue;
    Order[n] := Indices[i];
    inc(n);
  End;
  fTotal := 0;
  For i := 0 To high(Order) Do Begin
    fTotal := fTotal + fItems[Order[i]].Size;
  End;
  fDone := 0;
  fCancel := false;
  fFailed := Nil;
  fFailedReason := Nil;
  fLauncherFile := '';
  OkCount := 0;
  MemoLog.Clear;
  ShowPage(upProgress);
  ProgressBar1.Position := 0;
  ProgressBar2.Max := Max(1, fTotal Div 1024);
  ProgressBar2.Position := 0;
  LabelTotal.Caption := format(RF_ProgressTotal, [FileSizeToString(0), FileSizeToString(fTotal)]);
  fRunning := true;
  Try
    For i := 0 To high(Order) Do Begin
      If fCancel Then break;
      LabelFile.Caption := format(RF_ProgressFile, [i + 1, length(Order), fItems[Order[i]].Name]);
      ProgressBar1.Position := 0;
      Application.ProcessMessages;
      If DownloadItem(Order[i], Err) Then Begin
        inc(OkCount);
      End
      Else Begin
        If Not fCancel Then Begin
          setlength(fFailed, high(fFailed) + 2);
          fFailed[high(fFailed)] := Order[i];
          setlength(fFailedReason, length(fFailed));
          fFailedReason[high(fFailedReason)] := Err;
        End;
        log('Error, ' + fItems[Order[i]].Name + ': ' + Err);
      End;
      fDone := fDone + fItems[Order[i]].Size;
      ProgressBar2.Position := fDone Div 1024;
    End;
  Finally
    fRunning := false;
  End;
  ShowResult(OkCount, length(Order));
End;

Procedure TForm3.OnFileDownloadUpdateEvent(Sender: TObject; aSize,
  aTotalSize: Int64);
Begin
  ProgressBar1.Max := Max(1, aTotalSize Div 1024);
  ProgressBar1.Position := aSize Div 1024;
  ProgressBar2.Position := Min(ProgressBar2.Max, (fDone + aSize) Div 1024);
  LabelTotal.Caption := format(RF_ProgressTotal, [FileSizeToString(fDone + aSize), FileSizeToString(fTotal)]);
  If fCancel Then (Sender As TSynapesDownloader).Abort := true;
  Application.ProcessMessages;
End;

Function TForm3.DownloadItem(Index: integer; Out ErrorText: String): Boolean;
Var
  Item: TUpdateItem;
  Part: String;
  dl: TSynapesDownloader;
{$IFDEF Linux}
  pr: TProcessUTF8;
{$ENDIF}
Begin
  result := false;
  ErrorText := '';
  Item := fItems[Index];
  (*
   * Download nach <Datei>.part, das Original wird erst ersetzt, wenn der Download vollständig ist,
   * ein fehlgeschlagener oder abgebrochener Download lässt die Installation wie sie ist.
   *)
  Part := Item.Target + '.part';
  dl := TSynapesDownloader.Create;
  dl.OnFileDownloadUpdateEvent := @OnFileDownloadUpdateEvent;
  fDownloader := dl;
  Try
    Try
      result := dl.DownloadFile(Item.FileContainer.URL, Part);
      If Not result Then ErrorText := R_NoDataReceived;
    Except
      On e: Exception Do Begin
        result := false;
        ErrorText := e.Message;
      End;
    End;
  Finally
    fDownloader := Nil;
    dl.free;
  End;
  If fCancel Then Begin
    result := false;
    ErrorText := R_Cancelled;
  End;
  If result And (Item.Hash <> '') And (pos('/releases/download/', Item.FileContainer.URL) <> 0) Then Begin
    // Release Dateien müssen zum Hash passen (Dateien aus "main" können neuer als die Versionsinfo sein)
    If lowercase(MD5Print(MD5File(Part))) <> lowercase(Item.Hash) Then Begin
      result := false;
      ErrorText := R_ChecksumMismatch;
    End;
  End;
  If Not result Then Begin
    If FileExists(Part) Then DeleteFile(Part);
    exit;
  End;
  // Das Original ersetzen
  If FileExists(Item.Target) And (Not DeleteFile(Item.Target)) Then Begin
    DeleteFile(Part);
    ErrorText := R_ReplaceFailed;
    result := false;
    exit;
  End;
  If Not RenameFile(Part, Item.Target) Then Begin
    DeleteFile(Part);
    ErrorText := R_ReplaceFailed;
    result := false;
    exit;
  End;
  Case Item.FileContainer.Kind Of
    fkExecutable, fkScript: Begin
{$IFDEF LINUX}
        pr := TProcessUTF8.Create(Nil);
        pr.Options := [poWaitOnExit];
        pr.CurrentDirectory := GetCurrentDir;
        pr.Executable := 'chmod';
        pr.Parameters.Add('+x');
        pr.Parameters.Add(Item.Target);
        pr.Execute;
        pr.free;
{$ENDIF}
        // Die Locale Versionsanzeige "umbiegen", falls jemand 2 mal Check for update clickt ;)
        If lowercase(ExtractFileName(Item.Target)) = 'fpc_atomic'{$IFDEF Windows} + '.exe'{$ENDIF} Then Begin
          Form1.StoreVersion(fVersion.Version);
        End;
      End;
    fkZip: result := MergeZip(Item, ErrorText);
  End;
  If Item.Status In [usLauncherNew, usLauncherCurrent] Then Begin
    fLauncherFile := Item.Target;
  End;
End;

Function TForm3.MergeZip(Const Item: TUpdateItem; Out ErrorText: String
  ): Boolean;
Var
  UnZipper: TUnZipper;
  newRoot, root, source, target, TargetDir: String;
  sl: TStringList;
  i: Integer;
Begin
  result := true;
  ErrorText := '';
  UnZipper := TUnZipper.Create;
  Try
    UnZipper.FileName := Item.Target;
    If Item.FileContainer.InFileOffset = '' Then Begin
      UnZipper.OutputPath := ExtractFilePath(ParamStr(0));
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
    End
    Else Begin
      UnZipper.OutputPath := GetUpdateTempFolder();
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
      root := GetUpdateTempFolder() + Item.FileContainer.InFileOffset;
      sl := FindAllFiles(root);
      root := root + PathDelim;
      newRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
      For i := 0 To sl.count - 1 Do Begin
        source := sl[i];
        target := StringReplace(sl[i], root, newRoot, []);
        TargetDir := ExtractFilePath(target);
        If TargetDir <> '' Then Begin
          ForceDirectories(TargetDir); // Braucht keine Fehlermeldung, weil die unten durch Copyfile auch gemacht wird ;)
        End;
        If Not CopyFile(source, target) Then Begin
          log('Error, could not create: ' + target);
          result := false;
          ErrorText := R_WriteFailed + ' ' + target;
        End;
      End;
      sl.free;
    End;
  Except
    On av: exception Do Begin
      result := false;
      ErrorText := av.Message;
    End;
  End;
  UnZipper.Free;
End;

Procedure TForm3.TriggerUpdater(Executable: String);
Var
  p: TProcessUTF8;
Begin
  p := TProcessUTF8.Create(Nil);
{$IFDEF Windows}
  p.Executable := 'updater.exe';
{$ENDIF}
{$IFDEF LINUX}
  p.Executable := 'updater';
{$ENDIF}
  p.Parameters.add(Executable);
  p.Execute; // Ab jetzt heist es so schnell wie möglich raus aus der Anwendung
  p.free;
End;

(*
 * Phase 4: Ergebnis
 *)

Procedure TForm3.ShowResult(OkCount, Count: integer);
Var
  i: integer;
  NextHint: String;
  Missing: TStringList;
Begin
  ShowPage(upResult);
  If fCancel Then Begin
    LabelResult.Caption := format(RF_ResultCancelled, [OkCount, Count]);
    LabelResult.Font.Color := ColorWarning;
  End
  Else If length(fFailed) = 0 Then Begin
    If OkCount = 1 Then Begin
      LabelResult.Caption := R_ResultOkOne;
    End
    Else Begin
      LabelResult.Caption := format(RF_ResultOk, [OkCount]);
    End;
    LabelResult.Font.Color := ColorOk;
  End
  Else Begin
    LabelResult.Caption := format(RF_ResultFailed, [length(fFailed), Count]);
    LabelResult.Font.Color := ColorError;
  End;
  // Erst die fehlgeschlagenen Dateien, dann das Log
  MemoResult.Clear;
  For i := 0 To high(fFailed) Do Begin
    MemoResult.Lines.Append('✗ ' + fItems[fFailed[i]].Name + ': ' + fFailedReason[i]);
  End;
  If length(fFailed) <> 0 Then MemoResult.Lines.Append('');
  MemoResult.Lines.AddStrings(MemoLog.Lines);
  // Was als nächstes zu tun ist
  NextHint := '';
  Missing := CheckForFiles();
  PanelExtractor.Visible := Missing.Count <> 0;
  If Missing.Count <> 0 Then NextHint := R_ResultGameDataMissing;
  Missing.free;
  If fLauncherFile <> '' Then NextHint := trim(NextHint + LineEnding + R_ResultLauncherRestart);
  LabelResultHint.Caption := NextHint;
  KeepOrder([LabelResult, LabelResultHint, PanelExtractor, Label3]);
  If length(fFailed) <> 0 Then Begin
    ButtonMain.Caption := R_RetryFailed;
    ButtonMain.Visible := true;
  End
  Else If fLauncherFile <> '' Then Begin
    ButtonMain.Caption := R_RestartLauncher;
    ButtonMain.Visible := true;
  End;
  If ButtonMain.Visible Then Begin
    ButtonMain.SetFocus;
  End
  Else Begin
    ButtonClose.SetFocus;
  End;
End;

Procedure TForm3.ButtonExtractorClick(Sender: TObject);
Var
  Missing: TStringList;
Begin
  Form1.Button4Click(Nil);
  Missing := CheckForFiles();
  PanelExtractor.Visible := Missing.Count <> 0;
  If Missing.Count = 0 Then Begin
    LabelResultHint.Caption := trim(StringReplace(LabelResultHint.Caption, R_ResultGameDataMissing, '', []));
  End;
  Missing.free;
  KeepOrder([LabelResult, LabelResultHint, PanelExtractor, Label3]);
End;

End.

