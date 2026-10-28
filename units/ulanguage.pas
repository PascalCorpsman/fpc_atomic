(******************************************************************************)
(* ulanguage.pas                                                   26.10.2012 *)
(*                                                                            *)
(* Version : 0.07                                                             *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description :                                                              *)
(* Original taken from here :                                                 *)
(* http://scrabble.svn.sourceforge.net/viewvc/scrabble/ulanguage.pas          *)
(*                                                                            *)
(* modified by Corpsman for personal use.                                     *)
(* New Features :                                                             *)
(*               - AskNameChanges                                             *)
(*               - Editor                                                     *)
(*               - automatical removing from no more used entries             *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Unterstützung mehrer Formulare, durch Speicherung des *)
(*                      Formular namens.                                      *)
(*               0.03 - Hinzufügen der "Filter" von Dialogen                  *)
(*                      Entfernen der Info Eigenschaften                      *)
(*               0.04 - Hinzufügen TCombobox                                  *)
(*               0.05 - Bugfix Attribut Hint, Filter wurden nicht richtig     *)
(*                      gelesen                                               *)
(*               0.06 - Bugfix, Erlauben von UTF8 Zeichen in der Sprache      *)
(*               0.07 - Einführen Ignore Ident/ Ignore Comp                   *)
(*                      Bugfix löschen der ungenutzten Einträge hat zu viel   *)
(*                      gelöscht.                                             *)
(*                      Anpassungen für Multiplattform                        *)
(*                                                                            *)
(******************************************************************************)
Unit ulanguage;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Inifiles, ExtCtrls, Typinfo, CheckLst, Grids,
  lclproc, stdctrls, Dialogs, FileUtil;

Type

  { TLanguage }

  TLanguage = Class
  private
    fIgnored: Array Of String; // Die Liste der zu ignorierenden Sprachelemente
    fIgnoredComp: Array Of TComponent;
    FCurrentLanguage: String; // Die Aktuell Geladene Sprache
    Procedure SetLanguage(Const aValue: String); // Setzt die Aktuelle Sprache auf aValue und passt alle Komponenten entsprechend an
    Function GetLanguage: String; // Gibt die Sprache wieder zurück.
    Function HasProperty(comp: TComponent; prop: String): boolean; // Prüft ob comp das Feld prop hat ( z.B. Caption .. )
    Function GetProp(Comp: TComponent; Prop: String): String; // Ermittelt den Wert des Feldes Prop von Comp
    Procedure SetProp(comp: TComponent; Const prop, value: String); // Setzt den Wert des Feldes Prop auf Value
    Procedure DeleteAllUnusedItems(Const UsedList: TStringList; LanguageFile: String); // Löscht alle nicht in UsedList enthaltenen Einträge
    Function IndentisIgnored(indent: String): Boolean; // True, wenn der Eintrag auf der Ignore liste ist
    Function CompIsIgnored(Comp: TComponent): Boolean; // True, wenn die Componente auf der ignore Liste steht
  public
    AskNameChanges: boolean; // Dieser Switch zeigt beim Laden einer Sprache an, ob der Gespeicherte Wert <> dem Eingegebenen Wert (zur Designtime) ist, und wenn ja kann man sich aussuchen welchen man neu gespeichert haben will.
    Constructor Create;
    Destructor Destroy; override;
    // Achtung, es werden nur die Berücksichtigt, welche beim Aufruf bereits in TApplication
    // registriert sind (d.h. Im OnCreate ists ungünstig wenn Mehrere Formulare verwendet sind)
    // OnActivate ist dann das bessere.
    Property CurrentLanguage: String read GetLanguage write SetLanguage;

    // Fügt der Liste der Einträge einen hinzu, welcher Komplett ignoriert werden soll (typischerweise die form1.caption)
    Procedure AddIgnoredIdentifier(Indent: String);
    Procedure AddIgnoredComponent(Comp: TComponent);
  End;

Var
  Language: TLanguage;

Function Get_System_Default_Language(): String; // Gibt die Default Sprache des Aktuellen Systems wieder ( Achtung Merkwürdig Codiert )
Function decode(value: String): String; // "Entschlüsselt" einen Ini-String
Function encode(value: String): String; // "Verschlüsselt" einen Ini-String

(*
 * Durch das Einbinden via Include File, kann die ulanguage.pas eine "Lib" bleiben, und jede Anwendung kann ihre eigenen Ressourcenstrings deklarieren
 *
 * Kommt beim Compilieren hier ein Fehler, dann einfach im Verzeichniss der Anwendung die Datei "ressourcestrings.include" erstellen und leer lassen.
 *
 * Damit die Ressource Strings genutzt werden können, muss ulanguage in der entsprechenden Unit eingebunden ( Ressource Strings aus anderen Units werden nicht berücksichtigt/ übersetzt)
 *)
{$I ressourcestrings.inc}

Implementation

Uses lazutf8, Translations;

Function Get_System_Default_Language(): String;
Var
  Lang: TLanguageID;
Begin
  Lang := GetLanguageID();
  result := Lang.LanguageCode;
End;

(*
 * Decodes a single line string to a multiline string
 *)

Function decode(value: String): String;
Var
  i: integer;
Begin
  If length(value) > 1 Then Begin
    If (value[1] = '"') And (value[length(value)] = '"') Then Begin
      value := copy(value, 2, length(value) - 2);
    End;
  End;
  i := 1;
  While i <= length(value) Do Begin
    If value[i] = '~' Then Begin
      Case value[i + 1] Of
        '~': Begin // Den Doppelten einfach Löschen
            delete(value, i + 1, 1);
          End;
        '0': Begin
{$IFDEF WINDOWS }
            value[i] := LineEnding[1];
            value[i + 1] := LineEnding[2];
{$ELSE}
            value[i] := LineEnding;
            delete(value, i + 1, 1);
{$ENDIF}
          End;
      End;
    End;
    inc(i);
  End;
  result := value;
End;

(*
 * Encodes a multiline string to a single line string
 *)

Function encode(value: String): String;
Var
  i: integer;
Begin
  i := 1;
  While i <= length(value) Do Begin
    // Das Steuerzeichentoken wird verdoppelt
    If value[i] = '~' Then Begin
      value := copy(value, 1, i) + '~' + copy(value, i + 1, length(value));
      inc(i);
    End;
    // Todo: Für Mac brauchts den Sonderfall, dass #13 in ~0 umgewandelt wird.
    // CR in ""
    // Rremove #13 on Windows Plattform
    If value[i] = #13 Then Begin
      value := copy(value, 1, i - 1) + copy(value, i + 1, length(value));
    End;
    // RT in ~0
    If value[i] = #10 Then Begin
      value := copy(value, 1, i - 1) + '~0' + copy(value, i + 1, length(value));
    End;
    inc(i);
  End;
  result := '"' + value + '"'; // Die "" macht das TIniFile automatisch wieder weg, beim Speichern sind sie aber wichtig.
End;

Type

  (*
   * Callback Routine zum bearbeiten von Ressource Strings unterstützt nur 1 Argument Pointer, wir brauchen aber 2
   *)
  TTrampolinPointer = Record
    Inifile: TIniFile;
    StringList: TStringList;
  End;

  PTrampolinPointer = ^TTrampolinPointer;

  { DialogForm }

  TDialogForm = Class(TForm) // Das Abfrageformular, bei 2 unteschiedlichen Strings...
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    Memo1: Tmemo;
    Button1: TButton;
    Procedure Button1Click(Sender: TObject);
    Procedure OnCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Procedure TDialogForm.OnCreate(Sender: TObject);
Begin
  Position := poScreenCenter;
  caption := 'Question, modified property..';
  Width := 400;
  Height := 250;
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  label1 := tlabel.Create(self);
  label1.Parent := Self;
  label1.Left := 10;
  label1.Top := 10;
  RadioGroup1 := TRadioGroup.Create(self);
  RadioGroup1.Parent := self;
  RadioGroup1.Width := 380;
  RadioGroup1.Height := 160;
  RadioGroup1.Left := 10;
  RadioGroup1.Top := label1.top + 2 * label1.height + 5;
  memo1 := TMemo.Create(self);
  memo1.Parent := self;
  memo1.Top := 120;
  memo1.left := 40;
  memo1.Width := 338;
  memo1.height := 85;
  memo1.ScrollBars := ssAutoBoth;
  button1 := TButton.Create(self);
  button1.Parent := self;
  button1.Top := 220;
  button1.Left := 10;
  button1.Width := 380;
  button1.Caption := '&OK';
  button1.OnClick := @Button1Click;
End;

Procedure TDialogForm.Button1Click(Sender: TObject);
Begin
  close;
End;

Function Question2Strings(CompName, First, Second: String): String;
Var
  form: TDialogForm;
Begin
  form := TDialogForm.CreateNew(Nil);
  form.OnCreate(Nil);
  form.label1.Caption := format('%s' + LineEnding + 'has two different defines, please choose:', [CompName]);
  form.memo1.Text := Second;
  form.RadioGroup1.Items.Add(first);
  form.RadioGroup1.Items.Add('');
  form.RadioGroup1.ItemIndex := 1;
  form.ShowModal;
  If form.RadioGroup1.ItemIndex = 0 Then Begin
    result := form.RadioGroup1.Items[0];
  End
  Else Begin
    result := form.Memo1.Text;
  End;
  form.Free;
End;

Constructor TLanguage.Create;
Begin
  Inherited Create;
  AskNameChanges := false;
  FCurrentLanguage := '';
  fIgnored := Nil;
  fIgnoredComp := Nil;
End;

Destructor TLanguage.Destroy;
Begin
  setlength(fIgnoredComp, 0);
  setlength(fIgnored, 0);
End;

Procedure TLanguage.AddIgnoredIdentifier(Indent: String);
Begin
  If IndentisIgnored(Indent) Then exit;
  setlength(fIgnored, high(fIgnored) + 2);
  fIgnored[high(fIgnored)] := Indent;
End;

Procedure TLanguage.AddIgnoredComponent(Comp: TComponent);
Begin
  If CompIsIgnored(Comp) Then exit;
  setlength(fIgnoredComp, high(fIgnoredComp) + 2);
  fIgnoredComp[high(fIgnoredComp)] := comp;
End;

Function TLanguage.HasProperty(comp: TComponent; prop: String): boolean;
Begin
  Result := (GetPropInfo(Comp.classInfo, Prop) <> Nil) And (Comp.Name <> '');
End;

Function TLanguage.GetProp(Comp: TComponent; Prop: String): String;
Var
  pi: PPropInfo;
Begin
  pi := GetPropInfo(Comp.ClassInfo, Prop);
  If pi <> Nil Then
    Result := GetStrProp(Comp, pi)
  Else
    Result := '';
End;

Procedure TLanguage.SetProp(comp: TComponent; Const prop, value: String);
Var
  pi: PPropInfo;
Begin
  If Value <> '' Then Begin
    pi := GetPropInfo(Comp.ClassInfo, Prop);
    If pi <> Nil Then Begin
      SetStrProp(Comp, pi, Value);
    End
    Else Begin
      Raise exception.create('Error, could not set property "' + prop + '"+ for "' + comp.Name + '"');
    End;
  End;
End;

Procedure TLanguage.DeleteAllUnusedItems(Const UsedList: TStringList;
  LanguageFile: String);

Var
  changed_: Boolean;
  dest: TStringList;

  Procedure Quick(Const list: TstringList; li, re: integer);
  Var
    l, r: Integer;
    h, p: String;
  Begin
    If Li < Re Then Begin
      p := list[Trunc((li + re) / 2)]; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While (CompareText(list[l], p) > 0) Do
          inc(l);
        While CompareText(list[r], p) < 0 Do
          dec(r);
        If L <= R Then Begin
          // Oh ist das ein Eckliger Hack, zugriff auf 2 Globale Variablen
          If (list = dest) And (l <> R) Then Begin
            // So stellen wir sicher, dass wenn die Datei umsortiert wird sie auf jeden Fall auch gespeichert wird
            // Wenn das so gemacht wird sind die Ergebnis Dateien immer sortiert
            // => ist die Sprach Datei in einem VCS eingecheckt hat sie weniger und Nachvollziehbarere Änderungen.
            changed_ := true;
          End;
          h := list[l];
          list[l] := list[r];
          list[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(list, li, r);
      quick(list, l, re);
    End;
  End;

Var
  destindex, i: integer;

Begin
  changed_ := false;
  dest := TStringList.Create;
  dest.LoadFromFile(LanguageFile);
  destindex := -1;
  // Sortieren, dann geht das löschen nachher deutlich schneller O(n) anstatt O(N^2)
  // da das Sortieren im avg nur O(n log(n)) kostet sind wir hier tatsächlich schneller !!
  quick(dest, 1, dest.Count - 1); // Sortieren der in der Datei verwendeten Indizes aber ohne die 1. Zeile [Translation]
  destindex := 1;
  quick(UsedList, 0, UsedList.Count - 1); // Sortieren der Benutzten Indizes
  For i := 1 To dest.count - 1 Do Begin
    // Hier darf natürlich nur der Teil vor dem = klein gemacht werden
    dest[i] := lowercase(copy(dest[i], 1, pos('=', dest[i]))) + copy(dest[i], pos('=', dest[i]) + 1, length(dest[i]));
  End;
  For i := 0 To UsedList.count - 1 Do Begin
    UsedList[i] := lowercase(UsedList[i]);
  End;
  // Nun da beide Sortiert sind ( Egal wie, hauptsache Gleich )
  // Können wir in O(n) den Lösch und Vergleich Vorgang durchführen ( entspricht quasi einer Plateau soche )
  For i := 0 To UsedList.Count - 1 Do Begin
    While (pos(UsedList[i], dest[destindex]) <> 1) Do Begin
      dest.Delete(destindex);
      changed_ := true;
    End;
    inc(destindex);
  End;
  // Wenn am Ende noch Einträge sind, welche nicht mehr benutzt sind, werden diese nun auch gelöscht.
  While destindex < dest.count Do Begin
    dest.delete(dest.count - 1);
    changed_ := true;
  End;
  // Nur Bei Änderungen Speichern, das spart HDD Traffic.
  If changed_ Then Begin
    dest.SaveToFile(LanguageFile);
  End;
  dest.free;
End;

Function TLanguage.IndentisIgnored(indent: String): Boolean;
Var
  i: Integer;
Begin
  result := false;
  indent := lowercase(indent);
  For i := 0 To high(fIgnored) Do Begin
    If indent = lowercase(fIgnored[i]) Then Begin
      result := true;
      exit;
    End;
    // Will man etwas sperren wie alle .items, dann müssen wir das auch erkennen
    If pos(lowercase(fIgnored[i]), indent) = 1 Then Begin // Der Ignored ist damit ein Prefix
      If (length(indent) > length(fIgnored[i])) And // Der Ignored hat dann hinten ein [
        (indent[length(fIgnored[i]) + 1] = '[') Then Begin
        result := true;
        exit;
      End;
    End;
  End;
End;

Function TLanguage.CompIsIgnored(Comp: TComponent): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(fIgnoredComp) Do Begin
    If comp = fIgnoredComp[i] Then Begin
      result := true;
      exit;
    End;
  End;
End;

Function TranslateResource(Name, Value: AnsiString; Hash: Longint; arg: pointer): String;
Var
  comp, s3: String;
  TrampolinPointer: PTrampolinPointer;
Begin
  TrampolinPointer := arg;
  comp := copy(Name, 11, length(Name)); //remove ulanguage. from ulanguage.<resourcestring>
  TrampolinPointer^.StringList.Add(comp);
  Result := decode(TrampolinPointer^.Inifile.ReadString('Translation', comp, ''));
  If Result = '' Then Begin
    TrampolinPointer^.Inifile.WriteString('Translation', comp, encode(Value));
    Result := Value;
  End
  Else Begin
    If language.AskNameChanges Then Begin
      If result <> value Then Begin
        s3 := Question2Strings(Name, result, value);
        If s3 <> result Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
          TrampolinPointer^.Inifile.WriteString('Translation', comp, encode(s3));
        End;
        result := s3; // Übernehmen des Wertes als neuen Wert
      End;
    End;
  End;
End;

Procedure TLanguage.SetLanguage(Const aValue: String);
Var
  i, j, k: integer;
  ParentComp, Comp: TComponent;
  s3, s2, s: String;
  IniFile: TIniFile;
  UsedNamesList: TStringList; // Speichert alle Verwendeten namen, diese können nachher gefiltert und der Rest verworfen werden.
  TrampolinPointer: PTrampolinPointer;
  nvalue: String;
Begin
  nvalue := utf8tosys(avalue);
  If (nValue <> FCurrentLanguage) And (nValue <> '') And (nValue <> ' ') Then Begin
    //Inifile := TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + nValue);
    Inifile := TIniFile.Create(nValue);
    UsedNamesList := TStringList.Create;
    Try
      FCurrentLanguage := nValue;
      For i := 0 To Application.ComponentCount - 1 Do Begin // Alle TForms Abklappern
        ParentComp := Application.Components[i];
        If CompIsIgnored(ParentComp) Then Continue;
        If HasProperty(ParentComp, 'Caption') And (GetProp(ParentComp, 'Caption') <> '') Then Begin
          If (Not IndentisIgnored(ParentComp.Name + '.Caption') And (Not CompIsIgnored(ParentComp))) Then Begin
            s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.Caption', ''));
            UsedNamesList.Add(ParentComp.Name + '.Caption');
            If s = '' Then Begin
              s := GetProp(ParentComp, 'Caption');
              Inifile.WriteString('Translation', ParentComp.Name + '.Caption', encode(s));
            End
            Else Begin
              If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                s2 := GetProp(ParentComp, 'Caption');
                If encode(s) <> encode(s2) Then Begin
                  s3 := Question2Strings(ParentComp.Name + '.Caption', s, s2);
                  If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                    Inifile.WriteString('Translation', ParentComp.Name + '.Caption', encode(s3));
                  End;
                  s := s3;
                End;
              End;
            End;
            SetProp(ParentComp, 'Caption', s);
          End;
        End;

        If HasProperty(ParentComp, 'Hint') And (GetProp(ParentComp, 'Hint') <> '') Then Begin
          If (Not IndentisIgnored(ParentComp.Name + '.Hint') And (Not CompIsIgnored(ParentComp))) Then Begin
            s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.Hint', ''));
            UsedNamesList.Add(ParentComp.Name + '.Hint');
            If s = '' Then Begin
              s := GetProp(ParentComp, 'Hint');
              Inifile.WriteString('Translation', ParentComp.Name + '.Hint', encode(s));
            End
            Else Begin
              If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                s2 := GetProp(ParentComp, 'Hint');
                If encode(s) <> encode(s2) Then Begin
                  s3 := Question2Strings(ParentComp.Name + '.Hint', s, s2);
                  If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                    Inifile.WriteString('Translation', ParentComp.Name + '.Hint', encode(s3));
                  End;
                  s := s3;
                End;
              End;
            End;
            SetProp(ParentComp, 'Hint', s);
          End;
        End;

        For j := 0 To ParentComp.ComponentCount - 1 Do Begin // Alle Komponenten der TForms Abklappern
          Comp := ParentComp.Components[j];
          If CompIsIgnored(Comp) Then Continue;
          If HasProperty(Comp, 'Caption') And
            (getProp(Comp, 'Caption') <> '-') And // Auschließen des "Formatierungscaptions"
          (getProp(Comp, 'Caption') <> '') Then Begin // Auschließen von "Leeren" captions
            If (Not IndentisIgnored(ParentComp.Name + '.' + Comp.Name + '.Caption')) Then Begin
              s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.' + Comp.Name + '.Caption', ''));
              UsedNamesList.Add(ParentComp.Name + '.' + Comp.Name + '.Caption');
              If s = '' Then Begin
                s := GetProp(Comp, 'Caption');
                Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Caption', encode(s));
              End
              Else Begin
                If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                  s2 := GetProp(Comp, 'Caption');
                  If encode(s) <> encode(s2) Then Begin
                    s3 := Question2Strings(ParentComp.Name + '.' + Comp.Name + '.Caption', s, s2);
                    If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                      Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Caption', encode(s3));
                    End;
                    s := s3;
                  End;
                End;
              End;
              SetProp(Comp, 'Caption', s);
            End;
          End;

          If HasProperty(Comp, 'Hint') And (GetProp(Comp, 'Hint') <> '') Then Begin
            If (Not IndentisIgnored(ParentComp.Name + '.' + Comp.Name + '.Hint')) Then Begin
              s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.' + Comp.Name + '.Hint', ''));
              UsedNamesList.Add(ParentComp.Name + '.' + Comp.Name + '.Hint');
              If s = '' Then Begin
                s := GetProp(Comp, 'Hint');
                Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Hint', encode(s));
              End
              Else Begin
                If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                  s2 := GetProp(Comp, 'Hint');
                  If encode(s) <> encode(s2) Then Begin
                    s3 := Question2Strings(ParentComp.Name + '.' + Comp.Name + '.Hint', s, s2);
                    If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                      Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Hint', encode(s3));
                    End;
                    s := s3;
                  End;
                End;
              End;
              SetProp(Comp, 'Hint', s);
            End;
          End;

          If HasProperty(Comp, 'Filter') And (GetProp(Comp, 'Filter') <> '') Then Begin
            If (Not IndentisIgnored(ParentComp.Name + '.' + Comp.Name + '.Filter')) Then Begin
              s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.' + Comp.Name + '.Filter', ''));
              UsedNamesList.Add(ParentComp.Name + '.' + Comp.Name + '.Filter');
              If s = '' Then Begin
                s := GetProp(Comp, 'Filter');
                Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Filter', encode(s));
              End
              Else Begin
                If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                  s2 := GetProp(Comp, 'Filter');
                  If encode(s) <> encode(s2) Then Begin
                    s3 := Question2Strings(ParentComp.Name + '.' + Comp.Name + '.Filter', s, s2);
                    If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                      Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Filter', encode(s3));
                    End;
                    s := s3;
                  End;
                End;
              End;
              SetProp(Comp, 'Filter', s);
            End;
          End;

          If (Comp Is TRadioGroup) Then Begin
            For k := 0 To TRadioGroup(Comp).Items.Count - 1 Do Begin
              If (Not IndentisIgnored(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']')) Then Begin
                s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', ''));
                UsedNamesList.Add(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']');
                If s = '' Then Begin
                  s := TRadioGroup(Comp).Items[k];
                  Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', encode(s));
                End
                Else Begin
                  If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                    s2 := TRadioGroup(Comp).Items[k];
                    If encode(s) <> encode(s2) Then Begin
                      s3 := Question2Strings(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', s, s2);
                      If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                        Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', encode(s3));
                      End;
                      s := s3;
                    End;
                  End;
                End;
                (Comp As TRadioGroup).Items[k] := s;
              End;

            End;
          End;

          If (Comp Is TCheckListBox) Then Begin
            For k := 0 To TCheckListBox(Comp).Items.Count - 1 Do Begin
              If (Not IndentisIgnored(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']')) Then Begin
                s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', ''));
                UsedNamesList.Add(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']');
                If s = '' Then Begin
                  s := TCheckListBox(Comp).Items[k];
                  Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', encode(s));
                End
                Else Begin
                  If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                    s2 := TCheckListBox(Comp).Items[k];
                    If encode(s) <> encode(s2) Then Begin
                      s3 := Question2Strings(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', s, s2);
                      If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                        Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', encode(s3));
                      End;
                      s := s3;
                    End;
                  End;
                End;
                (Comp As TCheckListBox).Items[k] := s;
              End;
            End;
          End;

          If (Comp Is TStringGrid) Then Begin
            For k := 0 To TStringGrid(Comp).Columns.Count - 1 Do Begin
              If (Not IndentisIgnored(ParentComp.Name + '.' + Comp.Name + '.Column[' + IntToStr(k) + ']')) Then Begin
                s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.' + Comp.Name + '.Column[' + IntToStr(k) + ']', ''));
                UsedNamesList.Add(ParentComp.Name + '.' + Comp.Name + '.Column[' + IntToStr(k) + ']');
                If s = '' Then Begin
                  s := TStringGrid(Comp).Columns[k].Title.Caption;
                  Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Column[' + IntToStr(k) + ']', encode(s));
                End
                Else Begin
                  If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                    s2 := TStringGrid(Comp).Columns[k].Title.Caption;
                    If encode(s) <> encode(s2) Then Begin
                      s3 := Question2Strings(ParentComp.Name + '.' + Comp.Name + '.Column[' + IntToStr(k) + ']', s, s2);
                      If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                        Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Column[' + IntToStr(k) + ']', encode(s3));
                      End;
                      s := s3;
                    End;
                  End;
                End;
                (Comp As TStringGrid).Columns[k].Title.Caption := s;
              End;
            End;
          End;

          If (Comp Is TCombobox) Then Begin
            For k := 0 To TCombobox(Comp).Items.Count - 1 Do Begin
              If (Not IndentisIgnored(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']')) Then Begin
                s := decode(Inifile.ReadString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', ''));
                UsedNamesList.Add(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']');
                If s = '' Then Begin
                  s := TCombobox(Comp).Items[k];
                  Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', encode(s));
                End
                Else Begin
                  If AskNameChanges Then Begin // Wenn der Gespeicherte Wert <> Initialwert ist, und Veränderungen angefragt werden sollen
                    s2 := TCombobox(Comp).Items[k];
                    If encode(s) <> encode(s2) Then Begin
                      s3 := Question2Strings(ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', s, s2);
                      If s3 <> s Then Begin // Wenn der Neue Name nicht der ist, der in der Ini steht
                        Inifile.WriteString('Translation', ParentComp.Name + '.' + Comp.Name + '.Items[' + IntToStr(k) + ']', encode(s3));
                      End;
                      s := s3;
                    End;
                  End;
                End;
                (Comp As TCombobox).Items[k] := s;
              End;
            End;
          End;

          (*
           * Todo : Sollte es noch weitere Speichernswerte Komponenten geben, müssten sie Analog zu den Oberen hier implementiert werden..
           *)

        End; //for j
      End; //for i
      //translate resourcestrings
      new(TrampolinPointer);
      TrampolinPointer^.Inifile := IniFile;
      TrampolinPointer^.StringList := UsedNamesList;
      SetUnitResourceStrings('ulanguage', @TranslateResource, TrampolinPointer);
      Dispose(TrampolinPointer);
    Finally
      IniFile.Free;
    End;
    // Löschen aller nicht verwendeten Einträge in der Ini-Datei
    DeleteAllUnusedItems(UsedNamesList, nValue);
    UsedNamesList.free;
  End;
End;

Function TLanguage.GetLanguage: String;
Begin
  result := systoutf8(FCurrentLanguage);
End;

Initialization
  Language := TLanguage.Create;

Finalization
  Language.Free;
End.

