(******************************************************************************)
(* uncommenter.pas                                                 ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Removes "comments" from a string, using configurable rules.  *)
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
(*               0.02 - Conversion to Unicode, Linux, Removed "UseUnderLinux" *)
(*                      inserted Compilerswitches, to do that.                *)
(*               0.03 - Erste Version für StringLiterale (default,            *)
(*                      deaktiviert)                                          *)
(*                                                                            *)
(******************************************************************************)

Unit uncommenter;

{$MODE ObjFPC}{$H+}

Interface

Uses sysutils; // für inttostr

Type

  // Unser Typ zum Speichern einer Regel
  TRule = Record
    BeginChar: String; // Die zeichenkette die einen Kommentar Token einleitet
    EndChar: String; // Die ZeichenKette die einen Kommentar beendet
    Linewise: Boolean; // Wenn True dann geht der Kommentar nur 1 Zeile Lang
  End;

  TUnCommenter = Class // Die Eigentliche Klasse
  private
    Frules: Array Of Trule; // Die Kommentar Regeln
    FNumberLines: Boolean; // Wenn True dann werden hinter alle Zeilen mit Sonderzeichen Codiert die Echten Zeilennummern eingetragen.
    Fscanlength: Integer; // Legt fest wie viele Zeichen der Parser im Vorraus Einliest.
    FSonderCharLine: Char; // Das Zeichen das benutzt wird um Code von Steuerzeichen = Zeilennummer zu unterscheiden.
    FCaseSensitive: Boolean; // Case sensitiv ?
    FDellEmptyLines: Boolean; // Löschen von Komplett Leeren Zeilen ?
    FStringLiteral: Char; // Wenn <> #0, dann ignoriert der Uncommenter alles innerhalb von 2 FStringLiteral's
  public
    Property StringLiteral: Char read FStringLiteral write FStringLiteral;
    Property DellEmptyLines: boolean read FDellEmptyLines write FDellEmptyLines;
    Property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    Property NumberLines: boolean read FNumberLines write FNumberLines;
    Property ExtraCharLine: Char read FSonderCharLine write FSonderCharLine;
    Constructor Create;
    Destructor Destroy; override;
    Procedure AddRule(BeginChar, EndChar: String; Linewise: Boolean);
    Procedure ClearRules;
    Function Uncomment(Value: String): String;
  End;

Implementation

Uses Math;

Constructor TUnCommenter.Create;
Begin
  Inherited create;
  //UseUnderLinux := false;
  setlength(frules, 0);
  FNumberLines := false;
  FDellEmptyLines := true;
  Fscanlength := 0;
  FSonderCharLine := '~';
  FCaseSensitive := false;
  FStringLiteral := #0;
End;


Destructor TUnCommenter.Destroy;
Begin
  setlength(frules, 0);
End;

Procedure TUnCommenter.AddRule(BeginChar, EndChar: String; Linewise: Boolean);
Begin
  // Wenn Zeilenweise Kommentare sind brauchen wir mindestens 2 Zeichen um #13 #10 erkennen zu können.
  If linewise Then Begin
{$IFDEF Windows}
    Fscanlength := max(Fscanlength, 2); // Zum Erkennen von #13#10
{$ENDIF}
    EndChar := ''; // Gibt es in dem Fall nicht.
  End;
  Fscanlength := max(Fscanlength, length(BeginChar)); // Merken des Längsten Einführungszeichens unserer Tocken
  Fscanlength := max(Fscanlength, length(EndChar)); // Merken des Längsten Beendenzeichens unserer Tocken
  setlength(Frules, high(frules) + 2); // Übernehmen in die Rules Liste
  Frules[high(frules)].BeginChar := BeginChar;
  Frules[high(frules)].EndChar := EndChar;
  Frules[high(frules)].Linewise := Linewise;
End;

Procedure TUnCommenter.ClearRules;
Begin
  setlength(Frules, 0); // Wieder Löschen der Regeln
End;

// Die Eigentliche Arbeit wird hier verrichtet

Function TUnCommenter.Uncomment(Value: String): String;
Var
  ochars: String; // Merken der zueltzt gelesenen Zeichen
  akt: String; // merken der Aktuell gelesenen Zeichen ( Formatiert )
  akttmp: String; // merken der Aktuell gelesenen Zeichen ( unFormatiert )
  erg: String; // Das Ergebniss.
  aLineNumber: integer; // Speichern der Aktuellen Zeilen Nummer des Quell Textes
  i: integer; // Schleifen Zählvariable
  i2: integer; // Schleifen Zählvariable
  inCase: integer; // Merken in welchem Token wir uns gerade befinden -1 = Kein Token
  ueber: Integer; // Anzahl der Zeichen die "Überlesen" werden können
  fall: Boolean; // = True wenn gerade ein Token geschlossen wurde.
  lnm: boolean; // In Ganz Perversen Einstellungen wird die Letzte Zeile nicht beschriftet, dieser Bool behebt das
  b: Boolean; // Schmiermerker
  InString: Boolean; // Wenn True, dann sind wir in einem String -> Alle Regeln sind Deaktiviert
Begin
  If FStringLiteral <> #0 Then Begin
    For i := 0 To high(Frules) Do Begin
      If (pos(FStringLiteral, Frules[i].BeginChar) <> 0) Or
        (pos(FStringLiteral, Frules[i].EndChar) <> 0) Then Begin
        result := '';
        Raise exception.create('Error, StringLiteral is part of a rule.');
        exit;
      End;
    End;
  End;
  // Abfangen des einzigen Sonderfalles, der nicht berechnet werden mus.
  If (High(Frules) = -1) And (Not FNumberLines) And Not (FDellEmptyLines) Then Begin
    result := value;
  End
  Else Begin
    InString := false;
    // Der Text mus auf alle Fälle mit einem CR+ RT aufhören sonst ist die Zeilennumerierung nicht immer Richtig, eines zu viel schadet dabei zum Glück nicht ;)
    Value := Value + LineEnding;
    // Fals noch keine Regeln erzeugt wurden mus hier Fscanlength initialisiert werden.
    If FNumberLines Or FDellEmptyLines Then Fscanlength := max(Fscanlength, 2); // Nachträglich einschalten des scannens nach CR RT
    erg := ''; // Initialisieren
    ochars := ''; // Initialisieren
    inCase := -1; // Initialisieren
    aLineNumber := -1; // Initialisieren
    lnm := False;
    ueber := 0;
    // Die ersten Paar Zeichen können auf einen Schlag eingelesen werden.
    If (Length(Value) > Fscanlength) Then Begin
      akt := copy(value, 1, Fscanlength - 1);
      delete(value, 1, Fscanlength - 1);
    End
    Else Begin
      akt := value;
    End;
    // Stringregel im bereits geparsten berücksichtigen
    If FStringLiteral <> #0 Then Begin
      For i := 1 To length(akt) Do Begin
        If akt[i] = FStringLiteral Then InString := Not InString;
      End;
    End;
    // Zwischenspeichern des eingelesenen Textes Unformatiert
    akttmp := akt;
    If Not FCaseSensitive Then akt := lowercase(akt); // Formatierung für nicht Case Sensitiv
    (*

    Der Teil :

    Or (Length(Akt) <> 0)

    wurde Eingefügt, da der parser das AllerLetze Zeichen Verschluckt hatte.

    *)
    While (Length(Value) <> 0) Or (Length(Akt) <> 0) Do Begin // Solange es noch was zu betrachten gibt
      fall := false; // Eine alte Fallende Flanke Löschen.
      While (Length(Value) <> 0) And (Length(akt) < Fscanlength) Do Begin
        // Weiterlesen im Text.
        If Length(Value) <> 0 Then Begin
          If FCaseSensitive Then // Wenn Casesensitive
            akt := akt + Value[1] // Dann wird Akttmp eigentlich sinnlos.
          Else
            akt := akt + lowercase(Value[1]);
          akttmp := akttmp + Value[1]; // Mitziehen von Akttmp
          If (FStringLiteral <> #0) And (Incase = -1) Then Begin // Strings in Kommentaren werden Ignoriert
            If value[1] = FStringLiteral Then Begin
              InString := Not InString;
            End;
          End;
          delete(Value, 1, 1); // Löschen des bereits eingelesenen Textes
        End;
      End;
      // Scannen nach den BeginnTockens
      If Not InString Then Begin // Wenn wir in einem String Sind, wird die Regelerkennung "Deaktiviert"
        If Incase = -1 Then Begin
          For i := 0 To High(Frules) Do
            // schaun ob der Beginn unseres Tokens überhaupt geht.
            If length(akt) >= Length(Frules[i].BeginChar) Then Begin
              b := true; // Initialisieren
              i2 := 1; // Initialisieren
              While b And (i2 <= length(Frules[i].BeginChar)) Do Begin // Solange der gelesene Text mit dem Tocken übereinstimmt, lesen
                If FCaseSensitive Then Begin // wieder die Case Sensitiv sache
                  If akt[i2] <> Frules[i].BeginChar[i2] Then b := false; // Wenn die Zeicehn nicht Gleich sind
                End
                Else Begin
                  If akt[i2] <> lowercase(Frules[i].BeginChar[i2]) Then b := false; // Wenn die Zeichen nicht Gleich sind
                End;
                inc(i2); // Weiterzählen auf den nächsten Char
              End;
              If b Then Begin // Wenn wir tatsächlich einen Token erkannt haben
                incase := i; // Merken welcher Token es war
                ueber := Length(Frules[i].BeginChar) - 1;
                break; // Raus.
              End;
            End;
        End
        Else Begin // Scannen nach Ende Tokens
          i := incase;
          // schaun ob der Beginn unseres Tokens überhaupt geht.
          If (length(akt) >= Length(Frules[i].EndChar)) And (Not Frules[i].Linewise) Then Begin
            b := true; // Initialisieren
            i2 := 1; // Initialisieren
            While b And (i2 <= length(Frules[i].EndChar)) Do Begin // Solange der gelesene Text mit dem Tocken übereinstimmt, lesen
              If FCaseSensitive Then Begin // wieder die Case Sensitiv sache
                If akt[i2] <> Frules[i].EndChar[i2] Then b := false; // Wenn die Zeicehn nicht Gleich sind
              End
              Else Begin
                If akt[i2] <> lowercase(Frules[i].EndChar[i2]) Then b := false; // Wenn die Zeicehn nicht Gleich sind
              End;
              inc(i2); // Weiterzählen auf den nächsten Char
            End;
            If b Then Begin // Wenn wir tatsächlich einen Token erkannt haben
              incase := -1; // zurücksetzen des Tokenmerkers
              fall := true; // Merken der Fallenden Flanke.
              ueber := length(Frules[i].EndChar) - 1; // Berechnen der zu überspringenden Zeichen
            End;
          End;
{$IFDEF Windows}
          If (Length(Akt) > 1) And Frules[i].Linewise Then // Sonderfall bei nur einzeilenweisem
            If (Akt[1] = #13) And (Akt[2] = #10) Then Begin // Wenn die CRT erkannt wird
{$ELSE}
          If (Length(Akt) > 0) And Frules[i].Linewise Then // Sonderfall bei nur einzeilenweisem
            If (Akt[1] = #10) Then Begin // Wenn die CRT erkannt wird
{$ENDIF}
              incase := -1; // zurücksetzen des Tokenmerkers
              Fall := False; // Da wir hier einen Zeilenweisen Kommentar haben darf es keine Fallende Flanke geben, da sonst das folgende CRT verschluckt werden kann.
              ueber := 0; // Berechnen der zu überspringenden Zeichen
            End;
        End;
      End;
      // SonderFall wenn wir die Zeilen mit ihren Nummern Markieren müssen
{$IFDEF Windows}
      If (Length(Akt) > 1) And FNumberLines Then
        If (Akt[1] = #13) And (Akt[2] = #10) Then Begin // Bei crt
{$ELSE}
      If (Length(Akt) > 0) And FNumberLines Then
        If (Akt[1] = #10) Then Begin // Bei crt
{$ENDIF}
          inc(aLineNumber); // Hochzählen der Aktuellen Zeilennummer
{$IFDEF Windows}
          If (length(ochars) >= 2) And FDellEmptyLines Then Begin // Schaun ob wir ne Leerzeile hatten
            If Not ((ochars[length(Ochars) - 1] = #13) And (ochars[length(Ochars)] = #10)) Then Begin // Wenn nicht crt gelesen wurde
{$ELSE}
          If (length(ochars) >= 1) And FDellEmptyLines Then Begin // Schaun ob wir ne Leerzeile hatten
            If Not ((ochars[length(Ochars)] = #10)) Then Begin // Wenn nicht crt gelesen wurde
{$ENDIF}
              erg := erg + FSonderCharLine + inttostr(aLineNumber); // Einfügen der Zeilennummern
              lnm := true;
            End
          End
          Else If Length(Ochars) <> 0 Then Begin
            erg := erg + FSonderCharLine + inttostr(aLineNumber); // Einfügen der Zeilennummern
            lnm := True;
          End;
        End;
      // Wenn wir keinen Kommentar haben dann Kräftig übernehmen :=)
      If (Incase = -1) And Not fall Then Begin
        If FDellEmptyLines Then Begin // Wenn Leerzeilen Gelöscht werden müssen dann ist hier noch ein wenig Magic Notwendig.
{$IFDEF Windows}
          If (akt[1] = #13) Or (akt[1] = #10) Then Begin // Wenn gerade CRT Gelesen wird
            If (length(ochars) >= 2) Then Begin // schaun ob bereits crt gelesen wurde
              If Not ((ochars[length(Ochars) - 1] = #13) And (ochars[length(Ochars)] = #10)) Then Begin // Wenn nicht crt gelesen wurde
{$ELSE}
          If (akt[1] = #10) Then Begin // Wenn gerade CRT Gelesen wird
            If (length(ochars) >= 1) Then Begin // schaun ob bereits crt gelesen wurde
              If Not ((ochars[length(Ochars)] = #10)) Then Begin // Wenn nicht crt gelesen wurde
{$ENDIF}
                erg := erg + akttmp[1]; // übernehmen des gelesenen Codes in das Ergebniss
                ochars := ochars + akt[1]; // Merken der Zuletzt gelesenen zeichen
                // lnm := False; // Beim Einfügen eines CRT brauchen wir nicht noch Extra eine Zeilenbeschriftung
              End;
            End;
          End
          Else Begin
            erg := erg + akttmp[1]; // übernehmen des gelesenen Codes in das Ergebniss
            ochars := ochars + akt[1]; // Merken der Zuletzt gelesenen zeichen
            lnm := False; // Merken das nach diesem Zeichen auf alle Fälle nochmal Beschriftet werden mus.
          End;
        End
        Else Begin
          erg := erg + akttmp[1]; // übernehmen des gelesenen Codes in das Ergebniss
          ochars := ochars + akt[1]; // Merken der Zuletzt gelesenen zeichen
          lnm := False; // Merken das nach diesem Zeichen auf alle Fälle nochmal Beschriftet werden mus.
        End;
      End;
      // wenn es zeichen zu überspringen gibt dann geschieht das hier
      If Ueber <> 0 Then Begin
        Delete(akt, 1, ueber); // Der Witz ist das Akt immer Länger oder gleich lang wie Ueber + 1 ist !!
        Delete(akttmp, 1, ueber); // Der Witz ist das Akt immer Länger oder gleich lang wie Ueber + 1 ist !!
        Ueber := 0; // Zurücksetzen
      End;
      // Wir müssen nicht alle Zeichen als gelesen speichern
      // Eigentlich könnte auch > 2 stehen.
      If Length(Ochars) > Fscanlength Then
        delete(Ochars, 1, 1);
      // Am Schlus wird immer das 1. Element von Akt gelöscht
      delete(akt, 1, 1);
      delete(akttmp, 1, 1);
    End;
    // Beschriften der Allerletzten Zeile, falls die Beschriftungsoption an ist.
    If Not lnm And FNumberLines Then Begin
      erg := erg + FSonderCharLine + inttostr(aLineNumber); // Einfügen der Zeilennummern
    End;
    result := erg; // Rückgabe unseres Geparsten Textes
  End;
End;

End.

