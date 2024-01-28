(******************************************************************************)
(* uDomXML                                                         07.10.2016 *)
(*                                                                            *)
(* Version     : 0.10                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Module to load / save .xml files                             *)
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
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Fix Memleak on error, Fix Stringparsing               *)
(*               0.03 - Fix Parsing Error für <? ?> tag                       *)
(*                      Fix parsing Error für <Bla >Leerzeichen zuviel</Bla>  *)
(*                      Fix Memleak bei Parsingfehlern                        *)
(*               0.04 - Attribute via AttributValue zugreifbar                *)
(*               0.05 - Bessere Fehlermeldung beim Parsen falscher XML-Dateien*)
(*                      Parsing state -> enum                                 *)
(*               0.06 - Findpath (wie bei TJSONObj) implementiert             *)
(*               0.07 - ExpandEmptyNodes                                      *)
(*               0.08 - Erste implementierung zu "escaping"                   *)
(*               0.09 - generisches AddChild für TDomnode                     *)
(*               0.10 - FIX scan for invalid characters in Attributes         *)
(*                                                                            *)
(* Known Bugs  : -Unsinnige Fehlermeldung wenn gar keine gültige xml Datei    *)
(*                geparst werden soll.                                        *)
(*               -ParseNode hat keine Overflow protection                     *)
(*                                                                            *)
(******************************************************************************)
Unit uDOMXML;

{$MODE objfpc}{$H+}

Interface

Uses classes;

Type

  TDomAttribute = Record
    AttributeName: String;
    AttributeValue: String;
  End;

  { TDomNode }

  TDomNode = Class
  private
    fNodes: Array Of TDomNode;
    fAktualTraverser: Integer;
    fParent: TDomNode;
    fAttributes: Array Of TDomAttribute; // Sollte wohl besser über Property's gesteuert werden..
    Procedure Clear;
    Function getAttribute(index: integer): TDomAttribute;
    Function GetAttributeCount: integer;
    Function GetAttributeValue(AttribName: String): String;
    Procedure SetAttributeValue(AttribName: String; AValue: String);
    Procedure UnregisterChild(Const ChildNode: TDomNode);
  public
    NodeName: String;
    NodeValue: String;

    Property AttributeCount: integer read GetAttributeCount;
    Property Attribute[index: integer]: TDomAttribute read getAttribute;
    Property AttributeValue[AttribName: String]: String read GetAttributeValue write SetAttributeValue;
    Constructor Create;
    Destructor Destroy; override;

    Function FirstChild: TDomNode; // Der erste "Gültige" DomNode (also ohne Kommentare oder Ladeanweisungen)
    Function NextSibling: TDomNode; //Der Nächste "Gültige" DomNode, Nil wenn es keinen weiteren mehr gibt.

    (*
     * Durchsucht Self und alle Kinder nach SearchNodeName, gibt das 1. Vorkommen zurück (Sinnvoll, wenn es SearchNodeName nur 1 mal gibt)
     *)
    Function FindNode(Const SearchNodeName: String; CaseSensitive: Boolean = True): TDomNode;
    (*
     * Wie FindNode, nur dass mittels . getrennt ein Pfad angegeben werden kann (Sinnvoll, wenn man z.B. das Element "x" haben will und es vorher schon einen Node gibt der auch ein "x" element hat.
     *)
    Function FindPath(APath: String): TDomNode;

    (*
     * Routinen zum Händischen Erstellen aus dem "nichts"
     *)
    Function AddChild(ChildNodeName, ChildNodeValue: String): TDomNode; overload; // Erstellt einen neuen Kindknoten und gibt diesen zurück
    Function AddChild(Const NewChild: TDomNode): TDomNode; overload; // Damit kann dann ein beliebiges DomNode z.B. TDeclaraionDomNode, TCommentDomNode eingefügt werden.
    Procedure AddAttribute(aAttributeName, aAttributeValue: String);
    Procedure DelAttribute(index: integer);
  End;

  TCommentDomNode = Class(TDomNode); // <!
  TDeclaraionDomNode = Class(TDomNode); // <?

  { TDOMXML }

  TDOMXML = Class
  private
    fLastError: String;
    Function GetLastError: String;
    Function ParseNode(Var index: Integer; Buffer: String): TDomNode; // Extrahiert aus Buffer einen TDomNode (sammt aller Unternodes)
    Procedure ParseFromString(Buffer: String);
    Procedure SaveNode(Node: TDomNode; Const List: TStringList; Depth: Integer); // Speichert einen DomNode in List
    Procedure SaveToStringList(Const List: TStringList);
  public
    (*
     * Arbeiten
     *)
    DocumentElement: TDomNode;
    Indent: String; // Die Zeichenfolge mit dem beim erstellen Eingerückt wird, normalerweise #9

    ExpandEmptyNodes: boolean; // Wenn True, dann wird <xyz\> zo <xyz></xyz> expandiert beim Speichern

    Property LastError: String read GetLastError; // Zum Rausreichen des entstandenen Fehlers

    Constructor Create;
    Destructor Destroy; override;

    Procedure Clear;

    (*
     * laden Speichern
     *)
    Function LoadFromFile(Const Filename: String): Boolean;
    Function LoadFromStream(Const Stream: TStream): Boolean;
    Procedure SaveToFile(Const Filename: String);
    Procedure SaveToStream(Const Stream: TStream);
  End;

Implementation

Uses SysUtils, LazFileUtils;

(*
 * Konvertiert einen String in einen String der in einer XML-Datei stehen darf
 * > -> &#60 , ...
 *)

Function StringToXMLString(Value: String): String;
Begin
  result := value;
  result := StringReplace(result, '&', '&#38;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&#60;', [rfReplaceAll]);
  result := StringReplace(result, '>', '&#62;', [rfReplaceAll]);
  result := StringReplace(result, '''', '&#39;', [rfReplaceAll]);
  result := StringReplace(result, '"', '&#34;', [rfReplaceAll]);
End;

(*
 * Umkehrfunktion zu StringToXMLString
 *)

Function XMLStringToString(Value: String): String;
Begin
  result := value;
  result := StringReplace(result, '&#34;', '"', [rfReplaceAll]);
  result := StringReplace(result, '&#39;', '''', [rfReplaceAll]);
  result := StringReplace(result, '&gt;', '>', [rfReplaceAll]);
  result := StringReplace(result, '&#62;', '>', [rfReplaceAll]);
  result := StringReplace(result, '&lt;', '<', [rfReplaceAll]);
  result := StringReplace(result, '&#60;', '<', [rfReplaceAll]);
  result := StringReplace(result, '&#38;', '&', [rfReplaceAll]);
End;

(*
 * Konvertiert einen String in einen String der in einem XML-Attribut stehen darf
 * & -> &amp; , ...
 *)

Function StringToXMLAttributeString(Value: String): String;
Begin
  result := value;
  result := StringReplace(result, '&', '&amp;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := StringReplace(result, '"', '&quot;', [rfReplaceAll]);
End;

(*
 * Umkehrfunktion zu StringToXMLAttributeString
 *)

Function XMLAttributeStringToString(Value: String): String;
Begin
  result := value;
  result := StringReplace(result, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
End;

{ TDOMXML }

Procedure TDOMXML.Clear;
Begin
  fLastError := '';
  DocumentElement.Clear;
  DocumentElement.NodeValue := '';
End;

Constructor TDOMXML.Create;
Begin
  Inherited Create;
  DocumentElement := TDomNode.Create;
  Indent := #9;
  ExpandEmptyNodes := false;
End;

Destructor TDOMXML.Destroy;
Begin
  Clear;
  DocumentElement.free;
End;

Function TDOMXML.LoadFromFile(Const Filename: String): Boolean;
Var
  sl: TStringList;
  s: String;
Begin
  If Not FileExistsutf8(Filename) Then Begin
    Clear;
    result := false;
    exit;
  End;
  sl := TStringList.Create;
  sl.LoadFromFile(Filename);
  s := sl.Text;
  sl.Free;
  ParseFromString(s);
  result := assigned(DocumentElement.fNodes);
  If result Then Begin
    DocumentElement.NodeValue := Filename;
  End;
End;

Function TDOMXML.LoadFromStream(Const Stream: TStream): Boolean;
Var
  sl: TStringList;
  s: String;
Begin
  sl := TStringList.Create;
  sl.LoadFromStream(Stream);
  s := sl.Text;
  sl.Free;
  ParseFromString(s);
  result := assigned(DocumentElement.fNodes);
End;

Procedure TDOMXML.ParseFromString(Buffer: String);
Var
  l, i, j: Integer;
  n: TDomNode;
Begin
  Clear;
  // Parsen
  i := 1;
  Try
    n := ParseNode(i, Buffer);
    While assigned(n) Do Begin
      setlength(DocumentElement.fNodes, High(DocumentElement.fNodes) + 2);
      DocumentElement.fNodes[High(DocumentElement.fNodes)] := n;
      DocumentElement.fNodes[High(DocumentElement.fNodes)].fParent := DocumentElement;
      n := ParseNode(i, Buffer);
    End;
  Except
    On av: Exception Do Begin
      l := 1; // Der Versuch ungefähr die Zeile in der der Fehler auftrat zu bestimmen
      For j := 1 To i Do Begin
        If buffer[j] = #10 Then Begin
          inc(l);
        End;
      End;
      Clear;
      fLastError := av.Message + ' Line [' + inttostr(l) + ']';
    End;
  End;
End;

Function TDOMXML.GetLastError: String;
Begin
  result := fLastError;
End;

Function TDOMXML.ParseNode(Var index: Integer; Buffer: String): TDomNode;
  Procedure InlineAdd(Var t: String; Data: Char)Inline;
  Begin
    If ord(data) > 32 Then Begin
      t := t + data;
    End;
  End;

Type
  TState =
    (
    SearchtLT, // Suche "<"
    SearchSPGT, // Suche " " oder ">" -> Name
    ReadNameAttr, // lese name Attribut
    ReadValueAttr, // lese Wert Attribut
    ReadChildNodes, // lese KindNodes
    ReadCommend, // lese einen Kommentar und gib ein TIgnoreDomNode zurück
    SearchLTSlash, // Suche </ Tag
    RepeatReadName // lese Name erneut
    );

Var
  state: TState;
  inString: Boolean;
  oindex: Integer;
  an, t: String;
  tmpNode: TDomNode;
  isProcessingNode: Boolean;
Begin
  result := Nil;
  state := SearchtLT;
  inString := false;
  isProcessingNode := false;
  // TODO: Dieser Code hat jede Menge Lookaheads drin, aber keine Prüfung ob der Speicher auch gültig ist!!
  While Index < length(Buffer) Do Begin
    Case state Of
      SearchtLT: Begin // Lesen Node Name
          If (Buffer[Index] = '<') And (Buffer[Index + 1] <> '/') And (Buffer[Index + 1] <> '?') And (Buffer[Index + 1] <> '!') Then Begin // Öffnendes Token gefunden wir Parsen den Namen
            state := SearchSPGT;
            t := '';
          End
          Else Begin
            If (Buffer[Index] = '<') And (Buffer[Index + 1] = '/') Then Begin // Als Nächstes kommt ein Schließendes Token => wir haben gerade Content eines Nodes geparst.
              Index := Index - 1; // Wir setzen den Lesepointer wieder so, dass der Aufrufer nachher wieder das "<" lesen kann
              exit;
            End
            Else Begin
              If (Buffer[Index] = '<') And (Buffer[Index + 1] = '?') Then Begin // Als Nächstes kommt eine Verarbeitungsanweisung
                state := SearchSPGT;
                isProcessingNode := true;
                inc(Index);
                t := '';
              End
              Else Begin
                If (Buffer[Index] = '<') And (Buffer[Index + 1] = '!') Then Begin
                  If (Buffer[Index + 2] = '-') And (Buffer[Index + 3] = '-') Then Begin // Öffnender Kommentar
                    inc(Index, 3);
                    state := ReadCommend;
                    t := '';
                  End
                  Else Begin
                    If length(buffer) < (Index + length('![CDATA[]]>')) Then Begin
                      Raise Exception.Create('Invalid xml-file.');
                    End
                    Else Begin
                      // Siehe auch https://www.w3.org/TR/xml/
                      If (buffer[index + 2] = '[') And
                        (uppercase(buffer[index + 3]) = 'C') And
                        (uppercase(buffer[index + 4]) = 'D') And
                        (uppercase(buffer[index + 5]) = 'A') And
                        (uppercase(buffer[index + 6]) = 'T') And
                        (uppercase(buffer[index + 7]) = 'A') And
                        (uppercase(buffer[index + 8]) = '[') Then Begin
                        // Todo : Hier würde ein CDATA-Abschnittes erkannt und dann müsste entsprechd ein solcher Node zurückgegeben werden, das könnten aber auch mehrere sein
                        Raise Exception.Create('Missing Implementation for CDATA.');
                      End
                      Else Begin
                        Raise Exception.Create('Invalid xml-file.');
                      End;
                    End;
                  End;
                End;
              End;
            End;
          End;
        End;
      SearchSPGT: Begin // Lesen Attribute
          If (ord(Buffer[Index]) <= 32) Or (Buffer[Index] = '>') Or (Buffer[Index] = '/') Then Begin
            If isProcessingNode Then Begin
              result := TDeclaraionDomNode.Create;
            End
            Else Begin
              result := TDomNode.Create;
            End;
            result.NodeName := t;
            If (ord(Buffer[Index]) <= 32) Then Begin
              state := ReadNameAttr; // Lese Attribute
              t := '';
            End
            Else Begin
              If Buffer[Index] = '/' Then Begin
                If Buffer[Index + 1] = '>' Then Begin // Gültiges ende eines Nodes Erkannt
                  inc(Index, 2); // Überlesen des "/>"
                  exit;
                End
                Else Begin
                  result.free;
                  result := Nil;
                  Raise Exception.Create('Parsing error, expected : ">"');
                End;
              End
              Else Begin
                state := ReadChildNodes; // Lese alle Kind Nodes
              End;
            End;
          End
          Else Begin
            InlineAdd(t, Buffer[Index]);
          End;
        End;
      ReadNameAttr: Begin // Lesen Attribut Name
          If (Buffer[Index] = '=') Then Begin // Diese Implementierung akzeptiert ein Attribut "Teil1 Teil2", dies ist eigentlich nicht erlaubt, da Attributnamen keine Leerzeichen beinhalten dürfen
            an := trim(t); // Abschneiden führender und Nachgestellter Leerzeichen
            state := ReadValueAttr;
            t := '';
          End
          Else Begin
            If (trim(t) = '') And ((Buffer[Index] = '/') Or (Buffer[Index] = '?')) Then Begin
              // Da hat jemand nach dem Letzten Attribut und vor dem /> noch leerzeichen rein gemacht.
              If Buffer[Index + 1] = '>' Then Begin // Gültiges ende eines Nodes Erkannt
                inc(Index, 2); // Überlesen des "/>"
                exit;
              End
              Else Begin
                result.free;
                result := Nil;
                Raise Exception.Create('Parsing error, expected : ">"');
              End;
            End;
            If (trim(t) = '') And (buffer[index] = '>') Then Begin // Da hat jemand ein Node gemacht und nach dem Namen oder letzten Arttibut ein Leerzeichen eingefügt z.B. <bal > oder <Bla Blub="Blib" >
              state := ReadChildNodes;
            End;
            t := t + Buffer[Index]; // Eigentlich müsste hier inline Add stehen, da obiger Code aber Explizit Leerzeichen in Attributnamen Akzeptiert, müssen diese hier wiederrum auch übernommen werden
          End;
        End;
      ReadValueAttr: Begin // Lesen Attribut Wert
          If (Not inString) And ((ord(Buffer[Index]) <= 32) Or (Buffer[Index] = '>') Or (Buffer[Index] = '/') Or (isProcessingNode And (Buffer[Index] = '?'))) Then Begin
            setlength(result.fAttributes, High(result.fAttributes) + 2);
            result.fAttributes[High(result.fAttributes)].AttributeName := an;
            result.fAttributes[High(result.fAttributes)].AttributeValue := XMLAttributeStringToString(t);
            t := '';
            If (ord(Buffer[Index]) <= 32) Then Begin // Es folgt ein weiteres Attribut
              state := ReadNameAttr;
            End
            Else Begin
              If (Buffer[Index] = '>') Then Begin // Attribute Fertig gelesen Parsen des Inhalts
                state := ReadChildNodes;
              End
              Else Begin
                If Buffer[Index + 1] = '>' Then Begin // Gültiges ende eines Nodes Erkannt
                  inc(Index, 2); // Überlesen des "/>"
                  exit;
                End
                Else Begin
                  result.free;
                  result := Nil;
                  Raise Exception.Create('Parsing error, expected : ">"');
                End;
              End;
            End;
          End
          Else Begin
            If Buffer[Index] = '"' Then Begin
              // zwei "" hintereinanter
              inString := Not inString;
            End
            Else Begin
              t := t + Buffer[Index];
            End;
          End;
        End;
      ReadChildNodes: Begin
          Repeat
            oindex := Index;
            Try
              tmpNode := ParseNode(Index, Buffer);
            Except
              // Die node konnte nicht geparst werden, also geben wir alles Frei und reichen die Exception weiter hoch.
              result.Free;
              result := Nil;
              Raise;
            End;
            If assigned(tmpNode) Then Begin
              setlength(result.fNodes, High(result.fNodes) + 2);
              result.fNodes[High(result.fNodes)] := tmpNode;
              result.fNodes[High(result.fNodes)].fParent := result;
              oindex := Index;
            End;
          Until tmpNode = Nil;
          result.NodeValue := XMLStringToString(trim(copy(Buffer, oindex, Index - oindex + 1)));
          state := SearchLTSlash;
        End;
      ReadCommend: Begin
          If (Buffer[Index - 2] = '-') And (Buffer[Index - 1] = '-') And (Buffer[Index] = '>') Then Begin
            inc(Index);
            result := TCommentDomNode.Create;
            result.NodeValue := copy(t, 1, length(t) - 2);
            exit;
          End
          Else Begin
            t := t + Buffer[Index]; // in Kommentaren wird einfach alles übernommen
          End;
        End;
      SearchLTSlash: Begin // Der Ende Tag hat angefangen
          If (Buffer[Index - 1] = '<') And (Buffer[Index] = '/') Then Begin
            t := '';
            state := RepeatReadName;
          End;
        End;
      RepeatReadName: Begin
          If Buffer[Index] = '>' Then Begin
            If lowercase(t) = lowercase(result.NodeName) Then Begin // Alles palleti
              inc(Index);
              exit;
            End
            Else Begin
              an := result.NodeName;
              result.free;
              result := Nil;
              Raise Exception.Create('Parsing error, got "' + t + '" expected "' + an + '"');
            End;
          End
          Else Begin
            InlineAdd(t, Buffer[Index]);
          End;
        End;
    End;
    inc(Index);
  End;
  // Wenn wir bis hier her Kommen war was nicht I.O.
  If assigned(result) Then Begin
    result.Free;
    result := Nil;
    Raise Exception.Create('Error while parsing, invalid file.');
  End;
End;

Procedure TDOMXML.SaveNode(Node: TDomNode; Const List: TStringList; Depth: Integer);
Var
  s, dp: String;
  i: Integer;
Begin
  If Not assigned(Node) Then exit;
  If Node Is TCommentDomNode Then Begin
    s := '<!--' + Node.NodeValue + '-->';
    List.Text := List.Text + s;
    exit;
  End;
  // erstellen des Einrückungsseparators
  If Depth = 0 Then Begin
    dp := '';
  End
  Else Begin
    dp := '';
    For i := 1 To Depth Do Begin
      dp := dp + Indent;
    End;
  End;
  // Schreiben aller Attribute und des Namens
  If Node Is TDeclaraionDomNode Then Begin
    s := '<?' + Node.NodeName;
  End
  Else Begin
    s := '<' + Node.NodeName;
  End;
  For i := 0 To High(Node.fAttributes) Do Begin
    s := s + ' ' + Node.fAttributes[i].AttributeName + '="' + StringToXMLAttributeString(Node.fAttributes[i].AttributeValue) + '"';
  End;
  // Ein Node mit nur Attributen
  If (High(Node.fNodes) = -1) And (trim(Node.NodeValue) = '') Then Begin
    If ExpandEmptyNodes Then Begin
      s := s + '>';
      List.Add(dp + s);
      s := '</' + Node.NodeName + '>';
      List.Add(dp + s);
    End
    Else Begin
      If Node Is TDeclaraionDomNode Then Begin
        s := s + '?>';
      End
      Else Begin
        s := s + '/>';
      End;
      List.Add(dp + s);
    End;
    exit;
  End;
  s := s + '>';
  // Ein Node ohne Kind Nodes
  If (High(Node.fNodes) = -1) Then Begin
    s := s + StringToXMLString(Node.NodeValue);
    s := s + '</' + Node.NodeName + '>';
    List.Add(dp + s);
  End
  Else Begin
    // Ein Node mit Kind Nodes
    List.Add(dp + s);
    For i := 0 To High(Node.fNodes) Do Begin
      SaveNode(Node.fNodes[i], List, Depth + 1);
    End;
    If trim(Node.NodeValue) <> '' Then Begin // in der Regel sollte NodeValue = '' sein, aber man weis ja nie, erlaubt wäre es..
      List.Add(dp + StringToXMLString(Node.NodeValue));
    End;
    s := '</' + Node.NodeName + '>';
    List.Add(dp + s);
  End;
End;

Procedure TDOMXML.SaveToFile(Const Filename: String);
Var
  sl: TStringList;
Begin
  sl := TStringList.Create;
  SaveToStringList(sl);
  sl.SaveToFile(Filename);
  sl.Free;
  DocumentElement.NodeValue := Filename;
End;

Procedure TDOMXML.SaveToStream(Const Stream: TStream);
Var
  sl: TStringList;
Begin
  sl := TStringList.Create;
  SaveToStringList(sl);
  sl.SaveToStream(Stream);
  sl.Free;
End;

Procedure TDOMXML.SaveToStringList(Const List: TStringList);
Var
  i: Integer;
Begin
  For i := 0 To High(DocumentElement.fNodes) Do Begin
    SaveNode(DocumentElement.fNodes[i], List, 0);
  End;
End;

{ TDomNode }

Procedure TDomNode.Clear;
Var
  i: Integer;
Begin
  setlength(fAttributes, 0);
  For i := 0 To High(fNodes) Do Begin
    fNodes[i].fParent := Nil; // Wir sind es ja der das Kind Kaputt macht, da braucht es uns das nicht nochmal mit zu teilen.
    fNodes[i].Free;
  End;
  setlength(fNodes, 0);
End;

Function TDomNode.getAttribute(index: integer): TDomAttribute;
Begin
  result := fAttributes[index];
End;

Function TDomNode.GetAttributeCount: integer;
Begin
  result := length(fAttributes);
End;

Function TDomNode.GetAttributeValue(AttribName: String): String;
Var
  i: Integer;
Begin
  result := '';
  For i := 0 To high(fAttributes) Do Begin
    If lowercase(AttribName) = lowercase(fAttributes[i].AttributeName) Then Begin
      result := fAttributes[i].AttributeValue;
      exit;
    End;
  End;
End;

Procedure TDomNode.SetAttributeValue(AttribName: String; AValue: String);
Var
  i: Integer;
Begin
  For i := 0 To high(fAttributes) Do Begin
    If lowercase(AttribName) = lowercase(fAttributes[i].AttributeName) Then Begin
      fAttributes[i].AttributeValue := Avalue;
      exit;
    End;
  End;
  // Das Attribut gibt es noch nicht, also legen wir es an.
  AddAttribute(AttribName, AValue);
End;

Procedure TDomNode.UnregisterChild(Const ChildNode: TDomNode);
Var
  i, j: integer;
Begin
  For i := 0 To high(fNodes) Do Begin
    If fNodes[i] = ChildNode Then Begin
      For j := i To high(fNodes) - 1 Do Begin
        fnodes[j] := fnodes[j + 1];
      End;
      setlength(fNodes, high(fNodes));
      exit;
    End;
  End;
End;

Constructor TDomNode.Create;
Begin
  Inherited;
  NodeName := '';
  NodeValue := '';
  fAttributes := Nil;
  fNodes := Nil;
  fAktualTraverser := -1;
  fParent := Nil;
End;

Destructor TDomNode.Destroy;
Begin
  // Einem Evtlen Parent mitteilen, dass wir kaputt gehen.
  If assigned(fparent) Then Begin
    fParent.UnregisterChild(self);
  End;
  Clear;
End;

Function TDomNode.FirstChild: TDomNode;
Var
  i: Integer;
Begin
  result := Nil;
  fAktualTraverser := -1;
  For i := 0 To High(fNodes) Do Begin
    If (Not (fNodes[i] Is TCommentDomNode)) And
      (Not (fNodes[i] Is TDeclaraionDomNode)) Then Begin
      result := fNodes[i];
      fAktualTraverser := i;
      exit;
    End;
  End;
End;

Function TDomNode.NextSibling: TDomNode;
Var
  i: Integer;
Begin
  result := Nil;
  If fAktualTraverser = -1 Then Begin
    Raise Exception.Create('Iterator not defined, call Firstchild first.');
  End;
  For i := fAktualTraverser + 1 To High(fNodes) Do Begin
    If (Not (fNodes[i] Is TCommentDomNode)) And
      (Not (fNodes[i] Is TDeclaraionDomNode)) Then Begin
      result := fNodes[i];
      fAktualTraverser := i;
      exit;
    End;
  End;
  // Konnte keinen mehr finden, also ist auch der Traverser undefiniert
  fAktualTraverser := -1;
End;

Function TDomNode.FindNode(Const SearchNodeName: String; CaseSensitive: Boolean
  ): TDomNode;
Var
  i: Integer;
Begin
  result := Nil;
  // Sind wir selbst der Gesuchte Knoten ?
  If CaseSensitive Then Begin
    If SearchNodeName = NodeName Then Begin
      result := self;
      exit;
    End;
  End
  Else Begin
    If LowerCase(SearchNodeName) = LowerCase(NodeName) Then Begin
      result := self;
      exit;
    End;
  End;
  // Ist einer unserer Kindknoten der gesuchte ?
  For i := 0 To high(fNodes) Do Begin
    result := fNodes[i].FindNode(SearchNodeName, CaseSensitive);
    If assigned(result) Then exit;
  End;
End;

Function TDomNode.FindPath(APath: String): TDomNode;
Var
  elem: String;
  i: Integer;
Begin
  result := Nil;
  If APath = '' Then Begin
    result := Self;
    exit;
  End;
  elem := APath;
  APath := '';
  For i := 1 To length(elem) Do Begin
    If elem[i] = '.' Then Begin // Weiter Absteigen
      APath := copy(elem, i + 1, length(elem));
      elem := copy(elem, 1, i - 1);
      break;
    End;
    If elem[i] = '[' Then Begin // Zugriff auf Array Elemente
      APath := copy(elem, i, length(elem));
      elem := copy(elem, 1, i - 1);
      break;
    End;
  End;
  elem := LowerCase(elem);
  If (elem = '') And (pos('[', APath) = 1) Then Begin
    i := strtointdef(copy(APath, 2, pos(']', APath) - 2), -1);
    If (i <> -1) And (i <= high(fNodes)) Then Begin
      apath := copy(apath, pos(']', APath) + 1, length(APath));
      If (apath <> '') And (apath[1] = '.') Then Begin
        result := fNodes[i].FindPath(copy(APath, 2, length(APath)));
      End
      Else Begin
        // Es ist das Element direkt gefragt
        result := fNodes[i];
      End;
      exit;
    End;
  End;
  For i := 0 To high(fNodes) Do Begin
    If lowercase(fNodes[i].NodeName) = elem Then Begin
      result := fNodes[i].FindPath(APath);
      exit;
    End;
  End;
End;

Function TDomNode.AddChild(ChildNodeName, ChildNodeValue: String): TDomNode;
Begin
  setlength(fNodes, high(fNodes) + 2);
  fNodes[high(fNodes)] := TDomNode.Create;
  fNodes[high(fNodes)].fParent := self;
  result := fNodes[high(fNodes)];
  result.NodeName := ChildNodeName;
  result.NodeValue := ChildNodeValue;
End;

Function TDomNode.AddChild(Const NewChild: TDomNode): TDomNode;
Begin
  setlength(fNodes, high(fNodes) + 2);
  fNodes[high(fNodes)] := NewChild;
  fNodes[high(fNodes)].fParent := self;
  result := fNodes[high(fNodes)];
End;

Procedure TDomNode.AddAttribute(aAttributeName, aAttributeValue: String);
Begin
  // Todo: Prüfen ob es das schon gibt..
  setlength(fAttributes, high(fAttributes) + 2);
  fAttributes[high(fAttributes)].AttributeName := aAttributeName;
  fAttributes[high(fAttributes)].AttributeValue := aAttributeValue;
End;

Procedure TDomNode.DelAttribute(index: integer);
Var
  i: Integer;
Begin
  For i := index To high(fAttributes) - 1 Do Begin
    fAttributes[i] := fAttributes[i + 1];
  End;
  SetLength(fAttributes, High(fAttributes));
End;

End.


