(******************************************************************************)
(* uFifo.pas                                                     23.09.2005   *)
(*                                                                            *)
(* Version     : 0.04                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Simulation einer FIFO in FPC.                                *)
(*               All die Bekannten Eigenschaften, Befehle wie bei einem FiFo. *)
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
(*               0.02 - Speed up durch entfernen Schleife in Push             *)
(*                      Bugfix Verlust von Daten beim Push                    *)
(*               0.03 - property count                                        *)
(*               0.04 - TFifo thread Safe gemacht                             *)
(*                                                                            *)
(******************************************************************************)

Unit ufifo;

{$MODE ObjFPC}{$H+}

Interface

Uses sysutils, syncobjs; // Für die Exception

Type
  (*
   * TQueue
   *
   * Eine via Pointer realisierte FIFO, Thread Save
   *)

  { TFifo }

  Generic TFifo < T > = Class
  private
    // Hilfstypen Definieren
    Type
      PGenQ = ^TGenQ;
      TGenQ = Record
        Value: T;
        Next: PGenQ;
      End;
    Var
      Front, Back: PGenQ;
      fCount: integer;
      cs: TCriticalSection;
    public
      Property Count: integer read fCount; // Anzahl der Aktuell enthaltenen Elemente
      // Initialisieren
      Constructor create;
      // Freigeben
      Destructor Destroy; override;
      // Leeren
      Procedure Clear;
      // Hinzufügen eines Wertes
      Procedure Push(Value: T);
      // Rückgabe des Obersten Elementes und Löschen
      Function Pop: T;
      // Rückgabe des Obersten Elements
      Function Top: T;
      // Gibt True zurück wenn Leer
      Function isempty: Boolean;
  End;

  FifoException = Class(Exception);

  (*
   * TBufferedFifo
   *
   * Eine via Array realisierte FIFO, welche sich intern bei Bedarf erweitert
   * Vorteil : weniger Speicherallokationen
   * Nachteil : nicht Thread Save (höchstwahrscheinlich)
   *)

  { TBufferedFifo }

  Generic TBufferedFifo < T > = Class
  private
    fBuffer: Array Of T;
    fCount: integer;
    fHead: integer;
    fTail: integer;
  public
    Property Count: integer read fCount; // Anzahl der Aktuell enthaltenen Elemente
    // Initialisieren
    Constructor create; overload; // Ruft Create(16) auf.
    Constructor create(InitialBufferSize: integer); overload;
    // Freigeben
    Destructor Destroy; override;
    // Leeren
    Procedure Clear;
    // Hinzufügen eines Wertes
    Procedure Push(Value: T);
    // Rückgabe des Obersten Elementes und Löschen
    Function Pop: T;
    // Rückgabe des Obersten Elements
    Function Top: T;
    // Gibt True zurück wenn Leer
    Function isempty: Boolean;
  End;

  BufferedFifoException = Class(Exception);

Implementation

{ TFifo }

Constructor TFifo.create;
Begin
  Inherited create;
  Front := Nil;
  back := Nil;
  fCount := 0;
  cs := TCriticalSection.Create;
End;

Destructor TFifo.Destroy;
Begin
  clear;
  cs.Free;
  cs := Nil;
  Inherited Destroy;
End;

Procedure TFifo.Clear;
Var
  bl, bl2: PGenQ;
Begin
  cs.Acquire;
  Try
    If Front <> Nil Then Begin
      bl := Front;
      While bl <> Nil Do Begin
        bl2 := bl;
        bl := bl^.next;
        dispose(bl2);
      End;
      front := Nil;
    End;
    Front := Nil;
    back := Nil;
    fCount := 0;
  Finally
    cs.Release;
  End;
End;

Procedure TFifo.Push(Value: T);
Var
  b: PGenQ;
Begin
  cs.Acquire;
  Try
    inc(fCount);
    new(b);
    b^.Value := value;
    b^.next := Nil;
    If assigned(Front) Then Begin
      // bei exitierender FiFo wir das neue Element hinten angehängt
      Back^.next := b;
      back := b;
    End
    Else Begin
      // Bei einer neuen Fifo werden die Pointer entsprechend Initialisiert
      Front := b;
      back := b;
    End;
  Finally
    cs.Release;
  End;
End;

Function TFifo.Pop: T;
Var
  b: PGenQ;
Begin
  cs.Acquire;
  Try
    If assigned(front) Then Begin
      // Rückgabe des Wertes
      result := Front^.Value;
      // Löschen des Knoten in der Schlange
      b := Front;
      // gehen zum nächsten Element aus der Schlange
      front := front^.next;
      // Freigeben des Speichers
      Dispose(b);
      dec(fCount);
    End
    Else Begin
      // wird von einer Leeren Schlange Gepoppt dann Exception
      Raise FifoException.create('Error queue empty');
    End;
  Finally
    cs.Release;
  End;
End;

Function TFifo.Top: T;
Begin
  If assigned(Front) Then Begin
    // Rückgabe des Wertes
    result := Front^.Value;
  End
  Else Begin
    // wird von einer Leeren Schlange Gepoppt dann Exception
    Raise FifoException.create('Error queue empty');
  End;
End;

Function TFifo.isempty: Boolean;
Begin
  Result := Not assigned(Front);
End;

{ TBufferedFifo }

Constructor TBufferedFifo.create;
Begin
  create(16);
End;

Constructor TBufferedFifo.create(InitialBufferSize: integer);
Begin
  Inherited create();
  If InitialBufferSize <= 2 Then Begin
    Raise BufferedFifoException.Create('Invalid InitialBufferSize, has to be > 2');
  End;
  fHead := 0;
  fTail := 0;
  fCount := 0;
  setlength(fBuffer, InitialBufferSize);
End;

Destructor TBufferedFifo.Destroy;
Begin
  setlength(fbuffer, 0);
  Inherited Destroy;
End;

Procedure TBufferedFifo.Clear;
Begin
  // Die Indexe werden gelöscht, der Puffer bleibt Allokiert => die Query ist Leer ;)
  fHead := 0;
  fTail := 0;
  fCount := 0;
End;

Procedure TBufferedFifo.Push(Value: T);
Var
  Next, olength: integer;
Begin
  Next := (fHead + 1) Mod length(fBuffer);
  If next = fTail Then Begin // Überlauf der Buffer ist Voll und muss erweitert werden
    olength := length(fbuffer);
    // 1. Verdoppeln des bisher genutzten Speichers
    setlength(fBuffer, length(fBuffer) * 2);
    If next <> 0 Then Begin
      // 2. Umkopieren der Datensätze von 0 bis Head in den neuen Speicherbereich
      move(fbuffer[0], fbuffer[olength], (fhead + 1) * sizeof(t));
      FillChar(fbuffer[0], (fhead + 1) * sizeof(t), 0); // Sollte T Referenzcountet sein, dann muss der alte Speicher = 0 gesetzt werden, sonst funktioniert das RefCounting nicht mehr..
      // 3. Anpassen des neuen "Next"
      next := (olength + fHead + 1);
    End
    Else Begin
      next := olength;
    End;
  End;
  // Pushen des Elementes in das Array
  fbuffer[next] := value;
  fHead := next;
  inc(fCount);
End;

Function TBufferedFifo.Pop: T;
Begin
  If isempty() Then Begin
    Raise BufferedFifoException.create('Error queue empty');
  End;
  fTail := (fTail + 1) Mod length(fBuffer);
  result := fBuffer[fTail];
  dec(fCount);
End;

Function TBufferedFifo.Top: T;
Var
  next: integer;
Begin
  If isempty() Then Begin
    Raise BufferedFifoException.create('Error queue empty');
  End;
  next := (fTail + 1) Mod length(fBuffer);
  result := fBuffer[next];
End;

Function TBufferedFifo.isempty: Boolean;
Begin
  result := fHead = fTail;
End;

End.





