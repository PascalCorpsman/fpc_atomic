(******************************************************************************)
(* TChunkmanager                                                   03.06.2015 *)
(*                                                                            *)
(* Version     : 0.16                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This Module encapsulates the TLTCPComponent/ TLTcp to enable *)
(*               sending lots of different datas and get them seperated on    *)
(*               client side.                                                 *)
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
(*               0.02 - Data darf in SendChunk Nil sein.                      *)
(*               0.03 - Einfügen Methode SetNoDelay                           *)
(*               0.04 - Einfügen CallAction für Konsolemodus                  *)
(*               0.05 - Anpassen RegisterConnection für "unregister"          *)
(*                      Integration Logger                                    *)
(*               0.06 - Optionaler MagicHeader                                *)
(*               0.07 - Support für Empfangen von zerteilten Headern          *)
(*               0.08 - Bugfix, Uninitialiserte Variable => Absturz bei FPC 3 *)
(*               0.09 - Bugfix, Memleak bei SendJunk ohne Connected           *)
(*               0.10 - Implementierung Header zerstückelt senden             *)
(*               0.11 - HeaderInformationen in send Chunk reingezogen         *)
(*               0.12 - Entfernen ettlicher schleifen => SpeedUP              *)
(*               0.13 - Fix Nilpointer nach disconnect während receive Event  *)
(*               0.14 - Fix Memleak if server is killed with more than 1      *)
(*                      client connected.                                     *)
(*               0.15 - Fix AV on client disconnect                           *)
(*               0.16 - Disconnect socket instead of raising AV on error      *)
(*                                                                            *)
(******************************************************************************)
Unit uChunkmanager;

{$MODE objfpc}{$H+}

(*
 * Hier können Projektspezifisch Defines gesetzt werden
 *
 * Aktuell sind dies :
 *
 *  {$DEFINE UseLogger} // wenn Aktiv, dann nutzt der Chunkmanager die Logger Routinen
 *  {$DEFINE UseMagicHeader} // Wenn Aktiviert, dann wird jedem Chunk eine Kennung vorangestellt, diese prüft dann zusätzlich noch einmal ob die Daten stimmen
 *
 *)
{$I uchunkmanager.inc}

Interface

Uses
  Classes, SysUtils, lnet, ufifo
{$IFDEF UseLogger}
  , ulogger
{$ENDIF}
  ;

Const
  ChunkSize = 1024 * 2; // Die Typische Block Größe mit der Daten an den TCP-Stack übergeben werden, alles << 65536 Byte ist erlaubt.

Type

  (*
   * Der Datensatz, welcher nach außen Sichtbar ist
   *)
  TChunk = Record
    UID: Integer; // Unique ID des jeweiligen Sockets, zur Identifikation durch die Anwendung
    UserDefinedID: Integer; // Vom Benutzer frei wählbare ID
    Data: TMemoryStream; // Daten welcher Empfangen wurden
  End;

  (*
   * Callback zur Verarbeitung eines Empfangenen Datenblocks
   *
   * !! ACHTUNG !!
   * TChunk.Data darf nicht freigegeben werden. Die Daten müssen weg Kopiert werden, da sie nach dem Aufruf nicht mehr gültig sind.
   *)
  TOnReceivedChunk = Procedure(Sender: TObject; Const Chunk: TChunk) Of Object;

  { TChunkManager

  Typical usage :

    TChunkManager.create
    TChunkManager.RegisterConnection(..)
    TChunkManager.OnReceivedChunk := ..

    TChunkManager.Listen(..)       or       TChunkManager.Connect(..)

    TChunkManager.sendChunk(..)

    TChunkManager.disconnect(..)

    TChunkManager.free;

    ! Attention !
    The TLTCPComponent Component Read and write methods that was registered, shal not be used any more.
    You have to use the TChunkManager methods. Otherwise there could happen strange
    unwanted things.
  }

  TChunkManager = Class
  private
    fIsServer: Boolean; // True = Server, False = Client

    Funique_counter: integer; // EinZähler, welcher jeden Socket hochzählt, Potentielles Sicherheitsrisiko bei 2^32 Sockets

    FOnReceivedChunk: TOnReceivedChunk; // Eventhandler für received Chunks
    fconnection: TLTcp; // the connection to which we were bind

    (*
     * Captured Events
     *)
    FOnAccept_Captured: TLSocketEvent;
    FOnConnect_Captured: TLSocketEvent;
    FOnDisconnect_Captured: TLSocketEvent;

    Function fGetConnected(): Boolean; // Weiterreichen Connected der TLTCPComponent Komponente

    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnConnect(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);

    Procedure OnCanSend(aSocket: TLSocket);
    Procedure OnReceive(aSocket: TLSocket);

    Procedure RegisterSocket(aSocket: TLSocket);
    Procedure UnRegisterSocket(aSocket: TLSocket);

    (*
     * Fügt einen Chunk in die Warteschlange zum Senden ein und Startet ggf. das Senden
     *)
    Procedure AddChunkForSendToSocketQueue(aSocket: TLSocket; UserDefinedID: integer; Const Data: TStream);

    (*
     * Beginnt das Senden des ersten Elementes in der Warteschlange
     *)
    Procedure StartSendChunk(aSocket: TLSocket);

    (*
     * trennt einen Socket und ruft wenn möglich noch OnError mit msg auf
     *)
    Procedure ShutdownSocketWithError(Const msg: String; aSocket: TLSocket);

  public
    Property Connected: Boolean read fGetConnected;

    Property OnReceivedChunk: TOnReceivedChunk write FOnReceivedChunk; // After Calling this routine TChunkManager free's the whole Chunk

    Constructor create;
    Destructor destroy; override;

    (*
     * Bind Chunkmanager to a TLTCPComponent
     * if Connection = nil, then the old one will be unbound.
     *)
    Procedure RegisterConnection(Const Connection: TLTcp); // Captures OnCanSend and Onreceive, the other Events are passed through

    (*
     * Connection control methods
     *)
    Function Connect(Const Address: String; Const APort: Word): Boolean; // Connect as Client
    Function Listen(Const APort: Word; Const AIntf: String = LADDR_ANY): Boolean; // Start a Server
    Procedure Disconnect(Const Forced: Boolean = False); // Disconnect connection

    (*
     * Sendet Data und gibt es anschließend Frei, True, wenn Socket gefunden wurde
     * Im Server Modus :
     *   UID < 0 => Broadcast außer an Clienten mit -UID
     *   UID = 0 => Broadcast
     *   UID > 0 => Nachricht nur an Clienten mit UID
     *
     * Im Client Modus :
     *   UID  wird ignoriert
     *
     * Data darf Nil sein.
     *)
    Function SendChunk(UserDefinedID: Integer; Data: TStream; UID: integer = 0): Boolean;

    (*
     * Ermittelt die zum Socket gehörige UID, welche für SendChunk verwendet werden kann
     * result = 0 => Fehler, bzw. keine UID zum Socket mehr Registrtiert
     * result > 0 => Gültige UID gefunden
     *)
    Function SocketToUID(Const aSocket: TLSocket): integer; // Macht nur für Server Sinn.

    (*
     * Beim Versenden von Daten wartet das OS typischerweise ca. 100ms wenn die zu sendenden
     * Daten zu klein (weniger als 2048 Byte auf Windows) sind.
     * Will man diesen Effekt aufheben (also selbst kleinste Daten immer Sofort versenden),
     * so muss man nach dem Verbinden (oder z.B. im OnConnect, OnAccept)
     * Die NoDelay Option Client wie Serverseitig auf True setzen.
     *)
    Procedure SetNoDelay(Value: Boolean); // Besser wäre es, wenn man hier die SetState Routine zur Verfügung stellt !

    (*
     * Diese Routine muss nur im Konsole Modus aufgerufen werden, bei der Nutzung
     * der TLTCPComponent ist der Aufruf nicht notwendig, bzw füht ins Leere, da hier der LCLEventer das ganze abhandelt.
     *)
    Procedure CallAction();
  End;

Implementation

Uses math;

Const
{$IFDEF UseMagicHeader}
  MagicHeader: Array[0..3] Of byte = (ord('C'), ord('U'), ord('S'), ord('W'));
  HeaderLen = 16;
{$ELSE}
  HeaderLen = 12;
{$ENDIF}

Type

  (*
   * Empfangspuffer für noch unbestimmte Daten
   *)
  TRecvBuf = Record
    // Interne Strukturdaten
    Initialized: Boolean;
    HeaderBytes: Array[0..HeaderLen - 1] Of Byte;
    HeaderBytePos: integer;
    // Chunk bezogene Daten
    UserDefinedId: integer;
    Size: Int64;
    Data: TMemoryStream;
  End;

  (*
   * Inhalt der Warteschlange
   *)
  TSendData = Record
    // Interne Strukturdaten
    HeaderBytes: Array[0..HeaderLen - 1] Of Byte;
    HeaderPos: integer; // Anzahl in Bytes, wieviele Bytes bereits gesendet wurden
    // Chunk bezogene Daten
    Position: int64;
    UserDefinedID: Integer;
    Data: TStream;
  End;

  (*
   * Alle Daten die Versendet werden, laufen durch Fifo's.
   *
   * Beim Senden wird das 1. Element via Top gehohlt und nur bei Vollständiger Bearbeitung
   * via Pop aus der Fifo gelöscht.
   *
   * In dem Fall, wo ein Packet so Groß ist, dass es mehrfach via Top gehohlt werden muss
   * müssen die Zwischenstände in SendData gespeichert werden. Dies ist nur via Pointer
   * realisierbar. Aus diesem Grund darf für die Queue kein TSendData genutzt werden.
   *)
  PSendData = ^TSendData;

  TSendDataQueue = specialize TBufferedFifo < PSendData > ;

  (*
   * Daten, welche an den Socket gebunden werden, für
   *)
  TUserData = Record
    UID: integer; // Unique ID, bestimmt durch den Chunk Manager
    Queue: TSendDataQueue; // Warteschlange mit Anstehenden Packeten zum Versenden
    RecvBuf: TRecvBuf; // Aktueller Empfangspuffer
  End;

  PUserData = ^TUserData;

  { TChunkManager }

Constructor TChunkManager.create;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.create', llTrace);
{$ENDIF}
  Inherited create;
  FOnReceivedChunk := Nil;
  Funique_counter := 1; // 0 ist "ungültige" id
  fIsServer := false;
  fconnection := Nil;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Destructor TChunkManager.destroy;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.destroy', llTrace);
{$ENDIF}
  If Connected Then Begin
    Disconnect(true);
  End;
  RegisterConnection(Nil); // Löschen aller alten Captures..
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Function TChunkManager.fGetConnected: Boolean;
Begin
  If assigned(fconnection) Then Begin
    result := fconnection.Connected;
  End
  Else Begin
    result := false;
  End;
End;

Procedure TChunkManager.OnCanSend(aSocket: TLSocket);
Var
  pu: PUserData;
  send: PSendData;
  sent: integer;
  Data: Array[0..ChunkSize - 1] Of byte;
  j: Integer;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.OnCanSend' + asocket.PeerAddress, llTrace);
{$ENDIF}
  data[0] := 0; // Totaler Quatsch, aber beruhigt den Compiler, sonst gibts bei all den Move Befehlen ne Warnung
  pu := aSocket.UserData;
  If Not (pu^.Queue.isempty) Then Begin
    send := pu^.Queue.Top;
    (*
     * Wenn der Header noch nicht, oder nicht Vollständig übertragen wurde, dann wird dies nun vervollständigt.
     *)
    If send^.HeaderPos <> HeaderLen Then Begin
      j := HeaderLen - send^.HeaderPos;
      move(send^.HeaderBytes[send^.HeaderPos], data[0], j);
      sent := fConnection.Send(data, j, aSocket);
      send^.HeaderPos := send^.HeaderPos + Sent;
      // Der Äußerst Seltene Fall, dass wir den Header nicht am Stück senden konnten ist aufgetreten
      If send^.HeaderPos < HeaderLen Then Begin
{$IFDEF UseLogger}
        logleave;
{$ENDIF}
        exit;
      End;
    End;
    (*
     * Reguläres Versenden von Nutzdaten
     *)
    Repeat
      send^.Data.Position := send^.Position;
      j := min(send^.Data.Size - send^.Position, ChunkSize);
      If j = 0 Then Begin // Wenn es einen Chunk mit 0- Daten gibt, dann darf nicht gesendet werden.
        sent := 0; // Entsprechend muss sich gemerkt werden, dass wir nichts gesendet haben.
      End
      Else Begin
        send^.Data.Read(data[0], j);
        sent := fConnection.Send(data, j, aSocket);
      End;
      send^.Position := send^.Position + sent; // Wegen dieser Zeile müssen in den Fifo's Pointer stehen
      If send^.Position >= send^.Data.Size Then Begin
        pu^.Queue.Pop; // Weglöschen des 1. Elementes, da es nun Abgearbeitet ist
        send^.Data.free;
        dispose(send);
        StartSendChunk(aSocket); // Senden des evtl. nächsten Chunks
        sent := 0; // Abbruch, des Aktuellen Sendens
      End;
    Until (sent = 0);
  End;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.OnReceive(aSocket: TLSocket);
Var
  pu: PUserData;
{$IFDEF UseMagicHeader}
  magicTemp: String;
{$ENDIF}
  data: Array[0..ChunkSize - 1] Of byte;
  recv: integer;
  d: Integer;
  k: Int64;
  j: Integer;
  Chunk: TChunk;
  NeedFurtherReadings: Boolean;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.OnReceive' + asocket.PeerAddress, llTrace);
{$ENDIF}
  pu := aSocket.UserData;
  // Wenn die Gegenstelle Sendet wie blöd und wir uns aber gerade freigegeben
  // haben, kann es vorkommen, das OnReceive nochmal mit Nil aufgerufen wird.
  // In dem Fall lesen wir einfach alles weg und schmeisen es weg
  If Not assigned(pu) Then Begin
    Repeat
      recv := aSocket.get(data, ChunkSize);
    Until recv = 0;
    exit;
  End;
  Repeat
    NeedFurtherReadings := false;
    If Not pu^.RecvBuf.Initialized Then Begin // Empfangen des Headers
      recv := aSocket.Get(data, HeaderLen - pu^.RecvBuf.HeaderBytePos);
      If recv = 0 Then exit;
      NeedFurtherReadings := true;
      move(data[0], pu^.RecvBuf.HeaderBytes[pu^.RecvBuf.HeaderBytePos], recv);
      pu^.RecvBuf.HeaderBytePos := pu^.RecvBuf.HeaderBytePos + recv;
      If pu^.RecvBuf.HeaderBytePos = HeaderLen Then Begin // Der Header ist Vollständig empfangen, dann Auswerten
{$IFDEF UseMagicHeader}
        d := 4;
        If (pu^.RecvBuf.HeaderBytes[0] <> magicHeader[0]) Or
          (pu^.RecvBuf.HeaderBytes[1] <> magicHeader[1]) Or
          (pu^.RecvBuf.HeaderBytes[2] <> magicHeader[2]) Or
          (pu^.RecvBuf.HeaderBytes[3] <> magicHeader[3]) Then Begin
          magicTemp := ''; // prevent compiler warning
          SetLength(magicTemp, 4);
          magicTemp[1] := chr(pu^.RecvBuf.HeaderBytes[0]);
          magicTemp[2] := chr(pu^.RecvBuf.HeaderBytes[1]);
          magicTemp[3] := chr(pu^.RecvBuf.HeaderBytes[2]);
          magicTemp[4] := chr(pu^.RecvBuf.HeaderBytes[3]);
{$IFDEF UseLogger}
          log('Magic Header id invalid: ' + magicTemp, llerror);
{$ENDIF}
          ShutdownSocketWithError('Magic Header id invalid: ' + magicTemp, aSocket);
          exit;
        End;
{$ELSE}
        d := 0;
{$ENDIF}
        k := 0;
        For j := 0 To 3 Do Begin
          k := k Or (pu^.RecvBuf.HeaderBytes[j + d] Shl (8 * j));
        End;
        pu^.RecvBuf.UserDefinedId := k;
        k := 0;
        For j := 0 To 7 Do Begin
          k := k Or (pu^.RecvBuf.HeaderBytes[j + 4 + d] Shl (8 * j));
        End;
        pu^.RecvBuf.Size := k;
        pu^.RecvBuf.Initialized := true;
        (*
         *  Direkt weiter lesen (Muss gemacht werden, wenn Chunk.data.Size = 0 ist)
         *)
        // OnReceive(aSocket); -- Wird via NeedFurtherReadings erledigt !
      End;
    End
    Else Begin // Empfangen der Nutzdaten
      Repeat
        k := pu^.RecvBuf.Size - pu^.RecvBuf.Data.Size;
        If k <> 0 Then Begin // Wenn die Nutzdaten 0-Byte Groß sind, darf nicht weiter gelesen werden..
          recv := aSocket.Get(data, min(ChunkSize, k));
          pu^.RecvBuf.Data.Write(data[0], recv);
          If recv > 0 Then NeedFurtherReadings := true;
        End
        Else Begin
          (*
           * Wir haben hier nichts gelesen, aber eigentlich sollte immer was gelesen werden !
           * => Wir drehen noch ne Runde, da k = 0 war läuft das automatisch in den "Not pu^.RecvBuf.Initialized" Teil oben und dieser
           *    kommt problemlos damit klar dass nichts weiteres mehr "verfügbar" sein kann. Damit ist an dieser stelle auch keine
           *    Endlosschleife !
           *)
          NeedFurtherReadings := true;
        End;
        // Alle Daten für diesen Chunk sind Empfangen
        // Wir übergeben diesen an die Anwendung
        If pu^.RecvBuf.Data.Size >= pu^.RecvBuf.Size Then Begin
          If pu^.RecvBuf.Data.Size > pu^.RecvBuf.Size Then Begin
{$IFDEF UseLogger}
            log('To much data to be read from chunk', llerror);
{$ENDIF}
            ShutdownSocketWithError('To much data to be read from chunk', aSocket);
            exit;
          End;
          If assigned(fOnReceivedChunk) Then Begin
            pu^.RecvBuf.Data.Position := 0;
            Chunk.Data := pu^.RecvBuf.Data;
            Chunk.UID := pu^.UID;
            Chunk.UserDefinedID := pu^.RecvBuf.UserDefinedId;
            fOnReceivedChunk(self, Chunk);
          End;
          // Wenn fOnReceivedChunk Chunkmanager.Disconnect aufruft, dann ist der Pointer nicht mehr gültig, weil schon freigegeben !
          If assigned(asocket.UserData) Then Begin
            pu^.RecvBuf.Initialized := false;
            pu^.RecvBuf.HeaderBytePos := 0;
            pu^.RecvBuf.Data.Clear;
          End
          Else Begin
            NeedFurtherReadings := false; // Da der Socket frei gegeben wurde braucht auch nicht mehr weiter gelesen zu werden !
          End;
          recv := 0;
        End;
      Until (recv = 0);
    End;
  Until Not NeedFurtherReadings;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.RegisterSocket(aSocket: TLSocket);
Var
  pu: PUserData;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.RegisterSocket' + asocket.PeerAddress, llTrace);
{$ENDIF}
  new(pu);
  pu^.UID := Funique_counter;
  pu^.Queue := TSendDataQueue.create;
  pu^.RecvBuf.Initialized := false;
  pu^.RecvBuf.HeaderBytePos := 0;
  pu^.RecvBuf.Data := TMemoryStream.Create;
  Funique_counter := Funique_counter + 1;
  aSocket.UserData := pu;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.UnRegisterSocket(aSocket: TLSocket);
Var
  pu: PUserData;
  send: PSendData;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.UnRegisterSocket' + asocket.PeerAddress, llTrace);
{$ENDIF}
  pu := aSocket.UserData;
  If assigned(pu) Then Begin
    While Not pu^.Queue.isempty Do Begin
      send := pu^.Queue.Pop;
      send^.Data.free;
      send^.Data := Nil;
      dispose(send);
    End;
    pu^.Queue.Free;
    pu^.Queue := Nil;
    pu^.RecvBuf.Data.Free;
    pu^.RecvBuf.Data := Nil;
    dispose(pu);
  End;
  asocket.UserData := Nil;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.AddChunkForSendToSocketQueue(aSocket: TLSocket;
  UserDefinedID: integer; Const Data: TStream);
Var
  pu: PUserData;
  send: PSendData;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.AddChunkForSendToSocketQueue' + asocket.PeerAddress, llTrace);
{$ENDIF}
  If Not (fconnection.Iterator.ConnectionStatus = scConnected) Then Begin
    data.free;
{$IFDEF UseLogger}
    logleave;
{$ENDIF}
    exit;
  End;
  pu := fconnection.Iterator.UserData;
  new(send);
  send^.Position := 0;
  send^.Data := data;
  send^.UserDefinedID := UserDefinedID;
  If pu^.Queue.isempty Then Begin // Wenn die Queue Leer ist, wird gerade nichts gesendet, und das senden muss gestartet werden
    pu^.Queue.Push(send);
    StartSendChunk(aSocket);
  End
  Else Begin // Wir Senden bereits, dann einfach nur in die Warteschlange eintragen.
    pu^.Queue.Push(send);
  End;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.StartSendChunk(aSocket: TLSocket);
Var
  pu: PUserData;
  i, j, d: integer;
  k: int64;
  send: PSendData;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.StartSendChunk' + asocket.PeerAddress, llTrace);
{$ENDIF}
  pu := aSocket.UserData;
  If Not (aSocket.ConnectionStatus = scConnected) Then Begin // Der Socket ist schon gar nicht mehr verbunden, dann braucht auch nicht mehr gesendet werden..
    While Not pu^.Queue.isempty Do Begin // Löschen der Send Queue
      send := pu^.Queue.Pop;
      send^.Data.free;
      dispose(send);
    End;
{$IFDEF UseLogger}
    logleave;
{$ENDIF}
    exit;
  End;
  If Not pu^.Queue.isempty Then Begin
    // Berechnen des Headers
    send := pu^.Queue.Top;
{$IFDEF UseMagicHeader}
    send^.HeaderBytes[0] := magicHeader[0];
    send^.HeaderBytes[1] := magicHeader[1];
    send^.HeaderBytes[2] := magicHeader[2];
    send^.HeaderBytes[3] := magicHeader[3];
    d := 4;
{$ELSE}
    d := 0;
{$ENDIF}
    i := send^.UserDefinedID;
    For j := 0 To 3 Do Begin
      send^.HeaderBytes[j + d] := i And $FF;
      i := i Shr 8;
    End;
    k := send^.Data.Size;
    For j := 0 To 7 Do Begin
      send^.HeaderBytes[j + 4 + d] := k And $FF;
      k := k Shr 8;
    End;
    send^.HeaderPos := 0; // Initialisieren, dass der Header noch nicht gesendet wurde
    // Und Ab damit
    OnCanSend(asocket);
  End;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.ShutdownSocketWithError(Const msg: String;
  aSocket: TLSocket);
Var
  recv: Integer;
  data: Array[0..ChunkSize - 1] Of byte;
Begin
  // Wir Melden den Fehler
  If assigned(fconnection.OnError) Then Begin
    fconnection.OnError(msg, aSocket);
  End;
  // Lesen den Puffer Leer und schmeisen alles weg was da so kommt..
  Repeat
    recv := aSocket.get(data, ChunkSize);
  Until recv = 0;
  // Trennen der Verbindung
  aSocket.Disconnect(true);
End;

Procedure TChunkManager.OnAccept(aSocket: TLSocket);
Begin
{$IFDEF UseLogger}
  log('TChunkManager.OnAccept' + asocket.PeerAddress, llTrace);
{$ENDIF}
  // Der Server Akzeptiert Verbindungen
  RegisterSocket(aSocket);
  If assigned(FOnAccept_Captured) Then
    FOnAccept_Captured(aSocket);
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.OnConnect(aSocket: TLSocket);
Begin
{$IFDEF UseLogger}
  log('TChunkManager.OnConnect' + asocket.PeerAddress, llTrace);
{$ENDIF}
  // Der Client Verbindet sich
  RegisterSocket(aSocket);
  If assigned(FOnConnect_Captured) Then
    FOnConnect_Captured(aSocket);
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.OnDisconnect(aSocket: TLSocket);
Begin
{$IFDEF UseLogger}
  log('TChunkManager.OnDisconnect' + asocket.PeerAddress, llTrace);
{$ENDIF}
  If assigned(FOnDisconnect_Captured) Then
    FOnDisconnect_Captured(aSocket);
  UnRegisterSocket(asocket);
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.RegisterConnection(Const Connection: TLTcp);
Begin
{$IFDEF UseLogger}
  log('TChunkManager.RegisterConnection', llTrace);
{$ENDIF}
  If Fconnection <> Connection Then Begin
    // Alte Captures aushängen
    If assigned(Fconnection) Then Begin
      fconnection.OnAccept := FOnAccept_Captured;
      fconnection.OnConnect := FOnConnect_Captured;
      fconnection.OnDisconnect := FOnDisconnect_Captured;
      fconnection.OnCanSend := Nil;
      fconnection.OnReceive := Nil;
    End;
    fconnection := Connection;
    // Neue Captures einhängen, so denn nicht Fconnection gelöscht wurde.
    If assigned(Fconnection) Then Begin
      FOnAccept_Captured := fconnection.OnAccept;
      FOnConnect_Captured := fconnection.OnConnect;
      FOnDisconnect_Captured := fconnection.OnDisconnect;

      fconnection.OnAccept := @OnAccept;
      fconnection.OnConnect := @OnConnect;
      fconnection.OnDisconnect := @OnDisconnect;

      fconnection.OnCanSend := @OnCanSend;
      fconnection.OnReceive := @OnReceive;
    End;
  End;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Function TChunkManager.Connect(Const Address: String; Const APort: Word
  ): Boolean;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.Connect : ' + Address + ':' + inttostr(aPort), llTrace);
{$ENDIF}
  result := false;
  If Not assigned(fconnection) Then exit;
  fIsServer := false;
  result := fconnection.Connect(Address, APort);
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Function TChunkManager.Listen(Const APort: Word; Const AIntf: String): Boolean;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.Listen : ' + inttostr(aPort), llTrace);
{$ENDIF}
  result := false;
  If Not assigned(fconnection) Then exit;
  fIsServer := true;
  result := fconnection.Listen(APort, AIntf);
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.Disconnect(Const Forced: Boolean);
Begin
{$IFDEF UseLogger}
  log('TChunkManager.Disconnect : ' + inttostr(ord(Forced)), llTrace);
{$ENDIF}
  If assigned(fconnection) Then Begin
    // Freigeben aller User Daten
    If fIsServer Then Begin
      fconnection.IterReset;
      While fconnection.IterNext Do Begin
        If assigned(fconnection.Iterator) Then Begin
          OnDisconnect(fconnection.Iterator);
        End;
        UnRegisterSocket(fconnection.Iterator);
      End;
    End
    Else Begin
      fconnection.IterReset;
      If assigned(fconnection.Iterator) Then Begin
        OnDisconnect(fconnection.Iterator);
        UnRegisterSocket(fconnection.Iterator);
      End;
    End;
    fconnection.Disconnect(Forced);
  End;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Function TChunkManager.SendChunk(UserDefinedID: Integer; Data: TStream;
  UID: integer): Boolean;
Var
  m: TMemoryStream;
  b: Boolean;
  pu: PUserData;
Begin
{$IFDEF UseLogger}
  log('TChunkManager.SendChunk : ' + inttostr(UserDefinedID) + ', ' + inttostr(UID), llTrace);
{$ENDIF}
  // Wenn Data einfach erzeugt wird wenn der Nil Pointer da ist, dann sparen wir uns eine Menge Fallunterscheidungen für diesen Sonderfall.
  If Not assigned(data) Then Begin
    data := TMemoryStream.Create;
  End;
  result := false;
  If Not Connected Then Begin // Wir sind gar nicht verbunden, also kann auch nicht gesendet werden
    data.free;
    exit;
  End;
  If fIsServer Then Begin
    // Die Verbindung ist im Server Modus
    If (UID <= 0) Then Begin // Senden An Alle
      fconnection.IterReset;
      While fconnection.IterNext Do Begin // Skipt Root Socket
        pu := fconnection.Iterator.UserData;
        If assigned(pu) And (-uid <> pu^.UID) Then Begin
          // Jeder Socket bekommt eine Kopie der Daten, so können sie alle
          // unterschiedlich schnell arbeiten und jeder kann wie er es will
          // Seine Daten verwalten.
          m := TMemoryStream.Create;
          If data.Size <> 0 Then Begin
            data.Position := 0;
            m.CopyFrom(data, data.Size);
          End;
          AddChunkForSendToSocketQueue(fconnection.Iterator, UserDefinedID, m);
          result := true; // Es gab mindestens einen Clienten an den wir was senden konnten
        End;
      End;
      data.free;
    End
    Else Begin // Senden an den User mit der UserID
      b := false;
      fconnection.IterReset;
      // Suchen des Sockets mit der UserDefinedID
      While fconnection.IterNext Do Begin // Skipt Root Socket
        pu := fconnection.Iterator.UserData;
        If pu^.UID = UID Then Begin
          AddChunkForSendToSocketQueue(fconnection.Iterator, UserDefinedID, Data);
          result := true;
          b := true;
          break;
        End;
      End;
      If Not b Then Begin // Die Daten wurden nicht versand, also löschen wir sie hier händisch
        data.free;
      End;
    End;
  End
  Else Begin
    // Die Verbindung ist im Client Modus, da muss die Uid <> 0 sein, bzw. Gibt es nur eine, Keine ?
    fconnection.IterReset;
    If assigned(fconnection.Iterator) Then Begin // Wenn der Client überhaupt verbunden ist.
      AddChunkForSendToSocketQueue(fconnection.Iterator, UserDefinedID, Data);
      result := true;
    End;
  End;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Function TChunkManager.SocketToUID(Const aSocket: TLSocket): integer;
Var
  pu: PUserData;
Begin
  result := 0;
  If fIsServer Then Begin
    fconnection.IterReset;
    While fconnection.IterNext Do Begin // Skipt Root Socket
      If fconnection.Iterator = aSocket Then Begin
        pu := aSocket.UserData;
        If assigned(pu) Then Begin
          result := PU^.UID;
        End;
        exit;
      End;
    End;
  End;
End;

Procedure TChunkManager.SetNoDelay(Value: Boolean);
Begin
{$IFDEF UseLogger}
  log('TChunkManager.SetNoDelay', llTrace);
{$ENDIF}
  fconnection.IterReset;
  fconnection.Iterator.SetState(ssNoDelay, Value);
  While fconnection.IterNext Do Begin // Skipt Root Socket
    fconnection.Iterator.SetState(ssNoDelay, Value);
  End;
{$IFDEF UseLogger}
  logleave;
{$ENDIF}
End;

Procedure TChunkManager.CallAction;
Begin
  If assigned(fconnection) Then Begin
    fconnection.CallAction;
  End;
End;

End.

