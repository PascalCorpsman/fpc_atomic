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
Unit ulanscan;

{$MODE ObjFPC}{$H+}

Interface

(*
 * Search of the launcher for network games in the local network, the same as the game does with "Join network game":
 * a "Ping" via UDP to UDPPingPort goes to the broadcast address of every network, to 255.255.255.255 and to 127.0.0.1,
 * every server that waits for players answers with "<name>:<port>" and a check byte (see TServer.OnUDPReceiveEvent).
 *
 * Does not block: Poll is called by a timer, it sends the pings once per second and reads the answers that arrived.
 * The socket uses a free port of the OS, not UDPPingPort, so a server on the same PC can still listen there.
 *)

Uses
  Classes, SysUtils, blcksock;

Type

  TLanGame = Record
    Name: String; // e.g. "Player 1s game" or "no user connected", as the server sends it
    IP: String; // the address the answer came from, 127.0.0.1 for a server on this PC
    Port: integer; // TCP port of the server
    LastSeen: QWord; // GetTickCount64 of the last answer
  End;

  TLanGames = Array Of TLanGame;

  { TLanScanner }

  TLanScanner = Class
  private
    fSocket: TUDPBlockSocket;
    fGames: TLanGames;
    fOwnIPs: TStringList; // the addresses of this PC, answers from there are shown as 127.0.0.1
    fLastPing: QWord;
    Procedure SendPings;
    Function ReadAnswers: Boolean;
    Function RemoveOldGames: Boolean;
  public
    Property Games: TLanGames read fGames;
    Constructor Create;
    Destructor Destroy; override;
    (*
     * Sends the pings if due and reads the answers, true if the list of games changed
     *)
    Function Poll: Boolean;
  End;

(*
 * Checks an answer of a server and splits it into name and port
 *)
Function DecodePingAnswer(Const Data: AnsiString; Out Name: String; Out Port: integer): Boolean;

(*
 * The key of a game, IP:Port
 *)
Function LanGameKey(Const Game: TLanGame): String;

Implementation

Uses uip, uatomic_common, uatomic_global;

Const
  PingInterval = 1000; // ms between two pings
  GameTimeout = 3500; // ms without answer, then the game is gone (3 pings missed)

Function DecodePingAnswer(Const Data: AnsiString; Out Name: String; Out Port: integer): Boolean;
Var
  b: Byte;
  i: integer;
  s: String;
Begin
  result := false;
  Name := '';
  Port := -1;
  If length(Data) < 2 Then exit;
  b := 21; // like the client, CTD uses 42
  For i := 1 To length(Data) Do
    b := b Xor ord(Data[i]);
  If b <> 0 Then exit;
  s := copy(Data, 1, length(Data) - 1);
  i := length(s);
  While (i > 0) And (s[i] <> ':') Do
    dec(i);
  If i = 0 Then exit;
  Port := strtointdef(copy(s, i + 1, length(s)), -1);
  If (Port <= 0) Or (Port > 65535) Then exit;
  Name := copy(s, 1, i - 1);
  result := true;
End;

Function LanGameKey(Const Game: TLanGame): String;
Begin
  result := Game.IP + ':' + inttostr(Game.Port);
End;

{ TLanScanner }

Constructor TLanScanner.Create;
Begin
  Inherited Create;
  fGames := Nil;
  fLastPing := 0;
  fOwnIPs := TStringList.Create;
  fSocket := TUDPBlockSocket.Create;
  Try
    // With the default family SF_Any synapse creates no socket for Bind('0.0.0.0', '0'), so IPv4 (broadcasts only exist there)
    fSocket.Family := SF_IP4;
    fSocket.CreateSocket;
    fSocket.EnableBroadcast(true);
    fSocket.Bind('0.0.0.0', '0');
    If fSocket.LastError <> 0 Then Raise Exception.Create(fSocket.LastErrorDesc);
    // Windows: the ping to 127.0.0.1 without a local server would let the next receive fail with WSAECONNRESET
    IgnoreUdpConnReset(PtrInt(fSocket.Socket));
  Except
    On e: Exception Do Begin
      log('TLanScanner.Create: could not create the UDP socket: ' + e.Message, llWarning);
      FreeAndNil(fSocket);
    End;
  End;
End;

Destructor TLanScanner.Destroy;
Begin
  fSocket.Free;
  fOwnIPs.Free;
  Inherited Destroy;
End;

Procedure TLanScanner.SendPings;
Var
  n: TNetworkAdapterList;
  Targets: TStringList;
  i: integer;
Begin
  Targets := TStringList.Create;
  Try
    Targets.Add('255.255.255.255');
    Targets.Add('127.0.0.1');
    fOwnIPs.Clear;
    n := GetLocalIPs();
    For i := 0 To high(n) Do Begin
      fOwnIPs.Add(n[i].IpAddress);
      Try
        Targets.Add(CalculateBroadCastAddressFromAddress(n[i].IpAddress, n[i].SubnetMask));
      Except
        // invalid mask, this network is skipped
      End;
    End;
    For i := 0 To Targets.Count - 1 Do Begin
      fSocket.SetRemoteSin(Targets[i], inttostr(UDPPingPort));
      fSocket.SendString('Ping');
    End;
  Finally
    Targets.Free;
  End;
End;

Function TLanScanner.ReadAnswers: Boolean;
Var
  Data: AnsiString;
  aName, aIP: String;
  aPort, i: integer;
Begin
  result := false;
  While fSocket.WaitingData > 0 Do Begin
    Data := fSocket.RecvPacket(0);
    If fSocket.LastError <> 0 Then break;
    If Not DecodePingAnswer(Data, aName, aPort) Then Continue;
    aIP := fSocket.GetRemoteSinIP;
    // A server on this PC answers on 127.0.0.1 and on its network addresses, it is listed only once
    If fOwnIPs.IndexOf(aIP) <> -1 Then aIP := '127.0.0.1';
    i := 0;
    While (i <= high(fGames)) And ((fGames[i].IP <> aIP) Or (fGames[i].Port <> aPort)) Do
      inc(i);
    If i > high(fGames) Then Begin
      setlength(fGames, length(fGames) + 1);
      fGames[i].IP := aIP;
      fGames[i].Port := aPort;
      fGames[i].Name := '';
      log('Found network game: ' + aName + ' ' + aIP + ':' + inttostr(aPort), llInfo);
    End;
    If fGames[i].Name <> aName Then Begin
      fGames[i].Name := aName;
      result := true;
    End;
    fGames[i].LastSeen := GetTickCount64;
  End;
End;

Function TLanScanner.RemoveOldGames: Boolean;
Var
  i, j: integer;
  t: QWord;
Begin
  result := false;
  t := GetTickCount64;
  For i := high(fGames) Downto 0 Do Begin
    If t - fGames[i].LastSeen > GameTimeout Then Begin
      log('Network game is gone: ' + fGames[i].Name + ' ' + LanGameKey(fGames[i]), llInfo);
      For j := i To high(fGames) - 1 Do
        fGames[j] := fGames[j + 1];
      setlength(fGames, high(fGames));
      result := true;
    End;
  End;
End;

Function TLanScanner.Poll: Boolean;
Begin
  result := false;
  If Not assigned(fSocket) Then exit;
  Try
    If GetTickCount64 - fLastPing >= PingInterval Then Begin
      fLastPing := GetTickCount64;
      SendPings;
    End;
    result := ReadAnswers;
  Except
    On e: Exception Do
      log('TLanScanner.Poll: ' + e.Message, llWarning);
  End;
  If RemoveOldGames Then result := true;
End;

End.
