program tcp_packet_sender;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, sockets, DateUtils;

const
  SERVER_PORT = 5599;
  PACKET_INTERVAL_MS = 20; // Send packet every 20ms (50 FPS like game)
  PACKET_SIZE = 100; // Small packet (similar to heartbeat)

type
  TPacketHeader = packed record
    SequenceNumber: UInt32;
    ServerTimestamp: Int64; // GetTickCount64
  end;

var
  ServerSocket, ClientSocket: TSocket;
  ServerAddr, ClientAddr: TINetSockAddr;
  ClientAddrLen: TSockLen;
  PacketNum: UInt32;
  StartTime, LastSendTime: QWord;
  Header: TPacketHeader;
  PacketData: array[0..PACKET_SIZE-1] of Byte;
  SendInterval, ActualInterval: Int64;
  PacketsSent: UInt32;
  ReuseAddr: Integer;

function GetTimestampStr: String;
var
  dt: TDateTime;
begin
  dt := Now;
  Result := FormatDateTime('hh:nn:ss.zzz', dt);
end;

procedure SetTCPNoDelay(Sock: TSocket);
var
  Flag: Integer;
begin
  Flag := 1;
  if fpsetsockopt(Sock, IPPROTO_TCP, TCP_NODELAY, @Flag, SizeOf(Flag)) <> 0 then
    WriteLn('[', GetTimestampStr, '] Warning: Could not set TCP_NODELAY');
end;

procedure Log(const Msg: String);
begin
  WriteLn('[', GetTimestampStr, '] ', Msg);
end;

begin
  WriteLn('=== TCP Packet Sender (Server) ===');
  WriteLn('Will send ', PACKET_SIZE, ' byte packets every ', PACKET_INTERVAL_MS, 'ms');
  WriteLn('Listening on port ', SERVER_PORT);
  WriteLn('');

  // Create server socket
  ServerSocket := fpsocket(AF_INET, SOCK_STREAM, 0);
  if ServerSocket < 0 then begin
    WriteLn('Error: Could not create socket');
    Halt(1);
  end;

  // Allow address reuse
  ReuseAddr := 1;
  fpsetsockopt(ServerSocket, SOL_SOCKET, SO_REUSEADDR, @ReuseAddr, SizeOf(ReuseAddr));

  // Bind to port
  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_port := htons(SERVER_PORT);
  ServerAddr.sin_addr.s_addr := INADDR_ANY;
  
  if fpbind(ServerSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then begin
    WriteLn('Error: Could not bind to port ', SERVER_PORT);
    CloseSocket(ServerSocket);
    Halt(1);
  end;

  // Listen
  if fplisten(ServerSocket, 1) < 0 then begin
    WriteLn('Error: Could not listen');
    CloseSocket(ServerSocket);
    Halt(1);
  end;

  Log('Server started, waiting for client connection...');

  // Accept connection
  ClientAddrLen := SizeOf(ClientAddr);
  ClientSocket := fpaccept(ServerSocket, @ClientAddr, @ClientAddrLen);
  if ClientSocket < 0 then begin
    WriteLn('Error: Could not accept connection');
    CloseSocket(ServerSocket);
    Halt(1);
  end;

  Log('Client connected from ' + NetAddrToStr(ClientAddr.sin_addr));
  
  // Set TCP_NODELAY (disable Nagle's algorithm)
  SetTCPNoDelay(ClientSocket);
  Log('TCP_NODELAY enabled');

  // Initialize packet data
  FillChar(PacketData, SizeOf(PacketData), 0);
  
  PacketNum := 0;
  PacketsSent := 0;
  StartTime := GetTickCount64;
  LastSendTime := StartTime;

  Log('Starting packet transmission...');
  Log('Press Ctrl+C to stop');
  WriteLn('');

  // Main sending loop
  while True do begin
    // Prepare packet
    Header.SequenceNumber := PacketNum;
    Header.ServerTimestamp := GetTickCount64;
    Move(Header, PacketData[0], SizeOf(Header));

    // Send packet
    if fpsend(ClientSocket, @PacketData[0], SizeOf(PacketData), 0) < 0 then begin
      Log('Error: Send failed, client disconnected?');
      break;
    end;

    Inc(PacketNum);
    Inc(PacketsSent);

    // Log every 50 packets (every 1 second at 50 FPS)
    if (PacketNum mod 50) = 0 then begin
      ActualInterval := GetTickCount64 - LastSendTime;
      WriteLn(Format('[%s] Sent %d packets, avg interval=%.1fms', 
        [GetTimestampStr, PacketsSent, ActualInterval / 50.0]));
      LastSendTime := GetTickCount64;
    end;

    // Sleep to maintain interval
    SendInterval := PACKET_INTERVAL_MS - (GetTickCount64 - Header.ServerTimestamp);
    if SendInterval > 0 then
      Sleep(SendInterval);
  end;

  Log('Closing connection...');
  CloseSocket(ClientSocket);
  CloseSocket(ServerSocket);
  
  WriteLn('');
  Log(Format('Total packets sent: %d', [PacketsSent]));
  Log('Done.');
end.

