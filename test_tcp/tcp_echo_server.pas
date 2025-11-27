program tcp_echo_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, sockets, DateUtils;

const
  SERVER_PORT = 5600;
  BUFFER_SIZE = 1024;

var
  ServerSocket, ClientSocket: TSocket;
  ServerAddr, ClientAddr: TINetSockAddr;
  ClientAddrLen: TSockLen;
  Buffer: array[0..BUFFER_SIZE-1] of Byte;
  BytesReceived, BytesSent: Integer;
  PacketsEchoed: UInt32;
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
  WriteLn('=== TCP Echo Server ===');
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

  Log('Server started, waiting for connections...');
  Log('Press Ctrl+C to stop server');
  WriteLn('');

  // Main server loop - accept multiple clients
  while True do begin
    // Accept connection
    ClientAddrLen := SizeOf(ClientAddr);
    ClientSocket := fpaccept(ServerSocket, @ClientAddr, @ClientAddrLen);
    if ClientSocket < 0 then begin
      Log('Warning: Could not accept connection, retrying...');
      Sleep(100);
      continue;
    end;

    Log('Client connected from ' + NetAddrToStr(ClientAddr.sin_addr));
    
    // Set TCP_NODELAY
    SetTCPNoDelay(ClientSocket);
    Log('TCP_NODELAY enabled');
    Log('Echo mode active (echoing all received data)...');
    WriteLn('');

    PacketsEchoed := 0;

    // Echo loop for this client
    while True do begin
      // Receive data
      BytesReceived := fprecv(ClientSocket, @Buffer[0], SizeOf(Buffer), 0);
      
      if BytesReceived <= 0 then begin
        Log('Client disconnected');
        break;
      end;

      // Echo back immediately
      BytesSent := fpsend(ClientSocket, @Buffer[0], BytesReceived, 0);
      
      if BytesSent < 0 then begin
        Log('Error: Send failed');
        break;
      end;

      Inc(PacketsEchoed);

      // Log every 50 packets
      if (PacketsEchoed mod 50) = 0 then begin
        WriteLn(Format('[%s] Echoed %d packets', [GetTimestampStr, PacketsEchoed]));
      end;
    end;

    Log(Format('Session ended. Total packets echoed: %d', [PacketsEchoed]));
    CloseSocket(ClientSocket);
    WriteLn('');
    Log('Waiting for next client...');
    WriteLn('');
  end;

  // This is never reached, but just in case:
  CloseSocket(ServerSocket);
  Log('Server stopped.');
end.
