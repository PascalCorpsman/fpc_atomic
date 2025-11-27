program tcp_echo_client;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, sockets, DateUtils;

const
  PACKET_SIZE = 100;
  SEND_INTERVAL_MS = 50; // Send every 50ms
  TEST_DURATION_SEC = 30; // Run for 30 seconds

type
  TPacketHeader = packed record
    SequenceNumber: UInt32;
    ClientTimestamp: Int64;
  end;

  TStats = record
    PacketsSent: UInt32;
    PacketsReceived: UInt32;
    MinRTT: Int64;
    MaxRTT: Int64;
    TotalRTT: Int64;
    Timeouts: UInt32;
  end;

var
  ClientSocket: TSocket;
  ServerAddr: TINetSockAddr;
  SendBuffer, RecvBuffer: array[0..PACKET_SIZE-1] of Byte;
  SendHeader, RecvHeader: TPacketHeader;
  SendTime, RecvTime: QWord;
  RTT: Int64;
  Stats: TStats;
  ServerIP: String;
  ServerPort: Word;
  BytesReceived: Integer;
  LogFile: TextFile;
  TestStartTime: QWord;
  SequenceNum: UInt32;

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
  WriteLn(LogFile, '[', GetTimestampStr, '] ', Msg);
  Flush(LogFile);
end;

procedure PrintStats;
var
  AvgRTT: Double;
  LossRate: Double;
begin
  WriteLn('');
  WriteLn('=== STATISTICS ===');
  WriteLn(Format('Packets sent: %d', [Stats.PacketsSent]));
  WriteLn(Format('Packets received: %d', [Stats.PacketsReceived]));
  WriteLn(Format('Timeouts: %d', [Stats.Timeouts]));
  
  if Stats.PacketsSent > 0 then begin
    LossRate := ((Stats.PacketsSent - Stats.PacketsReceived) / Stats.PacketsSent) * 100;
    WriteLn(Format('Loss rate: %.2f%%', [LossRate]));
  end;
  
  if Stats.PacketsReceived > 0 then begin
    AvgRTT := Stats.TotalRTT / Stats.PacketsReceived;
    WriteLn(Format('Min RTT: %dms', [Stats.MinRTT]));
    WriteLn(Format('Max RTT: %dms', [Stats.MaxRTT]));
    WriteLn(Format('Avg RTT: %.1fms', [AvgRTT]));
  end;
  WriteLn('==================');
  WriteLn('');
end;

begin
  // Parse command line
  if ParamCount < 1 then begin
    WriteLn('Usage: tcp_echo_client <server_ip> [port]');
    WriteLn('Example: tcp_echo_client 37.16.31.7 5600');
    Halt(1);
  end;

  ServerIP := ParamStr(1);
  if ParamCount >= 2 then
    ServerPort := StrToIntDef(ParamStr(2), 5600)
  else
    ServerPort := 5600;

  WriteLn('=== TCP Echo Client (RTT Test) ===');
  WriteLn('Server: ', ServerIP, ':', ServerPort);
  WriteLn('Packet size: ', PACKET_SIZE, ' bytes');
  WriteLn('Send interval: ', SEND_INTERVAL_MS, 'ms');
  WriteLn('Test duration: ', TEST_DURATION_SEC, ' seconds');
  WriteLn('');

  // Open log file
  AssignFile(LogFile, 'tcp_echo_client.log');
  Rewrite(LogFile);
  WriteLn(LogFile, '=== TCP Echo Client Log ===');
  WriteLn(LogFile, 'Server: ', ServerIP, ':', ServerPort);
  WriteLn(LogFile, '');
  Flush(LogFile);

  // Create socket
  ClientSocket := fpsocket(AF_INET, SOCK_STREAM, 0);
  if ClientSocket < 0 then begin
    WriteLn('Error: Could not create socket');
    Halt(1);
  end;

  // Set TCP_NODELAY
  SetTCPNoDelay(ClientSocket);
  Log('TCP_NODELAY enabled');

  // Connect to server
  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_port := htons(ServerPort);
  ServerAddr.sin_addr := StrToNetAddr(ServerIP);

  Log('Connecting to server...');
  if fpconnect(ClientSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then begin
    WriteLn('Error: Could not connect to server');
    CloseSocket(ClientSocket);
    Halt(1);
  end;

  Log('Connected! Starting RTT test...');
  WriteLn('');

  // Initialize stats
  FillChar(Stats, SizeOf(Stats), 0);
  Stats.MinRTT := High(Int64);
  
  FillChar(SendBuffer, SizeOf(SendBuffer), 0);
  SequenceNum := 0;
  TestStartTime := GetTickCount64;

  // Main test loop
  while (GetTickCount64 - TestStartTime) < (TEST_DURATION_SEC * 1000) do begin
    // Prepare packet
    SendHeader.SequenceNumber := SequenceNum;
    SendHeader.ClientTimestamp := GetTickCount64;
    Move(SendHeader, SendBuffer[0], SizeOf(SendHeader));

    // Send packet
    SendTime := GetTickCount64;
    if fpsend(ClientSocket, @SendBuffer[0], SizeOf(SendBuffer), 0) < 0 then begin
      Log('Error: Send failed');
      break;
    end;
    Inc(Stats.PacketsSent);

    // Receive echo
    BytesReceived := fprecv(ClientSocket, @RecvBuffer[0], SizeOf(RecvBuffer), 0);
    RecvTime := GetTickCount64;

    if BytesReceived <= 0 then begin
      Log('Connection closed by server');
      break;
    end;

    if BytesReceived <> SizeOf(RecvBuffer) then begin
      Log(Format('Warning: Received %d bytes, expected %d', [BytesReceived, SizeOf(RecvBuffer)]));
      continue;
    end;

    // Parse response
    Move(RecvBuffer[0], RecvHeader, SizeOf(RecvHeader));

    // Verify sequence number
    if RecvHeader.SequenceNumber <> SequenceNum then begin
      Log(Format('⚠️  Sequence mismatch: sent=%d, received=%d', [SequenceNum, RecvHeader.SequenceNumber]));
      Inc(Stats.Timeouts);
    end else begin
      Inc(Stats.PacketsReceived);

      // Calculate RTT
      RTT := RecvTime - SendTime;
      Stats.TotalRTT := Stats.TotalRTT + RTT;

      if RTT < Stats.MinRTT then
        Stats.MinRTT := RTT;
      if RTT > Stats.MaxRTT then
        Stats.MaxRTT := RTT;

      // Log high RTT
      if RTT > 100 then begin
        Log(Format('🔴 HIGH RTT: seq=%d, RTT=%dms', [SequenceNum, RTT]));
      end;

      // Log every 20 packets
      if (Stats.PacketsReceived mod 20) = 0 then begin
        WriteLn(Format('[%s] Sent=%d, Received=%d, Last RTT=%dms, Avg RTT=%.1fms', 
          [GetTimestampStr, Stats.PacketsSent, Stats.PacketsReceived, RTT, 
           Stats.TotalRTT / Stats.PacketsReceived]));
      end;
    end;

    Inc(SequenceNum);

    // Sleep to maintain interval
    Sleep(SEND_INTERVAL_MS);
  end;

  Log('Test completed, closing connection...');
  CloseSocket(ClientSocket);
  
  PrintStats;
  
  WriteLn(LogFile, '');
  WriteLn(LogFile, '=== FINAL STATISTICS ===');
  WriteLn(LogFile, Format('Packets sent: %d', [Stats.PacketsSent]));
  WriteLn(LogFile, Format('Packets received: %d', [Stats.PacketsReceived]));
  WriteLn(LogFile, Format('Timeouts: %d', [Stats.Timeouts]));
  if Stats.PacketsReceived > 0 then begin
    WriteLn(LogFile, Format('Min RTT: %dms', [Stats.MinRTT]));
    WriteLn(LogFile, Format('Max RTT: %dms', [Stats.MaxRTT]));
    WriteLn(LogFile, Format('Avg RTT: %.1fms', [Stats.TotalRTT / Stats.PacketsReceived]));
  end;
  
  CloseFile(LogFile);
  WriteLn('[', GetTimestampStr, '] Log saved to tcp_echo_client.log');
  WriteLn('[', GetTimestampStr, '] Done.');
end.
