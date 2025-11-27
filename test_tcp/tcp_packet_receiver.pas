program tcp_packet_receiver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, sockets, DateUtils;

const
  PACKET_SIZE = 100;
  BURST_THRESHOLD_MS = 50; // If interval > 50ms, consider it a burst

type
  TPacketHeader = packed record
    SequenceNumber: UInt32;
    ServerTimestamp: Int64;
  end;

  TStats = record
    PacketsReceived: UInt32;
    PacketsLost: UInt32;
    MinInterval: Int64;
    MaxInterval: Int64;
    TotalInterval: Int64;
    BurstCount: UInt32;
    LastSequence: UInt32;
  end;

var
  ClientSocket: TSocket;
  ServerAddr: TINetSockAddr;
  PacketData: array[0..PACKET_SIZE-1] of Byte;
  Header: TPacketHeader;
  RecvTime, LastRecvTime: QWord;
  Interval: Int64;
  Stats: TStats;
  ServerIP: String;
  ServerPort: Word;
  BytesReceived: Integer;
  LogFile: TextFile;

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
  AvgInterval: Double;
  LossRate: Double;
begin
  WriteLn('');
  WriteLn('=== STATISTICS ===');
  WriteLn(Format('Packets received: %d', [Stats.PacketsReceived]));
  WriteLn(Format('Packets lost: %d', [Stats.PacketsLost]));
  
  if Stats.PacketsReceived > 0 then begin
    LossRate := (Stats.PacketsLost / (Stats.PacketsReceived + Stats.PacketsLost)) * 100;
    WriteLn(Format('Loss rate: %.2f%%', [LossRate]));
  end;
  
  if Stats.PacketsReceived > 1 then begin
    AvgInterval := Stats.TotalInterval / (Stats.PacketsReceived - 1);
    WriteLn(Format('Min interval: %dms', [Stats.MinInterval]));
    WriteLn(Format('Max interval: %dms', [Stats.MaxInterval]));
    WriteLn(Format('Avg interval: %.1fms', [AvgInterval]));
    WriteLn(Format('Bursts detected (>%dms): %d', [BURST_THRESHOLD_MS, Stats.BurstCount]));
  end;
  WriteLn('==================');
  WriteLn('');
end;

begin
  // Parse command line
  if ParamCount < 1 then begin
    WriteLn('Usage: tcp_packet_receiver <server_ip> [port]');
    WriteLn('Example: tcp_packet_receiver 37.16.31.7 5599');
    Halt(1);
  end;

  ServerIP := ParamStr(1);
  if ParamCount >= 2 then
    ServerPort := StrToIntDef(ParamStr(2), 5599)
  else
    ServerPort := 5599;

  WriteLn('=== TCP Packet Receiver (Client) ===');
  WriteLn('Connecting to ', ServerIP, ':', ServerPort);
  WriteLn('');

  // Open log file
  AssignFile(LogFile, 'tcp_receiver.log');
  Rewrite(LogFile);
  WriteLn(LogFile, '=== TCP Packet Receiver Log ===');
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

  Log('Connected! Receiving packets...');
  Log('Press Ctrl+C to stop');
  WriteLn('');

  // Initialize stats
  FillChar(Stats, SizeOf(Stats), 0);
  Stats.MinInterval := High(Int64);
  Stats.LastSequence := High(UInt32); // Invalid value
  LastRecvTime := 0;

  // Main receiving loop
  while True do begin
    // Receive packet
    BytesReceived := fprecv(ClientSocket, @PacketData[0], SizeOf(PacketData), 0);
    RecvTime := GetTickCount64;

    if BytesReceived <= 0 then begin
      Log('Connection closed by server');
      break;
    end;

    if BytesReceived <> SizeOf(PacketData) then begin
      Log(Format('Warning: Received %d bytes, expected %d', [BytesReceived, SizeOf(PacketData)]));
      continue;
    end;

    // Parse header
    Move(PacketData[0], Header, SizeOf(Header));

    // Check for packet loss
    if Stats.LastSequence <> High(UInt32) then begin
      if Header.SequenceNumber > Stats.LastSequence + 1 then begin
        Stats.PacketsLost := Stats.PacketsLost + (Header.SequenceNumber - Stats.LastSequence - 1);
        Log(Format('⚠️  PACKET LOSS: Expected seq=%d, got seq=%d (lost %d packets)', 
          [Stats.LastSequence + 1, Header.SequenceNumber, Header.SequenceNumber - Stats.LastSequence - 1]));
      end;
    end;
    Stats.LastSequence := Header.SequenceNumber;

    Inc(Stats.PacketsReceived);

    // Calculate interval
    if LastRecvTime > 0 then begin
      Interval := RecvTime - LastRecvTime;
      Stats.TotalInterval := Stats.TotalInterval + Interval;

      if Interval < Stats.MinInterval then
        Stats.MinInterval := Interval;
      if Interval > Stats.MaxInterval then
        Stats.MaxInterval := Interval;

      // Detect burst
      if Interval > BURST_THRESHOLD_MS then begin
        Inc(Stats.BurstCount);
        Log(Format('🔴 BURST DETECTED: seq=%d, interval=%dms (expected 20ms)', 
          [Header.SequenceNumber, Interval]));
      end;

      // Log every 50 packets
      if (Stats.PacketsReceived mod 50) = 0 then begin
        WriteLn(Format('[%s] Received %d packets, last interval=%dms, bursts=%d, lost=%d', 
          [GetTimestampStr, Stats.PacketsReceived, Interval, Stats.BurstCount, Stats.PacketsLost]));
      end;
    end;

    LastRecvTime := RecvTime;
  end;

  Log('Closing connection...');
  CloseSocket(ClientSocket);
  
  PrintStats;
  
  WriteLn(LogFile, '');
  WriteLn(LogFile, '=== FINAL STATISTICS ===');
  WriteLn(LogFile, Format('Packets received: %d', [Stats.PacketsReceived]));
  WriteLn(LogFile, Format('Packets lost: %d', [Stats.PacketsLost]));
  if Stats.PacketsReceived > 1 then begin
    WriteLn(LogFile, Format('Min interval: %dms', [Stats.MinInterval]));
    WriteLn(LogFile, Format('Max interval: %dms', [Stats.MaxInterval]));
    WriteLn(LogFile, Format('Avg interval: %.1fms', [Stats.TotalInterval / (Stats.PacketsReceived - 1)]));
    WriteLn(LogFile, Format('Bursts detected: %d', [Stats.BurstCount]));
  end;
  
  CloseFile(LogFile);
  WriteLn('[', GetTimestampStr, '] Log saved to tcp_receiver.log');
  WriteLn('[', GetTimestampStr, '] Done.');
end.

