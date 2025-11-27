program tcp_time_client;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, lNet;

type
  TTimeClient = class
  private
    FTCPClient: TLTCP;
    FConnected: Boolean;
    FLastPacketTime: QWord;
    FLastPacketID: Int64;
    FPacketsReceived: Int64;
    FTotalLatency: Int64;
    FMinLatency: Int64;
    FMaxLatency: Int64;
    FBurstDetections: Int64;
    FIntervals: array of Integer; // Store last 100 intervals
    FIntervalIndex: Integer;
    procedure OnReceive(aSocket: TLSocket);
    procedure OnConnect(aSocket: TLSocket);
    procedure OnDisconnect(aSocket: TLSocket);
    procedure OnError(const msg: string; aSocket: TLSocket);
    procedure ProcessPacket(const Data: Pointer; const Size: Integer);
    procedure PrintStatistics;
  public
    constructor Create(const AHost: string; const APort: Word);
    destructor Destroy; override;
    procedure Run;
  end;

constructor TTimeClient.Create(const AHost: string; const APort: Word);
begin
  FTCPClient := TLTCP.Create(nil);
  FTCPClient.OnReceive := @OnReceive;
  FTCPClient.OnConnect := @OnConnect;
  FTCPClient.OnDisconnect := @OnDisconnect;
  FTCPClient.OnError := @OnError;
  FTCPClient.Timeout := 5000;
  
  FConnected := False;
  FLastPacketTime := 0;
  FLastPacketID := -1;
  FPacketsReceived := 0;
  FTotalLatency := 0;
  FMinLatency := High(Int64);
  FMaxLatency := 0;
  FBurstDetections := 0;
  SetLength(FIntervals, 100);
  FIntervalIndex := 0;
  
  WriteLn('[TCP Time Client]');
  WriteLn('Connecting to ', AHost, ':', APort);
  
  if not FTCPClient.Connect(AHost, APort) then
  begin
    WriteLn('ERROR: Failed to connect to ', AHost, ':', APort);
    Halt(1);
  end;
end;

destructor TTimeClient.Destroy;
begin
  FTCPClient.Free;
  inherited;
end;

procedure TTimeClient.OnConnect(aSocket: TLSocket);
var
  NoDelay: Boolean;
begin
  WriteLn(Format('[%s] Connected to server!', [FormatDateTime('hh:nn:ss.zzz', Now)]));
  FConnected := True;
  
  // Enable TCP_NODELAY
  NoDelay := True;
  aSocket.SetState(ssNoDelay, NoDelay);
  WriteLn('  TCP_NODELAY enabled');
  WriteLn('');
  WriteLn('Receiving timestamps... (Press Ctrl+C to stop)');
  WriteLn('');
end;

procedure TTimeClient.OnDisconnect(aSocket: TLSocket);
begin
  WriteLn(Format('[%s] Disconnected from server', [FormatDateTime('hh:nn:ss.zzz', Now)]));
  FConnected := False;
  PrintStatistics;
end;

procedure TTimeClient.OnError(const msg: string; aSocket: TLSocket);
begin
  WriteLn(Format('[%s] ERROR: %s', [FormatDateTime('hh:nn:ss.zzz', Now), msg]));
end;

procedure TTimeClient.OnReceive(aSocket: TLSocket);
var
  Buffer: array[0..1023] of Byte;
  BytesRead: Integer;
begin
  repeat
    BytesRead := aSocket.Get(Buffer, SizeOf(Buffer));
    if BytesRead > 0 then
      ProcessPacket(@Buffer, BytesRead);
  until BytesRead <= 0;
end;

procedure TTimeClient.ProcessPacket(const Data: Pointer; const Size: Integer);
type
  TPacketData = packed record
    Timestamp: QWord;
    PacketID: Int64;
  end;
var
  Packet: ^TPacketData;
  CurrentTime: QWord;
  Latency: Int64;
  Interval: Integer;
  i, BurstCount: Integer;
  AvgInterval: Double;
begin
  if Size <> SizeOf(TPacketData) then
  begin
    WriteLn(Format('WARNING: Received invalid packet size: %d bytes (expected %d)', 
      [Size, SizeOf(TPacketData)]));
    Exit;
  end;
  
  Packet := Data;
  CurrentTime := GetTickCount64;
  Latency := Int64(CurrentTime) - Int64(Packet^.Timestamp);
  
  Inc(FPacketsReceived);
  FTotalLatency := FTotalLatency + Latency;
  
  if Latency < FMinLatency then FMinLatency := Latency;
  if Latency > FMaxLatency then FMaxLatency := Latency;
  
  // Calculate interval since last packet
  if FLastPacketTime > 0 then
  begin
    Interval := Integer(CurrentTime - FLastPacketTime);
    
    // Store interval for statistics
    FIntervals[FIntervalIndex] := Interval;
    FIntervalIndex := (FIntervalIndex + 1) mod Length(FIntervals);
    
    // Detect bursting (packet arrived much later than expected 20ms)
    if Interval > 50 then // More than 2.5x expected interval
    begin
      Inc(FBurstDetections);
      
      // Count how many packets we missed (should have arrived during this interval)
      BurstCount := (Interval div 20) - 1;
      
      WriteLn(Format('[%s] ⚠️  BURST DETECTED! Interval=%dms (expected 20ms), latency=%dms, missed ~%d packets', 
        [FormatDateTime('hh:nn:ss.zzz', Now), Interval, Latency, BurstCount]));
    end
    else if (FPacketsReceived mod 50 = 0) then // Print normal stats every 50 packets
    begin
      // Calculate average interval from last 100 packets
      AvgInterval := 0;
      for i := 0 to High(FIntervals) do
        AvgInterval := AvgInterval + FIntervals[i];
      AvgInterval := AvgInterval / Length(FIntervals);
      
      WriteLn(Format('[%s] Packet #%d: latency=%dms, interval=%dms (avg=%.1fms)', 
        [FormatDateTime('hh:nn:ss.zzz', Now), Packet^.PacketID, Latency, Interval, AvgInterval]));
    end;
  end;
  
  FLastPacketTime := CurrentTime;
  FLastPacketID := Packet^.PacketID;
end;

procedure TTimeClient.PrintStatistics;
var
  AvgLatency: Double;
  i: Integer;
  AvgInterval: Double;
  MinInterval, MaxInterval: Integer;
begin
  WriteLn('');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('                    FINAL STATISTICS                       ');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn(Format('Total packets received: %d', [FPacketsReceived]));
  
  if FPacketsReceived > 0 then
  begin
    AvgLatency := FTotalLatency / FPacketsReceived;
    WriteLn(Format('Latency: avg=%.1fms, min=%dms, max=%dms', 
      [AvgLatency, FMinLatency, FMaxLatency]));
    
    // Calculate interval statistics
    AvgInterval := 0;
    MinInterval := High(Integer);
    MaxInterval := 0;
    for i := 0 to High(FIntervals) do
    begin
      AvgInterval := AvgInterval + FIntervals[i];
      if FIntervals[i] < MinInterval then MinInterval := FIntervals[i];
      if FIntervals[i] > MaxInterval then MaxInterval := FIntervals[i];
    end;
    AvgInterval := AvgInterval / Length(FIntervals);
    
    WriteLn(Format('Interval: avg=%.1fms, min=%dms, max=%dms (expected 20ms)', 
      [AvgInterval, MinInterval, MaxInterval]));
    WriteLn(Format('Burst detections: %d (%.1f%%)', 
      [FBurstDetections, (FBurstDetections / FPacketsReceived) * 100]));
    
    if FBurstDetections > 0 then
    begin
      WriteLn('');
      WriteLn('⚠️  WARNING: Bursting detected! Network is buffering packets.');
      WriteLn('   This explains why the game feels "laggy" and time runs slowly.');
    end
    else
    begin
      WriteLn('');
      WriteLn('✅ No bursting detected. Network timing is good!');
    end;
  end;
  WriteLn('═══════════════════════════════════════════════════════════');
end;

procedure TTimeClient.Run;
var
  LastStatsTime: QWord;
begin
  LastStatsTime := GetTickCount64;
  
  while not FConnected do
  begin
    FTCPClient.CallAction;
    Sleep(10);
  end;
  
  while FConnected do
  begin
    FTCPClient.CallAction;
    Sleep(1); // Minimal sleep to avoid 100% CPU
    
    // Print statistics every 10 seconds
    if GetTickCount64 - LastStatsTime >= 10000 then
    begin
      if FPacketsReceived > 0 then
      begin
        WriteLn('');
        WriteLn(Format('--- Stats: %d packets received, avg latency=%.1fms, bursts=%d ---', 
          [FPacketsReceived, FTotalLatency / FPacketsReceived, FBurstDetections]));
        WriteLn('');
      end;
      LastStatsTime := GetTickCount64;
    end;
  end;
end;

var
  Client: TTimeClient;
  Host: string;
  Port: Word;

begin
  if ParamCount >= 1 then
    Host := ParamStr(1)
  else
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <host> [port]');
    WriteLn('Example: ', ExtractFileName(ParamStr(0)), ' 37.16.31.7 5522');
    Halt(1);
  end;
  
  if ParamCount >= 2 then
    Port := StrToIntDef(ParamStr(2), 5522)
  else
    Port := 5522;
  
  Client := TTimeClient.Create(Host, Port);
  try
    Client.Run;
  finally
    Client.Free;
  end;
end.

