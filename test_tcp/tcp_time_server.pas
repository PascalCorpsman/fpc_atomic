program tcp_time_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, lNet;

type
  TTimeServer = class
  private
    FTCPServer: TLTCP;
    FRunning: Boolean;
    FPacketsSent: Int64;
    FStartTime: QWord;
    FLastReportTime: QWord;
    procedure OnAccept(aSocket: TLSocket);
    procedure OnError(const msg: string; aSocket: TLSocket);
    procedure SendTimePackets;
  public
    constructor Create(const APort: Word);
    destructor Destroy; override;
    procedure Run;
  end;

constructor TTimeServer.Create(const APort: Word);
begin
  FTCPServer := TLTCP.Create(nil);
  FTCPServer.OnAccept := @OnAccept;
  FTCPServer.OnError := @OnError;
  
  WriteLn('[TCP Time Server]');
  WriteLn('Listening on port ', APort);
  
  if not FTCPServer.Listen(APort) then
  begin
    WriteLn('ERROR: Failed to start server on port ', APort);
    Halt(1);
  end;
  
  FRunning := True;
  FPacketsSent := 0;
  FStartTime := GetTickCount64;
  FLastReportTime := FStartTime;
  
  WriteLn('Server started successfully. Waiting for clients...');
end;

destructor TTimeServer.Destroy;
begin
  FTCPServer.Free;
  inherited;
end;

procedure TTimeServer.OnAccept(aSocket: TLSocket);
var
  NoDelay: Boolean;
begin
  WriteLn(Format('[%s] Client connected from %s:%d', 
    [FormatDateTime('hh:nn:ss.zzz', Now), 
     aSocket.PeerAddress, 
     aSocket.PeerPort]));
  
  // Enable TCP_NODELAY to disable Nagle's algorithm
  NoDelay := True;
  aSocket.SetState(ssNoDelay, NoDelay);
  WriteLn('  TCP_NODELAY enabled for this connection');
end;

procedure TTimeServer.OnError(const msg: string; aSocket: TLSocket);
begin
  WriteLn(Format('[%s] ERROR: %s', [FormatDateTime('hh:nn:ss.zzz', Now), msg]));
end;

procedure TTimeServer.SendTimePackets;
var
  CurrentTime: QWord;
  Data: packed record
    Timestamp: QWord;     // 8 bytes - timestamp when sent
    PacketID: Int64;      // 8 bytes - sequential packet ID
  end;
  i: Integer;
  SendCount: Integer;
begin
  CurrentTime := GetTickCount64;
  
  // Send to all connected clients
  FTCPServer.IterReset;
  SendCount := 0;
  while FTCPServer.IterNext do
  begin
    if Assigned(FTCPServer.Iterator) and (FTCPServer.Iterator.ConnectionStatus = scConnected) then
    begin
      Data.Timestamp := CurrentTime;
      Data.PacketID := FPacketsSent;
      
      if FTCPServer.Send(Data, SizeOf(Data), FTCPServer.Iterator) > 0 then
      begin
        Inc(SendCount);
      end;
    end;
  end;
  
  if SendCount > 0 then
  begin
    Inc(FPacketsSent);
    
    // Report statistics every 5 seconds
    if CurrentTime - FLastReportTime >= 5000 then
    begin
      WriteLn(Format('[%s] Stats: Sent %d packets in %.1f seconds (%.1f pps)', 
        [FormatDateTime('hh:nn:ss.zzz', Now),
         FPacketsSent,
         (CurrentTime - FStartTime) / 1000,
         FPacketsSent / ((CurrentTime - FStartTime) / 1000)]));
      FLastReportTime := CurrentTime;
    end;
  end;
end;

procedure TTimeServer.Run;
var
  LastSendTime: QWord;
  CurrentTime: QWord;
  NextSendTime: QWord;
  SleepTime: Integer;
begin
  WriteLn('Starting to send timestamps every 20ms...');
  WriteLn('Press Ctrl+C to stop');
  WriteLn('');
  
  LastSendTime := GetTickCount64;
  NextSendTime := LastSendTime + 20;
  
  while FRunning do
  begin
    CurrentTime := GetTickCount64;
    
    // Time to send?
    if CurrentTime >= NextSendTime then
    begin
      SendTimePackets;
      FTCPServer.CallAction; // Process network events
      NextSendTime := NextSendTime + 20; // Next send in 20ms
    end
    else
    begin
      // Sleep until next send time
      SleepTime := Integer(NextSendTime - CurrentTime);
      if SleepTime > 0 then
        Sleep(SleepTime div 2); // Sleep half the time to stay responsive
      FTCPServer.CallAction; // Process network events
    end;
  end;
end;

var
  Server: TTimeServer;
  Port: Word;

begin
  if ParamCount > 0 then
    Port := StrToIntDef(ParamStr(1), 5522)
  else
    Port := 5522;
  
  Server := TTimeServer.Create(Port);
  try
    Server.Run;
  finally
    Server.Free;
  end;
end.

