unit AiCommServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, websocketserver, wsmessages, wsstream;

type
  { TAiCommServer }
  TAiCommServer = class;

  TAiCommServerHandler = class(TThreadedWebsocketHandler)
  private
    fDataString, FResultDataString : String;
    FSyncDataString, FSync: TMultiReadExclusiveWriteSynchronizer;
    FClientConnected : Boolean;
    procedure ConnectionClosed(Sender: TObject);
    procedure MessageReceived(Sender: TObject);
  private
    FParent: TAiCommServer;
    function Accept(const ARequest: TRequestData;
      const ResponseHeaders: TStrings): boolean; override;
    procedure DoHandleCommunication(ACommunication: TWebsocketCommunicator);
      override;
  public
    constructor Create(Parent : TAiCommServer);
    destructor Destroy; override;

    procedure SendString(const Data : String);
    function GetLastResponse() : String;
  end;

  TAiCommServer = class(TThread)
  private
    FServer: TWebSocketServer;
    FHandler : TAiCommServerHandler;

  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartServer;
    procedure StopServer;

    procedure SendString(const Data : String);
    function GetLastResponse() : String;
  end;

implementation


{ TAiCommServer }



constructor TAiCommServer.Create;
begin
  inherited Create(True);

  FServer := TWebSocketServer.Create(12345);
  FServer.FreeHandlers := false;
  FHandler := TAiCommServerHandler.Create(self);
  FServer.RegisterHandler('*', '*', FHandler, True, True);
end;

destructor TAiCommServer.Destroy;
begin
  FServer.Free;
  FHandler.Free;
  inherited Destroy;
end;

procedure TAiCommServer.Execute;
begin
  FServer.Start;
end;

procedure TAiCommServer.StartServer;
begin
  Self.Start;
end;


procedure TAiCommServer.StopServer;
begin
  FServer.Stop(True);
end;

procedure TAiCommServer.SendString(const Data : String);
begin
  FHandler.SendString(Data);
end;
function TAiCommServer.GetLastResponse() : String;
begin
  result := FHandler.GetLastResponse();
end;

constructor TAiCommServerHandler.Create(Parent : TAiCommServer);
begin
  inherited Create;
  FParent := Parent;
  FClientConnected := false;
  FResultDataString := '';
  fDataString := '';

  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FSyncDataString := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TAiCommServerHandler.Destroy;
begin
   FSync.Free;
   FSyncDataString.Free;
   inherited;
end;

function TAiCommServerHandler.Accept(const ARequest: TRequestData;
        const ResponseHeaders: TStrings): boolean;
begin
  // Customize handshake here if needed
  Accept := not FClientConnected;
end;

procedure TAiCommServerHandler.SendString(const Data : String);
begin
  FSyncDataString.Beginwrite;
  try
    fDataString := Copy(Data, 1, Length(Data));
  finally
    FSyncDataString.Endwrite;
  end;
end;

function TAiCommServerHandler.GetLastResponse() : String;
begin
  FSync.BeginRead;
  try
    result := Copy(FResultDataString, 1, Length(FResultDataString));
    FResultDataString := '';
  finally
    FSync.EndRead;
  end;
end;

procedure TAiCommServerHandler.DoHandleCommunication(
  ACommunication: TWebsocketCommunicator);
var
  str, ThreadedDataString: string;
  Ticks : Longint;
  StringMessage : TWebsocketStringMessage;
begin
  FClientConnected := true;
  try
  //  WriteLn('Connected to ', ACommunication.SocketStream.RemoteAddress.Address);
    ACommunication.OnReceiveMessage := @MessageReceived;
    ACommunication.OnClose := @ConnectionClosed;
    while ACommunication.Open do
    begin
      FSyncDataString.BeginRead;
      try
        if Length(fDataString) <> 0 then
        begin
          ThreadedDataString := Copy(fDataString, 1, Length(fDataString));
          fDataString := '';
        end else
        begin
          Sleep(5);
          Continue;
        end;
      finally
        FSyncDataString.EndRead;
      end;

      Ticks := GetTickCount;
      ACommunication.WriteStringMessage(ThreadedDataString);

      if not ACommunication.Open then
        break;
      try
        StringMessage := ACommunication.WaitForStringMessage;
        if StringMessage = nil then
          break;

        Str := StringMessage.Data;
      except
        on E: EWebsocketReadError do
          break;
      end;

      if not ACommunication.Open then
        break;

      Ticks := GetTickCount - Ticks;
      Writeln('MSec: ', Ticks);

      FSync.BeginWrite;
      try
        FResultDataString:= Str;
      finally
        FSync.EndWrite;
      end;
    end;
  finally
    FClientConnected := false;
  end;
  //FParent.FServer.Stop(True);
end;

procedure TAiCommServerHandler.ConnectionClosed(Sender: TObject);
// WebSocket Communcation Connection Closed
var
 Comm: TWebsocketCommunicator;
begin
  Comm := TWebsocketCommunicator(Sender);
//  WriteLn('Connection to ', Comm.SocketStream.RemoteAddress.Address, ' closed');
end;

procedure TAiCommServerHandler.MessageReceived(Sender: TObject);
// Any WebSocket Communcation Message
var
 Messages: TWebsocketMessageOwnerList;
 m: TWebsocketMessage;
 Comm: TWebsocketCommunicator;
begin
 Comm := TWebsocketCommunicator(Sender);
 Messages := TWebsocketMessageOwnerList.Create(True);
 try
   Comm.GetUnprocessedMessages(Messages);
   for m in Messages do
     if m is TWebsocketStringMessage then
     begin
       //ignore
//       TWebsocketStringMessage(m).Data
     end;
 finally
   Messages.Free;
 end;
end;


end.

