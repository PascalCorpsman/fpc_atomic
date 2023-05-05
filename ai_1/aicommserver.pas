unit AiCommServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, websocketserver, wsmessages, wsstream, aiInfoJson, uai_Types;

type
   {$Z4}
  TMessageDataType = (mdtNone = 0, mdtString = 1, mdtCommand = 2);

const
  MAX_PLAYERS = 10;

type
  TMessageData = record
    Player: cardinal;
    case DataType: TMessageDataType of
      mdtCommand: (aiCommand: TAiCommand);
      mdtString: (Value: PString)
  end;

  TAiPlayer = record
    Command: TAiCommand;
    InfoSet: boolean;
    Info: TAiInfo;
    SentTime: cardinal;
  end;

  { TAiCommServer }
  TAiCommServer = class;

  TAiCommServerHandler = class(TThreadedWebsocketHandler)
  private
    FDataString, FResultDataString: string;
    FSyncDataString, FSync: TMultiReadExclusiveWriteSynchronizer;
    FClientConnected: boolean;
    FMessagesList: TWebsocketMessageOwnerList;
    FParent: TAiCommServer;
    FStringBuilder: TStringBuilder;
    FClosing: boolean;

    FPlayersAiCommands: array[0..MAX_PLAYERS - 1] of TAiPlayer;
    function SendAiInfo(ACommunication: TWebsocketCommunicator): boolean;
    function SendString(ACommunication: TWebsocketCommunicator;
      var Value: string): boolean;



    procedure ConnectionClosed(Sender: TObject);
    procedure MessageReceived(Sender: TObject);

    function ProcessResponse(ACommunication: TWebsocketCommunicator): boolean;
    procedure SetResponseMessageStringThreadSafe(const ResponseMessageString: string);

    function GetAiCommand(Index: integer): TAiCommand;
    procedure putAiInfo(Index: integer; const Item: TAiInfo);
    function GetResponseTime(Player: integer): cardinal;
  public

    function Accept(const ARequest: TRequestData;
      const ResponseHeaders: TStrings): boolean; override;
    procedure DoHandleCommunication(Communication: TWebsocketCommunicator);
      override;
  public
    constructor Create(Parent: TAiCommServer);
    destructor Destroy; override;

    procedure SendString(const Data: string);
    function GetLastResponse(): string;
  end;

  TAiCommServer = class(TThread)
  private
    FServer: TWebSocketServer;
    FHandler: TAiCommServerHandler;
    FIsRunning: boolean;

  protected
    procedure Execute; override;

    function GetAiCommand(Index: integer): TAiCommand;
    procedure PutAiInfo(Index: integer; const Item: TAiInfo);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartServer;
    procedure StopServer;

    procedure SendString(const Data: string);
    function GetLastResponse(): string;

    function GetResponseTime(Player: integer): cardinal;

    property AiCommand[Index: integer]: TAiCommand read GetAiCommand; default;
    property AiInfo[Index: integer]: TAiInfo write PutAiInfo;

  end;

function DumpExceptionCallStack(E: Exception): string;

implementation



{ TAiCommServer }



constructor TAiCommServer.Create;
begin
  inherited Create(True);

  FServer := TWebSocketServer.Create(12345);
  FServer.FreeHandlers := false;
  FHandler := TAiCommServerHandler.Create(self);
  FServer.RegisterHandler('*', '*', FHandler, True, True);
  FIsRunning := False;
end;

destructor TAiCommServer.Destroy;
begin
  try
    writeln('Stopping Server');

    if FIsRunning then
    begin
      StopServer;
      self.WaitFor();
      self.Terminate;
    end;

    writeln('Stopped');


    FServer.Free;

    inherited Destroy;
  except
    on E: Exception do
    begin
      Writeln('TAiCommServer.Destroy;');
      Writeln(E.ToString);
    end;
  end;
end;

procedure TAiCommServer.Execute;
begin
  try
    FIsRunning := True;
    try
      FServer.Start;
    except
      on E: Exception do
      begin
        Writeln('TAiCommServer.Execute; ');
        Writeln(E.ToString);
        Writeln(DumpExceptionCallStack(E));
      end;
    end;
  finally
    Writeln('Thread Ended');
    FIsRunning := False;
  end;
end;

procedure TAiCommServer.StartServer;
begin
  Self.Start;
end;


procedure TAiCommServer.StopServer;
begin
  FServer.Stop(True);
  // Wait a bit for stop to take effect
  Sleep(100);
end;

procedure TAiCommServer.SendString(const Data: string);
begin
  FHandler.SendString(Data);
end;

function TAiCommServer.GetLastResponse(): string;
begin
  Result := FHandler.GetLastResponse();
end;


function TAiCommServer.GetAiCommand(Index: integer): TAiCommand;
begin
  Result := FHandler.getAiCommand(Index);
end;

procedure TAiCommServer.PutAiInfo(Index: integer; const Item: TAiInfo);
begin
  FHandler.putAiInfo(Index, Item);
end;

function TAiCommServer.GetResponseTime(Player: integer): cardinal;
begin
  Result := FHandler.GetResponseTime(Player);
end;

{******************* TAiCommServerHandler *******************}

constructor TAiCommServerHandler.Create(Parent: TAiCommServer);
var
  i: integer;
begin
  inherited Create;
  FParent := Parent;
  FClientConnected := False;
  FResultDataString := '';
  FDataString := '';

  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FSyncDataString := TMultiReadExclusiveWriteSynchronizer.Create;
  FMessagesList := TWebsocketMessageOwnerList.Create(True);
  FStringBuilder := TStringBuilder.Create(1024 * 8);

  for i := 0 to High(FPlayersAiCommands) do
  begin
    FillByte(FPlayersAiCommands[i], sizeof(TAiPlayer), 0);
  end;
end;

destructor TAiCommServerHandler.Destroy;
begin
  FSync.Free;
  FSyncDataString.Free;
  FMessagesList.Free;
  FStringBuilder.Free;

  inherited;
end;


function TAiCommServerHandler.GetAiCommand(Index: integer): TAiCommand;
begin
  FSync.Beginwrite;
  try
    Result := FPlayersAiCommands[Index].Command;
    FPlayersAiCommands[Index].Command := NoAiCommands();
  finally
    FSync.Endwrite;
  end;
end;

procedure TAiCommServerHandler.PutAiInfo(Index: integer; const Item: TAiInfo);
var
  i : Integer;
  Bombs : array of TAiBombInfo = ();
begin
  // Have to copy all bomb items separately because
  // otherwise we get an invalid pointer exception
  // if we just copy Item in one
  SetLength(Bombs, Length(Item.Bombs));

  for i := low(Item.Bombs) to high(Item.Bombs) do
  begin
    Bombs[i] := Item.Bombs[i];
  end;

  FSync.Beginwrite;
  try
    FPlayersAiCommands[Index].InfoSet := True;

    FPlayersAiCommands[Index].Info.BombsCount := Item.BombsCount;
    FPlayersAiCommands[Index].Info.Teamplay := Item.Teamplay;
    FPlayersAiCommands[Index].Info.Field := Item.Field;
    FPlayersAiCommands[Index].Info.PlayerInfos := Item.PlayerInfos;

    // Mange these by hand to avoid exception of invalid pointer
    FPlayersAiCommands[Index].Info.Bombs := Bombs;

    // raises an exception in: Invalid Pointer
    //FPlayersAiCommands[Index].Info := Item;
    FPlayersAiCommands[Index].SentTime := GetTickCount;
  finally
    FSync.Endwrite;
  end;
end;

function TAiCommServerHandler.GetResponseTime(Player: integer): cardinal;
begin
  FSync.BeginRead;
  try
    Result := GetTickCount - FPlayersAiCommands[Player].SentTime;
  finally
    FSync.Endread;
  end;
end;

function TAiCommServerHandler.Accept(const ARequest: TRequestData;
  const ResponseHeaders: TStrings): boolean;
begin
  // Customize handshake here if needed
  Accept := not FClientConnected;
  if Accept then
    Writeln('Accept Request')
  else
    Writeln('Alredy Connected.');
end;

procedure TAiCommServerHandler.SendString(const Data: string);
begin
  if Data = '' then
    exit;

  FSyncDataString.Beginwrite;
  try
    FDataString := Copy(Data, 1, Length(Data));
  finally
    FSyncDataString.Endwrite;
  end;
end;

function TAiCommServerHandler.GetLastResponse(): string;
begin
  FSyncDataString.Beginwrite;
  try
    Result := Copy(FResultDataString, 1, Length(FResultDataString));
    FResultDataString := '';
  finally
    FSyncDataString.Endwrite;
  end;
end;




function TAiCommServerHandler.SendAiInfo(ACommunication:
  TWebsocketCommunicator): boolean;
var
  i: integer;
begin
  Result := False;

  FSync.BeginWrite;
  try
    for i := 0 to High(FPlayersAiCommands) do
    begin
      if not ACommunication.Open then
        break;

      if FPlayersAiCommands[i].InfoSet then
      begin
        AiInfoToJson(i, FPlayersAiCommands[i].Info, FStringBuilder);
        try
          ACommunication.WriteStringMessage(FStringBuilder.toString());
        finally
          FPlayersAiCommands[i].InfoSet := False;
          Result := True;
        end;
      end;
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TAiCommServerHandler.SendString(ACommunication: TWebsocketCommunicator;
  var Value: string): boolean;
begin
  Result := False;

  FSyncDataString.BeginWrite;
  try
    if Length(Value) = 0 then
    begin
      exit;
    end;

    ACommunication.WriteStringMessage(Value);
    Value := '';
    Result := True;
  finally
    FSyncDataString.EndWrite;
  end;
end;


function TAiCommServerHandler.ProcessResponse(ACommunication:
  TWebsocketCommunicator): boolean;
var
  DataRaw: ^TBytes; // array of byte
  i: integer;
  ResponseString: string;
  ResponseMessageData: TMessageData;
begin
  Result := True;

  try
    FMessagesList.Clear;
    ACommunication.GetUnprocessedMessages(FMessagesList);

    for i := FMessagesList.Count - 1 downto 0 do
    begin
      if FMessagesList.Items[i] is TWebsocketStringMessage then
      begin
        ResponseString := TWebsocketStringMessage(FMessagesList.Items[i]).Data;

        SetResponseMessageStringThreadSafe(ResponseString);
        exit;
      end;

      ResponseString := '';
      if FMessagesList.Items[i] is TWebsocketBinaryMessage then
      begin
        // Get bytearray as pointer to increment
        DataRaw := @TWebsocketBinaryMessage(FMessagesList.Items[i]).Data;
        ResponseMessageData.DataType := TMessageDataType(DataRaw^[0]);

        // get to the real data after this
        if (ResponseMessageData.DataType = TMessageDataType.mdtCommand) then
        begin
          ResponseMessageData.Player := byte(DataRaw^[1]);
          Move(DataRaw^[2], ResponseMessageData.aiCommand,
            sizeof(ResponseMessageData.aiCommand));

          FSync.BeginWrite;
          try
            FPlayersAiCommands[ResponseMessageData.Player].Command :=
              ResponseMessageData.aiCommand;
          finally
            FSync.EndWrite;
          end;
        end;
      end;
    end;
  except
    on E: EWebsocketReadError do
      Result := False;
    on E: Exception do
    begin
      Writeln('TAiCommServerHandler.ProcessResponse');
      writeln(E.ToString);
      Writeln(DumpExceptionCallStack(E));

    end;
  end;
end;

procedure TAiCommServerHandler.SetResponseMessageStringThreadSafe(
  const ResponseMessageString: string);
begin
  FSyncDataString.BeginWrite;
  try
    FResultDataString := ResponseMessageString;
  finally
    FSyncDataString.EndWrite;
  end;
end;

procedure TAiCommServerHandler.DoHandleCommunication(
  Communication: TWebsocketCommunicator);
begin
  FClientConnected := True;
  FClosing := False;
  try
    WriteLn('Connected to ', Communication.SocketStream.RemoteAddress.Address);
    Communication.OnReceiveMessage := @MessageReceived;
    Communication.OnClose := @ConnectionClosed;
    while FClientConnected and not FClosing and Communication.Open do
    begin
      try
        if not (SendString(Communication, FDataString) or SendAiInfo(Communication)) then
        begin
          Sleep(1);
          Continue;
        end;

      except
        on E: EWebsocketWriteError do
        begin
          Writeln('Could not send data. Websocket Write error');
          break;
        end;
      end;
    end;
    Writeln('Handle Communication ended');
    Communication.OnClose := nil;
    Communication.OnReceiveMessage := nil;
    Communication.Close(true);
  finally
    FClientConnected := False;
  end;
  //FParent.FServer.Stop(True);
end;

procedure TAiCommServerHandler.ConnectionClosed(Sender: TObject);
// WebSocket Communcation Connection Closed
var
  Comm: TWebsocketCommunicator;
begin
  FClosing := True;
  Comm := TWebsocketCommunicator(Sender);
  WriteLn('Connection to ', Comm.SocketStream.RemoteAddress.Address, ' closed');
end;

procedure TAiCommServerHandler.MessageReceived(Sender: TObject);
var
  ACommunication: TWebsocketCommunicator;
  ResponseString: string;
begin
  ACommunication := TWebsocketCommunicator(Sender);

  if not ProcessResponse(ACommunication) then
    exit;

  if not ACommunication.Open then
    exit;
end;


function DumpExceptionCallStack(E: Exception): string;
var
  I: integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding + 'Stacktrace:' +
    LineEnding + LineEnding;
  if E <> nil then
  begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
      'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);

  Result := Report;
end;

end.
