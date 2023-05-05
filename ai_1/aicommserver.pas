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
  TAiPlayer = record
    //Type of Command to send via websocket. Either String (Json) or TAiCommand
    Command: TAiCommand;
    // Set to true if Info fiel contains data to send
    InfoSet: boolean;
    // The Playfield info record for AI to be evaluated.
    // AiInfo contains all information about the Atomic Bomberman playfield (bombs and their positions, players, teams, etc.)
    Info: TAiInfo;
    // Round trip time in miliseconds
    SentTime: cardinal;
  end;

  { TAiCommServer }
  TAiCommServer = class;

  // A Websocker Handler for the AI Communication Server. Instanciated, used and freed by TAiCommServer.
  TAiCommServerHandler = class(TThreadedWebsocketHandler)
  private
    // String to send by websocket
    FDataString, 
    // Last string received by websocket
    FResultDataString: string;
    // Synchronizer for FDataString and FResultDataString
    FSyncDataString, 
    // Syncronizer for FAiPlayers
    FSync: TMultiReadExclusiveWriteSynchronizer;
    // Used to disallow multiple connections
    FClientConnected: boolean;
    // List of messages available to be read by websocket
    FMessagesList: TWebsocketMessageOwnerList;
    // Global String builder for building JSON strings faster
    FStringBuilder: TStringBuilder;
    // If true the Receive message loop will be closed (used in DoHandleCommunication)
    FClosing: boolean;

    // array of player data to send and receive.
    // Used to get ai information from TAiCommServer.AiInfo and store answers for TAiCommServer.AiCommand.
    FAiPlayers: array[0..MAX_PLAYERS - 1] of TAiPlayer;

    // Writes for each player in FAiPlayer TAiPlayer.Info to outgoing websocket if its information has been set (indicted byTAiPlayer.InfoSet).
    function SendAiInfo(ACommunication: TWebsocketCommunicator): boolean;
    // Sends any STRING (JSON) to the websocket.
    function SendString(ACommunication: TWebsocketCommunicator;
      var Value: string): boolean;

    procedure ConnectionClosed(Sender: TObject);
    procedure MessageReceived(Sender: TObject);

    // gets string and TAiCommand answers and processes them to FAiPlayers.
    function ProcessResponse(ACommunication: TWebsocketCommunicator): boolean;

    // Gets the last received string of the websocket and stores it threadsafe for TAiCommServer.GetLastResponse
    procedure SetResponseMessageStringThreadSafe(const ResponseMessageString: string);

    // Gets the last TAiCommand from FAiPlayers and resets its contents to no actions.
    function GetAiCommand(Player: integer): TAiCommand;

    procedure PutAiInfo(Player: integer; const AiWorldInfo: TAiInfo);

    // Gets the round trip time in milliseconds of between last sent and received message of the given player.
    function GetResponseTime(Player: integer): cardinal;
  public

    function Accept(const ARequest: TRequestData;
      const ResponseHeaders: TStrings): boolean; override;
    procedure DoHandleCommunication(Communication: TWebsocketCommunicator);
      override;
  public
    constructor Create(Parent: TAiCommServer);
    destructor Destroy; override;

    //Sends a string to the websocket (usually JSON)
    procedure SendString(const Data: string);
    // gets the last received string from the websocket (usually JSON)
    function GetLastResponse(): string;
  end;

  // Main WebSocket Server. Uses a Thread to communicate.
  TAiCommServer = class(TThread)
  private
    FServer: TWebSocketServer;
    FHandler: TAiCommServerHandler;
    FIsRunning: boolean;
  protected
    procedure Execute; override;

    function GetAiCommand(Player: integer): TAiCommand;
    procedure PutAiInfo(Player: integer; const AiWorldInfo: TAiInfo);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartServer;
    procedure StopServer;

    // Sends any String (JSON)
    procedure SendString(const Data: string);
    // Receives any Available string or empty string if none.
    function GetLastResponse(): string;

    function GetResponseTime(Player: integer): cardinal;

    // Gets the last received AI Command of a player.
    // The command of this player will be reset to no actions after reading its value.
    property AiCommand[Player: integer]: TAiCommand read GetAiCommand; default;
    // Sets and sends play field information of the given player
    // This information will be reset after sending the data. Data will be
    // sent asynchronously and may take some time.
    property AiInfo[Player: integer]: TAiInfo write PutAiInfo;
  end;

function DumpExceptionCallStack(E: Exception): string;


function HasAiCommands(const aiCommand : TAiCommand) : Boolean; inline;
function HasAiAction(const aiCommand : TAiCommand) : Boolean; inline;
function HasAiMovement(const aiCommand : TAiCommand) : Boolean; inline;
function NoAiCommands() : TAiCommand;

implementation



type
  // Message received
  TMessageData = record
    // Player target
    Player: cardinal;
    // Type of data. Either a TAiCommand or String (JSON)
    case DataType: TMessageDataType of
      mdtCommand: (aiCommand: TAiCommand);
      mdtString: (Value: PString)
  end;

{ TAiCommServer }



constructor TAiCommServer.Create;
begin
  inherited Create(True);

  FServer := TWebSocketServer.Create(12345);
  FServer.FreeHandlers := False;
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


function TAiCommServer.GetAiCommand(Player: integer): TAiCommand;
begin
  Result := FHandler.getAiCommand(Player);
end;

procedure TAiCommServer.PutAiInfo(Player: integer; const AiWorldInfo: TAiInfo);
begin
  FHandler.PutAiInfo(Player, AiWorldInfo);
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
  FClientConnected := False;
  FResultDataString := '';
  FDataString := '';

  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FSyncDataString := TMultiReadExclusiveWriteSynchronizer.Create;
  FMessagesList := TWebsocketMessageOwnerList.Create(True);
  FStringBuilder := TStringBuilder.Create(1024 * 8);

  for i := 0 to High(FAiPlayers) do
  begin
    FillByte(FAiPlayers[i], sizeof(TAiPlayer), 0);
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


function TAiCommServerHandler.GetAiCommand(Player: integer): TAiCommand;
begin
  FSync.Beginwrite;
  try
    Result := FAiPlayers[Player].Command;
    FAiPlayers[Player].Command := NoAiCommands();
  finally
    FSync.Endwrite;
  end;
end;

procedure TAiCommServerHandler.PutAiInfo(Player: integer; const AiWorldInfo: TAiInfo);
var
  i: integer;
  Bombs: array of TAiBombInfo = ();
begin
  // Have to copy all bomb items separately because
  // otherwise we get an invalid pointer exception
  // if we just copy Item in one
  SetLength(Bombs, Length(AiWorldInfo.Bombs));

  for i := low(AiWorldInfo.Bombs) to high(AiWorldInfo.Bombs) do
  begin
    Bombs[i] := AiWorldInfo.Bombs[i];
  end;

  FSync.Beginwrite;
  try
    FAiPlayers[Player].InfoSet := True;

    FAiPlayers[Player].Info.BombsCount := AiWorldInfo.BombsCount;
    FAiPlayers[Player].Info.Teamplay := AiWorldInfo.Teamplay;
    FAiPlayers[Player].Info.Field := AiWorldInfo.Field;
    FAiPlayers[Player].Info.PlayerInfos := AiWorldInfo.PlayerInfos;

    // Mange these by hand to avoid exception of invalid pointer
    FAiPlayers[Player].Info.Bombs := Bombs;

    // raises an exception in: Invalid Pointer
    //FAiPlayers[Player].Info := AiWorldInfo;
    FAiPlayers[Player].SentTime := GetTickCount;
  finally
    FSync.Endwrite;
  end;
end;

function TAiCommServerHandler.GetResponseTime(Player: integer): cardinal;
begin
  FSync.BeginRead;
  try
    Result := GetTickCount - FAiPlayers[Player].SentTime;
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
    for i := 0 to High(FAiPlayers) do
    begin
      if not ACommunication.Open then
        break;

      if FAiPlayers[i].InfoSet then
      begin
        AiInfoToJson(i, FAiPlayers[i].Info, FStringBuilder);
        try
          ACommunication.WriteStringMessage(FStringBuilder.toString());
        finally
          FAiPlayers[i].InfoSet := False;
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
            FAiPlayers[ResponseMessageData.Player].Command :=
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

    // Make extra sure that we do not spin forever here!
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
    Communication.Close(True);
  finally
    FClientConnected := False;
  end;
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

function NoAiCommands() : TAiCommand; inline;
begin
  FillByte(result, sizeof(result), 0);
end;

function HasAiCommands(const aiCommand : TAiCommand) : Boolean; inline;
begin
  result := (aiCommand.Action <> apNone) or (aiCommand.MoveState <> TAiMoveState.amNone);
end;

function HasAiAction(const aiCommand : TAiCommand) : Boolean; inline;
begin
  result := (aiCommand.Action <> apNone);
end;


function HasAiMovement(const aiCommand : TAiCommand) : Boolean; inline;
begin
  result := (aiCommand.MoveState <> TAiMoveState.amNone);
end;

end.
