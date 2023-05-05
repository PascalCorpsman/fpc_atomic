import { WebSocket } from 'ws';
import { MessageDataType, TAiCommand, TAiInfo, TAiNewRound, TMessageType } from './AiInfoDef';
import { watchAiModule, unwatchAiModule, IAiModule, getAiModule } from "./watcher";


const HOST = 'ws://localhost:12345';
const aiJsFilePath = "src/ai.js";

function AiCommandToBuffer(playerIndex: number, aiCommand: TAiCommand) {
  const data = Buffer.from(
    [MessageDataType.mdtCommand,
      playerIndex,
    aiCommand.Action, 0, 0, 0,
    aiCommand.MoveState, 0, 0, 0]);
  return data;
}

function SendAiCommand(webSocket: WebSocket, playerIndex: number, aiCommand: TAiCommand) {
  webSocket.send(AiCommandToBuffer(playerIndex, aiCommand));
  console.log(`sent command for player ${playerIndex}`)
}

async function connect() {
  const websocket = new WebSocket(HOST);
  const reconnect = () => setTimeout(() => {
    connect();
  }, 1000);

  websocket.on('open', async (stream: any) => {
    console.info('Connection established.');

  });

  websocket.on('close', () => {
    console.info('Connection closed.');
    reconnect();
  });

  websocket.on('error', (error: Error) => {
    console.error(`Error: ${error.message}`);
  });

  websocket.on('message', (data: WebSocket) => {
    const aiInfo: TAiInfo | TAiNewRound = JSON.parse(data.toString());

    if (aiInfo.type === TMessageType.NewRound) {
      getAiModule().NewRound((aiInfo as TAiNewRound).Strength);
    } else if (aiInfo.type === TMessageType.Update) {
      const command = getAiModule().Update(aiInfo as TAiInfo);
      SendAiCommand(websocket, (aiInfo as TAiInfo).player, command);
    }
  });
}

(async () => {
  await watchAiModule(aiJsFilePath);
  try {
    connect();
  } finally {
    unwatchAiModule();
  }
})();