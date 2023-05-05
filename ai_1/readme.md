# AiInfo Server for FPC Atomic Bomberman

## Folders

* ai_1 - This folder contains the AI written in FreePascal.
* ai_1/TS - This folder contains the endpoint for the AI written in Typescript.

## Introduction

This is a server for the game Atomic Bomberman. It is written in FreePascal and Typescript. The server is used to host a game of Atomic Bomberman and to provide an endpoint for the AI to connect to.
In this way it is possible to create an AI in typescript. Major features are:

1. Write an AI in Typescript
2. Connect to FPC Atomic Bomberman Proxy Server and receive play field information and send AI commands
3. The AI script in ai.ts can be changed on-the-fly without restarting the FPC Atomic Bomberman server.

## How to use

1. First you need to compile an ai.dll file that is loaded by the FPC AB server.
2. Then you have to install the aiTypescript client for managing ai behavior. To do so, navigate to aiTypescript folder an
   1. type `npm install`
   2. type `npm run watch`. This will start watching src/ai.ts, and transpiling to JavaScript for NodeJs that you need to adapt for your AI.
   3. At last you have to run `src/app.ts` in your IDE. This will try and at last connect to the server created by the 3rd step. A Debugger might be helpful.for debugging ai.ts.
   
3. Run the server by typing ` .\atomic_server.exe -p 1000 -l 0 -t 0`.