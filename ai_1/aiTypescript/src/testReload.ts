/// <reference path="AiInfoDef.ts" />

import readline from "readline";
import { watchAiModule, unwatchAiModule, IAiModule, getAiModule } from "./watcher";

const filePath = "src/ai.js";

(async () => {
  await watchAiModule(filePath);


  // Verwenden Sie die exportierten Funktionen von aiModule
  // ...

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false,
  });

  setInterval(async () => {
    getAiModule().NewRound(1);
  }, 5000);

  // Warten Sie auf das Drücken der ESC-Taste und beenden Sie das Programm
  rl.on("keypress", (key) => {
    if (key === "\u001B") {
      console.log("ESC wurde gedrückt. Beenden...");
      unwatchAiModule();
      process.exit();
    }
  });


})();