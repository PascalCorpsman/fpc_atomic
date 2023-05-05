import fs from "fs";
import { debounce } from "debounce";
import path from "path";
import { TAiInfo, TAiCommand } from "./AiInfoDef";

export interface IAiModule {
  NewRound: (Strength: number) => void;
  Update: (AiInfo: TAiInfo) => TAiCommand;
}

let watcher: fs.FSWatcher | undefined;

let aiModule: IAiModule;


export function getAiModule(): IAiModule {
  return aiModule;
}

// This code watches a file for changes and reloads the module when it changes.
export async function watchAiModule(filePath: string) : Promise<void> {


  const reloadModule = async (): Promise<void> => {
    try {
      const file = path.resolve(filePath);

      delete require.cache[require.resolve(file)];
      const { NewRound: newNewRound, Update: newUpdate } = require(file);

      aiModule = {
        NewRound: newNewRound,
        Update: newUpdate
      };

      console.log('Ai.ts module reloaded.')
    } catch (error) {
      console.error(`Failed to reload AI module: ${error}`);
    }
  };

  reloadModule();

  const reloadModuleDebounced = debounce(reloadModule, 500, false) 

  const watcher = fs.watch(path.dirname(filePath), async (event, filename) => {
    
    if (filename === path.basename(filePath)) {
      //console.log(`Reloading AI module (${event}): ${filename})`);
      reloadModuleDebounced();
    }
  });
  
}



export function unwatchAiModule(): void {
  if (watcher) {
    watcher.close();
    watcher = undefined;
    console.log("Stopped watching AI module.");
  }
}


