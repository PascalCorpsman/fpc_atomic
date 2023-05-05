import { AiAction, AiMoveState, TAiCommand, TAiInfo } from "./AiInfoDef";

// Exportiere die Funktion NewRound
export function NewRound(Strength: number): void {
  // Implementiere die Funktion NewRound
  console.log(`New Round. Strength ${Strength}}`);


}

// Exportiere die Funktion Update
export function Update(AiInfo: TAiInfo): TAiCommand {
  // Implementiere die Funktion Update

 // console.log(`new info for player ${AiInfo.player}`)

  return {Action: AiAction.apFirst, MoveState: AiMoveState.amUp} as TAiCommand;
}

