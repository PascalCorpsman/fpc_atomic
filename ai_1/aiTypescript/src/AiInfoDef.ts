export enum TMessageType {
    NewRound = "newround",
    Update = "info"
}

export enum MessageDataType {
    mdtString = 1,
    mdtCommand = 2
}

export interface TAiNewRound {
    type: TMessageType;
    Strength: number;
}


export interface TAiVector2 {
    x: number;
    y: number;
}



export enum TAiAbilities {
    Ability_CanKick = 1,
    Ability_CanSpoog = 2,
    Ability_CanPunch = 4,
    Ability_CanGrab = 8,
    Ability_CanTrigger = 16,
    Ability_CanJelly = 32,
}

export enum TAiField {
    fBlank = 0,
    fBrick = 1,
    fSolid = 2,
    fFlame = 3,
    fExtraBomb = 4,
    fLongerFlame = 5,
    fGoldflame = 6,
    fExtraSpeed = 7,
    fKick = 8,
    fSpooger = 9,
    fPunch = 10,
    fGrab = 11,
    fTrigger = 12,
    fJelly = 13,
    fRandom = 14,
    fSlow = 15,
    fDisease = 16,
    fBadDisease = 17
}

export interface TAiPlayerInfo {
    Team: number;
    Position: TAiVector2;
    Alive: boolean;
    Flying: boolean;
    FlameLength: number;
    AvailableBombs: number;
    Speed: number;
    Abilities: number;
}

export interface TAiBombInfo {
    Position: TAiVector2;
    FlameLength: number;
    Flying: boolean;
    Owner: number;
    ManualTrigger: boolean;
    Jelly: boolean;
    DudBomb: boolean;
}

export interface TAiInfo {
    type: TMessageType;
    player: number;
    Teamplay: boolean;
    PlayerInfos: [TAiPlayerInfo, TAiPlayerInfo, TAiPlayerInfo, TAiPlayerInfo, TAiPlayerInfo, TAiPlayerInfo, TAiPlayerInfo, TAiPlayerInfo, TAiPlayerInfo, TAiPlayerInfo];
    Field: [[TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField],
        [TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField, TAiField]
    ];
    BombsCount: number;
    Bombs: Array<TAiBombInfo>;
}


export enum AiAction {
    apNone, // AI does not want to execute any action
    apFirst, // AI wants to perform a "primary" action, e.g. place a bomb...
    apFirstDouble, // AI wants to perform a "primary" double action, e.g. spooge, grab... (when doing First Double, a First action is automatically done before)
    apSecond, // AI wants to perform a "secondary" action, e.g. punch...
    apSecondDouble // Unused till now       
}

export enum AiMoveState {
    amNone, // AI wants to stand still = no walking
    amLeft, // AI wants to move to the left
    amRight, // AI wants to move to the right
    amUp, // AI wants to move up
    amDown // AI wants to move down    
}

export interface TAiCommand {
    Action: AiAction;
    MoveState: AiMoveState;
}
