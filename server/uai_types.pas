(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FPC_Atomic                                            *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uai_types;

{$MODE ObjFPC}{$H+}

Interface

{$PACKRECORDS C}

Uses
  Classes, SysUtils, ctypes;

Const

  (*
   * On library load, this is the first thing that is checked to avoid later crashes.
   * The interface version always has to fit perfectly!
   *
   * Do not change this value, otherwise your AI can no longer be loaded.
   *)
  (*
   * History: 0.01 = Initialversion
   *          0.02 = switch to ctypes.pas for all types -> now the interface is C compatible !
   *                 ADD: Lifetime info to bomb stuct
   *)
  AiLibInterfaceVersion: cint32 = 2;

  (*
   * The bit masks for the TAiPlayerInfo.Abilities value
   *)
  Ability_CanKick = 1;
  Ability_CanSpoog = 2;
  Ability_CanPunch = 4;
  Ability_CanGrab = 8;
  Ability_CanTrigger = 16;
  Ability_CanJelly = 32;

Type

  (*
   * Data structure to store positions.
   * By default, the "Item" or "Atomic" is handled in the "Middle" of a field.
   *
   * The top left-most coordinate is [0.5 / 0.5].
   * The bottom right-most coordinate is [14.5 / 10.5].
   *
   * To get field coordinates, use:
   *   x := trunc(position.x);
   *   y := trunc(position.y);
   *)
  TAiVector2 = Record
    x, y: cfloat;
  End;

  TAiPlayerAction = (
    apNone, // AI does not want to execute any action
    apFirst, // AI wants to perform a "primary" action, e.g. place a bomb...
    apFirstDouble, // AI wants to perform a "primary" double action, e.g. spooge, grab... (when doing First Double, a First action is automatically done before)
    apSecond, // AI wants to perform a "secondary" action, e.g. punch...
    apSecondDouble // Unused till now
    );

  TAiMoveState = (
    amNone, // AI wants to stand still = no walking
    amLeft, // AI wants to move to the left
    amRight, // AI wants to move to the right
    amUp, // AI wants to move up
    amDown // AI wants to move down
    );

  (*
   * AI is able to move and do "Actions"; both are calculated in the THandleAiPlayer routine.
   *)
  TAiCommand = Record
    Action: TAiPlayerAction;
    MoveState: TAiMoveState;
  End;

  TAiPlayerInfo = Record
    Team: cUint32; // [0, 1] Only valid if the game mode is Teamplay.
    Position: TAiVector2; // The position of the player in world coordinates [0.5 .. 14.5, 0.5 .. 10.5].
    Alive: cbool; // If True, the player is alive and relevant for the game. If false, the THandleAiPlayer function will not be called anymore.
    Flying: cbool; // If True, the player's position can be outside the valid range.
    FlameLength: cint; // The actual length of the flame of a placed bomb if it will explode.
    AvailableBombs: cint; // The number of bombs the AI is allowed to place at the moment. This will decrease when a bomb is placed and increase if the bomb explodes. It is 0 if the player has the "no bomb" disease.
    Speed: cfloat; // The actual speed of the player in fields per second.
    Abilities: cUint32; // A bitfield of abilities. If the bit is 1, then the ability is available.
  End;

  TAiField = (
    // Field data
    fBlank, // Nothing on the field
    fBrick, // A destructible brick
    fSolid, // A non-destructible brick
    // Flame
    fFlame, // The entire field is filled with flames
    // Powerups -> this automatically means no flame and fBlank
    fExtraBomb, // Ability to lay down one more bomb
    fLongerFlame, // Increases FlameLen by 1
    fGoldflame, // Sets FlameLen to 99
    fExtraSpeed, // Increases speed by 10%
    fKick, // The ability to kick bombs
    fSpooger, // The ability to spooger all bombs the player has
    fPunch, // The ability to punch own and other bombs
    fGrab, // The ability to grab a bomb if it was laid down (only possible if the coordinates are the same as the ones of the player)
    fTrigger, // The ability to trigger the bombs at will
    fJelly, // The ability to reflect a moving bomb if it hits a wall
    fRandom, // A random ability (good or bad, who knows...)
    fSlow, // Decreases the speed to a really bad slow
    fDisease, // A disease, could be different ones but never good
    fBadDisease // A really bad disease that you definitely do not want to get!
    );

  TAiBombInfo = Record
    Position: TAiVector2;
    FlameLength: cint; // Humans do not know this directly, extra for AI ;)
    Flying: cBool; // Is the bomb flying? Not physically on the map, but can be triggered by other bombs.
    Owner: cInt; // Player index of the owner.
    ManualTrigger: cBool; // Player must trigger this bomb by hand to explode
    Jelly: cBool; // If true, bomb will bounce back on walls.
    DudBomb: cBool; // If true, the dud bomb animation is showing (means the bomb will not explode for an unknown amount of time).
    LifeTime: cint; // Time in ms since when the bomb is layed down  ! Attention ! if this bomb is dud or ManualTriggered, this time can be "resetted" to 0 when Dud is finished, or Manual Trigger is timed out
  End;

  TAiInfo = Record
    Teamplay: Boolean; // If true, TAiPlayerInfo.Team is relevant, otherwise free for all.
    PlayerInfos: Array[0..9] Of TAiPlayerInfo;
    Field: Array[0..14, 0..10] Of TAiField;
    BombsCount: cint32; // Number of elements in Bombs.
    Bombs: Array Of TAiBombInfo;
  End;

Implementation

End.

