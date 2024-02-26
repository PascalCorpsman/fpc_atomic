/******************************************************************************/
/*                                                                            */
/* Author      : Uwe Schächterle (Corpsman)                                   */
/*                                                                            */
/* This file is part of <program name>                                        */
/*                                                                            */
/*  See the file license.md, located under:                                   */
/*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  */
/*  for details about the license.                                            */
/*                                                                            */
/*               It is not allowed to change or remove this text from any     */
/*               source file of the project.                                  */
/*                                                                            */
/******************************************************************************/
#ifndef __AI_TYPES_H__
#define __AI_TYPES_H__

#include <stdint.h>
#include <stdbool.h>

#define AI_LIB_INTERFACE_VERSION 4u

#define MAX_PLAYERS 10
#define FIELD_WIDTH 15
#define FIELD_HEIGHT 11

#define Ability_CanKick 1u
#define Ability_CanSpoog 2u
#define Ability_CanPunch 4u
#define Ability_CanGrab 8u
#define Ability_CanTrigger 16u
#define Ability_CanJelly 32u

/*
 * Data structure to store positions.
 * By default, the "Item" or "Atomic" is handled in the "Middle" of a field.
 *
 * The top left-most coordinate is [0.5 / 0.5].
 * The bottom right-most coordinate is [14.5 / 10.5].
 *
 * To get field coordinates, use:
 *   x := trunc(position.x);
 *   y := trunc(position.y);
 */
typedef struct
{
    float x;
    float y;
} TAiVector2_t;

typedef enum
{
    apNone,        // AI does not want to execute any action
    apFirst,       // AI wants to perform a "primary" action, e.g., place a bomb...
    apFirstDouble, // AI wants to perform a "primary" double action, e.g., spooge, grab...
                   // (when doing First Double, a First action is automatically done before)
    apSecond,      // AI wants to perform a "secondary" action, e.g., punch...
    apSecondDouble // Unused till now
} TAiPlayerAction_t;

typedef enum
{
    amNone,  // AI wants to stand still = no walking
    amLeft,  // AI wants to move to the left
    amRight, // AI wants to move to the right
    amUp,    // AI wants to move up
    amDown   // AI wants to move down
} TAiMoveState_t;

typedef struct
{
    TAiPlayerAction_t Action;
    TAiMoveState_t MoveState;
} TAiCommand_t;

typedef struct
{
    uint32_t Team;         // [0, 1] Only valid if the game mode is Teamplay.
    TAiVector2_t Position; // The position of the player in world coordinates [0.5 .. 14.5, 0.5 .. 10.5].
    bool Alive;            // If True, the player is alive and relevant for the game. If false, the THandleAiPlayer function will not be called anymore.
    bool Flying;           // If True, the player's position can be outside the valid range.
    int FlameLength;       // The actual length of the flame of a placed bomb if it will explode.
    int AvailableBombs;    // The number of bombs the AI is allowed to place at the moment. This will decrease when a bomb is placed and increase if the bomb explodes. It is 0 if the player has the "no bomb" disease.
    float Speed;           // The actual speed of the player in fields per second.
    uint32_t Abilities;    // A bitfield of abilities. If the bit is 1, then the ability is available.
    IsIll: Boolean;        // If true the Player has a Disease (Unknown which)
} TAiPlayerInfo_t;

typedef enum
{
    // Field data
    fBlank, // Nothing on the field
    fBrick, // A destructible brick
    fSolid, // A non-destructible brick
    fHole, // A beamimg hole they beam counter clock wise
    fTramp, // A tramp that kicks the player in the air
    fConveyorUp, // If the player stands on this field it is being pushed in the "up" direction
    fConveyorDown, // If the player stands on this field it is being pushed in the "up" direction
    fConveyorLeft, // If the player stands on this field it is being pushed in the "up" direction
    fConveyorRight, // If the player stands on this field it is being pushed in the "up" direction
    fArrowUp, // If a bomb is beeing kicked against this field its direction will change to "up"
    fArrowDown, // If a bomb is beeing kicked against this field its direction will change to "down"
    fArrowLeft, // If a bomb is beeing kicked against this field its direction will change to "left"
    fArrowRight, // If a bomb is beeing kicked against this field its direction will change to "right"
    // Flame
    fFlame, // The entire field is filled with flames
    // Powerups -> this automatically means no flame and fBlank
    fExtraBomb,   // Ability to lay down one more bomb
    fLongerFlame, // Increases FlameLen by 1
    fGoldFlame,   // Sets FlameLen to 99
    fExtraSpeed,  // Increases speed by 10%
    fKick,        // The ability to kick bombs
    fSpooger,     // The ability to spooger all bombs the player has
    fPunch,       // The ability to punch own and other bombs
    fGrab,        // The ability to grab a bomb if it was laid down (only possible if the coordinates are the same as the ones of the player)
    fTrigger,     // The ability to trigger the bombs at will
    fJelly,       // The ability to reflect a moving bomb if it hits a wall
    fRandom,      // A random ability (good or bad, who knows...)
    fSlow,        // Decreases the speed to a really bad slow
    fDisease,     // A disease, could be different ones but never good
    fBadDisease   // A really bad disease that you definitely do not want to get!
} TAiField_t;

typedef struct {
    TAiVector2_t Position;
    int FlameLength;    // Humans do not know this directly, extra for AI ;)
    bool Flying;        // Is the bomb flying? Not physically on the map, but can be triggered by other bombs.
    int Owner;          // Player index of the owner.
    bool ManualTrigger; // Player must trigger this bomb by hand to explode.
    bool Jelly;         // If true, bomb will bounce back on walls.
    bool DudBomb;       // If true, the dud bomb animation is showing (means the bomb will not explode for an unknown amount of time).
    int LifeTime;       // Time in ms since when the bomb is laid down! Attention! if this bomb is dud or ManualTriggered, this time can be "resetted" to 0 when Dud is finished, or Manual Trigger is timed out.
} TAiBombInfo_t;

typedef struct {
    bool Teamplay;                          // If true, TAiPlayerInfo.Team is relevant, otherwise free for all.
    TAiPlayerInfo_t PlayerInfos[MAX_PLAYERS]; // Array of TAiPlayerInfo for each player.
    TAiField_t Field[FIELD_WIDTH][FIELD_HEIGHT]; // 2D array of TAiField representing the game field.
    int BombsCount;                         // Number of elements in Bombs.
    TAiBombInfo_t *Bombs;                     // Dynamic array of TAiBombInfo.
} TAiInfo_t;
#endif
