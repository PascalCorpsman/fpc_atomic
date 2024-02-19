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
#include "ai.h"

bool AiInit()
{
    return true;
    /*
     * Put your implementation here...
     */
}

void AiDeInit()
{
    /*
     * Put your implementation here...
     */
}

uint32_t AiInterfaceVersion()
{
    /*
     * Do not change !!
     */
    return AI_LIB_INTERFACE_VERSION;
}

char *AiVersion()
{
    /*
     * Put your implementation here...
     */
    return "Demo C ai ver. 0.01";
}

void AiNewRound(uint8_t Strength)
{
    /*
     * Put your implementation here...
     */
}

TAiCommand_t AiHandlePlayer(uint32_t PlayerIndex, TAiInfo_t* AiInfo)
{
    TAiCommand_t res;
    res.Action = apNone;
    res.MoveState = amNone;
    /*
     * Put your implementation here...
     */
    return res;
}