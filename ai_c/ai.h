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
#ifndef __AI_H__
#define __AI_H__

#include <stdint.h>
#include <stdbool.h>
#include "ai_types.h"


// Exported functions

bool AiInit();

void AiDeInit();

uint32_t AiInterfaceVersion();

char *AiVersion();

void AiNewRound(uint8_t Strength);

TAiCommand_t AiHandlePlayer(uint32_t PlayerIndex, TAiInfo_t* AiInfo);

#endif
