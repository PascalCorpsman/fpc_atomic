#(******************************************************************************)
#(*                                                                            *)
#(* Author      : Uwe Schächterle (Corpsman)                                   *)
#(*                                                                            *)
#(* This file is part of <program name>                                        *)
#(*                                                                            *)
#(*  See the file license.md, located under:                                   *)
#(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
#(*  for details about the license.                                            *)
#(*                                                                            *)
#(*               It is not allowed to change or remove this text from any     *)
#(*               source file of the project.                                  *)
#(*                                                                            *)
#(******************************************************************************)

#
# This is a small and lightwight Make file to utilize gcc
# to compile the c example for libai.so / ai.dll
#

CC = gcc
CFLAGS = -Wall 
LIBRARY_NAME = ai

ifeq ($(OS),Windows_NT)
    # Windows-specific commands
    TARGET = ai.dll
else
    # Linux-specific commands
    TARGET = lib$(LIBRARY_NAME).so
endif

all: $(LIBRARY_NAME)

$(LIBRARY_NAME): ai.c
	$(CC) $(CFLAGS) -shared -o ../$(TARGET) $^

clean:
	rm -f ../$(TARGET)
