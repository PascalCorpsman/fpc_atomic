# FPC Atomic

Author   : Uwe Schächterle (Corpsman)  
Homepage : https://www.Corpsman.de  
Source   : https://github.com/PascalCorpsman/fpc_atomic

## Description
FPC Atomic is a complete reimplementation of the original Atomic Bomberman game (from 1995). This repository hosts only the source code, as the graphics and sounds are copyrighted by Interplay Productions. To play the game, you need to extract the original graphics from the game disc and provide them as requested by the game (there is a tool planned for future versions to extract the data automatically).

## Differences to the original
Here are the differences between FPC Atomic and the original Atomic Bomberman:
- Implementation in TCP (instead of slow IPX)
- Missing AI (and thus, single player mode)
- Not all sounds are used
- Missing online manual; use the [manual](MANUAL.md) provided in this repository instead.
- Pro gamers may discover additional differences due to our guessing the original functionality.

## License
See the license.md file located at https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md for details about the license.

## Contributors
Idea : Interplay Productions  
Implementation : Uwe Schächterle  
Graphics : Interplay Productions  
Leveldesign : Interplay Productions  
Testing : Uwe Schächterle

## Manual
See the [manual](MANUAL.md) for detailed information.

## What needs to be done to get the code compiled?
To compile the FPC Atomic code, follow these steps:
1. Download dglopengl.pas from https://github.com/saschawillems/dglopengl .
2. Download and install bass.pas from https://www.un4seen.com/ .
3. Download and install synapse from http://www.ararat.cz/synapse/doku.php/download .
4. Install the following packages into the Lazarus IDE:
    - 'Lnet' from https://github.com/almindor/L-Net or the fixed version from https://github.com/PascalCorpsman/lnet
    - 'LazOpenGLContext' (from the list of available packages inside the IDE)

## What needs to be done to get the missing content of data subfolder?
The steps shown here are only needed to be done once.

1. get the Atomic Bomberman CD-content (buy the game it is worth it!)
2. compile the "cd_data_extractor.lpi" from subfolder "cd_data_extractor"
3. start the cd_data_extractor and follow the steps
4. enjoy the game :-)