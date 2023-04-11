# FPC Atomic

Author   : Uwe Schächterle (Corpsman)  
Homepage : https://www.Corpsman.de  
Source   : https://github.com/PascalCorpsman/fpc_atomic

## Description
This is a complete reimplementation of the original Atomic Bomberman (from 1995).
In this repository only the sourceode is hosted (as the graphics and sounds are copyright of Interplay Productions). To be able to play the game you need to extract the original graphics from the game disc and provide them as requested by the game (for future there is a tool planed to automatically extract the data from the game disc / files).

## Differences to the original
- Implementation in TCP (instead of slow IPX)
- missing AI (-> missing Single player)
- not all sounds are used
- missing online manual, use this [manual](manual.md) instead
- Due to guessing the original functionallity a pro gamer could figure out much more differences ...

## License
See the file license.md, located under:
 https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md
 for details about the license.     

## Contributors
Idea : Interplay Productions  
Implementation : Uwe Schächterle  
Graphics : Interplay Productions  
Leveldesign : Interplay Productions  
Testing : Uwe Schächterle

## Manual
See the [manual](manual.md) for detailed informations.

## What needs to be done to get the code compiled:
- download dglopengl.pas from https://github.com/saschawillems/dglopengl
- download and install bass.pas from https://www.un4seen.com/
- download and install synapse from http://www.ararat.cz/synapse/doku.php/download

The following packages need to be installed into the Lazarus IDE:
- Lnet from https://github.com/almindor/L-Net or the fixed version from https://github.com/PascalCorpsman/lnet
- LazOpenGLContext (from the list of available packages inside the IDE)
