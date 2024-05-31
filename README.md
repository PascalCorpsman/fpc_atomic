# FPC Atomic

Author   : Uwe Schächterle (Corpsman)  
Homepage : https://www.Corpsman.de  
Source   : https://github.com/PascalCorpsman/fpc_atomic  
Wiki     : https://en.wikipedia.org/wiki/Atomic_Bomberman

## Description
FPC Atomic is a complete reimplementation of the original Atomic Bomberman game (from 1997). This repository hosts only the source code, as the graphics and sounds are copyrighted by Interplay Productions. To play the game, you need to extract the original graphics from the game disc and provide them as requested by the game.

## What needs to be done to play the game?
The steps shown here are only needed to be done once (for installation).

```plantuml
Alice -> Bob: Authentication Request
Bob --> Alice: Authentication Response
   
Alice -> Bob: Another authentication Request
Alice <-- Bob: Another authentication Response

[[https://www.oocities.org/timessquare/tower/4056/download/ani.zip ani.zip]]
```

![](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/PascalCorpsman/fpc_atomic/main/documentation/installation.plantuml)

1. get the Atomic Bomberman CD-content (buy the game it is worth it!) and store it on your harddrive  
   1.1 if you want all animations download and merge the expansion pack from [here](https://www.oocities.org/timessquare/tower/4056/ani.html) or use this [direct download link](https://www.oocities.org/timessquare/tower/4056/download/ani.zip)  
   1.2 extract the content of ani.zip into the CD-Content "/DATA/ANI" subfolder
2. download and uncompress the .zip file of the subfolder [bin](https://github.com/PascalCorpsman/fpc_atomic/tree/main/bin) on your harddisc
3. run the "cd_data_extractor" and follow the instructions
4. read the [manual](MANUAL.md)  
4.5 if you are a Linux user run the "Linux_sound_install_script.sh"
1. start the game by executing "fpc_atomic" binary and enjoy

## How do i get updates ?
If there are updates of the engine available, the game will inform you through a popup on startup. Click yes and your version will be updated automatically.

## Differences to the original

- Implementation in TCP (instead of slow IPX)
- Custom AI implementation
- Single player mode
- Not all sounds are used
- Missing online manual; use the [manual](MANUAL.md) provided in this repository instead.
- Pro gamers may discover additional differences due to guessing of the original functionality.

## Manual
See the [manual](MANUAL.md) for detailed information.

## License
See the license.md file located at https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md for details about the license.

## What needs to be done to compile the code ?

1. Install Lazarus IDE from https://www.lazarus-ide.org/
2. Download dglopengl.pas from https://github.com/saschawillems/dglopengl and store it in the "units" folder
3. Download bass.pas from https://www.un4seen.com/ and store it in the "units" folder
4. Download synapse from http://www.ararat.cz/synapse/doku.php/download and store it in the "units" folder
5. Install the following packages into the Lazarus IDE:
    - 'Lnet' from https://github.com/almindor/L-Net or the fixed version from https://github.com/PascalCorpsman/lnet
    - 'LazOpenGLContext' (from the list of available packages inside the IDE)

## What needs to be done to implement a own AI ?

The AI that is delivered with this repository is more a try than a real AI, and to be correct in words it's a agent not a real AI (it does not learn nor uses neural nets). If you want to write your own version you have two options:

- Implement a new AI with Free Pascal using this [template](https://github.com/PascalCorpsman/fpc_atomic/tree/main/ai_empty)
- Implement a new AI using the C [template](https://github.com/PascalCorpsman/fpc_atomic/tree/main/ai_c) (or use the C-interface to port it to your most favourite programing language)

Read the manual section [Console commands](MANUAL.md#console-commands) to load and unload your ai without the need to restart the application.

## Contributors
Idea : Interplay Productions  
Implementation : Uwe Schächterle  
Graphics : Interplay Productions  
Leveldesign : Interplay Productions  
Testing : Uwe Schächterle
