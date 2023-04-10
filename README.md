# FPC Atomic

Author   : Uwe Schächterle (Corpsman)  
Homepage : www.Corpsman.de

## Description
This is a complete reimplementation of the orig Atomic Bomberman, here on Github is only the Sourceode hosted. To be able to play the game you need to extract the original graphics from the game disc and provide them as requested by the game.

## License
See the file license.md, located under:
 https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md
 for details about the license.     

## contributors
Idea : Interplay Productions  
Implementation : Uwe Schächterle  
Graphics : Interplay Productions  
Leveldesign : Interplay Productions  
Testing : Uwe Schächterle


## In game commands
"+" = Increase sound Volume during play  
"-" = Decrease sound volume during play  
"m" = Disable enable music during play


## Connect the Client over the internet
start the client with params:  
 -ip \<ip from router of server\>  
 -port \<port that is forwarded to the server port\>

if ip and port are defined, the game will always try to join on this as long as you restart the game or host, then the settings will be cleared.


## Server Control commands
Params:  
-p \<port\> = Port to listen on  
-t \<time\> = Timeout in seconds to automatically close if no user is connected (0= disabled)  
-l \<level\>= Loglevel (default = 2)  
Commands (typed in via keyboard in the console window):  
u = unload actual ai  
a = load new ai
