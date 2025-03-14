# Detailed user Manual

If you have a markdown viewer installed on your machine (e.g. VSCode) it is highly recommended to read the manual local on your machine, after you run the cd_data_extractor. Doing so enables a lot of graphics inside the manual which increases readabillity by a lot.

## Starting the Game/Server

To play FPC Atomic in a local area network, simply start the FPC Atomic client on each machine (run fpc_atomic executable). If desired, it is also possible to start the server in dedicated mode using the [Server control](#Server-Control-commands), if done so all clients need to "join" as the server is already startet. The first client who connects to a dedicated server will be treated as if he has startet the game.

## How to start a "game"
The first player will act as the "host" of the game, while all other players will join the game.

The "host" player should wait until all players have connected to the game, then press "return" to switch to the player setup. Depending on the teamplay settings (which can be configured in the options), each player will select their player color. Once everyone has finished selecting their color, the "host" player can press "return" to configure the map setup as needed.

After this, one more "return" will start the round. The round can end for two reasons: either all but one player have died, or in time mode, the available time has reached 0. This process repeats until the first player or team meets the required number of wins to win the match.

## In-Game Commands
Here are the commands you can use while playing FPC Atomic:
- "ESC"/ Back : Return to mainmenu (or quit game, when in mainmenu)
- "+" : Increases the sound volume during play. 
- "-" : Decreases the sound volume during play.
- "m" : Toggles the music on and off during play. You can also set this option in the game's options menu.
- "j" : Open IP / Port question dialog to connect to internet server.
  
## Powerups and game control
Here are the powerups available in the game:  
![](data/res/powbomb.png) Gives an extra bomb  
![](data/res/powtrig.png) Ability to trigger bombs. If picked up, only a limited amount of bombs are available see [second action](#Second-action)  
![](data/res/powspoog.png) Ability to spooge all available bombs. Disables the ability to grab bombs see [first action](#first-action-double-pressed)  
![](data/res/powflame.png) Increases flame length by one  
![](data/res/powgold.png) Increases flame length to infinity  
![](data/res/powskate.png) Increases walk speed by 10% (maximum 160% of default speed)  
![](data/res/powkick.png) Ability to kick bombs. The player needs to walk against a bomb to kick it  
![](data/res/powjelly.png) All kicked bombs will now bounce on walls  
![](data/res/powgrab.png) Adds ability to grab bombs. Disables ability to spooge bombs see [first action](#first-action-double-pressed)  
![](data/res/powpunch.png) Ability to punch bombs see [second action](#Second-action)  
![](data/res/powrand.png) Random powerup. Could be good or bad  
![](data/res/powslow.png)  Decreases walk speed to 60% of normal speed   
![](data/res/powdisea.png) Get a random disease  
![](data/res/powebola.png) Get a random super bad disease  

## In Game key commands
The key binding can be changed in the options dialog

### Direction keys
- move the player in then desired direction.
- move the player against a bomb, will kick the bomb (if powerup was collected)

### First action
- place bomb
  
### First action (double pressed)
- Grab and throw bomb (if powerup was collected)
- Spooge all available bombs (if powerup was collected)
  
### Second action
- Trigger all own bombs that are triggerable (and not flying)
- When pressed while walking towards a bomb "punch" a bomb in walk direction (if powerup was collected)

## Connecting a Client over the Internet to a local server
To connect a client to a server over the Internet, follow these steps: 

#### Preperation on server side
1. Configure a port forwarding on the router between the internet and the computer where the server should run
2. Start the server on the target computer, this could be done by hosting a game on the target computer or by starting the server in dedicated mode see [Server Control Commands](#Server-Control-Commands) 

#### Preperation on client side

* Press "j" when in MainMenu and enter the IP / Port settings from the server to join 
 
or

* Start the client with the following command line parameters:<br>
 -ip \<server's IP address\>: This should be the IP address of the router that the server is connected to.<br>
 -port \<forwarded port number\>: This should be the port number that is forwarded to the server's port.
  
 If the IP address and port number are correct, the client will automatically connect to the specified server. When you restart the client or host a new game the given parameters will be resetted and the game will work as "normal".


## Server Control Commands
Normally it is not necessary to take care of the server. The server is automatically started by fpc_atomic when a game is hosted. If desired the server can also be used in dedicated mode. The first client that connects to a dedicated server is treated as host.

To start the server in dedicated mode, use the following command line parameters:
- -h: online help for server (see to get more informations)
- -p \<port\>: the port to listen on (this parameter is mandatory)
- -t \<time\>: the timeout in seconds to automatically close the server if no users are connected. Use 0 to disable the timeout.
- -l \<level\>: the log level (default is 2)
- -ats \<speed\>: sets atomic normal speed (default is 5)

### Console commands:
During server execution it is possible to type the following commands in the console window:
- "u": Unload the current AI (artificial intelligence) library.
- "a": Load a new AI.
- "ESC": instance close server

### Launcher commands:

Since ver 0.08 the launcher supports command line commands:
- -ip \<IP\> : will prefill the "Router IP" field with the value given as \<IP\>
- -port \<port\>:  will prefill the "Router Port" field with the value given as \<port\>
- -cti: will check the checkbox "connect through internet"

## Included AI
The server will automatically load ai.so (or ai.dll on Windows platforms) during startup. You can utilize the [console commands](#console-commands) to unload the ai during gameplay and modify it. This feature is intended to assist in ai development.

### Ai development
Read section [What needs to be done to implement a own AI](README.md#what-needs-to-be-done-to-implement-a-own-ai) from the README.md
