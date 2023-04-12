# Detailed user Manual

## Starting the Game/Server

To play FPC Atomic in a local area network, simply start the FPC Atomic client on each machine. If desired, it is also possible to start the server in dedicated mode using the [Server control](#Server-Control-commands).

## How to start a "game"
The first player will act as the "host" of the game, while all other players will join the game.  

The "host" player should wait until all players have connected to the game, then press "return" to switch to the player setup. Depending on the teamplay settings (which can be configured in the options), each player will select their player color. Once everyone has finished selecting their color, the "host" player can press "return" to configure the map setup as needed.

After this, one more "return" will start the round. The round can end for two reasons: either all but one player have died, or in time mode, the available time has reached 0. This process repeats until the first player or team meets the required number of wins to win the match.

## In-Game Commands
Here are the commands you can use while playing FPC Atomic:
- "+" : Increases the sound volume during play. 
- "-" : Decreases the sound volume during play.
- "m" : Toggles the music on and off during play. You can also set this option in the game's options menu.
  
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
move the player in then desired direction.

### First action
- place bomb
  
### First action (double pressed)
- Grab and throw bomb (if available)
- Spooge all available bombs (if available)
  
### Second action
- Trigger all own bombs that are triggerable (and not flying)
- When pressed while walking towards a bomb "punch" a bomb in walk direction (if available)

## Connecting a Client over the Internet
To connect a client to a server over the Internet, follow these steps: 
1. Start the server with the following parameters:
- -ip \<server's IP address\>: This should be the IP address of the router that the server is connected to.
- -port \<forwarded port number\>: This should be the port number that is forwarded to the server's port.
2. If the IP address and port number are correctly specified, the game will always try to connect to the specified server when you restart the game or host a new game. Note that these settings will be cleared if you restart the game or host a new game.

## Server Control Commands
You can start the server in dedicated mode, where the first player who connects becomes the host.

To start the server in dedicated mode, use the following parameters:
- -p \<port\>: the port to listen on
- -t \<time\>: the timeout in seconds to automatically close the server if no users are connected. Use 0 to disable the timeout.
- -l \<level\>: the log level (default is 2)

### Console commands:  
You can type the following commands using your keyboard in the console window:
- "u": Unload the current AI (artificial intelligence).
- "a": Load a new AI.

## Included AI
The server will automatically load ai.so (or .dll on Windows platforms) during startup. You can utilize the [console commands](#console-commands) to unload the ai during gameplay and modify it. This feature is intended to assist in ai development

### Ai development
Start with the "ai_empty" template. The AI interface is compatible with C and other programming languages.