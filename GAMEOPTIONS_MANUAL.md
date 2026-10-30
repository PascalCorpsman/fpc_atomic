# Game options and game modes

[Deutsche Version](GAMEOPTIONS_MANUAL_DE.md)

This manual explains the game options (launcher: **Game options...**, in the game: the options menu) and the game modes (launcher: **Game mode...**). The optional rule changes are described in the [mods manual](MODS_MANUAL.md).

- [Part 1: Game options](#part-1-game-options)
- [Part 2: Game modes](#part-2-game-modes)
  - [Classic](#classic)
  - [Hunter](#hunter)
- [FAQ](#faq)

## Part 1: Game options

The game options are stored in `fpc_atomic.cfg` in the config folder (launcher: **Config folder**). The launcher dialog **Game options...** and the options menu of the game change the same values. Changes in the launcher apply to the next start of the game.

Most options are a setting of the **host**: when you host a game, the server plays with your team play, scheme, map, play time, wins and conveyor speed. The other players do not need to change anything.

| Option | Default | Meaning |
|---|---|---|
| Team play | off | The players play in two teams, red and white. Each player chooses a team in the player setup. A team wins the round when only players of this team are left alive, and every player of the winning team gets the win, also if he died. |
| Random start positions | on | The players start at random start positions of the scheme. Off: player 1 always starts at start position 1, player 2 at start position 2 and so on. |
| Lost net players revert to AI | off | If a network player loses the connection during a match, an AI takes over his slot. Off: the player leaves the round. While the players are still connecting, a lost player always just leaves. |
| Scheme | BASIC.SCH | The scheme sets the bricks, the start positions and which items lie below the bricks (files in `data/schemes`). |
| Map | Random each game | The graphics and the sound of the field (files in `data/maps`). The preview in the launcher shows the map with all bricks of the scheme. In the game some bricks are missing at random, depending on the brick density of the scheme. |
| Play time | 2:30 | Time of a round: Infinity or 1:30 to 10:00 in 30 s steps. 85 s before the end "Hurry" is shown, 80 s before the end the hurry starts (see below). When the time is over and no one has won, the round is a draw. Infinity = no time limit and no hurry. |
| Wins to win the match | 3 | How many rounds a player (or a team) has to win to win the match, 1 to 99. |
| Conveyor speed | Middle | Speed of the conveyor belts on maps that have them: slow, middle or fast. |
| Network port | 5521 | TCP port of the server when you host a game. Players who join over the internet need this port (see [MANUAL.md](MANUAL.md#connecting-a-client-over-the-internet-to-a-local-server)). |
| Volume | 100 % | Sound volume. In the game it can also be changed with `+` and `-` in 5 % steps. |
| Play sound effects | on | Explosions, items and so on. The music is switched separately with "Play music" in the launcher or with `m` in the game. |

Map and wins can also be changed in the game by the host before a round starts.

### The hurry

80 s before the end of the round the hurry starts: every 0.5 s a solid block falls onto the field, from the border in a spiral towards the middle. A player below a falling block dies, a bomb below it explodes first. With the [mods](MODS_MANUAL.md#common-rules), the hurry also ends every invulnerability, all ghosts vanish and every hit is lethal. In rounds with infinite time there is no hurry.

## Part 2: Game modes

The game mode is a setting of the host, like the mods. It is stored in `atomic_mods.ini` (sections `[GameMode]` and `[Hunter]`) and is sent to the server together with the mods when the host joins. The other players do not need to change anything.

In the launcher click **Game mode...**: the list on the left shows the modes, the selected one is the active mode. Exactly one mode is always active. The right side shows the settings of the mode. **OK** saves, **Defaults** resets the game mode to Classic and the Hunter settings to their defaults (the mods stay as they are). The main window shows the active mode next to the button.

### Classic

The normal game: the last player alive wins the round, or the round is a draw when the time is over. [Team play](#part-1-game-options) is switched on in the game options and is only played in this mode.

### Hunter

One player is the **hunted**. He gets (almost) all items, all other players hunt him. Whoever kills the hunted becomes the new hunted and gets the items, the old hunted falls back to normal.

**How a round works**

1. At the start of the round one player becomes the hunted (random or the leader, see the settings).
2. The hunted gets the start values of the scheme plus the **items of the hunted**. He blinks and is invulnerable for a short time.
3. If a hunter kills the hunted (with a bomb or a flamethrower), the hunter becomes the new hunted.
4. If the hunted dies **without a killer** (his own bomb, the hurry, a ghost bomb), the setting "Hunted dies without a killer" decides who is next.
5. With **respawn** dead players come back after a waiting time at a random free tile far away from the hunted, and are invulnerable for a short time. The round ends when the play time is over.
6. The round winner is decided by the setting "Round winner". The winner gets one win for the match. A tie is a draw.

**Settings**

| Setting | ini key (`[Hunter]`) | Default | Meaning |
|---|---|---|---|
| Game mode | `Mode` (`[GameMode]`) | `Classic` | `Classic` or `Hunter` |
| Items of the hunted | `HuntedItems` | `bomb*4,flame*4,speed*2,kick,punch,grab,jelly` | Items the hunted gets on top of the scheme, the same format as the start items of the [mod test mode](MODS_MANUAL.md#mod-test-mode): comma separated, `*n` = n times. Possible: bomb, flame, goldflame, speed, kick, punch, grab, spooger, jelly, trigger. Grab and spooger exclude each other. |
| Hunted at round start | `HuntedStart` | 0 | 0 = a random player, 1 = the leader (most won rounds in the match, random on a tie) |
| Hunted dies without a killer | `HuntedLost` | 0 | 0 = a random other living player becomes the hunted, 1 = he stays the hunted (only with respawn, without respawn like 0) |
| Round winner | `HunterScoring` | 0 | 0 = longest time as hunted, 1 = most kills of the hunted, 2 = the hunted at the end of the round |
| The old hunted gets | `HuntedLoseItems` | 0 | 0 = his items from before he became the hunted, 1 = no items (only the start values of the scheme) |
| Invulnerable as new hunted (ms) | `HuntedInvulnMs` | 1500 | 0 .. 60000, 0 = not at all |
| Respawn | `HunterRespawn` | 1 (on) | Dead players come back, the round only ends when the time is over, there is no hurry |
| Respawn after (ms) | `HunterRespawnMs` | 3000 | 0 .. 60000 |
| Invulnerable after respawn (ms) | `HunterRespawnInvulnMs` | 2000 | 0 .. 60000 |
| The hunted gets no diseases | `HuntedNoDiseases` | 1 (on) | Diseases, slow and swap have no effect on the hunted, he is not infected by others |
| Gold color | `HuntedGold` | 1 (on) | The hunted is drawn in gold |
| Aura | `HuntedAura` | 1 (on) | A pulsing aura around the hunted |
| The hunted sees his aura | `HuntedAuraSelf` | 0 (off) | Off: only the hunters see the aura. Two players on one PC always see it, because one of them could be a hunter. |

**Round winner (scoring)**

- **Longest time as hunted:** the time counts while the hunted is alive. Good for long rounds with respawn: everybody wants to get the items and keep them.
- **Most kills of the hunted:** counts how often a player killed the hunted. This rewards hunting, not hiding.
- **Hunted at the end of the round:** only the last moment counts. Exciting at the end, the time before is just the warm-up.

**Respawn and hurry**

With respawn nobody leaves the round, so the round can only end by time. That is why there is **no hurry** with respawn: it would only crush players who come back anyway. With the play time "Infinity" the round lasts 120 s, otherwise it would never end.

Without respawn the normal rules apply: the round ends when only one player is left alive (or the time is over), the hurry comes as usual and the [ghosts mod](MODS_MANUAL.md#ghosts) works.

**A new hunted, the old hunted**

- A new hunted blinks and is invulnerable for "Invulnerable as new hunted", so he is not killed by the bomb that made him the hunted.
- Bombs he has already placed are taken into account: with the new items he can place more bombs at once, the bombs on the field are counted against them.
- The old hunted (if he is still alive) gets his items from before or only the scheme values, see "The old hunted gets".
- The items of the hunted are not spread on the field when he dies, only items a player really picked up are spread as usual.

**Display**

- The hunted is drawn in **gold** and has a pulsing **aura** (both can be switched off). When the hunted blinks (invulnerable or stunned), the blinking is shown instead of the gold.
- In the hunter mode the header shows `S:` (score) and `H:` with the seconds a player has been the hunted in this round, instead of the kills (`K:`).

**Team play and mods**

- **Team play:** the hunter mode is ignored in team play, a normal team game is played. The game mode dialog shows a hint when team play is on.
- **Ghosts:** only without respawn. With respawn dead players come back anyway, the dialog shows a hint when both are on.
- **Hit points:** apply as usual. The hunted only changes when the hunted really dies, not when he loses a hit point.
- All other mods work as usual. The [common rules](MODS_MANUAL.md#common-rules) of the mods (invulnerability, hurry) apply.

**AI**

- An AI player can be a hunter and the hunted. The AI that comes with the game hunts the hunted: it walks to him and places bombs in his line. As the hunted it attacks everybody.
- Not every AI knows the game modes. If the loaded AI does not support the hunter mode (an older or a foreign `ai.dll` / `libai.so`), or if no AI is loaded, AI slots cannot be chosen in the player setup, the game shows a message instead. Existing AI slots are reported when switching to the map setup.
- For the same reason "Lost net players revert to AI" only works if the AI supports the mode. Otherwise the lost player leaves the round.

## FAQ

**Why can't I choose an AI?**
The loaded AI does not support the active game mode, or no AI is loaded at all. Switch to Classic, or use the AI that comes with this version of the game (`ai.dll` / `libai.so` next to the server).

**Why is there no hurry?**
In the hunter mode with respawn there is no hurry: dead players come back anyway, the round ends when the time is over. Switch off respawn to get the hurry back. In Classic there is no hurry with the play time "Infinity".

**Which items should the hunted get?**
The default (`bomb*4,flame*4,speed*2,kick,punch,grab,jelly`) makes the hunted strong but not untouchable. With `goldflame` or `trigger` he becomes much more dangerous, with fewer items the hunt gets shorter. For small fields fewer bombs are better. Try `bomb*2,flame*2,speed` for a fair hunt with beginners.

**The hunted dies by his own bomb, who is next?**
That is "Hunted dies without a killer": a random other player, or with respawn he can stay the hunted.

**I switched on team play, but the hunter mode is not played.**
That is intended: team play and the hunter mode do not fit together, in team play a normal team game is played.
