# Smart ai

A second ai for FPC Atomic, next to the original ai of Corpsman in `ai/`, which stays unchanged.
It plays smarter, stays fair for humans, and every ability can be switched off or used only with a chance in `ai_smart.ini`.

German: [README_DE.md](README_DE.md)

## What is different from the original ai

| Original ai | Smart ai |
| --- | --- |
| Places bombs on the spot when there are no bricks left in reach | Places a bomb only when it destroys bricks or hits an enemy |
| Attacks enemies only in hunter mode | Attacks in every game mode (`Attack`) |
| Own bomb flames stop at holes, conveyors, arrows and trampolines, so it walks into its own flames on those maps | Flames are simulated like in the game, always |
| Every bomb counts as "explodes now" | Knows when each bomb explodes, including chain reactions, and checks that its escape is fast enough (`TimedDanger`) |
| Triggers remote bombs the moment it stands safe, an enemy in the flames has no chance | Waits at least `MinReactionMs` before it triggers at an enemy (`Trigger`) |
| Collects powerups only by chance, burns them with its bombs | Walks to good powerups and avoids burning them (`Powerups`) |
| Never uses kick, punch, grab or spooger | Uses them (`Kick`, `Punch`, `Grab`, `Spooger`) |
| Stands still without a goal | Approaches the nearest enemy (`Approach`) |

## Choosing the ai

The smart ai is installed next to the original ai (`ai_smart.dll`, Linux: `libai_smart.so`, the launcher downloads it with the updates).

- **Launcher:** Game options, **AI** = Original or Smart, **Difficulty** 0..100 for the smart ai. The game passes the choice to the server it hosts.
- **Own server:** `atomic_server -p 5521 -ai ai_smart`. Without `-ai`, or if the file is missing, the server loads the original ai.

The server log shows `Atomic smart ai ver. ...` when the smart ai is loaded.

**For developers:** `ai_smart\try_ai.ps1 -GameDir <game folder>` (Linux: `ai_smart/try_ai.sh --gamedir <game folder>`) builds the smart ai and copies it into the game folder, next to the original. Without the parameter the repository root is used.

## Where the settings are

1. `ai_smart.ini` next to the server, if it exists (for tests with other settings).
2. Otherwise `ai_smart.ini` in the config folder of the game, next to `atomic_server.ini`:
   - Windows: `%LOCALAPPDATA%\fpc_atomic\ai_smart.ini`
   - Linux: `~/.config/fpc_atomic/ai_smart.ini`

If there is none, the ai writes it with the default values and a comment for every key.
The file is read again at the start of every round, so changes take effect with the next round without a restart.

## Difficulty

`[General] Difficulty` from 0 to 100 sets the strength with one number (default 50):

| | 0 (easy) | 50 (default) | 100 (strong) |
| --- | --- | --- | --- |
| Reaction (`DelayMs`) | 250 ms | 125 ms | 0 |
| Mistakes (`MistakeChance`) | 10 % | 5 % | 0 |
| Remote trigger at the earliest (`MinReactionMs`) | 1000 ms | 650 ms | 300 ms |
| Attack | 30 % | 65 % | 100 % |
| Spooger, punch, grab | 0 | 35 % | 70 % |
| Kick | 0 | 45 % | 90 % |
| Traps, chain reactions, anticipating | 0 | 50 % | 100 % |

Surviving (timed danger map, conveyors, hurry) and not hitting team mates stay on at every difficulty: an easier ai is slower and makes mistakes, but it does not blow itself up on purpose.

In the file the values that the difficulty sets are commented out (`;Chance=65`). Remove the `;` to fix a value yourself, it then overrides the difficulty. A file of version 0.01 (without `Difficulty`) is kept as `ai_smart.ini.bak` and written again.

## How Chance works

Every ability has a `Chance` from 0 to 100:

- `0` = never, `100` = always.
- Values in between are rolled once and the result is kept for `RerollMs` (default 3 s). Rolling every frame (10 ms) would make every value above 0 almost "always".

Invalid values are limited to their range, missing keys get the default.

## All keys

### [General]

| Key | Default (difficulty 0 / 50 / 100) | Meaning |
| --- | --- | --- |
| `Difficulty` | 50 | 0 = easy, 50 = normal, 100 = strong. Sets the values marked 0 / 50 / 100 below. |
| `Seed` | 0 | 0 = random. Any other number gives the same decisions in the same situations, for reproducible tests. |
| `LogDecisions` | 0 | 1 = every new goal of every ai player is written to `ai_smart.log` next to the ini (e.g. `P1 t=4200 attack (6,3)`). |
| `RerollMs` | 3000 | How long a rolled Chance is kept (100..60000 ms). |

### Abilities

| Section | Default (difficulty 0 / 50 / 100) | Meaning |
| --- | --- | --- |
| `[Attack]` | 30 / 65 / 100 | Places bombs to hit enemies in every game mode. In hunter mode it always attacks, like the original ai. |
| `[Powerups]` | 40 / 70 / 100 | Walks to good powerups on purpose and avoids burning them. How much it wants each one: `[PowerupWeights]`. |
| `[Approach]` | 30 / 65 / 100 | Without any other goal it walks towards the nearest enemy and keeps a distance of about 3 fields. |
| `[TimedDanger]` | 100 | Danger map with the explosion time of each bomb: it walks through fields that explode later and checks that its escape is fast enough. `0` = every bomb counts as dangerous at once, like the original ai. |
| `[Trigger]` | 100 | Remote bombs: triggered at an enemy only after `MinReactionMs`. `0` = triggered only after `MaxWaitMs`, never at an enemy. |
| `[Kick]` | 0 / 45 / 90 | Needs the kick item. Kicks a bomb towards an enemy in the same line, or kicks a bomb away when it has no other escape. |
| `[Punch]` | 0 / 35 / 70 | Needs the punch item. Punches an adjacent bomb 3 fields towards an enemy. Only when it has no remote bombs (the same key triggers them). |
| `[Grab]` | 0 / 35 / 70 | Needs the grab item. Places a bomb and throws it 3 fields onto an enemy. |
| `[Spooger]` | 0 / 35 / 70 | Needs the spooger item. Places a row of bombs towards an enemy in the same line. |
| `[Traps]` | 0 / 50 / 100 | Prefers attack fields after which the enemy has fewer free fields around it. |
| `[TeamSafety]` | 100 | Never places, kicks or triggers a bomb that hits a team mate (teamplay, and hunters among each other). Below 100 it sometimes does not look out for them. |
| `[OpenSpace]` | 50 / 75 / 100 | Of two about equal goals it takes the one with more free neighbour fields (fewer dead ends). |
| `[Infect]` | 0 / 35 / 70 | When it is ill, it walks to the nearest enemy to pass the disease on. |
| `[HuntedFlees]` | 30 / 65 / 100 | Hunter mode: the hunted keeps away from the hunters while it has at most one bomb. |
| `[Anticipate]` | 0 / 50 / 100 | Fields in the flame line of an enemy who still has bombs are less attractive as a goal. |
| `[ChainAttack]` | 0 / 50 / 100 | Also counts enemies that are hit through a chain reaction (the new bomb ignites another one). |
| `[Conveyors]` | 100 | Does not step onto a conveyor that pushes it into a dangerous field. |
| `[RoundEnd]` | 100 | Hurry (the last 80 s of a timed round): the fields that are walled up soon count as dangerous. |
| `[KickSnipe]` | 0 / 40 / 80 | Needs kick and remote bombs. Places a remote bomb, kicks it into a free line towards an enemy and triggers it when the enemy is in its flames. A kicked bomb stops in front of a player, so an enemy in the line is the best target. Several bombs line up one behind the other (they go off together). With jelly the bomb bounces between the next obstacle and the ai, which then steps out of the line. |
| `[Stun]` | 0 / 50 / 100 | Boxing glove mod, needs the punch item: stuns an enemy right in front of the ai (only without own remote bombs, the same key triggers them). |
| `[BrickWalk]` | 30 / 65 / 100 | Walk through bricks mod: walks over bricks while the time lasts, also to escape, and never stays in one when it ends. |
| `[Borders]` | 50 / 75 / 100 | Open borders mod: walks out over an open border (comes in on the other side), also to escape, and throws bombs over it (grab). Teleporters are avoided like holes. |

### Further keys of the abilities

| Key | Default (difficulty 0 / 50 / 100) | Meaning |
| --- | --- | --- |
| `[TimedDanger] SafetyMs` | 250 | Extra time in ms it keeps between itself and an explosion. Larger = more careful. |
| `[Trigger] MinReactionMs` | 1000 / 650 / 300 | A remote bomb lies at least this long before it is triggered at an enemy. Smaller = faster and less fair, about 600 is a human reaction. |
| `[Trigger] MaxWaitMs` | 2000 | A remote bomb is triggered after this time at the latest, also without an enemy (like a normal bomb). |
| `[Anticipate] Cost` | 3 | How much less attractive such a field is (0..10). |
| `[RoundEnd] HurryLookAheadMs` | 3000 | How far ahead it looks for fields that the hurry walls up. |
| `[KickSnipe] HitChance` | 30 / 60 / 90 | Chance in % to trigger a kicked bomb at the right moment, otherwise 300..800 ms too late (the enemy may be gone). |
| `[KickSnipe] WaitMs` | 8000 | How long the ai waits with its remote bombs for an enemy (the game turns them into normal bombs after 15 s). |
| `[KickSnipe] EndgameBricks` | 10 | Endgame: with at most this many bricks left the ai always snipes when it can (unless `Chance=0`). |

### [Reaction]

| Key | Default (difficulty 0 / 50 / 100) | Meaning |
| --- | --- | --- |
| `DelayMs` | 250 / 125 / 0 | It decides only every `DelayMs` and keeps walking in between. 0 = every frame. Larger = slower, more human, easier. |
| `MistakeChance` | 10 / 5 / 0 | Chance in % that a decision is replaced by a random step (never into flames or a dangerous field). |

### [PowerupWeights]

How much it wants each good powerup, 0 (ignore) to 10. Defaults: `ExtraBomb=5`, `LongerFlame=5`, `GoldFlame=6`, `Speed=3`, `Kick=4`, `Spooger=2`, `Punch=2`, `Grab=2`, `Trigger=3`, `Jelly=1`.

## Profiles

**Easy**: `Difficulty=0`. **Normal**: `Difficulty=50` (default). **Strong**: `Difficulty=100`.

**Like the original ai** (with the bug fixes: no bombs without a reason, correct flames): set every `Chance=0` (remove the `;`).

## Finding the cause of a problem

1. Play with the defaults and watch the whole picture.
2. If something looks wrong, set `LogDecisions=1`. The log shows which goal each ai player chose.
3. Set single abilities to `Chance=0`, one at a time, to find the one that causes it. The change works from the next round on.
4. With `Seed` set to a number, the same situation leads to the same decisions.

## Files

| File | Content |
| --- | --- |
| `ai_smart.lpr` | The lib with the same exports as the original ai |
| `uatomicai_smart.pas` | The ai |
| `usmartsettings.pas` | Reading and writing `ai_smart.ini`, the log |
| `try_ai.ps1`, `try_ai.sh` | Build and copy into a game folder (for developers) |
