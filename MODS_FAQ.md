# Mods FAQ

[Deutsche Version](MODS_FAQ_DE.md)

Questions and answers about the [game mods](MODS_MANUAL.md).

- [Which mods give beginners a chance against strong players?](#which-mods-give-beginners-a-chance-against-strong-players)

## Which mods give beginners a chance against strong players?

In the original game a strong player wins almost every round against beginners. Some mods even this out. The most important one is the [handicap](#hit-points-handicap), because it is the only mod that treats the players differently. The other mods help beginners because they fix typical beginner mistakes or add some luck.

### Recommended

#### Hit points (handicap)

[Hit points (handicap)](MODS_MANUAL.md#hit-points-handicap) give each player a number of hits they survive, set per player name. This is the most direct way to balance the game:

- Leave `DefaultHitPoints` at 1 for the strong players and give beginners 2 or 3 in the handicap table.
- After a hit the player is invulnerable for a while (`InvulnTimeMs`, default 5 s). This is enough time to get away, keep it at 5 s or more.
- Keep **Show hit points above the players** on, so everyone sees who still has extra lives.
- Adjust the table after a few rounds: if a beginner wins too often, take one hit point away.

In the hurry every hit is lethal. With a longer play time or infinite time (launcher: **Game...**) the hit points matter for a larger part of the round, with infinite time there is no hurry at all.

#### Glove at start

A typical beginner mistake: placing a bomb in the start corner and locking themselves in. With [glove at start](MODS_MANUAL.md#glove-at-start) every player can throw this bomb away until the first step. Strong players rarely make this mistake, so the mod mostly helps beginners.

#### Ghosts (3 or more players)

Beginners usually die first and then only watch. With [ghosts](MODS_MANUAL.md#ghosts) they keep playing at the border of the field and can come back into the round:

- `GhostMaxCount` 1 or 2: a player can come back once or twice per round.
- `GhostMaxTimeMs` 0: the ghost has as much time as it needs.
- `GhostFlameLen` 3 instead of 2: the ghost bomb hits more easily.

Strong players can become ghosts too, but as they die less often, they use it less.

#### Positive disease

Beginners pick up everything, also the disease items, while strong players avoid them. With the [positive disease](MODS_MANUAL.md#positive-disease) a part of the disease items makes the player invulnerable. So collecting everything is punished less, and the luck of the protection evens out the game a little. A chance of 30 to 50 % makes the effect noticeable.

#### Walk through bricks

Works like the positive disease: [walk through bricks](MODS_MANUAL.md#walk-through-bricks) turns some disease items into a way to escape. It helps players who are trapped, which happens more often to beginners.

### Rather not, or with care

These mods reward precise timing and aiming, so strong players profit more from them:

- **[Boxing glove](MODS_MANUAL.md#boxing-glove):** stun the opponent, then place a bomb next to the opponent. Strong players use this as a combo.
- **[Stun on bomb landing](MODS_MANUAL.md#stun-on-bomb-landing):** precise throws need practice. The stun also hits the thrower, beginners often stun themselves.
- **[Open borders](MODS_MANUAL.md#open-borders) and [teleporters](MODS_MANUAL.md#teleporters):** more ways to escape and to chase. Strong players use them on purpose, beginners lose the overview.
- **[Flamethrower](MODS_MANUAL.md#flamethrower):** easy to use, but strong players dodge the beam and hit more often. If it is on, a longer **Time to kill** (e.g. 3000 ms) makes it less dangerous.

[White screen](MODS_MANUAL.md#white-screen) and [white bomb](MODS_MANUAL.md#white-bomb) are neutral: they add chaos for everyone, but a blind beginner is just as lost as a blind expert. Good for fun, not for balance.

### Outside the mods

Some options of the game (launcher: **Game...**) help as well:

- **Team play:** put a beginner and a strong player in one team. Together with the handicap this works very well.
- **Fewer wins to win the match:** the fewer wins are needed, the more a lucky round counts.
- **Longer play time or infinite time:** less pressure from the hurry, and the hit points stay useful longer (see above).

### Example

A setup for an evening with two beginners (Anna, Ben) and some strong players, in `atomic_mods.ini` of the host (or the same values in the launcher under **Mods...**):

```ini
[Mods]
HitPoints=1
DefaultHitPoints=1
InvulnTimeMs=5000
ShowHitPoints=1
GloveAtStart=1
Ghosts=1
GhostMaxCount=2
GhostMaxTimeMs=0
GhostFlameLen=3
PositiveDisease=1
PositiveDiseaseChance=40

[Handicap]
Anna=3
Ben=2
```

The player names in `[Handicap]` must be written exactly like in the game. A second player on the same PC is `Name~2`, see [hit points (handicap)](MODS_MANUAL.md#hit-points-handicap).
