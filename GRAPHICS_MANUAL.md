# Graphics sets and graphics generator

[Deutsche Version](GRAPHICS_MANUAL_DE.md)

FPC Atomic needs graphics and sounds. Until now they only came from the Atomic Bomberman CD (`data/`, created by the CD extractor). Now FPC Atomic can also **compute its own graphics and sounds**: the figure, bombs, flames, power ups, all 11 maps, the menu screens, the schemes and all game sounds. Such a **graphics set** is created in a few seconds. With the CD everything stays as before, the original graphics can always be selected as "Original".

- [Playing without the CD](#playing-without-the-cd)
- [Choosing a graphics set](#choosing-a-graphics-set)
- [Sounds](#sounds)
- [The graphics generator](#the-graphics-generator)
- [The styles](#the-styles)
- [Where the sets are stored](#where-the-sets-are-stored)
- [Command line](#command-line)

## Playing without the CD

If the CD data is missing, the launcher shows the button **Generate graphics**. A click on **Launch** also generates a graphics set first:

1. A window shows **four styles** side by side, each with the same figure. **Roll again** shows four new variants.
2. A click on a style generates the whole set, a progress bar shows how far it is.
3. Then the launcher selects the new set and starts the game.

**Cancel** generates nothing, and the game is not started.

Game sounds and menu music come with the set (see [Sounds](#sounds)). The schemes (layouts of the bricks) come with the graphics set and are copied to `data/schemes` the first time. If there are schemes from the CD already, they stay unchanged.

## Choosing a graphics set

The current set is shown in the launcher **below the image on the left**: `◀  Name  ▶`. The arrows switch between "Original" and all generated sets, the image shows a preview of the set. The same choice is in the launcher under **Game options...** as **Graphics set**.

- If a file is missing in a set, the game takes it from the original (if available).
- **Network game:** every player chooses their own set, different sets play together. The maps are still the same, only their look differs.

## Sounds

Every set contains its own game sounds: explosions, kicks, bounces, teleporters and short callouts like "Erwischt!" or "Beeil dich!". They are not recordings, they are computed like the graphics (an interpretation of the original sounds, described in `asset_generator/sounds.ini`). There is also an own piece of music for the menus (`asset_generator/music.ini`).

The sound fits the style: flat sounds clear and punchy, soft warm with a little reverb, pixel like a 16-bit console with echo, retro rough like 8-bit. The seed changes pitch, length and timbre, so two sets of the same style sound different.

In the launcher the sounds are shown below the image on the left in a second row: `◀  Sounds: Name  ▶`. **Test sounds** plays an explosion, a callout and the start of the menu music of the selected source.

Which sounds the game plays is set there or under **Game options...** with **Sounds**:

- **Automatic** (default): the sounds from the CD if there are any, else those of the selected graphics set.
- **Original (CD)**: always the sounds from the CD.
- **a set**: the sounds of this set, even if the CD sounds are there. If a sound is missing in the set, it comes from the CD.

## The graphics generator

**Graphics...** in the launcher (or a double click on the image on the left) opens the graphics generator with the selected set. For "Original" it starts a new set.

- **Top:** name of the set, **style**, **seed** with **Roll dice** and **Own figure per player**. The seed is the number everything is computed from: same seed, same style and same settings always give exactly the same graphics.
- **Left:** the areas: figure (with the hunted and, if switched on, players 1 to 10), bombs, flames, power ups, field objects, the 11 maps, menu screens and the image for the launcher.
- **Middle:** the preview of the area. The figure walks in the selected **player color**, animation and direction can be chosen (walk, with bomb, kick, die, idle, ...). For the maps a scene shows bricks, the figure, a ticking bomb and an explosion.
- **Right:** the settings of the area as sliders (size, proportions, hue, ...), each with **Reset**. **New seed** rolls only this area again, **Lock seed** keeps it when the seed at the top is rolled.
- **Save** writes the set, **Save as set...** creates a new one.

When saving, the set is generated completely. Only when everything is done does it replace the old set. If the save is cancelled, the old set stays unchanged. If a file of the set is open in another program, a message is shown and the old set is kept as well. A running game shows the new graphics only after its next start. Do not put your own files into a set folder, they are lost on the next save.

## The styles

| Style | Look |
|---|---|
| Flat with outline | cartoon look: strong colors, thick dark outline, one hard shadow |
| Soft shading | painted like clay: smooth gradients, highlights, no black outline |
| Pixel art | 16-bit console: coarse pixels, shading in steps with dithering |
| Retro (DOS look) | very coarse pixels, few saturated colors, big heads |

In every style the figure wears the player color, and so do bombs and flames.

## Where the sets are stored

Every set is a folder under `assets/` next to the game, e.g. `assets/Generated/`. It contains the same subfolders as `data/` (`atomic/`, `maps/`, `res/`, `schemes/`), plus `profile.ini` (name, seed, style and all changed settings) and `preview.png` (the image in the launcher). To delete a set, delete its folder.

To share a set, just share its `profile.ini`: the generator creates the same set from it again.

## Command line

Graphics generator with user interface (`asset_generator_gui`):

- `--new`: new set with a random seed
- `--set <folder>`: open the set `assets/<folder>`
- `--auto --set <folder>`: only the style choice and the progress, as when starting without the CD; with `--style flat|soft|pixel|retro` without the choice

Without user interface (`asset_generator`):

```
asset_generator --out assets/MySet --name "My set" --seed random --style retro
```

- `--out <folder>`: target, an existing `profile.ini` there is read
- `--profile <file>`, `--seed <number|random>`, `--style <flat|soft|pixel|retro>`, `--name <text>`
- `--per-player`: own figure per player
- `--only <a,b,...>`: only these areas, `--list` shows all of them
- `--params`: print all settings with their values

The game itself takes the set with `-assets <folder>` and the sounds with `-sounds <folder>` (the launcher passes both automatically). The folder name `auto` is reserved for the sound selection.
