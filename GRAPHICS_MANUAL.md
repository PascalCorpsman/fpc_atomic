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

On the very first start without CD data and without a graphics set the launcher asks once: **Welcome to FPC Atomic — where should the graphics and sounds of the game come from?**

- **Generate own (recommended)**: a window shows **four styles** side by side, each with the same figure (**Roll again** shows four new variants). A click on a style generates that set in about 15 seconds, named after the style. The folders below `assets/` are always English (`Flat`, `Soft`, `Pixel`, `Retro`), the launcher, the game options and the graphics generator show the style name in the current language of the launcher (in German Flach, Weich, Pixel, Retro) and switch it with the language. The launcher selects it (sounds "Automatic") and you can play at once. The other three styles are generated **in the background** with the same seed (same figure and maps, other look); the status in the launcher shows how many are still running, they appear below the image when they are ready. If you close the launcher before, it waits invisibly until they are done. **Cancel** in the style window generates nothing and shows the question again.
- **Take them from the CD**: opens the CD extractor (the Atomic Bomberman CD is needed).
- **Later**: nothing happens. The image in the launcher then shows "No graphics yet", a click on it opens the question again.

The question comes by itself only once. **Setup...** in the launcher (group Installation, left of **CD extractor...**; bold as long as there are no game data) or `atomic_launcher --setup` open it any time, e.g. to generate the four styles again with a new seed. Sets that a setup created before are **replaced** (Flat, Soft, Pixel, Retro get the new look). Sets you saved in the graphics generator, or sets that came from elsewhere, are never overwritten: the new style is then called e.g. "Flat 2" (folder `Flat2`). Everything else stays possible: the CD extractor is always there, after extracting the CD "Original" is just one more choice, and the generated sets stay.

If the CD data is missing, a click on **Launch** generates a graphics set first (one style of your choice, the launcher then starts the game).

**You already have the CD data?** Then the question does not come by itself. Instead the launcher shows once a small notice above the image: **New: own graphics...**. A click on it opens the same style choice as the setup, generates the four styles and keeps "Original" selected; a short message then says that you can switch with ◀ ▶ below the image. The **×** next to it hides the notice for good. It is not shown if there are graphics sets already or the setup has run before.

If the game (`fpc_atomic`) is started directly, without the launcher, it uses the graphics set and the sounds chosen in the launcher (the same settings file). If there are neither CD data nor a usable set, it says "No graphics are set up yet" and offers **Open setup** (starts the launcher with the question) or **Quit**.

Game sounds and menu music come with the set (see [Sounds](#sounds)). The schemes (layouts of the bricks) come with the graphics set and are copied to `data/schemes` the first time. If there are schemes from the CD already, they stay unchanged.

## Choosing a graphics set

The current set is shown in the launcher **below the image on the left**: `◀  Name  ▶`. The arrows switch between "Original" and all generated sets, the image shows a preview of the set. The same choice is in the launcher under **Game options...** as **Graphics set**.

- If a file is missing in a set, the game takes it from the original (if available).
- **Network game:** every player chooses their own set, different sets play together. The maps are still the same, only their look differs.

## Sounds

Every set contains its own game sounds: explosions, kicks, bounces, teleporters and short callouts like "Erwischt!" or "Beeil dich!". They are not recordings, they are computed like the graphics (an interpretation of the original sounds, described in `asset_generator/sounds.ini`). There is also an own piece of music for the menus (`asset_generator/music.ini`).

The callouts exist in English, German, French and Spanish (own short phrases in every language, e.g. "Je t'ai eu !", "¡Te pillé!"), and every set contains all of them. The game and the launcher's **Test sounds** automatically play the callouts in the language of the launcher; switching the language in the launcher is enough. A set of an older version without French or Spanish callouts plays the English ones. The language in the area **Sounds** of the graphics generator only chooses what the preview plays and the default for other programs that know just one language (the plain files in `sounds/`, the languages are in `sounds/en/`, `sounds/de/`, `sounds/fr/`, `sounds/es/`).

The sound fits the style: flat sounds clear and punchy, soft warm with a little reverb, pixel like a 16-bit console with echo, retro rough like 8-bit. The seed changes pitch, length and timbre, so two sets of the same style sound different.

In the launcher the sounds are shown below the image on the left in a second row: `◀  Sounds: Name  ▶`. **Test sounds** plays an explosion, a callout and the start of the menu music of the selected source, even if music or sounds are switched off.

Which sounds the game plays is set there or under **Game options...** with **Sounds**:

- **Automatic** (default): the sounds of the selected graphics set. With "Original" (or a set without sounds) the sounds from the CD. So switching the graphics set also switches to its sounds. If it fits between the arrows, the actual source is shown, e.g. `Sounds: Auto (Name)`, else the tooltip shows it.
- **Original (CD)**: always the sounds from the CD, also with a generated graphics set.
- **a set**: the sounds of this set, independent of the graphics set and even if the CD sounds are there.

If a sound is missing in the selected source, it comes from the CD. If there are neither CD sounds nor sounds in the selected source, the game is silent, the launcher shows "(no sounds)" then.

The sounds are switched on and off in the launcher under **Game settings**: **Play music** (menu and game) and **Play sounds** (the game sounds, the same setting as **Play sound effects** in the game options).

## The graphics generator

**Graphics...** in the launcher (or a double click on the image on the left) opens the graphics generator with the selected set. For "Original" it starts a new set.

- **Top:** name of the set, **style**, **seed** with **Roll dice** and **Own figure per player**. The seed is the number everything is computed from: same seed, same style and same settings always give exactly the same graphics.
- **Left:** the areas: figure (with the hunted and, if switched on, players 1 to 10), bombs, flames, power ups, field objects, the 11 maps, menu screens and the image for the launcher.
- **Middle:** the preview of the area. The figure walks in the selected **player color**, animation and direction can be chosen (walk, with bomb, kick, die, idle, ...). For the maps a scene shows bricks, the figure, a ticking bomb and an explosion.
- **Sounds** and **Music** are areas of their own: sliders for pitch, length, brightness and noise, or key, tempo and timbre, plus a list of all sounds to listen to (▶) and **Play music**.
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

`profile.ini` also notes who created the set: `Origin=setup` for the sets of the launcher setup (a new setup may replace them), `Origin=editor` once the set was saved in the graphics generator (never replaced by the setup). The command line version keeps the value it finds.

To share a set, just share its `profile.ini`: the generator creates the same set from it again.

## Command line

Graphics generator with user interface (`asset_generator_gui`):

- `--new`: new set with a random seed
- `--set <folder>`: open the set `assets/<folder>`
- `--auto --set <folder>`: only the style choice and the progress, as when starting without the CD; with `--style flat|soft|pixel|retro` without the choice
- `--auto --setup --folders <flat,soft,pixel,retro> --names <flat,soft,pixel,retro>`: the style choice of the setup, the chosen style is generated into its own folder (one folder and name per style, comma separated). The folder is written to `assets/last_set.txt`, the launcher then generates the other styles with `asset_generator --out assets/<folder> --profile assets/<first set>/profile.ini --style <style> --name <name>` (same seed)

Without user interface (`asset_generator`):

```
asset_generator --out assets/MySet --name "My set" --seed random --style retro
```

- `--out <folder>`: target, an existing `profile.ini` there is read
- `--profile <file>`, `--seed <number|random>`, `--style <flat|soft|pixel|retro>`, `--name <text>`
- `--per-player`: own figure per player
- `--language en|de|fr|es`: default language of the callouts (the set always contains all of them)
- `--only <a,b,...>`: only these areas, `--list` shows all of them
- `--params`: print all settings with their values

The game itself takes the set with `-assets <folder>` and the sounds with `-sounds <folder>` (the launcher passes both automatically). Without these parameters it uses the settings of the launcher (`[Launcher] AssetSet` and `SoundSet`). The folder name `auto` is reserved for the sound selection.

The launcher opens the setup with `--setup` (also `-setup`).
