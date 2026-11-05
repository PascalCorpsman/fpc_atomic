# Developer tools of the asset generator

These tools are for developers working on the sounds of the asset generator.
The game, the launcher and the generator do not need them, and they are not part of a release.

## wav2ini

`wav2ini` helps to write the sound definitions `asset_generator/sounds.ini` and the menu music `asset_generator/music.ini`.
Both files are compiled into the generator as include files (`uag_sounds_ini.inc`, `uag_music_ini.inc`).

**Build** from this folder. The compiled units go to `lib/`, which Free Pascal does not create by itself:

```sh
mkdir lib
fpc -Mobjfpc -Sh -O2 -Fu.. -FUlib wav2ini.lpr
```

### After every change of sounds.ini or music.ini

The generator does not read the ini files at runtime. After a change, the include file must be written again and the generator rebuilt:

```sh
wav2ini --inc ../sounds.ini ../uag_sounds_ini.inc
wav2ini --inc ../music.ini ../uag_music_ini.inc
```

Commit the `.ini` and the `.inc` together.

### Creating sound designs from the CD sounds

The sounds of the generator are an own interpretation of the CD sounds.
`wav2ini` measures only coarse features (length, envelope, pitch, noise share, brightness, loudness) and writes a new synthesizer description from them.
No audio data is copied.

| Command | What it does |
| --- | --- |
| `wav2ini --design <data dir> <out.ini>` | Measures `data/sounds/*.wav`, `data/res/menuexit.wav` and `quitgame.wav` and writes a design per file. The result is a draft that is refined by hand and then pasted into `sounds.ini`. |
| `wav2ini --voices <data dir> <en\|de\|fr\|es> <out.ini>` | Only the spoken sounds, with the own phrases of that language. English becomes `[name]`, the others `[name@de]` (`@fr`, `@es`). |

### Listening and comparing

| Command | What it does |
| --- | --- |
| `wav2ini --render <sounds.ini> <out dir> [name ...]` | Renders all or the named sections as WAV files. |
| `wav2ini --music <music.ini> <out dir> [kit [seed]]` | Renders the music, optionally with the sound kit of a style (`flat`, `soft`, `pixel`, `retro`). |
| `wav2ini --features <file.wav> ...` | Prints the measured features of the files. |
| `wav2ini --compare <dir a> <dir b>` | Features of every WAV in `a` next to the file with the same name in `b`, e.g. the rendered sounds next to the originals. |
| `wav2ini --png <out.png> <file.wav> ...` | Waveform and spectrogram of each file, stacked with the same time scale. |

## Developer options of asset_generator_gui

The options for users are described in [GRAPHICS_MANUAL.md](../../GRAPHICS_MANUAL.md#command-line).
These additional options are for automated tests: the window does its work, then closes by itself.

| Option | What it does |
| --- | --- |
| `--screenshot <file.png>` | Saves a picture of the window as soon as the preview is ready, then closes. Also works for the style choice (`--auto`). |
| `--area <n>` | Selects area number `n` of the tree on start (0 = first), e.g. for a screenshot. |
| `--anim <n>` | Selects animation number `n` of the figure preview. |
| `--test-save <folder>` | Does what a user would do (per player figures on, change a parameter, roll the dice), saves the set into `assets/<folder>` and closes. Errors are not shown in a dialog. |
| `--cancel-after <n>` | Cancels saving after `n` files, to test that a cancelled save leaves the old set unchanged. |
| `--test-sound <file.txt>` | Plays a sound at volume 0 and writes the result (sound device, number of sounds) into the text file, then closes. |
