# Tests

Small console programs without GUI. They need only Free Pascal and are not part of a release.
Each one is built and run from this folder.
Exit code 0 means all is fine, 1 means a test or check failed.

The compiled units go to `lib/` (`-FUlib`), so nothing is written next to the sources. Free Pascal does not create that folder, so run `mkdir lib` once.

## test_startsets

Which graphics set and which sounds the game starts with. Covers the parameters `-assets` and `-sounds`, the launcher settings `[Launcher] AssetSet` and `SoundSet`, and the case without any graphics.

```sh
fpc -Mobjfpc -Sh -Fu../units -FUlib test_startsets.lpr
./test_startsets
```

Works in a temporary folder and does not touch the installed sets.

## test_soundlang

The language of the spoken sounds and the shown names of the sets.
A sound is looked up in this order:

1. `<dir>/<language>/<file>` of the set
2. `<dir>/en/<file>` of the set
3. the plain file of the set
4. `data/`

```sh
fpc -Mobjfpc -Sh -Fu../units -FUlib test_soundlang.lpr
./test_soundlang
```

Works in a temporary folder and does not touch the installed sets.

## test_assetset

Checks a generated set against what the game expects:

- size of every PNG compared with the original in `data/`, if it is there
- pink fringe: pixels close to the transparent color fuchsia, the game would show them pink
- green pixels: the game recolors them to the player color. Outside `atomic/` they are only reported as info.
- format of every `.ani` file

```sh
fpc -Mobjfpc -Sh -FUlib test_assetset.lpr
./test_assetset ../assets/Flat [<data dir>]
```

Without a data dir it uses `../data` next to the program. Without the CD data the sizes are not compared, the other checks still run.

## test_ai_smart

The smart ai (`ai_smart/`) with the mods the server reports to it: escaping over an open border, through bricks while walking through bricks is possible, and the boxing glove. Each test builds a small field by hand and calls the ai directly, without a server.

```sh
fpc -Mobjfpc -Sh -Fu../ai_smart -Fu../server -FUlib test_ai_smart.lpr
./test_ai_smart
```

It reads `ai_smart.ini` like the ai (next to the program first). The tests need the mod abilities at full strength, e.g. an `ai_smart.ini` next to the program with `[General]` `Difficulty=100`.
