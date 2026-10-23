# FPC Atomic – Build & Run Notes

Free Pascal / Lazarus reimplementation of Atomic Bomberman. No central build system;
the Lazarus projects (`.lpi`) are built with `lazbuild`, wrapped by the `Makefile`.

## Components

| Target     | Project                          | Output (repo root) |
|------------|----------------------------------|--------------------|
| client     | `client/fpc_atomic.lpi`          | `fpc_atomic`       |
| server     | `server/atomic_server.lpi`       | `atomic_server`    |
| launcher   | `launcher/atomic_launcher.lpi`   | `atomic_launcher`  |
| ai (Pascal)| `ai/ai.lpi`                      | `libai.so`         |
| ai (C)     | `ai_c/makefile` (gcc)            | `libai.so` (same file, don't build both) |

`ai_empty/` is a template only. Shared units live in `units/`.

## Building

```bash
make              # server, ai, client, launcher
make server       # or: client | launcher | ai | ai_c
```

Tool paths (override via env/make vars): `LAZBUILD=~/fpcupdeluxe/trunk/lazarus/lazbuild`,
`SYNAPSE=~/projekte/synapse` (Synapse lives outside the repo), `WS=gtk2`.

Facts learned the hard way:
- **Use `--ws=gtk2`.** The default widgetset is gtk3 and fails at link time
  (`-lgtk-3 not found`, no `libgtk-3-dev`). `libgtk2.0-dev` is installed.
- **A failed link deletes the existing binary** in the repo root (FPC removes the target
  first). Back up `fpc_atomic` etc. before building if you need the old one.
- The `.lpi` search paths (`../../Sample/...`) don't exist here, so the Makefile passes
  extra `-Fu` paths: `client/`, `client/sdl2`, SDL2-for-Pascal `units`, Synapse.
  `dglOpenGL.pas` and `bass.pas` are in `client/` (also needed by the launcher).
- Lazarus packages `lnetbase` and `lnetvisual` must be registered once (needed by server/client):
  `lazbuild --add-package-link ~/projekte/lnet/lazaruspackage/lnetbase.lpk` (same for `lnetvisual.lpk`).
  This is already done on this machine.
- Build artifacts (`*.so`, `*.o`, `*.ppu`, `*/lib/`) are git-ignored.
- Builds currently include debug info (binaries ~40 MB); a `Release` build mode is not defined in all `.lpi`s.

## Running

The game needs `libbass.so` and `libai.so` at runtime. They are in the repo root, which is
not on the loader path:

```bash
LD_LIBRARY_PATH=. ./fpc_atomic
```

(or `sudo cp libbass.so /usr/local/lib && sudo ldconfig`; `libbass.so` is already in
`/usr/local/lib` on this machine.) Game data comes from `data/` – copyrighted, not in git.

## Verifying a change

There is no test suite. Verify by building the affected target(s) with `make <target>`
and checking for `Linking ...` without `Error`. For gameplay, run client/server manually.
