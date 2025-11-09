# FPC Atomic – macOS build instrukce

## Požadované nástroje
- Xcode Command Line Tools (`xcode-select --install`)
- Free Pascal Compiler 3.2.2 (balíček pro Intel+ARM)
- Lazarus IDE 3.9 (nebo novější) – zkompilovaný s podporou `lazbuild`
- SDL2 runtime (např. `brew install sdl2` nebo `SDL2.framework` z oficiálního DMG)
- BASS knihovna (`libbass.dylib` z https://www.un4seen.com/)
- Další Pascal jednotky umístěné do `units/`:
  - `dglOpenGL.pas`
  - `bass.pas`
  - `synapse/` (celá složka)
  - `sdl2_for_pascal/` (hlavičky SDL2 od autora projektu)

## Struktura adresáře `macos/`
```
macos/
  bin/
    arm64/
    x86_64/
  lib/
    arm64/
    x86_64/
  data/
    atomic/
    maps/
  game_assets/        # sem patří proprietární data z originální hry (není versionováno)
  tools/
    run_launcher.command
    run_client.command
    run_server.command
  development/
    Documents/
      MacOS implementationplan.md
```

## Build módy v `.lpi`
- Každý projekt (klient, launcher, server, AI) má módy `macos_arm64` a `macos_x86_64`.
- Výstupy se generují do `macos/bin/<arch>` a `macos/lib/<arch>`.
- Jednotky se stále kompilují do `lib/<cpu>-<os>` v rámci projektu.

### Spuštění kompilace
```
# ARM64 (Apple Silicon)
lazbuild --build-mode=macos_arm64 launcher/atomic_launcher.lpi
lazbuild --build-mode=macos_arm64 client/fpc_atomic.lpi
lazbuild --build-mode=macos_arm64 server/atomic_server.lpi
lazbuild --build-mode=macos_arm64 ai/ai.lpi

# Intel (x86_64) – lze spustit z Apple Silicon díky cross-kompilátoru
lazbuild --build-mode=macos_x86_64 launcher/atomic_launcher.lpi
lazbuild --build-mode=macos_x86_64 client/fpc_atomic.lpi
lazbuild --build-mode=macos_x86_64 server/atomic_server.lpi
lazbuild --build-mode=macos_x86_64 ai/ai.lpi
```

> **Poznámka:** Pokud kompilace pro x86_64 hlásí chybějící SDK, přidej do příkazů parametry `--lazarusdir=/Applications/Lazarus_3.9` a pro FPC `-XR/path/k/SDK` (např. `/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk`).

## Runtime knihovny
- Zkopíruj `libbass.dylib` do `macos/lib/arm64` a `macos/lib/x86_64`. Pokud máš univerzální `dylib`, může být stejný soubor v obou adresářích.
- `SDL2.framework`/`libSDL2.dylib` umísti do `macos/lib/<arch>` nebo ponech v systému. Pokud používáš `.dylib`, ujisti se, že `DYLD_LIBRARY_PATH` ukazuje na `macos/lib/<arch>`.
- lNet knihovna: po stažení `https://github.com/PascalCorpsman/lnet` spusť `make lib` a obsah `lib/` synchronizuj do `macos/third_party/lnet/`. Build módy již obsahují cesty `../macos/third_party/lnet/lib` a `lib/sys`.
- SDL2 Pascal headers: `git clone https://github.com/PascalCorpsman/SDL2-for-Pascal.git macos/third_party/sdl2_for_pascal`, poté zkopíruj `macos/third_party/sdl2_for_pascal/units/*` do `units/sdl2_for_pascal/`.

## Spouštěcí skripty
- `macos/tools/run_launcher.command`
- `macos/tools/run_client.command`
- `macos/tools/run_server.command`

Skript zjistí architekturu (`uname -m`), nastaví `DYLD_LIBRARY_PATH` na odpovídající podsložku a spustí binárku. V případě Apple Silicon můžeš použít Rosettu pro x86_64 build: `arch -x86_64 macos/tools/run_client.command`.

## Integrace originálních dat
1. Zkopíruj obsah CD a (volitelně) expansion pack do `macos/game_assets/`.
2. Spusť `macos/tools/run_launcher.command` nebo samostatný nástroj `bin/<arch>/cd_data_extractor` (po dořešení buildů) a nasměruj výstup do `macos/data/`.
3. `macos/data/` zůstává verzovací čisté (ignorováno v `.gitignore`).

## Testování
- Spusť server: `macos/tools/run_server.command -p 9876`.
- Spusť klienta: `macos/tools/run_client.command --some-option` (parametry dle `MANUAL.md`).
- Launcher ověří dostupnost dat a aktualizací.

## Další kroky
- Přidat případné skripty pro nastavení RPATH (`install_name_tool`) po kompilaci.
- Zvážit vytvoření `.dmg` nebo `.pkg` pro distribuci.
- Udržovat `macos/development/Documents/MacOS implementationplan.md` jako living dokument.
