# FPC Atomic – macOS Overview

Tento dokument obsahuje přehled macOS build systému. Pro detailní návod na kompilaci viz **[BUILD_GUIDE.md](tools/BUILD_GUIDE.md)**.

## Rychlý start

```bash
# ARM64 (Apple Silicon)
./macos/tools/build_all_arm64.command

# x86_64 (Intel Macs)
./macos/tools/build_all_x86_64.command
```

## Požadované nástroje
- Xcode Command Line Tools (`xcode-select --install`)
- Free Pascal Compiler 3.2.2 (balíček pro Intel+ARM)
- Lazarus IDE 3.9 (nebo novější) – zkompilovaný s podporou `lazbuild`
- SDL2 runtime (např. `brew install sdl2` nebo `SDL2.framework` z oficiálního DMG)
- BASS knihovna (`libbass.dylib` z https://www.un4seen.com/)
- **Pro x86_64 build:** Rosetta Homebrew (instaluje se automaticky přes `install_rosetta_homebrew.command`)
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

### Automatická kompilace (doporučeno)

```bash
# ARM64 (Apple Silicon)
./macos/tools/build_all_arm64.command

# x86_64 (Intel Macs) - automaticky připraví knihovny
./macos/tools/build_all_x86_64.command
```

### Ruční kompilace

Viz detailní návod v [BUILD_GUIDE.md](tools/BUILD_GUIDE.md).

## Runtime knihovny

### ARM64
- `libbass.dylib` → `macos/lib/arm64/`
- `libSDL2.dylib` → `macos/lib/arm64/` (nebo použij framework)
- OpenSSL knihovny → automaticky z Homebrew

### x86_64
- **Automatická příprava:** Skript `prepare_x86_64_libs.command` automaticky:
  - Ověří architektury všech knihoven
  - Zkopíruje x86_64 OpenSSL z Rosetta Homebrew
  - Vytvoří `libSDL2.dylib` z frameworku
- **Ruční příprava:** Viz [BUILD_GUIDE.md](tools/BUILD_GUIDE.md)

### Poznámky
- SDL2 se načítá runtime (`SDL_RUNTIME_LOADING`), není potřeba explicitní linkování
- lNet knihovna: po stažení `https://github.com/PascalCorpsman/lnet` spusť `make lib` a obsah `lib/` synchronizuj do `macos/third_party/lnet/`
- SDL2 Pascal headers: `git clone https://github.com/PascalCorpsman/SDL2-for-Pascal.git macos/third_party/sdl2_for_pascal`, poté zkopíruj `macos/third_party/sdl2_for_pascal/units/*` do `units/sdl2_for_pascal/`

## Spouštěcí skripty
- `macos/tools/run_launcher.command`
- `macos/tools/run_client.command`
- `macos/tools/run_server.command`

Skript zjistí architekturu (`uname -m`), nastaví `DYLD_LIBRARY_PATH` na odpovídající podsložku a spustí binárku. V případě Apple Silicon můžeš použít Rosettu pro x86_64 build: `arch -x86_64 macos/tools/run_client.command`.

## Vytvoření `.app` balíčků

App bundly se vytvoří automaticky při použití `build_all_*.command` skriptů.

### Výstup
- `macos/app_arm64/` - App bundly pro Apple Silicon
- `macos/app_x86_64/` - App bundly pro Intel Macs

Každý obsahuje:
- `FPCAtomic.app` (klient)
- `FPCAtomicLauncher.app` (launcher + klient + server)
- `FPCAtomicServer.app` (samostatný server)

### Code signing
- App bundly jsou automaticky podepsané ad-hoc podpisem
- Pro distribuci je potřeba Developer ID podpis a notarizace
- Viz [BUILD_GUIDE.md](tools/BUILD_GUIDE.md) pro detailní informace

Tipy k použití:
- Launcher spuštěný z `.app` používá interní kopii klienta/serveru a nastavený pracovní adresář.
- Server `.app` při dvojkliku otevře Terminal a spustí `atomic_server`; parametry můžeš přidat přes `open macos/app/FPCAtomicServer.app --args -p 1234`.
- `macos/tools/run_server.command` i `.app` varianta serveru automaticky doplní `-p 5521` a `-t 0`, pokud nejsou předané – server tak zůstane běžet neomezeně dlouho. Vlastní port nebo timeout nastavíš přidáním `-p`/`-t` argumentů.

## Integrace originálních dat
1. Zkopíruj obsah CD a (volitelně) expansion pack do `macos/game_assets/`.
2. Spusť `macos/tools/run_launcher.command` nebo samostatný nástroj `bin/<arch>/cd_data_extractor` (po dořešení buildů) a nasměruj výstup do `macos/data/`.
3. `macos/data/` zůstává verzovací čisté (ignorováno v `.gitignore`).

## Testování
- Spusť server: `macos/tools/run_server.command -p 9876`.
- Spusť klienta: `macos/tools/run_client.command --some-option` (parametry dle `MANUAL.md`).
- Launcher ověří dostupnost dat a aktualizací.

## Dokumentace

- **[BUILD_GUIDE.md](tools/BUILD_GUIDE.md)** - Kompletní návod na kompilaci
- **[BUILD_README.md](tools/BUILD_README.md)** - Rychlý přehled build skriptů
- **[MacOS implementationplan.md](development/Documents/MacOS%20implementationplan.md)** - Technický plán implementace

## Další kroky
- Zvážit vytvoření `.dmg` nebo `.pkg` pro distribuci
- Udržovat dokumentaci aktuální
