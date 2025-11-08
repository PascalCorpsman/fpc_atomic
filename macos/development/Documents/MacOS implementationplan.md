# Plán implementace macOS buildů pro FPC Atomic

## 1. Příprava prostředí
- Nainstalovat FPC ≥ 3.2.2 a Lazarus IDE (Apple Silicon/Intel balíček).
- Přidat externí jednotky do `units/`: `dglOpenGL.pas`, `bass.pas`, `synapse`, `sdl2_for_pascal`.
- Nainstalovat do systému runtime knihovny (`SDL2`, `libbass.dylib`, příp. `libSDL2_mixer.dylib`) a ověřit je přes `otool -L`.

## 2. Konfigurace projektů
- V `.lpi` souborech (`client`, `server`, `launcher`, `ai`) nastavit cestu k externím jednotkám.
- Přidat build mód `macos` s výstupem do `macos/bin/`.
- Zajistit, že `sdl2_cfg.inc` používá dynamické linkování (`SDL_RUNTIME_LOADING`).

## 3. Kompilace
- Použít `lazbuild --build-mode=macos` pro `atomic_launcher`, `fpc_atomic`, `atomic_server`, `ai`.
- Po kompilaci zkontrolovat binárky (`file`, `otool -L`), zda jsou `Mach-O 64-bit` a mají správné závislosti.

## 4. Knihovny a bundlování
- Zkopírovat `libbass.dylib` a `libSDL2.dylib` do `macos/lib/` nebo `Contents/Frameworks`.
- Upravit RPATH (`install_name_tool -add_rpath @executable_path/../lib ...`).

## 5. Struktura balíčku
```
macos/
  bin/
    atomic_launcher
    fpc_atomic
    atomic_server
  lib/
    libSDL2.dylib
    libbass.dylib
  data/
    atomic/
    maps/
  tools/
    run_launcher.command
    run_server.command
```

## 6. Integrace originálních dat
- Vytvořit `macos/game_assets/` pro soukromé soubory a přidat cestu do `.gitignore`.
- Spustit `cd_data_extractor` tak, aby výstup směřoval do `macos/data/`.

## 7. Testování
- Spustit `./macos/bin/atomic_launcher` a dokončit instalaci dat.
- Ověřit běh klienta, serveru i AI modulů na macOS.

## 8. Dokumentace a distribuce
- Připravit `README_mac.md` se stručným návodem.
- Rozmyslet signování binárek a případný `.dmg` balíček.
