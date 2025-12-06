# Plán implementace macOS buildů pro FPC Atomic

## 1. Příprava prostředí
- Nainstalovat FPC ≥ 3.2.2 a Lazarus IDE (Apple Silicon/Intel balíček).
- Přidat externí jednotky do `units/`: `dglOpenGL.pas`, `bass.pas`, `synapse`, `sdl2_for_pascal`.
- Nainstalovat do systému runtime knihovny (`SDL2`, `libbass.dylib`, příp. `libSDL2_mixer.dylib`) a ověřit je přes `otool -L`.

## 2. Konfigurace projektů
- V `.lpi` souborech (`client`, `server`, `launcher`, `ai`) nastavit cestu k externím jednotkám.
- Přidat build mód `macos` s výstupem do `macos/bin/`.
// - Zajistit, že `sdl2_cfg.inc` používá dynamické linkování (`SDL_RUNTIME_LOADING`).

## 3. Kompilace
- Pro arm64 používat `lazbuild --build-mode=macos_arm64 --lazarusdir=/Applications/Lazarus_3.9 <projekt>.lpi`.
- Linker musí běžet přes `ld-classic`, takže jsou v `.lpi` nastaveny parametr `-k-ld_classic`.
- SDL2 se načítá runtime (`SDL_RUNTIME_LOADING`), proto není potřeba linkovat `libSDL2.dylib`; BASS se linkuje explicitně (`-k <path>/libbass.dylib`).
- Po kompilaci zkontrolovat binárky (`file`, `otool -L`), zda jsou `Mach-O 64-bit` a mají správné závislosti.

## 4. Knihovny a bundlování
- Zkopírovat `libbass.dylib` (a případně `libSDL2.dylib`) do `macos/lib/<arch>/`.
- RPATH je přidán v `.lpi` (`-k-rpath -k@executable_path/../lib`).
- Pro `$(TargetCPU)=aarch64` je v `macos/lib` vytvořen symlink `aarch64 -> arm64`.

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
- Udržovat `macos/development/Documents/MacOS implementationplan.md` aktuální.

## 9. Cross-build pro x86_64
- Lazarus balík obsahuje `ppcx64`, ale je potřeba dodělat LCL jednotky pro `x86_64-darwin` (`Tools → Build Lazarus with Profile` nebo `lazbuild --cpu=x86_64 --os=darwin --build-ide=`).
- Po zprovoznění cross jednotek lze použít `lazbuild --build-mode=macos_x86_64`.
