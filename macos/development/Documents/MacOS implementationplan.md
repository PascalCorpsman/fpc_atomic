# Plán implementace macOS buildů pro FPC Atomic

## 1. Příprava prostředí
- Nainstalovat FPC ≥ 3.2.2 a Lazarus IDE (Apple Silicon/Intel balíček).
- Přidat externí jednotky do `units/`: `dglOpenGL.pas`, `bass.pas`, `synapse`, `sdl2_for_pascal`.
- Připravit runtime knihovny:
  - `libbass.dylib` zkopírovat do `macos/lib/arm64` a `macos/lib/x86_64`.
  - `libSDL2.dylib` z `$(brew --prefix sdl2)/lib/` zkopírovat do `macos/lib/<arch>/`.
  - `libssl.3.dylib` a `libcrypto.3.dylib` z `$(brew --prefix openssl@3)/lib/` zkopírovat do `macos/lib/<arch>/`.
  - Po zkopírování odstranit atribut karantény a podepsat (viz README_mac.md).

## 2. Konfigurace projektů
- V `.lpi` souborech (`client`, `server`, `launcher`, `ai`) nastavit cesty k externím jednotkám a výstupy do `macos/bin/<arch>`.
- SDL zůstává v režimu runtime load (define `SDL_RUNTIME_LOADING` v `client/` a `launcher/sdl2_cfg.inc`).
- Synapse komponenty používají `ssl_openssl3`, takže se očekává OpenSSL 3 knihovna (viz výše).

## 3. Kompilace
- Pro arm64 používat `lazbuild --build-mode=macos_arm64 --lazarusdir=/Applications/Lazarus_3.9 <projekt>.lpi`.
- Linker musí běžet přes `ld-classic`, takže jsou v `.lpi` nastaveny parametr `-k-ld_classic`.
- SDL2 se načítá runtime (`SDL_RUNTIME_LOADING`), proto není potřeba linkovat `libSDL2.dylib`; BASS se linkuje explicitně (`-k <path>/libbass.dylib`).
- Po kompilaci zkontrolovat binárky (`file`, `otool -L`), zda jsou `Mach-O 64-bit` a mají správné závislosti.

## 4. Knihovny a bundlování
- V `macos/lib/<arch>/` musí být reálné kopie `libbass.dylib`, `libSDL2.dylib`, `libssl.3.dylib`, `libcrypto.3.dylib` (žádné symlinky na Homebrew cestu).
- `.lpi` přidávají RPATH (`-k-rpath -k@executable_path/../lib`) a explicitně linkují `libbass.dylib`; ostatní knihovny se načítají dynamicky.
- Po doplnění knihoven je vhodné spustit ad-hoc podpis (viz README_mac.md).
- Pro `$(TargetCPU)=aarch64` existuje symlink `macos/lib/aarch64 -> arm64`.

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
