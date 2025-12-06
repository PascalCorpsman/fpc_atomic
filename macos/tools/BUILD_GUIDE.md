# FPC Atomic - macOS Build Guide

Kompletní návod pro kompilaci FPC Atomic na macOS pro obě architektury (ARM64 a x86_64).

## Obsah

1. [Požadavky](#požadavky)
2. [Příprava prostředí](#příprava-prostředí)
3. [Příprava knihoven](#příprava-knihoven)
4. [Kompilace](#kompilace)
5. [Vytvoření app bundlů](#vytvoření-app-bundlů)
6. [Řešení problémů](#řešení-problémů)

---

## Požadavky

### Základní nástroje

- **macOS** 10.15 (Catalina) nebo novější
- **Xcode Command Line Tools**: `xcode-select --install`
- **Free Pascal Compiler** 3.2.2 nebo novější (balíček pro Intel+ARM)
- **Lazarus IDE** 3.9 nebo novější (s podporou `lazbuild`)
- **Homebrew** (pro instalaci knihoven)

### Pascal jednotky

Tyto jednotky musí být umístěny v `units/`:

- `dglOpenGL.pas` - stáhnout z https://github.com/saschawillems/dglopengl
- `bass.pas` - stáhnout z https://www.un4seen.com/
- `synapse/` - celá složka z http://www.ararat.cz/synapse/doku.php/download
- `sdl2_for_pascal/` - hlavičky SDL2 z https://github.com/PascalCorpsman/SDL2-for-Pascal

### Lazarus balíčky

V Lazarus IDE nainstaluj:
- **Lnet** z https://github.com/PascalCorpsman/lnet
- **LazOpenGLContext** (z dostupných balíčků v IDE)

---

## Příprava prostředí

### 1. Instalace Xcode Command Line Tools

```bash
xcode-select --install
```

### 2. Instalace Free Pascal a Lazarus

Stáhni a nainstaluj z https://www.lazarus-ide.org/:
- Balíček obsahující FPC i Lazarus
- Verze podporující jak ARM64, tak x86_64

### 3. Ověření instalace

```bash
# Zkontroluj FPC
fpc -i

# Zkontroluj lazbuild
lazbuild --version

# Zkontroluj x86_64 kompilátor (pro cross-compilation)
ppcx64 -i
```

---

## Příprava knihoven

### ARM64 (Apple Silicon)

#### SDL2

SDL2 se načítá runtime, takže není potřeba explicitní linkování. Framework nebo dylib musí být k dispozici:

```bash
# Možnost 1: Homebrew (doporučeno)
brew install sdl2

# Možnost 2: Stáhnout framework z https://www.libsdl.org/download-2.0.php
# a zkopírovat do macos/lib/arm64/frameworks/
```

#### BASS

```bash
# Stáhni libbass.dylib z https://www.un4seen.com/
# Zkopíruj do:
cp libbass.dylib macos/lib/arm64/
```

#### OpenSSL

OpenSSL knihovny jsou obvykle k dispozici přes Homebrew:

```bash
brew install openssl@3

# Zkopíruj knihovny (pokud jsou potřeba v projektu)
cp /opt/homebrew/lib/libssl.3.dylib macos/lib/arm64/ 2>/dev/null || true
cp /opt/homebrew/lib/libcrypto.3.dylib macos/lib/arm64/ 2>/dev/null || true
```

### x86_64 (Intel Macs / Cross-compilation)

Pro kompilaci x86_64 verze na Apple Silicon Macu je potřeba:

#### 1. Rosetta Homebrew (pro x86_64 knihovny)

```bash
# Instalace Rosetta Homebrew
arch -x86_64 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Instalace OpenSSL@3 přes Rosetta Homebrew
arch -x86_64 /usr/local/bin/brew install openssl@3
```

**Poznámka:** Rosetta Homebrew se instaluje do `/usr/local` a neovlivní tvůj ARM64 Homebrew v `/opt/homebrew`.

#### 2. Automatická příprava knihoven

Skript `prepare_x86_64_libs.command` automaticky:
- Ověří architektury všech knihoven
- Zkopíruje x86_64 OpenSSL z Rosetta Homebrew
- Vytvoří `libSDL2.dylib` z frameworku
- Zobrazí varování, pokud něco chybí

```bash
# Spusť ručně (nebo se spustí automaticky při buildu)
./macos/tools/prepare_x86_64_libs.command
```

#### 3. SDL2 Framework

SDL2 framework pro x86_64 musí být v:
```
macos/lib/x86_64/frameworks/SDL2.framework/
```

Stáhni z https://www.libsdl.org/download-2.0.php a zkopíruj správnou architekturu.

#### 4. BASS

```bash
# Zkopíruj x86_64 verzi libbass.dylib
cp libbass.dylib macos/lib/x86_64/
```

---

## Kompilace

### Rychlý build (doporučeno)

Pro automatický build všech aplikací a vytvoření app bundlů:

```bash
# ARM64 (Apple Silicon)
./macos/tools/build_all_arm64.command

# x86_64 (Intel Macs)
./macos/tools/build_all_x86_64.command
```

### Ruční build

#### ARM64

```bash
lazbuild --build-mode=macos_arm64 --lazarusdir=/Applications/Lazarus_3.9 client/fpc_atomic.lpi
lazbuild --build-mode=macos_arm64 --lazarusdir=/Applications/Lazarus_3.9 launcher/atomic_launcher.lpi
lazbuild --build-mode=macos_arm64 --lazarusdir=/Applications/Lazarus_3.9 server/atomic_server.lpi
lazbuild --build-mode=macos_arm64 --lazarusdir=/Applications/Lazarus_3.9 ai/ai.lpi
```

#### x86_64

```bash
# Ujisti se, že máš x86_64 kompilátor
export PP=/usr/local/bin/ppcx64  # nebo cesta k tvému ppcx64

lazbuild --build-mode=macos_x86_64 --lazarusdir=/Applications/Lazarus_3.9 --compiler=/usr/local/bin/ppcx64 client/fpc_atomic.lpi
lazbuild --build-mode=macos_x86_64 --lazarusdir=/Applications/Lazarus_3.9 --compiler=/usr/local/bin/ppcx64 launcher/atomic_launcher.lpi
lazbuild --build-mode=macos_x86_64 --lazarusdir=/Applications/Lazarus_3.9 --compiler=/usr/local/bin/ppcx64 server/atomic_server.lpi
lazbuild --build-mode=macos_x86_64 --lazarusdir=/Applications/Lazarus_3.9 --compiler=/usr/local/bin/ppcx64 ai/ai.lpi
```

### Výstup

Zkompilované binárky jsou v:
- `macos/bin/arm64/` (pro Apple Silicon)
- `macos/bin/x86_64/` (pro Intel Macs)

---

## Vytvoření app bundlů

### Automatické vytvoření

App bundly se vytvoří automaticky při použití `build_all_*.command` skriptů.

### Ruční vytvoření

```bash
# ARM64
./macos/tools/build_app_bundles.command arm64

# x86_64
./macos/tools/build_app_bundles.command x86_64
```

### Výstup

App bundly jsou v:
- `macos/app_arm64/`:
  - `FPCAtomic.app` (klient)
  - `FPCAtomicLauncher.app` (launcher + klient + server)
  - `FPCAtomicServer.app` (server)

- `macos/app_x86_64/`:
  - `FPCAtomic.app` (klient)
  - `FPCAtomicLauncher.app` (launcher + klient + server)
  - `FPCAtomicServer.app` (server)

### Struktura app bundle

```
FPCAtomic.app/
  Contents/
    MacOS/
      fpc_atomic          # Hlavní executable
      data -> ../../../data  # Symlink na sdílená data
    lib/
      libSDL2.dylib
      libbass.dylib
      libai.dylib
      libssl.3.dylib
      libcrypto.3.dylib
    Resources/
      AppIcon.icns
    Info.plist
```

### Code signing

App bundly jsou automaticky podepsané ad-hoc podpisem (`codesign --force --deep --sign -`), což je dostačující pro lokální testování.

**Pro distribuci** je potřeba Developer ID podpis a notarizace (viz sekce Řešení problémů).

---

## Řešení problémů

### Problém: Chybějící x86_64 knihovny

**Příznaky:**
```
Library not loaded: @rpath/libssl.3.dylib
```

**Řešení:**
1. Nainstaluj Rosetta Homebrew (viz sekce Příprava knihoven)
2. Spusť `./macos/tools/prepare_x86_64_libs.command`
3. Ověř architektury: `file macos/lib/x86_64/*.dylib`

### Problém: SDL2 knihovna nenalezena

**Příznaky:**
```
Library not loaded: @rpath/libSDL2.dylib
```

**Řešení:**
1. Ověř, že `libSDL2.dylib` existuje v `macos/lib/<arch>/`
2. Pokud chybí, skript `prepare_x86_64_libs.command` ho vytvoří z frameworku
3. Zkontroluj RPATH: `otool -l <executable> | grep LC_RPATH`

### Problém: Neplatný code signature

**Příznaky:**
```
code signature invalid
```

**Řešení:**
```bash
# Znovu podepiš všechny knihovny
for lib in macos/app_*/FPCAtomic.app/Contents/lib/*.dylib; do
  codesign --force --sign - "$lib"
done

# Znovu podepiš app bundly
for app in macos/app_*/FPCAtomic.app; do
  codesign --force --deep --sign - "$app"
done
```

### Problém: Gatekeeper blokuje aplikaci

**Příznaky:**
```
"Aplikace je poškozena a nelze ji otevřít"
```

**Řešení pro lokální testování:**
```bash
# Odstranění quarantine atributu
xattr -cr macos/app_*/FPCAtomic.app
```

**Pro distribuci:**
- Použij Developer ID Application certifikát
- Podepiš všechny komponenty
- Notarizuj aplikaci přes Apple

### Problém: Cross-compilation selhává

**Příznaky:**
```
ld: library not found for -lSDL2
```

**Řešení:**
1. Ověř, že máš x86_64 kompilátor: `ppcx64 -i`
2. Zkontroluj, že SDL2 není linkován staticky (používá se runtime loading)
3. Ověř nastavení v `client/fpc_atomic.lpi` - mělo by být `SDL_RUNTIME_LOADING`

### Problém: Chybějící data

**Příznaky:**
Aplikace se spustí, ale chybí grafika/zvuky

**Řešení:**
1. Zkopíruj obsah originálního CD do `macos/game_assets/`
2. Spusť launcher a nech ho extrahovat data do `macos/data/`
3. Ověř, že symlink `Contents/MacOS/data` existuje v app bundle

---

## Testování

### Spuštění aplikací

```bash
# Launcher
open macos/app_arm64/FPCAtomicLauncher.app

# Klient (přímo)
macos/app_arm64/FPCAtomic.app/Contents/MacOS/fpc_atomic

# Server
open macos/app_arm64/FPCAtomicServer.app
```

### Ověření architektur

```bash
# Binárky
file macos/bin/arm64/fpc_atomic
file macos/bin/x86_64/fpc_atomic

# Knihovny
file macos/lib/arm64/*.dylib
file macos/lib/x86_64/*.dylib

# App bundly
file macos/app_arm64/FPCAtomic.app/Contents/MacOS/fpc_atomic
file macos/app_x86_64/FPCAtomic.app/Contents/MacOS/fpc_atomic
```

### Ověření podpisů

```bash
# App bundle
codesign -dv macos/app_arm64/FPCAtomic.app

# Knihovny
codesign -dv macos/app_arm64/FPCAtomic.app/Contents/lib/*.dylib

# Ověření integrity
codesign --verify --verbose macos/app_arm64/FPCAtomic.app
```

---

## Další kroky

- **Distribuce:** Pro distribuci mimo App Store je potřeba Developer ID podpis a notarizace
- **DMG balíček:** Vytvoř `.dmg` pro snadnou distribuci
- **Aktualizace:** Udržuj tento dokument aktuální s novými změnami

---

## Užitečné odkazy

- [Lazarus IDE](https://www.lazarus-ide.org/)
- [Free Pascal Compiler](https://www.freepascal.org/)
- [SDL2](https://www.libsdl.org/)
- [BASS Audio Library](https://www.un4seen.com/)
- [Homebrew](https://brew.sh/)

