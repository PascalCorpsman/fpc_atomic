# Build Instructions

## Quick Build

Pro rychlý build všech aplikací a vytvoření app bundlů jednoduše spusť:

```bash
./macos/tools/build_all.command
```

Nebo z kořenového adresáře projektu:

```bash
cd /Users/pavelzverina/AiProjects/fpc_atomic_macos
./macos/tools/build_all.command
```

## Co script dělá

1. **Detekuje architekturu** - automaticky rozpozná arm64 nebo x86_64
2. **Zkompiluje všechny aplikace:**
   - `fpc_atomic` (klient)
   - `atomic_launcher` (launcher)
   - `atomic_server` (server)
   - `ai` (AI knihovna)
3. **Vytvoří app bundly** - spustí `build_app_bundles.command`

## Výstup

Zkompilované aplikace jsou v:
- `macos/bin/arm64/` (nebo `macos/bin/x86_64/`)

App bundly jsou v:
- `macos/app/FPCAtomic.app`
- `macos/app/FPCAtomicLauncher.app`
- `macos/app/FPCAtomicServer.app`

## Požadavky

- Lazarus IDE s `lazbuild` v PATH
- macOS development tools
- Ikona v `macos/assets/AtomicBomberIcon.icns` (nebo PNG pro generování)

