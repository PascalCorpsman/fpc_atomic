# Build Instructions

## Quick Build

Pro rychlý build všech aplikací a vytvoření app bundlů máte dvě možnosti:

### 1. Použít specializované build commandy (doporučeno)

```bash
# Build pro Apple Silicon (ARM64)
./macos/tools/build_all_arm64.command

# Build pro Intel Macs (x86_64)
./macos/tools/build_all_x86_64.command
```

### 2. Použít hlavní build command s parametrem

```bash
./macos/tools/build_all.command [architektura]
```

Nebo z kořenového adresáře projektu:

```bash
cd /Users/pavelzverina/AiProjects/fpc_atomic_macos
./macos/tools/build_all.command [architektura]
```

### Volba architektury (pro build_all.command)

- `arm64` nebo `aarch64` - Build pro Apple Silicon (ARM64)
- `x86_64` nebo `intel` - Build pro Intel Macs (x86_64)
- Bez argumentu - Automatická detekce podle systému

### Příklady

```bash
# Build pro Apple Silicon (doporučeno)
./macos/tools/build_all_arm64.command

# Build pro Intel Macs (doporučeno)
./macos/tools/build_all_x86_64.command

# Nebo pomocí hlavního commandu
./macos/tools/build_all.command arm64
./macos/tools/build_all.command x86_64
./macos/tools/build_all.command  # auto-detekce
```

## Co script dělá

1. **Zkompiluje všechny aplikace** pro zadanou architekturu:
   - `fpc_atomic` (klient)
   - `atomic_launcher` (launcher)
   - `atomic_server` (server)
   - `ai` (AI knihovna)
2. **Vytvoří app bundly** - spustí `build_app_bundles.command`

## Výstup

Zkompilované aplikace jsou v:
- `macos/bin/arm64/` (pro Apple Silicon)
- `macos/bin/x86_64/` (pro Intel Macs)

App bundly jsou v samostatných adresářích podle architektury:
- `macos/app_arm64/FPCAtomic.app`
- `macos/app_arm64/FPCAtomicLauncher.app`
- `macos/app_arm64/FPCAtomicServer.app`

a pro x86_64:
- `macos/app_x86_64/FPCAtomic.app`
- `macos/app_x86_64/FPCAtomicLauncher.app`
- `macos/app_x86_64/FPCAtomicServer.app`

## Požadavky

- Lazarus IDE s `lazbuild` v PATH
- macOS development tools
- Ikona v `macos/assets/AtomicBomberIcon.icns` (nebo PNG pro generování)

