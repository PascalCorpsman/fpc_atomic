# GitHub Actions Workflows

Tento adresář obsahuje CI/CD workflow pro automatické buildu aplikací pro všechny platformy.

## Build Windows

Workflow `build-windows.yml` automaticky kompiluje všechny aplikace pro Windows při:
- Push do `main` nebo `master` branch
- Pull request do `main` nebo `master` branch
- Manuálním spuštění přes GitHub Actions tab

## Build macOS

Workflow `build-macos.yml` automaticky kompiluje všechny aplikace pro macOS (ARM64 i x86_64) při:
- Push do `main` nebo `master` branch
- Pull request do `main` nebo `master` branch
- Manuálním spuštění přes GitHub Actions tab

**Poznámka**: macOS buildy běží na dvou runnerech:
- `macos-latest` - Apple Silicon (ARM64)
- `macos-13` - Intel (x86_64)

## Build Linux

Workflow `build-linux.yml` automaticky kompiluje všechny aplikace pro Linux při:
- Push do `main` nebo `master` branch
- Pull request do `main` nebo `master` branch
- Manuálním spuštění přes GitHub Actions tab

### Jak to funguje:

1. **Automatické spuštění**: Když pushneš změny do GitHubu, workflow se automaticky spustí
2. **Windows runner**: GitHub poskytne virtuální Windows počítač
3. **Instalace nástrojů**: Automaticky se nainstaluje Free Pascal Compiler a Lazarus IDE
4. **Kompilace**: Všechny aplikace se zkompilují (client, launcher, server, extractor)
5. **Artefakty**: Zkompilované `.exe` soubory jsou k dispozici ke stažení

### Jak použít:

1. **Pushni změny do GitHubu**:
   ```bash
   git add .
   git commit -m "Update code"
   git push
   ```

2. **Zkontroluj stav buildu**:
   - Jdi na GitHub → tvůj repozitář
   - Klikni na záložku "Actions"
   - Uvidíš běžící nebo dokončené buildu

3. **Stáhni artefakty**:
   - Po dokončení buildu klikni na workflow run
   - V sekci "Artifacts" klikni na příslušný artifact:
     - `windows-binaries` - Windows `.exe` soubory
     - `macos-arm64-binaries` - macOS ARM64 binárky
     - `macos-x86_64-binaries` - macOS Intel binárky
     - `linux-binaries` - Linux binárky
   - Stáhneš ZIP soubor se všemi zkompilovanými soubory

### Manuální spuštění:

1. Jdi na GitHub → tvůj repozitář → záložka "Actions"
2. Vyber workflow "Build Windows"
3. Klikni na "Run workflow"
4. Vyber branch a klikni "Run workflow"

### Poznámky:

- Build trvá obvykle 5-15 minut (závisí na platformě)
- Artefakty jsou dostupné 30 dní
- Pokud build selže, zkontroluj logy v sekci "Actions"
- Všechny tři workflow (Windows, macOS, Linux) běží paralelně, takže dostaneš buildy pro všechny platformy najednou

