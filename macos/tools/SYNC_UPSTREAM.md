# Jak synchronizovat změny z upstream repozitáře

Tento dokument popisuje, jak integrovat nové commity z upstream repozitáře (`PascalCorpsman/fpc_atomic`) do vašeho projektu.

## Automatický způsob (doporučeno)

Použijte připravený script:

```bash
./macos/tools/sync_upstream.command
```

Script vás provede celým procesem a zeptá se na potřeby.

## Manuální způsob

### 1. Zkontrolujte aktuální stav

```bash
git status
```

**Důležité**: Před synchronizací commitněte nebo uložte všechny lokální změny!

### 2. Uložte lokální změny (pokud jsou)

Pokud máte necommitnuté změny, které chcete dočasně uložit:

```bash
git stash
```

Nebo je commitněte:

```bash
git add .
git commit -m "Popis změn"
```

### 3. Stáhněte změny z upstream

```bash
git fetch upstream
```

Tento příkaz stáhne všechny nové změny z upstream, ale ještě je neintegruje.

### 4. Zobrazte nové commity (volitelné)

```bash
git log HEAD..upstream/main --oneline
```

Toto zobrazí seznam nových commitů, které ještě nemáte.

### 5. Integrujte změny

Máte dvě možnosti:

#### A) Merge (doporučeno pro začátečníky)

```bash
git merge upstream/main
```

- Vytvoří merge commit
- Zachová historii
- Snadnější řešení konfliktů

#### B) Rebase (pro pokročilé)

```bash
git rebase upstream/main
```

- Přepíše historii
- Čistší historie (bez merge commitů)
- Složitější řešení konfliktů

### 6. Vyřešte konflikty (pokud nastanou)

Pokud dojde ke konfliktům:

1. **Otevřete soubory s konflikty** (Git je označí)
2. **Vyřešte konflikty** - najdete značky `<<<<<<<`, `=======`, `>>>>>>>`
3. **Odeberte značky** a nechte správný kód
4. **Označte soubory jako vyřešené**:
   ```bash
   git add <soubor>
   ```
5. **Dokončete merge/rebase**:
   - Pro merge: `git commit`
   - Pro rebase: `git rebase --continue`

### 7. Pushněte změny

```bash
git push origin <vaše-branch>
```

Pokud jste použili rebase, možná budete muset použít force push:

```bash
git push origin <vaše-branch> --force-with-lease
```

⚠️ **Pozor**: Force push přepíše historii na remote. Používejte opatrně!

## Tipy

### Zobrazit rozdíly před merge

```bash
git diff HEAD upstream/main
```

### Zrušit merge/rebase

Pokud se něco pokazí:

```bash
# Pro merge
git merge --abort

# Pro rebase
git rebase --abort
```

### Obnovit změny ze stash

Pokud jste použili `git stash`:

```bash
git stash pop
```

## Příklad kompletního workflow

```bash
# 1. Uložit lokální změny
git stash

# 2. Stáhnout změny
git fetch upstream

# 3. Zobrazit nové commity
git log HEAD..upstream/main --oneline

# 4. Merge
git merge upstream/main

# 5. Obnovit lokální změny
git stash pop

# 6. Push
git push origin macos-port
```

## Řešení problémů

### "upstream remote není nastaven"

```bash
git remote add upstream https://github.com/PascalCorpsman/fpc_atomic.git
```

### "Your branch is ahead of 'origin/macos-port'"

To je v pořádku - znamená to, že máte lokální commity, které ještě nejsou na remote. Pushněte je:

```bash
git push origin macos-port
```

### "Cannot rebase: You have unstaged changes"

Commitněte nebo stashněte změny před rebase:

```bash
git stash
# nebo
git add . && git commit -m "WIP"
```

