#!/bin/zsh
set -euo pipefail

# Script pro synchronizaci s upstream repozitářem
# Tento script stáhne nové změny z upstream a integruje je do aktuální větve

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

cd "${PROJECT_ROOT}"

echo "========================================="
echo "Synchronizace s upstream repozitářem"
echo "========================================="
echo ""

# Zkontrolovat, že upstream je nastaven
if ! git remote | grep -q "^upstream$"; then
  echo "✗ Upstream remote není nastaven!"
  echo ""
  echo "Nastav upstream pomocí:"
  echo "  git remote add upstream https://github.com/PascalCorpsman/fpc_atomic.git"
  exit 1
fi

# Zkontrolovat, že nejsou necommitnuté změny
if ! git diff-index --quiet HEAD --; then
  echo "⚠ Máte necommitnuté změny!"
  echo ""
  echo "Možnosti:"
  echo "1. Commitnout změny:"
  echo "   git add ."
  echo "   git commit -m 'Your commit message'"
  echo ""
  echo "2. Uložit změny do stash:"
  echo "   git stash"
  echo ""
  read -q "response?Chcete pokračovat? (y/N) "
  echo ""
  if [[ "${response}" != "y" && "${response}" != "Y" ]]; then
    echo "Zrušeno."
    exit 0
  fi
fi

# Zjistit aktuální branch
CURRENT_BRANCH=$(git branch --show-current)
echo "Aktuální branch: ${CURRENT_BRANCH}"
echo ""

# 1. Fetchnout změny z upstream
echo "1. Stahování změn z upstream..."
git fetch upstream
echo "✓ Změny staženy"
echo ""

# 2. Zobrazit nové commity
echo "2. Nové commity v upstream/main:"
git log ${CURRENT_BRANCH}..upstream/main --oneline | head -10
if [ $? -ne 0 ]; then
  echo "   (žádné nové commity nebo upstream/main neexistuje)"
fi
echo ""

# 3. Zeptat se, jestli merge nebo rebase
echo "Jak chcete integrovat změny?"
echo "1. Merge - vytvoří merge commit (doporučeno pro začátečníky)"
echo "2. Rebase - přepíše historii (čistší, ale složitější)"
echo ""
read -q "response?Zvolte merge (1) nebo rebase (2): "
echo ""

if [[ "${response}" == "2" ]]; then
  echo ""
  echo "3. Rebase upstream/main do ${CURRENT_BRANCH}..."
  git rebase upstream/main
  echo "✓ Rebase dokončen"
else
  echo ""
  echo "3. Merge upstream/main do ${CURRENT_BRANCH}..."
  git merge upstream/main --no-edit
  echo "✓ Merge dokončen"
fi

echo ""
echo "========================================="
echo "Synchronizace dokončena!"
echo "========================================="
echo ""
echo "Pokud došlo ke konfliktům:"
echo "1. Vyřešte konflikty v souborech"
echo "2. git add <soubory>"
echo "3. git commit (pro merge) nebo git rebase --continue (pro rebase)"
echo ""
echo "Pro push změn:"
echo "  git push origin ${CURRENT_BRANCH}"
echo ""

