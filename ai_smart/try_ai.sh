#!/bin/sh
# Builds the smart ai and puts it in place of the original ai, to try it in the game.
#
#   ./try_ai.sh                    build and install into the game folder (default: the repository root)
#   ./try_ai.sh --gamedir <dir>    another game folder (where atomic_server is)
#   ./try_ai.sh --restore          put the original libai.so back
#
# The original libai.so is saved once as libai_original.so (an existing copy is never overwritten).
# lazbuild is taken from the PATH, or from $LAZBUILD. The settings are in ai_smart.ini, see README.md.
set -e

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
ROOT=$(dirname "$SCRIPT_DIR")
GAMEDIR=$ROOT
RESTORE=0
LAZBUILD=${LAZBUILD:-lazbuild}

while [ $# -gt 0 ]; do
  case "$1" in
    --gamedir) GAMEDIR=$2; shift 2 ;;
    --restore) RESTORE=1; shift ;;
    *) echo "Unknown option: $1"; exit 2 ;;
  esac
done

AI="$GAMEDIR/libai.so"
BACKUP="$GAMEDIR/libai_original.so"

if pgrep -x atomic_server >/dev/null 2>&1; then
  echo "Warning: atomic_server is running, it keeps the old ai loaded. Unload it there with 'u' and load it again with 'a' after this script."
fi

if [ "$RESTORE" = 1 ]; then
  [ -f "$BACKUP" ] || { echo "No backup found: $BACKUP"; exit 1; }
  cp -f "$BACKUP" "$AI"
  echo "Original ai restored: $AI"
  exit 0
fi

echo "Building ai_smart ..."
"$LAZBUILD" -q "$SCRIPT_DIR/ai_smart.lpi"
BUILT="$ROOT/libai_smart.so"
[ -f "$BUILT" ] || { echo "Not found after the build: $BUILT"; exit 1; }

if [ -f "$AI" ] && [ ! -f "$BACKUP" ]; then
  cp "$AI" "$BACKUP"
  echo "Original ai saved as $BACKUP"
fi
cp -f "$BUILT" "$AI"
echo "Smart ai installed: $AI"
echo "The server log shows 'Atomic smart ai' when it is loaded. Back to the original: ./try_ai.sh --restore"

if [ -f "$GAMEDIR/ai_smart.ini" ]; then
  echo "Settings: $GAMEDIR/ai_smart.ini (next to the server, wins)"
else
  echo "Settings: ${XDG_CONFIG_HOME:-$HOME/.config}/fpc_atomic/ai_smart.ini (written with the defaults on the first start)"
fi
