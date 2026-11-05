#!/bin/sh
# Builds the smart ai and copies it next to the original ai, to try it in the game.
# Then choose it in the launcher: Game options, AI = Smart.
#
#   ./try_ai.sh                    build and copy into the game folder (default: the repository root)
#   ./try_ai.sh --gamedir <dir>    another game folder (where atomic_server is)
#   ./try_ai.sh --restore          only for older versions of this script: put the original libai.so back
#                                  (they copied the smart ai over libai.so and saved it as libai_original.so)
#
# lazbuild is taken from the PATH, or from $LAZBUILD.
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
TARGET="$GAMEDIR/libai_smart.so"

if [ "$RESTORE" = 1 ]; then
  [ -f "$BACKUP" ] || { echo "No backup found: $BACKUP"; exit 1; }
  cp -f "$BACKUP" "$AI"
  rm -f "$BACKUP"
  echo "Original ai restored: $AI"
  exit 0
fi

echo "Building ai_smart ..."
"$LAZBUILD" -q "$SCRIPT_DIR/ai_smart.lpi"
BUILT="$ROOT/libai_smart.so"
[ -f "$BUILT" ] || { echo "Not found after the build: $BUILT"; exit 1; }
[ "$BUILT" = "$TARGET" ] || cp -f "$BUILT" "$TARGET"
echo "Smart ai: $TARGET"
echo "Choose it in the launcher: Game options, AI = Smart (the server log shows 'Atomic smart ai')."
if [ -f "$BACKUP" ]; then
  echo "Warning: libai.so was replaced by an older version of this script, put the original back with: ./try_ai.sh --gamedir \"$GAMEDIR\" --restore"
fi
