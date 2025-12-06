#!/bin/zsh
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
ARCH="$(uname -m)"
case "$ARCH" in
  arm64|aarch64)
    BIN_PATH="${PROJECT_ROOT}/bin/arm64/atomic_server"
    LIB_PATH="${PROJECT_ROOT}/lib/arm64"
    ;;
  x86_64)
    BIN_PATH="${PROJECT_ROOT}/bin/x86_64/atomic_server"
    LIB_PATH="${PROJECT_ROOT}/lib/x86_64"
    ;;
  *)
    echo "Unsupported architecture: ${ARCH}" >&2
    exit 1
    ;;
 esac

if [ ! -x "$BIN_PATH" ]; then
  echo "Binary not found or not executable: $BIN_PATH" >&2
  echo "Build the project with lazbuild --build-mode=macos_${ARCH} before running." >&2
  exit 1
fi

export DYLD_LIBRARY_PATH="${LIB_PATH}:${DYLD_LIBRARY_PATH}"

ARGS=("$@")
HAS_PORT=0
HAS_TIMEOUT=0

i=0
while [ $i -lt ${#ARGS[@]} ]; do
  case "${ARGS[$i]}" in
    -p)
      HAS_PORT=1
      i=$((i + 2))
      continue
      ;;
    -t)
      HAS_TIMEOUT=1
      i=$((i + 2))
      continue
      ;;
  esac
  i=$((i + 1))
done

if [ $HAS_PORT -eq 0 ]; then
  ARGS+=("-p" "5521")
fi
if [ $HAS_TIMEOUT -eq 0 ]; then
  ARGS+=("-t" "0")
fi

exec "$BIN_PATH" "${ARGS[@]}"
