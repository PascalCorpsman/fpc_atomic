#!/bin/zsh
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
ARCH="$(uname -m)"
case "$ARCH" in
  arm64|aarch64)
    BIN_PATH="${PROJECT_ROOT}/bin/arm64/atomic_launcher"
    LIB_PATH="${PROJECT_ROOT}/lib/arm64"
    ;;
  x86_64)
    BIN_PATH="${PROJECT_ROOT}/bin/x86_64/atomic_launcher"
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
cd "${PROJECT_ROOT}" || exit 1
exec "$BIN_PATH" "$@"
