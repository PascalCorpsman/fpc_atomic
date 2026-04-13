#!/bin/zsh
set -euo pipefail

# Prepare ARM64 (Apple Silicon) libraries for FPC Atomic.
# Gets libSDL2.dylib from official SDL2 release (no Homebrew required).
# End users never run this – the built .app contains the library.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
LIB_DIR="${PROJECT_ROOT}/lib/macos/arm64"

# SDL2 version and URL (official GitHub release, zlib license – redistribution allowed)
SDL2_VERSION="${SDL2_VERSION:-2.28.5}"
SDL2_DMG_URL="https://github.com/libsdl-org/SDL/releases/download/release-${SDL2_VERSION}/SDL2-${SDL2_VERSION}.dmg"

echo "========================================="
echo "Preparing ARM64 libraries"
echo "========================================="
echo ""

mkdir -p "${LIB_DIR}"
SDL_DEST="${LIB_DIR}/libSDL2.dylib"

if [[ -f "${SDL_DEST}" ]]; then
  if file "${SDL_DEST}" | grep -q "arm64"; then
    echo "  ✓ libSDL2.dylib already present (ARM64)"
    echo ""
    echo "Libraries ready in ${LIB_DIR}"
    exit 0
  fi
fi

# Try Homebrew first (optional, for developers who have it)
for brew_lib in "/opt/homebrew/lib/libSDL2.dylib" "$(brew --prefix 2>/dev/null)/lib/libSDL2.dylib"; do
  if [[ -f "${brew_lib}" ]] && file "${brew_lib}" | grep -q "arm64"; then
    echo "  → Copying libSDL2.dylib from Homebrew"
    cp "${brew_lib}" "${SDL_DEST}"
    install_name_tool -id "@rpath/libSDL2.dylib" "${SDL_DEST}" 2>/dev/null || true
    echo "  ✓ libSDL2.dylib installed"
    echo ""
    echo "Libraries ready in ${LIB_DIR}"
    exit 0
  fi
done

# Download SDL2 from official release (no Homebrew needed)
echo "  → Downloading SDL2 ${SDL2_VERSION} from GitHub..."
DMG="${PROJECT_ROOT}/lib/macos/SDL2-${SDL2_VERSION}.dmg"
mkdir -p "${PROJECT_ROOT}/lib/macos"

if [[ ! -f "${DMG}" ]]; then
  if ! curl -sSfL -o "${DMG}" "${SDL2_DMG_URL}"; then
    echo "  ✗ Download failed. Check network or set SDL2_VERSION to another version." >&2
    rm -f "${DMG}"
    exit 1
  fi
fi

MOUNT_POINT=$(hdiutil attach -nobrowse -readonly -noverify -noautoopen "${DMG}" 2>/dev/null | tail -1 | awk -F'\t' '{print $3}')
if [[ -z "${MOUNT_POINT}" ]] || [[ ! -d "${MOUNT_POINT}" ]]; then
  echo "  ✗ Failed to mount SDL2 DMG" >&2
  exit 1
fi

cleanup() {
  if [[ -n "${MOUNT_POINT}" ]] && [[ -d "${MOUNT_POINT}" ]]; then
    hdiutil detach "${MOUNT_POINT}" -quiet 2>/dev/null || true
  fi
}
trap cleanup EXIT

# DMG contains SDL2.framework; binary is SDL2.framework/SDL2 or Versions/A/SDL2
BINARY=""
for candidate in "${MOUNT_POINT}/SDL2.framework/SDL2" "${MOUNT_POINT}/SDL2.framework/Versions/A/SDL2"; do
  if [[ -f "${candidate}" ]]; then
    BINARY="${candidate}"
    break
  fi
done
if [[ -z "${BINARY}" ]] || [[ ! -f "${BINARY}" ]]; then
  echo "  ✗ SDL2 binary not found in DMG" >&2
  exit 1
fi

# Extract arm64 slice if framework is universal
if lipo -info "${BINARY}" 2>/dev/null | grep -q "arm64"; then
  if lipo -info "${BINARY}" 2>/dev/null | grep -q "Non-fat"; then
    cp "${BINARY}" "${SDL_DEST}"
  else
    lipo -extract arm64 "${BINARY}" -output "${SDL_DEST}"
  fi
else
  cp "${BINARY}" "${SDL_DEST}"
fi

install_name_tool -id "@rpath/libSDL2.dylib" "${SDL_DEST}" 2>/dev/null || true
echo "  ✓ libSDL2.dylib installed from SDL2 ${SDL2_VERSION}"

if ! file "${SDL_DEST}" | grep -q "arm64"; then
  echo "  ⚠ Warning: library may not be ARM64; use on Apple Silicon only if it matches." >&2
fi

echo ""
echo "Libraries ready in ${LIB_DIR}"
echo "You can now run build_all.command or build_app_bundles.command"
echo ""
