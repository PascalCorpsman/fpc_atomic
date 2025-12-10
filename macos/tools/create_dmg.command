#!/bin/zsh
set -euo pipefail

# Script to create DMG files for macOS distribution using create-dmg
# Creates a DMG with the app bundle and installation instructions

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Allow architecture to be specified as first argument
if [[ $# -gt 0 ]]; then
  REQUESTED_ARCH="$1"
  case "${REQUESTED_ARCH}" in
    arm64|aarch64)
      TARGET_ARCH="arm64"
      ;;
    x86_64|intel)
      TARGET_ARCH="x86_64"
      ;;
    universal)
      TARGET_ARCH="universal"
      ;;
    *)
      echo "Unsupported architecture: ${REQUESTED_ARCH}" >&2
      echo "Usage: $0 [arm64|x86_64|universal]" >&2
      exit 1
      ;;
  esac
else
  # Detect architecture automatically
  ARCH="$(uname -m)"
  case "${ARCH}" in
    arm64|aarch64)
      TARGET_ARCH="arm64"
      ;;
    x86_64)
      TARGET_ARCH="x86_64"
      ;;
    *)
      echo "Unsupported architecture: ${ARCH}" >&2
      exit 1
      ;;
  esac
fi

APP_ROOT="${PROJECT_ROOT}/macos/app_${TARGET_ARCH}"
DMG_OUTPUT_DIR="${PROJECT_ROOT}/releases"
DMG_NAME="FPCAtomic-${TARGET_ARCH}"
DMG_PATH="${DMG_OUTPUT_DIR}/${DMG_NAME}.dmg"

# Get version if available
VERSION="${VERSION:-}"
if [[ -z "${VERSION}" ]] && [[ -f "${PROJECT_ROOT}/bin/fpc_atomic.version" ]]; then
  VERSION=$(grep -E "^version=" "${PROJECT_ROOT}/bin/fpc_atomic.version" | cut -d'=' -f2 | tr -d '"' || echo "")
fi

if [[ -n "${VERSION}" ]]; then
  DMG_NAME="FPCAtomic-${VERSION}-${TARGET_ARCH}"
  DMG_PATH="${DMG_OUTPUT_DIR}/${DMG_NAME}.dmg"
else
  DMG_NAME="FPCAtomic-${TARGET_ARCH}"
  DMG_PATH="${DMG_OUTPUT_DIR}/${DMG_NAME}.dmg"
fi

# Check if app bundles exist
if [[ ! -d "${APP_ROOT}/FPCAtomic.app" ]]; then
  echo "Error: FPCAtomic.app not found at ${APP_ROOT}/FPCAtomic.app" >&2
  echo "Please build app bundles first using build_app_bundles.command" >&2
  exit 1
fi

# Check if create-dmg is installed
if ! command -v create-dmg &> /dev/null; then
  echo "Error: create-dmg is not installed" >&2
  echo "Please install it using:" >&2
  echo "  brew install create-dmg" >&2
  echo "Or download from: https://github.com/create-dmg/create-dmg" >&2
  exit 1
fi

# Check if data directory exists
DATA_DIR="${PROJECT_ROOT}/data"
if [[ ! -d "${DATA_DIR}" ]]; then
  echo "Warning: Data directory not found at ${DATA_DIR}" >&2
  echo "The DMG will be created without the data directory." >&2
  echo "Users will need to run the CD data extractor after installation." >&2
fi

# Create output directory
mkdir -p "${DMG_OUTPUT_DIR}"

# Create temporary source directory for DMG
DMG_SOURCE_DIR=$(mktemp -d -t fpc_atomic_dmg_source.XXXXXX)
trap "rm -rf '${DMG_SOURCE_DIR}'" EXIT

echo "========================================="
echo "Creating DMG for ${TARGET_ARCH}"
echo "========================================="

# Create FPCAtomic directory in source
FPCATOMIC_DIR="${DMG_SOURCE_DIR}/FPCAtomic"
mkdir -p "${FPCATOMIC_DIR}"

# Copy app bundles to FPCAtomic directory
echo "Copying app bundles..."
cp -R "${APP_ROOT}/FPCAtomic.app" "${FPCATOMIC_DIR}/"
cp -R "${APP_ROOT}/FPCAtomicLauncher.app" "${FPCATOMIC_DIR}/"
cp -R "${APP_ROOT}/FPCAtomicServer.app" "${FPCATOMIC_DIR}/"

# Copy remove_quarantine.command if it exists
if [[ -f "${APP_ROOT}/remove_quarantine.command" ]]; then
  echo "Copying remove_quarantine.command..."
  cp "${APP_ROOT}/remove_quarantine.command" "${FPCATOMIC_DIR}/"
  chmod +x "${FPCATOMIC_DIR}/remove_quarantine.command"
fi

# Copy data directory if it exists
if [[ -d "${DATA_DIR}" ]]; then
  echo "Copying data directory..."
  cp -R "${DATA_DIR}" "${FPCATOMIC_DIR}/"
fi

# Create README with installation instructions
echo "Creating README..."
cat > "${FPCATOMIC_DIR}/README.txt" << 'EOF'
FPC Atomic - Installation Instructions
======================================

To install FPC Atomic:

1. Drag the FPCAtomic folder to your Applications folder (or any location you prefer).

2. Open the FPCAtomic folder and double-click FPCAtomicLauncher.app to start the launcher.

3. If the data directory is included, the game is ready to play!
   If not, you will need to:
   - Click "Run CD data extractor"
   - Set the path to your Atomic Bomberman CD data
   - Click "Start extraction" to extract game assets

4. If you copied the apps via AirDrop or downloaded them, you may need to:
   - Double-click "remove_quarantine.command" to remove macOS quarantine
   - This allows the apps to run without security warnings

5. After extraction, you can launch the game from the launcher.

System Requirements:
- macOS 11.0 (Big Sur) or later
- For arm64 version: Apple Silicon Mac (M1/M2/M3 or later)
- For x86_64 version: Intel Mac
- For universal version: Any Mac (automatically uses the correct architecture)

For more information, visit:
https://github.com/PascalCorpsman/fpc_atomic

Enjoy the game!
EOF

# Prepare background image for DMG
BACKGROUND_IMAGE="${PROJECT_ROOT}/macos/assets/dmg_background.png"

# Remove existing DMG if it exists
if [[ -f "${DMG_PATH}" ]]; then
  rm -f "${DMG_PATH}"
fi

echo "Creating DMG file using create-dmg..."

# Build create-dmg command
CREATE_DMG_CMD=(
  create-dmg
  --volname "FPC Atomic ${TARGET_ARCH}"
  --window-pos 400 100
  --window-size 600 400
  --icon-size 96
  --text-size 14
  --app-drop-link 480 180
)

# Add background image if it exists
if [[ -f "${BACKGROUND_IMAGE}" ]]; then
  echo "Using custom background image: ${BACKGROUND_IMAGE}"
  CREATE_DMG_CMD+=(--background "${BACKGROUND_IMAGE}")
else
  echo "Note: Custom background not found at ${BACKGROUND_IMAGE}"
  echo "  You can add a custom background image with an arrow at that location"
fi

# Add icon positions
CREATE_DMG_CMD+=(
  --icon "FPCAtomic" 120 180
  --hide-extension "FPCAtomic"
)

# Add final DMG path and source directory
CREATE_DMG_CMD+=(
  "${DMG_PATH}"
  "${DMG_SOURCE_DIR}"
)

# Execute create-dmg
"${CREATE_DMG_CMD[@]}" || {
  echo "Error: Failed to create DMG" >&2
  exit 1
}

# Verify DMG was created
if [[ ! -f "${DMG_PATH}" ]]; then
  echo "Error: DMG was not created" >&2
  exit 1
fi

# Get DMG size for display
DMG_SIZE_MB=$(du -m "${DMG_PATH}" | cut -f1)

echo "========================================="
echo "DMG created successfully!"
echo "========================================="
echo "File: ${DMG_PATH}"
echo "Size: ${DMG_SIZE_MB}MB"
echo ""
echo "DMG files are saved in: ${DMG_OUTPUT_DIR}"
echo "========================================="
