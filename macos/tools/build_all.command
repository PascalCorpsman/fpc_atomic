#!/bin/zsh
set -euo pipefail

# Build script for FPC Atomic macOS
# This script compiles all applications and creates app bundles

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Detect architecture
ARCH="$(uname -m)"
case "${ARCH}" in
  arm64|aarch64)
    TARGET_ARCH="arm64"
    BUILD_MODE="macos_arm64"
    ;;
  x86_64)
    TARGET_ARCH="x86_64"
    BUILD_MODE="macos_x86_64"
    ;;
  *)
    echo "Unsupported architecture: ${ARCH}" >&2
    exit 1
    ;;
esac

BIN_DIR="${PROJECT_ROOT}/macos/bin/${TARGET_ARCH}"

echo "========================================="
echo "Building FPC Atomic for macOS (${TARGET_ARCH})"
echo "========================================="
echo ""

# Check if lazbuild is available
if ! command -v lazbuild &> /dev/null; then
  echo "Error: lazbuild not found in PATH" >&2
  echo "Please install Lazarus IDE or add lazbuild to PATH" >&2
  exit 1
fi

# Create bin directory if it doesn't exist
mkdir -p "${BIN_DIR}"

# Try to find Lazarus directory
LAZARUS_DIR=""
if [[ -d "/Applications/Lazarus.app" ]]; then
  LAZARUS_DIR="/Applications/Lazarus.app/Contents/MacOS"
elif [[ -d "/Applications/Lazarus_3.9" ]]; then
  LAZARUS_DIR="/Applications/Lazarus_3.9"
elif [[ -d "/usr/local/share/lazarus" ]]; then
  LAZARUS_DIR="/usr/local/share/lazarus"
fi

LAZBUILD_ARGS=""
if [[ -n "${LAZARUS_DIR}" ]]; then
  LAZBUILD_ARGS="--lazarusdir=${LAZARUS_DIR}"
  echo "Using Lazarus directory: ${LAZARUS_DIR}"
fi

# Build applications
echo "Building client (fpc_atomic)..."
lazbuild ${LAZBUILD_ARGS} --build-mode="${BUILD_MODE}" "${PROJECT_ROOT}/client/fpc_atomic.lpi" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Error: Failed to build client" >&2
  exit 1
}

echo "Building launcher (atomic_launcher)..."
lazbuild ${LAZBUILD_ARGS} --build-mode="${BUILD_MODE}" "${PROJECT_ROOT}/launcher/atomic_launcher.lpi" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Error: Failed to build launcher" >&2
  exit 1
}

echo "Building server (atomic_server)..."
lazbuild ${LAZBUILD_ARGS} --build-mode="${BUILD_MODE}" "${PROJECT_ROOT}/server/atomic_server.lpi" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Error: Failed to build server" >&2
  exit 1
}

echo "Building AI library (ai)..."
lazbuild ${LAZBUILD_ARGS} --build-mode="${BUILD_MODE}" "${PROJECT_ROOT}/ai/ai.lpi" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Warning: Failed to build AI library (may not be critical)" >&2
}

echo ""
echo "========================================="
echo "Creating app bundles..."
echo "========================================="
echo ""

# Build app bundles
"${SCRIPT_DIR}/build_app_bundles.command"

echo ""
echo "========================================="
echo "Build complete!"
echo "========================================="
echo "App bundles are in: ${PROJECT_ROOT}/macos/app/"
echo "  - FPCAtomic.app"
echo "  - FPCAtomicLauncher.app"
echo "  - FPCAtomicServer.app"
echo ""

