#!/bin/zsh
set -euo pipefail

# Build script for FPC Atomic macOS
# This script compiles all applications and creates app bundles

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Allow architecture to be specified as first argument
if [[ $# -gt 0 ]]; then
  REQUESTED_ARCH="$1"
  case "${REQUESTED_ARCH}" in
    arm64|aarch64)
      TARGET_ARCH="arm64"
      BUILD_MODE="macos_arm64"
      ;;
    x86_64|intel)
      TARGET_ARCH="x86_64"
      BUILD_MODE="macos_x86_64"
      ;;
    *)
      echo "Unsupported architecture: ${REQUESTED_ARCH}" >&2
      echo "Usage: $0 [arm64|x86_64]" >&2
      exit 1
      ;;
  esac
else
  # Detect architecture automatically
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
fi

BIN_DIR="${PROJECT_ROOT}/bin/macos/${TARGET_ARCH}"

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

# Set compiler for cross-compilation if needed
if [[ "${TARGET_ARCH}" == "x86_64" && "$(uname -m)" == "arm64" ]]; then
  # Try to find x86_64 compiler
  X86_64_COMPILER=""
  if command -v ppcx64 &> /dev/null; then
    X86_64_COMPILER="$(which ppcx64)"
  elif [[ -f "/usr/local/bin/ppcx64" ]]; then
    X86_64_COMPILER="/usr/local/bin/ppcx64"
  fi
  
  if [[ -n "${X86_64_COMPILER}" ]]; then
    export PP="${X86_64_COMPILER}"
    LAZBUILD_ARGS="${LAZBUILD_ARGS} --compiler=${X86_64_COMPILER}"
    echo "Using x86_64 compiler: ${X86_64_COMPILER}"
  else
    echo "Warning: x86_64 compiler (ppcx64) not found, lazbuild may use wrong compiler" >&2
  fi
fi

# Build applications - use eval to properly handle arguments with spaces
EVAL_CMD="lazbuild ${LAZBUILD_ARGS} --build-mode=\"${BUILD_MODE}\""

# Build applications
echo "Building client (fpc_atomic)..."
eval lazbuild ${LAZBUILD_ARGS} --build-mode=\"${BUILD_MODE}\" \"${PROJECT_ROOT}/client/fpc_atomic.lpi\" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Error: Failed to build client" >&2
  exit 1
}

echo "Building launcher (atomic_launcher)..."
eval lazbuild ${LAZBUILD_ARGS} --build-mode=\"${BUILD_MODE}\" \"${PROJECT_ROOT}/launcher/atomic_launcher.lpi\" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Error: Failed to build launcher" >&2
  exit 1
}

echo "Building server (atomic_server)..."
eval lazbuild ${LAZBUILD_ARGS} --build-mode=\"${BUILD_MODE}\" \"${PROJECT_ROOT}/server/atomic_server.lpi\" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Error: Failed to build server" >&2
  exit 1
}

echo "Building AI library (ai)..."
eval lazbuild ${LAZBUILD_ARGS} --build-mode=\"${BUILD_MODE}\" \"${PROJECT_ROOT}/ai/ai.lpi\" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Warning: Failed to build AI library (may not be critical)" >&2
}

echo "Building CD Data Extractor GUI (cd_data_extractor)..."
eval lazbuild ${LAZBUILD_ARGS} --build-mode=\"${BUILD_MODE}\" \"${PROJECT_ROOT}/cd_data_extractor_src/cd_data_extractor.lpi\" 2>&1 | grep -v "^Note:" | grep -v "^Hint:" || {
  echo "Warning: Failed to build CD Data Extractor GUI (may not be critical)" >&2
}

echo ""
echo "========================================="
echo "Checking data directory..."
echo "========================================="
echo ""

# Always copy data directory from project root to bin directory
DATA_SOURCE="${PROJECT_ROOT}/data"
DATA_TARGET="${BIN_DIR}/data"

if [[ -d "${DATA_SOURCE}" ]]; then
  echo "Copying data directory from project root to ${BIN_DIR}..."
  # Use rsync to sync data directory (preserves existing files, adds new ones)
  mkdir -p "${DATA_TARGET}"
  rsync -a "${DATA_SOURCE}/" "${DATA_TARGET}/"
  echo "  ✓ Synchronized data directory to ${DATA_TARGET}"
else
  echo "  ⚠ Warning: Data directory not found at ${DATA_SOURCE}" >&2
  if [[ ! -d "${DATA_TARGET}" ]]; then
    echo "  ⚠ Warning: Data directory not found at ${DATA_TARGET}" >&2
  fi
fi

echo ""
echo "========================================="
echo "Creating app bundles..."
echo "========================================="
echo ""

# Build app bundles
"${SCRIPT_DIR}/build_app_bundles.command" "${TARGET_ARCH}"

echo ""
echo "========================================="
echo "Build complete!"
echo "========================================="
echo "App bundles are in: ${PROJECT_ROOT}/macos/app_${TARGET_ARCH}/"
echo "  - FPCAtomic.app"
echo "  - FPCAtomicLauncher.app"
echo "  - FPCAtomicServer.app"
echo ""

