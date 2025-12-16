#!/bin/zsh
set -euo pipefail

# Build script for FPC Atomic Windows
# This script attempts to cross-compile for Windows from macOS

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

LAZARUS_DIR="${LAZARUS_DIR:-/Applications/Lazarus_3.9}"

echo "========================================="
echo "Building FPC Atomic for Windows"
echo "========================================="
echo ""

# Check for Windows cross-compiler
WINDOWS_COMPILER=""
if command -v ppcrossx64 >/dev/null 2>&1; then
  WINDOWS_COMPILER="ppcrossx64"
  echo "✓ Found Windows cross-compiler: ppcrossx64"
elif [ -f "/usr/local/bin/ppcrossx64" ]; then
  WINDOWS_COMPILER="/usr/local/bin/ppcrossx64"
  echo "✓ Found Windows cross-compiler: /usr/local/bin/ppcrossx64"
elif [ -f "/usr/local/lib/fpc/3.2.2/bin/ppcrossx64" ]; then
  WINDOWS_COMPILER="/usr/local/lib/fpc/3.2.2/bin/ppcrossx64"
  echo "✓ Found Windows cross-compiler: /usr/local/lib/fpc/3.2.2/bin/ppcrossx64"
else
  echo "✗ Windows cross-compiler (ppcrossx64) not found!"
  echo ""
  echo "To build for Windows, you need to install the Windows cross-compiler."
  echo ""
  echo "Options:"
  echo "1. Download from Free Pascal website:"
  echo "   https://www.freepascal.org/download.html"
  echo "   Look for 'cross-compilers' section"
  echo ""
  echo "2. Or use MacPorts (if installed):"
  echo "   sudo port install fpc-cross-x86_64-win64"
  echo ""
  echo "3. Or download pre-built cross-compiler:"
  echo "   https://sourceforge.net/projects/freepascal/files/Mac%20OS%20X/"
  echo ""
  exit 1
fi

# Check Lazarus
if [ ! -d "${LAZARUS_DIR}" ]; then
  echo "✗ Lazarus directory not found: ${LAZARUS_DIR}"
  echo "Set LAZARUS_DIR environment variable to point to your Lazarus installation"
  exit 1
fi

echo "Using Lazarus directory: ${LAZARUS_DIR}"
echo ""

# Create output directory
WINDOWS_BIN_DIR="${PROJECT_ROOT}/windows/bin"
mkdir -p "${WINDOWS_BIN_DIR}"

# Build client
echo "Building client (fpc_atomic)..."
cd "${PROJECT_ROOT}/client"
if lazbuild \
  --lazarusdir="${LAZARUS_DIR}" \
  --build-mode="default" \
  --os=win64 \
  --cpu=x86_64 \
  fpc_atomic.lpi 2>&1 | tee "${WINDOWS_BIN_DIR}/client_build.log"; then
  if [ -f "${PROJECT_ROOT}/fpc_atomic.exe" ]; then
    mv "${PROJECT_ROOT}/fpc_atomic.exe" "${WINDOWS_BIN_DIR}/fpc_atomic.exe"
    echo "✓ Client built successfully: ${WINDOWS_BIN_DIR}/fpc_atomic.exe"
  else
    echo "✗ Client executable not found after build"
    exit 1
  fi
else
  echo "✗ Failed to build client"
  exit 1
fi

# Build launcher
echo ""
echo "Building launcher (atomic_launcher)..."
cd "${PROJECT_ROOT}/launcher"
if lazbuild \
  --lazarusdir="${LAZARUS_DIR}" \
  --build-mode="default" \
  --os=win64 \
  --cpu=x86_64 \
  atomic_launcher.lpi 2>&1 | tee "${WINDOWS_BIN_DIR}/launcher_build.log"; then
  if [ -f "${PROJECT_ROOT}/atomic_launcher.exe" ]; then
    mv "${PROJECT_ROOT}/atomic_launcher.exe" "${WINDOWS_BIN_DIR}/atomic_launcher.exe"
    echo "✓ Launcher built successfully: ${WINDOWS_BIN_DIR}/atomic_launcher.exe"
  else
    echo "✗ Launcher executable not found after build"
    exit 1
  fi
else
  echo "✗ Failed to build launcher"
  exit 1
fi

# Build server
echo ""
echo "Building server (atomic_server)..."
cd "${PROJECT_ROOT}/server"
if lazbuild \
  --lazarusdir="${LAZARUS_DIR}" \
  --build-mode="default" \
  --os=win64 \
  --cpu=x86_64 \
  atomic_server.lpi 2>&1 | tee "${WINDOWS_BIN_DIR}/server_build.log"; then
  if [ -f "${PROJECT_ROOT}/atomic_server.exe" ]; then
    mv "${PROJECT_ROOT}/atomic_server.exe" "${WINDOWS_BIN_DIR}/atomic_server.exe"
    echo "✓ Server built successfully: ${WINDOWS_BIN_DIR}/atomic_server.exe"
  else
    echo "✗ Server executable not found after build"
    exit 1
  fi
else
  echo "✗ Failed to build server"
  exit 1
fi

# Build CD data extractor
echo ""
echo "Building CD data extractor (cd_data_extractor)..."
cd "${PROJECT_ROOT}/cd_data_extractor_src"
if lazbuild \
  --lazarusdir="${LAZARUS_DIR}" \
  --build-mode="default" \
  --os=win64 \
  --cpu=x86_64 \
  cd_data_extractor.lpi 2>&1 | tee "${WINDOWS_BIN_DIR}/extractor_build.log"; then
  if [ -f "${PROJECT_ROOT}/cd_data_extractor.exe" ]; then
    mv "${PROJECT_ROOT}/cd_data_extractor.exe" "${WINDOWS_BIN_DIR}/cd_data_extractor.exe"
    echo "✓ CD data extractor built successfully: ${WINDOWS_BIN_DIR}/cd_data_extractor.exe"
  else
    echo "✗ CD data extractor executable not found after build"
    exit 1
  fi
else
  echo "✗ Failed to build CD data extractor"
  exit 1
fi

echo ""
echo "========================================="
echo "Build complete!"
echo "========================================="
echo "Windows executables are in: ${WINDOWS_BIN_DIR}"
echo ""

