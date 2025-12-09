#!/bin/zsh
set -euo pipefail

# Build script for FPC Atomic macOS Universal Binary
# This script compiles both architectures (arm64 and x86_64) and creates universal binaries

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

echo "========================================="
echo "Building FPC Atomic for macOS (Universal)"
echo "========================================="
echo ""

# Check if lazbuild is available
if ! command -v lazbuild &> /dev/null; then
  echo "Error: lazbuild not found in PATH" >&2
  echo "Please install Lazarus IDE or add lazbuild to PATH" >&2
  exit 1
fi

# Check if lipo is available
if ! command -v lipo &> /dev/null; then
  echo "Error: lipo not found in PATH" >&2
  echo "Please install Xcode Command Line Tools" >&2
  exit 1
fi

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

# Note: x86_64 build will be handled by build_all_x86_64.command
# which properly handles Rosetta and library preparation

# Directories
BIN_DIR_ARM64="${PROJECT_ROOT}/bin/macos/arm64"
BIN_DIR_X86_64="${PROJECT_ROOT}/bin/macos/x86_64"
BIN_DIR_UNIVERSAL="${PROJECT_ROOT}/bin/macos/universal"
LIB_DIR_ARM64="${PROJECT_ROOT}/lib/macos/arm64"
LIB_DIR_X86_64="${PROJECT_ROOT}/lib/macos/x86_64"
LIB_DIR_UNIVERSAL="${PROJECT_ROOT}/lib/macos/universal"

# Create directories
mkdir -p "${BIN_DIR_UNIVERSAL}" "${LIB_DIR_UNIVERSAL}"

# Function to create universal binary
create_universal_binary() {
  local arm64_file="$1"
  local x86_64_file="$2"
  local output_file="$3"
  
  if [[ ! -f "${arm64_file}" ]]; then
    echo "Error: ARM64 binary not found: ${arm64_file}" >&2
    return 1
  fi
  
  if [[ ! -f "${x86_64_file}" ]]; then
    echo "Error: x86_64 binary not found: ${x86_64_file}" >&2
    return 1
  fi
  
  echo "  Creating universal binary: $(basename "${output_file}")"
  lipo -create "${arm64_file}" "${x86_64_file}" -output "${output_file}"
  
  # Verify the universal binary
  if lipo -info "${output_file}" | grep -q "arm64.*x86_64\|x86_64.*arm64"; then
    echo "    ✓ Universal binary created successfully"
  else
    echo "    ⚠ Warning: Universal binary verification failed" >&2
  fi
}

# Step 1: Build ARM64
echo "========================================="
echo "Step 1: Building ARM64 architecture..."
echo "========================================="
echo ""

"${SCRIPT_DIR}/build_all.command" arm64 || {
  echo "Error: Failed to build ARM64 architecture" >&2
  exit 1
}

# Step 2: Build x86_64
echo ""
echo "========================================="
echo "Step 2: Building x86_64 architecture..."
echo "========================================="
echo ""

# Step 2a: Prepare x86_64 libraries (same as build_all_x86_64.command)
if [[ "${SKIP_LIB_CHECK:-0}" != "1" ]]; then
  echo "Step 2a: Preparing x86_64 libraries..."
  "${SCRIPT_DIR}/prepare_x86_64_libs.command" || {
    echo "Warning: Library check failed, but continuing build..." >&2
  }
  echo ""
fi

# Step 2b: Build x86_64 (use build_all_x86_64.command which handles Rosetta properly)
"${SCRIPT_DIR}/build_all_x86_64.command" || {
  echo "Error: Failed to build x86_64 architecture" >&2
  exit 1
}

# Step 3: Create universal binaries
echo ""
echo "========================================="
echo "Step 3: Creating universal binaries..."
echo "========================================="
echo ""

# Binaries to merge
BINARIES=(
  "fpc_atomic"
  "atomic_launcher"
  "atomic_server"
  "cd_data_extractor"
)

for binary in "${BINARIES[@]}"; do
  if [[ -f "${BIN_DIR_ARM64}/${binary}" ]] && [[ -f "${BIN_DIR_X86_64}/${binary}" ]]; then
    create_universal_binary \
      "${BIN_DIR_ARM64}/${binary}" \
      "${BIN_DIR_X86_64}/${binary}" \
      "${BIN_DIR_UNIVERSAL}/${binary}"
  else
    echo "  ⚠ Warning: ${binary} not found for both architectures, skipping" >&2
  fi
done

# Step 4: Create universal libraries
echo ""
echo "========================================="
echo "Step 4: Creating universal libraries..."
echo "========================================="
echo ""

# Libraries to merge (only those that are not already universal)
LIBRARIES=(
  "libai.dylib"
  "libSDL2.dylib"
)

for lib in "${LIBRARIES[@]}"; do
  if [[ -f "${LIB_DIR_ARM64}/${lib}" ]] && [[ -f "${LIB_DIR_X86_64}/${lib}" ]]; then
    # Check if library is already universal
    if lipo -info "${LIB_DIR_ARM64}/${lib}" 2>/dev/null | grep -q "arm64.*x86_64\|x86_64.*arm64"; then
      echo "  ${lib} is already universal, copying from ARM64"
      cp "${LIB_DIR_ARM64}/${lib}" "${LIB_DIR_UNIVERSAL}/${lib}"
    else
      create_universal_binary \
        "${LIB_DIR_ARM64}/${lib}" \
        "${LIB_DIR_X86_64}/${lib}" \
        "${LIB_DIR_UNIVERSAL}/${lib}"
    fi
  else
    echo "  ⚠ Warning: ${lib} not found for both architectures, skipping" >&2
  fi
done

# Copy already universal libraries (libbass.dylib, etc.)
echo "  Copying already universal libraries..."
for lib in "${LIB_DIR_ARM64}"/*.dylib; do
  lib_name=$(basename "${lib}")
  if [[ ! -f "${LIB_DIR_UNIVERSAL}/${lib_name}" ]]; then
    # Check if it's already universal
    if lipo -info "${lib}" 2>/dev/null | grep -q "arm64.*x86_64\|x86_64.*arm64"; then
      echo "    Copying universal library: ${lib_name}"
      cp "${lib}" "${LIB_DIR_UNIVERSAL}/${lib_name}"
    fi
  fi
done

# Copy data directory (same for both architectures)
echo ""
echo "========================================="
echo "Step 5: Copying data directory..."
echo "========================================="
echo ""

DATA_SOURCE="${PROJECT_ROOT}/data"
DATA_TARGET="${BIN_DIR_UNIVERSAL}/data"

if [[ -d "${DATA_SOURCE}" ]]; then
  echo "Copying data directory to ${DATA_TARGET}..."
  mkdir -p "${DATA_TARGET}"
  rsync -a "${DATA_SOURCE}/" "${DATA_TARGET}/"
  echo "  ✓ Data directory copied"
else
  echo "  ⚠ Warning: Data directory not found at ${DATA_SOURCE}" >&2
fi

# Step 6: Create universal app bundles
echo ""
echo "========================================="
echo "Step 6: Creating universal app bundles..."
echo "========================================="
echo ""

"${SCRIPT_DIR}/build_app_bundles.command" universal || {
  echo "Error: Failed to create universal app bundles" >&2
  exit 1
}

echo ""
echo "========================================="
echo "Universal build complete!"
echo "========================================="
echo "Universal binaries are in: ${BIN_DIR_UNIVERSAL}/"
echo "Universal libraries are in: ${LIB_DIR_UNIVERSAL}/"
echo "App bundles are in: ${PROJECT_ROOT}/macos/app_universal/"
echo "  - FPCAtomic.app"
echo "  - FPCAtomicLauncher.app"
echo "  - FPCAtomicServer.app"
echo ""

