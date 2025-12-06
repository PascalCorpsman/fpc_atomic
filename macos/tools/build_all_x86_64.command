#!/bin/zsh
set -euo pipefail

# Build script for FPC Atomic macOS - x86_64 (Intel)
# This is a convenience wrapper that calls build_all.command with x86_64 parameter

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "========================================="
echo "Building FPC Atomic for macOS (x86_64)"
echo "========================================="
echo ""

# Step 1: Prepare x86_64 libraries (skip if SKIP_LIB_CHECK is set)
if [[ "${SKIP_LIB_CHECK:-0}" != "1" ]]; then
  echo "Step 1: Preparing x86_64 libraries..."
  "${SCRIPT_DIR}/prepare_x86_64_libs.command" || {
    echo "Warning: Library check failed, but continuing build..." >&2
  }
  echo ""
fi

# Step 2: Build applications
echo "Step 2: Building applications..."
"${SCRIPT_DIR}/build_all.command" x86_64

