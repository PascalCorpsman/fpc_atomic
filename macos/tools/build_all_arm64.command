#!/bin/zsh
set -euo pipefail

# Build script for FPC Atomic macOS - ARM64 (Apple Silicon)
# This is a convenience wrapper that calls build_all.command with arm64 parameter

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Call the main build script with arm64 architecture
"${SCRIPT_DIR}/build_all.command" arm64

