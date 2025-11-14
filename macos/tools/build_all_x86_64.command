#!/bin/zsh
set -euo pipefail

# Build script for FPC Atomic macOS - x86_64 (Intel)
# This is a convenience wrapper that calls build_all.command with x86_64 parameter

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Call the main build script with x86_64 architecture
"${SCRIPT_DIR}/build_all.command" x86_64

