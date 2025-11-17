#!/bin/zsh
set -euo pipefail

# Script to install Rosetta Homebrew and OpenSSL@3 for x86_64
# This installs Homebrew in /usr/local (x86_64) which doesn't conflict
# with ARM64 Homebrew in /opt/homebrew

echo "========================================="
echo "Installing Rosetta Homebrew (x86_64)"
echo "========================================="
echo ""
echo "This will install Homebrew in /usr/local (x86_64)"
echo "Your existing Homebrew in /opt/homebrew (ARM64) will not be affected."
echo ""

# Check if already installed
if [[ -f "/usr/local/bin/brew" ]]; then
  echo "✓ Rosetta Homebrew already installed at /usr/local/bin/brew"
  arch -x86_64 /usr/local/bin/brew --version | head -1
else
  echo "Installing Rosetta Homebrew..."
  echo "You may be prompted for your password."
  echo ""
  
  arch -x86_64 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  
  if [[ -f "/usr/local/bin/brew" ]]; then
    echo ""
    echo "✓ Rosetta Homebrew installed successfully!"
  else
    echo ""
    echo "✗ Installation failed. Please check the output above."
    exit 1
  fi
fi

echo ""
echo "========================================="
echo "Installing OpenSSL@3 (x86_64)"
echo "========================================="
echo ""

# Install OpenSSL@3 via Rosetta Homebrew
if arch -x86_64 /usr/local/bin/brew list openssl@3 &>/dev/null; then
  echo "✓ OpenSSL@3 already installed"
else
  echo "Installing OpenSSL@3..."
  arch -x86_64 /usr/local/bin/brew install openssl@3
fi

echo ""
echo "========================================="
echo "Copying OpenSSL libraries to project"
echo "========================================="
echo ""

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
LIB_DIR="${PROJECT_ROOT}/macos/lib/x86_64"

# Get Rosetta Homebrew prefix
ROSETTA_PREFIX=$(arch -x86_64 /usr/local/bin/brew --prefix)

# Copy libraries
for lib_name in "libssl.3.dylib" "libcrypto.3.dylib"; do
  source_lib="${ROSETTA_PREFIX}/lib/${lib_name}"
  dest_lib="${LIB_DIR}/${lib_name}"
  
  if [[ -f "${source_lib}" ]]; then
    echo "Copying ${lib_name}..."
    cp "${source_lib}" "${dest_lib}"
    
    # Verify architecture
    arch_info=$(file "${dest_lib}" | grep -oE "(x86_64|arm64)" | head -1)
    if [[ "${arch_info}" == "x86_64" ]]; then
      echo "  ✓ ${lib_name} is x86_64"
    else
      echo "  ⚠ ${lib_name} is ${arch_info} (expected x86_64)"
    fi
  else
    echo "  ✗ ${lib_name} not found at ${source_lib}"
  fi
done

echo ""
echo "========================================="
echo "Done!"
echo "========================================="
echo ""
echo "OpenSSL libraries have been copied to:"
echo "  ${LIB_DIR}/"
echo ""
echo "You can now rebuild the x86_64 version:"
echo "  ./macos/tools/build_all_x86_64.command"
echo ""

