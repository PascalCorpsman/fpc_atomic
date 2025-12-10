#!/bin/zsh
# Script to remove quarantine attribute from FPC Atomic app bundles
# This is needed when apps are copied via AirDrop or downloaded from the internet
# macOS Gatekeeper may quarantine the apps, preventing them from running

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Find app bundles in the same directory as this script, or in parent directories
# This allows the script to work when placed in the app bundle directory or DMG

# Try to find app bundles relative to script location
if [[ -d "${SCRIPT_DIR}/../FPCAtomic.app" ]]; then
  APP_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
elif [[ -d "${SCRIPT_DIR}/FPCAtomic.app" ]]; then
  APP_DIR="${SCRIPT_DIR}"
elif [[ -d "${PROJECT_ROOT}/macos/app_arm64" ]]; then
  APP_DIR="${PROJECT_ROOT}/macos/app_arm64"
elif [[ -d "${PROJECT_ROOT}/macos/app_x86_64" ]]; then
  APP_DIR="${PROJECT_ROOT}/macos/app_x86_64"
elif [[ -d "${PROJECT_ROOT}/macos/app_universal" ]]; then
  APP_DIR="${PROJECT_ROOT}/macos/app_universal"
else
  # Try current directory
  APP_DIR="$(pwd)"
fi

echo "Removing quarantine from FPC Atomic app bundles..."
echo "Searching in: ${APP_DIR}"
echo ""

# Function to remove quarantine from a single app bundle
remove_quarantine_from_app() {
  local app_path="$1"
  local app_name=$(basename "${app_path}")
  
  if [[ ! -d "${app_path}" ]]; then
    return 1
  fi
  
  # Check if quarantine attribute exists
  if xattr -l "${app_path}" 2>/dev/null | grep -q "com.apple.quarantine"; then
    echo "Removing quarantine from ${app_name}..."
    # Try without sudo first (works for user-owned files)
    if xattr -d com.apple.quarantine "${app_path}" 2>/dev/null || \
       xattr -cr "${app_path}" 2>/dev/null; then
      echo "  ✓ Quarantine removed from ${app_name}"
      return 0
    else
      # If that fails, try with sudo (may be needed for some system locations)
      echo "  Attempting with elevated permissions..."
      if sudo xattr -d com.apple.quarantine "${app_path}" 2>/dev/null || \
         sudo xattr -cr "${app_path}" 2>/dev/null; then
        echo "  ✓ Quarantine removed from ${app_name} (with sudo)"
        return 0
      else
        echo "  ⚠ Warning: Could not remove quarantine from ${app_name}" >&2
        echo "    You may need to run: sudo xattr -cr \"${app_path}\"" >&2
        return 1
      fi
    fi
  else
    echo "  ✓ ${app_name} is not quarantined (already safe)"
    return 0
  fi
}

# Remove quarantine from all app bundles
APPS_FOUND=0

for app in "FPCAtomic.app" "FPCAtomicLauncher.app" "FPCAtomicServer.app"; do
  app_path="${APP_DIR}/${app}"
  if [[ -d "${app_path}" ]]; then
    remove_quarantine_from_app "${app_path}"
    APPS_FOUND=$((APPS_FOUND + 1))
  fi
done

if [[ ${APPS_FOUND} -eq 0 ]]; then
  echo "Warning: No FPC Atomic app bundles found in ${APP_DIR}"
  echo ""
  echo "To use this script:"
  echo "1. Place it in the same directory as the app bundles, or"
  echo "2. Run it from the directory containing the app bundles"
  echo ""
  echo "You can also manually remove quarantine with:"
  echo "  xattr -d com.apple.quarantine /path/to/AppName.app"
  echo "  xattr -cr /path/to/AppName.app"
  exit 1
fi

echo ""
echo "Done! All app bundles are now safe to run."
echo ""
echo "If you still see security warnings, you may need to:"
echo "1. Right-click the app and select 'Open' (first time only)"
echo "2. Or go to System Settings > Privacy & Security and allow the app"
