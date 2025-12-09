#!/bin/zsh
set -euo pipefail

# Master script to build, sign, notarize, and create DMG for release
# Usage: ./create_release.command [arm64|x86_64|universal] [--skip-notarize]

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Load .env file if it exists
ENV_FILE="${PROJECT_ROOT}/.env"
if [[ -f "${ENV_FILE}" ]]; then
  echo "Loading configuration from .env file..."
  # Export variables from .env file (skip comments and empty lines)
  # This uses zsh's built-in source with set -a to auto-export
  set -a
  source "${ENV_FILE}" 2>/dev/null || {
    # Fallback: manually parse if source fails
    while IFS= read -r line || [[ -n "$line" ]]; do
      # Skip comments and empty lines
      [[ "$line" =~ ^[[:space:]]*# ]] && continue
      [[ -z "$line" ]] && continue
      # Remove 'export ' prefix if present
      line="${line#export }"
      # Export the line (assuming format KEY=value)
      export "$line"
    done < "${ENV_FILE}"
  }
  set +a
  
  # Map alternative variable names to expected names
  # Support both NOTARY_APP_SPECIFIC_PASSWORD and APPLE_ID_PASSWORD
  if [[ -n "${NOTARY_APP_SPECIFIC_PASSWORD:-}" ]] && [[ -z "${APPLE_ID_PASSWORD:-}" ]]; then
    export APPLE_ID_PASSWORD="${NOTARY_APP_SPECIFIC_PASSWORD}"
  fi
  # Support both APPLE_TEAM_ID and TEAM_ID
  if [[ -n "${APPLE_TEAM_ID:-}" ]] && [[ -z "${TEAM_ID:-}" ]]; then
    export TEAM_ID="${APPLE_TEAM_ID}"
  fi
  # Support SKIP_NOTARIZATION (inverted logic)
  if [[ -n "${SKIP_NOTARIZATION:-}" ]]; then
    if [[ "${SKIP_NOTARIZATION}" == "1" ]] || [[ "${SKIP_NOTARIZATION}" == "true" ]]; then
      export NOTARIZE="false"
    else
      export NOTARIZE="true"
    fi
  fi
fi

# Parse arguments
TARGET_ARCH=""
SKIP_NOTARIZE=false

for arg in "$@"; do
  case "${arg}" in
    arm64|aarch64)
      TARGET_ARCH="arm64"
      ;;
    x86_64|intel)
      TARGET_ARCH="x86_64"
      ;;
    universal)
      TARGET_ARCH="universal"
      ;;
    --skip-notarize)
      SKIP_NOTARIZE=true
      ;;
    *)
      echo "Unknown argument: ${arg}" >&2
      ;;
  esac
done

# If no architecture specified, detect automatically
if [[ -z "${TARGET_ARCH}" ]]; then
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

echo "========================================="
echo "Creating Release for ${TARGET_ARCH}"
echo "========================================="

# Step 1: Build binaries and app bundles
echo ""
echo "Step 1: Building binaries and app bundles..."
echo "========================================="
if [[ "${TARGET_ARCH}" == "universal" ]]; then
  "${SCRIPT_DIR}/build_all_universal.command" || {
    echo "Error: Build failed" >&2
    exit 1
  }
else
  "${SCRIPT_DIR}/build_all.command" "${TARGET_ARCH}" || {
    echo "Error: Build failed" >&2
    exit 1
  }
fi

# Step 2: Sign and notarize
echo ""
echo "Step 2: Signing and notarizing app bundles..."
echo "========================================="
if [[ "${SKIP_NOTARIZE}" == "true" ]]; then
  export NOTARIZE=false
  echo "Skipping notarization (--skip-notarize flag set)"
else
  export NOTARIZE=true
fi

"${SCRIPT_DIR}/sign_and_notarize.command" "${TARGET_ARCH}" || {
  echo "Error: Signing/notarization failed" >&2
  echo "You can continue without notarization using --skip-notarize flag" >&2
  exit 1
}

# Step 3: Create DMG
echo ""
echo "Step 3: Creating DMG..."
echo "========================================="
"${SCRIPT_DIR}/create_dmg.command" "${TARGET_ARCH}" || {
  echo "Error: DMG creation failed" >&2
  exit 1
}

# Get version if available for display
VERSION="${VERSION:-}"
if [[ -z "${VERSION}" ]] && [[ -f "${PROJECT_ROOT}/bin/fpc_atomic.version" ]]; then
  VERSION=$(grep -E "^version=" "${PROJECT_ROOT}/bin/fpc_atomic.version" | cut -d'=' -f2 | tr -d '"' || echo "")
fi

echo ""
echo "========================================="
echo "Release created successfully!"
echo "========================================="
echo "DMG file location:"
if [[ -n "${VERSION}" ]]; then
  echo "  releases/FPCAtomic-${VERSION}-${TARGET_ARCH}.dmg"
else
  echo "  releases/FPCAtomic-${TARGET_ARCH}.dmg"
fi
echo "========================================="

