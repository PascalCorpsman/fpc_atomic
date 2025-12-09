#!/bin/zsh
set -euo pipefail

# Script to sign and notarize macOS app bundles for distribution
# Requires:
# - Developer ID Application certificate installed in Keychain
# - Apple ID credentials configured for notarization
# - Environment variables: APPLE_ID, APPLE_ID_PASSWORD (or app-specific password), TEAM_ID

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

# Allow architecture to be specified as first argument
if [[ $# -gt 0 ]]; then
  REQUESTED_ARCH="$1"
  case "${REQUESTED_ARCH}" in
    arm64|aarch64)
      TARGET_ARCH="arm64"
      ;;
    x86_64|intel)
      TARGET_ARCH="x86_64"
      ;;
    universal)
      TARGET_ARCH="universal"
      ;;
    *)
      echo "Unsupported architecture: ${REQUESTED_ARCH}" >&2
      echo "Usage: $0 [arm64|x86_64|universal]" >&2
      exit 1
      ;;
  esac
else
  # Detect architecture automatically
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

APP_ROOT="${PROJECT_ROOT}/macos/app_${TARGET_ARCH}"

# Check if app bundles exist
if [[ ! -d "${APP_ROOT}/FPCAtomic.app" ]]; then
  echo "Error: FPCAtomic.app not found at ${APP_ROOT}/FPCAtomic.app" >&2
  echo "Please build app bundles first using build_app_bundles.command" >&2
  exit 1
fi

# Configuration
# These can be set via environment variables or will prompt
DEVELOPER_ID="${DEVELOPER_ID:-}"
APPLE_ID="${APPLE_ID:-}"
APPLE_ID_PASSWORD="${APPLE_ID_PASSWORD:-}"
TEAM_ID="${TEAM_ID:-}"
NOTARIZE="${NOTARIZE:-true}"

# Function to find Developer ID certificate
find_developer_id() {
  local cert_name
  # Try common Developer ID certificate names
  for cert_name in "Developer ID Application" "Developer ID Application:"; do
    if security find-identity -v -p codesigning | grep -q "${cert_name}"; then
      security find-identity -v -p codesigning | grep "${cert_name}" | head -1 | sed 's/.*"\(.*\)".*/\1/'
      return 0
    fi
  done
  return 1
}

# Function to prompt for credentials if not set
prompt_credentials() {
  if [[ -z "${DEVELOPER_ID}" ]]; then
    echo "Looking for Developer ID certificate..."
    DEVELOPER_ID=$(find_developer_id || echo "")
    if [[ -z "${DEVELOPER_ID}" ]]; then
      echo "Error: No Developer ID Application certificate found in Keychain" >&2
      echo "Please install a Developer ID Application certificate from Apple Developer portal" >&2
      echo "Or set DEVELOPER_ID in .env file (${ENV_FILE})" >&2
      echo "See macos/tools/.env.example for an example" >&2
      exit 1
    fi
    echo "Found certificate: ${DEVELOPER_ID}"
  fi

  if [[ "${NOTARIZE}" == "true" ]]; then
    if [[ -z "${APPLE_ID}" ]]; then
      echo "Error: APPLE_ID not set" >&2
      echo "Please set APPLE_ID in .env file (${ENV_FILE}) or as environment variable" >&2
      echo "See macos/tools/.env.example for an example" >&2
      exit 1
    fi
    
    if [[ -z "${APPLE_ID_PASSWORD}" ]]; then
      echo "Error: APPLE_ID_PASSWORD not set" >&2
      echo "Please set APPLE_ID_PASSWORD in .env file (${ENV_FILE}) or as environment variable" >&2
      echo "See macos/tools/.env.example for an example" >&2
      exit 1
    fi
    
    if [[ -z "${TEAM_ID}" ]]; then
      # Try to extract Team ID from certificate
      TEAM_ID=$(security find-certificate -c "${DEVELOPER_ID}" -p 2>/dev/null | openssl x509 -noout -subject 2>/dev/null | sed -n 's/.*OU=\([^/]*\).*/\1/p' | head -1)
      if [[ -z "${TEAM_ID}" ]]; then
        echo "Error: TEAM_ID not set and could not extract from certificate" >&2
        echo "Please set TEAM_ID in .env file (${ENV_FILE}) or as environment variable" >&2
        echo "See macos/tools/.env.example for an example" >&2
        exit 1
      fi
    fi
  fi
}

# Function to sign a single file or bundle
sign_item() {
  local item="$1"
  local entitlements_file="$2"
  
  if [[ ! -e "${item}" ]]; then
    echo "  ⚠ Warning: ${item} does not exist, skipping"
    return 0
  fi
  
  if [[ -f "${entitlements_file}" ]]; then
    /usr/bin/codesign --force --sign "${DEVELOPER_ID}" --entitlements "${entitlements_file}" --timestamp --options runtime "${item}" 2>&1 | grep -v "replacing existing signature" || true
  else
    /usr/bin/codesign --force --sign "${DEVELOPER_ID}" --timestamp --options runtime "${item}" 2>&1 | grep -v "replacing existing signature" || true
  fi
}

# Function to sign an app bundle
sign_app_bundle() {
  local app_bundle="$1"
  local app_name=$(basename "${app_bundle}")
  
  echo "Signing ${app_name}..."
  
  # Sign all libraries first (required for proper bundle signing)
  if [[ -d "${app_bundle}/Contents/lib" ]]; then
    for lib in "${app_bundle}/Contents/lib"/*.dylib; do
      if [[ -f "${lib}" ]]; then
        echo "  Signing library: $(basename "${lib}")"
        sign_item "${lib}" ""
      fi
    done
  fi
  
  # Sign all executables in MacOS directory (skip symlinks and directories)
  if [[ -d "${app_bundle}/Contents/MacOS" ]]; then
    for exe in "${app_bundle}/Contents/MacOS"/*; do
      if [[ -f "${exe}" ]] && [[ -x "${exe}" ]] && ! [[ -L "${exe}" ]]; then
        echo "  Signing executable: $(basename "${exe}")"
        sign_item "${exe}" ""
      fi
    done
  fi
  
  # Check if entitlements file exists
  local entitlements_file="${app_bundle}/Contents/entitlements.plist"
  if [[ -f "${entitlements_file}" ]]; then
    echo "  Signing bundle with entitlements..."
    sign_item "${app_bundle}" "${entitlements_file}"
  else
    echo "  Signing bundle without entitlements..."
    sign_item "${app_bundle}" ""
  fi
  
  # Verify signature
  echo "  Verifying signature..."
  if /usr/bin/codesign --verify --verbose "${app_bundle}" 2>&1; then
    echo "  ✓ ${app_name} signed successfully"
  else
    echo "  ✗ Error: Failed to verify signature for ${app_name}" >&2
    return 1
  fi
  
  # Note: --check-requirements is not available in all macOS versions
  # The signature verification above is sufficient for distribution
}

# Function to notarize an app bundle
notarize_app_bundle() {
  local app_bundle="$1"
  local app_name=$(basename "${app_bundle}")
  local zip_path="${app_bundle%.app}.zip"
  
  echo "Notarizing ${app_name}..."
  
  # Create a zip file for notarization
  echo "  Creating zip file for notarization..."
  cd "$(dirname "${app_bundle}")"
  /usr/bin/ditto -c -k --keepParent "${app_bundle}" "${zip_path}"
  
  # Submit for notarization
  echo "  Submitting to Apple for notarization..."
  local submission_output
  submission_output=$(xcrun notarytool submit "${zip_path}" \
    --apple-id "${APPLE_ID}" \
    --password "${APPLE_ID_PASSWORD}" \
    --team-id "${TEAM_ID}" \
    --wait \
    2>&1)
  
  local submission_id
  submission_id=$(echo "${submission_output}" | grep -i "id:" | head -1 | sed 's/.*[Ii][Dd]:[[:space:]]*\([^[:space:]]*\).*/\1/')
  
  if [[ -z "${submission_id}" ]]; then
    echo "  ✗ Error: Failed to submit for notarization" >&2
    echo "${submission_output}" >&2
    rm -f "${zip_path}"
    return 1
  fi
  
  echo "  Submission ID: ${submission_id}"
  
  # Check notarization status
  # Note: 'status' is a reserved variable in zsh, so we use 'notarization_status'
  # Parse the LAST status line (final status after processing completes)
  local notarization_status
  notarization_status=$(echo "${submission_output}" | grep -i "status:" | tail -1 | sed 's/.*[Ss]tatus:[[:space:]]*\([^[:space:]]*\).*/\1/' | sed 's/\..*$//')
  
  # Also check if output contains "Processing complete" and "status: Accepted" together
  if echo "${submission_output}" | grep -qi "Processing complete" && echo "${submission_output}" | grep -qi "status:.*Accepted"; then
    notarization_status="Accepted"
  fi
  
  if [[ "${notarization_status}" == "Accepted" ]]; then
    echo "  ✓ Notarization successful"
    
    # Staple the notarization ticket
    echo "  Stapling notarization ticket..."
    if xcrun stapler staple "${app_bundle}" 2>&1; then
      echo "  ✓ Stapling successful"
      
      # Verify stapling
      if xcrun stapler validate "${app_bundle}" 2>&1; then
        echo "  ✓ Validation successful"
      else
        echo "  ⚠ Warning: Validation failed" >&2
      fi
    else
      echo "  ✗ Error: Stapling failed" >&2
      rm -f "${zip_path}"
      return 1
    fi
  else
    echo "  ✗ Error: Notarization failed with status: ${notarization_status}" >&2
    echo "${submission_output}" >&2
    rm -f "${zip_path}"
    return 1
  fi
  
  # Clean up zip file
  rm -f "${zip_path}"
}

# Main execution
echo "========================================="
echo "Signing and Notarizing macOS App Bundles"
echo "Architecture: ${TARGET_ARCH}"
echo "========================================="

prompt_credentials

# Sign all app bundles
APPS=(
  "${APP_ROOT}/FPCAtomic.app"
  "${APP_ROOT}/FPCAtomicLauncher.app"
  "${APP_ROOT}/FPCAtomicServer.app"
)

for app in "${APPS[@]}"; do
  if [[ -d "${app}" ]]; then
    sign_app_bundle "${app}"
    echo
  else
    echo "⚠ Warning: ${app} not found, skipping" >&2
  fi
done

# Notarize if requested
if [[ "${NOTARIZE}" == "true" ]]; then
  echo "========================================="
  echo "Notarizing App Bundles"
  echo "========================================="
  echo "Note: Notarization may take up to 5 minutes per app bundle."
  echo "Please be patient while Apple processes your submission..."
  echo ""
  
  for app in "${APPS[@]}"; do
    if [[ -d "${app}" ]]; then
      notarize_app_bundle "${app}"
      echo
    fi
  done
else
  echo "========================================="
  echo "Skipping notarization (NOTARIZE=false)"
  echo "========================================="
fi

echo "========================================="
echo "Done!"
echo "Signed (and notarized) app bundles are in:"
echo "  ${APP_ROOT}"
echo "========================================="

