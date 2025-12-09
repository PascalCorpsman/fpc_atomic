#!/bin/zsh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

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

BIN_DIR="${PROJECT_ROOT}/bin/macos/${TARGET_ARCH}"
LIB_DIR="${PROJECT_ROOT}/lib/macos/${TARGET_ARCH}"
APP_ROOT="${PROJECT_ROOT}/macos/app_${TARGET_ARCH}"
SHARED_DATA_DIR="${APP_ROOT}/data"
TEMPLATE_ROOT="${PROJECT_ROOT}/macos/app_templates"
# Assets directory relative to macos/ directory (where PROJECT_ROOT points)
ASSETS_DIR="${PROJECT_ROOT}/macos/assets"
# Check for Icon Composer .icon format first (new format from 2025)
ICON_DIR="${ASSETS_DIR}/AtomicBomberIcon.icon"
ICON_FILE="${ASSETS_DIR}/AtomicBomberIcon.icns"
ICON_USE_FORMAT=""

# Priority 1: Use existing .icns file if it exists
if [[ -f "${ICON_FILE}" ]]; then
  ICON_USE_FORMAT="icns"
# Priority 2: Use .icon format directly (Icon Composer format)
elif [[ -d "${ICON_DIR}" ]] && [[ -f "${ICON_DIR}/icon.json" ]]; then
  ICON_USE_FORMAT="icon"
  # Try to generate .icns from .icon format if .iconset exists
  if [[ -d "${ASSETS_DIR}/AtomicBomberIcon.iconset" ]] && command -v iconutil &> /dev/null; then
    iconutil -c icns "${ASSETS_DIR}/AtomicBomberIcon.iconset" -o "${ICON_FILE}" &>/dev/null
    if [[ -f "${ICON_FILE}" ]]; then
      ICON_USE_FORMAT="icns"
    fi
  fi
# Priority 3: Generate .icns from PNG source
elif [[ -d "${ICON_DIR}" ]] && [[ -f "${ICON_DIR}/Assets/bomberman-ikona-2.png" ]] && command -v iconutil &> /dev/null && command -v sips &> /dev/null; then
  local iconset_dir="${ASSETS_DIR}/AtomicBomberIcon.iconset"
  mkdir -p "${iconset_dir}"
  local source_img="${ICON_DIR}/Assets/bomberman-ikona-2.png"
  sips -z 16 16 "${source_img}" --out "${iconset_dir}/icon_16x16.png" &>/dev/null
  sips -z 32 32 "${source_img}" --out "${iconset_dir}/icon_16x16@2x.png" &>/dev/null
  sips -z 32 32 "${source_img}" --out "${iconset_dir}/icon_32x32.png" &>/dev/null
  sips -z 64 64 "${source_img}" --out "${iconset_dir}/icon_32x32@2x.png" &>/dev/null
  sips -z 128 128 "${source_img}" --out "${iconset_dir}/icon_128x128.png" &>/dev/null
  sips -z 256 256 "${source_img}" --out "${iconset_dir}/icon_128x128@2x.png" &>/dev/null
  sips -z 256 256 "${source_img}" --out "${iconset_dir}/icon_256x256.png" &>/dev/null
  sips -z 512 512 "${source_img}" --out "${iconset_dir}/icon_256x256@2x.png" &>/dev/null
  sips -z 512 512 "${source_img}" --out "${iconset_dir}/icon_512x512.png" &>/dev/null
  sips -z 1024 1024 "${source_img}" --out "${iconset_dir}/icon_512x512@2x.png" &>/dev/null
  iconutil -c icns "${iconset_dir}" -o "${ICON_FILE}" &>/dev/null
  if [[ -f "${ICON_FILE}" ]]; then
    ICON_USE_FORMAT="icns"
  fi
fi

if [[ ! -x "${BIN_DIR}/fpc_atomic" ]]; then
  echo "Missing client binary at ${BIN_DIR}/fpc_atomic. Build it first (lazbuild --build-mode=macos_${TARGET_ARCH})." >&2
  exit 1
fi

if [[ ! -x "${BIN_DIR}/atomic_launcher" ]]; then
  echo "Missing launcher binary at ${BIN_DIR}/atomic_launcher. Build it first." >&2
  exit 1
fi

if [[ ! -x "${BIN_DIR}/atomic_server" ]]; then
  echo "Missing server binary at ${BIN_DIR}/atomic_server. Build it first." >&2
  exit 1
fi

if [[ ! -d "${BIN_DIR}/data" ]]; then
  echo "Missing data directory at ${BIN_DIR}/data. Run the data extractor and copy results before packaging." >&2
  exit 1
fi

if [[ ! -d "${LIB_DIR}" ]]; then
  echo "Missing library directory at ${LIB_DIR}. Ensure macOS libraries are prepared." >&2
  exit 1
fi

function sync_libs() {
  local dest_dir="$1"
  mkdir -p "${dest_dir}"
  rsync -a --delete "${LIB_DIR}/" "${dest_dir}/"
}

function ensure_shared_data() {
  mkdir -p "${SHARED_DATA_DIR}"
  rsync -a --delete "${BIN_DIR}/data/" "${SHARED_DATA_DIR}/"
}

function copy_binary() {
  local source_path="$1"
  local dest_path="$2"
  install -m 755 "${source_path}" "${dest_path}"
}

function link_shared_data() {
  local macos_dir="$1"
  local link_path="${macos_dir}/data"

  if [[ -e "${link_path}" || -L "${link_path}" ]]; then
    rm -rf "${link_path}"
  fi

  ln -s "../../../data" "${link_path}"
}

function copy_icon() {
  local app_bundle="$1"
  local resources_dir="${app_bundle}/Contents/Resources"
  
  mkdir -p "${resources_dir}"
  local info_plist="${app_bundle}/Contents/Info.plist"
  
  # Use the format determined at the top of the script
  if [[ "${ICON_USE_FORMAT}" == "icon" ]] && [[ -d "${ICON_DIR}" ]]; then
    # Use .icon format directly (Icon Composer format from 2025)
    # Copy the entire .icon directory to Resources
    local icon_bundle_name="AppIcon.icon"
    local icon_bundle_path="${resources_dir}/${icon_bundle_name}"
    
    # Remove old icon bundle if it exists
    if [[ -d "${icon_bundle_path}" ]] || [[ -L "${icon_bundle_path}" ]]; then
      rm -rf "${icon_bundle_path}"
    fi
    
    # Copy .icon directory
    cp -R "${ICON_DIR}" "${icon_bundle_path}"
    echo "  ✓ Copied .icon bundle from ${ICON_DIR} to ${icon_bundle_path}"
    
    # Set CFBundleIconFile to .icon (new format - may require macOS Sequoia 15.1+)
    if [[ -f "${info_plist}" ]] && command -v plutil &> /dev/null; then
      plutil -replace CFBundleIconFile -string "${icon_bundle_name}" "${info_plist}" 2>/dev/null || \
      plutil -insert CFBundleIconFile -string "${icon_bundle_name}" "${info_plist}" 2>/dev/null || true
    fi
  elif [[ "${ICON_USE_FORMAT}" == "icns" ]] && [[ -f "${ICON_FILE}" ]]; then
    # Use traditional .icns format
    cp "${ICON_FILE}" "${resources_dir}/AppIcon.icns"
    echo "  ✓ Copied .icns icon from ${ICON_FILE} to ${resources_dir}/AppIcon.icns"
    
    # Set CFBundleIconFile to AppIcon (without extension - macOS requirement for Dock icon)
    if [[ -f "${info_plist}" ]] && command -v plutil &> /dev/null; then
      plutil -replace CFBundleIconFile -string "AppIcon" "${info_plist}" 2>/dev/null || \
      plutil -insert CFBundleIconFile -string "AppIcon" "${info_plist}" 2>/dev/null || true
    fi
  else
    # Icon is optional - silently skip if not found
    return 0
  fi
  
  # Disable fullscreen capability to prevent "Game Mode" / "Capture Display" issues
  # This prevents macOS from automatically switching to fullscreen when window is maximized
  if [[ -f "${info_plist}" ]] && command -v plutil &> /dev/null; then
    # Set LSUIElement to false (if not set) - ensure app appears in dock
    plutil -replace LSUIElement -bool false "${info_plist}" 2>/dev/null || true
    # Note: Unfortunately, macOS doesn't provide a direct Info.plist key to disable
    # fullscreen mode. We prevent it in code by:
    # 1. Setting Constraints.MaxWidth/Height to Screen.Width/Height - 1 (prevents fullscreen trigger)
    # 2. Never using wsFullScreen/wsMaximized WindowState
    # 3. Monitoring WindowState in Timer1Timer and FormResize to immediately revert any fullscreen attempts
  fi
}

function ensure_template() {
  local template_name="$1"
  local bundle_name="$2"
  local template_dir="${TEMPLATE_ROOT}/${template_name}"
  local app_dir="${APP_ROOT}/${bundle_name}"

  if [[ ! -d "${template_dir}" ]]; then
    echo "Missing template directory ${template_dir}" >&2
    exit 1
  fi

  mkdir -p "${app_dir}"
  rsync -a --delete "${template_dir}/" "${app_dir}/"
}

if [[ "${TARGET_ARCH}" == "universal" ]]; then
  echo "Preparing FPCAtomic.app (Universal: arm64 + x86_64)"
else
  echo "Preparing FPCAtomic.app (${TARGET_ARCH})"
fi
ensure_template "Game" "FPCAtomic.app"
GAME_APP="${APP_ROOT}/FPCAtomic.app"
GAME_MACOS_DIR="${GAME_APP}/Contents/MacOS"
GAME_LIB_DIR="${GAME_APP}/Contents/lib"

mkdir -p "${GAME_MACOS_DIR}" "${GAME_LIB_DIR}"
copy_binary "${BIN_DIR}/fpc_atomic" "${GAME_MACOS_DIR}/fpc_atomic"
sync_libs "${GAME_LIB_DIR}"
link_shared_data "${GAME_MACOS_DIR}"
copy_icon "${GAME_APP}"

if [[ "${TARGET_ARCH}" == "universal" ]]; then
  echo "Preparing FPCAtomicLauncher.app (Universal: arm64 + x86_64)"
else
  echo "Preparing FPCAtomicLauncher.app (${TARGET_ARCH})"
fi
ensure_template "Launcher" "FPCAtomicLauncher.app"
LAUNCHER_APP="${APP_ROOT}/FPCAtomicLauncher.app"
LAUNCHER_MACOS_DIR="${LAUNCHER_APP}/Contents/MacOS"
LAUNCHER_LIB_DIR="${LAUNCHER_APP}/Contents/lib"

mkdir -p "${LAUNCHER_MACOS_DIR}" "${LAUNCHER_LIB_DIR}"
copy_binary "${BIN_DIR}/atomic_launcher" "${LAUNCHER_MACOS_DIR}/atomic_launcher"
copy_binary "${BIN_DIR}/fpc_atomic" "${LAUNCHER_MACOS_DIR}/fpc_atomic"
copy_binary "${BIN_DIR}/atomic_server" "${LAUNCHER_MACOS_DIR}/atomic_server"
# Copy CD Data Extractor GUI for launcher
if [[ -x "${BIN_DIR}/cd_data_extractor" ]]; then
  copy_binary "${BIN_DIR}/cd_data_extractor" "${LAUNCHER_MACOS_DIR}/cd_data_extractor"
  echo "  ✓ Copied cd_data_extractor to ${LAUNCHER_MACOS_DIR}/cd_data_extractor"
else
  echo "  ⚠ Warning: cd_data_extractor not found at ${BIN_DIR}/cd_data_extractor" >&2
fi
sync_libs "${LAUNCHER_LIB_DIR}"
link_shared_data "${LAUNCHER_MACOS_DIR}"
copy_icon "${LAUNCHER_APP}"

if [[ "${TARGET_ARCH}" == "universal" ]]; then
  echo "Preparing FPCAtomicServer.app (Universal: arm64 + x86_64)"
else
  echo "Preparing FPCAtomicServer.app (${TARGET_ARCH})"
fi
ensure_template "Server" "FPCAtomicServer.app"
SERVER_APP="${APP_ROOT}/FPCAtomicServer.app"
SERVER_MACOS_DIR="${SERVER_APP}/Contents/MacOS"
SERVER_LIB_DIR="${SERVER_APP}/Contents/lib"

mkdir -p "${SERVER_MACOS_DIR}" "${SERVER_LIB_DIR}"
copy_binary "${BIN_DIR}/atomic_server" "${SERVER_MACOS_DIR}/atomic_server"
chmod +x "${SERVER_MACOS_DIR}/run_server"
sync_libs "${SERVER_LIB_DIR}"
link_shared_data "${SERVER_MACOS_DIR}"
copy_icon "${SERVER_APP}"

echo "Synchronising shared data directory"
ensure_shared_data

echo "Signing app bundles"
for app in "${GAME_APP}" "${LAUNCHER_APP}" "${SERVER_APP}"; do
  # Sign all libraries first (required for proper bundle signing)
  echo "Signing libraries in $(basename "${app}")..."
  if [[ -d "${app}/Contents/lib" ]]; then
    for lib in "${app}/Contents/lib"/*.dylib; do
      if [[ -f "${lib}" ]]; then
        /usr/bin/codesign --force --sign - "${lib}" 2>&1 | grep -v "already signed" || true
      fi
    done
  fi
  
  # Sign all executables in MacOS directory (skip symlinks and directories)
  if [[ -d "${app}/Contents/MacOS" ]]; then
    for exe in "${app}/Contents/MacOS"/*; do
      if [[ -f "${exe}" ]] && [[ -x "${exe}" ]] && ! [[ -L "${exe}" ]]; then
        /usr/bin/codesign --force --sign - "${exe}" 2>&1 | grep -v "already signed" || true
      fi
    done
  fi
  
  # Check if entitlements file exists for this app
  ENTITLEMENTS_FILE="${app}/Contents/entitlements.plist"
  if [[ -f "${ENTITLEMENTS_FILE}" ]]; then
    echo "Signing $(basename "${app}") bundle with entitlements..."
    /usr/bin/codesign --force --deep --sign - --entitlements "${ENTITLEMENTS_FILE}" "${app}"
  else
    echo "Signing $(basename "${app}") bundle without entitlements..."
    /usr/bin/codesign --force --deep --sign - "${app}"
  fi
done

echo "Done. Bundles are in ${APP_ROOT}:"
echo "  - FPCAtomic.app"
echo "  - FPCAtomicLauncher.app"
echo "  - FPCAtomicServer.app"


