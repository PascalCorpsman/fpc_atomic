#!/bin/zsh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

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

BIN_DIR="${PROJECT_ROOT}/bin/${TARGET_ARCH}"
LIB_DIR="${PROJECT_ROOT}/lib/${TARGET_ARCH}"
APP_ROOT="${PROJECT_ROOT}/app"
TEMPLATE_ROOT="${PROJECT_ROOT}/app_templates"

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

function sync_data() {
  local dest_dir="$1"
  mkdir -p "${dest_dir}"
  rsync -a --delete "${BIN_DIR}/data/" "${dest_dir}/"
}

function copy_binary() {
  local source_path="$1"
  local dest_path="$2"
  install -m 755 "${source_path}" "${dest_path}"
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

echo "Preparing FPCAtomic.app (${TARGET_ARCH})"
GAME_APP="${APP_ROOT}/FPCAtomic.app"
GAME_MACOS_DIR="${GAME_APP}/Contents/MacOS"
GAME_LIB_DIR="${GAME_APP}/Contents/lib"

mkdir -p "${GAME_MACOS_DIR}" "${GAME_LIB_DIR}"
copy_binary "${BIN_DIR}/fpc_atomic" "${GAME_MACOS_DIR}/fpc_atomic"
sync_data "${GAME_MACOS_DIR}/data"
sync_libs "${GAME_LIB_DIR}"

echo "Preparing FPCAtomicLauncher.app (${TARGET_ARCH})"
ensure_template "Launcher" "FPCAtomicLauncher.app"
LAUNCHER_APP="${APP_ROOT}/FPCAtomicLauncher.app"
LAUNCHER_MACOS_DIR="${LAUNCHER_APP}/Contents/MacOS"
LAUNCHER_LIB_DIR="${LAUNCHER_APP}/Contents/lib"

mkdir -p "${LAUNCHER_MACOS_DIR}" "${LAUNCHER_LIB_DIR}"
copy_binary "${BIN_DIR}/atomic_launcher" "${LAUNCHER_MACOS_DIR}/atomic_launcher"
copy_binary "${BIN_DIR}/fpc_atomic" "${LAUNCHER_MACOS_DIR}/fpc_atomic"
copy_binary "${BIN_DIR}/atomic_server" "${LAUNCHER_MACOS_DIR}/atomic_server"
sync_data "${LAUNCHER_MACOS_DIR}/data"
sync_libs "${LAUNCHER_LIB_DIR}"

echo "Preparing FPCAtomicServer.app (${TARGET_ARCH})"
ensure_template "Server" "FPCAtomicServer.app"
SERVER_APP="${APP_ROOT}/FPCAtomicServer.app"
SERVER_MACOS_DIR="${SERVER_APP}/Contents/MacOS"
SERVER_LIB_DIR="${SERVER_APP}/Contents/lib"

mkdir -p "${SERVER_MACOS_DIR}" "${SERVER_LIB_DIR}"
copy_binary "${BIN_DIR}/atomic_server" "${SERVER_MACOS_DIR}/atomic_server"
chmod +x "${SERVER_MACOS_DIR}/run_server"
sync_libs "${SERVER_LIB_DIR}"

echo "Copying shared dynamic libraries"
for app in "${GAME_APP}" "${LAUNCHER_APP}" "${SERVER_APP}"; do
  if [[ ! -d "${app}" ]]; then
    echo "Expected bundle ${app} is missing after packaging." >&2
    exit 1
  fi
done

echo "Signing app bundles"
for app in "${GAME_APP}" "${LAUNCHER_APP}" "${SERVER_APP}"; do
  /usr/bin/codesign --force --deep --sign - "${app}"
done

echo "Done. Bundles are in ${APP_ROOT}:"
echo "  - FPCAtomic.app"
echo "  - FPCAtomicLauncher.app"
echo "  - FPCAtomicServer.app"

