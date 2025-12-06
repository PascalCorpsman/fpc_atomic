#!/bin/zsh
set -euo pipefail

# Script to prepare x86_64 libraries for cross-compilation
# This script verifies and fixes library architectures

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
LIB_DIR="${PROJECT_ROOT}/macos/lib/x86_64"

echo "========================================="
echo "Preparing x86_64 libraries"
echo "========================================="
echo ""

# Function to check architecture of a library
check_arch() {
  local lib_path="$1"
  if [[ ! -f "${lib_path}" ]]; then
    return 1
  fi
  
  # Check with lipo first
  local lipo_output=$(lipo -info "${lib_path}" 2>/dev/null || echo "")
  
  if [[ -z "${lipo_output}" ]]; then
    # lipo failed, try file command
    local file_output=$(file "${lib_path}" 2>/dev/null || echo "")
    if echo "${file_output}" | grep -q "x86_64"; then
      echo "x86_64"
      return 0
    elif echo "${file_output}" | grep -q "arm64"; then
      echo "arm64"
      return 0
    else
      echo "unknown"
      return 1
    fi
  fi
  
  # Parse lipo output
  if echo "${lipo_output}" | grep -q "Non-fat file"; then
    # Single architecture - extract the architecture name
    if echo "${lipo_output}" | grep -qE "architecture: x86_64"; then
      echo "x86_64"
      return 0
    elif echo "${lipo_output}" | grep -qE "architecture: arm64"; then
      echo "arm64"
      return 0
    else
      # Try to extract any architecture mentioned
      local extracted_arch=$(echo "${lipo_output}" | grep -oE "architecture: [^ ]+" | cut -d' ' -f2 || echo "")
      if [[ -n "${extracted_arch}" ]]; then
        echo "${extracted_arch}"
        return 0
      else
        echo "unknown"
        return 1
      fi
    fi
  elif echo "${lipo_output}" | grep -q "Architectures in the fat file"; then
    # Universal binary
    if echo "${lipo_output}" | grep -q "x86_64"; then
      echo "x86_64 (universal)"
      return 0
    else
      echo "no-x86_64"
      return 1
    fi
  else
    # Fallback to file command
    local file_output=$(file "${lib_path}" 2>/dev/null || echo "")
    if echo "${file_output}" | grep -q "x86_64"; then
      echo "x86_64"
      return 0
    elif echo "${file_output}" | grep -q "arm64"; then
      echo "arm64"
      return 0
    else
      echo "unknown"
      return 1
    fi
  fi
}

# Function to extract x86_64 slice from universal binary
extract_x86_64() {
  local lib_path="$1"
  local temp_path="${lib_path}.tmp"
  
  if lipo -extract x86_64 "${lib_path}" -output "${temp_path}" 2>/dev/null; then
    mv "${temp_path}" "${lib_path}"
    echo "  ✓ Extracted x86_64 slice"
    return 0
  else
    rm -f "${temp_path}"
    return 1
  fi
}

# Function to get x86_64 OpenSSL libraries
get_openssl_x86_64() {
  local lib_name="$1"
  local dest_path="${LIB_DIR}/${lib_name}"
  
  # Try to find Homebrew universal binary and extract x86_64
  local brew_paths=(
    "/opt/homebrew/lib/${lib_name}"
    "$(brew --prefix 2>/dev/null)/lib/${lib_name}"
  )
  
  for brew_path in "${brew_paths[@]}"; do
    if [[ -f "${brew_path}" ]]; then
      local arch=$(check_arch "${brew_path}")
      if [[ "${arch}" == "x86_64"* ]]; then
        echo "  → Found x86_64 version at ${brew_path}"
        cp "${brew_path}" "${dest_path}"
        return 0
      elif [[ "${arch}" == *"universal"* ]]; then
        echo "  → Found universal binary, extracting x86_64..."
        cp "${brew_path}" "${dest_path}"
        if extract_x86_64 "${dest_path}"; then
          return 0
        fi
      fi
    fi
  done
  
  # Try Rosetta-based Homebrew (x86_64 Homebrew installed via Rosetta)
  if command -v arch &> /dev/null; then
    # Try different possible locations for Rosetta Homebrew
    local rosetta_brew_paths=(
      "/usr/local/bin/brew"
      "$(which brew 2>/dev/null || echo "")"
    )
    
    for brew_bin in "${rosetta_brew_paths[@]}"; do
      if [[ -f "${brew_bin}" ]] || command -v "${brew_bin}" &> /dev/null; then
        echo "  → Checking Rosetta Homebrew at ${brew_bin}..."
        local rosetta_prefix=$(arch -x86_64 "${brew_bin}" --prefix 2>/dev/null || echo "")
        if [[ -n "${rosetta_prefix}" ]] && [[ "${rosetta_prefix}" != "$(brew --prefix 2>/dev/null)" ]]; then
          # This is a different Homebrew installation (likely Rosetta)
          local rosetta_lib="${rosetta_prefix}/lib/${lib_name}"
          if [[ -f "${rosetta_lib}" ]]; then
            local arch=$(check_arch "${rosetta_lib}")
            if [[ "${arch}" == "x86_64"* ]]; then
              echo "  → Found x86_64 version in Rosetta Homebrew at ${rosetta_lib}"
              cp "${rosetta_lib}" "${dest_path}"
              return 0
            elif [[ "${arch}" == *"universal"* ]]; then
              echo "  → Found universal binary in Rosetta Homebrew, extracting x86_64..."
              cp "${rosetta_lib}" "${dest_path}"
              if extract_x86_64 "${dest_path}"; then
                return 0
              fi
            fi
          fi
        fi
      fi
    done
    
    # Also try direct path check for /usr/local (common Rosetta Homebrew location)
    if [[ -d "/usr/local/lib" ]]; then
      local direct_rosetta_lib="/usr/local/lib/${lib_name}"
      if [[ -f "${direct_rosetta_lib}" ]]; then
        local arch=$(check_arch "${direct_rosetta_lib}")
        if [[ "${arch}" == "x86_64"* ]]; then
          echo "  → Found x86_64 version in /usr/local/lib at ${direct_rosetta_lib}"
          cp "${direct_rosetta_lib}" "${dest_path}"
          return 0
        elif [[ "${arch}" == *"universal"* ]]; then
          echo "  → Found universal binary in /usr/local/lib, extracting x86_64..."
          cp "${direct_rosetta_lib}" "${dest_path}"
          if extract_x86_64 "${dest_path}"; then
            return 0
          fi
        fi
      fi
    fi
  fi
  
  # Try system libraries (macOS may have OpenSSL in /usr/lib)
  local system_paths=(
    "/usr/lib/libssl.dylib"
    "/usr/lib/libcrypto.dylib"
  )
  
  # Map lib names to system names
  local system_lib_name="${lib_name}"
  if [[ "${lib_name}" == "libssl.3.dylib" ]]; then
    system_lib_name="libssl.dylib"
  elif [[ "${lib_name}" == "libcrypto.3.dylib" ]]; then
    system_lib_name="libcrypto.dylib"
  fi
  
  for sys_path in "/usr/lib/${system_lib_name}"; do
    if [[ -f "${sys_path}" ]]; then
      local arch=$(check_arch "${sys_path}")
      if [[ "${arch}" == "x86_64"* ]]; then
        echo "  → Found x86_64 version in system at ${sys_path}"
        cp "${sys_path}" "${dest_path}"
        # Fix install name if needed
        install_name_tool -id "@rpath/${lib_name}" "${dest_path}" 2>/dev/null || true
        return 0
      elif [[ "${arch}" == *"universal"* ]]; then
        echo "  → Found universal binary in system, extracting x86_64..."
        cp "${sys_path}" "${dest_path}"
        if extract_x86_64 "${dest_path}"; then
          install_name_tool -id "@rpath/${lib_name}" "${dest_path}" 2>/dev/null || true
          return 0
        fi
      fi
    fi
  done
  
  echo "  ⚠ Could not find x86_64 OpenSSL. Options:"
  echo "     1. Install x86_64 Homebrew via Rosetta:"
  echo "        arch -x86_64 /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
  echo "        Then: arch -x86_64 brew install openssl@3"
  echo "     2. Manually copy x86_64 libraries to ${LIB_DIR}/"
  echo "     3. Continue anyway (application may not work on x86_64 Mac)"
  
  return 1
}

# Ensure lib directory exists
mkdir -p "${LIB_DIR}"

# List of required libraries (using zsh associative array syntax)
typeset -A REQUIRED_LIBS
REQUIRED_LIBS[libSDL2.dylib]="SDL2"
REQUIRED_LIBS[libbass.dylib]="BASS"
REQUIRED_LIBS[libai.dylib]="AI"
REQUIRED_LIBS[libcrypto.3.dylib]="OpenSSL"
REQUIRED_LIBS[libssl.3.dylib]="OpenSSL"

ERRORS=0

# Check each library
for lib_name in "${(@k)REQUIRED_LIBS}"; do
  lib_path="${LIB_DIR}/${lib_name}"
  lib_type="${REQUIRED_LIBS[${lib_name}]}"
  
  echo "Checking ${lib_name} (${lib_type})..."
  
  if [[ ! -f "${lib_path}" ]]; then
    echo "  ✗ Missing: ${lib_name}"
    
    # Try to fix based on library type
    case "${lib_type}" in
      "SDL2")
        # Try to copy from framework
        framework_path="${LIB_DIR}/frameworks/SDL2.framework/SDL2"
        if [[ -f "${framework_path}" ]]; then
          local arch=$(check_arch "${framework_path}")
          if [[ "${arch}" == "x86_64"* ]]; then
            echo "  → Copying from SDL2.framework..."
            cp "${framework_path}" "${lib_path}"
            # Fix install name for dylib
            install_name_tool -id "@rpath/${lib_name}" "${lib_path}" 2>/dev/null || true
            echo "  ✓ Created ${lib_name} from framework"
          else
            echo "  ✗ Framework is not x86_64"
            ((ERRORS++))
          fi
        else
          echo "  ✗ SDL2.framework not found"
          ((ERRORS++))
        fi
        ;;
      "OpenSSL")
        if get_openssl_x86_64 "${lib_name}"; then
          echo "  ✓ Retrieved x86_64 version"
        else
          echo "  ✗ Could not get x86_64 version"
          ((ERRORS++))
        fi
        ;;
      *)
        echo "  ✗ Don't know how to fix this library"
        ((ERRORS++))
        ;;
    esac
    continue
  fi
  
  # Check architecture
  arch=$(check_arch "${lib_path}")
  
  if [[ "${arch}" == "x86_64"* ]]; then
    echo "  ✓ ${lib_name} is ${arch}"
  elif [[ "${arch}" == "arm64" ]]; then
    echo "  ✗ ${lib_name} is ARM64 (should be x86_64)"
    
    # Try to fix
    case "${lib_type}" in
      "OpenSSL")
        if get_openssl_x86_64 "${lib_name}"; then
          echo "  ✓ Fixed: replaced with x86_64 version"
        else
          echo "  ✗ Could not fix"
          ((ERRORS++))
        fi
        ;;
      "SDL2")
        # Try framework
        framework_path="${LIB_DIR}/frameworks/SDL2.framework/SDL2"
        if [[ -f "${framework_path}" ]]; then
          local framework_arch=$(check_arch "${framework_path}")
          if [[ "${framework_arch}" == "x86_64"* ]]; then
            echo "  → Copying from SDL2.framework..."
            cp "${framework_path}" "${lib_path}"
            install_name_tool -id "@rpath/${lib_name}" "${lib_path}" 2>/dev/null || true
            echo "  ✓ Fixed: replaced with x86_64 version from framework"
          else
            echo "  ✗ Framework is also wrong architecture"
            ((ERRORS++))
          fi
        else
          echo "  ✗ Could not fix"
          ((ERRORS++))
        fi
        ;;
      *)
        echo "  ✗ Don't know how to fix this library"
        ((ERRORS++))
        ;;
    esac
  elif [[ "${arch}" == "no-x86_64" ]]; then
    echo "  ✗ ${lib_name} is universal but doesn't contain x86_64"
    if extract_x86_64 "${lib_path}"; then
      echo "  ✓ Fixed: extracted x86_64 slice"
    else
      echo "  ✗ Could not extract x86_64"
      ((ERRORS++))
    fi
  else
    echo "  ✗ ${lib_name} has unknown architecture"
    ((ERRORS++))
  fi
done

echo ""
echo "========================================="
echo "Verifying library architectures"
echo "========================================="
echo ""

# Final verification
ALL_OK=true
for lib_name in "${(@k)REQUIRED_LIBS}"; do
  lib_path="${LIB_DIR}/${lib_name}"
  if [[ ! -f "${lib_path}" ]]; then
    echo "✗ ${lib_name}: MISSING"
    ALL_OK=false
    continue
  fi
  
  arch=$(check_arch "${lib_path}")
  if [[ "${arch}" == "x86_64"* ]]; then
    echo "✓ ${lib_name}: ${arch}"
  else
    echo "✗ ${lib_name}: ${arch} (should be x86_64)"
    ALL_OK=false
  fi
done

echo ""
if [[ "${ALL_OK}" == "true" && "${ERRORS}" -eq 0 ]]; then
  echo "========================================="
  echo "All x86_64 libraries are ready!"
  echo "========================================="
  exit 0
else
  echo "========================================="
  echo "WARNING: Some libraries are missing or have wrong architecture"
  echo "========================================="
  echo ""
  echo "The build will continue, but the application may not work correctly"
  echo "on x86_64 Macs. Please fix the library issues before distributing."
  echo ""
  echo "To skip this check in the future, set SKIP_LIB_CHECK=1"
  echo ""
  # Allow build to continue with warning
  exit 0
fi

