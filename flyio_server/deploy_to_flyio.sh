#!/bin/bash
# Script to deploy FPC Atomic Server to Fly.io
# Works on macOS, Linux, and Windows (with Git Bash or WSL)
# This script ensures the correct build context (project root) is used

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
FLYIO_DIR="${SCRIPT_DIR}"

echo "========================================="
echo "Deploying FPC Atomic Server to Fly.io"
echo "========================================="
echo ""

# Check if flyctl is installed
if ! command -v flyctl &> /dev/null; then
  echo "Error: flyctl is not installed" >&2
  echo "Please install it using:" >&2
  echo "  curl -L https://fly.io/install.sh | sh" >&2
  echo "Or visit: https://fly.io/docs/getting-started/installing-flyctl/" >&2
  exit 1
fi

# Check if fly.toml exists
if [[ ! -f "${FLYIO_DIR}/fly.toml" ]]; then
  echo "Error: fly.toml not found at ${FLYIO_DIR}/fly.toml" >&2
  exit 1
fi

# Check if Dockerfile exists
if [[ ! -f "${FLYIO_DIR}/Dockerfile" ]]; then
  echo "Error: Dockerfile not found at ${FLYIO_DIR}/Dockerfile" >&2
  exit 1
fi

# Check if data directory exists in flyio_server/
if [[ ! -d "${FLYIO_DIR}/data" ]]; then
  echo "========================================="
  echo "⚠️  WARNING: Game data directory not found!"
  echo "========================================="
  echo ""
  echo "The server will work but WITHOUT game maps (only random maps available)."
  echo ""
  echo "To add game data before deployment:"
  echo "  1. Extract data from the original Atomic Bomberman CD"
  echo "     using the CD Data Extractor tool (included in the repository)"
  echo "  2. Copy the extracted 'data' directory to:"
  echo "     ${FLYIO_DIR}/data"
  echo ""
  echo "The data directory should contain:"
  echo "  - maps/ (game maps)"
  echo "  - res/ (resources, textures, etc.)"
  echo "  - sounds/ (sound effects)"
  echo ""
  
  # Check if data exists in project root as alternative
  if [[ -d "${PROJECT_ROOT}/data" ]]; then
    echo "Found data directory in project root: ${PROJECT_ROOT}/data"
    read -p "Copy it to flyio_server/data? (y/N) " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
      echo "Copying data directory..."
      cp -r "${PROJECT_ROOT}/data" "${FLYIO_DIR}/data"
      echo "✓ Data directory copied"
    else
      read -p "Continue deployment without game data? (y/N) " -n 1 -r
      echo ""
      if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Deployment cancelled."
        exit 0
      fi
    fi
  else
    read -p "Continue deployment without game data? (y/N) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
      echo "Deployment cancelled."
      exit 0
    fi
  fi
else
  echo "✓ Game data directory found at ${FLYIO_DIR}/data"
fi

# Change to project root (build context must be project root)
cd "${PROJECT_ROOT}"

echo ""
echo "Build context: ${PROJECT_ROOT}"
echo "Fly.io config: ${FLYIO_DIR}/fly.toml"
echo "Dockerfile: ${FLYIO_DIR}/Dockerfile"
echo ""

# Deploy to Fly.io
# Build context is project root, so we use --config to point to fly.toml
echo "Starting deployment..."
flyctl deploy --config "${FLYIO_DIR}/fly.toml" "$@"

echo ""
echo "========================================="
echo "Deployment complete!"
echo "========================================="
echo ""
echo "To check server status:"
echo "  flyctl status --config ${FLYIO_DIR}/fly.toml"
echo ""
echo "To view logs:"
echo "  flyctl logs --config ${FLYIO_DIR}/fly.toml"
echo ""
