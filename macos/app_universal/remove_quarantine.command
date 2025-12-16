#!/bin/bash
# Remove quarantine attribute from all app bundles in this directory
# Run this after copying the apps to a new Mac

cd "$(dirname "$0")"

echo "Removing quarantine attribute from app bundles..."
echo "(You may be prompted for your password)"

for app in *.app; do
  if [[ -d "$app" ]]; then
    echo "  → $app"
    sudo xattr -cr "$app"
  fi
done

echo "Done. You can now run the applications."

# If this does not work, try the following:
# 1. Open the Terminal app
# Method 1: sudo xattr -d com.apple.quarantine (odstraní jen karanténu)
# Method 2: sudo xattr -cr (odstraní všechny extended attributes recursively)
# Method 3: bez sudo (pro soubory vlastněné uživatelem)
# Method 4: xattr -c bez sudo