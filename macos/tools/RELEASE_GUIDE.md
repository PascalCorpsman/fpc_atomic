# Release Guide for macOS

This guide explains how to create signed, notarized, and distributable DMG files for FPC Atomic on macOS.

## Prerequisites

1. **Apple Developer Account**
   - You need an active Apple Developer account ($99/year)
   - Enroll at: https://developer.apple.com/programs/

2. **Developer ID Certificate**
   - Log in to https://developer.apple.com/account/
   - Go to Certificates, Identifiers & Profiles
   - Create a "Developer ID Application" certificate
   - Download and install it in your Keychain

3. **Notarization Credentials**
   - You need your Apple ID email
   - Create an app-specific password at: https://appleid.apple.com/
   - Go to "Sign-In and Security" → "App-Specific Passwords"
   - Create a password for "Xcode" or "Command Line Tools"
   - Your Team ID (10 characters) - found in your Apple Developer account
   - **Create a `.env` file** in the project root with these credentials (see Configuration section below)

4. **Command Line Tools**
   - Xcode Command Line Tools: `xcode-select --install`
   - `notarytool` (included with Xcode 13+)

## Quick Start

### Option 1: Full Automated Release (Recommended)

```bash
cd macos/tools
./create_release.command arm64    # For Apple Silicon
./create_release.command x86_64   # For Intel Macs
./create_release.command universal # Universal binary
```

This will:
1. Build all binaries and app bundles
2. Sign them with your Developer ID
3. Notarize them with Apple
4. Create a DMG file ready for distribution

### Option 2: Step by Step

```bash
# 1. Build
./build_all.command arm64

# 2. Sign and notarize
# (Credentials are loaded from .env file automatically)
./sign_and_notarize.command arm64

# 3. Create DMG
./create_dmg.command arm64
```

**Note:** If you haven't created a `.env` file, the script will prompt you for credentials or try to find them automatically.

## Configuration

### Using .env File (Recommended)

Create a `.env` file in the project root with your signing credentials:

```bash
# .env file in project root
DEVELOPER_ID="Developer ID Application: Your Name (TEAM_ID)"
APPLE_ID="your@email.com"
APPLE_ID_PASSWORD="your-app-specific-password"
TEAM_ID="YOUR10CHARTEAMID"
NOTARIZE=true
```

**Note:** The `.env` file is already in `.gitignore`, so your credentials won't be committed to git.

The scripts will automatically load variables from `.env` if it exists.

### Using Environment Variables

Alternatively, you can set these environment variables directly:

```bash
export DEVELOPER_ID="Developer ID Application: Your Name (TEAM_ID)"
export APPLE_ID="your@email.com"
export APPLE_ID_PASSWORD="your-app-specific-password"
export TEAM_ID="YOUR10CHARTEAMID"
export NOTARIZE=true  # Set to false to skip notarization
```

## Scripts Overview

### `create_release.command`
Master script that runs the entire release process:
- Builds binaries and app bundles
- Signs with Developer ID
- Notarizes with Apple
- Creates DMG file

**Usage:**
```bash
./create_release.command [arm64|x86_64|universal] [--skip-notarize]
```

### `sign_and_notarize.command`
Signs app bundles with Developer ID and optionally notarizes them.

**Usage:**
```bash
./sign_and_notarize.command [arm64|x86_64|universal]
```

**Features:**
- Automatically finds Developer ID certificate
- Signs all libraries, executables, and bundles
- Submits to Apple for notarization
- Waits for notarization to complete
- Staples the notarization ticket

### `create_dmg.command`
Creates a DMG file with:
- All three app bundles (FPCAtomic.app, FPCAtomicLauncher.app, FPCAtomicServer.app)
- Applications folder symlink
- README.txt with installation instructions
- Properly configured window layout

**Usage:**
```bash
./create_dmg.command [arm64|x86_64|universal]
```

**Output:**
- DMG files are created in `releases/` directory
- Filename format: `FPCAtomic-{VERSION}-{ARCH}.dmg` (if version available)
- Filename format: `FPCAtomic-{ARCH}.dmg` (if version not available)

## Troubleshooting

### "No Developer ID Application certificate found"
- Make sure you've created and installed the certificate in Keychain
- Check with: `security find-identity -v -p codesigning`
- The certificate name should contain "Developer ID Application"
- Or set `DEVELOPER_ID` in your `.env` file with the exact certificate name

### "APPLE_ID not set" or "APPLE_ID_PASSWORD not set"
- Make sure you've created a `.env` file in the project root
- Check that `APPLE_ID` and `APPLE_ID_PASSWORD` are set in the `.env` file
- Verify the `.env` file format: `KEY=value` (no spaces around `=`)
- Make sure you're using an app-specific password, not your regular Apple ID password

### "Notarization failed"
- Check your Apple ID credentials in `.env` file
- Make sure you're using an app-specific password, not your regular password
- Verify your Team ID is correct in `.env` file
- Check Apple Developer account status

### "Stapling failed"
- Notarization must complete successfully before stapling
- Wait a few minutes and try again if notarization just completed
- Check with: `xcrun notarytool history --apple-id YOUR_EMAIL --password PASSWORD --team-id TEAM_ID`

### "DMG creation failed"
- Make sure app bundles exist in `macos/app_{ARCH}/`
- Check disk space (DMG needs ~100-200MB free)
- Make sure `releases/` directory exists or can be created
- Try running with `sudo` if permission issues occur

## Testing the Release

### Verify Signing
```bash
codesign -dv --verbose=4 macos/app_arm64/FPCAtomic.app
codesign --verify --verbose macos/app_arm64/FPCAtomic.app
```

### Verify Notarization
```bash
spctl -a -vv -t install macos/app_arm64/FPCAtomic.app
xcrun stapler validate macos/app_arm64/FPCAtomic.app
```

### Test DMG
1. Double-click the DMG file
2. Verify it opens and shows the apps
3. Try dragging an app to Applications
4. Launch the app and verify it runs without security warnings

## GitHub Release

To create a GitHub release:

1. Create the DMG files for all architectures:
   ```bash
   ./create_release.command arm64
   ./create_release.command x86_64
   ./create_release.command universal
   ```

2. Go to GitHub Releases: https://github.com/YOUR_REPO/releases

3. Create a new release:
   - Tag: `v1.0.0` (or your version)
   - Title: `FPC Atomic v1.0.0 - macOS`
   - Description: Include installation instructions

4. Upload the DMG files from `releases/` directory:
   - `releases/FPCAtomic-1.0.0-arm64.dmg`
   - `releases/FPCAtomic-1.0.0-x86_64.dmg`
   - `releases/FPCAtomic-1.0.0-universal.dmg`

5. Users can download and install:
   - Open the DMG
   - Drag apps to Applications
   - Launch FPCAtomicLauncher.app

## Security Notes

- **Never commit** your Apple ID password or app-specific password to git
- Use environment variables or keychain for credentials
- App-specific passwords are safer than your main Apple ID password
- Developer ID certificates should be kept secure

## Additional Resources

- [Apple Code Signing Guide](https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/)
- [Notarization Guide](https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution)
- [DMG Creation Guide](https://developer.apple.com/library/archive/documentation/CoreFoundation/Conceptual/CFBundles/BundleTypes/BundleTypes.html)

