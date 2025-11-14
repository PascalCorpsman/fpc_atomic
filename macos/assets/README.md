# Assets Directory

This directory contains application assets that are not committed to the Git repository, typically due to licensing restrictions.

## Application Icon

### Option 1: Direct .icns file (Recommended)

Place your application icon file directly here:

- **Icon file**: `AtomicBomberIcon.icns`
- **Location**: Copy `AtomicBomberIcon.icns` to this directory (`macos/assets/AtomicBomberIcon.icns`)
- **Usage**: The build script (`../tools/build_app_bundles.command`) will automatically copy this icon to all `.app` bundles during the build process

### Option 2: Create .icns from PNG/JPG

If you only have a PNG or JPG image (like `AtomicBomberIcon.icon/Assets/bomberman-ikona-gradient.jpg`), you can create a `.icns` file:

1. **Using online tools**: Upload your PNG to an online converter like:
   - https://cloudconvert.com/png-to-icns
   - https://iconverticons.com/online/

2. **Using macOS tools** (requires creating an icon set):
   ```bash
   # Create iconset directory structure
   mkdir -p AtomicBomberIcon.iconset
   
   # Copy image to various sizes (macOS requires specific sizes)
   sips -z 16 16 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_16x16.png
   sips -z 32 32 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_16x16@2x.png
   sips -z 32 32 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_32x32.png
   sips -z 64 64 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_32x32@2x.png
   sips -z 128 128 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_128x128.png
   sips -z 256 256 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_128x128@2x.png
   sips -z 256 256 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_256x256.png
   sips -z 512 512 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_256x256@2x.png
   sips -z 512 512 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_512x512.png
   sips -z 1024 1024 bomberman-ikona-gradient.jpg --out AtomicBomberIcon.iconset/icon_512x512@2x.png
   
   # Convert iconset to .icns
   iconutil -c icns AtomicBomberIcon.iconset -o AtomicBomberIcon.icns
   
   # Move to assets directory
   mv AtomicBomberIcon.icns ../AtomicBomberIcon.icns
   ```

The icon will be placed in `Contents/Resources/AppIcon.icns` within each application bundle.

### Note
The icon file is excluded from Git (see `.gitignore`). You need to place it here manually on each machine where you build the application bundles.
