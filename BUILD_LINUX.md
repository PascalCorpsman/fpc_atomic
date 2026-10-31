# Building on Linux: GTK2 and GTK3

This page explains which LCL widgetset the Linux builds use, how to build with GTK3 and what is known about the differences between Windows, GTK2 and GTK3. The general build steps (dglopengl, bass, synapse, lnet, SDL2) are in the [README](README.md#what-needs-to-be-done-to-compile-the-code-).

## Contents

- [Which widgetset?](#which-widgetset)
- [Why the Lazarus packages of the distribution are not enough](#why-the-lazarus-packages-of-the-distribution-are-not-enough)
- [Building with GTK3](#building-with-gtk3)
- [Building with GTK2](#building-with-gtk2)
- [Which widgetset does a binary use?](#which-widgetset-does-a-binary-use)
- [Windows Subsystem for Linux (WSL)](#windows-subsystem-for-linux-wsl)
- [Known GTK3 issues](#known-gtk3-issues)
- [Why the dialogs look different on every platform](#why-the-dialogs-look-different-on-every-platform)

## Which widgetset?

| Widgetset | Status |
|---|---|
| GTK3 | Default on Linux and the future. The client and the OpenGL renderer are made for it (core profile, see `units/uopengl_graphikengine.pas`). |
| GTK2 | Deprecated. The launcher prints a compiler warning when built with GTK2, the GTK2 specific code (`{$IFDEF LCLGTK2}` in `launcher/atomic_launcher.lpr`) will be removed. |

The `.lpi` files do not set a widgetset, so `lazbuild` and the IDE use the default of the Lazarus installation. That is GTK2 for all Lazarus releases up to 4.x and for the Lazarus packages of the distributions, so without further steps you get a GTK2 build.

The server is a console program and does not use any widgetset.

## Why the Lazarus packages of the distribution are not enough

Debian and Ubuntu (checked with Ubuntu 26.04, Lazarus 4.4) only ship these LCL backends as packages:

- `lcl-gtk2`
- `lcl-qt5`
- `lcl-nogui`

There is no `lcl-gtk3` package. The GTK3 sources are included (`/usr/lib/lazarus/<version>/lcl/interfaces/gtk3`), but they are not compiled and the folder belongs to root.

The GTK3 backend of Lazarus 4.4 is also clearly behind the current development version. Built with 4.4, the launcher window was smaller than designed and the right and bottom parts were cut off. With Lazarus trunk (5.99) the same code shows the window in the correct size. Upstream uses Lazarus trunk too (`LCLVersion = '5.99.0.0'` in the `.lfm` files), so **build GTK3 with Lazarus trunk**, otherwise you hunt bugs that are already fixed.

## Building with GTK3

### 1. Packages

```bash
sudo apt install fpc libgtk-3-dev git make
```

The client additionally needs the libraries from the [README](README.md#what-needs-to-be-done-to-compile-the-code-), in particular `libbass.so` in `/usr/lib/`, otherwise linking fails with `cannot find -lbass`.

### 2. Lazarus trunk

Only `lazbuild` is needed, not the IDE. The folder is your own, a system Lazarus stays untouched.

```bash
git clone --depth 1 https://gitlab.com/freepascal.org/lazarus/lazarus.git ~/lazarus-trunk
make -C ~/lazarus-trunk lazbuild FPC=/usr/bin/fpc
~/lazarus-trunk/lazbuild --version     # 5.99
```

To update later: `git -C ~/lazarus-trunk pull`, then the `make` line again.

### 3. Own configuration

Use a separate configuration folder (`--pcp`), otherwise the trunk `lazbuild` changes the settings of an installed Lazarus IDE. To keep the commands short:

```bash
L=~/lazarus-trunk
LB="$L/lazbuild --lazarusdir=$L --pcp=$L/.config"
```

Register the lnet packages once in this configuration (client and server need them):

```bash
$LB --add-package-link /path/to/lnet/lazaruspackage/lnetbase.lpk
$LB --add-package-link /path/to/lnet/lazaruspackage/lnetvisual.lpk
```

### 4. Build

From the repository root:

```bash
OPT="--opt=-Fu$PWD/synapse --opt=-Fu$PWD/sdl2_for_pascal"
$LB --ws=gtk3 -B $OPT launcher/atomic_launcher.lpi
$LB --ws=gtk3 -B $OPT client/fpc_atomic.lpi
$LB -B $OPT server/atomic_server.lpi
```

The first build compiles the GTK3 LCL inside `~/lazarus-trunk`, this takes a moment.

Use `-B` whenever you switch between GTK2 and GTK3. The project units in `lib/` do not depend on the widgetset folder, without a full rebuild old units of the other widgetset can end up in the binary.

### In the Lazarus IDE

- **Project only:** Project → Project Options → Compiler Options → Additions and Overrides → *Set "LCLWidgetType"* → `gtk3`. This writes the setting into the `.lpi`, do not commit it.
- **IDE itself:** Tools → Configure "Build Lazarus" → LCL widget type `GTK 3`. This also changes the default for all projects.

## Building with GTK2

With the Lazarus packages of the distribution nothing has to be done, GTK2 is the default:

```bash
sudo apt install lazarus lcl-gtk2 libgtk2.0-dev
lazbuild -B --opt=-Fu$PWD/synapse --opt=-Fu$PWD/sdl2_for_pascal launcher/atomic_launcher.lpi
```

With the trunk `lazbuild` from above: `$LB --ws=gtk2 -B ...`.

## Which widgetset does a binary use?

```bash
ldd atomic_launcher | grep libgtk
```

- `libgtk-3.so.0` → GTK3
- `libgtk-x11-2.0.so.0` → GTK2

In the code: `{$IFDEF LCLGTK3}` / `{$IFDEF LCLGTK2}`.

## Windows Subsystem for Linux (WSL)

The Linux builds can be tested on Windows with WSL 2. The windows appear on the Windows desktop through WSLg.

- **Build in the Linux file system** (e.g. copy the sources to `~/src/fpc_atomic`), not under `/mnt/c/...`. That is much faster and keeps the Linux build output out of the Windows tree.
- **Windows PATH:** WSL appends the Windows `PATH`. If a Windows Lazarus / FPC is installed, the Lazarus makefiles find `fpc.exe` there and build Windows binaries (you get a `lazbuild.exe` instead of `lazbuild`). Therefore pass `FPC=/usr/bin/fpc` to `make`, or remove the Windows paths for the build:
  ```bash
  PATH=$(echo "$PATH" | tr : '\n' | grep -v '^/mnt/' | paste -sd:) make -C ~/lazarus-trunk lazbuild FPC=/usr/bin/fpc
  ```
- **Wayland or X11:** GTK3 runs as a native Wayland client in WSLg (compositor: Weston). GTK2 always uses X11 (XWayland). To run a GTK3 build through X11 as well:
  ```bash
  GDK_BACKEND=x11 ./atomic_launcher
  ```
  This helps to find out whether a problem comes from GTK3 itself or from the Wayland compositor of WSLg (see below). GTK3 opens a window even without `DISPLAY`, it finds the Wayland socket of WSLg by itself.
- **Invisible windows:** If WSLg falls back to copy mode (`rdp_allocate_shared_memory: Failed` in `/mnt/wslg/weston.log`), windows can stay invisible. Fix: `wsl --shutdown` on Windows, then start again.

## Known GTK3 issues

Observed with Lazarus trunk 5.99 (commit `0cb3dfab`, 2026-10-31) in WSLg on Windows 11. Not yet checked on a native Linux desktop, so it is open whether the cause is the GTK3 LCL or WSLg.

### Modal dialogs can get behind the main window

1. Open a modal dialog in the launcher (e.g. *Game options*).
2. Alt+Tab to another window and back.
3. The dialog is behind the main window for a moment, then it comes to the front again.

If the dialog is left with Alt+Tab, then clicked again and closed, all controls of the main window stay greyed out. The main window apparently does not get the focus back after the modal dialog ends.

To narrow it down: Check whether the same happens with `GDK_BACKEND=x11` and on a native Linux desktop. If it only happens under Wayland / WSLg, it is a compositor topic and can be ignored for the game.

### Wayland: resizing a shown window together with its position

When a form that is already shown gets a new size and position at once (`SetBounds(Left, Top, W, H)`, or a resize followed by a move), the first such change under Wayland is not applied: the window keeps a smaller size and the right and bottom part is cut off. Later changes work. With `GDK_BACKEND=x11` every change works.

The cause seems to be the Wayland support of the GTK3 LCL, which in Lazarus trunk compensates the client side decorations (shadow and title bar) with deferred resizes (`WindowSizeAllocate` in `lcl/interfaces/gtk3/gtk3widgets.pas`). The launcher is not affected: `FitLayout` only changes the size (`ClientWidth` / `ClientHeight`), never the position.

### Wayland: windows start at a different position each time

Under Wayland an application can not place its windows, the compositor decides. `Position = poScreenCenter` has no effect, the launcher appears somewhere else on every start. This is normal Wayland behaviour, with `GDK_BACKEND=x11` the window is centered.

### Lazarus 4.4: window too small

With the GTK3 LCL of Lazarus 4.4 the launcher window is shown smaller than its designed size (about 500 instead of 547 px wide), the right and the bottom part are cut off. Fixed in Lazarus trunk, see [above](#why-the-lazarus-packages-of-the-distribution-are-not-enough).

### Texts cut off in the dialogs (fixed)

GTK3 controls are larger than on Windows and GTK2, see the next section. With the fixed positions of the designer, texts overlapped or were cut off, e.g. in the launcher (French):

- Keyboard settings: the action labels ran into the key names.
- Game options: long labels were cut off by the edit fields, the map combo box was too narrow.
- Game mode and Mods: the description texts were cut off at the bottom, long mod names in the list too.
- Main window: the launch button was cut off at the bottom.

The launcher now fixes this at runtime with `FitLayout` (`units/ulayout.pas`), see the next section.

## Why the dialogs look different on every platform

The LCL does not draw the controls itself, it uses the native controls of the platform (Win32, GTK2, GTK3, ...). Each of them has its own font, its own padding and its own minimum sizes. GTK3 with the Adwaita theme, for example, has clearly taller edit fields, combo boxes and spin edits than Windows, and a different default font size. On top of that, the translations have different text lengths.

A layout with fixed `Left` / `Top` / `Width` / `Height` values is therefore only correct on the platform and in the language it was designed with. Pixel values per platform (`{$IFDEF}` around sizes) do not scale: every widgetset, theme, font size and translation would need its own values.

### The usual LCL way: auto layout

The LCL has a built-in solution, the *auto layout*: `AutoSize`, anchors (`AnchorSide...` with `BorderSpacing`), `ChildSizing` for rows and columns and `AutoSize` on the form. See the Lazarus wiki: [Autosize / Layout](https://wiki.freepascal.org/Autosize_/_Layout) and [Anchor Sides](https://wiki.freepascal.org/Anchor_Sides).

For the launcher this would mean rebuilding every dialog with anchors in the `.lfm` (the mods dialog alone has 14 pages), and the dialogs could no longer simply be arranged by dragging the controls in the designer.

### What the launcher does: `FitLayout`

The dialogs stay as they are designed, with fixed positions. `FitLayout(Form)` from `units/ulayout.pas` is called when a dialog is shown (and in the main window after a language change) and adapts the design to the widgetset, the font and the language:

- Every control with a text (labels, buttons, check boxes, edits, combo boxes, lists) gets the size the widgetset needs, but never less than in the designer. Edits keep their width, labels with `WordWrap` keep their width and get taller, combo boxes and lists get as wide as their longest item.
- Controls side by side in a row are pushed to the right, controls one above the other are pushed down. Controls that had the same `Left` (or `Top`) in the designer keep it in common, so columns stay columns. The gaps of the designer are the minimum gaps.
- Panels, group boxes, page controls and the window grow until the content fits. A window with a fixed size (`Constraints` min = max) keeps a fixed size, the new one.
- A label next to a taller control (edit, combo box, ...) is centered vertically to it. A control that ended at the same height as a neighbour in the designer (e.g. the list next to the page control) grows with it.
- Controls with `Align`, anchored to other controls or anchored right / bottom are not moved, the LCL places them.

`FitLayout` always calculates from the positions of the designer (they are stored on the first call), so it can be called again and again. Where everything already fits (e.g. Windows in English), nothing changes.

For new dialogs: design them as usual, keep some space between the controls and call `FitLayout(self)` in `OnShow` (after all captions are set).
