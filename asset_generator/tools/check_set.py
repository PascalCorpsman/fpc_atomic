"""
Checks a generated asset set against the engine contract.

  python -I asset_generator/tools/check_set.py <set dir> [<data dir>]

For every PNG in the set that also exists in the data dir (original files,
only used for the expected size), it reports:
  - size differs from the original
  - pink fringe: pixels close to fuchsia that are not fuchsia (anti aliasing
    against the transparent background, the engine shows them pink)
  - green dominant pixels (g > r and g > b) outside atomic/: the engine only
    recolors atomic sheets, elsewhere this is fine, so only reported as info
For .ani files it parses the header and the embedded BMP.
Exit code 1 if an error was found.
"""
import os
import struct
import sys

from PIL import Image


def near_fuchsia(r, g, b):
    return r > 180 and b > 180 and g < 90 and not (r == 255 and g == 0 and b == 255)


def check_png(path, rel, data_dir, errors, infos):
    im = Image.open(path)
    orig = os.path.join(data_dir, rel) if data_dir else None
    if orig and os.path.exists(orig):
        o = Image.open(orig)
        if o.size != im.size:
            errors.append(f"{rel}: size {im.size}, original {o.size}")
    rgb = im.convert("RGBA")
    px = rgb.load()
    w, h = rgb.size
    fringe = 0
    fuchsia = 0
    green = 0
    for y in range(h):
        for x in range(w):
            r, g, b, a = px[x, y]
            if r == 255 and g == 0 and b == 255:
                fuchsia += 1
            elif near_fuchsia(r, g, b):
                fringe += 1
            if g > r and g > b:
                green += 1
    if fringe > 0 and fuchsia > 0:  # opaque images (no fuchsia at all) may use pink on purpose
        errors.append(f"{rel}: {fringe} pink fringe pixels")
    if not rel.startswith("atomic/") and green > 0:
        infos.append(f"{rel}: {green} green dominant pixels (fine outside atomic/)")
    if rel.startswith("atomic/") and green == 0 and "shadow" not in rel:
        infos.append(f"{rel}: no green pixels, nothing gets the player color")


def check_ani(path, rel, errors):
    d = open(path, "rb").read()
    if d[4:12] != b"CANIFILE":
        errors.append(f"{rel}: no CANIFILE header")
        return
    p = 12 + 4
    n = struct.unpack("<i", d[p:p + 4])[0]
    p += 4
    for i in range(n):
        p += 8
        p += 1
        has_bmp = d[p]
        p += 1
        if has_bmp:
            size = struct.unpack("<I", d[p + 2:p + 6])[0]
            p += size
        p += 1 + 4 + 16
        ln = struct.unpack("<i", d[p:p + 4])[0]
        p += 4 + ln + 1 + 24
    if p != len(d):
        errors.append(f"{rel}: parsed {p} of {len(d)} bytes")


def main():
    set_dir = sys.argv[1]
    data_dir = sys.argv[2] if len(sys.argv) > 2 else os.path.join(os.path.dirname(__file__), "..", "..", "data")
    errors, infos = [], []
    count = 0
    for root, _, files in os.walk(set_dir):
        for f in sorted(files):
            path = os.path.join(root, f)
            rel = os.path.relpath(path, set_dir).replace("\\", "/")
            if f.lower().endswith(".png"):
                count += 1
                check_png(path, rel, data_dir, errors, infos)
            elif f.lower().endswith(".ani"):
                count += 1
                check_ani(path, rel, errors)
    for i in infos:
        print("info :", i)
    for e in errors:
        print("ERROR:", e)
    print(f"{count} files checked, {len(errors)} errors")
    sys.exit(1 if errors else 0)


if __name__ == "__main__":
    main()
