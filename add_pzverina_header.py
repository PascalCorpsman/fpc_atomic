#!/usr/bin/env python3
"""
Script to add Pavel Zverina header to files modified by him.
The header will be inserted after the original copyright header (line 14).
"""

import os
import re
import sys

# List of files modified by Pavel Zverina (from git log)
MODIFIED_FILES = [
    "ai/ai.lpi",
    "cd_data_extractor_src/cd_data_extractor.lpi",
    "cd_data_extractor_src/cd_data_extractor.lpr",
    "cd_data_extractor_src/cd_data_extractor_nogui.lpi",
    "cd_data_extractor_src/ucdextractor.pas",
    "cd_data_extractor_src/unit1.pas",
    "client/fpc_atomic.lpi",
    "client/fpc_atomic.lpr",
    "client/uatomic.pas",
    "client/uearlylog.pas",
    "client/ugame.pas",
    "client/uloaderdialog.pas",
    "client/unit1.pas",
    "client/uscreens.pas",
    "client/usounds.pas",
    "launcher/atomic_launcher.lpi",
    "launcher/atomic_launcher.lpr",
    "launcher/ulauncher.pas",
    "launcher/unit1.pas",
    "server/atomic_server.lpi",
    "server/atomic_server.lpr",
    "server/uai.pas",
    "server/uatomic_server.pas",
    "units/bass.pas",
    "units/sdl2_for_pascal/sdl2.pas",
    "units/sdl2_for_pascal/sdl_runtime_linking.inc",
    "units/uatomic_common.pas",
    "units/uatomic_field.pas",
    "units/uchunkmanager.pas",
    "units/uip.pas",
    "units/ulogger.pas",
    "units/uopengl_ascii_font.pas",
    "units/uopengl_graphikengine.pas",
    "units/usdl_joystick.pas",
    "units/usynapsedownloader.pas",
]

# Header template to insert
NEW_HEADER = """(*                                                                            *)
(* Modified by  : Pavel Zverina                                               *)
(* Note         : This file has been modified while preserving the original   *)
(*                authorship and license terms.                                *)
(*                                                                            *)
"""

def find_header_end(lines):
    """
    Find the end of the original header block.
    Headers typically end with a line like (******************************************************************************)
    """
    # Skip XML files (.lpi files)
    if lines and ('<?xml' in lines[0] or '<CONFIG>' in lines[0] or '<ProjectOptions>' in lines[0]):
        return None
    
    # Look for the pattern that marks the end of header (line with 79 asterisks)
    for i, line in enumerate(lines):
        stripped = line.strip()
        # Match pattern like (******************************************************************************)
        if re.match(r'^\(\*+\*\)$', stripped) and len(stripped) >= 79:
            # Check if next line is empty or starts code (Unit, Program, etc.)
            if i + 1 < len(lines):
                next_line = lines[i + 1].strip()
                if (next_line == '' or 
                    next_line.startswith('Unit ') or 
                    next_line.startswith('Program ') or
                    next_line.startswith('{$') or
                    next_line.startswith('Library ')):
                    return i + 1
            else:
                return i + 1
    
    # Fallback: Look for common patterns like Unit, Program, etc. right after header
    for i in range(10, min(100, len(lines))):
        stripped = lines[i].strip()
        if (stripped.startswith('Unit ') or 
            stripped.startswith('Program ') or
            (stripped.startswith('{$') and i > 5)):
            # Go back to find the closing comment
            for j in range(i-1, max(0, i-10), -1):
                line_stripped = lines[j].strip()
                if re.match(r'^\(\*+\*\)$', line_stripped) and len(line_stripped) >= 79:
                    return j + 1
            # If no closing found, assume header ends before this line
            return i
    return None

def has_new_header(lines):
    """Check if file already has the new header."""
    content = '\n'.join(lines)
    return 'Modified by  : Pavel Zverina' in content

def add_header_to_file(filepath):
    """Add the new header to a file if it doesn't already have it."""
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"Error reading {filepath}: {e}", file=sys.stderr)
        return False
    
    if has_new_header(lines):
        print(f"✓ {filepath} - already has header, skipping")
        return True
    
    header_end = find_header_end(lines)
    if header_end is None:
        print(f"⚠ {filepath} - could not find header end, skipping")
        return False
    
    # Insert new header after original header
    new_lines = lines[:header_end] + [NEW_HEADER] + lines[header_end:]
    
    try:
        with open(filepath, 'w', encoding='utf-8', errors='ignore') as f:
            f.writelines(new_lines)
        print(f"✓ {filepath} - header added at line {header_end + 1}")
        return True
    except Exception as e:
        print(f"✗ {filepath} - error writing: {e}", file=sys.stderr)
        return False

def main():
    base_dir = os.path.dirname(os.path.abspath(__file__))
    processed = 0
    skipped = 0
    errors = 0
    
    for rel_path in MODIFIED_FILES:
        filepath = os.path.join(base_dir, rel_path)
        if not os.path.exists(filepath):
            print(f"⚠ {rel_path} - file not found, skipping")
            skipped += 1
            continue
        
        if add_header_to_file(filepath):
            processed += 1
        else:
            errors += 1
    
    print(f"\nSummary: {processed} processed, {skipped} skipped, {errors} errors")

if __name__ == '__main__':
    main()

