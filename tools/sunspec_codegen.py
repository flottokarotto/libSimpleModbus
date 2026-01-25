#!/usr/bin/env python3
"""
SunSpec Ada Code Generator

Generates Ada register offset constants from official SunSpec JSON models.
Source: https://github.com/sunspec/models

Usage:
    python sunspec_codegen.py <model_id> [model_id ...]
    python sunspec_codegen.py 203      # Generate Model 203 (3-phase meter)
    python sunspec_codegen.py 101 103  # Generate Models 101 and 103

Output is written to stdout as Ada code.
"""

import json
import sys
import urllib.request
import urllib.error
from typing import Any

SUNSPEC_BASE_URL = "https://raw.githubusercontent.com/sunspec/models/master/json"


def fetch_model(model_id: int) -> dict[str, Any]:
    """Fetch SunSpec model JSON from GitHub."""
    url = f"{SUNSPEC_BASE_URL}/model_{model_id}.json"
    try:
        with urllib.request.urlopen(url, timeout=10) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        print(f"-- HTTP error fetching model {model_id}: {e.code} {e.reason}", file=sys.stderr)
        sys.exit(1)
    except urllib.error.URLError as e:
        print(f"-- Network error fetching model {model_id}: {e.reason}", file=sys.stderr)
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"-- JSON parse error for model {model_id}: {e}", file=sys.stderr)
        sys.exit(1)
    except TimeoutError:
        print(f"-- Timeout fetching model {model_id}", file=sys.stderr)
        sys.exit(1)


def ada_name(name: str) -> str:
    """Convert SunSpec name to Ada constant name."""
    # Add Reg_ prefix, convert to proper case
    result = "Reg_" + name
    return result


def ada_type_comment(point: dict[str, Any]) -> str:
    """Generate Ada comment describing the point type."""
    ptype = point.get("type", "unknown")
    size = point.get("size", 1)
    units = point.get("units", "")
    sf = point.get("sf", "")
    mandatory = "M" if point.get("mandatory") == "M" else ""

    parts = [ptype]
    if size > 1:
        parts.append(f"size={size}")
    if units:
        parts.append(units)
    if sf:
        parts.append(f"SF={sf}")
    if mandatory:
        parts.append("mandatory")

    return "  --  " + ", ".join(parts)


def generate_ada_constants(model: dict[str, Any]) -> str:
    """Generate Ada constant declarations from SunSpec model."""
    model_id = model.get("id", 0)
    group = model.get("group", {})
    group_name = group.get("name", f"model_{model_id}")
    label = group.get("label", group_name)
    points = group.get("points", [])

    lines = []
    lines.append(f"   --  SunSpec Model {model_id}: {label}")
    lines.append(f"   --  Generated from: {SUNSPEC_BASE_URL}/model_{model_id}.json")
    lines.append("")

    # Calculate offsets from sizes (JSON doesn't include offsets)
    offset = 0
    total_size = 0

    for point in points:
        name = point.get("name", "")
        size = point.get("size", 1)

        # Skip standard header fields (ID=0, L=1)
        if name in ("ID", "L"):
            offset += size
            total_size += size
            continue

        const_name = ada_name(name)
        type_comment = ada_type_comment(point)

        # Pad constant name for alignment
        padded_name = const_name.ljust(20)
        lines.append(f"   {padded_name}: constant := {offset};{type_comment}")

        offset += size
        total_size += size

    # Add total size comment at the top
    lines.insert(2, f"   --  Total registers: {total_size} (header: 2, data: {total_size - 2})")

    return "\n".join(lines)


def generate_ada_record(model: dict[str, Any]) -> str:
    """Generate Ada record type from SunSpec model (optional)."""
    # This could be extended to generate full record types
    # For now, just generate constants
    return ""


def validate_against_file(model: dict[str, Any], ada_file: str) -> tuple[list[str], list[str]]:
    """Validate generated offsets against existing Ada file.

    Returns (errors, warnings) where:
    - errors: Offsets in Ada file that don't match JSON
    - warnings: Offsets in JSON not present in Ada file (optional fields)
    """
    import re

    errors = []
    warnings = []
    group = model.get("group", {})
    points = group.get("points", [])

    # Read existing Ada file
    try:
        with open(ada_file, "r") as f:
            ada_content = f.read()
    except FileNotFoundError:
        return [f"File not found: {ada_file}"], []

    # Build map of expected offsets
    expected_offsets = {}
    offset = 0
    for point in points:
        name = point.get("name", "")
        size = point.get("size", 1)

        if name not in ("ID", "L"):
            expected_offsets[offset] = name

        offset += size

    # Check all offsets defined in Ada file
    for match in re.finditer(r"Reg_\w+\s*:\s*constant\s*:=\s*(\d+);", ada_content):
        ada_offset = int(match.group(1))
        if ada_offset not in expected_offsets:
            errors.append(f"Offset {ada_offset} in Ada file not in SunSpec JSON")

    # Check which SunSpec offsets are missing in Ada (just warnings)
    for exp_offset, name in expected_offsets.items():
        pattern = rf": constant := {exp_offset};"
        if not re.search(pattern, ada_content):
            warnings.append(f"Offset {exp_offset} ({name}) not implemented")

    return errors, warnings


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    # Check for --validate flag
    if sys.argv[1] == "--validate":
        if len(sys.argv) < 4:
            print("Usage: sunspec_codegen.py --validate <model_id> <ada_file>")
            sys.exit(1)
        model_id = int(sys.argv[2])
        ada_file = sys.argv[3]
        model = fetch_model(model_id)
        errors, warnings = validate_against_file(model, ada_file)
        if errors:
            print(f"Validation FAILED for model {model_id}:")
            for e in errors:
                print(f"  ERROR: {e}")
            sys.exit(1)
        else:
            print(f"Validation OK: Model {model_id} - all Ada offsets match SunSpec JSON")
            if warnings:
                print(f"  ({len(warnings)} optional fields not implemented)")
            sys.exit(0)

    print("--  SunSpec Register Offsets - Auto-generated")
    print("--  Do not edit manually - regenerate from JSON source")
    print("")

    for arg in sys.argv[1:]:
        try:
            model_id = int(arg)
        except ValueError:
            print(f"-- Invalid model ID: {arg}", file=sys.stderr)
            continue

        model = fetch_model(model_id)
        ada_code = generate_ada_constants(model)
        print(ada_code)
        print("")


if __name__ == "__main__":
    main()
