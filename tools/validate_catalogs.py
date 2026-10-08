#!/usr/bin/env python3
"""Offline audit of explicitly declared, locally available catalog schemas.

Install tools/requirements-validation.txt. Unknown/missing schemas are reported
as uncovered, never inferred from a filename. Use --require-coverage for a full
coverage gate. A passing partial audit does not establish data provenance or
ECU compatibility. External schema references never trigger network requests.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path

from jsonschema import Draft202012Validator
from referencing import Registry
from referencing.exceptions import NoSuchResource


def deny_remote(uri: str):
    raise NoSuchResource(ref=uri)


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"Duplicate JSON object key: {key}")
        result[key] = value
    return result


def reject_constant(value):
    raise ValueError(f"Non-finite JSON number: {value}")


def read_json(path: Path):
    return json.loads(path.read_text(encoding="utf-8"), object_pairs_hook=unique_object,
                      parse_constant=reject_constant)


def validate_manifest(root, path, value, report):
    if path.name != '_manifest.json' or path.parent.name != 'ev-battery':
        return
    seen = set()
    for index, entry in enumerate(value.get('vendor_files', [])):
        if not isinstance(entry, dict):
            continue
        filename, vendor = entry.get('file'), entry.get('vendor')
        message = None
        if not isinstance(filename, str) or not isinstance(vendor, str):
            continue
        candidate = (path.parent / filename).resolve()
        if filename in seen:
            message = 'Duplicate vendor catalog reference'
        elif not candidate.is_relative_to(path.parent.resolve()) or not candidate.is_file():
            message = 'Vendor catalog reference does not exist inside ev-battery'
        else:
            try:
                catalog = read_json(candidate)
                if not isinstance(catalog, dict) or catalog.get('vendor') != vendor:
                    message = 'Manifest vendor does not match referenced catalog'
            except (ValueError, OSError) as exc:
                message = str(exc)
        seen.add(filename)
        if message:
            report['violations'].append({'file': str(path.relative_to(root)),
                'path': f'/vendor_files/{index}', 'message': message})


def audit(root: Path):
    root = root.resolve()
    schemas = {}
    registry = Registry(retrieve=deny_remote)
    report = {"checked": [], "uncovered": [], "violations": []}
    for path in sorted((root / "_schema").glob("*.json")):
        schema = read_json(path)
        Draft202012Validator.check_schema(schema)
        if "$id" in schema:
            if schema["$id"] in schemas:
                raise ValueError(f"Duplicate schema ID: {schema['$id']}")
            schemas[schema["$id"]] = (path, schema)
    for path in sorted(root.rglob("*.json")):
        if "_schema" in path.relative_to(root).parts:
            continue
        relative = str(path.relative_to(root))
        try:
            value = read_json(path)
        except (ValueError, OSError) as exc:
            report["violations"].append({"file": relative, "path": "", "message": str(exc)})
            continue
        if isinstance(value, dict):
            validate_manifest(root, path, value, report)
        declaration = value.get("$schema", "") if isinstance(value, dict) else ""
        target = schemas.get(declaration) if isinstance(declaration, str) else None
        if isinstance(declaration, str) and declaration.startswith("."):
            candidate = (path.parent / declaration).resolve()
            if candidate.is_relative_to(root) and candidate.is_file():
                target = (candidate, read_json(candidate))
        if target is None:
            report["uncovered"].append({"file": relative, "schema": declaration})
            continue
        schema_path, schema = target
        Draft202012Validator.check_schema(schema)
        report["checked"].append({"file": relative, "schema": str(schema_path.relative_to(root))})
        validator = Draft202012Validator(schema, registry=registry)
        for error in validator.iter_errors(value):
            report["violations"].append({
                "file": relative,
                "path": "/" + "/".join(str(part).replace("~", "~0").replace("/", "~1")
                                         for part in error.absolute_path),
                "message": error.message,
            })
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path,
                        default=Path(__file__).resolve().parents[1] / "catalogs")
    parser.add_argument("--report", type=Path, help="Write the full JSON audit")
    parser.add_argument("--require-coverage", action="store_true")
    args = parser.parse_args()
    report = audit(args.root)
    if args.report:
        args.report.write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n",
                               encoding="utf-8")
    print(f"Schema checked: {len(report['checked'])}; uncovered: {len(report['uncovered'])}; "
          f"violations: {len(report['violations'])}")
    for issue in report["violations"][:10]:
        print(f"{issue['file']} {issue['path']}: {issue['message']}")
    return int(bool(report["violations"] or (args.require_coverage and report["uncovered"])))


if __name__ == "__main__":
    raise SystemExit(main())
