#!/usr/bin/env python3
"""Validate Delphi-OBD i18n catalogues and source key usage."""
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

try:
    from jsonschema import Draft202012Validator
except Exception:  # pragma: no cover - validation environments install jsonschema
    Draft202012Validator = None

ROOT = Path(__file__).resolve().parents[1]
PLACEHOLDER_RE = re.compile(r"%(?!%)(?:[-+0 #]*\d*)?(?:\.\d+)?[a-zA-Z]")
KEY_RE = re.compile(r"\b(Tr|TrFmt|TrPlural)\s*\(\s*'([^']+)'")
OBD_KEY_RE = re.compile(r"\bOBDI18n\s*\.\s*(Tr|TrFmt|TrPlural)\s*\(\s*'([^']+)'")


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"Duplicate JSON object key: {key}")
        result[key] = value
    return result


def read_json(path: Path):
    raw = path.read_bytes()
    if raw.startswith(b"\xef\xbb\xbf"):
        raise ValueError(f"{path}: UTF-8 BOM is not allowed")
    return json.loads(raw.decode("utf-8"), object_pairs_hook=unique_object)


def placeholders(text: str) -> tuple[str, ...]:
    return tuple(match.group(0) for match in PLACEHOLDER_RE.finditer(text))


# CLDR cardinal plural categories that whole numbers reach, per language.
# Must match OBDPluralCategory in src/Core/ERD.I18n.pas. Fraction-only
# categories (for example "many" in Czech) and the French, Spanish,
# Italian, Portuguese and Catalan "many" for exact millions are not used.
PLURAL_CATEGORIES = {
    "lv": ("zero", "one", "other"),
    "lt": ("one", "few", "other"),
    "cs": ("one", "few", "other"),
    "sk": ("one", "few", "other"),
    "hr": ("one", "few", "other"),
    "sr": ("one", "few", "other"),
    "bs": ("one", "few", "other"),
    "ro": ("one", "few", "other"),
    "pl": ("one", "few", "many", "other"),
    "ru": ("one", "few", "many", "other"),
    "uk": ("one", "few", "many", "other"),
    "be": ("one", "few", "many", "other"),
    "sl": ("one", "two", "few", "other"),
    "ga": ("one", "two", "few", "many", "other"),
    "mt": ("one", "two", "few", "many", "other"),
}
DEFAULT_PLURAL_CATEGORIES = ("one", "other")
ALL_PLURAL_CATEGORIES = ("zero", "one", "two", "few", "many", "other")


def plural_categories(code: str) -> tuple[str, ...]:
    return PLURAL_CATEGORIES.get(code.split("-")[0], DEFAULT_PLURAL_CATEGORIES)


def flatten_strings(strings: dict, code: str = "en") -> dict[str, str]:
    flat = {}
    expected = set(plural_categories(code))
    for key, value in strings.items():
        if isinstance(value, str):
            flat[key] = value
        elif isinstance(value, dict):
            if set(value) != expected:
                raise ValueError(
                    f"{code}: {key}: plural entries must contain exactly "
                    f"{', '.join(c for c in ALL_PLURAL_CATEGORIES if c in expected)}")
            for category in ALL_PLURAL_CATEGORIES:
                if category in value:
                    flat[key + "." + category] = value[category]
        else:
            raise ValueError(f"{key}: value must be a string or plural object")
    return flat


def load_catalogs(language_dir: Path):
    schema = None
    schema_path = language_dir / "_schema" / "i18n-catalog.schema.json"
    if schema_path.exists():
        schema = read_json(schema_path)
        if Draft202012Validator is not None:
            Draft202012Validator.check_schema(schema)
    catalogs = {}
    flattened = {}
    for path in sorted(language_dir.glob("*.json")):
        doc = read_json(path)
        if schema is not None and Draft202012Validator is not None:
            Draft202012Validator(schema).validate(doc)
        code = doc.get("language")
        if not isinstance(code, str) or not code:
            raise ValueError(f"{path}: language is required")
        strings = doc.get("strings")
        if not isinstance(strings, dict):
            raise ValueError(f"{path}: strings must be an object")
        if path.stem != code:
            raise ValueError(f"{path}: file name must match language code {code}")
        catalogs[code] = doc
        flattened[code] = flatten_strings(strings, code)
    if "en" not in catalogs:
        raise ValueError("languages/en.json is required")
    return catalogs, flattened


def base_key(key: str) -> str:
    """Strips the plural category from a flattened plural key."""
    return key.rpartition(".")[0] if ".plural." in key else key


def english_key(key: str, english: dict[str, str]) -> str | None:
    """Maps a flattened key to its English counterpart (plural forms map
    to the English "other" form)."""
    if key in english:
        return key
    base, _, category = key.rpartition(".")
    if category in ALL_PLURAL_CATEGORIES and base + ".other" in english:
        return base + ".other"
    return None


def compare_catalogs(flattened: dict[str, dict[str, str]]):
    issues = []
    english = flattened["en"]
    en_base = {base_key(key) for key in english}
    for code, strings in sorted(flattened.items()):
        bases = {base_key(key) for key in strings}
        for key in sorted(en_base - bases):
            issues.append(f"{code}: missing key {key}")
        for key in sorted(bases - en_base):
            issues.append(f"{code}: extra key {key}")
        for key, text in sorted(strings.items()):
            source = english_key(key, english)
            if source is None:
                continue
            expected = placeholders(english[source])
            actual = placeholders(text)
            if expected != actual:
                issues.append(f"{code}: placeholder mismatch for {key}: {actual} != {expected}")
    return issues


def strip_pascal_comments(text: str) -> str:
    text = re.sub(r"\(\*.*?\*\)", "", text, flags=re.S)
    text = re.sub(r"\{.*?\}", "", text, flags=re.S)
    text = re.sub(r"//.*", "", text)
    return text


def scan_source_keys(source_root: Path):
    keys: dict[str, set[Path]] = {}
    for path in sorted(source_root.rglob("*.pas")):
        if path.name == "HEADER.template.pas":
            continue
        try:
            text = strip_pascal_comments(path.read_text(encoding="utf-8-sig"))
        except UnicodeDecodeError:
            text = strip_pascal_comments(path.read_text(encoding="cp1252"))
        for pattern in (KEY_RE, OBD_KEY_RE):
            for match in pattern.finditer(text):
                func, key = match.group(1), match.group(2)
                if func == "TrPlural" and not key.endswith(".plural"):
                    key = key + ".plural"
                keys.setdefault(key, set()).add(path)
    return keys


def audit(language_dir: Path, source_root: Path):
    catalogs, flattened = load_catalogs(language_dir)
    issues = compare_catalogs(flattened)
    used = scan_source_keys(source_root)
    english = flattened["en"]
    for key, paths in sorted(used.items()):
        if key not in english and key + ".one" not in english:
            rel = ", ".join(str(path.relative_to(ROOT)) if path.is_relative_to(ROOT) else str(path)
                            for path in sorted(paths))
            issues.append(f"source key missing from en.json: {key} ({rel})")
    return {
        "languages": sorted(catalogs),
        "string_count": len(catalogs["en"]["strings"]),
        "flat_count": len(flattened["en"]),
        "used_keys": len(used),
        "issues": issues,
    }


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--languages", type=Path, default=ROOT / "languages")
    parser.add_argument("--source", type=Path, default=ROOT / "src")
    args = parser.parse_args(argv)
    try:
        report = audit(args.languages, args.source)
    except Exception as exc:
        print(f"i18n check failed: {exc}")
        return 1
    print(
        "Languages: {langs}; strings: {strings}; source keys: {used}; issues: {issues}".format(
            langs=", ".join(report["languages"]),
            strings=report["string_count"],
            used=report["used_keys"],
            issues=len(report["issues"]),
        )
    )
    for issue in report["issues"][:50]:
        print(issue)
    return 1 if report["issues"] else 0


if __name__ == "__main__":
    raise SystemExit(main())
