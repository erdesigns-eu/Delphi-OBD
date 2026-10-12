#!/usr/bin/env python3
"""Generate and check the string catalogues of OBD-Studio Personal.

The application's own texts are written in the source as
``Tr('personal.<page>.<name>', 'English text')`` (also ``TrFmt`` and
``TrPlural``). This tool collects those calls from
applications/obd-studio-personal/*.pas and writes the English catalogue
applications/obd-studio-personal/languages/en.json from the defaults, so the
English text lives in one place: the code.

  python3 tools/personal_strings.py           check en.json is up to date and
                                              audit every translation
  python3 tools/personal_strings.py --write   regenerate en.json
  python3 tools/personal_strings.py --missing CODE
                                              print the English strings a
                                              translation still lacks (JSON)

The other catalogues in that folder (one per language, same format as the
library catalogues in languages/) are audited with tools/i18n_check.py:
missing or extra keys, placeholders and plural categories. At run time the
program merges languages/<code>.json and languages/personal/<code>.json.
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import i18n_check  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
APP = ROOT / "applications" / "obd-studio-personal"
LANG_DIR = APP / "languages"
SCHEMA = ROOT / "languages" / "_schema" / "i18n-catalog.schema.json"
PREFIX = "personal."

CALL_RE = re.compile(r"\b(Tr|TrFmt|TrPlural)\s*\(")


def strip_comments(text: str) -> str:
    """Removes Pascal comments but keeps string literals intact."""
    out = []
    i, n = 0, len(text)
    while i < n:
        ch = text[i]
        if ch == "'":
            j = i + 1
            while j < n:
                if text[j] == "'":
                    if j + 1 < n and text[j + 1] == "'":
                        j += 2
                        continue
                    break
                j += 1
            out.append(text[i:j + 1])
            i = j + 1
        elif text.startswith("//", i):
            j = text.find("\n", i)
            i = n if j < 0 else j
        elif ch == "{":
            j = text.find("}", i)
            i = n if j < 0 else j + 1
        elif text.startswith("(*", i):
            j = text.find("*)", i)
            i = n if j < 0 else j + 2
        else:
            out.append(ch)
            i += 1
    return "".join(out)


def split_args(text: str, start: int) -> tuple[list[str], int]:
    """Splits the arguments of the call whose '(' is at text[start - 1]."""
    args, depth, i, begin = [], 0, start, start
    n = len(text)
    while i < n:
        ch = text[i]
        if ch == "'":
            j = i + 1
            while j < n:
                if text[j] == "'":
                    if j + 1 < n and text[j + 1] == "'":
                        j += 2
                        continue
                    break
                j += 1
            i = j + 1
            continue
        if ch in "([":
            depth += 1
        elif ch in ")]":
            if depth == 0:
                args.append(text[begin:i].strip())
                return args, i + 1
            depth -= 1
        elif ch == "," and depth == 0:
            args.append(text[begin:i].strip())
            begin = i + 1
        i += 1
    raise ValueError("unterminated call")


LITERAL_TOKEN = re.compile(r"\s*(?:'((?:[^']|'')*)'|#(\d+)|#\$([0-9A-Fa-f]+)|(\+))")


def literal(expr: str) -> str | None:
    """Value of a constant string expression ('a' + #13#10 + 'b'), or None
    when the expression is not constant."""
    pos, parts, expect_value = 0, [], True
    expr = expr.strip()
    if not expr:
        return None
    while pos < len(expr):
        m = LITERAL_TOKEN.match(expr, pos)
        if not m:
            return None
        if m.group(4):
            if expect_value:
                return None
            expect_value = True
        else:
            if m.group(1) is not None:
                parts.append(m.group(1).replace("''", "'"))
            elif m.group(2):
                parts.append(chr(int(m.group(2))))
            else:
                parts.append(chr(int(m.group(3), 16)))
            expect_value = False
        pos = m.end()
    return None if expect_value else "".join(parts)


def collect(source_dir: Path = APP) -> tuple[dict, list[str]]:
    """Returns ({key: str or {'one','other'}}, problems)."""
    strings: dict = {}
    where: dict[str, str] = {}
    problems: list[str] = []
    for path in sorted(source_dir.glob("*.pas")):
        text = strip_comments(path.read_text(encoding="utf-8-sig"))
        for m in CALL_RE.finditer(text):
            func = m.group(1)
            line = text.count("\n", 0, m.start()) + 1
            loc = f"{path.relative_to(ROOT)}:{line}"
            try:
                args, _ = split_args(text, m.end())
            except ValueError:
                problems.append(f"{loc}: unterminated {func} call")
                continue
            key = literal(args[0]) if args else None
            if key is None or not key.startswith(PREFIX):
                continue
            if func == "TrPlural":
                if len(args) != 4:
                    problems.append(f"{loc}: TrPlural needs key, count, one, other")
                    continue
                one, other = literal(args[2]), literal(args[3])
                if one is None or other is None:
                    problems.append(f"{loc}: {key}: the defaults must be string literals")
                    continue
                key = key if key.endswith(".plural") else key + ".plural"
                value: object = {"one": one, "other": other}
            else:
                if len(args) < 2:
                    problems.append(f"{loc}: {func} needs a key and an English default")
                    continue
                value = literal(args[1])
                if value is None:
                    problems.append(f"{loc}: {key}: the default must be a string literal")
                    continue
            if key in strings and strings[key] != value:
                problems.append(f"{loc}: {key} has a different English text than "
                                f"{where[key]}")
                continue
            strings[key] = value
            where.setdefault(key, loc)
    return dict(sorted(strings.items())), problems


def english_catalog(strings: dict) -> dict:
    return {
        "$schema": "../../../languages/_schema/i18n-catalog.schema.json",
        "language": "en",
        "name": "English",
        "englishName": "English",
        "version": 1,
        "strings": strings,
    }


def dump(doc: dict) -> str:
    return json.dumps(doc, indent=2, ensure_ascii=False) + "\n"


def audit_translations() -> list[str]:
    """Audits every catalogue in LANG_DIR against en.json and the schema."""
    issues: list[str] = []
    schema = i18n_check.read_json(SCHEMA)
    validator = None
    if i18n_check.Draft202012Validator is not None:
        validator = i18n_check.Draft202012Validator(schema)
    flattened = {}
    for path in sorted(LANG_DIR.glob("*.json")):
        try:
            doc = i18n_check.read_json(path)
            if validator is not None:
                validator.validate(doc)
            code = doc["language"]
            if path.stem != code:
                raise ValueError(f"file name must match language code {code}")
            if not (ROOT / "languages" / f"{code}.json").exists():
                raise ValueError(f"{code} has no library catalogue in languages/")
            flattened[code] = i18n_check.flatten_strings(doc["strings"], code)
        except Exception as exc:  # report every file, not only the first
            issues.append(f"{path.relative_to(ROOT)}: {exc}")
    if "en" in flattened:
        issues.extend(i18n_check.compare_catalogs(flattened))
    return issues


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--write", action="store_true", help="regenerate en.json")
    parser.add_argument("--missing", metavar="CODE",
                        help="print the English strings missing from CODE.json")
    args = parser.parse_args(argv)

    strings, problems = collect()
    for problem in problems:
        print(problem)
    if problems:
        return 1
    expected = dump(english_catalog(strings))
    en_path = LANG_DIR / "en.json"

    if args.missing:
        path = LANG_DIR / f"{args.missing}.json"
        have = {}
        if path.exists():
            have = i18n_check.read_json(path)["strings"]
        print(dump({k: v for k, v in strings.items() if k not in have}), end="")
        return 0

    if args.write:
        LANG_DIR.mkdir(parents=True, exist_ok=True)
        en_path.write_text(expected, encoding="utf-8", newline="\n")
        print(f"wrote {en_path.relative_to(ROOT)} ({len(strings)} strings)")
    elif not en_path.exists() or en_path.read_text(encoding="utf-8") != expected:
        print(f"{en_path.relative_to(ROOT)} is out of date: "
              "run python3 tools/personal_strings.py --write")
        return 1

    issues = audit_translations()
    languages = sorted(p.stem for p in LANG_DIR.glob("*.json"))
    print(f"OBD-Studio Personal strings: {len(strings)}; languages: "
          f"{', '.join(languages)}; issues: {len(issues)}")
    for issue in issues[:80]:
        print(issue)
    return 1 if issues else 0


if __name__ == "__main__":
    raise SystemExit(main())
