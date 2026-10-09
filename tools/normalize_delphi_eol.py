#!/usr/bin/env python3
"""Apply Git's CRLF policy to existing tracked files without discarding edits."""
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def main():
    tracked = subprocess.run(
        ['git', 'ls-files', '-z'], cwd=ROOT, capture_output=True, check=True).stdout
    attrs = subprocess.run(
        ['git', 'check-attr', '-z', '--stdin', 'eol'], cwd=ROOT,
        input=tracked, capture_output=True, check=True).stdout.split(b'\0')
    changed = 0
    for name, attribute, value in zip(attrs[0::3], attrs[1::3], attrs[2::3]):
        if value != b'crlf':
            continue
        path = ROOT / name.decode('utf-8')
        if not path.is_file() or path.is_symlink():
            continue
        raw = path.read_bytes()
        if b'\0' in raw:
            raise ValueError(f'Refusing to normalize binary file: {path}')
        normalized = raw.replace(b'\r\n', b'\n').replace(b'\n', b'\r\n')
        if raw != normalized:
            path.write_bytes(normalized)
            changed += 1
    print(f'CRLF normalization: {changed} tracked files updated; source edits and BOMs preserved.')


if __name__ == '__main__':
    main()
