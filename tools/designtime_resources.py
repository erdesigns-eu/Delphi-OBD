#!/usr/bin/env python3
"""Build/check Windows .res data from tracked PNGs without image conversion."""
import argparse
import json
import re
import struct
import zlib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ASSETS = ROOT / 'assets/designtime'
TARGET = ROOT / 'src/DesignTime/ERD.Design.Icons.res'


def align(data):
    return data + b'\0' * (-len(data) % 4)


def identifier(value):
    if isinstance(value, int):
        return struct.pack('<HH', 0xFFFF, value)
    return value.encode('utf-16le') + b'\0\0'


def resource(kind, name, payload):
    names = align(identifier(kind) + identifier(name))
    # DataVersion, MemoryFlags (MOVEABLE|PURE), LanguageId, Version, Characteristics.
    header = names + struct.pack('<IHHII', 0, 0x0030, 0, 0, 0)
    return align(struct.pack('<II', len(payload), len(header) + 8) + header + payload)


def verify_png(raw, name, expected):
    """Check CRCs and the bounded native RGBA stream, without changing pixels."""
    if raw[:8] != b'\x89PNG\r\n\x1a\n' or raw[12:16] != b'IHDR':
        raise ValueError('Resource is not a PNG: ' + name)
    width, height = struct.unpack_from('>II', raw, 16)
    if (width, height) != expected:
        raise ValueError(f'{name}: expected {expected}, found {(width, height)}')
    if raw[24:29] != bytes([8, 6, 0, 0, 0]):
        raise ValueError('Native PNG must be noninterlaced 8-bit RGBA: ' + name)
    offset = 8
    compressed = bytearray()
    ended = False
    while offset + 12 <= len(raw):
        length = struct.unpack_from('>I', raw, offset)[0]
        end = offset + 12 + length
        if end > len(raw):
            raise ValueError('Truncated PNG chunk: ' + name)
        chunk = raw[offset + 4:offset + 8 + length]
        crc = struct.unpack_from('>I', raw, offset + 8 + length)[0]
        if zlib.crc32(chunk) != crc:
            raise ValueError('PNG CRC mismatch: ' + name)
        if chunk[:4] == b'IDAT':
            compressed.extend(chunk[4:])
        if chunk[:4] == b'IEND':
            ended = True
            if length or end != len(raw):
                raise ValueError('Invalid PNG end: ' + name)
            break
        offset = end
    if not ended:
        raise ValueError('Missing PNG end: ' + name)
    size = height * (width * 4 + 1)
    decoder = zlib.decompressobj()
    pixels = decoder.decompress(bytes(compressed), size + 1)
    if len(pixels) != size or not decoder.eof or decoder.unused_data:
        raise ValueError('Invalid PNG pixel stream: ' + name)
    if any(pixels[row * (width * 4 + 1)] > 4 for row in range(height)):
        raise ValueError('Invalid PNG row filter: ' + name)


def build():
    manifest = json.loads((ASSETS / 'resources.json').read_text(encoding='utf-8'))
    source = (ROOT / 'src/DesignTime/ERD.Design.Registration.pas').read_text(encoding='utf-8')
    registered = {name.upper() for block in re.findall(
        r"RegisterComponents\('[^']+',\s*\[(.*?)\]\);", source, re.S)
        for name in re.findall(r'\bTOBD\w+', block)}
    missing = registered - manifest.keys()
    if missing:
        raise ValueError('Missing registered icons: ' + ', '.join(sorted(missing)))
    if set(manifest) != registered:
        raise ValueError('Resource manifest contains missing or unregistered entries')
    result = struct.pack('<IIHHHHIHHII', 0, 32, 0xFFFF, 0, 0xFFFF, 0, 0, 0, 0, 0, 0)
    for name, entry in sorted(manifest.items()):
        path = (ASSETS / entry['file']).resolve()
        if ASSETS.resolve() not in path.parents:
            raise ValueError('Resource path escapes assets directory: ' + name)
        raw = path.read_bytes()
        expected = (24, 24)
        verify_png(raw, name, expected)
        expected_kind = 'PNG'
        if entry['type'] != expected_kind:
            raise ValueError('Wrong IDE resource type: ' + name)
        if 'alias_of' in entry:
            original = manifest[entry['alias_of']]
            if original['file'] != entry['file'] or original['type'] != entry['type']:
                raise ValueError('Icon alias does not match its original: ' + name)
        result += resource(entry['type'], name, raw)
    return result, len(registered)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--write', action='store_true', help='rebuild the tracked .res file')
    args = parser.parse_args()
    try:
        data, count = build()
        if args.write:
            TARGET.write_bytes(data)
        elif TARGET.read_bytes() != data:
            raise ValueError('Design-time .res is stale; run this tool with --write')
    except (ValueError, OSError, KeyError, struct.error, zlib.error) as exc:
        parser.exit(1, f'{exc}\n')
    print(f'{count} registered component icons verified.')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
