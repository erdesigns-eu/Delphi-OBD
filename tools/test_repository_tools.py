"""Regressions: missing icons, corrupt PNGs and stale/mismatched EV documents."""
import json
import struct
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import designtime_resources as resources
import ev_support_matrix as matrix

ROOT = Path(__file__).resolve().parents[1]


class ResourceTests(unittest.TestCase):
    def test_every_registered_class_has_exact_reproducible_resource(self):
        data, count = resources.build()
        self.assertEqual(count, 229)
        self.assertEqual(data, resources.TARGET.read_bytes())
        # The Win32 resource stream starts with the required 32-byte null header.
        self.assertEqual(struct.unpack_from('<II', data), (0, 32))

    def test_missing_component_is_a_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / 'src/DesignTime/ERD.Design.Registration.pas'
            source.parent.mkdir(parents=True)
            source.write_text("RegisterComponents('OBD', [TOBDMissing]);")
            with patch.object(resources, 'ROOT', root):
                with self.assertRaisesRegex(ValueError, 'Missing registered icons'):
                    resources.build()

    def test_corrupt_crc_and_truncation_are_rejected(self):
        raw = (resources.ASSETS / 'palette/TOBDCONNECTION.png').read_bytes()
        broken = bytearray(raw)
        broken[30] ^= 1
        with self.assertRaisesRegex(ValueError, 'CRC'):
            resources.verify_png(bytes(broken), 'broken', (24, 24))
        with self.assertRaises(ValueError):
            resources.verify_png(raw[:-7], 'truncated', (24, 24))

    def test_dimensions_are_checked(self):
        raw = (resources.ASSETS / 'palette/TOBDCONNECTION.png').read_bytes()
        with self.assertRaisesRegex(ValueError, 'expected'):
            resources.verify_png(raw, 'wrong-size', (16, 16))


class MatrixTests(unittest.TestCase):
    def test_generated_document_matches_catalogues_and_excludes_fixture(self):
        generated = matrix.render()
        self.assertEqual(generated, matrix.TARGET.read_text())
        self.assertEqual(sum(line.startswith('| ') for line in generated.splitlines()), 16)
        self.assertNotIn('| _stub-test |', generated)
        self.assertIn('**Not supported**', generated)

    def test_manifest_mismatch_is_a_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            folder = Path(directory)
            (folder / '_manifest.json').write_text(json.dumps({'vendor_files': [
                {'vendor': 'bmw', 'file': 'bmw.json', 'primary_source': None}]}))
            (folder / 'bmw.json').write_text(json.dumps({'vendor': 'other', 'fields': []}))
            with patch.object(matrix, 'CATALOGS', folder):
                with self.assertRaisesRegex(ValueError, 'vendor mismatch'):
                    matrix.render()


if __name__ == '__main__':
    unittest.main()
