"""Board importer regressions against the actual manufacturing exports."""
import copy
import json
import math
from pathlib import Path
import tempfile
import unittest
import zipfile
import import_pcb

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'demo/pcb/source/board.epro'
PLACEMENTS = ROOT / 'demo/pcb/source/placements.csv'

class PCBImportTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.board = import_pcb.import_board(SOURCE, PLACEMENTS)

    def test_export_reproduces_committed_package(self):
        expected = json.loads((ROOT / 'demo/pcb/board.json').read_text())
        self.assertEqual(self.board, expected)
        self.assertEqual(len(self.board['components']), 135)
        self.assertEqual(len(self.board['pads']), 496)

    def test_all_output_routes_are_verified(self):
        channels = {c['pin']: c for c in self.board['channels']}
        self.assertEqual(len(channels), 28)
        self.assertEqual(channels['U7.2']['gpio'], 'GPIO4')
        self.assertEqual(channels['U7.2']['route'], 'U6.4 → U9.2/3 → R17 → U7.2')
        pins = {p['id']: p for p in self.board['pads']}
        self.assertEqual([pins[f'U7.{n}']['net'] for n in (1, 2, 3)], ['12V', 'OUT_GPIO4', 'GND'])
        self.assertNotIn('SW1.1', channels)
        self.assertNotIn('SW2.1', channels)
        self.assertNotIn('U7.1', channels)

    def test_rotated_pin_positions_and_rounded_outline(self):
        # C1 at 90 degrees and C4 at -90 independently reconcile with pin-1 export.
        pins = {p['id']: p for p in self.board['pads']}
        self.assertAlmostEqual(pins['C1.1']['position'][1], 39.405, places=3)
        self.assertAlmostEqual(pins['C4.1']['position'][1], 39.989, places=3)
        self.assertEqual(len(self.board['outline']), 8)
        self.assertTrue(any(len(p) > 10 for p in self.board['outline']))
        for path in self.board['outline']:
            for x, y in path:
                self.assertTrue(-.01 <= x <= 140.01 and -.01 <= y <= 70.01)

    def test_duplicate_exposed_pad_keeps_distinct_geometry(self):
        exposed = [p for p in self.board['pads'] if p['id'] == 'U6.41']
        self.assertGreater(len(exposed), 1)
        self.assertEqual(len({p['shape_id'] for p in exposed}), len(exposed))
        self.assertEqual({p['net'] for p in exposed}, {'GND'})

    def test_mismatched_placement_fails(self):
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / 'placements.csv'
            text = PLACEMENTS.read_text(encoding='utf-16').replace('26.67mm', '26.97mm', 1)
            bad.write_text(text, encoding='utf-16')
            with self.assertRaisesRegex(ValueError, 'Placement mismatch: C1'):
                import_pcb.import_board(SOURCE, bad)

    def test_broken_buffer_route_fails(self):
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / 'broken.epro'
            with zipfile.ZipFile(SOURCE) as source, zipfile.ZipFile(bad, 'w') as target:
                for name in source.namelist():
                    data = source.read(name)
                    if name.endswith('.epcb'):
                        data = data.replace(b'"GPIO4"', b'"GPIO999"')
                    target.writestr(name, data)
            with self.assertRaisesRegex(ValueError, 'Unresolved/ambiguous buffered route: U7'):
                import_pcb.import_board(bad, PLACEMENTS)

if __name__ == '__main__':
    unittest.main()
