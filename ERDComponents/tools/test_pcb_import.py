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
        self.assertEqual(self.board['appearance']['board_color'], '#17624A')
        self.assertEqual(self.board['appearance']['silkscreen_color'], '#F1F5EB')

    def test_assembled_bodies_and_terminal_groups(self):
        components = {c['reference']: c for c in self.board['components']}
        self.assertTrue(all(len(c['body']) >= 4 for c in components.values()))
        self.assertEqual(sum(c['role'] == 'terminal' for c in components.values()), 29)
        self.assertEqual(len(self.board['terminals']), 28)
        for terminal in self.board['terminals']:
            self.assertEqual(components[terminal['id']]['role'], 'terminal')
            self.assertEqual(len(terminal['members']), 3)
            self.assertIn(terminal['signal_pin'], terminal['members'])

    def test_output_terminal_housings_do_not_overlap(self):
        components = {c['reference']: c for c in self.board['components']}
        pads = {p['id']: p for p in self.board['pads']}
        bounds = []
        for terminal in self.board['terminals']:
            component = components[terminal['id']]
            xs, ys = zip(*component['body'])
            box = (min(xs), min(ys), max(xs), max(ys))
            dimensions = sorted((box[2] - box[0], box[3] - box[1]))
            self.assertAlmostEqual(dimensions[0], 6.5, places=3)
            self.assertAlmostEqual(dimensions[1], 10.5, places=3)
            for member in terminal['members']:
                x, y = pads[member]['position']
                self.assertTrue(box[0] < x < box[2] and box[1] < y < box[3])
            for reference, other in bounds:
                overlap_x = min(box[2], other[2]) - max(box[0], other[0])
                overlap_y = min(box[3], other[3]) - max(box[1], other[1])
                self.assertFalse(overlap_x > 1e-5 and overlap_y > 1e-5,
                                 f"Overlapping terminal housings: {terminal['id']} / {reference}")
            bounds.append((terminal['id'], box))

    def test_terminal_body_requires_four_explicit_sides(self):
        def line(a, b):
            return ['POLY', 'test', 0, '', 3, 0, [*a, 'L', *b]]
        outline = [line((0, 0), (10, 0)), line((10, 0), (10, 6)),
                   line((10, 6), (0, 6)), line((0, 6), (0, 0))]
        self.assertIsNone(import_pcb.terminal_body(outline[:-1]))
        tabs = [line((0, 2), (-1, 2)), line((-1, 2), (-1, 3)), line((-1, 3), (0, 3))]
        self.assertEqual(import_pcb.terminal_body(outline + tabs),
                         [[0, 0], [10, 0], [10, 6], [0, 6], [0, 0]])
        self.assertIsNone(import_pcb.terminal_body([]))

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
