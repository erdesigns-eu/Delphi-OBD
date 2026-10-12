"""Regressions: missing icons, generated palette art, corrupt PNGs and stale/mismatched EV documents."""
import json
import struct
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import catalog_index as index
import designtime_icons as icons
import designtime_resources as resources
import ev_support_matrix as matrix
import fpc_runtime as runtime
import re
import safety_guide as guide
import validate_delphi_projects as projects
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]


class CompilerSelectionTests(unittest.TestCase):
    def test_built_source_tree_wins_over_system_bootstrap(self):
        with patch.object(runtime.shutil, 'which', return_value='/usr/bin/fpc') as lookup:
            selected = runtime.select_compiler(None, Path('/tmp/erd-fpc-source'))
        self.assertEqual(selected, '/tmp/erd-fpc-source/compiler/ppcx64')
        lookup.assert_not_called()

    def test_cli_source_tree_ignores_compiler_on_path(self):
        with patch('sys.argv', ['fpc_runtime.py', '--source-tree', '/tmp/erd-fpc-source']), \
                patch.object(runtime.shutil, 'which', return_value='/usr/bin/fpc'), \
                patch.object(runtime.subprocess, 'check_output',
                             side_effect=RuntimeError('stop after compiler selection')) as query:
            with self.assertRaisesRegex(RuntimeError, 'stop after compiler selection'):
                runtime.main()
        query.assert_called_once_with(
            ['/tmp/erd-fpc-source/compiler/ppcx64', '-iV'], text=True)

    def test_explicit_compiler_wins_over_source_tree(self):
        self.assertEqual(runtime.select_compiler('/opt/fpc/ppcx64', Path('/tmp/fpc')),
                         '/opt/fpc/ppcx64')

    def test_path_lookup_is_used_only_without_overrides(self):
        with patch.object(runtime.shutil, 'which', return_value='/usr/bin/fpc') as lookup:
            self.assertEqual(runtime.select_compiler(None, None), '/usr/bin/fpc')
        lookup.assert_called_once_with('fpc')

    def test_missing_built_compiler_does_not_fall_back_to_bootstrap(self):
        with patch.object(runtime.shutil, 'which', return_value='/usr/bin/fpc') as lookup:
            selected = runtime.select_compiler(None, Path('/missing/fpc-tree'))
        self.assertEqual(selected, '/missing/fpc-tree/compiler/ppcx64')
        lookup.assert_not_called()


class DelphiProjectTests(unittest.TestCase):
    def test_all_tracked_projects_have_complete_ide_metadata(self):
        for name in projects.PROJECTS:
            self.assertEqual(projects.inspect(ROOT / name), [])

    def modified(self, change):
        path = ROOT / 'packages/DelphiOBD_DT.dproj'
        tree = ET.fromstring(path.read_bytes())
        change(tree)
        raw = b'\xef\xbb\xbf' + ET.tostring(tree, encoding='unicode').replace('\n', '\r\n').encode('utf-8')
        with patch.object(Path, 'read_bytes', return_value=raw):
            return projects.inspect(path)

    def test_empty_package_type_is_rejected(self):
        found = self.modified(lambda tree: setattr(tree.find(
            'm:ProjectExtensions/m:Borland.ProjectType', projects.NS), 'text', ''))
        self.assertIn('Missing/invalid Borland.ProjectType', found)

    def test_missing_configurations_are_rejected(self):
        def remove(tree):
            for group in tree.findall('m:ItemGroup', projects.NS):
                for config in list(group.findall('m:BuildConfiguration', projects.NS)):
                    group.remove(config)
        self.assertIn('Incomplete IDE BuildConfiguration entries', self.modified(remove))

    def test_missing_release_activation_is_rejected(self):
        def remove(tree):
            for group in tree.findall('m:PropertyGroup', projects.NS):
                for child in list(group):
                    if child.tag.endswith('Cfg_2'):
                        group.remove(child)
        self.assertIn('Missing configuration activation group: Cfg_2', self.modified(remove))

    def test_wrong_encoding_is_rejected(self):
        path = ROOT / 'packages/DelphiOBD_DT.dproj'
        raw = path.read_bytes().removeprefix(b'\xef\xbb\xbf').replace(b'\r\n', b'\n')
        with patch.object(Path, 'read_bytes', return_value=raw):
            self.assertIn('IDE project must use UTF-8 BOM and CRLF', projects.inspect(path))


class ResourceTests(unittest.TestCase):
    def test_every_registered_class_has_exact_reproducible_resource(self):
        data, count = resources.build()
        self.assertEqual(count, 225)
        self.assertEqual(data, resources.TARGET.read_bytes())
        # The Win32 resource stream starts with the required 32-byte null header.
        self.assertEqual(struct.unpack_from('<II', data), (0, 32))
        # The IDE reads palette PNGs from RCDATA <CLASS>16_PNG / _PNG / 32_PNG.
        for name in ('TOBDDIALGAUGE16_PNG', 'TOBDDIALGAUGE_PNG', 'TOBDDIALGAUGE32_PNG'):
            self.assertIn(b'\x0a\x00' + name.encode('utf-16le') + b'\0\0', data, name)

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


class IconTests(unittest.TestCase):
    def test_tracked_icons_match_the_generator(self):
        files = icons.build()
        self.assertEqual(len(files), 225 * len(icons.ICON_SIZES) + 2)
        for rel, data in files.items():
            self.assertEqual(data, (icons.ASSETS / rel).read_bytes(), rel)
        tracked = {p.name for p in icons.PALETTE.glob('*.png')}
        self.assertEqual(tracked, {Path(rel).name for rel in files if rel.startswith('palette/')})

    def test_icons_are_line_icons_on_a_transparent_background(self):
        canvas = icons.render('TOBDDialGauge')
        self.assertEqual(canvas.pixels[0][3], 0.0)
        r, g, b, a = canvas.pixels[13 * icons.SIZE + 11]  # needle hub
        self.assertEqual(tuple(round(v) for v in (r, g, b)), icons.ORANGE)
        self.assertAlmostEqual(a, 1.0)
        self.assertFalse(any(px[:3] == list(icons.TILE) and px[3]
                             for px in canvas.pixels))

    def test_unregistered_design_is_a_failure(self):
        with patch.dict(icons.ICONS, {'TOBDNotRegistered': icons.dial}):
            with self.assertRaisesRegex(ValueError, 'unregistered'):
                icons.build()


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


class CatalogIndexTests(unittest.TestCase):
    def test_generated_index_matches_catalogues_and_excludes_fixture(self):
        generated = index.render()
        self.assertEqual(generated.encode('utf-8'), index.TARGET.read_bytes())
        self.assertNotIn('SCHEMA_V2_FIXTURE', generated)
        oem, dtc, universal, profiles, shared = index.collect()
        self.assertEqual([row['file'] for row in universal],
                         ['obd2-pids.json', 'uds-standard.json'])
        self.assertTrue(all(row['file'].startswith('dtc-') for row in dtc))
        self.assertEqual(sum(row['total'] for row in oem),
                         sum(sum(row['counts']) for row in oem))
        self.assertEqual(len(profiles), 2)
        self.assertTrue(shared)
        self.assertTrue(all(row['file'].startswith('_shared/') for row in shared))
        bases = {row['file'] for row in shared} | {row['file'] for row in oem}
        self.assertTrue(all(row['base'] in bases for row in oem if row['base']))


class SafetyGuideTests(unittest.TestCase):
    @staticmethod
    def decode(unit):
        """The guide lines held by the generated Pascal constant."""
        body = unit.split('of string = (', 1)[1].split('\r\n\r\nfunction', 1)[0]
        lines, current = [], ''
        for token in re.findall(r"'(?:[^']|'')*'|#\$[0-9A-F]{4}|[,)]", body):
            if token in (',', ')'):
                lines.append(current)
                current = ''
            elif token.startswith('#$'):
                current += chr(int(token[2:], 16))
            else:
                current += token[1:-1].replace("''", "'")
        return lines

    def test_generated_unit_matches_the_guide(self):
        text = guide.to_text(guide.SOURCE.read_text(encoding='utf-8'))
        unit = guide.UNIT.read_bytes()
        self.assertEqual(guide.pascal_unit(text).encode('ascii'), unit)
        self.assertEqual(self.decode(unit.decode('ascii')), text)
        self.assertEqual(text[0], 'Flashing safety')

    def test_inline_markup_is_removed_but_code_is_kept(self):
        self.assertEqual(guide.inline('**Wire `OnConfirm`** to *a* `T*` [doc](x.md)'),
                         'Wire OnConfirm to a T* doc')

    def test_long_lines_and_quotes_stay_within_literal_limits(self):
        line = "it's " * 80 + '\u2014 \u26a0\ufe0f'
        lines = guide.pascal_lines(line)
        self.assertTrue(all(len(source) < 100 for source in lines))
        unit = 'of string = (\r\n' + '\r\n'.join(lines) + ');\r\n\r\nfunction'
        self.assertEqual(self.decode(unit), [line])


if __name__ == '__main__':
    unittest.main()
