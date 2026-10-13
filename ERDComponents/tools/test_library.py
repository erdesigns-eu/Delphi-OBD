"""Integrity/portability regressions for generated assets and independent sources."""
import hashlib
import json
import re
import tempfile
import sys
import unittest
from pathlib import Path
from unittest.mock import patch
import build_icons
import catalog_vectors
import export_icons
import validate
ROOT=Path(__file__).resolve().parents[1]

class LibraryTests(unittest.TestCase):
    def test_showcase_contains_every_registered_component(self):
        registered = set(json.loads((ROOT/'assets/components.json').read_text()))
        dfm = (ROOT/'demo/Showcase.dfm').read_text(encoding='utf-8-sig')
        shown = set(re.findall(r'object \w+: (TERD\w+)', dfm))
        self.assertEqual(registered - shown, set())
        self.assertIn('OnClick = LaunchDialog', dfm)

    def test_interface_routine_bodies_are_rejected(self):
        sys.path.insert(0, str(ROOT/"tools/pascalcheck"))
        from check_sections import scan
        invalid = "interface\nfunction Parse: Integer;\nbegin\n  Result := 1;\nend;\nimplementation\nend."
        self.assertTrue(any("interface section" in message for _, _, message in scan(invalid, "sample.pas")))
        valid = "interface\nfunction Parse: Integer;\nimplementation\nfunction Parse: Integer;\nbegin\n  Result := 1;\nend;\nend."
        self.assertEqual(scan(valid, "sample.pas"), [])

    def test_independent_library_projects_and_resource_coverage(self):
        validate.validate()

    def test_regeneration_preserves_production_icon_tables(self):
        paths=[ROOT/'src/UI/ERD.Components.IconCatalog.pas',ROOT/'src/UI/ERD.Components.IconData.pas']
        before={p:p.read_bytes() for p in paths}
        build_icons.build_production()
        for path in paths:self.assertEqual(path.read_bytes(),before[path])

    def test_exported_svg_is_the_same_geometry_as_runtime_vector_source(self):
        for number in range(1,321):
            self.assertEqual((ROOT/f'assets/svg/{number:03}.svg').read_text().strip(),
                             export_icons.svg(catalog_vectors.vectors(number)))

    def test_catalog_semantics_have_no_empty_placeholders(self):
        catalog=json.loads((ROOT/'docs/iconset-catalog.json').read_text())
        self.assertEqual([i['id'] for i in catalog['icons']],list(range(1,321)))
        self.assertGreaterEqual(len({json.dumps(catalog_vectors.vectors(i)) for i in range(1,321)}),200)
        for number in range(1,321):self.assertTrue(catalog_vectors.vectors(number))

    def test_palette_bitmaps_are_class_specific_and_reproducible(self):
        path=ROOT/'src/DesignTime/ERD.Components.Palette.res'
        before=path.read_bytes();export_icons.build()
        self.assertEqual(path.read_bytes(),before)
        numbers=set(export_icons.COMPONENT_ICONS.values());self.assertGreater(len(numbers),30)

if __name__=='__main__':unittest.main()
