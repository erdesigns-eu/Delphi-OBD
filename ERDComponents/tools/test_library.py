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
        self.assertIn('OnClick = ExecuteDemoDialog', dfm)
        self.assertNotIn('TPageControl', dfm)
        self.assertNotIn('TTabSheet', dfm)
        self.assertIn('object PageTabs: TERDTabs', dfm)

    def test_showcase_fields_do_not_collide_with_event_methods(self):
        source = (ROOT/'demo/Showcase.pas').read_text(encoding='utf-8-sig')
        declaration = source.split('TShowcaseForm = class(TForm)', 1)[1].split('  end;', 1)[0]
        fields = {n.lower() for n in re.findall(r'^\s+(\w+): \w+;', declaration, re.M)}
        methods = {n.lower() for n in re.findall(r'procedure (\w+)\(', declaration)}
        self.assertFalse(fields & methods, fields & methods)

    def test_showcase_project_collection_properties_exist(self):
        sys.path.insert(0, str(ROOT/'tools/pascalcheck'))
        from typemap import all_units
        classes = {name: ti for unit in all_units().values() for name, ti in unit.types.items()}
        def properties(name, seen=None):
            seen = set() if seen is None else seen
            if name in seen or name not in classes: return set()
            seen.add(name)
            ti = classes[name]
            result = {n.lower() for n in ti.props}
            for parent in ti.parents: result |= properties(parent.lower(), seen)
            return result
        dfm = (ROOT/'demo/Showcase.dfm').read_text(encoding='utf-8-sig')
        stack = []
        collection_depth = 0
        collection_properties = {'items', 'tabs', 'datacolumns', 'categories', 'panels', 'buttons', 'quickaccess', 'steps'}
        for line in dfm.splitlines():
            line = line.strip()
            if collection_depth:
                collection_depth += line.count('<') - line.count('>')
                continue
            match = re.match(r'object \w+: (\w+)', line)
            if match: stack.append(match.group(1).lower()); continue
            if line == 'end':
                if stack: stack.pop()
                continue
            match = re.match(r'(\w+)\s*=\s*<', line)
            if match:
                name = match.group(1).lower()
                if stack and stack[-1].startswith('terd') and name in collection_properties:
                    self.assertIn(name, properties(stack[-1]), f'{stack[-1]}.{name}')
                collection_depth = line.count('<') - line.count('>')

    def test_demo_notifications_and_dialogs_are_bound_to_the_form_theme(self):
        dfm = (ROOT/'demo/Showcase.dfm').read_text(encoding='utf-8-sig')
        for name in ['Dialog', 'OpenDialog', 'SaveDialog', 'OpenPictureDialog',
                     'SavePictureDialog', 'FolderDialog', 'PrintDialog', 'PageSetupDialog',
                     'FontDialog', 'ColorDialog', 'FindDialog', 'ReplaceDialog',
                     'TaskDialog', 'ToastManager', 'HintStyle', 'PopupMenu']:
            self.assertIn(f'object Demo{name}: TERD{name}\n    Theme = Theme', dfm)

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
