"""Hardware-free regression fixtures for the offline catalog audit."""
import json
from pathlib import Path
import tempfile
import unittest

from referencing.exceptions import Unresolvable
from validate_catalogs import audit


class CatalogAuditTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        (self.root / '_schema').mkdir()
        self.schema = {
            '$schema': 'https://json-schema.org/draft/2020-12/schema',
            '$id': 'https://example.invalid/fixture.json',
            'type': 'object',
            'required': ['version'],
            'properties': {'version': {'type': 'integer'}},
        }

    def write(self, name, value):
        (self.root / name).write_text(json.dumps(value), encoding='utf-8')

    def test_valid_and_invalid_declared_catalogs(self):
        self.write('_schema/fixture.json', self.schema)
        self.write('valid.json', {'$schema': self.schema['$id'], 'version': 2})
        self.write('invalid.json', {'$schema': self.schema['$id'], 'version': 'two'})
        report = audit(self.root)
        self.assertEqual(2, len(report['checked']))
        self.assertEqual(['invalid.json'], [item['file'] for item in report['violations']])
        self.assertEqual('/version', report['violations'][0]['path'])

    def test_missing_schema_is_uncovered(self):
        self.write('unknown.json', {'$schema': 'https://example.invalid/missing.json'})
        report = audit(self.root)
        self.assertEqual([], report['checked'])
        self.assertEqual('unknown.json', report['uncovered'][0]['file'])

    def test_duplicate_object_keys_are_rejected(self):
        (self.root / 'duplicate.json').write_text('{"version":1,"version":2}')
        report = audit(self.root)
        self.assertIn('Duplicate JSON object key', report['violations'][0]['message'])

    def test_relative_schema_resolves_inside_catalog_root(self):
        self.write('_schema/fixture.json', self.schema)
        (self.root / 'nested').mkdir()
        self.write('nested/valid.json', {'$schema': '../_schema/fixture.json', 'version': 2})
        report = audit(self.root)
        self.assertEqual(1, len(report['checked']))
        self.assertEqual([], report['violations'])

    def test_non_finite_json_numbers_are_rejected(self):
        for token in ('NaN', 'Infinity', '-Infinity'):
            (self.root / 'bad.json').write_text('{"value":' + token + '}')
            self.assertIn('Non-finite JSON number', audit(self.root)['violations'][0]['message'])

    def test_manifest_references_require_existing_matching_unique_vendors(self):
        (self.root / 'ev-battery').mkdir()
        self.write('ev-battery/fixture.json', {'vendor': 'fixture'})
        manifest = {'vendor_files': [{'vendor': 'fixture', 'file': 'fixture.json'}]}
        self.write('ev-battery/_manifest.json', manifest)
        self.assertEqual([], audit(self.root)['violations'])
        manifest['vendor_files'][0]['vendor'] = 'wrong'
        self.write('ev-battery/_manifest.json', manifest)
        self.assertIn('does not match', audit(self.root)['violations'][0]['message'])
        manifest['vendor_files'][0] = {'vendor': 'fixture', 'file': 'missing.json'}
        self.write('ev-battery/_manifest.json', manifest)
        self.assertIn('does not exist', audit(self.root)['violations'][0]['message'])
        manifest['vendor_files'] = [{'vendor': 'fixture', 'file': 'fixture.json'}] * 2
        self.write('ev-battery/_manifest.json', manifest)
        self.assertIn('Duplicate', audit(self.root)['violations'][0]['message'])

    def test_external_schema_reference_is_denied(self):
        self.schema['properties']['version'] = {'$ref': 'https://example.invalid/remote.json'}
        self.write('_schema/fixture.json', self.schema)
        self.write('catalog.json', {'$schema': self.schema['$id'], 'version': 2})
        with self.assertRaises(Unresolvable):
            audit(self.root)


class CatalogSchemaContractTests(unittest.TestCase):
    @staticmethod
    def validator(name):
        from jsonschema import Draft202012Validator
        schema = Path(__file__).resolve().parents[1] / 'catalogs' / '_schema' / name
        return Draft202012Validator(json.loads(schema.read_text()))

    def test_extended_wmi_requires_low_volume_marker_and_valid_alphabet(self):
        validator = self.validator('vin-vds-rules.schema.json')
        for wmi, valid in [('1FT', True), ('1F9ABC', True), ('1FTABC', False),
                           ('1F9AIC', False), ('1F9AB', False)]:
            with self.subTest(wmi=wmi):
                value = {'schemas': {'fixture': {'wmis': [{'wmi': wmi}],
                         'patterns': [{'keys': '*', 'field': 'EngineModel'}]}}}
                self.assertEqual(valid, validator.is_valid(value))

    def test_manufacturer_prefix_and_exact_wmi_are_distinct_valid_shapes(self):
        validator = self.validator('vin-wmi.schema.json')
        for wmi, valid in [('1F', True), ('1FT', True), ('1', False), ('1F9ABC', False)]:
            with self.subTest(wmi=wmi):
                value = {'$schema': 'fixture', 'schema_version': 1,
                         'entries': [{'wmi': wmi, 'name': 'Manufacturer'}]}
                self.assertEqual(valid, validator.is_valid(value))

    def test_ev_notes_are_typed_documentation(self):
        validator = self.validator('ev-battery.schema.json')
        value = {'vendor': 'fixture', 'ecu': {'notes': 'Documentation'},
                 'fields': [{'field': 'soc', 'notes': 'Documentation'}]}
        self.assertTrue(validator.is_valid(value))
        value['fields'][0]['notes'] = {'routing': 'unsupported'}
        self.assertFalse(validator.is_valid(value))

    def test_ev_routing_fields_have_checked_types(self):
        validator = self.validator('ev-battery.schema.json')
        value = {'vendor': 'fixture', 'ecu': {}, 'fields': [
                 {'field': 'soc', 'ecu_request_id_hex': '0x7E0'}]}
        self.assertTrue(validator.is_valid(value))
        value['fields'][0]['ecu_request_id_hex'] = '0x7E0' + chr(13) + 'ATZ'
        self.assertFalse(validator.is_valid(value))

    def test_manifest_has_its_own_shape_and_disallows_parent_paths(self):
        validator = self.validator('ev-battery-manifest.schema.json')
        value = {'manifest_version': '1.0.0', 'generated': '2026-10-08',
                 'all_sources': [], 'vendor_files': [{'vendor': 'fixture',
                 'file': 'fixture.json', 'coverage': 'partial', 'primary_source': None}]}
        self.assertTrue(validator.is_valid(value))
        value['vendor_files'][0]['file'] = '../fixture.json'
        self.assertFalse(validator.is_valid(value))


class OEMInheritanceTests(unittest.TestCase):
    """"extends" chains, inherited duplicates and the shared-template budget."""

    OEM = 'https://erdesigns.eu/schema/oem-catalog-v2.json'

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        (self.root / '_schema').mkdir()
        (self.root / '_shared').mkdir()

    def write(self, name, **fields):
        value = {'$schema': self.OEM, **fields}
        (self.root / name).write_text(json.dumps(value), encoding='utf-8')

    def messages(self):
        return [item['message'] for item in audit(self.root)['violations']]

    def test_valid_extends_chain_passes(self):
        self.write('_shared/base.json', dids=[{'did': '0x0001', 'name': 'a'}])
        self.write('brand.json', extends='_shared/base.json',
                   dids=[{'did': '0x0002', 'name': 'b'}])
        self.assertEqual([], self.messages())

    def test_missing_base_is_a_violation(self):
        self.write('brand.json', extends='_shared/missing.json')
        self.assertIn('does not exist', self.messages()[0])

    def test_base_outside_the_root_and_absolute_paths_are_rejected(self):
        self.write('brand.json', extends='../outside.json')
        self.assertIn('does not exist inside', self.messages()[0])
        self.write('brand.json', extends='/etc/base.json')
        self.assertIn('relative path', self.messages()[0])

    def test_cycle_is_a_violation(self):
        self.write('_shared/a.json', extends='b.json')
        self.write('_shared/b.json', extends='a.json')
        self.assertTrue(any('deeper than' in m for m in self.messages()))

    def test_copying_an_inherited_entry_is_a_violation(self):
        entry = {'did': '0x0001', 'name': 'a'}
        self.write('_shared/base.json', dids=[entry])
        self.write('brand.json', extends='_shared/base.json',
                   dids=[dict(entry, source='brand-tool')])
        self.assertIn('repeat entries inherited', self.messages()[0])

    def test_template_copied_into_many_brands_exceeds_the_budget(self):
        rows = [{'did': '0x%04X' % n, 'name': 'd%d' % n} for n in range(1000)]
        for brand in range(5):
            self.write('brand%d.json' % brand, dids=rows)
        self.assertTrue(any('_shared/ base' in m for m in self.messages()))


if __name__ == '__main__':
    unittest.main()
