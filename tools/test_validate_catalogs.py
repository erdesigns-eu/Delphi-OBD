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


if __name__ == '__main__':
    unittest.main()
