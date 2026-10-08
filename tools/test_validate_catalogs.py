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

    def test_external_schema_reference_is_denied(self):
        self.schema['properties']['version'] = {'$ref': 'https://example.invalid/remote.json'}
        self.write('_schema/fixture.json', self.schema)
        self.write('catalog.json', {'$schema': self.schema['$id'], 'version': 2})
        with self.assertRaises(Unresolvable):
            audit(self.root)


if __name__ == '__main__':
    unittest.main()
