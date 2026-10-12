from __future__ import annotations

import shutil
import unittest
from pathlib import Path

import personal_strings

ROOT = Path(__file__).resolve().parents[1]
WORK = ROOT / "tools" / ".personal-strings-test-workspace"


class PersonalStringsTests(unittest.TestCase):
    def setUp(self):
        shutil.rmtree(WORK, ignore_errors=True)
        WORK.mkdir(parents=True)
        self.addCleanup(lambda: shutil.rmtree(WORK, ignore_errors=True))

    def write(self, text):
        (WORK / "untTest.pas").write_text(text, encoding="utf-8")

    def test_literal_expressions(self):
        self.assertEqual(personal_strings.literal("'It''s'"), "It's")
        self.assertEqual(personal_strings.literal("'a' + #13#10 + 'b'"), "a\r\nb")
        self.assertEqual(personal_strings.literal("'x' +\n  'y'"), "xy")
        self.assertIsNone(personal_strings.literal("SomeVar"))
        self.assertIsNone(personal_strings.literal("'a' + Name"))
        self.assertIsNone(personal_strings.literal("'a' +"))

    def test_collects_tr_trfmt_and_plurals(self):
        self.write(
            "  Caption := Tr('personal.codes.title', 'Trouble codes');\n"
            "  // Tr('personal.codes.commented', 'ignored')\n"
            "  S := TrFmt('personal.codes.found', 'Found %d codes, (%s)', [Count, Name]);\n"
            "  S := Format(TrPlural('personal.codes.count', Length(Codes), '%d code',\n"
            "    '%d codes'), [Length(Codes)]);\n"
            "  S := Tr('ui.dtc.title', 'library keys are ignored');\n"
            "  S := Tr('personal.codes.title', 'Trouble codes');\n")
        strings, problems = personal_strings.collect(WORK)
        self.assertEqual(problems, [])
        self.assertEqual(strings, {
            "personal.codes.count.plural": {"one": "%d code", "other": "%d codes"},
            "personal.codes.found": "Found %d codes, (%s)",
            "personal.codes.title": "Trouble codes",
        })

    def test_reports_conflicting_and_non_literal_defaults(self):
        self.write(
            "  A := Tr('personal.x.a', 'One');\n"
            "  B := Tr('personal.x.a', 'Two');\n"
            "  C := Tr('personal.x.c', SomeText);\n"
            "  D := Tr('personal.x.d');\n")
        _, problems = personal_strings.collect(WORK)
        self.assertEqual(len(problems), 3)
        self.assertIn("different English text", problems[0])
        self.assertIn("must be a string literal", problems[1])
        self.assertIn("needs a key and an English default", problems[2])

    def test_repository_catalogues_are_current(self):
        self.assertEqual(personal_strings.main([]), 0)


if __name__ == "__main__":
    unittest.main()
