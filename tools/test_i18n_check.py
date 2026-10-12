from __future__ import annotations

import json
import shutil
import unittest
from pathlib import Path

import i18n_check

ROOT = Path(__file__).resolve().parents[1]
WORK = ROOT / "tools" / ".i18n-check-test-workspace"


class I18nCheckTests(unittest.TestCase):
    def setUp(self):
        shutil.rmtree(WORK, ignore_errors=True)
        (WORK / "languages").mkdir(parents=True)
        (WORK / "src").mkdir(parents=True)
        self.addCleanup(lambda: shutil.rmtree(WORK, ignore_errors=True))

    def write_catalog(self, code, strings):
        (WORK / "languages" / f"{code}.json").write_text(json.dumps({
            "language": code,
            "name": code,
            "englishName": code,
            "version": 1,
            "strings": strings,
        }), encoding="utf-8")

    def test_accepts_matching_catalogs_and_source_keys(self):
        strings = {
            "ui.sample.title": "Value %d",
            "ui.sample.count.plural": {"one": "%d item", "other": "%d items"},
        }
        self.write_catalog("en", strings)
        self.write_catalog("nl", {
            "ui.sample.title": "Waarde %d",
            "ui.sample.count.plural": {"one": "%d item", "other": "%d items"},
        })
        (WORK / "src" / "Sample.pas").write_text(
            "unit Sample; interface implementation begin TrFmt('ui.sample.title', '', [1]); TrPlural('ui.sample.count', 2, '', ''); end.",
            encoding="utf-8",
        )
        report = i18n_check.audit(WORK / "languages", WORK / "src")
        self.assertEqual([], report["issues"])

    def test_reports_missing_key_and_placeholder_mismatch(self):
        self.write_catalog("en", {"ui.sample.title": "Value %d", "ui.sample.other": "Other"})
        self.write_catalog("fr", {"ui.sample.title": "Valeur %s"})
        (WORK / "src" / "Sample.pas").write_text("unit Sample; implementation begin Tr('ui.sample.missing', ''); end.", encoding="utf-8")
        report = i18n_check.audit(WORK / "languages", WORK / "src")
        joined = "\n".join(report["issues"])
        self.assertIn("fr: missing key ui.sample.other", joined)
        self.assertIn("placeholder mismatch", joined)
        self.assertIn("source key missing", joined)


if __name__ == "__main__":
    unittest.main()
