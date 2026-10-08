"""Regression fixtures for imported checkers and Delphi-OBD discovery."""
import os
import pathlib
import subprocess
import sys
import tempfile
import unittest

HERE = pathlib.Path(__file__).resolve().parent


class CheckerRegressionTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = pathlib.Path(self.temp.name)

    def source(self, path, text):
        target = self.root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text)
        return target

    def checker(self, name):
        env = dict(os.environ, REPO=str(self.root))
        result = subprocess.run([sys.executable, str(HERE / ('check_' + name + '.py'))],
                                cwd=self.root, env=env, capture_output=True, text=True)
        self.assertNotIn('Traceback', result.stdout + result.stderr)
        self.assertIn('total:', result.stdout)
        return result.stdout

    def test_namespaced_exports_are_visible_and_missing_import_is_found(self):
        self.source('src/Core/ERD.Values.pas', '''unit ERD.Values;
interface
function Answer: Integer;
implementation
function Answer: Integer;
begin Result := 42; end;
end.
''')
        consumer = self.source('samples/Demo/Consumer.pas', '''unit Consumer;
interface
procedure Run;
implementation
uses ERD.Values;
procedure Run;
begin Writeln(Answer); end;
end.
''')
        self.assertIn('total: 0', self.checker('reach'))
        consumer.write_text(consumer.read_text().replace('uses ERD.Values;', ''))
        out = self.checker('reach')
        self.assertIn('Answer', out)
        self.assertIn('total: 1', out)

    def test_alternative_platform_branches_are_not_duplicate_methods(self):
        source = self.source('src/Platform.pas', '''unit Platform;
interface
function PlatformNumber: Integer;
implementation
{$IFDEF MSWINDOWS}
function PlatformNumber: Integer;
begin Result := 1; end;
{$ELSE}
function PlatformNumber: Integer;
begin Result := 2; end;
{$ENDIF}
end.
''')
        self.assertNotIn('implemented 2 times', self.checker('dup'))
        source.write_text(source.read_text().replace('{$IFDEF MSWINDOWS}', '').replace('{$ELSE}', '').replace('{$ENDIF}', ''))
        self.assertIn('implemented 2 times', self.checker('dup'))

    def test_comma_literal_is_one_case_label(self):
        source = self.source('src/Cases.pas', '''unit Cases;
interface
procedure Draw(C: Char);
implementation
procedure Draw(C: Char);
begin
  case C of
    ',': Writeln(1);
    ''' + "''''" + ''': Writeln(2);
  end;
end;
end.
''')
        self.assertIn('total: 0', self.checker('caselabel'))
        source.write_text(source.read_text().replace("',': Writeln(1);", "',': Writeln(1);\n    ',': Writeln(3);"))
        self.assertIn('total: 1', self.checker('caselabel'))

    def test_optional_argument_after_empty_exception_class(self):
        self.source('src/Calls.pas', '''unit Calls;
interface
uses System.SysUtils;
type EBad = class(Exception);
procedure Send(const Value: string; const Separator: string = '');
procedure Read(const Values: array of Word);
implementation
procedure Send(const Value: string; const Separator: string);
begin end;
procedure Read(const Values: array of Word);
begin end;
procedure Run;
begin Send('x'); Read([]); end;
end.
''')
        self.assertIn('total: 0', self.checker('args'))

    def test_numeric_ifthen_is_not_mistaken_for_string_result(self):
        source = self.source('src/MathUse.pas', '''unit MathUse;
interface
uses System.Math;
implementation
procedure Run;
var N: Integer;
begin N := IfThen('x' <> '', Length('abc'), 0); end;
end.
''')
        self.assertIn('total: 0', self.checker('ifthen'))
        source.write_text(source.read_text().replace("Length('abc'), 0", "'abc', ''"))
        self.assertIn('total: 1', self.checker('ifthen'))

    def test_multiline_framework_import_is_a_runtime_violation(self):
        source = self.source('src/Service/ERD.Service.Demo.pas', """unit ERD.Service.Demo;
interface
uses
  System.Classes,
  Vcl.Graphics;
implementation
end.
""")
        self.assertIn('total: 1', self.checker('runtime'))
        source.write_text(source.read_text().replace('Vcl.Graphics', 'System.SysUtils'))
        self.assertIn('total: 0', self.checker('runtime'))

    def test_fpc_rtl_branch_keeps_delphi_import_visible(self):
        source = self.source('src/Portable.pas', """unit Portable;
interface
uses {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF};
implementation
procedure Run;
begin Writeln(UpperCase('abc')); end;
end.
""")
        self.assertIn('total: 0', self.checker('rtluses'))
        source.write_text(source.read_text().replace('System.SysUtils', 'System.Classes'))
        self.assertIn('UpperCase', self.checker('rtluses'))

    def test_fpc_nested_directives_preserve_offsets(self):
        sys.path.insert(0, str(HERE))
        from paslex import strip_code
        source = "{$IFDEF FPC}FpcOnly;{$IF X}Nested;{$ENDIF}{$ELSE}DelphiOnly;{$ENDIF} Tail;"
        clean, directives = strip_code(source)
        self.assertEqual(len(source), len(clean))
        self.assertNotIn('FpcOnly', clean)
        self.assertNotIn('Nested', clean)
        self.assertIn('DelphiOnly', clean)
        self.assertEqual(clean.index('Tail'), source.index('Tail'))
        self.assertEqual(len(directives), 5)

    def test_missing_optional_form_directory_does_not_crash(self):
        for name in ('caption', 'dfm', 'dispatch'):
            with self.subTest(name=name):
                self.assertIn('total: 0', self.checker(name))


if __name__ == '__main__':
    unittest.main()
