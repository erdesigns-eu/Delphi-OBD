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

    def test_anonymous_dynamic_array_cannot_be_returned_as_tarray(self):
        source = self.source('src/Arrays.pas', """unit Arrays;
interface
implementation
function Parse: TArray<Integer>;
var Acc: array of Integer;
begin
  SetLength(Acc, 1);
  Result := Acc;
end;
procedure Consume(const Items: array of Integer);
begin end;
end.
""")
        self.assertIn('total: 1', self.checker('arrayidentity'))
        source.write_text(source.read_text().replace('Acc: array of Integer',
                                                    'Acc: TArray<Integer>'))
        self.assertIn('total: 0', self.checker('arrayidentity'))

    def test_livebindings_notify_requires_helper_unit(self):
        source = self.source('src/Binding.pas', """unit Binding;
interface
uses Data.Bind.Components;
implementation
procedure Changed;
begin TBindings.Notify(Self, ''); end;
end.
""")
        self.assertIn('total: 1', self.checker('vcltypes'))
        source.write_text(source.read_text().replace('Data.Bind.Components;',
                          'Data.Bind.Components, System.Bindings.Helper;'))
        self.assertIn('total: 0', self.checker('vcltypes'))

    def test_qualified_rtl_type_does_not_require_unrelated_repository_homonym(self):
        self.source('src/Compat.pas', 'unit Compat;\ninterface\ntype TSocket = class end;\nimplementation\nend.')
        consumer = self.source('src/Consumer.pas', 'unit Consumer;\ninterface\nuses Winapi.Winsock2;\nvar Socket: Winapi.Winsock2.TSocket;\nimplementation\nend.')
        self.assertIn('total: 0', self.checker('impluses'))
        consumer.write_text(consumer.read_text().replace('Socket: Winapi.Winsock2.TSocket', 'Socket: TSocket'))
        self.assertIn('total: 1', self.checker('impluses'))

    def test_platform_api_checker_rejects_known_delphi_mismatches(self):
        source = self.source('src/Platform.pas', """unit Platform;
interface
uses System.Net.Socket, System.Bluetooth;
type TPassThruOpen = function(Name: PAnsiChar): Integer; cdecl;
var Socket: TSocket; Manager: TBluetoothLEManager; Device: TBluetoothLEDevice;
implementation
procedure Run;
begin
  Socket.SetKeepAlive(True);
  Manager.GetPairedDevices;
  Device.GetCharacteristic(Service, Guid);
  GetProcAddress(Lib, PChar(Name));
end;
end.
""")
        self.assertIn('total: 5', self.checker('platformapi'))
        source.write_text(source.read_text().replace('cdecl', 'stdcall')
                          .replace('System.Net.Socket', 'ERD.Compat.Socket')
                          .replace('Manager.GetPairedDevices', 'Manager.LastDiscoveredDevices')
                          .replace('Device.GetCharacteristic(Service, Guid)', 'Service.Characteristics')
                          .replace('PChar(Name)', 'PAnsiChar(AnsiString(Name))'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_event_returning_getter_must_be_invoked_for_event_assignment(self):
        source = self.source('src/Events.pas', """unit Events;
interface
implementation
procedure Attach;
begin
  Previous := Serial.GetOnDataReceived;
  StateHandler := Transport.GetOnStateChanged;
end;
end.
""")
        self.assertIn('total: 2', self.checker('platformapi'))
        source.write_text(source.read_text().replace('GetOnDataReceived;', 'GetOnDataReceived();')
                          .replace('GetOnStateChanged;', 'GetOnStateChanged();'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_kwp_serial_uses_transport_event_signature_and_portable_queue(self):
        source = self.source('src/KWPSerial.pas', """unit ERD.Protocol.KWP1281.Transport.Serial;
interface
var FQueue: TThreadedQueue<Byte>;
implementation
procedure HandleBytes(const ABytes: TBytes);
begin end;
procedure Open;
begin FSerial.OnDataReceived := HandleBytes; end;
end.
""")
        self.assertIn('total: 3', self.checker('platformapi'))
        source.write_text(source.read_text().replace('TThreadedQueue<Byte>', 'TOBDThreadedQueue<Byte>')
                          .replace('HandleBytes(const', 'HandleBytes(Sender: TObject; const')
                          .replace('FSerial.OnDataReceived := HandleBytes', 'FSerial.SetOnDataReceived(HandleBytes)'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_winsock_addrinfo_and_sendto_parameter_modes(self):
        source = self.source('src/Native.pas', """unit Native;
interface
uses Winapi.Winsock2;
var Res: PAddrInfoW;
implementation
procedure Run;
begin
  GetAddrInfoW(Host, Port, @Hints, Res);
  FreeAddrInfoW(Res);
  sendto(Socket, Bytes[0], Count, 0, PSockAddr(@Address)^, SizeOf(Address));
end;
end.
""")
        self.assertIn('total: 3', self.checker('needsunit'))
        source.write_text(source.read_text().replace('@Hints, Res', 'Hints, Res')
                          .replace('FreeAddrInfoW(Res)', 'FreeAddrInfoW(Res^)')
                          .replace('PSockAddr(@Address)^', 'PSockAddr(@Address)'))
        self.assertIn('total: 0', self.checker('needsunit'))

    def test_udp_transport_cannot_leak_fpc_only_endpoint_and_overloads(self):
        source = self.source('src/UDP.pas', """unit ERD.Connection.UDP;
interface
uses ERD.Compat.Socket;
implementation
procedure Run;
begin
  Local := TNetEndpoint.Create(TIPAddress.Any.IPv4Address, 0);
  Got := FSocket.ReceiveFrom(Buffer, Origin, [], 2048);
  Sent := FSocket.SendTo(Remote, Buffer);
end;
end.
""")
        self.assertIn('total: 3', self.checker('platformapi'))
        source.write_text(source.read_text()
                          .replace('TNetEndpoint.Create(TIPAddress.Any.IPv4Address, 0)', "TOBDDatagramEndpoint.Create('', 0)")
                          .replace('ReceiveFrom(Buffer, Origin, [], 2048)', 'ReceiveDatagram(Buffer, 2048)')
                          .replace('SendTo(Remote, Buffer)', 'SendDatagram(Remote, Buffer)'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_tproc_reader_literals_require_matching_value_parameter_modes(self):
        source = self.source('src/Reader.pas', """unit Reader;
interface
type TReader = class(TThread)
  FOnBytes: TProc<TBytes>;
  FOnError: TProc<TOBDErrorCode, string>;
end;
implementation
procedure Open;
begin
  Reader := TReader.Create(Socket,
    procedure(const Bytes: TBytes) begin FireBytes(Bytes); end,
    procedure(Code: TOBDErrorCode; const Msg: string) begin FireError(Code, Msg); end);
end;
end.
""")
        self.assertIn('total: 2', self.checker('platformapi'))
        source.write_text(source.read_text().replace('procedure(const Bytes:', 'procedure(Bytes:')
                          .replace('; const Msg:', '; Msg:'))
        self.assertIn('total: 0', self.checker('platformapi'))
        # A const callback declared as a custom reference remains valid.
        source.write_text(source.read_text().replace('TProc<TBytes>', 'TConstBytesCallback')
                          .replace('TProc<TOBDErrorCode, string>', 'TConstErrorCallback')
                          .replace('procedure(Bytes:', 'procedure(const Bytes:')
                          .replace('; Msg:', '; const Msg:'))
        self.assertIn('total: 0', self.checker('platformapi'))

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

    def test_ui_guard_checks_flash_layer_and_rejects_fmx_in_ui(self):
        source = self.source('src/Flashing/ERD.Flash.Demo.pas',
                             'unit ERD.Flash.Demo; interface uses Vcl.Graphics; implementation end.')
        self.assertIn('total: 1', self.checker('runtime'))
        source.unlink()
        visual = self.source('src/UI/ERD.UI.Demo.pas',
                             'unit ERD.UI.Demo; interface uses Vcl.Graphics; implementation end.')
        self.assertIn('total: 0', self.checker('runtime'))
        visual.write_text(visual.read_text().replace('Vcl.Graphics', 'FMX.Graphics'))
        self.assertIn('total: 1', self.checker('runtime'))

    def test_platform_guard_detects_wrong_ide_target(self):
        for filename, targets in [
                ('packages/DelphiOBD_RT.dproj', ['Win32', 'Win64']),
                ('packages/DelphiOBD_DT.dproj', ['Win32']),
                ('tests/DelphiOBD_Tests.dproj', ['Win32', 'Win64'])]:
            self.source(filename, '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'
                        '<PropertyGroup><Platform>Win32</Platform></PropertyGroup>'
                        '<ProjectExtensions><BorlandProject><Platforms>' +
                        ''.join('<Platform value="' + target + '">True</Platform>' for target in targets) +
                        '</Platforms></BorlandProject></ProjectExtensions></Project>')
        self.assertIn('total: 0', self.checker('platforms'))
        path = self.root / 'packages/DelphiOBD_DT.dproj'
        path.write_text(path.read_text().replace('value="Win32"', 'value="Win64"'))
        self.assertIn('total: 1', self.checker('platforms'))

    def test_eol_honors_per_file_attributes_without_reclassifying_all_pascal(self):
        subprocess.run(['git', 'init', '-q', str(self.root)], check=True)
        self.source('.gitattributes', 'packages/*.dpk text eol=crlf\n')
        self.source('src/Demo.pas', 'unit Demo; interface implementation end.\n')
        package = self.source('packages/Demo.dpk', 'package Demo; end.\n')
        self.assertIn('total: 1', self.checker('eol'))
        package.write_bytes(b'package Demo; end.\r\n')
        self.assertIn('total: 0', self.checker('eol'))

    def test_thread_start_inside_constructor_is_rejected(self):
        source = self.source('src/Worker.pas', """unit Worker;
interface
uses System.Classes;
type TWorker = class(TThread)
constructor Create;
end;
implementation
constructor TWorker.Create;
begin inherited Create(True); Start; end;
end.
""")
        self.assertIn('total: 1', self.checker('worker'))
        source.write_text(source.read_text().replace(' Start;', ''))
        self.assertIn('total: 0', self.checker('worker'))

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
