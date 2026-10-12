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

    def test_seed_key_provider_registration_selects_interface_overload(self):
        source = self.source('src/OEM.pas', """unit OEM;
interface
implementation
procedure RegisterDefaults;
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
  Reg.RegisterAlgorithm($02, TOBDSeedKeyXorMask.Create(Mask, 'fixture'));
end;
end.
""")
        self.assertIn('total: 2', self.checker('platformapi'))
        source.write_text(source.read_text()
                          .replace('TOBDSeedKeyKWP2000TwosComplement.Create);',
                                   'IOBDSeedKeyAlgorithm(TOBDSeedKeyKWP2000TwosComplement.Create()));')
                          .replace("TOBDSeedKeyXorMask.Create(Mask, 'fixture'));",
                                   "IOBDSeedKeyAlgorithm(TOBDSeedKeyXorMask.Create(Mask, 'fixture')));"))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_zero_argument_callback_value_requires_explicit_invocation(self):
        source = self.source('src/Callbacks.pas', """unit Callbacks;
interface
type TOBDSessionCloseCallback = reference to function: Boolean;
     TCallbacks = record CloseSession: TOBDSessionCloseCallback; end;
implementation
procedure Run(const Source: TFunc<Double>; const Callbacks: TCallbacks);
var SourceCopy: TFunc<Double>;
begin
  if not Callbacks.CloseSession then Fail;
  Value := Source;
  SourceCopy := Source;
  if Assigned(Source) then Pass;
end;
end.
""")
        self.assertIn('total: 2', self.checker('platformapi'))
        source.write_text(source.read_text().replace('CloseSession then', 'CloseSession() then')
                          .replace('Value := Source;', 'Value := Source();'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_global_generic_factory_is_not_delphi_compatible(self):
        source = self.source('src/Factory.pas', """unit Factory;
interface
function NewPromise<T>: IPromise<T>;
implementation
function NewPromise<T>: IPromise<T>;
begin end;
end.
""")
        self.assertIn('total: 2', self.checker('platformapi'))
        source.write_text("""unit Factory;
interface
type TFactory = class
  class function NewPromise<T>: IPromise<T>; static;
end;
implementation
class function TFactory.NewPromise<T>: IPromise<T>;
begin end;
end.
""")
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_tproc_statement_requires_explicit_invocation(self):
        source = self.source('src/Work.pas', """unit Work;
interface
type TWork = record Drop: TProc; end;
implementation
procedure Drain(const Item: TWork);
begin
  if Assigned(Item.Drop) then
    try Item.Drop; except end;
end;
end.
""")
        self.assertIn('total: 1', self.checker('platformapi'))
        source.write_text(source.read_text().replace('try Item.Drop;', 'try Item.Drop();'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_delphi_binary_hash_and_delete_file_contracts(self):
        source = self.source('src/Checkpoint.pas', """unit Checkpoint;
interface
uses System.Hash, System.SysUtils, Winapi.Windows;
implementation
procedure Save(const Image: TBytes; const TempName: string);
begin
  Digest := THashSHA2.GetHashBytes(Image, THashSHA2.TSHA2Version.SHA256);
  DeleteFile(TempName);
end;
end.
""")
        self.assertIn('total: 2', self.checker('platformapi'))
        source.write_text(source.read_text()
                          .replace('THashSHA2.GetHashBytes(Image, THashSHA2.TSHA2Version.SHA256)',
                                   'Hash.HashAsBytes()')
                          .replace('  DeleteFile(TempName);', '  System.SysUtils.DeleteFile(TempName);'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_style_color_and_rgb_helpers_require_declaring_units(self):
        source = self.source('src/Colors.pas', """unit Colors;
interface
uses Vcl.Graphics, System.UITypes;
function Color(Slot: TStyleColor): TColor;
implementation
uses Vcl.Themes;
function Color(Slot: TStyleColor): TColor;
begin
  R := GetRValue(Value);
  G := GetGValue(Value);
  B := GetBValue(Value);
end;
end.
""")
        self.assertIn('total: 1', self.checker('vcltypes'))
        self.assertIn('total: 3', self.checker('needsunit'))
        source.write_text(source.read_text().replace('Vcl.Graphics, System.UITypes;',
                          'Vcl.Graphics, Vcl.Themes, Winapi.Windows;').replace('implementation\nuses Vcl.Themes;', 'implementation'))
        self.assertIn('total: 0', self.checker('vcltypes'))
        self.assertIn('total: 0', self.checker('needsunit'))

    def test_bare_font_style_needs_vcl_graphics(self):
        source = self.source('applications/Page.pas', """unit Page;
interface
uses System.UITypes, Vcl.Controls, Vcl.StdCtrls;
implementation
procedure Build(L: TLabel);
begin
  L.Font.Style := [fsBold];
  L.Font.Style := [TFontStyle.fsItalic];
end;
end.
""")
        self.assertIn('total: 1', self.checker('needsunit'))
        source.write_text(source.read_text().replace('Vcl.Controls,', 'Vcl.Graphics, Vcl.Controls,'))
        self.assertIn('total: 0', self.checker('needsunit'))

    def test_edit_char_case_needs_vcl_stdctrls(self):
        source = self.source('applications/Console.pas', """unit Console;
interface
uses Vcl.Controls, Vcl.Forms, ERD.UI.Edits;
implementation
procedure Build(E: TOBDEdit);
begin
  E.CharCase := ecUpperCase;
end;
end.
""")
        self.assertIn('total: 1', self.checker('needsunit'))
        source.write_text(source.read_text().replace('Vcl.Forms,', 'Vcl.Forms, Vcl.StdCtrls,'))
        self.assertIn('total: 0', self.checker('needsunit'))

    def test_rtl_rect_uses_left_top_and_gdiplus_rect_uses_xy(self):
        source = self.source('src/Rectangles.pas', """unit Rectangles;
interface
implementation
procedure Draw(const Bounds: TRectF; const GDI: TGPRectF);
begin
  CenterX := Bounds.X;
  CenterY := Bounds.Y;
  Other := GDI.X + GDI.Y;
end;
end.
""")
        self.assertIn('total: 2', self.checker('platformapi'))
        source.write_text(source.read_text().replace('Bounds.X', 'Bounds.Left')
                          .replace('Bounds.Y', 'Bounds.Top'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_rectf_and_inline_color_dependencies_are_explicit(self):
        source = self.source('src/Visual.pas', """unit Visual;
interface
procedure Paint(const Bounds: TRectF; const Point: TPointF);
implementation
procedure Paint(const Bounds: TRectF; const Point: TPointF);
begin
  Color := ColorToARGB(Value);
  Color := PickColor(Value, Fallback);
end;
end.
""")
        self.assertIn('total: 2', self.checker('vcltypes'))
        self.assertIn('total: 2', self.checker('needsunit'))
        source.write_text(source.read_text().replace('interface\n',
                          'interface\nuses System.Types, System.UITypes;\n'))
        self.assertIn('total: 0', self.checker('vcltypes'))
        self.assertIn('total: 0', self.checker('needsunit'))

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

    def test_fpc_only_rtl_names_outside_fpc_branches(self):
        source = self.source('src/Fmt.pas', """unit Fmt;
interface
uses System.SysUtils;
implementation
function Settings: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
  Writeln('a' + LineEnding);
end;
function Size: {$IFDEF FPC}SizeInt{$ELSE}NativeInt{$ENDIF};
begin
  Result := 0;
end;
end.
""")
        self.assertIn('total: 2', self.checker('platformapi'))
        source.write_text(source.read_text()
                          .replace('DefaultFormatSettings', 'TFormatSettings.Invariant')
                          .replace('LineEnding', 'sLineBreak'))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_platform_attributes_and_misqualified_system_routines(self):
        source = self.source('src/Shell.pas', """unit Shell;
interface
implementation
function Hidden(A, B: Integer): Boolean;
begin
  Result := (A and (faHidden or faSysFile)) <> 0;
  Result := Result and (System.Math.Abs(B) > 0);
end;
end.
""")
        self.assertIn('total: 3', self.checker('platformapi'))
        source.write_text(source.read_text()
                          .replace('faHidden or faSysFile',
                                   'FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM')
                          .replace('System.Math.Abs(', 'Abs('))
        self.assertIn('total: 0', self.checker('platformapi'))

    def test_strict_private_field_is_class_scoped_within_a_unit(self):
        source = self.source('src/Dlg.pas', """unit Dlg;
interface
type
  TDlg = class
  strict private
    FOnApply: TNotifyEvent;
    FA, FB: Integer;
    procedure Run(X: Integer; Y: string);
  public
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;
  TController = class
  strict private
    FDialog: TDlg;
  public
    procedure Apply;
  end;
implementation
procedure TDlg.Run(X: Integer; Y: string);
begin
  FB := X;
  if Assigned(Self.FOnApply) then Self.FOnApply(Self);
end;
procedure TController.Apply;
begin
  if Assigned(FDialog.FOnApply) then
    FDialog.FB := 1;
end;
end.
""")
        self.assertIn('total: 2', self.checker('visibility'))
        source.write_text(source.read_text()
                          .replace('Assigned(FDialog.FOnApply)', 'Assigned(FDialog.OnApply)')
                          .replace('FDialog.FB := 1;', 'FDialog.OnApply(nil);'))
        self.assertIn('total: 0', self.checker('visibility'))

    def test_private_helper_shared_by_name_counts_per_class(self):
        source = self.source('src/Pair.pas', """unit Pair;
interface
type
  TOne = class
  strict private
    FName: string;
    function Text: string;
    procedure SetName(const AValue: string);
  public
    procedure Draw;
    property Name: string read FName write SetName;
  end;
  TTwo = class
  strict private
    function Text: string;
  public
    procedure Draw;
  end;
implementation
function TOne.Text: string;
begin
  Result := 'one';
end;
procedure TOne.SetName(const AValue: string);
begin
  FName := AValue;
end;
procedure TOne.Draw;
begin
end;
function TTwo.Text: string;
begin
  Result := 'two';
end;
procedure TTwo.Draw;
begin
  Text;
end;
end.
""")
        out = self.checker('private')
        self.assertIn('TOne.Text', out)
        self.assertNotIn('TTwo.Text', out)
        self.assertNotIn('SetName', out)
        source.write_text(source.read_text().replace(
            'procedure TOne.Draw;\nbegin\nend;', 'procedure TOne.Draw;\nbegin\n  Text;\nend;'))
        self.assertNotIn('TOne.Text', self.checker('private'))

    def test_property_must_not_hide_an_inherited_vcl_method(self):
        source = self.source('src/Row.pas', """unit Row;
interface
uses System.Classes;
type
  TRow = class(TCollectionItem)
  strict private
    FChanged: Boolean;
    FCaption: string;
  published
    property Changed: Boolean read FChanged write FChanged;
    property Caption: string read FCaption write FCaption;
  end;
implementation
end.
""")
        self.assertIn('total: 1', self.checker('hidemember'))
        source.write_text(source.read_text().replace('property Changed:', 'property IsChanged:'))
        self.assertIn('total: 0', self.checker('hidemember'))

    def test_property_field_must_have_the_property_type(self):
        source = self.source('src/Step.pas', """unit Step;
interface
uses System.Classes;
type
  TStep = class(TCollectionItem)
  strict private
    FLines: TStringList;
    FName: UnicodeString;
    FCount: Longint;
    function GetLines: TStrings;
  published
    property Lines: TStrings read FLines;
    property Name: string read FName write FName;
    property Count: Integer read FCount write FCount;
  end;
implementation
function TStep.GetLines: TStrings;
begin
  Result := FLines;
end;
end.
""")
        self.assertIn('total: 1', self.checker('proptype'))
        source.write_text(source.read_text().replace('read FLines;', 'read GetLines;'))
        self.assertIn('total: 0', self.checker('proptype'))

    def test_dfm_property_must_be_published_by_the_component(self):
        self.source('src/Tile.pas', """unit Tile;
interface
uses System.Classes, Vcl.Controls;
type
  TTileItem = class(TCollectionItem)
  published
    property Caption: string read FCaption write FCaption;
  end;
  TTileItems = class(TOwnedCollection)
  public
    property Items[Index: Integer]: TTileItem read GetItem; default;
  end;
  TTile = class(TCustomControl)
  public
    property Busy: Boolean read FBusy write FBusy;
  published
    property &Unit: string read FUnit write FUnit;
    property Tiles: TTileItems read FTiles write SetTiles;
  end;
implementation
end.
""")
        form = self.source('examples/app/Main.dfm', """object Main: TForm
  Caption = 'Main'
  object Tile1: TTile
    Left = 8
    Top = 8
    Unit = 'km/h'
    Busy = True
    Tiles = <
      item
        Caption = 'One'
        Subtitle = 'Two'
      end>
  end
end
""")
        out = self.checker('dfmpublished')
        self.assertIn('total: 2', out)
        self.assertIn('no published Busy', out)
        self.assertIn('no published Subtitle', out)
        form.write_text(form.read_text().replace("    Busy = True\n", '')
                        .replace("        Subtitle = 'Two'\n", ''))
        self.assertIn('total: 0', self.checker('dfmpublished'))

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
        consumer = self.source('examples/Demo/Consumer.pas', '''unit Consumer;
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

    def test_signature_guard_ignores_spacing_around_parameter_colon(self):
        path = self.source('src/SignatureSpacing.pas', """unit SignatureSpacing;
interface
type TDemo = class
  procedure Run(const A: string);
end;
implementation
procedure TDemo.Run(const A : string);
begin end;
end.
""")
        self.assertIn('total: 0', self.checker('signature'))
        path.write_text(path.read_text().replace('const A : string', 'const B : string'))
        self.assertIn('total: 1', self.checker('signature'))

    def test_platform_guard_detects_wrong_ide_target(self):
        for filename, targets in [
                ('packages/DelphiOBD_RT.dproj', ['Win32', 'Win64']),
                ('packages/DelphiOBD_DT.dproj', ['Win32', 'Win64x']),
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
        for name in ('caption', 'dfm', 'dfmpublished', 'dispatch'):
            with self.subTest(name=name):
                self.assertIn('total: 0', self.checker(name))


if __name__ == '__main__':
    unittest.main()
