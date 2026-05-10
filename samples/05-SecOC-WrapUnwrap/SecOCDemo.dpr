//------------------------------------------------------------------------------
//  SecOCDemo — sample 05
//
//  Demonstrates AUTOSAR SecOC end-to-end without any bus or hardware:
//  registers a 128-bit AES key against a Data ID, wraps a UDS
//  ReadDataByIdentifier request as an Authentic PDU (Original ||
//  Truncated FV || Truncated MAC), prints the wire bytes, then
//  unwraps and verifies the same buffer. Also shows that a single-bit
//  tamper trips the MAC check and that a replayed buffer trips the
//  freshness check.
//
//  Demonstrates the Phase 4e SecOC stack:
//    OBD.Protocol.SecOC.AES        (constant-time AES-128)
//    OBD.Protocol.SecOC.CMAC       (RFC 4493 / NIST SP 800-38B)
//    OBD.Protocol.SecOC.Keys       (in-memory key store)
//    OBD.Protocol.SecOC.Freshness  (in-memory freshness manager)
//    OBD.Protocol.SecOC            (wrap / unwrap codec)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

program SecOCDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.SecOC.AES        in '..\..\src\Protocol\OBD.Protocol.SecOC.AES.pas',
  OBD.Protocol.SecOC.CMAC       in '..\..\src\Protocol\OBD.Protocol.SecOC.CMAC.pas',
  OBD.Protocol.SecOC.Keys       in '..\..\src\Protocol\OBD.Protocol.SecOC.Keys.pas',
  OBD.Protocol.SecOC.Freshness  in '..\..\src\Protocol\OBD.Protocol.SecOC.Freshness.pas',
  OBD.Protocol.SecOC            in '..\..\src\Protocol\OBD.Protocol.SecOC.pas';

const
  /// Demo Data ID. In real systems this is the SecOC binding ID
  /// from the configuration.
  DataID  = $0E80;
  /// 128-bit AES-128 key. Replace with an HSM-backed key in
  /// production (the codec accepts any IOBDSecOCKeyProvider).
  DemoKey: TAES128Key =
    ($A0, $A1, $A2, $A3, $A4, $A5, $A6, $A7,
     $A8, $A9, $AA, $AB, $AC, $AD, $AE, $AF);

procedure DumpHex(const ALabel: string; const ABytes: TBytes);
var
  I: Integer;
begin
  Write(ALabel: 16, '  (', Length(ABytes): 3, ' B):  ');
  for I := 0 to High(ABytes) do
  begin
    if (I > 0) and (I mod 8 = 0) then Write(' ');
    Write(IntToHex(ABytes[I], 2));
  end;
  Writeln;
end;

procedure DemoSection(const ATitle: string);
begin
  Writeln;
  Writeln('--- ', ATitle, ' ', StringOfChar('-', 60 - Length(ATitle)));
end;

var
  Store: TOBDSecOCKeyStore;
  Fresh: TOBDSecOCFreshness;
  Codec: TOBDSecOCCodec;
  KeyProv: IOBDSecOCKeyProvider;
  FreshProv: IOBDSecOCFreshnessProvider;
  Original, Wire, Tampered: TBytes;
  Verified: TOBDSecOCVerification;
begin
  // 1. Configure providers.
  Store := TOBDSecOCKeyStore.Create;
  Fresh := TOBDSecOCFreshness.Create;
  KeyProv := Store;
  FreshProv := Fresh;
  Store.RegisterKey(DataID, DemoKey, 64, 16);

  // 2. Wire up the codec.
  Codec := TOBDSecOCCodec.Create(nil);
  try
    Codec.Keys      := KeyProv;
    Codec.Freshness := FreshProv;

    // 3. Build the application payload (UDS RDBI VIN, SID 0x22, DID 0xF190).
    Original := TBytes.Create($22, $F1, $90);

    DemoSection('Wrap');
    DumpHex('Original PDU', Original);
    Wire := Codec.Wrap(DataID, Original);
    DumpHex('Authentic PDU', Wire);
    Writeln('Layout         : <Original ', Length(Original),
            '> | <Truncated FV 2> | <Truncated MAC 8>');

    DemoSection('Unwrap');
    Verified := Codec.Unwrap(DataID, Wire);
    DumpHex('Recovered PDU', Verified.OriginalPDU);
    Writeln('Freshness V    : ', Verified.FreshnessValue);
    Writeln('Data ID        : 0x', IntToHex(Verified.DataID, 4));

    DemoSection('Tamper detection');
    Tampered := Copy(Wire, 0, Length(Wire));
    Tampered[0] := Tampered[0] xor $01; // flip a bit in the PDU
    try
      Codec.Unwrap(DataID, Tampered);
      Writeln('FAIL: tampered PDU should have raised');
    except
      on E: EOBDSecOCError do
        Writeln('OK: ', E.Message);
    end;

    DemoSection('Replay detection');
    // Re-feeding the same wire buffer fails because the freshness
    // counter has already advanced past it on the receiver side.
    try
      Codec.Unwrap(DataID, Wire);
      Writeln('FAIL: replay should have raised');
    except
      on E: EOBDSecOCError do
        Writeln('OK: ', E.Message);
    end;

    Writeln;
    Writeln('Demo complete.');
  finally
    Codec.Free;
  end;
end.
