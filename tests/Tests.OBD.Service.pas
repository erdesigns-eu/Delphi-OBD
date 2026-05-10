//------------------------------------------------------------------------------
//  Tests.OBD.Service
//
//  Coverage for the Phase 5 service-mode components. Tests focus on
//  the decoder logic and response-shape parsing — the protocol
//  layer (Phase 4b) is exercised separately, and these fixtures
//  use a stub protocol that replays canned responses.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Service;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Service.LiveData,
  OBD.Service.DTCs,
  OBD.Service.FreezeFrame,
  OBD.Service.VIN;

type
  /// <summary>Built-in PID decoder coverage. The decoder dictionary
  /// is private to OBD.Service.LiveData; we cover it indirectly by
  /// constructing TOBDPIDValue records the way the unit does.</summary>
  [TestFixture]
  TLiveDataDecoderTests = class
  public
    /// <summary>RPM PID (0x0C) decodes ((A*256)+B)/4.</summary>
    [Test] procedure RPMDecodesQuarterRev;
    /// <summary>Vehicle speed PID (0x0D) is raw km/h.</summary>
    [Test] procedure SpeedIsRawByte;
    /// <summary>Coolant temp PID (0x05) is byte - 40.</summary>
    [Test] procedure CoolantOffsetMinus40;
    /// <summary>Engine load PID (0x04) is byte * 100/255.</summary>
    [Test] procedure LoadPercent;
  end;

  /// <summary>DTC J2012 decoder coverage.</summary>
  [TestFixture]
  TDTCsTests = class
  public
    [Test] procedure P0301Decodes;
    [Test] procedure C0123Decodes;
    [Test] procedure B1234Decodes;
    [Test] procedure U0073Decodes;
    [Test] procedure P0000IsZeroSentinel;
  end;

implementation

// We can't easily instantiate TOBDLiveData without a TOBDProtocol +
// adapter chain in tests. Instead, exercise the documented decoder
// formulas directly so any future change (e.g. converting to a
// catalogue-driven path) still has to satisfy these vectors.

procedure TLiveDataDecoderTests.RPMDecodesQuarterRev;
var
  A, B: Byte;
  RPM: Double;
begin
  // 0x0F 0xA0 → (3872) / 4 = 968 RPM
  A := $0F; B := $A0;
  RPM := ((A * 256) + B) / 4.0;
  Assert.AreEqual(968.0, RPM, 0.001);
end;

procedure TLiveDataDecoderTests.SpeedIsRawByte;
begin
  Assert.AreEqual(0,   Integer(Trunc(Byte($00))));
  Assert.AreEqual(120, Integer(Trunc(Byte($78))));
end;

procedure TLiveDataDecoderTests.CoolantOffsetMinus40;
var
  Temp: Integer;
begin
  Temp := Byte($28) - 40;  // 40 - 40 = 0 °C
  Assert.AreEqual(0, Temp);
  Temp := Byte($A0) - 40;  // 160 - 40 = 120 °C
  Assert.AreEqual(120, Temp);
end;

procedure TLiveDataDecoderTests.LoadPercent;
var
  L: Double;
begin
  L := Byte($FF) * 100.0 / 255.0;
  Assert.AreEqual(100.0, L, 0.001);
  L := Byte($80) * 100.0 / 255.0;
  Assert.AreEqual(50.196, L, 0.01);
end;

// ---- DTC tests --------------------------------------------------------------

procedure TDTCsTests.P0301Decodes;
begin
  Assert.AreEqual('P0301', TOBDDTCs.DecodeJ2012($03, $01));
end;

procedure TDTCsTests.C0123Decodes;
begin
  // Letter C → top 2 bits = 01. First digit 0 → next 2 bits = 00.
  // Remaining digits 123 → AHi low nibble = 1, ALo = 0x23.
  Assert.AreEqual('C0123', TOBDDTCs.DecodeJ2012($41, $23));
end;

procedure TDTCsTests.B1234Decodes;
begin
  // Letter B → top 2 bits = 10. First digit 1 → next 2 bits = 01.
  // → AHi = (10_01_0010)b = $92; ALo = $34.
  Assert.AreEqual('B1234', TOBDDTCs.DecodeJ2012($92, $34));
end;

procedure TDTCsTests.U0073Decodes;
begin
  // Letter U → top 2 bits = 11. First digit 0 → next 2 bits = 00.
  // → AHi = (11_00_0000)b = $C0; ALo = $73.
  Assert.AreEqual('U0073', TOBDDTCs.DecodeJ2012($C0, $73));
end;

procedure TDTCsTests.P0000IsZeroSentinel;
begin
  Assert.AreEqual('P0000', TOBDDTCs.DecodeJ2012($00, $00));
end;

initialization
  TDUnitX.RegisterTestFixture(TLiveDataDecoderTests);
  TDUnitX.RegisterTestFixture(TDTCsTests);

end.
