//------------------------------------------------------------------------------
//  Tests.OBD.Speciality
//
//  Smoke tests for the Speciality family:
//    Tachograph (regulation 165/2014 record decoders)
//    Tachograph PCSC (Windows-only smartcard reader wrapper)
//    IsoBus (ISO 11783 address claim + NAME codec)
//    IsoBus VT  (Virtual Terminal)
//    IsoBus TC  (Task Controller)
//    IsoBus FS  (File Server)
//    IsoBus GNSS (NMEA 2000 PGNs republished as ISO 11783)
//
//  Most of the surface is pure class-static codec helpers
//  (build / decode payloads), so the tests exercise them
//  directly with synthetic bytes. The PCSC unit is
//  Windows-only and has no usable smoke without a
//  reader-loop test bench - covered by a build-only check.
//------------------------------------------------------------------------------

unit Tests.OBD.Speciality;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils,
  DUnitX.TestFramework,
  OBD.Speciality.Tachograph,
  {$IFDEF MSWINDOWS}
  OBD.Speciality.Tachograph.PCSC,
  {$ENDIF}
  OBD.Speciality.IsoBus,
  OBD.Speciality.IsoBus.VT,
  OBD.Speciality.IsoBus.TC,
  OBD.Speciality.IsoBus.FS,
  OBD.Speciality.IsoBus.GNSS;

type
  [TestFixture]
  TTachographTests = class
  public
    [Test] procedure DecodeTimeReal_2024_01_01_00_00_00;
    [Test] procedure EncodeTimeReal_RoundTrips;
    [Test] procedure EncodeTimeReal_DecodeTimeReal_Inverse;
  end;

  [TestFixture]
  TIsoBusNameTests = class
  public
    [Test] procedure EncodeNameProducesEightBytes;
    [Test] procedure DecodeNameRoundTripsEncoding;
    [Test] procedure NameLessThanIsAntisymmetric;
    [Test] procedure BuildAddressClaimPayloadIsEightBytes;
    [Test] procedure RegistryRoundTripCreateFree;
  end;

  [TestFixture]
  TIsoBusVTTests = class
  public
    [Test] procedure BuildGetMemoryProducesEightByteFrame;
    [Test] procedure BuildAudioSignalProducesEightByteFrame;
    [Test] procedure BuildEndOfObjectPoolProducesEightByteFrame;
  end;

  [TestFixture]
  TIsoBusTCTests = class
  public
    [Test] procedure BuildValueProducesEightByteFrame;
    [Test] procedure BuildSetValueProducesEightByteFrame;
    [Test] procedure BuildRequestVersionProducesEightByteFrame;
  end;

  [TestFixture]
  TIsoBusFSTests = class
  public
    [Test] procedure BuildGetPropertiesProducesEightByteFrame;
    [Test] procedure BuildCloseFileProducesEightByteFrame;
  end;

  [TestFixture]
  TIsoBusGNSSTests = class
  public
    [Test] procedure DecodePositionRapid_AcceptsLatLonBytes;
    [Test] procedure DecodeCogSog_AcceptsCogSogBytes;
  end;

  {$IFDEF MSWINDOWS}
  [TestFixture]
  TTachoPCSCTests = class
  public
    [Test] procedure CreateFree_IsClean;
  end;
  {$ENDIF}

implementation

{ ---- Tachograph ----------------------------------------------------------- }

procedure TTachographTests.DecodeTimeReal_2024_01_01_00_00_00;
var
  Reg: Cardinal;
  Got: TDateTime;
  Expect: TDateTime;
begin
  // TimeReal is seconds since 1970-01-01 00:00:00 UTC. Use a
  // round value: 2024-01-01 00:00:00 UTC = 1704067200 seconds.
  Reg := 1704067200;
  Got := TOBDTachograph.DecodeTimeReal(Reg);
  Expect := EncodeDateTime(2024, 1, 1, 0, 0, 0, 0);
  Assert.AreEqual(Expect, Got, 1.0 / SecsPerDay,
    'DecodeTimeReal should return 2024-01-01 UTC for 1704067200');
end;

procedure TTachographTests.EncodeTimeReal_RoundTrips;
var
  Now_: TDateTime;
  Reg:  Cardinal;
  Back: TDateTime;
begin
  Now_ := EncodeDateTime(2026, 5, 10, 12, 0, 0, 0);
  Reg := TOBDTachograph.EncodeTimeReal(Now_);
  Back := TOBDTachograph.DecodeTimeReal(Reg);
  Assert.AreEqual(Now_, Back, 1.0 / SecsPerDay);
end;

procedure TTachographTests.EncodeTimeReal_DecodeTimeReal_Inverse;
var R: Cardinal;
begin
  // Edge: epoch 0 = 1970-01-01.
  R := TOBDTachograph.EncodeTimeReal(EncodeDate(1970, 1, 1));
  Assert.AreEqual<Cardinal>(0, R);
end;

{ ---- IsoBus NAME --------------------------------------------------------- }

function MakeName: TOBDIsoBusName;
begin
  Result := Default(TOBDIsoBusName);
  Result.IdentityNumber := $123456;
  Result.ManufacturerCode := $321;
  Result.EcuInstance := 1;
  Result.FunctionInstance := 5;
  Result.Function_ := 130;
  Result.DeviceClass := 50;
  Result.DeviceClassInstance := 2;
  Result.IndustryGroup := 2;            // Agricultural & Forestry
  Result.SelfConfigurableAddress := True;
end;

procedure TIsoBusNameTests.EncodeNameProducesEightBytes;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBus.EncodeName(MakeName);
  Assert.AreEqual(8, Length(Bytes));
end;

procedure TIsoBusNameTests.DecodeNameRoundTripsEncoding;
var
  N1, N2: TOBDIsoBusName;
  Bytes:  TBytes;
begin
  N1 := MakeName;
  Bytes := TOBDIsoBus.EncodeName(N1);
  Assert.IsTrue(TOBDIsoBus.DecodeName(Bytes, N2));
  Assert.AreEqual<Cardinal>(N1.IdentityNumber, N2.IdentityNumber);
  Assert.AreEqual(Word(N1.ManufacturerCode), Word(N2.ManufacturerCode));
  Assert.AreEqual(Byte(N1.Function_),       Byte(N2.Function_));
  Assert.AreEqual(N1.SelfConfigurableAddress, N2.SelfConfigurableAddress);
end;

procedure TIsoBusNameTests.NameLessThanIsAntisymmetric;
var A, B: TOBDIsoBusName;
begin
  A := MakeName;
  B := MakeName;
  B.IdentityNumber := A.IdentityNumber + 1;
  // ISO 11783 NAME comparison: lower NAME wins. With everything
  // else equal, lower IdentityNumber sorts lower.
  Assert.IsTrue (TOBDIsoBus.NameLessThan(A, B));
  Assert.IsFalse(TOBDIsoBus.NameLessThan(B, A));
end;

procedure TIsoBusNameTests.BuildAddressClaimPayloadIsEightBytes;
var
  Bus: TOBDIsoBus;
  Pay: TBytes;
begin
  Bus := TOBDIsoBus.Create(nil);
  try
    Pay := Bus.BuildAddressClaim;
    Assert.AreEqual(8, Length(Pay));
  finally
    Bus.Free;
  end;
end;

procedure TIsoBusNameTests.RegistryRoundTripCreateFree;
var Bus: TOBDIsoBus;
begin
  Bus := TOBDIsoBus.Create(nil);
  try
    Assert.IsFalse(Bus.IsClaimed($80));
  finally
    Bus.Free;
  end;
end;

{ ---- IsoBus VT ----------------------------------------------------------- }

procedure TIsoBusVTTests.BuildGetMemoryProducesEightByteFrame;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBusVT.BuildGetMemory(65536);
  Assert.AreEqual(8, Length(Bytes));
end;

procedure TIsoBusVTTests.BuildAudioSignalProducesEightByteFrame;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBusVT.BuildAudioSignal(3, 1000, 200, 100);
  Assert.AreEqual(8, Length(Bytes));
end;

procedure TIsoBusVTTests.BuildEndOfObjectPoolProducesEightByteFrame;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBusVT.BuildEndOfObjectPool;
  Assert.AreEqual(8, Length(Bytes));
end;

{ ---- IsoBus TC ----------------------------------------------------------- }

procedure TIsoBusTCTests.BuildValueProducesEightByteFrame;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBusTC.BuildValue(1, 100, 12345);
  Assert.AreEqual(8, Length(Bytes));
end;

procedure TIsoBusTCTests.BuildSetValueProducesEightByteFrame;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBusTC.BuildSetValue(1, 100, 12345);
  Assert.AreEqual(8, Length(Bytes));
end;

procedure TIsoBusTCTests.BuildRequestVersionProducesEightByteFrame;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBusTC.BuildRequestVersion;
  Assert.AreEqual(8, Length(Bytes));
end;

{ ---- IsoBus FS ----------------------------------------------------------- }

procedure TIsoBusFSTests.BuildGetPropertiesProducesEightByteFrame;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBusFS.BuildGetProperties(1);
  Assert.AreEqual(8, Length(Bytes));
end;

procedure TIsoBusFSTests.BuildCloseFileProducesEightByteFrame;
var Bytes: TBytes;
begin
  Bytes := TOBDIsoBusFS.BuildCloseFile(1, 7);
  Assert.AreEqual(8, Length(Bytes));
end;

{ ---- IsoBus GNSS --------------------------------------------------------- }

procedure TIsoBusGNSSTests.DecodePositionRapid_AcceptsLatLonBytes;
var
  // PGN 129025 Position Rapid: 8-byte payload, 4 bytes lat,
  // 4 bytes lon, both little-endian, units 1e-7 degrees.
  Bytes: TBytes;
  Lat, Lon: Double;
begin
  // Lat = 50 deg = 500_000_000 = 0x1DCD6500 LE
  // Lon = 5 deg  = 50_000_000  = 0x02FAF080 LE
  Bytes := TBytes.Create($00,$65,$CD,$1D,  $80,$F0,$FA,$02);
  Assert.IsTrue(TOBDIsoBusGNSS.DecodePositionRapid(Bytes, Lat, Lon));
  Assert.AreEqual(50.0, Lat, 0.0001);
  Assert.AreEqual( 5.0, Lon, 0.0001);
end;

procedure TIsoBusGNSSTests.DecodeCogSog_AcceptsCogSogBytes;
var
  Bytes: TBytes;
  CogRad, SogMps: Double;
begin
  // PGN 129026 Cog/Sog: SID(1) + CogRef(1) + Reserved(1) +
  // Cog(2 LE, 1e-4 rad) + Sog(2 LE, 1e-2 m/s) + 1 reserved.
  // Cog = 1 rad = 10000 = 0x2710
  // Sog = 5 m/s = 500   = 0x01F4
  Bytes := TBytes.Create($00, $00, $FF,
                         $10, $27, $F4, $01, $FF);
  Assert.IsTrue(TOBDIsoBusGNSS.DecodeCogSog(Bytes, CogRad, SogMps));
  Assert.AreEqual(1.0, CogRad, 0.001);
  Assert.AreEqual(5.0, SogMps, 0.01);
end;

{$IFDEF MSWINDOWS}
procedure TTachoPCSCTests.CreateFree_IsClean;
var T: TOBDTachoPCSC;
begin
  T := TOBDTachoPCSC.Create(nil);
  try
    Assert.IsNotNull(T);
  finally
    T.Free;
  end;
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTachographTests);
  TDUnitX.RegisterTestFixture(TIsoBusNameTests);
  TDUnitX.RegisterTestFixture(TIsoBusVTTests);
  TDUnitX.RegisterTestFixture(TIsoBusTCTests);
  TDUnitX.RegisterTestFixture(TIsoBusFSTests);
  TDUnitX.RegisterTestFixture(TIsoBusGNSSTests);
  {$IFDEF MSWINDOWS}
  TDUnitX.RegisterTestFixture(TTachoPCSCTests);
  {$ENDIF}

end.
