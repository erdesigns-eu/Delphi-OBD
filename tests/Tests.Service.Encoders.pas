//------------------------------------------------------------------------------
// UNIT           : Tests.Service.Encoders
// CONTENTS       : Golden tests for OBD-II Service request encoders (01..0A)
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Service.Encoders;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TServiceEncoderTests = class
  public
    [Test]
    [TestCase('Service01_PID0C', '01,0C,010C')]
    [TestCase('Service01_PID00', '01,00,0100')]
    [TestCase('Service02_PID02', '02,02,0202')]
    [TestCase('Service03_PID00', '03,00,0300')]
    [TestCase('Service04_PID00', '04,00,0400')]
    [TestCase('Service05_PID00', '05,00,0500')]
    [TestCase('Service06_PID00', '06,00,0600')]
    [TestCase('Service07_PID00', '07,00,0700')]
    [TestCase('Service08_PID00', '08,00,0800')]
    [TestCase('Service09_PID02', '09,02,0902')]
    [TestCase('Service0A_PID00', '0A,00,0A00')]
    procedure EncodeServiceRequest_NoData_ProducesExpectedHex(
      const Service, PID, Expected: string);

    [Test]
    procedure EncodeServiceRequest_WithData_AppendsHex;

    [Test]
    procedure EncodeServiceRequest_WithEmptyData_MatchesNoDataForm;
  end;

implementation

uses
  System.SysUtils,
  OBD.Request.Encoders;

function HexToByte(const S: string): Byte;
begin
  Result := StrToInt('$' + S);
end;

{ TServiceEncoderTests }

procedure TServiceEncoderTests.EncodeServiceRequest_NoData_ProducesExpectedHex(
  const Service, PID, Expected: string);
var
  Encoder: TOBDServiceRequestEncoder;
begin
  // Pick the right encoder for the service-id string.
  case HexToByte(Service) of
    $01: Encoder := TOBDService01RequestEncoder.Create;
    $02: Encoder := TOBDService02RequestEncoder.Create;
    $03: Encoder := TOBDService03RequestEncoder.Create;
    $04: Encoder := TOBDService04RequestEncoder.Create;
    $05: Encoder := TOBDService05RequestEncoder.Create;
    $06: Encoder := TOBDService06RequestEncoder.Create;
    $07: Encoder := TOBDService07RequestEncoder.Create;
    $08: Encoder := TOBDService08RequestEncoder.Create;
    $09: Encoder := TOBDService09RequestEncoder.Create;
    $0A: Encoder := TOBDService0ARequestEncoder.Create;
  else
    Assert.Fail('Unknown service id: ' + Service);
    Exit;
  end;

  try
    Assert.AreEqual(Expected, Encoder.EncodeServiceRequest(HexToByte(PID)));
  finally
    Encoder.Free;
  end;
end;

procedure TServiceEncoderTests.EncodeServiceRequest_WithData_AppendsHex;
var
  Encoder: TOBDService01RequestEncoder;
  Data: TBytes;
begin
  // Service 01 PID 0C with two extra data bytes ($AB,$CD) → "010CABCD"
  Encoder := TOBDService01RequestEncoder.Create;
  try
    Data := TBytes.Create($AB, $CD);
    Assert.AreEqual('010CABCD', Encoder.EncodeServiceRequest($0C, Data));
  finally
    Encoder.Free;
  end;
end;

procedure TServiceEncoderTests.EncodeServiceRequest_WithEmptyData_MatchesNoDataForm;
var
  Encoder: TOBDService01RequestEncoder;
  Data: TBytes;
begin
  Encoder := TOBDService01RequestEncoder.Create;
  try
    SetLength(Data, 0);
    Assert.AreEqual(
      Encoder.EncodeServiceRequest($0C),
      Encoder.EncodeServiceRequest($0C, Data));
  finally
    Encoder.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TServiceEncoderTests);

end.
