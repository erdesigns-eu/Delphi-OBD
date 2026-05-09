//------------------------------------------------------------------------------
// UNIT           : Tests.Tachograph.Workshop
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Tachograph.Workshop;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTachographWorkshopTests = class
  public
    [Test] procedure UTCSyncRoundTrip;
    [Test] procedure UTCSyncBadCardIdRaises;
    [Test] procedure KLWRoundTrip;
    [Test] procedure KOutOfRangeRaises;
    [Test] procedure TyreSizeRoundTrip;
    [Test] procedure TyreOutOfRangeRaises;
    [Test] procedure VINRoundTrip;
    [Test] procedure VINBadLengthRaises;
    [Test] procedure VRPlateRoundTrip;
    [Test] procedure VRPlateTooLongRaises;
    [Test] procedure SealedActivationLayout;
    [Test] procedure DateTimeToTimeRealRoundTrips;
  end;

implementation

uses
  System.SysUtils, System.DateUtils,
  OBD.Tachograph.Workshop;

procedure TTachographWorkshopTests.UTCSyncRoundTrip;
var
  In_, Out_: TTachoUTCSync;
  Bytes: TBytes;
begin
  In_.UTCTimestamp := 1700000000;
  SetLength(In_.WorkshopCardId, 16);
  In_.WorkshopCardId[0] := $AA;
  In_.WorkshopCardId[15] := $77;
  Bytes := EncodeUTCSync(In_);
  Assert.AreEqual(20, Length(Bytes));
  Out_ := DecodeUTCSync(Bytes);
  Assert.AreEqual(In_.UTCTimestamp, Out_.UTCTimestamp);
  Assert.AreEqual(Integer($AA), Integer(Out_.WorkshopCardId[0]));
  Assert.AreEqual(Integer($77), Integer(Out_.WorkshopCardId[15]));
end;

procedure TTachographWorkshopTests.UTCSyncBadCardIdRaises;
var Op: TTachoUTCSync;
begin
  Op.UTCTimestamp := 0;
  SetLength(Op.WorkshopCardId, 8);
  Assert.WillRaise(
    procedure begin EncodeUTCSync(Op); end, EOBDTachoWorkshop);
end;

procedure TTachographWorkshopTests.KLWRoundTrip;
var
  In_, Out_: TTachoKLWFactors;
  Bytes: TBytes;
begin
  In_.K := 8000;
  In_.L := 200000 mod $10000;  // L is uint16; example value within range
  In_.W := 8200;
  Bytes := EncodeKLW(In_);
  Assert.AreEqual(6, Length(Bytes));
  Out_ := DecodeKLW(Bytes);
  Assert.AreEqual(In_.K, Out_.K);
  Assert.AreEqual(In_.L, Out_.L);
  Assert.AreEqual(In_.W, Out_.W);
end;

procedure TTachographWorkshopTests.KOutOfRangeRaises;
var Op: TTachoKLWFactors;
begin
  Op.K := 100;
  Op.L := 0;
  Op.W := 0;
  Assert.WillRaise(
    procedure begin EncodeKLW(Op); end, EOBDTachoWorkshop);
end;

procedure TTachographWorkshopTests.TyreSizeRoundTrip;
var
  In_, Out_: TTachoTyreSize;
  Bytes: TBytes;
begin
  In_.CircumferenceMm := 2050;
  Bytes := EncodeTyreSize(In_);
  Assert.AreEqual(2, Length(Bytes));
  Out_ := DecodeTyreSize(Bytes);
  Assert.AreEqual(2050, Integer(Out_.CircumferenceMm));
end;

procedure TTachographWorkshopTests.TyreOutOfRangeRaises;
var Op: TTachoTyreSize;
begin
  Op.CircumferenceMm := 500;
  Assert.WillRaise(
    procedure begin EncodeTyreSize(Op); end, EOBDTachoWorkshop);
end;

procedure TTachographWorkshopTests.VINRoundTrip;
var
  In_, Out_: TTachoVINUpdate;
  Bytes: TBytes;
begin
  In_.VIN := 'WVWZZZ8N8Z1234567';
  Bytes := EncodeVIN(In_);
  Assert.AreEqual(17, Length(Bytes));
  Out_ := DecodeVIN(Bytes);
  Assert.AreEqual('WVWZZZ8N8Z1234567', Out_.VIN);
end;

procedure TTachographWorkshopTests.VINBadLengthRaises;
var Op: TTachoVINUpdate;
begin
  Op.VIN := 'TOO-SHORT';
  Assert.WillRaise(
    procedure begin EncodeVIN(Op); end, EOBDTachoWorkshop);
end;

procedure TTachographWorkshopTests.VRPlateRoundTrip;
var
  In_, Out_: TTachoVRPlate;
  Bytes: TBytes;
begin
  In_.PlateText := 'NL-12-AB-34';
  In_.NationalSymbol := $1F; // arbitrary
  Bytes := EncodeVRPlate(In_);
  Out_ := DecodeVRPlate(Bytes);
  Assert.AreEqual('NL-12-AB-34', Out_.PlateText);
  Assert.AreEqual(Integer($1F), Integer(Out_.NationalSymbol));
end;

procedure TTachographWorkshopTests.VRPlateTooLongRaises;
var Op: TTachoVRPlate;
begin
  Op.PlateText := 'THIS-PLATE-IS-TOO-LONG-EXCEEDS-13';
  Op.NationalSymbol := 0;
  Assert.WillRaise(
    procedure begin EncodeVRPlate(Op); end, EOBDTachoWorkshop);
end;

procedure TTachographWorkshopTests.SealedActivationLayout;
var
  Op: TTachoSealedActivation;
  Bytes: TBytes;
begin
  Op.UTCTimestamp := 1700000000;
  SetLength(Op.WorkshopCardId, 16);
  Op.PostSealNote := 'OK';
  Bytes := EncodeSealedActivation(Op);
  // 4 + 16 + 1 (note-len) + 2 (note bytes) = 23
  Assert.AreEqual(23, Length(Bytes));
  Assert.AreEqual(2, Integer(Bytes[20]));
  Assert.AreEqual(Integer(Ord('O')), Integer(Bytes[21]));
  Assert.AreEqual(Integer(Ord('K')), Integer(Bytes[22]));
end;

procedure TTachographWorkshopTests.DateTimeToTimeRealRoundTrips;
var
  T: TDateTime;
  R: UInt32;
begin
  T := EncodeDate(2026, 5, 9) + EncodeTime(12, 0, 0, 0);
  R := DateTimeToTimeReal(T);
  Assert.AreEqual(SecondsBetween(EncodeDate(2026, 5, 9) + EncodeTime(12, 0, 0, 0),
                                 TimeRealToDateTime(R)), Int64(0));
end;

initialization
  TDUnitX.RegisterTestFixture(TTachographWorkshopTests);

end.
