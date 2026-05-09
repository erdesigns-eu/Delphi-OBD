//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.KeyAdaptation.Ford.pas
// CONTENTS       : Tests for OBD.OEM.KeyAdaptation.Ford
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.KeyAdaptation.Ford;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TFordPATSTests = class
  public
    /// <summary>
    ///   Request round trip.
    /// </summary>
    [Test] procedure RequestRoundTrip;
    /// <summary>
    ///   Request rejects bad v i n.
    /// </summary>
    [Test] procedure RequestRejectsBadVIN;
    /// <summary>
    ///   Request decode bad length raises.
    /// </summary>
    [Test] procedure RequestDecodeBadLengthRaises;
    /// <summary>
    ///   Status round trip.
    /// </summary>
    [Test] procedure StatusRoundTrip;
    /// <summary>
    ///   Status decode bad length raises.
    /// </summary>
    [Test] procedure StatusDecodeBadLengthRaises;
    /// <summary>
    ///   F150 is open.
    /// </summary>
    [Test] procedure F150IsOpen;
    /// <summary>
    ///   Mach e is gateway locked.
    /// </summary>
    [Test] procedure MachEIsGatewayLocked;
    /// <summary>
    ///   Unknown is gateway locked.
    /// </summary>
    [Test] procedure UnknownIsGatewayLocked;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.KeyAdaptation.Ford;

//------------------------------------------------------------------------------
// REQUEST ROUND TRIP
//------------------------------------------------------------------------------
procedure TFordPATSTests.RequestRoundTrip;
var
  In_, Out_: TFordPATSRequest;
  Bytes: TBytes;
begin
  In_.VIN := '1FTFW1ET8DKE12345';
  In_.Operation := fpoAddKey;
  In_.ProgrammerPresentByte := $42;
  Bytes := EncodeFordPATSRequest(In_);
  Out_ := DecodeFordPATSRequest(Bytes);
  Assert.AreEqual('1FTFW1ET8DKE12345', Out_.VIN);
  Assert.AreEqual(Ord(fpoAddKey), Ord(Out_.Operation));
  Assert.AreEqual(Integer($42), Integer(Out_.ProgrammerPresentByte));
end;

//------------------------------------------------------------------------------
// REQUEST REJECTS BAD VIN
//------------------------------------------------------------------------------
procedure TFordPATSTests.RequestRejectsBadVIN;
var
  Req: TFordPATSRequest;
begin
  Req.VIN := 'TOO-SHORT';
  Req.Operation := fpoStatus;
  Req.ProgrammerPresentByte := 0;
  Assert.WillRaise(
    procedure begin EncodeFordPATSRequest(Req); end, EOBDFordPATS);
end;

//------------------------------------------------------------------------------
// REQUEST DECODE BAD LENGTH RAISES
//------------------------------------------------------------------------------
procedure TFordPATSTests.RequestDecodeBadLengthRaises;
begin
  Assert.WillRaise(
    procedure begin DecodeFordPATSRequest(TBytes.Create($00, $00, $00)); end,
    EOBDFordPATS);
end;

//------------------------------------------------------------------------------
// STATUS ROUND TRIP
//------------------------------------------------------------------------------
procedure TFordPATSTests.StatusRoundTrip;
var
  In_, Out_: TFordPATSStatus;
  Bytes: TBytes;
begin
  In_.KeyCount := 4;
  In_.LockoutActive := True;
  In_.SecondsRemaining := 600;
  In_.PinCodePresent := True;
  Bytes := EncodeFordPATSStatus(In_);
  Out_ := DecodeFordPATSStatus(Bytes);
  Assert.AreEqual(Integer(4), Integer(Out_.KeyCount));
  Assert.IsTrue(Out_.LockoutActive);
  Assert.AreEqual(Word(600), Out_.SecondsRemaining);
  Assert.IsTrue(Out_.PinCodePresent);
end;

//------------------------------------------------------------------------------
// STATUS DECODE BAD LENGTH RAISES
//------------------------------------------------------------------------------
procedure TFordPATSTests.StatusDecodeBadLengthRaises;
begin
  Assert.WillRaise(
    procedure begin DecodeFordPATSStatus(TBytes.Create($00, $00, $00, $00)); end,
    EOBDFordPATS);
end;

//------------------------------------------------------------------------------
// F150 IS OPEN
//------------------------------------------------------------------------------
procedure TFordPATSTests.F150IsOpen;
var
  P: TFordPlatformInfo;
begin
  P := FindFordPlatform('p552');
  Assert.AreEqual(Ord(fpaOpen), Ord(P.Access));
end;

//------------------------------------------------------------------------------
// MACH EIS GATEWAY LOCKED
//------------------------------------------------------------------------------
procedure TFordPATSTests.MachEIsGatewayLocked;
var
  P: TFordPlatformInfo;
begin
  P := FindFordPlatform('cd542');
  Assert.AreEqual(Ord(fpaGatewayLocked), Ord(P.Access));
end;

//------------------------------------------------------------------------------
// UNKNOWN IS GATEWAY LOCKED
//------------------------------------------------------------------------------
procedure TFordPATSTests.UnknownIsGatewayLocked;
var
  P: TFordPlatformInfo;
begin
  P := FindFordPlatform('unknown-chassis');
  Assert.AreEqual(Ord(fpaGatewayLocked), Ord(P.Access));
end;

initialization
  TDUnitX.RegisterTestFixture(TFordPATSTests);

end.
