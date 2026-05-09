//------------------------------------------------------------------------------
// UNIT           : Tests.UDS.NRC.pas
// CONTENTS       : Tests for OBD.UDS.NRC
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.UDS.NRC;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TUDSNrcTests = class
  public
    [Test] procedure DescribeKnownGeneralRejectByName;
    [Test] procedure DescribeSecurityAccessDenied;
    [Test] procedure DescribeRequestCorrectlyReceivedResponsePending;
    [Test] procedure DescribeReservedFallsBack;
    [Test] procedure FormatProducesHexAndShortName;
    [Test] procedure TransientNRCDetected;
    [Test] procedure NonTransientNotFlagged;
    [Test] procedure SecurityCategoryClassifiedCorrectly;
    [Test] procedure ConditionCategoryClassifiedCorrectly;
  end;

implementation

uses
  System.SysUtils, OBD.UDS.NRC;

procedure TUDSNrcTests.DescribeKnownGeneralRejectByName;
var Info: TOBDUDSNrcInfo;
begin
  Info := DescribeNRC($10);
  Assert.AreEqual('GR', Info.ShortName);
  Assert.AreEqual('generalReject', Info.Description);
  Assert.AreEqual(Ord(nrcGeneral), Ord(Info.Category));
end;

procedure TUDSNrcTests.DescribeSecurityAccessDenied;
var Info: TOBDUDSNrcInfo;
begin
  Info := DescribeNRC($33);
  Assert.AreEqual('SAD', Info.ShortName);
  Assert.IsTrue(Info.Description.Contains('securityAccessDenied'));
  Assert.AreEqual(Ord(nrcSecurity), Ord(Info.Category));
end;

procedure TUDSNrcTests.DescribeRequestCorrectlyReceivedResponsePending;
var Info: TOBDUDSNrcInfo;
begin
  Info := DescribeNRC($78);
  Assert.AreEqual('RCRRP', Info.ShortName);
  Assert.AreEqual(Ord(nrcCondition), Ord(Info.Category));
end;

procedure TUDSNrcTests.DescribeReservedFallsBack;
var Info: TOBDUDSNrcInfo;
begin
  Info := DescribeNRC($AB);
  Assert.AreEqual(Ord(nrcReserved), Ord(Info.Category));
  Assert.IsTrue(Info.Description.Contains('AB'));
end;

procedure TUDSNrcTests.FormatProducesHexAndShortName;
begin
  Assert.IsTrue(FormatNRC($35).Contains('0x35'));
  Assert.IsTrue(FormatNRC($35).Contains('IK'));
  Assert.IsTrue(FormatNRC($35).Contains('invalidKey'));
end;

procedure TUDSNrcTests.TransientNRCDetected;
begin
  Assert.IsTrue(IsTransientNRC($21));   // BRR
  Assert.IsTrue(IsTransientNRC($22));   // CNC
  Assert.IsTrue(IsTransientNRC($78));   // RCRRP
  Assert.IsTrue(IsTransientNRC($94));   // RTNT
end;

procedure TUDSNrcTests.NonTransientNotFlagged;
begin
  Assert.IsFalse(IsTransientNRC($10));  // GR
  Assert.IsFalse(IsTransientNRC($33));  // SAD
  Assert.IsFalse(IsTransientNRC($72));  // GPF
end;

procedure TUDSNrcTests.SecurityCategoryClassifiedCorrectly;
const
  SecurityNrcs: array[0..6] of Byte = ($33, $34, $35, $36, $37, $38, $5A);
var
  N: Byte;
begin
  for N in SecurityNrcs do
    Assert.AreEqual(Ord(nrcSecurity), Ord(DescribeNRC(N).Category),
      Format('NRC 0x%.2x should be security', [N]));
end;

procedure TUDSNrcTests.ConditionCategoryClassifiedCorrectly;
const
  CondNrcs: array[0..6] of Byte = ($21, $22, $24, $78, $7E, $7F, $81);
var
  N: Byte;
begin
  for N in CondNrcs do
    Assert.AreEqual(Ord(nrcCondition), Ord(DescribeNRC(N).Category),
      Format('NRC 0x%.2x should be condition', [N]));
end;

initialization
  TDUnitX.RegisterTestFixture(TUDSNrcTests);

end.
