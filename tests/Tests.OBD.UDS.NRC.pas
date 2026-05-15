//------------------------------------------------------------------------------
//  Tests.OBD.UDS.NRC
//
//  Coverage for the NRC catalogue helpers: synthetic fallback
//  for uncatalogued codes, format-line output, and the
//  transient-NRC classifier.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.UDS.NRC;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  OBD.UDS.NRC;

type
  [TestFixture]
  TNRCTests = class
  public
    /// <summary>Unknown NRCs return a synthetic entry tagged
    /// <c>nrcReserved</c>.</summary>
    [Test] procedure DescribeNRC_Unknown_ReturnsSynthetic;
    /// <summary>FormatNRC renders <c>NRC 0xNN (short:
    /// description)</c>.</summary>
    [Test] procedure FormatNRC_RendersDecoratedLine;
    /// <summary>The four documented retry-friendly NRCs are
    /// classified transient.</summary>
    [Test] procedure IsTransientNRC_Retryable;
    /// <summary>Common non-transient NRCs are not classified
    /// transient.</summary>
    [Test] procedure IsTransientNRC_NotRetryable;
  end;

implementation

procedure TNRCTests.DescribeNRC_Unknown_ReturnsSynthetic;
var
  Info: TOBDUDSNrcInfo;
begin
  // 0xFE is not allocated by ISO 14229; expect the synthetic
  // fallback rather than a raise.
  Info := DescribeNRC($FE);
  Assert.AreEqual(Integer($FE), Integer(Info.Code));
  Assert.AreEqual(nrcReserved, Info.Category);
  Assert.IsTrue(Pos('NRC_0xfe', Info.ShortName) > 0);
end;

procedure TNRCTests.FormatNRC_RendersDecoratedLine;
var
  Line: string;
begin
  Line := FormatNRC($FE);
  // Format is "NRC 0xFE (NRC_0xfe: ...)".
  Assert.IsTrue(Pos('NRC 0xfe', Line) > 0);
  Assert.IsTrue(Pos('(', Line) > 0);
  Assert.IsTrue(Pos(')', Line) > 0);
end;

procedure TNRCTests.IsTransientNRC_Retryable;
begin
  Assert.IsTrue(IsTransientNRC($21));  // busyRepeatRequest
  Assert.IsTrue(IsTransientNRC($22));  // conditionsNotCorrect
  Assert.IsTrue(IsTransientNRC($78));  // responsePending
  Assert.IsTrue(IsTransientNRC($94));  // resourceTemporarilyUnavailable
end;

procedure TNRCTests.IsTransientNRC_NotRetryable;
begin
  Assert.IsFalse(IsTransientNRC($10));  // generalReject
  Assert.IsFalse(IsTransientNRC($33));  // securityAccessDenied
  Assert.IsFalse(IsTransientNRC($35));  // invalidKey
  Assert.IsFalse(IsTransientNRC($36));  // exceededNumberOfAttempts
  Assert.IsFalse(IsTransientNRC($7F));  // serviceNotSupportedInActiveSession
end;

initialization
  TDUnitX.RegisterTestFixture(TNRCTests);

end.
