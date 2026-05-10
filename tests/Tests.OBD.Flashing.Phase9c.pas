//------------------------------------------------------------------------------
//  Tests.OBD.Flashing.Phase9c
//
//  Phase 9c coverage: TOBDFlashPipeline safety surface. Without
//  a real ECU we cover the gates and the configuration-side
//  rejections; the wire-level happy path runs against a real
//  bench in the Phase 9f sample, gated behind the standing
//  hardware-loop deferral.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Flashing.Phase9c;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Flash.Phases,
  OBD.Flash.Pipeline;

type
  /// <summary>Pipeline safety surface.</summary>
  [TestFixture]
  TFlashPipelineTests = class
  public
    [Test] procedure FlashRaisesOnEmptyImage;
    [Test] procedure FlashRaisesWhenProtocolMissing;
    [Test] procedure FlashRequiresConfirmOrAutoExecute;
    [Test] procedure OnConfirmExecuteCanCancel;
    [Test] procedure ChecksAreExposed;
    [Test] procedure DefaultsArePerSpec;
  end;

implementation

procedure TFlashPipelineTests.FlashRaisesOnEmptyImage;
var
  P: TOBDFlashPipeline;
begin
  P := TOBDFlashPipeline.Create(nil);
  try
    P.AutoExecute := True;
    Assert.WillRaise(
      procedure begin P.Flash($00100000, nil); end,
      EOBDConfig);
  finally
    P.Free;
  end;
end;

procedure TFlashPipelineTests.FlashRaisesWhenProtocolMissing;
var
  P: TOBDFlashPipeline;
begin
  P := TOBDFlashPipeline.Create(nil);
  try
    P.AutoExecute := True;
    Assert.WillRaise(
      procedure begin
        P.Flash($00100000, TBytes.Create($00, $01));
      end,
      EOBDConfig);
  finally
    P.Free;
  end;
end;

procedure TFlashPipelineTests.FlashRequiresConfirmOrAutoExecute;
var
  P: TOBDFlashPipeline;
begin
  P := TOBDFlashPipeline.Create(nil);
  try
    Assert.IsFalse(P.AutoExecute);
    // No OnConfirmExecute handler + AutoExecute=False → abort.
    Assert.WillRaise(
      procedure begin
        P.Flash($00100000, TBytes.Create($00, $01));
      end,
      EOBDConfig);
  finally
    P.Free;
  end;
end;

procedure TFlashPipelineTests.OnConfirmExecuteCanCancel;
var
  P: TOBDFlashPipeline;
  Fired: Boolean;
begin
  P := TOBDFlashPipeline.Create(nil);
  try
    Fired := False;
    P.OnConfirmExecute :=
      procedure(Sender: TObject; A: UInt64; S: UInt32; var Allow: Boolean)
      begin
        Fired := True;
        Allow := False;
      end;
    Assert.WillRaise(
      procedure begin
        P.Flash($00100000, TBytes.Create($00, $01));
      end,
      EOBDConfig);
    Assert.IsTrue(Fired,
      'OnConfirmExecute must fire even when the user cancels');
  finally
    P.Free;
  end;
end;

procedure TFlashPipelineTests.ChecksAreExposed;
var
  P: TOBDFlashPipeline;
begin
  P := TOBDFlashPipeline.Create(nil);
  try
    Assert.IsNotNull(P.Checks);
    P.Checks.AddCheck(fpPreflight, csInfo, 'noop',
      function(out M: string): Boolean
      begin M := ''; Result := True; end);
    Assert.AreEqual(1, P.Checks.PhaseCount(fpPreflight));
  finally
    P.Free;
  end;
end;

procedure TFlashPipelineTests.DefaultsArePerSpec;
var
  P: TOBDFlashPipeline;
begin
  P := TOBDFlashPipeline.Create(nil);
  try
    Assert.IsFalse(P.AutoExecute, 'AutoExecute must default to False');
    Assert.IsTrue (P.ResetAfterFlash);
    Assert.AreEqual(4, Integer(P.AddressFormatBytes));
    Assert.AreEqual(4, Integer(P.LengthFormatBytes));
  finally
    P.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFlashPipelineTests);

end.
