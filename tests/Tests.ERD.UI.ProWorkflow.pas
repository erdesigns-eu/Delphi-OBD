//------------------------------------------------------------------------------
//  Tests.ERD.UI.ProWorkflow
//
//  Rendering and behaviour tests for TOBDPreconditionList,
//  TOBDSecurityAccessCard and TOBDWriteConfirm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.ProWorkflow;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Types,
  Vcl.Graphics,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.PreconditionList,
  ERD.UI.SecurityAccessCard,
  ERD.UI.WriteConfirm,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDPreconditionListTests = class
  strict private
    FList: TOBDPreconditionList;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsRows;
    [Test] procedure EmptyListStillDraws;
    [Test] procedure TabletDensityRendersDifferently;
    [Test] procedure RequiredCountsIgnoreNotRequired;
    [Test] procedure FailedConditionPreventsAllMet;
  end;

  [TestFixture]
  TOBDSecurityAccessCardTests = class
  strict private
    FCard: TOBDSecurityAccessCard;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsUnlockedCard;
    [Test] procedure EmptyCardStillDraws;
    [Test] procedure TabletDensityRendersDifferently;
    [Test] procedure KeepAliveProgressIsClamped;
    [Test] procedure UnlockedReflectsState;
  end;

  [TestFixture]
  TOBDWriteConfirmTests = class
  strict private
    FStrip: TOBDWriteConfirm;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure ReadyVariantDraws;
    [Test] procedure BackupVariantDraws;
    [Test] procedure TabletDensityRendersDifferently;
    [Test] procedure HitTestFindsPaintedButtons;
    [Test] procedure BackupVariantRendersDifferently;
  end;

implementation

{ TOBDPreconditionListTests }

procedure TOBDPreconditionListTests.Setup;
begin
  FList := TOBDPreconditionList.Create(nil);
  FList.SetBounds(0, 0, 520, 324);
end;

procedure TOBDPreconditionListTests.TearDown;
begin
  FreeAndNil(FList);
end;

procedure TOBDPreconditionListTests.PreviewDrawsRows;
begin
  FList.ForcePreview := True;
  Assert.IsTrue(InkRatio(FList) > 0.02, 'precondition preview is empty');
end;

procedure TOBDPreconditionListTests.EmptyListStillDraws;
begin
  Assert.AreEqual(0, FList.Items.Count);
  Assert.IsTrue(InkRatio(FList) > 0.005, 'empty precondition list is empty');
end;

procedure TOBDPreconditionListTests.TabletDensityRendersDifferently;
var
  Desktop, Tablet: TBitmap;
begin
  FList.ForcePreview := True;
  Desktop := RenderControl(FList);
  try
    FList.Density := dnTablet;
    Tablet := RenderControl(FList);
    try
      Assert.IsTrue(BitmapsDiffer(Desktop, Tablet, 100),
        'tablet density did not change precondition layout');
    finally
      Tablet.Free;
    end;
  finally
    Desktop.Free;
  end;
end;

procedure TOBDPreconditionListTests.RequiredCountsIgnoreNotRequired;
begin
  FList.AddPrecondition(pcnMet, 'Ignition on', 'Terminal 15');
  FList.AddPrecondition(pcnNotRequired, 'Doors closed', 'Not required');
  Assert.AreEqual(1, FList.RequiredCount);
  Assert.AreEqual(1, FList.MetCount);
  Assert.IsTrue(FList.AllMet);
end;

procedure TOBDPreconditionListTests.FailedConditionPreventsAllMet;
begin
  FList.AddPrecondition(pcnMet, 'Ignition on', 'Terminal 15');
  FList.AddPrecondition(pcnFailed, 'Battery charger', 'Not detected');
  Assert.AreEqual(2, FList.RequiredCount);
  Assert.AreEqual(1, FList.MetCount);
  Assert.IsFalse(FList.AllMet);
end;

{ TOBDSecurityAccessCardTests }

procedure TOBDSecurityAccessCardTests.Setup;
begin
  FCard := TOBDSecurityAccessCard.Create(nil);
  FCard.SetBounds(0, 0, 520, 184);
end;

procedure TOBDSecurityAccessCardTests.TearDown;
begin
  FreeAndNil(FCard);
end;

procedure TOBDSecurityAccessCardTests.PreviewDrawsUnlockedCard;
begin
  FCard.ForcePreview := True;
  Assert.IsTrue(InkRatio(FCard) > 0.02, 'security preview is empty');
  Assert.IsTrue(RenderedNear(FCard, BRAND_PALETTE_LIGHT.Success, 30) > 10,
    'unlocked preview did not draw success colour');
end;

procedure TOBDSecurityAccessCardTests.EmptyCardStillDraws;
begin
  Assert.IsFalse(FCard.HasData);
  Assert.IsTrue(InkRatio(FCard) > 0.005, 'empty security card is empty');
end;

procedure TOBDSecurityAccessCardTests.TabletDensityRendersDifferently;
var
  Desktop, Tablet: TBitmap;
begin
  FCard.ForcePreview := True;
  Desktop := RenderControl(FCard);
  try
    FCard.Density := dnTablet;
    Tablet := RenderControl(FCard);
    try
      Assert.IsTrue(BitmapsDiffer(Desktop, Tablet, 100),
        'tablet density did not change security layout');
    finally
      Tablet.Free;
    end;
  finally
    Desktop.Free;
  end;
end;

procedure TOBDSecurityAccessCardTests.KeepAliveProgressIsClamped;
begin
  FCard.KeepAliveProgress := 1.5;
  Assert.AreEqual(Double(1), FCard.KeepAliveProgress, 1e-9);
  FCard.KeepAliveProgress := -0.25;
  Assert.AreEqual(Double(0), FCard.KeepAliveProgress, 1e-9);
end;

procedure TOBDSecurityAccessCardTests.UnlockedReflectsState;
begin
  FCard.State := sacUnlocked;
  Assert.IsTrue(FCard.Unlocked);
  FCard.State := sacDenied;
  Assert.IsFalse(FCard.Unlocked);
end;

{ TOBDWriteConfirmTests }

procedure TOBDWriteConfirmTests.Setup;
begin
  FStrip := TOBDWriteConfirm.Create(nil);
  FStrip.SetBounds(0, 0, 720, 58);
end;

procedure TOBDWriteConfirmTests.TearDown;
begin
  FreeAndNil(FStrip);
end;

procedure TOBDWriteConfirmTests.ReadyVariantDraws;
begin
  FStrip.BackupDone := True;
  Assert.IsTrue(InkRatio(FStrip) > 0.02, 'ready write strip is empty');
end;

procedure TOBDWriteConfirmTests.BackupVariantDraws;
begin
  FStrip.BackupDone := False;
  Assert.IsTrue(InkRatio(FStrip) > 0.02, 'backup write strip is empty');
end;

procedure TOBDWriteConfirmTests.TabletDensityRendersDifferently;
var
  Desktop, Tablet: TBitmap;
begin
  Desktop := RenderControl(FStrip);
  try
    FStrip.Density := dnTablet;
    Tablet := RenderControl(FStrip);
    try
      Assert.IsTrue(BitmapsDiffer(Desktop, Tablet, 100),
        'tablet density did not change write strip layout');
    finally
      Tablet.Free;
    end;
  finally
    Desktop.Free;
  end;
end;

procedure TOBDWriteConfirmTests.HitTestFindsPaintedButtons;
var
  R: TRect;
  Bmp: TBitmap;
begin
  Bmp := RenderControl(FStrip);
  try
    R := FStrip.PrimaryButtonRect;
    Assert.AreEqual(Integer(wchPrimary), Integer(FStrip.HitTest(
      R.Left + R.Width div 2, R.Top + R.Height div 2)));
    R := FStrip.CancelButtonRect;
    Assert.AreEqual(Integer(wchCancel), Integer(FStrip.HitTest(
      R.Left + R.Width div 2, R.Top + R.Height div 2)));
  finally
    Bmp.Free;
  end;
end;

procedure TOBDWriteConfirmTests.BackupVariantRendersDifferently;
var
  Ready, Backup: TBitmap;
begin
  FStrip.BackupDone := True;
  Ready := RenderControl(FStrip);
  try
    FStrip.BackupDone := False;
    Backup := RenderControl(FStrip);
    try
      Assert.IsTrue(BitmapsDiffer(Ready, Backup, 200),
        'backup-required variant looks the same as ready');
    finally
      Backup.Free;
    end;
  finally
    Ready.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDPreconditionListTests);
  TDUnitX.RegisterTestFixture(TOBDSecurityAccessCardTests);
  TDUnitX.RegisterTestFixture(TOBDWriteConfirmTests);

end.
