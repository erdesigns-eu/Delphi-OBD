//------------------------------------------------------------------------------
//  Tests.ERD.UI.ProHistory
//
//  Rendering and behaviour tests for TOBDProcedureView, TOBDTimeline and
//  TOBDDiffView. The controls are rendered off-screen in preview, empty and
//  tablet-density states, and their model APIs are checked.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.ProHistory;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  Vcl.Graphics,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.ProcedureView,
  ERD.UI.Timeline,
  ERD.UI.DiffView,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDProcedureViewTests = class
  strict private
    FView: TOBDProcedureView;
    FStepChanged: Integer;
    FStepIndex: Integer;
    procedure StepChanged(Sender: TObject; AIndex: Integer);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsWorkflow;
    [Test] procedure EmptyViewStillDraws;
    [Test] procedure TabletDensityChangesRendering;
    [Test] procedure AddStepAndLiveValueUpdateCollections;
    [Test] procedure CurrentStepClampsAndFiresEvent;
  end;

  [TestFixture]
  TOBDTimelineTests = class
  strict private
    FTimeline: TOBDTimeline;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsTimeline;
    [Test] procedure EmptyTimelineStillDraws;
    [Test] procedure TabletDensityChangesRendering;
    [Test] procedure AddEntryPopulatesModel;
    [Test] procedure ErrorFilterChangesRendering;
  end;

  [TestFixture]
  TOBDDiffViewTests = class
  strict private
    FDiff: TOBDDiffView;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsDiff;
    [Test] procedure EmptyDiffStillDraws;
    [Test] procedure TabletDensityChangesRendering;
    [Test] procedure ChangedCountTracksRows;
    [Test] procedure OnlyChangesChangesRendering;
  end;

implementation

{ TOBDProcedureViewTests ---------------------------------------------------- }

procedure TOBDProcedureViewTests.Setup;
begin
  FView := TOBDProcedureView.Create(nil);
  FView.SetBounds(0, 0, 780, 430);
  FStepChanged := 0;
  FStepIndex := -1;
end;

procedure TOBDProcedureViewTests.TearDown;
begin
  FreeAndNil(FView);
end;

procedure TOBDProcedureViewTests.StepChanged(Sender: TObject; AIndex: Integer);
begin
  Inc(FStepChanged);
  FStepIndex := AIndex;
end;

procedure TOBDProcedureViewTests.PreviewDrawsWorkflow;
begin
  FView.ForcePreview := True;
  Assert.IsTrue(InkRatio(FView) > 0.02, 'preview procedure view is empty');
end;

procedure TOBDProcedureViewTests.EmptyViewStillDraws;
begin
  Assert.AreEqual(0, FView.Steps.Count);
  Assert.IsTrue(InkRatio(FView) > 0.002, 'empty procedure view is empty');
end;

procedure TOBDProcedureViewTests.TabletDensityChangesRendering;
var
  DesktopBmp: TBitmap;
  TabletBmp: TBitmap;
begin
  FView.ForcePreview := True;
  FView.Density := dnDesktop;
  DesktopBmp := RenderControl(FView);
  try
    FView.Density := dnTablet;
    TabletBmp := RenderControl(FView);
    try
      Assert.IsTrue(BitmapsDiffer(DesktopBmp, TabletBmp, 100), 'tablet density did not change rendering');
    finally
      TabletBmp.Free;
    end;
  finally
    DesktopBmp.Free;
  end;
end;

procedure TOBDProcedureViewTests.AddStepAndLiveValueUpdateCollections;
var
  Step: TOBDProcedureStep;
  Value: TOBDProcedureLiveValue;
begin
  Step := FView.AddStep('Open', 'Open the calipers');
  Step.Instructions.Add('Press the button.');
  Value := FView.AddLiveValue('Battery', '12.7 V', opvSuccess, 2.0);
  Assert.AreEqual(1, FView.Steps.Count);
  Assert.AreEqual(1, Step.Instructions.Count);
  Assert.AreEqual(1, FView.LiveValues.Count);
  Assert.AreEqual(Double(1.0), Value.Fraction, 1e-9);
  Assert.IsTrue(Value.ShowBar);
end;

procedure TOBDProcedureViewTests.CurrentStepClampsAndFiresEvent;
begin
  FView.AddStep('One', 'First');
  FView.AddStep('Two', 'Second');
  FView.OnStepChange := StepChanged;
  FView.CurrentStep := 99;
  Assert.AreEqual(1, FView.CurrentStep);
  Assert.AreEqual(1, FStepChanged);
  Assert.AreEqual(1, FStepIndex);
end;

{ TOBDTimelineTests --------------------------------------------------------- }

procedure TOBDTimelineTests.Setup;
begin
  FTimeline := TOBDTimeline.Create(nil);
  FTimeline.SetBounds(0, 0, 440, 420);
end;

procedure TOBDTimelineTests.TearDown;
begin
  FreeAndNil(FTimeline);
end;

procedure TOBDTimelineTests.PreviewDrawsTimeline;
begin
  FTimeline.ForcePreview := True;
  Assert.IsTrue(InkRatio(FTimeline) > 0.02, 'preview timeline is empty');
end;

procedure TOBDTimelineTests.EmptyTimelineStillDraws;
begin
  Assert.AreEqual(0, FTimeline.Entries.Count);
  Assert.IsTrue(InkRatio(FTimeline) > 0.002, 'empty timeline is empty');
end;

procedure TOBDTimelineTests.TabletDensityChangesRendering;
var
  DesktopBmp: TBitmap;
  TabletBmp: TBitmap;
begin
  FTimeline.ForcePreview := True;
  FTimeline.Density := dnDesktop;
  DesktopBmp := RenderControl(FTimeline);
  try
    FTimeline.Density := dnTablet;
    TabletBmp := RenderControl(FTimeline);
    try
      Assert.IsTrue(BitmapsDiffer(DesktopBmp, TabletBmp, 100), 'tablet density did not change rendering');
    finally
      TabletBmp.Free;
    end;
  finally
    DesktopBmp.Free;
  end;
end;

procedure TOBDTimelineTests.AddEntryPopulatesModel;
var
  Entry: TOBDTimelineEntry;
begin
  Entry := FTimeline.AddEntry('18:52', otlWrite, 'Coding written', '3 bytes', 'System');
  Assert.AreEqual(1, FTimeline.Entries.Count);
  Assert.AreEqual(Integer(glWrite), Integer(Entry.Glyph));
  Assert.AreEqual('Coding written', Entry.Title);
end;

procedure TOBDTimelineTests.ErrorFilterChangesRendering;
var
  AllBmp: TBitmap;
  ErrorBmp: TBitmap;
begin
  FTimeline.ForcePreview := True;
  FTimeline.Filter := otlAll;
  AllBmp := RenderControl(FTimeline);
  try
    FTimeline.Filter := otlErrors;
    ErrorBmp := RenderControl(FTimeline);
    try
      Assert.IsTrue(BitmapsDiffer(AllBmp, ErrorBmp, 100), 'error filter did not change rendering');
    finally
      ErrorBmp.Free;
    end;
  finally
    AllBmp.Free;
  end;
end;

{ TOBDDiffViewTests --------------------------------------------------------- }

procedure TOBDDiffViewTests.Setup;
begin
  FDiff := TOBDDiffView.Create(nil);
  FDiff.SetBounds(0, 0, 440, 420);
end;

procedure TOBDDiffViewTests.TearDown;
begin
  FreeAndNil(FDiff);
end;

procedure TOBDDiffViewTests.PreviewDrawsDiff;
begin
  FDiff.ForcePreview := True;
  Assert.IsTrue(InkRatio(FDiff) > 0.02, 'preview diff is empty');
end;

procedure TOBDDiffViewTests.EmptyDiffStillDraws;
begin
  Assert.AreEqual(0, FDiff.Rows.Count);
  Assert.IsTrue(InkRatio(FDiff) > 0.002, 'empty diff view is empty');
end;

procedure TOBDDiffViewTests.TabletDensityChangesRendering;
var
  DesktopBmp: TBitmap;
  TabletBmp: TBitmap;
begin
  FDiff.ForcePreview := True;
  FDiff.Density := dnDesktop;
  DesktopBmp := RenderControl(FDiff);
  try
    FDiff.Density := dnTablet;
    TabletBmp := RenderControl(FDiff);
    try
      Assert.IsTrue(BitmapsDiffer(DesktopBmp, TabletBmp, 100), 'tablet density did not change rendering');
    finally
      TabletBmp.Free;
    end;
  finally
    DesktopBmp.Free;
  end;
end;

procedure TOBDDiffViewTests.ChangedCountTracksRows;
begin
  FDiff.AddRow('Byte 0', 'Fog lights', 'Yes', 'Yes', False);
  FDiff.AddRow('Byte 2 . bit 4', 'Coming home', 'Off', 'On', True);
  FDiff.AddRow('Byte 2 . bit 5', 'Leaving home', 'Off', 'On', True);
  Assert.AreEqual(3, FDiff.Rows.Count);
  Assert.AreEqual(2, FDiff.ChangedCount);
end;

procedure TOBDDiffViewTests.OnlyChangesChangesRendering;
var
  AllBmp: TBitmap;
  ChangedBmp: TBitmap;
begin
  FDiff.ForcePreview := True;
  FDiff.OnlyChanges := False;
  AllBmp := RenderControl(FDiff);
  try
    FDiff.OnlyChanges := True;
    ChangedBmp := RenderControl(FDiff);
    try
      Assert.IsTrue(BitmapsDiffer(AllBmp, ChangedBmp, 100), 'only changes did not change rendering');
    finally
      ChangedBmp.Free;
    end;
  finally
    AllBmp.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDProcedureViewTests);
  TDUnitX.RegisterTestFixture(TOBDTimelineTests);
  TDUnitX.RegisterTestFixture(TOBDDiffViewTests);

end.
