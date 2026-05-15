//------------------------------------------------------------------------------
//  Tests.OBD.UI.LegacyVisualsTier5
//
//  Coverage for the second round of visual ports: TOBDKnob and
//  TOBDTrendGraph. The painted output is form-rendered and not
//  exercised here; this fixture covers the state machines and
//  public API surface.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.LegacyVisualsTier5;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Knob,
  OBD.UI.TrendGraph;

type
  /// <summary>DUnitX fixture for the second-round visual ports.</summary>
  [TestFixture]
  TLegacyVisualsTier5Tests = class
  public
    [Test] procedure Knob_DefaultsMinMaxValueStep;
    [Test] procedure Knob_SetValueClampsToRange;
    [Test] procedure Knob_OnChangeFiresOnChange;
    [Test] procedure Knob_StepRejectsNonPositive;
    [Test] procedure Knob_SetMinPullsValueUp;
    [Test] procedure Knob_SetMaxPullsValueDown;

    [Test] procedure Trend_AddSeriesAndPush;
    [Test] procedure Trend_RingBufferOverwritesOldest;
    [Test] procedure Trend_ClearAllSamplesEmpties;
    [Test] procedure Trend_BadIndexRaises;
    [Test] procedure Trend_SeriesValuesReadOldestFirst;
  end;

implementation

{ ---- TOBDKnob ----------------------------------------------------------- }

procedure TLegacyVisualsTier5Tests.Knob_DefaultsMinMaxValueStep;
var
  K: TOBDKnob;
begin
  K := TOBDKnob.Create(nil);
  try
    Assert.AreEqual(KNOB_DEFAULT_MIN,  K.Min,  0.001);
    Assert.AreEqual(KNOB_DEFAULT_MAX,  K.Max,  0.001);
    Assert.AreEqual(KNOB_DEFAULT_MIN,  K.Value, 0.001);
    Assert.AreEqual(KNOB_DEFAULT_STEP, K.Step,  0.001);
  finally
    K.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Knob_SetValueClampsToRange;
var
  K: TOBDKnob;
begin
  K := TOBDKnob.Create(nil);
  try
    K.Value := 1000;
    Assert.AreEqual(K.Max, K.Value, 0.001);
    K.Value := -500;
    Assert.AreEqual(K.Min, K.Value, 0.001);
  finally
    K.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Knob_OnChangeFiresOnChange;
var
  K: TOBDKnob;
  Hits: Integer;
begin
  K := TOBDKnob.Create(nil);
  try
    Hits := 0;
    K.OnChange :=
      procedure(Sender: TObject; AValue: Single)
      begin
        Inc(Hits);
      end;
    K.Value := 25;
    K.Value := 25;                       // same value — no fire
    K.Value := 50;
    Assert.AreEqual(2, Hits);
  finally
    K.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Knob_StepRejectsNonPositive;
var
  K: TOBDKnob;
begin
  K := TOBDKnob.Create(nil);
  try
    K.Step := 0;
    Assert.IsTrue(K.Step > 0);
    K.Step := -5;
    Assert.IsTrue(K.Step > 0);
  finally
    K.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Knob_SetMinPullsValueUp;
var
  K: TOBDKnob;
begin
  K := TOBDKnob.Create(nil);
  try
    K.Value := 5;
    K.Min := 10;
    Assert.AreEqual(10.0, K.Value, 0.001);
  finally
    K.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Knob_SetMaxPullsValueDown;
var
  K: TOBDKnob;
begin
  K := TOBDKnob.Create(nil);
  try
    K.Value := 80;
    K.Max := 50;
    Assert.AreEqual(50.0, K.Value, 0.001);
  finally
    K.Free;
  end;
end;

{ ---- TOBDTrendGraph ----------------------------------------------------- }

procedure TLegacyVisualsTier5Tests.Trend_AddSeriesAndPush;
var
  G: TOBDTrendGraph;
  S: TOBDTrendSeries;
begin
  G := TOBDTrendGraph.Create(nil);
  try
    S := G.AddSeries('RPM', clRed, 0, 8000);
    Assert.AreEqual(1, G.SeriesCount);
    Assert.AreEqual('RPM', S.Name);
    G.PushValue(0, 1500);
    G.PushValue(0, 2500);
    Assert.AreEqual(2, S.Count);
    Assert.AreEqual(2500.0, S.Values[1], 0.001);
  finally
    G.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Trend_RingBufferOverwritesOldest;
var
  G: TOBDTrendGraph;
  S: TOBDTrendSeries;
  I: Integer;
begin
  G := TOBDTrendGraph.Create(nil);
  try
    G.MaxSamples := 4;
    S := G.AddSeries('X', clBlue, 0, 100);
    for I := 1 to 6 do
      G.PushValue(0, I);
    Assert.AreEqual(4, S.Count);
    Assert.AreEqual(3.0, S.Values[0], 0.001);
    Assert.AreEqual(6.0, S.Values[3], 0.001);
  finally
    G.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Trend_ClearAllSamplesEmpties;
var
  G: TOBDTrendGraph;
begin
  G := TOBDTrendGraph.Create(nil);
  try
    G.AddSeries('A', clGreen, 0, 1);
    G.PushValue(0, 0.5);
    G.ClearAllSamples;
    Assert.AreEqual(0, G.Series(0).Count);
  finally
    G.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Trend_BadIndexRaises;
var
  G: TOBDTrendGraph;
begin
  G := TOBDTrendGraph.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        G.PushValue(0, 0);                // no series yet
      end,
      EArgumentOutOfRangeException);
  finally
    G.Free;
  end;
end;

procedure TLegacyVisualsTier5Tests.Trend_SeriesValuesReadOldestFirst;
var
  G: TOBDTrendGraph;
  S: TOBDTrendSeries;
  I: Integer;
begin
  G := TOBDTrendGraph.Create(nil);
  try
    G.MaxSamples := 3;
    S := G.AddSeries('Y', clYellow, 0, 100);
    for I := 1 to 5 do
      G.PushValue(0, I * 10);
    // Buffer holds [30, 40, 50] (3, 4, 5 × 10 after ring rolled).
    Assert.AreEqual(30.0, S.Values[0], 0.001);
    Assert.AreEqual(40.0, S.Values[1], 0.001);
    Assert.AreEqual(50.0, S.Values[2], 0.001);
  finally
    G.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLegacyVisualsTier5Tests);

end.
