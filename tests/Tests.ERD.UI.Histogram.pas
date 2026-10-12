// ------------------------------------------------------------------------------
// Tests.ERD.UI.Histogram
//
// DUnitX coverage for TOBDHistogram binning, auto range, ring buffer,
// statistics and normal-range classification.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit Tests.ERD.UI.Histogram;

interface

uses
  System.SysUtils,
  System.Math,
  DUnitX.TestFramework,
  ERD.UI.Histogram;

type
  [TestFixture]
  TOBDHistogramStatisticsTests = class
  public
    [Test] procedure PercentilesAndSpread;
  end;

  [TestFixture]
  TOBDHistogramTests = class
  strict private
    FHistogram: TOBDHistogram;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure BinningCountsValues;
    [Test] procedure AutoRangeExpandsConstantValues;
    [Test] procedure RingBufferDropsOldestSamples;
    [Test] procedure StatisticsProperties;
    [Test] procedure NormalRangeClassifiesBins;
  end;

implementation

{ TOBDHistogramStatisticsTests }

procedure TOBDHistogramStatisticsTests.PercentilesAndSpread;
var
  Stats: TOBDHistogramStatistics;
begin
  Stats := TOBDHistogramStatistics.Calculate([1, 2, 3, 4]);
  Assert.AreEqual(4, Stats.Count);
  Assert.AreEqual(Double(2.5), Stats.Mean, 1e-9);
  Assert.AreEqual(Double(1), Stats.Minimum, 1e-9);
  Assert.AreEqual(Double(4), Stats.Maximum, 1e-9);
  Assert.AreEqual(Double(2.5), Stats.Median, 1e-9);
  Assert.AreEqual(Sqrt(1.25), Stats.StdDev, 1e-9);
  Assert.AreEqual(Double(3.25), Stats.Percentile(75), 1e-9);
end;

{ TOBDHistogramTests }

procedure TOBDHistogramTests.Setup;
begin
  FHistogram := TOBDHistogram.Create(nil);
  FHistogram.AutoRange := False;
  FHistogram.Min := 0;
  FHistogram.Max := 100;
  FHistogram.BinCount := 5;
end;

procedure TOBDHistogramTests.TearDown;
begin
  FreeAndNil(FHistogram);
end;

procedure TOBDHistogramTests.BinningCountsValues;
var
  Bins: TArray<TOBDHistogramBin>;
begin
  FHistogram.AddSamples([0, 1, 19.9, 20, 50, 100]);
  Bins := FHistogram.GetBins;
  Assert.AreEqual(5, Length(Bins));
  Assert.AreEqual(3, Bins[0].Count);
  Assert.AreEqual(1, Bins[1].Count);
  Assert.AreEqual(1, Bins[2].Count);
  Assert.AreEqual(0, Bins[3].Count);
  Assert.AreEqual(1, Bins[4].Count);
  Assert.AreEqual(Double(0), Bins[0].LowValue, 1e-9);
  Assert.AreEqual(Double(100), Bins[4].HighValue, 1e-9);
end;

procedure TOBDHistogramTests.AutoRangeExpandsConstantValues;
var
  Bins: TArray<TOBDHistogramBin>;
begin
  FHistogram.AutoRange := True;
  FHistogram.AddSamples([42, 42]);
  Bins := FHistogram.GetBins;
  Assert.AreEqual(5, Length(Bins));
  Assert.IsTrue(Bins[0].LowValue < 42);
  Assert.IsTrue(Bins[High(Bins)].HighValue > 42);
  Assert.AreEqual(2, FHistogram.Count);
end;

procedure TOBDHistogramTests.RingBufferDropsOldestSamples;
begin
  FHistogram.MaxSamples := 3;
  FHistogram.AddSamples([1, 2, 3, 4]);
  Assert.AreEqual(3, FHistogram.Count);
  Assert.AreEqual(Double(2), FHistogram.Sample(0), 1e-9);
  Assert.AreEqual(Double(4), FHistogram.Sample(2), 1e-9);
end;

procedure TOBDHistogramTests.StatisticsProperties;
begin
  FHistogram.AddSamples([1, 2, 3, 4]);
  Assert.AreEqual(4, FHistogram.Count);
  Assert.AreEqual(Double(2.5), FHistogram.Mean, 1e-9);
  Assert.AreEqual(Double(1), FHistogram.Minimum, 1e-9);
  Assert.AreEqual(Double(4), FHistogram.Maximum, 1e-9);
  Assert.AreEqual(Double(2.5), FHistogram.Median, 1e-9);
  Assert.AreEqual(Sqrt(1.25), FHistogram.StdDev, 1e-9);
  Assert.AreEqual(Double(3.25), FHistogram.Percentile(75), 1e-9);
end;

procedure TOBDHistogramTests.NormalRangeClassifiesBins;
var
  Bins: TArray<TOBDHistogramBin>;
begin
  FHistogram.ShowNormalRange := True;
  FHistogram.NormalLow := 20;
  FHistogram.NormalHigh := 80;
  FHistogram.AddSamples([10, 30, 50, 90]);
  Bins := FHistogram.GetBins;
  Assert.IsTrue(Bins[0].OutsideNormal);
  Assert.IsFalse(Bins[1].OutsideNormal);
  Assert.IsFalse(Bins[2].OutsideNormal);
  Assert.IsFalse(Bins[3].OutsideNormal);
  Assert.IsTrue(Bins[4].OutsideNormal);
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDHistogramStatisticsTests);
  TDUnitX.RegisterTestFixture(TOBDHistogramTests);

end.
