//------------------------------------------------------------------------------
//  Tests.ERD.UI.Binding
//
//  Coverage for the shared data contract of the dashboard controls:
//  metric / imperial unit conversion, number formatting, the channel
//  binding (value, unit, stale and clear) and warning / alarm
//  thresholds.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Binding;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Units,
  ERD.UI.Binding,
  ERD.UI.Gauges.Base,
  ERD.Service.LiveData;

type
  [TestFixture]
  TOBDUnitConversionTests = class
  public
    [Test] procedure MetricIsIdentity;
    [Test] procedure CelsiusToFahrenheit;
    [Test] procedure KmhToMph;
    [Test] procedure KPaToPsi;
    [Test] procedure UnknownUnitPassesThrough;
    [Test] procedure FromDisplayInvertsToDisplay;
    [Test] procedure SpanIgnoresOffset;
    [Test] procedure FormatNumberUsesDotAndDecimals;
    [Test] procedure FormatNumberClampsDecimals;
  end;

  [TestFixture]
  TOBDChannelBindingTests = class
  strict private
    FOwner: TComponent;
    FBinding: TOBDChannelBinding;
    FValueEvents: Integer;
    FStateEvents: Integer;
    procedure HandleValue(Sender: TObject);
    procedure HandleState(Sender: TObject);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure StartsWithoutValue;
    [Test] procedure DefaultStaleAfterIs3000;
    [Test] procedure PushValueStoresValueAndFires;
    [Test] procedure PushPIDValueKeepsUnitAndDescription;
    [Test] procedure NaNValueKeepsPreviousValue;
    [Test] procedure ClearDropsValueAndFiresStateChange;
    [Test] procedure GoesStaleAfterTimeout;
    [Test] procedure ZeroStaleAfterNeverStale;
    [Test] procedure AssignCopiesPIDAndStaleAfter;
  end;

  [TestFixture]
  TOBDGaugeAlertsTests = class
  public
    [Test] procedure NoKindsIsAlwaysNormal;
    [Test] procedure HighWarningAndAlarm;
    [Test] procedure LowWarningAndAlarm;
    [Test] procedure NaNIsNormal;
    [Test] procedure ChangeEventFires;
  end;

implementation

type
  TChangeCounter = class
  public
    Count: Integer;
    procedure Handle(Sender: TObject);
  end;

procedure TChangeCounter.Handle(Sender: TObject);
begin
  Inc(Count);
end;

{ TOBDUnitConversionTests ---------------------------------------------------- }

procedure TOBDUnitConversionTests.MetricIsIdentity;
var
  C: TOBDUnitConversion;
begin
  C := OBDUnitConversion('km/h', usMetric);
  Assert.AreEqual('km/h', C.DisplayUnit);
  Assert.AreEqual(Double(88), C.ToDisplay(88), 1e-9);
end;

procedure TOBDUnitConversionTests.CelsiusToFahrenheit;
var
  C: TOBDUnitConversion;
begin
  C := OBDUnitConversion(OBD_DEGREE_SIGN + 'C', usImperial);
  Assert.AreEqual(OBD_DEGREE_SIGN + 'F', C.DisplayUnit);
  Assert.AreEqual(Double(212), C.ToDisplay(100), 1e-6);
  Assert.AreEqual(Double(32), C.ToDisplay(0), 1e-6);
end;

procedure TOBDUnitConversionTests.KmhToMph;
var
  C: TOBDUnitConversion;
begin
  C := OBDUnitConversion('km/h', usImperial);
  Assert.AreEqual('mph', C.DisplayUnit);
  Assert.AreEqual(Double(62.1371192), C.ToDisplay(100), 1e-4);
end;

procedure TOBDUnitConversionTests.KPaToPsi;
var
  C: TOBDUnitConversion;
begin
  C := OBDUnitConversion('kPa', usImperial);
  Assert.AreEqual('psi', C.DisplayUnit);
  Assert.AreEqual(Double(14.5037738), C.ToDisplay(100), 1e-4);
end;

procedure TOBDUnitConversionTests.UnknownUnitPassesThrough;
var
  C: TOBDUnitConversion;
begin
  C := OBDUnitConversion('rpm', usImperial);
  Assert.AreEqual('rpm', C.DisplayUnit);
  Assert.AreEqual(Double(3000), C.ToDisplay(3000), 1e-9);
end;

procedure TOBDUnitConversionTests.FromDisplayInvertsToDisplay;
var
  C: TOBDUnitConversion;
begin
  C := OBDUnitConversion(OBD_DEGREE_SIGN + 'C', usImperial);
  Assert.AreEqual(Double(90), C.FromDisplay(C.ToDisplay(90)), 1e-6);
end;

procedure TOBDUnitConversionTests.SpanIgnoresOffset;
var
  C: TOBDUnitConversion;
begin
  C := OBDUnitConversion(OBD_DEGREE_SIGN + 'C', usImperial);
  Assert.AreEqual(Double(18), C.SpanToDisplay(10), 1e-6);
end;

procedure TOBDUnitConversionTests.FormatNumberUsesDotAndDecimals;
begin
  Assert.AreEqual('12.5', OBDFormatNumber(12.5, 1));
  Assert.AreEqual('3000', OBDFormatNumber(2999.6, 0));
  Assert.AreEqual('-4.25', OBDFormatNumber(-4.25, 2));
end;

procedure TOBDUnitConversionTests.FormatNumberClampsDecimals;
begin
  Assert.AreEqual('13', OBDFormatNumber(12.6, -3));
  Assert.AreEqual('1.000000', OBDFormatNumber(1, 12));
end;

{ TOBDChannelBindingTests ---------------------------------------------------- }

procedure TOBDChannelBindingTests.Setup;
begin
  FOwner := TComponent.Create(nil);
  FBinding := TOBDChannelBinding.Create(FOwner);
  FBinding.OnValue := HandleValue;
  FBinding.OnStateChange := HandleState;
  FValueEvents := 0;
  FStateEvents := 0;
end;

procedure TOBDChannelBindingTests.TearDown;
begin
  FreeAndNil(FBinding);
  FreeAndNil(FOwner);
end;

procedure TOBDChannelBindingTests.HandleValue(Sender: TObject);
begin
  Inc(FValueEvents);
end;

procedure TOBDChannelBindingTests.HandleState(Sender: TObject);
begin
  Inc(FStateEvents);
end;

procedure TOBDChannelBindingTests.StartsWithoutValue;
begin
  Assert.IsFalse(FBinding.HasValue);
  Assert.IsTrue(IsNan(FBinding.Value));
  Assert.IsFalse(FBinding.IsStale);
  Assert.IsNull(FBinding.Source);
end;

procedure TOBDChannelBindingTests.DefaultStaleAfterIs3000;
begin
  Assert.AreEqual(Cardinal(3000), FBinding.StaleAfterMs);
end;

procedure TOBDChannelBindingTests.PushValueStoresValueAndFires;
begin
  FBinding.PushValue(42.5);
  Assert.IsTrue(FBinding.HasValue);
  Assert.AreEqual(Double(42.5), FBinding.Value, 1e-9);
  Assert.AreEqual(1, FValueEvents);
  Assert.IsFalse(FBinding.IsStale);
end;

procedure TOBDChannelBindingTests.PushPIDValueKeepsUnitAndDescription;
var
  V: TOBDPIDValue;
begin
  V.PID := $0C;
  V.Value := 850;
  V.Unit_ := 'rpm';
  V.Description := 'Engine speed';
  V.Raw := TBytes.Create($0D, $48);
  FBinding.PushValue(V);
  Assert.AreEqual('rpm', FBinding.LastUnit);
  Assert.AreEqual('Engine speed', FBinding.LastDescription);
  Assert.AreEqual(2, Length(FBinding.Raw));
  // A plain value push keeps the unit learnt from the decoder.
  FBinding.PushValue(900);
  Assert.AreEqual('rpm', FBinding.LastUnit);
end;

procedure TOBDChannelBindingTests.NaNValueKeepsPreviousValue;
begin
  FBinding.PushValue(10);
  FBinding.PushValue(NaN);
  Assert.AreEqual(Double(10), FBinding.Value, 1e-9);
end;

procedure TOBDChannelBindingTests.ClearDropsValueAndFiresStateChange;
begin
  FBinding.PushValue(1);
  FBinding.Clear;
  Assert.IsFalse(FBinding.HasValue);
  Assert.IsTrue(IsNan(FBinding.Value));
  Assert.AreEqual(1, FStateEvents);
end;

procedure TOBDChannelBindingTests.GoesStaleAfterTimeout;
begin
  FBinding.StaleAfterMs := 20;
  FBinding.PushValue(5);
  Sleep(80);
  Assert.IsTrue(FBinding.IsStale);
  Assert.IsTrue(FBinding.AgeMs >= 20);
end;

procedure TOBDChannelBindingTests.ZeroStaleAfterNeverStale;
begin
  FBinding.StaleAfterMs := 0;
  FBinding.PushValue(5);
  Sleep(30);
  Assert.IsFalse(FBinding.IsStale);
end;

procedure TOBDChannelBindingTests.AssignCopiesPIDAndStaleAfter;
var
  Other: TOBDChannelBinding;
begin
  FBinding.PID := $05;
  FBinding.StaleAfterMs := 1500;
  Other := TOBDChannelBinding.Create(FOwner);
  try
    Other.Assign(FBinding);
    Assert.AreEqual(Byte($05), Other.PID);
    Assert.AreEqual(Cardinal(1500), Other.StaleAfterMs);
  finally
    Other.Free;
  end;
end;

{ TOBDGaugeAlertsTests ------------------------------------------------------- }

procedure TOBDGaugeAlertsTests.NoKindsIsAlwaysNormal;
var
  A: TOBDGaugeAlerts;
begin
  A := TOBDGaugeAlerts.Create;
  try
    A.HighAlarm := 10;
    Assert.AreEqual(Ord(alvNormal), Ord(A.Level(1000)));
  finally
    A.Free;
  end;
end;

procedure TOBDGaugeAlertsTests.HighWarningAndAlarm;
var
  A: TOBDGaugeAlerts;
begin
  A := TOBDGaugeAlerts.Create;
  try
    A.SetHigh(100, 110);
    Assert.AreEqual(Ord(alvNormal), Ord(A.Level(90)));
    Assert.AreEqual(Ord(alvWarning), Ord(A.Level(100)));
    Assert.AreEqual(Ord(alvWarning), Ord(A.Level(105)));
    Assert.AreEqual(Ord(alvAlarm), Ord(A.Level(110)));
  finally
    A.Free;
  end;
end;

procedure TOBDGaugeAlertsTests.LowWarningAndAlarm;
var
  A: TOBDGaugeAlerts;
begin
  A := TOBDGaugeAlerts.Create;
  try
    A.SetLow(12.0, 11.5);
    Assert.AreEqual(Ord(alvNormal), Ord(A.Level(12.6)));
    Assert.AreEqual(Ord(alvWarning), Ord(A.Level(11.8)));
    Assert.AreEqual(Ord(alvAlarm), Ord(A.Level(11.2)));
  finally
    A.Free;
  end;
end;

procedure TOBDGaugeAlertsTests.NaNIsNormal;
var
  A: TOBDGaugeAlerts;
begin
  A := TOBDGaugeAlerts.Create;
  try
    A.SetHigh(1, 2);
    Assert.AreEqual(Ord(alvNormal), Ord(A.Level(NaN)));
  finally
    A.Free;
  end;
end;

procedure TOBDGaugeAlertsTests.ChangeEventFires;
var
  A: TOBDGaugeAlerts;
  Counter: TChangeCounter;
begin
  Counter := TChangeCounter.Create;
  A := TOBDGaugeAlerts.Create;
  try
    A.OnChange := Counter.Handle;
    A.SetHigh(1, 2);
    A.HighAlarm := 3;
    Assert.AreEqual(2, Counter.Count);
  finally
    A.Free;
    Counter.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDUnitConversionTests);
  TDUnitX.RegisterTestFixture(TOBDChannelBindingTests);
  TDUnitX.RegisterTestFixture(TOBDGaugeAlertsTests);

end.
