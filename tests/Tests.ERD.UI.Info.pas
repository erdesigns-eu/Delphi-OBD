//------------------------------------------------------------------------------
//  Tests.ERD.UI.Info
//
//  Non-visual contract for the A2.7 info / vehicle visuals.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Info;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF}, {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  Vcl.Graphics,
  DUnitX.TestFramework,
  ERD.Adapter,
  ERD.Service.VINInspector,
  ERD.Service.VINDecoder.Types,
  ERD.Service.DTCs,
  ERD.UI.Info;

type
  TTestCallback1 = reference to procedure(Sender: TObject; ATrip: Char);
  TTestCallback2 = reference to procedure(Sender: TObject; ATrip: Char);

  [TestFixture]
  TVINCardTests = class
  public
    [Test] procedure DefaultsEmptyInfo;
    [Test] procedure SetInfoRoundTrip;
    [Test] procedure RefreshWithoutSourceIsNoOp;
    [Test] procedure SourceBindAndFreeNotificationCleans;
  end;

  [TestFixture]
  TAdapterPanelTests = class
  public
    [Test] procedure DefaultsNoAdapter;
    [Test] procedure DTCCountRoundTrip;
    [Test] procedure ProtocolNameRoundTrip;
    [Test] procedure FreeNotificationClearsAdapter;
  end;

  [TestFixture]
  TOdometerTests = class
  strict private
    FTestCallback1: TTestCallback1;
    FTestCallback2: TTestCallback2;
    procedure HandleTestEvent1(Sender: TObject; ATrip: Char);
    procedure HandleTestEvent2(Sender: TObject; ATrip: Char);

  public
    [Test] procedure DefaultsZero;
    [Test] procedure TotalClampsNegative;
    [Test] procedure ResetTripAFiresEventAndZeroes;
    [Test] procedure ResetTripBFiresEvent;
    [Test] procedure DecimalsRoundTrip;
  end;

  [TestFixture]
  TClockTests = class
  public
    [Test] procedure DefaultsAnalog24hSeconds;
    [Test] procedure StyleRoundTrip;
    [Test] procedure HourFormatRoundTrip;
    [Test] procedure ShowSecondsToggle;
  end;

implementation

{ TVINCardTests ----------------------------------------------------------- }

procedure TVINCardTests.DefaultsEmptyInfo;
var C: TOBDVINCard;
begin
  C := TOBDVINCard.Create(nil);
  try
    Assert.IsFalse(C.Info.Valid);
    Assert.AreEqual('', C.Info.VIN);
  finally C.Free; end;
end;

procedure TVINCardTests.SetInfoRoundTrip;
var
  C:    TOBDVINCard;
  Info: TOBDVINInfo;
begin
  C := TOBDVINCard.Create(nil);
  try
    Info := Default(TOBDVINInfo);
    Info.VIN := 'TEST123';
    Info.Valid := True;
    Info.ModelYear := 2024;
    C.SetInfo(Info);
    Assert.AreEqual('TEST123', C.Info.VIN);
    Assert.IsTrue(C.Info.Valid);
    Assert.AreEqual<Word>(2024, C.Info.ModelYear);
  finally C.Free; end;
end;

procedure TVINCardTests.RefreshWithoutSourceIsNoOp;
var C: TOBDVINCard;
begin
  C := TOBDVINCard.Create(nil);
  try
    C.Refresh;
    Assert.IsFalse(C.Info.Valid);
  finally C.Free; end;
end;

procedure TVINCardTests.SourceBindAndFreeNotificationCleans;
var
  C: TOBDVINCard;
  I: TOBDVINInspector;
begin
  I := TOBDVINInspector.Create(nil);
  C := TOBDVINCard.Create(nil);
  try
    C.Source := I;
    Assert.IsNotNull(C.Source);
    I.Free; I := nil;
    Assert.IsNull(C.Source);
  finally
    C.Free;
    if I <> nil then I.Free;
  end;
end;

{ TAdapterPanelTests ------------------------------------------------------ }

procedure TAdapterPanelTests.DefaultsNoAdapter;
var P: TOBDAdapterPanel;
begin
  P := TOBDAdapterPanel.Create(nil);
  try
    Assert.IsNull(P.Adapter);
    Assert.AreEqual(0, P.DTCCount);
    Assert.AreEqual('', P.ProtocolName);
  finally P.Free; end;
end;

procedure TAdapterPanelTests.DTCCountRoundTrip;
var P: TOBDAdapterPanel;
begin
  P := TOBDAdapterPanel.Create(nil);
  try
    P.DTCCount := 7;
    Assert.AreEqual(7, P.DTCCount);
  finally P.Free; end;
end;

procedure TAdapterPanelTests.ProtocolNameRoundTrip;
var P: TOBDAdapterPanel;
begin
  P := TOBDAdapterPanel.Create(nil);
  try
    P.ProtocolName := 'ISO 15765-4 CAN 11/500';
    Assert.AreEqual('ISO 15765-4 CAN 11/500', P.ProtocolName);
  finally P.Free; end;
end;

procedure TAdapterPanelTests.FreeNotificationClearsAdapter;
var
  P: TOBDAdapterPanel;
  A: TOBDAdapter;
begin
  A := TOBDAdapter.Create(nil);
  P := TOBDAdapterPanel.Create(nil);
  try
    P.Adapter := A;
    Assert.IsNotNull(P.Adapter);
    A.Free; A := nil;
    Assert.IsNull(P.Adapter);
  finally
    P.Free;
    if A <> nil then A.Free;
  end;
end;

{ TOdometerTests ---------------------------------------------------------- }

procedure TOdometerTests.DefaultsZero;
var O: TOBDOdometer;
begin
  O := TOBDOdometer.Create(nil);
  try
    Assert.AreEqual(0.0, O.Total, 0.001);
    Assert.AreEqual(0.0, O.TripA, 0.001);
    Assert.AreEqual(0.0, O.TripB, 0.001);
    Assert.AreEqual('km', O.&Unit);
    Assert.IsTrue(O.ShowReset);
  finally O.Free; end;
end;

procedure TOdometerTests.TotalClampsNegative;
var O: TOBDOdometer;
begin
  O := TOBDOdometer.Create(nil);
  try
    O.Total := -1000;
    Assert.AreEqual(0.0, O.Total, 0.001);
  finally O.Free; end;
end;

procedure TOdometerTests.ResetTripAFiresEventAndZeroes;
var
  O: TOBDOdometer;
  Got: Char;
begin
  Got := #0;
  O := TOBDOdometer.Create(nil);
  try
    O.TripA := 123.4;
    FTestCallback1 :=
      procedure(Sender: TObject; ATrip: Char)
      begin Got := ATrip; end;
    O.OnTripReset := HandleTestEvent1;
    O.ResetTripA;
    Assert.AreEqual<Char>('A', Got);
    Assert.AreEqual(0.0, O.TripA, 0.001);
  finally O.Free; end;
end;

procedure TOdometerTests.ResetTripBFiresEvent;
var
  O: TOBDOdometer;
  Got: Char;
begin
  Got := #0;
  O := TOBDOdometer.Create(nil);
  try
    FTestCallback2 :=
      procedure(Sender: TObject; ATrip: Char)
      begin Got := ATrip; end;
    O.OnTripReset := HandleTestEvent2;
    O.ResetTripB;
    Assert.AreEqual<Char>('B', Got);
  finally O.Free; end;
end;

procedure TOdometerTests.DecimalsRoundTrip;
var O: TOBDOdometer;
begin
  O := TOBDOdometer.Create(nil);
  try
    O.Decimals := 3;
    Assert.AreEqual<Byte>(3, O.Decimals);
  finally O.Free; end;
end;

{ TClockTests ------------------------------------------------------------- }

procedure TClockTests.DefaultsAnalog24hSeconds;
var C: TOBDClock;
begin
  C := TOBDClock.Create(nil);
  try
    Assert.IsTrue(C.Style = csAnalog);
    Assert.IsTrue(C.HourFormat = h24);
    Assert.IsTrue(C.ShowSeconds);
  finally C.Free; end;
end;

procedure TClockTests.StyleRoundTrip;
var C: TOBDClock;
begin
  C := TOBDClock.Create(nil);
  try
    C.Style := csDigital;
    Assert.IsTrue(C.Style = csDigital);
  finally C.Free; end;
end;

procedure TClockTests.HourFormatRoundTrip;
var C: TOBDClock;
begin
  C := TOBDClock.Create(nil);
  try
    C.HourFormat := h12;
    Assert.IsTrue(C.HourFormat = h12);
  finally C.Free; end;
end;

procedure TClockTests.ShowSecondsToggle;
var C: TOBDClock;
begin
  C := TOBDClock.Create(nil);
  try
    C.ShowSeconds := False;
    Assert.IsFalse(C.ShowSeconds);
  finally C.Free; end;
end;

// Object-method adapters keep published events compatible with the IDE.
procedure TOdometerTests.HandleTestEvent1(Sender: TObject; ATrip: Char);
begin
  FTestCallback1(Sender, ATrip);
end;

procedure TOdometerTests.HandleTestEvent2(Sender: TObject; ATrip: Char);
begin
  FTestCallback2(Sender, ATrip);
end;

initialization
  TDUnitX.RegisterTestFixture(TVINCardTests);
  TDUnitX.RegisterTestFixture(TAdapterPanelTests);
  TDUnitX.RegisterTestFixture(TOdometerTests);
  TDUnitX.RegisterTestFixture(TClockTests);

end.
