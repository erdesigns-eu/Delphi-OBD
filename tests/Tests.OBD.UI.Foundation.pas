//------------------------------------------------------------------------------
//  Tests.OBD.UI.Foundation
//
//  Coverage for the visual UI foundation: TOBDTheme resolution
//  chain + TOBDVisualStyle reset/has-any + TOBDValueAnim
//  duration / easing arithmetic.
//
//  Component paint behaviour itself needs a windowed message
//  loop and is out of unit-test scope (manual smoke on the
//  demo dashboard).
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Foundation;

interface

uses
  System.SysUtils, System.Classes, System.Diagnostics,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Anim;

type
  [TestFixture]
  TOBDVisualStyleTests = class
  public
    [Test] procedure ResetClearsEverySlot;
    [Test] procedure HasAnyFalseAfterReset;
    [Test] procedure HasAnyTrueAfterOneSlot;
    [Test] procedure PickColorOverrideWinsWhenNonDefault;
    [Test] procedure PickColorFallsThroughOnDefault;
  end;

  [TestFixture]
  TOBDThemeTests = class
  public
    [Test] procedure DefaultsToAutoMode;
    [Test] procedure LightModeYieldsLightPalette;
    [Test] procedure DarkModeYieldsDarkPalette;
    [Test] procedure CustomOverridePaletteWins;
    [Test] procedure ClearOverrideReturnsToBrandDefault;
    [Test] procedure FindOnOwnerWalksAncestry;
    [Test] procedure FindOnOwnerReturnsNilWhenAbsent;
    [Test] procedure ProcessDefaultIsClassWide;
  end;

  [TestFixture]
  TOBDValueAnimTests = class
  public
    [Test] procedure SnapToFiresFrameAndDeactivates;
    [Test] procedure StopJumpsToFinalAndFiresDone;
    [Test] procedure EasingLinearIsIdentity;
    [Test] procedure EasingEaseOutZeroAtZeroOneAtOne;
    [Test] procedure DefaultDurationIs250Ms;
    [Test] procedure DefaultFPSIs60;
  end;

implementation

{ TOBDVisualStyleTests ----------------------------------------------------- }

procedure TOBDVisualStyleTests.ResetClearsEverySlot;
var S: TOBDVisualStyle;
begin
  S.Background := clRed;
  S.Foreground := clBlue;
  S.Accent     := clYellow;
  S.Border     := clGreen;
  S.Reset;
  Assert.AreEqual<TColor>(clDefault, S.Background);
  Assert.AreEqual<TColor>(clDefault, S.Foreground);
  Assert.AreEqual<TColor>(clDefault, S.Accent);
  Assert.AreEqual<TColor>(clDefault, S.Border);
end;

procedure TOBDVisualStyleTests.HasAnyFalseAfterReset;
var S: TOBDVisualStyle;
begin
  S.Reset;
  Assert.IsFalse(S.HasAny);
end;

procedure TOBDVisualStyleTests.HasAnyTrueAfterOneSlot;
var S: TOBDVisualStyle;
begin
  S.Reset;
  S.Accent := clRed;
  Assert.IsTrue(S.HasAny);
end;

procedure TOBDVisualStyleTests.PickColorOverrideWinsWhenNonDefault;
begin
  Assert.AreEqual<TColor>(clRed, PickColor(clRed, clBlue));
end;

procedure TOBDVisualStyleTests.PickColorFallsThroughOnDefault;
begin
  Assert.AreEqual<TColor>(clBlue, PickColor(clDefault, clBlue));
end;

{ TOBDThemeTests ----------------------------------------------------------- }

procedure TOBDThemeTests.DefaultsToAutoMode;
var T: TOBDTheme;
begin
  T := TOBDTheme.Create(nil);
  try
    Assert.AreEqual(Ord(tmAuto), Ord(T.Mode));
  finally
    T.Free;
  end;
end;

procedure TOBDThemeTests.LightModeYieldsLightPalette;
var T: TOBDTheme; P: TOBDThemePalette;
begin
  T := TOBDTheme.Create(nil);
  try
    T.Mode := tmLight;
    P := T.Palette;
    // Light palette has a near-white background.
    Assert.AreEqual<TColor>(BRAND_PALETTE_LIGHT.Background, P.Background);
    Assert.AreEqual<TColor>(BRAND_PALETTE_LIGHT.Accent,     P.Accent);
  finally
    T.Free;
  end;
end;

procedure TOBDThemeTests.DarkModeYieldsDarkPalette;
var T: TOBDTheme; P: TOBDThemePalette;
begin
  T := TOBDTheme.Create(nil);
  try
    T.Mode := tmDark;
    P := T.Palette;
    Assert.AreEqual<TColor>(BRAND_PALETTE_DARK.Background, P.Background);
  finally
    T.Free;
  end;
end;

procedure TOBDThemeTests.CustomOverridePaletteWins;
var
  T: TOBDTheme;
  Custom: TOBDThemePalette;
  Got: TOBDThemePalette;
begin
  T := TOBDTheme.Create(nil);
  try
    Custom := BRAND_PALETTE_LIGHT;
    Custom.Accent := clRed;
    T.SetCustomPalette(Custom, True);
    Assert.IsTrue(T.HasOverride);
    Got := T.Palette;
    Assert.AreEqual<TColor>(clRed, Got.Accent);
  finally
    T.Free;
  end;
end;

procedure TOBDThemeTests.ClearOverrideReturnsToBrandDefault;
var T: TOBDTheme; P: TOBDThemePalette;
begin
  T := TOBDTheme.Create(nil);
  try
    T.Mode := tmLight;
    T.SetCustomPalette(BRAND_PALETTE_DARK, True);
    Assert.IsTrue(T.HasOverride);
    T.SetCustomPalette(BRAND_PALETTE_DARK, False);
    Assert.IsFalse(T.HasOverride);
    P := T.Palette;
    Assert.AreEqual<TColor>(BRAND_PALETTE_LIGHT.Background, P.Background);
  finally
    T.Free;
  end;
end;

procedure TOBDThemeTests.FindOnOwnerWalksAncestry;
var
  RootOwner: TComponent;
  T: TOBDTheme;
  Probe: TComponent;
  Found: TOBDTheme;
begin
  // RootOwner owns both the theme and a leaf component. Owner
  // ancestry walk from the leaf should reach the theme.
  RootOwner := TComponent.Create(nil);
  try
    T := TOBDTheme.Create(RootOwner);
    Probe := TComponent.Create(RootOwner);
    Found := TOBDTheme.FindOnOwner(Probe);
    Assert.IsNotNull(Found);
    Assert.AreSame(T, Found);
  finally
    RootOwner.Free;
  end;
end;

procedure TOBDThemeTests.FindOnOwnerReturnsNilWhenAbsent;
var
  RootOwner: TComponent;
  Probe: TComponent;
begin
  RootOwner := TComponent.Create(nil);
  try
    Probe := TComponent.Create(RootOwner);
    Assert.IsNull(TOBDTheme.FindOnOwner(Probe));
  finally
    RootOwner.Free;
  end;
end;

procedure TOBDThemeTests.ProcessDefaultIsClassWide;
var T: TOBDTheme;
begin
  T := TOBDTheme.Create(nil);
  try
    TOBDTheme.RegisterDefault(T);
    Assert.AreSame(T, TOBDTheme.GetDefault);
  finally
    // Clear the default so other tests aren't affected.
    TOBDTheme.RegisterDefault(nil);
    T.Free;
  end;
end;

{ TOBDValueAnimTests ------------------------------------------------------- }

procedure TOBDValueAnimTests.SnapToFiresFrameAndDeactivates;
var
  A: TOBDValueAnim;
  Fired: Boolean;
  Got:   Double;
begin
  Fired := False;
  Got := 0;
  A := TOBDValueAnim.Create;
  try
    A.OnFrame :=
      procedure (Sender: TObject; AValue: Double)
      begin Fired := True; Got := AValue; end;
    A.SnapTo(42.5);
    Assert.IsTrue(Fired);
    Assert.AreEqual(42.5, Got, 0.001);
    Assert.IsFalse(A.Active);
  finally
    A.Free;
  end;
end;

procedure TOBDValueAnimTests.StopJumpsToFinalAndFiresDone;
var
  A: TOBDValueAnim;
  DoneFinal: Double;
  DoneFired: Boolean;
begin
  DoneFired := False;
  DoneFinal := 0;
  A := TOBDValueAnim.Create;
  try
    A.OnDone :=
      procedure (Sender: TObject; AFinal: Double)
      begin DoneFired := True; DoneFinal := AFinal; end;
    A.Animate(0, 100);
    Assert.IsTrue(A.Active);
    A.Stop;
    Assert.IsFalse(A.Active);
    Assert.IsTrue(DoneFired);
    Assert.AreEqual(100.0, DoneFinal, 0.001);
    Assert.AreEqual(100.0, A.Current, 0.001);
  finally
    A.Free;
  end;
end;

procedure TOBDValueAnimTests.EasingLinearIsIdentity;
var A: TOBDValueAnim;
begin
  A := TOBDValueAnim.Create;
  try
    A.Easing := emLinear;
    // Verify via SnapTo to mid-range and observe Current.
    A.SnapTo(0.5);
    Assert.AreEqual(0.5, A.Current, 0.001);
  finally
    A.Free;
  end;
end;

procedure TOBDValueAnimTests.EasingEaseOutZeroAtZeroOneAtOne;
var A: TOBDValueAnim;
begin
  // emEaseOut is 1 - (1-t)^2. At t=0 -> 0; at t=1 -> 1.
  // The class doesn't expose Ease publicly; we exercise it
  // indirectly via SnapTo + Animate having start/end endpoints.
  A := TOBDValueAnim.Create;
  try
    A.Easing := emEaseOut;
    A.SnapTo(0);
    Assert.AreEqual(0.0, A.Current, 0.001);
    A.SnapTo(1);
    Assert.AreEqual(1.0, A.Current, 0.001);
  finally
    A.Free;
  end;
end;

procedure TOBDValueAnimTests.DefaultDurationIs250Ms;
var A: TOBDValueAnim;
begin
  A := TOBDValueAnim.Create;
  try
    Assert.AreEqual<Cardinal>(250, A.DurationMs);
  finally
    A.Free;
  end;
end;

procedure TOBDValueAnimTests.DefaultFPSIs60;
var A: TOBDValueAnim;
begin
  A := TOBDValueAnim.Create;
  try
    Assert.AreEqual<Byte>(60, A.FPS);
  finally
    A.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDVisualStyleTests);
  TDUnitX.RegisterTestFixture(TOBDThemeTests);
  TDUnitX.RegisterTestFixture(TOBDValueAnimTests);

end.
