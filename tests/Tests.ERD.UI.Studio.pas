//------------------------------------------------------------------------------
//  Tests.ERD.UI.Studio
//
//  Rendering and behaviour tests for the OBD Studio controls: the building
//  blocks (card, buttons, check boxes, radio buttons, segmented strip,
//  inspector, sidebar), the range profiles and the panels composed of them.
//  Each control is drawn off-screen, and the pixels are checked, so a
//  control that paints nothing fails.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Studio;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.StdCtrls,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Card,
  ERD.UI.Buttons,
  ERD.UI.Segmented,
  ERD.UI.Inspector,
  ERD.UI.Sidebar,
  ERD.UI.RangeProfiles,
  ERD.UI.VehicleCard,
  ERD.UI.DtcPanel,
  ERD.UI.ReadinessPanel,
  ERD.UI.FreezeFrameView,
  ERD.UI.RangeEditor,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDStudioBlockTests = class
  strict private
    FCard: TOBDCard;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure CardDrawsTitleAndBorder;
    [Test] procedure CardHeaderAndFooterFollowFlags;
    [Test] procedure TabletDensityIsTaller;
    [Test] procedure ButtonDraws;
    [Test] procedure CheckBoxStates;
    [Test] procedure SwitchDrawsDifferentlyWhenOn;
    [Test] procedure RadioButtonsShareAGroup;
    [Test] procedure SegmentedClampsItemIndex;
  end;

  [TestFixture]
  TOBDInspectorTests = class
  strict private
    FInspector: TOBDInspector;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure CategoriesAndProperties;
    [Test] procedure ModifiedFollowsDefaultValue;
    [Test] procedure CheckValuesNormalise;
    [Test] procedure CollapsedCategoryStillDraws;
  end;

  [TestFixture]
  TOBDSidebarTests = class
  strict private
    FSidebar: TOBDSidebar;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure ItemsAndSelection;
    [Test] procedure CollapsedDrawsDifferently;
  end;

  [TestFixture]
  TOBDRangeProfileTests = class
  strict private
    FProfile: TOBDRangeProfile;
    FNotified: Integer;
    procedure ProfileChanged(Sender: TObject);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure DefaultsHaveRanges;
    [Test] procedure GarageEditIsModifiedAndResets;
    [Test] procedure LevelFollowsBand;
    [Test] procedure JSONRoundTrip;
    [Test] procedure ListenersAreNotified;
  end;

  [TestFixture]
  TOBDDtcPanelTests = class
  strict private
    FPanel: TOBDDtcPanel;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStateDraws;
    [Test] procedure AddAndClearCodes;
    [Test] procedure CodesChangeTheDrawing;
  end;

  [TestFixture]
  TOBDReadinessPanelTests = class
  strict private
    FPanel: TOBDReadinessPanel;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure NoDataIsNotReady;
    [Test] procedure DecodesDieselPID01;
    [Test] procedure AllowedIncompleteMakesReady;
    [Test] procedure CompactLayoutDraws;
  end;

  [TestFixture]
  TOBDVehicleInfoCardTests = class
  strict private
    FCard: TOBDVehicleInfoCard;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure CheckDigitVerdict;
    [Test] procedure CompactLayoutDraws;
  end;

  [TestFixture]
  TOBDFreezeFrameViewTests = class
  strict private
    FView: TOBDFreezeFrameView;
    FProfile: TOBDRangeProfile;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure ValuesAndLive;
    [Test] procedure InspectorLayoutDrawsDifferently;
    [Test] procedure FreedProfileIsReleased;
  end;

  [TestFixture]
  TOBDRangeEditorTests = class
  strict private
    FEditor: TOBDRangeEditor;
    FProfile: TOBDRangeProfile;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure ProfileDraws;
    [Test] procedure GarageEditChangesTheDrawing;
  end;

implementation

{ TOBDStudioBlockTests -------------------------------------------------------- }

procedure TOBDStudioBlockTests.Setup;
begin
  FCard := TOBDCard.Create(nil);
  FCard.SetBounds(0, 0, 360, 200);
end;

procedure TOBDStudioBlockTests.TearDown;
begin
  FreeAndNil(FCard);
end;

procedure TOBDStudioBlockTests.CardDrawsTitleAndBorder;
begin
  FCard.Title := 'Trouble codes';
  Assert.IsTrue(InkRatio(FCard) > 0.005, 'card draws nothing');
end;

procedure TOBDStudioBlockTests.CardHeaderAndFooterFollowFlags;
var
  WithFooter, WithoutFooter: TBitmap;
begin
  FCard.Title := 'Freeze frame';
  FCard.FooterText := 'Profile: VAG 1.6 TDI';
  FCard.ShowFooter := True;
  Assert.IsTrue(FCard.HeaderRect.Bottom > FCard.HeaderRect.Top,
    'no header area');
  Assert.IsTrue(FCard.FooterRect.Bottom > FCard.FooterRect.Top,
    'no footer area');
  WithFooter := RenderControl(FCard);
  try
    FCard.ShowFooter := False;
    WithoutFooter := RenderControl(FCard);
    try
      Assert.IsTrue(BitmapsDiffer(WithFooter, WithoutFooter, 20),
        'footer not drawn');
    finally
      WithoutFooter.Free;
    end;
  finally
    WithFooter.Free;
  end;
end;

procedure TOBDStudioBlockTests.TabletDensityIsTaller;
var
  Button: TOBDButton;
  Desktop: Integer;
begin
  Button := TOBDButton.Create(nil);
  try
    Button.Caption := 'Read codes';
    Button.Density := dnDesktop;
    Desktop := Button.Height;
    Button.Density := dnTablet;
    Assert.IsTrue(Button.Height > Desktop, 'tablet button is not taller');
  finally
    Button.Free;
  end;
end;

procedure TOBDStudioBlockTests.ButtonDraws;
var
  Button: TOBDButton;
begin
  Button := TOBDButton.Create(nil);
  try
    Button.Caption := 'Read codes';
    Button.Kind := bkPrimary;
    Assert.IsTrue(Button.Width > 0);
    Assert.IsTrue(InkRatio(Button) > 0.05, 'button draws nothing');
  finally
    Button.Free;
  end;
end;

procedure TOBDStudioBlockTests.CheckBoxStates;
var
  Check: TOBDCheckBox;
begin
  Check := TOBDCheckBox.Create(nil);
  try
    Check.Caption := 'Engine off';
    Assert.IsFalse(Check.Checked);
    Check.Checked := True;
    Assert.IsTrue(Check.State = cbChecked);
    Check.State := cbUnchecked;
    Assert.IsFalse(Check.Checked);
    Assert.IsTrue(InkRatio(Check) > 0.01, 'check box draws nothing');
  finally
    Check.Free;
  end;
end;

procedure TOBDStudioBlockTests.SwitchDrawsDifferentlyWhenOn;
var
  Check: TOBDCheckBox;
  Off, OnBmp: TBitmap;
begin
  Check := TOBDCheckBox.Create(nil);
  try
    Check.Style := csSwitch;
    Check.Caption := 'Compare with live';
    Off := RenderControl(Check);
    try
      Check.Checked := True;
      OnBmp := RenderControl(Check);
      try
        Assert.IsTrue(BitmapsDiffer(Off, OnBmp, 20), 'switch looks the same');
      finally
        OnBmp.Free;
      end;
    finally
      Off.Free;
    end;
  finally
    Check.Free;
  end;
end;

procedure TOBDStudioBlockTests.RadioButtonsShareAGroup;
var
  A, B, C: TOBDRadioButton;
begin
  A := TOBDRadioButton.Create(FCard);
  A.Parent := FCard;
  B := TOBDRadioButton.Create(FCard);
  B.Parent := FCard;
  C := TOBDRadioButton.Create(FCard);
  C.Parent := FCard;
  C.GroupIndex := 1;
  A.Checked := True;
  C.Checked := True;
  B.Checked := True;
  Assert.IsFalse(A.Checked, 'group sibling stays checked');
  Assert.IsTrue(B.Checked);
  Assert.IsTrue(C.Checked, 'other group was cleared');
end;

procedure TOBDStudioBlockTests.SegmentedClampsItemIndex;
var
  Seg: TOBDSegmented;
begin
  Seg := TOBDSegmented.Create(nil);
  try
    Seg.Items.CommaText := 'All,Stored,Pending,Permanent';
    Seg.ItemIndex := 2;
    Assert.AreEqual(2, Seg.ItemIndex);
    Seg.Items.CommaText := 'All,Stored';
    Assert.IsTrue(Seg.ItemIndex < 2, 'index past the last item');
    Assert.IsTrue(InkRatio(Seg) > 0.02, 'segmented strip draws nothing');
  finally
    Seg.Free;
  end;
end;

{ TOBDInspectorTests ---------------------------------------------------------- }

procedure TOBDInspectorTests.Setup;
begin
  FInspector := TOBDInspector.Create(nil);
  FInspector.SetBounds(0, 0, 360, 320);
end;

procedure TOBDInspectorTests.TearDown;
begin
  FreeAndNil(FInspector);
end;

procedure TOBDInspectorTests.CategoriesAndProperties;
var
  Cat: TOBDInspectorCategory;
begin
  Cat := FInspector.AddCategory('Engine');
  Cat.AddProperty('Coolant', '92 °C', ivReadOnly);
  Cat.AddProperty('RPM', '2140', ivReadOnly);
  Assert.AreEqual(1, FInspector.Categories.Count);
  Assert.IsNotNull(FInspector.FindProperty('RPM'));
  Assert.IsNull(FInspector.FindProperty('Boost'));
  Assert.IsTrue(InkRatio(FInspector) > 0.01, 'inspector rows not drawn');
  FInspector.Clear;
  Assert.AreEqual(0, FInspector.Categories.Count);
end;

procedure TOBDInspectorTests.ModifiedFollowsDefaultValue;
var
  Prop: TOBDInspectorProperty;
begin
  Prop := FInspector.AddCategory('Settings').AddProperty('Density', 'Desktop',
    ivPickList);
  Prop.DefaultValue := 'Desktop';
  Assert.IsFalse(Prop.Modified);
  Prop.Value := 'Tablet';
  Assert.IsTrue(Prop.Modified);
  Prop.ResetToDefault;
  Assert.AreEqual('Desktop', Prop.Value);
end;

procedure TOBDInspectorTests.CheckValuesNormalise;
var
  Prop: TOBDInspectorProperty;
begin
  Prop := FInspector.AddCategory('Settings').AddProperty('Imperial', 'False',
    ivCheck);
  Prop.Value := '1';
  Assert.AreEqual('True', Prop.Value);
  Prop.Value := 'no';
  Assert.AreEqual('False', Prop.Value);
end;

procedure TOBDInspectorTests.CollapsedCategoryStillDraws;
var
  Cat: TOBDInspectorCategory;
begin
  Cat := FInspector.AddCategory('Engine');
  Cat.AddProperty('Coolant', '92 °C', ivReadOnly);
  Cat.Collapsed := True;
  Assert.IsTrue(InkRatio(FInspector) > 0.002, 'collapsed inspector is empty');
end;

{ TOBDSidebarTests ------------------------------------------------------------ }

procedure TOBDSidebarTests.Setup;
begin
  FSidebar := TOBDSidebar.Create(nil);
  FSidebar.SetBounds(0, 0, 220, 480);
end;

procedure TOBDSidebarTests.TearDown;
begin
  FreeAndNil(FSidebar);
end;

procedure TOBDSidebarTests.PreviewDraws;
begin
  FSidebar.ForcePreview := True;
  Assert.IsTrue(InkRatio(FSidebar) > 0.01, 'sidebar preview is empty');
end;

procedure TOBDSidebarTests.ItemsAndSelection;
var
  Item: TOBDSidebarItem;
begin
  Item := FSidebar.Items.Add;
  Item.Caption := 'Trouble codes';
  Item.Badge := 5;
  FSidebar.Items.Add.Caption := 'Readiness';
  FSidebar.ItemIndex := 1;
  Assert.AreEqual(1, FSidebar.ItemIndex);
  Assert.IsTrue(InkRatio(FSidebar) > 0.01, 'sidebar items not drawn');
end;

procedure TOBDSidebarTests.CollapsedDrawsDifferently;
var
  Expanded, Collapsed: TBitmap;
begin
  FSidebar.AutoWidth := False;
  FSidebar.Items.Add.Caption := 'Trouble codes';
  FSidebar.Items.Add.Caption := 'Readiness';
  Expanded := RenderControl(FSidebar);
  try
    FSidebar.Collapsed := True;
    Collapsed := RenderControl(FSidebar);
    try
      Assert.IsTrue(BitmapsDiffer(Expanded, Collapsed, 50),
        'collapsed sidebar looks the same');
    finally
      Collapsed.Free;
    end;
  finally
    Expanded.Free;
  end;
end;

{ TOBDRangeProfileTests ------------------------------------------------------- }

procedure TOBDRangeProfileTests.Setup;
begin
  FProfile := TOBDRangeProfile.Create(nil);
  FNotified := 0;
end;

procedure TOBDRangeProfileTests.TearDown;
begin
  FreeAndNil(FProfile);
end;

procedure TOBDRangeProfileTests.ProfileChanged(Sender: TObject);
begin
  Inc(FNotified);
end;

procedure TOBDRangeProfileTests.DefaultsHaveRanges;
begin
  FProfile.LoadDefaults;
  Assert.IsTrue(FProfile.Ranges.Count > 0, 'no default ranges');
  Assert.IsNotNull(FProfile.Ranges.FindPID($05), 'no coolant range');
  Assert.AreEqual(0, FProfile.ModifiedCount);
end;

procedure TOBDRangeProfileTests.GarageEditIsModifiedAndResets;
var
  Range: TOBDValueRange;
begin
  FProfile.LoadDefaults;
  Range := FProfile.Ranges.FindPID($05);
  Range.High := Range.DefaultHigh + 5;
  Assert.IsTrue(Range.IsModified);
  Assert.AreEqual(1, FProfile.ModifiedCount);
  FProfile.ResetAll;
  Assert.AreEqual(0, FProfile.ModifiedCount);
end;

procedure TOBDRangeProfileTests.LevelFollowsBand;
var
  Range: TOBDValueRange;
begin
  FProfile.LoadDefaults;
  Range := FProfile.Ranges.FindPID($05);
  Assert.IsTrue(Range.Level((Range.Low + Range.High) / 2) = alvNormal);
  Assert.IsTrue(Range.Level(Range.High + (Range.High - Range.Low)) = alvAlarm);
end;

procedure TOBDRangeProfileTests.JSONRoundTrip;
var
  Loaded: TOBDRangeProfile;
  Range: TOBDValueRange;
begin
  FProfile.LoadDefaults;
  Range := FProfile.Ranges.FindPID($05);
  Range.High := Range.DefaultHigh + 3;
  Loaded := TOBDRangeProfile.Create(nil);
  try
    Loaded.LoadFromJSON(FProfile.ToJSON);
    Assert.AreEqual(FProfile.Ranges.Count, Loaded.Ranges.Count);
    Assert.AreEqual(Range.High, Loaded.Ranges.FindPID($05).High, 1e-9);
    Assert.AreEqual(1, Loaded.ModifiedCount);
  finally
    Loaded.Free;
  end;
end;

procedure TOBDRangeProfileTests.ListenersAreNotified;
begin
  FProfile.LoadDefaults;
  FProfile.AddChangeListener(ProfileChanged);
  FProfile.AddChangeListener(ProfileChanged);
  FProfile.ProfileName := 'Workshop';
  Assert.AreEqual(1, FNotified, 'listener not called exactly once');
  FProfile.RemoveChangeListener(ProfileChanged);
  FProfile.ProfileName := 'Other';
  Assert.AreEqual(1, FNotified, 'removed listener still called');
end;

{ TOBDDtcPanelTests ----------------------------------------------------------- }

procedure TOBDDtcPanelTests.Setup;
begin
  FPanel := TOBDDtcPanel.Create(nil);
  FPanel.SetBounds(0, 0, 720, 420);
end;

procedure TOBDDtcPanelTests.TearDown;
begin
  FreeAndNil(FPanel);
end;

procedure TOBDDtcPanelTests.PreviewDraws;
begin
  FPanel.ForcePreview := True;
  Assert.AreEqual(0, FPanel.Count, 'preview rows leaked into the data');
  Assert.IsTrue(InkRatio(FPanel) > 0.03, 'DTC panel preview is empty');
end;

procedure TOBDDtcPanelTests.EmptyStateDraws;
begin
  Assert.IsTrue(InkRatio(FPanel) > 0.005, 'empty DTC panel draws nothing');
end;

procedure TOBDDtcPanelTests.AddAndClearCodes;
begin
  FPanel.AddCode('P0401', 'EGR flow insufficient', 'Emissions', 'Engine',
    dsStored);
  FPanel.AddCode('P2463', 'DPF soot accumulation', 'Emissions', 'Engine',
    dsPending);
  Assert.AreEqual(2, FPanel.Count);
  FPanel.Clear;
  Assert.AreEqual(0, FPanel.Count);
end;

procedure TOBDDtcPanelTests.CodesChangeTheDrawing;
var
  Empty, Filled: TBitmap;
begin
  Empty := RenderControl(FPanel);
  try
    FPanel.AddCode('P0401', 'EGR flow insufficient', 'Emissions', 'Engine',
      dsStored);
    FPanel.AddCode('P20EE', 'SCR NOx catalyst efficiency below threshold',
      'Emissions', 'Engine', dsPermanent);
    Filled := RenderControl(FPanel);
    try
      Assert.IsTrue(BitmapsDiffer(Empty, Filled, 200), 'codes not drawn');
    finally
      Filled.Free;
    end;
  finally
    Empty.Free;
  end;
end;

{ TOBDReadinessPanelTests ----------------------------------------------------- }

procedure TOBDReadinessPanelTests.Setup;
begin
  FPanel := TOBDReadinessPanel.Create(nil);
  FPanel.SetBounds(0, 0, 640, 460);
end;

procedure TOBDReadinessPanelTests.TearDown;
begin
  FreeAndNil(FPanel);
end;

procedure TOBDReadinessPanelTests.PreviewDraws;
begin
  FPanel.ForcePreview := True;
  Assert.IsTrue(InkRatio(FPanel) > 0.03, 'readiness preview is empty');
end;

procedure TOBDReadinessPanelTests.NoDataIsNotReady;
begin
  Assert.AreEqual(0, FPanel.SupportedCount);
  Assert.IsFalse(FPanel.Ready, 'no supported monitors reads as ready');
end;

procedure TOBDReadinessPanelTests.DecodesDieselPID01;
begin
  // B: misfire, fuel system and components supported and complete,
  // compression ignition. C: NMHC, NOx/SCR, boost, exhaust gas sensor,
  // PM filter and EGR supported. D: NOx/SCR and PM filter incomplete.
  FPanel.LoadPID01($81, $0F, $EB, $42);
  Assert.IsTrue(FPanel.MilOn);
  Assert.AreEqual(1, FPanel.DtcCount);
  Assert.IsTrue(FPanel.CompressionIgnition);
  Assert.AreEqual(9, FPanel.SupportedCount);
  Assert.AreEqual(2, FPanel.IncompleteCount);
  Assert.IsTrue(FPanel.MonitorState[rmPMFilter] = msIncomplete);
  Assert.IsTrue(FPanel.MonitorState[rmBoostPressure] = msComplete);
  Assert.IsFalse(FPanel.Ready);
end;

procedure TOBDReadinessPanelTests.AllowedIncompleteMakesReady;
begin
  FPanel.LoadPID01($00, $0F, $EB, $40);
  Assert.IsFalse(FPanel.Ready);
  FPanel.AllowedIncomplete := 1;
  Assert.IsTrue(FPanel.Ready);
end;

procedure TOBDReadinessPanelTests.CompactLayoutDraws;
var
  Full, Compact: TBitmap;
begin
  FPanel.LoadPID01($00, $0F, $EB, $42);
  Full := RenderControl(FPanel);
  try
    FPanel.Layout := rlCompact;
    Compact := RenderControl(FPanel);
    try
      Assert.IsTrue(InkPixels(Compact) > 500, 'compact layout is empty');
      Assert.IsTrue(BitmapsDiffer(Full, Compact, 200),
        'compact layout looks like the full one');
    finally
      Compact.Free;
    end;
  finally
    Full.Free;
  end;
end;

{ TOBDVehicleInfoCardTests ---------------------------------------------------- }

procedure TOBDVehicleInfoCardTests.Setup;
begin
  FCard := TOBDVehicleInfoCard.Create(nil);
  FCard.SetBounds(0, 0, 640, 240);
end;

procedure TOBDVehicleInfoCardTests.TearDown;
begin
  FreeAndNil(FCard);
end;

procedure TOBDVehicleInfoCardTests.PreviewDraws;
begin
  FCard.ForcePreview := True;
  Assert.IsTrue(InkRatio(FCard) > 0.03, 'vehicle card preview is empty');
end;

procedure TOBDVehicleInfoCardTests.CheckDigitVerdict;
begin
  FCard.VIN := '1HGCM82633A004352';
  Assert.IsTrue(FCard.VINCheckDigitValid, 'valid VIN rejected');
  FCard.VIN := '1HGCM82643A004352';
  Assert.IsFalse(FCard.VINCheckDigitValid, 'wrong check digit accepted');
end;

procedure TOBDVehicleInfoCardTests.CompactLayoutDraws;
begin
  FCard.VIN := '1HGCM82633A004352';
  FCard.Make := 'Honda';
  FCard.Model := 'Accord';
  FCard.Layout := vlCompact;
  FCard.SetBounds(0, 0, 900, 64);
  Assert.IsTrue(InkRatio(FCard) > 0.02, 'compact vehicle strip is empty');
end;

{ TOBDFreezeFrameViewTests ---------------------------------------------------- }

procedure TOBDFreezeFrameViewTests.Setup;
begin
  FView := TOBDFreezeFrameView.Create(nil);
  FView.SetBounds(0, 0, 720, 420);
  FProfile := TOBDRangeProfile.Create(nil);
  FProfile.LoadDefaults;
end;

procedure TOBDFreezeFrameViewTests.TearDown;
begin
  FreeAndNil(FView);
  FreeAndNil(FProfile);
end;

procedure TOBDFreezeFrameViewTests.PreviewDraws;
begin
  FView.ForcePreview := True;
  Assert.AreEqual(0, FView.ValueCount, 'preview values leaked into the data');
  Assert.IsTrue(InkRatio(FView) > 0.03, 'freeze-frame preview is empty');
end;

procedure TOBDFreezeFrameViewTests.ValuesAndLive;
begin
  FView.RangeProfile := FProfile;
  FView.DtcCode := 'P0401';
  FView.AddValue($05, 'coolant_temp', 'Coolant temperature', 88, '°C', 0);
  FView.AddText($03, 'Fuel system status', 'Closed loop');
  FView.SetLive($05, 91);
  Assert.AreEqual(2, FView.ValueCount);
  Assert.IsTrue(FView.Values(0).HasLive, 'live value not stored');
  Assert.AreEqual(Double(91), FView.Values(0).Live, 1e-9);
  Assert.IsFalse(FView.Values(1).IsNumeric);
  Assert.IsTrue(InkRatio(FView) > 0.02, 'freeze-frame rows not drawn');
  FView.Clear;
  Assert.AreEqual(0, FView.ValueCount);
end;

procedure TOBDFreezeFrameViewTests.InspectorLayoutDrawsDifferently;
var
  Table, Inspector: TBitmap;
begin
  FView.RangeProfile := FProfile;
  FView.AddValue($05, 'coolant_temp', 'Coolant temperature', 88, '°C', 0);
  FView.AddValue($0C, 'engine_speed', 'Engine speed', 2140, 'rpm', 0);
  Table := RenderControl(FView);
  try
    FView.Layout := flInspector;
    Inspector := RenderControl(FView);
    try
      Assert.IsTrue(BitmapsDiffer(Table, Inspector, 200),
        'inspector layout looks like the table');
    finally
      Inspector.Free;
    end;
  finally
    Table.Free;
  end;
end;

procedure TOBDFreezeFrameViewTests.FreedProfileIsReleased;
begin
  FView.RangeProfile := FProfile;
  FreeAndNil(FProfile);
  Assert.IsNull(FView.RangeProfile, 'freed profile still referenced');
end;

{ TOBDRangeEditorTests -------------------------------------------------------- }

procedure TOBDRangeEditorTests.Setup;
begin
  FEditor := TOBDRangeEditor.Create(nil);
  FEditor.SetBounds(0, 0, 720, 420);
  FProfile := TOBDRangeProfile.Create(nil);
  FProfile.LoadDefaults;
end;

procedure TOBDRangeEditorTests.TearDown;
begin
  FreeAndNil(FEditor);
  FreeAndNil(FProfile);
end;

procedure TOBDRangeEditorTests.PreviewDraws;
begin
  FEditor.ForcePreview := True;
  Assert.IsTrue(InkRatio(FEditor) > 0.03, 'range editor preview is empty');
end;

procedure TOBDRangeEditorTests.ProfileDraws;
begin
  FEditor.RangeProfile := FProfile;
  Assert.IsTrue(InkRatio(FEditor) > 0.03, 'range editor rows not drawn');
end;

procedure TOBDRangeEditorTests.GarageEditChangesTheDrawing;
var
  Defaults, Edited: TBitmap;
  Range: TOBDValueRange;
begin
  FEditor.RangeProfile := FProfile;
  Defaults := RenderControl(FEditor);
  try
    Range := FProfile.Ranges.FindPID($05);
    Range.High := Range.DefaultHigh + 5;
    Edited := RenderControl(FEditor);
    try
      Assert.IsTrue(BitmapsDiffer(Defaults, Edited, 20),
        'garage value not shown');
    finally
      Edited.Free;
    end;
  finally
    Defaults.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDStudioBlockTests);
  TDUnitX.RegisterTestFixture(TOBDInspectorTests);
  TDUnitX.RegisterTestFixture(TOBDSidebarTests);
  TDUnitX.RegisterTestFixture(TOBDRangeProfileTests);
  TDUnitX.RegisterTestFixture(TOBDDtcPanelTests);
  TDUnitX.RegisterTestFixture(TOBDReadinessPanelTests);
  TDUnitX.RegisterTestFixture(TOBDVehicleInfoCardTests);
  TDUnitX.RegisterTestFixture(TOBDFreezeFrameViewTests);
  TDUnitX.RegisterTestFixture(TOBDRangeEditorTests);
end.
