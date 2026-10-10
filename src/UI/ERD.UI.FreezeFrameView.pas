//------------------------------------------------------------------------------
//  ERD.UI.FreezeFrameView
//
//  TOBDFreezeFrameView - a card that shows the ECU freeze-frame snapshot for
//  a DTC as either the compact comparison table from the OBD Studio mockups or
//  a read-only TOBDInspector grouped by system. It compares at-fault values
//  with optional live values and colours rows from a TOBDRangeProfile using
//  profile change listeners, leaving the profile's OnChange available to the
//  host application.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.FreezeFrameView;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Buttons,
  ERD.UI.Inspector,
  ERD.UI.RangeProfiles,
  ERD.Service.LiveData;

type
  /// <summary>Freeze-frame presentation.</summary>
  TOBDFreezeFrameLayout = (
    /// <summary>Painted compact table, matching the freeze-frame mockup.</summary>
    flTable,
    /// <summary>TOBDInspector child, matching the inspector mockup.</summary>
    flInspector);

  /// <summary>One freeze-frame parameter row.</summary>
  TOBDFreezeFrameValue = class
  public
    /// <summary>Mode 01 PID number, or 0 for key-only values.</summary>
    PID: Integer;
    /// <summary>Stable range-profile lookup key.</summary>
    Key: string;
    /// <summary>Human-readable parameter caption.</summary>
    Caption: string;
    /// <summary>Numeric value captured when the DTC was stored.</summary>
    AtFault: Double;
    /// <summary>Latest numeric live value for the same parameter.</summary>
    Live: Double;
    /// <summary>Display text for the captured value.</summary>
    AtFaultText: string;
    /// <summary>Display text for the live value.</summary>
    LiveText: string;
    /// <summary>Engineering unit shown beside numeric values.</summary>
    UnitText: string;
    /// <summary>Number of decimals used for numeric display.</summary>
    Decimals: Integer;
    /// <summary>True when AtFault and Live contain numeric values.</summary>
    IsNumeric: Boolean;
    /// <summary>True after a live value was supplied.</summary>
    HasLive: Boolean;
  end;

  /// <summary>Painted freeze-frame table with optional inspector layout.</summary>
  TOBDFreezeFrameView = class(TOBDCustomControl, IOBDSurface)
  strict private
    FValues: TObjectList<TOBDFreezeFrameValue>;
    FPreviewValues: TObjectList<TOBDFreezeFrameValue>;
    FPreviewProfile: TOBDRangeProfile;
    FRangeProfile: TOBDRangeProfile;
    FDtcCode: string;
    FTitle: string;
    FSubtitle: string;
    FHeaderInfo: string;
    FLayout: TOBDFreezeFrameLayout;
    FCompareLive: Boolean;
    FUpdateCount: Integer;
    FInspectorDirty: Boolean;
    FUpdatingChildren: Boolean;
    FInspector: TOBDInspector;
    FCompareSwitch: TOBDCheckBox;
    FEditButton: TOBDButton;
    FOnEditRanges: TNotifyEvent;
    procedure SetDtcCode(const AValue: string);
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetHeaderInfo(const AValue: string);
    procedure SetLayout(AValue: TOBDFreezeFrameLayout);
    procedure SetCompareLive(AValue: Boolean);
    procedure SetShowLive(AValue: Boolean);
    function GetShowLive: Boolean;
    procedure SetRangeProfile(AValue: TOBDRangeProfile);
    function ActiveProfile: TOBDRangeProfile;
    procedure CompareSwitchChanged(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ProfileChanged(Sender: TObject);
    procedure DataChanged;
    procedure EnsurePreviewProfile;
    procedure EnsurePreviewData;
    function DisplayValues: TObjectList<TOBDFreezeFrameValue>;
    function DisplayDtcCode: string;
    function DisplayTitle: string;
    function DisplaySubtitle: string;
    function DisplayHeaderInfo: string;
    procedure UpdateChildLayout;
    procedure RebuildInspector;
    function FindRange(AValue: TOBDFreezeFrameValue): TOBDValueRange;
    function RowLevel(AValue: TOBDFreezeFrameValue; ARange: TOBDValueRange): TOBDAlertLevel;
    function ValueText(AValue: TOBDFreezeFrameValue; ALive: Boolean): string;
    function InspectorGroup(AValue: TOBDFreezeFrameValue): string;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawTable(APainter: TOBDPainter);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Resize; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    /// <summary>Creates the table, footer buttons and inspector child.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Unregisters range-profile listeners and releases value lists.</summary>
    destructor Destroy; override;
    /// <summary>Repositions child controls for the effective density.</summary>
    procedure DensityChanged; override;

    /// <summary>Suspends layout/repaint while many values are inserted.</summary>
    procedure BeginUpdate;
    /// <summary>Resumes layout/repaint after BeginUpdate.</summary>
    procedure EndUpdate;
    /// <summary>Removes all values.</summary>
    procedure Clear;
    /// <summary>Adds a numeric freeze-frame value.</summary>
    function AddValue(APID: Integer; const AKey, ACaption: string; AAtFault: Double;
      const AUnitText: string = ''; ADecimals: Integer = 1): Integer; overload;
    /// <summary>Adds a numeric PID-only freeze-frame value.</summary>
    function AddValue(APID: Integer; const ACaption: string; AAtFault: Double;
      const AUnitText: string = ''; ADecimals: Integer = 1): Integer; overload;
    /// <summary>Adds a non-numeric freeze-frame value.</summary>
    function AddText(APID: Integer; const AKey, ACaption, AText: string;
      const AUnitText: string = ''): Integer; overload;
    /// <summary>Adds a non-numeric PID-only freeze-frame value.</summary>
    function AddText(APID: Integer; const ACaption, AText: string): Integer; overload;
    /// <summary>Sets a numeric live value for an existing PID.</summary>
    procedure SetLive(APID: Integer; AValue: Double); overload;
    /// <summary>Sets a text live value for an existing PID.</summary>
    procedure SetLive(APID: Integer; const AText: string); overload;
    /// <summary>Appends a decoded service result as a freeze-frame row.</summary>
    procedure AddFreezeFrameValue(const AValue: TOBDPIDValue; const AKey: string = '');
    /// <summary>Call after the assigned shared RangeProfile changed.</summary>
    procedure RangesChanged;
    /// <summary>Surface colour for child buttons and switches.</summary>
    function SurfaceColor: TColor;
    /// <summary>Number of values.</summary>
    function ValueCount: Integer;
    /// <summary>Read-only row access.</summary>
    function Values(AIndex: Integer): TOBDFreezeFrameValue;
  published
    /// <summary>DTC code shown as the red monospace chip.</summary>
    property DtcCode: string read FDtcCode write SetDtcCode;
    /// <summary>Main header caption.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Header subtitle shown under the title.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Right-aligned header context such as ECU and frame number.</summary>
    property HeaderInfo: string read FHeaderInfo write SetHeaderInfo;
    /// <summary>Table or inspector presentation.</summary>
    property Layout: TOBDFreezeFrameLayout read FLayout write SetLayout default flTable;
    /// <summary>True when the live-value comparison column is visible.</summary>
    property CompareLive: Boolean read FCompareLive write SetCompareLive default True;
    /// <summary>Compatibility alias for CompareLive.</summary>
    property ShowLive: Boolean read GetShowLive write SetShowLive stored False;
    /// <summary>Garage normal-range profile used to draw bars and status colours.</summary>
    property RangeProfile: TOBDRangeProfile read FRangeProfile write SetRangeProfile;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>True when density follows the resolved theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the Edit ranges button is clicked.</summary>
    property OnEditRanges: TNotifyEvent read FOnEditRanges write FOnEditRanges;
  end;

implementation

function FormatOBDNumber(AValue: Double; ADecimals: Integer): string;
var
  Mask: string;
begin
  if IsNan(AValue) then
    Exit('--');
  if ADecimals <= 0 then
    Mask := '0'
  else
    Mask := '0.' + StringOfChar('0', ADecimals);
  Result := FormatFloat(Mask, AValue);
end;

{ TOBDFreezeFrameView -------------------------------------------------------- }

constructor TOBDFreezeFrameView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValues := TObjectList<TOBDFreezeFrameValue>.Create(True);
  FPreviewValues := TObjectList<TOBDFreezeFrameValue>.Create(True);
  FPreviewProfile := TOBDRangeProfile.Create(Self);
  FTitle := 'Freeze frame';
  FSubtitle := 'Snapshot taken by the ECU when the code was stored.';
  FHeaderInfo := '';
  FDtcCode := '';
  FCompareLive := True;
  FLayout := flTable;
  TabStop := False;
  Width := 520;
  Height := 320;

  FCompareSwitch := TOBDCheckBox.Create(Self);
  FCompareSwitch.Parent := Self;
  FCompareSwitch.Style := csSwitch;
  FCompareSwitch.Caption := 'Compare with live';
  FCompareSwitch.Checked := True;
  FCompareSwitch.OnChange := CompareSwitchChanged;
  FCompareSwitch.AutoSize := False;

  FEditButton := TOBDButton.Create(Self);
  FEditButton.Parent := Self;
  FEditButton.Kind := bkGhost;
  FEditButton.Caption := 'Edit ranges…';
  FEditButton.OnClick := EditButtonClick;
  FEditButton.AutoSize := False;

  FInspector := TOBDInspector.Create(Self);
  FInspector.Parent := Self;
  FInspector.ReadOnly := True;
  FInspector.SplitterPosition := 150;
  FInspector.Visible := False;
  FInspectorDirty := True;
  UpdateChildLayout;
end;

destructor TOBDFreezeFrameView.Destroy;
begin
  if FRangeProfile <> nil then
  begin
    FRangeProfile.RemoveChangeListener(ProfileChanged);
    FRangeProfile.RemoveFreeNotification(Self);
  end;
  FValues.Free;
  FPreviewValues.Free;
  inherited Destroy;
end;

procedure TOBDFreezeFrameView.DensityChanged;
begin
  inherited DensityChanged;
  UpdateChildLayout;
  FInspectorDirty := True;
end;

procedure TOBDFreezeFrameView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRangeProfile) then
    FRangeProfile := nil;
end;

procedure TOBDFreezeFrameView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TOBDFreezeFrameView.EndUpdate;
begin
  if FUpdateCount = 0 then
    Exit;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    DataChanged;
end;

procedure TOBDFreezeFrameView.Clear;
begin
  FValues.Clear;
  DataChanged;
end;

function TOBDFreezeFrameView.AddValue(APID: Integer; const AKey, ACaption: string;
  AAtFault: Double; const AUnitText: string; ADecimals: Integer): Integer;
var
  V: TOBDFreezeFrameValue;
begin
  V := TOBDFreezeFrameValue.Create;
  V.PID := APID;
  V.Key := AKey;
  V.Caption := ACaption;
  V.AtFault := AAtFault;
  V.Live := NaN;
  V.UnitText := AUnitText;
  V.Decimals := ADecimals;
  V.IsNumeric := True;
  V.HasLive := False;
  V.AtFaultText := FormatOBDNumber(AAtFault, ADecimals);
  Result := FValues.Add(V);
  DataChanged;
end;

function TOBDFreezeFrameView.AddValue(APID: Integer; const ACaption: string;
  AAtFault: Double; const AUnitText: string; ADecimals: Integer): Integer;
begin
  Result := AddValue(APID, '', ACaption, AAtFault, AUnitText, ADecimals);
end;

function TOBDFreezeFrameView.AddText(APID: Integer; const AKey, ACaption,
  AText: string; const AUnitText: string): Integer;
var
  V: TOBDFreezeFrameValue;
begin
  V := TOBDFreezeFrameValue.Create;
  V.PID := APID;
  V.Key := AKey;
  V.Caption := ACaption;
  V.AtFault := NaN;
  V.Live := NaN;
  V.UnitText := AUnitText;
  V.Decimals := 0;
  V.IsNumeric := False;
  V.HasLive := False;
  V.AtFaultText := AText;
  Result := FValues.Add(V);
  DataChanged;
end;

function TOBDFreezeFrameView.AddText(APID: Integer; const ACaption,
  AText: string): Integer;
begin
  Result := AddText(APID, '', ACaption, AText, '');
end;

procedure TOBDFreezeFrameView.SetLive(APID: Integer; AValue: Double);
var
  V: TOBDFreezeFrameValue;
begin
  for V in FValues do
    if V.PID = APID then
    begin
      V.Live := AValue;
      V.LiveText := FormatOBDNumber(AValue, V.Decimals);
      V.HasLive := True;
      DataChanged;
      Exit;
    end;
end;

procedure TOBDFreezeFrameView.SetLive(APID: Integer; const AText: string);
var
  V: TOBDFreezeFrameValue;
begin
  for V in FValues do
    if V.PID = APID then
    begin
      V.Live := NaN;
      V.LiveText := AText;
      V.HasLive := True;
      DataChanged;
      Exit;
    end;
end;

procedure TOBDFreezeFrameView.AddFreezeFrameValue(const AValue: TOBDPIDValue;
  const AKey: string);
var
  Caption: string;
begin
  Caption := AValue.Description;
  if Caption = '' then
    Caption := 'PID $' + IntToHex(AValue.PID, 2);
  if IsNan(AValue.Value) then
    AddText(AValue.PID, AKey, Caption, '--', AValue.Unit_)
  else
    AddValue(AValue.PID, AKey, Caption, AValue.Value, AValue.Unit_, 1);
end;

procedure TOBDFreezeFrameView.RangesChanged;
begin
  FInspectorDirty := True;
  if FUpdateCount = 0 then
  begin
    if FLayout = flInspector then
      RebuildInspector;
    Invalidate;
  end;
end;

function TOBDFreezeFrameView.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

function TOBDFreezeFrameView.ValueCount: Integer;
begin
  Result := FValues.Count;
end;

function TOBDFreezeFrameView.Values(AIndex: Integer): TOBDFreezeFrameValue;
begin
  Result := FValues[AIndex];
end;

procedure TOBDFreezeFrameView.SetDtcCode(const AValue: string);
begin
  if FDtcCode = AValue then
    Exit;
  FDtcCode := AValue;
  Invalidate;
end;

procedure TOBDFreezeFrameView.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDFreezeFrameView.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TOBDFreezeFrameView.SetHeaderInfo(const AValue: string);
begin
  if FHeaderInfo = AValue then
    Exit;
  FHeaderInfo := AValue;
  Invalidate;
end;

procedure TOBDFreezeFrameView.SetLayout(AValue: TOBDFreezeFrameLayout);
begin
  if FLayout = AValue then
    Exit;
  FLayout := AValue;
  UpdateChildLayout;
  FInspectorDirty := True;
  Invalidate;
end;

procedure TOBDFreezeFrameView.SetCompareLive(AValue: Boolean);
begin
  if FCompareLive = AValue then
    Exit;
  FCompareLive := AValue;
  FUpdatingChildren := True;
  try
    FCompareSwitch.Checked := AValue;
  finally
    FUpdatingChildren := False;
  end;
  DataChanged;
end;

function TOBDFreezeFrameView.GetShowLive: Boolean;
begin
  Result := FCompareLive;
end;

procedure TOBDFreezeFrameView.SetShowLive(AValue: Boolean);
begin
  SetCompareLive(AValue);
end;

procedure TOBDFreezeFrameView.SetRangeProfile(AValue: TOBDRangeProfile);
begin
  if FRangeProfile = AValue then
    Exit;
  if FRangeProfile <> nil then
  begin
    FRangeProfile.RemoveChangeListener(ProfileChanged);
    FRangeProfile.RemoveFreeNotification(Self);
  end;
  FRangeProfile := AValue;
  if FRangeProfile <> nil then
  begin
    FRangeProfile.FreeNotification(Self);
    FRangeProfile.AddChangeListener(ProfileChanged);
  end;
  RangesChanged;
end;

function TOBDFreezeFrameView.ActiveProfile: TOBDRangeProfile;
begin
  Result := FRangeProfile;
  if (Result = nil) and IsPreview then
  begin
    EnsurePreviewProfile;
    Result := FPreviewProfile;
  end;
end;

procedure TOBDFreezeFrameView.CompareSwitchChanged(Sender: TObject);
begin
  if not FUpdatingChildren then
    SetCompareLive(FCompareSwitch.Checked);
end;

procedure TOBDFreezeFrameView.EditButtonClick(Sender: TObject);
begin
  if Assigned(FOnEditRanges) then
    FOnEditRanges(Self);
end;

procedure TOBDFreezeFrameView.ProfileChanged(Sender: TObject);
begin
  RangesChanged;
end;

procedure TOBDFreezeFrameView.DataChanged;
begin
  FInspectorDirty := True;
  if FUpdateCount = 0 then
  begin
    if FLayout = flInspector then
      RebuildInspector;
    Invalidate;
  end;
end;

procedure TOBDFreezeFrameView.EnsurePreviewProfile;
var
  R: TOBDValueRange;
begin
  if FPreviewProfile.Ranges.Count > 0 then
    Exit;
  FPreviewProfile.LoadDefaults;
  FPreviewProfile.ProfileName := 'VAG 1.6 TDI (garage)';
  FPreviewProfile.Vehicle := 'Golf 1.6 TDI CR';
  R := FPreviewProfile.Ranges.FindKey('egr_error');
  if R <> nil then
  begin
    R.Low := -10;
    R.High := 10;
  end;
end;

procedure TOBDFreezeFrameView.EnsurePreviewData;
  procedure AddPreviewValue(APID: Integer; const AKey, ACaption: string;
    AAtFault, ALive: Double; const AUnitText: string; ADecimals: Integer);
  var
    V: TOBDFreezeFrameValue;
  begin
    V := TOBDFreezeFrameValue.Create;
    V.PID := APID;
    V.Key := AKey;
    V.Caption := ACaption;
    V.AtFault := AAtFault;
    V.Live := ALive;
    V.AtFaultText := FormatOBDNumber(AAtFault, ADecimals);
    V.LiveText := FormatOBDNumber(ALive, ADecimals);
    V.UnitText := AUnitText;
    V.Decimals := ADecimals;
    V.IsNumeric := True;
    V.HasLive := True;
    FPreviewValues.Add(V);
  end;

  procedure AddPreviewText(APID: Integer; const AKey, ACaption, AAtFault,
    ALive: string);
  var
    V: TOBDFreezeFrameValue;
  begin
    V := TOBDFreezeFrameValue.Create;
    V.PID := APID;
    V.Key := AKey;
    V.Caption := ACaption;
    V.AtFault := NaN;
    V.Live := NaN;
    V.AtFaultText := AAtFault;
    V.LiveText := ALive;
    V.IsNumeric := False;
    V.HasLive := True;
    FPreviewValues.Add(V);
  end;

begin
  if (FPreviewValues.Count > 0) or (FValues.Count > 0) or not IsPreview then
    Exit;
  AddPreviewText(3, 'fuel_status', 'Fuel system status', 'Closed loop',
    'Closed loop');
  AddPreviewValue($04, 'calculated_load', 'Calculated load', 62.4, 21.6, '%',
    1);
  AddPreviewValue($05, 'coolant_temp', 'Coolant temperature', 84, 88, '°C', 0);
  AddPreviewValue($0C, 'engine_speed', 'Engine speed', 2140, 812, 'rpm', 0);
  AddPreviewValue($0D, 'vehicle_speed', 'Vehicle speed', 78, 0, 'km/h', 0);
  AddPreviewValue($0B, 'intake_map', 'Intake MAP', 142, 101, 'kPa', 0);
  AddPreviewValue(0, 'boost_desired', 'Boost desired', 196, 102, 'kPa', 0);
  AddPreviewValue($2C, 'commanded_egr', 'Commanded EGR', 38.0, 22.4, '%', 1);
  AddPreviewValue($2D, 'egr_error', 'EGR error', -31.5, -2.0, '%', 1);
  AddPreviewValue($0F, 'intake_air_temp', 'Intake air temp', 31, 24, '°C', 0);
  AddPreviewValue(0, 'dpf_dp', 'DPF differential', 18.6, 0.4, 'kPa', 1);
end;

function TOBDFreezeFrameView.DisplayValues: TObjectList<TOBDFreezeFrameValue>;
begin
  EnsurePreviewData;
  if (FValues.Count = 0) and IsPreview then
    Result := FPreviewValues
  else
    Result := FValues;
end;

function TOBDFreezeFrameView.DisplayDtcCode: string;
begin
  Result := FDtcCode;
  if (Result = '') and (FValues.Count = 0) and IsPreview then
    Result := 'P0401';
end;

function TOBDFreezeFrameView.DisplayTitle: string;
begin
  Result := FTitle;
  if (Result = '') and (FValues.Count = 0) and IsPreview then
    Result := 'Freeze frame';
end;

function TOBDFreezeFrameView.DisplaySubtitle: string;
begin
  Result := FSubtitle;
  if (Result = '') and (FValues.Count = 0) and IsPreview then
    Result := 'Snapshot taken by the ECU when the code was stored.';
end;

function TOBDFreezeFrameView.DisplayHeaderInfo: string;
begin
  Result := FHeaderInfo;
  if (Result = '') and (FValues.Count = 0) and IsPreview then
    Result := 'Engine · 7E8 · frame 0';
end;

procedure TOBDFreezeFrameView.UpdateChildLayout;
var
  M: TOBDDensityMetrics;
  HeaderH, FootH, ButtonH, Pad, BtnW, SwitchW, SwitchH, ContentTop, FooterY,
  MidY: Integer;
begin
  M := Metrics;
  Pad := ScaleValue(OBD_PAD);
  HeaderH := ScaleValue(68);
  FootH := ScaleValue(M.Foot);
  ButtonH := ScaleValue(M.Button);
  SwitchH := ScaleValue(M.Switch) + ScaleValue(8);
  SwitchW := ScaleValue(170);
  BtnW := ScaleValue(112);
  FooterY := Max(HeaderH, Height - FootH);
  MidY := FooterY + FootH div 2;

  FCompareSwitch.SetBounds(Pad - ScaleValue(4), MidY - SwitchH div 2,
    SwitchW, SwitchH);
  FEditButton.SetBounds(Max(Pad, Width - Pad - BtnW - ScaleValue(4)),
    FooterY + (FootH - ButtonH) div 2 - ScaleValue(4), BtnW + ScaleValue(8),
    ButtonH + ScaleValue(8));

  ContentTop := HeaderH;
  FInspector.Visible := FLayout = flInspector;
  FInspector.SetBounds(ScaleValue(1), ContentTop, Max(0, Width - ScaleValue(2)),
    Max(0, Height - HeaderH - FootH));
end;

procedure TOBDFreezeFrameView.RebuildInspector;
var
  CatEngine, CatAir, CatAfter, CatOther, Cat: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
  V: TOBDFreezeFrameValue;
  R: TOBDValueRange;
  DisplayValue, GroupName: string;
begin
  if (FUpdateCount > 0) or not FInspectorDirty then
    Exit;
  EnsurePreviewData;
  FInspector.BeginUpdate;
  try
    FInspector.Clear;
    CatEngine := FInspector.AddCategory('Engine');
    CatAir := FInspector.AddCategory('Air / EGR');
    CatAfter := FInspector.AddCategory('Aftertreatment');
    CatAfter.Collapsed := True;
    CatOther := nil;
    for V in DisplayValues do
    begin
      R := FindRange(V);
      GroupName := InspectorGroup(V);
      if GroupName = 'Engine' then
        Cat := CatEngine
      else if GroupName = 'Air / EGR' then
        Cat := CatAir
      else if GroupName = 'Aftertreatment' then
        Cat := CatAfter
      else
      begin
        if CatOther = nil then
          CatOther := FInspector.AddCategory('Other');
        Cat := CatOther;
      end;
      DisplayValue := ValueText(V, False);
      if FCompareLive and V.HasLive then
        DisplayValue := DisplayValue + ' · live ' + ValueText(V, True);
      Prop := Cat.AddProperty(V.Caption, DisplayValue, ivReadOnly);
      Prop.Level := RowLevel(V, R);
    end;
  finally
    FInspector.EndUpdate;
  end;
  FInspectorDirty := False;
end;

function TOBDFreezeFrameView.FindRange(AValue: TOBDFreezeFrameValue): TOBDValueRange;
var
  Profile: TOBDRangeProfile;
begin
  Result := nil;
  Profile := ActiveProfile;
  if Profile = nil then
    Exit;
  if AValue.Key <> '' then
    Result := Profile.Ranges.FindKey(AValue.Key);
  if (Result = nil) and (AValue.PID <> 0) then
    Result := Profile.Ranges.FindPID(AValue.PID);
end;

function TOBDFreezeFrameView.RowLevel(AValue: TOBDFreezeFrameValue;
  ARange: TOBDValueRange): TOBDAlertLevel;
begin
  if (ARange <> nil) and AValue.IsNumeric and not IsNan(AValue.AtFault) then
    Result := ARange.Level(AValue.AtFault)
  else
    Result := alvNormal;
end;

function TOBDFreezeFrameView.ValueText(AValue: TOBDFreezeFrameValue;
  ALive: Boolean): string;
begin
  if ALive then
  begin
    if AValue.HasLive then
      Result := AValue.LiveText
    else
      Result := '--';
  end
  else
    Result := AValue.AtFaultText;
  if (Result <> '--') and (AValue.UnitText <> '') then
    Result := Result + ' ' + AValue.UnitText;
end;

function TOBDFreezeFrameView.InspectorGroup(AValue: TOBDFreezeFrameValue): string;
var
  K, C: string;
begin
  K := LowerCase(AValue.Key);
  C := LowerCase(AValue.Caption);
  if (Pos('egr', K) > 0) or (Pos('egr', C) > 0) or
    (Pos('map', K) > 0) or (Pos('map', C) > 0) or
    (Pos('boost', K) > 0) or (Pos('boost', C) > 0) or
    (Pos('intake', K) > 0) or (Pos('intake', C) > 0) then
    Exit('Air / EGR');
  if (Pos('dpf', K) > 0) or (Pos('dpf', C) > 0) or
    (Pos('scr', K) > 0) or (Pos('scr', C) > 0) or
    (Pos('catalyst', K) > 0) or (Pos('catalyst', C) > 0) then
    Exit('Aftertreatment');
  Result := 'Engine';
end;

procedure TOBDFreezeFrameView.DrawHeader(APainter: TOBDPainter);
var
  X, Y, ChipX: Integer;
  S: string;
begin
  X := ScaleValue(OBD_PAD);
  Y := ScaleValue(26);
  S := DisplayTitle;
  APainter.Text(X, Y, S, 16, Palette.ForegroundText, twBold);
  ChipX := X + APainter.TextWidth(S, 16, twBold) + ScaleValue(12);
  S := DisplayDtcCode;
  if S <> '' then
    APainter.Chip(ChipX, ScaleValue(16), S, Palette.Danger, False, True);
  S := DisplayHeaderInfo;
  if S <> '' then
    APainter.Text(Width - ScaleValue(OBD_PAD), Y, S, 11.5, Palette.GaugeLabel,
      twRegular, taRightJustify);
  S := DisplaySubtitle;
  if S <> '' then
    APainter.Text(X, ScaleValue(50), S, 12, Palette.GaugeLabel,
      twRegular, taLeftJustify, Max(0, Width - X - ScaleValue(220)));
end;

procedure TOBDFreezeFrameView.DrawFooter(APainter: TOBDPainter);
var
  M: TOBDDensityMetrics;
  FootH, FY, MY: Integer;
  Profile: TOBDRangeProfile;
  Text: string;
begin
  M := Metrics;
  FootH := ScaleValue(M.Foot);
  FY := Height - FootH;
  MY := FY + FootH div 2;
  APainter.HLine(ScaleValue(1), FY, Width - ScaleValue(2), Palette.NeutralLight);
  Profile := ActiveProfile;
  if Profile <> nil then
  begin
    Text := Profile.ProfileName;
    if Text = '' then
      Text := Profile.Vehicle;
    if Text <> '' then
      APainter.Text(FEditButton.Left - ScaleValue(4), MY, Text, 11.5,
        Palette.GaugeLabel, twRegular, taRightJustify);
  end;
end;

procedure TOBDFreezeFrameView.DrawTable(APainter: TOBDPainter);
var
  M: TOBDDensityMetrics;
  HeaderH, ColHeadH, FootH, RowH, HY, RY, BarW: Integer;
  ColFault, ColLive, ColBar, MaxNameW: Integer;
  V: TOBDFreezeFrameValue;
  R: TOBDValueRange;
  Level: TOBDAlertLevel;
  C: TColor;
  Weight: TOBDTextWeight;
  LiveS: string;
  Strength: Single;
begin
  M := Metrics;
  HeaderH := ScaleValue(68);
  ColHeadH := ScaleValue(M.ColHead);
  FootH := ScaleValue(M.Foot);
  RowH := ScaleValue(M.CompactRow);
  HY := HeaderH;
  if Width >= ScaleValue(420) then
    BarW := ScaleValue(64)
  else
    BarW := ScaleValue(48);
  ColBar := Width - ScaleValue(OBD_PAD) - BarW;
  ColLive := ColBar - ScaleValue(16);
  if FCompareLive then
    ColFault := ColLive - ScaleValue(78)
  else
    ColFault := ColBar - ScaleValue(16);

  APainter.FillRect(Rect(ScaleValue(1), HY, Width - ScaleValue(1), HY + ColHeadH),
    APainter.HeaderFill);
  APainter.HLine(ScaleValue(1), HY + ColHeadH - ScaleValue(1),
    Width - ScaleValue(2), Palette.NeutralLight);
  APainter.Caps(ScaleValue(OBD_PAD), HY + ColHeadH div 2, 'Parameter');
  APainter.Caps(ColFault, HY + ColHeadH div 2, 'At fault', taRightJustify);
  if FCompareLive then
    APainter.Caps(ColLive, HY + ColHeadH div 2, 'Live', taRightJustify);
  APainter.Caps(ColBar, HY + ColHeadH div 2, 'Range');

  RY := HY + ColHeadH;
  for V in DisplayValues do
  begin
    if RY + RowH > Height - FootH then
      Break;
    R := FindRange(V);
    Level := RowLevel(V, R);
    if Level <> alvNormal then
    begin
      C := APainter.LevelColor(Level);
      if APainter.Dark then
        Strength := 0.12
      else
        Strength := 0.08;
      APainter.FillRect(Rect(ScaleValue(1), RY, Width - ScaleValue(1), RY + RowH),
        APainter.Tint(C, Strength));
      APainter.FillRect(Rect(ScaleValue(1), RY + ScaleValue(5), ScaleValue(4),
        RY + RowH - ScaleValue(5)), C);
    end;
    MaxNameW := Max(0, ColFault - ScaleValue(OBD_PAD) - ScaleValue(84));
    APainter.Text(ScaleValue(OBD_PAD), RY + RowH div 2, V.Caption, 12.5,
      Palette.ForegroundText, twRegular, taLeftJustify, MaxNameW);
    if Level = alvNormal then
      Weight := twSemibold
    else
      Weight := twBold;
    APainter.Text(ColFault, RY + RowH div 2, ValueText(V, False), 12.5,
      APainter.LevelColor(Level), Weight, taRightJustify);
    if FCompareLive then
    begin
      if V.HasLive then
        LiveS := ValueText(V, True)
      else
        LiveS := '--';
      APainter.Text(ColLive, RY + RowH div 2, LiveS, 12.5,
        Palette.GaugeLabel, twRegular, taRightJustify);
    end;
    if (R <> nil) and V.IsNumeric and not IsNan(V.AtFault) then
      APainter.RangeBar(ColBar, RY + RowH div 2, BarW, R.Min, R.Max, R.Low,
        R.High, V.AtFault, Level);
    APainter.HLine(ScaleValue(1), RY + RowH - ScaleValue(1), Width - ScaleValue(2),
      Palette.NeutralLight);
    Inc(RY, RowH);
  end;
end;

procedure TOBDFreezeFrameView.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  EnsurePreviewData;
  if FLayout = flInspector then
    RebuildInspector;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(ClientRect);
    DrawHeader(P);
    if FLayout = flTable then
      DrawTable(P);
    DrawFooter(P);
  finally
    P.Free;
  end;
end;

procedure TOBDFreezeFrameView.Resize;
begin
  inherited Resize;
  UpdateChildLayout;
end;

end.
