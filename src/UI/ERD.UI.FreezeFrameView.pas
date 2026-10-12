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
  ERD.UI.ScrollBar,
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
    FColumnWidths: array[0..2] of Integer;
    FResizeColumn: Integer;
    FResizeStartX: Integer;
    FResizeStartWidth: Integer;
    FTopRow: Integer;
    FBarHot: Boolean;
    FBarDragging: Boolean;
    FBarDragOffset: Integer;
    function RowHeight: Integer;
    function RowsTop: Integer;
    function RowsBottom: Integer;
    function VisibleRows: Integer;
    function MaxTopRow: Integer;
    function NeedsScrollBar: Boolean;
    function ScrollBarRect: TRect;
    function ScrollThumb: TRect;
    function RowRight: Integer;
    procedure ScrollTo(ATop: Integer);
    procedure SetBarHot(AValue: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure SetColumnWidth(AIndex, AValue: Integer);
    function GetColumnWidth(AIndex: Integer): Integer;
    procedure GetColumns(out AParam, AFault, ALive, ARange, ARight: Integer);
    function ColumnHeaderTop: Integer;
    function ColumnDividerAt(X, Y: Integer): Integer;
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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DblClick; override;
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
    /// <summary>Parameter column width at 96 DPI; 0 = fill the free space.
    /// Drag the header divider to resize, double-click it to reset.</summary>
    property ParameterColumnWidth: Integer index 0 read GetColumnWidth
      write SetColumnWidth default 0;
    /// <summary>At-fault column width at 96 DPI; 0 = automatic.</summary>
    property FaultColumnWidth: Integer index 1 read GetColumnWidth
      write SetColumnWidth default 0;
    /// <summary>Live column width at 96 DPI; 0 = automatic.</summary>
    property LiveColumnWidth: Integer index 2 read GetColumnWidth
      write SetColumnWidth default 0;
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
  FResizeColumn := -1;
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
  FTopRow := 0;
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
  FTopRow := EnsureRange(FTopRow, 0, MaxTopRow);
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
    APainter.Text(X, ScaleValue(50), S, OBD_TEXT_SMALL, Palette.GaugeLabel,
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

function TOBDFreezeFrameView.GetColumnWidth(AIndex: Integer): Integer;
begin
  Result := FColumnWidths[AIndex];
end;

procedure TOBDFreezeFrameView.SetColumnWidth(AIndex, AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FColumnWidths[AIndex] = AValue then
    Exit;
  FColumnWidths[AIndex] := AValue;
  Invalidate;
end;

function TOBDFreezeFrameView.ColumnHeaderTop: Integer;
begin
  Result := ScaleValue(68);
end;

function TOBDFreezeFrameView.RowHeight: Integer;
begin
  Result := Max(1, ScaleValue(Metrics.CompactRow));
end;

function TOBDFreezeFrameView.RowsTop: Integer;
begin
  Result := ColumnHeaderTop + ScaleValue(Metrics.ColHead);
end;

function TOBDFreezeFrameView.RowsBottom: Integer;
begin
  Result := Max(RowsTop, Height - ScaleValue(Metrics.Foot));
end;

function TOBDFreezeFrameView.VisibleRows: Integer;
begin
  Result := Max(1, (RowsBottom - RowsTop) div RowHeight);
end;

function TOBDFreezeFrameView.MaxTopRow: Integer;
begin
  Result := Max(0, DisplayValues.Count - VisibleRows);
end;

function TOBDFreezeFrameView.NeedsScrollBar: Boolean;
begin
  Result := (FLayout = flTable) and (DisplayValues.Count > VisibleRows);
end;

function TOBDFreezeFrameView.ScrollBarRect: TRect;
begin
  if not NeedsScrollBar then
    Exit(Rect(0, 0, 0, 0));
  Result := Rect(Width - ScaleValue(1) - ScaleValue(10), RowsTop,
    Width - ScaleValue(1), RowsBottom);
end;

function TOBDFreezeFrameView.ScrollThumb: TRect;
begin
  Result := OBDScrollThumbRect(ScrollBarRect, True, 0,
    DisplayValues.Count - 1, VisibleRows, FTopRow, FBarHot, FBarDragging);
end;

function TOBDFreezeFrameView.RowRight: Integer;
begin
  // Row tints and grid lines stop at the scroll bar.
  if NeedsScrollBar then
    Result := ScrollBarRect.Left
  else
    Result := Width - ScaleValue(1);
end;

procedure TOBDFreezeFrameView.ScrollTo(ATop: Integer);
begin
  ATop := EnsureRange(ATop, 0, MaxTopRow);
  if FTopRow = ATop then
    Exit;
  FTopRow := ATop;
  Invalidate;
end;

procedure TOBDFreezeFrameView.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

procedure TOBDFreezeFrameView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not FBarDragging then
    SetBarHot(False);
end;

procedure TOBDFreezeFrameView.CMMouseWheel(var Message: TCMMouseWheel);
begin
  if not NeedsScrollBar then
  begin
    inherited;
    Exit;
  end;
  ScrollTo(FTopRow - Sign(Message.WheelDelta) * 3);
  Message.Result := 1;
end;

procedure TOBDFreezeFrameView.GetColumns(out AParam, AFault, ALive, ARange,
  ARight: Integer);
var
  WParam, WFault, WLive, WRangeAuto: Integer;
begin
  // Column left edges, listview style: Parameter | At fault | Live | Range.
  // Range is the last column and takes whatever space is left.
  AParam := ScaleValue(OBD_PAD);
  ARight := Width - ScaleValue(OBD_PAD);
  if NeedsScrollBar then
    ARight := Min(ARight, ScrollBarRect.Left - ScaleValue(8));
  if FColumnWidths[1] > 0 then
    WFault := ScaleValue(FColumnWidths[1])
  else
    WFault := ScaleValue(96);
  WLive := 0;
  if FCompareLive then
  begin
    if FColumnWidths[2] > 0 then
      WLive := ScaleValue(FColumnWidths[2])
    else
      WLive := ScaleValue(78);
  end;
  if Width >= ScaleValue(420) then
    WRangeAuto := ScaleValue(64 + 16)
  else
    WRangeAuto := ScaleValue(48 + 16);
  if FColumnWidths[0] > 0 then
    WParam := ScaleValue(FColumnWidths[0])
  else
    WParam := Max(ScaleValue(60), ARight - AParam - WFault - WLive - WRangeAuto);
  AFault := AParam + WParam;
  ALive := AFault + WFault;
  ARange := ALive + WLive;
end;

function TOBDFreezeFrameView.ColumnDividerAt(X, Y: Integer): Integer;
var
  XParam, XFault, XLive, XRange, XRight, HY, Slop: Integer;
begin
  Result := -1;
  if FLayout <> flTable then
    Exit;
  HY := ColumnHeaderTop;
  if (Y < HY) or (Y >= HY + ScaleValue(Metrics.ColHead)) then
    Exit;
  GetColumns(XParam, XFault, XLive, XRange, XRight);
  Slop := ScaleValue(4);
  // Check right-most first so a collapsed column can still be widened.
  if FCompareLive and (Abs(X - XRange) <= Slop) then
    Exit(2);
  if Abs(X - XLive) <= Slop then
    Exit(1);
  if Abs(X - XFault) <= Slop then
    Exit(0);
end;

procedure TOBDFreezeFrameView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  XParam, XFault, XLive, XRange, XRight: Integer;
  Thumb: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or (ssDouble in Shift) then
    Exit;
  if NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)) then
  begin
    Thumb := ScrollThumb;
    if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
    begin
      FBarDragging := True;
      FBarDragOffset := Y - Thumb.Top;
      MouseCapture := True;
      Invalidate;
    end
    else if Y < Thumb.Top then
      ScrollTo(FTopRow - VisibleRows)
    else
      ScrollTo(FTopRow + VisibleRows);
    Exit;
  end;
  FResizeColumn := ColumnDividerAt(X, Y);
  if FResizeColumn < 0 then
    Exit;
  // Freeze the automatic widths so only the dragged column changes.
  GetColumns(XParam, XFault, XLive, XRange, XRight);
  FColumnWidths[0] := MulDiv(XFault - XParam, 96, ScaleValue(96));
  FColumnWidths[1] := MulDiv(XLive - XFault, 96, ScaleValue(96));
  if FCompareLive then
    FColumnWidths[2] := MulDiv(XRange - XLive, 96, ScaleValue(96));
  FResizeStartX := X;
  FResizeStartWidth := FColumnWidths[FResizeColumn];
end;

procedure TOBDFreezeFrameView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewWidth, Travel: Integer;
  Bar, Thumb: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  if FBarDragging then
  begin
    Bar := ScrollBarRect;
    Thumb := ScrollThumb;
    Travel := Bar.Height - Thumb.Height;
    if Travel > 0 then
      ScrollTo(Round((Y - FBarDragOffset - Bar.Top) / Travel * MaxTopRow));
    Exit;
  end;
  SetBarHot(NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)));
  if FResizeColumn >= 0 then
  begin
    NewWidth := Max(40, FResizeStartWidth +
      MulDiv(X - FResizeStartX, 96, ScaleValue(96)));
    SetColumnWidth(FResizeColumn, NewWidth);
    Exit;
  end;
  if ColumnDividerAt(X, Y) >= 0 then
    Cursor := crHSplit
  else
    Cursor := crDefault;
end;

procedure TOBDFreezeFrameView.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FResizeColumn := -1;
  if FBarDragging then
  begin
    FBarDragging := False;
    MouseCapture := False;
    SetBarHot(NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)));
    Invalidate;
  end;
end;

procedure TOBDFreezeFrameView.DblClick;
var
  P: TPoint;
  Col: Integer;
begin
  inherited DblClick;
  P := ScreenToClient(Mouse.CursorPos);
  Col := ColumnDividerAt(P.X, P.Y);
  if Col < 0 then
    Exit;
  // Double-clicking a divider returns the columns to automatic sizing.
  FColumnWidths[0] := 0;
  FColumnWidths[1] := 0;
  FColumnWidths[2] := 0;
  Invalidate;
end;

procedure TOBDFreezeFrameView.DrawTable(APainter: TOBDPainter);
var
  M: TOBDDensityMetrics;
  HeaderH, ColHeadH, FootH, RowH, HY, RY, BarW: Integer;
  ColFault, ColLive, ColBar, MaxNameW: Integer;
  XParam, XFault, XLive, XRange, XRight, CellPad: Integer;
  FaultW, LiveW: Integer;
  V: TOBDFreezeFrameValue;
  R: TOBDValueRange;
  Level: TOBDAlertLevel;
  C: TColor;
  Weight: TOBDTextWeight;
  LiveS: string;
  Strength: Single;
  Values: TObjectList<TOBDFreezeFrameValue>;
  I, RightX: Integer;
begin
  M := Metrics;
  HeaderH := ScaleValue(68);
  ColHeadH := ScaleValue(M.ColHead);
  FootH := ScaleValue(M.Foot);
  RowH := ScaleValue(M.CompactRow);
  HY := HeaderH;
  GetColumns(XParam, XFault, XLive, XRange, XRight);
  CellPad := ScaleValue(8);
  // Right-aligned value columns end CellPad before the next divider; the
  // range bar fills the last column.
  ColFault := XLive - CellPad;
  ColLive := XRange - CellPad;
  ColBar := XRange + ScaleValue(16);
  BarW := XRight - ColBar;
  // Every cell is limited to its own column (and to the card), so text
  // ellipses instead of running into the next column after a resize.
  MaxNameW := Max(1, Min(XFault, XRight) - XParam - CellPad);
  FaultW := Max(1, Min(XLive, XRight) - XFault - 2 * CellPad);
  LiveW := Max(1, Min(XRange, XRight) - XLive - 2 * CellPad);
  ColFault := Min(ColFault, XRight);
  ColLive := Min(ColLive, XRight);

  APainter.FillRect(Rect(ScaleValue(1), HY, Width - ScaleValue(1), HY + ColHeadH),
    APainter.HeaderFill);
  APainter.HLine(ScaleValue(1), HY + ColHeadH - ScaleValue(1),
    Width - ScaleValue(2), Palette.NeutralLight);
  APainter.VLine(XFault, HY + ScaleValue(6), ColHeadH - ScaleValue(12),
    Palette.NeutralLight);
  APainter.VLine(XLive, HY + ScaleValue(6), ColHeadH - ScaleValue(12),
    Palette.NeutralLight);
  if FCompareLive then
    APainter.VLine(XRange, HY + ScaleValue(6), ColHeadH - ScaleValue(12),
      Palette.NeutralLight);
  APainter.Caps(XParam, HY + ColHeadH div 2, 'Parameter', taLeftJustify,
    clDefault, MaxNameW);
  if XFault + CellPad < XRight then
    APainter.Caps(ColFault, HY + ColHeadH div 2, 'At fault', taRightJustify,
      clDefault, FaultW);
  if FCompareLive and (XLive + CellPad < XRight) then
    APainter.Caps(ColLive, HY + ColHeadH div 2, 'Live', taRightJustify,
      clDefault, LiveW);
  if BarW > 0 then
    APainter.Caps(ColBar, HY + ColHeadH div 2, 'Range', taLeftJustify,
      clDefault, BarW);

  RY := HY + ColHeadH;
  Values := DisplayValues;
  RightX := RowRight;
  for I := EnsureRange(FTopRow, 0, MaxTopRow) to Values.Count - 1 do
  begin
    V := Values[I];
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
      APainter.FillRect(Rect(ScaleValue(1), RY, RightX, RY + RowH),
        APainter.Tint(C, Strength));
      APainter.FillRect(Rect(ScaleValue(1), RY + ScaleValue(5), ScaleValue(4),
        RY + RowH - ScaleValue(5)), C);
    end;
    APainter.Text(XParam, RY + RowH div 2, V.Caption, 12.5,
      Palette.ForegroundText, twRegular, taLeftJustify, MaxNameW);
    if Level = alvNormal then
      Weight := twSemibold
    else
      Weight := twBold;
    if XFault + CellPad < XRight then
      APainter.Text(ColFault, RY + RowH div 2, ValueText(V, False), 12.5,
        APainter.LevelColor(Level), Weight, taRightJustify, FaultW);
    if FCompareLive then
    begin
      if V.HasLive then
        LiveS := ValueText(V, True)
      else
        LiveS := '--';
      if XLive + CellPad < XRight then
        APainter.Text(ColLive, RY + RowH div 2, LiveS, 12.5,
          Palette.GaugeLabel, twRegular, taRightJustify, LiveW);
    end;
    if (R <> nil) and V.IsNumeric and not IsNan(V.AtFault) and
      (BarW >= ScaleValue(16)) then
      APainter.RangeBar(ColBar, RY + RowH div 2, BarW, R.Min, R.Max, R.Low,
        R.High, V.AtFault, Level);
    APainter.HLine(ScaleValue(1), RY + RowH - ScaleValue(1),
      RightX - ScaleValue(1), Palette.NeutralLight);
    Inc(RY, RowH);
  end;

  if NeedsScrollBar then
    OBDPaintScrollBar(APainter, ScrollBarRect, True, 0, Values.Count - 1,
      VisibleRows, FTopRow, FBarHot, FBarDragging);
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
  FTopRow := EnsureRange(FTopRow, 0, MaxTopRow);
  UpdateChildLayout;
end;

end.
