//------------------------------------------------------------------------------
//  ERD.UI.ReadinessPanel
//
//  TOBDReadinessPanel - emissions-readiness card for OBD Studio. The control
//  decodes Mode 01 PID 01 monitor support and completion flags, shows the
//  inspection verdict wording and since-clear counters used by common European
//  regimes, and paints the desktop and compact monitor layouts from the
//  approved mockups.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.ReadinessPanel;

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Readiness panel layout.</summary>
  TOBDReadinessLayout = (
    /// <summary>Full card with a verdict banner, monitor tiles and legend.</summary>
    rlFull,
    /// <summary>Compact card with a short banner and small monitor rows.</summary>
    rlCompact);

  /// <summary>Inspection wording used in the readiness verdict.</summary>
  TOBDInspectionRegime = (
    /// <summary>Generic emissions-test wording.</summary>
    irGeneric,
    /// <summary>Dutch APK wording.</summary>
    irAPK,
    /// <summary>Belgian keuring wording.</summary>
    irKeuring,
    /// <summary>French contrôle technique wording.</summary>
    irControleTechnique,
    /// <summary>UK MOT wording.</summary>
    irMOT,
    /// <summary>German HU / AU wording.</summary>
    irHUAU,
    /// <summary>Irish NCT wording.</summary>
    irNCT,
    /// <summary>Custom wording supplied by <see cref="InspectionName"/>.</summary>
    irCustom);

  /// <summary>Monitor-completion state.</summary>
  TOBDMonitorState = (
    /// <summary>The ECU supports the monitor and it has completed.</summary>
    msComplete,
    /// <summary>The ECU supports the monitor and it is not complete.</summary>
    msIncomplete,
    /// <summary>The ECU does not support the monitor.</summary>
    msUnsupported);

  /// <summary>Readiness monitor shown by the panel.</summary>
  TOBDReadinessMonitor = (
    /// <summary>Misfire continuous monitor.</summary>
    rmMisfire,
    /// <summary>Fuel-system continuous monitor.</summary>
    rmFuelSystem,
    /// <summary>Comprehensive-components continuous monitor.</summary>
    rmComponents,
    /// <summary>Spark-ignition catalyst monitor.</summary>
    rmCatalyst,
    /// <summary>Spark-ignition heated-catalyst monitor.</summary>
    rmHeatedCatalyst,
    /// <summary>Spark-ignition evaporative-system monitor.</summary>
    rmEvaporativeSystem,
    /// <summary>Spark-ignition secondary-air-system monitor.</summary>
    rmSecondaryAirSystem,
    /// <summary>Spark-ignition A/C refrigerant monitor.</summary>
    rmACRefrigerant,
    /// <summary>Spark-ignition oxygen-sensor monitor.</summary>
    rmOxygenSensor,
    /// <summary>Spark-ignition oxygen-sensor-heater monitor.</summary>
    rmOxygenSensorHeater,
    /// <summary>Spark-ignition EGR-system monitor.</summary>
    rmEGRSystem,
    /// <summary>Compression-ignition NMHC catalyst monitor.</summary>
    rmNMHCCatalyst,
    /// <summary>Compression-ignition NOx / SCR aftertreatment monitor.</summary>
    rmNOxSCRAftertreatment,
    /// <summary>Compression-ignition boost-pressure monitor.</summary>
    rmBoostPressure,
    /// <summary>Compression-ignition exhaust-gas-sensor monitor.</summary>
    rmExhaustGasSensor,
    /// <summary>Compression-ignition PM-filter monitor.</summary>
    rmPMFilter,
    /// <summary>Compression-ignition EGR / VVT monitor.</summary>
    rmEGRVVTSystem);

  TOBDReadinessPanel = class;

  /// <summary>Editable monitor row in <see cref="TOBDReadinessMonitorCollection"/>.</summary>
  TOBDReadinessMonitorItem = class(TCollectionItem)
  strict private
    FMonitor: TOBDReadinessMonitor;
    FState: TOBDMonitorState;
    FNote: string;
    procedure SetMonitor(AValue: TOBDReadinessMonitor);
    procedure SetState(AValue: TOBDMonitorState);
    procedure SetNote(const AValue: string);
  protected
    /// <summary>Returns the monitor name in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an unsupported monitor item.</summary>
    constructor Create(Collection: TCollection); override;
  published
    /// <summary>Monitor represented by this row.</summary>
    property Monitor: TOBDReadinessMonitor read FMonitor write SetMonitor
      default rmMisfire;
    /// <summary>Completion state for this monitor.</summary>
    property State: TOBDMonitorState read FState write SetState
      default msUnsupported;
    /// <summary>Mechanic-facing note shown on incomplete monitor tiles.</summary>
    property Note: string read FNote write SetNote;
  end;

  /// <summary>Collection of readiness monitors edited by hosts or the IDE.</summary>
  TOBDReadinessMonitorCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDReadinessMonitorItem;
    procedure SetItem(Index: Integer; AValue: TOBDReadinessMonitorItem);
  protected
    /// <summary>Notifies the panel when an item changes.</summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a monitor-item collection.</summary>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a monitor item.</summary>
    function Add: TOBDReadinessMonitorItem;
    /// <summary>Finds the item for a monitor.</summary>
    function Find(AMonitor: TOBDReadinessMonitor): TOBDReadinessMonitorItem;
    /// <summary>Indexed monitor items.</summary>
    property Items[Index: Integer]: TOBDReadinessMonitorItem read GetItem
      write SetItem; default;
    /// <summary>Fires when a monitor item changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Emission-monitor readiness card.</summary>
  TOBDReadinessPanel = class(TOBDCustomControl, IOBDSurface)
  strict private
    FLayout: TOBDReadinessLayout;
    FInspectionRegime: TOBDInspectionRegime;
    FInspectionName: string;
    FAllowedIncomplete: Integer;
    FMilOn: Boolean;
    FDtcCount: Integer;
    FCompressionIgnition: Boolean;
    FDistanceSinceClear: Integer;
    FWarmUpsSinceClear: Integer;
    FSinceClearText: string;
    FUpdatedText: string;
    FActionText: string;
    FMonitors: TOBDReadinessMonitorCollection;
    FHasData: Boolean;
    FUpdateDepth: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLayout(AValue: TOBDReadinessLayout);
    procedure SetInspectionRegime(AValue: TOBDInspectionRegime);
    procedure SetInspectionName(const AValue: string);
    procedure SetAllowedIncomplete(AValue: Integer);
    procedure SetMilOn(AValue: Boolean);
    procedure SetDtcCount(AValue: Integer);
    procedure SetCompressionIgnition(AValue: Boolean);
    procedure SetDistanceSinceClear(AValue: Integer);
    procedure SetWarmUpsSinceClear(AValue: Integer);
    procedure SetSinceClearText(const AValue: string);
    procedure SetUpdatedText(const AValue: string);
    procedure SetActionText(const AValue: string);
    procedure SetMonitors(AValue: TOBDReadinessMonitorCollection);
    procedure MonitorsChanged(Sender: TObject);
    procedure DoChange;
    procedure ResetMonitorDefaults;
    function ItemByMonitor(AMonitor: TOBDReadinessMonitor)
      : TOBDReadinessMonitorItem;
    function UsePreviewData: Boolean;
    function VisualCompressionIgnition: Boolean;
    function VisualMilOn: Boolean;
    function VisualState(AMonitor: TOBDReadinessMonitor): TOBDMonitorState;
    function VisualNote(AMonitor: TOBDReadinessMonitor): string;
    function VisualSupportedCount: Integer;
    function VisualIncompleteCount: Integer;
    function VisualReady: Boolean;
    function VisualSinceClearText: string;
    function VisualUpdatedText: string;
    function InspectionDisplayName: string;
    function GetMonitorState(AMonitor: TOBDReadinessMonitor): TOBDMonitorState;
    function GetReady: Boolean;
    function GetIncompleteCount: Integer;
    function GetSupportedCount: Integer;
    procedure PaintBanner(var P: TOBDPainter; const R: TRect;
      ACompact: Boolean);
    procedure PaintMonitorIcon(var P: TOBDPainter; AState: TOBDMonitorState;
      CX, CY: Integer; AScale: Single);
    procedure PaintMonitorTile(var P: TOBDPainter; X, Y, W, H: Integer;
      AMonitor: TOBDReadinessMonitor);
    procedure PaintMonitorCompact(var P: TOBDPainter; X, Y, W, H: Integer;
      AMonitor: TOBDReadinessMonitor);
    procedure PaintGroup(var P: TOBDPainter; const ATitle: string;
      const AItems: array of TOBDReadinessMonitor; var Y: Integer);
    procedure PaintFull(var P: TOBDPainter);
    procedure PaintCompact(var P: TOBDPainter);
  protected
    /// <summary>Paints the readiness card.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates the panel with every monitor unsupported.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the monitor collection.</summary>
    destructor Destroy; override;
    /// <summary>Relays density changes to the paint buffer.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card face used by child controls.</summary>
    function SurfaceColor: TColor;
    /// <summary>Suspends change notifications while monitor data is updated.</summary>
    procedure BeginUpdate;
    /// <summary>Resumes change notifications and repaints the panel.</summary>
    procedure EndUpdate;
    /// <summary>Clears live data and returns all monitors to unsupported.</summary>
    procedure Clear;
    /// <summary>Decodes OBD-II Mode 01 PID 01 support and incomplete bits.</summary>
    procedure LoadPID01(A, B, C, D: Byte);
    /// <summary>Sets one monitor state and optionally its note.</summary>
    procedure SetMonitor(AMonitor: TOBDReadinessMonitor;
      AState: TOBDMonitorState; const ANote: string = '');
    /// <summary>Returns the current state for one monitor.</summary>
    property MonitorState[AMonitor: TOBDReadinessMonitor]: TOBDMonitorState
      read GetMonitorState;
    /// <summary>True when at least one real data call has populated the panel.</summary>
    property HasData: Boolean read FHasData;
    /// <summary>True when at least one monitor is supported and the
    /// incomplete monitors are within the allowance.</summary>
    property Ready: Boolean read GetReady;
    /// <summary>Number of supported monitors that are still incomplete.</summary>
    property IncompleteCount: Integer read GetIncompleteCount;
    /// <summary>Number of supported monitors.</summary>
    property SupportedCount: Integer read GetSupportedCount;
  published
    /// <summary>Full monitor card or compact dashboard card.</summary>
    property Layout: TOBDReadinessLayout read FLayout write SetLayout
      default rlFull;
    /// <summary>Inspection regime used for verdict wording.</summary>
    property InspectionRegime: TOBDInspectionRegime read FInspectionRegime
      write SetInspectionRegime default irGeneric;
    /// <summary>Custom inspection name used when <see cref="InspectionRegime"/>
    /// is <c>irCustom</c>.</summary>
    property InspectionName: string read FInspectionName write SetInspectionName;
    /// <summary>Supported incomplete monitors allowed by the local regime.</summary>
    property AllowedIncomplete: Integer read FAllowedIncomplete
      write SetAllowedIncomplete default 0;
    /// <summary>True when Mode 01 PID 01 reports the MIL on.</summary>
    property MilOn: Boolean read FMilOn write SetMilOn default False;
    /// <summary>Number of stored emission DTCs reported by Mode 01 PID 01.</summary>
    property DtcCount: Integer read FDtcCount write SetDtcCount default 0;
    /// <summary>True for compression-ignition readiness monitor mapping.</summary>
    property CompressionIgnition: Boolean read FCompressionIgnition
      write SetCompressionIgnition default False;
    /// <summary>Mode 01 PID $31 distance travelled since DTCs were cleared, in km.</summary>
    property DistanceSinceClear: Integer read FDistanceSinceClear
      write SetDistanceSinceClear default -1;
    /// <summary>Mode 01 PID $30 warm-up cycles since DTCs were cleared.</summary>
    property WarmUpsSinceClear: Integer read FWarmUpsSinceClear
      write SetWarmUpsSinceClear default -1;
    /// <summary>Footer text for distance and warm-ups since the last clear.</summary>
    property SinceClearText: string read FSinceClearText write SetSinceClearText;
    /// <summary>Legend timestamp text.</summary>
    property UpdatedText: string read FUpdatedText write SetUpdatedText;
    /// <summary>Compact-layout action label.</summary>
    property ActionText: string read FActionText write SetActionText;
    /// <summary>Editable monitor states and notes.</summary>
    property Monitors: TOBDReadinessMonitorCollection read FMonitors
      write SetMonitors;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when monitor data or verdict settings change.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

const
  TILE_H = 78;

  CONTINUOUS_MONITORS: array [0 .. 2] of TOBDReadinessMonitor = (
    rmMisfire, rmFuelSystem, rmComponents);
  SPARK_MONITORS: array [0 .. 7] of TOBDReadinessMonitor = (
    rmCatalyst, rmHeatedCatalyst, rmEvaporativeSystem, rmSecondaryAirSystem,
    rmACRefrigerant, rmOxygenSensor, rmOxygenSensorHeater, rmEGRSystem);
  DIESEL_MONITORS: array [0 .. 5] of TOBDReadinessMonitor = (
    rmNMHCCatalyst, rmNOxSCRAftertreatment, rmBoostPressure, rmExhaustGasSensor,
    rmPMFilter, rmEGRVVTSystem);

function MonitorName(AMonitor: TOBDReadinessMonitor): string;
begin
  case AMonitor of
    rmMisfire:
      Result := 'Misfire';
    rmFuelSystem:
      Result := 'Fuel system';
    rmComponents:
      Result := 'Comprehensive components';
    rmCatalyst:
      Result := 'Catalyst';
    rmHeatedCatalyst:
      Result := 'Heated catalyst';
    rmEvaporativeSystem:
      Result := 'Evaporative system';
    rmSecondaryAirSystem:
      Result := 'Secondary air system';
    rmACRefrigerant:
      Result := 'A/C refrigerant';
    rmOxygenSensor:
      Result := 'Oxygen sensor';
    rmOxygenSensorHeater:
      Result := 'Oxygen sensor heater';
    rmEGRSystem:
      Result := 'EGR system';
    rmNMHCCatalyst:
      Result := 'NMHC catalyst';
    rmNOxSCRAftertreatment:
      Result := 'NOx / SCR aftertreatment';
    rmBoostPressure:
      Result := 'Boost pressure';
    rmExhaustGasSensor:
      Result := 'Exhaust gas sensor';
    rmPMFilter:
      Result := 'PM filter';
    rmEGRVVTSystem:
      Result := 'EGR / VVT system';
  else
    Result := '';
  end;
end;

function DefaultMonitorNote(AMonitor: TOBDReadinessMonitor): string;
begin
  case AMonitor of
    rmCatalyst, rmHeatedCatalyst:
      Result := 'Needs a steady cruise after warm-up';
    rmEvaporativeSystem:
      Result := 'Needs a cold soak and EVAP drive cycle';
    rmSecondaryAirSystem:
      Result := 'Needs a cold start test';
    rmACRefrigerant:
      Result := 'Needs A/C monitor conditions';
    rmOxygenSensor, rmOxygenSensorHeater:
      Result := 'Needs closed-loop motorway driving';
    rmEGRSystem, rmEGRVVTSystem:
      Result := 'Needs mixed-speed EGR operation';
    rmNOxSCRAftertreatment:
      Result := 'Needs ~20 min of motorway driving';
    rmBoostPressure:
      Result := 'Needs a boost-pressure drive cycle';
    rmExhaustGasSensor:
      Result := 'Needs exhaust sensor warm-up';
    rmPMFilter:
      Result := 'Needs a completed DPF regeneration';
  else
    Result := '';
  end;
end;

function MonitorStatusColor(const APalette: TOBDThemePalette;
  AState: TOBDMonitorState): TColor;
begin
  case AState of
    msComplete:
      Result := APalette.Success;
    msIncomplete:
      Result := APalette.Warning;
  else
    Result := APalette.Subtle;
  end;
end;

function MonitorStateText(AState: TOBDMonitorState): string;
begin
  case AState of
    msComplete:
      Result := 'Complete';
    msIncomplete:
      Result := 'Incomplete';
  else
    Result := 'Not supported';
  end;
end;

function PreviewMonitorState(AMonitor: TOBDReadinessMonitor): TOBDMonitorState;
begin
  case AMonitor of
    rmMisfire, rmFuelSystem, rmComponents, rmBoostPressure, rmExhaustGasSensor,
      rmEGRVVTSystem:
      Result := msComplete;
    rmNOxSCRAftertreatment, rmPMFilter:
      Result := msIncomplete;
    rmNMHCCatalyst:
      Result := msUnsupported;
  else
    Result := msUnsupported;
  end;
end;

{ TOBDReadinessMonitorItem }

constructor TOBDReadinessMonitorItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FState := msUnsupported;
end;

function TOBDReadinessMonitorItem.GetDisplayName: string;
begin
  Result := MonitorName(FMonitor);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDReadinessMonitorItem.SetMonitor(AValue: TOBDReadinessMonitor);
begin
  if FMonitor = AValue then
    Exit;
  FMonitor := AValue;
  Changed(False);
end;

procedure TOBDReadinessMonitorItem.SetState(AValue: TOBDMonitorState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Changed(False);
end;

procedure TOBDReadinessMonitorItem.SetNote(const AValue: string);
begin
  if FNote = AValue then
    Exit;
  FNote := AValue;
  Changed(False);
end;

{ TOBDReadinessMonitorCollection }

constructor TOBDReadinessMonitorCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDReadinessMonitorItem);
end;

function TOBDReadinessMonitorCollection.Add: TOBDReadinessMonitorItem;
begin
  Result := TOBDReadinessMonitorItem(inherited Add);
end;

function TOBDReadinessMonitorCollection.Find(AMonitor: TOBDReadinessMonitor)
  : TOBDReadinessMonitorItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Monitor = AMonitor then
      Exit(Items[I]);
end;

function TOBDReadinessMonitorCollection.GetItem(Index: Integer)
  : TOBDReadinessMonitorItem;
begin
  Result := TOBDReadinessMonitorItem(inherited GetItem(Index));
end;

procedure TOBDReadinessMonitorCollection.SetItem(Index: Integer;
  AValue: TOBDReadinessMonitorItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDReadinessMonitorCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDReadinessPanel }

constructor TOBDReadinessPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FLayout := rlFull;
  FInspectionRegime := irGeneric;
  FAllowedIncomplete := 0;
  FDistanceSinceClear := -1;
  FWarmUpsSinceClear := -1;
  FActionText := 'Open readiness';
  FMonitors := TOBDReadinessMonitorCollection.Create(Self);
  FMonitors.OnChange := MonitorsChanged;
  ResetMonitorDefaults;
  Width := 720;
  Height := 360;
end;

destructor TOBDReadinessPanel.Destroy;
begin
  FMonitors.Free;
  inherited;
end;

procedure TOBDReadinessPanel.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TOBDReadinessPanel.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TOBDReadinessPanel.BeginUpdate;
begin
  Inc(FUpdateDepth);
  FMonitors.BeginUpdate;
end;

procedure TOBDReadinessPanel.EndUpdate;
begin
  FMonitors.EndUpdate;
  if FUpdateDepth > 0 then
    Dec(FUpdateDepth);
  if FUpdateDepth = 0 then
    DoChange;
end;

procedure TOBDReadinessPanel.Clear;
begin
  BeginUpdate;
  try
    FMilOn := False;
    FDtcCount := 0;
    FCompressionIgnition := False;
    FDistanceSinceClear := -1;
    FWarmUpsSinceClear := -1;
    FHasData := False;
    ResetMonitorDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TOBDReadinessPanel.DoChange;
begin
  if FUpdateDepth > 0 then
    Exit;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDReadinessPanel.MonitorsChanged(Sender: TObject);
begin
  FHasData := True;
  DoChange;
end;

procedure TOBDReadinessPanel.ResetMonitorDefaults;
var
  M: TOBDReadinessMonitor;
  Item: TOBDReadinessMonitorItem;
  SavedOnChange: TNotifyEvent;
begin
  SavedOnChange := FMonitors.OnChange;
  FMonitors.OnChange := nil;
  FMonitors.Clear;
  try
    for M := Low(TOBDReadinessMonitor) to High(TOBDReadinessMonitor) do
    begin
      Item := FMonitors.Add;
      Item.Monitor := M;
      Item.State := msUnsupported;
      Item.Note := DefaultMonitorNote(M);
    end;
  finally
    FMonitors.OnChange := SavedOnChange;
  end;
end;

function TOBDReadinessPanel.ItemByMonitor(AMonitor: TOBDReadinessMonitor)
  : TOBDReadinessMonitorItem;
begin
  Result := FMonitors.Find(AMonitor);
end;

procedure TOBDReadinessPanel.SetLayout(AValue: TOBDReadinessLayout);
begin
  if FLayout = AValue then
    Exit;
  FLayout := AValue;
  Invalidate;
end;

procedure TOBDReadinessPanel.SetInspectionRegime(AValue: TOBDInspectionRegime);
begin
  if FInspectionRegime = AValue then
    Exit;
  FInspectionRegime := AValue;
  DoChange;
end;

procedure TOBDReadinessPanel.SetInspectionName(const AValue: string);
begin
  if FInspectionName = AValue then
    Exit;
  FInspectionName := AValue;
  DoChange;
end;

procedure TOBDReadinessPanel.SetAllowedIncomplete(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FAllowedIncomplete = AValue then
    Exit;
  FAllowedIncomplete := AValue;
  DoChange;
end;

procedure TOBDReadinessPanel.SetMilOn(AValue: Boolean);
begin
  if FMilOn = AValue then
    Exit;
  FMilOn := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDReadinessPanel.SetDtcCount(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FDtcCount = AValue then
    Exit;
  FDtcCount := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDReadinessPanel.SetCompressionIgnition(AValue: Boolean);
begin
  if FCompressionIgnition = AValue then
    Exit;
  FCompressionIgnition := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDReadinessPanel.SetDistanceSinceClear(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if FDistanceSinceClear = AValue then
    Exit;
  FDistanceSinceClear := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDReadinessPanel.SetWarmUpsSinceClear(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if FWarmUpsSinceClear = AValue then
    Exit;
  FWarmUpsSinceClear := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDReadinessPanel.SetSinceClearText(const AValue: string);
begin
  if FSinceClearText = AValue then
    Exit;
  FSinceClearText := AValue;
  Invalidate;
end;

procedure TOBDReadinessPanel.SetUpdatedText(const AValue: string);
begin
  if FUpdatedText = AValue then
    Exit;
  FUpdatedText := AValue;
  Invalidate;
end;

procedure TOBDReadinessPanel.SetActionText(const AValue: string);
begin
  if FActionText = AValue then
    Exit;
  FActionText := AValue;
  Invalidate;
end;

procedure TOBDReadinessPanel.SetMonitors(AValue: TOBDReadinessMonitorCollection);
begin
  FMonitors.Assign(AValue);
end;

function TOBDReadinessPanel.GetMonitorState(AMonitor: TOBDReadinessMonitor)
  : TOBDMonitorState;
var
  Item: TOBDReadinessMonitorItem;
begin
  Item := ItemByMonitor(AMonitor);
  if Item <> nil then
    Result := Item.State
  else
    Result := msUnsupported;
end;

function TOBDReadinessPanel.GetSupportedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FMonitors.Count - 1 do
    if FMonitors[I].State <> msUnsupported then
      Inc(Result);
end;

function TOBDReadinessPanel.GetIncompleteCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FMonitors.Count - 1 do
    if FMonitors[I].State = msIncomplete then
      Inc(Result);
end;

function TOBDReadinessPanel.GetReady: Boolean;
begin
  Result := (GetSupportedCount > 0) and
    (GetIncompleteCount <= FAllowedIncomplete);
end;

function TOBDReadinessPanel.UsePreviewData: Boolean;
begin
  Result := IsPreview and not FHasData;
end;

function TOBDReadinessPanel.VisualCompressionIgnition: Boolean;
begin
  if UsePreviewData then
    Result := True
  else
    Result := FCompressionIgnition;
end;

function TOBDReadinessPanel.VisualMilOn: Boolean;
begin
  if UsePreviewData then
    Result := True
  else
    Result := FMilOn;
end;

function TOBDReadinessPanel.VisualState(AMonitor: TOBDReadinessMonitor)
  : TOBDMonitorState;
begin
  if UsePreviewData then
    Result := PreviewMonitorState(AMonitor)
  else
    Result := GetMonitorState(AMonitor);
end;

function TOBDReadinessPanel.VisualNote(AMonitor: TOBDReadinessMonitor): string;
var
  Item: TOBDReadinessMonitorItem;
begin
  if UsePreviewData then
    Exit(DefaultMonitorNote(AMonitor));
  Item := ItemByMonitor(AMonitor);
  if Item <> nil then
    Result := Item.Note
  else
    Result := '';
end;

function TOBDReadinessPanel.VisualSupportedCount: Integer;
var
  M: TOBDReadinessMonitor;
begin
  Result := 0;
  for M := Low(TOBDReadinessMonitor) to High(TOBDReadinessMonitor) do
    if VisualState(M) <> msUnsupported then
      Inc(Result);
end;

function TOBDReadinessPanel.VisualIncompleteCount: Integer;
var
  M: TOBDReadinessMonitor;
begin
  Result := 0;
  for M := Low(TOBDReadinessMonitor) to High(TOBDReadinessMonitor) do
    if VisualState(M) = msIncomplete then
      Inc(Result);
end;

function TOBDReadinessPanel.VisualReady: Boolean;
begin
  Result := VisualIncompleteCount <= FAllowedIncomplete;
end;

function TOBDReadinessPanel.VisualSinceClearText: string;
begin
  Result := FSinceClearText;
  if Result <> '' then
    Exit;
  if UsePreviewData then
    Result := 'Since clear: 42 km · 3 warm-ups';
  if Result <> '' then
    Exit;
  if (FDistanceSinceClear >= 0) and (FWarmUpsSinceClear >= 0) then
    Result := Format('Since clear: %d km · %d warm-ups',
      [FDistanceSinceClear, FWarmUpsSinceClear])
  else if FDistanceSinceClear >= 0 then
    Result := Format('Since clear: %d km', [FDistanceSinceClear])
  else if FWarmUpsSinceClear >= 0 then
    Result := Format('Since clear: %d warm-ups', [FWarmUpsSinceClear]);
end;

function TOBDReadinessPanel.VisualUpdatedText: string;
begin
  Result := FUpdatedText;
  if (Result = '') and UsePreviewData then
    Result := 'Updated 18:42:09';
end;

function TOBDReadinessPanel.InspectionDisplayName: string;
begin
  case FInspectionRegime of
    irAPK:
      Result := 'APK';
    irKeuring:
      Result := 'keuring';
    irControleTechnique:
      Result := 'contrôle technique';
    irMOT:
      Result := 'MOT';
    irHUAU:
      Result := 'HU / AU';
    irNCT:
      Result := 'NCT';
    irCustom:
      begin
        Result := FInspectionName;
        if Result = '' then
          Result := 'emissions test';
      end;
  else
    Result := 'emissions test';
  end;
end;

procedure TOBDReadinessPanel.SetMonitor(AMonitor: TOBDReadinessMonitor;
  AState: TOBDMonitorState; const ANote: string = '');
var
  Item: TOBDReadinessMonitorItem;
begin
  Item := ItemByMonitor(AMonitor);
  if Item = nil then
    Exit;
  BeginUpdate;
  try
    Item.State := AState;
    if ANote <> '' then
      Item.Note := ANote;
    FHasData := True;
  finally
    EndUpdate;
  end;
end;

procedure TOBDReadinessPanel.LoadPID01(A, B, C, D: Byte);

  procedure ApplyContinuous(AMonitor: TOBDReadinessMonitor; ASupportBit,
    AIncompleteBit: Byte);
  var
    Item: TOBDReadinessMonitorItem;
  begin
    Item := ItemByMonitor(AMonitor);
    if Item = nil then
      Exit;
    if (B and ASupportBit) = 0 then
      Item.State := msUnsupported
    else if (B and AIncompleteBit) <> 0 then
      Item.State := msIncomplete
    else
      Item.State := msComplete;
  end;

  procedure ApplyNonContinuous(AMonitor: TOBDReadinessMonitor; ABit: Byte);
  var
    Mask: Byte;
    Item: TOBDReadinessMonitorItem;
  begin
    Item := ItemByMonitor(AMonitor);
    if Item = nil then
      Exit;
    Mask := Byte(1 shl ABit);
    if (C and Mask) = 0 then
      Item.State := msUnsupported
    else if (D and Mask) <> 0 then
      Item.State := msIncomplete
    else
      Item.State := msComplete;
  end;

  procedure MarkUnsupported(const AItems: array of TOBDReadinessMonitor);
  var
    I: Integer;
    Item: TOBDReadinessMonitorItem;
  begin
    for I := Low(AItems) to High(AItems) do
    begin
      Item := ItemByMonitor(AItems[I]);
      if Item <> nil then
        Item.State := msUnsupported;
    end;
  end;

begin
  BeginUpdate;
  try
    FMilOn := (A and $80) <> 0;
    FDtcCount := A and $7F;
    FCompressionIgnition := (B and $08) <> 0;

    ApplyContinuous(rmMisfire, $01, $10);
    ApplyContinuous(rmFuelSystem, $02, $20);
    ApplyContinuous(rmComponents, $04, $40);

    if FCompressionIgnition then
    begin
      MarkUnsupported(SPARK_MONITORS);
      ApplyNonContinuous(rmNMHCCatalyst, 0);
      ApplyNonContinuous(rmNOxSCRAftertreatment, 1);
      ApplyNonContinuous(rmBoostPressure, 3);
      ApplyNonContinuous(rmExhaustGasSensor, 5);
      ApplyNonContinuous(rmPMFilter, 6);
      ApplyNonContinuous(rmEGRVVTSystem, 7);
    end
    else
    begin
      MarkUnsupported(DIESEL_MONITORS);
      ApplyNonContinuous(rmCatalyst, 0);
      ApplyNonContinuous(rmHeatedCatalyst, 1);
      ApplyNonContinuous(rmEvaporativeSystem, 2);
      ApplyNonContinuous(rmSecondaryAirSystem, 3);
      ApplyNonContinuous(rmACRefrigerant, 4);
      ApplyNonContinuous(rmOxygenSensor, 5);
      ApplyNonContinuous(rmOxygenSensorHeater, 6);
      ApplyNonContinuous(rmEGRSystem, 7);
    end;

    FHasData := True;
  finally
    EndUpdate;
  end;
end;

procedure TOBDReadinessPanel.PaintMonitorIcon(var P: TOBDPainter;
  AState: TOBDMonitorState; CX, CY: Integer; AScale: Single);
var
  Color: TColor;
begin
  Color := MonitorStatusColor(Palette, AState);
  case AState of
    msComplete:
      P.GlyphCheck(CX, CY, Color, AScale);
    msIncomplete:
      P.GlyphPending(CX, CY, Color, AScale);
  else
    P.GlyphDash(CX, CY, Color, AScale);
  end;
end;

procedure TOBDReadinessPanel.PaintBanner(var P: TOBDPainter; const R: TRect;
  ACompact: Boolean);
var
  Kind: TOBDBannerKind;
  Title, SubText, TextName, SinceText, MilText: string;
  TextX, TitleY, SubY, MaxW, ChipW: Integer;
  MilColor: TColor;
begin
  if VisualReady then
    Kind := bnSuccess
  else
    Kind := bnWarning;
  P.BannerFrame(R, Kind);
  P.BannerIcon(R.Left + P.S(28), (R.Top + R.Bottom) div 2, Kind);

  TextName := InspectionDisplayName;
  if VisualReady then
    Title := Format('Ready for the %s', [TextName])
  else
    Title := Format('Not ready for the %s', [TextName]);

  if VisualReady and (VisualIncompleteCount = 0) then
    SubText := Format('All %d supported monitors complete',
      [VisualSupportedCount])
  else
    SubText := Format('%d of %d supported monitors incomplete — drive cycle needed',
      [VisualIncompleteCount, VisualSupportedCount]);
  if ACompact then
  begin
    if VisualMilOn then
      SubText := SubText + ' · MIL on'
    else
      SubText := SubText + ' · MIL off';
    SinceText := VisualSinceClearText;
    if SinceText <> '' then
    begin
      SinceText := StringReplace(SinceText, 'Since clear: ', '', []);
      SubText := SubText + ' · ' + SinceText;
    end;
  end;

  TextX := R.Left + P.S(52);
  if ACompact then
  begin
    TitleY := (R.Top + R.Bottom) div 2 - P.S(9);
    SubY := (R.Top + R.Bottom) div 2 + P.S(10);
    MaxW := R.Right - TextX - P.S(16);
    P.Text(TextX, TitleY, Title, 14, Palette.ForegroundText, twBold,
      taLeftJustify, MaxW);
    P.Text(TextX, SubY, SubText, 12, Palette.ForegroundText, twRegular,
      taLeftJustify, MaxW);
  end
  else
  begin
    TitleY := (R.Top + R.Bottom) div 2 - P.S(11);
    SubY := (R.Top + R.Bottom) div 2 + P.S(12);
    MaxW := R.Right - TextX - P.S(230);
    P.Text(TextX, TitleY, Title, 15, Palette.ForegroundText, twBold,
      taLeftJustify, MaxW);
    P.Text(TextX, SubY, SubText, 12.5, Palette.ForegroundText, twRegular,
      taLeftJustify, MaxW);
    if VisualMilOn then
    begin
      MilText := 'MIL ON';
      MilColor := Palette.Danger;
    end
    else
    begin
      MilText := 'MIL OFF';
      MilColor := Palette.Success;
    end;
    ChipW := P.ChipWidth(MilText);
    P.Chip(R.Right - P.S(16) - ChipW, R.Top + P.S(14), MilText, MilColor);
    SinceText := VisualSinceClearText;
    if SinceText <> '' then
      P.Text(R.Right - P.S(16), R.Bottom - P.S(22), SinceText, 11.5,
        Palette.GaugeLabel, twRegular, taRightJustify);
  end;
end;

procedure TOBDReadinessPanel.PaintMonitorTile(var P: TOBDPainter; X, Y, W,
  H: Integer; AMonitor: TOBDReadinessMonitor);
var
  State: TOBDMonitorState;
  Color, FillColor, TextColor: TColor;
  Note: string;
begin
  State := VisualState(AMonitor);
  Color := MonitorStatusColor(Palette, State);
  if State = msUnsupported then
    FillColor := Palette.Background
  else
    FillColor := Palette.GaugeFace;
  P.FillRect(Rect(X, Y, X + W, Y + H), FillColor);
  P.FrameRect(Rect(X, Y, X + W, Y + H), Palette.NeutralLight);
  if State <> msUnsupported then
    P.FillRect(Rect(X, Y, X + P.S(4), Y + H), Color);

  if State = msUnsupported then
    TextColor := Palette.Subtle
  else
    TextColor := Palette.ForegroundText;
  P.Text(X + P.S(14), Y + P.S(20), MonitorName(AMonitor), 13, TextColor,
    twSemibold, taLeftJustify, W - P.S(28));
  PaintMonitorIcon(P, State, X + P.S(21), Y + P.S(43), 0.85);
  P.Text(X + P.S(34), Y + P.S(43), MonitorStateText(State), 12, Color,
    twSemibold, taLeftJustify, W - P.S(48));
  Note := VisualNote(AMonitor);
  if (State = msIncomplete) and (Note <> '') then
    P.Text(X + P.S(14), Y + P.S(63), Note, 11, Palette.GaugeLabel,
      twRegular, taLeftJustify, W - P.S(28));
end;

procedure TOBDReadinessPanel.PaintMonitorCompact(var P: TOBDPainter; X, Y, W,
  H: Integer; AMonitor: TOBDReadinessMonitor);
var
  State: TOBDMonitorState;
  FillColor, TextColor: TColor;
begin
  State := VisualState(AMonitor);
  if State = msUnsupported then
  begin
    FillColor := Palette.Background;
    TextColor := Palette.Subtle;
  end
  else
  begin
    FillColor := Palette.GaugeFace;
    TextColor := Palette.ForegroundText;
  end;
  P.FillRect(Rect(X, Y, X + W, Y + H), FillColor);
  P.FrameRect(Rect(X, Y, X + W, Y + H), Palette.NeutralLight);
  PaintMonitorIcon(P, State, X + P.S(16), Y + H div 2, 0.75);
  P.Text(X + P.S(30), Y + H div 2, MonitorName(AMonitor), 12, TextColor,
    twRegular, taLeftJustify, W - P.S(38));
end;

procedure TOBDReadinessPanel.PaintGroup(var P: TOBDPainter; const ATitle: string;
  const AItems: array of TOBDReadinessMonitor; var Y: Integer);
var
  Gap, TileW, I, Col, Row, Rows, X, TileY: Integer;
begin
  Gap := P.S(8);
  TileW := (Width - P.S(32) - 2 * Gap) div 3;
  P.Caps(P.S(16), Y + P.S(8), ATitle);
  Inc(Y, P.S(20));
  for I := Low(AItems) to High(AItems) do
  begin
    Col := I mod 3;
    Row := I div 3;
    X := P.S(16) + Col * (TileW + Gap);
    TileY := Y + Row * (P.S(TILE_H) + Gap);
    PaintMonitorTile(P, X, TileY, TileW, P.S(TILE_H), AItems[I]);
  end;
  Rows := (Length(AItems) + 2) div 3;
  Inc(Y, Rows * (P.S(TILE_H) + Gap) + P.S(6));
end;

procedure TOBDReadinessPanel.PaintFull(var P: TOBDPainter);
var
  Y, LX, LY: Integer;
  SourceText, Updated: string;
begin
  P.Text(P.S(16), P.S(26), 'Readiness monitors', 16, Palette.ForegroundText,
    twBold);
  if VisualCompressionIgnition then
    SourceText := 'Compression ignition  ·  Mode 01 PID 01'
  else
    SourceText := 'Spark ignition  ·  Mode 01 PID 01';
  P.Text(Width - P.S(16), P.S(26), SourceText, 11.5, Palette.GaugeLabel,
    twRegular, taRightJustify, Width - P.S(32));
  PaintBanner(P, Rect(P.S(16), P.S(50), Width - P.S(16), P.S(122)), False);

  Y := P.S(138);
  PaintGroup(P, 'Continuous', CONTINUOUS_MONITORS, Y);
  if VisualCompressionIgnition then
    PaintGroup(P, 'Non-continuous', DIESEL_MONITORS, Y)
  else
    PaintGroup(P, 'Non-continuous', SPARK_MONITORS, Y);

  LY := Height - P.S(22);
  LX := P.S(16);
  PaintMonitorIcon(P, msComplete, LX + P.S(6), LY, 0.8);
  Inc(LX, P.Text(LX + P.S(16), LY, 'Complete', 11.5, Palette.GaugeLabel) +
    P.S(36));
  PaintMonitorIcon(P, msIncomplete, LX + P.S(6), LY, 0.8);
  Inc(LX, P.Text(LX + P.S(16), LY, 'Incomplete', 11.5, Palette.GaugeLabel) +
    P.S(36));
  PaintMonitorIcon(P, msUnsupported, LX + P.S(6), LY, 0.8);
  P.Text(LX + P.S(16), LY, 'Not supported', 11.5, Palette.GaugeLabel);
  Updated := VisualUpdatedText;
  if Updated <> '' then
    P.Text(Width - P.S(16), LY, Updated, 11.5, Palette.GaugeLabel,
      twRegular, taRightJustify);
end;

procedure TOBDReadinessPanel.PaintCompact(var P: TOBDPainter);
var
  Gap, CellW, CellH, I, X, Y: Integer;
  Items: array of TOBDReadinessMonitor;
begin
  P.Text(P.S(16), P.S(24), 'Readiness', 15, Palette.ForegroundText, twBold);
  if FActionText <> '' then
    P.Text(Width - P.S(16), P.S(24), FActionText, 11.5, P.AccentText,
      twSemibold, taRightJustify);
  PaintBanner(P, Rect(P.S(16), P.S(44), Width - P.S(16), P.S(100)), True);

  if VisualCompressionIgnition then
  begin
    SetLength(Items, Length(CONTINUOUS_MONITORS) + Length(DIESEL_MONITORS));
    for I := 0 to High(CONTINUOUS_MONITORS) do
      Items[I] := CONTINUOUS_MONITORS[I];
    for I := 0 to High(DIESEL_MONITORS) do
      Items[Length(CONTINUOUS_MONITORS) + I] := DIESEL_MONITORS[I];
  end
  else
  begin
    SetLength(Items, Length(CONTINUOUS_MONITORS) + Length(SPARK_MONITORS));
    for I := 0 to High(CONTINUOUS_MONITORS) do
      Items[I] := CONTINUOUS_MONITORS[I];
    for I := 0 to High(SPARK_MONITORS) do
      Items[Length(CONTINUOUS_MONITORS) + I] := SPARK_MONITORS[I];
  end;

  Gap := P.S(8);
  CellW := (Width - P.S(32) - 2 * Gap) div 3;
  CellH := P.S(32);
  for I := 0 to High(Items) do
  begin
    X := P.S(16) + (I mod 3) * (CellW + Gap);
    Y := P.S(112) + (I div 3) * (CellH + P.S(6));
    PaintMonitorCompact(P, X, Y, CellW, CellH, Items[I]);
  end;
end;

procedure TOBDReadinessPanel.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), clNone, SurfaceColor);
    if FLayout = rlCompact then
      PaintCompact(P)
    else
      PaintFull(P);
  finally
    P.Free;
  end;
end;

end.
