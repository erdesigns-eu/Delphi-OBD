//------------------------------------------------------------------------------
//  ERD.UI.ConnectionBar
//
//  TOBDConnectionBar - one strip with everything a mechanic checks
//  before trusting the numbers on screen: link state, adapter,
//  protocol, battery voltage and VIN.
//
//  Link state and adapter name follow a bound TOBDConnection /
//  TOBDAdapter automatically (polled twice a second on the main
//  thread). Battery voltage follows its own channel (PID $42,
//  control module voltage) and is coloured: below 12.0 V amber,
//  below 11.5 V or above 15.0 V red. Protocol and VIN are set by the
//  host, which knows them after detection and Mode 09.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.ConnectionBar;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Units,
  ERD.UI.Binding,
  ERD.Connection.Types,
  ERD.Connection,
  ERD.Adapter,
  ERD.Service.LiveData;

type
  /// <summary>Link state shown by the bar.</summary>
  TOBDLinkState = (
    /// <summary>No connection.</summary>
    lnkDisconnected,
    /// <summary>Opening the port / initialising the adapter.</summary>
    lnkConnecting,
    /// <summary>Connected and ready.</summary>
    lnkConnected,
    /// <summary>Last attempt failed.</summary>
    lnkError);

  /// <summary>Status strip: link, adapter, protocol, battery and VIN.
  /// </summary>
  /// <remarks>Aligns to the top of its parent by default.</remarks>
  TOBDConnectionBar = class(TOBDCustomControl)
  strict private
    FConnection: TOBDConnection;
    FAdapter: TOBDAdapter;
    FBattery: TOBDChannelBinding;
    FLinkState: TOBDLinkState;
    FAdapterText: string;
    FProtocolText: string;
    FVIN: string;
    FPollTimer: TTimer;
    procedure SetConnection(AValue: TOBDConnection);
    procedure SetAdapter(AValue: TOBDAdapter);
    procedure SetBattery(AValue: TOBDChannelBinding);
    procedure SetLinkState(AValue: TOBDLinkState);
    procedure SetAdapterText(const AValue: string);
    procedure SetProtocolText(const AValue: string);
    procedure SetVIN(const AValue: string);
    procedure HandleBattery(Sender: TObject);
    procedure HandlePoll(Sender: TObject);
    procedure UpdatePollTimer;
    function LinkText: string;
    function LinkColor: TColor;
    function BatteryText: string;
    function BatteryColor: TColor;
    procedure DrawSegment(ACanvas: TCanvas; var AX: Integer;
      const ALabel, AValue: string; AValueColor: TColor);
  protected
    /// <summary>Re-subscribes the battery channel after streaming.
    /// </summary>
    procedure Loaded; override;
    /// <summary>Drops references to freed components.</summary>
    /// <param name="AComponent">Component inserted / removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the strip.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates a top-aligned bar.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops polling and releases the battery channel.
    /// </summary>
    destructor Destroy; override;
    /// <summary>Reads link state and adapter name from the bound
    /// components now (the poll timer does this every 500 ms).
    /// </summary>
    procedure RefreshFromComponents;
    /// <summary>Binds the battery channel to a data source.</summary>
    /// <param name="ASource">A <c>TOBDLiveData</c> or nil.</param>
    procedure AssignDataSource(ASource: TComponent); override;
  published
    /// <summary>Connection whose state is shown. Optional.</summary>
    property Connection: TOBDConnection read FConnection write SetConnection;
    /// <summary>Adapter whose identity is shown. Optional.</summary>
    property Adapter: TOBDAdapter read FAdapter write SetAdapter;
    /// <summary>Battery voltage channel (PID $42 by default).</summary>
    property Battery: TOBDChannelBinding read FBattery write SetBattery;
    /// <summary>Link state. Follows <see cref="Connection"/> when
    /// bound; set it yourself otherwise.</summary>
    property LinkState: TOBDLinkState read FLinkState write SetLinkState
      default lnkDisconnected;
    /// <summary>Adapter name. Follows <see cref="Adapter"/> when
    /// bound and identified.</summary>
    property AdapterText: string read FAdapterText write SetAdapterText;
    /// <summary>Vehicle protocol, e.g. <c>'ISO 15765-4 CAN 11/500'</c>.
    /// </summary>
    property ProtocolText: string read FProtocolText write SetProtocolText;
    /// <summary>Vehicle identification number.</summary>
    property VIN: string read FVIN write SetVIN;
    property Align default alTop;
  end;

implementation

constructor TOBDConnectionBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 640;
  Height := 36;
  Align := alTop;
  FLinkState := lnkDisconnected;
  FBattery := TOBDChannelBinding.Create(Self);
  FBattery.PID := $42;
  FBattery.OnValue := HandleBattery;
  FBattery.OnStateChange := HandleBattery;
end;

destructor TOBDConnectionBar.Destroy;
begin
  FPollTimer.Free;
  FBattery.Free;
  inherited;
end;

procedure TOBDConnectionBar.Loaded;
begin
  inherited;
  FBattery.Rebind;
  UpdatePollTimer;
end;

procedure TOBDConnectionBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation <> opRemove then
    Exit;
  if AComponent = FConnection then
  begin
    FConnection := nil;
    UpdatePollTimer;
  end;
  if AComponent = FAdapter then
  begin
    FAdapter := nil;
    UpdatePollTimer;
  end;
  if FBattery <> nil then
    FBattery.SourceRemoved(AComponent);
end;

procedure TOBDConnectionBar.AssignDataSource(ASource: TComponent);
begin
  if ASource is TOBDLiveData then
    FBattery.Source := TOBDLiveData(ASource)
  else
    FBattery.Source := nil;
end;

procedure TOBDConnectionBar.SetConnection(AValue: TOBDConnection);
begin
  if FConnection = AValue then
    Exit;
  if FConnection <> nil then
    FConnection.RemoveFreeNotification(Self);
  FConnection := AValue;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
  UpdatePollTimer;
  RefreshFromComponents;
end;

procedure TOBDConnectionBar.SetAdapter(AValue: TOBDAdapter);
begin
  if FAdapter = AValue then
    Exit;
  if FAdapter <> nil then
    FAdapter.RemoveFreeNotification(Self);
  FAdapter := AValue;
  if FAdapter <> nil then
    FAdapter.FreeNotification(Self);
  UpdatePollTimer;
  RefreshFromComponents;
end;

procedure TOBDConnectionBar.SetBattery(AValue: TOBDChannelBinding);
begin
  FBattery.Assign(AValue);
end;

procedure TOBDConnectionBar.SetLinkState(AValue: TOBDLinkState);
begin
  if FLinkState = AValue then
    Exit;
  FLinkState := AValue;
  Invalidate;
end;

procedure TOBDConnectionBar.SetAdapterText(const AValue: string);
begin
  if FAdapterText = AValue then
    Exit;
  FAdapterText := AValue;
  Invalidate;
end;

procedure TOBDConnectionBar.SetProtocolText(const AValue: string);
begin
  if FProtocolText = AValue then
    Exit;
  FProtocolText := AValue;
  Invalidate;
end;

procedure TOBDConnectionBar.SetVIN(const AValue: string);
begin
  if FVIN = AValue then
    Exit;
  FVIN := AValue;
  Invalidate;
end;

procedure TOBDConnectionBar.HandleBattery(Sender: TObject);
begin
  Invalidate;
end;

procedure TOBDConnectionBar.UpdatePollTimer;
var
  Want: Boolean;
begin
  Want := ((FConnection <> nil) or (FAdapter <> nil)) and
    not(csDesigning in ComponentState) and
    not(csLoading in ComponentState);
  if Want and (FPollTimer = nil) then
  begin
    FPollTimer := TTimer.Create(nil);
    FPollTimer.Interval := 500;
    FPollTimer.OnTimer := HandlePoll;
  end;
  if FPollTimer <> nil then
    FPollTimer.Enabled := Want;
end;

procedure TOBDConnectionBar.HandlePoll(Sender: TObject);
begin
  RefreshFromComponents;
end;

procedure TOBDConnectionBar.RefreshFromComponents;
var
  AdapterName: string;
begin
  if FConnection <> nil then
    case FConnection.State of
      csOpen:
        LinkState := lnkConnected;
      csOpening:
        LinkState := lnkConnecting;
      csError:
        LinkState := lnkError;
    else
      LinkState := lnkDisconnected;
    end;
  if FAdapter <> nil then
  begin
    AdapterName := FAdapter.Identity.DisplayName;
    if AdapterName <> '' then
      AdapterText := AdapterName;
  end;
end;

function TOBDConnectionBar.LinkText: string;
begin
  case FLinkState of
    lnkConnecting:
      Result := 'Connecting';
    lnkConnected:
      Result := 'Connected';
    lnkError:
      Result := 'Error';
  else
    Result := 'Disconnected';
  end;
end;

function TOBDConnectionBar.LinkColor: TColor;
begin
  case FLinkState of
    lnkConnecting:
      Result := Palette.Warning;
    lnkConnected:
      Result := Palette.Success;
    lnkError:
      Result := Palette.Danger;
  else
    Result := Palette.Subtle;
  end;
end;

function TOBDConnectionBar.BatteryText: string;
begin
  if IsPreview and not FBattery.HasValue then
    Result := '12.6 V'
  else if not FBattery.HasValue or IsNan(FBattery.Value) then
    Result := '-- V'
  else
    Result := OBDFormatNumber(FBattery.Value, 1) + ' V';
end;

function TOBDConnectionBar.BatteryColor: TColor;
var
  V: Double;
begin
  if not FBattery.HasValue or IsNan(FBattery.Value) or FBattery.IsStale then
    Exit(Palette.Subtle);
  V := FBattery.Value;
  if (V < 11.5) or (V > 15.0) then
    Result := Palette.Danger
  else if V < 12.0 then
    Result := Palette.Warning
  else
    Result := EffectiveForeground;
end;

procedure TOBDConnectionBar.DrawSegment(ACanvas: TCanvas; var AX: Integer;
  const ALabel, AValue: string; AValueColor: TColor);
var
  Y: Integer;
begin
  if AX >= Width then
    Exit;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := Palette.GaugeLabel;
  Y := (Height - ACanvas.TextHeight(ALabel)) div 2;
  ACanvas.TextOut(AX, Y, ALabel);
  Inc(AX, ACanvas.TextWidth(ALabel) + ScaleValue(4));
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := AValueColor;
  ACanvas.TextOut(AX, Y, AValue);
  Inc(AX, ACanvas.TextWidth(AValue) + ScaleValue(18));
end;

procedure TOBDConnectionBar.PaintControl(ACanvas: TCanvas);
var
  X, Dot, Pad: Integer;
  S: string;
begin
  Pad := ScaleValue(10);
  ACanvas.Pen.Color := EffectiveBorder;
  ACanvas.MoveTo(0, Height - 1);
  ACanvas.LineTo(Width, Height - 1);
  ACanvas.Font.Name := Font.Name;
  ACanvas.Font.Height := -System.Math.Max(ScaleValue(11),
    System.Math.Min(ScaleValue(16), Round(Height * 0.42)));

  // Link dot and state.
  Dot := System.Math.Max(ScaleValue(8), Round(Height * 0.32));
  X := Pad;
  ACanvas.Brush.Style := bsSolid;
  if IsPreview and (FConnection = nil) and (FLinkState = lnkDisconnected) then
    ACanvas.Brush.Color := Palette.Success
  else
    ACanvas.Brush.Color := LinkColor;
  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Ellipse(X, (Height - Dot) div 2, X + Dot, (Height + Dot) div 2);
  Inc(X, Dot + ScaleValue(6));
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := EffectiveForeground;
  if IsPreview and (FConnection = nil) and (FLinkState = lnkDisconnected) then
    S := 'Connected'
  else
    S := LinkText;
  ACanvas.TextOut(X, (Height - ACanvas.TextHeight(S)) div 2, S);
  Inc(X, ACanvas.TextWidth(S) + ScaleValue(18));

  S := FAdapterText;
  if (S = '') and IsPreview then
    S := 'ELM327 v1.5';
  if S = '' then
    S := '--';
  DrawSegment(ACanvas, X, 'Adapter', S, EffectiveForeground);

  S := FProtocolText;
  if (S = '') and IsPreview then
    S := 'ISO 15765-4 CAN';
  if S = '' then
    S := '--';
  DrawSegment(ACanvas, X, 'Protocol', S, EffectiveForeground);

  DrawSegment(ACanvas, X, 'Battery', BatteryText, BatteryColor);

  S := FVIN;
  if (S = '') and IsPreview then
    S := 'WVWZZZ1KZAW000000';
  if S = '' then
    S := '--';
  DrawSegment(ACanvas, X, 'VIN', S, EffectiveForeground);
end;

end.
