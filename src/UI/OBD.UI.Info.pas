//------------------------------------------------------------------------------
//  OBD.UI.Info
//
//  Info / vehicle widgets for the A2 inventory:
//
//    TOBDVINCard       decoded-VIN summary card. Reads from a
//                      bound TOBDVINInspector + Refresh
//                      method; renders region / manufacturer /
//                      model-year / plant / serial as a
//                      stacked card.
//    TOBDAdapterPanel  adapter family / chip / firmware /
//                      protocol / DTC-count strip. Binds to
//                      TOBDAdapter + (optionally) TOBDDTCs.
//    TOBDOdometer      total + Trip A + Trip B readout with
//                      hot-spot reset buttons (paint-driven,
//                      no sub-controls).
//    TOBDClock         analogue or digital clock. 12 / 24 h
//                      format, optional seconds hand.
//
//  All four inherit theme / HiDPI / VCL-Style awareness from
//  TOBDCustomControl, route state mutations through
//  TBindings.Notify, and guard timers + subscriptions with
//  csDesigning so the IDE Designer stays responsive.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Info;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.DateUtils,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.Service.VINDecoder.Types,
  OBD.Service.VINInspector,
  OBD.Adapter.Types,
  OBD.Adapter,
  OBD.Service.DTCs;

type
  /// <summary>Decoded-VIN summary card. Drop on a form,
  /// point <see cref="Source"/> at a configured
  /// <see cref="TOBDVINInspector"/>, call <c>Refresh</c> when
  /// the host's <c>OnDecoded</c> fires (or any time you want
  /// the card to re-read). The card never subscribes to the
  /// inspector's single-cast events — keeps the host's wiring
  /// untouched.</summary>
  TOBDVINCard = class(TOBDCustomControl)
  strict private
    FSource:      TOBDVINInspector;
    FInfo:        TOBDVINInfo;
    FTitleFont:   TFont;
    FBodyFont:    TFont;
    procedure SetSource(AValue: TOBDVINInspector);
    procedure SetTitleFont(AValue: TFont);
    procedure SetBodyFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Pulls the decoded info from
    /// <see cref="Source"/> and repaints. Silently no-ops
    /// when <c>Source</c> is nil.</summary>
    procedure Refresh;
    /// <summary>Manual route: skip the bound inspector and
    /// render an arbitrary <c>TOBDVINInfo</c> record.</summary>
    procedure SetInfo(const AInfo: TOBDVINInfo);
    /// <summary>Currently-rendered info record.</summary>
    property  Info: TOBDVINInfo read FInfo;
  published
    /// <summary>Bound inspector. nil = host drives the card
    /// via <see cref="SetInfo"/>.</summary>
    property Source: TOBDVINInspector
      read FSource write SetSource;
    /// <summary>Headline (VIN) font.</summary>
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    /// <summary>Body / detail-row font.</summary>
    property BodyFont:  TFont read FBodyFont  write SetBodyFont;
  end;

  /// <summary>Adapter info strip: family / chip / firmware /
  /// current protocol plus an optional DTC-count badge driven
  /// by a bound <see cref="TOBDDTCs"/>. Hosts that want the
  /// adapter info to track a live connection wire
  /// <c>TOBDAdapter.OnIdentityChanged</c> to
  /// <see cref="Refresh"/>.</summary>
  TOBDAdapterPanel = class(TOBDCustomControl)
  strict private
    FAdapter:      TOBDAdapter;
    FDTCs:         TOBDDTCs;
    FDTCCount:     Integer;
    FProtocolName: string;
    FCaptionFont:  TFont;
    FValueFont:    TFont;
    procedure SetAdapter(AValue: TOBDAdapter);
    procedure SetDTCs(AValue: TOBDDTCs);
    procedure SetDTCCount(AValue: Integer);
    procedure SetProtocolName(const AValue: string);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Force a repaint reading from the bound
    /// adapter / DTCs.</summary>
    procedure Refresh;
  published
    /// <summary>Bound adapter. nil hides the chip / firmware
    /// rows.</summary>
    property Adapter: TOBDAdapter read FAdapter write SetAdapter;
    /// <summary>Optional DTCs source — drives the DTC count
    /// badge. Hosts set this so a glance at the panel shows
    /// "3 DTCs" without separate plumbing.</summary>
    property DTCs:    TOBDDTCs    read FDTCs    write SetDTCs;
    /// <summary>Override the displayed DTC count directly
    /// (used by hosts that maintain the count themselves).
    /// </summary>
    property DTCCount: Integer
      read FDTCCount write SetDTCCount default 0;
    /// <summary>Currently-active protocol display string
    /// (e.g. "ISO 15765-4 CAN 11 / 500"). Hosts set this from
    /// their detection logic.</summary>
    property ProtocolName: string
      read FProtocolName write SetProtocolName;
    /// <summary>Caption / row-label font.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Value font (chip name, firmware, ...).</summary>
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>Fires when the user clicks one of the trip-reset
  /// hot-spots. <paramref name="ATrip"/> is 'A' or 'B'.
  /// </summary>
  TOBDOdometerTripResetEvent = procedure(Sender: TObject;
    ATrip: Char) of object;

  /// <summary>Three-row odometer: total kilometres / miles +
  /// Trip A + Trip B. Both trip rows expose a small "reset"
  /// hot-spot the user can click to zero the trip; the
  /// <see cref="OnTripReset"/> event fires so the host can
  /// persist the new value.</summary>
  TOBDOdometer = class(TOBDCustomControl)
  strict private
    FTotal:       Double;
    FTripA:       Double;
    FTripB:       Double;
    FUnit:        string;
    FDecimals:    Byte;
    FShowReset:   Boolean;
    FResetARect:  TRect;
    FResetBRect:  TRect;
    FCaptionFont: TFont;
    FValueFont:   TFont;
    FOnTripReset: TOBDOdometerTripResetEvent;
    procedure SetTotal(AValue: Double);
    procedure SetTripA(AValue: Double);
    procedure SetTripB(AValue: Double);
    procedure SetUnit(const AValue: string);
    procedure SetDecimals(AValue: Byte);
    procedure SetShowReset(AValue: Boolean);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  FormatVal(AValue: Double): string;
    procedure FireTripReset(ATrip: Char);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Programmatic reset for trip A — useful for
    /// hosts that drive resets from a menu item or hotkey.
    /// Fires <see cref="OnTripReset"/>.</summary>
    procedure ResetTripA;
    /// <summary>Programmatic reset for trip B.</summary>
    procedure ResetTripB;
  published
    /// <summary>Lifetime total distance.</summary>
    property Total: Double read FTotal write SetTotal;
    /// <summary>Trip A counter.</summary>
    property TripA: Double read FTripA write SetTripA;
    /// <summary>Trip B counter.</summary>
    property TripB: Double read FTripB write SetTripB;
    /// <summary>Distance unit suffix. Default "km".</summary>
    property &Unit: string read FUnit write SetUnit;
    /// <summary>Decimals on the trip readouts. Total always
    /// renders integer-only (matches dashboard convention).
    /// Default 1.</summary>
    property Decimals: Byte read FDecimals write SetDecimals
      default 1;
    /// <summary>Show the reset hot-spots next to each trip
    /// row. Default True.</summary>
    property ShowReset: Boolean
      read FShowReset write SetShowReset default True;
    /// <summary>Caption + reset-button font.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Big readout font.</summary>
    property ValueFont: TFont
      read FValueFont write SetValueFont;
    /// <summary>Fires when the user clicks a trip's reset
    /// hot-spot.</summary>
    property OnTripReset: TOBDOdometerTripResetEvent
      read FOnTripReset write FOnTripReset;
  end;

  /// <summary>Clock face style.</summary>
  TOBDClockStyle = (
    /// <summary>Round analogue dial with hands.</summary>
    csAnalog,
    /// <summary>Big-font digital readout.</summary>
    csDigital
  );

  /// <summary>Hour-format selector.</summary>
  TOBDClockHourFormat = (
    /// <summary>00..23.</summary>
    h24,
    /// <summary>1..12 with am/pm tag.</summary>
    h12
  );

  /// <summary>Wall-clock visual. Tick rate auto-throttles to
  /// 1 second when seconds are hidden, 250 ms when shown.
  /// </summary>
  TOBDClock = class(TOBDCustomControl)
  strict private
    FStyle:        TOBDClockStyle;
    FHourFormat:   TOBDClockHourFormat;
    FShowSeconds:  Boolean;
    FNow:          TDateTime;
    FTimer:        TTimer;
    FDigitalFont:  TFont;
    procedure SetStyle(AValue: TOBDClockStyle);
    procedure SetHourFormat(AValue: TOBDClockHourFormat);
    procedure SetShowSeconds(AValue: Boolean);
    procedure SetDigitalFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure HandleTimer(Sender: TObject);
    procedure UpdateTimerInterval;
    procedure NotifyBindings;
    procedure PaintAnalog(ACanvas: TCanvas);
    procedure PaintDigital(ACanvas: TCanvas);
  protected
    procedure Loaded; override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Analog dial or digital readout. Default
    /// <c>csAnalog</c>.</summary>
    property Style: TOBDClockStyle read FStyle write SetStyle
      default csAnalog;
    /// <summary>24 h or 12 h with am/pm. Default <c>h24</c>.
    /// </summary>
    property HourFormat: TOBDClockHourFormat
      read FHourFormat write SetHourFormat default h24;
    /// <summary>Show the second hand / seconds digits.
    /// Default True.</summary>
    property ShowSeconds: Boolean
      read FShowSeconds write SetShowSeconds default True;
    /// <summary>Digital-mode font.</summary>
    property DigitalFont: TFont
      read FDigitalFont write SetDigitalFont;
  end;

implementation

function AdapterFamilyDisplay(AFamily: TOBDAdapterFamily): string;
begin
  case AFamily of
    afELM327:  Result := 'ELM327';
    afOBDLink: Result := 'OBDLink';
    afJ2534:   Result := 'J2534';
    afDoIP:    Result := 'DoIP';
  else
    Result := 'Unknown';
  end;
end;

{ ---- TOBDVINCard --------------------------------------------------------- }

constructor TOBDVINCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 180;
  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Consolas';
  FTitleFont.Size := 14;
  FTitleFont.Style := [fsBold];
  FTitleFont.OnChange := HandleFontChange;
  FBodyFont := TFont.Create;
  FBodyFont.Name := 'Segoe UI';
  FBodyFont.Size := 9;
  FBodyFont.OnChange := HandleFontChange;
end;

destructor TOBDVINCard.Destroy;
begin
  FTitleFont.Free;
  FBodyFont.Free;
  inherited;
end;

procedure TOBDVINCard.Loaded;
begin
  inherited;
  // DFM streaming finished — pull the decoded info from the
  // bound inspector so the first paint reflects reality.
  if FSource <> nil then
    Refresh;
end;

procedure TOBDVINCard.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSource) then
    FSource := nil;
end;

procedure TOBDVINCard.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDVINCard.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDVINCard.SetSource(AValue: TOBDVINInspector);
begin
  if FSource = AValue then Exit;
  if FSource <> nil then FSource.RemoveFreeNotification(Self);
  FSource := AValue;
  if FSource <> nil then FSource.FreeNotification(Self);
  Refresh;
end;

procedure TOBDVINCard.SetTitleFont(AValue: TFont);
begin
  FTitleFont.Assign(AValue);
end;

procedure TOBDVINCard.SetBodyFont(AValue: TFont);
begin
  FBodyFont.Assign(AValue);
end;

procedure TOBDVINCard.Refresh;
begin
  if FSource <> nil then
    FInfo := FSource.Info
  else
    FInfo := Default(TOBDVINInfo);
  NotifyBindings;
  Repaint;
end;

procedure TOBDVINCard.SetInfo(const AInfo: TOBDVINInfo);
begin
  FInfo := AInfo;
  NotifyBindings;
  Repaint;
end;

procedure TOBDVINCard.PaintControl(ACanvas: TCanvas);
var
  Pad, RowH, Y: Integer;

  procedure Row(const ALabel, AVal: string);
  begin
    if AVal = '' then Exit;
    ACanvas.Font := FBodyFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y, ALabel);
    ACanvas.Font.Style := [fsBold];
    ACanvas.TextOut(Pad + ScaleValue(96), Y, AVal);
    ACanvas.Font.Style := [];
    Inc(Y, RowH);
  end;

begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(10);

  // Headline VIN.
  ACanvas.Font := FTitleFont;
  ACanvas.Font.Color := EffectiveAccent;
  if FInfo.VIN <> '' then
    ACanvas.TextOut(Pad, Pad, FInfo.VIN)
  else
    ACanvas.TextOut(Pad, Pad, '(VIN not set)');

  Y := Pad + ACanvas.TextHeight('M') + ScaleValue(6);
  RowH := Round(FBodyFont.Size * 1.8);
  if RowH < ScaleValue(16) then RowH := ScaleValue(16);

  // Invalid-state row.
  if (FInfo.VIN <> '') and not FInfo.Valid then
  begin
    ACanvas.Font := FBodyFont;
    ACanvas.Font.Color := Palette.Danger;
    ACanvas.TextOut(Pad, Y,
      'Invalid: ' + FInfo.InvalidReason);
    Inc(Y, RowH);
    Exit;
  end;

  Row('Region',       FInfo.Region.Name);
  Row('Country',      FInfo.Country.Name);
  Row('Manufacturer', FInfo.Manufacturer.Name);
  if FInfo.ModelYear > 0 then
    Row('Model year', IntToStr(FInfo.ModelYear))
  else
    Row('Model year', '');
  Row('Plant',        FInfo.Plant.Name);
  Row('Serial',       FInfo.Serial);
  if FInfo.CheckDigit <> #0 then
  begin
    if FInfo.CheckDigitValid then
      Row('Check digit', FInfo.CheckDigit + ' (valid)')
    else
      Row('Check digit', FInfo.CheckDigit + ' (invalid)');
  end;
end;

{ ---- TOBDAdapterPanel ---------------------------------------------------- }

constructor TOBDAdapterPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 120;
  FProtocolName := '';
  FDTCCount := 0;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 10;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDAdapterPanel.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDAdapterPanel.Loaded;
begin
  inherited;
  // Re-read adapter + DTCs once the form finishes streaming.
  Refresh;
end;

procedure TOBDAdapterPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FAdapter then FAdapter := nil;
    if AComponent = FDTCs    then FDTCs    := nil;
  end;
end;

procedure TOBDAdapterPanel.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDAdapterPanel.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDAdapterPanel.SetAdapter(AValue: TOBDAdapter);
begin
  if FAdapter = AValue then Exit;
  if FAdapter <> nil then FAdapter.RemoveFreeNotification(Self);
  FAdapter := AValue;
  if FAdapter <> nil then FAdapter.FreeNotification(Self);
  Refresh;
end;

procedure TOBDAdapterPanel.SetDTCs(AValue: TOBDDTCs);
begin
  if FDTCs = AValue then Exit;
  if FDTCs <> nil then FDTCs.RemoveFreeNotification(Self);
  FDTCs := AValue;
  if FDTCs <> nil then FDTCs.FreeNotification(Self);
  Refresh;
end;

procedure TOBDAdapterPanel.SetDTCCount(AValue: Integer);
begin
  if FDTCCount = AValue then Exit;
  FDTCCount := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDAdapterPanel.SetProtocolName(const AValue: string);
begin
  if FProtocolName = AValue then Exit;
  FProtocolName := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDAdapterPanel.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDAdapterPanel.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDAdapterPanel.Refresh;
begin
  NotifyBindings;
  Repaint;
end;

procedure TOBDAdapterPanel.PaintControl(ACanvas: TCanvas);
var
  Pad, RowH, Y: Integer;
  Family, Chip, Firmware: string;
  BadgeText: string;
  BadgeW: Integer;
  BadgeRect: TGPRectF;
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;

  procedure Row(const ALabel, AVal: string);
  begin
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y, ALabel);
    ACanvas.Font := FValueFont;
    ACanvas.Font.Color := EffectiveAccent;
    ACanvas.TextOut(Pad + ScaleValue(96), Y, AVal);
    Inc(Y, RowH);
  end;

begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(10);
  Y := Pad;
  RowH := Round(FValueFont.Size * 1.9);
  if RowH < ScaleValue(18) then RowH := ScaleValue(18);

  if FAdapter <> nil then
  begin
    Family   := AdapterFamilyDisplay(FAdapter.Family);
    Chip     := FAdapter.Identity.DisplayName;
    Firmware := FAdapter.Identity.FirmwareVersion;
  end
  else
  begin
    Family   := '(not set)';
    Chip     := '';
    Firmware := '';
  end;

  Row('Family',   Family);
  if Chip     <> '' then Row('Chip',     Chip);
  if Firmware <> '' then Row('Firmware', Firmware);
  if FProtocolName <> '' then Row('Protocol', FProtocolName);

  // DTC count badge (top-right). Source: explicit DTCCount
  // override wins; otherwise we don't have a live count
  // without a recent read (TOBDDTCs caches results in the
  // host's handler), so we just render whatever DTCCount
  // holds.
  if (FDTCCount > 0) or (FDTCs <> nil) then
  begin
    BadgeText := Format('%d DTC', [FDTCCount]);
    if FDTCCount <> 1 then BadgeText := BadgeText + 's';
    ACanvas.Font := FValueFont;
    BadgeW := ACanvas.TextWidth(BadgeText) + ScaleValue(16);
    BadgeRect.X := Width - BadgeW - Pad;
    BadgeRect.Y := Pad;
    BadgeRect.Width  := BadgeW;
    BadgeRect.Height := ScaleValue(24);

    Graphics := TGPGraphics.Create(ACanvas.Handle);
    if FDTCCount > 0 then
      Brush := TGPSolidBrush.Create(ColorToARGB(Palette.Danger))
    else
      Brush := TGPSolidBrush.Create(ColorToARGB(Palette.Success));
    try
      Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
      Graphics.FillEllipse(Brush, BadgeRect);
    finally
      Brush.Free;
      Graphics.Free;
    end;
    ACanvas.Font.Color := clWhite;
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(
      Round(BadgeRect.X + ScaleValue(8)),
      Round(BadgeRect.Y + (BadgeRect.Height -
        ACanvas.TextHeight(BadgeText)) / 2),
      BadgeText);
  end;
end;

{ ---- TOBDOdometer ------------------------------------------------------- }

constructor TOBDOdometer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 260;
  Height := 110;
  FTotal := 0;
  FTripA := 0;
  FTripB := 0;
  FUnit  := 'km';
  FDecimals := 1;
  FShowReset := True;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 14;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDOdometer.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDOdometer.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDOdometer.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDOdometer.SetTotal(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FTotal, AValue) then Exit;
  FTotal := AValue; NotifyBindings; Repaint;
end;

procedure TOBDOdometer.SetTripA(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FTripA, AValue) then Exit;
  FTripA := AValue; NotifyBindings; Repaint;
end;

procedure TOBDOdometer.SetTripB(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FTripB, AValue) then Exit;
  FTripB := AValue; NotifyBindings; Repaint;
end;

procedure TOBDOdometer.SetUnit(const AValue: string);
begin
  if FUnit = AValue then Exit;
  FUnit := AValue; Repaint;
end;

procedure TOBDOdometer.SetDecimals(AValue: Byte);
begin
  if FDecimals = AValue then Exit;
  FDecimals := AValue; Repaint;
end;

procedure TOBDOdometer.SetShowReset(AValue: Boolean);
begin
  if FShowReset = AValue then Exit;
  FShowReset := AValue; Repaint;
end;

procedure TOBDOdometer.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDOdometer.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

function TOBDOdometer.FormatVal(AValue: Double): string;
var FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  if FDecimals = 0 then
    Result := FormatFloat('0', AValue, FS)
  else
    Result := FormatFloat('0.' + StringOfChar('0', FDecimals),
      AValue, FS);
  if FUnit <> '' then Result := Result + ' ' + FUnit;
end;

procedure TOBDOdometer.FireTripReset(ATrip: Char);
begin
  if Assigned(FOnTripReset) then
    try
      FOnTripReset(Self, ATrip);
    except
    end;
end;

procedure TOBDOdometer.ResetTripA;
begin
  SetTripA(0);
  FireTripReset('A');
end;

procedure TOBDOdometer.ResetTripB;
begin
  SetTripB(0);
  FireTripReset('B');
end;

procedure TOBDOdometer.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  if not FShowReset then Exit;
  if PtInRect(FResetARect, Point(X, Y)) then
    ResetTripA
  else if PtInRect(FResetBRect, Point(X, Y)) then
    ResetTripB;
end;

procedure TOBDOdometer.PaintControl(ACanvas: TCanvas);
var
  Pad, RowH, Y: Integer;
  TotalStr: string;
  ButtonW: Integer;
  ResetCaption: string;

  procedure DrawResetButton(const ARect: TRect);
  var
    Graphics: TGPGraphics;
    Brush: TGPSolidBrush;
    Pen:   TGPPen;
    R: TGPRectF;
  begin
    R.X := ARect.Left;
    R.Y := ARect.Top;
    R.Width  := ARect.Width;
    R.Height := ARect.Height;
    Graphics := TGPGraphics.Create(ACanvas.Handle);
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    Pen   := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
      Graphics.FillRectangle(Brush, R);
      Graphics.DrawRectangle(Pen, R);
    finally
      Pen.Free;
      Brush.Free;
      Graphics.Free;
    end;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(
      ARect.Left + (ARect.Width  -
        ACanvas.TextWidth(ResetCaption)) div 2,
      ARect.Top  + (ARect.Height -
        ACanvas.TextHeight(ResetCaption)) div 2,
      ResetCaption);
  end;

  procedure DrawRow(ALabel: string; AValue: Double;
    AResetRect: PRect);
  var
    LabelW: Integer;
    ValueStr: string;
    ButtonRect: TRect;
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := EffectiveForeground;
    LabelW := ScaleValue(72);
    ACanvas.TextOut(Pad, Y + ScaleValue(2), ALabel);

    ACanvas.Font := FValueFont;
    ACanvas.Font.Color := EffectiveAccent;
    ValueStr := FormatVal(AValue);
    ACanvas.TextOut(Pad + LabelW, Y, ValueStr);

    if FShowReset and (AResetRect <> nil) then
    begin
      ButtonRect.Left   := Width - Pad - ButtonW;
      ButtonRect.Top    := Y + ScaleValue(2);
      ButtonRect.Right  := ButtonRect.Left + ButtonW;
      ButtonRect.Bottom := ButtonRect.Top + RowH - ScaleValue(4);
      DrawResetButton(ButtonRect);
      AResetRect^ := ButtonRect;
    end
    else if AResetRect <> nil then
      AResetRect^ := TRect.Empty;
  end;

begin
  Pad := ScaleValue(10);
  ACanvas.Font := FValueFont;
  RowH := ACanvas.TextHeight('Mg') + ScaleValue(8);
  if RowH < ScaleValue(28) then RowH := ScaleValue(28);
  ButtonW := ScaleValue(56);
  ResetCaption := 'reset';

  FResetARect := TRect.Empty;
  FResetBRect := TRect.Empty;

  // Total — integer-only, dashboard convention.
  Y := Pad;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(Pad, Y + ScaleValue(2), 'Total');
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveForeground;
  if FUnit <> '' then
    TotalStr := Format('%.0f %s', [FTotal, FUnit])
  else
    TotalStr := Format('%.0f', [FTotal]);
  ACanvas.TextOut(Pad + ScaleValue(72), Y, TotalStr);

  Y := Y + RowH;
  DrawRow('Trip A', FTripA, @FResetARect);

  Y := Y + RowH;
  DrawRow('Trip B', FTripB, @FResetBRect);
end;

{ ---- TOBDClock ----------------------------------------------------------- }

constructor TOBDClock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 140;
  Height := 140;
  FStyle       := csAnalog;
  FHourFormat  := h24;
  FShowSeconds := True;
  FNow         := Now;
  FDigitalFont := TFont.Create;
  FDigitalFont.Name := 'Consolas';
  FDigitalFont.Size := 22;
  FDigitalFont.Style := [fsBold];
  FDigitalFont.OnChange := HandleFontChange;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
  UpdateTimerInterval;
end;

destructor TOBDClock.Destroy;
begin
  FTimer.Free;
  FDigitalFont.Free;
  inherited;
end;

procedure TOBDClock.Loaded;
begin
  inherited;
  FTimer.Enabled := not (csDesigning in ComponentState);
end;

procedure TOBDClock.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDClock.UpdateTimerInterval;
begin
  if FShowSeconds then
    FTimer.Interval := 250
  else
    FTimer.Interval := 1000;
end;

procedure TOBDClock.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDClock.HandleTimer(Sender: TObject);
begin
  FNow := Now;
  NotifyBindings;
  Repaint;
end;

procedure TOBDClock.SetStyle(AValue: TOBDClockStyle);
begin
  if FStyle = AValue then Exit;
  FStyle := AValue; Repaint;
end;

procedure TOBDClock.SetHourFormat(AValue: TOBDClockHourFormat);
begin
  if FHourFormat = AValue then Exit;
  FHourFormat := AValue; Repaint;
end;

procedure TOBDClock.SetShowSeconds(AValue: Boolean);
begin
  if FShowSeconds = AValue then Exit;
  FShowSeconds := AValue;
  UpdateTimerInterval;
  Repaint;
end;

procedure TOBDClock.SetDigitalFont(AValue: TFont);
begin
  FDigitalFont.Assign(AValue);
end;

procedure TOBDClock.PaintControl(ACanvas: TCanvas);
begin
  if FStyle = csAnalog then PaintAnalog(ACanvas)
  else                      PaintDigital(ACanvas);
end;

procedure TOBDClock.PaintAnalog(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Face:  TGPRectF;
  Cx, Cy, R: Single;
  HH, MM, SS, MS: Word;
  HourAngle, MinAngle, SecAngle: Single;
  S: Integer;

  procedure DrawHand(AAngleRad: Single; ALength, AThick: Single;
    AColor: TColor);
  var P: TGPPen;
  begin
    P := TGPPen.Create(ColorToARGB(AColor), AThick);
    P.SetStartCap(LineCapRound);
    P.SetEndCap(LineCapRound);
    try
      Graphics.DrawLine(P,
        Cx, Cy,
        Cx + ALength * Sin(AAngleRad),
        Cy - ALength * Cos(AAngleRad));
    finally
      P.Free;
    end;
  end;

begin
  S := System.Math.Min(Width, Height);
  Cx := Width  / 2;
  Cy := Height / 2;
  R  := S / 2 - ScaleValue(4);

  Face.X := Cx - R;
  Face.Y := Cy - R;
  Face.Width  := R * 2;
  Face.Height := R * 2;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Face.
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.GaugeFace));
    try
      Graphics.FillEllipse(Brush, Face);
    finally
      Brush.Free;
    end;
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(2));
    try
      Graphics.DrawEllipse(Pen, Face);
    finally
      Pen.Free;
    end;

    // Hour ticks (12).
    Pen := TGPPen.Create(ColorToARGB(Palette.GaugeTick), ScaleValue(2));
    try
      var I: Integer;
      var A: Single;
      for I := 0 to 11 do
      begin
        A := I * 30 * Pi / 180;
        Graphics.DrawLine(Pen,
          Cx + (R - ScaleValue(8)) * Sin(A),
          Cy - (R - ScaleValue(8)) * Cos(A),
          Cx + (R - ScaleValue(2)) * Sin(A),
          Cy - (R - ScaleValue(2)) * Cos(A));
      end;
    finally
      Pen.Free;
    end;

    DecodeTime(FNow, HH, MM, SS, MS);
    HourAngle := ((HH mod 12) + MM / 60) * 30 * Pi / 180;
    MinAngle  := (MM + SS / 60) * 6 * Pi / 180;
    SecAngle  := SS * 6 * Pi / 180;

    DrawHand(HourAngle, R * 0.55, ScaleValue(4), Palette.GaugeNeedle);
    DrawHand(MinAngle,  R * 0.80, ScaleValue(3), Palette.GaugeNeedle);
    if FShowSeconds then
      DrawHand(SecAngle, R * 0.85, ScaleValue(1), Palette.Accent);

    // Hub.
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.GaugeNeedle));
    try
      Graphics.FillEllipse(Brush,
        Cx - ScaleValue(4), Cy - ScaleValue(4),
        ScaleValue(8), ScaleValue(8));
    finally
      Brush.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

procedure TOBDClock.PaintDigital(ACanvas: TCanvas);
var
  HH, MM, SS, MS: Word;
  Disp, Suffix: string;
  X, Y: Integer;
begin
  DecodeTime(FNow, HH, MM, SS, MS);
  Suffix := '';
  if FHourFormat = h12 then
  begin
    if HH < 12 then Suffix := ' am' else Suffix := ' pm';
    HH := HH mod 12;
    if HH = 0 then HH := 12;
  end;

  if FShowSeconds then
    Disp := Format('%2.2d:%2.2d:%2.2d', [HH, MM, SS])
  else
    Disp := Format('%2.2d:%2.2d', [HH, MM]);
  Disp := Disp + Suffix;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FDigitalFont;
  ACanvas.Font.Color := EffectiveAccent;
  X := (Width  - ACanvas.TextWidth(Disp))   div 2;
  Y := (Height - ACanvas.TextHeight(Disp)) div 2;
  ACanvas.TextOut(X, Y, Disp);
end;

end.
