//------------------------------------------------------------------------------
//  ERD.Components.RangeBar
//
//  TERDRangeBar - one value against its normal band: a caption on the
//  left, the value with its unit on the right (amber or red when the
//  value is outside the band), and under them a 6 px track with the
//  band in green and a marker at the value.
//
//  The band (Low .. High) is what a application adjusts in its range
//  profile; Min .. Max is the scale of the track. AlarmMargin decides
//  how far outside the band a warning becomes an alarm (see
//  ERDRangeLevel).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the Application controls.
//------------------------------------------------------------------------------

unit ERD.Components.RangeBar;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.Components.Types,
  ERD.Components.Control,
  ERD.Components.Paint;

type
  /// <summary>Value against its normal band.</summary>
  TERDRangeBar = class(TERDGraphicControl)
  strict private
    FValue: Double;
    FMin: Double;
    FMax: Double;
    FLow: Double;
    FHigh: Double;
    FAlarmMargin: Double;
    FDecimals: Integer;
    FUnitText: string;
    FShowCaption: Boolean;
    procedure SetValue(AValue: Double);
    procedure SetMin(AValue: Double);
    procedure SetMax(AValue: Double);
    procedure SetLow(AValue: Double);
    procedure SetHigh(AValue: Double);
    procedure SetAlarmMargin(AValue: Double);
    procedure SetDecimals(AValue: Integer);
    procedure SetUnitText(const AValue: string);
    procedure SetShowCaption(AValue: Boolean);
    function IsMinStored: Boolean;
    function IsMaxStored: Boolean;
    function IsLowStored: Boolean;
    function IsHighStored: Boolean;
    function IsAlarmMarginStored: Boolean;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates a 0..100 bar with band 0..100.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Sets scale and band in one call.</summary>
    /// <param name="AMin">Scale start.</param>
    /// <param name="AMax">Scale end.</param>
    /// <param name="ALow">Band start.</param>
    /// <param name="AHigh">Band end.</param>
    procedure SetRange(AMin, AMax, ALow, AHigh: Double);
    /// <summary>Alert level of the current value.</summary>
    /// <returns>Normal, warning or alarm.</returns>
    function Level: TERDAlertLevel;
    /// <summary>Value formatted with Decimals and the unit.</summary>
    /// <returns>Display text.</returns>
    function ValueText: string;
  published
    /// <summary>Parameter name on the left.</summary>
    property Caption;
    /// <summary>Current value.</summary>
    property Value: Double read FValue write SetValue;
    /// <summary>Scale start.</summary>
    property Min: Double read FMin write SetMin stored IsMinStored;
    /// <summary>Scale end.</summary>
    property Max: Double read FMax write SetMax stored IsMaxStored;
    /// <summary>Normal band start.</summary>
    property Low: Double read FLow write SetLow stored IsLowStored;
    /// <summary>Normal band end.</summary>
    property High: Double read FHigh write SetHigh stored IsHighStored;
    /// <summary>Distance outside the band, as a fraction of the band
    /// width, at which a warning becomes an alarm.</summary>
    property AlarmMargin: Double read FAlarmMargin write SetAlarmMargin
      stored IsAlarmMarginStored;
    /// <summary>Decimals of the value text.</summary>
    property Decimals: Integer read FDecimals write SetDecimals default 0;
    /// <summary>Unit after the value, e.g. "°C".</summary>
    property UnitText: string read FUnitText write SetUnitText;
    /// <summary>Shows the caption and value line above the track.
    /// </summary>
    property ShowCaption: Boolean read FShowCaption write SetShowCaption
      default True;
  end;

implementation

const
  DEFAULT_ALARM_MARGIN = 0.5;

{ TERDRangeBar }

constructor TERDRangeBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := 0;
  FMax := 100;
  FLow := 0;
  FHigh := 100;
  FAlarmMargin := DEFAULT_ALARM_MARGIN;
  FShowCaption := True;
  Width := 240;
  Height := 44;
end;

procedure TERDRangeBar.SetRange(AMin, AMax, ALow, AHigh: Double);
begin
  FMin := AMin;
  FMax := AMax;
  FLow := ALow;
  FHigh := AHigh;
  Invalidate;
end;

procedure TERDRangeBar.SetValue(AValue: Double);
begin
  if SameValue(FValue, AValue) then
    Exit;
  FValue := AValue;
  Invalidate;
end;

procedure TERDRangeBar.SetMin(AValue: Double);
begin
  if SameValue(FMin, AValue) then
    Exit;
  FMin := AValue;
  Invalidate;
end;

procedure TERDRangeBar.SetMax(AValue: Double);
begin
  if SameValue(FMax, AValue) then
    Exit;
  FMax := AValue;
  Invalidate;
end;

procedure TERDRangeBar.SetLow(AValue: Double);
begin
  if SameValue(FLow, AValue) then
    Exit;
  FLow := AValue;
  Invalidate;
end;

procedure TERDRangeBar.SetHigh(AValue: Double);
begin
  if SameValue(FHigh, AValue) then
    Exit;
  FHigh := AValue;
  Invalidate;
end;

procedure TERDRangeBar.SetAlarmMargin(AValue: Double);
begin
  if SameValue(FAlarmMargin, AValue) then
    Exit;
  FAlarmMargin := AValue;
  Invalidate;
end;

procedure TERDRangeBar.SetDecimals(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FDecimals = AValue then
    Exit;
  FDecimals := AValue;
  Invalidate;
end;

procedure TERDRangeBar.SetUnitText(const AValue: string);
begin
  if FUnitText = AValue then
    Exit;
  FUnitText := AValue;
  Invalidate;
end;

procedure TERDRangeBar.SetShowCaption(AValue: Boolean);
begin
  if FShowCaption = AValue then
    Exit;
  FShowCaption := AValue;
  Invalidate;
end;

function TERDRangeBar.IsMinStored: Boolean;
begin
  Result := not SameValue(FMin, 0);
end;

function TERDRangeBar.IsMaxStored: Boolean;
begin
  Result := not SameValue(FMax, 100);
end;

function TERDRangeBar.IsLowStored: Boolean;
begin
  Result := not SameValue(FLow, 0);
end;

function TERDRangeBar.IsHighStored: Boolean;
begin
  Result := not SameValue(FHigh, 100);
end;

function TERDRangeBar.IsAlarmMarginStored: Boolean;
begin
  Result := not SameValue(FAlarmMargin, DEFAULT_ALARM_MARGIN);
end;

procedure TERDRangeBar.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

function TERDRangeBar.Level: TERDAlertLevel;
begin
  Result := ERDRangeLevel(FValue, FLow, FHigh, FAlarmMargin);
end;

function TERDRangeBar.ValueText: string;
begin
  if FDecimals > 0 then
    Result := FormatFloat('0.' + StringOfChar('0', FDecimals), FValue)
  else
    Result := FormatFloat('0', FValue);
  if FUnitText <> '' then
    Result := Result + ' ' + FUnitText;
end;

procedure TERDRangeBar.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Lvl: TERDAlertLevel;
  Ink: TColor;
  Pad, BarY: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Lvl := Level;
    Pad := P.S(2);
    if FShowCaption then
    begin
      P.Text(0, P.S(8), Caption, 12.5, Palette.ForegroundText, twRegular,
        taLeftJustify, Width div 2);
      if Lvl = alvNormal then
        Ink := Palette.ForegroundText
      else
        Ink := P.LevelColor(Lvl);
      P.Text(Width, P.S(8), ValueText, 12.5, Ink, twSemibold,
        taRightJustify, Width div 2);
      BarY := P.S(30);
    end
    else
      BarY := Height div 2;
    P.RangeBar(Pad, BarY, Width - 2 * Pad, FMin, FMax, FLow, FHigh, FValue,
      Lvl);
  finally
    P.Free;
  end;
end;

end.
