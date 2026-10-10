//------------------------------------------------------------------------------
//  ERD.UI.VehicleCard
//
//  TOBDVehicleInfoCard - vehicle identification card for OBD Studio. The card
//  paints the full and compact vehicle-summary layouts from the approved
//  mockups, validates the VIN check digit, and exposes simple host-driven
//  properties for scan metadata, odometer, protocol and calibration ID.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.VehicleCard;

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.Protocol.VIN,
  ERD.Service.VINDecoder,
  ERD.Service.VINDecoder.Types,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Vehicle card layout.</summary>
  TOBDVehicleCardLayout = (
    /// <summary>Full vehicle-information card.</summary>
    vlFull,
    /// <summary>Compact vehicle strip for page headers.</summary>
    vlCompact);

  /// <summary>Vehicle identification and connection summary card.</summary>
  TOBDVehicleInfoCard = class(TOBDCustomControl, IOBDSurface)
  strict private
    FLayout: TOBDVehicleCardLayout;
    FVIN: string;
    FMake: string;
    FModel: string;
    FModelYear: Integer;
    FEngine: string;
    FBodyStyle: string;
    FFuelType: string;
    FEngineCode: string;
    FEnginePower: string;
    FProtocol: string;
    FProtocolCompact: string;
    FEcuCount: Integer;
    FOdometer: string;
    FCalibrationID: string;
    FMilOn: Boolean;
    FConnected: Boolean;
    FScanTimeText: string;
    FCopyCaption: string;
    FActionCaption: string;
    FHasData: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetLayout(AValue: TOBDVehicleCardLayout);
    procedure SetVIN(const AValue: string);
    procedure SetMake(const AValue: string);
    procedure SetModel(const AValue: string);
    procedure SetModelYear(AValue: Integer);
    procedure SetEngine(const AValue: string);
    procedure SetBodyStyle(const AValue: string);
    procedure SetFuelType(const AValue: string);
    procedure SetEngineCode(const AValue: string);
    procedure SetEnginePower(const AValue: string);
    procedure SetProtocol(const AValue: string);
    procedure SetProtocolCompact(const AValue: string);
    procedure SetEcuCount(AValue: Integer);
    procedure SetOdometer(const AValue: string);
    procedure SetCalibrationID(const AValue: string);
    procedure SetMilOn(AValue: Boolean);
    procedure SetConnected(AValue: Boolean);
    procedure SetScanTimeText(const AValue: string);
    procedure SetCopyCaption(const AValue: string);
    procedure SetActionCaption(const AValue: string);
    procedure DoChange;
    function UsePreviewData: Boolean;
    function VisualVIN: string;
    function VisualMake: string;
    function VisualModel: string;
    function VisualModelYear: Integer;
    function VisualEngine: string;
    function VisualBodyStyle: string;
    function VisualFuelType: string;
    function VisualEngineCode: string;
    function VisualEnginePower: string;
    function VisualProtocol: string;
    function VisualProtocolCompact: string;
    function VisualEcuCount: Integer;
    function VisualOdometer: string;
    function VisualCalibrationID: string;
    function VisualMilOn: Boolean;
    function VisualConnected: Boolean;
    function VisualScanTimeText: string;
    function VehicleTitle: string;
    function VehicleSubtitle: string;
    function CheckDigitValid: Boolean;
    procedure PaintVIN(var P: TOBDPainter; X, Y, W: Integer);
    procedure PaintFull(var P: TOBDPainter);
    procedure PaintCompact(var P: TOBDPainter);
  protected
    /// <summary>Paints the vehicle card.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates an empty vehicle card.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Returns the card face used by child controls.</summary>
    function SurfaceColor: TColor;
    /// <summary>Clears vehicle data so preview data can show at design time.</summary>
    procedure Clear;
    /// <summary>Loads VIN and best-effort decoded year, make and engine fields.</summary>
    procedure LoadFromVIN(const AVIN: string);
    /// <summary>True when at least one real data property has been assigned.</summary>
    property HasData: Boolean read FHasData;
    /// <summary>True when <see cref="VIN"/> has a valid ISO 3779 check digit.</summary>
    property VINCheckDigitValid: Boolean read CheckDigitValid;
  published
    /// <summary>Full vehicle card or compact vehicle strip.</summary>
    property Layout: TOBDVehicleCardLayout read FLayout write SetLayout
      default vlFull;
    /// <summary>Vehicle identification number.</summary>
    property VIN: string read FVIN write SetVIN;
    /// <summary>Vehicle make.</summary>
    property Make: string read FMake write SetMake;
    /// <summary>Vehicle model.</summary>
    property Model: string read FModel write SetModel;
    /// <summary>Model year shown in the subtitle.</summary>
    property ModelYear: Integer read FModelYear write SetModelYear default 0;
    /// <summary>Engine marketing name shown in the title.</summary>
    property Engine: string read FEngine write SetEngine;
    /// <summary>Body style shown in the subtitle.</summary>
    property BodyStyle: string read FBodyStyle write SetBodyStyle;
    /// <summary>Fuel type shown in the subtitle.</summary>
    property FuelType: string read FFuelType write SetFuelType;
    /// <summary>Engine code shown in the subtitle.</summary>
    property EngineCode: string read FEngineCode write SetEngineCode;
    /// <summary>Engine power shown in the subtitle.</summary>
    property EnginePower: string read FEnginePower write SetEnginePower;
    /// <summary>Protocol text shown on the full card.</summary>
    property Protocol: string read FProtocol write SetProtocol;
    /// <summary>Short protocol text shown on the compact strip.</summary>
    property ProtocolCompact: string read FProtocolCompact
      write SetProtocolCompact;
    /// <summary>Number of responding control units.</summary>
    property EcuCount: Integer read FEcuCount write SetEcuCount default 0;
    /// <summary>Odometer text.</summary>
    property Odometer: string read FOdometer write SetOdometer;
    /// <summary>Calibration ID shown in the facts row.</summary>
    property CalibrationID: string read FCalibrationID write SetCalibrationID;
    /// <summary>True when the MIL is on.</summary>
    property MilOn: Boolean read FMilOn write SetMilOn default False;
    /// <summary>True when the vehicle is connected.</summary>
    property Connected: Boolean read FConnected write SetConnected default False;
    /// <summary>Scan timestamp text shown on the compact strip.</summary>
    property ScanTimeText: string read FScanTimeText write SetScanTimeText;
    /// <summary>Full-card VIN action caption.</summary>
    property CopyCaption: string read FCopyCaption write SetCopyCaption;
    /// <summary>Compact-card action caption.</summary>
    property ActionCaption: string read FActionCaption write SetActionCaption;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when vehicle data changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

function JoinParts(const A, B: string): string;
begin
  if A = '' then
    Result := B
  else if B = '' then
    Result := A
  else
    Result := A + ' ' + B;
end;

procedure AddSubtitlePart(var AText: string; const APart: string);
begin
  if APart = '' then
    Exit;
  if AText <> '' then
    AText := AText + '  ·  ';
  AText := AText + APart;
end;

{ TOBDVehicleInfoCard }

constructor TOBDVehicleInfoCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FLayout := vlFull;
  FCopyCaption := 'Copy VIN';
  FActionCaption := 'Change vehicle';
  Width := 720;
  Height := 220;
end;

function TOBDVehicleInfoCard.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TOBDVehicleInfoCard.DoChange;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDVehicleInfoCard.Clear;
begin
  FVIN := '';
  FMake := '';
  FModel := '';
  FModelYear := 0;
  FEngine := '';
  FBodyStyle := '';
  FFuelType := '';
  FEngineCode := '';
  FEnginePower := '';
  FProtocol := '';
  FProtocolCompact := '';
  FEcuCount := 0;
  FOdometer := '';
  FCalibrationID := '';
  FMilOn := False;
  FConnected := False;
  FScanTimeText := '';
  FHasData := False;
  DoChange;
end;

procedure TOBDVehicleInfoCard.LoadFromVIN(const AVIN: string);
var
  Info: TOBDVINInfo;
  EngineText: string;
begin
  SetVIN(AVIN);
  try
    Info := TOBDVINDecoder.Decode(FVIN);
  except
    Exit;
  end;
  if not Info.Valid then
    Exit;
  if Info.Manufacturer.Name <> '' then
    FMake := Info.Manufacturer.Name;
  if Info.ModelYear <> 0 then
    FModelYear := Info.ModelYear;
  if Info.Features.BodyStyle <> '' then
    FBodyStyle := Info.Features.BodyStyle;
  EngineText := JoinParts(Info.Features.EngineDisplacement,
    Info.Features.EngineType);
  if EngineText <> '' then
    FEngine := EngineText;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetLayout(AValue: TOBDVehicleCardLayout);
begin
  if FLayout = AValue then
    Exit;
  FLayout := AValue;
  Invalidate;
end;

procedure TOBDVehicleInfoCard.SetVIN(const AValue: string);
var
  Normalized: string;
begin
  Normalized := TOBDVINValidator.Normalize(AValue);
  if FVIN = Normalized then
    Exit;
  FVIN := Normalized;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetMake(const AValue: string);
begin
  if FMake = AValue then
    Exit;
  FMake := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetModel(const AValue: string);
begin
  if FModel = AValue then
    Exit;
  FModel := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetModelYear(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FModelYear = AValue then
    Exit;
  FModelYear := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetEngine(const AValue: string);
begin
  if FEngine = AValue then
    Exit;
  FEngine := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetBodyStyle(const AValue: string);
begin
  if FBodyStyle = AValue then
    Exit;
  FBodyStyle := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetFuelType(const AValue: string);
begin
  if FFuelType = AValue then
    Exit;
  FFuelType := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetEngineCode(const AValue: string);
begin
  if FEngineCode = AValue then
    Exit;
  FEngineCode := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetEnginePower(const AValue: string);
begin
  if FEnginePower = AValue then
    Exit;
  FEnginePower := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetProtocol(const AValue: string);
begin
  if FProtocol = AValue then
    Exit;
  FProtocol := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetProtocolCompact(const AValue: string);
begin
  if FProtocolCompact = AValue then
    Exit;
  FProtocolCompact := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetEcuCount(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FEcuCount = AValue then
    Exit;
  FEcuCount := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetOdometer(const AValue: string);
begin
  if FOdometer = AValue then
    Exit;
  FOdometer := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetCalibrationID(const AValue: string);
begin
  if FCalibrationID = AValue then
    Exit;
  FCalibrationID := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetMilOn(AValue: Boolean);
begin
  if FMilOn = AValue then
    Exit;
  FMilOn := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetConnected(AValue: Boolean);
begin
  if FConnected = AValue then
    Exit;
  FConnected := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDVehicleInfoCard.SetScanTimeText(const AValue: string);
begin
  if FScanTimeText = AValue then
    Exit;
  FScanTimeText := AValue;
  Invalidate;
end;

procedure TOBDVehicleInfoCard.SetCopyCaption(const AValue: string);
begin
  if FCopyCaption = AValue then
    Exit;
  FCopyCaption := AValue;
  Invalidate;
end;

procedure TOBDVehicleInfoCard.SetActionCaption(const AValue: string);
begin
  if FActionCaption = AValue then
    Exit;
  FActionCaption := AValue;
  Invalidate;
end;

function TOBDVehicleInfoCard.UsePreviewData: Boolean;
begin
  Result := IsPreview and not FHasData;
end;

function TOBDVehicleInfoCard.VisualVIN: string;
begin
  if UsePreviewData then
    Result := 'WVWZZZAU6GW123456'
  else
    Result := FVIN;
end;

function TOBDVehicleInfoCard.VisualMake: string;
begin
  if UsePreviewData then
    Result := 'Volkswagen'
  else
    Result := FMake;
end;

function TOBDVehicleInfoCard.VisualModel: string;
begin
  if UsePreviewData then
    Result := 'Golf VII'
  else
    Result := FModel;
end;

function TOBDVehicleInfoCard.VisualModelYear: Integer;
begin
  if UsePreviewData then
    Result := 2016
  else
    Result := FModelYear;
end;

function TOBDVehicleInfoCard.VisualEngine: string;
begin
  if UsePreviewData then
    Result := '1.6 TDI'
  else
    Result := FEngine;
end;

function TOBDVehicleInfoCard.VisualBodyStyle: string;
begin
  if UsePreviewData then
    Result := 'Hatchback'
  else
    Result := FBodyStyle;
end;

function TOBDVehicleInfoCard.VisualFuelType: string;
begin
  if UsePreviewData then
    Result := 'Diesel'
  else
    Result := FFuelType;
end;

function TOBDVehicleInfoCard.VisualEngineCode: string;
begin
  if UsePreviewData then
    Result := 'CLHA'
  else
    Result := FEngineCode;
end;

function TOBDVehicleInfoCard.VisualEnginePower: string;
begin
  if UsePreviewData then
    Result := '81 kW (110 PS)'
  else
    Result := FEnginePower;
end;

function TOBDVehicleInfoCard.VisualProtocol: string;
begin
  if UsePreviewData then
    Result := 'ISO 15765-4 CAN · 11 bit · 500 kbit/s'
  else
    Result := FProtocol;
end;

function TOBDVehicleInfoCard.VisualProtocolCompact: string;
begin
  if UsePreviewData then
    Result := 'CAN 11/500'
  else if FProtocolCompact <> '' then
    Result := FProtocolCompact
  else
    Result := FProtocol;
end;

function TOBDVehicleInfoCard.VisualEcuCount: Integer;
begin
  if UsePreviewData then
    Result := 3
  else
    Result := FEcuCount;
end;

function TOBDVehicleInfoCard.VisualOdometer: string;
begin
  if UsePreviewData then
    Result := '148 312 km'
  else
    Result := FOdometer;
end;

function TOBDVehicleInfoCard.VisualCalibrationID: string;
begin
  if UsePreviewData then
    Result := '04L906056HT'
  else
    Result := FCalibrationID;
end;

function TOBDVehicleInfoCard.VisualMilOn: Boolean;
begin
  if UsePreviewData then
    Result := True
  else
    Result := FMilOn;
end;

function TOBDVehicleInfoCard.VisualConnected: Boolean;
begin
  if UsePreviewData then
    Result := True
  else
    Result := FConnected;
end;

function TOBDVehicleInfoCard.VisualScanTimeText: string;
begin
  if UsePreviewData and (FScanTimeText = '') then
    Result := 'Scanned 18:42'
  else
    Result := FScanTimeText;
end;

function TOBDVehicleInfoCard.VehicleTitle: string;
begin
  Result := JoinParts(VisualMake, VisualModel);
  Result := JoinParts(Result, VisualEngine);
  if Result = '' then
    Result := 'Vehicle';
end;

function TOBDVehicleInfoCard.VehicleSubtitle: string;
begin
  Result := '';
  if VisualModelYear > 0 then
    AddSubtitlePart(Result, IntToStr(VisualModelYear));
  AddSubtitlePart(Result, VisualBodyStyle);
  AddSubtitlePart(Result, VisualFuelType);
  AddSubtitlePart(Result, VisualEngineCode);
  AddSubtitlePart(Result, VisualEnginePower);
end;

function TOBDVehicleInfoCard.CheckDigitValid: Boolean;
begin
  Result := TOBDVINValidator.IsValid(VisualVIN);
end;

procedure TOBDVehicleInfoCard.PaintVIN(var P: TOBDPainter; X, Y, W: Integer);
var
  VINText, Part, Caption: string;
  VX, PartW: Integer;
  Valid: Boolean;
  VerdictColor: TColor;
begin
  VINText := VisualVIN;
  VX := X;
  if Length(VINText) >= 17 then
  begin
    Part := Copy(VINText, 1, 3);
    PartW := P.TextWidth(Part, 17, twMonoBold);
    P.Text(VX, Y, Part, 17, Palette.ForegroundText, twMonoBold);
    P.HLine(VX, Y + P.S(13), PartW, Palette.NeutralLight);
    P.Text(VX + PartW div 2, Y + P.S(22), 'WMI', 9.5, Palette.GaugeLabel,
      twSemibold, taCenter);
    Inc(VX, PartW + P.S(8));

    Part := Copy(VINText, 4, 6);
    PartW := P.TextWidth(Part, 17, twMonoBold);
    P.Text(VX, Y, Part, 17, Palette.ForegroundText, twMonoBold);
    P.HLine(VX, Y + P.S(13), PartW, Palette.NeutralLight);
    P.Text(VX + PartW div 2, Y + P.S(22), 'VDS', 9.5, Palette.GaugeLabel,
      twSemibold, taCenter);
    Inc(VX, PartW + P.S(8));

    Part := Copy(VINText, 10, 8);
    PartW := P.TextWidth(Part, 17, twMonoBold);
    P.Text(VX, Y, Part, 17, Palette.ForegroundText, twMonoBold);
    P.HLine(VX, Y + P.S(13), PartW, Palette.NeutralLight);
    P.Text(VX + PartW div 2, Y + P.S(22), 'VIS', 9.5, Palette.GaugeLabel,
      twSemibold, taCenter);
    Inc(VX, PartW + P.S(8));
  end
  else
  begin
    if VINText = '' then
      VINText := '—';
    VX := X + P.Text(X, Y, VINText, 17, Palette.ForegroundText, twMonoBold,
      taLeftJustify, W div 2) + P.S(8);
  end;

  Valid := CheckDigitValid;
  if Valid then
  begin
    VerdictColor := Palette.Success;
    Caption := 'Check digit valid';
    P.GlyphCheck(VX + P.S(8), Y, VerdictColor, 0.85);
  end
  else
  begin
    VerdictColor := Palette.Danger;
    Caption := 'Check digit invalid';
    P.GlyphAlert(VX + P.S(8), Y, VerdictColor);
  end;
  P.Text(VX + P.S(18), Y, Caption, 11.5, VerdictColor, twSemibold,
    taLeftJustify, W - (VX - X) - P.S(18));
end;

procedure TOBDVehicleInfoCard.PaintFull(var P: TOBDPainter);
var
  L, CX, VY, FY, FactW, I, FX, ChipW: Integer;
  FactsK: array [0 .. 3] of string;
  FactsV: array [0 .. 3] of string;
  FactsC: array [0 .. 3] of TColor;
  Mono: Boolean;
begin
  L := P.S(20);
  P.Caps(L, P.S(20), 'Vehicle');
  if VisualConnected then
  begin
    ChipW := P.ChipWidth('CONNECTED');
    CX := Width - P.S(16) - ChipW;
    P.Chip(CX, P.S(10), 'CONNECTED', Palette.Success);
  end
  else
  begin
    ChipW := P.ChipWidth('OFFLINE');
    CX := Width - P.S(16) - ChipW;
    P.Chip(CX, P.S(10), 'OFFLINE', Palette.Subtle);
  end;
  P.Text(CX - P.S(10), P.S(20), VisualProtocol, 11.5, Palette.GaugeLabel,
    twRegular, taRightJustify, CX - P.S(26));

  P.Text(L, P.S(50), VehicleTitle, 20, Palette.ForegroundText, twBold,
    taLeftJustify, Width - P.S(40));
  P.Text(L, P.S(75), VehicleSubtitle, 12.5, Palette.GaugeLabel,
    twRegular, taLeftJustify, Width - P.S(40));

  VY := P.S(104);
  PaintVIN(P, L, VY, Width - P.S(40));
  if FCopyCaption <> '' then
    P.Text(Width - P.S(16), VY, FCopyCaption, 11.5, P.AccentText,
      twSemibold, taRightJustify);

  FY := Height - P.S(52);
  P.HLine(P.S(4), FY - P.S(8), Width - P.S(4), Palette.NeutralLight);
  FactsK[0] := 'Odometer';
  FactsV[0] := VisualOdometer;
  FactsC[0] := Palette.ForegroundText;
  FactsK[1] := 'Control units';
  FactsV[1] := Format('%d responding', [VisualEcuCount]);
  FactsC[1] := Palette.ForegroundText;
  FactsK[2] := 'MIL';
  if VisualMilOn then
  begin
    FactsV[2] := 'On';
    FactsC[2] := Palette.Danger;
  end
  else
  begin
    FactsV[2] := 'Off';
    FactsC[2] := Palette.Success;
  end;
  FactsK[3] := 'Calibration ID';
  FactsV[3] := VisualCalibrationID;
  if FactsV[3] = '' then
    FactsV[3] := '—';
  FactsC[3] := Palette.ForegroundText;

  FactW := (Width - P.S(36)) div 4;
  for I := 0 to 3 do
  begin
    FX := L + I * FactW;
    if I > 0 then
      P.VLine(FX - P.S(10), FY, P.S(38), Palette.NeutralLight);
    P.Caps(FX, FY + P.S(8), FactsK[I]);
    Mono := FactsK[I] = 'Calibration ID';
    if Mono then
      P.Text(FX, FY + P.S(28), FactsV[I], 13, FactsC[I], twMonoBold,
        taLeftJustify, FactW - P.S(16))
    else
      P.Text(FX, FY + P.S(28), FactsV[I], 13, FactsC[I], twSemibold,
        taLeftJustify, FactW - P.S(16));
  end;
end;

procedure TOBDVehicleInfoCard.PaintCompact(var P: TOBDPainter);
var
  L, FX, I, VW, LabelW: Integer;
  FactsK: array [0 .. 3] of string;
  FactsV: array [0 .. 3] of string;
  FactsC: array [0 .. 3] of TColor;
  Weights: array [0 .. 3] of TOBDTextWeight;
  ScanText: string;
begin
  L := P.S(20);
  P.Caps(L, P.S(22), 'Vehicle');
  P.Text(L, P.S(50), VehicleTitle, 18, Palette.ForegroundText, twBold,
    taLeftJustify, P.S(290));
  P.Text(L, P.S(72), VehicleSubtitle, 12, Palette.GaugeLabel, twRegular,
    taLeftJustify, P.S(290));

  FactsK[0] := 'VIN';
  FactsV[0] := VisualVIN;
  FactsC[0] := Palette.ForegroundText;
  Weights[0] := twMonoBold;
  FactsK[1] := 'Odometer';
  FactsV[1] := VisualOdometer;
  FactsC[1] := Palette.ForegroundText;
  Weights[1] := twSemibold;
  FactsK[2] := 'Protocol';
  FactsV[2] := VisualProtocolCompact;
  FactsC[2] := Palette.ForegroundText;
  Weights[2] := twSemibold;
  FactsK[3] := 'MIL';
  if VisualMilOn then
  begin
    FactsV[3] := 'On';
    FactsC[3] := Palette.Danger;
  end
  else
  begin
    FactsV[3] := 'Off';
    FactsC[3] := Palette.Success;
  end;
  Weights[3] := twSemibold;

  FX := P.S(330);
  for I := 0 to 3 do
  begin
    P.VLine(FX - P.S(18), P.S(18), Height - P.S(36), Palette.NeutralLight);
    P.Caps(FX, P.S(34), FactsK[I]);
    VW := P.Text(FX, P.S(58), FactsV[I], 14, FactsC[I], Weights[I]);
    LabelW := P.TextWidth(AnsiUpperCase(FactsK[I]), 10.5, twSemibold);
    if VW < LabelW then
      VW := LabelW;
    Inc(FX, VW + P.S(40));
  end;

  if FActionCaption <> '' then
    P.Text(Width - P.S(16), P.S(34), FActionCaption, 11.5, P.AccentText,
      twSemibold, taRightJustify);
  ScanText := VisualScanTimeText;
  if ScanText <> '' then
    P.Text(Width - P.S(16), P.S(58), ScanText, 11.5, Palette.GaugeLabel,
      twRegular, taRightJustify);
end;

procedure TOBDVehicleInfoCard.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), Palette.Accent, SurfaceColor);
    if FLayout = vlCompact then
      PaintCompact(P)
    else
      PaintFull(P);
  finally
    P.Free;
  end;
end;

end.
