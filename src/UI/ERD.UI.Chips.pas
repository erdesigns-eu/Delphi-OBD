//------------------------------------------------------------------------------
//  ERD.UI.Chips
//
//  Small status pieces for the OBD Studio controls.
//
//    TOBDChip    status pill: STORED, PENDING, PERMANENT, COMPLETE,
//                a DTC code in monospace, or a filled CONNECTED pill.
//    TOBDBadge   counter bubble, hidden at zero unless ShowZero.
//    TOBDBanner  callout with a status edge, an icon, a bold title and
//                one line of text (info, success, warning, danger).
//                Controls dropped on a banner (a Retry button) sit on
//                its tinted fill; the text stops short of them.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Chips;

interface

uses
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Status pill.</summary>
  TOBDChip = class(TOBDGraphicControl)
  strict private
    FKind: TOBDStatusKind;
    FFilled: Boolean;
    FMono: Boolean;
    procedure SetKind(AValue: TOBDStatusKind);
    procedure SetFilled(AValue: Boolean);
    procedure SetMono(AValue: Boolean);
    function ChipColor(APainter: TOBDPainter): TColor;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
  public
    /// <summary>Creates a neutral chip.</summary>
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Sizes the chip to its caption.</summary>
    property AutoSize default True;
    /// <summary>Label, normally upper case.</summary>
    property Caption;
    /// <summary>Status colour.</summary>
    property Kind: TOBDStatusKind read FKind write SetKind default skNeutral;
    /// <summary>Solid fill instead of a tint.</summary>
    property Filled: Boolean read FFilled write SetFilled default False;
    /// <summary>Monospace label, for DTC codes.</summary>
    property Mono: Boolean read FMono write SetMono default False;
  end;

  /// <summary>Counter bubble.</summary>
  TOBDBadge = class(TOBDGraphicControl)
  strict private
    FCount: Integer;
    FKind: TOBDStatusKind;
    FShowZero: Boolean;
    FMaxCount: Integer;
    procedure SetCount(AValue: Integer);
    procedure SetKind(AValue: TOBDStatusKind);
    procedure SetShowZero(AValue: Boolean);
    procedure SetMaxCount(AValue: Integer);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
  public
    /// <summary>Creates a red badge with count 0.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Text the badge shows: the count, or "99+" above
    /// MaxCount.</summary>
    /// <returns>Badge text.</returns>
    function DisplayText: string;
  published
    /// <summary>Sizes the badge to its text.</summary>
    property AutoSize default True;
    /// <summary>Number shown.</summary>
    property Count: Integer read FCount write SetCount default 0;
    /// <summary>Fill colour.</summary>
    property Kind: TOBDStatusKind read FKind write SetKind default skDanger;
    /// <summary>Draws the badge at count 0 too.</summary>
    property ShowZero: Boolean read FShowZero write SetShowZero
      default False;
    /// <summary>Counts above this show as MaxCount followed by "+".
    /// </summary>
    property MaxCount: Integer read FMaxCount write SetMaxCount default 99;
  end;

  /// <summary>Callout with a status edge, icon, title and text.
  /// </summary>
  TOBDBanner = class(TOBDCustomControl, IOBDSurface)
  strict private
    FKind: TOBDBannerKind;
    FTitle: string;
    FText: string;
    FAlertIcon: Boolean;
    procedure SetKind(AValue: TOBDBannerKind);
    procedure SetTitle(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetAlertIcon(AValue: Boolean);
    function TextRight: Integer;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure AdjustClientRect(var Rect: TRect); override;
  public
    /// <summary>Creates an info banner.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Re-sizes for the new density.</summary>
    procedure DensityChanged; override;
    /// <summary>The tinted banner fill; children paint their
    /// background in it.</summary>
    /// <returns>Fill colour.</returns>
    function SurfaceColor: TColor;
  published
    /// <summary>Height follows the density (56 px desktop, 68 px
    /// tablet).</summary>
    property AutoSize default True;
    /// <summary>Colour and icon.</summary>
    property Kind: TOBDBannerKind read FKind write SetKind default bnInfo;
    /// <summary>Bold first line.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Second line.</summary>
    property Text: string read FText write SetText;
    /// <summary>Draws the warning triangle instead of the kind's icon.
    /// </summary>
    property AlertIcon: Boolean read FAlertIcon write SetAlertIcon
      default False;
    /// <summary>Desktop or tablet height.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
  end;

implementation

{ TOBDChip }

constructor TOBDChip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKind := skNeutral;
  Width := 72;
  Height := 20;
  AutoSize := True;
end;

procedure TOBDChip.SetKind(AValue: TOBDStatusKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
end;

procedure TOBDChip.SetFilled(AValue: Boolean);
begin
  if FFilled = AValue then
    Exit;
  FFilled := AValue;
  Invalidate;
end;

procedure TOBDChip.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TOBDChip.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

function TOBDChip.ChipColor(APainter: TOBDPainter): TColor;
begin
  if FFilled and (FKind = skAccent) then
    Result := Palette.Accent
  else
    Result := APainter.StatusColor(FKind);
end;

function TOBDChip.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  Weight: TOBDTextWeight;
begin
  Result := True;
  if FMono then
    Weight := twMonoBold
  else
    Weight := twBold;
  NewWidth := OBDMeasureText(Caption, 10.5, Weight, ScaleValue(96)) +
    ScaleValue(16);
  NewHeight := ScaleValue(OBD_CHIP_HEIGHT);
end;

procedure TOBDChip.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Chip(0, (Height - P.S(OBD_CHIP_HEIGHT)) div 2, Caption, ChipColor(P),
      FFilled, FMono);
  finally
    P.Free;
  end;
end;

{ TOBDBadge }

constructor TOBDBadge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKind := skDanger;
  FMaxCount := 99;
  Width := 20;
  Height := 18;
  AutoSize := True;
end;

function TOBDBadge.DisplayText: string;
begin
  if (FMaxCount > 0) and (FCount > FMaxCount) then
    Result := IntToStr(FMaxCount) + '+'
  else
    Result := IntToStr(FCount);
end;

procedure TOBDBadge.SetCount(AValue: Integer);
begin
  if FCount = AValue then
    Exit;
  FCount := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TOBDBadge.SetKind(AValue: TOBDStatusKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
end;

procedure TOBDBadge.SetShowZero(AValue: Boolean);
begin
  if FShowZero = AValue then
    Exit;
  FShowZero := AValue;
  Invalidate;
end;

procedure TOBDBadge.SetMaxCount(AValue: Integer);
begin
  if FMaxCount = AValue then
    Exit;
  FMaxCount := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

function TOBDBadge.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewWidth := Max(ScaleValue(20),
    OBDMeasureText(DisplayText, 10.5, twBold, ScaleValue(96)) +
    ScaleValue(12));
  NewHeight := ScaleValue(18);
end;

procedure TOBDBadge.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Fill: TColor;
begin
  if (FCount = 0) and not FShowZero and
    not (csDesigning in ComponentState) then
    Exit;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if FKind = skAccent then
      Fill := Palette.Accent
    else
      Fill := P.StatusColor(FKind);
    P.Badge(Width - (Width - P.BadgeWidth(DisplayText)) div 2,
      (Height - P.S(18)) div 2, DisplayText, Fill);
  finally
    P.Free;
  end;
end;

{ TOBDBanner }

constructor TOBDBanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FKind := bnInfo;
  Width := 420;
  Height := 56;
  AutoSize := True;
end;

procedure TOBDBanner.SetKind(AValue: TOBDBannerKind);
var
  I: Integer;
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
  for I := 0 to ControlCount - 1 do
    Controls[I].Invalidate;
end;

procedure TOBDBanner.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDBanner.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  Invalidate;
end;

procedure TOBDBanner.SetAlertIcon(AValue: Boolean);
begin
  if FAlertIcon = AValue then
    Exit;
  FAlertIcon := AValue;
  Invalidate;
end;

procedure TOBDBanner.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

function TOBDBanner.SurfaceColor: TColor;
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(nil, Palette, ScaleValue(96));
  try
    Result := P.BannerFill(FKind);
  finally
    P.Free;
  end;
end;

function TOBDBanner.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := ScaleValue(Max(56, Metrics.Head));
end;

procedure TOBDBanner.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Inc(Rect.Left, ScaleValue(OBD_EDGE));
end;

function TOBDBanner.TextRight: Integer;
var
  I: Integer;
  Child: TControl;
begin
  Result := Width - ScaleValue(12);
  for I := 0 to ControlCount - 1 do
  begin
    Child := Controls[I];
    if Child.Visible and (Child.Left > ScaleValue(44)) then
      Result := Min(Result, Child.Left - ScaleValue(8));
  end;
end;

procedure TOBDBanner.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  CY, X, MaxW: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.BannerFrame(Rect(0, 0, Width, Height), FKind);
    CY := Height div 2;
    P.BannerIcon(P.S(24), CY, FKind, FAlertIcon);
    X := P.S(44);
    MaxW := TextRight - X;
    if MaxW <= 0 then
      Exit;
    if FText = '' then
      P.Text(X, CY, FTitle, 13.5, Palette.ForegroundText, twBold,
        taLeftJustify, MaxW)
    else if FTitle = '' then
      P.Text(X, CY, FText, 12, Palette.ForegroundText, twRegular,
        taLeftJustify, MaxW)
    else
    begin
      P.Text(X, CY - P.S(9), FTitle, 13.5, Palette.ForegroundText, twBold,
        taLeftJustify, MaxW);
      P.Text(X, CY + P.S(10), FText, 12, Palette.ForegroundText, twRegular,
        taLeftJustify, MaxW);
    end;
  finally
    P.Free;
  end;
end;

end.
