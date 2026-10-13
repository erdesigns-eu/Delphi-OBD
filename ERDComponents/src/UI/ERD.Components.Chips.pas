//------------------------------------------------------------------------------
//  ERD.Components.Chips
//
//  Small status pieces for the Application controls.
//
//    TERDChip    status pill: STORED, PENDING, PERMANENT, COMPLETE,
//                a DTC code in monospace, or a filled CONNECTED pill.
//    TERDBadge   counter bubble, hidden at zero unless ShowZero.
//    TERDBanner  callout with a status edge, an icon, a bold title and
//                one line of text (info, success, warning, danger).
//                Controls dropped on a banner (a Retry button) sit on
//                its tinted fill; the text stops short of them.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the Application controls.
//------------------------------------------------------------------------------

unit ERD.Components.Chips;

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
  ERD.Components.Control,
  ERD.Components.Types, ERD.Components.Paint, ERD.Components.Buttons;

type
  /// <summary>Status pill.</summary>
  TERDChip = class(TERDGraphicControl)
  strict private
    FKind: TERDStatusKind;
    FFilled: Boolean;
    FMono: Boolean;
    procedure SetKind(AValue: TERDStatusKind);
    procedure SetFilled(AValue: Boolean);
    procedure SetMono(AValue: Boolean);
    function ChipColor(APainter: TERDPainter): TColor;
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
    property Kind: TERDStatusKind read FKind write SetKind default skNeutral;
    /// <summary>Solid fill instead of a tint.</summary>
    property Filled: Boolean read FFilled write SetFilled default False;
    /// <summary>Monospace label, for DTC files.</summary>
    property Mono: Boolean read FMono write SetMono default False;
  end;

  /// <summary>Counter bubble.</summary>
  TERDBadge = class(TERDGraphicControl)
  strict private
    FCount: Integer;
    FKind: TERDStatusKind;
    FShowZero: Boolean;
    FMaxCount: Integer;
    procedure SetCount(AValue: Integer);
    procedure SetKind(AValue: TERDStatusKind);
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
    property Kind: TERDStatusKind read FKind write SetKind default skDanger;
    /// <summary>Draws the badge at count 0 too.</summary>
    property ShowZero: Boolean read FShowZero write SetShowZero
      default False;
    /// <summary>Counts above this show as MaxCount followed by "+".
    /// </summary>
    property MaxCount: Integer read FMaxCount write SetMaxCount default 99;
  end;

  /// <summary>Callout with a status edge, icon, title and text.
  /// </summary>
  TERDBanner = class(TERDCustomControl, IERDSurface)
  strict private
    FCloseButton: TERDButton;
    FActionButton: TERDButton;
    FShowClose: Boolean;
    FActionCaption: string;
    FOnClose: TNotifyEvent;
    FOnAction: TNotifyEvent;
    FKind: TERDBannerKind;
    FTitle: string;
    FText: string;
    FAlertIcon: Boolean;
    FIconSize: Integer;
    procedure SetShowClose(Value: Boolean);
    procedure SetActionCaption(const Value: string);
    procedure CloseClick(Sender: TObject);
    procedure ActionClick(Sender: TObject);
    procedure LayoutActions;
    procedure SetKind(AValue: TERDBannerKind);
    procedure SetTitle(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetAlertIcon(AValue: Boolean);
    procedure SetIconSize(AValue: Integer);
    function IconCenterX: Integer;
    function TextLeft: Integer;
    function TextRight: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure AdjustClientRect(var Rect: TRect); override;
  public
    /// <summary>Creates an info banner.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Re-sizes for the new density.</summary>
    procedure DensityChanged; override;
    procedure ThemeChanged; override;
    /// <summary>The tinted banner fill; children paint their
    /// background in it.</summary>
    /// <returns>Fill colour.</returns>
    function SurfaceColor: TColor;
  published
    /// <summary>Height follows the density (56 px desktop, 68 px
    /// tablet).</summary>
    property ShowClose: Boolean read FShowClose write SetShowClose default False;
    property ActionCaption: string read FActionCaption write SetActionCaption;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnAction: TNotifyEvent read FOnAction write FOnAction;
    property AutoSize default True;
    /// <summary>Colour and icon.</summary>
    property Kind: TERDBannerKind read FKind write SetKind default bnInfo;
    /// <summary>Bold first line.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Second line.</summary>
    property Text: string read FText write SetText;
    /// <summary>Draws the warning triangle instead of the kind's icon.
    /// </summary>
    property AlertIcon: Boolean read FAlertIcon write SetAlertIcon
      default False;
    /// <summary>Icon size in pixels at 96 DPI (12 to 64). The text
    /// moves right and an auto-sized banner grows to fit the
    /// icon.</summary>
    property IconSize: Integer read FIconSize write SetIconSize default 18;
    /// <summary>Desktop or tablet height.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
  end;

implementation

{ TERDChip }

constructor TERDChip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKind := skNeutral;
  Width := 72;
  Height := 20;
  AutoSize := True;
end;

procedure TERDChip.SetKind(AValue: TERDStatusKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
end;

procedure TERDChip.SetFilled(AValue: Boolean);
begin
  if FFilled = AValue then
    Exit;
  FFilled := AValue;
  Invalidate;
end;

procedure TERDChip.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TERDChip.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

function TERDChip.ChipColor(APainter: TERDPainter): TColor;
begin
  if FFilled and (FKind = skAccent) then
    Result := Palette.Accent
  else
    Result := APainter.StatusColor(FKind);
end;

function TERDChip.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  Weight: TERDTextWeight;
begin
  Result := True;
  if FMono then
    Weight := twMonoBold
  else
    Weight := twBold;
  NewWidth := ERDMeasureText(Caption, 10.5, Weight, ScaleValue(96)) +
    ScaleValue(16);
  NewHeight := ScaleValue(ERD_CHIP_HEIGHT);
end;

procedure TERDChip.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Chip(0, (Height - P.S(ERD_CHIP_HEIGHT)) div 2, Caption, ChipColor(P),
      FFilled, FMono);
  finally
    P.Free;
  end;
end;

{ TERDBadge }

constructor TERDBadge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKind := skDanger;
  FMaxCount := 99;
  Width := 20;
  Height := 18;
  AutoSize := True;
end;

function TERDBadge.DisplayText: string;
begin
  if (FMaxCount > 0) and (FCount > FMaxCount) then
    Result := IntToStr(FMaxCount) + '+'
  else
    Result := IntToStr(FCount);
end;

procedure TERDBadge.SetCount(AValue: Integer);
begin
  if FCount = AValue then
    Exit;
  FCount := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TERDBadge.SetKind(AValue: TERDStatusKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
end;

procedure TERDBadge.SetShowZero(AValue: Boolean);
begin
  if FShowZero = AValue then
    Exit;
  FShowZero := AValue;
  Invalidate;
end;

procedure TERDBadge.SetMaxCount(AValue: Integer);
begin
  if FMaxCount = AValue then
    Exit;
  FMaxCount := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

function TERDBadge.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewWidth := Max(ScaleValue(20),
    ERDMeasureText(DisplayText, 10.5, twBold, ScaleValue(96)) +
    ScaleValue(12));
  NewHeight := ScaleValue(18);
end;

procedure TERDBadge.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Fill: TColor;
begin
  if (FCount = 0) and not FShowZero and
    not (csDesigning in ComponentState) then
    Exit;
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
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

{ TERDBanner }

constructor TERDBanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCloseButton := TERDButton.Create(Self);
  FCloseButton.Parent := Self;
  FCloseButton.AutoSize := False;
  FCloseButton.Kind := bkGhost;
  FCloseButton.Glyph := glClose;
  FCloseButton.Visible := False;
  FCloseButton.OnClick := CloseClick;
  FActionButton := TERDButton.Create(Self);
  FActionButton.Parent := Self;
  FActionButton.Kind := bkSecondary;
  FActionButton.Visible := False;
  FActionButton.OnClick := ActionClick;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FKind := bnInfo;
  FIconSize := 18;
  Width := 420;
  Height := 56;
  AutoSize := True;
end;

procedure TERDBanner.SetShowClose(Value: Boolean);
begin
  FShowClose := Value;
  FCloseButton.Visible := Value;
  LayoutActions;
end;
procedure TERDBanner.SetActionCaption(const Value: string);
begin
  FActionCaption := Value;
  FActionButton.Caption := Value;
  FActionButton.Visible := Value <> '';
  LayoutActions;
end;
procedure TERDBanner.CloseClick(Sender: TObject);
begin
  Visible := False;
  if Assigned(FOnClose) then FOnClose(Self);
end;
procedure TERDBanner.ActionClick(Sender: TObject);
begin
  if Assigned(FOnAction) then FOnAction(Self);
end;
procedure TERDBanner.LayoutActions;
var RightEdge, H: Integer;
begin
  if (FCloseButton = nil) or (FActionButton = nil) then Exit;
  H := ScaleValue(Metrics.Edit);
  RightEdge := Width - ScaleValue(8);
  if FShowClose then
  begin
    FCloseButton.SetBounds(RightEdge - H, (Height - H) div 2, H, H);
    Dec(RightEdge, H + ScaleValue(8));
  end;
  if FActionCaption <> '' then
    FActionButton.SetBounds(RightEdge - FActionButton.Width, (Height - H) div 2, FActionButton.Width, H);
  Invalidate;
end;
procedure TERDBanner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FCloseButton then FCloseButton := nil;
    if AComponent = FActionButton then FActionButton := nil;
  end;
end;
procedure TERDBanner.ThemeChanged;
begin
  inherited;
  if FCloseButton <> nil then FCloseButton.Theme := Theme;
  if FActionButton <> nil then FActionButton.Theme := Theme;
end;
procedure TERDBanner.Resize;
begin
  inherited;
  LayoutActions;
end;

procedure TERDBanner.SetKind(AValue: TERDBannerKind);
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

procedure TERDBanner.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TERDBanner.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  Invalidate;
end;

procedure TERDBanner.SetAlertIcon(AValue: Boolean);
begin
  if FAlertIcon = AValue then
    Exit;
  FAlertIcon := AValue;
  Invalidate;
end;

procedure TERDBanner.SetIconSize(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 12, 64);
  if FIconSize = AValue then
    Exit;
  FIconSize := AValue;
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  Invalidate;
end;

function TERDBanner.IconCenterX: Integer;
begin
  // 15 px from the edge to the icon, as with the default 18 px icon.
  Result := ScaleValue(15) + ScaleValue(FIconSize) div 2;
end;

function TERDBanner.TextLeft: Integer;
begin
  Result := ScaleValue(15) + ScaleValue(FIconSize) + ScaleValue(11);
end;

procedure TERDBanner.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

function TERDBanner.SurfaceColor: TColor;
var
  P: TERDPainter;
begin
  P := TERDPainter.Create(nil, Palette, ScaleValue(96));
  try
    Result := P.BannerFill(FKind);
  finally
    P.Free;
  end;
end;

function TERDBanner.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := ScaleValue(Max(Max(56, Metrics.Head), FIconSize + 24));
end;

procedure TERDBanner.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Inc(Rect.Left, ScaleValue(ERD_EDGE));
end;

function TERDBanner.TextRight: Integer;
var
  I: Integer;
  Child: TControl;
begin
  Result := Width - ScaleValue(12);
  for I := 0 to ControlCount - 1 do
  begin
    Child := Controls[I];
    if Child.Visible and (Child.Left > TextLeft) then
      Result := Min(Result, Child.Left - ScaleValue(8));
  end;
end;

procedure TERDBanner.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  CY, X, MaxW: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.BannerFrame(Rect(0, 0, Width, Height), FKind);
    CY := Height div 2;
    P.BannerIcon(IconCenterX, CY, FKind, FAlertIcon, FIconSize / 18);
    X := TextLeft;
    MaxW := TextRight - X;
    if MaxW <= 0 then
      Exit;
    if FText = '' then
      P.Text(X, CY, FTitle, ERD_TEXT_BODY, Palette.ForegroundText, twBold,
        taLeftJustify, MaxW)
    else if FTitle = '' then
      P.Text(X, CY, FText, ERD_TEXT_BODY, Palette.ForegroundText, twRegular,
        taLeftJustify, MaxW)
    else
    begin
      P.Text(X, CY - P.S(9), FTitle, ERD_TEXT_BODY, Palette.ForegroundText, twBold,
        taLeftJustify, MaxW);
      P.Text(X, CY + P.S(10), FText, ERD_TEXT_BODY, Palette.ForegroundText, twRegular,
        taLeftJustify, MaxW);
    end;
  finally
    P.Free;
  end;
end;

end.
