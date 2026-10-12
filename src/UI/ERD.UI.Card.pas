//------------------------------------------------------------------------------
//  ERD.UI.Card
//
//  TOBDCard - the surface every OBD Studio panel sits on: a face-coloured
//  rectangle with a 1 px border, an optional title header, an optional
//  footer line and an optional 4 px status edge on the left.
//
//  The card is a container. Drop any control on it; aligned children
//  fill the content area between the header and the footer, inside
//  Padding. Children derived from TOBDCustomControl paint their
//  background in the card face (IOBDSurface), so a button on a card
//  sits on the card and not on the form colour.
//
//  Header and footer heights follow the density (56 / 40 px desktop,
//  68 / 60 px tablet).
//
//  Collapsible turns the card into an expander: a chevron appears on the
//  right of the header and a click on the header folds the card to just
//  its title bar (Collapsed) and back to ExpandedHeight. Stack collapsible
//  cards top-aligned in a TOBDScrollBox for a settings page or a long
//  form; the cards below move up when one folds.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//    2026-10-11  ERD  Collapsible / Collapsed (expander card).
//------------------------------------------------------------------------------

unit ERD.UI.Card;

interface

uses
  Winapi.Windows,
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
  /// <summary>Colour of the status edge on the left of a card.</summary>
  TOBDCardEdge = (
    /// <summary>No edge.</summary>
    ceNone,
    /// <summary>Muted grey.</summary>
    ceNeutral,
    /// <summary>ERDesigns orange.</summary>
    ceAccent,
    /// <summary>Green: all good.</summary>
    ceSuccess,
    /// <summary>Amber: attention.</summary>
    ceWarning,
    /// <summary>Red: fault.</summary>
    ceDanger);

  /// <summary>Themed card container with a title header, a footer line
  /// and a status edge.</summary>
  TOBDCard = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FShowHeader: Boolean;
    FFooterText: string;
    FShowFooter: Boolean;
    FStatusEdge: TOBDCardEdge;
    FCollapsible: Boolean;
    FCollapsed: Boolean;
    FExpandedHeight: Integer;
    FOnCollapsedChange: TNotifyEvent;
    procedure SetTitle(const AValue: string);
    procedure SetShowHeader(AValue: Boolean);
    procedure SetFooterText(const AValue: string);
    procedure SetShowFooter(AValue: Boolean);
    procedure SetStatusEdge(AValue: TOBDCardEdge);
    procedure SetCollapsible(AValue: Boolean);
    procedure SetCollapsed(AValue: Boolean);
    function IsExpandedHeightStored: Boolean;
    function CollapsedHeight: Integer;
    procedure LayoutChanged;
  protected
    /// <summary>Paints face, border, edge, header and footer.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Excludes border, edge, header and footer from the area
    /// aligned children use.</summary>
    procedure AdjustClientRect(var Rect: TRect); override;
    /// <summary>Applies a collapsed state read from the form file.
    /// </summary>
    procedure Loaded; override;
    /// <summary>Scales the remembered expanded height with the form.
    /// </summary>
    /// <param name="M">Multiplier.</param>
    /// <param name="D">Divisor.</param>
    /// <param name="isDpiChange">True for a DPI change.</param>
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    /// <summary>Folds or unfolds a collapsible card on a header click.
    /// </summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    /// <summary>Creates a 320 x 200 card with a header.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Re-aligns the children for the new header and footer
    /// heights.</summary>
    procedure DensityChanged; override;
    /// <summary>The card face; children paint their background in it.
    /// </summary>
    /// <returns>GaugeFace of the palette, or StyleBackground when set.
    /// </returns>
    function SurfaceColor: TColor;
    /// <summary>Header band in client coordinates; empty without a
    /// header.</summary>
    /// <returns>Header rectangle.</returns>
    function HeaderRect: TRect;
    /// <summary>Footer band in client coordinates; empty without a
    /// footer.</summary>
    /// <returns>Footer rectangle.</returns>
    function FooterRect: TRect;
    /// <summary>Flips <see cref="Collapsed"/>.</summary>
    procedure Toggle;
  published
    /// <summary>Title in the header, 15 px bold.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Shows the header band with the title.</summary>
    property ShowHeader: Boolean read FShowHeader write SetShowHeader
      default True;
    /// <summary>Muted text in the footer band.</summary>
    property FooterText: string read FFooterText write SetFooterText;
    /// <summary>Shows the footer band under a divider line.</summary>
    property ShowFooter: Boolean read FShowFooter write SetShowFooter
      default False;
    /// <summary>Colour of the 4 px edge on the left.</summary>
    property StatusEdge: TOBDCardEdge read FStatusEdge write SetStatusEdge
      default ceNone;
    /// <summary>Header and footer heights: desktop or tablet.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Space between the content area and the children.
    /// </summary>
    property Padding;
    /// <summary>Shows a chevron in the header; a click on the header
    /// folds and unfolds the card. Needs ShowHeader.</summary>
    property Collapsible: Boolean read FCollapsible write SetCollapsible
      default False;
    /// <summary>Folds the card to its header; the children are clipped
    /// away until it unfolds to <see cref="ExpandedHeight"/>.</summary>
    property Collapsed: Boolean read FCollapsed write SetCollapsed
      default False;
    /// <summary>Height to unfold to; recorded when the card folds.
    /// </summary>
    property ExpandedHeight: Integer read FExpandedHeight
      write FExpandedHeight stored IsExpandedHeightStored;
    /// <summary>Fires on the main thread after the card folded or
    /// unfolded.</summary>
    property OnCollapsedChange: TNotifyEvent read FOnCollapsedChange
      write FOnCollapsedChange;
  end;

implementation

const
  CARD_TITLE_SIZE = OBD_TEXT_HEADING;
  CARD_FOOTER_SIZE = OBD_TEXT_SMALL;

{ TOBDCard }

constructor TOBDCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FShowHeader := True;
  FStatusEdge := ceNone;
  Padding.SetBounds(16, 4, 16, 12);
  Width := 320;
  Height := 200;
end;

procedure TOBDCard.LayoutChanged;
begin
  if not (csLoading in ComponentState) then
    Realign;
  Invalidate;
end;

procedure TOBDCard.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDCard.SetShowHeader(AValue: Boolean);
begin
  if FShowHeader = AValue then
    Exit;
  FShowHeader := AValue;
  LayoutChanged;
end;

procedure TOBDCard.SetFooterText(const AValue: string);
begin
  if FFooterText = AValue then
    Exit;
  FFooterText := AValue;
  Invalidate;
end;

procedure TOBDCard.SetShowFooter(AValue: Boolean);
begin
  if FShowFooter = AValue then
    Exit;
  FShowFooter := AValue;
  LayoutChanged;
end;

procedure TOBDCard.SetStatusEdge(AValue: TOBDCardEdge);
begin
  if FStatusEdge = AValue then
    Exit;
  FStatusEdge := AValue;
  LayoutChanged;
end;

procedure TOBDCard.DensityChanged;
begin
  if FCollapsed and not (csLoading in ComponentState) then
    Height := CollapsedHeight;
  if not (csLoading in ComponentState) then
    Realign;
  inherited DensityChanged;
end;

function TOBDCard.CollapsedHeight: Integer;
begin
  // The title bar alone: the header minus the gap under the title.
  Result := ScaleValue(Metrics.Head - 8);
end;

function TOBDCard.IsExpandedHeightStored: Boolean;
begin
  Result := FCollapsed;
end;

procedure TOBDCard.SetCollapsible(AValue: Boolean);
begin
  if FCollapsible = AValue then
    Exit;
  FCollapsible := AValue;
  if not AValue and FCollapsed then
    SetCollapsed(False);
  Invalidate;
end;

procedure TOBDCard.SetCollapsed(AValue: Boolean);
begin
  if FCollapsed = AValue then
    Exit;
  FCollapsed := AValue;
  if csLoading in ComponentState then
    Exit;
  if AValue then
  begin
    FExpandedHeight := Height;
    Height := CollapsedHeight;
  end
  else if FExpandedHeight > CollapsedHeight then
    Height := FExpandedHeight;
  LayoutChanged;
  if Assigned(FOnCollapsedChange) then
    FOnCollapsedChange(Self);
end;

procedure TOBDCard.Toggle;
begin
  SetCollapsed(not FCollapsed);
end;

procedure TOBDCard.Loaded;
begin
  inherited Loaded;
  if FCollapsed then
  begin
    if FExpandedHeight <= 0 then
      FExpandedHeight := Height;
    Height := CollapsedHeight;
  end;
end;

procedure TOBDCard.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited ChangeScale(M, D, isDpiChange);
  if FExpandedHeight > 0 then
    FExpandedHeight := MulDiv(FExpandedHeight, M, D);
end;

procedure TOBDCard.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and FCollapsible and FShowHeader and Enabled and
    (Y < CollapsedHeight) then
    Toggle;
end;

function TOBDCard.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

function TOBDCard.HeaderRect: TRect;
begin
  if FShowHeader then
    Result := Rect(0, 0, Width, ScaleValue(Metrics.Head))
  else
    Result := Rect(0, 0, 0, 0);
end;

function TOBDCard.FooterRect: TRect;
begin
  if FShowFooter then
    Result := Rect(0, Height - ScaleValue(Metrics.Foot), Width, Height)
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TOBDCard.AdjustClientRect(var Rect: TRect);
begin
  // The inherited call applies Padding; the frame comes on top of it.
  inherited AdjustClientRect(Rect);
  if FStatusEdge <> ceNone then
    Inc(Rect.Left, ScaleValue(OBD_EDGE))
  else
    Inc(Rect.Left);
  Dec(Rect.Right);
  if FShowHeader then
    Inc(Rect.Top, ScaleValue(Metrics.Head))
  else
    Inc(Rect.Top);
  if FShowFooter then
    Dec(Rect.Bottom, ScaleValue(Metrics.Foot))
  else
    Dec(Rect.Bottom);
  if Rect.Right < Rect.Left then
    Rect.Right := Rect.Left;
  if Rect.Bottom < Rect.Top then
    Rect.Bottom := Rect.Top;
end;

procedure TOBDCard.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Edge: TColor;
  M: TOBDDensityMetrics;
  Left, FootH: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    M := Metrics;
    case FStatusEdge of
      ceNeutral:
        Edge := Palette.Subtle;
      ceAccent:
        Edge := Palette.Accent;
      ceSuccess:
        Edge := Palette.Success;
      ceWarning:
        Edge := Palette.Warning;
      ceDanger:
        Edge := Palette.Danger;
    else
      Edge := clNone;
    end;
    P.Card(Rect(0, 0, Width, Height), Edge, SurfaceColor);
    Left := P.S(OBD_PAD);
    if FShowHeader and (FTitle <> '') then
      if FCollapsible then
        P.Text(Left, P.S((M.Head - 8) / 2), FTitle, CARD_TITLE_SIZE,
          Palette.ForegroundText, twBold, taLeftJustify,
          Width - Left * 2 - P.S(24))
      else
        P.Text(Left, P.S((M.Head - 8) / 2), FTitle, CARD_TITLE_SIZE,
          Palette.ForegroundText, twBold, taLeftJustify, Width - Left * 2);
    // Expander chevron: down while open, right while folded.
    if FShowHeader and FCollapsible then
      P.GlyphChevron(Width - P.SF(OBD_PAD) - P.SF(6), P.SF((M.Head - 8) / 2),
        Palette.Subtle, not FCollapsed);
    if FShowFooter and not FCollapsed then
    begin
      FootH := P.S(M.Foot);
      if FStatusEdge <> ceNone then
        P.HLine(P.S(OBD_EDGE), Height - FootH + P.S(4),
          Width - P.S(OBD_EDGE), Palette.NeutralLight)
      else
        P.HLine(1, Height - FootH + P.S(4), Width - 2, Palette.NeutralLight);
      if FFooterText <> '' then
        P.Text(Left, Height - P.S((M.Foot - 4) / 2), FFooterText,
          CARD_FOOTER_SIZE, Palette.GaugeLabel, twRegular, taLeftJustify,
          Width - Left * 2);
    end;
  finally
    P.Free;
  end;
end;

end.
