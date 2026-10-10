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
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Card;

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
    procedure SetTitle(const AValue: string);
    procedure SetShowHeader(AValue: Boolean);
    procedure SetFooterText(const AValue: string);
    procedure SetShowFooter(AValue: Boolean);
    procedure SetStatusEdge(AValue: TOBDCardEdge);
    procedure LayoutChanged;
  protected
    /// <summary>Paints face, border, edge, header and footer.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Excludes border, edge, header and footer from the area
    /// aligned children use.</summary>
    procedure AdjustClientRect(var Rect: TRect); override;
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
  end;

implementation

const
  CARD_TITLE_SIZE = 15;
  CARD_FOOTER_SIZE = 12;

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
  if not (csLoading in ComponentState) then
    Realign;
  inherited DensityChanged;
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
      P.Text(Left, P.S((M.Head - 8) / 2), FTitle, CARD_TITLE_SIZE,
        Palette.ForegroundText, twBold, taLeftJustify, Width - Left * 2);
    if FShowFooter then
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
