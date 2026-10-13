unit ERD.Components.Panel;

interface

uses
  System.Classes, System.Types, System.SysUtils, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint;

type
  /// <summary>General themed container for controls, badges, buttons and layouts.</summary>
  TERDPanel = class(TERDCustomControl, IERDSurface)
  private
    FBorderWidth: Integer;
    FHeaderHeight: Integer;
    procedure SetBorderWidth(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
  protected
    function SurfaceColor: TColor;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure AdjustClientRect(var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 0;
    property Caption;
    property Padding;
    property Align;
    property Anchors;
    property Constraints;
    property TabOrder;
    property TabStop;
    property Enabled;
    property Visible;
    property OnClick;
  end;

implementation

constructor TERDPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FBorderWidth := 1;
  Width := 240;
  Height := 120;
end;

function TERDPanel.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TERDPanel.SetBorderWidth(Value: Integer);
begin
  if Value < 0 then raise EArgumentOutOfRangeException.Create('BorderWidth must be nonnegative');
  if FBorderWidth = Value then Exit;
  FBorderWidth := Value;
  Realign;
  Invalidate;
end;
procedure TERDPanel.SetHeaderHeight(Value: Integer);
begin
  if Value < 0 then raise EArgumentOutOfRangeException.Create('HeaderHeight must be nonnegative');
  if FHeaderHeight = Value then Exit;
  FHeaderHeight := Value;
  Realign;
  Invalidate;
end;

procedure TERDPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  Inc(Rect.Left, ScaleValue(FBorderWidth));
  Inc(Rect.Top, ScaleValue(FBorderWidth + FHeaderHeight));
  Dec(Rect.Right, ScaleValue(FBorderWidth));
  Dec(Rect.Bottom, ScaleValue(FBorderWidth));
end;

procedure TERDPanel.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter;
begin
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, SurfaceColor);
    if FBorderWidth > 0 then Painter.FrameRect(ClientRect, Palette.NeutralLight,
      ScaleValue(FBorderWidth));
    if FHeaderHeight > 0 then
      Painter.Text(ScaleValue(8), ScaleValue(FHeaderHeight) div 2, Caption,
        12, Palette.ForegroundText, twSemibold);
  finally Painter.Free end;
end;
end.
