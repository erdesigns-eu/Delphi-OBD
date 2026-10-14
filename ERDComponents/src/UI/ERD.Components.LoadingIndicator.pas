unit ERD.Components.LoadingIndicator;
interface
uses
  System.Classes, System.SysUtils, System.Types, System.Math,
  Winapi.Windows, Winapi.Messages, Winapi.GDIPAPI, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint;
type
  TERDLoadingIndicatorStyle = (lisSpinner, lisRing, lisDots, lisBars, lisPulse);
  TERDLoadingIndicator = class(TERDCustomControl)
  private
    FActive, FRespectAnimations: Boolean;
    FFrame: Integer;
    FIndicatorSize, FAnimationInterval: Integer;
    FStyle: TERDLoadingIndicatorStyle;
    FTimer: TTimer;
    procedure SetStyle(Value: TERDLoadingIndicatorStyle);
    procedure SetIndicatorSize(Value: Integer);
    procedure SetAnimationInterval(Value: Integer);
    procedure SetActive(Value: Boolean);
    procedure SetRespectAnimations(Value: Boolean);
    procedure UpdateTimer;
    procedure TimerTick(Sender: TObject);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSettingChange(var Message: TMessage); message WM_SETTINGCHANGE;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure Advance;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DensityChanged; override;
    property FrameIndex: Integer read FFrame;
    function IsAnimating: Boolean;
  published
    property Style: TERDLoadingIndicatorStyle read FStyle write SetStyle default lisSpinner;
    /// <summary>Indicator box size in 96-DPI pixels; independent of Caption.</summary>
    property IndicatorSize: Integer read FIndicatorSize write SetIndicatorSize default 32;
    /// <summary>Time between animation frames in milliseconds.</summary>
    property AnimationInterval: Integer read FAnimationInterval write SetAnimationInterval default 80;
    property Active: Boolean read FActive write SetActive default True;
    property RespectAnimations: Boolean read FRespectAnimations write SetRespectAnimations default True;
    property Caption;
    property AutoSize default True;
    property TabStop default False;
    property Density;
    property ParentDensity;
  end;
implementation
constructor TERDLoadingIndicator.Create(AOwner: TComponent);
begin
  inherited;
  FStyle := lisSpinner;
  FIndicatorSize := 32;
  FAnimationInterval := 80;
  FActive := True;
  FRespectAnimations := True;
  AutoSize := True;
  TabStop := False;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := FAnimationInterval;
  FTimer.OnTimer := TimerTick;
  SetBounds(Left, Top, 32, 32);
end;
destructor TERDLoadingIndicator.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;
procedure TERDLoadingIndicator.SetStyle(Value: TERDLoadingIndicatorStyle);
begin
  if FStyle = Value then Exit;
  FStyle := Value;
  FFrame := 0;
  Invalidate;
end;
procedure TERDLoadingIndicator.SetIndicatorSize(Value: Integer);
begin
  Value := EnsureRange(Value, 8, 256);
  if FIndicatorSize = Value then Exit;
  FIndicatorSize := Value;
  if AutoSize then AdjustSize;
  Invalidate;
end;
procedure TERDLoadingIndicator.SetAnimationInterval(Value: Integer);
begin
  Value := EnsureRange(Value, 16, 2000);
  if FAnimationInterval = Value then Exit;
  FAnimationInterval := Value;
  if FTimer <> nil then FTimer.Interval := Value;
end;
procedure TERDLoadingIndicator.SetActive(Value: Boolean);
begin
  if FActive = Value then Exit;
  FActive := Value;
  UpdateTimer;
  Invalidate;
end;
procedure TERDLoadingIndicator.SetRespectAnimations(Value: Boolean);
begin
  if FRespectAnimations = Value then Exit;
  FRespectAnimations := Value;
  UpdateTimer;
end;
procedure TERDLoadingIndicator.UpdateTimer;
var Animate: BOOL;
begin
  if FTimer = nil then Exit;
  Animate := True;
  if FRespectAnimations then SystemParametersInfo(SPI_GETCLIENTAREAANIMATION, 0, @Animate, 0);
  FTimer.Enabled := FActive and Enabled and Showing and Animate and
    not (csDesigning in ComponentState) and not (csLoading in ComponentState) and
    not (csDestroying in ComponentState);
end;
function TERDLoadingIndicator.IsAnimating: Boolean;
begin Result := (FTimer <> nil) and FTimer.Enabled end;
procedure TERDLoadingIndicator.Advance;
begin
  if not FActive or not Enabled then Exit;
  FFrame := (FFrame + 1) mod 12;
  Invalidate;
end;
procedure TERDLoadingIndicator.TimerTick(Sender: TObject);
begin
  if not Showing then UpdateTimer else Advance;
end;
procedure TERDLoadingIndicator.CreateWnd;
begin inherited; UpdateTimer end;
procedure TERDLoadingIndicator.DestroyWnd;
begin
  if FTimer <> nil then FTimer.Enabled := False;
  inherited;
end;
procedure TERDLoadingIndicator.Loaded;
begin inherited; UpdateTimer end;
procedure TERDLoadingIndicator.CMShowingChanged(var Message: TMessage);
begin inherited; UpdateTimer end;
procedure TERDLoadingIndicator.CMEnabledChanged(var Message: TMessage);
begin inherited; UpdateTimer; Invalidate end;
procedure TERDLoadingIndicator.CMTextChanged(var Message: TMessage);
begin inherited; if AutoSize then AdjustSize; Invalidate end;
procedure TERDLoadingIndicator.WMSettingChange(var Message: TMessage);
begin inherited; UpdateTimer; Invalidate end;
procedure TERDLoadingIndicator.DensityChanged;
begin inherited; if AutoSize then AdjustSize end;
function TERDLoadingIndicator.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var Bitmap: TBitmap; P: TERDPainter;
begin
  Result := True;
  NewWidth := ScaleValue(FIndicatorSize);
  NewHeight := ScaleValue(FIndicatorSize);
  if Caption = '' then Exit;
  NewHeight := Max(NewHeight, ScaleValue(24));
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(1, 1);
    P := TERDPainter.Create(Bitmap.Canvas, Palette, ScaleValue(96));
    try Inc(NewWidth, P.TextWidth(Caption, ERD_TEXT_BODY, twRegular) + P.S(8)) finally P.Free end;
  finally Bitmap.Free end;
end;
procedure TERDLoadingIndicator.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; I, Age, TextX: Integer; CX, CY, Area, Angle, Radius, Diameter,
  Level, BarWidth, BarHeight, Gap, Phase, X: Single; Ink, Dot: TColor;
  Points: TArray<TGPPointF>;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Area := Max(1, Min(P.SF(FIndicatorSize), Min(Width, Height)));
    CX := Width / 2;
    if Caption <> '' then CX := Area / 2;
    CY := Height / 2;
    Ink := Palette.Accent;
    if not Enabled or not FActive then Ink := P.DisabledText;
    Phase := FFrame * Pi / 6;
    case FStyle of
      lisSpinner:
        begin
          Radius := Area / 4;
          Diameter := Area * 3 / 32;
          for I := 0 to 11 do
          begin
            Angle := I * Pi / 6 - Pi / 2;
            Age := (FFrame - I + 12) mod 12;
            Dot := ERDMixColor(Ink, Palette.GaugeFace, 1 - Age / 15);
            P.Ellipse(CX + Cos(Angle) * Radius - Diameter / 2,
              CY + Sin(Angle) * Radius - Diameter / 2, Diameter, Diameter, Dot, clNone);
          end;
        end;
      lisRing:
        begin
          Radius := Area * 0.34;
          P.Ellipse(CX - Radius, CY - Radius, Radius * 2, Radius * 2,
            clNone, P.Tint(Ink, 0.15), Max(1, Area / 12));
          SetLength(Points, 31);
          for I := 0 to High(Points) do
          begin
            Angle := Phase - Pi / 2 + I * Pi * 1.5 / High(Points);
            Points[I] := ERDPointF(CX + Cos(Angle) * Radius, CY + Sin(Angle) * Radius);
          end;
          P.Lines(Points, Ink, Max(1, Area / 12));
        end;
      lisDots:
        for I := 0 to 2 do
        begin
          Level := (1 + Cos(Phase - I * Pi / 2)) / 2;
          Diameter := Area * (0.12 + 0.05 * Level);
          X := CX + (I - 1) * Area * 0.26;
          P.Ellipse(X - Diameter / 2, CY - Diameter / 2 - Level * Area * 0.06,
            Diameter, Diameter, P.Tint(Ink, 0.3 + 0.7 * Level), clNone);
        end;
      lisBars:
        begin
          BarWidth := Area * 0.09;
          Gap := Area * 0.07;
          for I := 0 to 4 do
          begin
            Level := (1 + Cos(Phase - I * Pi / 3)) / 2;
            BarHeight := Area * (0.2 + 0.42 * Level);
            X := CX + (I - 2) * (BarWidth + Gap) - BarWidth / 2;
            P.RoundRect(X, CY - BarHeight / 2, BarWidth, BarHeight,
              BarWidth / 2, P.Tint(Ink, 0.35 + 0.65 * Level), clNone);
          end;
        end;
      lisPulse:
        begin
          Level := (1 + Sin(Phase)) / 2;
          Diameter := Area * (0.4 + 0.35 * Level);
          P.Ellipse(CX - Diameter / 2, CY - Diameter / 2, Diameter, Diameter,
            P.Tint(Ink, 0.15 + 0.25 * (1 - Level)), clNone);
          Diameter := Area * 0.28;
          P.Ellipse(CX - Diameter / 2, CY - Diameter / 2, Diameter, Diameter, Ink, clNone);
        end;
    end;
    if Caption <> '' then
    begin
      Ink := Palette.ForegroundText;
      if not Enabled then Ink := P.DisabledText;
      TextX := Round(Area) + P.S(8);
      P.Text(TextX, Round(CY), Caption, ERD_TEXT_BODY, Ink, twRegular,
        taLeftJustify, Max(0, Width - TextX - P.S(4)));
    end;
  finally P.Free end;
end;
end.
