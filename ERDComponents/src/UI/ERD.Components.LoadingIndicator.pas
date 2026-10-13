unit ERD.Components.LoadingIndicator;
interface
uses
  System.Classes, System.SysUtils, System.Types, System.Math,
  Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint;
type
  TERDLoadingIndicator = class(TERDCustomControl)
  private
    FActive, FRespectAnimations: Boolean;
    FFrame: Integer;
    FTimer: TTimer;
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
  FActive := True;
  FRespectAnimations := True;
  AutoSize := True;
  TabStop := False;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 80;
  FTimer.OnTimer := TimerTick;
  SetBounds(Left, Top, 32, 32);
end;
destructor TERDLoadingIndicator.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
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
  NewWidth := ScaleValue(32);
  NewHeight := ScaleValue(32);
  if Caption = '' then Exit;
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(1, 1);
    P := TERDPainter.Create(Bitmap.Canvas, Palette, ScaleValue(96));
    try Inc(NewWidth, P.TextWidth(Caption, ERD_TEXT_BODY, twRegular) + P.S(8)) finally P.Free end;
  finally Bitmap.Free end;
end;
procedure TERDLoadingIndicator.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; I, Age, CX, CY: Integer; Angle, Radius, Diameter: Single;
  Ink, Dot: TColor;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    CX := Width div 2;
    if Caption <> '' then CX := P.S(16);
    CY := Height div 2;
    Radius := P.SF(8);
    Diameter := P.SF(3);
    Ink := Palette.Accent;
    if not Enabled or not FActive then Ink := P.DisabledText;
    for I := 0 to 11 do
    begin
      Angle := I * Pi / 6 - Pi / 2;
      Age := (FFrame - I + 12) mod 12;
      Dot := ERDMixColor(Ink, Palette.GaugeFace, 1 - Age / 15);
      P.Ellipse(CX + Cos(Angle) * Radius - Diameter / 2,
        CY + Sin(Angle) * Radius - Diameter / 2, Diameter, Diameter, Dot, clNone);
    end;
    if Caption <> '' then
    begin
      Ink := Palette.ForegroundText;
      if not Enabled then Ink := P.DisabledText;
      P.Text(P.S(32), CY, Caption, ERD_TEXT_BODY, Ink, twRegular,
        taLeftJustify, Width - P.S(36));
    end;
  finally P.Free end;
end;
end.
