//------------------------------------------------------------------------------
// UNIT           : OBD.LED.FMX.pas
// CONTENTS       : FMX binding for the LED indicator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Backed by `OBD.Render.LED`. The VCL `TOBDLed`
//                  remains on its image-caching path because it leans
//                  on VCL `TStyleManager` for the background tint;
//                  unifying the two LED implementations is a v3.2+
//                  follow-up that would need a platform-neutral style
//                  abstraction. This FMX binding is independent.
//------------------------------------------------------------------------------
unit OBD.LED.FMX;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Skia, System.Skia,

  OBD.Render.LED;

const
  LEDFMX_DEFAULT_BACKGROUND     = $FF181818;
  LEDFMX_DEFAULT_GRAYED_FROM    = $FF606060;
  LEDFMX_DEFAULT_GRAYED_TO      = $FF202020;
  LEDFMX_DEFAULT_OFF_FROM       = $FF333333;
  LEDFMX_DEFAULT_OFF_TO         = $FF111111;
  LEDFMX_DEFAULT_ON_FROM        = $FF66FF66;
  LEDFMX_DEFAULT_ON_TO          = $FF008800;
  LEDFMX_DEFAULT_BORDER_FROM    = $FF606060;
  LEDFMX_DEFAULT_BORDER_TO      = $FF202020;
  LEDFMX_DEFAULT_BORDER_WIDTH: Single = 2.0;
  LEDFMX_DEFAULT_MARGIN: Single = 2.0;

type
  TOBDLedFMX = class(TSkPaintBox)
  private
    FState: TOBDLedState;
    FBackgroundColor: TAlphaColor;
    FGrayedFromColor, FGrayedToColor: TAlphaColor;
    FOffFromColor, FOffToColor: TAlphaColor;
    FOnFromColor, FOnToColor: TAlphaColor;
    FBorderFromColor, FBorderToColor: TAlphaColor;
    FBorderWidth: Single;
    FMarginFromBorder: Single;

    procedure SetState(const AValue: TOBDLedState);
    procedure SetBackgroundColor(const AValue: TAlphaColor);
    procedure SetGrayedFromColor(const AValue: TAlphaColor);
    procedure SetGrayedToColor(const AValue: TAlphaColor);
    procedure SetOffFromColor(const AValue: TAlphaColor);
    procedure SetOffToColor(const AValue: TAlphaColor);
    procedure SetOnFromColor(const AValue: TAlphaColor);
    procedure SetOnToColor(const AValue: TAlphaColor);
    procedure SetBorderFromColor(const AValue: TAlphaColor);
    procedure SetBorderToColor(const AValue: TAlphaColor);
    procedure SetBorderWidth(const AValue: Single);
    procedure SetMarginFromBorder(const AValue: Single);

    procedure HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property State: TOBDLedState read FState write SetState default lsGrayed;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default LEDFMX_DEFAULT_BACKGROUND;
    property GrayedFromColor: TAlphaColor read FGrayedFromColor write SetGrayedFromColor default LEDFMX_DEFAULT_GRAYED_FROM;
    property GrayedToColor: TAlphaColor read FGrayedToColor write SetGrayedToColor default LEDFMX_DEFAULT_GRAYED_TO;
    property OffFromColor: TAlphaColor read FOffFromColor write SetOffFromColor default LEDFMX_DEFAULT_OFF_FROM;
    property OffToColor: TAlphaColor read FOffToColor write SetOffToColor default LEDFMX_DEFAULT_OFF_TO;
    property OnFromColor: TAlphaColor read FOnFromColor write SetOnFromColor default LEDFMX_DEFAULT_ON_FROM;
    property OnToColor: TAlphaColor read FOnToColor write SetOnToColor default LEDFMX_DEFAULT_ON_TO;
    property BorderFromColor: TAlphaColor read FBorderFromColor write SetBorderFromColor default LEDFMX_DEFAULT_BORDER_FROM;
    property BorderToColor: TAlphaColor read FBorderToColor write SetBorderToColor default LEDFMX_DEFAULT_BORDER_TO;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property MarginFromBorder: Single read FMarginFromBorder write SetMarginFromBorder;
  end;

implementation

constructor TOBDLedFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := lsGrayed;
  FBackgroundColor := LEDFMX_DEFAULT_BACKGROUND;
  FGrayedFromColor := LEDFMX_DEFAULT_GRAYED_FROM;
  FGrayedToColor := LEDFMX_DEFAULT_GRAYED_TO;
  FOffFromColor := LEDFMX_DEFAULT_OFF_FROM;
  FOffToColor := LEDFMX_DEFAULT_OFF_TO;
  FOnFromColor := LEDFMX_DEFAULT_ON_FROM;
  FOnToColor := LEDFMX_DEFAULT_ON_TO;
  FBorderFromColor := LEDFMX_DEFAULT_BORDER_FROM;
  FBorderToColor := LEDFMX_DEFAULT_BORDER_TO;
  FBorderWidth := LEDFMX_DEFAULT_BORDER_WIDTH;
  FMarginFromBorder := LEDFMX_DEFAULT_MARGIN;

  Width := 36;
  Height := 36;
  OnDraw := HandleDraw;
end;

procedure TOBDLedFMX.SetState(const AValue: TOBDLedState);
begin if FState <> AValue then begin FState := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetBackgroundColor(const AValue: TAlphaColor);
begin if FBackgroundColor <> AValue then begin FBackgroundColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetGrayedFromColor(const AValue: TAlphaColor);
begin if FGrayedFromColor <> AValue then begin FGrayedFromColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetGrayedToColor(const AValue: TAlphaColor);
begin if FGrayedToColor <> AValue then begin FGrayedToColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetOffFromColor(const AValue: TAlphaColor);
begin if FOffFromColor <> AValue then begin FOffFromColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetOffToColor(const AValue: TAlphaColor);
begin if FOffToColor <> AValue then begin FOffToColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetOnFromColor(const AValue: TAlphaColor);
begin if FOnFromColor <> AValue then begin FOnFromColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetOnToColor(const AValue: TAlphaColor);
begin if FOnToColor <> AValue then begin FOnToColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetBorderFromColor(const AValue: TAlphaColor);
begin if FBorderFromColor <> AValue then begin FBorderFromColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetBorderToColor(const AValue: TAlphaColor);
begin if FBorderToColor <> AValue then begin FBorderToColor := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetBorderWidth(const AValue: Single);
begin if (AValue >= 0) and (FBorderWidth <> AValue) then begin FBorderWidth := AValue; Redraw; end; end;
procedure TOBDLedFMX.SetMarginFromBorder(const AValue: Single);
begin if (AValue >= 0) and (FMarginFromBorder <> AValue) then begin FMarginFromBorder := AValue; Redraw; end; end;

procedure TOBDLedFMX.HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  S: TOBDLedRenderState;
begin
  S.Width := ADest.Width;
  S.Height := ADest.Height;
  S.State := FState;
  S.BackgroundColor := FBackgroundColor;
  S.GrayedFromColor := FGrayedFromColor;
  S.GrayedToColor := FGrayedToColor;
  S.OffFromColor := FOffFromColor;
  S.OffToColor := FOffToColor;
  S.OnFromColor := FOnFromColor;
  S.OnToColor := FOnToColor;
  S.BorderFromColor := FBorderFromColor;
  S.BorderToColor := FBorderToColor;
  S.BorderWidth := FBorderWidth;
  S.MarginFromBorder := FMarginFromBorder;
  RenderLED(ACanvas, S);
end;

end.
