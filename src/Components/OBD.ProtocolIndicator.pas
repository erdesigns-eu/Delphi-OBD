//------------------------------------------------------------------------------
// UNIT           : OBD.ProtocolIndicator.pas
// CONTENTS       : OBD protocol indicator display
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.ProtocolIndicator;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  Vcl.Controls, WinApi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Themes,
  System.Skia, Vcl.Skia, System.Types, System.UITypes,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Animation,
  OBD.Protocol.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type

  /// <summary>
  ///   Color settings for protocol indicator
  /// </summary>
  TOBDProtocolIndicatorColors = class(TPersistent)
  private
    FOwner: TComponent;
    FActive: TColor;
    FInactive: TColor;
    FBackground: TColor;
    FBorder: TColor;
    FText: TColor;
    FOnChange: TNotifyEvent;
    procedure SetActive(Value: TColor);
    procedure SetInactive(Value: TColor);
    procedure SetBackground(Value: TColor);
    procedure SetBorder(Value: TColor);
    procedure SetText(Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Active: TColor read FActive write SetActive default $0000FF00;  // Green
    property Inactive: TColor read FInactive write SetInactive default clGray;
    property Background: TColor read FBackground write SetBackground default clWhite;
    property Border: TColor read FBorder write SetBorder default clGray;
    property Text: TColor read FText write SetText default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Protocol Indicator - Shows active OBD protocol
  /// </summary>
  TOBDProtocolIndicator = class(TOBDCustomControl)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FProtocol: TOBDProtocolType;
    FShowDescription: Boolean;
    FColors: TOBDProtocolIndicatorColors;
    FTitleFont: TFont;
    FNameFont: TFont;
    FDescriptionFont: TFont;
    procedure SetProtocol(Value: TOBDProtocolType);
    procedure SetShowDescription(Value: Boolean);
    procedure SetColors(Value: TOBDProtocolIndicatorColors);
    procedure SetTitleFont(Value: TFont);
    procedure SetNameFont(Value: TFont);
    procedure SetDescriptionFont(Value: TFont);
    procedure OnColorsChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
  protected
    procedure InvalidateBackground; virtual;
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure BuildBackgroundSnapshot;
    function AcquireBackgroundSnapshot: ISkImage;
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Protocol: TOBDProtocolType read FProtocol write SetProtocol default ptNone;
    property ShowDescription: Boolean read FShowDescription write SetShowDescription default True;
    property Colors: TOBDProtocolIndicatorColors read FColors write SetColors;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property NameFont: TFont read FNameFont write SetNameFont;
    property DescriptionFont: TFont read FDescriptionFont write SetDescriptionFont;
    property Align;
    property Anchors;
    property Visible;
    property Enabled;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDProtocolIndicatorColors
//------------------------------------------------------------------------------

constructor TOBDProtocolIndicatorColors.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FActive := $0000FF00;  // Green
  FInactive := clGray;
  FBackground := clWhite;
  FBorder := clGray;
  FText := clBlack;
end;

procedure TOBDProtocolIndicatorColors.Assign(Source: TPersistent);
begin
  if Source is TOBDProtocolIndicatorColors then
  begin
    FActive := TOBDProtocolIndicatorColors(Source).Active;
    FInactive := TOBDProtocolIndicatorColors(Source).Inactive;
    FBackground := TOBDProtocolIndicatorColors(Source).Background;
    FBorder := TOBDProtocolIndicatorColors(Source).Border;
    FText := TOBDProtocolIndicatorColors(Source).Text;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDProtocolIndicatorColors.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDProtocolIndicatorColors.SetActive(Value: TColor);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TOBDProtocolIndicatorColors.SetInactive(Value: TColor);
begin
  if FInactive <> Value then
  begin
    FInactive := Value;
    Changed;
  end;
end;

procedure TOBDProtocolIndicatorColors.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TOBDProtocolIndicatorColors.SetBorder(Value: TColor);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

procedure TOBDProtocolIndicatorColors.SetText(Value: TColor);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDProtocolIndicator
//------------------------------------------------------------------------------

constructor TOBDProtocolIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FProtocol := ptNone;
  FShowDescription := True;
  
  FColors := TOBDProtocolIndicatorColors.Create(Self);
  FColors.OnChange := OnColorsChanged;
  
  // Create fonts with default values
  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Segoe UI';
  FTitleFont.Style := [fsBold];
  FTitleFont.Size := 12;
  FTitleFont.OnChange := OnFontChanged;
  
  FNameFont := TFont.Create;
  FNameFont.Name := 'Segoe UI';
  FNameFont.Style := [fsBold];
  FNameFont.Size := 20;
  FNameFont.OnChange := OnFontChanged;
  
  FDescriptionFont := TFont.Create;
  FDescriptionFont.Name := 'Segoe UI';
  FDescriptionFont.Size := 12;
  FDescriptionFont.OnChange := OnFontChanged;
  
  Width := 300;
  Height := 100;
end;

destructor TOBDProtocolIndicator.Destroy;
begin
  FTitleFont.Free;
  FNameFont.Free;
  FDescriptionFont.Free;
  FColors.Free;
  FRenderLock.Free;
  inherited;
end;

procedure TOBDProtocolIndicator.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDProtocolIndicator.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDProtocolIndicator.InvalidateBackground;
begin
  TMonitor.Enter(FRenderLock);
  try
    FBackgroundSnapshot := nil;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

procedure TOBDProtocolIndicator.OnColorsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDProtocolIndicator.SetProtocol(Value: TOBDProtocolType);
begin
  if FProtocol <> Value then
  begin
    FProtocol := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDProtocolIndicator.SetShowDescription(Value: Boolean);
begin
  if FShowDescription <> Value then
  begin
    FShowDescription := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDProtocolIndicator.SetColors(Value: TOBDProtocolIndicatorColors);
begin
  FColors.Assign(Value);
end;

procedure TOBDProtocolIndicator.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TOBDProtocolIndicator.SetNameFont(Value: TFont);
begin
  FNameFont.Assign(Value);
end;

procedure TOBDProtocolIndicator.SetDescriptionFont(Value: TFont);
begin
  FDescriptionFont.Assign(Value);
end;

procedure TOBDProtocolIndicator.OnFontChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

function TOBDProtocolIndicator.AcquireBackgroundSnapshot: ISkImage;
begin
  TMonitor.Enter(FRenderLock);
  try
    if not Assigned(FBackgroundSnapshot) then
      BuildBackgroundSnapshot;
    Result := FBackgroundSnapshot;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

procedure TOBDProtocolIndicator.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  
  // Background
  Paint := TSkPaint.Create;
  Paint.Color := SafeColorRefToSkColor(FColors.Background);
  Paint.Style := TSkPaintStyle.Fill;
  Canvas.DrawRect(RectF(0, 0, Width, Height), Paint);
  
  // Border
  Paint.Color := SafeColorRefToSkColor(FColors.Border);
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2;
  Canvas.DrawRect(RectF(1, 1, Width - 1, Height - 1), Paint);
  
  // Title - use persistent font property
  DrawSkTextCentered(Canvas, 'PROTOCOL', FTitleFont, RectF(10, 10, Width / 2, 30), FColors.Text, TSkTextAlign.Left);
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

procedure TOBDProtocolIndicator.PaintSkia(Canvas: ISkCanvas);
var
  Background: ISkImage;
  Paint: ISkPaint;
  IndicatorColor: TColor;
  TextY: Single;
  NameRect, DescRect: TRectF;
begin
  Background := AcquireBackgroundSnapshot;
  if Assigned(Background) then
    Canvas.DrawImage(Background, 0, 0);
  
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  
  // Status indicator
  IndicatorColor := IfThen(FProtocol <> ptNone, FColors.Active, FColors.Inactive);
  Paint.Color := SafeColorRefToSkColor(IndicatorColor);
  Canvas.DrawCircle(Width - 20, 20, 8, Paint);
  
  // Protocol name - use persistent font property
  TextY := Height / 2;
  if FShowDescription then
    TextY := Height / 2 - 10;
  
  NameRect := RectF(10, TextY - 15, Width - 10, TextY + 15);
  DrawSkTextCentered(Canvas, GetProtocolTypeName(FProtocol), FNameFont, NameRect, FColors.Text);
  
  // Description - use persistent font property
  if FShowDescription then
  begin
    DescRect := RectF(10, Height / 2 + 5, Width - 10, Height / 2 + 25);
    DrawSkTextCentered(Canvas, GetProtocolTypeDescription(FProtocol), FDescriptionFont, DescRect, FColors.Text);
  end;
end;

end.
