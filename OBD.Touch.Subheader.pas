//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Subheader.pas
// CONTENTS       : Subheader component with Skia rendering
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/04/2024
// UPDATED        : 06/12/2025 - Refactored to inherit from TOBDCustomControl
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Touch.Subheader;

interface

uses
  System.SysUtils, System.Classes, System.Types, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes, Vcl.ExtCtrls, Vcl.Forms, System.Skia, Skia.Vcl,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Constants,
  OBD.Connection.Types, OBD.Connection.Component;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default height
  /// </summary>
  DEFAULT_HEIGHT = 40;

  /// <summary>
  ///   Default background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = clWhite;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00D7D7D7;

  /// <summary>
  ///   Default border from color
  /// </summary>
  DEFAULT_BORDER_FROM = $00918888;
  /// <summary>
  ///   Default border to color
  /// </summary>
  DEFAULT_BORDER_TO = $00776F6F;
  /// <summary>
  ///   Default border height
  /// </summary>
  DEFAULT_BORDER_HEIGHT = 2;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Touch Subheader background properties
  /// </summary>
  TOBDTouchSubheaderBackground = class(TPersistent)
  private
    FFromColor: TColor;
    FToColor: TColor;
    FOnChange: TNotifyEvent;

    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BACKGROUND_FROM;
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BACKGROUND_TO;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader border properties
  /// </summary>
  TOBDTouchSubheaderBorder = class(TPersistent)
  private
    FFromColor: TColor;
    FToColor: TColor;
    FHeight: Integer;
    FOnChange: TNotifyEvent;

    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetHeight(Value: Integer);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_FROM;
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_TO;
    property Height: Integer read FHeight write SetHeight default DEFAULT_BORDER_HEIGHT;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader battery indicator properties
  /// </summary>
  TOBDTouchSubheaderBatteryIndicator = class(TPersistent)
  private
    FVisible: Boolean;
    FSize: Integer;
    FFromColor: TColor;
    FToColor: TColor;
    FBorderWidth: Single;
    FBorderColor: TColor;
    FFont: TFont;
    FFormat: string;
    FVoltage: Single;
    FOnChange: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
    procedure SetBorderColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetFormat(const Value: string);
    procedure SetVoltage(Value: Single);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: Integer read FSize write SetSize default 24;
    property FromColor: TColor read FFromColor write SetFromColor default clLime;
    property ToColor: TColor read FToColor write SetToColor default clGreen;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Font: TFont read FFont write SetFont;
    property Format: string read FFormat write SetFormat;
    property Voltage: Single read FVoltage write SetVoltage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader VCI indicator properties
  /// </summary>
  TOBDTouchSubheaderVciIndicator = class(TPersistent)
  private
    FVisible: Boolean;
    FSize: Integer;
    FFromColor: TColor;
    FToColor: TColor;
    FBorderWidth: Single;
    FBorderColor: TColor;
    FFont: TFont;
    FCaption: string;
    FOnChange: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
    procedure SetBorderColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: Integer read FSize write SetSize default 24;
    property FromColor: TColor read FFromColor write SetFromColor default clSkyBlue;
    property ToColor: TColor read FToColor write SetToColor default clBlue;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Font: TFont read FFont write SetFont;
    property Caption: string read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader internet connection indicator properties
  /// </summary>
  TOBDTouchSubheaderInternetConnectionIndicator = class(TPersistent)
  private
    FVisible: Boolean;
    FSize: Integer;
    FFromColor: TColor;
    FToColor: TColor;
    FBorderWidth: Single;
    FBorderColor: TColor;
    FFont: TFont;
    FCaption: string;
    FOnChange: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
    procedure SetBorderColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default False;
    property Size: Integer read FSize write SetSize default 24;
    property FromColor: TColor read FFromColor write SetFromColor default clYellow;
    property ToColor: TColor read FToColor write SetToColor default clOlive;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Font: TFont read FFont write SetFont;
    property Caption: string read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader protocol indicator properties
  /// </summary>
  TOBDTouchSubheaderProtocolIndicator = class(TPersistent)
  private
    FVisible: Boolean;
    FSize: Integer;
    FFromColor: TColor;
    FToColor: TColor;
    FBorderWidth: Single;
    FBorderColor: TColor;
    FFont: TFont;
    FCaption: string;
    FOnChange: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
    procedure SetBorderColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: Integer read FSize write SetSize default 24;
    property FromColor: TColor read FFromColor write SetFromColor default clAqua;
    property ToColor: TColor read FToColor write SetToColor default clTeal;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Font: TFont read FFont write SetFont;
    property Caption: string read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader Component
  /// </summary>
  TOBDTouchSubheader = class(TOBDCustomControl)
  private
    FBackground: TOBDTouchSubheaderBackground;
    FBorder: TOBDTouchSubheaderBorder;
    FBatteryIndicator: TOBDTouchSubheaderBatteryIndicator;
    FVciIndicator: TOBDTouchSubheaderVciIndicator;
    FInternetConnectionIndicator: TOBDTouchSubheaderInternetConnectionIndicator;
    FProtocolIndicator: TOBDTouchSubheaderProtocolIndicator;
    FAutoApplyConnectionDetails: Boolean;
    FConnectionComponent: TOBDConnectionComponent;

    procedure SetBackground(Value: TOBDTouchSubheaderBackground);
    procedure SetBorder(Value: TOBDTouchSubheaderBorder);
    procedure SetBatteryIndicator(Value: TOBDTouchSubheaderBatteryIndicator);
    procedure SetVciIndicator(Value: TOBDTouchSubheaderVciIndicator);
    procedure SetInternetConnectionIndicator(Value: TOBDTouchSubheaderInternetConnectionIndicator);
    procedure SetProtocolIndicator(Value: TOBDTouchSubheaderProtocolIndicator);
    procedure SetAutoApplyConnectionDetails(Value: Boolean);
    procedure SetConnectionComponent(Value: TOBDConnectionComponent);
  protected
    procedure UpdateStyleElements; override;
    procedure SettingsChanged(Sender: TObject);
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure HandleConnectionStateChanged(Sender: TObject; ConnectionState: TOBDConnectionState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Repaint;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TOBDTouchSubheaderBackground read FBackground write SetBackground;
    property Border: TOBDTouchSubheaderBorder read FBorder write SetBorder;
    property BatteryIndicator: TOBDTouchSubheaderBatteryIndicator read FBatteryIndicator write SetBatteryIndicator;
    property VciIndicator: TOBDTouchSubheaderVciIndicator read FVciIndicator write SetVciIndicator;
    property InternetConnectionIndicator: TOBDTouchSubheaderInternetConnectionIndicator read FInternetConnectionIndicator write SetInternetConnectionIndicator;
    property ProtocolIndicator: TOBDTouchSubheaderProtocolIndicator read FProtocolIndicator write SetProtocolIndicator;
    property AutoApplyConnectionDetails: Boolean read FAutoApplyConnectionDetails write SetAutoApplyConnectionDetails default True;
    property ConnectionComponent: TOBDConnectionComponent read FConnectionComponent write SetConnectionComponent;
    property Align;
    property Anchors;
    property Color;
  end;

implementation

﻿//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Trigger repaint
  Invalidate;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SettingsChanged(Sender: TObject);
begin
  // Trigger repaint
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.PaintSkia(Canvas: ISkCanvas);
var
  Paint: ISkPaint;
  BackgroundRect, BorderRect: TRectF;
  MeasureRect: TRect;
  BatteryRect, VciRect, InternetRect, ProtocolRect: TRectF;
  BatteryCaptionRect, VciCaptionRect, InternetCaptionRect, ProtocolCaptionRect: TRect;
  BatteryLabelText, VciLabelText, InternetLabelText, ProtocolLabelText: string;
  HasBattery, HasVci, HasInternet, HasProtocol: Boolean;
  X, Y, Z: Single;
  TextSize: TSizeF;
begin
  // Clear canvas with background color
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

  // Draw the backround gradient when configured
  if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
  begin
    BackgroundRect := TRectF.Create(0.0, 0.0, Width + 0.0, Height + 0.0);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(BackgroundRect.Left, BackgroundRect.Top),
      TSkPoint.Create(BackgroundRect.Left, BackgroundRect.Bottom),
      [SafeColorRefToSkColor(Background.FromColor), SafeColorRefToSkColor(Background.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawRect(BackgroundRect, Paint);
  end;

  // Draw the border stripe when enabled
  if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
  begin
    BorderRect := TRectF.Create(0.0, Height - Border.Height + 0.0, Width + 0.0, Height + 0.0);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(BorderRect.Left, BorderRect.Top),
      TSkPoint.Create(BorderRect.Left, BorderRect.Bottom),
      [SafeColorRefToSkColor(Border.FromColor), SafeColorRefToSkColor(Border.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawRect(BorderRect, Paint);
  end;

  // Prepare layout state
  X := Width - 8;
  HasBattery := False;
  HasVci := False;
  HasInternet := False;
  HasProtocol := False;

  // Draw the battery indicator
  if BatteryIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (BatteryIndicator.Size / 2);
    BatteryLabelText := Format(BatteryIndicator.Format, [BatteryIndicator.Voltage]);

    TextSize := MeasureSkText(BatteryLabelText, BatteryIndicator.Font);
    MeasureRect := Rect(0, 0, Width - 8, Height - Border.Height);
    BatteryCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - (TextSize.cx + 4);

    BatteryRect := TRectF.Create(X - BatteryIndicator.Size, Y, X, Y + BatteryIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(BatteryRect.Left, BatteryRect.Top),
      TSkPoint.Create(BatteryRect.Left, BatteryRect.Bottom),
      [SafeColorRefToSkColor(BatteryIndicator.FromColor), SafeColorRefToSkColor(BatteryIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateVehicleBatteryPath(BatteryRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := BatteryIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(BatteryIndicator.BorderColor);
    Canvas.DrawPath(CreateVehicleBatteryPath(BatteryRect), Paint);

    X := X - (BatteryIndicator.Size + 4);
    HasBattery := True;
  end;

  // Draw the VCI indicator
  if VciIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (VciIndicator.Size / 2);
    VciLabelText := VciIndicator.Caption;

    TextSize := MeasureSkText(VciLabelText, VciIndicator.Font);
    MeasureRect := Rect(0, 0, Round(X), Height - Border.Height);
    VciCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - (TextSize.cx + 2);

    VciRect := TRectF.Create(X - VciIndicator.Size, Y, X, Y + VciIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(VciRect.Left, VciRect.Top),
      TSkPoint.Create(VciRect.Left, VciRect.Bottom),
      [SafeColorRefToSkColor(VciIndicator.FromColor), SafeColorRefToSkColor(VciIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateJ1962Path(VciRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := VciIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(VciIndicator.BorderColor);
    Canvas.DrawPath(CreateJ1962Path(VciRect), Paint);

    X := X - (VciIndicator.Size + 4);
    HasVci := True;
  end;

  // Draw the internet connection indicator
  if InternetConnectionIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (InternetConnectionIndicator.Size / 2);
    InternetLabelText := InternetConnectionIndicator.Caption;

    TextSize := MeasureSkText(InternetLabelText, InternetConnectionIndicator.Font);
    MeasureRect := Rect(0, 0, Round(X), Height - Border.Height);
    InternetCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - TextSize.cx;

    InternetRect := TRectF.Create(X - InternetConnectionIndicator.Size, Y, X, Y + InternetConnectionIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(InternetRect.Left, InternetRect.Top),
      TSkPoint.Create(InternetRect.Left, InternetRect.Bottom),
      [SafeColorRefToSkColor(InternetConnectionIndicator.FromColor), SafeColorRefToSkColor(InternetConnectionIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawOval(InternetRect, Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := InternetConnectionIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(InternetConnectionIndicator.BorderColor);
    Canvas.DrawPath(CreateInternetGlobePath(InternetRect), Paint);

    X := X - (InternetConnectionIndicator.Size + 4);
    HasInternet := True;
  end;

  // Draw the protocol indicator
  if ProtocolIndicator.Visible then
  begin
    Z := X;
    Y := ((Height - Border.Height) / 2) - (ProtocolIndicator.Size / 2);

    ProtocolRect := TRectF.Create(8, Y, 8 + ProtocolIndicator.Size, Y + ProtocolIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(ProtocolRect.Left, ProtocolRect.Top),
      TSkPoint.Create(ProtocolRect.Left, ProtocolRect.Bottom),
      [SafeColorRefToSkColor(ProtocolIndicator.FromColor), SafeColorRefToSkColor(ProtocolIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateProtocolPath(ProtocolRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := ProtocolIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(ProtocolIndicator.BorderColor);
    Canvas.DrawPath(CreateProtocolPath(ProtocolRect), Paint);

    ProtocolLabelText := FitTextToWidth(ProtocolIndicator.Caption, ProtocolIndicator.Font, Round(Z - (12 + ProtocolIndicator.Size)));
    TextSize := MeasureSkText(ProtocolLabelText, ProtocolIndicator.Font);
    MeasureRect := Rect(12 + ProtocolIndicator.Size, 0, Round(Z - (12 + ProtocolIndicator.Size)), Height - Border.Height);
    ProtocolCaptionRect := Rect(
      MeasureRect.Left,
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Left + Min(Ceil(TextSize.cx), MeasureRect.Right - MeasureRect.Left),
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    HasProtocol := True;
  end;

  // Render captions with Skia to keep text drawing inside the Skia pipeline
  if HasBattery then
  begin
    DrawSkTextCentered(Canvas, BatteryLabelText, BatteryIndicator.Font, TRectF.Create(BatteryCaptionRect), BatteryIndicator.Font.Color, TSkTextAlign.Right);
    DrawSkTextCentered(Canvas, '- +', BatteryIndicator.Font, BatteryRect, BatteryIndicator.Font.Color);
  end;

  if HasVci then
    DrawSkTextCentered(Canvas, VciLabelText, VciIndicator.Font, TRectF.Create(VciCaptionRect), VciIndicator.Font.Color, TSkTextAlign.Right);

  if HasInternet then
    DrawSkTextCentered(Canvas, InternetLabelText, InternetConnectionIndicator.Font, TRectF.Create(InternetCaptionRect), InternetConnectionIndicator.Font.Color, TSkTextAlign.Right);

  if HasProtocol then
    DrawSkTextCentered(Canvas, ProtocolLabelText, ProtocolIndicator.Font, TRectF.Create(ProtocolCaptionRect), ProtocolIndicator.Font.Color, TSkTextAlign.Left);

  // Direct rendering to canvas - no conversion needed!
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheader.Create(AOwner: TComponent);
begin
  // Call inherited constructor (handles rendering setup)
  inherited Create(AOwner);
  // Create background
  FBackground := TOBDTouchSubheaderBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create border
  FBorder := TOBDTouchSubheaderBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create battery indicator
  FBatteryIndicator := TOBDTouchSubheaderBatteryIndicator.Create;
  FBatteryIndicator.OnChange := SettingsChanged;
  // Create vci indicator
  FVciIndicator := TOBDTouchSubheaderVciIndicator.Create;
  FVciIndicator.OnChange := SettingsChanged;
  // Create internet connection indicator
  FInternetConnectionIndicator := TOBDTouchSubheaderInternetConnectionIndicator.Create;
  FInternetConnectionIndicator.OnChange := SettingsChanged;
  // Create protocol indicator
  FProtocolIndicator := TOBDTouchSubheaderProtocolIndicator.Create;
  FProtocolIndicator.OnChange := SettingsChanged;
  // Enable automatic connection application by default
  FAutoApplyConnectionDetails := True;
  // Set defaults
  Height := DEFAULT_HEIGHT;
  Align := alTop;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchSubheader.Destroy;
begin
  // Free background
  FBackground.Free;
  // Free border
  FBorder.Free;
  // Free battery indicator
  FBatteryIndicator.Free;
  // Free vci indicator
  FVciIndicator.Free;
  // Free internet connection indicator
  FInternetConnectionIndicator.Free;
  // Free protocol indicator
  FProtocolIndicator.Free;
  // Clear connection component reference
  if Assigned(FConnectionComponent) and (FConnectionComponent.OnConnectionStateChanged = HandleConnectionStateChanged) then
    FConnectionComponent.OnConnectionStateChanged := nil;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// REPAINT
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.Repaint;
begin
  // Call inherited repaint
  inherited;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDTouchSubheader) then
  begin
    FBackground.Assign((Source as TOBDTouchSubheader).Background);
    FBorder.Assign((Source as TOBDTouchSubheader).Border);
    FBatteryIndicator.Assign((Source as TOBDTouchSubheader).BatteryIndicator);
    FVciIndicator.Assign((Source as TOBDTouchSubheader).VciIndicator);
    FInternetConnectionIndicator.Assign((Source as TOBDTouchSubheader).InternetConnectionIndicator);
    FProtocolIndicator.Assign((Source as TOBDTouchSubheader).ProtocolIndicator);
  end;
end;

end.
