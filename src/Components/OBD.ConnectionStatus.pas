//------------------------------------------------------------------------------
// UNIT           : OBD.ConnectionStatus.pas
// CONTENTS       : OBD connection status indicator with signal strength
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.ConnectionStatus;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  Vcl.Controls, WinApi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Themes,
  System.Skia, Vcl.Skia, System.Types, System.UITypes,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Animation;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Connection status states
  /// </summary>
  TOBDConnectionState = (
    csDisconnected,   // Not connected
    csConnecting,     // Attempting to connect
    csConnected,      // Connected and active
    csError           // Connection error
  );

  /// <summary>
  ///   Color settings for connection status
  /// </summary>
  TOBDConnectionStatusColors = class(TPersistent)
  private
    FOwner: TComponent;
    FDisconnected: TColor;
    FConnecting: TColor;
    FConnected: TColor;
    FError: TColor;
    FBackground: TColor;
    FBorder: TColor;
    FText: TColor;
    FSignalBar: TColor;
    FOnChange: TNotifyEvent;
    procedure SetDisconnected(Value: TColor);
    procedure SetConnecting(Value: TColor);
    procedure SetConnected(Value: TColor);
    procedure SetError(Value: TColor);
    procedure SetBackground(Value: TColor);
    procedure SetBorder(Value: TColor);
    procedure SetText(Value: TColor);
    procedure SetSignalBar(Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Disconnected: TColor read FDisconnected write SetDisconnected default clGray;
    property Connecting: TColor read FConnecting write SetConnecting default $0000FFFF;  // Yellow
    property Connected: TColor read FConnected write SetConnected default $0000FF00;  // Green
    property Error: TColor read FError write SetError default clRed;
    property Background: TColor read FBackground write SetBackground default clWhite;
    property Border: TColor read FBorder write SetBorder default clGray;
    property Text: TColor read FText write SetText default clBlack;
    property SignalBar: TColor read FSignalBar write SetSignalBar default $0000FF00;  // Green
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Connection Status - Shows OBD connection state with signal strength
  /// </summary>
  TOBDConnectionStatus = class(TOBDCustomControl)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FConnectionState: TOBDConnectionState;
    FSignalStrength: Integer;
    FShowSignalBars: Boolean;
    FShowStatusText: Boolean;
    FColors: TOBDConnectionStatusColors;
    FAdapterName: string;
    procedure SetConnectionState(Value: TOBDConnectionState);
    procedure SetSignalStrength(Value: Integer);
    procedure SetShowSignalBars(Value: Boolean);
    procedure SetShowStatusText(Value: Boolean);
    procedure SetColors(Value: TOBDConnectionStatusColors);
    procedure SetAdapterName(Value: string);
    procedure OnColorsChanged(Sender: TObject);
    function GetStatusText: string;
    function GetStatusColor: TColor;
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
    property ConnectionState: TOBDConnectionState read FConnectionState write SetConnectionState default csDisconnected;
    property SignalStrength: Integer read FSignalStrength write SetSignalStrength default 0;
    property ShowSignalBars: Boolean read FShowSignalBars write SetShowSignalBars default True;
    property ShowStatusText: Boolean read FShowStatusText write SetShowStatusText default True;
    property AdapterName: string read FAdapterName write SetAdapterName;
    property Colors: TOBDConnectionStatusColors read FColors write SetColors;
    property Align;
    property Anchors;
    property Visible;
    property Enabled;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDConnectionStatusColors
//------------------------------------------------------------------------------

constructor TOBDConnectionStatusColors.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FDisconnected := clGray;
  FConnecting := $0000FFFF;  // Yellow
  FConnected := $0000FF00;  // Green
  FError := clRed;
  FBackground := clWhite;
  FBorder := clGray;
  FText := clBlack;
  FSignalBar := $0000FF00;  // Green
end;

procedure TOBDConnectionStatusColors.Assign(Source: TPersistent);
begin
  if Source is TOBDConnectionStatusColors then
  begin
    FDisconnected := TOBDConnectionStatusColors(Source).Disconnected;
    FConnecting := TOBDConnectionStatusColors(Source).Connecting;
    FConnected := TOBDConnectionStatusColors(Source).Connected;
    FError := TOBDConnectionStatusColors(Source).Error;
    FBackground := TOBDConnectionStatusColors(Source).Background;
    FBorder := TOBDConnectionStatusColors(Source).Border;
    FText := TOBDConnectionStatusColors(Source).Text;
    FSignalBar := TOBDConnectionStatusColors(Source).SignalBar;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDConnectionStatusColors.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDConnectionStatusColors.SetDisconnected(Value: TColor);
begin
  if FDisconnected <> Value then
  begin
    FDisconnected := Value;
    Changed;
  end;
end;

procedure TOBDConnectionStatusColors.SetConnecting(Value: TColor);
begin
  if FConnecting <> Value then
  begin
    FConnecting := Value;
    Changed;
  end;
end;

procedure TOBDConnectionStatusColors.SetConnected(Value: TColor);
begin
  if FConnected <> Value then
  begin
    FConnected := Value;
    Changed;
  end;
end;

procedure TOBDConnectionStatusColors.SetError(Value: TColor);
begin
  if FError <> Value then
  begin
    FError := Value;
    Changed;
  end;
end;

procedure TOBDConnectionStatusColors.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TOBDConnectionStatusColors.SetBorder(Value: TColor);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

procedure TOBDConnectionStatusColors.SetText(Value: TColor);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TOBDConnectionStatusColors.SetSignalBar(Value: TColor);
begin
  if FSignalBar <> Value then
  begin
    FSignalBar := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDConnectionStatus
//------------------------------------------------------------------------------

constructor TOBDConnectionStatus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FConnectionState := csDisconnected;
  FSignalStrength := 0;
  FShowSignalBars := True;
  FShowStatusText := True;
  FAdapterName := 'OBD Adapter';
  
  FColors := TOBDConnectionStatusColors.Create(Self);
  FColors.OnChange := OnColorsChanged;
  
  Width := 300;
  Height := 100;
end;

destructor TOBDConnectionStatus.Destroy;
begin
  FColors.Free;
  FRenderLock.Free;
  inherited;
end;

procedure TOBDConnectionStatus.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDConnectionStatus.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDConnectionStatus.InvalidateBackground;
begin
  TMonitor.Enter(FRenderLock);
  try
    FBackgroundSnapshot := nil;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

procedure TOBDConnectionStatus.OnColorsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDConnectionStatus.SetConnectionState(Value: TOBDConnectionState);
begin
  if FConnectionState <> Value then
  begin
    FConnectionState := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectionStatus.SetSignalStrength(Value: Integer);
begin
  Value := EnsureRange(Value, 0, 5);
  if FSignalStrength <> Value then
  begin
    FSignalStrength := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectionStatus.SetShowSignalBars(Value: Boolean);
begin
  if FShowSignalBars <> Value then
  begin
    FShowSignalBars := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectionStatus.SetShowStatusText(Value: Boolean);
begin
  if FShowStatusText <> Value then
  begin
    FShowStatusText := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectionStatus.SetAdapterName(Value: string);
begin
  if FAdapterName <> Value then
  begin
    FAdapterName := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectionStatus.SetColors(Value: TOBDConnectionStatusColors);
begin
  FColors.Assign(Value);
end;

function TOBDConnectionStatus.GetStatusText: string;
begin
  case FConnectionState of
    csDisconnected: Result := 'DISCONNECTED';
    csConnecting:   Result := 'CONNECTING...';
    csConnected:    Result := 'CONNECTED';
    csError:        Result := 'CONNECTION ERROR';
  else
    Result := 'UNKNOWN';
  end;
end;

function TOBDConnectionStatus.GetStatusColor: TColor;
begin
  case FConnectionState of
    csDisconnected: Result := FColors.Disconnected;
    csConnecting:   Result := FColors.Connecting;
    csConnected:    Result := FColors.Connected;
    csError:        Result := FColors.Error;
  else
    Result := FColors.Text;
  end;
end;

function TOBDConnectionStatus.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDConnectionStatus.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  Font: ISkFont;
  ConnectorRect: TRectF;
  ConnectorPath: ISkPath;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  
  // Background
  Paint := TSkPaint.Create;
  Paint.Color := FColors.Background;
  Paint.Style := TSkPaintStyle.Fill;
  Canvas.DrawRect(RectF(0, 0, Width, Height), Paint);
  
  // Border
  Paint.Color := FColors.Border;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2;
  Canvas.DrawRect(RectF(1, 1, Width - 1, Height - 1), Paint);
  
  // Draw J1962 connector icon
  ConnectorRect := RectF(10, 10, 50, 50);
  ConnectorPath := CreateJ1962Path(ConnectorRect);
  Paint.Color := FColors.Text;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1.5;
  Canvas.DrawPath(ConnectorPath, Paint);
  
  // Adapter name
  if FAdapterName <> '' then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Normal), 10);
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := FColors.Text;
    Canvas.DrawSimpleText(FAdapterName, 60, 35, Font, Paint, TSkTextAlign.Left);
  end;
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

procedure TOBDConnectionStatus.PaintSkia(Canvas: ISkCanvas);
var
  Background: ISkImage;
  Paint: ISkPaint;
  Font: ISkFont;
  StatusColor: TColor;
  I, BarHeight: Integer;
  BarRect: TRectF;
  BarX, BarY, BarWidth, BarSpacing: Single;
begin
  Background := AcquireBackgroundSnapshot;
  if Assigned(Background) then
    Canvas.DrawImage(Background, 0, 0);
  
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  StatusColor := GetStatusColor;
  
  // Status indicator circle
  Paint.Color := StatusColor;
  Canvas.DrawCircle(Width - 20, 20, 8, Paint);
  
  // Status text
  if FShowStatusText then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Bold), 16);
    Paint.Color := StatusColor;
    Canvas.DrawSimpleText(GetStatusText, Width / 2, Height - 20, Font, Paint, TSkTextAlign.Center);
  end;
  
  // Signal strength bars
  if FShowSignalBars and (FConnectionState = csConnected) then
  begin
    BarWidth := 8;
    BarSpacing := 4;
    BarX := Width - 100;
    BarY := Height - 45;
    
    for I := 1 to 5 do
    begin
      BarHeight := 10 + (I * 5);
      BarRect := RectF(BarX + (I - 1) * (BarWidth + BarSpacing), 
                       BarY - BarHeight, 
                       BarX + (I - 1) * (BarWidth + BarSpacing) + BarWidth, 
                       BarY);
      
      if I <= FSignalStrength then
        Paint.Color := FColors.SignalBar
      else
        Paint.Color := FColors.Disconnected;
      
      Canvas.DrawRect(BarRect, Paint);
    end;
  end;
end;

end.
