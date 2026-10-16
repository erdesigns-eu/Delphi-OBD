//------------------------------------------------------------------------------
//  ERD.Components.Hint
//
//  Themed hint windows for Application controls.
//
//    TERDHintWindow   THintWindow descendant that draws a card with a bold
//                     title, optional body text and an optional shortcut chip.
//    TERDHintStyle    non-visual component that installs TERDHintWindow for
//                     the application while active.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.Hint;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.Themes,
  ERD.Components.Types,
  ERD.Components.Theme,
  ERD.Components.Paint;

type
  /// <summary>Themed hint window with title, body and shortcut text.</summary>
  TERDHintWindow = class(THintWindow)
  strict private
    FTitle: string;
    FBody: string;
    FShortcut: string;
    procedure ParseHint(const AHint: string; AData: Pointer);
    function ActivePalette: TERDThemePalette;
    function ActiveDensity: TERDDensity;
    function TextWidth(const AText: string; ASize: Single;
      AWeight: TERDTextWeight): Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    /// <summary>Draws the themed card.</summary>
    procedure Paint; override;
    /// <summary>Draws no native border.</summary>
    /// <param name="Message">Non-client paint message.</param>
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
  public
    /// <summary>Calculates the themed hint rectangle.</summary>
    /// <param name="MaxWidth">Maximum width.</param>
    /// <param name="AHint">Hint text in Title|Body form.</param>
    /// <param name="AData">Hint information pointer.</param>
    /// <returns>Required rectangle.</returns>
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    /// <summary>Reads hint data before the window is shown.</summary>
    /// <param name="Rect">Window rectangle.</param>
    /// <param name="AHint">Hint text.</param>
    /// <param name="AData">Hint information pointer.</param>
    procedure ActivateHintData(Rect: TRect; const AHint: string;
      AData: Pointer); override;
    /// <summary>Creates the themed hint window.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>Application-level themed hint installer.</summary>
  TERDHintStyle = class(TComponent, IERDThemeAware)
  strict private
    FTheme: TERDTheme;
    FDensity: TERDDensity;
    FActive: Boolean;
    procedure SetTheme(AValue: TERDTheme);
    procedure SetDensity(AValue: TERDDensity);
    procedure SetActive(AValue: Boolean);
    function GetPalette: TERDThemePalette;
    procedure Install;
    procedure Uninstall;
  protected
    /// <summary>Clears theme references when they are removed.</summary>
    /// <param name="AComponent">Removed component.</param>
    /// <param name="Operation">Notification operation.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Creates an active hint style.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Restores the previous hint window class when needed.</summary>
    destructor Destroy; override;
    /// <summary>Receives theme changes.</summary>
    procedure ThemeChanged;
    /// <summary>Palette currently used by hint windows.</summary>
    /// <returns>Resolved palette.</returns>
    property Palette: TERDThemePalette read GetPalette;
  published
    /// <summary>Optional explicit theme. nil = default theme or VCL style.</summary>
    property Theme: TERDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density: TERDDensity read FDensity write SetDensity
      default dnDesktop;
    /// <summary>Installs the themed hint window at run time.</summary>
    property Active: Boolean read FActive write SetActive default True;
  end;

implementation

var
  GActiveHintStyle: TERDHintStyle = nil;
  GPreviousHintWindowClass: THintWindowClass = nil;

function FallbackPalette: TERDThemePalette;
begin
  Result := BRAND_PALETTE_LIGHT;
end;

{ TERDHintWindow }

constructor TERDHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clNone;
  StyleElements := [];
end;

procedure TERDHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // Keep the inherited popup shadow; replace only the native raised frame.
  Params.Style := Params.Style and not (WS_BORDER or WS_DLGFRAME or WS_THICKFRAME);
  Params.ExStyle := Params.ExStyle and not (WS_EX_CLIENTEDGE or
    WS_EX_STATICEDGE or WS_EX_WINDOWEDGE or WS_EX_DLGMODALFRAME);
end;

procedure TERDHintWindow.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  // Paint the single flat border in the client area, without a native inset.
  Message.Result := 0;
end;

function TERDHintWindow.ActivePalette: TERDThemePalette;
begin
  if GActiveHintStyle <> nil then
    Result := GActiveHintStyle.Palette
  else
    Result := FallbackPalette;
end;

function TERDHintWindow.ActiveDensity: TERDDensity;
begin
  if GActiveHintStyle <> nil then
    Result := GActiveHintStyle.Density
  else
    Result := dnDesktop;
end;

function TERDHintWindow.TextWidth(const AText: string; ASize: Single;
  AWeight: TERDTextWeight): Integer;
begin
  Result := ERDMeasureText(AText, ASize, AWeight, Screen.PixelsPerInch);
end;

procedure TERDHintWindow.ParseHint(const AHint: string; AData: Pointer);
var
  S: string;
  P, T: Integer;
  Control: TControl;
  Action: TCustomAction;
begin
  FTitle := AHint;
  FBody := '';
  FShortcut := '';

  P := Pos('|', FTitle);
  if P > 0 then
  begin
    FBody := Copy(FTitle, P + 1, MaxInt);
    FTitle := Copy(FTitle, 1, P - 1);
  end;

  T := Pos(#9, FTitle);
  if T > 0 then
  begin
    FShortcut := Copy(FTitle, T + 1, MaxInt);
    FTitle := Copy(FTitle, 1, T - 1);
  end;

  // AData is the control's HintInfo.HintData (normally nil), not a
  // PHintInfo, so never dereference it. Take the shortcut from the action of
  // the control under the mouse instead.
  if FShortcut = '' then
  begin
    Control := FindDragTarget(Mouse.CursorPos, True);
    if (Control <> nil) and (Control.Action is TCustomAction) then
    begin
      Action := TCustomAction(Control.Action);
      FShortcut := ShortCutToText(Action.ShortCut);
    end;
  end;

  S := Trim(FTitle);
  if S <> '' then
    FTitle := S;
  S := Trim(FBody);
  if S <> '' then
    FBody := S;
  FShortcut := Trim(FShortcut);
end;

function TERDHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;
var
  W, H, TitleW, BodyW, ShortcutW, Limit: Integer;
  Metrics: TERDDensityMetrics;
begin
  ParseHint(AHint, AData);
  Metrics := DensityMetrics(ActiveDensity);
  TitleW := TextWidth(FTitle, 12.5, twBold);
  BodyW := TextWidth(FBody, 12, twRegular);
  ShortcutW := TextWidth(FShortcut, 11.5, twRegular);
  W := System.Math.Max(TitleW + ShortcutW + 24, BodyW) + 24;
  Limit := MaxWidth;
  if Limit <= 0 then
    Limit := 360;
  W := EnsureRange(W, 96, Limit);
  if FBody <> '' then
    H := 52
  else
    H := 34;
  if Metrics.Button > 30 then
    H := H + (Metrics.Button - 30) div 2;
  Result := Rect(0, 0, W, H);
end;

procedure TERDHintWindow.ActivateHintData(Rect: TRect; const AHint: string;
  AData: Pointer);
begin
  ParseHint(AHint, AData);
  inherited ActivateHintData(Rect, AHint, AData);
end;

procedure TERDHintWindow.WMNCPaint(var Message: TWMNCPaint);
begin
  Message.Result := 0;
end;

procedure TERDHintWindow.Paint;
var
  Painter: TERDPainter;
  P: TERDThemePalette;
  R: TRect;
  YTitle, YBody, ShortcutW: Integer;
  MaxTitleW: Integer;
begin
  P := ActivePalette;
  Canvas.Brush.Color := P.GaugeFace;
  Canvas.FillRect(ClientRect);
  Painter := TERDPainter.Create(Canvas, P, Screen.PixelsPerInch);
  try
    R := ClientRect;
    Painter.FillRect(R, P.GaugeFace);
    Painter.FrameRect(R, P.NeutralLight);
    YTitle := 16;
    if FBody = '' then
      YTitle := R.Height div 2;
    ShortcutW := 0;
    if FShortcut <> '' then
      ShortcutW := Painter.TextWidth(FShortcut, 11.5, twRegular) + 8;
    MaxTitleW := R.Width - 24 - ShortcutW;
    Painter.Text(12, YTitle, FTitle, 12.5, P.ForegroundText, twBold,
      taLeftJustify, MaxTitleW);
    if FShortcut <> '' then
      Painter.Text(R.Right - 12, YTitle, FShortcut, 11.5, P.GaugeLabel,
        twRegular, taRightJustify);
    if FBody <> '' then
    begin
      YBody := 36;
      Painter.Text(12, YBody, FBody, ERD_TEXT_BODY, P.GaugeLabel, twRegular,
        taLeftJustify, R.Width - 24);
    end;
  finally
    Painter.Free;
  end;
end;

{ TERDHintStyle }

constructor TERDHintStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
  FActive := True;
  if not (csDesigning in ComponentState) then
    Install;
end;

destructor TERDHintStyle.Destroy;
begin
  Uninstall;
  SetTheme(nil);
  inherited;
end;

procedure TERDHintStyle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TERDHintStyle.ThemeChanged;
begin
end;

function TERDHintStyle.GetPalette: TERDThemePalette;
begin
  if FTheme <> nil then
    Result := FTheme.Palette
  else if TERDTheme.GetDefault <> nil then
    Result := TERDTheme.GetDefault.Palette
  else
    Result := FallbackPalette;
end;

procedure TERDHintStyle.SetTheme(AValue: TERDTheme);
begin
  if FTheme = AValue then
    Exit;
  if FTheme <> nil then
  begin
    FTheme.Detach(Self);
    FTheme.RemoveFreeNotification(Self);
  end;
  FTheme := AValue;
  if FTheme <> nil then
  begin
    FTheme.FreeNotification(Self);
    FTheme.Attach(Self);
  end;
end;

procedure TERDHintStyle.SetDensity(AValue: TERDDensity);
begin
  FDensity := AValue;
end;

procedure TERDHintStyle.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if FActive then
    Install
  else
    Uninstall;
end;

procedure RecreateHintWindow;
begin
  // Toggling ShowHint frees the application's hint window and creates one of
  // the current HintWindowClass. Never do that while the application shuts
  // down: the main form (and this component) is freed inside
  // Application.DestroyComponents, after DoneApplication already switched
  // ShowHint off. Switching it back on there would create a hint window that
  // DestroyComponents frees while Application still references it, which
  // ends in "Invalid pointer operation" on exit.
  if (Application = nil) or (csDestroying in Application.ComponentState) or
    Application.Terminated or not Application.ShowHint then
    Exit;
  Application.ShowHint := False;
  Application.ShowHint := True;
end;

procedure TERDHintStyle.Install;
begin
  if csDesigning in ComponentState then
    Exit;
  if GPreviousHintWindowClass = nil then
    GPreviousHintWindowClass := HintWindowClass;
  GActiveHintStyle := Self;
  HintWindowClass := TERDHintWindow;
  RecreateHintWindow;
end;

procedure TERDHintStyle.Uninstall;
begin
  if GActiveHintStyle <> Self then
    Exit;
  GActiveHintStyle := nil;
  if GPreviousHintWindowClass <> nil then
    HintWindowClass := GPreviousHintWindowClass;
  GPreviousHintWindowClass := nil;
  RecreateHintWindow;
end;

end.
