//------------------------------------------------------------------------------
//  ERD.UI.Hint
//
//  Themed hint windows for OBD Studio controls.
//
//    TOBDHintWindow   THintWindow descendant that draws a card with a bold
//                     title, optional body text and an optional shortcut chip.
//    TOBDHintStyle    non-visual component that installs TOBDHintWindow for
//                     the application while active.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.Hint;

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
  ERD.UI.Types,
  ERD.UI.Theme,
  ERD.UI.Paint;

type
  /// <summary>Themed hint window with title, body and shortcut text.</summary>
  TOBDHintWindow = class(THintWindow)
  strict private
    FTitle: string;
    FBody: string;
    FShortcut: string;
    procedure ParseHint(const AHint: string; AData: Pointer);
    function ActivePalette: TOBDThemePalette;
    function ActiveDensity: TOBDDensity;
    function TextWidth(const AText: string; ASize: Single;
      AWeight: TOBDTextWeight): Integer;
  protected
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
    /// <summary>Draws the themed card.</summary>
    procedure Paint; override;
    /// <summary>Draws no native border.</summary>
    /// <param name="Message">Non-client paint message.</param>
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
  public
    /// <summary>Creates the themed hint window.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>Application-level themed hint installer.</summary>
  TOBDHintStyle = class(TComponent, IOBDThemeAware)
  strict private
    FTheme: TOBDTheme;
    FDensity: TOBDDensity;
    FActive: Boolean;
    procedure SetTheme(AValue: TOBDTheme);
    procedure SetDensity(AValue: TOBDDensity);
    procedure SetActive(AValue: Boolean);
    function GetPalette: TOBDThemePalette;
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
    property Palette: TOBDThemePalette read GetPalette;
  published
    /// <summary>Optional explicit theme. nil = default theme or VCL style.</summary>
    property Theme: TOBDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density: TOBDDensity read FDensity write SetDensity
      default dnDesktop;
    /// <summary>Installs the themed hint window at run time.</summary>
    property Active: Boolean read FActive write SetActive default True;
  end;

implementation

var
  GActiveHintStyle: TOBDHintStyle = nil;
  GPreviousHintWindowClass: THintWindowClass = nil;

function FallbackPalette: TOBDThemePalette;
begin
  if VCLStyleIsDark then
    Result := BRAND_PALETTE_DARK
  else
    Result := BRAND_PALETTE_LIGHT;
  if TStyleManager.IsCustomStyleActive then
  begin
    Result.Background := StyleColor(scWindow, Result.Background);
    Result.ForegroundText := StyleServices.GetSystemColor(clWindowText);
  end;
end;

{ TOBDHintWindow }

constructor TOBDHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clNone;
end;

function TOBDHintWindow.ActivePalette: TOBDThemePalette;
begin
  if GActiveHintStyle <> nil then
    Result := GActiveHintStyle.Palette
  else
    Result := FallbackPalette;
end;

function TOBDHintWindow.ActiveDensity: TOBDDensity;
begin
  if GActiveHintStyle <> nil then
    Result := GActiveHintStyle.Density
  else
    Result := dnDesktop;
end;

function TOBDHintWindow.TextWidth(const AText: string; ASize: Single;
  AWeight: TOBDTextWeight): Integer;
begin
  Result := OBDMeasureText(AText, ASize, AWeight, Screen.PixelsPerInch);
end;

procedure TOBDHintWindow.ParseHint(const AHint: string; AData: Pointer);
var
  S: string;
  P, T: Integer;
  Info: PHintInfo;
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

  if (FShortcut = '') and (AData <> nil) then
  begin
    Info := PHintInfo(AData);
    if (Info^.HintControl <> nil) and
      (Info^.HintControl.Action is TCustomAction) then
    begin
      Action := TCustomAction(Info^.HintControl.Action);
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

function TOBDHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;
var
  W, H, TitleW, BodyW, ShortcutW, Limit: Integer;
  Metrics: TOBDDensityMetrics;
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

procedure TOBDHintWindow.ActivateHintData(Rect: TRect; const AHint: string;
  AData: Pointer);
begin
  ParseHint(AHint, AData);
  inherited ActivateHintData(Rect, AHint, AData);
end;

procedure TOBDHintWindow.WMNCPaint(var Message: TWMNCPaint);
begin
  Message.Result := 0;
end;

procedure TOBDHintWindow.Paint;
var
  Painter: TOBDPainter;
  P: TOBDThemePalette;
  R: TRect;
  YTitle, YBody, ShortcutW: Integer;
  MaxTitleW: Integer;
begin
  P := ActivePalette;
  Canvas.Brush.Color := P.GaugeFace;
  Canvas.FillRect(ClientRect);
  Painter := TOBDPainter.Create(Canvas, P, Screen.PixelsPerInch);
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
      Painter.Text(12, YBody, FBody, 12, P.GaugeLabel, twRegular,
        taLeftJustify, R.Width - 24);
    end;
  finally
    Painter.Free;
  end;
end;

{ TOBDHintStyle }

constructor TOBDHintStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
  FActive := True;
  if not (csDesigning in ComponentState) then
    Install;
end;

destructor TOBDHintStyle.Destroy;
begin
  Uninstall;
  SetTheme(nil);
  inherited;
end;

procedure TOBDHintStyle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TOBDHintStyle.ThemeChanged;
begin
end;

function TOBDHintStyle.GetPalette: TOBDThemePalette;
begin
  if FTheme <> nil then
    Result := FTheme.Palette
  else if TOBDTheme.GetDefault <> nil then
    Result := TOBDTheme.GetDefault.Palette
  else
    Result := FallbackPalette;
end;

procedure TOBDHintStyle.SetTheme(AValue: TOBDTheme);
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

procedure TOBDHintStyle.SetDensity(AValue: TOBDDensity);
begin
  FDensity := AValue;
end;

procedure TOBDHintStyle.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if FActive then
    Install
  else
    Uninstall;
end;

procedure TOBDHintStyle.Install;
begin
  if csDesigning in ComponentState then
    Exit;
  if GPreviousHintWindowClass = nil then
    GPreviousHintWindowClass := HintWindowClass;
  GActiveHintStyle := Self;
  HintWindowClass := TOBDHintWindow;
  Application.ShowHint := False;
  Application.ShowHint := True;
end;

procedure TOBDHintStyle.Uninstall;
begin
  if GActiveHintStyle <> Self then
    Exit;
  GActiveHintStyle := nil;
  if GPreviousHintWindowClass <> nil then
    HintWindowClass := GPreviousHintWindowClass;
  GPreviousHintWindowClass := nil;
  Application.ShowHint := False;
  Application.ShowHint := True;
end;

end.
