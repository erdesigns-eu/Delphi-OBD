//------------------------------------------------------------------------------
//  ERD.UI.SecurityAccessCard
//
//  TOBDSecurityAccessCard - OBD Studio Pro card that shows security-access
//  lock state, diagnostic session, security level and tester-present keep-alive.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.SecurityAccessCard;

interface

uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Security-access state shown by the card.</summary>
  TOBDSecurityAccessCardState = (
    /// <summary>The ECU is locked or no unlock was requested.</summary>
    sacLocked,
    /// <summary>A seed/key exchange is in progress.</summary>
    sacUnlocking,
    /// <summary>The ECU accepted the key and security access is active.</summary>
    sacUnlocked,
    /// <summary>The ECU denied the key or the access level.</summary>
    sacDenied,
    /// <summary>The unlock attempt failed for another reason.</summary>
    sacError);

  /// <summary>OBD Studio Pro security-access status card.</summary>
  TOBDSecurityAccessCard = class(TOBDCustomControl, IOBDSurface)
  strict private
    FState: TOBDSecurityAccessCardState;
    FControlUnit: string;
    FSessionText: string;
    FLevelText: string;
    FKeepAliveText: string;
    FKeepAliveProgress: Double;
    FHasData: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetState(AValue: TOBDSecurityAccessCardState);
    procedure SetControlUnit(const AValue: string);
    procedure SetSessionText(const AValue: string);
    procedure SetLevelText(const AValue: string);
    procedure SetKeepAliveText(const AValue: string);
    procedure SetKeepAliveProgress(AValue: Double);
    procedure DoChange;
    function UsePreviewData: Boolean;
    function VisualState: TOBDSecurityAccessCardState;
    function VisualControlUnit: string;
    function VisualSessionText: string;
    function VisualLevelText: string;
    function VisualKeepAliveText: string;
    function VisualKeepAliveProgress: Double;
    function GetUnlocked: Boolean;
    function HeaderHeight: Integer;
    function StateColor(var P: TOBDPainter;
      AState: TOBDSecurityAccessCardState): TColor;
    function StateText(AState: TOBDSecurityAccessCardState): string;
    procedure PaintHeader(var P: TOBDPainter; const R: TRect);
    procedure PaintKeyValue(var P: TOBDPainter; X, Y, W: Integer;
      const AKey, AValue: string; AValueColor: TColor = clDefault);
    procedure PaintBody(var P: TOBDPainter; const R: TRect);
  protected
    /// <summary>Paints the security-access card.</summary>
    /// <param name="ACanvas">Canvas to paint to.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates the card with locked defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Repaints the card after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card face used by child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Clears all live security-access data.</summary>
    procedure Clear;
    /// <summary>True when any live data was assigned.</summary>
    property HasData: Boolean read FHasData;
    /// <summary>True when the state is unlocked.</summary>
    property Unlocked: Boolean read GetUnlocked;
  published
    /// <summary>Current security-access state.</summary>
    property State: TOBDSecurityAccessCardState read FState write SetState
      default sacLocked;
    /// <summary>Control unit for which access was requested.</summary>
    property ControlUnit: string read FControlUnit write SetControlUnit;
    /// <summary>Diagnostic session text.</summary>
    property SessionText: string read FSessionText write SetSessionText;
    /// <summary>Security level text.</summary>
    property LevelText: string read FLevelText write SetLevelText;
    /// <summary>Tester-present keep-alive text.</summary>
    property KeepAliveText: string read FKeepAliveText write SetKeepAliveText;
    /// <summary>Keep-alive progress, clamped to 0..1.</summary>
    property KeepAliveProgress: Double read FKeepAliveProgress
      write SetKeepAliveProgress;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires after the card data changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TOBDSecurityAccessCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := sacLocked;
  FKeepAliveProgress := 0;
  Width := 520;
  Height := HeaderHeight + ScaleValue(128);
end;

procedure TOBDSecurityAccessCard.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TOBDSecurityAccessCard.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TOBDSecurityAccessCard.Clear;
begin
  FState := sacLocked;
  FControlUnit := '';
  FSessionText := '';
  FLevelText := '';
  FKeepAliveText := '';
  FKeepAliveProgress := 0;
  FHasData := False;
  DoChange;
end;

function TOBDSecurityAccessCard.GetUnlocked: Boolean;
begin
  Result := FState = sacUnlocked;
end;

procedure TOBDSecurityAccessCard.DoChange;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDSecurityAccessCard.SetState(AValue: TOBDSecurityAccessCardState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDSecurityAccessCard.SetControlUnit(const AValue: string);
begin
  if FControlUnit = AValue then
    Exit;
  FControlUnit := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDSecurityAccessCard.SetSessionText(const AValue: string);
begin
  if FSessionText = AValue then
    Exit;
  FSessionText := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDSecurityAccessCard.SetLevelText(const AValue: string);
begin
  if FLevelText = AValue then
    Exit;
  FLevelText := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDSecurityAccessCard.SetKeepAliveText(const AValue: string);
begin
  if FKeepAliveText = AValue then
    Exit;
  FKeepAliveText := AValue;
  FHasData := True;
  DoChange;
end;

procedure TOBDSecurityAccessCard.SetKeepAliveProgress(AValue: Double);
begin
  AValue := EnsureRange(AValue, 0, 1);
  if SameValue(FKeepAliveProgress, AValue) then
    Exit;
  FKeepAliveProgress := AValue;
  FHasData := True;
  DoChange;
end;

function TOBDSecurityAccessCard.UsePreviewData: Boolean;
begin
  Result := IsPreview and not FHasData;
end;

function TOBDSecurityAccessCard.VisualState: TOBDSecurityAccessCardState;
begin
  if UsePreviewData then
    Result := sacUnlocked
  else
    Result := FState;
end;

function TOBDSecurityAccessCard.VisualControlUnit: string;
begin
  Result := FControlUnit;
  if Result = '' then
  begin
    if UsePreviewData then
      Result := '09 Central electrics'
    else
      Result := 'No security access session';
  end;
end;

function TOBDSecurityAccessCard.VisualSessionText: string;
begin
  Result := FSessionText;
  if Result = '' then
  begin
    if UsePreviewData then
      Result := 'Extended (0x03)'
    else
      Result := '-';
  end;
end;

function TOBDSecurityAccessCard.VisualLevelText: string;
begin
  Result := FLevelText;
  if Result = '' then
  begin
    if UsePreviewData then
      Result := '0x03 / 0x04 ' + #$00B7 + ' seed-key'
    else
      Result := '-';
  end;
end;

function TOBDSecurityAccessCard.VisualKeepAliveText: string;
begin
  Result := FKeepAliveText;
  if Result = '' then
  begin
    if UsePreviewData then
      Result := 'Tester present ' + #$00B7 + ' 2 s'
    else
      Result := 'Not running';
  end;
end;

function TOBDSecurityAccessCard.VisualKeepAliveProgress: Double;
begin
  if UsePreviewData then
    Result := 0.7
  else
    Result := FKeepAliveProgress;
end;

function TOBDSecurityAccessCard.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDSecurityAccessCard.StateColor(var P: TOBDPainter;
  AState: TOBDSecurityAccessCardState): TColor;
begin
  case AState of
    sacUnlocked:
      Result := Palette.Success;
    sacUnlocking:
      Result := P.AccentText;
    sacDenied, sacError:
      Result := Palette.Danger;
  else
    Result := Palette.Subtle;
  end;
end;

function TOBDSecurityAccessCard.StateText(
  AState: TOBDSecurityAccessCardState): string;
begin
  case AState of
    sacUnlocking:
      Result := 'UNLOCKING';
    sacUnlocked:
      Result := 'UNLOCKED';
    sacDenied:
      Result := 'DENIED';
    sacError:
      Result := 'ERROR';
  else
    Result := 'LOCKED';
  end;
end;

procedure TOBDSecurityAccessCard.PaintHeader(var P: TOBDPainter;
  const R: TRect);
var
  State: TOBDSecurityAccessCardState;
  Icon: TOBDGlyph;
  X, Y, TextW, ChipW: Integer;
  ChipText: string;
  Color: TColor;
begin
  State := VisualState;
  if State = sacUnlocked then
    Icon := glUnlock
  else
    Icon := glLock;
  X := R.Left + P.S(16);
  Y := R.Top + R.Height div 2;
  P.Glyph(Icon, X + P.S(8), Y, P.AccentText, 1);
  TextW := P.Text(X + P.S(26), Y, 'Security access', OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold, taLeftJustify, R.Width - P.S(150));
  ChipText := StateText(State);
  Color := StateColor(P, State);
  ChipW := P.ChipWidth(ChipText);
  if X + P.S(26) + TextW + P.S(14) + ChipW < R.Right - P.S(16) then
    P.Chip(X + P.S(26) + TextW + P.S(14), Y - P.S(10), ChipText, Color);
  P.HLine(R.Left + P.S(1), R.Bottom - P.S(1), R.Width - P.S(2),
    Palette.NeutralLight);
end;

procedure TOBDSecurityAccessCard.PaintKeyValue(var P: TOBDPainter; X, Y,
  W: Integer; const AKey, AValue: string; AValueColor: TColor);
var
  Ink: TColor;
begin
  if AValueColor = clDefault then
    Ink := Palette.ForegroundText
  else
    Ink := AValueColor;
  P.Text(X, Y, AKey, OBD_TEXT_BODY, Palette.GaugeLabel, twRegular,
    taLeftJustify, W div 2);
  P.Text(X + W, Y, AValue, OBD_TEXT_BODY, Ink, twSemibold, taRightJustify,
    W div 2);
end;

procedure TOBDSecurityAccessCard.PaintBody(var P: TOBDPainter; const R: TRect);
var
  L, W, Y, TrackH, FillW: Integer;
  Color: TColor;
begin
  L := R.Left + P.S(20);
  W := R.Width - P.S(40);
  Y := R.Top + P.S(20);
  Color := StateColor(P, VisualState);
  PaintKeyValue(P, L, Y, W, 'Control unit', VisualControlUnit);
  PaintKeyValue(P, L, Y + P.S(24), W, 'Session', VisualSessionText);
  PaintKeyValue(P, L, Y + P.S(48), W, 'Level', VisualLevelText, Color);
  PaintKeyValue(P, L, Y + P.S(72), W, 'Keep-alive', VisualKeepAliveText);
  TrackH := P.S(4);
  P.FillRect(Rect(L, Y + P.S(92), L + W, Y + P.S(92) + TrackH),
    Palette.NeutralLight);
  FillW := Round(W * VisualKeepAliveProgress);
  if FillW > 0 then
    P.FillRect(Rect(L, Y + P.S(92), L + FillW, Y + P.S(92) + TrackH), Color);
end;

procedure TOBDSecurityAccessCard.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Edge: TColor;
  H: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if VisualState = sacUnlocked then
      Edge := Palette.Success
    else
      Edge := clNone;
    P.Card(Rect(0, 0, Width, Height), Edge, SurfaceColor);
    H := HeaderHeight;
    PaintHeader(P, Rect(0, 0, Width, H));
    PaintBody(P, Rect(0, H, Width, Height));
  finally
    P.Free;
  end;
end;

end.
