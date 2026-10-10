//------------------------------------------------------------------------------
//  ERD.UI.Toast
//
//  Non-activating themed toast notifications for OBD Studio.
//
//    TOBDToastManager  creates top-most no-activate popup toast windows,
//                      stacks them near the owner form or monitor work area,
//                      tracks duration and raises action / dismiss events.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.Toast;

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
  Vcl.ExtCtrls,
  Vcl.Themes,
  ERD.UI.Types,
  ERD.UI.Theme,
  ERD.UI.Paint;

type
  /// <summary>Toast severity and accent colour.</summary>
  TOBDToastKind = (
    /// <summary>Information toast.</summary>
    tkInfo,
    /// <summary>Success toast.</summary>
    tkSuccess,
    /// <summary>Warning toast.</summary>
    tkWarning,
    /// <summary>Danger toast.</summary>
    tkDanger);

  /// <summary>Toast stack anchor.</summary>
  TOBDToastPosition = (
    /// <summary>Bottom right of the owner client area or work area.</summary>
    tpBottomRight,
    /// <summary>Top right of the owner client area or work area.</summary>
    tpTopRight,
    /// <summary>Bottom centre of the owner client area or work area.</summary>
    tpBottomCenter);

  /// <summary>Toast event carrying the toast identifier.</summary>
  /// <param name="Sender">Toast manager.</param>
  /// <param name="AId">Toast identifier.</param>
  TOBDToastEvent = procedure(Sender: TObject; AId: Integer) of object;

  /// <summary>Creates and manages themed toast notifications.</summary>
  TOBDToastManager = class(TComponent, IOBDThemeAware)
  strict private
    FTheme: TOBDTheme;
    FDensity: TOBDDensity;
    FPosition: TOBDToastPosition;
    FDuration: Cardinal;
    FMaxVisible: Integer;
    FMargin: Integer;
    FNextId: Integer;
    FToasts: TList;
    FOnAction: TOBDToastEvent;
    FOnDismiss: TOBDToastEvent;
    procedure SetTheme(AValue: TOBDTheme);
    procedure SetDensity(AValue: TOBDDensity);
    procedure SetPosition(AValue: TOBDToastPosition);
    procedure SetDuration(AValue: Cardinal);
    procedure SetMaxVisible(AValue: Integer);
    procedure SetMargin(AValue: Integer);
    function GetPalette: TOBDThemePalette;
    function FindToast(AId: Integer): Integer;
    procedure ToastAction(AId: Integer; AOnAction: TNotifyEvent);
    procedure ToastDismissed(AId: Integer);
  protected
    /// <summary>Clears theme references when they are removed.</summary>
    /// <param name="AComponent">Removed component.</param>
    /// <param name="Operation">Notification operation.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Creates the manager with a 5000 ms duration.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Dismisses all toasts and releases resources.</summary>
    destructor Destroy; override;
    /// <summary>Receives theme changes and repaints visible toasts.</summary>
    procedure ThemeChanged;
    /// <summary>Shows a toast and returns its identifier.</summary>
    /// <param name="AKind">Toast kind.</param>
    /// <param name="ATitle">Bold title.</param>
    /// <param name="AText">Body text.</param>
    /// <param name="AActionCaption">Optional action link caption.</param>
    /// <param name="AOnAction">Optional per-toast action handler.</param>
    /// <returns>Toast identifier.</returns>
    function Show(AKind: TOBDToastKind; const ATitle, AText: string;
      const AActionCaption: string = ''; AOnAction: TNotifyEvent = nil)
      : Integer;
    /// <summary>Dismisses one toast by identifier.</summary>
    /// <param name="AId">Toast identifier.</param>
    procedure Dismiss(AId: Integer);
    /// <summary>Dismisses every visible toast.</summary>
    procedure DismissAll;
    /// <summary>Repositions all visible toasts.</summary>
    procedure Restack;
    /// <summary>Palette currently used by toast windows.</summary>
    /// <returns>Resolved palette.</returns>
    property Palette: TOBDThemePalette read GetPalette;
  published
    /// <summary>Optional explicit theme. nil = default theme or VCL style.</summary>
    property Theme: TOBDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density: TOBDDensity read FDensity write SetDensity
      default dnDesktop;
    /// <summary>Toast stack anchor.</summary>
    property Position: TOBDToastPosition read FPosition write SetPosition
      default tpBottomRight;
    /// <summary>Visible duration in milliseconds; 0 keeps toasts sticky.</summary>
    property Duration: Cardinal read FDuration write SetDuration default 5000;
    /// <summary>Maximum number of visible toasts.</summary>
    property MaxVisible: Integer read FMaxVisible write SetMaxVisible default 3;
    /// <summary>Distance from the owner area or work area edge.</summary>
    property Margin: Integer read FMargin write SetMargin default 16;
    /// <summary>Fires when an action link is clicked.</summary>
    property OnAction: TOBDToastEvent read FOnAction write FOnAction;
    /// <summary>Fires after a toast is dismissed.</summary>
    property OnDismiss: TOBDToastEvent read FOnDismiss write FOnDismiss;
  end;

implementation

type
  TToastWindow = class(TCustomControl)
  strict private
    FManager: TOBDToastManager;
    FId: Integer;
    FKind: TOBDToastKind;
    FTitle: string;
    FText: string;
    FActionCaption: string;
    FOnAction: TNotifyEvent;
    FDuration: Cardinal;
    FElapsed: Cardinal;
    FTimer: TTimer;
    FHover: Boolean;
    FClosing: Boolean;
    function AccentColor(const P: TOBDThemePalette): TColor;
    function ActionRect: TRect;
    function CloseRect: TRect;
    procedure TimerTick(Sender: TObject);
    procedure WMMouseActivate(var Message: TWMMouseActivate);
      message WM_MOUSEACTIVATE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
  public
    constructor CreateToast(AOwner: TComponent; AManager: TOBDToastManager;
      AId: Integer; AKind: TOBDToastKind; const ATitle, AText,
      AActionCaption: string; AOnAction: TNotifyEvent; ADuration: Cardinal);
    procedure ShowNoActivate(const R: TRect);
    property Id: Integer read FId;
  end;

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

{ TToastWindow }

constructor TToastWindow.CreateToast(AOwner: TComponent;
  AManager: TOBDToastManager; AId: Integer; AKind: TOBDToastKind;
  const ATitle, AText, AActionCaption: string; AOnAction: TNotifyEvent;
  ADuration: Cardinal);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FManager := AManager;
  FId := AId;
  FKind := AKind;
  FTitle := ATitle;
  FText := AText;
  FActionCaption := AActionCaption;
  FOnAction := AOnAction;
  FDuration := ADuration;
  Width := 340;
  Height := 64;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 40;
  FTimer.OnTimer := TimerTick;
  FTimer.Enabled := FDuration > 0;
end;

procedure TToastWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style and not WS_CHILD) or WS_POPUP or
    WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOACTIVATE or WS_EX_TOOLWINDOW or
    WS_EX_TOPMOST;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS or
    CS_DROPSHADOW;
end;

procedure TToastWindow.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TToastWindow.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHover := True;
  Invalidate;
end;

procedure TToastWindow.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHover := False;
  Invalidate;
end;

function TToastWindow.AccentColor(const P: TOBDThemePalette): TColor;
begin
  case FKind of
    tkSuccess:
      Result := P.Success;
    tkWarning:
      Result := P.Warning;
    tkDanger:
      Result := P.Danger;
  else
    Result := P.Accent;
  end;
end;

function TToastWindow.ActionRect: TRect;
var
  P: TOBDThemePalette;
  Painter: TOBDPainter;
  B: TBitmap;
  W: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if FActionCaption = '' then
    Exit;
  P := FManager.Palette;
  B := TBitmap.Create;
  try
    B.SetSize(1, 1);
    Painter := TOBDPainter.Create(B.Canvas, P, Screen.PixelsPerInch);
    try
      W := Painter.TextWidth(FActionCaption, 12, twSemibold) + 8;
    finally
      Painter.Free;
    end;
  finally
    B.Free;
  end;
  Result := Rect(Width - 14 - W, 32, Width - 10, Height - 6);
end;

function TToastWindow.CloseRect: TRect;
begin
  Result := Rect(Width - 32, 4, Width - 4, 32);
end;

procedure TToastWindow.ShowNoActivate(const R: TRect);
begin
  SetBounds(R.Left, R.Top, R.Width, R.Height);
  HandleNeeded;
  SetWindowPos(Handle, HWND_TOPMOST, R.Left, R.Top, R.Width, R.Height,
    SWP_NOACTIVATE or SWP_SHOWWINDOW);
  Invalidate;
end;

procedure TToastWindow.TimerTick(Sender: TObject);
begin
  if FHover or FClosing then
    Exit;
  Inc(FElapsed, FTimer.Interval);
  Invalidate;
  if (FDuration > 0) and (FElapsed >= FDuration) then
  begin
    FClosing := True;
    FManager.Dismiss(FId);
  end;
end;

procedure TToastWindow.Paint;
var
  P: TOBDThemePalette;
  Painter: TOBDPainter;
  C, Ink: TColor;
  R, A: TRect;
  TimerW: Integer;
  Ratio: Double;
begin
  P := FManager.Palette;
  C := AccentColor(P);
  Painter := TOBDPainter.Create(Canvas, P, Screen.PixelsPerInch);
  try
    R := ClientRect;
    Painter.FillRect(R, P.GaugeFace);
    Painter.FrameRect(R, P.NeutralLight);
    Painter.FillRect(Rect(0, 0, 4, Height), C);
    case FKind of
      tkSuccess:
        Painter.GlyphCheck(24, 22, C, 1.1);
      tkDanger:
        Painter.Text(24, 23, '!', 16, C, twBold, taCenter);
      tkWarning:
        Painter.GlyphPending(24, 22, C, 1.1);
    else
      Painter.Text(24, 23, 'i', 16, C, twBold, taCenter);
    end;
    Painter.Text(44, 22, FTitle, 13, P.ForegroundText, twBold,
      taLeftJustify, Width - 86);
    Painter.Text(44, 44, FText, 12, P.GaugeLabel, twRegular,
      taLeftJustify, Width - 120);
    Ink := P.Subtle;
    if FHover and PtInRect(CloseRect, ScreenToClient(Mouse.CursorPos)) then
      Ink := P.ForegroundText;
    Painter.Lines([MakePoint(Width - 22, 14), MakePoint(Width - 14, 22)],
      Ink, 1.6);
    Painter.Lines([MakePoint(Width - 14, 14), MakePoint(Width - 22, 22)],
      Ink, 1.6);
    A := ActionRect;
    if not A.IsEmpty then
      Painter.Text(A.Right - 4, 44, FActionCaption, 12, Painter.AccentText,
        twSemibold, taRightJustify);
    if FDuration > 0 then
    begin
      Ratio := 1 - EnsureRange(FElapsed / FDuration, 0, 1);
      TimerW := Round((Width - 4) * Ratio);
      Painter.FillRect(Rect(4, Height - 3, 4 + TimerW, Height - 1), C);
    end;
  finally
    Painter.Free;
  end;
end;

procedure TToastWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  if PtInRect(CloseRect, Point(X, Y)) then
    FManager.Dismiss(FId)
  else if PtInRect(ActionRect, Point(X, Y)) then
    FManager.ToastAction(FId, FOnAction);
end;

{ TOBDToastManager }

constructor TOBDToastManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
  FPosition := tpBottomRight;
  FDuration := 5000;
  FMaxVisible := 3;
  FMargin := 16;
  FNextId := 1;
  FToasts := TList.Create;
end;

destructor TOBDToastManager.Destroy;
begin
  DismissAll;
  FToasts.Free;
  SetTheme(nil);
  inherited;
end;

procedure TOBDToastManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TOBDToastManager.ThemeChanged;
var
  I: Integer;
begin
  for I := 0 to FToasts.Count - 1 do
    TToastWindow(FToasts[I]).Invalidate;
end;

function TOBDToastManager.GetPalette: TOBDThemePalette;
begin
  if FTheme <> nil then
    Result := FTheme.Palette
  else if TOBDTheme.GetDefault <> nil then
    Result := TOBDTheme.GetDefault.Palette
  else
    Result := FallbackPalette;
end;

procedure TOBDToastManager.SetTheme(AValue: TOBDTheme);
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
  ThemeChanged;
end;

procedure TOBDToastManager.SetDensity(AValue: TOBDDensity);
begin
  if FDensity = AValue then
    Exit;
  FDensity := AValue;
  Restack;
end;

procedure TOBDToastManager.SetPosition(AValue: TOBDToastPosition);
begin
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  Restack;
end;

procedure TOBDToastManager.SetDuration(AValue: Cardinal);
begin
  FDuration := AValue;
end;

procedure TOBDToastManager.SetMaxVisible(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FMaxVisible = AValue then
    Exit;
  FMaxVisible := AValue;
  while FToasts.Count > FMaxVisible do
    Dismiss(TToastWindow(FToasts[0]).Id);
  Restack;
end;

procedure TOBDToastManager.SetMargin(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FMargin = AValue then
    Exit;
  FMargin := AValue;
  Restack;
end;

function TOBDToastManager.FindToast(AId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FToasts.Count - 1 do
    if TToastWindow(FToasts[I]).Id = AId then
      Exit(I);
end;

function TOBDToastManager.Show(AKind: TOBDToastKind; const ATitle,
  AText: string; const AActionCaption: string; AOnAction: TNotifyEvent)
  : Integer;
var
  Toast: TToastWindow;
begin
  Result := FNextId;
  Inc(FNextId);
  Toast := TToastWindow.CreateToast(nil, Self, Result, AKind, ATitle, AText,
    AActionCaption, AOnAction, FDuration);
  FToasts.Add(Toast);
  while FToasts.Count > FMaxVisible do
    Dismiss(TToastWindow(FToasts[0]).Id);
  Restack;
end;

procedure TOBDToastManager.Dismiss(AId: Integer);
var
  I: Integer;
  Toast: TToastWindow;
begin
  I := FindToast(AId);
  if I < 0 then
    Exit;
  Toast := TToastWindow(FToasts[I]);
  FToasts.Delete(I);
  if Toast.HandleAllocated then
    ShowWindow(Toast.Handle, SW_HIDE);
  Toast.Free;
  ToastDismissed(AId);
  Restack;
end;

procedure TOBDToastManager.DismissAll;
begin
  while FToasts.Count > 0 do
    Dismiss(TToastWindow(FToasts[FToasts.Count - 1]).Id);
end;

procedure TOBDToastManager.ToastAction(AId: Integer; AOnAction: TNotifyEvent);
begin
  if Assigned(AOnAction) then
    AOnAction(Self);
  if Assigned(FOnAction) then
    FOnAction(Self, AId);
end;

procedure TOBDToastManager.ToastDismissed(AId: Integer);
begin
  if Assigned(FOnDismiss) then
    FOnDismiss(Self, AId);
end;

procedure TOBDToastManager.Restack;
var
  Area: TRect;
  OwnerForm: TCustomForm;
  I, W, H, Gap, X, Y: Integer;
  Mon: TMonitor;
  R: TRect;
  Metrics: TOBDDensityMetrics;
begin
  if FToasts.Count = 0 then
    Exit;

  OwnerForm := nil;
  if Owner is TCustomForm then
    OwnerForm := TCustomForm(Owner)
  else if Application.MainForm <> nil then
    OwnerForm := Application.MainForm;

  if (OwnerForm <> nil) and OwnerForm.Visible then
    Area := OwnerForm.ClientRect
  else
    Area := Rect(0, 0, 0, 0);

  if (OwnerForm <> nil) and OwnerForm.Visible then
  begin
    Area.TopLeft := OwnerForm.ClientToScreen(Area.TopLeft);
    Area.BottomRight := OwnerForm.ClientToScreen(Area.BottomRight);
  end
  else
  begin
    Mon := Screen.MonitorFromWindow(Application.Handle, mdNearest);
    if Mon <> nil then
      Area := Mon.WorkareaRect
    else
      Area := Screen.DesktopRect;
  end;

  Metrics := DensityMetrics(FDensity);
  W := 340;
  if Metrics.Button > 30 then
    W := 380;
  H := 64;
  Gap := 12;

  for I := 0 to FToasts.Count - 1 do
  begin
    case FPosition of
      tpTopRight:
        begin
          X := Area.Right - FMargin - W;
          Y := Area.Top + FMargin + I * (H + Gap);
        end;
      tpBottomCenter:
        begin
          X := Area.Left + (Area.Width - W) div 2;
          Y := Area.Bottom - FMargin - H - (FToasts.Count - 1 - I) *
            (H + Gap);
        end;
    else
      begin
        X := Area.Right - FMargin - W;
        Y := Area.Bottom - FMargin - H - (FToasts.Count - 1 - I) *
          (H + Gap);
      end;
    end;
    R := Rect(X, Y, X + W, Y + H);
    TToastWindow(FToasts[I]).ShowNoActivate(R);
  end;
end;

end.
