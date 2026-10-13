//------------------------------------------------------------------------------
//  ERD.Components.ScrollBar
//
//  Thin themed overlay scroll bar for Application surfaces.
//
//    TERDScrollBar       horizontal or vertical overlay scroll bar with a
//                        faint track, widening thumb on hover, paging,
//                        dragging and mouse-wheel support.
//    ERDPaintScrollBar   shared painter for controls that draw their own
//                        scroll bars.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.ScrollBar;

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
  Vcl.StdCtrls,
  ERD.Components.Types,
  ERD.Components.Control,
  ERD.Components.Paint;

type
  /// <summary>Thin themed overlay scroll bar.</summary>
  TERDScrollBar = class(TERDCustomControl)
  strict private
    FKind: TScrollBarKind;
    FMin: Integer;
    FMax: Integer;
    FPageSize: Integer;
    FPosition: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;
    FHover: Boolean;
    FPressed: Boolean;
    FDragging: Boolean;
    FDragOffset: Integer;
    FOnChange: TNotifyEvent;
    FOnScroll: TScrollEvent;
    procedure SetKind(AValue: TScrollBarKind);
    procedure SetMin(AValue: Integer);
    procedure SetMax(AValue: Integer);
    procedure SetPageSize(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure SetSmallChange(AValue: Integer);
    procedure SetLargeChange(AValue: Integer);
    function IsVertical: Boolean;
    function MaxPosition: Integer;
    function CurrentThumbRect: TRect;
    function PositionFromPoint(X, Y: Integer): Integer;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the overlay track and thumb.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles keyboard scrolling.</summary>
    /// <param name="Key">Virtual key code.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Starts thumb dragging or pages on the track.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Moves the thumb while dragging.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends thumb dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Scrolls by <see cref="SmallChange"/>.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Fires OnScroll and applies the resulting position.</summary>
    /// <param name="ACode">Scroll code.</param>
    /// <param name="ANewPosition">Requested position.</param>
    procedure DoScroll(ACode: TScrollCode; ANewPosition: Integer);
    /// <summary>Fires OnChange after the position changes.</summary>
    procedure Change; virtual;
  public
    /// <summary>Creates a vertical overlay scroll bar.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Horizontal or vertical orientation.</summary>
    property Kind: TScrollBarKind read FKind write SetKind default sbVertical;
    /// <summary>Lowest scroll position.</summary>
    property Min: Integer read FMin write SetMin default 0;
    /// <summary>Highest logical range value.</summary>
    property Max: Integer read FMax write SetMax default 100;
    /// <summary>Visible page size in logical range units.</summary>
    property PageSize: Integer read FPageSize write SetPageSize default 10;
    /// <summary>Current scroll position.</summary>
    property Position: Integer read FPosition write SetPosition default 0;
    /// <summary>Delta for arrow keys and mouse wheel.</summary>
    property SmallChange: Integer read FSmallChange write SetSmallChange
      default 1;
    /// <summary>Delta for Page Up / Page Down and track clicks.</summary>
    property LargeChange: Integer read FLargeChange write SetLargeChange
      default 10;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires after Position changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires before a user scroll applies a new position.</summary>
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  end;

/// <summary>Calculates the themed scroll thumb rectangle.</summary>
/// <param name="ARect">Scroll bar bounds.</param>
/// <param name="AVertical">True for a vertical bar.</param>
/// <param name="AMin">Lowest position.</param>
/// <param name="AMax">Highest logical range value.</param>
/// <param name="APage">Visible page size.</param>
/// <param name="APos">Current position.</param>
/// <param name="AHot">True when the bar is hovered.</param>
/// <param name="APressed">True while dragging or pressing.</param>
/// <returns>Thumb bounds.</returns>
function ERDScrollThumbRect(const ARect: TRect; AVertical: Boolean; AMin,
  AMax, APage, APos: Integer; AHot, APressed: Boolean): TRect;

/// <summary>Paints a thin themed overlay scroll bar.</summary>
/// <param name="APainter">Painter configured with the target palette.</param>
/// <param name="ARect">Scroll bar bounds.</param>
/// <param name="AVertical">True for a vertical bar.</param>
/// <param name="AMin">Lowest position.</param>
/// <param name="AMax">Highest logical range value.</param>
/// <param name="APage">Visible page size.</param>
/// <param name="APos">Current position.</param>
/// <param name="AHot">True when the bar is hovered.</param>
/// <param name="APressed">True while dragging or pressing.</param>
procedure ERDPaintScrollBar(APainter: TERDPainter; const ARect: TRect;
  AVertical: Boolean; AMin, AMax, APage, APos: Integer; AHot,
  APressed: Boolean);

implementation

function ScrollMaxPosition(AMin, AMax, APage: Integer): Integer;
begin
  if APage > 0 then
    Result := AMax - APage + 1
  else
    Result := AMax;
  if Result < AMin then
    Result := AMin;
end;

function ERDScrollThumbRect(const ARect: TRect; AVertical: Boolean; AMin,
  AMax, APage, APos: Integer; AHot, APressed: Boolean): TRect;
var
  TrackLen, TrackWidth, ThumbLen, ThumbWidth, MaxPos, Range, Offset: Integer;
  Ratio: Double;
begin
  Result := ARect;
  TrackLen := ARect.Height;
  TrackWidth := ARect.Width;
  if not AVertical then
  begin
    TrackLen := ARect.Width;
    TrackWidth := ARect.Height;
  end;

  if TrackLen <= 0 then
    Exit(Rect(0, 0, 0, 0));

  MaxPos := ScrollMaxPosition(AMin, AMax, APage);
  Range := MaxPos - AMin;
  if APage > 0 then
    ThumbLen := Round(TrackLen * (APage / System.Math.Max(1, AMax - AMin + APage)))
  else
    ThumbLen := TrackLen div 3;
  ThumbLen := EnsureRange(ThumbLen, System.Math.Min(TrackLen, 20), TrackLen);

  if Range > 0 then
    Ratio := (EnsureRange(APos, AMin, MaxPos) - AMin) / Range
  else
    Ratio := 0;
  Offset := Round((TrackLen - ThumbLen) * Ratio);

  if AHot or APressed then
    ThumbWidth := System.Math.Min(8, System.Math.Max(4, TrackWidth - 4))
  else
    ThumbWidth := System.Math.Min(4, System.Math.Max(2, TrackWidth - 6));

  if AVertical then
    Result := Rect(ARect.Left + (TrackWidth - ThumbWidth) div 2,
      ARect.Top + Offset, ARect.Left + (TrackWidth + ThumbWidth) div 2,
      ARect.Top + Offset + ThumbLen)
  else
    Result := Rect(ARect.Left + Offset,
      ARect.Top + (TrackWidth - ThumbWidth) div 2,
      ARect.Left + Offset + ThumbLen,
      ARect.Top + (TrackWidth + ThumbWidth) div 2);
end;

procedure ERDPaintScrollBar(APainter: TERDPainter; const ARect: TRect;
  AVertical: Boolean; AMin, AMax, APage, APos: Integer; AHot,
  APressed: Boolean);
var
  Thumb: TRect;
  Fill, Outline, ThumbColor: TColor;
  Radius: Integer;
begin
  if APainter = nil then
    Exit;

  Fill := APainter.Palette.GaugeFace;
  Outline := APainter.Palette.NeutralLight;
  APainter.FillRect(ARect, Fill);
  if AHot or APressed then
    APainter.FrameRect(ARect, Outline)
  else
    APainter.FrameRect(ARect, ERDMixColor(Outline, Fill, 0.35));

  Thumb := ERDScrollThumbRect(ARect, AVertical, AMin, AMax, APage, APos,
    AHot, APressed);
  if Thumb.IsEmpty then
    Exit;

  if AHot or APressed then
    ThumbColor := APainter.Palette.Subtle
  else
    ThumbColor := ERDMixColor(APainter.Palette.Subtle, Fill, 0.6);
  if APressed then
    ThumbColor := ERDMixColor(APainter.Palette.ForegroundText, ThumbColor,
      0.20);
  if AVertical then
    Radius := Thumb.Width div 2
  else
    Radius := Thumb.Height div 2;
  APainter.RoundRect(Thumb.Left, Thumb.Top, Thumb.Width, Thumb.Height,
    Radius, ThumbColor, clNone);
end;

{ TERDScrollBar }

constructor TERDScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  Width := 14;
  Height := 100;
  FKind := sbVertical;
  FMin := 0;
  FMax := 100;
  FPageSize := 10;
  FPosition := 0;
  FSmallChange := 1;
  FLargeChange := 10;
end;

function TERDScrollBar.IsVertical: Boolean;
begin
  Result := FKind = sbVertical;
end;

function TERDScrollBar.MaxPosition: Integer;
begin
  Result := ScrollMaxPosition(FMin, FMax, FPageSize);
end;

function TERDScrollBar.CurrentThumbRect: TRect;
begin
  Result := ERDScrollThumbRect(ClientRect, IsVertical, FMin, FMax,
    FPageSize, FPosition, FHover, FPressed);
end;

procedure TERDScrollBar.SetKind(AValue: TScrollBarKind);
var
  OldW: Integer;
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  // Swap the orientation only for an interactive change: while the form
  // streams in, Width and Height were already read for the final kind and
  // swapping them turned a 36 px history bar into one that covered the page.
  if not (csLoading in ComponentState) then
  begin
    OldW := Width;
    Width := Height;
    Height := OldW;
  end;
  Invalidate;
end;

procedure TERDScrollBar.SetMin(AValue: Integer);
begin
  if FMin = AValue then
    Exit;
  FMin := AValue;
  if FMax < FMin then
    FMax := FMin;
  SetPosition(FPosition);
  Invalidate;
end;

procedure TERDScrollBar.SetMax(AValue: Integer);
begin
  if FMax = AValue then
    Exit;
  FMax := AValue;
  if FMin > FMax then
    FMin := FMax;
  SetPosition(FPosition);
  Invalidate;
end;

procedure TERDScrollBar.SetPageSize(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FPageSize = AValue then
    Exit;
  FPageSize := AValue;
  SetPosition(FPosition);
  Invalidate;
end;

procedure TERDScrollBar.SetPosition(AValue: Integer);
var
  NewPos: Integer;
begin
  NewPos := EnsureRange(AValue, FMin, MaxPosition);
  if FPosition = NewPos then
    Exit;
  FPosition := NewPos;
  Invalidate;
  Change;
end;

procedure TERDScrollBar.SetSmallChange(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  FSmallChange := AValue;
end;

procedure TERDScrollBar.SetLargeChange(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  FLargeChange := AValue;
end;

procedure TERDScrollBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHover := True;
  Invalidate;
end;

procedure TERDScrollBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not FDragging then
  begin
    FHover := False;
    Invalidate;
  end;
end;

procedure TERDScrollBar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TERDScrollBar.PaintControl(ACanvas: TCanvas);
var
  Painter: TERDPainter;
begin
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    ERDPaintScrollBar(Painter, ClientRect, IsVertical, FMin, FMax,
      FPageSize, FPosition, FHover, FPressed);
  finally
    Painter.Free;
  end;
end;

procedure TERDScrollBar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDScrollBar.DoScroll(ACode: TScrollCode; ANewPosition: Integer);
var
  ScrollPos: Integer;
begin
  ScrollPos := EnsureRange(ANewPosition, FMin, MaxPosition);
  if Assigned(FOnScroll) then
    FOnScroll(Self, ACode, ScrollPos);
  SetPosition(ScrollPos);
end;

function TERDScrollBar.PositionFromPoint(X, Y: Integer): Integer;
var
  TrackLen, ThumbLen, Coord, Range, MaxPos: Integer;
  Thumb: TRect;
  Ratio: Double;
begin
  Thumb := CurrentThumbRect;
  if IsVertical then
  begin
    TrackLen := ClientHeight;
    ThumbLen := Thumb.Height;
    Coord := Y - FDragOffset;
  end
  else
  begin
    TrackLen := ClientWidth;
    ThumbLen := Thumb.Width;
    Coord := X - FDragOffset;
  end;
  MaxPos := MaxPosition;
  Range := MaxPos - FMin;
  if (Range <= 0) or (TrackLen <= ThumbLen) then
    Exit(FMin);
  Ratio := EnsureRange(Coord / (TrackLen - ThumbLen), 0, 1);
  Result := FMin + Round(Range * Ratio);
end;

procedure TERDScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Thumb: TRect;
  NewPos: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  if CanFocus then
    SetFocus;
  FHover := True;
  FPressed := True;
  Thumb := CurrentThumbRect;
  if PtInRect(Thumb, Point(X, Y)) then
  begin
    FDragging := True;
    if IsVertical then
      FDragOffset := Y - Thumb.Top
    else
      FDragOffset := X - Thumb.Left;
    MouseCapture := True;
    DoScroll(scTrack, FPosition);
  end
  else
  begin
    if (IsVertical and (Y < Thumb.Top)) or
      ((not IsVertical) and (X < Thumb.Left)) then
      NewPos := FPosition - FLargeChange
    else
      NewPos := FPosition + FLargeChange;
    DoScroll(scPageDown, NewPos);
  end;
  Invalidate;
end;

procedure TERDScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDragging then
    DoScroll(scTrack, PositionFromPoint(X, Y));
end;

procedure TERDScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  if FDragging then
    DoScroll(scEndScroll, FPosition);
  FDragging := False;
  FPressed := False;
  MouseCapture := False;
  Invalidate;
end;

function TERDScrollBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Delta: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result then
    Exit;
  Delta := FSmallChange;
  if WheelDelta > 0 then
    Delta := -Delta;
  DoScroll(scLineDown, FPosition + Delta);
  Result := True;
end;

procedure TERDScrollBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewPos: Integer;
  Code: TScrollCode;
begin
  inherited;
  case Key of
    VK_LEFT, VK_UP:
      begin
        NewPos := FPosition - FSmallChange;
        Code := scLineUp;
      end;
    VK_RIGHT, VK_DOWN:
      begin
        NewPos := FPosition + FSmallChange;
        Code := scLineDown;
      end;
    VK_PRIOR:
      begin
        NewPos := FPosition - FLargeChange;
        Code := scPageUp;
      end;
    VK_NEXT:
      begin
        NewPos := FPosition + FLargeChange;
        Code := scPageDown;
      end;
    VK_HOME:
      begin
        NewPos := FMin;
        Code := scTop;
      end;
    VK_END:
      begin
        NewPos := MaxPosition;
        Code := scBottom;
      end;
  else
    Exit;
  end;
  Key := 0;
  DoScroll(Code, NewPos);
end;

end.
