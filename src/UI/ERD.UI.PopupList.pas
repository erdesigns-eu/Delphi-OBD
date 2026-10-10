//------------------------------------------------------------------------------
//  ERD.UI.PopupList
//
//  TOBDPopupList - a themed no-activate drop-down list used by inline
//  editors in the OBD Studio controls.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.PopupList;

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
  Vcl.StdCtrls,
  Vcl.Forms,
  ERD.UI.Types,
  ERD.UI.Paint;

type
  /// <summary>Fires when the popup list closes.</summary>
  /// <param name="Sender">Popup list.</param>
  /// <param name="AAccepted">True when the highlighted item was accepted.</param>
  /// <param name="AIndex">Current item index at close time.</param>
  TOBDPopupCloseEvent = procedure(Sender: TObject; AAccepted: Boolean;
    AIndex: Integer) of object;

  /// <summary>Themed no-activate popup list used by inspector and combo editors.</summary>
  TOBDPopupList = class(TCustomListBox)
  strict private
    FIsOpen: Boolean;
    FOnCloseUp: TOBDPopupCloseEvent;
    FPalette: TOBDThemePalette;
    FPPI: Integer;
    FRowHeight: Integer;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    function ScalePixel(AValue: Integer): Integer;
  protected
    /// <summary>Creates a popup, border, top-most and no-activate window.</summary>
    /// <param name="Params">Window creation parameters.</param>
    procedure CreateParams(var Params: TCreateParams); override;
    /// <summary>Paints one owner-drawn row.</summary>
    /// <param name="Index">Item index.</param>
    /// <param name="Rect">Item rectangle.</param>
    /// <param name="State">Draw state.</param>
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    /// <summary>Accepts the item released under the mouse.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
  public
    /// <summary>Creates the popup list.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Shows the list under, or above, a field rectangle in screen coordinates.</summary>
    /// <param name="AOwnerControl">Control that owns the popup.</param>
    /// <param name="AFieldRect">Field bounds in screen coordinates.</param>
    /// <param name="AItems">Items to show.</param>
    /// <param name="AItemIndex">Initially highlighted item.</param>
    /// <param name="APalette">Palette used for painting.</param>
    /// <param name="APPI">Pixels per inch for themed drawing.</param>
    /// <param name="ARowHeight">Fixed row height in device pixels.</param>
    procedure Popup(AOwnerControl: TWinControl; const AFieldRect: TRect;
      AItems: TStrings; AItemIndex: Integer; const APalette: TOBDThemePalette;
      APPI, ARowHeight: Integer);
    /// <summary>Hides the list and reports whether the highlighted item was accepted.</summary>
    /// <param name="AAccepted">True to accept the highlighted item.</param>
    procedure CloseUp(AAccepted: Boolean);
    /// <summary>Handles owner key messages while the popup is open.</summary>
    /// <param name="Key">Virtual key code. Set to 0 when handled.</param>
    /// <param name="Shift">Shift state.</param>
    /// <returns>True when the key was consumed.</returns>
    function HandleKey(var Key: Word; Shift: TShiftState): Boolean;
    /// <summary>True while the popup window is visible.</summary>
    property IsOpen: Boolean read FIsOpen;
    /// <summary>Fires after <see cref="CloseUp"/> hides the popup.</summary>
    property OnCloseUp: TOBDPopupCloseEvent read FOnCloseUp write FOnCloseUp;
  end;

implementation

constructor TOBDPopupList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Style := lbOwnerDrawFixed;
  BorderStyle := bsNone;
  IntegralHeight := False;
  TabStop := False;
  ParentColor := False;
  FPalette := BRAND_PALETTE_LIGHT;
  FPPI := 96;
  FRowHeight := 24;
  ItemHeight := FRowHeight;
end;

function TOBDPopupList.ScalePixel(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, FPPI, 96);
end;

procedure TOBDPopupList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style and not WS_CHILD) or WS_POPUP or WS_BORDER or
    WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_TOPMOST or
    WS_EX_NOACTIVATE;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS;
end;

procedure TOBDPopupList.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TOBDPopupList.Popup(AOwnerControl: TWinControl;
  const AFieldRect: TRect; AItems: TStrings; AItemIndex: Integer;
  const APalette: TOBDThemePalette; APPI, ARowHeight: Integer);
var
  Rows, W, H, X, Y: Integer;
  Work: TRect;
  Mon: TMonitor;
begin
  if AOwnerControl = nil then
    Exit;

  Parent := AOwnerControl;
  FPalette := APalette;
  FPPI := APPI;
  if FPPI <= 0 then
    FPPI := 96;
  FRowHeight := System.Math.Max(1, ARowHeight);
  ItemHeight := FRowHeight;
  Color := FPalette.GaugeFace;
  Font.Color := FPalette.ForegroundText;
  Items.Assign(AItems);
  if Items.Count > 0 then
    ItemIndex := EnsureRange(AItemIndex, 0, Items.Count - 1)
  else
    ItemIndex := -1;

  Rows := System.Math.Max(1, System.Math.Min(8, System.Math.Max(Items.Count, 1)));
  W := System.Math.Max(AFieldRect.Width, ScalePixel(40));
  H := Rows * FRowHeight + 2;
  X := AFieldRect.Left;
  Y := AFieldRect.Bottom;

  Mon := Screen.MonitorFromRect(AFieldRect, mdNearest);
  if Mon <> nil then
    Work := Mon.WorkareaRect
  else
    Work := Screen.DesktopRect;

  if (Y + H > Work.Bottom) and (AFieldRect.Top - H >= Work.Top) then
    Y := AFieldRect.Top - H;
  if X + W > Work.Right then
    X := Work.Right - W;
  if X < Work.Left then
    X := Work.Left;

  HandleNeeded;
  FIsOpen := True;
  SetWindowPos(Handle, HWND_TOP, X, Y, W, H, SWP_NOACTIVATE or
    SWP_SHOWWINDOW);
  Invalidate;
end;

procedure TOBDPopupList.CloseUp(AAccepted: Boolean);
var
  LIndex: Integer;
begin
  LIndex := ItemIndex;
  if HandleAllocated then
    ShowWindow(Handle, SW_HIDE);
  FIsOpen := False;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self, AAccepted, LIndex);
end;

function TOBDPopupList.HandleKey(var Key: Word; Shift: TShiftState): Boolean;
var
  NewIndex: Integer;
begin
  Result := False;
  if not FIsOpen then
    Exit;

  if ((Key = VK_UP) or (Key = VK_DOWN)) and (ssAlt in Shift) then
  begin
    Key := 0;
    CloseUp(True);
    Exit(True);
  end;

  case Key of
    VK_UP:
      begin
        NewIndex := ItemIndex - 1;
        if NewIndex < 0 then
          NewIndex := 0;
        ItemIndex := NewIndex;
        Result := True;
      end;
    VK_DOWN:
      begin
        NewIndex := ItemIndex + 1;
        if NewIndex >= Items.Count then
          NewIndex := Items.Count - 1;
        ItemIndex := NewIndex;
        Result := True;
      end;
    VK_PRIOR:
      begin
        ItemIndex := System.Math.Max(0, ItemIndex - 8);
        Result := True;
      end;
    VK_NEXT:
      begin
        ItemIndex := System.Math.Min(Items.Count - 1, ItemIndex + 8);
        Result := True;
      end;
    VK_HOME:
      begin
        if Items.Count > 0 then
          ItemIndex := 0;
        Result := True;
      end;
    VK_END:
      begin
        if Items.Count > 0 then
          ItemIndex := Items.Count - 1;
        Result := True;
      end;
    VK_RETURN:
      begin
        CloseUp(True);
        Result := True;
      end;
    VK_ESCAPE:
      begin
        CloseUp(False);
        Result := True;
      end;
    VK_F4:
      begin
        CloseUp(True);
        Result := True;
      end;
  end;

  if Result then
  begin
    Key := 0;
    Invalidate;
  end;
end;

procedure TOBDPopupList.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  P: TOBDPainter;
  Fill, Ink: TColor;
  Weight: TOBDTextWeight;
begin
  P := TOBDPainter.Create(Canvas, FPalette, FPPI);
  try
    Fill := FPalette.GaugeFace;
    Ink := FPalette.ForegroundText;
    Weight := twRegular;
    if odSelected in State then
    begin
      if P.Dark then
        Fill := P.Tint(FPalette.Accent, 0.18)
      else
        Fill := P.Tint(FPalette.Accent, 0.12);
      Ink := P.AccentText;
      Weight := twSemibold;
    end;
    P.FillRect(Rect, Fill);
    P.Text(Rect.Left + ScalePixel(8), Rect.Top + Rect.Height div 2,
      Items[Index], 12.5, Ink, Weight, taLeftJustify,
      System.Math.Max(0, Rect.Width - ScalePixel(16)));
    P.HLine(Rect.Left, Rect.Bottom - 1, Rect.Width, FPalette.NeutralLight);
  finally
    P.Free;
  end;
end;

procedure TOBDPopupList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Idx: Integer;
begin
  inherited;
  if (Button = mbLeft) and FIsOpen then
  begin
    Idx := ItemAtPos(Point(X, Y), True);
    if Idx >= 0 then
      ItemIndex := Idx;
    CloseUp(Idx >= 0);
  end;
end;

end.
