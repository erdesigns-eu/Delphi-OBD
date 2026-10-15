//------------------------------------------------------------------------------
//  ERD.Components.PopupList
//
//  TERDPopupList - a themed no-activate drop-down list used by inline
//  editors in the Application controls.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the Application controls.
//------------------------------------------------------------------------------

unit ERD.Components.PopupList;

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
  Vcl.AppEvnts,
  ERD.Components.Types,
  ERD.Components.Paint,
  ERD.Components.LanguageServices;

type
  // Hint windows already own a no-activate popup HWND independently of Parent.
  // This avoids recreating a child HWND when the editor is detached on shutdown.
  TERDSignaturePopup = class(THintWindow)
  private
    FPalette: TERDThemePalette;
    FPPI, FActiveStart, FActiveCount: Integer;
    FLabelText, FDocumentation, FHeader: UnicodeString;
    FIsOpen: Boolean;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  protected
    procedure Paint; override;
    procedure DestroyWnd; override;
  public
    procedure Popup(const ScreenPoint: TPoint; const Help: TERDSignatureHelp;
      const APalette: TERDThemePalette; APPI: Integer);
    procedure CloseUp;
    property IsOpen: Boolean read FIsOpen;
  end;

  /// <summary>Fires when the popup list closes.</summary>
  /// <param name="Sender">Popup list.</param>
  /// <param name="AAccepted">True when the highlighted item was accepted.</param>
  /// <param name="AIndex">Current item index at close time.</param>
  TERDPopupCloseEvent = procedure(Sender: TObject; AAccepted: Boolean;
    AIndex: Integer) of object;

  /// <summary>Themed no-activate popup list used by inspector and combo editors.</summary>
  TERDPopupList = class(TCustomListBox)
  strict private
    FIsOpen: Boolean;
    FCompletionMode: Boolean;
    FCompletionItems: TArray<TERDCompletionItem>;
    FEmptyText: string;
    FOnCloseUp: TERDPopupCloseEvent;
    FPalette: TERDThemePalette;
    FPPI: Integer;
    FRowHeight: Integer;
    FAppEvents: TApplicationEvents;
    FOwnerControl: TWinControl;
    FFieldRect: TRect;
    FBarDragging: Boolean;
    FRowPressed: Boolean;
    FBarHot: Boolean;
    FBarDragOffset: Integer;
    FLastTop: Integer;
    FCheckingTop: Boolean;
    FScrollUpdateDepth: Integer;
    FHotIndex: Integer;
    procedure SetEmptyText(const Value: string);
    function VisibleRows: Integer;
    procedure SetHotIndex(AValue: Integer);
    procedure InvalidateItem(AIndex: Integer);
    procedure PaintFrame;
    procedure PaintCompletionFooter(Target: TCanvas; const Bounds: TRect);
    function CompletionFooterHeight: Integer;
    procedure PaintRows(ADC: HDC);
    procedure DrawRow(ACanvas: TCanvas; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    function NeedsBar: Boolean;
    function BarRect: TRect;
    function ThumbRect: TRect;
    procedure PaintBar;
    procedure BarMouseDown(Y: Integer);
    procedure BarDrag(Y: Integer);
    procedure CheckTopChanged;
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure AppDeactivate(Sender: TObject);
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    function ScalePixel(AValue: Integer): Integer;
  protected
    /// <summary>Creates a popup, top-most, no-activate window with a drop
    /// shadow. There is no WS_BORDER: Windows paints that frame in the
    /// system accent colour, so the list draws its own themed frame.</summary>
    /// <param name="Params">Window creation parameters.</param>
    procedure CreateParams(var Params: TCreateParams); override;
    /// <summary>Applies the fixed row height to the new list-box window.</summary>
    procedure CreateWnd; override;
    /// <summary>Drives the themed scroll bar (the Windows one is
    /// removed), the themed 1 px frame, hover tracking, and repaints after
    /// the list scrolls.</summary>
    /// <param name="Message">Window message.</param>
    procedure WndProc(var Message: TMessage); override;
    /// <summary>Scrolls three rows per wheel notch.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True: the wheel is handled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
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
    destructor Destroy; override;
    /// <summary>Enables structured completion rows and the selected-item information footer.</summary>
    procedure SetCompletionItems(const Values: TArray<TERDCompletionItem>);
    /// <summary>Shows the list under, or above, a field rectangle in screen coordinates.</summary>
    /// <param name="AOwnerControl">Control that owns the popup.</param>
    /// <param name="AFieldRect">Field bounds in screen coordinates.</param>
    /// <param name="AItems">Items to show.</param>
    /// <param name="AItemIndex">Initially highlighted item.</param>
    /// <param name="APalette">Palette used for painting.</param>
    /// <param name="APPI">Pixels per inch for themed drawing.</param>
    /// <param name="ARowHeight">Fixed row height in device pixels.</param>
    /// <param name="AMaxRows">Most rows shown before the list scrolls.</param>
    procedure Popup(AOwnerControl: TWinControl; const AFieldRect: TRect;
      AItems: TStrings; AItemIndex: Integer; const APalette: TERDThemePalette;
      APPI, ARowHeight: Integer; AMaxRows: Integer = 8);
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
    /// <summary>Message shown when the list contains no items.</summary>
    property EmptyText: string read FEmptyText write SetEmptyText;
    /// <summary>Item under the mouse, or -1.</summary>
    property HotIndex: Integer read FHotIndex;
    /// <summary>Fires after <see cref="CloseUp"/> hides the popup.</summary>
    property OnCloseUp: TERDPopupCloseEvent read FOnCloseUp write FOnCloseUp;
  end;

implementation

uses
  ERD.Components.ScrollBar;

procedure TERDSignaturePopup.WMMouseActivate(var Message: TWMMouseActivate);
begin Message.Result := MA_NOACTIVATE; end;
procedure TERDSignaturePopup.CloseUp;
begin
  FIsOpen := False;
  if HandleAllocated then ShowWindow(Handle, SW_HIDE);
end;
procedure TERDSignaturePopup.DestroyWnd;
begin FIsOpen := False; inherited; end;
procedure TERDSignaturePopup.Popup(const ScreenPoint: TPoint; const Help: TERDSignatureHelp;
  const APalette: TERDThemePalette; APPI: Integer);
var Signature: TERDSignature; Parameter: TERDSignatureParameter;
    Active, W, H, X, Y: Integer; Work, Docs: TRect;
begin
  if Length(Help.Signatures) = 0 then begin CloseUp; Exit; end;
  FPalette := APalette; FPPI := Max(96, APPI);
  Signature := Help.Signatures[EnsureRange(Help.ActiveSignature, 0, High(Help.Signatures))];
  FLabelText := Signature.LabelText; FDocumentation := Signature.Documentation;
  FActiveStart := -1; FActiveCount := 0;
  Active := Signature.ActiveParameter; if Active < 0 then Active := Help.ActiveParameter;
  if (Active >= 0) and (Active < Length(Signature.Parameters)) then
  begin
    Parameter := Signature.Parameters[Active]; FActiveStart := Parameter.Start; FActiveCount := Parameter.Count;
    if Parameter.Documentation <> '' then
    begin
      if FDocumentation <> '' then FDocumentation := Parameter.Documentation + #13#10#13#10 + FDocumentation
      else FDocumentation := Parameter.Documentation;
    end;
  end;
  FHeader := '';
  if Length(Help.Signatures) > 1 then
    FHeader := Format('%d / %d', [EnsureRange(Help.ActiveSignature, 0, High(Help.Signatures)) + 1, Length(Help.Signatures)]);
  Canvas.Font.Name := 'Consolas'; Canvas.Font.Height := -MulDiv(12, FPPI, 96);
  Work := Screen.MonitorFromPoint(ScreenPoint).WorkareaRect;
  W := Min(Work.Width, Min(MulDiv(640, FPPI, 96), Max(MulDiv(420, FPPI, 96), Canvas.TextWidth(FLabelText) + MulDiv(24, FPPI, 96))));
  H := MulDiv(30, FPPI, 96);
  if FDocumentation <> '' then
  begin
    Canvas.Font.Name := 'Segoe UI'; Canvas.Font.Height := -MulDiv(13, FPPI, 96);
    Docs := Rect(0, 0, Max(1, W - MulDiv(20, FPPI, 96)), 0);
    DrawTextW(Canvas.Handle, PWideChar(FDocumentation), Length(FDocumentation), Docs,
      DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
    Inc(H, Min(MulDiv(220, FPPI, 96), Docs.Height) + MulDiv(18, FPPI, 96));
  end;
  H := Min(Work.Height, H);
  X := EnsureRange(ScreenPoint.X, Work.Left, Max(Work.Left, Work.Right - W));
  Y := ScreenPoint.Y;
  if Y + H > Work.Bottom then Y := ScreenPoint.Y - H - MulDiv(22, FPPI, 96);
  Y := EnsureRange(Y, Work.Top, Max(Work.Top, Work.Bottom - H));
  DoubleBuffered := True;
  Color := FPalette.GaugeFace;
  // ActivateHint can recreate the native hint window (and call DestroyWnd).
  // Publish open state after activation so Paint is not suppressed by that reset.
  ActivateHint(Rect(X, Y, X + W, Y + H), '');
  FIsOpen := True;
  Invalidate;
  Update;
end;
procedure TERDSignaturePopup.Paint;
var Painter: TERDPainter; Saved, Offset, Left, Right, Origin, Available: Integer; R: TRect;
  function S(Value: Integer): Integer;
  begin Result := MulDiv(Value, FPPI, 96); end;
begin
  if not FIsOpen then Exit;
  Painter := TERDPainter.Create(Canvas, FPalette, FPPI);
  try
    Painter.FillRect(ClientRect, FPalette.GaugeFace);
    Painter.FrameRect(ClientRect, FPalette.NeutralLight);
    Available := ClientWidth - S(20);
    if FHeader <> '' then
    begin
      Dec(Available, S(54));
      Painter.Text(ClientWidth - S(48), S(15), FHeader, 11, FPalette.Subtle, twRegular);
    end;
    Canvas.Font.Name := 'Consolas'; Canvas.Font.Height := -S(12); Canvas.Font.Style := [];
    Left := 0; Right := 0;
    if (FActiveStart >= 0) and (FActiveCount > 0) and (FActiveStart + FActiveCount <= Length(FLabelText)) then
    begin
      Left := Canvas.TextWidth(Copy(FLabelText, 1, FActiveStart));
      Right := Left + Canvas.TextWidth(Copy(FLabelText, FActiveStart + 1, FActiveCount));
    end;
    Offset := Max(0, Right - Max(1, Available));
    Origin := S(10) - Offset;
    Saved := SaveDC(Canvas.Handle);
    try
      IntersectClipRect(Canvas.Handle, S(8), S(2), S(10) + Available, S(29));
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := FPalette.ForegroundText;
      Canvas.TextOut(Origin, S(8), FLabelText);
      if Right > Left then
      begin
        // Replace the active parameter's ink, without a selection rectangle.
        IntersectClipRect(Canvas.Handle, Origin + Left, 0, Origin + Right, S(29));
        Painter.FillRect(Rect(Origin + Left, S(2), Origin + Right, S(29)), FPalette.GaugeFace);
        Canvas.Brush.Style := bsClear;
        Canvas.Font.Color := Painter.AccentText;
        Canvas.Font.Style := [fsBold];
        Canvas.TextOut(Origin + Left, S(8), Copy(FLabelText, FActiveStart + 1, FActiveCount));
      end;
    finally RestoreDC(Canvas.Handle, Saved); end;
    if FDocumentation <> '' then
    begin
      Canvas.Pen.Color := FPalette.NeutralLight;
      Canvas.MoveTo(1, S(30)); Canvas.LineTo(ClientWidth - 1, S(30));
      Canvas.Font.Name := 'Segoe UI'; Canvas.Font.Height := -S(13); Canvas.Font.Style := [];
      Canvas.Font.Color := FPalette.ForegroundText; Canvas.Brush.Style := bsClear;
      R := Rect(S(10), S(39), ClientWidth - S(10), ClientHeight - S(9));
      DrawTextW(Canvas.Handle, PWideChar(FDocumentation), Length(FDocumentation), R,
        DT_WORDBREAK or DT_NOPREFIX or DT_END_ELLIPSIS);
    end;
  finally Painter.Free; end;
end;

constructor TERDPopupList.Create(AOwner: TComponent);
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
  FHotIndex := -1;
  ItemHeight := FRowHeight;
end;

destructor TERDPopupList.Destroy;
begin
  FIsOpen := False; FOnCloseUp := nil;
  if FAppEvents <> nil then
  begin
    FAppEvents.OnMessage := nil; FAppEvents.OnDeactivate := nil;
    FreeAndNil(FAppEvents);
  end;
  FBarDragging := False; FRowPressed := False;
  // Native listbox destruction still needs its item storage to be alive.
  if HandleAllocated then DestroyWnd;
  FOwnerControl := nil;
  inherited;
end;

procedure TERDPopupList.SetCompletionItems(const Values: TArray<TERDCompletionItem>);
begin
  FCompletionMode := True; FCompletionItems := Copy(Values); Invalidate;
end;
function TERDPopupList.CompletionFooterHeight: Integer;
begin
  if FCompletionMode then Result := ScalePixel(88) else Result := 0;
end;
procedure TERDPopupList.PaintCompletionFooter(Target: TCanvas; const Bounds: TRect);
var Painter: TERDPainter; R, DocRect: TRect;
    Item: TERDCompletionItem; Signature, Documentation: string;
begin
  if not FCompletionMode then Exit;
  Painter := TERDPainter.Create(Target, FPalette, FPPI);
  try
      R := Bounds;
      Painter.FillRect(R, FPalette.GaugeFace);
      Painter.HLine(R.Left, R.Top, R.Width, FPalette.NeutralLight);
      Item := Default(TERDCompletionItem);
      if (ItemIndex >= 0) and (ItemIndex < Length(FCompletionItems)) then Item := FCompletionItems[ItemIndex];
      Signature := Item.LabelText;
      if Item.Detail <> '' then Signature := Signature + '  ' + Item.Detail;
      if Signature = '' then Signature := 'No suggestions';
      Painter.Text(R.Left + ScalePixel(10), R.Top + ScalePixel(15), Signature, 12,
        FPalette.ForegroundText, twMono, taLeftJustify, Max(0, R.Width - ScalePixel(20)));
      Documentation := Item.Documentation;
      if Documentation = '' then Documentation := 'No additional documentation.';
      DocRect := Rect(R.Left + ScalePixel(10), R.Top + ScalePixel(29), R.Right - ScalePixel(10), R.Bottom - ScalePixel(26));
      Painter.WrapText(DocRect, Documentation, 11.5, FPalette.Subtle);
      Painter.HLine(R.Left, R.Bottom - ScalePixel(23), R.Width, FPalette.NeutralLight);
      Painter.Text(R.Left + ScalePixel(10), R.Bottom - ScalePixel(11),
        'Enter / Tab  accept     ' + #$2191 + ' / ' + #$2193 + '  navigate     Esc  dismiss',
        11, FPalette.Subtle, twRegular, taLeftJustify, Max(0, R.Width - ScalePixel(20)));
  finally Painter.Free; end;
end;

function TERDPopupList.ScalePixel(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, FPPI, 96);
end;

procedure TERDPopupList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // No Windows scroll bars: the list paints the themed overlay bar. No
  // WS_BORDER either: Windows draws that frame in the accent colour (blue
  // by default), so WM_NCCALCSIZE/WM_NCPAINT draw a themed one instead.
  Params.Style := (Params.Style and not (WS_CHILD or WS_VSCROLL or
    WS_HSCROLL or WS_BORDER or WS_DLGFRAME or WS_THICKFRAME)) or WS_POPUP or
    WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.ExStyle := (Params.ExStyle and not (WS_EX_CLIENTEDGE or
    WS_EX_WINDOWEDGE or WS_EX_STATICEDGE)) or WS_EX_TOOLWINDOW or
    WS_EX_TOPMOST or WS_EX_NOACTIVATE;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS or
    CS_DROPSHADOW;
end;

procedure TERDPopupList.CreateWnd;
begin
  inherited CreateWnd;
  // A WS_POPUP list box sends WM_MEASUREITEM to its owner window, which the
  // VCL does not route back, so Windows would fall back to the font height.
  SendMessage(Handle, LB_SETITEMHEIGHT, 0, FRowHeight);
end;

function TERDPopupList.VisibleRows: Integer;
begin
  Result := System.Math.Max(1, ClientHeight div System.Math.Max(1,
    FRowHeight));
end;

function TERDPopupList.NeedsBar: Boolean;
begin
  Result := HandleAllocated and (Items.Count > VisibleRows);
end;

function TERDPopupList.BarRect: TRect;
begin
  Result := Rect(ClientWidth - ScalePixel(10), 0, ClientWidth, ClientHeight);
end;

function TERDPopupList.ThumbRect: TRect;
begin
  Result := ERDScrollThumbRect(BarRect, True, 0, Items.Count - 1,
    VisibleRows, TopIndex, FBarHot, FBarDragging);
end;

procedure TERDPopupList.PaintBar;
var R: TRect;
begin
  if not HandleAllocated or not NeedsBar then Exit;
  R := BarRect;
  InvalidateRect(Handle, @R, False);
  UpdateWindow(Handle);
end;

procedure TERDPopupList.BarMouseDown(Y: Integer);
var
  Thumb: TRect;
begin
  Thumb := ThumbRect;
  if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
  begin
    FBarDragging := True;
    FBarDragOffset := Y - Thumb.Top;
    MouseCapture := True;
    PaintBar;
  end
  else if Y < Thumb.Top then
    TopIndex := System.Math.Max(0, TopIndex - VisibleRows)
  else
    TopIndex := System.Math.Min(Items.Count - VisibleRows,
      TopIndex + VisibleRows);
end;

procedure TERDPopupList.BarDrag(Y: Integer);
var
  Thumb: TRect;
  Travel, MaxTop: Integer;
begin
  Thumb := ThumbRect;
  Travel := ClientHeight - Thumb.Height;
  MaxTop := System.Math.Max(0, Items.Count - VisibleRows);
  if Travel > 0 then
    TopIndex := EnsureRange(Round((Y - FBarDragOffset) / Travel * MaxTop),
      0, MaxTop);
end;

procedure TERDPopupList.InvalidateItem(AIndex: Integer);
var
  R: TRect;
begin
  if not HandleAllocated or (AIndex < 0) or (AIndex >= Items.Count) then
    Exit;
  R := ItemRect(AIndex);
  if NeedsBar then R.Right := System.Math.Min(R.Right, BarRect.Left);
  // Hover changes only the row; leave the scrollbar untouched.
  InvalidateRect(Handle, @R, False);
end;

procedure TERDPopupList.SetHotIndex(AValue: Integer);
begin
  if (AValue < 0) or (AValue >= Items.Count) then
    AValue := -1;
  if AValue = FHotIndex then
    Exit;
  InvalidateItem(FHotIndex);
  FHotIndex := AValue;
  InvalidateItem(FHotIndex);
end;

procedure TERDPopupList.PaintFrame;
var
  DC: HDC;
  R: TRect;
  Brush: HBRUSH;
  Buffer: TBitmap;
  Footer: TRect;
begin
  if not HandleAllocated then
    Exit;
  DC := GetWindowDC(Handle);
  if DC = 0 then
    Exit;
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    if FCompletionMode then
    begin
      Buffer := TBitmap.Create;
      try
        Footer := Rect(1, R.Bottom - 1 - CompletionFooterHeight, R.Right - 1, R.Bottom - 1);
        Buffer.PixelFormat := pf32bit;
        Buffer.SetSize(Footer.Width, Footer.Height);
        // Use the bitmap's owning canvas: a second TCanvas on the same HDC
        // invalidates its selected GDI objects/state while drawing the text.
        PaintCompletionFooter(Buffer.Canvas, Rect(0, 0, Buffer.Width, Buffer.Height));
        BitBlt(DC, Footer.Left, Footer.Top, Footer.Width, Footer.Height,
          Buffer.Canvas.Handle, 0, 0, SRCCOPY);
      finally Buffer.Free; end;
    end;
    Brush := CreateSolidBrush(ColorToRGB(FPalette.NeutralLight));
    try
      Winapi.Windows.FrameRect(DC, R, Brush);
    finally
      DeleteObject(Brush);
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TERDPopupList.CheckTopChanged;
var
  Top: Integer;
begin
  if FCheckingTop or not HandleAllocated or not FIsOpen then
    Exit;
  FCheckingTop := True;
  try
    Top := TopIndex;
    // The list box scrolls by blitting its client area, which drags the
    // overlay bar along: repaint everything when the top row changed.
    if Top <> FLastTop then
    begin
      FLastTop := Top;
      InvalidateRect(Handle, nil, False);
    end;
  finally
    FCheckingTop := False;
  end;
end;

procedure TERDPopupList.SetEmptyText(const Value: string);
begin
  if FEmptyText = Value then Exit;
  FEmptyText := Value;
  Invalidate;
end;

procedure TERDPopupList.PaintRows(ADC: HDC);
var
  I: Integer;
  R: TRect;
  State: TOwnerDrawState;
  Buffer: TBitmap;
  Painter: TERDPainter;
begin
  if (ClientWidth <= 0) or (ClientHeight <= 0) then Exit;
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(ClientWidth, ClientHeight);
    Buffer.Canvas.Brush.Color := FPalette.GaugeFace;
    Buffer.Canvas.FillRect(ClientRect);
    for I := TopIndex to Items.Count - 1 do
    begin
      if SendMessage(Handle, LB_GETITEMRECT, I, LPARAM(@R)) = LB_ERR then Break;
      if R.Top >= ClientHeight then Break;
      if R.Bottom <= 0 then Continue;
      State := [];
      if I = ItemIndex then Include(State, odSelected);
      DrawRow(Buffer.Canvas, I, R, State);
    end;
    if (Items.Count = 0) and (FEmptyText <> '') then
    begin
      Painter := TERDPainter.Create(Buffer.Canvas, FPalette, FPPI);
      try
        Painter.Text(ScalePixel(8), FRowHeight div 2, FEmptyText,
          12.5, FPalette.ForegroundText, twRegular, taLeftJustify,
          System.Math.Max(0, ClientWidth - ScalePixel(16)));
      finally
        Painter.Free;
      end;
    end;
    if NeedsBar then
    begin
      Painter := TERDPainter.Create(Buffer.Canvas, FPalette, FPPI);
      try
        ERDPaintScrollBar(Painter, BarRect, True, 0, Items.Count - 1,
          VisibleRows, TopIndex, FBarHot, FBarDragging);
      finally
        Painter.Free;
      end;
    end;
    // Rows and scrollbar reach the window together; the paint DC clips
    // this copy to the dirty region, so row hover never clears the bar.
    BitBlt(ADC, 0, 0, ClientWidth, ClientHeight, Buffer.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    Buffer.Free;
  end;
end;

procedure TERDPopupList.WndProc(var Message: TMessage);
var
  Pt: TPoint;
  Hot: Boolean;
  DC: HDC;
  PaintInfo: TPaintStruct;
  Index: Integer;
  FreezeScroll: Boolean;
begin
  case Message.Msg of
    WM_PAINT:
      begin
        // WS_POPUP does not receive VCL child owner-draw reflection.
        // Paint visible rows directly instead of relying on WM_DRAWITEM.
        DC := BeginPaint(Handle, PaintInfo);
        try
          PaintRows(DC);
        finally
          EndPaint(Handle, PaintInfo);
        end;
        Message.Result := 0;
        Exit;
      end;
    WM_PRINTCLIENT:
      begin
        PaintRows(HDC(Message.WParam));
        Message.Result := 0;
        Exit;
      end;
    WM_NCCALCSIZE:
      begin
        // Reserve a 1 px non-client frame for the themed border.
        inherited WndProc(Message);
        if TWMNCCalcSize(Message).CalcValidRects then
        begin
          InflateRect(TWMNCCalcSize(Message).CalcSize_Params^.rgrc[0], -1, -1);
          Dec(TWMNCCalcSize(Message).CalcSize_Params^.rgrc[0].Bottom, CompletionFooterHeight);
        end
        else
        begin
          InflateRect(PRect(Message.LParam)^, -1, -1);
          Dec(PRect(Message.LParam)^.Bottom, CompletionFooterHeight);
        end;
        Exit;
      end;
    WM_NCPAINT:
      begin
        PaintFrame;
        Message.Result := 0;
        Exit;
      end;
    WM_ERASEBKGND:
      begin
        // PaintRows supplies the complete background in its buffer.
        Message.Result := 1;
        Exit;
      end;
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        Pt := Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos);
        if NeedsBar and PtInRect(BarRect, Pt) then
        begin
          BarMouseDown(Pt.Y);
          CheckTopChanged;
          Message.Result := 0;
          Exit;
        end;
        // Do not enter the native listbox tracking loop: it can consume
        // button-up before VCL dispatches the MouseUp override.
        Index := ItemAtPos(Pt, True);
        FRowPressed := Index >= 0;
        if FRowPressed then
        begin
          ItemIndex := Index;
          MouseCapture := True;
          Invalidate;
        end;
        Message.Result := 0;
        Exit;
      end;
    WM_MOUSEMOVE:
      begin
        Pt := Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos);
        if FBarDragging then
        begin
          BarDrag(Pt.Y);
          CheckTopChanged;
          Message.Result := 0;
          Exit;
        end;
        Hot := NeedsBar and PtInRect(BarRect, Pt);
        if Hot <> FBarHot then
        begin
          FBarHot := Hot;
          PaintBar;
        end;
        if Hot then
        begin
          SetHotIndex(-1);
          Message.Result := 0;
          Exit;
        end;
        // Hover highlight: the row under the mouse gets a light accent
        // tint, like the menus.
        SetHotIndex(ItemAtPos(Pt, True));
      end;
    WM_LBUTTONUP:
      begin
        Pt := Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos);
        if FBarDragging then
        begin
          FBarDragging := False;
          MouseCapture := False;
          PaintBar;
          Message.Result := 0;
          Exit;
        end;
        if NeedsBar and PtInRect(BarRect, Pt) then
        begin
          Message.Result := 0;
          Exit;
        end;
        if FRowPressed then
        begin
          FRowPressed := False;
          MouseCapture := False;
          Index := ItemAtPos(Pt, True);
          if Index >= 0 then ItemIndex := Index;
          CloseUp(Index >= 0);
        end;
        Message.Result := 0;
        Exit;
      end;
    CM_MOUSELEAVE:
      begin
        SetHotIndex(-1);
        if FBarHot and not FBarDragging then
        begin
          FBarHot := False;
          PaintBar;
        end;
      end;
  end;
  // A native listbox scrolls by copying its client pixels, including our
  // overlay thumb. Suppress that intermediate blit, then paint rows and thumb
  // together once at the final top index. Nesting covers wheel -> LB_SETTOPINDEX.
  FreezeScroll := FIsOpen and ((Message.Msg = WM_KEYDOWN) or (Message.Msg = WM_MOUSEWHEEL) or
    (Message.Msg = WM_VSCROLL) or (Message.Msg = LB_SETCURSEL) or (Message.Msg = LB_SETTOPINDEX));
  if FreezeScroll then
  begin
    Inc(FScrollUpdateDepth);
    if FScrollUpdateDepth = 1 then SendMessage(Handle, WM_SETREDRAW, 0, 0);
  end;
  try
    inherited WndProc(Message);
  finally
    if FreezeScroll then
    begin
      Dec(FScrollUpdateDepth);
      if (FScrollUpdateDepth = 0) and HandleAllocated then
      begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        InvalidateRect(Handle, nil, False); UpdateWindow(Handle);
      end;
    end;
  end;
  case Message.Msg of
    WM_KEYDOWN, WM_MOUSEWHEEL, WM_TIMER, WM_LBUTTONDOWN,
    WM_VSCROLL, LB_SETCURSEL, LB_SETTOPINDEX, LB_RESETCONTENT:
      begin
        CheckTopChanged;
        if FCompletionMode and FIsOpen then PaintFrame;
      end;
  end;
end;

function TERDPopupList.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result or (Items.Count = 0) then
    Exit;
  TopIndex := EnsureRange(TopIndex - Sign(WheelDelta) * 3, 0,
    System.Math.Max(0, Items.Count - VisibleRows));
  Result := True;
end;

procedure TERDPopupList.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if not FIsOpen or not HandleAllocated then
    Exit;
  case Msg.message of
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_LBUTTONDBLCLK,
    WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN, WM_NCLBUTTONDBLCLK:
      begin
        if (Msg.hwnd = Handle) or IsChild(Handle, Msg.hwnd) then
          Exit;
        // A press on the owner's field is left to the owner, which toggles
        // the list itself (like clicking a Windows combo box while open).
        if (FOwnerControl <> nil) and FOwnerControl.HandleAllocated and
          ((Msg.hwnd = FOwnerControl.Handle) or IsChild(FOwnerControl.Handle, Msg.hwnd)) and PtInRect(FFieldRect, Msg.pt) then
          Exit;
        // Any other press closes the list and is swallowed, as with the
        // standard combo box drop-down.
        CloseUp(False);
        Handled := True;
      end;
  end;
end;

procedure TERDPopupList.AppDeactivate(Sender: TObject);
begin
  if FIsOpen then
    CloseUp(False);
end;

procedure TERDPopupList.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TERDPopupList.Popup(AOwnerControl: TWinControl;
  const AFieldRect: TRect; AItems: TStrings; AItemIndex: Integer;
  const APalette: TERDThemePalette; APPI, ARowHeight: Integer;
  AMaxRows: Integer);
var
  Rows, W, H, X, Y: Integer;
  Work: TRect;
  Mon: TMonitor;
begin
  if AOwnerControl = nil then
    Exit;

  FOwnerControl := AOwnerControl;
  FFieldRect := AFieldRect;
  if FAppEvents = nil then
  begin
    FAppEvents := TApplicationEvents.Create(Self);
    FAppEvents.OnMessage := AppMessage;
    FAppEvents.OnDeactivate := AppDeactivate;
  end;
  FPalette := APalette;
  FPPI := APPI;
  if FPPI <= 0 then
    FPPI := 96;
  FRowHeight := System.Math.Max(1, ARowHeight);

  Rows := System.Math.Max(1, System.Math.Min(System.Math.Max(1, AMaxRows),
    System.Math.Max(AItems.Count, 1)));
  W := System.Math.Max(AFieldRect.Width, ScalePixel(40));
  H := Rows * FRowHeight + 2 + CompletionFooterHeight;
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

  if FCompletionMode then
  begin
    W := Min(W, Work.Width); H := Min(H, Work.Height);
    X := EnsureRange(X, Work.Left, Max(Work.Left, Work.Right - W));
    Y := EnsureRange(Y, Work.Top, Max(Work.Top, Work.Bottom - H));
  end;

  // Position the list before it gets a parent: assigning the parent
  // creates and shows the window at its current Left/Top, which for a new
  // list is the top-left corner of the desktop (a visible flash).
  SetBounds(X, Y, W, H);
  if Parent <> AOwnerControl then
    Parent := AOwnerControl;
  ItemHeight := FRowHeight;
  Color := FPalette.GaugeFace;
  Font.Color := FPalette.ForegroundText;
  Items.Assign(AItems);
  if Items.Count > 0 then
    ItemIndex := EnsureRange(AItemIndex, 0, Items.Count - 1)
  else
    ItemIndex := -1;

  HandleNeeded;
  SendMessage(Handle, LB_SETITEMHEIGHT, 0, FRowHeight);
  FBarDragging := False;
  FBarHot := False;
  FHotIndex := -1;
  FLastTop := -1;
  FIsOpen := True;
  // SWP_FRAMECHANGED re-runs WM_NCCALCSIZE so the themed frame is in place.
  SetWindowPos(Handle, HWND_TOP, X, Y, W, H, SWP_NOACTIVATE or
    SWP_SHOWWINDOW or SWP_FRAMECHANGED);
  Invalidate;
end;

procedure TERDPopupList.CloseUp(AAccepted: Boolean);
var
  LIndex: Integer;
begin
  if not FIsOpen then Exit;
  // Hiding or releasing capture can re-enter application/focus callbacks.
  // Mark closed first, and never let ItemIndex recreate a detached HWND.
  FIsOpen := False;
  LIndex := -1;
  if HandleAllocated then LIndex := ItemIndex else AAccepted := False;
  if FBarDragging or FRowPressed then
  begin
    FBarDragging := False;
    FRowPressed := False;
    MouseCapture := False;
  end;
  if HandleAllocated then
    ShowWindow(Handle, SW_HIDE);
  FHotIndex := -1;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self, AAccepted, LIndex);
end;

function TERDPopupList.HandleKey(var Key: Word; Shift: TShiftState): Boolean;
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
    // The keyboard now drives the highlight; drop the stale mouse hover.
    FHotIndex := -1;
    Invalidate;
  end;
end;

procedure TERDPopupList.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  DrawRow(Canvas, Index, Rect, State);
end;

procedure TERDPopupList.DrawRow(ACanvas: TCanvas; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  P: TERDPainter;
  Fill, Ink: TColor;
  Weight: TERDTextWeight;
  Selected, Hot: Boolean;
  KindWidth, NameWidth, Available, NameX, DetailX: Integer; Item: TERDCompletionItem;
begin
  P := TERDPainter.Create(ACanvas, FPalette, FPPI);
  try
    Fill := FPalette.GaugeFace;
    Ink := FPalette.ForegroundText;
    Weight := twRegular;
    Selected := odSelected in State;
    Hot := Index = FHotIndex;
    if Selected then
    begin
      if P.Dark and Hot then
        Fill := P.Tint(FPalette.Accent, 0.26)
      else if P.Dark or Hot then
        Fill := P.Tint(FPalette.Accent, 0.18)
      else
        Fill := P.Tint(FPalette.Accent, 0.12);
      Ink := P.AccentText;
      Weight := twSemibold;
    end
    else if Hot then
    begin
      if P.Dark then
        Fill := P.Tint(FPalette.Accent, 0.12)
      else
        Fill := P.Tint(FPalette.Accent, 0.07);
    end;
    // Leave the overlay scroll bar's strip alone.
    if NeedsBar then
      Rect.Right := System.Math.Min(Rect.Right, BarRect.Left);
    P.FillRect(Rect, Fill);
    if Selected then
      P.FillRect(System.Types.Rect(Rect.Left, Rect.Top + ScalePixel(4),
        Rect.Left + ScalePixel(3), Rect.Bottom - ScalePixel(4)),
        FPalette.Accent);
    if FCompletionMode and (Index >= 0) and (Index < Length(FCompletionItems)) then
    begin
      Item := FCompletionItems[Index];
      KindWidth := ScalePixel(90); Available := Max(0, Rect.Width - KindWidth - ScalePixel(24));
      NameWidth := Min(ScalePixel(210), Available div 2);
      NameX := Rect.Left + ScalePixel(10) + KindWidth; DetailX := NameX + NameWidth + ScalePixel(8);
      P.Text(Rect.Left + ScalePixel(10), Rect.Top + Rect.Height div 2,
        ERDCompletionKindText(Item.Kind), 11.5, P.AccentText, twRegular, taLeftJustify, KindWidth - ScalePixel(8));
      P.Text(NameX, Rect.Top + Rect.Height div 2, Item.LabelText, 12,
        Ink, twMonoBold, taLeftJustify, NameWidth);
      P.Text(DetailX, Rect.Top + Rect.Height div 2, Item.Detail, 11.5,
        FPalette.Subtle, twMono, taLeftJustify, Max(0, Rect.Right - DetailX - ScalePixel(8)));
    end
    else P.Text(Rect.Left + ScalePixel(8), Rect.Top + Rect.Height div 2,
      Items[Index], 12.5, Ink, Weight, taLeftJustify,
      System.Math.Max(0, Rect.Width - ScalePixel(16)));
    if not FCompletionMode then P.HLine(Rect.Left, Rect.Bottom - 1, Rect.Width, FPalette.NeutralLight);
  finally
    P.Free;
  end;
end;

procedure TERDPopupList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
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
