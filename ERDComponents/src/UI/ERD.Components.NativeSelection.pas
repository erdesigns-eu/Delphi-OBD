unit ERD.Components.NativeSelection;
interface
uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
  System.Math, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Types, ERD.Components.Paint;
type
  TERDSelectionEdit = class(TEdit)
  private
    FRedrawDepth: Integer;
  protected
    procedure WndProc(var Message: TMessage); override;
  end;
  TERDSelectionMemo = class(TMemo)
  private
    FRedrawDepth: Integer;
  protected
    procedure WndProc(var Message: TMessage); override;
  end;
implementation
type
  TERDSelectionControlAccess = class(TERDCustomControl)
  public
    function SelectionPalette: TERDThemePalette;
  end;

  TERDNativeEditAccess = class(TCustomEdit)
  public
    procedure PaintNativeClient(DC: HDC);
    property HideSelection;
    property PasswordChar;
    property Text;
    property Font;
    property Color;
  end;
procedure TERDNativeEditAccess.PaintNativeClient(DC: HDC);
var Message: TMessage;
begin
  Message.Msg := WM_PRINTCLIENT;
  Message.WParam := WPARAM(DC);
  Message.LParam := PRF_CLIENT;
  Message.Result := 0;
  // Bypass VCL's WM_PRINTCLIENT handler and invoke the native EDIT proc.
  DefaultHandler(Message);
end;

function TERDSelectionControlAccess.SelectionPalette: TERDThemePalette;
begin
  Result := Palette;
end;

procedure DrawSelection(Editor: TCustomEdit; Target: HDC);
var E: TERDNativeEditAccess; C: TCanvas; Palette: TERDThemePalette; P: TERDPainter;
  FormatRect, Clip, R: TRect; Metrics: TTextMetric; Source, Run: string;
  StartSel, EndSel, FirstLine, LastLine, LineNo, LineStart, LineEnd, NextLine,
  A, B, LastChar, RunStart, RunEnd, SavedDC, LineHeight: Integer;
  PosA, PosB, PosRun: TPoint; Multiline: Boolean;
  Fill, Ink: TColor; Flags: Cardinal;
  function CharPosition(Index: Integer; out Pos: TPoint): Boolean;
  var Value: LRESULT;
  begin
    Value := SendMessage(E.Handle, EM_POSFROMCHAR, WPARAM(Index), 0);
    Result := Value <> -1;
    Pos.X := SmallInt(LoWord(Value));
    Pos.Y := SmallInt(HiWord(Value));
  end;
begin
  if (Target = 0) or not (Editor.Parent is TERDCustomControl) then Exit;
  E := TERDNativeEditAccess(Editor);
  if (E.SelLength <= 0) or (not E.Focused and E.HideSelection) then Exit;
  Source := E.Text;
  StartSel := E.SelStart;
  EndSel := Min(Length(Source), StartSel + E.SelLength);
  if EndSel <= StartSel then Exit;
  Palette := TERDSelectionControlAccess(E.Parent).SelectionPalette;
  Fill := Palette.Accent;
  Ink := Palette.ForegroundText;
  if E.Focused then
  begin
    P := TERDPainter.Create(nil, Palette, E.Font.PixelsPerInch);
    try Ink := P.ContrastInk(Fill) finally P.Free end;
  end
  else Fill := ERDMixColor(Palette.Accent, Palette.GaugeFace, 0.2);
  C := TCanvas.Create;
  SavedDC := SaveDC(Target);
  try
    C.Handle := Target;
    C.Font.Assign(E.Font);
    C.Font.Color := Ink;
    SelectObject(Target, E.Font.Handle);
    C.Brush.Color := Fill;
    GetTextMetrics(Target, Metrics);
    LineHeight := Max(1, Metrics.tmHeight);
    Winapi.Windows.GetClientRect(E.Handle, FormatRect);
    Clip := FormatRect;
    Multiline := (GetWindowLong(E.Handle, GWL_STYLE) and ES_MULTILINE) <> 0;
    if Multiline then
    begin
      SendMessage(E.Handle, EM_GETRECT, 0, LPARAM(@FormatRect));
      FirstLine := Integer(SendMessage(E.Handle, EM_GETFIRSTVISIBLELINE, 0, 0));
      LastLine := Min(Integer(SendMessage(E.Handle, EM_GETLINECOUNT, 0, 0)) - 1,
        FirstLine + Clip.Height div LineHeight + 2);
    end
    else
    begin
      FirstLine := 0;
      LastLine := 0;
    end;
    SetTextColor(Target, ColorToRGB(Ink));
    SetBkMode(Target, TRANSPARENT);
    Flags := ETO_CLIPPED;
    if E.BiDiMode = bdRightToLeft then
    begin
      Flags := Flags or ETO_RTLREADING;
      SetTextAlign(Target, TA_RIGHT or TA_TOP);
    end
    else SetTextAlign(Target, TA_LEFT or TA_TOP);
    for LineNo := FirstLine to LastLine do
    begin
      if Multiline then
      begin
        LineStart := Integer(SendMessage(E.Handle, EM_LINEINDEX, WPARAM(LineNo), 0));
        if LineStart < 0 then Continue;
        LineEnd := LineStart + Integer(SendMessage(E.Handle, EM_LINELENGTH, WPARAM(LineStart), 0));
        NextLine := Integer(SendMessage(E.Handle, EM_LINEINDEX, WPARAM(LineNo + 1), 0));
        if NextLine < 0 then NextLine := Length(Source);
      end
      else
      begin
        LineStart := 0;
        LineEnd := Length(Source);
        NextLine := LineEnd;
      end;
      if (EndSel <= LineStart) or (StartSel >= NextLine) then Continue;
      A := Max(StartSel, LineStart);
      B := Min(EndSel, LineEnd);
      if not CharPosition(A, PosA) then
      begin
        PosA.X := FormatRect.Left;
        PosA.Y := FormatRect.Top + (LineNo - FirstLine) * LineHeight;
      end;
      if not CharPosition(B, PosB) then
      begin
        PosB := PosA;
        if B > A then
        begin
          LastChar := B - 1;
          if (LastChar > A) and (Ord(Source[LastChar + 1]) >= $DC00) and
            (Ord(Source[LastChar + 1]) <= $DFFF) then Dec(LastChar);
          if CharPosition(LastChar, PosB) then
          begin
            Run := Copy(Source, LastChar + 1, B - LastChar);
            if E.PasswordChar <> #0 then Run := StringOfChar(E.PasswordChar, Length(Run));
            if E.BiDiMode = bdRightToLeft then Dec(PosB.X, C.TextWidth(Run))
            else Inc(PosB.X, C.TextWidth(Run));
          end;
        end;
      end;
      R := Rect(Min(PosA.X, PosB.X), PosA.Y, Max(PosA.X, PosB.X), PosA.Y + LineHeight);
      if Multiline and (EndSel > LineEnd) then R.Right := FormatRect.Right;
      if not IntersectRect(R, R, Clip) or R.IsEmpty then Continue;
      C.FillRect(R);
      // Native positions preserve horizontal scroll and tab-stop placement.
      RunStart := A;
      while RunStart < B do
      begin
        if Source[RunStart + 1] = #9 then
        begin
          Inc(RunStart);
          Continue;
        end;
        RunEnd := RunStart;
        while (RunEnd < B) and (Source[RunEnd + 1] <> #9) do Inc(RunEnd);
        Run := Copy(Source, RunStart + 1, RunEnd - RunStart);
        if E.PasswordChar <> #0 then Run := StringOfChar(E.PasswordChar, Length(Run));
        if CharPosition(RunStart, PosRun) then
          ExtTextOut(Target, PosRun.X, PosRun.Y, Flags, @R, PChar(Run), Cardinal(Length(Run)), nil);
        RunStart := RunEnd;
      end;
    end;
  finally
    RestoreDC(Target, SavedDC);
    C.Handle := 0;
    C.Free;
  end;
end;
function PaintBufferedSelection(Editor: TCustomEdit; var Message: TMessage): Boolean;
var DC: HDC; Info: TPaintStruct; Bitmap: TBitmap;
begin
  Result := (Message.Msg = WM_PAINT) and (Message.WParam = 0) and
    (Editor.Parent is TERDCustomControl);
  if not Result then Exit;
  DC := BeginPaint(Editor.Handle, Info);
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(Max(1, Editor.ClientWidth), Max(1, Editor.ClientHeight));
    Bitmap.Canvas.Brush.Color := TERDNativeEditAccess(Editor).Color;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
    TERDNativeEditAccess(Editor).PaintNativeClient(Bitmap.Canvas.Handle);
    DrawSelection(Editor, Bitmap.Canvas.Handle);
    BitBlt(DC, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
    Message.Result := 0;
  finally Bitmap.Free; EndPaint(Editor.Handle, Info) end;
end;

function SelectionInput(const Message: TMessage): Boolean;
begin
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, WM_SETTEXT, WM_KEYDOWN, WM_KEYUP, WM_CHAR,
    WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK, WM_TIMER, WM_HSCROLL, WM_VSCROLL, WM_MOUSEWHEEL,
    EM_SETSEL, WM_CUT, WM_PASTE, WM_CLEAR, WM_UNDO, EM_UNDO: Result := True;
    WM_MOUSEMOVE: Result := (Message.WParam and MK_LBUTTON) <> 0;
  else Result := False;
  end;
end;

function SuspendSelectionDrawing(Editor: TCustomEdit; const Message: TMessage;
  var Depth: Integer): Boolean;
begin
  Result := SelectionInput(Message) and Editor.HandleAllocated and
    (Editor.Parent is TERDCustomControl) and ((Depth > 0) or IsWindowVisible(Editor.Handle));
  // Establish initial mouse focus while WS_VISIBLE is still present.
  if Result and (Message.Msg = WM_LBUTTONDOWN) and not Editor.Focused and Editor.CanFocus then
    Editor.SetFocus;
  if not Result then Exit;
  Inc(Depth);
  if Depth = 1 then SendMessage(Editor.Handle, WM_SETREDRAW, 0, 0);
end;
procedure ResumeSelectionDrawing(Editor: TCustomEdit; var Depth: Integer);
begin
  Dec(Depth);
  if Depth <> 0 then Exit;
  SendMessage(Editor.Handle, WM_SETREDRAW, 1, 0);
  RedrawWindow(Editor.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
end;

procedure PaintSelectionOverlay(Editor: TCustomEdit; const Message: TMessage);
var DC: HDC;
begin
  if Message.Msg = WM_PRINTCLIENT then
    DrawSelection(Editor, HDC(Message.WParam))
  else if Message.Msg = WM_PAINT then
  begin
    if Message.WParam <> 0 then DrawSelection(Editor, HDC(Message.WParam))
    else
    begin
      // Let the native EDIT window perform its own BeginPaint/text rendering.
      // Overlay only the selected range after native painting has completed.
      DC := GetDC(Editor.Handle);
      if DC = 0 then Exit;
      try DrawSelection(Editor, DC) finally ReleaseDC(Editor.Handle, DC) end;
    end;
  end;
end;

procedure RefreshSelection(Editor: TCustomEdit; const Message: TMessage);
begin
  // Native selection painting happens during input handling. Finish the
  // native repaint and ERD overlay before returning to the message loop,
  // rather than leaving the system highlight visible until a queued paint.
  if not (Editor.Parent is TERDCustomControl) or not Editor.HandleAllocated then Exit;
  if (Message.Msg = WM_MOUSEMOVE) and ((Message.WParam and MK_LBUTTON) = 0) then Exit;
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, WM_SETTEXT, WM_KEYDOWN, WM_KEYUP, WM_CHAR,
    WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_HSCROLL, WM_VSCROLL,
    WM_MOUSEWHEEL, EM_SETSEL, WM_CUT, WM_PASTE, WM_CLEAR, WM_UNDO, EM_UNDO:
      RedrawWindow(Editor.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
  end;
end;

procedure TERDSelectionEdit.WndProc(var Message: TMessage);
var Suspended: Boolean;
begin
  if PaintBufferedSelection(Self, Message) then Exit;
  Suspended := SuspendSelectionDrawing(Self, Message, FRedrawDepth);
  try inherited finally
    if Suspended then ResumeSelectionDrawing(Self, FRedrawDepth);
  end;
  PaintSelectionOverlay(Self, Message);
  if not Suspended and (FRedrawDepth = 0) then RefreshSelection(Self, Message);
end;
procedure TERDSelectionMemo.WndProc(var Message: TMessage);
var Suspended: Boolean;
begin
  if PaintBufferedSelection(Self, Message) then Exit;
  Suspended := SuspendSelectionDrawing(Self, Message, FRedrawDepth);
  try inherited finally
    if Suspended then ResumeSelectionDrawing(Self, FRedrawDepth);
  end;
  PaintSelectionOverlay(Self, Message);
  if not Suspended and (FRedrawDepth = 0) then RefreshSelection(Self, Message);
end;
end.
