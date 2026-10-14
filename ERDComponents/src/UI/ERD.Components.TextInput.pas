unit ERD.Components.TextInput;
// Self-painted single-line preview. No EDIT child window or native selection.
interface
uses
  Winapi.Windows, Winapi.Messages, Winapi.Imm,
  System.Classes, System.SysUtils, System.Types, System.Math,
  Vcl.Controls, Vcl.Graphics, Vcl.Clipbrd, Vcl.ExtCtrls,
  ERD.Components.Control, ERD.Components.TextModel;
type
  TERDTextInput = class(TERDCustomControl)
  private
    FModel: TERDTextModel;
    FDragTimer: TTimer;
    FHint, FComposition: UnicodeString;
    FReadOnly, FPasswordVisible, FSelecting, FCaretCreated: Boolean;
    FPasswordChar, FPendingSurrogate: WideChar;
    FMaxLength, FScroll: Integer;
    FOnChange, FOnSelectionChange: TNotifyEvent;
    procedure DragTick(Sender: TObject);
    function GetInputText: UnicodeString;
    procedure SetText(const AValue: UnicodeString);
    procedure SetHint(const AValue: UnicodeString);
    procedure SetPasswordChar(AValue: WideChar);
    procedure SetPasswordVisible(AValue: Boolean);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetMaxLength(AValue: Integer);
    function GetSelStart: Integer;
    function GetSelLength: Integer;
    procedure SetSelStart(AValue: Integer);
    procedure SetSelLength(AValue: Integer);
    function DisplayText: UnicodeString;
    function DisplayPosition(APosition: Integer): Integer;
    function ModelPosition(APosition: Integer): Integer;
    procedure UpdateCaret;
    procedure Changed(const ABefore: UnicodeString);
    procedure MoveTo(APosition: Integer; AExtend: Boolean);
    function HitPosition(X: Integer): Integer;
    procedure InsertText(const AValue: UnicodeString);
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMIMEStart(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMIMEComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMIMEEnd(var Message: TMessage); message WM_IME_ENDCOMPOSITION;
    procedure WMUniChar(var Message: TMessage); message WM_UNICHAR;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DestroyWnd; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectAll;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure Redo;
    procedure ReplaceSelection(const AValue: UnicodeString);
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
  published
    property Text: UnicodeString read GetInputText write SetText;
    property TextHint: UnicodeString read FHint write SetHint;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property PasswordChar: WideChar read FPasswordChar write SetPasswordChar default #0;
    property PasswordVisible: Boolean read FPasswordVisible write SetPasswordVisible stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;
implementation
uses ERD.Components.Paint, ERD.Components.Types;
type
  // Opaque Uniscribe handle; declarations avoid dependency on SDK unit variants.
  TERDShapedLine = class
  private
    FAnalysis: Pointer;
    FText: UnicodeString;
  public
    constructor Create(ACanvas: TCanvas; const AText: UnicodeString);
    destructor Destroy; override;
    function X(APosition: Integer): Integer;
    function Position(AX: Integer): Integer;
    function Width: Integer;
    procedure Range(APosition, ALength: Integer; out ALeft, ARight: Integer);
    procedure Draw(AX, AY: Integer);
  end;
function ScriptStringAnalyse(DC: HDC; Text: Pointer; Count, Glyphs, Charset: Integer;
  Flags: DWORD; RequiredWidth: Integer; Control, State, DX, TabDef, InClass: Pointer;
  var Analysis: Pointer): HRESULT; stdcall; external 'usp10.dll';
function ScriptStringFree(var Analysis: Pointer): HRESULT; stdcall; external 'usp10.dll';
function ScriptStringCPtoX(Analysis: Pointer; Position: Integer; Trailing: BOOL;
  var X: Integer): HRESULT; stdcall; external 'usp10.dll';
function ScriptStringXtoCP(Analysis: Pointer; X: Integer; var Position, Trailing: Integer): HRESULT;
  stdcall; external 'usp10.dll';
function ScriptString_pSize(Analysis: Pointer): PSize; stdcall; external 'usp10.dll';
function ScriptStringOut(Analysis: Pointer; X, Y: Integer; Options: UINT;
  Rect: PRect; MinSel, MaxSel: Integer; Disabled: BOOL): HRESULT;
  stdcall; external 'usp10.dll';
constructor TERDShapedLine.Create(ACanvas: TCanvas; const AText: UnicodeString);
begin
  inherited Create; FText := AText;
  SelectObject(ACanvas.Handle, ACanvas.Font.Handle);
  if FText <> '' then
    if ScriptStringAnalyse(ACanvas.Handle, PWideChar(FText), Length(FText),
      Length(FText) * 3 div 2 + 16, -1, $80 or $20 or $1000, 0,
      nil, nil, nil, nil, nil, FAnalysis) < 0 then
      raise EInvalidOperation.Create('Unable to shape text');
end;
destructor TERDShapedLine.Destroy;
begin if FAnalysis <> nil then ScriptStringFree(FAnalysis); inherited; end;
function TERDShapedLine.X(APosition: Integer): Integer;
begin
  Result := 0;
  if FAnalysis <> nil then ScriptStringCPtoX(FAnalysis,
    EnsureRange(APosition, 0, Length(FText)), False, Result);
end;
function TERDShapedLine.Position(AX: Integer): Integer;
var Trailing: Integer;
begin
  Result := 0; Trailing := 0;
  if FAnalysis <> nil then
  begin
    ScriptStringXtoCP(FAnalysis, AX, Result, Trailing);
    Result := EnsureRange(Result + Trailing, 0, Length(FText));
  end;
end;
function TERDShapedLine.Width: Integer;
begin Result := 0; if FAnalysis <> nil then Result := ScriptString_pSize(FAnalysis)^.cx; end;
procedure TERDShapedLine.Range(APosition, ALength: Integer; out ALeft, ARight: Integer);
var LastX: Integer;
begin
  ALeft := X(APosition); LastX := ALeft;
  if (FAnalysis <> nil) and (ALength > 0) then
    ScriptStringCPtoX(FAnalysis, APosition + ALength - 1, True, LastX);
  ARight := Max(ALeft, LastX); ALeft := Min(ALeft, LastX);
end;
procedure TERDShapedLine.Draw(AX, AY: Integer);
begin if FAnalysis <> nil then ScriptStringOut(FAnalysis, AX, AY, 0, nil, -1, -1, False); end;
function SingleLine(const S: UnicodeString): UnicodeString;
begin
  Result := StringReplace(S, #13#10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #9, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #0, '', [rfReplaceAll]);
end;
constructor TERDTextInput.Create(AOwner: TComponent);
begin
  inherited; FModel := TERDTextModel.Create;
  Width := 220; Height := 28; TabStop := True;
  ParentFont := False; Font.Name := 'Segoe UI'; Font.Height := -Round(ERD_TEXT_BODY);
  FDragTimer := TTimer.Create(Self); FDragTimer.Enabled := False;
  FDragTimer.Interval := 50; FDragTimer.OnTimer := DragTick;
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks];
end;
destructor TERDTextInput.Destroy;
begin FreeAndNil(FModel); inherited; end;
function TERDTextInput.GetInputText: UnicodeString;
begin Result := FModel.Text; end;
procedure TERDTextInput.SetText(const AValue: UnicodeString);
var Before: UnicodeString;
begin
  Before := Text; FModel.SetText(SingleLine(AValue)); FComposition := '';
  Changed(Before);
end;
procedure TERDTextInput.SetHint(const AValue: UnicodeString);
begin FHint := AValue; Invalidate; end;
procedure TERDTextInput.SetPasswordChar(AValue: WideChar);
begin
  if (Ord(AValue) >= $D800) and (Ord(AValue) <= $DFFF) then AValue := WideChar($2022);
  FPasswordChar := AValue; FScroll := 0; UpdateCaret; Invalidate;
end;
procedure TERDTextInput.SetPasswordVisible(AValue: Boolean);
begin FPasswordVisible := AValue; FScroll := 0; UpdateCaret; Invalidate; end;
procedure TERDTextInput.SetReadOnly(AValue: Boolean);
begin FReadOnly := AValue; Invalidate; end;
procedure TERDTextInput.SetMaxLength(AValue: Integer);
begin FMaxLength := Max(0, AValue); end;
function TERDTextInput.GetSelStart: Integer;
begin Result := FModel.SelStart; end;
function TERDTextInput.GetSelLength: Integer;
begin Result := FModel.SelLength; end;
procedure TERDTextInput.SetSelStart(AValue: Integer);
begin FModel.Select(AValue, AValue); UpdateCaret; Invalidate; end;
procedure TERDTextInput.SetSelLength(AValue: Integer);
begin FModel.Select(SelStart, SelStart + Max(0, AValue)); UpdateCaret; Invalidate; end;
function TERDTextInput.DisplayText: UnicodeString;
begin
  if (FPasswordChar <> #0) and not FPasswordVisible then
    Result := StringOfChar(FPasswordChar, Length(FModel.Boundaries) - 1)
  else Result := Text;
end;
function TERDTextInput.DisplayPosition(APosition: Integer): Integer;
var I: Integer;
begin
  Result := APosition;
  if (FPasswordChar = #0) or FPasswordVisible then Exit;
  Result := 0;
  for I := 0 to High(FModel.Boundaries) do
    if FModel.Boundaries[I] <= APosition then Result := I;
end;
function TERDTextInput.ModelPosition(APosition: Integer): Integer;
begin
  if (FPasswordChar = #0) or FPasswordVisible then Result := FModel.Snap(APosition)
  else Result := FModel.Boundaries[EnsureRange(APosition, 0, High(FModel.Boundaries))];
end;
procedure TERDTextInput.UpdateCaret;
var Line: TERDShapedLine; X, H: Integer; C: HIMC; Form: TCompositionForm;
begin
  if not HandleAllocated or (FModel = nil) then Exit;
  Canvas.Font.Assign(Font); H := Canvas.TextHeight('Mg');
  Line := TERDShapedLine.Create(Canvas, DisplayText);
  try
    X := Line.X(DisplayPosition(FModel.Caret));
    if X - FScroll > ClientWidth - ScaleValue(12) then FScroll := X - ClientWidth + ScaleValue(12);
    if X < FScroll then FScroll := X;
    FScroll := EnsureRange(FScroll, 0, Max(0, Line.Width - ClientWidth + ScaleValue(12)));
    if FCaretCreated then SetCaretPos(ScaleValue(6) + X - FScroll, Max(2, (Height - H) div 2));
    if Focused then
    begin
      C := ImmGetContext(Handle);
      if C <> 0 then
      try
        FillChar(Form, SizeOf(Form), 0); Form.dwStyle := CFS_POINT;
        Form.ptCurrentPos := Point(ScaleValue(6) + X - FScroll, (Height + H) div 2);
        ImmSetCompositionWindow(C, @Form);
      finally ImmReleaseContext(Handle, C); end;
    end;
  finally Line.Free; end;
end;
procedure TERDTextInput.Changed(const ABefore: UnicodeString);
begin
  UpdateCaret; Invalidate;
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  if (ABefore <> Text) and Assigned(FOnChange) then FOnChange(Self);
end;
procedure TERDTextInput.MoveTo(APosition: Integer; AExtend: Boolean);
begin
  if AExtend then FModel.Select(FModel.Anchor, APosition)
  else FModel.Select(APosition, APosition);
  Changed(Text);
end;
function TERDTextInput.HitPosition(X: Integer): Integer;
var Line: TERDShapedLine;
begin
  Canvas.Font.Assign(Font); Line := TERDShapedLine.Create(Canvas, DisplayText);
  try Result := ModelPosition(Line.Position(X - ScaleValue(6) + FScroll));
  finally Line.Free; end;
end;
procedure TERDTextInput.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Line: TERDShapedLine; R, SR: TRect; Saved, SavedSelection, I, L, H, Y, TextHeight: Integer;
    S: UnicodeString; Ink: TColor;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.EditFrame(ClientRect, Focused, Enabled); Ink := P.ContrastInk(Palette.Accent);
  finally P.Free; end;
  R := ClientRect; InflateRect(R, -ScaleValue(4), -ScaleValue(3));
  Saved := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    ACanvas.Font.Assign(Font); ACanvas.Font.Color := Palette.ForegroundText;
    TextHeight := ACanvas.TextHeight('Mg');
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    SetTextColor(ACanvas.Handle, ColorToRGB(Palette.ForegroundText));
    S := DisplayText; Y := Max(R.Top, (Height - TextHeight) div 2);
    if (S = '') and (FComposition = '') then
    begin
      SetTextColor(ACanvas.Handle, ColorToRGB(Palette.Subtle));
      TextOutW(ACanvas.Handle, ScaleValue(6), Y, PWideChar(FHint), Length(FHint));
    end
    else
    begin
      Line := TERDShapedLine.Create(ACanvas, S);
      try
        Line.Draw(ScaleValue(6) - FScroll, Y);
        // Paint each selected grapheme's visual range; handles disjoint bidi runs.
        if Focused and (SelLength > 0) then
          for I := 0 to High(FModel.Boundaries) - 1 do
            if (FModel.Boundaries[I] >= SelStart) and
              (FModel.Boundaries[I + 1] <= SelStart + SelLength) then
            begin
              Line.Range(DisplayPosition(FModel.Boundaries[I]),
                DisplayPosition(FModel.Boundaries[I + 1]) -
                DisplayPosition(FModel.Boundaries[I]), L, H);
              SR := Rect(ScaleValue(6) + Min(L,H) - FScroll, Y,
                ScaleValue(6) + Max(L,H) - FScroll, Y + TextHeight);
              ACanvas.Brush.Color := Palette.Accent; ACanvas.FillRect(SR);
              SavedSelection := SaveDC(ACanvas.Handle);
              try
                IntersectClipRect(ACanvas.Handle, SR.Left, SR.Top, SR.Right, SR.Bottom);
                SetTextColor(ACanvas.Handle, ColorToRGB(Ink));
                Line.Draw(ScaleValue(6) - FScroll, Y);
              finally RestoreDC(ACanvas.Handle, SavedSelection); end;
            end;
        if FComposition <> '' then
        begin
          L := ScaleValue(6) + Line.X(DisplayPosition(FModel.Caret)) - FScroll;
          S := FComposition;
          if (FPasswordChar <> #0) and not FPasswordVisible then
            S := StringOfChar(FPasswordChar, Length(ERDGraphemeBoundaries(S)) - 1);
          TextOutW(ACanvas.Handle, L, Y, PWideChar(S), Length(S));
          ACanvas.Pen.Color := Palette.Accent;
          ACanvas.MoveTo(L, Y + TextHeight);
          ACanvas.LineTo(L + ACanvas.TextWidth(S), Y + TextHeight);
        end;
      finally Line.Free; end;
    end;
  finally RestoreDC(ACanvas.Handle, Saved); end;
end;
procedure TERDTextInput.KeyDown(var Key: Word; Shift: TShiftState);
var Pos: Integer; Before: UnicodeString;
begin
  inherited; if Key = 0 then Exit;
  Before := Text;
  if (ssCtrl in Shift) and not (Key in [VK_LEFT, VK_RIGHT, VK_BACK, VK_DELETE]) then
  begin
    case Key of
      Ord('A'): SelectAll;
      Ord('C'): CopyToClipboard;
      Ord('X'): CutToClipboard;
      Ord('V'): PasteFromClipboard;
      Ord('Z'): if ssShift in Shift then Redo else Undo;
      Ord('Y'): Redo;
    else Exit;
    end;
    Key := 0; Exit;
  end;
  case Key of
    VK_LEFT:
      begin
        if ssCtrl in Shift then Pos := FModel.PreviousWord(FModel.Caret)
        else Pos := FModel.Previous(FModel.Caret);
        if (SelLength > 0) and not (ssShift in Shift) then Pos := SelStart;
        MoveTo(Pos, ssShift in Shift);
      end;
    VK_RIGHT:
      begin
        if ssCtrl in Shift then Pos := FModel.NextWord(FModel.Caret)
        else Pos := FModel.Next(FModel.Caret);
        if (SelLength > 0) and not (ssShift in Shift) then Pos := SelStart + SelLength;
        MoveTo(Pos, ssShift in Shift);
      end;
    VK_HOME: MoveTo(0, ssShift in Shift);
    VK_END: MoveTo(Length(Text), ssShift in Shift);
    VK_BACK: if not ReadOnly then
      begin
        if (ssCtrl in Shift) and (SelLength = 0) then
          FModel.Select(FModel.PreviousWord(FModel.Caret), FModel.Caret);
        FModel.DeleteBackward;
      end;
    VK_DELETE: if not ReadOnly then
      begin
        if (ssCtrl in Shift) and (SelLength = 0) then
          FModel.Select(FModel.Caret, FModel.NextWord(FModel.Caret));
        FModel.DeleteForward;
      end;
  else Exit;
  end;
  Key := 0; Changed(Before);
end;
procedure TERDTextInput.InsertText(const AValue: UnicodeString);
var Before: UnicodeString;
begin
  if ReadOnly or not Enabled then Exit;
  Before := Text; FModel.ReplaceSelection(SingleLine(AValue), FMaxLength); Changed(Before);
end;
procedure TERDTextInput.WMChar(var Message: TWMChar);
var C: Char;
begin
  C := WideChar(Message.CharCode);
  if Ord(C) < 32 then Exit;
  KeyPress(C); if C = #0 then Exit;
  if (Ord(C) >= $D800) and (Ord(C) <= $DBFF) then
  begin FPendingSurrogate := C; Exit; end;
  if FPendingSurrogate <> #0 then
  begin
    if (Ord(C) >= $DC00) and (Ord(C) <= $DFFF) then
      InsertText(UnicodeString(FPendingSurrogate) + C)
    else InsertText(UnicodeString(WideChar($FFFD)) + C);
    FPendingSurrogate := #0;
  end
  else InsertText(C);
  Message.Result := 0;
end;
procedure TERDTextInput.WMUniChar(var Message: TMessage);
var C: Cardinal;
begin
  C := Cardinal(Message.WParam);
  if C = $FFFF then begin Message.Result := 1; Exit; end;
  if C > $10FFFF then Exit;
  if C >= $10000 then
  begin
    Dec(C, $10000);
    InsertText(UnicodeString(WideChar($D800 + (C shr 10))) + WideChar($DC00 + (C and $3FF)));
  end
  else if C >= 32 then InsertText(WideChar(C));
  Message.Result := 0;
end;
procedure TERDTextInput.WMIMEStart(var Message: TMessage);
begin FComposition := ''; UpdateCaret; Message.Result := 0; end;
procedure TERDTextInput.WMIMEComposition(var Message: TMessage);
var C: HIMC; S: UnicodeString; N: Longint;
  function Composition(AKind: DWORD): UnicodeString;
  begin
    N := ImmGetCompositionStringW(C, AKind, nil, 0);
    Result := '';
    if N > 0 then
    begin SetLength(Result, N div SizeOf(WideChar)); ImmGetCompositionStringW(C, AKind, PWideChar(Result), N); end;
  end;
begin
  if ReadOnly then Exit;
  C := ImmGetContext(Handle);
  if C <> 0 then
  try
    if (Message.LParam and GCS_RESULTSTR) <> 0 then
    begin S := Composition(GCS_RESULTSTR); FComposition := ''; InsertText(S); end;
    if (Message.LParam and GCS_COMPSTR) <> 0 then FComposition := Composition(GCS_COMPSTR);
  finally ImmReleaseContext(Handle, C); end;
  UpdateCaret; Invalidate; Message.Result := 0;
end;
procedure TERDTextInput.WMIMEEnd(var Message: TMessage);
begin FComposition := ''; Invalidate; inherited; end;
procedure TERDTextInput.WMGetDlgCode(var Message: TMessage);
begin inherited; Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS; end;
procedure TERDTextInput.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  SetFocus; MoveTo(HitPosition(X), ssShift in Shift);
  FSelecting := True; MouseCapture := True; FDragTimer.Enabled := True;
end;
procedure TERDTextInput.MouseMove(Shift: TShiftState; X, Y: Integer);
begin inherited; if FSelecting and MouseCapture then MoveTo(HitPosition(X), True); end;
procedure TERDTextInput.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then begin FSelecting := False; MouseCapture := False; FDragTimer.Enabled := False; end;
end;
procedure TERDTextInput.DragTick(Sender: TObject);
var Point: TPoint;
begin
  if not FSelecting or not MouseCapture then begin FDragTimer.Enabled := False; Exit; end;
  GetCursorPos(Point); Point := ScreenToClient(Point);
  if Point.X < ScaleValue(4) then MoveTo(FModel.Previous(FModel.Caret), True)
  else if Point.X > ClientWidth - ScaleValue(4) then MoveTo(FModel.Next(FModel.Caret), True);
end;
procedure TERDTextInput.DblClick;
var L, H: Integer; Kind: Byte;
begin
  inherited;
  L := FModel.Caret; H := L; Kind := FModel.WordKind(L);
  while (L > 0) and (FModel.WordKind(FModel.Previous(L)) = Kind) do L := FModel.Previous(L);
  while (H < Length(Text)) and (FModel.WordKind(H) = Kind) do H := FModel.Next(H);
  FModel.Select(L, H); Changed(Text);
end;
procedure TERDTextInput.DoEnter;
begin
  inherited;
  Canvas.Font.Assign(Font);
  FCaretCreated := CreateCaret(Handle, 0, ScaleValue(1), Canvas.TextHeight('Mg'));
  UpdateCaret; if FCaretCreated then ShowCaret(Handle); Invalidate;
end;
procedure TERDTextInput.DoExit;
begin
  FSelecting := False; MouseCapture := False; FDragTimer.Enabled := False; FPendingSurrogate := #0; FComposition := '';
  if FCaretCreated then begin DestroyCaret; FCaretCreated := False; end;
  inherited; Invalidate;
end;
procedure TERDTextInput.DestroyWnd;
begin
  if FCaretCreated then begin DestroyCaret; FCaretCreated := False; end;
  inherited;
end;
procedure TERDTextInput.Resize;
begin inherited; UpdateCaret; end;
procedure TERDTextInput.SelectAll;
begin FModel.Select(0, Length(Text)); Changed(Text); end;
procedure TERDTextInput.CopyToClipboard;
begin
  if (FPasswordChar <> #0) or (SelLength = 0) then Exit;
  Clipboard.AsText := Copy(Text, SelStart + 1, SelLength);
end;
procedure TERDTextInput.CutToClipboard;
begin
  if ReadOnly or (FPasswordChar <> #0) or (SelLength = 0) then Exit;
  CopyToClipboard; InsertText('');
end;
procedure TERDTextInput.PasteFromClipboard;
begin if not ReadOnly and Clipboard.HasFormat(CF_UNICODETEXT) then InsertText(Clipboard.AsText); end;
procedure TERDTextInput.Undo;
var Before: UnicodeString;
begin if ReadOnly then Exit; Before := Text; FModel.Undo; Changed(Before); end;
procedure TERDTextInput.Redo;
var Before: UnicodeString;
begin if ReadOnly then Exit; Before := Text; FModel.Redo; Changed(Before); end;
procedure TERDTextInput.ReplaceSelection(const AValue: UnicodeString);
begin InsertText(AValue); end;
end.
