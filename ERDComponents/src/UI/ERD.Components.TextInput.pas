unit ERD.Components.TextInput;
// Self-painted single-line and multiline Unicode input. No Windows EDIT child.
{$B-}
interface
uses
  Winapi.Windows, Winapi.Messages, Winapi.Imm,
  System.Classes, System.SysUtils, System.Types, System.Math,
  Vcl.Controls, Vcl.Graphics, Vcl.Clipbrd, Vcl.ExtCtrls, Vcl.StdCtrls, System.UITypes,
  ERD.Components.TextLayout, ERD.Components.Control, ERD.Components.TextModel, ERD.Components.Menus;
type
  // Shared colour-capable layout for every single-line and multiline consumer.
  TERDTextLayoutLine = class
  private
    FDirectWrite: TERDDirectWriteLine;
  public
    constructor Create(ACanvas: TCanvas; const AText: UnicodeString);
    destructor Destroy; override;
    function X(APosition: Integer): Integer;
    function Width: Integer;
    procedure Range(APosition, ALength: Integer; out ALeft, ARight: Integer);
    procedure Draw(AX, AY: Integer);
  end;
  TERDTextInput = class(TERDCustomControl)
  private
    FModel: TERDTextModel;
    FBorderStyle: TBorderStyle;
    FAlignment: TAlignment;
    FCharCase: TEditCharCase;
    FNumbersOnly, FAutoSelect: Boolean;
    FCreatingWindow, FBackspaceHandled: Boolean;
    FHideSelection, FModified, FSettingText: Boolean;
    FWordSelecting: Boolean;
    FWordStart, FWordEnd: Integer;
    FWordText: UnicodeString;
    FWordAttributes: TArray<Byte>;
    FDragTimer: TTimer;
    FContextMenu: TERDPopupMenu;
    FHint, FComposition: UnicodeString;
    FReadOnly, FPasswordVisible, FSelecting, FCaretCreated: Boolean;
    FPasswordChar, FPendingSurrogate: WideChar;
    FMaxLength, FScroll: Integer;
    FOnChange, FOnSelectionChange: TNotifyEvent;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function SelectionFill: TColor;
    procedure SetHideSelection(Value: Boolean);
    function TextInset: Integer;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetAlignment(Value: TAlignment);
    function GetSelText: UnicodeString;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure EnsureWordAttributes;
    function WordLeft(Position: Integer): Integer;
    function WordRight(Position: Integer): Integer;
    procedure SelectWord(Position: Integer);
    procedure ExtendSelection(Position: Integer);
    procedure EditMenuClick(Sender: TObject);
    function GetInputText: UnicodeString;
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
    function HitPosition(X: Integer): Integer;
    procedure InsertText(const AValue: UnicodeString);
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMIMEStart(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMIMEComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMIMEEnd(var Message: TMessage); message WM_IME_ENDCOMPOSITION;
    procedure WMUniChar(var Message: TMessage); message WM_UNICHAR;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    procedure MoveTo(APosition: Integer; AExtend: Boolean);
    function LineOrigin(AWidth: Integer): Integer; virtual;
    property TextModel: TERDTextModel read FModel;
    procedure SetInputText(const AValue: UnicodeString); virtual;
    function NormalizeInput(const Value: UnicodeString): UnicodeString; virtual;
    procedure UpdateCaret; virtual;
    procedure Changed(const ABefore: UnicodeString); virtual;
    procedure DragTick(Sender: TObject); virtual;
    procedure CreateWnd; override;
    procedure BeforeEditCommand(MessageID: Cardinal); virtual;
    procedure WndProc(var Message: TMessage); override;
    function DefaultEditMenu: TERDPopupMenu;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
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
    procedure Clear;
    procedure ClearSelection;
    procedure ClearUndo;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property Modified: Boolean read FModified write FModified;
    procedure SelectAll;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure Redo;
    procedure ReplaceSelection(const AValue: UnicodeString);
    property SelText: UnicodeString read GetSelText write ReplaceSelection;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
  published
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property NumbersOnly: Boolean read FNumbersOnly write FNumbersOnly default False;
    property CharCase: TEditCharCase read FCharCase write FCharCase default ecNormal;
    property Color;
    property ParentColor;
    property AutoSize;
    property Text: UnicodeString read GetInputText write SetInputText;
    property TextHint: UnicodeString read FHint write SetHint;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property PasswordChar: WideChar read FPasswordChar write SetPasswordChar default #0;
    property PasswordVisible: Boolean read FPasswordVisible write SetPasswordVisible stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;
type
  TERDTextRow = record
    Start, Count: Integer;
  end;
  /// Multiline view of the same Unicode document, selection and undo model.
  TERDTextArea = class(TERDTextInput)
  private
    FLines: TStringList;
    FSyncLines, FWordWrap, FWantTabs, FWantReturns: Boolean;
    FRows: TArray<TERDTextRow>;
    FLayoutText: UnicodeString;
    FLayoutWidth: Integer;
    FLayoutFont: HFONT;
    FFirstRow, FPreferredX: Integer;
    function RowForPosition(Position: Integer): Integer;
    function RowText(Index: Integer): UnicodeString;
    function HitRow(X, Y: Integer): Integer;
    function GetLines: TStrings;
    procedure LinesChanged(Sender: TObject);
    procedure SetLines(Value: TStrings);
    procedure SetWordWrap(Value: Boolean);
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  protected
    function TextViewport: TRect; virtual;
    function LineOrigin(AWidth: Integer): Integer; override;
    procedure BuildRows;
    function RowHeight: Integer;
    procedure ScrollRows(Value: Integer);
    procedure InvalidateTextLayout;
    function VisualRows: TArray<TERDTextRow>;
    procedure FilterRows(var Rows: TArray<TERDTextRow>); virtual;
    procedure PaintRowBackground(ACanvas: TCanvas; Row: Integer; const R: TRect); virtual;
    procedure PaintTextBackground(ACanvas: TCanvas; Start, Count, X, Y: Integer;
      Line: TERDTextLayoutLine); virtual;
    procedure PaintTextOverlay(ACanvas: TCanvas; Start, Count, X, Y: Integer;
      Line: TERDTextLayoutLine); virtual;
    procedure PaintTextLine(ACanvas: TCanvas; Start, Count, X, Y: Integer;
      Line: TERDTextLayoutLine); virtual;
    procedure SetInputText(const AValue: UnicodeString); override;
    function NormalizeInput(const Value: UnicodeString): UnicodeString; override;
    procedure UpdateCaret; override;
    procedure Changed(const ABefore: UnicodeString); override;
    procedure DragTick(Sender: TObject); override;
    procedure WndProc(var Message: TMessage); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function VisibleRowCount: Integer;
    property FirstVisibleRow: Integer read FFirstRow;
  published
    property Lines: TStrings read GetLines write SetLines;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
    property WantTabs: Boolean read FWantTabs write FWantTabs default False;
    property WantReturns: Boolean read FWantReturns write FWantReturns default True;
  end;

implementation
uses System.Generics.Collections, Vcl.Menus, ERD.Components.Paint, ERD.Components.Types;
type
  TERDTextLines = class(TStringList)
  private
    FInput: TERDTextArea;
  protected
    function GetTextStr: string; override;
    procedure SetTextStr(const Value: string); override;
  public
    constructor Create(Input: TERDTextArea); reintroduce;
  end;
  TERDTextControlAccess = class(TControl)
  public
    property PopupMenu;
  end;
function ScriptBreak(Text: PWideChar; Count: Integer; Analysis, Attributes: Pointer): HRESULT;
  stdcall; external 'usp10.dll';
constructor TERDTextLayoutLine.Create(ACanvas: TCanvas; const AText: UnicodeString);
begin
  inherited Create;
  SelectObject(ACanvas.Handle, ACanvas.Font.Handle);
  FDirectWrite := TERDDirectWriteLine.Create(ACanvas, AText);
end;
destructor TERDTextLayoutLine.Destroy;
begin FDirectWrite.Free; inherited; end;
function TERDTextLayoutLine.X(APosition: Integer): Integer;
begin Result := FDirectWrite.X(APosition); end;
function TERDTextLayoutLine.Width: Integer;
begin Result := FDirectWrite.Width; end;
procedure TERDTextLayoutLine.Range(APosition, ALength: Integer; out ALeft, ARight: Integer);
begin FDirectWrite.Range(APosition, ALength, ALeft, ARight); end;
procedure TERDTextLayoutLine.Draw(AX, AY: Integer);
begin FDirectWrite.Draw(AX, AY); end;
function ERDDrawLayoutText(ACanvas: TCanvas; const Text: UnicodeString; X, Y: Integer): Integer;
var Line: TERDTextLayoutLine;
begin
  Line := TERDTextLayoutLine.Create(ACanvas, Text);
  try Line.Draw(X, Y); Result := Line.Width; finally Line.Free; end;
end;
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
  inherited;
  FModel := TERDTextModel.Create;
  FBorderStyle := bsSingle;
  FHideSelection := True;
  Cursor := crIBeam;
  FAlignment := taLeftJustify;
  FCharCase := ecNormal;
  Width := 220; Height := 28; TabStop := True;
  ParentFont := False; Font.Name := 'Segoe UI'; Font.Height := -Round(ERD_TEXT_BODY);
  FDragTimer := TTimer.Create(Self); FDragTimer.Enabled := False;
  FDragTimer.Interval := 50; FDragTimer.OnTimer := DragTick;
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks];
end;
destructor TERDTextInput.Destroy;
begin
  if FDragTimer <> nil then FDragTimer.Enabled := False;
  OnEnter := nil; OnExit := nil; OnKeyDown := nil; OnKeyPress := nil; OnKeyUp := nil;
  FOnChange := nil; FOnSelectionChange := nil;
  FreeAndNil(FModel); inherited;
end;
function TERDTextInput.GetCanUndo: Boolean;
begin Result := FModel.CanUndo; end;
function TERDTextInput.GetCanRedo: Boolean;
begin Result := FModel.CanRedo; end;
function TERDTextInput.SelectionFill: TColor;
begin
  if Focused then Result := Palette.Accent
  else Result := ERDMixColor(Palette.Accent, Palette.GaugeFace, 0.2);
end;
procedure TERDTextInput.SetHideSelection(Value: Boolean);
begin FHideSelection := Value; Invalidate; end;
function TERDTextInput.TextInset: Integer;
begin if FBorderStyle = bsNone then Result := ScaleValue(2) else Result := ScaleValue(6); end;
function TERDTextInput.LineOrigin(AWidth: Integer): Integer;
var Available: Integer;
begin
  Result := TextInset; Available := Max(0, ClientWidth - 2 * Result);
  if AWidth <= Available then
    case FAlignment of
      taRightJustify: Inc(Result, Available - AWidth);
      taCenter: Inc(Result, (Available - AWidth) div 2);
    end;
end;
procedure TERDTextInput.SetBorderStyle(Value: TBorderStyle);
begin FBorderStyle := Value; UpdateCaret; Invalidate; end;
procedure TERDTextInput.SetAlignment(Value: TAlignment);
begin FAlignment := Value; FScroll := 0; UpdateCaret; Invalidate; end;
function TERDTextInput.GetSelText: UnicodeString;
begin Result := Copy(Text, SelStart + 1, SelLength); end;
procedure TERDTextInput.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if FModel = nil then Exit;
  if FCaretCreated then
  begin
    DestroyCaret; Canvas.Font.Assign(Font);
    FCaretCreated := CreateCaret(Handle, 0, ScaleValue(1), Canvas.TextHeight('Mg'));
    if FCaretCreated then ShowCaret(Handle);
  end;
  UpdateCaret; Invalidate;
end;
function TERDTextInput.NormalizeInput(const Value: UnicodeString): UnicodeString;
begin Result := SingleLine(Value); end;
procedure TERDTextInput.CreateWnd;
begin
  FCreatingWindow := True;
  try inherited; finally FCreatingWindow := False; end;
end;
procedure TERDTextInput.BeforeEditCommand(MessageID: Cardinal);
begin end;
procedure TERDTextInput.WndProc(var Message: TMessage);
var A, B, Count: Integer;
begin
  if FModel <> nil then
    case Message.Msg of
      WM_GETTEXTLENGTH: begin Message.Result := Length(Text); Exit; end;
      WM_GETTEXT:
        begin
          Message.Result := 0;
          if (Message.WParam = 0) or (Message.LParam = 0) then Exit;
          if Message.WParam - 1 > WPARAM(MaxInt) then Count := Length(Text)
          else Count := Min(Length(Text), Integer(Message.WParam - 1));
          if Count > 0 then Move(PWideChar(Text)^, PWideChar(Message.LParam)^, Count * SizeOf(WideChar));
          PWideChar(Message.LParam)[Count] := #0; Message.Result := Count; Exit;
        end;
      WM_SETTEXT:
        begin
          if not FCreatingWindow then
          begin
            if Message.LParam = 0 then Text := '' else Text := PWideChar(Message.LParam);
            Message.Result := 1; Exit;
          end;
        end;
      WM_COPY: begin CopyToClipboard; Message.Result := 0; Exit; end;
      WM_CUT: begin CutToClipboard; Message.Result := 0; Exit; end;
      WM_PASTE: begin PasteFromClipboard; Message.Result := 0; Exit; end;
      WM_CLEAR: begin ReplaceSelection(''); Message.Result := 0; Exit; end;
      WM_UNDO, EM_UNDO: begin Undo; Message.Result := 1; Exit; end;
      EM_GETMODIFY: begin Message.Result := Ord(FModified); Exit; end;
      EM_SETMODIFY: begin FModified := Message.WParam <> 0; Message.Result := 0; Exit; end;
      EM_EMPTYUNDOBUFFER: begin ClearUndo; Message.Result := 0; Exit; end;
      EM_CANUNDO: begin Message.Result := Ord(FModel.CanUndo); Exit; end;
      EM_SETSEL:
        begin
          A := Integer(Message.WParam); B := Integer(Message.LParam);
          if A = -1 then FModel.Select(FModel.Caret, FModel.Caret)
          else
          begin
            if B = -1 then B := Length(Text);
            FModel.Select(A, B);
          end;
          Changed(Text); Message.Result := 0; Exit;
        end;
      EM_GETSEL:
        begin
          if Message.WParam <> 0 then PInteger(Message.WParam)^ := SelStart;
          if Message.LParam <> 0 then PInteger(Message.LParam)^ := SelStart + SelLength;
          if (SelStart > $FFFF) or (SelStart + SelLength > $FFFF) then
          begin Message.Result := -1; Exit; end;
          Message.Result := LRESULT((Cardinal(SelStart) and $FFFF) or
            ((Cardinal(SelStart + SelLength) and $FFFF) shl 16)); Exit;
        end;
      EM_REPLACESEL:
        begin
          if Message.LParam <> 0 then ReplaceSelection(PWideChar(Message.LParam));
          Message.Result := 0; Exit;
        end;
    end;
  inherited;
end;
function TERDTextInput.GetInputText: UnicodeString;
begin Result := FModel.Text; end;
procedure TERDTextInput.SetInputText(const AValue: UnicodeString);
var Before: UnicodeString;
begin
  Before := Text; FSettingText := True; FModified := False;
  try
    FModel.SetText(NormalizeInput(AValue)); FComposition := ''; Changed(Before);
  finally FSettingText := False; FModified := False; end;
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
procedure TERDTextInput.UpdateCaret;
var Line: TERDTextLayoutLine; X, H: Integer; C: HIMC; Form: TCompositionForm;
begin
  if not HandleAllocated or (FModel = nil) then Exit;
  Canvas.Font.Assign(Font); H := Canvas.TextHeight('Mg');
  Line := TERDTextLayoutLine.Create(Canvas, DisplayText);
  try
    X := Line.X(DisplayPosition(FModel.Caret));
    if X - FScroll > ClientWidth - 2 * TextInset then FScroll := X - ClientWidth + 2 * TextInset;
    if X < FScroll then FScroll := X;
    FScroll := EnsureRange(FScroll, 0, Max(0, Line.Width - ClientWidth + 2 * TextInset));
    if FCaretCreated then SetCaretPos(LineOrigin(Line.Width) + X - FScroll, Max(2, (Height - H) div 2));
    if Focused then
    begin
      C := ImmGetContext(Handle);
      if C <> 0 then
      try
        FillChar(Form, SizeOf(Form), 0); Form.dwStyle := CFS_POINT;
        Form.ptCurrentPos := Point(LineOrigin(Line.Width) + X - FScroll, (Height + H) div 2);
        ImmSetCompositionWindow(C, @Form);
      finally ImmReleaseContext(Handle, C); end;
    end;
  finally Line.Free; end;
end;
procedure TERDTextInput.Changed(const ABefore: UnicodeString);
begin
  if (ABefore <> Text) and not FSettingText then FModified := True;
  UpdateCaret; Invalidate;
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  if (ABefore <> Text) and Assigned(FOnChange) then FOnChange(Self);
end;
procedure TERDTextInput.EnsureWordAttributes;
var Analysis: Cardinal;
begin
  if (FWordText = Text) and (Length(FWordAttributes) = Length(Text)) then Exit;
  FWordText := Text; SetLength(FWordAttributes, Length(Text));
  if Text = '' then Exit;
  // SCRIPT_ANALYSIS is two 16-bit bitfields. Zero selects SCRIPT_UNDEFINED;
  // SCRIPT_LOGATTR is one byte. Its low bit is fSoftBreak, as used by EDIT.
  Analysis := 0;
  if ScriptBreak(PWideChar(Text), Length(Text), @Analysis, @FWordAttributes[0]) < 0 then
    raise EInvalidOperation.Create('Unable to find text word boundaries');
end;
function TERDTextInput.WordLeft(Position: Integer): Integer;
begin
  EnsureWordAttributes; Result := EnsureRange(Position, 0, Length(Text));
  if Result > 0 then Dec(Result);
  while (Result > 0) and ((FWordAttributes[Result] and 1) = 0) do Dec(Result);
  Result := FModel.Snap(Result);
end;
function TERDTextInput.WordRight(Position: Integer): Integer;
begin
  EnsureWordAttributes; Result := EnsureRange(Position, 0, Length(Text));
  while (Result < Length(Text)) and ((FWordAttributes[Result] and 1) = 0) do Inc(Result);
  if FModel.Snap(Result) <> Result then Result := FModel.Next(Result);
end;
procedure TERDTextInput.SelectWord(Position: Integer);
begin
  Position := FModel.Snap(Position);
  FWordStart := WordLeft(FModel.Next(Position));
  FWordEnd := WordRight(FModel.Next(Position));
  FWordSelecting := True; FModel.Select(FWordStart, FWordEnd); Changed(Text);
end;
procedure TERDTextInput.ExtendSelection(Position: Integer);
var Start, Finish: Integer;
begin
  if not FWordSelecting then begin MoveTo(Position, True); Exit; end;
  Start := WordLeft(FModel.Next(Position)); Finish := WordRight(FModel.Next(Position));
  if Position < FWordStart then FModel.Select(FWordEnd, Start)
  else if Position > FWordEnd then FModel.Select(FWordStart, Finish)
  else FModel.Select(FWordStart, FWordEnd);
  Changed(Text);
end;
procedure TERDTextInput.MoveTo(APosition: Integer; AExtend: Boolean);
begin
  if AExtend then FModel.Select(FModel.Anchor, APosition)
  else FModel.Select(APosition, APosition);
  Changed(Text);
end;
function TERDTextInput.HitPosition(X: Integer): Integer;
var Line: TERDTextLayoutLine; Boundary, Distance, BestDistance, TextX: Integer;
begin
  Canvas.Font.Assign(Font); Line := TERDTextLayoutLine.Create(Canvas, DisplayText);
  try
    Result := 0; BestDistance := MaxInt;
    TextX := X - LineOrigin(Line.Width) + FScroll;
    // Use exactly the stops used by caret painting, including emoji clusters
    // and the end-of-line trailing edge. Never hit the middle of a grapheme.
    for Boundary in FModel.Boundaries do
    begin
      Distance := Abs(Line.X(DisplayPosition(Boundary)) - TextX);
      if Distance < BestDistance then
      begin BestDistance := Distance; Result := Boundary; end;
    end;
  finally Line.Free; end;
end;
procedure TERDTextInput.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Line: TERDTextLayoutLine; R, SR: TRect; Saved, SavedSelection, I, L, H, Y, TextHeight: Integer;
    S: UnicodeString; Ink, Fill: TColor;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if FBorderStyle = bsSingle then P.EditFrame(ClientRect, Focused, Enabled)
    else P.FillRect(ClientRect, Color);
    Fill := SelectionFill; Ink := P.ContrastInk(Fill);
    if not Focused then
      if BorderStyle = bsNone then Ink := Font.Color else Ink := Palette.ForegroundText;
  finally P.Free; end;
  R := ClientRect;
  if FBorderStyle = bsSingle then InflateRect(R, -ScaleValue(4), -ScaleValue(3));
  Saved := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    ACanvas.Font.Assign(Font); if FBorderStyle = bsNone then ACanvas.Font.Color := Font.Color
    else ACanvas.Font.Color := Palette.ForegroundText;
    TextHeight := ACanvas.TextHeight('Mg');
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    SetTextColor(ACanvas.Handle, ColorToRGB(ACanvas.Font.Color));
    S := DisplayText; Y := Max(R.Top, (Height - TextHeight) div 2);
    if (S = '') and (FComposition = '') then
    begin
      SetTextColor(ACanvas.Handle, ColorToRGB(Palette.Subtle));
      ERDDrawLayoutText(ACanvas, FHint, TextInset, Y);
    end
    else
    begin
      Line := TERDTextLayoutLine.Create(ACanvas, S);
      try
        Line.Draw(LineOrigin(Line.Width) - FScroll, Y);
        // Paint each selected grapheme's visual range; handles disjoint bidi runs.
        if (Focused or not FHideSelection) and (SelLength > 0) then
          for I := 0 to High(FModel.Boundaries) - 1 do
            if (FModel.Boundaries[I] >= SelStart) and
              (FModel.Boundaries[I + 1] <= SelStart + SelLength) then
            begin
              Line.Range(DisplayPosition(FModel.Boundaries[I]),
                DisplayPosition(FModel.Boundaries[I + 1]) -
                DisplayPosition(FModel.Boundaries[I]), L, H);
              SR := Rect(LineOrigin(Line.Width) + Min(L,H) - FScroll, Y,
                LineOrigin(Line.Width) + Max(L,H) - FScroll, Y + TextHeight);
              ACanvas.Brush.Color := Fill; ACanvas.FillRect(SR);
              SavedSelection := SaveDC(ACanvas.Handle);
              try
                IntersectClipRect(ACanvas.Handle, SR.Left, SR.Top, SR.Right, SR.Bottom);
                SetTextColor(ACanvas.Handle, ColorToRGB(Ink));
                Line.Draw(LineOrigin(Line.Width) - FScroll, Y);
              finally RestoreDC(ACanvas.Handle, SavedSelection); end;
            end;
        if FComposition <> '' then
        begin
          L := LineOrigin(Line.Width) + Line.X(DisplayPosition(FModel.Caret)) - FScroll;
          S := FComposition;
          if (FPasswordChar <> #0) and not FPasswordVisible then
            S := StringOfChar(FPasswordChar, Length(ERDGraphemeBoundaries(S)) - 1);
          H := ERDDrawLayoutText(ACanvas, S, L, Y);
          ACanvas.Pen.Color := Palette.Accent;
          ACanvas.MoveTo(L, Y + TextHeight);
          ACanvas.LineTo(L + H, Y + TextHeight);
        end;
      finally Line.Free; end;
    end;
  finally RestoreDC(ACanvas.Handle, Saved); end;
end;
procedure TERDTextInput.KeyDown(var Key: Word; Shift: TShiftState);
var Pos: Integer; Before: UnicodeString;
begin
  FBackspaceHandled := False;
  inherited; if Key = 0 then Exit;
  Before := Text;
  if (Key = VK_INSERT) and (ssCtrl in Shift) then
  begin CopyToClipboard; Key := 0; Exit; end;
  if (Key = VK_INSERT) and (ssShift in Shift) then
  begin PasteFromClipboard; Key := 0; Exit; end;
  if (Key = VK_DELETE) and (ssShift in Shift) and not (ssCtrl in Shift) then
  begin CutToClipboard; Key := 0; Exit; end;
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
        if ssCtrl in Shift then Pos := WordLeft(FModel.Caret)
        else Pos := FModel.Previous(FModel.Caret);
        if (SelLength > 0) and not (ssShift in Shift) then Pos := SelStart;
        MoveTo(Pos, ssShift in Shift);
      end;
    VK_RIGHT:
      begin
        if ssCtrl in Shift then Pos := WordRight(FModel.Next(FModel.Caret))
        else Pos := FModel.Next(FModel.Caret);
        if (SelLength > 0) and not (ssShift in Shift) then Pos := SelStart + SelLength;
        MoveTo(Pos, ssShift in Shift);
      end;
    VK_HOME: MoveTo(0, ssShift in Shift);
    VK_END: MoveTo(Length(Text), ssShift in Shift);
    VK_BACK:
      begin
        // Plain Backspace is a WM_CHAR editing action. This lets OnKeyPress
        // reject it just as with a VCL edit, after OnKeyDown has run.
        if not (ssCtrl in Shift) then Exit;
        FBackspaceHandled := True;
        if not ReadOnly then
        begin
          if SelLength = 0 then FModel.Select(WordLeft(FModel.Caret), FModel.Caret);
          FModel.DeleteBackward;
        end;
      end;
    VK_DELETE: if not ReadOnly then
      begin
        if (ssCtrl in Shift) and (SelLength = 0) then
          FModel.Select(FModel.Caret, WordRight(FModel.Next(FModel.Caret)));
        FModel.DeleteForward;
      end;
  else Exit;
  end;
  Key := 0; Changed(Before);
end;
procedure TERDTextInput.InsertText(const AValue: UnicodeString);
var Before, Value: UnicodeString; C: Char;
begin
  if ReadOnly or not Enabled then Exit;
  Value := NormalizeInput(AValue);
  if FNumbersOnly then
  begin
    Before := '';
    for C in Value do if (C >= '0') and (C <= '9') then Before := Before + C;
    Value := Before;
    if (Value = '') and (AValue <> '') then Exit;
  end;
  case FCharCase of
    ecUpperCase: Value := UpperCase(Value);
    ecLowerCase: Value := LowerCase(Value);
  end;
  Before := Text; FModel.ReplaceSelection(Value, FMaxLength); Changed(Before);
end;
procedure TERDTextInput.WMChar(var Message: TWMChar);
var C: Char; Before: UnicodeString;
begin
  C := Char(Message.CharCode);
  KeyPress(C); if C = #0 then Exit;
  if C = #8 then
  begin
    if FBackspaceHandled then begin FBackspaceHandled := False; Exit; end;
    if not ReadOnly then
    begin Before := Text; FModel.DeleteBackward; Changed(Before); end;
    Message.Result := 0; Exit;
  end;
  if (Ord(C) < 32) or (C = #127) then Exit;
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
var Position: Integer;
begin
  inherited;
  if Button <> mbLeft then Exit;
  Position := HitPosition(X);
  SetFocus;
  if ssDouble in Shift then SelectWord(Position)
  else begin FWordSelecting := False; MoveTo(Position, ssShift in Shift); end;
  FSelecting := True; MouseCapture := True; FDragTimer.Enabled := True;
end;
procedure TERDTextInput.MouseMove(Shift: TShiftState; X, Y: Integer);
begin inherited; if FSelecting and MouseCapture then ExtendSelection(HitPosition(X)); end;
procedure TERDTextInput.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then begin FSelecting := False; FWordSelecting := False; MouseCapture := False; FDragTimer.Enabled := False; end;
end;
procedure TERDTextInput.DragTick(Sender: TObject);
var Point: TPoint;
begin
  if not FSelecting or not MouseCapture then begin FDragTimer.Enabled := False; Exit; end;
  GetCursorPos(Point); Point := ScreenToClient(Point);
  if Point.X < ScaleValue(4) then
  begin
    if FWordSelecting then ExtendSelection(FModel.Previous(FModel.SelStart))
    else MoveTo(FModel.Previous(FModel.Caret), True);
  end
  else if Point.X > ClientWidth - ScaleValue(4) then
  begin
    if FWordSelecting then ExtendSelection(FModel.Next(FModel.SelStart + FModel.SelLength))
    else MoveTo(FModel.Next(FModel.Caret), True);
  end;
end;
procedure TERDTextInput.DblClick;
begin SelectWord(FModel.Caret); inherited; end;
procedure TERDTextInput.DoEnter;
begin
  if FAutoSelect and (GetKeyState(VK_LBUTTON) >= 0) then FModel.Select(0, Length(Text));
  inherited;
  Canvas.Font.Assign(Font);
  FCaretCreated := CreateCaret(Handle, 0, ScaleValue(1), Canvas.TextHeight('Mg'));
  UpdateCaret; if FCaretCreated then ShowCaret(Handle); Invalidate;
end;
procedure TERDTextInput.DoExit;
begin
  FSelecting := False; FWordSelecting := False; MouseCapture := False; FDragTimer.Enabled := False; FPendingSurrogate := #0; FComposition := '';
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
function TERDTextInput.DefaultEditMenu: TERDPopupMenu;
  procedure Add(const Caption: string; Command: Integer; Shortcut: TShortCut);
  var Item: TERDMenuItem;
  begin
    Item := TERDMenuItem.Create(FContextMenu);
    Item.Caption := Caption; Item.Tag := Command; Item.ShortCut := Shortcut;
    if Command >= 0 then Item.OnClick := EditMenuClick;
    FContextMenu.Items.Add(Item);
  end;
begin
  if FContextMenu = nil then
  begin
    FContextMenu := TERDPopupMenu.Create(Self);
    Add('&Undo', 0, ShortCut(Ord('Z'), [ssCtrl]));
    Add('&Redo', 1, ShortCut(Ord('Y'), [ssCtrl]));
    Add('-', -1, 0);
    Add('Cu&t', 2, ShortCut(Ord('X'), [ssCtrl]));
    Add('&Copy', 3, ShortCut(Ord('C'), [ssCtrl]));
    Add('&Paste', 4, ShortCut(Ord('V'), [ssCtrl]));
    Add('&Delete', 5, ShortCut(VK_DELETE, []));
    Add('-', -1, 0);
    Add('Select &all', 6, ShortCut(Ord('A'), [ssCtrl]));
  end;
  FContextMenu.Theme := Theme; FContextMenu.Density := Density;
  FContextMenu.Items[0].Enabled := Enabled and not ReadOnly and FModel.CanUndo;
  FContextMenu.Items[1].Enabled := Enabled and not ReadOnly and FModel.CanRedo;
  FContextMenu.Items[3].Enabled := Enabled and not ReadOnly and (SelLength > 0) and (FPasswordChar = #0);
  FContextMenu.Items[4].Enabled := Enabled and (SelLength > 0) and (FPasswordChar = #0);
  FContextMenu.Items[5].Enabled := Enabled and not ReadOnly and Clipboard.HasFormat(CF_UNICODETEXT);
  FContextMenu.Items[6].Enabled := Enabled and not ReadOnly and (SelLength > 0);
  FContextMenu.Items[8].Enabled := Enabled and (Text <> '') and (SelLength < Length(Text));
  Result := FContextMenu;
end;
procedure TERDTextInput.EditMenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: Undo;
    1: Redo;
    2: CutToClipboard;
    3: CopyToClipboard;
    4: PasteFromClipboard;
    5: ReplaceSelection('');
    6: SelectAll;
  end;
end;
procedure TERDTextInput.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var Point: TPoint; Menu: TERDPopupMenu; ParentControl: TWinControl; CustomMenu: TPopupMenu;
begin
  inherited;
  if Handled or (PopupMenu <> nil) or not Enabled then Exit;
  SetFocus;
  if (MousePos.X = -1) and (MousePos.Y = -1) then
  begin
    if not GetCaretPos(Point) then Point := System.Types.Point(ScaleValue(6), Height div 2);
    Inc(Point.Y, Canvas.TextHeight('Mg'));
    Point := ClientToScreen(Point);
  end
  else Point := ClientToScreen(MousePos);
  ParentControl := Parent;
  while ParentControl <> nil do
  begin
    CustomMenu := TERDTextControlAccess(ParentControl).PopupMenu;
    if CustomMenu <> nil then
    begin
      Handled := True; CustomMenu.PopupComponent := Self;
      CustomMenu.Popup(Point.X, Point.Y); Exit;
    end;
    ParentControl := ParentControl.Parent;
  end;
  Handled := True;
  Menu := DefaultEditMenu; Menu.PopupComponent := Self;
  Menu.Popup(Point.X, Point.Y);
end;
procedure TERDTextInput.Clear;
begin Text := ''; end;
procedure TERDTextInput.ClearSelection;
begin ReplaceSelection(''); end;
procedure TERDTextInput.ClearUndo;
begin FModel.ClearUndo; end;
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
  CopyToClipboard; BeforeEditCommand(WM_CUT); InsertText('');
end;
procedure TERDTextInput.PasteFromClipboard;
begin
  if not ReadOnly and Clipboard.HasFormat(CF_UNICODETEXT) then
  begin BeforeEditCommand(WM_PASTE); InsertText(Clipboard.AsText); end;
end;
procedure TERDTextInput.Undo;
var Before: UnicodeString;
begin if ReadOnly then Exit; BeforeEditCommand(WM_UNDO); Before := Text; FModel.Undo; Changed(Before); end;
procedure TERDTextInput.Redo;
var Before: UnicodeString;
begin if ReadOnly then Exit; BeforeEditCommand(EM_UNDO); Before := Text; FModel.Redo; Changed(Before); end;
procedure TERDTextInput.ReplaceSelection(const AValue: UnicodeString);
begin
  if AValue = '' then BeforeEditCommand(WM_CLEAR);
  InsertText(AValue);
end;
constructor TERDTextLines.Create(Input: TERDTextArea);
begin inherited Create; FInput := Input; end;
function TERDTextLines.GetTextStr: string;
begin Result := FInput.Text; end;
procedure TERDTextLines.SetTextStr(const Value: string);
begin
  if FInput.FSyncLines then inherited
  else FInput.Text := Value;
end;

{ TERDTextArea }
constructor TERDTextArea.Create(AOwner: TComponent);
begin
  inherited;
  FWordWrap := True;
  FWantReturns := True;
  FPreferredX := -1;
  FLayoutWidth := -1;
  FLines := TERDTextLines.Create(Self); FLines.OnChange := LinesChanged;
  Height := 120;
end;
destructor TERDTextArea.Destroy;
begin
  if FLines <> nil then FLines.OnChange := nil;
  FreeAndNil(FLines); inherited;
end;
function TERDTextArea.GetLines: TStrings;
begin Result := FLines; end;
procedure TERDTextArea.SetLines(Value: TStrings);
begin FLines.Assign(Value); end;
procedure TERDTextArea.LinesChanged(Sender: TObject);
var Value: UnicodeString; I: Integer;
begin
  if FSyncLines then Exit;
  Value := '';
  for I := 0 to FLines.Count - 1 do
  begin
    if I > 0 then Value := Value + #13#10;
    Value := Value + FLines[I];
  end;
  Text := Value;
end;
procedure TERDTextArea.SetInputText(const AValue: UnicodeString);
var Before: UnicodeString;
begin
  Before := Text; FSettingText := True; FModified := False;
  try
    FModel.SetText(NormalizeInput(AValue)); FModel.Select(0, 0);
    FComposition := ''; FScroll := 0; FFirstRow := 0; Changed(Before);
  finally FSettingText := False; FModified := False; end;
end;
function TERDTextArea.NormalizeInput(const Value: UnicodeString): UnicodeString;
begin
  Result := StringReplace(Value, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #10, #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, #0, '', [rfReplaceAll]);
end;
procedure TERDTextArea.Changed(const ABefore: UnicodeString);
begin
  if (FLines <> nil) and (ABefore <> Text) then
  begin
    FSyncLines := True;
    try
      FLines.Text := Text;
      // TStringList's parser drops the final empty line; a memo must retain it.
      if Copy(Text, Max(1, Length(Text) - 1), 2) = #13#10 then FLines.Add('');
    finally FSyncLines := False; end;
  end;
  FPreferredX := -1; inherited;
end;
procedure TERDTextArea.SetWordWrap(Value: Boolean);
begin
  if FWordWrap = Value then Exit;
  FWordWrap := Value; FLayoutWidth := -1; FScroll := 0; UpdateCaret; Invalidate;
end;
function TERDTextArea.TextViewport: TRect;
begin Result := ClientRect; InflateRect(Result, -TextInset, -TextInset); end;
function TERDTextArea.LineOrigin(AWidth: Integer): Integer;
var R: TRect;
begin
  R := TextViewport; Result := R.Left;
  if AWidth <= R.Width then
    case Alignment of
      taRightJustify: Inc(Result, R.Width - AWidth);
      taCenter: Inc(Result, (R.Width - AWidth) div 2);
    end;
end;
procedure TERDTextArea.InvalidateTextLayout;
begin FLayoutWidth := -1; Invalidate; end;
function TERDTextArea.VisualRows: TArray<TERDTextRow>;
begin BuildRows; Result := FRows; end;
procedure TERDTextArea.FilterRows(var Rows: TArray<TERDTextRow>);
begin end;
procedure TERDTextArea.PaintRowBackground(ACanvas: TCanvas; Row: Integer; const R: TRect);
begin end;
procedure TERDTextArea.PaintTextBackground(ACanvas: TCanvas; Start, Count, X, Y: Integer;
  Line: TERDTextLayoutLine);
begin end;
procedure TERDTextArea.PaintTextOverlay(ACanvas: TCanvas; Start, Count, X, Y: Integer;
  Line: TERDTextLayoutLine);
begin end;
procedure TERDTextArea.PaintTextLine(ACanvas: TCanvas; Start, Count, X, Y: Integer;
  Line: TERDTextLayoutLine);
begin Line.Draw(X, Y); end;

procedure TERDTextArea.BuildRows;
var Rows: TList<TERDTextRow>; Row: TERDTextRow; Start, Finish, NextStart,
    At, LastFit, LastSpace, Boundary, Available, PreviousBoundary, Advance, LeftX, RightX: Integer; Line: TERDTextLayoutLine;
  procedure AddRow(AStart, AEnd: Integer);
  begin Row.Start := AStart; Row.Count := AEnd - AStart; Rows.Add(Row); end;
begin
  Available := Max(1, TextViewport.Width);
  if (Length(FRows) > 0) and (FLayoutText = Text) and
    (FLayoutWidth = Available) and (FLayoutFont = Font.Handle) then Exit;
  Canvas.Font.Assign(Font); Rows := TList<TERDTextRow>.Create;
  try
    Start := 0;
    repeat
      Finish := Start;
      while (Finish < Length(Text)) and (Text[Finish + 1] <> #13) do Inc(Finish);
      if Finish < Length(Text) then NextStart := Finish + 2 else NextStart := Length(Text) + 1;
      At := Start;
      if (At = Finish) or not FWordWrap then AddRow(At, Finish)
      else
        while At < Finish do
        begin
          Line := TERDTextLayoutLine.Create(Canvas, Copy(Text, At + 1, Finish - At));
          try
            if Line.Width <= Available then LastFit := Finish
            else
            begin
              LastFit := At; LastSpace := At; PreviousBoundary := At; Advance := 0;
              for Boundary in FModel.Boundaries do
                if (Boundary > At) and (Boundary <= Finish) then
                begin
                  Line.Range(PreviousBoundary - At, Boundary - PreviousBoundary, LeftX, RightX);
                  Inc(Advance, RightX - LeftX); PreviousBoundary := Boundary;
                  if Advance > Available then Break;
                  LastFit := Boundary;
                  if (Text[Boundary] = ' ') or (Text[Boundary] = #9) then LastSpace := Boundary;
                end;
              if LastSpace > At then LastFit := LastSpace;
              if LastFit = At then LastFit := Min(Finish, FModel.Next(At));
            end;
          finally Line.Free; end;
          AddRow(At, LastFit); At := LastFit;
        end;
      Start := NextStart;
    until Start > Length(Text);
    FRows := Rows.ToArray; FilterRows(FRows);
    FLayoutText := Text; FLayoutWidth := Available; FLayoutFont := Font.Handle;
    FFirstRow := EnsureRange(FFirstRow, 0, Max(0, Length(FRows) - VisibleRowCount));
  finally Rows.Free; end;
end;
function TERDTextArea.RowHeight: Integer;
begin Canvas.Font.Assign(Font); Result := Max(1, Canvas.TextHeight('Mg')); end;
function TERDTextArea.VisibleRowCount: Integer;
begin Result := Max(1, TextViewport.Height div RowHeight); end;
function TERDTextArea.RowForPosition(Position: Integer): Integer;
var I: Integer;
begin
  Result := 0;
  for I := High(FRows) downto 0 do if FRows[I].Start <= Position then Exit(I);
end;
function TERDTextArea.RowText(Index: Integer): UnicodeString;
begin Result := Copy(Text, FRows[Index].Start + 1, FRows[Index].Count); end;
procedure TERDTextArea.UpdateCaret;
var Row, H, X, Y, Page: Integer; Line: TERDTextLayoutLine; C: HIMC; Form: TCompositionForm;
begin
  if not HandleAllocated or (FModel = nil) then Exit;
  BuildRows; Row := RowForPosition(FModel.Caret); H := RowHeight;
  Page := VisibleRowCount;
  if Row < FFirstRow then FFirstRow := Row;
  if Row >= FFirstRow + Page then FFirstRow := Row - Page + 1;
  Line := TERDTextLayoutLine.Create(Canvas, RowText(Row));
  try
    X := Line.X(EnsureRange(FModel.Caret - FRows[Row].Start, 0, FRows[Row].Count));
    if FWordWrap then FScroll := 0
    else
    begin
      if X - FScroll > TextViewport.Width then FScroll := X - TextViewport.Width;
      if X < FScroll then FScroll := X;
    end;
    X := LineOrigin(Line.Width) + X - FScroll; Y := TextViewport.Top + (Row - FFirstRow) * H;
    if FCaretCreated then SetCaretPos(X, Y);
    if Focused then
    begin
      C := ImmGetContext(Handle);
      if C <> 0 then
      try
        FillChar(Form, SizeOf(Form), 0); Form.dwStyle := CFS_POINT;
        Form.ptCurrentPos := Point(X, Y + H); ImmSetCompositionWindow(C, @Form);
      finally ImmReleaseContext(Handle, C); end;
    end;
  finally Line.Free; end;
end;
function TERDTextArea.HitRow(X, Y: Integer): Integer;
var Row, Boundary, Distance, Best, TextX: Integer; Line: TERDTextLayoutLine;
begin
  BuildRows;
  Row := EnsureRange(FFirstRow + (Y - TextViewport.Top) div RowHeight, 0, High(FRows));
  Line := TERDTextLayoutLine.Create(Canvas, RowText(Row));
  try
    Result := FRows[Row].Start; Best := MaxInt; TextX := X - LineOrigin(Line.Width) + FScroll;
    for Boundary in FModel.Boundaries do
      if (Boundary >= FRows[Row].Start) and (Boundary <= FRows[Row].Start + FRows[Row].Count) then
      begin
        Distance := Abs(Line.X(Boundary - FRows[Row].Start) - TextX);
        if Distance < Best then begin Best := Distance; Result := Boundary; end;
      end;
  finally Line.Free; end;
end;
procedure TERDTextArea.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Saved, Row, H, Y, I, L, R, ClipSaved: Integer;
    Bounds, Selection: TRect; Line: TERDTextLayoutLine; Ink, Fill: TColor;
begin
  BuildRows; H := RowHeight; P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if BorderStyle = bsSingle then P.EditFrame(ClientRect, Focused, Enabled)
    else P.FillRect(ClientRect, Color);
    Fill := SelectionFill; Ink := P.ContrastInk(Fill);
    if not Focused then
      if BorderStyle = bsNone then Ink := Font.Color else Ink := Palette.ForegroundText;
  finally P.Free; end;
  Bounds := TextViewport;
  Saved := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
    ACanvas.Font.Assign(Font);
    if BorderStyle = bsNone then ACanvas.Font.Color := Font.Color else ACanvas.Font.Color := Palette.ForegroundText;
    SelectObject(ACanvas.Handle, ACanvas.Font.Handle);
    SetTextColor(ACanvas.Handle, ColorToRGB(ACanvas.Font.Color)); SetBkMode(ACanvas.Handle, TRANSPARENT);
    if Text = '' then
    begin
      SetTextColor(ACanvas.Handle, ColorToRGB(Palette.Subtle));
      ERDDrawLayoutText(ACanvas, FHint, Bounds.Left, Bounds.Top);
    end;
    for Row := FFirstRow to High(FRows) do
    begin
      Y := TextViewport.Top + (Row - FFirstRow) * H;
      if Y >= Bounds.Bottom then Break;
      Line := TERDTextLayoutLine.Create(ACanvas, RowText(Row));
      try
        PaintRowBackground(ACanvas, Row, Rect(Bounds.Left, Y, Bounds.Right, Y + H));
        PaintTextBackground(ACanvas, FRows[Row].Start, FRows[Row].Count,
          LineOrigin(Line.Width) - FScroll, Y, Line);
        PaintTextLine(ACanvas, FRows[Row].Start, FRows[Row].Count,
          LineOrigin(Line.Width) - FScroll, Y, Line);
        if (Focused or not FHideSelection) and (SelLength > 0) then
          for I := 0 to High(FModel.Boundaries) - 1 do
            if (FModel.Boundaries[I] >= Max(SelStart, FRows[Row].Start)) and
              (FModel.Boundaries[I + 1] <= Min(SelStart + SelLength, FRows[Row].Start + FRows[Row].Count)) then
            begin
              Line.Range(FModel.Boundaries[I] - FRows[Row].Start,
                FModel.Boundaries[I + 1] - FModel.Boundaries[I], L, R);
              Selection := Rect(LineOrigin(Line.Width) + L - FScroll, Y,
                LineOrigin(Line.Width) + R - FScroll, Y + H);
              ACanvas.Brush.Color := Fill; ACanvas.FillRect(Selection);
              ClipSaved := SaveDC(ACanvas.Handle);
              try
                IntersectClipRect(ACanvas.Handle, Selection.Left, Selection.Top, Selection.Right, Selection.Bottom);
                SetTextColor(ACanvas.Handle, ColorToRGB(Ink)); Line.Draw(LineOrigin(Line.Width) - FScroll, Y);
              finally RestoreDC(ACanvas.Handle, ClipSaved); end;
            end;
        I := FRows[Row].Start + FRows[Row].Count;
        if (Focused or not FHideSelection) and (I < Length(Text)) and (Text[I + 1] = #13) and
          (SelStart <= I) and (SelStart + SelLength > I) then
        begin
          L := LineOrigin(Line.Width) + Line.X(FRows[Row].Count) - FScroll;
          if Line.X(0) > Line.X(FRows[Row].Count) then Selection := Rect(Bounds.Left, Y, L, Y + H)
          else Selection := Rect(L, Y, Bounds.Right, Y + H);
          ACanvas.Brush.Color := Fill; ACanvas.FillRect(Selection);
        end;
        if (FComposition <> '') and (Row = RowForPosition(FModel.Caret)) then
        begin
          L := LineOrigin(Line.Width) + Line.X(Min(FRows[Row].Count, FModel.Caret - FRows[Row].Start)) - FScroll;
          R := ERDDrawLayoutText(ACanvas, FComposition, L, Y);
          ACanvas.Pen.Color := Palette.Accent; ACanvas.MoveTo(L, Y + H - 1);
          ACanvas.LineTo(L + R, Y + H - 1);
        end;
        PaintTextOverlay(ACanvas, FRows[Row].Start, FRows[Row].Count,
          LineOrigin(Line.Width) - FScroll, Y, Line);
      finally Line.Free; end;
    end;
  finally RestoreDC(ACanvas.Handle, Saved); end;
end;
procedure TERDTextArea.KeyDown(var Key: Word; Shift: TShiftState);
var Row, Target, X, Preferred: Integer; Line: TERDTextLayoutLine;
begin
  if not (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END]) then
  begin inherited; Exit; end;
  // These keys need row geometry. Dispatch the normal event without applying
  // the single-line Home/End behavior.
  if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift);
  if Key = 0 then Exit;
  BuildRows; Row := RowForPosition(FModel.Caret); Target := Row;
  if Key in [VK_HOME, VK_END] then
  begin
    if ssCtrl in Shift then
    begin if Key = VK_HOME then X := 0 else X := Length(Text); end
    else if Key = VK_HOME then X := FRows[Row].Start
    else X := FRows[Row].Start + FRows[Row].Count;
    MoveTo(X, ssShift in Shift);
  end
  else
  begin
    Line := TERDTextLayoutLine.Create(Canvas, RowText(Row));
    try
      if FPreferredX < 0 then FPreferredX := LineOrigin(Line.Width) +
        Line.X(Min(FRows[Row].Count, FModel.Caret - FRows[Row].Start)) - FScroll;
    finally Line.Free; end;
    Preferred := FPreferredX;
    case Key of
      VK_UP: Dec(Target);
      VK_DOWN: Inc(Target);
      VK_PRIOR: Dec(Target, Max(1, ClientHeight div RowHeight));
      VK_NEXT: Inc(Target, Max(1, ClientHeight div RowHeight));
    end;
    Target := EnsureRange(Target, 0, High(FRows));
    X := HitRow(Preferred, TextViewport.Top + (Target - FFirstRow) * RowHeight + RowHeight div 2);
    MoveTo(X, ssShift in Shift); FPreferredX := Preferred;
  end;
  Key := 0;
end;
procedure TERDTextArea.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: Integer;
begin
  if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
  if Button <> mbLeft then Exit;
  Position := HitRow(X, Y); SetFocus;
  if ssDouble in Shift then SelectWord(Position)
  else begin FWordSelecting := False; MoveTo(Position, ssShift in Shift); end;
  FSelecting := True; MouseCapture := True; FDragTimer.Enabled := True;
end;
procedure TERDTextArea.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseMove) then OnMouseMove(Self, Shift, X, Y);
  if FSelecting and MouseCapture then ExtendSelection(HitRow(X, Y));
end;
procedure TERDTextArea.DragTick(Sender: TObject);
var P: TPoint;
begin
  if not FSelecting or not MouseCapture then begin FDragTimer.Enabled := False; Exit; end;
  GetCursorPos(P); P := ScreenToClient(P);
  if P.Y < TextViewport.Top then ScrollRows(FFirstRow - 1)
  else if P.Y > TextViewport.Bottom then ScrollRows(FFirstRow + 1)
  else Exit;
  ExtendSelection(HitRow(P.X, EnsureRange(P.Y, TextViewport.Top, TextViewport.Bottom - 1)));
end;
procedure TERDTextArea.WMChar(var Message: TWMChar);
var C: Char;
begin
  C := Char(Message.CharCode);
  if (C = #13) or (C = #9) then
  begin
    if ((C = #13) and not FWantReturns) or ((C = #9) and not FWantTabs) then Exit;
    KeyPress(C); if C <> #0 then InsertText(C); Message.Result := 0;
  end
  else inherited;
end;
procedure TERDTextArea.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  if FWantTabs then Message.Result := Message.Result or DLGC_WANTTAB;
  if FWantReturns and (Message.WParam = VK_RETURN) then Message.Result := Message.Result or DLGC_WANTMESSAGE;
end;
procedure TERDTextArea.ScrollRows(Value: Integer);
var Row: Integer; Line: TERDTextLayoutLine;
begin
  BuildRows;
  FFirstRow := EnsureRange(Value, 0, Max(0, Length(FRows) - VisibleRowCount));
  Invalidate;
  if FCaretCreated then
  begin
    Row := RowForPosition(FModel.Caret); Line := TERDTextLayoutLine.Create(Canvas, RowText(Row));
    try
      SetCaretPos(LineOrigin(Line.Width) + Line.X(Min(FRows[Row].Count,
        FModel.Caret - FRows[Row].Start)) - FScroll, TextViewport.Top + (Row - FFirstRow) * RowHeight);
    finally Line.Free; end;
  end;
end;
function TERDTextArea.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin ScrollRows(FFirstRow - MulDiv(WheelDelta, 3, WHEEL_DELTA)); Result := True; end;
procedure TERDTextArea.WndProc(var Message: TMessage);
begin
  if FModel <> nil then
    case Message.Msg of
      EM_GETLINECOUNT: begin BuildRows; Message.Result := Length(FRows); Exit; end;
      EM_GETFIRSTVISIBLELINE: begin Message.Result := FFirstRow; Exit; end;
      EM_LINESCROLL: begin ScrollRows(FFirstRow + Integer(Message.LParam)); Message.Result := 1; Exit; end;
      EM_SCROLLCARET: begin UpdateCaret; Message.Result := 1; Exit; end;
    end;
  inherited;
end;

end.
