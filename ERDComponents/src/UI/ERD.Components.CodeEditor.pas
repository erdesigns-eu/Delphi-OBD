unit ERD.Components.CodeEditor;
// Code view over the shared Unicode engine. Syntax providers own language analysis.
{$B-}
interface
uses Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Types, System.Math, System.Generics.Collections, Vcl.Controls,
  Vcl.Graphics, ERD.Components.Control, ERD.Components.TextInput, ERD.Components.Syntax;
type
  TERDDiagnosticSeverity = (dsError, dsWarning, dsInformation, dsHint);
  TERDCodeDiagnostic = record
    Start, Count: Integer; // Zero-based UTF-16 offsets into the submitted document.
    Severity: TERDDiagnosticSeverity;
    MessageText, Source, Code: UnicodeString;
  end;
  TERDCodeEditor = class(TERDTextArea)
  private
    FLanguage: TERDCodeLanguage;
    FProvider: IERDSyntaxProvider;
    FAnalysis: TERDSyntaxAnalysis;
    FAnalyzedText: UnicodeString;
    FAnalysisValid: Boolean;
    FCollapsed: TList<Integer>;
    FShowLineNumbers, FShowFolding: Boolean;
    FTabWidth: Integer;
    FTabHandled: Boolean;
    FDiagnostics: TArray<TERDCodeDiagnostic>;
    function DiagnosticColor(Severity: TERDDiagnosticSeverity): TColor;
    procedure Analyze;
    procedure SetLanguage(Value: TERDCodeLanguage);
    procedure SetProvider(const Value: IERDSyntaxProvider);
    procedure SetShowLineNumbers(Value: Boolean);
    procedure SetShowFolding(Value: Boolean);
    procedure SetTabWidth(Value: Integer);
    function FoldHiddenStart(const Fold: TERDSyntaxFold): Integer;
    function FoldHiddenFinish(const Fold: TERDSyntaxFold): Integer;
    function FoldIndex(Position: Integer): Integer;
    function LineStart(Position: Integer): Integer;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  protected
    function TextViewport: TRect; override;
    procedure FilterRows(var Rows: TArray<TERDTextRow>); override;
    procedure PaintTextLine(ACanvas: TCanvas; Start, Count, X, Y: Integer;
      Line: TERDTextLayoutLine); override;
    procedure PaintTextOverlay(ACanvas: TCanvas; Start, Count, X, Y: Integer;
      Line: TERDTextLayoutLine); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure UpdateCaret; override;
    procedure Changed(const ABefore: UnicodeString); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ToggleFold(Position: Integer);
    procedure FoldAll;
    procedure UnfoldAll;
    procedure IndentSelection(Outdent: Boolean = False);
    function PublishDiagnostics(const SourceText: UnicodeString;
      const Items: TArray<TERDCodeDiagnostic>): Boolean;
    procedure ClearDiagnostics;
    function Diagnostics: TArray<TERDCodeDiagnostic>;
    function DiagnosticAt(Position: Integer; out Item: TERDCodeDiagnostic): Boolean;
    function SyntaxAnalysis: TERDSyntaxAnalysis;
    property SyntaxProvider: IERDSyntaxProvider read FProvider write SetProvider;
  published
    property Language: TERDCodeLanguage read FLanguage write SetLanguage default clTypeScript;
    property ShowLineNumbers: Boolean read FShowLineNumbers write SetShowLineNumbers default True;
    property ShowFolding: Boolean read FShowFolding write SetShowFolding default True;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 2;
  end;
implementation
constructor TERDCodeEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCollapsed := TList<Integer>.Create;
  FLanguage := clTypeScript;
  FTabWidth := 2;
  FShowLineNumbers := True;
  FShowFolding := True;
  FProvider := TERDBasicSyntaxProvider.Create(FLanguage);
  WordWrap := False; WantTabs := True; AutoSelect := False;
  Font.Name := 'Consolas';
end;
destructor TERDCodeEditor.Destroy;
begin FCollapsed.Free; inherited Destroy; end;
procedure TERDCodeEditor.Analyze;
var Span: TERDSyntaxSpan; Fold: TERDSyntaxFold;
    Spans: TList<TERDSyntaxSpan>; Folds: TList<TERDSyntaxFold>;
begin
  if FAnalysisValid and (FAnalyzedText = Text) then Exit;
  FAnalysis.Spans := nil; FAnalysis.Folds := nil;
  if FProvider <> nil then FAnalysis := FProvider.Analyze(Text);
  // Providers are replaceable; malformed ranges never reach the renderer.
  Spans := TList<TERDSyntaxSpan>.Create;
  Folds := TList<TERDSyntaxFold>.Create;
  try
    for Span in FAnalysis.Spans do
      if (Span.Start >= 0) and (Span.Start < Length(Text)) and (Span.Count > 0) then
      begin Span.Count := Min(Span.Count, Length(Text) - Span.Start); Spans.Add(Span); end;
    for Fold in FAnalysis.Folds do
      if (Fold.Start >= 0) and (Fold.Finish <= Length(Text)) and
        (Fold.Start < Fold.Finish) and (FoldHiddenStart(Fold) < FoldHiddenFinish(Fold)) then
        Folds.Add(Fold);
    FAnalysis.Spans := Spans.ToArray; FAnalysis.Folds := Folds.ToArray;
  finally Folds.Free; Spans.Free; end;
  FAnalyzedText := Text; FAnalysisValid := True;
end;
function TERDCodeEditor.SyntaxAnalysis: TERDSyntaxAnalysis;
begin
  Analyze; Result.Spans := Copy(FAnalysis.Spans); Result.Folds := Copy(FAnalysis.Folds);
end;
procedure TERDCodeEditor.SetProvider(const Value: IERDSyntaxProvider);
begin
  FProvider := Value; FAnalysisValid := False;
  if FCollapsed <> nil then FCollapsed.Clear;
  InvalidateTextLayout; UpdateCaret;
end;
procedure TERDCodeEditor.SetLanguage(Value: TERDCodeLanguage);
begin
  if FLanguage = Value then Exit;
  FLanguage := Value; SetProvider(TERDBasicSyntaxProvider.Create(Value));
end;
procedure TERDCodeEditor.SetShowLineNumbers(Value: Boolean);
begin
  if FShowLineNumbers = Value then Exit;
  FShowLineNumbers := Value; InvalidateTextLayout; UpdateCaret;
end;
procedure TERDCodeEditor.SetShowFolding(Value: Boolean);
begin
  if FShowFolding = Value then Exit;
  FShowFolding := Value;
  if not Value then UnfoldAll;
  InvalidateTextLayout; UpdateCaret;
end;
procedure TERDCodeEditor.SetTabWidth(Value: Integer);
begin FTabWidth := EnsureRange(Value, 1, 16); end;
function TERDCodeEditor.TextViewport: TRect;
begin
  Result := inherited TextViewport;
  if FShowLineNumbers then Inc(Result.Left, ScaleValue(48));
  if FShowFolding then Inc(Result.Left, ScaleValue(18));
  Result.Left := Min(Result.Left, Result.Right - 1);
end;
function TERDCodeEditor.LineStart(Position: Integer): Integer;
begin
  Result := EnsureRange(Position, 0, Length(Text));
  while (Result > 0) and (Text[Result] <> #10) do Dec(Result);
end;
function TERDCodeEditor.FoldHiddenStart(const Fold: TERDSyntaxFold): Integer;
begin
  Result := Fold.Start;
  while (Result < Length(Text)) and (Text[Result + 1] <> #10) do Inc(Result);
  if Result < Length(Text) then Inc(Result);
end;
function TERDCodeEditor.FoldHiddenFinish(const Fold: TERDSyntaxFold): Integer;
begin
  Result := Fold.Finish;
  while (Result < Length(Text)) and (Text[Result + 1] <> #10) do Inc(Result);
  if Result < Length(Text) then Inc(Result);
end;
function TERDCodeEditor.FoldIndex(Position: Integer): Integer;
var I, Start: Integer;
begin
  Analyze; Result := -1; Start := LineStart(Position);
  for I := 0 to High(FAnalysis.Folds) do
    if LineStart(FAnalysis.Folds[I].Start) = Start then
      if (Result < 0) or (FAnalysis.Folds[I].Finish > FAnalysis.Folds[Result].Finish) then Result := I;
end;
procedure TERDCodeEditor.FilterRows(var Rows: TArray<TERDTextRow>);
var Row: TERDTextRow; Fold: TERDSyntaxFold; Visible: TList<TERDTextRow>;
    Hidden: Boolean;
begin
  if (FCollapsed = nil) or (FCollapsed.Count = 0) then Exit;
  Analyze; Visible := TList<TERDTextRow>.Create;
  try
    for Row in Rows do
    begin
      Hidden := False;
      for Fold in FAnalysis.Folds do
        if FCollapsed.Contains(Fold.Start) and (Row.Start >= FoldHiddenStart(Fold)) and
          (Row.Start < FoldHiddenFinish(Fold)) then begin Hidden := True; Break; end;
      if not Hidden then Visible.Add(Row);
    end;
    Rows := Visible.ToArray;
  finally Visible.Free; end;
end;
procedure TERDCodeEditor.UpdateCaret;
var Fold: TERDSyntaxFold; Expanded: Boolean;
begin
  Expanded := False;
  if (FCollapsed <> nil) and (FCollapsed.Count > 0) then
  begin
    Analyze;
    for Fold in FAnalysis.Folds do
      if FCollapsed.Contains(Fold.Start) and (TextModel.Caret >= FoldHiddenStart(Fold)) and
        (TextModel.Caret < FoldHiddenFinish(Fold)) then
      begin FCollapsed.Remove(Fold.Start); Expanded := True; end;
    if Expanded then InvalidateTextLayout;
  end;
  inherited UpdateCaret;
end;
procedure TERDCodeEditor.Changed(const ABefore: UnicodeString);
begin
  if ABefore <> Text then
  begin
    // Editing invalidates structural offsets. Keep the text, expand stale folds.
    FDiagnostics := nil;
    FAnalysisValid := False;
    if FCollapsed <> nil then FCollapsed.Clear;
    InvalidateTextLayout;
  end;
  inherited Changed(ABefore);
end;
procedure TERDCodeEditor.ToggleFold(Position: Integer);
var I: Integer; Fold: TERDSyntaxFold;
begin
  if not FShowFolding then Exit;
  I := FoldIndex(Position); if I < 0 then Exit;
  Fold := FAnalysis.Folds[I];
  if FCollapsed.Contains(Fold.Start) then FCollapsed.Remove(Fold.Start)
  else
  begin
    if (TextModel.Caret >= FoldHiddenStart(Fold)) and (TextModel.Caret < FoldHiddenFinish(Fold)) then
    begin SelLength := 0; SelStart := Fold.Start; end;
    FCollapsed.Add(Fold.Start);
  end;
  InvalidateTextLayout; UpdateCaret;
end;
procedure TERDCodeEditor.FoldAll;
var Fold: TERDSyntaxFold;
begin
  if not FShowFolding then Exit;
  Analyze; SelLength := 0; SelStart := 0; FCollapsed.Clear;
  for Fold in FAnalysis.Folds do FCollapsed.Add(Fold.Start);
  InvalidateTextLayout; UpdateCaret;
end;
procedure TERDCodeEditor.UnfoldAll;
begin
  if FCollapsed = nil then Exit;
  FCollapsed.Clear; InvalidateTextLayout; UpdateCaret;
end;
procedure TERDCodeEditor.PaintTextLine(ACanvas: TCanvas; Start, Count, X, Y: Integer;
  Line: TERDTextLayoutLine);
var Span: TERDSyntaxSpan; LeftX, RightX, A, B, Saved: Integer; Ink: TColor;
begin
  inherited PaintTextLine(ACanvas, Start, Count, X, Y, Line);
  Analyze;
  for Span in FAnalysis.Spans do
  begin
    A := Max(Start, Span.Start); B := Min(Start + Count, Span.Start + Span.Count);
    if A >= B then Continue;
    case Span.Kind of
      skKeyword, skDirective: Ink := Palette.Accent;
      skNumber: Ink := Palette.Warning;
      skString: Ink := Palette.Success;
      skComment: Ink := Palette.Subtle;
    else Continue;
    end;
    Line.Range(A - Start, B - A, LeftX, RightX);
    Saved := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, X + LeftX, Y, X + RightX, Y + RowHeight);
      SetTextColor(ACanvas.Handle, ColorToRGB(Ink)); Line.Draw(X, Y);
    finally RestoreDC(ACanvas.Handle, Saved); end;
  end;
end;
function TERDCodeEditor.PublishDiagnostics(const SourceText: UnicodeString;
  const Items: TArray<TERDCodeDiagnostic>): Boolean;
var Item: TERDCodeDiagnostic; Valid: TList<TERDCodeDiagnostic>;
begin
  // An asynchronous validator may finish after the document changed.
  Result := SourceText = Text; if not Result then Exit;
  Valid := TList<TERDCodeDiagnostic>.Create;
  try
    for Item in Items do
      if (Item.Start >= 0) and (Item.Start <= Length(Text)) and (Item.Count >= 0) then
      begin
        Item.Count := Min(Item.Count, Length(Text) - Item.Start);
        Valid.Add(Item);
      end;
    FDiagnostics := Valid.ToArray;
  finally Valid.Free; end;
  Invalidate;
end;
procedure TERDCodeEditor.ClearDiagnostics;
begin FDiagnostics := nil; Invalidate; end;
function TERDCodeEditor.Diagnostics: TArray<TERDCodeDiagnostic>;
begin Result := Copy(FDiagnostics); end;
function TERDCodeEditor.DiagnosticAt(Position: Integer; out Item: TERDCodeDiagnostic): Boolean;
var Candidate: TERDCodeDiagnostic;
begin
  Item := Default(TERDCodeDiagnostic); Result := False;
  for Candidate in FDiagnostics do
    if (Position >= Candidate.Start) and
      (Position < Candidate.Start + Max(1, Candidate.Count)) then
      if not Result or (Ord(Candidate.Severity) < Ord(Item.Severity)) then
      begin Item := Candidate; Result := True; end;
end;
function TERDCodeEditor.DiagnosticColor(Severity: TERDDiagnosticSeverity): TColor;
begin
  case Severity of
    dsError: Result := Palette.Danger;
    dsWarning: Result := Palette.Warning;
    dsInformation: Result := Palette.Accent;
  else Result := Palette.Subtle;
  end;
end;
procedure TERDCodeEditor.PaintTextOverlay(ACanvas: TCanvas; Start, Count, X, Y: Integer;
  Line: TERDTextLayoutLine);
var Item: TERDCodeDiagnostic; A, B, L, R, At, Bottom, Step, Saved: Integer;
begin
  inherited;
  Saved := SaveDC(ACanvas.Handle);
  try
    Step := Max(2, ScaleValue(2)); Bottom := Y + RowHeight - 2 * Step;
    for Item in FDiagnostics do
    begin
      A := Max(Start, Item.Start); B := Min(Start + Count, Item.Start + Item.Count);
      if Item.Count = 0 then
      begin
        if (Item.Start < Start) or (Item.Start > Start + Count) then Continue;
        L := Line.X(Item.Start - Start); R := L + Step * 3;
      end
      else
      begin
        if A >= B then Continue;
        Line.Range(A - Start, B - A, L, R);
      end;
      ACanvas.Pen.Color := DiagnosticColor(Item.Severity);
      At := X + L; ACanvas.MoveTo(At, Bottom);
      while At < X + R do
      begin
        ACanvas.LineTo(Min(At + Step, X + R), Bottom + Step);
        Inc(At, Step);
        ACanvas.LineTo(Min(At + Step, X + R), Bottom);
        Inc(At, Step);
      end;
    end;
  finally RestoreDC(ACanvas.Handle, Saved); end;
end;
procedure TERDCodeEditor.PaintControl(ACanvas: TCanvas);
var Rows: TArray<TERDTextRow>; I, J, Number, Y, H, Index, X: Integer;
    R, View: TRect; Saved: Integer; Diagnostic: TERDCodeDiagnostic;
    DiagnosticFound: Boolean; RowFinish: Integer; Severity: TERDDiagnosticSeverity;
begin
  inherited PaintControl(ACanvas);
  if not (FShowLineNumbers or FShowFolding) then Exit;
  Rows := VisualRows; View := TextViewport; H := RowHeight;
  R := inherited TextViewport; R.Right := View.Left - ScaleValue(3);
  Saved := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    ACanvas.Brush.Color := Palette.Background; ACanvas.FillRect(R);
    ACanvas.Font.Assign(Font); ACanvas.Font.Color := Palette.Subtle;
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    Number := 1; J := 0;
    for I := FirstVisibleRow to Min(High(Rows), FirstVisibleRow + VisibleRowCount) do
    begin
      while J < Rows[I].Start do
      begin Inc(J); if Text[J] = #10 then Inc(Number); end;
      Y := View.Top + (I - FirstVisibleRow) * H;
      X := View.Left - ScaleValue(20);
      DiagnosticFound := False; Severity := dsHint;
      RowFinish := Rows[I].Start + Rows[I].Count;
      for Diagnostic in FDiagnostics do
        if (Diagnostic.Start <= RowFinish) and
          (Diagnostic.Start + Max(1, Diagnostic.Count) > Rows[I].Start) then
        begin
          if not DiagnosticFound or (Ord(Diagnostic.Severity) < Ord(Severity)) then
            Severity := Diagnostic.Severity;
          DiagnosticFound := True;
        end;
      if DiagnosticFound then
      begin
        ACanvas.Brush.Color := DiagnosticColor(Severity);
        ACanvas.Pen.Color := DiagnosticColor(Severity);
        ACanvas.Ellipse(R.Left, Y + H div 2 - ScaleValue(3),
          R.Left + ScaleValue(6), Y + H div 2 + ScaleValue(3));
        ACanvas.Brush.Color := Palette.Background;
      end;
      ACanvas.Brush.Style := bsClear;
      if FShowLineNumbers then
        ACanvas.TextOut(X - ScaleValue(6) - ACanvas.TextWidth(IntToStr(Number)), Y, IntToStr(Number));
      if FShowFolding then
      begin
        Index := FoldIndex(Rows[I].Start);
        if Index >= 0 then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Pen.Color := Palette.Subtle;
          ACanvas.Rectangle(X + 2, Y + H div 2 - 4, X + 11, Y + H div 2 + 5);
          ACanvas.MoveTo(X + 4, Y + H div 2); ACanvas.LineTo(X + 9, Y + H div 2);
          if FCollapsed.Contains(FAnalysis.Folds[Index].Start) then
          begin ACanvas.MoveTo(X + 6, Y + H div 2 - 2); ACanvas.LineTo(X + 6, Y + H div 2 + 3); end;
        end;
      end;
    end;
  finally RestoreDC(ACanvas.Handle, Saved); end;
end;
procedure TERDCodeEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Rows: TArray<TERDTextRow>; Row: Integer; View: TRect;
begin
  View := TextViewport;
  if (Button = mbLeft) and FShowFolding and (X >= View.Left - ScaleValue(20)) and
    (X < View.Left) and (Y >= View.Top) and (Y < View.Bottom) then
  begin
    Rows := VisualRows;
    Row := FirstVisibleRow + (Y - View.Top) div RowHeight;
    if (Row >= 0) and (Row < Length(Rows)) then ToggleFold(Rows[Row].Start);
    SetFocus; Exit;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;
procedure TERDCodeEditor.IndentSelection(Outdent: Boolean);
var Start, Finish, At, RemoveCount: Integer; Before, Replacement, Prefix: UnicodeString;
begin
  if ReadOnly then Exit;
  Before := Text; Start := LineStart(SelStart); Finish := SelStart + SelLength;
  if (SelLength > 0) and (Finish = LineStart(Finish)) then Finish := Max(Start, Finish - 2);
  while (Finish < Length(Before)) and (Before[Finish + 1] <> #13) do Inc(Finish);
  Replacement := ''; At := Start; Prefix := StringOfChar(' ', FTabWidth);
  repeat
    if Outdent then
    begin
      RemoveCount := 0;
      if (At < Finish) and (Before[At + 1] = #9) then RemoveCount := 1
      else while (At + RemoveCount < Finish) and (RemoveCount < FTabWidth) and
        (Before[At + RemoveCount + 1] = ' ') do Inc(RemoveCount);
      Inc(At, RemoveCount);
    end
    else Replacement := Replacement + Prefix;
    while (At < Finish) and (Before[At + 1] <> #13) do
    begin Replacement := Replacement + Before[At + 1]; Inc(At); end;
    if At >= Finish then Break;
    Replacement := Replacement + #13#10; Inc(At, 2);
  until At > Finish;
  if (MaxLength > 0) and (Length(Before) - (Finish - Start) + Length(Replacement) > MaxLength) then Exit;
  TextModel.ReplaceRange(Start, Finish - Start, Replacement); Changed(Before);
  SelStart := Start; SelLength := Length(Replacement);
end;
procedure TERDCodeEditor.KeyDown(var Key: Word; Shift: TShiftState);
var Before: UnicodeString; Position, Spaces: Integer;
begin
  FTabHandled := False;
  if (Key = VK_TAB) and not (ssCtrl in Shift) and not (ssAlt in Shift) then
  begin
    if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift);
    if Key = 0 then begin FTabHandled := True; Exit; end;
    if not ReadOnly then
      if (SelLength > 0) or (ssShift in Shift) then IndentSelection(ssShift in Shift)
      else
      begin
        Position := SelStart; Spaces := FTabWidth - ((Position - LineStart(Position)) mod FTabWidth);
        Before := Text; TextModel.ReplaceSelection(StringOfChar(' ', Spaces), MaxLength); Changed(Before);
      end;
    FTabHandled := True; Key := 0; Exit;
  end;
  inherited KeyDown(Key, Shift);
end;
procedure TERDCodeEditor.WMChar(var Message: TWMChar);
var C: Char; Position, Start: Integer; Prefix: UnicodeString;
begin
  if Message.CharCode = 9 then
  begin
    if FTabHandled then begin FTabHandled := False; Exit; end;
    IndentSelection(False); Exit;
  end;
  if (Message.CharCode = 13) and not ReadOnly and WantReturns then
  begin
    C := #13; KeyPress(C); if C = #0 then Exit;
    if C <> #13 then begin ReplaceSelection(C); Exit; end;
    Position := SelStart; Start := LineStart(Position); Prefix := '';
    while (Start < Position) and (Text[Start + 1] in [' ', #9]) do
    begin Prefix := Prefix + Text[Start + 1]; Inc(Start); end;
    ReplaceSelection(#13#10 + Prefix); Exit;
  end;
  inherited;
end;
end.
