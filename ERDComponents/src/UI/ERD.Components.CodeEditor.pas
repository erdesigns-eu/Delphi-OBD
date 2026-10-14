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
    FAutoCloseDelimiters, FSmartHome, FShowIndentGuides, FShowBracketMatch: Boolean;
    FDiagnostics: TArray<TERDCodeDiagnostic>;
    FBracketText: UnicodeString;
    FBracketCaret, FBracketOpening, FBracketClosing: Integer;
    FBracketValid: Boolean;
    procedure SetShowIndentGuides(Value: Boolean);
    procedure SetShowBracketMatch(Value: Boolean);
    function LiteralAt(Position: Integer): Boolean;
    function HandleDelimiter(C: Char): Boolean;
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
    function MatchingBrackets(out Opening, Closing: Integer): Boolean;
    function SyntaxAnalysis: TERDSyntaxAnalysis;
    property SyntaxProvider: IERDSyntaxProvider read FProvider write SetProvider;
  published
    property Language: TERDCodeLanguage read FLanguage write SetLanguage default clTypeScript;
    property ShowLineNumbers: Boolean read FShowLineNumbers write SetShowLineNumbers default True;
    property ShowFolding: Boolean read FShowFolding write SetShowFolding default True;
    property AutoCloseDelimiters: Boolean read FAutoCloseDelimiters write FAutoCloseDelimiters default True;
    property SmartHome: Boolean read FSmartHome write FSmartHome default True;
    property ShowIndentGuides: Boolean read FShowIndentGuides write SetShowIndentGuides default True;
    property ShowBracketMatch: Boolean read FShowBracketMatch write SetShowBracketMatch default True;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 2;
  end;
implementation
constructor TERDCodeEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCollapsed := TList<Integer>.Create;
  FLanguage := clTypeScript;
  FTabWidth := 2;
  FAutoCloseDelimiters := True;
  FSmartHome := True;
  FShowIndentGuides := True;
  FShowBracketMatch := True;
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
  FProvider := Value; FAnalysisValid := False; FBracketValid := False;
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
procedure TERDCodeEditor.SetShowIndentGuides(Value: Boolean);
begin FShowIndentGuides := Value; Invalidate; end;
procedure TERDCodeEditor.SetShowBracketMatch(Value: Boolean);
begin FShowBracketMatch := Value; Invalidate; end;
function TERDCodeEditor.LiteralAt(Position: Integer): Boolean;
var Span: TERDSyntaxSpan;
begin
  Analyze; Result := False;
  for Span in FAnalysis.Spans do
    if (Span.Kind in [skString, skComment, skDirective]) and
      (Position > Span.Start) and (Position < Span.Start + Span.Count) then Exit(True);
end;
function TERDCodeEditor.MatchingBrackets(out Opening, Closing: Integer): Boolean;
var Stack: TList<Integer>; I, Candidate, Top: Integer; C, First: Char;
    Span: TERDSyntaxSpan; Excluded: Boolean;
begin
  if FBracketValid and (FBracketText = Text) and (FBracketCaret = TextModel.Caret) then
  begin Opening := FBracketOpening; Closing := FBracketClosing; Exit(Opening >= 0); end;
  Opening := -1; Closing := -1; Result := False;
  FBracketText := Text; FBracketCaret := TextModel.Caret;
  FBracketOpening := -1; FBracketClosing := -1; FBracketValid := True;
  Candidate := TextModel.Caret;
  if (Candidate >= Length(Text)) or not (Text[Candidate + 1] in ['(',')','[',']','{','}']) then
    Dec(Candidate);
  if (Candidate < 0) or (Candidate >= Length(Text)) or
    not (Text[Candidate + 1] in ['(',')','[',']','{','}']) then Exit;
  Analyze; Stack := TList<Integer>.Create;
  try
    for I := 0 to Length(Text) - 1 do
    begin
      C := Text[I + 1]; if not (C in ['(',')','[',']','{','}']) then Continue;
      // Token interiors and their opening character both belong to literals.
      Excluded := False;
      for Span in FAnalysis.Spans do
        if (Span.Kind in [skString, skComment, skDirective]) and
          (I >= Span.Start) and (I < Span.Start + Span.Count) then
        begin Excluded := True; Break; end;
      if Excluded then Continue;
      if C in ['(','[','{'] then Stack.Add(I)
      else if Stack.Count > 0 then
      begin
        Top := Stack.Last; First := Text[Top + 1];
        if ((First = '(') and (C = ')')) or ((First = '[') and (C = ']')) or
          ((First = '{') and (C = '}')) then
        begin
          Stack.Delete(Stack.Count - 1);
          if (Candidate = Top) or (Candidate = I) then
          begin
            Opening := Top; Closing := I; FBracketOpening := Top; FBracketClosing := I; Exit(True);
          end;
        end
        else Stack.Clear;
      end;
    end;
  finally Stack.Free; end;
end;
function TERDCodeEditor.HandleDelimiter(C: Char): Boolean;
var Closing: Char; Position, Selected: Integer; Before, Value: UnicodeString;
    Span: TERDSyntaxSpan;
begin
  Result := False; if not FAutoCloseDelimiters or ReadOnly then Exit;
  Position := SelStart; Selected := SelLength;
  if (Selected = 0) and (Position < Length(Text)) and
    (C in [')',']','}',#39,'"','`']) and (Text[Position + 1] = C) and
    ((C in [#39,'"','`']) or not LiteralAt(Position)) then
  begin MoveTo(Position + 1, False); Exit(True); end;
  if LiteralAt(Position) then Exit;
  Analyze;
  // End-of-line comments extend to the caret at their final character.
  if (Selected = 0) and (Position > 0) then
    for Span in FAnalysis.Spans do
      if (Span.Kind = skComment) and (Span.Start + Span.Count = Position) and
        (Copy(Text, Span.Start + 1, 2) = '//') then Exit;
  Closing := #0;
  case C of
    '(': Closing := ')'; '[': Closing := ']'; '{': Closing := '}';
    #39, '"', '`': Closing := C;
  end;
  if Closing = #0 then Exit;
  // Apostrophes/quotes inside an identifier are ordinary input.
  if (C in [#39,'"','`']) and (Selected = 0) and (Position > 0) and
    ((Text[Position] in ['a'..'z','A'..'Z','0'..'9','_','$']) or (Ord(Text[Position]) >= $80)) then Exit;
  Value := C + SelText + Closing;
  Result := True;
  if (MaxLength > 0) and (Length(Text) - Selected + Length(Value) > MaxLength) then Exit;
  Before := Text; TextModel.ReplaceSelection(Value); Changed(Before);
  SelStart := Position + 1; SelLength := Selected;
end;
procedure TERDCodeEditor.SetTabWidth(Value: Integer);
begin FTabWidth := EnsureRange(Value, 1, 16); Invalidate; end;
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
    Opening, Closing, Column, Position, Index: Integer;
begin
  inherited;
  Saved := SaveDC(ACanvas.Handle);
  try
    if FShowIndentGuides and (FTabWidth > 0) then
    begin
      Position := Start; Column := 0; ACanvas.Pen.Color := Palette.Subtle;
      while (Position < Start + Count) and (Text[Position + 1] in [' ', #9]) do
      begin
        if Text[Position + 1] = #9 then Inc(Column, FTabWidth - Column mod FTabWidth)
        else Inc(Column);
        Inc(Position);
        if (Column mod FTabWidth = 0) and (Position < Start + Count) then
        begin
          L := X + Line.X(Position - Start);
          for Index := 0 to (RowHeight - 1) div Max(2, ScaleValue(3)) do
          begin
            ACanvas.MoveTo(L, Y + Index * Max(2, ScaleValue(3)));
            ACanvas.LineTo(L, Y + Index * Max(2, ScaleValue(3)) + 1);
          end;
        end;
      end;
    end;
    if FShowBracketMatch and MatchingBrackets(Opening, Closing) then
      for Index := 0 to 1 do
      begin
        if Index = 0 then Position := Opening else Position := Closing;
        if (Position < Start) or (Position >= Start + Count) then Continue;
        Line.Range(Position - Start, 1, L, R);
        ACanvas.Brush.Style := bsClear; ACanvas.Pen.Color := Palette.Accent;
        ACanvas.Rectangle(X + L, Y, X + R, Y + RowHeight);
      end;
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
var Before: UnicodeString; Position, Spaces, Start: Integer;
begin
  if FSmartHome and (Key = VK_HOME) and not (ssCtrl in Shift) and not (ssAlt in Shift) then
  begin
    if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift);
    if Key = 0 then Exit;
    if Key = VK_HOME then
    begin
      Start := LineStart(TextModel.Caret); Position := Start;
      while (Position < Length(Text)) and (Text[Position + 1] in [' ', #9]) do Inc(Position);
      if TextModel.Caret = Position then Position := Start;
      MoveTo(Position, ssShift in Shift); Key := 0; Exit;
    end;
    Exit;
  end;
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
var C: Char; Position, Start: Integer; Prefix, Before, Value: UnicodeString;
    Closing: Char;
begin
  if Message.CharCode = 9 then
  begin
    if FTabHandled then begin FTabHandled := False; Exit; end;
    IndentSelection(False); Exit;
  end;
  if not ReadOnly and FAutoCloseDelimiters and
    (((Message.CharCode = 8) and (GetKeyState(VK_CONTROL) >= 0)) or (Char(Message.CharCode) in ['(',')','[',']','{','}',#39,'"','`'])) then
  begin
    C := Char(Message.CharCode); KeyPress(C); if C = #0 then Exit;
    if (C = #8) and (GetKeyState(VK_CONTROL) >= 0) and (SelLength = 0) and
      (SelStart > 0) and (SelStart < Length(Text)) then
    begin
      Position := SelStart; Closing := #0;
      case Text[Position] of
        '(': Closing := ')'; '[': Closing := ']'; '{': Closing := '}';
        #39, '"', '`': Closing := Text[Position];
      end;
      if (Closing <> #0) and (Text[Position + 1] = Closing) then
      begin
        Before := Text; TextModel.ReplaceRange(Position - 1, 2, ''); Changed(Before); Exit;
      end;
    end;
    if HandleDelimiter(C) then Exit;
    // KeyPress was already dispatched above; do not dispatch it twice.
    if C = #8 then
    begin
      if GetKeyState(VK_CONTROL) >= 0 then
      begin Before := Text; TextModel.DeleteBackward; Changed(Before); end;
    end
    else ReplaceSelection(C);
    Exit;
  end;
  if (Message.CharCode = 13) and not ReadOnly and WantReturns then
  begin
    C := #13; KeyPress(C); if C = #0 then Exit;
    if C <> #13 then begin ReplaceSelection(C); Exit; end;
    Position := SelStart; Start := LineStart(Position); Prefix := '';
    while (Start < Position) and (Text[Start + 1] in [' ', #9]) do
    begin Prefix := Prefix + Text[Start + 1]; Inc(Start); end;
    Value := #13#10 + Prefix;
    if (SelLength = 0) and (Position > 0) and not LiteralAt(Position) and
      (Text[Position] in ['{','[','(']) then
    begin
      Value := Value + StringOfChar(' ', FTabWidth);
      if (Position < Length(Text)) and
        (((Text[Position] = '{') and (Text[Position + 1] = '}')) or
         ((Text[Position] = '[') and (Text[Position + 1] = ']')) or
         ((Text[Position] = '(') and (Text[Position + 1] = ')'))) then
      begin
        Start := Position + Length(Value);
        Value := Value + #13#10 + Prefix;
        if (MaxLength > 0) and (Length(Text) + Length(Value) > MaxLength) then Exit;
        ReplaceSelection(Value); MoveTo(Start, False); Exit;
      end;
    end;
    ReplaceSelection(Value); Exit;
  end;
  inherited;
end;
end.
