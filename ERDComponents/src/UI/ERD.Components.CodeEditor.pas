unit ERD.Components.CodeEditor;
// Code view over the shared Unicode engine. Syntax providers own language analysis.
{$B-}
interface
uses Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Types, System.UITypes, System.Math, System.Character, System.Generics.Collections, Vcl.Controls,
  Vcl.Graphics, Vcl.Forms, Vcl.StdCtrls, ERD.Components.Control, ERD.Components.TextInput, ERD.Components.Syntax, ERD.Components.LanguageServices, ERD.Components.PopupList, ERD.Components.ScrollBar;
type
  TERDCodeEditor = class;
  TERDKeywordColor = class(TCollectionItem)
  private
    FKeyword: string;
    FColor: TColor;
    procedure SetKeyword(const Value: string);
    procedure SetColor(Value: TColor);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Keyword: string read FKeyword write SetKeyword;
    property Color: TColor read FColor write SetColor default clDefault;
  end;
  TERDKeywordColors = class(TOwnedCollection)
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Owner: TPersistent);
  end;
  TERDLanguageResponseSink = class(TInterfacedObject, IERDLanguageResponses)
  private
    FEditor: TERDCodeEditor;
  public
    constructor Create(Editor: TERDCodeEditor);
    procedure Detach;
    function Completion(const Request: TERDLanguageRequest; const Items: TArray<TERDCompletionItem>): Boolean;
    function Hover(const Request: TERDLanguageRequest; const Text: UnicodeString): Boolean;
    function Definition(const Request: TERDLanguageRequest; const Target: TERDDefinitionTarget): Boolean;
  end;
  TERDCodeNavigateEvent = procedure(Sender: TObject; const Target: TERDDefinitionTarget) of object;
  TERDCodeSearchRequestEvent = procedure(Sender: TObject; Replacing: Boolean) of object;
  TERDCodeSearchOption = (soMatchCase, soWholeWord, soRegex);
  TERDCodeSearchOptions = set of TERDCodeSearchOption;
  TERDCodeSearchMatch = record Start, Count: Integer; end;
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
    FKeywordColors: TERDKeywordColors;
    FAnalysis: TERDSyntaxAnalysis;
    FAnalyzedText: UnicodeString;
    FAnalysisValid: Boolean;
    FCollapsed: TList<Integer>;
    FShowLineNumbers, FShowFolding, FShowFoldGuides: Boolean;
    FHotFold, FPressedFold: Integer;
    FSelectingLines, FLastGutterDown: Boolean;
    FLineAnchorStart, FLineAnchorFinish: Integer;
    FScrollBars: System.UITypes.TScrollStyle;
    FShowMinimap: Boolean;
    FMinimapDirty, FNavigationWidthDirty, FMinimapWindowHot: Boolean;
    FMinimapWidth: Integer;
    FNavigationWidth: Integer;
    FWidthCache: TDictionary<UnicodeString, Integer>;
    FWidthCacheFont: HFONT;
    FNavigationMeasuredText: UnicodeString;
    FNavigationHot, FNavigationDrag, FNavigationDragOffset: Integer;
    FMinimapDragRow: Integer;
    FMinimapBitmap: TBitmap;
    FMinimapFill, FMinimapColorKey: TColor;
    FHandlingFoldRelease: Boolean;
    FTabWidth: Integer;
    FInterfaceKeywordColor, FClassKeywordColor, FModuleKeywordColor: TColor;
    FTabHandled: Boolean;
    FAutoCloseDelimiters, FSmartHome, FShowIndentGuides, FShowBracketMatch: Boolean;
    FDiagnostics: TArray<TERDCodeDiagnostic>;
    FBracketText: UnicodeString;
    FBracketCaret, FBracketOpening, FBracketClosing: Integer;
    FBracketValid: Boolean;
    FOnSearchRequested: TERDCodeSearchRequestEvent;
    FSearchPattern, FSearchError: UnicodeString;
    FSearchOptions: TERDCodeSearchOptions;
    FSearchMatches: TArray<TERDCodeSearchMatch>;
    FSearchScopeStart, FSearchScopeCount, FSearchIndex: Integer;
    FLanguageService: IERDCodeLanguageService;
    FResponseObject: TERDLanguageResponseSink;
    FResponses: IERDLanguageResponses;
    FPending: array[TERDLanguageRequestKind] of Int64;
    FVersion, FRequestID: Int64;
    FLastCaret, FLastAnchor: Integer;
    FTrackCaret: Boolean;
    FAutoMemberCompletion: Boolean;
    FDocumentUri, FLanguageError, FHoverText: UnicodeString;
    FCompletionItems: TArray<TERDCompletionItem>;
    FCompletionStamp: TERDLanguageRequest;
    FCompletionWordStart: Integer;
    FCompletionPopup: TERDPopupList;
    FSuppressLanguageChar: Word;
    FLastLanguageResult: TERDLanguageRequestKind;
    FOnLanguageResults: TNotifyEvent;
    FOnNavigate: TERDCodeNavigateEvent;
    procedure SetScrollBars(Value: System.UITypes.TScrollStyle);
    procedure SetShowMinimap(Value: Boolean);
    procedure SetMinimapWidth(Value: Integer);
    function NavigationAt(X, Y: Integer): Integer;
    function MinimapViewportRect: TRect;
    function NavigationWidth: Integer;
    procedure NavigatePointer(X, Y: Integer);
    function NavigationMouseDown(X, Y: Integer): Boolean;
    procedure PaintNavigation(ACanvas: TCanvas);
    procedure RenderMinimap(Width, Height: Integer; const Rows: TArray<TERDTextRow>; Fill: TColor);
    procedure SetKeywordColors(Value: TERDKeywordColors);
    function ResolveKeywordColor(const Word: string; Kind: TERDSyntaxKind): TColor;
    procedure SyncLanguageDocument;
    procedure SetLanguageService(const Value: IERDCodeLanguageService);
    procedure SetDocumentUri(const Value: UnicodeString);
    function SnapshotCurrent(const Snapshot: TERDLanguageRequest): Boolean;
    function AcceptResponse(const Snapshot: TERDLanguageRequest; Kind: TERDLanguageRequestKind): Boolean;
    function IssueRequest(Kind: TERDLanguageRequestKind; Position: Integer; Trigger: WideChar = #0): Boolean;
    procedure CompletionClosed(Sender: TObject; Accepted: Boolean; Index: Integer);
    procedure RefreshSearch;
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
    procedure SetShowFoldGuides(Value: Boolean);
    function FoldButtonRect(Row: Integer): TRect;
    function GutterBounds: TRect;
    function FoldAtPoint(X, Y: Integer): Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure SetTabWidth(Value: Integer);
    procedure SetInterfaceKeywordColor(Value: TColor);
    procedure SetClassKeywordColor(Value: TColor);
    procedure SetModuleKeywordColor(Value: TColor);
    function FoldHiddenStart(const Fold: TERDSyntaxFold): Integer;
    function FoldHiddenFinish(const Fold: TERDSyntaxFold): Integer;
    function FoldIndex(Position: Integer): Integer;
    function LineStart(Position: Integer): Integer;
    function LineFinish(Position: Integer): Integer;
    procedure SelectGutterLine(Y: Integer; Extend: Boolean);
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  protected
    procedure NavigationRects(out Vertical, Horizontal, Map: TRect);
    function TextViewport: TRect; override;
    procedure FilterRows(var Rows: TArray<TERDTextRow>); override;
    procedure PaintTextLine(ACanvas: TCanvas; Start, Count, X, Y: Integer;
      Line: TERDTextLayoutLine); override;
    procedure PaintTextBackground(ACanvas: TCanvas; Start, Count, X, Y: Integer;
      Line: TERDTextLayoutLine); override;
    procedure PaintTextOverlay(ACanvas: TCanvas; Start, Count, X, Y: Integer;
      Line: TERDTextLayoutLine); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure UpdateCaret; override;
    procedure DoExit; override;
    procedure DblClick; override;
    procedure Changed(const ABefore: UnicodeString); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    procedure ThemeChanged; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function FoldInteractionRevision: Integer; static;
    function RequestCompletion: Boolean;
    function RequestHover(Position: Integer = -1): Boolean;
    function RequestDefinition(Position: Integer = -1): Boolean;
    procedure CancelLanguageRequests;
    function PublishCompletions(const Request: TERDLanguageRequest; const Items: TArray<TERDCompletionItem>): Boolean;
    function PublishHover(const Request: TERDLanguageRequest; const Text: UnicodeString): Boolean;
    function PublishDefinition(const Request: TERDLanguageRequest; const Target: TERDDefinitionTarget): Boolean;
    function Completions: TArray<TERDCompletionItem>;
    function ApplyCompletion(Index: Integer): Boolean;
    property LanguageService: IERDCodeLanguageService read FLanguageService write SetLanguageService;
    property DocumentUri: UnicodeString read FDocumentUri write SetDocumentUri;
    property DocumentVersion: Int64 read FVersion;
    property LanguageError: UnicodeString read FLanguageError;
    property LanguageResultKind: TERDLanguageRequestKind read FLastLanguageResult;
    property HoverText: UnicodeString read FHoverText;
    function SetSearch(const Pattern: UnicodeString; Options: TERDCodeSearchOptions = [];
      ScopeStart: Integer = 0; ScopeCount: Integer = -1): Boolean;
    procedure ClearSearch;
    function SearchMatches: TArray<TERDCodeSearchMatch>;
    function FindNext(Backwards: Boolean = False): Boolean;
    function ReplaceCurrent(const Replacement: UnicodeString): Boolean;
    function ReplaceAll(const Replacement: UnicodeString): Integer;
    property SearchError: UnicodeString read FSearchError;
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
    property ScrollBars: System.UITypes.TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property ShowMinimap: Boolean read FShowMinimap write SetShowMinimap default True;
    property MinimapWidth: Integer read FMinimapWidth write SetMinimapWidth default 96;
    property AutoMemberCompletion: Boolean read FAutoMemberCompletion write FAutoMemberCompletion default True;
    property OnLanguageResults: TNotifyEvent read FOnLanguageResults write FOnLanguageResults;
    property OnNavigate: TERDCodeNavigateEvent read FOnNavigate write FOnNavigate;
    property OnSearchRequested: TERDCodeSearchRequestEvent read FOnSearchRequested write FOnSearchRequested;
    property Language: TERDCodeLanguage read FLanguage write SetLanguage default clTypeScript;
    property ShowLineNumbers: Boolean read FShowLineNumbers write SetShowLineNumbers default True;
    property ShowFolding: Boolean read FShowFolding write SetShowFolding default True;
    property KeywordColors: TERDKeywordColors read FKeywordColors write SetKeywordColors;
    property InterfaceKeywordColor: TColor read FInterfaceKeywordColor write SetInterfaceKeywordColor default clDefault;
    property ClassKeywordColor: TColor read FClassKeywordColor write SetClassKeywordColor default clDefault;
    property ModuleKeywordColor: TColor read FModuleKeywordColor write SetModuleKeywordColor default clDefault;
    property ShowFoldGuides: Boolean read FShowFoldGuides write SetShowFoldGuides default True;
    property AutoCloseDelimiters: Boolean read FAutoCloseDelimiters write FAutoCloseDelimiters default True;
    property SmartHome: Boolean read FSmartHome write FSmartHome default True;
    property ShowIndentGuides: Boolean read FShowIndentGuides write SetShowIndentGuides default True;
    property ShowBracketMatch: Boolean read FShowBracketMatch write SetShowBracketMatch default True;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 2;
  end;
implementation
uses System.RegularExpressions, Winapi.GDIPAPI, Winapi.GDIPOBJ,
  ERD.Components.Paint, ERD.Components.GDIP;
constructor TERDKeywordColor.Create(Collection: TCollection);
begin
  inherited;
  FColor := clDefault;
end;
procedure TERDKeywordColor.SetKeyword(const Value: string);
begin if FKeyword = Value then Exit; FKeyword := Value; Changed(False); end;
procedure TERDKeywordColor.SetColor(Value: TColor);
begin if FColor = Value then Exit; FColor := Value; Changed(False); end;
function TERDKeywordColor.GetDisplayName: string;
begin if FKeyword = '' then Result := inherited GetDisplayName else Result := FKeyword; end;
procedure TERDKeywordColor.Assign(Source: TPersistent);
begin
  if Source is TERDKeywordColor then
  begin FKeyword := TERDKeywordColor(Source).Keyword; FColor := TERDKeywordColor(Source).Color; Changed(False); end
  else inherited;
end;
constructor TERDKeywordColors.Create(Owner: TPersistent);
begin inherited Create(Owner, TERDKeywordColor); end;
procedure TERDKeywordColors.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TERDCodeEditor then
  begin TERDCodeEditor(GetOwner).FMinimapDirty := True; TERDCodeEditor(GetOwner).Invalidate; end;
end;
procedure TERDCodeEditor.SetKeywordColors(Value: TERDKeywordColors);
begin FKeywordColors.Assign(Value); end;
function TERDCodeEditor.ResolveKeywordColor(const Word: string; Kind: TERDSyntaxKind): TColor;
var I: Integer; Entry: TERDKeywordColor; Dark: Boolean;
begin
  for I := FKeywordColors.Count - 1 downto 0 do
  begin
    Entry := TERDKeywordColor(FKeywordColors.Items[I]);
    if (Entry.Keyword = Word) and (Entry.Color <> clDefault) then Exit(Entry.Color);
  end;
  Dark := ERDIsDarkPalette(Palette); Result := Palette.Accent;
  case Kind of
    skInterfaceKeyword:
      begin
        Result := FInterfaceKeywordColor;
        if Result = clDefault then if Dark then Result := $00FFD080 else Result := $00AA5500;
      end;
    skClassKeyword:
      begin
        Result := FClassKeywordColor;
        if Result = clDefault then if Dark then Result := $00B0DF70 else Result := $006E7800;
      end;
    skModuleKeyword:
      begin
        Result := FModuleKeywordColor;
        if Result = clDefault then if Dark then Result := $00E8A8C8 else Result := $00802080;
      end;
  else
    if Pos('|' + Word + '|', '|any|bigint|boolean|intrinsic|never|number|object|string|symbol|unique|unknown|void|') > 0 then
    begin if Dark then Result := $00FFD080 else Result := $00AA5500; end
    else if Pos('|' + Word + '|', '|break|case|catch|continue|debugger|default|do|else|finally|for|if|return|switch|throw|try|while|yield|await|') > 0 then
    begin if Dark then Result := $00E8A8C8 else Result := $00802080; end
    else if Pos('|' + Word + '|', '|abstract|accessor|async|declare|out|override|private|protected|public|readonly|static|') > 0 then
    begin if Dark then Result := $00D8BEA0 else Result := $00906040; end
    else if Pos('|' + Word + '|', '|const|constructor|enum|function|get|global|let|namespace|package|set|type|using|var|module|') > 0 then
    begin if Dark then Result := $00B0DF70 else Result := $006E7800; end
    else if Pos('|' + Word + '|', '|true|false|null|undefined|this|super|') > 0 then
      Result := Palette.Warning
    else if Pos('|' + Word + '|', '|as|asserts|delete|extends|implements|in|infer|instanceof|is|keyof|new|of|satisfies|typeof|') > 0 then
    begin if Dark then Result := $00E0A0F0 else Result := $00903090; end;
  end;
end;
class function TERDCodeEditor.FoldInteractionRevision: Integer;
begin Result := 2; end;
constructor TERDCodeEditor.Create(AOwner: TComponent);
var Word: string;
begin
  inherited Create(AOwner);
  FMinimapBitmap := TBitmap.Create; FMinimapBitmap.PixelFormat := pf32bit;
  FWidthCache := TDictionary<UnicodeString, Integer>.Create;
  FScrollBars := ssBoth;
  FShowMinimap := True;
  FMinimapWidth := 96;
  FMinimapDirty := True; FNavigationWidthDirty := True;
  FCollapsed := TList<Integer>.Create;
  FResponseObject := TERDLanguageResponseSink.Create(Self); FResponses := FResponseObject;
  FSearchScopeCount := -1;
  FSearchIndex := -1;
  FLanguage := clTypeScript;
  FTabWidth := 2;
  FInterfaceKeywordColor := clDefault;
  FClassKeywordColor := clDefault;
  FModuleKeywordColor := clDefault;
  FAutoMemberCompletion := True;
  FAutoCloseDelimiters := True;
  FSmartHome := True;
  FShowIndentGuides := True;
  FShowBracketMatch := True;
  FShowLineNumbers := True;
  FShowFolding := True;
  FShowFoldGuides := True;
  FHotFold := -1; FPressedFold := -1;
  FKeywordColors := TERDKeywordColors.Create(Self);
  FKeywordColors.BeginUpdate;
  try
    for Word in ERDTypeScriptKeywords do TERDKeywordColor(FKeywordColors.Add).Keyword := Word;
  finally FKeywordColors.EndUpdate; end;
  FProvider := TERDBasicSyntaxProvider.Create(FLanguage);
  WordWrap := False; WantTabs := True; AutoSelect := False;
  Font.Name := 'Consolas';
end;
destructor TERDCodeEditor.Destroy;
begin
  if FResponseObject <> nil then FResponseObject.Detach;
  CancelLanguageRequests; FResponses := nil; FResponseObject := nil;
  if FCompletionPopup <> nil then FCompletionPopup.OnCloseUp := nil;
  FWidthCache.Free; FMinimapBitmap.Free; FKeywordColors.Free; FCollapsed.Free; inherited Destroy;
end;
constructor TERDLanguageResponseSink.Create(Editor: TERDCodeEditor);
begin inherited Create; FEditor := Editor; end;
procedure TERDLanguageResponseSink.Detach;
begin FEditor := nil; end;
function TERDLanguageResponseSink.Completion(const Request: TERDLanguageRequest; const Items: TArray<TERDCompletionItem>): Boolean;
begin Result := (GetCurrentThreadId = MainThreadID) and (FEditor <> nil); if Result then Result := FEditor.PublishCompletions(Request, Items); end;
function TERDLanguageResponseSink.Hover(const Request: TERDLanguageRequest; const Text: UnicodeString): Boolean;
begin Result := (GetCurrentThreadId = MainThreadID) and (FEditor <> nil); if Result then Result := FEditor.PublishHover(Request, Text); end;
function TERDLanguageResponseSink.Definition(const Request: TERDLanguageRequest; const Target: TERDDefinitionTarget): Boolean;
begin Result := (GetCurrentThreadId = MainThreadID) and (FEditor <> nil); if Result then Result := FEditor.PublishDefinition(Request, Target); end;
procedure TERDCodeEditor.SyncLanguageDocument;
var Service: IERDCodeDocumentService;
begin
  if Supports(FLanguageService, IERDCodeDocumentService, Service) then
    try Service.DocumentChanged(FDocumentUri, Text, FVersion);
    except on E: Exception do FLanguageError := E.Message; end;
end;
procedure TERDCodeEditor.SetLanguageService(const Value: IERDCodeLanguageService);
begin CancelLanguageRequests; FLanguageService := Value; FLanguageError := ''; SyncLanguageDocument; end;
procedure TERDCodeEditor.SetDocumentUri(const Value: UnicodeString);
begin if FDocumentUri = Value then Exit; CancelLanguageRequests; FDocumentUri := Value; Inc(FVersion); SyncLanguageDocument; end;
procedure TERDCodeEditor.DoExit;
begin
  FSelectingLines := False; FLastGutterDown := False; FNavigationDrag := 0; FMinimapWindowHot := False;
  FHotFold := -1;
  if FPressedFold >= 0 then begin FPressedFold := -1; MouseCapture := False; end;
  CancelLanguageRequests; inherited;
end;
procedure TERDCodeEditor.CancelLanguageRequests;
var Kind: TERDLanguageRequestKind; Pending: array[TERDLanguageRequestKind] of Int64;
begin
  // Invalidate every request before invoking any provider/user callback.
  for Kind := Low(TERDLanguageRequestKind) to High(TERDLanguageRequestKind) do
  begin Pending[Kind] := FPending[Kind]; FPending[Kind] := 0; end;
  FCompletionItems := nil; FCompletionStamp.ID := 0; FHoverText := '';
  if FCompletionPopup <> nil then FCompletionPopup.CloseUp(False);
  for Kind := Low(TERDLanguageRequestKind) to High(TERDLanguageRequestKind) do
    if (Pending[Kind] <> 0) and (FLanguageService <> nil) then
      try FLanguageService.Cancel(Pending[Kind]); except on E: Exception do FLanguageError := E.Message; end;
end;
function TERDCodeEditor.SnapshotCurrent(const Snapshot: TERDLanguageRequest): Boolean;
begin
  Result := (Snapshot.Version = FVersion) and (Snapshot.Text = Text) and
    (Snapshot.DocumentUri = FDocumentUri) and (Snapshot.Caret = TextModel.Caret) and (Snapshot.Anchor = TextModel.Anchor);
end;
function TERDCodeEditor.AcceptResponse(const Snapshot: TERDLanguageRequest; Kind: TERDLanguageRequestKind): Boolean;
begin
  Result := (GetCurrentThreadId = MainThreadID) and (Snapshot.Kind = Kind) and
    (Snapshot.ID <> 0) and (Snapshot.ID = FPending[Kind]) and SnapshotCurrent(Snapshot);
end;
function TERDCodeEditor.IssueRequest(Kind: TERDLanguageRequestKind; Position: Integer; Trigger: WideChar): Boolean;
var Snapshot: TERDLanguageRequest; Old: Int64;
begin
  Result := FLanguageService <> nil; if not Result then Exit;
  Snapshot := Default(TERDLanguageRequest);
  Old := FPending[Kind]; FPending[Kind] := 0;
  try
    if Old <> 0 then FLanguageService.Cancel(Old);
    Inc(FRequestID); Snapshot.ID := FRequestID; Snapshot.Kind := Kind;
    Snapshot.TriggerCharacter := Trigger;
    Snapshot.Version := FVersion; Snapshot.Text := Text; Snapshot.DocumentUri := FDocumentUri;
    Snapshot.Caret := TextModel.Caret; Snapshot.Anchor := TextModel.Anchor;
    if Position < 0 then Position := Snapshot.Caret;
    Snapshot.Position := TextModel.Snap(Position); FPending[Kind] := Snapshot.ID;
    FLanguageError := '';
    if Kind = lrCompletion then
    begin
      FCompletionWordStart := Snapshot.Caret;
      while (FCompletionWordStart > 0) and
        (Text[FCompletionWordStart].IsLetterOrDigit or CharInSet(Text[FCompletionWordStart], ['_', '$'])) do Dec(FCompletionWordStart);
      FCompletionItems := nil; FCompletionStamp.ID := 0;
      if FCompletionPopup <> nil then FCompletionPopup.CloseUp(False);
    end;
    FLanguageService.Request(Snapshot, FResponses);
  except on E: Exception do
    begin
      if FPending[Kind] = Snapshot.ID then FPending[Kind] := 0;
      FLanguageError := E.Message; Result := False;
      FLastLanguageResult := Kind;
      if Assigned(FOnLanguageResults) then FOnLanguageResults(Self);
    end;
  end;
end;
function TERDCodeEditor.RequestCompletion: Boolean;
begin
  if FLanguageService = nil then
  begin
    FLanguageError := 'No language service is connected. Connect TSVM to enable completion.';
    FLastLanguageResult := lrCompletion;
    if Assigned(FOnLanguageResults) then FOnLanguageResults(Self);
    Exit(False);
  end;
  Result := IssueRequest(lrCompletion, -1);
end;
function TERDCodeEditor.RequestHover(Position: Integer): Boolean;
begin Result := IssueRequest(lrHover, Position); end;
function TERDCodeEditor.RequestDefinition(Position: Integer): Boolean;
begin Result := IssueRequest(lrDefinition, Position); end;
function TERDCodeEditor.Completions: TArray<TERDCompletionItem>;
begin Result := Copy(FCompletionItems); end;
function TERDCodeEditor.PublishCompletions(const Request: TERDLanguageRequest; const Items: TArray<TERDCompletionItem>): Boolean;
var Item: TERDCompletionItem; Valid: TList<TERDCompletionItem>; Labels: TStringList; P: TPoint;
begin
  Result := AcceptResponse(Request, lrCompletion); if not Result then Exit;
  Valid := TList<TERDCompletionItem>.Create; Labels := TStringList.Create;
  try
    for Item in Items do
      if (Item.Start >= 0) and (Item.Start <= Length(Text)) and (Item.Count >= 0) and
        (Item.Count <= Length(Text) - Item.Start) and (TextModel.Snap(Item.Start) = Item.Start) and
        (TextModel.Snap(Item.Start + Item.Count) = Item.Start + Item.Count) then
      begin
        Valid.Add(Item);
        if Item.Detail = '' then Labels.Add(Item.LabelText) else Labels.Add(Item.LabelText + '  -  ' + Item.Detail);
      end;
    FPending[lrCompletion] := 0; FCompletionItems := Valid.ToArray; FCompletionStamp := Request;
    if (Labels.Count = 0) and (Request.TriggerCharacter = #0) then Labels.Add('No suggestions');
    if Focused and (Labels.Count > 0) then
    begin
      if FCompletionPopup = nil then
      begin FCompletionPopup := TERDPopupList.Create(Self); FCompletionPopup.OnCloseUp := CompletionClosed; end;
      if not GetCaretPos(P) then P := Point(TextViewport.Left, TextViewport.Top);
      P := ClientToScreen(P);
      FCompletionPopup.Popup(Self, Rect(P.X, P.Y, P.X + ScaleValue(320), P.Y + RowHeight),
        Labels, 0, Palette, ScaleValue(96), ScaleValue(28), 8);
    end;
  finally Labels.Free; Valid.Free; end;
  FLastLanguageResult := lrCompletion;
  if Assigned(FOnLanguageResults) then FOnLanguageResults(Self);
end;
procedure TERDCodeEditor.CompletionClosed(Sender: TObject; Accepted: Boolean; Index: Integer);
begin
  if Accepted then ApplyCompletion(Index);
  FCompletionItems := nil; FCompletionStamp.ID := 0;
end;
function TERDCodeEditor.ApplyCompletion(Index: Integer): Boolean;
var Item: TERDCompletionItem; Before, Value: UnicodeString;
begin
  Result := not ReadOnly and (Index >= 0) and (Index < Length(FCompletionItems)) and
    (FCompletionStamp.ID <> 0) and SnapshotCurrent(FCompletionStamp);
  if not Result then Exit;
  Item := FCompletionItems[Index]; Value := NormalizeInput(Item.InsertText); Before := Text;
  if (MaxLength > 0) and (Length(Text) - Item.Count + Length(Value) > MaxLength) then Exit(False);
  TextModel.ReplaceRange(Item.Start, Item.Count, Value); Changed(Before);
  FCompletionItems := nil; FCompletionStamp.ID := 0;
end;
function TERDCodeEditor.PublishHover(const Request: TERDLanguageRequest; const Text: UnicodeString): Boolean;
begin
  Result := AcceptResponse(Request, lrHover); if not Result then Exit;
  FPending[lrHover] := 0; FHoverText := Text; FLastLanguageResult := lrHover;
  if Assigned(FOnLanguageResults) then FOnLanguageResults(Self);
end;
function TERDCodeEditor.PublishDefinition(const Request: TERDLanguageRequest; const Target: TERDDefinitionTarget): Boolean;
begin
  Result := AcceptResponse(Request, lrDefinition); if not Result then Exit;
  if not Target.Found then
  begin
    FPending[lrDefinition] := 0; FLastLanguageResult := lrDefinition;
    if Assigned(FOnLanguageResults) then FOnLanguageResults(Self); Exit;
  end;
  if (Target.Start < 0) or (Target.Count < 0) or (Target.Count > MaxInt - Target.Start) then Exit(False);
  if (Target.DocumentUri = '') or (Target.DocumentUri = FDocumentUri) then
  begin
    if (Target.Start > Length(Text)) or (Target.Count > Length(Text) - Target.Start) then Exit(False);
    if (TextModel.Snap(Target.Start) <> Target.Start) or (TextModel.Snap(Target.Start + Target.Count) <> Target.Start + Target.Count) then Exit(False);
    FPending[lrDefinition] := 0; TextModel.Select(Target.Start, Target.Start + Target.Count); Changed(Text);
  end
  else FPending[lrDefinition] := 0;
  FLastLanguageResult := lrDefinition;
  if Assigned(FOnLanguageResults) then FOnLanguageResults(Self);
  if Assigned(FOnNavigate) then FOnNavigate(Self, Target);
end;
procedure TERDCodeEditor.Analyze;
var Span, ValidSpan: TERDSyntaxSpan; Fold: TERDSyntaxFold;
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
      begin
        ValidSpan := Span;
        ValidSpan.Count := Min(Span.Count, Length(Text) - Span.Start); Spans.Add(ValidSpan);
      end;
    for Fold in FAnalysis.Folds do
      if (Fold.Start >= 0) and (Fold.Finish <= Length(Text)) and
        (Fold.Start < Fold.Finish) and (FoldHiddenStart(Fold) < FoldHiddenFinish(Fold)) then
        Folds.Add(Fold);
    FAnalysis.Spans := Spans.ToArray; FAnalysis.Folds := Folds.ToArray;
  finally Folds.Free; Spans.Free; end;
  FAnalyzedText := Text; FAnalysisValid := True; FMinimapDirty := True;
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
  CancelLanguageRequests; FLanguage := Value; SetProvider(TERDBasicSyntaxProvider.Create(Value));
end;
procedure TERDCodeEditor.SetScrollBars(Value: System.UITypes.TScrollStyle);
begin
  if FScrollBars = Value then Exit;
  FScrollBars := Value; FNavigationDrag := 0;
  InvalidateTextLayout; UpdateCaret; Invalidate;
end;
procedure TERDCodeEditor.SetShowMinimap(Value: Boolean);
begin
  if FShowMinimap = Value then Exit;
  FShowMinimap := Value; FNavigationDrag := 0;
  InvalidateTextLayout; UpdateCaret; Invalidate;
end;
procedure TERDCodeEditor.SetMinimapWidth(Value: Integer);
begin
  Value := EnsureRange(Value, 32, 320); if FMinimapWidth = Value then Exit;
  FMinimapWidth := Value; FMinimapDirty := True;
  InvalidateTextLayout; UpdateCaret; Invalidate;
end;
procedure TERDCodeEditor.NavigationRects(out Vertical, Horizontal, Map: TRect);
var Inner, Base: TRect; BorderWidth, Size, Width, LocalGutterWidth, TextRight, Available: Integer;
begin
  Inner := ClientRect; BorderWidth := 0;
  if BorderStyle = bsSingle then
  begin
    BorderWidth := ScaleValue(1);
    if Focused and Enabled then BorderWidth := ScaleValue(2);
  end;
  InflateRect(Inner, -BorderWidth, -BorderWidth);
  Vertical := Rect(0, 0, 0, 0); Horizontal := Vertical; Map := Vertical;
  if (Inner.Width <= 0) or (Inner.Height <= 0) then Exit;
  Size := Max(1, ScaleValue(12)); Base := inherited TextViewport;
  LocalGutterWidth := 0;
  if FShowLineNumbers then Inc(LocalGutterWidth, ScaleValue(48));
  if FShowFolding then Inc(LocalGutterWidth, ScaleValue(18));
  if FScrollBars in [ssVertical, ssBoth] then
  begin
    Vertical := Rect(Max(Inner.Left, Inner.Right - Size), Inner.Top, Inner.Right, Inner.Bottom);
    Inner.Right := Vertical.Left;
  end;
  Width := 0;
  if FShowMinimap then
  begin
    Width := Min(ScaleValue(FMinimapWidth), Max(0, (Inner.Width - LocalGutterWidth - ScaleValue(32)) div 2));
    if Width < ScaleValue(16) then Width := 0;
  end;
  TextRight := Min(Base.Right, Inner.Right);
  if Width > 0 then TextRight := Min(TextRight, Inner.Right - Width - ScaleValue(3));
  Available := Max(1, TextRight - Base.Left - LocalGutterWidth);
  if (FScrollBars in [ssHorizontal, ssBoth]) and not WordWrap and (NavigationWidth > Available) then
  begin
    Horizontal := Rect(Min(Inner.Right, Base.Left + LocalGutterWidth),
      Max(Inner.Top, Inner.Bottom - Size), Inner.Right, Inner.Bottom);
    Inner.Bottom := Horizontal.Top;
    if not IsRectEmpty(Vertical) then Vertical.Bottom := Inner.Bottom;
  end;
  if Width > 0 then Map := Rect(Inner.Right - Width, Inner.Top, Inner.Right, Inner.Bottom);
end;
function TERDCodeEditor.MinimapViewportRect: TRect;
var Vertical, Horizontal, Map: TRect; Rows: TArray<TERDTextRow>; MapHeight, Top, Bottom: Integer;
begin
  Result := Rect(0, 0, 0, 0); NavigationRects(Vertical, Horizontal, Map);
  if IsRectEmpty(Map) then Exit;
  Rows := VisualRows; if Length(Rows) = 0 then Exit;
  MapHeight := Min(Map.Height, Length(Rows) * ScaleValue(3));
  Top := Map.Top + MulDiv(FirstVisibleRow, MapHeight, Length(Rows));
  Bottom := Map.Top + MulDiv(Min(Length(Rows), FirstVisibleRow + VisibleRowCount), MapHeight, Length(Rows));
  Result := Rect(Map.Left, Top, Map.Right, Min(Map.Bottom, Max(Top + ScaleValue(4), Bottom)));
end;
function TERDCodeEditor.NavigationAt(X, Y: Integer): Integer;
var Vertical, Horizontal, Map: TRect; P: TPoint;
begin
  NavigationRects(Vertical, Horizontal, Map); P := Point(X, Y);
  Result := 0;
  if PtInRect(Vertical, P) then Result := 1
  else if PtInRect(Horizontal, P) then Result := 2
  else if PtInRect(Map, P) then Result := 3;
end;
function TERDCodeEditor.NavigationWidth: Integer;
var Line: TERDTextLayoutLine; LineText: UnicodeString;
    Start, Finish, LineWidth: Integer; Fold: TERDSyntaxFold; Hidden: Boolean;
begin
  // Width measurement must not call VisualRows/TextViewport: layout itself asks
  // for this width to decide whether to reserve a horizontal scrollbar.
  if not FNavigationWidthDirty and (FNavigationMeasuredText = Text) and
    (FWidthCacheFont = Font.Handle) then Exit(FNavigationWidth);
  FNavigationWidth := 0; Canvas.Font.Assign(Font);
  if FWidthCacheFont <> Font.Handle then
  begin FWidthCache.Clear; FWidthCacheFont := Font.Handle; end;
  if (FCollapsed <> nil) and (FCollapsed.Count > 0) then Analyze;
  Start := 0;
  while Start < Length(Text) do
  begin
    Finish := Start;
    while (Finish < Length(Text)) and (Text[Finish + 1] <> #13) do Inc(Finish);
    Hidden := False;
    if (FCollapsed <> nil) and (FCollapsed.Count > 0) then
      for Fold in FAnalysis.Folds do
        if FCollapsed.Contains(Fold.Start) and (Start >= FoldHiddenStart(Fold)) and
          (Start < FoldHiddenFinish(Fold)) then begin Hidden := True; Break; end;
    if not Hidden then
    begin
      LineText := Copy(Text, Start + 1, Finish - Start);
      if not FWidthCache.TryGetValue(LineText, LineWidth) then
      begin
        Line := TERDTextLayoutLine.Create(Canvas, LineText);
        try LineWidth := Line.Width; finally Line.Free; end;
        if Length(LineText) <= 4096 then
        begin
          if FWidthCache.Count >= 1024 then FWidthCache.Clear;
          FWidthCache.Add(LineText, LineWidth);
        end;
      end;
      FNavigationWidth := Max(FNavigationWidth, LineWidth);
    end;
    Start := Finish + 2;
  end;
  FNavigationWidthDirty := False; FNavigationMeasuredText := Text;
  Result := FNavigationWidth;
end;
procedure TERDCodeEditor.NavigatePointer(X, Y: Integer);
var Vertical, Horizontal, Map, Thumb: TRect; Rows: TArray<TERDTextRow>;
    Range, Travel, Value, Page, MapHeight: Integer;
begin
  NavigationRects(Vertical, Horizontal, Map); Rows := VisualRows; Page := VisibleRowCount;
  case FNavigationDrag of
    1:
      begin
        Thumb := ERDScrollThumbRect(Vertical, True, 0, Max(0, Length(Rows) - 1), Page, FirstVisibleRow, True, True);
        Travel := Vertical.Height - Thumb.Height; Range := Max(0, Length(Rows) - Page);
        if Travel <= 0 then Exit;
        Value := Round(EnsureRange((Y - Vertical.Top - FNavigationDragOffset) / Travel, 0.0, 1.0) * Range);
        ScrollRows(Value);
      end;
    2:
      begin
        Range := Max(0, NavigationWidth - TextViewport.Width);
        Thumb := ERDScrollThumbRect(Horizontal, False, 0, Max(0, NavigationWidth - 1), TextViewport.Width,
          HorizontalScrollPosition, True, True);
        Travel := Horizontal.Width - Thumb.Width; if Travel <= 0 then Exit;
        Value := Round(EnsureRange((X - Horizontal.Left - FNavigationDragOffset) / Travel, 0.0, 1.0) * Range);
        ScrollHorizontal(Value);
      end;
    3:
      begin
        if Map.Height <= 0 then Exit;
        MapHeight := Max(1, Min(Map.Height, Length(Rows) * ScaleValue(3)));
        Value := FMinimapDragRow + Round((Y - FNavigationDragOffset) / MapHeight * Length(Rows));
        ScrollRows(Value);
      end;
  end;
end;
function TERDCodeEditor.NavigationMouseDown(X, Y: Integer): Boolean;
var Vertical, Horizontal, Map, Thumb: TRect; Rows: TArray<TERDTextRow>;
    Hit, Page, Value, MapHeight: Integer;
begin
  Hit := NavigationAt(X, Y); Result := Hit <> 0; if not Result then Exit;
  FLastGutterDown := True; // Prevent VCL's double-click word selection on navigation surfaces.
  FNavigationHot := Hit; NavigationRects(Vertical, Horizontal, Map);
  Rows := VisualRows; Page := VisibleRowCount;
  if Hit = 3 then
  begin
    FNavigationDrag := Hit; Thumb := MinimapViewportRect;
    FNavigationDragOffset := Y; FMinimapDragRow := FirstVisibleRow;
    if not PtInRect(Thumb, Point(X, Y)) then
    begin
      MapHeight := Max(1, Min(Map.Height, Length(Rows) * ScaleValue(3)));
      FMinimapDragRow := Round((Y - Map.Top - Thumb.Height * 0.5) / MapHeight * Length(Rows));
    end;
  end
  else
  begin
    if Hit = 1 then
      Thumb := ERDScrollThumbRect(Vertical, True, 0, Max(0, Length(Rows) - 1), Page, FirstVisibleRow, True, True)
    else Thumb := ERDScrollThumbRect(Horizontal, False, 0, Max(0, NavigationWidth - 1), TextViewport.Width,
      HorizontalScrollPosition, True, True);
    if PtInRect(Thumb, Point(X, Y)) then
    begin
      FNavigationDrag := Hit;
      if Hit = 1 then FNavigationDragOffset := Y - Thumb.Top
      else FNavigationDragOffset := X - Thumb.Left;
    end
    else if Hit = 1 then
    begin
      Value := Page; if Y < Thumb.Top then Value := -Value;
      ScrollRows(FirstVisibleRow + Value);
    end
    else
    begin
      Value := TextViewport.Width; if X < Thumb.Left then Value := -Value;
      ScrollHorizontal(EnsureRange(HorizontalScrollPosition + Value, 0, Max(0, NavigationWidth - TextViewport.Width)));
    end;
  end;
  if FNavigationDrag <> 0 then
  begin
    MouseCapture := True;
    if Hit = 3 then begin NavigatePointer(X, Y); FMinimapDragRow := FirstVisibleRow; end;
  end;
  Invalidate;
end;
procedure TERDCodeEditor.PaintNavigation(ACanvas: TCanvas);
var Vertical, Horizontal, Map, Viewport: TRect; Painter: TERDPainter;
    Rows: TArray<TERDTextRow>; Page: Integer; Fill: TColor; Opacity: Byte;
    Graphics: TGPGraphics; Brush: TGPSolidBrush; Blend: TBlendFunction;
begin
  NavigationRects(Vertical, Horizontal, Map); Rows := VisualRows; Page := VisibleRowCount;
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if not IsRectEmpty(Vertical) then
      ERDPaintScrollBar(Painter, Vertical, True, 0, Max(0, Length(Rows) - 1), Page,
        FirstVisibleRow, FNavigationHot = 1, FNavigationDrag = 1);
    if not IsRectEmpty(Horizontal) then
      ERDPaintScrollBar(Painter, Horizontal, False, 0, Max(0, NavigationWidth - 1), TextViewport.Width,
        HorizontalScrollPosition, FNavigationHot = 2, FNavigationDrag = 2);
    if not IsRectEmpty(Map) then
    begin
      Fill := ERDMixColor(Palette.Background, Palette.ForegroundText, 0.96);
      RenderMinimap(Map.Width, Map.Height, Rows, Fill);
      Painter.FillRect(Map, Fill);
      Blend.BlendOp := AC_SRC_OVER; Blend.BlendFlags := 0;
      Blend.SourceConstantAlpha := 179; Blend.AlphaFormat := 0;
      if not Winapi.Windows.AlphaBlend(ACanvas.Handle, Map.Left, Map.Top, Map.Width, Map.Height,
        FMinimapBitmap.Canvas.Handle, 0, 0, Map.Width, Map.Height, Blend) then
        ACanvas.Draw(Map.Left, Map.Top, FMinimapBitmap);
      Viewport := MinimapViewportRect;
      if not IsRectEmpty(Viewport) then
      begin
        // Keep the viewed code at full strength; dim only the surrounding code.
        BitBlt(ACanvas.Handle, Viewport.Left, Viewport.Top, Viewport.Width, Viewport.Height,
          FMinimapBitmap.Canvas.Handle, 0, Viewport.Top - Map.Top, SRCCOPY);
        Opacity := 28;
        if FMinimapWindowHot then Opacity := 48;
        if FNavigationDrag = 3 then Opacity := 72;
        // Composite over the cached code strokes; the viewport has no outline.
        Graphics := TGPGraphics.Create(ACanvas.Handle);
        try
          Brush := TGPSolidBrush.Create(ColorToARGB(Palette.ForegroundText, Opacity));
          try Graphics.FillRectangle(Brush, Viewport.Left, Viewport.Top, Viewport.Width, Viewport.Height);
          finally Brush.Free; end;
        finally Graphics.Free; end;
      end;
      Painter.VLine(Map.Left, Map.Top, Map.Height, Palette.NeutralLight);
    end;
  finally Painter.Free; end;
end;
procedure TERDCodeEditor.RenderMinimap(Width, Height: Integer; const Rows: TArray<TERDTextRow>; Fill: TColor);
var I, J, Column, SpanIndex, SpanCursor, At, Y, LastY, PixelX, RunX, RunEnd, LineHeight, MapHeight: Integer;
    Row: TERDTextRow; Ink, RunInk, ColorKey: TColor; C: WideChar;
begin
  // Rebuild on document/fold/layout/style changes, not on selection or wheel movement.
  ColorKey := TColor(ColorToRGB(Palette.Accent) xor ColorToRGB(Palette.Subtle) xor ColorToRGB(Palette.ForegroundText));
  if not FMinimapDirty and (FMinimapBitmap.Width = Width) and (FMinimapBitmap.Height = Height) and
    (FMinimapFill = Fill) and (FMinimapColorKey = ColorKey) then Exit;
  FMinimapBitmap.SetSize(Width, Height); FMinimapBitmap.Canvas.Brush.Color := Fill;
  FMinimapBitmap.Canvas.FillRect(Rect(0, 0, Width, Height)); Analyze;
  SpanIndex := 0; LastY := -1; MapHeight := Min(Height, Length(Rows) * ScaleValue(3));
  LineHeight := Max(1, Min(ScaleValue(2), Height div Max(1, Length(Rows))));
  for I := 0 to High(Rows) do
  begin
    Y := MulDiv(I, MapHeight, Max(1, Length(Rows))); if Y = LastY then Continue; LastY := Y;
    Row := Rows[I]; Column := 0; RunX := -1; RunEnd := 0; RunInk := clNone;
    while (SpanIndex < Length(FAnalysis.Spans)) and
      (FAnalysis.Spans[SpanIndex].Start + FAnalysis.Spans[SpanIndex].Count <= Row.Start) do Inc(SpanIndex);
    SpanCursor := SpanIndex;
    for J := 0 to Min(Row.Count, Width * 2) - 1 do
    begin
      At := Row.Start + J; C := Text[At + 1];
      if C = #9 then begin Inc(Column, FTabWidth - Column mod FTabWidth); Continue; end;
      PixelX := ScaleValue(3) + Column; Inc(Column); if PixelX >= Width - ScaleValue(2) then Break;
      if C = ' ' then Continue;
      Ink := Palette.ForegroundText;
      while (SpanCursor < Length(FAnalysis.Spans)) and
        (FAnalysis.Spans[SpanCursor].Start + FAnalysis.Spans[SpanCursor].Count <= At) do Inc(SpanCursor);
      if (SpanCursor < Length(FAnalysis.Spans)) and (FAnalysis.Spans[SpanCursor].Start <= At) then
        case FAnalysis.Spans[SpanCursor].Kind of
          skKeyword, skInterfaceKeyword, skClassKeyword, skModuleKeyword:
            Ink := ResolveKeywordColor(Copy(Text, FAnalysis.Spans[SpanCursor].Start + 1,
              FAnalysis.Spans[SpanCursor].Count), FAnalysis.Spans[SpanCursor].Kind);
          skString: Ink := Palette.Success;
          skNumber: Ink := Palette.Warning;
          skComment: Ink := Palette.Subtle;
          skDirective: Ink := Palette.Accent;
        end;
      if (RunX >= 0) and ((Ink <> RunInk) or (PixelX <> RunEnd)) then
      begin
        FMinimapBitmap.Canvas.Brush.Color := RunInk;
        FMinimapBitmap.Canvas.FillRect(Rect(RunX, Y, RunEnd, Min(Height, Y + LineHeight))); RunX := -1;
      end;
      if RunX < 0 then begin RunX := PixelX; RunInk := Ink; end;
      RunEnd := PixelX + 1;
    end;
    if RunX >= 0 then
    begin
      FMinimapBitmap.Canvas.Brush.Color := RunInk;
      FMinimapBitmap.Canvas.FillRect(Rect(RunX, Y, RunEnd, Min(Height, Y + LineHeight)));
    end;
  end;
  FMinimapDirty := False; FMinimapFill := Fill; FMinimapColorKey := ColorKey;
end;
procedure TERDCodeEditor.ThemeChanged;
begin FMinimapDirty := True; inherited; end;
procedure TERDCodeEditor.SetShowLineNumbers(Value: Boolean);
begin
  if FShowLineNumbers = Value then Exit;
  FShowLineNumbers := Value; InvalidateTextLayout; UpdateCaret;
end;
procedure TERDCodeEditor.SetShowFolding(Value: Boolean);
begin
  if FShowFolding = Value then Exit;
  FShowFolding := Value; FHotFold := -1;
  if FPressedFold >= 0 then begin FPressedFold := -1; MouseCapture := False; end;
  if not Value then UnfoldAll;
  InvalidateTextLayout; UpdateCaret;
end;
function TERDCodeEditor.SetSearch(const Pattern: UnicodeString; Options: TERDCodeSearchOptions;
  ScopeStart, ScopeCount: Integer): Boolean;
begin
  FSearchPattern := Pattern; FSearchOptions := Options;
  FSearchScopeStart := EnsureRange(ScopeStart, 0, Length(Text));
  FSearchScopeCount := Max(-1, ScopeCount); RefreshSearch;
  Result := FSearchError = ''; Invalidate;
end;
procedure TERDCodeEditor.ClearSearch;
begin FSearchPattern := ''; FSearchMatches := nil; FSearchError := ''; FSearchIndex := -1; Invalidate; end;
function TERDCodeEditor.SearchMatches: TArray<TERDCodeSearchMatch>;
begin Result := Copy(FSearchMatches); end;
procedure TERDCodeEditor.RefreshSearch;
var Pattern: UnicodeString; Regex: TRegEx; Matches: TMatchCollection; Match: TMatch;
    Options: TRegExOptions; Item: TERDCodeSearchMatch; Found: TList<TERDCodeSearchMatch>;
    ScopeFinish: Integer;
begin
  FSearchMatches := nil; FSearchError := ''; FSearchIndex := -1;
  if FSearchPattern = '' then Exit;
  ScopeFinish := Length(Text);
  if FSearchScopeCount >= 0 then
    ScopeFinish := FSearchScopeStart + Min(FSearchScopeCount, Max(0, Length(Text) - FSearchScopeStart));
  Found := TList<TERDCodeSearchMatch>.Create;
  try
    try
      Pattern := FSearchPattern;
      if not (soRegex in FSearchOptions) then Pattern := TRegEx.Escape(Pattern);
      if soWholeWord in FSearchOptions then
        Pattern := '(?<![\p{L}\p{M}\p{N}_$])(?:' + Pattern + ')(?![\p{L}\p{M}\p{N}_$])';
      Options := [roMultiLine];
      if not (soMatchCase in FSearchOptions) then Include(Options, roIgnoreCase);
      Regex := TRegEx.Create(Pattern, Options); Matches := Regex.Matches(Text);
      for Match in Matches do
      begin
        // Delphi TMatch.Index is one-based. Public editor ranges are zero-based.
        Item.Start := Match.Index - 1; Item.Count := Match.Length;
        if (Item.Start < FSearchScopeStart) or (Item.Start + Item.Count > ScopeFinish) then Continue;
        if (TextModel.Snap(Item.Start) <> Item.Start) or
          (TextModel.Snap(Item.Start + Item.Count) <> Item.Start + Item.Count) then Continue;
        Found.Add(Item);
      end;
      FSearchMatches := Found.ToArray;
    except on E: Exception do FSearchError := E.Message; end;
  finally Found.Free; end;
end;
function TERDCodeEditor.FindNext(Backwards: Boolean): Boolean;
var I, Candidate: Integer; Item: TERDCodeSearchMatch;
begin
  Result := Length(FSearchMatches) > 0; if not Result then Exit;
  Candidate := -1;
  if (FSearchIndex >= 0) and (FSearchIndex < Length(FSearchMatches)) and
    (SelStart = FSearchMatches[FSearchIndex].Start) and (SelLength = FSearchMatches[FSearchIndex].Count) then
  begin
    if Backwards then Candidate := FSearchIndex - 1 else Candidate := FSearchIndex + 1;
    if Candidate < 0 then Candidate := High(FSearchMatches)
    else if Candidate > High(FSearchMatches) then Candidate := 0;
  end
  else if Backwards then
  begin
    for I := High(FSearchMatches) downto 0 do
      if FSearchMatches[I].Start < SelStart then begin Candidate := I; Break; end;
    if Candidate < 0 then Candidate := High(FSearchMatches);
  end
  else
  begin
    for I := 0 to High(FSearchMatches) do
      if FSearchMatches[I].Start >= SelStart + SelLength then begin Candidate := I; Break; end;
    if Candidate < 0 then Candidate := 0;
  end;
  Item := FSearchMatches[Candidate]; FSearchIndex := Candidate;
  TextModel.Select(Item.Start, Item.Start + Item.Count); Changed(Text); Invalidate;
end;
function TERDCodeEditor.ReplaceCurrent(const Replacement: UnicodeString): Boolean;
var Item: TERDCodeSearchMatch; Before, Value: UnicodeString; Start: Integer;
begin
  Result := False; if ReadOnly or (FSearchError <> '') then Exit;
  for Item in FSearchMatches do
    if (Item.Start = SelStart) and (Item.Count = SelLength) then
    begin
      Before := Text; Value := NormalizeInput(Replacement); Start := Item.Start;
      if (MaxLength > 0) and (Length(Text) - Item.Count + Length(Value) > MaxLength) then Exit;
      if FSearchScopeCount >= 0 then Inc(FSearchScopeCount, Length(Value) - Item.Count);
      TextModel.ReplaceRange(Start, Item.Count, Value); Changed(Before);
      Result := True; Exit;
    end;
end;
function TERDCodeEditor.ReplaceAll(const Replacement: UnicodeString): Integer;
var Item: TERDCodeSearchMatch; Before, Value: UnicodeString; Builder: TStringBuilder;
    At, Count: Integer;
begin
  Result := 0; if ReadOnly or (FSearchError <> '') or (Length(FSearchMatches) = 0) then Exit;
  Before := Text; Value := NormalizeInput(Replacement); Builder := TStringBuilder.Create;
  try
    At := 0; Count := Length(FSearchMatches);
    for Item in FSearchMatches do
    begin
      Builder.Append(Copy(Before, At + 1, Item.Start - At)); Builder.Append(Value);
      At := Item.Start + Item.Count;
    end;
    Builder.Append(Copy(Before, At + 1, Length(Before) - At)); Value := Builder.ToString;
    if (MaxLength > 0) and (Length(Value) > MaxLength) then Exit;
    if FSearchScopeCount >= 0 then Inc(FSearchScopeCount, Length(Value) - Length(Before));
    TextModel.ReplaceRange(0, Length(Before), Value); Changed(Before); Result := Count;
  finally Builder.Free; end;
end;
procedure TERDCodeEditor.PaintTextBackground(ACanvas: TCanvas; Start, Count, X, Y: Integer;
  Line: TERDTextLayoutLine);
var Item: TERDCodeSearchMatch; A, B, L, R, Index: Integer;
    Boundaries: TArray<Integer>;
begin
  inherited; Boundaries := TextModel.Boundaries;
  ACanvas.Brush.Color := ERDMixColor(Palette.Warning, Palette.Background, 0.22);
  for Item in FSearchMatches do
  begin
    if Item.Count = 0 then
    begin
      if (Item.Start >= Start) and (Item.Start <= Start + Count) then
      begin
        L := Line.X(Item.Start - Start);
        ACanvas.FillRect(Rect(X + L, Y, X + L + Max(1, ScaleValue(2)), Y + RowHeight));
      end;
      Continue;
    end;
    A := Max(Start, Item.Start); B := Min(Start + Count, Item.Start + Item.Count);
    if A >= B then Continue;
    // Individual grapheme rectangles preserve disjoint bidi visual ranges.
    for Index := 0 to High(Boundaries) - 1 do
      if (Boundaries[Index] >= A) and (Boundaries[Index + 1] <= B) then
      begin
        Line.Range(Boundaries[Index] - Start, Boundaries[Index + 1] - Boundaries[Index], L, R);
        ACanvas.FillRect(Rect(X + L, Y, X + R, Y + RowHeight));
      end;
  end;
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
  if (Candidate >= Length(Text)) or not CharInSet(Text[Candidate + 1], ['(',')','[',']','{','}']) then
    Dec(Candidate);
  if (Candidate < 0) or (Candidate >= Length(Text)) or
    not CharInSet(Text[Candidate + 1], ['(',')','[',']','{','}']) then Exit;
  Analyze; Stack := TList<Integer>.Create;
  try
    for I := 0 to Length(Text) - 1 do
    begin
      C := Text[I + 1]; if not CharInSet(C, ['(',')','[',']','{','}']) then Continue;
      // Token interiors and their opening character both belong to literals.
      Excluded := False;
      for Span in FAnalysis.Spans do
        if (Span.Kind in [skString, skComment, skDirective]) and
          (I >= Span.Start) and (I < Span.Start + Span.Count) then
        begin Excluded := True; Break; end;
      if Excluded then Continue;
      if CharInSet(C, ['(','[','{']) then Stack.Add(I)
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
    CharInSet(C, [')',']','}',#39,'"','`']) and (Text[Position + 1] = C) and
    (CharInSet(C, [#39,'"','`']) or not LiteralAt(Position)) then
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
  if CharInSet(C, [#39,'"','`']) and (Selected = 0) and (Position > 0) and
    (CharInSet(Text[Position], ['a'..'z','A'..'Z','0'..'9','_','$']) or (Ord(Text[Position]) >= $80)) then Exit;
  Value := C + SelText + Closing;
  Result := True;
  if (MaxLength > 0) and (Length(Text) - Selected + Length(Value) > MaxLength) then Exit;
  Before := Text; TextModel.ReplaceSelection(Value); Changed(Before);
  SelStart := Position + 1; SelLength := Selected;
end;
procedure TERDCodeEditor.SetInterfaceKeywordColor(Value: TColor);
begin FInterfaceKeywordColor := Value; FMinimapDirty := True; Invalidate; end;
procedure TERDCodeEditor.SetClassKeywordColor(Value: TColor);
begin FClassKeywordColor := Value; FMinimapDirty := True; Invalidate; end;
procedure TERDCodeEditor.SetModuleKeywordColor(Value: TColor);
begin FModuleKeywordColor := Value; FMinimapDirty := True; Invalidate; end;
procedure TERDCodeEditor.SetTabWidth(Value: Integer);
begin FTabWidth := EnsureRange(Value, 1, 16); FMinimapDirty := True; Invalidate; end;
function TERDCodeEditor.TextViewport: TRect;
var Vertical, Horizontal, Map: TRect;
begin
  Result := inherited TextViewport;
  if FShowLineNumbers then Inc(Result.Left, ScaleValue(48));
  if FShowFolding then Inc(Result.Left, ScaleValue(18));
  NavigationRects(Vertical, Horizontal, Map);
  if not IsRectEmpty(Vertical) then Result.Right := Min(Result.Right, Vertical.Left);
  if not IsRectEmpty(Map) then Result.Right := Min(Result.Right, Map.Left - ScaleValue(3));
  if not IsRectEmpty(Horizontal) then Result.Bottom := Min(Result.Bottom, Horizontal.Top);
  Result.Right := Max(Result.Left + 1, Result.Right);
  Result.Bottom := Max(Result.Top + 1, Result.Bottom);
end;
function TERDCodeEditor.LineStart(Position: Integer): Integer;
begin
  Result := EnsureRange(Position, 0, Length(Text));
  while (Result > 0) and (Text[Result] <> #10) do Dec(Result);
end;
function TERDCodeEditor.LineFinish(Position: Integer): Integer;
begin
  Result := EnsureRange(Position, 0, Length(Text));
  while (Result < Length(Text)) and (Text[Result + 1] <> #10) do Inc(Result);
  if Result < Length(Text) then Inc(Result);
end;
procedure TERDCodeEditor.SelectGutterLine(Y: Integer; Extend: Boolean);
var Rows: TArray<TERDTextRow>; Row, Start, Finish: Integer;
begin
  Rows := VisualRows; if Length(Rows) = 0 then Exit;
  if Y < TextViewport.Top then Row := FirstVisibleRow - 1
  else Row := FirstVisibleRow + (Y - TextViewport.Top) div RowHeight;
  Row := EnsureRange(Row, 0, High(Rows));
  Start := LineStart(Rows[Row].Start); Finish := LineFinish(Start);
  if not Extend then
  begin FLineAnchorStart := Start; FLineAnchorFinish := Finish; end;
  if Start < FLineAnchorStart then TextModel.Select(FLineAnchorFinish, Start)
  else TextModel.Select(FLineAnchorStart, Finish);
  UpdateCaret; Invalidate;
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;
procedure TERDCodeEditor.DblClick;
begin
  // VCL also dispatches DblClick for gutter clicks; avoid base word selection.
  if FLastGutterDown then
  begin if Assigned(OnDblClick) then OnDblClick(Self); Exit; end;
  inherited;
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
procedure TERDCodeEditor.SetShowFoldGuides(Value: Boolean);
begin
  if FShowFoldGuides = Value then Exit;
  FShowFoldGuides := Value; Invalidate;
end;
function TERDCodeEditor.GutterBounds: TRect;
var BorderWidth: Integer;
begin
  Result := ClientRect;
  if BorderStyle = bsSingle then
  begin
    BorderWidth := ScaleValue(1);
    if Focused and Enabled then BorderWidth := ScaleValue(2);
    InflateRect(Result, -BorderWidth, -BorderWidth);
  end;
  Result.Right := Max(Result.Left, Min(Result.Right, TextViewport.Left - ScaleValue(3)));
  if not (FShowLineNumbers or FShowFolding) then Result.Right := Result.Left;
end;
function TERDCodeEditor.FoldButtonRect(Row: Integer): TRect;
var View: TRect; CenterX, CenterY, Radius: Integer;
begin
  View := TextViewport;
  CenterX := View.Left - ScaleValue(13);
  CenterY := View.Top + (Row - FirstVisibleRow) * RowHeight + RowHeight div 2;
  Radius := Max(4, ScaleValue(5));
  Result := Rect(CenterX - Radius, CenterY - Radius, CenterX + Radius + 1, CenterY + Radius + 1);
end;
function TERDCodeEditor.FoldAtPoint(X, Y: Integer): Integer;
var Rows: TArray<TERDTextRow>; Row, Index: Integer; View: TRect;
begin
  Result := -1; if not FShowFolding or not Enabled then Exit;
  View := TextViewport;
  if (Y < View.Top) or (Y >= View.Bottom) or (X < View.Left - ScaleValue(20)) or (X >= View.Left) then Exit;
  Rows := VisualRows; Row := FirstVisibleRow + (Y - View.Top) div RowHeight;
  if (Row < 0) or (Row >= Length(Rows)) then Exit;
  Index := FoldIndex(Rows[Row].Start);
  if (Index >= 0) and PtInRect(FoldButtonRect(Row), Point(X, Y)) then
    Result := FAnalysis.Folds[Index].Start;
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
  FNavigationWidthDirty := True; FMinimapDirty := True;
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
var Fold: TERDSyntaxFold; Expanded, CaretMoved: Boolean;
begin
  if TextModel <> nil then
  begin
    CaretMoved := FTrackCaret and ((FLastCaret <> TextModel.Caret) or (FLastAnchor <> TextModel.Anchor));
    FLastCaret := TextModel.Caret; FLastAnchor := TextModel.Anchor; FTrackCaret := True;
    if CaretMoved then CancelLanguageRequests;
  end;
  if (FCompletionStamp.ID <> 0) and not SnapshotCurrent(FCompletionStamp) then
  begin
    FCompletionItems := nil; FCompletionStamp.ID := 0;
    if FCompletionPopup <> nil then FCompletionPopup.CloseUp(False);
  end;
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
    FLineAnchorFinish := 0;
    FHotFold := -1;
    if FPressedFold >= 0 then begin FPressedFold := -1; MouseCapture := False; end;
    Inc(FVersion); CancelLanguageRequests;
    // Editing invalidates structural offsets. Keep the text, expand stale folds.
    FDiagnostics := nil;
    RefreshSearch;
    FAnalysisValid := False;
    if FCollapsed <> nil then FCollapsed.Clear;
    InvalidateTextLayout;
  end;
  inherited Changed(ABefore);
  if ABefore <> Text then SyncLanguageDocument;
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
      skKeyword, skInterfaceKeyword, skClassKeyword, skModuleKeyword:
        Ink := ResolveKeywordColor(Copy(Text, Span.Start + 1, Span.Count), Span.Kind);
      skDirective: Ink := Palette.Accent;
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
var Item, ValidItem: TERDCodeDiagnostic; Valid: TList<TERDCodeDiagnostic>;
begin
  // An asynchronous validator may finish after the document changed.
  Result := SourceText = Text; if not Result then Exit;
  Valid := TList<TERDCodeDiagnostic>.Create;
  try
    for Item in Items do
      if (Item.Start >= 0) and (Item.Start <= Length(Text)) and (Item.Count >= 0) then
      begin
        ValidItem := Item;
        ValidItem.Count := Min(Item.Count, Length(Text) - Item.Start);
        Valid.Add(ValidItem);
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
    Opening, Closing, Column, Position, Index, Padding: Integer;
    Metrics: TTextMetric; Glyph: TGlyphMetrics; Matrix: TMat2; BracketRect: TRect;
begin
  inherited;
  Saved := SaveDC(ACanvas.Handle);
  try
    if FShowIndentGuides and (FTabWidth > 0) then
    begin
      // Keep guides inside leading whitespace, away from the first text glyph.
      B := Start;
      while (B < Start + Count) and CharInSet(Text[B + 1], [' ', #9]) do Inc(B);
      Position := Start; Column := 0;
      ACanvas.Pen.Style := psSolid; ACanvas.Pen.Width := 1;
      ACanvas.Pen.Color := ERDMixColor(Palette.Background, Palette.ForegroundText, 0.88);
      while Position < B do
      begin
        if Text[Position + 1] = #9 then Inc(Column, FTabWidth - Column mod FTabWidth)
        else Inc(Column);
        Inc(Position);
        if (Column mod FTabWidth = 0) and (Position < B) then
        begin
          L := X + Line.X(Position - Start);
          ACanvas.MoveTo(L, Y); ACanvas.LineTo(L, Y + RowHeight);
        end;
      end;
    end;
    if FShowBracketMatch and MatchingBrackets(Opening, Closing) then
      for Index := 0 to 1 do
      begin
        if Index = 0 then Position := Opening else Position := Closing;
        if (Position < Start) or (Position >= Start + Count) then Continue;
        Line.Range(Position - Start, 1, L, R);
        // DirectWrite uses this GDI font's ascent as its baseline. Frame the ink,
        // excluding font descent and selection padding below the visible bracket.
        FillChar(Metrics, SizeOf(Metrics), 0); GetTextMetrics(ACanvas.Handle, Metrics);
        FillChar(Glyph, SizeOf(Glyph), 0);
        FillChar(Matrix, SizeOf(Matrix), 0); Matrix.eM11.value := 1; Matrix.eM22.value := 1;
        if (GetGlyphOutlineW(ACanvas.Handle, Ord(Text[Position + 1]), GGO_METRICS,
          Glyph, 0, nil, Matrix) <> DWORD(GDI_ERROR)) and (Glyph.gmBlackBoxY > 0) then
        begin
          BracketRect.Left := X + L + Glyph.gmptGlyphOrigin.X;
          BracketRect.Top := Y + ScaleValue(SelectionPadding) + Metrics.tmAscent - Glyph.gmptGlyphOrigin.Y;
          BracketRect.Right := BracketRect.Left + Integer(Glyph.gmBlackBoxX);
          BracketRect.Bottom := BracketRect.Top + Integer(Glyph.gmBlackBoxY);
        end
        else BracketRect := Rect(X + L, Y + ScaleValue(SelectionPadding) + Metrics.tmInternalLeading,
          X + Max(L + 1, R), Y + ScaleValue(SelectionPadding) + Metrics.tmAscent);
        Padding := Max(1, ScaleValue(2)); InflateRect(BracketRect, Padding, Padding);
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Style := psSolid; ACanvas.Pen.Width := 1;
        ACanvas.Pen.Color := Palette.Accent;
        ACanvas.Rectangle(BracketRect);
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
    R, View, ButtonRect: TRect; Saved, CenterX, CenterY, Arm: Integer; Diagnostic: TERDCodeDiagnostic;
    Fold: TERDSyntaxFold; Painter: TERDPainter; GutterFill, GuideInk, ButtonInk, ButtonFill, ButtonBorder: TColor;
    DiagnosticFound: Boolean; RowFinish: Integer; Severity: TERDDiagnosticSeverity;
begin
  inherited PaintControl(ACanvas);
  PaintNavigation(ACanvas);
  if not (FShowLineNumbers or FShowFolding) then Exit;
  Rows := VisualRows; View := TextViewport; H := RowHeight;
  R := GutterBounds;
  Saved := SaveDC(ACanvas.Handle);
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    GutterFill := ERDMixColor(Palette.Background, Palette.ForegroundText, 0.92);
    GuideInk := ERDMixColor(GutterFill, Palette.ForegroundText, 0.60);
    IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    Painter.FillRect(R, GutterFill);
    Painter.VLine(R.Right - 1, R.Top, R.Height, ERDMixColor(GutterFill, Palette.ForegroundText, 0.75));
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
        ACanvas.TextOut(X - ScaleValue(6) - ACanvas.TextWidth(IntToStr(Number)),
          Y + ScaleValue(SelectionPadding), IntToStr(Number));
      if FShowFolding then
      begin
        ButtonRect := FoldButtonRect(I);
        CenterX := (ButtonRect.Left + ButtonRect.Right) div 2;
        CenterY := Y + H div 2; Arm := Max(2, ScaleValue(3));
        ACanvas.Pen.Color := Palette.Subtle;
        if FShowFoldGuides then
          for Fold in FAnalysis.Folds do
            if not FCollapsed.Contains(Fold.Start) and
              (Rows[I].Start > LineStart(Fold.Start)) and (Rows[I].Start < FoldHiddenFinish(Fold)) then
            begin
              if (I = High(Rows)) or (Rows[I + 1].Start >= FoldHiddenFinish(Fold)) then
              begin
                Painter.VLine(CenterX, Y, H div 2 + 1, GuideInk);
                Painter.HLine(CenterX, CenterY, ButtonRect.Right + ScaleValue(2) - CenterX, GuideInk);
              end
              else Painter.VLine(CenterX, Y, H, GuideInk);
            end;
        Index := FoldIndex(Rows[I].Start);
        if Index >= 0 then
        begin
          if FShowFoldGuides and not FCollapsed.Contains(FAnalysis.Folds[Index].Start) then
            Painter.VLine(CenterX, CenterY, H - H div 2, GuideInk);
          ButtonFill := GutterFill;
          ButtonInk := ERDMixColor(GutterFill, Palette.ForegroundText, 0.20);
          ButtonBorder := ERDMixColor(GutterFill, Palette.ForegroundText, 0.65);
          if (FHotFold = FAnalysis.Folds[Index].Start) and Enabled then
          begin
            ButtonBorder := ERDMixColor(GutterFill, Palette.Accent, 0.35);
            ButtonFill := ERDMixColor(GutterFill, Palette.Accent, 0.88);
            if FPressedFold = FHotFold then
              ButtonFill := ERDMixColor(GutterFill, Palette.Accent, 0.70);
          end;
          Painter.RoundRect(ButtonRect.Left, ButtonRect.Top, ButtonRect.Width, ButtonRect.Height,
            ScaleValue(3), ButtonFill, ButtonBorder);
          Painter.HLine(CenterX - Arm, CenterY, Arm * 2 + 1, ButtonInk);
          if FCollapsed.Contains(FAnalysis.Folds[Index].Start) then
            Painter.VLine(CenterX, CenterY - Arm, Arm * 2 + 1, ButtonInk);
        end;
      end;
    end;
  finally Painter.Free; RestoreDC(ACanvas.Handle, Saved); end;
end;
procedure TERDCodeEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Fold: Integer; View, OuterView: TRect;
begin
  if (Button = mbLeft) and NavigationMouseDown(X, Y) then Exit;
  View := TextViewport; OuterView := GutterBounds; Fold := FoldAtPoint(X, Y);
  FLastGutterDown := (Button = mbLeft) and not IsRectEmpty(OuterView) and (X >= OuterView.Left) and (X < View.Left) and
    (Y >= OuterView.Top) and (Y < OuterView.Bottom);
  if (Button = mbLeft) and (Fold >= 0) then
  begin
    SetFocus; FPressedFold := Fold; FHotFold := Fold; MouseCapture := True;
    Invalidate; Exit;
  end;
  if FLastGutterDown then
  begin
    SetFocus;
    if (ssShift in Shift) and not ((FLineAnchorFinish > FLineAnchorStart) and
      ((TextModel.Anchor = FLineAnchorStart) or (TextModel.Anchor = FLineAnchorFinish))) then
    begin
      FLineAnchorStart := LineStart(TextModel.Anchor);
      FLineAnchorFinish := LineFinish(FLineAnchorStart);
    end;
    SelectGutterLine(Y, ssShift in Shift);
    FSelectingLines := True; MouseCapture := True; Exit;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;
procedure TERDCodeEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var Fold, Hit: Integer; View, OuterView: TRect; WindowHot: Boolean;
begin
  Hit := NavigationAt(X, Y);
  WindowHot := (Hit = 3) and PtInRect(MinimapViewportRect, Point(X, Y));
  if FMinimapWindowHot <> WindowHot then begin FMinimapWindowHot := WindowHot; Invalidate; end;
  if FNavigationHot <> Hit then begin FNavigationHot := Hit; Invalidate; end;
  if FNavigationDrag <> 0 then begin NavigatePointer(X, Y); Exit; end;
  if Hit <> 0 then begin Cursor := crArrow; Exit; end;
  Fold := FoldAtPoint(X, Y);
  if FHotFold <> Fold then begin FHotFold := Fold; Invalidate; end;
  View := TextViewport; OuterView := GutterBounds;
  if not IsRectEmpty(OuterView) and (X < View.Left) and (X >= OuterView.Left) then Cursor := crArrow
  else Cursor := crIBeam;
  if FPressedFold >= 0 then Exit;
  if FSelectingLines then
  begin if MouseCapture then SelectGutterLine(Y, True); Exit; end;
  inherited MouseMove(Shift, X, Y);
end;
procedure TERDCodeEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Fold: Integer;
begin
  if (Button = mbLeft) and (FNavigationDrag <> 0) then
  begin
    FNavigationDrag := 0; MouseCapture := False;
    FMinimapWindowHot := PtInRect(MinimapViewportRect, Point(X, Y)); Invalidate; Exit;
  end;
  if (Button = mbLeft) and FSelectingLines then
  begin FSelectingLines := False; MouseCapture := False; Exit; end;
  if (Button = mbLeft) and (FPressedFold >= 0) then
  begin
    Fold := FPressedFold; FPressedFold := -1; MouseCapture := False;
    FHotFold := FoldAtPoint(X, Y);
    if FHotFold = Fold then ToggleFold(Fold);
    Invalidate; Exit;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;
procedure TERDCodeEditor.WMSetCursor(var Message: TWMSetCursor);
var P: TPoint; View, OuterView: TRect;
begin
  if (Message.HitTest = HTCLIENT) and GetCursorPos(P) then
  begin
    P := ScreenToClient(P); View := TextViewport; OuterView := GutterBounds;
    if (NavigationAt(P.X, P.Y) <> 0) or
      (not IsRectEmpty(OuterView) and (P.X >= OuterView.Left) and (P.X < View.Left)) then
      Winapi.Windows.SetCursor(LoadCursor(0, IDC_ARROW))
    else Winapi.Windows.SetCursor(LoadCursor(0, IDC_IBEAM));
    Message.Result := 1; Exit;
  end;
  inherited;
end;
procedure TERDCodeEditor.CMMouseLeave(var Message: TMessage);
begin
  inherited; FHotFold := -1; FNavigationHot := 0; FMinimapWindowHot := False; Cursor := crIBeam; Invalidate;
end;
procedure TERDCodeEditor.WMLButtonUp(var Message: TWMLButtonUp);
begin
  // VCL releases capture before dispatching the virtual MouseUp callback.
  FHandlingFoldRelease := True;
  try inherited; finally FHandlingFoldRelease := False; end;
end;
procedure TERDCodeEditor.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  if not FHandlingFoldRelease then
  begin FPressedFold := -1; FSelectingLines := False; FNavigationDrag := 0; end;
  Invalidate;
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
procedure TERDCodeEditor.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  // Keep completion's space and popup navigation out of form dialog-key routing.
  if Message.WParam = VK_SPACE then Message.Result := Message.Result or DLGC_WANTMESSAGE;
  if (FCompletionPopup <> nil) and FCompletionPopup.IsOpen and
    (Message.WParam in [VK_RETURN, VK_TAB, VK_ESCAPE]) then
    Message.Result := Message.Result or DLGC_WANTMESSAGE;
end;
procedure TERDCodeEditor.CNKeyDown(var Message: TWMKeyDown);
var Key: Word;
begin
  // VCL asks CN_KEYDOWN before offering the key to form shortcuts/dialogs.
  if (Message.CharCode = VK_SPACE) and (GetKeyState(VK_CONTROL) < 0) and
    (GetKeyState(VK_MENU) >= 0) then
  begin
    Key := VK_SPACE; KeyDown(Key, [ssCtrl]); Message.Result := 1; Exit;
  end;
  inherited;
end;
procedure TERDCodeEditor.CMDialogKey(var Message: TCMDialogKey);
var Key: Word;
begin
  if Focused and (Message.CharCode = VK_SPACE) and (GetKeyState(VK_CONTROL) < 0) and
    (GetKeyState(VK_MENU) >= 0) then
  begin
    Key := VK_SPACE; KeyDown(Key, [ssCtrl]); Message.Result := 1; Exit;
  end;
  inherited;
end;
procedure TERDCodeEditor.KeyDown(var Key: Word; Shift: TShiftState);
var Before: UnicodeString; Position, Spaces, Start: Integer;
begin
  FSuppressLanguageChar := 0;
  if (Key = VK_ESCAPE) and (FPending[lrCompletion] <> 0) then
  begin CancelLanguageRequests; FSuppressLanguageChar := 27; Key := 0; Exit; end;
  if (FCompletionPopup <> nil) and FCompletionPopup.IsOpen then
  begin
    if (Key = VK_TAB) and (Shift = []) then
    begin FSuppressLanguageChar := 9; FCompletionPopup.CloseUp(True); Key := 0; Exit; end;
    if Key in [VK_RETURN, VK_TAB, VK_ESCAPE] then
      case Key of VK_RETURN: FSuppressLanguageChar := 13; VK_TAB: FSuppressLanguageChar := 9; VK_ESCAPE: FSuppressLanguageChar := 27; end;
    if FCompletionPopup.HandleKey(Key, Shift) then Exit;
  end;
  if ((Key = VK_SPACE) and (ssCtrl in Shift) and not (ssAlt in Shift)) or
    ((FLanguageService <> nil) and (Key in [VK_F1, VK_F12]) and (Shift = [])) then
  begin
    if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift); if Key = 0 then Exit;
    case Key of
      VK_SPACE: begin FSuppressLanguageChar := 32; RequestCompletion; end;
      VK_F1: RequestHover;
      VK_F12: RequestDefinition;
    end;
    Key := 0; Exit;
  end;
  if ((Key = VK_F3) and not (ssCtrl in Shift) and not (ssAlt in Shift)) or
    ((ssCtrl in Shift) and not (ssAlt in Shift) and Assigned(FOnSearchRequested) and
      (Key in [Ord('F'), Ord('H')])) then
  begin
    if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift);
    if Key = 0 then Exit;
    if Key = VK_F3 then FindNext(ssShift in Shift)
    else if Assigned(FOnSearchRequested) and (Key in [Ord('F'), Ord('H')]) then FOnSearchRequested(Self, Key = Ord('H'));
    Key := 0; Exit;
  end;
  if FSmartHome and (Key = VK_HOME) and not (ssCtrl in Shift) and not (ssAlt in Shift) then
  begin
    if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift);
    if Key = 0 then Exit;
    if Key = VK_HOME then
    begin
      Start := LineStart(TextModel.Caret); Position := Start;
      while (Position < Length(Text)) and CharInSet(Text[Position + 1], [' ', #9]) do Inc(Position);
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
    Closing: Char; Span: TERDSyntaxSpan; Suppress, ContinueCompletion: Boolean; WordStart: Integer;
begin
  if (FSuppressLanguageChar <> 0) and (Message.CharCode = FSuppressLanguageChar) then
  begin FSuppressLanguageChar := 0; Exit; end;
  ContinueCompletion := (FLanguageService <> nil) and not ReadOnly and
    ((FPending[lrCompletion] <> 0) or (FCompletionStamp.ID <> 0));
  WordStart := FCompletionWordStart; Before := Text;
  if Message.CharCode = 9 then
  begin
    if FTabHandled then begin FTabHandled := False; Exit; end;
    IndentSelection(False); Exit;
  end;
  if not ReadOnly and FAutoCloseDelimiters and
    (((Message.CharCode = 8) and (GetKeyState(VK_CONTROL) >= 0)) or CharInSet(Char(Message.CharCode), ['(',')','[',']','{','}',#39,'"','`'])) then
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
    if ContinueCompletion and (C = #8) and (Text <> Before) and (TextModel.Caret >= WordStart) then RequestCompletion;
    Exit;
  end;
  if (Message.CharCode = 13) and not ReadOnly and WantReturns then
  begin
    C := #13; KeyPress(C); if C = #0 then Exit;
    if C <> #13 then begin ReplaceSelection(C); Exit; end;
    Position := SelStart; Start := LineStart(Position); Prefix := '';
    while (Start < Position) and CharInSet(Text[Start + 1], [' ', #9]) do
    begin Prefix := Prefix + Text[Start + 1]; Inc(Start); end;
    Value := #13#10 + Prefix;
    if (SelLength = 0) and (Position > 0) and not LiteralAt(Position) and
      CharInSet(Text[Position], ['{','[','(']) then
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
  if (Message.CharCode = Ord('.')) and FAutoMemberCompletion and
    (FLanguage = clTypeScript) and not ReadOnly and (FLanguageService <> nil) then
  begin
    Before := Text; Start := SelStart; Value := SelText; inherited;
    Position := TextModel.Caret;
    if (Before = Text) or (Position <> Start + 1) or
      (Length(Text) <> Length(Before) - Length(Value) + 1) or (Text[Position] <> '.') then Exit;
    Analyze; Suppress := False;
    for Span in FAnalysis.Spans do
      if (Span.Kind in [skComment, skString, skDirective]) and
        (Position - 1 >= Span.Start) and (Position - 1 < Span.Start + Span.Count) then
      begin Suppress := True; Break; end;
    if (Position > 1) and (Text[Position - 1] = '.') then Suppress := True;
    if not Suppress then IssueRequest(lrCompletion, Position, '.');
    Exit;
  end;
  inherited;
  Position := TextModel.Caret;
  if ContinueCompletion and (Before <> Text) and (Position >= WordStart) and (Position > 0) and
    (Text[Position].IsLetterOrDigit or CharInSet(Text[Position], ['_', '$'])) then RequestCompletion;
end;
end.
