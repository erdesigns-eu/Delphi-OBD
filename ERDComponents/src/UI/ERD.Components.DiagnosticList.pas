unit ERD.Components.DiagnosticList;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  Winapi.Windows, Vcl.Controls, Vcl.Graphics, ERD.Components.VirtualTree,
  ERD.Components.VT, ERD.Components.VT.Types, ERD.Components.VT.Header,
  ERD.Components.VT.BaseTree, ERD.Components.Types, ERD.Components.Paint,
  ERD.Components.Hint;
type
  TERDDiagnosticSeverity = (dgsError, dgsWarning, dgsInformation, dgsHint);
  TERDDiagnosticSeverities = set of TERDDiagnosticSeverity;
  TERDDiagnostic = record
    ID, MessageText, Code, Origin, DocumentUri: string;
    Severity: TERDDiagnosticSeverity;
    // One-based UTF-16 coordinates; zero means unspecified. End is exclusive.
    Line, Column, EndLine, EndColumn: Integer;
  end;
  TERDDiagnosticRequest = record
    Session, Generation, RequestID: Int64;
  end;
  TERDDiagnosticNavigateEvent = procedure(Sender: TObject; const Context: TERDDiagnosticRequest;
    const Diagnostic: TERDDiagnostic) of object;
  /// Presentation only. Hosts supply snapshots and handle source navigation.
  TERDDiagnosticList = class(TERDVirtualStringTree, IERDHintProvider)
  private
    FCellIconLeft: Integer;
    FItems: TArray<TERDDiagnostic>;
    FVisible: TArray<Integer>;
    FGroupUris: TList<string>;
    FGroupCounts: TList<Integer>;
    FNodes: TDictionary<string,PVirtualNode>;
    FCollapsed: TDictionary<string,Boolean>;
    FRequest, FPublished: TERDDiagnosticRequest;
    FNextRequest, FLastSelectionRequest: Int64;
    FSearchText, FErrorText, FLastSelection: string;
    FSeverities: TERDDiagnosticSeverities;
    FGroupByFile, FPending, FStale, FUpdating: Boolean;
    FOnNavigate: TERDDiagnosticNavigateEvent;
    FOnChange, FOnSelectionChanged: TNotifyEvent;
    procedure SetSearchText(const Value: string);
    procedure SetSeverities(Value: TERDDiagnosticSeverities);
    procedure SetGroupByFile(Value: Boolean);
    function IndexFor(Node: PVirtualNode): Integer;
    function ItemFor(Node: PVirtualNode; out Value: TERDDiagnostic): Boolean;
    function RequestMatches(const Request: TERDDiagnosticRequest): Boolean;
    procedure CaptureGroups;
    procedure PruneGroups;
    procedure RebuildVisible(const PreferredID: string = '');
    procedure Changed;
    procedure SelectionChanged;
    function SeverityInk(Value: TERDDiagnosticSeverity): TColor;
  protected
    procedure DoGetText(var E: TVSTGetCellTextEventArgs); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BeginSnapshot(Session, Generation: Int64): TERDDiagnosticRequest;
    function PublishSnapshot(const Request: TERDDiagnosticRequest; const Values: TArray<TERDDiagnostic>): Boolean;
    function FailSnapshot(const Request: TERDDiagnosticRequest; const MessageText: string): Boolean;
    procedure MarkStale;
    procedure ClearDiagnostics;
    function DiagnosticCount: Integer;
    function VisibleDiagnosticCount: Integer;
    function SeverityCount(Severity: TERDDiagnosticSeverity; VisibleOnly: Boolean = False): Integer;
    function Diagnostic(Index: Integer): TERDDiagnostic;
    function SelectedDiagnostic(out Value: TERDDiagnostic): Boolean;
    function SelectDiagnostic(const ID: string): Boolean;
    procedure NavigateSelected;
    function NavigateNext(Backwards: Boolean = False): Boolean;
    function ExportText(VisibleOnly: Boolean = True): string;
    procedure CopySelectedDiagnostic;
    procedure CopyVisibleDiagnostics;
    function HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
    property Context: TERDDiagnosticRequest read FPublished;
    property Pending: Boolean read FPending;
    property Stale: Boolean read FStale;
    property ErrorText: string read FErrorText;
  published
    property RootNodeCount stored False;
    property Header stored False;
    property SearchText: string read FSearchText write SetSearchText stored False;
    property Severities: TERDDiagnosticSeverities read FSeverities write SetSeverities default [dgsError,dgsWarning,dgsInformation,dgsHint];
    property GroupByFile: Boolean read FGroupByFile write SetGroupByFile default True;
    property OnNavigate: TERDDiagnosticNavigateEvent read FOnNavigate write FOnNavigate;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;
function ERDDiagnosticSeverityName(Value: TERDDiagnosticSeverity): string;
implementation
uses System.StrUtils, Vcl.Clipbrd, Vcl.Forms;
function ERDDiagnosticSeverityName(Value: TERDDiagnosticSeverity): string;
begin
  case Value of dgsError: Result := 'Error'; dgsWarning: Result := 'Warning';
    dgsInformation: Result := 'Information'; dgsHint: Result := 'Hint';
  else Result := 'Unknown'; end;
end;
function DiagnosticGlyph(Value: TERDDiagnosticSeverity): TERDGlyph;
begin
  case Value of dgsError: Result := glClose; dgsWarning: Result := glAlert;
    dgsHint: Result := glHelp; else Result := glInfo; end;
end;
function LocationText(const Value: TERDDiagnostic): string;
begin
  Result := Value.DocumentUri;
  if Value.Line > 0 then begin Result := Result+':'+IntToStr(Value.Line);
    if Value.Column > 0 then Result := Result+':'+IntToStr(Value.Column); end;
end;
function DiagnosticText(const Value: TERDDiagnostic): string;
begin
  Result := ERDDiagnosticSeverityName(Value.Severity)+#9+Value.Code+#9+Value.MessageText+#9+LocationText(Value)+#9+Value.Origin;
end;
constructor TERDDiagnosticList.Create(AOwner: TComponent);
var I: Integer;
const Titles: array[0..6] of string = ('Message','Severity','Code','Document','Line','Column','Origin');
      Widths: array[0..6] of Integer = (420,130,100,280,70,70,130);
begin
  inherited; Width := 720; Height := 260; NodeDataSize := SizeOf(Integer);
  FGroupUris := TList<string>.Create; FGroupCounts := TList<Integer>.Create;
  FNodes := TDictionary<string,PVirtualNode>.Create; FCollapsed := TDictionary<string,Boolean>.Create;
  FGroupByFile := True; FStale := True; FRequest.Generation := -1;
  FSeverities := [dgsError,dgsWarning,dgsInformation,dgsHint];
  Header.Options := Header.Options+[hoVisible,hoColumnResize]; Header.Columns.Clear;
  for I := 0 to High(Titles) do with Header.Columns.Add do begin Text := Titles[I]; Width := Widths[I]; end;
  TreeOptions.SelectionOptions := (TreeOptions.SelectionOptions+[toFullRowSelect])-[toMultiSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions-[toEditable];
  TreeOptions.AutoOptions := TreeOptions.AutoOptions-[toAutoFreeOnCollapse];
  HintMode := hmDefault; ShowHint := True; Changed;
end;
destructor TERDDiagnosticList.Destroy;
begin
  FUpdating := True; FOnChange := nil; FOnSelectionChanged := nil; FOnNavigate := nil;
  Clear; FNodes.Free; FCollapsed.Free; FGroupCounts.Free; FGroupUris.Free; inherited;
end;
function TERDDiagnosticList.IndexFor(Node: PVirtualNode): Integer;
var Data: PInteger;
begin
  Result := -MaxInt; if Node = nil then Exit;
  Data := GetNodeData(Node); if Data <> nil then Result := Data^;
end;
function TERDDiagnosticList.ItemFor(Node: PVirtualNode; out Value: TERDDiagnostic): Boolean;
var Index: Integer;
begin
  Value := Default(TERDDiagnostic); Index := IndexFor(Node);
  Result := (Index >= 0) and (Index < Length(FItems)); if Result then Value := FItems[Index];
end;
function TERDDiagnosticList.RequestMatches(const Request: TERDDiagnosticRequest): Boolean;
begin
  Result := FPending and (Request.RequestID > 0) and (Request.RequestID = FRequest.RequestID) and
    (Request.Session = FRequest.Session) and (Request.Generation = FRequest.Generation);
end;
function TERDDiagnosticList.BeginSnapshot(Session, Generation: Int64): TERDDiagnosticRequest;
begin
  Result := Default(TERDDiagnosticRequest);
  if (Session < 0) or (Generation < 0) or ((Session = FRequest.Session) and (Generation <= FRequest.Generation)) then Exit;
  Inc(FNextRequest); FRequest.Session := Session; FRequest.Generation := Generation; FRequest.RequestID := FNextRequest;
  FPending := True; FStale := True; FErrorText := ''; Result := FRequest; Changed;
end;
function TERDDiagnosticList.PublishSnapshot(const Request: TERDDiagnosticRequest; const Values: TArray<TERDDiagnostic>): Boolean;
var Seen: TDictionary<string,Boolean>; Value: TERDDiagnostic; SavedID: string;
begin
  Result := False; if not RequestMatches(Request) or (Length(Values) > 100000) then Exit;
  Seen := TDictionary<string,Boolean>.Create;
  try
    for Value in Values do
    begin
      if (Trim(Value.ID) = '') or Seen.ContainsKey(Value.ID) or (Trim(Value.MessageText) = '') or
        (Ord(Value.Severity) < Ord(Low(TERDDiagnosticSeverity))) or (Ord(Value.Severity) > Ord(High(TERDDiagnosticSeverity))) or
        (Value.Line < 0) or (Value.Column < 0) or (Value.EndLine < 0) or (Value.EndColumn < 0) then Exit;
      if ((Value.Column > 0) and (Value.Line = 0)) or ((Value.Line > 0) and (Trim(Value.DocumentUri) = '')) or
        ((Value.EndLine > 0) and ((Value.Line = 0) or (Value.EndLine < Value.Line))) or
        ((Value.EndColumn > 0) and (Value.EndLine = 0)) or
        ((Value.EndLine = Value.Line) and (Value.EndColumn > 0) and (Value.Column > 0) and (Value.EndColumn < Value.Column)) then Exit;
      Seen.Add(Value.ID,True);
    end;
  finally Seen.Free; end;
  SavedID := ''; if (FPublished.Session = Request.Session) and ItemFor(FocusedNode,Value) then SavedID := Value.ID;
  FUpdating := True; BeginUpdate;
  try
    CaptureGroups; Clear; FNodes.Clear;
    if FPublished.Session <> Request.Session then FCollapsed.Clear;
    FItems := Copy(Values); PruneGroups; FPublished := Request;
    FPending := False; FStale := False; FErrorText := ''; RebuildVisible(SavedID);
  finally EndUpdate; FUpdating := False; end;
  Result := True; Changed;
end;
function TERDDiagnosticList.FailSnapshot(const Request: TERDDiagnosticRequest; const MessageText: string): Boolean;
begin
  Result := RequestMatches(Request); if not Result then Exit;
  FPending := False; FStale := True; FRequest.RequestID := 0; FErrorText := MessageText;
  if FErrorText = '' then FErrorText := 'Diagnostics unavailable'; Changed;
end;
procedure TERDDiagnosticList.MarkStale;
begin FPending := False; FStale := True; FRequest.RequestID := 0; FErrorText := ''; Changed; end;
procedure TERDDiagnosticList.ClearDiagnostics;
begin
  FUpdating := True; BeginUpdate;
  try
    Clear; FItems := nil; FVisible := nil; FNodes.Clear; FGroupUris.Clear; FGroupCounts.Clear; FCollapsed.Clear;
    FPublished := Default(TERDDiagnosticRequest); FPending := False; FStale := True; FErrorText := ''; FRequest.RequestID := 0;
  finally EndUpdate; FUpdating := False; end; Changed;
end;
procedure TERDDiagnosticList.PruneGroups;
var Uris: TDictionary<string,Boolean>; Value: TERDDiagnostic; Uri: string;
begin
  Uris := TDictionary<string,Boolean>.Create;
  try
    for Value in FItems do Uris.AddOrSetValue(Value.DocumentUri,True);
    for Uri in FCollapsed.Keys.ToArray do if not Uris.ContainsKey(Uri) then FCollapsed.Remove(Uri);
  finally Uris.Free; end;
end;
procedure TERDDiagnosticList.CaptureGroups;
var Node: PVirtualNode; Index: Integer;
begin
  Node := GetFirst;
  while Node <> nil do
  begin
    Index := -IndexFor(Node)-1;
    if (Index >= 0) and (Index < FGroupUris.Count) then FCollapsed.AddOrSetValue(FGroupUris[Index],not Expanded[Node]);
    Node := GetNextSibling(Node);
  end;
end;
procedure TERDDiagnosticList.RebuildVisible(const PreferredID: string);
var SavedID, Word, Haystack: string; Words: TArray<string>; I, N, Group: Integer;
    Match, WasUpdating, Collapsed: Boolean; Node, Parent: PVirtualNode;
    Groups: TDictionary<string,PVirtualNode>; Value: TERDDiagnostic;
begin
  SavedID := PreferredID; if (SavedID = '') and ItemFor(FocusedNode,Value) then SavedID := Value.ID;
  WasUpdating := FUpdating; FUpdating := True; BeginUpdate;
  Groups := TDictionary<string,PVirtualNode>.Create;
  try
    CaptureGroups; Clear; FNodes.Clear; FGroupUris.Clear; FGroupCounts.Clear;
    SetLength(FVisible,Length(FItems)); N := 0;
    Words := Trim(FSearchText).Split([' ',#9],TStringSplitOptions.ExcludeEmpty);
    for I := 0 to High(FItems) do
    begin
      Value := FItems[I]; if not (Value.Severity in FSeverities) then Continue;
      Haystack := Value.MessageText+' '+Value.Code+' '+Value.Origin+' '+LocationText(Value)+' '+ERDDiagnosticSeverityName(Value.Severity);
      Match := True; for Word in Words do if not ContainsText(Haystack,Word) then begin Match := False; Break; end;
      if not Match then Continue;
      FVisible[N] := I; Inc(N); Parent := nil;
      if FGroupByFile then
      begin
        if not Groups.TryGetValue(Value.DocumentUri,Parent) then
        begin
          Parent := AddChild(nil); Group := FGroupUris.Count;
          FGroupUris.Add(Value.DocumentUri); FGroupCounts.Add(0); PInteger(GetNodeData(Parent))^ := -Group-1;
          Groups.Add(Value.DocumentUri,Parent);
        end;
        Group := -IndexFor(Parent)-1; FGroupCounts[Group] := FGroupCounts[Group]+1;
      end;
      Node := AddChild(Parent); PInteger(GetNodeData(Node))^ := I; FNodes.Add(Value.ID,Node);
    end;
    SetLength(FVisible,N); Node := GetFirst;
    while Node <> nil do
    begin
      Group := -IndexFor(Node)-1;
      if (Group >= 0) and (Group < FGroupUris.Count) then
      begin
        Collapsed := False; FCollapsed.TryGetValue(FGroupUris[Group],Collapsed); Expanded[Node] := not Collapsed;
      end;
      Node := GetNextSibling(Node);
    end;
    // Keyboard/export order follows the tree, including collapsed children.
    N := 0; Node := GetFirst;
    while Node <> nil do
    begin
      I := IndexFor(Node); if I >= 0 then begin FVisible[N] := I; Inc(N); end;
      Node := GetNext(Node);
    end;
    if SavedID <> '' then SelectDiagnostic(SavedID);
    Application.CancelHint; Hint := '';
  finally Groups.Free; EndUpdate; FUpdating := WasUpdating; end;
end;
procedure TERDDiagnosticList.Changed;
var Text: string;
begin
  if FUpdating or (csDestroying in ComponentState) then Exit;
  Text := 'Messages ('+IntToStr(Length(FVisible))+'/'+IntToStr(Length(FItems))+')';
  if FPending then Text := Text+' - loading'
  else if FErrorText <> '' then Text := Text+' - '+FErrorText
  else if FStale then Text := Text+' - stale'
  else if Length(FItems) = 0 then Text := 'No diagnostics'
  else if Length(FVisible) = 0 then Text := 'No matching diagnostics';
  if Header.Columns.Count > 0 then Header.Columns[0].Text := Text;
  Application.CancelHint; Hint := ''; Invalidate; SelectionChanged;
  if Assigned(FOnChange) then FOnChange(Self);
end;
procedure TERDDiagnosticList.SelectionChanged;
var Value: TERDDiagnostic; ID: string;
begin
  if FUpdating then Exit;
  ID := ''; if SelectedDiagnostic(Value) then ID := Value.ID;
  if (ID = FLastSelection) and ((ID = '') or (FLastSelectionRequest = FPublished.RequestID)) then Exit;
  FLastSelection := ID; FLastSelectionRequest := FPublished.RequestID;
  if Assigned(FOnSelectionChanged) then FOnSelectionChanged(Self);
end;
procedure TERDDiagnosticList.SetSearchText(const Value: string);
begin if FSearchText = Value then Exit; FSearchText := Value; RebuildVisible; Changed; end;
procedure TERDDiagnosticList.SetSeverities(Value: TERDDiagnosticSeverities);
begin if FSeverities = Value then Exit; FSeverities := Value; RebuildVisible; Changed; end;
procedure TERDDiagnosticList.SetGroupByFile(Value: Boolean);
begin if FGroupByFile = Value then Exit; FGroupByFile := Value; RebuildVisible; Changed; end;
function TERDDiagnosticList.DiagnosticCount: Integer;
begin Result := Length(FItems); end;
function TERDDiagnosticList.VisibleDiagnosticCount: Integer;
begin Result := Length(FVisible); end;
function TERDDiagnosticList.SeverityCount(Severity: TERDDiagnosticSeverity; VisibleOnly: Boolean): Integer;
var I: Integer; Value: TERDDiagnostic;
begin
  Result := 0;
  if VisibleOnly then begin for I in FVisible do if FItems[I].Severity = Severity then Inc(Result); end
  else for Value in FItems do if Value.Severity = Severity then Inc(Result);
end;
function TERDDiagnosticList.Diagnostic(Index: Integer): TERDDiagnostic;
begin
  if (Index < 0) or (Index >= Length(FItems)) then raise EArgumentOutOfRangeException.Create('Unknown diagnostic');
  Result := FItems[Index];
end;
function TERDDiagnosticList.SelectedDiagnostic(out Value: TERDDiagnostic): Boolean;
begin Value := Default(TERDDiagnostic); Result := not FStale and ItemFor(FocusedNode,Value); end;
function TERDDiagnosticList.SelectDiagnostic(const ID: string): Boolean;
var Node: PVirtualNode;
begin
  Result := FNodes.TryGetValue(ID,Node); if not Result then Exit;
  if Node.Parent <> RootNode then Expanded[Node.Parent] := True;
  ClearSelection; Selected[Node] := True; FocusedNode := Node; ScrollIntoView(Node,False);
end;
procedure TERDDiagnosticList.NavigateSelected;
var Value: TERDDiagnostic; Current: TERDDiagnosticRequest;
begin
  Current := FPublished;
  if SelectedDiagnostic(Value) and (Value.DocumentUri <> '') and (Value.Line > 0) and Assigned(FOnNavigate) then FOnNavigate(Self,Current,Value);
end;
function TERDDiagnosticList.NavigateNext(Backwards: Boolean): Boolean;
var Value: TERDDiagnostic; I, Start, Index, Count: Integer;
begin
  Result := False; Count := Length(FVisible); if FStale or (Count = 0) then Exit;
  Start := -1; if Backwards then Start := 0;
  if ItemFor(FocusedNode,Value) then for I := 0 to Count-1 do if FItems[FVisible[I]].ID = Value.ID then begin Start := I; Break; end;
  for I := 1 to Count do
  begin
    if Backwards then Index := (Start-I+Count) mod Count else Index := (Start+I) mod Count;
    Value := FItems[FVisible[Index]];
    if (Value.DocumentUri = '') or (Value.Line = 0) then Continue;
    if SelectDiagnostic(Value.ID) then begin Result := True; NavigateSelected; Exit; end;
  end;
end;
function TERDDiagnosticList.ExportText(VisibleOnly: Boolean): string;
var Builder: TStringBuilder; I, Index, Count: Integer;
begin
  Builder := TStringBuilder.Create;
  try
    Count := Length(FItems); if VisibleOnly then Count := Length(FVisible);
    for I := 0 to Count-1 do
    begin
      Index := I; if VisibleOnly then Index := FVisible[I];
      if I > 0 then Builder.Append(sLineBreak); Builder.Append(DiagnosticText(FItems[Index]));
    end;
    Result := Builder.ToString;
  finally Builder.Free; end;
end;
procedure TERDDiagnosticList.CopySelectedDiagnostic;
var Value: TERDDiagnostic;
begin if ItemFor(FocusedNode,Value) then Clipboard.AsText := DiagnosticText(Value); end;
procedure TERDDiagnosticList.CopyVisibleDiagnostics;
begin Clipboard.AsText := ExportText; end;
function TERDDiagnosticList.SeverityInk(Value: TERDDiagnosticSeverity): TColor;
begin
  if FStale then Exit(Palette.Subtle);
  case Value of dgsError: Result := Palette.Danger; dgsWarning: Result := Palette.Warning;
    dgsInformation: Result := Palette.Accent; else Result := Palette.Subtle; end;
end;
procedure TERDDiagnosticList.DoGetText(var E: TVSTGetCellTextEventArgs);
var Value: TERDDiagnostic; Group: Integer;
begin
  E.CellText := '';
  if not ItemFor(E.Node,Value) then
  begin
    Group := -IndexFor(E.Node)-1;
    if (E.Column = 0) and (Group >= 0) and (Group < FGroupUris.Count) then
    begin
      E.CellText := FGroupUris[Group]; if E.CellText = '' then E.CellText := 'Project';
      E.CellText := E.CellText+' ('+IntToStr(FGroupCounts[Group])+')';
    end; Exit;
  end;
  case E.Column of
    0: E.CellText := Value.MessageText;
    1: E.CellText := ERDDiagnosticSeverityName(Value.Severity);
    2: E.CellText := Value.Code;
    3: E.CellText := Value.DocumentUri;
    4: if Value.Line > 0 then E.CellText := IntToStr(Value.Line);
    5: if Value.Column > 0 then E.CellText := IntToStr(Value.Column);
    6: E.CellText := Value.Origin;
  end;
end;
procedure TERDDiagnosticList.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
var Value: TERDDiagnostic;
begin
  inherited;
  if not ItemFor(Node,Value) then begin Canvas.Font.Style := Canvas.Font.Style+[fsBold]; Exit; end;
  Canvas.Font.Style := Canvas.Font.Style-[fsBold];
  if FStale then Canvas.Font.Color := Palette.Subtle else if Column = 1 then Canvas.Font.Color := SeverityInk(Value.Severity);
end;
procedure TERDDiagnosticList.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var Value: TERDDiagnostic;
begin
  inherited;
  if (Column <> 1) or not ItemFor(Node,Value) then Exit;
  // Selection is painted after this hook. Reserve space here and draw the icon afterwards.
  if CellPaintMode = cpmPaint then FCellIconLeft := ContentRect.Left;
  Inc(ContentRect.Left,MulDiv(28,CurrentPPI,96));
end;
procedure TERDDiagnosticList.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var Value: TERDDiagnostic; Painter: TERDPainter; Size: Integer; Ink: TColor;
begin
  inherited; if (Column <> 1) or not ItemFor(Node,Value) then Exit;
  Size := MulDiv(20,CurrentPPI,96); Ink := SeverityInk(Value.Severity);
  Painter := TERDPainter.Create(Canvas,Palette,CurrentPPI);
  try
    Painter.RoundRect(FCellIconLeft+MulDiv(3,CurrentPPI,96),CellRect.Top+(CellRect.Height-Size)/2,Size,Size,
      MulDiv(4,CurrentPPI,96),ERDMixColor(Ink,Palette.Background,0.14),clNone);
    Painter.GlyphLine(DiagnosticGlyph(Value.Severity),FCellIconLeft+MulDiv(13,CurrentPPI,96),CellRect.Top+CellRect.Height/2,Ink,CurrentPPI/96);
  finally Painter.Free; end;
end;
procedure TERDDiagnosticList.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
begin inherited; SelectionChanged; end;
function TERDDiagnosticList.HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
var P: TPoint; Value: TERDDiagnostic; Node: PVirtualNode; Group: Integer;
begin
  Content := Default(TERDHintContent); P := ScreenToClient(ScreenPoint);
  Result := False; if not PtInRect(ClientRect,P) then Exit;
  Content.Palette := Palette; Content.HasPalette := True; Node := GetNodeAt(P.X,P.Y);
  if not ItemFor(Node,Value) then
  begin
    Group := -IndexFor(Node)-1;
    if (Group >= 0) and (Group < FGroupUris.Count) then
    begin
      Content.Title := 'Source diagnostics'; Content.Description := FGroupUris[Group];
      if Content.Description = '' then Content.Description := 'Project';
      Content.Description := Content.Description+sLineBreak+IntToStr(FGroupCounts[Group])+' matching messages';
      Content.Glyph := glFile; Exit(True);
    end;
    Result := FErrorText <> '';
    if Result then begin Content.Title := 'Diagnostics unavailable'; Content.Description := FErrorText; Content.Glyph := glAlert; end;
    Exit;
  end;
  Result := True; Content.Title := ERDDiagnosticSeverityName(Value.Severity);
  if Value.Code <> '' then Content.Title := Content.Title+' '+Value.Code;
  Content.Description := Value.MessageText;
  if Value.DocumentUri <> '' then Content.Description := Content.Description+sLineBreak+LocationText(Value);
  if Value.Origin <> '' then Content.Description := Content.Description+sLineBreak+Value.Origin;
  if FStale then Content.Description := Content.Description+sLineBreak+'Stale result';
  if FErrorText <> '' then Content.Description := Content.Description+sLineBreak+FErrorText;
  Content.Glyph := DiagnosticGlyph(Value.Severity);
end;
procedure TERDDiagnosticList.MouseMove(Shift: TShiftState; X, Y: Integer);
var Content: TERDHintContent; Text: string;
begin
  inherited; Text := '';
  if HintContentAt(ClientToScreen(Point(X,Y)),Content) then Text := Content.Title+'|'+Content.Description;
  if Hint <> Text then begin Application.CancelHint; Hint := Text; end;
end;
procedure TERDDiagnosticList.DblClick;
begin NavigateSelected; end;
procedure TERDDiagnosticList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then begin NavigateSelected; Key := 0; Exit; end;
  if (Key = VK_F8) and ((Shift = []) or (Shift = [ssShift])) then begin NavigateNext(ssShift in Shift); Key := 0; Exit; end;
  if (Key = Ord('C')) and (Shift = [ssCtrl]) then begin CopySelectedDiagnostic; Key := 0; Exit; end;
  if (Key = Ord('C')) and (Shift = [ssCtrl,ssShift]) then begin CopyVisibleDiagnostics; Key := 0; Exit; end;
  inherited;
end;
end.
