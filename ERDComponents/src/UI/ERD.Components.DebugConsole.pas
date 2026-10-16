unit ERD.Components.DebugConsole;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Types, System.Math, System.Generics.Collections,
  Winapi.Windows, Vcl.Controls, Vcl.Graphics, Vcl.Menus,
  ERD.Components.Control, ERD.Components.Types, ERD.Components.Paint, ERD.Components.Edits,
  ERD.Components.Menus, ERD.Components.VirtualTree, ERD.Components.VT,
  ERD.Components.VT.Types, ERD.Components.VT.Header, ERD.Components.VT.BaseTree,
  ERD.Components.DebugModel;
type
  TERDDebugLogLevel = (dlsTrace, dlsDebug, dlsInfo, dlsWarning, dlsError);
  TERDDebugLogLevels = set of TERDDebugLogLevel;
  TERDConsoleEntry = record
    ID, Session: Int64;
    Timestamp: TDateTime;
    Level: TERDDebugLogLevel;
    Channel, Text: string;
    Truncated: Boolean;
  end;
  TERDConsoleCommandEvent = procedure(Sender: TObject; Session: Int64; const Command: string) of object;
  TERDConsoleEntryEvent = procedure(Sender: TObject; const Entry: TERDConsoleEntry) of object;
  TERDDebugConsole = class;
  TERDConsoleTree = class(TERDVirtualStringTree)
  private
    FHost: TERDDebugConsole;
  protected
    procedure DoGetText(var E: TVSTGetCellTextEventArgs); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  /// Bounded presentation and command dispatch. No transport, execution or worker thread.
  TERDDebugConsole = class(TERDCustomControl)
  private
    FTree: TERDConsoleTree;
    FSearch, FInput: TERDEdit;
    FLevelChoice, FChannelChoice: TERDComboBox;
    FMenu: TERDPopupMenu;
    FDebugModel: TERDDebugModel;
    FBuffer, FDisplay, FFrozen: TArray<TERDConsoleEntry>;
    FHead, FCount, FTextChars: Integer;
    FMaxEntries, FMaxTextChars, FMaxEntryChars, FHistoryLimit: Integer;
    FNextID, FSession, FPendingCount: Int64;
    FLevels: TERDDebugLogLevels;
    FSearchText, FChannelFilter, FCommandStatus, FHistoryDraft: string;
    FHistory: TStringList;
    FHistoryIndex, FUpdateDepth: Integer;
    FPaused, FAutoScroll, FFollowingTail, FShowInput, FUpdating, FHistoryNavigating, FSourceAvailable, FDirty: Boolean;
    FOnCommand: TERDConsoleCommandEvent;
    FOnInspectEntry: TERDConsoleEntryEvent;
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure ModelChanged(Sender: TObject);
    procedure SetMaxEntries(Value: Integer);
    procedure SetMaxTextChars(Value: Integer);
    procedure SetMaxEntryChars(Value: Integer);
    procedure SetHistoryLimit(Value: Integer);
    procedure SetPaused(Value: Boolean);
    procedure SetAutoScroll(Value: Boolean);
    procedure SetShowInput(Value: Boolean);
    procedure SetSearchText(const Value: string);
    procedure SetChannelFilter(const Value: string);
    procedure SetLevels(Value: TERDDebugLogLevels);
    procedure FilterChanged(Sender: TObject);
    procedure InputChanged(Sender: TObject);
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Scrolled(Sender: TERDVTBaseVirtualTree; DeltaX, DeltaY: TDimension);
    procedure MenuOpening(Sender: TObject);
    procedure MenuAction(Sender: TObject);
    procedure EvictOldest;
    procedure LimitEntry(var Entry: TERDConsoleEntry);
    procedure ApplyLimits;
    procedure Changed;
    procedure RebuildView;
    procedure UpdateInputState;
    function SourceReady: Boolean;
    function StoredEntries: TArray<TERDConsoleEntry>;
    function DisplayEntry(Node: PVirtualNode; out Entry: TERDConsoleEntry): Boolean;
    function PreviewText(const Entry: TERDConsoleEntry; out Lines: Integer): string;
    function FormatEntry(const Entry: TERDConsoleEntry): string;
    function LevelText(Level: TERDDebugLogLevel): string;
    procedure MoveHistory(Direction: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ThemeChanged; override;
    procedure DensityChanged; override;
    function BeginSession(Session: Int64; ClearHistory: Boolean = True): Boolean;
    function AddEntry(Session: Int64; Timestamp: TDateTime; Level: TERDDebugLogLevel; const Channel, Text: string): Int64;
    procedure AddEntries(const Entries: TArray<TERDConsoleEntry>);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure JumpToTail;
    function EntryAt(Index: Integer): TERDConsoleEntry;
    function VisibleEntry(Index: Integer; out Entry: TERDConsoleEntry): Boolean;
    function SelectedEntry(out Entry: TERDConsoleEntry): Boolean;
    function VisibleCount: Integer;
    function SubmitCommand(const Command: string): Boolean;
    procedure ClearCommandHistory;
    function CommandHistory: TArray<string>;
    function SelectedText: string;
    function VisibleText: string;
    procedure CopySelected;
    procedure CopyVisible;
    procedure InspectSelected;
    property Count: Integer read FCount;
    property TextChars: Integer read FTextChars;
    property ActiveSession: Int64 read FSession;
    property PendingCount: Int64 read FPendingCount;
    property FollowingTail: Boolean read FFollowingTail;
    property EntryTree: TERDConsoleTree read FTree;
    property CommandEditor: TERDEdit read FInput;
  published
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property MaxEntries: Integer read FMaxEntries write SetMaxEntries default 5000;
    property MaxTextChars: Integer read FMaxTextChars write SetMaxTextChars default 1048576;
    property MaxEntryChars: Integer read FMaxEntryChars write SetMaxEntryChars default 65536;
    property HistoryLimit: Integer read FHistoryLimit write SetHistoryLimit default 100;
    property Paused: Boolean read FPaused write SetPaused stored False;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    property ShowInput: Boolean read FShowInput write SetShowInput default False;
    property SearchText: string read FSearchText write SetSearchText stored False;
    property ChannelFilter: string read FChannelFilter write SetChannelFilter stored False;
    property Levels: TERDDebugLogLevels read FLevels write SetLevels stored False;
    property OnCommand: TERDConsoleCommandEvent read FOnCommand write FOnCommand;
    property OnInspectEntry: TERDConsoleEntryEvent read FOnInspectEntry write FOnInspectEntry;
  end;
implementation
uses Vcl.Clipbrd;
procedure ConsoleTruncate(var Text: string; Limit: Integer);
begin
  if Length(Text) <= Limit then Exit;
  if (Limit > 0) and (Ord(Text[Limit]) >= $D800) and (Ord(Text[Limit]) <= $DBFF) and
    (Ord(Text[Limit+1]) >= $DC00) and (Ord(Text[Limit+1]) <= $DFFF) then Dec(Limit);
  SetLength(Text,Limit);
end;
constructor TERDConsoleTree.Create(AOwner: TComponent);
var I: Integer;
const Titles: array[0..4] of string = ('Time','Level','Channel','Session','Message');
      Widths: array[0..4] of Integer = (115,90,110,80,700);
begin
  FHost := AOwner as TERDDebugConsole; inherited; Header.Options := Header.Options+[hoVisible,hoColumnResize]; Header.Columns.Clear;
  for I := 0 to High(Titles) do with Header.Columns.Add do begin Text := Titles[I]; Width := Widths[I]; end;
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions+[toFullRowSelect,toMultiSelect,toRightClickSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions-[toEditable];
end;
procedure TERDConsoleTree.DoGetText(var E: TVSTGetCellTextEventArgs);
var Entry: TERDConsoleEntry; Lines: Integer;
begin
  E.CellText := ''; if not FHost.DisplayEntry(E.Node,Entry) then Exit;
  case E.Column of
    0: E.CellText := FormatDateTime('hh:nn:ss.zzz',Entry.Timestamp);
    1: E.CellText := FHost.LevelText(Entry.Level);
    2: E.CellText := Entry.Channel;
    3: E.CellText := IntToStr(Entry.Session);
    4: E.CellText := FHost.PreviewText(Entry,Lines);
  end;
end;
procedure TERDConsoleTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
var Entry: TERDConsoleEntry;
begin
  inherited; if not FHost.DisplayEntry(Node,Entry) then Exit;
  if Column = 4 then Canvas.Font.Name := 'Consolas';
  case Entry.Level of dlsError: Canvas.Font.Color := Palette.Danger; dlsWarning: Canvas.Font.Color := Palette.Warning;
    dlsTrace,dlsDebug: Canvas.Font.Color := Palette.Subtle; end;
end;
procedure TERDConsoleTree.DblClick;
begin FHost.InspectSelected; end;
procedure TERDConsoleTree.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('C')) and (Shift = [ssCtrl]) then begin FHost.CopySelected; Key := 0; Exit; end;
  if (Key = VK_RETURN) and (Shift = []) then begin FHost.InspectSelected; Key := 0; Exit; end;
  if (Key = VK_END) and (Shift = [ssCtrl]) then begin FHost.JumpToTail; Key := 0; Exit; end;
  inherited;
end;
constructor TERDDebugConsole.Create(AOwner: TComponent);
var I: Integer; Item: TMenuItem;
const Actions: array[0..4] of string = ('Copy selected','Copy visible','Inspect entry','Pause / resume','Follow latest');
begin
  inherited; Width := 700; Height := 400; TabStop := False;
  FMaxEntries := 5000;
  FMaxTextChars := 1048576;
  FMaxEntryChars := 65536;
  FHistoryLimit := 100;
  FAutoScroll := True;
  FFollowingTail := True; FSourceAvailable := True; FLevels := [dlsTrace,dlsDebug,dlsInfo,dlsWarning,dlsError];
  SetLength(FBuffer,FMaxEntries); FHistory := TStringList.Create;
  FSearch := TERDEdit.Create(Self); FSearch.Parent := Self; FSearch.TextHint := 'Search log text / channel'; FSearch.OnChange := FilterChanged;
  FLevelChoice := TERDComboBox.Create(Self); FLevelChoice.Parent := Self; FLevelChoice.Items.Add('All levels');
  for I := Ord(Low(TERDDebugLogLevel)) to Ord(High(TERDDebugLogLevel)) do FLevelChoice.Items.Add(LevelText(TERDDebugLogLevel(I)));
  FLevelChoice.ItemIndex := 0; FLevelChoice.OnChange := FilterChanged;
  FChannelChoice := TERDComboBox.Create(Self); FChannelChoice.Parent := Self; FChannelChoice.Items.Add('All channels'); FChannelChoice.ItemIndex := 0; FChannelChoice.OnChange := FilterChanged;
  FInput := TERDEdit.Create(Self); FInput.Parent := Self; FInput.Mono := True; FInput.MaxLength := 4096; FInput.WantReturnKey := True;
  FInput.TextHint := 'Command (Enter to send, Up/Down for history)'; FInput.OnKeyDown := InputKeyDown; FInput.OnChange := InputChanged; FInput.Visible := False;
  FTree := TERDConsoleTree.Create(Self); FTree.Parent := Self; FTree.OnScroll := Scrolled;
  FMenu := TERDPopupMenu.Create(Self); FMenu.OnPopup := MenuOpening;
  for I := 0 to High(Actions) do begin Item := TMenuItem.Create(FMenu); Item.Caption := Actions[I]; Item.Tag := I; Item.OnClick := MenuAction; FMenu.Items.Add(Item); end;
  FTree.PopupMenu := FMenu; ThemeChanged; Resize;
end;
destructor TERDDebugConsole.Destroy;
begin
  FUpdating := True; SetDebugModel(nil);
  if FTree <> nil then FTree.OnScroll := nil;
  if FSearch <> nil then FSearch.OnChange := nil;
  if FLevelChoice <> nil then FLevelChoice.OnChange := nil;
  if FChannelChoice <> nil then FChannelChoice.OnChange := nil;
  if FInput <> nil then begin FInput.OnChange := nil; FInput.OnKeyDown := nil; end;
  if FMenu <> nil then FMenu.OnPopup := nil;
  FHistory.Free; inherited;
end;
procedure TERDDebugConsole.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin end;
procedure TERDDebugConsole.SetDebugModel(Value: TERDDebugModel);
begin
  if FDebugModel = Value then Exit;
  if FDebugModel <> nil then begin FDebugModel.RemoveListener(ModelChanged); FDebugModel.RemoveFreeNotification(Self); end;
  FDebugModel := Value; FSourceAvailable := True;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); BeginSession(Value.Session,True); end;
  UpdateInputState;
end;
procedure TERDDebugConsole.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FDebugModel) then begin FDebugModel := nil; FSourceAvailable := False; UpdateInputState; end; end;
procedure TERDDebugConsole.ModelChanged(Sender: TObject);
begin
  if (FDebugModel <> nil) and (FSession <> FDebugModel.Session) then BeginSession(FDebugModel.Session,False);
  UpdateInputState;
end;
function TERDDebugConsole.SourceReady: Boolean;
begin Result := FSourceAvailable and ((FDebugModel = nil) or (FDebugModel.State <> dsDisconnected)); end;
procedure TERDDebugConsole.UpdateInputState;
begin if FInput <> nil then FInput.Enabled := SourceReady; Invalidate; end;
function TERDDebugConsole.BeginSession(Session: Int64; ClearHistory: Boolean): Boolean;
begin
  Result := (Session >= 0) and ((FDebugModel = nil) or (FDebugModel.Session = Session)); if not Result then Exit;
  FSession := Session; FSourceAvailable := True;
  if FInput <> nil then FInput.Text := '';
  if ClearHistory then Clear;
  UpdateInputState;
end;
procedure TERDDebugConsole.EvictOldest;
begin
  if FCount = 0 then Exit;
  Dec(FTextChars,Length(FBuffer[FHead].Text)+Length(FBuffer[FHead].Channel));
  FBuffer[FHead] := Default(TERDConsoleEntry); FHead := (FHead+1) mod Length(FBuffer); Dec(FCount);
end;
procedure TERDDebugConsole.LimitEntry(var Entry: TERDConsoleEntry);
var Limit: Integer;
begin
  if Length(Entry.Channel) > 128 then begin ConsoleTruncate(Entry.Channel,128); Entry.Truncated := True; end;
  Limit := Min(FMaxEntryChars,FMaxTextChars-Length(Entry.Channel));
  if Length(Entry.Text) > Limit then begin ConsoleTruncate(Entry.Text,Limit); Entry.Truncated := True; end;
end;
function TERDDebugConsole.AddEntry(Session: Int64; Timestamp: TDateTime; Level: TERDDebugLogLevel; const Channel, Text: string): Int64;
var Entry: TERDConsoleEntry; Index: Integer;
begin
  Result := 0; if Session <> FSession then Exit;
  if IsNan(Timestamp) or IsInfinite(Timestamp) or (Timestamp < 0) or (Timestamp >= EncodeDate(9999,12,31)+1) or (Ord(Level) < 0) or (Ord(Level) > Ord(High(TERDDebugLogLevel))) then raise EArgumentException.Create('Invalid console entry');
  Entry := Default(TERDConsoleEntry); Inc(FNextID); Entry.ID := FNextID; Entry.Session := Session; Entry.Timestamp := Timestamp; Entry.Level := Level;
  Entry.Channel := Trim(StringReplace(StringReplace(StringReplace(Channel,#13,' ',[rfReplaceAll]),#10,' ',[rfReplaceAll]),#9,' ',[rfReplaceAll])); if Entry.Channel = '' then Entry.Channel := 'stdout';
  Entry.Text := StringReplace(StringReplace(Text,#13#10,#10,[rfReplaceAll]),#13,#10,[rfReplaceAll]); LimitEntry(Entry);
  while (FCount > 0) and ((FCount >= FMaxEntries) or (FTextChars+Length(Entry.Text)+Length(Entry.Channel) > FMaxTextChars)) do EvictOldest;
  Index := (FHead+FCount) mod Length(FBuffer); FBuffer[Index] := Entry; Inc(FCount); Inc(FTextChars,Length(Entry.Text)+Length(Entry.Channel));
  if FPaused then Inc(FPendingCount); Result := Entry.ID; Changed;
end;
procedure TERDDebugConsole.AddEntries(const Entries: TArray<TERDConsoleEntry>);
var Entry: TERDConsoleEntry;
begin
  for Entry in Entries do if (Entry.Session = FSession) and (IsNan(Entry.Timestamp) or IsInfinite(Entry.Timestamp) or (Entry.Timestamp < 0) or (Entry.Timestamp >= EncodeDate(9999,12,31)+1) or (Ord(Entry.Level) < 0) or (Ord(Entry.Level) > Ord(High(TERDDebugLogLevel)))) then raise EArgumentException.Create('Invalid console batch');
  BeginUpdate; try for Entry in Entries do AddEntry(Entry.Session,Entry.Timestamp,Entry.Level,Entry.Channel,Entry.Text); finally EndUpdate; end;
end;
procedure TERDDebugConsole.BeginUpdate;
begin Inc(FUpdateDepth); end;
procedure TERDDebugConsole.EndUpdate;
begin
  if FUpdateDepth = 0 then raise EInvalidOperation.Create('Unbalanced console EndUpdate');
  Dec(FUpdateDepth); if (FUpdateDepth = 0) and FDirty then Changed;
end;
procedure TERDDebugConsole.Changed;
begin
  FDirty := True; if (FUpdateDepth > 0) or FUpdating then Exit;
  FDirty := False; if not FPaused then RebuildView; Invalidate;
end;
function TERDDebugConsole.StoredEntries: TArray<TERDConsoleEntry>;
var I: Integer;
begin SetLength(Result,FCount); for I := 0 to FCount-1 do Result[I] := FBuffer[(FHead+I) mod Length(FBuffer)]; end;
function TERDDebugConsole.EntryAt(Index: Integer): TERDConsoleEntry;
begin
  if (Index < 0) or (Index >= FCount) then raise EArgumentOutOfRangeException.Create('Unknown console entry');
  Result := FBuffer[(FHead+Index) mod Length(FBuffer)];
end;
procedure TERDDebugConsole.Clear;
begin
  while FCount > 0 do EvictOldest;
  FHead := 0; FFrozen := nil; FPendingCount := 0; RebuildView; Invalidate;
end;
procedure TERDDebugConsole.ApplyLimits;
var Entries: TArray<TERDConsoleEntry>; Entry: TERDConsoleEntry; I, Start, Chars: Integer;
begin
  Entries := StoredEntries; FBuffer := nil; SetLength(FBuffer,FMaxEntries); FHead := 0; FCount := 0; FTextChars := 0;
  for Entry in Entries do
  begin
    LimitEntry(Entry);
    while (FCount > 0) and ((FCount >= FMaxEntries) or (FTextChars+Length(Entry.Text)+Length(Entry.Channel) > FMaxTextChars)) do EvictOldest;
    FBuffer[(FHead+FCount) mod Length(FBuffer)] := Entry; Inc(FCount); Inc(FTextChars,Length(Entry.Text)+Length(Entry.Channel));
  end;
  Chars := 0; for I := 0 to High(FFrozen) do begin LimitEntry(FFrozen[I]); Inc(Chars,Length(FFrozen[I].Text)+Length(FFrozen[I].Channel)); end;
  Start := 0;
  while (Start < Length(FFrozen)) and ((Length(FFrozen)-Start > FMaxEntries) or (Chars > FMaxTextChars)) do
  begin Dec(Chars,Length(FFrozen[Start].Text)+Length(FFrozen[Start].Channel)); Inc(Start); end;
  if Start > 0 then FFrozen := Copy(FFrozen,Start,Length(FFrozen)-Start);
  RebuildView; Invalidate;
end;
procedure TERDDebugConsole.SetMaxEntries(Value: Integer);
begin if (Value < 1) or (Value > 100000) then raise EArgumentOutOfRangeException.Create('MaxEntries must be 1..100000'); if FMaxEntries = Value then Exit; FMaxEntries := Value; ApplyLimits; end;
procedure TERDDebugConsole.SetMaxTextChars(Value: Integer);
begin if (Value < 256) or (Value > 16777216) then raise EArgumentOutOfRangeException.Create('MaxTextChars must be 256..16777216'); FMaxTextChars := Value; ApplyLimits; end;
procedure TERDDebugConsole.SetMaxEntryChars(Value: Integer);
begin if (Value < 16) or (Value > 1048576) then raise EArgumentOutOfRangeException.Create('MaxEntryChars must be 16..1048576'); FMaxEntryChars := Value; ApplyLimits; end;
procedure TERDDebugConsole.SetHistoryLimit(Value: Integer);
begin
  if (Value < 1) or (Value > 1000) then raise EArgumentOutOfRangeException.Create('HistoryLimit must be 1..1000');
  FHistoryLimit := Value; while FHistory.Count > Value do FHistory.Delete(0); FHistoryIndex := FHistory.Count;
end;
procedure TERDDebugConsole.SetPaused(Value: Boolean);
begin
  if FPaused = Value then Exit;
  if Value then FFrozen := StoredEntries else FFrozen := nil;
  FPaused := Value; FPendingCount := 0; if not Value then FFollowingTail := True; RebuildView; Invalidate;
end;
procedure TERDDebugConsole.SetAutoScroll(Value: Boolean);
begin FAutoScroll := Value; if Value then JumpToTail; end;
procedure TERDDebugConsole.SetShowInput(Value: Boolean);
begin FShowInput := Value; if FInput <> nil then FInput.Visible := Value; Resize; end;
procedure TERDDebugConsole.SetSearchText(const Value: string);
begin if FSearchText = Value then Exit; FSearchText := Value; RebuildView; end;
procedure TERDDebugConsole.SetChannelFilter(const Value: string);
begin if FChannelFilter = Value then Exit; FChannelFilter := Value; RebuildView; end;
procedure TERDDebugConsole.SetLevels(Value: TERDDebugLogLevels);
begin FLevels := Value; RebuildView; end;
function TERDDebugConsole.LevelText(Level: TERDDebugLogLevel): string;
const Labels: array[TERDDebugLogLevel] of string = ('Trace','Debug','Info','Warning','Error');
begin Result := Labels[Level]; end;
function TERDDebugConsole.PreviewText(const Entry: TERDConsoleEntry; out Lines: Integer): string;
var I: Integer;
begin
  Result := Entry.Text; Lines := 1;
  for I := 1 to Length(Result) do if Result[I] = #10 then
  begin
    Inc(Lines); if Lines = 16 then begin Result := Copy(Result,1,I)+'... (copy/inspect full entry)'; Break; end;
  end;
  if Entry.Truncated then Result := Result+' [truncated]';
end;
function TERDDebugConsole.DisplayEntry(Node: PVirtualNode; out Entry: TERDConsoleEntry): Boolean;
begin Entry := Default(TERDConsoleEntry); Result := (Node <> nil) and (Node.Index < Cardinal(Length(FDisplay))); if Result then Entry := FDisplay[Node.Index]; end;
function TERDDebugConsole.VisibleEntry(Index: Integer; out Entry: TERDConsoleEntry): Boolean;
begin Entry := Default(TERDConsoleEntry); Result := (Index >= 0) and (Index < Length(FDisplay)); if Result then Entry := FDisplay[Index]; end;
function TERDDebugConsole.SelectedEntry(out Entry: TERDConsoleEntry): Boolean;
begin Result := DisplayEntry(FTree.FocusedNode,Entry); end;
function TERDDebugConsole.VisibleCount: Integer;
begin Result := Length(FDisplay); end;
procedure TERDDebugConsole.RebuildView;
var Source, Display: TArray<TERDConsoleEntry>; Entry: TERDConsoleEntry; Channels: TStringList;
    Selected: TDictionary<Int64,Boolean>; Node, Top: PVirtualNode; FocusID, TopID: Int64;
    Count, I, Lines, Offset, ChannelIndex, LevelIndex: Integer; Query, Preview: string; R: TRect;
begin
  if FUpdating or (FTree = nil) then Exit;
  FocusID := 0; TopID := 0; Offset := 0;
  if DisplayEntry(FTree.FocusedNode,Entry) then FocusID := Entry.ID;
  if DisplayEntry(FTree.TopNode,Entry) then begin TopID := Entry.ID; R := FTree.GetDisplayRect(FTree.TopNode,NoColumn,False); Offset := R.Top-FTree.Header.Height; end;
  Selected := TDictionary<Int64,Boolean>.Create; Channels := TStringList.Create;
  try
    Node := FTree.GetFirstSelected;
    while Node <> nil do begin if DisplayEntry(Node,Entry) then Selected.AddOrSetValue(Entry.ID,True); Node := FTree.GetNextSelected(Node); end;
    if FPaused then Source := FFrozen else Source := StoredEntries;
    Channels.Sorted := True; Channels.CaseSensitive := False; Channels.Duplicates := dupIgnore;
    if FChannelFilter <> '' then Channels.Add(FChannelFilter);
    SetLength(Display,Length(Source)); Count := 0; Query := UpperCase(FSearchText);
    for Entry in Source do
    begin
      Channels.Add(Entry.Channel);
      if not (Entry.Level in FLevels) or ((FChannelFilter <> '') and not SameText(FChannelFilter,Entry.Channel)) then Continue;
      if (Query <> '') and (Pos(Query,UpperCase(Entry.Channel+#10+Entry.Text)) = 0) then Continue;
      Display[Count] := Entry; Inc(Count);
    end;
    SetLength(Display,Count); FUpdating := True; FTree.BeginUpdate;
    try
      FSearch.Text := FSearchText; FChannelChoice.Items.BeginUpdate;
      try
        FChannelChoice.Items.Clear; FChannelChoice.Items.Add('All channels'); FChannelChoice.Items.AddStrings(Channels);
        ChannelIndex := 0; if FChannelFilter <> '' then ChannelIndex := Channels.IndexOf(FChannelFilter)+1;
        FChannelChoice.ItemIndex := ChannelIndex;
      finally FChannelChoice.Items.EndUpdate; end;
      LevelIndex := 0;
      for I := Ord(Low(TERDDebugLogLevel)) to Ord(High(TERDDebugLogLevel)) do if FLevels = [TERDDebugLogLevel(I)] then LevelIndex := I+1;
      if (LevelIndex = 0) and (FLevels <> [dlsTrace,dlsDebug,dlsInfo,dlsWarning,dlsError]) then FLevelChoice.Items[0] := 'Custom levels' else FLevelChoice.Items[0] := 'All levels';
      FLevelChoice.ItemIndex := LevelIndex;
      FTree.Clear; FDisplay := Display; FTree.RootNodeCount := Count; Node := FTree.GetFirst; Top := nil;
      while Node <> nil do
      begin
        Entry := FDisplay[Node.Index]; Preview := PreviewText(Entry,Lines);
        if Lines > 1 then begin Include(Node.States,vsMultiline); FTree.NodeHeight[Node] := MulDiv(FTree.RowHeight,FTree.CurrentPPI,96)*Lines; end;
        if Selected.ContainsKey(Entry.ID) then FTree.Selected[Node] := True;
        if Entry.ID = FocusID then FTree.FocusedNode := Node;
        if Entry.ID = TopID then Top := Node;
        Node := FTree.GetNext(Node);
      end;
      if not FPaused and FAutoScroll and FFollowingTail then FTree.ScrollIntoView(FTree.GetLastVisible,False)
      else if Top <> nil then begin FTree.TopNode := Top; FTree.OffsetY := FTree.OffsetY+Offset; end;
    finally FTree.EndUpdate; FUpdating := False; end;
  finally Channels.Free; Selected.Free; end;
  Invalidate;
end;
procedure TERDDebugConsole.Scrolled(Sender: TERDVTBaseVirtualTree; DeltaX, DeltaY: TDimension);
begin
  if FUpdating or FPaused or (DeltaY = 0) then Exit;
  FFollowingTail := FTree.RangeY+FTree.OffsetY <= FTree.ClientHeight-FTree.Header.Height+2; Invalidate;
end;
procedure TERDDebugConsole.JumpToTail;
begin
  FPaused := False; FFrozen := nil; FPendingCount := 0; FAutoScroll := True; FFollowingTail := True; RebuildView; Invalidate;
end;
procedure TERDDebugConsole.FilterChanged(Sender: TObject);
begin
  if FUpdating then Exit;
  if Sender = FSearch then FSearchText := FSearch.Text
  else if Sender = FLevelChoice then begin FLevels := [dlsTrace,dlsDebug,dlsInfo,dlsWarning,dlsError]; if FLevelChoice.ItemIndex > 0 then FLevels := [TERDDebugLogLevel(FLevelChoice.ItemIndex-1)]; end
  else if FChannelChoice.ItemIndex > 0 then FChannelFilter := FChannelChoice.Items[FChannelChoice.ItemIndex] else FChannelFilter := '';
  RebuildView;
end;
procedure TERDDebugConsole.ThemeChanged;
begin
  inherited;
  if FTree <> nil then begin FTree.Theme := Theme; FTree.ThemeChanged; end;
  if FSearch <> nil then begin FSearch.Theme := Theme; FSearch.ThemeChanged; end;
  if FInput <> nil then begin FInput.Theme := Theme; FInput.ThemeChanged; end;
  if FLevelChoice <> nil then begin FLevelChoice.Theme := Theme; FLevelChoice.ThemeChanged; end;
  if FChannelChoice <> nil then begin FChannelChoice.Theme := Theme; FChannelChoice.ThemeChanged; end; Resize;
end;
procedure TERDDebugConsole.DensityChanged;
begin
  inherited;
  if FSearch <> nil then begin FSearch.ParentDensity := False; FSearch.Density := Density; end;
  if FInput <> nil then begin FInput.ParentDensity := False; FInput.Density := Density; end;
  if FLevelChoice <> nil then begin FLevelChoice.ParentDensity := False; FLevelChoice.Density := Density; end;
  if FChannelChoice <> nil then begin FChannelChoice.ParentDensity := False; FChannelChoice.Density := Density; end;
  if FTree <> nil then begin FTree.RowHeight := Metrics.Edit+6; FTree.HeaderHeight := Metrics.Edit+4; RebuildView; end; Resize;
end;
procedure TERDDebugConsole.Resize;
var H, Margin, Top, Bottom, Half: Integer;
begin
  inherited; if (FTree = nil) or (FInput = nil) then Exit;
  Margin := ScaleValue(6); H := ScaleValue(Metrics.Edit); Top := ScaleValue(28); Half := Max(0,(ClientWidth-3*Margin) div 2);
  FSearch.SetBounds(Margin,Top,Max(0,ClientWidth-2*Margin),H); Inc(Top,H+Margin);
  FLevelChoice.SetBounds(Margin,Top,Half,H); FChannelChoice.SetBounds(2*Margin+Half,Top,Half,H); Inc(Top,H+Margin);
  Bottom := ClientHeight;
  if FShowInput then begin Dec(Bottom,H+2*Margin); FInput.SetBounds(Margin,Bottom+Margin,Max(0,ClientWidth-2*Margin),H); end;
  FTree.SetBounds(0,Top,ClientWidth,Max(0,Bottom-Top));
end;
procedure TERDDebugConsole.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter; Text: string;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    Painter.FillRect(ClientRect,Palette.Background);
    Text := 'Console | '+IntToStr(Length(FDisplay))+' visible / '+IntToStr(FCount)+' retained';
    if FPaused then Text := Text+' | paused, '+IntToStr(FPendingCount)+' new'
    else if not FFollowingTail then Text := Text+' | reading history';
    if FCommandStatus <> '' then Text := Text+' | '+FCommandStatus;
    Painter.TextF(ScaleValue(8),ScaleValue(13),Text,11,Palette.ForegroundText,twRegular,taLeftJustify,Max(0,ClientWidth-ScaleValue(16)));
  finally Painter.Free; end;
end;
function TERDDebugConsole.FormatEntry(const Entry: TERDConsoleEntry): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Entry.Timestamp)+#9+IntToStr(Entry.Session)+#9+LevelText(Entry.Level)+#9+Entry.Channel+#9+StringReplace(Entry.Text,#10,sLineBreak,[rfReplaceAll]);
  if Entry.Truncated then Result := Result+' [truncated]';
end;
function TERDDebugConsole.SelectedText: string;
var Builder: TStringBuilder; Node: PVirtualNode; Entry: TERDConsoleEntry;
begin
  Builder := TStringBuilder.Create;
  try
    Node := FTree.GetFirstSelected;
    while Node <> nil do begin if DisplayEntry(Node,Entry) then begin if Builder.Length > 0 then Builder.Append(sLineBreak); Builder.Append(FormatEntry(Entry)); end; Node := FTree.GetNextSelected(Node); end;
    Result := Builder.ToString;
  finally Builder.Free; end;
end;
function TERDDebugConsole.VisibleText: string;
var Builder: TStringBuilder; Entry: TERDConsoleEntry;
begin
  Builder := TStringBuilder.Create;
  try for Entry in FDisplay do begin if Builder.Length > 0 then Builder.Append(sLineBreak); Builder.Append(FormatEntry(Entry)); end; Result := Builder.ToString; finally Builder.Free; end;
end;
procedure TERDDebugConsole.CopySelected;
var Text: string;
begin Text := SelectedText; if Text <> '' then Clipboard.AsText := Text; end;
procedure TERDDebugConsole.CopyVisible;
begin if Length(FDisplay) > 0 then Clipboard.AsText := VisibleText; end;
procedure TERDDebugConsole.InspectSelected;
var Entry: TERDConsoleEntry;
begin if SelectedEntry(Entry) and Assigned(FOnInspectEntry) then FOnInspectEntry(Self,Entry); end;
procedure TERDDebugConsole.ClearCommandHistory;
begin FHistory.Clear; FHistoryIndex := 0; FHistoryDraft := ''; end;
function TERDDebugConsole.CommandHistory: TArray<string>;
var I: Integer;
begin SetLength(Result,FHistory.Count); for I := 0 to FHistory.Count-1 do Result[I] := FHistory[I]; end;
function TERDDebugConsole.SubmitCommand(const Command: string): Boolean;
var Text: string; Session: Int64;
begin
  Result := False; Text := Trim(Command);
  if (Text = '') or (Length(Text) > 4096) or (Pos(#10,Text) > 0) or (Pos(#13,Text) > 0) then Exit;
  if not SourceReady then begin FCommandStatus := 'Command source disconnected'; Invalidate; Exit; end;
  if not Assigned(FOnCommand) then begin FCommandStatus := 'No command handler'; Invalidate; Exit; end;
  if (FHistory.Count = 0) or (FHistory[FHistory.Count-1] <> Text) then FHistory.Add(Text);
  while FHistory.Count > FHistoryLimit do FHistory.Delete(0);
  FHistoryIndex := FHistory.Count; FHistoryDraft := ''; FInput.Text := ''; FCommandStatus := ''; Session := FSession;
  Result := True; FOnCommand(Self,Session,Text);
end;
procedure TERDDebugConsole.InputChanged(Sender: TObject);
begin
  if FHistoryNavigating then Exit;
  FHistoryIndex := FHistory.Count; FHistoryDraft := FInput.Text; FCommandStatus := ''; Invalidate;
end;
procedure TERDDebugConsole.MoveHistory(Direction: Integer);
begin
  if FHistory.Count = 0 then Exit;
  if FHistoryIndex = FHistory.Count then FHistoryDraft := FInput.Text;
  FHistoryIndex := EnsureRange(FHistoryIndex+Direction,0,FHistory.Count); FHistoryNavigating := True;
  try if FHistoryIndex = FHistory.Count then FInput.Text := FHistoryDraft else FInput.Text := FHistory[FHistoryIndex]; FInput.SelStart := Length(FInput.Text);
  finally FHistoryNavigating := False; end;
end;
procedure TERDDebugConsole.InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then Exit;
  case Key of
    VK_RETURN: begin SubmitCommand(FInput.Text); Key := 0; end;
    VK_UP: begin MoveHistory(-1); Key := 0; end;
    VK_DOWN: begin MoveHistory(1); Key := 0; end;
    VK_ESCAPE: begin FInput.Text := ''; Key := 0; end;
  end;
end;
procedure TERDDebugConsole.MenuOpening(Sender: TObject);
begin
  FMenu.Theme := Theme; FMenu.Items[0].Enabled := FTree.SelectedCount > 0;
  FMenu.Items[1].Enabled := Length(FDisplay) > 0; FMenu.Items[2].Enabled := FTree.FocusedNode <> nil;
end;
procedure TERDDebugConsole.MenuAction(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of 0: CopySelected; 1: CopyVisible; 2: InspectSelected; 3: Paused := not Paused; 4: JumpToTail; end;
end;
end.
