unit ERD.Components.WatchPanel;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Math, System.Generics.Collections, Winapi.Windows,
  Vcl.Controls, Vcl.Graphics, Vcl.Menus, ERD.Components.Control, ERD.Components.Types,
  ERD.Components.Paint, ERD.Components.Edits, ERD.Components.Menus, ERD.Components.ValueTree,
  ERD.Components.CallStack, ERD.Components.DebugModel;
type
  TERDWatchPanel = class;
  TERDWatchItem = class(TCollectionItem)
  private
    FExpression: string;
    procedure SetExpression(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Expression: string read FExpression write SetExpression;
  end;
  TERDWatchCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TERDWatchItem;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Owner: TERDWatchPanel);
    function Add: TERDWatchItem;
    property Items[Index: Integer]: TERDWatchItem read GetItem; default;
  end;
  TERDWatchContext = record
    Stack: TERDStackRequest;
    ThreadID, FrameID, RequestID: Int64;
  end;
  TERDWatchExpression = record
    ID: Integer;
    Expression: string;
  end;
  TERDWatchResult = record
    ID: Integer;
    Value: TERDInspectValue;
  end;
  TERDEvaluateWatchesEvent = procedure(Sender: TObject; const Context: TERDWatchContext; const Expressions: TArray<TERDWatchExpression>) of object;
  TERDWatchVariablesEvent = procedure(Sender: TObject; const Context: TERDWatchContext; const Request: TERDValueRequest; const Parent: TERDInspectValue) of object;
  TERDInspectWatchEvent = procedure(Sender: TObject; const Context: TERDWatchContext; const Value: TERDInspectValue) of object;
  /// Expression configuration and read-only results; evaluation belongs to the host.
  TERDWatchPanel = class(TERDCustomControl)
  private
    FWatches: TERDWatchCollection;
    FTree: TERDValueTree;
    FInput: TERDEdit;
    FMenu: TERDPopupMenu;
    FDebugModel: TERDDebugModel;
    FContext: TERDWatchContext;
    FFrame: TERDStackFrame;
    FNextRequest, FViewGeneration: Int64;
    FEditingID: Integer;
    FActive, FPending, FDestroying: Boolean;
    FErrorText, FExecutionUri: string;
    FExecutionLine: Integer;
    FOnEvaluate: TERDEvaluateWatchesEvent;
    FOnRequestVariables: TERDWatchVariablesEvent;
    FOnInspectValue: TERDInspectWatchEvent;
    procedure SetWatches(Value: TERDWatchCollection);
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure ModelChanged(Sender: TObject);
    procedure WatchesChanged;
    procedure ChildrenRequested(Sender: TObject; const Request: TERDValueRequest);
    procedure InspectRequested(Sender: TObject; const Value: TERDInspectValue);
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuOpening(Sender: TObject);
    procedure MenuAction(Sender: TObject);
    function FindWatch(ID: Integer): TERDWatchItem;
    function ContextMatches(const Context: TERDWatchContext): Boolean;
    function SelectedWatchID: Integer;
    function GetPageSize: Integer;
    procedure SetPageSize(Value: Integer);
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
    function BeginFrame(const Stack: TERDStackRequest; const Frame: TERDStackFrame): TERDWatchContext;
    function RefreshValues: TERDWatchContext;
    function PublishResults(const Context: TERDWatchContext; const Results: TArray<TERDWatchResult>): Boolean;
    function FailEvaluation(const Context: TERDWatchContext; const MessageText: string): Boolean;
    function PublishVariables(const Context: TERDWatchContext; const Request: TERDValueRequest; const Values: TArray<TERDInspectValue>; HasMore: Boolean): Boolean;
    function FailVariables(const Context: TERDWatchContext; const Request: TERDValueRequest; const MessageText: string): Boolean;
    function AddWatch(const Expression: string): Integer;
    procedure SetExpression(ID: Integer; const Expression: string);
    procedure DeleteWatch(ID: Integer);
    procedure EditSelected;
    procedure DeleteSelected;
    procedure MarkStale;
    property Context: TERDWatchContext read FContext;
    property Pending: Boolean read FPending;
    property Active: Boolean read FActive;
    property ErrorText: string read FErrorText;
    property ValueTree: TERDValueTree read FTree;
    property ExpressionEditor: TERDEdit read FInput;
  published
    property Watches: TERDWatchCollection read FWatches write SetWatches;
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property PageSize: Integer read GetPageSize write SetPageSize default 100;
    property OnEvaluate: TERDEvaluateWatchesEvent read FOnEvaluate write FOnEvaluate;
    property OnRequestVariables: TERDWatchVariablesEvent read FOnRequestVariables write FOnRequestVariables;
    property OnInspectValue: TERDInspectWatchEvent read FOnInspectValue write FOnInspectValue;
  end;
implementation
constructor TERDWatchItem.Create(Collection: TCollection);
begin FExpression := 'value'; inherited; end;
procedure TERDWatchItem.SetExpression(const Value: string);
var Text: string;
begin
  Text := Trim(Value);
  if (Text = '') or (Pos(#10,Text) > 0) or (Pos(#13,Text) > 0) then raise EArgumentException.Create('Watch expression must be a nonempty single line');
  if FExpression = Text then Exit; FExpression := Text; Changed(False);
end;
procedure TERDWatchItem.Assign(Source: TPersistent);
begin if Source is TERDWatchItem then Expression := TERDWatchItem(Source).Expression else inherited; end;
constructor TERDWatchCollection.Create(Owner: TERDWatchPanel);
begin inherited Create(Owner,TERDWatchItem); end;
function TERDWatchCollection.Add: TERDWatchItem;
begin Result := TERDWatchItem(inherited Add); end;
function TERDWatchCollection.GetItem(Index: Integer): TERDWatchItem;
begin Result := TERDWatchItem(inherited Items[Index]); end;
procedure TERDWatchCollection.Update(Item: TCollectionItem);
begin inherited; if GetOwner is TERDWatchPanel then TERDWatchPanel(GetOwner).WatchesChanged; end;
constructor TERDWatchPanel.Create(AOwner: TComponent);
var I: Integer; Item: TMenuItem;
const Captions: array[0..3] of string = ('Add expression','Edit expression','Delete expression','Refresh watches');
begin
  inherited; Width := 400; Height := 260; TabStop := False; FEditingID := -1; FContext.Stack.Generation := -1;
  FWatches := TERDWatchCollection.Create(Self);
  FInput := TERDEdit.Create(Self); FInput.Parent := Self; FInput.TextHint := 'Expression (Enter to add)'; FInput.OnKeyDown := InputKeyDown;
  FTree := TERDValueTree.Create(Self); FTree.Parent := Self;
  FTree.OnRequestChildren := ChildrenRequested; FTree.OnInspectValue := InspectRequested; FTree.OnKeyDown := TreeKeyDown;
  FMenu := TERDPopupMenu.Create(Self); FMenu.OnPopup := MenuOpening;
  for I := 0 to High(Captions) do
  begin Item := TMenuItem.Create(FMenu); Item.Caption := Captions[I]; Item.Tag := I; Item.OnClick := MenuAction; FMenu.Items.Add(Item); end;
  FTree.PopupMenu := FMenu; ThemeChanged; Resize;
end;
destructor TERDWatchPanel.Destroy;
begin
  FDestroying := True; SetDebugModel(nil);
  if FInput <> nil then FInput.OnKeyDown := nil;
  if FTree <> nil then begin FTree.OnRequestChildren := nil; FTree.OnInspectValue := nil; FTree.OnKeyDown := nil; end;
  if FMenu <> nil then FMenu.OnPopup := nil;
  FWatches.Free; inherited;
end;
procedure TERDWatchPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin end;
procedure TERDWatchPanel.SetWatches(Value: TERDWatchCollection);
begin FWatches.Assign(Value); end;
procedure TERDWatchPanel.SetDebugModel(Value: TERDDebugModel);
begin
  if FDebugModel = Value then Exit;
  if FDebugModel <> nil then begin FDebugModel.RemoveListener(ModelChanged); FDebugModel.RemoveFreeNotification(Self); end;
  FDebugModel := Value;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); end;
  FContext.Stack.Generation := -1; MarkStale;
end;
procedure TERDWatchPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited; if (Operation = opRemove) and (AComponent = FDebugModel) then begin FDebugModel := nil; MarkStale; end;
end;
procedure TERDWatchPanel.ModelChanged(Sender: TObject);
begin
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> FContext.Stack.Session) or
    (FDebugModel.ExecutionUri <> FExecutionUri) or (FDebugModel.ExecutionLine <> FExecutionLine)) then MarkStale;
end;
function TERDWatchPanel.GetPageSize: Integer;
begin Result := 100; if FTree <> nil then Result := FTree.PageSize; end;
procedure TERDWatchPanel.SetPageSize(Value: Integer);
begin FTree.PageSize := Value; end;
procedure TERDWatchPanel.ThemeChanged;
begin
  inherited;
  if FInput <> nil then begin FInput.Theme := Theme; FInput.ThemeChanged; end;
  if FTree <> nil then begin FTree.Theme := Theme; FTree.ThemeChanged; end; Resize;
end;
procedure TERDWatchPanel.DensityChanged;
begin
  inherited; if FInput <> nil then begin FInput.ParentDensity := False; FInput.Density := Density; end; Resize;
end;
procedure TERDWatchPanel.Resize;
var H, Top: Integer;
begin
  inherited; if (FTree = nil) or (FInput = nil) then Exit;
  H := ScaleValue(Metrics.Edit); Top := ScaleValue(28);
  FInput.SetBounds(ScaleValue(6),Top,Max(0,ClientWidth-ScaleValue(12)),H); Inc(Top,H+ScaleValue(6));
  FTree.SetBounds(0,Top,ClientWidth,Max(0,ClientHeight-Top));
end;
procedure TERDWatchPanel.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter; Text: string; Ink: TColor;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    Painter.FillRect(ClientRect,Palette.Background); Text := 'Watches'; Ink := Palette.ForegroundText;
    if FPending then Text := Text+' - evaluating...'
    else if FErrorText <> '' then begin Text := Text+' - '+FErrorText; Ink := Palette.Danger; end
    else if not FActive then Text := Text+' - stale / no current frame'
    else Text := Text+' - '+FFrame.Name;
    Painter.TextF(ScaleValue(8),ScaleValue(13),Text,12,Ink,twSemibold,taLeftJustify,Max(0,ClientWidth-ScaleValue(16)));
  finally Painter.Free; end;
end;
function TERDWatchPanel.FindWatch(ID: Integer): TERDWatchItem;
var I: Integer;
begin Result := nil; for I := 0 to FWatches.Count-1 do if FWatches[I].ID = ID then Exit(FWatches[I]); end;
function TERDWatchPanel.AddWatch(const Expression: string): Integer;
var Item: TERDWatchItem;
begin
  if (Trim(Expression) = '') or (Pos(#10,Expression) > 0) or (Pos(#13,Expression) > 0) then raise EArgumentException.Create('Watch expression must be a nonempty single line');
  FWatches.BeginUpdate;
  try Item := FWatches.Add; Item.Expression := Expression; Result := Item.ID; finally FWatches.EndUpdate; end;
end;
procedure TERDWatchPanel.SetExpression(ID: Integer; const Expression: string);
var Item: TERDWatchItem;
begin Item := FindWatch(ID); if Item = nil then raise EArgumentException.Create('Unknown watch'); Item.Expression := Expression; end;
procedure TERDWatchPanel.DeleteWatch(ID: Integer);
var Item: TERDWatchItem;
begin Item := FindWatch(ID); if Item <> nil then Item.Free; end;
procedure TERDWatchPanel.WatchesChanged;
var Roots: TArray<TERDInspectValue>; I: Integer;
begin
  if FDestroying or (FTree = nil) then Exit;
  FPending := False; FContext.RequestID := 0; FErrorText := '';
  if (FEditingID >= 0) and (FindWatch(FEditingID) = nil) then begin FEditingID := -1; FInput.Text := ''; FInput.TextHint := 'Expression (Enter to add)'; end;
  SetLength(Roots,FWatches.Count);
  for I := 0 to FWatches.Count-1 do
  begin Roots[I].ID := 'watch:'+IntToStr(FWatches[I].ID); Roots[I].Name := FWatches[I].Expression; Roots[I].ValueText := 'Not evaluated'; end;
  Inc(FViewGeneration); FTree.LoadSnapshot('watch-configuration',FViewGeneration,Roots); FTree.MarkStale; Invalidate;
  if FActive and not (csLoading in ComponentState) then RefreshValues;
end;
function TERDWatchPanel.BeginFrame(const Stack: TERDStackRequest; const Frame: TERDStackFrame): TERDWatchContext;
begin
  Result := Default(TERDWatchContext);
  if (Stack.RequestID < 1) or (Stack.Session < 0) or (Stack.Generation < 0) or (Frame.ThreadID < 1) or (Frame.ID < 0) then Exit;
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> Stack.Session)) then Exit;
  if Stack.Session = FContext.Stack.Session then
    if (Stack.Generation < FContext.Stack.Generation) or ((Stack.Generation = FContext.Stack.Generation) and (Stack.RequestID < FContext.Stack.RequestID)) then Exit;
  FContext.Stack := Stack; FContext.ThreadID := Frame.ThreadID; FContext.FrameID := Frame.ID; FFrame := Frame; FActive := True;
  if FDebugModel <> nil then begin FExecutionUri := FDebugModel.ExecutionUri; FExecutionLine := FDebugModel.ExecutionLine; end;
  Result := RefreshValues;
end;
function TERDWatchPanel.RefreshValues: TERDWatchContext;
var Expressions: TArray<TERDWatchExpression>; I: Integer;
begin
  Result := Default(TERDWatchContext); if not FActive then Exit;
  Inc(FNextRequest); FContext.RequestID := FNextRequest; Result := FContext;
  FPending := True; FErrorText := ''; FTree.MarkStale; Invalidate; SetLength(Expressions,FWatches.Count);
  for I := 0 to FWatches.Count-1 do begin Expressions[I].ID := FWatches[I].ID; Expressions[I].Expression := FWatches[I].Expression; end;
  if Length(Expressions) = 0 then PublishResults(Result,nil)
  else if Assigned(FOnEvaluate) then FOnEvaluate(Self,Result,Expressions)
  else FailEvaluation(Result,'No evaluation provider assigned');
end;
function TERDWatchPanel.ContextMatches(const Context: TERDWatchContext): Boolean;
begin
  Result := FActive and (Context.RequestID > 0) and (Context.RequestID = FContext.RequestID) and
    (Context.Stack.Session = FContext.Stack.Session) and (Context.Stack.Generation = FContext.Stack.Generation) and
    (Context.Stack.RequestID = FContext.Stack.RequestID) and (Context.ThreadID = FContext.ThreadID) and (Context.FrameID = FContext.FrameID);
end;
function TERDWatchPanel.PublishResults(const Context: TERDWatchContext; const Results: TArray<TERDWatchResult>): Boolean;
var Seen: TDictionary<Integer,Integer>; Roots: TArray<TERDInspectValue>; I, Index: Integer; SessionKey: string;
begin
  Result := False; if not FPending or not ContextMatches(Context) or (Length(Results) <> FWatches.Count) then Exit;
  Seen := TDictionary<Integer,Integer>.Create;
  try
    for I := 0 to High(Results) do
    begin
      if (FindWatch(Results[I].ID) = nil) or Seen.ContainsKey(Results[I].ID) then Exit;
      Seen.Add(Results[I].ID,I);
    end;
    SetLength(Roots,FWatches.Count);
    for I := 0 to FWatches.Count-1 do
    begin
      Index := Seen[FWatches[I].ID]; Roots[I] := Results[Index].Value;
      Roots[I].ID := 'watch:'+IntToStr(FWatches[I].ID); Roots[I].Name := FWatches[I].Expression;
    end;
  finally Seen.Free; end;
  SessionKey := IntToStr(Context.Stack.Session)+'/'+IntToStr(Context.Stack.Generation)+'/'+IntToStr(Context.Stack.RequestID)+
    '/'+IntToStr(Context.ThreadID)+'/'+IntToStr(Context.FrameID);
  FPending := False; FErrorText := ''; Inc(FViewGeneration); Result := FTree.LoadSnapshot(SessionKey,FViewGeneration,Roots); Invalidate;
end;
function TERDWatchPanel.FailEvaluation(const Context: TERDWatchContext; const MessageText: string): Boolean;
begin
  Result := FPending and ContextMatches(Context);
  if Result then begin FPending := False; FContext.RequestID := 0; FTree.MarkStale; FErrorText := MessageText; if FErrorText = '' then FErrorText := 'Evaluation failed'; Invalidate; end;
end;
procedure TERDWatchPanel.ChildrenRequested(Sender: TObject; const Request: TERDValueRequest);
var Parent: TERDInspectValue; Current: TERDWatchContext;
begin
  if not FActive or FPending or (FContext.RequestID = 0) or not FTree.ValueByID(Request.ParentID,Parent) then Exit;
  Current := FContext;
  if Assigned(FOnRequestVariables) then FOnRequestVariables(Self,Current,Request,Parent)
  else FTree.FailChildren(Request,'No variables provider assigned');
end;
function TERDWatchPanel.PublishVariables(const Context: TERDWatchContext; const Request: TERDValueRequest; const Values: TArray<TERDInspectValue>; HasMore: Boolean): Boolean;
begin Result := not FPending and ContextMatches(Context) and FTree.PublishChildren(Request,Values,HasMore); end;
function TERDWatchPanel.FailVariables(const Context: TERDWatchContext; const Request: TERDValueRequest; const MessageText: string): Boolean;
begin Result := not FPending and ContextMatches(Context) and FTree.FailChildren(Request,MessageText); end;
procedure TERDWatchPanel.InspectRequested(Sender: TObject; const Value: TERDInspectValue);
var Current: TERDWatchContext;
begin
  Current := FContext; if not FPending and ContextMatches(Current) and Assigned(FOnInspectValue) then FOnInspectValue(Self,Current,Value);
end;
procedure TERDWatchPanel.MarkStale;
begin
  FActive := False; FPending := False; FContext.RequestID := 0; FErrorText := '';
  if FTree <> nil then FTree.MarkStale; Invalidate;
end;
function TERDWatchPanel.SelectedWatchID: Integer;
var Value: TERDInspectValue;
begin
  Result := -1;
  if FTree.SelectedValue(Value) and Value.ID.StartsWith('watch:') then
    if not TryStrToInt(Copy(Value.ID,7,MaxInt),Result) then Result := -1;
end;
procedure TERDWatchPanel.EditSelected;
var Item: TERDWatchItem;
begin
  Item := FindWatch(SelectedWatchID); if Item = nil then Exit;
  FEditingID := Item.ID; FInput.Text := Item.Expression; FInput.TextHint := 'Edit expression (Enter to save, Esc to cancel)'; FInput.SetFocus;
end;
procedure TERDWatchPanel.DeleteSelected;
begin DeleteWatch(SelectedWatchID); end;
procedure TERDWatchPanel.InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then Exit;
  if Key = VK_RETURN then
  begin
    Key := 0; if Trim(FInput.Text) = '' then Exit;
    if FEditingID >= 0 then SetExpression(FEditingID,FInput.Text) else AddWatch(FInput.Text);
    FEditingID := -1; FInput.Text := ''; FInput.TextHint := 'Expression (Enter to add)';
  end
  else if Key = VK_ESCAPE then begin FEditingID := -1; FInput.Text := ''; FInput.TextHint := 'Expression (Enter to add)'; Key := 0; end;
end;
procedure TERDWatchPanel.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then Exit;
  case Key of VK_DELETE: begin DeleteSelected; Key := 0; end; VK_F2: begin EditSelected; Key := 0; end; end;
end;
procedure TERDWatchPanel.MenuOpening(Sender: TObject);
begin
  FMenu.Theme := Theme; FMenu.Items[1].Enabled := SelectedWatchID >= 0;
  FMenu.Items[2].Enabled := FMenu.Items[1].Enabled; FMenu.Items[3].Enabled := FActive;
end;
procedure TERDWatchPanel.MenuAction(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0: begin FEditingID := -1; FInput.Text := ''; FInput.TextHint := 'Expression (Enter to add)'; FInput.SetFocus; end;
    1: EditSelected; 2: DeleteSelected; 3: RefreshValues;
  end;
end;
end.
