unit ERD.Components.LocalsPanel;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
  Winapi.Windows, Vcl.Controls, Vcl.Graphics, Vcl.Menus, ERD.Components.Menus, ERD.Components.Control,
  ERD.Components.Types, ERD.Components.Paint, ERD.Components.ValueTree,
  ERD.Components.CallStack, ERD.Components.DebugModel;
type
  TERDDebugScope = record
    ID, Name: string;
    Reference: Int64;
    Expensive: Boolean;
  end;
  TERDLocalsRequest = record
    Stack: TERDStackRequest;
    ThreadID, FrameID, RequestID: Int64;
  end;
  TERDRequestScopesEvent = procedure(Sender: TObject; const Context: TERDLocalsRequest) of object;
  TERDRequestVariablesEvent = procedure(Sender: TObject; const Context: TERDLocalsRequest;
    const Request: TERDValueRequest; const Parent: TERDInspectValue) of object;
  TERDInspectLocalEvent = procedure(Sender: TObject; const Context: TERDLocalsRequest; const Value: TERDInspectValue) of object;
  /// Scoped, read-only locals presentation. The host supplies scopes and paged variables.
  TERDLocalsPanel = class(TERDCustomControl)
  private
    FDebugModel: TERDDebugModel;
    FTree: TERDValueTree;
    FMenu: TERDPopupMenu;
    FContext: TERDLocalsRequest;
    FFrame: TERDStackFrame;
    FNextRequest: Int64;
    FExecutionUri, FErrorText: string;
    FExecutionLine: Integer;
    FStale, FPending: Boolean;
    FOnRequestScopes: TERDRequestScopesEvent;
    FOnRequestVariables: TERDRequestVariablesEvent;
    FOnInspectValue: TERDInspectLocalEvent;
    procedure MenuOpening(Sender: TObject);
    procedure MenuAction(Sender: TObject);
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure ModelChanged(Sender: TObject);
    procedure ChildrenRequested(Sender: TObject; const Request: TERDValueRequest);
    procedure InspectRequested(Sender: TObject; const Value: TERDInspectValue);
    function ContextMatches(const Context: TERDLocalsRequest): Boolean;
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
    function BeginFrame(const Stack: TERDStackRequest; const Frame: TERDStackFrame): TERDLocalsRequest;
    function PublishScopes(const Context: TERDLocalsRequest; const Scopes: TArray<TERDDebugScope>): Boolean;
    function FailScopes(const Context: TERDLocalsRequest; const MessageText: string): Boolean;
    function PublishVariables(const Context: TERDLocalsRequest; const Request: TERDValueRequest;
      const Values: TArray<TERDInspectValue>; HasMore: Boolean): Boolean;
    function FailVariables(const Context: TERDLocalsRequest; const Request: TERDValueRequest; const MessageText: string): Boolean;
    procedure MarkStale;
    procedure RetryScopes;
    property ValueTree: TERDValueTree read FTree;
    property Context: TERDLocalsRequest read FContext;
    property Stale: Boolean read FStale;
    property Pending: Boolean read FPending;
    property ErrorText: string read FErrorText;
  published
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property PageSize: Integer read GetPageSize write SetPageSize default 100;
    property OnRequestScopes: TERDRequestScopesEvent read FOnRequestScopes write FOnRequestScopes;
    property OnRequestVariables: TERDRequestVariablesEvent read FOnRequestVariables write FOnRequestVariables;
    property OnInspectValue: TERDInspectLocalEvent read FOnInspectValue write FOnInspectValue;
  end;
implementation
constructor TERDLocalsPanel.Create(AOwner: TComponent);
var Item: TMenuItem;
begin
  inherited; Width := 400; Height := 300; TabStop := False;
  FStale := True; FContext.Stack.Generation := -1;
  FTree := TERDValueTree.Create(Self); FTree.Parent := Self;
  FTree.OnRequestChildren := ChildrenRequested; FTree.OnInspectValue := InspectRequested;
  FMenu := TERDPopupMenu.Create(Self); FMenu.OnPopup := MenuOpening;
  Item := TMenuItem.Create(FMenu); Item.Caption := 'Retry scopes'; Item.Tag := 0; Item.OnClick := MenuAction; FMenu.Items.Add(Item);
  Item := TMenuItem.Create(FMenu); Item.Caption := 'Retry variables'; Item.Tag := 1; Item.OnClick := MenuAction; FMenu.Items.Add(Item);
  FTree.PopupMenu := FMenu;
  ThemeChanged; Resize;
end;
destructor TERDLocalsPanel.Destroy;
begin
  SetDebugModel(nil);
  if FMenu <> nil then FMenu.OnPopup := nil;
  if FTree <> nil then begin FTree.OnRequestChildren := nil; FTree.OnInspectValue := nil; end;
  inherited;
end;
procedure TERDLocalsPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin end;
procedure TERDLocalsPanel.SetDebugModel(Value: TERDDebugModel);
begin
  if FDebugModel = Value then Exit;
  if FDebugModel <> nil then begin FDebugModel.RemoveListener(ModelChanged); FDebugModel.RemoveFreeNotification(Self); end;
  FDebugModel := Value;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); end;
  FContext.Stack.Generation := -1; MarkStale;
end;
procedure TERDLocalsPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDebugModel) then begin FDebugModel := nil; MarkStale; end;
end;
procedure TERDLocalsPanel.ModelChanged(Sender: TObject);
begin
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> FContext.Stack.Session) or
    (FDebugModel.ExecutionUri <> FExecutionUri) or (FDebugModel.ExecutionLine <> FExecutionLine)) then MarkStale;
end;
function TERDLocalsPanel.GetPageSize: Integer;
begin Result := 100; if FTree <> nil then Result := FTree.PageSize; end;
procedure TERDLocalsPanel.SetPageSize(Value: Integer);
begin FTree.PageSize := Value; end;
procedure TERDLocalsPanel.ThemeChanged;
begin
  inherited; if FTree <> nil then begin FTree.Theme := Theme; FTree.ThemeChanged; end; Resize;
end;
procedure TERDLocalsPanel.DensityChanged;
begin inherited; Resize; end;
procedure TERDLocalsPanel.Resize;
begin
  inherited; if FTree <> nil then FTree.SetBounds(0,ScaleValue(48),ClientWidth,Max(0,ClientHeight-ScaleValue(48)));
end;
procedure TERDLocalsPanel.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter; Title, StatusText: string; Ink: TColor;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    Painter.FillRect(ClientRect,Palette.Background); Title := 'Locals';
    if FFrame.Name <> '' then Title := Title+' - '+FFrame.Name;
    Ink := Palette.Subtle;
    if FPending then StatusText := 'Loading scopes...'
    else if FErrorText <> '' then begin StatusText := 'Error: '+FErrorText; Ink := Palette.Danger; end
    else if FContext.Stack.Generation < 0 then StatusText := 'Select a stack frame'
    else if FStale then StatusText := 'Stale - waiting for a current frame'
    else if (FTree = nil) or (FTree.RootNodeCount = 0) then StatusText := 'No scopes available'
    else StatusText := 'Expand a scope or object to load variables';
    Painter.TextF(ScaleValue(8),ScaleValue(13),Title,12,Palette.ForegroundText,twSemibold,taLeftJustify,Max(0,ClientWidth-ScaleValue(16)));
    Painter.TextF(ScaleValue(8),ScaleValue(33),StatusText,11,Ink,twRegular,taLeftJustify,Max(0,ClientWidth-ScaleValue(16)));
  finally Painter.Free; end;
end;
function TERDLocalsPanel.BeginFrame(const Stack: TERDStackRequest; const Frame: TERDStackFrame): TERDLocalsRequest;
begin
  Result := Default(TERDLocalsRequest);
  if (Stack.RequestID < 1) or (Stack.Session < 0) or (Stack.Generation < 0) or (Frame.ThreadID < 1) or (Frame.ID < 0) then Exit;
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> Stack.Session)) then Exit;
  if Stack.Session = FContext.Stack.Session then
    if (Stack.Generation < FContext.Stack.Generation) or ((Stack.Generation = FContext.Stack.Generation) and
      (Stack.RequestID < FContext.Stack.RequestID)) then Exit;
  MarkStale; Inc(FNextRequest); FContext.Stack := Stack; FContext.ThreadID := Frame.ThreadID;
  FContext.FrameID := Frame.ID; FContext.RequestID := FNextRequest; FFrame := Frame; FPending := True;
  if FDebugModel <> nil then begin FExecutionUri := FDebugModel.ExecutionUri; FExecutionLine := FDebugModel.ExecutionLine; end;
  Result := FContext; Invalidate;
  if Assigned(FOnRequestScopes) then FOnRequestScopes(Self,Result);
end;
function TERDLocalsPanel.ContextMatches(const Context: TERDLocalsRequest): Boolean;
begin
  Result := (Context.RequestID > 0) and (Context.RequestID = FContext.RequestID) and
    (Context.Stack.Session = FContext.Stack.Session) and (Context.Stack.Generation = FContext.Stack.Generation) and
    (Context.Stack.RequestID = FContext.Stack.RequestID) and (Context.ThreadID = FContext.ThreadID) and (Context.FrameID = FContext.FrameID);
end;
function TERDLocalsPanel.PublishScopes(const Context: TERDLocalsRequest; const Scopes: TArray<TERDDebugScope>): Boolean;
var Roots: TArray<TERDInspectValue>; Seen: TDictionary<string,Boolean>; I: Integer; SessionKey: string;
begin
  Result := False; if not FPending or not ContextMatches(Context) then Exit;
  Seen := TDictionary<string,Boolean>.Create;
  try
    for I := 0 to High(Scopes) do
    begin
      if (Scopes[I].ID = '') or (Scopes[I].Reference < 0) or Seen.ContainsKey(Scopes[I].ID) then Exit;
      Seen.Add(Scopes[I].ID,True);
    end;
  finally Seen.Free; end;
  SetLength(Roots,Length(Scopes));
  for I := 0 to High(Scopes) do
  begin
    Roots[I].ID := Scopes[I].ID; Roots[I].Name := Scopes[I].Name; Roots[I].Reference := Scopes[I].Reference;
    Roots[I].TypeName := 'scope'; Roots[I].HasChildren := Scopes[I].Reference > 0;
    if Scopes[I].Expensive then Roots[I].ValueText := 'Expensive - expand to load'
    else if Scopes[I].Reference = 0 then Roots[I].ValueText := 'Empty scope';
  end;
  SessionKey := IntToStr(Context.Stack.Session)+'/'+IntToStr(Context.Stack.Generation)+'/'+IntToStr(Context.Stack.RequestID)+
    '/'+IntToStr(Context.ThreadID)+'/'+IntToStr(Context.FrameID);
  FPending := False; FStale := False; FErrorText := ''; Invalidate;
  Result := FTree.LoadSnapshot(SessionKey,Context.RequestID,Roots);
end;
function TERDLocalsPanel.FailScopes(const Context: TERDLocalsRequest; const MessageText: string): Boolean;
begin
  Result := FPending and ContextMatches(Context);
  if Result then begin MarkStale; FErrorText := MessageText; if FErrorText = '' then FErrorText := 'Scopes unavailable'; Invalidate; end;
end;
procedure TERDLocalsPanel.ChildrenRequested(Sender: TObject; const Request: TERDValueRequest);
var Value: TERDInspectValue; Current: TERDLocalsRequest;
begin
  if FStale or FPending or not FTree.ValueByID(Request.ParentID,Value) then Exit;
  Current := FContext;
  if Assigned(FOnRequestVariables) then FOnRequestVariables(Self,Current,Request,Value)
  else FTree.FailChildren(Request,'No variables provider assigned');
end;
function TERDLocalsPanel.PublishVariables(const Context: TERDLocalsRequest; const Request: TERDValueRequest;
  const Values: TArray<TERDInspectValue>; HasMore: Boolean): Boolean;
begin Result := not FStale and not FPending and ContextMatches(Context) and FTree.PublishChildren(Request,Values,HasMore); end;
function TERDLocalsPanel.FailVariables(const Context: TERDLocalsRequest; const Request: TERDValueRequest; const MessageText: string): Boolean;
begin Result := not FStale and not FPending and ContextMatches(Context) and FTree.FailChildren(Request,MessageText); end;
procedure TERDLocalsPanel.InspectRequested(Sender: TObject; const Value: TERDInspectValue);
var Current: TERDLocalsRequest;
begin
  Current := FContext;
  if not FStale and not FPending and Assigned(FOnInspectValue) then FOnInspectValue(Self,Current,Value);
end;
procedure TERDLocalsPanel.MarkStale;
begin
  FStale := True; FPending := False; FContext.RequestID := 0; FErrorText := '';
  if FTree <> nil then FTree.MarkStale; Invalidate;
end;
procedure TERDLocalsPanel.MenuOpening(Sender: TObject);
var Value: TERDInspectValue;
begin
  FMenu.Theme := Theme;
  FMenu.Items[0].Enabled := not FPending and (FErrorText <> '');
  FMenu.Items[1].Enabled := not FStale and not FPending and FTree.SelectedValue(Value) and Value.HasChildren;
end;
procedure TERDLocalsPanel.MenuAction(Sender: TObject);
var Value: TERDInspectValue;
begin
  if TMenuItem(Sender).Tag = 0 then RetryScopes
  else if not FStale and not FPending and FTree.SelectedValue(Value) and Value.HasChildren then FTree.RequestChildren(Value.ID);
end;
procedure TERDLocalsPanel.RetryScopes;
var Stack: TERDStackRequest; Frame: TERDStackFrame;
begin
  if FErrorText = '' then Exit;
  Stack := FContext.Stack; Frame := FFrame; BeginFrame(Stack,Frame);
end;
end.
