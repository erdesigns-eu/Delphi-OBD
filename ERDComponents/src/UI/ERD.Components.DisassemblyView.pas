unit ERD.Components.DisassemblyView;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Generics.Collections, Winapi.Windows,
  Vcl.Controls, Vcl.Graphics, ERD.Components.VirtualTree, ERD.Components.VT,
  ERD.Components.VT.Types, ERD.Components.VT.Header, ERD.Components.VT.BaseTree,
  ERD.Components.DebugModel;
type
  TERDInstructionAvailability = (iaAvailable, iaUnavailable);
  TERDDisassembledInstruction = record
    Address, Bytes, Instruction, Symbol, DocumentUri: string;
    Line, Column: Integer;
    Availability: TERDInstructionAvailability;
  end;
  TERDDisassemblyRequest = record
    Session, Generation, RequestID, InstructionOffset: Int64;
    MemoryReference: string;
    Count: Integer;
  end;
  TERDRequestDisassemblyEvent = procedure(Sender: TObject; const Request: TERDDisassemblyRequest) of object;
  TERDInstructionNavigateEvent = procedure(Sender: TObject; const DocumentUri: string; Line, Column: Integer) of object;
  /// Read-only instruction window. References and addresses are opaque backend strings.
  TERDDisassemblyView = class(TERDVirtualStringTree)
  private
    FDebugModel: TERDDebugModel;
    FItems: TArray<TERDDisassembledInstruction>;
    FRequest: TERDDisassemblyRequest;
    FNextRequest: Int64;
    FPageSize, FExecutionLine: Integer;
    FCurrentAddress, FExecutionUri, FErrorText: string;
    FPending, FStale: Boolean;
    FOnRequest: TERDRequestDisassemblyEvent;
    FOnNavigate: TERDInstructionNavigateEvent;
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure SetPageSize(Value: Integer);
    procedure ModelChanged(Sender: TObject);
    procedure UpdateStatus;
    function RequestMatches(const Request: TERDDisassemblyRequest): Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoGetText(var E: TVSTGetCellTextEventArgs); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RequestInstructions(Session, Generation: Int64; const MemoryReference: string; InstructionOffset: Int64 = 0): TERDDisassemblyRequest;
    function PublishInstructions(const Request: TERDDisassemblyRequest; const Instructions: TArray<TERDDisassembledInstruction>; const CurrentAddress: string): Boolean;
    function FailInstructions(const Request: TERDDisassemblyRequest; const MessageText: string): Boolean;
    function MovePage(Direction: Integer): TERDDisassemblyRequest;
    procedure SetInstructionPointer(const Address: string);
    function SelectAddress(const Address: string): Boolean;
    function SelectedInstruction(out Value: TERDDisassembledInstruction): Boolean;
    procedure NavigateSelected;
    procedure CopySelectedInstruction;
    procedure MarkStale;
    property Request: TERDDisassemblyRequest read FRequest;
    property CurrentAddress: string read FCurrentAddress;
    property Pending: Boolean read FPending;
    property Stale: Boolean read FStale;
    property ErrorText: string read FErrorText;
  published
    property RootNodeCount stored False;
    property Header stored False;
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property PageSize: Integer read FPageSize write SetPageSize default 128;
    property OnRequestInstructions: TERDRequestDisassemblyEvent read FOnRequest write FOnRequest;
    property OnNavigate: TERDInstructionNavigateEvent read FOnNavigate write FOnNavigate;
  end;
implementation
uses Vcl.Clipbrd;
constructor TERDDisassemblyView.Create(AOwner: TComponent);
var I: Integer;
const Titles: array[0..5] of string = ('','Address','Bytes','Instruction','Symbol','Source');
      Widths: array[0..5] of Integer = (30,120,130,260,160,280);
begin
  inherited; Width := 650; Height := 300;
  FPageSize := 128;
  FStale := True; FRequest.Generation := -1;
  Font.Name := 'Consolas'; ShowHint := True;
  Header.Options := Header.Options+[hoVisible,hoColumnResize]; Header.Columns.Clear;
  for I := 0 to High(Titles) do with Header.Columns.Add do begin Text := Titles[I]; Width := Widths[I]; end;
  TreeOptions.SelectionOptions := (TreeOptions.SelectionOptions+[toFullRowSelect])-[toMultiSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions-[toEditable];
end;
destructor TERDDisassemblyView.Destroy;
begin SetDebugModel(nil); inherited; end;
procedure TERDDisassemblyView.SetDebugModel(Value: TERDDebugModel);
begin
  if FDebugModel = Value then Exit;
  if FDebugModel <> nil then begin FDebugModel.RemoveListener(ModelChanged); FDebugModel.RemoveFreeNotification(Self); end;
  FDebugModel := Value;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); end;
  FRequest.Generation := -1; MarkStale;
end;
procedure TERDDisassemblyView.SetPageSize(Value: Integer);
begin
  if (Value < 1) or (Value > 4096) then raise EArgumentOutOfRangeException.Create('PageSize must be 1..4096');
  if FPageSize = Value then Exit; FPageSize := Value; MarkStale;
end;
procedure TERDDisassemblyView.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FDebugModel) then begin FDebugModel := nil; MarkStale; end; end;
procedure TERDDisassemblyView.ModelChanged(Sender: TObject);
begin
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> FRequest.Session) or
    (FDebugModel.ExecutionUri <> FExecutionUri) or (FDebugModel.ExecutionLine <> FExecutionLine)) then MarkStale;
end;
procedure TERDDisassemblyView.UpdateStatus;
begin
  if Header.Columns.Count < 4 then Exit;
  if FPending then Header.Columns[3].Text := 'Instruction (loading...)'
  else if FErrorText <> '' then Header.Columns[3].Text := 'Instruction - '+FErrorText
  else if FStale then Header.Columns[3].Text := 'Instruction (stale)'
  else Header.Columns[3].Text := 'Instruction';
  Hint := FErrorText; Invalidate;
end;
procedure TERDDisassemblyView.MarkStale;
begin FPending := False; FStale := True; FRequest.RequestID := 0; FCurrentAddress := ''; FErrorText := ''; UpdateStatus; end;
function TERDDisassemblyView.RequestInstructions(Session, Generation: Int64; const MemoryReference: string; InstructionOffset: Int64): TERDDisassemblyRequest;
begin
  Result := Default(TERDDisassemblyRequest);
  if (Session < 0) or (Generation < 0) or (MemoryReference = '') then Exit;
  if (FDebugModel <> nil) and ((FDebugModel.State <> dsPaused) or (FDebugModel.Session <> Session)) then Exit;
  if (FRequest.Session = Session) and (Generation < FRequest.Generation) then Exit;
  Inc(FNextRequest); FRequest.Session := Session; FRequest.Generation := Generation; FRequest.RequestID := FNextRequest;
  FRequest.MemoryReference := MemoryReference; FRequest.InstructionOffset := InstructionOffset; FRequest.Count := FPageSize;
  FPending := True; FStale := True; FCurrentAddress := ''; FErrorText := '';
  if FDebugModel <> nil then begin FExecutionUri := FDebugModel.ExecutionUri; FExecutionLine := FDebugModel.ExecutionLine; end;
  Result := FRequest; UpdateStatus; if Assigned(FOnRequest) then FOnRequest(Self,Result);
end;
function TERDDisassemblyView.RequestMatches(const Request: TERDDisassemblyRequest): Boolean;
begin
  Result := FPending and (Request.RequestID > 0) and (Request.RequestID = FRequest.RequestID) and
    (Request.Session = FRequest.Session) and (Request.Generation = FRequest.Generation) and
    (Request.MemoryReference = FRequest.MemoryReference) and (Request.InstructionOffset = FRequest.InstructionOffset) and (Request.Count = FRequest.Count);
end;
function TERDDisassemblyView.PublishInstructions(const Request: TERDDisassemblyRequest; const Instructions: TArray<TERDDisassembledInstruction>; const CurrentAddress: string): Boolean;
var Seen: TDictionary<string,Boolean>; Value: TERDDisassembledInstruction;
begin
  Result := False; if not RequestMatches(Request) or (Length(Instructions) > Request.Count) then Exit;
  Seen := TDictionary<string,Boolean>.Create;
  try
    for Value in Instructions do
    begin
      if (Value.Line < 0) or (Value.Column < 0) then Exit;
      if Value.Address = '' then begin if Value.Availability = iaAvailable then Exit; end
      else begin if Seen.ContainsKey(Value.Address) then Exit; Seen.Add(Value.Address,True); end;
    end;
  finally Seen.Free; end;
  BeginUpdate;
  try
    Clear; FItems := Copy(Instructions); RootNodeCount := Length(FItems); FPending := False; FStale := False;
    FCurrentAddress := CurrentAddress; FErrorText := '';
    SelectAddress(CurrentAddress);
  finally EndUpdate; end;
  Result := True; UpdateStatus;
end;
function TERDDisassemblyView.FailInstructions(const Request: TERDDisassemblyRequest; const MessageText: string): Boolean;
begin
  Result := RequestMatches(Request); if not Result then Exit;
  MarkStale; FErrorText := MessageText; if FErrorText = '' then FErrorText := 'Disassembly unavailable'; UpdateStatus;
end;
function TERDDisassemblyView.MovePage(Direction: Integer): TERDDisassemblyRequest;
var Offset: Int64;
begin
  Result := Default(TERDDisassemblyRequest);
  if ((Direction <> -1) and (Direction <> 1)) or (FRequest.MemoryReference = '') then Exit;
  if FStale and not FPending and (FErrorText = '') then Exit;
  Offset := FRequest.InstructionOffset;
  if (Direction > 0) and (Offset > High(Int64)-FPageSize) then Exit;
  if (Direction < 0) and (Offset < Low(Int64)+FPageSize) then Exit;
  Inc(Offset,Int64(Direction)*FPageSize);
  Result := RequestInstructions(FRequest.Session,FRequest.Generation,FRequest.MemoryReference,Offset);
end;
procedure TERDDisassemblyView.SetInstructionPointer(const Address: string);
begin if FStale then Exit; FCurrentAddress := Address; Invalidate; end;
function TERDDisassemblyView.SelectAddress(const Address: string): Boolean;
var Node: PVirtualNode;
begin
  Result := False; if FStale or (Address = '') then Exit; Node := GetFirst;
  while Node <> nil do
  begin
    if FItems[Node.Index].Address = Address then begin ClearSelection; Selected[Node] := True; FocusedNode := Node; ScrollIntoView(Node,False); Exit(True); end;
    Node := GetNext(Node);
  end;
end;
function TERDDisassemblyView.SelectedInstruction(out Value: TERDDisassembledInstruction): Boolean;
begin
  Value := Default(TERDDisassembledInstruction);
  Result := not FStale and (FocusedNode <> nil) and (FocusedNode.Index < Cardinal(Length(FItems)));
  if Result then Value := FItems[FocusedNode.Index];
end;
procedure TERDDisassemblyView.DoGetText(var E: TVSTGetCellTextEventArgs);
var Value: TERDDisassembledInstruction;
begin
  E.CellText := ''; if (E.Node = nil) or (E.Node.Index >= Cardinal(Length(FItems))) then Exit; Value := FItems[E.Node.Index];
  case E.Column of
    0: if not FStale and (Value.Availability = iaAvailable) and (Value.Address <> '') and (Value.Address = FCurrentAddress) then E.CellText := '▶';
    1: E.CellText := Value.Address;
    2: E.CellText := Value.Bytes;
    3: if Value.Availability = iaUnavailable then E.CellText := '<unavailable>' else E.CellText := Value.Instruction;
    4: E.CellText := Value.Symbol;
    5: begin E.CellText := Value.DocumentUri; if Value.Line > 0 then E.CellText := E.CellText+':'+IntToStr(Value.Line); if Value.Column > 0 then E.CellText := E.CellText+':'+IntToStr(Value.Column); end;
  end;
end;
procedure TERDDisassemblyView.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited; Canvas.Font.Name := 'Consolas'; if (Node = nil) or (Node.Index >= Cardinal(Length(FItems))) then Exit;
  if FStale or (FItems[Node.Index].Availability = iaUnavailable) then Canvas.Font.Color := Palette.Subtle
  else if (FCurrentAddress <> '') and (FItems[Node.Index].Address = FCurrentAddress) then Canvas.Font.Color := Palette.Warning;
end;
procedure TERDDisassemblyView.NavigateSelected;
var Value: TERDDisassembledInstruction;
begin
  if SelectedInstruction(Value) and (Value.Availability = iaAvailable) and (Value.DocumentUri <> '') and (Value.Line > 0) and Assigned(FOnNavigate) then
    FOnNavigate(Self,Value.DocumentUri,Value.Line,Value.Column);
end;
procedure TERDDisassemblyView.CopySelectedInstruction;
var Value: TERDDisassembledInstruction;
begin if SelectedInstruction(Value) then Clipboard.AsText := Value.Address+' '+Value.Bytes+' '+Value.Instruction; end;
procedure TERDDisassemblyView.DblClick;
begin NavigateSelected; end;
procedure TERDDisassemblyView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then begin NavigateSelected; Key := 0; Exit; end;
  if (Key = Ord('C')) and (Shift = [ssCtrl]) then begin CopySelectedInstruction; Key := 0; Exit; end;
  if (Key = VK_RIGHT) and (Shift = [ssAlt]) then begin MovePage(1); Key := 0; Exit; end;
  if (Key = VK_LEFT) and (Shift = [ssAlt]) then begin MovePage(-1); Key := 0; Exit; end;
  inherited;
end;
end.
