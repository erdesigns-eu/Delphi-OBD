unit ERD.Components.PCBScene;
interface
uses System.Classes, System.SysUtils, System.Types, System.Math, System.JSON,
  System.Generics.Collections, ERD.Components.Types;
type
  TERDPCBPinRole = (prPrimary, prSecondary, prClock, prSelect);
  TERDPCBItemKind = (piTimer, piCounter, piOutputGroup, piNote, piUART, piCANBus, piI2C, piSPI);
  TERDPCBNoteType = (ntNone, ntInfo, ntWarning, ntError, ntQuestion);
  /// Registered types reuse a base kind's pin rules and editing behavior.
  TERDPCBComponentType = record
    TypeID, DisplayName, NamePrefix, Caption, GlyphText, PropertiesJSON: string;
    Kind: TERDPCBItemKind;
    Icon: TERDGlyph;
    Width, Height: Double;
    Value: Integer;
    NoteType: TERDPCBNoteType;
  end;
  TERDPCBDesignItem = record
    ID, Name, Caption, Pin, SecondaryPin, ClockPin, SelectPin, TypeID, PropertiesJSON: string;
    Kind: TERDPCBItemKind;
    NoteType: TERDPCBNoteType;
    X, Y, Width, Height: Double;
    Value: Integer;
    Enabled, Locked, Selected: Boolean;
  end;
  TERDPCBValidateItemEvent = procedure(Sender: TObject; const Item: TERDPCBDesignItem) of object;
  /// Editable scene metadata; no hardware or application logic is executed.
  TERDPCBScene = class(TPersistent)
  private
    FOwner: TPersistent;
    FItems: TArray<TERDPCBDesignItem>;
    FUndo, FRedo: TList<string>;
    FDepth: Integer;
    FStart: string;
    FOnChange, FOnSelectionChange: TNotifyEvent;
    FOnValidateItem: TERDPCBValidateItemEvent;
    function Serialize(SelectedOnly, IncludeSelection: Boolean): string;
    procedure Remember;
    procedure Changed(Selection: Boolean);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    function UniqueName(const Prefix: string): string;
    function ItemNamePrefix(const Value: TERDPCBDesignItem): string;
    procedure CheckItem(const Item: TERDPCBDesignItem; Ignore: Integer);
  protected
    function GetOwner: TPersistent; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Count: Integer;
    function Item(Index: Integer): TERDPCBDesignItem;
    function IndexOf(const ID: string): Integer;
    function SelectionCount: Integer;
    function PrimaryIndex: Integer;
    function Add(Kind: TERDPCBItemKind; const Position: TPointF): Integer;
    function AddType(const TypeID: string; const Position: TPointF): Integer;
    procedure ValidateItem(Index: Integer; const Value: TERDPCBDesignItem);
    procedure UpdateItem(Index: Integer; const Value: TERDPCBDesignItem);
    procedure Select(Index: Integer; Additive: Boolean = False; Toggle: Boolean = False);
    procedure SelectAll;
    procedure ClearSelection;
    procedure DeleteSelected;
    procedure MoveSelected(DX, DY: Double);
    procedure BeginEdit;
    procedure EndEdit;
    procedure CancelEdit;
    procedure ResetHistory;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure Undo;
    procedure Redo;
    /// Scene content without transient selection flags, for host dirty tracking.
    function ContentJSON: string;
    function ToJSON(SelectedOnly: Boolean = False): string;
    procedure LoadJSON(const Text: string);
    procedure PasteJSON(const Text: string; Offset: Double = 1);
    property OnValidateItem: TERDPCBValidateItemEvent read FOnValidateItem write FOnValidateItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;
function ERDPCBNoteTypeName(NoteType: TERDPCBNoteType): string;
function ERDPCBParseNoteType(const Text: string): TERDPCBNoteType;
function ERDPCBBuiltInTypeID(Kind: TERDPCBItemKind): string;
function ERDPCBComponentTypes: TArray<TERDPCBComponentType>;
function ERDPCBFindComponentType(const TypeID: string; out Definition: TERDPCBComponentType): Boolean;
procedure ERDPCBRegisterComponentType(const Definition: TERDPCBComponentType);
function ERDPCBItemKindName(Kind: TERDPCBItemKind): string;
implementation
var ComponentTypes: TList<TERDPCBComponentType>;
function ERDPCBNoteTypeName(NoteType: TERDPCBNoteType): string;
begin
  case NoteType of ntNone: Result := 'None'; ntInfo: Result := 'Info';
    ntWarning: Result := 'Warning'; ntError: Result := 'Error'; ntQuestion: Result := 'Question';
  else raise EArgumentException.Create('Unknown note type'); end;
end;
function ERDPCBParseNoteType(const Text: string): TERDPCBNoteType;
var Value: TERDPCBNoteType;
begin
  for Value := Low(TERDPCBNoteType) to High(TERDPCBNoteType) do
    if SameText(Text, ERDPCBNoteTypeName(Value)) then Exit(Value);
  raise EArgumentException.Create('Unknown note type');
end;
function ERDPCBBuiltInTypeID(Kind: TERDPCBItemKind): string;
begin Result := 'erd.' + LowerCase(ERDPCBItemKindName(Kind)); end;
function ERDPCBComponentTypes: TArray<TERDPCBComponentType>;
begin Result := ComponentTypes.ToArray; end;
function ERDPCBFindComponentType(const TypeID: string; out Definition: TERDPCBComponentType): Boolean;
var Value: TERDPCBComponentType;
begin
  Definition := Default(TERDPCBComponentType);
  for Value in ComponentTypes do if SameText(Value.TypeID, TypeID) then begin Definition := Value; Exit(True); end;
  Result := False;
end;
procedure ValidateProperties(const Text: string);
var Root: TJSONValue;
begin
  if Text = '' then Exit;
  Root := TJSONObject.ParseJSONValue(Text);
  try if not (Root is TJSONObject) then raise EArgumentException.Create('Component properties must be a JSON object');
  finally Root.Free; end;
end;
procedure ERDPCBRegisterComponentType(const Definition: TERDPCBComponentType);
var Existing: TERDPCBComponentType;
begin
  ValidateProperties(Definition.PropertiesJSON);
  if (Ord(Definition.Kind) > Ord(High(TERDPCBItemKind))) or
    (Ord(Definition.NoteType) > Ord(High(TERDPCBNoteType))) then raise EArgumentException.Create('Invalid component kind or note type');
  if (Trim(Definition.TypeID) = '') or (Trim(Definition.DisplayName) = '') or (Trim(Definition.NamePrefix) = '') then
    raise EArgumentException.Create('Type ID, display name and name prefix are required');
  if ERDPCBFindComponentType(Definition.TypeID, Existing) then raise EArgumentException.Create('Component type is already registered');
  if IsNan(Definition.Width) or IsInfinite(Definition.Width) or IsNan(Definition.Height) or IsInfinite(Definition.Height) or
    (Definition.Width < 2) or (Definition.Width > 1000) or (Definition.Height < 2) or (Definition.Height > 1000) or
    (Definition.Value < 0) then raise EArgumentException.Create('Invalid component defaults');
  if ((Definition.Kind in [piTimer, piUART, piCANBus, piI2C, piSPI]) and (Definition.Value < 1)) or
    ((Definition.Kind = piOutputGroup) and ((Definition.Value < 1) or (Definition.Value > 65535))) then
    raise EArgumentException.Create('Invalid component value default');
  if (Definition.Kind <> piNote) and (Definition.NoteType <> ntNone) then raise EArgumentException.Create('Only notes have a note type');
  ComponentTypes.Add(Definition);
end;
function ERDPCBItemKindName(Kind: TERDPCBItemKind): string;
begin
  case Kind of piTimer: Result := 'Timer'; piCounter: Result := 'Counter';
    piOutputGroup: Result := 'OutputGroup'; piUART: Result := 'UART'; piCANBus: Result := 'CANBus';
    piI2C: Result := 'I2C'; piSPI: Result := 'SPI'; else Result := 'Note'; end;
end;
function NewID: string;
var G: TGUID;
begin CreateGUID(G); Result := GUIDToString(G); end;
constructor TERDPCBScene.Create(AOwner: TPersistent);
begin inherited Create; FOwner := AOwner; FUndo := TList<string>.Create; FRedo := TList<string>.Create; end;
destructor TERDPCBScene.Destroy;
begin FUndo.Free; FRedo.Free; inherited; end;
function TERDPCBScene.GetOwner: TPersistent;
begin Result := FOwner; end;
procedure TERDPCBScene.Changed(Selection: Boolean);
begin
  if Assigned(FOnChange) then FOnChange(Self);
  if Selection and Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;
procedure TERDPCBScene.Remember;
begin
  if FDepth > 0 then Exit;
  FUndo.Add(ToJSON); if FUndo.Count > 100 then FUndo.Delete(0); FRedo.Clear;
end;
procedure TERDPCBScene.BeginEdit;
begin if FDepth = 0 then FStart := ToJSON; Inc(FDepth); end;
procedure TERDPCBScene.EndEdit;
begin
  if FDepth = 0 then Exit;
  Dec(FDepth);
  if FDepth = 0 then
  begin
    if FStart <> ToJSON then begin FUndo.Add(FStart); if FUndo.Count > 100 then FUndo.Delete(0); FRedo.Clear; end;
    FStart := '';
  end;
end;
procedure TERDPCBScene.CancelEdit;
var Text: string;
begin if FDepth = 0 then Exit; Text := FStart; FDepth := 0; FStart := ''; LoadJSON(Text); end;
procedure TERDPCBScene.ResetHistory;
begin FUndo.Clear; FRedo.Clear; FDepth := 0; FStart := ''; end;
function TERDPCBScene.CanUndo: Boolean;
begin Result := (FUndo.Count > 0) and (FDepth = 0); end;
function TERDPCBScene.CanRedo: Boolean;
begin Result := (FRedo.Count > 0) and (FDepth = 0); end;
procedure TERDPCBScene.Undo;
var Text: string;
begin
  if not CanUndo then Exit; Text := FUndo.Last; FRedo.Add(ToJSON); FUndo.Delete(FUndo.Count - 1); LoadJSON(Text);
end;
procedure TERDPCBScene.Redo;
var Text: string;
begin
  if not CanRedo then Exit; Text := FRedo.Last; FUndo.Add(ToJSON); FRedo.Delete(FRedo.Count - 1); LoadJSON(Text);
end;
function TERDPCBScene.Count: Integer;
begin Result := Length(FItems); end;
function TERDPCBScene.Item(Index: Integer): TERDPCBDesignItem;
begin
  if (Index < 0) or (Index >= Count) then raise EArgumentOutOfRangeException.Create('Unknown designer item');
  Result := FItems[Index];
end;
function TERDPCBScene.IndexOf(const ID: string): Integer;
var I: Integer;
begin Result := -1; for I := 0 to High(FItems) do if FItems[I].ID = ID then Exit(I); end;
function TERDPCBScene.SelectionCount: Integer;
var I: Integer;
begin Result := 0; for I := 0 to High(FItems) do if FItems[I].Selected then Inc(Result); end;
function TERDPCBScene.PrimaryIndex: Integer;
var I: Integer;
begin Result := -1; for I := High(FItems) downto 0 do if FItems[I].Selected then Exit(I); end;
function TERDPCBScene.ItemNamePrefix(const Value: TERDPCBDesignItem): string;
var Definition: TERDPCBComponentType;
begin
  if ERDPCBFindComponentType(Value.TypeID, Definition) then Result := Definition.NamePrefix
  else Result := ERDPCBItemKindName(Value.Kind);
end;
function TERDPCBScene.UniqueName(const Prefix: string): string;
var N, I: Integer; Found: Boolean;
begin
  N := 0;
  repeat
    Inc(N); Result := Prefix + IntToStr(N); Found := False;
    for I := 0 to High(FItems) do if SameText(FItems[I].Name, Result) then begin Found := True; Break; end;
  until not Found;
end;
procedure TERDPCBScene.CheckItem(const Item: TERDPCBDesignItem; Ignore: Integer);
var I: Integer; Definition: TERDPCBComponentType;
begin
  ValidateProperties(Item.PropertiesJSON);
  if Ord(Item.NoteType) > Ord(High(TERDPCBNoteType)) then raise EArgumentException.Create('Invalid note type');
  if (Item.TypeID <> '') and ERDPCBFindComponentType(Item.TypeID, Definition) and (Definition.Kind <> Item.Kind) then
    raise EArgumentException.Create('Component type does not match its base kind');
  if (Item.Kind <> piNote) and (Item.NoteType <> ntNone) then raise EArgumentException.Create('Only notes have a note type');
  if Assigned(FOnValidateItem) then FOnValidateItem(Self, Item);
  if Trim(Item.Name) = '' then raise EArgumentException.Create('Component name is required');
  for I := 0 to High(FItems) do if I <> Ignore then
    if SameText(FItems[I].Name, Item.Name) or (FItems[I].ID = Item.ID) then
      raise EArgumentException.Create('Component names and IDs must be unique');
  if IsNan(Item.X) or IsInfinite(Item.X) or IsNan(Item.Y) or IsInfinite(Item.Y) or
    IsNan(Item.Width) or IsInfinite(Item.Width) or IsNan(Item.Height) or IsInfinite(Item.Height) or
    (Abs(Item.X) > 10000) or (Abs(Item.Y) > 10000) or (Item.Width < 2) or (Item.Height < 2) or
    (Item.Width > 1000) or (Item.Height > 1000) then raise EArgumentException.Create('Invalid component bounds');
  if (Item.Kind = piNote) and ((Item.Pin <> '') or (Item.SecondaryPin <> '') or
    (Item.ClockPin <> '') or (Item.SelectPin <> '')) then raise EArgumentException.Create('Notes cannot have pin assignments');
  if Item.Value < 0 then raise EArgumentException.Create('Value cannot be negative');
  if (Item.Kind in [piTimer, piUART, piCANBus, piI2C, piSPI]) and (Item.Value < 1) then raise EArgumentException.Create('Timer interval must be positive');
  if (Item.Kind = piOutputGroup) and ((Item.Value < 1) or (Item.Value > 65535)) then
    raise EArgumentException.Create('LED count must be 1..65535');
end;
function TERDPCBScene.Add(Kind: TERDPCBItemKind; const Position: TPointF): Integer;
begin Result := AddType(ERDPCBBuiltInTypeID(Kind), Position); end;
function TERDPCBScene.AddType(const TypeID: string; const Position: TPointF): Integer;
var Value: TERDPCBDesignItem; Definition: TERDPCBComponentType; I: Integer;
begin
  if not ERDPCBFindComponentType(TypeID, Definition) then raise EArgumentException.Create('Component type is not registered: ' + TypeID);
  Value := Default(TERDPCBDesignItem); Value.ID := NewID; Value.TypeID := Definition.TypeID;
  Value.Name := UniqueName(Definition.NamePrefix); Value.Kind := Definition.Kind;
  Value.PropertiesJSON := Definition.PropertiesJSON; if Value.PropertiesJSON = '' then Value.PropertiesJSON := '{}';
  Value.Caption := Definition.Caption; Value.Value := Definition.Value; Value.NoteType := Definition.NoteType;
  Value.Enabled := True; Value.Selected := True; Value.X := Position.X; Value.Y := Position.Y;
  Value.Width := Definition.Width; Value.Height := Definition.Height;
  CheckItem(Value, -1); Remember;
  for I := 0 to High(FItems) do FItems[I].Selected := False;
  Result := Count; SetLength(FItems, Result + 1); FItems[Result] := Value; Changed(True);
end;
procedure TERDPCBScene.ValidateItem(Index: Integer; const Value: TERDPCBDesignItem);
begin
  Item(Index); CheckItem(Value, Index);
  if Value.ID <> FItems[Index].ID then raise EArgumentException.Create('Component identity is immutable');
end;
procedure TERDPCBScene.UpdateItem(Index: Integer; const Value: TERDPCBDesignItem);
begin
  Item(Index); CheckItem(Value, Index);
  if Value.ID <> FItems[Index].ID then raise EArgumentException.Create('Component identity is immutable');
  Remember; FItems[Index] := Value; Changed(False);
end;
procedure TERDPCBScene.Select(Index: Integer; Additive: Boolean; Toggle: Boolean);
var I: Integer;
begin
  if Index >= 0 then Item(Index);
  if not Additive then for I := 0 to High(FItems) do if I <> Index then FItems[I].Selected := False;
  if Index >= 0 then
    if Toggle then FItems[Index].Selected := not FItems[Index].Selected else FItems[Index].Selected := True;
  Changed(True);
end;
procedure TERDPCBScene.ClearSelection;
var I: Integer;
begin
  if SelectionCount = 0 then Exit;
  for I := 0 to High(FItems) do FItems[I].Selected := False; Changed(True);
end;
procedure TERDPCBScene.SelectAll;
var I: Integer;
begin for I := 0 to High(FItems) do FItems[I].Selected := True; Changed(True); end;
procedure TERDPCBScene.DeleteSelected;
var I, J: Integer;
begin
  BeginEdit;
  try
    for I := High(FItems) downto 0 do if FItems[I].Selected and not FItems[I].Locked then
    begin for J := I to High(FItems) - 1 do FItems[J] := FItems[J + 1]; SetLength(FItems, Count - 1); end;
  finally EndEdit; end;
  Changed(True);
end;
procedure TERDPCBScene.MoveSelected(DX, DY: Double);
var I: Integer; Value: TERDPCBDesignItem;
begin
  if (DX = 0) and (DY = 0) then Exit;
  BeginEdit;
  try
    for I := 0 to High(FItems) do if FItems[I].Selected and not FItems[I].Locked then
    begin Value := FItems[I]; Value.X := Value.X + DX; Value.Y := Value.Y + DY; UpdateItem(I, Value); end;
  except CancelEdit; raise; end;
  EndEdit;
end;
function TERDPCBScene.Serialize(SelectedOnly, IncludeSelection: Boolean): string;
var Root, Obj: TJSONObject; Items: TJSONArray; Value: TERDPCBDesignItem; Properties: TJSONValue;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('format', 'erd-pcb-scene'); Root.AddPair('version', TJSONNumber.Create(1));
    Items := TJSONArray.Create; Root.AddPair('items', Items);
    for Value in FItems do if not SelectedOnly or Value.Selected then
    begin
      Obj := TJSONObject.Create; Items.AddElement(Obj);
      Obj.AddPair('id', Value.ID); Obj.AddPair('name', Value.Name); Obj.AddPair('caption', Value.Caption); Obj.AddPair('pin', Value.Pin);
      Obj.AddPair('secondary_pin', Value.SecondaryPin); Obj.AddPair('clock_pin', Value.ClockPin); Obj.AddPair('select_pin', Value.SelectPin);
      if Value.PropertiesJSON = '' then Properties := TJSONObject.Create
      else Properties := TJSONObject.ParseJSONValue(Value.PropertiesJSON);
      Obj.AddPair('properties', Properties);
      Obj.AddPair('type_id', Value.TypeID); Obj.AddPair('note_type', ERDPCBNoteTypeName(Value.NoteType));
      Obj.AddPair('kind', TJSONNumber.Create(Ord(Value.Kind))); Obj.AddPair('value', TJSONNumber.Create(Value.Value));
      Obj.AddPair('x', TJSONNumber.Create(Value.X)); Obj.AddPair('y', TJSONNumber.Create(Value.Y));
      Obj.AddPair('width', TJSONNumber.Create(Value.Width)); Obj.AddPair('height', TJSONNumber.Create(Value.Height));
      Obj.AddPair('enabled', TJSONBool.Create(Value.Enabled)); Obj.AddPair('locked', TJSONBool.Create(Value.Locked));
      if IncludeSelection then Obj.AddPair('selected', TJSONBool.Create(Value.Selected));
    end;
    Result := Root.ToJSON;
  finally Root.Free; end;
end;
function TERDPCBScene.ToJSON(SelectedOnly: Boolean): string;
begin Result := Serialize(SelectedOnly, True); end;
function TERDPCBScene.ContentJSON: string;
begin Result := Serialize(False, False); end;
procedure TERDPCBScene.LoadJSON(const Text: string);
var Root: TJSONValue; Items: TJSONArray; Obj: TJSONObject; Temp: TERDPCBScene;
    Value: TERDPCBDesignItem; I, Kind: Integer; Properties: TJSONValue;
begin
  Root := TJSONObject.ParseJSONValue(Text); Temp := TERDPCBScene.Create(nil);
  try
    if not (Root is TJSONObject) then raise EConvertError.Create('Invalid designer scene');
    if (Root.GetValue<string>('format') <> 'erd-pcb-scene') or (Root.GetValue<Integer>('version') <> 1) then
      raise EConvertError.Create('Unsupported designer scene');
    Items := Root.GetValue<TJSONArray>('items');
    if Items.Count > 4096 then raise EConvertError.Create('Too many designer components');
    for I := 0 to Items.Count - 1 do
    begin
      if not (Items.Items[I] is TJSONObject) then raise EConvertError.Create('Invalid designer component');
      Obj := TJSONObject(Items.Items[I]); Value := Default(TERDPCBDesignItem);
      Value.ID := Obj.GetValue<string>('id'); Value.Name := Obj.GetValue<string>('name');
      Value.Caption := Obj.GetValue<string>('caption'); Value.Pin := Obj.GetValue<string>('pin');
      Value.SecondaryPin := Obj.GetValue<string>('secondary_pin', ''); Value.ClockPin := Obj.GetValue<string>('clock_pin', ''); Value.SelectPin := Obj.GetValue<string>('select_pin', '');
      Kind := Obj.GetValue<Integer>('kind');
      if (Kind < Ord(Low(TERDPCBItemKind))) or (Kind > Ord(High(TERDPCBItemKind))) or (Value.ID = '') then
        raise EConvertError.Create('Invalid component kind or identity');
      Value.Kind := TERDPCBItemKind(Kind);
      Value.TypeID := Obj.GetValue<string>('type_id', ERDPCBBuiltInTypeID(Value.Kind));
      Properties := Obj.GetValue('properties'); Value.PropertiesJSON := '{}';
      if Properties <> nil then
      begin
        if not (Properties is TJSONObject) then raise EConvertError.Create('Invalid component properties');
        Value.PropertiesJSON := Properties.ToJSON;
      end;
      Value.NoteType := ERDPCBParseNoteType(Obj.GetValue<string>('note_type', 'None'));
      Value.Value := Obj.GetValue<Integer>('value');
      Value.X := Obj.GetValue<Double>('x'); Value.Y := Obj.GetValue<Double>('y');
      Value.Width := Obj.GetValue<Double>('width'); Value.Height := Obj.GetValue<Double>('height');
      Value.Enabled := Obj.GetValue<Boolean>('enabled'); Value.Locked := Obj.GetValue<Boolean>('locked');
      Value.Selected := Obj.GetValue<Boolean>('selected', False); Temp.CheckItem(Value, -1);
      SetLength(Temp.FItems, I + 1); Temp.FItems[I] := Value;
    end;
    if Assigned(FOnValidateItem) then
      for I := 0 to Temp.Count - 1 do FOnValidateItem(Temp, Temp.Item(I));
    FItems := Copy(Temp.FItems); Changed(True);
  finally Temp.Free; Root.Free; end;
end;
procedure TERDPCBScene.PasteJSON(const Text: string; Offset: Double);
var Temp: TERDPCBScene; I, Index: Integer; Value: TERDPCBDesignItem;
begin
  Temp := TERDPCBScene.Create(nil);
  try
    Temp.LoadJSON(Text); BeginEdit;
    try
      for I := 0 to High(FItems) do FItems[I].Selected := False;
      for I := 0 to Temp.Count - 1 do
      begin
        Value := Temp.Item(I); Value.ID := NewID; Value.Name := UniqueName(Temp.ItemNamePrefix(Value));
        Value.X := Value.X + Offset; Value.Y := Value.Y + Offset; Value.Selected := True;
        if Value.Kind in [piOutputGroup, piUART, piCANBus, piI2C, piSPI] then
        begin Value.Pin := ''; Value.SecondaryPin := ''; Value.ClockPin := ''; Value.SelectPin := ''; end;
        CheckItem(Value, -1); Index := Count; SetLength(FItems, Index + 1); FItems[Index] := Value;
      end;
    except CancelEdit; raise; end;
    EndEdit; Changed(True);
  finally Temp.Free; end;
end;
procedure TERDPCBScene.Assign(Source: TPersistent);
begin if Source is TERDPCBScene then LoadJSON(TERDPCBScene(Source).ToJSON) else inherited; end;
procedure TERDPCBScene.DefineProperties(Filer: TFiler);
begin
  inherited;
  if Filer.Ancestor is TERDPCBScene then
    Filer.DefineBinaryProperty('Data', ReadData, WriteData, ToJSON <> TERDPCBScene(Filer.Ancestor).ToJSON)
  else Filer.DefineBinaryProperty('Data', ReadData, WriteData, Count > 0);
end;
procedure TERDPCBScene.ReadData(Stream: TStream);
var Bytes: TBytes; Size: Int64;
begin
  Size := Stream.Size - Stream.Position;
  if (Size < 0) or (Size > 16 * 1024 * 1024) then raise EReadError.Create('Invalid scene size');
  SetLength(Bytes, Integer(Size)); if Size > 0 then Stream.ReadBuffer(Bytes[0], Length(Bytes));
  LoadJSON(TEncoding.UTF8.GetString(Bytes)); ResetHistory;
end;
procedure TERDPCBScene.WriteData(Stream: TStream);
var Bytes: TBytes;
begin Bytes := TEncoding.UTF8.GetBytes(ToJSON); if Length(Bytes) > 0 then Stream.WriteBuffer(Bytes[0], Length(Bytes)); end;
procedure RegisterBuiltInTypes;
var Kind: TERDPCBItemKind; NoteType: TERDPCBNoteType; Definition: TERDPCBComponentType;
begin
  for Kind := Low(TERDPCBItemKind) to High(TERDPCBItemKind) do
  begin
    Definition := Default(TERDPCBComponentType); Definition.Kind := Kind;
    Definition.TypeID := ERDPCBBuiltInTypeID(Kind); Definition.DisplayName := ERDPCBItemKindName(Kind);
    Definition.NamePrefix := Definition.DisplayName; Definition.Caption := Definition.DisplayName;
    Definition.Width := 16; Definition.Height := 8;
    case Kind of
      piTimer: begin Definition.Value := 1000; Definition.Icon := glClock; end;
      piCounter: Definition.GlyphText := '#';
      piOutputGroup: begin Definition.Value := 1; Definition.GlyphText := 'LED'; end;
      piNote: begin Definition.Width := 30; Definition.Height := 18; Definition.Icon := glFile; end;
      piUART: begin Definition.Value := 115200; Definition.GlyphText := 'UART'; end;
      piCANBus: begin Definition.Value := 500000; Definition.GlyphText := 'CAN'; end;
      piI2C: begin Definition.Value := 100000; Definition.GlyphText := 'I2C'; end;
      piSPI: begin Definition.Value := 1000000; Definition.GlyphText := 'SPI'; end;
    end;
    ERDPCBRegisterComponentType(Definition);
  end;
  for NoteType := ntInfo to ntQuestion do
  begin
    ERDPCBFindComponentType(ERDPCBBuiltInTypeID(piNote), Definition);
    Definition.TypeID := 'erd.note.' + LowerCase(ERDPCBNoteTypeName(NoteType));
    Definition.DisplayName := ERDPCBNoteTypeName(NoteType) + ' note';
    Definition.NamePrefix := ERDPCBNoteTypeName(NoteType) + 'Note'; Definition.NoteType := NoteType;
    ERDPCBRegisterComponentType(Definition);
  end;
end;
initialization
  ComponentTypes := TList<TERDPCBComponentType>.Create;
  RegisterBuiltInTypes;
finalization
  ComponentTypes.Free;
end.
