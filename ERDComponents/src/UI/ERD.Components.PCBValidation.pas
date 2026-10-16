unit ERD.Components.PCBValidation;
interface
uses System.SysUtils, ERD.Components.PCBScene;
type
  TERDPCBValidationSeverity = (vsError, vsWarning, vsInfo);
  TERDPCBValidationIssue = record
    Severity: TERDPCBValidationSeverity;
    Code, MessageText, ItemID, ItemName, PropertyName, Pin, RelatedItemID: string;
    function Key: string;
  end;
  TERDPCBValidationPin = record
    ID: string;
    Capabilities: TArray<string>;
  end;
  TERDPCBValidationContext = record
    Items: TArray<TERDPCBDesignItem>;
    Pins: TArray<TERDPCBValidationPin>;
    HasTemplate: Boolean;
  end;
  TERDPCBValidationReport = class
  private
    FItems: TArray<TERDPCBValidationIssue>;
  public
    procedure Add(Severity: TERDPCBValidationSeverity; const Code, MessageText: string;
      const ItemID: string = ''; const ItemName: string = ''; const PropertyName: string = '';
      const Pin: string = ''; const RelatedItemID: string = '');
    function Issues: TArray<TERDPCBValidationIssue>;
  end;
  TERDPCBValidationRuleEvent = procedure(Sender: TObject; const Context: TERDPCBValidationContext;
    Report: TERDPCBValidationReport) of object;
  TERDPCBValidator = class
  private
    FOnValidate: TERDPCBValidationRuleEvent;
    procedure BuiltInRules(const Context: TERDPCBValidationContext; Report: TERDPCBValidationReport);
  public
    function Validate(const Context: TERDPCBValidationContext): TArray<TERDPCBValidationIssue>;
    property OnValidate: TERDPCBValidationRuleEvent read FOnValidate write FOnValidate;
  end;
function ERDPCBCaptureValidation(Scene: TERDPCBScene; const Pins: TArray<TERDPCBValidationPin>;
  HasTemplate: Boolean): TERDPCBValidationContext;
function ERDPCBValidationSeverityName(Value: TERDPCBValidationSeverity): string;
implementation
function ERDPCBValidationSeverityName(Value: TERDPCBValidationSeverity): string;
begin case Value of vsError: Result := 'Errors'; vsWarning: Result := 'Warnings'; else Result := 'Information'; end; end;
function TERDPCBValidationIssue.Key: string;
begin Result := Code+#31+ItemID+#31+PropertyName+#31+Pin+#31+RelatedItemID; end;
function CopyContext(const Value: TERDPCBValidationContext): TERDPCBValidationContext;
var I: Integer;
begin
  Result := Value; Result.Items := Copy(Value.Items); Result.Pins := Copy(Value.Pins);
  for I := 0 to High(Result.Pins) do Result.Pins[I].Capabilities := Copy(Value.Pins[I].Capabilities);
end;
function ERDPCBCaptureValidation(Scene: TERDPCBScene; const Pins: TArray<TERDPCBValidationPin>; HasTemplate: Boolean): TERDPCBValidationContext;
var I: Integer;
begin
  Result := Default(TERDPCBValidationContext); Result.HasTemplate := HasTemplate;
  if Scene <> nil then begin SetLength(Result.Items,Scene.Count); for I := 0 to Scene.Count-1 do Result.Items[I] := Scene.Item(I); end;
  Result.Pins := Copy(Pins); for I := 0 to High(Result.Pins) do Result.Pins[I].Capabilities := Copy(Pins[I].Capabilities);
end;
procedure TERDPCBValidationReport.Add(Severity: TERDPCBValidationSeverity; const Code, MessageText, ItemID, ItemName, PropertyName, Pin, RelatedItemID: string);
var N: Integer;
begin
  if (Ord(Severity) > Ord(High(TERDPCBValidationSeverity))) or (Trim(Code) = '') or (Trim(MessageText) = '') then raise EArgumentException.Create('Validation messages need a code and text');
  N := Length(FItems); SetLength(FItems,N+1); FItems[N].Severity := Severity; FItems[N].Code := Code;
  FItems[N].MessageText := MessageText; FItems[N].ItemID := ItemID; FItems[N].ItemName := ItemName;
  FItems[N].PropertyName := PropertyName; FItems[N].Pin := Pin; FItems[N].RelatedItemID := RelatedItemID;
end;
function TERDPCBValidationReport.Issues: TArray<TERDPCBValidationIssue>;
begin Result := Copy(FItems); end;
procedure TERDPCBValidator.BuiltInRules(const Context: TERDPCBValidationContext; Report: TERDPCBValidationReport);
var I, J: Integer; Item, Other: TERDPCBDesignItem; Definition: TERDPCBComponentType;
    Prop: TERDPCBPropertyDefinition; NeedsTemplate: Boolean;
  function DrivesPins(Kind: TERDPCBItemKind): Boolean;
  begin Result := Kind in [piOutputGroup,piUART,piCANBus,piI2C,piSPI]; end;
  procedure CheckPin(const Pin, Field, LabelText, Capability: string; Required: Boolean);
  var P, R: Integer; Supported: Boolean; OtherPin, Used: string; Role: TERDPCBPinRole;
  begin
    if Pin = '' then
    begin
      if Required and Item.Enabled then Report.Add(vsWarning,'pin.missing',LabelText+' has no assigned pin',Item.ID,Item.Name,Field);
      Exit;
    end;
    if Context.HasTemplate then
    begin
      P := -1; for R := 0 to High(Context.Pins) do if SameText(Context.Pins[R].ID,Pin) then begin P := R; Break; end;
      if P < 0 then Report.Add(vsError,'pin.unavailable','Pin '+Pin+' is not programmable in this template',Item.ID,Item.Name,Field,Pin)
      else if Capability <> '' then
      begin
        Supported := False; for Used in Context.Pins[P].Capabilities do if SameText(Used,Capability) then Supported := True;
        if not Supported then Report.Add(vsError,'pin.capability',LabelText+' requires '+Capability,Item.ID,Item.Name,Field,Pin);
      end;
    end;
    if not DrivesPins(Item.Kind) or not Item.Enabled then Exit;
    for R := 0 to High(Context.Items) do
    begin
      Other := Context.Items[R]; if (R = I) or not Other.Enabled or not DrivesPins(Other.Kind) then Continue;
      for Role := Low(TERDPCBPinRole) to High(TERDPCBPinRole) do
      begin
        case Role of prPrimary: OtherPin := Other.Pin; prSecondary: OtherPin := Other.SecondaryPin;
          prClock: OtherPin := Other.ClockPin; else OtherPin := Other.SelectPin; end;
        if (OtherPin <> '') and SameText(Pin,OtherPin) then
        begin Report.Add(vsError,'pin.conflict','Pin '+Pin+' is also assigned to '+Other.Name,Item.ID,Item.Name,Field,Pin,Other.ID); Break; end;
      end;
    end;
  end;
  procedure CheckDistinctRoles;
  var Pins: array[0..3] of string; A, B: Integer;
  begin
    Pins[0] := Item.Pin; Pins[1] := Item.SecondaryPin; Pins[2] := Item.ClockPin; Pins[3] := Item.SelectPin;
    for A := 0 to 3 do if Pins[A] <> '' then for B := 0 to A-1 do
      if SameText(Pins[A],Pins[B]) then begin Report.Add(vsError,'pin.role-conflict','Bus roles must use distinct pins',Item.ID,Item.Name,'pins',Pins[A]); Break; end;
  end;
begin
  NeedsTemplate := False;
  for I := 0 to High(Context.Items) do
  begin
    Item := Context.Items[I];
    if DrivesPins(Item.Kind) or (Item.Pin <> '') then NeedsTemplate := True;
    if not ERDPCBFindComponentType(Item.TypeID,Definition) then
      Report.Add(vsWarning,'type.unknown','Component type is not registered: '+Item.TypeID,Item.ID,Item.Name,'type')
    else
    begin
      if Definition.Kind <> Item.Kind then Report.Add(vsError,'type.kind','Registered type does not match this component',Item.ID,Item.Name,'type');
      for Prop in Definition.Properties do
        try ERDPCBValidateProperty(Prop,ERDPCBPropertyValue(Item,Prop.Name));
        except on E: EArgumentException do Report.Add(vsError,'property.invalid',E.Message,Item.ID,Item.Name,Prop.Name); end;
    end;
    if Trim(Item.Name) = '' then Report.Add(vsError,'name.missing','Component name is required',Item.ID,Item.Name,'name');
    for J := 0 to I-1 do
      if SameText(Item.Name,Context.Items[J].Name) then Report.Add(vsError,'name.conflict','Component name is already used',Item.ID,Item.Name,'name','',Context.Items[J].ID);
    if Item.Kind = piNote then Continue;
    CheckDistinctRoles;
    case Item.Kind of
      piOutputGroup: CheckPin(Item.Pin,'pin','LED output','addressable-led-output',True);
      piUART: begin CheckPin(Item.Pin,'pin','TX','uart-tx',True); CheckPin(Item.SecondaryPin,'secondary_pin','RX','uart-rx',True); end;
      piCANBus: begin CheckPin(Item.Pin,'pin','TX','can-tx',True); CheckPin(Item.SecondaryPin,'secondary_pin','RX','can-rx',True); end;
      piI2C: begin CheckPin(Item.Pin,'pin','SDA','i2c-sda',True); CheckPin(Item.SecondaryPin,'secondary_pin','SCL','i2c-scl',True); end;
      piSPI: begin CheckPin(Item.Pin,'pin','MOSI','spi-mosi',True); CheckPin(Item.SecondaryPin,'secondary_pin','MISO','spi-miso',True);
        CheckPin(Item.ClockPin,'clock_pin','Clock','spi-clock',True); CheckPin(Item.SelectPin,'select_pin','Select','spi-select',True); end;
    else CheckPin(Item.Pin,'pin','Pin','',False); end;
  end;
  if NeedsTemplate and not Context.HasTemplate then Report.Add(vsInfo,'template.unavailable','Load a PCB template to check pin availability and capabilities');
end;
function TERDPCBValidator.Validate(const Context: TERDPCBValidationContext): TArray<TERDPCBValidationIssue>;
var Snapshot: TERDPCBValidationContext; Report: TERDPCBValidationReport;
begin
  Snapshot := CopyContext(Context); Report := TERDPCBValidationReport.Create;
  try BuiltInRules(Snapshot,Report); if Assigned(FOnValidate) then FOnValidate(Self,Snapshot,Report); Result := Report.Issues;
  finally Report.Free; end;
end;
end.
