//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Diff.pas
// CONTENTS       : Coding diff and dry-run with explicit confirm
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Diff;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  OBD.OEM.Coding;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDCodingDiffError = class(Exception);

  /// <summary>
  ///   Optional named-field schema. Each entry describes a bit-
  ///   or byte-range in the coding payload so the diff can render
  ///   human-readable field changes ("LongCoding[7].bit3: false -> true:
  ///   CornerLights") rather than just byte indices.
  /// </summary>
  TOBDCodingFieldKind = (cfkBit, cfkByte, cfkUInt16);

  TOBDCodingFieldSchema = record
    /// <summary>
    ///   Name.
    /// </summary>
    Name: string;
    /// <summary>
    ///   Description.
    /// </summary>
    Description: string;
    /// <summary>
    ///   Kind.
    /// </summary>
    Kind: TOBDCodingFieldKind;
    /// <summary>
    ///   Byte index.
    /// </summary>
    ByteIndex: Integer;
    BitIndex: Integer;       // valid only for cfkBit
  end;

  TOBDCodingSchema = TArray<TOBDCodingFieldSchema>;

  /// <summary>
  ///   One diff entry — either field-level (when Schema supplied)
  ///   or byte-level (no schema).
  /// </summary>
  TOBDCodingDiffEntry = record
    FieldName: string;       // empty when byte-level
    Description: string;     // empty when byte-level
    /// <summary>
    ///   Byte index.
    /// </summary>
    ByteIndex: Integer;
    BitIndex: Integer;       // -1 for byte/uint16 entries
    /// <summary>
    ///   Before value.
    /// </summary>
    BeforeValue: UInt32;
    /// <summary>
    ///   After value.
    /// </summary>
    AfterValue: UInt32;
    /// <summary>
    ///   As text.
    /// </summary>
    function AsText: string;
  end;

  TOBDCodingDiff = TArray<TOBDCodingDiffEntry>;

  /// <summary>
  ///   Callback invoked by Plan.Apply when the caller confirms
  ///   the write. Implementations typically wrap the OEM-specific
  ///   WriteDataByIdentifier (UDS 0x2E) call. Raise on failure; the plan
  ///   catches and reports through Last write outcome.
  /// </summary>
  TOBDCodingWriter = reference to procedure(const Bytes: TBytes);

  /// <summary>
  ///   Holds a snapshot pair + diff. Apply is a no-op unless the
  ///   caller passes Confirmed=True, encoding the human in the loop into
  ///   the type signature.
  /// </summary>
  TOBDCodingPlan = class
  private
    FCurrent: TBytes;
    FTarget: TBytes;
    FSchema: TOBDCodingSchema;
    FDiff: TOBDCodingDiff;
    FApplied: Boolean;
    /// <summary>
    ///   Compute diff.
    /// </summary>
    procedure ComputeDiff;
  public
    /// <summary>
    ///   Create.
    /// </summary>
    constructor Create(const Current, Target: TBytes;
      const Schema: TOBDCodingSchema = nil);
    /// <summary>
    ///   Destroy.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Is no op.
    /// </summary>
    function IsNoOp: Boolean;
    /// <summary>
    ///   As text.
    /// </summary>
    function AsText: string;
    /// <summary>
    ///   Apply.
    /// </summary>
    procedure Apply(Confirmed: Boolean; const Writer: TOBDCodingWriter);

    /// <summary>
    ///   Current.
    /// </summary>
    property Current: TBytes read FCurrent;
    /// <summary>
    ///   Target.
    /// </summary>
    property Target: TBytes read FTarget;
    /// <summary>
    ///   Diff.
    /// </summary>
    property Diff: TOBDCodingDiff read FDiff;
    /// <summary>
    ///   Applied.
    /// </summary>
    property Applied: Boolean read FApplied;
  end;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

{ TOBDCodingDiffEntry }

//------------------------------------------------------------------------------
// AS TEXT
//------------------------------------------------------------------------------
function TOBDCodingDiffEntry.AsText: string;
begin
  if FieldName <> '' then
  begin
    if BitIndex >= 0 then
      Result := Format('%s [byte %d bit %d]: %d -> %d',
        [FieldName, ByteIndex, BitIndex, BeforeValue, AfterValue])
    else
      Result := Format('%s [byte %d]: 0x%.2x -> 0x%.2x',
        [FieldName, ByteIndex, BeforeValue, AfterValue]);
    if Description <> '' then
      Result := Result + ' (' + Description + ')';
  end
  else
    Result := Format('byte %d: 0x%.2x -> 0x%.2x',
      [ByteIndex, BeforeValue, AfterValue]);
end;

{ TOBDCodingPlan }

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDCodingPlan.Create(const Current, Target: TBytes;
  const Schema: TOBDCodingSchema);
begin
  // Initialize the inherited class
  inherited Create;
  if Length(Current) <> Length(Target) then
    raise EOBDCodingDiffError.CreateFmt(
      'Coding plan mismatch: current=%d bytes, target=%d bytes',
      [Length(Current), Length(Target)]);
  FCurrent := Copy(Current);
  FTarget := Copy(Target);
  FSchema := Schema;
  ComputeDiff;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDCodingPlan.Destroy;
begin
  // Call the inherited handler
  inherited;
end;

//------------------------------------------------------------------------------
// COMPUTE DIFF
//------------------------------------------------------------------------------
procedure TOBDCodingPlan.ComputeDiff;
var
  I: Integer;
  Field: TOBDCodingFieldSchema;
  Entry: TOBDCodingDiffEntry;
  EntryList: TList<TOBDCodingDiffEntry>;
  BeforeBit, AfterBit: Boolean;
begin
  // Create EntryList
  EntryList := TList<TOBDCodingDiffEntry>.Create;
  try
    if Length(FSchema) > 0 then
    begin
      // Field-level diff: walk the schema.
      for Field in FSchema do
      begin
        Entry := Default(TOBDCodingDiffEntry);
        Entry.FieldName := Field.Name;
        Entry.Description := Field.Description;
        Entry.ByteIndex := Field.ByteIndex;
        Entry.BitIndex := -1;
        case Field.Kind of
          cfkBit:
            begin
              if (Field.ByteIndex < 0) or (Field.ByteIndex > High(FCurrent)) then
                Continue;
              BeforeBit := GetBit(FCurrent, Field.ByteIndex, Field.BitIndex);
              AfterBit := GetBit(FTarget, Field.ByteIndex, Field.BitIndex);
              if BeforeBit = AfterBit then Continue;
              Entry.BitIndex := Field.BitIndex;
              Entry.BeforeValue := UInt32(Ord(BeforeBit));
              Entry.AfterValue := UInt32(Ord(AfterBit));
            end;
          cfkByte:
            begin
              if (Field.ByteIndex < 0) or (Field.ByteIndex > High(FCurrent)) then
                Continue;
              if FCurrent[Field.ByteIndex] = FTarget[Field.ByteIndex] then Continue;
              Entry.BeforeValue := FCurrent[Field.ByteIndex];
              Entry.AfterValue := FTarget[Field.ByteIndex];
            end;
          cfkUInt16:
            begin
              if (Field.ByteIndex < 0) or (Field.ByteIndex + 1 > High(FCurrent)) then
                Continue;
              Entry.BeforeValue := (UInt32(FCurrent[Field.ByteIndex]) shl 8)
                                 or FCurrent[Field.ByteIndex + 1];
              Entry.AfterValue := (UInt32(FTarget[Field.ByteIndex]) shl 8)
                                 or FTarget[Field.ByteIndex + 1];
              if Entry.BeforeValue = Entry.AfterValue then Continue;
            end;
        end;
        EntryList.Add(Entry);
      end;
    end
    else
    begin
      // Byte-level fallback when no schema is supplied.
      for I := 0 to High(FCurrent) do
        if FCurrent[I] <> FTarget[I] then
        begin
          Entry := Default(TOBDCodingDiffEntry);
          Entry.ByteIndex := I;
          Entry.BitIndex := -1;
          Entry.BeforeValue := FCurrent[I];
          Entry.AfterValue := FTarget[I];
          EntryList.Add(Entry);
        end;
    end;
    FDiff := EntryList.ToArray;
  finally
    // Free EntryList
    EntryList.Free;
  end;
end;

//------------------------------------------------------------------------------
// IS NO OP
//------------------------------------------------------------------------------
function TOBDCodingPlan.IsNoOp: Boolean;
begin
  Result := Length(FDiff) = 0;
end;

//------------------------------------------------------------------------------
// AS TEXT
//------------------------------------------------------------------------------
function TOBDCodingPlan.AsText: string;
var
  Entry: TOBDCodingDiffEntry;
  Buf: TStringBuilder;
begin
  if IsNoOp then Exit('Coding plan is a no-op (no fields differ).');
  // Create Buf
  Buf := TStringBuilder.Create;
  try
    Buf.AppendLine(Format('Coding plan: %d field(s) change',
      [Length(FDiff)]));
    // Loop over FDiff
    for Entry in FDiff do
      Buf.AppendLine('  ' + Entry.AsText);
    Result := Buf.ToString;
  finally
    // Free Buf
    Buf.Free;
  end;
end;

//------------------------------------------------------------------------------
// APPLY
//------------------------------------------------------------------------------
procedure TOBDCodingPlan.Apply(Confirmed: Boolean;
  const Writer: TOBDCodingWriter);
begin
  if not Confirmed then
    raise EOBDCodingDiffError.Create(
      'Apply called with Confirmed=False; coding write skipped');
  if not Assigned(Writer) then
    raise EOBDCodingDiffError.Create(
      'Apply requires a non-nil writer callback');
  if IsNoOp then
  begin
    FApplied := True;
    Exit;
  end;
  Writer(FTarget);
  FApplied := True;
end;

end.
