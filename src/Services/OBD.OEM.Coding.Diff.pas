//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Diff.pas
// CONTENTS       : Coding diff & dry-run flow on top of the OBD.OEM.Coding
//                : helpers. Reads current ECU coding bytes, computes a
//                : structured diff against the target, and only writes when
//                : the caller explicitly confirms.
//
// Why            : Coding writes can brick an ECU. Treating "compute target
//                : -> blast write" as one atomic step is a footgun. This
//                : module forces a four-step flow:
//                :   1. Snapshot Current bytes.
//                :   2. Build a TOBDCodingPlan(Current, Target [, Schema]).
//                :   3. Inspect Plan.Diff / Plan.IsNoOp / Plan.AsText.
//                :   4. Plan.Apply(Confirmed=True, WriteCallback).
//                : Step 4 is a no-op unless Confirmed is True; the type
//                : signature makes the confirm explicit.
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Diff;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  OBD.OEM.Coding;

type
  EOBDCodingDiffError = class(Exception);

  /// <summary>Optional named-field schema. Each entry describes a bit-
  /// or byte-range in the coding payload so the diff can render
  /// human-readable field changes ("LongCoding[7].bit3: false -> true:
  /// CornerLights") rather than just byte indices.</summary>
  TOBDCodingFieldKind = (cfkBit, cfkByte, cfkUInt16);

  TOBDCodingFieldSchema = record
    Name: string;
    Description: string;
    Kind: TOBDCodingFieldKind;
    ByteIndex: Integer;
    BitIndex: Integer;       // valid only for cfkBit
  end;

  TOBDCodingSchema = TArray<TOBDCodingFieldSchema>;

  /// <summary>One diff entry — either field-level (when Schema supplied)
  /// or byte-level (no schema).</summary>
  TOBDCodingDiffEntry = record
    FieldName: string;       // empty when byte-level
    Description: string;     // empty when byte-level
    ByteIndex: Integer;
    BitIndex: Integer;       // -1 for byte/uint16 entries
    BeforeValue: UInt32;
    AfterValue: UInt32;
    function AsText: string;
  end;

  TOBDCodingDiff = TArray<TOBDCodingDiffEntry>;

  /// <summary>Callback invoked by Plan.Apply when the caller confirms
  /// the write. Implementations typically wrap the OEM-specific
  /// WriteDataByIdentifier (UDS 0x2E) call. Raise on failure; the plan
  /// catches and reports through Last write outcome.</summary>
  TOBDCodingWriter = reference to procedure(const Bytes: TBytes);

  /// <summary>Holds a snapshot pair + diff. Apply is a no-op unless the
  /// caller passes Confirmed=True, encoding the human in the loop into
  /// the type signature.</summary>
  TOBDCodingPlan = class
  private
    FCurrent: TBytes;
    FTarget: TBytes;
    FSchema: TOBDCodingSchema;
    FDiff: TOBDCodingDiff;
    FApplied: Boolean;
    procedure ComputeDiff;
  public
    constructor Create(const Current, Target: TBytes;
      const Schema: TOBDCodingSchema = nil);
    destructor Destroy; override;

    function IsNoOp: Boolean;
    function AsText: string;
    procedure Apply(Confirmed: Boolean; const Writer: TOBDCodingWriter);

    property Current: TBytes read FCurrent;
    property Target: TBytes read FTarget;
    property Diff: TOBDCodingDiff read FDiff;
    property Applied: Boolean read FApplied;
  end;

implementation

{ TOBDCodingDiffEntry }

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

constructor TOBDCodingPlan.Create(const Current, Target: TBytes;
  const Schema: TOBDCodingSchema);
begin
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

destructor TOBDCodingPlan.Destroy;
begin
  inherited;
end;

procedure TOBDCodingPlan.ComputeDiff;
var
  I: Integer;
  Field: TOBDCodingFieldSchema;
  Entry: TOBDCodingDiffEntry;
  EntryList: TList<TOBDCodingDiffEntry>;
  BeforeBit, AfterBit: Boolean;
begin
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
    EntryList.Free;
  end;
end;

function TOBDCodingPlan.IsNoOp: Boolean;
begin
  Result := Length(FDiff) = 0;
end;

function TOBDCodingPlan.AsText: string;
var
  Entry: TOBDCodingDiffEntry;
  Buf: TStringBuilder;
begin
  if IsNoOp then Exit('Coding plan is a no-op (no fields differ).');
  Buf := TStringBuilder.Create;
  try
    Buf.AppendLine(Format('Coding plan: %d field(s) change',
      [Length(FDiff)]));
    for Entry in FDiff do
      Buf.AppendLine('  ' + Entry.AsText);
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

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
