//------------------------------------------------------------------------------
// UNIT           : OBD.J1939.PGNs.pas
// CONTENTS       : SAE J1939-71/73/75 named-PGN catalog
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.J1939.PGNs;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  TJ1939PGNDescriptor = record
    PGN: UInt32;
    Mnemonic: string;
    Name: string;
    LengthBytes: Integer;
    DefaultPriority: Byte;
    TxRateMs: Integer;
    SpecSection: string;
  end;

/// <summary>Look up a PGN by id. Returns a zero record when not found;
/// callers can check Result.PGN &lt;&gt; 0.</summary>
function FindPGN(const PGN: UInt32): TJ1939PGNDescriptor;

/// <summary>Register a custom PGN (e.g. for OEM-specific extensions).
/// Replaces an existing entry with the same id.</summary>
procedure RegisterJ1939PGN(const Desc: TJ1939PGNDescriptor);

/// <summary>Total entries in the registry (catalog + registered).</summary>
function J1939PGNCount: Integer;

/// <summary>Iterate all PGNs in ascending order.</summary>
function J1939PGNAll: TArray<TJ1939PGNDescriptor>;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

const
  CatalogFileName = 'j1939-pgns.json';

var
  GPGNs: TList<TJ1939PGNDescriptor>;

function FindPGNIndex(PGN: UInt32; out Idx: Integer): Boolean;
var
  Lo, Hi, Mid: Integer;
  V: UInt32;
begin
  Lo := 0; Hi := GPGNs.Count - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) shr 1;
    V := GPGNs[Mid].PGN;
    if V = PGN then begin Idx := Mid; Exit(True); end
    else if V < PGN then Lo := Mid + 1
    else Hi := Mid - 1;
  end;
  Idx := -1;
  Result := False;
end;

procedure SortByPGN;
begin
  GPGNs.Sort(TComparer<TJ1939PGNDescriptor>.Construct(
    function(const A, B: TJ1939PGNDescriptor): Integer
    begin
      if A.PGN < B.PGN then Result := -1
      else if A.PGN > B.PGN then Result := 1
      else Result := 0;
    end));
end;

function ParseHexUInt32(const S: string; out V: UInt32): Boolean;
var
  T: string;
  I64: Int64;
begin
  T := S;
  if T.StartsWith('0x', True) then T := '$' + T.Substring(2);
  Result := TryStrToInt64(T, I64) and (I64 >= 0) and (I64 <= $FFFFFFFF);
  if Result then V := UInt32(I64);
end;

procedure LoadCatalog;
var
  Path, Raw: string;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  D: TJ1939PGNDescriptor;
  Stream: TStringStream;
begin
  Path := ResolveCatalogPath(CatalogFileName);
  if Path = '' then Exit;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    Stream.Free;
  end;
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      D := Default(TJ1939PGNDescriptor);
      if not ParseHexUInt32(Obj.GetValue<string>('pgn', ''), D.PGN) then Continue;
      D.Mnemonic        := Obj.GetValue<string>('mnemonic', '');
      D.Name            := Obj.GetValue<string>('name', '');
      D.LengthBytes     := Obj.GetValue<Integer>('length_bytes', 0);
      D.DefaultPriority := Byte(Obj.GetValue<Integer>('default_priority', 6));
      D.TxRateMs        := Obj.GetValue<Integer>('tx_rate_ms', 0);
      D.SpecSection     := Obj.GetValue<string>('spec_section', '');
      GPGNs.Add(D);
    end;
  finally
    Doc.Free;
  end;
end;

function FindPGN(const PGN: UInt32): TJ1939PGNDescriptor;
var Idx: Integer;
begin
  if FindPGNIndex(PGN, Idx) then Result := GPGNs[Idx]
  else Result := Default(TJ1939PGNDescriptor);
end;

procedure RegisterJ1939PGN(const Desc: TJ1939PGNDescriptor);
var Idx: Integer;
begin
  if FindPGNIndex(Desc.PGN, Idx) then GPGNs[Idx] := Desc
  else
  begin
    GPGNs.Add(Desc);
    SortByPGN;
  end;
end;

function J1939PGNCount: Integer;
begin Result := GPGNs.Count; end;

function J1939PGNAll: TArray<TJ1939PGNDescriptor>;
begin Result := GPGNs.ToArray; end;

initialization
  GPGNs := TList<TJ1939PGNDescriptor>.Create;
  LoadCatalog;
  SortByPGN;

finalization
  GPGNs.Free;

end.
