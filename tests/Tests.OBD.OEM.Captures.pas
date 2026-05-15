//------------------------------------------------------------------------------
//  Tests.OBD.OEM.Captures
//
//  Coverage for ExtractCapturePairs + ValidateAgainstExtension.
//  Drives the validator with synthesised TArray<TOBDLogEntry>
//  payloads so no recorder I/O is needed.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.Captures;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.OEM,
  OBD.OEM.Types,
  OBD.OEM.Extensions,
  OBD.OEM.DTC,
  OBD.Recorder,
  OBD.OEM.Captures;

type
  /// <summary>Minimal extension that catalogues a single
  /// DID + supplies a deterministic DecodeDID
  /// implementation.</summary>
  TCaptureStubExtension = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(
      var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
    function DecodeDID(const DID: Word;
      const Payload: TBytes): string; override;
  end;

  [TestFixture]
  TCapturesTests = class
  private
    function MakeFrame(ASid: Byte; const ABody: TBytes;
      AElapsedMs: Cardinal = 0): TOBDLogEntry;
    function MakeResponse(ASid: Byte;
      const ABody: TBytes): TOBDLogEntry;
    function MakeNRC(ASid, ANRC: Byte): TOBDLogEntry;
  public
    /// <summary>A Frame followed by a Response pairs cleanly
    /// and exposes the request DID + payload past the
    /// echoed DID.</summary>
    [Test] procedure Pair_ReadDID_StripsEcho;
    /// <summary>A Frame followed by an NRC entry produces a
    /// pair with IsNegative + NRC.</summary>
    [Test] procedure Pair_NRC_MarkedNegative;
    /// <summary>Two Frames in a row flushes the first as a
    /// dangling-request pair.</summary>
    [Test] procedure Pair_HangingFrame_StillEmitted;
    /// <summary>Validator surfaces DidIsCatalogued + DidName
    /// for the registered DID.</summary>
    [Test] procedure Validate_CataloguedDID_NameReturned;
    /// <summary>Validator passes through non-22 service IDs
    /// without decoder side effects.</summary>
    [Test] procedure Validate_NonReadDID_NotDecoded;
  end;

implementation

{ TCaptureStubExtension }

procedure TCaptureStubExtension.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
var
  DID: TOBDOEMDataIdentifier;
begin
  DID := Default(TOBDOEMDataIdentifier);
  DID.DID := $F190;
  DID.Name := 'vin';
  DID.Description := 'Vehicle Identification Number';
  DIDs := DIDs + [DID];
end;

function TCaptureStubExtension.ManufacturerKey: string;
begin
  Result := 'STUB';
end;

function TCaptureStubExtension.DisplayName: string;
begin
  Result := 'Capture-test stub extension';
end;

function TCaptureStubExtension.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := True;
end;

function TCaptureStubExtension.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  if DID = $F190 then
    Result := 'vin=' + TEncoding.ASCII.GetString(Payload)
  else
    Result := inherited DecodeDID(DID, Payload);
end;

{ TCapturesTests }

function TCapturesTests.MakeFrame(ASid: Byte; const ABody: TBytes;
  AElapsedMs: Cardinal): TOBDLogEntry;
begin
  Result := Default(TOBDLogEntry);
  Result.Kind := leFrame;
  Result.ElapsedMs := AElapsedMs;
  Result.ServiceID := ASid;
  Result.HasServiceID := True;
  SetLength(Result.Raw, 1 + Length(ABody));
  Result.Raw[0] := ASid;
  if Length(ABody) > 0 then
    Move(ABody[0], Result.Raw[1], Length(ABody));
end;

function TCapturesTests.MakeResponse(ASid: Byte;
  const ABody: TBytes): TOBDLogEntry;
begin
  Result := Default(TOBDLogEntry);
  Result.Kind := leResponse;
  Result.ServiceID := ASid + $40;
  Result.HasServiceID := True;
  SetLength(Result.Raw, 1 + Length(ABody));
  Result.Raw[0] := ASid + $40;
  if Length(ABody) > 0 then
    Move(ABody[0], Result.Raw[1], Length(ABody));
end;

function TCapturesTests.MakeNRC(ASid, ANRC: Byte): TOBDLogEntry;
begin
  Result := Default(TOBDLogEntry);
  Result.Kind := leNRC;
  Result.ServiceID := ASid;
  Result.HasServiceID := True;
  Result.NRC := ANRC;
  Result.HasNRC := True;
end;

procedure TCapturesTests.Pair_ReadDID_StripsEcho;
var
  Entries: TArray<TOBDLogEntry>;
  Pairs: TArray<TOBDCapturePair>;
begin
  // Request: 22 F1 90 ; Response: 62 F1 90 'V' 'I' 'N'…
  Entries := Entries + [MakeFrame($22, TBytes.Create($F1, $90), 100)];
  Entries := Entries + [MakeResponse($22,
    TBytes.Create($F1, $90, $56, $49, $4E))];
  Pairs := ExtractCapturePairs(Entries);
  Assert.AreEqual(1, Length(Pairs));
  Assert.AreEqual(Integer($22), Integer(Pairs[0].ServiceID));
  Assert.AreEqual(Integer($F190), Integer(Pairs[0].DID));
  Assert.AreEqual(3, Length(Pairs[0].PayloadBytes));
  Assert.AreEqual(Integer($56), Integer(Pairs[0].PayloadBytes[0]));
end;

procedure TCapturesTests.Pair_NRC_MarkedNegative;
var
  Entries: TArray<TOBDLogEntry>;
  Pairs: TArray<TOBDCapturePair>;
begin
  Entries := Entries + [MakeFrame($22, TBytes.Create($DE, $AD))];
  Entries := Entries + [MakeNRC($22, $31)];
  Pairs := ExtractCapturePairs(Entries);
  Assert.AreEqual(1, Length(Pairs));
  Assert.IsTrue(Pairs[0].IsNegative);
  Assert.AreEqual(Integer($31),
    Integer(Pairs[0].NegativeResponseCode));
end;

procedure TCapturesTests.Pair_HangingFrame_StillEmitted;
var
  Entries: TArray<TOBDLogEntry>;
  Pairs: TArray<TOBDCapturePair>;
begin
  Entries := Entries + [MakeFrame($22, TBytes.Create($F1, $90))];
  Entries := Entries + [MakeFrame($22, TBytes.Create($F1, $A0))];
  Pairs := ExtractCapturePairs(Entries);
  Assert.AreEqual(2, Length(Pairs));
  // First pair was flushed without a response.
  Assert.AreEqual(0, Length(Pairs[0].ResponseBytes));
  Assert.AreEqual(Integer($F190), Integer(Pairs[0].DID));
end;

procedure TCapturesTests.Validate_CataloguedDID_NameReturned;
var
  Ext: IOBDOEMExtension;
  Entries: TArray<TOBDLogEntry>;
  Pairs: TArray<TOBDCapturePair>;
  Decoded: TArray<TOBDCaptureDecoded>;
begin
  Ext := TCaptureStubExtension.Create;
  Entries := Entries + [MakeFrame($22, TBytes.Create($F1, $90))];
  Entries := Entries + [MakeResponse($22,
    TBytes.Create($F1, $90, $56, $49, $4E))];
  Pairs := ExtractCapturePairs(Entries);
  Decoded := ValidateAgainstExtension(Pairs, Ext);
  Assert.AreEqual(1, Length(Decoded));
  Assert.IsTrue(Decoded[0].DidIsCatalogued);
  Assert.AreEqual('vin', Decoded[0].DidName);
  Assert.IsTrue(Pos('vin=VIN', Decoded[0].Display) >= 1);
end;

procedure TCapturesTests.Validate_NonReadDID_NotDecoded;
var
  Ext: IOBDOEMExtension;
  Entries: TArray<TOBDLogEntry>;
  Pairs: TArray<TOBDCapturePair>;
  Decoded: TArray<TOBDCaptureDecoded>;
begin
  Ext := TCaptureStubExtension.Create;
  // RoutineControl request — not a 22 read.
  Entries := Entries + [MakeFrame($31,
    TBytes.Create($01, $03, $01))];
  Entries := Entries + [MakeResponse($31,
    TBytes.Create($01, $03, $01))];
  Pairs := ExtractCapturePairs(Entries);
  Decoded := ValidateAgainstExtension(Pairs, Ext);
  Assert.AreEqual(1, Length(Decoded));
  Assert.IsFalse(Decoded[0].DidIsCatalogued);
  Assert.AreEqual('', Decoded[0].DidName);
  Assert.AreEqual('', Decoded[0].Display);
end;

initialization
  TDUnitX.RegisterTestFixture(TCapturesTests);

end.
