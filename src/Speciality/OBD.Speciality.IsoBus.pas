//------------------------------------------------------------------------------
//  OBD.Speciality.IsoBus
//
//  TOBDIsoBus — non-visual ISO 11783 (IsoBus) client. Provides the
//  base-protocol surface every IsoBus stack needs:
//
//    - 64-bit NAME encoding / decoding (ISO 11783-5 §4.4)
//    - Address-claim handshake (PGN 0xEE00 broadcast)
//    - PGN request (PGN 0xEA00) helper
//    - Source-address ↔ NAME registry for the application layer
//
//  IsoBus is layered on top of J1939, which is already covered by
//  the Phase 4c TOBDJ1939SessionManager. This component sits above
//  that layer and consumes raw J1939 PGN frames; hosts wire it to
//  their CAN driver via a small adapter.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 11783-3:2018 (Data link layer)
//    - ISO 11783-5:2019 (Network management)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Speciality.IsoBus;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types;

const
  /// <summary>Address claim PGN.</summary>
  ISOBUS_PGN_ADDRESS_CLAIM = $00EE00;
  /// <summary>Request-for-PGN PGN.</summary>
  ISOBUS_PGN_REQUEST       = $00EA00;
  /// <summary>"Cannot claim" / null address.</summary>
  ISOBUS_NULL_ADDRESS      = $FE;
  /// <summary>Global broadcast address.</summary>
  ISOBUS_GLOBAL_ADDRESS    = $FF;

  // ---- Industry groups (ISO 11783-5 §4.4.1.1) ----
  ISOBUS_INDUSTRY_GLOBAL    = 0;
  ISOBUS_INDUSTRY_HIGHWAY   = 1;
  ISOBUS_INDUSTRY_AGRO      = 2;
  ISOBUS_INDUSTRY_CONSTRUCT = 3;
  ISOBUS_INDUSTRY_MARINE    = 4;
  ISOBUS_INDUSTRY_INDUSTRIAL= 5;

type
  /// <summary>Decoded 64-bit NAME field (ISO 11783-5 §4.4).</summary>
  TOBDIsoBusName = record
    /// <summary>Identity number (21 bits — vendor-managed serial).</summary>
    IdentityNumber: Cardinal;
    /// <summary>Manufacturer code (11 bits — ISO 11783 registry).</summary>
    ManufacturerCode: Word;
    /// <summary>ECU instance (3 bits, 0..7).</summary>
    EcuInstance: Byte;
    /// <summary>Function instance (5 bits, 0..31).</summary>
    FunctionInstance: Byte;
    /// <summary>Function (8 bits — defined per industry group).</summary>
    Function_: Byte;
    /// <summary>Reserved bit.</summary>
    Reserved: Byte;
    /// <summary>Device class (7 bits).</summary>
    DeviceClass: Byte;
    /// <summary>Device class instance (4 bits).</summary>
    DeviceClassInstance: Byte;
    /// <summary>Industry group (3 bits, ISOBUS_INDUSTRY_*).</summary>
    IndustryGroup: Byte;
    /// <summary>Self-configurable address bit.</summary>
    SelfConfigurableAddress: Boolean;
  end;

  /// <summary>One claimed address with its NAME.</summary>
  TOBDIsoBusClaim = record
    Address: Byte;
    Name: TOBDIsoBusName;
  end;

  /// <summary>Fires when the local stack wins or loses a claim.</summary>
  TOBDIsoBusClaimEvent = procedure(Sender: TObject;
    AAddress: Byte; const AName: TOBDIsoBusName) of object;

  /// <summary>
  ///   IsoBus base-protocol component. Maintains the
  ///   address ↔ NAME registry and exposes encoding helpers; the
  ///   actual CAN frame TX/RX is the host's responsibility.
  /// </summary>
  TOBDIsoBus = class(TComponent)
  strict private
    FLock: TCriticalSection;
    FRegistry: TDictionary<Byte, TOBDIsoBusName>;
    FLocalName: TOBDIsoBusName;
    FLocalAddress: Byte;
    FOnClaim: TOBDIsoBusClaimEvent;
    FOnLost: TOBDIsoBusClaimEvent;
    procedure FireClaim(AAddress: Byte; const AName: TOBDIsoBusName);
    procedure FireLost(AAddress: Byte; const AName: TOBDIsoBusName);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Encodes a NAME to its 8-byte LSB-first wire form.</summary>
    class function EncodeName(const AName: TOBDIsoBusName): TBytes; static;
    /// <summary>Decodes 8 wire bytes into a NAME.</summary>
    class function DecodeName(const ABytes: TBytes;
      out AName: TOBDIsoBusName): Boolean; static;
    /// <summary>Compares two NAMEs by priority per ISO 11783-5
    /// §4.4.4: lower numeric value wins.</summary>
    class function NameLessThan(const A, B: TOBDIsoBusName): Boolean; static;

    /// <summary>Builds the 8-byte payload for an Address Claim
    /// frame. The host wires it onto a CAN frame with PGN 0x00EE00,
    /// priority 6, source = candidate address, dest = 0xFF.</summary>
    function BuildAddressClaim: TBytes;

    /// <summary>Feeds a received Address Claim into the registry.
    /// Returns True when the local stack must yield its address
    /// (a higher-priority NAME claimed the same address); call
    /// <c>BuildAddressClaim</c> with a new candidate after that.</summary>
    function HandleAddressClaim(ASourceAddress: Byte;
      const APayload: TBytes): Boolean;

    /// <summary>True when <c>AAddress</c> is currently claimed in
    /// the registry.</summary>
    function IsClaimed(AAddress: Byte): Boolean;
    /// <summary>Returns the NAME claimed at <c>AAddress</c>; False
    /// when unclaimed.</summary>
    function TryGetName(AAddress: Byte;
      out AName: TOBDIsoBusName): Boolean;

    /// <summary>Builds the 3-byte payload for a Request-PGN frame
    /// (ISO 11783-3 §5.4.5).</summary>
    class function BuildPGNRequest(APGN: Cardinal): TBytes; static;

    /// <summary>Configures the local NAME used for outbound
    /// claims.</summary>
    property LocalName: TOBDIsoBusName read FLocalName write FLocalName;
    /// <summary>Currently-held local source address. <c>0xFE</c>
    /// when not yet claimed.</summary>
    property LocalAddress: Byte read FLocalAddress write FLocalAddress;
  published
    /// <summary>Fires when an address is added to the registry
    /// (main thread).</summary>
    property OnClaim: TOBDIsoBusClaimEvent read FOnClaim write FOnClaim;
    /// <summary>Fires when the local stack loses an address
    /// to a higher-priority claim.</summary>
    property OnAddressLost: TOBDIsoBusClaimEvent read FOnLost write FOnLost;
  end;

implementation

{ ---- helpers ---------------------------------------------------------------- }

function NameToUInt64(const AName: TOBDIsoBusName): UInt64;
begin
  // Layout per §4.4 (LSB to MSB):
  //   bits 0..20   identity number (21)
  //   bits 21..31  manufacturer code (11)
  //   bits 32..34  ECU instance (3)
  //   bits 35..39  function instance (5)
  //   bits 40..47  function (8)
  //   bit  48      reserved
  //   bits 49..55  device class (7)
  //   bits 56..59  device class instance (4)
  //   bits 60..62  industry group (3)
  //   bit  63      self-configurable address
  Result :=
    (UInt64(AName.IdentityNumber and $1FFFFF))            or
    (UInt64(AName.ManufacturerCode and $7FF) shl 21)      or
    (UInt64(AName.EcuInstance and $07) shl 32)            or
    (UInt64(AName.FunctionInstance and $1F) shl 35)       or
    (UInt64(AName.Function_) shl 40)                      or
    (UInt64(AName.Reserved and $01) shl 48)               or
    (UInt64(AName.DeviceClass and $7F) shl 49)            or
    (UInt64(AName.DeviceClassInstance and $0F) shl 56)    or
    (UInt64(AName.IndustryGroup and $07) shl 60)          or
    (UInt64(Byte(AName.SelfConfigurableAddress)) shl 63);
end;

function UInt64ToName(AValue: UInt64): TOBDIsoBusName;
begin
  Result.IdentityNumber          := Cardinal(AValue and $1FFFFF);
  Result.ManufacturerCode        := Word((AValue shr 21) and $7FF);
  Result.EcuInstance             := Byte((AValue shr 32) and $07);
  Result.FunctionInstance        := Byte((AValue shr 35) and $1F);
  Result.Function_               := Byte((AValue shr 40) and $FF);
  Result.Reserved                := Byte((AValue shr 48) and $01);
  Result.DeviceClass             := Byte((AValue shr 49) and $7F);
  Result.DeviceClassInstance     := Byte((AValue shr 56) and $0F);
  Result.IndustryGroup           := Byte((AValue shr 60) and $07);
  Result.SelfConfigurableAddress := ((AValue shr 63) and $01) <> 0;
end;

{ ---- TOBDIsoBus ------------------------------------------------------------- }

constructor TOBDIsoBus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FRegistry := TDictionary<Byte, TOBDIsoBusName>.Create;
  FLocalAddress := ISOBUS_NULL_ADDRESS;
end;

destructor TOBDIsoBus.Destroy;
begin
  FRegistry.Free;
  FLock.Free;
  inherited;
end;

class function TOBDIsoBus.EncodeName(
  const AName: TOBDIsoBusName): TBytes;
var
  V: UInt64;
  I: Integer;
begin
  V := NameToUInt64(AName);
  SetLength(Result, 8);
  for I := 0 to 7 do
    Result[I] := Byte((V shr (8 * I)) and $FF); // LSB first
end;

class function TOBDIsoBus.DecodeName(const ABytes: TBytes;
  out AName: TOBDIsoBusName): Boolean;
var
  V: UInt64;
  I: Integer;
begin
  AName := Default(TOBDIsoBusName);
  if Length(ABytes) < 8 then Exit(False);
  V := 0;
  for I := 7 downto 0 do
    V := (V shl 8) or ABytes[I];
  AName := UInt64ToName(V);
  Result := True;
end;

class function TOBDIsoBus.NameLessThan(const A, B: TOBDIsoBusName): Boolean;
begin
  Result := NameToUInt64(A) < NameToUInt64(B);
end;

function TOBDIsoBus.BuildAddressClaim: TBytes;
begin
  Result := EncodeName(FLocalName);
end;

function TOBDIsoBus.HandleAddressClaim(ASourceAddress: Byte;
  const APayload: TBytes): Boolean;
var
  Incoming: TOBDIsoBusName;
  Existing: TOBDIsoBusName;
  MustYield: Boolean;
begin
  Result := False;
  if not DecodeName(APayload, Incoming) then Exit;
  MustYield := False;
  FLock.Enter;
  try
    if FRegistry.TryGetValue(ASourceAddress, Existing) then
    begin
      // Conflict — higher-priority NAME wins.
      if NameLessThan(Incoming, Existing) then
        FRegistry.AddOrSetValue(ASourceAddress, Incoming);
    end
    else
      FRegistry.AddOrSetValue(ASourceAddress, Incoming);

    if (FLocalAddress = ASourceAddress) and
       NameLessThan(Incoming, FLocalName) then
    begin
      // The remote claim outranks our own.
      MustYield := True;
      FLocalAddress := ISOBUS_NULL_ADDRESS;
    end;
  finally
    FLock.Leave;
  end;
  if MustYield then
  begin
    FireLost(ASourceAddress, Incoming);
    Result := True;
  end
  else
    FireClaim(ASourceAddress, Incoming);
end;

function TOBDIsoBus.IsClaimed(AAddress: Byte): Boolean;
begin
  FLock.Enter;
  try Result := FRegistry.ContainsKey(AAddress);
  finally FLock.Leave; end;
end;

function TOBDIsoBus.TryGetName(AAddress: Byte;
  out AName: TOBDIsoBusName): Boolean;
begin
  FLock.Enter;
  try Result := FRegistry.TryGetValue(AAddress, AName);
  finally FLock.Leave; end;
end;

class function TOBDIsoBus.BuildPGNRequest(APGN: Cardinal): TBytes;
begin
  SetLength(Result, 3);
  Result[0] := Byte(APGN and $FF);
  Result[1] := Byte((APGN shr 8) and $FF);
  Result[2] := Byte((APGN shr 16) and $FF);
end;

procedure TOBDIsoBus.FireClaim(AAddress: Byte;
  const AName: TOBDIsoBusName);
var
  Self_: TOBDIsoBus; Addr: Byte; N: TOBDIsoBusName;
begin
  if not Assigned(FOnClaim) then Exit;
  Self_ := Self; Addr := AAddress; N := AName;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnClaim(Self_, Addr, N)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnClaim) then Self_.FOnClaim(Self_, Addr, N);
    end);
end;

procedure TOBDIsoBus.FireLost(AAddress: Byte;
  const AName: TOBDIsoBusName);
var
  Self_: TOBDIsoBus; Addr: Byte; N: TOBDIsoBusName;
begin
  if not Assigned(FOnLost) then Exit;
  Self_ := Self; Addr := AAddress; N := AName;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnLost(Self_, Addr, N)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnLost) then Self_.FOnLost(Self_, Addr, N);
    end);
end;

end.
