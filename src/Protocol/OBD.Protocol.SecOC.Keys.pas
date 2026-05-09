//------------------------------------------------------------------------------
//  OBD.Protocol.SecOC.Keys
//
//  In-memory SecOC key store. AUTOSAR SecOC binds keys to a Data ID
//  (16-bit identifier carried implicitly by the underlying message
//  layer; on UDS this is typically the SID + DID, on CAN it's the
//  CAN ID). Each binding carries:
//
//    - The 128-bit AES key.
//    - The truncated-MAC length on the wire (in bits).
//    - The truncated-freshness-value length on the wire (in bits).
//
//  Hosts that want a hardware-backed key store (HSM / TPM) implement
//  IOBDSecOCKeyProvider externally and pass the instance to the
//  codec.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4e initial.
//------------------------------------------------------------------------------

unit OBD.Protocol.SecOC.Keys;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.SecOC.AES;

type
  /// <summary>SecOC binding for one Data ID.</summary>
  TOBDSecOCBinding = record
    /// <summary>16-bit Data ID (AUTOSAR SecOC freshness/key tag).</summary>
    DataID: Word;
    /// <summary>128-bit AES key.</summary>
    Key: TAES128Key;
    /// <summary>Truncated-MAC length on the wire, in bits. Common
    /// SecOC values: 24, 28, 56, 64. Range <c>[8..128]</c>.</summary>
    TagBits: Byte;
    /// <summary>Truncated-freshness length on the wire, in bits.
    /// Common values: 4, 8, 16, 24. Range <c>[1..32]</c>.</summary>
    FreshnessBits: Byte;
  end;

  /// <summary>
  ///   Provider contract for SecOC bindings. Implement this
  ///   interface to back the codec with a hardware key store
  ///   (TPM / HSM / vehicle key fob).
  /// </summary>
  IOBDSecOCKeyProvider = interface
    ['{6F1A2D3B-9C4E-4FA1-8C39-7B2A5D6E1F90}']
    /// <summary>
    ///   Resolves the binding for a Data ID. Returns False when
    ///   the Data ID is unknown.
    /// </summary>
    /// <param name="ADataID">Data ID to look up.</param>
    /// <param name="ABinding">Output binding. Untouched on miss.</param>
    function TryGet(ADataID: Word;
      out ABinding: TOBDSecOCBinding): Boolean;
  end;

  /// <summary>In-memory implementation of
  /// <see cref="IOBDSecOCKeyProvider"/>. Thread-safe.</summary>
  TOBDSecOCKeyStore = class(TInterfacedObject, IOBDSecOCKeyProvider)
  strict private
    FLock: TCriticalSection;
    FBindings: TDictionary<Word, TOBDSecOCBinding>;
  public
    /// <summary>Creates an empty store.</summary>
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Registers a binding. Replaces any existing entry for the
    ///   same Data ID.
    /// </summary>
    /// <param name="ABinding">Fully populated binding.</param>
    /// <exception cref="EOBDConfig">Tag or freshness bit length out
    /// of range.</exception>
    procedure Register(const ABinding: TOBDSecOCBinding);

    /// <summary>
    ///   Convenience: register a Data ID + key + lengths.
    /// </summary>
    /// <param name="ADataID">16-bit Data ID.</param>
    /// <param name="AKey">128-bit AES key.</param>
    /// <param name="ATagBits">Truncated-MAC length on the wire.</param>
    /// <param name="AFreshnessBits">Truncated-freshness length on
    /// the wire.</param>
    procedure RegisterKey(ADataID: Word; const AKey: TAES128Key;
      ATagBits: Byte = 64; AFreshnessBits: Byte = 16);

    /// <summary>Removes the binding for a Data ID. No-op when the
    /// Data ID is unknown.</summary>
    procedure Unregister(ADataID: Word);

    /// <summary>Removes all bindings. Useful on session teardown.</summary>
    procedure Clear;

    /// <summary>Number of registered bindings.</summary>
    function Count: Integer;

    function TryGet(ADataID: Word;
      out ABinding: TOBDSecOCBinding): Boolean;
  end;

implementation

constructor TOBDSecOCKeyStore.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FBindings := TDictionary<Word, TOBDSecOCBinding>.Create;
end;

destructor TOBDSecOCKeyStore.Destroy;
begin
  FBindings.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDSecOCKeyStore.Register(const ABinding: TOBDSecOCBinding);
begin
  if (ABinding.TagBits < 8) or (ABinding.TagBits > 128) then
    raise EOBDConfig.CreateFmt(
      'SecOC binding (Data ID 0x%4.4X): TagBits %d out of [8..128]',
      [ABinding.DataID, ABinding.TagBits]);
  if (ABinding.FreshnessBits < 1) or (ABinding.FreshnessBits > 32) then
    raise EOBDConfig.CreateFmt(
      'SecOC binding (Data ID 0x%4.4X): FreshnessBits %d out of [1..32]',
      [ABinding.DataID, ABinding.FreshnessBits]);
  // v1 codec only supports byte-aligned truncation. Reject early
  // to keep the diagnostic local to registration.
  if (ABinding.TagBits mod 8) <> 0 then
    raise EOBDConfig.CreateFmt(
      'SecOC binding (Data ID 0x%4.4X): TagBits %d must be a multiple of 8',
      [ABinding.DataID, ABinding.TagBits]);
  if (ABinding.FreshnessBits mod 8) <> 0 then
    raise EOBDConfig.CreateFmt(
      'SecOC binding (Data ID 0x%4.4X): FreshnessBits %d must be a multiple of 8',
      [ABinding.DataID, ABinding.FreshnessBits]);
  FLock.Enter;
  try
    FBindings.AddOrSetValue(ABinding.DataID, ABinding);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecOCKeyStore.RegisterKey(ADataID: Word;
  const AKey: TAES128Key; ATagBits, AFreshnessBits: Byte);
var
  B: TOBDSecOCBinding;
begin
  B.DataID := ADataID;
  B.Key := AKey;
  B.TagBits := ATagBits;
  B.FreshnessBits := AFreshnessBits;
  Register(B);
end;

procedure TOBDSecOCKeyStore.Unregister(ADataID: Word);
begin
  FLock.Enter;
  try
    FBindings.Remove(ADataID);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecOCKeyStore.Clear;
begin
  FLock.Enter;
  try
    FBindings.Clear;
  finally
    FLock.Leave;
  end;
end;

function TOBDSecOCKeyStore.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FBindings.Count;
  finally
    FLock.Leave;
  end;
end;

function TOBDSecOCKeyStore.TryGet(ADataID: Word;
  out ABinding: TOBDSecOCBinding): Boolean;
begin
  FLock.Enter;
  try
    Result := FBindings.TryGetValue(ADataID, ABinding);
  finally
    FLock.Leave;
  end;
end;

end.
