//------------------------------------------------------------------------------
//  OBD.Protocol.SecOC.Freshness
//
//  SecOC freshness counter manager. AUTOSAR SecOC sends a truncated
//  Freshness Value (FV) on the wire — typically 4..24 bits — while
//  the MAC is computed over the full 32- / 64-bit FV. Sender and
//  receiver maintain a per-Data-ID counter; the receiver
//  reconstructs the full FV from the truncated bits using its own
//  last-seen value plus a tolerance window for in-flight messages.
//
//  This unit ships an in-memory implementation. Hosts that persist
//  freshness across power-cycles wrap it (or implement
//  IOBDSecOCFreshnessProvider externally) so the counter survives
//  ignition-off — losing freshness across a power-cycle is a
//  classic SecOC pitfall.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - AUTOSAR SecOC SWS §7.5 (Freshness handling)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.SecOC.Freshness;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>
  ///   Provider contract for the per-Data-ID freshness counter.
  ///   Hosts back this with NVM / EEPROM to survive power-cycles.
  /// </summary>
  IOBDSecOCFreshnessProvider = interface
    ['{C39A1F2E-77B4-4D4D-A0AB-90F1E2D5C6AB}']
    /// <summary>
    ///   Increments and returns the next freshness value to send
    ///   for <c>ADataID</c>. The returned value is the new full
    ///   counter — the codec truncates the wire portion itself.
    /// </summary>
    /// <param name="ADataID">SecOC Data ID.</param>
    /// <returns>Next 64-bit FV.</returns>
    function NextTx(ADataID: Word): UInt64;

    /// <summary>
    ///   Reconstructs the full FV from <c>ATruncatedFV</c> using the
    ///   receiver's last-seen value plus tolerance window. Updates
    ///   the receiver-side counter when the message is accepted.
    /// </summary>
    /// <param name="ADataID">SecOC Data ID.</param>
    /// <param name="ATruncatedFV">Low <c>AFreshnessBits</c> of the
    /// FV from the wire.</param>
    /// <param name="AFreshnessBits">Number of bits actually carried
    /// in <c>ATruncatedFV</c>.</param>
    /// <param name="AReconstructed">Output: the full FV (ready to
    /// feed to CMAC).</param>
    /// <returns>True when the reconstructed FV is plausibly fresh
    /// (greater than the previous accept and within
    /// <see cref="MaxJump"/>); False to reject as replay or
    /// out-of-range jump.</returns>
    function TryAccept(ADataID: Word; ATruncatedFV: UInt64;
      AFreshnessBits: Byte; out AReconstructed: UInt64): Boolean;
  end;

  /// <summary>
  ///   Default in-memory freshness provider. Per-Data-ID 64-bit
  ///   counter, monotonic, with a configurable maximum allowed jump
  ///   to limit acceptance of "from the future" replays. Thread-safe.
  /// </summary>
  TOBDSecOCFreshness = class(TInterfacedObject, IOBDSecOCFreshnessProvider)
  strict private
    FLock: TCriticalSection;
    FTxCounter: TDictionary<Word, UInt64>;
    FRxCounter: TDictionary<Word, UInt64>;
    FMaxJump: UInt64;
  public
    /// <summary>Creates a freshness store with default
    /// <c>MaxJump = 16</c> (AUTOSAR-typical for an 8-bit truncation
    /// window — keep small to bound replay surface).</summary>
    constructor Create;
    destructor Destroy; override;

    /// <summary>Forces the TX counter for a Data ID. Useful for
    /// resuming after a controlled reboot when the host has
    /// persisted the last value externally.</summary>
    procedure SeedTx(ADataID: Word; ACounter: UInt64);
    /// <summary>Forces the RX counter for a Data ID.</summary>
    procedure SeedRx(ADataID: Word; ACounter: UInt64);

    /// <summary>Removes both TX and RX counters for a Data ID.</summary>
    procedure Reset(ADataID: Word);
    /// <summary>Removes all counters.</summary>
    procedure ResetAll;

    /// <summary>Maximum delta between the last-accepted RX FV and a
    /// new candidate; messages further than this are rejected as
    /// suspect (lost-counter window). Default 16.</summary>
    property MaxJump: UInt64 read FMaxJump write FMaxJump;

    function NextTx(ADataID: Word): UInt64;
    function TryAccept(ADataID: Word; ATruncatedFV: UInt64;
      AFreshnessBits: Byte; out AReconstructed: UInt64): Boolean;
  end;

implementation

constructor TOBDSecOCFreshness.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FTxCounter := TDictionary<Word, UInt64>.Create;
  FRxCounter := TDictionary<Word, UInt64>.Create;
  FMaxJump := 16;
end;

destructor TOBDSecOCFreshness.Destroy;
begin
  FRxCounter.Free;
  FTxCounter.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDSecOCFreshness.SeedTx(ADataID: Word; ACounter: UInt64);
begin
  FLock.Enter;
  try
    FTxCounter.AddOrSetValue(ADataID, ACounter);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecOCFreshness.SeedRx(ADataID: Word; ACounter: UInt64);
begin
  FLock.Enter;
  try
    FRxCounter.AddOrSetValue(ADataID, ACounter);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecOCFreshness.Reset(ADataID: Word);
begin
  FLock.Enter;
  try
    FTxCounter.Remove(ADataID);
    FRxCounter.Remove(ADataID);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecOCFreshness.ResetAll;
begin
  FLock.Enter;
  try
    FTxCounter.Clear;
    FRxCounter.Clear;
  finally
    FLock.Leave;
  end;
end;

function TOBDSecOCFreshness.NextTx(ADataID: Word): UInt64;
var
  Cur: UInt64;
begin
  FLock.Enter;
  try
    if not FTxCounter.TryGetValue(ADataID, Cur) then
      Cur := 0;
    Inc(Cur);
    FTxCounter.AddOrSetValue(ADataID, Cur);
    Result := Cur;
  finally
    FLock.Leave;
  end;
end;

function TOBDSecOCFreshness.TryAccept(ADataID: Word; ATruncatedFV: UInt64;
  AFreshnessBits: Byte; out AReconstructed: UInt64): Boolean;
var
  LastRx, Mask, HighPart, Candidate: UInt64;
begin
  Result := False;
  AReconstructed := 0;
  if (AFreshnessBits = 0) or (AFreshnessBits > 64) then Exit;

  if AFreshnessBits >= 64 then
    Mask := UInt64(-1)
  else
    Mask := (UInt64(1) shl AFreshnessBits) - 1;

  ATruncatedFV := ATruncatedFV and Mask;

  FLock.Enter;
  try
    if not FRxCounter.TryGetValue(ADataID, LastRx) then
      LastRx := 0;

    HighPart := LastRx and (not Mask);
    Candidate := HighPart or ATruncatedFV;

    if Candidate <= LastRx then
    begin
      // Truncated FV wrapped — try the next epoch.
      Candidate := Candidate + (Mask + 1);
    end;

    // Accept only if strictly increasing AND within the jump window.
    if (Candidate > LastRx) and ((Candidate - LastRx) <= FMaxJump) then
    begin
      AReconstructed := Candidate;
      FRxCounter.AddOrSetValue(ADataID, Candidate);
      Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
