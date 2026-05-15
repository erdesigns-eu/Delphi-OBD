//------------------------------------------------------------------------------
//  OBD.Diagnostics.J1939.DM
//
//  TOBDJ1939DM — non-visual J1939-73 diagnostic-message component.
//  Decodes the standard Diagnostic Messages (DM1..DM32) into
//  structured records: SPN + FMI + CM + occurrence-count entries
//  per ISO J1939-73 §5.7, with the two lamp-status bytes parsed
//  into named flags.
//
//  Inbound DM frames are routed in through
//  <see cref="DispatchDM"/>: the host wires a TOBDJ1939's
//  <c>OnFrame</c> (or its session manager's <c>OnComplete</c>) and
//  forwards every DM-PGN frame to this component. The component
//  decodes the body and re-fires the structured payload through
//  <c>OnDTCs</c> (DM1/DM2/DM6/DM12/DM23/DM27/DM28) or
//  <c>OnRaw</c> (every other DM PGN — the host owns the decode).
//
//  SPN encoding per J1939-73 §5.7.1:
//
//    Each DTC record = 4 bytes:
//      byte 0:  SPN low 8 bits
//      byte 1:  SPN mid 8 bits
//      byte 2:  upper 3 bits = SPN high; lower 5 bits = FMI
//      byte 3:  bit 7 = CM (conversion method); bits 6..0 = OC
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1939-73:2024 §5.7 (Diagnostic Message DM1)
//    - SAE J1939-73:2024 §5.7.13 (DTC structure)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.J1939.DM;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.J1939;

type
  /// <summary>
  ///   Two-byte lamp-status decode (per J1939-73 §5.7.2).
  /// </summary>
  /// <remarks>
  ///   Each lamp has a 2-bit state — values 00 (off), 01 (on), or
  ///   11 (not supported). We expose the on-state as a Boolean
  ///   plus the raw bit pair for callers that care.
  /// </remarks>
  TOBDJ1939Lamps = record
    /// <summary>Malfunction Indicator Lamp state.</summary>
    MIL:    Boolean;
    /// <summary>Red Stop Lamp state.</summary>
    Red:    Boolean;
    /// <summary>Amber Warning Lamp state.</summary>
    Amber:  Boolean;
    /// <summary>Protect Lamp state.</summary>
    Protect: Boolean;
    /// <summary>Raw two-byte lamp word (high byte first).</summary>
    Raw:    array[0..1] of Byte;
  end;

  /// <summary>
  ///   One J1939 DTC entry decoded from a DM body.
  /// </summary>
  TOBDJ1939DtcEntry = record
    /// <summary>19-bit Suspect Parameter Number (the failing
    /// signal).</summary>
    SPN: Cardinal;
    /// <summary>5-bit Failure Mode Identifier (failure kind).</summary>
    FMI: Byte;
    /// <summary>Conversion Method (0 = J1939-71 version 4,
    /// 1 = legacy).</summary>
    CM:  Byte;
    /// <summary>7-bit Occurrence Count.</summary>
    OC:  Byte;
    /// <summary>Raw 4-byte record exactly as received.</summary>
    Raw: array[0..3] of Byte;
  end;

  /// <summary>
  ///   Fires after a successful structured DM decode. Main thread.
  /// </summary>
  TOBDJ1939DMEvent = procedure(Sender: TObject; APGN: Cardinal;
    const ALamps: TOBDJ1939Lamps;
    const AEntries: TArray<TOBDJ1939DtcEntry>) of object;

  /// <summary>
  ///   Fires for DM PGNs the component does not decode. Main thread.
  /// </summary>
  TOBDJ1939DMRawEvent = procedure(Sender: TObject; APGN: Cardinal;
    const AData: TBytes) of object;

  /// <summary>
  ///   J1939-73 diagnostic-message component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form alongside a <see cref="TOBDJ1939"/> bus
  ///   client. Wire <c>TOBDJ1939.OnFrame</c> (or its session
  ///   manager's <c>OnComplete</c>) to a handler that calls
  ///   <see cref="DispatchDM"/> for every DM-PGN. The component
  ///   handles SPN extraction and lamp decoding then re-fires
  ///   through <c>OnDTCs</c> (structured) or <c>OnRaw</c> (any
  ///   DM PGN that does not follow the standard 2-byte lamps +
  ///   4-byte DTC records layout).
  /// </remarks>
  TOBDJ1939DM = class(TComponent)
  strict private
    FOnDTCs: TOBDJ1939DMEvent;
    FOnRaw: TOBDJ1939DMRawEvent;
    function DecodeLamps(const AData: TBytes;
      AOffset: Integer): TOBDJ1939Lamps;
    function DecodeEntries(const AData: TBytes;
      AOffset: Integer): TArray<TOBDJ1939DtcEntry>;
    function IsStructuredDM(APGN: Cardinal): Boolean;
    procedure FireDTCs(APGN: Cardinal;
      const ALamps: TOBDJ1939Lamps;
      const AEntries: TArray<TOBDJ1939DtcEntry>);
    procedure FireRaw(APGN: Cardinal; const AData: TBytes);
  public
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Decodes one J1939 DTC record from <c>AData</c> at
    ///   <c>AOffset</c>.
    /// </summary>
    /// <param name="AData">Source byte buffer.</param>
    /// <param name="AOffset">Index of the first DTC record byte.</param>
    /// <returns>Decoded entry. The caller is responsible for
    /// confirming <c>AData</c> has at least 4 bytes from
    /// <c>AOffset</c>.</returns>
    class function DecodeEntry(const AData: TBytes;
      AOffset: Integer): TOBDJ1939DtcEntry; static;

    /// <summary>
    ///   Routes a received DM payload through the decoder.
    /// </summary>
    /// <param name="APGN">DM PGN (e.g. <c>J1939_PGN_DM1</c>).</param>
    /// <param name="AData">Reassembled DM body bytes.</param>
    /// <remarks>
    ///   Structured DM PGNs (DM1, DM2, DM6, DM12, DM23, DM27,
    ///   DM28) fire <c>OnDTCs</c>; all other DM PGNs fire
    ///   <c>OnRaw</c> with the verbatim body.
    /// </remarks>
    procedure DispatchDM(APGN: Cardinal; const AData: TBytes);
  published
    /// <summary>Fires on structured DM decode. Main thread.</summary>
    property OnDTCs: TOBDJ1939DMEvent read FOnDTCs write FOnDTCs;
    /// <summary>Fires on unstructured DM payload. Main thread.</summary>
    property OnRaw: TOBDJ1939DMRawEvent read FOnRaw write FOnRaw;
  end;

implementation

constructor TOBDJ1939DM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TOBDJ1939DM.IsStructuredDM(APGN: Cardinal): Boolean;
begin
  // The DM PGNs that follow the standard <lamps> + [<DTC>]* layout
  // per J1939-73 §5.7. DM3 (clear) / DM5 (readiness) / DM11 (clear
  // active) / DM19 (CalID) / DM20 (IUMPR) and the rest carry
  // different payload shapes; route them to OnRaw.
  case APGN of
    J1939_PGN_DM1, J1939_PGN_DM2,
    J1939_PGN_DM6, J1939_PGN_DM12,
    J1939_PGN_DM23, J1939_PGN_DM27,
    J1939_PGN_DM28:
      Result := True;
  else
    Result := False;
  end;
end;

function TOBDJ1939DM.DecodeLamps(const AData: TBytes;
  AOffset: Integer): TOBDJ1939Lamps;
var
  B0, B1: Byte;
begin
  Result := Default(TOBDJ1939Lamps);
  if AOffset + 2 > Length(AData) then
    Exit;
  B0 := AData[AOffset];
  B1 := AData[AOffset + 1];
  Result.Raw[0] := B0;
  Result.Raw[1] := B1;
  // Lamp status nibbles per J1939-73 §5.7.2 (2 bits each, byte 0
  // hi-nibble = MIL/Red, byte 0 lo-nibble = Amber/Protect; byte 1
  // carries the lamp-flash-status that we do not yet decode).
  Result.MIL     := ((B0 shr 6) and $03) = $01;
  Result.Red     := ((B0 shr 4) and $03) = $01;
  Result.Amber   := ((B0 shr 2) and $03) = $01;
  Result.Protect := ( B0        and $03) = $01;
end;

class function TOBDJ1939DM.DecodeEntry(const AData: TBytes;
  AOffset: Integer): TOBDJ1939DtcEntry;
var
  B0, B1, B2, B3: Byte;
begin
  B0 := AData[AOffset];
  B1 := AData[AOffset + 1];
  B2 := AData[AOffset + 2];
  B3 := AData[AOffset + 3];
  Result := Default(TOBDJ1939DtcEntry);
  // SPN: byte 0 = low 8, byte 1 = mid 8, byte 2 upper 3 bits = high.
  Result.SPN := (Cardinal(B0)) or
                (Cardinal(B1) shl 8) or
                ((Cardinal(B2) and $E0) shl 11);
  // FMI: lower 5 bits of byte 2.
  Result.FMI := B2 and $1F;
  // CM: bit 7 of byte 3. OC: bits 6..0 of byte 3.
  Result.CM := (B3 shr 7) and $01;
  Result.OC := B3 and $7F;
  Result.Raw[0] := B0;
  Result.Raw[1] := B1;
  Result.Raw[2] := B2;
  Result.Raw[3] := B3;
end;

function TOBDJ1939DM.DecodeEntries(const AData: TBytes;
  AOffset: Integer): TArray<TOBDJ1939DtcEntry>;
var
  Off: Integer;
  Acc: TList<TOBDJ1939DtcEntry>;
  E: TOBDJ1939DtcEntry;
begin
  Acc := TList<TOBDJ1939DtcEntry>.Create;
  try
    Off := AOffset;
    while Off + 4 <= Length(AData) do
    begin
      E := DecodeEntry(AData, Off);
      // J1939-73 reserves the all-zero DTC slot as "no fault".
      // Skip it so a "clean" DM1 doesn't surface a phantom entry.
      if (E.SPN <> 0) or (E.FMI <> 0) then
        Acc.Add(E);
      Inc(Off, 4);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

procedure TOBDJ1939DM.DispatchDM(APGN: Cardinal; const AData: TBytes);
var
  Lamps: TOBDJ1939Lamps;
  Entries: TArray<TOBDJ1939DtcEntry>;
begin
  if IsStructuredDM(APGN) then
  begin
    Lamps := DecodeLamps(AData, 0);
    Entries := DecodeEntries(AData, 2);
    FireDTCs(APGN, Lamps, Entries);
  end
  else
    FireRaw(APGN, AData);
end;

procedure TOBDJ1939DM.FireDTCs(APGN: Cardinal;
  const ALamps: TOBDJ1939Lamps;
  const AEntries: TArray<TOBDJ1939DtcEntry>);
var
  Self_: TOBDJ1939DM;
  PGN: Cardinal;
  Lamps: TOBDJ1939Lamps;
  Snap: TArray<TOBDJ1939DtcEntry>;
begin
  if not Assigned(FOnDTCs) then
    Exit;
  Self_ := Self;
  PGN := APGN;
  Lamps := ALamps;
  Snap := Copy(AEntries, 0, Length(AEntries));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDTCs(Self_, PGN, Lamps, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnDTCs) then
          Self_.FOnDTCs(Self_, PGN, Lamps, Snap);
      end);
end;

procedure TOBDJ1939DM.FireRaw(APGN: Cardinal; const AData: TBytes);
var
  Self_: TOBDJ1939DM;
  PGN: Cardinal;
  Snap: TBytes;
begin
  if not Assigned(FOnRaw) then
    Exit;
  Self_ := Self;
  PGN := APGN;
  Snap := Copy(AData, 0, Length(AData));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRaw(Self_, PGN, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnRaw) then
          Self_.FOnRaw(Self_, PGN, Snap);
      end);
end;

end.
