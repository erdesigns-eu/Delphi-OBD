//------------------------------------------------------------------------------
//  OBD.Protocol.UDS
//
//  UDS (ISO 14229) request / response codec. Encodes a TOBDRequest
//  into a hex command string suitable for ELM327 / OBDLink chips
//  (which abstract the wire framing) and decodes the textual hex
//  response into a TOBDResponse — including the negative-response
//  shape <c>0x7F service NRC</c>.
//
//  This unit does not own a transport. It is consumed by the
//  TOBDProtocol component, which runs the encoded request through
//  TOBDAdapter.WriteOBDCommand.
//
//  Phase 6's high-level UDS components (TOBDUDS, TOBDUDSReadDID, …)
//  build on this codec.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §7 (services + response shapes)
//    - ISO 14229-1:2020 §8.7 (negative response codes)
//
//  History     :
//    2026-05-09  ERD  Initial implementation codec.
//------------------------------------------------------------------------------

unit OBD.Protocol.UDS;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Catalog,
  OBD.Protocol.Types;

const
  /// <summary>UDS positive-response offset (added to the request
  /// service ID).</summary>
  UDS_POSITIVE_RESPONSE_OFFSET = $40;
  /// <summary>Leading byte of every UDS negative response.</summary>
  UDS_NEGATIVE_RESPONSE = $7F;

  // ---- UDS Service IDs (ISO 14229-1 §7) ----
  UDS_SID_DiagnosticSessionControl     = $10;
  UDS_SID_ECUReset                     = $11;
  UDS_SID_ClearDiagnosticInformation   = $14;
  UDS_SID_ReadDTCInformation           = $19;
  UDS_SID_ReadDataByIdentifier         = $22;
  UDS_SID_ReadMemoryByAddress          = $23;
  UDS_SID_ReadScalingDataByIdentifier  = $24;
  UDS_SID_SecurityAccess               = $27;
  UDS_SID_CommunicationControl         = $28;
  UDS_SID_Authentication               = $29;
  UDS_SID_ReadDataByPeriodicIdentifier = $2A;
  UDS_SID_DynamicallyDefineDataIdentifier = $2C;
  UDS_SID_WriteDataByIdentifier        = $2E;
  UDS_SID_InputOutputControlByIdentifier = $2F;
  UDS_SID_RoutineControl               = $31;
  UDS_SID_RequestDownload              = $34;
  UDS_SID_RequestUpload                = $35;
  UDS_SID_TransferData                 = $36;
  UDS_SID_RequestTransferExit          = $37;
  UDS_SID_WriteMemoryByAddress         = $3D;
  UDS_SID_TesterPresent                = $3E;
  UDS_SID_AccessTimingParameter        = $83;
  UDS_SID_SecuredDataTransmission      = $84;
  UDS_SID_ControlDTCSetting            = $85;
  UDS_SID_ResponseOnEvent              = $86;
  UDS_SID_LinkControl                  = $87;

  // ---- Common NRC codes (full table lives in catalogs/obd2/nrc.json) ----
  UDS_NRC_GeneralReject                = $10;
  UDS_NRC_ServiceNotSupported          = $11;
  UDS_NRC_SubFunctionNotSupported      = $12;
  UDS_NRC_IncorrectMessageLength       = $13;
  UDS_NRC_ResponseTooLong              = $14;
  UDS_NRC_BusyRepeatRequest            = $21;
  UDS_NRC_ConditionsNotCorrect         = $22;
  UDS_NRC_RequestSequenceError         = $24;
  UDS_NRC_RequestOutOfRange            = $31;
  UDS_NRC_SecurityAccessDenied         = $33;
  UDS_NRC_InvalidKey                   = $35;
  UDS_NRC_ExceededAttempts             = $36;
  UDS_NRC_RequiredTimeDelayNotExpired  = $37;
  UDS_NRC_ResponsePending              = $78;
  UDS_NRC_ServiceNotSupportedInSession = $7F;

type
  /// <summary>
  ///   Stateless UDS encoder / decoder.
  /// </summary>
  TOBDUDSCodec = class
  public
    /// <summary>
    ///   Encodes a request into the hex command form ELM327 chips
    ///   expect (e.g. <c>'22 F1 90'</c>).
    /// </summary>
    /// <param name="ARequest">Request. <c>ServiceID</c> must be set.</param>
    /// <returns>Hex command string with single-space separators.</returns>
    /// <exception cref="EOBDProtocolErr"><c>ServiceID</c> is zero.</exception>
    class function Encode(const ARequest: TOBDRequest): string; static;

    /// <summary>
    ///   Decodes the hex-text response from the adapter into a
    ///   structured response. Detects the <c>0x7F service NRC</c>
    ///   shape and resolves NRC text via <c>OBD.Catalog</c>.
    /// </summary>
    /// <param name="ARequest">The originating request.</param>
    /// <param name="ARawHex">Raw hex text from the adapter (one or
    /// more concatenated lines).</param>
    /// <param name="AResponse">Output response.</param>
    /// <returns>True when at least one byte was decoded.</returns>
    class function Decode(const ARequest: TOBDRequest;
      const ARawHex: string; out AResponse: TOBDResponse): Boolean; static;

    /// <summary>
    ///   Returns the conventional positive-response service ID for a
    ///   request (request SID + 0x40).
    /// </summary>
    /// <param name="ARequestSID">Request service ID.</param>
    /// <returns>Positive-response SID.</returns>
    class function ExpectedPositiveResponse(ARequestSID: Byte): Byte; static;

    /// <summary>
    ///   Resolves an NRC byte to its human-readable text via
    ///   <c>OBD.Catalog</c>. Returns a synthetic
    ///   <c>'NRC 0xXX'</c> when no catalogue entry matches.
    /// </summary>
    /// <param name="ANRC">NRC byte.</param>
    /// <returns>Resolved text.</returns>
    class function ResolveNRCText(ANRC: Byte): string; static;
  end;

implementation

{ ---- TOBDUDSCodec ------------------------------------------------------------ }

class function TOBDUDSCodec.ExpectedPositiveResponse(ARequestSID: Byte): Byte;
begin
  Result := ARequestSID + UDS_POSITIVE_RESPONSE_OFFSET;
end;

class function TOBDUDSCodec.Encode(const ARequest: TOBDRequest): string;
var
  Body: TBytes;
begin
  if ARequest.ServiceID = 0 then
    raise EOBDProtocolErr.Create('UDS Encode: ServiceID is 0');
  SetLength(Body, 1 + Length(ARequest.Data));
  Body[0] := ARequest.ServiceID;
  if Length(ARequest.Data) > 0 then
    Move(ARequest.Data[0], Body[1], Length(ARequest.Data));
  Result := BytesToHex(Body);
end;

class function TOBDUDSCodec.ResolveNRCText(ANRC: Byte): string;
begin
  if not TOBDCatalogStore.Default.FindText(ckUDSNRC, ANRC, Result) then
    Result := Format('NRC 0x%2.2X', [ANRC]);
end;

class function TOBDUDSCodec.Decode(const ARequest: TOBDRequest;
  const ARawHex: string; out AResponse: TOBDResponse): Boolean;
var
  Bytes: TBytes;
  Expected: Byte;
begin
  AResponse := MakeOBDResponse;
  AResponse.Request := ARequest;
  Bytes := HexToBytes(ARawHex);
  if Length(Bytes) = 0 then Exit(False);

  // Negative response: 7F <SID> <NRC>
  if (Bytes[0] = UDS_NEGATIVE_RESPONSE) and (Length(Bytes) >= 3) then
  begin
    AResponse.IsNegative := True;
    AResponse.ServiceID := Bytes[1];
    AResponse.NRC := Bytes[2];
    AResponse.NRCText := ResolveNRCText(AResponse.NRC);
    SetLength(AResponse.Data, 0);
    Exit(True);
  end;

  Expected := ExpectedPositiveResponse(ARequest.ServiceID);
  AResponse.ServiceID := Bytes[0];
  if Length(Bytes) > 1 then
  begin
    SetLength(AResponse.Data, Length(Bytes) - 1);
    Move(Bytes[1], AResponse.Data[0], Length(Bytes) - 1);
  end;

  // Mismatched service ID is not a hard fault — some chips strip the
  // SID echo. Caller can compare AResponse.ServiceID with Expected
  // and decide.
  if (Expected = 0) or (AResponse.ServiceID = Expected) then
    // expected match (or no expectation).
    ;

  Result := True;
end;

end.
