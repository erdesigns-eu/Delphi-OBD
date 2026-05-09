//------------------------------------------------------------------------------
//  OBD.Protocol.KWP2000
//
//  KWP2000 (ISO 14230) request / response codec. Service IDs differ
//  from UDS but the negative-response shape is identical
//  (<c>0x7F service NRC</c>) and most of the codec is shared.
//
//  KWP2000 supports two physical layer flavours: K-line with 5-baud
//  init (legacy) and CAN with fast init. The codec produces the same
//  hex command bytes either way; the wire-init dance is the adapter's
//  responsibility (Phase 3).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14230-3 (Application layer)
//    - ISO 14230-2 (Data link layer)
//
//  History     :
//    2026-05-09  ERD  Phase 4 initial codec.
//------------------------------------------------------------------------------

unit OBD.Protocol.KWP2000;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Catalog,
  OBD.Protocol.Types,
  OBD.Protocol.UDS;

const
  /// <summary>KWP2000 positive response offset (same convention as
  /// UDS).</summary>
  KWP_POSITIVE_RESPONSE_OFFSET = $40;
  /// <summary>Leading byte of every KWP2000 negative response.</summary>
  KWP_NEGATIVE_RESPONSE = $7F;

  // ---- KWP2000 Service IDs (ISO 14230-3 Annex A) ----
  KWP_SID_StartDiagnosticSession         = $10;
  KWP_SID_ECUReset                       = $11;
  KWP_SID_ClearDiagnosticInformation     = $14;
  KWP_SID_ReadDTCByStatus                = $18;
  KWP_SID_ReadStatusOfDTC                = $19;
  KWP_SID_ReadECUIdentification          = $1A;
  KWP_SID_ReadDataByLocalIdentifier      = $21;
  KWP_SID_ReadDataByCommonIdentifier     = $22;
  KWP_SID_ReadMemoryByAddress            = $23;
  KWP_SID_StopDiagnosticSession          = $20;
  KWP_SID_SecurityAccess                 = $27;
  KWP_SID_DynamicallyDefineLocalID       = $2C;
  KWP_SID_InputOutputControlByLocalID    = $2F;
  KWP_SID_InputOutputControlByCommonID   = $30;
  KWP_SID_StartRoutineByLocalID          = $31;
  KWP_SID_StopRoutineByLocalID           = $32;
  KWP_SID_RequestRoutineResultsByLocalID = $33;
  KWP_SID_RequestDownload                = $34;
  KWP_SID_RequestUpload                  = $35;
  KWP_SID_TransferData                   = $36;
  KWP_SID_RequestTransferExit            = $37;
  KWP_SID_TesterPresent                  = $3E;

type
  /// <summary>
  ///   Stateless KWP2000 encoder / decoder.
  /// </summary>
  /// <remarks>
  ///   Shares its negative-response handling with UDS — both protocols
  ///   use the same 0x7F sid nrc shape. Positive-response detection
  ///   adds 0x40 to the request SID, identical to UDS.
  /// </remarks>
  TOBDKWPCodec = class
  public
    /// <summary>Encodes a KWP2000 request as space-separated hex.</summary>
    /// <param name="ARequest">Request. <c>ServiceID</c> required.</param>
    /// <returns>Hex command string.</returns>
    /// <exception cref="EOBDProtocolErr"><c>ServiceID</c> is zero.</exception>
    class function Encode(const ARequest: TOBDRequest): string; static;

    /// <summary>
    ///   Decodes the hex-text response. The response shape is
    ///   identical to UDS so this method delegates to
    ///   <see cref="TOBDUDSCodec.Decode"/>.
    /// </summary>
    /// <param name="ARequest">Originating request.</param>
    /// <param name="ARawHex">Raw hex text from the adapter.</param>
    /// <param name="AResponse">Output response.</param>
    /// <returns>True when at least one byte was decoded.</returns>
    class function Decode(const ARequest: TOBDRequest;
      const ARawHex: string; out AResponse: TOBDResponse): Boolean; static;

    /// <summary>
    ///   Returns the conventional positive-response service ID for a
    ///   KWP2000 request.
    /// </summary>
    /// <param name="ARequestSID">Request service ID.</param>
    /// <returns>Positive-response SID.</returns>
    class function ExpectedPositiveResponse(ARequestSID: Byte): Byte; static;
  end;

implementation

{ ---- TOBDKWPCodec ------------------------------------------------------------ }

class function TOBDKWPCodec.ExpectedPositiveResponse(ARequestSID: Byte): Byte;
begin
  Result := ARequestSID + KWP_POSITIVE_RESPONSE_OFFSET;
end;

class function TOBDKWPCodec.Encode(const ARequest: TOBDRequest): string;
var
  Body: TBytes;
begin
  if ARequest.ServiceID = 0 then
    raise EOBDProtocolErr.Create('KWP Encode: ServiceID is 0');
  SetLength(Body, 1 + Length(ARequest.Data));
  Body[0] := ARequest.ServiceID;
  if Length(ARequest.Data) > 0 then
    Move(ARequest.Data[0], Body[1], Length(ARequest.Data));
  Result := BytesToHex(Body);
end;

class function TOBDKWPCodec.Decode(const ARequest: TOBDRequest;
  const ARawHex: string; out AResponse: TOBDResponse): Boolean;
begin
  // KWP2000 and UDS share the on-the-wire response shape; reuse the
  // UDS decoder verbatim. NRC text resolution also goes through the
  // same uds-nrc catalogue (KWP2000 NRCs that overlap UDS use the
  // same numeric values).
  Result := TOBDUDSCodec.Decode(ARequest, ARawHex, AResponse);
end;

end.
