//------------------------------------------------------------------------------
//  OBD.Errors
//
//  Error-code → message mapping for transient errors surfaced via the
//  <c>OnError(Sender; ErrorCode; const Message; var Handled)</c> event.
//
//  Components fill <c>Message</c> with a context-specific string when
//  they have one; otherwise they fall back to <see cref="OBDErrorCodeToMessage"/>
//  to produce the canonical text for the code.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial mapping for the TOBDErrorCode enum.
//
//  Future work :
//    - Localisable text (resource string) once we settle on a translation
//      strategy.
//------------------------------------------------------------------------------

unit OBD.Errors;

interface

uses
  OBD.Types;

/// <summary>
///   Returns the canonical English message for a <see cref="TOBDErrorCode"/>.
/// </summary>
/// <param name="ACode">Error code to translate.</param>
/// <returns>Single-line English message, suitable for logs and error
/// dialogs. Never empty for any defined enum value.</returns>
/// <remarks>
///   Components are free to override this with a more specific message
///   in their <c>OnError</c> event. This function is the fallback when
///   no context is available.
/// </remarks>
function OBDErrorCodeToMessage(ACode: TOBDErrorCode): string;

/// <summary>
///   Returns the short identifier for a <see cref="TOBDErrorCode"/>
///   (e.g. <c>'oeTimeout'</c>). Used in log files and audit entries.
/// </summary>
/// <param name="ACode">Error code to translate.</param>
/// <returns>Short Pascal-style identifier.</returns>
function OBDErrorCodeToIdent(ACode: TOBDErrorCode): string;

implementation

function OBDErrorCodeToMessage(ACode: TOBDErrorCode): string;
begin
  case ACode of
    oeNone:             Result := 'No error.';
    oeTimeout:          Result := 'Operation timed out waiting for a response.';
    oeNoData:           Result := 'No data: the ECU did not respond.';
    oeBufferFull:       Result := 'Adapter buffer overflow.';
    oeBusError:         Result := 'Bus error reported by the adapter.';
    oeUnsupportedPID:   Result := 'PID is not supported by the ECU.';
    oeNRC:              Result := 'UDS negative response code received.';
    oeProtocolMismatch: Result := 'Protocol mismatch for the current bus.';
    oeChecksumFailed:   Result := 'Frame failed checksum or CRC validation.';
    oeReassemblyFailed: Result := 'Multi-frame reassembly failed.';
    oeAdapterFault:     Result := 'Adapter reported an internal fault.';
    oeUnexpectedFrame:  Result := 'Unsolicited or unexpected frame received.';
    oeIO:               Result := 'I/O error on the underlying transport.';
  else
    Result := 'Unknown error.';
  end;
end;

function OBDErrorCodeToIdent(ACode: TOBDErrorCode): string;
begin
  case ACode of
    oeNone:             Result := 'oeNone';
    oeTimeout:          Result := 'oeTimeout';
    oeNoData:           Result := 'oeNoData';
    oeBufferFull:       Result := 'oeBufferFull';
    oeBusError:         Result := 'oeBusError';
    oeUnsupportedPID:   Result := 'oeUnsupportedPID';
    oeNRC:              Result := 'oeNRC';
    oeProtocolMismatch: Result := 'oeProtocolMismatch';
    oeChecksumFailed:   Result := 'oeChecksumFailed';
    oeReassemblyFailed: Result := 'oeReassemblyFailed';
    oeAdapterFault:     Result := 'oeAdapterFault';
    oeUnexpectedFrame:  Result := 'oeUnexpectedFrame';
    oeIO:               Result := 'oeIO';
  else
    Result := 'oeUnknown';
  end;
end;

end.
