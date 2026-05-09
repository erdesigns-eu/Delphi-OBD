//------------------------------------------------------------------------------
// UNIT           : OBD.FreezeFrame.pas
// CONTENTS       : Wire helpers + decoder for OBD-II Service 02
//                  (Freeze-frame data — same PIDs as Service 01,
//                   captured at the moment a confirmed DTC was set).
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Service 02 takes both a PID and a frame number
//                  (almost always 0 — the frame number selects
//                  which freeze-frame snapshot when the ECU keeps
//                  multiple). Encoding:
//                    request:  02 PID FrameNum
//                    response: 42 PID FrameNum DATA…
//                  Some scan-tool bridges add a leading length
//                  byte; the helpers here strip it transparently.
//------------------------------------------------------------------------------
unit OBD.FreezeFrame;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  EOBDFreezeFrameError = class(Exception);

  /// <summary>
  ///   One PID/data pair from a freeze-frame snapshot.
  /// </summary>
  TOBDFreezeFrameEntry = record
    /// <summary>
    ///   Frame number (almost always 0; ECUs that store
    ///   multiple frames keep them numbered sequentially).
    /// </summary>
    FrameNumber: Byte;
    /// <summary>
    ///   The Service 01 PID this entry mirrors.
    /// </summary>
    PID: Byte;
    /// <summary>
    ///   Raw payload bytes (everything past the SID + PID
    ///   + frame echo). Decode through your usual OBD-II decoder
    ///   (`OBD.OEM.Catalog.JSON.DecodePayload` against
    ///   `obd2-pids.json`).
    /// </summary>
    Payload: TBytes;
  end;

  /// <summary>
  ///   One full freeze-frame snapshot — typically the DTC
  ///   that triggered the snapshot plus a handful of correlated
  ///   PIDs (engine load, coolant temp, RPM, vehicle speed, …).
  /// </summary>
  TOBDFreezeFrameSnapshot = record
    FrameNumber: Byte;
    /// <summary>
    ///   The DTC that was being set when the freeze frame
    ///   was captured. Decoded from PID 0x02 (DTC that caused freeze
    ///   frame). Empty when PID 0x02 wasn't queried.
    /// </summary>
    TriggerDTC: string;
    /// <summary>
    ///   Per-PID payload entries collected for this frame.
    /// </summary>
    Entries: TArray<TOBDFreezeFrameEntry>;
  end;

/// <summary>
///   Build the request frame for Service 02.
/// </summary>
function BuildFreezeFrameRequest(const PID: Byte;
  const FrameNum: Byte = 0): TBytes;

/// <summary>
///   Parse a positive Service 02 reply (<c>42 PID FrameNum
///   DATA…</c>). Throws on a negative response (<c>7F 02 NRC</c>) or
///   a malformed frame.
/// </summary>
function ParseFreezeFrameResponse(const Response: TBytes;
  const ExpectedPID: Byte): TOBDFreezeFrameEntry;

/// <summary>
///   Format the trigger-DTC bytes from PID 0x02 into the
///   canonical 5-character string (e.g. <c>"P0301"</c>). Mirrors
///   OBD.OEM.DTC.FormatDtc but specialised for the 2-byte PID 0x02
///   payload.
/// </summary>
function FormatFreezeFrameTriggerDTC(const Bytes: TBytes): string;

implementation

uses
  OBD.OEM.DTC;

//------------------------------------------------------------------------------
// BUILD FREEZE FRAME REQUEST
//------------------------------------------------------------------------------
function BuildFreezeFrameRequest(const PID: Byte;
  const FrameNum: Byte): TBytes;
begin
  Result := TBytes.Create($02, PID, FrameNum);
end;

//------------------------------------------------------------------------------
// PARSE FREEZE FRAME RESPONSE
//------------------------------------------------------------------------------
function ParseFreezeFrameResponse(const Response: TBytes;
  const ExpectedPID: Byte): TOBDFreezeFrameEntry;
begin
  Result := Default(TOBDFreezeFrameEntry);
  if Length(Response) < 3 then
  begin
    if (Length(Response) >= 3) and (Response[0] = $7F) and
       (Response[1] = $02) then
      raise EOBDFreezeFrameError.CreateFmt(
        'Negative freeze-frame response: NRC 0x%.2X', [Response[2]]);
    raise EOBDFreezeFrameError.CreateFmt(
      'Freeze-frame response too short: %d bytes (need at least 3)',
      [Length(Response)]);
  end;
  if Response[0] = $7F then
  begin
    if Response[1] <> $02 then
      raise EOBDFreezeFrameError.CreateFmt(
        'Negative response targets the wrong SID 0x%.2X', [Response[1]]);
    raise EOBDFreezeFrameError.CreateFmt(
      'Negative freeze-frame response: NRC 0x%.2X', [Response[2]]);
  end;
  if Response[0] <> $42 then
    raise EOBDFreezeFrameError.CreateFmt(
      'Unexpected freeze-frame SID 0x%.2X (want 0x42)', [Response[0]]);
  if Response[1] <> ExpectedPID then
    raise EOBDFreezeFrameError.CreateFmt(
      'Freeze-frame PID mismatch: got 0x%.2X, expected 0x%.2X',
      [Response[1], ExpectedPID]);

  Result.PID := Response[1];
  Result.FrameNumber := Response[2];
  Result.Payload := Copy(Response, 3, Length(Response) - 3);
end;

//------------------------------------------------------------------------------
// FORMAT FREEZE FRAME TRIGGER DTC
//------------------------------------------------------------------------------
function FormatFreezeFrameTriggerDTC(const Bytes: TBytes): string;
begin
  // PID 0x02 returns 2 bytes (encoded per ISO 15031-5 / SAE J2012).
  // Reuse OBD.OEM.DTC.FormatDtc which knows the same encoding.
  if Length(Bytes) < 2 then Exit('');
  Result := FormatDtc(Bytes[0], Bytes[1]);
end;

end.
