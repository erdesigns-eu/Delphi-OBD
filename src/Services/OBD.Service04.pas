//------------------------------------------------------------------------------
// UNIT           : OBD.Service04.pas
// CONTENTS       : OBD Service 04 (Clear DTCs and Turn Off MIL)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.Service04;

interface

uses
  System.SysUtils, System.Classes,

  OBD.Request.Constants, OBD.Request.Encoders, OBD.Response.Decoders,
  OBD.Service.Types, OBD.Service;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 04 - Clear Diagnostic Trouble Codes and MIL (Malfunction Indicator Lamp)
  ///   
  ///   This is a command-only service with no response data.
  ///   It clears the following information:
  ///   - All stored diagnostic trouble codes (DTCs)
  ///   - Freeze frame data
  ///   - Oxygen sensor test data (Service 05)
  ///   - Status of system monitoring tests (readiness status)
  ///   - On-board monitoring test results (Service 06)
  ///   - Distance traveled while MIL activated
  ///   - Number of warm-ups since DTCs cleared
  ///   - Distance traveled since DTCs cleared
  ///   - Time run by engine while MIL activated
  ///   - Time since DTCs cleared
  ///   - Turns off the MIL (Check Engine Light)
  ///   
  ///   NOTE: This service does NOT return data. Most ECUs respond with
  ///   no message or a simple acknowledgment. The service is considered
  ///   successful if no error is returned.
  /// </summary>
  TOBDService04 = class(TOBDService)
  protected
    /// <summary>
    ///   Get the service id
    /// </summary>
    function GetServiceID: Byte; override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Parse service response (typically no response or simple ACK)
    /// </summary>
    procedure ParseResponse(const Response: TBytes); override;
  end;

  /// <summary>
  ///   Service 04 Request Encoder
  /// </summary>
  TOBDService04RequestEncoder = class(TOBDRequestEncoder)
  public
    /// <summary>
    ///   Encode request to clear DTCs and turn off MIL
    ///   Returns: [04] - Service 04 command only, no parameters
    /// </summary>
    function EncodeClearDTCsRequest: TBytes;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 04: GET SERVICE ID
//------------------------------------------------------------------------------
function TOBDService04.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_04;
end;

//------------------------------------------------------------------------------
// SERVICE 04: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService04.Create;
begin
  inherited Create;
end;

//------------------------------------------------------------------------------
// SERVICE 04: DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService04.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 04: PARSE RESPONSE
//------------------------------------------------------------------------------
procedure TOBDService04.ParseResponse(const Response: TBytes);
begin
  // Service 04 is a command-only service
  // Most ECUs do not send a response, or send $44 (0x04 + 0x40) acknowledgment
  // Some may return error codes if the operation failed
  // No data is parsed - silence or $44 indicates success
  // Errors would be caught by the protocol layer's negative response handling
end;

//------------------------------------------------------------------------------
// SERVICE 04 REQUEST ENCODER: ENCODE CLEAR DTCS REQUEST
//------------------------------------------------------------------------------
function TOBDService04RequestEncoder.EncodeClearDTCsRequest: TBytes;
begin
  // Service 04 command: no parameters, just the service ID
  SetLength(Result, 1);
  Result[0] := OBD_SERVICE_04;
end;

end.
