//------------------------------------------------------------------------------
// UNIT           : OBD.Service03.pas
// CONTENTS       : OBD Service 03 (Show stored Diagnostic Trouble Codes)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 18/03/2024
//------------------------------------------------------------------------------
unit OBD.Service03;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  OBD.Request.Constants, OBD.Request.Encoders, OBD.Response.Decoders, OBD.Service.Types,
  OBD.Service;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 03
  /// </summary>
  TOBDService03 = class(TOBDService)
  private
    /// <summary>
    ///   List with Diagnostic Trouble Codes (One code per line)
    /// </summary>
    FDTC: TArray<TOBDServiceDiagnosticTroubleCode>;
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
    ///   Reset (clear all data)
    /// </summary>
    procedure Reset;
    /// <summary>
    ///   Parse service response
    /// </summary>
    procedure ParseResponse(Response: TBytes); override;

    /// <summary>
    ///   List with Diagnostic Trouble Codes (One code per line)
    /// </summary>
    property DTC: TArray<TOBDServiceDiagnosticTroubleCode> read FDTC;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 03: GET SERVIVE ID
//------------------------------------------------------------------------------
function TOBDService03.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_03;
end;

//------------------------------------------------------------------------------
// SERVICE 03: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService03.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Clear DTC list
  SetLength(FDTC, 0);
  // Clear all data and set to defaults
  Reset;
end;

//------------------------------------------------------------------------------
// SERVICE 03: DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService03.Destroy;
begin
  // Clear DTC list
  SetLength(FDTC, 0);
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 03: RESET - CLEAR ALL DATA
//------------------------------------------------------------------------------
procedure TOBDService03.Reset;
begin
  // Reset list of DTC's
  SetLength(FDTC, 0);
end;

//------------------------------------------------------------------------------
// SERVICE 03: PARSE RESPONSE
//------------------------------------------------------------------------------
procedure TOBDService03.ParseResponse(Response: TBytes);
var
  ServiceDecoder: IOBDServiceResponseDecoder;
  ErrorDecoder: IOBDResponseDecoder;
  Error: Boolean;
  I, DTCNumber: Integer;
  E: Byte;
  ServiceID, ParameterID: Integer;
  Data, Additional: TBytes;
  DTCFirstChar: Char;
begin
  // Create decoder
  ServiceDecoder := TOBDServiceResponseDecoder.Create;
  // Decode service response
  ServiceDecoder.DecodeServiceResponse(Response, Error, ServiceID, ParameterID, Data);

  // Check if the response is an error
  if Error then
  begin
    // Create decoder
    ErrorDecoder := TOBDErrorDecoder.Create;
    // Try to parse the error
    if (ErrorDecoder as TOBDErrorDecoder).Parse(Response, ServiceID, E, Additional) then
    begin
      // Notify we have a error response
      if Assigned(OnErrorResponse) then OnErrorResponse(Self, E, Additional);
    end;
    // Exit here because this is not a service response
    Exit;
  end;

  // If this response is not for this service, exit here
  if ServiceID <> GetServiceID then Exit;

  // Make sure the data is at least 2 bytes long
  if Length(Data) < 2 then Exit;
  // Make sure the data is even (2 bytes per DTC)
  if Length(Data) mod 2 <> 0 then Exit;

  // Parse DTC's
  I := 0;
  while I < Length(Data) do
  begin
    // Determine the first character based on the most significant nibble of the first byte
    case (Data[I] shr 6) of
      0: DTCFirstChar := 'P'; // Powertrain
      1: DTCFirstChar := 'C'; // Chassis
      2: DTCFirstChar := 'B'; // Body
      3: DTCFirstChar := 'U'; // Network
    else
      DTCFirstChar := '?';
    end;
    // Combine the remaining bits of the first byte with the second byte to form the DTC number
    DTCNumber := ((Data[I] and $3F) shl 8) + Data[I + 1];
    // Construct the DTC code string and add it to the list
    SetLength(FDTC, Length(FDTC) + 1);
    FDTC[Length(FDTC) -1] := TOBDServiceDiagnosticTroubleCode.Create(Format('%s%s', [DTCFirstChar, IntToHex(DTCNumber, 4)]));
    // Move to the next DTC (skip the next two bytes)
    Inc(I, 2);
  end;
end;

end.
