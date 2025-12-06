//------------------------------------------------------------------------------
// UNIT           : OBD.Service08.pas
// CONTENTS       : OBD Service 08 (Request Control of On-Board System/Component)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.Service08;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  OBD.Request.Constants, OBD.Request.Encoders, OBD.Response.Decoders,
  OBD.Service.Types, OBD.Service;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Control Test Result
  /// </summary>
  TOBDControlTestResult = record
    /// <summary>
    ///   Test ID (TID)
    /// </summary>
    TestID: Byte;
    /// <summary>
    ///   Control status
    /// </summary>
    ControlStatus: Byte;
    /// <summary>
    ///   Test value/result
    /// </summary>
    TestValue: Integer;
    /// <summary>
    ///   Minimum limit
    /// </summary>
    MinLimit: Integer;
    /// <summary>
    ///   Maximum limit
    /// </summary>
    MaxLimit: Integer;
    /// <summary>
    ///   Test description
    /// </summary>
    Description: string;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 08 - Request Control of On-Board System, Test, or Component
  ///   
  ///   Allows bidirectional control of on-board systems for diagnostic purposes.
  ///   Common test IDs include:
  ///   $00 - Reserved
  ///   $01 - IAC System (Idle Air Control)
  ///   $02 - Ignition System
  ///   $03 - Fuel System
  ///   $04 - EGR System
  ///   $05 - VVT System (Variable Valve Timing)
  ///   $06 - EVAP System
  ///   $07 - O2 Sensor Heater
  ///   $08 - O2 Sensor
  ///   $09 - Secondary Air System
  ///   $0A - Thermostat
  ///   $0B - Catalyst Heater
  ///   
  ///   NOTE: This service is used primarily by scan tools and may require
  ///   special security access or may be restricted by the vehicle manufacturer.
  /// </summary>
  TOBDService08 = class(TOBDService)
  private
    /// <summary>
    ///   List of control test results
    /// </summary>
    FTestResults: TList<TOBDControlTestResult>;
    /// <summary>
    ///   Supported TIDs
    /// </summary>
    FSupportedTIDs: TList<Byte>;
  protected
    /// <summary>
    ///   Get the service id
    /// </summary>
    function GetServiceID: Byte; override;
    /// <summary>
    ///   Get test description from TID
    /// </summary>
    function GetTestDescription(const TID: Byte): string;
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
    procedure ParseResponse(const Response: TBytes); override;
    /// <summary>
    ///   Get test results as array
    /// </summary>
    function GetTestResultsArray: TArray<TOBDControlTestResult>;

    /// <summary>
    ///   List of control test results
    /// </summary>
    property TestResults: TList<TOBDControlTestResult> read FTestResults;
    /// <summary>
    ///   Supported Test IDs
    /// </summary>
    property SupportedTIDs: TList<Byte> read FSupportedTIDs;
  end;

  /// <summary>
  ///   Service 08 Request Encoder
  /// </summary>
  TOBDService08RequestEncoder = class(TOBDRequestEncoder)
  public
    /// <summary>
    ///   Encode request for control test
    /// </summary>
    function EncodeControlTestRequest(const TestID: Byte; const ControlParameter: Byte = $00): TBytes;
    /// <summary>
    ///   Encode request for supported TIDs
    /// </summary>
    function EncodeSupportedTIDsRequest: TBytes;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 08: GET SERVICE ID
//------------------------------------------------------------------------------
function TOBDService08.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_08;
end;

//------------------------------------------------------------------------------
// SERVICE 08: GET TEST DESCRIPTION
//------------------------------------------------------------------------------
function TOBDService08.GetTestDescription(const TID: Byte): string;
begin
  case TID of
    $00: Result := 'Supported TIDs';
    $01: Result := 'IAC System (Idle Air Control)';
    $02: Result := 'Ignition System';
    $03: Result := 'Fuel System';
    $04: Result := 'EGR System';
    $05: Result := 'VVT System (Variable Valve Timing)';
    $06: Result := 'EVAP System (Evaporative Emissions)';
    $07: Result := 'O2 Sensor Heater';
    $08: Result := 'O2 Sensor';
    $09: Result := 'Secondary Air System';
    $0A: Result := 'Thermostat';
    $0B: Result := 'Catalyst Heater';
    $0C: Result := 'A/C System';
    $0D: Result := 'Generator/Alternator';
    $0E: Result := 'Fuel Pump';
    $0F: Result := 'PCV Valve';
    $10: Result := 'Turbocharger/Supercharger';
    $11: Result := 'Transmission';
    $12: Result := 'Throttle Actuator Control';
    $13: Result := 'Exhaust Gas Recirculation';
    $14: Result := 'Fuel Injector';
    $15: Result := 'Intake Manifold Tuning Valve';
    $20: Result := 'Supported TIDs [21-40]';
    $40: Result := 'Supported TIDs [41-60]';
    $60: Result := 'Supported TIDs [61-80]';
    $80: Result := 'Supported TIDs [81-A0]';
    $A0: Result := 'Supported TIDs [A1-C0]';
    $C0: Result := 'Supported TIDs [C1-E0]';
  else
    Result := Format('Unknown Test ID $%X', [TID]);
  end;
end;

//------------------------------------------------------------------------------
// SERVICE 08: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService08.Create;
begin
  inherited Create;
  FTestResults := TList<TOBDControlTestResult>.Create;
  FSupportedTIDs := TList<Byte>.Create;
end;

//------------------------------------------------------------------------------
// SERVICE 08: DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService08.Destroy;
begin
  FTestResults.Free;
  FSupportedTIDs.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 08: RESET
//------------------------------------------------------------------------------
procedure TOBDService08.Reset;
begin
  FTestResults.Clear;
  FSupportedTIDs.Clear;
end;

//------------------------------------------------------------------------------
// SERVICE 08: PARSE RESPONSE
//------------------------------------------------------------------------------
procedure TOBDService08.ParseResponse(const Response: TBytes);
var
  I: Integer;
  TestResult: TOBDControlTestResult;
  TID: Byte;
begin
  // Response format: [48] [TID] [Control Status] [Data...]
  // For supported TIDs: [48] [TID] [4 bytes bitmap]
  // For control results: [48] [TID] [Status] [ValueHi] [ValueLo] [MinHi] [MinLo] [MaxHi] [MaxLo]
  
  if Length(Response) < 2 then Exit;
  if Response[0] <> ($08 + $40) then Exit;

  TID := Response[1];
  
  // Check if this is a supported TIDs response
  if (TID = $00) or (TID = $20) or (TID = $40) or (TID = $60) or 
     (TID = $80) or (TID = $A0) or (TID = $C0) then
  begin
    // Parse supported TIDs bitmap (4 bytes = 32 bits)
    if Length(Response) >= 6 then
    begin
      for I := 0 to 31 do
      begin
        if ((Response[2 + (I div 8)] shr (7 - (I mod 8))) and 1) = 1 then
          FSupportedTIDs.Add(TID + I + 1);
      end;
    end;
  end
  else
  begin
    // Parse control test result
    if Length(Response) >= 10 then
    begin
      TestResult.TestID := TID;
      TestResult.ControlStatus := Response[2];
      TestResult.TestValue := (Response[3] shl 8) or Response[4];
      TestResult.MinLimit := (Response[5] shl 8) or Response[6];
      TestResult.MaxLimit := (Response[7] shl 8) or Response[8];
      TestResult.Description := GetTestDescription(TID);
      
      FTestResults.Add(TestResult);
    end;
  end;
end;

//------------------------------------------------------------------------------
// SERVICE 08: GET TEST RESULTS ARRAY
//------------------------------------------------------------------------------
function TOBDService08.GetTestResultsArray: TArray<TOBDControlTestResult>;
begin
  Result := FTestResults.ToArray;
end;

//------------------------------------------------------------------------------
// SERVICE 08 REQUEST ENCODER: ENCODE CONTROL TEST REQUEST
//------------------------------------------------------------------------------
function TOBDService08RequestEncoder.EncodeControlTestRequest(const TestID: Byte; 
  const ControlParameter: Byte): TBytes;
begin
  // Service 08 with TID and optional control parameter
  SetLength(Result, 3);
  Result[0] := OBD_SERVICE_08;
  Result[1] := TestID;
  Result[2] := ControlParameter;
end;

//------------------------------------------------------------------------------
// SERVICE 08 REQUEST ENCODER: ENCODE SUPPORTED TIDS REQUEST
//------------------------------------------------------------------------------
function TOBDService08RequestEncoder.EncodeSupportedTIDsRequest: TBytes;
begin
  // Request supported TIDs (TID $00)
  SetLength(Result, 2);
  Result[0] := OBD_SERVICE_08;
  Result[1] := $00;
end;

end.
