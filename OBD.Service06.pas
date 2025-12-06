//------------------------------------------------------------------------------
// UNIT           : OBD.Service06.pas
// CONTENTS       : OBD Service 06 (On-Board Monitoring Test Results)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.Service06;

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
  ///   On-Board Monitoring Test Result
  /// </summary>
  TOBDMonitoringTestResult = record
    /// <summary>
    ///   Test ID (TID)
    /// </summary>
    TestID: Byte;
    /// <summary>
    ///   Component ID (CID) - identifies specific component
    /// </summary>
    ComponentID: Byte;
    /// <summary>
    ///   Test value (actual measured value)
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
    ///   Test passed (value within limits)
    /// </summary>
    Passed: Boolean;
    /// <summary>
    ///   Test name/description
    /// </summary>
    Description: string;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 06 - Request On-Board Monitoring Test Results
  ///   Provides access to test results for continuously and non-continuously
  ///   monitored systems
  /// </summary>
  TOBDService06 = class(TOBDService)
  private
    /// <summary>
    ///   List of test results
    /// </summary>
    FTestResults: TList<TOBDMonitoringTestResult>;
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
    function GetTestResultsArray: TArray<TOBDMonitoringTestResult>;

    /// <summary>
    ///   List of monitoring test results
    /// </summary>
    property TestResults: TList<TOBDMonitoringTestResult> read FTestResults;
  end;

  /// <summary>
  ///   Service 06 Request Encoder
  /// </summary>
  TOBDService06RequestEncoder = class(TOBDRequestEncoder)
  public
    /// <summary>
    ///   Encode request for specific test ID
    /// </summary>
    function EncodeMonitoringTestRequest(const TestID: Byte = $00): TBytes;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 06: GET SERVICE ID
//------------------------------------------------------------------------------
function TOBDService06.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_06;
end;

//------------------------------------------------------------------------------
// SERVICE 06: GET TEST DESCRIPTION
//------------------------------------------------------------------------------
function TOBDService06.GetTestDescription(const TID: Byte): string;
begin
  case TID of
    $00: Result := 'Supported TIDs';
    $01: Result := 'Oxygen Sensor Monitor Bank 1 Sensor 1';
    $02: Result := 'Oxygen Sensor Monitor Bank 1 Sensor 2';
    $03: Result := 'Oxygen Sensor Monitor Bank 1 Sensor 3';
    $04: Result := 'Oxygen Sensor Monitor Bank 1 Sensor 4';
    $05: Result := 'Oxygen Sensor Monitor Bank 2 Sensor 1';
    $06: Result := 'Oxygen Sensor Monitor Bank 2 Sensor 2';
    $07: Result := 'Oxygen Sensor Monitor Bank 2 Sensor 3';
    $08: Result := 'Oxygen Sensor Monitor Bank 2 Sensor 4';
    $09: Result := 'Oxygen Sensor Monitor Bank 3 Sensor 1';
    $0A: Result := 'Oxygen Sensor Monitor Bank 3 Sensor 2';
    $0B: Result := 'Oxygen Sensor Monitor Bank 3 Sensor 3';
    $0C: Result := 'Oxygen Sensor Monitor Bank 3 Sensor 4';
    $0D: Result := 'Oxygen Sensor Monitor Bank 4 Sensor 1';
    $0E: Result := 'Oxygen Sensor Monitor Bank 4 Sensor 2';
    $0F: Result := 'Oxygen Sensor Monitor Bank 4 Sensor 3';
    $10: Result := 'Oxygen Sensor Monitor Bank 4 Sensor 4';
    $20: Result := 'Supported TIDs [21-40]';
    $21: Result := 'Catalyst Monitor Bank 1';
    $22: Result := 'Catalyst Monitor Bank 2';
    $23: Result := 'Catalyst Monitor Bank 3';
    $24: Result := 'Catalyst Monitor Bank 4';
    $31: Result := 'EGR Monitor Bank 1';
    $32: Result := 'EGR Monitor Bank 2';
    $33: Result := 'VVT System Bank 1';
    $34: Result := 'VVT System Bank 2';
    $35: Result := 'EVAP System (0.090")';
    $36: Result := 'EVAP System (0.040")';
    $37: Result := 'EVAP System (0.020")';
    $38: Result := 'Purge Flow Monitor';
    $40: Result := 'Supported TIDs [41-60]';
    $41: Result := 'Oxygen Sensor Heater Bank 1 Sensor 1';
    $42: Result := 'Oxygen Sensor Heater Bank 1 Sensor 2';
    $43: Result := 'Oxygen Sensor Heater Bank 2 Sensor 1';
    $44: Result := 'Oxygen Sensor Heater Bank 2 Sensor 2';
    $45: Result := 'Oxygen Sensor Heater Bank 3 Sensor 1';
    $46: Result := 'Oxygen Sensor Heater Bank 3 Sensor 2';
    $47: Result := 'Oxygen Sensor Heater Bank 4 Sensor 1';
    $48: Result := 'Oxygen Sensor Heater Bank 4 Sensor 2';
    $4D: Result := 'Heated Catalyst Temperature Bank 1 Sensor 1';
    $4E: Result := 'Heated Catalyst Temperature Bank 2 Sensor 1';
    $4F: Result := 'Heated Catalyst Temperature Bank 1 Sensor 2';
    $50: Result := 'Heated Catalyst Temperature Bank 2 Sensor 2';
  else
    Result := Format('Unknown Test ID $%X', [TID]);
  end;
end;

//------------------------------------------------------------------------------
// SERVICE 06: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService06.Create;
begin
  inherited Create;
  FTestResults := TList<TOBDMonitoringTestResult>.Create;
end;

//------------------------------------------------------------------------------
// SERVICE 06: DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService06.Destroy;
begin
  FTestResults.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 06: RESET
//------------------------------------------------------------------------------
procedure TOBDService06.Reset;
begin
  FTestResults.Clear;
end;

//------------------------------------------------------------------------------
// SERVICE 06: PARSE RESPONSE
//------------------------------------------------------------------------------
procedure TOBDService06.ParseResponse(const Response: TBytes);
var
  I: Integer;
  TestResult: TOBDMonitoringTestResult;
begin
  // Response format: [46] [TID] [CID] [ValueHi] [ValueLo] [MinHi] [MinLo] [MaxHi] [MaxLo] ...
  // Each test result is 9 bytes (TID + CID + 3 words)
  
  FTestResults.Clear;
  
  if Length(Response) < 2 then Exit;
  if Response[0] <> ($06 + $40) then Exit;

  I := 1;
  while I + 8 <= Length(Response) do
  begin
    TestResult.TestID := Response[I];
    TestResult.ComponentID := Response[I + 1];
    TestResult.TestValue := (Response[I + 2] shl 8) or Response[I + 3];
    TestResult.MinLimit := (Response[I + 4] shl 8) or Response[I + 5];
    TestResult.MaxLimit := (Response[I + 6] shl 8) or Response[I + 7];
    TestResult.Passed := (TestResult.TestValue >= TestResult.MinLimit) and 
                         (TestResult.TestValue <= TestResult.MaxLimit);
    TestResult.Description := GetTestDescription(TestResult.TestID);

    FTestResults.Add(TestResult);
    Inc(I, 8);
  end;
end;

//------------------------------------------------------------------------------
// SERVICE 06: GET TEST RESULTS ARRAY
//------------------------------------------------------------------------------
function TOBDService06.GetTestResultsArray: TArray<TOBDMonitoringTestResult>;
begin
  Result := FTestResults.ToArray;
end;

//------------------------------------------------------------------------------
// SERVICE 06 REQUEST ENCODER: ENCODE MONITORING TEST REQUEST
//------------------------------------------------------------------------------
function TOBDService06RequestEncoder.EncodeMonitoringTestRequest(const TestID: Byte): TBytes;
begin
  // Service 06 with optional TID parameter
  SetLength(Result, 2);
  Result[0] := OBD_SERVICE_06;
  Result[1] := TestID; // $00 for all supported tests
end;

end.
