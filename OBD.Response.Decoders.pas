//------------------------------------------------------------------------------
// UNIT           : OBD.Response.Decoders.pas
// CONTENTS       : OBD Response Decoders
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 15/03/2024
//------------------------------------------------------------------------------
unit OBD.Response.Decoders;

interface

uses
  System.SysUtils,

  OBD.Request.Constants, OBD.Response.Constants, OBD.Service.Types;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service Response Decoder (INTERFACE)
  /// </summary>
  IOBDServiceResponseDecoder = interface
    ['{508C8798-8B71-4B30-8DB4-B619089E072E}']
    /// <summary>
    ///   Decode service response
    /// </summary>
    function DecodeServiceResponse(Response: TBytes; var Error: Boolean; var ServiceID: Byte; var PID: Byte; var Data: TBytes): Boolean;
  end;

  /// <summary>
  ///   OBD Response Decoder (INTERFACE)
  /// </summary>
  IOBDResponseDecoder = interface
    ['{437EC6E2-F7C0-4660-8185-0F0C60DCB439}']
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service Response Decoder (CLASS)
  /// </summary>
  TOBDServiceResponseDecoder = class(TInterfacedObject, IOBDServiceResponseDecoder)
  protected
    /// <summary>
    ///   Decode service response
    /// </summary>
    function DecodeServiceResponse(Response: TBytes; var Error: Boolean; var ServiceID: Byte; var PID: Byte; var Data: TBytes): Boolean; virtual;
  end;

  /// <summary>
  ///   Base OBD Response Decoder (CLASS)
  /// </summary>
  TOBDResponseDecoder = class(TInterfacedObject, IOBDResponseDecoder)
  end;

  /// <summary>
  ///   OBD Error Decoder
  /// </summary>
  TOBDErrorDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the error from the response data
    /// </summary>
    function Parse(Data: TBytes; var ServiceID: Byte; var Error: Byte; var AdditionalData: TBytes): Boolean;
  end;

  /// <summary>
  ///   OBD Supported PID Decoder
  /// </summary>
  TOBDSupportedPIDDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the supported PID's from the response data
    /// </summary>
    function Parse(PID: Byte; Data: TBytes; var SupportedPID: TBytes): Boolean;
  end;

  /// <summary>
  ///   OBD Fuel System Status Decoder
  /// </summary>
  TOBDFuelSystemStatusDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the fuel system status
    /// </summary>
    function Parse(Data: TBytes; var FuelSystem1: TOBDServiceFuelSystemStatus; var FuelSystem2: TOBDServiceFuelSystemStatus): Boolean;
  end;

  /// <summary>
  ///   OBD Percentage Decoder
  /// </summary>
  TOBDPercentageDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the value to a scale of 0-100%
    /// </summary>
    function Parse(Data: TBytes; var Percent: Double): Boolean;  //Result := (Response[0] / 255) * 100;
  end;

implementation

//------------------------------------------------------------------------------
// DECODE SERVICE RESPONSE
//------------------------------------------------------------------------------
function TOBDServiceResponseDecoder.DecodeServiceResponse(Response: TBytes; var Error: Boolean; var ServiceID: Byte; var PID: Byte; var Data: TBytes): Boolean;
begin
  // initialize result
  Result := False;
  // initialize error
  Error := False;

  // Make sure the response is at least 2 bytes
  if Length(Response) < 2 then Exit;

  // Check if this is a error response
  if Response[0] = OBD_NEGATIVE_RESPONSE then
  begin
    Error := True;
    Exit;
  end;

  // Make sure we get a response to a service (OBD-II starts from 01)
  if Response[0] < $40 then Exit;

  // Get the service PID
  ServiceID := Response[0] - $40;
  // Get the PID
  PID := Response[1];
  // Get the data (if there is any)
  if Length(Response) > 2 then Data := Copy(Response, 2, Length(Response) - 2);

  // If we make it until here, decoding succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// ERROR DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDErrorDecoder.Parse(Data: TBytes; var ServiceID: Byte; var Error: Byte; var AdditionalData: TBytes): Boolean;
begin
  // initialize result
  Result := False;
  // initialize error
  Error := OBD_UNKNOWN_ERROR;

  // Make sure we have at least 3 bytes
  if Length(Data) < 3 then Exit;

  // Get the service ID
  ServiceID := Data[1];
  // Get the error
  Error := Data[2];
  // Get additional data (Manufacturer specific)
  if Length(Data) > 3 then AdditionalData := Copy(Data, 3, Length(Data) - 3);

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// PID DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDSupportedPIDDecoder.Parse(PID: Byte; Data: TBytes; var SupportedPID: TBytes): Boolean;
var
  I, BitPos, StartPID, Index: Integer;
begin
  // initialize result
  Result := False;
  // Set the length of the array
  SetLength(SupportedPID, 0);

  // Make sure we have at least 4 bytes
  if Length(Data) < 4 then Exit;

  // Calculate the starting PID based on the input PID
  StartPID := (PID div 32) * 32 + 1;

  // Initialize index for supported PIDs
  Index := 0;
  // Loop through each byte of the data
  for I := 0 to High(Data) do
  begin
    // Check each bit in the byte
    for BitPos := 7 downto 0 do
    begin
      if (Data[I] and (1 shl BitPos)) <> 0 then
      begin
        // If the bit is set, this PID is supported, add it to the array
        // of supported PID's.
        SetLength(SupportedPID, Length(SupportedPID) + 1);
        SupportedPID[Index] := StartPID + I * 8 + (7 - BitPos);
        Inc(Index);
      end;
    end;
  end;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// FUEL SYSTEM STATUS DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDFuelSystemStatusDecoder.Parse(Data: TBytes; var FuelSystem1: TOBDServiceFuelSystemStatus; var FuelSystem2: TOBDServiceFuelSystemStatus): Boolean;

  function ByteToFuelSystemStatus(Value: Byte): TOBDServiceFuelSystemStatus;
  begin
    case Value of
      0: Result := fssMotorOff;
      1: Result := fssOpenLoopInsufficientTemp;
      2: Result := fssClosedLoop;
      4: Result := fssOpenLoopEngineLoad;
      8: Result := fssOpenLoopSystemFailure;
      16: Result := fssClosedLoopFault;
    else
      Result := fsUnknown;
    end;
  end;

begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;

  // Get status for fuel system 1
  FuelSystem1 := ByteToFuelSystemStatus(Data[0]);
  // Get status for fuel system 2
  FuelSystem2 := ByteToFuelSystemStatus(Data[1]);

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// PERCENTAGE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDPercentageDecoder.Parse(Data: TBytes; var Percent: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 1 byte
  if Length(Data) < 1 then Exit;
  // Get the percentage
  Percent := (Data[0] / 255) * 100;

  // If we make it until here, parsing succeeded
  Result := True;
end;

end.
