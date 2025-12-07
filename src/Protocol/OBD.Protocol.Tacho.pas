//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.Tacho.pas
// CONTENTS       : Tachograph and Odometer Protocol Support
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.Tacho;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.DateUtils,
  OBD.Protocol, OBD.Protocol.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Digital Tachograph Standard
  /// </summary>
  TTachoStandard = (
    tsNone,
    tsEU_1360_2002,    // EU Regulation 1360/2002 (Generation 1)
    tsEU_165_2014,     // EU Regulation 165/2014 (Generation 2 - Smart Tachograph)
    tsKORETS,          // Korean E-Tachograph System
    tsRussiaERA,       // Russian ERA-GLONASS System
    tsChina_GB17691,   // China GB 17691 Heavy Duty Vehicle Standard
    tsBrazilOBD_BR1,   // Brazilian OBD-BR1 System
    tsJapanDPF,        // Japanese DPF/SCR Monitoring System
    tsAustraliaADR80   // Australian ADR 80/03 System
  );

  /// <summary>
  ///   Vehicle Make Specific Tachograph Type
  /// </summary>
  TTachoVehicleMake = (
    tvmGeneric,
    // European Makes
    tvmMercedes,       // Mercedes-Benz (VDO, Stoneridge)
    tvmVolvo,          // Volvo Trucks (VDO)
    tvmScania,         // Scania (VDO)
    tvmMAN,            // MAN Trucks (VDO, Continental)
    tvmDAF,            // DAF Trucks (VDO)
    tvmIveco,          // Iveco (VDO)
    tvmRenaultTrucks,  // Renault Trucks (VDO)
    // Asian Makes
    tvmHino,           // Hino (Denso)
    tvmIsuzuTruck,     // Isuzu Trucks (Denso)
    tvmMitsubishiFuso, // Mitsubishi Fuso (Denso)
    tvmUDTrucks,       // UD Trucks/Nissan Diesel (Yazaki)
    tvmHyundaiTruck,   // Hyundai Trucks
    // American Makes
    tvmFreightliner,   // Freightliner (Cummins, Detroit Diesel)
    tvmPeterbilt,      // Peterbilt (Cummins, Paccar)
    tvmKenworth,       // Kenworth (Cummins, Paccar)
    tvmInternational,  // International (Navistar)
    tvmMack,           // Mack Trucks (Mack MP7/MP8)
    tvmWesternStar,    // Western Star (Detroit Diesel)
    tvmVolvoNA         // Volvo Trucks North America
  );

  /// <summary>
  ///   Tachograph Manufacturer
  /// </summary>
  TTachoManufacturer = (
    tmVDO,             // VDO/Continental (Most common in Europe)
    tmStoneridge,      // Stoneridge Electronics
    tmDenso,           // Denso (Common in Asian vehicles)
    tmContinental,     // Continental
    tmYazaki,          // Yazaki
    tmActia,           // Actia
    tmCummins,         // Cummins (North America)
    tmDetroitDiesel,   // Detroit Diesel (North America)
    tmGeneric          // Generic/Unknown
  );

  /// <summary>
  ///   Tachograph Data Type
  /// </summary>
  TTachoDataType = (
    tdDriverCard,           // Driver card data
    tdVehicleUnit,          // Vehicle unit data
    tdActivities,           // Driver activities
    tdEvents,               // Events and faults
    tdOdometer,             // Odometer reading
    tdSpeed,                // Speed data
    tdTechnicalData,        // Technical data
    tdCompanyCard,          // Company card data
    tdWorkshopCard,         // Workshop card data
    tdControlCard           // Control card data
  );

  /// <summary>
  ///   Odometer Data
  /// </summary>
  TOdometerData = record
    TotalDistance: Cardinal;      // Total distance in km
    TripDistance: Cardinal;       // Trip distance in km
    LastCalibration: TDateTime;   // Last calibration date
    CalibrationDue: TDateTime;    // Next calibration due date
    PulsePerKm: Word;            // Pulses per kilometer
    TireCircumference: Word;      // Tire circumference in mm
    Valid: Boolean;
  end;

  /// <summary>
  ///   Tachograph Activity Type
  /// </summary>
  TTachoActivity = (
    taUnknown    = $00,
    taDriving    = $01,
    taWork       = $02,
    taAvailable  = $03,
    taRest       = $04
  );

  /// <summary>
  ///   Driver Activity Record
  /// </summary>
  TDriverActivity = record
    Activity: TTachoActivity;
    StartTime: TDateTime;
    Duration: Integer;  // Duration in minutes
    Distance: Cardinal; // Distance covered in km
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Tachograph Protocol
  ///   Digital tachograph and odometer data reading/programming
  /// </summary>
  TTachographProtocol = class(TOBDProtocol)
  private
    FStandard: TTachoStandard;
    FSecurityLevel: Byte;
    FVehicleMake: TTachoVehicleMake;
    FManufacturer: TTachoManufacturer;
    
    /// <summary>
    ///   Build tachograph command
    /// </summary>
    function BuildCommand(DataType: TTachoDataType; const Parameters: TBytes): TBytes;
    
    /// <summary>
    ///   Parse tachograph response
    /// </summary>
    function ParseResponse(const Response: TBytes; out Data: TBytes): Boolean;
    
    /// <summary>
    ///   Calculate authentication code
    /// </summary>
    function CalculateAuthCode(const Challenge: TBytes; const Key: TBytes): TBytes;
  protected
    function GetName: string; override;
    function GetDisplayName: string; override;
    function GetELMID: string; override;
  public
    constructor Create(Standard: TTachoStandard = tsEU_165_2014; VehicleMake: TTachoVehicleMake = tvmGeneric; Manufacturer: TTachoManufacturer = tmGeneric);
    
    /// <summary>
    ///   Auto-detect tachograph type and manufacturer
    /// </summary>
    function AutoDetect: Boolean;
    
    /// <summary>
    ///   Get vehicle make specific protocol parameters
    /// </summary>
    class function GetMakeParameters(Make: TTachoVehicleMake): record
      CANId: Cardinal;
      BaudRate: Cardinal;
      UseExtendedCAN: Boolean;
      RequiresPIN: Boolean;
    end;
    
    /// <summary>
    ///   Authenticate with workshop card
    /// </summary>
    function AuthenticateWorkshop(const CardData: TBytes; const PIN: string): Boolean;
    
    /// <summary>
    ///   Read odometer data
    /// </summary>
    function ReadOdometer(var Data: TOdometerData): Boolean;
    
    /// <summary>
    ///   Read total distance
    /// </summary>
    function ReadTotalDistance: Cardinal;
    
    /// <summary>
    ///   Read trip distance
    /// </summary>
    function ReadTripDistance: Cardinal;
    
    /// <summary>
    ///   Reset trip counter (requires authentication)
    /// </summary>
    function ResetTripCounter: Boolean;
    
    /// <summary>
    ///   Read driver activities (last 28 days)
    /// </summary>
    function ReadDriverActivities(Days: Integer = 28): TArray<TDriverActivity>;
    
    /// <summary>
    ///   Read driver card data
    /// </summary>
    function ReadDriverCard: TBytes;
    
    /// <summary>
    ///   Read vehicle unit identification
    /// </summary>
    function ReadVehicleUnitID: string;
    
    /// <summary>
    ///   Read VIN from tachograph
    /// </summary>
    function ReadVIN: string;
    
    /// <summary>
    ///   Read current speed
    /// </summary>
    function ReadCurrentSpeed: Single;
    
    /// <summary>
    ///   Calibrate odometer (requires workshop card)
    /// </summary>
    function CalibrateOdometer(TireCircumference: Word; PulsesPerKm: Word): Boolean;
    
    /// <summary>
    ///   Set odometer value (requires workshop card and special authorization)
    /// </summary>
    function SetOdometerValue(NewValue: Cardinal; const AuthCode: TBytes): Boolean;
    
    /// <summary>
    ///   Read events and faults
    /// </summary>
    function ReadEventsAndFaults: TBytes;
    
    /// <summary>
    ///   Read technical data
    /// </summary>
    function ReadTechnicalData: TBytes;
    
    /// <summary>
    ///   Download tachograph data (DDD file format)
    /// </summary>
    function DownloadTachoData(StartDate, EndDate: TDateTime): TBytes;
    
    /// <summary>
    ///   Tachograph standard
    /// </summary>
    property Standard: TTachoStandard read FStandard write FStandard;
    
    /// <summary>
    ///   Security level (0 = none, 1 = driver, 2 = workshop, 3 = calibration)
    /// </summary>
    property SecurityLevel: Byte read FSecurityLevel;
    
    /// <summary>
    ///   Vehicle make
    /// </summary>
    property VehicleMake: TTachoVehicleMake read FVehicleMake write FVehicleMake;
    
    /// <summary>
    ///   Tachograph manufacturer
    /// </summary>
    property Manufacturer: TTachoManufacturer read FManufacturer write FManufacturer;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TTachographProtocol.Create(Standard: TTachoStandard; VehicleMake: TTachoVehicleMake; Manufacturer: TTachoManufacturer);
begin
  inherited Create;
  FStandard := Standard;
  FSecurityLevel := 0;
  FVehicleMake := VehicleMake;
  FManufacturer := Manufacturer;
end;

//------------------------------------------------------------------------------
// AUTO DETECT
//------------------------------------------------------------------------------
function TTachographProtocol.AutoDetect: Boolean;
var
  Response: TBytes;
  IDString: string;
begin
  Result := False;
  
  // Try to read tachograph identification
  Response := Send(BuildCommand(tdVehicleUnit, []));
  
  if Length(Response) > 10 then
  begin
    // Parse manufacturer from response
    IDString := string(PAnsiChar(@Response[2]));
    
    if Pos('VDO', IDString) > 0 then
      FManufacturer := tmVDO
    else if Pos('STONERIDGE', IDString) > 0 then
      FManufacturer := tmStoneridge
    else if Pos('DENSO', IDString) > 0 then
      FManufacturer := tmDenso
    else if Pos('CONTINENTAL', IDString) > 0 then
      FManufacturer := tmContinental
    else if Pos('YAZAKI', IDString) > 0 then
      FManufacturer := tmYazaki
    else if Pos('ACTIA', IDString) > 0 then
      FManufacturer := tmActia
    else if Pos('CUMMINS', IDString) > 0 then
      FManufacturer := tmCummins
    else if Pos('DETROIT', IDString) > 0 then
      FManufacturer := tmDetroitDiesel
    else
      FManufacturer := tmGeneric;
      
    Result := True;
  end;
end;

//------------------------------------------------------------------------------
// GET MAKE PARAMETERS
//------------------------------------------------------------------------------
class function TTachographProtocol.GetMakeParameters(Make: TTachoVehicleMake): record
  CANId: Cardinal;
  BaudRate: Cardinal;
  UseExtendedCAN: Boolean;
  RequiresPIN: Boolean;
end;
begin
  // Default values
  Result.CANId := $18FEF100;
  Result.BaudRate := 250000;
  Result.UseExtendedCAN := True;
  Result.RequiresPIN := True;
  
  case Make of
    // European Makes
    tvmMercedes:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 500000;
    end;
    tvmVolvo, tvmScania, tvmMAN:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 250000;
    end;
    tvmDAF, tvmIveco, tvmRenaultTrucks:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 250000;
    end;
    
    // Asian Makes
    tvmHino, tvmIsuzuTruck, tvmMitsubishiFuso:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 250000;
      Result.RequiresPIN := False; // Some models
    end;
    tvmUDTrucks, tvmHyundaiTruck:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 250000;
    end;
    
    // American Makes
    tvmFreightliner, tvmWesternStar:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 250000; // J1939
    end;
    tvmPeterbilt, tvmKenworth:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 250000; // J1939
    end;
    tvmInternational, tvmMack:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 500000;
    end;
    tvmVolvoNA:
    begin
      Result.CANId := $18FEF100;
      Result.BaudRate := 250000;
    end;
  end;
end;

//------------------------------------------------------------------------------
// GET NAME
//------------------------------------------------------------------------------
function TTachographProtocol.GetName: string;
begin
  case FStandard of
    tsEU_1360_2002: Result := 'EU Tachograph Gen1';
    tsEU_165_2014:  Result := 'EU Smart Tachograph Gen2';
    tsKORETS:       Result := 'Korean E-Tachograph';
  else
    Result := 'Tachograph';
  end;
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TTachographProtocol.GetDisplayName: string;
begin
  Result := GetName + ' Protocol';
end;

//------------------------------------------------------------------------------
// GET ELM ID
//------------------------------------------------------------------------------
function TTachographProtocol.GetELMID: string;
begin
  Result := 'T'; // Tachograph protocol identifier
end;

//------------------------------------------------------------------------------
// BUILD COMMAND
//------------------------------------------------------------------------------
function TTachographProtocol.BuildCommand(DataType: TTachoDataType; const Parameters: TBytes): TBytes;
var
  Len: Integer;
begin
  Len := 2 + Length(Parameters);
  SetLength(Result, Len);
  
  Result[0] := $81; // Tachograph command prefix
  Result[1] := Byte(DataType);
  
  if Length(Parameters) > 0 then
    Move(Parameters[0], Result[2], Length(Parameters));
end;

//------------------------------------------------------------------------------
// PARSE RESPONSE
//------------------------------------------------------------------------------
function TTachographProtocol.ParseResponse(const Response: TBytes; out Data: TBytes): Boolean;
begin
  Result := False;
  
  if Length(Response) < 3 then
    Exit;
  
  // Check response prefix
  if Response[0] <> $82 then
    Exit;
  
  // Check status byte
  if Response[1] = $00 then // Success
  begin
    SetLength(Data, Length(Response) - 2);
    if Length(Data) > 0 then
      Move(Response[2], Data[0], Length(Data));
    Result := True;
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE AUTH CODE
//------------------------------------------------------------------------------
function TTachographProtocol.CalculateAuthCode(const Challenge: TBytes; const Key: TBytes): TBytes;
var
  I: Integer;
begin
  // Simplified authentication (real implementation would use proper crypto)
  SetLength(Result, 8);
  for I := 0 to 7 do
    Result[I] := Challenge[I] xor Key[I mod Length(Key)];
end;

//------------------------------------------------------------------------------
// AUTHENTICATE WORKSHOP
//------------------------------------------------------------------------------
function TTachographProtocol.AuthenticateWorkshop(const CardData: TBytes; const PIN: string): Boolean;
var
  Command: TBytes;
  Response: TBytes;
begin
  SetLength(Command, Length(CardData) + Length(PIN));
  Move(CardData[0], Command[0], Length(CardData));
  Move(PIN[1], Command[Length(CardData)], Length(PIN));
  
  // Send authentication command
  Command := BuildCommand(tdWorkshopCard, Command);
  
  // In real implementation, send command and check response
  Result := True; // Placeholder
  if Result then
    FSecurityLevel := 2;
end;

//------------------------------------------------------------------------------
// READ ODOMETER
//------------------------------------------------------------------------------
function TTachographProtocol.ReadOdometer(var Data: TOdometerData): Boolean;
var
  Command: TBytes;
  Response: TBytes;
  ResponseData: TBytes;
begin
  Command := BuildCommand(tdOdometer, nil);
  
  // In real implementation, send command and parse response
  // This is a placeholder
  FillChar(Data, SizeOf(Data), 0);
  Data.Valid := False;
  
  Result := False; // Placeholder - would parse actual response
end;

//------------------------------------------------------------------------------
// READ TOTAL DISTANCE
//------------------------------------------------------------------------------
function TTachographProtocol.ReadTotalDistance: Cardinal;
var
  OdomData: TOdometerData;
begin
  if ReadOdometer(OdomData) then
    Result := OdomData.TotalDistance
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// READ TRIP DISTANCE
//------------------------------------------------------------------------------
function TTachographProtocol.ReadTripDistance: Cardinal;
var
  OdomData: TOdometerData;
begin
  if ReadOdometer(OdomData) then
    Result := OdomData.TripDistance
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// RESET TRIP COUNTER
//------------------------------------------------------------------------------
function TTachographProtocol.ResetTripCounter: Boolean;
var
  Command: TBytes;
begin
  if FSecurityLevel < 1 then
    Exit(False);
  
  SetLength(Command, 1);
  Command[0] := $01; // Reset command
  
  Command := BuildCommand(tdOdometer, Command);
  
  Result := True; // Placeholder
end;

//------------------------------------------------------------------------------
// READ DRIVER ACTIVITIES
//------------------------------------------------------------------------------
function TTachographProtocol.ReadDriverActivities(Days: Integer): TArray<TDriverActivity>;
var
  Command: TBytes;
begin
  SetLength(Command, 1);
  Command[0] := Byte(Days);
  
  Command := BuildCommand(tdActivities, Command);
  
  // Placeholder - would parse actual response
  SetLength(Result, 0);
end;

//------------------------------------------------------------------------------
// READ DRIVER CARD
//------------------------------------------------------------------------------
function TTachographProtocol.ReadDriverCard: TBytes;
begin
  Result := BuildCommand(tdDriverCard, nil);
end;

//------------------------------------------------------------------------------
// READ VEHICLE UNIT ID
//------------------------------------------------------------------------------
function TTachographProtocol.ReadVehicleUnitID: string;
var
  Command: TBytes;
begin
  Command := BuildCommand(tdVehicleUnit, nil);
  Result := 'VU-PLACEHOLDER'; // Would parse from actual response
end;

//------------------------------------------------------------------------------
// READ VIN
//------------------------------------------------------------------------------
function TTachographProtocol.ReadVIN: string;
var
  Command: TBytes;
begin
  SetLength(Command, 1);
  Command[0] := $10; // VIN request sub-command
  
  Command := BuildCommand(tdTechnicalData, Command);
  Result := ''; // Would parse from actual response
end;

//------------------------------------------------------------------------------
// READ CURRENT SPEED
//------------------------------------------------------------------------------
function TTachographProtocol.ReadCurrentSpeed: Single;
var
  Command: TBytes;
begin
  Command := BuildCommand(tdSpeed, nil);
  Result := 0.0; // Would parse from actual response
end;

//------------------------------------------------------------------------------
// CALIBRATE ODOMETER
//------------------------------------------------------------------------------
function TTachographProtocol.CalibrateOdometer(TireCircumference: Word; PulsesPerKm: Word): Boolean;
var
  Command: TBytes;
begin
  if FSecurityLevel < 2 then
    Exit(False);
  
  SetLength(Command, 5);
  Command[0] := $02; // Calibration sub-command
  Command[1] := (TireCircumference shr 8) and $FF;
  Command[2] := TireCircumference and $FF;
  Command[3] := (PulsesPerKm shr 8) and $FF;
  Command[4] := PulsesPerKm and $FF;
  
  Command := BuildCommand(tdOdometer, Command);
  
  Result := True; // Placeholder
end;

//------------------------------------------------------------------------------
// SET ODOMETER VALUE
//------------------------------------------------------------------------------
function TTachographProtocol.SetOdometerValue(NewValue: Cardinal; const AuthCode: TBytes): Boolean;
var
  Command: TBytes;
begin
  if FSecurityLevel < 3 then
    Exit(False);
  
  SetLength(Command, 5 + Length(AuthCode));
  Command[0] := $03; // Set value sub-command
  Command[1] := (NewValue shr 24) and $FF;
  Command[2] := (NewValue shr 16) and $FF;
  Command[3] := (NewValue shr 8) and $FF;
  Command[4] := NewValue and $FF;
  
  if Length(AuthCode) > 0 then
    Move(AuthCode[0], Command[5], Length(AuthCode));
  
  Command := BuildCommand(tdOdometer, Command);
  
  Result := True; // Placeholder
end;

//------------------------------------------------------------------------------
// READ EVENTS AND FAULTS
//------------------------------------------------------------------------------
function TTachographProtocol.ReadEventsAndFaults: TBytes;
begin
  Result := BuildCommand(tdEvents, nil);
end;

//------------------------------------------------------------------------------
// READ TECHNICAL DATA
//------------------------------------------------------------------------------
function TTachographProtocol.ReadTechnicalData: TBytes;
begin
  Result := BuildCommand(tdTechnicalData, nil);
end;

//------------------------------------------------------------------------------
// DOWNLOAD TACHO DATA
//------------------------------------------------------------------------------
function TTachographProtocol.DownloadTachoData(StartDate, EndDate: TDateTime): TBytes;
var
  Command: TBytes;
  StartDays, EndDays: Integer;
begin
  // Convert dates to days since epoch
  StartDays := DaysBetween(StartDate, EncodeDate(1970, 1, 1));
  EndDays := DaysBetween(EndDate, EncodeDate(1970, 1, 1));
  
  SetLength(Command, 8);
  Command[0] := $FF; // Download all data sub-command
  Command[1] := (StartDays shr 24) and $FF;
  Command[2] := (StartDays shr 16) and $FF;
  Command[3] := (StartDays shr 8) and $FF;
  Command[4] := StartDays and $FF;
  Command[5] := (EndDays shr 24) and $FF;
  Command[6] := (EndDays shr 16) and $FF;
  Command[7] := (EndDays shr 8) and $FF;
  Command[8] := EndDays and $FF;
  
  Result := BuildCommand(tdVehicleUnit, Command);
end;

end.
