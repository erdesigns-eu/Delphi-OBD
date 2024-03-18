//------------------------------------------------------------------------------
// UNIT           : OBD.Service.Types.pas
// CONTENTS       : OBD Service Types
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 15/03/2024
//------------------------------------------------------------------------------
unit OBD.Service.Types;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections;

//------------------------------------------------------------------------------
// COMPARERS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Comparer for supported PID's
  /// </summary>
  TOBDServiceSupportedPIDComparer = class(TInterfacedObject, IComparer<Byte>)
  public
    function Compare(const Left, Right: Byte): Integer;
  end;

//------------------------------------------------------------------------------
// OTHER
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 01 Parameter 03 (Fuel system status)
  /// </summary>
  TOBDServiceFuelSystemStatus = (fsUnknown, fssMotorOff, fssOpenLoopInsufficientTemp, fssClosedLoop, fssOpenLoopEngineLoad, fssOpenLoopSystemFailure, fssClosedLoopFault);
  /// <summary>
  ///   OBD Service 01 Parameter 12 (Commanded secondary air status)
  /// </summary>
  TOBDServiceCommandedSecondaryAirStatus = (sasUnknown, sasUpstream, sasDownstream, sasAtmosphereOff, sasDiagnostics);
  /// <summary>
  ///   OBD Service 01 Parameter 1E (Auxiliary input status)
  /// </summary>
  TOBDServiceAuxilaryInputStatus = (aisUnknown, aisPowerTakeOff, aisActive);
  /// <summary>
  ///   OBD Service 03 (Diagnostic Trouble Codes) Category
  /// </summary>
  TOBDServiceDTCCategory = (ccUnknown, ccPowerTrain, ccChassis, ccBody, ccNetwork);
  /// <summary>
  ///   OBD Service 03 (Diagnostic Trouble Codes) Origin
  /// </summary>
  TOBDServiceDTCOrigin = (coUnknown, coGeneric, coManufacturer);

//------------------------------------------------------------------------------
// EVENTS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Service Error Response event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="Error">
  ///   Error (NSC) Byte.
  /// </param>
  /// <param name="AdditionalData">
  ///   Additional data bytes (optional) - Manufacturer specific
  /// </param>
  TOBDServiceErrorResponseEvent = procedure(Sender: TObject; Error: Byte; AdditionalData: TBytes) of object;

  /// <summary>
  ///   OBD Service Data event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="SID">
  ///   Service ID
  /// </param>
  /// <param name="PID">
  ///   Parameter ID
  /// </param>
  TOBDServiceDataEvent = procedure(Sender: TObject; SID: Byte; PID: Byte) of object;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service Diagnostic Trouble Code (INTERFACE)
  /// </summary>
  IOBDServiceDiagnosticTroubleCode = interface
    ['{42764FB3-12D3-4711-B9A5-42D9A3F1AD3B}']
    /// <summary>
    ///   Get the category
    /// </summary>
    function GetCategory: TOBDServiceDTCCategory;
    /// <summary>
    ///   Get the origin
    /// </summary>
    function GetOrigin: TOBDServiceDTCOrigin;
    /// <summary>
    ///   Get the Diagnostic Trouble Code String
    /// </summary>
    function GetDTC: string;

    /// <summary>
    ///   DTC category
    /// </summary>
    property Category: TOBDServiceDTCCategory read GetCategory;
    /// <summary>
    ///   DTC origin
    /// </summary>
    property Origin: TOBDServiceDTCOrigin read GetOrigin;
    /// <summary>
    ///   DTC string
    /// </summary>
    property DTC: string read GetDTC;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service Diagnostic Trouble Code (CLASS)
  /// </summary>
  TOBDServiceDiagnosticTroubleCode = class
  private
    /// <summary>
    ///   Category
    /// </summary>
    FCategory: TOBDServiceDTCCategory;
    /// <summary>
    ///   Origin
    /// </summary>
    FOrigin: TOBDServiceDTCOrigin;
    /// <summary>
    ///   DTC string
    /// </summary>
    FDTC: string;
  protected
    /// <summary>
    ///   Get the category
    /// </summary>
    function GetCategory: TOBDServiceDTCCategory;
    /// <summary>
    ///   Get the origin
    /// </summary>
    function GetOrigin: TOBDServiceDTCOrigin;
    /// <summary>
    ///   Get the Diagnostic Trouble Code String
    /// </summary>
    function GetDTC: string;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(DTC: string); virtual;

    /// <summary>
    ///   DTC category
    /// </summary>
    property Category: TOBDServiceDTCCategory read GetCategory;
    /// <summary>
    ///   DTC origin
    /// </summary>
    property Origin: TOBDServiceDTCOrigin read GetOrigin;
    /// <summary>
    ///   DTC string
    /// </summary>
    property DTC: string read GetDTC;
  end;

implementation

//------------------------------------------------------------------------------
// COMPARE SUPPORTED PID
//------------------------------------------------------------------------------
function TOBDServiceSupportedPIDComparer.Compare(const Left, Right: Byte): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// DTC: GET CATEGORY
//------------------------------------------------------------------------------
function TOBDServiceDiagnosticTroubleCode.GetCategory: TOBDServiceDTCCategory;
begin
  Result := FCategory;
end;

//------------------------------------------------------------------------------
// DTC: GET ORIGIN
//------------------------------------------------------------------------------
function TOBDServiceDiagnosticTroubleCode.GetOrigin: TOBDServiceDTCOrigin;
begin
  Result := FOrigin;
end;

//------------------------------------------------------------------------------
// DTC: GET DTC STRING
//------------------------------------------------------------------------------
function TOBDServiceDiagnosticTroubleCode.GetDTC: string;
begin
  Result := FDTC;
end;

//------------------------------------------------------------------------------
// DTC: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDServiceDiagnosticTroubleCode.Create(DTC: string);
begin
  // Call inherited constructor
  inherited Create;
  // Set DTC string
  FDTC := DTC;
  // Set category
  case DTC[1] of
    'P': FCategory := ccPowerTrain;
    'C': FCategory := ccChassis;
    'B': FCategory := ccBody;
    'U': FCategory := ccNetwork;
    else FCategory := ccUnknown;
  end;
  // Set origin
  case DTC[2] of
    '0': FOrigin := coGeneric;
    '1': FOrigin := coManufacturer;
    else FOrigin := coUnknown;
  end;
end;

end.
