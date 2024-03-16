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
  TSupportedPIDComparer = class(TInterfacedObject, IComparer<Byte>)
  public
    function Compare(const Left, Right: Byte): Integer;
  end;

//------------------------------------------------------------------------------
// OTHER
//------------------------------------------------------------------------------
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
  ///   Service Live Data event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="SID">
  ///   Live data Service ID
  /// </param>
  /// <param name="PID">
  ///   Live data Parameter ID
  /// </param>
  TOBDLiveDataEvent = procedure(Sender: TObject; SID: Byte; PID: Byte) of object;

implementation

//------------------------------------------------------------------------------
// COMPARE SUPPORTED PID
//------------------------------------------------------------------------------
function TSupportedPIDComparer.Compare(const Left, Right: Byte): Integer;
begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
end;

end.
