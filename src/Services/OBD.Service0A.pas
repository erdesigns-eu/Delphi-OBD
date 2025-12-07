//------------------------------------------------------------------------------
// UNIT           : OBD.Service0A.pas
// CONTENTS       : OBD Service 0A (Permanent Diagnostic Trouble Codes)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 19/03/2024
//------------------------------------------------------------------------------
unit OBD.Service0A;

interface

uses
  OBD.Request.Constants, OBD.Service03;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 0A (Is the same as service 03, but with permanent DTC's)
  /// </summary>
  TOBDService0A = class(TOBDService03)
  protected
    /// <summary>
    ///   Get the service id
    /// </summary>
    function GetServiceID: Byte; override;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 0A: GET SERVIVE ID
//------------------------------------------------------------------------------
function TOBDService0A.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_0A;
end;

end.
