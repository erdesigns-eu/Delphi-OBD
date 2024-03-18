//------------------------------------------------------------------------------
// UNIT           : OBD.Service02.pas
// CONTENTS       : OBD Service 02
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 18/03/2024
//------------------------------------------------------------------------------
unit OBD.Service02;

interface

uses
  OBD.Request.Constants, OBD.Service01;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 02 (Is the same as service 01, but Freeze-Frame data)
  /// </summary>
  TOBDService02 = class(TOBDService01)
  protected
    /// <summary>
    ///   Get the service id
    /// </summary>
    function GetServiceID: Byte; override;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 02: GET SERVIVE ID
//------------------------------------------------------------------------------
function TOBDService02.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_02;
end;

end.
