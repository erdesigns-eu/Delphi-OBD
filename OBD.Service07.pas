//------------------------------------------------------------------------------
// UNIT           : OBD.Service07.pas
// CONTENTS       : OBD Service 07 (Show pending Diagnostic Trouble Codes)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 19/03/2024
//------------------------------------------------------------------------------
unit OBD.Service07;

interface

uses
  OBD.Request.Constants, OBD.Service03;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 07 (Is the same as service 03, but detected during current or last driving cycle)
  /// </summary>
  TOBDService07 = class(TOBDService03)
  protected
    /// <summary>
    ///   Get the service id
    /// </summary>
    function GetServiceID: Byte; override;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 07: GET SERVIVE ID
//------------------------------------------------------------------------------
function TOBDService07.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_07;
end;

end.
