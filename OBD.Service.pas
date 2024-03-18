//------------------------------------------------------------------------------
// UNIT           : OBD.Service.pas
// CONTENTS       : OBD Service
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 15/03/2024
//------------------------------------------------------------------------------
unit OBD.Service;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  OBD.Service.Types;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service (INTERFACE)
  /// </summary>
  IOBDService = interface
    ['{E76AAF33-A74D-4224-ACA1-4D63E5003793}']
    /// <summary>
    ///   Get the service id (e.g. $01, $02, ...)
    /// </summary>
    function GetServiceID: Byte;
    /// <summary>
    ///   Parse service response
    /// </summary>
    procedure ParseResponse(Response: TBytes);
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service (CLASS)
  /// </summary>
  TOBDService = class(TInterfacedObject, IOBDService)
  private
    /// <summary>
    ///   Error response event
    /// </summary>
    FOnErrorResponse: TOBDServiceErrorResponseEvent;
  protected
    /// <summary>
    ///   Get the service id (e.g. $01, $02, ...)
    /// </summary>
    function GetServiceID: Byte; virtual; abstract;
  public
    /// <summary>
    ///   Parse service response
    /// </summary>
    procedure ParseResponse(Response: TBytes); virtual; abstract;

    /// <summary>
    ///   Error response event
    /// </summary>
    property OnErrorResponse: TOBDServiceErrorResponseEvent read FOnErrorResponse write FOnErrorResponse;
  end;

implementation

end.

