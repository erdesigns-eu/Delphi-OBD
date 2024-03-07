//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.ELM327.pas
// CONTENTS       : ELM327 OBD Adapter Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/03/2024
//------------------------------------------------------------------------------
unit OBD.Adapter.ELM327;

interface

uses
  OBD.Adapter, OBD.Connection;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   ELM327 OBD Adapter Class
  /// </summary>
  TELM327Adapter = class(TELMAdapter)
  private

  protected
    /// <summary>
    ///   Initialize connection
    /// </summary>
    procedure Init; override;
  public
    /// <summary>
    ///   Constructor
    /// <summary>
    constructor Create; override;
    /// <summary>
    ///   Destructor
    /// <summary>
    destructor Destroy; override;
  end;

implementation

//------------------------------------------------------------------------------
// INIT CONNECTION
//------------------------------------------------------------------------------
procedure TELM327Adapter.Init;
begin
  // RESET_ALL     - ATZ
  // ECHO_OFF      - ATE0
  // HEADERS_ON    - ATH1
  // LINEFEEDS_OFF - ATL0
  // READ_VOLTAGE  - ATRV
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TELM327Adapter.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TELM327Adapter.Destroy;
begin
  // Call inherited destructor
  inherited Destroy;
end;



end.
