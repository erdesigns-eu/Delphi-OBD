//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.OBDLink.pas
// CONTENTS       : OBDLink OBD Adapter Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/03/2024
//------------------------------------------------------------------------------
unit OBD.Adapter.OBDLink;

interface

uses
  OBD.Adapter, OBD.Connection;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBDLink Adapter Exception
  /// </summary>
  TOBDLinkAdapterException = class(TOBDAdapterException);

  /// <summary>
  ///   OBDLink OBD Adapter Class
  /// </summary>
  TOBDLinkAdapter = class(TELMAdapter)
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

    /// <summary>
    ///   Write AT Command
    /// </summary>
    /// <param name="ATCommand">
    ///   The AT command string (AT Z, AT SP 0, AT R, ..)
    /// </param>
    function WriteATCommand(const ATCommand: string): Boolean; virtual;
    /// <summary>
    ///   Write ST Command
    /// </summary>
    /// <param name="STCommand">
    ///   The AT command string (STDI, STI, STFMR, ..)
    /// </param>
    function WriteSTCommand(const STCommand: string): Boolean; virtual;
    /// <summary>
    ///   Write OBD Command
    /// </summary>
    /// <param name="OBDCommand">
    ///   The OBD command string (01 00, 01 1C, ..)
    /// </param>
    function WriteOBDCommand(const OBDCommand: string): Boolean; virtual;
  end;

implementation

uses System.StrUtils;

//------------------------------------------------------------------------------
// INIT CONNECTION
//------------------------------------------------------------------------------
procedure TOBDLinkAdapter.Init;
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
constructor TOBDLinkAdapter.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDLinkAdapter.Destroy;
begin
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND
//------------------------------------------------------------------------------
function TOBDLinkAdapter.WriteATCommand(const ATCommand: string): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write AT command
  Result := Connection.WriteATCommand(IfThen(Pos(#13, ATCommand) = 0, ATCommand + #13, ATCommand));
end;

//------------------------------------------------------------------------------
// WRITE ST COMMAND
//------------------------------------------------------------------------------
function TOBDLinkAdapter.WriteSTCommand(const STCommand: string): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write ST command
  Result := Connection.WriteSTCommand(IfThen(Pos(#13, STCommand) = 0, STCommand + #13, STCommand));
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND
//------------------------------------------------------------------------------
function TOBDLinkAdapter.WriteOBDCommand(const OBDCommand: string): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write OBD command
  Result := Connection.WriteOBDCommand(IfThen(Pos(#13, OBDCommand) = 0, OBDCommand + #13, OBDCommand));
end;

end.
