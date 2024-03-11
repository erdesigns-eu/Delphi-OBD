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
  OBD.Adapter, OBD.Adapter.Constants, OBD.Connection;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   ELM327 Adapter Exception
  /// </summary>
  TELM327AdapterException = class(TOBDAdapterException);

  /// <summary>
  ///   ELM327 OBD Adapter Class
  /// </summary>
  TELM327Adapter = class(TELMAdapter)
  protected
    /// <summary>
    ///   Initialize connection
    /// </summary>
    function Init: Boolean; override;
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
    ///   Write AT Command (ASYNC)
    /// </summary>
    /// <param name="ATCommand">
    ///   The AT command string (AT Z, AT SP 0, AT R, ..)
    /// </param>
    function WriteATCommand(const ATCommand: string): Boolean; virtual;
    /// <summary>
    ///   Write AT Command (SYNC)
    /// </summary>
    /// <param name="ATCommand">
    ///   The AT command string (AT Z, AT SP 0, AT R, ..)
    /// </param>
    function WriteATCommandSync(const ATCommand: string; const Timeout: Integer = 5000): string; virtual;
    /// <summary>
    ///   Write OBD Command (ASYNC)
    /// </summary>
    /// <param name="OBDCommand">
    ///   The OBD command string (01 00, 01 1C, ..)
    /// </param>
    function WriteOBDCommand(const OBDCommand: string): Boolean; virtual;
    /// <summary>
    ///   Write OBD Command (SYNC)
    /// </summary>
    /// <param name="OBDCommand">
    ///   The OBD command string (01 00, 01 1C, ..)
    /// </param>
    function WriteOBDCommandSync(const OBDCommand: string; const Timeout: Integer = 5000): string; virtual;
  end;

implementation

uses System.StrUtils;

//------------------------------------------------------------------------------
// INIT CONNECTION
//------------------------------------------------------------------------------
function TELM327Adapter.Init: Boolean;
begin
  // initialize result
  Result := inherited Init;
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

//------------------------------------------------------------------------------
// WRITE AT COMMAND (ASYNC)
//------------------------------------------------------------------------------
function TELM327Adapter.WriteATCommand(const ATCommand: string): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write AT command
  Result := Connection.WriteATCommand(IfThen(Pos(ELM_COMMAND_TERMINATOR, ATCommand) = 0, ATCommand + ELM_COMMAND_TERMINATOR, ATCommand));
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND (SYNC)
//------------------------------------------------------------------------------
function TELM327Adapter.WriteATCommandSync(const ATCommand: string; const Timeout: Integer = 5000): string;
begin
  // initialize result
  Result := '';
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write AT command
  Result := WriteCommandSync(ctATCommand, IfThen(Pos(ELM_COMMAND_TERMINATOR, ATCommand) = 0, ATCommand + ELM_COMMAND_TERMINATOR, ATCommand), Timeout);
end;

//------------------------------------------------------------------------------
// WRITE OBD COMMAND (ASYNC)
//------------------------------------------------------------------------------
function TELM327Adapter.WriteOBDCommand(const OBDCommand: string): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write OBD command
  Result := Connection.WriteOBDCommand(IfThen(Pos(ELM_COMMAND_TERMINATOR, OBDCommand) = 0, OBDCommand + ELM_COMMAND_TERMINATOR, OBDCommand));
end;

//------------------------------------------------------------------------------
// WRITE OBD COMMAND (SYNC)
//------------------------------------------------------------------------------
function TELM327Adapter.WriteOBDCommandSync(const OBDCommand: string; const Timeout: Integer = 5000): string;
begin
  // initialize result
  Result := '';
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write AT command
  Result := WriteCommandSync(ctOBDCommand, IfThen(Pos(ELM_COMMAND_TERMINATOR, OBDCommand) = 0, OBDCommand + ELM_COMMAND_TERMINATOR, OBDCommand), Timeout);
end;

end.
