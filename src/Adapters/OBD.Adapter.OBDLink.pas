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
  OBD.Adapter, OBD.Adapter.Constants, OBD.Connection;

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
    ///   Write ST Command (ASYNC)
    /// </summary>
    /// <param name="STCommand">
    ///   The AT command string (STDI, STI, STFMR, ..)
    /// </param>
    function WriteSTCommand(const STCommand: string): Boolean; virtual;
    /// <summary>
    ///   Write ST Command (SYNC)
    /// </summary>
    /// <param name="STCommand">
    ///   The AT command string (STDI, STI, STFMR, ..)
    /// </param>
    function WriteSTCommandSync(const STCommand: string; const Timeout: Integer = 5000): string; virtual;
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
function TOBDLinkAdapter.Init: Boolean;
begin
  // initialize result
  Result := inherited Init;
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
// WRITE AT COMMAND (ASYNC)
//------------------------------------------------------------------------------
function TOBDLinkAdapter.WriteATCommand(const ATCommand: string): Boolean;
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
function TOBDLinkAdapter.WriteATCommandSync(const ATCommand: string; const Timeout: Integer = 5000): string;
begin
  // initialize result
  Result := '';
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write AT command
  Result := WriteCommandSync(ctATCommand, IfThen(Pos(ELM_COMMAND_TERMINATOR, ATCommand) = 0, ATCommand + ELM_COMMAND_TERMINATOR, ATCommand), Timeout);
end;

//------------------------------------------------------------------------------
// WRITE ST COMMAND (ASYNC)
//------------------------------------------------------------------------------
function TOBDLinkAdapter.WriteSTCommand(const STCommand: string): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write ST command
  Result := Connection.WriteSTCommand(IfThen(Pos(ELM_COMMAND_TERMINATOR, STCommand) = 0, STCommand + ELM_COMMAND_TERMINATOR, STCommand));
end;

//------------------------------------------------------------------------------
// WRITE ST COMMAND (SYNC)
//------------------------------------------------------------------------------
function TOBDLinkAdapter.WriteSTCommandSync(const STCommand: string; const Timeout: Integer = 5000): string;
begin
  // initialize result
  Result := '';
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write AT command
  Result := WriteCommandSync(ctSTCommand, IfThen(Pos(ELM_COMMAND_TERMINATOR, STCommand) = 0, STCommand + ELM_COMMAND_TERMINATOR, STCommand), Timeout);
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND (ASYNC)
//------------------------------------------------------------------------------
function TOBDLinkAdapter.WriteOBDCommand(const OBDCommand: string): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write OBD command
  Result := Connection.WriteOBDCommand(IfThen(Pos(ELM_COMMAND_TERMINATOR, OBDCommand) = 0, OBDCommand + ELM_COMMAND_TERMINATOR, OBDCommand));
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND (SYNC)
//------------------------------------------------------------------------------
function TOBDLinkAdapter.WriteOBDCommandSync(const OBDCommand: string; const Timeout: Integer = 5000): string;
begin
  // initialize result
  Result := '';
  // Exit here if we're not connected
  if not Connected then Exit;
  // Write AT command
  Result := WriteCommandSync(ctOBDCommand, IfThen(Pos(ELM_COMMAND_TERMINATOR, OBDCommand) = 0, OBDCommand + ELM_COMMAND_TERMINATOR, OBDCommand), Timeout);
end;

end.
