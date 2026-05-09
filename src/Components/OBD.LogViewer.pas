//------------------------------------------------------------------------------
// UNIT           : OBD.LogViewer.pas
// CONTENTS       : In-app live log viewer (extends TOBDTerminal as a sink)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Drop-in TOBDTerminal subclass that also implements
//                  IOBDLogSink. Register it with TOBDLogger.RegisterSink
//                  and every log event renders into the terminal with
//                  the appropriate severity colour.
//------------------------------------------------------------------------------
unit OBD.LogViewer;

interface

uses
  System.SysUtils, System.Classes, System.UITypes,
  Vcl.Controls, Vcl.Graphics,

  OBD.Terminal, OBD.Logger.Sinks;

type
  TOBDLogViewer = class(TOBDTerminal, IOBDLogSink)
  private
    FShowLevelTag: Boolean;
    procedure SetShowLevelTag(const AValue: Boolean);
  protected
    // TInterfacedObject-style ref counting on a TComponent — VCL
    // components are not reference-counted by default, so we provide
    // a no-op IInterface implementation that hands out the same instance
    // to every QueryInterface caller.
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Render a single log event.
    /// </summary>
    procedure Write(const Event: TOBDLogEvent);
    procedure Flush;
  published
    /// <summary>
    ///   Prefix every line with [LEVEL] when true (default).
    /// </summary>
    property ShowLevelTag: Boolean read FShowLevelTag write SetShowLevelTag default True;
  end;

implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDLogViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowLevelTag := True;
end;

//------------------------------------------------------------------------------
// SET SHOW LEVEL TAG
//------------------------------------------------------------------------------
procedure TOBDLogViewer.SetShowLevelTag(const AValue: Boolean);
begin
  if FShowLevelTag <> AValue then FShowLevelTag := AValue;
end;

//------------------------------------------------------------------------------
// QUERY INTERFACE
//------------------------------------------------------------------------------
function TOBDLogViewer.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

//------------------------------------------------------------------------------
// _ADD REF
//------------------------------------------------------------------------------
function TOBDLogViewer._AddRef: Integer;
begin
  // VCL controls own themselves — never reference count.
  Result := -1;
end;

//------------------------------------------------------------------------------
// _RELEASE
//------------------------------------------------------------------------------
function TOBDLogViewer._Release: Integer;
begin
  Result := -1;
end;

//------------------------------------------------------------------------------
// WRITE
//------------------------------------------------------------------------------
procedure TOBDLogViewer.Write(const Event: TOBDLogEvent);
var
  Tag, Source, Body: string;
begin
  Tag := '';
  Source := '';
  if FShowLevelTag then Tag := '[' + LogLevelName(Event.Level) + '] ';
  if Event.Source <> '' then Source := '[' + Event.Source + '] ';
  Body := Tag + Source + Event.Message;

  // The terminal's existing direction-coloured rows give us severity
  // colouring for free.
  case Event.Level of
    lsDebug:    LogInfo(Body);
    lsInfo:     LogInfo(Body);
    lsWarning:  LogReceived(Body);   // green-ish — visible without alarm
    lsError:    LogError(Body);
    lsCritical: LogError(Body);
  else
    LogInfo(Body);
  end;
end;

//------------------------------------------------------------------------------
// FLUSH
//------------------------------------------------------------------------------
procedure TOBDLogViewer.Flush;
begin
  // Nothing to flush — terminal renders synchronously.
end;

end.
