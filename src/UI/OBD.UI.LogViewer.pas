//------------------------------------------------------------------------------
//  OBD.UI.LogViewer
//
//  TOBDLogViewer — descends from <see cref="TOBDTerminal"/> and
//  prefixes every line with a severity tag. Wires through the
//  Terminal's direction-coloured paint pipeline (info / error
//  rows already get the right foreground) and adds a Trace /
//  Warn alias on top.
//
//  Implements <see cref="IOBDLogSink"/> so it can be registered
//  with <c>TOBDLogger.Instance.RegisterSink</c> and receive
//  every <c>TOBDLogger.Log</c> call automatically. As a
//  <c>TComponent</c> the viewer is not reference-counted; the
//  unit hands out a no-op <c>_AddRef</c> / <c>_Release</c>
//  pair so the logger's interface reference doesn't try to
//  free a designer-owned control.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.UI.LogViewer;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  OBD.UI.Terminal,
  OBD.Logger.Sinks;

type
  /// <summary>
  ///   In-app log viewer.
  /// </summary>
  /// <remarks>
  ///   Each call to <see cref="WriteLog"/> appends a row in the
  ///   form <c>[LEVEL] text</c> (the <c>[LEVEL]</c> prefix is
  ///   suppressed when <see cref="ShowLevelTag"/> is <c>False</c>).
  ///   The row's direction tag is derived from the severity, so
  ///   the underlying Terminal paint colours each level cleanly.
  ///
  ///   Implements <see cref="IOBDLogSink"/> — register the
  ///   instance with the process-wide <c>TOBDLogger</c> to mirror
  ///   every logged event into the form.
  /// </remarks>
  TOBDLogViewer = class(TOBDTerminal, IOBDLogSink)
  strict private
    FShowLevelTag: Boolean;
    FWarnColor: TColor;
    procedure SetShowLevelTag(AValue: Boolean);
    function DirectionFor(ALevel: TOBDLogLevel): TOBDTerminalDirection;
    function PrefixFor(ALevel: TOBDLogLevel): string;
  protected
    // TComponent is not reference-counted; provide a no-op
    // IInterface so the logger can hold an IOBDLogSink reference
    // without trying to free the form-owned viewer.
    function QueryInterface(const IID: TGUID; out Obj): HResult;
      stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    /// <summary>Constructs the viewer with
    /// <c>ShowLevelTag = True</c>.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Appends one log row.
    /// </summary>
    /// <param name="ALevel">Severity level.</param>
    /// <param name="AText">Row text (the level tag is prefixed
    /// automatically when <c>ShowLevelTag</c> is <c>True</c>).</param>
    procedure WriteLog(ALevel: TOBDLogLevel; const AText: string);

    /// <summary>Convenience for <c>WriteLog(olDebug, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Debug(const AText: string);
    /// <summary>Convenience for <c>WriteLog(olInfo, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Info(const AText: string);
    /// <summary>Convenience for <c>WriteLog(olWarning, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Warn(const AText: string);
    /// <summary>Convenience for <c>WriteLog(olError, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Error(const AText: string);
    /// <summary>Convenience for <c>WriteLog(olCritical, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Critical(const AText: string);

    // ---- IOBDLogSink ----
    /// <summary>Sink-protocol entry — appends the event as one
    /// row prefixed by category and level.</summary>
    /// <param name="AEvent">Event to write.</param>
    procedure Write(const AEvent: TOBDLogEvent); reintroduce;
    /// <summary>Sink-protocol flush — synchronous render, no
    /// pending output.</summary>
    procedure Flush;
  published
    /// <summary>
    ///   Prefix every row with a <c>[DEBUG] / [INFO] / [WARN] /
    ///   [ERROR] / [FATAL]</c> tag. Default <c>True</c>.
    /// </summary>
    property ShowLevelTag: Boolean read FShowLevelTag
      write SetShowLevelTag default True;

    /// <summary>
    ///   Foreground colour for <c>olWarning</c> rows. Default
    ///   amber (<c>$0000A5FF</c>).
    /// </summary>
    property WarnColor: TColor read FWarnColor write FWarnColor
      default TColor($0000A5FF);
  end;

implementation

constructor TOBDLogViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowLevelTag := True;
  FWarnColor := TColor($0000A5FF);
end;

procedure TOBDLogViewer.SetShowLevelTag(AValue: Boolean);
begin
  if FShowLevelTag = AValue then
    Exit;
  FShowLevelTag := AValue;
  Invalidate;
end;

function TOBDLogViewer.DirectionFor(
  ALevel: TOBDLogLevel): TOBDTerminalDirection;
begin
  case ALevel of
    olDebug, olInfo, olWarning: Result := tdInfo;
    olError, olCritical:        Result := tdError;
  else
    Result := tdInfo;
  end;
end;

function TOBDLogViewer.PrefixFor(ALevel: TOBDLogLevel): string;
begin
  if not FShowLevelTag then
    Exit('');
  case ALevel of
    olDebug:    Result := '[DEBUG] ';
    olInfo:     Result := '[INFO]  ';
    olWarning:  Result := '[WARN]  ';
    olError:    Result := '[ERROR] ';
    olCritical: Result := '[FATAL] ';
  else
    Result := '';
  end;
end;

procedure TOBDLogViewer.WriteLog(ALevel: TOBDLogLevel;
  const AText: string);
var
  PrevInfo: TColor;
begin
  // olWarning re-uses tdInfo with a distinct foreground colour
  // for this call only — restore InfoColor afterwards so other
  // tdInfo rows keep their normal grey paint.
  if ALevel = olWarning then
  begin
    PrevInfo := InfoColor;
    InfoColor := FWarnColor;
    try
      Log(DirectionFor(ALevel), PrefixFor(ALevel) + AText);
    finally
      InfoColor := PrevInfo;
    end;
  end
  else
    Log(DirectionFor(ALevel), PrefixFor(ALevel) + AText);
end;

procedure TOBDLogViewer.Debug(const AText: string);
begin
  WriteLog(olDebug, AText);
end;

procedure TOBDLogViewer.Info(const AText: string);
begin
  WriteLog(olInfo, AText);
end;

procedure TOBDLogViewer.Warn(const AText: string);
begin
  WriteLog(olWarning, AText);
end;

procedure TOBDLogViewer.Error(const AText: string);
begin
  WriteLog(olError, AText);
end;

procedure TOBDLogViewer.Critical(const AText: string);
begin
  WriteLog(olCritical, AText);
end;

procedure TOBDLogViewer.Write(const AEvent: TOBDLogEvent);
var
  Body: string;
begin
  if AEvent.Category <> '' then
    Body := '[' + AEvent.Category + '] ' + AEvent.Message
  else
    Body := AEvent.Message;
  WriteLog(AEvent.Level, Body);
end;

procedure TOBDLogViewer.Flush;
begin
  // The viewer renders synchronously into the underlying listbox;
  // nothing pending.
end;

function TOBDLogViewer.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TOBDLogViewer._AddRef: Integer;
begin
  // Form-owned control — ref counting is a no-op.
  Result := -1;
end;

function TOBDLogViewer._Release: Integer;
begin
  Result := -1;
end;

end.
