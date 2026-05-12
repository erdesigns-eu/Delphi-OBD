//------------------------------------------------------------------------------
//  OBD.UI.LogViewer
//
//  TOBDLogViewer — descends from <see cref="TOBDTerminal"/> and
//  prefixes every line with a severity tag. Wires through the
//  Terminal's direction-coloured paint pipeline (info / error
//  rows already get the right foreground) and adds a Trace / Warn
//  alias on top.
//
//  A future <c>TOBDLogger</c> (Tier-4 port) will accept this
//  component as a log sink via its <c>RegisterSink</c> hook. Until
//  the logger lands, hosts feed the viewer directly by calling
//  <see cref="WriteLog"/>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.LogViewer.pas, on top
//                     of the VCL-native v2 Terminal.
//------------------------------------------------------------------------------

unit OBD.UI.LogViewer;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  OBD.UI.Terminal;

type
  /// <summary>Severity level for one log row.</summary>
  TOBDLogLevel = (
    /// <summary>Verbose tracing — colours like <c>tdInfo</c>.</summary>
    llTrace,
    /// <summary>Informational — colours like <c>tdInfo</c>.</summary>
    llInfo,
    /// <summary>Warning — distinct amber-style colour.</summary>
    llWarn,
    /// <summary>Error — colours like <c>tdError</c>.</summary>
    llError
  );

  /// <summary>
  ///   In-app log viewer.
  /// </summary>
  /// <remarks>
  ///   Each call to <see cref="WriteLog"/> appends a row in the
  ///   form <c>[LEVEL] text</c> (the <c>[LEVEL]</c> prefix is
  ///   suppressed when <see cref="ShowLevelTag"/> is <c>False</c>).
  ///   The row's direction tag is derived from the severity, so
  ///   the underlying Terminal paint colours each level cleanly.
  /// </remarks>
  TOBDLogViewer = class(TOBDTerminal)
  strict private
    FShowLevelTag: Boolean;
    FWarnColor: TColor;
    procedure SetShowLevelTag(AValue: Boolean);
    function DirectionFor(ALevel: TOBDLogLevel): TOBDTerminalDirection;
    function PrefixFor(ALevel: TOBDLogLevel): string;
  public
    /// <summary>Constructs the viewer with <c>ShowLevelTag = True</c>.
    /// </summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Appends one log row.
    /// </summary>
    /// <param name="ALevel">Severity level.</param>
    /// <param name="AText">Row text (the level tag is prefixed
    /// automatically when <c>ShowLevelTag</c> is <c>True</c>).</param>
    procedure WriteLog(ALevel: TOBDLogLevel; const AText: string);

    /// <summary>Convenience for <c>WriteLog(llTrace, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Trace(const AText: string);
    /// <summary>Convenience for <c>WriteLog(llInfo, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Info(const AText: string);
    /// <summary>Convenience for <c>WriteLog(llWarn, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Warn(const AText: string);
    /// <summary>Convenience for <c>WriteLog(llError, AText)</c>.</summary>
    /// <param name="AText">Row text.</param>
    procedure Error(const AText: string);
  published
    /// <summary>
    ///   Prefix every row with a <c>[TRACE] / [INFO] / [WARN] /
    ///   [ERROR]</c> tag. Default <c>True</c>.
    /// </summary>
    property ShowLevelTag: Boolean read FShowLevelTag
      write SetShowLevelTag default True;

    /// <summary>
    ///   Foreground colour for <c>llWarn</c> rows. Default amber
    ///   (<c>$0000A5FF</c>).
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
    llTrace, llInfo: Result := tdInfo;
    llWarn:          Result := tdInfo;
    llError:         Result := tdError;
  else
    Result := tdInfo;
  end;
end;

function TOBDLogViewer.PrefixFor(ALevel: TOBDLogLevel): string;
begin
  if not FShowLevelTag then
    Exit('');
  case ALevel of
    llTrace: Result := '[TRACE] ';
    llInfo:  Result := '[INFO]  ';
    llWarn:  Result := '[WARN]  ';
    llError: Result := '[ERROR] ';
  else
    Result := '';
  end;
end;

procedure TOBDLogViewer.WriteLog(ALevel: TOBDLogLevel;
  const AText: string);
var
  PrevInfo: TColor;
begin
  // llWarn re-uses tdInfo but with a distinct foreground colour for
  // this call — restore InfoColor afterwards so other tdInfo rows
  // keep their normal grey paint.
  if ALevel = llWarn then
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

procedure TOBDLogViewer.Trace(const AText: string);
begin
  WriteLog(llTrace, AText);
end;

procedure TOBDLogViewer.Info(const AText: string);
begin
  WriteLog(llInfo, AText);
end;

procedure TOBDLogViewer.Warn(const AText: string);
begin
  WriteLog(llWarn, AText);
end;

procedure TOBDLogViewer.Error(const AText: string);
begin
  WriteLog(llError, AText);
end;

end.
