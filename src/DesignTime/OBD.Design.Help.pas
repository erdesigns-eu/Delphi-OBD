//------------------------------------------------------------------------------
//  OBD.Design.Help
//
//  Help-keyword registration for the Delphi-OBD design-time
//  package. Maps every published Delphi-OBD component class to a
//  documentation anchor under <c>docs/components.md</c> so an F1
//  press on a component selected in the form designer resolves
//  to the right page.
//
//  The IDE help service in modern RAD Studio (12+) consumes the
//  registered keyword via the host's installed help collection.
//  This unit emits the keyword names; configuring a help
//  collection that maps them to URLs (online docs, local
//  CHM / PDF, …) is a host-deployment step documented in
//  <c>docs/components.md#help-keywords</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Help;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>
  ///   Keyword registry the design-time package publishes.
  /// </summary>
  /// <remarks>
  ///   Each entry is a (component-class-name, keyword) pair. The
  ///   keyword is the documentation-anchor convention
  ///   <c>delphi-obd:&lt;ClassName&gt;</c>. Hosts that build a
  ///   help collection map each keyword to a URL or local file
  ///   path.
  /// </remarks>
  TOBDHelpKeyword = record
    /// <summary>Component class name (e.g. <c>'TOBDLiveData'</c>).</summary>
    ClassName: string;
    /// <summary>Help keyword (e.g.
    /// <c>'delphi-obd:TOBDLiveData'</c>).</summary>
    Keyword: string;
  end;

/// <summary>
///   Registers every Delphi-OBD design-time help keyword.
/// </summary>
/// <remarks>
///   Called from <c>OBD.Design.Registration.Register</c>.
///   Subsequent calls are idempotent — the underlying registry
///   deduplicates by class name.
/// </remarks>
procedure RegisterDelphiOBDHelpKeywords;

/// <summary>
///   Returns the full keyword table, useful for IDE host
///   tooling that wants to iterate the mappings (e.g. a
///   help-collection generator).
/// </summary>
/// <returns>Copy of the internal table.</returns>
function DelphiOBDHelpKeywords: TArray<TOBDHelpKeyword>;

implementation

var
  GKeywords: TDictionary<string, string>;

procedure Ensure;
begin
  if GKeywords = nil then
    GKeywords := TDictionary<string, string>.Create;
end;

procedure AddOne(const AClassName: string);
begin
  Ensure;
  GKeywords.AddOrSetValue(AClassName,
    'delphi-obd:' + AClassName);
end;

procedure RegisterDelphiOBDHelpKeywords;
const
  // Every component class exposed by the design-time package.
  // Listed here rather than walked via RTTI so a future refactor
  // doesn't silently drop a keyword.
  AllClasses: array[0..47] of string = (
    // Connection / adapter / protocol stack
    'TOBDConnection', 'TOBDAdapter', 'TOBDProtocol',
    'TOBDDoIPClient', 'TOBDSecOCCodec',

    // Service-mode (Phase 5)
    'TOBDLiveData', 'TOBDDTCs', 'TOBDVIN', 'TOBDVINInspector',
    'TOBDFreezeFrame', 'TOBDOnBoardMonitor', 'TOBDActuator',
    'TOBDVehicleHealth', 'TOBDDriveCycleAdvisor', 'TOBDEVBattery',
    'TOBDClearDTC', 'TOBDOxygenMonitor', 'TOBDDataSource',
    'TOBDWWHOBD', 'TOBDWWHReadiness',

    // Diagnostics (Phase 6)
    'TOBDUDS', 'TOBDUDSReset', 'TOBDUDSReadMemory',
    'TOBDUDSIOControl', 'TOBDUDSReadDID', 'TOBDUDSReadDTC',
    'TOBDUDSReadByPeriodic', 'TOBDUDSDynamicDID',
    'TOBDKWP', 'TOBDKWPReadID', 'TOBDKWPReadDTC',
    'TOBDKWPIOControl', 'TOBDKWPRoutine',
    'TOBDJ1939', 'TOBDJ1939DM',

    // Coding / flashing
    'TOBDSecurityAccess', 'TOBDDataIdentifierIO', 'TOBDRoutineControl',
    'TOBDFlasher', 'TOBDUDSTransfer', 'TOBDFlashPipeline',
    'TOBDUDSWriteMemory', 'TOBDUDSWriteDID', 'TOBDKWPWriteID',

    // Calibration / speciality
    'TOBDXCP', 'TOBDCCP', 'TOBDIsoBus',
    'TOBDOEMCatalog');
var
  S: string;
begin
  for S in AllClasses do
    AddOne(S);
end;

function DelphiOBDHelpKeywords: TArray<TOBDHelpKeyword>;
var
  Pair: TPair<string, string>;
  N: Integer;
  Entry: TOBDHelpKeyword;
begin
  Ensure;
  SetLength(Result, GKeywords.Count);
  N := 0;
  for Pair in GKeywords do
  begin
    Entry.ClassName := Pair.Key;
    Entry.Keyword := Pair.Value;
    Result[N] := Entry;
    Inc(N);
  end;
end;

initialization

finalization
  FreeAndNil(GKeywords);

end.
