//------------------------------------------------------------------------------
//  OBD.Adapter.Commands
//
//  Unified AT + ST command catalogue and the single FormatCommand
//  helper. Replaces the v1 dual OBD.Adapter.ATCommands /
//  OBD.Adapter.STCommands modules — a single TOBDAdapterCommand record
//  shape covers both, distinguished only by the Kind field.
//
//  This unit ships the most-used built-in commands. OEM packages and
//  user code may register additional commands via
//  TOBDAdapterCommandCatalog.Register.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ELM327 datasheet rev 2.3 (Elm Electronics)
//    - OBDLink ST command reference (ScanTool / OBDSolutions)
//
//  History     :
//    2026-05-09  ERD  Phase 3 initial: ~30 built-in AT commands, ~12 ST
//                     commands, FormatCommand with %s/%d/%x/%xx
//                     placeholders + capability gating.
//------------------------------------------------------------------------------

unit OBD.Adapter.Commands;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types,
  OBD.Adapter.Types;

type
  /// <summary>
  ///   Process-wide command catalogue. Entries are keyed by upper-case
  ///   verb (e.g. <c>'ATZ'</c>, <c>'STI'</c>).
  /// </summary>
  /// <remarks>
  ///   Built-in commands are seeded in this unit's initialisation.
  ///   OEM packages may register additional commands (vendor-specific
  ///   AT or ST verbs) via <see cref="Register"/>.
  /// </remarks>
  TOBDAdapterCommandCatalog = class
  strict private
    class var FInstance: TOBDAdapterCommandCatalog;
    FByVerb: TDictionary<string, TOBDAdapterCommand>;
    procedure SeedBuiltins;
  public
    /// <summary>Constructs an empty catalogue and seeds built-ins.</summary>
    constructor Create;
    /// <summary>Releases internal storage.</summary>
    destructor Destroy; override;

    /// <summary>The shared process-wide instance.</summary>
    /// <returns>Singleton; created on first call.</returns>
    class function Default: TOBDAdapterCommandCatalog;
    /// <summary>Releases the shared instance.</summary>
    class procedure ReleaseDefault;

    /// <summary>Registers (or replaces) a command.</summary>
    /// <param name="ACommand">Command to register. <c>Verb</c> must be
    /// non-empty.</param>
    /// <exception cref="EOBDConfig"><c>Verb</c> is empty.</exception>
    procedure Register(const ACommand: TOBDAdapterCommand);

    /// <summary>Looks up a command by verb (case-insensitive).</summary>
    /// <param name="AVerb">Verb (e.g. <c>'ATZ'</c>).</param>
    /// <param name="ACommand">Output, valid only when True.</param>
    /// <returns>True when the verb is registered.</returns>
    function TryFind(const AVerb: string;
      out ACommand: TOBDAdapterCommand): Boolean;

    /// <summary>Number of registered commands.</summary>
    function Count: Integer;
  end;

/// <summary>
///   Substitutes parameters into a command template.
/// </summary>
/// <param name="ATemplate">Template with placeholders. Supported:
/// <c>%s</c> (string), <c>%d</c> (decimal integer), <c>%x</c> (hex
/// digit, lowercase), <c>%X</c> (hex digit, uppercase), <c>%xx</c> /
/// <c>%XX</c> (two hex digits zero-padded), <c>%xxxx</c> /
/// <c>%XXXX</c> (four hex digits).</param>
/// <param name="AParams">Parameter values; substituted in order.</param>
/// <returns>Formatted command string.</returns>
/// <exception cref="EOBDConfig">Template has more placeholders than
/// <c>AParams</c> provides, or an unknown placeholder is present.</exception>
function FormatCommand(const ATemplate: string;
  const AParams: array of const): string;

/// <summary>
///   Validates that an adapter with the given capability set can
///   execute a command.
/// </summary>
/// <param name="ACommand">Command from the catalogue.</param>
/// <param name="ACapabilities">Adapter capability set.</param>
/// <param name="AMissing">When False, contains a human-readable name
/// of the missing capability.</param>
/// <returns>True when the adapter has all required capabilities.</returns>
function CommandSupportedBy(const ACommand: TOBDAdapterCommand;
  const ACapabilities: TOBDAdapterCapabilities;
  out AMissing: string): Boolean;

implementation

uses
  System.TypInfo;

{ ---- helpers ----------------------------------------------------------------- }

function CapabilityName(ACap: TOBDAdapterCapability): string;
begin
  Result := GetEnumName(TypeInfo(TOBDAdapterCapability), Ord(ACap));
  if (Length(Result) > 2) and (Result[1] = 'a') and (Result[2] = 'c') then
    Result := Copy(Result, 3, MaxInt);
end;

function FormatCommand(const ATemplate: string;
  const AParams: array of const): string;
var
  I, ParamIdx: Integer;
  Ch: Char;
  Token: string;
  Width: Integer;
  Lower: Boolean;
begin
  Result := '';
  ParamIdx := 0;
  I := 1;
  while I <= Length(ATemplate) do
  begin
    Ch := ATemplate[I];
    if Ch <> '%' then
    begin
      Result := Result + Ch;
      Inc(I);
      Continue;
    end;
    // Placeholder
    Inc(I);
    if I > Length(ATemplate) then
      raise EOBDConfig.Create('FormatCommand: trailing % in template');

    Token := '';
    while (I <= Length(ATemplate)) and CharInSet(ATemplate[I],
      ['s', 'd', 'x', 'X']) do
    begin
      Token := Token + ATemplate[I];
      Inc(I);
    end;
    if Token = '' then
      raise EOBDConfig.CreateFmt(
        'FormatCommand: unknown placeholder near "%%%s"',
        [Copy(ATemplate, I, 1)]);

    if ParamIdx > High(AParams) then
      raise EOBDConfig.CreateFmt(
        'FormatCommand: too few parameters for template "%s"', [ATemplate]);

    case Token[1] of
      's':
        begin
          if AParams[ParamIdx].VType = vtUnicodeString then
            Result := Result + UnicodeString(AParams[ParamIdx].VUnicodeString)
          else if AParams[ParamIdx].VType = vtAnsiString then
            Result := Result + string(AnsiString(AParams[ParamIdx].VAnsiString))
          else if AParams[ParamIdx].VType = vtString then
            Result := Result + string(PShortString(AParams[ParamIdx].VString)^)
          else
            raise EOBDConfig.Create('FormatCommand: %s expects a string');
        end;
      'd':
        begin
          if AParams[ParamIdx].VType = vtInteger then
            Result := Result + IntToStr(AParams[ParamIdx].VInteger)
          else if AParams[ParamIdx].VType = vtInt64 then
            Result := Result + IntToStr(AParams[ParamIdx].VInt64^)
          else
            raise EOBDConfig.Create('FormatCommand: %d expects an integer');
        end;
      'x', 'X':
        begin
          Width := Length(Token);  // %x = 1 hex digit, %xx = 2, %xxxx = 4
          Lower := Token[1] = 'x';
          if AParams[ParamIdx].VType = vtInteger then
            Result := Result + IntToHex(AParams[ParamIdx].VInteger, Width)
          else if AParams[ParamIdx].VType = vtInt64 then
            Result := Result + IntToHex(AParams[ParamIdx].VInt64^, Width)
          else
            raise EOBDConfig.Create('FormatCommand: %x expects an integer');
          if Lower then
            Result := LowerCase(Result);
        end;
    end;
    Inc(ParamIdx);
  end;
end;

function CommandSupportedBy(const ACommand: TOBDAdapterCommand;
  const ACapabilities: TOBDAdapterCapabilities;
  out AMissing: string): Boolean;
var
  Cap: TOBDAdapterCapability;
begin
  AMissing := '';
  for Cap := Low(TOBDAdapterCapability) to High(TOBDAdapterCapability) do
    if (Cap in ACommand.RequiredCapabilities) and not (Cap in ACapabilities) then
    begin
      AMissing := CapabilityName(Cap);
      Exit(False);
    end;
  Result := True;
end;

{ ---- TOBDAdapterCommandCatalog ----------------------------------------------- }

constructor TOBDAdapterCommandCatalog.Create;
begin
  inherited;
  FByVerb := TDictionary<string, TOBDAdapterCommand>.Create;
  SeedBuiltins;
end;

destructor TOBDAdapterCommandCatalog.Destroy;
begin
  FByVerb.Free;
  inherited;
end;

class function TOBDAdapterCommandCatalog.Default: TOBDAdapterCommandCatalog;
begin
  if FInstance = nil then
    FInstance := TOBDAdapterCommandCatalog.Create;
  Result := FInstance;
end;

class procedure TOBDAdapterCommandCatalog.ReleaseDefault;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDAdapterCommandCatalog.Register(
  const ACommand: TOBDAdapterCommand);
var
  Key: string;
begin
  Key := UpperCase(Trim(ACommand.Verb));
  if Key = '' then
    raise EOBDConfig.Create('Command verb cannot be empty');
  FByVerb.AddOrSetValue(Key, ACommand);
end;

function TOBDAdapterCommandCatalog.TryFind(const AVerb: string;
  out ACommand: TOBDAdapterCommand): Boolean;
begin
  Result := FByVerb.TryGetValue(UpperCase(Trim(AVerb)), ACommand);
end;

function TOBDAdapterCommandCatalog.Count: Integer;
begin
  Result := FByVerb.Count;
end;

procedure TOBDAdapterCommandCatalog.SeedBuiltins;
  procedure AddAT(const AVerb, ADescription: string;
    ARequired: TOBDAdapterCapabilities = []);
  var
    C: TOBDAdapterCommand;
  begin
    C.Kind := ckAT;
    C.Verb := AVerb;
    C.Description := ADescription;
    C.RequiredCapabilities := ARequired;
    Register(C);
  end;
  procedure AddST(const AVerb, ADescription: string;
    ARequired: TOBDAdapterCapabilities = []);
  var
    C: TOBDAdapterCommand;
  begin
    C.Kind := ckST;
    C.Verb := AVerb;
    C.Description := ADescription;
    C.RequiredCapabilities := ARequired + [acSTCommands];
    Register(C);
  end;
begin
  // ---- AT commands -------------------------------------------------
  AddAT('ATZ',         'Reset all (warm reset).');
  AddAT('AT WS',       'Warm soft reset.');
  AddAT('ATD',         'Restore defaults.');
  AddAT('ATE0',        'Echo off.');
  AddAT('ATE1',        'Echo on.');
  AddAT('ATL0',        'Linefeeds off.');
  AddAT('ATL1',        'Linefeeds on.');
  AddAT('ATH0',        'Headers off.');
  AddAT('ATH1',        'Headers on.');
  AddAT('ATS0',        'Spaces off (compact responses).');
  AddAT('ATS1',        'Spaces on.');
  AddAT('ATAT0',       'Adaptive timing off.');
  AddAT('ATAT1',       'Adaptive timing 1 (default).');
  AddAT('ATAT2',       'Adaptive timing 2 (aggressive).');
  AddAT('ATSP%d',      'Set protocol (0=auto).');
  AddAT('ATTP%d',      'Try protocol n (does not save).');
  AddAT('ATSP A%d',    'Set protocol with auto fallback.');
  AddAT('ATDPN',       'Display current protocol number.');
  AddAT('ATDP',        'Display current protocol.');
  AddAT('ATSH %xxxxxx','Set CAN/header. Use %xxxx for 11-bit / %xxxxxx for 29-bit.',
    [acProgrammableHeaders]);
  AddAT('ATCRA %xxxx', 'Set CAN receive-address filter.',
    [acMessageFilters]);
  AddAT('ATCAF0',      'CAN auto-formatting off.', [acCAN]);
  AddAT('ATCAF1',      'CAN auto-formatting on.', [acCAN]);
  AddAT('ATSTM',       'Set max wait time multiplier.');
  AddAT('ATST %xx',    'Set timeout (×4 ms).');
  AddAT('ATAR',        'Auto receive on (default).');
  AddAT('ATAL',        'Allow long messages (>7 bytes).',
    [acAdapterMultiFrame]);
  AddAT('ATR0',        'Responses off.');
  AddAT('ATR1',        'Responses on.');
  AddAT('ATRV',        'Read battery voltage.', [acVoltageMonitor]);
  AddAT('ATI',         'Identify chip (e.g. "ELM327 v1.5").');
  AddAT('AT@1',        'Display device description.');
  AddAT('AT@2',        'Display device identifier.');
  AddAT('ATPP %xx OFF', 'Disable a programmable parameter.');
  AddAT('ATPP %xx ON',  'Enable a programmable parameter.');
  AddAT('ATPP %xx SV %xx', 'Set programmable parameter value.');
  AddAT('ATBRD %xx',   'Switch baud rate (advanced).');
  AddAT('ATPC',        'Protocol close.');
  AddAT('ATBI',        'Bypass init sequence.');

  // ---- ST commands (OBDLink) ---------------------------------------
  AddST('STI',         'Identify device (OBDLink-specific).');
  AddST('STDI',        'Display device hardware info.');
  AddST('STSN',        'Display device serial number.');
  AddST('STMFR',       'Display manufacturer.');
  AddST('STSR',        'Software reset.');
  AddST('STBR%d',      'Set UART baud rate (host side).');
  AddST('STSLU',       'Sleep / low-power mode.');
  AddST('STP %d',      'Set protocol (extended ST set).');
  AddST('STPRS',       'Display current ST protocol.');
  AddST('STM',         'Monitor for messages until stopped.');
  AddST('STFAC',       'Filter add (CAN ID).', [acMessageFilters]);
  AddST('STFAP %xxxx,%xxxx', 'Filter add pass (id, mask).',
    [acMessageFilters]);
  AddST('STFCB',       'Filter clear.', [acMessageFilters]);
  AddST('STCSEGT %d',  'Set segment time (ISO-TP timing).',
    [acAdapterMultiFrame]);
  AddST('STCFCP %xx,%xx,%xx', 'Set flow-control parameters.',
    [acAdapterMultiFrame]);
end;

initialization

finalization
  TOBDAdapterCommandCatalog.ReleaseDefault;

end.
