//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.Capabilities.pas
// CONTENTS       : Adapter capability set for feature-gating CAN-FD,
//                : ISO-TP, DoIP, J1939, voltage monitoring, secure-onboard
//                : communication, and J2534 pass-through. A read-only
//                : process-wide registry maps adapter-kind keys to their
//                : capability set so callers can ask "does this connected
//                : adapter handle CAN-FD?" without instantiating it.
//
// Why            : Apps that mix ELM327 (CAN only), OBDLink EX (CAN-FD),
//                : and DoIP gateways need a uniform way to detect
//                : capabilities and pick the right transport at runtime.
//                : Until now, capability detection was scattered across
//                : adapter-specific probes; centralising it removes a
//                : recurring source of "works on my bench, fails in the
//                : field" bugs.
//
// Adopting       : Existing adapter units can opt in by calling
//                : RegisterAdapterCapabilities at unit init. Until they
//                : do, callers can probe at runtime via the per-adapter
//                : feature flags this unit defines.
//------------------------------------------------------------------------------
unit OBD.Adapter.Capabilities;

interface

uses
  System.SysUtils, System.SyncObjs, System.Generics.Collections;

type
  /// <summary>One capability bit. Stable enum values; never renumber.</summary>
  TOBDAdapterCapability = (
    acCAN              = 0,
    acCANFD            = 1,   // CAN-FD (ISO 11898-1:2015)
    acISOTP            = 2,   // ISO 15765-2 framing
    acISOTPLargeFrame  = 3,   // CAN-FD-only 64-byte single-frame
    acDoIP             = 4,
    acJ1939            = 5,
    acKLine            = 6,   // ISO 9141-2 / KWP2000
    acVoltageMonitor   = 7,
    acSecureOnboard    = 8,   // SecOC awareness (per-OEM still required)
    acJ2534            = 9,
    acJ2534v2          = 10,  // J2534-2 (2018) extensions
    acBluetoothLE      = 11,
    acWiFi             = 12,
    acFTDI             = 13
  );

  TOBDAdapterCapabilitySet = set of TOBDAdapterCapability;

  TOBDAdapterCapabilities = record
    AdapterKey: string;       // e.g. 'elm327', 'obdlink_ex', 'doip_gateway'
    DisplayName: string;
    CapSet: TOBDAdapterCapabilitySet;
    /// <summary>Maximum ISO-TP frame body length in bytes. 7 for CAN
    /// classic single-frame; 62 for CAN-FD 64-byte single-frame.</summary>
    MaxIsoTpFrameBytes: Integer;
  end;

/// <summary>Render a capability set as a comma-separated list, useful
/// for log lines and UI display.</summary>
function CapabilitySetToString(const S: TOBDAdapterCapabilitySet): string;

/// <summary>Register or replace an adapter's capabilities. Idempotent
/// on the same key.</summary>
procedure RegisterAdapterCapabilities(const Caps: TOBDAdapterCapabilities);

/// <summary>Look up an adapter's capabilities by key. Returns False if
/// the adapter hasn't registered.</summary>
function FindAdapterCapabilities(const AdapterKey: string;
  out Caps: TOBDAdapterCapabilities): Boolean;

/// <summary>Convenience: True iff the adapter is registered and the
/// capability is set.</summary>
function AdapterSupports(const AdapterKey: string;
  Capability: TOBDAdapterCapability): Boolean;

/// <summary>Pick the best ISO-TP single-frame size for the resolved
/// adapter. Returns 7 for CAN-classic (or unknown), 62 for CAN-FD
/// when acISOTPLargeFrame is set.</summary>
function ResolveIsoTpFrameBytes(const AdapterKey: string): Integer;

implementation

var
  GLock: TCriticalSection;
  GByKey: TDictionary<string, TOBDAdapterCapabilities>;

const
  CapNames: array[TOBDAdapterCapability] of string = (
    'CAN', 'CAN-FD', 'ISO-TP', 'ISO-TP-LF', 'DoIP', 'J1939', 'K-Line',
    'Voltage', 'SecOC', 'J2534', 'J2534v2', 'BLE', 'WiFi', 'FTDI'
  );

function CapabilitySetToString(const S: TOBDAdapterCapabilitySet): string;
var
  C: TOBDAdapterCapability;
  Buf: TStringList;
begin
  Buf := TStringList.Create;
  try
    Buf.Delimiter := ',';
    Buf.StrictDelimiter := True;
    for C := Low(TOBDAdapterCapability) to High(TOBDAdapterCapability) do
      if C in S then
        Buf.Add(CapNames[C]);
    Result := Buf.DelimitedText;
  finally
    Buf.Free;
  end;
end;

procedure RegisterAdapterCapabilities(const Caps: TOBDAdapterCapabilities);
var
  Stored: TOBDAdapterCapabilities;
  Key: string;
begin
  if Caps.AdapterKey = '' then
    raise Exception.Create('AdapterKey required');
  Stored := Caps;
  Key := LowerCase(Caps.AdapterKey);
  Stored.AdapterKey := Key;
  GLock.Acquire;
  try
    GByKey.AddOrSetValue(Key, Stored);
  finally
    GLock.Release;
  end;
end;

function FindAdapterCapabilities(const AdapterKey: string;
  out Caps: TOBDAdapterCapabilities): Boolean;
begin
  GLock.Acquire;
  try
    Result := GByKey.TryGetValue(LowerCase(AdapterKey), Caps);
  finally
    GLock.Release;
  end;
end;

function AdapterSupports(const AdapterKey: string;
  Capability: TOBDAdapterCapability): Boolean;
var
  Caps: TOBDAdapterCapabilities;
begin
  Result := FindAdapterCapabilities(AdapterKey, Caps)
            and (Capability in Caps.CapSet);
end;

function ResolveIsoTpFrameBytes(const AdapterKey: string): Integer;
var
  Caps: TOBDAdapterCapabilities;
begin
  if FindAdapterCapabilities(AdapterKey, Caps) then
  begin
    if (acISOTPLargeFrame in Caps.CapSet) and (Caps.MaxIsoTpFrameBytes > 0) then
      Exit(Caps.MaxIsoTpFrameBytes);
    if acISOTPLargeFrame in Caps.CapSet then Exit(62);
  end;
  Result := 7;
end;

procedure SeedDefaultAdapters;

  procedure Reg(const Key, Name: string; const Caps: TOBDAdapterCapabilitySet;
    MaxIsoTp: Integer);
  var R: TOBDAdapterCapabilities;
  begin
    R.AdapterKey := Key;
    R.DisplayName := Name;
    R.CapSet := Caps;
    R.MaxIsoTpFrameBytes := MaxIsoTp;
    RegisterAdapterCapabilities(R);
  end;

begin
  // ELM327 — CAN only, ISO-TP, K-Line, voltage. No CAN-FD.
  Reg('elm327', 'ELM327',
    [acCAN, acISOTP, acKLine, acVoltageMonitor], 7);
  // OBDLink SX/MX — same as ELM327 plus ST commands; still no CAN-FD.
  Reg('obdlink_mx', 'OBDLink MX',
    [acCAN, acISOTP, acKLine, acVoltageMonitor, acBluetoothLE], 7);
  // OBDLink EX — STN2255 supports CAN-FD.
  Reg('obdlink_ex', 'OBDLink EX',
    [acCAN, acCANFD, acISOTP, acISOTPLargeFrame, acKLine,
     acVoltageMonitor, acFTDI], 62);
  // DoIP gateway — Ethernet only, no K-Line / classical CAN.
  Reg('doip_gateway', 'DoIP Gateway',
    [acDoIP, acISOTP, acISOTPLargeFrame, acVoltageMonitor], 4095);
  // J2534 pass-through — CAN classic and FD when the vendor DLL exposes it.
  Reg('j2534', 'J2534 Pass-Through',
    [acCAN, acISOTP, acKLine, acJ1939, acJ2534, acVoltageMonitor], 7);
end;

initialization
  GLock := TCriticalSection.Create;
  GByKey := TDictionary<string, TOBDAdapterCapabilities>.Create;
  SeedDefaultAdapters;

finalization
  GByKey.Free;
  GLock.Free;

end.
