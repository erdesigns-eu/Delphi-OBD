//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.Capabilities.pas
// CONTENTS       : Adapter capability registry (CAN, CAN-FD, ISO-TP, DoIP, ...)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Adapter.Capabilities;

interface

uses
  System.SysUtils, System.SyncObjs, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   One capability bit. Stable enum values; never renumber.
  /// </summary>
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
    /// <summary>
    ///   Display name.
    /// </summary>
    DisplayName: string;
    /// <summary>
    ///   Cap set.
    /// </summary>
    CapSet: TOBDAdapterCapabilitySet;
    /// <summary>
    ///   Maximum ISO-TP frame body length in bytes. 7 for CAN
    ///   classic single-frame; 62 for CAN-FD 64-byte single-frame.
    /// </summary>
    MaxIsoTpFrameBytes: Integer;
  end;

/// <summary>
///   Render a capability set as a comma-separated list, useful
///   for log lines and UI display.
/// </summary>
function CapabilitySetToString(const S: TOBDAdapterCapabilitySet): string;

/// <summary>
///   Register or replace an adapter's capabilities. Idempotent
///   on the same key.
/// </summary>
procedure RegisterAdapterCapabilities(const Caps: TOBDAdapterCapabilities);

/// <summary>
///   Look up an adapter's capabilities by key. Returns False if
///   the adapter hasn't registered.
/// </summary>
function FindAdapterCapabilities(const AdapterKey: string;
  out Caps: TOBDAdapterCapabilities): Boolean;

/// <summary>
///   Convenience: True iff the adapter is registered and the
///   capability is set.
/// </summary>
function AdapterSupports(const AdapterKey: string;
  Capability: TOBDAdapterCapability): Boolean;

/// <summary>
///   Pick the best ISO-TP single-frame size for the resolved
///   adapter. Returns 7 for CAN-classic (or unknown), 62 for CAN-FD
///   when acISOTPLargeFrame is set.
/// </summary>
function ResolveIsoTpFrameBytes(const AdapterKey: string): Integer;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.TypInfo, System.Classes, System.JSON,
  OBD.Catalog.Path;

var
  GLock: TCriticalSection;
  GByKey: TDictionary<string, TOBDAdapterCapabilities>;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  CapNames: array[TOBDAdapterCapability] of string = (
    'CAN', 'CAN-FD', 'ISO-TP', 'ISO-TP-LF', 'DoIP', 'J1939', 'K-Line',
    'Voltage', 'SecOC', 'J2534', 'J2534v2', 'BLE', 'WiFi', 'FTDI'
  );

//------------------------------------------------------------------------------
// CAPABILITY SET TO STRING
//------------------------------------------------------------------------------
function CapabilitySetToString(const S: TOBDAdapterCapabilitySet): string;
var
  C: TOBDAdapterCapability;
  Buf: TStringList;
begin
  // Create Buf
  Buf := TStringList.Create;
  try
    Buf.Delimiter := ',';
    Buf.StrictDelimiter := True;
    // Loop over TOBDAdapterCapability
    for C := Low(TOBDAdapterCapability) to High(TOBDAdapterCapability) do
      if C in S then
        Buf.Add(CapNames[C]);
    Result := Buf.DelimitedText;
  finally
    // Free Buf
    Buf.Free;
  end;
end;

//------------------------------------------------------------------------------
// REGISTER ADAPTER CAPABILITIES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// FIND ADAPTER CAPABILITIES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// ADAPTER SUPPORTS
//------------------------------------------------------------------------------
function AdapterSupports(const AdapterKey: string;
  Capability: TOBDAdapterCapability): Boolean;
var
  Caps: TOBDAdapterCapabilities;
begin
  Result := FindAdapterCapabilities(AdapterKey, Caps)
            and (Capability in Caps.CapSet);
end;

//------------------------------------------------------------------------------
// RESOLVE ISO TP FRAME BYTES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// CAPABILITY FROM STRING
//------------------------------------------------------------------------------
function CapabilityFromString(const S: string; out C: TOBDAdapterCapability): Boolean;
var
  I: TOBDAdapterCapability;
begin
  // Loop over TOBDAdapterCapability
  for I := Low(TOBDAdapterCapability) to High(TOBDAdapterCapability) do
    if SameText(S, CapNames[I]) or SameText(S, GetEnumName(TypeInfo(TOBDAdapterCapability), Ord(I))) then
    begin
      C := I; Exit(True);
    end;
  // Initialize result
  Result := False;
end;

//------------------------------------------------------------------------------
// LOAD ADAPTER CATALOG
//------------------------------------------------------------------------------
procedure LoadAdapterCatalog;
var
  Path, Raw: string;
  Doc: TJSONValue;
  Arr, CapArr: TJSONArray;
  Item, CapItem: TJSONValue;
  Obj: TJSONObject;
  R: TOBDAdapterCapabilities;
  Cap: TOBDAdapterCapability;
  Stream: TStringStream;
begin
  // Resolve catalog path
  Path := ResolveCatalogPath('adapter-capabilities.json');
  // Bail if catalog path is missing
  if Path = '' then Exit;
  // Create stream
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    // Load file into stream
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    // Free the stream
    Stream.Free;
  end;
  // Parse JSON document
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    // Bail if array is missing
    if Arr = nil then Exit;
    // Loop over Arr
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      R := Default(TOBDAdapterCapabilities);
      R.AdapterKey  := Obj.GetValue<string>('adapter_key', '');
      if R.AdapterKey = '' then Continue;
      R.DisplayName := Obj.GetValue<string>('display_name', '');
      R.MaxIsoTpFrameBytes := Obj.GetValue<Integer>('max_iso_tp_frame_bytes', 7);
      CapArr := Obj.GetValue<TJSONArray>('capabilities');
      if CapArr <> nil then
        // Loop over CapArr
        for CapItem in CapArr do
          if (CapItem is TJSONString) and CapabilityFromString(CapItem.Value, Cap) then
            Include(R.CapSet, Cap);
      RegisterAdapterCapabilities(R);
    end;
  finally
    // Free the document
    Doc.Free;
  end;
end;

initialization
  GLock := TCriticalSection.Create;
  GByKey := TDictionary<string, TOBDAdapterCapabilities>.Create;
  LoadAdapterCatalog;

finalization
  GByKey.Free;
  GLock.Free;

end.
