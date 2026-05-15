//------------------------------------------------------------------------------
//  OBD.OEM.Types
//
//  Shared record types for the OEM-extension framework. Defines
//  the catalogue rows every vendor extension publishes:
//    - DID + Routine + ECU descriptors (the baseline catalogue)
//    - Coding blocks + adaptations + actuator tests (extended
//      writeable-coding schema)
//    - Live PID descriptors (Service 0x01 + Service 0x22 PIDs
//      hosts can poll)
//    - DTC extended-data records (UDS Service 0x19 sub 0x06)
//
//  Vendor extensions return these records from
//  <see cref="IOBDOEMExtension"/> in OBD.OEM.Extensions; tooling
//  consumes them to render coding forms, diagnostic dashboards
//  and DTC viewers.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//    2026-05-12  ERD  TOBDDtcCatalogEntry carries the full
//                     v2 DTC schema — severity, possible_causes,
//                     symptoms, repair guidance, monitor type,
//                     freeze-frame relevance, related DIDs /
//                     routines, OEM bulletin, source, verified.
//                     Added TOBDDtcSeverity / TOBDDtcSystem /
//                     TOBDDtcMonitorType enums.
//------------------------------------------------------------------------------

unit OBD.OEM.Types;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>Decoder kinds shared between live-PID and DTC
  /// extended-data records.</summary>
  TOBDOEMDecoderKind = (
    dkUnknown, dkAscii, dkHex, dkUInt8, dkUInt16BE, dkUInt32BE,
    dkInt16BE, dkInt32BE, dkBcdDate, dkEnum, dkBitmask, dkSeconds);

  /// <summary>Field-type tag inside a writeable coding block.</summary>
  TOBDCodingFieldKind = (
    cfkUnknown, cfkBit, cfkUInt8, cfkUInt16BE, cfkUInt32BE,
    cfkInt16BE, cfkInt32BE, cfkAscii, cfkEnum, cfkBitmask);

  /// <summary>Adaptation-channel value kind (VAG-style).</summary>
  TOBDAdaptationKind = (
    adkUnknown, adkUInt8, adkUInt16BE, adkUInt32BE,
    adkInt16BE, adkInt32BE, adkEnum);

  /// <summary>Expected response shape of an actuator test.</summary>
  TOBDActuatorResponseKind = (
    arkNone, arkBoolean, arkUInt8, arkUInt16BE, arkAscii);

  /// <summary>Live-PID service family.</summary>
  TOBDLivePIDMode = (lpmUnknown, lpmService01, lpmService22);

  /// <summary>DTC extended-data record kind.</summary>
  TOBDDtcExtendedDataKind = (
    xdkUnknown, xdkOccurrenceCounter, xdkAgingCounter,
    xdkMilesSinceCleared, xdkFreezeFrameTemplate,
    xdkOemStatusByte, xdkEnvironmentalData);

  /// <summary>Decoder spec carried by every catalogue entry
  /// that ships a <c>decoder</c> sub-object — DIDs, live PIDs,
  /// DTC extended-data records. Mirrors the catalogue schema
  /// 1:1; absent fields stay at their record defaults.</summary>
  TOBDOEMDecoderSpec = record
    /// <summary>Decoder kind, or <c>dkUnknown</c> when the
    /// entry has no decoder.</summary>
    Kind: TOBDOEMDecoderKind;
    /// <summary>Fixed payload size in bytes for enum / bitmask
    /// / fixed-length kinds; 0 when the schema omits it.</summary>
    Size: Integer;
    /// <summary>Multiplicative scale (default 1.0 when the
    /// catalogue omits the field).</summary>
    Scale: Double;
    /// <summary>Additive offset (default 0.0).</summary>
    Offset: Double;
    /// <summary>Display unit (e.g. <c>°C</c>, <c>%</c>,
    /// <c>kPa</c>).</summary>
    Unit_: string;
    /// <summary>Value → label pairs for <c>dkEnum</c>.</summary>
    EnumValues: TArray<TPair<Integer, string>>;
    /// <summary>Bit-index → label pairs for
    /// <c>dkBitmask</c>.</summary>
    BitNames: TArray<TPair<Integer, string>>;
  end;

  /// <summary>One Data Identifier (DID) in a vendor catalogue.</summary>
  TOBDOEMDataIdentifier = record
    /// <summary>16-bit DID value.</summary>
    DID: Word;
    /// <summary>Short snake-case key (e.g. <c>battery_voltage</c>).</summary>
    Name: string;
    /// <summary>Longer human-readable description.</summary>
    Description: string;
    /// <summary>UDS request CAN-ID that owns this DID; 0 = global.</summary>
    EcuAddress: Word;
    /// <summary>Provenance ("iso-15031-6", "oem-spec",
    /// "community-pr", …).</summary>
    Source: string;
    /// <summary>True only when matched against an authoritative
    /// spec or capture fixture.</summary>
    Verified: Boolean;
    /// <summary>Decoder spec for the payload bytes returned by
    /// reading this DID. <c>Decoder.Kind = dkUnknown</c> when
    /// the catalogue declares no decoder.</summary>
    Decoder: TOBDOEMDecoderSpec;
  end;

  /// <summary>One RoutineControl (SID 0x31) entry.</summary>
  TOBDOEMRoutine = record
    /// <summary>16-bit routine identifier.</summary>
    Identifier: Word;
    /// <summary>Short key.</summary>
    Name: string;
    /// <summary>Human-readable description.</summary>
    Description: string;
    /// <summary>Owning ECU address; 0 = global.</summary>
    EcuAddress: Word;
    /// <summary>Provenance.</summary>
    Source: string;
    /// <summary>Spec-confirmed.</summary>
    Verified: Boolean;
  end;

  /// <summary>One ECU on the vehicle bus.</summary>
  TOBDOEMECU = record
    /// <summary>UDS request CAN-ID (e.g. <c>0x7E0</c>).</summary>
    Address: Word;
    /// <summary>Short key (<c>engine</c>, <c>transmission</c>, …).</summary>
    Name: string;
    /// <summary>Display label.</summary>
    CommonName: string;
  end;

  /// <summary>One field inside a writeable coding block.</summary>
  TOBDCodingField = record
    /// <summary>Short key.</summary>
    Name: string;
    /// <summary>Human-readable label.</summary>
    Label_: string;
    /// <summary>Description.</summary>
    Description: string;
    /// <summary>Value kind.</summary>
    Kind: TOBDCodingFieldKind;
    /// <summary>Byte offset within the block payload.</summary>
    ByteOffset: Integer;
    /// <summary>Bit offset (0..7) for sub-byte fields.</summary>
    BitOffset: Integer;
    /// <summary>Bit width; defaults from <c>Kind</c> when 0.</summary>
    BitWidth: Integer;
    /// <summary>Factory default for numeric kinds.</summary>
    DefaultValue: Int64;
    /// <summary>Factory default for ASCII kinds.</summary>
    DefaultAscii: string;
    /// <summary>Minimum allowed value.</summary>
    MinValue: Int64;
    /// <summary>Maximum allowed value.</summary>
    MaxValue: Int64;
    /// <summary>Enum value/label pairs for <c>cfkEnum</c>.</summary>
    EnumValues: TArray<TPair<Integer, string>>;
  end;

  /// <summary>One writeable DID block.</summary>
  TOBDOEMCodingBlock = record
    /// <summary>Target DID.</summary>
    DataIdentifier: Word;
    /// <summary>Short key.</summary>
    Name: string;
    /// <summary>Description.</summary>
    Description: string;
    /// <summary>Owning ECU address.</summary>
    EcuAddress: Word;
    /// <summary>Expected payload length in bytes.</summary>
    PayloadSize: Integer;
    /// <summary>Field layout.</summary>
    Fields: TArray<TOBDCodingField>;
    /// <summary>Provenance.</summary>
    Source: string;
    /// <summary>Spec-confirmed.</summary>
    Verified: Boolean;
  end;

  /// <summary>One numbered adaptation channel.</summary>
  TOBDOEMAdaptation = record
    /// <summary>Channel number.</summary>
    Channel: Word;
    /// <summary>Short key.</summary>
    Name: string;
    /// <summary>Description.</summary>
    Description: string;
    /// <summary>Owning ECU address.</summary>
    EcuAddress: Word;
    /// <summary>Value kind.</summary>
    Kind: TOBDAdaptationKind;
    /// <summary>Minimum allowed value.</summary>
    MinValue: Int64;
    /// <summary>Maximum allowed value.</summary>
    MaxValue: Int64;
    /// <summary>Factory default.</summary>
    DefaultValue: Int64;
    /// <summary>Display unit (e.g. <c>°C</c>, <c>%</c>).</summary>
    Unit_: string;
    /// <summary>Enum value/label pairs when <c>Kind = adkEnum</c>.</summary>
    EnumValues: TArray<TPair<Integer, string>>;
    /// <summary>Provenance.</summary>
    Source: string;
    /// <summary>Spec-confirmed.</summary>
    Verified: Boolean;
  end;

  /// <summary>One forced-output actuator test.</summary>
  TOBDOEMActuatorTest = record
    /// <summary>Routine / actuator ID.</summary>
    Identifier: Word;
    /// <summary>Short key.</summary>
    Name: string;
    /// <summary>Description.</summary>
    Description: string;
    /// <summary>Owning ECU address.</summary>
    EcuAddress: Word;
    /// <summary>Test duration in milliseconds.</summary>
    DurationMs: Cardinal;
    /// <summary>Safety warning to surface before firing.</summary>
    SafetyWarning: string;
    /// <summary>Expected response shape.</summary>
    ExpectedResponseKind: TOBDActuatorResponseKind;
    /// <summary>Expected response label.</summary>
    ExpectedResponseLabel: string;
    /// <summary>Provenance.</summary>
    Source: string;
    /// <summary>Spec-confirmed.</summary>
    Verified: Boolean;
  end;

  /// <summary>One streamable live PID.</summary>
  TOBDOEMLivePID = record
    /// <summary>Service family.</summary>
    Mode: TOBDLivePIDMode;
    /// <summary>PID byte (Mode 01) or 16-bit PID (Mode 22).</summary>
    PID: Word;
    /// <summary>Short key.</summary>
    Name: string;
    /// <summary>Description.</summary>
    Description: string;
    /// <summary>Owning ECU address.</summary>
    EcuAddress: Word;
    /// <summary>Byte offset of this signal in the response payload.</summary>
    FrameOffset: Integer;
    /// <summary>Decoder kind (mirrored from
    /// <c>Decoder.Kind</c> for back-compat).</summary>
    DecoderKind: TOBDOEMDecoderKind;
    /// <summary>Multiplicative scale (mirrored from
    /// <c>Decoder.Scale</c>).</summary>
    Scale: Double;
    /// <summary>Additive offset (mirrored from
    /// <c>Decoder.Offset</c>).</summary>
    Offset: Double;
    /// <summary>Display unit (mirrored from
    /// <c>Decoder.Unit_</c>).</summary>
    Unit_: string;
    /// <summary>Provenance.</summary>
    Source: string;
    /// <summary>Spec-confirmed.</summary>
    Verified: Boolean;
    /// <summary>Full decoder spec including enum-value and
    /// bitmask-name maps. The flat fields above mirror
    /// <c>Decoder.Kind</c> / <c>Scale</c> / <c>Offset</c> /
    /// <c>Unit_</c> for callers that only need the scalar
    /// decoder parameters.</summary>
    Decoder: TOBDOEMDecoderSpec;
  end;

  /// <summary>One DTC extended-data record (UDS 0x19 sub 0x06).</summary>
  TOBDDtcExtendedDataRecord = record
    /// <summary>DTC code (e.g. <c>P0420</c>).</summary>
    DtcCode: string;
    /// <summary>Sub-record number passed to 0x19 0x06.</summary>
    RecordNumber: Byte;
    /// <summary>Record kind.</summary>
    Kind: TOBDDtcExtendedDataKind;
    /// <summary>Description.</summary>
    Description: string;
    /// <summary>Provenance.</summary>
    Source: string;
    /// <summary>Spec-confirmed.</summary>
    Verified: Boolean;
    /// <summary>Full decoder spec.</summary>
    Decoder: TOBDOEMDecoderSpec;
    /// <summary>Decoder kind (mirrored from
    /// <c>Decoder.Kind</c>).</summary>
    DecoderKind: TOBDOEMDecoderKind;
    /// <summary>Multiplicative scale.</summary>
    Scale: Double;
    /// <summary>Additive offset.</summary>
    Offset: Double;
    /// <summary>Display unit.</summary>
    Unit_: string;
  end;

  /// <summary>Per-ECU catalogue slice returned by
  /// <c>IOBDOEMExtension.CatalogForECU</c>.</summary>
  TOBDOEMSubCatalog = record
    /// <summary>ECU address (0 = global).</summary>
    EcuAddress: Word;
    /// <summary>DIDs scoped to this ECU plus globals.</summary>
    DIDs: TArray<TOBDOEMDataIdentifier>;
    /// <summary>Routines scoped to this ECU plus globals.</summary>
    Routines: TArray<TOBDOEMRoutine>;
  end;

  /// <summary>Coarse severity hint for UI tinting / triage. The
  /// wire protocol does not carry severity — these are catalogue
  /// metadata.</summary>
  TOBDDtcSeverity = (
    /// <summary>Severity not stated in the catalogue.</summary>
    dtcSeverityUnknown,
    /// <summary>Informational only.</summary>
    dtcSeverityInfo,
    /// <summary>Drivability / emissions impact.</summary>
    dtcSeverityWarning,
    /// <summary>Safety / immediate-attention.</summary>
    dtcSeverityCritical);

  /// <summary>SAE J2012 system letter — the first character of
  /// a DTC code.</summary>
  TOBDDtcSystem = (
    /// <summary>Powertrain (P codes).</summary>
    dtcPowertrain,
    /// <summary>Chassis (C codes).</summary>
    dtcChassis,
    /// <summary>Body (B codes).</summary>
    dtcBody,
    /// <summary>Network / communication (U codes).</summary>
    dtcNetwork);

  /// <summary>OBD-II monitor classification (SAE J1979 §6).
  /// Drives readiness UI.</summary>
  TOBDDtcMonitorType = (
    /// <summary>Not stated.</summary>
    dmtUnknown,
    /// <summary>Misfire / fuel-system / comprehensive monitors that
    /// run continuously while the engine is on.</summary>
    dmtContinuous,
    /// <summary>Catalyst / EVAP / O2 / EGR monitors that run
    /// during specific drive cycles.</summary>
    dmtNonContinuous,
    /// <summary>Comprehensive component monitor.</summary>
    dmtComprehensiveComponent);

  /// <summary>One DTC catalogue entry. Fields beyond
  /// <c>Code</c> + <c>Description</c> are optional in the JSON
  /// schema; absent fields stay at their record defaults.</summary>
  TOBDDtcCatalogEntry = record
    /// <summary>5-character J2012 code (uppercase,
    /// e.g. <c>P0301</c>).</summary>
    Code: string;
    /// <summary>Coarse severity hint.</summary>
    Severity: TOBDDtcSeverity;
    /// <summary>Single-line description
    /// ("Cylinder 1 misfire detected").</summary>
    Description: string;
    /// <summary>Free-form list of plausible causes shown to the
    /// user.</summary>
    PossibleCauses: TArray<string>;
    /// <summary>Free-form repair-hint paragraph (often borrowed
    /// from service manuals).</summary>
    RepairHints: string;
    /// <summary>Free-form cause / repair note (legacy
    /// field).</summary>
    Notes: string;
    /// <summary>Provenance — same vocabulary as the DID
    /// catalogue (<c>iso-15031-6</c>, <c>sae-j2012</c>,
    /// <c>ross-tech-wiki</c>, <c>esys-community</c>,
    /// <c>community-pr</c>, …).</summary>
    Source: string;
    /// <summary>True only when matched against an authoritative
    /// spec or capture fixture.</summary>
    Verified: Boolean;
    /// <summary>Driver-observable symptoms ("rough idle",
    /// "MIL on", "lurching upshift").</summary>
    Symptoms: TArray<string>;
    /// <summary>Stepped repair guidance (numbered steps).
    /// Distinct from the free-form
    /// <c>RepairHints</c> paragraph.</summary>
    RepairGuidance: TArray<string>;
    /// <summary>SAE J1979 monitor category.</summary>
    MonitorType: TOBDDtcMonitorType;
    /// <summary>Whether this code triggers a freeze-frame
    /// snapshot. MIL-on codes typically do; pending codes
    /// typically do not.</summary>
    FreezeFrameRelevant: Boolean;
    /// <summary>DID names from the same OEM catalogue that are
    /// useful during diagnosis (e.g. <c>ecm_misfire</c>,
    /// <c>ecm_lambda_b1</c> for P0301).</summary>
    RelatedDIDs: TArray<string>;
    /// <summary>Routine names that are likely the corrective
    /// action (e.g. <c>ecm_dpf_regen_force</c> for a P244A DPF
    /// fault).</summary>
    RelatedRoutines: TArray<string>;
    /// <summary>OEM service-bulletin reference (TSB number /
    /// recall ID / dealer fix code). Free text.</summary>
    OemBulletin: string;
  end;

implementation

end.
