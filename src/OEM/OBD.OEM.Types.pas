//------------------------------------------------------------------------------
//  OBD.OEM.Types
//
//  Shared record types for the OEM-extension framework. Defines
//  the catalogue rows every vendor extension publishes:
//    - DID + Routine + ECU descriptors (the v1 baseline)
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
//    2026-05-11  ERD  Initial port from v1 OBD.OEM record schema.
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
    /// <summary>Decoder kind.</summary>
    DecoderKind: TOBDOEMDecoderKind;
    /// <summary>Multiplicative scale.</summary>
    Scale: Double;
    /// <summary>Additive offset.</summary>
    Offset: Double;
    /// <summary>Display unit.</summary>
    Unit_: string;
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
    /// <summary>Decoder kind.</summary>
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

  /// <summary>One DTC catalogue entry.</summary>
  TOBDDtcCatalogEntry = record
    /// <summary>5-character J2012 code.</summary>
    Code: string;
    /// <summary>Human-readable description.</summary>
    Description: string;
    /// <summary>Cause / repair-hint note.</summary>
    Notes: string;
  end;

implementation

end.
