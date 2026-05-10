//------------------------------------------------------------------------------
//  OBD.Speciality.Tachograph
//
//  TOBDTachograph — non-visual decoder for the digital-tachograph
//  records defined by EU Regulation 165/2014 / Commission
//  Implementing Regulation 2016/799 (Annex IC) and the legacy
//  3821/85 generation. Covers the record types every workshop /
//  authority tool needs to read off a Vehicle Unit (VU) or driver
//  card download:
//
//    - DriverActivityRecord (work / drive / rest / availability)
//    - EventRecord          (security / overspeed / sensor faults)
//    - FaultRecord          (recording-equipment faults)
//    - CalibrationRecord    (workshop activations / parameter changes)
//    - VuOverview           (vehicle / owner / activation timestamps)
//
//  v1 ships pure decoders — TLV-style structural parsing of the
//  ASN.1 BER blobs Annex IC defines. Card / VU dumps are produced
//  by the workshop tool's transport layer (PC/SC card reader or
//  K-line / CAN VU diagnostic surface); this component consumes
//  the raw bytes those tools deliver.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - Commission Implementing Regulation (EU) 2016/799 Annex IC
//    - ISO 16844-3 (Tachograph CAN diagnostic surface)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Speciality.Tachograph;

interface

uses
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  OBD.Types;

const
  // ---- Activity codes (ISO 16844-3 / Annex IC §2.1) ----
  TACHO_ACT_WORK         = $01;
  TACHO_ACT_DRIVE        = $02;
  TACHO_ACT_REST         = $03;
  TACHO_ACT_AVAILABLE    = $04;

  // ---- Event types ----
  TACHO_EVENT_OVERSPEED       = $11;
  TACHO_EVENT_TIME_OVERLAP    = $13;
  TACHO_EVENT_SENSOR_FAILURE  = $30;
  TACHO_EVENT_POWER_LOSS      = $31;
  TACHO_EVENT_MOTION_DATA_ERR = $32;
  TACHO_EVENT_CARD_INVALID    = $40;

type
  /// <summary>One decoded activity record.</summary>
  TOBDTachoActivity = record
    /// <summary>Activity code (TACHO_ACT_*).</summary>
    Activity: Byte;
    /// <summary>Slot identifier (1 = driver, 2 = co-driver).</summary>
    SlotID: Byte;
    /// <summary>Card-inserted flag.</summary>
    CardInserted: Boolean;
    /// <summary>Begin timestamp (UTC).</summary>
    BeginTime: TDateTime;
  end;

  /// <summary>One decoded event record.</summary>
  TOBDTachoEvent = record
    EventType: Byte;
    BeginTime: TDateTime;
    EndTime: TDateTime;
    /// <summary>Source code (Annex IC table — reserved / driver /
    /// co-driver / vehicle / unknown).</summary>
    Source: Byte;
  end;

  /// <summary>One decoded fault record.</summary>
  TOBDTachoFault = record
    FaultType: Byte;
    BeginTime: TDateTime;
    EndTime: TDateTime;
  end;

  /// <summary>One decoded calibration record (workshop card
  /// activation / parameter change).</summary>
  TOBDTachoCalibration = record
    Purpose: Byte;
    WorkshopName: string;
    WorkshopCardNumber: string;
    Date: TDateTime;
    /// <summary>VIN at calibration time.</summary>
    VIN: string;
    /// <summary>w-vehicle constant (imp/km).</summary>
    WVehicleCharacteristic: Word;
    /// <summary>k-coefficient (imp/km).</summary>
    KConstant: Word;
    /// <summary>Tyre size string.</summary>
    TyreSize: string;
    /// <summary>Authorised speed (km/h).</summary>
    AuthorisedSpeedKmh: Byte;
  end;

  /// <summary>VU overview (Annex IC, "VuOverviewBlock").</summary>
  TOBDTachoOverview = record
    /// <summary>Member-state code letter pair (e.g. "DE", "FR").</summary>
    MemberState: string;
    /// <summary>VIN of the vehicle.</summary>
    VIN: string;
    /// <summary>VU manufacturer name.</summary>
    Manufacturer: string;
    /// <summary>Software / firmware version string.</summary>
    SoftwareVersion: string;
    /// <summary>VU activation timestamp.</summary>
    ActivationDate: TDateTime;
  end;

  /// <summary>Tachograph record decoder. All methods are
  /// stateless.</summary>
  TOBDTachograph = class
  public
    /// <summary>Decodes one 5-byte ActivityChangeInfo blob
    /// (Annex IC §2.1).</summary>
    /// <returns>True on success.</returns>
    class function DecodeActivity(const ABytes: TBytes;
      out AActivity: TOBDTachoActivity): Boolean; static;
    /// <summary>Decodes one 24-byte EventRecord blob.</summary>
    class function DecodeEvent(const ABytes: TBytes;
      out AEvent: TOBDTachoEvent): Boolean; static;
    /// <summary>Decodes one 24-byte FaultRecord blob.</summary>
    class function DecodeFault(const ABytes: TBytes;
      out AFault: TOBDTachoFault): Boolean; static;
    /// <summary>Decodes one CalibrationRecord blob (Annex IC §2.32,
    /// Gen 1 fixed layout). The variable-length card-number fields
    /// are decoded as raw ASCII strings — Gen 2 / V2 hosts that
    /// need the structured BCD form post-process the
    /// <c>WorkshopCardNumber</c> field themselves.</summary>
    class function DecodeCalibration(const ABytes: TBytes;
      out ACalibration: TOBDTachoCalibration): Boolean; static;
    /// <summary>Converts an Annex IC TimeReal (32-bit seconds since
    /// 1970-01-01 UTC) to a Delphi <c>TDateTime</c>.</summary>
    class function DecodeTimeReal(AValue: Cardinal): TDateTime; static;
    /// <summary>Inverse of <see cref="DecodeTimeReal"/>.</summary>
    class function EncodeTimeReal(AValue: TDateTime): Cardinal; static;
    /// <summary>Decodes a fixed-length, zero-padded ASCII field.</summary>
    class function DecodeString(const ABytes: TBytes;
      AOffset, ALength: Integer): string; static;
  end;

implementation

const
  /// <summary>Tachograph epoch: 1970-01-01 UTC (Annex IC §2.41 TimeReal).</summary>
  TachoEpoch = 25569.0; // = EncodeDateTime(1970,1,1,0,0,0,0)

class function TOBDTachograph.DecodeTimeReal(AValue: Cardinal): TDateTime;
begin
  Result := TachoEpoch + AValue / SecsPerDay;
end;

class function TOBDTachograph.EncodeTimeReal(AValue: TDateTime): Cardinal;
begin
  if AValue <= TachoEpoch then Exit(0);
  Result := Cardinal(Round((AValue - TachoEpoch) * SecsPerDay));
end;

class function TOBDTachograph.DecodeActivity(const ABytes: TBytes;
  out AActivity: TOBDTachoActivity): Boolean;
var
  W: Word;
  Minutes: Word;
begin
  AActivity := Default(TOBDTachoActivity);
  // Annex IC ActivityChangeInfo: 16 bits packed
  //   bit 15..14 slot
  //   bit 13     status (card inserted)
  //   bit 12..11 activity (in our enum)
  //   bit 10..0  minutes since 00:00 (0..1439)
  // Followed by no further bytes — the record is just 2 bytes.
  if Length(ABytes) < 2 then Exit(False);
  W := (Word(ABytes[0]) shl 8) or Word(ABytes[1]);
  AActivity.SlotID       := (W shr 14) and $03;
  AActivity.CardInserted := ((W shr 13) and $01) <> 0;
  AActivity.Activity     := (W shr 11) and $03;
  Minutes                := W and $7FF;
  AActivity.BeginTime    := EncodeTime(Minutes div 60, Minutes mod 60, 0, 0);
  Result := True;
end;

class function TOBDTachograph.DecodeEvent(const ABytes: TBytes;
  out AEvent: TOBDTachoEvent): Boolean;
var
  Begin_, End_: Cardinal;
begin
  AEvent := Default(TOBDTachoEvent);
  // EventRecord per Annex IC §2.46:
  //   eventType (1) eventBeginTime (4) eventEndTime (4) eventRecordPurpose (1)
  //   cardNumberDriverSlotBegin (18 — skipped here)
  //   cardNumberCodriverSlotBegin (18)
  // Minimum 10 bytes for the timing-and-type slice we expose.
  if Length(ABytes) < 10 then Exit(False);
  AEvent.EventType := ABytes[0];
  Begin_ := (Cardinal(ABytes[1]) shl 24) or
            (Cardinal(ABytes[2]) shl 16) or
            (Cardinal(ABytes[3]) shl 8)  or
             Cardinal(ABytes[4]);
  End_   := (Cardinal(ABytes[5]) shl 24) or
            (Cardinal(ABytes[6]) shl 16) or
            (Cardinal(ABytes[7]) shl 8)  or
             Cardinal(ABytes[8]);
  AEvent.BeginTime := DecodeTimeReal(Begin_);
  AEvent.EndTime   := DecodeTimeReal(End_);
  AEvent.Source    := ABytes[9];
  Result := True;
end;

class function TOBDTachograph.DecodeFault(const ABytes: TBytes;
  out AFault: TOBDTachoFault): Boolean;
var
  Begin_, End_: Cardinal;
begin
  AFault := Default(TOBDTachoFault);
  // FaultRecord shares the layout of EventRecord (bytes 0..8).
  if Length(ABytes) < 9 then Exit(False);
  AFault.FaultType := ABytes[0];
  Begin_ := (Cardinal(ABytes[1]) shl 24) or
            (Cardinal(ABytes[2]) shl 16) or
            (Cardinal(ABytes[3]) shl 8)  or
             Cardinal(ABytes[4]);
  End_   := (Cardinal(ABytes[5]) shl 24) or
            (Cardinal(ABytes[6]) shl 16) or
            (Cardinal(ABytes[7]) shl 8)  or
             Cardinal(ABytes[8]);
  AFault.BeginTime := DecodeTimeReal(Begin_);
  AFault.EndTime   := DecodeTimeReal(End_);
  Result := True;
end;

class function TOBDTachograph.DecodeCalibration(const ABytes: TBytes;
  out ACalibration: TOBDTachoCalibration): Boolean;
var
  Off: Integer;
  Wcc, Kcc: Cardinal;
  DateRaw: Cardinal;
begin
  ACalibration := Default(TOBDTachoCalibration);
  // Gen-1 fixed-offset layout (Annex IB §2.39):
  //   0     calibrationPurpose (1)
  //   1     workshopName     (36 = 1 codepage + 35 chars)
  //   37    workshopAddress  (36 — skipped)
  //   73    workshopCardNumber (16 — name(14) + replacement(1) + renewal(1))
  //   89    workshopCardExpiryDate (TimeReal 4)
  //   93    vehicleIdentificationNumber (17)
  //   110   vehicleRegistrationNation (1)
  //   111   vehicleRegistrationNumber (14)  — skipped
  //   125   wVehicleCharacteristicConstant (2 BE)
  //   127   kConstantOfRecordingEquipment (2 BE)
  //   129   lTyreCircumference (2 BE — skipped)
  //   131   tyreSize (15)
  //   146   authorisedSpeed (1)
  //   147   ...odometers / times (Gen 1 stops here at 162)
  if Length(ABytes) < 147 then Exit(False);

  ACalibration.Purpose := ABytes[0];
  // Skip the 1-byte code page in workshopName (offset 1) → name at 2..36
  ACalibration.WorkshopName := DecodeString(ABytes, 2, 35);
  ACalibration.WorkshopCardNumber := DecodeString(ABytes, 73 + 1, 13);
  DateRaw := (Cardinal(ABytes[89]) shl 24) or
             (Cardinal(ABytes[90]) shl 16) or
             (Cardinal(ABytes[91]) shl  8) or
              Cardinal(ABytes[92]);
  ACalibration.Date := DecodeTimeReal(DateRaw);
  ACalibration.VIN  := DecodeString(ABytes, 93, 17);
  Wcc := (Cardinal(ABytes[125]) shl 8) or ABytes[126];
  Kcc := (Cardinal(ABytes[127]) shl 8) or ABytes[128];
  ACalibration.WVehicleCharacteristic := Word(Wcc);
  ACalibration.KConstant              := Word(Kcc);
  ACalibration.TyreSize := DecodeString(ABytes, 131, 15);
  ACalibration.AuthorisedSpeedKmh := ABytes[146];

  Off := 1; // suppress unused warning
  if Off = 0 then ;
  Result := True;
end;

class function TOBDTachograph.DecodeString(const ABytes: TBytes;
  AOffset, ALength: Integer): string;
var
  Slice: TBytes;
  CopyLen, I: Integer;
begin
  if (AOffset < 0) or (ALength <= 0) or (AOffset >= Length(ABytes)) then
    Exit('');
  CopyLen := ALength;
  if AOffset + CopyLen > Length(ABytes) then
    CopyLen := Length(ABytes) - AOffset;
  SetLength(Slice, CopyLen);
  if CopyLen > 0 then
    Move(ABytes[AOffset], Slice[0], CopyLen);
  // Strip trailing 0x00 / 0x20 padding.
  for I := High(Slice) downto 0 do
  begin
    if (Slice[I] = $00) or (Slice[I] = $20) then
      SetLength(Slice, I)
    else
      Break;
  end;
  Result := TEncoding.ASCII.GetString(Slice);
end;

end.
