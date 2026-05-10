//------------------------------------------------------------------------------
//  OBD.Service.VWRadioSAFE
//
//  TOBDVWRadioSAFE - non-visual component that recovers the SAFE
//  unlock code from VW group factory radios (Gamma 5 / Premium IV
//  / Premium V / Rhapsody) by reading the radio's internal
//  EEPROM over the diagnostic bus using KWP1281 "Read EEPROM"
//  blocks.
//
//  Why a separate component (and not part of the radio-code calc
//  family or the EEPROM-dump extractors):
//
//    - The calc family takes a serial+VIN and runs an algorithm.
//      VW radios are licensed and the algorithm isn't bundled.
//    - The EEPROM-dump extractors take a .bin from a chip
//      programmer and read a fixed offset. VW radios *can* be
//      dumped that way, but the documented community path is
//      cleaner: KWP1281 has a "Read EEPROM" block, the radio
//      answers it over the K-line, the host never has to open
//      the unit.
//
//  This component owns:
//
//    - the variant -> EEPROM-offset mapping (from the
//      mnaberez/vwradio reference notes)
//    - the byte-level decode rule (BCD vs ASCII, byte order, ...)
//    - input validation of the offset map for the chosen variant
//
//  The host owns:
//
//    - KWP1281 transport (5-baud init, block framing, per-byte
//      complement-ACK). Wire it to <c>OnReadEEPROM</c> - the
//      component asks for AData of length ALength bytes at the
//      EEPROM address it computed, and the host's KWP1281 codec
//      returns the bytes (or an error).
//
//  Reference :
//    https://github.com/mnaberez/vwradio
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Per-variant
//                     offsets sourced from mnaberez/vwradio
//                     community notes; hosts with a different
//                     firmware revision override OnReadEEPROM and
//                     decode themselves.
//------------------------------------------------------------------------------

unit OBD.Service.VWRadioSAFE;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Protocol.KWP1281;

type
  /// <summary>VW group radio variants whose SAFE-EEPROM map is
  /// documented. Pick the closest match for the firmware on the
  /// radio in front of you; if none fit, set
  /// <c>svCustom</c> and decode in <c>OnDecode</c>.</summary>
  TVWRadioSAFEVariant = (
    svGamma5,
    svPremiumIV,
    svPremiumV,
    svRhapsody,
    svCustom
  );

  /// <summary>One SAFE-extraction result. Same shape as the
  /// EEPROM-dump extractor result so a host UI can show either
  /// without special-casing.</summary>
  TVWRadioSAFEResult = record
    Success:     Boolean;
    Code:        string;
    Message:     string;
    VariantUsed: string;
    /// <summary>Hex pretty-print of the bytes the radio returned
    /// for the SAFE block.</summary>
    RawHex:      string;
  end;

  /// <summary>Host-supplied KWP1281 "Read EEPROM" callback. The
  /// host runs the block exchange (5-baud init must already have
  /// completed), reads <c>ALength</c> bytes starting at
  /// <c>AAddress</c>, and returns them in <c>AData</c>. On
  /// transport failure set <c>ASuccess := False</c> and put a
  /// short reason in <c>AError</c>.</summary>
  TVWRadioSAFEReadEEPROMEvent = procedure(Sender: TObject;
    AAddress: Word; ALength: Byte;
    out AData: TBytes; out ASuccess: Boolean;
    out AError: string) of object;

  /// <summary>Optional host-supplied decoder. Fired when
  /// <c>RadioVariant</c> = <c>svCustom</c> or when the host
  /// wants to override the bundled decode rule. Called with the
  /// raw bytes the radio returned; populate <c>AResult.Code</c>
  /// and <c>AResult.Success</c>.</summary>
  TVWRadioSAFEDecodeEvent = procedure(Sender: TObject;
    const ARawBytes: TBytes;
    var AResult: TVWRadioSAFEResult) of object;

  /// <summary>Component that recovers the VW SAFE unlock code
  /// over KWP1281 by reading a documented EEPROM offset.
  /// <para>Three transport paths in precedence order:</para>
  /// <list type="number">
  /// <item><see cref="Codec"/>     - host-managed
  /// <c>TKWP1281Codec</c>; SAFE just calls
  /// <c>ReadEEPROM</c>. Best when the host wants to reuse one
  /// codec session for SAFE + DTC reads + adaptation, etc.</item>
  /// <item><see cref="Transport"/> - raw <c>IKWP1281Transport</c>;
  /// SAFE creates an internal codec, runs Connect / ReadEEPROM
  /// / Disconnect inside Extract.</item>
  /// <item><see cref="OnReadEEPROM"/> - host-supplied
  /// callback; SAFE delegates the byte read entirely.</item>
  /// </list></summary>
  TOBDVWRadioSAFE = class(TComponent)
  strict private
    FVariant:       TVWRadioSAFEVariant;
    FOnReadEEPROM:  TVWRadioSAFEReadEEPROMEvent;
    FOnDecode:      TVWRadioSAFEDecodeEvent;
    FLastResult:    TVWRadioSAFEResult;
    FCustomAddress: Word;
    FCustomLength:  Byte;
    FCustomTitle:   Byte;
    FRadioAddress:  Byte;
    FCodec:         TKWP1281Codec;
    FTransport:     IKWP1281Transport;
    function VariantOffset: Word;
    function VariantLength: Byte;
    function VariantTitle:  Byte;
    function VariantName:   string;
    function DecodeBundled(const ARaw: TBytes): TVWRadioSAFEResult;
    function ReadViaCodec(ACodec: TKWP1281Codec;
      AAddress: Word; ALength: Byte;
      out AData: TBytes): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Runs the documented SAFE-extraction sequence
    /// against the chosen variant: looks up the EEPROM offset,
    /// asks <see cref="OnReadEEPROM"/> for the bytes, and
    /// decodes them per <see cref="OnDecode"/> if wired or the
    /// bundled rule otherwise.</summary>
    function Extract: TVWRadioSAFEResult;

    /// <summary>Last extraction result.</summary>
    property LastResult: TVWRadioSAFEResult read FLastResult;

    /// <summary>Path 1 - host-managed KWP1281 codec. When non-
    /// nil, <see cref="Extract"/> calls <c>ReadEEPROM</c> on
    /// it directly and does NOT touch Connect / Disconnect.
    /// Highest precedence.</summary>
    property Codec: TKWP1281Codec read FCodec write FCodec;

    /// <summary>Path 2 - raw KWP1281 transport. When non-nil
    /// (and Codec is nil), Extract creates a temporary codec,
    /// connects to the radio at <see cref="RadioAddress"/>,
    /// reads the SAFE block, and disconnects. Middle
    /// precedence.</summary>
    property Transport: IKWP1281Transport
      read FTransport write FTransport;
  published
    /// <summary>Which VW radio variant the SAFE-EEPROM map
    /// should be picked for. Default: <c>svPremiumIV</c>.</summary>
    property RadioVariant: TVWRadioSAFEVariant
      read FVariant write FVariant default svPremiumIV;

    /// <summary>EEPROM address used when
    /// <see cref="RadioVariant"/> = <c>svCustom</c>. Ignored
    /// otherwise.</summary>
    property CustomAddress: Word
      read FCustomAddress write FCustomAddress default 0;

    /// <summary>EEPROM length used when
    /// <see cref="RadioVariant"/> = <c>svCustom</c>. Ignored
    /// otherwise.</summary>
    property CustomLength: Byte
      read FCustomLength write FCustomLength default 0;

    /// <summary>KWP1281 block title used to read EEPROM when
    /// <see cref="RadioVariant"/> = <c>svCustom</c>. Defaults
    /// to <c>0x03</c> (READ_ROM_OR_EEPROM); some radios use
    /// <c>0x19</c>.</summary>
    property CustomTitle: Byte
      read FCustomTitle write FCustomTitle default $03;

    /// <summary>KWP1281 diagnostic address of the radio for
    /// the 5-baud init when SAFE owns the codec lifecycle (see
    /// <see cref="Transport"/>). Default <c>0x56</c>.</summary>
    property RadioAddress: Byte
      read FRadioAddress write FRadioAddress default $56;

    /// <summary>Required - host-supplied KWP1281 "Read EEPROM"
    /// transport.</summary>
    property OnReadEEPROM: TVWRadioSAFEReadEEPROMEvent
      read FOnReadEEPROM write FOnReadEEPROM;

    /// <summary>Optional - host-supplied byte-decode override
    /// (mandatory for <c>svCustom</c>).</summary>
    property OnDecode: TVWRadioSAFEDecodeEvent
      read FOnDecode write FOnDecode;
  end;

implementation

{ TOBDVWRadioSAFE -------------------------------------------------------------}

constructor TOBDVWRadioSAFE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVariant      := svPremiumIV;
  FRadioAddress := $56;
  FCustomTitle  := $03;
end;

function TOBDVWRadioSAFE.VariantOffset: Word;
begin
  // Offsets sourced from mnaberez/vwradio community notes. Vary
  // per firmware revision; if the bundled offset misses the
  // SAFE block on the radio in front of you, switch to
  // svCustom + CustomAddress/CustomLength and decode via
  // OnDecode.
  case FVariant of
    svGamma5:    Result := $0014;
    svPremiumIV: Result := $0083;
    svPremiumV:  Result := $0083;
    svRhapsody:  Result := $0083;
    svCustom:    Result := FCustomAddress;
  else           Result := 0;
  end;
end;

function TOBDVWRadioSAFE.VariantLength: Byte;
begin
  case FVariant of
    svGamma5:    Result := 2;          // 4 BCD digits packed in 2 bytes
    svPremiumIV: Result := 4;          // 4 ASCII digits
    svPremiumV:  Result := 4;
    svRhapsody:  Result := 4;
    svCustom:    Result := FCustomLength;
  else           Result := 0;
  end;
end;

function TOBDVWRadioSAFE.VariantTitle: Byte;
begin
  // Most VAG radios accept the standard READ_ROM_OR_EEPROM
  // title ($03). A handful of late Premium IV firmwares use
  // the radio-specific READ_EEPROM_RADIO ($19) instead - we
  // pick the safer default and let the host override.
  case FVariant of
    svPremiumIV, svPremiumV, svRhapsody:
      Result := KWP1281_TITLE_READ_EEPROM_RADIO;
    svCustom:
      if FCustomTitle <> 0 then Result := FCustomTitle
      else Result := KWP1281_TITLE_READ_ROM_OR_EEPROM;
  else
    Result := KWP1281_TITLE_READ_ROM_OR_EEPROM;
  end;
end;

function TOBDVWRadioSAFE.ReadViaCodec(ACodec: TKWP1281Codec;
  AAddress: Word; ALength: Byte;
  out AData: TBytes): Boolean;
begin
  AData := nil;
  try
    AData := ACodec.ReadEEPROM(AAddress, ALength, VariantTitle);
    Result := Length(AData) >= ALength;
  except
    on E: EKWP1281Error do
      Exit(False);
  end;
end;

function TOBDVWRadioSAFE.VariantName: string;
begin
  case FVariant of
    svGamma5:    Result := 'Gamma 5';
    svPremiumIV: Result := 'Premium IV';
    svPremiumV:  Result := 'Premium V';
    svRhapsody:  Result := 'Rhapsody';
    svCustom:    Result := 'custom';
  else           Result := '?';
  end;
end;

function TOBDVWRadioSAFE.DecodeBundled(
  const ARaw: TBytes): TVWRadioSAFEResult;
var
  I: Integer;
  Hi, Lo: Byte;
begin
  Result := Default(TVWRadioSAFEResult);
  Result.VariantUsed := VariantName;

  case FVariant of
    svGamma5:
      begin
        // 2 BCD bytes -> 4 digits, big-endian nibble order.
        if Length(ARaw) < 2 then
        begin
          Result.Message := 'Gamma 5 SAFE block needs 2 BCD bytes';
          Exit;
        end;
        for I := 0 to 1 do
        begin
          Hi := (ARaw[I] shr 4) and $0F;
          Lo :=  ARaw[I]        and $0F;
          if (Hi > 9) or (Lo > 9) then
          begin
            Result.Message :=
              'Gamma 5 SAFE block bytes are not valid BCD - ' +
              'wire OnDecode for this firmware revision';
            Exit;
          end;
          Result.Code := Result.Code + Format('%d%d', [Hi, Lo]);
        end;
        Result.Success := True;
      end;

    svPremiumIV, svPremiumV, svRhapsody:
      begin
        // 4 ASCII digits.
        if Length(ARaw) < 4 then
        begin
          Result.Message := VariantName + ' SAFE block needs 4 bytes';
          Exit;
        end;
        for I := 0 to 3 do
        begin
          if (ARaw[I] < Ord('0')) or (ARaw[I] > Ord('9')) then
          begin
            Result.Message :=
              VariantName + ' SAFE block bytes are not 4 ASCII ' +
              'digits - wire OnDecode for this firmware revision';
            Exit;
          end;
          Result.Code := Result.Code + Char(ARaw[I]);
        end;
        Result.Success := True;
      end;

    svCustom:
      begin
        Result.Message :=
          'svCustom requires an OnDecode handler - none wired';
      end;
  end;
end;

function TOBDVWRadioSAFE.Extract: TVWRadioSAFEResult;
var
  Address:    Word;
  Length_:    Byte;
  Raw:        TBytes;
  Ok:         Boolean;
  Err:        string;
  HexParts:   string;
  I:          Integer;
  TempCodec:  TKWP1281Codec;
  HavePath:   Boolean;
  PathLabel:  string;
begin
  FLastResult := Default(TVWRadioSAFEResult);
  FLastResult.VariantUsed := VariantName;

  HavePath := Assigned(FCodec) or (FTransport <> nil) or
              Assigned(FOnReadEEPROM);
  if not HavePath then
  begin
    FLastResult.Message :=
      'TOBDVWRadioSAFE.Extract: no transport wired. Set ' +
      'Codec, Transport, or OnReadEEPROM (precedence in that ' +
      'order).';
    Exit(FLastResult);
  end;

  Address := VariantOffset;
  Length_ := VariantLength;
  if (Length_ = 0) and (FVariant <> svCustom) then
  begin
    FLastResult.Message :=
      'TOBDVWRadioSAFE.Extract: variant has no SAFE map; ' +
      'pick a different RadioVariant or use svCustom.';
    Exit(FLastResult);
  end;
  if (FVariant = svCustom) and ((Length_ = 0) or
     not Assigned(FOnDecode)) then
  begin
    FLastResult.Message :=
      'TOBDVWRadioSAFE.Extract: svCustom needs CustomLength > 0 ' +
      'and an OnDecode handler.';
    Exit(FLastResult);
  end;

  Raw := nil;
  Ok  := False;
  Err := '';

  // ---- Path 1: host-managed Codec ---------------------------
  if Assigned(FCodec) then
  begin
    PathLabel := 'Codec';
    Ok := ReadViaCodec(FCodec, Address, Length_, Raw);
    if not Ok then
      Err := 'KWP1281 ReadEEPROM via host-supplied Codec ' +
             'returned no data';
  end
  // ---- Path 2: SAFE-owned Transport -------------------------
  else if FTransport <> nil then
  begin
    PathLabel := 'Transport';
    TempCodec := TKWP1281Codec.Create(FTransport);
    try
      try
        TempCodec.Connect(FRadioAddress);
        Ok := ReadViaCodec(TempCodec, Address, Length_, Raw);
        if not Ok then
          Err := 'KWP1281 ReadEEPROM via Transport returned ' +
                 'no data';
      except
        on E: EKWP1281Error do
        begin
          Ok := False;
          Err := 'KWP1281 transport failed: ' + E.Message;
        end;
      end;
    finally
      try TempCodec.Disconnect; except end;
      TempCodec.Free;
    end;
  end
  // ---- Path 3: OnReadEEPROM ---------------------------------
  else
  begin
    PathLabel := 'OnReadEEPROM';
    FOnReadEEPROM(Self, Address, Length_, Raw, Ok, Err);
  end;

  if not Ok then
  begin
    if Err = '' then Err := 'no detail';
    FLastResult.Message :=
      Format('SAFE extract via %s failed: %s', [PathLabel, Err]);
    Exit(FLastResult);
  end;
  if System.Length(Raw) < Length_ then
  begin
    FLastResult.Message :=
      Format('SAFE extract via %s returned %d bytes, expected %d',
        [PathLabel, System.Length(Raw), Length_]);
    Exit(FLastResult);
  end;

  HexParts := '';
  for I := 0 to System.Length(Raw) - 1 do
  begin
    if I > 0 then HexParts := HexParts + ' ';
    HexParts := HexParts + Format('%.2x', [Raw[I]]);
  end;

  if Assigned(FOnDecode) then
  begin
    FLastResult.VariantUsed := VariantName;
    FOnDecode(Self, Raw, FLastResult);
    if FLastResult.VariantUsed = '' then
      FLastResult.VariantUsed := VariantName;
  end
  else
    FLastResult := DecodeBundled(Raw);

  FLastResult.RawHex := HexParts;
  Result := FLastResult;
end;

end.
