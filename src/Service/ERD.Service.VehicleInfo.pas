// ------------------------------------------------------------------------------
// ERD.Service.VehicleInfo
//
// TOBDVehicleInfo — non-visual component that reads the OBD vehicle
// information items (SAE J1979 Service 09 / SAE J1979-2 DIDs F8xx)
// from every emissions ECU:
//
// - supported InfoTypes (00)
// - VIN (02)
// - calibration IDs (04) and calibration verification numbers (06)
// - in-use performance tracking, spark ignition (08) and
//   compression ignition (0B), with completion / condition ratios
// - ECU name (0A)
// - engine serial number (0D) and type-approval number (0F)
//
// Every ECU answers separately. CAN replies carry a NODI (item
// count) byte; legacy buses (J1850 / ISO 9141 / KWP) split an item
// over numbered 4-byte messages. TOBDVehicleInfoParser assembles
// both shapes into one message per ECU; it is pure so hosts and
// tests can feed recorded responses.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - SAE J1979 Service 09 (ISO 15031-5)
// - SAE J1979-2 (ISO 14229 ReadDataByIdentifier F800-F8FF)
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.Service.VehicleInfo;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.Async.Task,
  ERD.Connection,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFNDEF FPC}Data.Bind.Components, System.Bindings.Helper, {$ENDIF}
  ERD.Types,
  ERD.Protocol.Types,
  ERD.Protocol.UDS,
  ERD.Protocol.VIN,
  ERD.Protocol;

const
  /// <summary>Service 09 request mode.</summary>
  OBD_MODE_INFO = $09;
  /// <summary>Supported InfoTypes 01-20 bitmap.</summary>
  OBD_INFOTYPE_SUPPORTED = $00;
  /// <summary>Vehicle Identification Number.</summary>
  OBD_INFOTYPE_VIN = $02;
  /// <summary>Calibration identifications (16 characters each).</summary>
  OBD_INFOTYPE_CALID = $04;
  /// <summary>Calibration verification numbers (4 bytes each).</summary>
  OBD_INFOTYPE_CVN = $06;
  /// <summary>In-use performance tracking, spark ignition.</summary>
  OBD_INFOTYPE_IPT_SPARK = $08;
  /// <summary>ECU name (acronym + text name, 20 characters).</summary>
  OBD_INFOTYPE_ECU_NAME = $0A;
  /// <summary>In-use performance tracking, compression ignition.</summary>
  OBD_INFOTYPE_IPT_COMPRESSION = $0B;
  /// <summary>Engine serial number.</summary>
  OBD_INFOTYPE_ESN = $0D;
  /// <summary>Exhaust regulation or type-approval number.</summary>
  OBD_INFOTYPE_EROTAN = $0F;
  /// <summary>J1979-2 DID of an InfoType is <c>$F800 + InfoType</c>.</summary>
  OBD_J1979_2_INFO_DID_BASE = $F800;

type
  /// <summary>Which request family reads the items.</summary>
  TOBDVehicleInfoSource = (
    /// <summary>Service 09 first, J1979-2 DIDs when the vehicle does
    /// not answer Service 09.</summary>
    visAuto,
    /// <summary>SAE J1979 Service 09 only.</summary>
    visOBD,
    /// <summary>SAE J1979-2 (UDS 22 F8xx) only.</summary>
    visUDS);

  /// <summary>One InfoType answer of one ECU, assembled.</summary>
  TOBDInfoTypeMessage = record
    /// <summary>ECU header (<c>'7E8'</c>); empty with headers off.</summary>
    Ecu: string;
    /// <summary>Numeric ECU identifier; 0 when unknown.</summary>
    EcuId: Cardinal;
    /// <summary>InfoType this message answers.</summary>
    InfoType: Byte;
    /// <summary>NODI (number of data items) the ECU reported; 0 when
    /// the bus does not send it (legacy multi-message replies).</summary>
    ItemCount: Integer;
    /// <summary>Item bytes (after the InfoType / NODI bytes).</summary>
    Data: TBytes;
    /// <summary>Request family that produced the message.</summary>
    Source: TOBDVehicleInfoSource;
  end;

  /// <summary>A text item reported by one ECU (VIN, ESN, EROTAN).</summary>
  TOBDEcuText = record
    Ecu: string;
    EcuId: Cardinal;
    Text: string;
  end;

  /// <summary>One calibration of one ECU.</summary>
  TOBDCalibration = record
    Ecu: string;
    EcuId: Cardinal;
    /// <summary>Calibration ID (up to 16 characters).</summary>
    CalibrationId: string;
    /// <summary>Calibration verification number as 8 hex digits.</summary>
    CVN: string;
    /// <summary>False when the ECU reported no CVN for this calibration.</summary>
    HasCVN: Boolean;
  end;

  /// <summary>ECU name (InfoType 0A).</summary>
  TOBDEcuNameInfo = record
    Ecu: string;
    EcuId: Cardinal;
    /// <summary>Acronym (<c>'ECM'</c>, <c>'TCM'</c>).</summary>
    Acronym: string;
    /// <summary>Text name (<c>'EngineControl'</c>).</summary>
    Name: string;
  end;

  /// <summary>One raw in-use performance counter.</summary>
  TOBDIptCounter = record
    /// <summary>J1979 mnemonic (<c>'OBDCOND'</c>, <c>'CATCOMP1'</c>).</summary>
    Name: string;
    Value: Word;
  end;

  /// <summary>Completion / condition pair of one monitor.</summary>
  TOBDIptMonitor = record
    /// <summary>Stable key for translations (<c>'catalyst_b1'</c>).</summary>
    Key: string;
    /// <summary>English display name.</summary>
    Name: string;
    /// <summary>Times the monitor completed.</summary>
    Completions: Word;
    /// <summary>Times the monitoring conditions were met.</summary>
    Conditions: Word;
    /// <summary>Completions / Conditions; 0 when Conditions is 0.</summary>
    Ratio: Double;
  end;

  /// <summary>In-use performance tracking of one ECU.</summary>
  TOBDInUsePerformance = record
    Ecu: string;
    EcuId: Cardinal;
    /// <summary>True for InfoType 0B (diesel).</summary>
    CompressionIgnition: Boolean;
    /// <summary>OBDCOND — general denominator.</summary>
    GeneralDenominator: Word;
    /// <summary>IGNCNTR — ignition cycle counter.</summary>
    IgnitionCycles: Word;
    /// <summary>Every counter in reported order.</summary>
    Counters: TArray<TOBDIptCounter>;
    /// <summary>Monitor pairs after OBDCOND / IGNCNTR.</summary>
    Monitors: TArray<TOBDIptMonitor>;
  end;

  /// <summary>Everything <c>ReadAll</c> collects.</summary>
  TOBDVehicleInfoReport = record
    /// <summary>Supported InfoTypes (union across ECUs).</summary>
    Supported: TArray<Byte>;
    /// <summary>First VIN reported.</summary>
    VIN: string;
    /// <summary>True when <c>VIN</c> passes the ISO 3779 check.</summary>
    VINValid: Boolean;
    /// <summary>VIN per answering ECU.</summary>
    VINs: TArray<TOBDEcuText>;
    Calibrations: TArray<TOBDCalibration>;
    EcuNames: TArray<TOBDEcuNameInfo>;
    InUsePerformance: TArray<TOBDInUsePerformance>;
    SerialNumbers: TArray<TOBDEcuText>;
    TypeApprovals: TArray<TOBDEcuText>;
    /// <summary>Request family that answered.</summary>
    Source: TOBDVehicleInfoSource;
  end;

  /// <summary>Fires when <c>ReadAllAsync</c> completes.</summary>
  TOBDVehicleInfoEvent = procedure(Sender: TObject;
    const AReport: TOBDVehicleInfoReport) of object;

  /// <summary>Pure Service 09 / J1979-2 response decoding.</summary>
  TOBDVehicleInfoParser = class
  public
    /// <summary>Assembles a Service 09 response into one message per
    /// ECU (CAN NODI form and legacy numbered messages).</summary>
    class function Assemble(const AResponse: TOBDResponse;
      AInfoType: Byte): TArray<TOBDInfoTypeMessage>; static;
    /// <summary>Assembles a J1979-2 <c>62 F8xx</c> response. A
    /// leading NODI byte is dropped when its value matches the item
    /// count implied by the length.</summary>
    class function AssembleUDS(const AResponse: TOBDResponse;
      AInfoType: Byte): TArray<TOBDInfoTypeMessage>; static;
    /// <summary>InfoTypes flagged in the supported bitmaps.</summary>
    class function SupportedInfoTypes(const AMessages
      : TArray<TOBDInfoTypeMessage>): TArray<Byte>; static;
    /// <summary>Splits a text message into items of
    /// <c>AItemSize</c> bytes (0 = one item, or <c>ItemCount</c>
    /// equal parts). Non-printable bytes are dropped.</summary>
    class function TextItems(const AMessage: TOBDInfoTypeMessage;
      AItemSize: Integer): TArray<string>; static;
    /// <summary>CVNs (4 bytes each) as 8 upper-case hex digits.</summary>
    class function CVNs(const AMessage: TOBDInfoTypeMessage)
      : TArray<string>; static;
    /// <summary>Decodes an ECU name message.</summary>
    class function EcuName(const AMessage: TOBDInfoTypeMessage)
      : TOBDEcuNameInfo; static;
    /// <summary>Decodes an in-use performance message (08 / 0B).</summary>
    class function InUsePerformance(const AMessage: TOBDInfoTypeMessage)
      : TOBDInUsePerformance; static;
    /// <summary>Pairs calibration IDs with CVNs per ECU (by ECU id,
    /// else by message order).</summary>
    class function Calibrations(const ACalIds, ACvns
      : TArray<TOBDInfoTypeMessage>): TArray<TOBDCalibration>; static;
  end;

  /// <summary>Vehicle information (Service 09) component.</summary>
  TOBDVehicleInfo = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FSource: TOBDVehicleInfoSource;
    FOwnedTask: TOBDOwnedTask;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnReport: TOBDVehicleInfoEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function ReadOBD(AInfoType: Byte): TArray<TOBDInfoTypeMessage>;
    function ReadUDS(AInfoType: Byte): TArray<TOBDInfoTypeMessage>;
    function TextList(AInfoType: Byte; AItemSize: Integer)
      : TArray<TOBDEcuText>;
    procedure FireReport(const AReport: TOBDVehicleInfoReport);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads one InfoType from every ECU. Empty when no ECU
    /// supports it.</summary>
    /// <exception cref="EOBDConfig">Protocol not assigned.</exception>
    function Read(AInfoType: Byte): TArray<TOBDInfoTypeMessage>;
    /// <summary>Supported InfoTypes 01-20 (union across ECUs).</summary>
    function ReadSupported: TArray<Byte>;
    /// <summary>VIN of the first ECU that reports one.</summary>
    function ReadVIN: string;
    /// <summary>Calibration IDs with their CVNs, per ECU.</summary>
    function ReadCalibrations: TArray<TOBDCalibration>;
    /// <summary>ECU names.</summary>
    function ReadEcuNames: TArray<TOBDEcuNameInfo>;
    /// <summary>In-use performance tracking (spark and compression
    /// ignition), per ECU.</summary>
    function ReadInUsePerformance: TArray<TOBDInUsePerformance>;
    /// <summary>Engine serial numbers.</summary>
    function ReadSerialNumbers: TArray<TOBDEcuText>;
    /// <summary>Exhaust regulation / type-approval numbers.</summary>
    function ReadTypeApprovals: TArray<TOBDEcuText>;
    /// <summary>Reads every supported item. Unsupported items stay
    /// empty; a negative answer on one item does not stop the
    /// rest.</summary>
    function ReadAll: TOBDVehicleInfoReport;
    /// <summary>Non-blocking <see cref="ReadAll"/>; result via
    /// <c>OnReport</c>.</summary>
    procedure ReadAllAsync;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Request family (default: Service 09 with J1979-2
    /// fallback).</summary>
    property Source: TOBDVehicleInfoSource read FSource write FSource
      default visAuto;
    /// <summary>Fires when <c>ReadAllAsync</c> completes (main thread).</summary>
    property OnReport: TOBDVehicleInfoEvent read FOnReport write FOnReport;
    /// <summary>Fires for transient I/O errors (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

const
  IPT_SPARK_NAMES: array [0 .. 19] of string = ('OBDCOND', 'IGNCNTR',
    'CATCOMP1', 'CATCOND1', 'CATCOMP2', 'CATCOND2', 'O2SCOMP1', 'O2SCOND1',
    'O2SCOMP2', 'O2SCOND2', 'EGRCOMP', 'EGRCOND', 'AIRCOMP', 'AIRCOND',
    'EVAPCOMP', 'EVAPCOND', 'SO2SCOMP1', 'SO2SCOND1', 'SO2SCOMP2',
    'SO2SCOND2');
  IPT_SPARK_KEYS: array [0 .. 8] of string = ('catalyst_b1', 'catalyst_b2',
    'o2_sensor_b1', 'o2_sensor_b2', 'egr_vvt', 'secondary_air', 'evap',
    'secondary_o2_b1', 'secondary_o2_b2');
  IPT_SPARK_TITLES: array [0 .. 8] of string = ('Catalyst bank 1',
    'Catalyst bank 2', 'O2 sensor bank 1', 'O2 sensor bank 2', 'EGR / VVT',
    'Secondary air', 'EVAP system', 'Secondary O2 sensor bank 1',
    'Secondary O2 sensor bank 2');
  IPT_COMPRESSION_NAMES: array [0 .. 17] of string = ('OBDCOND', 'IGNCNTR',
    'HCCATCOMP', 'HCCATCOND', 'NCATCOMP', 'NCATCOND', 'NADSCOMP', 'NADSCOND',
    'PMCOMP', 'PMCOND', 'EGSCOMP', 'EGSCOND', 'EGRCOMP', 'EGRCOND', 'BPCOMP',
    'BPCOND', 'FUELCOMP', 'FUELCOND');
  IPT_COMPRESSION_KEYS: array [0 .. 7] of string = ('nmhc_catalyst',
    'nox_catalyst', 'nox_adsorber', 'pm_filter', 'exhaust_gas_sensor',
    'egr_vvt', 'boost_pressure', 'fuel_system');
  IPT_COMPRESSION_TITLES: array [0 .. 7] of string = ('NMHC catalyst',
    'NOx / SCR catalyst', 'NOx adsorber', 'Particulate filter',
    'Exhaust gas sensor', 'EGR / VVT', 'Boost pressure', 'Fuel system');

procedure AppendMessage(var AList: TArray<TOBDInfoTypeMessage>;
  const AMessage: TOBDInfoTypeMessage);
begin
  SetLength(AList, Length(AList) + 1);
  AList[High(AList)] := AMessage;
end;

function AsciiText(const ABytes: TBytes; AStart, ACount: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := AStart to AStart + ACount - 1 do
    if (I >= 0) and (I < Length(ABytes)) and (ABytes[I] >= $20) and
      (ABytes[I] <= $7E) then
      Result := Result + Char(ABytes[I]);
  Result := Trim(Result);
end;

{ TOBDVehicleInfoParser }

class function TOBDVehicleInfoParser.Assemble(const AResponse: TOBDResponse;
  AInfoType: Byte): TArray<TOBDInfoTypeMessage>;
var
  Msgs: TArray<TOBDEcuMessage>;
  LastSeq: TArray<Integer>;
  I, J, Run, N: Integer;
  D: TBytes;
  Item: TOBDInfoTypeMessage;
begin
  Result := nil;
  LastSeq := nil;
  Msgs := OBDPositiveMessages(AResponse, OBD_MODE_INFO + $40);
  for I := 0 to High(Msgs) do
  begin
    D := Msgs[I].Data;
    if (Length(D) < 2) or (D[1] <> AInfoType) then
      Continue;
    Item := Default (TOBDInfoTypeMessage);
    Item.Ecu := Msgs[I].Header;
    Item.EcuId := Msgs[I].CanId;
    Item.InfoType := AInfoType;
    Item.Source := visOBD;
    if AInfoType = OBD_INFOTYPE_SUPPORTED then
    begin
      // CAN: 49 00 A B C D; legacy adds a message number before A.
      if Length(D) < 6 then
        Continue;
      Item.Data := Copy(D, Length(D) - 4, 4);
      AppendMessage(Result, Item);
      SetLength(LastSeq, Length(Result));
      LastSeq[High(LastSeq)] := -1;
      Continue;
    end;
    if Length(D) < 3 then
      Continue;
    // Legacy buses send 4 data bytes per numbered message. Continue
    // the open run of the same ECU when the number follows on.
    Run := -1;
    if Length(D) = 7 then
      for J := High(Result) downto 0 do
        if (Result[J].Ecu = Item.Ecu) and (Result[J].EcuId = Item.EcuId) then
        begin
          if (LastSeq[J] >= 1) and (D[2] = LastSeq[J] + 1) then
            Run := J;
          Break;
        end;
    if Run >= 0 then
    begin
      N := Length(Result[Run].Data);
      SetLength(Result[Run].Data, N + 4);
      Move(D[3], Result[Run].Data[N], 4);
      Result[Run].ItemCount := 0;
      LastSeq[Run] := D[2];
      Continue;
    end;
    Item.ItemCount := D[2];
    Item.Data := Copy(D, 3, MaxInt);
    AppendMessage(Result, Item);
    SetLength(LastSeq, Length(Result));
    if (Length(D) = 7) and (D[2] = 1) then
      LastSeq[High(LastSeq)] := 1
    else
      LastSeq[High(LastSeq)] := -1;
  end;
end;

class function TOBDVehicleInfoParser.AssembleUDS(const AResponse: TOBDResponse;
  AInfoType: Byte): TArray<TOBDInfoTypeMessage>;
var
  Msgs: TArray<TOBDEcuMessage>;
  I, Len, ItemSize: Integer;
  D: TBytes;
  Did: Word;
  Item: TOBDInfoTypeMessage;
  HasNodi: Boolean;
begin
  Result := nil;
  Did := OBD_J1979_2_INFO_DID_BASE + AInfoType;
  Msgs := OBDPositiveMessages(AResponse, UDS_SID_ReadDataByIdentifier +
    UDS_POSITIVE_RESPONSE_OFFSET);
  case AInfoType of
    OBD_INFOTYPE_VIN:
      ItemSize := 17;
    OBD_INFOTYPE_CALID:
      ItemSize := 16;
    OBD_INFOTYPE_CVN:
      ItemSize := 4;
    OBD_INFOTYPE_IPT_SPARK, OBD_INFOTYPE_IPT_COMPRESSION:
      ItemSize := 2;
    OBD_INFOTYPE_ECU_NAME:
      ItemSize := 20;
  else
    ItemSize := 0;
  end;
  for I := 0 to High(Msgs) do
  begin
    D := Msgs[I].Data;
    if (Length(D) < 3) or (D[1] <> Hi(Did)) or (D[2] <> Lo(Did)) then
      Continue;
    Item := Default (TOBDInfoTypeMessage);
    Item.Ecu := Msgs[I].Header;
    Item.EcuId := Msgs[I].CanId;
    Item.InfoType := AInfoType;
    Item.Source := visUDS;
    Len := Length(D) - 3;
    HasNodi := False;
    if (AInfoType <> OBD_INFOTYPE_SUPPORTED) and (Len > 0) then
    begin
      if ItemSize > 0 then
        HasNodi := ((Len - 1) mod ItemSize = 0) and
          (D[3] = (Len - 1) div ItemSize) and (D[3] > 0)
      else
        HasNodi := (D[3] = 1) and (Len > 1);
    end;
    if HasNodi then
    begin
      Item.ItemCount := D[3];
      Item.Data := Copy(D, 4, MaxInt);
    end
    else
    begin
      if ItemSize > 0 then
        Item.ItemCount := Len div ItemSize;
      Item.Data := Copy(D, 3, MaxInt);
    end;
    AppendMessage(Result, Item);
  end;
end;

class function TOBDVehicleInfoParser.SupportedInfoTypes(const AMessages
  : TArray<TOBDInfoTypeMessage>): TArray<Byte>;
var
  Flags: array [1 .. 32] of Boolean;
  I, Bit, N: Integer;
begin
  FillChar(Flags, SizeOf(Flags), 0);
  for I := 0 to High(AMessages) do
    if Length(AMessages[I].Data) >= 4 then
      for Bit := 0 to 31 do
        if (AMessages[I].Data[Bit div 8] and ($80 shr (Bit mod 8))) <> 0 then
          Flags[Bit + 1] := True;
  SetLength(Result, 32);
  N := 0;
  for I := 1 to 32 do
    if Flags[I] then
    begin
      Result[N] := Byte(I);
      Inc(N);
    end;
  SetLength(Result, N);
end;

class function TOBDVehicleInfoParser.TextItems(const AMessage
  : TOBDInfoTypeMessage; AItemSize: Integer): TArray<string>;
var
  Size, Count, I, N: Integer;
  S: string;
begin
  Result := nil;
  if Length(AMessage.Data) = 0 then
    Exit;
  Size := AItemSize;
  if (Size <= 0) and (AMessage.ItemCount > 1) and
    (Length(AMessage.Data) mod AMessage.ItemCount = 0) then
    Size := Length(AMessage.Data) div AMessage.ItemCount;
  if (Size <= 0) or (Size >= Length(AMessage.Data)) then
  begin
    S := AsciiText(AMessage.Data, 0, Length(AMessage.Data));
    if S <> '' then
      Result := TArray<string>.Create(S);
    Exit;
  end;
  Count := (Length(AMessage.Data) + Size - 1) div Size;
  SetLength(Result, Count);
  N := 0;
  for I := 0 to Count - 1 do
  begin
    S := AsciiText(AMessage.Data, I * Size, Size);
    if S <> '' then
    begin
      Result[N] := S;
      Inc(N);
    end;
  end;
  SetLength(Result, N);
end;

class function TOBDVehicleInfoParser.CVNs(const AMessage: TOBDInfoTypeMessage)
  : TArray<string>;
var
  I, Count: Integer;
begin
  Count := Length(AMessage.Data) div 4;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := IntToHex(AMessage.Data[I * 4], 2) +
      IntToHex(AMessage.Data[I * 4 + 1], 2) + IntToHex(AMessage.Data[I * 4 + 2],
      2) + IntToHex(AMessage.Data[I * 4 + 3], 2);
end;

class function TOBDVehicleInfoParser.EcuName(const AMessage
  : TOBDInfoTypeMessage): TOBDEcuNameInfo;
var
  Text: string;
  P: Integer;
begin
  Result := Default (TOBDEcuNameInfo);
  Result.Ecu := AMessage.Ecu;
  Result.EcuId := AMessage.EcuId;
  // 4-character acronym (NUL padded), '-', 15-character text name.
  Text := AsciiText(AMessage.Data, 0, Length(AMessage.Data));
  if (Length(AMessage.Data) >= 5) and (AMessage.Data[4] = Ord('-')) then
  begin
    Result.Acronym := AsciiText(AMessage.Data, 0, 4);
    Result.Name := AsciiText(AMessage.Data, 5, Length(AMessage.Data) - 5);
    Exit;
  end;
  P := Pos('-', Text);
  if P > 0 then
  begin
    Result.Acronym := Trim(Copy(Text, 1, P - 1));
    Result.Name := Trim(Copy(Text, P + 1, MaxInt));
  end
  else
    Result.Name := Text;
end;

class function TOBDVehicleInfoParser.InUsePerformance(const AMessage
  : TOBDInfoTypeMessage): TOBDInUsePerformance;
var
  Count, I, Pairs: Integer;
  Value: Word;
begin
  Result := Default (TOBDInUsePerformance);
  Result.Ecu := AMessage.Ecu;
  Result.EcuId := AMessage.EcuId;
  Result.CompressionIgnition := AMessage.InfoType = OBD_INFOTYPE_IPT_COMPRESSION;
  Count := Length(AMessage.Data) div 2;
  if (AMessage.ItemCount > 0) and (AMessage.ItemCount < Count) then
    Count := AMessage.ItemCount;
  SetLength(Result.Counters, Count);
  for I := 0 to Count - 1 do
  begin
    Value := (Word(AMessage.Data[I * 2]) shl 8) or AMessage.Data[I * 2 + 1];
    Result.Counters[I].Value := Value;
    if Result.CompressionIgnition and (I <= High(IPT_COMPRESSION_NAMES)) then
      Result.Counters[I].Name := IPT_COMPRESSION_NAMES[I]
    else if (not Result.CompressionIgnition) and (I <= High(IPT_SPARK_NAMES))
    then
      Result.Counters[I].Name := IPT_SPARK_NAMES[I]
    else
      Result.Counters[I].Name := Format('COUNTER%d', [I + 1]);
  end;
  if Count >= 1 then
    Result.GeneralDenominator := Result.Counters[0].Value;
  if Count >= 2 then
    Result.IgnitionCycles := Result.Counters[1].Value;
  Pairs := (Count - 2) div 2;
  if Pairs < 0 then
    Pairs := 0;
  SetLength(Result.Monitors, Pairs);
  for I := 0 to Pairs - 1 do
  begin
    Result.Monitors[I].Completions := Result.Counters[2 + I * 2].Value;
    Result.Monitors[I].Conditions := Result.Counters[3 + I * 2].Value;
    if Result.Monitors[I].Conditions > 0 then
      Result.Monitors[I].Ratio := Result.Monitors[I].Completions /
        Result.Monitors[I].Conditions;
    if Result.CompressionIgnition and (I <= High(IPT_COMPRESSION_KEYS)) then
    begin
      Result.Monitors[I].Key := IPT_COMPRESSION_KEYS[I];
      Result.Monitors[I].Name := IPT_COMPRESSION_TITLES[I];
    end
    else if (not Result.CompressionIgnition) and (I <= High(IPT_SPARK_KEYS))
    then
    begin
      Result.Monitors[I].Key := IPT_SPARK_KEYS[I];
      Result.Monitors[I].Name := IPT_SPARK_TITLES[I];
    end
    else
    begin
      Result.Monitors[I].Key := Format('monitor_%d', [I + 1]);
      Result.Monitors[I].Name := Format('Monitor %d', [I + 1]);
    end;
  end;
end;

class function TOBDVehicleInfoParser.Calibrations(const ACalIds, ACvns
  : TArray<TOBDInfoTypeMessage>): TArray<TOBDCalibration>;
var
  I, J, K, N: Integer;
  Ids, Cvn: TArray<string>;
  Match: Integer;
begin
  Result := nil;
  N := 0;
  for I := 0 to High(ACalIds) do
  begin
    Ids := TextItems(ACalIds[I], 16);
    // The CVN message of the same ECU: by identifier when headers
    // are on, else by message order.
    Match := -1;
    if ACalIds[I].EcuId <> 0 then
    begin
      for J := 0 to High(ACvns) do
        if ACvns[J].EcuId = ACalIds[I].EcuId then
        begin
          Match := J;
          Break;
        end;
    end
    else if I <= High(ACvns) then
      Match := I;
    if Match >= 0 then
      Cvn := CVNs(ACvns[Match])
    else
      Cvn := nil;
    SetLength(Result, N + Length(Ids));
    for K := 0 to High(Ids) do
    begin
      Result[N].Ecu := ACalIds[I].Ecu;
      Result[N].EcuId := ACalIds[I].EcuId;
      Result[N].CalibrationId := Ids[K];
      Result[N].HasCVN := K <= High(Cvn);
      if Result[N].HasCVN then
        Result[N].CVN := Cvn[K]
      else
        Result[N].CVN := '';
      Inc(N);
    end;
  end;
end;

{ TOBDVehicleInfo }

constructor TOBDVehicleInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSource := visAuto;
  FAsyncLock := TCriticalSection.Create;
  FOwnedTask := TOBDOwnedTask.Create;
end;

destructor TOBDVehicleInfo.Destroy;
begin
  if FOwnedTask <> nil then
    FOwnedTask.Cancel;
  FreeAndNil(FOwnedTask);
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDVehicleInfo.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('Async start requires the main thread');
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDVehicleInfo: async read already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDVehicleInfo.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDVehicleInfo.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FOwnedTask <> nil then
    FOwnedTask.Quiesce;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDVehicleInfo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
  begin
    if FOwnedTask <> nil then
      FOwnedTask.Quiesce;
    FProtocol := nil;
  end;
end;

function TOBDVehicleInfo.ReadOBD(AInfoType: Byte): TArray<TOBDInfoTypeMessage>;
var
  Resp: TOBDResponse;
begin
  Resp := FProtocol.Request(OBD_MODE_INFO, TBytes.Create(AInfoType));
  if Resp.IsNegative then
    Exit(nil);
  Result := TOBDVehicleInfoParser.Assemble(Resp, AInfoType);
end;

function TOBDVehicleInfo.ReadUDS(AInfoType: Byte): TArray<TOBDInfoTypeMessage>;
var
  Resp: TOBDResponse;
  Did: Word;
begin
  Did := OBD_J1979_2_INFO_DID_BASE + AInfoType;
  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier,
    TBytes.Create(Hi(Did), Lo(Did)));
  if Resp.IsNegative then
    Exit(nil);
  Result := TOBDVehicleInfoParser.AssembleUDS(Resp, AInfoType);
end;

function TOBDVehicleInfo.Read(AInfoType: Byte): TArray<TOBDInfoTypeMessage>;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDVehicleInfo: Protocol not assigned');
  case FSource of
    visOBD:
      Result := ReadOBD(AInfoType);
    visUDS:
      Result := ReadUDS(AInfoType);
  else
    begin
      Result := ReadOBD(AInfoType);
      if Length(Result) = 0 then
        Result := ReadUDS(AInfoType);
    end;
  end;
end;

function TOBDVehicleInfo.ReadSupported: TArray<Byte>;
begin
  Result := TOBDVehicleInfoParser.SupportedInfoTypes
    (Read(OBD_INFOTYPE_SUPPORTED));
end;

function TOBDVehicleInfo.TextList(AInfoType: Byte; AItemSize: Integer)
  : TArray<TOBDEcuText>;
var
  Msgs: TArray<TOBDInfoTypeMessage>;
  Items: TArray<string>;
  I, K, N: Integer;
begin
  Result := nil;
  N := 0;
  Msgs := Read(AInfoType);
  for I := 0 to High(Msgs) do
  begin
    Items := TOBDVehicleInfoParser.TextItems(Msgs[I], AItemSize);
    SetLength(Result, N + Length(Items));
    for K := 0 to High(Items) do
    begin
      Result[N].Ecu := Msgs[I].Ecu;
      Result[N].EcuId := Msgs[I].EcuId;
      Result[N].Text := Items[K];
      Inc(N);
    end;
  end;
end;

function TOBDVehicleInfo.ReadVIN: string;
var
  List: TArray<TOBDEcuText>;
begin
  List := TextList(OBD_INFOTYPE_VIN, 0);
  if Length(List) > 0 then
    Result := List[0].Text
  else
    Result := '';
end;

function TOBDVehicleInfo.ReadCalibrations: TArray<TOBDCalibration>;
var
  Ids, Cvns: TArray<TOBDInfoTypeMessage>;
begin
  Ids := Read(OBD_INFOTYPE_CALID);
  if Length(Ids) = 0 then
    Exit(nil);
  Cvns := Read(OBD_INFOTYPE_CVN);
  Result := TOBDVehicleInfoParser.Calibrations(Ids, Cvns);
end;

function TOBDVehicleInfo.ReadEcuNames: TArray<TOBDEcuNameInfo>;
var
  Msgs: TArray<TOBDInfoTypeMessage>;
  I: Integer;
begin
  Msgs := Read(OBD_INFOTYPE_ECU_NAME);
  SetLength(Result, Length(Msgs));
  for I := 0 to High(Msgs) do
    Result[I] := TOBDVehicleInfoParser.EcuName(Msgs[I]);
end;

function TOBDVehicleInfo.ReadInUsePerformance: TArray<TOBDInUsePerformance>;
var
  Msgs: TArray<TOBDInfoTypeMessage>;
  Kinds: array [0 .. 1] of Byte;
  I, K, N: Integer;
begin
  Result := nil;
  N := 0;
  Kinds[0] := OBD_INFOTYPE_IPT_SPARK;
  Kinds[1] := OBD_INFOTYPE_IPT_COMPRESSION;
  for K := 0 to 1 do
  begin
    Msgs := Read(Kinds[K]);
    SetLength(Result, N + Length(Msgs));
    for I := 0 to High(Msgs) do
    begin
      Result[N] := TOBDVehicleInfoParser.InUsePerformance(Msgs[I]);
      Inc(N);
    end;
  end;
end;

function TOBDVehicleInfo.ReadSerialNumbers: TArray<TOBDEcuText>;
begin
  Result := TextList(OBD_INFOTYPE_ESN, 0);
end;

function TOBDVehicleInfo.ReadTypeApprovals: TArray<TOBDEcuText>;
begin
  Result := TextList(OBD_INFOTYPE_EROTAN, 0);
end;

function HasInfoType(const ASupported: TArray<Byte>;
  AInfoType: Byte): Boolean;
var
  I: Integer;
begin
  // Without a bitmap (old or non-compliant ECUs) try everything.
  if Length(ASupported) = 0 then
    Exit(True);
  for I := 0 to High(ASupported) do
    if ASupported[I] = AInfoType then
      Exit(True);
  Result := False;
end;

function TOBDVehicleInfo.ReadAll: TOBDVehicleInfoReport;
var
  SavedSource: TOBDVehicleInfoSource;
  Probe: TArray<TOBDInfoTypeMessage>;
  Sup: TArray<Byte>;
begin
  Result := Default (TOBDVehicleInfoReport);
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDVehicleInfo: Protocol not assigned');
  SavedSource := FSource;
  try
    // Settle the request family once so every item uses the same.
    Probe := Read(OBD_INFOTYPE_SUPPORTED);
    if Length(Probe) > 0 then
      FSource := Probe[0].Source;
    Result.Source := FSource;
    Sup := TOBDVehicleInfoParser.SupportedInfoTypes(Probe);
    Result.Supported := Sup;
    if HasInfoType(Sup, OBD_INFOTYPE_VIN) then
    begin
      Result.VINs := TextList(OBD_INFOTYPE_VIN, 0);
      if Length(Result.VINs) > 0 then
      begin
        Result.VIN := Result.VINs[0].Text;
        Result.VINValid := TOBDVINValidator.IsValid(Result.VIN);
      end;
    end;
    if HasInfoType(Sup, OBD_INFOTYPE_CALID) then
      Result.Calibrations := ReadCalibrations;
    if HasInfoType(Sup, OBD_INFOTYPE_ECU_NAME) then
      Result.EcuNames := ReadEcuNames;
    if HasInfoType(Sup, OBD_INFOTYPE_IPT_SPARK) or HasInfoType(Sup, OBD_INFOTYPE_IPT_COMPRESSION) then
      Result.InUsePerformance := ReadInUsePerformance;
    if HasInfoType(Sup, OBD_INFOTYPE_ESN) then
      Result.SerialNumbers := ReadSerialNumbers;
    if HasInfoType(Sup, OBD_INFOTYPE_EROTAN) then
      Result.TypeApprovals := ReadTypeApprovals;
  finally
    FSource := SavedSource;
  end;
end;

procedure TOBDVehicleInfo.ReadAllAsync;
var
  Self_: TOBDVehicleInfo;
begin
  GuardSingleAsync;
  try
    Self_ := Self;
    FOwnedTask.Start(
      procedure
      var
        R: TOBDVehicleInfoReport;
      begin
        try
          try
            R := Self_.ReadAll;
            Self_.FireReport(R);
          except
            on E: Exception do
              Self_.FireError(oeIO, E.Message);
          end;
        finally
          Self_.ReleaseAsync;
        end;
      end);
  except
    ReleaseAsync;
    raise;
  end;
end;

procedure TOBDVehicleInfo.FireReport(const AReport: TOBDVehicleInfoReport);
var
  Self_: TOBDVehicleInfo;
  Snap: TOBDVehicleInfoReport;
begin
  Self_ := Self;
  Snap := AReport;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    try
{$IFNDEF FPC}TBindings.Notify(Self_, ''); {$ENDIF}
    except
    end;
    if Assigned(FOnReport) then
      FOnReport(Self_, Snap);
  end
  else
    FOwnedTask.Post(
      procedure
      begin
        try
{$IFNDEF FPC}TBindings.Notify(Self_, ''); {$ENDIF}
        except
        end;
        if Assigned(Self_.FOnReport) then
          Self_.FOnReport(Self_, Snap);
      end);
end;

procedure TOBDVehicleInfo.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDVehicleInfo;
  Code: TOBDErrorCode;
  Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    FOwnedTask.Post(
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
