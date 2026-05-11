//------------------------------------------------------------------------------
//  OBD.Diagnostics.UDS.ReadDTC
//
//  TOBDUDSReadDTC — non-visual component for the UDS
//  ReadDTCInformation service (SID 0x19). Distinct from
//  TOBDDTCs.ReadUDS (which only exposes sub-function 0x02
//  reportDTCByStatusMask): this component handles every sub-function
//  the host might need, returning the response payload verbatim
//  for sub-functions whose record shape is OEM-defined.
//
//  Wire format per ISO 14229-1 §11.3:
//
//    Request : 19 <subFunction> [<status mask | DTC | …>]
//    Response: 59 <subFunction> [<DTCStatusAvailMask>] [<records>]
//
//  Three convenience methods cover the common cases:
//    - ReadByStatusMask (sub 0x02) — every DTC matching a mask
//    - ReadSupportedDTCs (sub 0x0A) — every supported DTC
//    - ReadByDTCNumber (sub 0x06) — extended data record by DTC
//
//  Plus the universal Send(subFunction, body) escape hatch for
//  any sub-function not covered by a convenience helper.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §11.3 (ReadDTCInformation)
//    - ISO 15031-5 § 7 — J2012 DTC encoding (P/C/B/U)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.UDS.ReadDTC;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol;

const
  /// <summary>0x01 reportNumberOfDTCByStatusMask.</summary>
  UDS_RDTC_SUB_NumberByStatusMask        = $01;
  /// <summary>0x02 reportDTCByStatusMask.</summary>
  UDS_RDTC_SUB_ByStatusMask              = $02;
  /// <summary>0x03 reportDTCSnapshotIdentification.</summary>
  UDS_RDTC_SUB_SnapshotIdentification    = $03;
  /// <summary>0x04 reportDTCSnapshotRecordByDTCNumber.</summary>
  UDS_RDTC_SUB_SnapshotRecordByDTCNumber = $04;
  /// <summary>0x06 reportDTCExtDataRecordByDTCNumber.</summary>
  UDS_RDTC_SUB_ExtDataRecordByDTCNumber  = $06;
  /// <summary>0x0A reportSupportedDTC.</summary>
  UDS_RDTC_SUB_SupportedDTC              = $0A;

type
  /// <summary>
  ///   One decoded DTC entry.
  /// </summary>
  TOBDUDSDtcEntry = record
    /// <summary>5-character J2012 DTC code (e.g. P0420).</summary>
    Code: string;
    /// <summary>UDS DTC-status byte (test failed bits).</summary>
    Status: Byte;
    /// <summary>Raw 3-byte DTC + 1-byte status as returned by the
    /// ECU.</summary>
    Raw: TBytes;
  end;

  /// <summary>
  ///   Fires after a successful DTC sweep. Main thread.
  /// </summary>
  TOBDUDSReadDTCEvent = procedure(Sender: TObject;
    ASubFunction: Byte;
    const AEntries: TArray<TOBDUDSDtcEntry>) of object;

  /// <summary>
  ///   Fires after a raw sub-function send. Main thread.
  /// </summary>
  TOBDUDSReadDTCRawEvent = procedure(Sender: TObject;
    ASubFunction: Byte; const AData: TBytes) of object;

  /// <summary>
  ///   UDS ReadDTCInformation component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form and assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>. Use one of the
  ///   convenience helpers (<see cref="ReadByStatusMask"/>,
  ///   <see cref="ReadSupportedDTCs"/>,
  ///   <see cref="ReadByDTCNumber"/>) for the common sub-functions,
  ///   or <see cref="Send"/> for anything else (the raw payload
  ///   surfaces via <c>OnRaw</c>).
  /// </remarks>
  TOBDUDSReadDTC = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnRead: TOBDUDSReadDTCEvent;
    FOnRaw: TOBDUDSReadDTCRawEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoSend(ASubFunction: Byte;
      const ABody: TBytes): TBytes;
    function DecodeDtcRecords(
      const AData: TBytes; AStart: Integer): TArray<TOBDUDSDtcEntry>;
    procedure FireRead(ASubFunction: Byte;
      const AEntries: TArray<TOBDUDSDtcEntry>);
    procedure FireRaw(ASubFunction: Byte; const AData: TBytes);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Decodes a 2-byte raw DTC into the J2012 5-character form.
    /// </summary>
    /// <param name="AHi">High DTC byte.</param>
    /// <param name="ALo">Low DTC byte.</param>
    /// <returns>Decoded DTC string.</returns>
    class function DecodeJ2012(AHi: Byte; ALo: Byte): string; static;

    /// <summary>
    ///   Reads every DTC whose status byte intersects <c>AMask</c>
    ///   (sub-function 0x02).
    /// </summary>
    /// <param name="AMask">DTC-status bitmask.</param>
    /// <returns>Decoded entries.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response.
    /// </exception>
    function ReadByStatusMask(AMask: Byte = $FF): TArray<TOBDUDSDtcEntry>;

    /// <summary>
    ///   Reads every supported DTC (sub-function 0x0A).
    /// </summary>
    /// <returns>Decoded entries. Each entry's status byte is the
    /// reported DTCStatus.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response.
    /// </exception>
    function ReadSupportedDTCs: TArray<TOBDUDSDtcEntry>;

    /// <summary>
    ///   Reads the extended data record for one DTC
    ///   (sub-function 0x06).
    /// </summary>
    /// <param name="ADtcHi">High DTC byte.</param>
    /// <param name="ADtcMid">Mid DTC byte.</param>
    /// <param name="ADtcLo">Low DTC byte.</param>
    /// <param name="AExtDataRecordNumber">Extended-data-record
    /// number (0xFF = all records).</param>
    /// <returns>Raw response payload (sub-function + DTC + status
    /// + records).</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    function ReadByDTCNumber(ADtcHi: Byte; ADtcMid: Byte; ADtcLo: Byte;
      AExtDataRecordNumber: Byte = $FF): TBytes;

    /// <summary>
    ///   Universal raw sub-function send.
    /// </summary>
    /// <param name="ASubFunction">Sub-function byte.</param>
    /// <param name="ABody">Optional payload after the sub-function
    /// byte.</param>
    /// <returns>Response payload after the sub-function byte.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    function Send(ASubFunction: Byte; const ABody: TBytes = nil): TBytes;

    /// <summary>Non-blocking <see cref="ReadByStatusMask"/>.</summary>
    /// <param name="AMask">DTC-status bitmask.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadByStatusMaskAsync(AMask: Byte = $FF);

    /// <summary>Non-blocking <see cref="ReadSupportedDTCs"/>.</summary>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadSupportedDTCsAsync;
  published
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires on a successful decode. Main thread.</summary>
    property OnRead: TOBDUDSReadDTCEvent read FOnRead write FOnRead;
    /// <summary>Fires after a raw <see cref="Send"/>. Main thread.</summary>
    property OnRaw: TOBDUDSReadDTCRawEvent read FOnRaw write FOnRaw;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSReadDTC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDUDSReadDTC.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSReadDTC.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSReadDTC.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSReadDTC.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSReadDTC: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSReadDTC.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

class function TOBDUDSReadDTC.DecodeJ2012(AHi: Byte; ALo: Byte): string;
const
  Prefix: array[0..3] of Char = ('P', 'C', 'B', 'U');
var
  PrefixIdx: Integer;
begin
  PrefixIdx := (AHi shr 6) and $03;
  Result := Prefix[PrefixIdx] +
            IntToHex((AHi shr 4) and $03, 1) +
            IntToHex(AHi and $0F, 1) +
            IntToHex((ALo shr 4) and $0F, 1) +
            IntToHex(ALo and $0F, 1);
end;

function TOBDUDSReadDTC.DecodeDtcRecords(const AData: TBytes;
  AStart: Integer): TArray<TOBDUDSDtcEntry>;
var
  Off: Integer;
  Acc: TList<TOBDUDSDtcEntry>;
  E: TOBDUDSDtcEntry;
begin
  Acc := TList<TOBDUDSDtcEntry>.Create;
  try
    Off := AStart;
    // Each record: <DTChi> <DTCmid> <DTClo> <statusOfDTC> — 4 bytes.
    while Off + 4 <= Length(AData) do
    begin
      E := Default(TOBDUDSDtcEntry);
      E.Code := DecodeJ2012(AData[Off], AData[Off + 1]);
      E.Status := AData[Off + 3];
      E.Raw := Copy(AData, Off, 4);
      Acc.Add(E);
      Inc(Off, 4);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDUDSReadDTC.DoSend(ASubFunction: Byte;
  const ABody: TBytes): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSReadDTC: Protocol not assigned');
  SetLength(Req, 1 + Length(ABody));
  Req[0] := ASubFunction;
  if Length(ABody) > 0 then
    Move(ABody[0], Req[1], Length(ABody));

  Resp := FProtocol.Request(UDS_SID_ReadDTCInformation, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDTCInformation (sub 0x%.2x) negative: %s',
      [ASubFunction, Resp.NRCText]);
  // Response is <subFunction> + payload — return payload verbatim.
  Result := Copy(Resp.Data, 0, Length(Resp.Data));
end;

function TOBDUDSReadDTC.ReadByStatusMask(AMask: Byte): TArray<TOBDUDSDtcEntry>;
var
  Body: TBytes;
  Raw: TBytes;
begin
  SetLength(Body, 1);
  Body[0] := AMask;
  Raw := DoSend(UDS_RDTC_SUB_ByStatusMask, Body);
  // Response per ISO 14229-1 §11.3.5.4:
  //   <subFunction> <DTCStatusAvailMask> [<DTC hi mid lo> <status>]*
  // DoSend strips the leading sub-function byte echo from the
  // returned payload — the next byte is DTCStatusAvailMask, then
  // the records start at offset 1.
  if Length(Raw) >= 1 then
    Result := DecodeDtcRecords(Raw, 1)
  else
    SetLength(Result, 0);
  FireRead(UDS_RDTC_SUB_ByStatusMask, Result);
end;

function TOBDUDSReadDTC.ReadSupportedDTCs: TArray<TOBDUDSDtcEntry>;
var
  Raw: TBytes;
begin
  Raw := DoSend(UDS_RDTC_SUB_SupportedDTC, nil);
  // Response: <subFunction echo> <DTCStatusAvailMask>
  //           [<DTC hi mid lo> <status>]*
  if Length(Raw) >= 1 then
    Result := DecodeDtcRecords(Raw, 1)
  else
    SetLength(Result, 0);
  FireRead(UDS_RDTC_SUB_SupportedDTC, Result);
end;

function TOBDUDSReadDTC.ReadByDTCNumber(ADtcHi: Byte; ADtcMid: Byte;
  ADtcLo: Byte; AExtDataRecordNumber: Byte): TBytes;
var
  Body: TBytes;
begin
  SetLength(Body, 4);
  Body[0] := ADtcHi;
  Body[1] := ADtcMid;
  Body[2] := ADtcLo;
  Body[3] := AExtDataRecordNumber;
  Result := DoSend(UDS_RDTC_SUB_ExtDataRecordByDTCNumber, Body);
  FireRaw(UDS_RDTC_SUB_ExtDataRecordByDTCNumber, Result);
end;

function TOBDUDSReadDTC.Send(ASubFunction: Byte;
  const ABody: TBytes): TBytes;
begin
  Result := DoSend(ASubFunction, ABody);
  FireRaw(ASubFunction, Result);
end;

procedure TOBDUDSReadDTC.ReadByStatusMaskAsync(AMask: Byte);
var
  Self_: TOBDUDSReadDTC;
  Mask: Byte;
begin
  GuardSingleAsync;
  Self_ := Self;
  Mask := AMask;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadByStatusMask(Mask);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSReadDTC.ReadSupportedDTCsAsync;
var
  Self_: TOBDUDSReadDTC;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadSupportedDTCs;
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSReadDTC.FireRead(ASubFunction: Byte;
  const AEntries: TArray<TOBDUDSDtcEntry>);
var
  Self_: TOBDUDSReadDTC;
  Sub: Byte;
  Snap: TArray<TOBDUDSDtcEntry>;
begin
  if not Assigned(FOnRead) then
    Exit;
  Self_ := Self;
  Sub := ASubFunction;
  Snap := Copy(AEntries, 0, Length(AEntries));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRead(Self_, Sub, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnRead) then
          Self_.FOnRead(Self_, Sub, Snap);
      end);
end;

procedure TOBDUDSReadDTC.FireRaw(ASubFunction: Byte; const AData: TBytes);
var
  Self_: TOBDUDSReadDTC;
  Sub: Byte;
  Snap: TBytes;
begin
  if not Assigned(FOnRaw) then
    Exit;
  Self_ := Self;
  Sub := ASubFunction;
  Snap := Copy(AData, 0, Length(AData));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRaw(Self_, Sub, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnRaw) then
          Self_.FOnRaw(Self_, Sub, Snap);
      end);
end;

procedure TOBDUDSReadDTC.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSReadDTC;
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
    TThread.Queue(nil,
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
