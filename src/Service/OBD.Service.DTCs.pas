//------------------------------------------------------------------------------
//  OBD.Service.DTCs
//
//  TOBDDTCs — non-visual component that reads stored / pending /
//  permanent Diagnostic Trouble Codes from the ECU. Covers:
//
//    - OBD-II Mode 03 — current (confirmed) DTCs
//    - OBD-II Mode 07 — pending DTCs
//    - OBD-II Mode 0A — permanent DTCs
//    - UDS Service 0x19 — ReadDTCInformation (sub-function 0x02
//      reportDTCByStatusMask)
//
//  Decodes the 2-byte raw codes into the SAE J2012 string form
//  ("P0301", "C0123", …). Optionally resolves human-readable text
//  via OBD.Catalog when a DTC catalogue is loaded.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J2012 (DTC nomenclature)
//    - ISO 15031-6 (OBD-II diagnostic trouble codes)
//    - ISO 14229-1 § 11.3.5 (UDS ReadDTCInformation)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Service.DTCs;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Data.Bind.Components,
  OBD.Types,
  OBD.Catalog,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol,
  OBD.Service.Catalog;

const
  /// <summary>OBD-II Mode 03 — current (confirmed) DTCs.</summary>
  OBD_MODE_CURRENT_DTCS    = $03;
  /// <summary>OBD-II Mode 07 — pending DTCs.</summary>
  OBD_MODE_PENDING_DTCS    = $07;
  /// <summary>OBD-II Mode 0A — permanent DTCs.</summary>
  OBD_MODE_PERMANENT_DTCS  = $0A;

  /// <summary>UDS Service 0x19 sub-function: report DTC by status
  /// mask.</summary>
  UDS_DTC_REPORT_BY_STATUS_MASK = $02;
  /// <summary>UDS Service 0x19 sub-function: report DTC snapshot
  /// identification.</summary>
  UDS_DTC_REPORT_SNAPSHOT_IDS   = $03;

type
  /// <summary>Source bucket for a DTC entry.</summary>
  TOBDDtcKind = (
    dkConfirmed,    // Mode 03 / UDS confirmed
    dkPending,      // Mode 07
    dkPermanent,    // Mode 0A
    dkUDSStatusMask // UDS Service 0x19 / 0x02
  );

  /// <summary>One decoded DTC entry.</summary>
  TOBDDtcEntry = record
    /// <summary>Raw 2- or 3-byte code from the ECU.</summary>
    Raw: TBytes;
    /// <summary>SAE J2012 string form (e.g. <c>"P0301"</c>).</summary>
    Code: string;
    /// <summary>Source bucket.</summary>
    Kind: TOBDDtcKind;
    /// <summary>UDS status byte (only meaningful for
    /// <c>dkUDSStatusMask</c>).</summary>
    Status: Byte;
    /// <summary>Whether <c>Status</c> is present.</summary>
    HasStatus: Boolean;
    /// <summary>Resolved description from <c>OBD.Catalog</c>; empty
    /// when no catalogue match.</summary>
    Description: string;
  end;

  /// <summary>Fires when a DTC read completes.</summary>
  TOBDDtcsEvent = procedure(Sender: TObject;
    AKind: TOBDDtcKind;
    const AEntries: TArray<TOBDDtcEntry>) of object;

  /// <summary>
  ///   Diagnostic-Trouble-Codes service component.
  /// </summary>
  TOBDDTCs = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnDTCs: TOBDDtcsEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;

    function ReadOBDMode(AMode: Byte; AKind: TOBDDtcKind): TArray<TOBDDtcEntry>;
    function ReadUDSStatusMask(AStatusMask: Byte): TArray<TOBDDtcEntry>;
    procedure DispatchAsync(AKind: TOBDDtcKind;
      AImpl: TFunc<TArray<TOBDDtcEntry>>);
    procedure FireDTCs(AKind: TOBDDtcKind;
      const AEntries: TArray<TOBDDtcEntry>);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads confirmed (Mode 03) DTCs synchronously.</summary>
    function ReadConfirmed: TArray<TOBDDtcEntry>;
    /// <summary>Non-blocking <see cref="ReadConfirmed"/>.</summary>
    procedure ReadConfirmedAsync;

    /// <summary>Reads pending (Mode 07) DTCs synchronously.</summary>
    function ReadPending: TArray<TOBDDtcEntry>;
    /// <summary>Non-blocking <see cref="ReadPending"/>.</summary>
    procedure ReadPendingAsync;

    /// <summary>Reads permanent (Mode 0A) DTCs synchronously.</summary>
    function ReadPermanent: TArray<TOBDDtcEntry>;
    /// <summary>Non-blocking <see cref="ReadPermanent"/>.</summary>
    procedure ReadPermanentAsync;

    /// <summary>
    ///   UDS Service 0x19 sub-function 0x02. Each entry carries
    ///   <c>Status</c> + 3-byte raw code.
    /// </summary>
    /// <param name="AStatusMask">Status bits the host wants
    /// (typical: <c>$FF</c> for "all DTCs").</param>
    function ReadUDS(AStatusMask: Byte = $FF): TArray<TOBDDtcEntry>;
    /// <summary>Non-blocking <see cref="ReadUDS"/>.</summary>
    procedure ReadUDSAsync(AStatusMask: Byte = $FF);

    /// <summary>
    ///   Clears confirmed DTCs (OBD-II Mode 04 / UDS Service
    ///   0x14 ClearDiagnosticInformation). Returns when the ECU
    ///   acknowledges.
    /// </summary>
    /// <exception cref="EOBDProtocolErr">ECU refused or transient
    /// error.</exception>
    procedure Clear;

    /// <summary>Decodes a 2-byte SAE J2012 raw DTC into its string
    /// form (e.g. <c>$03,$01</c> → <c>"P0301"</c>).</summary>
    class function DecodeJ2012(AHi, ALo: Byte): string; static;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    property OnDTCs: TOBDDtcsEvent read FOnDTCs write FOnDTCs;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

const
  CDtcLetters: array[0..3] of Char = ('P', 'C', 'B', 'U');

constructor TOBDDTCs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDDTCs.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDDTCs.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDDTCs: async read already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDDTCs.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDDTCs.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDDTCs.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

class function TOBDDTCs.DecodeJ2012(AHi, ALo: Byte): string;
var
  Letter: Char;
  FirstDigit: Byte;
begin
  Letter := CDtcLetters[(AHi shr 6) and $03];
  FirstDigit := (AHi shr 4) and $03;
  Result := Letter + IntToStr(FirstDigit) +
    IntToHex(AHi and $0F, 1) + IntToHex(ALo, 2);
end;

function ResolveDtcText(const ACode: string): string;
var
  Info: TOBDDtcInfo;
begin
  Result := '';
  // 1. JSON DTC catalogue (richest data, including severity).
  if TOBDServiceCatalog.Default.TryGetDTC(ACode, Info) then
  begin
    Result := Info.Description;
    if Result <> '' then Exit;
  end;
  // 2. Legacy v1 schema catalogue. Encode the family letter in
  //    the top byte so the catalogue can split P / C / B / U.
  if Length(ACode) < 5 then Exit;
  var HexPart: string := Copy(ACode, 2, 4);
  var ParsedHex: Integer;
  if not TryStrToInt('$' + HexPart, ParsedHex) then Exit;
  var Numeric: Cardinal := Cardinal(ParsedHex);
  case ACode[1] of
    'P': Numeric := Numeric or $00000000;
    'C': Numeric := Numeric or $00010000;
    'B': Numeric := Numeric or $00020000;
    'U': Numeric := Numeric or $00030000;
  end;
  TOBDCatalogStore.Default.FindText(ckOBD2DTC, Numeric, Result);
end;

function ParseTwoByteList(const AData: TBytes; AStart: Integer;
  AKind: TOBDDtcKind): TArray<TOBDDtcEntry>;
var
  I, Count: Integer;
  Entry: TOBDDtcEntry;
  Acc: TArray<TOBDDtcEntry>;
begin
  Count := (Length(AData) - AStart) div 2;
  if Count <= 0 then Exit(nil);
  SetLength(Acc, Count);
  for I := 0 to Count - 1 do
  begin
    Entry := Default(TOBDDtcEntry);
    Entry.Kind := AKind;
    SetLength(Entry.Raw, 2);
    Entry.Raw[0] := AData[AStart + I * 2];
    Entry.Raw[1] := AData[AStart + I * 2 + 1];
    // 0x0000 is a sentinel meaning "no DTC" — skip silently.
    if (Entry.Raw[0] = 0) and (Entry.Raw[1] = 0) then
    begin
      SetLength(Acc, I);
      Break;
    end;
    Entry.Code := TOBDDTCs.DecodeJ2012(Entry.Raw[0], Entry.Raw[1]);
    Entry.Description := ResolveDtcText(Entry.Code);
    Acc[I] := Entry;
  end;
  Result := Acc;
end;

function TOBDDTCs.ReadOBDMode(AMode: Byte;
  AKind: TOBDDtcKind): TArray<TOBDDtcEntry>;
var
  Resp: TOBDResponse;
  Start: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDDTCs: Protocol not assigned');
  Resp := FProtocol.Request(AMode, nil);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'OBD Mode 0x%2.2X negative: %s', [AMode, Resp.NRCText]);
  // Some controllers prepend a 1-byte count. Detect by parity:
  // an odd-length payload means there's a count byte first.
  if Odd(Length(Resp.Data)) then
    Start := 1
  else
    Start := 0;
  Result := ParseTwoByteList(Resp.Data, Start, AKind);
end;

function TOBDDTCs.ReadUDSStatusMask(
  AStatusMask: Byte): TArray<TOBDDtcEntry>;
var
  Resp: TOBDResponse;
  I, Off, N: Integer;
  Entry: TOBDDtcEntry;
  Acc: TArray<TOBDDtcEntry>;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDDTCs: Protocol not assigned');
  Resp := FProtocol.Request(UDS_SID_ReadDTCInformation,
    TBytes.Create(UDS_DTC_REPORT_BY_STATUS_MASK, AStatusMask));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'UDS 19 02 negative: %s', [Resp.NRCText]);
  // Response Data: <subFunc echo> <statusAvailMask> [DTC3 STATUS]*
  if Length(Resp.Data) < 2 then Exit(nil);
  Off := 2;
  N := (Length(Resp.Data) - Off) div 4;
  if N <= 0 then Exit(nil);
  SetLength(Acc, N);
  for I := 0 to N - 1 do
  begin
    Entry := Default(TOBDDtcEntry);
    Entry.Kind := dkUDSStatusMask;
    SetLength(Entry.Raw, 3);
    Entry.Raw[0] := Resp.Data[Off + I * 4];
    Entry.Raw[1] := Resp.Data[Off + I * 4 + 1];
    Entry.Raw[2] := Resp.Data[Off + I * 4 + 2];
    Entry.Status := Resp.Data[Off + I * 4 + 3];
    Entry.HasStatus := True;
    // J2012 letter is in the top 2 bits of byte 0; bottom byte goes
    // into the low nibble. UDS DTC is 24 bits — the third byte
    // expands the 4-hex-digit code to 6 hex digits.
    Entry.Code := TOBDDTCs.DecodeJ2012(Entry.Raw[0], Entry.Raw[1]) +
      IntToHex(Entry.Raw[2], 2);
    Entry.Description := ResolveDtcText(
      TOBDDTCs.DecodeJ2012(Entry.Raw[0], Entry.Raw[1]));
    Acc[I] := Entry;
  end;
  Result := Acc;
end;

function TOBDDTCs.ReadConfirmed: TArray<TOBDDtcEntry>;
begin
  Result := ReadOBDMode(OBD_MODE_CURRENT_DTCS, dkConfirmed);
  FireDTCs(dkConfirmed, Result);
end;

function TOBDDTCs.ReadPending: TArray<TOBDDtcEntry>;
begin
  Result := ReadOBDMode(OBD_MODE_PENDING_DTCS, dkPending);
  FireDTCs(dkPending, Result);
end;

function TOBDDTCs.ReadPermanent: TArray<TOBDDtcEntry>;
begin
  Result := ReadOBDMode(OBD_MODE_PERMANENT_DTCS, dkPermanent);
  FireDTCs(dkPermanent, Result);
end;

function TOBDDTCs.ReadUDS(AStatusMask: Byte): TArray<TOBDDtcEntry>;
begin
  Result := ReadUDSStatusMask(AStatusMask);
  FireDTCs(dkUDSStatusMask, Result);
end;

procedure TOBDDTCs.DispatchAsync(AKind: TOBDDtcKind;
  AImpl: TFunc<TArray<TOBDDtcEntry>>);
var
  Self_: TOBDDTCs;
  Kind: TOBDDtcKind;
  Impl: TFunc<TArray<TOBDDtcEntry>>;
begin
  GuardSingleAsync;
  Self_ := Self; Kind := AKind; Impl := AImpl;
  TThread.CreateAnonymousThread(
    procedure
    var
      Entries: TArray<TOBDDtcEntry>;
    begin
      try
        try
          Entries := Impl();
          Self_.FireDTCs(Kind, Entries);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDDTCs.ReadConfirmedAsync;
begin
  DispatchAsync(dkConfirmed,
    function: TArray<TOBDDtcEntry>
    begin
      Result := ReadOBDMode(OBD_MODE_CURRENT_DTCS, dkConfirmed);
    end);
end;

procedure TOBDDTCs.ReadPendingAsync;
begin
  DispatchAsync(dkPending,
    function: TArray<TOBDDtcEntry>
    begin
      Result := ReadOBDMode(OBD_MODE_PENDING_DTCS, dkPending);
    end);
end;

procedure TOBDDTCs.ReadPermanentAsync;
begin
  DispatchAsync(dkPermanent,
    function: TArray<TOBDDtcEntry>
    begin
      Result := ReadOBDMode(OBD_MODE_PERMANENT_DTCS, dkPermanent);
    end);
end;

procedure TOBDDTCs.ReadUDSAsync(AStatusMask: Byte);
var
  Mask: Byte;
begin
  Mask := AStatusMask;
  DispatchAsync(dkUDSStatusMask,
    function: TArray<TOBDDtcEntry>
    begin
      Result := ReadUDSStatusMask(Mask);
    end);
end;

procedure TOBDDTCs.Clear;
var
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDDTCs: Protocol not assigned');
  // OBD-II Mode 04. UDS uses 0x14 with a 3-byte group (0xFFFFFF =
  // all groups). The OBD Mode 04 is the simpler universal path.
  Resp := FProtocol.Request($04, nil);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'Clear DTCs negative: %s', [Resp.NRCText]);
  // Clear is a state-changing operation even though it returns
  // no DTC list — notify any host-bound observers so they can
  // refresh whatever they show next to "DTCs cleared".
  try TBindings.Notify(Self, ''); except end;
end;

procedure TOBDDTCs.FireDTCs(AKind: TOBDDtcKind;
  const AEntries: TArray<TOBDDtcEntry>);
var
  Self_: TOBDDTCs;
  Kind: TOBDDtcKind;
  Snap: TArray<TOBDDtcEntry>;
begin
  Self_ := Self; Kind := AKind; Snap := Copy(AEntries, 0, Length(AEntries));
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    // LiveBindings refresh runs alongside the event dispatch so
    // bound TLinkPropertyToField / TLinkObservableProperty pick
    // up the new DTC list even when the host has no OnDTCs
    // handler wired.
    try TBindings.Notify(Self_, ''); except end;
    if Assigned(FOnDTCs) then FOnDTCs(Self_, Kind, Snap);
  end
  else
    TThread.Queue(nil, procedure begin
      try TBindings.Notify(Self_, ''); except end;
      if Assigned(Self_.FOnDTCs) then Self_.FOnDTCs(Self_, Kind, Snap);
    end);
end;

procedure TOBDDTCs.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDDTCs; Code: TOBDErrorCode; Msg: string;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil, procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
