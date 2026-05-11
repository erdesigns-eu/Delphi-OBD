//------------------------------------------------------------------------------
//  OBD.Diagnostics.KWP.ReadDTC
//
//  TOBDKWPReadDTC — non-visual component for the KWP2000 read-DTC
//  services:
//
//    Service 0x18 — ReadDTCByStatus    (selective read by status
//                                       byte + DTC-group word)
//    Service 0x19 — ReadStatusOfDTC    (status query for one DTC)
//
//  Wire format per ISO 14230-3:1999 §6.6:
//
//    ReadDTCByStatus  request : 18 <statusByte> <group-hi> <group-lo>
//    ReadDTCByStatus  response: 58 <count> [<DTC-hi> <DTC-lo> <status>]*
//
//    ReadStatusOfDTC  request : 19 <DTC-hi> <DTC-lo>
//    ReadStatusOfDTC  response: 59 <DTC-hi> <DTC-lo> <status>
//
//  The component decodes the 16-bit raw DTC into J2012 P/C/B/U
//  format identically to the UDS ReadDTC component.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14230-3:1999 §6.6 (Read fault information services)
//    - ISO 15031-5 § 7 (J2012 DTC encoding)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.KWP.ReadDTC;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.KWP2000,
  OBD.Protocol;

const
  /// <summary>"All DTCs" group code (universally accepted).</summary>
  KWP_DTC_GROUP_ALL = $FFFF;

  /// <summary>Status byte mask — every fault that has occurred at
  /// least once.</summary>
  KWP_DTC_STATUS_ALL = $FF;

type
  /// <summary>
  ///   One decoded KWP DTC entry.
  /// </summary>
  TOBDKWPDtcEntry = record
    /// <summary>5-character J2012 DTC code (e.g. P0420).</summary>
    Code: string;
    /// <summary>KWP DTC-status byte (vendor-defined bit map).</summary>
    Status: Byte;
    /// <summary>Raw 2-byte DTC + 1-byte status as returned by the
    /// ECU.</summary>
    Raw: TBytes;
  end;

  /// <summary>Fires after a successful DTC sweep. Main thread.</summary>
  TOBDKWPReadDTCEvent = procedure(Sender: TObject;
    const AEntries: TArray<TOBDKWPDtcEntry>) of object;

  /// <summary>
  ///   KWP2000 ReadDTC component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, assign <c>Protocol</c>, call
  ///   <see cref="ReadByStatus"/> for a bulk sweep or
  ///   <see cref="ReadStatusOf"/> for a single-DTC status query.
  /// </remarks>
  TOBDKWPReadDTC = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnRead: TOBDKWPReadDTCEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoReadByStatus(AStatus: Byte;
      AGroup: Word): TArray<TOBDKWPDtcEntry>;
    function DoReadStatusOf(ADTC: Word): TOBDKWPDtcEntry;
    procedure FireRead(const AEntries: TArray<TOBDKWPDtcEntry>);
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
    ///   Decodes a 16-bit raw DTC into the J2012 5-character form.
    /// </summary>
    /// <param name="AHi">High DTC byte.</param>
    /// <param name="ALo">Low DTC byte.</param>
    /// <returns>Decoded DTC string.</returns>
    class function DecodeJ2012(AHi: Byte; ALo: Byte): string; static;

    /// <summary>
    ///   Service 0x18 — ReadDTCByStatus.
    /// </summary>
    /// <param name="AStatus">Status-byte filter (e.g.
    /// <c>KWP_DTC_STATUS_ALL</c>).</param>
    /// <param name="AGroup">DTC-group word (e.g.
    /// <c>KWP_DTC_GROUP_ALL</c>).</param>
    /// <returns>Decoded DTC entries.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or truncated response.
    /// </exception>
    function ReadByStatus(AStatus: Byte = KWP_DTC_STATUS_ALL;
      AGroup: Word = KWP_DTC_GROUP_ALL): TArray<TOBDKWPDtcEntry>;

    /// <summary>
    ///   Service 0x19 — ReadStatusOfDTC.
    /// </summary>
    /// <param name="ADTC">2-byte DTC value to query.</param>
    /// <returns>Decoded entry with the current status byte.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response, or the DTC
    ///   echo did not match.
    /// </exception>
    function ReadStatusOf(ADTC: Word): TOBDKWPDtcEntry;

    /// <summary>Non-blocking <see cref="ReadByStatus"/>.</summary>
    /// <param name="AStatus">Status-byte filter.</param>
    /// <param name="AGroup">DTC-group word.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadByStatusAsync(AStatus: Byte = KWP_DTC_STATUS_ALL;
      AGroup: Word = KWP_DTC_GROUP_ALL);
  published
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires on success. Main thread.</summary>
    property OnRead: TOBDKWPReadDTCEvent read FOnRead write FOnRead;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDKWPReadDTC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDKWPReadDTC.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDKWPReadDTC.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDKWPReadDTC.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDKWPReadDTC.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDKWPReadDTC: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDKWPReadDTC.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

class function TOBDKWPReadDTC.DecodeJ2012(AHi: Byte; ALo: Byte): string;
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

function TOBDKWPReadDTC.DoReadByStatus(AStatus: Byte;
  AGroup: Word): TArray<TOBDKWPDtcEntry>;
var
  Req: TBytes;
  Resp: TOBDResponse;
  Off: Integer;
  Acc: TList<TOBDKWPDtcEntry>;
  E: TOBDKWPDtcEntry;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWPReadDTC: Protocol not assigned');
  SetLength(Req, 3);
  Req[0] := AStatus;
  Req[1] := Byte((AGroup shr 8) and $FF);
  Req[2] := Byte(AGroup and $FF);

  Resp := FProtocol.Request(KWP_SID_ReadDTCByStatus, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDTCByStatus negative: %s', [Resp.NRCText]);
  if Length(Resp.Data) < 1 then
    raise EOBDProtocolErr.Create(
      'ReadDTCByStatus: response missing count byte');

  // Response: <count> [<DTC-hi> <DTC-lo> <status>]*
  Acc := TList<TOBDKWPDtcEntry>.Create;
  try
    Off := 1;
    while Off + 3 <= Length(Resp.Data) do
    begin
      E := Default(TOBDKWPDtcEntry);
      E.Code := DecodeJ2012(Resp.Data[Off], Resp.Data[Off + 1]);
      E.Status := Resp.Data[Off + 2];
      E.Raw := Copy(Resp.Data, Off, 3);
      Acc.Add(E);
      Inc(Off, 3);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDKWPReadDTC.DoReadStatusOf(ADTC: Word): TOBDKWPDtcEntry;
var
  Req: TBytes;
  Resp: TOBDResponse;
  EchoDTC: Word;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWPReadDTC: Protocol not assigned');
  SetLength(Req, 2);
  Req[0] := Byte((ADTC shr 8) and $FF);
  Req[1] := Byte(ADTC and $FF);
  Resp := FProtocol.Request(KWP_SID_ReadStatusOfDTC, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadStatusOfDTC (0x%.4x) negative: %s', [ADTC, Resp.NRCText]);
  if Length(Resp.Data) < 3 then
    raise EOBDProtocolErr.CreateFmt(
      'ReadStatusOfDTC (0x%.4x): short response', [ADTC]);

  EchoDTC := (Word(Resp.Data[0]) shl 8) or Word(Resp.Data[1]);
  if EchoDTC <> ADTC then
    raise EOBDProtocolErr.CreateFmt(
      'ReadStatusOfDTC echo mismatch: requested 0x%.4x, got 0x%.4x',
      [ADTC, EchoDTC]);

  Result := Default(TOBDKWPDtcEntry);
  Result.Code := DecodeJ2012(Resp.Data[0], Resp.Data[1]);
  Result.Status := Resp.Data[2];
  Result.Raw := Copy(Resp.Data, 0, 3);
end;

function TOBDKWPReadDTC.ReadByStatus(AStatus: Byte;
  AGroup: Word): TArray<TOBDKWPDtcEntry>;
begin
  Result := DoReadByStatus(AStatus, AGroup);
  FireRead(Result);
end;

function TOBDKWPReadDTC.ReadStatusOf(ADTC: Word): TOBDKWPDtcEntry;
var
  One: TArray<TOBDKWPDtcEntry>;
begin
  Result := DoReadStatusOf(ADTC);
  SetLength(One, 1);
  One[0] := Result;
  FireRead(One);
end;

procedure TOBDKWPReadDTC.ReadByStatusAsync(AStatus: Byte;
  AGroup: Word);
var
  Self_: TOBDKWPReadDTC;
  S: Byte;
  G: Word;
begin
  GuardSingleAsync;
  Self_ := Self;
  S := AStatus;
  G := AGroup;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadByStatus(S, G);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDKWPReadDTC.FireRead(const AEntries: TArray<TOBDKWPDtcEntry>);
var
  Self_: TOBDKWPReadDTC;
  Snap: TArray<TOBDKWPDtcEntry>;
begin
  if not Assigned(FOnRead) then
    Exit;
  Self_ := Self;
  Snap := Copy(AEntries, 0, Length(AEntries));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRead(Self_, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnRead) then
          Self_.FOnRead(Self_, Snap);
      end);
end;

procedure TOBDKWPReadDTC.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDKWPReadDTC;
  Code: TOBDErrorCode;
  Msg: string;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
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
