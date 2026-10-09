// ------------------------------------------------------------------------------
// ERD.UDS.WriteDID
//
// TOBDUDSWriteDID — non-visual component for ISO 14229-1
// WriteDataByIdentifier (SID 0x2E). Distinct from
// WriteMemoryByAddress (SID 0x3D, covered by TOBDUDSWriteMemory):
// WriteDataByIdentifier targets a 16-bit DID and writes raw or
// catalogue-shaped bytes. The DID-IO surface (read + write via
// TOBDDataIdentifierIO) covers the same wire path; this component
// exists as a focused single-purpose helper so coding orchestrators
// can wire one component per service when that fits their design.
//
// Wire format per ISO 14229-1 §11.6:
//
// Request : 2E <DID-hi> <DID-lo> <data...>
// Response: 6E <DID-hi> <DID-lo>
//
// AutoExecute = False default — every Write raises EOBDConfig
// before any wire access until the host explicitly opts in.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// References  :
// - ISO 14229-1:2020 § 11.6 (WriteDataByIdentifier)
//
// History     :
// 2026-05-11  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.UDS.WriteDID;

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
  ERD.Types,
  ERD.Protocol.Types,
  ERD.Protocol.UDS,
  ERD.Protocol;

const
  /// <summary>UDS WriteDataByIdentifier service identifier.</summary>
  UDS_SID_WriteDataByIdentifier = $2E;

type
  /// <summary>
  /// Fired after a successful write of <c>ADID</c>.
  /// </summary>
  /// <remarks>
  /// Always fires on the main thread, even when triggered from
  /// <see cref="TOBDUDSWriteDID.WriteAsync"/>.
  /// </remarks>
  TOBDUDSWriteDIDEvent = procedure(Sender: TObject; ADID: Word) of object;

  /// <summary>
  /// Single-purpose UDS WriteDataByIdentifier (SID 0x2E) component.
  /// </summary>
  /// <remarks>
  /// Drop the component on a form, assign <c>Protocol</c> to a
  /// connected <see cref="TOBDProtocol"/>, set
  /// <c>AutoExecute := True</c> once the host has consented, then
  /// call <see cref="Write"/> (sync) or <see cref="WriteAsync"/>
  /// (non-blocking).
  ///
  /// The component validates the response: the ECU must echo the
  /// 16-bit DID in the positive response; a missing or mismatched
  /// echo raises <c>EOBDProtocolErr</c>.
  /// </remarks>
  TOBDUDSWriteDID = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FOwnedTask: TOBDOwnedTask;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnWrite: TOBDUDSWriteDIDEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoWrite(ADID: Word; const AData: TBytes);
    procedure FireWrite(ADID: Word);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner (standard VCL pattern).</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Cancels in-flight async work and frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    /// Writes <c>AData</c> to <c>ADID</c> synchronously.
    /// </summary>
    /// <param name="ADID">16-bit Data Identifier per ISO 14229-1
    /// §10.2.</param>
    /// <param name="AData">Payload bytes to write. Must not be
    /// empty.</param>
    /// <remarks>
    /// Blocks the caller until the ECU response arrives or the
    /// protocol times out. From GUI code prefer
    /// <see cref="WriteAsync"/>.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    /// <c>Protocol</c> is not assigned; or <c>AutoExecute</c> is
    /// <c>False</c>; or <c>AData</c> is empty.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    /// ECU returned a negative response, a truncated positive
    /// response, or a DID echo that does not match
    /// <c>ADID</c>.
    /// </exception>
    procedure Write(ADID: Word; const AData: TBytes);

    /// <summary>
    /// Writes <c>AData</c> to <c>ADID</c> without blocking.
    /// </summary>
    /// <param name="ADID">16-bit Data Identifier.</param>
    /// <param name="AData">Payload bytes (deep-copied for the
    /// worker thread).</param>
    /// <remarks>
    /// Spawns a worker thread; reports completion via
    /// <c>OnWrite</c> or failure via <c>OnError</c> on the main
    /// thread. Only one <c>WriteAsync</c> may be in flight at a
    /// time — overlapping calls raise <c>EOBDConfig</c> from
    /// the calling thread before the worker is started.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    /// Another async write is already in flight.
    /// </exception>
    procedure WriteAsync(ADID: Word; const AData: TBytes);
  published
    /// <summary>Protocol stack to write through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>
    /// Safety gate. Default <c>False</c>.
    /// </summary>
    /// <remarks>
    /// Every <c>Write</c> raises <c>EOBDConfig</c> while this is
    /// <c>False</c>. The host flips it to <c>True</c> once the
    /// operator has explicitly consented to the write.
    /// </remarks>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;

    /// <summary>Fires on success. Main thread.</summary>
    property OnWrite: TOBDUDSWriteDIDEvent read FOnWrite write FOnWrite;

    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSWriteDID.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FOwnedTask := TOBDOwnedTask.Create;
end;

destructor TOBDUDSWriteDID.Destroy;
begin
  if FOwnedTask <> nil then
    FOwnedTask.Cancel;
  FreeAndNil(FOwnedTask);
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSWriteDID.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSWriteDID.Notification(AComponent: TComponent;
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

procedure TOBDUDSWriteDID.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('Async start requires the main thread');
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSWriteDID: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSWriteDID.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSWriteDID.DoWrite(ADID: Word; const AData: TBytes);
var
  Body: TBytes;
  Resp: TOBDResponse;
  EchoDID: Word;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSWriteDID: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create
      ('TOBDUDSWriteDID: AutoExecute is False — set it before writing');
  if Length(AData) = 0 then
    raise EOBDConfig.Create('TOBDUDSWriteDID: empty data');

  SetLength(Body, 2 + Length(AData));
  Body[0] := Byte((ADID shr 8) and $FF);
  Body[1] := Byte(ADID and $FF);
  Move(AData[0], Body[2], Length(AData));

  Resp := FProtocol.Request(UDS_SID_WriteDataByIdentifier, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('WriteDataByIdentifier 0x%.4x negative: %s',
      [ADID, Resp.NRCText]);
  if Length(Resp.Data) < 2 then
    raise EOBDProtocolErr.CreateFmt
      ('WriteDataByIdentifier 0x%.4x: response too short', [ADID]);
  EchoDID := (Word(Resp.Data[0]) shl 8) or Word(Resp.Data[1]);
  if EchoDID <> ADID then
    raise EOBDProtocolErr.CreateFmt
      ('WriteDataByIdentifier echo mismatch: requested 0x%.4x, got 0x%.4x',
      [ADID, EchoDID]);
end;

procedure TOBDUDSWriteDID.Write(ADID: Word; const AData: TBytes);
begin
  DoWrite(ADID, AData);
  FireWrite(ADID);
end;

procedure TOBDUDSWriteDID.WriteAsync(ADID: Word; const AData: TBytes);
var
  Self_: TOBDUDSWriteDID;
  DIDValue: Word;
  Data: TBytes;
begin
  GuardSingleAsync;
  try
    Self_ := Self;
    DIDValue := ADID;
    Data := Copy(AData, 0, Length(AData));
    FOwnedTask.Start(
      procedure
      begin
        try
          try
            Self_.DoWrite(DIDValue, Data);
            Self_.FireWrite(DIDValue);
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

procedure TOBDUDSWriteDID.FireWrite(ADID: Word);
var
  Self_: TOBDUDSWriteDID;
  DIDValue: Word;
begin
  if not Assigned(FOnWrite) then
    Exit;
  Self_ := Self;
  DIDValue := ADID;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnWrite(Self_, DIDValue)
  else
    FOwnedTask.Post(
      procedure
      begin
        if Assigned(Self_.FOnWrite) then
          Self_.FOnWrite(Self_, DIDValue);
      end);
end;

procedure TOBDUDSWriteDID.FireError(ACode: TOBDErrorCode;
const AMessage: string);
var
  Self_: TOBDUDSWriteDID;
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
