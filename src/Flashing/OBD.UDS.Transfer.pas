//------------------------------------------------------------------------------
//  OBD.UDS.Transfer
//
//  TOBDUDSTransfer — full ISO 14229-1 data-transfer state machine.
//  This is the production transfer engine that the Phase 9 flash
//  pipeline drives; the Phase 6 TOBDFlasher (in OBD.Coding.Flasher)
//  remains as a simple one-shot helper and is internally a thin
//  wrapper over the same UDS sub-services.
//
//  SAFETY — BRICK RISK ----------------------------------------------------
//  This unit talks to RequestDownload (0x34), TransferData (0x36)
//  and RequestTransferExit (0x37). Misuse can BRICK the ECU. Read
//  docs/flashing-safety.md before integrating. Every entry-point
//  on this unit defaults <c>AutoExecute = False</c>; nothing
//  reaches the wire until the host explicitly opts in.
//  ------------------------------------------------------------------------
//
//  v9a feature set:
//
//    - State-machine driven transfer with the full ISO 14229-1
//      §14 lifecycle: Idle → RequestingDownload → Transferring →
//      RequestingExit → Completed (or → Aborted on any failure).
//    - Chunked TransferData with BSC counter (1..255 then wraps to
//      1) and the per-chunk retry budget the user pulled forward
//      in the Close-out.
//    - NRC 0x78 (responsePending) auto-retransmit with budget.
//    - Resumable: hosts pass an optional <see cref="TOBDFlashCheckpoint"/>
//      shape (offset + BSC) to resume an interrupted transfer
//      without re-doing the bytes already accepted by the ECU. The
//      checkpoint persistence layer ships in 9b; this unit just
//      consumes the shape.
//    - Per-chunk OnProgress with the BSC + byte offset + total.
//    - OnStateChange so a host UI can render the current phase.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 § 14.2 (RequestDownload)
//    - ISO 14229-1:2020 § 14.5 (TransferData)
//    - ISO 14229-1:2020 § 14.6 (RequestTransferExit)
//    - ISO 14229-1:2020 § 7.5  (Pending response NRC 0x78)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.UDS.Transfer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol;

type
  /// <summary>State of the transfer state machine.</summary>
  TOBDTransferState = (
    tsIdle,
    tsRequestingDownload,
    tsTransferring,
    tsRequestingExit,
    tsCompleted,
    tsAborted
  );

  /// <summary>Snapshot of the transfer cursor — used both for
  /// resume and for in-flight reporting.</summary>
  TOBDTransferCursor = record
    /// <summary>Bytes already accepted by the ECU.</summary>
    BytesSent: UInt32;
    /// <summary>Total bytes the original RequestDownload negotiated.</summary>
    TotalBytes: UInt32;
    /// <summary>Next BSC to send (1..255).</summary>
    NextBSC: Byte;
    /// <summary>Maximum data bytes per chunk (MAX_BLOCK − 2 for
    /// SID + BSC overhead).</summary>
    MaxChunkBytes: UInt32;
    /// <summary>Target memory address.</summary>
    Address: UInt64;
  end;

  /// <summary>Fires on every state change.</summary>
  TOBDTransferStateEvent = procedure(Sender: TObject;
    AOld, ANew: TOBDTransferState) of object;

  /// <summary>Fires per chunk with the current cursor.</summary>
  TOBDTransferProgressEvent = procedure(Sender: TObject;
    const ACursor: TOBDTransferCursor) of object;

  /// <summary>
  ///   ISO 14229-1 transfer state machine. Owns the wire sequence
  ///   for one download (tester → ECU); resumable.
  /// </summary>
  TOBDUDSTransfer = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAddressFormatBytes: Byte;
    FLengthFormatBytes: Byte;
    FDataFormatIdentifier: Byte;
    FMaxPendingRetries: Integer;
    FPendingDelayMs: Cardinal;
    FMaxChunkRetries: Integer;
    FChunkRetryDelayMs: Cardinal;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FState: TOBDTransferState;
    FCursor: TOBDTransferCursor;
    FCancel: Boolean;
    FOnStateChange: TOBDTransferStateEvent;
    FOnProgress: TOBDTransferProgressEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure SetState(AValue: TOBDTransferState);
    function RequestWithPending(ASid: Byte; const ABody: TBytes;
      const AContext: string): TOBDResponse;
    procedure DoRequestDownload(AAddress: UInt64; ASize: UInt32);
    procedure DoTransferLoop(const AImage: TBytes; AStartOffset: UInt32;
      AStartBSC: Byte);
    procedure DoRequestTransferExit;
    function EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
    procedure FireStateChange(AOld, ANew: TOBDTransferState);
    procedure FireProgress(const ACursor: TOBDTransferCursor);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Runs a fresh transfer: 0x34 → loop 0x36 → 0x37.
    /// Synchronous; blocks until completion or abort.</summary>
    /// <param name="AAddress">Target memory address.</param>
    /// <param name="AImage">Firmware bytes.</param>
    /// <exception cref="EOBDConfig"><c>AutoExecute = False</c> or
    /// empty image.</exception>
    /// <exception cref="EOBDProtocolErr">Negative response or
    /// retry budget exhausted.</exception>
    procedure Run(AAddress: UInt64; const AImage: TBytes);

    /// <summary>Resumes a transfer from <c>ACursor</c>. The host
    /// supplies the same image bytes; we skip the prefix that the
    /// ECU already accepted.</summary>
    procedure Resume(const ACursor: TOBDTransferCursor;
      const AImage: TBytes);

    /// <summary>Non-blocking <see cref="Run"/>.</summary>
    procedure RunAsync(AAddress: UInt64; const AImage: TBytes);

    /// <summary>Requests cooperative cancel. The state machine
    /// stops at the next chunk boundary, advances to
    /// <c>tsAborted</c> and fires <c>OnError(oeIO, …)</c>.</summary>
    procedure Cancel;

    /// <summary>Current state (read-only outside the engine).</summary>
    property State: TOBDTransferState read FState;
    /// <summary>Live cursor snapshot.</summary>
    property Cursor: TOBDTransferCursor read FCursor;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate — Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    property AddressFormatBytes: Byte read FAddressFormatBytes
      write FAddressFormatBytes default 4;
    property LengthFormatBytes: Byte read FLengthFormatBytes
      write FLengthFormatBytes default 4;
    property DataFormatIdentifier: Byte read FDataFormatIdentifier
      write FDataFormatIdentifier default 0;
    property MaxPendingRetries: Integer read FMaxPendingRetries
      write FMaxPendingRetries default 10;
    property PendingDelayMs: Cardinal read FPendingDelayMs
      write FPendingDelayMs default 50;
    property MaxChunkRetries: Integer read FMaxChunkRetries
      write FMaxChunkRetries default 3;
    property ChunkRetryDelayMs: Cardinal read FChunkRetryDelayMs
      write FChunkRetryDelayMs default 20;
    property OnStateChange: TOBDTransferStateEvent read FOnStateChange
      write FOnStateChange;
    property OnProgress: TOBDTransferProgressEvent read FOnProgress
      write FOnProgress;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSTransfer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FAddressFormatBytes := 4;
  FLengthFormatBytes := 4;
  FMaxPendingRetries := 10;
  FPendingDelayMs := 50;
  FMaxChunkRetries := 3;
  FChunkRetryDelayMs := 20;
end;

destructor TOBDUDSTransfer.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSTransfer.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSTransfer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSTransfer.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSTransfer: already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDUDSTransfer.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDUDSTransfer.SetState(AValue: TOBDTransferState);
var
  Old: TOBDTransferState;
begin
  if FState = AValue then Exit;
  Old := FState;
  FState := AValue;
  FireStateChange(Old, AValue);
end;

procedure TOBDUDSTransfer.Cancel;
begin
  FCancel := True;
end;

function TOBDUDSTransfer.EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
var
  I: Integer;
begin
  if (ABytes = 0) or (ABytes > 8) then
    raise EOBDConfig.CreateFmt(
      'TOBDUDSTransfer: bad format byte count %d', [ABytes]);
  SetLength(Result, ABytes);
  for I := 0 to ABytes - 1 do
    Result[I] := Byte((AValue shr (8 * (ABytes - 1 - I))) and $FF);
end;

function TOBDUDSTransfer.RequestWithPending(ASid: Byte;
  const ABody: TBytes; const AContext: string): TOBDResponse;
var
  PendingTries: Integer;
begin
  PendingTries := 0;
  while True do
  begin
    Result := FProtocol.Request(ASid, ABody);
    if not Result.IsNegative then Exit;
    if Result.NRC <> UDS_NRC_ResponsePending then Exit;
    Inc(PendingTries);
    if PendingTries > FMaxPendingRetries then
      raise EOBDProtocolErr.CreateFmt(
        '%s: NRC 0x78 exceeded %d pending retries',
        [AContext, FMaxPendingRetries]);
    Sleep(FPendingDelayMs);
  end;
end;

procedure TOBDUDSTransfer.DoRequestDownload(AAddress: UInt64; ASize: UInt32);
var
  Body: TBytes;
  Resp: TOBDResponse;
  AddrBytes, LenBytes: TBytes;
  AddrAndLenFmt: Byte;
  LenLen: Byte;
  I: Integer;
  MaxBlock: UInt64;
begin
  AddrBytes := EncodeMSB(AAddress, FAddressFormatBytes);
  LenBytes  := EncodeMSB(ASize,    FLengthFormatBytes);
  AddrAndLenFmt := Byte((FLengthFormatBytes shl 4) or
                        (FAddressFormatBytes and $0F));
  SetLength(Body, 2 + Length(AddrBytes) + Length(LenBytes));
  Body[0] := FDataFormatIdentifier;
  Body[1] := AddrAndLenFmt;
  Move(AddrBytes[0], Body[2], Length(AddrBytes));
  Move(LenBytes[0],  Body[2 + Length(AddrBytes)], Length(LenBytes));

  Resp := RequestWithPending(UDS_SID_RequestDownload, Body,
    'RequestDownload');
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'RequestDownload negative: %s', [Resp.NRCText]);
  if Length(Resp.Data) < 1 then
    raise EOBDProtocolErr.Create('RequestDownload: empty response');
  LenLen := (Resp.Data[0] shr 4) and $0F;
  if (LenLen = 0) or (LenLen > 4) or
     (Length(Resp.Data) < 1 + Integer(LenLen)) then
    raise EOBDProtocolErr.Create(
      'RequestDownload: malformed maxNumberOfBlockLength');
  MaxBlock := 0;
  for I := 0 to LenLen - 1 do
    MaxBlock := (MaxBlock shl 8) or Resp.Data[1 + I];
  if MaxBlock < 3 then
    raise EOBDProtocolErr.CreateFmt(
      'RequestDownload: maxNumberOfBlockLength %d too small', [MaxBlock]);
  FCursor.MaxChunkBytes := UInt32(MaxBlock - 2);
end;

procedure TOBDUDSTransfer.DoTransferLoop(const AImage: TBytes;
  AStartOffset: UInt32; AStartBSC: Byte);
var
  Off, ChunkSize, Total, ChunkAttempt: Integer;
  Body: TBytes;
  Resp: TOBDResponse;
  Bsc: Byte;
begin
  Total := Length(AImage);
  if Total = 0 then Exit;
  Bsc := AStartBSC;
  if Bsc = 0 then Bsc := 1;
  Off := Integer(AStartOffset);
  if Off < 0 then Off := 0;

  while Off < Total do
  begin
    if FCancel then
      raise EOBDProtocolErr.Create('Transfer: cancelled by host');
    ChunkSize := Total - Off;
    if ChunkSize > Integer(FCursor.MaxChunkBytes) then
      ChunkSize := Integer(FCursor.MaxChunkBytes);
    SetLength(Body, 1 + ChunkSize);
    Body[0] := Bsc;
    Move(AImage[Off], Body[1], ChunkSize);

    ChunkAttempt := 0;
    while True do
    begin
      Resp := RequestWithPending(UDS_SID_TransferData, Body,
        Format('TransferData BSC %d', [Bsc]));
      if not Resp.IsNegative then Break;
      Inc(ChunkAttempt);
      if ChunkAttempt > FMaxChunkRetries then
        raise EOBDProtocolErr.CreateFmt(
          'TransferData BSC %d failed after %d retries: %s',
          [Bsc, FMaxChunkRetries, Resp.NRCText]);
      Sleep(FChunkRetryDelayMs);
    end;
    if (Length(Resp.Data) >= 1) and (Resp.Data[0] <> Bsc) then
      raise EOBDProtocolErr.CreateFmt(
        'TransferData: BSC echo mismatch (sent %d, got %d)',
        [Bsc, Resp.Data[0]]);

    Inc(Off, ChunkSize);
    FCursor.BytesSent := UInt32(Off);
    Inc(Bsc);
    if Bsc = 0 then Bsc := 1;
    FCursor.NextBSC := Bsc;
    FireProgress(FCursor);
  end;
end;

procedure TOBDUDSTransfer.DoRequestTransferExit;
var
  Resp: TOBDResponse;
begin
  Resp := RequestWithPending(UDS_SID_RequestTransferExit, nil,
    'RequestTransferExit');
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'RequestTransferExit negative: %s', [Resp.NRCText]);
end;

procedure TOBDUDSTransfer.Run(AAddress: UInt64; const AImage: TBytes);
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSTransfer: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDUDSTransfer: AutoExecute is False — set it explicitly');
  if Length(AImage) = 0 then
    raise EOBDConfig.Create('TOBDUDSTransfer: empty image');

  FCancel := False;
  FCursor := Default(TOBDTransferCursor);
  FCursor.Address := AAddress;
  FCursor.TotalBytes := UInt32(Length(AImage));
  FCursor.NextBSC := 1;

  try
    SetState(tsRequestingDownload);
    DoRequestDownload(AAddress, FCursor.TotalBytes);

    SetState(tsTransferring);
    DoTransferLoop(AImage, 0, 1);

    SetState(tsRequestingExit);
    DoRequestTransferExit;

    SetState(tsCompleted);
  except
    on E: Exception do
    begin
      SetState(tsAborted);
      FireError(oeIO, E.Message);
      raise;
    end;
  end;
end;

procedure TOBDUDSTransfer.Resume(const ACursor: TOBDTransferCursor;
  const AImage: TBytes);
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSTransfer: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDUDSTransfer: AutoExecute is False — set it explicitly');
  if Length(AImage) = 0 then
    raise EOBDConfig.Create('TOBDUDSTransfer.Resume: empty image');
  if ACursor.BytesSent >= ACursor.TotalBytes then
    raise EOBDConfig.Create('TOBDUDSTransfer.Resume: cursor already complete');
  if ACursor.MaxChunkBytes = 0 then
    raise EOBDConfig.Create(
      'TOBDUDSTransfer.Resume: cursor missing MaxChunkBytes');
  if Length(AImage) <> Integer(ACursor.TotalBytes) then
    raise EOBDConfig.CreateFmt(
      'TOBDUDSTransfer.Resume: image size %d mismatches cursor.TotalBytes %d',
      [Length(AImage), ACursor.TotalBytes]);

  FCancel := False;
  FCursor := ACursor;
  // Resume skips RequestDownload — the ECU is mid-transfer.
  try
    SetState(tsTransferring);
    DoTransferLoop(AImage, ACursor.BytesSent, ACursor.NextBSC);

    SetState(tsRequestingExit);
    DoRequestTransferExit;

    SetState(tsCompleted);
  except
    on E: Exception do
    begin
      SetState(tsAborted);
      FireError(oeIO, E.Message);
      raise;
    end;
  end;
end;

procedure TOBDUDSTransfer.RunAsync(AAddress: UInt64; const AImage: TBytes);
var
  Self_: TOBDUDSTransfer;
  Addr: UInt64;
  Img: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self; Addr := AAddress;
  Img := Copy(AImage, 0, Length(AImage));
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.Run(Addr, Img);
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSTransfer.FireStateChange(AOld, ANew: TOBDTransferState);
var
  Self_: TOBDUDSTransfer;
  O, N: TOBDTransferState;
begin
  if not Assigned(FOnStateChange) then Exit;
  Self_ := Self; O := AOld; N := ANew;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnStateChange(Self_, O, N)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnStateChange) then
        Self_.FOnStateChange(Self_, O, N);
    end);
end;

procedure TOBDUDSTransfer.FireProgress(const ACursor: TOBDTransferCursor);
var
  Self_: TOBDUDSTransfer;
  C: TOBDTransferCursor;
begin
  if not Assigned(FOnProgress) then Exit;
  Self_ := Self; C := ACursor;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self_, C)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnProgress) then Self_.FOnProgress(Self_, C);
    end);
end;

procedure TOBDUDSTransfer.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSTransfer; Code: TOBDErrorCode; Msg: string;
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
