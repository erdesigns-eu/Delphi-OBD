//------------------------------------------------------------------------------
//  OBD.Coding.Flasher
//
//  TOBDFlasher — non-visual component that drives the ISO 14229-1
//  data-transfer trio:
//
//    - 0x34 RequestDownload    (negotiate target memory + size)
//    - 0x36 TransferData       (chunked image upload, BSC-counted)
//    - 0x37 RequestTransferExit (finalize)
//
//  Writing a new firmware image to an ECU is the most-destructive
//  action in this entire package. The component therefore:
//
//    - defaults <c>AutoExecute</c> to <c>False</c>; <c>Flash</c>
//      raises <c>EOBDConfig</c> until the host explicitly opts in.
//    - fires <c>OnBeforeFlash</c> on the main thread with a
//      <c>Cancel: Boolean</c> out-parameter so a UI can pop a
//      "are you absolutely sure" confirmation.
//    - fires <c>OnProgress</c> per-chunk with the BSC, byte
//      offset, and total size — drives a progress bar without
//      polling.
//    - guards single-in-flight via <c>GuardSingleAsync</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 § 14.2 (RequestDownload)
//    - ISO 14229-1:2020 § 14.5 (TransferData)
//    - ISO 14229-1:2020 § 14.6 (RequestTransferExit)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.Flasher;

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
  /// <summary>Argument record for <c>OnBeforeFlash</c>.</summary>
  TOBDFlasherBeforeEvent = procedure(Sender: TObject;
    AAddress: UInt64; ASize: UInt32;
    var ACancel: Boolean) of object;

  /// <summary>Argument record for <c>OnFlashComplete</c>.</summary>
  TOBDFlasherCompleteEvent = procedure(Sender: TObject;
    AAddress: UInt64; ASize: UInt32) of object;

  /// <summary>
  ///   ISO 14229-1 firmware-flash component.
  /// </summary>
  TOBDFlasher = class(TComponent)
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
    FOnBeforeFlash: TOBDFlasherBeforeEvent;
    FOnFlashComplete: TOBDFlasherCompleteEvent;
    FOnError: TOBDConnectionErrorEvent;
    FOnProgress: TOBDProgressEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function RequestWithPending(ASid: Byte; const ABody: TBytes;
      const AContext: string): TOBDResponse;
    function RequestDownload(AAddress: UInt64; ASize: UInt32): UInt32;
    procedure TransferChunks(AAddress: UInt64; const AImage: TBytes;
      AMaxBlock: UInt32);
    procedure RequestTransferExit;
    procedure DoFlash(AAddress: UInt64; const AImage: TBytes);
    function FireBeforeFlash(AAddress: UInt64; ASize: UInt32): Boolean;
    procedure FireComplete(AAddress: UInt64; ASize: UInt32);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure FireProgress(AIndex, ACount: Cardinal;
      const AName, ADetail: string);
    procedure SetProtocol(AValue: TOBDProtocol);
    function EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Flashes <c>AImage</c> into ECU memory starting at
    ///   <c>AAddress</c>. Synchronous.
    /// </summary>
    /// <param name="AAddress">Target memory address.</param>
    /// <param name="AImage">Firmware bytes.</param>
    /// <exception cref="EOBDConfig"><c>AutoExecute = False</c>,
    /// <c>OnBeforeFlash</c> cancelled, or zero-length image.</exception>
    /// <exception cref="EOBDProtocolErr">Negative response from
    /// any of the three sub-services.</exception>
    procedure Flash(AAddress: UInt64; const AImage: TBytes);
    /// <summary>Non-blocking <see cref="Flash"/>.</summary>
    procedure FlashAsync(AAddress: UInt64; const AImage: TBytes);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    /// <summary>Number of bytes for the memoryAddress field.
    /// Default 4 (32-bit address).</summary>
    property AddressFormatBytes: Byte read FAddressFormatBytes
      write FAddressFormatBytes default 4;
    /// <summary>Number of bytes for the memorySize field.
    /// Default 4 (32-bit size).</summary>
    property LengthFormatBytes: Byte read FLengthFormatBytes
      write FLengthFormatBytes default 4;
    /// <summary>Compression / encryption byte (high nibble =
    /// compression, low nibble = encryption). Default 0 (none).</summary>
    property DataFormatIdentifier: Byte read FDataFormatIdentifier
      write FDataFormatIdentifier default 0;
    /// <summary>Maximum number of times a request is repeated when
    /// the ECU answers with NRC 0x78 (responsePending). ISO 14229-1
    /// §7.5 lets the tester either wait or retransmit; this
    /// component retransmits. Default 10.</summary>
    property MaxPendingRetries: Integer read FMaxPendingRetries
      write FMaxPendingRetries default 10;
    /// <summary>Sleep between pending-retransmissions, in ms.
    /// Default 50.</summary>
    property PendingDelayMs: Cardinal read FPendingDelayMs
      write FPendingDelayMs default 50;
    /// <summary>Maximum number of times a single TransferData chunk
    /// is re-sent on a non-pending negative response. Default 3.
    /// 0 disables retry — the first failure aborts the flash.</summary>
    property MaxChunkRetries: Integer read FMaxChunkRetries
      write FMaxChunkRetries default 3;
    /// <summary>Sleep between chunk retries, in ms. Default 20.</summary>
    property ChunkRetryDelayMs: Cardinal read FChunkRetryDelayMs
      write FChunkRetryDelayMs default 20;
    /// <summary>Fires before any wire access (main thread).</summary>
    property OnBeforeFlash: TOBDFlasherBeforeEvent read FOnBeforeFlash
      write FOnBeforeFlash;
    /// <summary>Fires on a successful completion.</summary>
    property OnFlashComplete: TOBDFlasherCompleteEvent
      read FOnFlashComplete write FOnFlashComplete;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
    /// <summary>Per-chunk progress (main thread).</summary>
    property OnProgress: TOBDProgressEvent read FOnProgress
      write FOnProgress;
  end;

implementation

constructor TOBDFlasher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FAutoExecute := False;
  FAddressFormatBytes := 4;
  FLengthFormatBytes := 4;
  FDataFormatIdentifier := 0;
  FMaxPendingRetries := 10;
  FPendingDelayMs := 50;
  FMaxChunkRetries := 3;
  FChunkRetryDelayMs := 20;
end;

destructor TOBDFlasher.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDFlasher.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDFlasher.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDFlasher.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDFlasher: flash already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDFlasher.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDFlasher.RequestWithPending(ASid: Byte;
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

function TOBDFlasher.EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
var
  I: Integer;
begin
  if (ABytes = 0) or (ABytes > 8) then
    raise EOBDConfig.CreateFmt('TOBDFlasher: bad format byte count %d', [ABytes]);
  SetLength(Result, ABytes);
  for I := 0 to ABytes - 1 do
    Result[I] := Byte((AValue shr (8 * (ABytes - 1 - I))) and $FF);
end;

function TOBDFlasher.RequestDownload(AAddress: UInt64;
  ASize: UInt32): UInt32;
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
  // addressAndLengthFormatIdentifier: high nibble = length-of-memorySize,
  // low nibble = length-of-memoryAddress.
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

  // Response: lengthFormatIdentifier (high nibble) + maxNumberOfBlockLength.
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
  // ISO 14229-1 maxNumberOfBlockLength includes the SID + BSC bytes
  // (overhead = 2 bytes). Subtract before chunking.
  if MaxBlock < 3 then
    raise EOBDProtocolErr.CreateFmt(
      'RequestDownload: maxNumberOfBlockLength %d too small', [MaxBlock]);
  Result := UInt32(MaxBlock - 2);
end;

procedure TOBDFlasher.TransferChunks(AAddress: UInt64;
  const AImage: TBytes; AMaxBlock: UInt32);
var
  Off: Integer;
  ChunkSize: Integer;
  Bsc: Byte;
  Body: TBytes;
  Resp: TOBDResponse;
  Total: Integer;
  ChunkIdx, ChunkCount: Cardinal;
  ChunkAttempt: Integer;
begin
  Total := Length(AImage);
  if Total = 0 then Exit;
  Bsc := 1;
  Off := 0;
  ChunkCount := Cardinal((Total + Integer(AMaxBlock) - 1) div Integer(AMaxBlock));
  ChunkIdx := 0;
  while Off < Total do
  begin
    ChunkSize := Total - Off;
    if ChunkSize > Integer(AMaxBlock) then ChunkSize := Integer(AMaxBlock);
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
      FireProgress(ChunkIdx, ChunkCount, 'TransferData',
        Format('BSC=%d retry %d/%d (%s)',
          [Bsc, ChunkAttempt, FMaxChunkRetries, Resp.NRCText]));
      Sleep(FChunkRetryDelayMs);
    end;
    // Response echoes BSC; refuse on mismatch.
    if (Length(Resp.Data) >= 1) and (Resp.Data[0] <> Bsc) then
      raise EOBDProtocolErr.CreateFmt(
        'TransferData: BSC echo mismatch (sent %d, got %d)',
        [Bsc, Resp.Data[0]]);

    Inc(Off, ChunkSize);
    Inc(ChunkIdx);
    FireProgress(ChunkIdx, ChunkCount, 'TransferData',
      Format('BSC=%d offset=0x%x', [Bsc, Off]));
    Inc(Bsc);
    if Bsc = 0 then Bsc := 1; // BSC wraps from 0xFF -> 0x01 per spec
  end;
end;

procedure TOBDFlasher.RequestTransferExit;
var
  Resp: TOBDResponse;
begin
  Resp := RequestWithPending(UDS_SID_RequestTransferExit, nil,
    'RequestTransferExit');
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'RequestTransferExit negative: %s', [Resp.NRCText]);
end;

procedure TOBDFlasher.DoFlash(AAddress: UInt64; const AImage: TBytes);
var
  MaxBlock: UInt32;
  Size: UInt32;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDFlasher: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDFlasher: AutoExecute is False — set it before flashing');
  if Length(AImage) = 0 then
    raise EOBDConfig.Create('TOBDFlasher: empty image');
  Size := UInt32(Length(AImage));
  if not FireBeforeFlash(AAddress, Size) then
    raise EOBDConfig.Create('TOBDFlasher: cancelled by OnBeforeFlash handler');

  FireProgress(1, 3, 'Flash', 'requestDownload');
  MaxBlock := RequestDownload(AAddress, Size);

  FireProgress(2, 3, 'Flash', Format('transferData (max %d B / chunk)',
    [MaxBlock]));
  TransferChunks(AAddress, AImage, MaxBlock);

  FireProgress(3, 3, 'Flash', 'requestTransferExit');
  RequestTransferExit;

  FireComplete(AAddress, Size);
end;

procedure TOBDFlasher.Flash(AAddress: UInt64; const AImage: TBytes);
begin
  DoFlash(AAddress, AImage);
end;

procedure TOBDFlasher.FlashAsync(AAddress: UInt64; const AImage: TBytes);
var
  Self_: TOBDFlasher;
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
          Self_.DoFlash(Addr, Img);
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

function TOBDFlasher.FireBeforeFlash(AAddress: UInt64;
  ASize: UInt32): Boolean;
var
  Cancel: Boolean;
  Self_: TOBDFlasher;
  Addr: UInt64;
  Sz: UInt32;
  Local: Boolean;
begin
  if not Assigned(FOnBeforeFlash) then Exit(True);
  Self_ := Self; Addr := AAddress; Sz := ASize; Cancel := False;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnBeforeFlash(Self_, Addr, Sz, Cancel)
  else
  begin
    Local := False;
    TThread.Synchronize(nil, procedure
      var C: Boolean;
      begin
        C := False;
        if Assigned(Self_.FOnBeforeFlash) then
          Self_.FOnBeforeFlash(Self_, Addr, Sz, C);
        Local := C;
      end);
    Cancel := Local;
  end;
  Result := not Cancel;
end;

procedure TOBDFlasher.FireComplete(AAddress: UInt64; ASize: UInt32);
var
  Self_: TOBDFlasher;
  Addr: UInt64;
  Sz: UInt32;
begin
  if not Assigned(FOnFlashComplete) then Exit;
  Self_ := Self; Addr := AAddress; Sz := ASize;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnFlashComplete(Self_, Addr, Sz)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnFlashComplete) then
        Self_.FOnFlashComplete(Self_, Addr, Sz);
    end);
end;

procedure TOBDFlasher.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDFlasher; Code: TOBDErrorCode; Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
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

procedure TOBDFlasher.FireProgress(AIndex, ACount: Cardinal;
  const AName, ADetail: string);
var
  Self_: TOBDFlasher;
  Step: TOBDProgressStep;
begin
  if not Assigned(FOnProgress) then Exit;
  Self_ := Self;
  Step := TOBDProgressStep.MakeStep(AIndex, ACount, AName, ADetail);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self_, Step)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnProgress) then Self_.FOnProgress(Self_, Step);
    end);
end;

end.
