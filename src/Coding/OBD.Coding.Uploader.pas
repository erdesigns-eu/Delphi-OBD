//------------------------------------------------------------------------------
//  OBD.Coding.Uploader
//
//  TOBDUploader — non-visual component that drives the
//  ECU → tester data-transfer trio:
//
//    - 0x35 RequestUpload      (negotiate source memory + size)
//    - 0x36 TransferData       (chunked image readback,
//                               BSC-counted, response carries data)
//    - 0x37 RequestTransferExit (finalize)
//
//  Used to dump flash partitions for backup, EEPROM extraction or
//  VBF / S19 generation. Reading is non-destructive but typically
//  requires a SecurityAccess unlock; that's the host's
//  responsibility.
//
//  Honest contract:
//
//    - Upload itself does not modify ECU state, so this component
//      does NOT default <c>AutoExecute</c> to <c>False</c>. A
//      client that wants confirmation flow can still listen on
//      <c>OnBeforeUpload</c>.
//    - Same NRC 0x78 pending-retry + per-chunk retry budget as
//      <c>TOBDFlasher</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 § 14.3 (RequestUpload)
//    - ISO 14229-1:2020 § 14.5 (TransferData)
//    - ISO 14229-1:2020 § 14.6 (RequestTransferExit)
//
//  History     :
//    2026-05-09  ERD  Phase 6 follow-up.
//------------------------------------------------------------------------------

unit OBD.Coding.Uploader;

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
  /// <summary>Argument record for <c>OnBeforeUpload</c>.</summary>
  TOBDUploaderBeforeEvent = procedure(Sender: TObject;
    AAddress: UInt64; ASize: UInt32; var ACancel: Boolean) of object;

  /// <summary>Argument record for <c>OnUploadComplete</c>.</summary>
  TOBDUploaderCompleteEvent = procedure(Sender: TObject;
    AAddress: UInt64; const AImage: TBytes) of object;

  /// <summary>UDS Service 0x35 RequestUpload component.</summary>
  TOBDUploader = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAddressFormatBytes: Byte;
    FLengthFormatBytes: Byte;
    FDataFormatIdentifier: Byte;
    FMaxPendingRetries: Integer;
    FPendingDelayMs: Cardinal;
    FMaxChunkRetries: Integer;
    FChunkRetryDelayMs: Cardinal;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnBeforeUpload: TOBDUploaderBeforeEvent;
    FOnUploadComplete: TOBDUploaderCompleteEvent;
    FOnError: TOBDConnectionErrorEvent;
    FOnProgress: TOBDProgressEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function RequestWithPending(ASid: Byte; const ABody: TBytes;
      const AContext: string): TOBDResponse;
    function RequestUpload(AAddress: UInt64; ASize: UInt32): UInt32;
    function ReceiveChunks(ASize: UInt32; AMaxBlock: UInt32): TBytes;
    procedure RequestTransferExit;
    function DoUpload(AAddress: UInt64; ASize: UInt32): TBytes;
    function FireBeforeUpload(AAddress: UInt64; ASize: UInt32): Boolean;
    procedure FireComplete(AAddress: UInt64; const AImage: TBytes);
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

    /// <summary>Reads <c>ASize</c> bytes from ECU memory starting
    /// at <c>AAddress</c>.</summary>
    function Upload(AAddress: UInt64; ASize: UInt32): TBytes;
    /// <summary>Non-blocking <see cref="Upload"/>.</summary>
    procedure UploadAsync(AAddress: UInt64; ASize: UInt32);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
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
    property OnBeforeUpload: TOBDUploaderBeforeEvent
      read FOnBeforeUpload write FOnBeforeUpload;
    property OnUploadComplete: TOBDUploaderCompleteEvent
      read FOnUploadComplete write FOnUploadComplete;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
    property OnProgress: TOBDProgressEvent read FOnProgress
      write FOnProgress;
  end;

implementation

constructor TOBDUploader.Create(AOwner: TComponent);
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

destructor TOBDUploader.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUploader.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDUploader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUploader.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUploader: upload already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDUploader.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDUploader.EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
var
  I: Integer;
begin
  if (ABytes = 0) or (ABytes > 8) then
    raise EOBDConfig.CreateFmt('TOBDUploader: bad format byte count %d', [ABytes]);
  SetLength(Result, ABytes);
  for I := 0 to ABytes - 1 do
    Result[I] := Byte((AValue shr (8 * (ABytes - 1 - I))) and $FF);
end;

function TOBDUploader.RequestWithPending(ASid: Byte;
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

function TOBDUploader.RequestUpload(AAddress: UInt64;
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
  AddrAndLenFmt := Byte((FLengthFormatBytes shl 4) or
                        (FAddressFormatBytes and $0F));
  SetLength(Body, 2 + Length(AddrBytes) + Length(LenBytes));
  Body[0] := FDataFormatIdentifier;
  Body[1] := AddrAndLenFmt;
  Move(AddrBytes[0], Body[2], Length(AddrBytes));
  Move(LenBytes[0],  Body[2 + Length(AddrBytes)], Length(LenBytes));

  Resp := RequestWithPending(UDS_SID_RequestUpload, Body, 'RequestUpload');
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('RequestUpload negative: %s',
      [Resp.NRCText]);

  if Length(Resp.Data) < 1 then
    raise EOBDProtocolErr.Create('RequestUpload: empty response');
  LenLen := (Resp.Data[0] shr 4) and $0F;
  if (LenLen = 0) or (LenLen > 4) or
     (Length(Resp.Data) < 1 + Integer(LenLen)) then
    raise EOBDProtocolErr.Create(
      'RequestUpload: malformed maxNumberOfBlockLength');
  MaxBlock := 0;
  for I := 0 to LenLen - 1 do
    MaxBlock := (MaxBlock shl 8) or Resp.Data[1 + I];
  // maxNumberOfBlockLength includes SID + BSC overhead (2 B).
  if MaxBlock < 3 then
    raise EOBDProtocolErr.CreateFmt(
      'RequestUpload: maxNumberOfBlockLength %d too small', [MaxBlock]);
  Result := UInt32(MaxBlock - 2);
end;

function TOBDUploader.ReceiveChunks(ASize, AMaxBlock: UInt32): TBytes;
var
  Off: Integer;
  Bsc: Byte;
  Body: TBytes;
  Resp: TOBDResponse;
  ChunkAttempt: Integer;
  ChunkIdx, ChunkCount: Cardinal;
  Take: Integer;
begin
  if ASize = 0 then Exit(nil);
  SetLength(Result, ASize);
  Bsc := 1;
  Off := 0;
  ChunkCount := Cardinal((Integer(ASize) + Integer(AMaxBlock) - 1)
    div Integer(AMaxBlock));
  ChunkIdx := 0;
  while Off < Integer(ASize) do
  begin
    SetLength(Body, 1);
    Body[0] := Bsc;

    ChunkAttempt := 0;
    while True do
    begin
      Resp := RequestWithPending(UDS_SID_TransferData, Body,
        Format('TransferData(upload) BSC %d', [Bsc]));
      if not Resp.IsNegative then Break;
      Inc(ChunkAttempt);
      if ChunkAttempt > FMaxChunkRetries then
        raise EOBDProtocolErr.CreateFmt(
          'TransferData(upload) BSC %d failed after %d retries: %s',
          [Bsc, FMaxChunkRetries, Resp.NRCText]);
      Sleep(FChunkRetryDelayMs);
    end;
    // Response: <BSC echo> <data...>
    if (Length(Resp.Data) < 1) or (Resp.Data[0] <> Bsc) then
      raise EOBDProtocolErr.CreateFmt(
        'TransferData(upload): BSC echo mismatch (sent %d)', [Bsc]);
    Take := Length(Resp.Data) - 1;
    if Take > Integer(ASize) - Off then
      Take := Integer(ASize) - Off;
    if Take > 0 then
      Move(Resp.Data[1], Result[Off], Take);
    Inc(Off, Take);
    Inc(ChunkIdx);
    FireProgress(ChunkIdx, ChunkCount, 'TransferData(upload)',
      Format('BSC=%d offset=0x%x', [Bsc, Off]));
    Inc(Bsc);
    if Bsc = 0 then Bsc := 1;
  end;
end;

procedure TOBDUploader.RequestTransferExit;
var
  Resp: TOBDResponse;
begin
  Resp := RequestWithPending(UDS_SID_RequestTransferExit, nil,
    'RequestTransferExit');
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'RequestTransferExit negative: %s', [Resp.NRCText]);
end;

function TOBDUploader.DoUpload(AAddress: UInt64; ASize: UInt32): TBytes;
var
  MaxBlock: UInt32;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUploader: Protocol not assigned');
  if ASize = 0 then
    raise EOBDConfig.Create('TOBDUploader: zero-size upload');
  if not FireBeforeUpload(AAddress, ASize) then
    raise EOBDConfig.Create(
      'TOBDUploader: cancelled by OnBeforeUpload handler');

  FireProgress(1, 3, 'Upload', 'requestUpload');
  MaxBlock := RequestUpload(AAddress, ASize);
  FireProgress(2, 3, 'Upload',
    Format('transferData (max %d B / chunk)', [MaxBlock]));
  Result := ReceiveChunks(ASize, MaxBlock);
  FireProgress(3, 3, 'Upload', 'requestTransferExit');
  RequestTransferExit;
  FireComplete(AAddress, Result);
end;

function TOBDUploader.Upload(AAddress: UInt64; ASize: UInt32): TBytes;
begin
  Result := DoUpload(AAddress, ASize);
end;

procedure TOBDUploader.UploadAsync(AAddress: UInt64; ASize: UInt32);
var
  Self_: TOBDUploader;
  Addr: UInt64;
  Sz: UInt32;
begin
  GuardSingleAsync;
  Self_ := Self; Addr := AAddress; Sz := ASize;
  TThread.CreateAnonymousThread(
    procedure
    var
      Img: TBytes;
    begin
      try
        try
          Img := Self_.DoUpload(Addr, Sz);
          // Img already delivered via FireComplete inside DoUpload.
          if Length(Img) = 0 then ; // suppress hint
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

function TOBDUploader.FireBeforeUpload(AAddress: UInt64;
  ASize: UInt32): Boolean;
var
  Cancel: Boolean;
  Self_: TOBDUploader;
  Addr: UInt64;
  Sz: UInt32;
  Local: Boolean;
begin
  if not Assigned(FOnBeforeUpload) then Exit(True);
  Self_ := Self; Addr := AAddress; Sz := ASize; Cancel := False;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnBeforeUpload(Self_, Addr, Sz, Cancel)
  else
  begin
    Local := False;
    TThread.Synchronize(nil, procedure
      var C: Boolean;
      begin
        C := False;
        if Assigned(Self_.FOnBeforeUpload) then
          Self_.FOnBeforeUpload(Self_, Addr, Sz, C);
        Local := C;
      end);
    Cancel := Local;
  end;
  Result := not Cancel;
end;

procedure TOBDUploader.FireComplete(AAddress: UInt64;
  const AImage: TBytes);
var
  Self_: TOBDUploader;
  Addr: UInt64;
  Snap: TBytes;
begin
  if not Assigned(FOnUploadComplete) then Exit;
  Self_ := Self; Addr := AAddress;
  Snap := Copy(AImage, 0, Length(AImage));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnUploadComplete(Self_, Addr, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnUploadComplete) then
        Self_.FOnUploadComplete(Self_, Addr, Snap);
    end);
end;

procedure TOBDUploader.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUploader; Code: TOBDErrorCode; Msg: string;
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

procedure TOBDUploader.FireProgress(AIndex, ACount: Cardinal;
  const AName, ADetail: string);
var
  Self_: TOBDUploader;
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
