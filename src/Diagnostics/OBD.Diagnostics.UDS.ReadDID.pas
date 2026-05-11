//------------------------------------------------------------------------------
//  OBD.Diagnostics.UDS.ReadDID
//
//  TOBDUDSReadDID — non-visual component for the UDS
//  ReadDataByIdentifier service (SID 0x22) and ReadScalingDataBy-
//  Identifier (SID 0x24). Reads one or more 16-bit DIDs in a
//  single request and returns the response bytes per DID.
//
//  Distinct from the bundled TOBDDataIdentifierIO (which combines
//  read + write into one component). This component is read-only
//  so a host can drop a single-purpose reader on a form without
//  bringing in the safety-gated write surface.
//
//  Wire format per ISO 14229-1 §10.2:
//
//    Request : 22 <DID1-hi> <DID1-lo> [<DID2-hi> <DID2-lo> ...]
//    Response: 62 <DID1-hi> <DID1-lo> <data1...>
//                  [<DID2-hi> <DID2-lo> <data2...>]
//
//  Because the response is variable-length per DID the component
//  needs to know the expected length of each DID; callers pass an
//  explicit per-DID length array. The Phase 5 catalogue
//  (catalogs/uds/dids-generic.json) is the recommended source for
//  those lengths.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §10.2 (ReadDataByIdentifier)
//    - ISO 14229-1:2020 §10.4 (ReadScalingDataByIdentifier)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.UDS.ReadDID;

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

type
  /// <summary>
  ///   One decoded DID value returned by <see cref="TOBDUDSReadDID"/>.
  /// </summary>
  TOBDUDSDIDValue = record
    /// <summary>The 16-bit DID requested.</summary>
    DID: Word;
    /// <summary>Response bytes for this DID (excluding the DID
    /// echo).</summary>
    Data: TBytes;
  end;

  /// <summary>
  ///   Fires after a successful multi-DID read. Main thread.
  /// </summary>
  TOBDUDSReadDIDsEvent = procedure(Sender: TObject;
    const AValues: TArray<TOBDUDSDIDValue>) of object;

  /// <summary>
  ///   UDS ReadDataByIdentifier component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form and assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>. Call
  ///   <see cref="Read"/> with one or more DIDs and matching expected
  ///   lengths; the component encodes the multi-DID request and
  ///   splits the variable-length response back into per-DID
  ///   buckets.
  /// </remarks>
  TOBDUDSReadDID = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnRead: TOBDUDSReadDIDsEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoRead(const ADIDs: array of Word;
      const ALengths: array of Integer): TArray<TOBDUDSDIDValue>;
    procedure FireRead(const AValues: TArray<TOBDUDSDIDValue>);
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
    ///   Reads one or more DIDs in a single UDS request.
    /// </summary>
    /// <param name="ADIDs">DIDs to read.</param>
    /// <param name="ALengths">Expected response byte count per
    /// DID. Must match <c>ADIDs</c> in length.</param>
    /// <returns>Decoded per-DID values in request order.</returns>
    /// <remarks>Blocks. Fires <c>OnRead</c> on success.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned, <c>ADIDs</c> is empty,
    ///   or the length array doesn't match.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response, the response was
    ///   truncated, or a DID echo did not match the requested
    ///   order.
    /// </exception>
    function Read(const ADIDs: array of Word;
      const ALengths: array of Integer): TArray<TOBDUDSDIDValue>;

    /// <summary>Non-blocking <see cref="Read"/>.</summary>
    /// <param name="ADIDs">DIDs to read.</param>
    /// <param name="ALengths">Expected response byte counts.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadAsync(const ADIDs: array of Word;
      const ALengths: array of Integer);
  published
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires on success. Main thread.</summary>
    property OnRead: TOBDUDSReadDIDsEvent read FOnRead write FOnRead;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSReadDID.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDUDSReadDID.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSReadDID.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSReadDID.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSReadDID.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSReadDID: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSReadDID.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDUDSReadDID.DoRead(const ADIDs: array of Word;
  const ALengths: array of Integer): TArray<TOBDUDSDIDValue>;
var
  Req: TBytes;
  Resp: TOBDResponse;
  I: Integer;
  Off: Integer;
  EchoDID: Word;
  Want: Integer;
  Acc: TList<TOBDUDSDIDValue>;
  V: TOBDUDSDIDValue;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSReadDID: Protocol not assigned');
  if Length(ADIDs) = 0 then
    raise EOBDConfig.Create('TOBDUDSReadDID: no DIDs requested');
  if Length(ALengths) <> Length(ADIDs) then
    raise EOBDConfig.Create(
      'TOBDUDSReadDID: length array size does not match DID count');

  SetLength(Req, 2 * Length(ADIDs));
  for I := 0 to High(ADIDs) do
  begin
    Req[2 * I]     := Byte((ADIDs[I] shr 8) and $FF);
    Req[2 * I + 1] := Byte(ADIDs[I] and $FF);
  end;

  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDataByIdentifier negative: %s', [Resp.NRCText]);

  Acc := TList<TOBDUDSDIDValue>.Create;
  try
    Off := 0;
    for I := 0 to High(ADIDs) do
    begin
      Want := ALengths[I];
      if Off + 2 + Want > Length(Resp.Data) then
        raise EOBDProtocolErr.CreateFmt(
          'ReadDataByIdentifier: truncated at DID 0x%.4x (offset %d, '
          + 'wanted %d more bytes)',
          [ADIDs[I], Off, 2 + Want]);

      EchoDID := (Word(Resp.Data[Off]) shl 8) or
                  Word(Resp.Data[Off + 1]);
      if EchoDID <> ADIDs[I] then
        raise EOBDProtocolErr.CreateFmt(
          'ReadDataByIdentifier echo mismatch at slot %d: ' +
          'requested 0x%.4x, got 0x%.4x',
          [I, ADIDs[I], EchoDID]);

      V.DID := EchoDID;
      V.Data := Copy(Resp.Data, Off + 2, Want);
      Acc.Add(V);
      Inc(Off, 2 + Want);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDUDSReadDID.Read(const ADIDs: array of Word;
  const ALengths: array of Integer): TArray<TOBDUDSDIDValue>;
begin
  Result := DoRead(ADIDs, ALengths);
  FireRead(Result);
end;

procedure TOBDUDSReadDID.ReadAsync(const ADIDs: array of Word;
  const ALengths: array of Integer);
var
  Self_: TOBDUDSReadDID;
  DIDsCopy: TArray<Word>;
  LensCopy: TArray<Integer>;
  I: Integer;
begin
  GuardSingleAsync;
  Self_ := Self;
  SetLength(DIDsCopy, Length(ADIDs));
  for I := 0 to High(ADIDs) do
    DIDsCopy[I] := ADIDs[I];
  SetLength(LensCopy, Length(ALengths));
  for I := 0 to High(ALengths) do
    LensCopy[I] := ALengths[I];

  TThread.CreateAnonymousThread(
    procedure
    var
      Values: TArray<TOBDUDSDIDValue>;
    begin
      try
        try
          Values := Self_.DoRead(DIDsCopy, LensCopy);
          Self_.FireRead(Values);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSReadDID.FireRead(const AValues: TArray<TOBDUDSDIDValue>);
var
  Self_: TOBDUDSReadDID;
  Snap: TArray<TOBDUDSDIDValue>;
begin
  if not Assigned(FOnRead) then
    Exit;
  Self_ := Self;
  Snap := Copy(AValues, 0, Length(AValues));
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

procedure TOBDUDSReadDID.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSReadDID;
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
