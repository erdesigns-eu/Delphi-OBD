//------------------------------------------------------------------------------
//  OBD.Coding.DataIdentifierIO
//
//  TOBDDataIdentifierIO — non-visual component that reads and
//  writes UDS Data Identifiers (DIDs) per ISO 14229-1 §11.4 / §11.6.
//
//    - ReadDataByIdentifier (0x22): one or more DIDs in, raw bytes
//      out per DID echo.
//    - WriteDataByIdentifier (0x2E): one DID + bytes in, ack out.
//
//  Writing is gated by <c>AutoExecute</c> (default <c>False</c>),
//  matching the safety contract from <c>TOBDActuator</c>: a host
//  that intentionally writes to the ECU explicitly opts in. Reads
//  are unrestricted — they cannot mutate ECU state.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 § 11.4 (ReadDataByIdentifier)
//    - ISO 14229-1:2020 § 11.6 (WriteDataByIdentifier)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.DataIdentifierIO;

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
  /// <summary>One DID + raw bytes pair returned by a read.</summary>
  TOBDDIDValue = record
    DID: Word;
    Data: TBytes;
  end;

  /// <summary>Fires when a read completes with the decoded list.</summary>
  TOBDDIDReadEvent = procedure(Sender: TObject;
    const AValues: TArray<TOBDDIDValue>) of object;

  /// <summary>Fires when a write is acknowledged by the ECU.</summary>
  TOBDDIDWriteEvent = procedure(Sender: TObject; ADID: Word) of object;

  /// <summary>UDS Read/Write DID component.</summary>
  TOBDDataIdentifierIO = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnRead: TOBDDIDReadEvent;
    FOnWrite: TOBDDIDWriteEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoRead(const ADIDs: array of Word): TArray<TOBDDIDValue>;
    procedure DoWrite(ADID: Word; const AData: TBytes);
    procedure FireRead(const AValues: TArray<TOBDDIDValue>);
    procedure FireWrite(ADID: Word);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads one or more DIDs in a single 0x22 request.
    /// Returns the response split per DID echo.</summary>
    /// <param name="ADIDs">DIDs to read (1..n).</param>
    /// <exception cref="EOBDProtocolErr">Negative response or
    /// malformed reply.</exception>
    function Read(const ADIDs: array of Word): TArray<TOBDDIDValue>;
    /// <summary>
    ///   Strict-mode read: the caller declares the data length per
    ///   DID, so the response is split deterministically by length
    ///   instead of heuristically by next-DID-echo. Use this when
    ///   one of your DIDs could legitimately produce payload bytes
    ///   that collide with another requested DID's byte pair.
    /// </summary>
    /// <param name="ADIDs">DIDs to read.</param>
    /// <param name="ALengths">Per-DID payload byte counts. Length
    /// must match <c>ADIDs</c>.</param>
    /// <exception cref="EOBDConfig">Length-array size mismatch.</exception>
    /// <exception cref="EOBDProtocolErr">DID echo mismatch or
    /// truncated response.</exception>
    function ReadStrict(const ADIDs: array of Word;
      const ALengths: array of Integer): TArray<TOBDDIDValue>;
    /// <summary>Convenience: read a single DID.</summary>
    function ReadOne(ADID: Word): TBytes;
    /// <summary>Non-blocking <see cref="Read"/>.</summary>
    procedure ReadAsync(const ADIDs: array of Word);

    /// <summary>Writes <c>AData</c> to <c>ADID</c>. Requires
    /// <c>AutoExecute = True</c>; raises <c>EOBDConfig</c>
    /// otherwise.</summary>
    procedure Write(ADID: Word; const AData: TBytes);
    /// <summary>Non-blocking <see cref="Write"/>.</summary>
    procedure WriteAsync(ADID: Word; const AData: TBytes);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate for <c>Write</c> / <c>WriteAsync</c>.
    /// Default <c>False</c>. Reads are not gated.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    /// <summary>Fires on successful read (main thread).</summary>
    property OnRead: TOBDDIDReadEvent read FOnRead write FOnRead;
    /// <summary>Fires on successful write (main thread).</summary>
    property OnWrite: TOBDDIDWriteEvent read FOnWrite write FOnWrite;
    /// <summary>Fires on transient I/O errors (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDDataIdentifierIO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FAutoExecute := False;
end;

destructor TOBDDataIdentifierIO.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDDataIdentifierIO.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDDataIdentifierIO.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDDataIdentifierIO.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDDataIdentifierIO: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDDataIdentifierIO.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDDataIdentifierIO.DoRead(
  const ADIDs: array of Word): TArray<TOBDDIDValue>;
var
  Body: TBytes;
  Resp: TOBDResponse;
  I, Off, Total, NextDIDStart: Integer;
  Acc: TList<TOBDDIDValue>;
  Cur: TOBDDIDValue;
  CurDID: Word;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDDataIdentifierIO: Protocol not assigned');
  if Length(ADIDs) = 0 then
    raise EOBDConfig.Create('TOBDDataIdentifierIO.Read: no DIDs requested');

  SetLength(Body, 2 * Length(ADIDs));
  for I := 0 to High(ADIDs) do
  begin
    Body[I * 2]     := Hi(ADIDs[I]);
    Body[I * 2 + 1] := Lo(ADIDs[I]);
  end;
  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDataByIdentifier negative: %s', [Resp.NRCText]);

  // Response: <DID HI LO> <data...> repeated. To split, we walk
  // forward and assume each DID block ends at the next DID byte
  // pair. In multi-DID requests, the boundary is ambiguous unless
  // the host knows the lengths per DID; we follow the common
  // convention used by every OEM tester I've inspected: split on
  // the requested DIDs in the order they were sent and consume
  // whatever bytes follow until the next requested DID is seen.
  Total := Length(Resp.Data);
  Acc := TList<TOBDDIDValue>.Create;
  try
    Off := 0;
    for I := 0 to High(ADIDs) do
    begin
      if Off + 2 > Total then
        raise EOBDProtocolErr.Create(
          'ReadDataByIdentifier: response truncated');
      CurDID := (Word(Resp.Data[Off]) shl 8) or Word(Resp.Data[Off + 1]);
      if CurDID <> ADIDs[I] then
        raise EOBDProtocolErr.CreateFmt(
          'ReadDataByIdentifier: expected DID 0x%4.4X at offset %d, got 0x%4.4X',
          [ADIDs[I], Off, CurDID]);
      Inc(Off, 2);
      if I < High(ADIDs) then
      begin
        // Find the next requested DID boundary forward in the buffer.
        NextDIDStart := Total;
        var J: Integer;
        for J := Off to Total - 2 do
          if (Resp.Data[J] = Hi(ADIDs[I + 1])) and
             (Resp.Data[J + 1] = Lo(ADIDs[I + 1])) then
          begin
            NextDIDStart := J;
            Break;
          end;
      end
      else
        NextDIDStart := Total;
      Cur := Default(TOBDDIDValue);
      Cur.DID := CurDID;
      SetLength(Cur.Data, NextDIDStart - Off);
      if Length(Cur.Data) > 0 then
        Move(Resp.Data[Off], Cur.Data[0], Length(Cur.Data));
      Acc.Add(Cur);
      Off := NextDIDStart;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDDataIdentifierIO.Read(
  const ADIDs: array of Word): TArray<TOBDDIDValue>;
begin
  Result := DoRead(ADIDs);
  FireRead(Result);
end;

function TOBDDataIdentifierIO.ReadStrict(const ADIDs: array of Word;
  const ALengths: array of Integer): TArray<TOBDDIDValue>;
var
  Body: TBytes;
  Resp: TOBDResponse;
  I, Off, Total: Integer;
  Acc: TList<TOBDDIDValue>;
  Cur: TOBDDIDValue;
  CurDID: Word;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDDataIdentifierIO: Protocol not assigned');
  if Length(ADIDs) = 0 then
    raise EOBDConfig.Create('ReadStrict: no DIDs');
  if Length(ADIDs) <> Length(ALengths) then
    raise EOBDConfig.Create(
      'ReadStrict: ADIDs and ALengths must have the same length');

  SetLength(Body, 2 * Length(ADIDs));
  for I := 0 to High(ADIDs) do
  begin
    Body[I * 2]     := Hi(ADIDs[I]);
    Body[I * 2 + 1] := Lo(ADIDs[I]);
  end;
  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadStrict negative: %s', [Resp.NRCText]);

  Total := Length(Resp.Data);
  Acc := TList<TOBDDIDValue>.Create;
  try
    Off := 0;
    for I := 0 to High(ADIDs) do
    begin
      if Off + 2 + ALengths[I] > Total then
        raise EOBDProtocolErr.CreateFmt(
          'ReadStrict: response truncated at DID 0x%4.4X (need %d B + 2 B echo, have %d)',
          [ADIDs[I], ALengths[I], Total - Off]);
      CurDID := (Word(Resp.Data[Off]) shl 8) or Word(Resp.Data[Off + 1]);
      if CurDID <> ADIDs[I] then
        raise EOBDProtocolErr.CreateFmt(
          'ReadStrict: expected DID 0x%4.4X at offset %d, got 0x%4.4X',
          [ADIDs[I], Off, CurDID]);
      Inc(Off, 2);
      Cur := Default(TOBDDIDValue);
      Cur.DID := CurDID;
      SetLength(Cur.Data, ALengths[I]);
      if ALengths[I] > 0 then
        Move(Resp.Data[Off], Cur.Data[0], ALengths[I]);
      Inc(Off, ALengths[I]);
      Acc.Add(Cur);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
  FireRead(Result);
end;

function TOBDDataIdentifierIO.ReadOne(ADID: Word): TBytes;
var
  Acc: TArray<TOBDDIDValue>;
begin
  Acc := DoRead([ADID]);
  if Length(Acc) = 0 then
    raise EOBDProtocolErr.Create('ReadDataByIdentifier: empty response');
  Result := Acc[0].Data;
  FireRead(Acc);
end;

procedure TOBDDataIdentifierIO.ReadAsync(const ADIDs: array of Word);
var
  Self_: TOBDDataIdentifierIO;
  DIDsCopy: TArray<Word>;
  I: Integer;
begin
  GuardSingleAsync;
  Self_ := Self;
  SetLength(DIDsCopy, Length(ADIDs));
  for I := 0 to High(ADIDs) do DIDsCopy[I] := ADIDs[I];
  TThread.CreateAnonymousThread(
    procedure
    var
      Acc: TArray<TOBDDIDValue>;
    begin
      try
        try
          Acc := Self_.DoRead(DIDsCopy);
          Self_.FireRead(Acc);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDDataIdentifierIO.DoWrite(ADID: Word; const AData: TBytes);
var
  Body: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDDataIdentifierIO: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDDataIdentifierIO: AutoExecute is False — set it before writing');
  SetLength(Body, 2 + Length(AData));
  Body[0] := Hi(ADID);
  Body[1] := Lo(ADID);
  if Length(AData) > 0 then
    Move(AData[0], Body[2], Length(AData));
  Resp := FProtocol.Request(UDS_SID_WriteDataByIdentifier, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'WriteDataByIdentifier 0x%4.4X negative: %s',
      [ADID, Resp.NRCText]);
end;

procedure TOBDDataIdentifierIO.Write(ADID: Word; const AData: TBytes);
begin
  DoWrite(ADID, AData);
  FireWrite(ADID);
end;

procedure TOBDDataIdentifierIO.WriteAsync(ADID: Word; const AData: TBytes);
var
  Self_: TOBDDataIdentifierIO;
  DID: Word;
  Data: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self; DID := ADID; Data := Copy(AData, 0, Length(AData));
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoWrite(DID, Data);
          Self_.FireWrite(DID);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDDataIdentifierIO.FireRead(
  const AValues: TArray<TOBDDIDValue>);
var
  Self_: TOBDDataIdentifierIO;
  Snap: TArray<TOBDDIDValue>;
begin
  if not Assigned(FOnRead) then Exit;
  Self_ := Self; Snap := Copy(AValues, 0, Length(AValues));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRead(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnRead) then Self_.FOnRead(Self_, Snap);
    end);
end;

procedure TOBDDataIdentifierIO.FireWrite(ADID: Word);
var
  Self_: TOBDDataIdentifierIO;
  DID: Word;
begin
  if not Assigned(FOnWrite) then Exit;
  Self_ := Self; DID := ADID;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnWrite(Self_, DID)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnWrite) then Self_.FOnWrite(Self_, DID);
    end);
end;

procedure TOBDDataIdentifierIO.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDDataIdentifierIO; Code: TOBDErrorCode; Msg: string;
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

end.
