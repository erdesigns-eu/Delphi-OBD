//------------------------------------------------------------------------------
//  OBD.Diagnostics.UDS.DynamicDID
//
//  TOBDUDSDynamicDID — non-visual component for the UDS
//  DynamicallyDefineDataIdentifier service (SID 0x2C). Lets a host
//  build a synthetic DID by concatenating slices of existing DIDs
//  or raw memory regions, then read that synthetic DID through the
//  normal 0x22 surface. Useful for batching many small values into
//  one ReadDataByIdentifier round-trip.
//
//  Wire format per ISO 14229-1 §10.7:
//
//    Define by DID :  2C 01 <dynDID-hi> <dynDID-lo>
//                       [<src-DID-hi> <src-DID-lo> <off> <len>]+
//    Define by mem :  2C 02 <dynDID-hi> <dynDID-lo>
//                       <addrAndLenFmt> [<addr>...] [<len>...]
//    Clear         :  2C 03 [<dynDID-hi> <dynDID-lo>]
//    Response      :  6C <subFunction> <dynDID-hi> <dynDID-lo>
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §10.7 (DynamicallyDefineDataIdentifier)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.UDS.DynamicDID;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol;

const
  /// <summary>0x01 defineByIdentifier (compose from other DIDs).</summary>
  UDS_DDD_SUB_DefineByDID    = $01;
  /// <summary>0x02 defineByMemoryAddress.</summary>
  UDS_DDD_SUB_DefineByMemory = $02;
  /// <summary>0x03 clearDynamicallyDefinedDataIdentifier.</summary>
  UDS_DDD_SUB_Clear          = $03;

type
  /// <summary>
  ///   One source-DID slice in a "define by DID" request.
  /// </summary>
  TOBDUDSDDDSlice = record
    /// <summary>Source 16-bit DID to read from.</summary>
    SourceDID: Word;
    /// <summary>Byte position within the source DID (1-based per
    /// ISO 14229-1 §10.7.2.3 positionInSourceDataRecord).</summary>
    Position: Byte;
    /// <summary>Byte count to copy from the source.</summary>
    MemorySize: Byte;
  end;

  /// <summary>
  ///   Fires after a successful define / clear. Main thread.
  /// </summary>
  TOBDUDSDynamicDIDEvent = procedure(Sender: TObject;
    ASubFunction: Byte; ADynamicDID: Word) of object;

  /// <summary>
  ///   UDS DynamicallyDefineDataIdentifier component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form and assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>. Use
  ///   <see cref="DefineByDID"/> to compose a synthetic DID from
  ///   slices of existing DIDs, <see cref="ClearDynamic"/> to drop
  ///   one (or all). Once defined, read the synthetic DID through
  ///   the normal ReadDataByIdentifier surface.
  /// </remarks>
  TOBDUDSDynamicDID = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnDefined: TOBDUDSDynamicDIDEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoSend(ASubFunction: Byte; const ABody: TBytes);
    procedure FireDefined(ASubFunction: Byte; ADynamicDID: Word);
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
    ///   Defines a dynamic DID by composing slices of existing
    ///   DIDs.
    /// </summary>
    /// <param name="ADynamicDID">Synthetic 16-bit DID number to
    /// define (must be in the OEM-defined dynamic range).</param>
    /// <param name="ASlices">Ordered list of source-DID slices.
    /// Must contain at least one entry.</param>
    /// <remarks>Blocks. Fires <c>OnDefined</c> with
    /// <c>UDS_DDD_SUB_DefineByDID</c> on success.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned or no slices supplied.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    procedure DefineByDID(ADynamicDID: Word;
      const ASlices: array of TOBDUDSDDDSlice);

    /// <summary>
    ///   Clears a single dynamic DID, or every dynamic DID when
    ///   <c>ADynamicDID = 0</c>.
    /// </summary>
    /// <param name="ADynamicDID">DID to clear, or 0 for clear-all.</param>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    procedure ClearDynamic(ADynamicDID: Word = 0);

    /// <summary>Non-blocking <see cref="DefineByDID"/>.</summary>
    /// <param name="ADynamicDID">Synthetic DID.</param>
    /// <param name="ASlices">Source slices.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async send is already in flight.
    /// </exception>
    procedure DefineByDIDAsync(ADynamicDID: Word;
      const ASlices: array of TOBDUDSDDDSlice);
  published
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires after a successful define / clear. Main thread.</summary>
    property OnDefined: TOBDUDSDynamicDIDEvent read FOnDefined
      write FOnDefined;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSDynamicDID.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDUDSDynamicDID.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSDynamicDID.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSDynamicDID.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSDynamicDID.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSDynamicDID: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSDynamicDID.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSDynamicDID.DoSend(ASubFunction: Byte;
  const ABody: TBytes);
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSDynamicDID: Protocol not assigned');
  SetLength(Req, 1 + Length(ABody));
  Req[0] := ASubFunction;
  if Length(ABody) > 0 then
    Move(ABody[0], Req[1], Length(ABody));

  Resp := FProtocol.Request(UDS_SID_DynamicallyDefineDataIdentifier, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'DynamicallyDefineDataIdentifier (sub 0x%.2x) negative: %s',
      [ASubFunction, Resp.NRCText]);
end;

procedure TOBDUDSDynamicDID.DefineByDID(ADynamicDID: Word;
  const ASlices: array of TOBDUDSDDDSlice);
var
  Body: TBytes;
  I: Integer;
  Off: Integer;
begin
  if Length(ASlices) = 0 then
    raise EOBDConfig.Create(
      'TOBDUDSDynamicDID.DefineByDID: at least one slice required');

  // Body layout: <dynDID-hi> <dynDID-lo>
  //              [<srcDID-hi> <srcDID-lo> <pos> <len>]+
  SetLength(Body, 2 + 4 * Length(ASlices));
  Body[0] := Byte((ADynamicDID shr 8) and $FF);
  Body[1] := Byte(ADynamicDID and $FF);
  Off := 2;
  for I := 0 to High(ASlices) do
  begin
    Body[Off]     := Byte((ASlices[I].SourceDID shr 8) and $FF);
    Body[Off + 1] := Byte(ASlices[I].SourceDID and $FF);
    Body[Off + 2] := ASlices[I].Position;
    Body[Off + 3] := ASlices[I].MemorySize;
    Inc(Off, 4);
  end;

  DoSend(UDS_DDD_SUB_DefineByDID, Body);
  FireDefined(UDS_DDD_SUB_DefineByDID, ADynamicDID);
end;

procedure TOBDUDSDynamicDID.ClearDynamic(ADynamicDID: Word);
var
  Body: TBytes;
begin
  if ADynamicDID = 0 then
    SetLength(Body, 0)
  else
  begin
    SetLength(Body, 2);
    Body[0] := Byte((ADynamicDID shr 8) and $FF);
    Body[1] := Byte(ADynamicDID and $FF);
  end;
  DoSend(UDS_DDD_SUB_Clear, Body);
  FireDefined(UDS_DDD_SUB_Clear, ADynamicDID);
end;

procedure TOBDUDSDynamicDID.DefineByDIDAsync(ADynamicDID: Word;
  const ASlices: array of TOBDUDSDDDSlice);
var
  Self_: TOBDUDSDynamicDID;
  DID: Word;
  SlicesCopy: TArray<TOBDUDSDDDSlice>;
  I: Integer;
begin
  GuardSingleAsync;
  Self_ := Self;
  DID := ADynamicDID;
  SetLength(SlicesCopy, Length(ASlices));
  for I := 0 to High(ASlices) do
    SlicesCopy[I] := ASlices[I];

  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DefineByDID(DID, SlicesCopy);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSDynamicDID.FireDefined(ASubFunction: Byte;
  ADynamicDID: Word);
var
  Self_: TOBDUDSDynamicDID;
  Sub: Byte;
  DID: Word;
begin
  if not Assigned(FOnDefined) then
    Exit;
  Self_ := Self;
  Sub := ASubFunction;
  DID := ADynamicDID;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDefined(Self_, Sub, DID)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnDefined) then
          Self_.FOnDefined(Self_, Sub, DID);
      end);
end;

procedure TOBDUDSDynamicDID.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSDynamicDID;
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
