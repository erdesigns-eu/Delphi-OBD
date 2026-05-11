//------------------------------------------------------------------------------
//  OBD.UDS.WriteMemory
//
//  TOBDUDSWriteMemory — non-visual component for ISO 14229-1
//  WriteMemoryByAddress (SID 0x3D). Distinct from
//  WriteDataByIdentifier (0x2E, covered by TOBDDataIdentifierIO):
//  WriteMemoryByAddress targets a raw memory region without a DID
//  catalogue, which is the path used for byte-level coding patches
//  on some controllers and for write-after-erase during flashing.
//
//  Wire format per ISO 14229-1 §11.7:
//
//    Request : 3D <addrAndLengthFmt> <memoryAddress> <memorySize> <data...>
//    Response: 7D <addrAndLengthFmt> <memoryAddress> <memorySize>
//
//  AutoExecute = False default — every write raises EOBDConfig
//  before any wire access until the host explicitly opts in.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 § 11.7 (WriteMemoryByAddress)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.UDS.WriteMemory;

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
  /// <summary>UDS WriteMemoryByAddress SID.</summary>
  UDS_SID_WriteMemoryByAddress = $3D;

type
  /// <summary>WriteMemoryByAddress component.</summary>
  TOBDUDSWriteMemory = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAddressFormatBytes: Byte;
    FLengthFormatBytes: Byte;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnWrite: TNotifyEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoWrite(AAddress: UInt64; const AData: TBytes);
    function EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
    procedure FireWrite;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Writes <c>AData</c> at <c>AAddress</c>. Single
    /// call — the caller chunks if the payload exceeds the
    /// adapter's frame budget.</summary>
    procedure Write(AAddress: UInt64; const AData: TBytes);
    /// <summary>Non-blocking <see cref="Write"/>.</summary>
    procedure WriteAsync(AAddress: UInt64; const AData: TBytes);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    property AddressFormatBytes: Byte read FAddressFormatBytes
      write FAddressFormatBytes default 4;
    property LengthFormatBytes: Byte read FLengthFormatBytes
      write FLengthFormatBytes default 4;
    property OnWrite: TNotifyEvent read FOnWrite write FOnWrite;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSWriteMemory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FAddressFormatBytes := 4;
  FLengthFormatBytes := 4;
end;

destructor TOBDUDSWriteMemory.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSWriteMemory.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSWriteMemory.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSWriteMemory.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSWriteMemory: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDUDSWriteMemory.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDUDSWriteMemory.EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
var
  I: Integer;
begin
  if (ABytes = 0) or (ABytes > 8) then
    raise EOBDConfig.CreateFmt(
      'TOBDUDSWriteMemory: bad format byte count %d', [ABytes]);
  SetLength(Result, ABytes);
  for I := 0 to ABytes - 1 do
    Result[I] := Byte((AValue shr (8 * (ABytes - 1 - I))) and $FF);
end;

procedure TOBDUDSWriteMemory.DoWrite(AAddress: UInt64;
  const AData: TBytes);
var
  Body: TBytes;
  AddrBytes, LenBytes: TBytes;
  AddrAndLenFmt: Byte;
  Resp: TOBDResponse;
  Off: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSWriteMemory: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDUDSWriteMemory: AutoExecute is False — set it before writing');
  if Length(AData) = 0 then
    raise EOBDConfig.Create('TOBDUDSWriteMemory: empty data');

  AddrBytes := EncodeMSB(AAddress, FAddressFormatBytes);
  LenBytes  := EncodeMSB(UInt64(Length(AData)), FLengthFormatBytes);
  AddrAndLenFmt := Byte((FLengthFormatBytes shl 4) or
                        (FAddressFormatBytes and $0F));

  SetLength(Body, 1 + Length(AddrBytes) + Length(LenBytes) + Length(AData));
  Body[0] := AddrAndLenFmt;
  Off := 1;
  Move(AddrBytes[0], Body[Off], Length(AddrBytes));
  Inc(Off, Length(AddrBytes));
  Move(LenBytes[0], Body[Off], Length(LenBytes));
  Inc(Off, Length(LenBytes));
  Move(AData[0], Body[Off], Length(AData));

  Resp := FProtocol.Request(UDS_SID_WriteMemoryByAddress, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'WriteMemoryByAddress 0x%x negative: %s', [AAddress, Resp.NRCText]);
end;

procedure TOBDUDSWriteMemory.Write(AAddress: UInt64; const AData: TBytes);
begin
  DoWrite(AAddress, AData);
  FireWrite;
end;

procedure TOBDUDSWriteMemory.WriteAsync(AAddress: UInt64;
  const AData: TBytes);
var
  Self_: TOBDUDSWriteMemory;
  Addr: UInt64;
  Data: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self; Addr := AAddress;
  Data := Copy(AData, 0, Length(AData));
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoWrite(Addr, Data);
          Self_.FireWrite;
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSWriteMemory.FireWrite;
var
  Self_: TOBDUDSWriteMemory;
begin
  if not Assigned(FOnWrite) then Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnWrite(Self_)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnWrite) then Self_.FOnWrite(Self_);
    end);
end;

procedure TOBDUDSWriteMemory.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSWriteMemory; Code: TOBDErrorCode; Msg: string;
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
