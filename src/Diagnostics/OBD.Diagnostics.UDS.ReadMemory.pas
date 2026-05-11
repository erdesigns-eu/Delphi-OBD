//------------------------------------------------------------------------------
//  OBD.Diagnostics.UDS.ReadMemory
//
//  TOBDUDSReadMemory — non-visual component for the UDS
//  ReadMemoryByAddress service (SID 0x23). Reads a raw memory
//  region from the ECU by (address, length); the host is
//  responsible for knowing what those bytes mean.
//
//  Wire format per ISO 14229-1 §10.3:
//
//    Request : 23 <addrAndLengthFmt> <memoryAddress> <memorySize>
//    Response: 63 <data...>
//
//  Read is not destructive but it can leak proprietary content
//  (e.g. firmware regions) — most ECUs gate it behind an extended
//  diagnostic session, and many gate it behind Security Access as
//  well. The component does not enforce a gate; callers wire the
//  session hub + security-access component themselves.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §10.3 (ReadMemoryByAddress)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.UDS.ReadMemory;

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
  /// <summary>
  ///   Fires after a successful read. Main thread.
  /// </summary>
  /// <remarks>
  ///   <c>AAddress</c> echoes the requested address;
  ///   <c>AData</c> carries the returned bytes (a fresh copy
  ///   safe to retain past the handler).
  /// </remarks>
  TOBDUDSReadMemoryEvent = procedure(Sender: TObject;
    AAddress: UInt64; const AData: TBytes) of object;

  /// <summary>
  ///   UDS ReadMemoryByAddress component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form and assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>. Tune
  ///   <see cref="AddressFormatBytes"/> and
  ///   <see cref="LengthFormatBytes"/> to match the target ECU's
  ///   addressing model (most modern ECUs are 4 / 4, some legacy
  ///   16-bit cores are 2 / 2).
  /// </remarks>
  TOBDUDSReadMemory = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAddressFormatBytes: Byte;
    FLengthFormatBytes: Byte;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnRead: TOBDUDSReadMemoryEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoRead(AAddress: UInt64; ALength: Cardinal): TBytes;
    function EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
    procedure FireRead(AAddress: UInt64; const AData: TBytes);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the component with 4-byte address and
    /// 4-byte length defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state and cancels in-flight async work.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Reads <c>ALength</c> bytes starting at <c>AAddress</c>.
    /// </summary>
    /// <param name="AAddress">Source memory address.</param>
    /// <param name="ALength">Byte count to read.</param>
    /// <returns>The returned data bytes.</returns>
    /// <remarks>Blocks. From GUI code prefer
    /// <see cref="ReadAsync"/>.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned, or
    ///   <c>AddressFormatBytes</c> / <c>LengthFormatBytes</c> are
    ///   out of range (0 or > 8).
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    function Read(AAddress: UInt64; ALength: Cardinal): TBytes;

    /// <summary>Non-blocking <see cref="Read"/>.</summary>
    /// <param name="AAddress">Source memory address.</param>
    /// <param name="ALength">Byte count to read.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadAsync(AAddress: UInt64; ALength: Cardinal);
  published
    /// <summary>Protocol stack to read through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>
    ///   Address-field width on the wire. Default <c>4</c>; range
    ///   <c>1..8</c>. Sets the low nibble of the
    ///   <c>addressAndLengthFormatIdentifier</c> byte.
    /// </summary>
    property AddressFormatBytes: Byte read FAddressFormatBytes
      write FAddressFormatBytes default 4;

    /// <summary>
    ///   Length-field width on the wire. Default <c>4</c>; range
    ///   <c>1..8</c>. Sets the high nibble of the
    ///   <c>addressAndLengthFormatIdentifier</c> byte.
    /// </summary>
    property LengthFormatBytes: Byte read FLengthFormatBytes
      write FLengthFormatBytes default 4;

    /// <summary>Fires on success. Main thread.</summary>
    property OnRead: TOBDUDSReadMemoryEvent read FOnRead write FOnRead;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSReadMemory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FAddressFormatBytes := 4;
  FLengthFormatBytes := 4;
end;

destructor TOBDUDSReadMemory.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSReadMemory.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSReadMemory.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSReadMemory.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSReadMemory: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSReadMemory.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDUDSReadMemory.EncodeMSB(AValue: UInt64; ABytes: Byte): TBytes;
var
  I: Integer;
begin
  if (ABytes = 0) or (ABytes > 8) then
    raise EOBDConfig.CreateFmt(
      'TOBDUDSReadMemory: bad format byte count %d', [ABytes]);
  SetLength(Result, ABytes);
  for I := 0 to ABytes - 1 do
    Result[I] := Byte((AValue shr (8 * (ABytes - 1 - I))) and $FF);
end;

function TOBDUDSReadMemory.DoRead(AAddress: UInt64;
  ALength: Cardinal): TBytes;
var
  Body: TBytes;
  AddrBytes: TBytes;
  LenBytes: TBytes;
  AddrAndLenFmt: Byte;
  Resp: TOBDResponse;
  Off: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSReadMemory: Protocol not assigned');
  if ALength = 0 then
    raise EOBDConfig.Create('TOBDUDSReadMemory: zero-length read');

  AddrBytes := EncodeMSB(AAddress, FAddressFormatBytes);
  LenBytes := EncodeMSB(UInt64(ALength), FLengthFormatBytes);
  AddrAndLenFmt := Byte((FLengthFormatBytes shl 4) or
                        (FAddressFormatBytes and $0F));

  SetLength(Body, 1 + Length(AddrBytes) + Length(LenBytes));
  Body[0] := AddrAndLenFmt;
  Off := 1;
  Move(AddrBytes[0], Body[Off], Length(AddrBytes));
  Inc(Off, Length(AddrBytes));
  Move(LenBytes[0], Body[Off], Length(LenBytes));

  Resp := FProtocol.Request(UDS_SID_ReadMemoryByAddress, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadMemoryByAddress (0x%x) negative: %s',
      [AAddress, Resp.NRCText]);
  Result := Copy(Resp.Data, 0, Length(Resp.Data));
end;

function TOBDUDSReadMemory.Read(AAddress: UInt64;
  ALength: Cardinal): TBytes;
begin
  Result := DoRead(AAddress, ALength);
  FireRead(AAddress, Result);
end;

procedure TOBDUDSReadMemory.ReadAsync(AAddress: UInt64;
  ALength: Cardinal);
var
  Self_: TOBDUDSReadMemory;
  Addr: UInt64;
  Len: Cardinal;
begin
  GuardSingleAsync;
  Self_ := Self;
  Addr := AAddress;
  Len := ALength;
  TThread.CreateAnonymousThread(
    procedure
    var
      Data: TBytes;
    begin
      try
        try
          Data := Self_.DoRead(Addr, Len);
          Self_.FireRead(Addr, Data);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSReadMemory.FireRead(AAddress: UInt64;
  const AData: TBytes);
var
  Self_: TOBDUDSReadMemory;
  Addr: UInt64;
  Snap: TBytes;
begin
  if not Assigned(FOnRead) then
    Exit;
  Self_ := Self;
  Addr := AAddress;
  Snap := Copy(AData, 0, Length(AData));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRead(Self_, Addr, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnRead) then
          Self_.FOnRead(Self_, Addr, Snap);
      end);
end;

procedure TOBDUDSReadMemory.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSReadMemory;
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
