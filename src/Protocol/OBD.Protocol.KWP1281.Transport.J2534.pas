//------------------------------------------------------------------------------
//  OBD.Protocol.KWP1281.Transport.J2534
//
//  J2534-based transports for KWP1281:
//
//    TJ2534CANTransport       - implements ICANTransport over a
//                               TJ2534Channel (jpCAN protocol). Plugs
//                               under the TP2.0 / ISO-TP transports
//                               so they can run on a J2534 device
//                               instead of a custom CAN driver.
//
//    TKWP1281J2534Transport   - implements IKWP1281Transport via a
//                               TJ2534Channel opened on jpISO9141
//                               (the closest J2534 protocol to raw
//                               KWP1281 K-line traffic). 5-baud init
//                               is delegated to the device via the
//                               documented FAST_INIT / SLOW_INIT
//                               IOCTLs (one of the few real-world
//                               places the IOCTL gets used outside
//                               of advanced scripting).
//
//  Caveat: not all J2534 DLLs implement SLOW_INIT correctly for
//  arbitrary addresses. The transport falls back to a raw
//  byte-level send/receive if the IOCTL returns NOT_SUPPORTED.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Protocol.KWP1281.Transport.J2534;

{$IFNDEF MSWINDOWS}
{$MESSAGE FATAL 'OBD.Protocol.KWP1281.Transport.J2534 is Windows-only.'}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Protocol.Types,
  OBD.Protocol.CAN,
  OBD.Protocol.KWP1281,
  OBD.J2534;

type
  TJ2534CANTransport = class(TInterfacedObject, ICANTransport)
  strict private
    FChannel: TJ2534Channel;
    FOwnsChannel: Boolean;
    FOnFrame: TCANFrameEvent;
    FExtended: Boolean;
    function  GetOnFrame: TCANFrameEvent;
    procedure SetOnFrame(const AValue: TCANFrameEvent);
  public
    constructor Wrap(AChannel: TJ2534Channel);
    destructor  Destroy; override;
    procedure SendFrame(const AFrame: TOBDFrame; ATimeoutMs: Integer);
    function  ReceiveFrame(ATimeoutMs: Integer): TOBDFrame;
    procedure SetAcceptanceFilter(const AIds: TArray<Cardinal>;
      AExtended: Boolean = False);
    procedure DrainRx;
    property  OnFrame: TCANFrameEvent read GetOnFrame write SetOnFrame;
  end;

  /// <summary>KWP1281 transport on a J2534 device using
  /// jpISO9141 (the wire format the K-line uses for KWP1281).
  /// 5-baud init via IOCTL; falls back to host-driven
  /// byte-level init if the device's DLL doesn't support
  /// SLOW_INIT for arbitrary addresses.</summary>
  TKWP1281J2534Transport = class(TInterfacedObject, IKWP1281Transport)
  strict private
    FChannel: TJ2534Channel;
    FOwnsChannel: Boolean;
    FRxBuf:   TQueue<Byte>;
    procedure RefillRx(ATimeoutMs: Integer);
  public
    constructor Wrap(AChannel: TJ2534Channel);
    destructor Destroy; override;
    procedure SendByte(AByte: Byte; ATimeoutMs: Integer);
    function  ReceiveByte(ATimeoutMs: Integer): Byte;
    procedure FiveBaudInit(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);
    procedure Hangup;
  end;

const
  // J2534 IOCTL ids (subset)
  J2534_IOCTL_FIVE_BAUD_INIT = $05;
  J2534_IOCTL_FAST_INIT      = $06;
  J2534_IOCTL_CLEAR_TX_BUFFER = $07;
  J2534_IOCTL_CLEAR_RX_BUFFER = $08;

implementation

{ TJ2534CANTransport ----------------------------------------------------------}

constructor TJ2534CANTransport.Wrap(AChannel: TJ2534Channel);
begin
  inherited Create;
  FChannel := AChannel;
  FOwnsChannel := False;
end;

destructor TJ2534CANTransport.Destroy;
begin
  if FOwnsChannel then FChannel.Free;
  inherited;
end;

function TJ2534CANTransport.GetOnFrame: TCANFrameEvent;
begin
  Result := FOnFrame;
end;

procedure TJ2534CANTransport.SetOnFrame(const AValue: TCANFrameEvent);
begin
  FOnFrame := AValue;
end;

procedure TJ2534CANTransport.SendFrame(const AFrame: TOBDFrame;
  ATimeoutMs: Integer);
var
  Buf: TBytes;
  TxFlags: Cardinal;
begin
  // J2534 CAN messages: first 4 bytes are the CAN ID big-endian,
  // followed by the payload.
  SetLength(Buf, 4 + Length(AFrame.Payload));
  Buf[0] := Byte(AFrame.Id shr 24);
  Buf[1] := Byte(AFrame.Id shr 16);
  Buf[2] := Byte(AFrame.Id shr 8);
  Buf[3] := Byte(AFrame.Id);
  if Length(AFrame.Payload) > 0 then
    Move(AFrame.Payload[0], Buf[4], Length(AFrame.Payload));
  TxFlags := 0;
  if AFrame.IsExtendedId then
    TxFlags := TxFlags or J2534_TX_ISO15765_CAN_29BIT;
  FChannel.WriteMsg(Buf, TxFlags, Cardinal(ATimeoutMs));
  if Assigned(FOnFrame) then
    FOnFrame(Self, cdTx, AFrame);
end;

function TJ2534CANTransport.ReceiveFrame(ATimeoutMs: Integer): TOBDFrame;
var
  Buf: TBytes;
begin
  if not FChannel.TryReadMsg(Buf, Cardinal(ATimeoutMs)) then
    raise EOBDCANTimeout.CreateFmt(
      'J2534 CAN: no frame within %d ms', [ATimeoutMs]);
  if Length(Buf) < 4 then
    raise EOBDCANError.Create(
      'J2534 CAN: truncated frame (need at least 4 bytes for ID)');
  Result := Default(TOBDFrame);
  Result.Id := (Cardinal(Buf[0]) shl 24) or
               (Cardinal(Buf[1]) shl 16) or
               (Cardinal(Buf[2]) shl 8)  or
                Cardinal(Buf[3]);
  Result.IsExtendedId := FExtended;
  if Length(Buf) > 4 then
  begin
    SetLength(Result.Payload, Length(Buf) - 4);
    Move(Buf[4], Result.Payload[0], Length(Buf) - 4);
  end;
  if Assigned(FOnFrame) then
    FOnFrame(Self, cdRx, Result);
end;

procedure TJ2534CANTransport.SetAcceptanceFilter(
  const AIds: TArray<Cardinal>; AExtended: Boolean);
var Id: Cardinal;
begin
  FExtended := AExtended;
  for Id in AIds do
    FChannel.InstallPassFilter(Id, AExtended);
end;

procedure TJ2534CANTransport.DrainRx;
var Dummy: TBytes;
begin
  while FChannel.TryReadMsg(Dummy, 0) do ;
end;

{ TKWP1281J2534Transport ------------------------------------------------------}

constructor TKWP1281J2534Transport.Wrap(AChannel: TJ2534Channel);
begin
  inherited Create;
  FChannel := AChannel;
  FRxBuf   := TQueue<Byte>.Create;
end;

destructor TKWP1281J2534Transport.Destroy;
begin
  FRxBuf.Free;
  if FOwnsChannel then FChannel.Free;
  inherited;
end;

procedure TKWP1281J2534Transport.RefillRx(ATimeoutMs: Integer);
var Buf: TBytes; I: Integer;
begin
  if not FChannel.TryReadMsg(Buf, Cardinal(ATimeoutMs)) then
    Exit;
  for I := 0 to Length(Buf) - 1 do
    FRxBuf.Enqueue(Buf[I]);
end;

procedure TKWP1281J2534Transport.SendByte(AByte: Byte;
  ATimeoutMs: Integer);
begin
  FChannel.WriteMsg(TBytes.Create(AByte), 0, Cardinal(ATimeoutMs));
end;

function TKWP1281J2534Transport.ReceiveByte(ATimeoutMs: Integer): Byte;
begin
  if FRxBuf.Count = 0 then
    RefillRx(ATimeoutMs);
  if FRxBuf.Count = 0 then
    raise EKWP1281Timeout.CreateFmt(
      'TKWP1281J2534Transport: no byte within %d ms',
      [ATimeoutMs]);
  Result := FRxBuf.Dequeue;
end;

procedure TKWP1281J2534Transport.FiveBaudInit(AAddress: Byte;
  out AKW1, AKW2: Byte; ATimeoutMs: Integer);
var
  InBuf:  array[0..0] of Cardinal;
  OutBuf: array[0..2] of Cardinal;
begin
  InBuf[0] := AAddress;
  FillChar(OutBuf, SizeOf(OutBuf), 0);
  try
    FChannel.Driver.Ioctl(FChannel.ChannelId,
      J2534_IOCTL_FIVE_BAUD_INIT, @InBuf, @OutBuf);
    AKW1 := Byte(OutBuf[1]);
    AKW2 := Byte(OutBuf[2]);
  except
    on E: EOBDJ2534Error do
      if E.ErrorCode = J2534_ERR_NOT_SUPPORTED then
        raise EKWP1281Error.Create(
          'TKWP1281J2534Transport: this DLL does not support ' +
          'IOCTL FIVE_BAUD_INIT - host must script the init ' +
          'sequence manually')
      else raise;
  end;
end;

procedure TKWP1281J2534Transport.Hangup;
begin
  // Channel close happens on Destroy.
end;

end.
