//------------------------------------------------------------------------------
//  OBD.Protocol.KWP1281.Transport.Serial
//
//  TKWP1281SerialTransport - IKWP1281Transport implementation
//  on top of <see cref="TOBDSerialTransport"/> (Win32 only).
//
//  How the 5-baud init works:
//    1. Hold TX line LOW for 200 ms (start bit).
//    2. For each of the 7 address bits (LSB first), drive TX
//       low for 200 ms if the bit is 0 or release for 200 ms
//       if the bit is 1.
//    3. Release TX (mark) for 200 ms (stop bit).
//    4. Wait for the ECU's sync byte (0x55) at 9600 baud, then
//       read KW1 (1 byte) and KW2 (1 byte).
//
//  Total init time: 8 x 200 ms wake-up + 200 ms stop bit +
//  ECU sync time = ~2.5 seconds. Don't ATB the user during it.
//
//  Per-byte I/O:
//    SendByte() does WriteBytes([b]); the byte clocks out at
//    the configured baud rate (typically 9600 or 10400 for VW
//    radios, 12500 for MB W124).
//    ReceiveByte() pops from a thread-safe queue fed by the
//    base transport's read thread. The queue gives us the
//    synchronous semantics KWP1281 needs without re-implementing
//    the read loop.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial Win32 implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.KWP1281.Transport.Serial;

{$IFNDEF MSWINDOWS}
{$MESSAGE FATAL 'OBD.Protocol.KWP1281.Transport.Serial is Windows-only.'}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Connection.Settings,
  OBD.Connection.Serial,
  OBD.Protocol.KWP1281;

type
  /// <summary>5-baud init bit timing - the standard is 200 ms
  /// per bit (5 baud = 1 bit / 200 ms).</summary>
  TKWP1281SerialInitTiming = record
    BitMs:      Integer;  // default 200
    StopBitMs:  Integer;  // default 200
    SyncWaitMs: Integer;  // wait for radio's $55 sync after stop
  end;

  /// <summary>Concrete KWP1281 K-line transport on top of
  /// <see cref="TOBDSerialTransport"/>. The transport wraps the
  /// serial port (you supply the settings; the transport opens
  /// it on first use), buffers received bytes into a queue so
  /// SendByte/ReceiveByte are synchronous, and bit-bangs the
  /// 5-baud init using SetBreak / ClearBreak / Sleep.</summary>
  TKWP1281SerialTransport = class(TInterfacedObject, IKWP1281Transport)
  strict private
    FSerial:   TOBDSerialTransport;
    FSettings: TOBDSerialSettings;
    FQueue:    TThreadedQueue<Byte>;
    FOwnsSerial: Boolean;
    FInitTiming: TKWP1281SerialInitTiming;
    procedure HandleBytes(const ABytes: TBytes);
    procedure EnsureOpen;
    procedure DrainQueue;
  public
    /// <summary>Wraps an existing - possibly already-open -
    /// serial transport. The caller retains ownership.</summary>
    constructor Wrap(ASerial: TOBDSerialTransport);

    /// <summary>Opens its own serial port using the supplied
    /// settings. The transport owns the port and closes it on
    /// destroy.</summary>
    constructor Create(ASettings: TOBDSerialSettings);

    destructor Destroy; override;

    // ---- IKWP1281Transport -------------------------------------
    procedure SendByte(AByte: Byte; ATimeoutMs: Integer);
    function  ReceiveByte(ATimeoutMs: Integer): Byte;
    procedure FiveBaudInit(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);
    procedure Hangup;

    /// <summary>Init bit-timing knobs (default 200/200/2000 ms).
    /// Some ECUs are picky about the inter-bit gap.</summary>
    property InitTiming: TKWP1281SerialInitTiming
      read FInitTiming write FInitTiming;
  end;

implementation

{ TKWP1281SerialTransport -----------------------------------------------------}

constructor TKWP1281SerialTransport.Wrap(ASerial: TOBDSerialTransport);
begin
  inherited Create;
  FSerial      := ASerial;
  FOwnsSerial  := False;
  FQueue       := TThreadedQueue<Byte>.Create(4096, INFINITE, 0);
  FInitTiming.BitMs      := 200;
  FInitTiming.StopBitMs  := 200;
  FInitTiming.SyncWaitMs := 2000;
  FSerial.OnDataReceived := HandleBytes;
end;

constructor TKWP1281SerialTransport.Create(ASettings: TOBDSerialSettings);
begin
  inherited Create;
  FSerial     := TOBDSerialTransport.Create;
  FSettings   := ASettings;
  FOwnsSerial := True;
  FQueue      := TThreadedQueue<Byte>.Create(4096, INFINITE, 0);
  FInitTiming.BitMs      := 200;
  FInitTiming.StopBitMs  := 200;
  FInitTiming.SyncWaitMs := 2000;
  FSerial.OnDataReceived := HandleBytes;
end;

destructor TKWP1281SerialTransport.Destroy;
begin
  if FOwnsSerial then
    FSerial.Free;
  FQueue.Free;
  inherited;
end;

procedure TKWP1281SerialTransport.HandleBytes(const ABytes: TBytes);
var I: Integer;
begin
  for I := 0 to Length(ABytes) - 1 do
    FQueue.PushItem(ABytes[I]);
end;

procedure TKWP1281SerialTransport.EnsureOpen;
begin
  if not FSerial.IsOpen then
  begin
    if not FOwnsSerial then
      raise EKWP1281Error.Create(
        'KWP1281SerialTransport: wrapped serial port is not open');
    FSerial.Open(FSettings);
  end;
end;

procedure TKWP1281SerialTransport.DrainQueue;
var Dummy: Byte;
begin
  while FQueue.PopItem(Dummy, 0) = wrSignaled do
    ;
end;

procedure TKWP1281SerialTransport.SendByte(AByte: Byte;
  ATimeoutMs: Integer);
begin
  EnsureOpen;
  FSerial.WriteBytes(TBytes.Create(AByte));
end;

function TKWP1281SerialTransport.ReceiveByte(ATimeoutMs: Integer): Byte;
var Status: TWaitResult;
begin
  EnsureOpen;
  // Re-create queue with the requested timeout per call. Cheaper
  // alternative: keep one queue with INFINITE timeout and
  // implement the wait via a TStopwatch loop.
  Status := FQueue.PopItem(Result, ATimeoutMs);
  if Status <> wrSignaled then
    raise EKWP1281Timeout.CreateFmt(
      'KWP1281SerialTransport: no byte arrived within %d ms',
      [ATimeoutMs]);
end;

procedure TKWP1281SerialTransport.FiveBaudInit(AAddress: Byte;
  out AKW1, AKW2: Byte; ATimeoutMs: Integer);
var
  I:   Integer;
  Bit: Integer;
  Sw:  TDateTime;
  Sync: Byte;
  WaitedMs: Integer;
begin
  EnsureOpen;
  DrainQueue;

  // Start bit: low for BitMs.
  FSerial.SetBreak;
  Sleep(FInitTiming.BitMs);

  // 7 address bits LSB first.
  for I := 0 to 6 do
  begin
    Bit := (AAddress shr I) and 1;
    if Bit = 1 then FSerial.ClearBreak
              else FSerial.SetBreak;
    Sleep(FInitTiming.BitMs);
  end;

  // Stop bit (mark / high) - release the line.
  FSerial.ClearBreak;
  Sleep(FInitTiming.StopBitMs);

  // Drain anything the UART might have synthesized while we were
  // bit-banging (junk bytes from spurious framing).
  DrainQueue;

  // Read sync byte ($55), KW1, KW2.
  WaitedMs := 0;
  Sw := Now;
  while WaitedMs < FInitTiming.SyncWaitMs do
  begin
    if FQueue.PopItem(Sync, 100) = wrSignaled then
    begin
      if Sync = $55 then Break;
      // Some adapters return a $00 framing-error byte first;
      // skip it and keep waiting.
    end;
    WaitedMs := Round((Now - Sw) * 86400 * 1000);
  end;
  if WaitedMs >= FInitTiming.SyncWaitMs then
    raise EKWP1281Timeout.CreateFmt(
      'KWP1281SerialTransport: sync byte $55 not seen within %d ms',
      [FInitTiming.SyncWaitMs]);

  AKW1 := ReceiveByte(ATimeoutMs);
  AKW2 := ReceiveByte(ATimeoutMs);
end;

procedure TKWP1281SerialTransport.Hangup;
begin
  if FOwnsSerial and FSerial.IsOpen then
    FSerial.Close;
end;

end.
