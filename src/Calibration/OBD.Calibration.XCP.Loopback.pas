//------------------------------------------------------------------------------
//  OBD.Calibration.XCP.Loopback
//
//  TOBDXCPLoopbackTransport — reference implementation of
//  IOBDXCPTransport that ships in-process, useful for:
//
//    1. Wiring up tests for code that drives a TOBDXCP master.
//    2. Hosts that talk to an in-process simulated slave (e.g. a
//       host-side ECU model running in the same Delphi binary).
//    3. Demonstrating the contract every host CAN driver
//       implementation must satisfy.
//
//  The master sends packets via SendPacket; the host plug
//  (typically a stub slave running in another thread) calls
//  PostFromSlave to enqueue a response packet back to the master.
//  Both directions are blocking up to a host-supplied timeout.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit OBD.Calibration.XCP.Loopback;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Calibration.XCP.Transport;

type
  /// <summary>Fires when the master sends a packet — gives the
  /// host a chance to compose a response synchronously without
  /// shipping a second thread.</summary>
  TOBDXCPLoopbackEvent = procedure(Sender: TObject;
    const APacket: TBytes) of object;

  /// <summary>
  ///   In-process IOBDXCPTransport implementation. Two FIFOs +
  ///   two events: one for outbound (master → slave), one for
  ///   inbound (slave → master).
  /// </summary>
  TOBDXCPLoopbackTransport = class(TInterfacedObject, IOBDXCPTransport)
  strict private
    FConnected: Boolean;
    FLock: TCriticalSection;
    FInbound: TQueue<TBytes>;
    FInboundEvent: TEvent;
    FOnPacketSent: TOBDXCPLoopbackEvent;
  public
    constructor Create;
    destructor Destroy; override;

    // --- IOBDXCPTransport ---
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    procedure SendPacket(const ABytes: TBytes);
    function ReceivePacket(ATimeoutMs: Cardinal): TBytes;

    /// <summary>Pushes a packet from the simulated slave back to
    /// the master. Wakes a blocked <c>ReceivePacket</c>.</summary>
    procedure PostFromSlave(const APacket: TBytes);

    /// <summary>Number of pending master-side reads.</summary>
    function PendingCount: Integer;

    /// <summary>Fires (synchronously, on the calling thread) every
    /// time the master sends a packet. Hosts use this to compose a
    /// response inline without a worker thread.</summary>
    property OnPacketSent: TOBDXCPLoopbackEvent
      read FOnPacketSent write FOnPacketSent;
  end;

implementation

constructor TOBDXCPLoopbackTransport.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FInbound := TQueue<TBytes>.Create;
  FInboundEvent := TEvent.Create(nil, True, False, '');
end;

destructor TOBDXCPLoopbackTransport.Destroy;
begin
  FInboundEvent.Free;
  FInbound.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDXCPLoopbackTransport.Connect;
begin
  FConnected := True;
end;

procedure TOBDXCPLoopbackTransport.Disconnect;
begin
  FConnected := False;
  FInboundEvent.SetEvent;
end;

function TOBDXCPLoopbackTransport.IsConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TOBDXCPLoopbackTransport.SendPacket(const ABytes: TBytes);
var
  Snap: TBytes;
begin
  Snap := Copy(ABytes, 0, Length(ABytes));
  if Assigned(FOnPacketSent) then
    FOnPacketSent(Self, Snap);
end;

function TOBDXCPLoopbackTransport.ReceivePacket(
  ATimeoutMs: Cardinal): TBytes;
begin
  FLock.Enter;
  try
    if FInbound.Count > 0 then
    begin
      Result := FInbound.Dequeue;
      if FInbound.Count = 0 then FInboundEvent.ResetEvent;
      Exit;
    end;
  finally
    FLock.Leave;
  end;
  if FInboundEvent.WaitFor(ATimeoutMs) <> wrSignaled then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  FLock.Enter;
  try
    if FInbound.Count > 0 then
      Result := FInbound.Dequeue
    else
      SetLength(Result, 0);
    if FInbound.Count = 0 then FInboundEvent.ResetEvent;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDXCPLoopbackTransport.PostFromSlave(const APacket: TBytes);
var
  Snap: TBytes;
begin
  Snap := Copy(APacket, 0, Length(APacket));
  FLock.Enter;
  try
    FInbound.Enqueue(Snap);
  finally
    FLock.Leave;
  end;
  FInboundEvent.SetEvent;
end;

function TOBDXCPLoopbackTransport.PendingCount: Integer;
begin
  FLock.Enter;
  try Result := FInbound.Count;
  finally FLock.Leave; end;
end;

end.
