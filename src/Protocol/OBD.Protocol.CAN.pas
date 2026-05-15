//------------------------------------------------------------------------------
//  OBD.Protocol.CAN
//
//  Minimal CAN-frame transport contract used by every higher-
//  level transport that needs to exchange raw CAN frames
//  (TP2.0, ISO-TP, J1939, J2534, ...). Concrete CAN drivers
//  (J2534 device, SocketCAN, PCAN, Vector XL, ...) implement
//  this interface; protocol layers consume it.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Protocol.CAN;

interface

uses
  System.SysUtils,
  OBD.Protocol.Types;

type
  EOBDCANError   = class(Exception);
  EOBDCANTimeout = class(EOBDCANError);

  /// <summary>Direction of a CAN-frame I/O operation. Used by
  /// loggers and by hosts that want to label frames by source.</summary>
  TCANDirection = (cdTx, cdRx);

  /// <summary>Optional callback fired on every frame the host
  /// transmits or receives via this transport. Hosts use it
  /// for trace logging and live displays.</summary>
  TCANFrameEvent = procedure(Sender: TObject;
    ADirection: TCANDirection; const AFrame: TOBDFrame) of object;

  /// <summary>CAN-bus transport contract. Concrete drivers
  /// (J2534, SocketCAN, PCAN, Vector, ...) implement this; the
  /// TP2.0 / ISO-TP / J1939 layers consume it.</summary>
  ICANTransport = interface
    ['{1F8B7A02-3D6A-4E0C-9B7C-E7B92F3D5E10}']

    /// <summary>Sends one CAN frame. Blocks until the frame is
    /// queued for transmission (not necessarily ack'd on the
    /// bus). Raises EOBDCANTimeout if the TX queue stays full.</summary>
    procedure SendFrame(const AFrame: TOBDFrame; ATimeoutMs: Integer);

    /// <summary>Receives one CAN frame matching one of the IDs
    /// the host has installed via SetAcceptanceFilter. Raises
    /// EOBDCANTimeout if no frame arrives.</summary>
    function  ReceiveFrame(ATimeoutMs: Integer): TOBDFrame;

    /// <summary>Replaces the current acceptance-filter set with
    /// AIds. Empty array = receive everything (promiscuous).</summary>
    procedure SetAcceptanceFilter(const AIds: TArray<Cardinal>;
                                  AExtended: Boolean = False);

    /// <summary>Drops everything in the RX buffer.</summary>
    procedure DrainRx;

    /// <summary>Optional trace hook. Implementations should fire
    /// it for both TX and RX frames. nil = no tracing.</summary>
    function  GetOnFrame: TCANFrameEvent;
    procedure SetOnFrame(const AValue: TCANFrameEvent);
    property  OnFrame: TCANFrameEvent
      read GetOnFrame write SetOnFrame;
  end;

implementation

end.
