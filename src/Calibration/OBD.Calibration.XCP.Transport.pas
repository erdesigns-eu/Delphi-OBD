//------------------------------------------------------------------------------
//  OBD.Calibration.XCP.Transport
//
//  XCP transport abstraction. ASAM MCD-1 XCP rides on top of CAN,
//  CAN-FD, Ethernet (TCP / UDP), FlexRay, USB and others; the wire
//  framing differs but the application-layer packets are identical.
//  TOBDXCP talks to one IOBDXCPTransport and never touches a
//  network or driver directly.
//
//  Hosts plug their own CAN driver (Vector / PEAK / Kvaser /
//  SocketCAN) by implementing this interface in 100 lines or so.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ASAM MCD-1 XCP v1.7 §1.2 (Transport-layer separation)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Calibration.XCP.Transport;

interface

uses
  System.SysUtils,
  OBD.Types;

type
  /// <summary>
  ///   Transport contract for the XCP master. Implementations cover
  ///   the wire framing (CAN ID, Ethernet header, etc.) and expose
  ///   raw XCP packets to the master.
  /// </summary>
  IOBDXCPTransport = interface
    ['{2C8E1F35-AA64-4B1A-9D17-08F3D2E5A1B0}']
    /// <summary>Connects (configures CAN IDs / opens a TCP socket /
    /// claims a USB endpoint). Implementations may no-op when the
    /// underlying driver is connected externally.</summary>
    procedure Connect;
    /// <summary>Disconnects.</summary>
    procedure Disconnect;
    /// <summary>True when the transport is ready.</summary>
    function IsConnected: Boolean;
    /// <summary>Sends one XCP packet (CTO / DTO). Length is up to
    /// the transport's MAX_CTO / MAX_DTO; on CAN classic that's 8
    /// bytes.</summary>
    procedure SendPacket(const ABytes: TBytes);
    /// <summary>Reads one packet, blocking up to <c>ATimeoutMs</c>
    /// milliseconds. Returns an empty array on timeout.</summary>
    function ReceivePacket(ATimeoutMs: Cardinal): TBytes;
  end;

implementation

end.
