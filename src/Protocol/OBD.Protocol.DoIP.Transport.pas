//------------------------------------------------------------------------------
//  OBD.Protocol.DoIP.Transport
//
//  Transport abstraction for the DoIP client. Two implementations
//  ship in v1:
//
//    TOBDDoIPPlainTransport  — wraps a TOBDConnection (TCP via the
//                              Wi-Fi transport from Phase 2). Used
//                              for unencrypted DoIP-on-port-13400.
//
//    TOBDDoIPOpenSSLTransport — drop-in OpenSSL 3.x plug. Lives in
//                              OBD.Protocol.DoIP.TLS.OpenSSL. Same
//                              IOBDDoIPTransport contract, full TLS
//                              1.2 / 1.3 handshake, used for
//                              port-3496 DoIP-over-TLS.
//
//  Hosts that prefer a different TLS library (Indy / SChannel /
//  custom) implement IOBDDoIPTransport themselves and pass the
//  instance to the client. The client itself never touches a TLS
//  library directly.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.DoIP.Transport;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  System.Net.Socket,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings,
  OBD.Connection;

type
  /// <summary>
  ///   Common transport contract for DoIP. Implemented by both the
  ///   plain and TLS variants.
  /// </summary>
  /// <remarks>
  ///   The client owns the transport lifetime; <c>Disconnect</c>
  ///   must be safe to call multiple times. Implementations are
  ///   thread-safe in the sense that <c>Send</c> and
  ///   <c>Receive</c> may run concurrently from different threads.
  /// </remarks>
  IOBDDoIPTransport = interface
    ['{B8D4F5A7-3E29-4C71-AC42-9F8B1E2D5C7A}']
    /// <summary>Connects to <c>AHost:APort</c> and completes any
    /// handshake. Synchronous.</summary>
    /// <param name="AHost">DNS name or IP.</param>
    /// <param name="APort">TCP port.</param>
    /// <param name="ATimeoutMs">Connect-timeout budget.</param>
    procedure Connect(const AHost: string; APort: Word;
      ATimeoutMs: Cardinal);
    /// <summary>Tears down the socket and any cryptographic state.</summary>
    procedure Disconnect;
    /// <summary>True when the transport is open.</summary>
    function IsConnected: Boolean;
    /// <summary>Sends bytes. Returns the number actually sent.</summary>
    /// <param name="ABytes">Bytes to send.</param>
    /// <returns>Bytes accepted.</returns>
    function Send(const ABytes: TBytes): Integer;
    /// <summary>Reads up to <c>AMaxBytes</c> bytes, blocking for at
    /// most <c>ATimeoutMs</c> milliseconds. Returns the bytes
    /// actually read; empty array means timeout.</summary>
    /// <param name="AMaxBytes">Buffer ceiling.</param>
    /// <param name="ATimeoutMs">Read-timeout budget.</param>
    /// <returns>Received bytes (length may be 0 on timeout).</returns>
    function Receive(AMaxBytes: Integer;
      ATimeoutMs: Cardinal): TBytes;
  end;

  /// <summary>
  ///   Plain (unencrypted) DoIP transport built on the Phase 2
  ///   Wi-Fi connection. Suitable for port 13400 on a trusted LAN.
  /// </summary>
  TOBDDoIPPlainTransport = class(TInterfacedObject, IOBDDoIPTransport)
  strict private
    FConnection: TOBDConnection;
    FOwnsConnection: Boolean;
    FRxLock: TCriticalSection;
    FRxBuffer: TBytes;
    FRxComplete: TEvent;
    procedure HandleRawBytes(Sender: TObject; const ABytes: TBytes);
  public
    /// <summary>Creates a transport that owns its own
    /// <c>TOBDConnection</c> internally.</summary>
    constructor Create; overload;
    /// <summary>Creates a transport that shares an existing
    /// <c>TOBDConnection</c>. The caller owns its lifetime; the
    /// transport will not free it.</summary>
    /// <param name="AConnection">Connection to share. Must not be
    /// <c>nil</c>.</param>
    constructor Create(AConnection: TOBDConnection); overload;
    /// <summary>Disconnects and frees the owned connection (when
    /// applicable).</summary>
    destructor Destroy; override;

    procedure Connect(const AHost: string; APort: Word;
      ATimeoutMs: Cardinal);
    procedure Disconnect;
    function IsConnected: Boolean;
    function Send(const ABytes: TBytes): Integer;
    function Receive(AMaxBytes: Integer;
      ATimeoutMs: Cardinal): TBytes;
  end;

implementation

{ ---- TOBDDoIPPlainTransport -------------------------------------------------- }

constructor TOBDDoIPPlainTransport.Create;
begin
  inherited Create;
  FConnection := TOBDConnection.Create(nil);
  FOwnsConnection := True;
  FRxLock := TCriticalSection.Create;
  FRxComplete := TEvent.Create(nil, True, False, '');
end;

constructor TOBDDoIPPlainTransport.Create(AConnection: TOBDConnection);
begin
  inherited Create;
  if AConnection = nil then
    raise EOBDConfig.Create('TOBDDoIPPlainTransport: AConnection is nil');
  FConnection := AConnection;
  FOwnsConnection := False;
  FRxLock := TCriticalSection.Create;
  FRxComplete := TEvent.Create(nil, True, False, '');
end;

destructor TOBDDoIPPlainTransport.Destroy;
begin
  Disconnect;
  if FOwnsConnection then
    FConnection.Free;
  FRxComplete.Free;
  FRxLock.Free;
  inherited;
end;

procedure TOBDDoIPPlainTransport.HandleRawBytes(Sender: TObject;
  const ABytes: TBytes);
var
  StartLen: Integer;
begin
  if Length(ABytes) = 0 then Exit;
  FRxLock.Enter;
  try
    StartLen := Length(FRxBuffer);
    SetLength(FRxBuffer, StartLen + Length(ABytes));
    Move(ABytes[0], FRxBuffer[StartLen], Length(ABytes));
  finally
    FRxLock.Leave;
  end;
  FRxComplete.SetEvent;
end;

procedure TOBDDoIPPlainTransport.Connect(const AHost: string; APort: Word;
  ATimeoutMs: Cardinal);
begin
  FConnection.Transport := otWiFi;
  FConnection.WiFiSettings.Host := AHost;
  FConnection.WiFiSettings.Port := APort;
  FConnection.WiFiSettings.ConnectTimeout := ATimeoutMs;
  FConnection.WiFiSettings.KeepAlive := True;
  FConnection.OnDataReceivedRaw := HandleRawBytes;
  FConnection.Open;
end;

procedure TOBDDoIPPlainTransport.Disconnect;
begin
  if FConnection = nil then Exit;
  if FConnection.Active then
    FConnection.Close;
end;

function TOBDDoIPPlainTransport.IsConnected: Boolean;
begin
  Result := (FConnection <> nil) and FConnection.Active;
end;

function TOBDDoIPPlainTransport.Send(const ABytes: TBytes): Integer;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP plain transport not connected');
  Result := FConnection.WriteBytes(ABytes);
end;

function TOBDDoIPPlainTransport.Receive(AMaxBytes: Integer;
  ATimeoutMs: Cardinal): TBytes;
var
  Sw: TStopwatch;
  Available: Integer;
  Take: Integer;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP plain transport not connected');
  Sw := TStopwatch.StartNew;
  while True do
  begin
    FRxLock.Enter;
    try
      Available := Length(FRxBuffer);
      if Available > 0 then
      begin
        Take := Available;
        if (AMaxBytes > 0) and (Take > AMaxBytes) then
          Take := AMaxBytes;
        SetLength(Result, Take);
        Move(FRxBuffer[0], Result[0], Take);
        if Take = Available then
        begin
          SetLength(FRxBuffer, 0);
          FRxComplete.ResetEvent;
        end
        else
        begin
          // Shift remaining bytes left.
          Move(FRxBuffer[Take], FRxBuffer[0], Available - Take);
          SetLength(FRxBuffer, Available - Take);
        end;
        Exit;
      end;
    finally
      FRxLock.Leave;
    end;
    if Cardinal(Sw.ElapsedMilliseconds) >= ATimeoutMs then
    begin
      SetLength(Result, 0);
      Exit;
    end;
    FRxComplete.WaitFor(50);
  end;
end;

end.
