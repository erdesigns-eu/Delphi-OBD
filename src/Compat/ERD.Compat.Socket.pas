//------------------------------------------------------------------------------
//  ERD.Compat.Socket
//  IPv4 TCP/UDP sockets for FPC, backed by native socket calls.
//  Author: ERDesigns and Delphi-OBD contributors
//  License: MIT — see LICENSE
//------------------------------------------------------------------------------
unit ERD.Compat.Socket;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
interface
{$IFDEF FPC}
uses SysUtils, Sockets, NetDB, SyncObjs
  {$IFDEF UNIX}, BaseUnix{$ENDIF};
type
  TSocketType = (TCP, UDP);
  TSocketFlag = (WAITALL);
  TSocketFlags = set of TSocketFlag;
  TSocketOption = (Broadcast);
  TIPAddress = record
    IPv4Address: in_addr;
    /// <summary>Resolve an IPv4 literal, hosts-file entry or DNS hostname.</summary>
    class function LookupName(const AHost: string): TIPAddress; static;
    class function Any: TIPAddress; static;
  end;
  TNetEndpoint = record
    Address: in_addr;
    Port: Word;
    class function Create(const AAddress: TIPAddress; APort: Word): TNetEndpoint; overload; static;
    class function Create(const AAddress: in_addr; APort: Word): TNetEndpoint; overload; static;
  end;
  TSocket = class
  private
    FHandle: LongInt;
    function Handle: LongInt;
    function NativeAddress(const AEndpoint: TNetEndpoint): TInetSockAddr;
  public
    constructor Create(AType: TSocketType; AEncoding: TEncoding = nil);
    destructor Destroy; override;
    /// <summary>Connect with a bounded timeout on Unix; close owned sockets before destruction.</summary>
    procedure Connect(const AEndpoint: TNetEndpoint; ATimeoutMs: Cardinal = 10000);
    procedure SetTimeouts(ATimeoutMs: Cardinal);
    /// <summary>Transfer descriptor ownership to the caller (used by OpenSSL).</summary>
    function DetachHandle: LongInt;
    procedure Bind(const AEndpoint: TNetEndpoint);
    procedure SetKeepAlive(AEnabled: Boolean);
    procedure SetSocketOpt(AOption: TSocketOption; AValue: Integer);
    function Send(const ABytes: TBytes): Integer;
    function Receive(var ABytes: TBytes; AOffset, ACount: Integer; AFlags: TSocketFlags): Integer;
    function SendTo(const AEndpoint: TNetEndpoint; const ABytes: TBytes): Integer;
    function ReceiveFrom(var ABytes: TBytes; out AEndpoint: TNetEndpoint;
      AFlags: TSocketFlags; ACount: Integer): Integer;
    procedure Close;
  end;
{$ENDIF}
implementation
{$IFDEF FPC}
procedure CheckSocket(AResult: LongInt);
begin if AResult < 0 then raise EOSError.CreateFmt('Socket error %d', [SocketError]) end;
class function TIPAddress.LookupName(const AHost: string): TIPAddress;
var Entry: THostEntry;
begin
  if TryStrToNetAddr(AHost, Result.IPv4Address) then Exit;
  if not GetHostByName(AHost, Entry) and not ResolveHostByName(AHost, Entry) then
    raise EOSError.Create('Cannot resolve IPv4 host: ' + AHost);
  Result.IPv4Address := HostToNet(Entry.Addr);
end;
class function TIPAddress.Any: TIPAddress;
begin Result.IPv4Address.s_addr := 0 end;
class function TNetEndpoint.Create(const AAddress: TIPAddress; APort: Word): TNetEndpoint;
begin Result := Create(AAddress.IPv4Address, APort) end;
class function TNetEndpoint.Create(const AAddress: in_addr; APort: Word): TNetEndpoint;
begin Result.Address := AAddress; Result.Port := APort end;
constructor TSocket.Create(AType: TSocketType; AEncoding: TEncoding);
begin
  inherited Create;
  FHandle := -1;
  if AType = TCP then FHandle := fpSocket(AF_INET, SOCK_STREAM, 0)
  else FHandle := fpSocket(AF_INET, SOCK_DGRAM, 0);
  CheckSocket(FHandle);
end;
destructor TSocket.Destroy;
begin Close; inherited end;
function TSocket.Handle: LongInt;
begin
  Result := TInterlocked.CompareExchange(FHandle, -1, -1);
  if Result < 0 then raise EOSError.Create('Socket is closed');
end;
function TSocket.NativeAddress(const AEndpoint: TNetEndpoint): TInetSockAddr;
begin
  Result := Default(TInetSockAddr); Result.sin_family := AF_INET;
  Result.sin_port := htons(AEndpoint.Port); Result.sin_addr := AEndpoint.Address;
end;
procedure TSocket.Connect(const AEndpoint: TNetEndpoint; ATimeoutMs: Cardinal);
var Address: TInetSockAddr;
{$IFDEF UNIX}
  FD, Flags, RC, ErrorCode: LongInt;
  WriteSet: TFDSet;
  Time: TTimeVal;
  ErrorSize: TSockLen;
{$ENDIF}
begin
  Address := NativeAddress(AEndpoint);
{$IFDEF UNIX}
  FD := Handle; Flags := fpFcntl(FD, F_GETFL, 0); CheckSocket(Flags);
  CheckSocket(fpFcntl(FD, F_SETFL, Flags or O_NONBLOCK));
  try
    RC := fpConnect(FD, @Address, SizeOf(Address));
    if (RC < 0) and (SocketError <> ESysEINPROGRESS) then CheckSocket(RC);
    if RC < 0 then
    begin
      fpFD_ZERO(WriteSet); fpFD_SET(FD, WriteSet);
      Time.tv_sec := ATimeoutMs div 1000; Time.tv_usec := (ATimeoutMs mod 1000) * 1000;
      RC := fpSelect(FD + 1, nil, @WriteSet, nil, @Time);
      if RC = 0 then raise EOSError.Create('TCP connect timeout');
      CheckSocket(RC);
      ErrorSize := SizeOf(ErrorCode); ErrorCode := 0;
      CheckSocket(fpGetSockOpt(FD, SOL_SOCKET, SO_ERROR, @ErrorCode, @ErrorSize));
      if ErrorCode <> 0 then raise EOSError.CreateFmt('TCP connect failed: %d', [ErrorCode]);
    end;
  finally fpFcntl(FD, F_SETFL, Flags) end;
{$ELSE}
  CheckSocket(fpConnect(Handle, @Address, SizeOf(Address)));
{$ENDIF}
end;
procedure TSocket.SetTimeouts(ATimeoutMs: Cardinal);
{$IFDEF UNIX}var Time: TTimeVal;{$ENDIF}
begin
{$IFDEF UNIX}
  Time.tv_sec := ATimeoutMs div 1000; Time.tv_usec := (ATimeoutMs mod 1000) * 1000;
  CheckSocket(fpSetSockOpt(Handle, SOL_SOCKET, SO_RCVTIMEO, @Time, SizeOf(Time)));
  CheckSocket(fpSetSockOpt(Handle, SOL_SOCKET, SO_SNDTIMEO, @Time, SizeOf(Time)));
{$ELSE}
  CheckSocket(fpSetSockOpt(Handle, SOL_SOCKET, SO_RCVTIMEO, @ATimeoutMs, SizeOf(ATimeoutMs)));
  CheckSocket(fpSetSockOpt(Handle, SOL_SOCKET, SO_SNDTIMEO, @ATimeoutMs, SizeOf(ATimeoutMs)));
{$ENDIF}
end;
function TSocket.DetachHandle: LongInt;
begin
  Result := TInterlocked.Exchange(FHandle, -1);
  if Result < 0 then raise EOSError.Create('Socket is closed');
end;
procedure TSocket.Bind(const AEndpoint: TNetEndpoint);
var Address: TInetSockAddr;
begin Address := NativeAddress(AEndpoint); CheckSocket(fpBind(Handle, @Address, SizeOf(Address))) end;
procedure TSocket.SetKeepAlive(AEnabled: Boolean);
var Enabled: LongInt;
begin Enabled := Ord(AEnabled); CheckSocket(fpSetSockOpt(Handle, SOL_SOCKET, SO_KEEPALIVE, @Enabled, SizeOf(Enabled))) end;
procedure TSocket.SetSocketOpt(AOption: TSocketOption; AValue: Integer);
begin
  case AOption of Broadcast: CheckSocket(fpSetSockOpt(Handle, SOL_SOCKET, SO_BROADCAST, @AValue, SizeOf(AValue))) end;
end;
function TSocket.Send(const ABytes: TBytes): Integer;
begin
  if Length(ABytes) = 0 then Exit(0);
  {$IFDEF UNIX}Result := fpSend(Handle, @ABytes[0], Length(ABytes), MSG_NOSIGNAL);{$ELSE}
  Result := fpSend(Handle, @ABytes[0], Length(ABytes), 0);{$ENDIF}
  CheckSocket(Result);
end;
function TSocket.Receive(var ABytes: TBytes; AOffset, ACount: Integer; AFlags: TSocketFlags): Integer;
var Flags: Integer;
begin
  if (AOffset < 0) or (ACount < 0) or (AOffset > Length(ABytes)) or
    (ACount > Length(ABytes) - AOffset) then raise ERangeError.Create('Invalid socket receive slice');
  if ACount = 0 then Exit(0);
  Flags := 0;
  if WAITALL in AFlags then Flags := MSG_WAITALL;
  Result := fpRecv(Handle, @ABytes[AOffset], ACount, Flags); CheckSocket(Result);
end;
function TSocket.SendTo(const AEndpoint: TNetEndpoint; const ABytes: TBytes): Integer;
var Address: TInetSockAddr; Data: Pointer;
begin
  Address := NativeAddress(AEndpoint); Data := nil;
  if Length(ABytes) > 0 then Data := @ABytes[0];
  Result := fpSendTo(Handle, Data, Length(ABytes), 0, @Address, SizeOf(Address)); CheckSocket(Result);
end;
function TSocket.ReceiveFrom(var ABytes: TBytes; out AEndpoint: TNetEndpoint;
  AFlags: TSocketFlags; ACount: Integer): Integer;
var Address: TInetSockAddr; AddressSize: TSockLen;
begin
  if (ACount < 1) or (ACount > Length(ABytes)) then raise ERangeError.Create('Invalid UDP receive size');
  AddressSize := SizeOf(Address); Address := Default(TInetSockAddr);
  Result := fpRecvFrom(Handle, @ABytes[0], ACount, 0, @Address, @AddressSize); CheckSocket(Result);
  AEndpoint.Address := Address.sin_addr; AEndpoint.Port := ntohs(Address.sin_port);
end;
procedure TSocket.Close;
var Local: LongInt;
begin
  Local := TInterlocked.Exchange(FHandle, -1);
  if Local < 0 then Exit;
  fpShutdown(Local, 2);
  CloseSocket(Local);
end;
{$ENDIF}
end.
