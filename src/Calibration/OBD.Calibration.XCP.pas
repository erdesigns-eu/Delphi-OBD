//------------------------------------------------------------------------------
//  OBD.Calibration.XCP
//
//  TOBDXCP — non-visual ASAM MCD-1 XCP master. Speaks the XCP
//  application-layer command set on top of any IOBDXCPTransport
//  (CAN, CAN-FD, Ethernet, FlexRay, USB).
//
//  v1 covers the standard set every measurement / calibration host
//  needs:
//
//    - CONNECT / DISCONNECT / GET_STATUS / GET_COMM_MODE_INFO
//    - GET_ID (ASCII strings: vendor, comment, A2L filename, ...)
//    - GET_SEED / UNLOCK (resource-protected XCP)
//    - SET_MTA / UPLOAD / SHORT_UPLOAD / DOWNLOAD / SHORT_DOWNLOAD
//    - SET_CAL_PAGE / GET_CAL_PAGE
//    - START_STOP_DAQ_LIST  (begin / stop streamed measurement)
//
//  Sync only: each XCP command is one packet round-trip. A future
//  follow-up adds an async wrapper for hosts that don't want to
//  block.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ASAM MCD-1 XCP v1.7 § 4 (Protocol Layer)
//
//  History     :
//    2026-05-09  ERD  Phase 7 initial.
//------------------------------------------------------------------------------

unit OBD.Calibration.XCP;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Calibration.XCP.Transport;

const
  // ---- Master command codes (PID, ASAM MCD-1 XCP §4.1) ----
  XCP_CMD_CONNECT             = $FF;
  XCP_CMD_DISCONNECT          = $FE;
  XCP_CMD_GET_STATUS          = $FD;
  XCP_CMD_SYNCH               = $FC;
  XCP_CMD_GET_COMM_MODE_INFO  = $FB;
  XCP_CMD_GET_ID              = $FA;
  XCP_CMD_SET_REQUEST         = $F9;
  XCP_CMD_GET_SEED            = $F8;
  XCP_CMD_UNLOCK              = $F7;
  XCP_CMD_SET_MTA             = $F6;
  XCP_CMD_UPLOAD              = $F5;
  XCP_CMD_SHORT_UPLOAD        = $F4;
  XCP_CMD_BUILD_CHECKSUM      = $F3;
  XCP_CMD_DOWNLOAD            = $F0;
  XCP_CMD_SHORT_DOWNLOAD      = $ED;
  XCP_CMD_SET_CAL_PAGE        = $EB;
  XCP_CMD_GET_CAL_PAGE        = $EA;
  XCP_CMD_GET_DAQ_PROCESSOR_INFO = $DA;
  XCP_CMD_START_STOP_DAQ_LIST = $DE;
  XCP_CMD_START_STOP_SYNCH    = $DD;

  // ---- Slave response PIDs ----
  XCP_RES_OK    = $FF;
  XCP_RES_ERR   = $FE;
  XCP_RES_EVENT = $FD;
  XCP_RES_SERV  = $FC;

  // ---- Resource bits returned by CONNECT ----
  XCP_RESOURCE_DAQ  = $04;
  XCP_RESOURCE_STIM = $08;
  XCP_RESOURCE_CAL  = $01;
  XCP_RESOURCE_PGM  = $10;

  // ---- START_STOP_DAQ_LIST modes ----
  XCP_DAQ_STOP   = $00;
  XCP_DAQ_START  = $01;
  XCP_DAQ_SELECT = $02;

  // ---- GET_ID types ----
  XCP_ID_ASCII             = $00;
  XCP_ID_ASAM_FILENAME     = $01;
  XCP_ID_ASAM_PATH         = $02;
  XCP_ID_ASAM_URL          = $03;
  XCP_ID_FILE_TO_UPLOAD    = $04;

type
  /// <summary>Result of a CONNECT command.</summary>
  TOBDXCPConnectInfo = record
    Resource: Byte;            // bitmap of resources advertised
    CommModeBasic: Byte;
    MaxCTO: Byte;              // max command bytes
    MaxDTO: Word;              // max DAQ bytes
    ProtocolVersion: Byte;
    TransportVersion: Byte;
    /// <summary>True when the slave reports ByteOrder = MotorolaMSB.</summary>
    BigEndian: Boolean;
  end;

  /// <summary>Result of GET_STATUS.</summary>
  TOBDXCPStatus = record
    SessionStatus: Byte;
    ResourceProtection: Byte;
    SessionConfigurationID: Word;
  end;

  /// <summary>
  ///   Stateful XCP master. Holds a transport reference and tracks
  ///   the connection state. Single-threaded by design — the
  ///   caller serialises commands.
  /// </summary>
  TOBDXCP = class(TComponent)
  strict private
    FTransport: IOBDXCPTransport;
    FConnected: Boolean;
    FBigEndian: Boolean;
    FDefaultTimeoutMs: Cardinal;
    FMaxCTO: Byte;
    FMaxDTO: Word;
    procedure SendCommand(const ABytes: TBytes);
    function Exchange(const ABytes: TBytes;
      ATimeoutMs: Cardinal = 0): TBytes;
    procedure WriteAddrPacket(var AOut: TBytes; AOffset: Integer;
      AAddress: UInt32; AExtension: Byte);
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>Establishes an XCP session with the slave. Returns
    /// the slave's CONNECT response.</summary>
    /// <exception cref="EOBDProtocolErr">Slave returned ERR or
    /// transport timed out.</exception>
    function Connect: TOBDXCPConnectInfo;
    /// <summary>Closes the XCP session.</summary>
    procedure Disconnect;
    /// <summary>True when the master has an active session.</summary>
    function IsConnected: Boolean;
    /// <summary>Reads slave session status.</summary>
    function GetStatus: TOBDXCPStatus;
    /// <summary>Reads an identification string of the chosen kind
    /// (XCP_ID_*). The slave returns the length first; the master
    /// then runs UPLOAD to pull the body.</summary>
    function GetID(AIDKind: Byte = XCP_ID_ASCII): string;

    /// <summary>Initiates a seed/key unlock for one or more
    /// resources. The host computes the key from the seed and
    /// passes it back via <c>Unlock</c>.</summary>
    /// <param name="AResource">Resource mask; first call sets
    /// modes and returns the seed.</param>
    function GetSeed(AResource: Byte): TBytes;
    /// <summary>Sends the computed key back to the slave; returns
    /// the resource-protection bitmap on success.</summary>
    function Unlock(AResource: Byte; const AKey: TBytes): Byte;

    /// <summary>Sets the Memory Transfer Address. Required before
    /// UPLOAD / DOWNLOAD that don't carry the address inline.</summary>
    procedure SetMTA(AAddress: UInt32; AExtension: Byte = 0);
    /// <summary>Reads <c>ALen</c> bytes from MTA and returns them.</summary>
    function Upload(ALen: Byte): TBytes;
    /// <summary>Reads <c>ALen</c> bytes starting at the inline-
    /// supplied address (no SET_MTA round-trip).</summary>
    function ShortUpload(AAddress: UInt32; AExtension: Byte;
      ALen: Byte): TBytes;
    /// <summary>Writes <c>AData</c> to MTA. Single packet — the
    /// caller chunks if length exceeds MAX_CTO.</summary>
    procedure Download(const AData: TBytes);
    /// <summary>Inline-address single-packet write.</summary>
    procedure ShortDownload(AAddress: UInt32; AExtension: Byte;
      const AData: TBytes);

    /// <summary>Sets the active calibration page on a logical
    /// segment / page index for the chosen access mode.</summary>
    procedure SetCalPage(ALogicalSegment, ALogicalPage,
      AMode: Byte);
    /// <summary>Reads the active calibration page.</summary>
    function GetCalPage(ALogicalSegment, AMode: Byte): Byte;

    /// <summary>Starts, stops, or selects DAQ measurement.</summary>
    procedure StartStopDAQList(AMode, ADaqList: Byte);
    /// <summary>Stops every DAQ list at once.</summary>
    procedure StopAllDAQ;

    /// <summary>Bound transport. Set before <c>Connect</c>.</summary>
    property Transport: IOBDXCPTransport read FTransport write FTransport;
    /// <summary>True when the slave reports Motorola byte order.</summary>
    property BigEndian: Boolean read FBigEndian;
    /// <summary>Maximum command-packet bytes reported by the slave.
    /// Valid only after a successful CONNECT.</summary>
    property MaxCTO: Byte read FMaxCTO;
    /// <summary>Maximum DAQ packet bytes reported by the slave.</summary>
    property MaxDTO: Word read FMaxDTO;
  published
    /// <summary>Default round-trip timeout in ms. Default 1000.</summary>
    property DefaultTimeoutMs: Cardinal read FDefaultTimeoutMs
      write FDefaultTimeoutMs default 1000;
  end;

implementation

constructor TOBDXCP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultTimeoutMs := 1000;
end;

procedure TOBDXCP.SendCommand(const ABytes: TBytes);
begin
  if FTransport = nil then
    raise EOBDConfig.Create('TOBDXCP: Transport not assigned');
  FTransport.SendPacket(ABytes);
end;

function TOBDXCP.Exchange(const ABytes: TBytes;
  ATimeoutMs: Cardinal): TBytes;
var
  Effective: Cardinal;
begin
  if FTransport = nil then
    raise EOBDConfig.Create('TOBDXCP: Transport not assigned');
  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FDefaultTimeoutMs;
  FTransport.SendPacket(ABytes);
  Result := FTransport.ReceivePacket(Effective);
  if Length(Result) = 0 then
    raise EOBDProtocolErr.Create('XCP: response timeout');
  if Result[0] = XCP_RES_ERR then
    raise EOBDProtocolErr.CreateFmt('XCP: ERR 0x%2.2X',
      [Result[1]]);
  if Result[0] <> XCP_RES_OK then
    raise EOBDProtocolErr.CreateFmt(
      'XCP: unexpected PID 0x%2.2X', [Result[0]]);
end;

procedure TOBDXCP.WriteAddrPacket(var AOut: TBytes; AOffset: Integer;
  AAddress: UInt32; AExtension: Byte);
begin
  // Address bytes use the slave's byte order.
  if FBigEndian then
  begin
    AOut[AOffset    ] := Byte((AAddress shr 24) and $FF);
    AOut[AOffset + 1] := Byte((AAddress shr 16) and $FF);
    AOut[AOffset + 2] := Byte((AAddress shr  8) and $FF);
    AOut[AOffset + 3] := Byte(AAddress and $FF);
  end
  else
  begin
    AOut[AOffset    ] := Byte(AAddress and $FF);
    AOut[AOffset + 1] := Byte((AAddress shr  8) and $FF);
    AOut[AOffset + 2] := Byte((AAddress shr 16) and $FF);
    AOut[AOffset + 3] := Byte((AAddress shr 24) and $FF);
  end;
end;

function TOBDXCP.Connect: TOBDXCPConnectInfo;
var
  Cmd, Resp: TBytes;
begin
  if FTransport = nil then
    raise EOBDConfig.Create('TOBDXCP: Transport not assigned');
  if not FTransport.IsConnected then FTransport.Connect;
  SetLength(Cmd, 2);
  Cmd[0] := XCP_CMD_CONNECT;
  Cmd[1] := $00; // mode = NORMAL
  Resp := Exchange(Cmd);
  if Length(Resp) < 8 then
    raise EOBDProtocolErr.Create('XCP CONNECT: short response');
  Result.Resource         := Resp[1];
  Result.CommModeBasic    := Resp[2];
  Result.MaxCTO           := Resp[3];
  Result.BigEndian        := (Result.CommModeBasic and $01) <> 0;
  // MAX_DTO is encoded in the slave's byte order from the start.
  // CONNECT always returns it in network order though — § 4.1.1.
  Result.MaxDTO           := (Word(Resp[4]) shl 8) or Resp[5];
  Result.ProtocolVersion  := Resp[6];
  Result.TransportVersion := Resp[7];

  FConnected := True;
  FBigEndian := Result.BigEndian;
  FMaxCTO    := Result.MaxCTO;
  FMaxDTO    := Result.MaxDTO;
end;

procedure TOBDXCP.Disconnect;
var
  Cmd: TBytes;
begin
  if not FConnected then Exit;
  SetLength(Cmd, 1);
  Cmd[0] := XCP_CMD_DISCONNECT;
  try
    Exchange(Cmd);
  except
    // Best-effort close; swallow.
  end;
  FConnected := False;
  if FTransport <> nil then FTransport.Disconnect;
end;

function TOBDXCP.IsConnected: Boolean;
begin
  Result := FConnected and (FTransport <> nil) and FTransport.IsConnected;
end;

function TOBDXCP.GetStatus: TOBDXCPStatus;
var
  Cmd, Resp: TBytes;
begin
  SetLength(Cmd, 1);
  Cmd[0] := XCP_CMD_GET_STATUS;
  Resp := Exchange(Cmd);
  if Length(Resp) < 6 then
    raise EOBDProtocolErr.Create('XCP GET_STATUS: short response');
  Result.SessionStatus      := Resp[1];
  Result.ResourceProtection := Resp[2];
  if FBigEndian then
    Result.SessionConfigurationID := (Word(Resp[4]) shl 8) or Resp[5]
  else
    Result.SessionConfigurationID := (Word(Resp[5]) shl 8) or Resp[4];
end;

function TOBDXCP.GetID(AIDKind: Byte): string;
var
  Cmd, Resp, Body: TBytes;
  TextLen: UInt32;
begin
  SetLength(Cmd, 2);
  Cmd[0] := XCP_CMD_GET_ID;
  Cmd[1] := AIDKind;
  Resp := Exchange(Cmd);
  if Length(Resp) < 8 then
    raise EOBDProtocolErr.Create('XCP GET_ID: short response');
  // Bytes 4..7 carry the length in slave byte order.
  if FBigEndian then
    TextLen := (Cardinal(Resp[4]) shl 24) or (Cardinal(Resp[5]) shl 16) or
               (Cardinal(Resp[6]) shl 8) or Cardinal(Resp[7])
  else
    TextLen := (Cardinal(Resp[7]) shl 24) or (Cardinal(Resp[6]) shl 16) or
               (Cardinal(Resp[5]) shl 8) or Cardinal(Resp[4]);
  if TextLen = 0 then Exit('');

  // Pull the body via UPLOAD chunks. SET_MTA is implicit when the
  // slave returned the data inline (Mode = TRANSFER_MODE_INLINE).
  // For simplicity we always run UPLOAD; slaves that returned the
  // text inline will reject UPLOAD, in which case the host should
  // call ShortUpload at the address embedded in the GET_ID response.
  // §4.1.7 leaves either path valid.
  Body := Upload(Byte(TextLen));
  Result := TEncoding.ASCII.GetString(Body);
end;

function TOBDXCP.GetSeed(AResource: Byte): TBytes;
var
  Cmd, Resp: TBytes;
  SeedLen: Byte;
begin
  SetLength(Cmd, 3);
  Cmd[0] := XCP_CMD_GET_SEED;
  Cmd[1] := $00;        // mode = first call
  Cmd[2] := AResource;
  Resp := Exchange(Cmd);
  if Length(Resp) < 2 then
    raise EOBDProtocolErr.Create('XCP GET_SEED: short response');
  SeedLen := Resp[1];
  SetLength(Result, SeedLen);
  if SeedLen > 0 then
    Move(Resp[2], Result[0], SeedLen);
end;

function TOBDXCP.Unlock(AResource: Byte; const AKey: TBytes): Byte;
var
  Cmd, Resp: TBytes;
begin
  if Length(AKey) = 0 then
    raise EOBDConfig.Create('XCP UNLOCK: empty key');
  SetLength(Cmd, 2 + Length(AKey));
  Cmd[0] := XCP_CMD_UNLOCK;
  Cmd[1] := Byte(Length(AKey));
  Move(AKey[0], Cmd[2], Length(AKey));
  Resp := Exchange(Cmd);
  if Length(Resp) < 2 then
    raise EOBDProtocolErr.Create('XCP UNLOCK: short response');
  Result := Resp[1]; // current resource protection bitmap
  if AResource = 0 then ; // suppress unused-warning when a host omits it
end;

procedure TOBDXCP.SetMTA(AAddress: UInt32; AExtension: Byte);
var
  Cmd: TBytes;
begin
  SetLength(Cmd, 8);
  Cmd[0] := XCP_CMD_SET_MTA;
  Cmd[1] := 0;
  Cmd[2] := 0;
  Cmd[3] := AExtension;
  WriteAddrPacket(Cmd, 4, AAddress, AExtension);
  Exchange(Cmd);
end;

function TOBDXCP.Upload(ALen: Byte): TBytes;
var
  Cmd, Resp: TBytes;
begin
  SetLength(Cmd, 2);
  Cmd[0] := XCP_CMD_UPLOAD;
  Cmd[1] := ALen;
  Resp := Exchange(Cmd);
  if Length(Resp) < 1 + ALen then
    raise EOBDProtocolErr.CreateFmt(
      'XCP UPLOAD: expected %d data bytes, got %d', [ALen, Length(Resp) - 1]);
  SetLength(Result, ALen);
  if ALen > 0 then
    Move(Resp[1], Result[0], ALen);
end;

function TOBDXCP.ShortUpload(AAddress: UInt32; AExtension: Byte;
  ALen: Byte): TBytes;
var
  Cmd, Resp: TBytes;
begin
  SetLength(Cmd, 8);
  Cmd[0] := XCP_CMD_SHORT_UPLOAD;
  Cmd[1] := ALen;
  Cmd[2] := 0;
  Cmd[3] := AExtension;
  WriteAddrPacket(Cmd, 4, AAddress, AExtension);
  Resp := Exchange(Cmd);
  if Length(Resp) < 1 + ALen then
    raise EOBDProtocolErr.Create('XCP SHORT_UPLOAD: short response');
  SetLength(Result, ALen);
  if ALen > 0 then
    Move(Resp[1], Result[0], ALen);
end;

procedure TOBDXCP.Download(const AData: TBytes);
var
  Cmd: TBytes;
  N: Integer;
begin
  N := Length(AData);
  if N = 0 then Exit;
  SetLength(Cmd, 2 + N);
  Cmd[0] := XCP_CMD_DOWNLOAD;
  Cmd[1] := Byte(N);
  Move(AData[0], Cmd[2], N);
  Exchange(Cmd);
end;

procedure TOBDXCP.ShortDownload(AAddress: UInt32; AExtension: Byte;
  const AData: TBytes);
var
  Cmd: TBytes;
  N: Integer;
begin
  N := Length(AData);
  if N = 0 then Exit;
  SetLength(Cmd, 8 + N);
  Cmd[0] := XCP_CMD_SHORT_DOWNLOAD;
  Cmd[1] := Byte(N);
  Cmd[2] := 0;
  Cmd[3] := AExtension;
  WriteAddrPacket(Cmd, 4, AAddress, AExtension);
  Move(AData[0], Cmd[8], N);
  Exchange(Cmd);
end;

procedure TOBDXCP.SetCalPage(ALogicalSegment, ALogicalPage,
  AMode: Byte);
var
  Cmd: TBytes;
begin
  SetLength(Cmd, 4);
  Cmd[0] := XCP_CMD_SET_CAL_PAGE;
  Cmd[1] := AMode;
  Cmd[2] := ALogicalSegment;
  Cmd[3] := ALogicalPage;
  Exchange(Cmd);
end;

function TOBDXCP.GetCalPage(ALogicalSegment, AMode: Byte): Byte;
var
  Cmd, Resp: TBytes;
begin
  SetLength(Cmd, 4);
  Cmd[0] := XCP_CMD_GET_CAL_PAGE;
  Cmd[1] := AMode;
  Cmd[2] := ALogicalSegment;
  Cmd[3] := 0;
  Resp := Exchange(Cmd);
  if Length(Resp) < 4 then
    raise EOBDProtocolErr.Create('XCP GET_CAL_PAGE: short response');
  Result := Resp[3];
end;

procedure TOBDXCP.StartStopDAQList(AMode, ADaqList: Byte);
var
  Cmd: TBytes;
begin
  SetLength(Cmd, 4);
  Cmd[0] := XCP_CMD_START_STOP_DAQ_LIST;
  Cmd[1] := AMode;
  if FBigEndian then
  begin
    Cmd[2] := 0;
    Cmd[3] := ADaqList;
  end
  else
  begin
    Cmd[2] := ADaqList;
    Cmd[3] := 0;
  end;
  Exchange(Cmd);
end;

procedure TOBDXCP.StopAllDAQ;
var
  Cmd: TBytes;
begin
  SetLength(Cmd, 2);
  Cmd[0] := XCP_CMD_START_STOP_SYNCH;
  Cmd[1] := XCP_DAQ_STOP;
  Exchange(Cmd);
end;

end.
