//------------------------------------------------------------------------------
//  OBD.Calibration.CCP
//
//  TOBDCCP — non-visual ASAP1a CCP (CAN Calibration Protocol) master.
//
//  CCP is the legacy predecessor of XCP and lives only on classic
//  CAN. Each command is 8 bytes: PID + counter + 6 parameter bytes.
//  Responses are also 8 bytes and carry a 2-byte CRO/CRM counter
//  echo for matching.
//
//  v1 covers:
//
//    - CONNECT / DISCONNECT
//    - GET_CCP_VERSION
//    - EXCHANGE_ID
//    - SET_MTA / DNLOAD / UPLOAD
//    - GET_SEED / UNLOCK
//    - SELECT_CAL_PAGE
//    - DTM_SET_DAQ_PTR / DTM_WRITE_DAQ / DTM_START_STOP
//
//  Like the XCP master, CCP rides on top of an IOBDXCPTransport
//  implementation. CCP doesn't need an XCP-specific contract — the
//  same raw-byte send/receive surface works.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ASAP1a CCP v2.1 (CAN Calibration Protocol)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Calibration.CCP;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Calibration.XCP.Transport;

const
  // ---- CCP command codes ----
  CCP_CMD_CONNECT          = $01;
  CCP_CMD_GET_CCP_VERSION  = $1B;
  CCP_CMD_EXCHANGE_ID      = $17;
  CCP_CMD_GET_SEED         = $12;
  CCP_CMD_UNLOCK           = $13;
  CCP_CMD_SET_MTA          = $02;
  CCP_CMD_DNLOAD           = $03;
  CCP_CMD_UPLOAD           = $04;
  CCP_CMD_DISCONNECT       = $07;
  CCP_CMD_SELECT_CAL_PAGE  = $11;
  CCP_CMD_GET_DAQ_SIZE     = $14;
  CCP_CMD_SET_DAQ_PTR      = $15;
  CCP_CMD_WRITE_DAQ        = $16;
  CCP_CMD_START_STOP       = $06;
  CCP_CMD_START_STOP_ALL   = $08;

  // CCP packet IDs in the slave response.
  CCP_RES_OK    = $FF;
  CCP_RES_EVENT = $FE;
  CCP_RES_DATA  = $FD;

type
  /// <summary>CCP master component.</summary>
  TOBDCCP = class(TComponent)
  strict private
    FTransport: IOBDXCPTransport;
    FStationAddress: Word;
    FCounter: Byte;
    FConnected: Boolean;
    FDefaultTimeoutMs: Cardinal;
    function MakePacket(ACmd: Byte; const AArgs: TBytes): TBytes;
    function Exchange(ACmd: Byte; const AArgs: TBytes): TBytes;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>Connects to the slave at <c>AStationAddress</c>.</summary>
    procedure Connect(AStationAddress: Word);
    /// <summary>Disconnects (temporary).</summary>
    procedure Disconnect(APermanent: Boolean = False);
    /// <summary>True when connected.</summary>
    function IsConnected: Boolean;

    /// <summary>Reads CCP version (Major, Minor).</summary>
    procedure GetVersion(out AMajor, AMinor: Byte);
    /// <summary>Exchanges Master-Slave Device IDs. Returns the
    /// slave's device ID byte count.</summary>
    function ExchangeID(const AMasterID: TBytes): Byte;

    /// <summary>Issues GET_SEED for the chosen resource (0..255 vendor-defined).</summary>
    function GetSeed(AResource: Byte): TBytes;
    /// <summary>Sends UNLOCK with the host-computed key.</summary>
    procedure Unlock(const AKey: TBytes);

    /// <summary>SET_MTA — pointer setup for subsequent
    /// UPLOAD / DNLOAD.</summary>
    procedure SetMTA(AMtaNumber, AAddressExt: Byte; AAddress: UInt32);
    /// <summary>Reads up to 5 bytes from MTA0.</summary>
    function Upload(ALen: Byte): TBytes;
    /// <summary>Writes up to 5 bytes to MTA0.</summary>
    procedure Download(const AData: TBytes);

    /// <summary>Selects a calibration page (vendor-defined
    /// MTA-relative).</summary>
    procedure SelectCalPage;

    /// <summary>Starts or stops a DAQ list.</summary>
    procedure StartStop(AMode, ADaqList, ALastODT, AEventChan: Byte;
      APrescaler: Word);

    /// <summary>GET_DAQ_SIZE — returns the size of the requested
    /// DAQ list (number of ODTs) and resets the DAQ pointer.</summary>
    function GetDAQSize(ADaqList: Byte; ACanID: UInt32;
      out AFirstPID: Byte): Byte;
    /// <summary>SET_DAQ_PTR — points the cursor at
    /// <c>(ADaqList, AOdt, AOdtElement)</c>.</summary>
    procedure SetDAQPtr(ADaqList, AOdt, AOdtElement: Byte);
    /// <summary>WRITE_DAQ — writes the current ODT element with
    /// (Size, AddressExt, Address).</summary>
    procedure WriteDAQ(ASize, AAddressExt: Byte; AAddress: UInt32);
    /// <summary>START_STOP_ALL — synchronous start / stop of every
    /// DAQ list at once.</summary>
    procedure StartStopAll(AStart: Boolean);

    property Transport: IOBDXCPTransport read FTransport write FTransport;
    /// <summary>Currently connected station address.</summary>
    property StationAddress: Word read FStationAddress;
  published
    property DefaultTimeoutMs: Cardinal read FDefaultTimeoutMs
      write FDefaultTimeoutMs default 1000;
  end;

implementation

constructor TOBDCCP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultTimeoutMs := 1000;
end;

function TOBDCCP.MakePacket(ACmd: Byte; const AArgs: TBytes): TBytes;
var
  N: Integer;
begin
  SetLength(Result, 8);
  Result[0] := ACmd;
  Result[1] := FCounter;
  Inc(FCounter); // CCP wraps naturally
  N := Length(AArgs);
  if N > 6 then N := 6;
  if N > 0 then Move(AArgs[0], Result[2], N);
end;

function TOBDCCP.Exchange(ACmd: Byte; const AArgs: TBytes): TBytes;
var
  Pkt: TBytes;
  Effective: Cardinal;
begin
  if FTransport = nil then
    raise EOBDConfig.Create('TOBDCCP: Transport not assigned');
  Pkt := MakePacket(ACmd, AArgs);
  Effective := FDefaultTimeoutMs;
  FTransport.SendPacket(Pkt);
  Result := FTransport.ReceivePacket(Effective);
  if Length(Result) < 3 then
    raise EOBDProtocolErr.Create('CCP: short response');
  if Result[0] <> CCP_RES_OK then
    raise EOBDProtocolErr.CreateFmt('CCP: PID 0x%2.2X', [Result[0]]);
  // Result[1] = command return code; Result[2] = counter echo.
  if Result[1] <> $00 then
    raise EOBDProtocolErr.CreateFmt('CCP: error code 0x%2.2X', [Result[1]]);
end;

procedure TOBDCCP.Connect(AStationAddress: Word);
var
  Args: TBytes;
begin
  if FTransport = nil then
    raise EOBDConfig.Create('TOBDCCP: Transport not assigned');
  if not FTransport.IsConnected then FTransport.Connect;
  // Args: stationAddrLo, stationAddrHi, padding x4 (CCP §6.1.2).
  SetLength(Args, 6);
  Args[0] := Byte(AStationAddress and $FF);
  Args[1] := Byte((AStationAddress shr 8) and $FF);
  Exchange(CCP_CMD_CONNECT, Args);
  FStationAddress := AStationAddress;
  FConnected := True;
end;

procedure TOBDCCP.Disconnect(APermanent: Boolean);
var
  Args: TBytes;
begin
  if not FConnected then Exit;
  SetLength(Args, 4);
  if APermanent then Args[0] := $00 else Args[0] := $01;
  Args[1] := $00;
  Args[2] := Byte(FStationAddress and $FF);
  Args[3] := Byte((FStationAddress shr 8) and $FF);
  try
    Exchange(CCP_CMD_DISCONNECT, Args);
  except
    // best-effort
  end;
  FConnected := False;
  if FTransport <> nil then FTransport.Disconnect;
end;

function TOBDCCP.IsConnected: Boolean;
begin
  Result := FConnected and (FTransport <> nil) and FTransport.IsConnected;
end;

procedure TOBDCCP.GetVersion(out AMajor, AMinor: Byte);
var
  Args, Resp: TBytes;
begin
  SetLength(Args, 2);
  Args[0] := $02; // CCP version major
  Args[1] := $01; // CCP version minor
  Resp := Exchange(CCP_CMD_GET_CCP_VERSION, Args);
  if Length(Resp) < 5 then
    raise EOBDProtocolErr.Create('CCP GET_CCP_VERSION: short response');
  AMajor := Resp[3];
  AMinor := Resp[4];
end;

function TOBDCCP.ExchangeID(const AMasterID: TBytes): Byte;
var
  Args, Resp: TBytes;
  N: Integer;
begin
  SetLength(Args, 6);
  N := Length(AMasterID); if N > 6 then N := 6;
  if N > 0 then Move(AMasterID[0], Args[0], N);
  Resp := Exchange(CCP_CMD_EXCHANGE_ID, Args);
  if Length(Resp) < 4 then
    raise EOBDProtocolErr.Create('CCP EXCHANGE_ID: short response');
  Result := Resp[3]; // length of slave device ID
end;

function TOBDCCP.GetSeed(AResource: Byte): TBytes;
var
  Args, Resp: TBytes;
  N: Integer;
begin
  SetLength(Args, 1);
  Args[0] := AResource;
  Resp := Exchange(CCP_CMD_GET_SEED, Args);
  // Response: PID, ERR, CTR, ProtStatus, seed[0..3]
  if Length(Resp) < 4 then
    raise EOBDProtocolErr.Create('CCP GET_SEED: short response');
  N := Length(Resp) - 4;
  if N < 0 then N := 0;
  SetLength(Result, N);
  if N > 0 then Move(Resp[4], Result[0], N);
end;

procedure TOBDCCP.Unlock(const AKey: TBytes);
var
  Args: TBytes;
  N: Integer;
begin
  N := Length(AKey); if N > 6 then N := 6;
  SetLength(Args, N);
  if N > 0 then Move(AKey[0], Args[0], N);
  Exchange(CCP_CMD_UNLOCK, Args);
end;

procedure TOBDCCP.SetMTA(AMtaNumber, AAddressExt: Byte; AAddress: UInt32);
var
  Args: TBytes;
begin
  SetLength(Args, 6);
  Args[0] := AMtaNumber;
  Args[1] := AAddressExt;
  // Big-endian per CCP spec.
  Args[2] := Byte((AAddress shr 24) and $FF);
  Args[3] := Byte((AAddress shr 16) and $FF);
  Args[4] := Byte((AAddress shr 8) and $FF);
  Args[5] := Byte(AAddress and $FF);
  Exchange(CCP_CMD_SET_MTA, Args);
end;

function TOBDCCP.Upload(ALen: Byte): TBytes;
var
  Args, Resp: TBytes;
begin
  if (ALen = 0) or (ALen > 5) then
    raise EOBDConfig.Create('CCP UPLOAD: length must be 1..5');
  SetLength(Args, 1);
  Args[0] := ALen;
  Resp := Exchange(CCP_CMD_UPLOAD, Args);
  if Length(Resp) < 3 + ALen then
    raise EOBDProtocolErr.Create('CCP UPLOAD: short response');
  SetLength(Result, ALen);
  Move(Resp[3], Result[0], ALen);
end;

procedure TOBDCCP.Download(const AData: TBytes);
var
  Args: TBytes;
  N: Integer;
begin
  N := Length(AData);
  if (N = 0) or (N > 5) then
    raise EOBDConfig.Create('CCP DNLOAD: length must be 1..5');
  SetLength(Args, 1 + N);
  Args[0] := Byte(N);
  Move(AData[0], Args[1], N);
  Exchange(CCP_CMD_DNLOAD, Args);
end;

procedure TOBDCCP.SelectCalPage;
begin
  Exchange(CCP_CMD_SELECT_CAL_PAGE, nil);
end;

procedure TOBDCCP.StartStop(AMode, ADaqList, ALastODT, AEventChan: Byte;
  APrescaler: Word);
var
  Args: TBytes;
begin
  SetLength(Args, 6);
  Args[0] := AMode;
  Args[1] := ADaqList;
  Args[2] := ALastODT;
  Args[3] := AEventChan;
  Args[4] := Byte((APrescaler shr 8) and $FF);
  Args[5] := Byte(APrescaler and $FF);
  Exchange(CCP_CMD_START_STOP, Args);
end;

function TOBDCCP.GetDAQSize(ADaqList: Byte; ACanID: UInt32;
  out AFirstPID: Byte): Byte;
var
  Args, Resp: TBytes;
begin
  SetLength(Args, 6);
  Args[0] := ADaqList;
  Args[1] := 0;
  Args[2] := Byte((ACanID shr 24) and $FF);
  Args[3] := Byte((ACanID shr 16) and $FF);
  Args[4] := Byte((ACanID shr 8)  and $FF);
  Args[5] := Byte(ACanID and $FF);
  Resp := Exchange(CCP_CMD_GET_DAQ_SIZE, Args);
  // Resp: PID, ERR, CTR, DAQListSize, FirstPID
  if Length(Resp) < 5 then
    raise EOBDProtocolErr.Create('CCP GET_DAQ_SIZE: short response');
  Result := Resp[3];
  AFirstPID := Resp[4];
end;

procedure TOBDCCP.SetDAQPtr(ADaqList, AOdt, AOdtElement: Byte);
var
  Args: TBytes;
begin
  SetLength(Args, 3);
  Args[0] := ADaqList;
  Args[1] := AOdt;
  Args[2] := AOdtElement;
  Exchange(CCP_CMD_SET_DAQ_PTR, Args);
end;

procedure TOBDCCP.WriteDAQ(ASize, AAddressExt: Byte; AAddress: UInt32);
var
  Args: TBytes;
begin
  SetLength(Args, 6);
  Args[0] := ASize;
  Args[1] := AAddressExt;
  Args[2] := Byte((AAddress shr 24) and $FF);
  Args[3] := Byte((AAddress shr 16) and $FF);
  Args[4] := Byte((AAddress shr 8)  and $FF);
  Args[5] := Byte(AAddress and $FF);
  Exchange(CCP_CMD_WRITE_DAQ, Args);
end;

procedure TOBDCCP.StartStopAll(AStart: Boolean);
var
  Args: TBytes;
begin
  SetLength(Args, 1);
  if AStart then Args[0] := $01 else Args[0] := $00;
  Exchange(CCP_CMD_START_STOP_ALL, Args);
end;

end.
