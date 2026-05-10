//------------------------------------------------------------------------------
//  OBD.Service.VIN
//
//  TOBDVIN — non-visual component that reads the Vehicle
//  Identification Number from the ECU. Supports both:
//
//    - OBD-II Service 09 PID 02 (legacy / OBD-II path)
//    - UDS Service 22 DID 0xF190 (UDS / KWP path)
//
//  Validates the result with <c>TOBDVINValidator</c> (ISO 3779
//  check digit). Hosts that
//  prefer to disable the check digit (older / non-NA vehicles)
//  can read <c>RawVIN</c> instead of <c>VIN</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1979 Service 09 PID 02
//    - ISO 14229-1 § 11.4 (UDS ReadDataByIdentifier)
//    - ISO 3779 (VIN check-digit)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Service.VIN;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol.VIN,
  OBD.Protocol,
  OBD.Service.VINDecoder.Types,
  OBD.Service.VINDecoder;

const
  /// <summary>OBD-II Service 09 — Request Vehicle Information.</summary>
  OBD_MODE_VEHICLE_INFO = $09;
  /// <summary>Service 09 PID 0x02 — VIN message count + VIN.</summary>
  OBD_PID_VIN           = $02;
  /// <summary>UDS DID for the Vehicle Identification Number
  /// (ISO 14229-1 § 11.4.7).</summary>
  UDS_DID_VIN           = $F190;

type
  /// <summary>Selector for the read path.</summary>
  TOBDVINSource = (
    /// <summary>OBD-II Service 09 PID 02.</summary>
    vsOBDII,
    /// <summary>UDS Service 22 DID 0xF190.</summary>
    vsUDS
  );

  /// <summary>VIN read result.</summary>
  TOBDVINResult = record
    /// <summary>VIN string as received (whitespace stripped).</summary>
    RawVIN: string;
    /// <summary>True when ISO 3779 check-digit verifies.</summary>
    Valid: Boolean;
    /// <summary>Read source.</summary>
    Source: TOBDVINSource;
  end;

  /// <summary>Fires when a VIN read completes.</summary>
  TOBDVINEvent = procedure(Sender: TObject;
    const AResult: TOBDVINResult) of object;

  /// <summary>VIN service component.</summary>
  TOBDVIN = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnVIN: TOBDVINEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;

    function ReadOBDII: TOBDVINResult;
    function ReadUDS: TOBDVINResult;
    procedure FireVIN(const AResult: TOBDVINResult);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads the VIN synchronously via the chosen
    /// source.</summary>
    /// <param name="ASource">Read path (default OBD-II).</param>
    function Read(ASource: TOBDVINSource = vsOBDII): TOBDVINResult;
    /// <summary>Non-blocking <see cref="Read"/>.</summary>
    procedure ReadAsync(ASource: TOBDVINSource = vsOBDII);
    /// <summary>Reads the VIN and immediately runs it through
    /// <see cref="OBD.Service.VINDecoder.TOBDVINDecoder.Decode"/>.
    /// Convenience wrapper for hosts that want the full WMI / VDS
    /// / VIS breakdown alongside the raw read.</summary>
    function ReadAndDecode(
      ASource: TOBDVINSource = vsOBDII): TOBDVINInfo;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires for each successful read (main thread).</summary>
    property OnVIN: TOBDVINEvent read FOnVIN write FOnVIN;
    /// <summary>Fires for transient I/O errors (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDVIN.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDVIN.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDVIN.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDVIN: async read already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDVIN.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDVIN.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDVIN.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

function TOBDVIN.ReadOBDII: TOBDVINResult;
var
  Resp: TOBDResponse;
  Vin: string;
  Bytes: TBytes;
  Start: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDVIN: Protocol not assigned');
  Resp := FProtocol.Request(OBD_MODE_VEHICLE_INFO, TBytes.Create(OBD_PID_VIN));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'Service 09 PID 02 negative: %s', [Resp.NRCText]);
  // Response Data: <PID echo 0x02> <NODI / message count> <VIN ASCII…>
  // Some controllers strip the PID echo; some return only the VIN.
  Bytes := Resp.Data;
  if (Length(Bytes) >= 2) and (Bytes[0] = OBD_PID_VIN) then
    Start := 2  // skip PID echo + message count
  else if Length(Bytes) >= 1 then
    Start := 1
  else
    Start := 0;
  if Length(Bytes) > Start then
  begin
    SetLength(Bytes, Length(Bytes));
    Vin := TEncoding.ASCII.GetString(
      Copy(Bytes, Start, Length(Bytes) - Start));
  end
  else
    Vin := '';
  Result.RawVIN := Trim(Vin);
  Result.Source := vsOBDII;
  Result.Valid  := TOBDVINValidator.IsValid(Result.RawVIN);
end;

function TOBDVIN.ReadUDS: TOBDVINResult;
var
  Resp: TOBDResponse;
  Bytes: TBytes;
  Vin: string;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDVIN: Protocol not assigned');
  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier,
    TBytes.Create(Hi(UDS_DID_VIN), Lo(UDS_DID_VIN)));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'UDS RDBI 0xF190 negative: %s', [Resp.NRCText]);
  // Response Data: <DID HI> <DID LO> <VIN ASCII…>
  Bytes := Resp.Data;
  if Length(Bytes) >= 2 then
    Vin := TEncoding.ASCII.GetString(Copy(Bytes, 2, Length(Bytes) - 2))
  else
    Vin := '';
  Result.RawVIN := Trim(Vin);
  Result.Source := vsUDS;
  Result.Valid  := TOBDVINValidator.IsValid(Result.RawVIN);
end;

function TOBDVIN.Read(ASource: TOBDVINSource): TOBDVINResult;
begin
  case ASource of
    vsUDS:    Result := ReadUDS;
  else
    Result := ReadOBDII;
  end;
  FireVIN(Result);
end;

function TOBDVIN.ReadAndDecode(ASource: TOBDVINSource): TOBDVINInfo;
var
  Raw: TOBDVINResult;
begin
  Raw := Read(ASource);
  Result := TOBDVINDecoder.Decode(Raw.RawVIN);
end;

procedure TOBDVIN.ReadAsync(ASource: TOBDVINSource);
var
  Self_: TOBDVIN;
  Source: TOBDVINSource;
begin
  GuardSingleAsync;
  Self_ := Self; Source := ASource;
  TThread.CreateAnonymousThread(
    procedure
    var
      R: TOBDVINResult;
    begin
      try
        try
          case Source of
            vsUDS:    R := Self_.ReadUDS;
          else
            R := Self_.ReadOBDII;
          end;
          Self_.FireVIN(R);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDVIN.FireVIN(const AResult: TOBDVINResult);
var
  Self_: TOBDVIN;
  Snap: TOBDVINResult;
begin
  if not Assigned(FOnVIN) then Exit;
  Self_ := Self; Snap := AResult;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnVIN(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnVIN) then Self_.FOnVIN(Self_, Snap);
    end);
end;

procedure TOBDVIN.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDVIN; Code: TOBDErrorCode; Msg: string;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil, procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
