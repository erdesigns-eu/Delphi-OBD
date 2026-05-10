//------------------------------------------------------------------------------
//  OBD.Service.FreezeFrame
//
//  TOBDFreezeFrame — non-visual component that reads OBD-II Mode 02
//  freeze-frame data. Mode 02 mirrors Mode 01 but every request
//  carries a frame index byte after the PID. Index 0 is the most
//  recent stored snapshot.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1979 Mode 02 (Freeze frame data)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Service.FreezeFrame;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Data.Bind.Components,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol,
  OBD.Service.LiveData;

const
  /// <summary>OBD-II Mode 02 — Freeze frame data.</summary>
  OBD_MODE_FREEZE_FRAME = $02;

type
  /// <summary>Fires when a freeze-frame PID read completes.</summary>
  TOBDFreezeFrameEvent = procedure(Sender: TObject;
    AFrameIndex: Byte; const AValue: TOBDPIDValue) of object;

  /// <summary>Mode 02 freeze-frame service component.</summary>
  TOBDFreezeFrame = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnValue: TOBDFreezeFrameEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoRead(APID: Byte; AFrameIndex: Byte): TOBDPIDValue;
    procedure FireValue(AFrameIndex: Byte; const AValue: TOBDPIDValue);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads <c>APID</c> from freeze-frame
    /// <c>AFrameIndex</c>.</summary>
    /// <param name="APID">Mode 01 PID byte.</param>
    /// <param name="AFrameIndex">Frame index (0 = most recent).</param>
    function Read(APID: Byte;
      AFrameIndex: Byte = 0): TOBDPIDValue;
    /// <summary>Non-blocking <see cref="Read"/>.</summary>
    procedure ReadAsync(APID: Byte; AFrameIndex: Byte = 0);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    property OnValue: TOBDFreezeFrameEvent read FOnValue write FOnValue;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  System.Math;

constructor TOBDFreezeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDFreezeFrame.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDFreezeFrame.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDFreezeFrame: async read already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDFreezeFrame.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDFreezeFrame.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDFreezeFrame.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

// LiveData's built-in PID decoders are private to that unit; keep
// the freeze-frame variant raw and let the host decode via
// TOBDLiveData.Read on the same PID after a snapshot. The Mode 02
// payload shape is identical to Mode 01, so a host that wants the
// engineering value can copy the Raw bytes into a fresh
// TOBDPIDValue and feed them through their preferred decoder.

function TOBDFreezeFrame.DoRead(APID: Byte;
  AFrameIndex: Byte): TOBDPIDValue;
var
  Resp: TOBDResponse;
  Raw: TBytes;
  N: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDFreezeFrame: Protocol not assigned');
  Resp := FProtocol.Request(OBD_MODE_FREEZE_FRAME,
    TBytes.Create(APID, AFrameIndex));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'Mode 02 PID 0x%2.2X (frame %d) negative: %s',
      [APID, AFrameIndex, Resp.NRCText]);

  // Response Data: [PID echo, frame index echo, value bytes…].
  N := Length(Resp.Data);
  Result := Default(TOBDPIDValue);
  Result.PID := APID;
  Result.Value := NaN;
  if (N >= 2) and (Resp.Data[0] = APID) and (Resp.Data[1] = AFrameIndex) then
  begin
    SetLength(Raw, N - 2);
    if N > 2 then Move(Resp.Data[2], Raw[0], N - 2);
  end
  else
    Raw := Copy(Resp.Data, 0, N);
  Result.Raw := Raw;
end;

function TOBDFreezeFrame.Read(APID, AFrameIndex: Byte): TOBDPIDValue;
begin
  Result := DoRead(APID, AFrameIndex);
  FireValue(AFrameIndex, Result);
end;

procedure TOBDFreezeFrame.ReadAsync(APID, AFrameIndex: Byte);
var
  Self_: TOBDFreezeFrame;
  PID, Frame: Byte;
begin
  GuardSingleAsync;
  Self_ := Self; PID := APID; Frame := AFrameIndex;
  TThread.CreateAnonymousThread(
    procedure
    var
      V: TOBDPIDValue;
    begin
      try
        try
          V := Self_.DoRead(PID, Frame);
          Self_.FireValue(Frame, V);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDFreezeFrame.FireValue(AFrameIndex: Byte;
  const AValue: TOBDPIDValue);
var
  Self_: TOBDFreezeFrame;
  Frame: Byte;
  Snap: TOBDPIDValue;
begin
  Self_ := Self; Frame := AFrameIndex; Snap := AValue;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    try
      TBindings.Notify(Self_, '');
    except
    end;
    if Assigned(FOnValue) then FOnValue(Self_, Frame, Snap);
  end
  else
    TThread.Queue(nil, procedure begin
      try
        TBindings.Notify(Self_, '');
      except
      end;
      if Assigned(Self_.FOnValue) then Self_.FOnValue(Self_, Frame, Snap);
    end);
end;

procedure TOBDFreezeFrame.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDFreezeFrame; Code: TOBDErrorCode; Msg: string;
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
