//------------------------------------------------------------------------------
//  OBD.Service.LiveData
//
//  TOBDLiveData — non-visual component that sits on top of a
//  TOBDProtocol and exposes the OBD-II Mode 01 (Show current data)
//  service: support-bitmaps, single-PID reads, periodic polling.
//
//  Owns a small built-in PID decoder dictionary covering the
//  classic SAE J1979 PIDs (RPM, vehicle speed, throttle position,
//  coolant temperature, intake-air temperature, MAF, fuel level,
//  control-module voltage, distance with MIL on, ambient temp,
//  oil temperature). Hosts that need extra PIDs subscribe to
//  <c>OnRaw</c> and decode the bytes themselves.
//
//  Honours the dual-method + main-thread + progress rule:
//  Read / ReadAsync, Poll / PollStop, OnValue / OnRaw / OnError.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1979 (E/E Diagnostic Test Modes, Mode 01 PID catalogue)
//    - ISO 15031-5 (OBD-II diagnostic services)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Service.LiveData;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol,
  OBD.Service.Catalog;

const
  /// <summary>OBD-II Mode 01 — show current data.</summary>
  OBD_MODE_CURRENT_DATA = $01;
  /// <summary>Mode 01 PID 0x00 — bitmap of PIDs 0x01..0x20.</summary>
  OBD_PID_SUPPORTED_01_20 = $00;
  /// <summary>Mode 01 PID 0x20 — bitmap of PIDs 0x21..0x40.</summary>
  OBD_PID_SUPPORTED_21_40 = $20;
  /// <summary>Mode 01 PID 0x40 — bitmap of PIDs 0x41..0x60.</summary>
  OBD_PID_SUPPORTED_41_60 = $40;

type
  /// <summary>Decoded numeric value with engineering units.</summary>
  TOBDPIDValue = record
    /// <summary>PID byte (0x00..0xFF).</summary>
    PID: Byte;
    /// <summary>Decoded scalar value when a built-in decoder
    /// matches; <c>NaN</c> otherwise.</summary>
    Value: Double;
    /// <summary>Engineering unit (e.g. <c>"rpm"</c>, <c>"°C"</c>,
    /// <c>"%"</c>). Empty when no built-in decoder matched.</summary>
    Unit_: string;
    /// <summary>Short human-readable PID description.</summary>
    Description: string;
    /// <summary>Raw value bytes (after the PID echo).</summary>
    Raw: TBytes;
  end;

  /// <summary>Fires for every successful PID read with the decoded
  /// value snapshot.</summary>
  TOBDPIDValueEvent = procedure(Sender: TObject;
    const AValue: TOBDPIDValue) of object;

  /// <summary>Fires alongside <c>OnValue</c> with the raw bytes
  /// before decoding. Useful for unsupported / OEM PIDs.</summary>
  TOBDPIDRawEvent = procedure(Sender: TObject; APID: Byte;
    const ARaw: TBytes) of object;

  /// <summary>
  ///   Mode 01 service component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, point <c>Protocol</c> at a configured
  ///   <c>TOBDProtocol</c>, then call <c>Read(0x0C)</c> for engine
  ///   RPM or <c>ReadAsync(0x0D)</c> for vehicle speed. Use
  ///   <c>SupportedPIDs</c> to discover which PIDs the ECU
  ///   advertises before calling them. <c>Poll</c> drives a list of
  ///   PIDs at a fixed interval until <c>PollStop</c>.
  /// </remarks>
  TOBDLiveData = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FPollIntervalMs: Cardinal;
    FPollPIDs: TBytes;
    FPollLock: TCriticalSection;
    FPollThread: TThread;
    FPollStop: Boolean;

    FOnValue: TOBDPIDValueEvent;
    FOnRaw: TOBDPIDRawEvent;
    FOnError: TOBDConnectionErrorEvent;

    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;

    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoRead(APID: Byte): TOBDPIDValue;
    procedure DispatchValue(const AValue: TOBDPIDValue;
      const ARaw: TBytes);
    procedure FireValue(const AValue: TOBDPIDValue);
    procedure FireRaw(APID: Byte; const ARaw: TBytes);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Creates the component.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops polling and releases state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Reads a single Mode 01 PID synchronously and returns the
    ///   decoded value (or raw bytes when no decoder applies).
    /// </summary>
    /// <param name="APID">Mode 01 PID byte (0x00..0xFF).</param>
    /// <returns>Decoded value snapshot.</returns>
    /// <exception cref="EOBDProtocolErr">Negative response or
    /// short response.</exception>
    function Read(APID: Byte): TOBDPIDValue;

    /// <summary>Non-blocking <see cref="Read"/>. Fires
    /// <c>OnValue</c> on success, <c>OnError</c> on failure.</summary>
    procedure ReadAsync(APID: Byte);

    /// <summary>
    ///   Returns the set of supported Mode 01 PIDs by walking the
    ///   support-bitmap PIDs (0x00 → 0x20 → 0x40 → 0x60 → 0x80 →
    ///   0xA0 → 0xC0 → 0xE0). Stops as soon as a bitmap response
    ///   indicates that no further bitmap is supported.
    /// </summary>
    /// <returns>Sorted PID bytes.</returns>
    function SupportedPIDs: TBytes;

    /// <summary>Starts a background poll over <c>APIDs</c> at
    /// <c>AIntervalMs</c> ms. Each PID read fires <c>OnValue</c>.
    /// Calling <c>Poll</c> a second time replaces the running
    /// poll.</summary>
    /// <param name="APIDs">PID list to cycle through.</param>
    /// <param name="AIntervalMs">Per-cycle delay in ms.</param>
    procedure Poll(const APIDs: TBytes; AIntervalMs: Cardinal);

    /// <summary>Stops the background poll. Joins the worker.</summary>
    procedure PollStop;

    /// <summary>True when a poll is running.</summary>
    function IsPolling: Boolean;
  published
    /// <summary>Bound protocol component.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Most-recently-set poll interval in ms (set by
    /// <c>Poll</c>).</summary>
    property PollIntervalMs: Cardinal read FPollIntervalMs;

    /// <summary>Fires for each successful read with the decoded
    /// value (main thread).</summary>
    property OnValue: TOBDPIDValueEvent read FOnValue write FOnValue;
    /// <summary>Fires alongside <c>OnValue</c> with raw bytes
    /// (main thread).</summary>
    property OnRaw: TOBDPIDRawEvent read FOnRaw write FOnRaw;
    /// <summary>Fires for transient I/O errors (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  System.Math;

{ ---- built-in PID decoders -------------------------------------------------- }

function DecodePID(APID: Byte; const A: TBytes;
  out AValue: Double; out AUnit, ADescription: string): Boolean;
begin
  Result := True;
  AValue := NaN;
  AUnit := '';
  ADescription := '';
  case APID of
    $04: // calculated engine load
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0] * 100.0 / 255.0;
      AUnit := '%'; ADescription := 'Engine load';
    end;
    $05: // coolant temperature
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0] - 40;
      AUnit := '°C'; ADescription := 'Engine coolant temperature';
    end;
    $0A: // fuel pressure
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0] * 3;
      AUnit := 'kPa'; ADescription := 'Fuel pressure';
    end;
    $0B: // intake manifold pressure
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0];
      AUnit := 'kPa'; ADescription := 'Intake manifold pressure';
    end;
    $0C: // engine RPM
    begin
      if Length(A) < 2 then Exit(False);
      AValue := ((A[0] * 256) + A[1]) / 4.0;
      AUnit := 'rpm'; ADescription := 'Engine RPM';
    end;
    $0D: // vehicle speed
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0];
      AUnit := 'km/h'; ADescription := 'Vehicle speed';
    end;
    $0E: // timing advance
    begin
      if Length(A) < 1 then Exit(False);
      AValue := (A[0] / 2.0) - 64.0;
      AUnit := '°'; ADescription := 'Timing advance';
    end;
    $0F: // intake air temperature
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0] - 40;
      AUnit := '°C'; ADescription := 'Intake air temperature';
    end;
    $10: // MAF air flow rate
    begin
      if Length(A) < 2 then Exit(False);
      AValue := ((A[0] * 256) + A[1]) / 100.0;
      AUnit := 'g/s'; ADescription := 'Mass air flow';
    end;
    $11: // throttle position
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0] * 100.0 / 255.0;
      AUnit := '%'; ADescription := 'Throttle position';
    end;
    $1F: // run time since engine start
    begin
      if Length(A) < 2 then Exit(False);
      AValue := (A[0] * 256) + A[1];
      AUnit := 's'; ADescription := 'Run time since engine start';
    end;
    $21: // distance with MIL on
    begin
      if Length(A) < 2 then Exit(False);
      AValue := (A[0] * 256) + A[1];
      AUnit := 'km'; ADescription := 'Distance with MIL on';
    end;
    $2F: // fuel tank level
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0] * 100.0 / 255.0;
      AUnit := '%'; ADescription := 'Fuel tank level';
    end;
    $33: // barometric pressure
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0];
      AUnit := 'kPa'; ADescription := 'Barometric pressure';
    end;
    $42: // control module voltage
    begin
      if Length(A) < 2 then Exit(False);
      AValue := ((A[0] * 256) + A[1]) / 1000.0;
      AUnit := 'V'; ADescription := 'Control module voltage';
    end;
    $46: // ambient air temperature
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0] - 40;
      AUnit := '°C'; ADescription := 'Ambient air temperature';
    end;
    $5C: // engine oil temperature
    begin
      if Length(A) < 1 then Exit(False);
      AValue := A[0] - 40;
      AUnit := '°C'; ADescription := 'Engine oil temperature';
    end;
    $5E: // engine fuel rate
    begin
      if Length(A) < 2 then Exit(False);
      AValue := ((A[0] * 256) + A[1]) * 0.05;
      AUnit := 'L/h'; ADescription := 'Engine fuel rate';
    end;
  else
    Result := False;
  end;
end;

{ ---- TOBDLiveData ----------------------------------------------------------- }

constructor TOBDLiveData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPollLock := TCriticalSection.Create;
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDLiveData.Destroy;
begin
  PollStop;
  FAsyncLock.Free;
  FPollLock.Free;
  inherited;
end;

procedure TOBDLiveData.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDLiveData: async read already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDLiveData.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDLiveData.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDLiveData.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

function TOBDLiveData.DoRead(APID: Byte): TOBDPIDValue;
var
  Resp: TOBDResponse;
  Raw: TBytes;
  V: Double;
  Unit_, Desc: string;
  N: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDLiveData: Protocol not assigned');
  Resp := FProtocol.Request(OBD_MODE_CURRENT_DATA, TBytes.Create(APID));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'Mode 01 PID 0x%2.2X negative: %s', [APID, Resp.NRCText]);

  // Response Data carries [PID echo, value bytes...]. Some adapters
  // strip the PID echo on broadcast IDs; tolerate either layout.
  N := Length(Resp.Data);
  if (N >= 1) and (Resp.Data[0] = APID) then
  begin
    SetLength(Raw, N - 1);
    if N > 1 then Move(Resp.Data[1], Raw[0], N - 1);
  end
  else
    Raw := Copy(Resp.Data, 0, N);

  Result := Default(TOBDPIDValue);
  Result.PID := APID;
  Result.Raw := Raw;
  Result.Value := NaN;

  // Catalog first (JSON-driven). Fall back to the hand-coded
  // dictionary so a host that has not loaded a catalogue still
  // gets the J1979 classics.
  var CatInfo: TOBDPIDInfo;
  if TOBDServiceCatalog.Default.TryGetPID(APID, CatInfo) then
  begin
    Result.Description := CatInfo.Description;
    if CatInfo.Description = '' then Result.Description := CatInfo.Name;
    Result.Unit_ := CatInfo.Decoder.Unit_;
    if EvaluatePIDDecoder(CatInfo.Decoder, Raw, V) then
      Result.Value := V;
  end
  else if DecodePID(APID, Raw, V, Unit_, Desc) then
  begin
    Result.Value := V;
    Result.Unit_ := Unit_;
    Result.Description := Desc;
  end;
end;

function TOBDLiveData.Read(APID: Byte): TOBDPIDValue;
begin
  Result := DoRead(APID);
  DispatchValue(Result, Result.Raw);
end;

procedure TOBDLiveData.ReadAsync(APID: Byte);
var
  Self_: TOBDLiveData;
  PIDCopy: Byte;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDLiveData: Protocol not assigned');
  GuardSingleAsync;
  Self_ := Self;
  PIDCopy := APID;
  TThread.CreateAnonymousThread(
    procedure
    var
      V: TOBDPIDValue;
    begin
      try
        try
          V := Self_.DoRead(PIDCopy);
          Self_.DispatchValue(V, V.Raw);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDLiveData.DispatchValue(const AValue: TOBDPIDValue;
  const ARaw: TBytes);
begin
  FireValue(AValue);
  FireRaw(AValue.PID, ARaw);
end;

function TOBDLiveData.SupportedPIDs: TBytes;
const
  Bases: array[0..7] of Byte = ($00, $20, $40, $60, $80, $A0, $C0, $E0);
var
  List: TList<Byte>;
  Resp: TOBDResponse;
  I, J: Integer;
  Base: Byte;
  HasNextBitmap: Boolean;
  Bitmap: Cardinal;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDLiveData: Protocol not assigned');
  List := TList<Byte>.Create;
  try
    for I := 0 to High(Bases) do
    begin
      Base := Bases[I];
      Resp := FProtocol.Request(OBD_MODE_CURRENT_DATA, TBytes.Create(Base));
      if Resp.IsNegative then Break;
      // Expect [PID echo, 4 bytes bitmap] or just 4 bytes.
      if (Length(Resp.Data) >= 5) and (Resp.Data[0] = Base) then
        Bitmap := (Cardinal(Resp.Data[1]) shl 24) or
                  (Cardinal(Resp.Data[2]) shl 16) or
                  (Cardinal(Resp.Data[3]) shl 8) or
                  Cardinal(Resp.Data[4])
      else if Length(Resp.Data) >= 4 then
        Bitmap := (Cardinal(Resp.Data[0]) shl 24) or
                  (Cardinal(Resp.Data[1]) shl 16) or
                  (Cardinal(Resp.Data[2]) shl 8) or
                  Cardinal(Resp.Data[3])
      else
        Break;

      // Bit 31 = PID Base+1, bit 0 = PID Base+0x20.
      for J := 0 to 31 do
        if (Bitmap and (Cardinal(1) shl (31 - J))) <> 0 then
          List.Add(Base + Byte(J + 1));

      // The lowest bit of each bitmap signals "next bitmap supported".
      HasNextBitmap := (Bitmap and 1) <> 0;
      if not HasNextBitmap then Break;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

procedure TOBDLiveData.Poll(const APIDs: TBytes; AIntervalMs: Cardinal);
var
  Self_: TOBDLiveData;
  PIDCopy: TBytes;
  Interval: Cardinal;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDLiveData: Protocol not assigned');
  if Length(APIDs) = 0 then
    raise EOBDConfig.Create('TOBDLiveData.Poll: empty PID list');
  PollStop;
  FPollPIDs := Copy(APIDs, 0, Length(APIDs));
  FPollIntervalMs := AIntervalMs;
  FPollStop := False;
  Self_ := Self;
  PIDCopy := FPollPIDs;
  Interval := AIntervalMs;
  FPollThread := TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
      V: TOBDPIDValue;
    begin
      while not Self_.FPollStop do
      begin
        for I := 0 to High(PIDCopy) do
        begin
          if Self_.FPollStop then Break;
          try
            V := Self_.DoRead(PIDCopy[I]);
            Self_.DispatchValue(V, V.Raw);
          except
            on E: Exception do
              Self_.FireError(oeIO, E.Message);
          end;
        end;
        if Self_.FPollStop then Break;
        Sleep(Interval);
      end;
    end);
  FPollThread.FreeOnTerminate := False;
  FPollThread.Start;
end;

procedure TOBDLiveData.PollStop;
var
  T: TThread;
begin
  FPollStop := True;
  T := FPollThread;
  FPollThread := nil;
  if T <> nil then
  begin
    T.WaitFor;
    T.Free;
  end;
end;

function TOBDLiveData.IsPolling: Boolean;
begin
  Result := FPollThread <> nil;
end;

procedure TOBDLiveData.FireValue(const AValue: TOBDPIDValue);
var
  Self_: TOBDLiveData;
  Snap: TOBDPIDValue;
begin
  if not Assigned(FOnValue) then Exit;
  Self_ := Self; Snap := AValue;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnValue(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnValue) then Self_.FOnValue(Self_, Snap);
    end);
end;

procedure TOBDLiveData.FireRaw(APID: Byte; const ARaw: TBytes);
var
  Self_: TOBDLiveData; PID: Byte; Snap: TBytes;
begin
  if not Assigned(FOnRaw) then Exit;
  Self_ := Self; PID := APID; Snap := Copy(ARaw, 0, Length(ARaw));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRaw(Self_, PID, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnRaw) then Self_.FOnRaw(Self_, PID, Snap);
    end);
end;

procedure TOBDLiveData.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDLiveData; Code: TOBDErrorCode; Msg: string;
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
