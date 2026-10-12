//------------------------------------------------------------------------------
//  untSession
//
//  The vehicle session of OBD-Studio Personal: one place that owns the
//  Delphi-OBD chain (connection or demo car -> adapter -> protocol ->
//  services) and runs the diagnostic work in the background.
//
//  Threading: pages never call a blocking service method on the main
//  thread. They hand the work to RunJob; the jobs run one after the other
//  on a single worker thread (the adapter talks to one car at a time) and
//  the Done callback runs on the main thread with an empty error text or
//  the message of the exception. Disconnect cancels what is in flight
//  and drops the jobs that are still waiting. Live polling (LiveData.Poll,
//  CSV logging) runs on the library's own threads; the adapter serialises
//  the exchanges, so polling and jobs can overlap safely.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untSession;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  ERD.Connection,
  ERD.Adapter,
  ERD.Protocol,
  ERD.Simulator.ELM327,
  ERD.Service.LiveData,
  ERD.Service.DTCs,
  ERD.ClearDTC,
  ERD.Service.FreezeFrame,
  ERD.Service.VIN,
  ERD.Service.VehicleInfo,
  ERD.Service.OnBoardMonitor,
  ERD.Service.DriveCycle,
  ERD.Service.VehicleHealth,
  ERD.WWHOBD,
  ERD.Service.EVBattery,
  ERD.Service.Inspection.Component,
  ERD.Recorder,
  ERD.Recorder.CSV,
  untSettings;

type
  /// <summary>State of the link with the car.</summary>
  TPersonalLinkState = (
    /// <summary>No adapter open.</summary>
    plsOffline,
    /// <summary>Opening the adapter, identifying it and finding the
    /// vehicle protocol.</summary>
    plsConnecting,
    /// <summary>Connected; services can be used.</summary>
    plsOnline,
    /// <summary>The last connect attempt failed (see LastError).</summary>
    plsFailed);

  /// <summary>Main-thread callback of a job: AError is empty on success.</summary>
  TPersonalJobDone = reference to procedure(const AError: string);

  TPersonalJobThread = class;

  /// <summary>Owns the Delphi-OBD components of one vehicle session.</summary>
  TPersonalSession = class(TComponent)
  strict private
    FSerial: TOBDConnection;
    FDemo: TOBDEmulatorConnection;
    FAdapter: TOBDAdapter;
    FProtocol: TOBDProtocol;
    FLiveData: TOBDLiveData;
    FDTCs: TOBDDTCs;
    FClearDTC: TOBDClearDTC;
    FFreezeFrame: TOBDFreezeFrame;
    FVIN: TOBDVIN;
    FVehicleInfo: TOBDVehicleInfo;
    FOnBoardMonitor: TOBDOnBoardMonitor;
    FDriveCycle: TOBDDriveCycleAdvisor;
    FVehicleHealth: TOBDVehicleHealth;
    FWWHOBD: TOBDWWHOBD;
    FEVBattery: TOBDEVBattery;
    FInspection: TOBDInspectionRules;
    FRecorder: TOBDRecorder;
    FCSVLogger: TOBDCSVLogger;
    FWorker: TPersonalJobThread;
    FState: TPersonalLinkState;
    FKind: TPersonalConnectionKind;
    FLastError: string;
    FProtocolName: string;
    FVehicleVIN: string;
    FBatteryVolts: Double;
    FGeneration: Integer;
    FClosing: Boolean;
    FOnStateChanged: TNotifyEvent;
    FOnStatus: TNotifyEvent;
    procedure SetState(AValue: TPersonalLinkState);
    procedure ConfigureConnection(ASettings: TPersonalSettings);
    procedure ConnectionLost(Sender: TObject);
    function ActiveConnection: TOBDConnection;
    procedure CloseLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Opens the adapter chosen in the settings (or the demo car),
    /// identifies it, finds the vehicle protocol and reads the VIN.
    /// Returns at once; OnStateChanged reports the result.</summary>
    procedure Connect(ASettings: TPersonalSettings);
    /// <summary>Stops polling and logging, cancels the jobs and closes
    /// the adapter.</summary>
    procedure Disconnect;
    /// <summary>Queues AWork for the worker thread. ADone (optional) runs
    /// on the main thread afterwards. Jobs queued before a Disconnect are
    /// dropped with the error text 'cancelled'.</summary>
    procedure RunJob(const AWork: TProc; const ADone: TPersonalJobDone);
    /// <summary>Reads the battery voltage at the OBD socket in the
    /// background and fires OnStatus.</summary>
    procedure RefreshBattery;
    /// <summary>Sends a raw adapter command (console page) on the worker
    /// thread; AReply gets the reply lines or the error text.</summary>
    procedure SendRaw(ACommand: string; AReply: TProc<string>);
    /// <summary>True while connected.</summary>
    function Online: Boolean;
    /// <summary>True when the demo car is the active connection.</summary>
    function IsDemo: Boolean;
    /// <summary>Display name of the adapter (identity, or the demo).</summary>
    function AdapterName: string;

    property State: TPersonalLinkState read FState;
    property Kind: TPersonalConnectionKind read FKind;
    property LastError: string read FLastError;
    /// <summary>Vehicle protocol as reported by the adapter
    /// (e.g. 'ISO 15765-4 (CAN 11/500)').</summary>
    property ProtocolName: string read FProtocolName;
    /// <summary>VIN read during connect; empty when the car does not
    /// report it.</summary>
    property VehicleVIN: string read FVehicleVIN;
    /// <summary>Last battery voltage; 0 when unknown.</summary>
    property BatteryVolts: Double read FBatteryVolts;

    property Adapter: TOBDAdapter read FAdapter;
    property Protocol: TOBDProtocol read FProtocol;
    property LiveData: TOBDLiveData read FLiveData;
    property DTCs: TOBDDTCs read FDTCs;
    property ClearDTC: TOBDClearDTC read FClearDTC;
    property FreezeFrame: TOBDFreezeFrame read FFreezeFrame;
    property VIN: TOBDVIN read FVIN;
    property VehicleInfo: TOBDVehicleInfo read FVehicleInfo;
    property OnBoardMonitor: TOBDOnBoardMonitor read FOnBoardMonitor;
    property DriveCycle: TOBDDriveCycleAdvisor read FDriveCycle;
    property VehicleHealth: TOBDVehicleHealth read FVehicleHealth;
    property WWHOBD: TOBDWWHOBD read FWWHOBD;
    property EVBattery: TOBDEVBattery read FEVBattery;
    property Inspection: TOBDInspectionRules read FInspection;
    property Recorder: TOBDRecorder read FRecorder;
    property CSVLogger: TOBDCSVLogger read FCSVLogger;

    /// <summary>Fires on the main thread when State changes.</summary>
    property OnStateChanged: TNotifyEvent read FOnStateChanged
      write FOnStateChanged;
    /// <summary>Fires on the main thread when the protocol, VIN or
    /// battery voltage changes.</summary>
    property OnStatus: TNotifyEvent read FOnStatus write FOnStatus;
  end;

  /// <summary>One queued piece of background work.</summary>
  TPersonalJob = record
    Work: TProc;
    Done: TPersonalJobDone;
    Generation: Integer;
  end;

  /// <summary>Runs the session's jobs one after the other.</summary>
  TPersonalJobThread = class(TThread)
  strict private
    FJobs: TQueue<TPersonalJob>;
    FLock: TCriticalSection;
    FSignal: TEvent;
    FGeneration: Integer;
    function TakeJob(out AJob: TPersonalJob): Boolean;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AJob: TPersonalJob);
    /// <summary>Drops waiting jobs older than AGeneration.</summary>
    procedure SetGeneration(AGeneration: Integer);
  end;

implementation

uses
  ERD.Types,
  ERD.Connection.Types,
  ERD.Adapter.Types;

const
  JOB_CANCELLED = 'cancelled';

// Queues the main-thread callback of one job. A routine of its own so
// that the anonymous method captures this call's parameters, not the
// loop variables of the worker (which the next job overwrites). The
// parameters are not const: the closure has to hold its own references.
procedure QueueJobDone(AThread: TThread; ADone: TPersonalJobDone;
  AError: string);
begin
  TThread.Queue(AThread,
    procedure
    begin
      ADone(AError);
    end);
end;

function BaudRateFromInteger(AValue: Integer): TOBDBaudRate;
begin
  case AValue of
    9600: Result := br9600;
    19200: Result := br19200;
    57600: Result := br57600;
    115200: Result := br115200;
    230400: Result := br230400;
    460800: Result := br460800;
    500000: Result := br500000;
    921600: Result := br921600;
    1000000: Result := br1000000;
    2000000: Result := br2000000;
  else
    Result := br38400;
  end;
end;

{ TPersonalJobThread }

constructor TPersonalJobThread.Create;
begin
  FJobs := TQueue<TPersonalJob>.Create;
  FLock := TCriticalSection.Create;
  FSignal := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TPersonalJobThread.Destroy;
begin
  inherited Destroy;
  FSignal.Free;
  FLock.Free;
  FJobs.Free;
end;

procedure TPersonalJobThread.TerminatedSet;
begin
  inherited;
  FSignal.SetEvent;
end;

procedure TPersonalJobThread.Add(const AJob: TPersonalJob);
begin
  FLock.Enter;
  try
    FJobs.Enqueue(AJob);
  finally
    FLock.Leave;
  end;
  FSignal.SetEvent;
end;

procedure TPersonalJobThread.SetGeneration(AGeneration: Integer);
begin
  FLock.Enter;
  try
    FGeneration := AGeneration;
  finally
    FLock.Leave;
  end;
end;

function TPersonalJobThread.TakeJob(out AJob: TPersonalJob): Boolean;
begin
  FLock.Enter;
  try
    Result := FJobs.Count > 0;
    if Result then
    begin
      AJob := FJobs.Dequeue;
      // A job queued before the last disconnect still reports back, but
      // its work is skipped.
      if AJob.Generation <> FGeneration then
        AJob.Work := nil;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TPersonalJobThread.Execute;
var
  Job: TPersonalJob;
  ErrorText: string;
begin
  NameThreadForDebugging('OBD-Studio jobs');
  while not Terminated do
  begin
    if not TakeJob(Job) then
    begin
      FSignal.WaitFor(INFINITE);
      Continue;
    end;
    if Assigned(Job.Work) then
    begin
      ErrorText := '';
      try
        Job.Work();
      except
        on E: Exception do
        begin
          ErrorText := E.Message;
          if ErrorText = '' then
            ErrorText := E.ClassName;
        end;
      end;
    end
    else
      ErrorText := JOB_CANCELLED;
    if Assigned(Job.Done) and not Terminated then
      QueueJobDone(Self, Job.Done, ErrorText);
  end;
end;

{ TPersonalSession }

constructor TPersonalSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSerial := TOBDConnection.Create(Self);
  FSerial.OnDisconnect := ConnectionLost;
  FDemo := TOBDEmulatorConnection.Create(Self);
  FDemo.EnableJ1979DIDs := True;
  FAdapter := TOBDAdapter.Create(Self);
  FProtocol := TOBDProtocol.Create(Self);
  FProtocol.Adapter := FAdapter;

  FLiveData := TOBDLiveData.Create(Self);
  FLiveData.Protocol := FProtocol;
  FDTCs := TOBDDTCs.Create(Self);
  FDTCs.Protocol := FProtocol;
  FClearDTC := TOBDClearDTC.Create(Self);
  FClearDTC.Protocol := FProtocol;
  FFreezeFrame := TOBDFreezeFrame.Create(Self);
  FFreezeFrame.Protocol := FProtocol;
  FVIN := TOBDVIN.Create(Self);
  FVIN.Protocol := FProtocol;
  FVehicleInfo := TOBDVehicleInfo.Create(Self);
  FVehicleInfo.Protocol := FProtocol;
  FOnBoardMonitor := TOBDOnBoardMonitor.Create(Self);
  FOnBoardMonitor.Protocol := FProtocol;
  FDriveCycle := TOBDDriveCycleAdvisor.Create(Self);
  FDriveCycle.Protocol := FProtocol;
  FWWHOBD := TOBDWWHOBD.Create(Self);
  FWWHOBD.Protocol := FProtocol;
  FEVBattery := TOBDEVBattery.Create(Self);
  FEVBattery.Protocol := FProtocol;

  FVehicleHealth := TOBDVehicleHealth.Create(Self);
  FVehicleHealth.Protocol := FProtocol;
  FVehicleHealth.VIN := FVIN;
  FVehicleHealth.DTCs := FDTCs;
  FVehicleHealth.LiveData := FLiveData;
  FVehicleHealth.OnBoardMonitor := FOnBoardMonitor;
  FVehicleHealth.FreezeFrame := FFreezeFrame;

  FInspection := TOBDInspectionRules.Create(Self);
  FRecorder := TOBDRecorder.Create(Self);
  FRecorder.Protocol := FProtocol;
  FCSVLogger := TOBDCSVLogger.Create(Self);
  FCSVLogger.LiveData := FLiveData;

  FWorker := TPersonalJobThread.Create;
end;

destructor TPersonalSession.Destroy;
begin
  if FWorker <> nil then
  begin
    CloseLink;
    FWorker.Terminate;
    FWorker.WaitFor;
    TThread.RemoveQueuedEvents(FWorker);
    FreeAndNil(FWorker);
  end;
  inherited Destroy;
end;

function TPersonalSession.ActiveConnection: TOBDConnection;
begin
  if FKind = pckDemo then
    Result := FDemo
  else
    Result := FSerial;
end;

procedure TPersonalSession.ConfigureConnection(ASettings: TPersonalSettings);
begin
  FKind := ASettings.ConnectionKind;
  case FKind of
    pckSerial:
      begin
        FSerial.Transport := otSerial;
        FSerial.SerialSettings.Port := ASettings.SerialPort;
        FSerial.SerialSettings.BaudRate :=
          BaudRateFromInteger(ASettings.BaudRate);
      end;
    pckWiFi:
      begin
        FSerial.Transport := otWiFi;
        FSerial.WiFiSettings.Host := ASettings.WiFiHost;
        FSerial.WiFiSettings.Port := Word(ASettings.WiFiPort);
      end;
    pckBLE:
      begin
        FSerial.Transport := otBLE;
        FSerial.BLESettings.DeviceAddress := ASettings.BLEAddress;
      end;
    pckDemo:
      ;
  end;
end;

procedure TPersonalSession.SetState(AValue: TPersonalLinkState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

function TPersonalSession.Online: Boolean;
begin
  Result := FState = plsOnline;
end;

function TPersonalSession.IsDemo: Boolean;
begin
  Result := FKind = pckDemo;
end;

function TPersonalSession.AdapterName: string;
begin
  Result := FAdapter.Identity.DisplayName;
  if Result = '' then
    Result := 'ELM327';
  if FAdapter.Identity.FirmwareVersion <> '' then
    Result := Result + ' ' + FAdapter.Identity.FirmwareVersion;
end;

procedure TPersonalSession.Connect(ASettings: TPersonalSettings);
var
  Connection: TOBDConnection;
begin
  if FState in [plsConnecting, plsOnline] then
    Exit;
  ConfigureConnection(ASettings);
  Connection := ActiveConnection;
  FAdapter.Connection := Connection;
  FLastError := '';
  FProtocolName := '';
  FVehicleVIN := '';
  FBatteryVolts := 0;
  SetState(plsConnecting);
  RunJob(
    procedure
    var
      Reply: TOBDAdapterResponse;
      VINResult: TOBDVINResult;
    begin
      Connection.Open;
      FAdapter.Detect;
      // The first request makes the adapter search the vehicle protocol;
      // it fails when the ignition is off or the car is not reachable.
      FLiveData.SupportedPIDs;
      Reply := FAdapter.SendCommand('ATDP', 2000);
      if Length(Reply.Lines) > 0 then
        FProtocolName := Trim(Reply.Lines[0]);
      try
        VINResult := FVIN.Read;
        if VINResult.Valid then
          FVehicleVIN := VINResult.RawVIN;
      except
        // Older cars do not report the VIN; that is not an error.
        FVehicleVIN := '';
      end;
    end,
    procedure(const AError: string)
    begin
      if FState <> plsConnecting then
        Exit; // disconnected meanwhile
      if AError = '' then
      begin
        SetState(plsOnline);
        RefreshBattery;
      end
      else
      begin
        FLastError := AError;
        CloseLink;
        SetState(plsFailed);
      end;
      if Assigned(FOnStatus) then
        FOnStatus(Self);
    end);
end;

procedure TPersonalSession.CloseLink;
begin
  if FClosing then
    Exit;
  FClosing := True;
  try
    Inc(FGeneration);
    FWorker.SetGeneration(FGeneration);
    if FCSVLogger.Active then
      FCSVLogger.Stop;
    if FLiveData.IsPolling then
      FLiveData.PollStop;
    FDriveCycle.Stop;
    FEVBattery.Stop;
    FAdapter.Close;
    if FDemo.Active then
      FDemo.Close;
    if FSerial.Active then
      FSerial.Close;
  finally
    FClosing := False;
  end;
end;

procedure TPersonalSession.Disconnect;
begin
  if FState = plsOffline then
    Exit;
  CloseLink;
  FProtocolName := '';
  FBatteryVolts := 0;
  SetState(plsOffline);
  if Assigned(FOnStatus) then
    FOnStatus(Self);
end;

procedure TPersonalSession.ConnectionLost(Sender: TObject);
begin
  // The adapter was unplugged or the Wi-Fi link dropped. A close of our
  // own (Disconnect) also fires OnDisconnect; FClosing filters it out.
  if (FState = plsOnline) and not FClosing then
  begin
    FLastError := 'The connection with the adapter was lost.';
    CloseLink;
    SetState(plsFailed);
  end;
end;

procedure TPersonalSession.RunJob(const AWork: TProc;
  const ADone: TPersonalJobDone);
var
  Job: TPersonalJob;
begin
  Job.Work := AWork;
  Job.Done := ADone;
  Job.Generation := FGeneration;
  FWorker.Add(Job);
end;

procedure TPersonalSession.RefreshBattery;
var
  Volts: Double;
begin
  if FState <> plsOnline then
    Exit;
  Volts := 0;
  RunJob(
    procedure
    var
      Reply: TOBDAdapterResponse;
      Text: string;
      Settings: TFormatSettings;
    begin
      Reply := FAdapter.SendCommand('ATRV', 1000);
      if Length(Reply.Lines) = 0 then
        Exit;
      Text := Trim(Reply.Lines[0]).Replace('V', '', [rfIgnoreCase]);
      Settings := TFormatSettings.Invariant;
      Volts := StrToFloatDef(Text, 0, Settings);
    end,
    procedure(const AError: string)
    begin
      if (AError = '') and (FState = plsOnline) then
      begin
        FBatteryVolts := Volts;
        if Assigned(FOnStatus) then
          FOnStatus(Self);
      end;
    end);
end;

procedure TPersonalSession.SendRaw(ACommand: string; AReply: TProc<string>);
var
  ReplyText: string;
begin
  ReplyText := '';
  RunJob(
    procedure
    var
      Reply: TOBDAdapterResponse;
    begin
      Reply := FAdapter.SendCommand(ACommand, 5000);
      ReplyText := string.Join(sLineBreak, Reply.Lines);
    end,
    procedure(const AError: string)
    begin
      if AError = '' then
        AReply(ReplyText)
      else
        AReply('? ' + AError);
    end);
end;

end.
