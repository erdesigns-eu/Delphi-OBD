//------------------------------------------------------------------------------
//  OBD.DataSource
//
//  TOBDDataSource — non-visual bridge analogous to VCL's TDataSource.
//  Sits between a service-mode component (TOBDLiveData / TOBDDTCs /
//  TOBDVIN / TOBDFreezeFrame / TOBDOnBoardMonitor) and any number of
//  consumers (visual or non-visual). Consumers wire OnDataChange /
//  OnStateChange instead of subscribing to the underlying component
//  directly — so the source can be swapped at runtime without every
//  consumer having to re-subscribe, and a single underlying
//  component serves many consumers without duplicate subscriptions.
//
//  The bridge intentionally only re-fires events; it does NOT
//  cache values. Visual controls that need a last-known value
//  pull it from the bound component or capture it from the
//  OnDataChange payload.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.DataSource;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types,
  OBD.Service.LiveData,
  OBD.Service.DTCs,
  OBD.Service.FreezeFrame,
  OBD.Service.OnBoardMonitor,
  OBD.Service.VIN;

type
  /// <summary>
  ///   Source-kind tag derived from the bound component's class.
  /// </summary>
  /// <remarks>
  ///   The actual source reference is held in
  ///   <see cref="TOBDDataSource.Source"/>. Consumers that only
  ///   need to dispatch on kind read this enum from the payload.
  /// </remarks>
  TOBDDataSourceKind = (
    /// <summary>No source bound.</summary>
    dsNone,
    /// <summary>Source is a <c>TOBDLiveData</c>.</summary>
    dsLiveData,
    /// <summary>Source is a <c>TOBDDTCs</c>.</summary>
    dsDTCs,
    /// <summary>Source is a <c>TOBDFreezeFrame</c>.</summary>
    dsFreezeFrame,
    /// <summary>Source is a <c>TOBDOnBoardMonitor</c>.</summary>
    dsOnBoardMonitor,
    /// <summary>Source is a <c>TOBDVIN</c>.</summary>
    dsVIN
  );

  /// <summary>
  ///   Data-change notification payload.
  /// </summary>
  /// <remarks>
  ///   Opaque carrier so the bridge can re-fire any service
  ///   component's events without the source-kind leaking into
  ///   consumers that don't care. Most fields are optional;
  ///   <c>Kind</c> tells the consumer which ones are populated.
  /// </remarks>
  TOBDDataSourcePayload = record
    /// <summary>Source kind that produced this notification.</summary>
    Kind: TOBDDataSourceKind;
    /// <summary>Mode 0x01 PID byte (when <c>Kind = dsLiveData</c>),
    /// Mode 0x06 MID byte (when <c>Kind = dsOnBoardMonitor</c>), or
    /// freeze-frame index (when <c>Kind = dsFreezeFrame</c>). 0
    /// otherwise.</summary>
    PID: Byte;
    /// <summary>Raw response bytes — any kind.</summary>
    Raw: TBytes;
    /// <summary>Short human-readable description for logging.</summary>
    Caption: string;
  end;

  /// <summary>
  ///   Data-change event. Main thread.
  /// </summary>
  TOBDDataSourceEvent = procedure(Sender: TObject;
    const APayload: TOBDDataSourcePayload) of object;

  /// <summary>
  ///   State-change event. Main thread.
  /// </summary>
  /// <remarks>
  ///   Fires when <c>Source</c> is assigned, cleared (by
  ///   FreeNotification or explicit unset), or when
  ///   <c>Active</c> toggles. <c>AActive</c> is <c>True</c> iff
  ///   <c>Active</c> and a non-nil <c>Source</c> are both true.
  /// </remarks>
  TOBDDataSourceStateEvent = procedure(Sender: TObject;
    AActive: Boolean) of object;

  /// <summary>
  ///   TDataSource-style bridge.
  /// </summary>
  /// <remarks>
  ///   Drop the bridge on a form, point <c>Source</c> at a Phase-5
  ///   service component, then wire <c>OnDataChange</c> on each
  ///   consumer. Toggling <c>Active</c> unsubscribes / re-subscribes
  ///   without losing the source reference. Re-assigning
  ///   <c>Source</c> automatically migrates subscribers.
  ///
  ///   The bridge only knows the public event surface of the bound
  ///   service component, so consumers that need more detail (e.g.
  ///   the full decoded <c>TOBDPIDValue</c>) should still subscribe
  ///   to that component directly.
  /// </remarks>
  TOBDDataSource = class(TComponent)
  strict private
    FSource: TComponent;
    FKind: TOBDDataSourceKind;
    FActive: Boolean;
    FOnDataChange: TOBDDataSourceEvent;
    FOnStateChange: TOBDDataSourceStateEvent;
    procedure SetSource(AValue: TComponent);
    procedure SetActive(AValue: Boolean);
    procedure Rewire;
    procedure Unwire;
    procedure FireData(const APayload: TOBDDataSourcePayload);
    procedure FireState;
    procedure HandleLiveRaw(Sender: TObject; APID: Byte;
      const ARaw: TBytes);
    procedure HandleDTCs(Sender: TObject; AKind: TOBDDtcKind;
      const AEntries: TArray<TOBDDtcEntry>);
    procedure HandleFreezeFrame(Sender: TObject; AFrameIndex: Byte;
      const AValue: TOBDPIDValue);
    procedure HandleMonitor(Sender: TObject; AMID: Byte;
      const AResults: TArray<TOBDMonitorResult>);
    procedure HandleVIN(Sender: TObject;
      const AResult: TOBDVINResult);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the bridge with <c>Active = True</c>.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Unsubscribes from the current source and frees
    /// state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Pushes a data-change notification manually.
    /// </summary>
    /// <param name="APayload">Payload to forward to every wired
    /// <c>OnDataChange</c> handler.</param>
    /// <remarks>
    ///   Useful for synthetic or mocked sources during tests.
    /// </remarks>
    procedure Notify(const APayload: TOBDDataSourcePayload);
  published
    /// <summary>
    ///   Bound service component (or <c>nil</c>).
    /// </summary>
    /// <remarks>
    ///   Assigning unsubscribes from the previous source and
    ///   subscribes to the new one when <c>Active = True</c>.
    ///   <c>FreeNotification</c> is wired so the bridge clears
    ///   itself when the source is destroyed.
    /// </remarks>
    property Source: TComponent read FSource write SetSource;

    /// <summary>
    ///   Read-only kind derived from <c>Source</c>'s class.
    /// </summary>
    property Kind: TOBDDataSourceKind read FKind;

    /// <summary>
    ///   Subscribe / unsubscribe gate. Default <c>True</c>.
    /// </summary>
    /// <remarks>
    ///   Setting to <c>False</c> detaches every event handler from
    ///   <c>Source</c>; setting back to <c>True</c> reattaches.
    /// </remarks>
    property Active: Boolean read FActive write SetActive default True;

    /// <summary>Fires on every source-driven update. Main thread.</summary>
    property OnDataChange: TOBDDataSourceEvent read FOnDataChange
      write FOnDataChange;

    /// <summary>Fires on source / active changes. Main thread.</summary>
    property OnStateChange: TOBDDataSourceStateEvent read FOnStateChange
      write FOnStateChange;
  end;

implementation

constructor TOBDDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
end;

destructor TOBDDataSource.Destroy;
begin
  Unwire;
  inherited;
end;

procedure TOBDDataSource.SetSource(AValue: TComponent);
begin
  if FSource = AValue then
    Exit;
  Unwire;
  if FSource <> nil then
    FSource.RemoveFreeNotification(Self);

  FSource := AValue;
  if AValue is TOBDLiveData then
    FKind := dsLiveData
  else if AValue is TOBDDTCs then
    FKind := dsDTCs
  else if AValue is TOBDFreezeFrame then
    FKind := dsFreezeFrame
  else if AValue is TOBDOnBoardMonitor then
    FKind := dsOnBoardMonitor
  else if AValue is TOBDVIN then
    FKind := dsVIN
  else
    FKind := dsNone;

  if FSource <> nil then
    FSource.FreeNotification(Self);
  if FActive then
    Rewire;
  FireState;
end;

procedure TOBDDataSource.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if FActive then
    Rewire
  else
    Unwire;
  FireState;
end;

procedure TOBDDataSource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSource) then
  begin
    FSource := nil;
    FKind := dsNone;
    FireState;
  end;
end;

procedure TOBDDataSource.Rewire;
begin
  if FSource = nil then
    Exit;
  // TOBDLiveData uses a subscribe API rather than a single
  // OnValue event, so we can't blanket-attach to every PID — the
  // OnRaw event is the universal hook here, mirroring how the
  // service-mode component reports any inbound frame.
  if FSource is TOBDLiveData then
    TOBDLiveData(FSource).OnRaw := HandleLiveRaw
  else if FSource is TOBDDTCs then
    TOBDDTCs(FSource).OnDTCs := HandleDTCs
  else if FSource is TOBDFreezeFrame then
    TOBDFreezeFrame(FSource).OnValue := HandleFreezeFrame
  else if FSource is TOBDOnBoardMonitor then
    TOBDOnBoardMonitor(FSource).OnResults := HandleMonitor
  else if FSource is TOBDVIN then
    TOBDVIN(FSource).OnVIN := HandleVIN;
end;

procedure TOBDDataSource.Unwire;
begin
  if FSource = nil then
    Exit;
  if FSource is TOBDLiveData then
    TOBDLiveData(FSource).OnRaw := nil
  else if FSource is TOBDDTCs then
    TOBDDTCs(FSource).OnDTCs := nil
  else if FSource is TOBDFreezeFrame then
    TOBDFreezeFrame(FSource).OnValue := nil
  else if FSource is TOBDOnBoardMonitor then
    TOBDOnBoardMonitor(FSource).OnResults := nil
  else if FSource is TOBDVIN then
    TOBDVIN(FSource).OnVIN := nil;
end;

procedure TOBDDataSource.HandleLiveRaw(Sender: TObject; APID: Byte;
  const ARaw: TBytes);
var
  P: TOBDDataSourcePayload;
begin
  P := Default(TOBDDataSourcePayload);
  P.Kind := dsLiveData;
  P.PID := APID;
  P.Raw := Copy(ARaw, 0, Length(ARaw));
  P.Caption := Format('Mode 01 PID %.2x raw', [APID]);
  FireData(P);
end;

procedure TOBDDataSource.HandleDTCs(Sender: TObject; AKind: TOBDDtcKind;
  const AEntries: TArray<TOBDDtcEntry>);
var
  P: TOBDDataSourcePayload;
begin
  P := Default(TOBDDataSourcePayload);
  P.Kind := dsDTCs;
  P.Caption := Format('DTCs (%d)', [Length(AEntries)]);
  FireData(P);
end;

procedure TOBDDataSource.HandleFreezeFrame(Sender: TObject;
  AFrameIndex: Byte; const AValue: TOBDPIDValue);
var
  P: TOBDDataSourcePayload;
begin
  P := Default(TOBDDataSourcePayload);
  P.Kind := dsFreezeFrame;
  P.PID := AFrameIndex;
  P.Caption := Format('Freeze frame %.2x', [AFrameIndex]);
  FireData(P);
end;

procedure TOBDDataSource.HandleMonitor(Sender: TObject; AMID: Byte;
  const AResults: TArray<TOBDMonitorResult>);
var
  P: TOBDDataSourcePayload;
begin
  P := Default(TOBDDataSourcePayload);
  P.Kind := dsOnBoardMonitor;
  P.PID := AMID;
  P.Caption := Format('Mode 06 MID %.2x (%d results)',
    [AMID, Length(AResults)]);
  FireData(P);
end;

procedure TOBDDataSource.HandleVIN(Sender: TObject;
  const AResult: TOBDVINResult);
var
  P: TOBDDataSourcePayload;
begin
  P := Default(TOBDDataSourcePayload);
  P.Kind := dsVIN;
  if AResult.Valid then
    P.Caption := AResult.RawVIN
  else
    P.Caption := '(invalid VIN: ' + AResult.RawVIN + ')';
  FireData(P);
end;

procedure TOBDDataSource.Notify(const APayload: TOBDDataSourcePayload);
begin
  FireData(APayload);
end;

procedure TOBDDataSource.FireData(const APayload: TOBDDataSourcePayload);
var
  Self_: TOBDDataSource;
  Snap: TOBDDataSourcePayload;
begin
  if not Assigned(FOnDataChange) then
    Exit;
  Self_ := Self;
  Snap := APayload;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDataChange(Self_, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnDataChange) then
          Self_.FOnDataChange(Self_, Snap);
      end);
end;

procedure TOBDDataSource.FireState;
var
  Self_: TOBDDataSource;
  ActiveNow: Boolean;
begin
  if not Assigned(FOnStateChange) then
    Exit;
  Self_ := Self;
  ActiveNow := FActive and (FSource <> nil);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnStateChange(Self_, ActiveNow)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnStateChange) then
          Self_.FOnStateChange(Self_, ActiveNow);
      end);
end;

end.
