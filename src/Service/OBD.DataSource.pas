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
  /// <summary>Source-kind enum (informational; the actual source
  /// reference is held in <see cref="TOBDDataSource.Source"/>).
  /// </summary>
  TOBDDataSourceKind = (
    dsNone,
    dsLiveData,
    dsDTCs,
    dsFreezeFrame,
    dsOnBoardMonitor,
    dsVIN
  );

  /// <summary>Data-change notification payload — opaque carrier so
  /// the bridge can re-fire any service component's events without
  /// the source-kind leaking into consumers that don't care.</summary>
  TOBDDataSourcePayload = record
    Kind:    TOBDDataSourceKind;
    /// <summary>Mode 01 PID byte (when <c>Kind = dsLiveData</c>).</summary>
    PID:     Byte;
    /// <summary>Decoded value (when <c>Kind = dsLiveData</c>).</summary>
    Value:   TOBDValue;
    /// <summary>Raw response bytes (any kind).</summary>
    Raw:     TBytes;
    /// <summary>Short human-readable description.</summary>
    Caption: string;
  end;

  TOBDDataSourceEvent = procedure(Sender: TObject;
    const APayload: TOBDDataSourcePayload) of object;
  TOBDDataSourceStateEvent = procedure(Sender: TObject;
    AActive: Boolean) of object;

  /// <summary>The bridge.</summary>
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
    // Adapters for each known source kind.
    procedure HandleLiveRaw(Sender: TObject;
      APID: Byte; const ARaw: TBytes);
    procedure HandleDTCs(Sender: TObject;
      AKind: TOBDDtcKind; const AEntries: TArray<TOBDDtcEntry>);
    procedure HandleFreezeFrame(Sender: TObject;
      AFrameIndex: Byte; const AValue: TOBDPIDValue);
    procedure HandleMonitor(Sender: TObject;
      AMID: Byte; const AResults: TArray<TOBDMonitorResult>);
    procedure HandleVIN(Sender: TObject; const AResult: TOBDVINResult);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Manually push a data-change notification to every
    /// listener. Useful for synthetic / mocked sources during tests.
    /// </summary>
    procedure Notify(const APayload: TOBDDataSourcePayload);
  published
    /// <summary>One of TOBDLiveData / TOBDDTCs / TOBDFreezeFrame /
    /// TOBDOnBoardMonitor / TOBDVIN. Assigning unsubscribes from
    /// the previous source and subscribes to the new one when
    /// <c>Active</c> is <c>True</c>.</summary>
    property Source: TComponent read FSource write SetSource;
    /// <summary>Read-only — derived from <c>Source</c>'s class.</summary>
    property Kind: TOBDDataSourceKind read FKind;
    /// <summary>Subscribe / unsubscribe gate. Default <c>True</c>.
    /// </summary>
    property Active: Boolean read FActive write SetActive default True;
    property OnDataChange: TOBDDataSourceEvent read FOnDataChange
      write FOnDataChange;
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
  if FSource = AValue then Exit;
  Unwire;
  if FSource <> nil then FSource.RemoveFreeNotification(Self);
  FSource := AValue;
  if AValue is TOBDLiveData            then FKind := dsLiveData
  else if AValue is TOBDDTCs           then FKind := dsDTCs
  else if AValue is TOBDFreezeFrame    then FKind := dsFreezeFrame
  else if AValue is TOBDOnBoardMonitor then FKind := dsOnBoardMonitor
  else if AValue is TOBDVIN            then FKind := dsVIN
  else                                      FKind := dsNone;
  if FSource <> nil then FSource.FreeNotification(Self);
  if FActive then Rewire;
  FireState;
end;

procedure TOBDDataSource.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if FActive then Rewire else Unwire;
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
  if FSource = nil then Exit;
  // TOBDLiveData uses a subscribe API rather than a single
  // OnValue event, so we can't blanket-attach to every PID — the
  // OnRaw event is the universal hook here, mirroring how the
  // service-mode component reports any inbound frame.
  if FSource is TOBDLiveData then
  begin
    TOBDLiveData(FSource).OnRaw := HandleLiveRaw;
  end
  else if FSource is TOBDDTCs then
  begin
    TOBDDTCs(FSource).OnDTCs := HandleDTCs;
  end
  else if FSource is TOBDFreezeFrame then
  begin
    TOBDFreezeFrame(FSource).OnValue := HandleFreezeFrame;
  end
  else if FSource is TOBDOnBoardMonitor then
  begin
    TOBDOnBoardMonitor(FSource).OnResults := HandleMonitor;
  end
  else if FSource is TOBDVIN then
  begin
    TOBDVIN(FSource).OnVIN := HandleVIN;
  end;
end;

procedure TOBDDataSource.Unwire;
begin
  if FSource = nil then Exit;
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

procedure TOBDDataSource.HandleLiveRaw(Sender: TObject;
  APID: Byte; const ARaw: TBytes);
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

procedure TOBDDataSource.HandleDTCs(Sender: TObject;
  AKind: TOBDDtcKind; const AEntries: TArray<TOBDDtcEntry>);
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

procedure TOBDDataSource.HandleMonitor(Sender: TObject;
  AMID: Byte; const AResults: TArray<TOBDMonitorResult>);
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
  if not Assigned(FOnDataChange) then Exit;
  Self_ := Self;
  Snap := APayload;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDataChange(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnDataChange) then Self_.FOnDataChange(Self_, Snap);
    end);
end;

procedure TOBDDataSource.FireState;
var
  Self_: TOBDDataSource; Act: Boolean;
begin
  if not Assigned(FOnStateChange) then Exit;
  Self_ := Self; Act := FActive and (FSource <> nil);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnStateChange(Self_, Act)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnStateChange) then Self_.FOnStateChange(Self_, Act);
    end);
end;

end.
