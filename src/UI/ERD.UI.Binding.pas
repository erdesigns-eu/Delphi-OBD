//------------------------------------------------------------------------------
//  ERD.UI.Binding
//
//  TOBDChannelBinding - the one way every dashboard control gets its
//  data.
//
//  A channel is "data source + channel id". The source is a
//  TOBDLiveData and the id is a Mode 01 PID; the same object is the
//  extension point for UDS DIDs and calculated values.
//
//  The binding tracks:
//    - whether any value has arrived (no-data state);
//    - when the last value arrived, flipping to stale after
//      StaleAfterMs without an update (a small timer repaints the
//      owner when that happens);
//    - the engineering unit and raw bytes of the last value.
//
//  Hosts that compute their own values (or tests) call PushValue;
//  the binding then behaves exactly as if the value came from the
//  data source.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.Binding;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.ExtCtrls,
  ERD.Service.LiveData;

type
  /// <summary>Data channel shared by every dashboard control.</summary>
  /// <remarks>
  /// <para>Owned by a control (published as <c>Channel</c>). The
  /// owning control must forward <c>Notification(opRemove)</c> to
  /// <see cref="SourceRemoved"/> and call <see cref="Rebind"/> from
  /// <c>Loaded</c>; <c>TOBDGaugeBase</c> does both.</para>
  /// <para>The binding never subscribes at design time, so a form
  /// with a connected-looking dashboard does not start talking to a
  /// vehicle inside the IDE.</para>
  /// </remarks>
  TOBDChannelBinding = class(TPersistent)
  strict private
    FOwner: TComponent;
    FSource: TOBDLiveData;
    FPID: Byte;
    FStaleAfterMs: Cardinal;
    FSubscribed: Boolean;
    FHasValue: Boolean;
    FValue: Double;
    FLastUnit: string;
    FLastDescription: string;
    FRaw: TBytes;
    FLastTick: UInt64;
    FWasStale: Boolean;
    FTimer: TTimer;
    FOnValue: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    procedure SetSource(AValue: TOBDLiveData);
    procedure SetPID(AValue: Byte);
    procedure SetStaleAfterMs(AValue: Cardinal);
    procedure Subscribe;
    procedure Unsubscribe;
    procedure HandleValue(Sender: TObject; const AValue: TOBDPIDValue);
    procedure HandleTimer(Sender: TObject);
    procedure EnsureTimer;
    function Designing: Boolean;
  protected
    /// <summary>Returns the owning control so the Object Inspector
    /// shows the binding as a sub-property.</summary>
    /// <returns>Owner component.</returns>
    function GetOwner: TPersistent; override;
  public
    /// <summary>Creates an unbound channel.</summary>
    /// <param name="AOwner">Owning control. Receives free
    /// notifications for <see cref="Source"/>.</param>
    constructor Create(AOwner: TComponent);
    /// <summary>Unsubscribes and frees the stale timer.</summary>
    destructor Destroy; override;
    /// <summary>Copies source, PID and stale timeout from another
    /// binding.</summary>
    /// <param name="ASource">Binding to copy from.</param>
    procedure Assign(ASource: TPersistent); override;

    /// <summary>Feeds a value as if it arrived from the source.
    /// </summary>
    /// <param name="AValue">Value in the channel's metric unit.</param>
    procedure PushValue(AValue: Double); overload;
    /// <summary>Feeds a decoded PID snapshot as if it arrived from the
    /// source. NaN values update raw bytes and freshness only.
    /// </summary>
    /// <param name="AValue">Decoded snapshot.</param>
    procedure PushValue(const AValue: TOBDPIDValue); overload;
    /// <summary>Forgets the last value; the channel returns to the
    /// no-data state.</summary>
    procedure Clear;
    /// <summary>Re-subscribes with the current source and PID. Call
    /// from the owner's <c>Loaded</c>.</summary>
    procedure Rebind;
    /// <summary>Drops the source when it is being destroyed. Call
    /// from the owner's <c>Notification</c>.</summary>
    /// <param name="AComponent">Component being removed.</param>
    procedure SourceRemoved(AComponent: TComponent);
    /// <summary>True when a value arrived but is older than
    /// <see cref="StaleAfterMs"/>.</summary>
    /// <returns>Stale flag.</returns>
    function IsStale: Boolean;
    /// <summary>Milliseconds since the last value, or -1 when no
    /// value arrived yet.</summary>
    /// <returns>Age in milliseconds.</returns>
    function AgeMs: Int64;

    /// <summary>True once any value arrived.</summary>
    property HasValue: Boolean read FHasValue;
    /// <summary>Last value in the metric unit; NaN before the first
    /// value.</summary>
    property Value: Double read FValue;
    /// <summary>Unit reported by the decoder for the last value, e.g.
    /// <c>'rpm'</c>. Empty for host-pushed plain values.</summary>
    property LastUnit: string read FLastUnit;
    /// <summary>Decoder description of the last value.</summary>
    property LastDescription: string read FLastDescription;
    /// <summary>Raw bytes of the last value (for bit-coded PIDs such
    /// as the MIL status).</summary>
    property Raw: TBytes read FRaw;
    /// <summary>Fires on the main thread for every value.</summary>
    property OnValue: TNotifyEvent read FOnValue write FOnValue;
    /// <summary>Fires when the channel turns stale or is cleared.
    /// </summary>
    property OnStateChange: TNotifyEvent read FOnStateChange
      write FOnStateChange;
  published
    /// <summary>Data source. nil = host-driven via
    /// <see cref="PushValue"/> or the control's <c>Value</c>.
    /// </summary>
    property Source: TOBDLiveData read FSource write SetSource;
    /// <summary>Mode 01 PID to follow on <see cref="Source"/>.
    /// </summary>
    property PID: Byte read FPID write SetPID;
    /// <summary>Age after which the value is shown as stale.
    /// 0 disables stale detection.</summary>
    property StaleAfterMs: Cardinal read FStaleAfterMs write SetStaleAfterMs
      default 3000;
  end;

implementation

constructor TOBDChannelBinding.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FStaleAfterMs := 3000;
  FValue := NaN;
end;

destructor TOBDChannelBinding.Destroy;
begin
  Unsubscribe;
  FTimer.Free;
  inherited;
end;

function TOBDChannelBinding.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TOBDChannelBinding.Assign(ASource: TPersistent);
begin
  if ASource is TOBDChannelBinding then
  begin
    StaleAfterMs := TOBDChannelBinding(ASource).StaleAfterMs;
    PID := TOBDChannelBinding(ASource).PID;
    Source := TOBDChannelBinding(ASource).Source;
  end
  else
    inherited Assign(ASource);
end;

function TOBDChannelBinding.Designing: Boolean;
begin
  Result := (FOwner <> nil) and (csDesigning in FOwner.ComponentState);
end;

procedure TOBDChannelBinding.SetSource(AValue: TOBDLiveData);
begin
  if FSource = AValue then
    Exit;
  Unsubscribe;
  if (FSource <> nil) and (FOwner <> nil) then
    FSource.RemoveFreeNotification(FOwner);
  FSource := AValue;
  if (FSource <> nil) and (FOwner <> nil) then
    FSource.FreeNotification(FOwner);
  Subscribe;
end;

procedure TOBDChannelBinding.SetPID(AValue: Byte);
begin
  if FPID = AValue then
    Exit;
  Unsubscribe;
  FPID := AValue;
  Subscribe;
end;

procedure TOBDChannelBinding.SetStaleAfterMs(AValue: Cardinal);
begin
  FStaleAfterMs := AValue;
end;

procedure TOBDChannelBinding.Subscribe;
begin
  if FSubscribed or (FSource = nil) or Designing then
    Exit;
  if (FOwner <> nil) and (csLoading in FOwner.ComponentState) then
    Exit;
  FSource.Subscribe(FPID, HandleValue);
  FSubscribed := True;
end;

procedure TOBDChannelBinding.Unsubscribe;
begin
  if FSubscribed and (FSource <> nil) then
    FSource.Unsubscribe(FPID, HandleValue);
  FSubscribed := False;
end;

procedure TOBDChannelBinding.Rebind;
begin
  Unsubscribe;
  Subscribe;
end;

procedure TOBDChannelBinding.SourceRemoved(AComponent: TComponent);
begin
  if (AComponent <> nil) and (AComponent = FSource) then
  begin
    FSubscribed := False;
    FSource := nil;
  end;
end;

procedure TOBDChannelBinding.HandleValue(Sender: TObject;
  const AValue: TOBDPIDValue);
begin
  // TOBDLiveData dispatches subscribers on the main thread.
  PushValue(AValue);
end;

procedure TOBDChannelBinding.PushValue(AValue: Double);
var
  V: TOBDPIDValue;
begin
  V.PID := FPID;
  V.Value := AValue;
  V.Unit_ := FLastUnit;
  V.Description := FLastDescription;
  V.Raw := nil;
  PushValue(V);
end;

procedure TOBDChannelBinding.PushValue(const AValue: TOBDPIDValue);
begin
  FLastTick := TThread.GetTickCount64;
  FRaw := Copy(AValue.Raw);
  if AValue.Unit_ <> '' then
    FLastUnit := AValue.Unit_;
  if AValue.Description <> '' then
    FLastDescription := AValue.Description;
  if not IsNan(AValue.Value) then
    FValue := AValue.Value;
  FHasValue := FHasValue or not IsNan(AValue.Value) or (Length(FRaw) > 0);
  FWasStale := False;
  EnsureTimer;
  if Assigned(FOnValue) then
    FOnValue(Self);
end;

procedure TOBDChannelBinding.Clear;
begin
  FHasValue := False;
  FValue := NaN;
  FRaw := nil;
  FLastTick := 0;
  FWasStale := False;
  if FTimer <> nil then
    FTimer.Enabled := False;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TOBDChannelBinding.EnsureTimer;
begin
  if (FStaleAfterMs = 0) or Designing then
    Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Interval := 250;
    FTimer.OnTimer := HandleTimer;
  end;
  FTimer.Enabled := True;
end;

procedure TOBDChannelBinding.HandleTimer(Sender: TObject);
begin
  if IsStale and not FWasStale then
  begin
    FWasStale := True;
    FTimer.Enabled := False;
    if Assigned(FOnStateChange) then
      FOnStateChange(Self);
  end;
end;

function TOBDChannelBinding.AgeMs: Int64;
begin
  if not FHasValue then
    Result := -1
  else
    Result := Int64(TThread.GetTickCount64 - FLastTick);
end;

function TOBDChannelBinding.IsStale: Boolean;
begin
  Result := FHasValue and (FStaleAfterMs > 0) and
    (AgeMs > Int64(FStaleAfterMs));
end;

end.
