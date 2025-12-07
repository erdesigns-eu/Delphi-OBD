//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.Component.pas
// CONTENTS       : Non-visual protocol component wrappers
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : OpenAI Assistant
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 05/02/2025
//------------------------------------------------------------------------------
unit OBD.Protocol.Component;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, WinApi.Windows,

  OBD.Adapter.Types,
  OBD.Component.BindingHelpers,
  OBD.Connection.Types, OBD.Connection, OBD.Connection.Component,
  OBD.Protocol, OBD.Protocol.Types, OBD.Protocol.CAN, OBD.Protocol.Legacy;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Class reference used to create protocol instances for the non-visual
  ///   component.
  /// </summary>
  TOBDProtocolClass = class of TOBDProtocol;

  /// <summary>
  ///   Non-visual protocol component that instantiates an OBD protocol parser
  ///   and optionally binds to a connection component to consume incoming data.
  /// </summary>
  TOBDProtocolComponent = class(TComponent)
  private
    /// <summary>
    ///   Backing store for the published <c>ConnectionComponent</c> property.
    /// </summary>
    FConnectionComponent: TOBDConnectionComponent;
    /// <summary>
    ///   Currently bound connection instance retrieved from the connection
    ///   component.
    /// </summary>
    FConnection: IOBDConnection;
    /// <summary>
    ///   Optional chained handler captured from the connection prior to binding
    ///   so existing callbacks still execute.
    /// </summary>
    FChainedOnDataReceived: TDataReceivedEvent;
    /// <summary>
    ///   Protocol class reference used to build the protocol instance.
    /// </summary>
    FProtocolClass: TOBDProtocolClass;
    /// <summary>
    ///   Protocol instance created from the configured class reference.
    /// </summary>
    FProtocol: IOBDProtocol;
    /// <summary>
    ///   Flag indicating whether long messages are allowed when constructing the
    ///   protocol instance.
    /// </summary>
    FAllowLongMessages: Boolean;
    /// <summary>
    ///   Buffer that stores incomplete received data until line terminators are
    ///   encountered.
    /// </summary>
    FPendingBuffer: AnsiString;
    /// <summary>
    ///   Temporary storage for completed text lines awaiting protocol
    ///   processing.
    /// </summary>
    FPendingLines: TStringList;
    /// <summary>
    ///   Monitor guarding buffer and line list updates coming from the
    ///   connection event handler.
    /// </summary>
    FBufferLock: TObject;
    /// <summary>
    ///   Monitor used to guard event binding/unbinding for connection handlers.
    /// </summary>
    FBindingLock: TObject;
    /// <summary>
    ///   Indicates if the component should automatically hook into the
    ///   connection component's data events.
    /// </summary>
    FAutoBindConnection: Boolean;
    /// <summary>
    ///   Event handler invoked after parsing messages from received lines.
    /// </summary>
    FOnMessages: TReceiveDataMessagesEvent;
    /// <summary>
    ///   Monitor guarding message dispatch to avoid concurrent invocation when
    ///   marshalling to the main thread.
    /// </summary>
    FDispatchLock: TObject;
    /// <summary>
    ///   Event handler invoked after individual data fragments are transformed
    ///   into lines while still forwarding to the chained handler.
    /// </summary>
    FOnDataReceived: TDataReceivedEvent;
    /// <summary>
    ///   Optional callback raised when connection bindings are swapped or
    ///   restored for observability.
    /// </summary>
    FOnBindingNotification: TOBDBindingNotification;
    /// <summary>
    ///   Monitor object guarding access to the diagnostic line buffer.
    /// </summary>
    FDiagnosticsLock: TObject;
    /// <summary>
    ///   Maximum number of diagnostic lines retained in memory for snapshot
    ///   retrieval.
    /// </summary>
    FDiagnosticsDepth: Integer;
    /// <summary>
    ///   Ring buffer of recent lines observed by the parser for diagnostics.
    /// </summary>
    FRecentLines: TStringList;
    /// <summary>
    ///   Optional callback raised whenever diagnostic lines are updated.
    /// </summary>
    FOnDiagnosticsUpdated: TNotifyEvent;
    /// <summary>
    ///   Optional diagnostic callback invoked when monitor waits exceed the
    ///   configured threshold.
    /// </summary>
    FOnDiagnostic: TOBDDiagnosticEvent;
    /// <summary>
    ///   Replace the current protocol instance using the configured class
    ///   reference and allowance for long messages.
    /// </summary>
    procedure RebuildProtocol;
    /// <summary>
    ///   Assign the component to a connection component and rebind events when
    ///   auto-binding is enabled.
    /// </summary>
    procedure SetConnectionComponent(const Value: TOBDConnectionComponent);
    /// <summary>
    ///   Update the protocol class and recreate the parser to reflect the new
    ///   selection.
    /// </summary>
    procedure SetProtocolClass(const Value: TOBDProtocolClass);
    /// <summary>
    ///   Update the long-message flag and rebuild the protocol instance when
    ///   necessary.
    /// </summary>
    procedure SetAllowLongMessages(const Value: Boolean);
    /// <summary>
    ///   Toggle automatic binding to the connection component's data events.
    /// </summary>
    procedure SetAutoBindConnection(const Value: Boolean);
    /// <summary>
    ///   Bind or unbind the component from the configured connection component
    ///   depending on the auto-bind flag and availability.
    /// </summary>
    procedure RefreshConnectionBinding;
    /// <summary>
    ///   Install the internal data-received handler on the connection while
    ///   preserving any existing callback.
    /// </summary>
    procedure AttachConnection;
    /// <summary>
    ///   Remove the internal data-received handler from the connection and
    ///   restore any prior callback.
    /// </summary>
    procedure DetachConnection;
    /// <summary>
    ///   Consume inbound data bytes, split them into lines, and dispatch them to
    ///   the protocol for parsing while chaining the original handler.
    /// </summary>
    procedure HandleDataReceived(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///   Convert buffered text into complete lines using CR/LF delimiters and
    ///   enqueue them for protocol parsing.
    /// </summary>
    procedure ExtractCompletedLines(const Incoming: AnsiString);
    /// <summary>
    ///   Parse the pending lines with the active protocol and emit the messages
    ///   event to listeners.
    /// </summary>
    procedure DispatchMessages;
    /// <summary>
    ///   Deliver parsed messages to listeners, marshalling to the main thread
    ///   when needed.
    /// </summary>
    procedure DeliverMessages(const Messages: TArray<IOBDDataMessage>);
    /// <summary>
    ///   Retrieve a thread-safe snapshot of the currently parsed ECUs from the
    ///   active protocol instance for design-time consumers.
    /// </summary>
    function GetECUListSnapshot: TArray<string>;
    /// <summary>
    ///   Records a diagnostic line while trimming the buffer to the configured
    ///   depth.
    /// </summary>
    procedure RecordDiagnosticLine(const Line: string);
    /// <summary>
    ///   Trims the diagnostic buffer under lock so it does not exceed the
    ///   configured depth.
    /// </summary>
    procedure TrimDiagnosticBuffer;
    /// <summary>
    ///   Retrieves a thread-safe snapshot of the diagnostic buffer contents.
    /// </summary>
    function GetDiagnosticsSnapshot: TArray<string>;
    /// <summary>
    ///   Updates the configured diagnostic depth and trims existing entries when
    ///   needed.
    /// </summary>
    procedure SetDiagnosticsDepth(const Value: Integer);
    /// <summary>
    ///   Raises the diagnostics updated event on the main thread.
    /// </summary>
    procedure NotifyDiagnosticsUpdated;
    /// <summary>
    ///   Emits a diagnostic callback when subscribed listeners are present.
    /// </summary>
    procedure EmitDiagnostic(const Detail: string);
    /// <summary>
    ///   Enters a monitor and reports contention when the wait exceeds the
    ///   configured diagnostic threshold.
    /// </summary>
    procedure EnterLock(const Lock: TObject; const Context: string; const ThresholdMs: Cardinal = 15);
  public
    /// <summary>
    ///   Initialize the component, allocating buffers and building the default
    ///   protocol instance.
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Tear down internal bindings, protocol instances, and buffers.
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///   Process a caller-supplied set of lines through the configured protocol
    ///   and emit parsed messages.
    /// </summary>
    procedure ProcessLines(Lines: TStrings);
    /// <summary>
    ///   Thread-safe snapshot of ECUs parsed by the active protocol.
    /// </summary>
    property ECUListSnapshot: TArray<string> read GetECUListSnapshot;
    /// <summary>
    ///   Current protocol instance created by the component.
    /// </summary>
    property Protocol: IOBDProtocol read FProtocol;
  published
    /// <summary>
    ///   Connection component whose data events should be consumed when
    ///   <c>AutoBindConnection</c> is enabled.
    /// </summary>
    property ConnectionComponent: TOBDConnectionComponent read FConnectionComponent write SetConnectionComponent;
    /// <summary>
    ///   Protocol class reference used to instantiate the parser.
    /// </summary>
    property ProtocolClass: TOBDProtocolClass read FProtocolClass write SetProtocolClass;
    /// <summary>
    ///   Indicates whether long messages should be permitted by the protocol
    ///   parser.
    /// </summary>
    property AllowLongMessages: Boolean read FAllowLongMessages write SetAllowLongMessages default False;
    /// <summary>
    ///   When enabled, automatically binds the connection component's data
    ///   events to the protocol component's handler.
    /// </summary>
    property AutoBindConnection: Boolean read FAutoBindConnection write SetAutoBindConnection default True;
    /// <summary>
    ///   Event fired when a data fragment is received from the bound connection
    ///   prior to message parsing.
    /// </summary>
    property OnDataReceived: TDataReceivedEvent read FOnDataReceived write FOnDataReceived;
    /// <summary>
    ///   Event fired after the protocol parses incoming lines into data
    ///   messages.
    /// </summary>
    property OnMessages: TReceiveDataMessagesEvent read FOnMessages write FOnMessages;
    /// <summary>
    ///   Optional callback raised when the component swaps or restores
    ///   connection bindings for observability purposes.
    /// </summary>
    property OnBindingNotification: TOBDBindingNotification read FOnBindingNotification
      write FOnBindingNotification;
    /// <summary>
    ///   Maximum number of diagnostic lines retained for inspection.
    /// </summary>
    property DiagnosticsDepth: Integer read FDiagnosticsDepth write SetDiagnosticsDepth default 50;
    /// <summary>
    ///   Thread-safe snapshot of the most recent diagnostic lines processed by
    ///   the parser.
    /// </summary>
    property DiagnosticsSnapshot: TArray<string> read GetDiagnosticsSnapshot;
    /// <summary>
    ///   Event fired when diagnostics are updated, dispatched onto the main
    ///   thread for UI safety.
    /// </summary>
    property OnDiagnosticsUpdated: TNotifyEvent read FOnDiagnosticsUpdated write FOnDiagnosticsUpdated;
    /// <summary>
    ///   Event fired when the component observes extended monitor waits during
    ///   binding or buffer protection.
    /// </summary>
    property OnDiagnostic: TOBDDiagnosticEvent read FOnDiagnostic write FOnDiagnostic;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDProtocolComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBufferLock := TObject.Create;
  FBindingLock := TObject.Create;
  FDispatchLock := TObject.Create;
  FDiagnosticsLock := TObject.Create;
  FRecentLines := TStringList.Create;
  FDiagnosticsDepth := 50;
  FPendingLines := TStringList.Create;
  FAutoBindConnection := True;
  FAllowLongMessages := False;
  FProtocolClass := TISO_15765_4_11BIT_500K_OBDProtocol;
  RebuildProtocol;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDProtocolComponent.Destroy;
begin
  DetachConnection;
  EnterLock(FBufferLock, 'BufferLock.Destroy');
  try
    FPendingLines.Free;
  finally
    TMonitor.Exit(FBufferLock);
  end;
  EnterLock(FDiagnosticsLock, 'DiagnosticsLock.Destroy');
  try
    FRecentLines.Free;
  finally
    TMonitor.Exit(FDiagnosticsLock);
  end;
  FBindingLock.Free;
  FDispatchLock.Free;
  FDiagnosticsLock.Free;
  FBufferLock.Free;
  FProtocol := nil;
  inherited;
end;

//------------------------------------------------------------------------------
// REBUILD PROTOCOL
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.RebuildProtocol;
var
  BootstrapLines: TStringList;
begin
  BootstrapLines := TStringList.Create;
  try
    FProtocol := FProtocolClass.Create(BootstrapLines, FAllowLongMessages);
  finally
    BootstrapLines.Free;
  end;
end;

//------------------------------------------------------------------------------
// SET CONNECTION COMPONENT
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.SetConnectionComponent(const Value: TOBDConnectionComponent);
begin
  if FConnectionComponent = Value then
    Exit;
  DetachConnection;
  FConnectionComponent := Value;
  RefreshConnectionBinding;
end;

//------------------------------------------------------------------------------
// SET PROTOCOL CLASS
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.SetProtocolClass(const Value: TOBDProtocolClass);
begin
  if FProtocolClass = Value then
    Exit;
  FProtocolClass := Value;
  RebuildProtocol;
end;

//------------------------------------------------------------------------------
// SET ALLOW LONG MESSAGES
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.SetAllowLongMessages(const Value: Boolean);
begin
  if FAllowLongMessages = Value then
    Exit;
  FAllowLongMessages := Value;
  RebuildProtocol;
end;

//------------------------------------------------------------------------------
// SET AUTO BIND CONNECTION
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.SetAutoBindConnection(const Value: Boolean);
begin
  if FAutoBindConnection = Value then
    Exit;
  FAutoBindConnection := Value;
  RefreshConnectionBinding;
end;

//------------------------------------------------------------------------------
// REFRESH CONNECTION BINDING
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.RefreshConnectionBinding;
begin
  DetachConnection;
  if FAutoBindConnection and Assigned(FConnectionComponent) then
    AttachConnection;
end;

//------------------------------------------------------------------------------
// ATTACH CONNECTION
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.AttachConnection;
var
  CurrentHandler: TDataReceivedEvent;
begin
  if not Assigned(FConnectionComponent) then
    Exit;
  FConnection := FConnectionComponent.ConnectionInstance;
  if not Assigned(FConnection) then
    Exit;
  CurrentHandler := FConnection.OnDataReceived;
  TOBDBindingHelpers.SwapHandler<TDataReceivedEvent>(FBindingLock, CurrentHandler,
    FChainedOnDataReceived, HandleDataReceived, 'Connection.OnDataReceived', Self,
    FOnBindingNotification, FOnDiagnostic, 'ProtocolConnectionBinding', 20);
  FConnection.OnDataReceived := CurrentHandler;
end;

//------------------------------------------------------------------------------
// DETACH CONNECTION
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.DetachConnection;
var
  CurrentHandler: TDataReceivedEvent;
begin
  if Assigned(FConnection) then
  begin
    CurrentHandler := FConnection.OnDataReceived;
    TOBDBindingHelpers.RestoreHandler<TDataReceivedEvent>(FBindingLock, CurrentHandler,
      FChainedOnDataReceived, 'Connection.OnDataReceived', Self, FOnBindingNotification,
      FOnDiagnostic, 'ProtocolConnectionBinding', 20);
    FConnection.OnDataReceived := CurrentHandler;
    FConnection := nil;
  end;
end;

//------------------------------------------------------------------------------
// HANDLE DATA RECEIVED
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.HandleDataReceived(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
var
  Incoming: AnsiString;
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(Sender, DataPtr, DataSize);
  if (DataPtr = nil) or (DataSize = 0) then
  begin
    if Assigned(FChainedOnDataReceived) then
      FChainedOnDataReceived(Sender, DataPtr, DataSize);
    Exit;
  end;
  SetString(Incoming, PAnsiChar(DataPtr), DataSize);
  ExtractCompletedLines(Incoming);
  if Assigned(FChainedOnDataReceived) then
    FChainedOnDataReceived(Sender, DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// EXTRACT COMPLETED LINES
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.ExtractCompletedLines(const Incoming: AnsiString);
var
  LineBreakPos: Integer;
  RawLine: AnsiString;
begin
  EnterLock(FBufferLock, 'BufferLock.ExtractCompletedLines');
  try
    FPendingBuffer := FPendingBuffer + Incoming;
    repeat
      LineBreakPos := Pos(#10, FPendingBuffer);
      if LineBreakPos = 0 then
        LineBreakPos := Pos(#13, FPendingBuffer);
      if LineBreakPos > 0 then
      begin
        RawLine := Copy(FPendingBuffer, 1, LineBreakPos - 1);
        Delete(FPendingBuffer, 1, LineBreakPos);
        RawLine := StringReplace(RawLine, #13, '', [rfReplaceAll]);
        RawLine := StringReplace(RawLine, #10, '', [rfReplaceAll]);
        if RawLine <> '' then
        begin
          FPendingLines.Add(string(RawLine));
          RecordDiagnosticLine(string(RawLine));
        end;
      end;
    until LineBreakPos = 0;
  finally
    TMonitor.Exit(FBufferLock);
  end;
  DispatchMessages;
end;

//------------------------------------------------------------------------------
// DISPATCH MESSAGES
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.DispatchMessages;
var
  LocalLines: TStringList;
  Messages: TArray<IOBDDataMessage>;
begin
  LocalLines := TStringList.Create;
  try
    EnterLock(FBufferLock, 'BufferLock.DispatchMessages');
    try
      LocalLines.Assign(FPendingLines);
      FPendingLines.Clear;
    finally
      TMonitor.Exit(FBufferLock);
    end;
    if LocalLines.Count > 0 then
    begin
      Messages := FProtocol.Invoke(LocalLines);
      DeliverMessages(Messages);
    end;
  finally
    LocalLines.Free;
  end;
end;

//------------------------------------------------------------------------------
// DELIVER MESSAGES
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.DeliverMessages(const Messages: TArray<IOBDDataMessage>);
var
  LocalMessages: TArray<IOBDDataMessage>;
begin
  if not Assigned(FOnMessages) or (Length(Messages) = 0) then
    Exit;
  if TThread.Current.ThreadID = MainThreadID then
  begin
    FOnMessages(Self, Messages);
    Exit;
  end;

  // Copy messages to local variable for capture
  LocalMessages := Messages;
  
  EnterLock(FDispatchLock, 'DispatchLock.DeliverMessages');
  try
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnMessages) then
          FOnMessages(Self, LocalMessages);
      end);
  finally
    TMonitor.Exit(FDispatchLock);
  end;
end;

//------------------------------------------------------------------------------
// RECORD DIAGNOSTIC LINE
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.RecordDiagnosticLine(const Line: string);
begin
  if Line = '' then
    Exit;
  EnterLock(FDiagnosticsLock, 'DiagnosticsLock.Record');
  try
    FRecentLines.Add(Line);
    TrimDiagnosticBuffer;
  finally
    TMonitor.Exit(FDiagnosticsLock);
  end;
  NotifyDiagnosticsUpdated;
end;

//------------------------------------------------------------------------------
// TRIM DIAGNOSTIC BUFFER
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.TrimDiagnosticBuffer;
begin
  if FDiagnosticsDepth <= 0 then
  begin
    FRecentLines.Clear;
    Exit;
  end;
  while FRecentLines.Count > FDiagnosticsDepth do
    FRecentLines.Delete(0);
end;

//------------------------------------------------------------------------------
// GET DIAGNOSTICS SNAPSHOT
//------------------------------------------------------------------------------
function TOBDProtocolComponent.GetDiagnosticsSnapshot: TArray<string>;
var
  Index: Integer;
begin
  EnterLock(FDiagnosticsLock, 'DiagnosticsLock.Snapshot');
  try
    SetLength(Result, FRecentLines.Count);
    for Index := 0 to FRecentLines.Count - 1 do
      Result[Index] := FRecentLines[Index];
  finally
    TMonitor.Exit(FDiagnosticsLock);
  end;
end;

//------------------------------------------------------------------------------
// SET DIAGNOSTICS DEPTH
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.SetDiagnosticsDepth(const Value: Integer);
begin
  if FDiagnosticsDepth = Value then
    Exit;
  FDiagnosticsDepth := Value;
  EnterLock(FDiagnosticsLock, 'DiagnosticsLock.SetDepth');
  try
    TrimDiagnosticBuffer;
  finally
    TMonitor.Exit(FDiagnosticsLock);
  end;
end;

//------------------------------------------------------------------------------
// NOTIFY DIAGNOSTICS UPDATED
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.NotifyDiagnosticsUpdated;
begin
  if not Assigned(FOnDiagnosticsUpdated) then
    Exit;
  if TThread.Current.ThreadID = MainThreadID then
  begin
    FOnDiagnosticsUpdated(Self);
    Exit;
  end;

  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnDiagnosticsUpdated) then
        FOnDiagnosticsUpdated(Self);
    end);
end;

//------------------------------------------------------------------------------
// EMIT DIAGNOSTIC
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.EmitDiagnostic(const Detail: string);
begin
  if Assigned(FOnDiagnostic) then
    FOnDiagnostic(Self, Detail);
end;

//------------------------------------------------------------------------------
// ENTER LOCK
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.EnterLock(const Lock: TObject; const Context: string;
  const ThresholdMs: Cardinal = 15);
var
  StartTicks: UInt64;
  Elapsed: UInt64;
begin
  StartTicks := TThread.GetTickCount64;
  TMonitor.Enter(Lock);
  Elapsed := TThread.GetTickCount64 - StartTicks;
  if (ThresholdMs > 0) and (Elapsed >= ThresholdMs) then
    EmitDiagnostic(Format('%s lock wait: %d ms', [Context, Elapsed]));
end;

//------------------------------------------------------------------------------
// GET ECU LIST SNAPSHOT
//------------------------------------------------------------------------------
function TOBDProtocolComponent.GetECUListSnapshot: TArray<string>;
begin
  if Assigned(FProtocol) then
    Result := FProtocol.ECUListSnapshot
  else
    Result := [];
end;

//------------------------------------------------------------------------------
// PROCESS LINES
//------------------------------------------------------------------------------
procedure TOBDProtocolComponent.ProcessLines(Lines: TStrings);
var
  Messages: TArray<IOBDDataMessage>;
begin
  if not Assigned(Lines) then
    Exit;
  Messages := FProtocol.Invoke(Lines);
  DeliverMessages(Messages);
end;

end.
