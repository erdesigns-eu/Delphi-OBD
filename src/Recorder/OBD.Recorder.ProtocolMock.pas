//------------------------------------------------------------------------------
//  OBD.Recorder.ProtocolMock
//
//  TOBDProtocolMock — a non-component class that exposes the
//  same event surface as <see cref="OBD.Protocol.TOBDProtocol"/>
//  (<c>OnFrame</c> / <c>OnResponse</c> / <c>OnNRC</c> /
//  <c>OnError</c>) but drives those events from a recorded
//  <c>.obdlog</c> instead of a live ECU. Hosts wire their
//  integration tests against the mock the same way they wire to
//  a real protocol; flip a constant and the test runs against a
//  capture from the bench.
//
//  Designed for end-to-end host-side tests:
//    - Capture a session against a live car with TOBDRecorder.
//    - Ship the .obdlog as a fixture.
//    - In CI: load the fixture into TOBDProtocolMock and replay.
//
//  This class is intentionally not registered on the palette —
//  it is a test helper.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Phase 10 follow-up: end-to-end capture replay.
//------------------------------------------------------------------------------

unit OBD.Recorder.ProtocolMock;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Recorder,
  OBD.Replayer;

type
  /// <summary>Replays a recording back through the
  /// <see cref="TOBDProtocol"/> event surface so host code can
  /// run integration tests against a captured session.</summary>
  TOBDProtocolMock = class(TComponent)
  strict private
    FReplayer: TOBDReplayer;
    FOnFrame: TOBDProtocolFrameEvent;
    FOnResponse: TOBDProtocolResponseEvent;
    FOnNRC: TOBDProtocolNRCEvent;
    FOnError: TOBDConnectionErrorEvent;
    FOnComplete: TNotifyEvent;
    procedure HandleEntry(Sender: TObject; const AEntry: TOBDLogEntry);
    procedure HandleComplete(Sender: TObject);
    procedure HandleError(Sender: TObject; ACode: TOBDErrorCode;
      const AMessage: string; var AHandled: Boolean);
    function GetFileName: string;
    procedure SetFileName(const AValue: string);
    function GetMode: TOBDReplayMode;
    procedure SetMode(AValue: TOBDReplayMode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Synchronous replay. Blocks until the recording
    /// finishes or <c>Stop</c> is called.</summary>
    procedure Run;
    /// <summary>Non-blocking replay. Fires <c>OnComplete</c> when
    /// the capture is fully drained.</summary>
    procedure RunAsync;
    /// <summary>Cooperative cancel.</summary>
    procedure Stop;

    /// <summary>Path to the recorded <c>.obdlog</c> (plain or
    /// gzip).</summary>
    property FileName: string read GetFileName write SetFileName;
    /// <summary>Playback mode. <c>rmAsFastAsPossible</c> is the
    /// default (CI-friendly).</summary>
    property Mode: TOBDReplayMode read GetMode write SetMode;

    /// <summary>Mirrors <c>TOBDProtocol.OnFrame</c>.</summary>
    property OnFrame: TOBDProtocolFrameEvent read FOnFrame write FOnFrame;
    /// <summary>Mirrors <c>TOBDProtocol.OnResponse</c>.</summary>
    property OnResponse: TOBDProtocolResponseEvent
      read FOnResponse write FOnResponse;
    /// <summary>Mirrors <c>TOBDProtocol.OnNRC</c>.</summary>
    property OnNRC: TOBDProtocolNRCEvent read FOnNRC write FOnNRC;
    /// <summary>Mirrors <c>TOBDProtocol.OnError</c>.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
    /// <summary>Fires once the capture is fully drained.</summary>
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

implementation

constructor TOBDProtocolMock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReplayer := TOBDReplayer.Create(Self);
  FReplayer.OnEntry    := HandleEntry;
  FReplayer.OnComplete := HandleComplete;
  FReplayer.OnError    := HandleError;
end;

destructor TOBDProtocolMock.Destroy;
begin
  // FReplayer is owned via the component hierarchy; no explicit
  // free required.
  inherited;
end;

function TOBDProtocolMock.GetFileName: string;
begin
  Result := FReplayer.FileName;
end;

procedure TOBDProtocolMock.SetFileName(const AValue: string);
begin
  FReplayer.FileName := AValue;
end;

function TOBDProtocolMock.GetMode: TOBDReplayMode;
begin
  Result := FReplayer.Mode;
end;

procedure TOBDProtocolMock.SetMode(AValue: TOBDReplayMode);
begin
  FReplayer.Mode := AValue;
end;

procedure TOBDProtocolMock.Run;
begin
  FReplayer.Play;
end;

procedure TOBDProtocolMock.RunAsync;
begin
  FReplayer.PlayAsync;
end;

procedure TOBDProtocolMock.Stop;
begin
  FReplayer.Stop;
end;

procedure TOBDProtocolMock.HandleEntry(Sender: TObject;
  const AEntry: TOBDLogEntry);
var
  Frame: TOBDFrame;
  Resp: TOBDResponse;
  Req: TOBDRequest;
  Handled: Boolean;
begin
  case AEntry.Kind of
    leFrame:
      if Assigned(FOnFrame) then
      begin
        Frame := Default(TOBDFrame);
        Frame.Id := AEntry.FrameID;
        Frame.IsExtendedId := AEntry.Extended;
        Frame.Payload := AEntry.Raw;
        Frame.Timestamp := AEntry.Timestamp;
        Frame.Kind := fkRaw;
        FOnFrame(Self, Frame);
      end;

    leResponse:
      if Assigned(FOnResponse) then
      begin
        Resp := MakeOBDResponse;
        Resp.ServiceID := AEntry.ServiceID;
        Resp.Data      := AEntry.Raw;
        Resp.Elapsed   := AEntry.ElapsedMs;
        FOnResponse(Self, Resp);
      end;

    leNRC:
      if Assigned(FOnNRC) then
      begin
        Req := MakeOBDRequest;
        Req.ServiceID := AEntry.ServiceID;
        FOnNRC(Self, Req, AEntry.NRC, AEntry.NRCText);
      end;

    leError:
      if Assigned(FOnError) then
      begin
        Handled := False;
        FOnError(Self, oeIO, AEntry.Message, Handled);
      end;

    leInfo:
      ; // Host-supplied breadcrumbs — not surfaced via protocol events.
  end;
end;

procedure TOBDProtocolMock.HandleComplete(Sender: TObject);
begin
  if Assigned(FOnComplete) then FOnComplete(Self);
end;

procedure TOBDProtocolMock.HandleError(Sender: TObject;
  ACode: TOBDErrorCode; const AMessage: string;
  var AHandled: Boolean);
begin
  if Assigned(FOnError) then FOnError(Self, ACode, AMessage, AHandled);
end;

end.
