//------------------------------------------------------------------------------
//  OBD.Recorder
//
//  TOBDRecorder — captures every protocol-level frame + response
//  + error to an append-only `.obdlog` file. Hosts use this both
//  as a forensic / audit trail and as the input for the Phase 10
//  TOBDReplayer (run a captured session against a stub protocol
//  stack for offline analysis).
//
//  File format (one entry per line, JSONL):
//
//    {"ts":"2026-05-09T12:34:56.789Z",
//     "kind":"frame|response|nrc|error|info",
//     "elapsed_ms":42,
//     "service_id":"0x22",
//     "raw":"BASE64",
//     "id":"0x7E0",
//     "extended":false,
//     "nrc":"0x33",
//     "nrc_text":"Security access denied",
//     "message":"..."}
//
//  Schema is intentionally close to the v1 .obdlog format so a
//  host can post-process either generation with the same tools.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 10 initial.
//------------------------------------------------------------------------------

unit OBD.Recorder;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.JSON,
  System.NetEncoding,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol;

type
  /// <summary>One recorder entry kind.</summary>
  TOBDLogEntryKind = (
    leInfo,
    leFrame,
    leResponse,
    leNRC,
    leError
  );

  /// <summary>One log entry pre-serialisation.</summary>
  TOBDLogEntry = record
    Timestamp: TDateTime;
    Kind: TOBDLogEntryKind;
    ElapsedMs: Cardinal;
    ServiceID: Byte;
    HasServiceID: Boolean;
    Raw: TBytes;
    FrameID: Cardinal;
    HasFrameID: Boolean;
    Extended: Boolean;
    NRC: Byte;
    HasNRC: Boolean;
    NRCText: string;
    Message: string;
  end;

  /// <summary>Recorder component. Drop on a form, point
  /// <c>Protocol</c> at a configured <see cref="TOBDProtocol"/>,
  /// call <c>Open(filename)</c>, then exercise the protocol —
  /// every event from the protocol is logged.</summary>
  TOBDRecorder = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FStream: TFileStream;
    FFileName: string;
    FLock: TCriticalSection;
    FActive: Boolean;
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure Subscribe;
    procedure Unsubscribe;
    procedure HandleFrame(Sender: TObject; const AFrame: TOBDFrame);
    procedure HandleResponse(Sender: TObject;
      const AResponse: TOBDResponse);
    procedure HandleNRC(Sender: TObject; const ARequest: TOBDRequest;
      ANRC: Byte; const AText: string);
    procedure HandleError(Sender: TObject; ACode: TOBDErrorCode;
      const AMessage: string; var AHandled: Boolean);
    procedure WriteLine(const ALine: string);
    function FormatTimestamp(ADt: TDateTime): string;
    function KindToText(AKind: TOBDLogEntryKind): string;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Opens / creates <c>AFileName</c> for append.
    /// Subscribes to the bound <c>Protocol</c>'s events.</summary>
    procedure Open(const AFileName: string);
    /// <summary>Closes the file and unsubscribes.</summary>
    procedure Close;
    /// <summary>Appends one entry. Hosts may call directly
    /// to record application-level info (start of test step,
    /// VIN, operator name, …).</summary>
    procedure Append(const AEntry: TOBDLogEntry);

    /// <summary>True when the recorder is open.</summary>
    property Active: Boolean read FActive;
    /// <summary>Currently-open file (empty when closed).</summary>
    property FileName: string read FFileName;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
  end;

implementation

uses
  System.TypInfo;

constructor TOBDRecorder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
end;

destructor TOBDRecorder.Destroy;
begin
  Close;
  FLock.Free;
  inherited;
end;

procedure TOBDRecorder.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  Unsubscribe;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
  Subscribe;
end;

procedure TOBDRecorder.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
  begin
    Unsubscribe;
    FProtocol := nil;
  end;
end;

procedure TOBDRecorder.Subscribe;
begin
  if (FProtocol = nil) or not FActive then Exit;
  FProtocol.OnFrame    := HandleFrame;
  FProtocol.OnResponse := HandleResponse;
  FProtocol.OnNRC      := HandleNRC;
  FProtocol.OnError    := HandleError;
end;

procedure TOBDRecorder.Unsubscribe;
begin
  if FProtocol = nil then Exit;
  // Best-effort: only clear handlers we own. Hosts that wired
  // their own handlers before connecting the recorder will need
  // to re-wire after Close.
  if @FProtocol.OnFrame    = @HandleFrame    then FProtocol.OnFrame    := nil;
  if @FProtocol.OnResponse = @HandleResponse then FProtocol.OnResponse := nil;
  if @FProtocol.OnNRC      = @HandleNRC      then FProtocol.OnNRC      := nil;
  if @FProtocol.OnError    = @HandleError    then FProtocol.OnError    := nil;
end;

function TOBDRecorder.FormatTimestamp(ADt: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"', ADt);
end;

function TOBDRecorder.KindToText(AKind: TOBDLogEntryKind): string;
begin
  case AKind of
    leInfo:     Result := 'info';
    leFrame:    Result := 'frame';
    leResponse: Result := 'response';
    leNRC:      Result := 'nrc';
    leError:    Result := 'error';
  else
    Result := 'info';
  end;
end;

procedure TOBDRecorder.WriteLine(const ALine: string);
var
  Bytes, NL: TBytes;
begin
  if FStream = nil then Exit;
  Bytes := TEncoding.UTF8.GetBytes(ALine);
  NL := TEncoding.UTF8.GetBytes(#10);
  FStream.WriteBuffer(Bytes[0], Length(Bytes));
  FStream.WriteBuffer(NL[0], 1);
end;

procedure TOBDRecorder.Open(const AFileName: string);
begin
  FLock.Enter;
  try
    Close;
    FFileName := AFileName;
    if FileExists(AFileName) then
      FStream := TFileStream.Create(AFileName,
        fmOpenReadWrite or fmShareDenyWrite)
    else
      FStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    FStream.Seek(0, soEnd);
    FActive := True;
  finally
    FLock.Leave;
  end;
  Subscribe;
end;

procedure TOBDRecorder.Close;
begin
  Unsubscribe;
  FLock.Enter;
  try
    if FStream <> nil then
    begin
      FStream.Free;
      FStream := nil;
    end;
    FActive := False;
    FFileName := '';
  finally
    FLock.Leave;
  end;
end;

procedure TOBDRecorder.Append(const AEntry: TOBDLogEntry);
var
  Obj: TJSONObject;
  Line: string;
begin
  FLock.Enter;
  try
    if FStream = nil then Exit;
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('ts', FormatTimestamp(AEntry.Timestamp));
      Obj.AddPair('kind', KindToText(AEntry.Kind));
      if AEntry.ElapsedMs > 0 then
        Obj.AddPair('elapsed_ms', TJSONNumber.Create(AEntry.ElapsedMs));
      if AEntry.HasServiceID then
        Obj.AddPair('service_id', '0x' + IntToHex(AEntry.ServiceID, 2));
      if Length(AEntry.Raw) > 0 then
        Obj.AddPair('raw',
          TNetEncoding.Base64.EncodeBytesToString(AEntry.Raw));
      if AEntry.HasFrameID then
      begin
        Obj.AddPair('id', '0x' + IntToHex(AEntry.FrameID, 0));
        Obj.AddPair('extended', TJSONBool.Create(AEntry.Extended));
      end;
      if AEntry.HasNRC then
      begin
        Obj.AddPair('nrc', '0x' + IntToHex(AEntry.NRC, 2));
        if AEntry.NRCText <> '' then
          Obj.AddPair('nrc_text', AEntry.NRCText);
      end;
      if AEntry.Message <> '' then
        Obj.AddPair('message', AEntry.Message);
      Line := Obj.ToJSON;
    finally
      Obj.Free;
    end;
    WriteLine(Line);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDRecorder.HandleFrame(Sender: TObject;
  const AFrame: TOBDFrame);
var
  Entry: TOBDLogEntry;
begin
  Entry := Default(TOBDLogEntry);
  Entry.Timestamp := AFrame.Timestamp;
  Entry.Kind := leFrame;
  Entry.Raw := AFrame.Payload;
  Entry.FrameID := AFrame.Id;
  Entry.HasFrameID := True;
  Entry.Extended := AFrame.IsExtendedId;
  Append(Entry);
end;

procedure TOBDRecorder.HandleResponse(Sender: TObject;
  const AResponse: TOBDResponse);
var
  Entry: TOBDLogEntry;
begin
  Entry := Default(TOBDLogEntry);
  Entry.Timestamp := Now;
  Entry.Kind := leResponse;
  Entry.ElapsedMs := AResponse.Elapsed;
  Entry.ServiceID := AResponse.ServiceID;
  Entry.HasServiceID := True;
  Entry.Raw := AResponse.Data;
  Append(Entry);
end;

procedure TOBDRecorder.HandleNRC(Sender: TObject;
  const ARequest: TOBDRequest; ANRC: Byte; const AText: string);
var
  Entry: TOBDLogEntry;
begin
  Entry := Default(TOBDLogEntry);
  Entry.Timestamp := Now;
  Entry.Kind := leNRC;
  Entry.ServiceID := ARequest.ServiceID;
  Entry.HasServiceID := True;
  Entry.NRC := ANRC;
  Entry.HasNRC := True;
  Entry.NRCText := AText;
  Append(Entry);
end;

procedure TOBDRecorder.HandleError(Sender: TObject;
  ACode: TOBDErrorCode; const AMessage: string;
  var AHandled: Boolean);
var
  Entry: TOBDLogEntry;
begin
  Entry := Default(TOBDLogEntry);
  Entry.Timestamp := Now;
  Entry.Kind := leError;
  Entry.Message := Format('[%s] %s',
    [GetEnumName(TypeInfo(TOBDErrorCode), Ord(ACode)), AMessage]);
  Append(Entry);
  AHandled := False;
end;

end.
