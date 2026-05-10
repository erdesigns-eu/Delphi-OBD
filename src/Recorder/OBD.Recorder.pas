//------------------------------------------------------------------------------
//  OBD.Recorder
//
//  TOBDRecorder — captures every protocol-level frame + response
//  + error to an append-only `.obdlog` file. Hosts use this both
//  as a forensic / audit trail and as the input for
//  <see cref="TOBDReplayer"/> (run a captured session against a
//  stub protocol stack for offline analysis).
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
//    2026-05-09  ERD  Initial implementation.
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
  System.ZLib,
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
  /// every event from the protocol is logged. When
  /// <c>filename</c> ends in <c>.gz</c> the recorder wraps the
  /// output stream with gzip compression (typical 5-10x size
  /// saving for diagnostic captures).</summary>
  TOBDRecorder = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FFileStream: TFileStream;
    FStream: TStream;             // = FFileStream OR a gzip wrapper
    FOwnsStream: Boolean;
    FFileName: string;
    FCompressed: Boolean;
    FLock: TCriticalSection;
    FActive: Boolean;
    FListenerId: Integer;         // 0 = not subscribed
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure Subscribe;
    procedure Unsubscribe;
    procedure HandleFrame(Sender: TObject; const AFrame: TOBDFrame);
    procedure HandleResponse(Sender: TObject;
      const AResponse: TOBDResponse);
    procedure HandleNRC(Sender: TObject; const ARequest: TOBDRequest;
      ANRC: Byte; const AText: string);
    procedure HandleError(Sender: TObject; ACode: TOBDErrorCode;
      const AMessage: string);
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
    /// <summary>True when the active log is gzip-compressed
    /// (file name ended in <c>.gz</c>).</summary>
    property Compressed: Boolean read FCompressed;
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
var
  Listener: TOBDProtocolListener;
begin
  if (FProtocol = nil) or not FActive or (FListenerId <> 0) then Exit;
  // Multi-cast registration via TOBDProtocol.AddListener. Co-
  // exists with any single-cast OnXxx handler the host has
  // wired, so attaching a recorder no longer clobbers the
  // host's plumbing.
  Listener.OnFrame    := HandleFrame;
  Listener.OnResponse := HandleResponse;
  Listener.OnNRC      := HandleNRC;
  Listener.OnError    := HandleError;
  FListenerId := FProtocol.AddListener(Listener);
end;

procedure TOBDRecorder.Unsubscribe;
begin
  if (FProtocol = nil) or (FListenerId = 0) then Exit;
  FProtocol.RemoveListener(FListenerId);
  FListenerId := 0;
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
const
  GZIP_WINDOW_BITS = 15 + 16; // System.ZLib magic — gzip wrapper
begin
  FLock.Enter;
  try
    Close;
    FFileName := AFileName;
    FCompressed := SameText(ExtractFileExt(AFileName), '.gz');

    if FCompressed then
    begin
      // Compressed: always create-truncate. Appending to a gzip
      // stream is technically valid (concatenated members) but
      // most readers don't expect it; keep the contract simple
      // and overwrite.
      FFileStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
      FStream := TZCompressionStream.Create(FFileStream,
        TZCompressionLevel.zcDefault, GZIP_WINDOW_BITS);
      FOwnsStream := True;
    end
    else
    begin
      if FileExists(AFileName) then
        FFileStream := TFileStream.Create(AFileName,
          fmOpenReadWrite or fmShareDenyWrite)
      else
        FFileStream := TFileStream.Create(AFileName,
          fmCreate or fmShareDenyWrite);
      FFileStream.Seek(0, soEnd);
      FStream := FFileStream;
      FOwnsStream := False;
    end;
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
    // Free the wrapper first (if any) so the gzip footer flushes
    // before we close the underlying file.
    if FOwnsStream and (FStream <> nil) then FStream.Free;
    FStream := nil;
    if FFileStream <> nil then
    begin
      FFileStream.Free;
      FFileStream := nil;
    end;
    FOwnsStream := False;
    FActive := False;
    FFileName := '';
    FCompressed := False;
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
  ACode: TOBDErrorCode; const AMessage: string);
var
  Entry: TOBDLogEntry;
begin
  Entry := Default(TOBDLogEntry);
  Entry.Timestamp := Now;
  Entry.Kind := leError;
  Entry.Message := Format('[%s] %s',
    [GetEnumName(TypeInfo(TOBDErrorCode), Ord(ACode)), AMessage]);
  Append(Entry);
end;

end.
