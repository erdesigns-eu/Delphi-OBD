//------------------------------------------------------------------------------
//  OBD.Diagnostics.KWP.ReadID
//
//  TOBDKWPReadID — non-visual component for the KWP2000 read-side
//  identifier services:
//
//    Service 0x1A — ReadECUIdentification        (1-byte ID)
//    Service 0x21 — ReadDataByLocalIdentifier    (1-byte LocalID)
//    Service 0x22 — ReadDataByCommonIdentifier   (2-byte DID)
//
//  Each service returns the requested identifier's bytes verbatim.
//  The component does NOT decode them — the host owns the
//  vendor-specific record layouts.
//
//  Wire format per ISO 14230-3:1999 §6.7:
//
//    ReadECUId   request : 1A <id>
//    ReadECUId   response: 5A <id> <data...>
//
//    ReadByLocal request : 21 <localId>
//    ReadByLocal response: 61 <localId> <data...>
//
//    ReadByCommon request : 22 <commonId-hi> <commonId-lo>
//    ReadByCommon response: 62 <commonId-hi> <commonId-lo> <data...>
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14230-3:1999 §6.7 (Read identification services)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.KWP.ReadID;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.KWP2000,
  OBD.Protocol;

type
  /// <summary>
  ///   Fires after a successful read.
  /// </summary>
  /// <remarks>
  ///   <c>AKind</c> echoes the service-ID byte requested
  ///   (0x1A / 0x21 / 0x22). <c>AID</c> is the read identifier
  ///   value (the high byte is 0 for 1-byte services). Main thread.
  /// </remarks>
  TOBDKWPReadIDEvent = procedure(Sender: TObject; AKind: Byte;
    AID: Word; const AData: TBytes) of object;

  /// <summary>
  ///   KWP2000 read-identifier component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form, assign <c>Protocol</c>, and
  ///   call one of <see cref="ReadECUID"/>,
  ///   <see cref="ReadByLocalID"/> or <see cref="ReadByCommonID"/>.
  /// </remarks>
  TOBDKWPReadID = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnRead: TOBDKWPReadIDEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoRead(AService: Byte; const AReq: TBytes;
      AEchoBytes: Integer): TBytes;
    procedure FireRead(AKind: Byte; AID: Word;
      const AData: TBytes);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Service 0x1A — ReadECUIdentification.
    /// </summary>
    /// <param name="AID">1-byte identifier value.</param>
    /// <returns>Identifier payload bytes.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response, or the echoed
    ///   identifier did not match.
    /// </exception>
    function ReadECUID(AID: Byte): TBytes;

    /// <summary>
    ///   Service 0x21 — ReadDataByLocalIdentifier.
    /// </summary>
    /// <param name="ALocalID">1-byte local identifier.</param>
    /// <returns>Identifier payload bytes.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response, or the echoed
    ///   identifier did not match.
    /// </exception>
    function ReadByLocalID(ALocalID: Byte): TBytes;

    /// <summary>
    ///   Service 0x22 — ReadDataByCommonIdentifier.
    /// </summary>
    /// <param name="ACommonID">2-byte common identifier.</param>
    /// <returns>Identifier payload bytes.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response, or the echoed
    ///   identifier did not match.
    /// </exception>
    function ReadByCommonID(ACommonID: Word): TBytes;

    /// <summary>Non-blocking <see cref="ReadECUID"/>.</summary>
    /// <param name="AID">1-byte identifier.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadECUIDAsync(AID: Byte);

    /// <summary>Non-blocking <see cref="ReadByLocalID"/>.</summary>
    /// <param name="ALocalID">1-byte local identifier.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadByLocalIDAsync(ALocalID: Byte);

    /// <summary>Non-blocking <see cref="ReadByCommonID"/>.</summary>
    /// <param name="ACommonID">2-byte common identifier.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadByCommonIDAsync(ACommonID: Word);
  published
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires after a successful read. Main thread.</summary>
    property OnRead: TOBDKWPReadIDEvent read FOnRead write FOnRead;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDKWPReadID.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDKWPReadID.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDKWPReadID.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDKWPReadID.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDKWPReadID.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDKWPReadID: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDKWPReadID.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDKWPReadID.DoRead(AService: Byte; const AReq: TBytes;
  AEchoBytes: Integer): TBytes;
var
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWPReadID: Protocol not assigned');
  Resp := FProtocol.Request(AService, AReq);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'KWP service 0x%.2x negative: %s', [AService, Resp.NRCText]);
  if Length(Resp.Data) < AEchoBytes then
    raise EOBDProtocolErr.CreateFmt(
      'KWP service 0x%.2x: response too short', [AService]);
  // Verify the echo bytes match the request body — KWP services
  // always echo the requested identifier in the response.
  if AEchoBytes > 0 then
  begin
    if not CompareMem(@AReq[0], @Resp.Data[0], AEchoBytes) then
      raise EOBDProtocolErr.CreateFmt(
        'KWP service 0x%.2x: identifier echo mismatch', [AService]);
  end;
  if Length(Resp.Data) > AEchoBytes then
    Result := Copy(Resp.Data, AEchoBytes,
                    Length(Resp.Data) - AEchoBytes)
  else
    SetLength(Result, 0);
end;

function TOBDKWPReadID.ReadECUID(AID: Byte): TBytes;
var
  Req: TBytes;
begin
  SetLength(Req, 1);
  Req[0] := AID;
  Result := DoRead(KWP_SID_ReadECUIdentification, Req, 1);
  FireRead(KWP_SID_ReadECUIdentification, AID, Result);
end;

function TOBDKWPReadID.ReadByLocalID(ALocalID: Byte): TBytes;
var
  Req: TBytes;
begin
  SetLength(Req, 1);
  Req[0] := ALocalID;
  Result := DoRead(KWP_SID_ReadDataByLocalIdentifier, Req, 1);
  FireRead(KWP_SID_ReadDataByLocalIdentifier, ALocalID, Result);
end;

function TOBDKWPReadID.ReadByCommonID(ACommonID: Word): TBytes;
var
  Req: TBytes;
begin
  SetLength(Req, 2);
  Req[0] := Byte((ACommonID shr 8) and $FF);
  Req[1] := Byte(ACommonID and $FF);
  Result := DoRead(KWP_SID_ReadDataByCommonIdentifier, Req, 2);
  FireRead(KWP_SID_ReadDataByCommonIdentifier, ACommonID, Result);
end;

procedure TOBDKWPReadID.ReadECUIDAsync(AID: Byte);
var
  Self_: TOBDKWPReadID;
  ID: Byte;
begin
  GuardSingleAsync;
  Self_ := Self;
  ID := AID;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadECUID(ID);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDKWPReadID.ReadByLocalIDAsync(ALocalID: Byte);
var
  Self_: TOBDKWPReadID;
  LID: Byte;
begin
  GuardSingleAsync;
  Self_ := Self;
  LID := ALocalID;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadByLocalID(LID);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDKWPReadID.ReadByCommonIDAsync(ACommonID: Word);
var
  Self_: TOBDKWPReadID;
  CID: Word;
begin
  GuardSingleAsync;
  Self_ := Self;
  CID := ACommonID;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadByCommonID(CID);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDKWPReadID.FireRead(AKind: Byte; AID: Word;
  const AData: TBytes);
var
  Self_: TOBDKWPReadID;
  Kind: Byte;
  IDValue: Word;
  Snap: TBytes;
begin
  if not Assigned(FOnRead) then
    Exit;
  Self_ := Self;
  Kind := AKind;
  IDValue := AID;
  Snap := Copy(AData, 0, Length(AData));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRead(Self_, Kind, IDValue, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnRead) then
          Self_.FOnRead(Self_, Kind, IDValue, Snap);
      end);
end;

procedure TOBDKWPReadID.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDKWPReadID;
  Code: TOBDErrorCode;
  Msg: string;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil,
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
