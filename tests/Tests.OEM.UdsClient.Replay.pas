//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.UdsClient.Replay
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : End-to-end round-trip test for the new
//                  IOBDUdsClient: feeds a recorded .obdlog into a
//                  capture-replay transport, drives ReadDID through
//                  the shipped OEM catalog, and asserts the decoded
//                  values bit-exactly match the recorded payloads.
//                  This is the first regression test that exercises
//                  the catalog loader + UDS client + DID decoder
//                  together against real wire data.
//------------------------------------------------------------------------------
unit Tests.OEM.UdsClient.Replay;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TUdsClientReplayTests = class
  public
    /// <summary>V w  v i n  decodes from captured f190.</summary>
    [Test] procedure VW_VIN_DecodesFromCapturedF190;
    /// <summary>V w  hardware number  decodes from captured f187.</summary>
    [Test] procedure VW_HardwareNumber_DecodesFromCapturedF187;
    /// <summary>V w  unknown d i d  raises catalog miss.</summary>
    [Test] procedure VW_UnknownDID_RaisesCatalogMiss;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils,
  System.Generics.Collections, System.Character,
  OBD.OEM.Catalog.JSON, OBD.OEM.Catalog.Loader,
  OBD.OEM.UdsClient, OBD.OEM.Captures, OBD.Service.Recorder;

//==============================================================================
// Helpers
//==============================================================================

function HexStringToBytes(const Hex: string): TBytes;
var
  Clean: string;
  C: Char;
  I, B: Integer;
begin
  Clean := '';
  for C in Hex do
    if (C <> ' ') and (C <> #9) and (C <> #13) and (C <> #10) then
      Clean := Clean + C;
  if Odd(Length(Clean)) then
    raise Exception.Create('odd-length hex: ' + Hex);
  SetLength(Result, Length(Clean) div 2);
  I := 1;
  for B := 0 to High(Result) do
  begin
    Result[B] := StrToInt('$' + Copy(Clean, I, 2));
    Inc(I, 2);
  end;
end;

function FixturePath(const FileName: string): string;
var
  Candidate: string;
  CWD: string;
begin
  CWD := GetCurrentDir;
  Candidate := TPath.Combine(CWD, TPath.Combine('fixtures',
    TPath.Combine('captures', FileName)));
  if TFile.Exists(Candidate) then Exit(Candidate);
  Candidate := TPath.Combine(CWD, TPath.Combine('tests',
    TPath.Combine('fixtures', TPath.Combine('captures', FileName))));
  if TFile.Exists(Candidate) then Exit(Candidate);
  Candidate := TPath.Combine(TPath.Combine(CWD, '..'),
    TPath.Combine('fixtures', TPath.Combine('captures', FileName)));
  Result := TPath.GetFullPath(Candidate);
end;

//==============================================================================
// Replay transport — request-keyed lookup over loaded capture pairs.
//==============================================================================
type
  TCapturePair = record
    Request: TBytes;
    Response: TBytes;
  end;

  TCaptureReplayTransport = class(TInterfacedObject, IOBDDiagnosticTransport)
  strict private
    FECU: Word;
    FPairs: TList<TCapturePair>;
    FConsumed: array of Boolean;
  public
    constructor Create(const Pairs: TArray<TOBDCapturePair>);
    destructor Destroy; override;
    function  SendReceive(const Request: TBytes;
                          TimeoutMs: Cardinal = 1500): TBytes;
    procedure SetTargetECU(Address: Word);
    function  TargetECU: Word;
  end;

constructor TCaptureReplayTransport.Create(
  const Pairs: TArray<TOBDCapturePair>);
var
  P: TOBDCapturePair;
  Item: TCapturePair;
begin
  inherited Create;
  FPairs := TList<TCapturePair>.Create;
  for P in Pairs do
  begin
    Item.Request := HexStringToBytes(P.RequestText);
    Item.Response := HexStringToBytes(NormalizeResponseText(P.ResponseText));
    FPairs.Add(Item);
  end;
  SetLength(FConsumed, FPairs.Count);
end;

destructor TCaptureReplayTransport.Destroy;
begin
  FPairs.Free;
  inherited;
end;

function BytesEqual(const A, B: TBytes): Boolean;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then Exit(False);
  for I := 0 to High(A) do
    if A[I] <> B[I] then Exit(False);
  Result := True;
end;

function TCaptureReplayTransport.SendReceive(const Request: TBytes;
  TimeoutMs: Cardinal): TBytes;
var
  I: Integer;
begin
  for I := 0 to FPairs.Count - 1 do
    if (not FConsumed[I]) and BytesEqual(FPairs[I].Request, Request) then
    begin
      FConsumed[I] := True;
      Exit(FPairs[I].Response);
    end;
  raise Exception.CreateFmt(
    'replay: no pair matches request (%d bytes)', [Length(Request)]);
end;

procedure TCaptureReplayTransport.SetTargetECU(Address: Word);
begin FECU := Address; end;

function TCaptureReplayTransport.TargetECU: Word;
begin Result := FECU; end;

//==============================================================================
// Tests
//==============================================================================
function LoadVwCapture: TArray<TOBDCapturePair>;
var
  Path: string;
  Replayer: TOBDReplayer;
begin
  Path := FixturePath('sample-vw.obdlog');
  if not TFile.Exists(Path) then
    Assert.Pass('sample-vw.obdlog not on path; skipping');
  Replayer := TOBDReplayer.Create;
  try
    Replayer.LoadFromFile(Path);
    Result := ExtractCapturePairs(Replayer.Entries);
  finally
    Replayer.Free;
  end;
end;

function LoadVwCatalog: TOBDOEMJSONCatalog;
var
  Path: string;
begin
  Path := ResolveCatalogPath('vw.json');
  if Path = '' then
    Assert.Pass('vw.json not on path; skipping');
  Result := TOBDOEMJSONCatalog.Create(Path);
end;

procedure TUdsClientReplayTests.VW_VIN_DecodesFromCapturedF190;
var
  Pairs: TArray<TOBDCapturePair>;
  Catalog: TOBDOEMJSONCatalog;
  Transport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Decoded: TOBDDecodedValue;
begin
  Pairs := LoadVwCapture;
  Catalog := LoadVwCatalog;
  try
    Transport := TCaptureReplayTransport.Create(Pairs);
    Client := CreateUdsClient;
    Client.OpenSession(Catalog, Transport, $7E0);
    try
      Decoded := Client.ReadDID('vin');
      // Capture bytes 57 56 57 5A 5A 5A 38 4E 38 5A 31 32 33 34 35 36 37
      Assert.AreEqual('WVWZZZ8N8Z1234567', Trim(Decoded.AsString));
    finally
      Client.CloseSession;
    end;
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientReplayTests.VW_HardwareNumber_DecodesFromCapturedF187;
var
  Pairs: TArray<TOBDCapturePair>;
  Catalog: TOBDOEMJSONCatalog;
  Transport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Decoded: TOBDDecodedValue;
begin
  Pairs := LoadVwCapture;
  Catalog := LoadVwCatalog;
  try
    Transport := TCaptureReplayTransport.Create(Pairs);
    Client := CreateUdsClient;
    Client.OpenSession(Catalog, Transport, $7E0);
    try
      // F187 — VAG part-number string
      Decoded := Client.ReadDID('0xF187');
      // Capture bytes 30 34 4C 39 30 36 30 35 36 41 41 = "04L906056AA"
      Assert.AreEqual('04L906056AA', Trim(Decoded.AsString));
    finally
      Client.CloseSession;
    end;
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientReplayTests.VW_UnknownDID_RaisesCatalogMiss;
var
  Pairs: TArray<TOBDCapturePair>;
  Catalog: TOBDOEMJSONCatalog;
  Transport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
begin
  // Sanity: the capture-replay path must surface a missing DID as
  // a catalog miss (no transport call) — important for tools that
  // walk a capture and try to decode every recorded request.
  Pairs := LoadVwCapture;
  Catalog := LoadVwCatalog;
  try
    Transport := TCaptureReplayTransport.Create(Pairs);
    Client := CreateUdsClient;
    Client.OpenSession(Catalog, Transport, $7E0);
    try
      Assert.WillRaise(
        procedure begin Client.ReadDID('0xDEAD'); end,
        EOBDUdsCatalogMiss);
    finally
      Client.CloseSession;
    end;
  finally
    Catalog.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TUdsClientReplayTests);

end.
