//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.KWP1281.TransportELM
//
//  Pinned-vector tests for the ELM327 KWP1281 transport. A
//  scripted underlying transport answers AT commands with the
//  bytes a real ELM327 v1.5 would return so the auto-detect
//  + ATSI 5-baud-init path can be verified without hardware.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.KWP1281.TransportELM;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Transport.Base,
  OBD.Protocol.KWP1281,
  OBD.Protocol.KWP1281.Transport.ELM;

type
  /// <summary>Mocks the ELM327 by matching command strings to
  /// scripted reply strings (each ending with ">"). Anything
  /// not in the script returns a bare prompt.</summary>
  TFakeElmTransport = class(TOBDBaseTransport)
  strict private
    FScript: TDictionary<string, string>;
    FInbox:  TStringBuilder;
  public
    constructor Create;
    destructor  Destroy; override;
    /// <summary>Maps an AT command (case-insensitive, no
    /// trailing CR) to the ASCII reply the fake should send
    /// back next time it sees that command.</summary>
    procedure   Script(const ACmd, AReply: string);
    procedure   Close; override;
    function    WriteBytes(const ABytes: TBytes): Integer; override;
  end;

  [TestFixture]
  TKWP1281ElmTransportTests = class
  public
    [Test] procedure DetectsElmVersionViaATI;
    [Test] procedure AtSiInitParsesSyncAndKeyBytes;
    [Test] procedure AtSiInitFailsLoudlyOnWrongSync;
  end;

implementation

{ TFakeElmTransport -----------------------------------------------------------}

constructor TFakeElmTransport.Create;
begin
  inherited Create;
  FScript := TDictionary<string, string>.Create;
  FInbox  := TStringBuilder.Create;
end;

destructor TFakeElmTransport.Destroy;
begin
  FScript.Free;
  FInbox.Free;
  inherited;
end;

procedure TFakeElmTransport.Script(const ACmd, AReply: string);
begin
  FScript.AddOrSetValue(UpperCase(Trim(ACmd)), AReply);
end;

procedure TFakeElmTransport.Close;
begin
  // No-op for the fake.
end;

function TFakeElmTransport.WriteBytes(const ABytes: TBytes): Integer;
var
  Cmd, Reply, AsText: string;
  ReplyBytes: TBytes;
begin
  AsText := TEncoding.ASCII.GetString(ABytes);
  FInbox.Append(AsText);

  // Once we see a CR, take everything since last CR as the cmd.
  if Pos(#13, AsText) > 0 then
  begin
    Cmd := UpperCase(Trim(StringReplace(FInbox.ToString, #13, ' ',
      [rfReplaceAll])));
    FInbox.Clear;
    if not FScript.TryGetValue(Cmd, Reply) then
      Reply := '>';
    if (Reply = '') or (Reply[Length(Reply)] <> '>') then
      Reply := Reply + '>';
    ReplyBytes := TEncoding.ASCII.GetBytes(Reply);
    FireBytes(ReplyBytes);
  end;
  Result := Length(ABytes);
end;

{ TKWP1281ElmTransportTests ---------------------------------------------------}

procedure TKWP1281ElmTransportTests.DetectsElmVersionViaATI;
var
  Fake:  TFakeElmTransport;
  Iface: IOBDConnectionTransport;
  Elm:   TKWP1281ELMTransport;
  KW1, KW2: Byte;
begin
  Fake := TFakeElmTransport.Create;
  Iface := Fake;
  Fake.Script('ATI',     'ELM327 v1.5'#13);
  Fake.Script('AT IIA 56', 'OK'#13);
  Fake.Script('AT KW0',  'OK'#13);
  Fake.Script('AT SI',   '55 01 8A'#13);
  Elm := TKWP1281ELMTransport.Create(Iface, imAuto);
  try
    Elm.FiveBaudInit($56, KW1, KW2, 2000);
    Assert.IsTrue(Elm.ElmVersion.Contains('ELM327'),
      'Version not parsed: ' + Elm.ElmVersion);
    Assert.AreEqual(Byte($01), KW1);
    Assert.AreEqual(Byte($8A), KW2);
  finally
    Elm.Free;
    Iface := nil;  // releases Fake via interface ref-count
  end;
end;

procedure TKWP1281ElmTransportTests.AtSiInitParsesSyncAndKeyBytes;
var
  Fake:  TFakeElmTransport;
  Iface: IOBDConnectionTransport;
  Elm:   TKWP1281ELMTransport;
  KW1, KW2: Byte;
begin
  Fake := TFakeElmTransport.Create;
  Iface := Fake;
  Fake.Script('ATI',     'ELM327 v1.5'#13);
  Fake.Script('AT IIA 17', 'OK'#13);
  Fake.Script('AT KW0',  'OK'#13);
  Fake.Script('AT SI',   '55 4A 4F'#13);
  Elm := TKWP1281ELMTransport.Create(Iface, imAtSi);
  try
    Elm.FiveBaudInit($17, KW1, KW2, 2000);
    Assert.AreEqual(Byte($4A), KW1);
    Assert.AreEqual(Byte($4F), KW2);
  finally
    Elm.Free;
    Iface := nil;
  end;
end;

procedure TKWP1281ElmTransportTests.AtSiInitFailsLoudlyOnWrongSync;
var
  Fake:  TFakeElmTransport;
  Iface: IOBDConnectionTransport;
  Elm:   TKWP1281ELMTransport;
  KW1, KW2: Byte;
begin
  Fake := TFakeElmTransport.Create;
  Iface := Fake;
  Fake.Script('ATI',     'ELM327 v1.5'#13);
  Fake.Script('AT IIA 56', 'OK'#13);
  Fake.Script('AT KW0',  'OK'#13);
  Fake.Script('AT SI',   '00 01 8A'#13);  // wrong sync byte
  Elm := TKWP1281ELMTransport.Create(Iface, imAtSi);
  try
    Assert.WillRaise(
      procedure begin Elm.FiveBaudInit($56, KW1, KW2, 2000) end,
      EKWP1281Error);
  finally
    Elm.Free;
    Iface := nil;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TKWP1281ElmTransportTests);

end.
