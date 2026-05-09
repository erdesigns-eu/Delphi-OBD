//------------------------------------------------------------------------------
//  Tests.OBD.Adapter.Detection
//
//  DUnitX coverage for TOBDAdapterDetector. Drives the detector with a
//  scripted IOBDAdapterCommandSender that maps verb → response without
//  any real connection.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 3 initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Adapter.Detection;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>Detection unit tests.</summary>
  [TestFixture]
  TAdapterDetectionTests = class
  public
    /// <summary>ATI = 'ELM327 v1.5' produces afELM327, version 1.5,
    /// likely-clone heuristic fires.</summary>
    [Test] procedure DetectsELM327V15Clone;
    /// <summary>ATI = 'ELM327 v2.3' is a genuine ELM327 not flagged
    /// as clone.</summary>
    [Test] procedure DetectsGenuineELM327V23;
    /// <summary>ATI containing 'OBDLink' resolves to afOBDLink with
    /// MX adapter-key.</summary>
    [Test] procedure DetectsOBDLinkMX;
    /// <summary>STN1110 chip identified as OBDLink family.</summary>
    [Test] procedure DetectsSTN1110;
    /// <summary>Six progress phases fire in order.</summary>
    [Test] procedure ProgressFiresAllSixPhases;
    /// <summary>ParseInfoLine extracts version from typical
    /// strings.</summary>
    [Test] procedure ParseInfoLineVariants;
    /// <summary>Nil sender raises EOBDAdapter.</summary>
    [Test] procedure NilSenderRaises;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types,
  OBD.Adapter.Types,
  OBD.Adapter.Detection;

type
  /// <summary>Scripted command-sender for tests: maps a verb to a
  /// canned response.</summary>
  TScriptedSender = class(TInterfacedObject, IOBDAdapterCommandSender)
  strict private
    FResponses: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Map(const AVerb, AResponse: string);
    function SendCommand(const ACommand: string;
      ATimeoutMs: Cardinal): TOBDAdapterResponse;
  end;

constructor TScriptedSender.Create;
begin
  inherited;
  FResponses := TDictionary<string, string>.Create;
end;

destructor TScriptedSender.Destroy;
begin
  FResponses.Free;
  inherited;
end;

procedure TScriptedSender.Map(const AVerb, AResponse: string);
begin
  FResponses.AddOrSetValue(UpperCase(Trim(AVerb)), AResponse);
end;

function TScriptedSender.SendCommand(const ACommand: string;
  ATimeoutMs: Cardinal): TOBDAdapterResponse;
var
  Body: string;
begin
  Result.Command := ACommand;
  Result.Elapsed := 0;
  Result.IsError := False;
  Result.ErrorKeyword := '';
  if FResponses.TryGetValue(UpperCase(Trim(ACommand)), Body) then
  begin
    Result.Raw := Body;
    Result.Lines := SplitString(Body, #13#10);
  end
  else
  begin
    Result.Raw := '?';
    Result.Lines := TArray<string>.Create('?');
    Result.IsError := True;
    Result.ErrorKeyword := '?';
  end;
end;

procedure TAdapterDetectionTests.DetectsELM327V15Clone;
var
  S: TScriptedSender;
  Sender: IOBDAdapterCommandSender;
  Identity: TOBDAdapterIdentity;
begin
  S := TScriptedSender.Create;
  Sender := S;
  S.Map('ATZ',  'ELM327 v1.5');
  S.Map('ATE0', 'OK');
  S.Map('ATI',  'ELM327 v1.5');
  // Clone hint: AT@1 / AT@2 unmapped -> '?' responses, leaving them empty.
  Assert.IsTrue(TOBDAdapterDetector.Detect(Sender, Identity));
  Assert.AreEqual(Ord(afELM327), Ord(Identity.Family));
  Assert.AreEqual('1.5', Identity.FirmwareVersion);
  Assert.IsTrue(Identity.IsClone, 'v1.5 with empty AT@1/AT@2 should be flagged as clone');
end;

procedure TAdapterDetectionTests.DetectsGenuineELM327V23;
var
  S: TScriptedSender;
  Sender: IOBDAdapterCommandSender;
  Identity: TOBDAdapterIdentity;
begin
  S := TScriptedSender.Create;
  Sender := S;
  S.Map('ATZ',  'ELM327 v2.3');
  S.Map('ATE0', 'OK');
  S.Map('ATI',  'ELM327 v2.3');
  S.Map('AT@1', 'ELM Electronics ELM327 OBD Diagnostics');
  S.Map('AT@2', 'ELM-ECU-2026-001');
  Assert.IsTrue(TOBDAdapterDetector.Detect(Sender, Identity));
  Assert.AreEqual('2.3', Identity.FirmwareVersion);
  Assert.IsFalse(Identity.IsClone);
end;

procedure TAdapterDetectionTests.DetectsOBDLinkMX;
var
  S: TScriptedSender;
  Sender: IOBDAdapterCommandSender;
  Identity: TOBDAdapterIdentity;
begin
  S := TScriptedSender.Create;
  Sender := S;
  S.Map('ATZ',  'OBDLink MX 4.2.0');
  S.Map('ATE0', 'OK');
  S.Map('ATI',  'OBDLink MX 4.2.0');
  S.Map('AT@1', 'OBDLink MX');
  S.Map('STI',  'OBDLink MX 4.2.0');
  Assert.IsTrue(TOBDAdapterDetector.Detect(Sender, Identity));
  Assert.AreEqual(Ord(afOBDLink), Ord(Identity.Family));
  Assert.AreEqual('obdlink_mx', Identity.AdapterKey);
end;

procedure TAdapterDetectionTests.DetectsSTN1110;
var
  S: TScriptedSender;
  Sender: IOBDAdapterCommandSender;
  Identity: TOBDAdapterIdentity;
begin
  S := TScriptedSender.Create;
  Sender := S;
  S.Map('ATZ',  'STN1110 v3.1.0');
  S.Map('ATE0', 'OK');
  S.Map('ATI',  'STN1110 v3.1.0');
  Assert.IsTrue(TOBDAdapterDetector.Detect(Sender, Identity));
  Assert.AreEqual(Ord(afOBDLink), Ord(Identity.Family));
end;

procedure TAdapterDetectionTests.ProgressFiresAllSixPhases;
var
  S: TScriptedSender;
  Sender: IOBDAdapterCommandSender;
  Identity: TOBDAdapterIdentity;
  Indices: TArray<Cardinal>;
  CB: TOBDDetectionProgress;
begin
  S := TScriptedSender.Create;
  Sender := S;
  S.Map('ATZ',  'ELM327 v2.3');
  S.Map('ATE0', 'OK');
  S.Map('ATI',  'ELM327 v2.3');
  CB :=
    procedure(AIndex, ACount: Cardinal; const AName, ADetail: string)
    begin
      SetLength(Indices, Length(Indices) + 1);
      Indices[High(Indices)] := AIndex;
    end;
  TOBDAdapterDetector.Detect(Sender, Identity, CB);
  Assert.AreEqual<NativeInt>(6, Length(Indices));
  Assert.AreEqual<Cardinal>(1, Indices[0]);
  Assert.AreEqual<Cardinal>(6, Indices[5]);
end;

procedure TAdapterDetectionTests.ParseInfoLineVariants;
var
  Family: TOBDAdapterFamily;
  Version: string;
begin
  Assert.IsTrue(TOBDAdapterDetector.ParseInfoLine('ELM327 v1.5',
    Family, Version));
  Assert.AreEqual(Ord(afELM327), Ord(Family));
  Assert.AreEqual('1.5', Version);

  Assert.IsTrue(TOBDAdapterDetector.ParseInfoLine('OBDLink MX+ 4.2.0',
    Family, Version));
  Assert.AreEqual(Ord(afOBDLink), Ord(Family));
  Assert.AreEqual('4.2.0', Version);
end;

procedure TAdapterDetectionTests.NilSenderRaises;
var
  Identity: TOBDAdapterIdentity;
begin
  Assert.WillRaise(
    procedure
    begin
      TOBDAdapterDetector.Detect(nil, Identity);
    end,
    EOBDAdapter);
end;

initialization
  TDUnitX.RegisterTestFixture(TAdapterDetectionTests);

end.
