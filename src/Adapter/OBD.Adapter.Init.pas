//------------------------------------------------------------------------------
//  OBD.Adapter.Init
//
//  Per-family init sequence. Built-ins are baked into this unit so the
//  adapter is functional without any catalogue file; sequences may be
//  overridden via JSON for OEM-specific quirks.
//
//  The initializer runs each AT/ST step through the same
//  IOBDAdapterCommandSender abstraction the detector uses; a missing
//  optional step (e.g. STM on a non-OBDLink chip) does not abort the
//  sequence — only required steps do.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 3 initial: built-in sequences for ELM327 /
//                     OBDLink / J2534 / DoIP, JSON loader stub for
//                     overrides.
//------------------------------------------------------------------------------

unit OBD.Adapter.Init;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  OBD.Types,
  OBD.Adapter.Types,
  OBD.Adapter.Detection;

type
  /// <summary>
  ///   Single step in an init sequence.
  /// </summary>
  TOBDInitStep = record
    /// <summary>Command verb (already formatted; AT or ST).</summary>
    Verb: string;
    /// <summary>Human-readable phase name; surfaces via
    /// <c>OnProgress</c>.</summary>
    Name: string;
    /// <summary>If True, a non-success response (or timeout) aborts
    /// the sequence with <see cref="EOBDAdapter"/>. If False, the
    /// step is best-effort.</summary>
    Required: Boolean;
  end;

  /// <summary>Sequence of init steps for a given family.</summary>
  TOBDInitSequence = TArray<TOBDInitStep>;

  /// <summary>
  ///   Stateless initializer.
  /// </summary>
  /// <remarks>
  ///   <c>Run</c> walks the sequence in order. Each step fires a
  ///   step-progress event with <c>Index/Count</c> matching its
  ///   position in the sequence. A required step that fails (timeout,
  ///   error keyword in the response) raises
  ///   <see cref="EOBDAdapter"/>.
  /// </remarks>
  TOBDAdapterInitializer = class
  strict private
    class var FOverrides: TDictionary<TOBDAdapterFamily, TOBDInitSequence>;
    class procedure EnsureOverrides; static;
  public
    /// <summary>Releases the override registry. Called from this
    /// unit's finalisation.</summary>
    class procedure ReleaseOverrides; static;

    /// <summary>
    ///   Returns the resolved sequence for the given family. Falls
    ///   back to the in-source built-in when no JSON override is
    ///   registered.
    /// </summary>
    /// <param name="AFamily">Adapter family.</param>
    /// <returns>Array of steps. Empty for families that don't have an
    /// AT/ST init phase (J2534, DoIP).</returns>
    class function ResolvedSequence(
      AFamily: TOBDAdapterFamily): TOBDInitSequence; static;

    /// <summary>
    ///   Returns the in-source built-in sequence for the given
    ///   family, ignoring any JSON overrides.
    /// </summary>
    /// <param name="AFamily">Adapter family.</param>
    /// <returns>Array of steps.</returns>
    class function BuiltinSequence(
      AFamily: TOBDAdapterFamily): TOBDInitSequence; static;

    /// <summary>
    ///   Replaces (or registers) the override sequence for a family.
    ///   Pass an empty array to clear the override.
    /// </summary>
    /// <param name="AFamily">Family to override.</param>
    /// <param name="ASequence">Sequence to use in place of the
    /// built-in.</param>
    class procedure RegisterOverride(AFamily: TOBDAdapterFamily;
      const ASequence: TOBDInitSequence); static;

    /// <summary>
    ///   Loads override sequences from a JSON file (matching the
    ///   <c>catalogs/adapter/init-sequences.json</c> schema). Each
    ///   recognised family's array is registered as an override; the
    ///   built-ins remain in place for families the file does not
    ///   mention.
    /// </summary>
    /// <param name="AFileName">Path to the JSON file.</param>
    /// <returns>Number of family overrides registered.</returns>
    /// <exception cref="EOBDConfig">File missing, malformed JSON, or
    /// schema-incompatible.</exception>
    class function LoadFromJSON(const AFileName: string): Integer; static;

    /// <summary>
    ///   Concatenates a base sequence with optional user-supplied
    ///   commands (each treated as best-effort, name = the verb).
    /// </summary>
    /// <param name="ABase">Base sequence.</param>
    /// <param name="AExtra">Extra commands to append.</param>
    /// <returns>Merged sequence.</returns>
    class function ExtendSequence(const ABase: TOBDInitSequence;
      const AExtra: TStrings): TOBDInitSequence; static;

    /// <summary>
    ///   Runs an init sequence through the supplied sender.
    /// </summary>
    /// <param name="ASender">Command sender. Must not be
    /// <c>nil</c>.</param>
    /// <param name="ASequence">Steps to execute, in order.</param>
    /// <param name="AOnProgress">Optional progress callback.</param>
    /// <param name="ATimeoutMs">Per-step timeout. Default 5000.</param>
    /// <exception cref="EOBDAdapter">Sender is <c>nil</c> or a
    /// required step failed.</exception>
    class procedure Run(const ASender: IOBDAdapterCommandSender;
      const ASequence: TOBDInitSequence;
      const AOnProgress: TOBDDetectionProgress = nil;
      ATimeoutMs: Cardinal = 5000); static;
  end;

implementation

{ ---- helpers ----------------------------------------------------------------- }

function MakeStep(const AVerb, AName: string; ARequired: Boolean): TOBDInitStep;
begin
  Result.Verb := AVerb;
  Result.Name := AName;
  Result.Required := ARequired;
end;

procedure ReportProgress(const ACallback: TOBDDetectionProgress;
  AIndex, ACount: Cardinal; const AName, ADetail: string);
begin
  if Assigned(ACallback) then
    ACallback(AIndex, ACount, AName, ADetail);
end;

{ ---- TOBDAdapterInitializer -------------------------------------------------- }

class procedure TOBDAdapterInitializer.EnsureOverrides;
begin
  if FOverrides = nil then
    FOverrides := TDictionary<TOBDAdapterFamily, TOBDInitSequence>.Create;
end;

class procedure TOBDAdapterInitializer.ReleaseOverrides;
begin
  FreeAndNil(FOverrides);
end;

class procedure TOBDAdapterInitializer.RegisterOverride(
  AFamily: TOBDAdapterFamily; const ASequence: TOBDInitSequence);
begin
  EnsureOverrides;
  if Length(ASequence) = 0 then
    FOverrides.Remove(AFamily)
  else
    FOverrides.AddOrSetValue(AFamily, Copy(ASequence));
end;

class function TOBDAdapterInitializer.ResolvedSequence(
  AFamily: TOBDAdapterFamily): TOBDInitSequence;
var
  Seq: TOBDInitSequence;
begin
  EnsureOverrides;
  if FOverrides.TryGetValue(AFamily, Seq) then
    Exit(Copy(Seq));
  Result := BuiltinSequence(AFamily);
end;

function FamilyFromString(const AKey: string;
  out AFamily: TOBDAdapterFamily): Boolean;
var
  Norm: string;
begin
  Norm := LowerCase(Trim(AKey));
  Result := True;
  if      Norm = 'elm327'  then AFamily := afELM327
  else if Norm = 'obdlink' then AFamily := afOBDLink
  else if Norm = 'j2534'   then AFamily := afJ2534
  else if Norm = 'doip'    then AFamily := afDoIP
  else Result := False;
end;

class function TOBDAdapterInitializer.LoadFromJSON(
  const AFileName: string): Integer;
var
  Json: string;
  Doc: TJSONValue;
  Root, FamilyObj, StepObj: TJSONObject;
  Families, Steps: TJSONArray;
  KeyVal: TJSONValue;
  TypeStr: string;
  Version: Integer;
  I, J: Integer;
  Family: TOBDAdapterFamily;
  FamilyKey: string;
  Sequence: TOBDInitSequence;
  Step: TOBDInitStep;
begin
  if not TFile.Exists(AFileName) then
    raise EOBDConfig.CreateFmt('Init-sequences file not found: %s',
      [AFileName]);

  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json);
  if Doc = nil then
    raise EOBDConfig.CreateFmt('%s: invalid JSON', [AFileName]);

  Result := 0;
  try
    if not (Doc is TJSONObject) then
      raise EOBDConfig.CreateFmt('%s: root is not an object', [AFileName]);
    Root := Doc as TJSONObject;

    KeyVal := Root.GetValue('version');
    if (KeyVal <> nil) and (KeyVal is TJSONNumber) then
      Version := (KeyVal as TJSONNumber).AsInt
    else
      Version := 1;
    if Version <> 1 then
      raise EOBDConfig.CreateFmt('%s: unsupported schema version %d',
        [AFileName, Version]);

    KeyVal := Root.GetValue('type');
    if KeyVal <> nil then
      TypeStr := KeyVal.Value
    else
      TypeStr := 'adapter-init-sequences';
    if not SameText(TypeStr, 'adapter-init-sequences') then
      raise EOBDConfig.CreateFmt(
        '%s: type "%s" is not "adapter-init-sequences"',
        [AFileName, TypeStr]);

    if not (Root.GetValue('families') is TJSONArray) then
      raise EOBDConfig.CreateFmt('%s: "families" must be an array',
        [AFileName]);
    Families := Root.GetValue('families') as TJSONArray;

    for I := 0 to Families.Count - 1 do
    begin
      if not (Families.Items[I] is TJSONObject) then
        raise EOBDConfig.CreateFmt('%s: families[%d] is not an object',
          [AFileName, I]);
      FamilyObj := Families.Items[I] as TJSONObject;

      KeyVal := FamilyObj.GetValue('family');
      if KeyVal = nil then
        raise EOBDConfig.CreateFmt(
          '%s: families[%d].family missing', [AFileName, I]);
      FamilyKey := KeyVal.Value;
      if not FamilyFromString(FamilyKey, Family) then
        // Unknown family — skip silently rather than raise; lets
        // future families be added to the JSON without breaking
        // older builds.
        Continue;

      if not (FamilyObj.GetValue('steps') is TJSONArray) then
        raise EOBDConfig.CreateFmt(
          '%s: families[%d].steps must be an array', [AFileName, I]);
      Steps := FamilyObj.GetValue('steps') as TJSONArray;

      SetLength(Sequence, 0);
      for J := 0 to Steps.Count - 1 do
      begin
        if not (Steps.Items[J] is TJSONObject) then
          raise EOBDConfig.CreateFmt(
            '%s: families[%d].steps[%d] is not an object',
            [AFileName, I, J]);
        StepObj := Steps.Items[J] as TJSONObject;

        Step.Verb := '';
        Step.Name := '';
        Step.Required := False;

        KeyVal := StepObj.GetValue('verb');
        if KeyVal <> nil then Step.Verb := KeyVal.Value;
        if Trim(Step.Verb) = '' then
          raise EOBDConfig.CreateFmt(
            '%s: families[%d].steps[%d].verb is empty',
            [AFileName, I, J]);

        KeyVal := StepObj.GetValue('name');
        if KeyVal <> nil then Step.Name := KeyVal.Value;
        if Step.Name = '' then Step.Name := Step.Verb;

        KeyVal := StepObj.GetValue('required');
        if (KeyVal <> nil) and (KeyVal is TJSONBool) then
          Step.Required := (KeyVal as TJSONBool).AsBoolean;

        SetLength(Sequence, Length(Sequence) + 1);
        Sequence[High(Sequence)] := Step;
      end;

      RegisterOverride(Family, Sequence);
      Inc(Result);
    end;
  finally
    Doc.Free;
  end;
end;

class function TOBDAdapterInitializer.BuiltinSequence(
  AFamily: TOBDAdapterFamily): TOBDInitSequence;
begin
  case AFamily of
    afELM327:
      Result := TOBDInitSequence.Create(
        MakeStep('ATZ',   'Reset',                 True),
        MakeStep('ATE0',  'Echo off',              True),
        MakeStep('ATL0',  'Linefeeds off',         True),
        MakeStep('ATS0',  'Spaces off',            False),
        MakeStep('ATH0',  'Headers off',           False),
        MakeStep('ATAT1', 'Adaptive timing on',    False),
        MakeStep('ATSP0', 'Auto-protocol',         True));
    afOBDLink:
      Result := TOBDInitSequence.Create(
        MakeStep('ATZ',   'Reset',                 True),
        MakeStep('ATE0',  'Echo off',              True),
        MakeStep('ATL0',  'Linefeeds off',         True),
        MakeStep('ATS0',  'Spaces off',            False),
        MakeStep('ATH0',  'Headers off',           False),
        MakeStep('STSR',  'Software reset (ST)',   False),
        MakeStep('ATAT1', 'Adaptive timing on',    False),
        MakeStep('ATSP0', 'Auto-protocol',         True));
    afJ2534:
      // J2534 has its own programmatic open via the PassThru API — the
      // AT/ST sequence is irrelevant. Sequence is empty; the adapter
      // wires the J2534 open directly.
      Result := TOBDInitSequence.Create();
    afDoIP:
      Result := TOBDInitSequence.Create();
  else
    Result := TOBDInitSequence.Create();
  end;
end;

class function TOBDAdapterInitializer.ExtendSequence(
  const ABase: TOBDInitSequence; const AExtra: TStrings): TOBDInitSequence;
var
  I: Integer;
  Step: TOBDInitStep;
begin
  Result := Copy(ABase);
  if AExtra = nil then Exit;
  for I := 0 to AExtra.Count - 1 do
  begin
    if Trim(AExtra[I]) = '' then Continue;
    Step := MakeStep(Trim(AExtra[I]), 'Custom: ' + Trim(AExtra[I]), False);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Step;
  end;
end;

class procedure TOBDAdapterInitializer.Run(
  const ASender: IOBDAdapterCommandSender;
  const ASequence: TOBDInitSequence;
  const AOnProgress: TOBDDetectionProgress; ATimeoutMs: Cardinal);
var
  I: Integer;
  Step: TOBDInitStep;
  Resp: TOBDAdapterResponse;
begin
  if ASender = nil then
    raise EOBDAdapter.Create('Init.Run: command sender is nil');

  for I := 0 to High(ASequence) do
  begin
    Step := ASequence[I];
    ReportProgress(AOnProgress, I + 1, Length(ASequence), Step.Name, Step.Verb);
    try
      Resp := ASender.SendCommand(Step.Verb, ATimeoutMs);
      if Step.Required and Resp.IsError then
        raise EOBDAdapter.CreateFmt(
          'Init step %d (%s) failed: %s',
          [I + 1, Step.Verb, Resp.ErrorKeyword]);
    except
      on E: Exception do
      begin
        if Step.Required then
          raise EOBDAdapter.CreateFmt(
            'Init step %d (%s) failed: %s',
            [I + 1, Step.Verb, E.Message]);
        // Best-effort step: ignore and continue.
      end;
    end;
  end;
end;

initialization

finalization
  TOBDAdapterInitializer.ReleaseOverrides;

end.
