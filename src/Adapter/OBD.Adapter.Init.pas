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
  public
    /// <summary>
    ///   Returns the built-in sequence for the given family.
    /// </summary>
    /// <param name="AFamily">Adapter family.</param>
    /// <returns>Array of steps.</returns>
    class function BuiltinSequence(
      AFamily: TOBDAdapterFamily): TOBDInitSequence; static;

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

end.
