//------------------------------------------------------------------------------
//  OBD.OEM.GoldenCheck
//
//  Spot-check helper that validates an OEM extension's
//  <c>DecodeDID</c> against a list of (DID, payload, expected)
//  tuples. Used by golden tests to surface decoder regressions.
//
//  Framework-neutral by design — the helper returns the failure
//  list rather than calling <c>Assert</c> directly, so callers
//  can decide whether to fail the whole batch or report the count
//  and drill in.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial port from v1 OBD.OEM.GoldenCheck.
//------------------------------------------------------------------------------

unit OBD.OEM.GoldenCheck;

interface

uses
  System.SysUtils,
  OBD.OEM.Extensions;

type
  /// <summary>One spot-check vector.</summary>
  TOBDGoldenVector = record
    /// <summary>DID being checked.</summary>
    DID: Word;
    /// <summary>Raw response bytes.</summary>
    Payload: TBytes;
    /// <summary>
    ///   Substring the decoder's output must contain. Empty =
    ///   "any non-empty output passes".
    /// </summary>
    ExpectedSubstring: string;
    /// <summary>Free-text label for failure messages.</summary>
    Description: string;
  end;

  /// <summary>One spot-check failure.</summary>
  TOBDGoldenFailure = record
    /// <summary>The vector that failed.</summary>
    Vector: TOBDGoldenVector;
    /// <summary>The decoder's actual output.</summary>
    ActualOutput: string;
    /// <summary>Human-readable failure reason.</summary>
    Reason: string;
  end;

/// <summary>Builds a vector inline.</summary>
/// <param name="DID">DID being checked.</param>
/// <param name="Payload">Raw response bytes.</param>
/// <param name="ExpectedSubstring">Substring the decoder's output
/// must contain (empty = any non-empty output passes).</param>
/// <param name="Description">Label for failure messages.</param>
function GoldenVector(const DID: Word; const Payload: TBytes;
  const ExpectedSubstring, Description: string): TOBDGoldenVector;

/// <summary>
///   Runs every vector through <c>Ext.DecodeDID</c>. Returns the
///   failures list — empty when every vector passed.
/// </summary>
/// <param name="Ext">Extension under test.</param>
/// <param name="Vectors">Vectors to run.</param>
function CheckGoldenVectors(const Ext: IOBDOEMExtension;
  const Vectors: TArray<TOBDGoldenVector>): TArray<TOBDGoldenFailure>;

implementation

uses
  System.Generics.Collections;

function GoldenVector(const DID: Word; const Payload: TBytes;
  const ExpectedSubstring, Description: string): TOBDGoldenVector;
begin
  Result.DID := DID;
  Result.Payload := Payload;
  Result.ExpectedSubstring := ExpectedSubstring;
  Result.Description := Description;
end;

function CheckGoldenVectors(const Ext: IOBDOEMExtension;
  const Vectors: TArray<TOBDGoldenVector>): TArray<TOBDGoldenFailure>;
var
  Failures: TList<TOBDGoldenFailure>;
  V: TOBDGoldenVector;
  Output: string;
  Failure: TOBDGoldenFailure;
begin
  Failures := TList<TOBDGoldenFailure>.Create;
  try
    for V in Vectors do
    begin
      Output := Ext.DecodeDID(V.DID, V.Payload);
      Failure := Default(TOBDGoldenFailure);
      Failure.Vector := V;
      Failure.ActualOutput := Output;
      if Trim(Output) = '' then
      begin
        Failure.Reason := Format(
          'DID 0x%.4X (%s): decoder produced empty output',
          [V.DID, V.Description]);
        Failures.Add(Failure);
        Continue;
      end;
      if (V.ExpectedSubstring <> '') and
         (Pos(V.ExpectedSubstring, Output) = 0) then
      begin
        Failure.Reason := Format(
          'DID 0x%.4X (%s): expected output to contain "%s", got "%s"',
          [V.DID, V.Description, V.ExpectedSubstring, Output]);
        Failures.Add(Failure);
      end;
    end;
    Result := Failures.ToArray;
  finally
    Failures.Free;
  end;
end;

end.
