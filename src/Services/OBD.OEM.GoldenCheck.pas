//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.GoldenCheck.pas
// CONTENTS       : Spot-check helper that validates an OEM catalog
//                  against a list of (DID, payload, expected) tuples.
//                  Used by golden tests to surface decoder regressions.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The helper is deliberately framework-neutral —
//                  it returns a failure list rather than calling
//                  Assert directly, so callers can decide whether
//                  to fail the whole batch or report the count and
//                  drill in.
//------------------------------------------------------------------------------
unit OBD.OEM.GoldenCheck;

interface

uses
  System.SysUtils, OBD.OEM;

type
  TOBDGoldenVector = record
    DID: Word;
    Payload: TBytes;
    /// <summary>
    ///   Substring the decoder's output must contain. Empty
    ///   means "any non-empty output passes".
    /// </summary>
    ExpectedSubstring: string;
    /// <summary>
    ///   Free-text label for failure messages.
    /// </summary>
    Description: string;
  end;

  TOBDGoldenFailure = record
    Vector: TOBDGoldenVector;
    ActualOutput: string;
    Reason: string;
  end;

/// <summary>
///   Build a vector inline.
/// </summary>
function GoldenVector(const DID: Word; const Payload: TBytes;
  const ExpectedSubstring, Description: string): TOBDGoldenVector;

/// <summary>
///   Run every vector through <c>Ext.DecodeDID</c>. Returns
///   the failures list — empty when every vector passed.
/// </summary>
function CheckGoldenVectors(const Ext: IOBDOEMExtension;
  const Vectors: TArray<TOBDGoldenVector>): TArray<TOBDGoldenFailure>;

implementation

uses
  System.Generics.Collections;

//------------------------------------------------------------------------------
// GOLDEN VECTOR
//------------------------------------------------------------------------------
function GoldenVector(const DID: Word; const Payload: TBytes;
  const ExpectedSubstring, Description: string): TOBDGoldenVector;
begin
  Result.DID := DID;
  Result.Payload := Payload;
  Result.ExpectedSubstring := ExpectedSubstring;
  Result.Description := Description;
end;

//------------------------------------------------------------------------------
// CHECK GOLDEN VECTORS
//------------------------------------------------------------------------------
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
