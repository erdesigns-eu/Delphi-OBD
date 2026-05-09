//------------------------------------------------------------------------------
//  OBD.Coding.Diff
//
//  TOBDCodingDiff — byte-array diff and patch utilities tailored
//  for coding workflows. The diff is byte-level, two-sided
//  (before / after), and emits per-byte change records that
//  drive snapshot ↔ apply ↔ rollback flows.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 initial.
//------------------------------------------------------------------------------

unit OBD.Coding.Diff;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>One byte-level change.</summary>
  TOBDCodingChange = record
    /// <summary>Byte offset within the buffer.</summary>
    Offset: Integer;
    /// <summary>Byte value before the change.</summary>
    Before: Byte;
    /// <summary>Byte value after the change.</summary>
    After: Byte;
  end;

  /// <summary>Result of diffing two buffers.</summary>
  TOBDCodingDiffResult = record
    Changes: TArray<TOBDCodingChange>;
    /// <summary>Old buffer length (for length mismatch detection
    /// during patch).</summary>
    OldLength: Integer;
    /// <summary>New buffer length.</summary>
    NewLength: Integer;
  end;

  /// <summary>Stateless diff / patch helpers.</summary>
  TOBDCodingDiff = class
  public
    /// <summary>Computes a per-byte diff between <c>ABefore</c> and
    /// <c>AAfter</c>. When the lengths differ, every byte from
    /// <c>min(LenBefore, LenAfter)</c> onward is emitted as a
    /// change.</summary>
    class function Compute(const ABefore, AAfter: TBytes): TOBDCodingDiffResult; static;
    /// <summary>Applies <c>ADiff</c> forward to <c>ABefore</c>,
    /// returning a buffer that equals the original
    /// <c>AAfter</c>.</summary>
    /// <exception cref="EOBDProtocol">The buffer length doesn't
    /// match <c>OldLength</c>, or a change's <c>Before</c> byte
    /// doesn't match the buffer.</exception>
    class function Apply(const ABefore: TBytes;
      const ADiff: TOBDCodingDiffResult): TBytes; static;
    /// <summary>Inverts a diff and applies it to <c>AAfter</c>,
    /// reverting to the original <c>ABefore</c>. Used for rollback.</summary>
    class function Revert(const AAfter: TBytes;
      const ADiff: TOBDCodingDiffResult): TBytes; static;
    /// <summary>Returns the same change set with offsets-in-order
    /// guaranteed to be ascending. Callers normally don't need this
    /// — Compute already returns sorted output.</summary>
    class function Sort(const ADiff: TOBDCodingDiffResult): TOBDCodingDiffResult; static;
  end;

implementation

uses
  System.Generics.Defaults;

class function TOBDCodingDiff.Compute(
  const ABefore, AAfter: TBytes): TOBDCodingDiffResult;
var
  I, MinLen, MaxLen: Integer;
  Acc: TList<TOBDCodingChange>;
  Change: TOBDCodingChange;
  B, A: Byte;
begin
  Result := Default(TOBDCodingDiffResult);
  Result.OldLength := Length(ABefore);
  Result.NewLength := Length(AAfter);
  if Result.OldLength <= Result.NewLength then
    MinLen := Result.OldLength
  else
    MinLen := Result.NewLength;
  if Result.OldLength >= Result.NewLength then
    MaxLen := Result.OldLength
  else
    MaxLen := Result.NewLength;

  Acc := TList<TOBDCodingChange>.Create;
  try
    for I := 0 to MinLen - 1 do
    begin
      if ABefore[I] <> AAfter[I] then
      begin
        Change.Offset := I;
        Change.Before := ABefore[I];
        Change.After  := AAfter[I];
        Acc.Add(Change);
      end;
    end;
    // Length difference — synthesise changes for the trailing
    // tail. The "missing" side is reported as 0x00.
    for I := MinLen to MaxLen - 1 do
    begin
      if I < Result.OldLength then B := ABefore[I] else B := 0;
      if I < Result.NewLength then A := AAfter[I]  else A := 0;
      if B <> A then
      begin
        Change.Offset := I;
        Change.Before := B;
        Change.After  := A;
        Acc.Add(Change);
      end;
    end;
    Result.Changes := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

class function TOBDCodingDiff.Apply(const ABefore: TBytes;
  const ADiff: TOBDCodingDiffResult): TBytes;
var
  I: Integer;
  Change: TOBDCodingChange;
begin
  if Length(ABefore) <> ADiff.OldLength then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingDiff.Apply: source length %d does not match diff.OldLength %d',
      [Length(ABefore), ADiff.OldLength]);
  SetLength(Result, ADiff.NewLength);
  // Copy unchanged prefix / overlap.
  if ADiff.OldLength <= ADiff.NewLength then
  begin
    if ADiff.OldLength > 0 then
      Move(ABefore[0], Result[0], ADiff.OldLength);
    // Tail bytes default-zero from SetLength; the diff entries
    // that cover them flip Before=0 → After=N.
  end
  else
  begin
    if ADiff.NewLength > 0 then
      Move(ABefore[0], Result[0], ADiff.NewLength);
  end;
  for I := 0 to High(ADiff.Changes) do
  begin
    Change := ADiff.Changes[I];
    if (Change.Offset < 0) or (Change.Offset >= ADiff.NewLength) then
    begin
      // Trailing change of a shrinking buffer — already truncated by SetLength.
      if Change.Offset < ADiff.OldLength then Continue;
    end;
    if (Change.Offset < ADiff.OldLength) and
       (ABefore[Change.Offset] <> Change.Before) then
      raise EOBDProtocol.CreateFmt(
        'TOBDCodingDiff.Apply: source byte at 0x%x is 0x%2.2X, expected 0x%2.2X',
        [Change.Offset, ABefore[Change.Offset], Change.Before]);
    if Change.Offset < ADiff.NewLength then
      Result[Change.Offset] := Change.After;
  end;
end;

class function TOBDCodingDiff.Revert(const AAfter: TBytes;
  const ADiff: TOBDCodingDiffResult): TBytes;
var
  I: Integer;
  Change: TOBDCodingChange;
  Inverse: TOBDCodingDiffResult;
begin
  Inverse.OldLength := ADiff.NewLength;
  Inverse.NewLength := ADiff.OldLength;
  SetLength(Inverse.Changes, Length(ADiff.Changes));
  for I := 0 to High(ADiff.Changes) do
  begin
    Change := ADiff.Changes[I];
    Inverse.Changes[I].Offset := Change.Offset;
    Inverse.Changes[I].Before := Change.After;
    Inverse.Changes[I].After  := Change.Before;
  end;
  Result := Apply(AAfter, Inverse);
end;

class function TOBDCodingDiff.Sort(
  const ADiff: TOBDCodingDiffResult): TOBDCodingDiffResult;
begin
  Result := ADiff;
  TArray.Sort<TOBDCodingChange>(Result.Changes,
    TComparer<TOBDCodingChange>.Construct(
      function(const L, R: TOBDCodingChange): Integer
      begin
        if L.Offset < R.Offset then Result := -1
        else if L.Offset > R.Offset then Result := 1
        else Result := 0;
      end));
end;

end.
