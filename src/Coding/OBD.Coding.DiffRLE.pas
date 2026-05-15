//------------------------------------------------------------------------------
//  OBD.Coding.DiffRLE
//
//  TOBDCodingDiffRLE — run-length-encoded diff for firmware-scale
//  buffers. The byte-level <see cref="TOBDCodingDiff"/> emits one
//  record per changed byte, which is fine for sub-100-byte coding
//  payloads but explodes for 64KB+ flash images.
//
//  This unit emits one record per *contiguous run* of changed
//  bytes:
//
//    Offset, Before-bytes, After-bytes
//
//  Apply / Revert mirror the byte-level helpers; the host typically
//  uses RLE diffs as the input to a flasher's chunked transfer
//  loop (one TransferData chunk per run).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit OBD.Coding.DiffRLE;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>One contiguous run of differing bytes.</summary>
  TOBDCodingRun = record
    /// <summary>Starting offset.</summary>
    Offset: Integer;
    /// <summary>Bytes from the source buffer at this run.</summary>
    Before: TBytes;
    /// <summary>Bytes from the target buffer at this run.</summary>
    After: TBytes;
  end;

  /// <summary>Run-encoded diff result.</summary>
  TOBDCodingDiffRLEResult = record
    Runs: TArray<TOBDCodingRun>;
    OldLength: Integer;
    NewLength: Integer;
  end;

  /// <summary>Run-length-encoded diff helpers.</summary>
  TOBDCodingDiffRLE = class
  public
    /// <summary>Computes the RLE diff between
    /// <c>ABefore</c> and <c>AAfter</c>. <c>AGap</c> is the maximum
    /// number of consecutive equal bytes the algorithm tolerates
    /// inside a run before splitting (default 0 = strict — every
    /// equal byte ends the run; raise to merge nearby runs into
    /// fewer, larger transfers).</summary>
    class function Compute(const ABefore, AAfter: TBytes;
      AGap: Integer = 0): TOBDCodingDiffRLEResult; static;
    /// <summary>Applies the diff forward.</summary>
    class function Apply(const ABefore: TBytes;
      const ADiff: TOBDCodingDiffRLEResult): TBytes; static;
    /// <summary>Reverts the diff (reapplies <c>Before</c> over
    /// <c>After</c>).</summary>
    class function Revert(const AAfter: TBytes;
      const ADiff: TOBDCodingDiffRLEResult): TBytes; static;
    /// <summary>Total bytes that need to be transferred when
    /// applying this diff (sum of run lengths).</summary>
    class function TransferSize(const ADiff: TOBDCodingDiffRLEResult): Integer; static;
  end;

implementation

class function TOBDCodingDiffRLE.Compute(const ABefore, AAfter: TBytes;
  AGap: Integer): TOBDCodingDiffRLEResult;
var
  I, MinLen, MaxLen, RunStart, GapCount: Integer;
  InRun: Boolean;
  Runs: TList<TOBDCodingRun>;
  Run: TOBDCodingRun;
  B, A: Byte;

  procedure FlushRun(AEndExclusive: Integer);
  var
    Len, K: Integer;
  begin
    if not InRun then Exit;
    // Trim trailing matching tail when a gap was present.
    Len := AEndExclusive - RunStart;
    while Len > 0 do
    begin
      K := RunStart + Len - 1;
      if (K < Length(ABefore)) and (K < Length(AAfter)) and
         (ABefore[K] = AAfter[K]) then
        Dec(Len)
      else
        Break;
    end;
    if Len > 0 then
    begin
      Run := Default(TOBDCodingRun);
      Run.Offset := RunStart;
      SetLength(Run.Before, Len);
      SetLength(Run.After, Len);
      for K := 0 to Len - 1 do
      begin
        if RunStart + K < Length(ABefore) then
          Run.Before[K] := ABefore[RunStart + K]
        else
          Run.Before[K] := 0;
        if RunStart + K < Length(AAfter) then
          Run.After[K] := AAfter[RunStart + K]
        else
          Run.After[K] := 0;
      end;
      Runs.Add(Run);
    end;
    InRun := False;
  end;

begin
  Result := Default(TOBDCodingDiffRLEResult);
  Result.OldLength := Length(ABefore);
  Result.NewLength := Length(AAfter);
  if Result.OldLength <= Result.NewLength then MinLen := Result.OldLength
  else MinLen := Result.NewLength;
  if Result.OldLength >= Result.NewLength then MaxLen := Result.OldLength
  else MaxLen := Result.NewLength;
  if AGap < 0 then AGap := 0;

  Runs := TList<TOBDCodingRun>.Create;
  try
    InRun := False;
    GapCount := 0;
    RunStart := 0;
    for I := 0 to MaxLen - 1 do
    begin
      if I < Length(ABefore) then B := ABefore[I] else B := 0;
      if I < Length(AAfter)  then A := AAfter[I]  else A := 0;
      if (I >= MinLen) or (B <> A) then
      begin
        if not InRun then
        begin
          InRun := True;
          RunStart := I;
          GapCount := 0;
        end
        else
          GapCount := 0;
      end
      else
      begin
        if InRun then
        begin
          Inc(GapCount);
          if GapCount > AGap then
            FlushRun(I - GapCount + 1);
        end;
      end;
    end;
    FlushRun(MaxLen);
    Result.Runs := Runs.ToArray;
  finally
    Runs.Free;
  end;
end;

class function TOBDCodingDiffRLE.Apply(const ABefore: TBytes;
  const ADiff: TOBDCodingDiffRLEResult): TBytes;
var
  I, J, RunLen: Integer;
  Run: TOBDCodingRun;
begin
  if Length(ABefore) <> ADiff.OldLength then
    raise EOBDProtocol.CreateFmt(
      'TOBDCodingDiffRLE.Apply: source length %d does not match diff.OldLength %d',
      [Length(ABefore), ADiff.OldLength]);
  SetLength(Result, ADiff.NewLength);
  // Copy the unchanged baseline.
  if (ADiff.OldLength > 0) and (ADiff.NewLength > 0) then
  begin
    if ADiff.OldLength <= ADiff.NewLength then
      Move(ABefore[0], Result[0], ADiff.OldLength)
    else
      Move(ABefore[0], Result[0], ADiff.NewLength);
  end;
  for I := 0 to High(ADiff.Runs) do
  begin
    Run := ADiff.Runs[I];
    RunLen := Length(Run.After);
    for J := 0 to RunLen - 1 do
    begin
      if Run.Offset + J >= ADiff.NewLength then Break;
      // Optional sanity check: pre-image byte must match.
      if (Run.Offset + J < ADiff.OldLength) and
         (J < Length(Run.Before)) and
         (ABefore[Run.Offset + J] <> Run.Before[J]) then
        raise EOBDProtocol.CreateFmt(
          'TOBDCodingDiffRLE.Apply: pre-image mismatch at 0x%x ' +
          '(buffer=0x%2.2X, run=0x%2.2X)',
          [Run.Offset + J, ABefore[Run.Offset + J], Run.Before[J]]);
      Result[Run.Offset + J] := Run.After[J];
    end;
  end;
end;

class function TOBDCodingDiffRLE.Revert(const AAfter: TBytes;
  const ADiff: TOBDCodingDiffRLEResult): TBytes;
var
  Inverse: TOBDCodingDiffRLEResult;
  I: Integer;
begin
  Inverse := Default(TOBDCodingDiffRLEResult);
  Inverse.OldLength := ADiff.NewLength;
  Inverse.NewLength := ADiff.OldLength;
  SetLength(Inverse.Runs, Length(ADiff.Runs));
  for I := 0 to High(ADiff.Runs) do
  begin
    Inverse.Runs[I].Offset := ADiff.Runs[I].Offset;
    Inverse.Runs[I].Before := ADiff.Runs[I].After;
    Inverse.Runs[I].After  := ADiff.Runs[I].Before;
  end;
  Result := Apply(AAfter, Inverse);
end;

class function TOBDCodingDiffRLE.TransferSize(
  const ADiff: TOBDCodingDiffRLEResult): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(ADiff.Runs) do
    Result := Result + Length(ADiff.Runs[I].After);
end;

end.
