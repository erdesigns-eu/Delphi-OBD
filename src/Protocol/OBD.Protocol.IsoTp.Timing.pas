//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.IsoTp.Timing.pas
// CONTENTS       : ISO 15765-2 STmin/BS timing audit harness
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Protocol.IsoTp.Timing;

interface

uses
  System.SysUtils, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDIsoTpTiming = class(Exception);

  TIsoTpFrameKind = (
    iftSingle,        // SF — single frame
    iftFirst,         // FF — first frame of a multi-frame transmission
    iftConsecutive,   // CF — consecutive frame
    iftFlowControl    // FC — flow control (sender -> receiver, BS + STmin)
  );

  /// <summary>One observed frame on the bus or in a capture.</summary>
  TIsoTpFrameObservation = record
    Kind: TIsoTpFrameKind;
    /// <summary>Wall-clock time of the frame in microseconds since
    /// some arbitrary t0. Resolution must be at least 1 ms.</summary>
    TimestampMicros: Int64;
    /// <summary>Direction. True = tester->ECU, False = ECU->tester.
    /// STmin checks apply to the consecutive-frame stream from the
    /// sender on whichever side the FC frame came from.</summary>
    SenderIsTester: Boolean;
  end;

  TIsoTpTimingViolationKind = (
    itvIntraGapTooSmall,
    itvBlockSizeExceeded,
    itvUnexpectedFrameKind
  );

  TIsoTpTimingViolation = record
    Kind: TIsoTpTimingViolationKind;
    FrameIndex: Integer;
    Detail: string;
  end;

  TIsoTpTimingResult = record
    Compliant: Boolean;
    DeclaredStminMicros: Integer;
    DeclaredBlockSize: Integer;
    Violations: TArray<TIsoTpTimingViolation>;
  end;

  TOBDIsoTpTimingChecker = class
  private
    FStminMicros: Integer;
    FBlockSize: Integer;
    FToleranceMicros: Integer;
    procedure Note(var Result: TIsoTpTimingResult;
      Kind: TIsoTpTimingViolationKind; FrameIndex: Integer;
      const Detail: string);
  public
    constructor Create;
    /// <summary>Configure the checker from the FC byte values
    /// observed on the wire (STmin: 0x00..0x7F = ms; 0xF1..0xF9 =
    /// 100..900 us; BS: 0x00 = unlimited else count).</summary>
    procedure ApplyFlowControl(const StminByte, BlockSizeByte: Byte);
    /// <summary>Allow up to this much under-shoot per inter-frame gap
    /// before counting as a violation. Default 200 us — within scope
    /// timer jitter on a typical adapter.</summary>
    property ToleranceMicros: Integer read FToleranceMicros write FToleranceMicros;

    function Audit(const Frames: TArray<TIsoTpFrameObservation>): TIsoTpTimingResult;
  end;

/// <summary>Decode the STmin byte to microseconds. Raises on reserved
/// values (0x80..0xF0 + 0xFA..0xFF).</summary>
function DecodeStminMicros(const StminByte: Byte): Integer;

/// <summary>Encode microseconds back to the STmin byte. Quantises to
/// the nearest representable value: 1 ms granularity in [0..127] ms,
/// 100 us granularity in [100..900] us. Out-of-range raises.</summary>
function EncodeStminMicros(const Micros: Integer): Byte;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

function DecodeStminMicros(const StminByte: Byte): Integer;
begin
  if StminByte <= $7F then
    Exit(Integer(StminByte) * 1000);
  if (StminByte >= $F1) and (StminByte <= $F9) then
    Exit(Integer(StminByte - $F0) * 100);
  raise EOBDIsoTpTiming.CreateFmt(
    'Reserved STmin byte 0x%.2x', [StminByte]);
end;

function EncodeStminMicros(const Micros: Integer): Byte;
var
  Ms: Integer;
begin
  if Micros < 0 then
    raise EOBDIsoTpTiming.Create('STmin must be non-negative');
  if (Micros >= 100) and (Micros <= 900) and (Micros mod 100 = 0) then
    Exit(Byte($F0 + (Micros div 100)));
  if Micros mod 1000 = 0 then
  begin
    Ms := Micros div 1000;
    if (Ms >= 0) and (Ms <= $7F) then
      Exit(Byte(Ms));
  end;
  raise EOBDIsoTpTiming.CreateFmt(
    'STmin %d microseconds is not representable: must be 0..127 ms ' +
    'or 100..900 us in 100us steps', [Micros]);
end;

{ TOBDIsoTpTimingChecker }

constructor TOBDIsoTpTimingChecker.Create;
begin
  inherited;
  FStminMicros := 0;
  FBlockSize := 0;
  FToleranceMicros := 200;
end;

procedure TOBDIsoTpTimingChecker.ApplyFlowControl(
  const StminByte, BlockSizeByte: Byte);
begin
  FStminMicros := DecodeStminMicros(StminByte);
  FBlockSize := BlockSizeByte;
end;

procedure TOBDIsoTpTimingChecker.Note(var Result: TIsoTpTimingResult;
  Kind: TIsoTpTimingViolationKind; FrameIndex: Integer;
  const Detail: string);
var
  V: TIsoTpTimingViolation;
begin
  Result.Compliant := False;
  V.Kind := Kind;
  V.FrameIndex := FrameIndex;
  V.Detail := Detail;
  Result.Violations := Result.Violations + [V];
end;

function TOBDIsoTpTimingChecker.Audit(
  const Frames: TArray<TIsoTpFrameObservation>): TIsoTpTimingResult;
var
  I: Integer;
  PrevCfTimestamp: Int64;
  HasPrevCf: Boolean;
  CfCountSinceFC: Integer;
  Gap: Int64;
begin
  Result := Default(TIsoTpTimingResult);
  Result.Compliant := True;
  Result.DeclaredStminMicros := FStminMicros;
  Result.DeclaredBlockSize := FBlockSize;

  HasPrevCf := False;
  PrevCfTimestamp := 0;
  CfCountSinceFC := 0;

  for I := 0 to High(Frames) do
  begin
    case Frames[I].Kind of
      iftFlowControl:
        begin
          HasPrevCf := False;
          CfCountSinceFC := 0;
        end;
      iftConsecutive:
        begin
          if HasPrevCf then
          begin
            Gap := Frames[I].TimestampMicros - PrevCfTimestamp;
            if Gap + FToleranceMicros < FStminMicros then
              Note(Result, itvIntraGapTooSmall, I,
                Format('gap=%d us < STmin=%d us (tolerance=%d)',
                  [Gap, FStminMicros, FToleranceMicros]));
          end;
          PrevCfTimestamp := Frames[I].TimestampMicros;
          HasPrevCf := True;
          Inc(CfCountSinceFC);
          if (FBlockSize > 0) and (CfCountSinceFC > FBlockSize) then
            Note(Result, itvBlockSizeExceeded, I,
              Format('CF count %d exceeded BlockSize %d without intervening FC',
                [CfCountSinceFC, FBlockSize]));
        end;
      iftFirst, iftSingle:
        begin
          HasPrevCf := False;
          CfCountSinceFC := 0;
        end;
    end;
  end;
end;

end.
