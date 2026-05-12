//------------------------------------------------------------------------------
//  Tests.OBD.OEM.Routines
//
//  Coverage for the UDS service-0x31 helpers and the canonical
//  service-function dispatcher:
//
//    - HexStringToBytes / BytesToHexString / GetBit / SetBit
//      (OBD.OEM.Coding)
//    - BuildStartRoutine / BuildStopRoutine /
//      BuildRequestRoutineResults / ParseRoutineResponse /
//      TOBDRoutineRequestBuilder / TOBDRoutineResponseReader /
//      DecodeRoutineOutput (OBD.OEM.RoutineControl)
//    - TOBDServiceFunctionRegistry / FindServiceFunction /
//      ListServiceFunctions / BuildServiceFunctionFrame
//      (OBD.OEM.ServiceFunction)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.Routines;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  DUnitX.TestFramework,
  OBD.OEM.Types,
  OBD.OEM.Extensions,
  OBD.OEM.Coding,
  OBD.OEM.RoutineControl,
  OBD.OEM.ServiceFunction;

type
  /// <summary>Service-function fixture extension.</summary>
  TSvcFnExt = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(
      var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
  end;

  /// <summary>DUnitX fixture for the routine + service-function
  /// surface.</summary>
  [TestFixture]
  TOEMRoutinesTests = class
  public
    // ---- Coding ----
    [Test] procedure Coding_HexStringRoundTrip;
    [Test] procedure Coding_HexStringAcceptsSeparators;
    [Test] procedure Coding_HexStringOddLengthRaises;
    [Test] procedure Coding_HexStringBadCharRaises;
    [Test] procedure Coding_GetBitAndSetBit;
    [Test] procedure Coding_OutOfRangeRaises;

    // ---- RoutineControl wire helpers ----
    [Test] procedure Routine_BuildStartFrameHasSIDAndSF;
    [Test] procedure Routine_BuildStopAndResultsFrames;
    [Test] procedure Routine_ParsePositiveResponse;
    [Test] procedure Routine_ParseNegativeResponseRaises;
    [Test] procedure Routine_ParseWrongSidRaises;
    [Test] procedure Routine_ParseWrongRidRaises;
    [Test] procedure Routine_ParseShortRaises;

    // ---- Builder + Reader ----
    [Test] procedure Builder_EmitsTypedFieldsBigEndian;
    [Test] procedure Builder_AsciiFixedLengthPadsAndRejectsOversize;
    [Test] procedure Builder_BcdDateAndYear;
    [Test] procedure Builder_ToFrameWrapsHeader;
    [Test] procedure Reader_RoundTripsBuilderOutput;
    [Test] procedure Reader_UnderReadRaises;

    // ---- Schema decode ----
    [Test] procedure Schema_DecodeMixedFields;
    [Test] procedure Schema_ShortPayloadStopsCleanly;

    // ---- ServiceFunction ----
    [Test] procedure SvcFn_ClassifyKnownNames;
    [Test] procedure SvcFn_ClassifyUnknownReturnsUnknown;
    [Test] procedure SvcFn_FindOilLifeInExt;
    [Test] procedure SvcFn_ListEnumeratesAllSupported;
    [Test] procedure SvcFn_BuildFrameUsesStartRoutine;
    [Test] procedure SvcFn_KindNameHasReadableLabel;
  end;

implementation

{ TSvcFnExt }

function TSvcFnExt.ManufacturerKey: string;
begin
  Result := 'SVC';
end;

function TSvcFnExt.DisplayName: string;
begin
  Result := 'Service-function fixture';
end;

function TSvcFnExt.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := False;
end;

procedure TSvcFnExt.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
var
  R: TOBDOEMRoutine;
begin
  R := Default(TOBDOEMRoutine);
  R.Identifier := $4001;
  R.Name := 'oil_life_reset';
  R.Description := 'Reset oil life monitor';
  Routines := Routines + [R];

  R := Default(TOBDOEMRoutine);
  R.Identifier := $4002;
  R.Name := 'tpms_relearn';
  R.Description := 'TPMS sensor relearn';
  Routines := Routines + [R];

  R := Default(TOBDOEMRoutine);
  R.Identifier := $4003;
  R.Name := 'unrelated_routine';
  R.Description := 'Not a service function';
  Routines := Routines + [R];
end;

{ ---- Coding ------------------------------------------------------------- }

procedure TOEMRoutinesTests.Coding_HexStringRoundTrip;
var
  Bytes: TBytes;
begin
  Bytes := HexStringToBytes('0204110030');
  Assert.AreEqual(5, Length(Bytes));
  Assert.AreEqual($02, Integer(Bytes[0]));
  Assert.AreEqual($30, Integer(Bytes[4]));
  Assert.AreEqual('0204110030', BytesToHexString(Bytes));
  Assert.AreEqual('02 04 11 00 30', BytesToHexString(Bytes, ' '));
end;

procedure TOEMRoutinesTests.Coding_HexStringAcceptsSeparators;
var
  A, B, C: TBytes;
begin
  A := HexStringToBytes('02 04 11 00 30');
  B := HexStringToBytes('02-04-11-00-30');
  C := HexStringToBytes('02:04:11:00:30');
  Assert.AreEqual(5, Length(A));
  Assert.AreEqual(5, Length(B));
  Assert.AreEqual(5, Length(C));
  Assert.AreEqual($30, Integer(A[4]));
  Assert.AreEqual($30, Integer(B[4]));
  Assert.AreEqual($30, Integer(C[4]));
end;

procedure TOEMRoutinesTests.Coding_HexStringOddLengthRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      HexStringToBytes('ABC');
    end,
    EOBDCodingError);
end;

procedure TOEMRoutinesTests.Coding_HexStringBadCharRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      HexStringToBytes('AB!CD');
    end,
    EOBDCodingError);
end;

procedure TOEMRoutinesTests.Coding_GetBitAndSetBit;
var
  Bytes: TBytes;
begin
  Bytes := TBytes.Create($00, $00);
  SetBit(Bytes, 0, 3, True);
  Assert.AreEqual($08, Integer(Bytes[0]));
  Assert.IsTrue(GetBit(Bytes, 0, 3));
  Assert.IsFalse(GetBit(Bytes, 0, 4));
  SetBit(Bytes, 1, 7, True);
  Assert.AreEqual($80, Integer(Bytes[1]));
  SetBit(Bytes, 0, 3, False);
  Assert.AreEqual($00, Integer(Bytes[0]));
end;

procedure TOEMRoutinesTests.Coding_OutOfRangeRaises;
var
  Bytes: TBytes;
begin
  Bytes := TBytes.Create($00);
  Assert.WillRaise(
    procedure
    begin
      GetBit(Bytes, 5, 0);
    end,
    EOBDCodingError);
  Assert.WillRaise(
    procedure
    begin
      GetBit(Bytes, 0, 9);
    end,
    EOBDCodingError);
end;

{ ---- RoutineControl wire helpers ---------------------------------------- }

procedure TOEMRoutinesTests.Routine_BuildStartFrameHasSIDAndSF;
var
  Frame: TBytes;
begin
  Frame := BuildStartRoutine($0203, TBytes.Create($AB, $CD));
  Assert.AreEqual(6, Length(Frame));
  Assert.AreEqual($31, Integer(Frame[0]));
  Assert.AreEqual($01, Integer(Frame[1]));
  Assert.AreEqual($02, Integer(Frame[2]));
  Assert.AreEqual($03, Integer(Frame[3]));
  Assert.AreEqual($AB, Integer(Frame[4]));
  Assert.AreEqual($CD, Integer(Frame[5]));
end;

procedure TOEMRoutinesTests.Routine_BuildStopAndResultsFrames;
var
  Stop, Results: TBytes;
begin
  Stop := BuildStopRoutine($0203);
  Results := BuildRequestRoutineResults($0203);
  Assert.AreEqual($02, Integer(Stop[1]));
  Assert.AreEqual($03, Integer(Results[1]));
end;

procedure TOEMRoutinesTests.Routine_ParsePositiveResponse;
var
  Payload: TBytes;
begin
  Payload := ParseRoutineResponse(
    TBytes.Create($71, $01, $02, $03, $AA, $BB),
    rcStart, $0203);
  Assert.AreEqual(2, Length(Payload));
  Assert.AreEqual($AA, Integer(Payload[0]));
  Assert.AreEqual($BB, Integer(Payload[1]));
end;

procedure TOEMRoutinesTests.Routine_ParseNegativeResponseRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      ParseRoutineResponse(TBytes.Create($7F, $31, $33),
        rcStart, $0203);
    end,
    EOBDRoutineError);
end;

procedure TOEMRoutinesTests.Routine_ParseWrongSidRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      ParseRoutineResponse(TBytes.Create($72, $01, $02, $03),
        rcStart, $0203);
    end,
    EOBDRoutineError);
end;

procedure TOEMRoutinesTests.Routine_ParseWrongRidRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      ParseRoutineResponse(TBytes.Create($71, $01, $99, $99),
        rcStart, $0203);
    end,
    EOBDRoutineError);
end;

procedure TOEMRoutinesTests.Routine_ParseShortRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      ParseRoutineResponse(TBytes.Create($71, $01), rcStart, $0203);
    end,
    EOBDRoutineError);
end;

{ ---- Builder + Reader --------------------------------------------------- }

procedure TOEMRoutinesTests.Builder_EmitsTypedFieldsBigEndian;
var
  B: TOBDRoutineRequestBuilder;
  Payload: TBytes;
begin
  B := Default(TOBDRoutineRequestBuilder);
  B.AddUInt8($AB);
  B.AddUInt16BE($1234);
  B.AddUInt32BE($DEADBEEF);
  Payload := B.PayloadBytes;
  Assert.AreEqual(7, Length(Payload));
  Assert.AreEqual($AB, Integer(Payload[0]));
  Assert.AreEqual($12, Integer(Payload[1]));
  Assert.AreEqual($34, Integer(Payload[2]));
  Assert.AreEqual($DE, Integer(Payload[3]));
  Assert.AreEqual($AD, Integer(Payload[4]));
  Assert.AreEqual($BE, Integer(Payload[5]));
  Assert.AreEqual($EF, Integer(Payload[6]));
end;

procedure TOEMRoutinesTests.Builder_AsciiFixedLengthPadsAndRejectsOversize;
var
  B: TOBDRoutineRequestBuilder;
  Payload: TBytes;
begin
  B := Default(TOBDRoutineRequestBuilder);
  B.AddAscii('AB', 4);
  Payload := B.PayloadBytes;
  Assert.AreEqual(4, Length(Payload));
  Assert.AreEqual(Ord('A'), Integer(Payload[0]));
  Assert.AreEqual(Ord('B'), Integer(Payload[1]));
  Assert.AreEqual($00, Integer(Payload[2]));
  Assert.AreEqual($00, Integer(Payload[3]));
  Assert.WillRaise(
    procedure
    var
      B2: TOBDRoutineRequestBuilder;
    begin
      B2 := Default(TOBDRoutineRequestBuilder);
      B2.AddAscii('TOOLONG', 4);
    end,
    EOBDRoutineError);
end;

procedure TOEMRoutinesTests.Builder_BcdDateAndYear;
var
  B: TOBDRoutineRequestBuilder;
  Payload: TBytes;
begin
  B := Default(TOBDRoutineRequestBuilder);
  B.AddBcdDate(26, 5, 12);
  B.AddBcdYear(99);
  Payload := B.PayloadBytes;
  Assert.AreEqual(4, Length(Payload));
  Assert.AreEqual($26, Integer(Payload[0]));
  Assert.AreEqual($05, Integer(Payload[1]));
  Assert.AreEqual($12, Integer(Payload[2]));
  Assert.AreEqual($99, Integer(Payload[3]));
end;

procedure TOEMRoutinesTests.Builder_ToFrameWrapsHeader;
var
  B: TOBDRoutineRequestBuilder;
  Frame: TBytes;
begin
  B := Default(TOBDRoutineRequestBuilder);
  B.AddUInt8($AA);
  Frame := B.ToFrame(rcStart, $0203);
  Assert.AreEqual(5, Length(Frame));
  Assert.AreEqual($31, Integer(Frame[0]));
  Assert.AreEqual($01, Integer(Frame[1]));
  Assert.AreEqual($02, Integer(Frame[2]));
  Assert.AreEqual($03, Integer(Frame[3]));
  Assert.AreEqual($AA, Integer(Frame[4]));
end;

procedure TOEMRoutinesTests.Reader_RoundTripsBuilderOutput;
var
  B: TOBDRoutineRequestBuilder;
  R: TOBDRoutineResponseReader;
begin
  B := Default(TOBDRoutineRequestBuilder);
  B.AddUInt16BE($1234);
  B.AddInt16BE(-1);
  B.AddAscii('OK', 4);
  R := TOBDRoutineResponseReader.Wrap(B.PayloadBytes);
  Assert.AreEqual($1234, Integer(R.ReadUInt16BE));
  Assert.AreEqual(-1, Integer(R.ReadInt16BE));
  Assert.AreEqual('OK', R.ReadAscii(4));
  Assert.IsFalse(R.HasMore);
end;

procedure TOEMRoutinesTests.Reader_UnderReadRaises;
var
  R: TOBDRoutineResponseReader;
begin
  R := TOBDRoutineResponseReader.Wrap(TBytes.Create($01));
  Assert.WillRaise(
    procedure
    begin
      R.ReadUInt32BE;
    end,
    EOBDRoutineError);
end;

{ ---- Schema decode ------------------------------------------------------ }

procedure TOEMRoutinesTests.Schema_DecodeMixedFields;
var
  Schema: TOBDRoutineSchema;
  Decoded: TArray<TOBDDecodedField>;
  F: TOBDRoutineField;
begin
  Schema := Default(TOBDRoutineSchema);
  F := Default(TOBDRoutineField);
  F.Name := 'rpm';
  F.Kind := rfkUInt16BE;
  F.Scale := 1;
  F.Unit_ := 'rpm';
  Schema.OutputFields := Schema.OutputFields + [F];
  F := Default(TOBDRoutineField);
  F.Name := 'label';
  F.Kind := rfkAscii;
  F.Size := 4;
  Schema.OutputFields := Schema.OutputFields + [F];
  Decoded := DecodeRoutineOutput(Schema,
    TBytes.Create($07, $D0, Ord('O'), Ord('K'), $00, $00));
  Assert.AreEqual(2, Length(Decoded));
  Assert.Contains(Decoded[0].Display, '2000');
  Assert.Contains(Decoded[0].Display, 'rpm');
  Assert.Contains(Decoded[1].Display, '"OK"');
end;

procedure TOEMRoutinesTests.Schema_ShortPayloadStopsCleanly;
var
  Schema: TOBDRoutineSchema;
  Decoded: TArray<TOBDDecodedField>;
  F: TOBDRoutineField;
begin
  Schema := Default(TOBDRoutineSchema);
  F := Default(TOBDRoutineField);
  F.Name := 'rpm';
  F.Kind := rfkUInt16BE;
  F.Scale := 1;
  Schema.OutputFields := Schema.OutputFields + [F];
  F := Default(TOBDRoutineField);
  F.Name := 'optional';
  F.Kind := rfkUInt32BE;
  F.Scale := 1;
  Schema.OutputFields := Schema.OutputFields + [F];
  Decoded := DecodeRoutineOutput(Schema, TBytes.Create($07, $D0));
  Assert.AreEqual(1, Length(Decoded));
end;

{ ---- ServiceFunction --------------------------------------------------- }

procedure TOEMRoutinesTests.SvcFn_ClassifyKnownNames;
begin
  Assert.AreEqual(Ord(sfOilLifeReset),
    Ord(TOBDServiceFunctionRegistry.ClassifyName('oil_life_reset')));
  Assert.AreEqual(Ord(sfOilLifeReset),
    Ord(TOBDServiceFunctionRegistry.ClassifyName(
      'engine_oil_reset_routine')));
  Assert.AreEqual(Ord(sfTPMSRelearn),
    Ord(TOBDServiceFunctionRegistry.ClassifyName('tpms_relearn')));
  Assert.AreEqual(Ord(sfBatteryRegistration),
    Ord(TOBDServiceFunctionRegistry.ClassifyName(
      'register_battery')));
end;

procedure TOEMRoutinesTests.SvcFn_ClassifyUnknownReturnsUnknown;
begin
  Assert.AreEqual(Ord(sfUnknown),
    Ord(TOBDServiceFunctionRegistry.ClassifyName(
      'unrelated_routine_name')));
end;

procedure TOEMRoutinesTests.SvcFn_FindOilLifeInExt;
var
  Ext: IOBDOEMExtension;
  Func: TOBDServiceFunction;
begin
  Ext := TSvcFnExt.Create;
  Assert.IsTrue(FindServiceFunction(Ext, sfOilLifeReset, Func));
  Assert.AreEqual($4001, Integer(Func.RoutineId));
  Assert.AreEqual('oil_life_reset', Func.RoutineName);
  Assert.IsFalse(FindServiceFunction(Ext, sfDPFRegen, Func));
end;

procedure TOEMRoutinesTests.SvcFn_ListEnumeratesAllSupported;
var
  Ext: IOBDOEMExtension;
  List: TArray<TOBDServiceFunction>;
begin
  Ext := TSvcFnExt.Create;
  List := ListServiceFunctions(Ext);
  Assert.AreEqual(2, Length(List));               // oil + tpms; unrelated skipped
end;

procedure TOEMRoutinesTests.SvcFn_BuildFrameUsesStartRoutine;
var
  Func: TOBDServiceFunction;
  Frame: TBytes;
begin
  Func := Default(TOBDServiceFunction);
  Func.RoutineId := $4001;
  Frame := BuildServiceFunctionFrame(Func, TBytes.Create($AB));
  Assert.AreEqual(5, Length(Frame));
  Assert.AreEqual($31, Integer(Frame[0]));
  Assert.AreEqual($01, Integer(Frame[1]));
  Assert.AreEqual($40, Integer(Frame[2]));
  Assert.AreEqual($01, Integer(Frame[3]));
  Assert.AreEqual($AB, Integer(Frame[4]));
end;

procedure TOEMRoutinesTests.SvcFn_KindNameHasReadableLabel;
begin
  Assert.AreEqual('Oil Life Reset',
    ServiceFunctionKindName(sfOilLifeReset));
  Assert.AreEqual('Unknown Service Function',
    ServiceFunctionKindName(sfUnknown));
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMRoutinesTests);

end.
