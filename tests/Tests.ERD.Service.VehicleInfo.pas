//------------------------------------------------------------------------------
//  Tests.ERD.Service.VehicleInfo
//
//  Coverage for TOBDVehicleInfoParser: Service 09 assembly over CAN
//  (NODI form, headers on / off, several ECUs) and legacy buses
//  (numbered 4-byte messages), J1979-2 DIDs, calibration pairing,
//  in-use performance counters and ECU names.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.Service.VehicleInfo;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  DUnitX.TestFramework,
  ERD.Protocol.Types,
  ERD.Protocol.UDS,
  ERD.Service.VehicleInfo;

type
  [TestFixture]
  TVehicleInfoParserTests = class
  public
    /// <summary>CAN VIN, headers off, ISO-TP multi-frame.</summary>
    [Test] procedure CanVinHeadersOff;
    /// <summary>Legacy VIN: five numbered messages, zero padded.</summary>
    [Test] procedure LegacyVinFiveMessages;
    /// <summary>Supported bitmaps of two ECUs are merged.</summary>
    [Test] procedure SupportedUnionAcrossEcus;
    /// <summary>CALID and CVN of two ECUs pair by header.</summary>
    [Test] procedure CalibrationsPairPerEcu;
    /// <summary>Legacy CVNs: numbered messages join into one ECU.</summary>
    [Test] procedure LegacyCvnsJoin;
    /// <summary>Headers off: two single-CVN answers stay two ECUs.</summary>
    [Test] procedure HeadersOffCvnsStayApart;
    /// <summary>Spark in-use performance: names, counters, ratio.</summary>
    [Test] procedure InUsePerformanceSpark;
    /// <summary>Compression in-use performance uses the diesel names.</summary>
    [Test] procedure InUsePerformanceCompression;
    /// <summary>ECU name: acronym and text name.</summary>
    [Test] procedure EcuNameSplits;
    /// <summary>J1979-2 F802 with a NODI byte.</summary>
    [Test] procedure UdsVinWithNodi;
    /// <summary>J1979-2 F806 without a NODI byte.</summary>
    [Test] procedure UdsCvnWithoutNodi;
  end;

implementation

function DecodeRaw(ASid: Byte; const AData: TBytes;
  const ARaw: string): TOBDResponse;
var
  Req: TOBDRequest;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := ASid;
  Req.Data := AData;
  TOBDUDSCodec.Decode(Req, ARaw, Result);
end;

function Info(AInfoType: Byte; const ARaw: string)
  : TArray<TOBDInfoTypeMessage>;
begin
  Result := TOBDVehicleInfoParser.Assemble(DecodeRaw($09,
    TBytes.Create(AInfoType), ARaw), AInfoType);
end;

procedure TVehicleInfoParserTests.CanVinHeadersOff;
var
  M: TArray<TOBDInfoTypeMessage>;
  T: TArray<string>;
begin
  M := Info($02, '014'#13'0: 49 02 01 57 56 57'#13'1: 5A 5A 5A 41 55 5A 47'#13 +
    '2: 57 31 32 33 34 35 36'#13#13'>');
  Assert.AreEqual(1, Integer(Length(M)));
  Assert.AreEqual(1, M[0].ItemCount);
  T := TOBDVehicleInfoParser.TextItems(M[0], 0);
  Assert.AreEqual(1, Integer(Length(T)));
  Assert.AreEqual('WVWZZZAUZGW123456', T[0]);
end;

procedure TVehicleInfoParserTests.LegacyVinFiveMessages;
var
  M: TArray<TOBDInfoTypeMessage>;
  T: TArray<string>;
begin
  M := Info($02, '49 02 01 00 00 00 57'#13'49 02 02 56 57 5A 5A'#13 +
    '49 02 03 5A 41 55 5A'#13'49 02 04 47 57 31 32'#13 +
    '49 02 05 33 34 35 36'#13'>');
  Assert.AreEqual(1, Integer(Length(M)));
  T := TOBDVehicleInfoParser.TextItems(M[0], 0);
  Assert.AreEqual('WVWZZZAUZGW123456', T[0]);
end;

procedure TVehicleInfoParserTests.SupportedUnionAcrossEcus;
var
  S: TArray<Byte>;
begin
  S := TOBDVehicleInfoParser.SupportedInfoTypes(Info($00,
    '7E8 06 49 00 55 40 00 00'#13'7E9 06 49 00 14 40 00 00'#13'>'));
  Assert.AreEqual(5, Integer(Length(S)));
  Assert.AreEqual(2, Integer(S[0]));
  Assert.AreEqual(4, Integer(S[1]));
  Assert.AreEqual(6, Integer(S[2]));
  Assert.AreEqual(8, Integer(S[3]));
  Assert.AreEqual($0A, Integer(S[4]));
end;

procedure TVehicleInfoParserTests.CalibrationsPairPerEcu;
var
  Cal: TArray<TOBDCalibration>;
begin
  Cal := TOBDVehicleInfoParser.Calibrations(Info($04,
    '7E8 10 13 49 04 01 30 34 4C'#13'7E9 10 13 49 04 01 30 43 57'#13 +
    '7E8 21 39 30 36 30 32 31 44'#13'7E9 21 33 30 30 30 34 31 48'#13 +
    '7E8 22 54 20 35 31 34 33 00'#13'7E9 22 20 34 30 31 32 00 00'#13'>'),
    Info($06, '7E9 07 49 06 01 A1 B2 0C 77'#13 +
    '7E8 07 49 06 01 6F 3A 92 C1'#13'>'));
  Assert.AreEqual(2, Integer(Length(Cal)));
  Assert.AreEqual('7E8', Cal[0].Ecu);
  Assert.AreEqual('04L906021DT 5143', Cal[0].CalibrationId);
  Assert.IsTrue(Cal[0].HasCVN);
  Assert.AreEqual('6F3A92C1', Cal[0].CVN);
  Assert.AreEqual('7E9', Cal[1].Ecu);
  Assert.AreEqual('0CW300041H 4012', Cal[1].CalibrationId);
  Assert.AreEqual('A1B20C77', Cal[1].CVN);
end;

procedure TVehicleInfoParserTests.LegacyCvnsJoin;
var
  M: TArray<TOBDInfoTypeMessage>;
  T: TArray<string>;
begin
  M := Info($06, '49 06 01 11 22 33 44'#13'49 06 02 55 66 77 88'#13'>');
  Assert.AreEqual(1, Integer(Length(M)));
  T := TOBDVehicleInfoParser.CVNs(M[0]);
  Assert.AreEqual(2, Integer(Length(T)));
  Assert.AreEqual('11223344', T[0]);
  Assert.AreEqual('55667788', T[1]);
end;

procedure TVehicleInfoParserTests.HeadersOffCvnsStayApart;
begin
  Assert.AreEqual(2, Integer(Length(Info($06,
    '49 06 01 11 22 33 44'#13'49 06 01 55 66 77 88'#13'>'))));
end;

procedure TVehicleInfoParserTests.InUsePerformanceSpark;
var
  M: TArray<TOBDInfoTypeMessage>;
  P: TOBDInUsePerformance;
begin
  M := Info($08, '7E8 10 23 49 08 10 00 64 00'#13'7E8 21 C8 00 0A 00 14 00 00'#13
    + '7E8 22 00 00 05 00 0A 00 00'#13'7E8 23 00 00 01 00 02 00 00'#13 +
    '7E8 24 00 00 00 00 00 03 00'#13'7E8 25 06 00 00 00 00 00 00'#13'>');
  Assert.AreEqual(1, Integer(Length(M)));
  Assert.AreEqual(16, M[0].ItemCount);
  P := TOBDVehicleInfoParser.InUsePerformance(M[0]);
  Assert.IsFalse(P.CompressionIgnition);
  Assert.AreEqual(16, Integer(Length(P.Counters)));
  Assert.AreEqual('OBDCOND', P.Counters[0].Name);
  Assert.AreEqual(100, Integer(P.GeneralDenominator));
  Assert.AreEqual(200, Integer(P.IgnitionCycles));
  Assert.AreEqual(7, Integer(Length(P.Monitors)));
  Assert.AreEqual('catalyst_b1', P.Monitors[0].Key);
  Assert.AreEqual(10, Integer(P.Monitors[0].Completions));
  Assert.AreEqual(20, Integer(P.Monitors[0].Conditions));
  Assert.AreEqual(0.5, P.Monitors[0].Ratio, 1E-9);
  Assert.AreEqual('evap', P.Monitors[6].Key);
  Assert.AreEqual(0.5, P.Monitors[6].Ratio, 1E-9);
end;

procedure TVehicleInfoParserTests.InUsePerformanceCompression;
var
  Msg: TOBDInfoTypeMessage;
  P: TOBDInUsePerformance;
  I: Integer;
begin
  Msg := Default (TOBDInfoTypeMessage);
  Msg.InfoType := OBD_INFOTYPE_IPT_COMPRESSION;
  Msg.ItemCount := 18;
  SetLength(Msg.Data, 36);
  for I := 0 to 17 do
    Msg.Data[I * 2 + 1] := I + 1;
  P := TOBDVehicleInfoParser.InUsePerformance(Msg);
  Assert.IsTrue(P.CompressionIgnition);
  Assert.AreEqual(18, Integer(Length(P.Counters)));
  Assert.AreEqual('FUELCOND', P.Counters[17].Name);
  Assert.AreEqual(8, Integer(Length(P.Monitors)));
  Assert.AreEqual('nmhc_catalyst', P.Monitors[0].Key);
  Assert.AreEqual('pm_filter', P.Monitors[3].Key);
  Assert.AreEqual('fuel_system', P.Monitors[7].Key);
end;

procedure TVehicleInfoParserTests.EcuNameSplits;
var
  N: TOBDEcuNameInfo;
begin
  N := TOBDVehicleInfoParser.EcuName(Info($0A,
    '7E8 10 17 49 0A 01 45 43 4D'#13'7E8 21 00 2D 45 6E 67 69 6E'#13 +
    '7E8 22 65 43 6F 6E 74 72 6F'#13'7E8 23 6C 00 00 00 00 00 00'#13'>')[0]);
  Assert.AreEqual('ECM', N.Acronym);
  Assert.AreEqual('EngineControl', N.Name);
  Assert.AreEqual('7E8', N.Ecu);
end;

procedure TVehicleInfoParserTests.UdsVinWithNodi;
var
  M: TArray<TOBDInfoTypeMessage>;
begin
  M := TOBDVehicleInfoParser.AssembleUDS(DecodeRaw($22, TBytes.Create($F8,
    $02), '62 F8 02 01 57 56 57 5A 5A 5A 41 55 5A 47 57 31 32 33 34 35 36'),
    $02);
  Assert.AreEqual(1, Integer(Length(M)));
  Assert.AreEqual(1, M[0].ItemCount);
  Assert.AreEqual('WVWZZZAUZGW123456',
    TOBDVehicleInfoParser.TextItems(M[0], 0)[0]);
end;

procedure TVehicleInfoParserTests.UdsCvnWithoutNodi;
var
  M: TArray<TOBDInfoTypeMessage>;
begin
  M := TOBDVehicleInfoParser.AssembleUDS(DecodeRaw($22, TBytes.Create($F8,
    $06), '62 F8 06 6F 3A 92 C1'), $06);
  Assert.AreEqual(1, Integer(Length(M)));
  Assert.AreEqual('6F3A92C1', TOBDVehicleInfoParser.CVNs(M[0])[0]);
end;

initialization
  TDUnitX.RegisterTestFixture(TVehicleInfoParserTests);

end.
